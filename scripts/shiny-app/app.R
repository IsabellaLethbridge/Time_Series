# =============================================================================
# JSE regime explorer
# Hidden Markov models for daily FTSE/JSE All Share Index returns (Project 4)
#
# Run locally:  shiny::runApp("scripts/shiny-app")
# Data:         app_data.rds, created by precompute.R
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(plotly)

app_data = readRDS("app_data.rds")

series  = app_data$series
n_train = app_data$n_train
n_all   = nrow(series)
test_start = series$date[n_train + 1]
ann     = sqrt(252)

# -----------------------------------------------------------------------------
# Look and feel (portfolio colours: steel blue -> purple -> deep pink)
# -----------------------------------------------------------------------------

ink    = "#2E2338"
purple = "#800080"
pink   = "#FF1493"
pal4   = c("#4682B4", "#9370DB", "#800080", "#FF1493")
font   = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif"

regime_cols  = function(m) switch(m, pal4[1], pal4[c(1, 4)], pal4[c(1, 3, 4)], pal4)
regime_names = function(m) switch(m, "Single regime",
                                  c("Calm", "Turbulent"),
                                  c("Calm", "Elevated", "Crisis"),
                                  c("Calm", "Normal", "Elevated", "Crisis"))

theme = bs_theme(
  version      = 5,
  bg           = "#FFFFFF",
  fg           = ink,
  primary      = purple,
  secondary    = pink,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  "border-radius" = "0.6rem"
) |>
  bs_add_rules("
    .bslib-page-title { font-weight: 600; letter-spacing: -0.01em; }
    .bslib-value-box .value-box-title { font-size: 0.85rem; opacity: 0.8; }
    .bslib-value-box .value-box-value { font-weight: 600; }
    .bslib-value-box .value-box-showcase { color: #800080; }
    .table { font-size: 0.9rem; }
    .table td, .table th { text-align: right; }
    .table td:first-child, .table th:first-child { text-align: left; }
    .table tr.selected-row td { background: #F6E9F6; font-weight: 600; }
    .note { font-size: 0.85rem; color: #6c6375; }
    .swatch { display: inline-block; width: 0.8em; height: 0.8em; border-radius: 50%;
              margin-right: 0.4em; vertical-align: middle; }
  ")

soft_box = value_box_theme(bg = "#FBF3FA", fg = ink)

# Shared plotly styling:
style_plot = function(p, ...) {
  p |>
    layout(font = list(family = font, color = ink, size = 13),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           hoverlabel = list(font = list(family = font)),
           margin = list(l = 60, r = 20, t = 30, b = 40), ...) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Small helper for clean HTML tables with an optional highlighted row:
html_table = function(df, digits = 3, highlight = NULL) {
  # Format each column: whole-number columns without decimals, missing values as a dash.
  cells = lapply(df, function(x) {
    if (!is.numeric(x)) return(as.character(x))
    ok = !is.na(x) & is.finite(x)
    d = if (all(x[ok] == round(x[ok]))) 0 else digits
    out = rep("–", length(x))
    out[ok] = formatC(x[ok], format = "f", digits = d, big.mark = ",")
    out
  })
  body = lapply(seq_len(nrow(df)), function(i) {
    tags$tr(class = if (!is.null(highlight) && i %in% highlight) "selected-row",
            lapply(cells, function(col) tags$td(HTML(col[i]))))
  })
  tags$table(class = "table table-sm table-hover align-middle mb-0",
             tags$thead(tags$tr(lapply(names(df), tags$th))),
             tags$tbody(body))
}

# Periods to zoom in on:
zooms = list(
  "Full sample (1995–2017)"         = NULL,
  "1998 emerging-market crisis"          = c("1998-04-01", "1998-12-31"),
  "2001 rand crash"                      = c("2001-07-01", "2002-03-31"),
  "2006 emerging-market sell-off"        = c("2006-03-01", "2006-09-30"),
  "2008–09 global financial crisis" = c("2008-06-01", "2009-09-30"),
  "Test period (2013–2017)"         = c("2013-01-01", "2017-02-28")
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui = page_sidebar(
  title = "JSE regime explorer",
  theme = theme,
  fillable = FALSE,

  sidebar = sidebar(
    width = 290,
    radioButtons("m", "Number of regimes", choices = 1:4, selected = 4, inline = TRUE),
    radioButtons("decode", "How to assign regimes",
                 choices = c("Global decoding (Viterbi)" = "viterbi",
                             "Local decoding (smoothed)" = "smoothed",
                             "Filtered (real time)"      = "filtered"),
                 selected = "viterbi"),
    selectInput("zoom", "Zoom to", choices = names(zooms)),
    uiOutput("legend"),
    hr(),
    p(class = "note",
      "Gaussian hidden Markov models fitted to daily log returns of the FTSE/JSE",
      "All Share total return index. Parameters were estimated on July 1995 to",
      "December 2012; January 2013 to February 2017 (shaded) is out of sample."),
    p(class = "note", "Project 4 of my Time Series Analysis portfolio, UCT 2026.")
  ),

  uiOutput("value_boxes"),

  navset_card_underline(
    id = "tabs",
    full_screen = TRUE,

    nav_panel(
      "Regime timeline",
      plotlyOutput("timeline", height = "560px"),
      p(class = "note mt-2", textOutput("timeline_note", inline = TRUE))
    ),

    nav_panel(
      "Regimes",
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Estimated regimes"),
             uiOutput("regime_tbl"),
             p(class = "note mt-2",
               "Volatility is annualised with √252. Expected spell = 1 / (1 − γ",
               tags$sub("ii"), "), in trading days.")),
        card(card_header("Transition probabilities"),
             plotlyOutput("tpm", height = "330px"))
      ),
      card(card_header("Volatility and mean of each regime"),
           plotlyOutput("ladder", height = "300px"))
    ),

    nav_panel(
      "Model fit",
      layout_columns(
        col_widths = c(12, 6, 6),
        card(card_header("Choosing the number of regimes (training set)"),
             uiOutput("ic_tbl"),
             p(class = "note mt-2", "Lower AIC and BIC are better. The selected model is highlighted.")),
        card(card_header("Normal QQ plot of pseudo-residuals"),
             plotlyOutput("qq", height = "320px")),
        card(card_header("ACF of squared pseudo-residuals"),
             plotlyOutput("acf", height = "320px"),
             p(class = "note", textOutput("diag_note", inline = TRUE)))
      )
    ),

    nav_panel(
      "HMM vs GJR-GARCH",
      card(card_header("Annualised volatility"),
           plotlyOutput("vol", height = "380px"),
           p(class = "note", textOutput("vol_note", inline = TRUE))),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Out-of-sample forecasts: cumulative log score difference"),
             plotlyOutput("cumscore", height = "320px"),
             p(class = "note", "HMM minus GJR-GARCH, summed over the test period.",
               "A line going down means GJR-GARCH forecasts the next day's return better.")),
        card(card_header("Mean one-step-ahead log score (higher is better)"),
             uiOutput("score_tbl"),
             p(class = "note mt-2", "p-value: test that the mean test-set difference to",
               "GJR-GARCH is zero (Newey–West standard errors)."))
      )
    ),

    nav_panel(
      "About",
      div(class = "p-2", style = "max-width: 760px;",
          markdown("
### What this app shows

A **hidden Markov model** (HMM) assumes the market is always in one of *m* unobserved
regimes. Each regime has its own mean return and volatility, and the market moves between
regimes according to a Markov chain with transition matrix Γ. Because regimes are
persistent, an HMM reproduces volatility clustering and heavy tails.

**Ways of assigning regimes**

- **Global decoding (Viterbi):** the single most likely *sequence* of regimes, given all the data.
- **Local decoding (smoothed):** for each day separately, the most likely regime given all the data,
  P(C<sub>t</sub> | x<sub>1</sub>, ..., x<sub>T</sub>). It uses future returns, so it is a description of the past, not a forecast.
- **Filtered (real time):** the regime probabilities using only returns up to that day,
  P(C<sub>t</sub> | x<sub>1</sub>, ..., x<sub>t</sub>). This is what an investor could have known on the day,
  and it is the fair comparison with GJR-GARCH.

**Comparison with GJR-GARCH.** The GJR-GARCH(1,1) model with Student-*t* errors and an
AR(1) mean is the model selected in Project 3. Both models are evaluated with the
one-step-ahead log score: the log of the forecast density at the return that actually happened.

### Data and methods

Daily FTSE/JSE All Share total return index (J203), 30 June 1995 to 3 February 2017.
Source: Gebbie, T. (2020). *Daily Sampled South African Market Index Data 1994–2017*.
University of Cape Town. <https://doi.org/10.25375/uct.11302325.v1>

All HMM algorithms (scaled forward and backward algorithms, Viterbi, pseudo-residuals) were
written from first principles and the likelihood was maximised with `optim()`. GJR-GARCH
was fitted with `rugarch`. Parameters are estimated on the training set only and kept fixed
for the test period. The fits are precomputed (see `precompute.R`), so the app does no
estimation itself.

### References

- Hamilton, J. D. (1989). A new approach to the economic analysis of nonstationary time series
  and the business cycle. *Econometrica*, 57(2), 357–384.
- Zucchini, W., MacDonald, I. L., & Langrock, R. (2016). *Hidden Markov models for time series:
  An introduction using R* (2nd ed.). CRC Press.
"))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server = function(input, output, session) {

  m     = reactive(as.integer(input$m))
  mod   = reactive(app_data$models[[m()]])
  cols  = reactive(regime_cols(m()))
  nms   = reactive(regime_names(m()))

  probs = reactive(if (input$decode == "filtered") mod()$filtered else mod()$smoothed)

  states = reactive({
    if (input$decode == "viterbi") mod()$viterbi
    else max.col(probs(), ties.method = "first")
  })

  x_range = reactive({
    z = zooms[[input$zoom]]
    if (is.null(z)) as.character(range(series$date)) else z
  })

  # HMM volatility (% per day) for the chosen way of assigning regimes:
  hmm_vol = reactive({
    md = mod()
    if (input$decode == "viterbi") return(md$sigma[states()])
    p = probs()
    mean_t = as.numeric(p %*% md$mu)
    sqrt(pmax(as.numeric(p %*% (md$sigma^2 + md$mu^2)) - mean_t^2, 0))
  })

  test_shade = list(type = "rect", xref = "x", yref = "paper",
                    x0 = test_start, x1 = max(series$date), y0 = 0, y1 = 1,
                    fillcolor = "rgba(46,35,56,0.06)", line = list(width = 0), layer = "below")

  # ---- Sidebar legend --------------------------------------------------------

  output$legend = renderUI({
    md = mod()
    div(class = "mb-2",
        tags$label(class = "form-label", "Regimes"),
        lapply(seq_len(m()), function(j) {
          div(class = "note", style = "color: inherit;",
              span(class = "swatch", style = paste0("background:", cols()[j], ";")),
              sprintf("%s: %.0f%% volatility", nms()[j], md$sigma[j] * ann))
        }))
  })

  # ---- Value boxes -----------------------------------------------------------

  output$value_boxes = renderUI({
    md = mod(); mm = m()
    top = nms()[mm]
    bics = sapply(app_data$models, `[[`, "BIC")
    d_bic = md$BIC - min(bics)
    test_ls = app_data$scores$test[mm]
    gjr_ls  = app_data$scores$test[5]

    layout_column_wrap(
      width = "200px", fill = FALSE, class = "mb-3",
      value_box(
        title = paste(if (mm == 1) "Volatility" else paste(top, "regime volatility")),
        value = sprintf("%.0f%%", md$sigma[mm] * ann),
        showcase = bs_icon("activity"), theme = soft_box,
        p(if (mm == 1) "annualised" else sprintf("annualised; calm regime %.0f%%", md$sigma[1] * ann))
      ),
      value_box(
        title = if (mm == 1) "Mean return" else paste("Time in", tolower(top), "regime"),
        value = if (mm == 1) sprintf("%.3f%%", md$mu[1]) else sprintf("%.0f%%", 100 * md$delta[mm]),
        showcase = bs_icon("hourglass-split"), theme = soft_box,
        p(if (mm == 1) "per day" else sprintf("a spell lasts %.0f trading days on average",
                                             1 / (1 - md$Gamma[mm, mm])))
      ),
      value_box(
        title = "BIC (training set)",
        value = formatC(md$BIC, format = "f", digits = 0, big.mark = ","),
        showcase = bs_icon("bar-chart-steps"), theme = soft_box,
        p(if (d_bic < 1e-8) "best of the four models" else sprintf("%.1f above the best model", d_bic))
      ),
      value_box(
        title = "Test log score",
        value = sprintf("%.3f", test_ls),
        showcase = bs_icon("bullseye"), theme = soft_box,
        p(sprintf("GJR-GARCH: %.3f (higher is better)", gjr_ls))
      )
    )
  })

  # ---- Regime timeline -------------------------------------------------------

  output$timeline = renderPlotly({
    mm = m(); cl = cols(); nm = nms(); st = states(); pr = probs()
    lab = factor(nm[st], levels = nm)
    hover = sprintf("<b>%s</b><br>Return: %.2f%%<br>Regime: %s<br>P(%s): %.2f",
                    format(series$date, "%d %b %Y"), series$r, nm[st], nm[mm], pr[, mm])

    p1 = plot_ly(x = series$date, y = series$r, type = "scattergl", mode = "markers",
                 color = lab, colors = setNames(cl, nm),
                 marker = list(size = 4, opacity = 0.85),
                 text = hover, hoverinfo = "text")

    p2 = plot_ly()
    for (j in seq_len(mm)) {
      p2 = add_trace(p2, x = series$date, y = pr[, j], type = "scatter", mode = "lines",
                     stackgroup = "probs", line = list(width = 0.4, color = cl[j]),
                     fillcolor = cl[j], name = nm[j], showlegend = FALSE,
                     hovertemplate = paste0(nm[j], ": %{y:.2f}<extra></extra>"))
    }

    prob_title = if (input$decode == "filtered") "Filtered probability" else "Smoothed probability"

    subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.62, 0.38),
            titleY = TRUE, margin = 0.04) |>
      style_plot(
        xaxis  = list(range = x_range(), title = ""),
        yaxis  = list(title = "Daily return (%)", zeroline = FALSE, gridcolor = "#EEE8F0"),
        yaxis2 = list(title = prob_title, range = c(0, 1), gridcolor = "#EEE8F0"),
        legend = list(orientation = "h", x = 0, y = 1.07),
        shapes = list(test_shade),
        annotations = list(list(x = test_start, y = 1, xref = "x", yref = "paper",
                                text = "Test period", showarrow = FALSE,
                                xanchor = "left", yanchor = "bottom",
                                font = list(size = 11, color = "#6c6375")))
      )
  })

  output$timeline_note = renderText({
    switch(input$decode,
      viterbi  = "Points are coloured by the most likely sequence of regimes (Viterbi). The bottom panel shows the smoothed regime probabilities.",
      smoothed = "Points are coloured by the most likely regime on each day, using all the data. These probabilities use future returns too.",
      filtered = "Points are coloured by the most likely regime using only returns up to that day, which is what was knowable in real time.")
  })

  # ---- Regimes tab -----------------------------------------------------------

  output$regime_tbl = renderUI({
    md = mod(); mm = m()
    stay = diag(md$Gamma)
    html_table(data.frame(
      Regime                 = sprintf("<span class='swatch' style='background:%s'></span>%s", cols(), nms()),
      `Mean (% per day)`     = md$mu,
      `SD (% per day)`       = md$sigma,
      `Volatility (%)`       = md$sigma * ann,
      `Share of time (%)`    = 100 * md$delta,
      `Expected spell (days)` = ifelse(stay >= 1, NA, 1 / (1 - stay)),
      check.names = FALSE
    ), digits = 2)
  })

  output$tpm = renderPlotly({
    md = mod(); nm = nms(); mm = m()
    grid = expand.grid(from = 1:mm, to = 1:mm)
    z = md$Gamma[cbind(grid$from, grid$to)]
    labels = lapply(seq_along(z), function(k) {
      list(x = nm[grid$to[k]], y = nm[grid$from[k]], xref = "x", yref = "y",
           text = formatC(z[k], format = "f", digits = 3), showarrow = FALSE,
           font = list(color = if (z[k] > 0.5) "#FFFFFF" else ink, size = 13))
    })
    plot_ly(x = nm, y = nm, z = md$Gamma, type = "heatmap",
            colorscale = list(list(0, "#FFFFFF"), list(0.15, "#E9D3EC"), list(1, purple)),
            zmin = 0, zmax = 1, showscale = FALSE, xgap = 2, ygap = 2,
            hovertemplate = "From %{y} to %{x}: %{z:.4f}<extra></extra>") |>
      style_plot(xaxis = list(title = "To", side = "top", type = "category"),
                 yaxis = list(title = "From", autorange = "reversed", type = "category"),
                 annotations = labels)
  })

  output$ladder = renderPlotly({
    md = mod(); nm = nms(); cl = cols()
    plot_ly(x = md$sigma * ann, y = md$mu * 252, type = "scatter", mode = "markers+text",
            text = nm, textposition = "top center",
            marker = list(size = 14 + 60 * md$delta, color = cl,
                          line = list(color = "#FFFFFF", width = 1.5)),
            hovertemplate = paste0("%{text}<br>Volatility: %{x:.1f}%<br>",
                                   "Mean return: %{y:.1f}% per year<extra></extra>")) |>
      style_plot(xaxis = list(title = "Annualised volatility (%)", rangemode = "tozero",
                              gridcolor = "#EEE8F0"),
                 yaxis = list(title = "Annualised mean return (%)", gridcolor = "#EEE8F0",
                              zerolinecolor = "#CFC6D4"),
                 showlegend = FALSE)
  })

  # ---- Model fit tab ---------------------------------------------------------

  output$ic_tbl = renderUI({
    ms = app_data$models
    bics = sapply(ms, `[[`, "BIC")
    html_table(data.frame(
      Regimes          = 1:4,
      Parameters       = sapply(ms, `[[`, "k"),
      `Log-likelihood` = sapply(ms, `[[`, "logL"),
      AIC              = sapply(ms, `[[`, "AIC"),
      BIC              = bics,
      `ΔBIC`      = bics - min(bics),
      check.names = FALSE
    ), digits = 1, highlight = m())
  })

  output$qq = renderPlotly({
    z = sort(mod()$pz)
    q = qnorm(ppoints(length(z)))
    lim = range(c(q, z))
    plot_ly() |>
      add_trace(x = lim, y = lim, type = "scatter", mode = "lines",
                line = list(color = "#B9AFC0", dash = "dash"), hoverinfo = "skip") |>
      add_trace(x = q, y = z, type = "scattergl", mode = "markers",
                marker = list(size = 4, color = purple, opacity = 0.6),
                hovertemplate = "Theoretical: %{x:.2f}<br>Sample: %{y:.2f}<extra></extra>") |>
      style_plot(xaxis = list(title = "Standard normal quantiles", gridcolor = "#EEE8F0"),
                 yaxis = list(title = "Pseudo-residual quantiles", gridcolor = "#EEE8F0"),
                 showlegend = FALSE)
  })

  output$acf = renderPlotly({
    z = mod()$pz
    a = acf(z^2, lag.max = 30, plot = FALSE)$acf[-1]
    bound = 1.96 / sqrt(length(z))
    plot_ly(x = 1:30, y = a, type = "bar", marker = list(color = pink),
            hovertemplate = "Lag %{x}: %{y:.3f}<extra></extra>") |>
      style_plot(xaxis = list(title = "Lag (trading days)"),
                 yaxis = list(title = "Autocorrelation", gridcolor = "#EEE8F0"),
                 shapes = list(
                   list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = bound, y1 = bound,
                        line = list(color = "#6c6375", dash = "dot", width = 1)),
                   list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = -bound, y1 = -bound,
                        line = list(color = "#6c6375", dash = "dot", width = 1))),
                 showlegend = FALSE)
  })

  output$diag_note = renderText({
    z = mod()$pz
    kurt = mean((z - mean(z))^4) / mean((z - mean(z))^2)^2 - 3
    lb = Box.test(z^2, lag = 10, type = "Ljung-Box")$p.value
    sprintf("Excess kurtosis %.2f; Ljung-Box test on the squares (lag 10): p = %.3f. If the model is right, pseudo-residuals are independent N(0, 1), so the points should follow the line and the bars should stay inside the dotted bounds.",
            kurt, lb)
  })

  # ---- HMM vs GJR-GARCH tab --------------------------------------------------

  output$vol = renderPlotly({
    hv = hmm_vol() * ann
    gv = app_data$gjr$sigma * ann
    plot_ly(x = series$date) |>
      add_lines(y = gv, name = "GJR-GARCH(1,1)-t", line = list(color = purple, width = 1),
                hovertemplate = "GJR-GARCH: %{y:.1f}%<extra></extra>") |>
      add_lines(y = hv, name = paste0("HMM, ", m(), if (m() > 1) " regimes" else " regime"),
                line = list(color = pink, width = 1.4),
                hovertemplate = "HMM: %{y:.1f}%<extra></extra>") |>
      style_plot(xaxis = list(range = x_range(), title = ""),
                 yaxis = list(title = "Annualised volatility (%)", gridcolor = "#EEE8F0",
                              rangemode = "tozero"),
                 legend = list(orientation = "h", x = 0, y = 1.1),
                 hovermode = "x unified",
                 shapes = list(test_shade))
  })

  output$vol_note = renderText({
    r = cor(hmm_vol(), app_data$gjr$sigma)
    paste0(sprintf("Correlation between the two volatility series: %.2f. ", r),
           switch(input$decode,
             viterbi  = "The HMM curve is the volatility of the Viterbi regime on each day, so it jumps between a few fixed levels.",
             smoothed = "The HMM curve uses smoothed probabilities, which look ahead, so it is smoother than anything available in real time.",
             filtered = "Both curves now use only past returns, so this is the like-for-like comparison."))
  })

  output$cumscore = renderPlotly({
    idx = (n_train + 1):n_all
    p = plot_ly()
    for (j in 1:4) {
      d = cumsum(app_data$models[[j]]$ls[idx] - app_data$gjr$ls[idx])
      sel = j == m()
      p = add_lines(p, x = series$date[idx], y = d,
                    name = paste0(j, if (j > 1) " regimes" else " regime"),
                    line = list(color = if (sel) pink else "#C9BFD0", width = if (sel) 2.4 else 1.2),
                    hovertemplate = paste0(j, "-regime HMM: %{y:.1f}<extra></extra>"))
    }
    p |>
      style_plot(xaxis = list(title = ""),
                 yaxis = list(title = "Cumulative difference", gridcolor = "#EEE8F0",
                              zerolinecolor = "#6c6375"),
                 legend = list(orientation = "h", x = 0, y = 1.12))
  })

  output$score_tbl = renderUI({
    s = app_data$scores
    html_table(data.frame(
      Model                 = s$model,
      Train                 = s$train,
      Test                  = s$test,
      `Difference to GJR`   = s$diff,
      `p-value`             = s$p,
      check.names = FALSE
    ), digits = 3, highlight = m())
  })
}

shinyApp(ui, server)
