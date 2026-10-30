# =============================================================================
# JSE regime explorer: precompute the model fits used by app.R
#
# Run once from the root of the portfolio project (the folder with _quarto.yml):
#   source("scripts/shiny-app/precompute.R")
#
# It fits Gaussian HMMs with 1-4 states and GJR-GARCH(1,1)-t on the training
# set (Jul 1995 - Dec 2012), exactly as in the Project 4 analysis, runs every
# model through the whole series with the parameters fixed, and saves the
# results to scripts/shiny-app/app_data.rds. The app only reads that file, so
# it starts instantly and does not need rugarch or readxl when deployed.
# =============================================================================

library(readxl)
library(rugarch)

set.seed(2026)

data_path  = "_data/PT-DATA-ALBI-JIBAR-JSEIND-Daily-1994-2017.xlsx"
out_path   = "scripts/shiny-app/app_data.rds"
sheet_name = "JSE Various Indices"
train_end  = as.Date("2012-12-31")

if (!file.exists(data_path)) {
  stop("Could not find ", data_path, ". Run this script from the portfolio root folder.")
}

# -----------------------------------------------------------------------------
# HMM toolkit (same functions as in project-04-analysis.qmd)
# -----------------------------------------------------------------------------

hmm_loglik = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  v = delta * dens[1, ]
  ll = log(sum(v))
  phi = v / sum(v)
  if (T_len > 1) {
    for (t in 2:T_len) {
      v = as.numeric(phi %*% Gamma) * dens[t, ]
      u = sum(v)
      ll = ll + log(u)
      phi = v / u
    }
  }
  ll
}

hmm_forward = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  phi_mat = matrix(NA, T_len, m)
  lu = numeric(T_len)
  for (t in 1:T_len) {
    v = if (t == 1) delta * dens[1, ] else as.numeric(phi %*% Gamma) * dens[t, ]
    u = sum(v)
    lu[t] = log(u)
    phi = v / u
    phi_mat[t, ] = phi
  }
  list(loglik = sum(lu), lu = lu, phi = phi_mat,
       lalpha = cumsum(lu) + log(phi_mat))
}

hmm_backward = function(dens, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  lbeta = matrix(0, T_len, m)
  phi = rep(1 / m, m)
  lscale = log(m)
  if (T_len > 1) {
    for (t in (T_len - 1):1) {
      v = as.numeric(Gamma %*% (dens[t + 1, ] * phi))
      lbeta[t, ] = log(v) + lscale
      phi = v / sum(v)
      lscale = lscale + log(sum(v))
    }
  }
  lbeta
}

hmm_local = function(dens, delta, Gamma) {
  fwd = hmm_forward(dens, delta, Gamma)
  lbeta = hmm_backward(dens, Gamma)
  exp(fwd$lalpha + lbeta - fwd$loglik)
}

hmm_viterbi = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  lG = log(Gamma)
  ld = log(dens)
  xi = matrix(NA, T_len, m)
  xi[1, ] = log(delta) + ld[1, ]
  for (t in 2:T_len) {
    for (j in 1:m) xi[t, j] = max(xi[t - 1, ] + lG[, j]) + ld[t, j]
  }
  states = numeric(T_len)
  states[T_len] = which.max(xi[T_len, ])
  for (t in (T_len - 1):1) states[t] = which.max(xi[t, ] + lG[, states[t + 1]])
  states
}

stat_dist = function(Gamma) {
  m = nrow(Gamma)
  solve(t(diag(m) - Gamma + 1), rep(1, m))
}

gamma_w2n = function(w, m) {
  G = diag(m)
  G[!G] = exp(w)
  G / rowSums(G)
}

norm_hmm_nll = function(w, x, m) {
  mu = w[1:m]
  sigma = exp(w[(m + 1):(2 * m)])
  Gamma = if (m == 1) matrix(1) else gamma_w2n(w[(2 * m + 1):(2 * m + m * (m - 1))], m)
  delta = if (m == 1) 1 else stat_dist(Gamma)
  dens = sapply(1:m, function(j) dnorm(x, mu[j], sigma[j]))
  -hmm_loglik(matrix(dens, ncol = m), delta, Gamma)
}

norm_hmm_unpack = function(w, m) {
  mu = w[1:m]
  sigma = exp(w[(m + 1):(2 * m)])
  Gamma = if (m == 1) matrix(1) else gamma_w2n(w[(2 * m + 1):(2 * m + m * (m - 1))], m)
  ord = order(sigma)
  Gamma = Gamma[ord, ord, drop = FALSE]
  list(m = m, mu = mu[ord], sigma = sigma[ord], Gamma = Gamma,
       delta = if (m == 1) 1 else stat_dist(Gamma))
}

norm_dens = function(x, pars) {
  matrix(sapply(1:pars$m, function(j) dnorm(x, pars$mu[j], pars$sigma[j])), ncol = pars$m)
}

norm_pseudo = function(x, pars) {
  fwd = hmm_forward(norm_dens(x, pars), pars$delta, pars$Gamma)
  T_len = length(x)
  pred = rbind(pars$delta, fwd$phi[-T_len, , drop = FALSE] %*% pars$Gamma)
  cdf = matrix(sapply(1:pars$m, function(j) pnorm(x, pars$mu[j], pars$sigma[j])), ncol = pars$m)
  qnorm(pmin(pmax(rowSums(pred * cdf), 1e-10), 1 - 1e-10))
}

# Newey-West standard error of a mean (Bartlett kernel):
nw_se = function(d, L = 6) {
  n = length(d)
  u = d - mean(d)
  s = sum(u^2) / n
  for (l in 1:L) s = s + 2 * (1 - l / (L + 1)) * sum(u[-(1:l)] * u[1:(n - l)]) / n
  sqrt(s / n)
}

# -----------------------------------------------------------------------------
# Data (same cleaning as Projects 3 and 4)
# -----------------------------------------------------------------------------

find_col = function(hdr, code, field) {
  idx = which(unlist(hdr[1, ]) == code & unlist(hdr[2, ]) == field)
  if (length(idx) != 1) stop(paste("Could not uniquely find", code, field))
  idx
}

hdr = read_excel(data_path, sheet = sheet_name, col_names = FALSE, n_max = 4)
raw = read_excel(data_path, sheet = sheet_name, col_names = FALSE, skip = 4,
                 na = c("#N/A", "", "NA"), guess_max = 10000)

date_col = which(sapply(raw, function(x) inherits(x, c("POSIXct", "Date"))))[1]
alsi_col = find_col(hdr, "J203:XJSE:IND", "TRI")

alsi = data.frame(date = as.Date(raw[[date_col]]), tri = as.numeric(raw[[alsi_col]]))
alsi = alsi[!is.na(alsi$date) & !is.na(alsi$tri), ]
alsi = alsi[order(alsi$date), ]
alsi = alsi[!duplicated(alsi$date), ]
alsi$r = c(NA, 100 * diff(log(alsi$tri)))
alsi = alsi[!is.na(alsi$r), ]

r_all   = alsi$r
n_train = sum(alsi$date <= train_end)
r_train = r_all[1:n_train]
train_idx = 1:n_train
test_idx  = (n_train + 1):length(r_all)

# -----------------------------------------------------------------------------
# Gaussian HMMs with 1-4 states (same starting values as the analysis)
# -----------------------------------------------------------------------------

jse_starts = list(
  c(mean(r_train), log(sd(r_train))),
  c(0.1, -0.1, log(0.8), log(2), rep(-3, 2)),
  c(0.1, 0.05, -0.3, log(0.6), log(1.1), log(2.5), rep(-4, 6)),
  c(0.12, 0.08, 0, -0.3, log(0.5), log(0.85), log(1.4), log(2.8), rep(-4, 12))
)

models = lapply(1:4, function(m) {
  message("Fitting the ", m, "-state HMM ...")
  fit = optim(jse_starts[[m]], norm_hmm_nll, x = r_train, m = m, method = "BFGS",
              control = list(maxit = 2000))
  pars = norm_hmm_unpack(fit$par, m)

  # Run through the whole series with the training parameters fixed:
  dens_all = norm_dens(r_all, pars)
  fwd = hmm_forward(dens_all, pars$delta, pars$Gamma)

  k = m^2 + m
  logL = -fit$value
  c(pars, list(
    logL        = logL,
    k           = k,
    AIC         = -2 * logL + 2 * k,
    BIC         = -2 * logL + k * log(n_train),
    convergence = fit$convergence,
    filtered    = fwd$phi,                                     # P(C_t | x_1:t)
    smoothed    = hmm_local(dens_all, pars$delta, pars$Gamma), # P(C_t | x_1:T)
    viterbi     = hmm_viterbi(dens_all, pars$delta, pars$Gamma),
    ls          = fwd$lu,                                      # one-step-ahead log scores
    pz          = norm_pseudo(r_train, pars)                   # pseudo-residuals (training)
  ))
})

# -----------------------------------------------------------------------------
# GJR-GARCH(1,1) with Student-t errors and an AR(1) mean (Project 3 model)
# -----------------------------------------------------------------------------

message("Fitting GJR-GARCH(1,1)-t ...")
spec_gjr = ugarchspec(variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                      mean.model         = list(armaOrder = c(1, 0), include.mean = TRUE),
                      distribution.model = "std")
gjr_fit = ugarchfit(spec = spec_gjr, data = r_train, solver = "hybrid")

spec_fixed = spec_gjr
setfixed(spec_fixed) = as.list(coef(gjr_fit))
gjr_filt = ugarchfilter(spec = spec_fixed, data = r_all)

gjr_mu    = as.numeric(fitted(gjr_filt))
gjr_sigma = as.numeric(sigma(gjr_filt))
gjr_ls    = log(ddist("std", y = r_all, mu = gjr_mu, sigma = gjr_sigma,
                      shape = coef(gjr_fit)["shape"]))

# -----------------------------------------------------------------------------
# Log-score comparison table
# -----------------------------------------------------------------------------

scores = do.call(rbind, lapply(1:4, function(m) {
  d = models[[m]]$ls[test_idx] - gjr_ls[test_idx]
  data.frame(model = paste0("Gaussian HMM, ", m, " state", if (m > 1) "s" else ""),
             m = m,
             train = mean(models[[m]]$ls[train_idx]),
             test  = mean(models[[m]]$ls[test_idx]),
             diff  = mean(d),
             p     = 2 * pnorm(-abs(mean(d) / nw_se(d))))
}))
scores = rbind(scores, data.frame(model = "GJR-GARCH(1,1)-t", m = NA,
                                  train = mean(gjr_ls[train_idx]),
                                  test  = mean(gjr_ls[test_idx]),
                                  diff  = NA, p = NA))

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

app_data = list(
  series   = data.frame(date = alsi$date, r = r_all),
  n_train  = n_train,
  models   = models,
  gjr      = list(sigma = gjr_sigma, mu = gjr_mu, ls = gjr_ls, coef = coef(gjr_fit)),
  scores   = scores,
  created  = Sys.time(),
  versions = c(R = R.version.string, rugarch = as.character(packageVersion("rugarch")))
)

saveRDS(app_data, out_path)
message("Saved ", out_path, " (", round(file.size(out_path) / 1e6, 1), " MB).")

# Quick check against the Project 4 analysis (expected about -7276, -6660, -6551, -6514):
print(round(sapply(models, `[[`, "logL"), 1))
