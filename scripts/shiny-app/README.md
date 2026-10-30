# JSE regime explorer

An interactive companion to Project 4: Gaussian hidden Markov models for daily
FTSE/JSE All Share Index returns, compared with GJR-GARCH(1,1)-t from Project 3.

| File | What it does |
|------|--------------|
| `precompute.R` | Fits the HMMs (1–4 regimes) and GJR-GARCH on the training set and saves everything to `app_data.rds`. Run once. |
| `app.R` | The Shiny app. It only reads `app_data.rds`, so it starts instantly. |
| `app_data.rds` | Created by `precompute.R`. |
| `export-shinylive.R` | Turns the app into a static web app in `projects/jse-app/` that runs in the browser (Shinylive). |

Run everything from the portfolio root: open `Time_Series.Rproj` first.

## 1. Install the packages (once)

```r
install.packages(c("shiny", "bslib", "bsicons", "plotly", "shinylive", "httpuv"))
# precompute.R also needs readxl and rugarch, which you already have.
```

## 2. Create the data file

```r
source("scripts/shiny-app/precompute.R")
```

It takes a minute or two. At the end it prints the four log-likelihoods. They should
be about −7276.3, −6660.3, −6550.6 and −6514.2, the same as in the Project 4 analysis.

## 3. Test the app as a normal Shiny app

```r
shiny::runApp("scripts/shiny-app")
```

Click through all five tabs and try 1–4 regimes with each decoding option. Errors are
easier to read here than in the browser version, so fix anything here first.

## 4. Export it for the browser (Shinylive)

```r
source("scripts/shiny-app/export-shinylive.R")
httpuv::runStaticServer("projects/jse-app")   # preview; press Esc / Stop to end
```

The first export downloads the Shinylive files (internet needed). The preview opens
in your browser and takes 10–30 seconds to load R the first time.

Then render the portfolio as usual (`quarto render`). `_quarto.yml` copies
`projects/jse-app/` into `_site`, and the Project 4 summary page embeds the app.
Preview the whole site with `quarto preview`: the app will not work if you open the
`.html` files by double-clicking, because it has to be served over http.

**After changing the app:** rerun `export-shinylive.R` (and `precompute.R` first if
the models changed), then render again.

## 5. Put the site online with GitHub Pages

Your portfolio is already in `IsabellaLethbridge/Time_Series` on GitHub.

1. **Visibility.** GitHub Pages is free for *public* repositories. For a private repository
   you need GitHub Pro, which is free for students through the GitHub Student Developer
   Pack. Note that a public repository makes your code visible to others.
2. **Create the `gh-pages` branch** (once). In the RStudio *Terminal* tab (not the Console),
   with no uncommitted changes:

   ```bash
   git checkout --orphan gh-pages
   git reset --hard
   git commit --allow-empty -m "Initialise gh-pages branch"
   git push origin gh-pages
   git checkout main
   ```

   Be careful with these commands: `git reset --hard` clears the working files *on the
   new empty branch only*. Commit or stash your work on `main` before you start, and
   check you are back on `main` at the end (`git branch`).
3. **Publish:**

   ```bash
   quarto publish gh-pages
   ```

   This renders the site and pushes `_site` to the `gh-pages` branch.
4. **Turn on Pages.** On GitHub: repository → **Settings** → **Pages** → *Build and
   deployment* → Source: **Deploy from a branch**, Branch: **gh-pages**, folder **/(root)**.
   After a minute or two the site is at
   `https://isabellalethbridge.github.io/Time_Series/`, and the app at
   `https://isabellalethbridge.github.io/Time_Series/projects/jse-app/`.
5. **Updating:** rerun `quarto publish gh-pages` whenever you change the portfolio.

Put the site link in your submission (and on the landing page), since the zipped site
cannot run the app when opened from the file system.

## Backup options

- **Run it from GitHub** (anyone with R and the packages):

  ```r
  shiny::runGitHub("Time_Series", "IsabellaLethbridge", subdir = "scripts/shiny-app")
  ```

  This needs `app_data.rds` to be committed to the repository.
- **shinyapps.io:** `rsconnect::deployApp("scripts/shiny-app", appName = "jse-regimes")`
  after connecting your account (shinyapps.io → your name → Tokens).

## Troubleshooting the browser version

- **Blank page or endless loading:** open the browser's developer console (F12). A
  message like *"package X is not available"* means that package has no WebAssembly
  build. Tell me which one; the app can usually do without it.
- **Fonts look plain:** the Inter font loads from Google Fonts. Without internet the app
  falls back to the system font, which is fine.
- **PDF portfolio:** the app cannot run in a PDF. Add a screenshot with the link underneath.
