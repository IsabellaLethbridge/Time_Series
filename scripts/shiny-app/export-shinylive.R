# =============================================================================
# Export the JSE regime explorer as a Shinylive app (runs in the browser, no server)
#
# Run from the portfolio root (open Time_Series.Rproj first), AFTER precompute.R:
#   source("scripts/shiny-app/export-shinylive.R")
#
# It writes a static web app to projects/jse-app/. _quarto.yml copies that folder
# into the rendered site, and project-04.qmd embeds it.
# =============================================================================

# install.packages(c("shinylive", "httpuv"))   # once

app_dir = "scripts/shiny-app"
out_dir = "projects/jse-app"

if (!file.exists(file.path(app_dir, "app_data.rds"))) {
  stop("app_data.rds not found. Run scripts/shiny-app/precompute.R first.")
}

# Start from a clean folder so old files don't linger:
unlink(out_dir, recursive = TRUE)

# Only app.R and app_data.rds are needed in the browser, so export a temporary copy
# that leaves out precompute.R, README.md and this script:
tmp = file.path(tempdir(), "jse-app-src")
unlink(tmp, recursive = TRUE)
dir.create(tmp)
file.copy(file.path(app_dir, c("app.R", "app_data.rds")), tmp)

shinylive::export(tmp, out_dir)

message("Exported to ", out_dir, ".")
message("To preview it, run:  httpuv::runStaticServer(\"", out_dir, "\")")
message("(Opening index.html by double-clicking will not work; it must be served over http.)")
