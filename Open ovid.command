#!/bin/bash
# Double-click to open the ovid launcher: one window that starts CS, WLR, GT, GS and RS (core/launcher_app).
# R: the portable runtime in runtime/ beside this file when there is one (build/make_runtime_mac.sh), else the R installed
# on this computer. With the installed R, the first start installs the packages build/renv.lock lists (once).
cd "$(dirname "$0")" || exit 1
ROOT="$(pwd)"
if [ -x "$ROOT/runtime/R/bin/Rscript" ]; then
  R="$ROOT/runtime/R/bin/Rscript"
else
  R=$(command -v Rscript || echo /Library/Frameworks/R.framework/Resources/bin/Rscript)
fi
OVID_ROOT="$ROOT" "$R" -e '
need <- c("shiny", "processx", "callr", "jsonlite", "httpuv")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) { cat("Installing what the launcher needs:", paste(miss, collapse = ", "), "\n"); install.packages(miss, repos = "https://cloud.r-project.org") }
port <- tryCatch(httpuv::randomPort(min = 8050L, max = 8099L), error = function(e) 8077L)
cat("ovid launcher at http://127.0.0.1:", port, "  (close this window to stop the launcher)\n", sep = "")
shiny::runApp(file.path(Sys.getenv("OVID_ROOT"), "core", "launcher_app"), port = port, host = "127.0.0.1", launch.browser = TRUE, quiet = TRUE)'
