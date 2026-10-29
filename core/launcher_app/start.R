# core/launcher_app/start.R  --  Starts the ovid launcher in the default browser. Run by "Open ovid.command" (Mac) and
# "Open ovid.bat" (Windows) with OVID_ROOT set to the Ovid_Apps folder, using the portable runtime/ when there is one, else
# the installed R. Installs the few packages the launcher itself needs when they are missing (the launcher then installs the
# rest, from build/renv.lock, with its first-start progress card).
need <- c("shiny", "processx", "callr", "jsonlite", "httpuv")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  cat("Installing what the launcher needs:", paste(miss, collapse = ", "), "\n")
  utils::install.packages(miss, repos = "https://cloud.r-project.org", type = if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else "source")
}
root <- Sys.getenv("OVID_ROOT")
if (!nzchar(root) || !dir.exists(file.path(root, "core", "launcher_app"))) stop("OVID_ROOT must be the Ovid_Apps folder (it holds core/launcher_app).")
port <- tryCatch(httpuv::randomPort(min = 8050L, max = 8099L), error = function(e) 8077L)
cat("ovid launcher at http://127.0.0.1:", port, "  (close this window to stop the launcher)\n", sep = "")
shiny::runApp(file.path(root, "core", "launcher_app"), port = port, host = "127.0.0.1", launch.browser = TRUE, quiet = TRUE)
