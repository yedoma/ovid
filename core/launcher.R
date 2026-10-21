# Ovid/launcher.R  --  the shared Windows launcher, sourced by each app's launcher/run_app.R (started by its .bat file).
# Part of Ovid Core (shared). Keeps every package in the app's own library/ folder, installs what is missing on the
# first start, before the app starts (from launcher/packages if that folder holds package .zip files, else the versions
# in renv.lock, else CRAN's current ones), then opens the app in the default browser. Started with "--portable" it instead makes the folder self-contained: R into R-portable/,
# packages into library/, and these shared files into core/.
#
# Expects from run_app.R: APP (key, title, bat, pkgs), app_dir, args and CORE_DIR.

lib <- file.path(app_dir, "library")
dir.create(lib, showWarnings = FALSE)
.libPaths(lib)                                   # only this folder and the R installation's own library, so the folder is self-contained
options(repos = c(CRAN = "https://cloud.r-project.org"), install.packages.check.source = "no")
if (.Platform$OS.type == "windows") options(pkgType = "binary")

PKGS <- unique(c(APP$pkgs, "later", "httpuv", "readxl", "zip", "ncdf4"))     # plus what the shared core itself uses (set-up import from .xlsx, zip export, NetCDF)
cat(APP$title, "\n  app:     ", app_dir, "\n  R:       ", R.home(), " (", R.version.string, ")\n  library: ", lib, "\n\n", sep = "")
if (getRversion() < "4.2.0") stop("R 4.2 or newer is needed; this is ", R.version.string, ". Install a newer R from https://cran.r-project.org")

missing_pkgs <- function() PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
install_missing <- function() {
  need <- missing_pkgs(); if (!length(need)) return(invisible())
  offline <- file.path(app_dir, "launcher", "packages")
  zips <- if (dir.exists(offline)) list.files(offline, "\\.(zip|tgz|tar\\.gz)$", full.names = TRUE) else character()
  if (length(zips)) {
    cat("Installing packages from launcher/packages ...\n")
    install.packages(zips, lib = lib, repos = NULL, type = if (.Platform$OS.type == "windows") "win.binary" else "source", quiet = TRUE)
    need <- missing_pkgs()
  }
  lock <- c(file.path(app_dir, "renv.lock"), file.path(dirname(app_dir), "renv.lock")); lock <- lock[file.exists(lock)][1]
  if (length(need) && !is.na(lock)) {        # the versions the apps are tested with (CONTRIBUTING.md 14)
    cat("First start: installing", paste(need, collapse = ", "), "and what they need, as in renv.lock, into library/ (needs the internet, once) ...\n")
    if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", lib = lib)
    tryCatch(renv::restore(lockfile = lock, library = lib, packages = need, prompt = FALSE), error = function(e)
      cat("The versions in renv.lock could not all be installed (", conditionMessage(e), "); installing the current ones instead.\n", sep = ""))
    need <- missing_pkgs()
  }
  if (length(need)) {
    cat("First start: installing", paste(need, collapse = ", "), "and what they need into library/ (needs the internet, once) ...\n")
    install.packages(need, lib = lib, dependencies = c("Depends", "Imports", "LinkingTo"))
  }
  if (length(still <- missing_pkgs())) stop("These packages could not be installed: ", paste(still, collapse = ", "),
                                            ". Check the internet connection (or copy package .zip files into launcher/packages) and start again.")
}

# ---- make the folder self-contained: copy this R into R-portable/ and every package into library/ -----------------------------
if ("--portable" %in% args) {
  install_missing()
  dest <- file.path(app_dir, "R-portable")             # not R/: that folder holds the app's code
  if (normalizePath(R.home(), winslash = "/") != normalizePath(dest, winslash = "/", mustWork = FALSE)) {
    cat("Copying R from", R.home(), "into", dest, "(a few hundred MB) ...\n")
    unlink(dest, recursive = TRUE); dir.create(dest)
    ok <- file.copy(list.files(R.home(), full.names = TRUE, all.files = FALSE), dest, recursive = TRUE)
    if (!all(ok)) stop("Copying R failed; is there enough disk space?")
  }
  # the shared Ovid Core files go in core/ so the copy does not need the Ovid_Apps bundle beside it
  core_dest <- file.path(app_dir, "core")
  if (normalizePath(CORE_DIR, winslash = "/") != normalizePath(core_dest, winslash = "/", mustWork = FALSE)) {
    cat("Copying the shared Ovid Core files from", CORE_DIR, "into", core_dest, "...\n")
    unlink(core_dest, recursive = TRUE); dir.create(core_dest)
    if (!all(file.copy(list.files(CORE_DIR, full.names = TRUE), core_dest, recursive = TRUE))) stop("Copying the Ovid Core files failed.")
  }
  lock <- file.path(dirname(app_dir), "renv.lock")          # the package versions travel with the copy
  if (file.exists(lock)) file.copy(lock, file.path(app_dir, "renv.lock"), overwrite = TRUE)
  # packages that came with R are in R-portable/library; the rest are in library/
  cat("\nDone. This folder now runs on a Windows computer without R installed:\n",
      "zip the whole folder (", app_dir, "), copy it anywhere, unzip, and double-click ", APP$bat, ".\n", sep = "")
  quit(save = "no", status = 0)
}

install_missing()

port <- tryCatch(httpuv::randomPort(min = 8100, max = 8999), error = function(e) 8765L)
do.call(Sys.setenv, setNames(list("1"), paste0(APP$key, "_LOCAL")))     # the app shows Browse... links when started this way
Sys.setenv(OVID_CORE = CORE_DIR)
setwd(app_dir)
cat("\nStarting at http://127.0.0.1:", port, "  (close this window to stop the app)\n\n", sep = "")
shiny::runApp(app_dir, port = port, host = "127.0.0.1", launch.browser = TRUE, quiet = TRUE)
