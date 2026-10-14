# build/make_lockfile.R  --  Writes build/renv.lock: the R version and the exact version of every package the five apps need
# (and of everything those packages need), from the library this R uses. The bundled-R build installs from it, and anyone can
# rebuild the same set with renv::restore(lockfile = "build/renv.lock").
#   Rscript build/make_lockfile.R          (from the Ovid_Apps folder; renv must be installed)
# The list comes from each app's launcher/run_app.R (APP$pkgs) plus what the shared core and launcher use, and the packages
# the apps use only when present (parquet, Unicode clean-up, Excel writing in tests).
here <- normalizePath(if (file.exists("build/make_lockfile.R")) "." else "..")
apps <- c("CS", "WLR", "GT", "GS", "RS")
decl <- unlist(lapply(apps, function(a) { l <- grep("pkgs =", readLines(file.path(here, a, "launcher", "run_app.R")), value = TRUE)
  eval(parse(text = sub("\\)\\)\\s*$", ")", sub("^.*pkgs = ", "", l)))) }))
need <- sort(unique(c(decl, "later", "httpuv", "readxl", "zip", "codetools")))
optional <- c("stringi", "writexl")                                  # used only when present
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("not installed here, so they cannot be pinned: ", paste(miss, collapse = ", "))
opt <- optional[vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
if (length(setdiff(optional, opt))) cat("optional, not installed here and left out:", paste(setdiff(optional, opt), collapse = ", "), "\n")
pkgs <- c(need, opt)
tmp <- tempfile("ovid_lock_"); dir.create(tmp)
renv::snapshot(project = tmp, lockfile = file.path(here, "build", "renv.lock"), packages = pkgs, prompt = FALSE, force = TRUE)
cat("build/renv.lock: R", paste(R.version$major, R.version$minor, sep = "."), "and", length(jsonlite::read_json(file.path(here, "build", "renv.lock"))$Packages), "packages\n")
