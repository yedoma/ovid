# build/make_lockfile.R  --  Writes build/renv.lock: the R version and the exact version of every package the five apps need
# (and of everything those packages need), from the library this R uses. The bundled-R build installs from it, and anyone can
# rebuild the same set with renv::restore(lockfile = "build/renv.lock").
#   Rscript build/make_lockfile.R          (from the Ovid_Apps folder; renv must be installed)
# The list comes from each app's launcher/run_app.R (APP$pkgs), what renv::dependencies() finds in core/ and the apps, what the
# launcher uses, and the packages the apps use only when present (Unicode clean-up, Excel writing in tests). renv is not
# activated: the snapshot is taken in a temporary project, so no .Rprofile or renv/ library is written.
here <- normalizePath(if (file.exists("build/make_lockfile.R")) "." else "..")
apps <- c("CS", "WLR", "GT", "GS", "RS")
decl <- unlist(lapply(apps, function(a) { l <- grep("pkgs =", readLines(file.path(here, a, "launcher", "run_app.R")), value = TRUE)
  eval(parse(text = sub("\\)\\)\\s*$", ")", sub("^.*pkgs = ", "", l)))) }))
# and every package the code itself uses (renv::dependencies over core and the apps; not docs/, build/ or bundled libraries)
used <- do.call(rbind, lapply(c("core", apps), function(x) renv::dependencies(file.path(here, x), progress = FALSE, errors = "ignored", dev = TRUE)))
used <- setdiff(unique(used$Package[!grepl("/(docs|build|library)/", used$Source)]), rownames(utils::installed.packages(priority = "base")))
optional <- c("stringi", "writexl")                                  # used only when present
need <- sort(setdiff(unique(c(decl, used, "later", "httpuv", "readxl", "zip", "codetools")), optional))
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("not installed here, so they cannot be pinned: ", paste(miss, collapse = ", "))
opt <- optional[vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
if (length(setdiff(optional, opt))) cat("optional, not installed here and left out:", paste(setdiff(optional, opt), collapse = ", "), "\n")
pkgs <- c(need, opt)
tmp <- tempfile("ovid_lock_"); dir.create(tmp)
renv::snapshot(project = tmp, lockfile = file.path(here, "build", "renv.lock"), packages = pkgs, prompt = FALSE, force = TRUE)
cat("build/renv.lock: R", paste(R.version$major, R.version$minor, sep = "."), "and", length(jsonlite::read_json(file.path(here, "build", "renv.lock"))$Packages), "packages\n")
