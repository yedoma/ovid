# build/runtime_packages.R  --  Puts the packages build/renv.lock pins into a runtime's own library and checks them. Run by
# make_runtime_mac.sh and make_runtime_win.ps1 with the runtime's Rscript (RHOME set, R_LIBS_USER and R_LIBS_SITE empty).
#
#   Rscript build/runtime_packages.R install <out folder>
#     installs what the library lacks as binaries from the snapshot in build/snapshot.txt, then from CRAN what the snapshot
#     lacks (one repo at a time, so CRAN never overrides the snapshot). Loads no package, so it runs before relocation.
#   Rscript build/runtime_packages.R check <out folder> [--allow-newer]
#     stops when a version differs from the lockfile (with --allow-newer a newer one is listed and allowed); writes
#     <out folder>/versions.txt.
#   Rscript build/runtime_packages.R smoke <out folder>
#     checks the runtime runs on its own: its R (and R's share folder) is the one in <out folder>, its library the only
#     one, time zones resolve with no warning, the main packages load.
args <- commandArgs(TRUE); mode <- args[1]; out <- args[2]; newer_ok <- "--allow-newer" %in% args
snap <- grep("^#", readLines("build/snapshot.txt"), value = TRUE, invert = TRUE)[1]
l <- readLines("build/renv.lock")
pk <- regmatches(l, regexpr("(?<=^    \")[A-Za-z0-9._]+(?=\": [{])", l, perl = TRUE))
if (mode == "install") {
  cat("   from", snap, "(what it lacks from CRAN)\n")
  for (repo in c(snap, "https://cloud.r-project.org")) {
    need <- setdiff(pk, rownames(installed.packages())); if (!length(need)) break
    install.packages(need, lib = .libPaths()[1], repos = repo, type = "binary", quiet = TRUE) }
  quit(save = "no")
}
if (mode == "smoke") {
  stopifnot(startsWith(normalizePath(R.home(), "/"), normalizePath(out, "/")), length(.libPaths()) == 1,
            startsWith(normalizePath(R.home("share"), "/"), normalizePath(out, "/")))
  # time zones from the runtime's own zoneinfo: a warning here means local times would silently be read as UTC
  withCallingHandlers({ Sys.timezone(); stopifnot(format(as.POSIXct("2020-07-01 12:00", tz = "America/Edmonton"), "%Z") == "MDT") },
    warning = function(w) stop("time zones: ", conditionMessage(w)))
  for (p in c("shiny", "data.table", "ggplot2", "DT", "plotly", "processx", "callr", "ncdf4", "readxl")) suppressPackageStartupMessages(library(p, character.only = TRUE))
  cat("  R", as.character(getRversion()), R.version$arch, "with", nrow(installed.packages()), "packages\n")
  quit(save = "no")
}
lock <- jsonlite::read_json("build/renv.lock")
bad <- character(); newer <- character()
for (p in names(lock$Packages)) { v <- package_version(lock$Packages[[p]]$Version)
  if (!nzchar(system.file(package = p))) bad <- c(bad, paste(p, "(missing)"))
  else if (packageVersion(p) > v && newer_ok) newer <- c(newer, sprintf("%s %s (lockfile %s)", p, packageVersion(p), v))
  else if (packageVersion(p) != v) bad <- c(bad, sprintf("%s %s (lockfile %s)", p, packageVersion(p), v)) }
if (length(bad)) stop("Not as the lockfile says: ", paste(bad, collapse = "; "), ". Refresh it (Rscript build/make_lockfile.R), move build/snapshot.txt to a matching date, or pass --allow-newer.")
if (length(newer)) cat("   newer than the lockfile (allowed):", paste(newer, collapse = "; "), "\n")
ip <- installed.packages()
writeLines(c(paste("R", getRversion(), R.version$arch), paste("snapshot", snap), paste(ip[, "Package"], ip[, "Version"])), file.path(out, "versions.txt"))
cat("  ", nrow(ip), "packages\n")
