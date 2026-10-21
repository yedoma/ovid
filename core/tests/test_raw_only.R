# core/tests/test_raw_only.R  --  Every app runs with only its raw data folder set: no metadata, no project folder, no set-up
# tables. The pipeline is run in a fresh R with HOME in a temporary folder, so the default project folder
# (~/Projects/Ovid/Projects/<app>/project, core util.R) is made there and nothing of the user's is touched.
#   Rscript core/tests/test_raw_only.R [APP ...]        (exit status 1 if anything fails)
# Raw data: OVID_TEST_RAW_<APP> if set, else ~/Projects/Ovid/Projects/test_samples/<APP>/raw, else the first folder in that sample's
# raw_dirs.txt; an app with none is skipped with a note, and the summary counts it as skipped, not passed.
A <- commandArgs(TRUE); apps <- if (length(A)) A else c("CS", "WLR", "GT", "GS", "RS")
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(dirname(dirname(normalizePath(sub("^--file=", "", a[1]))))) })
sample_raw <- function(app) file.path("~/Projects/Ovid/Projects/test_samples", app, "raw")
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
ok <- 0L; fail <- character(); skipped <- character()
check <- function(what, cond) if (isTRUE(cond)) { ok <<- ok + 1L; cat("  ok   ", what, "\n") } else { fail <<- c(fail, what); cat("  FAIL ", what, "\n") }
for (app in apps) {
  raw <- path.expand(Sys.getenv(paste0("OVID_TEST_RAW_", app), sample_raw(app)))
  lst <- file.path(dirname(raw), "raw_dirs.txt")   # a sample that lists its folders instead: the first one
  if (!dir.exists(raw) && file.exists(lst)) raw <- path.expand(trimws(strsplit(paste(readLines(lst, warn = FALSE), collapse = ";"), ";")[[1]][1]))
  if (!dir.exists(raw)) { skipped <- c(skipped, app); cat("  skip  ", app, ": no raw data (set OVID_TEST_RAW_", app, ")\n", sep = ""); next }
  home <- tempfile("ovid_home_"); dir.create(file.path(home, "Desktop"), recursive = TRUE)
  pipe <- list.files(file.path(here, app), "_pipeline\\.R$", full.names = TRUE)[1]
  env <- c(paste0("HOME=", home), paste0(app, "_RAW_DIRS=", raw), paste0(app, "_NO_RUN=1"), paste0(app, "_PROJECT_DIR="), paste0(app, "_METADATA_DIR="))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf("setwd(%s); source(%s); r <- tryCatch({ run_pipeline(); 'OK' }, error = function(e) paste('ERROR', conditionMessage(e))); cat('\\nRESULT', r, '\\n')",
    deparse(dirname(pipe)), deparse(basename(pipe))))), stdout = TRUE, stderr = TRUE, env = env))
  res <- sub("^RESULT ", "", grep("^RESULT", out, value = TRUE)[1] %||% "no result")
  proj <- file.path(home, "Projects", "Ovid", "Projects", app, "project")
  check(sprintf("%s runs with only a raw folder (no metadata, no project folder): %s", app, trimws(res)), identical(trimws(res), "OK"))
  check(sprintf("%s wrote its output in the default project folder ~/Projects/Ovid/Projects/%s/project", app, app), dir.exists(file.path(proj, "output")) && length(list.files(file.path(proj, "output"), recursive = TRUE)) > 0)
  unlink(home, recursive = TRUE)
}
cat(sprintf("\n%d passed, %d failed%s\n", ok, length(fail), if (length(skipped)) sprintf(", %d skipped", length(skipped)) else "")); if (length(fail)) quit(status = 1)
