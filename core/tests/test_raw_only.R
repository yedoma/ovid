# core/tests/test_raw_only.R  --  Every app runs with only its raw data folder set: no metadata, no project folder, no set-up
# tables. The pipeline is run in a fresh R with HOME in a temporary folder, so the default project folder
# (~/Projects/Ovid/Projects/<app>/project, core util.R) is made there and nothing of the user's is touched.
#   Rscript core/tests/test_raw_only.R [APP ...]        (exit status 1 if anything fails)
# Raw data: OVID_TEST_RAW_<APP> if set, else the app's checked-in golden raw folder (<APP>/tests/golden/raw: small, synthetic
# or trimmed and neutralized, so it ships with the code and every machine and CI runner has it), copied to a temporary folder
# first so it is only ever read; else ~/Projects/Ovid/Projects/test_samples/<APP>/raw or its raw_dirs.txt. An app with none is
# skipped with a note and counted as skipped, not passed (run_all_tests.R --release counts that as a failure).
A <- commandArgs(TRUE); apps <- if (length(A)) A else c("CS", "WLR", "GT", "GS", "RS")
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(dirname(dirname(normalizePath(sub("^--file=", "", a[1]))))) })
sample_raw <- function(app) file.path("~/Projects/Ovid/Projects/test_samples", app, "raw")
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
ok <- 0L; fail <- character(); skipped <- character()
check <- function(what, cond) if (isTRUE(cond)) { ok <<- ok + 1L; cat("  ok   ", what, "\n") } else { fail <<- c(fail, what); cat("  FAIL ", what, "\n") }
for (app in apps) {
  fx <- file.path(here, app, "tests", "golden", "raw")
  raw <- path.expand(Sys.getenv(paste0("OVID_TEST_RAW_", app), ""))
  if (!nzchar(raw) && dir.exists(fx)) { raw <- file.path(tempfile("ovid_raw_"), "raw"); dir.create(raw, recursive = TRUE)
    file.copy(list.files(fx, full.names = TRUE, all.files = TRUE, no.. = TRUE), raw, recursive = TRUE, copy.date = TRUE) }
  if (!nzchar(raw)) raw <- path.expand(sample_raw(app))
  lst <- file.path(dirname(raw), "raw_dirs.txt")   # a sample that lists its folders instead: the first one
  if (!dir.exists(raw) && file.exists(lst)) raw <- path.expand(trimws(strsplit(paste(readLines(lst, warn = FALSE), collapse = ";"), ";")[[1]][1]))
  if (!dir.exists(raw)) { skipped <- c(skipped, app); cat("  skip  ", app, ": no raw data (set OVID_TEST_RAW_", app, ")\n", sep = ""); next }
  home <- tempfile("ovid_home_"); dir.create(file.path(home, "Desktop"), recursive = TRUE)
  pipe <- list.files(file.path(here, app), "_pipeline\\.R$", full.names = TRUE)[1]
  # Set in this process and inherited by the child, then put back: system2(env =) puts "NAME=value" before the command,
  # which Windows cannot run (as in run_all_tests.R). On Windows R finds "~" through R_USER, so that is set as well.
  env <- setNames(c(home, raw, "1", "", ""), c("HOME", paste0(app, c("_RAW_DIRS", "_NO_RUN", "_PROJECT_DIR", "_METADATA_DIR"))))
  if (.Platform$OS.type == "windows") env <- c(env, R_USER = home)
  old <- Sys.getenv(names(env), unset = NA); do.call(Sys.setenv, as.list(env))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf("setwd(%s); source(%s); r <- tryCatch({ run_pipeline(); 'OK' }, error = function(e) paste('ERROR', conditionMessage(e))); cat('\\nRESULT', r, '\\n')",
    deparse(dirname(pipe)), deparse(basename(pipe))))), stdout = TRUE, stderr = TRUE))
  for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(old[[k]]), k))
  res <- sub("^RESULT ", "", grep("^RESULT", out, value = TRUE)[1] %||% "no result")
  proj <- file.path(home, "Projects", "Ovid", "Projects", app, "project")
  check(sprintf("%s runs with only a raw folder (no metadata, no project folder): %s", app, trimws(res)), identical(trimws(res), "OK"))
  check(sprintf("%s wrote its output in the default project folder ~/Projects/Ovid/Projects/%s/project", app, app), dir.exists(file.path(proj, "output")) && length(list.files(file.path(proj, "output"), recursive = TRUE)) > 0)
  unlink(home, recursive = TRUE)
}
cat(sprintf("\n%d passed, %d failed%s\n", ok, length(fail), if (length(skipped)) sprintf(", %d skipped", length(skipped)) else "")); if (length(fail)) quit(status = 1)
