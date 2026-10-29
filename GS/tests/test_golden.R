# tests/test_golden.R  --  A whole GS run on a tiny real project, compared with outputs a person has checked and accepted.
#   Rscript tests/test_golden.R                         run and compare (from the GS folder; also run by tests/run_tests.R)
#   GS_UPDATE_GOLDEN=1 Rscript tests/test_golden.R      save this run's outputs as the expected ones (after a deliberate change)
#   Rscript tests/test_golden.R accept "<your name>"    accept the expected outputs, after checking them in tests/golden/expected
# tests/golden holds the project: raw/ (an AGI SuperSting inverse Schlumberger line with its command and contact resistance
# files, an EarthImager subset of another line whose parent is not here, and an IRIS Prosys export cut to 60 quadrupoles with a
# Res2DInv file of another line; the reader samples' real files). The run uses the built-in QC profiles and a base zone of
# UTC-7; GPR lines are not catalogued, so none are here. It runs on a copy of raw/ (file times fixed) into a temporary folder.
# The outputs (every CSV under output/ and config/ but logs, run history, manifest, audit_index and detail) are saved
# normalized: run stamps and machine paths left out, so a run on any computer compares equal. Like the reader samples (core
# testkit.R), what is saved is only a record of what GS made: the test fails until tests/golden/accepted.csv holds the md5 of
# the expected outputs as they are now.
# Core candidate: golden_outputs(), golden_diff(), accept_golden(), given the app's run function.

if (!exists("check")) {
  Sys.setenv(GS_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
  source(file.path(here, "gs_pipeline.R"), chdir = TRUE)
  fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
  GD_ALONE <- TRUE
}

GOLDEN_SKIP  <- "^output/logs/|^output/audit/summary\\.csv$|\\.(png|html|pdf|rds|log|svg|zip)$|run_history|run_manifest|audit_index|/detail/|\\.tmp"
GOLDEN_STAMP <- "^(run_id|run_at|run_time|started|finished|generated|created|log_file|pipeline_version|app_version|version|seconds|dir|mtime|path|last_scanned)$"
GOLDEN_ACCEPT_COLS <- c("checked_by", "checked_on", "expected_md5", "note")

# The run's outputs, normalized, written as CSV under `to`; returns their paths relative to `to`.
golden_outputs <- function(p, raw, to) {
  f <- list.files(p, recursive = TRUE); f <- f[grepl("^(output|config)/.*\\.csv$", f) & !grepl(GOLDEN_SKIP, f)]
  roots <- c(setNames(rep("<project>", 3), c(p, normalizePath(p), normalizePath(p, winslash = "/"))),   # Windows: "\\" and "/" spellings
             setNames(rep("<raw>", 3), c(raw, normalizePath(raw), normalizePath(raw, winslash = "/")))); roots <- roots[order(-nchar(names(roots)))]
  for (x in f) { d <- if (file.size(file.path(p, x)) > 0) fread(file.path(p, x), colClasses = "character", na.strings = NULL) else data.table()
    k <- grep(GOLDEN_STAMP, names(d), value = TRUE); if (length(k)) d[, (k) := NULL]
    for (j in names(d)) { v <- d[[j]]; for (r in names(roots)) v <- gsub(r, roots[[r]], v, fixed = TRUE); set(d, j = j, value = v) }
    dir.create(dirname(file.path(to, x)), recursive = TRUE, showWarnings = FALSE); fwrite(d, file.path(to, x), eol = "\n") }   # "\n" on Windows too, as the expected files
  sort(f)
}
golden_md5 <- function(dir) { f <- sort(list.files(dir, recursive = TRUE)); if (!length(f)) return("")
  t <- tempfile(); on.exit(unlink(t)); writeBin(charToRaw(paste0(f, " ", tools::md5sum(file.path(dir, f)), "\n", collapse = "")), t); unname(as.character(tools::md5sum(t))) }   # "\n" on Windows too

# Cells that differ: numbers by more than 1e-9 of their size (the last bits of a mean or a percentile differ between processors,
# x86_64 and arm64), everything else as text.
golden_cells_differ <- function(a, b) { na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b)); n <- !is.na(na) & !is.na(nb)
  out <- a != b; out[n] <- abs(na[n] - nb[n]) > 1e-9 * pmax(1, abs(na[n])); out }
# What differs between two folders of normalized outputs, one line each (as run_check.R reports it).
golden_diff <- function(want, got) {
  fw <- sort(list.files(want, recursive = TRUE)); fg <- sort(list.files(got, recursive = TRUE)); out <- c(paste("gone:", setdiff(fw, fg)), paste("new: ", setdiff(fg, fw)))
  for (p in intersect(fw, fg)) { if (tools::md5sum(file.path(want, p)) == tools::md5sum(file.path(got, p))) next
    rd <- function(f) if (file.size(f) > 0) fread(f, colClasses = "character", na.strings = NULL) else data.table()
    x <- rd(file.path(want, p)); y <- rd(file.path(got, p)); m <- character()
    if (nrow(x) != nrow(y)) m <- c(m, sprintf("rows %d -> %d", nrow(x), nrow(y)))
    if (length(d <- setdiff(names(x), names(y)))) m <- c(m, paste("columns gone:", paste(d, collapse = " ")))
    if (length(d <- setdiff(names(y), names(x)))) m <- c(m, paste("columns new:", paste(d, collapse = " ")))
    if (nrow(x) == nrow(y)) { cm <- intersect(names(x), names(y)); k <- vapply(cm, function(c) sum(golden_cells_differ(x[[c]], y[[c]])), 0L)
      if (any(k > 0)) m <- c(m, paste("cells:", paste(sprintf("%s=%d", names(k)[k > 0], k[k > 0]), collapse = " "))) }
    if (length(m)) out <- c(out, paste0("differs: ", p, " (", paste(m, collapse = "; "), ")")) }   # numbers within the tolerance are the same
  out[!grepl("^(gone|new): *$", out)]
}

# Accept the expected outputs after looking at them: writes tests/golden/accepted.csv. Called by hand, never by a test.
accept_golden <- function(gdir, checked_by, note = "") {
  ed <- file.path(gdir, "expected"); if (!length(list.files(ed, recursive = TRUE))) stop("no expected outputs: run the test first")
  fwrite(data.table(checked_by = checked_by, checked_on = format(Sys.Date()), expected_md5 = golden_md5(ed), note = note), file.path(gdir, "accepted.csv"))
  invisible(TRUE)
}

gdir <- file.path(here, "tests", "golden"); a <- if (exists("GD_ALONE")) commandArgs(TRUE) else character()
if (length(a) && a[1] == "accept") { accept_golden(gdir, if (length(a) > 1) a[2] else stop("give your name: accept \"<your name>\"")); cat("accepted\n"); quit(status = 0) }
say("golden project (a whole run)")
local({
  raw <- file.path(tempfile("gs_golden"), "raw"); dir.create(dirname(raw)); file.copy(file.path(gdir, "raw"), dirname(raw), recursive = TRUE)   # a checkout's file times differ:
  Sys.setFileTime(list.files(raw, recursive = TRUE, full.names = TRUE), as.POSIXct("2024-01-01", tz = "UTC")); raw <- normalizePath(raw)   # fixed, so no run depends on them
  ed <- file.path(gdir, "expected")
  run_once <- function() { p <- tempfile("gs_golden"); dir.create(p)
    r <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = p, base_offset_h = -7, previews = FALSE))))); "" },
                  error = function(e) conditionMessage(e))
    o <- tempfile("gs_golden_out"); if (!nzchar(r)) golden_outputs(p, raw, o); unlink(p, recursive = TRUE); list(error = r, out = o) }
  r1 <- run_once(); r2 <- run_once()
  check(!nzchar(r1$error) && !nzchar(r2$error), "the golden project runs")
  if (nzchar(r1$error)) { say("  run stopped: ", r1$error); return(invisible()) }
  check(!length(golden_diff(r1$out, r2$out)), "two runs of the golden project give the same outputs")
  saved <- FALSE
  if (identical(Sys.getenv("GS_UPDATE_GOLDEN"), "1") || !length(list.files(ed, recursive = TRUE))) {
    unlink(ed, recursive = TRUE); dir.create(ed); file.copy(list.files(r1$out, full.names = TRUE), ed, recursive = TRUE); saved <- TRUE
    cat("  (this run's outputs saved as the expected ones: check them in tests/golden/expected)\n") }
  d <- golden_diff(ed, r1$out); for (x in utils::head(d, 30)) cat("  ", x, "\n")
  acc <- if (file.exists(file.path(gdir, "accepted.csv"))) fread(file.path(gdir, "accepted.csv"), colClasses = "character") else data.table()
  ok_acc <- nrow(acc) == 1 && identical(acc$expected_md5, golden_md5(ed))
  if (!ok_acc) cat("  not accepted. Check tests/golden/expected, then Rscript tests/test_golden.R accept \"<your name>\"\n")
  check(!length(d) && ok_acc, "the golden project's run gives the outputs a person checked and accepted")
  unlink(c(r1$out, r2$out, dirname(raw)), recursive = TRUE) })

if (exists("GD_ALONE")) { if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green") }
