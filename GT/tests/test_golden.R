# tests/test_golden.R  --  A whole GT run on a tiny real project, compared with outputs a person has checked and accepted.
#   Rscript tests/test_golden.R                         run and compare (from the GT folder; also run by tests/run_tests.R)
#   GT_UPDATE_GOLDEN=1 Rscript tests/test_golden.R      save this run's outputs as the expected ones (after a deliberate change)
#   Rscript tests/test_golden.R accept "<your name>"    accept the expected outputs, after checking them in tests/golden/expected
# tests/golden holds the project: raw/ (copies of reader samples, prefix left off) and config/ (a deployment sheet: sites and
# depths). It runs on a copy of raw/ (file times fixed) into a temporary folder, with incremental runs off.
# The outputs (every CSV under output/ and config/ but logs, run history, manifest and finish time, audit_index and detail, as in
# test_samples/run_check.R) are saved normalized: run stamps and machine paths left out, so a run on any computer compares
# equal. Like the reader samples (core testkit.R), what is saved is only a record of what GT made: the test fails until
# tests/golden/accepted.csv holds the md5 of the expected outputs as they are now.
# Core candidate: golden_outputs(), golden_diff(), accept_golden(), given the app's run function.

if (!exists("check")) {
  invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
  Sys.setenv(GT_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- if (file.exists("gt_pipeline.R")) "." else ".."; suppressMessages(source(file.path(here, "gt_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; GD_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}

GOLDEN_SKIP  <- "^output/logs/|^output/audit/summary\\.csv$|\\.(png|html|pdf|rds|log|svg|zip)$|run_history|run_manifest|audit_index|/detail/|\\.tmp"
GOLDEN_STAMP <- "^(run_id|run_at|run_time|started|finished|generated|created|log_file|pipeline_version|app_version|version|seconds|dir|mtime|path)$"
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
# Files in ICU's English order, the same on every system and the order the accepted md5s were made in (sort() follows the
# system's own collation: Windows puts deployments.csv before deployment_sheet.csv; byte order would change the Mac's).
golden_md5 <- function(dir) { f <- stringi::stri_sort(list.files(dir, recursive = TRUE), locale = "en"); if (!length(f)) return("")
  t <- tempfile(); on.exit(unlink(t)); writeBin(charToRaw(paste0(f, " ", tools::md5sum(file.path(dir, f)), "\n", collapse = "")), t); unname(as.character(tools::md5sum(t))) }   # "\n" on Windows too

# What differs between two folders of normalized outputs, one line each (as run_check.R reports it).
golden_diff <- function(want, got) {
  fw <- sort(list.files(want, recursive = TRUE)); fg <- sort(list.files(got, recursive = TRUE)); out <- c(paste("gone:", setdiff(fw, fg)), paste("new: ", setdiff(fg, fw)))
  for (p in intersect(fw, fg)) { if (tools::md5sum(file.path(want, p)) == tools::md5sum(file.path(got, p))) next
    rd <- function(f) if (file.size(f) > 0) fread(f, colClasses = "character", na.strings = NULL) else data.table()
    x <- rd(file.path(want, p)); y <- rd(file.path(got, p)); m <- character()
    if (nrow(x) != nrow(y)) m <- c(m, sprintf("rows %d -> %d", nrow(x), nrow(y)))
    if (length(d <- setdiff(names(x), names(y)))) m <- c(m, paste("columns gone:", paste(d, collapse = " ")))
    if (length(d <- setdiff(names(y), names(x)))) m <- c(m, paste("columns new:", paste(d, collapse = " ")))
    if (nrow(x) == nrow(y)) { cm <- intersect(names(x), names(y)); k <- vapply(cm, function(c) sum(x[[c]] != y[[c]]), 0L)
      if (any(k > 0)) m <- c(m, paste("cells:", paste(sprintf("%s=%d", names(k)[k > 0], k[k > 0]), collapse = " "))) }
    out <- c(out, paste0("differs: ", p, if (length(m)) paste0(" (", paste(m, collapse = "; "), ")") else "")) }
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
cat("golden project (a whole run)\n")
local({
  raw <- file.path(tempfile("gt_golden"), "raw"); dir.create(raw, recursive = TRUE); file.copy(list.files(file.path(gdir, "raw"), full.names = TRUE), raw)   # a checkout's file times differ:
  Sys.setFileTime(list.files(raw, full.names = TRUE), as.POSIXct("2024-01-01", tz = "UTC")); raw <- normalizePath(raw)                # GT uses them where a file states no download time
  ed <- file.path(gdir, "expected")
  run_once <- function() { p <- tempfile("gt_golden"); dir.create(p); file.copy(file.path(gdir, "config"), p, recursive = TRUE)
    r <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", project_dir = p, make_plots = FALSE, incremental = FALSE))))); "" },
                  error = function(e) conditionMessage(e))
    o <- tempfile("gt_golden_out"); if (!nzchar(r)) golden_outputs(p, raw, o); unlink(p, recursive = TRUE); list(error = r, out = o) }
  r1 <- run_once(); r2 <- run_once()
  check("the golden project runs", !nzchar(r1$error) && !nzchar(r2$error))
  if (nzchar(r1$error)) { cat("  run stopped:", r1$error, "\n"); return(invisible()) }
  check("two runs of the golden project give the same outputs", !length(golden_diff(r1$out, r2$out)))
  saved <- FALSE
  if (identical(Sys.getenv("GT_UPDATE_GOLDEN"), "1") || !length(list.files(ed, recursive = TRUE))) {
    unlink(ed, recursive = TRUE); dir.create(ed); file.copy(list.files(r1$out, full.names = TRUE), ed, recursive = TRUE); saved <- TRUE
    cat("  (this run's outputs saved as the expected ones: check them in tests/golden/expected)\n") }
  d <- golden_diff(ed, r1$out); for (x in utils::head(d, 30)) cat("  ", x, "\n")
  acc <- if (file.exists(file.path(gdir, "accepted.csv"))) fread(file.path(gdir, "accepted.csv"), colClasses = "character") else data.table()
  ok_acc <- nrow(acc) == 1 && identical(acc$expected_md5, golden_md5(ed))
  if (!ok_acc) cat("  not accepted. Check tests/golden/expected, then Rscript tests/test_golden.R accept \"<your name>\"\n")
  check("the golden project's run gives the outputs a person checked and accepted", !length(d) && ok_acc)
  unlink(c(r1$out, r2$out, dirname(raw)), recursive = TRUE) })

if (exists("GD_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
