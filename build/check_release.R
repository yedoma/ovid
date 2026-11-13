# build/check_release.R  --  The release candidate's workflow check, run with the runtime's own R from a clean copy of the
# candidate source: for each app, a copy of its checked-in sample (tests/golden/raw) is imported into a new project, a second
# run is killed part way (a real process, at the point where the output copy has just been made), the next start puts the
# output back, the project is opened and run again; then a run is killed after writing all its output, a recovery whose
# clean-up fails keeps everything, the retry finishes (output byte for byte as before, the stopped run's log kept), the next
# run works, the raw sample is unchanged, and an export is written. Nothing outside temporary folders is touched.
#
#   <runtime>/R/bin/Rscript build/check_release.R <Ovid_Apps copy> [APP ...]      (RHOME and an empty R_LIBS_USER / R_LIBS_SITE)
# Exits 1 when any step fails; prints one line per app and step.
args <- commandArgs(TRUE); root <- normalizePath(if (length(args)) args[1] else ".", mustWork = TRUE)
apps <- if (length(args) > 1) toupper(args[-1]) else c("CS", "WLR", "GT", "GS", "RS")
rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
tmp <- file.path(tempdir(), "ovid_release_check"); dir.create(tmp, showWarnings = FALSE)
fail <- character(); step <- function(app, what, ok, detail = "") { cat(sprintf("  %-4s %-7s %s%s\n", app, if (isTRUE(ok)) "ok" else "FAIL", what, if (nzchar(detail)) paste0(" (", detail, ")") else ""))
  if (!isTRUE(ok)) fail <<- c(fail, paste(app, what)) }
# one R process: the app's pipeline sourced, then `code` (with raw, proj, zip set); prints RESULT OK or RESULT ERROR <message>
child <- function(app, code, raw, proj, zip, kill_at = "none") {
  pipe <- list.files(file.path(root, app), "_pipeline\\.R$", full.names = TRUE)[1]
  r <- sprintf(paste0("Sys.setenv(%s_NO_RUN = '1', OVID_RUN_SYNC = '1', OVID_TEST_KILL_AT = %s); setwd(%s); suppressMessages(source(%s)); say <- function(...) invisible(NULL)\n",
                      "raw <- %s; proj <- %s; zip <- %s\n",
                      "r <- tryCatch({ invisible(capture.output({ %s })); 'OK' }, error = function(e) paste('ERROR', conditionMessage(e))); cat('RESULT', r, '\\n')"),
               app, deparse(kill_at), deparse(dirname(pipe)), deparse(basename(pipe)), deparse(raw), deparse(proj), deparse(zip), code)
  f <- tempfile(fileext = ".R"); writeLines(r, f)
  out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  res <- grep("^RESULT", out, value = TRUE); if (length(res)) trimws(sub("^RESULT ", "", res[1])) else paste("no result:", utils::tail(out, 1))
}
run_code <- c(CS = "run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = proj, make_plots = FALSE)))",
              WLR = "run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = '', dgps_dir = '', project_dir = proj, make_plots = FALSE, unzoned_offset_h = -7)))",
              GT = "run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = '', project_dir = proj, make_plots = FALSE)))",
              GS = "run_pipeline(list(raw_dirs = raw, project_dir = proj, incremental = TRUE))",
              RS = "run_pipeline(list(project_dir = proj), scan = raw, source_id = 'sample')")
export_code <- c(CS = "s <- data.table::fread(file.path(proj, 'output', 'audit', 'station_summary.csv'))[, .(station, stream)]; build_export(proj, unique(s), zip, summaries = FALSE, report = FALSE)",
                 WLR = "make_export_zip(proj, zip)",
                 GT = "ss <- data.table::fread(file.path(proj, 'output', 'audit', 'sites_summary.csv')); build_export(proj, ss$site, zip, res = 'day', out_offset = 0)",
                 GS = "export_archive(zip, proj)",
                 RS = "run_pipeline(list(project_dir = proj), scan = character(), export = TRUE); file.copy(file.path(proj, 'output', 'viewer', 'Ovid_RS_viewer.zip'), zip)")
cat("R", as.character(getRversion()), R.version$arch, "at", R.home(), "\n")
for (app in apps) {
  fx <- file.path(root, app, "tests", "golden", "raw"); if (!dir.exists(fx)) { step(app, "sample", FALSE, "no tests/golden/raw"); next }
  raw <- file.path(tmp, app, "raw"); dir.create(raw, recursive = TRUE); file.copy(list.files(fx, full.names = TRUE), raw, recursive = TRUE, copy.date = TRUE)
  proj <- file.path(tmp, app, "project"); zip <- file.path(tmp, app, "export.zip"); rc <- run_code[[app]]
  raw_inv <- function(d) { f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE)); c(f, unname(tools::md5sum(f))) }; raw0 <- raw_inv(raw)
  r1 <- child(app, rc, raw, proj, zip); step(app, "sample import (a first run on the sample)", identical(r1, "OK"), r1)
  before <- tools::md5sum(sort(list.files(file.path(proj, "output"), recursive = TRUE, full.names = TRUE)))
  r2 <- child(app, rc, raw, proj, zip, kill_at = "backup_copy")
  step(app, "a run killed part way", !identical(r2, "OK") && file.exists(file.path(proj, ".ovid_run")), r2)
  r3 <- child(app, sprintf("cat('NOTE', output_recover(proj, list(raw_dirs = raw)), '\\n')"), raw, proj, zip)
  after <- tools::md5sum(sort(list.files(file.path(proj, "output"), recursive = TRUE, full.names = TRUE)))
  step(app, "recovery on the next start (output as before, nothing left over)", identical(r3, "OK") && identical(unname(after), unname(before)) &&
         !any(file.exists(file.path(proj, c(".ovid_run", ".output_before_run", ".output_before_run.part", ".ovid_lock")))), r3)
  r4 <- child(app, rc, raw, proj, zip); step(app, "project reopened and run again", identical(r4, "OK"), r4)
  data_of <- function() { f <- sort(list.files(file.path(proj, "output"), recursive = TRUE, full.names = TRUE, all.files = TRUE))
    f <- f[!grepl("/logs/|/\\.run_|/\\.ovid_kept", f)]; setNames(unname(tools::md5sum(f)), substring(f, nchar(proj) + 2L)) }
  logs_of <- function() list.files(file.path(proj, "output", "logs"), "\\.log$")
  before2 <- data_of(); logs0 <- logs_of()
  r6 <- child(app, rc, raw, proj, zip, kill_at = "run_written")
  step(app, "a run killed after writing all its output, before it finished", !identical(r6, "OK") && file.exists(file.path(proj, ".ovid_run")), r6)
  r7 <- child(app, paste0("real <- remove_within; remove_within <- function(paths, root, ...) { if (any(basename(paths) == '.output_failed')) stop('held open'); real(paths, root, ...) }; ",
                          "cat('NOTE', output_recover(proj, list(raw_dirs = raw)), '\\n')"), raw, proj, zip)
  step(app, "recovery whose clean-up fails: the output is back, the stopped run's output kept, the journal kept",
       identical(r7, "OK") && identical(data_of(), before2) && dir.exists(file.path(proj, ".output_failed")) && file.exists(file.path(proj, ".ovid_run")), r7)
  r8 <- child(app, "cat('NOTE', output_recover(proj, list(raw_dirs = raw)), '\\n')", raw, proj, zip)
  step(app, "the retry finishes: output as the last finished run left it, the stopped run's log kept, nothing left over",
       identical(r8, "OK") && identical(data_of(), before2) && length(setdiff(logs_of(), logs0)) == 1 &&
         !any(file.exists(file.path(proj, c(".ovid_run", ".output_before_run", ".output_failed", ".ovid_lock")))), r8)
  r9 <- child(app, rc, raw, proj, zip); step(app, "the next run after recovery", identical(r9, "OK"), r9)
  step(app, "the raw sample unchanged (file list and hashes)", identical(raw_inv(raw), raw0))
  r5 <- child(app, export_code[[app]], raw, proj, zip); step(app, "export", identical(r5, "OK") && file.exists(zip) && file.size(zip) > 0, r5)
}
cat(if (length(fail)) paste0("\n", length(fail), " step(s) failed: ", paste(fail, collapse = "; "), "\n") else "\nAll release steps passed.\n")
if (length(fail)) quit(status = 1)
