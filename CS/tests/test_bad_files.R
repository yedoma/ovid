# tests/test_bad_files.R  --  Damaged copies of the reader samples, made in a temporary folder at test time: each must give a
# clear problem or flag, never a stopped run and never a record lost without a word.
#   Rscript tests/test_bad_files.R              (from the CS folder; also run by tests/run_tests.R)
#   CS_BAD_FILES_TABLE=1 Rscript ...            prints what CS does with each damaged file
# The TOA5 samples get every damage (and the Campbell ones: a program change partway, a table re-initialised, NAN, INF and
# -7999, two intervals in one table, a second download overlapping the first); the TOB3 and mixed-array samples are cut,
# emptied and left with their header only. All damaged files of one kind go into one raw folder and one run, beside one good
# logger table written here.
# Core candidate: the damage by data row (as GT and WLR tests/test_bad_files.R), given a format's row pattern and time column.

if (!exists("check")) {
  Sys.setenv(CS_NO_RUN = "1", OVID_RUN_SYNC = "1")
  .app <- if (file.exists("cs_pipeline.R")) "." else ".."; suppressMessages(source(file.path(.app, "cs_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; BF_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("damaged raw files\n")
local({
  F <- list(row = "^\"[0-9]{4}-", sep = ",", tfmt = "%Y-%m-%d %H:%M:%S", vcol = 3L)        # a TOA5 record: "time",record,values...
  sp <- function(x) strsplit(x, ",", fixed = TRUE)[[1]]
  set_t <- function(line, secs) { p <- sp(line); t <- as.POSIXct(gsub("\"", "", p[1]), tz = "UTC") + secs; p[1] <- paste0("\"", format(t, F$tfmt), "\""); paste(p, collapse = ",") }
  set_v <- function(line, v, col = F$vcol) { p <- sp(line); p[col] <- v; paste(p, collapse = ",") }
  set_rec <- function(line, r) { p <- sp(line); p[2] <- r; paste(p, collapse = ",") }
  drop_c <- function(line, col = F$vcol) { p <- sp(line); paste(p[-col], collapse = ",") }
  text_variants <- function(path) {
    l <- readLines(path, warn = FALSE); di <- grep(F$row, l); n <- length(di); mid <- di[n %/% 2]; k <- which(di > mid)
    b <- readBin(path, "raw", file.size(path)); st <- c(1L, which(b == as.raw(10L)) + 1L)[mid]; p <- sp(l[mid])
    v <- list(empty = list(bytes = raw(0), rows = 0L), header_only = list(lines = l[1:4], rows = 0L),
              truncated = list(bytes = b[seq_len(st - 1L + nchar(paste0(paste(p[seq_len(F$vcol)], collapse = ","), ","), "bytes") + 2L)], rows = n %/% 2, cut = TRUE))
    ld <- append(l, set_v(l[di[10]], "9.5"), after = di[10]); v$duplicate_times <- list(lines = append(ld, l[di[5]], after = di[5]), rows = n + 2L)
    lo <- l; lo[di[20:24]] <- l[di[24:20]]; v$out_of_order <- list(lines = lo, rows = n)
    hd <- l[1:4]; hd[1] <- sub(",\"[^\"]*\",\"[0-9]+\",\"[^\"]*\"$", ",\"CPU:changed.CR1X\",\"999\",\"Table1\"", hd[1]); hd[2:4] <- vapply(hd[2:4], drop_c, "")
    lp <- l; lp[di[k]] <- vapply(l[di[k]], drop_c, ""); v$program_change <- list(lines = append(lp, hd, after = di[k[1]] - 1L), rows = n)   # a new program: its header, one column fewer
    lr <- l; lr[di[k]] <- vapply(seq_along(k), function(j) set_rec(l[di[k[j]]], j - 1L), ""); v$table_reinit <- list(lines = lr, rows = n)   # RECORD from 0 again partway
    lb <- l; lb[di[k]] <- vapply(l[di[k]], set_t, "", secs = -3 * 86400 * 30); v$clock_jump_back <- list(lines = lb, rows = n)
    la <- l; la[di[k]] <- vapply(l[di[k]], set_t, "", secs = 400 * 86400); v$clock_jump_ahead <- list(lines = la, rows = n)
    ln <- l; ln[di[7]] <- set_v(l[di[7]], "\"NAN\""); ln[di[8]] <- set_v(l[di[8]], "\"INF\""); ln[di[9]] <- set_v(l[di[9]], "-7999"); v$nan_and_codes <- list(lines = ln, rows = n, codes = 3L)
    st2 <- as.numeric(diff(as.POSIXct(gsub("\"", "", sapply(l[di[1:2]], function(x) sp(x)[1])), tz = "UTC")), units = "secs")
    lm <- l; lm[di[k]] <- vapply(seq_along(k), function(j) set_t(l[di[k[1]]], (j - 1) * st2 / 2), ""); v$mixed_intervals <- list(lines = lm, rows = n)   # second half at half the interval
    v }
  wr <- function(x, to) if (!is.null(x$bytes)) writeBin(x$bytes, to) else writeLines(x$lines, to)
  good <- function(f) writeLines(c('"TOA5","Good","CR1000","88001","os","CPU:g.CR1","1","Table1"', '"TIMESTAMP","RECORD","AirTC_Avg"', '"TS","RN","Deg C"', '"","","Avg"',
                                   sprintf('"2020-01-01 %02d:00:00",%d,%.1f', 0:23, 0:23, 1 + (0:23) / 10)), f)

  sd <- file.path(.app, "tests", "samples"); smp <- list.files(sd, "__", full.names = TRUE); smp <- smp[!dir.exists(smp)]
  kind <- sub("__.*$", "", basename(smp)); nm <- sub("^.*?__", "", basename(smp)); cases <- list()
  for (i in seq_along(smp)) {
    vv <- if (kind[i] == "TOA5") text_variants(smp[i]) else { b <- readBin(smp[i], "raw", file.size(smp[i]))
      hb <- if (kind[i] == "TOB3") which(b[-length(b)] == as.raw(13) & b[-1] == as.raw(10))[6] + 1L else 0L
      c(list(empty = list(bytes = raw(0)), truncated = list(bytes = b[seq_len(length(b) %/% 2)], cut = TRUE)), if (hb) list(header_only = list(bytes = b[seq_len(hb)]))) }
    for (d in names(vv)) cases[[length(cases) + 1L]] <- c(vv[[d]], list(file = nm[i], kind = kind[i], damage = d))
  }
  work <- tempfile("cs_bad"); dir.create(work); res <- list()
  run_dir <- function(raw, p) tryCatch({ invisible(capture.output(r <- run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = p, make_plots = FALSE, write_stitched = FALSE, incremental = FALSE))))); list(error = "", r = r) },
                                       error = function(e) list(error = conditionMessage(e), r = NULL))
  for (dm in unique(vapply(cases, `[[`, "", "damage"))) {
    raw <- file.path(work, dm); dir.create(raw); cs <- Filter(function(x) x$damage == dm, cases); for (x in cs) wr(x, file.path(raw, x$file)); good(file.path(raw, "Good_Table1.dat"))
    rd <- lapply(cs, function(x) { p <- file.path(raw, x$file); h <- read_header(p)
      r <- tryCatch(if (x$kind == "TOA5" && h$status == "ok") read_toa5(p, h$columns[[1]], NA, h$sep) else NULL, error = function(e) NULL); list(status = h$status, rows = if (is.null(r)) NA_integer_ else nrow(r)) })
    p <- file.path(work, paste0("p_", dm)); o <- run_dir(raw, p)
    fa <- if (file.exists(file.path(p, "output", "audit", "file_audit.csv"))) fread(file.path(p, "output", "audit", "file_audit.csv"), colClasses = "character") else data.table()
    au <- tryCatch(load_audit(p), error = function(e) data.table()); pr <- if (!is.null(o$r)) o$r$problems else data.table(where = character(), problem = character())
    tc <- file.path(p, "output", "audit", "timestamp_checks.csv"); tc <- if (file.exists(tc) && file.size(tc) > 0) fread(tc, colClasses = "character") else data.table(source_file = character(), check = character())
    for (j in seq_along(cs)) { x <- cs[[j]]; s <- if (nrow(fa)) fa[basename(file) == x$file] else data.table()
      lid <- if (nrow(au) && "source_file" %in% names(au)) unique(au[basename(sub("#.*$", "", source_file)) == x$file, logger_id]) else character()
      res[[length(res) + 1L]] <- data.table(damage = dm, kind = x$kind, file = x$file, run_error = o$error, header = rd[[j]]$status, rows_read = rd[[j]]$rows, rows_expected = x$rows %||% NA_integer_,
        status = if (nrow(s)) s$status[1] else NA_character_, used = if (nrow(s)) s$used[1] else NA_character_, read = if (nrow(s)) s$read[1] else NA_character_,
        in_output = if (nrow(s)) s$in_output[1] else NA_character_, dropped = if (nrow(s)) s$dropped[1] else NA_character_, reconciles = if (nrow(s)) s$reconciles[1] else NA_character_,
        problem = if (nrow(pr)) paste(pr[grepl(x$file, paste(where, problem), fixed = TRUE), problem], collapse = " / ") else "",
        rules = if (nrow(au)) paste(unique(au[basename(sub("#.*$", "", source_file)) == x$file | logger_id %in% lid, rule]), collapse = ";") else "",
        clock = paste(unique(tc[basename(source_file) == x$file, check]), collapse = ";"), cut = isTRUE(x$cut)) } }
  bf <- rbindlist(res, fill = TRUE)
  if (identical(Sys.getenv("CS_BAD_FILES_TABLE"), "1")) { op <- options(width = 250); print(bf[, !"run_error"], nrows = 500); print(unique(bf[, .(damage, run_error)])); options(op) }
  told <- function(x) nzchar(x$problem) || !x$status %in% c("ok", NA) || !x$header %in% "ok"
  check(paste0("no damaged file stops a run (", uniqueN(bf$damage), " runs, ", nrow(bf), " damaged files, each beside a good logger table)"), all(!nzchar(bf$run_error)))
  x <- bf[damage %in% c("empty", "header_only")]
  check("an empty file or one left with its header only is reported, not taken as data", nrow(x) > 0 && all(vapply(seq_len(nrow(x)), function(i) told(x[i]), NA)) && !any(as.integer(x$in_output) > 0, na.rm = TRUE))
  u <- bf[used == "TRUE"]
  check("every damaged file read adds up: records read = in a station file + dropped with a reason", nrow(u) > 0 && all(u$reconciles == "TRUE"))
  x <- bf[damage %in% c("duplicate_times", "out_of_order", "table_reinit", "clock_jump_back", "clock_jump_ahead", "nan_and_codes", "mixed_intervals") & !is.na(rows_expected)]
  check("no record is left out by the reader: every row of a damaged TOA5 file is read", nrow(x) > 0 && all(x$rows_read == x$rows_expected))
  x <- bf[damage == "duplicate_times"]
  check("a time written twice is audited (the same record once, a different one as a conflict)", nrow(x) > 0 && all(grepl("duplicate|conflict|repeat", x$rules)))
  x <- bf[damage %in% c("out_of_order", "table_reinit", "mixed_intervals", "clock_jump_ahead")]
  check("rows out of order, a table re-initialised (RECORD from 0), two intervals and a clock jump forward keep every record", nrow(x) > 0 && all(x$reconciles == "TRUE") && all(x$in_output == x$read))
  x <- bf[damage == "clock_jump_back"]
  check("a clock set back by months inside a file is listed in timestamp_checks.csv (clock_went_backwards); the records are kept", nrow(x) > 0 && all(x$reconciles == "TRUE") && all(grepl("clock_went_backwards", x$clock)))
  x <- bf[damage == "table_reinit"]
  check("a table re-initialised partway (RECORD from 0) is listed in timestamp_checks.csv (logger_restarted)", nrow(x) > 0 && all(grepl("logger_restarted", x$clock)))
  x <- bf[damage == "nan_and_codes"]
  check("NAN, INF and -7999 are never kept as values: each leaves with a reason", nrow(x) > 0 && all(grepl("sentinel|nan|logger_nan|code", x$rules, ignore.case = TRUE)))
  x <- bf[damage == "program_change"]
  check("a program change partway (a new TOA5 header, one column fewer): read up to it and reported, no value read under the wrong column",
        nrow(x) > 0 && all(grepl("new TOA5 header", x$problem)) && all(x$reconciles == "TRUE") && all(as.integer(x$in_output) < x$rows_expected))
  x <- bf[damage == "truncated" & kind == "TOA5"]
  check("a TOA5 file cut off inside a row: the cut row is flagged or reported, not read as a whole record", nrow(x) > 0 && all(nzchar(x$problem) | grepl("cut|malformed|partial", x$rules)))
  x <- bf[damage == "truncated" & kind != "TOA5"]
  check("a TOB3 or mixed-array file cut in half is read up to the cut or reported", nrow(x) > 0 && all(vapply(seq_len(nrow(x)), function(i) told(x[i]) || x$used[i] %in% "TRUE", NA)))
  unlink(work, recursive = TRUE)
})

if (exists("BF_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
