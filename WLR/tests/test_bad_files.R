# tests/test_bad_files.R  --  Damaged copies of the reader samples, and barometric records that do not fit their water loggers,
# made in a temporary folder at test time: each must give a clear problem or flag, never a stopped run and never a reading
# lost without a word.
#   Rscript tests/test_bad_files.R              (from the WLR folder; also run by tests/run_tests.R)
#   WLR_BAD_FILES_TABLE=1 Rscript ...           prints what WLR does with each damaged file
# Text samples (HOBOware and Solinst CSV, Solinst .xle) get every damage; the .hobo samples are cut, emptied and left with their
# header only. All damaged files of one kind go into one raw folder and one run, beside a good barologger and a good water logger. The barometric cases
# (a gap, a late start, another time zone, another interval, a logger in the air) are small HOBOware files written here.
# Core candidate: the damage by data row (as GT tests/test_bad_files.R), given a format's row pattern and time column.

if (!exists("check")) {
  invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
  Sys.setenv(WLR_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- if (file.exists("wlr_pipeline.R")) "." else ".."; suppressMessages(source(file.path(here, "wlr_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; BF_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("damaged raw files\n")
local({
  # How each line-based format lays out a data row: the row pattern, separator, time columns and their format, the first value
  # column, the line naming the columns, and what stands for no value. An .xle record is a <Log> block.
  BF_TEXT <- list(
    hobo_csv    = list(row = "^[0-9]+,", sep = ",", tcol = 2, tfmt = "%m/%d/%y %I:%M:%S %p", vcol = 3, head = "Date Time", nan = "NaN"),
    solinst_csv = list(row = "^\\s*[0-9]+/", sep = ",", tcol = 1:2, tfmt = "%m/%d/%Y %I:%M:%S %p", vcol = 4, head = "^\\s*Date\\s*,", nan = "NaN"))
  split1 <- function(x, s) strsplit(x, s, fixed = TRUE)[[1]]
  join <- function(p, line, f) paste0(paste(p, collapse = f$sep), if (endsWith(line, f$sep)) f$sep else "")
  shift_t <- function(line, f, secs) { p <- split1(line, f$sep); t <- as.POSIXct(strptime(paste(p[f$tcol], collapse = " "), f$tfmt, tz = "UTC")) + secs
    s <- split1(format(t, f$tfmt), " "); if (length(f$tcol) == 2) p[f$tcol] <- c(s[1], paste(s[-1], collapse = " ")) else p[f$tcol] <- format(t, f$tfmt); join(p, line, f) }
  set_v <- function(line, f, v) { p <- split1(line, f$sep); p[f$vcol] <- v; join(p, line, f) }
  drop_v <- function(line, f) { p <- split1(line, f$sep); join(p[-f$vcol], line, f) }
  text_variants <- function(path, f) {
    l <- readLines(path, warn = FALSE); di <- grep(f$row, l); n <- length(di); mid <- di[n %/% 2]; k <- which(di > mid); hb <- max(grep(f$head, l[seq_len(di[1] - 1)]))
    b <- readBin(path, "raw", file.size(path)); st <- c(1L, which(b == as.raw(10L)) + 1L)[mid]; p <- split1(l[mid], f$sep)
    v <- list(empty = list(bytes = raw(0), rows = 0L), header_only = list(lines = l[seq_len(di[1] - 1)], rows = 0L),
              truncated = list(bytes = b[seq_len(st - 1L + nchar(paste0(paste(p[seq_len(f$vcol)], collapse = f$sep), f$sep), "bytes") + 2L)], rows = n %/% 2, cut = TRUE))
    ld <- append(l, set_v(l[di[10]], f, "9.5"), after = di[10]); v$duplicate_times <- list(lines = append(ld, l[di[5]], after = di[5]), rows = n + 2L)
    lo <- l; lo[di[20:24]] <- l[di[24:20]]; v$out_of_order <- list(lines = lo, rows = n)
    lh <- l; lh[di[k]] <- vapply(l[di[k]], drop_v, "", f = f); v$header_changed <- list(lines = append(lh, drop_v(l[hb], f), after = di[k[1]] - 1L), rows = n)
    lb <- l; lb[di[k]] <- vapply(l[di[k]], shift_t, "", f = f, secs = -3 * 86400); v$clock_jump_back <- list(lines = lb, rows = n)
    la <- l; la[di[k]] <- vapply(l[di[k]], shift_t, "", f = f, secs = 400 * 86400); v$clock_jump_ahead <- list(lines = la, rows = n)
    ln <- l; ln[di[7]] <- set_v(l[di[7]], f, f$nan); ln[di[9]] <- set_v(l[di[9]], f, "-9999"); v$nan_and_codes <- list(lines = ln, rows = n, nan_rows = c(7L, 9L))
    v }
  xle_variants <- function(path) {                                                 # Solinst .xle: the same damages, by <Log> block
    l <- readLines(path, encoding = "latin1", warn = FALSE); a <- grep("<Log id=", l, fixed = TRUE); e <- grep("</Log>", l, fixed = TRUE); n <- length(a)
    blk <- function(i) l[a[i]:e[i]]; head <- l[seq_len(a[1] - 1)]; tail <- l[(e[n] + 1):length(l)]
    body <- function(ord, edit = list()) unlist(lapply(seq_along(ord), function(j) { x <- blk(ord[j]); if (!is.null(edit[[as.character(j)]])) x <- edit[[as.character(j)]](x); x }))
    dt <- function(x, secs) { d <- sub(".*<Date>(.*)</Date>.*", "\\1", x[grep("<Date>", x)]); t <- sub(".*<Time>(.*)</Time>.*", "\\1", x[grep("<Time>", x)])
      u <- as.POSIXct(paste(d, t), "%Y/%m/%d %H:%M:%S", tz = "UTC") + secs
      x[grep("<Date>", x)] <- sub("<Date>.*</Date>", paste0("<Date>", format(u, "%Y/%m/%d"), "</Date>"), x[grep("<Date>", x)])
      x[grep("<Time>", x)] <- sub("<Time>.*</Time>", paste0("<Time>", format(u, "%H:%M:%S"), "</Time>"), x[grep("<Time>", x)]); x }
    ch1 <- function(v) function(x) { x[grep("<ch1>", x)] <- sub("<ch1>.*</ch1>", paste0("<ch1>", v, "</ch1>"), x[grep("<ch1>", x)]); x }
    k <- (n %/% 2 + 1):n; b <- readBin(path, "raw", file.size(path))
    sh <- function(secs) setNames(lapply(k, function(i) function(x) dt(x, secs)), k)
    list(empty = list(bytes = raw(0), rows = 0L), header_only = list(lines = c(head, tail), rows = 0L),
         truncated = list(bytes = b[seq_len(sum(nchar(l[seq_len(a[n %/% 2] + 3)], "bytes") + 2L) + 3L)], rows = n %/% 2, cut = TRUE),
         duplicate_times = list(lines = c(head, body(c(1:5, 5, 6:10, 10, 11:n), list("12" = ch1("9.5"))), tail), rows = n + 2L),
         out_of_order = list(lines = c(head, body(c(1:19, 24:20, 25:n)), tail), rows = n),
         clock_jump_back = list(lines = c(head, body(seq_len(n), sh(-3 * 86400)), tail), rows = n),
         clock_jump_ahead = list(lines = c(head, body(seq_len(n), sh(400 * 86400)), tail), rows = n),
         nan_and_codes = list(lines = c(head, body(seq_len(n), list("7" = ch1("NaN"), "9" = ch1("-9999"))), tail), rows = n, nan_rows = c(7L, 9L))) }
  wr <- function(x, to) if (!is.null(x$bytes)) writeBin(x$bytes, to) else { con <- file(to, "wb"); writeLines(x$lines, con, sep = "\r\n", useBytes = TRUE); close(con) }

  hobo_kpa <- function(f, serial, t, p, temp = 10, zone = "GMT-07:00") writeLines(c(sprintf("\"Plot Title: %s\"", serial),
    sprintf("\"#\",\"Date Time, %s\",\"Abs Pres, kPa (LGR S/N: %s, SEN S/N: %s)\",\"Temp, °C (LGR S/N: %s, SEN S/N: %s)\"", zone, serial, serial, serial, serial),
    sprintf("%d,%s,%s,%.2f", seq_along(t), format(t, "%m/%d/%y %I:%M:%S %p"), ifelse(is.na(p), "", sprintf("%.3f", p)), temp)), f)

  sd <- file.path(here, "tests", "samples"); smp <- list.files(sd, "__", full.names = TRUE); smp <- smp[!dir.exists(smp)]
  kind <- sub("__.*$", "", basename(smp)); nm <- sub("^.*?__", "", basename(smp))
  smp <- smp[kind != "hobo_binary"]; nm <- nm[kind != "hobo_binary"]; kind <- kind[kind != "hobo_binary"]     # the same .hobo as hobo_decoded
  cases <- list()
  for (i in seq_along(smp)) {
    vv <- if (kind[i] %in% names(BF_TEXT)) text_variants(smp[i], BF_TEXT[[kind[i]]]) else if (kind[i] == "solinst_xle") xle_variants(smp[i]) else {
      b <- readBin(smp[i], "raw", file.size(smp[i])); hs <- hobo_header(b)$data_start
      list(empty = list(bytes = raw(0)), truncated = list(bytes = b[seq_len(length(b) %/% 2)], cut = TRUE), header_only = list(bytes = b[seq_len(hs - 1L)])) }
    for (d in names(vv)) cases[[length(cases) + 1L]] <- c(vv[[d]], list(file = nm[i], kind = kind[i], damage = d))
  }
  work <- tempfile("wlr_bad"); dir.create(work); res <- list()
  bt <- as.POSIXct("2014-01-01", tz = "UTC") + (0:(24 * 4000)) * 3600                            # a good barologger over every sample's span
  run_dir <- function(raw, p) tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", dgps_dir = "", project_dir = p,
                                                                                                       make_plots = FALSE, incremental = FALSE))))); "" }, error = function(e) conditionMessage(e))
  rd <- function(p, f) { x <- file.path(p, "output", "audit", f); if (file.exists(x) && file.size(x) > 0) fread(x, colClasses = "character") else data.table() }
  for (dm in unique(vapply(cases, `[[`, "", "damage"))) {
    raw <- file.path(work, dm); dir.create(file.path(raw, "2024"), recursive = TRUE); dir.create(file.path(raw, "baro", "2024"), recursive = TRUE)
    hobo_kpa(file.path(raw, "baro", "2024", "20000099.csv"), "20000099", bt, 100 + 0.2 * sin(seq_along(bt) / 12))
    hobo_kpa(file.path(raw, "2024", "20000098.csv"), "20000098", bt[1:48], 105)
    cs <- Filter(function(x) x$damage == dm, cases); for (x in cs) wr(x, file.path(raw, "2024", x$file))
    rr <- lapply(cs, function(x) { p <- file.path(raw, "2024", x$file); r <- tryCatch(read_logger_file(p, if (x$kind == "hobo_decoded") "hobo_decoded" else sniff_format(p)), error = function(e) conditionMessage(e))
      if (is.character(r)) list(error = r, rows = NA_integer_) else list(error = "", rows = nrow(r$data %||% data.table())) })
    p <- file.path(work, paste0("p_", dm)); err <- run_dir(raw, p)
    fa <- rd(p, "file_audit.csv"); pr <- rd(p, "problems.csv"); ix <- rd(p, "audit_index.csv"); fl <- rd(p, "flags.csv"); fs <- rd(p, "files.csv")
    for (j in seq_along(cs)) { x <- cs[[j]]; s <- if (nrow(fa)) fa[name == x$file] else data.table(); ser <- if (nrow(s)) s$serial[1] else ""
      res[[length(res) + 1L]] <- data.table(damage = dm, kind = x$kind, file = x$file, run_error = err, reader_error = rr[[j]]$error, rows_read = rr[[j]]$rows, rows_expected = x$rows %||% NA_integer_,
        status = if (nrow(fs)) fs[name == x$file, status][1] else NA_character_, used = if (nrow(s)) s$used[1] else NA_character_, read = if (nrow(s)) s$readings_read[1] else NA_character_,
        in_output = if (nrow(s)) s$in_output[1] else NA_character_, dropped = if (nrow(s)) s$dropped[1] else NA_character_, reconciles = if (nrow(s)) s$reconciles[1] else NA_character_,
        problem = if (nrow(pr)) paste(pr[grepl(x$file, problem, fixed = TRUE) | grepl(x$file, where, fixed = TRUE) | (nzchar(ser) & grepl(ser, where, fixed = TRUE)), problem], collapse = " / ") else "",
        rules = if (nrow(ix) && nzchar(ser)) paste(unique(ix[logger_id == ser, rule]), collapse = ";") else "", flags = if (nrow(fl) && nzchar(ser)) paste(unique(fl[logger_id == ser, check]), collapse = ";") else "",
        cut = isTRUE(x$cut), nan_rows = length(x$nan_rows %||% integer())) } }
  bf <- rbindlist(res)
  if (identical(Sys.getenv("WLR_BAD_FILES_TABLE"), "1")) { op <- options(width = 250); print(bf[, !"run_error"], nrows = 500); print(unique(bf[, .(damage, run_error)])); options(op) }
  told <- function(x) nzchar(x$problem) || !x$status %in% c("ok", NA) || nzchar(x$rules) || nzchar(x$flags)
  check(paste0("no damaged file stops a run (", uniqueN(bf$damage), " runs, ", nrow(bf), " damaged files, each beside a good barologger and water logger)"), all(!nzchar(bf$run_error)))
  x <- bf[damage %in% c("empty", "header_only")]
  check("an empty file or one left with its header only is reported, not taken as data", nrow(x) > 0 && all(vapply(seq_len(nrow(x)), function(i) told(x[i]), NA)) && !any(x$used %in% "TRUE"))
  u <- bf[used == "TRUE"]
  check("every damaged file read adds up: readings read = in the output + dropped with a reason", nrow(u) > 0 && all(u$reconciles == "TRUE"))
  x <- bf[damage %in% c("duplicate_times", "out_of_order", "clock_jump_back", "clock_jump_ahead", "nan_and_codes") & !is.na(rows_expected)]
  check("no reading is left out by a reader: every row of a damaged text file is read", nrow(x) > 0 && all(x$rows_read == x$rows_expected))
  x <- bf[damage == "duplicate_times" & !is.na(rows_expected)]
  check("a time written twice is audited: the same reading kept once, a different one as a conflict (the newest download wins)", nrow(x) > 0 && all(grepl("duplicate_record", x$rules) & grepl("overlap_conflict", x$rules)))
  x <- bf[damage == "out_of_order" & !is.na(rows_expected)]
  check("rows out of order are put in time order and every one is kept", nrow(x) > 0 && all(x$reconciles == "TRUE") && all(x$in_output == x$read))
  x <- bf[damage == "clock_jump_back" & !is.na(rows_expected)]
  check("a clock set back by more than a day inside a file: the readings after it leave as clock_reset (core clock_faults()), nothing lost without a reason",
        nrow(x) > 0 && all(grepl("clock_reset", x$rules)) && all(x$reconciles == "TRUE"))
  x <- bf[damage == "clock_jump_ahead" & !is.na(rows_expected)]
  check("a clock jump forward is a gap that is listed, and nothing is lost", nrow(x) > 0 && all(x$reconciles == "TRUE") && all(grepl("gap", paste(x$flags, x$problem, x$rules)) | as.integer(x$dropped) == 0L))
  x <- bf[damage == "nan_and_codes" & !is.na(rows_expected)]
  check("NaN is audited as the logger writing no reading (logger_nan) and -9999 leaves by the range rules: neither is kept as a reading", nrow(x) > 0 && all(grepl("logger_nan", x$rules)) && all(grepl("range_m", x$rules)))
  x <- bf[damage == "header_changed" & !is.na(rows_expected)]
  check("a second header with other columns partway: the file is read up to it and reported, no value read under the wrong column",
        nrow(x) > 0 && all(grepl("second header", x$problem)) && all(x$rows_read < x$rows_expected) && all(x$reconciles == "TRUE"))
  x <- bf[damage == "truncated" & !is.na(rows_expected)]
  check("a file cut off inside a row: the cut row is left out with a reason or reported", nrow(x) > 0 && all(nzchar(x$problem) | nzchar(x$rules) | !x$used %in% "TRUE"))
  x <- bf[damage == "truncated" & is.na(rows_expected)]
  check("a .hobo cut in half is read up to the cut or reported", nrow(x) > 0 && all(vapply(seq_len(nrow(x)), function(i) told(x[i]) || x$used[i] %in% "TRUE", NA)))

  cat("barometric records that do not fit\n")
  wt <- as.POSIXct("2025-06-01 00:00", tz = "UTC") + 0:(24 * 10 - 1) * 3600; bar <- 100 + 0.3 * sin(seq_along(wt) / 12)
  baro_case <- function(name, wfun, bfun) { raw <- file.path(work, name); dir.create(file.path(raw, "2025"), recursive = TRUE); dir.create(file.path(raw, "baro", "2025"), recursive = TRUE)
    bfun(file.path(raw, "baro", "2025", "20000002.csv")); wfun(file.path(raw, "2025", "20000001.csv")); p <- file.path(work, paste0("p_", name)); e <- run_dir(raw, p)
    s <- list.files(file.path(p, "output", "sites"), "\\.csv$", full.names = TRUE)
    list(error = e, site = if (length(s)) fread(s[1]) else data.table(), fa = rd(p, "file_audit.csv"), pr = rd(p, "problems.csv"), fl = rd(p, "flags.csv")) }
  g <- baro_case("baro_gap", function(f) hobo_kpa(f, "20000001", wt, bar + 5), function(f) { b <- bar; b[50:100] <- NA; hobo_kpa(f, "20000002", wt[-(50:100)], b[-(50:100)]) })
  gap <- g$site[as.numeric(as.POSIXct(TIMESTAMP, tz = "UTC")) %in% as.numeric(wt[55:95])]
  check("a gap in the barologger longer than baro_max_gap_h: the water readings in it are kept, with no depth and the no_baro flag",
        !nzchar(g$error) && nrow(g$site) == length(wt) && nrow(gap) == 41 && all(is.na(gap$depth_m)) && all(grepl("no_baro", gap$flag_depth_m)) && all(g$fa$reconciles == "TRUE"))
  m <- baro_case("baro_late", function(f) hobo_kpa(f, "20000001", wt, bar + 5), function(f) hobo_kpa(f, "20000002", wt[-(1:48)], bar[-(1:48)]))
  early <- m$site[as.numeric(as.POSIXct(TIMESTAMP, tz = "UTC")) < as.numeric(wt[49])]
  check("a barologger that starts two days late: the water readings before it are kept and flagged no_baro, the rest compensated",
        !nzchar(m$error) && nrow(early) == 48 && all(is.na(early$depth_m)) && all(grepl("no_baro", early$flag_depth_m)) && all(abs(m$site[!is.na(depth_m), depth_m] - 5 / 9.80665) < 1e-6))
  z <- baro_case("baro_zone", function(f) hobo_kpa(f, "20000001", wt, bar + 5), function(f) hobo_kpa(f, "20000002", wt + 3600, bar, zone = "GMT-06:00"))
  check("a barologger whose file states another time zone is moved onto the base zone before compensating: the depth stays 5 kPa of water",
        !nzchar(z$error) && sum(!is.na(z$site$depth_m)) >= length(wt) - 1 && max(abs(z$site$depth_m - 5 / 9.80665), na.rm = TRUE) < 1e-6)
  wq <- as.POSIXct("2025-06-01 00:00", tz = "UTC") + 0:(4 * 24 * 10 - 4) * 900; barq <- approx(as.numeric(wt), bar, as.numeric(wq))$y
  iv <- baro_case("baro_interval", function(f) hobo_kpa(f, "20000001", wq, barq + 5), function(f) hobo_kpa(f, "20000002", wt, bar))
  check("a water logger at 15 minutes against an hourly barologger: the air pressure is interpolated between its readings (baro_interp)",
        !nzchar(iv$error) && sum(!is.na(iv$site$depth_m)) == length(wq) && max(abs(iv$site$depth_m - 5 / 9.80665), na.rm = TRUE) < 2e-3)
  wa <- bar + 5; wa[1:5] <- bar[1:5] + 0.01; wa[(length(wt) - 4):length(wt)] <- bar[(length(wt) - 4):length(wt)] + 0.01
  a <- baro_case("in_air", function(f) hobo_kpa(f, "20000001", wt, wa), function(f) hobo_kpa(f, "20000002", wt, bar))
  ends <- a$site[c(1:5, (nrow(a$site) - 4):nrow(a$site))]
  check("a logger in the air at the start and end of its deployment: those readings are kept and flagged, not taken as water (auto_trim_air off)",
        !nzchar(a$error) && nrow(a$site) == length(wt) && all(nzchar(ends$flag_depth_m)) && all(a$fa$reconciles == "TRUE"))
  local({ raw <- file.path(work, "only_bad"); dir.create(file.path(raw, "2024"), recursive = TRUE); dir.create(file.path(raw, "baro", "2024"), recursive = TRUE)
    hobo_kpa(file.path(raw, "baro", "2024", "20000099.csv"), "20000099", bt[1:48], 100); file.create(file.path(raw, "2024", "20000097.csv"))
    writeLines(readLines(smp[kind == "hobo_csv"][1], 2), file.path(raw, "2024", "20000096.csv")); e <- run_dir(raw, file.path(work, "p_only_bad"))
    check("a folder whose water files are all empty or header only stops with a message naming each file", grepl("holds readings", e) && grepl("20000097.csv", e, fixed = TRUE) && grepl("20000096.csv", e, fixed = TRUE)) })
  unlink(work, recursive = TRUE)
})

if (exists("BF_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
