# tests/test_bad_files.R  --  Damaged copies of the reader samples, made in a temporary folder at test time: each must give a
# clear problem or flag, never a stopped run and never a value lost without a word.
#   Rscript tests/test_bad_files.R              (from the GT folder; also run by tests/run_tests.R)
#   GT_BAD_FILES_TABLE=1 Rscript ...            prints what GT does with each damaged file
# Text samples (one per text format in tests/samples) get every damage; binary samples are cut, emptied and left with their
# header only. All damaged files of one kind go into one raw folder and one run, beside one good file of another logger.
#   reader   read_logger_file() on the damaged file: its values, or the error it gives
#   run      the whole pipeline on the folder: the file's row in files.csv and file_audit.csv, problems.csv, audit_index.csv
# Core candidate: bad_variants() (damage by data row, for any line-based logger format given its row pattern and time column).

if (!exists("check")) {
  invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
  Sys.setenv(GT_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- if (file.exists("gt_pipeline.R")) "." else ".."; suppressMessages(source(file.path(here, "gt_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; BF_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("damaged raw files\n")

# How each line-based format lays out a data row: the row pattern, separator, time column and its format, the first value
# column (hcol: its column in the header, where that differs), the header block (lines before the first data row that name the
# columns), and what the logger writes for no value (nan_blank: the reader reads it as an empty cell, audited as missing_value).
BF_TEXT <- list(
  fg2_csv       = list(row = "^[0-9]+,", sep = ",", tcol = 2, tfmt = "%d.%m.%Y %H:%M:%S", vcol = 3, head = "^NO,TIME", nan = "(NoReply)"),
  hobo_csv      = list(row = "^[0-9]+,", sep = ",", tcol = 2, tfmt = "%m/%d/%y %I:%M:%S %p", vcol = 3, head = "Date Time", nan = "", nan_blank = TRUE),
  rbr_txt       = list(row = "^[^/][^\t]*\t[^\t]*\t[^\t]*\t[0-9]{4}-", sep = "\t", tcol = 4, tfmt = "%Y-%m-%dT%H:%M:%OS", tout = "%Y-%m-%dT%H:%M:%OS3", vcol = 8, hcol = 9, head = "yyyy-mm-dd", nan = "NaN", nan_blank = TRUE),
  campbell_toa5 = list(row = "^\"[0-9]{4}-", sep = ",", tcol = 1, tfmt = "%Y-%m-%d %H:%M:%S", vcol = 4, head = "^\"TOA5", nan = "\"NAN\"", sentinel = "-7999", quoted = TRUE))
BF_KNOWN <- list(header_changed = c(rbr_txt = "a second header line is read as a row and later values shift one channel left (Ruskin never writes one)",
                                    campbell_toa5 = "a second TOA5 header block (two tables pasted together) makes every time unreadable: the whole file is audited as impossible_time"))
BF_BIN_HEAD <- list(rbr_hex = function(l) l[seq_len(grep("^Number of bytes of data", l)[1])])   # a header a binary-ish file can be left with

bf_split <- function(x, s) strsplit(x, s, fixed = TRUE)[[1]]
bf_shift <- function(line, f, secs) { p <- bf_split(line, f$sep); q <- isTRUE(f$quoted); t <- gsub("\"", "", p[f$tcol])
  t2 <- as.POSIXct(strptime(t, f$tfmt, tz = "UTC")) + secs; s <- format(t2, f$tout %||% f$tfmt); p[f$tcol] <- if (q) paste0("\"", s, "\"") else s
  paste0(paste(p, collapse = f$sep), if (endsWith(line, f$sep)) f$sep else "") }
bf_set <- function(line, f, col, v) { p <- bf_split(line, f$sep); p[col] <- v; paste0(paste(p, collapse = f$sep), if (endsWith(line, f$sep)) f$sep else "") }
bf_drop <- function(line, f, col) { p <- bf_split(line, f$sep); paste0(paste(p[-col], collapse = f$sep), if (endsWith(line, f$sep)) f$sep else "") }

# The damaged versions of one text sample: name -> list(lines or bytes, rows = data rows it holds, chans = value columns per row
# (a vector when it changes), what = the outcome that counts as saying so).
bad_variants <- function(path, f) {
  l <- readLines(path, warn = FALSE); di <- grep(f$row, l); n <- length(di); mid <- di[n %/% 2]
  hb <- if (identical(f$head, "^\"TOA5")) 1:4 else max(grep(f$head, l[seq_len(di[1] - 1)]))
  v <- list()
  v$empty       <- list(bytes = raw(0), rows = 0L)
  v$header_only <- list(lines = l[seq_len(di[1] - 1)], rows = 0L)
  b <- readBin(path, "raw", file.size(path)); st <- c(1L, which(b == as.raw(10L)) + 1L)[mid]                    # cut inside the 2nd value of the middle row
  pf <- bf_split(l[mid], f$sep); cut_at <- st - 1L + nchar(paste0(paste(pf[seq_len(f$vcol)], collapse = f$sep), f$sep, sub("^(\\s*.{2}).*$", "\\1", pf[f$vcol + 1L])), "bytes")
  v$truncated   <- list(bytes = b[seq_len(cut_at)], rows = n %/% 2, cut = TRUE)
  ld <- append(l, bf_set(l[di[10]], f, f$vcol, "9.5"), after = di[10])                                    # one row twice, one time twice with another value
  ld <- append(ld, l[di[5]], after = di[5])
  v$duplicate_times <- list(lines = ld, rows = n + 2L, expect = c("clock_resync", "duplicate_value", "overlap_conflict"))
  lo <- l; lo[di[20:24]] <- l[di[24:20]]
  v$out_of_order <- list(lines = lo, rows = n)
  k <- which(di > mid); lh <- l; lh[di[k]] <- vapply(l[di[k]], bf_drop, "", f = f, col = f$vcol)              # a channel taken out partway, with a new header
  hdr <- l[hb]; hi <- if (length(hb) > 1) 2:4 else 1L; hdr[hi] <- vapply(hdr[hi], bf_drop, "", f = f, col = f$hcol %||% f$vcol)
  lh <- append(lh, hdr, after = di[k[1]] - 1L)
  v$header_changed <- list(lines = lh, rows = n, chans = c(rep(0L, n - length(k)), rep(-1L, length(k))))
  lb <- l; lb[di[k]] <- vapply(l[di[k]], bf_shift, "", f = f, secs = -3 * 86400)
  v$clock_jump_back <- list(lines = lb, rows = n, expect = c("clock_reset", "clock_resync"))
  la <- l; la[di[k]] <- vapply(l[di[k]], bf_shift, "", f = f, secs = 400 * 86400)
  v$clock_jump_ahead <- list(lines = la, rows = n, expect = "gap")
  ln <- l; ln[di[7]] <- bf_set(l[di[7]], f, f$vcol, f$nan); if (!is.null(f$sentinel)) ln[di[9]] <- bf_set(l[di[9]], f, f$vcol, f$sentinel)
  v$nan_and_codes <- list(lines = ln, rows = n, chans = if (isTRUE(f$nan_blank)) -1L else 0L, expect = c("sensor_error", "missing_value"), n_bad = 1L + !is.null(f$sentinel))
  v
}
bf_write <- function(x, to) if (!is.null(x$bytes)) writeBin(x$bytes, to) else writeLines(x$lines, to)

sd <- file.path(here, "tests", "samples"); smp <- list.files(sd, "__", full.names = TRUE); smp <- smp[!dir.exists(smp)]
kind <- sub("__.*$", "", basename(smp)); nm <- sub("^.*?__", "", basename(smp))
work <- tempfile("gt_bad"); dir.create(work)
cases <- list(); orig <- list()
good <- sub("1000004", "1000099", readLines(smp[kind == "hobo_csv"][1], warn = FALSE), fixed = TRUE)          # a good logger file in every run
for (i in seq_along(smp)) {
  if (kind[i] %in% names(BF_TEXT)) { vv <- bad_variants(smp[i], BF_TEXT[[kind[i]]])
    orig[[nm[i]]] <- read_logger_file(smp[i], kind[i])$data; nch <- uniqueN(orig[[nm[i]]][variable == "temp", channel]) } else {
    b <- readBin(smp[i], "raw", file.size(smp[i])); nch <- NA_integer_
    vv <- list(empty = list(bytes = raw(0), rows = 0L), truncated = list(bytes = b[seq_len(length(b) %/% 2)], rows = NA_integer_, cut = TRUE))
    if (kind[i] %in% names(BF_BIN_HEAD)) vv$header_only <- list(lines = BF_BIN_HEAD[[kind[i]]](readLines(smp[i], warn = FALSE)), rows = 0L) }
  for (d in names(vv)) cases[[length(cases) + 1L]] <- c(vv[[d]], list(file = nm[i], kind = kind[i], damage = d, nch = nch))
}

# reader: the damaged file read on its own
# reader: the damaged file read on its own. same_last: the last row's values sit in the same channels as in the good file.
read_case <- function(cs, p) { r <- tryCatch(read_logger_file(p, sniff_format(p)), error = function(e) conditionMessage(e))
  if (is.character(r)) return(list(error = r, temp = NA_integer_, flagged = NA_integer_, cut_flag = NA, same_last = NA))
  d <- r$data %||% data.table(variable = character(), err = character(), value = numeric(), ord = integer())
  t <- d[variable == "temp" & (!is.na(value) | !is.na(err))]; o <- orig[[cs$file]]
  sl <- if (is.null(o) || !nrow(t)) NA else { a <- merge(t[ord == max(ord), .(channel, value)], o[variable == "temp" & ord == max(ord), .(channel, value)], by = "channel")[!is.na(value.x)]
    nrow(a) > 0 && all(abs(a$value.x - a$value.y) < 1e-9, na.rm = TRUE) }
  list(error = "", temp = nrow(t), flagged = sum(!is.na(t$err)), cut_flag = if (nrow(t)) any(!is.na(t[ord == max(ord), err])) else NA, same_last = sl) }

res <- list()
for (dm in unique(vapply(cases, `[[`, "", "damage"))) {
  raw <- file.path(work, dm); dir.create(raw); cs <- Filter(function(x) x$damage == dm, cases)
  for (x in cs) bf_write(x, file.path(raw, x$file)); writeLines(good, file.path(raw, "1000099_Good.csv"))
  rd <- lapply(cs, function(x) read_case(x, file.path(raw, x$file)))
  p <- file.path(work, paste0("p_", dm))
  run <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", project_dir = p, make_plots = FALSE))))); "" },
                  error = function(e) conditionMessage(e))
  a <- file.path(p, "output", "audit"); rf <- function(f) if (file.exists(file.path(a, f)) && file.size(file.path(a, f)) > 0) fread(file.path(a, f), colClasses = "character") else data.table()
  fl <- rf("files.csv"); fa <- rf("file_audit.csv"); pr <- rf("problems.csv"); ix <- rf("audit_index.csv"); gp <- rf("gaps.csv")
  for (j in seq_along(cs)) { x <- cs[[j]]; f <- x$file; s <- if (nrow(fa)) fa[fa$file == f] else data.table(); lid_rules <- if (nrow(s) && nrow(ix)) ix[logger_id %in% paste(TYPE_LABEL[s$type], s$serial), unique(rule)] else character()
    res[[length(res) + 1L]] <- data.table(damage = dm, kind = x$kind, file = f, run_error = run, reader_error = rd[[j]]$error,
      temp_read = rd[[j]]$temp, temp_expected = if (is.na(x$nch) || is.na(x$rows)) NA_integer_ else as.integer(x$rows * x$nch + sum(x$chans %||% 0L)),
      temp_flagged = rd[[j]]$flagged, last_row_flagged = rd[[j]]$cut_flag, same_last = rd[[j]]$same_last, n_bad = x$n_bad %||% 0L,
      status = if (nrow(fl)) fl$status[fl$file == f][1] else NA_character_, used = if (nrow(s)) s$used[1] else NA_character_,
      read = if (nrow(s)) s$read[1] else NA_character_, in_output = if (nrow(s)) s$in_output[1] else NA_character_, dropped = if (nrow(s)) s$dropped[1] else NA_character_,
      reconciles = if (nrow(s)) s$reconciles[1] else NA_character_, problem = if (nrow(pr)) paste(pr[grepl(f, where, fixed = TRUE) | grepl(f, problem, fixed = TRUE), problem], collapse = " / ") else "",
      rules = paste(lid_rules, collapse = ";"), gaps = if (nrow(gp) && nrow(s)) nrow(gp[logger_id == paste(TYPE_LABEL[s$type], s$serial)]) else 0L,
      expect = paste(x$expect %||% "", collapse = "|"), cut = isTRUE(x$cut), nch_rows = if (is.na(x$nch)) NA_integer_ else x$nch) }
}
bf <- rbindlist(res); unlink(work, recursive = TRUE)
if (identical(Sys.getenv("GT_BAD_FILES_TABLE"), "1")) { op <- options(width = 250); print(bf[, !"run_error"], nrows = 500); print(unique(bf[, .(damage, run_error)])); options(op) }

# what counts as saying so: the file is reported (a problem naming it, or a status other than ok), or flagged in the audit
told <- function(x) nzchar(x$problem) || !x$status %in% c("ok", NA) || nzchar(x$rules)
check(paste0("no damaged file stops a run (", uniqueN(bf$damage), " runs, ", nrow(bf), " damaged files, each run with one good file)"), all(!nzchar(bf$run_error)))
x <- bf[damage %in% c("empty", "header_only")]
check("an empty file or one left with its header only is reported, not taken as data", nrow(x) > 0 && all(vapply(seq_len(nrow(x)), function(i) told(x[i]), NA)) && !any(x$used == "TRUE"))
u <- bf[used == "TRUE"]
check("every damaged file read adds up: read = in the output + dropped with a reason", nrow(u) > 0 && all(u$reconciles == "TRUE") && all(as.integer(u$read) == as.integer(u$in_output) + as.integer(u$dropped)))
x <- bf[!is.na(temp_expected) & damage %in% c("duplicate_times", "out_of_order", "clock_jump_back", "clock_jump_ahead", "nan_and_codes")]
check("no value is left out by a reader: every temperature in a damaged text file is read", nrow(x) > 0 && all(x$temp_read == x$temp_expected))
x <- bf[damage == "duplicate_times" & !is.na(temp_expected)]
check("a time written twice in a file is audited: the earlier row leaves with the clock-set-back reason, the later is kept",
      nrow(x) > 0 && all(grepl("clock_resync|duplicate_value|overlap_conflict", x$rules)) && all(as.integer(x$dropped) >= 2L * x$nch_rows))
x <- bf[damage == "out_of_order" & !is.na(temp_expected)]
check("rows whose times step back are audited as a clock set back, none lost without a reason", nrow(x) > 0 && all(grepl("clock_resync|clock_reset", x$rules)) && all(x$reconciles == "TRUE"))
x <- bf[damage == "clock_jump_back" & !is.na(temp_expected)]
check("a clock set back by days is audited as a clock reset or resync", nrow(x) > 0 && all(grepl("clock_reset|clock_resync", x$rules)))
x <- bf[damage == "clock_jump_ahead" & !is.na(temp_expected)]
check("a clock jump forward leaves a gap that is listed", nrow(x) > 0 && all(x$gaps > 0))
x <- bf[damage == "nan_and_codes" & !is.na(temp_expected)]
check("NaN and the logger's no-value codes are values with a reason, never numbers", nrow(x) > 0 && all(x$temp_flagged >= x$n_bad | grepl("missing_value", x$rules)) && all(grepl("sensor_error|missing_value", x$rules)))
x <- bf[damage == "header_changed" & !is.na(temp_expected)]; kn <- BF_KNOWN$header_changed
check("a header that changes partway (an FG2-Shell export over a new parameter set): each row read by the header above it, values in their own channels, or the file reported",
      nrow(x[!kind %in% names(kn)]) > 0 && all(x[!kind %in% names(kn), (temp_read == temp_expected & same_last %in% TRUE) | (nzchar(problem) & used != "TRUE") |
                                                (kind == "hobo_csv" & nzchar(problem) & temp_read > 0 & temp_read < temp_expected)]))
x <- bf[damage == "header_changed" & kind == "hobo_csv"]
check("a HOBOware CSV with two exports joined: the rows up to the second heading are read and the rest are listed as a problem (as WLR)",
      nrow(x) == 1 && x$temp_read > 0 && x$temp_read < x$temp_expected && grepl("second header with other columns", x$problem, fixed = TRUE))
for (k in intersect(names(kn), x$kind)) cat("  known ", k, ": ", kn[[k]], "\n", sep = "")
x <- bf[damage == "truncated" & !is.na(temp_expected)]
check("a text file cut off inside a row: the cut row is flagged or reported, not read as a whole value", nrow(x) > 0 && all(x$last_row_flagged %in% TRUE | nzchar(x$problem)))
x <- bf[damage == "truncated" & is.na(temp_expected)]
check("a binary file cut in half is read up to the cut or reported", nrow(x) > 0 && all(!nzchar(x$run_error)) && all(vapply(seq_len(nrow(x)), function(i) told(x[i]) || x$used[i] == "TRUE", NA)))
local({ raw <- tempfile("gt_bad_only"); dir.create(raw); file.create(file.path(raw, "1000004_A.csv")); writeLines(good[1:2], file.path(raw, "1000099_B.csv"))
  r <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", project_dir = tempfile("p"), make_plots = FALSE))))); "" }, error = function(e) conditionMessage(e))
  check("a folder whose files are all empty or header only stops with a message naming each file", grepl("holds readings", r) && grepl("1000004_A.csv", r, fixed = TRUE) && grepl("1000099_B.csv", r, fixed = TRUE)) })

if (exists("BF_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
