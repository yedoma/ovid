# Ovid/testkit.R  --  Reader tests on real sample files, shared by the apps' tests. Not sourced by any app or pipeline.
#
# Each app keeps tests/samples/: one small real file per format its readers know (named <format>__<file>), and in
# tests/samples/expected/ what its reader made of each (.rds). A test reads every sample twice: the two reads must be the
# same (a rerun gives identical output) and must equal the expected result. After a deliberate change to a reader, rerun the
# tests with OVID_UPDATE_SAMPLES=1 to save the new results, and check the change in the diff of what they hold.
# A saved result is only a record of what the reader made, not proof that it is right. So a sample also has to be accepted:
# tests/samples/accepted.csv (file, expected_md5, checked_by, checked_on, note) names each sample whose expected .rds a
# person has checked, with the md5 of that .rds. A new sample, or one whose expected result was saved again after a reader
# change, fails until someone looks at the .rds and calls accept_sample(). tests/samples/reference.csv (file, what, value,
# source) holds values checked by hand outside the reader (from the raw file, or the logger's own software), and each one
# must equal what the reader makes: what is rows, first_time, last_time (times as "YYYY-MM-DD HH:MM:SS") or
# col:<name>@<row> (column <name> in row <row> of the reader's table); numbers compare within 1e-6 relative.
#   kind_of(path)       the format a file is (NA or "" to leave it out): the app's own sniffer
#   read(path, kind)    what the app's reader returns for it

# Paths in a result as the file's name only, and the parts that change between machines and runs left out, so a result
# compares equal wherever it was made.
sample_strip <- function(x, root) {
  drop <- c("path", "mtime", "size", "md5", "read_at", "ctime")
  if (is.data.frame(x)) { x <- copy(as.data.table(x)); k <- intersect(names(x), drop); if (length(k)) x[, (k) := NULL]
    for (j in names(x)) if (is.character(x[[j]])) set(x, j = j, value = sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", root), "/?"), "", x[[j]]))
    return(x[]) }
  if (is.list(x)) { x <- x[!names(x) %in% drop | is.null(names(x))]; return(lapply(x, sample_strip, root = root)) }
  if (is.character(x)) return(sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", root), "/?"), "", x))
  x
}
rows_of <- function(r) { d <- if (is.data.frame(r)) r else if (is.list(r)) Filter(is.data.frame, r)[1][[1]] else NULL; if (is.null(d)) 0L else nrow(d) }

# Pick the samples: for each format, the smallest file the reader reads with at least one row; a line-based text file is cut
# to its first max_lines lines when it still reads that way, any other file is copied whole up to max_bytes.
#   no_rows  formats whose reader gives a header only (a binary file the app does not decode): readable without rows
make_reader_samples <- function(files, dest, kind_of, read, trim = character(), max_lines = 60L, max_bytes = 1.5e6, no_rows = character()) {
  dir.create(file.path(dest, "expected"), recursive = TRUE, showWarnings = FALSE)
  files <- files[file.exists(files) & !startsWith(basename(files), ".")]
  files <- files[order(file.size(files))]
  kinds <- vapply(files, function(f) { k <- tryCatch(kind_of(f), error = function(e) NA_character_); if (length(k) != 1 || is.na(k)) "" else as.character(k) }, "")
  out <- character()
  for (k in setdiff(unique(kinds), c("", "unknown", "empty"))) for (f in files[kinds == k]) {
    ok <- function(p) isTRUE(tryCatch({ r <- read(p, k); rows_of(r) > 0 || (k %in% no_rows && length(r) > 0) }, error = function(e) FALSE))
    to <- file.path(dest, paste0(k, "__", gsub("[^A-Za-z0-9_.-]+", "_", basename(f))))
    if (k %in% trim) { l <- readLines(f, warn = FALSE); writeLines(utils::head(l, max_lines), to)
      if (ok(to)) { out <- c(out, to); break }; unlink(to) }
    if (file.size(f) <= max_bytes && ok(f)) { file.copy(f, to, overwrite = TRUE); out <- c(out, to); break }
  }
  invisible(out)
}

# The table a reader returns (itself, or the first table in its list), the column with its times, and the value of one
# reference.csv `what` in it, as text.
sample_table <- function(r) if (is.data.frame(r)) r else if (is.list(r)) Filter(is.data.frame, r)[1][[1]] else NULL
sample_value <- function(r, what) { d <- sample_table(r); if (is.null(d)) return(NA_character_)
  tx <- function(v) if (inherits(v, "POSIXct")) format(v, "%Y-%m-%d %H:%M:%S", tz = attr(v, "tzone") %||% "UTC") else trimws(as.character(v))
  if (what == "rows") return(as.character(nrow(d)))
  if (what %in% c("first_time", "last_time")) {
    tc <- intersect(c("TIMESTAMP", "datetime"), names(d))[1]; if (is.na(tc)) tc <- names(d)[vapply(d, inherits, NA, "POSIXct")][1]
    if (is.na(tc)) return(NA_character_); v <- d[[tc]]; v <- v[!is.na(v)]; if (is.character(v)) v <- v[nzchar(v)]
    if (!length(v)) return(NA_character_); return(tx(if (what == "first_time") min(v) else max(v))) }
  m <- regmatches(what, regexec("^col:(.+)@([0-9]+)$", what))[[1]]
  if (length(m) != 3 || !m[2] %in% names(d) || as.integer(m[3]) > nrow(d)) return(NA_character_)
  tx(d[[m[2]]][as.integer(m[3])])
}
sample_value_ok <- function(want, got) { if (is.na(got)) return(FALSE)
  w <- suppressWarnings(as.numeric(want)); g <- suppressWarnings(as.numeric(got))
  if (!is.na(w) && !is.na(g)) abs(g - w) <= 1e-6 * abs(w) else identical(trimws(want), trimws(got)) }
SAMPLE_ACCEPT_COLS <- c("file", "expected_md5", "checked_by", "checked_on", "note")
SAMPLE_REF_COLS <- c("file", "what", "value", "source")
read_sample_table <- function(f, cols) {
  x <- if (file.exists(f) && file.size(f) > 0) fread(f, colClasses = "character", na.strings = character()) else data.table()
  for (k in setdiff(cols, names(x))) set(x, j = k, value = character(nrow(x))); x[, cols, with = FALSE] }

# Times compared to the millisecond: all.equal() treats a POSIXct as a plain number with a relative tolerance of 1.5e-8, which
# lets about 26 s through at today's dates (an 18 s shift once passed unseen). Walks lists and tables; other values are left to all.equal().
sample_times_same <- function(x, y) {
  tm <- function(v) inherits(v, c("POSIXt", "Date"))
  if (tm(x) || tm(y)) return(tm(x) && tm(y) && length(x) == length(y) && isTRUE(all(is.na(x) == is.na(y))) &&
                             all(abs(as.numeric(x[!is.na(x)]) - as.numeric(y[!is.na(y)])) <= if (inherits(x, "Date")) 0 else 1e-3))
  if (is.list(x) && is.list(y)) { k <- if (!is.null(names(x)) && !is.null(names(y))) intersect(names(x), names(y)) else seq_len(min(length(x), length(y)))
    return(all(vapply(k, function(i) sample_times_same(x[[i]], y[[i]]), NA))) }
  TRUE
}

# Read every sample twice and against its expected result. One row per sample: rerun_identical, matches_expected (NA where
# the expected result was just written), accepted (named in accepted.csv with the md5 of its expected .rds as it is now)
# and reference_ok (every reference.csv value for it matches; NA where it has none).
check_reader_samples <- function(dir, read, update = identical(Sys.getenv("OVID_UPDATE_SAMPLES"), "1")) {
  f <- list.files(dir, pattern = "__", full.names = TRUE); f <- f[!dir.exists(f)]
  if (!length(f)) return(data.table(file = character(), kind = character(), rows = integer(), rerun_identical = logical(), matches_expected = logical(),
                                    accepted = logical(), reference_ok = logical()))
  acc <- read_sample_table(file.path(dir, "accepted.csv"), SAMPLE_ACCEPT_COLS); ref <- read_sample_table(file.path(dir, "reference.csv"), SAMPLE_REF_COLS)
  dir.create(file.path(dir, "expected"), showWarnings = FALSE)
  rbindlist(lapply(f, function(p) { k <- sub("__.*$", "", basename(p))
    a <- tryCatch(sample_strip(read(p, k), dir), error = function(e) structure(list(conditionMessage(e)), class = "read_error"))
    b <- tryCatch(sample_strip(read(p, k), dir), error = function(e) structure(list(conditionMessage(e)), class = "read_error"))
    ef <- file.path(dir, "expected", paste0(basename(p), ".rds")); bad <- inherits(a, "read_error")
    same <- !bad && file.exists(ef) && { e <- readRDS(ef); isTRUE(all.equal(e, a, check.attributes = FALSE)) && sample_times_same(e, a) }
    m <- if (bad) FALSE else if (same) TRUE else if (update || !file.exists(ef)) { saveRDS(a, ef, version = 2); NA } else FALSE   # an unchanged result is not saved again, so it stays accepted
    ar <- acc[file == basename(p)]
    ok_acc <- file.exists(ef) && nrow(ar) == 1 && identical(ar$expected_md5, unname(tools::md5sum(ef)))
    rr <- ref[file == basename(p)]
    ref_ok <- if (!nrow(rr)) NA else if (bad) FALSE else all(mapply(function(w, v) sample_value_ok(v, sample_value(a, w)), rr$what, rr$value))
    data.table(file = basename(p), kind = k, rows = if (bad) NA_integer_ else rows_of(a), rerun_identical = identical(a, b), matches_expected = m,
               accepted = ok_acc, reference_ok = ref_ok) }))
}

# Accept a sample after looking at its expected .rds: writes or updates its accepted.csv row with today's date and the md5 of
# the .rds as it is now. Called by hand, never by a test.
accept_sample <- function(dir, file, checked_by, note = "") {
  ef <- file.path(dir, "expected", paste0(basename(file), ".rds")); if (!file.exists(ef)) stop("no expected result for ", basename(file), ": run the tests first")
  af <- file.path(dir, "accepted.csv"); acc <- read_sample_table(af, SAMPLE_ACCEPT_COLS); nm <- basename(file)
  acc <- rbind(acc[acc$file != nm], data.table(file = nm, expected_md5 = unname(tools::md5sum(ef)), checked_by = checked_by, checked_on = format(Sys.Date()), note = note))
  fwrite(acc[order(file)], af); invisible(acc)
}

# ---- damaged input: a run must not stop on a damaged raw file, and stops on a damaged set-up table only to say which ----
# Each sample is put in a raw folder of its own with the other samples, damaged one way, and run once in a fresh project.
# run(raw, project) runs the app's pipeline. Returns one row per sample and damage with the run's result ("ok", or the error).
# DAMAGE: what is done to the file. The quick set runs by default; OVID_FUZZ=1 runs all of them.
DAMAGE <- list(
  empty       = function(f, o) file.create(o),
  half        = function(f, o) writeBin(readBin(f, "raw", file.size(f))[seq_len(file.size(f) %/% 2)], o),
  garbage     = function(f, o) { set.seed(1); writeBin(as.raw(sample(0:255, 3000, TRUE)), o) },
  blank_lines = function(f, o) { b <- readBin(f, "raw", file.size(f)); if (mean(b[seq_len(min(2000, length(b)))] == as.raw(0)) < 0.01) writeLines(as.vector(rbind(readLines(f, warn = FALSE), "")), o) else writeBin(b, o) },
  doubled     = function(f, o) { b <- readBin(f, "raw", file.size(f)); if (mean(b[seq_len(min(2000, length(b)))] == as.raw(0)) < 0.01) { l <- readLines(f, warn = FALSE); writeLines(c(l, l), o) } else writeBin(b, o) },
  unreadable  = function(f, o) { file.copy(f, o); Sys.chmod(o, "000") })
DAMAGE_QUICK <- c("half", "blank_lines", "unreadable")
damaged_file_runs <- function(samples_dir, run, work = tempfile("damage"), damage = if (identical(Sys.getenv("OVID_FUZZ"), "1")) names(DAMAGE) else DAMAGE_QUICK) {
  s <- list.files(samples_dir, pattern = "__", full.names = TRUE); s <- s[!dir.exists(s)]
  nm <- function(f) sub("^.*?__", "", basename(f)); dir.create(work, recursive = TRUE); out <- list(); k <- 0L
  for (f in s) for (v in damage) { k <- k + 1L; raw <- file.path(work, paste0("raw", k)); dir.create(raw)
    for (g in setdiff(s, f)) file.copy(g, file.path(raw, nm(g)))
    o <- file.path(raw, nm(f)); tryCatch(DAMAGE[[v]](f, o), error = function(e) NULL)
    r <- tryCatch({ run(raw, file.path(work, paste0("p", k))); "ok" }, error = function(e) conditionMessage(e))
    Sys.chmod(o, "644"); out[[k]] <- data.table(file = nm(f), damage = v, result = r) }
  unlink(work, recursive = TRUE); rbindlist(out)
}
# Each set-up table of a project that has run once, damaged (binary content, or emptied) and run again.
damaged_table_runs <- function(project, run_again, damage = c("garbage", "empty")) {
  cd <- file.path(project, "config"); bk <- tempfile("cfg"); dir.create(bk); file.copy(list.files(cd, "\\.csv$", full.names = TRUE), bk)
  out <- list()
  for (t in list.files(bk)) for (v in damage) {
    file.copy(list.files(bk, full.names = TRUE), cd, overwrite = TRUE); f <- file.path(cd, t)
    if (v == "garbage") { set.seed(1); writeBin(as.raw(sample(0:255, 500, TRUE)), f) } else file.create(f)
    r <- tryCatch({ run_again(); "ok" }, error = function(e) conditionMessage(e))
    out[[length(out) + 1L]] <- data.table(table = t, damage = v, result = r) }
  file.copy(list.files(bk, full.names = TRUE), cd, overwrite = TRUE); unlink(bk, recursive = TRUE)
  rbindlist(out)
}
