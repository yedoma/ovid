# Ovid/logger_files.R  --  Finding raw logger files and reading the headers of the formats more than one app reads.
# Part of Ovid Core (shared). Sourced by the processors' pipelines (CS, GT, WLR) before their own R/ files; not meant to be
# run on its own.
#
# Only what the apps had in common is here: listing the raw folders, checksums that are reused while a file is unchanged,
# byte-identical copies, and the header (identity, columns, units, time zone) of a HOBOware CSV, a .hobo file and a Campbell
# TOA5 table. What a file's columns mean, and which of them an app keeps, stays in the app's own reader.

# ---- the raw folders ----------------------------------------------------------------------------------------------------
# Every file under the folders (subfolders too), as one row each: path (absolute, "/"), root (the folder given), rel (the path
# under it), file, size, mtime. pattern: a regular expression on the file name (NULL = any). Files the system or another app
# leaves behind (._ forks, .DS_Store, Office lock files) and the folders `skip_dirs` (a project's own output inside the raw
# folder) are left out. A path listed under two folders appears once, under the first.
RAW_SKIP_FILES <- "^(~\\$|\\._|\\.DS_Store)"
nfc <- function(x) if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_nfc(x) else x      # one Unicode spelling of a name
raw_file_list <- function(dirs, pattern = NULL, skip_dirs = character()) {
  dirs <- path.expand(dirs[!is.na(dirs) & nzchar(dirs)]); dirs <- dirs[dir.exists(dirs)]
  rows <- lapply(dirs, function(d) { f <- list.files(d, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    f <- f[!grepl(RAW_SKIP_FILES, basename(f))]
    if (length(skip_dirs)) f <- f[!grepl(paste0("(^|/)(", paste(skip_dirs, collapse = "|"), ")/"), substring(f, nchar(d) + 2L))]
    if (!length(f)) return(NULL)
    data.table(path = normalizePath(f, winslash = "/"), root = d, rel = substring(f, nchar(d) + 2L), file = basename(f)) })
  m <- unique(rbindlist(rows), by = "path")
  if (!nrow(m)) return(data.table(path = character(), root = character(), rel = character(), file = character(), size = numeric(), mtime = as.POSIXct(character())))
  fi <- file.info(m$path); m[, `:=`(size = fi$size, mtime = fi$mtime)]
  m[, rel := nfc(rel)]                                   # accented names as one spelling on macOS and Windows (file rules match rel)
  long <- m$path[nchar(m$path) > 250]                    # Windows reads paths over 260 characters only where long paths are switched on
  if (length(long) && .Platform$OS.type == "windows" && exists("problem")) problem("raw folder", paste0(length(long), " file path(s) are over 250 characters and may not be read on Windows: ",
                                                                                                          paste(utils::head(basename(long), 3), collapse = ", ")))
  m[]
}

# ---- checksums and what is known about unchanged files -------------------------------------------------------------------
# A file is unchanged while its path, size and modification time are (and the code that read it). file_known() reads
# what an earlier run kept about each (a list by signature, or an empty list); raw_md5() takes the checksum from there and
# works out only the others.
file_known <- function(cache_file) if (nzchar(cache_file %||% "") && file.exists(cache_file)) tryCatch(readRDS(cache_file), error = function(e) list()) else list()
raw_md5 <- function(path, sig = NULL, known = list()) {
  m <- if (length(known) && !is.null(sig)) vapply(sig, function(x) known[[x]]$md5 %||% NA_character_, "", USE.NAMES = FALSE) else rep(NA_character_, length(path))
  w <- is.na(m); if (any(w)) m[w] <- unname(tools::md5sum(path[w])); m
}

# "Rescan": what is known about these files (absolute paths) is dropped, so the next run reads them again.
file_known_drop <- function(cache_file, paths) {
  if (file.exists(cache_file)) tryCatch({ h <- readRDS(cache_file); h <- h[!vapply(names(h), function(x) any(startsWith(x, paste0(paths, "|"))), logical(1))]
    saveRDS(h, cache_file) }, error = function(e) unlink(cache_file))
  invisible()
}

# Byte-identical copies: in the order the rows are in, the first of each checksum is kept and the others point to it
# (dup_of). ok: the rows that take part. Returns dup_of (NA for a kept file and for rows not taking part).
copy_of <- function(md5, path, ok = rep(TRUE, length(md5))) {
  ok <- ok & !is.na(md5)                                  # a file that could not be read has no checksum: it is not a copy of anything
  first <- path[ok][match(md5, md5[ok])]
  ifelse(ok & !is.na(first) & first != path, first, NA_character_)
}

# ---- HOBOware ------------------------------------------------------------------------------------------------------------
# The .hobo binary is Onset's closed format: only its plain-text identity is read (model, serial, title, time zone), so the
# file can be paired with its HOBOware CSV export or listed with the logger it came from.
hobo_bin_identity <- function(path) {
  raw <- readBin(path, "raw", min(file.info(path)$size, 4096))
  raw[as.integer(raw) < 32L | as.integer(raw) > 126L] <- as.raw(10)          # printable ASCII runs only, like `strings`
  parts <- trim(strsplit(rawToChar(raw), "\n", fixed = TRUE)[[1]]); parts <- parts[nchar(parts) >= 4]
  parts <- sub("^[0-9]%(?=HOBOware)", "", parts, perl = TRUE)
  model <- parts[grepl("^HOBO ", parts)][1]; ser <- parts[grepl("^[0-9]{6,10}$", parts)][1]
  ttl <- if (!is.na(ser)) { i <- match(ser, parts); if (i < length(parts)) parts[i + 1] else NA } else NA
  zone <- parts[grepl("GMT|Time$", parts)][1]
  list(serial = ser, model = model, title = ttl, zone = zone, offset_hours = parse_offset(zone %||% ""), size = file.info(path)$size)
}

# A HOBOware CSV export: line 1 "Plot Title: ..." (sometimes left out), then the column headings ("#", "Date Time, GMT-07:00",
# "Abs Pres, kPa (LGR S/N: 20004562, SEN S/N: ...)", ...), then the records. lines: the file's lines (each app reads them its
# own way). Returns the heading line (hi), its separator, the headings, the date-time column, its time zone, the plot title
# and the logger serial from "LGR S/N" (NA when no heading has one).
hobo_csv_head <- function(lines) {
  hi <- which(grepl("Date Time", lines, fixed = TRUE))[1]
  if (is.na(hi)) stop("no Date Time column")
  sep <- if (grepl("\t", lines[hi], fixed = TRUE)) "\t" else ","
  hdr <- trim(scan(text = lines[hi], what = character(), sep = sep, quote = "\"", quiet = TRUE, na.strings = NULL))
  dcol <- grep("^Date Time", hdr)[1]
  title <- if (hi > 1 && grepl("Plot Title", lines[1], fixed = TRUE)) sub("^\"?Plot Title:\\s*", "", sub("\"?[\t,]*$", "", trim(lines[1]))) else NA_character_
  sn <- regmatches(hdr, regexpr("LGR S/N: *[0-9]+", hdr))
  list(hi = hi, sep = sep, header = hdr, dcol = dcol, offset_hours = parse_offset(hdr[dcol]), title = title,
       serial = if (length(sn)) sub(".*: *", "", sn[1]) else NA_character_)
}

# ---- Campbell Scientific TOA5 --------------------------------------------------------------------------------------------
# The four header lines of a TOA5 table: the environment line (format, station name, model, serial, OS, program, signature,
# table), the column names, units and processing, blank lines left aside (toa5_skip(): how many lines the data starts after).
# A table saved from a spreadsheet is often tab-separated and unquoted; a
# header with no station name (7 fields, model second) has it filled in as "". status "ok", or why it cannot be read.
toa5_head <- function(path) {
  h <- fix_enc(suppressWarnings(readLines(path, n = 40L, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
  h <- utils::head(h[nzchar(trimws(h))], 4L)                                   # blank lines (a file saved with extra line breaks) do not count
  first <- if (length(h)) fix_enc(h[1]) else ""
  if (!grepl("^\"?TOA5\"?[,\t]", first)) return(list(status = "not_toa5"))
  sep <- if (grepl("^\"?TOA5\"?\t", first)) "\t" else ","
  if (length(h) < 2) return(list(status = "truncated_header", sep = sep))
  env <- drop_trailing_blank(parse_csv_line(h[1], sep))
  if (length(env) == 7 && grepl("^CR[0-9]", env[2]) && grepl("^[0-9]+$", env[3])) env <- append(env, "", after = 1L)
  length(env) <- 8; env[is.na(env)] <- ""
  cols <- drop_trailing_blank(parse_csv_line(h[2], sep))
  if (!length(cols)) return(list(status = "truncated_header", sep = sep, env = env))
  pad <- function(x) { x <- c(x, rep("", length(cols)))[seq_along(cols)]; x[is.na(x)] <- ""; x }
  list(status = "ok", sep = sep, env = env, station_name = env[2], model = env[3], serial = env[4], os = env[5], program = env[6],
       signature = env[7], table = env[8], columns = cols,
       units = pad(if (length(h) >= 3) parse_csv_line(h[3], sep) else character()),
       process = pad(if (length(h) >= 4) parse_csv_line(h[4], sep) else character()))
}

toa5_skip <- function(path) {
  h <- fix_enc(suppressWarnings(readLines(path, n = 40L, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
  k <- which(nzchar(trimws(h))); if (length(k) >= 4L) k[4L] else 4L
}

# ---- gaps ------------------------------------------------------------------------------------------------------------------
# Holes in a record (times in order, one logger table or channel): a step longer than `factor` x the logging interval
# `step` (seconds). One row per hole: its start and end (the records either side), its length in seconds and how many records
# would fill it. NULL when there is none. Each app writes them in its own columns.
gap_rows <- function(t, step, factor = 1.5) {
  g <- diff(as.numeric(t)); k <- which(g > factor * step)
  if (!length(k)) return(NULL)
  data.table(gap_start = t[k], gap_end = t[k + 1L], gap_s = g[k], missing_records = round(g[k] / step) - 1)
}
