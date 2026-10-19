# R/readers.R  --  Reading logger files: RBR (.hex, Ruskin .txt and .xls), GeoPrecision FG2 (.csv), Onset HOBO (HOBOware .csv
# and the .hobo header), METER/Decagon EM50 (.xls) and Campbell Scientific (TOA5 .dat). The GeoPrecision .f2b itself is in R/f2b.R.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# Every reader returns the same shape:
#   meta    list: format, type, serial, model, title, offset_hours (the zone the file's clock is in; NA = the logger's own clock),
#           interval_s, rows, download (when the file was downloaded or exported, if known) and any notes
#   data    data.table, long: TIMESTAMP (as written in the file), ord (row order in the file), channel, variable (temp, vwc, ec,
#           battery), value, err (why a value is missing: an error code from the logger; NA otherwise)
#   events  data.table: TIMESTAMP, event   (GeoPrecision tags, HOBO event columns, RBR header times)
# Raw files are only ever read.

empty_data <- function() data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC"), ord = integer(), channel = character(), variable = character(),
                                    value = numeric(), err = character())
empty_events <- function() data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC"), event = character())

# wide (TIMESTAMP, ord, one column per channel) -> long
melt_channels <- function(w, chans, variable, err = NULL) {
  if (!length(chans) || !nrow(w)) return(empty_data())
  out <- rbindlist(lapply(seq_along(chans), function(k) {
    v <- w[[chans[k]]]
    data.table(TIMESTAMP = w$TIMESTAMP, ord = w$ord, channel = chans[k], variable = variable[k], value = suppressWarnings(as.numeric(v)),
               err = if (is.null(err)) NA_character_ else err[[k]])
  }))
  out
}

# A text file that does not end with a line break was cut off (a copy or download that stopped): its last row can hold a value
# cut short ("23.5" of 23.52), so that row's values are marked, not read as whole. Every logger and software export seen ends
# with a line break (2,394 files checked).
CUT_ERR <- "last row cut short (the file ends inside a row)"
ends_cut <- function(path) { n <- file.info(path)$size; if (is.na(n) || n == 0) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con)); seek(con, n - 1); !identical(readBin(con, "raw", 1L), as.raw(10L)) }
mark_cut <- function(long, path) { if (nrow(long) && ends_cut(path)) long[ord == max(ord) & is.na(err), `:=`(value = NA_real_, err = CUT_ERR)]; long }

# ---- what kind of file is this? -------------------------------------------------
sniff_format <- function(path) {
  ext <- tolower(tools::file_ext(path)); size <- file.info(path)$size
  if (is.na(size)) return("unreadable")
  if (size == 0) return("empty_file")
  if (file.access(path, 4L) != 0L) return("unreadable")                           # no permission, or the drive went away
  first <- tryCatch(read_lines_any(path, 3), error = function(e) character())
  f1 <- if (length(first)) first[1] else ""
  switch(ext,
    hex  = if (startsWith(f1, "RBR")) "rbr_hex" else "unrecognised_format",
    rsk  = "rbr_rsk",
    f2b  = { h <- readBin(path, "raw", 2L)                                       # an .f2b starts with a marker (fe 02-06)
             if (length(h) == 2L && h[1] == as.raw(0xfe) && as.integer(h[2]) >= 2L) "fg2_f2b" else if (length(h) && h[1] == as.raw(0xfe)) "fg2_binary" else "unrecognised_format" },
    log  = if (startsWith(f1, "<")) "fg2_log" else "unrecognised_format",
    hobo = "hobo_binary",
    dat  = if (grepl("^\"?TOA5", f1)) "campbell_toa5" else if (grepl("^\"?TOB", f1)) "campbell_binary" else if (grepl("^[0-9]+,", f1)) "campbell_array" else "unrecognised_format",
    txt  = if (any(grepl("<Creator>RBR", first, fixed = TRUE))) "rbr_txt" else "unrecognised_format",
    csv  = if (startsWith(f1, "<FG2")) "fg2_csv" else if (grepl("Plot Title", f1, fixed = TRUE) || grepl("Date Time", f1, fixed = TRUE) ||
               (length(first) > 1 && grepl("Date Time", first[2], fixed = TRUE))) "hobo_csv" else "unrecognised_format",
    xls  = , xlsx = {
      sh <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
      if (any(grepl("^Em50|Unprocessed Data", sh))) "em50_xls"
      else if (length(sh)) { a <- tryCatch(readxl::read_excel(path, sheet = 1, col_names = FALSE, n_max = 1, .name_repair = "minimal"), error = function(e) NULL)
        if (!is.null(a) && ncol(a) && grepl("RBR", as.character(a[[1]][1]))) "rbr_xls" else "unrecognised_format" }
      else "unreadable" },
    "unrecognised_format")
}

# Which logger family a format belongs to, and whether the app can read it.
FORMAT_INFO <- fread(sep = "|", text = "
format|type|readable|role|label
rbr_hex|rbr|TRUE|raw|RBR logger file (.hex)
rbr_txt|rbr|TRUE|export|Ruskin text export (.txt)
rbr_xls|rbr|TRUE|export|Ruskin Excel export (.xls)
rbr_rsk|rbr|FALSE|raw|Ruskin database (.rsk)
fg2_f2b|geoprecision|TRUE|raw|GeoPrecision logger file (.f2b), decoded by GT
fg2_binary|geoprecision|FALSE|raw|GeoPrecision logger file (.f2b) that cannot be decoded
fg2_csv|geoprecision|TRUE|export|FG2-Shell export (.csv)
fg2_log|geoprecision|FALSE|companion|FG2-Shell decode log (.log)
hobo_binary|hobo|FALSE|raw|HOBO logger file (.hobo)
hobo_csv|hobo|TRUE|export|HOBOware export (.csv)
em50_xls|em50|TRUE|raw|ECH2O Utility workbook (.xls)
campbell_toa5|campbell|TRUE|raw|Campbell TOA5 table (.dat)
campbell_array|campbell|FALSE|raw|Campbell mixed-array file (.dat)
campbell_binary|campbell|FALSE|raw|Campbell binary table (.dat)
")

TYPE_LABEL <- c(rbr = "RBR", geoprecision = "GeoPrecision", hobo = "HOBO", em50 = "EM50", campbell = "Campbell")

# ---- RBR .hex ---------------------------------------------------------------------
# Plain-text header (model, serial, clocks, sample period, calibration of each channel) followed by the memory as hex:
# a short binary header, then 3-byte counts, one per channel per sample, with 12-byte time-stamp events ("\0\0\0TIM" +
# YYMMDDhhmmss) where the logger marked a time. Thermistor channels are converted with the header's coefficients:
#   r = count / 2^24,  L = ln((1 - r) / r),  T = 1 / (c0 + c1 L + c2 L^2 + c3 L^3) - 273.15
# (checked against Ruskin's own export: identical to 7 decimals).
read_rbr_hex <- function(path, header_only = FALSE) {
  l <- sub("\\s+$", "", read_lines_any(path))
  g <- function(key) { x <- l[startsWith(l, key)][1]; if (is.na(x)) NA_character_ else trim(substring(x, nchar(key) + 1)) }
  id <- strsplit(trim(l[1]), "\\s+")[[1]]
  serial <- norm_serial(id[4]); model <- paste("RBR", id[2])
  rbr_t <- function(x) as.POSIXct(strptime(x, "%y/%m/%d %H:%M:%S", tz = "UTC"))
  host <- rbr_t(g("Host time")); lgr <- rbr_t(g("Logger time"))
  per <- g("Sample period"); ps <- as.numeric(strsplit(per %||% "", ":")[[1]]); period <- if (length(ps) == 3) sum(ps * c(3600, 60, 1)) else NA_real_
  nl <- l[grepl("^Number of channels", l)][1]
  nch <- as.integer(sub(".*channels\\s*=\\s*([0-9]+).*", "\\1", nl)); nsamp <- as.integer(sub(".*samples\\s*=\\s*([0-9]+).*", "\\1", nl))
  # calibration: "Calibration  k: c0" then three more coefficient lines, the last ending in the unit
  ci <- grep("^Calibration\\s+[0-9]+:", l)
  cal <- lapply(ci, function(i) { v <- c(sub("^Calibration\\s+[0-9]+:\\s*", "", l[i]), trim(l[i + 1:3]))
    unit <- sub("^[-0-9.eE+]+\\s*", "", v[4]); list(coef = suppressWarnings(as.numeric(sub("\\s.*$", "", v))), unit = unit) })
  stamps <- regmatches(l, regexec("^Timestamp ([0-9/]+ [0-9:]+) at sample ([0-9]+)", l)); stamps <- stamps[lengths(stamps) == 3]
  anchors <- if (length(stamps)) data.table(t = as.POSIXct(strptime(vapply(stamps, `[`, "", 2), "%Y/%m/%d %H:%M:%S", tz = "UTC")),
                                            s = as.integer(vapply(stamps, `[`, "", 3))) else data.table(t = rbr_t(g("Logging start")), s = 1L)
  anchors <- anchors[!is.na(t)][, .SD[.N], by = s][order(s)]
  drift <- if (!is.na(host) && !is.na(lgr)) { d <- as.numeric(host - lgr, units = "secs"); d - round(d / 3600) * 3600 } else NA_real_
  meta <- list(format = "rbr_hex", type = "rbr", serial = serial, model = model, title = NA_character_, offset_hours = NA_real_, interval_s = period,
               rows = nsamp, download = host, host_time = host, logger_time = lgr, drift_s = drift, channels = nch,
               units = vapply(cal, `[[`, "", "unit"))
  if (header_only) return(list(meta = meta))
  hb <- as.integer(g("Number of bytes in header")); di <- grep("^Number of bytes of data", l)[1]
  if (is.na(di) || is.na(nch) || nch < 1) stop("no data section in the .hex file")
  body <- if (di < length(l)) l[(di + 1):length(l)] else character(); body <- body[grepl("^[0-9A-Fa-f]+$", body)]
  if (!length(body)) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))   # the header only
  s <- toupper(paste(body, collapse = ""))
  if (!is.na(hb) && hb > 0) s <- substring(s, 2L * hb + 1L, nchar(s))   # substring() stops at 1e6 characters unless given an end
  n <- nchar(s) %/% 6L
  w <- substring(s, seq.int(1L, by = 6L, length.out = n), seq.int(6L, by = 6L, length.out = n))
  # events: a zero word followed by three upper-case letters, then 6 bytes of BCD time = 4 words
  ev <- which(w[-length(w)] == "000000" & grepl("^(4[1-9A-F]|5[0-9A])(4[1-9A-F]|5[0-9A])(4[1-9A-F]|5[0-9A])$", w[-1]))
  evt <- empty_events()
  if (length(ev)) {
    et <- as.POSIXct(strptime(paste0(w[ev + 2L], w[ev + 3L]), "%y%m%d%H%M%S", tz = "UTC"))
    nm <- vapply(w[ev + 1L], function(h) rawToChar(as.raw(strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L))), "")
    evt <- data.table(TIMESTAMP = et, event = paste("logger event", nm))
    w <- w[-c(ev, ev + 1L, ev + 2L, ev + 3L)]
  }
  ns <- length(w) %/% nch
  if (!is.na(nsamp) && ns > nsamp) ns <- nsamp
  if (ns < 1) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = evt))
  cnt <- matrix(strtoi(w[seq_len(ns * nch)], 16L), ncol = nch, byrow = TRUE)
  i <- seq_len(ns); a <- findInterval(i, anchors$s); a[a < 1] <- 1L
  tt <- anchors$t[a] + (i - anchors$s[a]) * period
  wide <- data.table(TIMESTAMP = tt, ord = i)
  chans <- sprintf("T%d", seq_len(nch)); vars <- rep("temp", nch); errs <- vector("list", nch)
  for (k in seq_len(nch)) {
    x <- cnt[, k]; cf <- if (k <= length(cal)) cal[[k]]$coef else NA; unit <- if (k <= length(cal)) cal[[k]]$unit else ""
    bad <- is.na(x) | x <= 0 | x >= 16777215
    if (length(cf) == 4 && !anyNA(cf) && grepl("Deg", unit, ignore.case = TRUE)) {
      r <- x / 2^24; L <- log((1 - r) / r); v <- 1 / (cf[1] + cf[2] * L + cf[3] * L^2 + cf[4] * L^3) - 273.15
    } else { v <- rep(NA_real_, ns); vars[k] <- "raw"; bad[] <- TRUE }
    v[bad] <- NA_real_; set(wide, j = chans[k], value = v)
    errs[[k]] <- ifelse(bad, if (vars[k] == "raw") "channel is not a thermistor (unit not supported)" else "raw count out of range (open or shorted thermistor)", NA_character_)
  }
  list(meta = meta, data = melt_channels(wide, chans, vars, errs), events = evt)
}

# ---- RBR Ruskin text export (ODV spreadsheet, times in UTC) ---------------------------------
# The header names one more column ("Depth [m]") than the rows hold values, so the columns are read by position.
read_rbr_txt <- function(path, header_only = FALSE) {
  l <- read_lines_any(path); com <- l[startsWith(l, "//")]; l <- l[!startsWith(l, "//") & nzchar(l)]
  kv <- function(k) { x <- com[grepl(paste0("^//\\s*", k, "="), com)][1]; if (is.na(x)) NA_character_ else trim(sub("^[^=]*=", "", x)) }
  hdr <- trim(strsplit(l[1], "\t", fixed = TRUE)[[1]]); hdr <- hdr[nzchar(hdr)]
  meta <- list(format = "rbr_txt", type = "rbr", serial = norm_serial(kv("Serial")), model = paste("RBR", kv("Model") %||% ""), title = NA_character_,
               offset_hours = 0, interval_s = NA_real_, rows = length(l) - 1L,
               download = as.POSIXct(strptime(sub("^//<CreateTime>(.*)</CreateTime>.*", "\\1", com[grepl("CreateTime", com)][1]), "%Y-%m-%dT%H:%M:%OS", tz = "UTC")))
  if (header_only) return(list(meta = meta))
  if (length(l) < 2) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))
  p <- tstrsplit(l[-1], "\t", fixed = TRUE)
  p <- p[vapply(p, function(v) any(nzchar(trim(v)) & !is.na(v)), logical(1))]
  ti <- match("yyyy-mm-ddThh:mm:ss.sss", hdr); if (is.na(ti)) ti <- 4L
  first_val <- ti + 4L                                                    # time, longitude, latitude, bottom depth, then the values
  vals <- if (length(p) >= first_val) p[first_val:length(p)] else list()
  names_after <- hdr[(ti + 4L):length(hdr)]
  if (length(names_after) == length(vals) + 1L && grepl("^Depth", names_after[1])) names_after <- names_after[-1]
  nch <- length(vals)
  wide <- data.table(TIMESTAMP = as.POSIXct(strptime(p[[ti]], "%Y-%m-%dT%H:%M:%OS", tz = "UTC")), ord = seq_along(p[[ti]]))
  chans <- sprintf("T%d", seq_len(nch)); vars <- ifelse(grepl("Temp", names_after[seq_len(nch)], ignore.case = TRUE) | is.na(names_after[seq_len(nch)]), "temp", "raw")
  for (k in seq_len(nch)) set(wide, j = chans[k], value = num_cells(vals[[k]]))
  meta$interval_s <- step_of(head(wide$TIMESTAMP, 500)); meta$channels <- nch
  list(meta = meta, data = mark_cut(melt_channels(wide, chans, vars), path), events = empty_events())
}

# ---- RBR Ruskin Excel export (times in the logger's clock) -----------------------------------
read_rbr_xls <- function(path, header_only = FALSE) {
  d <- readxl::read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal", col_types = "text", n_max = if (header_only) 12 else Inf)
  d <- as.data.table(d); cell <- function(r, c) if (nrow(d) >= r && ncol(d) >= c) trim(d[[c]][r]) else NA_character_
  find <- function(lbl) { for (r in seq_len(min(nrow(d), 12))) for (c in seq_len(ncol(d))) if (isTRUE(grepl(lbl, d[[c]][r], fixed = TRUE))) return(cell(r, c + 1)); NA_character_ }
  hr <- which(trim(d[[1]]) == "Timestamp")[1]
  meta <- list(format = "rbr_xls", type = "rbr", serial = norm_serial(find("Serial Number")), model = paste("RBR", find("Model") %||% ""), title = NA_character_,
               offset_hours = NA_real_, interval_s = suppressWarnings(as.numeric(find("sampling period"))), rows = if (is.na(hr)) 0L else nrow(d) - hr, download = NA)
  if (header_only || is.na(hr)) return(list(meta = meta, data = empty_data(), events = empty_events()))
  body <- d[(hr + 1):nrow(d)]; body <- body[!is.na(body[[1]]) & nzchar(body[[1]])]
  tt <- parse_dt(body[[1]], c("%d/%m/%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS"))
  if (all(is.na(tt))) tt <- excel_time(body[[1]])
  nch <- ncol(body) - 1L; chans <- sprintf("T%d", seq_len(nch))
  wide <- data.table(TIMESTAMP = tt, ord = seq_along(tt))
  for (k in seq_len(nch)) set(wide, j = chans[k], value = num_cells(body[[k + 1L]]))
  hdr <- unlist(d[hr, -1]); vars <- ifelse(grepl("Temp", hdr) | is.na(hdr), "temp", "raw")
  meta$channels <- nch; meta$rows <- nrow(wide)
  list(meta = meta, data = melt_channels(wide, chans, vars), events = empty_events())
}

# ---- GeoPrecision FG2-Shell export -------------------------------------------------------
# A log, not a table: <TAG> lines (<NEW PAR>, <PERIOD:>, <READOUT>, <FIRMWARE RESET>, <MEMORY CLEAR> ...) sit between the
# data rows and the NO,TIME,... header repeats. A dead node reads (NoReply). HK-BAT (battery) is written every few rows.
read_fg2_csv <- function(path, header_only = FALSE) {
  l <- sub(",+$", "", read_lines_any(path)); l <- l[nzchar(l)]
  tag <- startsWith(l, "<")
  lg <- sub("^<LOGGER: \\$?([^>]*)>.*", "\\1", l[grepl("^<LOGGER:", l)][1])
  now <- sub(".*NOW\\(UTC\\):([0-9.]+ [0-9:]+)>.*", "\\1", l[grepl("NOW\\(UTC\\)", l)][1])
  stem <- regmatches(basename(path), regexpr("[0-9]{14}", basename(path)))
  dl <- if (length(stem)) as.POSIXct(strptime(stem, "%Y%m%d%H%M%S", tz = "UTC")) else NA
  meta <- list(format = "fg2_csv", type = "geoprecision", serial = norm_serial(lg %||% sub("_.*$", "", basename(path))), model = "GeoPrecision FG2",
               title = NA_character_, offset_hours = NA_real_, interval_s = NA_real_, rows = sum(grepl("^[0-9]+,", l)), download = dl,
               export_utc = as.POSIXct(strptime(now, "%d.%m.%Y %H:%M:%S", tz = "UTC")))
  if (header_only) return(list(meta = meta))
  hi <- which(startsWith(l, "NO,TIME")); di <- which(grepl("^[0-9]+,", l))
  if (!length(hi) || !length(di)) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))
  # each row is read by the header above it (a new parameter set can change the channels); a header's units are left out of
  # its names, as the first header can say "(unk.)" where a later one says "(oC)"
  hn <- vapply(strsplit(l[hi], ",", fixed = TRUE), function(h) paste(sub("\\(.*$", "", h), collapse = ","), "")
  sg <- hn[pmax(1L, findInterval(di, hi))]
  w <- rbindlist(lapply(unique(sg), function(s) { k <- which(sg == s); h <- strsplit(s, ",", fixed = TRUE)[[1]]
    p <- tstrsplit(l[di[k]], ",", fixed = TRUE, fill = NA); p <- c(p, rep(list(rep(NA_character_, length(k))), max(0L, length(h) - length(p))))[seq_along(h)]
    setnames(setDT(p), h)[, ord := k] }), fill = TRUE)[order(ord)]
  wide <- data.table(TIMESTAMP = as.POSIXct(strptime(w$TIME, "%d.%m.%Y %H:%M:%S", tz = "UTC")), ord = w$ord)
  vc <- setdiff(names(w), c("NO", "TIME", "ord")); chans <- vc; chans[grepl("^HK-BAT", chans)] <- "HK-BAT"
  vars <- ifelse(chans == "HK-BAT", "battery", ifelse(grepl("^#", chans), "temp", "raw"))
  chans <- sub("^#", "T", chans)
  errs <- vector("list", length(vc))
  for (k in seq_along(vc)) { v <- w[[vc[k]]]; v[is.na(v)] <- ""
    e <- ifelse(grepl("^\\(", v), v, NA_character_)                              # (NoReply), (Error) ...
    set(wide, j = chans[k], value = num_cells(ifelse(is.na(e), v, NA)))
    errs[[k]] <- ifelse(is.na(e), NA_character_, paste("logger reported", e)) }
  long <- mark_cut(melt_channels(wide, chans, vars, errs), path)
  long <- long[!(variable == "battery" & is.na(value) & is.na(err))]          # battery is written only every few rows
  # events: each tag belongs to the next data row
  nxt <- findInterval(which(tag), di) + 1L; nxt[nxt > length(di)] <- length(di)
  et <- l[tag]; keep <- !grepl("^<(TIME|HK-BATUSED|FG2|FILE|FILESIZE|LOGGER|INFO):", et)
  evt <- data.table(TIMESTAMP = wide$TIMESTAMP[nxt[keep]], event = gsub("^<|>$", "", et[keep]))
  per <- suppressWarnings(as.numeric(sub("^<PERIOD: ([0-9]+) sec>.*", "\\1", l[grepl("^<PERIOD:", l)])))
  meta$interval_s <- if (length(per)) per[length(per)] else step_of(wide$TIMESTAMP)
  meta$channels <- sum(vars == "temp")
  list(meta = meta, data = long, events = evt)
}

# ---- HOBOware CSV export -----------------------------------------------------------------
# Channels of HOBO water-level (U20) and conductivity (U24) loggers: these belong to WLR, and GT leaves them out.
HOBO_WATER_CHANNELS <- "^(Abs Pres|Pres|Diff Pres|Water Level|Sensor Depth|Low Range|Full Range|High Range|Range|Specific Conductance|Spec Cond)|(\u00b5|\u03bc|u)S/cm"
HOBO_WATER_MODELS   <- "Water Level|Conductivity|Pressure|U20|U24|U26"
read_hobo_csv <- function(path, header_only = FALSE) {
  l <- read_lines_any(path); l <- l[nzchar(trim(l))]
  h <- hobo_csv_head(l); hi <- h$hi; hdr <- h$header; dcol <- h$dcol; offset <- h$offset_hours; title <- h$title   # core logger_files.R
  serial <- if (!is.na(h$serial)) norm_serial(h$serial) else NA_character_
  if (is.na(serial)) { s2 <- regmatches(basename(path), regexpr("^[0-9]{6,10}", basename(path))); if (length(s2)) serial <- s2 }
  is_t <- grepl("^Temp", hdr); is_rh <- grepl("^RH", hdr)
  water <- any(grepl(HOBO_WATER_CHANNELS, hdr[-dcol]))   # a water-level or conductivity logger: WLR reads it, not GT
  is_ev <- !is_t & !is_rh & seq_along(hdr) > dcol & !grepl("^(Abs Pres|Water Level|Light|Intensity|Voltage|Volt)", hdr)
  meta <- list(format = "hobo_csv", type = "hobo", serial = serial, model = "HOBO", title = title, offset_hours = offset, interval_s = NA_real_,
               rows = length(l) - hi, download = NA, water = water)
  if (header_only) return(list(meta = meta))
  if (hi >= length(l)) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))   # the header only
  d <- fread(text = l[(hi + 1):length(l)], header = FALSE, sep = h$sep, colClasses = "character", fill = TRUE, na.strings = NULL)
  if (!nrow(d)) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))
  tt <- parse_dt(d[[dcol]])
  wide <- data.table(TIMESTAMP = tt, ord = seq_len(nrow(d)))
  ti <- which(is_t); lbl <- sub(".*LBL: *([^)]*)\\).*", "\\1", hdr[ti]); lbl[lbl == hdr[ti]] <- paste0("T", seq_along(ti))[lbl == hdr[ti]]
  fahr <- grepl("\u00b0F|\\bF\\b", hdr[ti])
  for (k in seq_along(ti)) { v <- num_cells(d[[ti[k]]]); if (fahr[k]) v <- convert_unit(v, "degF", "degC"); set(wide, j = lbl[k], value = v) }
  evt <- rbindlist(lapply(which(is_ev), function(j) { k <- which(nzchar(trim(d[[j]])))
    if (!length(k)) return(NULL); data.table(TIMESTAMP = tt[k], event = sub(" *\\(.*$", "", hdr[j])) }))
  if (!nrow(evt)) evt <- empty_events()
  meta$interval_s <- step_of(head(tt, 500)); meta$channels <- length(ti)
  list(meta = meta, data = mark_cut(melt_channels(wide, lbl, rep("temp", length(ti))), path), events = evt)
}

# The .hobo binary is Onset's closed format: only its plain-text identity is read (model, serial, title, zone).
read_hobo_binary_header <- function(path) { id <- hobo_bin_identity(path)                      # core logger_files.R
  list(meta = list(format = "hobo_binary", type = "hobo", serial = norm_serial(id$serial), model = id$model, title = id$title, offset_hours = id$offset_hours,
                   interval_s = NA_real_, rows = NA_integer_, download = NA, water = grepl(HOBO_WATER_MODELS, id$model)))
}

# ---- EM50 workbook from ECH2O Utility -------------------------------------------------------
# Sheet 1: processed values; "Unprocessed Data": raw counts in the same layout. Three header rows (port, sensor, unit).
# Raw counts of 4094/4095 (or 0 on a port with a sensor) are the logger's error codes; the processed sheet then shows
# nonsense (VWC 1.011), so those values are set missing with the reason.
read_em50_xls <- function(path, header_only = FALSE) {
  sh <- readxl::excel_sheets(path)
  d <- as.data.table(readxl::read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal", col_types = "text", n_max = if (header_only) 4 else Inf))
  title <- trim(d[[1]][1]); port <- trim(unlist(d[1, -1])); sensor <- trim(unlist(d[2, -1])); unit <- trim(unlist(d[3, -1]))
  sn <- regmatches(basename(path), regexpr("EM[0-9]{3,6}", basename(path), ignore.case = TRUE))
  serial <- if (length(sn)) norm_serial(sub("^EM", "", sn, ignore.case = TRUE)) else norm_serial(title)
  meta <- list(format = "em50_xls", type = "em50", serial = serial, model = paste("EM50", paste(unique(na.omit(sensor)), collapse = ", ")), title = title,
               offset_hours = NA_real_, interval_s = NA_real_, rows = max(0L, nrow(d) - 3L), download = NA)
  if (header_only) return(list(meta = meta))
  body <- d[-(1:3)]; body <- body[!is.na(body[[1]]) & nzchar(body[[1]])]
  tt <- excel_time(suppressWarnings(as.numeric(body[[1]])))
  var <- ifelse(grepl("VWC", unit), "vwc", ifelse(grepl("Temp", unit), "temp", ifelse(grepl("EC", unit), "ec", NA_character_)))
  keep <- which(!is.na(var) & !is.na(port))
  raw <- NULL; ri <- grep("^Unprocessed", sh)[1]
  if (!is.na(ri)) { raw <- as.data.table(readxl::read_excel(path, sheet = ri, col_names = FALSE, .name_repair = "minimal", col_types = "text"))[-(1:3)]
    raw <- raw[!is.na(raw[[1]]) & nzchar(raw[[1]])]; if (nrow(raw) != nrow(body)) raw <- NULL }
  wide <- data.table(TIMESTAMP = tt, ord = seq_along(tt))
  chans <- paste0("P", sub("^Port\\s*", "", port[keep]), "_", var[keep]); errs <- vector("list", length(keep))
  for (k in seq_along(keep)) { j <- keep[k] + 1L; v <- num_cells(body[[j]]); e <- rep(NA_character_, length(v))
    if (!is.null(raw)) { r <- suppressWarnings(as.numeric(raw[[j]])); e[!is.na(r) & r >= 4094] <- paste("raw count", r[!is.na(r) & r >= 4094], "(sensor error code)") }
    if (var[keep[k]] == "vwc") e[is.na(e) & !is.na(v) & v > 1] <- "VWC above 1 (sensor error)"
    v[!is.na(e)] <- NA_real_; set(wide, j = chans[k], value = v); errs[[k]] <- e }
  meta$interval_s <- step_of(head(tt, 500)); meta$channels <- sum(var[keep] == "temp")
  list(meta = meta, data = melt_channels(wide, chans, var[keep], errs), events = empty_events())
}

# ---- Campbell Scientific TOA5 table -----------------------------------------------------------
# Only the columns that match the patterns on the Set-up page are kept (thermistors by default; soil moisture and
# conductivity when asked). The serial is the logger's (or the station name when the header says No_SN) plus the table.
CAMPBELL_SENTINELS <- c(-7999, 7999, -6999, 6999, -9999)
read_campbell_toa5 <- function(path, header_only = FALSE, patterns = NULL) {
  th <- toa5_head(path); if (th$status != "ok") stop("not a readable TOA5 table (", th$status, ")")   # core logger_files.R
  env <- trim(th$env); nm <- trim(th$columns); un <- trim(th$units)
  sn <- if (!grepl("No_SN|^$", env[4])) env[4] else env[2]
  tbl <- if (nzchar(env[8])) env[8] else "Table"
  meta <- list(format = "campbell_toa5", type = "campbell", serial = norm_serial(paste0(sn, "_", tbl)), model = paste("Campbell", env[3]),
               title = env[2], table = tbl, offset_hours = NA_real_, interval_s = NA_real_, rows = NA_integer_, download = NA)
  if (header_only) return(list(meta = meta))
  if (is.null(patterns) || !nrow(patterns)) patterns <- default_campbell_patterns()
  var <- rep(NA_character_, length(nm))
  for (i in seq_len(nrow(patterns))) { hit <- is.na(var) & grepl(patterns$pattern[i], nm, ignore.case = TRUE, perl = TRUE)
    if (nzchar(patterns$exclude[i] %||% "")) hit <- hit & !grepl(patterns$exclude[i], nm, ignore.case = TRUE, perl = TRUE)
    var[hit] <- patterns$variable[i] }
  keep <- which(!is.na(var) & var != "skip")
  sk <- toa5_skip(path); hl <- suppressWarnings(readLines(path, n = sk + 40L, warn = FALSE, skipNul = TRUE))
  if (!any(nzchar(trimws(hl[-seq_len(sk)])))) return(list(meta = modifyList(meta, list(rows = 0L)), data = empty_data(), events = empty_events()))   # the header only
  d <- fread(path, skip = sk, header = FALSE, sep = th$sep, colClasses = "character", na.strings = NULL, fill = TRUE, blank.lines.skip = TRUE)
  meta$rows <- nrow(d)
  if (!nrow(d) || !length(keep)) return(list(meta = meta, data = empty_data(), events = empty_events()))
  tt <- parse_dt(d[[1]])
  wide <- data.table(TIMESTAMP = tt, ord = seq_len(nrow(d))); errs <- vector("list", length(keep))
  for (k in seq_along(keep)) { v <- trim(d[[keep[k]]]); x <- num_cells(v)
    e <- ifelse(toupper(v) %in% c("NAN", "INF", "-INF"), paste("logger wrote", v), ifelse(!is.na(x) & x %in% CAMPBELL_SENTINELS, paste("Campbell out-of-range code", x), NA_character_))
    x[!is.na(e)] <- NA_real_
    if (grepl("F", un[keep[k]] %||% "") && grepl("Deg", un[keep[k]] %||% "") && var[keep[k]] == "temp") x <- convert_unit(x, "degF", "degC")
    set(wide, j = nm[keep[k]], value = x); errs[[k]] <- e }
  meta$interval_s <- step_of(head(tt, 500)); meta$channels <- sum(var[keep] == "temp")
  list(meta = meta, data = mark_cut(melt_channels(wide, nm[keep], var[keep], errs), path), events = empty_events())
}

default_campbell_patterns <- function() fread(sep = "~", colClasses = "character", na.strings = NULL, text = "
pattern~exclude~variable~notes
^T10[79]_C(_[0-9]+)?_Avg$~~temp~107/109 thermistors (averages)
^T10[79]_[0-9]+_C_Avg$~~temp~107/109 thermistors, other naming
^(Therm|Tsoil|SoilT|GT|TS|Ground_?T|T_?soil)[_0-9A-Za-z]*(_Avg)?$~(Max|Min|Std)$~temp~other ground temperature names
^T_?[0-9]+_?cm(_Avg)?$~(Max|Min)$~temp~temperature named by depth (T10cm, T_10_cm)
^(VW|VWC|WaterContent|SWC)(_[0-9]+)?(_Avg)?$~(Max|Min)$~vwc~soil water content
^(EC|BulkEC|Soil_?EC)(_[0-9]+)?(_Avg)?$~(Max|Min)$~ec~soil conductivity
")

# Dispatch on format.
read_logger_file <- function(path, format, header_only = FALSE, campbell_patterns = NULL) {
  unread_take()                                                           # cells that are not numbers, counted per file (core convert.R)
  unread_mark(switch(format,
    rbr_hex = read_rbr_hex(path, header_only), rbr_txt = read_rbr_txt(path, header_only), rbr_xls = read_rbr_xls(path, header_only),
    fg2_csv = read_fg2_csv(path, header_only), fg2_f2b = read_f2b(path, header_only), hobo_csv = read_hobo_csv(path, header_only), hobo_binary = read_hobo_binary_header(path),
    em50_xls = read_em50_xls(path, header_only), campbell_toa5 = read_campbell_toa5(path, header_only, campbell_patterns),
    stop("no reader for ", format)))
}

# Identity of a file whose contents cannot be read (serial from the file name).
name_identity <- function(path, format) {
  b <- basename(path); type <- FORMAT_INFO$type[match(format, FORMAT_INFO$format)]
  ser <- switch(type %||% "",
    geoprecision = sub("_.*$", "", b),
    rbr = regmatches(b, regexpr("^[0-9]{5,6}", b)),
    hobo = regmatches(b, regexpr("^[0-9]{6,10}", b)),
    campbell = NA_character_, NA_character_)
  list(format = format, type = type %||% NA_character_, serial = norm_serial(if (length(ser)) ser else NA_character_), model = NA_character_, title = NA_character_,
       offset_hours = NA_real_, interval_s = NA_real_, rows = NA_integer_, download = NA)
}
