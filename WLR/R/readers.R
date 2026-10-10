# R/readers.R  --  Reading logger files: HOBOware CSV exports, Solinst .xle and Solinst CSV exports.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# Every reader returns the same shape:
#   meta   one-row list: format, serial, model, title, location, offset_hours (as stated; NA when the file states none),
#          units (named: pres, temp, ec), interval_s, first, last, rows
#   data   data.table: TIMESTAMP (clock time in the base zone, UTC-7) + any of pres, temp, ec_low, ec_full, ec (native units)
#   events data.table: TIMESTAMP, event   (HOBO only: Coupler Detached, Host Connected, Bad Battery, ...)
# Raw files are only ever read.

# WLR keeps its own reader (NUL bytes kept as they are, unlike the shared read_lines_any() in Ovid/util.R).
wlr_read_lines <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  if (length(raw) >= 3 && all(raw[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8", sub = "?")
  strsplit(gsub("\r", "", txt, fixed = TRUE), "\n", fixed = TRUE)[[1]]
}

split_cells <- function(l, sep) trim(scan(text = l, what = character(), sep = sep, quote = "\"", quiet = TRUE, na.strings = NULL))

empty_events <- function() data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC"), event = character())

# ---- what kind of file is this? -------------------------------------------------
sniff_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "hobo") return("hobo_binary")
  if (ext == "xle") return("solinst_xle")
  if (ext %in% c("csv", "txt")) {
    l <- tryCatch(wlr_read_lines(path)[1:3], error = function(e) character())
    l <- l[!is.na(l)]
    if (!length(l)) return("empty")
    if (grepl("Plot Title", l[1], fixed = TRUE)) return("hobo_csv")
    if (grepl("^Serial_number", l[1])) return("solinst_csv")
    if (length(l) > 1 && grepl("Date Time", l[2], fixed = TRUE)) return("hobo_csv")
  }
  "unrecognised"
}

# ---- HOBOware CSV ---------------------------------------------------------------
hobo_role <- function(name) {
  n <- tolower(name)
  if (grepl("^abs pres", n)) "pres" else if (grepl("^temp", n)) "temp" else if (grepl("^low range", n)) "ec_low"
  else if (grepl("^full range", n)) "ec_full" else if (grepl("^(spec|specific) cond", n)) "ec_spc"
  else if (grepl("^(water level|sensor depth|diff press|bar(ometric)? pres)", n)) paste0("derived_", gsub("[^a-z]+", "_", n)) else NA_character_
}

read_hobo_csv <- function(path, header_only = FALSE) {
  L <- wlr_read_lines(path)
  if (length(L) < 2) stop("file has no header")
  h <- hobo_csv_head(L); sep <- h$sep; hdr <- h$header; dcol <- h$dcol; offset <- h$offset_hours   # core logger_files.R
  title <- h$title %|% ""
  serial <- if (!is.na(h$serial)) norm_serial(h$serial) else norm_serial(sub("[^0-9].*$", "", trim(title)))
  if (is.na(dcol)) stop("no Date Time column")
  base_name <- trim(sub("\\(.*$", "", hdr))                        # "Abs Pres, kPa"
  name <- trim(sub(",.*$", "", base_name)); unit <- ifelse(grepl(",", base_name), trim(sub("^[^,]*,", "", base_name)), "")
  unit <- gsub("\ufffd|\\?C$", "\u00b0C", unit); unit[grepl("^.?C$", unit) & nchar(unit) <= 2] <- "\u00b0C"
  role <- vapply(name, hobo_role, character(1), USE.NAMES = FALSE); role[dcol] <- NA
  role[seq_along(role) == 1 & name == "#"] <- NA
  is_event <- is.na(role) & seq_along(hdr) > dcol & !nzchar(unit)
  units <- c(pres = unit[match("pres", role)], temp = unit[match("temp", role)], ec = unit[match("ec_low", role)])
  model <- NA_character_
  if (header_only) return(list(meta = list(format = "hobo_csv", serial = serial, model = model, title = title, location = title,
                                           offset_hours = offset, units = units), header = hdr))
  body <- L[-seq_len(h$hi)]; body <- body[nzchar(trim(body)) & !grepl("Date Time", body, fixed = TRUE)]   # no blank lines (fread stops at one) or repeated headings
  d <- fread(text = body, sep = sep, header = FALSE, colClasses = "character", fill = TRUE, na.strings = c("", "NA"), quote = "\"")
  if (!nrow(d)) stop("no records")
  ts <- parse_dt(d[[dcol]])
  bad <- is.na(ts)
  out <- data.table(TIMESTAMP = ts)
  for (j in which(!is.na(role) & !startsWith(role, "derived_"))) set(out, j = role[j], value = num_cells(gsub(",", ".", d[[j]], fixed = TRUE)))
  ev <- rbindlist(lapply(which(is_event), function(j) { k <- which(!is.na(d[[j]]) & nzchar(d[[j]]))
    if (length(k)) data.table(TIMESTAMP = ts[k], event = name[j]) else NULL }))
  if (!nrow(ev)) ev <- empty_events()
  vals <- intersect(c("pres", "temp", "ec_low", "ec_full", "ec_spc"), names(out))
  has_val <- if (length(vals)) rowSums(!is.na(out[, ..vals])) > 0 else rep(FALSE, nrow(out))
  out <- out[!bad & has_val]                                          # event-only lines carry no reading
  out[, TIMESTAMP := to_base(TIMESTAMP, offset)]; ev[, TIMESTAMP := to_base(TIMESTAMP, offset)]
  list(meta = list(format = "hobo_csv", serial = serial, model = model, title = title, location = title, offset_hours = offset,
                   units = units, interval_s = step_of(out$TIMESTAMP), first = suppressWarnings(min(out$TIMESTAMP)),
                   last = suppressWarnings(max(out$TIMESTAMP)), rows = nrow(out), unparsed_rows = sum(bad & has_val)),
       data = out, events = ev)
}

# The .hobo binary is Onset's closed format: only its plain-text identity is read (model, serial, title, zone),
# so the file can be paired with its HOBOware CSV export.
read_hobo_binary_header <- function(path) { id <- hobo_bin_identity(path)                      # core logger_files.R
  list(format = "hobo_binary", serial = norm_serial(id$serial), model = id$model, title = id$title, location = id$title,
       offset_hours = id$offset_hours, size = id$size)
}

# A U20 or U20L .hobo with no CSV export: its records read directly (core hobo.R), in kPa and degrees C.
read_hobo_decoded <- function(path, header_only = FALSE) { meta <- read_hobo_binary_header(path); meta$format <- "hobo_decoded"
  meta$units <- c(pres = "kPa", temp = "\u00b0C", ec = NA_character_); if (header_only) return(list(meta = meta))
  d <- hobo_decode(path); if (is.null(d) || !nrow(d)) stop("no records could be read from this .hobo file")
  off <- meta$offset_hours %||% NA_real_
  out <- data.table(TIMESTAMP = to_base(d$TIMESTAMP, off), pres = d$pres_kpa, temp = d$temp_c)
  meta <- c(meta, list(interval_s = attr(d, "interval_s"), first = min(out$TIMESTAMP), last = max(out$TIMESTAMP), rows = nrow(out)))
  list(meta = meta, data = out, events = empty_events()) }

# ---- Solinst .xle (XML) ---------------------------------------------------------
xle_role <- function(id) { id <- toupper(trim(id))
  if (id %in% c("LEVEL", "PRESSURE", "BAROMETRIC")) "pres" else if (grepl("TEMP", id)) "temp" else if (grepl("COND", id)) "ec" else NA_character_ }

# Older Levelogger software writes Latin-1 (a degree sign in the units) without saying so in the XML declaration;
# libxml2 then refuses the file, so it is read again as Latin-1.
read_xle_xml <- function(path) {
  opts <- c("RECOVER", "NOBLANKS", "HUGE")
  tryCatch(xml2::read_xml(path, options = opts), error = function(e) {
    b <- readBin(path, "raw", file.size(path)); b <- b[b != as.raw(0)]
    txt <- rawToChar(b); if (validUTF8(txt)) stop(e)
    txt <- iconv(txt, "latin1", "UTF-8"); txt <- sub("encoding=\"[^\"]*\"", "", txt)
    xml2::read_xml(txt, options = opts) })
}

read_solinst_xle <- function(path, header_only = FALSE) {
  doc <- read_xle_xml(path)
  g <- function(xp) { n <- xml2::xml_find_first(doc, xp); if (inherits(n, "xml_missing")) NA_character_ else trim(xml2::xml_text(n)) }
  serial <- norm_serial(g("//Instrument_info/Serial_number"))
  model <- paste(na.omit(c(g("//Instrument_info/Instrument_type"), g("//Instrument_info/Model_number"))), collapse = " ")
  loc <- g("//Instrument_info_data_header/Location")
  chans <- xml2::xml_find_all(doc, "//*[starts-with(name(), 'Ch') and contains(name(), '_data_header')]")
  ch <- data.table(node = xml2::xml_name(chans),
                   id = vapply(chans, function(n) trim(xml2::xml_text(xml2::xml_find_first(n, "Identification"))), ""),
                   unit = vapply(chans, function(n) trim(xml2::xml_text(xml2::xml_find_first(n, "Unit"))), ""),
                   offset = vapply(chans, function(n) { o <- xml2::xml_find_first(n, "Parameters/Offset")
                     if (inherits(o, "xml_missing")) NA_real_ else suppressWarnings(as.numeric(xml2::xml_attr(o, "Val"))) }, 0),
                   altitude = vapply(chans, function(n) { a <- xml2::xml_find_first(n, "Parameters/Altitude")
                     if (inherits(a, "xml_missing")) return(NA_real_)
                     v <- suppressWarnings(as.numeric(xml2::xml_attr(a, "Val"))); u <- tolower(trim(xml2::xml_attr(a, "Unit") %||% "m"))
                     if (grepl("^f", u)) v <- v * 0.3048
                     if (is.na(v) || v < -300 || v > 5000) NA_real_ else v }, 0))
  ch[, unit := gsub("\u00c2", "", unit, fixed = TRUE)][, k := tolower(sub("_data_header$", "", node))][, role := vapply(id, xle_role, "")]
  # a logger that lost its programming writes channels with no name: tell them apart by the unit
  ch[!nzchar(id) & is.na(role), role := ifelse(tolower(unit) %in% c("m", "cm", "mm", "ft", "in", "kpa", "psi", "mbar", "bar"), "pres",
                                         ifelse(grepl("c$|f$", tolower(unit)), "temp", ifelse(grepl("s/cm", tolower(unit)), "ec", NA_character_)))]
  rate <- suppressWarnings(as.numeric(g("//Instrument_info_data_header/Sample_rate")))
  units <- c(pres = ch$unit[match("pres", ch$role)], temp = ch$unit[match("temp", ch$role)], ec = ch$unit[match("ec", ch$role)])
  meta <- list(format = "solinst_xle", serial = serial, model = model, title = loc, location = loc, offset_hours = NA_real_,
               units = units, channel_offset = ch$offset[match("pres", ch$role)], num_log = suppressWarnings(as.integer(g("//Num_log"))),
               altitude_m = ch$altitude[match("pres", ch$role)])   # the altitude programmed into a Levelogger Gold / Junior: already added to its readings
  if (header_only) return(list(meta = meta))
  logs <- xml2::xml_find_all(doc, "//Data/Log")
  if (!length(logs)) { meta$rows <- 0L; return(list(meta = meta, data = data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC")), events = empty_events())) }
  col <- function(tag) xml2::xml_text(xml2::xml_find_first(logs, tag))
  ms <- suppressWarnings(as.numeric(col("ms"))); ms[is.na(ms)] <- 0
  ts <- parse_dt(paste(col("Date"), col("Time")), "%Y/%m/%d %H:%M:%S") + ms / 1000
  out <- data.table(TIMESTAMP = ts)
  for (i in which(!is.na(ch$role))) set(out, j = ch$role[i], value = num_cells(col(ch$k[i])))
  out <- out[!is.na(TIMESTAMP)]
  meta <- c(meta, list(interval_s = if (!is.na(rate) && rate > 0) rate / 100 else step_of(out$TIMESTAMP),
                       first = suppressWarnings(min(out$TIMESTAMP)), last = suppressWarnings(max(out$TIMESTAMP)), rows = nrow(out)))
  list(meta = meta, data = out, events = empty_events())
}

# ---- Solinst CSV export -----------------------------------------------------------
read_solinst_csv <- function(path, header_only = FALSE) {
  L <- wlr_read_lines(path)
  h <- grep("^\\s*Date\\s*,\\s*Time", L)[1]
  if (is.na(h)) stop("no Date,Time header line")
  head <- trim(L[seq_len(h - 1)]); head <- head[nzchar(head)]               # blank lines do not separate a key from its value
  after <- function(key) { i <- grep(paste0("^", key), head)[1]; if (is.na(i) || i >= length(head)) NA_character_ else head[i + 1] }
  serial <- norm_serial(after("Serial_number")); loc <- after("Location")
  cols <- split_cells(L[h], ",")
  # channel blocks: a name line (LEVEL / TEMPERATURE / CONDUCTIVITY) followed by "UNIT: m"
  ui <- grep("^UNIT:", head)
  ch <- data.table(id = head[ui - 1], unit = gsub("\ufffd|\\?", "\u00b0", trim(sub("^UNIT:", "", head[ui]))))
  ch[unit %in% c("\u00b0C", "\u00b0", "C"), unit := "\u00b0C"][, role := vapply(id, xle_role, "")]
  units <- c(pres = ch$unit[match("pres", ch$role)], temp = ch$unit[match("temp", ch$role)], ec = ch$unit[match("ec", ch$role)])
  meta <- list(format = "solinst_csv", serial = serial, model = "Solinst", title = loc, location = loc, offset_hours = NA_real_, units = units)
  if (header_only) return(list(meta = meta))
  body <- L[-seq_len(h)]; body <- body[grepl("^\\s*[0-9]", body)]        # records only: blank lines and a repeated header block (two exports joined) left out
  if (!length(body)) stop("no records")
  d <- fread(text = body, sep = ",", header = FALSE, colClasses = "character", fill = TRUE, na.strings = c("", "NA"))
  ts <- parse_dt(paste(d[[1]], d[[2]]), c("%m/%d/%Y %I:%M:%S %p", "%Y/%m/%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S"))
  ms <- if ("ms" %in% cols) suppressWarnings(as.numeric(d[[match("ms", cols)]])) else 0
  out <- data.table(TIMESTAMP = ts + ifelse(is.na(ms), 0, ms) / 1000)
  for (j in seq_along(cols)) { r <- xle_role(cols[j]); if (!is.na(r)) set(out, j = r, value = num_cells(d[[j]])) }
  out <- out[!is.na(TIMESTAMP)]
  meta <- c(meta, list(interval_s = step_of(out$TIMESTAMP), first = suppressWarnings(min(out$TIMESTAMP)),
                       last = suppressWarnings(max(out$TIMESTAMP)), rows = nrow(out)))
  list(meta = meta, data = out, events = empty_events())
}

read_logger_file <- function(path, format = sniff_format(path), header_only = FALSE) { unread_take(); unread_mark(switch(format,   # cells that are not numbers, counted per file (core convert.R)
  hobo_csv = read_hobo_csv(path, header_only), solinst_xle = read_solinst_xle(path, header_only),
  solinst_csv = read_solinst_csv(path, header_only), hobo_binary = list(meta = read_hobo_binary_header(path)),
  hobo_decoded = read_hobo_decoded(path, header_only),
  stop("not a logger file this app reads"))) }
