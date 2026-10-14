# R/importers.R  --  Telling what each file is, and reading instrument files into the common Ovid GS model.
# Part of Ovid Core // GS. Sourced by gs_pipeline.R; not meant to be run on its own.
#
# Each format is one entry in FORMATS: its type (instrument family), role (data, derived, companion, result, notes) and a
# sniff() on the file's first lines. Data and derived formats have a read() returning
#   list(meta = list(...), meas = <common columns, MEAS_COLS>, extra = <instrument fields, keyed by meas_id>)
# Companion readers return their own tables, which catalogue.R joins to the survey. Readers never write anything.

MEAS_COLS <- c("meas_id", "rec_no", "datetime", "a", "b", "m", "n", "ax", "ay", "az", "bx", "by", "bz", "mx", "my", "mz", "nx", "ny", "nz",
               "voltage_v", "current_a", "resistance_ohm", "rhoa_ohmm", "geom_factor_m", "error_pct", "ip_mrad", "chargeability_mv_v",
               "stack", "contact_ohm", "array")

TYPE_INFO <- data.table(type = c("agi", "syscal", "other"),
                        manufacturer = c("Advanced Geosciences Inc. (AGI)", "IRIS Instruments", ""),
                        model_unknown = c("AGI SuperSting (model unknown)", "IRIS Instruments (model unknown)", ""))

head_of <- function(path, n = 4L) tryCatch(suppressWarnings(readLines(path, n = n, warn = FALSE, encoding = "latin1")), error = function(e) character())

FORMATS <- list(
  list(format = "agi_stg", type = "agi", role = "data", readable = TRUE,
       sniff = function(p, h) grepl("\\.stg$", p, TRUE) && length(h) && grepl("^Advanced Geosciences", h[1]) && !any(grepl("EarthImager", h[1:2])),
       read = function(p) read_agi_stg(p)),
  list(format = "agi_stg_earthimager", type = "agi", role = "derived", readable = TRUE,
       sniff = function(p, h) grepl("\\.stg$", p, TRUE) && length(h) > 1 && grepl("^Advanced Geosciences", h[1]) && grepl("EarthImager", h[2]),
       read = function(p) read_agi_stg(p)),
  list(format = "agi_crs", type = "agi", role = "companion", readable = TRUE,
       sniff = function(p, h) grepl("\\.crs$", p, TRUE) && any(grepl("Contact resistance", h, TRUE)) || grepl("\\.crs$", p, TRUE) && length(h) > 3 && grepl("Contact", h[4])),
  list(format = "agi_cmd", type = "agi", role = "companion", readable = TRUE,
       sniff = function(p, h) grepl("\\.cmd(\\.txt)?$", p, TRUE)),
  list(format = "agi_trn", type = "agi", role = "companion", readable = FALSE, sniff = function(p, h) grepl("\\.trn$", p, TRUE)),
  list(format = "agi_memory_bin", type = "agi", role = "companion", readable = FALSE,
       sniff = function(p, h) grepl("(\\.(stg|cmd)\\.bin|(^|[/\\\\])(CmdDir|FileDir|Calib)\\.bin)$", p, TRUE)),
  list(format = "syscal_prosys_txt", type = "syscal", role = "data", readable = TRUE,
       sniff = function(p, h) length(h) && grepl("El-array|Spa\\.1|xA ?\\(m\\)", h[1]) && grepl("Rho|VMN|Vp", h[1]),
       read = function(p) read_syscal_prosys(p)),
  list(format = "syscal_bin", type = "syscal", role = "data", readable = FALSE, sniff = function(p, h) grepl("\\.bin$", p, TRUE)),
  list(format = "res2dinv_dat", type = "other", role = "derived", readable = TRUE,
       sniff = function(p, h) grepl("\\.dat$", p, TRUE) && length(h) >= 3 && grepl("^\\s*[0-9.]+\\s*$", h[2]) && grepl("^\\s*[0-9]+\\s*$", h[3]),
       read = function(p) read_res2dinv(p)),
  list(format = "inversion_xyz", type = "other", role = "result", readable = FALSE,       # x, z, resistivity of an inverted model
       sniff = function(p, h) grepl("\\.(dat|txt|xyz)$", p, TRUE) && length(h) >= 3 && all(grepl("^\\s*-?[0-9.]+(E[-+]?[0-9]+)?(\\s+-?[0-9.]+(E[-+]?[0-9]+)?){2}\\s*$", h[1:3], ignore.case = TRUE))),
  list(format = "earthimager_result", type = "agi", role = "result", readable = FALSE, sniff = function(p, h) grepl("\\.(out|ini|mdl|att|pos)$", p, TRUE)),
  list(format = "gps", type = "other", role = "companion", readable = FALSE, sniff = function(p, h) grepl("\\.(gpx|kml|kmz)$", p, TRUE)),
  list(format = "coordinates", type = "other", role = "companion", readable = FALSE, sniff = function(p, h) grepl("\\.(xyz)$", p, TRUE)),
  list(format = "photo", type = "other", role = "notes", readable = FALSE, sniff = function(p, h) grepl("\\.(jpe?g|png|tiff?|heic)$", p, TRUE)),
  list(format = "document", type = "other", role = "notes", readable = FALSE, sniff = function(p, h) grepl("\\.(pdf|docx?|txt|xlsx?|rtf|md)$", p, TRUE)),
  list(format = "archive", type = "other", role = "result", readable = FALSE, sniff = function(p, h) grepl("\\.(zip|7z|rar)$", p, TRUE)),
  list(format = "matlab", type = "other", role = "result", readable = FALSE, sniff = function(p, h) grepl("\\.(m|mat)$", p, TRUE)))
FORMAT_INFO <- rbindlist(lapply(FORMATS, function(f) f[c("format", "type", "role", "readable")]))

sniff_format <- function(path) {
  h <- head_of(path)
  for (f in FORMATS) if (isTRUE(tryCatch(f$sniff(path, h), error = function(e) FALSE))) return(f$format)
  if (!length(h)) "empty_or_binary" else "unrecognised"
}

read_format <- function(path, format) {
  f <- Filter(function(x) x$format == format, FORMATS)[[1]]
  if (is.null(f$read)) stop("no reader for ", format)
  unread_take(); unread_mark(f$read(path))                                # cells that are not numbers, counted per file (core convert.R)
}

# ---- AGI SuperSting / Sting .stg ------------------------------------------------------------------------------------
# Instrument files: 3 header lines, then per record: rec no, USER, date, time, V/I (ohm), error (per mille), current (mA),
# apparent resistivity (ohm-m), survey name, A xyz, B xyz, M xyz, N xyz, then key=value settings (Cmd, HV, Cyk, MTime,
# Gain, Ch, and IP fields on IP firmware). EarthImager sub/trimmed sets have the same columns without the settings.
read_agi_stg <- function(path) {
  L <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr <- L[seq_len(min(3, length(L)))]
  meta <- list(manufacturer = TYPE_INFO$manufacturer[1], header = paste(hdr, collapse = " | "))
  m1 <- regmatches(hdr[1], regexec("(Sting|SuperSting)\\s+([A-Z0-9-]+)", hdr[1]))[[1]]
  meta$model <- if (length(m1) == 3) paste("AGI", m1[2], m1[3]) else TYPE_INFO$model_unknown[1]
  if (grepl("EarthImager", paste(hdr, collapse = " "))) meta$model <- TYPE_INFO$model_unknown[1]
  meta$serial   <- sub(".*S/N:\\s*([^ ]+).*", "\\1", grep("S/N:", hdr, value = TRUE)[1] %||% NA_character_)
  meta$firmware <- sub(".*Firmware version:\\s*([^ ]+).*", "\\1", grep("Firmware", hdr, value = TRUE)[1] %||% NA_character_)
  meta$software <- if (any(grepl("EarthImager", hdr))) sub(".*(EarthImager[^.]*\\. Version: [0-9.]+).*", "AGI \\1", grep("EarthImager", hdr, value = TRUE)[1]) else NA_character_
  meta$processing <- if (length(hdr) > 1 && grepl("EarthImager", hdr[2])) trim(sub("with AGI.*|by AGI.*", "", hdr[2])) else NA_character_
  meta$geometry_type <- sub(".*Type:\\s*([A-Za-z0-9]+).*", "\\1", grep("Type:", hdr[1], value = TRUE)[1] %||% NA_character_)
  meta$n_stated <- suppressWarnings(as.integer(sub(".*Records:\\s*([0-9]+).*", "\\1", grep("Records:", hdr, value = TRUE)[1] %||% NA)))
  meta$unit <- sub(".*Unit:\\s*([A-Za-z]+).*", "\\1", grep("Unit:", hdr, value = TRUE)[1] %||% NA_character_)
  rows <- L[-(1:3)]; rows <- rows[grepl("^\\s*[0-9]+\\s*,", rows)]
  if (!length(rows)) return(list(meta = meta, meas = empty_meas(), extra = data.table(meas_id = integer())))
  tok <- strsplit(rows, ",", fixed = TRUE)
  fx <- function(i) trim(vapply(tok, function(t) if (length(t) >= i) t[i] else NA_character_, ""))
  nm <- function(i) suppressWarnings(as.numeric(fx(i)))
  d <- data.table(meas_id = seq_along(rows), rec_no = as.integer(nm(1)),
                  datetime = as.POSIXct(paste(fx(3), fx(4)), format = "%Y%m%d %H:%M:%S", tz = "UTC"),
                  resistance_ohm = nm(5), error_permille = nm(6), current_ma = nm(7), rhoa_ohmm = nm(8), name = fx(9))
  xyz <- c("ax", "ay", "az", "bx", "by", "bz", "mx", "my", "mz", "nx", "ny", "nz")
  for (k in seq_along(xyz)) set(d, j = xyz[k], value = nm(9 + k))
  # key=value settings and anything else after the coordinates go to the instrument-specific table
  extra <- data.table(meas_id = d$meas_id, survey_name = d$name, error_permille = d$error_permille, current_ma = d$current_ma)
  kv <- lapply(tok, function(t) if (length(t) > 21) trim(t[22:length(t)]) else character())
  keys <- unique(unlist(lapply(kv, function(v) sub("=.*", "", v[grepl("=", v)]))))
  for (k in keys) set(extra, j = k, value = vapply(kv, function(v) { x <- v[startsWith(v, paste0(k, "="))]; if (length(x)) sub("^[^=]*=", "", x[1]) else NA_character_ }, ""))
  # IP firmware writes "IP:" then numbers after the coordinates; kept as ip_1, ip_2 ... (ResIPy reads the total time from
  # field 24 and six Vs/Vp windows from fields 25-30 of the line). Converting them to chargeability is left for later.
  ipf <- lapply(kv, function(v) { i <- grep("^IP", v); if (length(i)) suppressWarnings(as.numeric(sub("^IP:?\\s*", "", v[i[1]:length(v)]))) else numeric() })
  if (any(lengths(ipf) > 0)) for (k in seq_len(max(lengths(ipf)))) set(extra, j = paste0("ip_", k), value = vapply(ipf, function(x) if (length(x) >= k) x[k] else NA_real_, 0))
  other <- vapply(kv, function(v) paste(v[!grepl("=", v) & !grepl("^IP", v) & nzchar(v)], collapse = ","), "")
  if (any(nzchar(other))) extra[, unparsed := other]
  ren <- c(Cmd = "cmd_id", HV = "hv_v", Cyk = "cycles", MTime = "meas_time_s", Gain = "gain", Ch = "channel")
  hit <- intersect(names(ren), names(extra)); setnames(extra, hit, ren[hit])
  d[, `:=`(current_a = current_ma / 1000, error_pct = error_permille / 10)]
  d[, voltage_v := resistance_ohm * current_a]
  meas <- finish_meas(d)
  meta$survey_name <- names(sort(table(d$name), decreasing = TRUE))[1]
  list(meta = meta, meas = meas, extra = extra)
}

# AGI .crs: contact resistance between electrode pairs, measured before each command.
read_agi_crs <- function(path) {
  L <- readLines(path, warn = FALSE, encoding = "latin1"); L <- L[grepl("^\\s*[0-9]+\\s*,", L)]
  if (!length(L)) return(data.table())
  d <- fread(text = L, header = FALSE, fill = TRUE)
  setnames(d, seq_len(min(14, ncol(d))), c("rec_no", "vcode", "current_a", "contact_ohm", "date", "time", "addr_a", "addr_b",
                                           "ax", "ay", "az", "bx", "by", "bz")[seq_len(min(14, ncol(d)))])
  d[, datetime := as.POSIXct(paste(date, time), format = "%Y%m%d %H:%M:%S", tz = "UTC")][, c("date", "time") := NULL][]
}

# AGI .cmd: the command file. Returns the electrode geometry and the planned measurement list.
read_agi_cmd <- function(path) {
  L <- trim(sub("^\ufeff", "", readLines(path, warn = FALSE, encoding = "UTF-8")))
  sec <- cumsum(grepl("^:", L)); name <- sub("^:", "", L[grepl("^:", L)])[pmax(sec, 1)]
  body <- function(s) { x <- L[name == s & !grepl("^[:;]", L) & nzchar(L)]; x }
  h <- body("header"); hdr <- setNames(sub("^[^=]*=", "", h), sub("=.*", "", h))
  g <- body("geometry"); geo <- if (length(g)) setnames(fread(text = g, header = FALSE)[, 1:4], c("electrode", "x", "y", "z")) else data.table()
  cm <- body("commands"); cmds <- if (length(cm)) fread(text = cm, header = FALSE) else data.table()
  list(header = as.list(hdr), geometry = geo, n_commands = nrow(cmds), commands = cmds)
}

# ---- IRIS Instruments: Prosys ASCII export (Syscal and other IRIS meters) --------------------------------------------------------------------------------
# One header row (comma, semicolon or tab separated; units sometimes in the names), one row per quadrupole. Known columns:
# Spa.1-4 = A, B, M, N positions (m); Rho (ohm-m); Dev. (std of the stack, %); M (chargeability, mV/V); Sp, Vp (mV);
# In (mA); Mdly, Tm (ms); Stack; Rs or Rab (contact resistance); Date and Time when exported. Every column is also kept,
# under its own name, in the instrument-specific table. Exports carry no instrument model or serial.
# Prosys II/III name the columns xA (m), yA (m), zA (m), VMN (mV), IAB (mA), Dev. Rho (%), M (mV/V) (as read by ResIPy).
read_syscal_prosys <- function(path) {
  h <- head_of(path, 1); n <- c("," = lengths(regmatches(h, gregexpr(",", h))), ";" = lengths(regmatches(h, gregexpr(";", h))), "\t" = lengths(regmatches(h, gregexpr("\t", h))))
  raw <- fread(path, sep = names(n)[which.max(n)], colClasses = "character", na.strings = c("", "NA"), fill = TRUE, encoding = "Latin-1")
  orig <- names(raw); key <- tolower(gsub("[^A-Za-z0-9]", "", sub("\\(.*", "", orig)))
  setnames(raw, make.unique(ifelse(nzchar(orig), trim(orig), "blank")))
  col <- function(...) { i <- match(c(...), key); i <- i[!is.na(i)][1]; if (is.na(i)) rep(NA_real_, nrow(raw)) else num_cells(sub(",", ".", raw[[i]], fixed = TRUE)) }
  vp <- col("vp", "vmn"); inn <- col("in", "iab")
  err <- col("dev", "devrho"); if (all(err %in% c(0, NA)) && "devm" %in% key) err <- col("devm")   # Prosys III: Dev. M when IP was measured
  d <- data.table(meas_id = seq_len(nrow(raw)), rec_no = seq_len(nrow(raw)),
                  ax = col("spa1", "xa"), bx = col("spa2", "xb"), mx = col("spa3", "xm"), nx = col("spa4", "xn"),
                  ay = col("ya"), by = col("yb"), my = col("ym"), ny = col("yn"), az = col("za"), bz = col("zb"), mz = col("zm"), nz = col("zn"),
                  rhoa_ohmm = col("rho", "rhoa"), error_pct = err, chargeability_mv_v = col("m", "mtot"),
                  voltage_v = vp / 1000, current_a = inn / 1000, resistance_ohm = vp / inn, stack = col("stack"), contact_ohm = col("rs", "rab", "rschk"))
  for (v in c("ay", "az", "by", "bz", "my", "mz", "ny", "nz")) set(d, which(is.na(d[[v]])), v, 0)   # 2D exports have no y or z
  dt <- if (all(c("date", "time") %in% key)) as.POSIXct(paste(raw[[match("date", key)]], raw[[match("time", key)]]), tz = "UTC",
                                                         tryFormats = c("%d/%m/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S"), optional = TRUE) else NA
  d[, datetime := as.POSIXct(dt, tz = "UTC")]
  meta <- list(manufacturer = TYPE_INFO$manufacturer[2], model = TYPE_INFO$model_unknown[2], serial = NA_character_,
               software = "IRIS Prosys export", n_stated = NA_integer_, unit = "meter",
               header = paste(orig, collapse = ","), survey_name = sub("\\.[^.]*$", "", basename(path)),
               arrays_stated = paste(unique(raw[[match("elarray", key)]] %||% NA), collapse = "; "))
  extra <- cbind(data.table(meas_id = d$meas_id), raw[, setdiff(names(raw), "blank"), with = FALSE][, lapply(.SD, trim)])
  list(meta = meta, meas = finish_meas(d), extra = extra)
}

# ---- Res2DInv input (.dat), general array format ----------------------------------------------------------------------
# name, spacing, array (11 = general), sub-type, a text line, 0 = rhoa / 1 = resistance, n data, x-location type, IP flag,
# then per row: 4  Ax Az  Bx Bz  Mx Mz  Nx Nz  value [ip]. The name line often holds the original instrument file
# (for example FG72_3917.bin, a Syscal binary name), which is kept as provenance. Other layouts are listed as not read.
read_res2dinv <- function(path) {
  L <- trim(readLines(path, warn = FALSE, encoding = "latin1"))
  if (length(L) < 10 || trim(L[3]) != "11") stop("Res2DInv layout not read yet (array code '", L[3], "'; only the general array, 11, is read)")
  is_r <- trim(L[6]) == "1"; n <- as.integer(L[7]); ip <- trim(L[9]) == "1"
  rows <- L[10:(9 + n)]; rows <- rows[grepl("^4\\s", rows)]
  t <- fread(text = rows, header = FALSE)
  d <- data.table(meas_id = seq_len(nrow(t)), rec_no = seq_len(nrow(t)), ax = t$V2, az = t$V3, bx = t$V4, bz = t$V5,
                  mx = t$V6, mz = t$V7, nx = t$V8, nz = t$V9)
  for (v in c("ay", "by", "my", "ny")) set(d, j = v, value = 0)
  if (is_r) d[, resistance_ohm := t$V10] else d[, rhoa_ohmm := t$V10]
  if (ip && ncol(t) >= 11) d[, chargeability_mv_v := t$V11]
  d[, datetime := as.POSIXct(NA, tz = "UTC")]
  m <- finish_meas(d)
  if (is_r) m[, rhoa_ohmm := geom_factor_m * resistance_ohm] else m[, resistance_ohm := rhoa_ohmm / geom_factor_m]
  meta <- list(manufacturer = NA_character_, model = NA_character_, software = "Res2DInv input (general array)",
               spacing_stated = suppressWarnings(as.numeric(L[2])), n_stated = n, survey_name = L[1],
               processing = paste0("made from ", L[1], if (grepl("\\.bin$", L[1], TRUE)) " (an instrument binary name, likely IRIS; not in the archive)" else ""))
  list(meta = meta, meas = m, extra = data.table(meas_id = m$meas_id))
}

empty_meas <- function() { d <- as.data.table(setNames(replicate(length(MEAS_COLS), numeric(), simplify = FALSE), MEAS_COLS)); d[, datetime := as.POSIXct(character(), tz = "UTC")][, array := character()][] }

# Common finishing for every reader: electrode numbers from positions, geometric factor, array type, column order.
finish_meas <- function(d) {
  for (v in setdiff(MEAS_COLS, names(d))) set(d, j = v, value = NA_real_)
  P <- unique(rbindlist(lapply(c("a", "b", "m", "n"), function(e) d[, .(x = get(paste0(e, "x")), y = get(paste0(e, "y")), z = get(paste0(e, "z")))])))
  # remote (infinite) electrodes are written as coordinates of 9999 or more (AGI, IRIS): electrode 0, at infinity
  for (e in c("a", "b", "m", "n")) { r <- abs(d[[paste0(e, "x")]]) >= 9999 | abs(d[[paste0(e, "y")]]) >= 9999
    for (k in c("x", "y", "z")) set(d, which(r), paste0(e, k), Inf) }
  P <- P[!is.na(x) & abs(x) < 9999 & abs(y) < 9999]; setorder(P, x, y, z)
  # electrode number from the distance along the line when positions sit on a regular spacing (unused electrodes keep
  # their number), else the rank of the position
  P[, s := c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))]
  st <- round(diff(P$s), 2); st <- st[st > 0]
  sp <- if (length(st)) as.numeric(names(which.max(table(st)))) else NA_real_
  if (!is.na(sp) && sp > 0 && all(abs(P$s / sp - round(P$s / sp)) < 0.05)) P[, electrode := as.integer(round(s / sp)) + 1L] else P[, electrode := .I]
  key <- function(x, y, z) paste(round(x, 3), round(y, 3), round(z, 3))
  P[, k := key(x, y, z)]
  for (e in c("a", "b", "m", "n")) set(d, j = e, value = ifelse(is.infinite(d[[paste0(e, "x")]]), 0L,
                                                         P$electrode[match(key(d[[paste0(e, "x")]], d[[paste0(e, "y")]], d[[paste0(e, "z")]]), P$k)]))
  dist <- function(e1, e2) ifelse(is.infinite(d[[paste0(e1, "x")]]) | is.infinite(d[[paste0(e2, "x")]]), Inf, sqrt((d[[paste0(e1, "x")]] - d[[paste0(e2, "x")]])^2 + (d[[paste0(e1, "y")]] - d[[paste0(e2, "y")]])^2 + (d[[paste0(e1, "z")]] - d[[paste0(e2, "z")]])^2))
  am <- dist("a", "m"); bm <- dist("b", "m"); an <- dist("a", "n"); bn <- dist("b", "n")
  d[, geom_factor_m := 2 * pi / (1 / am - 1 / bm - 1 / an + 1 / bn)]
  d[, array := classify_array(a, b, m, n)]
  d[, as.data.table(.SD), .SDcols = MEAS_COLS]
}

# Array type of each quadrupole from electrode numbers (equal spacing assumed; "mixed" otherwise; 0 = remote electrode).
classify_array <- function(a, b, m, n) {
  lo <- pmin(a, b); hi <- pmax(a, b); plo <- pmin(m, n); phi <- pmax(m, n)
  out <- rep("mixed", length(a))
  inside <- plo > lo & phi < hi
  out[inside & (plo - lo) == (phi - plo) & (hi - phi) == (phi - plo)] <- "wenner"
  out[inside & (plo - lo) == (hi - phi) & (phi - plo) < (plo - lo)] <- "schlumberger"
  out[!inside & (hi - lo) == (phi - plo) & (plo > hi | phi < lo)] <- "dipole-dipole"
  out[(b == 0) %in% TRUE | (n == 0) %in% TRUE] <- "pole-dipole"
  out[(b == 0 & n == 0) %in% TRUE] <- "pole-pole"
  out[is.na(a) | is.na(m)] <- NA_character_
  out
}
