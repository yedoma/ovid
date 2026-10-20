# =============================================================================
#  R/gnss.R  --  GNSS survey CSVs: read, stitch together, export   (Ovid Core // RS)
# =============================================================================
# Reads point exports from Emlid Reach (Emlid Flow / ReachView), DJI RTK (D-RTK and aircraft RTK point or MRK exports) and
# Leica (Captivate / Infinity / LGO CSV, with or without a header), plus any CSV with latitude/longitude or easting/northing.
# Each file becomes rows with the same columns (GNSS_COLS); stitching binds them, drops exact repeats and flags point names
# that appear with different coordinates. Values are copied as found: no datum change, adjustment or re-processing.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R.

GNSS_COLS <- c("device", "source_file", "row", "point", "code", "lat", "lon", "ellip_h", "easting", "northing", "elevation",
               "solution", "rms_h", "rms_v", "antenna_h", "n_sat", "pdop", "hdop", "vdop", "samples", "age_s", "baseline_m", "correction",
               "time", "crs", "description", "note")
GNSS_NUM <- c("lat", "lon", "ellip_h", "easting", "northing", "elevation", "rms_h", "rms_v", "antenna_h", "n_sat", "pdop", "hdop", "vdop", "samples", "age_s", "baseline_m")

# header name (lower case, spaces and punctuation squashed) -> column. First match wins.
GNSS_ALIASES <- list(
  point = c("name", "pointid", "point", "id", "pt", "ptid", "pointname", "station"),
  code = c("code", "featurecode", "pointcode", "attribute"),
  lat = c("latitude", "lat", "wgs84latitude", "latdeg", "latitudedeg"),
  lon = c("longitude", "lon", "long", "lng", "wgs84longitude", "londeg", "longitudedeg"),
  ellip_h = c("ellipsoidalheight", "ellipsoidheight", "ellhgt", "ellipsoidhgt", "height", "hae", "altitude", "alt", "ellh"),
  easting = c("easting", "east", "e", "x", "gridx", "localeasting"),
  northing = c("northing", "north", "n", "y", "gridy", "localnorthing"),
  elevation = c("elevation", "orthohgt", "orthometricheight", "orthoheight", "elev", "z", "msl", "localheight"),
  solution = c("solutionstatus", "solution", "fixtype", "fix", "status", "rtkstatus", "rtkflag", "positionquality", "gpsquality", "quality", "postype", "solutiontype"),
  rms_h = c("lateralrms", "horizontalrms", "hrms", "hzprec", "2dcq", "poscq", "positionqlty", "posqlty", "horizontalprecision", "hprec", "stdh"),
  rms_v = c("elevationrms", "verticalrms", "vrms", "htprec", "1dcq", "heightcq", "heightqlty", "hgtqlty", "verticalprecision", "vprec", "stdv"),
  antenna_h = c("antennaheight", "antheight", "anthgt", "antennahgt", "instrumentheight"),
  # quality of each position, where the file gives it: satellites used, dilution of precision, averaging samples (epochs),
  # age of the corrections, distance to the base, and how it was corrected (NTRIP, LoRa, PPK ...)
  n_sat = c("satellites", "sats", "numsats", "numberofsatellites", "nsat", "nsats", "svs", "satellitesused", "satsused", "noofsatellites", "satellitecount"),
  pdop = c("pdop", "positiondop"), hdop = c("hdop"), vdop = c("vdop"),
  samples = c("samples", "epochs", "numberofepochs", "averagingsamples", "noofepochs"),
  age_s = c("age", "diffage", "differentialage", "correctionage", "ageofdifferential", "agesec", "ageofcorrections"),
  baseline_m = c("baseline", "baselinelength", "baselinem", "baselinelengthm"),
  correction = c("correctiontype", "correctionsource", "rtksource", "corrections"),
  time = c("averagingstart", "collectionstart", "datetime", "time", "timestamp", "date", "gpstime", "utctime", "observedat", "measuredat"),
  crs = c("csname", "coordinatesystem", "crs", "projection", "epsg"),
  description = c("description", "desc", "remark", "remarks", "note", "notes", "comment"))

if (!exists("%|na|%")) `%|na|%` <- function(a, b) ifelse(is.na(a) | a == "", b, a)     # also defined by the app; the pipeline needs it for Leica GSI units
gnss_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Which unit wrote the file, from its header (and name); "generic" when nothing says.
gnss_device <- function(hdr, file) {
  k <- gnss_key(hdr); f <- tolower(basename(file))
  if (any(k %in% c("solutionstatus", "averagingstart", "lateralrms", "csname", "correctiontype")) || grepl("emlid|reach", f)) return("Emlid Reach")
  if (any(k %in% c("3dcq", "2dcq", "1dcq", "poscq", "orthohgt", "pointid") ) || any(grepl("leica|captivate|lgo", c(k, f)))) return("Leica")
  if (any(grepl("dji|rtkflag|rtkstatus|drtk", c(k, f))) || grepl("\\.mrk$", f)) return("DJI RTK")
  "generic"
}

# DJI timestamp.MRK: one line per photo, "<n> <sow> [<week>] ... <lat>,Lat <lon>,Lon <h>,Ellh ... <flag>,Q"
read_dji_mrk_points <- function(file) {
  l <- readLines(file, warn = FALSE); l <- l[grepl(",Lat", l, fixed = TRUE)]
  if (!length(l)) return(data.table())
  g <- function(tag) suppressWarnings(as.numeric(sub(paste0(".*?(-?[0-9.]+),", tag, ".*"), "\\1", l)))
  data.table(point = trim(sub("^\\s*([0-9]+).*", "\\1", l)), lat = g("Lat"), lon = g("Lon"), ellip_h = g("Ellh"),
             solution = sub(".*?([0-9]+),Q.*", "\\1", l), time = trim(sub("^\\s*[0-9]+\\s+([0-9.]+)\\s+\\[?([0-9]+)?\\]?.*", "GPS sow \\1 week \\2", l)))
}

# Emlid position logs. Reach writes .LLH ("yyyy/mm/dd hh:mm:ss.sss lat lon h Q ns sdn sde sdu ..."); Emlid Studio and
# RTKLIB write .pos with the same columns under "%" header lines (time may be "week sow"). Q: 1 fix, 2 float, 3 SBAS,
# 4 DGPS, 5 single, 6 PPP. Kept at one row per second so long logs stay light; lat/lon in degrees only.
GNSS_Q <- c("FIX", "FLOAT", "SBAS", "DGPS", "SINGLE", "PPP")
read_emlid_log <- function(file) {
  l <- readLines(file, warn = FALSE); hdr <- l[startsWith(l, "%")]; l <- trim(l[!startsWith(l, "%") & nzchar(trim(l))])
  if (any(grepl("ecef|d'", hdr))) return(structure(data.table(), problem = "solution log is in ECEF or deg-min-sec; export it in decimal degrees to plot"))
  w <- strsplit(l, "\\s+"); w <- w[lengths(w) >= 6]
  if (!length(w)) return(data.table())
  m <- do.call(rbind, lapply(w, function(x) x[1:15]))                  # ... Q ns sdn sde sdu sdne sdeu sdun age ratio
  q <- suppressWarnings(as.integer(m[, 6]))
  d <- data.table(time = paste(m[, 1], m[, 2]), lat = m[, 3], lon = m[, 4], ellip_h = m[, 5], solution = GNSS_Q[q], point = "",
                  rms_h = suppressWarnings(sprintf("%.4f", sqrt(as.numeric(m[, 8])^2 + as.numeric(m[, 9])^2))), rms_v = m[, 10], description = "position log",
                  n_sat = m[, 7], age_s = m[, 14])
  d[!duplicated(substr(time, 1, 19))]
}

# NMEA position log: GGA sentences ($GNGGA, $GPGGA). Quality 4 fix, 5 float, 2 DGPS, 1 single. Ellipsoidal height = MSL + geoid.
read_nmea_log <- function(file) {
  l <- grep("^\\$..GGA,", readLines(file, warn = FALSE), value = TRUE); if (!length(l)) return(data.table())
  f <- do.call(rbind, lapply(strsplit(sub("\\*.*$", "", l), ","), function(x) x[1:14]))      # ... 8 satellites, 9 HDOP, 14 age of the corrections
  dm <- function(v, h) { v <- suppressWarnings(as.numeric(v)); d <- floor(v / 100) + (v %% 100) / 60; ifelse(h %in% c("S", "W"), -d, d) }
  q <- c(`1` = "SINGLE", `2` = "DGPS", `4` = "FIX", `5` = "FLOAT", `6` = "DEAD RECKONING")[f[, 7]]
  d <- data.table(time = f[, 2], lat = dm(f[, 3], f[, 4]), lon = dm(f[, 5], f[, 6]), solution = unname(q), point = "",
                  ellip_h = suppressWarnings(as.numeric(f[, 10]) + as.numeric(f[, 12])), elevation = f[, 10], description = "NMEA log",
                  n_sat = f[, 8], hdop = f[, 9], age_s = f[, 14])
  d[is.finite(lat)][!duplicated(substr(time, 1, 6))]
}

# Degrees written as d deg m's" or with a hemisphere letter -> decimal; plain numbers pass through untouched.
gnss_deg <- function(x) {
  x <- as.character(x); i <- which(grepl("[^-+0-9., ]", x) | grepl("[0-9] +[0-9]", trim(x)))
  for (k in i) { n <- as.numeric(regmatches(x[k], gregexpr("[0-9]+(\\.[0-9]+)?", x[k]))[[1]]); n <- n[seq_len(min(3, length(n)))]
    if (length(n)) x[k] <- as.character((if (grepl("^\\s*-|[SWsw]", x[k])) -1 else 1) * sum(n / c(1, 60, 3600)[seq_along(n)])) }
  x
}
GNSS_RAW <- "\\.(ubx|erb|rtcm3?|[0-9]{2}[ognmdlpbONGMDLPB]|obs|nav|rnx)$"

# Leica GSI-8 / GSI-16 (Viva custom export, TPS and older Leica GNSS). Words: WI (2), info (4, last = unit), sign, data.
# WI 11 point id, 71 code/remark, 81 easting, 82 northing, 83 height. Unit 0/1 = 1/1000 m or ft, 6/7 = 1/10000, 8 = 1/100000 m.
read_leica_gsi <- function(file) {
  l <- sub("^\\*", "", trim(readLines(file, warn = FALSE))); l <- l[grepl("^11", l)]; if (!length(l)) return(data.table())
  sc <- c(`0` = 1e-3, `1` = 1e-3, `6` = 1e-4, `7` = 1e-4, `8` = 1e-5)
  rbindlist(lapply(strsplit(l, " +"), function(w) {
    wi <- substr(w, 1, 2); dat <- function(k) { x <- w[wi == k][1]; if (is.na(x)) NA_character_ else substring(x, 8) }
    num <- function(k) { x <- w[wi == k][1]; if (is.na(x)) return(NA_character_)
      as.character(as.numeric(substring(x, 8)) * (sc[substr(x, 6, 6)] %|na|% 1e-3) * (if (substr(x, 7, 7) == "-") -1 else 1)) }
    data.table(point = sub("^0+(?=.)", "", dat("11"), perl = TRUE), code = sub("^0+(?=.)", "", dat("71"), perl = TRUE),
               easting = num("81"), northing = num("82"), elevation = num("83"))
  }))
}

# LandXML (Viva "Export XML data"): <CgPoint name=".." code="..">northing easting elevation</CgPoint>, grid coordinates.
read_landxml_points <- function(file) {
  x <- paste(readLines(file, warn = FALSE), collapse = " ")
  p <- regmatches(x, gregexpr("<CgPoint[^>]*>[^<]*</CgPoint>", x))[[1]]; if (!length(p)) return(data.table())
  at <- function(a) { v <- sub(paste0('.*\\b', a, '="([^"]*)".*'), "\\1", sub(">.*", "", p)); ifelse(grepl(paste0('\\b', a, '="'), p), v, NA_character_) }
  v <- strsplit(trim(sub(".*>([^<]*)</CgPoint>", "\\1", p)), "\\s+")
  data.table(point = at("name"), code = at("code"), description = at("desc"),
             northing = sapply(v, `[`, 1), easting = sapply(v, `[`, 2), elevation = sapply(v, `[`, 3))
}

# Grid-only points (Leica ASCII/GSI/LandXML exports are always local grid) can only go on the map with a zone.
# UTM to WGS84 lat/lon, for display only: the grid values stay as found; the zone comes from the file's CS name
# ("... UTM zone 10N") or from the zone typed on the GNSS page.
gnss_zone <- function(x) { m <- regexpr("(?i)zone ?[0-9]{1,2} ?[NS]\\b", x, perl = TRUE); z <- rep(NA_character_, length(x))
  z[!is.na(m) & m > 0] <- gsub("ZONE| ", "", toupper(regmatches(x, m))); z }

# One file -> rows in GNSS_COLS. Errors become a note row-free empty table with attr "problem".
read_gnss_file <- function(file, name = basename(file)) {
  if (grepl("\\.zip$", name, ignore.case = TRUE)) {   # Emlid Flow downloads logs and projects as zip archives
    # safe: a zip that cannot be opened gives no files, and the caller says no rows were read
    x <- file.path(tempfile("gnsszip"), ""); inner <- tryCatch(utils::unzip(file, exdir = x), error = function(e) character())
    inner <- inner[!grepl("__MACOSX|/\\._", inner) & grepl("\\.(csv|txt|llh|pos|nmea|mrk)$", inner, ignore.case = TRUE)]
    if (!length(inner)) return(structure(data.table(), problem = paste(name, ": no point files or position logs inside (raw logs only?)")))
    parts <- lapply(inner, function(f) read_gnss_file(f, paste0(name, "/", basename(f))))
    d <- rbindlist(parts, fill = TRUE); p <- unlist(lapply(parts, attr, "problem"))
    return(if (nrow(d)) structure(d, problem = p) else structure(data.table(), problem = c(p, paste(name, ": no coordinates found"))))
  }
  if (grepl(GNSS_RAW, name, ignore.case = TRUE)) return(structure(data.table(), problem = paste(name, ": raw GNSS observations, no positions to plot (not processed here)")))
  if (grepl("\\.mrk$", name, ignore.case = TRUE)) { d <- read_dji_mrk_points(file); dev <- "DJI RTK" }
  else if (grepl("\\.(llh|pos)$", name, ignore.case = TRUE)) { d <- read_emlid_log(file); dev <- if (grepl("\\.llh$", name, ignore.case = TRUE)) "Emlid Reach" else "Emlid Studio / RTKLIB"
    if (!nrow(d)) return(structure(data.table(), problem = paste(name, ":", attr(d, "problem") %||% "no positions read"))) }
  else if (grepl("\\.nmea$", name, ignore.case = TRUE)) { d <- read_nmea_log(file); dev <- "NMEA log" }
  else if (grepl("\\.gsi$", name, ignore.case = TRUE)) { d <- read_leica_gsi(file); dev <- "Leica" }
  else if (grepl("\\.xml$", name, ignore.case = TRUE)) { d <- read_landxml_points(file); dev <- "Leica" }
  else {
    # safe: the next line returns "no rows read" as the file's problem
    d <- tryCatch(fread(file, colClasses = "character", fill = TRUE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(structure(data.table(), problem = paste(name, ": no rows read")))
    hdr <- names(d)
    if (all(grepl("^V[0-9]+$", hdr))) {   # no header: Leica style ID, E, N, H [, code] (or ID, lat, lon, h)
      v <- suppressWarnings(as.numeric(d[[2]])); geo <- all(abs(v) <= 90, na.rm = TRUE)
      nm <- c("point", if (geo) c("lat", "lon") else c("easting", "northing"), if (geo) "ellip_h" else "elevation", "code", "description")
      setnames(d, seq_len(min(ncol(d), length(nm))), nm[seq_len(min(ncol(d), length(nm)))]); dev <- if (geo) "generic" else "Leica"
    } else {
      dev <- gnss_device(hdr, name); k <- gnss_key(hdr); out <- list()
      for (col in names(GNSS_ALIASES)) { i <- match(GNSS_ALIASES[[col]], k); i <- i[!is.na(i)]
        i <- setdiff(i, unlist(lapply(out, attr, "i"))); if (length(i)) out[[col]] <- structure(d[[i[1]]], i = i[1]) }
      sat_cols <- which(grepl("^(gps|glonass|galileo|beidou|qzss|sbas|navic)satellites$", k))      # Emlid Flow: satellites per constellation
      if (!length(out$n_sat) && length(sat_cols)) out$n_sat <- rowSums(sapply(sat_cols, function(j) suppressWarnings(as.numeric(d[[j]]))), na.rm = TRUE)
      d <- as.data.table(lapply(out, as.vector))
    }
  }
  d <- ensure_cols(d, GNSS_COLS)[, ..GNSS_COLS]
  for (c in c("lat", "lon")) set(d, j = c, value = gnss_deg(d[[c]]))
  for (c in GNSS_NUM)
    set(d, j = c, value = suppressWarnings(as.numeric(gsub(",", ".", sub("^\\s*([-0-9.,]+).*", "\\1", d[[c]])))))
  if (!any(is.finite(d$lat) | is.finite(d$easting))) return(structure(data.table(), problem = paste(name, ": no coordinates found")))
  d[, `:=`(device = dev, source_file = name, row = seq_len(.N))]
  d[]
}

# Bind several files, drop exact repeats (same point and coordinates and time), flag names reused with other coordinates.
stitch_gnss <- function(files, names = basename(files), zone = NA_character_) {
  parts <- Map(read_gnss_file, files, names); probs <- unlist(lapply(parts, attr, "problem"))
  d <- rbindlist(parts, fill = TRUE); if (!nrow(d)) return(structure(ensure_cols(data.table(), GNSS_COLS)[0], problems = probs))
  d[, note := ""]
  k <- d[, paste(point, round(lat, 8), round(lon, 8), round(easting, 3), round(northing, 3), time)]
  dup <- duplicated(k); if (any(dup)) say("  ", sum(dup), " repeated GNSS rows dropped")
  d <- d[!dup]
  d[, n_xy := uniqueN(paste(round(lat, 6), round(lon, 6), round(easting, 2), round(northing, 2))), by = point]
  d[nzchar(point) & n_xy > 1, note := "point name used with different coordinates"][, n_xy := NULL]
  # grid-only rows: lat/lon for the map from the file's UTM zone, else the zone typed on the page; grid values untouched
  d[, z := fifelse(is.na(gnss_zone(crs)), toupper(gsub(" ", "", zone %||% NA_character_)), gnss_zone(crs))]
  g <- d[, which(!is.finite(lat) & is.finite(easting) & is.finite(northing) & grepl("^[0-9]{1,2}[NS]$", z))]
  if (length(g)) { ll <- utm_to_ll(d$easting[g], d$northing[g], d$z[g])
    d[g, `:=`(lat = ll[, "lat"], lon = ll[, "lon"], note = trim(paste(note, "lat/lon derived from UTM", z, "grid, for the map only")))] }
  if (any(!is.finite(d$lat) & is.finite(d$easting))) probs <- c(probs, "some points have grid coordinates only: type their UTM zone (for example 33N) to put them on the map")
  d[, z := NULL]
  crs <- unique(d$crs[!is.na(d$crs) & nzchar(d$crs)]); if (length(crs) > 1) probs <- c(probs, paste("files use different coordinate systems:", paste(crs, collapse = ", ")))
  structure(d[], problems = probs)
}

write_gnss <- function(d, file) { dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE); fwrite(d, file); invisible(file) }

# Quality of each file's positions, for the GNSS page: how many, what share were fixed, the median horizontal and vertical
# RMS, the median PDOP and the fewest satellites, the oldest corrections and the longest baseline (where the file gives them).
gnss_quality <- function(d) {
  if (is.null(d) || !nrow(d)) return(data.table())
  m <- function(x) if (any(is.finite(x))) round(stats::median(x[is.finite(x)]), 3) else NA_real_
  d[, .(points = .N, fixed_pct = if (any(!is.na(solution) & nzchar(solution))) round(100 * mean(toupper(solution) %in% c("FIX", "FIXED", "50", "RTK FIX", "RTK FIXED")), 1) else NA_real_,
        rms_h_median_m = m(rms_h), rms_v_median_m = m(rms_v), pdop_median = m(pdop), hdop_median = m(hdop),
        min_satellites = if (any(is.finite(n_sat))) min(n_sat, na.rm = TRUE) else NA_real_, max_age_s = if (any(is.finite(age_s))) max(age_s, na.rm = TRUE) else NA_real_,
        max_baseline_m = if (any(is.finite(baseline_m))) round(max(baseline_m, na.rm = TRUE), 1) else NA_real_,
        correction = paste(unique(correction[!is.na(correction) & nzchar(correction)]), collapse = ", ")), by = .(device, source_file)]
}

# Each file's quality against the calibrated limits (config/calibration_options.csv, applies_to a base or file name, or
# *): the oldest corrections, the longest baseline and the share of fixed positions. "ok", or what is over a limit; no limit, no check.
gnss_limit_check <- function(q, cal = NULL, bases = NULL) {
  if (is.null(q) || !nrow(q)) return(data.table())
  b <- gnss_base_rows(q$source_file, bases)
  out <- q[, .(source_file, base = b$base[match(source_file, b$source_file)])]
  out[, limits := mapply(function(f, bs, age, bl, fx) {
    nm <- if (!is.na(bs) && nzchar(bs)) bs else f; lim <- function(v) { n <- cal_num(cal, v, nm); if (is.finite(n)) n else cal_num(cal, v, f) }
    a <- lim("max_correction_age_s"); l <- lim("max_baseline_m"); m <- lim("min_fixed_pct")
    w <- c(if (is.finite(age) && is.finite(a) && age > a) sprintf("corrections up to %s s old (limit %s s)", format(age), format(a)),
           if (is.finite(bl) && is.finite(l) && bl > l) sprintf("baseline up to %s km (limit %s km)", format(round(bl / 1000, 1)), format(l / 1000)),
           if (is.finite(fx) && is.finite(m) && fx < m) sprintf("%s %% fixed (minimum %s %%)", format(fx), format(m)))
    if (length(w)) paste(w, collapse = "; ") else "ok" }, source_file, base, q$max_age_s, q$max_baseline_m, q$fixed_pct)]
  out[, .(source_file, limits)]
}

# ---- GNSS base: how it was positioned -------------------------------------------------------------------------------------
# A base placed by single averaging is only metres accurate, however long it averaged, and every point surveyed from it is
# off by the same amount in absolute terms while their RTK RMS still reads centimetres. Logging the base's raw data and
# post-processing it (NRCan CSRS-PPP in Canada, or a nearby CORS) gives an absolute position. The method is recorded in
# config/gnss_bases.csv; the files rarely say. Sources: Emlid docs and community forum; NRCan CSRS-PPP
# (_hub/Derived_values_references.md, section 14).

# A calibration value for one name (core/calibration.R): a row for the name over *, a set choice over the default;
# default and scotty_creek numbers come from RS_CALIBRATION. A blank number means no limit (NA).
rs_calibration <- function(cal, value, name = "*") { g <- calib_get(cal, RS_CALIBRATION, value, name); g[c("number", "source", "choice", "note")] }
cal_num <- function(cal, value, name = "*") rs_calibration(cal, value, name)$number

# The gnss_bases row for each file: an exact file name wins over a pattern (* ? as in file names), and a pattern over *.
gnss_base_rows <- function(files, bases) {
  b <- if (is.null(bases)) empty_cfg(GNSS_BASE_COLS) else ensure_cols(copy(bases), GNSS_BASE_COLS)
  b <- b[nzchar(file)]
  rbindlist(lapply(files, function(f) {
    hit <- b[file == f | basename(f) == file]
    if (!nrow(hit)) { g <- b[file != "*" & vapply(file, function(x) grepl(utils::glob2rx(x), basename(f), ignore.case = TRUE), TRUE)]; hit <- g }
    if (!nrow(hit)) hit <- b[file == "*"]
    if (nrow(hit)) cbind(source_file = f, hit[1, !"file"]) else data.table(source_file = f, base = "", method = "", post_processed = "", service = "", notes = "")
  }), fill = TRUE)
}

# Per file: the base, how it was set, whether it was post-processed and with what, and the check: ok, flag or needs you.
# Nothing is assigned on a guess: a point named or coded base with a single solution is only a suggestion.
gnss_base_check <- function(d, bases = NULL, cal = NULL) {
  if (is.null(d) || !nrow(d)) return(data.table())
  r <- gnss_base_rows(unique(d$source_file), bases)
  r[, method := tolower(trim(method))][, post_processed := tolower(trim(post_processed))]
  m <- match(r$method, tolower(BASE_METHODS)); r[!is.na(m), method := BASE_METHODS[m[!is.na(m)]]]   # as listed, whatever its case: "network (NTRIP / CORS)"
  r[, c("base_check", "base_note") := {
    pp <- post_processed %in% c("yes", "y", "true")
    if (method %in% names(BASE_LIMITS) && !pp) {
      a <- rs_calibration(cal, BASE_LIMITS[[method]], if (nzchar(base)) base else source_file)
      list("flag", sprintf("Base set by %s and not post-processed: every point's absolute position may be off by up to about %s m (%s), though positions relative to each other keep their RTK accuracy. Post-process the base's raw log (NRCan CSRS-PPP, at least 4 h, or a nearby CORS) and shift the points, or record it as post-processed.",
                           method, format(a$number), a$source)) }
    else if (method %in% names(BASE_LIMITS) && pp)
      list("ok", paste0("Base set by ", method, ", then post-processed", if (nzchar(service)) paste0(" (", service, ")") else " (service not recorded)", "."))
    else if (method %in% setdiff(BASE_METHODS, "unknown"))
      list("ok", paste0("Base: ", method, if (pp) paste0(", post-processed", if (nzchar(service)) paste0(" (", service, ")") else "") else "", ".",
                        if (method == "known point" && !nzchar(service)) " Record in service where the point's coordinates came from (for example NRCan CSRS-PPP)." else ""))
    else {
      s <- d[source_file == .BY$source_file & grepl("base", paste(point, code), ignore.case = TRUE) & toupper(solution) == "SINGLE"]
      list("needs you", paste0("How the base was positioned is not recorded: add it to GNSS bases (method, post-processed, service).",
                               if (nrow(s)) paste0(" Point ", s$point[1], " has a single solution: was the base set by single averaging?") else ""))
    } }, by = source_file]
  setnames(r, c("base", "method", "post_processed", "service"), c("base", "base_method", "base_post_processed", "base_service"))
  r[, .(source_file, base, base_method, base_post_processed, base_service, base_check, base_note)]
}

# ---- checkpoint accuracy (ASPRS Positional Accuracy Standards for Digital Geospatial Data, Edition 2, 2023) ---------------
# A product (orthomosaic, DSM, point cloud) checked against surveyed checkpoints: one row per point with the surveyed and the
# product's coordinates, in metres. RMSE only (Edition 2 has no 95 % statistic): RMSE_H = sqrt(RMSE_x^2 + RMSE_y^2), RMSE_V,
# RMSE_3D = sqrt(RMSE_H^2 + RMSE_V^2); the checkpoints' own survey error is added in quadrature to the product's accuracy.
# At least 30 checkpoints (at most 120) are required; with fewer the result is reported with that statement. Checkpoints
# should be at least twice as accurate as the product.
CHECKPOINT_FIELDS <- data.table(
  field   = c("id", "x_survey", "y_survey", "z_survey", "x_product", "y_product", "z_product"),
  label   = c("Point", "Surveyed easting", "Surveyed northing", "Surveyed elevation", "Product easting", "Product northing", "Product elevation"),
  aliases = c("name|point|checkpoint|check point|gcp|label", "x|e|easting|survey x|survey e|x survey|e survey|x_gnss|e_gnss|x true|easting true|x_ref|e_ref",
              "y|n|northing|survey y|survey n|y survey|n survey|y_gnss|n_gnss|y true|northing true|y_ref|n_ref",
              "z|h|elevation|height|survey z|z survey|z_gnss|z true|elevation true|z_ref|h_ref",
              "x product|e product|x_measured|e_measured|x_ortho|x_model|x_est|easting measured|x computed|x_comp",
              "y product|n product|y_measured|n_measured|y_ortho|y_model|y_est|northing measured|y computed|y_comp",
              "z product|z_measured|z_dsm|z_model|z_est|elevation measured|z computed|z_comp|dsm"),
  type = c("text", rep("number", 6)),
  identifies = c("TRUE", rep("FALSE", 6)))
checkpoint_accuracy <- function(d, survey_rmse_h_m = 0, survey_rmse_v_m = 0) {
  num <- function(k) if (k %in% names(d)) suppressWarnings(as.numeric(d[[k]])) else rep(NA_real_, nrow(d))
  p <- data.table(id = if ("id" %in% names(d)) as.character(d$id) else as.character(seq_len(nrow(d))),
                  dx = num("x_product") - num("x_survey"), dy = num("y_product") - num("y_survey"), dz = num("z_product") - num("z_survey"))
  p[, dh := sqrt(dx^2 + dy^2)]
  rm <- function(v) { v <- v[is.finite(v)]; if (length(v)) sqrt(mean(v^2)) else NA_real_ }
  nh <- sum(is.finite(p$dh)); nv <- sum(is.finite(p$dz)); rx <- rm(p$dx); ry <- rm(p$dy); rh <- sqrt(rx^2 + ry^2); rv <- rm(p$dz)
  sh <- if (is.finite(survey_rmse_h_m)) survey_rmse_h_m else 0; sv <- if (is.finite(survey_rmse_v_m)) survey_rmse_v_m else 0
  ph <- sqrt(rh^2 + sh^2); pv <- sqrt(rv^2 + sv^2)
  cm <- function(x) round(100 * x, 1)
  s <- data.table(checkpoints_horizontal = nh, checkpoints_vertical = nv, mean_dx_cm = cm(mean(p$dx, na.rm = TRUE)), mean_dy_cm = cm(mean(p$dy, na.rm = TRUE)),
                  mean_dz_cm = cm(mean(p$dz, na.rm = TRUE)), rmse_x_cm = cm(rx), rmse_y_cm = cm(ry), rmse_h_cm = cm(ph), rmse_v_cm = cm(pv),
                  rmse_3d_cm = cm(sqrt(ph^2 + pv^2)), survey_rmse_h_cm = cm(sh), survey_rmse_v_cm = cm(sv))
  n <- max(nh, nv); notes <- character()
  if (n < 30) notes <- c(notes, sprintf("Only %d checkpoints: ASPRS Edition 2 asks for at least 30, so this accuracy is reported for information and not as a tested class.", n))
  if (n > 120) notes <- c(notes, "More than 120 checkpoints: Edition 2 caps the count at 120.")
  if (is.finite(rh) && sh > 0 && 2 * sh > rh) notes <- c(notes, "The checkpoints are not twice as accurate as the product horizontally.")
  if (is.finite(rv) && sv > 0 && 2 * sv > rv) notes <- c(notes, "The checkpoints are not twice as accurate as the product vertically.")
  # vertical bias: RTK-only blocks can carry a vertical offset or doming that RMSE alone hides (James & Robson 2014;
  # Stroner et al. 2020). The mean is a bias when it is more than twice its standard error (about 95 %).
  z <- p$dz[is.finite(p$dz)]; se <- if (length(z) >= 2) stats::sd(z) / sqrt(length(z)) else NA_real_
  bias <- is.finite(se) && se > 0 && abs(mean(z)) > 2 * se
  s[, `:=`(sd_dz_cm = if (length(z) >= 2) cm(stats::sd(z)) else NA_real_, vertical_bias = if (is.finite(se)) bias else NA)]
  if (bias) notes <- c(notes, sprintf("Vertical bias: the mean vertical error is %s cm, more than twice its standard error (%s cm), so the product sits %s the checkpoints. RTK-only blocks can carry a vertical offset or doming (James & Robson 2014; \u0160troner et al. 2020): add oblique images or at least one ground control point.",
                                      format(cm(mean(z))), format(cm(se)), if (mean(z) > 0) "above" else "below"))
  stmt <- sprintf("This data set was tested %s ASPRS Positional Accuracy Standards for Digital Geospatial Data, Edition 2 (2023), with %d checkpoints: RMSE_H %s cm, RMSE_V %s cm, RMSE_3D %s cm (the checkpoint survey's error included).",
                  if (n >= 30) "to meet the" else "against the", n, format(s$rmse_h_cm), format(s$rmse_v_cm), format(s$rmse_3d_cm))
  list(points = p[, .(id, dx_cm = cm(dx), dy_cm = cm(dy), dz_cm = cm(dz), dh_cm = cm(dh))], summary = s, statement = stmt, notes = notes)
}
