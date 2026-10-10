# Ovid/standards.R  --  Processing levels, quality-flag attributes and a NetCDF writer for standards-based exports.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Nothing here changes a value. It names what an exported value has been through (its processing level), says what its
# quality code means in the words other software reads (CF flag_values / flag_meanings), and writes CF-1.10 / ACDD-1.3
# NetCDF files without any package beyond base R (the classic format, 64-bit offsets, which every NetCDF library reads).

# ---- Processing levels -----------------------------------------------------------------------------------------------
# One scale for the whole suite, after the NASA data processing levels as adapted for drone data (Scientific Data FAIR
# framework for UAV data) and mapped to the USGS approval states. A value's level is the furthest step it has been through.
# Every level above 0 is provisional until someone reviews it and marks it approved.
PROC_LEVELS <- data.table(level = 0:4, code = paste0("L", 0:4),
  name = c("raw", "quality controlled", "corrected", "gap-filled", "derived"),
  meaning = c("as read from the instrument's files; nothing checked or changed",
              "checked and flagged; values that failed a rule may be blanked, none is changed",
              "changed by a correction (drift, calibration, clock, barometric compensation, undercatch)",
              "includes values estimated where none was recorded (interpolated or from another series)",
              "computed from other values (daily and annual statistics, indices, depths of thaw, gradients)"),
  drone = c("Level 0 raw data", "Level 1 basic calibrated data", "Level 2 corrected data", "Level 2 corrected data", "Level 3 derived products"))

# The level of one exported variable from its WMO codes (core/qc.R): an estimated value (6) makes it L3, a changed one (5)
# L2, anything else L1. derived = TRUE is L4 whatever the codes.
proc_level <- function(codes = integer(), derived = FALSE) {
  if (derived) return(4L); codes <- unique(codes[!is.na(codes)])
  if (6L %in% codes) 3L else if (5L %in% codes) 2L else 1L }

# "L2 corrected (provisional)", for a file's processing_level attribute and the readme.
proc_level_label <- function(level, approval = "provisional") paste0(PROC_LEVELS$code[match(level, PROC_LEVELS$level)], " ",
  PROC_LEVELS$name[match(level, PROC_LEVELS$level)], if (nzchar(approval)) paste0(" (", approval, ")") else "")

# A table of the levels for the readme of an export.
proc_levels_text <- function() c("Processing levels (one scale for every Ovid app):",
  paste0("  ", PROC_LEVELS$code, "  ", PROC_LEVELS$name, ": ", PROC_LEVELS$meaning, "."),
  "  Every level above L0 is provisional until reviewed.")

# ---- Quality flags in CF terms -----------------------------------------------------------------------------------------
# The attributes of a <variable>_qc variable holding WMO codes. CF asks that flag_values have the type of the variable
# (bytes here) and that flag_meanings be one blank-separated word per value.
cf_qc_atts <- function(of, standard_name = NULL) c(
  list(long_name = paste("quality control code of", of)),
  if (!is.null(standard_name) && nzchar(standard_name)) list(standard_name = paste(standard_name, "status_flag")),
  list(flag_values = nc_byte(WMO_QC$code), flag_meanings = paste(WMO_QC$name, collapse = " "),
       references = "WMO BUFR code table 0 33 020 (quality control indication of following value)"))

# ---- NetCDF (classic format, 64-bit offsets) -------------------------------------------------------------------------------
# A value tagged with the NetCDF type it is written as; untagged values go by their R type (character -> char,
# integer -> int, double -> double).
nc_byte <- function(x) structure(as.integer(x), nc_type = "byte")
nc_short <- function(x) structure(as.integer(x), nc_type = "short")
nc_float <- function(x) structure(as.numeric(x), nc_type = "float")
NC_TYPES <- c(byte = 1L, char = 2L, short = 3L, int = 4L, float = 5L, double = 6L)
NC_SIZE <- c(byte = 1L, char = 1L, short = 2L, int = 4L, float = 4L, double = 8L)
NC_FILL <- list(byte = -127L, char = 0L, short = -32767L, int = -2147483647L, float = 9.969209968386869e36, double = 9.969209968386869e36)

nc_type_of <- function(x) attr(x, "nc_type") %||% (if (is.character(x)) "char" else if (is.integer(x) || is.logical(x)) "int" else "double")

# The bytes of n values of a type, big-endian, padded with zeros to a multiple of 4.
nc_pad <- function(r) c(r, raw((4L - length(r) %% 4L) %% 4L))
nc_bin <- function(x, type) switch(type,
  char = { s <- if (length(x)) paste(x, collapse = "") else ""; charToRaw(enc2utf8(s)) },
  byte = writeBin(as.integer(x), raw(), size = 1L), short = writeBin(as.integer(x), raw(), size = 2L, endian = "big"),
  int = writeBin(as.integer(x), raw(), size = 4L, endian = "big"), float = writeBin(as.numeric(x), raw(), size = 4L, endian = "big"),
  double = writeBin(as.numeric(x), raw(), size = 8L, endian = "big"))
nc_i4 <- function(n) writeBin(as.integer(n), raw(), size = 4L, endian = "big")
nc_i8 <- function(n) { n <- as.numeric(n); c(nc_i4(n %/% 2^32), writeBin(as.integer(if (n %% 2^32 >= 2^31) n %% 2^32 - 2^32 else n %% 2^32), raw(), size = 4L, endian = "big")) }
nc_name <- function(s) { r <- charToRaw(enc2utf8(s)); c(nc_i4(length(r)), nc_pad(r)) }
nc_atts <- function(a, as_type = NULL) {
  a <- a[!vapply(a, is.null, logical(1))]
  if (!length(a)) return(c(nc_i4(0L), nc_i4(0L)))
  body <- lapply(names(a), function(k) { v <- a[[k]]
    ty <- if (!is.null(as_type) && k %in% c("_FillValue", "valid_min", "valid_max", "valid_range", "missing_value")) as_type else nc_type_of(v)
    if (ty == "char") v <- paste(as.character(v), collapse = "")
    b <- nc_bin(v, ty); c(nc_name(k), nc_i4(NC_TYPES[[ty]]), nc_i4(if (ty == "char") length(b) else length(v)), nc_pad(b)) })
  c(nc_i4(12L), nc_i4(length(a)), unlist(body))
}

# Write a NetCDF file.
#   dims  named integer vector of dimension lengths, in order (no unlimited dimension)
#   vars  list of variables, each list(name, dims = character(), data, atts = list(), type = optional). A character
#         variable's last dimension is its string length; its data is one string per row of the other dimensions.
#         Multi-dimensional data is given with the first dimension varying slowest (row-major, as NetCDF stores it); a
#         matrix with dims c("time", "depth") is time x depth. Missing values are written as _FillValue (the type's
#         default fill when there is none).
#   atts  global attributes
nc_write <- function(path, dims, vars, atts = list()) {
  dn <- names(dims)
  prep <- lapply(vars, function(v) {
    ty <- v$type %||% nc_type_of(v$data); vd <- v$dims %||% character()
    if (!all(vd %in% dn)) stop("variable ", v$name, " uses an unknown dimension")
    shape <- unname(dims[vd]); n <- if (length(shape)) prod(shape) else 1
    x <- v$data
    if (ty == "char") { L <- shape[length(shape)]; s <- as.character(x); s[is.na(s)] <- ""
      b <- unlist(lapply(s, function(z) { r <- charToRaw(enc2utf8(z)); r <- r[seq_len(min(length(r), L))]; c(r, raw(L - length(r))) }))
      if (length(b) != n) stop("variable ", v$name, ": ", length(s), " strings for ", n / L, " rows")
      data <- b
    } else {
      if (is.matrix(x)) x <- as.vector(t(x)) else x <- as.vector(x)
      if (length(x) != n) stop("variable ", v$name, ": ", length(x), " values for ", n, " places")
      a <- v$atts %||% list(); fill <- a[["_FillValue"]] %||% NC_FILL[[ty]]
      x[is.na(x)] <- fill
      data <- nc_bin(x, ty) }
    list(name = v$name, dimid = match(vd, dn) - 1L, atts = v$atts %||% list(), type = ty, data = nc_pad(data), vsize = length(nc_pad(data))) })
  head_of <- function(begins) c(charToRaw("CDF"), as.raw(2L), nc_i4(0L),
    if (length(dims)) c(nc_i4(10L), nc_i4(length(dims)), unlist(lapply(dn, function(k) c(nc_name(k), nc_i4(dims[[k]]))))) else c(nc_i4(0L), nc_i4(0L)),
    nc_atts(atts),
    if (length(prep)) c(nc_i4(11L), nc_i4(length(prep)), unlist(lapply(seq_along(prep), function(i) { p <- prep[[i]]
      c(nc_name(p$name), nc_i4(length(p$dimid)), if (length(p$dimid)) nc_i4(p$dimid), nc_atts(p$atts, p$type),
        nc_i4(NC_TYPES[[p$type]]), nc_i4(min(p$vsize, 2^31 - 1)), nc_i8(begins[i])) }))) else c(nc_i4(0L), nc_i4(0L)))
  h0 <- length(head_of(rep(0, length(prep))))
  begins <- h0 + cumsum(c(0, vapply(prep, `[[`, numeric(1), "vsize")))[seq_along(prep)]
  con <- file(path, "wb"); on.exit(close(con))
  writeBin(head_of(begins), con); for (p in prep) writeBin(p$data, con)
  invisible(path)
}

# Read a file written by nc_write() (or any classic or 64-bit-offset NetCDF without an unlimited dimension) back into
# list(dims, atts, vars), each variable as list(dims, atts, data) with fills turned back into NA. For tests and checks.
nc_read <- function(path) {
  r <- readBin(path, "raw", file.info(path)$size); p <- 1L
  i4 <- function() { v <- readBin(r[p:(p + 3L)], "integer", size = 4L, endian = "big"); p <<- p + 4L; v }
  i8 <- function() { hi <- i4(); lo <- i4(); hi * 2^32 + (if (lo < 0) lo + 2^32 else lo) }
  nm <- function() { n <- i4(); s <- if (n) rawToChar(r[p:(p + n - 1L)]) else ""; p <<- p + n + (4L - n %% 4L) %% 4L; s }
  vals <- function(ty, n, at) { tn <- names(NC_TYPES)[ty]; sz <- NC_SIZE[[tn]]; b <- if (n) r[at:(at + n * sz - 1L)] else raw()
    switch(tn, char = rawToChar(b[b != as.raw(0)]), byte = readBin(b, "integer", n, size = 1L, signed = TRUE),
      short = readBin(b, "integer", n, size = 2L, endian = "big"), int = readBin(b, "integer", n, size = 4L, endian = "big"),
      float = readBin(b, "numeric", n, size = 4L, endian = "big"), double = readBin(b, "numeric", n, size = 8L, endian = "big")) }
  att_list <- function() { tag <- i4(); n <- i4(); out <- list(); if (tag == 0L) return(out)
    for (k in seq_len(n)) { a <- nm(); ty <- i4(); m <- i4(); out[[a]] <- vals(ty, m, p)
      p <<- p + { b <- m * NC_SIZE[[names(NC_TYPES)[ty]]]; b + (4L - b %% 4L) %% 4L } }
    out }
  if (rawToChar(r[1:3]) != "CDF") stop("not a NetCDF classic file")
  ver <- as.integer(r[4]); p <- 5L; i4()
  tag <- i4(); nd <- i4(); dims <- integer()
  if (tag != 0L) for (k in seq_len(nd)) { a <- nm(); dims[a] <- i4() }
  atts <- att_list()
  tag <- i4(); nv <- i4(); vars <- list()
  if (tag != 0L) for (k in seq_len(nv)) { a <- nm(); m <- i4(); ids <- if (m) vapply(seq_len(m), function(j) i4(), 1L) else integer()
    va <- att_list(); ty <- i4(); i4(); at <- if (ver == 1L) i4() else i8()
    vd <- names(dims)[ids + 1L]; n <- if (length(vd)) prod(dims[vd]) else 1
    tn <- names(NC_TYPES)[ty]
    x <- if (tn == "char") { L <- dims[[vd[length(vd)]]]; vapply(seq_len(n / L), function(j) { b <- r[(at + (j - 1) * L + 1):(at + j * L)]; rawToChar(b[b != as.raw(0)]) }, "")
         } else vals(ty, n, at + 1)
    if (tn != "char") { fill <- va[["_FillValue"]] %||% NC_FILL[[tn]]; x[x == fill] <- NA }
    vars[[a]] <- list(dims = vd, atts = va, data = x) }
  list(dims = dims, atts = atts, vars = vars)
}

# ---- CF and ACDD attributes ------------------------------------------------------------------------------------------------
# Times as CF wants them: seconds since 1970-01-01 UTC. The apps keep times in one fixed offset from UTC (the "base", such
# as UTC-7) while labelling them UTC; offset_hours is that base, taken off so the file holds true UTC.
nc_time <- function(t, offset_hours = 0) as.numeric(t) - offset_hours * 3600
NC_TIME_ATTS <- list(standard_name = "time", long_name = "time", units = "seconds since 1970-01-01 00:00:00", calendar = "standard", axis = "T")
nc_iso <- function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# The global attributes of an Ovid export (ACDD 1.3 discovery attributes; the attributes left NULL are not written).
#   time        times in true UTC seconds (from nc_time())
#   lat, lon    decimal degrees (NA when unknown)
#   level       the file's processing level (the highest of its variables)
acdd_atts <- function(title, summary, id, time = numeric(), lat = NA_real_, lon = NA_real_, feature = "timeSeries", level = 1L,
                      keywords = NULL, source = NULL, history = NULL, extra = list()) {
  ok <- function(x) length(x) && any(is.finite(x))
  c(list(Conventions = "CF-1.10, ACDD-1.3", title = title, summary = summary, id = id, naming_authority = "Ovid",
         featureType = feature, cdm_data_type = c(timeSeries = "Station", timeSeriesProfile = "Station")[[feature]] %||% "Station",
         keywords = if (length(keywords)) paste(keywords, collapse = ", ") else NULL,
         processing_level = proc_level_label(level),
         product_version = if (exists("APP_VERSION")) paste0(if (exists("APP_NAME")) paste0(APP_NAME, " ") else "", APP_VERSION) else NULL,
         source = source, history = paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), " written by Ovid",
                                           if (exists("CORE_VERSION")) paste0(" core ", CORE_VERSION) else "", if (length(history)) paste0("; ", history) else ""),
         date_created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         standard_name_vocabulary = "CF Standard Name Table v84",
         time_coverage_start = if (ok(time)) nc_iso(min(time, na.rm = TRUE)) else NULL,
         time_coverage_end = if (ok(time)) nc_iso(max(time, na.rm = TRUE)) else NULL,
         geospatial_lat_min = if (ok(lat)) lat else NULL, geospatial_lat_max = if (ok(lat)) lat else NULL,
         geospatial_lon_min = if (ok(lon)) lon else NULL, geospatial_lon_max = if (ok(lon)) lon else NULL,
         geospatial_lat_units = if (ok(lat)) "degrees_north" else NULL, geospatial_lon_units = if (ok(lon)) "degrees_east" else NULL),
    extra)
}

# The station variables of a single-station file (CF discrete sampling geometry: one timeSeries, the station as scalars).
nc_station_vars <- function(name, lat = NA_real_, lon = NA_real_, strlen = 64L) {
  out <- list(list(name = "station_name", dims = "name_strlen", data = substr(name, 1, strlen), type = "char",
                   atts = list(long_name = "station name", cf_role = "timeseries_id")))
  if (is.finite(lat)) out[[2]] <- list(name = "lat", dims = character(), data = lat, atts = list(standard_name = "latitude", long_name = "latitude", units = "degrees_north"))
  if (is.finite(lon)) out[[length(out) + 1]] <- list(name = "lon", dims = character(), data = lon, atts = list(standard_name = "longitude", long_name = "longitude", units = "degrees_east"))
  out
}
