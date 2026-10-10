# R/exif.R  --  Reading what Ovid needs from the start of a JPG: the EXIF tags every camera writes (make, model, time,
# serial, GPS position) and the XMP packet DJI and senseFly add. Part of Ovid Core // RS; sourced by rs_pipeline.R.
#
# Only the first HEAD_BYTES of each file are read (the metadata sits at the start; DJI's XMP packet about 40 kB in), so a
# large archive on a slow drive is read at a few hundred images a second. No package is needed: the TIFF structure inside
# the EXIF segment is walked by hand.

HEAD_BYTES <- 65536L

# The bytes of a file's head, grown once when a tag seems to be further in.
file_head <- function(path, n = HEAD_BYTES) tryCatch(readBin(path, "raw", n), error = function(e) raw())

u16 <- function(b, i, le) { if (i + 1L > length(b)) return(NA_integer_); v <- as.integer(b[i:(i + 1L)]); if (le) v[1] + 256L * v[2] else v[2] + 256L * v[1] }
u32 <- function(b, i, le) { if (i + 3L > length(b)) return(NA_real_); v <- as.numeric(b[i:(i + 3L)]); if (le) sum(v * 256^(0:3)) else sum(v * 256^(3:0)) }

EXIF_TAGS <- c(`271` = "make", `272` = "model", `306` = "datetime", `36867` = "datetime_original", `37521` = "subsec", `42033` = "body_serial",
               `42036` = "lens_model", `42016` = "image_unique_id",
               `37386` = "focal_length", `41486` = "fp_xres", `41488` = "fp_unit", `40962` = "px_x", `40963` = "px_y",   # for the ground sample distance
               `33434` = "exposure", `33437` = "f_number", `34855` = "iso")                                                 # exposure, for motion blur
GPS_TAGS  <- c(`1` = "lat_ref", `2` = "lat", `3` = "lon_ref", `4` = "lon", `5` = "alt_ref", `6` = "alt", `7` = "gps_time", `29` = "gps_date", `9` = "gps_status")

# One IFD: a named list of the tags asked for (ASCII as text, rationals as numbers), plus the pointers to the Exif and GPS IFDs.
read_ifd <- function(b, off, le, tiff0, want) {
  n <- u16(b, tiff0 + off, le); if (is.na(n) || n > 500) return(list())
  out <- list()
  for (k in seq_len(n)) {
    e <- tiff0 + off + 2L + 12L * (k - 1L)
    tag <- u16(b, e, le); typ <- u16(b, e + 2L, le); cnt <- u32(b, e + 4L, le)
    if (is.na(tag) || is.na(typ) || is.na(cnt)) break
    size <- c(1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8)[typ]; if (is.na(size)) next
    len <- size * cnt
    pos <- if (len <= 4) e + 8L else tiff0 + u32(b, e + 8L, le)
    if (tag %in% c(34665L, 34853L)) { out[[if (tag == 34665L) ".exif" else ".gps"]] <- u32(b, e + 8L, le); next }
    nm <- want[as.character(tag)]; if (is.na(nm)) next
    if (is.na(pos) || pos + len - 1L > length(b) || pos < 1L) { out[[nm]] <- NA; next }
    v <- b[pos:(pos + len - 1L)]
    out[[nm]] <- if (typ == 2L) { v <- v[v != as.raw(0)]; s <- rawToChar(v); Encoding(s) <- "latin1"; trimws(enc2utf8(s)) }
                 else if (typ == 5L || typ == 10L) vapply(seq_len(cnt), function(j) { a <- u32(v, 8L * (j - 1L) + 1L, le); d <- u32(v, 8L * (j - 1L) + 5L, le); if (is.na(d) || d == 0) NA_real_ else a / d }, 0)
                 else if (typ == 1L || typ == 7L) as.integer(v) else if (typ == 3L) vapply(seq_len(cnt), function(j) u16(v, 2L * (j - 1L) + 1L, le), 0L)
                 else if (typ == 4L) vapply(seq_len(cnt), function(j) u32(v, 4L * (j - 1L) + 1L, le), 0) else NA
  }
  out
}

# EXIF from a JPG's (or TIFF's) head: camera make, model, serial, the time the picture was taken, and the GPS tags, as a one-row list.
read_exif <- function(b) {
  z <- list(make = NA_character_, model = NA_character_, body_serial = NA_character_, datetime_original = NA_character_, subsec = NA_character_,
            lat = NA_real_, lon = NA_real_, alt = NA_real_, gps_time = NA_character_, gps_date = NA_character_,
            focal_mm = NA_real_, fp_xres = NA_real_, fp_unit = NA_real_, px_x = NA_real_, px_y = NA_real_, exposure_s = NA_real_, f_number = NA_real_, iso = NA_real_)
  if (length(b) < 12L) return(z)
  tiff <- identical(b[1:4], as.raw(c(0x49, 0x49, 0x2A, 0x00))) || identical(b[1:4], as.raw(c(0x4D, 0x4D, 0x00, 0x2A)))   # a TIFF (Sequoia bands)
  if (tiff) { t0 <- 1L; le <- b[1] == as.raw(0x49) } else {
  if (b[1] != as.raw(0xFF) || b[2] != as.raw(0xD8)) return(z)
  i <- 3L
  while (i + 3L <= length(b) && b[i] == as.raw(0xFF)) {               # walk the segments until APP1 Exif
    marker <- as.integer(b[i + 1L]); len <- u16(b, i + 2L, FALSE); if (is.na(len)) return(z)
    if (marker == 0xE1L && i + 9L <= length(b) && rawToChar(b[(i + 4L):(i + 7L)]) == "Exif") break
    if (marker == 0xDAL) return(z)
    i <- i + 2L + len
  }
  if (i + 9L > length(b)) return(z)
  t0 <- i + 10L; le <- b[t0] == as.raw(0x49) }
  ifd0 <- read_ifd(b, u32(b, t0 + 4L, le), le, t0, EXIF_TAGS)
  ex <- if (!is.null(ifd0$.exif)) read_ifd(b, ifd0$.exif, le, t0, EXIF_TAGS) else list()
  gp <- if (!is.null(ifd0$.gps)) read_ifd(b, ifd0$.gps, le, t0, GPS_TAGS) else list()
  dms <- function(v, ref) if (is.null(v) || length(v) < 3 || anyNA(v)) NA_real_ else (v[1] + v[2] / 60 + v[3] / 3600) * (if (!is.null(ref) && ref %in% c("S", "W")) -1 else 1)
  z$make <- ifd0$make %||% NA_character_; z$model <- ifd0$model %||% NA_character_
  z$body_serial <- ex$body_serial %||% NA_character_; z$datetime_original <- ex$datetime_original %||% ifd0$datetime %||% NA_character_
  z$subsec <- ex$subsec %||% NA_character_
  z$lat <- dms(gp$lat, gp$lat_ref); z$lon <- dms(gp$lon, gp$lon_ref)
  z$alt <- if (is.null(gp$alt) || is.na(gp$alt[1])) NA_real_ else gp$alt[1] * (if (!is.null(gp$alt_ref) && gp$alt_ref[1] == 1L) -1 else 1)
  z$gps_time <- if (is.null(gp$gps_time) || anyNA(gp$gps_time)) NA_character_ else sprintf("%02d:%02d:%06.3f", as.integer(gp$gps_time[1]), as.integer(gp$gps_time[2]), gp$gps_time[3])
  z$gps_date <- gp$gps_date %||% NA_character_
  num1 <- function(v) if (is.null(v) || !length(v) || is.na(v[1])) NA_real_ else as.numeric(v[1])
  z$focal_mm <- num1(ex$focal_length); z$fp_xres <- num1(ex$fp_xres); z$fp_unit <- num1(ex$fp_unit)
  z$px_x <- num1(ex$px_x); z$px_y <- num1(ex$px_y); z$exposure_s <- num1(ex$exposure); z$f_number <- num1(ex$f_number); z$iso <- num1(ex$iso)
  z
}

# The XMP packet, if the head holds one (DJI about 40 kB in, senseFly right after the EXIF).
xmp_text <- function(b) {
  i <- grepRaw("<x:xmpmeta", b, fixed = TRUE); if (!length(i)) return(NA_character_)
  j <- grepRaw("</x:xmpmeta>", b, offset = i, fixed = TRUE); if (!length(j)) j <- length(b) - 11L
  v <- b[i:min(length(b), j + 11L)]; v[v == as.raw(0)] <- as.raw(32)
  s <- rawToChar(v); Encoding(s) <- "UTF-8"; s
}

xmp_tag <- function(x, tag) {
  m <- regmatches(x, regexec(paste0(tag, "(=\"([^\"]*)\"|>([^<]*)<)"), x))
  trim(vapply(m, function(v) if (length(v) == 4) paste0(v[3], v[4]) else NA_character_, ""))
}
