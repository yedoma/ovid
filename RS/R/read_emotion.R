# R/read_emotion.R  --  Reading senseFly eMotion 3 files without eMotion: black-box logs (.bbx) and mission files (.mis3, .syn3,
# .rtk3). Part of Ovid Core // RS. Sourced by rs_pipeline.R; not meant to be run on its own.
#
# .bbx (eBee / eBee Plus autopilot and thermoMAP camera logs) describe themselves: a schema record lists every block (GPS,
# attitude, photo tags, battery ...) with its fields, types and counts, and every later record is one block, framed as
#   1E 8A 9A 5E | block id (u16) | time since power-on (u48, microseconds) | payload | payload length (u16).
# A record counts only when the length after its payload matches the schema, so a damaged or cut file loses only the bad
# records.
# .bb3 (eBee Plus logs from about flight 20 on, and eMotion's own *_em.bb3 link logs) are framed and encrypted: see read_bb3.
# .mis3 / .syn3 / .rtk3 are JSON hidden under a fixed 512-byte XOR key. A mission holds the working area, take-off and
# landing, each block's area and planned waypoints, and the photo geotags eMotion kept for every flight (the same numbers
# as its _geoinfo.txt export). Readers only read: nothing is written next to the source files.

# ---- black-box logs (.bbx) --------------------------------------------------------------------------------------------
BBX_SYNC <- as.raw(c(0x1e, 0x8a, 0x9a, 0x5e))
# field type codes in the schema: bytes per value and how to read them (u = unsigned, i = signed, f = float)
BBX_TYPES <- data.table(type = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L), size = c(1L, 1L, 2L, 2L, 4L, 4L, 4L, 8L, 4L, 8L),
                        kind = c("char", "u", "i", "u", "i", "u", "i", "u", "f", "f"))
# blocks read by default: GPS fixes and UTC time, the flight state, photo tags, battery, wind, and the drone's identity
BBX_BLOCKS <- c("id_bb", "gps_identification", "gpsglue_bbTimeUTC", "gpsglue_bbGPSData", "sth_State_bb", "cam_PhotoTag_bb",
                "bat_bb", "wind_bb", "logbook_bb", "wa_Param_bb", "cam_id_bb", "cam_config_bb", "main_bb", "thm_temp_bb", "geotags_bb", "files_bb")

bbx_schema <- function(r, p = 1L) {
  u16 <- function(i) as.integer(r[i]) + 256L * as.integer(r[i + 1L])
  if (!identical(r[p:(p + 3L)], BBX_SYNC) || u16(p + 4L) != 65535L) stop("not a senseFly .bbx log (no schema at the start)")
  q <- p + 12L; nb <- u16(q); q <- q + 2L; out <- vector("list", nb)
  for (b in seq_len(nb)) {
    id <- u16(q); nl <- as.integer(r[q + 2L]); nm <- rawToChar(r[q + 2L + seq_len(nl)]); q <- q + 3L + nl
    nf <- u16(q); q <- q + 2L; f <- vector("list", nf)
    for (k in seq_len(nf)) {
      t <- as.integer(r[q]); nl <- as.integer(r[q + 1L]); fn <- rawToChar(r[q + 1L + seq_len(nl)]); q <- q + 2L + nl
      f[[k]] <- list(ord = k, field = fn, type = t, count = u16(q)); q <- q + 2L
    }
    out[[b]] <- data.table(block_id = id, block = nm, rbindlist(f))
  }
  s <- BBX_TYPES[rbindlist(out), on = "type"][order(block_id, ord)]
  s[, offset := cumsum(size * count) - size * count, by = block_id]
  list(schema = s, end = q + 2L)
}

# One block's records as a table: each field read for all records at once from the matrix of payload bytes.
bbx_block <- function(r, st, f) {
  L <- sum(f$size * f$count); n <- length(st)
  t_lo <- readBin(r[outer(0:3, st + 6L, "+")], "integer", n, 4L, endian = "little")
  t_hi <- readBin(r[outer(0:1, st + 10L, "+")], "integer", n, 2L, signed = FALSE, endian = "little")
  d <- data.table(t_us = ifelse(t_lo < 0, t_lo + 2^32, t_lo) + t_hi * 2^32)
  for (k in seq_len(nrow(f))) {
    sz <- f$size[k]; cnt <- f$count[k]; kind <- f$kind[k]; nm <- f$field[k]
    by <- r[outer(seq_len(sz * cnt) - 1L, st + 12L + f$offset[k], "+")]
    if (kind == "char" || (sz == 1L && cnt >= 8L)) {               # text, or byte arrays that hold text (serials, versions)
      if (cnt > 64L) next                                           # raw blobs (GNSS frames, routes) are left out
      m <- matrix(as.integer(by), nrow = cnt); m[apply(m == 0L, 2, cumsum) > 0] <- 0L   # a string ends at its first NUL
      txt_ok <- all(m == 0L | (m >= 32L & m <= 126L))
      if (kind == "char" || txt_ok) {
        m[m < 32L | m > 126L] <- 32L                                 # padding and stray bytes become spaces
        d[, (nm) := trimws(apply(m, 2, function(x) rawToChar(as.raw(x))))]; next
      }
      if (cnt > 16L) next
    }
    v <- if (kind == "f") readBin(by, "double", n * cnt, sz, endian = "little")
         else if (sz == 8L) { lo <- readBin(by[rep(c(TRUE, FALSE), each = 4L)], "integer", n * cnt, 4L, endian = "little")
                              hi <- readBin(by[rep(c(FALSE, TRUE), each = 4L)], "integer", n * cnt, 4L, endian = "little")
                              ifelse(lo < 0, lo + 2^32, lo) + ifelse(hi < 0, hi + 2^32, hi) * 2^32 }
         else if (sz == 4L) { x <- readBin(by, "integer", n * cnt, 4L, endian = "little"); if (kind == "u") ifelse(x < 0, x + 2^32, x) else x }
         else readBin(by, "integer", n * cnt, sz, signed = kind == "i", endian = "little")
    if (cnt == 1L) d[, (nm) := v] else for (j in seq_len(cnt)) d[, (paste0(nm, "_", j)) := v[seq(j, length(v), by = cnt)]]
  }
  d
}

# read_bbx(path, blocks): blocks = names to decode (NULL = every block in the schema). Returns
#   meta   one row: drone name / serial / firmware, GNSS receiver, records read and skipped, first UTC time
#   schema every block and field the log declares
#   blocks a list of tables, one per decoded block, each with t_us (microseconds since power-on)
#   track  GPS fixes with UTC time (from gpsglue_bbGPSData, time of week tied to UTC by gpsglue_bbTimeUTC)
#   photos the camera's photo tags with UTC time, position, attitude and accuracy (cam_PhotoTag_bb)
read_bbx <- function(path, blocks = BBX_BLOCKS) {
  r <- readBin(path, "raw", file.size(path)); n <- length(r)
  sc <- bbx_schema(r); s <- sc$schema; bsz <- s[, .(L = sum(size * count)), by = .(block_id, block)]
  # candidate records: the sync bytes, a known block id and a length trailer that agrees with the schema
  c0 <- which(r[seq_len(n - 13L)] == BBX_SYNC[1]); c0 <- c0[c0 >= sc$end]
  c0 <- c0[r[c0 + 1L] == BBX_SYNC[2] & r[c0 + 2L] == BBX_SYNC[3] & r[c0 + 3L] == BBX_SYNC[4]]
  id <- as.integer(r[c0 + 4L]) + 256L * as.integer(r[c0 + 5L])
  L <- bsz$L[match(id, bsz$block_id)]; ok <- !is.na(L) & c0 + 13L + L <= n; c0 <- c0[ok]; id <- id[ok]; L <- L[ok]
  tr <- as.integer(r[c0 + 12L + L]) + 256L * as.integer(r[c0 + 13L + L]); ok <- tr == L; c0 <- c0[ok]; id <- id[ok]; L <- L[ok]
  # drop matches that fall inside an accepted record (sync bytes that happen to occur in a payload)
  keep <- logical(length(c0)); nxt <- 0
  for (i in seq_along(c0)) if (c0[i] >= nxt) { keep[i] <- TRUE; nxt <- c0[i] + 14L + L[i] }
  c0 <- c0[keep]; id <- id[keep]
  want <- if (is.null(blocks)) unique(s$block) else intersect(blocks, s$block)
  out <- setNames(lapply(want, function(b) { f <- s[block == b]; st <- c0[id == f$block_id[1]]
                                             if (length(st)) bbx_block(r, st, f) else NULL }), want)
  out <- out[!vapply(out, is.null, logical(1))]
  track <- photos <- data.table(); utc0 <- NA_real_
  if (!is.null(u <- out$gpsglue_bbTimeUTC) && nrow(u <- u[valid > 0 & year > 2000])) {
    utc <- as.numeric(ISOdatetime(u$year, u$month, u$day, u$hour, u$min, u$sec, tz = "UTC")) + round(u$nano / 1e9, 3)
    utc0 <- stats::median(utc - u$itow / 1000)                     # start of the GPS week, in UTC
  }
  if (!is.null(g <- out$gpsglue_bbGPSData)) {
    track <- g[lat != 0 | lon != 0][, utc := as.POSIXct(utc0 + tow / 1000, origin = "1970-01-01", tz = "UTC")]
    setcolorder(track, c("utc", "t_us", "lat", "lon", "altWGS84", "alt"))
  }
  if (!is.null(p <- out$cam_PhotoTag_bb)) {
    photos <- copy(p)[, utc := as.POSIXct(ISOdatetime(utcYear, utcMonth, utcDay, utcHour, utcMin, 0, tz = "UTC") + utcSec)]
    setcolorder(photos, c("id", "utc", "latitude", "longitude", "altitudeWgs84", "altitudeMsl"))
  }
  g1 <- function(b, f) if (!is.null(out[[b]]) && f %in% names(out[[b]]) && nrow(out[[b]])) out[[b]][[f]][1] else NA
  meta <- data.table(file = basename(path), drone = g1("id_bb", "name"), drone_sn = as.character(if (is.na(g1("id_bb", "sn"))) g1("id_bb", "droneSn") else g1("id_bb", "sn")),
                     firmware = as.character(g1("id_bb", "firmwareVersion")), gnss_sn = g1("gps_identification", "sn"),
                     camera = g1("cam_id_bb", "versionName"), records = length(c0), blocks_declared = uniqueN(s$block),
                     start_utc = if (nrow(track)) min(track$utc) else if (nrow(photos)) min(photos$utc) else as.POSIXct(NA),
                     gps_fixes = nrow(track), photo_tags = nrow(photos))
  list(meta = meta, schema = s[, .(block_id, block, field, type, count)], blocks = out, track = track, photos = photos)
}

# ---- mission files (.mis3, .syn3, .rtk3) ------------------------------------------------------------------------------
EMOTION_KEY <- paste0(
  "dd01de120702b3459c06208e087bebe407c6dd5da4bc25d5bb7e760c2b29f9acb9ea8833466b4f8190f8180b7eb928a8058d1373ee01276ac67691448277133c",
  "d0060b3da93a7f95ce68093f51e1e42170526b907252774a429bcb0664ec6bf678709d77e19f56bd1792f6dcbf830d9e0240c29d6c98531630dc1c6b4b6b026c",
  "4655c52d0676f9bdbff7f9c1639d085315e1b307f97061d0be8a79c222a16436160acd599be5b817f0236517666219df74278691b1e3bc851e126fbce340ac47",
  "71f6e818aa15bad136889d2ef084a9897364de2399a44d5e2682505ed6bc94318ebe8009d75c05a571985f9e8bfb905117c360d764d6d1e59dd012a4b5b563f6",
  "2cf3a61f23cea7f44c43dce8e349545f68de6bd5e6ffac58b77415777558b1fd089dfe920db92bd93694b1f7d91b07d4d0d039a84c70a6432e0103ad1d93c35c",
  "42c23fcb736b5c9ee20b52b3930aa663e8bb05537ec929149c849799211329af9a3ea86169154ac6f474da4e78a51096ab90e49cc30626eceb984dc2dd134270",
  "601c86f9143d6b0bb95452804f305ffc8428d65d3cecb76aa5a2df0ea0c3687e4acf4bd721121477ccd7127d2104fa6bdca8cef53d394798ec00db705cded183",
  "9188c252ad248f4cc5440dd1fedcd00d5af0741e8bca6eceaf5e8201f84c513758d592a294d8b5bad6f47204c92eba5e7e1dc41130b827dc170a6453a668fc28")

emotion_decrypt <- function(path) {
  k <- as.raw(strtoi(substring(EMOTION_KEY, seq(1, 1023, 2), seq(2, 1024, 2)), 16L))
  r <- readBin(path, "raw", file.size(path)); if (!length(r)) return("")
  rawToChar(xor(r, rep_len(k, length(r))))
}

# eMotion's photo geotags: 336 bytes per photo, base64 in the mission and the same bytes as the drone's photo-tag record in
# its .bb3 log. Offsets found by matching the flight's _geoinfo.txt. geotag_table() takes one photo per column.
geotag_table <- function(m) {
  n <- ncol(m); if (!n) return(data.table())
  rd <- function(o, sz, what = "double", ...) readBin(as.vector(m[o + seq_len(sz), , drop = FALSE]), what, n, sz, endian = "little", ...)
  data.table(photo = rd(280L, 4L, "integer"), lon = rd(0L, 8L), lat = rd(8L, 8L), alt_msl = rd(16L, 4L), alt_wgs84 = rd(20L, 4L),
             acc_xy = rd(72L, 4L), acc_z = rd(76L, 4L), pitch = rd(288L, 4L), yaw = rd(292L, 4L), roll = rd(296L, 4L),
             utc = as.POSIXct(ISOdatetime(rd(300L, 2L, "integer", signed = FALSE), as.integer(m[303L, ]), as.integer(m[304L, ]),
                                          as.integer(m[305L, ]), as.integer(m[306L, ]), 0, tz = "UTC") + rd(306L, 4L)))
}
emotion_geotags <- function(g) {
  b <- lapply(g, geotag_bytes); b <- b[lengths(b) == 336L]
  if (!length(b)) return(data.table())
  geotag_table(do.call(cbind, b))[order(photo)]
}

# read_emotion_plan(path): returns kind (mission / sync / rtk), name, meta (working area, take-off, home), waypoints (each
# planned waypoint with its block, task and route), areas (each mapping block's polygon), geotags (photo positions eMotion
# kept per flight, named by the flight's log file) and json (the full decoded file).
read_emotion_plan <- function(path) {
  txt <- emotion_decrypt(path)
  # safe: stops on the next line with a message
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE)$root, error = function(e) NULL)
  if (is.null(j)) stop("not an eMotion 3 file, or a different key: ", basename(path))
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  wp_rows <- function(wps, ...) rbindlist(lapply(wps, function(w) { w <- w$waypoint %||% w; p <- w$position
    data.table(..., wpt_id = num(w$id), wpt = w$name %||% "", lat = num(p$lat), lon = num(p$lon), alt = num(p$alt), alt_type = num(p$altType)) }))
  tasks_wps <- function(blk, part) rbindlist(lapply(blk, function(b) { mb <- b$missionBlock
    rbindlist(lapply(mb$tasks, function(t) { t <- t$missionTask; rt <- t$sfv1$route %||% t$sfx1$route
      if (is.null(rt) || !length(rt$waypoints)) return(NULL)
      wp_rows(rt$waypoints, part = part, block = mb$description %||% mb$name, task = t$name, log_file = t$owner$logFile %||% "") })) }))
  if (!is.null(j$mission)) {
    m <- j$mission; wa <- m$parameters$workingArea
    pos <- function(b, k) { p <- b[[1]]$missionBlock[[k]]; list(lat = num(p$lat), lon = num(p$lon)) }
    to <- if (length(m$takeOffs)) m$takeOffs[[1]]$missionBlock$takeOffBlock else NULL
    hm <- if (length(m$landings)) m$landings[[1]]$missionBlock$landingBlock$homeWaypoint$waypoint$position else NULL
    meta <- data.table(name = m$name %||% "", version = num(j$version), blocks = length(m$blocks),
                       area_lat = num(wa$center$lat), area_lon = num(wa$center$lon), area_radius_m = num(wa$radius), area_ceiling_m = num(wa$ceiling),
                       takeoff_lat = num(to$takeOffPosition$lat), takeoff_lon = num(to$takeOffPosition$lon),
                       takeoff_alt_wgs84 = if (length(m$takeOffs)) num(m$takeOffs[[1]]$missionBlock$takeOffAltitudeWgs84) else NA_real_,
                       home_lat = num(hm$lat), home_lon = num(hm$lon), home_alt = num(hm$alt), place = m$weather$cityName %||% "")
    wps <- rbindlist(list(tasks_wps(m$takeOffs, "takeoff"), tasks_wps(m$blocks, "block"), tasks_wps(m$landings, "landing")), fill = TRUE)
    areas <- rbindlist(lapply(m$blocks, function(b) { mb <- b$missionBlock; poly <- mb$horizontalMappingBlock$polygon %||% mb$corridorMappingBlock$polygon
      if (!length(poly)) return(NULL)
      data.table(block = mb$description %||% mb$name, kind = mb$name, vertex = seq_along(poly),
                 lat = vapply(poly, function(v) num(v$lat), 0), lon = vapply(poly, function(v) num(v$lon), 0)) }))
    geo <- rbindlist(lapply(m$photoFootprints, function(pf) rbindlist(lapply(pf$flightFootprints, function(ff) {
      g <- emotion_geotags(ff$geotags); if (nrow(g)) g[, `:=`(drone = pf$robotName %||% "", log_file = ff$bbxName %||% "")]; g }))))
    if (nrow(geo)) setcolorder(geo, c("drone", "log_file"))
    return(list(kind = "mission", name = meta$name, meta = meta, waypoints = wps, areas = areas, geotags = geo, json = j))
  }
  if (!is.null(j$waypoints) || !is.null(j$routes)) {                # .syn3: eMotion's local ids against the ids on the drone
    ids <- function(x) rbindlist(lapply(x, as.data.table), fill = TRUE)
    return(list(kind = "sync", name = sub("\\.[^.]+$", "", basename(path)),
                meta = data.table(routes = length(j$routes), tasks = length(j$tasks), waypoints = length(j$waypoints)),
                waypoints = ids(j$waypoints), routes = ids(j$routes), tasks = ids(j$tasks), areas = data.table(), geotags = data.table(), json = j))
  }
  list(kind = if (!is.null(j$rtk)) "rtk" else "other", name = sub("\\.[^.]+$", "", basename(path)),
       meta = as.data.table(lapply(unlist(j), identity)), waypoints = data.table(), areas = data.table(), geotags = data.table(), json = j)
}

# ---- encrypted black-box logs (.bb3) -----------------------------------------------------------------------------------
# A .bb3 is a stream of frames  CE CD | flags | time since power-on (u48, microseconds) | payload | CRC-32 (little endian),
# where CF C0 / C1 / C2 / C3 inside a frame stand for the bytes CF / CD / CE / 2B. Frames with flags 0x83 are the drone's
# records; the others are radio-link messages. A record's payload is XORed with a window of one fixed keystream: a ring of
# 4093 blocks of 16 bytes, the same in every flight, entered two bytes before a block boundary. The first two payload bytes
# are the record type, so each record's window is found by trying every entry point and keeping the one whose type agrees
# with the other records of that length; the rest of the payload then reads in clear.
# The ring is never shipped or kept in the repository: build_bb3_ring() rebuilds it on each computer from the user's own
# mission (.mis3), whose photo geotags are the plain text of the matching .bb3 photo records, into the project's config
# folder (<project>/config/emotion_bb3.ring, outside the repository), where read_bb3() looks for it.
BB3_ESC <- as.raw(c(0xcf, 0xcd, 0xce, 0x2b))                  # what CF C0, CF C1, CF C2, CF C3 stand for
# record layouts found by matching photo tags (offsets from the start of the payload, type bytes included)
BB3_PHOTO <- c(0x03, 0x01)                                    # 338 bytes: type + eMotion's 336-byte geotag
BB3_STATE <- c(0x07, 0x01)                                    # 148 bytes: alt MSL f32 @2, alt WGS84 f32 @7, lon f64 @116, lat f64 @124
BB3_GPS   <- c(0x05, 0x01)                                    # 540-604 bytes: lon f64 @2, lat f64 @10, alt MSL f32 @89, alt WGS84 f32 @94,
                                                              #   UTC year u16 @224, month, day, hour, minute u8, seconds f32 @230
BB3_ID    <- c(0x14, 0x02)                                    # firmware, serial and receiver text
# State, battery and wind: found by matching value ranges and behaviour to the .bbx fields of the same drone (sth_State_bb,
# bat_bb, wind_bb) and cross-checked within the log (course against the flown track's bearing, roll against the roll command).
# The values are f32 at the given payload offsets, each followed by a status byte.
BB3_STATE_F <- c(alt_msl = 2L, alt_wgs84 = 7L, height = 12L, height_agl = 17L, roll = 27L, pitch = 32L, heading = 37L,
                 airspeed = 72L, ground_speed = 86L, course = 111L)                                   # in BB3_STATE (148 bytes)
BB3_BATT  <- c(0x07, 0x02)                                    # 124 bytes
BB3_BATT_F <- c(voltage_lpf = 77L, voltage = 82L, current_a = 87L, used_mah = 92L, remaining_mah = 97L, wind_speed = 40L, wind_dir = 45L)
BB3_WIND  <- c(0x06, 0x05)                                    # 31 bytes: wind estimate
BB3_WIND_F <- c(wind_speed = 3L, wind_dir = 11L)

# A geotag's 336 bytes, or none when the mission holds it empty or as something else (one such geotag stopped the whole key build).
geotag_bytes <- function(x) if (is.character(x$bytes) && length(x$bytes) == 1L && nzchar(x$bytes)) jsonlite::base64_dec(x$bytes) else raw()
BB3_NO_KEY <- paste("No .bb3 key in this project yet: build it once with build_bb3_ring() from an eMotion mission (.mis3)",
                    "that holds photo geotags, together with the .bb3 logs of those flights.")
bb3_ring_path <- function(project_dir = if (exists("CFG")) CFG$project_dir else "") {
  o <- getOption("ovid.emotion_ring"); if (!is.null(o)) return(o)
  if (nzchar(project_dir %||% "")) file.path(path.expand(project_dir), "config", "emotion_bb3.ring") else NA_character_
}

# CRC-32 of many byte runs at once, in 16-bit halves (R integers are 32-bit signed): starts a and lengths n in u.
crc32_runs <- function(u, a, n) {
  tl <- th <- integer(256)
  for (i in 0:255) { lo <- i; hi <- 0L
    for (k in 1:8) { odd <- bitwAnd(lo, 1L) == 1L
      lo <- bitwOr(bitwShiftR(lo, 1L), bitwShiftL(bitwAnd(hi, 1L), 15L)); hi <- bitwShiftR(hi, 1L)
      if (odd) { lo <- bitwXor(lo, 0x8320L); hi <- bitwXor(hi, 0xEDB8L) } }
    tl[i + 1L] <- lo; th[i + 1L] <- hi }
  o <- order(n, decreasing = TRUE); a <- a[o]; n <- n[o]; lo <- hi <- rep(0xFFFFL, length(a))
  for (p in seq_len(max(c(0L, n))) - 1L) {
    m <- sum(n > p); if (!m) break; s <- seq_len(m)
    x <- bitwAnd(bitwXor(lo[s], as.integer(u[a[s] + p])), 255L) + 1L
    lo[s] <- bitwXor(bitwOr(bitwShiftR(lo[s], 8L), bitwShiftL(bitwAnd(hi[s], 255L), 8L)), tl[x]); hi[s] <- bitwXor(bitwShiftR(hi[s], 8L), th[x])
  }
  r <- cbind(lo = bitwXor(lo, 0xFFFFL), hi = bitwXor(hi, 0xFFFFL)); r[order(o), , drop = FALSE]
}

# The frames of a .bb3: the unescaped bytes u, each frame's first byte (flags) and length (CRC included), and its CRC check.
bb3_frames <- function(path, check_crc = TRUE) {
  r <- readBin(path, "raw", file.size(path))
  sy <- grepRaw(as.raw(c(0xce, 0xcd)), r, fixed = TRUE, all = TRUE)
  es <- lapply(0:3, function(k) grepRaw(as.raw(c(0xcf, 0xc0 + k)), r, fixed = TRUE, all = TRUE))
  for (k in 1:4) r[es[[k]]] <- BB3_ESC[k]
  del <- sort(c(sy, sy + 1L, unlist(es) + 1L))
  newpos <- function(p) p - findInterval(p - 1L, del)          # where an original byte position lands once del is removed
  u <- if (length(del)) r[-del] else r; rm(r)
  a <- newpos(sy + 2L); e <- c(newpos(sy[-1L]) - 1L, length(u)); n <- e - a + 1L
  ok <- n >= 11L
  if (check_crc && any(ok)) {
    cr <- crc32_runs(u, a[ok], n[ok] - 4L); ee <- e[ok]
    ok[ok] <- cr[, "lo"] == as.integer(u[ee - 3L]) + 256L * as.integer(u[ee - 2L]) & cr[, "hi"] == as.integer(u[ee - 1L]) + 256L * as.integer(u[ee])
  }
  list(u = u, a = a[ok], n = n[ok], flags = as.integer(u[a[ok]]), frames = length(sy), bad = sum(!ok))
}

bb3_ring <- function(ring) {
  R <- if (is.raw(ring)) ring else readBin(ring, "raw", file.size(ring)); N <- length(R)
  if (!N || N %% 16L) stop("not an eMotion .bb3 keystream ring: ", if (is.raw(ring)) "raw" else ring)
  NB <- N %/% 16L; s <- (16L * (seq_len(NB) - 1L) - 2L) %% N   # 0-based entry point of each block
  RR <- c(R, R[seq_len(min(N, 4096L))])
  key <- as.integer(RR[s + 1L]) * 256L + as.integer(RR[s + 2L]); o <- order(key)
  cnt <- tabulate(key + 1L, 65536L)
  list(R = R, RR = RR, N = N, NB = NB, s = s, k0 = as.integer(RR[s + 1L]), k1 = as.integer(RR[s + 2L]),
       js = o, first = cumsum(c(1L, cnt))[seq_len(65536L)], cnt = cnt)
}

# Decrypted payloads of records (payload starts p, length L, ring block j), one record per column.
bb3_plain <- function(u, rg, p, L, j) {
  if (!length(p)) return(matrix(raw(0), L, 0L))
  matrix(xor(u[outer(seq_len(L) - 1L, p, "+")], rg$RR[outer(seq_len(L), rg$s[j], "+")]), nrow = L)
}

# Find every record's ring block. types: optional table (L, type) of 2-byte types per payload length; learnt when NULL.
# 1. a type is a header value that most records of one length decrypt to, at some entry point;
# 2. a record whose type fits at exactly one entry point is placed;
# 3. the entry point runs with the power-on clock: block = time %/% 16 + d (mod ring), with d one of a few dozen values in a
#    file. The d of the placed records settle records with several fits and find the ones whose type was not learnt.
bb3_locate <- function(fr, rg, types = NULL, sample = 200L, min_share = 0.2) {
  u <- fr$u; k <- fr$flags == 0x83L; NB <- rg$NB
  rec <- data.table(a = fr$a[k], n = fr$n[k])[, `:=`(p = a + 7L, L = n - 11L)][L >= 2L]
  rec[, `:=`(rid = .I, c0 = as.integer(u[p]), c1 = as.integer(u[p + 1L]))]
  rec[, t_us := readBin(u[outer(1:4, a, "+")], "integer", .N, 4L, endian = "little") %% 2^32 +
                 (as.integer(u[a + 5L]) + 256 * as.integer(u[a + 6L])) * 2^32]
  rec[, tb := as.integer((t_us %/% 16) %% NB)]
  hdr <- function(c0, c1, j) bitwXor(c0, rg$k0[j]) * 256L + bitwXor(c1, rg$k1[j])
  if (is.null(types)) {
    types <- rec[, if (.N >= 20L) {                            # rarer lengths are left to the clock-offset pass below
      ix <- if (.N > sample) sample.int(.N, sample) else seq_len(.N)
      h <- bitwXor(rep(c0[ix], each = NB), rg$k0) * 256L + bitwXor(rep(c1[ix], each = NB), rg$k1)
      tb <- unique(data.table(r = rep(seq_along(ix), each = NB), h = h))[, .N, by = h]
      tb[N >= max(5L, min_share * length(ix)), .(type = h)] }, by = L]
  }
  if (is.null(types) || !nrow(types)) types <- data.table(L = integer(), type = integer())
  if (!nrow(rec)) return(list(rec = rec[, `:=`(j = integer(), type = integer())], types = types, offsets = integer()))
  cand <- types[rec, on = "L", allow.cartesian = TRUE, nomatch = NULL]
  cand[, key := bitwXor(c0, bitwShiftR(type, 8L)) * 256L + bitwXor(c1, bitwAnd(type, 255L))][, nc := rg$cnt[key + 1L]]
  cand <- cand[nc > 0L][rep(seq_len(.N), nc)][, j := rg$js[rg$first[key + 1L] + seq_len(.N) - 1L], by = .(rid, type)]
  cand[, nr := .N, by = rid]
  one <- cand[nr == 1L, .(rid, j, type)]
  # the clock offsets d seen in the placed records (kept when they are more than chance)
  dd <- rec[one, on = "rid"][, .N, by = .(d = (j - 1L - tb) %% NB)]
  dset <- dd[N >= max(5L, 0.002 * nrow(one)), d]
  if (length(dset)) {
    one <- rec[one, on = "rid"][(j - 1L - tb) %% NB %in% dset, .(rid, j, type)]   # a single fit off every known offset is chance
    # settle records with several fits: keep the fits on a known offset
    many <- rec[cand[nr > 1L, .(rid, j, type)], on = "rid"][(j - 1L - tb) %% NB %in% dset][, nfit := .N, by = rid][nfit == 1L]   # columns kept when none settle
    one <- rbind(one, many[, .(rid, j, type)])
    # records still unplaced: try each known offset; a header that recurs across records of a length is a new type
    left <- rec[!rid %in% one$rid]
    if (nrow(left)) {
      tr <- left[, .(d = dset), by = .(rid, L, c0, c1, tb)][, j := (tb + d) %% NB + 1L][, h := hdr(c0, c1, j)]
      nl <- left[, .N, by = L]
      newt <- tr[, .(n = uniqueN(rid)), by = .(L, h)][nl, on = "L"][n >= pmax(3L, min_share * N), .(L, type = h)]
      newt <- newt[!types, on = .(L, type)]
      types <- rbind(types, newt)
      hit <- tr[types, on = .(L, h = type), nomatch = NULL][, nfit := .N, by = rid][nfit == 1L]
      one <- rbind(one, hit[, .(rid, j, type = h)])
    }
  }
  rec[, `:=`(j = NA_integer_, type = NA_integer_)][one, on = "rid", `:=`(j = i.j, type = i.type)]
  list(rec = rec, types = types, offsets = dset)
}

# read_bb3(path, ring): returns
#   meta    one row: file, frames, records, decrypted share, start UTC, drone text found in the identity records
#   photos  the photo tags (same columns as a mission's geotags), with t_us and the UTC eMotion stored
#   track   flight state at about 10 Hz: t_us, utc, lat, lon, alt_msl, alt_wgs84, height (above take-off), height_agl,
#           roll, pitch, heading, airspeed, ground_speed, course (degrees, m, m/s)
#   battery voltage (filtered and raw, V), current (A), charge used and remaining (mAh), and the wind estimate, about 1 Hz
#   wind    the wind estimate (speed m/s, direction degrees)
#   gps     GNSS fixes with their own UTC time and position
#   records every drone record: time, length, type and its ring block (NA when it could not be placed)
# UTC for the track is the GPS clock tied to the power-on clock (the offset is constant to about a millisecond).
read_bb3 <- function(path, ring = bb3_ring_path(), types = NULL, check_crc = TRUE) {
  fr <- bb3_frames(path, check_crc)
  if (!is.raw(ring) && (is.na(ring) || !file.exists(ring))) {
    msg <- BB3_NO_KEY; message(msg)
    return(list(meta = data.table(file = basename(path), frames = fr$frames, bad_frames = fr$bad, records = sum(fr$flags == 0x83L),
                                  decrypted_pct = 0, note = msg), photos = data.table(), track = data.table(), gps = data.table(), records = data.table()))
  }
  rg <- bb3_ring(ring); lc <- bb3_locate(fr, rg, types); rec <- lc$rec; u <- fr$u
  pick <- function(ty, len = NULL) { x <- rec[!is.na(j) & type == ty[1] * 256L + ty[2]]; if (!is.null(len)) x <- x[L %in% len]; x[order(t_us)] }
  f32 <- function(m, o) readBin(as.vector(m[o + 1:4, , drop = FALSE]), "double", ncol(m), 4L, endian = "little")
  f64 <- function(m, o) readBin(as.vector(m[o + 1:8, , drop = FALSE]), "double", ncol(m), 8L, endian = "little")
  g <- pick(BB3_GPS)[L >= 234L]; gps <- data.table()
  if (nrow(g)) {
    m <- bb3_plain(u, rg, g$p, 234L, g$j)                       # the fixed part of every GNSS record
    gps <- data.table(t_us = g$t_us, lon = f64(m, 2L), lat = f64(m, 10L), alt_msl = f32(m, 89L), alt_wgs84 = f32(m, 94L),
                      utc = as.numeric(ISOdatetime(readBin(as.vector(m[225:226, , drop = FALSE]), "integer", ncol(m), 2L, signed = FALSE, endian = "little"),
                                                   as.integer(m[227L, ]), as.integer(m[228L, ]), as.integer(m[229L, ]), as.integer(m[230L, ]), 0, tz = "UTC")) + f32(m, 230L))
  }
  off <- if (nrow(gps) && any(ok <- is.finite(gps$utc) & gps$utc > 1e9)) stats::median(gps$utc[ok] - gps$t_us[ok] / 1e6) else NA_real_
  ph <- pick(BB3_PHOTO, 338L); photos <- data.table()
  if (nrow(ph)) photos <- geotag_table(bb3_plain(u, rg, ph$p, 338L, ph$j)[-(1:2), , drop = FALSE])[, t_us := ph$t_us][order(photo)]
  if (!is.finite(off) && nrow(photos) && any(ok <- is.finite(photos$utc) & as.numeric(photos$utc) > 1e9))   # older firmware: no UTC in the GNSS record
    off <- stats::median(as.numeric(photos$utc[ok]) - photos$t_us[ok] / 1e6)
  utc_of <- function(t) as.POSIXct(off + t / 1e6, origin = "1970-01-01", tz = "UTC")
  if (nrow(gps)) gps <- gps[is.finite(utc) & utc > 1e9 & (lat != 0 | lon != 0)][, utc := as.POSIXct(utc, origin = "1970-01-01", tz = "UTC")]
  fields <- function(ty, len, fl) { x <- pick(ty, len); if (!nrow(x)) return(data.table())
    m <- bb3_plain(u, rg, x$p, len, x$j); d <- data.table(t_us = x$t_us, utc = utc_of(x$t_us))
    for (k in names(fl)) set(d, j = k, value = f32(m, fl[[k]]))
    if ("wind_dir" %in% names(d)) d[, wind_dir := wind_dir %% 360]
    d }
  battery <- fields(BB3_BATT, 124L, BB3_BATT_F)
  if (nrow(battery)) battery <- battery[voltage > 0]
  wind <- fields(BB3_WIND, 31L, BB3_WIND_F)
  if (nrow(wind)) wind <- wind[wind_speed > 0]
  s <- pick(BB3_STATE, 148L); track <- data.table()
  if (nrow(s)) { m <- bb3_plain(u, rg, s$p, 148L, s$j)
    track <- data.table(t_us = s$t_us, utc = utc_of(s$t_us), lat = f64(m, 124L), lon = f64(m, 116L))
    for (k in names(BB3_STATE_F)) set(track, j = k, value = f32(m, BB3_STATE_F[[k]]))
    track[, `:=`(heading = heading %% 360, course = course %% 360)]
    track <- track[is.finite(lat) & is.finite(lon) & (lat != 0 | lon != 0) & abs(lat) <= 90 & abs(lon) <= 180] }
  id <- pick(BB3_ID); txt <- character()
  if (nrow(id)) for (i in seq_len(min(nrow(id), 4L))) { b <- bb3_plain(u, rg, id$p[i], id$L[i], id$j[i])[-(1:2)]
    txt <- c(txt, regmatches(rawToChar(b[b != as.raw(0) & b < as.raw(127)]), gregexpr("[ -~]{5,}", rawToChar(b[b != as.raw(0) & b < as.raw(127)])))[[1]]) }
  meta <- data.table(file = basename(path), frames = fr$frames, bad_frames = fr$bad, records = nrow(rec),
                     decrypted_pct = if (nrow(rec)) round(100 * mean(!is.na(rec$j)), 2) else NA_real_, start_utc = if (is.finite(off)) utc_of(min(rec$t_us)) else as.POSIXct(NA),
                     photos = nrow(photos), track_points = nrow(track), gps_fixes = nrow(gps), id_text = paste(unique(txt), collapse = " | "))
  list(meta = meta, photos = photos, track = track, gps = gps, battery = battery, wind = wind,
       records = rec[, .(t_us, L, type = ifelse(is.na(type), NA_character_, sprintf("%04x", type)), j)], types = lc$types, clock_offsets = lc$offsets)
}

# build_bb3_ring(mis3_paths, bb3_paths, out): rebuilds the keystream from the user's own data. A mission keeps each flight's
# photo geotags under the name of its .bb3 log; where the log holds exactly as many photo records, each record XOR its
# geotag gives 21 keystream blocks and the order they follow. Blocks from all flights are chained; the ring is complete when
# the chain closes on itself. Returns the ring (raw, NULL if not closed) with the blocks found and any contradictions, and
# writes it to out when given.
build_bb3_ring <- function(mis3_paths, bb3_paths, out = bb3_ring_path(), check_crc = FALSE) {
  geo <- list()
  for (mp in mis3_paths) { j <- read_emotion_plan(mp)$json$mission
    for (pf in j$photoFootprints) for (ff in pf$flightFootprints) if (length(ff$geotags)) {
      b <- lapply(ff$geotags, geotag_bytes); b <- do.call(cbind, b[lengths(b) == 336L])
      if (is.null(b) || !ncol(b)) next                              # no usable geotag in this flight
      geo[[ff$bbxName]] <- b[, order(readBin(as.vector(b[281:284, , drop = FALSE]), "integer", ncol(b), 4L, endian = "little")), drop = FALSE] } }
  used <- data.table(); edges <- list()
  for (bp in bb3_paths) {
    g <- geo[[basename(bp)]]; if (is.null(g)) next
    fr <- bb3_frames(bp, check_crc); k <- fr$flags == 0x83L & fr$n == 349L
    a <- fr$a[k]; t <- readBin(fr$u[outer(1:4, a, "+")], "integer", length(a), 4L, endian = "little") %% 2^32 +
                       (as.integer(fr$u[a + 5L]) + 256 * as.integer(fr$u[a + 6L])) * 2^32
    a <- a[order(t)]
    used <- rbind(used, data.table(file = basename(bp), photo_records = length(a), geotags = ncol(g), used = length(a) == ncol(g)))
    if (length(a) != ncol(g)) next
    c <- matrix(fr$u[outer(7:344, a, "+")], nrow = 338L)         # 338 payload bytes per photo record
    ks <- matrix(xor(c[-(1:2), , drop = FALSE], g), nrow = 336L) # keystream for payload bytes 3..338 = 21 whole blocks
    bl <- apply(ks, 2, function(x) apply(matrix(x, 16L), 2, function(b) paste(as.character(b), collapse = "")))
    edges[[length(edges) + 1L]] <- data.table(from = as.vector(bl[-21L, ]), to = as.vector(bl[-1L, ]))
  }
  if (!length(edges)) return(list(ring = NULL, flights = used, blocks = 0L, conflicts = 0L, closed = FALSE))
  e <- unique(rbindlist(edges)); conflicts <- sum(duplicated(e$from)); e <- e[!duplicated(from)]
  blocks <- unique(c(e$from, e$to)); nx <- setNames(e$to, e$from)
  cur <- min(blocks); seq_b <- character(); closed <- FALSE
  for (i in seq_along(blocks)) { seq_b <- c(seq_b, cur); cur <- nx[cur]; if (is.na(cur)) break; if (cur == seq_b[1]) { closed <- TRUE; break } }
  closed <- closed && length(seq_b) == length(blocks)
  ring <- if (closed) as.raw(strtoi(substring(paste(seq_b, collapse = ""), seq(1, 32 * length(seq_b), 2), seq(2, 32 * length(seq_b), 2)), 16L)) else NULL
  if (closed && length(out) && !is.na(out)) { dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE); writeBin(ring, out) }
  list(ring = ring, file = if (closed) out else NA_character_, flights = used, blocks = length(blocks), conflicts = conflicts, closed = closed)
}
