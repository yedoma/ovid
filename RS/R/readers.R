# R/readers.R  --  Telling what each file is, and reading images (DJI, senseFly, Parrot Sequoia, Canon), DJI MRK and RPT
# files, eMotion geoinfo files, KML/KMZ tracks and plans, and RINEX headers. Part of Ovid Core // RS. Sourced by rs_pipeline.R; not meant to be run on its own.
#
# Each format is one entry in FORMATS: its family (aircraft or source), role, and a match on the file name. Readers
# only read: they return facts with where they came from and never decide which flight a file belongs to (catalogue.R does).

FORMATS <- list(
  list(format = "image_jpg",       family = "camera",  role = "image",         pattern = "\\.jpe?g$"),
  list(format = "image_tif",       family = "camera",  role = "image",         pattern = "^(IMG|DJI|DSC|MAX|R0)[0-9]*_.*\\.tiff?$"),   # camera TIFFs (Sequoia bands)
  list(format = "sat_imd",         family = "dataset", role = "dataset_meta",  pattern = "\\.imd$"),
  list(format = "spot_dimap",      family = "dataset", role = "dataset_meta",  pattern = "^metadata\\.dim$"),
  list(format = "image_raw",       family = "camera",  role = "image_raw",     pattern = "\\.(cr2|cr3|dng|nef|arw)$"),
  list(format = "dji_mrk",         family = "dji",     role = "flight_record", pattern = "_Timestamp\\.MRK$|\\.MRK$"),
  list(format = "dji_rpt",         family = "dji",     role = "flight_record", pattern = "^DJI_.*\\.RPT$"),
  list(format = "dji_gnss_raw",    family = "dji",     role = "gnss_obs",      pattern = "^DJI_.*\\.(RTK|RTB|RTL|RTS)$"),
  list(format = "dji_lidar_raw",   family = "dji",     role = "lidar_raw",     pattern = "^DJI_.*\\.(LDR|LDRT|CLC|CLI|DBG|IMU|RPOS)$"),
  list(format = "dji_sig",         family = "dji",     role = "flight_record", pattern = "^DJI_.*\\.SIG$"),
  list(format = "video",           family = "camera",  role = "video",         pattern = "\\.(mp4|mov)$"),
  list(format = "video_srt",       family = "dji",     role = "flight_record", pattern = "\\.srt$"),
  list(format = "sensefly_log",    family = "ebee",    role = "flight_log",    pattern = "\\.(bb3|bbx|bbz)$"),
  list(format = "emotion_geoinfo", family = "ebee",    role = "geotags",       pattern = "_geoinfo\\.txt$"),
  list(format = "emotion_plan",    family = "ebee",    role = "mission",       pattern = "\\.(mis3|rtk3|syn3)$"),
  list(format = "pix4d_project",   family = "pix4d",   role = "mission",       pattern = "\\.p4d$"),
  list(format = "metashape",       family = "other",   role = "processed",     pattern = "\\.psx$"),
  list(format = "kml",             family = "other",   role = "mission",       pattern = "\\.(kmz|kml|wpml)$"),
  list(format = "mission",         family = "other",   role = "mission",       pattern = "\\.plan$"),
  list(format = "rinex",           family = "gnss",    role = "gnss_obs",      pattern = "\\.([0-9]{2}[ognmdONGMD]|obs|nav|rnx|m00)$"),
  list(format = "gnss_solution",   family = "gnss",    role = "gnss_obs",      pattern = "\\.(pos|llh|ubx)$"),
  list(format = "gis",             family = "other",   role = "gis",           pattern = "\\.(shp|geojson|gpx|gpkg)$"),
  list(format = "lidar_tile",      family = "dataset", role = "lidar_tile",    pattern = "\\.(las|laz)$"),     # UAV point clouds sit in processed folders: out of scope
  list(format = "point_cloud",     family = "other",   role = "processed",     pattern = "\\.ply$"),
  list(format = "processed_ortho", family = "other",   role = "processed",     pattern = "(orthomosaic|transparent_mosaic|_dsm|_dtm|^result)[^/]*\\.(tiff?|ecw|jp2)$"),
  list(format = "raster",          family = "dataset", role = "raster",        pattern = "\\.(tiff?|pix|img|ecw|jp2|grd|sid|ntf)$"),   # satellite, aerial, LiDAR rasters
  list(format = "document",        family = "other",   role = "notes",         pattern = "\\.(pdf|docx?|txt|xlsx?|csv|rtf|md|log)$"),
  list(format = "archive",         family = "other",   role = "archive",       pattern = "\\.(zip|7z|rar)$"))
FORMAT_INFO <- rbindlist(lapply(FORMATS, function(f) f[c("format", "family", "role")]))

# By name only, so a large archive is listed without opening every file; images are opened later, once, to read their metadata.
format_of <- function(file) {
  out <- rep("unrecognised", length(file))
  for (f in rev(FORMATS)) out[grepl(f$pattern, file, ignore.case = TRUE)] <- f$format
  out
}

# ---- images ----------------------------------------------------------------------------------------------------------
# One row per image from its EXIF and XMP: camera, serial, aircraft, time, position, and what the positioning was worth.
#   DJI: XMP drone-dji tags (aircraft model and serial, UTC exposure time, RTK flag 50 fixed / 34 float / 16 single, and
#        the RTK standard deviations). senseFly S.O.D.A.: EXIF time is UTC; XMP Camera:GPSXYAccuracy is the geotag's
#        accuracy in m (about 0.02 when corrected, metres when not). Canon S110 on the eBee: no GPS, a clock reading 1980.
DJI_TAGS <- c(drone_model = "drone-dji:DroneModel", drone_serial = "drone-dji:DroneSerialNumber", product = "drone-dji:ProductName",
              camera_serial = "drone-dji:CameraSerialNumber", utc = "drone-dji:UTCAtExposure", rtk_flag = "drone-dji:RtkFlag",
              rtk_std_lat = "drone-dji:RtkStdLat", rtk_std_lon = "drone-dji:RtkStdLon", rtk_std_hgt = "drone-dji:RtkStdHgt",
              alt_rel = "drone-dji:RelativeAltitude", gimbal_pitch = "drone-dji:GimbalPitchDegree",
              speed_x = "drone-dji:FlightXSpeed", speed_y = "drone-dji:FlightYSpeed", speed_z = "drone-dji:FlightZSpeed")
SF_TAGS  <- c(acc_xy = "Camera:GPSXYAccuracy", acc_z = "Camera:GPSZAccuracy", cam_id = "sensefly:CamId",
              capture_uuid = "Camera:CaptureUUID", flight_uuid = "Camera:FlightUUID", rig = "Camera:RigName")

# DJI cameras that name their aircraft (older DJI images carry no drone-dji:DroneModel). The Phantom 3 and 4 and 4 Pro
# have no RTK, so their positions are standalone GNSS; the Phantom 4 RTK writes RTK flags like the M350.
DJI_CAMERAS <- c(FC300X = "Phantom 3 Professional", FC300S = "Phantom 3 Advanced", FC300C = "Phantom 3 Standard", FC330 = "Phantom 4",
                 FC6310 = "Phantom 4 Pro", FC6310S = "Phantom 4 Pro V2", FC6310R = "Phantom 4 RTK", FC6360 = "Phantom 4 Multispectral")
# Sensor widths (mm) of the cameras in the archive, for images whose EXIF has no focal plane resolution. From the makers'
# specifications: 1-inch sensors 13.2 mm, 1/2.3-inch 6.17 mm, 1/1.7-inch 7.44 mm, 4/3-inch 17.3 mm (the Zenmuse L2's RGB
# camera, EXIF model "L2"), full frame 35.9 mm.
SENSOR_WIDTH_MM <- c(FC300X = 6.17, FC300S = 6.17, FC300C = 6.17, FC330 = 6.17, FC6310 = 13.2, FC6310S = 13.2, FC6310R = 13.2, L2 = 17.3,
                     ZenmuseP1 = 35.9, `S.O.D.A.` = 13.2, `Canon PowerShot S110` = 7.44)
NO_RTK_AIRCRAFT <- c("Phantom 3 Professional", "Phantom 3 Advanced", "Phantom 3 Standard", "Phantom 4", "Phantom 4 Pro", "Phantom 4 Pro V2")

read_image_meta <- function(paths) {
  rows <- lapply(paths, function(p) {
    b <- file_head(p); e <- read_exif(b); x <- xmp_text(b)
    if (is.na(x) && length(b) == HEAD_BYTES && !is.na(e$make) && e$make == "DJI") { b <- file_head(p, 4L * HEAD_BYTES); x <- xmp_text(b) }
    tags <- c(DJI_TAGS, SF_TAGS)
    d <- c(e, setNames(as.list(vapply(tags, function(t) xmp_tag(x, t), "")), names(tags)))
    d$has_xmp <- !is.na(x)
    d$file_size <- file.size(p); d$head_md5 <- if (is.na(e$datetime_original)) md5_raw(b) else NA_character_   # for the photo key (copies)
    d$drone_xmp <- !is.na(x) && grepl("drone-dji:|sensefly:|(^|[^A-Za-z])Camera:", x)   # DJI, senseFly, Sequoia / MicaSense (not GCamera:)
    d$irradiance <- !is.na(x) && grepl("Irradiance", x, fixed = TRUE)          # a downwelling light sensor (DLS, sunshine sensor) wrote its reading
    d$band <- xmp_tag(x, "Camera:BandName"); d
  })
  d <- rbindlist(rows, fill = TRUE)
  for (k in c("rtk_flag", "rtk_std_lat", "rtk_std_lon", "rtk_std_hgt", "alt_rel", "gimbal_pitch", "acc_xy", "acc_z", "speed_x", "speed_y", "speed_z")) set(d, j = k, value = suppressWarnings(as.numeric(d[[k]])))
  # sensor width from the EXIF focal plane resolution (pixels per inch, cm or mm), else from the table of known cameras
  unit_mm <- c(`2` = 25.4, `3` = 10, `4` = 1)
  d[, sensor_w_mm := round(fifelse(!is.na(fp_xres) & fp_xres > 0 & !is.na(px_x) & as.character(fp_unit) %in% names(unit_mm), px_x / fp_xres * unit_mm[as.character(fp_unit)], NA_real_), 3)]
  d[, sensor_w_source := fifelse(!is.na(sensor_w_mm), "EXIF", "")]
  k <- which(is.na(d$sensor_w_mm) & !is.na(d$model) & d$model %in% names(SENSOR_WIDTH_MM))
  if (length(k)) set(d, i = k, j = c("sensor_w_mm", "sensor_w_source"), value = list(unname(SENSOR_WIDTH_MM[d$model[k]]), "camera table"))
  d[, c("fp_xres", "fp_unit") := NULL]
  d[, sensor := fifelse(!is.na(product) & nzchar(product), product,
                fifelse(!is.na(model), trimws(paste(fifelse(is.na(make) | startsWith(model, make), "", make), model)), NA_character_))]
  d[, camera_serial := fifelse(!is.na(camera_serial) & nzchar(camera_serial), camera_serial, body_serial)]
  # the photo's identity, to count copies once (catalogue/duplicates.csv): size, camera serial, exposure time to the subsecond and
  # band; without an EXIF time, size and the md5 of the first 64 kB (the eMotion logs' key uses the same hash)
  bl <- function(x) fifelse(is.na(x), "", as.character(x))
  d[, photo_key := fifelse(!is.na(datetime_original), paste("photo", file_size, bl(camera_serial), paste0(datetime_original, fifelse(is.na(subsec) | !nzchar(bl(subsec)), "", paste0(".", subsec))), bl(band), sep = "|"),
                           paste("photo", file_size, paste0("md5:", head_md5), sep = "|"))][, c("file_size", "head_md5") := NULL]
  d[, drone_model := fifelse(!is.na(drone_model) & nzchar(drone_model), drone_model, unname(DJI_CAMERAS[model]))]
  d[!is.na(rig) & rig == "Sequoia", sensor := "Parrot Sequoia"]
  # exposure time in UTC: DJI gives it; senseFly's and Sequoia's EXIF clocks are UTC (GPS-set); other cameras: local clock, zone unknown
  t_exif <- as.POSIXct(datetime_original <- d$datetime_original, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
  d[, time_exif := t_exif]
  d[, time_utc := fifelse(!is.na(utc) & nzchar(utc), as.POSIXct(sub("T", " ", substr(utc, 1, 19)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                  fifelse((!is.na(make) & make %in% c("senseFly", "Parrot")) | (!is.na(rig) & rig == "Sequoia"), t_exif, as.POSIXct(NA)))]
  d[, clock_ok := !is.na(t_exif) & t_exif > as.POSIXct("2000-01-01", tz = "UTC")]
  d[, c("utc", "product", "body_serial", "datetime_original", "subsec", "gps_time", "gps_date", "rig", "cam_id") := NULL][]
}

md5_raw <- function(b) { f <- tempfile(); on.exit(unlink(f)); writeBin(b, f); unname(tools::md5sum(f)) }

# Why each photo is left out ("" = a drone photo): a row of config/drone_cameras.csv with drone "no" leaves it out (DJI's handheld
# cameras write drone tags too); else a drone XMP tag, or a make and model matching a row (make in any case, model a prefix, a
# blank model any model of that make), keeps it.
drone_why <- function(make, model, xmp, cams) {
  lo <- function(x) tolower(trimws(fifelse(is.na(x), "", as.character(x))))
  mk <- lo(make); md <- lo(model); hit <- xmp %in% TRUE; no <- rep(FALSE, length(mk))
  if (!is.null(cams) && nrow(cams)) { cm <- lo(cams$make); cd <- lo(cams$model); dr <- if ("drone" %in% names(cams)) lo(cams$drone) else rep("", nrow(cams))
    for (j in which(nzchar(cm))) { m <- mk == cm[j] & (!nzchar(cd[j]) | startsWith(md, cd[j])); if (dr[j] %in% c("no", "n", "false")) no <- no | m else hit <- hit | m } }
  fifelse(no, "a handheld or other camera marked not a drone in drone_cameras.csv", fifelse(hit, "",
          fifelse(!nzchar(mk) & !nzchar(md), "no camera in the EXIF and no drone tag", "camera not in drone_cameras.csv and no drone tag")))
}

# ---- DJI _Timestamp.MRK -------------------------------------------------------------------------------------------
# One line per exposure, tab separated: shot, GPS seconds of week, [GPS week], N/E/V offsets (mm), "lat,Lat", "lon,Lon",
# "height,Ellh", three standard deviations (m) and "flag,Q" (50 fixed, 34 float, 16 single).
read_mrk <- function(path) {
  L <- read_lines_any(path); L <- L[grepl(",Lat", L, fixed = TRUE)]
  g <- function(re) suppressWarnings(as.numeric(ifelse(grepl(re, L), sub(re, "\\1", L), NA)))
  d <- data.table(shot = as.integer(sub("^\\s*([0-9]+).*", "\\1", L)), lat = g(".*\\s(-?[0-9.]+),Lat.*"),
                  lon = g(".*\\s(-?[0-9.]+),Lon.*"), h_ellps = g(".*\\s(-?[0-9.]+),Ellh.*"), q = g(".*\\s([0-9]+),Q.*"))
  sow <- suppressWarnings(as.numeric(sub("^\\s*[0-9]+\\s+([0-9.]+).*", "\\1", L))); wk <- suppressWarnings(as.numeric(sub(".*\\[([0-9]{4})\\].*", "\\1", L)))
  t0 <- if (length(L) && is.finite(wk[1]) && is.finite(sow[1])) format(as.POSIXct("1980-01-06", tz = "UTC") + wk[1] * 604800 + sow[1] - 18, "%Y-%m-%d %H:%M:%S") else NA_character_   # GPS time, 18 s ahead of UTC
  ok <- is.finite(d$lat) & is.finite(d$lon) & (d$lat != 0 | d$lon != 0); r <- function(f, v) if (any(ok)) f(v[ok]) else NA_real_
  list(mrk_shots = nrow(d), mrk_fixed_pct = if (nrow(d)) round(100 * mean(d$q == 50, na.rm = TRUE), 1) else NA_real_,
       mrk_start_utc = t0, mrk_lat_min = r(min, d$lat), mrk_lat_max = r(max, d$lat), mrk_lon_min = r(min, d$lon), mrk_lon_max = r(max, d$lon))
}

# ---- DJI RPT surveying report (JSON) --------------------------------------------------------------------------------
# Mission id, start time, flight time and the camera's own count of RTK fixed / float / single exposures.
read_rpt <- function(path) {
  j <- jsonlite::fromJSON(paste(read_lines_any(path), collapse = "\n"), simplifyVector = FALSE)$SURVEYING_REPORT_ROOT
  b <- j$BASIC_INFO_UNIT %||% list(); v <- j$VISIBLE_CAM_INFO_UNIT %||% list()
  n <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  list(mission_id = n(b$MISSION_ID), mission_start_utc = if (is.null(b$MISSION_START_TIME)) NA_character_ else
         fmt_ts(as.POSIXct(as.numeric(b$MISSION_START_TIME), origin = "1970-01-01", tz = "UTC")),
       fly_time_s = n(b$FLY_TIME), rpt_captures = n(v$TOTAL_CAP_NUM), rpt_fixed = n(v$RTK_FIXED_NUM), rpt_float = n(v$RTK_FLOAT_NUM),
       rpt_single = n(v$RTK_SINGLE_NUM))
}

# ---- Pix4D project (.p4d, XML; eMotion writes one per flight): the image names it lists tie it to its flight ----
read_p4d <- function(path) {
  L <- read_lines_any(path); m <- regmatches(L, regexpr('<image [^>]*path="[^"]+"', L))
  n <- basename(gsub("\\\\", "/", sub('.*path="([^"]+)"', "\\1", m)))
  # a saved project also holds each image's position and time; a new one, its processing area
  num <- function(re) suppressWarnings(as.numeric(sub(re, "\\1", grep(re, L, value = TRUE))))
  la <- c(num('.*<gps [^>]*lat="(-?[0-9.]+)".*'), num('.*<geoCoord2D [^>]*lat="(-?[0-9.]+)".*'))
  lo <- c(num('.*<gps [^>]*lng="(-?[0-9.]+)".*'), num('.*<geoCoord2D [^>]*lng="(-?[0-9.]+)".*'))
  tt <- as.POSIXct(sub(".*<time>([0-9: ]+)</time>.*", "\\1", grep("<time>", L, value = TRUE)), format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
  r <- function(f, v) { v <- v[is.finite(v) & v != 0]; if (length(v)) f(v) else NA_real_ }
  list(p4d_images = length(n), p4d_files = paste(n, collapse = ";"),
       p4d_lat_min = r(min, la), p4d_lat_max = r(max, la), p4d_lon_min = r(min, lo), p4d_lon_max = r(max, lo),
       p4d_start_utc = if (any(!is.na(tt))) format(min(tt, na.rm = TRUE), "%Y-%m-%d %H:%M:%S") else NA_character_)
}

# ---- eMotion geoinfo.txt ----------------------------------------------------------------------------------------------
# The geotags eMotion wrote for a flight (sensefly/geoinfo/3): fileName, latitude, longitude, altitude_wgs84, Accuracy XY, Accuracy Z.
# A centimetre accuracy here is the corrected position; metres means the geotags were not corrected.
read_geoinfo <- function(path) {
  L <- read_lines_any(path); L <- L[!startsWith(L, "#")]
  d <- tryCatch(fread(text = L, sep = "\t", header = TRUE, fill = TRUE, colClasses = "character"), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(list(geo_images = 0L, geo_acc_xy_median_m = NA_real_))
  n <- function(k) suppressWarnings(as.numeric(d[[k]]))
  acc <- n(grep("Accuracy ?XY", names(d), value = TRUE)[1])
  list(geo_images = nrow(d), geo_acc_xy_median_m = round(stats::median(acc, na.rm = TRUE), 3),
       geo_lat_min = min(n("latitude"), na.rm = TRUE), geo_lat_max = max(n("latitude"), na.rm = TRUE),
       geo_lon_min = min(n("longitude"), na.rm = TRUE), geo_lon_max = max(n("longitude"), na.rm = TRUE),
       geo_files = paste(d$fileName, collapse = ";"))
}

# ---- KML / KMZ / DJI wpml ---------------------------------------------------------------------------------------------------
# What a KML holds: a flown track (gx:Track with times, as eMotion exports), a planned route or area (LineString, Polygon,
# DJI waylines with no times), and named points (eMotion lists each image). Kept: the kind, the extent, the image names and
# the line thinned to KML_POINTS vertices for the map. KMZ is unzipped to a temporary folder; nothing is written in the source.
KML_POINTS <- 300L
read_kml <- function(path) {
  txt <- if (grepl("\\.kmz$", path, ignore.case = TRUE)) {
    td <- tempfile("kmz"); on.exit(unlink(td, TRUE)); f <- utils::unzip(path, exdir = td)
    f <- f[grepl("\\.(kml|wpml)$", f, ignore.case = TRUE)]; f <- c(f[grepl("waylines", f)], f)
    if (!length(f)) return(list(kml_kind = "empty")); read_lines_any(f[1])
  } else read_lines_any(path)
  x <- paste(txt, collapse = "\n")
  num <- function(v) suppressWarnings(as.numeric(v))
  tr <- regmatches(x, gregexpr("<gx:coord>[^<]*</gx:coord>", x))[[1]]
  if (length(tr)) { kind <- "observed"; co <- gsub("</?gx:coord>", "", tr) }
  else { kind <- if (grepl("<(LineString|Polygon|wpml:|Folder)", x)) "planned" else "points"
    blk <- regmatches(x, gregexpr("(?s)<(LineString|LinearRing)>.*?</(LineString|LinearRing)>", x, perl = TRUE))[[1]]
    if (!length(blk)) blk <- regmatches(x, gregexpr("(?s)<Point>.*?</Point>", x, perl = TRUE))[[1]]
    co <- unlist(strsplit(trimws(gsub("(?s).*<coordinates>|</coordinates>.*", "", blk, perl = TRUE)), "\\s+")); co <- gsub(",", " ", co) }
  m <- do.call(rbind, lapply(strsplit(trimws(co), " +"), function(v) num(v[1:2])))
  if (is.null(m)) return(list(kml_kind = "empty"))
  m <- m[is.finite(m[, 1]) & is.finite(m[, 2]) & !(m[, 1] == 0 & m[, 2] == 0), , drop = FALSE]
  if (!nrow(m)) return(list(kml_kind = "empty"))
  k <- unique(round(seq(1, nrow(m), length.out = min(nrow(m), KML_POINTS))))
  nm <- gsub("</?name>", "", regmatches(x, gregexpr("<name>[^<]*\\.(jpe?g|tiff?)</name>", x, ignore.case = TRUE))[[1]])
  wt <- regmatches(x, gregexpr("<when>[^<]*</when>", x))[[1]]
  wt <- as.POSIXct(sub("Z$", "", gsub("</?when>|T", " ", wt[c(1, length(wt))])), tz = "UTC")
  list(kml_kind = kind, kml_vertices = nrow(m), kml_lon_min = min(m[, 1]), kml_lon_max = max(m[, 1]), kml_lat_min = min(m[, 2]), kml_lat_max = max(m[, 2]),
       kml_start_utc = if (length(wt)) fmt_ts(wt[1]) else NA_character_, kml_end_utc = if (length(wt)) fmt_ts(wt[2]) else NA_character_,
       kml_files = paste(nm, collapse = ";"), kml_line = paste(sprintf("%.6f %.6f", m[k, 2], m[k, 1]), collapse = ";"))
}

# ---- RINEX observation headers --------------------------------------------------------------------------------------
# Receiver, marker, approximate position and the time the observations span (first epoch from the header or first record,
# last from the file's tail). Only the header and the last 64 kB are read. Observations prove data exist, not that a
# flight was corrected with them.
read_rinex <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  h <- readLines(con, n = 400L, warn = FALSE); e <- which(grepl("END OF HEADER", h))[1]
  if (is.na(e)) return(list(rinex_version = NA_real_))
  lab <- function(k) { l <- h[seq_len(e)][grepl(k, substring(h[seq_len(e)], 61), fixed = TRUE)]; if (length(l)) substr(l[1], 1, 60) else NA_character_ }
  epoch <- function(l) { v <- suppressWarnings(as.numeric(strsplit(trimws(sub("^>", "", l)), " +")[[1]][1:6])); if (anyNA(v)) return(as.POSIXct(NA))
    if (v[1] < 100) v[1] <- v[1] + if (v[1] < 80) 2000 else 1900
    ISOdatetime(v[1], v[2], v[3], v[4], v[5], v[6], tz = "UTC") }
  first <- if (!is.na(lab("TIME OF FIRST OBS"))) epoch(lab("TIME OF FIRST OBS")) else as.POSIXct(NA)
  if (is.na(first) && length(h) > e) first <- epoch(h[e + 1L])
  sz <- file.size(path); seek(con, max(0, sz - 65536)); t <- readLines(con, warn = FALSE)
  ep <- if (any(startsWith(t, ">"))) t[startsWith(t, ">")] else t[grepl("^ [0-9 ]{2} [ 0-9]{2} [ 0-9]{2} [ 0-9]{2} [ 0-9]{2} [ 0-9.]{10}  [0-9]", t)]
  last <- if (length(ep)) epoch(ep[length(ep)]) else as.POSIXct(NA)
  xyz <- suppressWarnings(as.numeric(strsplit(trimws(lab("APPROX POSITION XYZ")), " +")[[1]][1:3]))
  ll <- if (!anyNA(xyz) && sum(abs(xyz)) > 1e6) ecef_to_ll(xyz) else c(NA_real_, NA_real_)
  list(rinex_version = suppressWarnings(as.numeric(substr(h[1], 1, 9))), rinex_marker = trimws(lab("MARKER NAME")),
       rinex_receiver = trimws(substr(lab("REC # / TYPE / VERS"), 21, 40)), rinex_program = trimws(substr(lab("PGM / RUN BY / DATE"), 1, 20)),
       obs_start_utc = fmt_ts(first), obs_end_utc = fmt_ts(last), rinex_lat = ll[1], rinex_lon = ll[2])
}

# ---- names ------------------------------------------------------------------------------------------------------------
# DJI file names carry the local clock time: DJI_20200101120000_0001_D.RTK, and folders DJI_202001011200_001_<name>.
dji_name_time <- function(file) {
  s <- sub("^DJI_([0-9]{12,14})_.*", "\\1", file); s[!grepl("^DJI_[0-9]{12,14}_", file)] <- NA
  as.POSIXct(substr(paste0(s, "00"), 1, 14), format = "%Y%m%d%H%M%S", tz = "UTC")
}

# senseFly flight numbers: the aircraft serial and flight count in image names (EP-01-00001_0359_0005.JPG), logs
# (EP-01-00001_0359.bb3, _em.bb3, XX-00-00000_00001.bbx), eMotion output folders (2026-09-08_EBEE_2026_RGB_359, NIR_BogA_354) and old
# folders (EBEE_0303). The number alone is enough to tie them together within one aircraft.
ebee_flight_no <- function(x) {
  b <- basename(as.character(x)); serial <- rep(NA_character_, length(b)); no <- rep(NA_integer_, length(b))
  k <- which(grepl("^[A-Z]{2}-[0-9]{2}-[0-9]{5}_", b))                  # only names that can match are taken apart
  if (length(k)) { m <- regmatches(b[k], regexec("^([A-Z]{2}-[0-9]{2}-[0-9]{5})_([0-9]{3,5})(_em)?(_[0-9]{4})?(_[A-Z]{3})?(\\.|$)", b[k]))
    serial[k] <- vapply(m, function(v) if (length(v)) v[2] else NA_character_, ""); no[k] <- vapply(m, function(v) if (length(v)) as.integer(v[3]) else NA_integer_, 0L) }
  list(serial = serial, no = no)
}
# A flight number in a folder path: the deepest folder named like EP-01-00001_0359, EBEE_0303, NIR_BogA_354 or
# 2026-09-08_EBEE_2026_RGB_359 (not DJI mission folders, whose trailing number is the mission count).
folder_flight_no <- function(folder) { u <- unique(folder)                 # worked out once per folder, not per image
  v <- vapply(strsplit(u, "/", fixed = TRUE), function(p) {
    for (s in rev(p)) { if (grepl("^DJI_", s)) return(NA_integer_)
      e <- ebee_flight_no(s)$no; if (!is.na(e)) return(e)
      if (grepl("[_ -][0-9]{3,4}$", s) && !grepl("[0-9]{4}[-_][0-9]{2}[-_][0-9]{2}$", s) && !grepl("[_ -](19|20)[0-9]{2}$", s)) return(as.integer(sub(".*[_ -]", "", s))) }   # a year is not a flight number
    NA_integer_ }, 0L)
  v[match(folder, u)] }

# Site from folder names. DJI: DJI_202001011200_001_LakeA-RGB -> LakeA. Others: the folder with dates, EBEE,
# years, sensor words and flight numbers taken out: 2024_EBEE_BogA -> BogA, NIR_BogA_354 -> BogA, 2018-08-28_North Transect -> North Transect.
DJI_SUFFIX <- "-(RGB|LiDAR|L2|P1|H30T?|thermal|NIR|copy)$"
site_of <- function(folder) {
  parts <- strsplit(folder, "/", fixed = TRUE)
  vapply(parts, function(p) {
    p <- p[!grepl("^(DCIM|img|[0-9]{3}___[0-9]{2}|Drone Logs|Emotion Logs|[0-9]{4}-Drone Flights|Optical-.*|NIR-.*|LiDAR-.*|Processed)$", p)]
    if (!length(p)) return("")
    b <- p[length(p)]
    if (grepl("^DJI_[0-9]{12}_[0-9]{3}_", b)) { s <- sub("^DJI_[0-9]{12}_[0-9]{3}_", "", b); for (i in 1:2) s <- sub(DJI_SUFFIX, "", s, ignore.case = TRUE); return(s) }
    if (grepl("^EP-[0-9]{2}-[0-9]{5}_[0-9]+$", b)) b <- if (length(p) > 1) p[length(p) - 1] else ""
    s <- gsub("[_-]+", " ", gsub("[0-9]{4}[-_][0-9]{2}[-_][0-9]{2}", " ", b))
    s <- gsub("\\b(EBEE|RGB|NIR|SODA|S\\.O\\.D\\.A\\.|REDO|Flights?|First|Frist|Drone)\\b|\\b(19|20)[0-9]{2}\\b|\\b[0-9]{3,4}$", " ", s, ignore.case = TRUE)
    trimws(gsub("\\s+", " ", s))
  }, "")
}
