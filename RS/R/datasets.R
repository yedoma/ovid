# R/datasets.R  --  Satellite, aerial and LiDAR datasets: what each file is, when it was taken and where it lies.
# Reads vendor metadata (Maxar/DigitalGlobe .IMD for WorldView, QuickBird, GeoEye, IKONOS; SPOT DIMAP), GeoTIFF and
# LAS/LAZ headers (bounds and coordinate system), world files and .aux.xml sidecars. Only metadata is read: the imagery
# and points are never opened past their headers and never changed. Footprints from vendor corners are "source" geometry;
# extents worked out from a header's bounds are "derived" (converted from UTM to latitude/longitude for the map only).
# Part of Ovid Core // RS. Sourced by rs_pipeline.R.

DATASET_COLS <- c("rel", "folder", "file", "kind", "platform", "product", "acquired", "date_source", "crs", "epsg",
                  "lat_min", "lon_min", "lat_max", "lon_max", "footprint", "geometry_from", "gsd_m", "cloud_pct", "n_points", "detail",
                  "point_density", "classes")       # LiDAR: points per square map unit of the tile's box (m2 in a metric CRS); ASPRS classes in a sample

# What a file is, from its name and folders. Platform words found in a path are an inference and say so in date_source/detail.
PLATFORMS <- list(c("WorldView-3", "wv0?3|worldview.?3"), c("WorldView-2", "wv0?2|worldview.?2|worldview"), c("QuickBird", "qb0?2|quickbird"),
                  c("GeoEye-1", "ge0?1|geoeye"), c("IKONOS", "ikonos|(^|/)po_[0-9]+"), c("SPOT 5", "spot.?5|(^|/)s5_"), c("SPOT 4", "spot.?4|(^|/)s4_"),
                  c("SPOT", "spot"), c("Pleiades", "pleiades|phr1"), c("Landsat", "landsat|(^|/)l[cet]0?[0-9]_"), c("Sentinel-2", "sentinel|(^|/)s2[ab]_"),
                  c("Aerial photograph", "aerial|air.?photo|(^|/)a[0-9]{5}_[0-9]{3}"), c("Airborne LiDAR", "lidar|\\.la[sz]$"))
SAT_IDS <- c(WV01 = "WorldView-1", WV02 = "WorldView-2", WV03 = "WorldView-3", WV04 = "WorldView-4", QB02 = "QuickBird", GE01 = "GeoEye-1", IK02 = "IKONOS")

platform_of <- function(rel) { r <- tolower(rel); out <- rep(NA_character_, length(r))
  for (p in rev(PLATFORMS)) out[grepl(p[2], r)] <- p[1]; out }
kind_of <- function(platform) fifelse(platform %in% "Aerial photograph", "aerial", fifelse(platform %in% "Airborne LiDAR", "lidar", fifelse(is.na(platform), "raster", "satellite")))
product_of <- function(rel) { r <- tolower(basename(rel))
  fifelse(grepl("dem|dtm", r), "DEM (LiDAR or survey product)", fifelse(grepl("dsm", r), "DSM", fifelse(grepl("class", r), "classification",
  fifelse(grepl("-p[0-9]a|_pan|pan_|(^|_)p[0-9]{3}_|_p10_|panchro", r), "panchromatic", fifelse(grepl("-m[0-9]a|_ms_|_mul|_m20_|multi", r), "multispectral",
  fifelse(grepl("ortho", r), "ortho image", fifelse(grepl("\\.la[sz]$", r), "point cloud tile", "image"))))))) }

# ---- coordinates: UTM (WGS84 / NAD83, the same to a metre here) to latitude/longitude, for drawing only ----------------
is_geographic <- function(epsg = NA, text = "") (!is.na(epsg) && as.integer(epsg) %in% c(4326L, 4269L, 4617L, 4140L)) || (nzchar(text) && grepl("^GEOGCS|GEOGCRS", text))
# Bounds in a file's own coordinates -> lat/lon box (min/max of the four corners).
box_ll <- function(xmin, ymin, xmax, ymax, epsg = NA, text = "") {
  if (any(!is.finite(c(xmin, ymin, xmax, ymax)))) return(NULL)
  if (abs(xmin) > 360 || abs(ymin) > 360) { if (!is.na(epsg) && is_geographic(epsg)) epsg <- NA }
  else if (is_geographic(epsg, text) || (abs(xmin) <= 180 && abs(xmax) <= 180 && abs(ymin) <= 90 && abs(ymax) <= 90)) return(c(ymin, xmin, ymax, xmax))
  z <- utm_zone(epsg, text); if (is.null(z)) return(NULL)
  p <- utm_to_ll(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax), z$zone, z$south)
  r <- c(min(p[, 1]), min(p[, 2]), max(p[, 1]), max(p[, 2])); if (all(is.finite(r)) && r[1] >= -90 && r[3] <= 90) r else NULL
}

# ---- readers ----------------------------------------------------------------------------------------------------------
# Maxar/DigitalGlobe .IMD: satellite, time, cloud, GSD and the four corner coordinates of the scene.
read_imd <- function(path) {
  L <- readLines(path, warn = FALSE)
  v <- function(k) { x <- grep(paste0("^\\s*", k, "\\s*="), L, value = TRUE); if (length(x)) trim(gsub("[\";]", "", sub(".*?=", "", x[1]))) else NA_character_ }
  num <- function(k) suppressWarnings(as.numeric(v(k)))
  c4 <- c(num("ULLat"), num("ULLon"), num("URLat"), num("URLon"), num("LRLat"), num("LRLon"), num("LLLat"), num("LLLon"))
  sat <- v("satId"); t <- v("firstLineTime"); if (is.na(t)) t <- v("earliestAcqTime")
  cc <- num("cloudCover")
  list(platform = if (!is.na(sat) && sat %in% names(SAT_IDS)) unname(SAT_IDS[sat]) else sat, acquired = sub("T", " ", substr(t, 1, 19)), date_source = "vendor metadata (.IMD)",
       product = paste(na.omit(c(v("bandId"), v("productLevel"))), collapse = " "), cloud_pct = if (is.finite(cc) && cc >= 0) round(100 * cc, 1) else NA_real_,
       gsd_m = num("meanCollectedGSD") %|NA|% num("meanProductGSD"), crs = paste(na.omit(c(v("mapProjName"), v("mapZone"), v("datumName"))), collapse = " "),
       lat_min = min(c4[c(1, 3, 5, 7)]), lat_max = max(c4[c(1, 3, 5, 7)]), lon_min = min(c4[c(2, 4, 6, 8)]), lon_max = max(c4[c(2, 4, 6, 8)]),
       footprint = if (all(is.finite(c4))) sprintf("%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f", c4[1], c4[2], c4[3], c4[4], c4[5], c4[6], c4[7], c4[8], c4[1], c4[2]) else NA_character_,
       geometry_from = "vendor scene corners")
}
`%|NA|%` <- function(a, b) if (length(a) && is.finite(a)) a else b

# SPOT DIMAP (METADATA.DIM): mission, date and scene frame.
read_dimap <- function(path) {
  x <- paste(readLines(path, warn = FALSE), collapse = " ")
  tag <- function(t) { m <- regmatches(x, gregexpr(paste0("<", t, "[^>]*>[^<]*</", t, ">"), x))[[1]]; trim(gsub("<[^>]+>", "", m)) }
  la <- suppressWarnings(as.numeric(tag("FRAME_LAT"))); lo <- suppressWarnings(as.numeric(tag("FRAME_LON")))
  fp <- if (length(la) >= 3 && length(la) == length(lo)) paste(sprintf("%.6f %.6f", c(la, la[1]), c(lo, lo[1])), collapse = ";") else NA_character_
  list(platform = trim(paste(tag("MISSION")[1], tag("MISSION_INDEX")[1])), acquired = paste(tag("IMAGING_DATE")[1], tag("IMAGING_TIME")[1]),
       date_source = "vendor metadata (DIMAP)", product = tag("DATASET_NAME")[1], crs = tag("HORIZONTAL_CS_NAME")[1] %||% NA_character_,
       lat_min = suppressWarnings(min(la)), lat_max = suppressWarnings(max(la)), lon_min = suppressWarnings(min(lo)), lon_max = suppressWarnings(max(lo)),
       footprint = fp, geometry_from = "vendor scene corners")
}

# GeoKeyDirectory (as numbers) -> EPSG of the projected or geographic system.
geokey_epsg <- function(k) { if (length(k) < 8) return(NA_integer_); n <- k[4]
  r <- matrix(k[5:(4 + 4 * n)], ncol = 4, byrow = TRUE); r <- r[r[, 2] == 0 & r[, 4] > 1000 & r[, 4] < 32767, , drop = FALSE]
  for (key in c(3072, 2048)) if (any(r[, 1] == key)) return(as.integer(r[r[, 1] == key, 4][1]))   # projected first, then geographic
  NA_integer_ }

# n bytes at a 0-based offset, without reading what comes before (the IFD of a large GeoTIFF often sits at its end).
read_at <- function(con, off, n) { if (!is.finite(off) || off < 0 || n <= 0) return(raw()); seek(con, off); readBin(con, "raw", n) }

# GeoTIFF: size, pixel scale, tie point and coordinate system from the first IFD, wherever it sits in the file.
read_geotiff <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  b <- read_at(con, 0, 8); if (length(b) < 8) return(NULL)
  le <- b[1] == as.raw(0x49); if (!identical(u16(b, 3L, le), 42L)) return(NULL)        # classic TIFF only (BigTIFF: sidecars)
  off <- u32(b, 5L, le); n <- u16(read_at(con, off, 2), 1L, le); if (is.na(n) || n > 1000) return(NULL)
  ifd <- read_at(con, off + 2, 12 * n); tg <- list(); en <- if (le) "little" else "big"
  for (k in seq_len(n)) { e <- 12L * (k - 1L) + 1L; tag <- u16(ifd, e, le); typ <- u16(ifd, e + 2L, le); cnt <- u32(ifd, e + 4L, le)
    if (is.na(tag) || !tag %in% c(256L, 257L, 33550L, 33922L, 34735L, 34737L) || is.na(typ) || typ > 12L) next
    len <- c(1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8)[typ] * cnt; if (len > 1e6) next
    v <- if (len <= 4) ifd[(e + 8L):(e + 7L + len)] else read_at(con, u32(ifd, e + 8L, le), len)
    tg[[as.character(tag)]] <- if (typ == 12L) readBin(v, "double", cnt, 8, endian = en)
      else if (typ == 3L) readBin(v, "integer", cnt, 2, signed = FALSE, endian = en)
      else if (typ == 4L) vapply(seq_len(cnt), function(j) u32(v, 4L * (j - 1L) + 1L, le), 0)
      else if (typ == 2L) rawToChar(v[v != as.raw(0)]) else NULL }
  w <- tg[["256"]][1]; h <- tg[["257"]][1]; sc <- tg[["33550"]]; tp <- tg[["33922"]]
  epsg <- geokey_epsg(tg[["34735"]]); txt <- tg[["34737"]] %||% ""
  if (is.null(sc) || length(tp) < 6 || is.null(w)) return(list(width = w, height = h, epsg = epsg, crs = txt))
  x0 <- tp[4] - tp[1] * sc[1]; y0 <- tp[5] + tp[2] * sc[2]
  list(width = w, height = h, epsg = epsg, crs = txt, xmin = x0, xmax = x0 + w * sc[1], ymax = y0, ymin = y0 - h * sc[2], gsd_m = sc[1])
}

# LAS/LAZ public header (LAZ keeps it uncompressed): version, date, point count, bounds and the coordinate system VLR.
LAS_SAMPLE <- 20000L
read_las <- function(path) {
  b <- file_head(path, 4096L); if (length(b) < 227 || rawToChar(b[1:4]) != "LASF") return(NULL)
  d <- function(i) readBin(b[i:(i + 7)], "double", 1, 8, endian = "little")
  hs <- u16(b, 95L, TRUE); pd <- u32(b, 97L, TRUE); nv <- u32(b, 101L, TRUE); ver <- paste0(as.integer(b[25]), ".", as.integer(b[26]))
  np <- u32(b, 108L, TRUE); if (as.integer(b[26]) >= 4 && length(b) >= 255) { np2 <- sum(as.numeric(b[248:255]) * 256^(0:7)); if (np2 > 0) np <- np2 }
  day <- u16(b, 91L, TRUE); yr <- u16(b, 93L, TRUE)
  if (pd > length(b)) b <- file_head(path, min(pd, 1e6))
  epsg <- NA_integer_; wkt <- ""; p <- hs + 1
  for (i in seq_len(min(nv, 50))) { if (p + 54 > length(b)) break
    rid <- u16(b, p + 18L, TRUE); len <- u16(b, p + 20L, TRUE); dp <- p + 54
    if (dp + len - 1 <= length(b) && len > 0) { v <- b[dp:(dp + len - 1)]
      if (rid == 34735L) epsg <- geokey_epsg(readBin(v, "integer", len / 2, 2, signed = FALSE, endian = "little"))
      if (rid == 2112L) wkt <- rawToChar(v[v != as.raw(0)]) }
    p <- dp + len }
  xmin <- d(188); xmax <- d(180); ymin <- d(204); ymax <- d(196)
  # points per square unit of the box (geographic coordinates have no density in metres), and the classification codes of
  # the first LAS_SAMPLE points (LAZ is compressed: none)
  fmt <- as.integer(b[105]); rl <- u16(b, 106L, TRUE); area <- (xmax - xmin) * (ymax - ymin)
  dens <- if (is.finite(area) && area > 0 && !(abs(xmax) <= 180 && abs(ymax) <= 90)) signif(np / area, 3) else NA_real_
  cls <- ""
  if (fmt < 128 && !is.na(rl) && rl > 0 && np > 0) { k <- min(np, LAS_SAMPLE); raw <- tryCatch({ con <- file(path, "rb"); tryCatch({ seek(con, pd); readBin(con, "raw", k * rl) }, finally = close(con)) }, error = function(e) raw())
    if (length(raw) >= rl) { n <- length(raw) %/% rl; at <- (seq_len(n) - 1L) * rl + if (fmt >= 6) 17L else 16L
      v <- as.integer(raw[at]); if (fmt < 6) v <- bitwAnd(v, 31L); u <- sort(unique(v))
      cls <- paste0(paste(u, collapse = ","), if (np > n) paste0(" (first ", format(n, big.mark = ","), " points)") else "") } }
  list(version = ver, n_points = np, acquired = if (yr > 1990 && day > 0) format(as.Date(sprintf("%d-01-01", yr)) + day - 1) else NA_character_,
       xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, epsg = epsg, crs = wkt, point_format = fmt %% 128L, point_density = dens, classes = cls)
}

# A world file (.tfw/.jgw) and .aux.xml/.prj beside a raster give its corner, pixel size and coordinate system.
sidecar_crs <- function(path) { for (s in c(paste0(path, ".aux.xml"), sub("\\.[^.]+$", ".prj", path), paste0(path, ".xml")))
    if (file.exists(s)) { x <- paste(readLines(s, warn = FALSE, n = 400), collapse = " "); m <- regmatches(x, regexpr("(PROJCS|GEOGCS|PROJCRS|GEOGCRS)\\[[^<]*", x)); if (length(m)) return(m) }
  "" }
world_file <- function(path) { w <- c(sub("\\.[^.]+$", ".tfw", path), sub("\\.[^.]+$", ".tifw", path), paste0(path, "w"))
  w <- w[file.exists(w)]; if (!length(w)) return(NULL); v <- suppressWarnings(as.numeric(readLines(w[1], n = 6, warn = FALSE))); if (length(v) < 6 || anyNA(v)) NULL else v }

# ---- one source: every dataset file -> one row ---------------------------------------------------------------------
DATASET_ROLES <- c("raster", "lidar_tile", "dataset_meta")
read_source_datasets <- function(files) {
  f <- files[role %in% DATASET_ROLES & status == ""]
  if (!nrow(f)) return(data.table())
  rows <- lapply(seq_len(nrow(f)), function(i) {
    p <- f$path[i]; r <- tryCatch(switch(f$format[i],
      sat_imd = read_imd(p), spot_dimap = read_dimap(p),
      lidar_tile = { h <- read_las(p); if (is.null(h)) NULL else { bb <- box_ll(h$xmin, h$ymin, h$xmax, h$ymax, h$epsg, h$crs)
        list(acquired = h$acquired, date_source = if (!is.na(h$acquired)) "LAS header (file creation day)" else NA, n_points = h$n_points, epsg = h$epsg,
             crs = if (nzchar(h$crs)) substr(h$crs, 1, 80) else NA, detail = paste0("LAS ", h$version, ", point format ", h$point_format), bb = bb, geometry_from = "LAS header bounds",
             point_density = h$point_density, classes = if (nzchar(h$classes)) h$classes else NA_character_) } },
      { g <- if (grepl("\\.tiff?$", p, ignore.case = TRUE)) read_geotiff(p) else NULL; txt <- sidecar_crs(p); wf <- world_file(p)
        if ((is.null(g) || is.null(g$xmin)) && !is.null(wf) && !is.null(g$width)) g <- c(g, list(xmin = wf[5], ymax = wf[6], xmax = wf[5] + g$width * wf[1], ymin = wf[6] + g$height * wf[4], gsd_m = wf[1]))
        epsg <- g$epsg %||% NA; crs <- if (!is.null(g$crs) && nzchar(g$crs)) g$crs else txt
        bb <- if (!is.null(g$xmin)) box_ll(g$xmin, g$ymin, g$xmax, g$ymax, epsg, crs) else NULL
        list(epsg = epsg, crs = if (nzchar(crs %||% "")) substr(sub("^(PROJCS|GEOGCS)\\[\"([^\"]*)\".*", "\\2", crs), 1, 80) else NA, gsd_m = g$gsd_m %||% NA,
             detail = if (!is.null(g$width)) paste(g$width, "x", g$height, "px") else NA, bb = bb,
             geometry_from = if (is.null(bb)) NA else if (!is.null(g$xmin) && is.null(wf)) "GeoTIFF tags" else "world file") }),
      error = function(e) { problem(f$rel[i], conditionMessage(e)); NULL })
    if (is.null(r)) r <- list()
    if (!is.null(r$bb)) { b <- r$bb; r[c("lat_min", "lon_min", "lat_max", "lon_max")] <- as.list(b)
      r$footprint <- sprintf("%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f", b[1], b[2], b[1], b[4], b[3], b[4], b[3], b[2], b[1], b[2]) }
    r$bb <- NULL; r[vapply(r, function(x) is.null(x) || length(x) != 1, TRUE)] <- NULL
    as.data.table(c(list(.i = i), r))
  })
  d <- rbindlist(rows, fill = TRUE); setorder(d, .i); d[, .i := NULL]
  for (k in setdiff(DATASET_COLS, names(d))) set(d, j = k, value = rep(NA, nrow(d)))
  d <- cbind(f[, .(rel, folder, file)], d[, setdiff(DATASET_COLS, c("rel", "folder", "file")), with = FALSE])
  for (k in c("platform", "product", "acquired", "date_source", "crs", "footprint", "geometry_from", "detail")) set(d, j = k, value = as.character(d[[k]]))
  guess <- platform_of(d$rel)
  d[, platform := fifelse(is.na(platform) | !nzchar(platform), guess, platform)]
  d[, kind := kind_of(platform)][, product := fifelse(is.na(product) | !nzchar(product), product_of(rel), product)]
  d[is.na(acquired) | !nzchar(trim(acquired)), `:=`(acquired = NA_character_, date_source = NA_character_)]
  d[is.na(acquired) & grepl("(19[4-9][0-9]|20[0-4][0-9])", rel), `:=`(acquired = sub(".*?(19[4-9][0-9]|20[0-4][0-9]).*", "\\1", rel), date_source = "year in the folder or file name (inferred)")]
  # a scene's image files take the satellite, time and cloud of the .IMD with the same name
  im <- d[grepl("\\.imd$", file, ignore.case = TRUE), .(stem = sub("\\.imd$", "", file, ignore.case = TRUE), folder, platform, acquired, date_source, cloud_pct)]
  for (i in seq_len(nrow(im))) d[folder == im$folder[i] & !grepl("\\.imd$", file, ignore.case = TRUE) & grepl(im$stem[i], file, fixed = TRUE),
                                 `:=`(platform = im$platform[i], kind = "satellite", acquired = im$acquired[i], date_source = paste(im$date_source[i], "of the same scene"), cloud_pct = im$cloud_pct[i])]
  # an 8-digit date in a LiDAR or image file name (C2-20180725f_...) is the likelier collection day than a LAS file's creation day
  nd <- sub(".*?((19[4-9][0-9]|20[0-4][0-9])(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])).*", "\\1", d$file)
  has <- grepl("(19[4-9][0-9]|20[0-4][0-9])(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])", d$file) & (is.na(d$date_source) | !grepl("vendor", d$date_source))
  d[has, `:=`(acquired = format(as.Date(nd[has], "%Y%m%d")), date_source = "date in the file name (inferred)")]
  d[, year := suppressWarnings(as.integer(substr(acquired, 1, 4)))]
  d[]
}
