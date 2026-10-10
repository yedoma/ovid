# R/gis.R  --  GIS layers (shapefile, GeoJSON, GPX): survey points, GCPs and checkpoints, plots, transects and grids,
# boundaries and extents, field sites and trails. Each file gets its extent; small layers also get their features, thinned
# for the map. Coordinates are converted from UTM to latitude/longitude for drawing only; the files are never changed.
# Large reference sets (a folder with many shapefiles, such as CanVec or basin outlines) keep only their extent.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R.

GIS_CATEGORIES <- list(c("GCPs and checkpoints", "gcp|ground.?control|check.?point|(^|[^a-z])chk"),
                       c("Plots, transects and grids", "transect|grid|plot|quadrat|sampl|train|tile"),
                       c("Boundaries and extents", "boundar|extent|(^|[^a-z])site|aoi|study.?area|basin|watershed|outline|footprint"),
                       c("Field sites and trails", "trail|road|camp|(^|[^a-z])met([^a-z]|$)|flux|tower|station|landing|instrument|well|logger|lake|bog|fen"))
GIS_MAX_BYTES <- 2e6; GIS_MAX_FEATURES <- 500; GIS_MAX_VERTICES <- 200; GIS_REFERENCE_FOLDER <- 20

gis_category <- function(rel) { r <- tolower(rel); out <- rep("Other GIS layers", length(r))
  for (k in rev(GIS_CATEGORIES)) out[grepl(k[2], r)] <- k[1]; out }

# x/y in a layer's own coordinates -> lat/lon (NULL when the system is not known and the numbers are not degrees)
to_ll <- function(x, y, epsg = NA, text = "") {
  if (!length(x)) return(NULL)
  if (all(abs(x) <= 180 & abs(y) <= 90, na.rm = TRUE)) return(cbind(y, x))
  z <- utm_zone(epsg, text); if (is.null(z)) return(NULL)
  utm_to_ll(x, y, z$zone, z$south)
}
thin_idx <- function(n, k = GIS_MAX_VERTICES) if (n <= k) seq_len(n) else unique(c(round(seq(1, n, length.out = k)), n))
ll_text <- function(m) paste(sprintf("%.6f %.6f", m[, 1], m[, 2]), collapse = ";")

# dBASE table beside a shapefile: one label per record (a NAME/ID-like column, else the first one)
read_dbf_labels <- function(path) {
  if (!file.exists(path)) return(character())
  b <- readBin(path, "raw", min(file.size(path), 2e6)); if (length(b) < 33) return(character())
  n <- u32(b, 5L, TRUE); hl <- u16(b, 9L, TRUE); rl <- u16(b, 11L, TRUE); f <- list(); p <- 33L
  while (p + 31L <= length(b) && b[p] != as.raw(0x0D)) { nm <- rawToChar(b[p:(p + 10L)][b[p:(p + 10L)] != as.raw(0)]); f[[length(f) + 1]] <- list(name = nm, len = as.integer(b[p + 16L])); p <- p + 32L }
  if (!length(f)) return(character())
  nms <- vapply(f, `[[`, "", "name"); lens <- vapply(f, `[[`, 0L, "len"); start <- cumsum(c(1L, lens))[seq_along(lens)] + 1L
  k <- c(grep("^(name|label|site|id|plot|point|station|desc)", nms, ignore.case = TRUE), 1L)[1]
  vapply(seq_len(min(n, floor((length(b) - hl) / rl))), function(i) { o <- hl + (i - 1L) * rl + start[k]; v <- b[o:(o + lens[k] - 1L)]
    trimws(iconv(rawToChar(v[v != as.raw(0)]), "latin1", "UTF-8")) }, "")
}

# ESRI shapefile: extent from the 100-byte header; features (points, lines, polygons) when the file is small enough.
read_shp <- function(path, features = TRUE) {
  h <- file_head(path, 100L); if (length(h) < 100 || u32(h, 1L, FALSE) != 9994) return(NULL)
  dbl <- function(b, i, n = 1) readBin(b[i:(i + 8 * n - 1)], "double", n, 8, endian = "little")
  typ <- u32(h, 33L, TRUE); bb <- dbl(h, 37L, 4)
  prj <- sub("\\.shp$", ".prj", path, ignore.case = TRUE); borrowed <- FALSE
  if (!file.exists(prj)) { nb <- list.files(dirname(path), "\\.prj$", full.names = TRUE, ignore.case = TRUE)   # no .prj: a neighbouring layer's (inferred)
    if (length(nb)) { prj <- nb[1]; borrowed <- TRUE } }
  txt <- if (file.exists(prj)) paste(readLines(prj, warn = FALSE), collapse = "") else ""
  kind <- c(`1` = "point", `11` = "point", `21` = "point", `8` = "point", `18` = "point", `28` = "point", `3` = "line", `13` = "line", `23` = "line",
            `5` = "polygon", `15` = "polygon", `25` = "polygon")[as.character(typ)]
  out <- list(geom_type = unname(kind), crs = if (nzchar(txt)) paste0(sub("^(PROJCS|GEOGCS)\\[\"([^\"]*)\".*", "\\2", txt), if (borrowed) " (from a neighbouring layer's .prj, inferred)" else "") else NA_character_, bbox = bb, crs_text = txt, features = NULL)
  if (!features || is.na(kind) || file.size(path) > GIS_MAX_BYTES) return(out)
  b <- readBin(path, "raw", file.size(path)); p <- 101L; fs <- list(); labs <- read_dbf_labels(sub("\\.shp$", ".dbf", path, ignore.case = TRUE)); i <- 0L
  while (p + 12L <= length(b) && length(fs) < GIS_MAX_FEATURES) {
    i <- i + 1L; clen <- 2 * u32(b, p + 4L, FALSE); c0 <- p + 8L; st <- u32(b, c0, TRUE); p <- c0 + clen
    if (is.na(st) || st == 0) next
    xy <- if (st %in% c(1, 11, 21)) matrix(dbl(b, c0 + 4L, 2), ncol = 2)
      else if (st %in% c(8, 18, 28)) { np <- u32(b, c0 + 36L, TRUE); matrix(dbl(b, c0 + 40L, 2 * np), ncol = 2, byrow = TRUE) }
      else { nparts <- u32(b, c0 + 36L, TRUE); np <- u32(b, c0 + 40L, TRUE); pt0 <- c0 + 44L + 4L * nparts
        parts <- vapply(seq_len(nparts), function(j) u32(b, c0 + 44L + 4L * (j - 1L), TRUE), 0)
        m <- matrix(dbl(b, pt0, 2 * np), ncol = 2, byrow = TRUE); m[seq_len(if (nparts > 1) parts[2] else np), , drop = FALSE] }   # first ring or part
    ll <- to_ll(xy[, 1], xy[, 2], NA, txt); if (is.null(ll)) next
    lab <- if (i <= length(labs) && !labs[i] %in% c("0", "")) labs[i] else ""
    if (kind == "point") for (r in seq_len(nrow(ll))) fs[[length(fs) + 1]] <- list(feature = lab, type = "point", line = ll_text(ll[r, , drop = FALSE]))
    else fs[[length(fs) + 1]] <- list(feature = lab, type = kind, line = ll_text(ll[thin_idx(nrow(ll)), , drop = FALSE]))
  }
  out$features <- rbindlist(fs); out
}

# GeoJSON (coordinates are longitude, latitude): every geometry type, labels from a name-like property.
read_geojson <- function(path, features = TRUE) {
  if (file.size(path) > GIS_MAX_BYTES) return(list(geom_type = NA_character_, crs = "WGS 84", bbox = rep(NA_real_, 4), features = NULL))
  g <- jsonlite::fromJSON(path, simplifyVector = FALSE); f <- if (identical(g$type, "FeatureCollection")) g$features else list(g)
  fs <- list(); allx <- c(); ally <- c()
  add <- function(type, co, lab) { m <- do.call(rbind, lapply(co, function(c) as.numeric(unlist(c)[1:2]))); if (is.null(m)) return()
    allx <<- c(allx, m[, 1]); ally <<- c(ally, m[, 2]); ll <- cbind(m[, 2], m[, 1])
    fs[[length(fs) + 1]] <<- list(feature = lab, type = type, line = ll_text(ll[if (type == "point") 1 else thin_idx(nrow(ll)), , drop = FALSE])) }
  for (x in f[seq_len(min(length(f), GIS_MAX_FEATURES))]) { geo <- if (!is.null(x$geometry)) x$geometry else x; if (is.null(geo$type)) next
    pr <- x$properties %||% list(); lab <- as.character(pr[[c(grep("^(name|label|site|id)", names(pr), ignore.case = TRUE), 1L)[1]]] %||% "")
    switch(geo$type, Point = add("point", list(geo$coordinates), lab), MultiPoint = for (c in geo$coordinates) add("point", list(c), lab),
           LineString = add("line", geo$coordinates, lab), MultiLineString = for (c in geo$coordinates) add("line", c, lab),
           Polygon = add("polygon", geo$coordinates[[1]], lab), MultiPolygon = for (c in geo$coordinates) add("polygon", c[[1]], lab), NULL) }
  fs <- rbindlist(fs)
  list(geom_type = if (nrow(fs)) names(sort(table(fs$type), decreasing = TRUE))[1] else NA_character_, crs = "WGS 84",
       bbox = if (length(allx)) c(min(allx), min(ally), max(allx), max(ally)) else rep(NA_real_, 4), features = if (features) fs)
}

# GPX: tracks and routes as lines, waypoints as points.
read_gpx <- function(path, features = TRUE) {
  x <- paste(readLines(path, warn = FALSE), collapse = " ")
  pts <- function(tag) { m <- regmatches(x, gregexpr(paste0("<", tag, "[^>]*>"), x))[[1]]
    if (!length(m)) return(NULL); cbind(as.numeric(sub('.*lat="([-0-9.]+)".*', "\\1", m)), as.numeric(sub('.*lon="([-0-9.]+)".*', "\\1", m))) }
  tr <- rbind(pts("trkpt"), pts("rtept")); wp <- pts("wpt"); fs <- list()
  if (!is.null(tr)) fs[[1]] <- list(feature = "track", type = "line", line = ll_text(tr[thin_idx(nrow(tr), 1000), , drop = FALSE]))
  if (!is.null(wp)) for (r in seq_len(min(nrow(wp), GIS_MAX_FEATURES))) fs[[length(fs) + 1]] <- list(feature = "waypoint", type = "point", line = ll_text(wp[r, , drop = FALSE]))
  a <- rbind(tr, wp)
  list(geom_type = if (!is.null(tr)) "line" else "point", crs = "WGS 84", bbox = if (is.null(a)) rep(NA_real_, 4) else c(min(a[, 2]), min(a[, 1]), max(a[, 2]), max(a[, 1])),
       features = if (features) rbindlist(fs))
}

# One source: a row per GIS file (layers) and its features (thinned), both tagged with the file.
read_source_gis <- function(files) {
  f <- files[role == "gis" & status == "" & grepl("\\.(shp|geojson|json|gpx)$", file, ignore.case = TRUE)]
  if (!nrow(f)) return(list(layers = data.table(), features = data.table()))
  ref <- f[, .N, by = folder][N > GIS_REFERENCE_FOLDER]$folder        # many layers in one folder: a reference set, extents only
  L <- list(); Fe <- list()
  for (i in seq_len(nrow(f))) {
    want <- !f$folder[i] %in% ref; p <- f$path[i]
    r <- tryCatch(if (grepl("\\.shp$", p, ignore.case = TRUE)) read_shp(p, want) else if (grepl("\\.gpx$", p, ignore.case = TRUE)) read_gpx(p, want) else read_geojson(p, want),
                  error = function(e) { problem(f$rel[i], paste("GIS layer not read:", conditionMessage(e))); NULL })
    if (is.null(r)) next
    bb <- r$bbox; ll <- if (all(is.finite(bb))) to_ll(c(bb[1], bb[3]), c(bb[2], bb[4]), NA, r$crs_text %||% "") else NULL
    L[[length(L) + 1]] <- data.table(rel = f$rel[i], folder = f$folder[i], file = f$file[i], category = gis_category(f$rel[i]), geom_type = r$geom_type %||% NA_character_,
      crs = r$crs %||% NA_character_, n_features = if (is.null(r$features)) NA_integer_ else nrow(r$features),
      lat_min = if (is.null(ll)) NA_real_ else min(ll[, 1]), lon_min = if (is.null(ll)) NA_real_ else min(ll[, 2]),
      lat_max = if (is.null(ll)) NA_real_ else max(ll[, 1]), lon_max = if (is.null(ll)) NA_real_ else max(ll[, 2]),
      detail = if (!want) "reference set: extent only" else if (is.null(ll)) "coordinate system not known: not placed" else "")
    if (!is.null(r$features) && nrow(r$features)) Fe[[length(Fe) + 1]] <- cbind(data.table(rel = f$rel[i], category = gis_category(f$rel[i])), r$features)
  }
  list(layers = rbindlist(L, fill = TRUE), features = rbindlist(Fe, fill = TRUE))
}
