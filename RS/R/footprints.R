# R/footprints.R  --  The ground each flight covered, and the flights of a year joined into one outline. Part of Ovid
# Core // RS. Sourced by rs_pipeline.R (each flight's footprint, catalogue/footprints.csv) and used by the app (the Map
# page's "One footprint per year" and its report). Nothing here reads the archive: it works from the image table.
#
# A photo covers a rectangle on the ground (pinhole camera; McGlone 2013, Wolf et al. 2014):
#   across the track = sensor width x height / focal length;  along the track = that x image height / image width
# turned to the flight direction (the short side of the image along the track, the usual layout for mapping flights, as
# fwd_overlap_pct in R/catalogue.R). A LiDAR flight covers a strip along its flown lines (Baltsavias 1999):
#   swath width = 2 x height x tan(field of view / 2)
# Height is above the ground the aircraft took off from: DJI's RelativeAltitude; for an eBee, the photo's GNSS height in
# its flight log less the log's take-off height (both RTK, ellipsoidal), or, with no take-off in the log, less the height
# of the GNSS base on the ground (its RINEX header, less the antenna height). Over sloping ground the footprint is
# approximate. Footprints are marked on a grid (FOOTPRINT_CELL_M) in metres around the centre of what is drawn, the
# covered cells are traced once into outlines with isoband (holes kept), simplified by one cell (Douglas and Peucker
# 1973), and parts and holes under FOOTPRINT_MIN_M2 are dropped. Sources: _hub/Derived_values_references.md, section 14.

FOOTPRINT_CELL_M <- 2              # grid cell (m): fine against a photo footprint of 50 m or more
FOOTPRINT_MAX_CELLS <- 6e6         # a wider area takes a coarser cell, so a year spread over many km still fits in memory
FOOTPRINT_MIN_M2 <- 500            # parts and holes smaller than this (a few cells at a strip's end) are dropped
FOOTPRINT_LINE_GAP_S <- 30         # positions further apart in time are on different lines (as flight_metrics)
# LiDAR field of view across the track (degrees), by the EXIF model of the payload's camera. Zenmuse L2: 70 deg horizontal
# in both scan modes (DJI 2024, Zenmuse L2 User Manual v1.2); Zenmuse L1: 70.4 deg (DJI 2020, Zenmuse L1 User Manual v1.0).
LIDAR_FOV_DEG <- c(L2 = 70, L1 = 70.4)

# ---- local metres --------------------------------------------------------------------------------------------------------
# Equirectangular around (lat0, lon0): within a few km the error is far below a grid cell.
FP_R <- 6371008.8                  # mean Earth radius (m), IUGG
fp_xy <- function(lat, lon, lat0, lon0) list(x = (lon - lon0) * pi / 180 * FP_R * cos(lat0 * pi / 180), y = (lat - lat0) * pi / 180 * FP_R)
fp_ll <- function(x, y, lat0, lon0) list(lat = lat0 + y / FP_R * 180 / pi, lon = lon0 + x / (FP_R * cos(lat0 * pi / 180)) * 180 / pi)
fp_area <- function(x, y) { n <- length(x); j <- c(2:n, 1L); abs(sum(x * y[j] - x[j] * y)) / 2 }

# ---- rectangles ---------------------------------------------------------------------------------------------------------
# One rectangle per position, centred on (x, y), w across and l along the direction h (radians from +x): four corners
# per id, in order, as rows of id, x, y.
fp_rects <- function(id, x, y, w, l, h) {
  c1 <- cos(h); s1 <- sin(h); k <- data.table(a = c(-1, 1, 1, -1), b = c(-1, -1, 1, 1))
  d <- data.table(id = rep(id, each = 4L), a = rep(k$a, length(id)), b = rep(k$b, length(id)), x = rep(x, each = 4L), y = rep(y, each = 4L),
                  c1 = rep(c1, each = 4L), s1 = rep(s1, each = 4L), w = rep(w, each = 4L), l = rep(l, each = 4L))
  d[, .(id, x = x + a * c1 * l / 2 - b * s1 * w / 2, y = y + a * s1 * l / 2 + b * c1 * w / 2)]
}

# The direction of travel at each position: from the position before to the one after on the same line (NA alone).
fp_heading <- function(x, y, t) {
  n <- length(x); if (n < 2L) return(rep(NA_real_, n))
  same <- function(i, j) is.na(t[i]) | is.na(t[j]) | abs(t[j] - t[i]) <= FOOTPRINT_LINE_GAP_S
  nx <- c(2:n, n); pv <- c(1L, 1:(n - 1L))
  nx[!same(seq_len(n), nx)] <- seq_len(n)[!same(seq_len(n), nx)]; pv[!same(pv, seq_len(n))] <- seq_len(n)[!same(pv, seq_len(n))]
  h <- atan2(y[nx] - y[pv], x[nx] - x[pv]); h[nx == pv] <- NA_real_; h
}

# ---- grid ---------------------------------------------------------------------------------------------------------------
# A grid over x/y ranges with a margin: cell size grows when the area would need more than FOOTPRINT_MAX_CELLS.
fp_grid <- function(x, y, cell = FOOTPRINT_CELL_M) {
  w <- diff(range(x)) + 4 * cell; h <- diff(range(y)) + 4 * cell
  cell <- max(cell, sqrt(w * h / FOOTPRINT_MAX_CELLS))
  list(x0 = round(min(x), 6) - 2 * cell, y0 = round(min(y), 6) - 2 * cell, cell = cell, nx = ceiling(w / cell) + 2L, ny = ceiling(h / cell) + 2L)
}

# How many groups cover each cell: polygons as rows of g (a group: one photo, one flight), r (a ring of it) and x, y in
# order. Within a group the rings fill even-odd (a flight's holes stay holes); groups add up. Scanline at the cell
# centres, so a cell counts when its centre is inside.
fp_count <- function(p, gr) {
  p <- copy(p)[, `:=`(x = round(x, 6), y = round(y, 6))]      # to the micrometre: the last bits of sin, cos and atan2 differ between processors
  e <- p[, { n <- .N; j <- c(2:n, 1L); .(x1 = x, y1 = y, x2 = x[j], y2 = y[j]) }, by = .(g, r)][y1 != y2]
  e[, `:=`(lo = ceiling((pmin(y1, y2) - gr$y0) / gr$cell + 0.5), hi = ceiling((pmax(y1, y2) - gr$y0) / gr$cell + 0.5) - 1L)]
  e <- e[hi >= lo]
  m <- matrix(0L, gr$ny, gr$nx + 1L)
  if (!nrow(e)) return(m[, -ncol(m), drop = FALSE])
  rr <- e[rep(seq_len(.N), hi - lo + 1L)][, row := lo + sequence(e$hi - e$lo + 1L) - 1L]
  rr[, xi := round(x1 + (gr$y0 + (row - 0.5) * gr$cell - y1) * (x2 - x1) / (y2 - y1), 6)]
  sp <- rr[order(g, row, xi), .(xa = xi[c(TRUE, FALSE)], xb = xi[c(FALSE, TRUE)]), by = .(g, row)]
  sp[, `:=`(ia = pmax(1L, ceiling((xa - gr$x0) / gr$cell + 0.5)), ib = pmin(gr$nx, ceiling((xb - gr$x0) / gr$cell + 0.5) - 1L))]
  sp <- sp[ib >= ia]
  add <- function(i, j, v) { k <- tabulate((j - 1L) * gr$ny + i, gr$ny * (gr$nx + 1L)); m <<- m + v * matrix(k, gr$ny) }
  add(sp$row, sp$ia, 1L); add(sp$row, sp$ib + 1L, -1L)
  t(apply(m, 1, cumsum))[, seq_len(gr$nx), drop = FALSE]
}

# Douglas-Peucker on a closed ring (first point not repeated): points closer than tol to the line are dropped.
fp_simplify <- function(x, y, tol) {
  n <- length(x); if (n <= 4L) return(list(x = x, y = y))
  keep <- logical(n); keep[c(1L, n)] <- TRUE; st <- list(c(1L, n))
  # squared distances, no sqrt: exact on grid points (halves of a cell), so a point exactly tol off goes the same way on every processor
  while (length(st)) { s <- st[[length(st)]]; st[[length(st)]] <- NULL; a <- s[1]; b <- s[2]; if (b - a < 2L) next
    i <- (a + 1L):(b - 1L); dx <- x[b] - x[a]; dy <- y[b] - y[a]; L2 <- dx^2 + dy^2
    d2 <- if (L2 > 0) (dy * x[i] - dx * y[i] + x[b] * y[a] - y[b] * x[a])^2 else (x[i] - x[a])^2 + (y[i] - y[a])^2
    if (max(d2) > tol^2 * (if (L2 > 0) L2 else 1)) { k <- i[which.max(d2)]; keep[k] <- TRUE; st <- c(st, list(c(a, k), c(k, b))) } }
  list(x = x[keep], y = y[keep])
}

fp_inside <- function(px, py, x, y) { n <- length(x); j <- c(n, seq_len(n - 1L))
  vapply(seq_along(px), function(k) sum(((y > py[k]) != (y[j] > py[k])) & (px[k] < (x[j] - x) * (py[k] - y) / (y[j] - y) + x)) %% 2 == 1, logical(1)) }

# The outline of the covered cells (count > 0): rows of part, ring (0 = the outer edge, 1+ = holes), x, y.
fp_trace <- function(cnt, gr) {
  z <- matrix(0, nrow(cnt) + 2L, ncol(cnt) + 2L); z[2:(nrow(cnt) + 1L), 2:(ncol(cnt) + 1L)] <- cnt > 0
  L <- isoband::isolines(seq_len(ncol(z)) - 1.5, seq_len(nrow(z)) - 1.5, z, 0.5)[[1]]   # in cells: the points fall on halves
  if (!length(L$x)) return(data.table(part = integer(), ring = integer(), x = numeric(), y = numeric()))
  rg <- lapply(split(data.table(x = L$x, y = L$y), L$id), function(d) { d <- d[-nrow(d)]
    if (sum(d$x * shift(d$y, -1L, fill = d$y[1]) - shift(d$x, -1L, fill = d$x[1]) * d$y) < 0) d <- d[.N:1]   # counter-clockwise, from the lowest-left
    k <- d[, which(y == min(y))][which.min(d[y == min(y), x])]; d <- d[c(k:.N, seq_len(k - 1L))]               # point, so the outline is the same
    s <- fp_simplify(d$x, d$y, 1)                                                                                    # wherever the tracing starts
    data.table(x = gr$x0 + s$x * gr$cell, y = gr$y0 + s$y * gr$cell) })
  rg <- rg[vapply(rg, nrow, 0L) >= 3L]
  a <- vapply(rg, function(d) fp_area(d$x, d$y), 0); rg <- rg[a >= FOOTPRINT_MIN_M2]; a <- a[a >= FOOTPRINT_MIN_M2]
  if (!length(rg)) return(data.table(part = integer(), ring = integer(), x = numeric(), y = numeric()))
  # nesting: a ring inside an odd number of others is a hole of the smallest ring around it
  inn <- sapply(seq_along(rg), function(i) vapply(seq_along(rg), function(j) i != j && fp_inside(rg[[i]]$x[1], rg[[i]]$y[1], rg[[j]]$x, rg[[j]]$y), TRUE))
  inn <- matrix(inn, length(rg))                              # inn[j, i]: ring i lies inside ring j
  depth <- colSums(inn); outer <- which(depth %% 2 == 0)
  part <- integer(length(rg)); part[outer] <- seq_along(outer)
  for (i in which(depth %% 2 == 1)) { j <- which(inn[, i] & depth == depth[i] - 1L); part[i] <- part[j[1]] }
  ring <- integer(length(rg)); for (p in unique(part)) { k <- which(part == p & depth %% 2 == 1); ring[k] <- seq_along(k) }
  rbindlist(lapply(seq_along(rg), function(i) rg[[i]][, `:=`(part = part[i], ring = ring[i])]))[, .(part, ring, x, y)][order(part, ring)]
}

fp_outline_area <- function(o) if (!nrow(o)) 0 else o[, .(a = fp_area(x, y) * fifelse(ring[1] == 0L, 1, -1)), by = .(part, ring)][, sum(a)]

# Rings as text, as the other map lines ("lat lon;lat lon;..."), one row per ring.
fp_lines <- function(o, lat0, lon0) {
  if (!nrow(o)) return(data.table(part = integer(), ring = integer(), line = character()))
  o[, { p <- fp_ll(c(x, x[1]), c(y, y[1]), lat0, lon0); .(line = paste(sprintf("%.7f %.7f", p$lat, p$lon), collapse = ";")) }, by = .(part, ring)]
}
fp_parse <- function(line) { v <- strsplit(strsplit(line, ";", fixed = TRUE)[[1]], " ", fixed = TRUE)
  list(lat = as.numeric(vapply(v, `[`, "", 1L)), lon = as.numeric(vapply(v, `[`, "", 2L))) }

# ---- heights --------------------------------------------------------------------------------------------------------------
# Each image's height above the take-off ground, and where it came from (a flight-level text). img: flight_id, alt,
# alt_rel, family. emo: the eMotion records (tracks and photos by source_id:rel); fl: flights with log_ref; bases: one row
# per flight with a base ground height (base_ground_m). DJI: alt_rel. eBee: the log's photos less its take-off, applied to
# each image as the flight's median plus the image's own difference from its flight's median altitude (so the image
# altitude's reference cancels out).
fp_heights <- function(img, fl, emo = NULL, bases = NULL) {
  h <- data.table(flight_id = unique(img$flight_id), h_med = NA_real_, from = "")
  tr <- emo$tracks; ph <- emo$photos
  if (!is.null(fl) && "log_ref" %in% names(fl) && !is.null(ph) && nrow(ph) && "alt_wgs84" %in% names(ph)) {
    lr <- fl[!is.na(log_ref), .(flight_id, ref = log_ref)]
    pm <- ph[is.finite(alt_wgs84), .(ph_wgs = stats::median(alt_wgs84)), by = .(ref = paste0(source_id, ":", rel))]
    to <- if (!is.null(tr) && nrow(tr) && "alt_wgs84" %in% names(tr))
            tr[is.finite(alt_wgs84)][order(utc), .(to_wgs = stats::median(utils::head(alt_wgs84, 5L))), by = .(ref = paste0(source_id, ":", rel))] else data.table(ref = character(), to_wgs = numeric())
    lr <- merge(merge(lr, pm, by = "ref"), to, by = "ref", all.x = TRUE)
    if (!is.null(bases) && nrow(bases)) lr <- merge(lr, bases[, .(flight_id, base_ground_m)], by = "flight_id", all.x = TRUE) else lr[, base_ground_m := NA_real_]
    lr[, `:=`(hh = fifelse(is.finite(to_wgs), ph_wgs - to_wgs, ph_wgs - base_ground_m),
              src = fifelse(is.finite(to_wgs), "flight log: photo GNSS height less take-off height", "flight log photo height less the GNSS base's ground height"))]
    h[lr[is.finite(hh) & hh > 0], `:=`(h_med = i.hh, from = i.src), on = "flight_id"]
  }
  d <- img[, .(flight_id, alt, alt_rel)]
  d[, alt_med := stats::median(alt, na.rm = TRUE), by = flight_id]
  d[h, `:=`(h_med = i.h_med, from = i.from), on = "flight_id"]
  d[, height := fifelse(is.finite(alt_rel) & alt_rel > 0, alt_rel, h_med + fifelse(is.finite(alt) & is.finite(alt_med), alt - alt_med, 0))]
  d[is.finite(alt_rel) & alt_rel > 0, from := "DJI height above take-off"]
  d[!is.finite(height) | height <= 0, `:=`(height = NA_real_, from = "")]
  list(height = d$height, from = d[, .(from = { f <- from[nzchar(from)]; if (length(f)) f[1] else "" }), by = flight_id])
}

# The GNSS base's ground height for flights tied to a base RINEX file with an approximate position: its ellipsoidal height
# less the antenna height (RINEX ANTENNA: DELTA H/E/N). A RINEX file counts as a base when config/gnss_bases.csv lists it
# (file) or its path names a base; a rover's file would give the aircraft's height, not the ground's. links: confirmed or
# automatic links; rinex: the header table (rinex_h, rinex_ant_h; scans made before 2026-10-26 have neither until rescanned).
fp_base_ground <- function(links, rinex, bases = NULL) {
  if (is.null(rinex) || !nrow(rinex) || !all(c("rinex_h", "rinex_ant_h") %in% names(rinex)) || is.null(links) || !nrow(links)) return(data.table(flight_id = character(), base_ground_m = numeric()))
  listed <- if (!is.null(bases) && nrow(bases) && "file" %in% names(bases)) tolower(basename(bases$file[nzchar(bases$file)])) else character()
  r <- rinex[is.finite(rinex_h) & (tolower(basename(rel)) %in% listed | grepl("base", rel, ignore.case = TRUE)),
             .(file = paste0(source_id, ":", rel), g = rinex_h - fifelse(is.finite(rinex_ant_h), rinex_ant_h, 0))]
  l <- links[state %in% c("auto", "probable", "confirmed") & format == "rinex", .(flight_id, file)]
  merge(l, r, by = "file")[, .(base_ground_m = stats::median(g)), by = flight_id]
}

# ---- one footprint per flight ------------------------------------------------------------------------------------------
# img: one flight's images with lat, lon, t (s), height, sensor_w_mm, focal_mm, px_x, px_y, model, file. lidar_fov: the
# flight's LiDAR field of view (NA for a camera flight). Returns list(outline = part, ring, line; summary row).
flight_footprint <- function(img, lidar_fov = NA_real_, cell = FOOTPRINT_CELL_M) {
  d <- img[is.finite(lat) & is.finite(lon) & !(lat == 0 & lon == 0) & is.finite(height) & height > 0]
  lidar <- is.finite(lidar_fov)
  row <- data.table(fp_kind = if (lidar) "lidar" else "optical", fp_area_m2 = NA_real_, fp_height_m = NA_real_, fp_width_m = NA_real_, fp_gsd_cm = NA_real_,
                    fp_images = nrow(d), fp_cell_m = NA_real_, fp_note = "")
  if (!lidar) d <- d[is.finite(sensor_w_mm) & is.finite(focal_mm) & focal_mm > 0 & is.finite(px_x) & is.finite(px_y) & px_x > 0]
  if (nrow(d) < 2L) { row[, `:=`(fp_images = nrow(d), fp_note = if (nrow(d)) "too few placed images with a height" else "no placed images with a height")]
    return(list(outline = data.table(part = integer(), ring = integer(), line = character()), summary = row)) }
  d <- unique(d[order(t, file)], by = c("lat", "lon"))       # the bands of a capture share a position
  lat0 <- stats::median(d$lat); lon0 <- stats::median(d$lon); p <- fp_xy(d$lat, d$lon, lat0, lon0)
  d[, `:=`(x = p$x, y = p$y)][, hd := fp_heading(x, y, as.numeric(t))]
  if (lidar) {                                               # a strip between each position and the next on its line
    w <- 2 * d$height * tan(lidar_fov / 2 * pi / 180); n <- nrow(d); i <- seq_len(n - 1L)
    gap <- sqrt(diff(d$x)^2 + diff(d$y)^2); dt <- diff(as.numeric(d$t))
    k <- i[(is.na(dt) | dt <= FOOTPRINT_LINE_GAP_S) & gap > 0 & gap <= 3 * stats::median(gap[gap > 0])]
    q <- fp_rects(k, (d$x[k] + d$x[k + 1L]) / 2, (d$y[k] + d$y[k + 1L]) / 2, (w[k] + w[k + 1L]) / 2, gap[k] + cell, atan2(d$y[k + 1L] - d$y[k], d$x[k + 1L] - d$x[k]))
    row[, fp_width_m := round(stats::median(w), 2)]
  } else {
    w <- d$sensor_w_mm * d$height / d$focal_mm; l <- w * d$px_y / d$px_x
    q <- fp_rects(seq_len(nrow(d)), d$x, d$y, w, l, fifelse(is.na(d$hd), 0, d$hd))
    row[, fp_width_m := round(stats::median(w), 2)]
  }
  if (!nrow(q)) { row[, fp_note := "no flown lines found"]; return(list(outline = data.table(part = integer(), ring = integer(), line = character()), summary = row)) }
  gr <- fp_grid(q$x, q$y, cell)
  o <- fp_trace(fp_count(q[, .(g = id, r = 1L, x, y)], gr), gr)
  g <- d$sensor_w_mm * d$height * 100 / (d$focal_mm * d$px_x); g <- g[is.finite(g) & g > 0]   # GSD (cm) at the height above the take-off ground
  row[, `:=`(fp_area_m2 = round(fp_outline_area(o)), fp_height_m = round(stats::median(d$height), 1), fp_cell_m = round(gr$cell, 2),
             fp_gsd_cm = if (length(g)) round(stats::median(g), 2) else NA_real_)]
  list(outline = fp_lines(o, lat0, lon0), summary = row)
}

# Every flight's footprint: catalogue/footprints.csv (flight_id, kind, part, ring, line) and the flight columns fp_kind,
# fp_area_m2, fp_height_m, fp_height_from, fp_width_m (photo width across the track, or the LiDAR swath), fp_gsd_cm (the
# camera's GSD at that height; for an eBee the only GSD there is), fp_images, fp_cell_m, fp_note. A flight is LiDAR when it has LiDAR raw files and its payload's camera has a field of view in
# LIDAR_FOV_DEG; its footprint is then the scanned strip, otherwise its photos.
footprints_build <- function(img, fl, emo = NULL, links = NULL, rinex = NULL, bases = NULL) {
  need <- c("flight_id", "lat", "lon", "alt", "alt_rel", "sensor_w_mm", "focal_mm", "px_x", "px_y", "model", "t", "file")
  img <- copy(img); for (k in setdiff(need, names(img))) set(img, j = k, value = NA)
  for (k in c("lat", "lon", "alt", "alt_rel", "sensor_w_mm", "focal_mm", "px_x", "px_y")) set(img, j = k, value = suppressWarnings(as.numeric(img[[k]])))
  img <- img[!is.na(flight_id) & flight_id %in% fl$flight_id]
  hz <- fp_heights(img, fl, emo, fp_base_ground(links, rinex, bases)); img[, height := hz$height]
  lid <- fl[, .(flight_id, lidar = if ("has_lidar_raw" %in% names(fl)) has_lidar_raw %in% TRUE else FALSE)]
  fov <- img[, .(fov = { m <- names(LIDAR_FOV_DEG)[names(LIDAR_FOV_DEG) %in% model]; if (length(m)) LIDAR_FOV_DEG[[m[1]]] else NA_real_ }), by = flight_id]
  lid <- merge(lid, fov, by = "flight_id", all.x = TRUE)[, fov := fifelse(lidar, fov, NA_real_)]
  ids <- unique(img$flight_id)
  r <- lapply(ids, function(f) { x <- flight_footprint(img[flight_id == f], lid[flight_id == f, fov][1])
    list(o = if (nrow(x$outline)) x$outline[, `:=`(flight_id = f, kind = x$summary$fp_kind)], s = x$summary[, flight_id := f]) })
  sm <- rbindlist(lapply(r, `[[`, "s"), fill = TRUE)
  sm <- merge(sm, hz$from[, .(flight_id, fp_height_from = from)], by = "flight_id", all.x = TRUE)
  out <- rbindlist(lapply(r, `[[`, "o"), fill = TRUE)
  if (!nrow(out)) out <- data.table(flight_id = character(), kind = character(), part = integer(), ring = integer(), line = character())
  list(outlines = out[, .(flight_id, kind, part, ring, line)], flights = sm)
}

# ---- one footprint per year (the app) ----------------------------------------------------------------------------------
# fp: footprint rows (flight_id, kind, part, ring, line) of the flights shown. Returns the joined outline as rings of
# lat/lon, its area, the area under 1, 2, ... flights, and the cell size used.
footprint_union <- function(fp, cell = FOOTPRINT_CELL_M) {
  empty <- list(rings = data.table(part = integer(), ring = integer(), line = character()), area_m2 = 0, sum_m2 = 0, by_n = data.table(n = integer(), m2 = numeric()), cell = cell, parts = 0L, holes = 0L)
  if (is.null(fp) || !nrow(fp)) return(empty)
  pts <- fp[, { p <- fp_parse(line); .(lat = p$lat, lon = p$lon) }, by = .(flight_id, part, ring)]
  lat0 <- stats::median(pts$lat); lon0 <- stats::median(pts$lon); p <- fp_xy(pts$lat, pts$lon, lat0, lon0); pts[, `:=`(x = p$x, y = p$y)]
  pts <- pts[, .SD[-.N], by = .(flight_id, part, ring)]       # the closing point
  gr <- fp_grid(pts$x, pts$y, cell)
  cnt <- fp_count(pts[, .(g = flight_id, r = paste(part, ring), x, y)], gr)
  o <- fp_trace(cnt, gr)
  sum_m2 <- pts[, .(a = fp_area(x, y) * fifelse(ring[1] == 0L, 1, -1)), by = .(flight_id, part, ring)][, sum(a)]
  tb <- tabulate(cnt[cnt > 0]); by_n <- data.table(n = seq_along(tb), m2 = tb * gr$cell^2)[m2 > 0]
  list(rings = fp_lines(o, lat0, lon0), area_m2 = fp_outline_area(o), sum_m2 = sum_m2, by_n = by_n, cell = gr$cell,
       parts = if (nrow(o)) uniqueN(o$part) else 0L, holes = if (nrow(o)) uniqueN(o[ring > 0, paste(part, ring)]) else 0L)
}
