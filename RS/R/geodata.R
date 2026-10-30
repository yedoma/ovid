# R/geodata.R  --  Other geodata (satellite, aerial, LiDAR and raster files, GIS layers) that belongs with the flights, and what is
# set aside. A drive often holds geodata from other work (national layers, thesis maps, map caches): by default a folder is
# catalogued only when it holds or sits beside flight photos, or lies within spatial_max_km of a flight; the rest is set aside
# (catalogue/set_aside.csv) with the reason, never dropped or changed, and one Keep (review_decisions.csv) brings a folder back.
# Decided when the catalogue is built, from the sources' last scans: changing a limit or keeping a folder needs no rescan.
# Distances: the gap between a folder's box and a flight's box, in km by the equirectangular approximation (Snyder 1987).
# Part of Ovid Core // RS. Sourced by rs_pipeline.R.

KM_PER_DEG <- 111.32                                   # one degree of latitude, km (WGS 84 mean; Snyder 1987)
GEODATA_KINDS <- c(gis = "GIS layers", imagery = "satellite, aerial, LiDAR and raster files")

# km between two boxes (0 when they touch); a, b: lat_min, lat_max, lon_min, lon_max (b may hold several boxes)
box_gap_km <- function(a1, a2, o1, o2, b1, b2, p1, p2) {
  dlat <- pmax(0, a1 - b2, b1 - a2); dlon <- pmax(0, o1 - p2, p1 - o2)
  mid <- ((a1 + a2) / 2 + (b1 + b2) / 2) / 2
  sqrt((dlat * KM_PER_DEG)^2 + (dlon * KM_PER_DEG * cos(mid * pi / 180))^2)
}
box_size_km <- function(a1, a2, o1, o2) sqrt(((a2 - a1) * KM_PER_DEG)^2 + ((o2 - o1) * KM_PER_DEG * cos((a1 + a2) / 2 * pi / 180))^2)

# One row per folder and kind: files, km to the nearest flight, extent, and keep (TRUE) or the reason it is set aside.
# fl: flights with lat_min..lon_max; fdirs: source_id and folder of the folders holding flight photos; kept: review ids kept
# (geodata_id()); on: c(gis = TRUE, imagery = TRUE); max_km, max_extent_km: the limits (calibration options).
geodata_review <- function(ds, gl, fl, fdirs, kept = character(), on = c(gis = TRUE, imagery = TRUE), max_km = 5, max_extent_km = 200) {
  num <- function(x) suppressWarnings(as.numeric(x))
  pick <- function(d, kind, group) if (is.null(d) || !nrow(d)) NULL else
    d[, .(source_id = as.character(source_id), rel = as.character(rel), folder = as.character(folder), kind = if (is.null(kind)) as.character(d$kind) else kind, group = group,
          lat_min = num(lat_min), lat_max = num(lat_max), lon_min = num(lon_min), lon_max = num(lon_max))]
  d <- rbindlist(list(pick(ds, NULL, "imagery"), pick(gl, "GIS layer", "gis")), fill = TRUE)
  if (is.null(d) || !nrow(d)) return(list(files = data.table(source_id = character(), rel = character(), kind = character(), folder = character(), keep = logical(), why = character(), km = numeric()),
                                          folders = data.table(source_id = character(), folder = character(), kind = character(), files = integer(), keep = logical(), why = character(), km = numeric())))
  d[is.na(folder), folder := ""]
  fb <- if (!is.null(fl) && nrow(fl)) fl[is.finite(num(lat_min)) & is.finite(num(lon_min)) & !(num(lat_min) == 0 & num(lon_min) == 0),
                                        .(b1 = num(lat_min), b2 = num(lat_max), p1 = num(lon_min), p2 = num(lon_max))] else data.table()
  # each file on its own (one wide layer in a folder must not carry the others with it); beside and kept go by its folder
  d[, km := if (nrow(fb)) vapply(seq_len(.N), function(i) if (!is.finite(lat_min[i]) || !is.finite(lon_min[i])) NA_real_
                                 else min(box_gap_km(lat_min[i], lat_max[i], lon_min[i], lon_max[i], fb$b1, fb$b2, fb$p1, fb$p2)), 0) else NA_real_]
  d[, extent := box_size_km(lat_min, lat_max, lon_min, lon_max)]
  # beside flight photos: the same folder, one inside it, or one it is inside (a drone mission's products and its photos)
  fd <- if (!is.null(fdirs) && nrow(fdirs)) unique(fdirs[, .(source_id = as.character(source_id), folder = as.character(folder))]) else data.table(source_id = character(), folder = character())
  g <- unique(d[, .(source_id, folder, kind)])
  g[, beside := vapply(seq_len(.N), function(i) { f <- fd$folder[fd$source_id == source_id[i]]; x <- folder[i]
    length(f) > 0 && any(f == x | startsWith(f, paste0(x, "/")) | startsWith(x, paste0(f, "/")) | (!nzchar(x) & nzchar(f))) }, NA)]
  g[, ok := beside | geodata_id(source_id, folder, kind) %in% kept | geodata_id("*", "*", "*") %in% kept]
  d <- g[d, on = .(source_id, folder, kind)]
  d[, why := fifelse(!on[group], paste(GEODATA_KINDS[group], "are switched off for this project"),
             fifelse(ok, "",
             fifelse(!is.finite(lat_min) | !is.finite(lon_min), "no position read",
             fifelse(is.finite(extent) & extent > max_extent_km, sprintf("covers %.0f km across: a basemap or national layer", extent),
             fifelse(is.finite(km) & km <= max_km, "", if (nrow(fb)) sprintf("%.1f km from the nearest flight", km) else "no flight has a position to compare with")))))]
  d[, keep := !nzchar(why)]
  files <- d[, .(source_id, rel, kind, folder, keep, why, km = round(km, 1))]
  folders <- d[, .(files = .N, set_aside = sum(!keep), keep = all(keep), why = if (all(keep)) "" else names(sort(table(why[!keep]), decreasing = TRUE))[1],
                   km = suppressWarnings(round(min(km, na.rm = TRUE), 1))), by = .(source_id, folder, kind)]
  folders[!is.finite(km), km := NA_real_]
  list(files = files, folders = folders[order(keep, -files)])
}
# The review id of a folder of other geodata (config/review_decisions.csv): a folder kept is catalogued whatever its distance;
# geodata_id("*", "*", "*") keeps everything.
geodata_id <- function(source_id, folder, kind) review_hash(paste("geodata", source_id, folder, kind, sep = "|"))
