# R/stac.R  --  Frames and datums of every dataset, records of derived products, coverage by site and year, and a STAC catalogue.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R; not meant to be run on its own.
#
# Nothing here changes the catalogue's own tables beyond adding columns: frames are read from what a file states, products are
# what people record in config/products.csv, and the STAC catalogue is written beside the catalogue (output/stac/).

# ---- Frames and datums ---------------------------------------------------------------------------------------------------------
# What a dataset's coordinates are in, from its EPSG code or the coordinate system's name: the horizontal frame (WGS 84,
# NAD83, NAD83(CSRS)) and whether the file says enough to compare it with surveys at the centimetre scale. A file that states
# no system is "datum unknown". Heights are never stated by these files' headers, so height_type is left to the user.
frame_of_crs <- function(epsg = NA, text = "") {
  e <- suppressWarnings(as.integer(epsg)); t <- toupper(text %||% ""); t[is.na(t)] <- ""
  out <- rep("", length(e))
  csrs <- e %in% c(4617L, 2955L, 2956L, 2957L, 3154L, 3155L, 3156L, 3157L, 3158L, 3159L, 3160L, 22207L:22222L) | grepl("CSRS", t)
  nad <- (e %in% c(4269L) | (e >= 26901L & e <= 26923L) | grepl("NAD83|NAD_1983|NORTH_AMERICAN_1983", t)) & !csrs
  wgs <- e %in% 4326L | (e >= 32601L & e <= 32660L) | (e >= 32701L & e <= 32760L) | grepl("WGS[ _]?84|WGS_1984", t)
  out[wgs] <- "WGS 84"; out[nad] <- "NAD83 (version not stated)"; out[csrs] <- "NAD83(CSRS)"
  out
}
crs_status <- function(epsg = NA, text = "") {
  has_e <- !is.na(suppressWarnings(as.integer(epsg))); has_t <- !is.na(text) & nzchar(trim(text))
  fr <- frame_of_crs(epsg, text)
  ifelse(!has_e & !has_t, "datum unknown",
         ifelse(fr == "", "coordinate system stated, frame not recognised",
                ifelse(fr == "NAD83(CSRS)", "frame stated (NAD83(CSRS)); epoch not stated", paste0("frame stated (", fr, "); epoch not stated"))))
}

# ---- Derived products ------------------------------------------------------------------------------------------------------------
# config/products.csv: one row per product made from the flights (an orthomosaic, a DSM, a point cloud, a reflectance map):
# where it is, which flights went into it, the software and its settings, the control and check points, and the accuracy
# the processing report gives. The level follows the drone processing levels (Scientific Data FAIR framework for UAV data,
# after NASA): L1 calibrated (reflectance), L2 corrected and mapped (orthomosaic, DSM, DTM, point cloud), L4 derived (an
# index, a classification, a change map). A blank level is taken from the kind.
PRODUCT_KINDS <- c(orthomosaic = 2L, dsm = 2L, dtm = 2L, `point cloud` = 2L, reflectance = 1L, index = 4L, classification = 4L, `change map` = 4L)
products_table <- function(prod, ds = NULL, flights = NULL) {
  if (is.null(prod) || !nrow(prod)) return(ensure_cols(data.table(), c(PRODUCT_COLS, "level_name", "flights_found", "file_found", "crs_status"))[0])
  p <- ensure_cols(copy(prod), PRODUCT_COLS)
  lv <- suppressWarnings(as.integer(sub("^L", "", trim(p$level))))
  lv[is.na(lv)] <- PRODUCT_KINDS[tolower(trim(p$kind[is.na(lv)]))]
  p[, level := ifelse(is.na(lv), "", paste0("L", lv))][, level_name := ifelse(is.na(lv), "", PROC_LEVELS$name[match(lv, PROC_LEVELS$level)])]
  ids <- if (!is.null(flights) && nrow(flights)) flights$flight_id else character()
  p[, flights_found := vapply(strsplit(flights, "[;, ]+"), function(f) { f <- f[nzchar(f)]; if (!length(f)) "" else sprintf("%d of %d", sum(f %in% ids), length(f)) }, "")]
  hit <- if (!is.null(ds) && nrow(ds)) match(trim(p$file), ds$rel) else rep(NA_integer_, nrow(p))
  p[, file_found := !is.na(hit)]
  e <- ifelse(nzchar(trim(p$epsg)), p$epsg, if (!is.null(ds) && nrow(ds)) as.character(ds$epsg[hit]) else NA)
  p[, crs_status := ifelse(nzchar(trim(frame)), paste0("frame given (", frame, if_else_chr(nzchar(trim(epoch)), paste0(", epoch ", epoch), ", epoch not given"), ")"),
                           crs_status(e, if (!is.null(ds) && nrow(ds)) ds$crs[hit] else ""))]
  p[]
}
if_else_chr <- function(cond, yes, no) ifelse(cond, yes, no)

# ---- Coverage --------------------------------------------------------------------------------------------------------------------
# For planning change detection: per site and year, the flights by sensor (RGB, multispectral, LiDAR), the finest ground
# sample distance and how they were positioned; and per year and platform, the scenes and tiles of the satellite, aerial and
# LiDAR list. A site with flights in one year and none in another is where a comparison is missing.
coverage <- function(flights, ds = NULL) {
  a <- if (!is.null(flights) && nrow(flights)) {
    f <- copy(flights)
    f[, sensor_class := fifelse(multispectral %in% TRUE, "multispectral", fifelse(has_lidar_raw %in% TRUE | grepl("L1|L2|Zenmuse L|lidar", paste(sensor, aircraft), ignore.case = TRUE), "LiDAR", "RGB"))]
    f[, corrected := positioning_status %in% c("RTK corrected", "PPK corrected", "partly RTK corrected")]
    f[, .(scope = "flights", count = .N, images = sum(n_images, na.rm = TRUE), finest_gsd_cm = if ("gsd_cm_median" %in% names(f) && any(is.finite(gsd_cm_median))) round(min(gsd_cm_median, na.rm = TRUE), 2) else NA_real_,
          corrected_pct = round(100 * mean(corrected), 0), first = min(date), last = max(date)),
      by = .(site = fifelse(is.na(site) | !nzchar(site), "(no site)", site), year, kind = sensor_class)] } else NULL
  b <- if (!is.null(ds) && nrow(ds)) {
    d <- copy(ds); if (!"year" %in% names(d)) d[, year := suppressWarnings(as.integer(substr(acquired, 1, 4)))]
    d[, .(scope = "datasets", count = .N, images = NA_integer_, finest_gsd_cm = if (any(is.finite(gsd_m))) round(100 * min(gsd_m, na.rm = TRUE), 2) else NA_real_,
          corrected_pct = NA_real_, first = suppressWarnings(min(acquired, na.rm = TRUE)), last = suppressWarnings(max(acquired, na.rm = TRUE))),
      by = .(year, kind = fifelse(is.na(platform), kind, platform))][, site := "(any)"] } else NULL
  out <- rbind(a, b, fill = TRUE)
  if (is.null(out) || !nrow(out)) return(data.table(scope = character(), site = character(), year = integer(), kind = character(), count = integer()))
  setcolorder(out, c("scope", "site", "year", "kind"))[order(scope, site, year, kind)]
}

# ---- STAC ---------------------------------------------------------------------------------------------------------------------
# A static STAC 1.0 catalogue (an OGC community standard): catalog.json, one collection per project (flights) and one for
# the satellite, aerial and LiDAR list and one for products, and one item per flight, scene or tile and product, with the
# projection, electro-optical and view extensions where they apply (a LiDAR tile's count and density are ovid: fields, since
# the point cloud extension also asks for the encoding and point schema, which the catalogue does not read). Assets point at the files as they sit in
# their source (source_id:path), since the archive is not served; a catalogue can be browsed with any STAC viewer.
STAC_EXT <- c(proj = "https://stac-extensions.github.io/projection/v1.1.0/schema.json", eo = "https://stac-extensions.github.io/eo/v1.1.0/schema.json",
              view = "https://stac-extensions.github.io/view/v1.0.0/schema.json")
stac_bbox <- function(lon_min, lat_min, lon_max, lat_max) if (all(is.finite(c(lon_min, lat_min, lon_max, lat_max)))) c(lon_min, lat_min, lon_max, lat_max) else NULL
stac_geom <- function(b) if (is.null(b)) NULL else list(type = "Polygon", coordinates = list(list(c(b[1], b[2]), c(b[3], b[2]), c(b[3], b[4]), c(b[1], b[4]), c(b[1], b[2]))))
# A time as STAC wants it, or NULL. Blank, missing or unreadable times give NULL (as.POSIXct stops on "" rather than giving NA);
# ISO times with T and Z, fractional seconds, slashes, and a year or year-month alone (the start of that year or month) are read.
stac_time <- function(t) {
  if (inherits(t, "POSIXct")) return(if (length(t) && !is.na(t[1])) format(t[1], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NULL)
  t <- trim(as.character(t %||% NA)[1]); if (is.na(t) || !nzchar(t)) return(NULL)
  t <- sub("Z$", "", sub("^(\\d{4}-\\d{2}-\\d{2})T", "\\1 ", t))
  if (grepl("^\\d{4}$", t)) t <- paste0(t, "-01-01") else if (grepl("^\\d{4}-\\d{2}$", t)) t <- paste0(t, "-01")
  x <- tryCatch(suppressWarnings(as.POSIXct(t, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d", "%Y/%m/%d %H:%M:%OS", "%Y/%m/%d"))),
                error = function(e) NA)
  if (is.na(x)) NULL else format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") }
# A year alone (inferred from a folder or file name) is a span: the whole year, as STAC start and end times.
stac_year_span <- function(t) { t <- trim(as.character(t %||% NA)[1]); if (is.na(t) || !grepl("^\\d{4}$", t)) NULL else list(start = paste0(t, "-01-01T00:00:00Z"), end = paste0(t, "-12-31T23:59:59Z")) }
stac_id <- function(x) gsub("[^A-Za-z0-9_.-]+", "-", x)
write_stac_json <- function(x, f) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); jsonlite::write_json(x, f, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA) }

stac_export <- function(dir, flights, ds = NULL, products = NULL, title = "ovid RS archive") {
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  cols <- list(); n_items <- 0L
  add_item <- function(coll, it) { cols[[coll$id]]$items[[length(cols[[coll$id]]$items) + 1]] <<- it }
  coll_of <- function(id, ttl, desc) { if (is.null(cols[[id]])) cols[[id]] <<- list(id = id, title = ttl, description = desc, items = list()); cols[[id]] }
  if (!is.null(flights) && nrow(flights)) for (i in seq_len(nrow(flights))) { f <- flights[i]
    b <- stac_bbox(f$lon_min, f$lat_min, f$lon_max, f$lat_max); dt <- stac_time(f$start_utc); if (is.null(b) || is.null(dt)) next
    pj <- if (nzchar(f$project %||% "")) f$project else "unassigned"
    cl <- coll_of(stac_id(paste0("flights-", pj)), paste("Drone flights,", pj), paste("UAV flights of project", pj, "in the Ovid RS catalogue."))
    props <- list(datetime = dt, start_datetime = dt, end_datetime = stac_time(f$end_utc) %||% dt, platform = f$aircraft %||% NULL, instruments = if (nzchar(f$sensor %||% "")) list(f$sensor) else NULL,
                  gsd = if (is.finite(f$gsd_cm_median %||% NA)) round(f$gsd_cm_median / 100, 4) else NULL, `proj:epsg` = 4326L,
                  `view:sun_elevation` = if (is.finite(f$sun_elev_max_deg %||% NA)) round(f$sun_elev_max_deg, 1) else NULL,
                  `ovid:site` = f$site %||% "", `ovid:images` = f$n_images, `ovid:positioning` = f$positioning_status %||% "", `ovid:mission` = f$mission %||% "",
                  `ovid:processing_level` = "L0 raw", `ovid:qa_flags` = f$qa_flags %||% "")
    props <- props[!vapply(props, is.null, logical(1))]
    add_item(cl, list(type = "Feature", stac_version = "1.0.0", stac_extensions = I(unname(STAC_EXT[c("proj", if (!is.null(props$`view:sun_elevation`)) "view")])),
                      id = stac_id(f$flight_id), bbox = b, geometry = stac_geom(b), properties = props,
                      assets = list(images = list(href = paste0(f$source_id, ":", f$folder), title = paste(f$n_images, "images"), roles = list("data"))))) }
  if (!is.null(ds) && nrow(ds)) for (i in seq_len(nrow(ds))) { d <- ds[i]
    b <- stac_bbox(d$lon_min, d$lat_min, d$lon_max, d$lat_max); dt <- stac_time(d$acquired); if (is.null(b) || is.null(dt)) next
    cl <- coll_of("imagery-and-lidar", "Satellite, aerial and LiDAR", "Scenes, photographs, LiDAR tiles and rasters in the Ovid RS catalogue.")
    lidar <- identical(d$kind, "lidar")
    ys <- stac_year_span(d$acquired)
    props <- list(datetime = dt, start_datetime = ys$start, end_datetime = ys$end, platform = d$platform %||% NULL, gsd = if (is.finite(d$gsd_m %||% NA)) d$gsd_m else NULL,
                  `proj:epsg` = if (!is.na(suppressWarnings(as.integer(d$epsg)))) as.integer(d$epsg) else NULL,
                  `eo:cloud_cover` = if (is.finite(d$cloud_pct %||% NA)) d$cloud_pct else NULL,
                  `ovid:point_count` = if (lidar && is.finite(d$n_points %||% NA)) d$n_points else NULL,
                  `ovid:point_density` = if (lidar && is.finite(d$point_density %||% NA)) d$point_density else NULL,
                  `ovid:product` = d$product %||% "", `ovid:crs_status` = crs_status(d$epsg, d$crs))
    props <- props[!vapply(props, is.null, logical(1))]
    ext <- c(if (!is.null(props$`proj:epsg`)) "proj", if (!is.null(props$`eo:cloud_cover`)) "eo")
    add_item(cl, list(type = "Feature", stac_version = "1.0.0", stac_extensions = I(unname(STAC_EXT[ext])), id = stac_id(paste(d$source_id %||% "", d$rel)),
                      bbox = b, geometry = stac_geom(b), properties = props, assets = list(data = list(href = paste0(d$source_id %||% "", ":", d$rel), roles = list("data"))))) }
  if (!is.null(products) && nrow(products)) for (i in seq_len(nrow(products))) { p <- products[i]
    fl <- strsplit(p$flights, "[;, ]+")[[1]]; fl <- fl[nzchar(fl)]; ff <- if (!is.null(flights) && nrow(flights)) flights[flight_id %in% fl] else data.table()
    b <- if (nrow(ff)) stac_bbox(min(ff$lon_min), min(ff$lat_min), max(ff$lon_max), max(ff$lat_max)) else NULL
    dt <- stac_time(p$made) %||% (if (nrow(ff)) stac_time(min(ff$start_utc)) else NULL); if (is.null(dt)) next
    cl <- coll_of("products", "Products", "Orthomosaics, surface models, point clouds and maps made from the flights, with how they were made.")
    props <- list(datetime = dt, `ovid:kind` = p$kind, `ovid:processing_level` = paste(p$level, p$level_name), `ovid:flights` = I(fl),
                  `ovid:software` = trim(paste(p$software, p$version)), `ovid:parameters` = p$parameters, `ovid:gcps` = p$gcps, `ovid:checkpoints` = p$checkpoints,
                  `ovid:rmse_h_m` = suppressWarnings(as.numeric(p$rmse_h_m)), `ovid:rmse_v_m` = suppressWarnings(as.numeric(p$rmse_v_m)),
                  `proj:epsg` = if (!is.na(suppressWarnings(as.integer(p$epsg)))) as.integer(p$epsg) else NULL, `ovid:crs_status` = p$crs_status)
    props <- props[!vapply(props, function(v) is.null(v) || (length(v) == 1 && (is.na(v) || identical(v, ""))), logical(1))]
    add_item(cl, list(type = "Feature", stac_version = "1.0.0", stac_extensions = I(unname(STAC_EXT[if (!is.null(props$`proj:epsg`)) "proj"])), id = stac_id(p$product_id),
                      bbox = b, geometry = stac_geom(b), properties = props, assets = list(data = list(href = p$file, roles = list("data"))))) }
  root <- list(type = "Catalog", stac_version = "1.0.0", id = "ovid-rs", title = title, description = "Drone flights, imagery, LiDAR and products catalogued by Ovid RS.",
               links = c(list(list(rel = "root", href = "./catalog.json", type = "application/json"), list(rel = "self", href = "./catalog.json", type = "application/json")),
                         lapply(names(cols), function(k) list(rel = "child", href = paste0("./", k, "/collection.json"), type = "application/json"))))
  write_stac_json(root, file.path(dir, "catalog.json"))
  for (k in names(cols)) { cl <- cols[[k]]; its <- cl$items
    bb <- do.call(rbind, lapply(its, function(it) it$bbox)); tt <- sort(vapply(its, function(it) it$properties$datetime, ""))
    write_stac_json(list(type = "Collection", stac_version = "1.0.0", id = k, title = cl$title, description = cl$description, license = "proprietary",
                         extent = list(spatial = list(bbox = list(if (!is.null(bb)) c(min(bb[, 1]), min(bb[, 2]), max(bb[, 3]), max(bb[, 4])) else c(-180, -90, 180, 90))),
                                       temporal = list(interval = list(c(tt[1], tt[length(tt)])))),
                         links = c(list(list(rel = "root", href = "../catalog.json", type = "application/json"), list(rel = "parent", href = "../catalog.json", type = "application/json"),
                                        list(rel = "self", href = "./collection.json", type = "application/json")),
                                   lapply(its, function(it) list(rel = "item", href = paste0("./", it$id, "/", it$id, ".json"), type = "application/geo+json")))),
                    file.path(dir, k, "collection.json"))
    for (it in its) { it$collection <- k
      it$links <- list(list(rel = "root", href = "../../catalog.json", type = "application/json"), list(rel = "parent", href = "../collection.json", type = "application/json"),
                       list(rel = "collection", href = "../collection.json", type = "application/json"), list(rel = "self", href = paste0("./", it$id, ".json"), type = "application/geo+json"))
      write_stac_json(it, file.path(dir, k, it$id, paste0(it$id, ".json"))); n_items <- n_items + 1L } }
  invisible(list(dir = dir, collections = length(cols), items = n_items))
}
