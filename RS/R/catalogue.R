# R/catalogue.R  --  Scanning sources one after another, grouping images into flights, linking each flight's other files
# with the evidence for each link, and working out how its positions were corrected.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R; not meant to be run on its own.
#
# The archive is the union of its sources. Scanning a source lists its files and reads the metadata of its images, and
# keeps both in <project>/cache/sources/<source_id>/ (image metadata per folder, redone only when a folder's files change).
# Flights, links and statuses are then rebuilt from every source's cache, so a source scanned earlier stays in the
# catalogue when it is unplugged, and a flight that appears in two sources (a backup, a copy) is one flight with copies.
#
# A flight is a run of images from one camera, split where exposures pause for longer than cfg$flight_gap_min; senseFly
# images carry their flight number, which is used instead. Its id comes from the camera and the first exposure (UTC),
# or from the flight number and first image name when the camera clock is unusable, so a copied or renamed folder is the same flight.

LINK_ROLES <- c("flight_record", "gnss_obs", "lidar_raw", "video", "flight_log", "geotags", "mission")
# Processed products (orthomosaics, DSMs, Pix4D and DJI Terra outputs) are outside the archive: listed, marked out of scope, never read.
PROCESSED_DIRS <- "(^|/)(processed|lidars|1_initial|2_densification|3_dsm_ortho|4_index|tiles|mosaic|orthomosaic|dsm|dem)(/|$)"
SAT_RX <- "(^|[/_ .-])(SPOT[0-9]|PHR1[AB]|PNEO|PLEIADES|WV0[1-4]|GE01|QB02|IK0[12]|S2[AB]_MSI|LC0[89]_|LE07_|LT05_)"   # satellite names in paths
SKIP_DIRS  <- "(^|/)(\\$RECYCLE\\.BIN|System Volume Information|\\.Trashes|\\.Spotlight-V100|\\.fseventsd|\\.TemporaryItems)(/|$)"

mode_of <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (length(x)) names(sort(table(x), decreasing = TRUE))[1] else NA_character_ }
rng <- function(x, f) { x <- x[is.finite(x)]; if (length(x)) f(x) else NA_real_ }
source_dir <- function(cfg, sid) file.path(path.expand(cfg$project_dir), "cache", "sources", safe_name(sid))

# ---- one source ----------------------------------------------------------------------------------------------------
list_source <- function(root, rules = NULL) {
  nd <- normalizePath(path.expand(root), winslash = "/", mustWork = TRUE)
  f <- list.files(nd, recursive = TRUE, full.names = TRUE)
  f <- f[!grepl(SKIP_DIRS, f) & !grepl("^(\\._|\\.DS_Store$|~\\$)", basename(f))]
  if (!length(f)) return(data.table())
  out <- data.table(path = f, rel = substring(f, nchar(nd) + 2L))
  fi <- file.info(out$path, extra_cols = FALSE)
  out[, `:=`(file = basename(rel), folder = dirname(rel), size = fi$size, mtime = fi$mtime)][folder == ".", folder := ""]
  out[, format := format_of(file)]
  out <- merge(out, FORMAT_INFO, by = "format", all.x = TRUE, sort = FALSE)
  out[is.na(role), `:=`(role = "unknown", family = "other")][, status := ""]
  out[size %in% 0, status := "empty file"]                                    # nothing to read: listed with why, never catalogued
  out[role == "processed" | grepl(PROCESSED_DIRS, folder, ignore.case = TRUE), status := "out of scope"]
  # satellite scenes are rasters, never photos: TIFF / JP2 / NITF in a folder with satellite metadata (.IMD, .TIL, DIMAP) or with
  # a satellite name are listed as rasters, and their JPG quicklooks are only listed
  sat <- out$folder %in% out[format %in% c("sat_imd", "spot_dimap") | grepl("\\.til$|^dim_.*\\.xml$", file, ignore.case = TRUE), unique(folder)] | grepl(SAT_RX, out$rel, ignore.case = TRUE)
  out[sat & role == "image" & grepl("\\.(tiff?|jp2|ntf)$", file, ignore.case = TRUE), `:=`(format = "raster", family = "dataset", role = "raster")]
  out[sat & role %in% c("image", "image_raw"), status := "satellite quicklook"]
  # a Google Earth tile pyramid (super-overlay) is thousands of small KMLs that each place one image tile: not flight
  # plans or tracks, so they are listed as map tiles and never read as records
  k <- which(out$format == "kml" & out$status == "" & grepl("\\.kml$", out$file, ignore.case = TRUE))
  if (length(k)) out[k[vapply(out$path[k], is_kml_tile, TRUE)], status := "map tile"]
  if (!is.null(rules) && nrow(rules)) for (i in seq_len(nrow(rules))) {
    hit <- grepl(rules$pattern[i], out$rel, perl = TRUE)
    if (rules$action[i] == "inner source") { out <- out[!hit]; next }       # files of a source inside this one belong to that source
    if (rules$action[i] == "ignore") out[hit, status := "ignored"] else if (rules$action[i] %in% FORMAT_INFO$role) out[hit, role := rules$action[i]]
  }
  setorder(out, rel)[]
}

is_kml_tile <- function(path) {
  # safe: sniffing only; a file that cannot be read is not that kind
  x <- tryCatch(readChar(path, 4096L, useBytes = TRUE), error = function(e) "")
  length(x) == 1L && !is.na(x) && grepl("<Lod>", x, fixed = TRUE) && grepl("<(GroundOverlay|NetworkLink)>", x) && !grepl("<(Placemark|gx:Track|LineString)>", x)
}

# Image metadata for one source: every image (and raw photo, for the drone test) is read once and kept in
# cache/sources/<id>/exif.rds by path, size and modified time, so a rescan reads only new or changed files. The cache is
# dropped when the reading code changes (STEP_HASH$images). sample_idx() and fill_sampled() thin a folder to IMAGE_SAMPLE
# images; scans no longer use them (every image is read), they are kept for quick looks and the tests.
IMAGE_SAMPLE <- 30L
sample_idx <- function(n) if (n <= 2L * IMAGE_SAMPLE || Sys.getenv("RS_FULL_READ") == "1") seq_len(n) else unique(round(seq(1, n, length.out = IMAGE_SAMPLE)))
fill_sampled <- function(r, g, i) {
  r[, meta_from := "read"]; if (length(i) == nrow(g)) return(r)
  n <- nrow(g); near <- i[pmax(1L, findInterval(seq_len(n), i))]               # the sampled image at or before each file
  out <- r[match(near, i)]; out[, c("rel", "folder", "file", "size") := g[, .(rel, folder, file, size)]]
  rest <- !(seq_len(n) %in% i); out[rest, meta_from := "interpolated"]
  for (k in intersect(c("lat", "lon", "alt", "alt_rel", "gimbal_pitch", "rtk_std_lat", "rtk_std_lon", "rtk_std_hgt"), names(out))) set(out, which(rest), k, NA)
  for (k in intersect(c("time_utc", "time_exif"), names(out))) { v <- as.numeric(r[[k]]); ok <- !is.na(v)
    if (sum(ok) >= 2L) set(out, which(rest), k, as.POSIXct(approx(i[ok], v[ok], which(rest), rule = 2)$y, origin = "1970-01-01", tz = "UTC")) }
  out }

read_source_images <- function(files, sdir, chunk = 250L) {
  if (dir.exists(file.path(sdir, "images"))) unlink(file.path(sdir, "images"), recursive = TRUE)   # the per-folder cache of earlier versions
  cf <- file.path(sdir, "exif.rds"); key <- STEP_HASH$images
  im <- files[role %in% c("image", "image_raw") & status == ""]
  if (!nrow(im)) { unlink(cf); return(data.table()) }
  im[, k := paste(rel, size, as.numeric(mtime))]
  # safe: a damaged cache is read again from the source
  old <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL); have <- if (identical(old$key, key) && is.data.table(old$rows)) old$rows[k %in% im$k] else data.table()
  todo <- im[!k %in% have$k][order(folder, file)]; got <- list(have)
  keep <- function() { x <- rbindlist(got, fill = TRUE); tmp <- paste0(cf, ".part"); saveRDS(list(key = key, rows = x), tmp); file.rename(tmp, cf); x }
  ch <- split(seq_len(nrow(todo)), (seq_len(nrow(todo)) - 1L) %/% chunk)
  for (j in seq_along(ch)) { g <- todo[ch[[j]]]
    run_tick(j, length(ch), sprintf("images %s of %s", num(max(ch[[j]])), num(nrow(todo))))   # progress and Cancel for a scan from the app (core util.R)
    r <- tryCatch(cbind(g[, .(rel, folder, file, size, k, raw = role == "image_raw")], read_image_meta(g$path)),
                  error = function(e) { problem(g$folder[1], paste("image metadata not read:", conditionMessage(e))); NULL })
    if (!is.null(r)) got[[length(got) + 1L]] <- r[, meta_from := "read"]
    if (j %% 20L == 0L) keep() }                                                 # a stopped scan keeps what it has read
  out <- if (!nrow(todo) && identical(old$key, key) && nrow(have) == nrow(old$rows)) have else keep()   # nothing new: left as it is (core output_restore drops cache files a stopped run wrote)
  say("    read ", num(nrow(todo)), " images", if (nrow(have)) paste0("; ", num(nrow(have)), " unchanged since the last scan") else "")
  out[, k := NULL][]
}

# Small records (MRK, RPT, geoinfo, KML, RINEX headers) are read every scan.
read_source_records <- function(files) {
  rd <- function(fmt, fun, only = ".") { f <- files[format == fmt & status == "" & grepl(only, file, ignore.case = TRUE)]
    rbindlist(lapply(seq_len(nrow(f)), function(i) {
      r <- tryCatch(fun(f$path[i]), error = function(e) { problem(f$rel[i], conditionMessage(e)); NULL })
      if (!is.null(r)) data.table(rel = f$rel[i], as.data.table(r))
    }), fill = TRUE) }
  list(mrk = rd("dji_mrk", read_mrk), rpt = rd("dji_rpt", read_rpt), geo = rd("emotion_geoinfo", read_geoinfo), kml = rd("kml", read_kml), p4d = rd("pix4d_project", read_p4d),
       rinex = rd("rinex", read_rinex, "\\.([0-9]{2}o|obs|rnx)$"))   # observation files only, not navigation
}

# Scans one source: lists it, reads what is new, and keeps the result in its cache folder. Returns the scan summary.
scan_source <- function(src, cfg, rules) {
  sdir <- source_dir(cfg, src$source_id); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  root <- path.expand(src$path)
  if (!dir.exists(root)) { problem(src$source_id, paste("source not found at", src$path, "(kept from its last scan)")); return(invisible(FALSE)) }
  say("  ", src$source_id, ": ", root, col = "accent")
  files <- list_source(root, rules)
  if (!nrow(files)) { problem(src$source_id, "no files found"); return(invisible(FALSE)) }
  say("    ", num(nrow(files)), " files, ", round(sum(files$size, na.rm = TRUE) / 1e9, 1), " GB: ", paste(files[, .N, by = .(role = fifelse(status %in% "map tile", "map tile", role))][order(-N)][1:min(.N, 6), paste(N, role)], collapse = ", "))
  img <- read_source_images(files, sdir)
  recs <- read_source_records(files)
  ring <- bb3_ring_path(cfg$project_dir); emotion_key_auto(files, ring)   # no .bb3 key yet: built from the source's .mis3 and .bb3
  emo <- read_source_emotion(files, sdir, ring)              # senseFly logs and eMotion missions (R/emotion_catalogue.R)
  gis <- read_source_gis(files); if (nrow(gis$layers)) say("    ", nrow(gis$layers), " GIS layers, ", nrow(gis$features), " features kept for the map")
  ds <- read_source_datasets(files); if (nrow(ds)) say("    ", nrow(ds), " satellite, aerial, LiDAR and raster files: ", paste(ds[, .N, by = kind][, paste(N, kind)], collapse = ", "))
  files[, path := NULL]
  tmp <- file.path(sdir, "scan.rds.part")   # written aside then renamed, so a stopped scan keeps the last good one
  saveRDS(list(files = files, images = img, records = recs, emotion = emo, datasets = ds, gis_layers = gis$layers, gis_features = gis$features, scanned = Sys.time(), root = root), tmp)
  file.rename(tmp, file.path(sdir, "scan.rds"))
  invisible(TRUE)
}

# Every source's last scan, stacked, with source_id in front of each relative path.
load_sources <- function(sources, cfg) {
  sc <- lapply(sources$source_id, function(sid) { f <- file.path(source_dir(cfg, sid), "scan.rds"); if (file.exists(f)) try_or(readRDS(f), where = paste(sid, "scan.rds")) })
  names(sc) <- sources$source_id; sc <- Filter(Negate(is.null), sc)
  if (!length(sc)) return(NULL)
  tag <- function(what) rbindlist(lapply(names(sc), function(sid) { d <- copy(sc[[sid]][[what]]); if (!is.null(d) && nrow(d)) d[, source_id := sid] }), fill = TRUE)
  rec <- function(what) rbindlist(lapply(names(sc), function(sid) { d <- copy(sc[[sid]]$records[[what]]); if (!is.null(d) && nrow(d)) d[, source_id := sid] }), fill = TRUE)
  list(files = tag("files"), images = tag("images"), datasets = tag("datasets"), gis_layers = tag("gis_layers"), gis_features = tag("gis_features"), records = list(mrk = rec("mrk"), rpt = rec("rpt"), geo = rec("geo"), kml = rec("kml"), p4d = rec("p4d"), rinex = rec("rinex"), emotion = emotion_stack(sc)),
       scanned = rbindlist(lapply(names(sc), function(sid) data.table(source_id = sid, root = sc[[sid]]$root, last_scanned = fmt_ts(sc[[sid]]$scanned)))))
}

# File rules (config/file_rules.csv) that ignore files, applied again to every source's last scan, so a rule added after a
# source was scanned takes effect on the next rebuild without scanning it again: the files are marked ignored, and their
# images and records are left out. (A rule that gives files a role, or one deleted since the scan, needs a rescan: the scan
# reads files by their role, and skips ignored ones.)
apply_file_rules <- function(A, rules) {
  if (is.null(A) || is.null(rules) || !nrow(rules) || !nrow(A$files)) return(A)
  ig <- rules[trim(action) == "ignore" & nzchar(trim(pattern))]
  for (i in seq_len(nrow(ig))) { hit <- tryCatch(grepl(ig$pattern[i], A$files$rel, perl = TRUE), error = function(e) { problem("file_rules.csv", paste("pattern not understood:", ig$pattern[i])); FALSE })
    A$files[hit & status == "", status := "ignored"] }
  gone <- A$files[status == "ignored", paste(source_id, rel)]
  if (!length(gone)) return(A)
  drop <- function(d) if (is.list(d) && !is.data.frame(d)) lapply(d, drop)          # the emotion record is a list of tables
                      else if (is.null(d) || !nrow(d) || !"rel" %in% names(d)) d else d[!paste(source_id, rel) %in% gone]
  A$images <- drop(A$images); A$records <- lapply(A$records, drop)
  A
}

# ---- time zones ---------------------------------------------------------------------------------------------------------
# Most cameras keep local time with no zone (DJI and senseFly give UTC). config/timezones.csv (Ovid/timezones.R) says which
# zone a camera's clock was in, by serial (camera_serial), model (sensor), folder or file (file_pattern, on source_id:rel)
# and date; a matching row gives those images a UTC time. A time the camera states as UTC is never changed.
apply_timezones <- function(img, tz) {
  if (!nrow(img)) return(img)                                  # := on a table with no columns would add a row
  img[, tz_rule := NA_character_]
  if (is.null(tz) || !nrow(tz)) return(img)
  todo <- img[is.na(time_utc) & clock_ok %in% TRUE, which = TRUE]
  if (!length(todo)) return(img)
  g <- img[todo, .(i = .I, serial = camera_serial, sensor, path = paste0(source_id, ":", dirname(rel), "/"), day = as.Date(time_exif))]
  g[, k := paste(serial, sensor, path, day)]
  keys <- g[, .(serial = serial[1], sensor = sensor[1], path = path[1], first = min(img$time_exif[todo[i]])), by = k]
  keys[, row := vapply(seq_len(.N), function(j) tz_match(tz, serial[j], path[j], sensor[j], first[j]), 0L)]
  keys <- keys[!is.na(row)]
  if (!nrow(keys)) return(img)
  keys[, `:=`(off = as.numeric(tz$offset_hours[row]), why = paste0("time zone table row ", row, ": UTC", sprintf("%+g", as.numeric(tz$offset_hours[row]))))]
  g <- merge(g, keys[, .(k, off, why)], by = "k")
  idx <- todo[g$i]
  set(img, idx, "time_utc", img$time_exif[idx] - g$off * 3600); set(img, idx, "tz_rule", g$why)
  say("  time zone table: UTC time for ", num(length(idx)), " images")
  img
}

# Every file accounted for: whether it reached the catalogue (an image in a flight, a file linked to a flight, a satellite
# or aerial dataset, a GIS layer) and, when not, why. One row per file; read = in_output + dropped (Ovid/file_audit.R).
rs_file_audit <- function(files, images, links, datasets, layers) {
  f <- files[, .(file = paste0(source_id, ":", rel), source_id, rel, format, role, status, size)]
  f[, `:=`(used = status == "", read = fifelse(status == "", 1L, NA_integer_))]
  f[!used %in% TRUE, status := fifelse(status == "", "not read", status)]
  in_img <- if (nrow(images)) images[!is.na(flight_id), unique(paste0(source_id, ":", rel))] else character()
  in_link <- if (nrow(links)) links[state != "rejected", unique(file)] else character()
  in_ds <- if (nrow(datasets) && all(c("source_id", "rel") %in% names(datasets))) datasets[, unique(paste0(source_id, ":", rel))] else character()
  in_gis <- if (nrow(layers) && all(c("source_id", "rel") %in% names(layers))) layers[, unique(paste0(source_id, ":", rel))] else character()
  where <- c(setNames(rep("flight image", length(in_img)), in_img), setNames(rep("linked to a flight", length(in_link)), in_link),
             setNames(rep("dataset", length(in_ds)), in_ds), setNames(rep("GIS layer", length(in_gis)), in_gis))
  where <- where[!duplicated(names(where))]
  kept <- data.table(source_file = names(where), in_output = 1L, used_in = unname(where))
  drop <- f[used %in% TRUE & !file %in% kept$source_file, .(source_file = file, stage = "catalogue", level = "row", n = 1L, entries = 1L,
            rule = fifelse(role %in% LINK_ROLES, "not linked to any flight", fifelse(role == "image", "image not in any flight", paste(role, "file: listed only"))))]
  fa <- file_reconcile(f, kept, drop, unit = "files")
  fa[used %in% TRUE, what_happened := fifelse(in_output > 0, paste("in the catalogue:", used_in), paste("not in the catalogue:", sub("^1 ", "", dropped_why)))]
  fa[, values_removed := NULL][]
}

# ---- flights ----------------------------------------------------------------------------------------------------------
# senseFly logs give a flight number to images whose names and folders carry none (Sequoia bands in img/msp, for one):
# the one flight number among the logs in the image folder or the two folders above it.
log_flight_no <- function(folder, source, files) {
  lg <- files[format == "sensefly_log", .(source_id, folder, no = ebee_flight_no(file)$no, serial = ebee_flight_no(file)$serial)][!is.na(no)]
  if (!nrow(lg)) return(list(no = rep(NA_integer_, length(folder)), serial = rep(NA_character_, length(folder))))
  up <- function(f) if (f %in% c("", ".")) "" else { d <- dirname(f); if (d == ".") "" else d }
  r <- lapply(seq_along(folder), function(i) { f <- folder[i]
    for (k in 0:2) { h <- unique(lg[source_id == source[i] & folder == f, .(no, serial)]); if (nrow(h) == 1L) return(h); if (nrow(h) > 1L || f == "") break; f <- up(f) }
    data.table(no = NA_integer_, serial = NA_character_) })
  r <- rbindlist(r); list(no = r$no, serial = r$serial)
}
# Multispectral cameras (radiometric calibration matters for them: Assmann et al. 2019): MicaSense RedEdge and Altum, Parrot
# Sequoia, the DJI P4 Multispectral and Mavic 3M, or any image with a band name in its XMP.
MULTISPECTRAL_RX <- "Sequoia|RedEdge|Altum|MicaSense|Multispectral|FC6360|M3M|Mavic 3M"
build_flights <- function(img, gap_min = 10, files = NULL, blur_limit_px = 1) {
  for (k in c("irradiance", "band", "model")) if (!k %in% names(img)) set(img, j = k, value = if (k == "irradiance") NA else NA_character_)
  if (!nrow(img) || !"file" %in% names(img)) return(list(flights = data.table(), images = img, folders = data.table()))
  e <- ebee_flight_no(img$file); d <- copy(img)[, `:=`(fno = e$no, fserial = e$serial)]
  for (v in c("capture_uuid", "flight_uuid")) if (!v %in% names(d)) d[, (v) := NA_character_]
  d[is.na(fno) & !(make %in% "DJI"), fno := folder_flight_no(folder)]
  if (!is.null(files) && nrow(u <- unique(d[is.na(fno) & !(make %in% "DJI"), .(source_id, folder)]))) {
    lf <- log_flight_no(u$folder, u$source_id, files); u[, `:=`(lno = lf$no, lserial = lf$serial)]
    d[u, `:=`(fno = fifelse(is.na(fno), i.lno, fno), fserial = fifelse(is.na(fserial), i.lserial, fserial)), on = .(source_id, folder)] }
  d[, cam := fifelse(!is.na(camera_serial) & nzchar(camera_serial), camera_serial, fifelse(is.na(sensor), "unknown camera", sensor))]
  # Sequoia writes its multispectral and RGB images of one flight into img/msp and img/rgb: one flight, not two
  d[, key := paste(source_id, sub("/(msp|rgb)$", "", folder), fifelse(!is.na(sensor) & grepl("Sequoia", sensor), "Parrot Sequoia", cam), fifelse(is.na(fno), fifelse(is.na(flight_uuid), "", flight_uuid), as.character(fno)))]
  d[, t := fifelse(clock_ok, fifelse(is.na(time_utc), time_exif, time_utc), as.POSIXct(NA))]
  setorder(d, key, t, file, na.last = TRUE)
  d[, seg := if (all(is.na(t)) || !is.na(fno[1]) || !is.na(flight_uuid[1])) 1L else cumsum(c(TRUE, diff(as.numeric(t)) > gap_min * 60)), by = key]
  # the identity of a flight: camera + first exposure; or, without a usable clock, camera + flight number + first image name.
  # A DJI exposure counts at the GPS time the camera wrote (time_utc plus the leap seconds), so flight ids made before the
  # leap seconds were taken off (2026-10-19) stay the same, and with them every edit saved against them.
  d[, t_id := fifelse(make %in% "DJI" & !is.na(time_utc) & clock_ok, t + (as.numeric(t) - as.numeric(gps_to_utc(t))), t)]
  d[, flight_id := { t0 <- suppressWarnings(min(t_id, na.rm = TRUE))
      k <- if (is.finite(t0)) list(cam[1], fmt_ts(t0)) else list(cam[1], fno[1], file[1], .N)
      paste0("UAV-", substr(hash_of(k), 1, 10)) }, by = .(key, seg)][, t_id := NULL]
  # one flight per eBee flight number (and serial, when known): a camera folder and eMotion's re-geotagged img/ copy, or a
  # partial copy elsewhere, start at different times and so got flight ids of their own. eBee images with the same flight
  # number starting within an hour of each other are one flight (the largest group's id); numbers taken from year folders
  # on other cameras never qualify, and the same number on another day stays another flight.
  m <- d[!is.na(fno) & (!is.na(fserial) | grepl("senseFly|SODA|Sequoia|S110|thermoMAP", sensor)),
         .(n = .N, img_out = basename(folder[1]) == "img", serial = fserial[!is.na(fserial)][1],
           t0 = { x <- suppressWarnings(min(as.numeric(t), na.rm = TRUE))               # a camera with an unusable clock: its own time,
                  if (is.finite(x)) x else suppressWarnings(min(as.numeric(time_exif), na.rm = TRUE)) }), by = .(fno, flight_id)]   # enough to tell days apart
  m <- m[is.finite(t0)][order(fno, t0)][, grp := cumsum(c(TRUE, diff(t0) > 3600)), by = fno]
  m[, serial := serial[!is.na(serial)][1], by = .(fno, grp)]
  m <- m[order(fno, grp, img_out, -n)][, keep := flight_id[1], by = .(fno, grp, serial)]
  if (any(m$flight_id != m$keep)) d[m[flight_id != keep], flight_id := i.keep, on = "flight_id"]
  # the same flight in several folders or sources (copies, eMotion's img/ output): a camera folder with the most images is primary
  fold <- d[, .(n = .N, img_out = basename(folder[1]) == "img"), by = .(flight_id, source_id, folder)][order(flight_id, img_out, -n, source_id, folder)]
  fold[, fk := paste(source_id, sub("/(msp|rgb)$", "", folder))]
  fold[, primary := fk == fk[1], by = flight_id]                 # Sequoia's img/msp and img/rgb are both the primary folder
  fold[, fk := NULL]
  d[fold, primary := i.primary, on = .(flight_id, source_id, folder)]
  p <- d[primary == TRUE]
  fl <- p[, .(source_id = source_id[1], folder = folder[1], family = fifelse(any(grepl("senseFly", sensor), na.rm = TRUE) | !is.na(fno[1]), "ebee", fifelse(any(make %in% "DJI"), "dji", "other")),
              sensor = mode_of(sensor), camera_serial = cam[1],
              aircraft = fifelse(!is.na(fserial[1]), fifelse(startsWith(fserial[1], "EP-"), "senseFly eBee Plus", "senseFly eBee"), mode_of(drone_model)),
              aircraft_serial = fifelse(!is.na(fserial[1]), fserial[1], mode_of(drone_serial)),
              flight_no = fno[1], start_utc = suppressWarnings(min(t, na.rm = TRUE)), end_utc = suppressWarnings(max(t, na.rm = TRUE)), clock_ok = all(clock_ok),
              utc_offset_h = { o <- round(as.numeric(difftime(time_exif, time_utc, units = "hours")) * 2) / 2; as.numeric(mode_of(o)) },
              n_images = .N, n_captures = uniqueN(fifelse(is.na(capture_uuid), file, capture_uuid)), n_georef = sum(is.finite(lat) & is.finite(lon) & !(lat == 0 & lon == 0)), size_gb = round(sum(size) / 1e9, 2),
              lat_min = rng(lat, min), lat_max = rng(lat, max), lon_min = rng(lon, min), lon_max = rng(lon, max),
              alt_rel_median_m = round(rng(alt_rel, stats::median), 1),
              rtk_fixed_pct = if (all(is.na(rtk_flag))) NA_real_ else round(100 * mean(rtk_flag == 50, na.rm = TRUE), 1),
              rtk_float_pct = if (all(is.na(rtk_flag))) NA_real_ else round(100 * mean(rtk_flag == 34, na.rm = TRUE), 1),
              rtk_std_h_median_m = round(rng(sqrt(rtk_std_lat^2 + rtk_std_lon^2), stats::median), 3),
              acc_capture_m = round(rng(acc_xy, stats::median), 3),
              multispectral = any(grepl(MULTISPECTRAL_RX, paste(sensor, model), ignore.case = TRUE)) | any(!is.na(band) & nzchar(band)),
              irradiance_sensor = any(irradiance %in% TRUE)), by = flight_id]
  fl[!is.finite(start_utc), `:=`(start_utc = as.POSIXct(NA), end_utc = as.POSIXct(NA))]
  fl <- merge(fl, flight_metrics(d, blur_limit_px), by = "flight_id", all.x = TRUE)          # ground sample distance, speed, blur, overlap, sun
  # accuracy of copies that eMotion re-geotagged (its img/ folders), the processed evidence
  proc <- d[primary == FALSE & basename(folder) == "img", .(acc_output_m = round(rng(acc_xy, stats::median), 3)), by = flight_id]
  cp <- fold[primary == FALSE, .(copies = paste0(source_id, ":", folder, " (", n, " images)", collapse = "; "), n_copies = .N), by = flight_id]
  fl <- merge(fl, proc, by = "flight_id", all.x = TRUE)
  fl <- merge(fl, cp, by = "flight_id", all.x = TRUE)[is.na(copies), `:=`(copies = "", n_copies = 0L)]
  list(flights = fl, images = d, folders = fold)
}

# ---- links -----------------------------------------------------------------------------------------------------------
# Route 3 of link_files: the flights whose image names a list file (geotags, KML, Pix4D) names, by lower-case name. Names like
# DJI_0001.JPG repeat across many flights, so only the flight(s) with the most names in common are kept; k is how many tie.
list_hits <- function(gi, images) {
  h <- merge(gi, unique(images[, .(name = tolower(file), flight_id)]), by = "name", allow.cartesian = TRUE)[, .(n = .N), by = .(source_id, rel, flight_id)]
  h[, best := n == max(n), by = .(source_id, rel)][best == TRUE][, k := .N, by = .(source_id, rel)][, best := NULL][]
}

# Each flight's other files, with a confidence from the evidence. Two routes: files in a flight's folders (DJI records,
# onboard GNSS and LiDAR raw files, video), scored by time and name; and files carrying a senseFly flight number
# (logs, eMotion geotags, plans, Pix4D projects), matched on that number anywhere in the archive. People's confirmed
# and rejected links (config/links.csv, file = source_id:rel) win.
link_files <- function(fl, folders, files, user = NULL, geo = NULL, images = NULL, kml = NULL, rinex = NULL, mrk = NULL, p4d = NULL) {
  cand <- files[role %in% LINK_ROLES & status == "", .(source_id, rel, file, folder, format, role)]
  cand[, ref := paste0(source_id, ":", rel)]
  # route 1: same folder; for eMotion's layout (images in img/, img/msp, img/rgb) also the flight folder one or two levels up,
  # where its logs, geotags and Pix4D projects sit
  up <- function(f) fifelse(grepl("/", f), dirname(f), "")
  fo <- folders[, .(flight_id, source_id, folder)]
  im1 <- fo[basename(folder) %in% c("img", "msp", "rgb")][, folder := up(folder)]
  im2 <- im1[basename(folder) == "img"][, folder := up(folder)]
  fo <- unique(rbind(fo, im1[nzchar(folder)], im2[nzchar(folder)]))   # a flight at the source's top level keeps "" (a flight folder scanned on its own)
  x <- merge(cand, fo, by = c("source_id", "folder"), allow.cartesian = TRUE)
  x <- merge(x, fl[, .(flight_id, start_utc, end_utc, utc_offset_h)], by = "flight_id")
  x <- merge(x, fo[, .(n_flights = uniqueN(flight_id)), by = .(source_id, folder)], by = c("source_id", "folder"))
  x[, t := dji_name_time(file) - fifelse(is.na(utc_offset_h), 0, utc_offset_h) * 3600]
  x[, `:=`(in_time = !is.na(t) & !is.na(start_utc) & t >= start_utc - 900 & t <= end_utc + 300, name_match = nzchar(folder) & startsWith(file, basename(folder)))]
  x[, out_time := !is.na(t) & !in_time & !is.na(utc_offset_h) & !is.na(start_utc)]
  x[, score := pmin(1, pmax(0, 0.5 + 0.4 * in_time - 0.3 * out_time + 0.3 * (is.na(t) & n_flights == 1) + 0.4 * name_match))]
  x[, evidence := paste("same folder", fifelse(in_time, "file time within the flight", ""), fifelse(out_time, "file time outside the flight", ""),
       fifelse(name_match, "named after the flight folder", ""), fifelse(is.na(t) & n_flights == 1, "only flight in the folder", ""), sep = "; ")]
  # route 2: senseFly flight numbers (a number in the file name, or in its folder's name for eMotion output folders)
  cn <- cand[family_of(format) == "ebee" | format %in% c("pix4d_project", "mission", "metashape", "kml")]
  if (nrow(cn)) {
    e <- ebee_flight_no(cn$file); cn[, `:=`(fno = e$no, fserial = e$serial)]
    cn[is.na(fno), fno := folder_flight_no(folder)]
    cn <- cn[!is.na(fno)]
    y <- merge(cn, fl[!is.na(flight_no), .(flight_id, flight_no, aircraft_serial)], by.x = "fno", by.y = "flight_no", allow.cartesian = TRUE)
    y <- y[is.na(fserial) | is.na(aircraft_serial) | fserial == aircraft_serial]
    y[, `:=`(score = fifelse(!is.na(fserial), 0.95, 0.8),
             evidence = fifelse(!is.na(fserial), "aircraft serial and flight number in the file name", "flight number in the folder name"))]
    x <- rbind(x, y[, names(y) %in% names(x), with = FALSE], fill = TRUE)
  }
  # route 3: eMotion geotag files, KML tracks and Pix4D projects that list a flight's own image names
  lists <- rbind(if (!is.null(geo) && nrow(geo) && "geo_files" %in% names(geo)) geo[, .(source_id, rel, names = geo_files)],
                 if (!is.null(kml) && nrow(kml) && "kml_files" %in% names(kml)) kml[!is.na(kml_files) & nzchar(kml_files), .(source_id, rel, names = kml_files)],
                 if (!is.null(p4d) && nrow(p4d) && "p4d_files" %in% names(p4d)) p4d[!is.na(p4d_files) & nzchar(p4d_files), .(source_id, rel, names = p4d_files)])
  if (!is.null(lists) && nrow(lists) && !is.null(images)) {
    gi <- lists[, .(name = tolower(strsplit(names, ";", fixed = TRUE)[[1]])), by = .(source_id, rel)]
    h <- list_hits(gi, images)
    if (nrow(h)) { h <- merge(h, cand, by = c("source_id", "rel"))
      x <- rbind(x, h[, .(flight_id, source_id, rel, file, folder, format, role, ref, score = fifelse(k == 1L, 0.95, 0.6),
                          evidence = paste0("lists ", n, " of the flight's image names", fifelse(k > 1L, paste0(" (as do ", k - 1L, " other flights)"), "")))], fill = TRUE) }
  }
  # route 4: GNSS observations anywhere in the archive whose time span covers the flight (a base station, say): probable at best
  if (!is.null(rinex) && nrow(rinex) && "obs_start_utc" %in% names(rinex)) {
    rx <- rinex[!is.na(obs_start_utc) & !is.na(obs_end_utc), .(source_id, rel, s = as.POSIXct(obs_start_utc, tz = "UTC"), e = as.POSIXct(obs_end_utc, tz = "UTC"))]
    ft <- fl[!is.na(start_utc), .(flight_id, start_utc, end_utc)]
    if (nrow(rx) && nrow(ft)) { o <- ft[rx, on = .(start_utc >= s, end_utc <= e), nomatch = 0L, .(flight_id, source_id = i.source_id, rel = i.rel)]
      if (nrow(o)) { o <- merge(o, cand, by = c("source_id", "rel"))
        x <- rbind(x, o[, .(flight_id, source_id, rel, file, folder, format, role, ref, score = 0.6, evidence = "observations span the whole flight")], fill = TRUE) } }
  }
  # route 5: mission KMLs, flown tracks, DJI MRK (RTK) files and eMotion geotags kept anywhere (any source, any depth), matched
  # by where they are (their extent covers
  # a flight's) and, when they carry one, by time; one flight in reach is auto, several are told apart by date or left suggested
  bb <- function(d, p) if (!is.null(d) && nrow(d) && paste0(p, "_lat_min") %in% names(d)) d[is.finite(get(paste0(p, "_lat_min"))), .(source_id, rel,
    a1 = get(paste0(p, "_lat_min")), a2 = get(paste0(p, "_lat_max")), o1 = get(paste0(p, "_lon_min")), o2 = get(paste0(p, "_lon_max")),
    ts = if (paste0(p, "_start_utc") %in% names(d)) as.POSIXct(get(paste0(p, "_start_utc")), tz = "UTC") else as.POSIXct(NA))]
  name_day <- function(f) { m <- regmatches(f, regexpr("(19|20)[0-9]{2}[-_]?[01][0-9][-_]?[0-3][0-9]", f))   # a date in the name, for same-day only
    out <- as.POSIXct(rep(NA_real_, length(f)), origin = "1970-01-01", tz = "UTC"); h <- regexpr("(19|20)[0-9]{2}[-_]?[01][0-9][-_]?[0-3][0-9]", f) > 0
    out[h] <- as.POSIXct(paste(gsub("[-_]", "", m), "19"), format = "%Y%m%d %H", tz = "UTC"); out }
  sp <- rbind(bb(kml, "kml"), bb(mrk, "mrk"), bb(geo, "geo"), bb(p4d, "p4d"))
  if (!is.null(sp) && nrow(sp) && all(c("lat_min", "lon_max") %in% names(fl))) {
    sp <- merge(sp, cand[, .(source_id, rel, file)], by = c("source_id", "rel"))
    ff <- fl[is.finite(lat_min), .(flight_id, b1 = lat_min, b2 = lat_max, c1 = lon_min, c2 = lon_max, start_utc, end_utc, utc_offset_h)]
    if (nrow(sp) && nrow(ff)) {
      pad <- 0.0005; near <- 0.02                       # about 50 m for single-point or narrow extents; within ~2 km counts only with a matching time
      o <- sp[, .(k = 1L, source_id, rel, file, a1, a2, o1, o2, ts)][ff[, k := 1L], on = "k", allow.cartesian = TRUE][
        pmin(a2, b2) + near >= pmax(a1, b1) & pmin(o2, c2) + near >= pmax(o1, c1)]
      if (nrow(o)) {
        o[, share := (pmin(a2, b2) - pmax(a1, b1) + pad) * (pmin(o2, c2) - pmax(o1, c1) + pad) / ((pmin(a2 - a1, b2 - b1) + pad) * (pmin(o2 - o1, c2 - c1) + pad))]
        o[, t := fifelse(is.na(ts), dji_name_time(file) - fifelse(is.na(utc_offset_h), 0, utc_offset_h) * 3600, ts)]
        o[, `:=`(in_time = !is.na(t) & !is.na(start_utc) & t >= start_utc - 1800 & t <= end_utc + 1800,
                 same_day = !is.na(start_utc) & abs(as.numeric(difftime(fifelse(is.na(t), name_day(file), t), start_utc, units = "hours"))) < 18)]
        o[is.na(same_day), same_day := FALSE]
        o <- o[in_time | (pmin(a2, b2) + pad >= pmax(a1, b1) & pmin(o2, c2) + pad >= pmax(o1, c1))]
        o[, n_here := .N, by = .(source_id, rel)]; o[, n_day := sum(same_day), by = .(source_id, rel)]
        o[, score := fifelse(in_time, 0.9, fifelse(n_here == 1L & share > 0.3, 0.8, fifelse(same_day & n_day == 1L, 0.8, fifelse(same_day, 0.6, 0.4))))]
        o[, evidence := paste0("covers the flight's area", fifelse(in_time, "; time within the flight", fifelse(n_here == 1L, "; the only flight there",
                                 fifelse(same_day, "; same day", "; other flights there too"))))]
        o <- merge(o[, .(flight_id, source_id, rel, score, evidence)], cand, by = c("source_id", "rel"))
        x <- rbind(x, o[, .(flight_id, source_id, rel, file, folder, format, role, ref, score, evidence)], fill = TRUE) } } }
  x[, evidence := gsub("^; |; $", "", gsub("(; )+", "; ", evidence))]
  if (!is.null(user) && nrow(user)) x <- x[!user[state == "rejected"], on = .(flight_id, ref = file)]
  setorder(x, ref, -score); x <- x[, .SD[1], by = ref]
  x[, state := fifelse(score >= 0.8, "auto", fifelse(score >= 0.5, "probable", "suggested"))]
  if (!is.null(user) && nrow(cf <- user[state == "confirmed" & flight_id %in% fl$flight_id])) {
    x <- x[!ref %in% cf$file]
    add <- merge(cf[, .(flight_id, ref = file, evidence = paste0("confirmed", fifelse(nzchar(author), paste(" by", author), ""), fifelse(nzchar(reason), paste0(": ", reason), "")))],
                 cand, by = "ref")
    x <- rbind(x, add[, `:=`(score = 1, state = "confirmed")], fill = TRUE)
  }
  x[, .(flight_id, file = ref, source_id, rel, name = file, folder, format, role, score = round(score, 2), state, evidence)][order(flight_id, file)]
}
family_of <- function(format) FORMAT_INFO$family[match(format, FORMAT_INFO$format)]

# ---- positioning -------------------------------------------------------------------------------------------------------
# How each flight's positions were corrected, from the images themselves. Correction files alone never prove it.
#   DJI: the RTK flag each image carries (and the MRK/RPT counts beside it).
#   senseFly: the geotag accuracy. Centimetres in the camera's own images = corrected at capture (RTK); metres there but
#   centimetres in eMotion's geotags (geoinfo.txt, or its re-geotagged img/ copies) = corrected afterwards (PPK);
#   metres everywhere = standalone; nothing to judge from = unknown, or "available, not confirmed" when GNSS observations exist.
positioning <- function(fl, links, recs, corrected_m = CORRECTED_M) {
  ok <- links[state %in% c("auto", "probable", "confirmed")]
  has <- function(fmt) fl$flight_id %in% ok[format %in% fmt]$flight_id
  fl[, `:=`(has_mrk = has("dji_mrk"), has_rpt = has("dji_rpt"), has_gnss_obs = has(c("dji_gnss_raw", "rinex", "gnss_solution")),
            has_lidar_raw = has("dji_lidar_raw"), has_video = has("video"), has_log = has("sensefly_log"), has_geotags = has("emotion_geoinfo"),
            has_mission = has(c("mission", "emotion_plan", "pix4d_project")))]
  first_of <- function(fmt, rec, cols) { if (is.null(rec) || !nrow(rec)) return(NULL)
    m <- merge(ok[format == fmt, .(flight_id, source_id, rel)], rec, by = c("source_id", "rel"))[, .SD[1], by = flight_id]
    m[, c("flight_id", intersect(cols, names(m))), with = FALSE] }
  for (m in list(first_of("dji_mrk", recs$mrk, c("mrk_shots", "mrk_fixed_pct")),
                 first_of("dji_rpt", recs$rpt, c("mission_id", "fly_time_s", "rpt_captures", "rpt_fixed")),
                 first_of("emotion_geoinfo", recs$geo, c("geo_images", "geo_acc_xy_median_m", "geo_lat_min", "geo_lat_max", "geo_lon_min", "geo_lon_max")),
                 first_of("kml", if ("kml_kind" %in% names(recs$kml)) recs$kml[kml_kind == "observed"], c("kml_start_utc", "kml_end_utc"))))
    if (!is.null(m) && ncol(m) > 1) fl <- merge(fl, m, by = "flight_id", all.x = TRUE)
  for (v in c("mrk_shots", "mrk_fixed_pct", "mission_id", "fly_time_s", "rpt_captures", "rpt_fixed", "geo_images", "geo_acc_xy_median_m",
              "geo_lat_min", "geo_lat_max", "geo_lon_min", "geo_lon_max", "acc_output_m")) if (!v %in% names(fl)) fl[, (v) := NA_real_]
  # a flight whose camera clock is unusable (the Canon S110) takes its time from the flown track eMotion exported, when linked
  for (v in c("kml_start_utc", "kml_end_utc")) if (!v %in% names(fl)) fl[, (v) := NA_character_]
  fl[, time_source := fifelse(is.na(start_utc), "", "camera")]
  # a camera clock never set (a compact camera at its factory date) is further from the flown track's GNSS time than any time
  # zone explains (CLOCK_LOG_TOL_H, R/emotion_catalogue.R): the track's time is used, and a decoded log's wins over both
  off_k <- abs(as.numeric(difftime(fl$start_utc, as.POSIXct(fl$kml_start_utc, tz = "UTC"), units = "hours")))
  fl[time_source == "camera" & is.finite(off_k) & off_k > CLOCK_LOG_TOL_H, `:=`(clock_ok = FALSE, start_utc = NA)]
  fl[is.na(start_utc) & !is.na(kml_start_utc), `:=`(start_utc = as.POSIXct(kml_start_utc, tz = "UTC"), end_utc = as.POSIXct(kml_end_utc, tz = "UTC"), time_source = "flown track (KML)")]
  # the linked senseFly log: its time span, track extent and photo tags, and the eMotion mission block (R/emotion_catalogue.R)
  fl <- emotion_flight_fields(fl, ok, recs$emotion %||% list())
  # a flight whose own images have no position takes its extent from eMotion's geotags
  fl[!is.finite(lat_min) & is.finite(geo_lat_min), `:=`(lat_min = geo_lat_min, lat_max = geo_lat_max, lon_min = geo_lon_min, lon_max = geo_lon_max)]
  fl[, acc_processed_m := fifelse(is.finite(geo_acc_xy_median_m), geo_acc_xy_median_m, acc_output_m)]
  P <- POSITIONING; C <- corrected_m
  fl[, positioning_found := fifelse(family == "dji",
       fifelse(is.na(rtk_fixed_pct), fifelse(has_gnss_obs, P[5], fifelse(aircraft %in% NO_RTK_AIRCRAFT, P[4], P[6])),
               fifelse(rtk_fixed_pct >= 95, P[1], fifelse(rtk_fixed_pct > 0, P[2], fifelse(has_gnss_obs, P[5], P[4])))),
       fifelse(is.finite(acc_capture_m) & acc_capture_m <= C, P[1],
       fifelse(is.finite(acc_processed_m) & acc_processed_m <= C, P[3],
       fifelse(is.finite(acc_capture_m) | is.finite(acc_processed_m), fifelse(has_gnss_obs, P[5], P[4]), fifelse(has_gnss_obs, P[5], P[6])))))]
  fl[, positioning_evidence := fifelse(family == "dji",
       fifelse(is.na(rtk_fixed_pct), fifelse(aircraft %in% NO_RTK_AIRCRAFT, paste(aircraft, "has no RTK; image positions from its own GNSS"), "no RTK flag in the image metadata"),
               paste0(rtk_fixed_pct, "% of images RTK fixed (image XMP)", fifelse(is.na(rpt_fixed), "", paste0("; RPT: ", rpt_fixed, " of ", rpt_captures, " fixed")),
                      fifelse(has_gnss_obs & rtk_fixed_pct < 95, "; onboard GNSS observations kept (PPK possible, not confirmed)", ""))),
       paste0(fifelse(is.finite(acc_capture_m), paste0("geotag accuracy in the camera images ", acc_capture_m, " m"), "no geotag accuracy in the camera images"),
              fifelse(is.finite(acc_processed_m), paste0("; in eMotion's geotags ", acc_processed_m, " m"), "; no eMotion geotags found"),
              fifelse(has_gnss_obs, "; GNSS observations linked", "")))]
  fl[, qa_flags := gsub("^; |; $", "", gsub("(; )+", "; ", paste(
        fifelse(n_georef == 0 & !is.finite(geo_lat_min), "no georeferenced images", ""),
        fifelse(n_georef == 0 & is.finite(geo_lat_min), "positions only in eMotion geotags", ""),
        fifelse(!clock_ok, paste0("camera clock unusable", fifelse(time_source == "", "", paste0("; time from ", time_source))), ""),
        fifelse(!is.na(rpt_captures) & rpt_captures != n_images, paste0("RPT counts ", rpt_captures, " captures, ", n_images, " images found"), ""),
        fifelse(!is.na(mrk_shots) & mrk_shots != n_images, paste0("MRK has ", mrk_shots, " shots, ", n_images, " images found"), ""),
        fifelse(!is.na(geo_images) & geo_images != n_images, paste0("eMotion geotags for ", geo_images, " images, ", n_images, " found"), ""),
        fifelse(family == "ebee" & !has_log, "no flight log", ""), emo_qa,
        fifelse(!is.na(rtk_float_pct) & rtk_float_pct > 0, paste0(rtk_float_pct, "% RTK float"), ""), sep = "; ")))]
  fl[]
}

# ---- geometry ------------------------------------------------------------------------------------------------------------
# Every line or outline the map draws, tagged with what it is: planned (a mission or plan), observed (a flown track, image
# positions, a RINEX marker) or derived (worked out by Ovid, such as a flight's extent from its image positions).
geometry <- function(fl, links, kml, rinex) {
  g <- list(fl[is.finite(lat_min), .(flight_id, kind = "derived", what = "extent of image positions", ref = "",
                                    line = sprintf("%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f;%.6f %.6f", lat_min, lon_min, lat_min, lon_max, lat_max, lon_max, lat_max, lon_min, lat_min, lon_min))])
  ok <- links[state %in% c("auto", "probable", "confirmed")]
  if (!is.null(kml) && nrow(kml) && "kml_line" %in% names(kml)) {
    k <- kml[kml_kind %in% c("planned", "observed"), .(ref = paste0(source_id, ":", rel), kind = kml_kind, line = kml_line)]
    k <- merge(k, ok[, .(ref = file, flight_id)], by = "ref", all.x = TRUE)
    g <- c(g, list(k[, .(flight_id, kind, what = fifelse(kind == "planned", "mission plan (KML)", "flown track (KML)"), ref, line)])) }
  if (!is.null(rinex) && nrow(rinex) && "rinex_lat" %in% names(rinex)) {
    r <- rinex[is.finite(rinex_lat), .(ref = paste0(source_id, ":", rel), kind = "observed", what = paste("GNSS receiver", rinex_marker), line = sprintf("%.6f %.6f", rinex_lat, rinex_lon))]
    g <- c(g, list(merge(r, ok[, .(ref = file, flight_id)], by = "ref", all.x = TRUE)[, .(flight_id, kind, what, ref, line)])) }
  rbindlist(g, fill = TRUE)
}

# ---- missions -----------------------------------------------------------------------------------------------------------
# One survey is often flown as several sorties: a battery swap mid-mission, or two short flights on one battery. Each
# sortie stays a flight of its own; consecutive sorties of one aircraft on one day are put in one mission when the pause
# between them is short and they share a plan, a folder, or an overlapping area plus the site or the next flight number. A grouping with only weak evidence is marked
# "uncertain" for people to confirm or split on the Flight page. People's choice (config/flights.csv, column mission_set:
# a name groups every sortie given it, "own" keeps a sortie on its own) wins over what the scan finds.
MISSION_GAP_MIN <- 60      # longest pause between sorties of one mission (landing, battery swap, take-off)
MISSION_PAD_DEG <- 0       # areas must overlap: neighbouring surveys are often separate datasets
group_missions <- function(fl, links = NULL) {
  if (!nrow(fl)) return(fl)
  num0 <- function(a, b) fifelse(is.finite(a), a, b)
  d <- fl[, .(flight_id, family, folder, src = source_id, date, site = fifelse(is.na(site), "", site),
              plat = fifelse(!is.na(aircraft_serial) & nzchar(aircraft_serial), aircraft_serial, fifelse(is.na(camera_serial), "", camera_serial)),
              s = start_utc, e = end_utc, no = flight_no, mid = if ("mission_id" %in% names(fl)) as.character(mission_id) else NA_character_,
              a1 = num0(lat_min, geo_lat_min), a2 = num0(lat_max, geo_lat_max), o1 = num0(lon_min, geo_lon_min), o2 = num0(lon_max, geo_lon_max))]
  # the plan each sortie was flown from (DJI mission KMZ, eMotion plan), when linked
  pl <- if (!is.null(links) && nrow(links)) links[state %in% c("auto", "probable", "confirmed") & (role == "mission" | format %in% c("mission", "emotion_plan")),
                                                  .(plan = paste(sort(unique(file)), collapse = "|")), by = flight_id] else data.table(flight_id = character(), plan = character())
  d <- merge(d, pl, by = "flight_id", all.x = TRUE)[is.na(plan), plan := ""]
  um <- if ("mission_set" %in% names(fl)) fl[!is.na(mission_set) & nzchar(trimws(mission_set)), .(flight_id, um = trimws(as.character(mission_set)))] else data.table(flight_id = character(), um = character())
  d <- merge(d, um, by = "flight_id", all.x = TRUE)
  setorder(d, family, plat, s, na.last = TRUE)
  d[, `:=`(join = FALSE, sure = FALSE, why = "")]
  for (i in seq_len(nrow(d))[-1]) { p <- i - 1L
    if (!is.na(d$um[i]) || !is.na(d$um[p]) || d$family[i] != d$family[p] || d$plat[i] != d$plat[p] || is.na(d$s[i]) || is.na(d$e[p]) || !identical(d$date[i], d$date[p])) next
    gap <- as.numeric(difftime(d$s[i], d$e[p], units = "mins")); if (gap < -2 || gap > MISSION_GAP_MIN) next
    plan  <- nzchar(d$plan[i]) && d$plan[i] == d$plan[p]
    mid   <- !is.na(d$mid[i]) && nzchar(d$mid[i]) && identical(d$mid[i], d$mid[p])
    fold  <- d$src[i] == d$src[p] && d$folder[i] == d$folder[p]
    near  <- all(is.finite(c(d$a1[i], d$a1[p]))) && d$a1[i] <= d$a2[p] + MISSION_PAD_DEG && d$a1[p] <= d$a2[i] + MISSION_PAD_DEG && d$o1[i] <= d$o2[p] + MISSION_PAD_DEG && d$o1[p] <= d$o2[i] + MISSION_PAD_DEG
    site2 <- nzchar(d$site[i]) && nzchar(d$site[p]); same_site <- site2 && d$site[i] == d$site[p]
    seq_no <- !is.na(d$no[i]) && !is.na(d$no[p]) && d$no[i] == d$no[p] + 1L
    if (!(plan || mid || fold || (near && gap <= 30 && (same_site || seq_no))) || (site2 && !same_site && !(plan || mid))) next
    d$join[i] <- TRUE
    d$sure[i] <- plan || mid || (gap <= 30 && (fold || (near && same_site && (seq_no || gap <= 15))))
    d$why[i] <- paste(c(sprintf("%.0f min after the previous sortie", max(gap, 0)), if (plan) "same mission plan", if (mid) "same DJI mission id",
                        if (fold) "same folder", if (near) "overlapping area", if (same_site) "same site", if (seq_no) "next flight number"), collapse = ", ") }
  d[, grp := cumsum(!join | !is.na(um))]
  d[, mission := paste0("MIS-", substr(hash_of(list(flight_id[1])), 1, 8)), by = grp]
  d[, mission_status := fifelse(.N == 1L, "single", fifelse(all(sure[-1]), "grouped", "uncertain")), by = grp]
  d[, mission_evidence := fifelse(.N == 1L, "", paste(fifelse(join, why, "first sortie")[-1], collapse = "; ")), by = grp]
  # people's choice
  d[!is.na(um), `:=`(mission = fifelse(tolower(um) == "own", paste0("MIS-", substr(hash_of(list(flight_id)), 1, 8)), um),
                     mission_status = fifelse(tolower(um) == "own", "single", "confirmed"), mission_evidence = fifelse(tolower(um) == "own", "kept on its own by you", "grouped by you")), by = flight_id]
  setorder(d, mission, s, na.last = TRUE)
  d[, `:=`(mission_part = seq_len(.N), mission_parts = .N), by = mission]
  d[mission_parts == 1L & mission_status == "confirmed", mission_status := "single"]
  fl <- merge(fl, d[, .(flight_id, mission, mission_part, mission_parts, mission_status, mission_evidence)], by = "flight_id", all.x = TRUE, sort = FALSE)
  fl[mission_status == "uncertain", qa_flags := fifelse(is.na(qa_flags) | !nzchar(qa_flags), "mission grouping uncertain", paste0(qa_flags, "; mission grouping uncertain"))]
  fl[]
}

# ---- flight quality from the image headers ---------------------------------------------------------------------------------
# Per image (columns added to the image table) and per flight (from its primary images), all from what the camera wrote:
#   gsd_cm         ground sample distance = sensor width x height above take-off / (focal length x image width), in cm per
#                  pixel. The height is DJI's RelativeAltitude (above the take-off point, not the ground under the image), so
#                  over sloping ground or water it is approximate. Images without a height (eBee) have none.
#   speed_ms       DJI's FlightXSpeed and FlightYSpeed where written, else the distance to the previous image over the time
#                  between them (images up to 30 s apart in the same folder).
#   blur_px        how far the camera moved during the exposure, in pixels: speed x exposure time / GSD. Under 0.5 to 1 pixel
#                  is usually wanted for mapping; a flight's blur_over_limit_pct is the share of its images above blur_limit_px (1 by default; config/calibration_options.csv).
#   fwd_overlap_pct  forward overlap with the previous image on the same line: 1 - distance / footprint along the track, with
#                  the short side of the image taken as along the track (the usual layout for mapping flights). Where only a
#                  sample of headers was read (big folders), the flight's value is estimated from speed x time between
#                  exposures instead; fwd_overlap_source says which.
#   side_overlap_pct (per flight) between neighbouring parallel lines: 1 - line spacing / footprint across the track. Lines
#                  start where the heading turns by more than 45 degrees or the images are more than 30 s apart.
#   sun_elev_deg   the sun's elevation at the exposure (within about 4°: Cooper 1969 declination, no equation of time), from the position and the UTC time.
sun_elevation_deg <- function(t, lat, lon) {
  u <- as.numeric(t); doy <- as.POSIXlt(u, origin = "1970-01-01", tz = "UTC")$yday + 1
  dec <- 23.44 * pi / 180 * sin(2 * pi * (284 + doy) / 365); h <- ((u %% 86400) / 3600 + lon / 15 - 12) * 15 * pi / 180; la <- lat * pi / 180
  asin(sin(la) * sin(dec) + cos(la) * cos(dec) * cos(h)) * 180 / pi
}

flight_metrics <- function(d, blur_limit_px = 1) {
  num <- function(v) if (v %in% names(d)) as.numeric(d[[v]]) else rep(NA_real_, nrow(d))
  gsd <- num("sensor_w_mm") * num("alt_rel") * 100 / (num("focal_mm") * num("px_x")); gsd[!is.finite(gsd) | gsd <= 0] <- NA_real_
  set(d, j = "gsd_cm", value = round(gsd, 3))
  ok <- is.finite(d$lat) & is.finite(d$lon) & !(d$lat == 0 & d$lon == 0)
  x <- data.table(i = seq_len(nrow(d)), f = d$flight_id, g = paste(d$source_id, d$folder), t = as.numeric(d$t), lat = d$lat, lon = d$lon, ok = ok,
                  sx = num("speed_x"), sy = num("speed_y"), gsd = gsd, ex = num("exposure_s"), py = num("px_y"), px = num("px_x"), primary = d$primary %in% TRUE)
  setorder(x, f, g, t)
  R <- 6371000
  x[, `:=`(X = lon * pi / 180 * R * cos(stats::median(lat[ok], na.rm = TRUE) * pi / 180), Y = lat * pi / 180 * R), by = f]
  x[, `:=`(dX = X - shift(X), dY = Y - shift(Y), dt = t - shift(t)), by = .(f, g)]
  x[, dist := sqrt(dX^2 + dY^2)][, near := ok & shift(ok, fill = FALSE) & is.finite(dt) & dt > 0 & dt <= 30, by = .(f, g)]
  x[, head := fifelse(near & dist > 0, (atan2(dX, dY) * 180 / pi) %% 360, NA_real_)]
  x[, speed := fifelse(is.finite(sx) & is.finite(sy), sqrt(sx^2 + sy^2), fifelse(near, dist / dt, NA_real_))]
  x[, turn := { h <- head; dh <- abs(((h - shift(h)) + 180) %% 360 - 180); !near | (!is.na(dh) & dh > 45) }, by = .(f, g)]
  x[, line := cumsum(turn %in% TRUE), by = .(f, g)]
  x[, fwd := fifelse(near & !turn & is.finite(gsd) & is.finite(py) & py > 0, 100 * (1 - dist / (gsd / 100 * py)), NA_real_)]
  x[, blur := fifelse(is.finite(speed) & is.finite(ex) & is.finite(gsd) & gsd > 0, speed * ex / (gsd / 100), NA_real_)]
  x[, sun := fifelse(ok & is.finite(t), sun_elevation_deg(t, lat, lon), NA_real_)]
  setorder(x, i)
  set(d, j = "speed_ms", value = round(x$speed, 2)); set(d, j = "blur_px", value = round(x$blur, 2))
  set(d, j = "fwd_overlap_pct", value = round(x$fwd, 1)); set(d, j = "sun_elev_deg", value = round(x$sun, 1))
  # side overlap: neighbouring lines of the primary images that run parallel (or back and forth)
  side <- x[primary == TRUE & ok, .(n = .N, mx = mean(X), my = mean(Y), hd = { h <- head[is.finite(head)]; if (length(h)) (atan2(mean(sin(h * pi / 180)), mean(cos(h * pi / 180))) * 180 / pi) %% 360 else NA_real_ },
                                  t0 = min(t, na.rm = TRUE), fw = stats::median(gsd / 100 * px, na.rm = TRUE)), by = .(f, g, line)][n >= 3 & is.finite(hd)]
  setorder(side, f, t0)
  side[, so := { dh <- abs(((hd - shift(hd)) + 180) %% 360 - 180); par <- !is.na(dh) & (dh < 20 | dh > 160)
         ux <- sin(shift(hd) * pi / 180); uy <- cos(shift(hd) * pi / 180)
         sp <- abs((mx - shift(mx)) * uy - (my - shift(my)) * ux)                # spacing across the previous line's direction
         fifelse(par & is.finite(fw) & fw > 0 & sp > 0, 100 * (1 - sp / fw), NA_real_) }, by = f]
  p <- x[primary == TRUE]
  m <- function(v) if (any(is.finite(v))) round(stats::median(v[is.finite(v)]), 2) else NA_real_
  out <- p[, .(gsd_cm_median = m(gsd), speed_ms_median = m(speed), exposure_s_median = if (any(is.finite(ex))) signif(stats::median(ex[is.finite(ex)]), 3) else NA_real_, blur_px_median = m(blur),
               blur_limit_px = blur_limit_px, blur_over_limit_pct = if (any(is.finite(blur))) round(100 * mean(blur[is.finite(blur)] > blur_limit_px), 1) else NA_real_,
               fwd_overlap_pct_median = m(fwd), sun_elev_min_deg = if (any(is.finite(sun))) round(min(sun, na.rm = TRUE), 1) else NA_real_,
               sun_elev_max_deg = if (any(is.finite(sun))) round(max(sun, na.rm = TRUE), 1) else NA_real_), by = .(flight_id = f)]
  so <- side[, .(side_overlap_pct_median = m(so)), by = .(flight_id = f)]
  out <- merge(out, so, by = "flight_id", all.x = TRUE)
  # In a big folder only a sample of headers is read, so neighbouring images rarely both have a position. Forward overlap is then
  # estimated per flight from the median speed times the median time between exposures (all images have a time), over the
  # median footprint along the track. Side overlap needs the lines, so it stays empty there.
  est <- x[primary == TRUE, .(iv = { d <- diff(sort(t)); d <- d[is.finite(d) & d > 0 & d <= 30]; if (length(d)) stats::median(d) else NA_real_ },
                              sp = m(speed), fa = stats::median(gsd / 100 * py, na.rm = TRUE)), by = .(flight_id = f)]
  out[est, on = "flight_id", fwd_est := fifelse(is.finite(i.iv) & is.finite(i.sp) & is.finite(i.fa) & i.fa > 0, round(100 * (1 - i.sp * i.iv / i.fa), 1), NA_real_)]
  out[, fwd_overlap_source := fifelse(!is.na(fwd_overlap_pct_median), "image pairs", fifelse(!is.na(fwd_est), "speed x interval", ""))]
  out[is.na(fwd_overlap_pct_median), fwd_overlap_pct_median := fwd_est][, fwd_est := NULL][]
}

# Images placed per flight and where their positions came from: one definition for the catalogue (placed_from in
# flights.csv) and the app's checks (pages/fct_checks.R). The image tags (EXIF GPS, n_georef) or the flight log (eMotion
# geotags, geo_images, or the photo tags of a decoded .bbx/.bb3 log, log_photos), whichever places more. A log's photo tag
# places a capture, so every band image of it. placed_from: "photo geotags", "flight log" or "none".
placed_counts <- function(fl) {
  v <- function(k) { x <- if (k %in% names(fl)) suppressWarnings(as.numeric(fl[[k]])) else rep(NA_real_, nrow(fl)); x[!is.finite(x)] <- NA_real_; x }
  n <- v("n_images"); n[is.na(n)] <- 0
  nc <- v("n_captures"); per <- ifelse(!is.na(nc) & nc > 0, n / nc, 1)
  tags <- v("n_georef"); tags[is.na(tags)] <- 0
  logn <- pmin(pmax(v("geo_images"), round(v("log_photos") * per), na.rm = TRUE), n); logn[is.na(logn)] <- 0
  placed <- pmax(tags, logn)
  data.table(n_placed = placed, placed_from = ifelse(placed == 0, "none", ifelse(logn > tags, "flight log", "photo geotags")))
}
