#!/usr/bin/env Rscript
# =============================================================================
#  rs_pipeline.R  --  Remote sensing archive catalogue   (Ovid Core // RS)
# =============================================================================
#  What it does
#    1. Reads the set-up tables (sources, flights, sites, confirmed and rejected links, file rules).
#    2. Scans the sources asked for (drives, folders, backups), one after another: lists every file and reads each image's
#       EXIF/XMP once (DJI, senseFly S.O.D.A., Canon S110 ...); kept per source and folder, so a rescan reads only what changed.
#    3. Gathers every source's last scan, so a drive scanned earlier stays in the catalogue when it is unplugged.
#    4. Groups images into flights (the same flight found twice is one flight with copies), links logs, geotags, MRK/RPT,
#       GNSS and missions with the evidence, and works out how each flight's positions were corrected.
#    5. Writes the catalogue (output/catalogue) and, on request, a viewer to send to people without the archive.
#  Sources are never modified: files are only read, and nothing is written inside them.
#
#  Use
#    Rscript rs_pipeline.R [scan <folder> [id] | build | export]   (see the end of this file)
#  Design: docs/DESIGN.md
# =============================================================================

for (.l in if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8", "UTF-8"))   # UTF-8 text (degC, uS/cm);
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break                               # macOS has no C.UTF-8

RS_VERSION <- "0.6.0"

OVID_APP <- "RS"                                          # its projects go in ~/Desktop/Ovid_Projects/RS/ (core util.R)
CFG <- list(
  raw_dirs       = c("/Volumes/Drive"),             # sources added on the first run when config/sources.csv is empty; read only
  project_dir    = "",       # config/, cache/ and output/ live here (never inside a source)
  flight_gap_min = 10,                           # a pause longer than this between exposures starts a new flight
  points_per_flight = 150)                       # image positions per flight kept in the shareable viewer
if (nzchar(Sys.getenv("RS_RAW_DIRS")))    CFG$raw_dirs    <- strsplit(Sys.getenv("RS_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("RS_PROJECT_DIR"))) CFG$project_dir <- Sys.getenv("RS_PROJECT_DIR")
# Catalogues made before the UAV to RS rename live in ~/Desktop/UAV_project: keep using it until that folder is renamed RS_project
if (!nzchar(Sys.getenv("RS_PROJECT_DIR")) && !dir.exists(path.expand(CFG$project_dir)) && dir.exists(path.expand("~/Desktop/UAV_project")))
  CFG$project_dir <- "~/Desktop/UAV_project"
RUN_ON_SOURCE <- !identical(Sys.getenv("RS_NO_RUN"), "1")

for (p in c("data.table", "jsonlite"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages(library(data.table))

PIPELINE_DIR <- local({ f <- NULL
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { f <- o; break } }
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) getwd() else dirname(normalizePath(f)) })
APP_NAME <- "Ovid Core // RS"
APP_VERSION <- RS_VERSION

CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(PIPELINE_DIR, "core"), file.path(dirname(PIPELINE_DIR), "core"), "~/Desktop/Ovid")
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "util.R"))]
  if (!length(hit)) stop("The shared Ovid Core folder was not found. Looked in: ", paste(d, collapse = ", "), ". Set OVID_CORE to its path.")
  normalizePath(hit[1]) })
for (.f in c("util", "convert", "audit", "standards", "file_audit", "cache", "timezones", "config", "calibration", "table_rules", "manifest", "preflight", "theme"))
  source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
CFG$project_dir <- ovid_project_dir(CFG$project_dir)               # none set: ~/Desktop/Ovid_Projects/<app>/Untitled (core util.R)
for (.f in c("config", "exif", "readers", "read_emotion", "emotion_catalogue", "catalogue", "export", "gnss", "datasets", "gis", "stac", "preflight"))
  source(file.path(PIPELINE_DIR, "R", paste0(.f, ".R")), local = TRUE)
CODE_HASH <- code_hash(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "rs_pipeline.R"))   # every file's code (the run manifest)
# What each cached step depends on (core code_fingerprint()): reading image headers follows the image readers only, so no
# other edit makes a scan read every image again. STEP_HASH is set at the end of this file.
CODE_NAMES <- code_names(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "rs_pipeline.R"))
step_hashes <- function(env = parent.frame()) list(images = code_fingerprint(c("read_image_meta", "read_source_images"), env = env),
                                                    emotion = code_fingerprint(c("emotion_log_summary", "emotion_plan_summary"), env = env))   # senseFly logs and missions

# =============================================================================
#  RUN
# =============================================================================
# A source is added to config/sources.csv the first time it is scanned; its id defaults to the folder or volume name.
add_source <- function(cfgs, cfg, path, source_id = "") {
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  s <- cfgs$sources; want <- path; sid0 <- source_id
  np <- normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE)
  if (any(np == want)) return(cfgs)   # known path (the same real folder): rescan it
  for (k in which(startsWith(want, paste0(np, "/")))) say("  ", path, " is inside source ", s$source_id[k], ": its files are counted under the new source only")
  for (k in which(startsWith(np, paste0(want, "/")))) say("  ", path, " contains source ", s$source_id[k], ": that folder's files stay with ", s$source_id[k])
  # a name already used by another folder does not hijack it: the new folder gets name_2, name_3, ...
  sid <- if (nzchar(source_id)) source_id else safe_name(basename(path)); i <- 1
  base <- sid; while (sid %in% s$source_id) { i <- i + 1; sid <- paste0(base, "_", i) }
  cfgs$sources <- rbind(s, ensure_cols(data.table(source_id = sid, path = path), SOURCE_COLS)[, ..SOURCE_COLS])
  write_cfg(cfgs$sources, cfg$project_dir, "sources"); say("  new source ", sid, " = ", path)
  cfgs
}

# Sources inside other sources (real paths compared): for each source, the folders (relative to it) of the sources inside it.
# Each file belongs to the innermost source that holds it; inner_rules() keeps them out of the outer source's scan and
# drop_inner() out of a scan made before the inner source was added.
inner_sources <- function(s) { if (!nrow(s)) return(list()); np <- normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE)
  setNames(lapply(seq_along(np), function(i) { k <- which(startsWith(np, paste0(np[i], "/"))); substring(np[k], nchar(np[i]) + 2L) }), s$source_id) }
inner_rules <- function(rel) if (!length(rel)) NULL else data.table(pattern = paste0("^\\Q", rel, "\\E/"), action = "inner source", reason = "a source of its own")
drop_inner <- function(A, inner) {
  for (sid in names(inner)) for (r in inner[[sid]]) { off <- function(d) if (is.data.table(d) && nrow(d) && all(c("source_id", "rel") %in% names(d))) d[!(source_id == sid & startsWith(rel, paste0(r, "/")))] else d
    for (k in c("files", "images", "datasets", "gis_layers", "gis_features")) A[[k]] <- off(A[[k]])
    for (k in setdiff(names(A$records), "emotion")) A$records[[k]] <- off(A$records[[k]]) }   # eMotion logs: the eMotion hook-up's own copies check
  A }

# Removes a source: its row in config/sources.csv and its rows in the catalogue tables (its files, photos and the flights it
# holds, with their links and shapes). The source's own files are never touched; its scan cache is kept, so scanning the
# folder again adds it back quickly. A rebuild then finds any of its flights that also have a copy in another source.
remove_source <- function(project_dir, source_id) {
  sid <- source_id; cfg <- modifyList(CFG, list(project_dir = project_dir)); s <- load_configs(cfg)$sources
  if (!sid %in% s$source_id) return(invisible(NULL))
  write_cfg(s[s$source_id != sid], project_dir, "sources", who = "Sources page")
  catd <- file.path(path.expand(project_dir), "output", "catalogue"); rd <- function(f) fread(file.path(catd, f), colClasses = "character")
  gone <- if (file.exists(file.path(catd, "flights.csv"))) { f <- rd("flights.csv"); if ("source_id" %in% names(f)) f[source_id == sid, flight_id] else character() } else character()
  n <- 0L
  for (f in list.files(catd, "\\.csv$")) { x <- rd(f); drop <- rep(FALSE, nrow(x))
    if ("source_id" %in% names(x)) drop <- drop | x$source_id %in% sid
    if ("flight_id" %in% names(x)) drop <- drop | x$flight_id %in% gone
    if (any(drop)) { n <- n + sum(drop); fwrite(x[!drop], file.path(catd, f)) } }
  say("Source ", sid, " removed: ", length(gone), " flight(s), ", num(n), " catalogue rows; no file was touched")
  invisible(list(flights = length(gone), rows = n))
}

# scan: the sources to scan now (paths; NULL = every source in config/sources.csv that is plugged in, or CFG$raw_dirs at first).
# Sources not scanned this time keep their last scan. Then the catalogue is rebuilt from every source's scan; export = TRUE
# also writes the viewer. Duplicated flights (the same flight in two sources or folders) become one flight with copies.
run_pipeline_steps <- function(cfg = CFG, scan = NULL, source_id = "", export = FALSE) {
  t0 <- Sys.time(); cfg <- modifyList(CFG, cfg)
  if (!is.null(cfg$base_offset_h)) BASE_OFFSET <<- cfg$base_offset_h   # Settings > Time zone
  od <- file.path(path.expand(cfg$project_dir), "output"); catd <- file.path(od, "catalogue")
  for (d in c(catd, file.path(od, "audit"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  audit_reset(file.path(od, "audit"))

  say("[1/5] Reading the set-up", col = "accent")
  cfgs <- load_configs(cfg); cfgs$flights <- ensure_cols(cfgs$flights, FLIGHT_COLS)
  if (!nrow(cfgs$sources) && is.null(scan)) scan <- cfg$raw_dirs
  for (p in scan) cfgs <- add_source(cfgs, cfg, p, if (length(scan) == 1) source_id else "")
  sc <- setup_check(cfg); fwrite(sc, file.path(od, "audit", "setup_check.csv"))
  if (any(sc$level == "error")) problem("set-up check", paste(sum(sc$level == "error"), "problem(s) in the set-up; see output/audit/setup_check.csv"))
  pd <- paste0(normalizePath(path.expand(cfg$project_dir), winslash = "/", mustWork = FALSE), "/")
  if (any(startsWith(pd, paste0(normalizePath(path.expand(cfgs$sources$path), winslash = "/", mustWork = FALSE), "/"))))
    stop("The project folder must not be inside a source.")

  say("[2/5] Scanning sources", col = "accent")
  todo <- if (is.null(scan)) cfgs$sources[dir.exists(path.expand(path))]
          else cfgs$sources[normalizePath(path.expand(path), winslash = "/", mustWork = FALSE) %in% normalizePath(path.expand(scan), winslash = "/", mustWork = FALSE)]
  inner <- inner_sources(cfgs$sources)                       # a source inside another: its files are scanned once, under the inner one
  for (i in seq_len(nrow(todo))) scan_source(todo[i], cfg, rbind(cfgs$file_rules, inner_rules(inner[[todo$source_id[i]]]), fill = TRUE))
  if (!nrow(todo)) say("  nothing plugged in to scan; rebuilding from earlier scans")

  say("[3/5] Gathering every source's scan", col = "accent")
  A <- load_sources(cfgs$sources, cfg)
  if (is.null(A)) stop("No source could be read: ", paste(if (length(cfg$raw_dirs)) cfg$raw_dirs else "no source folder is set", collapse = ", "),
                       ". Check that the drive is plugged in, or choose the folder that holds the flights on 1 Project (Sources), then run again. Nothing was changed.")
  A <- apply_file_rules(A, cfgs$file_rules)                   # an ignore rule added since a source was scanned applies without a rescan
  A <- drop_inner(A, inner)                                   # and a source added inside one scanned before it
  # drone photos only (config/drone_cameras.csv, readers.R drone_why): other photos, and raw photos whose camera is not a drone's,
  # are left out and listed in catalogue/other_photos.csv; a raw photo whose EXIF could not be read stays listed as before
  im <- A$images %||% data.table(); op <- data.table(source_id = character(), rel = character(), path = character(), make = character(), model = character(), why = character())
  if (nrow(im)) { for (v in c("raw", "drone_xmp", "make", "model")) if (!v %in% names(im)) set(im, j = v, value = NA)
    im[, why := drone_why(make, model, drone_xmp, cfgs$drone_cameras)]; rw <- im$raw %in% TRUE
    out_i <- nzchar(im$why) & !(rw & is.na(im$make) & is.na(im$model))
    if (any(out_i)) op <- merge(im[out_i, .(source_id, rel, make, model, why)], A$scanned[, .(source_id, root)], by = "source_id", all.x = TRUE)[, .(source_id, rel, path = file.path(root, rel), make, model, why)]
    A$images <- im[!rw & !nzchar(why)][, c("why", "raw") := NULL]
    if (nrow(op)) A$files[paste(source_id, rel) %in% paste(op$source_id, op$rel), status := "other photo"] }
  fwrite(op, file.path(catd, "other_photos.csv"))
  if (nrow(op)) say("  ", num(nrow(op)), " other photos left out: no drone tag and a camera not in drone_cameras.csv (catalogue/other_photos.csv)")
  say("  ", nrow(A$scanned), " source(s): ", num(nrow(A$files)), " files, ", num(nrow(A$images)), " drone photos")
  A$images <- apply_timezones(A$images, cfgs$timezones)       # cameras with a local clock: the time-zone table gives their UTC time

  ds <- if (is.null(A$datasets) || !nrow(A$datasets)) ensure_cols(data.table(), c("source_id", DATASET_COLS, "year"))[0] else A$datasets
  setcolorder(ds, intersect(c("source_id", DATASET_COLS, "year"), names(ds)))
  if (nrow(ds)) ds[, `:=`(frame = frame_of_crs(epsg, crs), crs_status = crs_status(epsg, crs))] else ds[, `:=`(frame = character(), crs_status = character())]
  fwrite(ds, file.path(catd, "datasets.csv"))
  if (nrow(ds) && any(ds$crs_status == "datum unknown")) say("  ", sum(ds$crs_status == "datum unknown"), " dataset(s) state no coordinate system: datum unknown (catalogue/datasets.csv)", col = "yellow")
  gl <- A$gis_layers %||% data.table(); gf <- A$gis_features %||% data.table()
  for (g in list(list(gl, "gis_layers.csv"), list(gf, "gis_features.csv")))   # no GIS files: no file (the app reads a missing one as empty)
    if (ncol(g[[1]])) fwrite(g[[1]], file.path(catd, g[[2]])) else unlink(file.path(catd, g[[2]]))
  if (nrow(gl)) say("  ", nrow(gl), " GIS layers catalogued")
  if (nrow(ds)) say("  ", nrow(ds), " satellite, aerial, LiDAR and raster files catalogued")
  say("[4/5] Grouping flights and linking their files", col = "accent")
  cal <- cfgs$calibration; lim <- function(v, d) { n <- cal_num(cal, v); if (is.finite(n)) n else d }    # config/calibration_options.csv
  b <- build_flights(A$images, cfg$flight_gap_min, A$files, blur_limit_px = lim("blur_limit_px", 1))
  # the same photo in several folders or sources (copies, by its photo key, readers.R): counted once; the copy in the flight's primary
  # folder is kept and the others are listed in catalogue/duplicates.csv (kind, key, kept_path, dup_path, why; the eMotion logs use the same columns)
  dup <- data.table(kind = character(), key = character(), kept_path = character(), dup_path = character(), why = character())
  if ("photo_key" %in% names(b$images) && nrow(b$images)) { ki <- b$images[!is.na(photo_key), .(pk = photo_key, p = paste0(source_id, ":", rel), pr = primary %in% TRUE)][order(pk, -pr, p)]
    ki <- ki[, if (.N > 1L) .(kept_path = p[1], dup_path = p[-1]), by = pk]
    if (nrow(ki)) dup <- ki[, .(kind = "photo", key = pk, kept_path, dup_path, why = fifelse(grepl("|md5:", pk, fixed = TRUE), "same size and first 64 kB", "same size, camera, exposure time and band"))] }
  fwrite(dup, file.path(catd, "duplicates.csv"))
  if (nrow(dup)) say("  ", num(nrow(dup)), " duplicate photos counted once (catalogue/duplicates.csv)")
  if (!nrow(b$flights)) stop("No flights found: no drone images were read", if (nrow(ds)) " (the satellite, aerial and LiDAR list was written)" else "", ".")
  links <- link_files(b$flights, b$folders, A$files, cfgs$links, A$records$geo, b$images, A$records$kml, A$records$rinex, A$records$mrk, A$records$p4d)
  fl <- positioning(b$flights, links, A$records, corrected_m = lim("geotag_corrected_m", CORRECTED_M))
  if (exists("emotion_duplicates") && nrow(ed <- emotion_duplicates(fl, A$records$emotion %||% list()))) {   # eBee log copies (eMotion hook-up)
    fwrite(rbind(dup, ed, fill = TRUE), file.path(catd, "duplicates.csv")); say("  ", num(nrow(ed)), " duplicate flight logs counted once (catalogue/duplicates.csv)") }
  geo <- rbind(geometry(fl, links, A$records$kml, A$records$rinex), emotion_geometry(fl, links, A$records$emotion %||% list()), fill = TRUE)
  # where the images were placed from (placed_counts(), R/catalogue.R; the app's checks use the same): placed from the photos'
  # EXIF GPS (photo geotags), the flight's track comes from them too unless a decoded log drew one (the eMotion hook-up)
  fl[, placed_from := placed_counts(fl)$placed_from]
  log_track <- if ("log_track_points" %in% names(fl)) fifelse(is.na(fl$log_track_points), FALSE, fl$log_track_points > 0) else rep(FALSE, nrow(fl))
  pt <- b$images[primary %in% TRUE & is.finite(lat) & is.finite(lon) & !(lat == 0 & lon == 0) & flight_id %in% fl[placed_from == "photo geotags" & !log_track]$flight_id][order(flight_id, t, file)]
  if (nrow(pt)) { tr <- pt[, if (.N >= 2L) { i <- unique(round(seq(1, .N, length.out = min(.N, 300L)))); .(line = paste(sprintf("%.6f %.6f", lat[i], lon[i]), collapse = ";")) }, by = flight_id]
    if (nrow(tr)) geo <- rbind(geo, tr[, .(flight_id, kind = "observed", what = "track from photo geotags", ref = "", line)], fill = TRUE) }
  say("  ", nrow(fl), " flights (", sum(fl$n_copies > 0), " also found as copies; ", sum(fl$placed_from == "photo geotags"), " placed from photo geotags, ", sum(fl$placed_from == "flight log"), " from a flight log); ", paste(fl[, .N, by = positioning_found][, paste(N, positioning_found)], collapse = ", "))

  say("[5/5] Writing the catalogue", col = "accent")
  new <- setdiff(fl$flight_id, cfgs$flights$flight_id)
  if (length(new)) { cfgs$flights <- rbind(cfgs$flights, ensure_cols(data.table(flight_id = new), FLIGHT_COLS)[, ..FLIGHT_COLS])
    write_cfg(cfgs$flights, cfg$project_dir, "flights") }
  base <- to_base(fl$start_utc, 0)                           # the project's base zone (Settings), as the other apps
  fl[, `:=`(year = as.integer(format(base, "%Y")), date = format(base, "%Y-%m-%d"), start = fmt_ts(base),
            duration_min = round(as.numeric(difftime(end_utc, start_utc, units = "mins")), 1), site_found = site_of(folder))]
  out <- merge(fl, cfgs$flights, by = "flight_id", all.x = TRUE, sort = FALSE)
  out[, site := fifelse(!is.na(site) & nzchar(site), site, site_found)]
  out[, positioning_status := fifelse(!is.na(positioning_status) & nzchar(positioning_status), positioning_status, positioning_found)]
  out[cfgs$sites, project_site := i.project, on = "site"]
  out[, project := fifelse(!is.na(project) & nzchar(project), project, fifelse(is.na(project_site), "", project_site))][, project_site := NULL]
  out <- group_missions(out, links)                          # sorties of one survey (battery swaps, two flights on one battery)
  # multispectral flights: reflectance is comparable only with a calibration panel (and helped by an irradiance sensor)
  if ("multispectral" %in% names(out)) { for (k in c("panel_id", "panel_capture")) if (!k %in% names(out)) out[, (k) := ""]
    out[multispectral %in% TRUE & (is.na(panel_id) | !nzchar(panel_id)), qa_flags := fifelse(is.na(qa_flags) | !nzchar(qa_flags), "multispectral: no calibration panel recorded",
                                                                                         paste0(qa_flags, "; multispectral: no calibration panel recorded"))] }
  # overlap and sun limits, only where the project set one (config/calibration_options.csv; no limit by default)
  addq <- function(i, msg) if (any(i, na.rm = TRUE)) { i <- which(i); out[i, qa_flags := fifelse(is.na(qa_flags) | !nzchar(qa_flags), msg[i], paste0(qa_flags, "; ", msg[i]))] }
  if (!"qa_flags" %in% names(out)) out[, qa_flags := ""]
  for (k in list(list("fwd_overlap_target_pct", "fwd_overlap_pct_median", "forward overlap"), list("side_overlap_target_pct", "side_overlap_pct_median", "side overlap"))) {
    t <- cal_num(cal, k[[1]]); if (is.finite(t) && k[[2]] %in% names(out)) addq(out[[k[[2]]]] < t, sprintf("%s %s %% (target %s %%)", k[[3]], format(round(out[[k[[2]]]])), format(t))) }
  t <- cal_num(cal, "min_sun_elev_deg"); if (is.finite(t) && "sun_elev_min_deg" %in% names(out)) addq(out$sun_elev_min_deg < t, sprintf("sun down to %s\u00b0 (minimum %s\u00b0)", format(out$sun_elev_min_deg), format(t)))
  first <- c("flight_id", "display_name", "year", "date", "start", "duration_min", "project", "site", "mission", "mission_part", "mission_parts", "mission_status", "family", "source_id", "folder", "aircraft", "sensor",
             "flight_no", "n_images", "n_captures", "n_georef", "size_gb", "positioning_status", "positioning_found", "positioning_evidence",
             "has_log", "placed_from", "has_geotags", "has_mrk", "has_rpt", "has_gnss_obs", "has_lidar_raw", "has_mission", "copies", "qa_flags")
  setcolorder(out, intersect(first, names(out))); setorder(out, year, date, start, na.last = TRUE)
  out[, `:=`(start_utc = fmt_ts(start_utc), end_utc = fmt_ts(end_utc))]
  fwrite(out, file.path(catd, "flights.csv")); fwrite(links, file.path(catd, "links.csv")); fwrite(A$scanned, file.path(catd, "sources.csv")); fwrite(geo, file.path(catd, "geometry.csv"))
  fwrite(A$files[, .(source_id, rel, format, family, role, status, size, mtime = fmt_ts(mtime))], file.path(catd, "files.csv"))
  for (k in c("focal_mm", "exposure_s", "gsd_cm", "speed_ms", "blur_px", "fwd_overlap_pct", "sun_elev_deg")) if (!k %in% names(b$images)) set(b$images, j = k, value = NA_real_)
  fwrite(b$images[, .(source_id, rel, flight_id, primary, time_utc = fmt_ts(time_utc), lat, lon, alt, alt_rel, rtk_flag, acc_xy, sensor, camera_serial, size, focal_mm, exposure_s, gsd_cm, speed_ms, blur_px, fwd_overlap_pct, sun_elev_deg)], file.path(catd, "images.csv"))
  un <- A$files[role %in% LINK_ROLES & status == "" & !paste0(source_id, ":", rel) %in% links[state != "suggested"]$file, .(source_id, rel, role)]
  fwrite(un, file.path(catd, "unassigned.csv"))
  fa <- rs_file_audit(A$files, b$images, links, ds, gl); fwrite(fa, file.path(od, "audit", "file_audit.csv"))
  pr <- products_table(cfgs$products, ds, out); fwrite(pr, file.path(catd, "products.csv"))
  fwrite(coverage(out, ds), file.path(catd, "coverage.csv"))
  st <- tryCatch(stac_export(file.path(od, "stac"), out, ds, pr, cfg$viewer_title %||% "Ovid RS archive"), error = function(e) { problem("STAC catalogue", conditionMessage(e)); NULL })
  if (!is.null(st)) say("  STAC catalogue: ", st$collections, " collection(s), ", st$items, " item(s) (output/stac/catalog.json)")
  viewer <- NULL
  if (isTRUE(export)) { say("  writing the viewer", col = "accent")
    viewer <- export_viewer(out, links, b$images, A$scanned, geo, file.path(od, "viewer", "Ovid_RS_viewer"), cfg$viewer_title %||% "Ovid RS archive", datasets = ds, gis = gf) }
  probs <- rbindlist(AUDIT$problems); pf <- file.path(od, "audit", "problems.csv")
  if (nrow(probs)) fwrite(probs, pf) else unlink(pf)          # a clean run leaves no problems from an earlier one
  write_run_manifest(cfg, t0, list(sources = nrow(A$scanned), files = nrow(A$files), images = nrow(A$images), flights = nrow(fl), links = nrow(links)),
                     settings = list(sources = paste(A$scanned$source_id, collapse = ";"), flight_gap_min = cfg$flight_gap_min), packages = c("data.table", "jsonlite"))
  say("Done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s. Catalogue: ", catd, if (!is.null(viewer)) paste0("; viewer: ", viewer), col = "green")
  invisible(list(files = A$files, images = b$images, flights = out, links = links, geometry = geo, unassigned = un, viewer = viewer))
}

# Rscript rs_pipeline.R                      scan every source that is plugged in, rebuild
#        rs_pipeline.R scan <folder> [id]    add (or rescan) one source, rebuild
#        rs_pipeline.R gnss <out.csv> <file>...  stitch GNSS survey CSVs (Emlid Reach, DJI RTK, Leica, generic) into one CSV
#        rs_pipeline.R build                 rebuild from earlier scans only
#        rs_pipeline.R export                rebuild and write the shareable viewer (output/viewer/Ovid_RS_viewer.zip)
run_pipeline <- function(cfg = CFG, ...) { cfg$project_dir <- ovid_project_dir(cfg$project_dir); run_project(cfg, function() run_pipeline_steps(cfg, ...), need_raw = FALSE) }   # folders checked, project locked, run logged (core manifest.R)   # every run leaves output/logs/<start>.log (core manifest.R)
STEP_HASH <- step_hashes(environment())          # after every function is defined
warn_core_shadows(PIPELINE_DIR)                  # an app function that replaces a core one (core/cache.R)
if (RUN_ON_SOURCE && sys.nframe() == 0L) {
  a <- commandArgs(trailingOnly = TRUE); cmd <- if (length(a)) a[1] else ""
  if (cmd == "scan" && length(a) >= 2) run_pipeline(scan = a[2], source_id = if (length(a) >= 3) a[3] else "")
  else if (cmd == "gnss" && length(a) >= 3) { d <- stitch_gnss(a[-(1:2)]); for (p in attr(d, "problems")) say("  ", p); write_gnss(d, a[2]); say(nrow(d), " points -> ", a[2])
    cf <- try_or(load_configs(CFG), NULL, "set-up tables: GNSS base check used the default limits"); b <- gnss_base_check(d, cf$gnss_bases, cf$calibration)
    for (i in which(b$base_check != "ok")) say("  ", b$source_file[i], ": ", b$base_note[i])
    lc <- gnss_limit_check(gnss_quality(d), cf$calibration, cf$gnss_bases); for (i in which(lc$limits != "ok")) say("  ", lc$source_file[i], ": ", lc$limits[i]) }
  else if (cmd == "build") run_pipeline(scan = character())
  else if (cmd == "export") run_pipeline(scan = character(), export = TRUE)
  else run_pipeline()
}
