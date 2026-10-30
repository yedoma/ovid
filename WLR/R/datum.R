# R/datum.R  --  Elevations from dGPS, the hang length (T_ST), logger offsets, and the manual-measurement check.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# A logger hangs from a reference point: the T-coupler on the black iron pipe (code T, the stable year-to-year datum) or,
# where there is no T-coupler, the top of the well casing (code TOC). T_ST is the length from that point to the tip of the
# logger, and the logger offset is the distance from the tip up to the pressure sensor itself. So
#     sensor elevation   z_sensor = z_ref - T_ST + offset
#     water elevation    z_water  = z_sensor + depth_m             (depth_m = water above the sensor)
#     depth to water     below the reference point = T_ST - offset - depth_m ; below ground (GS) = z_ground - z_water

# ---- dGPS -----------------------------------------------------------------------------------------------
DGPS_CODES <- c("T", "TOC", "GS")

# Emlid-style survey CSV: Name, Location (the code: T / TOC / GS), Elevation and/or Ellipsoidal height, Elevation RMS,
# Solution status, Averaging start ("2026-09-01 12:55:22.8 UTC-06:00"). Only FIX solutions are kept.
read_dgps_file <- function(path) {
  d <- fread(file = path, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
  setnames(d, trim(names(d)))
  col <- function(...) { for (n in c(...)) if (n %in% names(d)) return(trim(d[[n]])); rep(NA_character_, nrow(d)) }
  elev <- suppressWarnings(as.numeric(col("Elevation"))); ell <- suppressWarnings(as.numeric(col("Ellipsoidal height")))
  st <- col("Averaging start", "Start time", "Time")
  off <- vapply(st, function(x) if (is.na(x)) NA_real_ else parse_offset(x), 0, USE.NAMES = FALSE)
  t <- as.POSIXct(substr(st, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  t <- t - (ifelse(is.na(off), BASE_OFFSET, off) - BASE_OFFSET) * 3600
  out <- data.table(gps_name = col("Name"), code = toupper(col("Location", "Code")), z = ifelse(is.na(elev), ell, elev),
                    z_kind = ifelse(is.na(elev), "ellipsoidal", "elevation"), rms = suppressWarnings(as.numeric(col("Elevation RMS"))),
                    time = t, lon = suppressWarnings(as.numeric(col("Longitude"))), lat = suppressWarnings(as.numeric(col("Latitude"))),
                    solution = toupper(col("Solution status")), source = basename(path),
                    h = ell, H = elev, geoid_N = ell - elev, cs_name = col("CS name", "Coordinate system", "CRS"))
  # an ellipsoidal height alone becomes an elevation with the geoid separation of the same survey's other points (H = h - N)
  n_file <- stats::median(out$geoid_N, na.rm = TRUE)
  k <- is.na(out$H) & !is.na(out$h) & is.finite(n_file)
  if (any(k)) out[k, `:=`(z = round(h - n_file, 4), z_kind = "elevation (h - N of the same survey)")]
  sf <- col("Source file"); out[!is.na(sf), source := sf[!is.na(sf)]]
  out <- out[!is.na(gps_name) & nzchar(gps_name) & !is.na(z)]
  dropped <- out[!is.na(solution) & solution != "FIX"]
  if (nrow(dropped)) problem(basename(path), sprintf("%d survey points without a FIX solution were left out", nrow(dropped)))
  out <- out[is.na(solution) | solution == "FIX"]
  # a name like "B103 D T" carries its code at the end when the Location column is empty
  tail_code <- toupper(sub("^.*\\s", "", out$gps_name))
  k <- !(out$code %in% DGPS_CODES) & tail_code %in% DGPS_CODES & grepl("\\s", out$gps_name)
  out[k, `:=`(code = tail_code[k], gps_name = trim(sub("\\s+\\S+$", "", gps_name[k])))]
  u <- ll_to_utm(out$lon, out$lat, UTM_ZONE)        # core convert.R, in the project's UTM zone (Settings), as the workbook coordinates
  out[, `:=`(easting = round(u[, "easting"], 2), northing = round(u[, "northing"], 2))]
  out[]
}

read_dgps <- function(dir) {
  if (!nzchar(dir %||% "") || !dir.exists(path.expand(dir))) return(data.table())
  f <- list.files(path.expand(dir), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) return(data.table())
  rbindlist(lapply(f, function(p) tryCatch(read_dgps_file(p), error = function(e) { problem(basename(p), conditionMessage(e)); NULL })), fill = TRUE)
}

# Which survey name belongs to which site. gps_points.csv holds the user's choices; anything not listed is suggested by
# name (same letters and digits, ignoring case and spaces) and then by distance (within 15 m of the workbook coordinates).
GPS_POINT_COLS <- c("site", "gps_name", "code", "notes")
# survey values typed by hand; height_type elevation (orthometric H, the default) or ellipsoidal (h: geoid_N_m turns it into
# H = h - N); frame (NAD83(CSRS) v8 ...), epoch and geoid_model are kept with the value and shown with the elevation used
DATUM_COLS <- c("site", "code", "elevation_m", "date", "reason", "author", "height_type", "frame", "epoch", "geoid_model", "geoid_N_m")
# The frame named in a coordinate-system label (Emlid's "CS name"), for telling surveys in different frames apart.
frame_of <- function(cs) { x <- toupper(cs %||% ""); x[is.na(x)] <- ""
  ifelse(grepl("CSRS", x), "NAD83(CSRS)", ifelse(grepl("NAD ?83", x), "NAD83", ifelse(grepl("ITRF", x), sub(".*(ITRF ?[0-9]{2,4}).*", "\\1", x),
         ifelse(grepl("WGS ?84|EPSG:4326", x), "WGS 84", "")))) }
DGPS_EXCLUDE_COLS <- c("gps_name", "code", "time", "reason", "author")             # survey points left out (time blank = every survey of that point)

exclude_dgps <- function(gps, ex) {
  if (is.null(ex) || !nrow(ex) || !nrow(gps)) return(gps)
  drop <- rep(FALSE, nrow(gps))
  for (k in seq_len(nrow(ex))) {
    hit <- gps$gps_name == ex$gps_name[k] & (!nzchar(ex$code[k]) | gps$code == toupper(ex$code[k]))
    if (nzchar(ex$time[k])) hit <- hit & format(gps$time, "%Y-%m-%d %H:%M") == substr(fmt_ts(parse_dt(ex$time[k], c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"))), 1, 16)
    drop <- drop | hit
  }
  if (any(drop)) problem("dGPS", sprintf("%d survey point(s) left out by hand", sum(drop)))
  gps[!drop]
}

# How each site's surveyed points vary between surveys and years: one row per survey of a point, with the change from
# the previous survey and from the first, and the spread of all of them.
gps_history <- function(gps, links, tol_m = 0.02) {
  if (!nrow(gps) || !nrow(links)) return(data.table())
  p <- merge(gps[code %in% DGPS_CODES], unique(links[, .(site, gps_name, code)]), by = c("gps_name", "code"), allow.cartesian = TRUE)
  if (!nrow(p)) return(data.table())
  setorder(p, site, code, time)
  p[, `:=`(survey_date = as.Date(time), survey_year = as.integer(format(time, "%Y")), n_surveys = .N, change_from_previous_m = round(z - shift(z), 3),
           change_from_first_m = round(z - z[1], 3), sd_m = round(stats::sd(z), 3), range_m = round(max(z) - min(z), 3)), by = .(site, code)]
  p[, status := fifelse(is.na(change_from_previous_m), "", fifelse(abs(change_from_previous_m) > pmax(tol_m, 2 * sqrt(rms^2 + shift(rms)^2), na.rm = TRUE), "moved more than the survey error", "within the survey error")), by = .(site, code)]
  p[, .(site, code, gps_name, survey_date, survey_year, time, z, rms, z_kind, change_from_previous_m, change_from_first_m, n_surveys, sd_m, range_m, status, source)]
}

squash <- function(x) gsub("bog", "b", gsub("[^a-z0-9]", "", tolower(x)))       # "Bog 101" and "B101" are the same name

suggest_gps_points <- function(sites, gps, chosen = NULL) {
  if (!nrow(gps) || !nrow(sites)) return(data.table(site = character(), gps_name = character(), code = character(), how = character(), distance_m = numeric()))
  g <- unique(gps[code %in% DGPS_CODES, .(easting = mean(easting, na.rm = TRUE), northing = mean(northing, na.rm = TRUE)), by = .(gps_name, code)])
  g[, key := squash(gps_name)]
  out <- rbindlist(lapply(seq_len(nrow(sites)), function(i) {
    s <- sites$site[i]; keys <- squash(c(s, sites$alias[i])); keys <- keys[nzchar(keys)]
    if (!is.null(chosen) && nrow(chosen) && s %in% chosen$site) {
      c0 <- chosen[site == s]; return(data.table(site = s, gps_name = c0$gps_name, code = c0$code, how = "set by hand", distance_m = NA_real_)) }
    d <- sqrt((g$easting - sites$easting[i])^2 + (g$northing - sites$northing[i])^2)
    hit <- which(g$key %in% keys)
    how <- "same name"
    if (!length(hit)) { hit <- which(!is.na(d) & d <= 15); how <- "within 15 m" }
    if (!length(hit)) return(NULL)
    data.table(site = s, gps_name = g$gps_name[hit], code = g$code[hit], how = how, distance_m = round(d[hit], 1))
  }))
  out <- unique(out, by = c("site", "gps_name", "code"))
  # one point per site and code: a same-name match, else the nearest
  out[order(site, code, how != "set by hand", how != "same name", distance_m)][, .SD[if (how[1] == "within 15 m") 1L else which(how == how[1])], by = .(site, code)][, names(out), with = FALSE]
}

# The reference elevation for each deployment: T unless the hang length was measured from the casing (then TOC);
# the survey nearest in time to the end of the deployment; GS nearest in time for the ground.
datum_for <- function(deps, gps, links, manual = NULL, pick = "nearest") {
  pts <- if (nrow(gps) && nrow(links)) merge(gps, unique(links[, .(site, gps_name, code)]), by = c("gps_name", "code")) else data.table()
  if (!is.null(manual) && nrow(manual)) {
    manual <- ensure_cols(copy(manual), DATUM_COLS)
    mm <- manual[nzchar(site) & nzchar(elevation_m), { ell <- tolower(trim(height_type)) == "ellipsoidal"; N <- suppressWarnings(as.numeric(geoid_N_m)); v <- as.numeric(elevation_m)
      .(site, code = toupper(code), z = ifelse(ell & !is.na(N), v - N, v), z_kind = ifelse(ell, ifelse(is.na(N), "ellipsoidal", "elevation (h - N typed)"), "typed"),
        rms = NA_real_, time = as.POSIXct(excel_date(date)), cs_name = trim(gsub(" +", " ", paste(frame, ifelse(!is.na(epoch) & nzchar(epoch), paste0("epoch ", epoch), ""), geoid_model))),   # one per row
        source = paste("typed:", reason), gps_name = "") }]
    pts <- rbind(pts, mm, fill = TRUE)
  }
  if (nrow(pts)) {                # ellipsoidal heights are not mixed with elevations (they differ by the geoid, tens of metres)
    if (!"cs_name" %in% names(pts)) pts[, cs_name := ""]
    mixed <- pts[, .(both = any(z_kind == "ellipsoidal") && any(z_kind != "ellipsoidal")), by = site][both == TRUE, site]
    for (s in mixed) problem(paste("dGPS", s), "ellipsoidal heights left out: this site also has elevations, and the two differ by the geoid (give the geoid separation, or export elevations)")
    pts <- pts[!(site %in% mixed & z_kind == "ellipsoidal")]
    fr <- pts[, .(frames = paste(sort(unique(setdiff(frame_of(cs_name), ""))), collapse = ", ")), by = site][grepl(",", frames)]
    for (i in seq_len(nrow(fr))) problem(paste("dGPS", fr$site[i]), paste0("surveys in different frames (", fr$frames[i], "): their heights can differ by decimetres or more; transform them to one frame before comparing"))
  }
  out <- copy(deps)[, `:=`(ref_code = ifelse(tst_from_toc, "TOC", "T"), z_ref = NA_real_, z_ref_source = "", z_ref_time = as.POSIXct(NA, tz = "UTC"),
                           z_ref_rms = NA_real_, z_kind = "", z_ground = NA_real_, z_ref_code = "")]
  if (!nrow(pts)) return(out)
  setattr(out, "points", pts[, intersect(c("site", "code", "time", "z", "rms", "z_kind"), names(pts)), with = FALSE])     # every survey, for dgps_pick = "interpolate"
  mode <- pick
  pick <- function(s, cd, when) { p <- pts[site == s & code == cd]; if (!nrow(p)) return(NULL)
    if (mode == "latest") return(p[order(-as.numeric(time))][1])
    if (mode == "mean" && nrow(p) > 1) { r <- p[order(-as.numeric(time))][1]
      return(r[, `:=`(z = round(mean(p$z), 4), rms = round(stats::sd(p$z) / sqrt(nrow(p)), 4), source = sprintf("mean of %d surveys", nrow(p)))]) }
    w <- if (is.na(when)) max(p$time, na.rm = TRUE) else when
    p[order(abs(as.numeric(time) - as.numeric(w)))][1] }
  for (i in seq_len(nrow(out))) {
    p <- pick(out$site[i], out$ref_code[i], out$end[i]); why <- ""
    if (is.null(p)) { alt <- setdiff(c("T", "TOC"), out$ref_code[i]); p <- pick(out$site[i], alt, out$end[i])
      if (!is.null(p)) why <- sprintf(" (no %s survey; %s used)", out$ref_code[i], alt) }
    if (!is.null(p)) set(out, i, c("z_ref", "z_ref_code", "z_ref_source", "z_ref_time", "z_ref_rms", "z_kind"),
                         list(p$z, p$code, paste0(p$code, " ", p$gps_name, " ", p$source, why, if (!is.null(p$cs_name) && !is.na(p$cs_name) && nzchar(p$cs_name)) paste0(" [", p$cs_name, "]") else ""), p$time, p$rms, p$z_kind))
    gs <- pick(out$site[i], "GS", out$end[i]); if (!is.null(gs)) set(out, i, "z_ground", gs$z)
  }
  out
}

# ---- logger offsets ----------------------------------------------------------------------------------------
# Distance from the tip of the logger up to the pressure sensor, in metres (config/logger_offsets.csv). Matched on serial
# first, then on model: `model` is a pattern (regular expression, any case) tried against the model name the file states,
# first row first. A blank offset_m is skipped. It is applied only where the hang length (T_ST) was measured to the tip
# (`measured_to` on Deployment history, else the hang_measured_to setting); to leave it out, set it to 0 or measured_to to sensor.
# The seeded rows come from the makers' specifications; `source` says where each value came from.
OFFSET_COLS <- c("serial", "model", "offset_m", "source", "notes")
OLD_OFFSET_NOTE <- "enter the distance from the logger tip to the pressure sensor"      # the all-zero table before 0.11.0
default_offsets <- function() data.table(
  serial = "",
  model = c("U20L", "U20-001", "LT_?EDGE|Levelogger ?Edge|Junior ?Edge", "LT_?5|Levelogger ?5", "LTC", "Junior", "LT_?Gold|Levelogger ?Gold"),
  offset_m = c("0.0125", "0.0125", "", "", "", "", ""),
  source = c("Onset U20L-0x specifications: distance from sensing element to tip 1.25 cm (0.49 in)",
             "Onset U20-001-0x specifications: distance from sensing element to tip 1.25 cm (0.49 in)",
             rep("Solinst Levelogger user guide: the sensor is the machined line just above the pressure holes; no distance is given in its text", 5)),
  notes = c("HOBO U20L water level", "HOBO U20 water level (also -Ti)",
            rep("measure the tip to the machined line and enter it here", 5)))

offset_row <- function(serial, model, tab) {
  if (is.null(tab) || !nrow(tab)) return(NA_integer_)
  v <- suppressWarnings(as.numeric(tab$offset_m))
  k <- which(nzchar(tab$serial) & norm_serial(tab$serial) == serial & !is.na(v))
  if (!length(k) && !is.na(model)) k <- which(!nzchar(tab$serial) & nzchar(tab$model) & !is.na(v) &
                                              # safe: a model pattern that is not a valid regular expression matches no logger
                                              vapply(tab$model, function(m) isTRUE(tryCatch(grepl(m, model, ignore.case = TRUE), error = function(e) FALSE)), logical(1)))
  if (length(k)) k[1] else NA_integer_
}
offset_for <- function(serial, model, tab) { k <- offset_row(serial, model, tab); if (is.na(k)) 0 else as.numeric(tab$offset_m[k]) }

# The known offsets offered in the app's drop-down: the seeded makers' values and any row of the project's table that has a
# value, one entry per model (or serial) and value.
known_offsets <- function(tab = NULL) {
  k <- rbind(default_offsets(), if (!is.null(tab) && nrow(tab)) ensure_cols(copy(tab), OFFSET_COLS)[, OFFSET_COLS, with = FALSE], fill = TRUE)
  k <- k[!is.na(suppressWarnings(as.numeric(offset_m)))]
  k[, offset_m := as.character(as.numeric(offset_m))]
  k <- unique(k, by = c("serial", "model", "offset_m"))
  k[, label := sprintf("%s: %s cm%s", ifelse(nzchar(serial), paste("logger", serial), ifelse(nzchar(notes), notes, model)),
                       format(round(as.numeric(offset_m) * 100, 3)), ifelse(nzchar(source), paste0(" (", source, ")"), ""))]
  k[]
}

# Sets one row of the offsets table, from the app's form: a serial row or a model row, replacing the row already there for it.
# A new row goes first, so it wins over a broader pattern further down. offset_m in metres.
set_offset_row <- function(tab, serial = "", model = "", offset_m, source = "", notes = "") {
  tab <- if (is.null(tab) || !nrow(tab)) empty_cfg(OFFSET_COLS) else ensure_cols(as.data.table(tab), OFFSET_COLS)[, OFFSET_COLS, with = FALSE]
  serial <- trim(serial %||% ""); model <- trim(model %||% "")
  if (!nzchar(serial) && !nzchar(model)) stop("name a model or a serial")
  row <- data.table(serial = serial, model = if (nzchar(serial)) "" else model, offset_m = as.character(round(offset_m, 5)), source = source, notes = notes)
  same <- if (nzchar(serial)) norm_serial(tab$serial) == norm_serial(serial) & nzchar(tab$serial) else !nzchar(tab$serial) & tab$model == model
  if (any(same)) { i <- which(same)[1]; tab[i, (OFFSET_COLS) := row]; return(tab[]) }
  rbind(row, tab)
}

# A project still holding the all-zero table from before 0.11.0 gets the seeded one (write_cfg keeps a backup).
upgrade_offsets <- function(tab) {
  if (is.null(tab) || !nrow(tab) || !all(tab$notes == OLD_OFFSET_NOTE & suppressWarnings(as.numeric(tab$offset_m)) %in% 0)) return(NULL)
  default_offsets()
}

# The tip-to-sensor offset each deployment gets: the table's value where T_ST was measured to the tip, 0 where it was measured
# to the sensor, and 0 everywhere when add_sensor_offset is off (the default). Adds offset_spec_m (the table's value), measured_to, offset_m (applied)
# and offset_source.
apply_sensor_offsets <- function(deps, models, tab, cfg) {
  deps <- copy(deps)
  if (!"measured_to" %in% names(deps)) deps[, measured_to := ""]
  deps[is.na(measured_to) | !nzchar(measured_to), measured_to := tolower(cfg$hang_measured_to %||% "tip")]
  k <- vapply(seq_len(nrow(deps)), function(i) offset_row(deps$serial[i], models[i], tab), 0L)
  deps[, offset_spec_m := ifelse(is.na(k), 0, suppressWarnings(as.numeric(tab$offset_m[k])))]
  deps[, offset_m := ifelse(measured_to == "tip" & isTRUE(cfg$add_sensor_offset), offset_spec_m, 0)]
  deps[, offset_source := ifelse(is.na(k), "no row in logger_offsets.csv", paste0(ifelse(nzchar(tab$serial[k]), paste("serial", tab$serial[k]), paste("model", tab$model[k])),
                                                                                  ifelse(nzchar(tab$source[k] %||% ""), paste0(" (", tab$source[k], ")"), "")))]
  deps[measured_to != "tip", offset_source := paste("hang length to the sensor;", offset_source)]
  if (!isTRUE(cfg$add_sensor_offset)) deps[, offset_source := paste("tip-to-sensor offset switched off (Settings);", offset_source)]
  deps[]
}

# ---- manual check -----------------------------------------------------------------------------------------
# At each field visit the water level was measured by tape from the reference point. Water above the sensor by tape:
#     h_tape = T_ST - offset - T_water                      (from the T-coupler)
#     h_tape = T_ST - offset - TOC_water                    (when T_ST was measured from the casing)
# The logger's own depth just before the visit (median over the readings in the window before the visit time; after it,
# if there are none before) is compared with it. A steady difference points at a wrong T_ST, offset or baro; a drift
# over the years points at the sensor.
manual_check <- function(dep, visits, dt, offset_m, window_h = 3) {
  if (is.null(dt) || !nrow(dt) || !"depth_m" %in% names(dt) || !nrow(visits)) return(NULL)
  v <- visits[!is.na(visit_time)]
  if (!nrow(v)) return(NULL)
  water <- ifelse(dep$tst_from_toc, v$TOC_water, ifelse(is.na(v$T_water) & !is.na(v$T_TOC) & !is.na(v$TOC_water), v$T_TOC + v$TOC_water, v$T_water))
  tst <- if (!is.na(dep$T_ST)) dep$T_ST else v$T_ST
  h_tape <- tst - offset_m - water
  t <- as.numeric(dt$TIMESTAMP); w <- window_h * 3600
  logger <- vapply(as.numeric(v$visit_time), function(x) {
    b <- dt$depth_m[t < x & t >= x - w & !is.na(dt$depth_m)]
    if (!length(b)) b <- dt$depth_m[t > x & t <= x + w & !is.na(dt$depth_m)]
    if (length(b)) stats::median(utils::tail(b, 6)) else NA_real_ }, 0)
  data.table(deployment = dep$deployment, site = dep$site, serial = dep$serial, year = dep$year, visit_time = v$visit_time,
             visit_time_known = v$visit_time_known, reference = ifelse(dep$tst_from_toc, "TOC", "T"), T_ST = tst, offset_m = offset_m,
             tape_to_water = water, h_tape = round(h_tape, 3), h_logger = round(logger, 3), difference_m = round(logger - h_tape, 3))
}
