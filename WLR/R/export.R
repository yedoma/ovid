# R/export.R  --  Loading outputs back (for the app and for analysis), and the export zip.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

# The run manifest (write_run_manifest, file_md5) is in the shared core (Ovid/manifest.R).

# ---- reading outputs back ----------------------------------------------------------------------------------------

read_out <- function(project_dir, file, ...) { f <- out_dir(project_dir, "audit", file)
  if (file.exists(f) && file.info(f)$size > 0) try_or(fread(file = f, ...), data.table(), paste0("output/audit/", file, ": could not be read")) else data.table() }

as_ts <- function(x) as.POSIXct(x, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d"))

fix_ts_cols <- function(dt, cols = c("TIMESTAMP", "start", "end", "first", "last", "first_ts", "last_ts", "visit_time", "z_ref_time", "gap_start", "gap_end", "time")) {
  for (v in intersect(cols, names(dt))) if (!inherits(dt[[v]], "POSIXct")) set(dt, j = v, value = as_ts(ifelse(dt[[v]] == "", NA, dt[[v]])))
  else attr(dt[[v]], "tzone") <- "UTC"
  dt
}

load_deployment <- function(deployment, project_dir) {
  f <- out_dir(project_dir, "sites", paste0(safe_name(deployment), ".csv"))
  if (!file.exists(f)) return(NULL)
  fix_ts_cols(fread(file = f, na.strings = c("", "NA")))
}

# Every deployment at one site, oldest first: one record for the site across years and loggers.
load_site <- function(site, project_dir) {
  s <- read_out(project_dir, "summary.csv"); if (!nrow(s)) return(NULL)
  st <- site; ds <- s[s$site == st][order(start)]$deployment
  d <- rbindlist(lapply(ds, load_deployment, project_dir = project_dir), fill = TRUE)
  if (nrow(d)) d[order(TIMESTAMP)] else NULL
}

load_baro <- function(project_dir) {                       # cleaned record of each barologger, by serial
  f <- list.files(out_dir(project_dir, "baro"), pattern = "\\.csv$", full.names = TRUE); f <- f[!startsWith(basename(f), "station__")]
  setNames(lapply(f, function(x) fix_ts_cols(fread(file = x, na.strings = c("", "NA")))), sub("^.*__", "", tools::file_path_sans_ext(basename(f))))
}
load_baro_stations <- function(project_dir) {              # stitched series of each baro station
  f <- list.files(out_dir(project_dir, "baro"), pattern = "^station__.*\\.csv$", full.names = TRUE)
  setNames(lapply(f, function(x) { d <- fix_ts_cols(fread(file = x, na.strings = c("", "NA"))); d[, baro_serial := as.character(baro_serial)] }),
           sub("^station__", "", tools::file_path_sans_ext(basename(f))))
}

# Values removed by the quality rules for one deployment (from the audit detail), for plotting as red ticks.
removed_values <- function(serial, project_dir, from = NULL, to = NULL) {
  f <- out_dir(project_dir, "audit", "detail", paste0(safe_name(paste0("logger_", serial)), ".csv"))
  if (!file.exists(f)) return(data.table())
  a <- fread(file = f, colClasses = "character", na.strings = "")[level == "cell"]
  if (!nrow(a)) return(data.table())
  a <- a[, .(TIMESTAMP = as_ts(timestamp), parameter = column, rule, reason, old_value = suppressWarnings(as.numeric(old_value)))]
  if (!is.null(from)) a <- a[TIMESTAMP >= from]; if (!is.null(to)) a <- a[TIMESTAMP <= to]
  a
}

# ---- export --------------------------------------------------------------------------------------------------------
# Columns kept for analysis: time, site, the values and a flag column per value (S suspect, R removed, M missing).
EXPORT_VALUES <- c("depth_m", "elev_m", "dtw_m", "wt_ground_m", "temp_c", "spc_uscm", "ec_uscm", "pres_kpa", "baro_kpa")
# What each WLR check tells us about a value, for the WMO codes (WMO BUFR table 0 33 020; the generic checks are in core/qc.R).
# A reading in the air or out of the water is wrong for a water level; the others may still be right. Compensating with the
# secondary barologger leaves the value good; filling the barometric reading from another station makes it an estimate.
qc_wmo_register(c(in_air = 3L, out_of_water = 3L, out_of_water_manual = 3L, below_air = 2L, freezing = 2L, service_visit = 2L,
                  no_baro = 2L, baro_secondary = 0L, baro_fill = 6L))

# One WMO code per value of a deployment table. `tested` names the parameters that have a quality rule; any other is "not checked".
wmo_codes <- function(d, v, rm, tested) {
  code <- rep(if (v %in% tested) 0L else 4L, nrow(d)); code[is.na(d[[v]])] <- 7L
  fc <- paste0("flag_", v)
  if (fc %in% names(d)) { s <- d[[fc]]; u <- unique(s[!is.na(s) & nzchar(s)])
    for (z in u) { cls <- qc_wmo_class(strsplit(z, ";", fixed = TRUE)[[1]]); k <- which(s == z)
      code[k] <- qc_wmo_combine(code[k], Reduce(qc_wmo_combine, cls)) } }
  if (nrow(rm)) { r <- rm[parameter == v | (v %in% c("depth_m", "elev_m", "dtw_m", "wt_ground_m") & parameter == "pres_kpa")]
    if (nrow(r)) { m <- match(fmt_ts(d$TIMESTAMP), fmt_ts(r$TIMESTAMP)); hit <- which(!is.na(m)); code[hit] <- qc_wmo_combine(code[hit], qc_wmo_class(r$rule[m[hit]])) } }
  code
}

export_table <- function(d, serial, project_dir, versions = "both", hide_accepted = TRUE,
                         fl = read_out(project_dir, "flags.csv", colClasses = list(character = c("logger_id", "start", "end")))) {   # flags.csv: a run passes it, read once
  corr <- intersect(c("depth_m_corr", "elev_m_corr", "dtw_m_corr"), names(d))                      # drift-corrected values, present only when a correction was applied (R/drift.R)
  vals <- intersect(c(EXPORT_VALUES, corr), names(d))
  out <- d[, c("TIMESTAMP", "site", "logger_id", "year", intersect("baro_serial", names(d)), vals, intersect("drift_offset_m", names(d))), with = FALSE]
  rm <- removed_values(serial, project_dir)
  if (hide_accepted && nrow(fl) && "site" %in% names(d)) d <- review_unmark(copy(d), fl[(attr(fl, "accepted", exact = TRUE) %||% flag_accepted(fl)) & site == d$site[1] & logger_id %in% c(as.character(serial), as.character(d$logger_id[1]))])   # flags decided not a problem leave the flag columns (core config.R)
  qr <- file.path(path.expand(project_dir), "config", "qc_rules.csv"); rules <- if (file.exists(qr)) fread(file = qr, colClasses = "character", na.strings = "") else data.table()
  has <- function(x) !is.na(x) & nzchar(x)
  tested <- if (nrow(rules)) unique(rules[has(min) | has(max) | has(max_spike) | has(max_flat_hours), parameter]) else character()
  wc <- list()
  for (v in vals) { ov <- sub("_corr$", "", v); fc <- paste0("flag_", ov); f <- if (fc %in% names(d)) ifelse(nzchar(d[[fc]] %||% "") & !is.na(d[[fc]]), "S", "") else rep("", nrow(d))
    if (nrow(rm)) { k <- fmt_ts(d$TIMESTAMP) %in% fmt_ts(rm[parameter == ov | (ov %in% c("depth_m", "elev_m", "dtw_m", "wt_ground_m") & parameter == "pres_kpa"), TIMESTAMP]); f[k] <- "R" }
    f[f == "" & is.na(d[[v]])] <- "M"; set(out, j = paste0(v, "_flag"), value = f); if (!v %in% corr) wc[[v]] <- wmo_codes(d, v, rm, tested) }
  for (v in intersect(c("depth_m", "elev_m", "dtw_m"), vals)) if ("pres_kpa" %in% vals) wc[[v]] <- qc_wmo_combine(wc[[v]], wc$pres_kpa)   # computed from the pressure, so it inherits its flags
  for (v in intersect(c("elev_m", "dtw_m", "wt_ground_m"), vals)) if ("depth_m" %in% vals) wc[[v]] <- qc_wmo_combine(wc[[v]], wc$depth_m)
  for (v in corr) { ov <- sub("_corr$", "", v); ch <- !is.na(d$drift_offset_m) & d$drift_offset_m != 0                  # a corrected value that differs from the logger's is "has been changed" (WMO 5)
    wc[[v]] <- ifelse(is.na(d[[v]]), 7L, ifelse(ch, qc_wmo_combine(wc[[ov]], 5L), wc[[ov]])) }
  for (v in vals) set(out, j = paste0(v, "_qc"), value = wc[[v]])
  out <- drift_versions(out, versions)
  out[, TIMESTAMP := format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")]
  setnames(out, "TIMESTAMP", paste0("TIMESTAMP_UTC", sprintf("%+d", BASE_OFFSET)))
}

make_export_zip <- function(project_dir, zipfile, deployments = NULL, reports = c("html", "pdf"), progress = function(i, n, what) NULL, versions = "both", report_opts = NULL, hide_accepted = TRUE) {
  s <- read_out(project_dir, "summary.csv"); if (!nrow(s)) stop("Nothing to export: run the pipeline first.")
  if (!is.null(deployments)) s <- s[deployment %in% deployments]
  tmp <- file.path(tempdir(), paste0("wlr_export_", format(Sys.time(), "%H%M%S"))); dir.create(file.path(tmp, "data"), recursive = TRUE)
  ap <- read_approvals(project_dir)
  for (i in seq_len(nrow(s))) { d <- load_deployment(s$deployment[i], project_dir); if (is.null(d)) next
    x <- export_table(d, s$serial[i], project_dir, versions, hide_accepted); x[, approval := approval_of(s$site[i], d$TIMESTAMP, ap)]
    fwrite(x, file.path(tmp, "data", paste0(safe_name(s$deployment[i]), ".csv"))) }
  for (f in c(APPLIED_FILE, "summary.csv", "deployments.csv", "manual_check.csv", "measurements.csv", "flags.csv", "audit_index.csv", "problems.csv", "run_manifest.csv", "reconciliation.csv", "files.csv",
              "uncertainty.csv", "barometric_efficiency.csv", "casing_movement.csv"))
    if (file.exists(out_dir(project_dir, "audit", f))) file.copy(out_dir(project_dir, "audit", f), file.path(tmp, f))
  sm <- out_dir(project_dir, "summaries", c("monthly.csv", "water_year.csv", "hydroperiod.csv", "gradients.csv", "recharge.csv")); sm <- sm[file.exists(sm)]
  if (length(sm)) { dir.create(file.path(tmp, "summaries")); file.copy(sm, file.path(tmp, "summaries")) }
  if (length(reports)) write_wlr_reports(project_dir, file.path(tmp, "reports"), unique(s$site), reports, progress = progress, opts = report_opts)
  writeLines(c("Ovid Core // WLR export", paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M")),
               sprintf("Time stamps are UTC%+d with no daylight saving.", BASE_OFFSET),
               "depth_m: water above the pressure sensor (m). elev_m: water elevation from the dGPS datum (m). dtw_m: depth to water below the T-coupler or casing (m).",
               "wt_ground_m: the water table above (+) or below (-) the ground surface (m), from the T-coupler to vegetation height or a ground survey.",
               "approval: approved up to the date in config/approvals.csv for the site (after a review), provisional after it.",
               "uncertainty.csv: the standard uncertainty (1 sigma) of each deployment's depth, depth to water and elevation, and its parts.",
               "barometric_efficiency.csv: how each well answers the air pressure; casing_movement.csv: the casing's height above the ground at each visit.",
               "Flag columns: _flag is S suspect (a check failed, value kept), R removed by a quality rule (blank), M missing in the source.",
               "_qc columns: the WMO quality control code of each value (BUFR table 0 33 020): 0 good, 1 inconsistent, 2 doubtful, 3 wrong, 4 not checked, 5 changed, 6 estimated, 7 missing.",
               "_corr columns and drift_offset_m appear only where a drift or offset correction was applied (it is off unless asked for): depth_m, elev_m and dtw_m keep the logger's own values,",
               paste0("  depth_m_corr, elev_m_corr and dtw_m_corr hold the corrected ones, and drift_offset_m is the offset added to the depth (m). This export: ", c(both = "both versions", original = "the logger's values only", corrected = "corrected values in the usual columns")[[versions]], "."),
               "summaries/: monthly and water-year (from 1 October unless the project sets another start) mean, minimum and maximum of the daily means per site; complete = 90% of the days present;",
               "  sigma_m the largest uncertainty of the site's deployments that season. hydroperiod.csv, gradients.csv (pairs in well_pairs.csv), recharge.csv (specific_yield.csv).",
               proc_levels_text(),
               "measurements.csv: manual measurements against the logger at the same time (difference = logger minus manual).", APPLIED_README,
               if (length(reports)) paste0("reports/: the run report and one report per site (", paste(toupper(reports), collapse = " and "), ").")), file.path(tmp, "README.txt"))
  old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
  zip::zip(zipfile, list.files(".", recursive = TRUE))
  invisible(zipfile)
}

# ---- raw file audit ---------------------------------------------------------------------------------------------------
# One row per raw file found. Readings read = readings that reached the output + readings dropped (each with its reason).
# The join itself is the shared core's file_reconcile() (Ovid/file_audit.R).
file_audit <- function(man, contrib, by_file) {
  files <- man[, .(file = path, name, folder_year = year, kind, format, status, used = use, serial, model, md5, size_bytes = size, first_ts, last_ts,
                   read = ifelse(use, rows, NA_integer_), pair, note)]
  kept <- if (nrow(contrib)) contrib[, .(source_file, in_output = records, used_in)] else data.table(source_file = character(), in_output = integer(), used_in = character())
  fa <- file_reconcile(files, kept, by_file, unit = "readings")
  setnames(fa, "read", "readings_read")
  fa[order(kind, serial, first_ts)]
}

# ---- corrected data for analysis ---------------------------------------------------------------------------------------
# Sites side by side or one under another, every record or hourly / daily means, any period.
corrected_data <- function(project_dir, sites, values = c("depth_m", "temp_c"), res = "raw", from = NULL, to = NULL, layout = "long", flags = TRUE, flag_scheme = "wmo", versions = "both") {
  d <- rbindlist(lapply(sites, function(s) { f <- out_dir(project_dir, "corrected", paste0(safe_name(s), ".csv")); if (!file.exists(f)) return(NULL)
    x <- fread(file = f, na.strings = c("", "NA")); setnames(x, 1, "TIMESTAMP"); x[, TIMESTAMP := as_ts(TIMESTAMP)]; x[, logger_id := as.character(logger_id)]; x }), fill = TRUE)
  if (!nrow(d)) return(data.table())
  if (!is.null(from)) d <- d[TIMESTAMP >= as.POSIXct(format(from), tz = "UTC")]
  if (!is.null(to)) d <- d[TIMESTAMP < as.POSIXct(format(as.Date(to) + 1), tz = "UTC")]
  d <- drift_versions(d, versions)                                       # both / original / corrected, for sites that were drift corrected
  values <- intersect(values, names(d))
  if (identical(versions, "both")) values <- c(values, intersect(c(paste0(values, "_corr"), "drift_offset_m"), names(d)))
  fl <- intersect(paste0(values, if (identical(flag_scheme, "wmo")) "_qc" else "_flag"), names(d))
  keep <- c("TIMESTAMP", "site", "logger_id", "year", values, if (flags && res == "raw") fl)
  d <- d[, intersect(keep, names(d)), with = FALSE]
  if (res != "raw") {
    step <- c(hour = 3600, day = 86400)[res]
    d[, TIMESTAMP := as.POSIXct(floor(as.numeric(TIMESTAMP) / step) * step, origin = "1970-01-01", tz = "UTC")]
    d <- d[, c(list(readings = .N), lapply(.SD, function(x) if (all(is.na(x))) NA_real_ else round(mean(x, na.rm = TRUE), 4))), by = .(TIMESTAMP, site), .SDcols = values]
  }
  ap <- read_approvals(project_dir); d[, approval := approval_of(site[1], TIMESTAMP, ap), by = site]
  if (layout == "wide") {
    d <- dcast(d, TIMESTAMP ~ site, value.var = values, sep = "__")
  }
  setorderv(d, intersect(c("TIMESTAMP", "site"), names(d)))
  d[, TIMESTAMP := format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")]
  setnames(d, "TIMESTAMP", paste0("TIMESTAMP_UTC", sprintf("%+d", BASE_OFFSET)))
}

# ---- bulk export: one file per site ---------------------------------------------------------------------------------------
# Every site (or the ones given) at once, as a zip with one corrected-data CSV per site, the same table of all of them together,
# and a readme. The choices are those of corrected_data().
corrected_data_zip <- function(project_dir, sites, zipfile, values = c("depth_m", "elev_m", "temp_c"), res = "raw", from = NULL, to = NULL, layout = "long",
                               flags = TRUE, flag_scheme = "wmo", versions = "both", progress = function(i, n, what) NULL, netcdf = FALSE) {
  tmp <- file.path(tempdir(), paste0("wlr_bulk_", format(Sys.time(), "%H%M%S"))); dir.create(file.path(tmp, "sites"), recursive = TRUE); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  all <- list(); wrote <- character()
  for (i in seq_along(sites)) { st <- sites[i]; progress(i, length(sites), st)
    x <- corrected_data(project_dir, st, values, res, from, to, layout, flags, flag_scheme, versions)
    if (!is.null(x) && nrow(x)) { fwrite(x, file.path(tmp, "sites", paste0(safe_name(st), ".csv"))); wrote <- c(wrote, st); if (layout == "long") all[[st]] <- x }
    if (netcdf) { dir.create(file.path(tmp, "netcdf"), showWarnings = FALSE); site_netcdf(project_dir, st, file.path(tmp, "netcdf", paste0(safe_name(st), ".nc")), values, res, from, to) } }
  if (!length(wrote)) stop("Nothing to export for this choice.")
  if (length(all)) fwrite(rbindlist(all, fill = TRUE), file.path(tmp, "all_sites.csv"))
  applied_export(project_dir, tmp)                  # every correction applied in the run (core applied.R)
  writeLines(c("Ovid Core // WLR bulk export", paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M")), sprintf("Time stamps are UTC%+d with no daylight saving.", BASE_OFFSET), "",
               paste0("sites/: one CSV per site (", length(wrote), " sites). ", if (length(all)) "all_sites.csv: the same rows for every site in one file." else "Wide layout: one file per site only."),
               paste0("Time step: ", c(raw = "every reading", hour = "hourly mean", day = "daily mean")[[res]], ". Layout: ", layout, ". Values: ", paste(values, collapse = ", "), "."),
               "depth_m: water above the pressure sensor (m). elev_m: water elevation from the dGPS datum (m). dtw_m: depth to water below the reference point (m).",
               "approval: approved up to the date in config/approvals.csv for the site, provisional after it.",
               if (netcdf) "netcdf/: each site as CF-1.10 / ACDD-1.3 NetCDF (times in UTC; every value with its WMO code, its uncertainty and its approval).",
               if (flags) c(if (identical(flag_scheme, "wmo")) "_qc columns: the WMO quality control code (BUFR table 0 33 020): 0 good, 1 inconsistent, 2 doubtful, 3 wrong, 4 not checked, 5 changed, 6 estimated, 7 missing."
                            else "_flag columns: S suspect (kept), R removed by a quality rule (blank), M missing in the source."),
               paste0("Drift and offset correction (off unless asked for): ", c(both = "both versions are given, the logger's own values and the corrected ones in _corr columns, with drift_offset_m.",
                      original = "the logger's own values only.", corrected = "the corrected values are in the usual columns wherever a correction was made.")[[versions]]), APPLIED_README), file.path(tmp, "README.txt"))
  zipfile <- normalizePath(zipfile, mustWork = FALSE); unlink(zipfile)
  old <- setwd(tmp); on.exit(setwd(old), add = TRUE, after = FALSE)
  zip::zip(zipfile, list.files(".", recursive = TRUE))
  invisible(zipfile)
}

# ---- approvals and NetCDF --------------------------------------------------------------------------------------------------
read_approvals <- function(project_dir) { f <- file.path(path.expand(project_dir), "config", "approvals.csv")
  if (file.exists(f)) try_or(fread(file = f, colClasses = "character", na.strings = NULL), where = "approvals.csv") else NULL }

# What each value is in CF terms. Only names from the CF standard name table are given; the rest have a long name only.
NC_WLR <- list(
  depth_m = list(long_name = "depth of water above the pressure sensor", units = "m"),
  elev_m = list(standard_name = "water_surface_height_above_reference_datum", long_name = "water level elevation above the survey datum", units = "m"),
  dtw_m = list(long_name = "depth to water below the reference point (T-coupler or top of casing)", units = "m", positive = "down"),
  wt_ground_m = list(long_name = "water table above (+) or below (-) the ground surface", units = "m"),
  temp_c = list(long_name = "water temperature at the sensor", units = "degree_Celsius"),
  spc_uscm = list(long_name = "specific conductance at 25 degrees Celsius", units = "uS cm-1"),
  ec_uscm = list(long_name = "electrical conductivity", units = "uS cm-1"),
  pres_kpa = list(long_name = "absolute pressure at the sensor", units = "kPa"),
  baro_kpa = list(standard_name = "surface_air_pressure", long_name = "barometric pressure used for compensation", units = "kPa"))
SIGMA_COL <- c(depth_m = "depth_sigma_m", dtw_m = "dtw_sigma_m", wt_ground_m = "dtw_sigma_m", elev_m = "elev_sigma_m")

# One site as a CF-1.10 / ACDD-1.3 NetCDF file (featureType timeSeries): each value with its WMO code (<v>_qc, CF flags), its
# standard uncertainty where the run gave one (<v>_sigma, from uncertainty.csv by deployment), and the approval state of
# every time. Times in true UTC. Values are those of corrected_data() (the logger's own, not drift-corrected, unless asked).
site_netcdf <- function(project_dir, site, path, values = c("depth_m", "elev_m", "temp_c"), res = "raw", from = NULL, to = NULL) {
  d <- corrected_data(project_dir, site, unique(c(values)), res, from, to, "long", TRUE, "wmo", "original")
  if (is.null(d) || !nrow(d)) return(NULL)
  setnames(d, 1, "TIMESTAMP"); d[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]
  if (res == "raw") { f <- out_dir(project_dir, "corrected", paste0(safe_name(site), ".csv"))      # the deployment of each time, for its uncertainty
    raw <- fread(file = f, na.strings = c("", "NA"), select = c(1, which(names(fread(file = f, nrows = 0)) %in% c("logger_id", "year")))); setnames(raw, 1, "TIMESTAMP"); raw[, TIMESTAMP := as_ts(TIMESTAMP)]
    d <- merge(d, unique(raw[, .(TIMESTAMP, logger_id = as.character(logger_id), year)], by = "TIMESTAMP"), by = "TIMESTAMP", all.x = TRUE, suffixes = c("", ".r")) }
  s0 <- site; u <- read_out(project_dir, "uncertainty.csv"); if (nrow(u)) u <- u[site == s0]
  vals <- intersect(values, intersect(names(NC_WLR), names(d)))
  t <- nc_time(d$TIMESTAMP, BASE_OFFSET); n <- nrow(d); vars <- list(list(name = "time", dims = "time", data = t, atts = NC_TIME_ATTS)); codes <- integer()
  for (v in vals) {
    qc <- if (paste0(v, "_qc") %in% names(d)) as.integer(d[[paste0(v, "_qc")]]) else ifelse(is.na(d[[v]]), 7L, 4L); codes <- c(codes, qc)
    sg <- NULL
    if (res == "raw" && nrow(u) && v %in% names(SIGMA_COL) && SIGMA_COL[[v]] %in% names(u) && all(c("logger_id", "year") %in% names(d))) {
      k <- match(paste(d$logger_id, d$year), paste(as.character(u$serial), u$year)); sg <- as.numeric(u[[SIGMA_COL[[v]]]])[k] }
    anc <- paste(c(paste0(v, "_qc"), if (!is.null(sg) && any(is.finite(sg))) paste0(v, "_sigma")), collapse = " ")
    vars[[length(vars) + 1]] <- list(name = v, dims = "time", data = d[[v]], atts = c(NC_WLR[[v]], list(`_FillValue` = -9999, coordinates = "time lat lon", ancillary_variables = anc,
                                     cell_methods = if (res == "raw") "time: point" else "time: mean")))
    vars[[length(vars) + 1]] <- list(name = paste0(v, "_qc"), dims = "time", data = qc, type = "byte", atts = cf_qc_atts(v, NC_WLR[[v]]$standard_name))
    if (!is.null(sg) && any(is.finite(sg))) vars[[length(vars) + 1]] <- list(name = paste0(v, "_sigma"), dims = "time", data = round(sg, 4),
      atts = c(list(long_name = paste("standard uncertainty (1 sigma) of", v), units = "m", `_FillValue` = -9999),
               if (!is.null(NC_WLR[[v]]$standard_name)) list(standard_name = paste(NC_WLR[[v]]$standard_name, "standard_error")))) }
  vars[[length(vars) + 1]] <- list(name = "approval", dims = "time", data = as.integer(d$approval == "approved"), type = "byte",
    atts = list(long_name = "approval state of the record", flag_values = nc_byte(0:1), flag_meanings = "provisional approved"))
  ss <- read_out(project_dir, "summary.csv"); dep <- read_out(project_dir, "deployments.csv")
  e <- if (nrow(dep) && all(c("easting", "northing") %in% names(dep))) dep[site == s0 & is.finite(easting) & is.finite(northing)][1] else data.table()
  ll <- if (nrow(e)) try_or(utm_to_ll(e$easting, e$northing, UTM_ZONE), NULL, paste0(site, ": NetCDF position left blank")) else NULL   # core convert.R
  lat <- if (!is.null(ll)) round(ll[1, "lat"], 6) else NA_real_; lon <- if (!is.null(ll)) round(ll[1, "lon"], 6) else NA_real_
  vars <- c(vars, nc_station_vars(site, lat, lon))
  nc_write(path, c(time = n, name_strlen = 64L), vars,
    acdd_atts(paste("Water level at", site), paste0("Barometrically compensated water level and temperature at ", site, " from pressure loggers, ",
                c(raw = "every reading", hour = "hourly means", day = "daily means")[[res]], ", with a WMO quality control code, a standard uncertainty and the approval state of every value. ",
                "Values removed by a quality rule are absent (fill value)."),
              paste0("ovid-wlr-", safe_name(site)), t, lat, lon, "timeSeries", proc_level(codes, approved = nrow(d) > 0 && all(d$approval == "approved")),
              keywords = c("groundwater", "water table", "peatland", "water level"), source = "pressure loggers (HOBO, Solinst), barometric compensation",
              extra = list(approval = if (all(d$approval == "approved")) "approved" else if (any(d$approval == "approved")) "approved in part" else "provisional"), project_dir = project_dir))
  path
}
