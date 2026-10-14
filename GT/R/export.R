# R/export.R  --  The export zip: site records with flag columns in any time zone, summaries, metadata, the audit trail and a readme.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.

# Depth columns of an exported table in the project's convention (sgn from depth_sign(); GT's own is down positive).
export_depths <- function(x, sgn = 1) {
  if (sgn == 1 || !NROW(x)) return(x)
  for (v in grep("(^|_)depth(_top|_bottom)?_m$", names(x), value = TRUE)) if (is.numeric(x[[v]])) set(x, j = v, value = depth_typed(x[[v]], sgn))
  x
}

zone_label <- function(h) sprintf("utc%+03d%s", as.integer(trunc(h)), if (h %% 1 != 0) sprintf("%02d", as.integer(round(abs(h %% 1) * 60))) else "")

# One site's temperatures, ready to write: long (one row per time and depth) or wide (a column per depth), at any time
# step, in any zone (stored times are in the base zone BASE_OFFSET; out_offset moves them).
# sgn: the project's depth convention (depth_sign()); depths are written as the project types them.
site_table <- function(d, res = "raw", layout = "long", flags = TRUE, out_offset = BASE_OFFSET, base = BASE_OFFSET, from = NULL, to = NULL, meta = NULL, scheme = "wmo", checked = TRUE,
                       sgn = 1) {
  if (is.null(d) || !nrow(d)) return(data.table())
  if (length(from) && !is.na(from)) d <- d[TIMESTAMP >= as.POSIXct(from, tz = "UTC")]
  if (length(to) && !is.na(to)) d <- d[TIMESTAMP < as.POSIXct(to, tz = "UTC") + 86400]
  if (!nrow(d)) return(data.table())
  wmo <- flags && identical(scheme, "wmo")
  if (wmo) { d <- copy(d); d[, qc := wmo_code(value, flag, checked)] }                  # one WMO code per value (WMO BUFR table 0 33 020)
  x <- if (res == "raw") d[, c("TIMESTAMP", "depth_m", "value", "flag", if (wmo) "qc", "serial", "sensor", "depth_source"), with = FALSE] else {
    r <- resample(d[, c("depth_m", "TIMESTAMP", "value", "flag", if (wmo) "qc"), with = FALSE], res); r[, c("TIMESTAMP", "depth_m", "value", "flag", if (wmo) "qc", "n"), with = FALSE][, value := round(value, 4)] }
  x[, TIMESTAMP := TIMESTAMP + (out_offset - base) * 3600]
  if (sgn != 1) x[, depth_m := depth_typed(depth_m, sgn)]
  tcol <- paste0("timestamp_", zone_label(out_offset))
  if (layout == "wide") {
    w <- dcast(x, TIMESTAMP ~ depth_m, value.var = "value"); dn <- names(w)[-1]; setnames(w, c("TIMESTAMP", paste0("T_", dn, "m")))
    if (flags) { f <- dcast(x, TIMESTAMP ~ depth_m, value.var = "flag", fill = ""); setnames(f, c("TIMESTAMP", paste0("T_", dn, "m_flag"))); w <- merge(w, f, by = "TIMESTAMP")
      ord <- as.vector(rbind(paste0("T_", dn, "m"), paste0("T_", dn, "m_flag")))
      if (wmo) { q <- dcast(x, TIMESTAMP ~ depth_m, value.var = "qc", fill = 7L); setnames(q, c("TIMESTAMP", paste0("T_", dn, "m_qc"))); w <- merge(w, q, by = "TIMESTAMP")
        ord <- as.vector(rbind(paste0("T_", dn, "m"), paste0("T_", dn, "m_flag"), paste0("T_", dn, "m_qc"))) }
      setcolorder(w, c("TIMESTAMP", ord)) }
    w[, TIMESTAMP := fmt_ts(TIMESTAMP)]; setnames(w, "TIMESTAMP", tcol); return(w)
  }
  if (!flags) x[, flag := NULL]
  if (!is.null(meta)) for (v in names(meta)) x[, (v) := meta[[v]]]
  x[, TIMESTAMP := fmt_ts(TIMESTAMP)]; setnames(x, c("TIMESTAMP", "value"), c(tcol, "temp_c"))
  setcolorder(x, intersect(c(names(meta), tcol, "depth_m", "temp_c", "flag", "qc", "serial", "sensor", "depth_source", "n"), names(x)))
  x
}

# What each GT check tells us about a value, for the WMO codes (the generic checks are in core/qc.R). A string of sensors that all
# read the same is not in the ground yet, so its values are wrong; an air-like swing, a flat run or a difference from the
# other sites means the value may still be right.
qc_wmo_register(c(pre_install = 3L, air_like = 2L, differs_from_other_sites = 2L))

# WMO code of each value from its flag text (check names separated by ;). checked = FALSE means no quality rule covers temperature.
wmo_code <- function(value, flag, checked = TRUE) {
  code <- rep(if (checked) 0L else 4L, length(value)); code[is.na(value)] <- 7L
  u <- unique(flag[!is.na(flag) & nzchar(flag)])
  for (z in u) { k <- which(flag == z); code[k] <- qc_wmo_combine(code[k], Reduce(qc_wmo_combine, qc_wmo_class(strsplit(z, ";", fixed = TRUE)[[1]]))) }
  code
}

# One site as a CF-1.10 / ACDD-1.3 NetCDF file (featureType timeSeriesProfile: temperature on time x depth, depth positive
# down, the WMO code of every value beside it), the form GTN-P and the PermafrostNet data recommendations ask for. Times are
# true UTC whatever the zone of the CSV export. NULL when the site has no data in the period.
site_netcdf <- function(d, path, site, meta = list(), res = "raw", checked = TRUE, from = NULL, to = NULL, base = BASE_OFFSET, project_dir = NULL) {
  if (is.null(d) || !nrow(d)) return(NULL)
  if (length(from) && !is.na(from)) d <- d[TIMESTAMP >= as.POSIXct(from, tz = "UTC")]
  if (length(to) && !is.na(to)) d <- d[TIMESTAMP < as.POSIXct(to, tz = "UTC") + 86400]
  if (!nrow(d)) return(NULL)
  d <- copy(d); d[, qc := wmo_code(value, flag, checked)]
  x <- if (res == "raw") d[, .(TIMESTAMP, depth_m, value, qc)] else resample(d[, .(depth_m, TIMESTAMP, value, flag, qc)], res)[, .(TIMESTAMP, depth_m, value, qc)]
  x <- x[!is.na(depth_m), .(value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE), qc = qc_wmo_worst(qc)), by = .(TIMESTAMP, depth_m)]   # two sensors at one depth: their mean
  tt <- sort(unique(x$TIMESTAMP)); zz <- sort(unique(x$depth_m))
  v <- matrix(NA_real_, length(tt), length(zz)); q <- matrix(7L, length(tt), length(zz))
  ij <- cbind(match(x$TIMESTAMP, tt), match(x$depth_m, zz)); v[ij] <- round(x$value, 4); q[ij] <- as.integer(x$qc)
  lat <- suppressWarnings(as.numeric(meta$latitude %||% NA)); lon <- suppressWarnings(as.numeric(meta$longitude %||% NA))
  ts <- nc_time(tt, base); lvl <- proc_level(q)
  vars <- c(list(
    list(name = "time", dims = "time", data = ts, atts = NC_TIME_ATTS),
    list(name = "depth", dims = "depth", data = zz, atts = list(standard_name = "depth", long_name = "depth below the ground surface (negative = above)", units = "m", positive = "down", axis = "Z")),
    list(name = "soil_temperature", dims = c("time", "depth"), data = v,
         atts = list(standard_name = "soil_temperature", long_name = paste("ground temperature,", RES_LABEL[[res]]), units = "degree_Celsius",
                     coordinates = "time depth", `_FillValue` = -9999, ancillary_variables = "soil_temperature_qc",
                     cell_methods = if (res == "raw") "time: point" else "time: mean")),
    list(name = "soil_temperature_qc", dims = c("time", "depth"), data = q, type = "byte", atts = cf_qc_atts("soil_temperature", "soil_temperature"))),
    nc_station_vars(site, lat, lon))
  inst <- meta$instrument %||% ""; lc <- meta$landcover %||% ""
  nc_write(path, c(time = length(tt), depth = length(zz), name_strlen = 64L), vars,
    acdd_atts(paste("Ground temperature at", site), paste0("Ground temperature profile at ", site, if (nzchar(lc)) paste0(" (", lc, ")") else "",
                ", ", RES_LABEL[[res]], ", with a WMO quality control code for every value. Values removed by a quality rule are absent (fill value)."),
              paste0("ovid-gt-", safe_name(site)), ts, lat, lon, "timeSeriesProfile", lvl,
              keywords = c("permafrost", "ground temperature", "active layer", "borehole"), source = if (nzchar(inst)) paste("logger:", inst) else "ground temperature loggers",
              extra = list(geospatial_vertical_min = min(zz), geospatial_vertical_max = max(zz), geospatial_vertical_units = "m", geospatial_vertical_positive = "down"), project_dir = project_dir))
  path
}

# Depths in the CSVs (sites/, soil/, summaries/, metadata/) follow the project's depth_positive, as the set-up tables are
# typed, and the readme says which; the NetCDF files are positive down as CF asks.
build_export <- function(project_dir, sites, zipfile, from = NULL, to = NULL, res = "raw", layout = "long", flags = TRUE, out_offset = BASE_OFFSET,
                         soil = TRUE, summaries = TRUE, audit = TRUE, progress = function(i, n, what) NULL, reports = c("html", "pdf"), flag_scheme = "wmo",
                         netcdf = FALSE, report_opts = NULL, depth_positive = CFG$depth_positive) {
  sgn <- depth_sign(list(depth_positive = depth_positive)); up <- sgn == -1
  od <- out_dir(project_dir); tmp <- file.path(tempdir(), paste0("gtexp_", format(Sys.time(), "%H%M%S"))); dir.create(file.path(tmp, "sites"), recursive = TRUE)
  n_prob <- length(AUDIT$problems)                 # problems from here on go into the export's problems.csv
  sc <- try_or(read_cfg(cfg_path(project_dir, "sites")), data.table(site = character()), "config/sites.csv: site metadata left out of the export")
  qr <- try_or(read_cfg(cfg_path(project_dir, "qc")), data.table(), "config/qc_rules.csv: could not tell whether temperature was quality checked")
  has <- function(x) !is.na(x) & nzchar(x)
  checked <- nrow(qr) > 0 && any(qr$variable == "temp" & (has(qr$min) | has(qr$max) | has(qr$max_spike) | has(qr$max_flat_hours)))   # does any quality rule cover temperature?
  for (i in seq_along(sites)) { s <- sites[i]; progress(i, length(sites), s)
    m <- sc[site == s][1]
    meta <- if (nrow(m) && !is.na(m$site)) list(site = s, site_group = m$site_group, landcover = m$landcover, instrument = m$instrument) else list(site = s)
    t <- site_table(load_site(s, project_dir), res, layout, flags, out_offset, BASE_OFFSET, from, to, if (layout == "long") meta else NULL, flag_scheme, checked, sgn)
    if (nrow(t)) fwrite(t, file.path(tmp, "sites", paste0(safe_name(s), "_ground_temp.csv")))
    if (netcdf) { dir.create(file.path(tmp, "netcdf"), showWarnings = FALSE)
      site_netcdf(load_site(s, project_dir), file.path(tmp, "netcdf", paste0(safe_name(s), "_ground_temp.nc")), s, if (nrow(m)) as.list(m) else list(), res, checked, from, to, project_dir = project_dir) }
    if (soil) { so <- load_soil(s, project_dir); if (!is.null(so) && nrow(so)) { dir.create(file.path(tmp, "soil"), showWarnings = FALSE)
      so <- export_depths(so, sgn); so[, TIMESTAMP := fmt_ts(TIMESTAMP + (out_offset - BASE_OFFSET) * 3600)]; setnames(so, "TIMESTAMP", paste0("timestamp_", zone_label(out_offset)))
      fwrite(so, file.path(tmp, "soil", paste0(safe_name(s), "_soil.csv"))) } }
  }
  if (summaries) { dir.create(file.path(tmp, "summaries"))
    for (f in list.files(file.path(od, "summaries"), "\\.csv$", full.names = TRUE)) { x <- fread(f); if ("site" %in% names(x)) x <- x[site %in% sites]; fwrite(export_depths(x, sgn), file.path(tmp, "summaries", basename(f))) } }
  dir.create(file.path(tmp, "metadata"))
  if (nrow(sc)) fwrite(sc[site %in% sites], file.path(tmp, "metadata", "sites.csv"))
  se <- try_or(read_cfg(cfg_path(project_dir, "sensors")), data.table(), "config/sensors.csv: sensors and depths left out of the export"); if (nrow(se)) { se <- se[site %in% sites]   # as typed ("+0.3" above the ground) into numbers in the project's convention, which a column names
    se[, depth_m := as.character(depth_typed(typed_depth(depth_m, sgn), sgn))][is.na(depth_m), depth_m := ""][, depth_positive := if (up) "up" else "down"]
    fwrite(se, file.path(tmp, "metadata", "sensors_and_depths.csv")) }
  if (audit) { dir.create(file.path(tmp, "audit"))
    for (f in c("audit_index.csv", "flags.csv", "sensor_health.csv", "between_sites.csv", "gaps.csv", "reconciliation.csv", "files.csv", "bundles.csv", "run_manifest.csv")) {
      p <- file.path(od, "audit", f); if (file.exists(p)) { x <- fread(p); if ("site" %in% names(x)) x <- x[is.na(site) | site %in% sites | !nzchar(site)]; fwrite(x, file.path(tmp, "audit", f)) } } }
  if (length(reports)) write_gt_reports(project_dir, file.path(tmp, "reports"), sites, reports, progress = function(i, n, what) progress(i, n, paste("report:", what)), opts = report_opts)
  probs <- problems_since(n_prob); if (nrow(probs)) fwrite(probs, file.path(tmp, "problems.csv"))
  applied_export(project_dir, tmp)                  # every correction applied in the run (core applied.R)
  writeLines(c("Ovid Core // GT export", paste("Made", format(Sys.time(), "%Y-%m-%d %H:%M"), "with version", GT_VERSION), "",
               paste0("Times: ", zone_label(out_offset), " (no daylight saving). The data are stored at ", zone_text(BASE_OFFSET), " and were moved to this zone for the export."),
               paste0("Time step: ", RES_LABEL[[res]], ". Layout: ", layout, "."), "",
               paste0("Depths: ", if (up) "metres above the ground surface positive, below it negative (depth_positive = up, as this project's set-up is typed)"
                      else "metres below the ground surface positive, above it negative (depth_positive = down)", ", in depth_m and the T_<depth>m columns of",
                      " sites/, soil/, summaries/ and metadata/. Thaw depth and active-layer thickness are metres below the surface."), "",
               "sites/      ground temperature, one file per site.",
               "            flag: blank = ok; otherwise the checks that marked the value as suspect (kept), separated by ;",
               if (flags && flag_scheme == "wmo") c("            qc: the WMO quality control code of the value (BUFR table 0 33 020): 0 good, 1 inconsistent, 2 doubtful, 3 wrong,",
                 "                4 not checked, 5 changed, 6 estimated, 7 missing. When several checks apply the worst is given; a mean takes the worst of its values."),
               "            depth_source: manual / registry = given; template / default = ASSUMED (no depth was given for that sensor).",
               if (netcdf) c("netcdf/     the same ground temperatures as CF-1.10 / ACDD-1.3 NetCDF, one file per site (time x depth, depth positive down,",
                 "            times in UTC), each value with its WMO code in soil_temperature_qc (flag_values / flag_meanings). For GTN-P and PermafrostNet."),
               "soil/       soil moisture (vwc, m3/m3) and bulk conductivity (ec, mS/cm), kept apart from temperature.",
               "summaries/  daily means, annual statistics (trumpet curves), thaw depth, active-layer thickness, freeze/thaw dates, zero curtain.",
               "metadata/   sites (landcover, location, installation) and the sensors-and-depths table.",
               "audit/      every value removed and why (audit_index.csv), flags, sensor health, the between-site check, gaps, reconciliation of value counts, files read.",
               if (length(reports)) paste0("reports/    the run report and one report per site (", paste(toupper(reports), collapse = " and "), ")."),
               if (nrow(probs)) "problems.csv  parts of this export that could not be made, and why.", APPLIED_README,
               "", "Values removed by a rule or by hand are not in sites/; they are listed in audit/.", "",
               "Processing level: sites/ and netcdf/ are L1 (quality controlled), or L2 where a calibration changed values; summaries/ are L4 (derived).",
               proc_levels_text()), file.path(tmp, "README.txt"))
  zip::zip(zipfile, list.files(tmp, recursive = TRUE), root = tmp)
  invisible(zipfile)
}
