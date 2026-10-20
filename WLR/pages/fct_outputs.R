# WLR: the output tables the pages read (columns, serials) and load_outputs(). Sourced by app.R.
SERIAL_COLS <- c("serial", "logger_id", "primary", "secondary", "baro_serial", "serial_raw", "file_serial", "meta_serial")
# The columns pages read from output tables a run may write empty (no deployment processed yet).
OUT_COLS <- list(`summary.csv` = c("deployment", "site", "serial", "year", "kind", "start", "end", "records", "baro_group", "primary", "secondary", "baro_primary_share", "baro_missing",
                                   "baro_filled", "depth_min", "depth_median", "depth_max", "below_air_share", "T_ST", "T_ST_source", "reference", "z_ref", "measured_to", "offset_m",
                                   "offset_source", "tape_diff_median"),
                 `flags.csv` = c("site", "logger_id", "parameter", "start", "end", "records", "check", "detail", "rule_id"),
                 `baro_suggestions.csv` = c("serial", "group", "baro_site", "coverage", "distance_km", "interval_s", "departure_kpa", "score", "year"),
                 `baro_compare.csv` = c("day", "diff_kpa", "pair"),
                 `dgps.csv` = c("gps_name", "code", "z", "z_kind", "rms", "time", "lon", "lat", "solution", "source", "easting", "northing"),
                 `measurements.csv` = c("site", "parameter", "time", "manual"))
load_outputs <- function(pd) {
  a <- file.path(pd, "output", "audit")
  if (!file.exists(file.path(a, "summary.csv"))) return(NULL)
  rd <- function(f, ts = TRUE) { p <- file.path(a, f)
    # safe: an output table that cannot be read shows as empty until the next run writes it
    d <- if (file.exists(p) && file.info(p)$size > 0) tryCatch(fread(p, na.strings = c("", "NA")), error = function(e) data.table()) else data.table()
    if (!is.null(OUT_COLS[[f]])) for (v in setdiff(OUT_COLS[[f]], names(d))) set(d, j = v, value = if (nrow(d)) NA else logical())   # a run that wrote none (every logger waiting in Needs you)
    for (v in intersect(SERIAL_COLS, names(d))) set(d, j = v, value = as.character(d[[v]]))
    if (ts && nrow(d)) fix_ts_cols(d) else d }
  deps <- rd("deployments.csv")                                         # a run before 0.11.0 has no measured_to or offset_source
  if (nrow(deps)) for (v in setdiff(c("measured_to", "offset_source"), names(deps))) set(deps, j = v, value = NA_character_)
  list(files = rd("files.csv"), loggers = rd("loggers.csv"), deps = deps, summary = rd("summary.csv"), checks = rd("manual_check.csv"), meas = rd("measurements.csv"),
       flags = rd("flags.csv", FALSE), index = rd("audit_index.csv", FALSE), problems = rd("problems.csv", FALSE), recon = rd("reconciliation.csv", FALSE),
       sugg = rd("baro_suggestions.csv", FALSE), bcmp = rd("baro_compare.csv", FALSE), gaps = rd("gaps.csv"), md = rd("metadata.csv"), dgps = rd("dgps.csv"),
       links = rd("gps_links.csv", FALSE), overlaps = rd("overlaps.csv"), seams = rd("baro_seams.csv"), ghist = rd("dgps_history.csv"), air = rd("air_runs.csv"),
       fileaudit = rd("file_audit.csv"), manifest = rd("run_manifest.csv", FALSE), runs = rd("run_history.csv", FALSE),
       when = file.mtime(file.path(a, "summary.csv")))
}

# The run's suggested barologgers for one processing year as rows of baro_groups.csv: the best-scored for each group as
# primary, the next as secondary. The Barometric page's pickers start from these for a year with no saved groups.
bo_suggested_groups <- function(sugg, y) {
  empty <- data.frame(year = character(), group = character(), primary = character(), secondary = character(), notes = character(), stringsAsFactors = FALSE)
  if (is.null(sugg) || !nrow(sugg)) return(empty)
  sc <- as.data.table(sugg)[as.character(year) == as.character(y)]; if (!nrow(sc)) return(empty)
  best <- sc[order(group, -score)][, .(primary = as.character(serial[1]), secondary = if (.N > 1) as.character(serial[2]) else ""), by = group]
  data.frame(year = as.character(y), group = best$group, primary = best$primary, secondary = best$secondary, notes = "suggested", stringsAsFactors = FALSE)
}
