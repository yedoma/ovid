# R/config.R  --  Set-up tables: where they live, their columns, defaults, and loading.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# Everything the user decides lives in <project>/config/*.csv, written by the app (or by hand) and read by the pipeline.
# The metadata workbook is read as it is and never written to; corrections to it go in these tables instead.

# The deployment sheet carries the logger list's columns (core metasheet.R), each optional; the corrections over a period carry
# what manual removals and drift rows need (removals inside a band of values, a drift's offset at the end, a year).
WLR_SHEET_OWN <- setdiff(META_FIELDS$field, DEPLOY_COLS)
WLR_PERIOD_EXTRA <- c("min_value", "max_value", "value_end", "year")
PERIOD_APP_ACTIONS <- c(drift_tape = "drift from the tape checks", drift_manual = "drift typed in (value at the start, value_end at the end)",
                        drift_none = "no drift correction", drift_keep = "tape offset kept as it is",
                        set_value = "value typed by hand (missing values only)")
WLR_DRIFT_ACTIONS <- c("drift_tape", "drift_manual", "drift_none", "drift_keep")

# Columns added after a table was first written; a blank one means the default (hang_measured_to, a plain shift, an
# orthometric height), so a table without them is read with them empty and no notice.
DATUM_NEWER_COLS <- c("height_type", "frame", "epoch", "geoid_model", "geoid_N_m")

CONFIG_FILES <- list(
  file_rules   = list(file = "file_rules.csv",       cols = function() FILE_RULE_COLS),
  deployments  = list(file = "deployments.csv",      cols = function() DEPLOY_OVERRIDE_COLS, optional = "measured_to"),
  baro_groups  = list(file = "baro_groups.csv",      cols = function() BARO_GROUP_COLS),
  baro_members = list(file = "baro_members.csv",     cols = function() BARO_MEMBER_COLS),
  baro_stations = list(file = "baro_stations.csv",   cols = function() BARO_STATION_COLS),
  baro_corrections = list(file = "baro_corrections.csv", cols = function() BARO_CORR_COLS),
  external_baro = list(file = "external_baro.csv",   cols = function() EXT_BARO_COLS),
  qc           = list(file = "qc_rules.csv",         cols = function() QC_COLS, default = function() default_qc(), rule_ids = "Q"),
  measurements = list(file = "manual_measurements.csv", cols = function() MEAS_COLS),
  oow          = list(file = "out_of_water.csv",     cols = function() OOW_COLS),
  time         = list(file = "time_corrections.csv", cols = function() TIME_COLS, optional = "mode",
                      rules = list(required = "serial", choices = list(mode = c("shift", "drift")), known = list(serial = known_from("output/audit/files.csv", "serial")))),
  timezones    = list(file = "timezones.csv",        cols = function() TZ_COLS),
  gps_points   = list(file = "gps_points.csv",       cols = function() GPS_POINT_COLS),
  datum        = list(file = "datum_manual.csv",     cols = function() DATUM_COLS, optional = DATUM_NEWER_COLS),
  dgps_exclude = list(file = "dgps_exclude.csv",     cols = function() DGPS_EXCLUDE_COLS),
  offsets      = list(file = "logger_offsets.csv",   cols = function() OFFSET_COLS, default = function() default_offsets()),
  accuracy     = list(file = "logger_accuracy.csv",  cols = function() ACCURACY_COLS, default = function() default_accuracy()),
  well_pairs   = list(file = "well_pairs.csv",       cols = function() PAIR_COLS),
  specific_yield = list(file = "specific_yield.csv", cols = function() SY_COLS),
  approvals    = list(file = "approvals.csv",        cols = function() APPROVAL_COLS),
  deploy_sheet = list(file = "deployment_sheet.csv", cols = function() c(DEPLOY_COLS, WLR_SHEET_OWN), optional = WLR_SHEET_OWN),
  period       = period_config(WLR_PERIOD_EXTRA))

# read_cfg(), ensure_cols(), load_configs(), write_cfg() (which keeps a backup of every change) and the configuration
# import / export are in the shared core (Ovid/config.R).

# Every change a run makes to the data on its own (core corrections.R): listed on the Settings page and in each run's log.
CORRECTIONS <- list(
  list(id = "stray_records", rules = "stray_record", does = "removes", setting = "drop_stray_records", panel = TRUE,
       what = "Records with corrupt time stamps among good ones (off the time grid and out of order) are left out; each is in the audit trail."),
  list(id = "bad_clock", rules = c("bad_timestamp", "clock_reset"), does = "removes", setting = "drop_bad_clock", panel = TRUE,
       what = "Records stamped before the first year or in the future, or after the clock jumped back by more than a day within a file, are left out as a reset clock (off: kept at the time they carry; records with no time are always left out)."),
  list(id = "base_zone", does = "changes", setting = "", where = "Settings (base zone)", what = "Every time stamp is moved to the base zone (the time zone in Settings)."),
  list(id = "clock", does = "changes", table = "time_corrections.csv", where = "Clock corrections", what = "Logger clocks are shifted by the rows of the clock corrections table."),
  list(id = "solinst_altitude", does = "changes", setting = "solinst_altitude", on = function(cfg) identical(cfg$solinst_altitude, "undo"), where = "Settings, Units and conversion",
       what = "The water a Solinst Gold or Junior adds for the altitude it was programmed with is taken off again."),
  list(id = "baro_gaps", does = "fills", setting = "baro_interp", panel = TRUE,
       what = "Barometric readings are interpolated linearly across gaps up to the hours under Settings, Advanced (off: only water readings at a barometric reading's time are compensated)."),
  list(id = "baro_fill", rules = "baro_fill", does = "fills", setting = "baro_fill", where = "Settings, Checks",
       what = "Where neither chosen barologger has a reading, another one fills in (shifted onto the primary's level, flagged)."),
  list(id = "baro_corrections", does = "changes", table = "baro_corrections.csv", where = "Barometric compensation", what = "Barologger readings are corrected by the rows of the baro corrections table."),
  list(id = "sensor_offset", does = "changes", setting = "add_sensor_offset", on = function(cfg) isTRUE(cfg$add_sensor_offset), where = "Settings, Units and conversion",
       what = "The tip-to-sensor distance is added where the hang length was measured to the tip."),
  list(id = "drift", rules = "drift_correction", does = "changes", setting = "drift_correction", on = function(cfg) identical(cfg$drift_correction, "apply"), where = "Settings, Checks (drift between tape checks)",
       what = "Water levels are corrected to the tape checks between visits."),
  list(id = "air_trim", rules = "launched_in_air", does = "removes", setting = "auto_trim_air", where = "Settings, Checks",
       what = "Readings taken in the air at the start or end of a deployment are removed (when off they are flagged and listed as suggestions)."),
  list(id = "qc_rules", does = "removes", table = "qc_rules.csv", where = "Quality rules", what = "Values outside each rule's limits are removed or flagged, as each row says."),
  list(id = "period", does = "changes", table = PERIOD_FILE, where = "Corrections over a period", what = "Values in each period of the corrections list are removed (with the limits), offset, scaled, drift-corrected, filled by interpolation or flagged: pressure and temperature before barometric compensation, depth before the elevations are worked out."),
  list(id = "out_of_water", does = "removes", table = "out_of_water.csv", where = "Out of water", what = "Periods the out-of-water table lists are removed."),
  list(id = "hide_accepted", does = "flags", setting = "hide_accepted_flags", panel = TRUE,
       what = "Flags decided not a problem (Needs you) are left out of the exported flag columns; flags.csv keeps them, marked accepted."),
  list(id = "visits", rules = c("service_visit", "out_of_water", "below_air"), does = "flags", setting = "", where = "Settings, Checks (service window, out of water)",
       what = "Readings close to installation, a download or a field visit, with too little water above the sensor, or well below the air pressure are flagged."))

# Where a site's name is held (core rename.R): renaming one changes every table here.
RENAMES <- list(
  site = list(label = "Site", main = c("deployments.csv", "site"),
    cols = list(c("baro_members.csv", "site"), c("manual_measurements.csv", "site"),
                c("out_of_water.csv", "site"), c("gps_points.csv", "site"), c("datum_manual.csv", "site"), c("well_pairs.csv", "site_a"), c("well_pairs.csv", "site_b"),
                c("specific_yield.csv", "site"), c("approvals.csv", "site"), c("deployment_sheet.csv", "site"), c("deployment_sheet.csv", "station")),
    lists = list(c("qc_rules.csv", "applies_to"))),
  baro_station = list(label = "Barometric station", main = c("baro_stations.csv", "station"), cols = list(), lists = list()))

# Set-up files beside the tables that the configuration zip carries as they are (core export_config()): the deployment sheet's
# own columns (core metasheet.R) and the decisions made on Check the data (core workbench.R).
CONFIG_EXTRA <- c("metadata_fields.csv" = "Deployment sheet columns", "review_decisions.csv" = "Review decisions")

# ---- tables of an older version, folded into others (core config.R config_migrate()) --------------------------------------
# Manual removals and drift rows are rows of the corrections over a period; the logger list is part of the deployment sheet.
# A row names its logger and site in target (serial|site); the pipeline reads them back in the old shape (wlr_period_to_*).
# An old row with neither a serial nor a site applied to nothing: it is kept, switched off.
wlr_period_row <- function(n) data.table(target = rep("", n), parameter = "", from = "", to = "", action = "", value = "", on = "", note = "", author = "", added = "",
                                         min_value = "", max_value = "", value_end = "", year = "")
wlr_target <- function(serial, site) vapply(seq_along(serial), function(i) paste(unique(c(trim(serial[i]), trim(site[i]))[nzchar(c(trim(serial[i]), trim(site[i])))]), collapse = "|"), "")
wlr_manual_to_period <- function(m) { m <- ensure_cols(copy(as.data.table(m)), MANUAL_COLS); r <- wlr_period_row(nrow(m)); tg <- wlr_target(m$serial, m$site)
  r[, `:=`(target = tg, parameter = m$parameter, from = m$start, to = m$end, action = "remove", on = ifelse(nzchar(tg), "", "off"),
           note = ifelse(nzchar(tg), m$reason, paste0(m$reason, ifelse(nzchar(m$reason), "; ", ""), "no serial or site: it applied to nothing")),
           author = m$author, min_value = m$min_value, max_value = m$max_value)]; r[] }
wlr_drift_to_period <- function(x) { x <- ensure_cols(copy(as.data.table(x)), DRIFT_COLS); r <- wlr_period_row(nrow(x)); tg <- wlr_target(x$serial, x$site)
  md <- tolower(trim(x$mode)); ok <- md %in% c("tape", "manual", "none", "keep") & nzchar(tg)
  r[, `:=`(target = tg, from = x$start, to = x$end, action = ifelse(md %in% c("tape", "manual", "none", "keep"), paste0("drift_", md), "drift_none"), value = x$offset_m,
           value_end = x$offset_end_m, year = x$year, on = ifelse(ok, "", "off"), author = x$author,
           note = ifelse(ok, x$reason, paste0(x$reason, ifelse(nzchar(x$reason), "; ", ""), ifelse(nzchar(tg), paste0("mode '", x$mode, "' was not known"), "no serial or site"), ": it applied to nothing")))]; r[] }
# Back in the old shape, for apply_manual() and apply_drift(): target in both serial and site (span_rows() and drift_rows()
# read either; a blank target, which applies to every logger, as "*"), with the row's number in the table (row).
wlr_back <- function(p) { p <- ensure_cols(copy(as.data.table(p)), c(PERIOD_COLS, WLR_PERIOD_EXTRA)); tg <- ifelse(nzchar(trim(p$target)), trim(p$target), "*")
  list(p = p, tg = tg, row = if ("row" %in% names(p)) p$row else seq_len(nrow(p))) }
wlr_period_to_manual <- function(p) { b <- wlr_back(p); p <- b$p
  data.table(serial = b$tg, site = b$tg, start = p$from, end = p$to, parameter = p$parameter, min_value = p$min_value, max_value = p$max_value, reason = p$note, author = p$author, row = b$row) }
wlr_period_to_drift <- function(p) { b <- wlr_back(p); p <- b$p
  data.table(serial = b$tg, site = b$tg, year = p$year, mode = sub("^drift_", "", period_action(p$action)), offset_m = p$value, offset_end_m = p$value_end,
             start = p$from, end = p$to, reason = p$note, author = p$author, row = b$row) }
CONFIG_FOLDS <- list(
  list(from = "manual_removals.csv", into = PERIOD_FILE, fold = function(old, cur)
    list(rows = rbind(ensure_cols(as.data.table(cur %||% wlr_period_row(0L)), names(wlr_period_row(0L))), wlr_manual_to_period(old), fill = TRUE), log = NULL)),
  list(from = "drift_corrections.csv", into = PERIOD_FILE, fold = function(old, cur)
    list(rows = rbind(ensure_cols(as.data.table(cur %||% wlr_period_row(0L)), names(wlr_period_row(0L))), wlr_drift_to_period(old), fill = TRUE), log = NULL)),
  list(from = "metadata.csv", into = DEPLOY_SHEET, fold = meta_fold(META_FIELDS, wlr_deploy_rows)))
