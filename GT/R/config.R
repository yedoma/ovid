# R/config.R  --  Set-up tables: where they live, their columns, defaults, loading, and import / export of a whole configuration.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# Everything the user decides lives in <project>/config/*.csv, written by the app (or by hand) and read by the pipeline.
# The registry workbook in the metadata folder is read, never written; it seeds these tables on the first run.

SENSOR_COLS   <- c("logger_type", "make", "model", "serial", "sensor", "depth_m", "depth_source", "site", "start", "end", "notes")
SITE_COLS     <- c("site", "site_group", "landcover", "instrument", "status", "utm_zone", "easting", "northing", "latitude", "longitude",
                   "install_date", "drilling_method", "borehole_depth_m", "sensor_type", "sensor_accuracy", "calibration", "description", "source", "profile")
TEMPLATE_COLS <- c("make", "model", "depths", "notes")
TIME_COLS     <- c("serial", "start", "end", "shift_minutes", "reason")
FILE_RULE_COLS <- c("pattern", "action", "serial", "reason", "author")
QC_COLS       <- c("variable", "applies_to", "min", "max", "max_spike", "max_flat_hours", "flat_exempt_band", "action", "notes", "rule_id")
MANUAL_COLS   <- c("site", "serial", "sensor", "depth_m", "variable", "start", "end", "min_value", "max_value", "action", "reason", "author")
CAMPBELL_COLS <- c("pattern", "exclude", "variable", "notes")
AIR_COLS      <- c("site", "file", "time_column", "value_column", "notes")   # air temperature for n-factors: any CSV (a CS export, say); site * for every site
SURFACE_COLS  <- c("site", "date", "surface_change_m", "method", "notes")      # ground surface lowered (subsidence, +) or raised (heave, -) since installation

LANDCOVERS <- c("Plateau", "Bog", "Fen", "Other")
MAKES <- c(rbr = "RBR", geoprecision = "GeoPrecision", hobo = "Onset", em50 = "METER", campbell = "Campbell Scientific")

default_qc <- function() qc_rule_ids(fread(sep = "|", colClasses = "character", na.strings = NULL, text = "
variable|applies_to|min|max|max_spike|max_flat_hours|flat_exempt_band|action|notes
temp|all|-50|50|1|||remove|Limits beyond which a thermistor is open or shorted or its logger is failing. A spike is a single reading that jumps by at least max_spike (and by far more than the sensor usually moves) and comes straight back.
temp|all||||720|0.2|flag|Unchanged for 30 days: a stuck sensor. Readings within 0.2 C of zero are exempt (the zero curtain is real).
vwc|all|0|0.8|0.3|||remove|Volumetric water content, m3/m3.
ec|all|0|20||||remove|Bulk electrical conductivity, mS/cm.
battery|all|2.5|4.2||||flag|GeoPrecision battery, V.
"), "Q")

# The permafrost test (consecutive frozen years) is the setting permafrost_years (Settings, Summaries). It was the one value of
# config/calibration_options.csv; a project that still has that file keeps its value while the setting is not saved.
# Source: van Everdingen, R. (ed.) (1998, rev. 2005), IPA glossary: ground at or below 0 C for at least two consecutive years.
PERMAFROST_YEARS <- 1:3; PERMAFROST_YEARS_DEFAULT <- 2L
CALIB_FILE <- "calibration_options.csv"
# The value of a calibration table: its custom row for all sites (applies_to * or blank), else NA.
gt_calib_years <- function(d) {
  if (is.null(d) || !NROW(d) || !all(c("value", "choice", "number") %in% names(d))) return(NA_integer_)
  at <- if ("applies_to" %in% names(d)) trim(d$applies_to) else rep("*", nrow(d))
  v <- suppressWarnings(as.integer(d$number[d$value == "permafrost_years" & at %in% c("*", "") & tolower(trim(d$choice)) == "custom"]))
  v <- v[!is.na(v) & v %in% PERMAFROST_YEARS]; if (length(v)) v[1] else NA_integer_
}
# The years the test needs: the setting when it is 1, 2 or 3, else the project's calibration_options.csv, else 2.
gt_permafrost_years <- function(value, project_dir = NULL) {
  v <- suppressWarnings(as.integer(value)); if (length(v) == 1 && !is.na(v) && v %in% PERMAFROST_YEARS) return(v)
  p <- if (length(project_dir) == 1 && !is.na(project_dir) && nzchar(project_dir)) file.path(path.expand(project_dir), "config", CALIB_FILE) else ""
  if (nzchar(p) && file.exists(p)) { v <- try_or(gt_calib_years(read_cfg(p)), NA_integer_, CALIB_FILE); if (!is.na(v)) return(v) }
  PERMAFROST_YEARS_DEFAULT
}

# The deployment sheet carries the logger list's columns (core metasheet.R; its From and Until are the sheet's From and To),
# each optional; the corrections over a period carry what manual removals need (the serial when the target is a site, the
# sensors and depths, removals inside a band of values).
GT_MAP <- c(start = "from", end = "to")
gt_sheet_own <- function() setdiff(META_FIELDS$field, c(DEPLOY_COLS, names(GT_MAP)))   # META_FIELDS: R/metadata.R, sourced after this file
GT_PERIOD_EXTRA <- c("serial", "sensor", "depth_m", "min_value", "max_value")

CONFIG_FILES <- list(
  sensors    = list(file = "sensors.csv",          cols = SENSOR_COLS,    label = "Sensors and depths"),
  sites      = list(file = "sites.csv",            cols = SITE_COLS,      label = "Sites", rules = list(choices = list(profile = c("shallow", "deep")))),
  templates  = list(file = "sensor_templates.csv", cols = TEMPLATE_COLS,  label = "Depth templates by model"),
  timezones  = list(file = "timezones.csv",        cols = TZ_COLS,        label = "Time zones"),
  time       = list(file = "time_corrections.csv", cols = TIME_COLS,      label = "Clock corrections",
                    rules = list(required = "serial", known = list(serial = known_from("output/audit/loggers.csv", "serial")))),
  file_rules = list(file = "file_rules.csv",       cols = FILE_RULE_COLS, label = "File rules"),
  qc         = list(file = "qc_rules.csv",         cols = QC_COLS,        label = "Quality rules", default = default_qc, rule_ids = "Q"),
  campbell   = list(file = "campbell_columns.csv", cols = CAMPBELL_COLS,  label = "Campbell columns", default = default_campbell_patterns),
  air        = list(file = "air_temperature.csv",  cols = AIR_COLS,       label = "Air temperature"),
  surface    = list(file = "surface_changes.csv",  cols = SURFACE_COLS,   label = "Ground-surface changes"),
  deploy_sheet = list(file = "deployment_sheet.csv", cols = function() c(DEPLOY_COLS, gt_sheet_own()), optional = gt_sheet_own, label = "Deployment sheet"),
  period     = period_config(GT_PERIOD_EXTRA))

# read_cfg(), ensure_cols(), load_configs(), write_cfg() with a backup of every change, and the import / export of a
# whole configuration are in the shared core (Ovid/config.R).

# A configuration export holds, beside the set-up tables (core export_config()), the deployment sheet's columns and the
# review decisions, copied as they are. An old calibration_options.csv (alone or in a zip) comes in as the permafrost_years
# setting (config_import_more(), called by core read_config_import()).
CONFIG_EXTRA <- c("metadata_fields.csv" = "Deployment sheet columns", "review_decisions.csv" = "Review decisions")
CONFIG_IMPORT_FILES <- CALIB_FILE
config_import_more <- function(out, files) {
  d <- files[[CALIB_FILE]]; v <- if (is.null(d)) NA_integer_ else gt_calib_years(d)
  if (!is.na(v) && !"permafrost_years" %in% out$settings$setting) out$settings <- rbind(out$settings, data.table(setting = "permafrost_years", value = as.character(v)), fill = TRUE)
  if (is.null(out$settings)) out$settings <- data.table(setting = character(), value = character())
  out
}

# ---- tables of an older version, folded into others (core config.R config_migrate()) --------------------------------------
# Manual removals and flags are rows of the corrections over a period (actions remove and flag): the site, else the serial, is
# the target (a site with a serial keeps the serial in its own column), the variable the parameter. The quality rules step
# reads them back in the old shape (gt_period_to_manual()). An old row with neither a site nor a serial applied to nothing:
# kept, switched off. The logger list is part of the deployment sheet.
gt_period_row <- function(n) data.table(target = rep("", n), parameter = "", from = "", to = "", action = "", value = "", on = "", note = "", author = "", added = "",
                                        serial = "", sensor = "", depth_m = "", min_value = "", max_value = "")
gt_manual_to_period <- function(m) { m <- ensure_cols(copy(as.data.table(m)), MANUAL_COLS); r <- gt_period_row(nrow(m))
  st <- trim(m$site); sn <- trim(m$serial); none <- !nzchar(st) & !nzchar(sn); act <- ifelse(tolower(trim(m$action)) == "flag", "flag", "remove")
  r[, `:=`(target = ifelse(nzchar(st), st, sn), serial = ifelse(nzchar(st), sn, ""), sensor = m$sensor, depth_m = m$depth_m, parameter = m$variable, from = m$start, to = m$end,
           action = act, on = ifelse(none, "off", ""), min_value = m$min_value, max_value = m$max_value, author = m$author,
           note = ifelse(none, paste0(m$reason, ifelse(nzchar(m$reason), "; ", ""), "no site or serial: it applied to nothing"), m$reason))]; r[] }
# Back in the old shape: target in site (a site, a serial or a sensor, several with |; blank or * for every logger), with the
# row's number in the table (row).
gt_period_to_manual <- function(p) { p <- ensure_cols(copy(as.data.table(p)), c(PERIOD_COLS, GT_PERIOD_EXTRA))
  data.table(site = p$target, serial = p$serial, sensor = p$sensor, depth_m = p$depth_m, variable = p$parameter, start = p$from, end = p$to, min_value = p$min_value,
             max_value = p$max_value, action = period_action(p$action), reason = p$note, author = p$author, row = if ("row" %in% names(p)) p$row else seq_len(nrow(p))) }
CONFIG_FOLDS <- list(
  list(from = "manual_removals.csv", into = PERIOD_FILE, fold = function(old, cur)
    list(rows = rbind(ensure_cols(as.data.table(cur %||% gt_period_row(0L)), names(gt_period_row(0L))), gt_manual_to_period(old), fill = TRUE), log = NULL)),
  list(from = "metadata.csv", into = DEPLOY_SHEET, fold = function(old, cur) meta_fold(META_FIELDS, gt_deploy_rows, GT_MAP)(old, cur)))

# Every change a run makes to the data on its own (core corrections.R): listed on the Automatic corrections step and in each run's log.
CORRECTIONS <- list(
  list(id = "stray_records", rules = "stray_record", does = "removes", setting = "drop_stray_records", panel = TRUE,
       what = "Readings with corrupt time stamps among good ones (off the time grid and out of order) are left out; each is in the audit trail."),
  list(id = "bad_clock", rules = "impossible_time", does = "removes", setting = "drop_bad_clock", panel = TRUE,
       what = "Readings stamped before the clock-reset year (Settings, Advanced) or in the future are left out as a reset clock (off: kept at the time they carry)."),
  list(id = "base_zone", does = "changes", setting = "", where = "Settings, Time and depths",
       what = "Every time stamp is moved to the base zone (the time zone in Settings)."),
  list(id = "rbr_drift", does = "changes", setting = "correct_rbr_drift", panel = TRUE, default = FALSE,
       what = "Correct RBR clock drift: the clock error found at download is spread over the record."),
  list(id = "clock", does = "changes", table = "time_corrections.csv", where = "Clock corrections", what = "Logger clocks are shifted by the rows of the clock corrections table."),
  list(id = "default_depths", unit = "sensors", does = "fills", setting = "assign_default_depths", panel = TRUE,
       what = "Sensors with no depth are given one: the first at the default first depth, then one step deeper each (Settings, Time and depths). Off: their values are left out until a depth is typed."),
  list(id = "qc_rules", does = "removes", table = "qc_rules.csv", where = "Quality rules", what = "Values outside each rule's limits are removed or flagged, as each row says."),
  list(id = "period", does = "changes", table = PERIOD_FILE, where = "Corrections over a period", what = "Values in each period of the corrections list are removed or flagged (with the quality rules), offset, scaled, drift-corrected or filled by interpolation, after the quality rules."),
  list(id = "gap_fill", does = "fills", setting = "",
       what = "Short daily gaps at depth are estimated (linearly, or from the sensors above and below); estimated days are marked and kept out of annual statistics unless that is switched on."),
  list(id = "fill_in_annual", does = "fills", setting = "fill_in_annual", panel = TRUE, default = FALSE, what = "Annual statistics may use estimated days."),
  list(id = "air_like", rules = "air_like", does = "flags", setting = "check_air_like", panel = TRUE, what = "Flag sensors that swing like air: buried sensors whose daily range is like the air's."),
  list(id = "pre_install", rules = "pre_install", does = "flags", setting = "check_pre_install", panel = TRUE,
       what = "Flag the start of a record where every sensor reads the same (not yet in the ground)."),
  list(id = "check_exports", does = "flags", setting = "check_exports", panel = TRUE,
       what = "Check RBR .hex files against their Ruskin exports (Raw files, Downloads); no value is changed."),
  list(id = "hide_accepted", does = "flags", setting = "hide_accepted_flags", panel = TRUE,
       what = "Flags decided not a problem (Needs you) are left out of the exported flag columns; flags.csv keeps them, marked accepted."),
  list(id = "between_sites", rules = "differs_from_other_sites", does = "flags", setting = "between_site_check", panel = TRUE,
       what = "Days on which a site departs from the other sites at the same depth are flagged."))

# Where a site's name is held (core rename.R): renaming one changes every table here.
# The registry the Simple view's Edit offers names from (the project's registry_dir setting): its file, and the columns that
# hold the serial, the station name and the start of each deployment (the latest one names the serial).
REGISTRY_NAMES <- list(file = "deployments.csv", serial = "serial", name = "station", order = "start")
RENAMES <- list(
  site = list(label = "Site", main = c("sites.csv", "site"),
    cols = list(c("sensors.csv", "site"), c("air_temperature.csv", "site"), c("surface_changes.csv", "site"),
                c("deployment_sheet.csv", "site"), c("deployment_sheet.csv", "station")),
    lists = list(c("qc_rules.csv", "applies_to"))),
  site_group = list(label = "Site group", main = c("sites.csv", "site_group"), cols = list(), lists = list()))
