# WLR: set-up table editors, background runs, open sessions and the plot choices. Sourced by app.R.
# =============================================================================
#  Editable set-up table (one module for every config CSV)
# =============================================================================
log_change <- function(path, new, who = "") backup_cfg(path, new, who)   # Ovid/config.R: keeps the old version in config/history

# Every set-up table the interfaces edit: module id, the table (CONFIG_FILES), its file and what it holds. Both interfaces
# build their editors from this list; a saved change to any of them needs a run to apply.
SETUP_TABLES <- data.table(
  id    = c("deps", "bmem", "bstn", "bcor", "gex", "qc", "oow", "meas", "time", "tzt", "gpsp", "datm", "offs", "frules", "xbaro", "acc", "pairs", "sy", "appr", "depsheet", "period"),
  cfg   = c("deployments", "baro_members", "baro_stations", "baro_corrections", "dgps_exclude", "qc", "oow", "measurements", "time",
            "timezones", "gps_points", "datum", "offsets", "file_rules", "external_baro", "accuracy", "well_pairs", "specific_yield", "approvals", "deploy_sheet", "period"),
  label = c("Corrections to the workbook", "Sites in each baro group", "Baro stations", "Barologger checks", "Surveys left out", "Quality rules",
            "Out of water", "Manual measurements", "Clock corrections", "Time-zone table", "Survey points set by hand", "Elevations typed by hand",
            "Logger offsets", "File sorting rules", "Other barometric records", "Logger accuracy", "Pairs of wells", "Specific yield", "Approvals",
            "Deployment sheet", "Corrections over a period"))
SETUP_TABLES[, file := vapply(cfg, function(k) CONFIG_FILES[[k]]$file, "")]
# skip: tables another editor keeps (the deployment sheet: meta_sheet_server() in app.R), so no two editors save one file.
setup_editors <- function(cfgp, tick, mark, skip = character()) { i <- which(!SETUP_TABLES$id %in% skip)
  setNames(lapply(i, function(i) editServer(SETUP_TABLES$id[i], cfgp(SETUP_TABLES$cfg[i]), tick, on_change = mark)), SETUP_TABLES$id[i]) }
# The tables the Workbench lists: the set-up tables (the deployment sheet with the logger list's columns, core/metasheet_ui.R),
# and the barologgers by year, set with the per-year pickers on Barometric (its editor is made in app.R, with no table on screen).
SETUP_TREE <- rbind(SETUP_TABLES, data.table(id = "bgrp", cfg = "baro_groups", label = "Barologgers by year", file = CONFIG_FILES$baro_groups$file))
LOGGER_LIST_INTRO <- paste(DEPLOY_SHEET_INTRO, "Instead of (or as well as) a WL_Metadata workbook, the optional columns say what you know about each logger:",
  "a name, a year, a type or a hang length are used when given. Start from the loggers found, paste rows from a spreadsheet,",
  "or import one, and add columns of your own with Columns. Leave the year blank for every year the logger has files.")

# The Automatic corrections step's status line (ovid_steps): how many of the switched corrections are on, as the list shows them (core corrections.R).
corrections_status <- function(cfg) {
  # safe: the corrections' state is a label only
  t <- tryCatch(corrections_table(cfg), error = function(e) NULL); if (is.null(t) || !nrow(t)) return(list("off", ""))
  n_on <- sum(t$state == "on"); n_off <- sum(t$state == "off")
  if (!n_on && !n_off) list("info", "") else if (n_off) list("info", sprintf("%d on, %d off", n_on, n_off)) else list("ok", sprintf("All %d on", n_on))
}

# A configuration's settings.csv (setting, value) as settings: lists joined by ";" split again, TRUE and FALSE as switches.
config_settings_list <- function(tab, keys) {
  if (is.null(tab) || !nrow(tab) || !all(c("setting", "value") %in% names(tab))) return(list())
  tab <- tab[tab$setting %in% keys, ]; v <- trimws(as.character(tab$value)); v[is.na(v)] <- ""
  out <- stats::setNames(lapply(v, function(x) if (x %in% c("TRUE", "FALSE")) as.logical(x) else x), tab$setting)
  for (k in intersect(c("raw_dirs", "years"), names(out))) out[[k]] <- strsplit(as.character(out[[k]]), ";", fixed = TRUE)[[1]]
  out
}

# What the last run redid, from its manifest: water loggers whose record and set-up had not changed are reused.
run_redone <- function(pd) {
  f <- file.path(path.expand(pd), "output", "audit", "run_manifest.csv"); if (!file.exists(f)) return("")
  m <- tryCatch(fread(f, colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
  v <- function(k) suppressWarnings(as.integer(m[key == k, value][1]))
  n <- v("water_loggers"); r <- v("water_loggers_reused"); if (anyNA(c(n, r))) return("")
  wb_redone(n - r, n, "water loggers")
}

# set with setNames, not as "label" = value: R turns argument names into the native text encoding when it reads
# the file, so on a session that is not UTF-8 the degree sign would show as <U+00B0>
param_choices <- setNames(c("depth_m", "elev_m", "dtw_m", "pres_kpa", "baro_kpa", "dp_kpa", "temp_c", "spc_uscm", "ec_uscm", "pres_raw", "wt_ground_m"),
                          c("Water above sensor (m)", "Water elevation (m)", "Depth to water below reference (m)", "Water pressure (kPa)",
                            "Barometric pressure (kPa)", "Water minus baro (kPa)", "Temperature (\u00b0C)", "Specific conductance (\u00b5S/cm)",
                            "Conductivity (\u00b5S/cm)", "Raw pressure (file units)", "Water table above the ground (m)"))
