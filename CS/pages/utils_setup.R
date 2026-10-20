# CS: set-up table editors, background runs, settings, history and open sessions. Sourced by app.R.
# =============================================================================
#  Editable config table (one module, used for every config file)
# =============================================================================
# Every saved change to a set-up table is logged, and the version before it is kept, in config/history/.
log_change <- function(path, new, who = "", what = "") backup_cfg(path, new, who, what)   # Ovid/config.R


# Every set-up table the interfaces edit: module id, file, columns that cannot be edited, and whether a saved change
# needs a run to apply (the run state then says so). Both interfaces build their editors from this list.
SETUP_TABLES <- data.table(
  id     = c("stations", "aliases", "qc", "cr10x", "groups", "timecorr", "pinfo", "gauges", "sites", "fieldlog", "filerules", "depsheet", "calib", "period", "timezones"),
  file   = c("stations.csv", "aliases.csv", "qc_rules.csv", "cr10x.csv", "groups.csv", "time_corrections.csv",
             "parameter_info.csv", "precip_gauges.csv", "station_info.csv", "field_log.csv", "file_rules.csv", "deployment_sheet.csv", "calibration_options.csv", PERIOD_FILE, "timezones.csv"),
  label  = c("Stations", "Column aliases", "Quality rules", "CR10X files", "Parameter groups", "Clock corrections",
             "Parameter guide", "Precipitation gauges", "Station details", "Field log", "File rules", "Deployment sheet", "Calibration options", "Corrections over a period", "Time zones"),
  locked = c("logger_id|seen_as", "", "", "file", "", "", "", "", "station", "", "", "", "", "", ""),
  run    = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))
setup_editors <- function(cfg_path, tick, mark) setNames(lapply(seq_len(nrow(SETUP_TABLES)), function(i) { s <- SETUP_TABLES[i]
  editServer(s$id, cfg_path(s$file), tick, locked = if (nzchar(s$locked)) strsplit(s$locked, "|", fixed = TRUE)[[1]] else character(),
             on_change = if (s$run) mark else function() NULL) }), SETUP_TABLES$id)

# One pipeline run, from either interface, in the background (core runner.R): the app stays usable, the corner box shows
# its progress and Cancel. When it ends its log goes to LOG$lines and done(res, r) is called: res is what run_pipeline()
# returned, NULL when the run stopped or was cancelled; r says which (r$status).
app_run <- function(cfg, done) {
  CFG <<- cfg; LOG$lines <- character()
  run_start(getDefaultReactiveDomain(), cfg, function(r) {
    LOG$lines <- c(r$lines, if (!identical(r$status, "finished")) paste("STOPPED:", r$message))
    done(if (identical(r$status, "finished")) r$value, r) })
}
# What the last run redid, from its manifest: logger and station tables whose inputs had not changed are reused.
run_redone <- function(pd) {
  f <- file.path(path.expand(pd), "output", "audit", "run_manifest.csv"); if (!file.exists(f)) return("")
  m <- tryCatch(fread(f, colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
  v <- function(k) suppressWarnings(as.integer(m[key == k, value][1]))
  lt <- v("logger_tables"); st <- v("station_tables"); lr <- v("tables_reused"); sr <- v("stations_reused")
  if (anyNA(c(lt, st, lr, sr))) return("")
  if (lr == lt && sr == st) return(" Nothing had changed: every table was reused.")
  paste0(" Redone: ", st - sr, " of ", st, " station tables", if (lt > lr) paste0(" and ", lt - lr, " of ", lt, " logger tables") else "",
         if (sr + lr > 0) "; the rest was unchanged and reused." else ".")
}
# Saves some settings into app_settings.json and keeps the rest (each interface has its own subset of the inputs).
update_settings <- function(x) {
  # safe: the settings apply in this session; only remembering them failed
  s <- tryCatch(settings_save(SETTINGS_FILE, x), error = function(e) NULL); SETTINGS[names(x)] <<- x; invisible(s)
}
# History of the set-up tables (config/history/change_log.csv), newest first, and going back to one saved version.
hist_log <- function(pd) { f <- file.path(pd, "config", "history", "change_log.csv"); if (!file.exists(f)) return(data.table())
  # safe: a history that cannot be read lists nothing
  d <- tryCatch(fread(f, colClasses = "character", na.strings = NULL), error = function(e) data.table()); d[order(-seq_len(.N))] }
hist_restore <- function(pd, r, who = "") {
  tb <- sub(" \\(.*$", "", r$table); cur <- file.path(pd, "config", tb); bk <- file.path(pd, "config", "history", r$backup)
  if (!file.exists(bk)) return(FALSE)
  old <- as.data.frame(read_cfg(bk)); log_change(cur, old, who, "restored"); file.copy(bk, cur, overwrite = TRUE)
}
