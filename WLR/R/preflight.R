# R/preflight.R  --  Set-up check: what in the set-up would be ignored, misapplied or is missing, before a run.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
# Reads the set-up tables, the metadata workbook and (when there is one) the last run's deployments. Changes nothing.

hang_lengths <- function(md, deps = NULL) {
  # one row per deployment with data (or per workbook deployment when there is no run yet), with the T_ST history of that site
  d <- if (!is.null(deps) && nrow(deps)) copy(deps)[has_data == TRUE] else build_deployments(md)
  if (!nrow(d)) return(data.table())
  d <- d[, .(deployment, site, serial, year, T_ST, T_ST_source, tst_from_toc, tst_problem = tst_problem %||% "")]
  hist <- if (!is.null(md) && nrow(md)) md[!is.na(T_ST)][order(-year)][, .(T_ST_years = paste0(head(paste0(year, ": ", sprintf("%.0f", T_ST * 100)), 6), collapse = ", ")), by = site] else data.table(site = character(), T_ST_years = character())
  d <- merge(d, hist, by = "site", all.x = TRUE)
  d[, previous_T_ST := vapply(seq_len(.N), function(i) { if (is.null(md) || !nrow(md)) return(NA_real_)
    p <- md[site == d$site[i] & year < d$year[i] & !is.na(T_ST)]; if (nrow(p)) p$T_ST[which.max(p$year)] else NA_real_ }, 0)]
  d[, status := fifelse(is.na(T_ST) & tst_problem == "not a number", "not a number on the sheet",
                fifelse(is.na(T_ST), "missing",
                fifelse(!is.na(previous_T_ST) & abs(T_ST - previous_T_ST) > 0.05, sprintf("changed %+.0f cm from the year before", 100 * (T_ST - previous_T_ST)),
                fifelse(tst_from_toc, "measured from the casing (TOC)", "ok"))))]
  d[order(status == "ok", site, year)]
}

setup_check <- function(cfg) {
  chk <- check_collector(c("level", "area", "item", "problem", "fix")); add <- chk$add
  pd <- path.expand(cfg$project_dir)
  check_folders(add, cfg$raw_dirs, cfg$project_dir)
  mf <- find_metadata_file(cfg$metadata_dir)
  md <- if (is.na(mf)) NULL else tryCatch(read_metadata(mf), error = function(e) { add("error", "Metadata", basename(mf), conditionMessage(e)); NULL })
  config_migrate(pd, "preflight")                                                          # older tables moved in once (core config.R)
  ms_bad <- FALSE
  ms <- tryCatch(meta_sheet(cfg$project_dir, META_FIELDS, report = FALSE, deploy = wlr_deploy_rows),
                 error = function(e) { ms_bad <<- TRUE; add("error", "Deployment sheet", "reading", conditionMessage(e), "Fix the table named in the message"); NULL })
  cf0 <- attr(ms, "conflicts"); for (i in seq_len(NROW(cf0))) { z <- cf0[i]                  # core metasheet.R: the typed column wins, and says so
    add("warning", "Deployment sheet", z$item, paste0("From and To give ", z$field, " '", z$sheet, "', the ", z$field, " column '", z$list, "', which is used"),
        "Change the one that is wrong on the Deployment sheet.") }
  if (is.na(mf) && is.null(ms) && !ms_bad) add("info", "Metadata", "optional", "no metadata workbook and no deployment sheet: each logger is processed under its serial",
                                    "To name loggers or give sites, years and hang lengths, fill in the Deployment sheet (Loggers and metadata), or put a WL_Metadata workbook in the metadata folder.")
  if (!is.null(ms)) { p <- meta_sheet_problems(cfg$project_dir, META_FIELDS, wlr_deploy_rows)
    for (i in seq_len(nrow(p))) add("warning", DEPLOY_SHEET, paste("row", p$row[i] + 1L), paste0(p$field[i], " '", p$value[i], "': ", p$problem[i]), "Fix it on the Deployment sheet; the value is left blank meanwhile.") }
  sm <- tryCatch(sheet_metadata(ms), error = function(e) { add("error", "Deployment sheet", "reading", conditionMessage(e), "Fix the row named in the message"); NULL })
  md <- if (is.null(md) && is.null(sm)) NULL else merge_metadata(md, sm)
  if (!dir.exists(path.expand(cfg$dgps_dir %||% ""))) add("info", "dGPS", "optional", "no dGPS folder: water levels are given below the T-coupler, with no elevations", "Set the folder on the Home page to add elevations.")
  cf <- tryCatch(load_configs(cfg), error = function(e) { add("error", "Set-up tables", "reading", conditionMessage(e), "Fix the table named in the message"); NULL })
  if (is.null(cf)) return(chk$result())
  bad_dt <- function(x) bad_date(x)                     # cell by cell, as the pipeline reads them (parse_date_cell)
  for (w in c("oow", "time")) { t <- cf[[w]]; if (!nrow(t)) next
    k <- which(bad_dt(t$start) | bad_dt(t$end)); for (i in k) add("error", CONFIG_FILES[[w]]$file, paste("row", i), "a date could not be read (use YYYY-MM-DD HH:MM)", "Fix it on its page.") }
  pf <- cfg_path(pd, "period"); pr <- period_read(pf)                                     # corrections over a period, removals and drift rows among them
  for (i in seq_len(nrow(pr$bad))) add("error", PERIOD_FILE, paste("row", pr$bad$row[i]), paste0(pr$bad$problem[i], "; the row is not applied"), "Fix it under Quality rules, Corrections over a period.")
  dr <- wlr_period_to_drift(period_own_rows(pf, "drift_manual"))
  for (i in which(is.na(suppressWarnings(as.numeric(dr$offset_m))))) add("error", PERIOD_FILE, paste("row", dr$row[i]), "a drift typed in needs a number in value", "Fix it under Quality rules, Corrections over a period.")
  dp <- cf$deployments; for (i in seq_len(nrow(dp))) {
    if (bad_dt(dp$deployed[i]) || bad_dt(dp$removed[i])) add("error", "deployments.csv", paste("row", i), "a deployed or removed date could not be read")
    if (nzchar(dp$T_ST_m[i]) && is.na(suppressWarnings(as.numeric(dp$T_ST_m[i])))) add("error", "deployments.csv", paste("row", i), "T_ST_m is not a number")
    if (nzchar(dp$T_ST_m[i]) && isTRUE(as.numeric(dp$T_ST_m[i]) > 10)) add("warning", "deployments.csv", paste("row", i), "T_ST_m is over 10 m: was it typed in cm? This table takes metres") }
  q <- cf$qc; for (i in seq_len(nrow(q))) if (!tolower(q$action[i]) %in% c("remove", "flag")) add("error", "qc_rules.csv", paste("row", i), "action must be remove or flag")
  tzc <- tz_check(cf$timezones); for (i in seq_len(nrow(tzc))) add("error", "timezones.csv", paste("row", tzc$row[i]), tzc$problem[i], "Fix it on the Clock checks page.")
  ms <- cf$measurements; for (i in seq_len(nrow(ms))) {
    if (bad_dt(ms$time[i]) || !nzchar(ms$time[i])) add("error", "manual_measurements.csv", paste("row", i), "the time could not be read (use YYYY-MM-DD HH:MM)", "Fix it on the Field visits page.")
    if (!ms$parameter[i] %in% MEAS_PARAMS) add("error", "manual_measurements.csv", paste("row", i), paste("parameter must be one of", paste(MEAS_PARAMS, collapse = ", ")), "Fix it on the Field visits page.")
    if (is.na(suppressWarnings(as.numeric(ms$value[i])))) add("error", "manual_measurements.csv", paste("row", i), "value is not a number", "Fix it on the Field visits page.") }
  fr <- cf$file_rules; for (i in seq_len(nrow(fr))) if (!tolower(fr$action[i]) %in% c("ignore", "serial", "use", "tz", "baro", "water")) add("error", "file_rules.csv", paste("row", i), "action must be ignore, serial, use, tz, baro or water")
  last <- file.path(pd, "output", "audit", "deployments.csv")
  # safe: the last run's deployments, for hang lengths only
  deps <- if (file.exists(last)) tryCatch(fix_ts_cols(fread(last, colClasses = list(character = "serial"))), error = function(e) NULL) else NULL
  if (!is.null(md)) {
    h <- hang_lengths(md, deps)
    for (i in which(h$status %in% c("missing", "not a number on the sheet")))
      add("warning", "Hang length (T_ST)", paste(h$site[i], h$year[i]), paste("T_ST", h$status[i], "- depth to water and elevation will be blank"), "Type a length or pick another year below.")
    for (i in which(grepl("^changed", h$status))) add("info", "Hang length (T_ST)", paste(h$site[i], h$year[i]), h$status[i], "Check the sheet; if it is a typo, enter the right value below.")
  }
  g <- cf$baro_groups
  for (i in seq_len(nrow(g))) if (!nzchar(g$primary[i])) add("error", "baro_groups.csv", paste(g$year[i], g$group[i]), "no primary barologger", "Pick one on the Barometric page.")
  pr <- file.path(pd, "output", "audit", "problems.csv")
  if (file.exists(pr)) { p <- fread(pr, colClasses = "character"); for (i in seq_len(nrow(p))) add("warning", "Last run", p$where[i], p$problem[i]) }
  if (!is.null(deps) && nrow(deps)) {
    nz <- deps[has_data == TRUE & is.na(z_ref)]
    if (nrow(nz)) add("info", "dGPS", paste(nrow(nz), "deployments"), paste("no surveyed reference elevation:", paste(head(unique(nz$site), 8), collapse = ", "), if (uniqueN(nz$site) > 8) "..."), "Link survey points on the Datum and dGPS page.")
  }
  o <- cf$offsets; ov <- suppressWarnings(as.numeric(o$offset_m))
  if (nrow(o) && all(ov %in% 0)) add("info", "Logger offsets", "logger_offsets.csv", "every logger offset is 0: the sensor is taken to be at the tip of the logger", "Enter the distances on the Datum and dGPS page.")
  else if (nrow(o) && any(is.na(ov))) add("info", "Logger offsets", "logger_offsets.csv", sprintf("%d model row(s) have no tip-to-sensor distance yet (%s): those loggers get 0", sum(is.na(ov)), paste(o$model[is.na(ov)], collapse = "; ")),
                                          "Measure the tip to the sensor mark once and enter it on Datum and dGPS > Logger offsets, or delete rows for models you do not use.")
  r <- with_declared(chk$result(), cfg$project_dir); r[order(match(level, c("error", "warning", "info")))]   # with every table's declared rules (core table_rules.R)
}
