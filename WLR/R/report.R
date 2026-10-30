# R/report.R  --  The run report and one report per site, in HTML, PDF or both (rendered by the shared core, Ovid/report.R).
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

wlr_manifest <- function(project_dir) { m <- read_out(project_dir, "run_manifest.csv", colClasses = "character")
  if (nrow(m)) setNames(m$value, m$key) else character() }

# What the last run did: files, loggers, deployments, barometric seams, tape checks, every removal by rule, and problems.
wlr_run_report_doc <- function(project_dir) {
  rd <- function(f, ...) read_out(project_dir, f, ...)
  man <- wlr_manifest(project_dir); s <- rd("summary.csv"); files <- rd("files.csv"); lg <- rd("loggers.csv", colClasses = list(character = "serial"))
  chk <- rd("manual_check.csv"); idx <- rd("audit_index.csv"); probs <- rd("problems.csv"); seams <- rd("baro_seams.csv"); rec <- rd("reconciliation.csv")
  g <- function(k) if (k %in% names(man)) man[[k]] else NA
  removed <- if (nrow(idx)) sum(idx[level == "cell", n], na.rm = TRUE) else 0
  tape <- if (nrow(chk)) chk[!is.na(difference_m), .(checks = .N, median_diff_m = round(median(difference_m), 3), largest_diff_m = round(difference_m[which.max(abs(difference_m))], 3)), by = site][order(-abs(largest_diff_m))] else data.table()
  rpt_doc("Water level and temperature: run report",
    paste0("Run ", g("run_id"), "  |  ", APP_NAME, " ", g("pipeline_version"), " (core ", g("core_version"), ")  |  ", g("started"), sprintf("  |  times are UTC%+d", BASE_OFFSET)), list(
    rpt_cards(c(`files found` = as.numeric(g("files")), `files used` = as.numeric(g("files_used")), loggers = as.numeric(g("loggers")),
                deployments = as.numeric(g("deployments")), sites = uniqueN(s$site), `values removed` = removed, problems = as.numeric(g("problems")))),
    rpt_text(if (nrow(rec) && all(rec$ok %in% c(TRUE, "TRUE"))) "Every record read is accounted for: written, or removed with a reason in the audit trail."
             else if (nrow(rec)) paste(sum(!rec$ok %in% c(TRUE, "TRUE")), "logger(s) do not reconcile; see the Audit trail page.")),
    rpt_heading("Deployments"),
    rpt_table(if (nrow(s)) s[order(site, year), .(site, year, serial, start, end, records, baro = primary, depth_median_m = depth_median, T_ST, reference, offset_m, tape_diff_median_m = tape_diff_median)] else NULL,
              "One row per logger and season. depth_median_m: median water above the sensor. offset_m: tip-to-sensor offset applied. tape_diff_median_m: logger minus tape, in metres."),
    rpt_heading("Tape checks"),
    rpt_table(tape, "Manual (tape) water levels against the logger at the same time, by site; the largest difference is listed first."),
    rpt_heading("Barometric records"),
    rpt_table(seams, "Where one barologger hands over to the next within a station, with the pressure difference across the seam."),
    applied_report(project_dir),
    rpt_heading("What was removed or flagged"),
    rpt_table(if (nrow(idx)) idx[, .(values = sum(n, na.rm = TRUE), entries = sum(entries, na.rm = TRUE), loggers = uniqueN(logger_id)), by = .(stage, rule, level)][order(-values)] else NULL,
              "Every rule that removed or flagged something. Each value is listed in output/audit/detail/."),
    rpt_heading("Files"),
    rpt_table(if (nrow(files)) files[, .(files = .N, used = sum(use %in% c(TRUE, "TRUE"))), by = .(kind, format, status)][order(kind, -files)] else NULL),
    rpt_heading("Loggers"),
    rpt_table(lg),
    rpt_heading("Problems"),
    rpt_table(probs, "Things that did not stop the run but need a look.")))
}


# ---- Site report pieces ---------------------------------------------------------------------------------------------------------------
# Tape checks are compared with two bands: DRIFT_TOL_M, the threshold the drift correction uses, and WORKING_TOL_M, an unofficial working
# tolerance (3 cm) that M chose; it is a guide for reading the chart, not a rule the pipeline enforces.
WORKING_TOL_M <- 0.03
wlr_day <- function(x) ifelse(is.na(x), NA_character_, substr(as.character(x), 1, 10))

# The site, its loggers and the datum behind its water levels.
wlr_site_info <- function(st, s, project_dir) {
  lg <- read_out(project_dir, "loggers.csv", colClasses = list(character = "serial")); if (nrow(lg)) lg <- lg[serial %in% unique(as.character(s$serial))]
  g <- col_or_null                                                           # core util.R
  mod <- if (nrow(lg)) paste0(lg$serial, if ("model" %in% names(lg)) paste0(" (", lg$model, ")") else "") else unique(as.character(s$serial))
  kv_table(list(Site = st, Loggers = mod, `Recording interval` = g(lg, "interval"), Seasons = paste(range(s$year), collapse = " to "), `Deployments` = nrow(s),
              `Reference point used` = g(s, "reference"), `T_ST: reference point to the logger tip, or to the sensor (m)` = if ("T_ST" %in% names(s)) paste(range(s$T_ST, na.rm = TRUE), collapse = " to "),
              `T_ST taken from` = g(s, "T_ST_source"), `Reference elevation (m)` = if ("z_ref" %in% names(s) && any(!is.na(s$z_ref))) paste(round(range(s$z_ref, na.rm = TRUE), 3), collapse = " to "),
              `Hang length measured to` = g(s, "measured_to"),
              `Tip-to-sensor offset applied (m)` = if ("offset_m" %in% names(s) && any(!is.na(s$offset_m))) paste(unique(range(s$offset_m, na.rm = TRUE)), collapse = " to "),
              `Offset taken from` = g(s, "offset_source")))
}
# One row per season: when the logger was in, how many readings, which barologger compensated it and where it was filled from elsewhere.
wlr_deployments <- function(s) {
  x <- copy(s); x[, `:=`(deployed = wlr_day(start), removed = wlr_day(end))]
  if ("baro_primary_share" %in% names(x)) x[, `% primary baro` := round(100 * baro_primary_share)]
  if ("depth_median" %in% names(x)) setnames(x, "depth_median", "median_depth_m"); if ("tape_diff_median" %in% names(x)) setnames(x, "tape_diff_median", "tape_diff_m"); if ("baro_filled" %in% names(x)) setnames(x, "baro_filled", "filled")
  keep <- c("year", "serial", "deployed", "removed", "records", "primary", "secondary", "% primary baro", "filled", "median_depth_m", "tape_diff_m")
  x[order(year), intersect(keep, names(x)), with = FALSE]
}
# Barologger seams that touch the barologgers this site used.
wlr_baro_seams <- function(s, project_dir) {
  sm <- read_out(project_dir, "baro_seams.csv", colClasses = list(character = c("from_serial", "to_serial"))); if (!nrow(sm)) return(NULL)
  used <- unique(c(as.character(s$primary), as.character(s$secondary))); used <- used[!is.na(used) & nzchar(used)]
  sm <- sm[station %in% used | from_serial %in% used | to_serial %in% used]; if (nrow(sm)) sm else NULL
}
# Tape check difference at each visit (logger minus tape), with the drift threshold and the working tolerance drawn as bands.
wlr_tape_plot <- function(chk, pal, drift_tol = 0.01) {
  x <- chk[!is.na(difference_m) & !is.na(visit_time)]; if (!nrow(x)) return(NULL); lv <- rpt_levels(pal)
  x[, status := ifelse(abs(difference_m) <= drift_tol, "within the drift threshold", ifelse(abs(difference_m) <= WORKING_TOL_M, "within the working tolerance", "beyond both"))]
  x[, status := factor(status, levels = c("within the drift threshold", "within the working tolerance", "beyond both"))]
  cols <- c("within the drift threshold" = unname(lv["ok"]), "within the working tolerance" = unname(lv["warn"]), "beyond both" = unname(lv["error"]))
  p <- ggplot2::ggplot(x, ggplot2::aes(visit_time, difference_m * 100, colour = status)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -WORKING_TOL_M * 100, ymax = WORKING_TOL_M * 100, fill = lv["warn"], alpha = 0.10) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -drift_tol * 100, ymax = drift_tol * 100, fill = lv["ok"], alpha = 0.18) +
    ggplot2::geom_hline(yintercept = 0, colour = pal$fg4, linewidth = 0.3) + ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_colour_manual(values = cols, drop = FALSE, name = NULL) +
    ggplot2::labs(title = NULL, subtitle = sprintf("Logger minus tape (cm). Green band: drift threshold (%s cm). Yellow band: working tolerance (%s cm), an unofficial guide.", drift_tol * 100, WORKING_TOL_M * 100), x = NULL, y = "cm") +
    theme_ovid(pal, base_size = 9, legend = "bottom")
  rpt_plot(p, 11, 2.8, inline = TRUE)
}
# Where each measurement has data, season after season: valid, flagged (kept) or in the air; dashed lines mark tape-check visits.
wlr_availability <- function(daily_src, fl, air, chk, pal) {
  if (!length(daily_src)) return(NULL)
  x <- rbindlist(daily_src, fill = TRUE); if (!nrow(x)) return(NULL)
  grp <- function(p) ifelse(p %in% c("depth_m", "elev_m"), "Water level", ifelse(p == "temp_c", "Temperature", "Conductivity"))
  lab <- c(depth_m = "water above sensor (m)", elev_m = "water elevation (m)", temp_c = "temperature (C)", spc_uscm = "conductivity (uS/cm)")
  x[, `:=`(row = ifelse(parameter %in% names(lab), lab[as.character(parameter)], as.character(parameter)), group = grp(as.character(parameter)))]
  if (!is.null(fl) && nrow(fl)) { f <- fl[, .(parameter = as.character(parameter), a = as.Date(as_ts(start)), b = as.Date(as_ts(end)))]
    x[, status := "Valid"]; for (i in seq_len(nrow(f))) x[parameter == f$parameter[i] & time >= f$a[i] & time <= f$b[i], status := "Flagged (kept in the data)"] } else x[, status := "Valid"]
  if (!is.null(air) && nrow(air) && all(c("from", "to") %in% names(air))) { a <- data.table(time = as.Date(as_ts(air$from)), b = as.Date(as_ts(air$to))); a <- a[!is.na(time) & !is.na(b)]
    if (nrow(a)) { ad <- a[, .(time = seq(time, b, by = "day")), by = seq_len(nrow(a))]; ad <- ad[, .(time)]; ar <- rbindlist(lapply(unique(x$row[x$group == "Water level"]), function(r) data.table(row = r, group = "Water level", time = ad$time, status = "In the air (trimmed)", parameter = "depth_m"))); x <- rbind(x, ar, fill = TRUE) } }
  lv <- rpt_levels(pal); marks <- if (!is.null(chk) && nrow(chk) && "visit_time" %in% names(chk)) data.table(time = chk$visit_time[!is.na(chk$visit_time)]) else NULL
  rpt_availability_long(x[, .(row, group, time, status)], pal, c(Valid = unname(lv["ok"]), `Flagged (kept in the data)` = unname(lv["warn"]), `In the air (trimmed)` = unname(lv["error"])),
                        title = "Data availability by measurement", marks = marks, mark_label = "tape-check visits")
}

# One site across every season: the deployments, water level and elevation by season, tape checks and removals.
# The parts of a site report that can be left out (Export page).
WLR_REPORT_SECTIONS <- c("Site, loggers and datum" = "info", "Deployments and barometric compensation" = "deployments", "Data availability" = "availability",
                         "Tape checks" = "tape", "All seasons at a glance" = "overview", "Statistics" = "stats", "Water years" = "water_years",
                         "Manual measurements" = "manual", "Quality control" = "qc", "By season" = "seasons")
# Axes shared by the site reports written together (core report_options.R), from each site's daily means (summaries/water_year.csv).
# Water elevation keeps each site's own level and shares only the span, as sites stand at different heights.
wlr_report_limits <- function(project_dir, sites, mode = "same") {
  if (identical(mode, "free") || !length(sites)) return(NULL)
  # safe: no water-year table yet means no water-year page
  wy <- tryCatch(fread(file = out_dir(project_dir, "summaries", "water_year.csv")), error = function(e) data.table()); if (!nrow(wy)) return(NULL)
  wy <- wy[site %in% sites]; if (!nrow(wy)) return(NULL); x <- wy[, .(unit = rep(site, 2), key = rep(parameter, 2), value = c(min, max))]
  rbind(axis_limits(x[key != "elev_m"], mode), axis_limits(x[key == "elev_m"], "span"))
}
wlr_site_report_doc <- function(site, project_dir, opts = NULL, lims = NULL) {
  ro <- report_opts(opts); want <- function(id) report_wants(ro, id)
  s <- read_out(project_dir, "summary.csv"); st <- site; s <- s[s$site == st][order(start)]
  if (!nrow(s)) return(NULL)
  chk <- read_out(project_dir, "manual_check.csv"); if (nrow(chk)) { chk <- chk[chk$site == st]; chk <- fix_ts_cols(chk) }
  idx <- read_out(project_dir, "audit_index.csv", colClasses = list(character = "logger_id")); if (nrow(idx)) idx <- idx[idx$site == st & level == "cell"]
  fl <- read_out(project_dir, "flags.csv", colClasses = list(character = "logger_id")); if (nrow(fl)) fl <- fl[fl$site == st]
  air <- read_out(project_dir, "air_runs.csv"); if (nrow(air) && "site" %in% names(air)) air <- air[air$site == st] else air <- data.table()
  ms <- read_out(project_dir, "measurements.csv"); if (nrow(ms)) ms <- ms[ms$site == st]
  # safe: no water-year table yet means no water-year rows
  wy <- tryCatch(fread(file = out_dir(project_dir, "summaries", "water_year.csv")), error = function(e) data.table()); if (nrow(wy)) wy <- wy[wy$site == st]
  plots <- list(); daily <- list(); allp <- list()
  for (i in seq_len(nrow(s))) { d <- load_deployment(s$deployment[i], project_dir); if (is.null(d) || !nrow(d)) next
    pp <- intersect(c("depth_m", "elev_m", "temp_c", "spc_uscm"), names(d)); pp <- pp[vapply(pp, function(p) any(!is.na(d[[p]])), logical(1))]
    if (!length(pp)) next
    allp[[length(allp) + 1L]] <- d[, c("TIMESTAMP", pp), with = FALSE]
    daily[[length(daily) + 1L]] <- try_or(melt(d[, c(list(day = as.Date(TIMESTAMP)), .SD), .SDcols = pp][, lapply(.SD, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)), by = day],
                                                  id.vars = "day", variable.name = "parameter", value.name = "value")[, `:=`(parameter = as.character(parameter), time = day)][!is.na(value)], where = paste("report", st, "daily values"))
    rm <- removed_values(s$serial[i], project_dir); tp <- if (nrow(chk)) chk[chk$deployment == s$deployment[i]] else NULL
    p <- try_or(plot_logger(d, pp, title = paste0(st, "  -  season ", s$year[i]), subtitle = paste("serial", s$serial[i]), removed = rm,
                              visits = if (!is.null(tp)) tp$visit_time else NULL, tape = tp, meas = if (nrow(ms)) fix_ts_cols(copy(ms)) else NULL), where = paste("report", st, "plot"))
    plots[[length(plots) + 1L]] <- rpt_plot(p, 11, 1.6 + 2.2 * length(pp)) }
  stt <- NULL; ov <- NULL
  if (length(daily)) { l <- rbindlist(daily); l[, group := ifelse(parameter %in% c("depth_m", "elev_m"), "Water level", ifelse(parameter == "temp_c", "Temperature", "Conductivity"))]
    l <- l[order(group, parameter, time)]
    ov <- try_or(rpt_overview(l, NORD, paste0("Daily means, all seasons  -  ", st), ylim = if (!is.null(lims)) lims[unit == st, .(parameter = key, lo, hi)]), where = paste("report", st, "overview")) }
  if (length(allp)) { al <- rbindlist(allp, fill = TRUE); pp <- intersect(c("depth_m", "elev_m", "temp_c", "spc_uscm"), names(al)); grp <- setNames(ifelse(pp %in% c("depth_m", "elev_m"), "Water level", ifelse(pp == "temp_c", "Temperature", "Conductivity")), pp)
    stt <- try_or(rpt_stats(al, pp, grp, caption = "From every reading of every season (not daily means). depth_m: water above the sensor (m); elev_m: water elevation (m); temp_c: degrees C; spc_uscm: specific conductance."), where = paste("report", st, "statistics")) }
  info <- try_or(wlr_site_info(st, s, project_dir), where = paste("report", st, "site information"))
  seams <- try_or(wlr_baro_seams(s, project_dir), where = paste("report", st, "barometric seams"))
  av <- try_or(wlr_availability(daily, fl, air, chk, NORD), where = paste("report", st, "availability"))
  tp <- if (nrow(chk)) try_or(wlr_tape_plot(chk, NORD), where = paste("report", st, "tape plot"))
  rem_t <- if (nrow(idx)) idx[, .(values = sum(n, na.rm = TRUE)), by = rule][order(-values)] else NULL
  flg_t <- if (nrow(fl)) fl[, .(values = sum(as.numeric(records), na.rm = TRUE)), by = check][order(-values)] else NULL
  tapes <- if (nrow(chk)) chk[!is.na(difference_m)] else data.table()
  cards <- c(seasons = nrow(s), records = sum(s$records), `median water level (m)` = if ("depth_median" %in% names(s) && any(!is.na(s$depth_median))) round(stats::median(s$depth_median, na.rm = TRUE), 2),
             `tape checks` = nrow(tapes), `median tape diff (cm)` = if (nrow(tapes)) round(100 * stats::median(tapes$difference_m), 1),
             `values removed` = if (nrow(idx)) sum(idx$n, na.rm = TRUE) else 0)
  fr <- format(as.Date(min(s$start))); to <- format(as.Date(max(s$end)))
  rpt_doc(report_title(ro, paste0(st, ": water level and temperature"), name = st, from = fr, to = to, app = "WLR"),
    report_subtitle(ro, paste0("Seasons ", paste(range(s$year), collapse = " to "), "  |  loggers ", paste(unique(s$serial), collapse = ", "), sprintf("  |  times are UTC%+d", BASE_OFFSET)),
                    name = st, from = fr, to = to, app = "WLR"), c(list(
    rpt_cards(cards),
    if (want("info") && !is.null(info)) rpt_heading("Site, loggers and datum"),
    if (want("info") && !is.null(info)) rpt_table(info, "From the logger files and the site and datum set-up. Water elevation = survey of the reference point minus T_ST, plus the tip-to-sensor offset (only where T_ST was measured to the logger tip), plus water above the sensor.", col_width = 110L),
    if (want("deployments")) rpt_heading("Deployments and barometric compensation"),
    if (want("deployments")) rpt_table(wlr_deployments(s), "One row per season. % primary baro: share of readings compensated with the primary barologger; filled: readings filled from other stations. median_depth_m: median water above the sensor; tape_diff_m: median logger minus tape."),
    if (want("deployments") && !is.null(seams)) rpt_table(seams, "Where one barologger hands over to the next at the stations used, with the pressure difference across the seam."),
    if (want("availability") && !is.null(av)) rpt_heading("Data availability"),
    if (want("availability")) av,
    if (want("tape") && nrow(tapes)) rpt_heading("Tape checks"),
    if (want("tape")) tp,
    if (want("tape")) rpt_table(if (nrow(chk)) chk[, .(year, visit_time, reference, T_ST, offset_m, tape_to_water, tape_level_m = h_tape, logger_level_m = h_logger, difference_m)] else NULL,
              "Water above the sensor from the tape (T_ST minus the tip-to-sensor offset minus tape to water) and from the logger at the same time; difference = logger minus tape."),
    if (want("overview") && !is.null(ov)) rpt_heading("All seasons at a glance"),
    if (want("overview") && !is.null(ov)) rpt_text("Each measurement as daily means across every season (the pale band is the daily minimum to maximum); gaps are the time between seasons."),
    if (want("overview")) ov,
    if (want("stats") && !is.null(stt)) rpt_heading("Statistics by measurement"),
    if (want("stats")) stt,
    if (want("water_years")) rpt_heading("Water years"),
    if (want("water_years")) rpt_table(if (nrow(wy)) wy[, .(water_year, parameter, mean, min, date_min, max, date_max, days, complete)] else NULL, "From daily means; water years run from 1 October unless the project sets another start."),
    if (want("manual") && nrow(ms)) rpt_heading("Manual measurements"),
    if (want("manual") && nrow(ms)) rpt_table(ms[, .(time, parameter, manual, logger, difference, method)], "Readings taken by hand against the logger at the same time; difference = logger minus manual. Reference only, never applied."),
    if (want("qc")) rpt_heading("Quality control"),
    if (want("qc")) rpt_quality_bars(rem_t, flg_t, NORD),
    if (want("qc")) rpt_table(rem_t, "Removed values by rule (blank in the data; listed in output/audit/detail/)."),
    if (want("qc")) rpt_table(flg_t, "Flagged (suspect) periods by check; the values are kept."),
    if (want("qc") && nrow(air)) rpt_table(air, "Periods when the logger was probably in the air at the start or end of a deployment."),
    if (want("seasons")) rpt_heading("By season"),
    if (want("seasons")) rpt_text("Water above the sensor, water elevation and temperature for each season. Red ticks: removed values; yellow circles: flagged; green crosses: tape checks; white diamonds: manual measurements (labelled logger minus manual); dashed lines: field visits.")),
    if (want("seasons")) plots))
}

# Writes the run report and the chosen site reports into dir, in the chosen formats. Returns the files written.
write_wlr_reports <- function(project_dir, dir, sites = NULL, formats = c("html", "pdf"), run = TRUE, progress = function(i, n, what) NULL, opts = NULL) {
  o <- report_opts(opts); lims <- try_or(wlr_report_limits(project_dir, sites, o$axis), where = "report axis limits")
  if (!length(formats)) return(character())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE); out <- character()
  if (run) out <- c(out, report_write(wlr_run_report_doc(project_dir), file.path(dir, "run_report"), formats, NORD))
  for (i in seq_along(sites)) { progress(i, length(sites), sites[i])
    doc <- tryCatch(wlr_site_report_doc(sites[i], project_dir, o, lims), error = function(e) { problem("report", paste(sites[i], conditionMessage(e))); NULL })
    if (!is.null(doc)) out <- c(out, report_write(doc, file.path(dir, report_file(o, safe_name(sites[i]), name = sites[i], app = "WLR")), formats, NORD)) }
  out
}
