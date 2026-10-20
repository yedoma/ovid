# R/report.R  --  The run report and one report per site, in HTML, PDF or both (rendered by the shared core, Ovid/report.R).
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.

gt_read <- function(project_dir, dir, f) { p <- out_dir(project_dir, dir, f)
  if (file.exists(p) && file.info(p)$size > 0) try_or(fread(p, na.strings = c("", "NA"), colClasses = list(character = intersect(c("serial", "channel", "site", "logger_id"), names(fread(p, nrows = 0))))),
                                                     data.table(), paste0("output/", dir, "/", f, ": left out of the report")) else data.table() }

# What the last run did: downloads (including those that need an export), loggers, sites, clock events, sensor health, removals, problems.
gt_run_report_doc <- function(project_dir) {
  a <- function(f) gt_read(project_dir, "audit", f)
  mf <- a("run_manifest.csv"); man <- if (nrow(mf)) setNames(as.character(mf$value), mf$key) else character(); g <- function(k) if (k %in% names(man)) man[[k]] else NA
  bu <- a("bundles.csv"); lg <- a("loggers.csv"); ss <- a("sites_summary.csv"); ck <- a("clock.csv"); he <- a("sensor_health.csv"); bs <- a("between_sites.csv"); idx <- a("audit_index.csv"); probs <- a("problems.csv")
  rpt_doc("Ground temperature: run report",
    paste0("Run ", g("run_id"), "  |  ", APP_NAME, " ", g("pipeline_version"), " (core ", g("core_version"), ")  |  ", g("started"), sprintf("  |  times are UTC%+d", BASE_OFFSET)), list(
    rpt_cards(c(`files found` = as.numeric(g("files")), loggers = as.numeric(g("loggers")), sites = as.numeric(g("sites")), `values kept` = as.numeric(g("values_out")),
                `values removed` = as.numeric(g("values_removed")), `downloads needing an export` = if (nrow(bu)) sum(bu$status == "needs_export") else 0, problems = as.numeric(g("problems")))),
    rpt_heading("Downloads"),
    rpt_table(if (nrow(bu)) bu[order(status == "ok", type, serial), .(status, type, serial, folder, source_file, values_read, values_out, dropped, reconciles, advice)] else NULL,
              "One row per download: the logger's own file and its exports. A download whose only file cannot be read needs an export; the advice says how."),
    rpt_heading("Sites"),
    rpt_table(if (nrow(ss)) ss[order(site), .(site, instrument, loggers, depths, depth_list, assumed_depths, first, last, values, flagged, site_group)] else NULL,
              "assumed_depths: sensors whose depth was not given, so it came from a template or the default steps."),
    rpt_heading("Loggers"),
    rpt_table(lg),
    rpt_heading("Clock events"),
    rpt_table(ck, "Clock resets, jumps and drift corrections found while stitching each logger."),
    rpt_heading("Sensor health"),
    rpt_table(if (nrow(he)) he[order(health == "ok", site, depth_m), .(site, depth_m, depth_source, logger_id, channel, health, pct_valid, errors, removed_qc, gaps, longest_gap_h, first_valid, last_valid)] else NULL,
              "Sensors that are not ok are listed first."),
    rpt_heading("Between sites"),
    rpt_table(if (nrow(bs)) bs[verdict != "ok"] else NULL, "Site and depth pairs that depart from the other sites at the same depth at times, or do not move with them (out of step)."),
    applied_report(project_dir),
    rpt_heading("What was removed or flagged"),
    rpt_table(if (nrow(idx)) idx[, .(values = sum(n, na.rm = TRUE), loggers = uniqueN(logger_id)), by = .(stage, rule, level)][order(-values)] else NULL,
              "Every rule that removed or flagged something. Each value is listed in output/audit/detail/."),
    rpt_heading("Problems"),
    rpt_table(probs, "Things that did not stop the run but need a look.")))
}


# What the files say about one site's instrument and loggers, for the front of its report. Everything is optional: a part that has no data is left out.
gt_site_info <- function(st, d, project_dir) {
  sp <- file.path(path.expand(project_dir), "config", "sites.csv")
  cfg <- if (file.exists(sp)) try_or(fread(sp, colClasses = "character", na.strings = ""), data.table(), "config/sites.csv: site details left out of the report") else data.table(); if (nrow(cfg)) cfg <- cfg[site == st]
  lg <- gt_read(project_dir, "audit", "loggers.csv"); if (nrow(lg)) lg <- lg[logger_id %in% unique(d$logger_id)]
  ss <- gt_read(project_dir, "audit", "sites_summary.csv"); if (nrow(ss)) ss <- ss[site == st]
  g <- col_or_null                                                           # core util.R
  ll <- if (nrow(cfg) && has_value(g(cfg, "latitude")) && has_value(g(cfg, "longitude"))) paste0(g(cfg, "latitude")[1], ", ", g(cfg, "longitude")[1]) else NULL
  utm <- if (nrow(cfg) && has_value(g(cfg, "easting"))) paste0(g(cfg, "easting")[1], " E, ", g(cfg, "northing")[1], " N (UTM ", g(cfg, "utm_zone")[1], ")") else NULL
  kv_table(list(`Site group` = g(cfg, "site_group") %||% g(ss, "site_group"), `Land cover` = g(cfg, "landcover") %||% g(ss, "landcover"), Status = g(cfg, "status"), Location = ll, Position = utm,
             `Install date` = g(cfg, "install_date"), `Drilling method` = g(cfg, "drilling_method"), `Borehole depth (m)` = g(cfg, "borehole_depth_m"),
             Instrument = g(cfg, "instrument") %||% g(ss, "instrument"), `Logger model` = g(lg, "model"), Loggers = g(lg, "logger_id"), `Sensor type` = g(cfg, "sensor_type"), `Sensor accuracy` = g(cfg, "sensor_accuracy"),
             Calibration = g(cfg, "calibration"), `Recording interval (s)` = g(lg, "interval_s"), `Depths (m)` = g(ss, "depth_list"), `Description` = g(cfg, "description")))
}
# The loggers' own log: firmware and shell versions, recording periods, memory clears and resets, clock events and the battery.
gt_logger_log <- function(d, project_dir) {
  ids <- unique(d$logger_id); ev <- gt_read(project_dir, "audit", "events.csv"); ck <- gt_read(project_dir, "audit", "clock.csv"); bt <- gt_read(project_dir, "audit", "battery.csv")
  if (nrow(ev)) ev <- ev[logger_id %in% ids]; if (nrow(ck)) ck <- ck[logger_id %in% ids]; if (nrow(bt)) bt <- bt[logger_id %in% ids]
  ver <- if (nrow(ev)) unique(sub("^.*'([^']+)'.*$", "\\1", ev$event[grepl("SHELL", ev$event)])) else NULL
  per <- if (nrow(ev)) unique(sub("^PERIOD:\\s*", "", ev$event[grepl("^PERIOD:", ev$event)])) else NULL
  cnt <- function(pat) if (nrow(ev)) sum(grepl(pat, ev$event)) else 0L
  clk <- if (nrow(ck)) paste(names(table(ck$rule)), as.integer(table(ck$rule)), sep = ": ", collapse = ", ") else NULL
  bat <- if (nrow(bt) && has_value(bt$volts_min)) { b <- bt[which.min(volts_min)]; sprintf("lowest %s V on %s (%s)", format(b$volts_min), b$date, b$logger_id) } else NULL
  kv_table(list(`Shell or firmware` = ver, `Recording period` = if (length(per)) paste0(per, collapse = ", ") else NULL, `Memory clears` = if (cnt("MEMORY CLEAR")) cnt("MEMORY CLEAR"),
             `Firmware resets` = if (cnt("FIRMWARE RESET")) cnt("FIRMWARE RESET"), `Readouts` = if (cnt("READOUT")) cnt("READOUT"), `Clock events` = clk, Battery = bat))
}
# Availability of each depth by day: valid, flagged (kept in the data) or outside the deployment (before installation or after removal).
gt_availability <- function(d, pal) {
  if (!nrow(d)) return(NULL); fl <- if ("flag" %in% names(d)) d$flag else rep("", nrow(d)); fl[is.na(fl)] <- ""
  z <- data.table(depth_m = d$depth_m, time = d$TIMESTAMP, status = ifelse(!nzchar(fl), "Valid", ifelse(grepl("^(pre_|post_)", fl), "Outside the deployment", "Flagged (kept in the data)")))
  z[, `:=`(row = sprintf("%s m", format(depth_m)), group = gt_band(depth_m))]
  z <- z[order(depth_m)]
  rpt_availability_long(z, pal, c(Valid = unname(rpt_levels(pal)["ok"]), `Flagged (kept in the data)` = unname(rpt_levels(pal)["warn"]), `Outside the deployment` = pal$gray), title = "Data availability by depth")
}
# One row per depth: the usual statistics from every reading, then what a ground-temperature reader wants next to them.
gt_depth_stats <- function(d, an, ft) {
  x <- d[, .(group = "Ground temperature", parameter = sprintf("%s m", format(depth_m)), depth_m, time = as.Date(TIMESTAMP), value)]
  out <- rpt_stats_long(x[order(depth_m)], dates = FALSE, caption = "From every reading (not daily means), degrees C. Annual figures use complete calendar years only; days frozen, thawed and in the zero curtain are totals over the record.")
  if (is.null(out)) return(NULL); t <- out$data; t[, group := NULL]; t[, depth_m := as.numeric(sub(" m$", "", parameter))]
  if (nrow(an)) { a <- an[year_type == "calendar" & as.character(complete) %in% c("TRUE", "true", "T")][, .(`mean annual` = round(mean(mean, na.rm = TRUE), 2), `annual range` = round(mean(range, na.rm = TRUE), 2), years = .N), by = depth_m]; t <- merge(t, a, by = "depth_m", all.x = TRUE) }
  if (nrow(ft)) { f <- ft[, .(`days frozen` = sum(days_frozen, na.rm = TRUE), `days thawed` = sum(days_thawed, na.rm = TRUE), `zero curtain` = sum(days_zero_curtain, na.rm = TRUE)), by = depth_m]; t <- merge(t, f, by = "depth_m", all.x = TRUE) }
  t <- t[order(depth_m)][, depth_m := NULL]; setnames(t, c("parameter", "% of records"), c("depth", "% valid")); rpt_table(t, out$caption, 200)
}

# One site: depths and where they came from, heatmap, trumpet curves, annual statistics, active layer and freeze/thaw.
# The parts of a site report that can be left out (Export page).
GT_REPORT_SECTIONS <- c("Site and instrument" = "info", "Logger log" = "logger", "Data availability" = "availability", "Temperature at each depth" = "overview",
                        "Statistics by depth" = "stats", "Sensors and depths" = "sensors", "Quality control" = "qc", "Active layer" = "active_layer",
                        "Freeze and thaw" = "freeze_thaw", "Annual statistics" = "annual", "Figures" = "figures")
gt_band <- function(z) ifelse(z < 0, "Above ground", ifelse(z < 0.5, "Near surface (above 0.5 m)", ifelse(z < 2, "Shallow (0.5 to 2 m)", "Deep (2 m and below)")))

# The daily means of the last run; empty before the first run, and a problem when the file is there but cannot be read.
gt_daily <- function(project_dir) { p <- out_dir(project_dir, "summaries", "daily.rds")
  if (file.exists(p)) try_or(readRDS(p), data.table(), "output/summaries/daily.rds: daily means could not be read") else data.table() }

# Shallow or deep profile of each site: the profile column of sites.csv when it says, else by the deepest sensor (no deeper than
# cut: shallow). Returns site, deepest (m), profile, how ("set in Sites" or "from the deepest sensor").
PROFILE_CUT_M <- 3                     # m; a site whose deepest sensor is no deeper than this is a shallow profile unless Sites says
gt_profiles <- function(project_dir, sites = NULL, cut = PROFILE_CUT_M, dd = NULL) {
  if (is.null(dd)) dd <- gt_daily(project_dir)
  deep <- if (nrow(dd)) dd[, .(deepest = max(depth_m, na.rm = TRUE)), by = site] else data.table(site = character(), deepest = numeric())
  if (!is.null(sites)) deep <- merge(data.table(site = sites), deep, by = "site", all.x = TRUE)
  sc <- tryCatch(read_cfg(cfg_path(project_dir, "sites")), error = function(e) data.table(site = character()))
  set_to <- if ("profile" %in% names(sc)) tolower(trim(sc$profile[match(deep$site, sc$site)])) else rep("", nrow(deep)); set_to[is.na(set_to)] <- ""
  cut <- suppressWarnings(as.numeric(cut)); if (!length(cut) || is.na(cut)) cut <- PROFILE_CUT_M
  deep[, `:=`(profile = ifelse(set_to %in% c("shallow", "deep"), set_to, ifelse(!is.na(deepest) & deepest <= cut, "shallow", "deep")),
              how = ifelse(set_to %in% c("shallow", "deep"), "set in Sites", "from the deepest sensor"))][]
}

# Axes shared by the site reports written together (core report_options.R): temperature per depth band and the heatmap colours
# among sites of one kind of profile, the depth axis of the heatmap and the trumpet curves likewise, the trumpet's temperatures too.
gt_report_limits <- function(project_dir, sites, mode = "same") {
  if (identical(mode, "free") || !length(sites)) return(NULL)
  dd <- gt_daily(project_dir); if (!nrow(dd)) return(NULL)
  dd <- dd[site %in% sites & is.finite(temp)]; pr <- gt_profiles(project_dir, sites, dd = dd); dd[, prof := pr$profile[match(site, pr$site)]]
  an <- gt_read(project_dir, "summaries", "annual_stats.csv"); if (nrow(an)) { an <- an[site %in% sites & year_type == "calendar"]; an[, prof := pr$profile[match(site, pr$site)]] }
  dz <- dd[, .(lo = min(depth_m), hi = max(depth_m)), by = .(site, prof)]; dz[, `:=`(lo = min(lo), hi = max(hi)), by = prof]
  list(profiles = pr, band = axis_limits(dd[, .(unit = site, key = paste(prof, gt_band(depth_m)), value = temp)], mode),
       heat = axis_limits(dd[, .(value = stats::quantile(temp, c(0.01, 0.99), names = FALSE)), by = .(unit = site, key = prof)], mode),   # as the heatmap's own (1 to 99 %)
       depth = dz[, .(unit = site, key = prof, lo = pmin(0, lo), hi = hi * 1.02)],
       trumpet = if (nrow(an)) axis_limits(an[, .(unit = rep(site, 2), key = rep(prof, 2), value = c(min, max))], mode))
}

gt_site_report_doc <- function(site, project_dir, opts = NULL, lims = NULL) {
  ro <- report_opts(opts); want <- function(id) report_wants(ro, id)
  st <- site; d <- load_site(st, project_dir); if (is.null(d) || !nrow(d)) return(NULL)
  s <- function(f) { x <- gt_read(project_dir, "summaries", f); if (nrow(x) && "site" %in% names(x)) x[x$site == st] else data.table() }
  an <- s("annual_stats.csv"); alt <- s("active_layer.csv"); ft <- s("freeze_thaw.csv"); zaa <- s("zero_annual_amplitude.csv")
  he <- gt_read(project_dir, "audit", "sensor_health.csv"); if (nrow(he)) he <- he[he$site == st]
  # safe: no daily table yet means no freeze-thaw figure
  dd <- tryCatch(readRDS(out_dir(project_dir, "summaries", "daily.rds")), error = function(e) data.table()); if (nrow(dd)) dd <- dd[site == st]
  zs <- sort(unique(d$depth_m)); assumed <- if ("depth_source" %in% names(d)) sort(unique(d$depth_m[d$depth_source %in% c("default", "template")])) else numeric()
  yrs <- range(as.integer(format(d$TIMESTAMP, "%Y")))
  # the front of the report: the site, the instrument and what the loggers recorded about themselves
  info <- try_or(gt_site_info(st, d, project_dir), where = paste("report", st, "logger log")); lglog <- try_or(gt_logger_log(d, project_dir), where = paste("report", st, "site information"))
  av <- try_or(gt_availability(d, MI), where = paste("report", st, "availability"))
  ov <- if (nrow(dd)) try_or({ o <- dd[order(depth_m, date)]; band <- function(z) ifelse(z < 0, "Above ground", ifelse(z < 0.5, "Near surface (above 0.5 m)", ifelse(z < 2, "Shallow (0.5 to 2 m)", "Deep (2 m and below)")))
    pf <- if (!is.null(lims)) lims$profiles$profile[match(st, lims$profiles$site)] else NA_character_
    ylim <- if (!is.null(lims$band)) { u <- unique(o[, .(depth_m)]); b <- lims$band[unit == st]; u[, key := paste(pf, gt_band(depth_m))]
      u <- merge(u, b, by = "key"); if (nrow(u)) u[, .(parameter = sprintf("ground_temp  %s m", format(depth_m)), lo, hi)] } else NULL
    rpt_overview(o[, .(group = band(depth_m), parameter = sprintf("ground_temp  %s m", format(depth_m)), time = as.Date(date), value = temp)], MI, ylim = ylim, paste0("Daily mean ground temperature by depth  -  ", st)) }, where = paste("report", st, "overview"))
  stt <- try_or(gt_depth_stats(d, an, ft), where = paste("report", st, "statistics"))
  # quality control for this site's loggers
  idx <- gt_read(project_dir, "audit", "audit_index.csv"); if (nrow(idx)) idx <- idx[logger_id %in% unique(d$logger_id)]
  fl <- gt_read(project_dir, "audit", "flags.csv"); if (nrow(fl)) fl <- fl[site == st]
  rem_t <- if (nrow(idx)) idx[, .(values = sum(n, na.rm = TRUE)), by = rule][order(-values)] else NULL
  flg_t <- if (nrow(fl)) fl[, .(values = sum(as.numeric(records), na.rm = TRUE)), by = check][order(-values)] else NULL
  # permafrost headline figures
  last_alt <- if (nrow(alt) && "alt_m" %in% names(alt)) alt[!is.na(alt_m)][which.max(year)] else data.table()
  last_zaa <- if (nrow(zaa) && "zaa_m" %in% names(zaa)) zaa[!is.na(zaa_m)][which.max(year)] else data.table()
  deepest_frozen <- if (nrow(alt) && "deepest_frozen_m" %in% names(alt) && any(!is.na(alt$deepest_frozen_m))) max(alt$deepest_frozen_m, na.rm = TRUE) else NA_real_
  cards <- c(depths = length(zs), `assumed depths` = length(assumed), years = diff(yrs) + 1, values = nrow(d), flagged = if ("flag" %in% names(d)) sum(nzchar(d$flag) & !is.na(d$flag)) else 0,
             if (nrow(last_alt)) setNames(last_alt$alt_m, paste0("active layer ", last_alt$year, " (m)")), if (nrow(last_zaa)) setNames(last_zaa$zaa_m, paste0("zero amplitude ", last_zaa$year, " (m)")),
             if (is.finite(deepest_frozen)) c(`deepest frozen depth (m)` = deepest_frozen))
  fr <- format(min(d$TIMESTAMP), "%Y-%m-%d"); to <- format(max(d$TIMESTAMP), "%Y-%m-%d")
  pf <- if (!is.null(lims)) lims$profiles[site == st] else NULL
  dl <- if (!is.null(lims$depth)) { z <- lims$depth[unit == st]; if (nrow(z)) c(z$hi[1], z$lo[1]) } else NULL          # reversed: depth runs down
  rpt_doc(report_title(ro, paste0(st, ": ground temperature"), name = st, from = fr, to = to, app = "GT"),
    report_subtitle(ro, paste0(fr, " to ", to, "  |  depths ", paste(zs, collapse = ", "), " m", sprintf("  |  times are UTC%+d", BASE_OFFSET),
                              if (!is.null(pf) && nrow(pf)) paste0("  |  ", pf$profile[1], " profile")), name = st, from = fr, to = to, app = "GT"), list(
    rpt_cards(cards),
    if (length(assumed)) rpt_text(paste0("Assumed depths (not given for the sensor, so taken from a template or the default steps): ", paste(assumed, collapse = ", "), " m. Treat them with care.")),
    if (want("info") && !is.null(info)) rpt_heading("Site and instrument"),
    if (want("info") && !is.null(info)) rpt_table(info, "From the site set-up table, the logger files and the registry.", col_width = 110L),
    if (want("logger") && !is.null(lglog)) rpt_heading("Logger log"),
    if (want("logger") && !is.null(lglog)) rpt_table(lglog, "What the loggers recorded about themselves: firmware, recording period, memory clears and resets, clock events and the battery.", col_width = 110L),
    if (want("availability") && !is.null(av)) rpt_heading("Data availability"),
    if (want("availability")) av,
    if (want("overview") && !is.null(ov)) rpt_heading("Temperature at each depth"),
    if (want("overview") && !is.null(ov)) rpt_text("Daily mean temperature at every depth (the pale band is the daily minimum to maximum), grouped by depth."),
    if (want("overview")) ov,
    if (want("stats") && !is.null(stt)) rpt_heading("Statistics by depth"),
    if (want("stats")) stt,
    if (want("sensors")) rpt_heading("Sensors and depths"),
    if (want("sensors")) rpt_table(if (nrow(he)) he[order(depth_m), .(depth_m, depth_source, logger_id, channel, health, pct_valid, values_kept, removed_qc, gaps, first_valid, last_valid)] else NULL),
    if (want("qc")) rpt_heading("Quality control"),
    if (want("qc")) rpt_quality_bars(rem_t, flg_t, MI),
    if (want("qc")) rpt_table(rem_t, "Removed values by rule (blank in the data; each is listed in output/audit/detail/)."),
    if (want("qc")) rpt_table(flg_t, "Flagged (suspect) values by check; they are kept in the data."),
    if (want("active_layer")) rpt_heading("Active layer"),
    if (want("active_layer")) rpt_table(alt, "Active-layer thickness by year, from the deepest depth that thawed (interpolated between sensors)."),
    if (want("active_layer")) rpt_table(zaa, "Depth of zero annual amplitude (annual range below 0.1 \u00b0C), by calendar year."),
    if (want("freeze_thaw")) rpt_heading("Freeze and thaw"),
    if (want("freeze_thaw")) rpt_table(if (nrow(ft)) ft[order(water_year, depth_m)] else NULL, "Per depth and water year: first and last day at or below 0 \u00b0C, days frozen, thawed and in the zero curtain."),
    if (want("annual")) rpt_heading("Annual statistics"),
    if (want("annual")) rpt_table(if (nrow(an)) an[year_type == "calendar"][order(year, depth_m), .(year, depth_m, mean, min, max, range, days, complete)] else NULL,
              "Calendar years; complete = enough days for the year to count.", 400),
    if (want("figures")) rpt_heading("Figures"),
    if (want("figures")) rpt_plot(try_or(plot_heatmap(d, title = st, res = "day", caption = assumed_caption(d), lim = axis_of(lims$heat, st, pf$profile[1]), depth_lim = dl), where = paste("report", st, "heat map")), 11, 6),
    if (want("figures") && nrow(an)) rpt_plot(try_or(plot_trumpet(an, title = paste(st, "- trumpet curves"), xlim = axis_of(lims$trumpet, st, pf$profile[1]), depth_lim = dl), where = paste("report", st, "trumpet curves")), 11, 7),
    if (want("figures") && nrow(dd)) rpt_plot(try_or(plot_freeze_thaw(dd, title = paste(st, "- frozen, zero curtain and thawed")), where = paste("report", st, "freeze-thaw")), 11, 6)))
}

# The one report picker of Reports and export: each report in HTML and in PDF, as "<format>|<what>", what being "" for the
# run report, a site, or "*" for every report (the run report and one per site exported, as a ZIP). report_pick() reads it back.
report_choices <- function(sites = character()) {
  one <- function(f, lab) setNames(paste0(f, "|", c("", "*", sites)), paste0(c("Run report", "Every report, as a ZIP", sites), " (", lab, ")"))
  list(HTML = one("html", "HTML"), PDF = one("pdf", "PDF"))
}
report_pick <- function(v) {
  v <- if (length(v) == 1 && !is.na(v) && grepl("|", v, fixed = TRUE)) v else "html|"
  f <- sub("\\|.*$", "", v); list(format = if (f %in% REPORT_FORMATS) f else "html", what = sub("^[^|]*\\|", "", v))
}

# Writes the run report and the chosen site reports into dir, in the chosen formats. Returns the files written.
write_gt_reports <- function(project_dir, dir, sites = NULL, formats = c("html", "pdf"), run = TRUE, progress = function(i, n, what) NULL, opts = NULL) {
  o <- report_opts(opts); lims <- try_or(gt_report_limits(project_dir, sites, o$axis), where = "report axis limits")
  if (!length(formats)) return(character())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE); out <- character()
  if (run) out <- c(out, report_write(gt_run_report_doc(project_dir), file.path(dir, "run_report"), formats, MI))
  for (i in seq_along(sites)) { progress(i, length(sites), sites[i])
    doc <- tryCatch(gt_site_report_doc(sites[i], project_dir, o, lims), error = function(e) { problem("report", paste(sites[i], conditionMessage(e))); NULL })
    if (!is.null(doc)) out <- c(out, report_write(doc, file.path(dir, report_file(o, safe_name(sites[i]), name = sites[i], app = "GT")), formats, MI)) }
  out
}
