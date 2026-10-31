# R/plots.R  --  The Nord dark theme, the plots used by the pipeline and the app.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

# Nord (arcticicestudio): Polar Night backgrounds, Snow Storm text, Frost blues as accents, Aurora for data and warnings.
# The colours and their role names are OVID_SCHEMES$Nord and nord_palette() in Ovid/theme.R.
NORD <- nord_palette()
# Settings > Colours offers these schemes (M, 2026-10-08; OVID_ALT_SCHEMES in Ovid/theme.R); the first is WLR's own,
# and in the others the accent is the scheme's cyan. APP_SCHEME is the one picked (app_settings.json), read at the start.
APP_SCHEMES <- c("Nord", "Iceberg", "Spacedust", "Aurora")
APP_SCHEME <- saved_scheme(APP_SCHEMES)
NORD_OWN <- NORD
app_palette <- function(scheme = APP_SCHEMES[1]) {
  p <- scheme_pick(NORD_OWN, scheme, "aqua", APP_SCHEMES)
  p <- legible(p, c("red", "orange", "purple", "deep"), 4.5)                     # Nord's red and purple are too dim for text on nord0
  p <- legible(p, "gray", 4.5, on = c(p$bg, p$bg1))                              # (Ovid/theme.R; the hue stays, only lightness rises)
  p$water <- p$frost                                                             # water level keeps the Frost blue
  p }
NORD <- app_palette(APP_SCHEME)
PAL <- NORD                                          # console colours (say() in the shared core)

NORD_PAL <- unname(unlist(NORD[c("frost", "yellow", "red", "green", "purple", "orange", "aqua", "deep", "fg4")]))

theme_nord <- function(base_size = 13) theme_ovid(NORD, base_size)          # Ovid/theme.R
scale_colour_nord <- function(...) scale_colour_ovid(NORD_PAL, ...)
scale_fill_nord   <- function(...) scale_fill_ovid(NORD_PAL, ...)
theme_set(theme_nord())

nord_save <- function(p, file, w = 11, h = 6) tryCatch({ ggsave(file, p, width = w, height = h, dpi = 110, bg = NORD$bg); TRUE },
                                                      error = function(e) { problem("plot", paste(basename(file), conditionMessage(e))); FALSE })

PARAM_LABEL <- c(pres_raw = "Raw pressure (file units)", pres_kpa = "Water pressure (kPa)", baro_kpa = "Barometric pressure (kPa)",
                 dp_kpa = "Water minus baro (kPa)", depth_m = "Water above sensor (m)", elev_m = "Water elevation (m)",
                 dtw_m = "Depth to water below reference (m)", temp_c = "Temperature (\u00b0C)", ec_uscm = "Conductivity (\u00b5S/cm)",
                 spc_uscm = "Specific conductance at 25 \u00b0C (\u00b5S/cm)")
param_label <- function(p) ifelse(p %in% names(PARAM_LABEL), PARAM_LABEL[p], p)


# One panel per parameter; lines break at gaps; flagged values in yellow, removed values as red ticks along the bottom,
# shaded spans (out of water, outside deployment) behind, and dashed lines at field visits.
plot_logger <- function(dt, params, title = "", subtitle = NULL, removed = NULL, spans = NULL, visits = NULL, tape = NULL,
                        max_points = 20000, xlim = NULL, meas = NULL) {
  params <- intersect(params, names(dt)); if (!length(params)) return(NULL)
  d <- if (!is.null(xlim)) dt[TIMESTAMP >= xlim[1] & TIMESTAMP <= xlim[2]] else dt
  if (!nrow(d)) return(NULL)
  step <- step_of(d$TIMESTAMP)
  long <- rbindlist(lapply(params, function(p) { fl <- if (paste0("flag_", p) %in% names(d)) d[[paste0("flag_", p)]] else rep("", nrow(d))
    x <- data.table(TIMESTAMP = d$TIMESTAMP, value = d[[p]], flag = nzchar(fl %||% ""), parameter = param_label(p))
    x[, seg := seg_id(TIMESTAMP, step)]; x[, seg := if (.N > 1) seg else -1L, by = seg]; thin(x, max_points) }))
  long[, parameter := factor(parameter, levels = param_label(params))]
  pc <- setNames(param_colours(params, NORD), param_label(params))          # water blue, temperature red, conductivity green, baro sand
  g <- ggplot(long, aes(TIMESTAMP, value, colour = parameter))
  if (!is.null(spans) && nrow(spans)) {
    sp <- spans[b >= min(d$TIMESTAMP) & a <= max(d$TIMESTAMP)]
    if (nrow(sp)) g <- g + geom_rect(data = sp, aes(xmin = pmax(a, min(d$TIMESTAMP)), xmax = pmin(b, max(d$TIMESTAMP)), ymin = -Inf, ymax = Inf, fill = what),
                                     inherit.aes = FALSE, alpha = 0.22) +
      scale_fill_manual(values = c("out of water" = NORD$red, "removed by hand" = NORD$orange, "outside deployment" = NORD$bg3), name = NULL)
  }
  visits <- visits[!is.na(visits)]
  if (length(visits)) g <- g + geom_vline(xintercept = visits[visits >= min(d$TIMESTAMP) & visits <= max(d$TIMESTAMP)],
                                                               colour = NORD$purple, linetype = "dashed", linewidth = 0.35)
  g <- g + geom_line(data = long[seg >= 0], aes(group = seg), linewidth = 0.35, na.rm = TRUE) + scale_colour_manual(values = pc, guide = "none")
  if (any(long$seg < 0)) g <- g + geom_point(data = long[seg < 0], size = 0.6, na.rm = TRUE)
  if (any(long$flag)) g <- g + geom_point(data = long[flag == TRUE], colour = NORD$yellow, shape = 1, size = 1.1, na.rm = TRUE)
  if (!is.null(removed) && nrow(removed)) { r <- removed[parameter %in% params & TIMESTAMP >= min(d$TIMESTAMP) & TIMESTAMP <= max(d$TIMESTAMP)]
    if (nrow(r)) { r[, parameter := factor(param_label(parameter), levels = levels(long$parameter))]
      g <- g + geom_rug(data = thin(r, 5000), aes(x = TIMESTAMP), inherit.aes = FALSE, colour = NORD$red, sides = "b", alpha = 0.6) } }
  if (!is.null(tape) && nrow(tape) && "depth_m" %in% params) {
    tp <- tape[!is.na(h_tape)][, parameter := factor(param_label("depth_m"), levels = levels(long$parameter))]
    if (nrow(tp)) g <- g + geom_point(data = tp, aes(visit_time, h_tape), inherit.aes = FALSE, colour = NORD$green, shape = 4, size = 3, stroke = 1.2)
  }
  if (!is.null(meas) && nrow(meas)) {                     # manual measurements: white diamonds, labelled with logger minus manual
    mm <- as.data.table(meas)[parameter %in% params & time >= min(d$TIMESTAMP) & time <= max(d$TIMESTAMP)]
    if (nrow(mm)) { mm[, parameter := factor(param_label(parameter), levels = levels(long$parameter))]
      mm[, lab := ifelse(is.na(difference), "", sprintf("%+.3g", difference))]
      g <- g + geom_point(data = mm, aes(time, manual), inherit.aes = FALSE, shape = 23, size = 2.8, fill = NORD$fg, colour = NORD$bg, stroke = 0.6) +
        geom_text(data = mm, aes(time, manual, label = lab), inherit.aes = FALSE, colour = NORD$fg, size = 3, vjust = -1.1) } }
  g + facet_wrap(~parameter, ncol = 1, scales = "free_y", strip.position = "top") +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) + theme(legend.position = if (!is.null(spans) && nrow(spans)) "bottom" else "none")
}

# Several loggers on one axis (sites in a baro group, or one site over many years).
plot_overlay <- function(dts, param, title = "", max_points = 8000) {
  long <- rbindlist(lapply(names(dts), function(n) { d <- dts[[n]]; if (is.null(d) || !param %in% names(d)) return(NULL)
    x <- data.table(TIMESTAMP = d$TIMESTAMP, value = d[[param]], series = n); x[, seg := seg_id(TIMESTAMP)]; thin(x, max_points) }))
  if (!nrow(long)) return(NULL)
  ggplot(long, aes(TIMESTAMP, value, colour = series, group = interaction(series, seg))) + geom_line(linewidth = 0.4, na.rm = TRUE) +
    scale_colour_nord(name = NULL) + labs(x = NULL, y = param_label(param), title = title)
}

# Barologgers side by side, and their daily differences.
plot_baro <- function(baros, title = "Barometric pressure") {
  if (!length(baros)) return(NULL)
  plot_overlay(lapply(baros, function(b) b[, .(TIMESTAMP, baro_kpa)]), "baro_kpa", title)
}
plot_baro_diff <- function(cmp) {
  if (is.null(cmp) || !nrow(cmp)) return(NULL)
  ggplot(cmp, aes(as.POSIXct(day), diff_kpa, colour = pair)) + geom_hline(yintercept = 0, colour = NORD$bg3) + geom_line(linewidth = 0.45) +
    scale_colour_nord(name = NULL) + labs(x = NULL, y = "Daily median difference (kPa)", title = "Barologgers compared")
}

# Where each logger was, year by year: one bar per deployment, coloured by site.
plot_deployments <- function(deps, recs = NULL) {
  if (!nrow(deps)) return(NULL)
  d <- copy(deps)
  if (!is.null(recs) && nrow(recs)) d <- merge(d, recs, by = "serial", all.x = TRUE) else d[, `:=`(first = as.POSIXct(NA, tz = "UTC"), last = as.POSIXct(NA, tz = "UTC"))]
  d[, a := fifelse(is.na(start), first, start)][, b := fifelse(is.na(end), last, end)]
  d <- d[!is.na(a) & !is.na(b)]
  if (!nrow(d)) return(NULL)
  d[, serial := factor(serial, levels = rev(sort(unique(serial))))]
  ggplot(d, aes(y = serial)) + geom_segment(aes(x = a, xend = b, yend = serial, colour = site), linewidth = 4, lineend = "butt") +
    geom_text(aes(x = a + (b - a) / 2, label = site), colour = NORD$bg_hard, size = 2.8, fontface = "bold", check_overlap = TRUE) +
    scale_colour_manual(values = rep(NORD_PAL, 50), guide = "none") + labs(x = NULL, y = NULL, title = "Where each logger was deployed")
}

# Files on a time line, one row per serial (Raw files page).
plot_files <- function(m) {
  d <- m[!is.na(first_ts) & !is.na(last_ts)]
  if (!nrow(d)) return(NULL)
  d[, row := factor(paste(kind, serial), levels = rev(sort(unique(paste(kind, serial)))))]
  ggplot(d, aes(y = row)) + geom_segment(aes(x = first_ts, xend = last_ts, yend = row, colour = ifelse(use, "used", "not used")), linewidth = 3.2, alpha = 0.8) +
    scale_colour_manual(values = c(used = NORD$frost, "not used" = NORD$bg3), name = NULL) + labs(x = NULL, y = NULL, title = "Files by logger")
}

plot_manual_check <- function(chk) {
  if (is.null(chk) || !nrow(chk[!is.na(difference_m)])) return(NULL)
  ggplot(chk[!is.na(difference_m)], aes(visit_time, 100 * difference_m, colour = site)) + geom_hline(yintercept = 0, colour = NORD$bg3) +
    geom_point(size = 2.4) + geom_line(aes(group = serial), linewidth = 0.3, alpha = 0.6) + scale_colour_manual(values = rep(NORD_PAL, 50), name = NULL) +
    labs(x = NULL, y = "Logger minus tape (cm)", title = "Logger depth against the tape at each visit")
}

# ---- comparisons ------------------------------------------------------------------------------------------------------
to_res <- function(d, param, res) {
  x <- d[, .(TIMESTAMP, value = get(param))]
  if (res == "day") x <- x[, .(value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)), by = .(TIMESTAMP = as.POSIXct(as.Date(TIMESTAMP)) + 0)]
  else if (res == "hour") x <- x[, .(value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)), by = .(TIMESTAMP = as.POSIXct(floor(as.numeric(TIMESTAMP) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"))]
  attr(x$TIMESTAMP, "tzone") <- "UTC"; x
}

# Several sites on one axis. mode "anomaly" takes each site's own median away, so sites at different depths can be compared.
compare_sites_data <- function(dts, param, res = "day", mode = "value") {
  rbindlist(lapply(names(dts), function(n) { d <- dts[[n]]; if (is.null(d) || !param %in% names(d)) return(NULL)
    x <- to_res(d, param, res); if (mode == "anomaly") x[, value := value - stats::median(value, na.rm = TRUE)]; x[, site := n] }))
}
plot_compare_sites <- function(long, param, mode = "value", title = NULL) {
  if (is.null(long) || !nrow(long)) return(NULL)
  long <- copy(long)[, seg := seg_id(TIMESTAMP), by = site]
  ggplot(long, aes(TIMESTAMP, value, colour = site, group = interaction(site, seg))) + geom_line(linewidth = 0.45, na.rm = TRUE) +
    scale_colour_nord(name = NULL) + labs(x = NULL, y = paste0(param_label(param), if (mode == "anomaly") ", difference from the site's median" else ""),
                                          title = title %||% paste(param_label(param), "by site"))
}

# One site, each year on the same axis. axis "season" starts on the water year's first day (1 October by default); "calendar" on 1 January.
compare_years_data <- function(d, param, res = "day", axis = "season", water_year_start = WATER_YEAR_START) {
  x <- to_res(d, param, res)
  if (axis == "season") { x[, yr := water_year_of(TIMESTAMP, water_year_start)]; start <- water_year_first_day(x$yr, water_year_start) }
  else { x[, yr := as.integer(format(TIMESTAMP, "%Y"))]; start <- as.POSIXct(paste0(x$yr, "-01-01"), tz = "UTC") }
  x[, day := as.numeric(difftime(TIMESTAMP, start, units = "days"))][, year := factor(yr)]
  x[, .(year, day, TIMESTAMP, value)]
}
plot_compare_years <- function(long, param, axis = "season", title = "", water_year_start = WATER_YEAR_START) {
  if (is.null(long) || !nrow(long)) return(NULL)
  long <- copy(long)[, seg := cumsum(c(1, diff(day) > 2)), by = year]
  ref <- if (axis == "season") as.Date(water_year_first_day(2002, water_year_start)) else as.Date("2002-01-01")
  br <- seq(0, 365, by = 30.4); lab <- format(ref + br, "%b")
  ggplot(long, aes(day, value, colour = year, group = interaction(year, seg))) + geom_line(linewidth = 0.5, na.rm = TRUE) +
    scale_x_continuous(breaks = br, labels = lab, limits = c(0, 366)) + scale_colour_nord(name = NULL) +
    labs(x = if (axis == "season") paste("water year from", water_year_label(water_year_start)) else NULL, y = param_label(param), title = title)
}

# ---- survey history ----------------------------------------------------------------------------------------------------
plot_gps_history <- function(h, sites = NULL) {
  if (is.null(h) || !nrow(h)) return(NULL)
  x <- copy(h); if (length(sites)) x <- x[site %in% sites]
  if (!nrow(x)) return(NULL)
  x[, label := paste(site, code)]
  ggplot(x, aes(time, change_from_first_m, colour = label, group = label)) + geom_hline(yintercept = 0, colour = NORD$bg3) +
    geom_line(linewidth = 0.4) + geom_errorbar(aes(ymin = change_from_first_m - 2 * rms, ymax = change_from_first_m + 2 * rms), width = 0, alpha = 0.6, na.rm = TRUE) +
    geom_point(size = 2.2) + scale_colour_manual(values = rep(NORD_PAL, 60), name = NULL) +
    labs(x = NULL, y = "Change from the first survey (m)", title = "Surveyed reference points over time", subtitle = "bars: \u00b12 \u00d7 the survey's elevation RMS") +
    theme(legend.position = if (uniqueN(x$label) > 14) "none" else "bottom")
}

# A baro station across years, coloured by the instrument that was there.
plot_baro_station <- function(st, name) {
  if (is.null(st) || !nrow(st)) return(NULL)
  x <- thin(copy(st), 30000)[, seg := seg_id(TIMESTAMP)]
  ggplot(x, aes(TIMESTAMP, baro_kpa, colour = as.character(baro_serial), group = interaction(baro_serial, seg))) + geom_line(linewidth = 0.35) +
    scale_colour_nord(name = "Barologger") + labs(x = NULL, y = "Barometric pressure (kPa)", title = paste("Baro station", name), subtitle = "one series stitched from every barologger that was there")
}
