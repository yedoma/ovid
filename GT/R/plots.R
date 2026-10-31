# R/plots.R  --  The soviet.nvim theme and every plot: heatmap, full profile, individual depths, box plots, profiles on dates,
# trumpet curves, seasonal overlays, climatology, freeze/thaw, thaw depth, availability, comparisons, soil.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.

theme_miasma <- function(base_size = 13) theme_ovid(MI, base_size, title = MI$sand, subtitle = MI$gray, legend = "right", strip_size = 0.85)   # Ovid/theme.R
theme_set(theme_miasma())
update_geom_defaults("line", list(colour = MI$orange, linewidth = 0.4))
update_geom_defaults("point", list(colour = MI$orange))

# Diverging ground-temperature scale centred on 0 C: soviet's enamel blue below (darkest coldest), a dimmed parchment at zero
# (so the bright 0 C contour stands out),
# gold / orange / red above.
DIV_COLS <- c(mix_col(MI$blue, MI$bg, 0.65), mix_col(MI$blue, MI$bg, 0.35), MI$blue, mix_col(MI$fg4, MI$bg, 0.4), mix_col(MI$sand, MI$bg, 0.3), MI$gold, MI$orange, MI$red, mix_col(MI$red, MI$bg, 0.3))
CAT_PAL <- c(MI$orange, MI$blue, MI$gold, MI$green, MI$purple, MI$fg4, MI$sand, MI$lime, MI$brown, MI$fg)
# Depths run warm to cool: the shallowest, which follow the air, in ochre orange, the deepest in soviet's enamel blue.
depth_ramp <- function(n) if (n <= 1) MI$orange else colorRampPalette(c(MI$orange, MI$gold, MI$lime, MI$fg4, MI$blue))(n)
STATE_COLS <- c(frozen = MI$blue, `zero curtain` = MI$sand, thawed = MI$orange)

gt_save <- function(p, file, w = 11, h = 6) tryCatch({ ggsave(file, p, width = w, height = min(h, 45), dpi = 120, bg = MI$bg, limitsize = FALSE); TRUE },
  # safe: the caller lists a figure that could not be saved
  error = function(e) FALSE)

msg_plot <- function(txt) ggplot() + annotate("text", x = 0, y = 0, label = txt, colour = MI$fg4, size = 5) + theme_void() +
  theme(plot.background = element_rect(fill = MI$bg, colour = NA))

# ---- data helpers -------------------------------------------------------------------------------------------------
RES_LABEL <- c(raw = "every reading", hour = "hourly means", day = "daily means", week = "weekly means", month = "monthly means")

resample <- function(d, res = "day") {
  if (is.null(d) || !nrow(d) || res == "raw") return(d)
  unit <- switch(res, hour = 3600, day = 86400, week = 7 * 86400, NA)
  tn <- if (res == "month") as.numeric(as.POSIXct(format(d$TIMESTAMP, "%Y-%m-01"), tz = "UTC")) else floor(as.numeric(d$TIMESTAMP) / unit) * unit
  by <- intersect(c("site", "variable", "depth_m"), names(d))
  hasf <- "flag" %in% names(d); hasq <- "qc" %in% names(d)           # qc: a WMO code per value; a mean gets the worst of its values
  x <- d[, c(by, "value", if (hasf) "flag", if (hasq) "qc"), with = FALSE][, tn := tn]
  out <- x[, c(list(value = mean(value), n = .N, flagged = if (hasf) sum(nzchar(flag)) else 0L), if (hasq) list(qc = qc_wmo_worst(qc))), by = c(by, "tn")]
  out[, TIMESTAMP := as.POSIXct(tn, origin = "1970-01-01", tz = "UTC")][, tn := NULL]
  out[, flag := fifelse(flagged > 0, "flagged", "")]
  out[]
}

# Thin for drawing only (never for statistics).

# Temperature axis: auto, fixed (lo, hi), symmetric around 0, or percentile-clipped.
temp_limits <- function(v, mode = "auto", lo = NA, hi = NA, pct = 1) {
  v <- v[is.finite(v)]; if (!length(v)) return(NULL)
  switch(mode, fixed = if (!is.na(lo) && !is.na(hi) && hi > lo) c(lo, hi) else NULL,
         sym = { m <- max(abs(range(v))); c(-m, m) },
         pct = as.numeric(stats::quantile(v, c(pct, 100 - pct) / 100)),
         NULL)
}

with_segments <- function(d) { d[, seg := seg_id(TIMESTAMP), by = depth_m]; d }

season_of <- function(t) { m <- as.integer(format(t, "%m"))
  factor(c("Winter (DJF)", "Winter (DJF)", "Spring (MAM)", "Spring (MAM)", "Spring (MAM)", "Summer (JJA)", "Summer (JJA)", "Summer (JJA)",
           "Autumn (SON)", "Autumn (SON)", "Autumn (SON)", "Winter (DJF)")[m], levels = c("Winter (DJF)", "Spring (MAM)", "Summer (JJA)", "Autumn (SON)")) }

assumed_caption <- function(d) { k <- d$depth_source %in% c("default", "template"); a <- unique(d$depth_m[k])
  if (length(a)) paste0("Assumed depths (", paste(unique(d$depth_source[k]), collapse = ", "), "): ", paste(depth_lab(sort(a)), collapse = ", ")) else NULL }

depth_scale <- function(zs, trans = "linear", expand = c(0.02, 0)) {
  if (trans == "sqrt") scale_y_continuous(trans = scales::trans_new("rsqrt", function(x) -sign(x) * sqrt(abs(x)), function(x) -sign(x) * x^2),
                                          breaks = zs, labels = depth_lab(zs), expand = expand, name = "Depth (m)")
  else scale_y_reverse(breaks = zs, labels = depth_lab(zs), expand = expand, name = "Depth (m)")
}

# ---- 1. contour heatmap -------------------------------------------------------------------------------------------
# Average onto a regular time step, interpolate linearly in depth only (never past the shallowest or deepest working
# sensor at that time), optionally smooth over a few time steps, then draw filled colour with the 0 C isotherm on top.
heatmap_grid <- function(d, res = "day", dz = 0.05, smooth = 1, interp = TRUE) {
  x <- resample(d, res)[, .(TIMESTAMP, depth_m, value)]
  if (!nrow(x)) return(data.table())
  zs <- sort(unique(x$depth_m))
  if (!interp || length(zs) < 2) {                                            # one band per sensor: nothing invented
    half <- if (length(zs) > 1) diff(zs) / 2 else 0.05
    edges <- c(zs[1] - half[1], zs[-length(zs)] + half, zs[length(zs)] + half[length(half)])
    i <- match(x$depth_m, zs)
    return(x[, .(TIMESTAMP, depth = depth_m, temp = value, ymin = edges[i], ymax = edges[i + 1L])])
  }
  grid_z <- seq(min(zs), max(zs), by = dz)
  if (length(grid_z) * uniqueN(x$TIMESTAMP) > 2e6) { dz <- (max(zs) - min(zs)) / max(20, floor(2e6 / uniqueN(x$TIMESTAMP))); grid_z <- seq(min(zs), max(zs), by = dz) }
  g <- x[order(depth_m), if (.N >= 2) list(depth = grid_z, temp = stats::approx(depth_m, value, grid_z, rule = 1)$y) else NULL, by = TIMESTAMP]
  if (smooth > 1) { setorder(g, depth, TIMESTAMP); g[, temp := frollmean(temp, smooth, align = "center", na.rm = TRUE), by = depth] }
  g
}

# The heatmap's smoothing from its one field: time steps, 0 (or 1) = off; blank = the default 7. Drawing only.
smooth_steps <- function(v, default = 7) { v <- suppressWarnings(as.numeric(v)); if (length(v) != 1 || is.na(v)) return(default); if (v < 2) 1 else round(v) }
plot_heatmap <- function(d, title = "", res = "day", dz = 0.05, smooth = 1, interp = TRUE, lim = NULL, isotherms = c(-5, 2, 5),
                         show_zero = TRUE, show_sensors = TRUE, trans = "linear", caption = NULL, depth_lim = NULL) {
  if (is.null(d) || !nrow(d)) return(msg_plot("No temperature data for this site."))
  g <- tryCatch(heatmap_grid(d, res, dz, smooth, interp), error = function(e) data.table())   # e.g. no reading at all in the period
  if (!nrow(g)) return(msg_plot("Not enough sensors to draw a profile."))
  v <- g$temp[is.finite(g$temp)]
  if (!length(v)) return(msg_plot("Not enough data to draw a profile for this period."))
  if (length(lim) != 2 || !all(is.finite(lim)) || lim[2] <= lim[1]) lim <- NULL      # a fixed scale with an empty box, or no spread: the automatic one
  if (is.null(lim)) lim <- { m <- stats::quantile(v, c(0.01, 0.99), na.rm = TRUE); c(min(m[[1]], -1), max(m[[2]], 1)) }
  stops <- sort(unique(c(lim[1], lim[1] * 0.55, lim[1] * 0.2, lim[1] * 0.04, 0, lim[2] * 0.08, lim[2] * 0.25, lim[2] * 0.55, lim[2])))
  stops <- stops[stops >= lim[1] & stops <= lim[2]]
  cols <- if (length(stops) == length(DIV_COLS)) DIV_COLS else colorRampPalette(DIV_COLS)(length(stops))
  g[, fill := pmin(pmax(temp, lim[1]), lim[2])]
  dt <- switch(res, raw = , hour = step_of(head(g$TIMESTAMP, 5000)), day = 86400, week = 7 * 86400, month = 30.4 * 86400, 86400)
  banded <- "ymin" %in% names(g)
  p <- ggplot(g, aes(TIMESTAMP, depth))
  p <- if (!banded) p + geom_raster(aes(fill = fill), na.rm = TRUE, hjust = 0)
       else p + geom_rect(aes(xmin = TIMESTAMP, xmax = TIMESTAMP + dt, ymin = ymin, ymax = ymax, fill = fill), na.rm = TRUE)
  if (!banded && uniqueN(g$depth) > 2) {
    if (length(isotherms)) p <- p + geom_contour(aes(z = temp), breaks = isotherms, colour = "#00000066", linewidth = 0.25, na.rm = TRUE)
    if (show_zero) p <- p + geom_contour(aes(z = temp), breaks = 0, colour = MI$fg, linewidth = 0.85, na.rm = TRUE)
  }
  zs <- sort(unique(d$depth_m))
  p <- p + scale_fill_gradientn(colours = cols, values = scales::rescale(stops, from = lim), limits = lim, name = "\u00b0C", oob = scales::squish, na.value = NA) +
    guides(fill = guide_colourbar(barheight = grid::unit(14, "lines"), barwidth = grid::unit(0.6, "lines"))) +
    scale_x_datetime(expand = c(0, 0)) + depth_scale(zs, trans, c(0, 0)) +
    labs(title = title, x = NULL, caption = caption,
         subtitle = paste0(RES_LABEL[[res]], if (!banded) paste0(", interpolated in depth every ", dz * 100, " cm") else ", one band per sensor (not interpolated)",
                           if (smooth > 1 && !banded) paste0(", smoothed over ", smooth, " steps") else "", if (show_zero && !banded) " \u00b7 cream line = 0 \u00b0C" else ""))
  if (show_sensors) p <- p + geom_hline(yintercept = zs, colour = "#ffffff1a", linewidth = 0.25)
  if (!is.null(depth_lim)) p <- p + coord_cartesian(ylim = depth_lim, expand = FALSE)        # the same depths as other sites of its kind (reports)
  p
}

# ---- 2. full profile: every depth on one axis ---------------------------------------------------------------------
plot_profile_lines <- function(d, title = "", res = "day", ylim = NULL, depths = NULL, caption = NULL) {
  if (is.null(d) || !nrow(d)) return(msg_plot("No temperature data for this site."))
  x <- resample(if (length(depths)) d[depth_m %in% depths] else d, res); x <- with_segments(thin(x[order(depth_m, TIMESTAMP)]))
  lv <- sort(unique(x$depth_m)); x[, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))]
  ggplot(x, aes(TIMESTAMP, value, colour = dep, group = interaction(dep, seg))) + geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.35, linetype = "22") +
    geom_line(linewidth = 0.45) + scale_colour_manual(values = depth_ramp(length(lv)), name = "Depth") +
    labs(title = title, subtitle = paste("Every depth on one axis,", RES_LABEL[[res]]), x = NULL, y = "Temperature (\u00b0C)", caption = caption) +
    coord_cartesian(ylim = ylim)
}

# ---- 3. individual depths ------------------------------------------------------------------------------------------
plot_depth_panels <- function(d, title = "", res = "day", ylim = NULL, free_y = TRUE, depths = NULL, ncol = 2, flags = TRUE, removed = NULL) {
  if (is.null(d) || !nrow(d)) return(msg_plot("No temperature data for this site."))
  x <- resample(if (length(depths)) d[depth_m %in% depths] else d, res); x <- with_segments(x[order(depth_m, TIMESTAMP)])
  lv <- sort(unique(x$depth_m)); x[, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))]
  p <- ggplot(x, aes(TIMESTAMP, value)) + geom_hline(yintercept = 0, colour = MI$gray, linewidth = 0.3) +
    geom_line(aes(colour = dep, group = seg), linewidth = 0.4, show.legend = FALSE) + scale_colour_manual(values = depth_ramp(length(lv))) +
    facet_wrap(~dep, ncol = ncol, scales = if (free_y && is.null(ylim)) "free_y" else "fixed") +
    labs(title = title, subtitle = paste0("One panel per sensor, ", RES_LABEL[[res]], if (free_y && is.null(ylim)) " \u00b7 temperature axis free per panel" else "",
                                         if (flags) " \u00b7 gold circles = flagged" else ""), x = NULL, y = "Temperature (\u00b0C)")
  if (flags) { f <- x[nzchar(flag)]; if (nrow(f)) p <- p + geom_point(data = f, shape = 1, size = 1.2, colour = MI$gold) }
  if (!is.null(removed) && nrow(removed)) { r <- removed[variable == "temp" & depth_m %in% lv][, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))]
    if (nrow(r)) p <- p + geom_point(data = thin(r, 20000), shape = 4, size = 1.1, colour = MI$red) }
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

# ---- 4. box plots by depth, on a chosen time scale ----------------------------------------------------------------
BOX_GROUPS <- c("Whole record" = "none", "Year" = "year", "Water year" = "water_year", "Season" = "season", "Month" = "month")
box_group <- function(t, group, wy_month = 10) switch(group, year = factor(format(t, "%Y")), water_year = factor(water_year(t, wy_month)), season = season_of(t),
                                                      month = factor(month.abb[as.integer(format(t, "%m"))], levels = month.abb), factor(rep("All", length(t))))
plot_boxplots <- function(d, title = "", res = "day", group = "none", ylim = NULL, layout = "dodge", wy_month = 10) {
  if (is.null(d) || !nrow(d)) return(msg_plot("No temperature data for this site."))
  x <- resample(d, res); lv <- sort(unique(x$depth_m))
  x[, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))][, grp := droplevels(box_group(TIMESTAMP, group, wy_month))]
  ng <- nlevels(x$grp); gname <- names(BOX_GROUPS)[BOX_GROUPS == group]
  sub <- paste0("Distribution of ", RES_LABEL[[res]], " at each depth", if (group != "none") paste0(", by ", tolower(gname)) else "",
                " \u00b7 box = middle 50 %, line = median, whiskers = 1.5 \u00d7 IQR")
  if (group == "none") {
    p <- ggplot(x, aes(dep, value, fill = dep)) + geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.3, linetype = "22") +
      geom_boxplot(colour = MI$fg4, outlier.size = 0.5, outlier.alpha = 0.4, outlier.colour = MI$fg4, linewidth = 0.3, width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = depth_ramp(length(lv)))
  } else if (layout == "dodge") {
    p <- ggplot(x, aes(dep, value, fill = grp)) + geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.3, linetype = "22") +
      geom_boxplot(colour = MI$fg4, outlier.size = 0.35, outlier.alpha = 0.35, outlier.colour = MI$fg4, linewidth = 0.25, width = 0.8, position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = rep(CAT_PAL, 10)[seq_len(ng)], name = gname) + theme(legend.position = "top")
  } else {
    p <- ggplot(x, aes(grp, value, fill = dep)) + geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.3, linetype = "22") +
      geom_boxplot(colour = MI$fg4, outlier.size = 0.3, outlier.alpha = 0.35, outlier.colour = MI$fg4, linewidth = 0.25, show.legend = FALSE) +
      scale_fill_manual(values = depth_ramp(length(lv))) + facet_wrap(~dep, scales = if (is.null(ylim)) "free_y" else "fixed") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  p <- p + labs(title = title, subtitle = sub, x = if (group != "none" && layout != "dodge") gname else "Depth", y = "Temperature (\u00b0C)")
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}
box_stats <- function(d, res = "day", group = "none", wy_month = 10) {
  x <- resample(d, res); x[, grp := as.character(box_group(TIMESTAMP, group, wy_month))]
  x[, .(n = .N, min = round(min(value), 3), q25 = round(stats::quantile(value, 0.25), 3), median = round(stats::median(value), 3), mean = round(mean(value), 3),
        q75 = round(stats::quantile(value, 0.75), 3), max = round(max(value), 3), sd = round(stats::sd(value), 3)), by = .(group = grp, depth_m)][order(group, depth_m)]
}

# ---- 5. profile on dates ------------------------------------------------------------------------------------------
plot_profile_dates <- function(dd, dates, title = "", xlim = NULL) {
  x <- dd[date %in% as.Date(dates)]
  if (!nrow(x)) return(msg_plot("No daily means on the dates chosen."))
  x[, day := factor(format(date))]
  ggplot(x[order(date, depth_m)], aes(temp, depth_m, colour = day, group = day)) + geom_vline(xintercept = 0, colour = MI$orange, linewidth = 0.35, linetype = "22") +
    geom_path(linewidth = 0.7) + geom_point(size = 1.8) + scale_y_reverse() + scale_colour_manual(values = rep(CAT_PAL, 10)[seq_len(nlevels(x$day))], name = "Date") +
    labs(title = title, subtitle = "Daily mean temperature against depth", x = "Temperature (\u00b0C)", y = "Depth (m)") + coord_cartesian(xlim = xlim)
}

# ---- 6. trumpet curves: maximum always red, minimum always blue, mean sand ----------------------------------------------
plot_trumpet <- function(an, years = NULL, yt = "calendar", title = "", layout = "facet", xlim = NULL, complete_only = FALSE, depth_lim = NULL) {
  x <- an[year_type == yt]; if (length(years)) x <- x[year %in% years]
  if (complete_only) x <- x[complete == TRUE]
  if (!nrow(x)) return(msg_plot("No annual statistics for the years chosen."))
  x <- x[order(year, depth_m)]; x[, yr := factor(year)]
  long <- rbind(x[, .(yr, depth_m, value = max, what = "Maximum", complete)], x[, .(yr, depth_m, value = mean, what = "Mean", complete)],
                x[, .(yr, depth_m, value = min, what = "Minimum", complete)])
  long[, what := factor(what, levels = c("Maximum", "Mean", "Minimum"))]
  p <- ggplot(long, aes(value, depth_m, colour = what, linetype = what)) + geom_vline(xintercept = 0, colour = MI$gray, linewidth = 0.35) +
    scale_colour_manual(values = c(Maximum = MI$red, Mean = MI$sand, Minimum = MI$blue), name = NULL) +
    scale_linetype_manual(values = c(Maximum = "solid", Mean = "42", Minimum = "solid"), name = NULL) + scale_y_reverse() +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
    labs(title = title, subtitle = paste0("Annual maximum (red), mean (sand) and minimum (blue) of daily means, ", if (yt == "water") "water years" else "calendar years",
                                         if (!complete_only) " \u00b7 hollow points = incomplete year" else ""), x = "Temperature (\u00b0C)", y = "Depth (m)") + theme(legend.position = "top")
  if (layout == "facet") p <- p + geom_path(aes(group = interaction(what, yr)), linewidth = 0.7) + geom_point(aes(shape = complete), size = 1.7) + facet_wrap(~yr)
  else p <- p + geom_path(aes(group = interaction(what, yr), alpha = yr), linewidth = 0.7) + geom_point(aes(alpha = yr, shape = complete), size = 1.5) +
         scale_alpha_discrete(range = c(0.3, 1), name = "Year")
  if (!is.null(xlim) || !is.null(depth_lim)) p <- p + coord_cartesian(xlim = xlim, ylim = depth_lim)
  p
}

# ---- 7. seasonal overlay and climatology --------------------------------------------------------------------------
doy_axis <- function(date, axis = "water", wy_month = 10) {
  if (axis == "water") { y <- water_year(as.POSIXct(date), wy_month); start <- as.Date(sprintf("%d-%02d-01", y - 1L, wy_month)); as.Date(sprintf("2001-%02d-01", wy_month)) + as.numeric(date - start) }
  else as.Date(paste0("2001-", format(date, "%m-%d")), optional = TRUE)
}
plot_seasonal <- function(dd, depth, axis = "water", title = "", ylim = NULL, wy_month = 10) {
  x <- dd[abs(depth_m - depth) < 1e-6]; if (!nrow(x)) return(msg_plot("No daily means at this depth."))
  x[, `:=`(yr = factor(if (axis == "water") water_year(as.POSIXct(date), wy_month) else as.integer(format(date, "%Y"))), ax = doy_axis(date, axis, wy_month))]
  x <- x[!is.na(ax)][order(yr, ax)]; x[, seg := cumsum(c(1, diff(as.numeric(ax)) != 1)), by = yr]
  ggplot(x, aes(ax, temp, colour = yr, group = interaction(yr, seg))) + geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.3, linetype = "22") +
    geom_line(linewidth = 0.55) + scale_colour_manual(values = rep(CAT_PAL, 10)[seq_len(nlevels(x$yr))], name = if (axis == "water") "Water year" else "Year") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.005, 0)) + coord_cartesian(ylim = ylim) +
    labs(title = title, subtitle = paste0(depth_lab(depth), ": each ", if (axis == "water") paste0("water year (", wy_months(wy_month), ")") else "year", " on a common axis, daily means"), x = NULL, y = "Temperature (\u00b0C)")
}
plot_climatology <- function(dd, depth, highlight = NULL, title = "", ylim = NULL) {
  x <- dd[abs(depth_m - depth) < 1e-6]; if (!nrow(x)) return(msg_plot("No daily means at this depth."))
  x[, ax := doy_axis(date, "calendar")]; x <- x[!is.na(ax)]
  cl <- x[, .(mean = mean(temp), lo = min(temp), hi = max(temp), years = .N), by = ax][order(ax)]
  p <- ggplot(cl, aes(ax)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = MI$bg3, alpha = 0.7) + geom_line(aes(y = mean), colour = MI$fg4, linewidth = 0.5) +
    geom_hline(yintercept = 0, colour = MI$orange, linewidth = 0.3, linetype = "22")
  if (length(highlight) && nzchar(highlight)) { h <- x[format(date, "%Y") == highlight][order(ax)]; if (nrow(h)) p <- p + geom_line(data = h, aes(ax, temp), colour = MI$orange, linewidth = 0.7) }
  p + scale_x_date(date_labels = "%b", date_breaks = "1 month") + coord_cartesian(ylim = ylim) +
    labs(title = title, subtitle = paste0(depth_lab(depth), ": mean by day of year (grey line) and the range of all years (band)", if (length(highlight) && nzchar(highlight)) paste0(", ", highlight, " in orange") else ""), x = NULL, y = "Temperature (\u00b0C)")
}

# ---- 8. annual means by depth --------------------------------------------------------------------------------------
plot_annual <- function(an, yt = "calendar", title = "", complete_only = TRUE) {
  x <- an[year_type == yt]; if (complete_only) x <- x[complete == TRUE]
  if (!nrow(x)) return(msg_plot("No complete years yet (switch off 'complete years only' to see partial years)."))
  lv <- sort(unique(x$depth_m)); x[, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))]
  ggplot(x, aes(year, mean, colour = dep)) + geom_hline(yintercept = 0, colour = MI$gray, linewidth = 0.3) + geom_line(linewidth = 0.6) + geom_point(size = 1.8) +
    scale_colour_manual(values = depth_ramp(length(lv)), name = "Depth") + scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 1)) +
    labs(title = title, subtitle = "Annual mean of daily means (MAGT) at each depth", x = NULL, y = "Temperature (\u00b0C)")
}

# ---- 9. freeze / thaw timeline and thaw depth ------------------------------------------------------------------------
plot_freeze_thaw <- function(dd, band = 0.2, title = "") {
  if (!nrow(dd)) return(msg_plot("No daily means for this site."))
  x <- copy(dd); lv <- sort(unique(x$depth_m))
  x[, state := factor(fifelse(temp < -band, "frozen", fifelse(temp > band, "thawed", "zero curtain")), levels = names(STATE_COLS))]
  x[, dep := factor(depth_lab(depth_m), levels = rev(depth_lab(lv)))]
  ggplot(x, aes(as.POSIXct(date), dep, fill = state)) + geom_tile(height = 0.9, width = 86400) + scale_fill_manual(values = STATE_COLS, name = NULL, drop = FALSE) +
    scale_x_datetime(expand = c(0, 0)) + theme(legend.position = "top") +
    labs(title = title, subtitle = paste0("Daily state at each depth: frozen (below \u2212", band, " \u00b0C), zero curtain (within \u00b1", band, " \u00b0C), thawed (above ", band, " \u00b0C)"), x = NULL, y = NULL)
}
plot_thaw_depth <- function(td, alt, title = "") {
  x <- td[!is.na(thaw_m)][order(date)]; if (!nrow(x)) return(msg_plot("The 0 \u00b0C line never falls between two sensors at this site."))
  x[, seg := cumsum(c(1, diff(as.numeric(date)) > 1))]
  p <- ggplot(x, aes(date, thaw_m)) + geom_line(aes(group = seg), colour = MI$orange, linewidth = 0.6) + scale_y_reverse() +
    labs(title = title, subtitle = "Depth of the 0 \u00b0C line each day (interpolated between sensors); red points = active-layer thickness of the year", x = NULL, y = "Thaw depth (m)")
  a <- alt[!is.na(alt_m)]; if (nrow(a)) p <- p + geom_point(data = a, aes(date_alt, alt_m), colour = MI$red, size = 2.6) +
    geom_text(data = a, aes(date_alt, alt_m, label = sprintf("%.2f m", alt_m)), colour = MI$fg, vjust = -1, size = 3.4)
  p
}

# ---- 10. availability and QC -------------------------------------------------------------------------------------
plot_availability <- function(d, title = "") {
  if (is.null(d) || !nrow(d)) return(msg_plot("No data."))
  x <- d[, .(n = .N, flagged = sum(nzchar(flag))), by = .(depth_m, date = as.Date(TIMESTAMP))]
  lv <- sort(unique(x$depth_m)); x[, dep := factor(depth_lab(depth_m), levels = rev(depth_lab(lv)))]
  x[, state := fifelse(flagged > 0, "some values flagged", "data")]
  ggplot(x, aes(as.POSIXct(date), dep, fill = state)) + geom_tile(height = 0.85, width = 86400) +
    scale_fill_manual(values = c(data = MI$green, `some values flagged` = MI$gold), name = NULL) + scale_x_datetime(expand = c(0, 0)) +
    labs(title = title, subtitle = "Days with data at each depth (blank = no data: a gap, a failed sensor or removed values)", x = NULL, y = NULL) + theme(legend.position = "top")
}

# ---- 11. comparing sites -----------------------------------------------------------------------------------------
# For each site, the sensor nearest the chosen depth (within tol). mode: value, difference from a reference site, or anomaly
# (difference from the site's own mean).
compare_data <- function(dts, depth, tol = 0.05, res = "day", mode = "value", ref = NULL) {
  out <- rbindlist(lapply(names(dts), function(s) { d <- dts[[s]]; if (is.null(d) || !nrow(d)) return(NULL)
    z <- sort(unique(d$depth_m)); k <- z[which.min(abs(z - depth))]; if (!length(k) || abs(k - depth) > tol + 1e-9) return(NULL)
    x <- resample(d[depth_m == k], res); x[, `:=`(site = s, actual_depth = k)][, .(site, actual_depth, TIMESTAMP, value)] }))
  if (!nrow(out)) return(out)
  if (mode == "anomaly") out[, value := value - mean(value), by = site]
  if (mode == "diff" && length(ref) && ref %in% out$site) { r <- out[site == ref, .(TIMESTAMP, rv = value)]; out <- merge(out, r, by = "TIMESTAMP"); out[, value := value - rv][, rv := NULL] }
  out
}
plot_compare <- function(x, depth, mode = "value", ref = NULL, title = "", ylim = NULL, kind = "line") {
  if (!nrow(x)) return(msg_plot("None of these sites has a sensor near this depth."))
  x[, lab := paste0(site, " (", depth_lab(actual_depth), ")")]
  if (kind == "scatter") {
    s <- unique(x$site); if (length(s) < 2) return(msg_plot("Pick two sites for a scatter."))
    w <- dcast(x, TIMESTAMP ~ site, value.var = "value"); setnames(w, s[1:2], c("a", "b")); w <- w[!is.na(a) & !is.na(b)]
    if (!nrow(w)) return(msg_plot("These two sites have no times in common."))
    return(ggplot(w, aes(a, b)) + geom_abline(colour = MI$gray, linetype = "22") + geom_point(colour = MI$orange, alpha = 0.35, size = 0.9) +
             labs(title = title, subtitle = paste0(depth_lab(depth), ": one point per time step; dashed line = 1:1"), x = s[1], y = s[2]) + coord_equal())
  }
  x[, seg := seg_id(TIMESTAMP), by = site]
  ggplot(x, aes(TIMESTAMP, value, colour = lab, group = interaction(lab, seg))) + geom_hline(yintercept = 0, colour = MI$gray, linewidth = 0.3) +
    geom_line(linewidth = 0.5) + scale_colour_manual(values = rep(CAT_PAL, 10)[seq_len(uniqueN(x$lab))], name = NULL) + coord_cartesian(ylim = ylim) +
    labs(title = title, subtitle = paste0("Sensors nearest ", depth_lab(depth), switch(mode, anomaly = " \u00b7 difference from each site's own mean", diff = paste0(" \u00b7 difference from ", ref), "")),
         x = NULL, y = if (mode == "value") "\u00b0C" else "\u0394 \u00b0C") + theme(legend.position = "top")
}

# ---- 12. soil moisture and conductivity (kept apart from ground temperature) ----------------------------------------
SOIL_LABEL <- c(vwc = "Volumetric water content (m\u00b3/m\u00b3)", ec = "Bulk electrical conductivity (mS/cm)")
plot_soil <- function(s, var = "vwc", res = "day", title = "", ylim = NULL) {
  x <- s[variable == var]; if (!nrow(x)) return(msg_plot("No values of this kind at this site."))
  x <- resample(x, res); x <- with_segments(x[order(depth_m, TIMESTAMP)]); lv <- sort(unique(x$depth_m))
  x[, dep := factor(depth_lab(depth_m), levels = depth_lab(lv))]
  ggplot(x, aes(TIMESTAMP, value, colour = dep, group = interaction(dep, seg))) + geom_line(linewidth = 0.5) +
    scale_colour_manual(values = colorRampPalette(if (var == "vwc") c(mix_col(MI$blue, MI$fg, 0.5), MI$blue, MI$slate) else c(MI$lime, MI$green, mix_col(MI$green, MI$bg, 0.35)))(length(lv)), name = "Depth")   # water blue, conductivity green + coord_cartesian(ylim = ylim) +
    labs(title = title, subtitle = paste0(SOIL_LABEL[[var]], ", ", RES_LABEL[[res]]), x = NULL, y = SOIL_LABEL[[var]])
}

# ---- 13. which logger covers what ---------------------------------------------------------------------------------
plot_coverage <- function(lg) {
  if (!nrow(lg)) return(msg_plot("No loggers."))
  x <- copy(lg)[order(logger_type, serial)]; x[, lab := factor(paste(logger_id, "\u2192", sites), levels = rev(unique(paste(logger_id, "\u2192", sites))))]
  ggplot(x, aes(y = lab, colour = logger_type)) + geom_segment(aes(x = as.POSIXct(first), xend = as.POSIXct(last), yend = lab), linewidth = 3.2, lineend = "round") +
    scale_colour_manual(values = setNames(CAT_PAL[1:5], names(TYPE_LABEL)), labels = TYPE_LABEL, name = NULL) +
    labs(title = NULL, x = NULL, y = NULL) + theme(legend.position = "top")
}
