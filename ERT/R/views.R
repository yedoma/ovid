# R/views.R  --  Interactive views for the app (plotly): pseudosection with hover values, distributions, contact
# resistance by electrode, error against current, electrode geometry. Sourced by app.R only (the pipeline does not need plotly).
# The views follow the diagnostic plots of AGI EarthImager (repeat error, current, voltage and contact resistance plots)
# and ResIPy (pseudosection, error and electrode views).

VAR_INFO <- data.table(
  var   = c("rhoa_ohmm", "resistance_ohm", "current_ma", "voltage_mv", "error_pct", "contact_ohm", "chargeability_mv_v"),
  label = c("Apparent resistivity (ohm-m, log)", "Resistance V/I (ohm, log)", "Current (mA, log)", "Voltage (mV, log)", "Repeat error (%, log)", "Contact resistance (ohm, log)", "Chargeability (mV/V, log)"),
  floor = c(NA, NA, NA, NA, 0.05, NA, 0.01))      # every value axis is logarithmic; zero errors and chargeabilities are drawn at this floor
RHOA_LIM <- log10(RHOA_RANGE)                     # apparent resistivity is drawn on RHOA_RANGE (R/plots.R; log10) so surveys compare
to_log <- function(v, colour) { f <- VAR_INFO[var == colour]$floor; if (!is.na(f)) v <- pmax(v, f); ifelse(!is.na(v) & v > 0, log10(v), NA_real_) }
PALETTES <- list(Spectral = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), Viridis = c("#440154", "#3b528b", "#21908c", "#5dc963", "#fde725"),
                 Kanagawa = c("#7E9CD8", "#7FB4CA", "#E6C384", "#FFA066", "#E46876"), Greys = c("#2A2A37", "#DCD7BA"))

# Measurements with their flags and the derived values the views use.
view_data <- function(d, flags) {
  fl <- flags[, .(flag = paste(unique(flag), collapse = ", ")), by = meas_id]
  p <- merge(cbind(copy(d), pseudo_xy(d)), fl, by = "meas_id", all.x = TRUE, sort = FALSE)
  p[, `:=`(flagged = !is.na(flag), current_ma = current_a * 1000, voltage_mv = voltage_v * 1000)][is.na(flag), flag := ""]
  p[, hover := sprintf("<b>%s ohm-m</b><br>R = %s ohm<br>I = %s mA, V = %s mV<br>error %s %%%s<br>A %s  B %s  M %s  N %s<br>%s%s",
                       fmtv(rhoa_ohmm), fmtv(resistance_ohm), fmtv(current_ma), fmtv(voltage_mv), fmtv(error_pct),
                       ifelse(is.na(contact_ohm), "", paste0("<br>contact ", fmtv(contact_ohm), " ohm")),
                       a, b, m, n, { t <- if (inherits(datetime, "POSIXct")) format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC") else as.character(datetime); ifelse(is.na(t) | t == "", "", paste0(t, "<br>")) },
                       ifelse(flagged, paste0("<span style='color:", PAL$red, "'>flagged: ", flag, "</span>"), "passes QC"))]
  p
}
# Ticks for a log10 axis: powers of ten, with 2 and 5 between them when the range is narrow.
log_ticks <- function(lim) { t <- seq(floor(lim[1]), ceiling(lim[2])); if (length(t) < 4) t <- sort(c(t, t + log10(2), t + log10(5)))
  t <- t[t >= lim[1] - 0.01 & t <= lim[2] + 0.01]; list(tickvals = t, ticktext = fmtv(10^t)) }
fmtv <- function(x) ifelse(is.na(x), "-", formatC(signif(x, 4), format = "fg", big.mark = ",", digits = 4))

plotly_layout <- function(p, xlab, ylab, ylog = FALSE, xlog = FALSE, yrev = FALSE) plotly::layout(p,
  paper_bgcolor = PAL$bg1, plot_bgcolor = PAL$bg1, font = list(color = PAL$fg, family = "Inter, Avenir Next, Segoe UI, -apple-system, Helvetica, Arial, sans-serif", size = 13),
  xaxis = list(title = xlab, gridcolor = PAL$bg2, zerolinecolor = PAL$bg2, type = if (xlog) "log" else "linear"),
  yaxis = list(title = ylab, gridcolor = PAL$bg2, zerolinecolor = PAL$bg2, type = if (ylog) "log" else "linear", autorange = if (yrev) "reversed" else TRUE),
  legend = list(orientation = "h", y = -0.15), margin = list(l = 60, r = 20, t = 20, b = 50), hoverlabel = list(bgcolor = PAL$bg, font = list(color = PAL$fg))) |>
  plotly::config(displaylogo = FALSE, scrollZoom = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))

PSEUDO_FLAG <- "reason"   # how Survey and QC draw flagged points (by reason), and their point size; once settings in Display options
PSEUDO_SIZE <- 7
# Pseudosection. colour: a VAR_INFO key; show: all / passed / flagged; flag_style (with show = all): grey, ring, colour or hide.
# logscale: log10 (default) or linear colours; range: the apparent-resistivity colour range in ohm-m (default 1 to 10,000).
view_pseudosection <- function(p, colour = "rhoa_ohmm", show = "all", flag_style = "grey", palette = "Spectral", logscale = TRUE, size = 7, range = 10^RHOA_LIM) {
  vi <- VAR_INFO[var == colour]; vi$label <- sub(", log\\)", if (logscale) ", log)" else ", linear)", vi$label)
  rng <- suppressWarnings(as.numeric(range)); if (length(rng) != 2 || any(!is.finite(rng)) || rng[1] >= rng[2] || (logscale && rng[1] <= 0)) rng <- 10^RHOA_LIM
  p <- copy(p); p[, cval := if (logscale) to_log(p[[colour]], colour) else p[[colour]]]
  base <- switch(show, passed = p[flagged == FALSE], flagged = p[flagged == TRUE], p)
  coloured <- if (show == "all" && flag_style != "colour") base[flagged == FALSE & !is.na(cval)] else base[!is.na(cval)]
  if (show == "flagged" && flag_style == "reason") coloured <- coloured[0]
  lim <- if (colour == "rhoa_ohmm") (if (logscale) log10(rng) else rng) else quantile(coloured$cval, c(0.02, 0.98), na.rm = TRUE); if (!all(is.finite(lim))) lim <- c(0, 1)
  if (diff(lim) == 0) lim <- lim + c(-0.5, 0.5)
  tick <- if (logscale) log_ticks(lim) else list()
  pal <- PALETTES[[palette]]; cs <- Map(list, seq(0, 1, length.out = length(pal)), pal)
  g <- plotly::plot_ly(type = "scatter", mode = "markers")
  add_coloured <- function(g) if (!nrow(coloured)) g else plotly::add_markers(g, data = coloured, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", name = if (show == "flagged") "flagged" else "measurements",
    marker = list(size = size, color = pmin(pmax(coloured$cval, lim[1]), lim[2]), colorscale = cs, cmin = lim[1], cmax = lim[2], line = list(width = 0),
                  colorbar = c(list(title = list(text = vi$label, side = "right"), thickness = 12, outlinewidth = 0), tick)))
  if (flag_style != "reason" || show == "passed") g <- add_coloured(g)
  fl <- base[flagged == TRUE & !is.na(cval)]
  if (show == "all" && nrow(fl) && flag_style == "grey") g <- plotly::add_markers(g, data = fl, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", name = "flagged",
    marker = list(size = size * 0.8, color = PAL$gray, opacity = 0.7))
  if (show == "all" && nrow(fl) && flag_style == "ring") g <- plotly::add_markers(g, data = fl, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", name = "flagged",
    marker = list(size = size + 4, color = "rgba(0,0,0,0)", line = list(color = PAL$red, width = 1.5)))
  if (show == "all" && nrow(fl) && flag_style == "ring") g <- plotly::add_markers(g, data = fl, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", showlegend = FALSE,
    marker = list(size = size, color = pmin(pmax(fl$cval, lim[1]), lim[2]), colorscale = cs, cmin = lim[1], cmax = lim[2], line = list(width = 0)))
  if (flag_style == "reason" && show != "passed") {                  # flagged points on top, one colour and legend entry per reason
    fr <- base[flagged == TRUE][, reason := sub(",.*", "", flag)]
    cols <- distinct_cols(c(PAL$red, PAL$gold, PAL$purple, PAL$orange, PAL$teal, PAL$green, "#ffffff", PAL$blue), PAL)   # told apart with colour blindness too (core/theme.R)
    rs <- names(sort(table(fr$reason), decreasing = TRUE))
    for (i in seq_along(rs)) { q <- fr[reason == rs[i]]
      g <- plotly::add_markers(g, data = q, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", name = paste0(rs[i], " (", nrow(q), ")"),
        marker = list(size = size + 1, symbol = "x", color = cols[(i - 1) %% length(cols) + 1], line = list(width = 0))) }
    base <- base[flagged == FALSE]
    g <- add_coloured(g)                    # passing points last, so they stay visible where a flagged point shares their position
  }
  bad <- base[is.na(cval)]
  if (nrow(bad) && show != "passed" && flag_style != "hide") g <- plotly::add_markers(g, data = bad, x = ~px, y = ~pz, text = ~hover, hoverinfo = "text", name = "no value or <= 0",
    marker = list(size = size, symbol = "x-thin-open", color = PAL$red, line = list(color = PAL$red, width = 1.5)))
  plotly_layout(g, "Distance along line (m)", "Pseudo-depth (m)")
}

view_histogram <- function(p, colour = "rhoa_ohmm", logscale = TRUE, range = 10^RHOA_LIM) {
  vi <- VAR_INFO[var == colour]; vi$label <- sub(", log\\)", if (logscale) ", log)" else ", linear)", vi$label)
  rng <- suppressWarnings(as.numeric(range)); if (length(rng) != 2 || any(!is.finite(rng)) || rng[1] >= rng[2] || (logscale && rng[1] <= 0)) rng <- 10^RHOA_LIM
  q <- copy(p)[, xv := if (logscale) to_log(p[[colour]], colour) else p[[colour]]][!is.na(xv)]
  lr <- if (logscale) log10(rng) else rng
  if (colour == "rhoa_ohmm") q[, xv := pmin(pmax(xv, lr[1]), lr[2])]                       # same range as the pseudosection
  g <- plotly::plot_ly(alpha = 0.8)
  for (s in c(FALSE, TRUE)) if (nrow(q[flagged == s])) g <- plotly::add_histogram(g, x = q[flagged == s]$xv, name = if (s) "flagged" else "passes QC", nbinsx = 50,
    marker = list(color = if (s) PAL$red else PAL$accent))
  g <- plotly::layout(g, barmode = "overlay")
  if (!logscale) return(plotly_layout(g, vi$label, "Measurements", ylog = TRUE))
  f <- q$xv[is.finite(q$xv)]; if (colour != "rhoa_ohmm" && !length(f)) return(plotly_layout(g, vi$label, "Measurements", ylog = TRUE))   # nothing to draw: no ticks to place
  t <- if (colour == "rhoa_ohmm") log_ticks(lr)$tickvals else seq(floor(min(f)), ceiling(max(f)))
  plotly_layout(g, vi$label, "Measurements", ylog = TRUE) |> plotly::layout(xaxis = list(tickvals = t, ticktext = fmtv(10^t)))
}

# Contact resistance by electrode: each current injection's A-B value drawn at both of its electrodes.
view_contact <- function(contact) {
  if (is.null(contact) || !nrow(contact)) return(plotly_layout(plotly::plot_ly(type = "scatter", mode = "markers"), "Electrode", "Contact resistance (ohm)") |>
    plotly::layout(annotations = list(text = "No contact resistance recorded for this survey", showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5)))
  e <- rbind(contact[, .(electrode = addr_a, contact_ohm)], contact[, .(electrode = addr_b, contact_ohm)])
  med <- e[, .(med = median(contact_ohm)), by = electrode][order(electrode)]
  g <- plotly::plot_ly() |>
    plotly::add_markers(data = e, x = ~electrode, y = ~contact_ohm, name = "readings", marker = list(size = 4, color = PAL$fg4, opacity = 0.35),
                        text = ~paste0("electrode ", electrode, "<br>", fmtv(contact_ohm), " ohm"), hoverinfo = "text") |>
    plotly::add_lines(data = med, x = ~electrode, y = ~med, name = "median", line = list(color = PAL$accent, width = 2),
                      text = ~paste0("electrode ", electrode, "<br>median ", fmtv(med), " ohm"), hoverinfo = "text")
  for (lim in c(2000, 5000, 10000))                     # guide lines (EarthImager 3.5.9), one neutral colour, named by their value
    g <- plotly::add_lines(g, x = range(e$electrode), y = lim, name = paste(fmtv(lim / 1000), "kohm"), line = list(color = PAL$fg4, dash = "dot", width = 1), hoverinfo = "name")
  plotly_layout(g, "Electrode", "Contact resistance, A-B pair (ohm)", ylog = TRUE)
}

view_error_current <- function(p) {
  q <- p[!is.na(current_ma) & current_ma > 0 & !is.na(error_pct)][, error_plot := pmax(error_pct, 0.05)]
  g <- plotly::plot_ly()
  for (s in c(FALSE, TRUE)) if (nrow(q[flagged == s])) g <- plotly::add_markers(g, data = q[flagged == s], x = ~current_ma, y = ~error_plot, text = ~hover, hoverinfo = "text",
    name = if (s) "flagged" else "passes QC", marker = list(size = 6, color = if (s) PAL$red else PAL$accent, opacity = 0.7))
  plotly_layout(g, "Current (mA)", "Repeat error (%; 0 drawn at 0.05)", xlog = TRUE, ylog = TRUE)
}

view_geometry <- function(e) {
  if (is.null(e) || !nrow(e)) return(plotly_layout(plotly::plot_ly(type = "scatter", mode = "markers"), "Distance (m)", "Elevation (m)"))
  e <- copy(e)[order(electrode)]; e[, s := c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))]
  st <- diff(e$s); sp <- median(st); gap <- c(FALSE, abs(st - sp) > 0.05 * sp)
  plotly::plot_ly() |>
    plotly::add_lines(data = e, x = ~s, y = ~z, name = "ground", line = list(color = PAL$fg4, width = 1), hoverinfo = "none") |>
    plotly::add_markers(data = e, x = ~s, y = ~z, name = "electrodes", text = ~sprintf("electrode %d<br>x %s  y %s  z %s", electrode, fmtv(x), fmtv(y), fmtv(z)),
                        hoverinfo = "text", marker = list(size = 7, color = ifelse(gap, PAL$gold, PAL$accent))) |>
    plotly_layout("Distance along line (m)", "Elevation (m)") |>
    plotly::layout(annotations = list(
    list(text = "start", x = e$s[1], y = e$z[1], showarrow = TRUE, arrowcolor = PAL$fg4, font = list(color = PAL$fg4)),
    list(text = "end", x = e$s[nrow(e)], y = e$z[nrow(e)], showarrow = TRUE, arrowcolor = PAL$fg4, font = list(color = PAL$fg4))))
}

# Grouped facts about a survey as label / value pairs; empty values are left out.
survey_facts <- function(s) {
  f <- function(x) if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(as.character(x[1])) || as.character(x[1]) == "NA") NULL else as.character(x[1])
  u <- function(x, unit) if (is.null(f(x))) NULL else paste(fmtv(as.numeric(x)), unit)
  dur <- if (!is.null(f(s$datetime_start)) && !is.null(f(s$datetime_end))) fmt_span(as.numeric(difftime(as.POSIXct(s$datetime_end, tz = "UTC"), as.POSIXct(s$datetime_start, tz = "UTC"), units = "secs")))
  list(
    Instrument = list(Manufacturer = f(s$manufacturer), Model = f(s$model_lbl %||% s$model), Serial = f(s$serial), Firmware = f(s$firmware), Software = f(s$software)),
    Acquisition = list(`First reading` = f(s$datetime_start), `Last reading` = f(s$datetime_end), Duration = dur,
                       Records = f(paste(s$n_read, if (!is.null(f(s$n_stated))) paste("of", s$n_stated, "stated"))), `Command file` = f(s$cmd_program), Commands = f(s$n_commands)),
    Geometry = list(Array = f(s$array_lbl %||% s$array), Electrodes = f(s$n_electrodes), Spacing = u(s$spacing_m, "m"), `Line length` = u(s$length_m, "m"),
                    `Elevation range` = if (isTRUE(as.numeric(s$elevation_range_m) > 0)) u(s$elevation_range_m, "m"), `Unused positions` = if (isTRUE(as.numeric(s$electrode_gaps) > 0)) f(s$electrode_gaps),
                    Coordinates = if (isTRUE(as.logical(s$has_coordinates))) "x, y, z" else "along the line only"),
    Signal = list(`Median apparent resistivity` = u(s$rhoa_median, "ohm-m"), `Median current` = u(s$current_ma_median, "mA"), `Median repeat error` = u(s$error_pct_median, "%"),
                  `Negative values` = f(s$n_negative_rhoa), `Kept %` = if (!is.null(f(s$pct_retained))) paste0(s$pct_retained, "% (", gsub("_", " ", s$qc_profile), ")")),
    Record = list(`Survey id` = f(s$survey_id), Type = f(s$kind_lbl %||% s$kind), Format = f(s$format_lbl %||% s$format), Transect = f(s$transect_id), Processing = f(s$processing), `Linked to` = f(s$parent_ids), Status = f(c(ok = "Read", incomplete = "Incomplete", empty = "Empty", read_error = "Could not be read", needs_reader = "No reader yet")[s$status] %||% s$status)))
}
facts_ui <- function(facts) div(class = "facts", lapply(names(facts), function(g) { x <- Filter(Negate(is.null), facts[[g]]); if (!length(x)) return(NULL)
  div(class = "fact-group", div(class = "fact-title", g), lapply(names(x), function(k) div(class = "fact", span(class = "k", k), span(class = "v", x[[k]])))) }))
