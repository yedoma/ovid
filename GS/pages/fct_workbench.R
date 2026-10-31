# GS/pages/fct_workbench.R  --  The figures behind the Workbench's Stack and Edit data tabs, as plain functions (no Shiny;
# tests call them): where each electrode sits along the line, its contact resistance and flags, the data levels, what a
# contact cutoff would flag, the flags per rule, and the aligned panels of the Stack. Sourced by workbench/wb_gs.R.
# Raw values only: nothing is smoothed or fitted. Contact limits 2, 5 and 10 kohm are EarthImager's (AGI 2009, 3.5.9).

CONTACT_LINES <- c(`2 kohm` = 2000, `5 kohm` = 5000, `10 kohm` = 10000)   # guide lines, all one neutral colour
ELECTRODE_COL <- c(A = "#d7191c", B = "#fdae61", M = "#2c7bb6", N = "#abd9e9")      # the four electrodes of a measurement

# Each electrode's distance along the line, measured the way pseudo_xy() measures the points (x, or the distance from the
# first A electrode on an x/y line).
electrode_dist <- function(d) {
  e <- rbindlist(lapply(c("a", "b", "m", "n"), function(k) d[, .(electrode = get(k), x = get(paste0(k, "x")), y = get(paste0(k, "y")))]))
  e <- unique(e[!is.na(electrode) & electrode > 0 & is.finite(x)], by = "electrode")
  if (!nrow(e)) return(data.table(electrode = integer(), dist = numeric()))
  flat <- all(d$ay == 0 & d$by == 0 & d$my == 0 & d$ny == 0, na.rm = TRUE)
  e[, dist := if (flat) x else sqrt((x - d$ax[1])^2 + (y - d$ay[1])^2)]
  setorder(e[, .(electrode, dist)], electrode)[]
}

# Contact resistance per electrode: every A-B reading (contact.csv: addr_a, addr_b, contact_ohm) counted for both electrodes,
# with the median of each electrode's readings, at its distance. NULL when the survey recorded none.
electrode_contact <- function(contact, ed) {
  if (is.null(contact) || !nrow(contact) || !all(c("addr_a", "addr_b", "contact_ohm") %in% names(contact))) return(NULL)
  e <- rbind(contact[, .(electrode = as.integer(addr_a), contact_ohm = as.numeric(contact_ohm))], contact[, .(electrode = as.integer(addr_b), contact_ohm = as.numeric(contact_ohm))])
  e <- e[is.finite(contact_ohm) & contact_ohm > 0]; if (!nrow(e)) return(NULL)
  m <- e[, .(n = .N, median_ohm = stats::median(contact_ohm), max_ohm = max(contact_ohm)), by = electrode]
  merge(m, ed, by = "electrode")[order(electrode)]
}

# Per electrode: the measurements that use it and how many of them are flagged.
electrode_flags <- function(d, flagged, ed) {
  u <- rbindlist(lapply(c("a", "b", "m", "n"), function(k) d[, .(electrode = get(k), meas_id)]))[!is.na(electrode) & electrode > 0]
  u <- unique(u)[, .(used = .N, flagged = sum(meas_id %in% flagged)), by = electrode]
  merge(u, ed, by = "electrode")[, pct := 100 * flagged / used][order(electrode)]
}

# The data levels: one pseudo-depth with the same current and potential dipole lengths (as the moving_median rule groups them),
# so different arrays are not mixed. Adds px, pz, level (a label, shallowest first) and level_no.
data_levels <- function(d) {
  x <- cbind(d[, .(meas_id, a, b, m, n, rhoa_ohmm)], pseudo_xy(d))
  dl <- function(p, q) { r <- abs(d[[p]] - d[[q]]); r[!is.finite(r)] <- -1; round(r, 2) }
  x[, `:=`(lev = round(pz, 2), ab = dl("ax", "bx"), mn = dl("mx", "nx"))]
  k <- unique(x[is.finite(lev), .(lev, ab, mn)])[order(-lev, ab, mn)][, level_no := .I]
  k[, level := sprintf("%d · %s m deep · AB %s, MN %s m", level_no, format(abs(lev)), ifelse(ab < 0, "remote", format(ab)), ifelse(mn < 0, "remote", format(mn)))]
  merge(x, k, by = c("lev", "ab", "mn"), all.x = TRUE)[order(level_no, px)]
}

# What a contact cutoff (ohm) would catch: measurements whose contact reading is above it.
contact_cutoff <- function(d, cutoff) { v <- suppressWarnings(as.numeric(d$contact_ohm)); d$meas_id[is.finite(v) & v > cutoff] }

# Flags per rule (a measurement can be in several, as EarthImager counts them), and the measurements flagged by any.
rule_counts <- function(flags, n_total) {
  if (is.null(flags) || !nrow(flags)) return(data.table(rule = character(), rule_id = character(), flag = character(), n = integer(), pct = numeric()))
  f <- as.data.table(flags); if (!"rule_id" %in% names(f)) f[, rule_id := rule]
  f[, .(n = uniqueN(meas_id), flag = paste(unique(flag), collapse = ", ")), by = .(rule, rule_id)][, pct := 100 * n / max(n_total, 1)][order(-n)]
}

# The Stack tab's panels on one distance axis (plotly, shared x, one cursor across them): the pseudosection (flagged points
# crossed), contact per electrode with the 2, 5 and 10 kohm lines, flagged share per electrode, and the change image when a
# comparable survey is given (ch: stack_change()). panels: which to draw, in order. Points carry their measurement number and
# electrodes their number (customdata), so a click picks them.
wb_stack_figure <- function(d, flagged, contact, ef, ch = NULL, panels = c("pseudo", "contact", "flags", "change"), pal = PAL, lim = RHOA_LIM) {
  pp <- list(); hs <- numeric()
  xr <- range(c(pseudo_xy(d)$px, ef$dist), na.rm = TRUE)
  ax <- function(t, log = FALSE, rev = FALSE) list(title = list(text = t, font = list(size = 11)), type = if (log) "log" else "linear", autorange = if (rev) "reversed" else TRUE, zeroline = FALSE)
  for (p in panels) {
    if (p == "pseudo") { x <- cbind(d[, .(meas_id, a, b, m, n, rhoa_ohmm)], pseudo_xy(d))[is.finite(px) & is.finite(pz)]
      x[, `:=`(c = to_log(rhoa_ohmm, "rhoa_ohmm"), fl = meas_id %in% flagged)]
      x[, hv := sprintf("#%d · %s ohm-m<br>A%s B%s M%s N%s%s", meas_id, fmtv(rhoa_ohmm), a, b, m, n, ifelse(fl, "<br>flagged", ""))]
      g <- plotly::plot_ly(source = "wbstack") |>
        plotly::add_markers(data = x[fl == FALSE], x = ~px, y = ~pz, customdata = ~meas_id, text = ~hv, hoverinfo = "text", name = "measurements", showlegend = FALSE,
                            marker = list(size = 6, color = ~c, colorscale = lapply(seq_along(PALETTES$Spectral), function(i) list((i - 1) / (length(PALETTES$Spectral) - 1), PALETTES$Spectral[i])),
                                          cmin = lim[1], cmax = lim[2], colorbar = list(title = "Apparent resistivity (log10 of \u03a9\u00b7m)", len = 0.3, y = 1, yanchor = "top"))) |>
        plotly::add_markers(data = x[fl == TRUE], x = ~px, y = ~pz, customdata = ~meas_id, text = ~hv, hoverinfo = "text", name = "flagged", showlegend = FALSE,
                            marker = list(symbol = "x", size = 7, color = pal$red))
      pp[[length(pp) + 1]] <- plotly::layout(g, yaxis = ax("Pseudo-depth (m)")); hs <- c(hs, 0.4) }
    if (p == "contact") {
      g <- plotly::plot_ly(source = "wbstack")
      if (is.null(contact)) g <- plotly::add_annotations(g, text = "No contact resistance recorded for this survey", showarrow = FALSE, x = mean(xr), y = 3.5) |> plotly::layout(yaxis = ax("contact (ohm)"))
      else { g <- plotly::add_trace(g, data = contact, x = ~dist, y = ~median_ohm, customdata = ~paste0("e", electrode), type = "scatter", mode = "lines+markers", name = "median contact", showlegend = FALSE,
                                    text = ~sprintf("electrode %d at %s m<br>median %s ohm (%d readings)", electrode, fmtv(dist), fmtv(median_ohm), n), hoverinfo = "text",
                                    line = list(color = pal$accent), marker = list(size = 5, color = pal$accent))
        for (k in names(CONTACT_LINES)) g <- plotly::add_lines(g, x = xr, y = CONTACT_LINES[[k]], name = k, showlegend = FALSE, hoverinfo = "name",
                                                               line = list(dash = "dot", width = 1, color = pal$fg4))
        g <- plotly::layout(g, yaxis = ax("contact (ohm)", log = TRUE)) }
      pp[[length(pp) + 1]] <- g; hs <- c(hs, 0.2) }
    if (p == "flags") {
      g <- plotly::plot_ly(source = "wbstack", data = ef, x = ~dist, y = ~pct, customdata = ~paste0("e", electrode), type = "bar", name = "flagged", showlegend = FALSE,
                           text = ~sprintf("electrode %d at %s m<br>%d of %d measurements flagged (%s %%)", electrode, fmtv(dist), flagged, used, round(pct)), hoverinfo = "text",
                           marker = list(color = pal$red))
      pp[[length(pp) + 1]] <- plotly::layout(g, yaxis = ax("flagged (%)")); hs <- c(hs, 0.15) }
    if (p == "change" && !is.null(ch) && nrow(ch$data)) {
      g <- plotly::plot_ly(source = "wbstack", data = ch$data, x = ~px, y = ~pz, type = "scatter", mode = "markers", name = "change", showlegend = FALSE,
                           text = ~sprintf("%+.1f %%", change_pct), hoverinfo = "text",
                           marker = list(size = 6, color = ~pmin(pmax(change_pct, -25), 25), cmin = -25, cmax = 25,
                                         colorscale = list(list(0, "#2166ac"), list(0.5, "#f7f7f7"), list(1, "#b2182b")), colorbar = list(title = "Change (%)", len = 0.25, y = 0.25, yanchor = "top")))
      pp[[length(pp) + 1]] <- plotly::layout(g, yaxis = ax("Pseudo-depth (m)")); hs <- c(hs, 0.3) }
  }
  if (!length(pp)) return(NULL)
  f <- plotly::subplot(pp, nrows = length(pp), shareX = TRUE, titleY = TRUE, heights = hs / sum(hs), margin = 0.015)
  xa <- list(title = "Distance along line (m)", showspikes = TRUE, spikemode = "across", spikesnap = "cursor", spikethickness = 1, spikecolor = pal$fg4, spikedash = "solid")
  plotly::layout(f, hovermode = "closest", xaxis = xa, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = pal$fg, size = 11),
                 margin = list(l = 60, r = 20, t = 10, b = 40)) |> plotly::config(displaylogo = FALSE)
}

# One measurement's four electrodes on the line, as a small SVG (A red, B orange, M blue, N light blue).
quad_svg <- function(w, ed, width = 260) {
  if (!nrow(ed)) return("")
  r <- range(ed$dist); sx <- function(v) 10 + (width - 20) * (v - r[1]) / max(diff(r), 1e-9)
  dots <- vapply(c("A", "B", "M", "N"), function(k) { e <- w[[tolower(k)]]; x <- ed$dist[match(e, ed$electrode)]; if (is.na(x)) return("")
    sprintf("<circle cx='%.1f' cy='14' r='5' fill='%s'/><text x='%.1f' y='32' font-size='10' text-anchor='middle' fill='currentColor'>%s%s</text>", sx(x), ELECTRODE_COL[[k]], sx(x), k, e) }, "")
  sprintf("<svg width='%d' height='38' role='img' aria-label='electrodes'><line x1='10' y1='14' x2='%d' y2='14' stroke='currentColor' stroke-opacity='.4'/>%s</svg>", width, width - 10, paste(dots, collapse = ""))
}
