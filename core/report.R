# Ovid/report.R  --  Reports in HTML, PDF or both, built once from the same blocks.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# An app builds a report as a list of blocks and report_write() renders it in the formats the user picked:
#   rpt_heading("Stations")                   a section heading
#   rpt_text("One paragraph.", "Another.")    paragraphs (long lines wrap)
#   rpt_cards(c(files = 120, loggers = 14))   big numbers with a label under each
#   rpt_table(dt, "caption", max_rows = 200)  a table (the PDF repeats the header on each page and drops columns that do not fit)
#   rpt_plot(ggplot, w = 11, h = 6)           a figure (a page of its own in the PDF)
#   rpt_stats(dt, params, group)              a table of statistics for each parameter (valid values, coverage, min, mean, median, max, sd, first and last)
#   rpt_overview(long, pal)                   every parameter as a small chart, grouped and paged (a list of figures; see below)
# The HTML is one self-contained file (figures are embedded), so it can be mailed or archived on its own.
# Both formats use the app's palette (pal: bg, bg1, bg2, fg, fg4, accent, ...), so a report looks like the app it came from.

REPORT_FORMATS <- c("HTML" = "html", "PDF" = "pdf")

# Blocks may be nested (rpt_overview returns a list of figures): they are flattened here.
.rpt_flat <- function(x) { out <- list(); for (b in x) { if (is.null(b)) next; if (is.null(b$type)) out <- c(out, .rpt_flat(b)) else out[[length(out) + 1L]] <- b }; out }
rpt_doc     <- function(title, subtitle = "", blocks = list()) list(title = title, subtitle = subtitle, blocks = .rpt_flat(blocks))
# The five QA/QC level colours (ok, warn, error, info, off) taken from an app palette, the same ones the app shell uses.
rpt_levels  <- function(pal) c(ok = .or(pal$green, "#7fa563"), warn = .or(pal$yellow, pal$gold, "#e6c384"), error = .or(pal$red, "#cc6666"), info = .or(pal$accent, pal$blue, "#81a2be"), off = .or(pal$gray, "#888888"))
rpt_heading <- function(text) list(type = "heading", text = text)
rpt_text    <- function(...) { x <- unlist(list(...)); x <- x[!is.na(x)]; if (!length(x)) NULL else list(type = "text", text = as.character(x)) }
rpt_cards   <- function(x) list(type = "cards", values = x)
# col_width: the widest a column is drawn in the PDF (characters); raise it for tables of long text such as a key/value list.
rpt_table   <- function(dt, caption = NULL, max_rows = 200, col_width = 28L) list(type = "table", data = if (is.null(dt)) data.table() else as.data.table(dt), caption = caption, max_rows = max_rows, col_width = col_width)
# inline = TRUE: the figure flows with the text on the page (keep h under about 4.5 in) instead of taking a page of its own.
rpt_plot    <- function(p, w = 11, h = 6, caption = NULL, inline = FALSE) if (is.null(p)) NULL else list(type = "plot", plot = p, w = w, h = h, caption = caption, inline = isTRUE(inline))

# ---- Parameter statistics ---------------------------------------------------------------------------------------------
# One row per parameter, worked out from every record (not from daily means) without copying the data: wide data (one column per
# parameter, as the apps hold a station) go through rpt_stats(); data already long (group, parameter, time, value) through
# rpt_stats_long(). Returns a table block; put an rpt_heading() before it.
.stat_row <- function(v, tm, total = length(v)) {
  ok <- !is.na(v) & is.finite(v); n <- sum(ok); fl <- function(z) if (n) format(as.Date(z), "%Y-%m-%d") else ""
  if (!n) return(list(values = 0L, `% of records` = 0, min = NA_real_, mean = NA_real_, median = NA_real_, max = NA_real_, sd = NA_real_, first = "", last = ""))
  x <- v[ok]; r <- range(tm[ok])
  list(values = n, `% of records` = round(100 * n / total, 1), min = min(x), mean = mean(x), median = stats::median(x), max = max(x), sd = if (n > 1) stats::sd(x) else NA_real_, first = fl(r[1]), last = fl(r[2]))
}
rpt_stats <- function(dt, params, group = NULL, time = "TIMESTAMP", caption = "Worked out from every record, not from daily means. Blank values are not counted.", max_rows = 500) {
  params <- params[vapply(params, function(p) is.numeric(dt[[p]]) && any(!is.na(dt[[p]])), NA)]; if (!length(params)) return(NULL)
  tm <- dt[[time]]
  rows <- lapply(params, function(p) data.table::as.data.table(.stat_row(dt[[p]], tm)))
  out <- data.table::rbindlist(rows); out <- cbind(if (!is.null(group)) data.table::data.table(group = unname(group[match(params, names(group))])) else NULL, parameter = params, out)
  if (!is.null(group)) out <- out[order(match(group, unique(group)))]
  rpt_table(out, caption, max_rows)
}
rpt_stats_long <- function(x, caption = "Worked out from the daily means shown in the overview.", max_rows = 500, dates = TRUE) {
  x <- data.table::as.data.table(x); if (!nrow(x)) return(NULL)
  out <- x[, data.table::as.data.table(.stat_row(value, time)), by = .(group, parameter)]; if (!dates) out[, c("first", "last") := NULL]
  rpt_table(out, caption, max_rows)
}

# ---- Summary figures ----------------------------------------------------------------------------------------------------
# Data availability: one row per parameter, a bar wherever it has data (days with at least one value, runs closer than `tol_days` joined) and
# a blank where it has none, grouped like the overview, with the share of days that have data at the right. Gaps and the order in which
# sensors came and went are visible at a glance. Worked out from wide data without copying it. Returns a list of figure blocks (one page
# holds up to `per_page` parameters; a group stays together when it fits).
rpt_availability <- function(dt, params, group, time = "TIMESTAMP", pal, per_page = 26L, tol_days = 2L, title = "Data availability") {
  params <- params[vapply(params, function(p) is.numeric(dt[[p]]) && any(!is.na(dt[[p]])), NA)]; if (!length(params)) return(NULL)
  day <- as.integer(as.Date(time_vec <- dt[[time]])); d0 <- min(day, na.rm = TRUE); d1 <- max(day, na.rm = TRUE); span <- d1 - d0 + 1L
  runs <- data.table::rbindlist(lapply(params, function(p) { d <- sort(unique(day[!is.na(dt[[p]])])); if (!length(d)) return(NULL)
    g <- cumsum(c(TRUE, diff(d) > tol_days)); r <- data.table::data.table(d, g)[, .(s = min(d), e = max(d)), by = g]; r[, parameter := p]; r[, g := NULL]; r }))
  info <- data.table::data.table(parameter = params, group = unname(group[params]))
  info[, pct := round(100 * vapply(params, function(p) length(unique(day[!is.na(dt[[p]])])), 0L) / span)]
  kc <- tryCatch(kind_colours(pal), error = function(e) NULL)
  col_of <- function(p) { k <- tryCatch(unname(kc[param_kind(p)]), error = function(e) NULL); if (length(k) && !is.na(k)) k else pal$accent }
  cols <- stats::setNames(vapply(params, col_of, ""), params)
  groups <- unique(info$group); pages <- list(); cur <- character()
  for (g in groups) { ps <- info$parameter[info$group == g]
    if (length(cur) && length(cur) + length(ps) > per_page && length(ps) <= per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() }
    for (p in ps) { cur <- c(cur, p); if (length(cur) == per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() } } }
  if (length(cur)) pages[[length(pages) + 1L]] <- cur
  x0 <- as.Date(d0, origin = "1970-01-01"); x1 <- as.Date(d1 + 1L, origin = "1970-01-01")
  out <- vector("list", length(pages))
  for (i in seq_along(pages)) { ps <- pages[[i]]; inf <- info[match(ps, parameter)]
    inf[, `:=`(parameter = factor(parameter, levels = rev(ps)), group = factor(group, levels = unique(group)))]
    r <- runs[parameter %in% ps]; r[, parameter := factor(parameter, levels = rev(ps))]; r[, group := inf$group[match(parameter, inf$parameter)]]
    r[, `:=`(xmin = as.Date(s, origin = "1970-01-01"), xmax = as.Date(e + 1L, origin = "1970-01-01"))]
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = inf, ggplot2::aes(xmin = x0, xmax = x1, ymin = as.numeric(parameter) - 0.34, ymax = as.numeric(parameter) + 0.34), fill = pal$bg1) +
      ggplot2::geom_rect(data = r, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = as.numeric(parameter) - 0.34, ymax = as.numeric(parameter) + 0.34, fill = parameter)) +
      ggplot2::geom_text(data = inf, ggplot2::aes(x = x1, y = as.numeric(parameter), label = paste0(pct, "%")), hjust = -0.25, size = 2.6, colour = pal$fg4) +
      ggplot2::scale_fill_manual(values = cols, guide = "none") +
      ggplot2::scale_y_continuous(breaks = seq_along(levels(inf$parameter)), labels = levels(inf$parameter), expand = ggplot2::expansion(add = 0.5)) +
      ggplot2::scale_x_date(limits = c(x0, x1), expand = c(0, 0), breaks = scales::breaks_pretty(8)) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(title = if (length(pages) > 1) sprintf("%s  (%d of %d)", title, i, length(pages)),
                    subtitle = sprintf("Bars show where a parameter has data (%s to %s); the figure at the right is the share of days with data", format(x0), format(x1 - 1L)), x = NULL, y = NULL) +
      theme_ovid(pal, base_size = 9, legend = "none") +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(colour = pal$fg, size = 7.5), plot.title = ggplot2::element_text(size = 12, face = "bold"),
                     plot.margin = ggplot2::margin(12, 34, 10, 12))
    p <- p + ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, colour = pal$fg4, face = "plain", size = 7.5), strip.background = ggplot2::element_blank(),
                     panel.spacing.y = grid::unit(7, "pt"), panel.border = ggplot2::element_blank())
    ng <- length(unique(inf$group))
    out[[i]] <- rpt_plot(p, 11, min(8.5, 1.5 + 0.2 * length(ps) + 0.14 * ng)) }
  out
}

# The same chart for data that carry a status (valid, flagged, outside the deployment, removed ...) and are held long: x has group, row (the label
# of each bar row), time and status; `status_cols` names the colour of each status in the order they are listed in the legend. A day takes its
# best status (the first listed in `status_cols` that occurs that day), runs of the same status are joined across gaps up to `tol_days`.
# The share at the right is the share of days in the span whose best status is the first one. Returns a list of figure blocks.
rpt_availability_long <- function(x, pal, status_cols, per_page = 26L, tol_days = 2L, title = "Data availability", marks = NULL, mark_label = NULL) {
  x <- data.table::as.data.table(x); if (!nrow(x)) return(NULL)
  lv <- names(status_cols); x[, `:=`(row = as.character(row), group = as.character(group), day = as.integer(as.Date(time)), st = match(as.character(status), lv))]; x <- x[!is.na(st)]
  d0 <- min(x$day); d1 <- max(x$day); span <- d1 - d0 + 1L
  dd <- x[, .(st = min(st)), by = .(row, day)][order(row, day)]
  dd[, run := cumsum(c(TRUE, st[-1] != st[-.N] | diff(day) > tol_days + 1L)), by = row]
  runs <- dd[, .(s = min(day), e = max(day), st = st[1]), by = .(row, run)]
  rows <- x[, .(group = group[1]), by = row]; rows[, pct := round(100 * dd[st == 1L, .N, by = row]$N[match(row, dd[st == 1L, .N, by = row]$row)] / span)]; rows[is.na(pct), pct := 0]
  rows <- rows[order(match(group, unique(group)))]
  groups <- unique(rows$group); pages <- list(); cur <- character()
  for (g in groups) { ps <- rows$row[rows$group == g]
    if (length(cur) && length(cur) + length(ps) > per_page && length(ps) <= per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() }
    for (p in ps) { cur <- c(cur, p); if (length(cur) == per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() } } }
  if (length(cur)) pages[[length(pages) + 1L]] <- cur
  x0 <- as.Date(d0, origin = "1970-01-01"); x1 <- as.Date(d1 + 1L, origin = "1970-01-01"); out <- vector("list", length(pages))
  for (i in seq_along(pages)) { ps <- pages[[i]]; inf <- rows[match(ps, row)]
    inf[, `:=`(row = factor(row, levels = rev(ps)), group = factor(group, levels = unique(group)))]
    r <- runs[row %in% ps]; r[, row := factor(row, levels = rev(ps))]; r[, group := inf$group[match(row, inf$row)]]
    r[, `:=`(xmin = as.Date(s, origin = "1970-01-01"), xmax = as.Date(e + 1L, origin = "1970-01-01"), status = factor(lv[st], levels = lv))]
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = inf, ggplot2::aes(xmin = x0, xmax = x1, ymin = as.numeric(row) - 0.34, ymax = as.numeric(row) + 0.34), fill = pal$bg1) +
      ggplot2::geom_rect(data = r, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = as.numeric(row) - 0.34, ymax = as.numeric(row) + 0.34, fill = status)) +
      ggplot2::geom_text(data = inf, ggplot2::aes(x = x1, y = as.numeric(row), label = paste0(pct, "%")), hjust = -0.25, size = 2.6, colour = pal$fg4) +
      (if (!is.null(marks) && nrow(marks)) ggplot2::geom_vline(data = marks, ggplot2::aes(xintercept = as.Date(time)), colour = pal$fg4, linewidth = 0.25, linetype = "dashed") else NULL) +
      ggplot2::scale_fill_manual(values = status_cols, limits = names(status_cols)[names(status_cols) %in% lv[unique(x$st)]], name = NULL) +
      ggplot2::scale_y_continuous(breaks = seq_along(levels(inf$row)), labels = levels(inf$row), expand = ggplot2::expansion(add = 0.5)) +
      ggplot2::scale_x_date(limits = c(x0, x1), expand = c(0, 0), breaks = scales::breaks_pretty(8)) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(title = if (length(pages) > 1) sprintf("%s  (%d of %d)", title, i, length(pages)),
                    subtitle = sprintf("%s to %s.  Right: share of days with %s data.%s", format(x0), format(x1 - 1L), tolower(lv[1]), if (!is.null(mark_label)) paste0("  Dashed: ", mark_label, ".") else ""), x = NULL, y = NULL) +
      theme_ovid(pal, base_size = 9, legend = "bottom") +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(colour = pal$fg, size = 7.5), plot.margin = ggplot2::margin(12, 34, 10, 12), legend.key.size = grid::unit(10, "pt"))
    p <- p + ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, colour = pal$fg4, face = "plain", size = 7.5), strip.background = ggplot2::element_blank(),
                     panel.spacing.y = grid::unit(7, "pt"), panel.border = ggplot2::element_blank())
    out[[i]] <- rpt_plot(p, 11, min(8.5, 1.7 + 0.2 * length(ps) + 0.14 * length(unique(inf$group))), inline = length(pages) == 1L && length(ps) <= 8L) }      # a short chart shares the page with what is around it
  out
}

# Horizontal bars of what the quality rules removed and flagged: removed and flagged are data frames with name and values columns.
rpt_quality_bars <- function(removed, flagged, pal, title = "Quality control at a glance") {
  one <- function(d, what) if (is.null(d) || !nrow(d)) NULL else data.table::data.table(what = what, name = as.character(d[[1]]), values = as.numeric(d[[2]]))
  l <- data.table::rbindlist(list(one(removed, "Removed (blank in the data)"), one(flagged, "Flagged (kept in the data)"))); if (is.null(l) || !nrow(l)) return(NULL)
  l[, what := factor(what, levels = unique(what))]; l[, name := factor(name, levels = rev(unique(name[order(-values)])))]
  col <- c("Removed (blank in the data)" = pal$red, "Flagged (kept in the data)" = pal$yellow)
  p <- ggplot2::ggplot(l, ggplot2::aes(values, name, fill = what)) + ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = format(values, big.mark = ",", trim = TRUE, scientific = FALSE)), hjust = -0.15, size = 2.8, colour = pal$fg4) +
    ggplot2::facet_wrap(~what, scales = "free", nrow = 1) + ggplot2::scale_fill_manual(values = col, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.3)), labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::labs(title = title, x = NULL, y = NULL) + theme_ovid(pal, base_size = 9, legend = "none") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(colour = pal$fg, size = 8), plot.title = ggplot2::element_text(size = 11, face = "bold"))
  rpt_plot(p, 11, min(3.4, 1.2 + 0.3 * max(table(l$what))), inline = TRUE)
}

# ---- Parameter overview -----------------------------------------------------------------------------------------------
# One small chart for every parameter, so a station with dozens of them can be taken in at a glance. x is long data with
# columns group, parameter, time (POSIXct or Date) and value (daily means are the usual choice; NA rows are fine and count as gaps).
# Parameters that belong together (same group) stay together: a group is kept whole on a page when it fits, and a larger group
# runs on to the next page. Large series are binned to `max_pts` points per chart; each bin keeps its mean as the line and
# its minimum and maximum as a pale band, so spikes survive the thinning. Each chart's title carries the range and the share of
# the time span that has data, and every chart takes the colour of what it measures (kind_colours).
# Returns a list of rpt_plot() blocks, one per page.
# ylim: optional limits shared with other reports (parameter, lo, hi; core report_options.R axis_limits()): each chart of a
# parameter listed spans at least lo to hi, so the same parameter reads the same at every site or station.
rpt_overview <- function(x, pal, title = "Parameter overview", per_page = 20L, ncol = 4L, max_pts = 400L, units = NULL, ylim = NULL) {
  x <- data.table::as.data.table(x); if (!nrow(x)) return(NULL)
  x <- x[!is.na(time)]; nt <- data.table::uniqueN(x$time); x <- x[is.finite(value)]; if (!nrow(x)) return(NULL)
  x[, `:=`(group = as.character(group), parameter = as.character(parameter))]
  tr <- range(x$time); is_date <- inherits(x$time, "Date")
  kc <- tryCatch(kind_colours(pal), error = function(e) NULL)                  # one colour per kind of measurement, not shaded apart: the overview shows the kind
  col_of <- function(p) { k <- tryCatch(unname(kc[param_kind(p)]), error = function(e) NULL); if (length(k) && !is.na(k)) k else pal$accent }
  num <- function(v) vapply(v, function(z) if (abs(z) >= 1000) format(round(z), big.mark = ",", trim = TRUE) else format(signif(z, 3), trim = TRUE), "")
  fix <- function(d, p) {                                  # one parameter: bin, find gaps
    d <- d[order(time)]; n <- nrow(d); tn <- as.numeric(d$time)
    b <- if (n > max_pts) ceiling(seq_len(n) * max_pts / n) else seq_len(n)
    d <- d[, .(time = time[ceiling(.N / 2)], value = mean(value), lo = min(value), hi = max(value)), by = b]
    st <- if (nrow(d) > 1) max(stats::median(diff(as.numeric(d$time))), 1e-9) else 1
    d[, seg := cumsum(c(0, diff(as.numeric(time))) > 3 * st)]; d[, parameter := p]; d[] }
  info <- x[, .(group = group[1], n = .N, lo = min(value), hi = max(value)), by = parameter]
  info[, cov := pmin(100, round(100 * n / nt))]
  info[, stat := paste0(num(lo), " to ", num(hi), if (!is.null(units)) { u <- unname(units[parameter]); ifelse(is.na(u) | !nzchar(u), "", paste0(" ", u)) } else "", "  |  ", cov, "% of days")]
  groups <- unique(info$group); pages <- list(); cur <- character()
  for (g in groups) { ps <- info$parameter[info$group == g]
    if (length(cur) && length(cur) + length(ps) > per_page && length(ps) <= per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() }
    for (p in ps) { cur <- c(cur, p); if (length(cur) == per_page) { pages[[length(pages) + 1L]] <- cur; cur <- character() } } }
  if (length(cur)) pages[[length(pages) + 1L]] <- cur
  out <- vector("list", length(pages))
  for (i in seq_along(pages)) { ps <- pages[[i]]; gi <- info[match(ps, parameter)]; multi <- length(unique(gi$group)) > 1
    d <- data.table::rbindlist(lapply(ps, function(p) fix(x[parameter == p], p)))
    l1 <- if (multi) ifelse(nchar(paste0(gi$group, " / ", gi$parameter)) <= 34, paste0(gi$group, " / ", gi$parameter), gi$parameter) else gi$parameter     # the group joins the title only where it fits
    lab <- paste0(l1, "\n", gi$stat); names(lab) <- ps
    d[, panel := factor(lab[parameter], levels = unique(lab))]
    cols <- stats::setNames(vapply(ps, col_of, ""), lab[ps])
    d[, colr := cols[as.character(panel)]]
    yl <- if (!is.null(ylim) && nrow(ylim)) { yl <- data.table::as.data.table(ylim)[parameter %in% ps]
      if (nrow(yl)) data.table::data.table(time = tr[1], value = c(yl$lo, yl$hi), seg = 0, panel = factor(rep(lab[yl$parameter], 2), levels = levels(d$panel))) } else NULL
    p <- ggplot2::ggplot(d, ggplot2::aes(time, value, group = seg)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = panel), alpha = 0.28, colour = NA) +
      ggplot2::geom_line(ggplot2::aes(colour = panel), linewidth = 0.45) +
      ggplot2::geom_point(data = d[, .n := .N, by = .(parameter, seg)][.n == 1L], ggplot2::aes(colour = panel), size = 0.6) +
      (if (!is.null(yl)) ggplot2::geom_blank(data = yl, ggplot2::aes(time, value), inherit.aes = FALSE)) +
      ggplot2::scale_colour_manual(values = cols, guide = "none") + ggplot2::scale_fill_manual(values = cols, guide = "none") +
      ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free_y") +
      (if (is_date) ggplot2::scale_x_date(limits = tr, breaks = scales::breaks_pretty(3), expand = ggplot2::expansion(0.01)) else ggplot2::scale_x_datetime(limits = tr, breaks = scales::breaks_pretty(3), expand = ggplot2::expansion(0.01))) +
      ggplot2::scale_y_continuous(n.breaks = 3) +
      ggplot2::labs(title = paste0(title, if (length(pages) > 1) sprintf("  (%d of %d)", i, length(pages)) else ""),
                    subtitle = paste0(paste(unique(gi$group), collapse = "  |  "), "   -   ", length(ps), " parameters, ", format(tr[1], "%Y-%m-%d"), " to ", format(tr[2], "%Y-%m-%d")), x = NULL, y = NULL) +
      theme_ovid(pal, base_size = 9, legend = "none") +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 6.5, colour = pal$fg4), strip.text = ggplot2::element_text(face = "plain", size = 7, colour = pal$fg, lineheight = 1.15),
                     panel.spacing = grid::unit(7, "pt"), panel.grid.major = ggplot2::element_line(colour = pal$bg1, linewidth = 0.25), plot.title = ggplot2::element_text(size = 12, face = "bold"))
    rows <- ceiling(length(ps) / ncol)
    out[[i]] <- rpt_plot(p, 11, min(8.5, 1.35 + 1.5 * rows), caption = NULL) }
  out
}

# Renders the report in each format asked for; `base` is the path without an extension. Returns the files written.
report_write <- function(doc, base, formats = "html", pal = PAL) {
  out <- character()
  if ("html" %in% formats) { f <- paste0(base, ".html"); render_html(doc, f, pal); out <- c(out, f) }
  if ("pdf" %in% formats)  { f <- paste0(base, ".pdf");  render_pdf(doc, f, pal);  out <- c(out, f) }
  invisible(out)
}

# Years, serials and ids are printed without thousands separators.
RPT_PLAIN <- "(^|_)(year|years|serial|id|run_id|logger)$|^logger_id$|^serial"
rpt_cell <- function(v, name = "") {
  if (is.numeric(v) && grepl(RPT_PLAIN, name, ignore.case = TRUE)) { x <- as.character(v); x[is.na(v)] <- ""; return(x) }
  if (inherits(v, "POSIXct")) return(fmt_ts(v))
  if (inherits(v, "Date")) return(format(v))
  if (is.numeric(v)) return(vapply(seq_along(v), function(i) if (is.na(v[i])) "" else if (abs(v[i]) >= 1e9) format(signif(v[i], 4), scientific = TRUE, trim = TRUE) else if (abs(v[i] - round(v[i])) < 1e-9) format(round(v[i]), big.mark = ",", trim = TRUE, scientific = FALSE) else format(signif(v[i], 4), trim = TRUE), ""))
  x <- as.character(v); x[is.na(x)] <- ""; x
}

# The app's short name for the report header (APP_NAME reads "Ovid Core // CS").
rpt_app <- function() sub("^.*//\\s*", "", APP_NAME)

# ---- HTML -------------------------------------------------------------------------------------------------------------
# One self-contained page in the app's colours and the Ovid type: a header with the brand and a contents strip, cards as one
# joined band, tables that scroll inside a fixed height with their header held (so a 500-row table does not push the page
# away), figures that load as they are scrolled to, and a print style (landscape, light, nothing split across pages).
report_css <- function(pal) {
  bgh <- if (is.null(pal$bg_hard)) mix_col(pal$bg, "#000000", 0.22) else pal$bg_hard
  sprintf("
  :root{--bgh:%s;--bg:%s;--bg1:%s;--bg2:%s;--fg:%s;--fg4:%s;--acc:%s}
  *{box-sizing:border-box} html{scroll-behavior:smooth;scroll-padding-top:56px}
  body{background:var(--bg);color:var(--fg);font:14px/1.5 'Inter','Avenir Next','Segoe UI',-apple-system,Helvetica,Arial,sans-serif;margin:0}
  header{background:var(--bgh);border-bottom:1px solid var(--bg2);padding:20px 24px 14px} .in{max-width:1180px;margin:0 auto}
  .brand{font-weight:600;font-size:13px;letter-spacing:.01em;color:var(--fg)} .brand i{font-style:normal;color:var(--fg4);margin:0 5px} .brand b{color:var(--acc)}
  h1{font-size:26px;line-height:1.2;margin:8px 0 4px;color:var(--fg);letter-spacing:-.01em} .sub{color:var(--fg4);font-size:13.5px}
  nav{position:sticky;top:0;z-index:5;background:var(--bgh);border-bottom:1px solid var(--bg2);overflow-x:auto;white-space:nowrap}
  nav .in{display:flex;gap:4px;padding:6px 24px} nav a{color:var(--fg4);text-decoration:none;font-size:12.5px;padding:4px 10px;border-radius:6px} nav a:hover{background:var(--bg1);color:var(--fg)}
  main{max-width:1180px;margin:0 auto;padding:6px 24px 48px}
  h2{font-size:16px;font-weight:600;color:var(--fg);margin:36px 0 10px;padding-bottom:6px;border-bottom:1px solid var(--bg2)} h2:before{content:'';display:inline-block;width:4px;height:14px;background:var(--acc);border-radius:2px;margin-right:9px;vertical-align:-1px}
  p{max-width:100ch} .dim{color:var(--fg4)}
  .cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:1px;background:var(--bg2);border-radius:8px;overflow:hidden;margin:20px 0}
  .card{background:var(--bg1);padding:12px 16px} .card b{display:block;font-size:22px;font-weight:600;line-height:1.25;font-variant-numeric:tabular-nums;color:var(--fg)} .card span{color:var(--fg4);font-size:12.5px}
  .cap{color:var(--fg4);font-size:12.5px;margin:12px 0 6px} .note{color:var(--fg4);font-size:12px;margin:6px 0 0}
  .tw{overflow:auto;max-height:520px;border:1px solid var(--bg2);border-radius:8px}
  table{border-collapse:separate;border-spacing:0;width:100%%;font-size:12.5px}
  th{position:sticky;top:0;z-index:1;background:var(--bg1);color:var(--fg4);font-weight:600;text-align:left;padding:7px 12px;white-space:nowrap;border-bottom:1px solid var(--bg2)}
  td{padding:5px 12px;border-top:1px solid var(--bg1);font-variant-numeric:tabular-nums;white-space:nowrap} th.n,td.n{text-align:right}
  tbody tr:nth-child(even) td{background:color-mix(in srgb,var(--bg1) 45%%,transparent)} tbody tr:hover td{background:var(--bg1)}
  .fig{margin:14px 0 4px;content-visibility:auto;contain-intrinsic-size:auto 600px} img{display:block;width:100%%;height:auto;border-radius:8px;border:1px solid var(--bg2)}
  footer{color:var(--fg4);font-size:12px;margin-top:44px;padding-top:10px;border-top:1px solid var(--bg2)} code{background:var(--bg2);padding:1px 5px;border-radius:3px}
  @media (max-width:640px){header,main{padding-left:14px;padding-right:14px} h1{font-size:21px}}
  @page{size:landscape;margin:12mm}
  @media print{:root{--bgh:#fff;--bg:#fff;--bg1:#f1f1ee;--bg2:#d8d8d4;--fg:#1b1c1e;--fg4:#555;--acc:#1b1c1e} body{font-size:11px} nav{display:none} .tw{max-height:none;overflow:visible}
    th{position:static} h2{break-after:avoid} img,.card,tr,.cap{break-inside:avoid} .fig{content-visibility:visible} *{-webkit-print-color-adjust:exact;print-color-adjust:exact}}",
  bgh, pal$bg, pal$bg1, pal$bg2, pal$fg, pal$fg4, pal$accent) |> (function(css) paste0(ovid_font_css(embed = c("400", "600")), css))()
}

html_table <- function(dt, max_rows = 200) {
  if (is.null(dt) || !nrow(dt)) return("<p class='dim'>none</p>")
  more <- nrow(dt) - max_rows; d <- head(dt, max_rows)
  num <- vapply(names(d), function(n) is.numeric(d[[n]]) && !grepl(RPT_PLAIN, n, ignore.case = TRUE), NA)
  cells <- matrix(vapply(names(d), function(n) html_esc(rpt_cell(d[[n]], n)), character(nrow(d))), nrow = nrow(d))
  open <- ifelse(num, "<td class='n'>", "<td>")
  rows <- apply(cells, 1, function(r) paste0("<tr>", paste0(open, r, "</td>", collapse = ""), "</tr>"))
  paste0("<div class='tw'><table><thead><tr>", paste0(ifelse(num, "<th class='n'>", "<th>"), html_esc(names(d)), "</th>", collapse = ""),
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>",
         if (more > 0) sprintf("<p class='note'>Showing the first %s of %s rows; the rest are in the CSV.</p>", format(max_rows, big.mark = ","), format(nrow(dt), big.mark = ",")) else "")
}

# Figures are embedded as PNG at 110 dpi, so one tall figure cannot make the file huge (height is capped at 45 in).
plot_png_uri <- function(p, w, h, pal) {
  f <- tempfile(fileext = ".png"); on.exit(unlink(f))
  ok <- tryCatch({ ggplot2::ggsave(f, p, width = w, height = min(h, 45), dpi = 110, bg = pal$bg, limitsize = FALSE); TRUE }, error = function(e) FALSE)
  if (!ok || !file.exists(f)) return(NULL)
  paste0("data:image/png;base64,", base64enc::base64encode(f))
}

render_html <- function(doc, file, pal = PAL) {
  heads <- Filter(function(b) b$type == "heading", doc$blocks); ids <- paste0("s", seq_along(heads)); hi <- 0L
  body <- vapply(doc$blocks, function(b) switch(b$type,
    heading = { hi <<- hi + 1L; paste0("<h2 id='", ids[hi], "'>", html_esc(b$text), "</h2>") },
    text    = paste0("<p>", html_esc(b$text), "</p>", collapse = ""),
    cards   = paste0("<div class='cards'>", paste0("<div class='card'><b>", html_esc(rpt_cell(unname(b$values))), "</b><span>", html_esc(names(b$values)), "</span></div>", collapse = ""), "</div>"),
    table   = paste0(if (!is.null(b$caption)) paste0("<p class='cap'>", html_esc(b$caption), "</p>"), html_table(b$data, b$max_rows)),
    plot    = { u <- plot_png_uri(b$plot, b$w, b$h, pal)
                paste0("<div class='fig'>", if (is.null(u)) "<p class='dim'>(figure could not be drawn)</p>" else paste0("<img loading='lazy' decoding='async' alt='", html_esc(if (is.null(b$caption)) "figure" else b$caption), "' src='", u, "'>"),
                       if (!is.null(b$caption)) paste0("<p class='note'>", html_esc(b$caption), "</p>"), "</div>") },
    ""), "")
  html <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
            "<title>", html_esc(doc$title), "</title><style>", report_css(pal), "</style></head><body>",
            "<header><div class='in'><div class='brand'>ovid<i>//</i><b>", html_esc(rpt_app()), "</b></div><h1>", html_esc(doc$title), "</h1>",
            if (nzchar(doc$subtitle)) paste0("<div class='sub'>", html_esc(doc$subtitle), "</div>"), "</div></header>",
            if (length(heads) >= 3) paste0("<nav><div class='in'>", paste0("<a href='#", ids, "'>", html_esc(vapply(heads, `[[`, "", "text")), "</a>", collapse = ""), "</div></nav>"),
            "<main>", body, "<footer>", html_esc(paste0("Made ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by ", APP_NAME, " ", APP_VERSION, " (core ", CORE_VERSION, ")")), "</footer>",
            "</main></body></html>")
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(file)
}

# ---- PDF --------------------------------------------------------------------------------------------------------------
# Letter landscape. Text and tables flow down the page and break onto a new one; each figure has a page to itself.
# Cairo's PDF device draws any character (minus signs, degree and micro signs, subscripts in plot labels, Dene and other
# non-Latin names). A Mac without XQuartz has no Cairo: its own Quartz PDF device draws them too. Elsewhere without Cairo the
# plain pdf() device is used and text is reduced to Latin-1.
PDF_CAIRO <- isTRUE(capabilities("cairo")) &&                # capabilities() can say yes while cairo.so fails to load (a Mac without XQuartz)
  tryCatch({ f <- tempfile(fileext = ".pdf"); on.exit(unlink(f)); suppressWarnings(grDevices::cairo_pdf(f)); grDevices::dev.off(); file.exists(f) }, error = function(e) FALSE)   # opens one to be sure
PDF_QUARTZ <- !PDF_CAIRO && identical(Sys.info()[["sysname"]], "Darwin") &&
  tryCatch({ f <- tempfile(fileext = ".pdf"); on.exit(unlink(f)); grDevices::quartz(type = "pdf", file = f); grDevices::dev.off(); file.exists(f) }, error = function(e) FALSE)
PDF_UNICODE <- PDF_CAIRO || PDF_QUARTZ
pdf_str <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; if (PDF_UNICODE) return(enc2utf8(x))
  y <- iconv(x, "UTF-8", "latin1", sub = "?"); y[is.na(y)] <- gsub("[^ -~]", "?", x[is.na(y)]); y }

render_pdf <- function(doc, file, pal = PAL) {
  if (PDF_CAIRO) grDevices::cairo_pdf(file, width = 11, height = 8.5, bg = pal$bg, onefile = TRUE)
  else if (PDF_QUARTZ) grDevices::quartz(type = "pdf", file = file, width = 11, height = 8.5, bg = pal$bg, title = pdf_str(doc$title))
  else grDevices::pdf(file, width = 11, height = 8.5, bg = pal$bg, encoding = "ISOLatin1", title = pdf_str(doc$title))
  on.exit(grDevices::dev.off(), add = TRUE)
  mono <- if (PDF_UNICODE) "mono" else "Courier"
  st <- new.env(); st$y <- 0; st$page <- 0L
  gp <- function(...) grid::gpar(...)
  foot <- function() { grid::grid.lines(c(0.06, 0.94), 0.062, gp = gp(col = pal$bg2, lwd = 0.8))
    grid::grid.text(pdf_str(paste0("ovid // ", rpt_app())), 0.06, 0.035, just = "left", gp = gp(col = pal$fg4, fontsize = 8))
    grid::grid.text(pdf_str(paste0(doc$title, "   |   page ", st$page)), 0.94, 0.035, just = "right", gp = gp(col = pal$fg4, fontsize = 8)) }
  new_page <- function() { grid::grid.newpage(); st$page <- st$page + 1L; st$y <- 0.93; foot() }
  need <- function(h) if (st$page == 0L || st$y - h < 0.07) new_page()
  MONO <- mono
  line <- function(txt, size, col, h, mono = FALSE, bold = FALSE) { need(h)
    grid::grid.text(pdf_str(txt), 0.06, st$y, just = c("left", "top"), gp = gp(col = col, fontsize = size, fontfamily = if (mono) MONO else "", fontface = if (bold) "bold" else "plain"))
    st$y <- st$y - h }
  new_page()
  grid::grid.rect(0.5, 1, 1, 0.15, just = c("centre", "top"), gp = gp(fill = mix_col(pal$bg, "#000000", 0.22), col = NA))        # header band
  grid::grid.rect(0.5, 0.85, 1, 0.004, just = c("centre", "top"), gp = gp(fill = pal$accent, col = NA))
  grid::grid.text(pdf_str(paste0("ovid // ", rpt_app())), 0.06, 0.975, just = c("left", "top"), gp = gp(col = pal$fg4, fontsize = 9))
  st$y <- 0.95; line(doc$title, 22, pal$fg, 0.055, bold = TRUE)
  if (nzchar(doc$subtitle)) for (s in strwrap(doc$subtitle, 140)) line(s, 10, pal$fg4, 0.024)
  st$y <- 0.82
  # A heading or note that leads straight into a figure goes on the figure's page (above it) instead of stranding on a page of its own.
  blk <- doc$blocks; nb <- length(blk); lead <- rep(FALSE, nb); st$pend <- list()
  for (i in rev(seq_len(nb))) lead[i] <- blk[[i]]$type %in% c("heading", "text") && i < nb && ((blk[[i + 1L]]$type == "plot" && !isTRUE(blk[[i + 1L]]$inline)) || (blk[[i + 1L]]$type == "text" && lead[i + 1L]))
  for (bi in seq_len(nb)) { b <- blk[[bi]]; if (lead[bi]) { st$pend[[length(st$pend) + 1L]] <- b; next }; switch(b$type,
    heading = { nxt <- if (bi < nb) blk[[bi + 1L]]; need(if (!is.null(nxt) && nxt$type == "plot" && isTRUE(nxt$inline)) 0.1 + min(nxt$h / 8.5, 0.32) else 0.14); st$y <- st$y - 0.02; line(b$text, 14, pal$fg, 0.04, bold = TRUE)
      grid::grid.lines(c(0.06, 0.94), st$y + 0.004, gp = gp(col = pal$accent, lwd = 1)); st$y <- st$y - 0.014 },
    text    = { for (p in b$text) { for (s in strwrap(p, 150)) line(s, 10.5, pal$fg, 0.024); st$y <- st$y - 0.008 } },
    cards   = { v <- b$values; per <- 6
      for (ch in split(seq_along(v), ceiling(seq_along(v) / per))) { need(0.1)
        for (k in seq_along(ch)) { x0 <- 0.06 + (k - 1) * 0.148
          grid::grid.rect(x0, st$y, 0.138, 0.085, just = c("left", "top"), gp = gp(fill = pal$bg1, col = NA))
          grid::grid.text(pdf_str(rpt_cell(unname(v[ch[k]]))), x0 + 0.01, st$y - 0.012, just = c("left", "top"), gp = gp(col = pal$fg, fontsize = 16, fontface = "bold"))
          grid::grid.text(pdf_str(names(v)[ch[k]]), x0 + 0.01, st$y - 0.058, just = c("left", "top"), gp = gp(col = pal$fg4, fontsize = 8.5)) }
        st$y <- st$y - 0.1 } },
    table   = { d <- b$data
      if (!is.null(b$caption)) for (s in strwrap(b$caption, 150)) line(s, 9.5, pal$fg4, 0.022)
      if (!nrow(d)) { line("none", 10, pal$fg4, 0.024); next }
      more <- nrow(d) - b$max_rows; d <- head(d, b$max_rows)
      cells <- lapply(names(d), function(n) pdf_str(rpt_cell(d[[n]], n)))
      w <- pmin(b$col_width %||% 28L, pmax(nchar(pdf_str(names(d))), vapply(cells, function(x) max(0L, nchar(x)), 0L)))
      keep <- which(cumsum(w + 2L) <= 148L); dropped <- ncol(d) - length(keep)
      fmt <- function(vals) paste(vapply(seq_along(keep), function(k) { s <- vals[k]; s <- if (nchar(s) > w[keep[k]]) paste0(substr(s, 1, w[keep[k]] - 1), "~") else s; formatC(s, width = -w[keep[k]]) }, ""), collapse = "  ")
      hdr <- fmt(pdf_str(names(d))[keep]); rows <- vapply(seq_len(nrow(d)), function(i) fmt(vapply(keep, function(j) cells[[j]][i], "")), "")
      rh <- 0.0185; i <- 1L                                    # one row = rh of the page height; each row is placed at its own y, so stripes and text always agree
      while (i <= length(rows)) { need(rh * 4)
        grid::grid.rect(0.06, st$y + 0.003, 0.88, rh + 0.004, just = c("left", "top"), gp = gp(fill = pal$bg1, col = NA))
        line(hdr, 8.5, pal$accent, rh + 0.006, mono = TRUE, bold = TRUE)
        n_fit <- max(1L, floor((st$y - 0.075) / rh)); j <- min(length(rows), i + n_fit - 1L); y <- st$y - (seq_len(j - i + 1L) - 1L) * rh
        odd <- which((seq(i, j) %% 2L) == 0L); if (length(odd)) grid::grid.rect(0.06, y[odd] + 0.002, 0.88, rh, just = c("left", "top"), gp = gp(fill = pal$bg1, col = NA, alpha = 0.55))
        grid::grid.text(rows[i:j], 0.06, y, just = c("left", "top"), gp = gp(col = pal$fg, fontsize = 8.5, fontfamily = mono))
        st$y <- st$y - rh * (j - i + 1L); i <- j + 1L }
      notes <- c(if (more > 0) paste(format(more, big.mark = ","), "more rows in the CSV"), if (dropped > 0) paste(dropped, "more columns in the CSV"))
      if (length(notes)) line(paste(notes, collapse = "; "), 8.5, pal$fg4, 0.022)
      st$y <- st$y - 0.01 },
    plot    = if (isTRUE(b$inline)) { hh <- b$h / 8.5; avail <- st$y - 0.115; if (st$page > 0L && hh > avail && avail >= 0.26) hh <- avail      # shrink to the room left rather than start a page
      need(hh + 0.02)
      ok <- tryCatch({ print(b$plot, vp = grid::viewport(0.5, st$y - hh / 2, 0.88, hh)); TRUE }, error = function(e) FALSE)
      if (!ok) line("(figure could not be drawn)", 9, pal$fg4, 0.03)
      st$y <- st$y - hh - 0.012
      if (!is.null(b$caption)) line(b$caption, 8.5, pal$fg4, 0.022) } else { st$page <- st$page + 1L; grid::grid.newpage()
      top <- 1
      if (length(st$pend)) { y <- 0.95
        for (q in st$pend) if (q$type == "heading") { grid::grid.text(pdf_str(q$text), 0.06, y, just = c("left", "top"), gp = gp(col = pal$fg, fontsize = 14, fontface = "bold")); y <- y - 0.04
            grid::grid.lines(c(0.06, 0.94), y + 0.004, gp = gp(col = pal$accent, lwd = 1)); y <- y - 0.014 }
          else for (p in q$text) { for (s in strwrap(p, 150)) { grid::grid.text(pdf_str(s), 0.06, y, just = c("left", "top"), gp = gp(col = pal$fg, fontsize = 10.5)); y <- y - 0.024 }; y <- y - 0.008 }
        top <- y; st$pend <- list() }
      ok <- tryCatch({ print(b$plot, vp = grid::viewport(0.5, (0.07 + top) / 2, 1, top - 0.07)); TRUE }, error = function(e) FALSE)
      if (!ok) { grid::grid.newpage(); grid::grid.text("(figure could not be drawn)", gp = gp(col = pal$fg4)) }
      foot()
      if (!is.null(b$caption)) grid::grid.text(pdf_str(b$caption), 0.06, 0.075, just = c("left", "bottom"), gp = gp(col = pal$fg4, fontsize = 8))
      st$y <- 0 } ) }
  invisible(file)
}
