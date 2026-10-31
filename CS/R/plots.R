# R/plots.R  --  The colour theme, plots and the HTML report.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# olive-crt (vimcolorschemes, dark; see Ovid/theme.R): an olive-black CRT ground, pale phosphor text and soft pastels
# for data; the accent is its signature olive green. The roles are the theme's own (its Neovim palette): the title bar and
# rail take its status-line ground, pages its editor ground, cards its side-panel ground, dividers its selection colour and
# pane edges its border. (The list keeps its old name, GB.)
# Settings > Colours offers these schemes (M, 2026-10-08; OVID_ALT_SCHEMES in Ovid/theme.R); the first is CS's own,
# and in the others the accent is the scheme's green. APP_SCHEME is the one picked (app_settings.json), read at the start.
APP_SCHEMES <- c("Olive CRT", "Everforest", "Werks", "The Unix Zoo")
APP_SCHEME <- saved_scheme(APP_SCHEMES)
app_palette <- function(scheme = APP_SCHEMES[1]) {
  p <- scheme_pick(olive_crt_palette(), scheme, "green", APP_SCHEMES)            # Ovid/theme.R; green = ok (teal), as in the design system
  legible(p, c("gray", "fg4", "red", "green", "yellow", "blue", "purple", "aqua", "orange", "accent"), 4.5, on = c(p$bg, p$bg1)) }     # readable on the cards (Ovid/theme.R)
GB <- app_palette(APP_SCHEME)
PAL <- GB                                            # console colours (say() in the shared core)

GB_PAL <- unname(unlist(GB[c("blue", "yellow", "red", "green", "purple", "orange", "accent", "aqua")]))

theme_gruvbox <- function(base_size = 13) theme_ovid(GB, base_size, border = GB$bg1)     # old name, kept for the code that calls it
scale_colour_gruvbox <- function(...) scale_colour_ovid(GB_PAL, ...)
scale_fill_gruvbox   <- function(...) scale_fill_ovid(GB_PAL, ...)

theme_set(theme_gruvbox())

update_geom_defaults("line",  list(colour = GB$blue, linewidth = 0.35))

update_geom_defaults("point", list(colour = GB$blue))

update_geom_defaults("col",   list(fill = GB$blue))

# A plot that fails must never stop a run.
gb_save <- function(p, file, w = 11, h = 6) tryCatch({
  suppressMessages(suppressWarnings(ggsave(file, p, width = w, height = min(h, 45), dpi = 110, bg = GB$bg, limitsize = FALSE))); file },
  error = function(e) { problem(basename(file), paste("plot not written:", conditionMessage(e))); character() })

# groups: optional named vector (parameter -> group). When given, parameters of
# the same group are overlaid in one panel instead of one panel each.
# units: optional named vector (parameter -> unit) shown beside each panel title.
# bands: optional data.table(parameter, lo, hi); the part of each panel inside the usual range is tinted.
# style: "auto" (totals as bars, wind direction as points, everything else a line), or one of
# line, point, linepoint, step, bar, area for every parameter.
plot_style_of <- function(params, style = "auto") {
  if (!identical(style, "auto") && length(style) == 1) return(setNames(rep(style, length(params)), params))
  setNames(ifelse(grepl(TOTAL_RX, params, ignore.case = TRUE), "bar", ifelse(grepl("wind_?dir|wnd_dir", params, ignore.case = TRUE) & !grepl("sd|std", params, ignore.case = TRUE), "point", "line")), params)
}

plot_station <- function(dt, params = NULL, max_params = CFG$plot_max_params, removed = NULL, groups = NULL, units = NULL, bands = NULL, style = "auto") {
  lab <- function(p) { u <- if (is.null(units)) rep(NA_character_, length(p)) else unname(units[p])
    ifelse(!is.na(u) & nzchar(u), paste0(p, "   (", u, ")"), p) }
  vals <- setdiff(names(dt), META_COLS)
  vals <- vals[vapply(vals, function(v) is.numeric(dt[[v]]) && any(!is.na(dt[[v]])), logical(1))]
  params <- if (is.null(params)) head(vals, max_params) else intersect(params, vals)
  if (!length(params)) return(NULL)
  d <- copy(dt); if (!"stream" %in% names(d)) d[, stream := ""]
  long <- melt(d[, c("TIMESTAMP", "stream", params), with = FALSE], id.vars = c("TIMESTAMP", "stream"), variable.name = "parameter", na.rm = TRUE)
  setorder(long, parameter, TIMESTAMP)
  # one continuous line per parameter; the line breaks only across a real gap (three intervals of the table those records came from)
  long[, .step := if (.N > 1) max(stats::median(diff(as.numeric(TIMESTAMP))), 1) else 1, by = .(parameter, stream)]
  long[, .seg := { g <- c(0, diff(as.numeric(TIMESTAMP))); cumsum(g > 1.5 * pmax(.step, shift(.step, fill = .step[1]))) }, by = parameter]
  long[, stream := NULL]; long[, .n := .N, by = .(parameter, .seg)]
  long[, .style := plot_style_of(params, style)[as.character(parameter)]]
  if (nrow(long) > CFG$plot_max_points * length(params)) long <- long[, .SD[unique(round(seq(1, .N, length.out = CFG$plot_max_points)))], by = parameter]
  grouped <- !is.null(groups)
  long[, panel := if (grouped) factor(groups[as.character(parameter)], levels = unique(groups[params]))
                  else factor(lab(as.character(parameter)), levels = unique(lab(params)))]
  sub <- function(st) long[.style %in% st]; few <- nrow(long) <= 400
  pc <- param_colours(params, GB)                  # each parameter in the colour of what it measures (Ovid/theme.R)
  p <- ggplot(long, aes(TIMESTAMP, value, colour = parameter, group = interaction(parameter, .seg)))
  if (nrow(sub("area"))) p <- p + geom_ribbon(data = sub("area"), aes(ymin = pmin(value, 0), ymax = pmax(value, 0), fill = parameter), colour = NA, alpha = 0.3) + scale_fill_manual(values = pc, guide = "none")
  if (nrow(sub("bar"))) p <- p + (if (few) geom_col(data = sub("bar"), aes(fill = parameter), colour = NA, width = sub("bar")$.step[1] * 0.85) else geom_linerange(data = sub("bar"), aes(ymin = 0, ymax = value), linewidth = 0.35)) +
      (if (few && !nrow(sub("area"))) scale_fill_manual(values = pc, guide = "none") else NULL)
  if (nrow(sub("step"))) p <- p + geom_step(data = sub("step"), linewidth = 0.4)
  ln <- sub(c("line", "linepoint", "area"))
  if (nrow(ln)) p <- p + geom_line(data = ln[.n > 1], linewidth = 0.4) + geom_point(data = ln[.n == 1], size = 0.45)      # a lone record between two gaps is a dot
  pt <- if (few) sub(c("point", "linepoint", "line", "step")) else sub(c("point", "linepoint"))
  if (nrow(pt)) p <- p + geom_point(data = pt, size = if (few) 1 else 0.45, stroke = 0, shape = 16)
  p <- p +
    facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "top") +
    (if (grouped) scale_colour_manual(values = pc, name = NULL) else scale_colour_manual(values = pc, guide = "none")) +
    labs(title = paste(dt$station[1], "-", dt$stream[1]), x = NULL, y = NULL,
         subtitle = paste0(format(nrow(dt), big.mark = ","), " records, ",
                           format(min(dt$TIMESTAMP), "%Y-%m-%d"), " to ", format(max(dt$TIMESTAMP), "%Y-%m-%d"),
                           "  |  loggers: ", paste(unique(dt$logger_id), collapse = ", ")),
         caption = if (!is.null(removed) && nrow(removed)) "red ticks = values removed by QC (see audit log)" else NULL)
  if (!grouped && !is.null(bands) && nrow(bands)) {
    rg <- long[, .(dlo = min(value), dhi = max(value)), by = .(parameter, panel)]
    b <- merge(rg, bands, by = "parameter")[!is.na(lo) & !is.na(hi)]
    b <- b[dlo < lo | dhi > hi]                          # only where something falls outside
    if (nrow(b)) { b[, `:=`(ymin = pmax(lo, dlo), ymax = pmin(hi, dhi))]; b <- b[ymax > ymin]
      inf <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
      if (nrow(b)) p$layers <- c(list(geom_rect(data = b, aes(xmin = inf(-Inf), xmax = inf(Inf), ymin = ymin, ymax = ymax),
                                                inherit.aes = FALSE, fill = GB$green, alpha = 0.10)), p$layers) }
  }
  if (!is.null(removed) && nrow(removed)) {
    r <- removed[column %in% params, .(TIMESTAMP = parse_time(timestamp), parameter = factor(column, levels = params))]
    r[, panel := if (grouped) factor(groups[as.character(parameter)], levels = unique(groups[params]))
                 else factor(lab(as.character(parameter)), levels = unique(lab(params)))]
    if (nrow(r)) p <- p + geom_rug(data = r, aes(x = TIMESTAMP), inherit.aes = FALSE, sides = "b",
                                   colour = GB$red, alpha = 0.6, length = unit(0.06, "npc"))
  }
  p
}

# One row per logger (all its tables together), one bar per source file.
plot_timeline <- function(man) {
  d <- man[status == "ok" & !is.na(first_ts)]
  d[, grp := logger_id]
  setorder(d, grp, first_ts)
  d[, piece := factor((seq_len(.N) - 1L) %% 2L), by = grp]
  d[, grp := factor(grp, levels = rev(unique(d[order(first_ts), grp])))]
  ggplot(d) + geom_segment(aes(x = first_ts, xend = last_ts, y = grp, yend = grp, colour = piece),
                           linewidth = 2.6, alpha = 0.85) +
    geom_point(aes(first_ts, grp), colour = GB$fg, size = 0.5) +
    scale_colour_manual(values = c(GB$blue, GB$yellow), guide = "none") +
    labs(title = "Stitching: which files cover which period",
         subtitle = "One row per logger, one bar per source file, alternating colour; dots mark where a file starts",
         x = NULL, y = NULL)
}

plot_audit <- function(aud) {
  s <- aud[, .(n = sum(n, na.rm = TRUE)), by = .(rule, stage)][order(n)]
  s[, rule := factor(rule, levels = unique(rule))]
  ggplot(s, aes(n, rule, fill = stage)) + geom_col(width = 0.7) +
    geom_text(aes(label = format(n, big.mark = ",")), hjust = -0.15, colour = GB$fg4, size = 3.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) + scale_fill_gruvbox() +
    labs(title = "Audit: what was removed and why", x = "Records or values", y = NULL, fill = NULL)
}

write_report <- function(out, cfg, man, summ, aud_summ, gaps, dict, plots) {
  css <- sprintf("
  body{background:%s;color:%s;font:14px/1.5 -apple-system,'Segoe UI',Helvetica,Arial,sans-serif;margin:0;padding:28px 16px}
  main{max-width:1100px;margin:0 auto} h1{font-size:22px;margin:0 0 4px} h2{font-size:16px;color:%s;margin:34px 0 10px;border-bottom:1px solid %s;padding-bottom:6px}
  .dim{color:%s} .cards{display:flex;flex-wrap:wrap;gap:10px;margin:18px 0}
  .card{background:%s;border-radius:6px;padding:10px 14px;min-width:120px} .card b{display:block;font-size:20px;color:%s;font-variant-numeric:tabular-nums}
  .card span{color:%s;font-size:12px} .tw{overflow-x:auto;border:1px solid %s;border-radius:6px}
  table{border-collapse:collapse;width:100%%;font-size:12.5px} th{background:%s;color:%s;text-align:left;padding:6px 10px;white-space:nowrap}
  td{padding:5px 10px;border-top:1px solid %s;font-variant-numeric:tabular-nums;white-space:nowrap} tr:hover td{background:%s}
  img{max-width:100%%;border-radius:6px;margin:8px 0;border:1px solid %s} code{background:%s;padding:1px 5px;border-radius:3px;color:%s}",
    GB$bg, GB$fg, GB$yellow, GB$bg2, GB$fg4, GB$bg1, GB$fg, GB$fg4, GB$bg2, GB$bg1, GB$fg4, GB$bg1, GB$bg_hard, GB$bg2, GB$bg1, GB$aqua)
  card <- function(n, lab) sprintf("<div class='card'><b>%s</b><span>%s</span></div>", format(n, big.mark = ","), lab)
  skipped <- man[status != "ok", .(files = .N), by = status][order(-files)]
  unmapped <- dict[parameter == raw_column, .(loggers = uniqueN(logger_id)), by = .(raw_column, unit)][order(-loggers)]
  img <- function(f) sprintf("<img loading='lazy' src='plots/%s' alt='%s'>", basename(f), basename(f))
  html <- c("<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>Ovid Core // CS report</title><style>", css, "</style></head><body><main>",
    "<h1>Campbell Scientific pipeline</h1>",
    sprintf("<div class='dim'>run %s &nbsp;|&nbsp; %s</div>", AUDIT$run_id, html_esc(paste(cfg$raw_dirs, collapse = ", "))),
    "<div class='cards'>", card(nrow(man), "files found"), card(sum(man$status == "ok"), "files stitched"),
    card(uniqueN(man[status == "ok", logger_id]), "loggers"), card(uniqueN(summ$station), "stations"),
    card(sum(summ$records), "clean records"), card(sum(aud_summ[level == "cell", n]), "values removed"),
    card(sum(aud_summ[level == "row", n]), "records dropped / merged"), "</div>",
    "<h2>Stations</h2>", html_table(summ),
    "<h2>Audit summary</h2><p class='dim'>Every line of detail is in <code>audit/detail/</code>, one file per logger and per station.</p>",
    if (length(plots$audit)) img(plots$audit), html_table(aud_summ),
    "<h2>Stitching</h2>", if (length(plots$timeline)) img(plots$timeline),
    "<h2>Gaps in the stitched record</h2>", html_table(gaps[order(-gap_hours)], 40),
    "<h2>Files not ingested</h2>", html_table(skipped),
    "<h2>Raw columns with no alias yet</h2><p class='dim'>Add a row to <code>config/aliases.csv</code> to map any of these. Full list in <code>audit/column_dictionary.csv</code>.</p>",
    html_table(unmapped, 60),
    "<h2>Station plots</h2>", vapply(plots$stations, img, ""), "</main></body></html>")
  writeLines(html, file.path(out, "report.html"))
}

