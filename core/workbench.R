# core/workbench.R  --  The Workbench interface (preview): an IDE-style layout with the record charted over a grid.
# Part of Ovid Core (shared). Sourced by an app's workbench/ files; the Standard interface (shell.R) does not use it.
#
# Layout: a title bar (app icon, app name, project, the interface switch, Import / Run / Export), a tree on the left,
# tabs with the chart over the grid in the middle, an inspector on the right and a status bar. Colours come from the
# app's palette through ovid_tokens() (shell.R), so each app keeps its own theme. Dark only.
# An app can show its Standard pages inside the Workbench: they go in the same hidden tab set ("nav") as in the Standard
# shell, so ovid_go() and data-ovid-go links work there too, and the shell's CSS and script go in `head`.
# wb_grid() is the spreadsheet: click, drag, shift-click and cmd/ctrl-click select cells; a row header selects the
# record, a column header the parameter, the corner everything; arrows move, shift-arrows extend, Esc clears.
# The data view charts the grid's records over time (wb_chart_ui) or, for records that are not a time series, as points
# at x and y coloured by a value (wb_xy_ui: an ERT pseudosection, image positions); either highlights the grid's selection
# and selects in the grid.
#
# The two interfaces run in one app process on one project: ?ui=workbench in the address opens this one, ?ui=standard
# the Standard one; with neither, OVID_UI and then the app's "ui" setting decide (Standard by default).

OVID_WORKBENCH_VERSION <- "0.9.0"

wb_query <- function(query) if (is.null(query) || !length(query) || !nzchar(query)) list() else shiny::parseQueryString(query)

# TRUE when a request (its query string) should get the Workbench.
wb_wanted <- function(query, setting = NULL) {
  q <- wb_query(query)
  if (identical(q$ui, "workbench")) return(TRUE)
  if (identical(q$ui, "standard")) return(FALSE)
  env <- Sys.getenv("OVID_UI")
  if (nzchar(env)) return(identical(env, "workbench"))
  identical(setting, "workbench")
}

WB_TITLE_FONT <- "'IBM Plex Sans',Inter,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif"   # the interface (core/fonts, else a system sans)
WB_MONO_FONT <- "'IBM Plex Mono','JetBrains Mono',ui-monospace,SFMono-Regular,Menlo,Consolas,monospace"   # numbers, ids and dates
WB_FONT_FILES <- list(list("IBM Plex Sans", "400 600", "IBMPlexSans.woff2"), list("IBM Plex Mono", "400", "IBMPlexMono-Regular.woff2"),
                      list("IBM Plex Mono", "500", "IBMPlexMono-Medium.woff2"))
# @font-face rules for the Workbench fonts in core/fonts (served at ovid-fonts/, as ovid_font_css() does for Inter).
wb_font_css <- function() {
  d <- ovid_font_dir(); if (is.null(d)) return("")
  paste0(vapply(Filter(function(f) file.exists(file.path(d, f[[3]])), WB_FONT_FILES), function(f)
    sprintf("@font-face{font-family:'%s';font-style:normal;font-weight:%s;font-display:swap;src:url(ovid-fonts/%s) format('woff2')}", f[[1]], f[[2]], f[[3]]), ""), collapse = "\n")
}

# CSS variables from the app palette, then the layout rules.
wb_css <- function(pal) {
  tk <- ovid_tokens(pal); g <- function(k, d) if (!is.null(pal[[k]])) pal[[k]] else d
  v <- c(bar = tk$bgh, bg = tk$bg, panel = tk$bg1, sel = tk$bg2, line = g("line", tk$bg3), float = g("bg_float", tk$bg1),
         fg = tk$fg, fg2 = tk$fg4, fg3 = wb_label_col(tk$gray, tk$fg, c(tk$bg1, tk$bg)), acc = tk$acc, acc2 = tk$acc2, onacc = tk$onacc,
         ok = tk$green, warn = tk$yellow, err = tk$red, info = tk$blue,
         warnbg = mix_col(g("warn_bg", mix_col(tk$yellow, tk$bg, 0.75)), tk$bg, 0.7), errbg = mix_col(g("error_bg", mix_col(tk$red, tk$bg, 0.75)), tk$bg, 0.2),
         chipwarn = g("warn_bg", mix_col(tk$yellow, tk$bg, 0.75)))
  paste0(":root{", paste0("--wb-", names(v), ":", v, ";", collapse = ""), "--wb-title:", WB_TITLE_FONT, ";--wb-mono:", WB_MONO_FONT, "}\n", WB_CSS)
}

# A label colour that reads at min:1 on every background in on: col mixed toward fg in steps until it does.
wb_label_col <- function(col, fg, on, min = 4.5) {
  for (i in 0:10) { x <- if (i) mix_col(col, fg, 1 - i / 10) else col; if (all(vapply(on, function(b) contrast_ratio(x, b), 0) >= min)) return(x) }
  fg
}

WB_CSS <- paste(readLines(file.path(CORE_DIR, "www", "workbench.css"), encoding = "UTF-8"), collapse = "\n")

# A whole Workbench page. nav, main and aside are the three panes; actions sit at the right of the title bar; status is the
# status bar. standard_href opens the Standard interface (the left half of the switch). theme: the app's bslib theme
# (Bootstrap 5 apps: GS, RS), so their own pages work inside the Workbench; NULL for plain Shiny.
wb_page <- function(app, pal, project = NULL, standard_href = "?ui=standard", actions = NULL, nav = NULL, main = NULL, aside = NULL,
                    status = NULL, head = NULL, theme = NULL, areas = NULL) {
  ic <- ovid_icon(app)
  if (!is.null(ovid_font_dir())) shiny::addResourcePath("ovid-fonts", ovid_font_dir())
  shiny::bootstrapPage(title = paste0("ovid // ", app, " \u00b7 workbench"), theme = theme,
    tags$head(if (!is.null(ic)) tags$link(rel = "icon", type = "image/png", href = ic), head,      # head first: these rules win
              tags$style(HTML(paste0(ovid_font_css(), "\n", wb_font_css(), "\n", wb_css(pal), "\n", get0("SHEET_CSS", ifnotfound = ""), "\n", get0("OVID_PARTS_CSS", ifnotfound = ""), "\n", get0("WB_NEEDS_CSS", ifnotfound = ""), "\n", get0("WB_QUEUE_CSS", ifnotfound = "")))), tags$script(HTML(WB_JS)), tags$script(HTML(get0("SHEET_JS", ifnotfound = ""))), tags$script(HTML(WB_CHART_JS)), tags$script(HTML(WB_XY_JS))),
    div(class = "wb",
      div(class = "wb-top",
        div(class = "wb-brand", if (!is.null(ic)) tags$img(src = ic, alt = ""), span("ovid"), span(class = "sep", "//"), span(class = "app", app)),
        ovid_areas_ui(areas, class = "wb-switch"),                                                  # sides of the app, as in the Standard bar (shell.R)
        if (!is.null(project)) span(class = "wb-project", project),
        div(class = "wb-switch", role = "group", `aria-label` = "Interface",
            tags$a(href = standard_href, id = "wb_to_standard", title = "Open the Standard interface on this project", "Standard"),
            span(class = "on", "Workbench", span(class = "wb-pv", "preview"))),
        div(class = "wb-fill"), actions),
      div(class = "wb-body",                                                  # side panels: fold, resize, float (www/workbench.js wbPanels)
        wb_strip("nav", "Tree", "›"),
        tags$nav(class = "wb-nav", id = "wb_nav", `aria-label` = "Tree", wb_panel_head("nav", "Tree", "‹"), div(class = "wb-pc", nav)),
        wb_grip("nav", "tree"),
        tags$main(class = "wb-main", main),
        wb_grip("aside", "inspector"),
        tags$aside(class = "wb-aside", id = "wb_aside", `aria-label` = "Inspector",
          wb_panel_head("aside", "Inspector", "›", tags$button(type = "button", class = "wb-pbtn", `data-wb-float` = "1", `aria-label` = "Float the inspector over the page",
                                                                    title = "Float over the page; again to dock", "⧉")), div(class = "wb-pc", aside)),
        wb_strip("aside", "Inspector", "‹")),
      div(class = "wb-status", status)),
    if (exists("run_ui", mode = "function")) run_ui())
}

# The side panels' parts: a header with the fold button (Alt+[ the tree, Alt+] the inspector), the 28 px strip a folded panel
# leaves (its name, and a button to open it) and the drag handle on its inner edge (widths 180 to 480 px).
WB_PANEL_KEY <- c(nav = "Alt+[", aside = "Alt+]")
wb_panel_head <- function(p, name, arrow, ...) div(class = "wb-ph", span(class = "t", name), ...,
  tags$button(type = "button", class = "wb-pbtn", `data-wb-fold` = p, `aria-controls` = paste0("wb_", p), `aria-expanded` = "true",
              `aria-label` = paste("Hide the", tolower(name)), title = sprintf("Hide (%s)", WB_PANEL_KEY[[p]]), arrow))
wb_strip <- function(p, name, arrow) div(class = paste0("wb-strip wb-strip-", p),
  tags$button(type = "button", class = "wb-pbtn", `data-wb-unfold` = p, `aria-controls` = paste0("wb_", p), `aria-expanded` = "false",
              `aria-label` = paste("Show the", tolower(name)), title = sprintf("Show the %s (%s)", tolower(name), WB_PANEL_KEY[[p]]), arrow),
  span(class = "lab", name))
wb_grip <- function(p, name) div(class = paste0("wb-grip wb-grip-", p), `data-wb-grip` = p, role = "separator", `aria-orientation` = "vertical",
                                 `aria-label` = paste("Drag to resize the", name), title = "Drag to resize")

# Rows of the tree send their key to input[[id]]; a row is <a class="wb-row" data-wb-input="id" data-wb-key="...">.
WB_JS <- paste(readLines(file.path(CORE_DIR, "www", "workbench.js"), encoding = "UTF-8"), collapse = "\n")

# The chart of the data view (an element with id wb_chart holding a canvas and a .wb-ctip): see its first lines.
WB_CHART_JS <- paste(readLines(file.path(CORE_DIR, "www", "workbench_chart.js"), encoding = "UTF-8"), collapse = "\n")

# The point chart of the data view (an element with id wb_xy holding a canvas and a .wb-ctip): see its first lines.
WB_XY_JS <- paste(readLines(file.path(CORE_DIR, "www", "workbench_xy.js"), encoding = "UTF-8"), collapse = "\n")

wb_row <- function(label, key = NULL, input = NULL, n = "", on = FALSE, warn = FALSE, level = 0, title = NULL, dot = FALSE) {
  a <- list(class = paste("wb-row", if (on) "on", if (level) paste0("l", level)), href = "#", title = title)
  if (!is.null(input)) { a$`data-wb-input` <- input; a$`data-wb-key` <- key }
  do.call(tags$a, c(a, list(span(class = "lbl", label), if (dot) span(class = "dot", title = "unsaved changes"),
                            if (warn) span(class = "w", title = "needs a look", HTML("&#9650;")), span(class = "n", n))))
}
# A tab of the main area: a page of the "nav" tab set (data-ovid-go, handled by the shell's script).
wb_tab <- function(label, go, on = FALSE) tags$a(class = paste("wb-tab", if (on) "on"), href = "#", `data-ovid-go` = go, label)

# The grid: rows (records) by columns (parameters). rows and cols are the labels; cells and cls hold one character vector
# per column, each cell's HTML and class. sig names what is shown: a selection survives a redraw with the same sig.
# The selection comes back in input[[input]] as runs of rows per column (c, r1, r2) and the active cell (ar, ac).
# units: the unit of each column, named by column (shown under its heading; blank or missing shows none).
wb_grid <- function(rows, cols, cells, cls, sig, input = "wb_grid_sel", corner = "", times = NULL, vals = NULL, units = NULL) {
  nr <- length(rows); nc <- length(cols); e <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  body <- if (!nr) "" else {
    td <- lapply(seq_len(nc), function(j) { v <- if (!is.null(vals)) vals[[j]] else rep(NA_real_, nr)
      paste0('<td data-c="', j, '"', ifelse(nzchar(cls[[j]]), paste0(' class="', cls[[j]], '"'), ""), ifelse(is.finite(v), paste0(' data-v="', format(v, digits = 15, trim = TRUE), '"'), ""),
             ">", cells[[j]], "</td>") })
    paste0('<tr data-r="', seq_len(nr), '"', if (!is.null(times)) paste0(' data-t="', format(times, scientific = FALSE, trim = TRUE), '"') else "", '><th class="rh">', rows, "</th>",
           if (nc) do.call(paste0, td) else "", "</tr>", collapse = "") }
  head <- paste0('<tr><th class="cn" title="Select all">', e(corner), "</th>",
                 paste0('<th class="ch" data-c="', seq_len(nc), '" data-p="', e(cols), '" title="Select the column; shift or cmd/ctrl adds"><span class="wb-grip" title="Drag to move the column" draggable="true">\u22ee</span>', e(cols), wb_unit_html(cols, units),
                        '<span class="wb-sort" title="Sort by this column (again: the other way, then by time)">\u21c5</span><span class="wb-rsz" title="Drag to resize; double-click to fit"></span></th>', collapse = ""), "</tr>",
                 '<tr class="wb-gfrow"><th class="cf"></th>', paste0('<th class="gf" data-c="', seq_len(nc), '"><input class="wb-gfilt" data-c="', seq_len(nc), '" placeholder="filter" title="Show only rows where this column matches: >5, <=0, 1..3, =2, or text"></th>', collapse = ""), "</tr>")
  HTML(paste0('<div class="wb-gwrap" tabindex="0"><table class="wb-gt" data-input="', e(input), '" data-sig="', e(sig), '" data-nr="', nr, '" data-nc="', nc, '">',
              "<thead>", head, "</thead><tbody>", body, "</tbody></table></div>"))
}
wb_unit_html <- function(cols, units) { if (is.null(units)) return(""); u <- unname(units[cols]); u[is.na(u)] <- ""
  ifelse(nzchar(u), paste0('<span class="u">', htmltools::htmlEscape(u), "</span>"), "") }
# The unit and the range limits (lo, hi; NA where a rule sets none) of each parameter from an app's own tables: keys are
# the rule's parameter names (exact) or patterns (regex = TRUE, case ignored); the first rule that matches and sets a
# limit gives it. wb_match(params, keys, values) gives one value per parameter the same way.
wb_match <- function(params, keys, values, regex = FALSE) { keys <- as.character(keys); values <- as.character(values)
  stats::setNames(vapply(params, function(p) { k <- which(nzchar(values) & !is.na(values) & (if (regex) vapply(keys, function(x) isTRUE(tryCatch(grepl(x, p, ignore.case = TRUE, perl = TRUE), error = function(e) FALSE)), NA) else keys == p))
    if (length(k)) values[k[1]] else NA_character_ }, ""), params) }
wb_limits <- function(params, keys, lo, hi, regex = FALSE) { num <- function(x) suppressWarnings(as.numeric(x))
  l <- num(wb_match(params, keys, lo, regex)); h <- num(wb_match(params, keys, hi, regex))
  stats::setNames(lapply(seq_along(params), function(i) c(lo = l[i], hi = h[i])), params) }
wb_sec <- function(title, note = NULL) div(class = "wb-sec", span(class = "wb-eb", title), if (!is.null(note)) span(class = "n", note))
wb_fact <- function(k, v, sub = NULL) div(class = "wb-fact", span(class = "wb-eb", k), div(class = "v", v), if (!is.null(sub)) div(class = "s", sub))
wb_kv <- function(k, v) div(class = "wb-kv", span(k), span(v))
wb_step <- function(a, b, kind = "") div(class = "wb-step", span(class = paste("d", kind)), div(div(a), div(class = "m", b)))
wb_state <- function(label, kind = "flag") span(class = paste("wb-state", kind), label)

# ---- The data view ---------------------------------------------------------------------------------------------------
# Shared by every app's Workbench. The app supplies its records (a table with TIMESTAMP and one numeric column per
# parameter), the state of each cell and its corrections; these build the grid, the chart's data and the view's frame.
# A cell's state is "" (passed), "flag" (kept, marked suspect), "rem" (removed; old holds the value it had), "pend" (a
# saved correction the next run applies) or "back" (removed in the last run, its correction since deleted).

wb_digits <- function(v) { m <- stats::median(abs(v[is.finite(v)])); if (!is.finite(m)) 2L else if (m >= 100) 1L else if (m >= 10) 2L else 3L }
wb_fmt <- function(v, d) ifelse(is.na(v), "", sub("^-", "\u2212", formatC(v, format = "f", digits = d)))

# The cells of the grid: per parameter, each cell's HTML and its class (the state). states: per parameter a data.table
# with state and old. digits: decimals for some parameters (named), the others get them from their values.
wb_grid_cells <- function(w, params, states, digits = NULL) {
  out <- lapply(params, function(p) {
    v <- w[[p]]; s <- states[[p]]; d <- if (!is.null(digits) && !is.na(digits[p])) as.integer(digits[[p]]) else wb_digits(c(v, s$old)); txt <- wb_fmt(v, d)
    txt <- ifelse(s$state == "rem", paste0('<span class="x">\u2715</span><s>', wb_fmt(s$old, d), "</s>"),
           ifelse(s$state == "back", paste0('<span class="x">\u21ba</span>', wb_fmt(s$old, d)), txt))
    list(txt = txt, cls = s$state) })
  list(txt = lapply(out, `[[`, "txt"), cls = lapply(out, `[[`, "cls"))
}

# The chart's data for records d (WB_CHART_JS): per parameter its values, and where there are more than max records,
# the lowest and highest of each stretch of time instead (so a spike still shows; zooming in asks for the rest), with
# the old value of each removed value (removed: timestamp, parameter, old_value), saved corrections not run yet (pend:
# p, t, v) and flagged periods (fl: parameter, s, e). Times are seconds; gap is the time without records after which
# the line breaks.
# units (named by parameter) label each panel; limits (named list of c(lo, hi), from wb_limits()) are drawn as dashed lines.
wb_chart_data <- function(d, params, removed, fl, pend = NULL, max = 8000L, units = NULL, limits = NULL) {
  t <- as.numeric(d$TIMESTAMP); n <- length(t); thin <- n > max
  r0 <- if (n) min(t) else 0; r1 <- if (n) max(t) else 0; step <- if (n > 1) stats::median(diff(t)) else 0
  nb <- max(1L, as.integer(max %/% 2)); bw <- (r1 - r0) / nb
  pts <- function(tt, vv) { if (!thin) return(list(t = I(tt), v = I(vv)))
    x <- data.table(b = pmin(nb - 1L, as.integer((tt - r0) %/% max(bw, 1e-9))), t = tt, v = vv)[!is.na(v)]
    x <- unique(x[x[, .I[c(which.min(v), which.max(v))], by = b]$V1])[order(t)]
    list(t = I(x$t), v = I(x$v)) }
  few <- function(tt, vv) { if (length(tt) > max) { k <- unique(round(seq(1, length(tt), length.out = max))); tt <- tt[k]; vv <- vv[k] }; list(t = I(tt), v = I(vv)) }
  if (is.null(removed) || !nrow(removed)) removed <- data.table(timestamp = as.POSIXct(character(), tz = "UTC"), parameter = character(), old_value = numeric())
  if (is.null(fl) || !nrow(fl)) fl <- data.table(parameter = character(), s = numeric(), e = numeric())
  lim <- function(p, k) { v <- if (is.null(limits[[p]])) NA_real_ else unname(limits[[p]][k]); if (length(v) && is.finite(v)) v else NULL }
  list(series = lapply(params, function(p) c(list(p = p, u = if (!is.null(units) && !is.na(units[p])) unname(units[p]) else "", lo = lim(p, 1), hi = lim(p, 2)), pts(t, d[[p]]))),
       rem = lapply(params, function(p) { r <- removed[parameter == p & is.finite(old_value) & as.numeric(timestamp) >= r0 & as.numeric(timestamp) <= r1][order(timestamp)]
         c(list(p = p), few(as.numeric(r$timestamp), r$old_value)) }),
       pend = lapply(params, function(q) { x <- if (!is.null(pend) && nrow(pend)) pend[pend$p == q][order(t)] else data.table(t = numeric(), v = numeric())   # q, not p: inside [ ] p is the column
         c(list(p = q), few(as.numeric(x$t), x$v)) }),
       flags = lapply(params, function(p) { f <- fl[parameter == p & as.numeric(e) >= r0 & as.numeric(s) <= r1]
         list(p = p, s = I(as.numeric(f$s)), e = I(as.numeric(f$e))) }),
       n = n, decimated = thin, step = step, gap = if (thin) 3 * max(step, bw) else 3 * step)
}

# Runs of flagged rows as periods (parameter, s, e, detail), from a per-row flag column (rows in time order).
wb_flag_runs <- function(t, flag, parameter) {
  k <- which(!is.na(flag) & nzchar(flag)); if (!length(k)) return(data.table(parameter = character(), s = as.POSIXct(character(), tz = "UTC"), e = as.POSIXct(character(), tz = "UTC"), detail = character()))
  run <- cumsum(c(1L, diff(k) != 1L | flag[k][-1] != flag[k][-length(k)]))
  data.table(i = k, run = run, f = flag[k])[, .(parameter = parameter, s = t[min(i)], e = t[max(i)], detail = f[1]), by = run][, run := NULL][]
}

# The grid's selection (input from wb_grid(): runs of rows per column) as cells r (row of the window), c (column), p.
# g: the grid as shown (params, w and sig).
wb_sel_cells <- function(s, g) {
  none <- data.table(r = integer(), c = integer(), p = character())
  if (is.null(s) || is.null(g) || !identical(s$sig, g$sig)) return(none)
  cc <- as.integer(unlist(s$c)); a <- as.integer(unlist(s$r1)); b <- as.integer(unlist(s$r2))
  if (!length(cc) || length(a) != length(cc) || length(b) != length(cc)) return(none)
  x <- unique(rbindlist(Map(function(c, a, b) data.table(r = seq.int(a, b), c = c), cc, a, b)))[c >= 1 & c <= length(g$params) & r >= 1 & r <= nrow(g$w)]
  setorder(x, c, r); x[, p := g$params[c]][]
}

# Per parameter: how many cells are selected, their mean, low and high, and how many are removed (or saved for removal)
# and flagged. For the inspector when more than one cell is selected.
wb_sel_table <- function(cc, g, label = function(p) p) {
  x <- cc[, { v <- suppressWarnings(as.numeric(g$w[[p[1]]][r])); s <- g$states[[p[1]]]$state[r]; f <- v[is.finite(v)]
    .(n = .N, mean = if (length(f)) signif(mean(f), 4) else NA_real_, sd = if (length(f) > 1) signif(stats::sd(f), 3) else NA_real_,
      min = if (length(f)) signif(min(f), 4) else NA_real_, max = if (length(f)) signif(max(f), 4) else NA_real_,
      missing = sum(!is.finite(v) & !s %in% c("rem", "pend")), removed = sum(s %in% c("rem", "pend")), flagged = sum(s == "flag")) }, by = p]
  num <- function(v) if (is.na(v)) "\u2014" else sub("^-", "\u2212", format(v, big.mark = ",", scientific = FALSE, trim = TRUE))
  if (nrow(x) == 1) return(tags$table(class = "wb-seltbl wb-stats", tags$tr(tags$th(label(x$p)), tags$th("value")),
    lapply(list(c("Records", "n"), c("Mean", "mean"), c("Standard deviation", "sd"), c("Minimum", "min"), c("Maximum", "max"),
                c("Missing", "missing"), c("Removed or saved for removal", "removed"), c("Flagged", "flagged")),
           function(k) tags$tr(tags$td(k[1]), tags$td(num(x[[k[2]]]))))))
  tags$table(class = "wb-seltbl", tags$tr(tags$th("parameter"), tags$th("n"), tags$th("mean"), tags$th(title = "standard deviation", "std dev"), tags$th("min"), tags$th("max"),
                                         tags$th(title = "removed, or saved for removal", "removed"), tags$th("flagged")),
    lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(label(x$p[i])), tags$td(num(x$n[i])), tags$td(num(x$mean[i])), tags$td(num(x$sd[i])), tags$td(num(x$min[i])),
                                                tags$td(num(x$max[i])), tags$td(num(x$removed[i])), tags$td(num(x$flagged[i])))))
}

# The state of a cell as a chip: rule names what removed or flagged it; pend is the label of a saved correction.
wb_state_chip <- function(state, rule = "", pend = "saved \u00b7 run to apply") switch(state %||% "",
  rem = wb_state(paste("removed \u00b7", rule), "rem"), flag = wb_state(paste("flagged \u00b7", rule), "flag"),
  pend = wb_state(pend, "pend"), back = wb_state("restored \u00b7 run to apply", "back"), wb_state("passed", "ok"))

# The chart's toolbar and canvas (WB_CHART_JS draws it; its element has the id wb_chart).
wb_chart_ui <- function() tagList(
  div(class = "wb-ctools",
    tags$button(type = "button", `data-wb-chart` = "out", title = "Zoom out (wheel down)", HTML("&minus;")),
    tags$button(type = "button", `data-wb-chart` = "in", title = "Zoom in (wheel up, or Zoom to this on a selection)", "+"),
    tags$button(type = "button", `data-wb-chart` = "left", title = "Earlier (drag the time axis, Space + drag, sideways wheel or shift-wheel)", HTML("&#9664;")),
    tags$button(type = "button", `data-wb-chart` = "right", title = "Later", HTML("&#9654;")),
    tags$button(type = "button", `data-wb-chart` = "reset", title = "The whole period (or double-click the chart)", "Reset"),
    tags$button(type = "button", `data-wb-chart` = "back", title = "The previous view (Backspace)", HTML("&larr; Back")),
    span(class = "sep"),
    tags$button(type = "button", `data-wb-chart` = "period", title = "Make the stretch in view the period, so the grid lists its records", "Use as period"),
    span(class = "wb-chint", "drag \u2192 zooms in, \u2190 zooms out \u00b7 Shift + drag selects \u00b7 click a point \u00b7 Shift + click adds \u00b7 wheel zooms \u00b7 drag the time axis or Space + drag to move \u00b7 Esc clears")),
  div(id = "wb_chart", class = "wb-chart2", tags$canvas(), div(class = "wb-ctip")))

# The point chart's toolbar and canvas (WB_XY_JS draws it; its element has the id wb_xy).
wb_xy_ui <- function() tagList(
  div(class = "wb-ctools",
    tags$button(type = "button", `data-wb-xy` = "out", title = "Zoom out (wheel down)", HTML("&minus;")),
    tags$button(type = "button", `data-wb-xy` = "in", title = "Zoom in (wheel up, or Zoom to this on a selection)", "+"),
    tags$button(type = "button", `data-wb-xy` = "reset", title = "Every point (or double-click the chart)", "Reset"),
    tags$button(type = "button", `data-wb-xy` = "back", title = "The previous view (Backspace)", HTML("&larr; Back")),
    span(class = "sep"),
    span(class = "wb-chint", "drag \u2192 zooms to a box, \u2190 zooms out \u00b7 Shift + drag selects a box \u00b7 click a point \u00b7 Shift + click adds \u00b7 wheel zooms \u00b7 Space + drag moves \u00b7 Esc clears")),
  div(id = "wb_xy", class = "wb-chart2 wb-xy", tags$canvas(), div(class = "wb-ctip")))

# The point chart's data (send it with session$sendCustomMessage("wb-xy", ...)). x, y: the positions; id: each point's row
# key in the grid (the times argument of wb_grid()); c: the value that colours it (NA: drawn as a cross) on the scale clim,
# with ticks cticks labelled clabs; v and vlab: the value and its name on hover (as c when not given); lab: a line about
# each point on hover; s: each point's state ("", "flag", "pend", "back"), drawn as a ring; pal: colours from low to high;
# key: what is shown (the zoom is kept while it stays the same and the points cover the same area); equal: x and y are in the
# same unit (metres east and north), drawn to the same scale.
wb_xy_message <- function(x, y, id, c, v = NULL, s = "", lab = NULL, key = "", xlab = "x", ylab = "y", clab = "", vlab = clab,
                          clim = NULL, cticks = NULL, clabs = NULL, pal = c("#2c7bb6", "#ffffbf", "#d7191c"), idlab = "#", equal = FALSE) {
  n <- length(x); f <- c[is.finite(c)]
  if (is.null(clim) || length(clim) != 2 || !all(is.finite(clim))) clim <- if (length(f)) range(f) else c(0, 1)
  if (clim[1] == clim[2]) clim <- clim + c(-0.5, 0.5)
  if (is.null(cticks)) { cticks <- pretty(clim, 5); cticks <- cticks[cticks >= clim[1] & cticks <= clim[2]] }
  if (is.null(clabs)) clabs <- format(cticks, trim = TRUE)
  list(key = key, n = n, x = I(as.numeric(x)), y = I(as.numeric(y)), id = I(id), c = I(as.numeric(c)), v = I(if (is.null(v)) numeric() else as.numeric(v)),
       s = I(rep_len(as.character(s), n)), lab = I(if (is.null(lab)) character() else as.character(lab)), xlab = xlab, ylab = ylab, clab = clab, vlab = vlab,
       clim = I(as.numeric(clim)), cticks = I(as.numeric(cticks)), clabs = I(as.character(clabs)), pal = I(pal), idlab = idlab, equal = isTRUE(equal))
}

# The data view: chips of the charted parameters, the layout switch, the chart, the cell bar (the selected cell, and
# actions: the app's correction buttons) and the grid. A time series gets the period, the overview strip and the time
# chart; xy = TRUE the point chart instead (records that are not a time series), with no period.
wb_data_ui <- function(actions = NULL, tools = NULL, xy = FALSE) tagList(
  div(class = "wb-tools", uiOutput("wb_chips", inline = TRUE), div(class = "wb-fill"), tools,
      div(class = "wb-seg", radioButtons("wb_layout", NULL, c("Chart + table" = "both", "Table" = "table", "Chart" = "chart"), inline = TRUE)),
      if (!xy) tagList(dateRangeInput("wb_period", NULL, width = "250px"), actionLink("wb_whole", "Whole record"))),
  conditionalPanel("input.wb_layout != 'table'", div(class = "wb-chart", if (xy) wb_xy_ui() else tagList(plotOutput("wb_overview", height = "46px"), wb_chart_ui())),
    div(class = "wb-split", title = "Drag to give the chart or the table more room (double-click: back to the usual size)")),
  div(class = "wb-cellbar", uiOutput("wb_cellinfo", class = "wb-cellinfo"), if (length(actions)) div(class = "wb-acts", actions)),
  div(class = "wb-gtools", tags$input(type = "search", class = "wb-gfind", placeholder = "Find in the table\u2026", title = "Marks the cells that hold this text or number"),
      if (!xy) tags$input(type = "date", class = "wb-ggoto", title = "Go to date: the period starts there, as long as it is now"),
      tags$button(type = "button", class = "wb-btn sm", `data-wb-gcopy` = "1", title = "Copy the selected cells as tab-separated text (also Cmd/Ctrl+C)", "Copy"),
      span(class = "wb-gcolw", tags$button(type = "button", class = "wb-btn sm", `data-wb-gcols` = "1", title = "Show or hide columns, and the decimals each one shows (the data is not changed)", "Columns")),
      tags$button(type = "button", class = "wb-btn sm", `data-wb-gwrap` = "1", title = "Wrap long headings and cells (the row grows to fit)", "Wrap"),
      span(class = "wb-gmsg"), span(class = "wb-gstat", title = "Of the selected cells that hold a number")),
  conditionalPanel("input.wb_layout != 'chart'", class = "wb-gpanel", uiOutput("wb_grid", class = "wb-gout"), div(class = "wb-ginfo", textOutput("wb_ginfo", inline = TRUE))))

# The strip above the chart: the daily median of the first charted parameter over the whole record, the period as a box
# and the stretch in view (view: input wb_view) shaded.
# TRUE once the strip has room to draw: while the side panels settle their widths it can briefly be a few pixels wide,
# and drawing then fails ("figure margins too large"); the strip waits for its next size instead.
wb_overview_fits <- function(session = shiny::getDefaultReactiveDomain()) isTRUE((session$clientData$output_wb_overview_width %||% 0) >= 60)
wb_overview_plot <- function(t, y, period, view = NULL, pal) {
  tk <- ovid_tokens(pal)
  m <- data.table(day = as.Date(t), y = y)[, .(y = stats::median(y, na.rm = TRUE)), by = day]
  pr <- as.Date(period)
  vb <- if (!is.null(view) && is.finite(suppressWarnings(as.numeric(view$x0 %||% NA)))) as.POSIXct(c(as.numeric(view$x0), as.numeric(view$x1)), origin = "1970-01-01", tz = "UTC") else NULL
  ggplot2::ggplot(m, ggplot2::aes(as.POSIXct(day), y)) +
    ggplot2::annotate("rect", xmin = as.POSIXct(pr[1]), xmax = as.POSIXct(pr[2]) + 86400, ymin = -Inf, ymax = Inf, fill = NA, colour = tk$acc2, linewidth = 0.6) +
    (if (!is.null(vb)) ggplot2::annotate("rect", xmin = vb[1], xmax = vb[2], ymin = -Inf, ymax = Inf, fill = tk$acc2, alpha = 0.3, colour = NA)) +
    ggplot2::geom_line(colour = tk$fg4, linewidth = 0.3, na.rm = TRUE) + ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = tk$bg, colour = NA), plot.margin = ggplot2::margin(2, 4, 2, 4))
}

# Sends the chart its data (WB_CHART_JS). base(): the period's message (type "base", key, sig, period, zone and
# wb_chart_data()); detail(a, z): the data of a zoomed-in window at full resolution, asked for when the period was
# thinned (NULL when it holds too many records). "Use as period" sets the date range input period_id. Returns the view
# in the chart (debounced), for the overview strip.
wb_chart_server <- function(input, session, base, detail = NULL, period_id = "wb_period") {
  observe(session$sendCustomMessage("wb-chart", base()))
  observeEvent(input$wb_view, { b <- base(); if (!isTRUE(b$decimated) || !is.function(detail)) return()
    v <- input$wb_view; x0 <- suppressWarnings(as.numeric(v$x0)); x1 <- suppressWarnings(as.numeric(v$x1)); req(is.finite(x0), is.finite(x1), x1 > x0)
    w <- x1 - x0; a <- max(b$period[1], x0 - w / 2); z <- min(b$period[2], x1 + w / 2)
    m <- detail(a, z); if (is.null(m)) return()
    session$sendCustomMessage("wb-chart", c(list(type = "detail", sig = b$sig, cover = I(c(a, z))), m)) })
  observeEvent(input$wb_goto, { d <- suppressWarnings(as.Date(input$wb_goto)); p <- input[[period_id]]; req(!is.na(d))   # Go to date: the same span from there
    w <- if (length(p) == 2 && !anyNA(p)) as.numeric(diff(as.Date(p))) else 21; updateDateRangeInput(session, period_id, start = d, end = d + w) })
  observeEvent(input$wb_view_period, { v <- input$wb_view_period
    d <- as.Date(as.POSIXct(c(as.numeric(v$x0), as.numeric(v$x1)), origin = "1970-01-01", tz = "UTC")); updateDateRangeInput(session, period_id, start = d[1], end = d[2]) })
  debounce(reactive(input$wb_view), 300)
}

# Keys of the data view: R, or Delete in the grid, presses the button with the id `target` (the first correction).
wb_keys_js <- function(target = "wb_act_remove") paste0("$(document).on('keydown', function(e){ if ($(e.target).is('input,textarea,select,[contenteditable]') || e.metaKey || e.ctrlKey || e.altKey) return;
    if (!$('.tab-pane.active[data-value=\"wb-data\"]').length || $('.modal.in,.modal.show').length) return;
    var grid = $(e.target).closest('.wb-gwrap').length > 0;
    if (e.key === 'r' || e.key === 'R' || (grid && (e.key === 'Delete' || e.key === 'Backspace'))) { var b = document.getElementById('", target, "'); if (b) { e.preventDefault(); b.click(); } } });")

# A table as HTML, for previews of the rows a correction writes.
wb_html_table <- function(d, max_rows = 12) {
  d <- as.data.frame(d, stringsAsFactors = FALSE); more <- nrow(d) - max_rows; d <- utils::head(d, max_rows)
  e <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; htmltools::htmlEscape(x) }
  rows <- vapply(seq_len(nrow(d)), function(i) paste0("<tr>", paste0("<td>", e(vapply(d[i, , drop = FALSE], function(v) as.character(v), "")), "</td>", collapse = ""), "</tr>"), "")
  paste0("<table><thead><tr>", paste0("<th>", e(names(d)), "</th>", collapse = ""), "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>",
         if (more > 0) paste0("<p class='wb-p' style='color:var(--wb-fg2);padding:4px 8px'>and ", more, " more rows</p>") else "")
}
wb_preview <- function(d) {
  if (is.null(d) || !nrow(d)) return(div(class = "wb-p", style = "color:var(--wb-fg2)", "Nothing to write."))
  div(class = "wb-preview", HTML(wb_html_table(d, 12)))
}

# ---- Pages, tabs and set-up tables -------------------------------------------------------------------------------------
# The app's Standard pages as a tree, by the rail's sections; Home is labelled `home` (the project overview).
wb_tools_tree <- function(pages, home = "Overview", open = FALSE) {
  out <- list(); sec <- NULL; rows <- list()
  flush <- function() if (length(rows)) out[[length(out) + 1]] <<- tagList(if (!is.null(sec)) div(class = "wb-subsec", sec), rows)
  for (x in pages) {
    if (is.character(x)) { flush(); sec <- x; rows <- list(); next }
    if (inherits(x, "ovid_stage")) { flush(); sec <- paste(x$n, x$title); rows <- list(); next }       # a stage of the set-up flow
    if (!inherits(x, "ovid_tool") || !isTRUE(x$rail)) next
    rows[[length(rows) + 1]] <- tags$a(class = "wb-row", href = "#", `data-ovid-go` = x$value, title = x$title,
                                       span(class = "lbl", if (!is.null(x$n)) span(class = "n", x$n), if (identical(x$value, "Home")) home else x$title))
  }
  flush()
  tags$details(class = "wb-tree", open = if (open) NA, tags$summary(span(class = "wb-eb", "Pages")), out)
}

# The tab bar: Import, the data view (labelled data_label()), and the app pages opened last (up to five), each closable.
# Returns the list of recent pages.
wb_tabs_server <- function(input, output, session, pages, data_label, home = "Overview", pipeline = NULL) {
  recent <- reactiveVal(character())
  titles <- local({ x <- Filter(function(x) inherits(x, "ovid_tool"), pages); stats::setNames(vapply(x, `[[`, "", "title"), vapply(x, `[[`, "", "value")) })
  fixed <- vapply(pipeline %||% list(), function(x) x$go, "")
  observeEvent(input$nav, { v <- input$nav; if (!grepl("^wb-", v) && !v %in% fixed) recent(utils::head(unique(c(v, recent())), 5)) })
  observeEvent(input$wb_close, { recent(setdiff(recent(), input$wb_close)); if (identical(input$nav, input$wb_close)) updateTabsetPanel(session, "nav", selected = "wb-data") })
  output$wb_tabbar <- renderUI({ v <- input$nav %||% "wb-data"; lab <- tryCatch(data_label(), error = function(e) NULL)
    data_tab <- function(l) wb_tab(if (length(lab) && nzchar(lab)) paste0(l, " \u00b7 ", lab) else l, "wb-data", on = identical(v, "wb-data"))
    tagList(wb_tab(HTML("&#10515; Import"), "wb-import", on = identical(v, "wb-import")),
            if (length(pipeline)) lapply(pipeline, function(x) if (identical(x$go, "wb-data")) data_tab(x$label) else
              tags$a(class = paste("wb-tab", if (identical(v, x$go)) "on"), href = "#", title = x$title %||% x$label,
                     `data-wb-open` = as.character(jsonlite::toJSON(x[intersect(names(x), c("go", "tab", "details"))], auto_unbox = TRUE)), x$label))
            else wb_tab(if (length(lab) && nzchar(lab)) lab else "Data", "wb-data", on = identical(v, "wb-data")),
            lapply(rev(recent()), function(r) tags$span(class = paste("wb-tab", if (identical(v, r)) "on"),
              tags$a(href = "#", `data-ovid-go` = r, if (identical(r, "Home")) home else if (r %in% names(titles)) titles[[r]] else r),
              tags$a(href = "#", class = "x", title = "Close", `data-wb-input` = "wb_close", `data-wb-key` = r, HTML("&times;"))))) })
  recent
}

# Where each set-up table is edited (open: id -> list(go = page, tab = a tab on it, details = a folded section on it)),
# as the data-wb-open attribute of a link.
wb_open_attr <- function(open, id) as.character(jsonlite::toJSON(open[[id]], auto_unbox = TRUE))

# The set-up tables in the tree: a row per table (tables: id, file, label) with its row count and a dot for unsaved
# edits (E: the table editors, core/editor.R); a click opens the page and the tab with its editor.
wb_setup_tree <- function(E, tables, open, stale = FALSE) tags$details(class = "wb-tree",
  tags$summary(span(class = "wb-eb", "Set-up tables"), if (isTRUE(stale)) span(class = "w", title = "saved changes the next run applies", "run to apply")),
  lapply(seq_len(nrow(tables)), function(i) { s <- tables[i]; ed <- E[[s$id]]; d <- if (!is.null(ed)) ed$data(); dt <- !is.null(ed) && isTRUE(ed$dirty())
    tags$a(class = "wb-row", href = "#", `data-wb-open` = wb_open_attr(open, s$id), title = paste0(s$label, if (dt) " (unsaved changes)" else ""),
           span(class = "lbl", s$file), if (dt) span(class = "dot"), span(class = "n", if (is.null(d)) "" else nrow(d))) }))

# Outputs written by another window or from the command line: read them again. file(): a file every run rewrites (the
# audit index); rv$tick is the app's counter that makes it read its outputs (this window's own runs move it already).
wb_watch_outputs <- function(session, file, rv, every = 3000) {
  now <- reactivePoll(every, session, function() file_stamp(file()), function() file_stamp(file()))
  seen <- reactiveVal("")
  observeEvent(rv$tick, seen(file_stamp(isolate(file()))))
  observeEvent(now(), if (!identical(now(), isolate(seen()))) { seen(now()); rv$tick <- isolate(rv$tick) + 1 }, ignoreInit = TRUE)
  invisible(now)
}

# The count of what a run redid, for its message: n of total units (stations, loggers, sites, surveys...) built again.
wb_redone <- function(redone, total, unit) {
  if (is.na(redone) || is.na(total) || !total) return("")
  if (redone == 0) return(paste0(" Nothing had changed: every ", sub("s$", "", unit), " was reused."))
  paste0(" Redone: ", redone, " of ", total, " ", unit, if (redone < total) "; the rest was unchanged and reused." else ".")
}

# ---- Undo after an action (plot standards A11) -------------------------------------------------------------------------
# After a Workbench action writes its rows: a bar saying what was done, with Undo (or Ctrl/Cmd+Z while the bar shows), which
# puts that table back as it was (the table store keeps the version before: core editor.R). Nothing reaches the data before a
# run, so an action and its Undo only change the set-up table. Call wb_undo_server() once in the server, and
# wb_undo_offer(session, msg) where an action has been written.
wb_undo_server <- function(input, session) observeEvent(input$wb_undo, { u <- session$userData$ovid_undo; session$userData$ovid_undo <- NULL
  if (!is.null(u) && isTRUE(u$fn())) showNotification(paste0("Undone: ", u$file, " is as it was before. Run to apply."), type = "message") })
wb_undo_offer <- function(session, msg) { u <- session$userData$ovid_undo
  if (!is.null(u) && difftime(Sys.time(), u$at, units = "secs") < 5) session$sendCustomMessage("wb-undo", list(msg = msg)) else showNotification(msg, type = "message") }

# ---- Corrections over a period (core/periods.R) ------------------------------------------------------------------------
# The period tool: a button for the cell bar, and its dialog. The period is the selected cells' (a drag on the chart or
# a range in the grid), or typed; the row goes to period_corrections.csv through the app's editor for it (ed), where the
# list can be edited, switched off or undone; a run applies it, the raw files are never changed.
wb_period_button <- function() actionButton("wb_act_period", "Correct a period\u2026", class = "wb-btn sm",
  title = "Remove, offset, scale, linear drift, fill by interpolation or flag over a period: typed dates, or the cells or chart stretch selected (period_corrections.csv)")
# sel(): the selection (list p = parameters, t = times) or NULL; target(): the record's name (station, site, logger);
# params(): the parameters to offer; open_table(): opens the corrections list.
wb_period_server <- function(input, output, session, ed, sel, target, params, open_table = function() NULL, preset = NULL) {
  who <- reactiveVal("")
  row <- reactive(data.table(target = trim(input$wb_pc_target %||% ""), parameter = paste(input$wb_pc_params %||% character(), collapse = "|"),
    from = trim(input$wb_pc_from %||% ""), to = trim(input$wb_pc_to %||% ""), action = input$wb_pc_action %||% "remove",
    value = if ((input$wb_pc_action %||% "") %in% c("offset", "scale", "drift")) trim(input$wb_pc_value %||% "") else "",
    on = "on", note = trim(input$wb_pc_note %||% ""), author = trim(input$wb_pc_author %||% ""), added = format(Sys.time(), "%Y-%m-%d %H:%M")))
  f <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")
  dialog <- function(tg, ps, from, to, action = "remove", note = "", lead = NULL) {
    showModal(modalDialog(title = "Correct a period", size = "l", easyClose = TRUE,
      div(class = "wb-p", style = "color:var(--wb-fg2)", lead %||% "Type the period (a date without a time runs to the end of that day), or close this, select cells or drag across the chart, and open it again."),
      div(class = "wb-form2", textInput("wb_pc_target", "On (station, site or logger; blank for all)", tg %||% "", width = "100%"),
          selectizeInput("wb_pc_params", "Parameters (blank for every value)", choices = unique(c(ps, tryCatch(params(), error = function(e) character()))), selected = ps, multiple = TRUE, width = "100%")),
      div(class = "wb-form2", textInput("wb_pc_from", "From", from, width = "100%"), textInput("wb_pc_to", "To", to, width = "100%")),
      div(class = "wb-form2", selectInput("wb_pc_action", "Action", stats::setNames(names(PERIOD_ACTIONS), PERIOD_ACTIONS), selected = action, width = "100%"), textInput("wb_pc_value", "Value (for offset, scale and drift)", "", width = "100%")),
      div(class = "wb-form2", textInput("wb_pc_note", "Why (goes in the audit trail)", note, width = "100%"), textInput("wb_pc_author", "Initials", isolate(who()), width = "100%")),
      div(class = "wb-p", style = "color:var(--wb-fg2)", HTML(paste0("<b>Remove</b> blanks the values; <b>offset</b> adds the value; <b>scale</b> multiplies by it; <b>linear drift</b> adds a share of it that grows from none at From to all of it at To; ",
        "<b>fill by interpolation</b> draws a straight line in time from the last value before the period to the first after it; <b>flag</b> keeps the values and flags them. ",
        "Corrections apply in the order of the list, after calibrations and before the quality rules; the raw files are never changed."))),
      wb_sec("Row for period_corrections.csv"), uiOutput("wb_pc_preview"),
      footer = tagList(actionLink("wb_pc_list", "Open the list"), modalButton("Cancel"), actionButton("wb_do_period", "Add to corrections", class = "btn-primary")))) }
  observeEvent(input$wb_act_period, { if (isTRUE(get0("VIEWER"))) return(); s <- sel(); has <- !is.null(s) && length(s$t)
    if (has) dialog(target(), unique(s$p), f(min(s$t)), f(max(s$t)), lead = paste0("The selected stretch: ", f(min(s$t)), " to ", f(max(s$t)), ", ", length(s$t), " values. Change the dates to type another period."))
    else dialog(tryCatch(target(), error = function(e) ""), NULL, "", "") })
  # from the Needs you list: list(target, parameter, from, to, action, note)
  if (!is.null(preset)) observeEvent(preset(), { x <- preset(); req(x); if (isTRUE(get0("VIEWER"))) return()
    dialog(x$target, x$parameter, x$from, x$to, x$action %||% "remove", x$note %||% "", lead = x$lead) }, ignoreInit = TRUE)
  output$wb_pc_preview <- renderUI(wb_preview(row()[, .(target, parameter, from, to, action, value, note)]))
  observeEvent(input$wb_pc_list, { removeModal(); open_table() })
  observeEvent(input$wb_do_period, { r <- row(); chk <- period_read_rows(r)
    if (nrow(chk$bad)) { pb <- chk$bad$problem[1]; return(ovid_field_error(if (grepl("^from", pb)) "wb_pc_from" else if (grepl("^to", pb)) "wb_pc_to" else if (grepl("value", pb)) "wb_pc_value" else "wb_pc_action", pb)) }
    if (!nzchar(r$from) && !nzchar(r$to)) return(ovid_field_error("wb_pc_from", "Give the period: From, To or both."))
    if (!nzchar(r$note)) return(ovid_field_error("wb_pc_note", "Say why; it goes in the audit trail."))
    if (isTRUE(ed$append(as.data.frame(r)))) { who(r$author); removeModal()
      showNotification(paste0(PERIOD_ACTIONS[[r$action]], " added to the corrections list (period_corrections.csv). Run to apply."), type = "message") } })
  invisible()
}

# The app's own processing, as a section of the tree: each row opens the page (and tab or folded section) where that step
# is set, as the per-app set-up design lists them. items: list of list(label, go, tab, details, title).
wb_process_tree <- function(app, items, open = TRUE) tags$details(class = "wb-tree", open = if (open) NA,
  tags$summary(span(class = "wb-eb", paste(app, "processing"))),
  lapply(items, function(x) tags$a(class = "wb-row", href = "#", title = x$title %||% x$label,
    `data-wb-open` = as.character(jsonlite::toJSON(x[intersect(names(x), c("go", "tab", "details"))], auto_unbox = TRUE)), span(class = "lbl", x$label))))

# ---- Needs you (top right) ---------------------------------------------------------------------------------------------
# What waits for a decision, counted at the top of the Workbench: deployment matches that are only a guess (a year, a name
# in the files, a logger's only deployment), loggers found in the files in no row, files with no serial (core deployments.R,
# as the Needs you page shows them) and the open items of the one review list (core flow.R: flags, moves, the run's other
# findings). Each opens the Needs you page, where it is settled.
# Grouped by place (site or station; a logger's serial until it has one) with the same check on nearby days merged into one
# line, and each line labelled with what to do about it (core flow.R needs_fix_of: decide, fix in ovid, raw data, add a file).
wb_needs_items <- function(check = NULL, items = NULL) {
  out <- list(); add <- function(group, fix, what, detail, n = 1L) out[[length(out) + 1L]] <<- data.table(group = group, fix = fix, what = what, detail = detail, n = n, go = "Needs")
  if (!is.null(check)) {
    r <- check$rows; if (!is.null(r) && nrow(r)) { r <- r[!match %in% c("dates", "no data", "no data in period")]
      if (nrow(r)) { st <- if ("station" %in% names(r)) as.character(r$station) else rep("", nrow(r))
        add(ifelse(!is.na(st) & nzchar(st), st, as.character(r$serial)), "fix", paste0(r$serial, " matched by ", r$match), "a suggestion to accept or change") } }
    u <- check$unclaimed; if (!is.null(u) && nrow(u)) { u <- u[, .(nm = paste(unique(unlist(strsplit(as.character(names_in_files), "; ", fixed = TRUE))), collapse = "; ")), by = serial]
      add("Not in the deployment sheet", "fix", as.character(u$serial), paste0("found in the files (", u$nm, "): add it to the deployment sheet")) }
    f <- check$unassigned; if (!is.null(f) && nrow(f)) add("Not in the deployment sheet", "raw", paste(nrow(f), "file(s) with no serial"), "the file header has no serial: name the logger, or leave the file out")
  }
  if (!is.null(items) && NROW(items)) { x <- copy(as.data.table(items)); for (v in c("detail", "parameter", "start", "group", "fix")) if (!v %in% names(x)) x[, (v) := ""]
    x[, `:=`(.g = needs_group_of(kind, where, group), .f = needs_fix_of(kind, paste(what, detail), fix))]
    fl <- x[kind == "flag"]
    if (nrow(fl)) { m <- fl[, .(n = .N, days = paste(unique(substr(start, 1, 10)), collapse = ", "), p = paste(unique(unlist(strsplit(parameter, ", ", fixed = TRUE))), collapse = ", ")), by = .(.g, .f, what)]
      add(m$.g, m$.f, paste0(m$what, " \u00b7 ", m$p, ifelse(m$n > 1, paste0(" \u00b7 ", m$n, " times"), "")), m$days, m$n) }
    ot <- x[kind != "flag"]
    if (nrow(ot)) add(ot$.g, ot$.f, ifelse(ot$kind == "moved", paste(ot$where, "looks moved"), ifelse(nzchar(ot$where) & ot$.g == "Project", paste0(ot$where, ": ", ot$what), ot$what)),
                    ifelse(ot$kind == "moved", ot$what, ot$detail)) }
  if (!length(out)) return(data.table(group = character(), fix = character(), what = character(), detail = character(), n = integer(), go = character()))
  x <- rbindlist(out); x[, detail := ifelse(is.na(detail), "", detail)]
  x[order(group == "Project", group == "Not in the deployment sheet", match(group, unique(group)))]
}
wb_needs_ui <- function() uiOutput("wb_needs", inline = TRUE)
# by: group the list by place (default) or by what to do (input wb_needs_by)
wb_needs_server <- function(output, items, input = shiny::getDefaultReactiveDomain()$input) output$wb_needs <- renderUI({
  x <- tryCatch(items(), error = function(e) NULL); n <- if (is.null(x)) 0L else sum(x$n)
  by <- if (identical(input$wb_needs_by, "fix")) "fix" else "group"
  body <- if (!n) div(class = "wb-needs-none", "Nothing waits for you.") else {
    key <- if (by == "fix") unname(NEEDS_LABELS[x$fix]) else x$group; ks <- unique(if (by == "fix") unname(NEEDS_LABELS[intersect(names(NEEDS_LABELS), x$fix)]) else key)
    lapply(ks, function(k) { y <- x[key == k]
      tags$details(class = "wb-needs-g", open = if (length(ks) <= 4 || k %in% c("Project", "Not in the deployment sheet")) NA,
        tags$summary(tags$b(k), span(class = "ct", sum(y$n))),
        lapply(seq_len(min(nrow(y), 40)), function(i) tags$a(class = "wb-needs-it", href = "#", `data-ovid-go` = y$go[i],
          span(class = paste0("wb-tag t-", y$fix[i]), NEEDS_LABELS[[y$fix[i]]]), div(tags$b(y$what[i]), if (nzchar(y$detail[i])) div(class = "m", y$detail[i])))),
        if (nrow(y) > 40) div(class = "wb-needs-none", paste("and", nrow(y) - 40, "more on the Needs you page"))) }) }
  tags$details(class = "wb-needs", tags$summary(class = "wb-btn", title = "Matches to confirm, flags to decide and what else the last run found", "Needs you ", tags$b(class = if (n) "w" else "ok", n), " \u25be"),
    div(class = "wb-needs-pop",
      div(class = "wb-needs-by", span("group by"),
        tags$a(href = "#", class = if (by == "group") "on", onclick = "Shiny.setInputValue('wb_needs_by','group');return false", "place"),
        tags$a(href = "#", class = if (by == "fix") "on", onclick = "Shiny.setInputValue('wb_needs_by','fix');return false", "what to do")),
      body)) })
WB_NEEDS_CSS <- paste(readLines(file.path(CORE_DIR, "www", "workbench_needs.css"), encoding = "UTF-8"), collapse = "\n")

# The review list (core flow.R) inside the Workbench: a flag opens its record here, and Remove... and Correct... open the
# period tool with the flag's stretch (preset: wb_period_server's reactiveVal).
wb_review_handlers <- function(review, open, preset) { if (is.null(review$handlers)) return(invisible())
  review$handlers$open <- open; review$handlers$preset <- preset; invisible() }
# The chips and table of RS's own Needs you tab (RS/workbench/wb_views.R).
WB_QUEUE_CSS <- paste(readLines(file.path(CORE_DIR, "www", "workbench_queue.css"), encoding = "UTF-8"), collapse = "\n")
# Open a record at a stretch (from the Needs you list): the table key k, the stretch t0 to t1 (a day either side) and its
# parameters ticked. jump: list(period, params) of reactiveVals that the app's own table-change observer takes up
# (wb_jump_take) when the table changes; the same table is moved at once.
wb_jump_new <- function() list(period = reactiveVal(NULL), params = reactiveVal(NULL))
wb_jump_to <- function(session, input, key, jump, set_view, k, params, t0, t1) {
  p <- suppressWarnings(as.Date(substr(c(t0, t1), 1, 10))) + c(-1, 1); if (anyNA(p)) p <- NULL
  if (identical(isolate(key()), k)) { if (!is.null(p)) updateDateRangeInput(session, "wb_period", start = p[1], end = p[2])
    if (length(params)) updateCheckboxGroupInput(session, "wb_params", selected = unique(c(params, isolate(input$wb_params)))) }
  else if (!is.null(k) && length(k) == 1 && !is.na(k)) { jump$period(p); jump$params(params); key(k) }
  set_view("wb-data") }
# In the app's observer that sets the parameters and the period for a new table: what a jump asked for, if any.
wb_jump_take <- function(jump, pa, want, r, from, to) {
  jp <- isolate(jump$params()); if (length(jp)) { want <- unique(c(intersect(jp, pa), want)); jump$params(NULL) }
  jw <- isolate(jump$period()); if (!is.null(jw)) { from <- max(as.Date(r[1]), jw[1]); to <- min(as.Date(r[2]), jw[2]); jump$period(NULL) }
  from <- max(as.Date(r[1]), as.Date(from)); to <- min(as.Date(r[2]), as.Date(to)); if (to < from) { from <- max(as.Date(r[1]), as.Date(r[2]) - 21); to <- as.Date(r[2]) }   # always inside the record
  list(want = want, from = from, to = to) }
# The corrections over a period that cover a clicked value, as lineage steps: tab, the list as the editor holds it (saved
# or not); keys, the names the record answers to; names, the value's column (and sensor or channel); t, its time.
wb_period_steps <- function(tab, keys, names, t) {
  if (is.null(tab) || !NROW(tab) || is.null(t) || is.na(t)) return(NULL)
  r <- tryCatch(period_read_rows(ensure_cols(as.data.table(tab), PERIOD_COLS))$rows, error = function(e) NULL); if (is.null(r) || !nrow(r)) return(NULL)
  k <- tolower(trim(as.character(keys[!is.na(keys)]))); nm <- trim(as.character(names[!is.na(names)]))
  hit <- (!nzchar(r$target) | r$target == "*" | tolower(r$target) %in% k) &
    (!nzchar(r$parameter) | r$parameter == "*" | vapply(strsplit(r$parameter, "|", fixed = TRUE), function(p) any(trim(p) %in% nm), NA)) &
    (is.na(r$t0) | t >= r$t0) & (is.na(r$t1) | t <= r$t1)
  r <- r[hit]; if (!nrow(r)) return(NULL)
  lapply(seq_len(nrow(r)), function(i) wb_step(paste0(PERIOD_ACTIONS[[r$action[i]]], if (!is.na(r$value[i])) paste0(" ", r$value[i])),
    paste0("corrections over a period, row ", r$row[i], if (nzchar(r$note[i])) paste0(": ", r$note[i]), " (applied by a run)"), "w"))
}
# The Workbench with no data yet: the way in (Import data) and a look at the raw files as they are (raw: that page's value).
wb_empty_acts <- function(raw = NULL) div(class = "wb-inline", style = "margin:8px 0",
  tags$a(class = "wb-btn sm primary", href = "#", `data-ovid-go` = "wb-import", HTML("&#10515; Go to Import")),
  if (!is.null(raw)) tags$a(class = "wb-btn sm", href = "#", `data-ovid-go` = raw, title = "Open a raw file and look at it as it is, before any run", "Open a raw file"))
