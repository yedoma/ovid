# GT/workbench/wb_gt.R  --  The GT Workbench (preview): one site's ground temperatures (or its soil moisture and
# conductivity), a column per depth, charted over a spreadsheet grid, with the selected cells, where they came from and how
# to correct them beside it, and every page of the Standard interface around it.
# Part of Ovid Core // GT. Sourced by app.R after gt_pipeline.R and core/shell.R.
# A Workbench session runs the Standard server too (app.R, server(embedded = TRUE)) and shows its pages in the same hidden
# tab set, so every Standard process (run, plot and explore, compare sites, summaries, soil, site details, raw files, sensor
# health, clock checks, audit trail, sites, sensors and depths, quality rules, configuration, set-up check, settings, reports)
# works here through the same code. This file adds the data view; wb_edit.R the corrections made from a selection;
# wb_project.R the Import view, the trees and the tabs.

source(file.path(CORE_DIR, "workbench.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_edit.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_project.R"), local = TRUE)

WB_MAX_ROWS <- 5000L                                   # rows the grid shows at once; narrow the period for more
WB_CHART_MAX <- 8000L                                  # records per depth the chart gets for the period; more are thinned
WB_DETAIL_MAX <- 40000L                                # records it gets at full resolution for a zoomed-in window
WB_GRID_PARAMS <- 30L                                  # depths in the grid
WB_MAX_PARAMS <- 6L                                    # of those, charted
WB_ATTR <- c("flag", "serial", "sensor", "logger_id", "depth_source")    # kept per value beside it, in columns "<attr>|<column>"


# A record in the tree: a site's ground temperatures ("temp:<site>") or its soil moisture and conductivity ("soil:<site>").
wb_split <- function(key) list(kind = sub(":.*$", "", key), id = sub("^[^:]*:", "", key))

# The records the Workbench lists: every site of the last run (by instrument), with its soil record when it has one.
wb_tables <- function(o, pd) {
  if (is.null(o) || !nrow(o$ss)) return(data.table())
  s <- o$ss[order(instrument, site)]
  t <- s[, .(key = paste0("temp:", site), site, group = site, label = "temperature", instrument, records = values, kind = "temp")]
  so <- s[file.exists(out_dir(pd, "soil", paste0(safe_name(site), ".rds")))]
  if (nrow(so)) t <- rbind(t, so[, .(key = paste0("soil:", site), site, group = site, label = "soil", instrument, records = NA_integer_, kind = "soil")])
  t[order(match(site, s$site), kind != "temp")]
}

# The column of a value: its depth (temperature), or its variable and depth (soil).
wb_col <- function(variable, depth_m, kind) if (identical(kind, "temp")) depth_lab(depth_m) else paste(variable, depth_lab(depth_m))

# A site's record as a wide table: TIMESTAMP, a numeric column per depth (or variable and depth), and beside each the
# flag, serial, sensor, logger and depth source of every value ("<attr>|<column>"). Columns in order of depth.
# Its attributes: cols (the value columns) and where (column -> variable and depth_m).
wb_load <- function(key, pd) {
  if (!length(key) || is.na(key[1]) || !nzchar(key[1])) return(NULL)             # nothing chosen yet
  k <- wb_split(key); if (!length(k$kind)) return(NULL)
  d <- if (k$kind == "temp") load_site(k$id, pd) else load_soil(k$id, pd)
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- copy(d); if (!"variable" %in% names(d)) d[, variable := "temp"]
  for (a in WB_ATTR) if (!a %in% names(d)) d[, (a) := NA_character_]
  d[, col := wb_col(variable, depth_m, k$kind)]
  where <- unique(d[, .(col, variable, depth_m = round(depth_m, 4))], by = "col")[order(variable != "temp", variable, depth_m)]   # one depth per column
  first <- function(x) x[1]
  w <- dcast(d, TIMESTAMP ~ col, value.var = "value", fun.aggregate = first, fill = NA)
  for (a in WB_ATTR) { x <- dcast(d, TIMESTAMP ~ col, value.var = a, fun.aggregate = first, fill = NA)
    setnames(x, where$col, paste0(a, "|", where$col), skip_absent = TRUE); w <- w[x, on = "TIMESTAMP"] }
  # times at which every value was removed (a trimmed start, a period removed at every depth) are kept as rows, so their
  # removed values can be seen, selected and restored
  r <- load_removed(k$id, pd)
  if (!is.null(r) && nrow(r)) { r <- r[if (k$kind == "temp") variable == "temp" else variable != "temp"]
    gone <- unique(r$TIMESTAMP[!r$TIMESTAMP %in% w$TIMESTAMP]); if (length(gone)) w <- rbind(w, data.table(TIMESTAMP = gone), fill = TRUE) }
  setcolorder(w, c("TIMESTAMP", where$col)); setorder(w, TIMESTAMP)
  setattr(w, "where", where); w
}
wb_params <- function(d) { w <- attr(d, "where"); if (is.null(w)) character() else intersect(w$col, names(d)) }

# The values a quality rule or a site join removed from a site (load_removed()), by the grid's columns.
wb_removed <- function(site, kind, pd) {
  r <- load_removed(site, pd)
  if (is.null(r) || !nrow(r)) return(data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC"), col = character(), old_value = numeric(), rule = character(), logger_id = character()))
  r <- r[if (kind == "temp") variable == "temp" else variable != "temp"]
  r[, .(TIMESTAMP, col = wb_col(variable, depth_m, kind), old_value = value, rule, logger_id)]
}

# State of every cell in a window of records w (see core/workbench.R): flagged from each value's own flag, removed from the
# values the run removed, and, from the manual removals as saved now, "pend" (a saved removal the next run applies) and
# "back" (removed by hand in the last run, its row since deleted). Per column a data.table (state, why, rule, old).
wb_states <- function(w, params, removed, manual = NULL, site = "", sgn = 1) {
  t <- as.numeric(w$TIMESTAMP); n <- nrow(w); where <- attr(w, "where")
  stats::setNames(lapply(params, function(p) {
    st <- rep("", n); why <- rep("", n); rule <- rep("", n); old <- rep(NA_real_, n)
    f <- as.character(w[[paste0("flag|", p)]]); k <- which(!is.na(f) & nzchar(f)); st[k] <- "flag"; why[k] <- gsub(";", ", ", f[k]); rule[k] <- sub(";.*$", "", f[k])
    r <- removed[col == p]
    if (nrow(r)) { m <- match(t, as.numeric(r$TIMESTAMP)); k <- which(!is.na(m) & is.na(w[[p]]))
      st[k] <- "rem"; why[k] <- paste("removed:", r$rule[m[k]]); rule[k] <- r$rule[m[k]]; old[k] <- r$old_value[m[k]] }
    v <- if (!is.null(where)) where[col == p] else NULL
    cov <- if (!is.null(v) && nrow(v)) wb_cover(manual, site, w, p, v$variable[1], v$depth_m[1], sgn) else rep(FALSE, n)
    k <- which(cov & st != "rem" & !is.na(w[[p]])); st[k] <- "pend"; why[k] <- "removal saved; the next run applies it"; rule[k] <- "manual"
    k <- which(!cov & st == "rem" & rule == "manual"); st[k] <- "back"; why[k] <- "its manual removal was deleted; the next run restores the value"
    data.table(state = st, why = why, rule = rule, old = old)
  }), params)
}

# ---- UI ---------------------------------------------------------------------------------------------------------------
# The data view (core/workbench.R) with GT's corrections in the cell bar.
wb_gt_actions <- function() if (!VIEWER) tagList(
  actionButton("wb_act_remove", HTML("&#10005; Remove&hellip; <kbd>R</kbd>"), class = "wb-btn sm", title = "Remove the selected values (corrections over a period). R or Delete"),
  actionButton("wb_act_flag", "Flag only\u2026", class = "wb-btn sm", title = "Keep the selected values and flag them (corrections over a period, action flag)"),
  actionButton("wb_act_restore", HTML("&#8634; Restore&hellip;"), class = "wb-btn sm", title = "Delete the manual removals and flags that cover the selected values"),
  wb_period_button(),
  actionButton("wb_act_ok", "Not a problem\u2026", class = "wb-btn sm", title = "The run flagged values in the selection and they are fine: say why and they leave Needs you (review_decisions.csv)"),
  actionButton("wb_act_trim", "Trim\u2026", class = "wb-btn sm", title = "The record starts after the selected records, or ends at them"),
  actionButton("wb_act_clock", "Clock\u2026", class = "wb-btn sm", title = "Shift the timestamps of a period of one logger (time_corrections.csv)"))

wb_ui <- function(req) {
  pages <- c(gt_flow_pages(sources = FALSE), list(convert_tool(), reference_tool("GT")))   # the Reference page last (core reference.R)
  std_panels <- lapply(Filter(function(x) inherits(x, "ovid_tool"), pages), function(x) tabPanel(x$title, value = x$value, div(class = "wb-std d-tool", x$ui)))
  wb_page("GT", MI, project = if (!VIEWER) projects_switch_ui("proj") else textOutput("wb_project", inline = TRUE), standard_href = "?ui=standard",
    head = tagList(tags$style(HTML(paste0(ovid_css(ovid_tokens(MI)), if (exists("OVID_FILES_CSS")) OVID_FILES_CSS else "", std_css()))),
                   tags$script(HTML(OVID_SHELL_JS)), std_head(), tags$script(HTML(wb_keys_js()))),
    actions = tagList(
      if (!VIEWER) wb_needs_ui(),
      if (!VIEWER) tags$a(class = "wb-btn outline", href = "#", `data-ovid-go` = "wb-import", title = "Folders, the set-up check and the run", HTML("&#10515; Import data")),
      if (!VIEWER) actionButton("run", HTML("&#9656; Run"), class = "wb-btn primary", title = "Save the set-up tables, check them and run the pipeline") else span(class = "wb-badge", "Viewer"),
      downloadButton("wb_export", HTML("&#8599; Export view"), class = "wb-btn", icon = NULL),
      if (file.exists(file.path(APP_DIR, "docs", "GT_reference_guide.html"))) tags$a(class = "wb-btn", href = "docs/GT_reference_guide.html", target = "_blank", title = "Reference guide", "?")),
    nav = tagList(
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Sites"), span(class = "n", textOutput("wb_n_st", inline = TRUE))), uiOutput("wb_tree")),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Depths"), span(class = "n", textOutput("wb_params_title", inline = TRUE))),
                   checkboxGroupInput("wb_params", NULL, choices = character())),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Loggers")), group_by_ui("wbgb", wb = TRUE)),   # core grouping.R
      wb_process_tree("GT", WB_PROCESS),
      wb_tools_tree(pages),
      uiOutput("wb_setup_tree")),
    main = tagList(uiOutput("wb_tabbar", class = "wb-tabs"),
      do.call(tabsetPanel, c(list(id = "nav", type = "hidden", selected = "wb-data",
        tabPanel("Data", value = "wb-data", uiOutput("wb_hm_strip"), wb_data_ui(wb_gt_actions())), tabPanel("Import data", value = "wb-import", wb_import_ui())), std_panels))),
    aside = uiOutput("wb_inspector"),
    status = tagList(uiOutput("runstate", inline = TRUE), uiOutput("wb_status", style = "display:contents")))
}

# ---- server -----------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns: the set-up table editors (E), outputs (outs), project folder (pd), run state
# (rv, mark), the set-up check (pf) and the run settings (cfg).
wb_server <- function(input, output, session, std) {
  q <- wb_query(isolate(session$clientData$url_search))
  ctx <- environment()                                          # what wb_edit.R and wb_project.R share
  E <- std$E; pd <- std$pd; outs <- std$outs
  wsgn <- function() tryCatch(depth_sign(std$cfg()), error = function(e) 1)   # the project's depth convention (R/util.R)
  view <- reactive(input$nav %||% "wb-data")
  set_view <- function(v) updateTabsetPanel(session, "nav", selected = v)
  open_table <- function(id) session$sendCustomMessage("wb-open", WB_OPEN[[id]])
  zone <- function() if (BASE_OFFSET == 0) "UTC" else paste0("UTC", if (BASE_OFFSET < 0) "\u2212" else "+", abs(BASE_OFFSET))
  output$wb_project <- renderText(basename(pd()))

  tables <- reactive({ o <- outs(); req(o); wb_tables(o, pd()) })
  first_key <- function(want) { s <- tables(); if (!nrow(s)) return(NULL)
    if (!is.null(want) && want %in% s$key) return(want)
    if (!is.null(want) && paste0("temp:", want) %in% s$key) return(paste0("temp:", want))
    s[kind == "temp"][order(-records)]$key[1] }
  key <- reactiveVal(NULL); jump <- wb_jump_new()                       # jump: a review-queue item to open (core workbench.R)
  observeEvent(tables(), { k <- isolate(key()); if (is.null(k) || !k %in% tables()$key) key(first_key(if (is.null(k)) q$site else NULL)) })
  observeEvent(input$wb_table, { key(input$wb_table); set_view("wb-data") })
  part <- reactive({ req(key()); k <- wb_split(key()); o <- outs(); r <- if (!is.null(o)) o$ss[site == k$id] else data.table()
    list(kind = k$kind, site = k$id, label = paste(k$id, "\u00b7", if (k$kind == "temp") "temperature" else "soil"), row = r) })
  dat <- reactive({ outs(); d <- wb_load(key(), pd()); validate(need(!is.null(d) && nrow(d) > 0, "No records for this site.")); d })
  params_all <- reactive(wb_params(dat()))
  first_load <- reactiveVal(TRUE)
  observeEvent(params_all(), {
    pa <- params_all(); keep <- intersect(isolate(input$wb_params), pa)
    want <- if (isTRUE(first_load()) && !is.null(q$params)) intersect(strsplit(q$params, ",")[[1]], pa) else keep
    if (!length(want)) want <- head(pa, 4)
    updateCheckboxGroupInput(session, "wb_params", choices = stats::setNames(pa, pa), selected = want)
    d <- dat(); r <- range(d$TIMESTAMP, na.rm = TRUE); cur <- isolate(input$wb_period)
    ok <- length(cur) == 2 && !anyNA(cur) && cur[1] >= as.Date(r[1]) && cur[2] <= as.Date(r[2])
    from <- if (isTRUE(first_load()) && !is.null(q$from)) as.Date(q$from) else if (ok) cur[1] else max(as.Date(r[1]), as.Date(r[2]) - 30)
    to <- if (isTRUE(first_load()) && !is.null(q$to)) as.Date(q$to) else if (ok) cur[2] else as.Date(r[2])
    jt <- wb_jump_take(jump, pa, want, r, from, to); from <- jt$from; to <- jt$to
    if (!identical(jt$want, want)) updateCheckboxGroupInput(session, "wb_params", selected = jt$want)
    updateDateRangeInput(session, "wb_period", start = from, end = to, min = as.Date(r[1]), max = as.Date(r[2]))
    first_load(FALSE)
  })
  observeEvent(input$wb_whole, { r <- range(dat()$TIMESTAMP, na.rm = TRUE); updateDateRangeInput(session, "wb_period", start = as.Date(r[1]), end = as.Date(r[2])) })
  sel_params <- reactive(head(intersect(input$wb_params, params_all()), WB_GRID_PARAMS))
  plot_params <- reactive(head(sel_params(), WB_MAX_PARAMS))
  period <- reactive({ p <- input$wb_period; req(length(p) == 2, !anyNA(p))
    c(as.POSIXct(paste(p[1], "00:00:00"), tz = "UTC"), as.POSIXct(paste(p[2], "23:59:59"), tz = "UTC")) })
  keep_where <- function(x) { setattr(x, "where", attr(dat(), "where")); x }
  per <- reactive({ d <- dat(); pr <- period(); keep_where(d[TIMESTAMP >= pr[1] & TIMESTAMP <= pr[2]]) })
  removed <- reactive({ outs(); p <- part(); wb_removed(p$site, p$kind, pd()) })
  flagruns <- reactive({ d <- per(); rbindlist(lapply(plot_params(), function(p) wb_flag_runs(d$TIMESTAMP, as.character(d[[paste0("flag|", p)]]), p)), fill = TRUE) })
  win <- reactive(per())                                         # the grid lists the period; the chart zooms within it
  grid <- reactive({ w <- win(); ps <- sel_params(); validate(need(length(ps) > 0, "Tick a depth on the left."), need(nrow(w) > 0, "No records in this period."))
    n <- nrow(w); w <- keep_where(head(w, WB_MAX_ROWS)); st <- wb_states(w, ps, removed(), E$manual$data(), part()$site, wsgn())
    list(w = w, n = n, params = ps, states = st, cells = wb_grid_cells(w, ps, st, digits = stats::setNames(rep(2L, length(ps)), ps)),
         sig = paste(key(), paste(ps, collapse = ","), format(w$TIMESTAMP[1], tz = "UTC"), nrow(w), sep = "|")) })

  # a depth-by-time strip above the chart: the daily mean of every sensor as its own band (no interpolation between sensors, no smoothing),
  # for the chosen period, so a whole period can be looked over before picking a depth. Temperature records only.
  output$wb_hm_strip <- renderUI({ p <- tryCatch(part(), error = function(e) NULL); if (is.null(p) || !identical(p$kind, "temp")) return(NULL)
    div(class = "wb-hmstrip", style = "padding:4px 8px 0", plotOutput("wb_hm", height = "130px")) })
  output$wb_hm <- renderPlot({ p <- part(); req(identical(p$kind, "temp")); pr <- period(); d <- load_site(p$site, pd()); req(!is.null(d), nrow(d) > 0)
    d <- d[TIMESTAMP >= pr[1] & TIMESTAMP <= pr[2]]; req(nrow(d) > 0)
    plot_heatmap(d, "", "day", 0.05, 1, FALSE, NULL, numeric(0), FALSE, TRUE, "linear", NULL) + ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(2, 6, 0, 2)) }, bg = MI$bg)
  # tree
  output$wb_n_st <- renderText({ s <- tables(); if (nrow(s)) uniqueN(s$site) else "" })
  output$wb_tree <- renderUI({ s <- tables(); validate(need(nrow(s), "No output yet. Set the folders under Import data and press Run."))
    k <- key(); cur <- if (!is.null(k)) wb_split(k)$id else ""
    tagList(lapply(unique(s$instrument), function(ins) { x <- s[instrument == ins]
      tagList(div(class = "wb-subsec", ins), lapply(unique(x$site), function(st) { y <- x[site == st]; open <- identical(st, cur) && nrow(y) > 1
        tagList(wb_row(st, key = y$key[1], input = "wb_table", n = format(y$records[1], big.mark = ","), on = identical(y$key[1], k) && identical(view(), "wb-data") && !open),
                if (open) lapply(seq_len(nrow(y)), function(i) wb_row(y$label[i], key = y$key[i], input = "wb_table", on = identical(y$key[i], k) && identical(view(), "wb-data"), level = 1))) })) })) })
  output$wb_params_title <- renderText(tryCatch(if (identical(part()$kind, "temp")) "temperature" else "soil", error = function(e) ""))
  output$wb_chips <- renderUI(tagList(lapply(plot_params(), function(p) span(class = "wb-chip", p)),
    if (length(sel_params()) > length(plot_params())) span(class = "wb-chip", style = "opacity:.7", paste0("+", length(sel_params()) - length(plot_params()), " in the table"))))

  # grid and selection
  output$wb_grid <- renderUI({ g <- grid()
    vals <- lapply(g$params, function(p) { v <- suppressWarnings(as.numeric(g$w[[p]])); o <- g$states[[p]]$old; ifelse(is.na(v), o, v) })
    wb_grid(format(g$w$TIMESTAMP, "%Y-%m-%d %H:%M", tz = "UTC"), g$params, g$cells$txt, g$cells$cls, g$sig, corner = paste("time", zone()),
            times = as.numeric(g$w$TIMESTAMP), vals = vals, units = tryCatch(units(), error = function(e) NULL)) })
  output$wb_ginfo <- renderText({ g <- grid(); cc <- cells()
    paste0(if (g$n > nrow(g$w)) sprintf("first %s of %s rows; narrow the period, or zoom the chart and press Use as period", format(nrow(g$w), big.mark = ","), format(g$n, big.mark = ","))
           else paste(format(g$n, big.mark = ","), "rows"),
           if (nrow(cc)) paste0(" \u00b7 ", format(nrow(cc), big.mark = ","), " selected") else "",
           " \u00b7 click, drag, shift-click; a time selects the record, a heading the depth; cmd/ctrl adds; Esc clears") })
  cells <- reactive(wb_sel_cells(input$wb_grid_sel, tryCatch(grid(), error = function(e) NULL)))
  active <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); s <- input$wb_grid_sel
    a <- cc[r == as.integer(s$ar %||% 0) & c == as.integer(s$ac %||% 0)]; if (nrow(a)) a[1] else cc[1] })
  # the values the corrections act on: the selected cells, with the depth, variable and logger of each
  sel <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); w <- grid()$w; where <- attr(w, "where"); i <- match(cc$p, where$col)
    at <- function(a) vapply(seq_len(nrow(cc)), function(j) as.character(w[[paste0(a, "|", cc$p[j])]][cc$r[j]]), "")
    list(p = cc$p, t = w$TIMESTAMP[cc$r], depth = where$depth_m[i], variable = where$variable[i], serial = at("serial"), logger = at("logger_id"), sensor = at("sensor")) })
  clear_sel <- function() session$sendCustomMessage("wb-grid-clear", list())
  cell_info <- function(r, p) { g <- grid(); s <- g$states[[p]]; w <- g$w; at <- function(a) as.character(w[[paste0(a, "|", p)]][r])
    rr <- removed()[col == p & as.numeric(TIMESTAMP) == as.numeric(w$TIMESTAMP[r])]
    list(t = w$TIMESTAMP[r], v = w[[p]][r], state = s$state[r], why = s$why[r], rule = s$rule[r], old = s$old[r],
         serial = at("serial") %|% "", sensor = at("sensor") %|% "", logger = (at("logger_id") %|% "") %|% (if (nrow(rr)) rr$logger_id[1] else ""),
         dsrc = at("depth_source") %|% "", d = 2L) }

  # the chart is drawn in the browser (WB_CHART_JS): it gets the period, thinned when long, and a zoomed-in window at full
  # resolution when it asks (input$wb_view); the grid's selection is highlighted there directly
  # each column's unit (by its variable) and range limits (Quality rules, qc_rules.csv: a row for this site first, then for all)
  units <- reactive({ w <- attr(dat(), "where"); if (is.null(w)) return(NULL)
    u <- unname(c(temp = "\u00b0C", vwc = "m\u00b3/m\u00b3", ec = "mS/cm")[w$variable]); u[is.na(u)] <- ""; stats::setNames(u, w$col) })
  limits <- reactive({ w <- attr(dat(), "where"); r <- tryCatch(as.data.table(E$qc$data()), error = function(e) data.table()); if (is.null(w) || !nrow(r) || !all(c("variable", "min", "max") %in% names(r))) return(NULL)
    ap <- trim(as.character(r$applies_to %||% rep("", nrow(r)))); ap[is.na(ap)] <- ""; site <- tryCatch(wb_split(key())$id, error = function(e) "")
    mine <- vapply(strsplit(ap, "|", fixed = TRUE), function(a) any(trim(a) %in% site), NA)
    r <- rbind(r[mine], r[!mine & ap %in% c("all", "")]); lv <- wb_limits(unique(w$variable), r$variable, r$min, r$max)
    stats::setNames(lv[w$variable], w$col) })
  chart_pend <- function(d, ps) { if (!nrow(d)) return(data.table(p = character(), t = numeric(), v = numeric())); where <- attr(dat(), "where")
    rbindlist(lapply(ps, function(p) { v <- where[col == p]; k <- wb_cover(E$manual$data(), part()$site, d, p, v$variable[1], v$depth_m[1], wsgn()) & !is.na(d[[p]])
      data.table(p = p, t = as.numeric(d$TIMESTAMP[k]), v = d[[p]][k]) })) }
  chart_rem <- reactive({ r <- removed(); r[, .(timestamp = TIMESTAMP, parameter = col, old_value)] })
  chart_base <- reactive({ d <- per(); ps <- plot_params(); req(length(ps) > 0, nrow(d) > 0); pr <- as.numeric(period())
    x <- wb_chart_data(d, ps, chart_rem(), flagruns(), chart_pend(d, ps), WB_CHART_MAX, units = tryCatch(units(), error = function(e) NULL), limits = tryCatch(limits(), error = function(e) NULL))
    c(list(type = "base", key = key(), sig = paste(key(), paste(ps, collapse = ","), pr[1], pr[2], x$n, sep = "|"), period = I(pr), zone = zone()), x) })
  view_d <- wb_chart_server(input, session, chart_base, function(a, z) { d <- per(); k <- as.numeric(d$TIMESTAMP)
    if (sum(k >= a & k <= z) > WB_DETAIL_MAX) return(NULL)
    dd <- keep_where(d[k >= a & k <= z]); ps <- plot_params(); wb_chart_data(dd, ps, chart_rem(), flagruns(), chart_pend(dd, ps), Inf) })
  output$wb_overview <- renderPlot({ d <- dat(); ps <- plot_params(); req(length(ps) > 0)
    wb_overview_plot(d$TIMESTAMP, d[[ps[1]]], period(), view_d(), MI) }, bg = MI$bg)
  state_chip <- function(ci) wb_state_chip(ci$state, ci$rule, pend = "removal saved \u00b7 run to apply")

  # cell bar, inspector, status
  output$wb_cellinfo <- renderUI({ cc <- cells()
    if (!nrow(cc)) return(span(class = "txt", if (VIEWER) "Select cells for their values, states and sources." else "Select cells, records (a time) or depths (a heading), or click or drag on the chart; then correct them."))
    a <- active(); ci <- cell_info(a$r, a$p)
    val <- if (ci$state %in% c("rem", "back")) paste0("\u2014 (was ", wb_fmt(ci$old, ci$d), ")") else wb_fmt(ci$v, ci$d)
    tagList(span(class = "mono", style = paste0("color:", MI$fg4), paste0(LETTERS[min(a$c + 1, 26)], a$r)),
        span(class = "mono", paste(a$p, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"))), span(class = "mono", style = "font-size:14px", val),
        state_chip(ci), span(class = "txt", if (nrow(cc) > 1) paste(format(nrow(cc), big.mark = ","), "cells selected") else if (nzchar(ci$why)) ci$why else ci$logger)) })
  btn <- function(lbl, target, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  correct_btns <- function(state = "") tagList(
    textInput("wb_dec_reason", "Reason (kept with the decision)", value = isolate(input$wb_dec_reason %||% ""), placeholder = "e.g. stuck thermistor", width = "100%"),
    div(class = "wb-inline", if (state %in% c("rem", "pend", "back")) btn("&#8634; Restore&hellip;", "wb_act_restore") else btn("&#10005; Remove&hellip;", "wb_act_remove", "danger"),
        btn("Flag only&hellip;", "wb_act_flag"), btn("Correct a period&hellip;", "wb_act_period"), btn("Not a problem&hellip;", "wb_act_ok")),
    div(class = "wb-inline", btn("Trim&hellip;", "wb_act_trim"), btn("Clock&hellip;", "wb_act_clock")),
    div(class = "wb-inline", actionLink("wb_sensor_open", "Open this logger's sensors and depths"), actionLink("wb_rule_open", "Open the quality rules")))
  correct_box <- function(ci) { if (VIEWER) return(NULL)
    what <- switch(ci$state %||% "",
      rem = p(class = "wb-p", if (identical(ci$rule, "manual")) "Removed by hand (corrections over a period)." else if (identical(ci$rule, "site_overlap")) "Two loggers of this site covered this depth and time; the later deployment was kept." else
              paste0("Removed by a quality rule (", ci$rule, "). It comes back when the rule is changed on Quality rules.")),
      flag = p(class = "wb-p", "Kept and flagged."), pend = p(class = "wb-p", "A saved removal covers this value; the next run removes it."),
      back = p(class = "wb-p", "Its removal was deleted; the next run brings the value back."), NULL)
    div(class = "wb-hr", span(class = "wb-eb", "Decide"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", what, correct_btns(ci$state))) }
  site_box <- function() { r <- part()$row; if (!nrow(r)) return(NULL)
    div(class = "wb-hr", span(class = "wb-eb", paste("Site \u00b7", r$site[1])), div(style = "margin-top:6px",
      wb_kv("instrument", r$instrument[1]), wb_kv("loggers", r$loggers[1]), wb_kv("depths", paste0(r$depths[1], if (isTRUE(r$assumed_depths[1] > 0)) paste0(" (", r$assumed_depths[1], " assumed)") else "")),
      wb_kv("span", paste(substr(r$first[1], 1, 10), "\u2192", substr(r$last[1], 1, 10))), wb_kv("values", format(r$values[1], big.mark = ",")),
      wb_kv("flagged", format(r$flagged[1], big.mark = ",")), if (nzchar(r$landcover[1] %|% "")) wb_kv("landcover", r$landcover[1]))) }
  wb_import_aside <- function() { o <- outs(); m <- if (!is.null(o)) o$manifest else NULL; v <- function(k) if (!is.null(m) && nrow(m)) m$value[match(k, m$key)] else NA
    steps <- c(`Set-up` = "the set-up tables, and sites and sensors from the registry on the first run", Files = "every file, grouped by download with its exports",
               `Read and stitch` = "each logger's downloads in one record, on the base clock; repeats kept once", `Sensors and depths` = "a site and a depth for every sensor",
               `Quality rules` = "limits, spikes, stuck sensors, exposed sensors, manual removals; removed values kept in the audit", Sites = "one record per site, loggers that replaced each other joined",
               Summaries = "daily means, annual statistics, thaw depth, freeze and thaw", Audit = "every value read accounted for")
    div(class = "wb-insp", div(span(class = "wb-eb", "Pipeline"), div(style = "margin-top:8px",
        lapply(seq_along(steps), function(i) div(class = "wb-pstep", span(class = "n", i), div(div(names(steps)[i]), div(class = "s", steps[[i]])))))),
      div(class = "wb-hr", span(class = "wb-eb", "Last run"), div(style = "margin-top:6px",
        if (is.null(o)) p(class = "wb-p", "Not run yet.") else tagList(wb_kv("when", format(o$when, "%Y-%m-%d %H:%M")), wb_kv("files read", num(sum(o$files$use %in% TRUE))),
          wb_kv("sites", nrow(o$ss)), wb_kv("problems", nrow(o$problems)),
          if (!is.na(v("loggers_reused"))) wb_kv("loggers reused", paste(v("loggers_reused"), "of", v("loggers_checked") %|% "?"))))),
      if (length(std$rv$log)) div(class = "wb-hr", span(class = "wb-eb", "Log of this run"), tags$pre(class = "wb-log", paste(tail(std$rv$log, 40), collapse = "\n"))),
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Removed values are kept with their old values in the audit trail. Raw files are never written.")) }

  output$wb_inspector <- renderUI({ v <- view()
    if (identical(v, "wb-import")) return(wb_import_aside())
    if (!identical(v, "wb-data")) return(NULL)
    cc <- cells(); p <- tryCatch(part(), error = function(e) NULL)
    if (is.null(p)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet"), wb_empty_acts("Files")), p(class = "wb-p", "Set the folders under Import data and press Run.")))
    if (!nrow(cc)) {
      g <- tryCatch(grid(), error = function(e) NULL)
      cnt <- function(k) if (is.null(g)) 0 else sum(vapply(g$states, function(x) sum(x$state == k), 0))
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No cell selected"),
        div(class = "sub", "Click or drag in the grid; a time selects the record, a heading the depth. On the chart, click a point or drag across it.")),
        if (!is.null(g)) div(class = "wb-facts", wb_fact("rows", format(g$n, big.mark = ","), "in the period"), wb_fact("removed", cnt("rem"), "cells shown"),
                             wb_fact("flagged", cnt("flag"), "cells shown"), wb_fact("pending", cnt("pend") + cnt("back"), "saved, not run")),
        site_box()))
    }
    if (nrow(cc) > 1) {
      g <- grid(); tt <- range(g$w$TIMESTAMP[cc$r])
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2(paste(format(nrow(cc), big.mark = ","), "values")),
          div(class = "sub", paste(format(tt[1], "%Y-%m-%d %H:%M", tz = "UTC"), "\u2013", format(tt[2], "%Y-%m-%d %H:%M", tz = "UTC"), "\u00b7", uniqueN(cc$r), "records"))),
        wb_sel_table(cc, g),
        if (!VIEWER) div(class = "wb-hr", span(class = "wb-eb", "Decide"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", correct_btns())),
        div(class = "wb-inline", actionLink("wb_zoom_sel", "Zoom the chart to the selection")),
        site_box()))
    }
    a <- cc[1]; ci <- cell_info(a$r, a$p)
    qc_step <- switch(ci$state %||% "",
      rem = wb_step(paste("Removed by", ci$rule), ci$why, "e"), back = wb_step(paste("Removed by", ci$rule, "in the last run"), ci$why, "w"),
      flag = wb_step(paste("Flagged:", ci$rule), ci$why, "w"), pend = wb_step("Removal saved", ci$why, "w"), wb_step("No rule removed or flagged it", "quality rules and manual removals"))
    div(class = "wb-insp",
      div(span(class = "wb-eb", "Selected cell"), tags$h2(a$p), div(class = "sub", paste(p$label, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"), zone()))),
      div(style = "display:flex;align-items:baseline;gap:10px;flex-wrap:wrap", span(style = "font-family:var(--wb-mono);font-size:22px",
          if (ci$state %in% c("rem", "back")) "\u2014" else wb_fmt(ci$v, ci$d)), state_chip(ci)),
      div(span(class = "wb-eb", "Lineage"), div(style = "margin-top:6px",
        wb_step(paste("Read by", ci$logger %|% "its logger"), paste0(if (nzchar(ci$sensor)) paste("sensor", ci$sensor) else "", if (ci$state %in% c("rem", "back")) paste0(" \u00b7 ", wb_fmt(ci$old, ci$d)) else "")),
        wb_step(paste("At", a$p), if (nzchar(ci$dsrc)) paste("depth from", ci$dsrc) else "depth as set on Sensors and depths", if (ci$dsrc %in% c("default", "template")) "w" else ""),
        wb_step(paste("Joined into", p$site), "loggers that replaced each other at this site, the later deployment kept where they overlap"),
        wb_period_steps(E$period$data(), c(p$site, ci$logger, ci$sensor), c(a$p, ci$sensor), ci$t), qc_step)),
      correct_box(ci), site_box())
  })
  observeEvent(input$wb_zoom_sel, { cc <- cells(); req(nrow(cc)); g <- grid(); tt <- as.numeric(range(g$w$TIMESTAMP[cc$r]))   # zoom the chart to it
    pad <- max(diff(tt) * 0.15, 6 * stats::median(diff(as.numeric(g$w$TIMESTAMP)), na.rm = TRUE), 3600, na.rm = TRUE)
    session$sendCustomMessage("wb-chart-view", list(x0 = tt[1] - pad, x1 = tt[2] + pad)) })

  output$wb_status <- renderUI({ o <- outs(); cc <- cells(); g <- tryCatch(grid(), error = function(e) NULL)
    np <- if (!is.null(o)) nrow(o$problems) else 0L
    rec <- if (!is.null(o) && nrow(o$recon) && "reconciles" %in% names(o$recon)) all(as.logical(o$recon$reconciles), na.rm = TRUE) else NA
    last <- if (!is.null(o) && nrow(o$runs)) tail(o$runs, 1) else NULL
    mid <- if (!identical(view(), "wb-data")) NULL else if (nrow(cc) > 1 && !is.null(g)) { v <- unlist(lapply(seq_len(nrow(cc)), function(i) g$w[[cc$p[i]]][cc$r[i]])); v <- v[is.finite(v)]
      tagList(span(paste(format(nrow(cc), big.mark = ","), "selected")), if (length(v)) tagList(span(paste("mean", signif(mean(v), 5))), span(paste("min", signif(min(v), 5))), span(paste("max", signif(max(v), 5))))) }
      else if (!is.null(g)) span(paste(format(g$n, big.mark = ","), "rows in the period"))
    tagList(span(style = paste0("color:", MI$fg), HTML(if (np) paste0('<a href="#" data-ovid-go="Preflight" style="color:inherit"><span style="color:', MI$gold, '">&#9650;</span> ', np, " problems</a>") else paste0('<span style="color:', MI$green, '">&#10003;</span> No problems'))),
            div(class = "wb-fill mono", style = "display:flex;justify-content:center;gap:16px", mid),
            span(style = "display:flex;align-items:center;gap:7px", HTML(paste0('<span style="width:7px;height:7px;border-radius:50%;background:', if (isTRUE(rec)) MI$green else MI$gold, '"></span>')),
                 paste0(if (isTRUE(rec)) "Reconciled" else "Not checked", if (!is.null(last) && "run_id" %in% names(last)) paste0(" \u00b7 run ", last$run_id) else "")),
            span(class = "mono", paste0(zone(), " \u00b7 GT ", GT_VERSION, " \u00b7 workbench ", OVID_WORKBENCH_VERSION)))
  })
  output$wb_export <- downloadHandler(function() paste0("gt_", safe_name(part()$site), "_", part()$kind, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) { w <- win(); ps <- sel_params(); fwrite(w[, c("TIMESTAMP", ps), with = FALSE], file, dateTimeAs = "write.csv") })

  wb_project_server(input, output, session, ctx)
  wb_edit_server(input, output, session, ctx)
  wbgb_deps <- reactive({ s <- live_table(cfg_path(pd(), "sensors")); if (!nrow(s) || !all(c("serial", "site") %in% names(s))) NULL else unique(s[nzchar(site), .(serial = as.character(serial), from = as.character(start %||% ""), to = as.character(end %||% ""), name = site)]) })
  group_by_server("wbgb", reactive({ outs(); f <- tryCatch(fread(file.path(pd(), "output", "audit", "files.csv"), colClasses = "character"), error = function(e) NULL)
      if (is.null(f) || !nrow(f)) NULL else f[nzchar(serial), .(serial, file = rel, first = first_ts, last = last_ts)] }), pd, wbgb_deps,
    open = function(r) { k <- { s <- wbgb_deps(); site <- if (!is.null(s) && nrow(s[serial == r$serial])) s[serial == r$serial]$name[1] else r$serial; tb <- tables(); x <- tb[tb$site == site & tb$kind == "temp"]; if (nrow(x)) x$key[1] else NA_character_ }; if (is.na(k)) return(showNotification(paste(r$serial, "has no table in this view yet."), type = "message"))
      wb_jump_to(session, input, key, jump, set_view, k, NULL, format(r$first, "%Y-%m-%d"), format(r$last, "%Y-%m-%d")) })   # core grouping.R
  observe({ v <- tryCatch(wb_split(key())$id, error = function(e) NULL); pr <- input$wb_period                 # what this view shows, for the switch (core shell.R)
    ovid_ctx(session, "site", v, input$wb_params, if (length(pr) == 2) pr[1], if (length(pr) == 2) pr[2], tryCatch(if (!is.null(sel()) && length(sel()$t)) format(min(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL), tryCatch(if (!is.null(sel()) && length(sel()$t)) format(max(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL)) })
  wb_needs_server(output, reactive(wb_needs_items(if (is.function(std$needs$check)) gt_needs_check(std$needs$check(), pd()) else NULL, std$review$items())))   # core workbench.R; loggers GT places (R/metadata.R) are not listed
  qpreset <- reactiveVal(NULL)
  wb_period_server(input, output, session, E$period, sel, reactive(wb_split(key())$id), params_all, function() open_table("period"), preset = qpreset)   # core workbench.R
  queue_rows <- reactive({ o <- outs(); f <- o$flags; if (is.null(f) || !nrow(f)) return(NULL)
      f[, .(where = site, key = paste0(ifelse(variable == "temp", "temp:", "soil:"), site), parameter = channel, start = as.character(start), end = as.character(end), records, check, detail, qc_code = if ("qc_code" %in% names(f)) qc_code else NA)] })
  # Not a problem on a selection: the flagged stretches (Needs you items) of this site and these sensors that overlap the selected times are
  # marked done with the reason; the values are not touched. Nothing flagged there: nothing to mark, and it says so.
  hit_items <- reactive({ s <- sel(); req(s); x <- queue_rows(); if (is.null(x) || !nrow(x)) return(data.table())
    t0 <- min(s$t); t1 <- max(s$t); sn <- unique(s$sensor[nzchar(s$sensor)]); st <- part()$site
    y <- x[where == st & (!length(sn) | parameter %in% sn) & as.POSIXct(start, tz = "UTC") <= t1 & as.POSIXct(end, tz = "UTC") >= t0]
    if (nrow(y)) y[, id := wb_queue_id(where, parameter, start, end, check)]; y })
  observeEvent(input$wb_act_ok, { if (VIEWER) return(); s <- sel()
    if (is.null(s) || !length(s$t)) return(showNotification("Select cells in the grid, or drag across the chart, first.", type = "warning"))
    y <- hit_items(); if (!nrow(y)) return(showNotification("The last run flagged nothing in this selection, so there is nothing to mark as not a problem. Use Remove or Flag only to act on the values.", type = "message", duration = 8))
    showModal(modalDialog(title = "Not a problem", size = "l", easyClose = TRUE,
      div(class = "wb-p", style = "color:var(--wb-fg2)", paste0(nrow(y), if (nrow(y) == 1) " flagged stretch" else " flagged stretches", " in the selection (", paste(unique(y$check), collapse = ", "), "). They leave Needs you; the values are not changed.")),
      div(class = "wb-form2", textInput("wb_ok_reason", "Why it is not a problem (kept with the decision)", value = isolate(input$wb_dec_reason %||% ""), width = "100%"), textInput("wb_ok_author", "Initials", width = "100%")),
      uiOutput("wb_ok_err"), footer = tagList(modalButton("Cancel"), actionButton("wb_ok_do", "Mark not a problem", class = "btn-primary")))) })
  observeEvent(input$wb_ok_do, { y <- hit_items(); req(nrow(y)); why <- trim(input$wb_ok_reason %||% "")
    if (!nzchar(why)) { output$wb_ok_err <- renderUI(div(class = "text-danger", style = "font-size:12px", "Say why, in a few words: it goes with the decision.")); return() }
    new <- data.table(id = y$id, decision = "not a problem", note = paste0(why, " (", y$where, " \u00b7 ", y$parameter, " \u00b7 ", y$check, " \u00b7 ", y$start, ")"), author = trim(input$wb_ok_author %||% ""), at = format(Sys.time(), "%Y-%m-%d %H:%M"))
    review_write(file.path(pd(), "config", REVIEW_FILE), y$id, new); removeModal(); clear_sel()   # core config.R
    showNotification(sprintf("%d flagged stretch%s marked not a problem.", nrow(y), if (nrow(y) == 1) "" else "es"), type = "message") })
  wb_review_handlers(std$review, function(r) wb_jump_to(session, input, key, jump, set_view, r$key, NULL, r$start, r$end), qpreset)   # Needs you (core workbench.R)
  invisible(ctx)
}
