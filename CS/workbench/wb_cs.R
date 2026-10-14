# CS/workbench/wb_cs.R  --  The CS Workbench (preview): the record charted over a spreadsheet grid, with the selected
# cells, where they came from and how to correct them beside it, and every page of the Standard interface around it.
# Part of Ovid Core // CS. Sourced by app.R after cs_pipeline.R and core/shell.R.
# A Workbench session runs the Standard server too (app.R, server(embedded = TRUE)) and shows its pages in the same
# hidden tab set, so every Standard process (import, run, set-up tables, raw files, clock checks, quality rules, station
# naming, column names, CR10X and program matching, precipitation, combined records, statistics, reports, history...)
# works here through the same code. This file adds the data view; wb_edit.R the corrections made from a selection;
# wb_project.R the Import view, the trees and the tabs.

source(file.path(CORE_DIR, "workbench.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_edit.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_project.R"), local = TRUE)

WB_META <- c("station", "stream", "TIMESTAMP", "logger_id", "RECORD", "source_file")
WB_MAX_ROWS <- 5000L                                   # rows the grid shows at once; narrow the period for more
WB_CHART_MAX <- 8000L                                  # records per parameter the chart gets for the period; more are thinned
WB_DETAIL_MAX <- 40000L                                # records it gets at full resolution for a zoomed-in window
WB_GRID_PARAMS <- 24L                                  # parameters in the grid
WB_MAX_PARAMS <- 6L                                    # of those, charted

wb_split <- function(key) { i <- regexpr("__", key, fixed = TRUE)
  if (i < 0) list(station = key, stream = NULL) else list(station = substr(key, 1, i - 1), stream = substring(key, i + 2)) }

# Values a quality rule removed from one station table (output/qc/<station>__<stream>_removed.csv).
wb_removed <- function(pd, key) {
  f <- file.path(path.expand(pd), "output", "qc", paste0(safe_name(key), "_removed.csv"))
  empty <- data.table(timestamp = as.POSIXct(character(), tz = "UTC"), parameter = character(), rule = character(), old_value = numeric(), reason = character())
  if (!file.exists(f)) return(empty)
  x <- tryCatch(fread(f, colClasses = "character", na.strings = ""), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(empty)
  # parse_dt (core/util.R), not as.POSIXct(): that picks one format for the whole column and drops every time of day
  # when one element reads as a date only
  x[, timestamp := parse_dt(timestamp)]; x[, old_value := suppressWarnings(as.numeric(old_value))]
  x[, .(timestamp, parameter, rule, old_value, reason)]
}

# Flagged periods of one station table (output/audit/flags.csv), with start and end as times.
wb_flags <- function(flags, station, stream) {
  if (is.null(flags) || !nrow(flags)) return(data.table())
  st <- station; sm <- stream
  x <- flags[station == st & stream == sm]
  if (!nrow(x)) return(x)
  x[, `:=`(s = parse_dt(start), e = parse_dt(end))][]
}

# The quality rule (qc_rules.csv) that covers a parameter at a station: a row with applies_to naming the station or
# logger wins over the general row, as in apply_qc() (R/qc.R).
wb_rule_for <- function(rules, param, station, logger) {
  if (is.null(rules) || !nrow(rules)) return(NULL)
  rules <- as.data.table(rules)
  hit <- which(vapply(rules$pattern, function(p) isTRUE(tryCatch(grepl(p, param, ignore.case = TRUE, perl = TRUE), error = function(e) FALSE)), NA))
  if (!length(hit)) return(NULL)
  ap <- if ("applies_to" %in% names(rules)) trim(rules$applies_to[hit]) else rep("", length(hit)); ap[is.na(ap)] <- ""
  here <- vapply(ap, function(a) nzchar(a) && any(trim(strsplit(a, "|", fixed = TRUE)[[1]]) %in% c(station, logger)), NA)
  i <- if (any(here)) hit[here][1] else hit[!nzchar(ap)][1]
  if (is.na(i)) NULL else rules[i]
}

# Label of a station's clock from its utc_offset_hours (Station details); "logger time" when none is given.
wb_zone <- function(off) if (length(off) != 1 || is.na(off)) "logger time" else if (off == 0) "UTC" else
  paste0("UTC", if (off < 0) "\u2212" else "+", format(abs(off)))

# State of every cell in a window of records: "" (passed), "flag" (inside a flagged period), "rem" (removed by a rule),
# and, from the manual removals as saved now: "pend" (a saved removal the next run applies) and "back" (removed by hand
# in the last run, its row since deleted). Returns per-parameter data.tables (state, why, rule, old) the size of the window.
wb_states <- function(w, params, removed, fl, manual = NULL, station = "", stream = "") {
  t <- as.numeric(w$TIMESTAMP)
  stats::setNames(lapply(params, function(p) {
    st <- rep("", nrow(w)); why <- rep("", nrow(w)); rule <- rep("", nrow(w)); old <- rep(NA_real_, nrow(w))
    if (nrow(fl)) { f <- fl[parameter == p]
      for (i in seq_len(nrow(f))) { k <- which(t >= as.numeric(f$s[i]) & t <= as.numeric(f$e[i]))
        st[k] <- "flag"; why[k] <- f$detail[i]; rule[k] <- paste(f$check[i], if ("rule_id" %in% names(f)) paste0("(", f$rule_id[i], ")") else "") } }
    if (nrow(removed)) { r <- removed[parameter == p]
      if (nrow(r)) { m <- match(t, as.numeric(r$timestamp)); k <- which(!is.na(m))
        st[k] <- "rem"; why[k] <- r$reason[m[k]]; rule[k] <- r$rule[m[k]]; old[k] <- r$old_value[m[k]] } }
    if (!is.null(manual) && nrow(manual)) {
      cov <- wb_manual_cover(manual, station, stream, rep(p, nrow(w)), w$TIMESTAMP)
      k <- which(cov & st != "rem" & !is.na(w[[p]])); st[k] <- "pend"; why[k] <- "removal saved; the next run applies it"; rule[k] <- "manual"
      k <- which(!cov & st == "rem" & rule == "manual"); st[k] <- "back"; why[k] <- "manual removal deleted; the next run restores the value"
    }
    data.table(state = st, why = why, rule = rule, old = old)
  }), params)
}

# ---- UI ---------------------------------------------------------------------------------------------------------------
# The data view (core/workbench.R) with CS's corrections in the cell bar.
wb_cs_actions <- function() if (!VIEWER) tagList(
  actionButton("wb_act_remove", HTML("&#10005; Remove&hellip; <kbd>R</kbd>"), class = "wb-btn sm", title = "Remove the selected values (corrections over a period). R or Delete"),
  actionButton("wb_act_restore", HTML("&#8634; Restore&hellip;"), class = "wb-btn sm", title = "Delete the manual removals that cover the selected values"),
  actionButton("wb_act_cal", "Calibrate\u2026", class = "wb-btn sm", title = "Multiplier and offset for a period (corrections over a period)"),
  wb_period_button(),
  actionButton("wb_act_clock", "Clock\u2026", class = "wb-btn sm", title = "Shift or remove the records of a period (time_corrections.csv)"),
  actionButton("wb_act_note", "Note\u2026", class = "wb-btn sm", title = "Field log entry (field_log.csv)"))

wb_ui <- function(req) {
  pages <- c(cs_flow_pages(sources = FALSE), list(convert_tool(), reference_tool("CS")))   # the Reference page last (core reference.R)
  std_panels <- lapply(Filter(function(x) inherits(x, "ovid_tool"), pages), function(x) tabPanel(x$title, value = x$value, div(class = "wb-std d-tool", x$ui)))
  wb_page("CS", GB, project = if (!VIEWER) projects_switch_ui("proj") else textOutput("wb_project", inline = TRUE), standard_href = "?ui=standard",
    head = tagList(tags$style(HTML(paste0(ovid_css(ovid_tokens(GB)), if (exists("OVID_FILES_CSS")) OVID_FILES_CSS else "", std_css()))),
                   tags$script(HTML(OVID_SHELL_JS)), std_head(), tags$script(HTML(wb_keys_js()))),
    actions = tagList(
      if (!VIEWER) wb_needs_ui(),
      if (!VIEWER) tags$a(class = "wb-btn outline", href = "#", `data-ovid-go` = "wb-import", title = "Sources, run settings and the set-up check", HTML("&#10515; Import data")),
      if (!VIEWER) actionButton("run", HTML("&#9656; Run"), class = "wb-btn primary", title = "Save the set-up tables, check them and run the pipeline") else span(class = "wb-badge", "Viewer"),
      downloadButton("wb_export", HTML("&#8599; Export view"), class = "wb-btn", icon = NULL),
      if (file.exists(file.path(APP_DIR, "docs", "CS_reference_guide.html"))) tags$a(class = "wb-btn", href = "docs/CS_reference_guide.html", target = "_blank", title = "Reference guide", "?")),
    nav = tagList(
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Stations"), span(class = "n", textOutput("wb_n_st", inline = TRUE))), uiOutput("wb_tree")),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Parameters"), span(class = "n", textOutput("wb_params_title", inline = TRUE))),
                   checkboxGroupInput("wb_params", NULL, choices = character())),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Loggers")), group_by_ui("wbgb", wb = TRUE)),   # core grouping.R
      wb_process_tree("CS", WB_PROCESS),
      wb_tools_tree(pages),
      uiOutput("wb_setup_tree")),
    main = tagList(uiOutput("wb_tabbar", class = "wb-tabs"),
      do.call(tabsetPanel, c(list(id = "nav", type = "hidden", selected = "wb-data",
        tabPanel("Data", value = "wb-data", wb_data_ui(wb_cs_actions())), tabPanel("Import data", value = "wb-import", wb_import_ui())), std_panels))),
    aside = uiOutput("wb_inspector"),
    status = tagList(uiOutput("runstate", inline = TRUE), uiOutput("wb_status", style = "display:contents")))
}

# ---- server -----------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns: the set-up table editors (E), outputs (outs), project folder (pd), run
# state (rv, mark) and the set-up check (pf).
wb_server <- function(input, output, session, std) {
  q <- wb_query(isolate(session$clientData$url_search))
  ctx <- environment()                                          # what wb_edit.R and wb_project.R share
  E <- std$E; pd <- std$pd; outs <- std$outs
  view <- reactive(input$nav %||% "wb-data")
  set_view <- function(v) updateTabsetPanel(session, "nav", selected = v)
  open_table <- function(id) session$sendCustomMessage("wb-open", WB_OPEN[[id]])
  output$wb_project <- renderText(basename(pd()))

  tables <- reactive({ o <- outs(); req(o); s <- o$stations; if (!nrow(s)) return(data.table())
    copy(s)[, key := paste0(station, "__", stream)][order(station, -records)] })
  first_key <- function(st) { s <- tables(); if (!nrow(s)) return(NULL)
    if (!is.null(st) && st %in% s$key) return(st)
    if (!is.null(st) && st %in% s$station) return(s[station == st][order(-records)]$key[1])
    s[order(-records)]$key[1] }
  key <- reactiveVal(NULL); jump <- wb_jump_new()                       # jump: a review-queue item to open (core workbench.R)
  observeEvent(tables(), { k <- isolate(key()); if (is.null(k) || !k %in% tables()$key) key(first_key(if (is.null(k)) q$station else NULL)) })
  observeEvent(input$wb_table, { key(input$wb_table); set_view("wb-data") })
  part <- reactive({ req(key()); wb_split(key()) })
  dat <- reactive({ outs(); p <- part(); d <- tryCatch(load_station(p$station, p$stream, pd()), error = function(e) NULL)
    validate(need(!is.null(d) && nrow(d) > 0, "No station file for this table: run the pipeline to rebuild the output.")); d })
  params_all <- reactive({ d <- dat(); setdiff(names(d)[vapply(d, is.numeric, NA)], WB_META) })
  first_load <- reactiveVal(TRUE)
  observeEvent(params_all(), {
    pa <- params_all(); keep <- intersect(isolate(input$wb_params), pa)
    want <- if (isTRUE(first_load()) && !is.null(q$params)) intersect(strsplit(q$params, ",")[[1]], pa) else keep
    if (!length(want)) want <- head(pa[!grepl("batt|panel|ptemp|^RECORD$", pa, ignore.case = TRUE)], 2)
    updateCheckboxGroupInput(session, "wb_params", choices = pa, selected = want)
    d <- dat(); r <- range(d$TIMESTAMP, na.rm = TRUE); cur <- isolate(input$wb_period)
    ok <- length(cur) == 2 && !anyNA(cur) && cur[1] >= as.Date(r[1]) && cur[2] <= as.Date(r[2])
    from <- if (isTRUE(first_load()) && !is.null(q$from)) as.Date(q$from) else if (ok) cur[1] else as.Date(r[2]) - 21
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
  per <- reactive({ d <- dat(); pr <- period(); d[TIMESTAMP >= pr[1] & TIMESTAMP <= pr[2]] })
  removed <- reactive({ outs(); wb_removed(pd(), key()) })
  zone <- reactive({ live_stamp(file.path(pd(), "config", "station_info.csv")); tryCatch(wb_zone(station_offsets(pd(), part()$station)), error = function(e) "logger time") })   # offsets edited on any page
  fl <- reactive({ p <- part(); wb_flags(outs()$flags, p$station, p$stream) })
  win <- reactive(per())                                         # the grid lists the period; the chart zooms within it
  grid <- reactive({ w <- win(); ps <- sel_params(); validate(need(length(ps) > 0, "Tick a parameter on the left."), need(nrow(w) > 0, "No records in this period."))
    n <- nrow(w); w <- head(w, WB_MAX_ROWS); p <- part(); st <- wb_states(w, ps, removed(), fl(), E$manual$data(), p$station, p$stream)
    list(w = w, n = n, params = ps, states = st, cells = wb_grid_cells(w, ps, st),
         sig = paste(key(), paste(ps, collapse = ","), format(w$TIMESTAMP[1], tz = "UTC"), nrow(w), sep = "|")) })

  # tree
  output$wb_n_st <- renderText({ s <- tables(); if (nrow(s)) uniqueN(s$station) else "" })
  output$wb_tree <- renderUI({ s <- tables(); validate(need(nrow(s), "No output yet. Set the sources under Import data and press Run."))
    k <- key(); cur <- if (!is.null(k)) wb_split(k)$station else ""
    tagList(lapply(unique(s$station), function(st) { x <- s[station == st]; open <- st == cur
      tagList(wb_row(st, key = x$key[1], input = "wb_table", n = if (!open) nrow(x) else "", on = FALSE),
              if (open) lapply(seq_len(nrow(x)), function(i) wb_row(x$stream[i], key = x$key[i], input = "wb_table",
                n = format(x$records[i], big.mark = ","), on = identical(x$key[i], k) && identical(view(), "wb-data"), level = 1))) })) })
  output$wb_params_title <- renderText(part()$stream %||% "")
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
           " \u00b7 click, drag, shift-click; a time selects the record, a heading the parameter; cmd/ctrl adds; Esc clears") })
  cells <- reactive(wb_sel_cells(input$wb_grid_sel, tryCatch(grid(), error = function(e) NULL)))
  active <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); s <- input$wb_grid_sel
    a <- cc[r == as.integer(s$ar %||% 0) & c == as.integer(s$ac %||% 0)]; if (nrow(a)) a[1] else cc[1] })
  # the values the corrections act on: the selected cells (in the grid, or dragged or clicked on the chart)
  sel <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); list(kind = "cells", p = cc$p, t = grid()$w$TIMESTAMP[cc$r]) })
  clear_sel <- function() session$sendCustomMessage("wb-grid-clear", list())
  cell_info <- function(r, p) { g <- grid(); s <- g$states[[p]]; w <- g$w
    list(t = w$TIMESTAMP[r], v = w[[p]][r], state = s$state[r], why = s$why[r], rule = s$rule[r], old = s$old[r],
         src = if ("source_file" %in% names(w)) w$source_file[r] else "", logger = if ("logger_id" %in% names(w)) w$logger_id[r] else "",
         d = wb_digits(c(w[[p]], s$old))) }
  rules <- reactive({ d <- E$qc$data(); if (is.null(d)) data.table() else as.data.table(d) })
  # each parameter's unit (parameter_info.csv, the Parameter guide) and range limits (its quality rule), for the grid and chart
  units <- reactive({ outs(); pi <- tryCatch(fread(file.path(pd(), "config", "parameter_info.csv"), colClasses = "character", na.strings = NULL), error = function(e) NULL)
    if (is.null(pi) || !all(c("pattern", "unit") %in% names(pi))) pi <- tryCatch(default_parameter_info(), error = function(e) NULL)
    ps <- params_all(); if (is.null(pi) || !length(ps)) return(NULL); wb_match(ps, pi$pattern, pi$unit, regex = TRUE) })
  limits <- reactive({ ps <- params_all(); p0 <- part(); r <- rules(); num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
    stats::setNames(lapply(ps, function(p) { x <- wb_rule_for(r, p, p0$station, ""); c(lo = num(x$min[1]), hi = num(x$max[1])) }), ps) })
  sel_info <- reactive({ cc <- cells(); req(nrow(cc) == 1); ci <- cell_info(cc$r[1], cc$p[1])
    c(ci, list(p = cc$p[1], qc_rule = wb_rule_for(rules(), cc$p[1], part()$station, ci$logger))) })
  # the chart is drawn in the browser (WB_CHART_JS): it gets the period as a message, thinned when long, and a
  # zoomed-in window at full resolution when it asks (input$wb_view); the grid's selection is highlighted there directly
  chart_pend <- function(d, ps, p0) { if (!nrow(d)) return(data.table(p = character(), t = numeric(), v = numeric()))
    x <- rbindlist(lapply(ps, function(p) data.table(p = p, t = as.numeric(d$TIMESTAMP), v = d[[p]])))[!is.na(v)]
    x[wb_manual_cover(E$manual$data(), p0$station, p0$stream, x$p, as.POSIXct(x$t, origin = "1970-01-01", tz = "UTC"))] }
  chart_base <- reactive({ d <- per(); ps <- plot_params(); req(length(ps) > 0, nrow(d) > 0); p0 <- part(); pr <- as.numeric(period())
    x <- wb_chart_data(d, ps, removed(), fl(), chart_pend(d, ps, p0), WB_CHART_MAX, units = tryCatch(units(), error = function(e) NULL), limits = tryCatch(limits(), error = function(e) NULL))
    c(list(type = "base", key = key(), sig = paste(key(), paste(ps, collapse = ","), pr[1], pr[2], x$n, sep = "|"), period = I(pr), zone = zone()), x) })
  view_d <- wb_chart_server(input, session, chart_base, function(a, z) { d <- per(); k <- as.numeric(d$TIMESTAMP)
    if (sum(k >= a & k <= z) > WB_DETAIL_MAX) return(NULL)
    dd <- d[k >= a & k <= z]; ps <- plot_params(); wb_chart_data(dd, ps, removed(), fl(), chart_pend(dd, ps, part()), Inf) })

  # charts
  output$wb_overview <- renderPlot({ req(wb_overview_fits()); d <- dat(); ps <- plot_params(); req(length(ps) > 0)
    wb_overview_plot(d$TIMESTAMP, d[[ps[1]]], period(), view_d(), GB) }, bg = GB$bg)
  state_chip <- function(ci) wb_state_chip(ci$state, ci$rule, pend = "removal saved \u00b7 run to apply")

  # cell bar, inspector, status
  output$wb_cellinfo <- renderUI({ cc <- cells()
    if (!nrow(cc)) return(span(class = "txt", if (VIEWER) "Select cells for their values, states and sources." else "Select cells, records (a time) or parameters (a heading), or click or drag on the chart; then correct them."))
    a <- active(); ci <- cell_info(a$r, a$p)
    val <- if (ci$state %in% c("rem", "back")) paste0("\u2014 (was ", wb_fmt(ci$old, ci$d), ")") else wb_fmt(ci$v, ci$d)
    tagList(span(class = "mono", style = paste0("color:", GB$fg4), paste0(LETTERS[min(a$c + 1, 26)], a$r)),
        span(class = "mono", paste(a$p, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"))), span(class = "mono", style = "font-size:14px", val),
        state_chip(ci), span(class = "txt", if (nrow(cc) > 1) paste(format(nrow(cc), big.mark = ","), "cells selected") else if (nzchar(ci$why)) ci$why else basename(ci$src))) })
  btn <- function(lbl, target, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  rule_btns <- function(ru) if (!is.null(ru) && !VIEWER) div(class = "wb-inline",
    if (nzchar(ru$rule_id %||% "")) tags$a(class = "wb-btn sm", href = "#", `data-wb-input` = "wb_rule_open", `data-wb-key` = ru$rule_id, paste("Open rule", ru$rule_id)),
    actionButton("wb_rule_station", paste0("Rule for ", part()$station, "\u2026"), class = "wb-btn sm", title = "Add a row to qc_rules.csv for this station only, copied from this rule"))
  correct_box <- function(ci, ru) { if (VIEWER) return(NULL)
    what <- switch(ci$state %||% "",
      rem = if (identical(ci$rule, "manual")) tagList(p(class = "wb-p", "Removed by hand (corrections over a period)."), div(class = "wb-inline", btn("&#8634; Restore&hellip;", "wb_act_restore")))
            else tagList(p(class = "wb-p", paste0("Removed by a quality rule (", ci$rule, "). It comes back when the rule changes, for this station or everywhere.")), rule_btns(ru)),
      flag = tagList(p(class = "wb-p", "Kept and flagged by a quality rule."), div(class = "wb-inline", btn("&#10005; Remove&hellip;", "wb_act_remove", "danger")), rule_btns(ru)),
      pend = tagList(p(class = "wb-p", "A saved manual removal covers this value; the next run blanks it."), div(class = "wb-inline", btn("Undo&hellip;", "wb_act_restore"))),
      back = p(class = "wb-p", "Its manual removal was deleted; the next run brings the value back."),
      tagList(div(class = "wb-inline", btn("&#10005; Remove&hellip;", "wb_act_remove", "danger")), rule_btns(ru)))
    div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", what,
        div(class = "wb-inline", btn("Calibrate&hellip;", "wb_act_cal"), btn("Clock&hellip;", "wb_act_clock"), btn("Note&hellip;", "wb_act_note")))) }
  wb_import_aside <- function() { o <- outs(); last <- if (!is.null(o) && nrow(o$runs)) tail(o$runs, 1) else NULL
    steps <- c(Read = "new and changed files", Stitch = "one series per logger table; duplicate records dropped", `Time and names` = "clock corrections and time zones; parameter names from the aliases",
               Calibrate = "corrections over a period", `Quality rules` = "limits, spikes, steps, flat lines, manual removals; removed values kept in the audit", Outputs = "station tables, summaries, audit trail")
    div(class = "wb-insp", div(span(class = "wb-eb", "Pipeline"), div(style = "margin-top:8px",
        lapply(seq_along(steps), function(i) div(class = "wb-pstep", span(class = "n", i), div(div(names(steps)[i]), div(class = "s", steps[[i]])))))),
      div(class = "wb-hr", span(class = "wb-eb", "Last run"), div(style = "margin-top:6px",
        if (is.null(o)) p(class = "wb-p", "Not run yet.") else tagList(wb_kv("when", format(o$when, "%Y-%m-%d %H:%M")), wb_kv("files read", num(sum(o$manifest$status == "ok"))),
          wb_kv("station tables", nrow(o$stations)), wb_kv("problems", nrow(o$problems)), if (!is.null(last) && "run_id" %in% names(last)) wb_kv("run", last$run_id)))),
      if (length(std$rv$log)) div(class = "wb-hr", span(class = "wb-eb", "Log of this run"), tags$pre(class = "wb-log", paste(tail(std$rv$log, 40), collapse = "\n"))),
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Removed values are listed with their old values in the audit trail. Raw folders are never written.")) }

  output$wb_inspector <- renderUI({ v <- view()
    if (identical(v, "wb-import")) return(wb_import_aside())
    if (!identical(v, "wb-data")) return(NULL)
    cc <- cells(); p <- tryCatch(part(), error = function(e) NULL); o <- outs()
    if (is.null(p)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet"), wb_empty_acts("Raw files")), p(class = "wb-p", "Set the sources under Import data and press Run.")))
    meta <- if (!is.null(o) && nrow(o$meta)) o$meta[station == p$station & stream == p$stream] else data.table()
    station_box <- div(class = "wb-hr", span(class = "wb-eb", paste("Table \u00b7", p$station, p$stream)), div(style = "margin-top:6px",
      if (nrow(meta)) tagList(wb_kv("logger", paste(meta$model[1], "\u00b7", meta$logger_id[1])), wb_kv("interval", meta$interval[1]),
                              wb_kv("records", format(meta$records[1], big.mark = ",")), wb_kv("span", paste(substr(meta$start[1], 1, 10), "\u2192", substr(meta$end[1], 1, 10))),
                              wb_kv("complete", paste0(meta$complete_pct[1], " % \u00b7 ", meta$gaps[1], " gaps")), wb_kv("files", meta$files[1]))))
    if (!nrow(cc)) {
      g <- tryCatch(grid(), error = function(e) NULL)
      cnt <- function(k) if (is.null(g)) 0 else sum(vapply(g$states, function(x) sum(x$state == k), 0))
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No cell selected"),
        div(class = "sub", "Click or drag in the grid; a time selects the record, a heading the parameter. On the chart, click a point or drag across it.")),
        if (!is.null(g)) div(class = "wb-facts", wb_fact("rows", format(g$n, big.mark = ","), "in the period"), wb_fact("removed", cnt("rem"), "cells shown"),
                             wb_fact("flagged", cnt("flag"), "cells shown"), wb_fact("pending", cnt("pend") + cnt("back"), "saved, not run")),
        station_box))
    }
    if (nrow(cc) > 1) {
      g <- grid(); tt <- range(g$w$TIMESTAMP[cc$r])
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2(paste(format(nrow(cc), big.mark = ","), "values")),
          div(class = "sub", paste(format(tt[1], "%Y-%m-%d %H:%M", tz = "UTC"), "\u2013", format(tt[2], "%Y-%m-%d %H:%M", tz = "UTC"), "\u00b7", uniqueN(cc$r), "records"))),
        wb_sel_table(cc, g),
        if (!VIEWER) div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(class = "wb-inline", style = "margin-top:8px",
          btn("&#10005; Remove&hellip;", "wb_act_remove", "danger"), btn("&#8634; Restore&hellip;", "wb_act_restore"), btn("Calibrate&hellip;", "wb_act_cal"), btn("Clock&hellip;", "wb_act_clock"), btn("Note&hellip;", "wb_act_note"))),
        div(class = "wb-inline", actionLink("wb_zoom_sel", "Zoom the chart to the selection")),
        station_box))
    }
    a <- cc[1]; ci <- cell_info(a$r, a$p); ru <- wb_rule_for(rules(), a$p, p$station, ci$logger)
    qc_step <- switch(ci$state %||% "",
      rem = wb_step(paste("Removed by", ci$rule), ci$why, "e"), back = wb_step(paste("Removed by", ci$rule, "in the last run"), ci$why, "w"),
      flag = wb_step(paste("Flagged:", ci$rule), ci$why, "w"), pend = wb_step("Removal saved", ci$why, "w"),
      wb_step("No rule matched", if (!is.null(ru)) paste("checked against", ru$rule_id %||% "", ru$pattern) else "no rule covers this parameter"))
    div(class = "wb-insp",
      div(span(class = "wb-eb", "Selected cell"), tags$h2(a$p), div(class = "sub", paste(p$station, "\u00b7", p$stream, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"), zone()))),
      div(style = "display:flex;align-items:baseline;gap:10px;flex-wrap:wrap", span(style = "font-family:var(--wb-mono);font-size:22px",
          if (ci$state %in% c("rem", "back")) "\u2014" else wb_fmt(ci$v, ci$d)), state_chip(ci)),
      div(span(class = "wb-eb", "Lineage"), div(style = "margin-top:6px",
        wb_step(paste("Read", if (ci$state %in% c("rem", "back")) wb_fmt(ci$old, ci$d) else wb_fmt(ci$v, ci$d)), basename(ci$src)),
        wb_step(paste("Stitched into", p$stream), paste("logger", ci$logger)), wb_period_steps(E$period$data(), c(p$station, ci$logger), a$p, ci$t), qc_step)),
      correct_box(ci, ru),
      if (!is.null(ru)) div(class = "wb-hr", span(class = "wb-eb", paste("Rule", ru$rule_id %||% "")), div(style = "margin-top:6px",
        wb_kv("pattern", ru$pattern), if (nzchar(ru$applies_to %||% "")) wb_kv("applies to", ru$applies_to), wb_kv("range", paste(ru$min %||% "", "to", ru$max %||% "")),
        if (nzchar(ru$max_spike %||% "")) wb_kv("spike", ru$max_spike), if (nzchar(ru$max_step_per_hour %||% "")) wb_kv("step per hour", ru$max_step_per_hour),
        if (nzchar(ru$max_flat_hours %||% "")) wb_kv("flat hours", ru$max_flat_hours), if (nzchar(ru$action %||% "")) wb_kv("action", ru$action))),
      station_box)
  })
  observeEvent(input$wb_zoom_sel, { cc <- cells(); req(nrow(cc)); g <- grid(); tt <- as.numeric(range(g$w$TIMESTAMP[cc$r]))   # zoom the chart to it
    pad <- max(diff(tt) * 0.15, 6 * stats::median(diff(as.numeric(g$w$TIMESTAMP)), na.rm = TRUE), 3600, na.rm = TRUE)
    session$sendCustomMessage("wb-chart-view", list(x0 = tt[1] - pad, x1 = tt[2] + pad)) })

  output$wb_status <- renderUI({ o <- outs(); cc <- cells(); g <- tryCatch(grid(), error = function(e) NULL)
    np <- if (!is.null(o)) nrow(o$problems) else 0L
    rec <- if (!is.null(o) && nrow(o$file_audit) && "reconciles" %in% names(o$file_audit)) all(as.logical(o$file_audit$reconciles), na.rm = TRUE) else NA
    last <- if (!is.null(o) && nrow(o$runs)) tail(o$runs, 1) else NULL
    mid <- if (!identical(view(), "wb-data")) NULL else if (nrow(cc) > 1 && !is.null(g)) { v <- unlist(lapply(seq_len(nrow(cc)), function(i) g$w[[cc$p[i]]][cc$r[i]])); v <- v[is.finite(v)]
      tagList(span(paste(format(nrow(cc), big.mark = ","), "selected")), if (length(v)) tagList(span(paste("mean", signif(mean(v), 5))), span(paste("min", signif(min(v), 5))), span(paste("max", signif(max(v), 5))))) }
      else if (!is.null(g)) span(paste(format(g$n, big.mark = ","), "rows in the period"))
    tagList(span(style = paste0("color:", GB$fg), HTML(if (np) paste0('<a href="#" data-ovid-go="Metadata" style="color:inherit"><span style="color:', GB$yellow, '">&#9650;</span> ', np, " problems</a>") else paste0('<span style="color:', GB$green, '">&#10003;</span> No problems'))),
            div(class = "wb-fill mono", style = "display:flex;justify-content:center;gap:16px", mid),
            span(style = "display:flex;align-items:center;gap:7px", HTML(paste0('<span style="width:7px;height:7px;border-radius:50%;background:', if (isTRUE(rec)) GB$green else GB$yellow, '"></span>')),
                 paste0(if (isTRUE(rec)) "Reconciled" else "Not checked", if (!is.null(last) && "run_id" %in% names(last)) paste0(" \u00b7 run ", last$run_id) else "")),
            span(class = "mono", paste0(tryCatch(paste(zone(), "\u00b7 "), error = function(e) ""), "CS ", PIPELINE_VERSION, " \u00b7 workbench ", OVID_WORKBENCH_VERSION)))
  })
  output$wb_export <- downloadHandler(function() paste0("cs_", safe_name(key()), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) { w <- win(); ps <- sel_params(); fwrite(w[, c("TIMESTAMP", ps, intersect(c("logger_id", "source_file"), names(w))), with = FALSE], file, dateTimeAs = "write.csv") })

  wb_project_server(input, output, session, ctx)
  wb_edit_server(input, output, session, ctx)
  wbgb_deps <- reactive({ s <- tryCatch(as.data.table(E$stations$data()), error = function(e) data.table())
    if (!nrow(s) || !all(c("logger_id", "station") %in% names(s))) NULL else unique(s[nzchar(station), .(serial = as.character(logger_id), from = as.character(start), to = as.character(end), name = station)]) })
  group_by_server("wbgb", reactive({ outs(); f <- tryCatch(fread(file.path(pd(), "output", "audit", "file_manifest.csv"), colClasses = "character"), error = function(e) NULL)
      if (is.null(f) || !nrow(f) || !"logger_id" %in% names(f)) NULL else f[nzchar(logger_id), .(serial = logger_id, file = path, first = first_ts, last = last_ts)] }), pd, wbgb_deps,
    open = function(r) { s <- wbgb_deps(); st <- if (!is.null(s) && nrow(s[serial == r$serial])) s[serial == r$serial]$name[1] else r$serial
      k <- first_key(st); if (is.null(k)) return(showNotification(paste(r$serial, "has no station table yet."), type = "message"))
      wb_jump_to(session, input, key, jump, set_view, k, NULL, format(r$first, "%Y-%m-%d"), format(r$last, "%Y-%m-%d")) })   # core grouping.R
  observe({ v <- tryCatch(part()$station, error = function(e) NULL); pr <- input$wb_period                 # what this view shows, for the switch (core shell.R)
    ovid_ctx(session, "station", v, input$wb_params, if (length(pr) == 2) pr[1], if (length(pr) == 2) pr[2], tryCatch(if (!is.null(sel()) && length(sel()$t)) format(min(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL), tryCatch(if (!is.null(sel()) && length(sel()$t)) format(max(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL)) })
  wb_needs_server(output, reactive(wb_needs_items(if (is.function(std$needs$check)) std$needs$check() else NULL, std$review$items())))   # core workbench.R
  qpreset <- reactiveVal(NULL)
  wb_period_server(input, output, session, E$period, sel, reactive(part()$station), params_all, function() open_table("period"), preset = qpreset)   # core workbench.R
  wb_review_handlers(std$review, function(r) wb_jump_to(session, input, key, jump, set_view, r$key, trim(strsplit(r$parameter, ",", fixed = TRUE)[[1]]), r$start, r$end), qpreset)   # Needs you (core workbench.R)
  invisible(ctx)
}
