# WLR/workbench/wb_wlr.R  --  The WLR Workbench (preview): one deployment's record (or one barologger's) charted over a
# spreadsheet grid, with the selected cells, where they came from and how to correct them beside it, and every page of the
# Standard interface around it. Part of Ovid Core // WLR. Sourced by app.R after wlr_pipeline.R and core/shell.R.
# A Workbench session runs the Standard server too (app.R, server(embedded = TRUE)) and shows its pages in the same hidden
# tab set, so every Standard process (run, set-up check, barometric groups, deployment history, datum and
# dGPS, field visits, quality rules, clock checks, raw files, audit trail, reports, settings) works here through the same
# code. This file adds the data view; wb_edit.R the corrections made from a selection; wb_project.R the Import view, the
# trees and the tabs.

source(file.path(CORE_DIR, "workbench.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_edit.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_project.R"), local = TRUE)

WB_META <- c("TIMESTAMP", "source_file", "unit_pres", "logger_id", "kind", "site", "deployment", "year", "baro_source", "baro_name", "baro_serial", "baro_shift")
WB_FIRST <- c("depth_m", "elev_m", "dtw_m", "pres_kpa", "baro_kpa", "dp_kpa", "temp_c", "spc_uscm", "ec_uscm", "pres_raw")   # the usual order
WB_DERIVED <- c("depth_m", "elev_m", "dtw_m", "dp_kpa")        # worked out from the water pressure (removed at their source)
WB_MAX_ROWS <- 5000L                                   # rows the grid shows at once; narrow the period for more
WB_CHART_MAX <- 8000L                                  # records per parameter the chart gets for the period; more are thinned
WB_DETAIL_MAX <- 40000L                                # records it gets at full resolution for a zoomed-in window
WB_GRID_PARAMS <- 24L                                  # parameters in the grid
WB_MAX_PARAMS <- 6L                                    # of those, charted


# A record in the tree: a deployment ("dep:<deployment>"), a site's whole record ("site:<site>": every deployment there
# joined in time order, whichever logger it was; the time between deployments stays empty) or a barologger's cleaned
# record ("baro:<serial>").
wb_split <- function(key) list(kind = sub(":.*$", "", key), id = sub("^[^:]*:", "", key))

# The records the Workbench lists: every deployment of the last run, by site, and every barologger, by baro station.
wb_tables <- function(o, pd) {
  d <- if (!is.null(o) && nrow(o$summary)) o$summary[order(site, year), .(key = paste0("dep:", deployment), group = site, label = paste(year, "\u00b7", serial),
                                                                         serial = as.character(serial), site, records, kind = "dep")] else data.table()
  if (nrow(d)) d <- rbind(                 # a site with more than one deployment gets its whole record first
                          d[, .(key = paste0("site:", site[1]), group = site[1], label = "Whole record", serial = paste(unique(serial), collapse = ", "),
                                site = site[1], records = sum(records), kind = "site", n = .N), by = site][n > 1][, n := NULL][, site := NULL][, .(key, group, label, serial, site = group, records, kind)],
                          d)[order(group, kind != "site")]
  st <- tryCatch(fread(out_dir(pd, "audit", "baro_stations.csv"), colClasses = "character"), error = function(e) data.table(serial = character(), station = character()))
  b <- if (!is.null(o) && nrow(o$loggers)) o$loggers[kind == "baro", .(serial = as.character(serial), records)] else data.table(serial = character(), records = integer())
  b <- b[vapply(serial, function(s) !is.null(wb_baro_file(pd, s)), NA)]
  if (nrow(b)) { b[, station := st$station[match(serial, st$serial)]]; b[is.na(station), station := paste("Baro", serial)]
    b <- b[order(station, serial), .(key = paste0("baro:", serial), group = "Barologgers", label = paste(station, "\u00b7", serial), serial, site = station, records, kind = "baro")] }
  rbind(d, b, fill = TRUE)
}
wb_baro_file <- function(pd, serial) { f <- list.files(out_dir(pd, "baro"), pattern = paste0("__", serial, "\\.csv$"), full.names = TRUE)
  f <- f[!startsWith(basename(f), "station__")]; if (length(f)) f[1] else NULL }
wb_load <- function(key, pd) { if (!length(key) || is.na(key[1]) || !nzchar(key[1])) return(NULL); k <- wb_split(key)   # nothing chosen yet
  if (!length(k$kind)) return(NULL)
  d <- if (k$kind == "dep") load_deployment(k$id, pd) else if (k$kind == "site") load_site(k$id, pd) else { f <- wb_baro_file(pd, k$id); if (is.null(f)) NULL else fix_ts_cols(fread(f, na.strings = c("", "NA"))) }
  if (!is.null(d) && "logger_id" %in% names(d)) set(d, j = "logger_id", value = as.character(d$logger_id))
  if (!is.null(d) && k$kind == "site") { sn <- suppressWarnings(as.numeric(d$logger_id)); if (!all(is.na(sn))) set(d, j = "serial", value = sn) }   # the grid's serial column
  d }

# The parameters of a record: its numeric columns, the usual ones first.
wb_params <- function(d) { x <- setdiff(names(d)[vapply(d, is.numeric, NA)], c(WB_META, "year")); c(intersect(WB_FIRST, x), setdiff(x, WB_FIRST)) }

# State of every cell in a window of records w (see core/workbench.R): flagged from the record's own flag_ columns, removed
# from the audit trail (a value worked out from the water pressure is removed with it), and, from the manual removals and
# out-of-water periods as saved now, "pend" (saved, the next run applies it) and "back" (removed by hand in the last run,
# its row since deleted). Returns per-parameter data.tables (state, why, rule, old) the size of the window.
wb_states <- function(w, params, removed, manual = NULL, oow = NULL, serial = "", site = "") {
  t <- as.numeric(w$TIMESTAMP); n <- nrow(w)
  rp <- if (nrow(removed)) removed[parameter == "pres_kpa"] else removed
  stats::setNames(lapply(params, function(p) {
    st <- rep("", n); why <- rep("", n); rule <- rep("", n); old <- rep(NA_real_, n)
    fc <- paste0("flag_", p)
    if (fc %in% names(w)) { f <- as.character(w[[fc]]); k <- which(!is.na(f) & nzchar(f)); st[k] <- "flag"; why[k] <- gsub(";", ", ", f[k]); rule[k] <- sub(";.*$", "", f[k]) }
    if (nrow(removed)) {
      if (p %in% WB_DERIVED && nrow(rp)) { m <- match(t, as.numeric(rp$TIMESTAMP)); k <- which(!is.na(m) & is.na(w[[p]]))
        st[k] <- "rem"; why[k] <- paste("worked out from the water pressure, removed:", rp$reason[m[k]]); rule[k] <- rp$rule[m[k]] }
      r <- removed[parameter == p]
      if (nrow(r)) { m <- match(t, as.numeric(r$TIMESTAMP)); k <- which(!is.na(m))
        st[k] <- "rem"; why[k] <- r$reason[m[k]]; rule[k] <- r$rule[m[k]]; old[k] <- r$old_value[m[k]] } }
    cov <- wb_cover(manual, oow, serial, site, w, p)
    k <- which(cov & st != "rem" & !is.na(w[[p]])); st[k] <- "pend"; why[k] <- "removal saved; the next run applies it"; rule[k] <- "manual"
    k <- which(!cov & st == "rem" & rule %in% c("manual", "out_of_water_manual")); st[k] <- "back"; why[k] <- "its manual removal was deleted; the next run restores the value"
    data.table(state = st, why = why, rule = rule, old = old)
  }), params)
}

# ---- UI ---------------------------------------------------------------------------------------------------------------
# The data view (core/workbench.R) with WLR's corrections in the cell bar.
wb_wlr_actions <- function() if (!VIEWER) tagList(
  actionButton("wb_act_remove", HTML("&#10005; Remove&hellip; <kbd>R</kbd>"), class = "wb-btn sm", title = "Remove the selected values (corrections over a period). R or Delete"),
  actionButton("wb_act_restore", HTML("&#8634; Restore&hellip;"), class = "wb-btn sm", title = "Delete the manual removals and out-of-water periods that cover the selected values"),
  actionButton("wb_act_oow", "Out of water\u2026", class = "wb-btn sm", title = "The logger was out of the water for the selected records (out_of_water.csv)"),
  wb_period_button(),
  actionButton("wb_act_drop", "Drop records\u2026", class = "wb-btn sm", title = "Drop the selected records whole (corrections over a period, parameter records)"),
  actionButton("wb_act_clock", "Clock\u2026", class = "wb-btn sm", title = "Shift the timestamps of a period (time_corrections.csv)"))

wb_ui <- function(req) {
  pages <- c(wlr_flow_pages(sources = FALSE), list(convert_tool(), reference_tool("WLR")))   # the Reference page last (core reference.R)
  std_panels <- lapply(Filter(function(x) inherits(x, "ovid_tool"), pages), function(x) tabPanel(x$title, value = x$value, div(class = "wb-std d-tool", x$ui)))
  tk <- utils::modifyList(ovid_tokens(NORD), WLR_TOKENS)
  wb_page("WLR", WLR_PAL, project = if (!VIEWER) projects_switch_ui("proj") else textOutput("wb_project", inline = TRUE), standard_href = "?ui=standard",
    head = tagList(tags$style(HTML(paste0(ovid_css(tk), if (exists("OVID_FILES_CSS")) OVID_FILES_CSS else "", std_css()))),
                   tags$script(HTML(OVID_SHELL_JS)), std_head(), tags$script(HTML(wb_keys_js()))),
    actions = tagList(
      if (!VIEWER) wb_needs_ui(),
      if (!VIEWER) tags$a(class = "wb-btn outline", href = "#", `data-ovid-go` = "wb-import", title = "Folders, the set-up check and the run", HTML("&#10515; Import data")),
      if (!VIEWER) actionButton("run", HTML("&#9656; Run"), class = "wb-btn primary", title = "Save the set-up tables, check them and run the pipeline") else span(class = "wb-badge", "Viewer"),
      downloadButton("wb_export", HTML("&#8599; Export view"), class = "wb-btn", icon = NULL),
      if (file.exists(file.path(APP_DIR, "docs", "WLR_reference_guide.html"))) tags$a(class = "wb-btn", href = "docs/WLR_reference_guide.html", target = "_blank", title = "Reference guide", "?")),
    nav = tagList(
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Sites"), span(class = "n", textOutput("wb_n_st", inline = TRUE))), uiOutput("wb_tree")),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Parameters"), span(class = "n", textOutput("wb_params_title", inline = TRUE))),
                   checkboxGroupInput("wb_params", NULL, choices = character())),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Loggers")), group_by_ui("wbgb", wb = TRUE)),   # core grouping.R
      wb_process_tree("WLR", WB_PROCESS),
      wb_tools_tree(pages),
      uiOutput("wb_setup_tree")),
    main = tagList(uiOutput("wb_tabbar", class = "wb-tabs"),
      do.call(tabsetPanel, c(list(id = "nav", type = "hidden", selected = "wb-data",
        tabPanel("Data", value = "wb-data", wb_data_ui(wb_wlr_actions())), tabPanel("Import data", value = "wb-import", wb_import_ui())), std_panels))),
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
  view <- reactive(input$nav %||% "wb-data")
  set_view <- function(v) updateTabsetPanel(session, "nav", selected = v)
  open_table <- function(id) session$sendCustomMessage("wb-open", WB_OPEN[[id]])
  zone <- function() if (BASE_OFFSET == 0) "UTC" else paste0("UTC", if (BASE_OFFSET < 0) "\u2212" else "+", abs(BASE_OFFSET))
  output$wb_project <- renderText(basename(pd()))

  tables <- reactive({ o <- outs(); req(o); wb_tables(o, pd()) })
  first_key <- function(want) { s <- tables(); if (!nrow(s)) return(NULL)
    if (!is.null(want) && want %in% s$key) return(want)
    if (!is.null(want) && paste0("dep:", want) %in% s$key) return(paste0("dep:", want))
    if (!is.null(want) && want %in% s$site) return(s[site == want][order(kind != "site", -records)]$key[1])   # a site (from the Standard view): its whole record
    d <- s[kind == "dep"]; if (nrow(d)) d[order(-records)]$key[1] else s$key[1] }
  key <- reactiveVal(NULL); jump <- wb_jump_new()                       # jump: a review-queue item to open (core workbench.R)
  observeEvent(tables(), { k <- isolate(key()); if (is.null(k) || !k %in% tables()$key) key(first_key(if (is.null(k)) q$deployment %||% q$site else NULL)) })
  observeEvent(input$wb_table, { key(input$wb_table); set_view("wb-data") })
  part <- reactive({ req(key()); k <- wb_split(key()); s <- tables()[key == key()]
    o <- outs(); r <- if (is.null(o)) data.table() else if (k$kind == "dep") o$summary[deployment == k$id] else if (k$kind == "site") o$summary[site == k$id][order(start)] else data.table()
    list(kind = k$kind, id = k$id, serial = if (nrow(s)) s$serial[1] else k$id, site = if (nrow(s)) s$site[1] else "", label = if (nrow(s)) paste(s$group[1], "\u00b7", s$label[1]) else k$id, row = r) })
  dat <- reactive({ outs(); d <- wb_load(key(), pd()); validate(need(!is.null(d) && nrow(d) > 0, "No records in this table.")); d })
  params_all <- reactive(wb_params(dat()))
  first_load <- reactiveVal(TRUE)
  observeEvent(params_all(), {
    pa <- params_all(); keep <- intersect(isolate(input$wb_params), pa)
    want <- if (isTRUE(first_load()) && !is.null(q$params)) intersect(strsplit(q$params, ",")[[1]], pa) else keep
    if (!length(want)) want <- head(intersect(c("depth_m", "temp_c", "baro_kpa", "pres_kpa"), pa), 2)
    if (!length(want)) want <- head(pa, 2)
    updateCheckboxGroupInput(session, "wb_params", choices = stats::setNames(pa, pa), selected = want)
    d <- dat(); r <- range(d$TIMESTAMP, na.rm = TRUE); cur <- isolate(input$wb_period)
    ok <- length(cur) == 2 && !anyNA(cur) && cur[1] >= as.Date(r[1]) && cur[2] <= as.Date(r[2])
    from <- if (isTRUE(first_load()) && !is.null(q$from)) as.Date(q$from) else if (ok) cur[1] else max(as.Date(r[1]), as.Date(r[2]) - 21)
    to <- if (isTRUE(first_load()) && !is.null(q$to)) as.Date(q$to) else if (ok) cur[2] else as.Date(r[2])
    jt <- wb_jump_take(jump, pa, want, r, from, to); from <- jt$from; to <- jt$to
    if (!identical(jt$want, want)) updateCheckboxGroupInput(session, "wb_params", selected = jt$want)
    updateDateRangeInput(session, "wb_period", start = from, end = to, min = as.Date(r[1]), max = as.Date(r[2]))
    first_load(FALSE)
  })
  observeEvent(input$wb_whole, { r <- range(dat()$TIMESTAMP, na.rm = TRUE); updateDateRangeInput(session, "wb_period", start = as.Date(r[1]), end = as.Date(r[2])) })
  sel_params <- reactive({ x <- head(intersect(input$wb_params, params_all()), WB_GRID_PARAMS)   # a whole record lists the serial of each row first
    if (identical(part()$kind, "site") && "serial" %in% params_all()) c("serial", setdiff(x, "serial")) else x })
  plot_params <- reactive(head(setdiff(sel_params(), "serial"), WB_MAX_PARAMS))
  period <- reactive({ p <- input$wb_period; req(length(p) == 2, !anyNA(p))
    c(as.POSIXct(paste(p[1], "00:00:00"), tz = "UTC"), as.POSIXct(paste(p[2], "23:59:59"), tz = "UTC")) })
  per <- reactive({ d <- dat(); pr <- period(); d[TIMESTAMP >= pr[1] & TIMESTAMP <= pr[2]] })
  removed <- reactive({ outs(); p <- part(); d <- dat()
    if (identical(p$kind, "site")) return(rbindlist(lapply(split(d, by = "logger_id"), function(x) { r <- removed_values(x$logger_id[1], pd())     # each logger over its own rows
      if (nrow(r)) r[TIMESTAMP >= min(x$TIMESTAMP) & TIMESTAMP <= max(x$TIMESTAMP)] else r }), fill = TRUE))
    r <- removed_values(p$serial, pd()); if (nrow(r)) r[TIMESTAMP >= min(d$TIMESTAMP) & TIMESTAMP <= max(d$TIMESTAMP)] else r })
  # a whole record's rows by logger (its serial and row numbers), so saved removals are matched to the logger of each row
  by_logger <- function(w) { p <- part(); if (identical(p$kind, "site") && "logger_id" %in% names(w)) split(seq_len(nrow(w)), as.character(w$logger_id)) else stats::setNames(list(seq_len(nrow(w))), p$serial) }
  flagruns <- reactive({ d <- per(); rbindlist(lapply(plot_params(), function(p) { fc <- paste0("flag_", p); if (fc %in% names(d)) wb_flag_runs(d$TIMESTAMP, as.character(d[[fc]]), p) }), fill = TRUE) })
  win <- reactive(per())                                         # the grid lists the period; the chart zooms within it
  grid <- reactive({ w <- win(); ps <- sel_params(); validate(need(length(ps) > 0, "Tick a parameter on the left."), need(nrow(w) > 0, "No records in this period."))
    n <- nrow(w); w <- head(w, WB_MAX_ROWS); p <- part(); g <- by_logger(w)
    st <- lapply(names(g), function(sn) wb_states(w[g[[sn]]], ps, removed(), E$manual$data(), E$oow$data(), sn, p$site))
    st <- if (length(st) == 1) st[[1]] else { o <- order(unlist(g)); stats::setNames(lapply(ps, function(q) rbindlist(lapply(st, `[[`, q))[o]), ps) }
    list(w = w, n = n, params = ps, states = st, cells = wb_grid_cells(w, ps, st, digits = c(serial = 0L)),
         sig = paste(key(), paste(ps, collapse = ","), format(w$TIMESTAMP[1], tz = "UTC"), nrow(w), sep = "|")) })

  # tree
  output$wb_n_st <- renderText({ s <- tables(); if (nrow(s)) uniqueN(s[kind == "dep", group]) else "" })
  output$wb_tree <- renderUI({ s <- tables(); validate(need(nrow(s), "No output yet. Set the folders under Import data and press Run."))
    k <- key(); cur <- if (!is.null(k)) s[key == k, group][1] else ""
    tagList(lapply(unique(s$group), function(g) { x <- s[group == g]; open <- identical(g, cur)
      tagList(wb_row(g, key = x$key[1], input = "wb_table", n = if (!open) nrow(x) else "", on = FALSE),
              if (open) lapply(seq_len(nrow(x)), function(i) wb_row(x$label[i], key = x$key[i], input = "wb_table",
                n = format(x$records[i], big.mark = ","), on = identical(x$key[i], k) && identical(view(), "wb-data"), level = 1))) })) })
  # safe: a title only
  output$wb_params_title <- renderText(tryCatch(if (part()$kind == "baro") "barologger" else part()$row$kind[1] %||% "", error = function(e) ""))
  output$wb_chips <- renderUI(tagList(lapply(plot_params(), function(p) span(class = "wb-chip", title = param_label(p), p)),
    if (length(sel_params()) > length(plot_params())) span(class = "wb-chip", style = "opacity:.7", paste0("+", length(sel_params()) - length(plot_params()), " in the table"))))

  # grid and selection
  output$wb_grid <- renderUI({ g <- grid()
    vals <- lapply(g$params, function(p) { v <- suppressWarnings(as.numeric(g$w[[p]])); o <- g$states[[p]]$old; ifelse(is.na(v), o, v) })
    wb_grid(format(g$w$TIMESTAMP, "%Y-%m-%d %H:%M", tz = "UTC"), g$params, g$cells$txt, g$cells$cls, g$sig, corner = paste("time", zone()),
            # safe: units are labels only
            times = as.numeric(g$w$TIMESTAMP), vals = vals, units = tryCatch(units(), error = function(e) NULL)) })
  output$wb_ginfo <- renderText({ g <- grid(); cc <- cells()
    paste0(if (g$n > nrow(g$w)) sprintf("first %s of %s rows; narrow the period, or zoom the chart and press Use as period", format(nrow(g$w), big.mark = ","), format(g$n, big.mark = ","))
           else paste(format(g$n, big.mark = ","), "rows"),
           if (nrow(cc)) paste0(" \u00b7 ", format(nrow(cc), big.mark = ","), " selected") else "",
           " \u00b7 click, drag, shift-click; a time selects the record, a heading the parameter; cmd/ctrl adds; Esc clears") })
  # safe: no grid yet: no cells selected
  cells <- reactive(wb_sel_cells(input$wb_grid_sel, tryCatch(grid(), error = function(e) NULL)))
  active <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); s <- input$wb_grid_sel
    a <- cc[r == as.integer(s$ar %||% 0) & c == as.integer(s$ac %||% 0)]; if (nrow(a)) a[1] else cc[1] })
  # the values the corrections act on: the selected cells (in the grid, or dragged or clicked on the chart)
  sel <- reactive({ cc <- cells(); cc <- cc[cc$p != "serial"]; if (!nrow(cc)) return(NULL); w <- grid()$w   # the serial column is not corrected
    list(p = cc$p, t = w$TIMESTAMP[cc$r], serial = if ("logger_id" %in% names(w)) as.character(w$logger_id[cc$r]) else rep(part()$serial, nrow(cc))) })
  clear_sel <- function() session$sendCustomMessage("wb-grid-clear", list())
  cell_info <- function(r, p) { g <- grid(); s <- g$states[[p]]; w <- g$w; col <- function(k) if (k %in% names(w)) w[[k]][r] else NA
    list(t = w$TIMESTAMP[r], v = w[[p]][r], state = s$state[r], why = s$why[r], rule = s$rule[r], old = s$old[r], src = col("source_file") %|% "",
         ser = col("logger_id") %|% "", raw = col("pres_raw"), unit = col("unit_pres"), baro = col("baro_kpa"), bsrc = col("baro_source"), bname = col("baro_name"), bser = col("baro_serial"), bshift = col("baro_shift"),
         d = wb_digits(c(w[[p]], s$old))) }

  # the chart is drawn in the browser (WB_CHART_JS): it gets the period, thinned when long, and a zoomed-in window at full
  # resolution when it asks (input$wb_view); the grid's selection is highlighted there directly
  chart_pend <- function(d, ps) { if (!nrow(d)) return(data.table(p = character(), t = numeric(), v = numeric())); p0 <- part()
    g <- by_logger(d)
    rbindlist(lapply(names(g), function(sn) { x <- d[g[[sn]]]
      rbindlist(lapply(ps, function(p) { k <- wb_cover(E$manual$data(), E$oow$data(), sn, p0$site, x, p) & !is.na(x[[p]])
        data.table(p = p, t = as.numeric(x$TIMESTAMP[k]), v = x[[p]][k]) })) })) }
  chart_rem <- reactive({ r <- removed(); if (!nrow(r)) return(data.table(timestamp = as.POSIXct(character(), tz = "UTC"), parameter = character(), old_value = numeric()))
    r[, .(timestamp = TIMESTAMP, parameter, old_value)] })
  chart_base <- reactive({ d <- per(); ps <- plot_params(); req(length(ps) > 0, nrow(d) > 0); pr <- as.numeric(period())
    # safe: units and limits are drawn when known; the data are drawn either way
    x <- wb_chart_data(d, ps, chart_rem(), flagruns(), chart_pend(d, ps), WB_CHART_MAX, units = tryCatch(units(), error = function(e) NULL), limits = tryCatch(limits(), error = function(e) NULL))
    p0 <- part(); sp <- if (identical(p0$kind, "site") && nrow(p0$row)) absent_spans(p0$row$start, p0$row$end, p0$row$serial, zone()) else NULL   # the Whole record: the time between deployments hatched, never filled
    c(list(type = "base", key = key(), sig = paste(key(), paste(ps, collapse = ","), pr[1], pr[2], x$n, sep = "|"), period = I(pr), zone = zone(),
           spans = lapply(seq_len(NROW(sp)), function(i) as.list(sp[i]))), x) })
  view_d <- wb_chart_server(input, session, chart_base, function(a, z) { d <- per(); k <- as.numeric(d$TIMESTAMP)
    if (sum(k >= a & k <= z) > WB_DETAIL_MAX) return(NULL)
    dd <- d[k >= a & k <= z]; ps <- plot_params(); wb_chart_data(dd, ps, chart_rem(), flagruns(), chart_pend(dd, ps), Inf) })
  output$wb_overview <- renderPlot({ req(wb_overview_fits()); d <- dat(); ps <- plot_params(); req(length(ps) > 0)
    wb_overview_plot(d$TIMESTAMP, d[[ps[1]]], period(), view_d(), WLR_PAL) }, bg = NORD$bg)
  # each parameter's unit (from its label) and range limits (Automatic limits, qc_rules.csv: a row for this site or serial
  # first, then for the logger kind or all), for the grid and chart
  units <- reactive({ ps <- params_all(); u <- sub("^.*\\(([^()]*)\\)$", "\\1", param_label(ps)); stats::setNames(ifelse(grepl("\\)$", param_label(ps)) & u != "file units", u, ""), ps) })
  # safe: the QC table's editor reports a read problem
  limits <- reactive({ ps <- params_all(); r <- tryCatch(as.data.table(E$qc$data()), error = function(e) data.table()); p0 <- part()
    if (!nrow(r) || !all(c("parameter", "min", "max") %in% names(r))) return(NULL)
    ap <- trim(as.character(r$applies_to %||% rep("", nrow(r)))); ap[is.na(ap)] <- ""; kind <- if (identical(p0$kind, "baro")) "baro" else "water"
    mine <- vapply(strsplit(ap, "|", fixed = TRUE), function(a) any(trim(a) %in% c(p0$site, p0$serial)), NA)
    r <- rbind(r[mine], r[!mine & ap %in% c(kind, "all", "")]); wb_limits(ps, r$parameter, r$min, r$max) })
  state_chip <- function(ci) wb_state_chip(ci$state, ci$rule, pend = "removal saved \u00b7 run to apply")

  # cell bar, inspector, status
  output$wb_cellinfo <- renderUI({ cc <- cells()
    if (!nrow(cc)) return(span(class = "txt", if (VIEWER) "Select cells for their values, states and sources." else "Select cells, records (a time) or parameters (a heading), or click or drag on the chart; then correct them."))
    a <- active(); ci <- cell_info(a$r, a$p)
    val <- if (ci$state %in% c("rem", "back")) paste0("\u2014", if (!is.na(ci$old)) paste0(" (was ", wb_fmt(ci$old, ci$d), ")") else "") else wb_fmt(ci$v, ci$d)
    tagList(span(class = "mono", style = paste0("color:", NORD$fg4), paste0(LETTERS[min(a$c + 1, 26)], a$r)),
        span(class = "mono", paste(a$p, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"))), span(class = "mono", style = "font-size:14px", val),
        state_chip(ci), span(class = "txt", if (nrow(cc) > 1) paste(format(nrow(cc), big.mark = ","), "cells selected") else if (nzchar(ci$why)) ci$why else basename(ci$src))) })
  btn <- function(lbl, target, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  correct_btns <- function(state = "") tagList(
    div(class = "wb-inline", if (state %in% c("rem", "pend", "back")) btn("&#8634; Restore&hellip;", "wb_act_restore") else btn("&#10005; Remove&hellip;", "wb_act_remove", "danger"),
        btn("Out of water&hellip;", "wb_act_oow"), btn("Drop records&hellip;", "wb_act_drop")),
    div(class = "wb-inline", btn("Clock&hellip;", "wb_act_clock"),
        if (identical(part()$kind, "dep")) tagList(btn("In / out dates&hellip;", "wb_act_dates"), btn("Measurement&hellip;", "wb_act_meas"))),
    div(class = "wb-inline", actionLink("wb_rule_open", "Open the quality rules for this parameter")))
  correct_box <- function(ci) { if (VIEWER) return(NULL)
    what <- switch(ci$state %||% "",
      rem = p(class = "wb-p", if (ci$rule %in% c("manual", "out_of_water_manual")) "Removed by hand (corrections over a period or out_of_water.csv)." else
              paste0("Removed by a quality rule (", ci$rule, "). It comes back when the rule is changed on Quality rules.")),
      flag = p(class = "wb-p", "Kept and flagged."), pend = p(class = "wb-p", "A saved removal covers this value; the next run blanks it."),
      back = p(class = "wb-p", "Its removal was deleted; the next run brings the value back."), NULL)
    div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", what, correct_btns(ci$state))) }
  dep_box <- function() { p <- part(); r <- p$row
    if (identical(p$kind, "baro")) return(div(class = "wb-hr", span(class = "wb-eb", paste("Barologger \u00b7", p$serial)), div(style = "margin-top:6px",
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Its cleaned record, as it went into its baro station. A removal here changes the water levels it compensates on the next run."))))
    if (!nrow(r)) return(NULL)
    if (identical(p$kind, "site")) return(div(class = "wb-hr", span(class = "wb-eb", paste("Whole record \u00b7", p$site)), div(style = "margin-top:6px",
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Every deployment here in time order, whichever logger it was. Between deployments there are no records; nothing is filled in."),
      lapply(seq_len(nrow(r)), function(i) wb_kv(paste(r$year[i], "\u00b7", r$serial[i]), paste(substr(fmt_ts(r$start[i]), 1, 10), "\u2192", substr(fmt_ts(r$end[i]), 1, 10)))))))
    div(class = "wb-hr", span(class = "wb-eb", paste("Deployment \u00b7", r$site[1], r$year[1])), div(style = "margin-top:6px",
      wb_kv("serial", r$serial[1]), wb_kv("records", format(r$records[1], big.mark = ",")), wb_kv("span", paste(substr(fmt_ts(r$start[1]), 1, 10), "\u2192", substr(fmt_ts(r$end[1]), 1, 10))),
      wb_kv("T_ST", if (is.na(r$T_ST[1])) "not set" else paste(r$T_ST[1], "m", if (nzchar(r$reference[1] %|% "")) paste("from", r$reference[1]) else "")),
      wb_kv("offset", paste(r$offset_m[1] %|% 0, "m")), wb_kv("reference elevation", if (is.na(r$z_ref[1])) "not surveyed" else paste(r$z_ref[1], "m")),
      wb_kv("baro", paste(r$primary[1] %|% "", if (nzchar(r$secondary[1] %|% "")) paste("then", r$secondary[1]) else "")),
      if (!is.na(r$tape_diff_median[1])) wb_kv("tape \u2212 logger", paste(r$tape_diff_median[1], "m")))) }
  wb_import_aside <- function() { o <- outs(); m <- if (!is.null(o)) o$manifest else NULL; v <- function(k) if (!is.null(m) && nrow(m)) m$value[match(k, m$key)] else NA
    steps <- c(Read = "new and changed files; HOBO from their HOBOware CSV, Solinst from the .xle", Stitch = "one record per serial, every year together; repeats kept once",
               Barologgers = "limits and removals, then one series per baro station", Deployments = "split by the workbook's visit dates; baro groups by year",
               Compensate = "water minus baro, to metres of water, depth to water and elevation", `Quality rules` = "limits, spikes, flat lines, removals and out-of-water periods; removed values kept in the audit",
               Outputs = "site files, corrected data per site, summaries, audit trail")
    div(class = "wb-insp", div(span(class = "wb-eb", "Pipeline"), div(style = "margin-top:8px",
        lapply(seq_along(steps), function(i) div(class = "wb-pstep", span(class = "n", i), div(div(names(steps)[i]), div(class = "s", steps[[i]])))))),
      div(class = "wb-hr", span(class = "wb-eb", "Last run"), div(style = "margin-top:6px",
        if (is.null(o)) p(class = "wb-p", "Not run yet.") else tagList(wb_kv("when", format(o$when, "%Y-%m-%d %H:%M")), wb_kv("files used", num(sum(o$files$use %in% TRUE))),
          wb_kv("deployments", nrow(o$summary)), wb_kv("problems", nrow(o$problems)),
          if (!is.na(v("water_loggers_reused"))) wb_kv("water loggers reused", paste(v("water_loggers_reused"), "of", v("water_loggers")))))),
      if (length(std$rv$log)) div(class = "wb-hr", span(class = "wb-eb", "Log of this run"), tags$pre(class = "wb-log", paste(tail(std$rv$log, 40), collapse = "\n"))),
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Removed values are listed with their old values in the audit trail. Raw files and the metadata workbook are never written.")) }

  output$wb_inspector <- renderUI({ v <- view()
    if (identical(v, "wb-import")) return(wb_import_aside())
    if (!identical(v, "wb-data")) return(NULL)
    # safe: nothing picked yet
    cc <- cells(); p <- tryCatch(part(), error = function(e) NULL)
    if (is.null(p)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet"), wb_empty_acts("Files")), p(class = "wb-p", "Set the folders under Import data and press Run.")))
    if (!nrow(cc)) {
      # safe: no grid yet
      g <- tryCatch(grid(), error = function(e) NULL)
      cnt <- function(k) if (is.null(g)) 0 else sum(vapply(g$states, function(x) sum(x$state == k), 0))
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No cell selected"),
        div(class = "sub", "Click or drag in the grid; a time selects the record, a heading the parameter. On the chart, click a point or drag across it.")),
        if (!is.null(g)) div(class = "wb-facts", wb_fact("rows", format(g$n, big.mark = ","), "in the period"), wb_fact("removed", cnt("rem"), "cells shown"),
                             wb_fact("flagged", cnt("flag"), "cells shown"), wb_fact("pending", cnt("pend") + cnt("back"), "saved, not run")),
        dep_box()))
    }
    if (nrow(cc) > 1) {
      g <- grid(); tt <- range(g$w$TIMESTAMP[cc$r])
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2(paste(format(nrow(cc), big.mark = ","), "values")),
          div(class = "sub", paste(format(tt[1], "%Y-%m-%d %H:%M", tz = "UTC"), "\u2013", format(tt[2], "%Y-%m-%d %H:%M", tz = "UTC"), "\u00b7", uniqueN(cc$r), "records"))),
        wb_sel_table(cc, g),
        if (!VIEWER) div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", correct_btns())),
        div(class = "wb-inline", actionLink("wb_zoom_sel", "Zoom the chart to the selection")),
        dep_box()))
    }
    a <- cc[1]; ci <- cell_info(a$r, a$p)
    qc_step <- switch(ci$state %||% "",
      rem = wb_step(paste("Removed by", ci$rule), ci$why, "e"), back = wb_step(paste("Removed by", ci$rule, "in the last run"), ci$why, "w"),
      flag = wb_step(paste("Flagged:", ci$rule), ci$why, "w"), pend = wb_step("Removal saved", ci$why, "w"), wb_step("No rule removed or flagged it", "quality rules, removals and out-of-water periods"))
    read_v <- if (!is.na(ci$raw) && a$p %in% c("pres_kpa", "pres_raw", WB_DERIVED)) paste(signif(ci$raw, 6), ci$unit %|% "") else if (ci$state %in% c("rem", "back")) wb_fmt(ci$old, ci$d) else wb_fmt(ci$v, ci$d)
    div(class = "wb-insp",
      div(span(class = "wb-eb", "Selected cell"), tags$h2(a$p), div(class = "sub", paste(param_label(a$p), "\u00b7", p$label, "\u00b7", format(ci$t, "%Y-%m-%d %H:%M", tz = "UTC"), zone()))),
      div(style = "display:flex;align-items:baseline;gap:10px;flex-wrap:wrap", span(style = "font-family:var(--wb-mono);font-size:22px",
          if (ci$state %in% c("rem", "back")) "\u2014" else wb_fmt(ci$v, ci$d)), state_chip(ci)),
      div(span(class = "wb-eb", "Lineage"), div(style = "margin-top:6px",
        wb_step(paste("Read", read_v), basename(ci$src)),
        wb_step(paste("Stitched into serial", if (identical(p$kind, "site")) ci$ser else p$serial), if (p$kind %in% c("dep", "site")) "every year of this logger together, then split by the deployment dates" else "its cleaned barologger record"),
        if (p$kind %in% c("dep", "site") && a$p %in% c(WB_DERIVED, "baro_kpa"))
          wb_step(paste("Compensated with", if (nzchar(ci$bname %|% "")) ci$bname else "no barologger"),
                  paste0(ci$bsrc %|% "", if (nzchar(ci$bser %|% "")) paste0(" \u00b7 serial ", ci$bser), if (!is.na(ci$baro)) paste0(" \u00b7 ", signif(ci$baro, 6), " kPa"),
                         if (!is.na(ci$bshift) && ci$bshift != 0) paste0(" \u00b7 shifted ", ci$bshift, " kPa"))),
        if (a$p %in% c("depth_m", "dtw_m", "elev_m")) wb_step("Converted", switch(a$p, depth_m = "water minus baro \u00d7 1000 / (\u03c1 g)", dtw_m = "T_ST \u2212 offset \u2212 water above the sensor",
                                                                                  elev_m = "reference elevation \u2212 T_ST + offset + water above the sensor")),
        wb_period_steps(E$period$data(), c(row_serial(a$r), p$site, p$id), a$p, ci$t), qc_step)),
      missing_box(a, ci), correct_box(ci), dep_box())
  })
  # a missing value (none in the record): type one with its reason, or fill the run of missing values it is in by linear
  # interpolation (only when asked; sources in the hub references file). Both are rows of the corrections over a period.
  row_serial <- function(r) { p <- part(); w <- grid()$w; if (identical(p$kind, "site") && "logger_id" %in% names(w)) as.character(w$logger_id[r]) else p$serial }
  gap_of <- function(r, q) { x <- is.na(grid()$w[[q]]); i0 <- r; i1 <- r
    while (i0 > 1 && x[i0 - 1]) i0 <- i0 - 1; while (i1 < length(x) && x[i1 + 1]) i1 <- i1 + 1; c(i0, i1) }
  missing_box <- function(a, ci) { if (VIEWER || !is.na(ci$v) || a$p == "serial") return(NULL)
    # safe: units are labels only
    u <- tryCatch(units()[[a$p]], error = function(e) "") %|% ""; g <- gap_of(a$r, a$p); n <- g[2] - g[1] + 1L; w <- grid()$w
    div(class = "wb-hr", span(class = "wb-eb", "Missing value"), div(style = "margin-top:6px;display:flex;flex-direction:column;gap:6px",
      div(class = "wb-p", style = "color:var(--wb-fg2)", if (n > 1) sprintf("%d missing values in a row here, %s to %s.", n, fmt_ts(w$TIMESTAMP[g[1]]), fmt_ts(w$TIMESTAMP[g[2]])) else "No value at this time."),
      div(class = "wb-form2", numericInput("wb_mv_value", paste0("Value", if (nzchar(u)) paste0(" (", u, ")")), NA, width = "100%"), textInput("wb_mv_author", "By", "", width = "100%")),
      textInput("wb_mv_reason", "Reason", "", width = "100%", placeholder = "manual reading at the visit"),
      actionButton("wb_mv_save", "Type a value", class = "wb-btn sm", title = "Saved to the corrections over a period; the next run puts it in and flags it as typed by hand"),
      actionButton("wb_mv_interp", HTML("Fill by linear interpolation&hellip;"), class = "wb-btn sm", title = "Opens the period tool for this run of missing values; nothing is filled until you add it"))) }
  observeEvent(input$wb_mv_save, { cc <- cells(); req(nrow(cc) == 1); a <- cc[1]; w <- grid()$w
    v <- input$wb_mv_value; why <- trim(input$wb_mv_reason %||% "")
    if (is.null(v) || is.na(v)) return(ovid_field_error("wb_mv_value", "Type a number."))
    if (!nzchar(why)) return(ovid_field_error("wb_mv_reason", "Give the reason: where the value comes from."))
    t <- fmt_ts(w$TIMESTAMP[a$r])
    r <- data.table(target = row_serial(a$r), parameter = a$p, from = t, to = t, action = "set_value", value = format(v, digits = 10), on = "on",
                    note = why, author = trim(input$wb_mv_author %||% ""), added = format(Sys.time(), "%Y-%m-%d %H:%M"))
    if (isTRUE(E$period$append(as.data.frame(r)))) showNotification("Typed value added to the corrections over a period (period_corrections.csv). Run to apply.", type = "message") })
  observeEvent(input$wb_mv_interp, { cc <- cells(); req(nrow(cc) == 1); a <- cc[1]; w <- grid()$w; g <- gap_of(a$r, a$p); n <- g[2] - g[1] + 1L
    qpreset(list(target = row_serial(a$r), parameter = a$p, from = fmt_ts(w$TIMESTAMP[g[1]]), to = fmt_ts(w$TIMESTAMP[g[2]]), action = "interpolate", note = "",
                 nonce = as.numeric(Sys.time()),
                 lead = paste0(n, " missing value", if (n > 1) "s", ", filled with a straight line in time between the last value before and the first after ",
                               "(linear interpolation, for short gaps: see the references file). The filled values are flagged and listed in the audit trail."))) })
  observeEvent(input$wb_zoom_sel, { cc <- cells(); req(nrow(cc)); g <- grid(); tt <- as.numeric(range(g$w$TIMESTAMP[cc$r]))   # zoom the chart to it
    pad <- max(diff(tt) * 0.15, 6 * stats::median(diff(as.numeric(g$w$TIMESTAMP)), na.rm = TRUE), 3600, na.rm = TRUE)
    session$sendCustomMessage("wb-chart-view", list(x0 = tt[1] - pad, x1 = tt[2] + pad)) })

  # safe: the status bar shows what it has
  output$wb_status <- renderUI({ o <- outs(); cc <- cells(); g <- tryCatch(grid(), error = function(e) NULL)
    np <- if (!is.null(o)) nrow(o$problems) else 0L
    rec <- if (!is.null(o) && nrow(o$recon) && "ok" %in% names(o$recon)) all(as.logical(o$recon$ok), na.rm = TRUE) else NA
    last <- if (!is.null(o) && nrow(o$runs)) tail(o$runs, 1) else NULL
    mid <- if (!identical(view(), "wb-data")) NULL else if (nrow(cc) > 1 && !is.null(g)) { v <- unlist(lapply(seq_len(nrow(cc)), function(i) g$w[[cc$p[i]]][cc$r[i]])); v <- v[is.finite(v)]
      tagList(span(paste(format(nrow(cc), big.mark = ","), "selected")), if (length(v)) tagList(span(paste("mean", signif(mean(v), 5))), span(paste("min", signif(min(v), 5))), span(paste("max", signif(max(v), 5))))) }
      else if (!is.null(g)) span(paste(format(g$n, big.mark = ","), "rows in the period"))
    tagList(span(style = paste0("color:", NORD$fg), HTML(if (np) paste0('<a href="#" data-ovid-go="Preflight" style="color:inherit"><span style="color:', NORD$yellow, '">&#9650;</span> ', np, " problems</a>") else paste0('<span style="color:', NORD$green, '">&#10003;</span> No problems'))),
            div(class = "wb-fill mono", style = "display:flex;justify-content:center;gap:16px", mid),
            span(style = "display:flex;align-items:center;gap:7px", HTML(paste0('<span style="width:7px;height:7px;border-radius:50%;background:', if (isTRUE(rec)) NORD$green else NORD$yellow, '"></span>')),
                 paste0(if (isTRUE(rec)) "Reconciled" else "Not checked", if (!is.null(last) && "run_id" %in% names(last)) paste0(" \u00b7 run ", last$run_id) else "")),
            span(class = "mono", paste0(zone(), " \u00b7 WLR ", PIPELINE_VERSION, " \u00b7 workbench ", OVID_WORKBENCH_VERSION)))
  })
  output$wb_export <- downloadHandler(function() paste0("wlr_", safe_name(part()$id), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) { w <- win(); ps <- sel_params(); fwrite(w[, c("TIMESTAMP", ps, intersect(c("logger_id", "source_file"), names(w))), with = FALSE], file, dateTimeAs = "write.csv") })

  wb_project_server(input, output, session, ctx)
  wb_edit_server(input, output, session, ctx)
  wbgb_deps <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$summary)) NULL else o$summary[, .(serial = as.character(serial), from = fmt_ts(start), to = fmt_ts(end), name = site)] })
  # safe: an output of the last run, only shown
  group_by_server("wbgb", reactive({ outs(); f <- tryCatch(fread(file.path(pd(), "output", "audit", "files.csv"), colClasses = "character"), error = function(e) NULL)
      if (is.null(f) || !nrow(f)) NULL else f[nzchar(serial), .(serial, file = path, first = first_ts, last = last_ts)] }), pd, wbgb_deps,
    open = function(r) { k <- { tb <- tables(); x <- tb[serial == r$serial & kind == "dep"]; y <- x[grepl(substr(format(r$first, "%Y"), 1, 4), label, fixed = TRUE)]; if (nrow(y)) y$key[1] else if (nrow(x)) x$key[1] else NA_character_ }; if (is.na(k)) return(showNotification(paste(r$serial, "has no table in this view yet."), type = "message"))
      wb_jump_to(session, input, key, jump, set_view, k, NULL, format(r$first, "%Y-%m-%d"), format(r$last, "%Y-%m-%d")) })   # core grouping.R
  observe({ v <- tryCatch({ p <- part(); if (nzchar(p$site)) p$site else p$serial }, error = function(e) NULL); pr <- input$wb_period                 # what this view shows, for the switch (core shell.R)
    # safe: the context bar shows what it has
    ovid_ctx(session, "site", v, input$wb_params, if (length(pr) == 2) pr[1], if (length(pr) == 2) pr[2], tryCatch(if (!is.null(sel()) && length(sel()$t)) format(min(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL), tryCatch(if (!is.null(sel()) && length(sel()$t)) format(max(sel()$t), "%Y-%m-%d %H:%M", tz = "UTC"), error = function(e) NULL)) })
  wb_needs_server(output, reactive(wb_needs_items(if (is.function(std$needs$check)) std$needs$check() else NULL, std$review$items())))   # core workbench.R
  qpreset <- reactiveVal(NULL)
  wb_period_server(input, output, session, E$period, sel, reactive({ p <- part(); if (nzchar(p$site)) p$site else p$serial }), params_all, function() open_table("period"), preset = qpreset)   # core workbench.R
  wb_review_handlers(std$review, function(r) { tb <- tables(); x <- tb[serial == as.character(r$serial) & (site == r$where | kind == "baro")]   # the deployment of the flag's year
      y <- x[grepl(substr(r$start, 1, 4), label, fixed = TRUE)]; k <- if (nrow(y)) y$key[1] else if (nrow(x)) x$key[1] else NA_character_
      wb_jump_to(session, input, key, jump, set_view, k, intersect(trim(strsplit(r$parameter, ",", fixed = TRUE)[[1]]), isolate(params_all())), r$start, r$end) }, qpreset)   # Needs you (core workbench.R)
  invisible(ctx)
}
