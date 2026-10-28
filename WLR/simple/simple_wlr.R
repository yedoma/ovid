# WLR/simple/simple_wlr.R  --  The Simple view of WLR (M 28 Oct): a field check of the water levels, in four steps.
# Import: the folders, Read the data and a checklist (files paired, field sheet, sites, barologgers, tape readings).
# View: one tile per site (last record, water below the ground, its barologger, tape against logger at the last visit) and
# one plot of daily means. Edit: names, places, land cover and which site each logger is at, the datum shown (core simple.R).
# Export: the field check report, the daily water levels and the plots. The Standard server runs underneath
# (server(embedded = TRUE)), so a run, the outputs and Needs you are the same; Edit writes only deployment_sheet.csv and,
# for a rename, the tables RENAMES lists (R/config.R). Datum, hang length, offsets, rules and corrections are not edited
# here, and data values never. Sourced by app.R.

SP_WLR_STOPPED_DAYS <- 30                     # a site whose last record is this much older than the newest has stopped
SP_WLR_SHADE <- c("service_visit", "in_air", "out_of_water")   # flags shaded on the plot (the values are kept, not removed)
# What the level of a site is shown as, first one the record has: the water table against the ground, the depth to water
# below the reference point, else the water above the sensor (no hang length yet).
SP_WLR_LEVEL <- list(wt_ground_m = list(lab = "Water below ground", plot = "Below ground", note = "below (−) or above (+) the ground"),
                     dtw_m = list(lab = "Depth to water", plot = "Depth to water", note = "below the reference point"),
                     depth_m = list(lab = "Water above sensor", plot = "Water above sensor", note = "above the sensor (no hang length yet)"))

sp_wlr_chr <- function(d, cols) { d <- as.data.table(d); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x }); ensure_cols(d, cols) }
sp_wlr_blank <- function(d, ...) { r <- as.data.table(as.list(stats::setNames(rep("", ncol(d)), names(d)))); x <- list(...)
  for (k in names(x)) set(r, j = k, value = as.character(x[[k]])); r }
sp_wlr_unsited <- function(site, serial) is.na(site) | !nzchar(site) | site == serial | site == paste("Unlisted", serial)   # wlr_pipeline.R names a logger with no site "Unlisted <serial>"
sp_wlr_tape_m <- function() as.numeric(SETTINGS$simple_tape_m %||% CFG$simple_tape_m %||% 0.02)

# A site's record as daily means (core rule: a day counts with min_complete of its usual records, R/summaries.R), wide:
# date and one column per value. NULL when the site has no corrected file.
SP_WLR_DAILY <- new.env()                     # daily means by corrected file and its time stamp: read once per run
sp_wlr_daily <- function(pd, site) {
  f <- out_dir(pd, "corrected", paste0(safe_name(site), ".csv")); if (!file.exists(f)) return(NULL)
  key <- paste(f, file.mtime(f)); if (!is.null(SP_WLR_DAILY[[key]])) return(SP_WLR_DAILY[[key]])
  # safe: a corrected file that cannot be read shows as no daily values
  d <- tryCatch({ h <- names(fread(f, nrows = 0)); fread(f, select = intersect(h, c(grep("^TIMESTAMP", h, value = TRUE)[1], "site", SUMMARY_PARAMS))) }, error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  tc <- grep("^TIMESTAMP", names(d), value = TRUE)[1]; if (is.na(tc)) return(NULL); setnames(d, tc, "TIMESTAMP")
  if (!inherits(d$TIMESTAMP, "POSIXct")) d[, TIMESTAMP := as.POSIXct(as.character(TIMESTAMP), tz = "UTC")]; d[, site := as.character(site)]
  for (v in intersect(SUMMARY_PARAMS, names(d))) set(d, j = v, value = suppressWarnings(as.numeric(d[[v]])))   # a value never measured reads as logical
  dd <- daily_of(d[!is.na(TIMESTAMP)], CFG$min_complete %||% 0.8); if (is.null(dd) || !nrow(dd)) return(NULL)
  w <- dcast(dd, date ~ parameter, value.var = "mean"); setorder(w, date); assign(key, w, envir = SP_WLR_DAILY); w[] }
sp_wlr_level_col <- function(d) { if (is.null(d)) return(NULL); k <- names(SP_WLR_LEVEL)[names(SP_WLR_LEVEL) %in% names(d)]
  k <- k[vapply(k, function(x) any(is.finite(d[[x]])), NA)]; if (length(k)) k[1] else NULL }

# One row per water site: first and last record, days with a daily mean over the days from first to last, the level shown
# (its latest daily mean), the barologger of the latest deployment, tape against logger at the last visit (cm), the
# deployment's standard uncertainty, and how far its last record is behind the newest of the project.
sp_wlr_facts <- function(o, pd) {
  s <- copy(o$summary); if (is.null(s) || !nrow(s)) return(NULL); s <- s[kind == "water" & !is.na(site) & nzchar(site)]; if (!nrow(s)) return(NULL)
  s[, `:=`(start = as.POSIXct(start, tz = "UTC"), end = as.POSIXct(end, tz = "UTC"))]; newest <- max(s$end, na.rm = TRUE)
  ck <- if (nrow(o$checks) && all(c("difference_m", "visit_time") %in% names(o$checks))) copy(o$checks)[!is.na(difference_m)] else data.table()
  un <- file.path(pd, "output", "audit", "uncertainty.csv")
  # safe: no uncertainty table (an older run) shows as no figure
  un <- if (file.exists(un)) tryCatch(fread(un), error = function(e) data.table()) else data.table()
  daily <- list(); lim <- sp_wlr_tape_m(); need <- CFG$min_complete %||% 0.8
  f <- rbindlist(lapply(split(s, by = "site"), function(x) { k <- x$site[1]; x <- x[order(start)]; d <- sp_wlr_daily(pd, k); daily[[k]] <<- d
    lc <- sp_wlr_level_col(d); lv <- if (!is.null(lc)) { v <- d[[lc]][is.finite(d[[lc]])]; utils::tail(v, 1) } else NA_real_
    span <- as.integer(as.Date(max(x$end)) - as.Date(min(x$start))) + 1L
    days <- if (is.null(d) || is.null(lc)) NA_integer_ else sum(is.finite(d[[lc]]))
    c1 <- if (nrow(ck)) ck[site == k][order(visit_time)] else data.table()
    tape <- if (nrow(c1)) -100 * as.numeric(utils::tail(c1$difference_m, 1)) else NA_real_           # tape minus logger (difference_m is logger minus tape)
    u <- if (nrow(un) && "deployment" %in% names(un)) un[deployment == utils::tail(x$deployment, 1)] else data.table()
    sig <- if (nrow(u)) { v <- c(if (identical(lc, "depth_m")) u$depth_sigma_m else u$dtw_sigma_m, u$depth_sigma_m); v <- suppressWarnings(as.numeric(v)); v <- v[is.finite(v)]; if (length(v)) v[1] else NA_real_ } else NA_real_
    baro <- as.character(utils::tail(x$primary, 1)); if (is.na(baro)) baro <- ""
    data.table(site = k, first = min(x$start), last = max(x$end), records = sum(as.numeric(x$records), na.rm = TRUE), loggers = uniqueN(x$serial),
               days = days, span = span, level_col = lc %||% "", level = lv, baro = baro, tape_cm = tape, visits = nrow(c1), sigma = sig,
               behind = as.numeric(difftime(newest, max(x$end), units = "days"))) }))
  f[, state := "ok"]                                                              # each test NA-safe: a value not known is not a finding
  f[(!is.na(tape_cm) & abs(tape_cm) > 100 * lim) | (!is.na(days) & days / span < need), state := "warn"]
  f[(!is.na(behind) & behind > SP_WLR_STOPPED_DAYS) | !nzchar(baro), state := "error"]
  f[is.na(records) | records == 0, state := "off"]
  f[, note := fifelse(!nzchar(baro), "No barologger: the water level cannot be worked out. Choose one in Standard, Barometric compensation.",
              fifelse(state == "error", paste0("No data after ", format(last, "%d %b %Y"), "."),
              fifelse(state == "warn" & !is.na(tape_cm) & abs(tape_cm) > 100 * lim,
                      sprintf("Tape and logger differ by %+.1f cm at the last visit (more than %g cm).", tape_cm, 100 * lim),
              fifelse(state == "warn", sprintf("Days with a daily mean cover %.0f %% of the period.", 100 * days / span), ""))))]
  list(f = f[order(match(state, c("error", "warn", "ok", "off")), site)], daily = daily, newest = newest)
}

# ---- interface ---------------------------------------------------------------------------------------------------------------
sp_wlr_import_ui <- function() tagList(
  p(class = "sp-lead", "Point ovid at the folders that hold the logger files (.hobo and .xle with their CSV exports) and the field sheet, then press Read the data. The raw files are never changed."),
  div(class = "sp-cols",
    div(class = "sp-card", div(class = "sp-h", "Folders"),
      textAreaInput("raw_dirs", "Logger files, one folder per line (each holds 2025/, 2024/ … and baro/2025/ …)", paste(SETTINGS$raw_dirs, collapse = "\n"), rows = 3, width = "100%"),
      ovid_browse_button("raw_dirs"), uiOutput("raw_check"),
      textInput("metadata_dir", "Field sheet folder (the newest WL_Metadata .xlsx in it)", SETTINGS$metadata_dir, width = "100%"), ovid_browse_button("metadata_dir"),
      textInput("project_dir", "Project folder (the results are kept here)", SETTINGS$project_dir, width = "100%"), ovid_browse_button("project_dir"),
      tags$details(tags$summary("More options"),
        textInput("dgps_dir", "dGPS folder (survey exports, .csv)", SETTINGS$dgps_dir, width = "100%"), ovid_browse_button("dgps_dir"),
        textInput("years", "Years to read (blank = every year with data), e.g. 2024, 2025", paste(SETTINGS$years, collapse = ", "), width = "100%")),
      div(class = "sp-row", if (VIEWER) span(class = "sp-dim", "A viewer copy: the data are already read.") else actionButton("run", "Read the data", class = "sp-btn"))),
    div(class = "sp-card", div(class = "sp-h", "What was read"), uiOutput("sp_read"), uiOutput("sp_checks"))),
  div(class = "sp-card", div(class = "sp-h", "Files"), uiOutput("sp_files")))

sp_wlr_view_ui <- function() tagList(
  uiOutput("sp_hero"), uiOutput("sp_tiles", class = "sp-tiles"),
  div(class = "sp-card",
    div(class = "sp-row", style = "margin-top:0;justify-content:space-between",
      tags$b(textOutput("sp_plot_title", inline = TRUE)),
      radioButtons("sp_period", NULL, c("Last 30 days" = "30", "Since install" = "all"), selected = "all", inline = TRUE)),
    div(class = "sp-row", style = "margin-top:0",
      radioButtons("sp_param", NULL, c("Water level" = "level", "Water temperature" = "temp"), inline = TRUE),
      radioButtons("sp_ref", NULL, c("Below ground" = "ground", "Elevation" = "elev"), inline = TRUE)),
    ts_view("sp_plot", 260), p(class = "sp-dim sp-small", textOutput("sp_plot_note", inline = TRUE))),
  uiOutput("sp_next"))

sp_wlr_export_ui <- function() div(class = "sp-cols",
  div(class = "sp-card", div(class = "sp-h", "Field check report"),
    p("One page: the period, sites and loggers; per site the days, how complete the record is, depth to water, tape against logger, its barologger and uncertainty; what needs a look; the methods with their sources."),
    textInput("sp_rpt_who", "Your initials (on the report)", width = "220px"),
    div(class = "sp-row", downloadButton("sp_rpt", "Download the report (HTML)", class = "sp-btn", icon = NULL))),
  div(class = "sp-card", div(class = "sp-h", "Daily water levels and plots"),
    p("One CSV per site with a value per day (date, depth to water, elevation, temperature and the flags of that day), with a README of the methods, their sources and the run's record."),
    div(class = "sp-row", downloadButton("sp_csv", "Daily water levels (.zip)", class = "sp-btn2", icon = NULL), downloadButton("sp_png", "Plots (.zip)", class = "sp-btn2", icon = NULL)),
    p(class = "sp-dim sp-small", "Every reading, any period, NetCDF or the full report: ", tags$a(href = simple_std_href("Export"), "Standard, Report"), ".")))

# The Import checklist (WLR's own: one row per thing a water level needs), in the Simple view's colours (core simple.css).
SP_WLR_CSS <- paste(".sp-checks{margin-top:10px;border-top:1px solid var(--sp-line)}",
  ".sp-check{display:flex;flex-wrap:wrap;align-items:baseline;gap:4px 10px;padding:7px 0;border-bottom:1px solid var(--sp-line)}",
  ".sp-check>.sp-dot{flex:none;width:9px;height:9px;border-radius:50%;background:var(--sp-ok);align-self:center}",
  ".sp-check.sp-warn>.sp-dot{background:var(--sp-warn)} .sp-check.sp-error>.sp-dot{background:var(--sp-err)} .sp-check.sp-error>b{color:var(--sp-err)}",
  ".sp-check>.sp-fill{flex:1}",
  ".sp-table{width:100%;border-collapse:collapse;font-size:13px;font-variant-numeric:tabular-nums} .sp-table th{text-align:left;font-weight:600;color:var(--sp-mut);padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line2)}",
  ".sp-table td{padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line);vertical-align:middle} .sp-table .form-group{margin:0}")
simple_ui <- function(req) {
  pages <- Filter(function(x) inherits(x, "ovid_tool"), wlr_flow_pages(sources = FALSE))           # the Standard pages, hidden: their
  hidden <- lapply(pages, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))   # inputs exist for its server
  simple_page("WLR", WLR_PAL, import = sp_wlr_import_ui(), view = sp_wlr_view_ui(), edit = simple_edit_ui("spe", "site", "loggers"), export = sp_wlr_export_ui(),
              project = if (!VIEWER) projects_switch_ui("proj"), hidden = hidden, head = tagList(std_head(), tags$style(HTML(SP_WLR_CSS))))
}

# ---- server ------------------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns (E, outs, pd, rv, mark, review).
simple_server <- function(input, output, session, std) {
  outs <- std$outs; pd <- std$pd
  steps <- simple_steps_server(session, function() c(import = !is.null(outs())))
  if (!is.null(isolate(outs()))) steps$go("view")
  ran <- reactiveVal(FALSE)
  observeEvent(input$run, ran(TRUE), priority = 10)
  observeEvent(outs(), if (isTRUE(ran()) && !is.null(outs())) { ran(FALSE); steps$go("view") })
  go_step <- function(k) sprintf("document.querySelector('.sp-step[data-step=\"%s\"]').click()", k)
  to_std <- function(page, label, site = NULL) tags$a(href = simple_std_href(page, list(site = site)), label)   # a page of Standard (core simple.R)

  last_run <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$runs)) NULL else o$runs[.N] })
  output$sp_read <- renderUI({ o <- outs(); r <- last_run()
    if (is.null(o)) return(p(class = "sp-dim", "Nothing read yet."))
    lg <- o$loggers; s <- o$summary[kind == "water"]
    tags$dl(class = "sp-kv",
      tags$dt("Files"), tags$dd(if (!is.null(r)) sprintf("%s found, %s read", num(as.integer(r$files)), num(as.integer(r$files_used))) else num(nrow(o$files))),
      tags$dt("Loggers"), tags$dd(sprintf("%d (%d water, %d baro)", nrow(lg), sum(lg$kind != "baro"), sum(lg$kind == "baro"))),
      tags$dt("Sites"), tags$dd(num(uniqueN(s$site))),
      tags$dt("Field sheet"), tags$dd(if (nrow(o$md)) paste("read:", paste(sort(unique(o$md$year)), collapse = ", ")) else "not found"),
      tags$dt("Period"), tags$dd(if (nrow(s)) paste(substr(min(as.character(s$start)), 1, 10), "to", substr(max(as.character(s$end)), 1, 10)) else "—"),
      tags$dt("Read"), tags$dd(if (!is.null(r)) r$started else format(o$when, "%Y-%m-%d %H:%M")))
  })
  # the checklist: what a water level needs, each with its state (ok, warn, error) and the page in Standard that fixes it
  output$sp_checks <- renderUI({ o <- outs(); if (is.null(o)) return(NULL)
    fl <- o$files; bin <- if (nrow(fl)) fl[format %in% c("hobo_binary", "solinst_xle") & status == "ok"] else data.table()
    pr <- if (nrow(bin)) sum(!is.na(bin$pair) & nzchar(bin$pair)) else 0L
    s <- o$summary[kind == "water"]; nosite <- s[sp_wlr_unsited(site, serial)]
    nobaro <- unique(s[is.na(primary) | !nzchar(primary)]$site)
    ck <- o$checks; dep_ck <- if (nrow(ck) && "difference_m" %in% names(ck)) unique(ck[!is.na(difference_m)]$deployment) else character(); notape <- setdiff(s$deployment, dep_ck)
    row <- function(lv, what, detail, fix = NULL) div(class = paste("sp-check", paste0("sp-", lv)), span(class = "sp-dot"), tags$b(what), span(class = "sp-dim", detail), div(class = "sp-fill"), fix)
    div(class = "sp-checks",
      row(if (!nrow(bin) || pr == nrow(bin)) "ok" else "warn", "Each logger file paired with its CSV", sprintf("%d / %d", pr, nrow(bin)), if (nrow(bin) && pr < nrow(bin)) to_std("Files", "Bring in files")),
      row(if (nrow(o$md)) "ok" else "warn", "Field sheet read", if (nrow(o$md)) paste("years", paste(sort(unique(o$md$year)), collapse = ", ")) else "none: sites and visits come from the deployment sheet"),
      row(if (!nrow(nosite)) "ok" else "warn", "Every water logger has a site", if (!nrow(nosite)) sprintf("%d of %d", nrow(s), nrow(s)) else paste(nrow(nosite), "without one: give it in Edit")),
      row(if (!length(nobaro)) "ok" else "error", "Every site has a barologger", if (!length(nobaro)) "through its baro group" else paste("none for", paste(nobaro, collapse = ", ")),
          if (length(nobaro)) to_std("Baro", "Barometric compensation")),
      row(if (!length(notape)) "ok" else "warn", "Tape readings at each visit", if (!length(notape)) sprintf("%d of %d deployments", nrow(s), nrow(s)) else paste(length(notape), "of", nrow(s), "deployments have none"),
          if (length(notape)) to_std("Visits", "Manual levels and drift"))) })
  output$sp_files <- renderUI({ o <- outs(); if (is.null(o) || !nrow(o$files)) return(p(class = "sp-dim", "No files read yet."))
    fl <- o$files[status == "ok"]; s <- o$summary
    site_of <- function(sr) { x <- s[serial == sr]; if (!nrow(x)) "" else paste(unique(x$site), collapse = ", ") }
    fl[, `:=`(type = fifelse(kind == "baro", "baro", fifelse(kind == "water", "water", as.character(kind))), what = fifelse(grepl("csv", format), "CSV export", fifelse(format == "hobo_binary", ".hobo", fifelse(format == "solinst_xle", ".xle", format))))]
    fl[, at := vapply(serial, site_of, "")]
    tb <- function(i) tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("File"), tags$th("Serial"), tags$th("Type"), tags$th("Site"), tags$th("Kind"))),
      tags$tbody(lapply(i, function(i) tags$tr(tags$td(class = "sp-mono", fl$name[i]), tags$td(class = "sp-mono", fl$serial[i]), tags$td(fl$type[i]),
        tags$td(if (nzchar(fl$at[i])) fl$at[i] else span(class = "sp-dim", "—")), tags$td(fl$what[i])))))
    n <- 12L; tagList(tb(seq_len(min(nrow(fl), n))),
      if (nrow(fl) > n) tags$details(tags$summary(sprintf("All %d files", nrow(fl))), tb(seq(n + 1L, nrow(fl))))) })

  # ---- View
  facts <- reactive({ o <- outs(); req(o); sp_wlr_facts(o, pd()) })
  pick <- reactiveVal(NULL)
  observeEvent(input$sp_pick, pick(input$sp_pick))
  cur <- reactive({ x <- facts(); req(x); k <- pick(); if (is.null(k) || !k %in% x$f$site) x$f[state != "off"]$site[1] %||% x$f$site[1] else k })
  needs <- reactive(tryCatch(nrow(std$review$items()), error = function(e) 0L))
  output$sp_hero <- renderUI({ o <- outs()
    if (is.null(o)) return(simple_hero("off", "Nothing read yet", "Choose the folders and press Read the data.", tags$button(type = "button", class = "btn sp-btn", onclick = go_step("import"), "Import")))
    x <- facts(); if (is.null(x)) return(simple_hero("warn", "No water level sites yet", "The loggers read have no site: give them one in Edit, or in the field sheet."))
    f <- x$f; r <- last_run(); bad <- f[state %in% c("error", "warn")]
    lv <- if (any(f$state == "error")) "error" else if (nrow(bad)) "warn" else "ok"
    head <- paste0(nrow(f), " sites · ", if (!any(f$state == "error")) "all reporting" else paste(sum(f$state == "error"), "need a look first"))
    sub <- paste0("Newest record ", format(x$newest, "%d %b %Y %H:%M"), ". ", if (nrow(bad)) paste0(bad$site[1], ": ", bad$note[1], if (nrow(bad) > 1) paste0(" And ", nrow(bad) - 1, " more below.")) else "Nothing to look at.")
    simple_hero(lv, head, sub, if (!VIEWER) tags$button(type = "button", class = "btn sp-btn2", onclick = "document.getElementById('run').click()", "Read again")) })
  output$sp_tiles <- renderUI({ x <- facts(); req(x); f <- x$f; k <- cur()
    lapply(seq_len(nrow(f)), function(i) { r <- f[i]; d <- x$daily[[r$site]]; lc <- if (nzchar(r$level_col)) r$level_col else NULL
      lab <- if (is.null(lc)) "Water level" else SP_WLR_LEVEL[[lc]]$lab
      lvl <- if (is.na(r$level)) "—" else if (identical(lc, "wt_ground_m")) sprintf("%.2f m %s", abs(r$level), if (r$level <= 0) "below" else "above") else sprintf("%.2f m", r$level)
      simple_tile(r$site, r$state, spark = if (!is.null(lc)) simple_spark(utils::tail(d[[lc]], 365), level = r$state),
        facts = stats::setNames(list(format(r$last, "%d %b %Y %H:%M"), lvl, if (nzchar(r$baro)) r$baro else "none",
                                     if (is.na(r$tape_cm)) "no tape reading" else sprintf("%+.1f cm", r$tape_cm)),
                                c("Last record", lab, "Barologger", "Tape − logger")),
        note = if (nzchar(r$note)) r$note, on = identical(r$site, k),
        link = if (!nzchar(r$baro)) list(label = "Open in Standard: Barometric compensation", page = "Baro", site = r$site)
               else if (r$state == "error") list(label = "Open in Standard: Needs you", page = "Needs", site = r$site)
               else if (!is.na(r$tape_cm) && abs(r$tape_cm) > 100 * sp_wlr_tape_m()) list(label = "Open in Standard: Manual levels and drift", page = "Visits", site = r$site)) }) })
  sel_daily <- reactive({ x <- facts(); x$daily[[cur()]] })
  output$sp_plot_title <- renderText(cur())
  plot_data <- reactive({ d <- sel_daily(); if (is.null(d)) return(NULL); w <- input$sp_param %||% "level"
    p <- if (w == "temp") (if ("temp_c" %in% names(d)) "temp_c") else if (identical(input$sp_ref, "elev") && "elev_m" %in% names(d) && any(is.finite(d$elev_m))) "elev_m" else sp_wlr_level_col(d)
    if (is.null(p)) return(NULL)
    n <- input$sp_period %||% "all"; if (n != "all") d <- d[date > max(date) - as.integer(n)]
    list(d = d, p = p) })
  output$sp_plot_note <- renderText({ x <- plot_data()
    if (is.null(x)) return(paste0(cur(), " has no daily values for this yet."))
    el <- identical(input$sp_ref, "elev") && x$p != "elev_m" && !identical(input$sp_param, "temp")
    paste0(if (el) "No elevation yet: the datum is set in Standard (Datum and offsets). Shown instead: " else "",
           "Daily mean of the ", tolower(switch(x$p, temp_c = "water temperature", elev_m = "water elevation", SP_WLR_LEVEL[[x$p]]$lab %||% x$p)), ", quality checked as the project sets; water density from temperature after Tanaka et al. (2001). A day counts when at least ",
           round(100 * (CFG$min_complete %||% 0.8)), " % of its records are there (WMO-No. 1203). Shaded: service visits, logger in the air or out of the water (flagged, kept). Dots: tape readings.") })
  observe({ req(identical(input$simple_step, "view")); x <- plot_data(); req(x); o <- outs(); k <- cur()
    lab <- switch(x$p, temp_c = list("Water temperature", "°C"), elev_m = list("Water elevation", "m"), list(SP_WLR_LEVEL[[x$p]]$plot %||% x$p, "m"))
    t <- as.POSIXct(x$d$date) + 43200
    ck <- if (nrow(o$checks) && all(c("difference_m", "visit_time") %in% names(o$checks)) && x$p != "temp_c") o$checks[site == k & !is.na(difference_m) & !is.na(visit_time)] else data.table()
    mk <- if (nrow(ck)) { tv <- as.POSIXct(ck$visit_time, tz = "UTC"); v <- x$d[[x$p]][match(as.Date(tv), x$d$date)] - ck$difference_m * (if (x$p == "dtw_m") -1 else 1)
      list(t = as.numeric(tv), v = v, label = sprintf("tape %+.1f cm against the logger", -100 * ck$difference_m)) } else NULL
    ground <- if (identical(x$p, "wt_ground_m")) list(name = "Ground", v = rep(0, nrow(x$d))) else if (identical(x$p, "elev_m") && "wt_ground_m" %in% names(x$d))
      list(name = "Ground", v = x$d$elev_m - x$d$wt_ground_m) else NULL
    fl <- if (nrow(o$flags)) o$flags[site == k & check %in% SP_WLR_SHADE] else data.table()
    sp <- if (nrow(fl)) data.frame(t0 = as.numeric(as.POSIXct(fl$start, tz = "UTC")), t1 = as.numeric(as.POSIXct(fl$end, tz = "UTC")), label = fl$check) else NULL
    s <- list(c(list(name = lab[[1]], key = x$p, unit = lab[[2]], v = as.numeric(x$d[[x$p]])), if (!is.null(mk)) list(marks = mk), if (!is.null(ground)) list(ref = ground)))
    session$onFlushed(function() ts_view_send(session, "sp_plot", t, s, zone = "UTC", spans = sp), once = TRUE) })
  output$sp_next <- renderUI({ req(outs()); n <- needs()
    div(class = "sp-row",
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("edit"), "✎ Fix a name or place"),
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("export"), "⤓ Export or report"),
      div(class = "sp-fill"),
      if (n) span(class = "sp-dim", paste(n, if (n == 1) "thing waits" else "things wait", "for a decision in Needs you:")),
      to_std(if (n) "Needs" else "Home", if (n) "Open in Standard" else "Assess in Standard ›")) })

  # ---- Edit
  cfgp <- function(f) file.path(pd(), "config", f)
  rows <- reactive({ std$rv$tick; o <- outs(); req(nzchar(pd()))
    sh <- sp_wlr_sheet(pd()); s <- if (!is.null(o) && nrow(o$summary)) o$summary[kind == "water"] else data.table(serial = character(), site = character(), kind = character(), start = character(), end = character())
    lg <- if (!is.null(o) && nrow(o$loggers)) o$loggers else data.table(serial = character(), kind = character(), first = character(), last = character())
    md <- if (!is.null(o) && nrow(o$md) && all(c("serial", "site", "year") %in% names(o$md))) o$md[order(year)] else data.table(serial = character(), site = character())
    ids <- unique(c(sh$serial[nzchar(sh$serial)], as.character(s$serial), as.character(lg$serial))); if (!length(ids)) return(NULL)
    r <- rbindlist(lapply(ids, function(id) { x <- s[serial == id]; baro <- identical(lg$kind[match(id, lg$serial)], "baro"); lab <- paste0(id, if (baro) " · baro" else "")
      if (nrow(x)) return(x[, .(first = substr(min(as.character(start)), 1, 16), last = substr(max(as.character(end)), 1, 16)), by = .(group = site)][, `:=`(item = id, label = lab, kind = "")])
      h <- sh[serial == id]; m <- md[as.character(serial) == id & !is.na(site) & nzchar(site)]; l <- lg[serial == id]
      data.table(item = id, label = lab, kind = if (baro) "baro" else "", group = if (nrow(h) && nzchar(h$site[nrow(h)])) h$site[nrow(h)] else if (nrow(m)) utils::tail(m$site, 1) else "",
                 first = if (nrow(l)) substr(as.character(l$first[1]), 1, 16) else "", last = if (nrow(l)) substr(as.character(l$last[1]), 1, 16) else "") }), fill = TRUE)
    r[sp_wlr_unsited(group, item), group := ""]
    pl <- sh[nzchar(site) & (nzchar(latitude) | nzchar(longitude)), .(lat = suppressWarnings(as.numeric(latitude[1])), lon = suppressWarnings(as.numeric(longitude[1]))), by = site]
    r[, `:=`(lat = pl$lat[match(group, pl$site)], lon = pl$lon[match(group, pl$site)])]
    r[, .(item, label, kind, group, first, last, lat, lon)] })
  touched <- c(DEPLOY_SHEET, vapply(c(RENAMES$site$cols, RENAMES$site$lists), `[`, "", 1), RENAMES$site$main[1])
  busy <- reactive({ invalidateLater(3000); k <- ls(OVID_PENDING); if (!length(k)) return(character())
    k <- k[dirname(k) == live_path(file.path(pd(), "config"))]; intersect(unique(basename(k)), touched) })
  # each site's land cover (edited here, in the deployment sheet) and its datum (shown; set in Standard, Datum and offsets)
  land_cover <- function(k) { std$rv$tick; o <- outs(); sh <- sp_wlr_sheet(pd()); v <- sh$land_cover[sh$site == k & nzchar(sh$land_cover)]; if (length(v)) return(v[1])
    if (!is.null(o) && nrow(o$md) && all(c("site", "land_cover") %in% names(o$md))) { w <- o$md[site == k & !is.na(land_cover) & nzchar(land_cover)]$land_cover; if (length(w)) return(utils::tail(w, 1)) }; "" }
  datum <- function(k) { o <- outs(); x <- if (!is.null(o) && nrow(o$summary)) o$summary[site == k] else data.table(); if (!nrow(x)) return("no deployment yet"); x <- x[.N]
    z <- suppressWarnings(as.numeric(x$z_ref)); h <- suppressWarnings(as.numeric(x$T_ST))
    paste0(if (is.finite(z)) sprintf("reference %s at %.3f m", x$reference, z) else "no surveyed reference yet", if (is.finite(h)) sprintf(" · hang length %.3f m", h) else "") }
  simple_edit_server("spe", rows, apply = function(ops, who) { r <- sp_wlr_apply(pd(), ops, who, isolate(rows()), isolate(outs())); std$rv$tick <- isolate(std$rv$tick) + 1; std$mark(); r },
    busy = busy, noun = "site", items = "loggers",
    fields = list(list(id = "land_cover", label = "Land cover", type = "text", get = land_cover),
                  list(id = "datum", label = "Datum", type = "readonly", get = datum, link = "Datum")))
  # ---- Export
  table_of <- function() { x <- facts(); f <- x$f[state != "off"]; need <- CFG$min_complete %||% 0.8
    st <- function(k, fn) { d <- x$daily[[k]]; lc <- if (!is.null(d) && "dtw_m" %in% names(d) && any(is.finite(d$dtw_m))) "dtw_m" else NULL; if (is.null(lc)) NA_real_ else round(fn(d[[lc]], na.rm = TRUE), 2) }
    f[, .(Site = site, Days = span, `Complete %` = round(100 * days / span), `Min DTW (m)` = vapply(site, st, 0, fn = min), `Mean DTW (m)` = vapply(site, st, 0, fn = mean),
          `Max DTW (m)` = vapply(site, st, 0, fn = max), `Tape − logger (cm)` = round(tape_cm, 1), Baro = fifelse(nzchar(baro), baro, "none"), `± m` = round(sigma, 3))] }
  doc <- function() { x <- facts(); o <- outs(); f <- x$f[state != "off"]; r <- last_run(); nm <- basename(pd()); if (identical(nm, "project")) nm <- basename(dirname(pd()))
    need <- round(100 * (CFG$min_complete %||% 0.8)); fl <- if (nrow(o$flags)) o$flags[site %in% f$site, .N, by = check][order(-N)] else data.table()
    bad <- f[state %in% c("error", "warn")]
    lvl <- rbindlist(lapply(f$site, function(k) { d <- x$daily[[k]]; lc <- sp_wlr_level_col(d); if (is.null(lc)) NULL else data.table(site = k, date = d$date, v = as.numeric(d[[lc]]), what = lc) }))
    plot <- if (nrow(lvl)) ggplot2::ggplot(lvl[!is.na(v)], ggplot2::aes(date, v, colour = site)) + ggplot2::geom_line(linewidth = 0.35) +
      ggplot2::scale_colour_viridis_d(end = 0.85) + ggplot2::labs(x = NULL, y = "m", colour = NULL) + ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) else NULL
    who <- trim(input$sp_rpt_who %||% "")
    simple_report_doc(paste("Field check:", nm),
      meta = c(paste(format(min(f$first), "%d %b %Y"), "to", format(max(f$last), "%d %b %Y")), paste(nrow(f), "sites"), paste(sum(o$loggers$kind != "baro"), "water loggers,", sum(o$loggers$kind == "baro"), "barologgers"),
               if (!is.null(r)) paste("run", substr(r$started, 1, 10)), if (nzchar(who)) paste("checked by", who)),
      summary = paste0(nrow(f) - sum(f$state == "error"), " of ", nrow(f), " sites were recording up to the newest record (", format(x$newest, "%d %b %Y"), "). ",
                       if (nrow(bad)) paste0("To look at: ", paste0(bad$site, " (", sub("\\.$", "", bad$note), ")", collapse = "; "), ". ") else "Nothing to look at. ",
                       if (nrow(fl)) paste0("Flags (values kept): ", paste0(fl$N, " ", gsub("_", " ", fl$check), collapse = ", "), ".") else ""),
      figures = c(Sites = as.character(nrow(f)), `Complete` = sprintf("%.0f %%", 100 * sum(f$days, na.rm = TRUE) / sum(f$span[!is.na(f$days)])),
                  `Tape checks` = as.character(sum(f$visits)), Loggers = as.character(nrow(o$loggers))),
      notes = c(Sites = "with a water level", Complete = paste0("days with a daily mean (≥ ", need, " % of records)"), `Tape checks` = "tape readings at visits", Loggers = "water and baro"),
      plot = plot, plot_caption = "Daily mean water level at each site (below the ground where known, else depth to water or water above the sensor), quality checked as the project sets.",
      table = table_of(), table_caption = "Sites",
      methods = c(paste0("Water pressure minus barometric pressure from the site's barologger (its baro group), as metres of water: h = ΔP·1000/(ρg), g = 9.80665 m/s², ρ from the logger's temperature: Tanaka, M., et al. (2001) Metrologia 38, 301-309; Freeman, L. A., et al. (2004) USGS TWRI 8-A3 (ovid WLR pipeline ", APP_VERSION, ")."),
                  "Depth to water and elevation from the hang length, the logger offset and the survey of the reference point: Cunningham, W. L., Schalk, C. W. (2011) USGS Techniques and Methods 1-A1 (GWPD 1, 16); Freeman et al. (2004).",
                  "Tape − logger: the water level from the tape at a visit minus the logger's at that time: Freeman et al. (2004).",
                  paste0("Complete %: days with a daily mean (at least ", need, " % of the day's records) over the days from first to last record: WMO (2017) WMO-No. 1203; WMO (2018) WMO-No. 100."),
                  "± m: the deployment's standard uncertainty (1σ), its components (logger, barometric, density, tape, survey) added in quadrature: JCGM 100:2008 (GUM); logger accuracy from the Onset and Solinst specification sheets.",
                  "Quality rules, removals and out-of-water periods as the project sets them; flagged values (service visits, logger in the air or out of the water) are kept."),
      record = c(run = r$run_id %||% "", started = r$started %||% "", files = if (!is.null(r)) paste(r$files, "found,", r$files_used, "read") else "",
                 pipeline = r$pipeline_version %||% APP_VERSION, core = r$core_version %||% CORE_VERSION, `set-up` = r$config_hash %||% "",
                 audit = "output/audit: every removed or changed value"), pal = WLR_PAL) }
  output$sp_rpt <- downloadHandler(filename = function() "field_check.html",
    content = function(file) { req(outs()); withProgress(message = "Making the report", value = 0.5, { b <- tempfile("field_check"); out <- simple_report_write(doc(), b, "html", WLR_PAL); file.copy(out[1], file, overwrite = TRUE) }) })
  readme <- function(r) c(paste("Daily water levels from ovid WLR, Simple view,", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    "One file per site. date is the day (logger time); dtw_m the depth to water below the reference point (m), elev_m the water elevation (m),",
    "temp_c the water temperature (°C), each the day's mean, quality checked as the project sets; flags the checks flagged that day (values kept).",
    paste0("A day has a value when at least ", round(100 * (CFG$min_complete %||% 0.8)), " % of its records are there: WMO (2017) WMO-No. 1203; WMO (2018) WMO-No. 100."),
    "Water density from temperature: Tanaka et al. (2001) Metrologia 38, 301-309. Depth to water and elevation: Cunningham and Schalk (2011) USGS TM 1-A1; Freeman et al. (2004) USGS TWRI 8-A3.", "",
    paste("Run:", r$run_id %||% ""), paste("Pipeline:", r$pipeline_version %||% APP_VERSION, " core:", r$core_version %||% CORE_VERSION),
    paste("Set-up:", r$config_hash %||% ""), "Every removed or changed value is in the project's output/audit folder.")
  output$sp_csv <- downloadHandler(filename = function() "daily_water_levels.zip", contentType = "application/zip",
    content = function(file) { x <- facts(); o <- outs(); dir <- tempfile("daily"); dir.create(dir)
      fs <- unlist(lapply(names(x$daily), function(k) { d <- x$daily[[k]]; if (is.null(d)) return(NULL); d <- ensure_cols(copy(d), c("dtw_m", "elev_m", "temp_c"))
        fl <- if (nrow(o$flags)) o$flags[site == k] else data.table()
        d[, flags := vapply(date, function(dy) { if (!nrow(fl)) return(""); h <- fl[as.Date(start) <= dy & as.Date(end) >= dy]; paste(unique(h$check), collapse = ";") }, "")]
        f <- file.path(dir, paste0("daily_", safe_name(k), ".csv")); fwrite(d[, .(date, dtw_m, elev_m, temp_c, flags)], f); basename(f) }))
      writeLines(readme(last_run()), file.path(dir, "README.txt")); zip::zip(file, c(fs, "README.txt"), root = dir) })
  output$sp_png <- downloadHandler(filename = function() "water_level_plots.zip", contentType = "application/zip",
    content = function(file) { x <- facts(); dir <- tempfile("plots"); dir.create(dir)
      fs <- unlist(lapply(names(x$daily), function(k) { d <- x$daily[[k]]; lc <- sp_wlr_level_col(d); if (is.null(lc)) return(NULL)
        g <- ggplot2::ggplot(d[is.finite(d[[lc]])], ggplot2::aes(date, .data[[lc]])) + ggplot2::geom_line(linewidth = 0.4, colour = simple_ink(WLR_PAL)) +
          ggplot2::labs(x = NULL, y = paste(SP_WLR_LEVEL[[lc]]$plot, "(m)"), title = k, subtitle = "Daily means, quality checked as the project sets") + ggplot2::theme_minimal(base_size = 10)
        f <- file.path(dir, paste0(safe_name(k), ".png")); ggplot2::ggsave(f, g, width = 18, height = 8, units = "cm", dpi = 200, bg = "white"); basename(f) }))
      zip::zip(file, fs, root = dir) })
}

# The project's deployment sheet as text (core deployments.R), with the columns WLR adds.
sp_wlr_sheet <- function(pd) { p <- dep_sheet_path(pd); sp_wlr_chr(if (file.exists(p)) read_cfg(p) else data.table(), c(DEPLOY_COLS, WLR_SHEET_OWN)) }

# Saves the Edit step's changes in the deployment sheet, in order: a rename goes through rename.R (every table RENAMES
# lists, the sheet among them); a site the field sheet alone names gets rows for its deployments under the new name. A
# move ends the logger's row the day before and starts a row at the new site on that day (or moves the whole record); a
# place sets latitude and longitude on the site's rows, a land cover its land_cover. Each write keeps the version before in config/history and the
# change log (backup_cfg). view: the rows before the changes (rows() above); o: the outputs, for the deployments' dates.
sp_wlr_apply <- function(pd, ops, who, view, o = NULL) {
  p <- dep_sheet_path(pd); before <- list()
  keep <- function(f) { fp <- file.path(pd, "config", f); if (is.null(before[[f]])) before[[f]] <<- if (file.exists(fp)) copy(read_cfg(fp)) else NA }
  wr <- function(d, what) { keep(DEPLOY_SHEET); dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE); backup_cfg(p, d, who, what); fwrite_safe(d, p) }
  deps <- if (!is.null(o) && nrow(o$summary)) o$summary else data.table(serial = character(), site = character(), start = character(), end = character())
  day <- function(x) substr(as.character(x), 1, 10)
  # rows for the loggers at a site named only by the field sheet or the files (no sheet row yet), so the sheet can carry it
  seed <- function(sh, at, to = at) { have <- sh$serial[sh$site == at]; x <- deps[deps$site == at & !as.character(deps$serial) %in% have]
    if (!nrow(x)) return(sh); rbind(sh, rbindlist(lapply(seq_len(nrow(x)), function(i) sp_wlr_blank(sh, serial = x$serial[i], site = to, from = day(x$start[i]), to = day(x$end[i]), notes = "from the Simple view")))) }
  for (i in seq_len(nrow(ops))) { q <- ops[i]
    if (q$type == "rename") {
      plan <- rename_plan(pd, "site", q$group, q$to); for (f in unique(plan$file)) keep(f)
      if (nrow(plan)) rename_apply(pd, plan, who, "rename (Simple view)")
      sh <- sp_wlr_sheet(pd); sh2 <- seed(sh, q$group, q$to); if (nrow(sh2) > nrow(sh)) wr(sh2, "rename (Simple view)")
    } else if (q$type == "move") {
      sh <- sp_wlr_sheet(pd); if (nzchar(q$group)) sh <- seed(sh, q$group)
      j <- which(sh$serial == q$item & (sh$site == q$group | sh$station == q$group)); if (!length(j)) j <- which(sh$serial == q$item & !nzchar(sh$site))
      if (!length(j)) { sh <- rbind(sh, sp_wlr_blank(sh, serial = q$item, site = q$group)); j <- nrow(sh) }
      if (!nzchar(q$at)) sh[j, `:=`(site = q$to, station = "")]
      else { at <- as.Date(substr(q$at, 1, 10)); j <- j[!nzchar(sh$to[j]) | sh$to[j] >= day(at)]
        nw <- copy(sh[j[length(j)]]); nw[, `:=`(from = format(at), to = "", site = q$to, station = "", notes = "moved here in the Simple view")]
        sh[j, to := format(at - 1)]; sh <- rbind(sh, nw) }
      wr(sh, "move (Simple view)")
    } else if (q$type == "field" && identical(q$item, "land_cover")) {
      sh <- seed(sp_wlr_sheet(pd), q$group); k <- which(sh$site == q$group)
      if (!length(k)) next
      sh[k, land_cover := q$to]
      wr(sh, "land cover (Simple view)")
    } else if (q$type == "place") {
      sh <- seed(sp_wlr_sheet(pd), q$group); k <- which(sh$site == q$group)
      if (!length(k)) next
      sh[k, `:=`(latitude = format(q$lat, digits = 9), longitude = format(q$lon, digits = 9))]
      wr(sh, "place (Simple view)") }
    view <- simple_ops_preview(view, ops[i]) }
  n <- nrow(ops)
  list(ok = TRUE, message = sprintf("Saved %d change%s. Read the data again (Import) to see %s in the results.", n, if (n > 1) "s" else "", if (n > 1) "them" else "it"),
       undo = function() for (f in names(before)) { fp <- file.path(pd, "config", f); b <- before[[f]]
         if (identical(b, NA)) { if (file.exists(fp)) { e <- read_cfg(fp)[0]; backup_cfg(fp, e, who, "undo (Simple view)"); fwrite_safe(e, fp) } }
         else { backup_cfg(fp, b, who, "undo (Simple view)"); fwrite_safe(b, fp) } })
}

