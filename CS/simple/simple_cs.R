# CS/simple/simple_cs.R  --  The Simple view of CS (M 28 Oct): a field check for someone new to the data, in four steps.
# Import: the folders and Read the data. View: one tile per station (last record, days with data, lowest battery) and
# one plot of four plain parameters. Edit: names, places and which station each logger belongs to (core simple.R).
# Export: the Simple report and the daily values. The Standard server runs underneath (server(embedded = TRUE)), so a
# run, the outputs and Needs you are the same; Edit writes only stations.csv, station_info.csv and, for a rename, the
# tables RENAMES lists (R/config.R). Data values are never changed here. Sourced by app.R.

# The four parameters View offers, by column name in the daily files (output/derived), first match wins.
SP_CS_PARAMS <- list(
  "Air temperature" = list(rx = c("^air_temp$", "^air_temp_avg$", "^air_temp"), unit = "°C"),
  "Rain and snow"   = list(rx = c("^precip", "^rain"), unit = "mm"),
  "Snow depth"      = list(rx = c("^snow_depth$", "^snow_depth"), unit = ""),
  "Sunlight"        = list(rx = c("^sw_in$", "^sw_in"), unit = "W/m²"))
SP_CS_STOPPED_DAYS <- 30                      # a station whose last record is this much older than the newest has stopped

sp_cs_daily <- function(pd, station, stream) {
  f <- file.path(pd, "output", "derived", paste0(safe_name(paste(station, stream, sep = "__")), "_daily.csv")); if (!file.exists(f)) return(NULL)
  # safe: a daily file that cannot be read shows as no daily values
  d <- tryCatch(fread(file = f), error = function(e) NULL); if (is.null(d) || !nrow(d) || !"Date" %in% names(d)) return(NULL)
  d[, Date := as.Date(Date)][] }
sp_cs_col <- function(d, what) { if (is.null(d)) return(NULL); nm <- names(d)[vapply(d, is.numeric, NA)]
  cs_home_pick(nm[!grepl("_(min|max|sd|n|quality|flag)$", nm)], SP_CS_PARAMS[[what]]$rx) }
sp_cs_chr <- function(d, cols) { d <- as.data.table(d); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x }); ensure_cols(d, cols) }
sp_cs_blank <- function(d, ...) { r <- as.data.table(as.list(stats::setNames(rep("", ncol(d)), names(d)))); x <- list(...)
  for (k in names(x)) set(r, j = k, value = as.character(x[[k]])); r }

# One row per station: its main table (the most records with daily values), first and last record, days with data over
# the days from first to last, the battery's 1st percentile against the parameter guide's usual lowest, and how far its
# last record is behind the newest of the project. daily: the main table's daily values, by station.
sp_cs_facts <- function(o, pd) {
  s <- copy(o$stations); if (is.null(s) || !nrow(s)) return(NULL)
  s[, `:=`(first_t = as.POSIXct(first, tz = "UTC"), last_t = as.POSIXct(last, tz = "UTC"))]; newest <- max(s$last_t, na.rm = TRUE)
  daily <- list()
  f <- rbindlist(lapply(split(s, by = "station"), function(x) { x <- x[order(-records)]; k <- x$station[1]; d <- NULL
    for (m in x$stream) { d <- sp_cs_daily(pd, k, m); if (!is.null(d)) break }
    daily[[k]] <<- d
    days <- if (!is.null(d) && "records" %in% names(d)) sum(d$records > 0, na.rm = TRUE) else NA_integer_
    span <- as.integer(as.Date(max(x$last_t)) - as.Date(min(x$first_t))) + 1L
    b <- if (!is.null(o$pstats) && nrow(o$pstats)) o$pstats[station == k & grepl("^battery_v", parameter)] else data.table()
    lo <- suppressWarnings(min(as.numeric(b$p01 %||% numeric()), na.rm = TRUE)); us <- suppressWarnings(min(as.numeric(b$typical_min %||% numeric()), na.rm = TRUE))
    data.table(station = k, first = min(x$first_t), last = max(x$last_t), records = sum(x$records), days = days, span = span,
               battery = if (is.finite(lo)) lo else NA_real_, battery_usual = if (is.finite(us)) us else NA_real_,
               behind = as.numeric(difftime(newest, max(x$last_t), units = "days"))) }))
  need <- CFG$min_complete %||% 0.8
  f[, level := fifelse(records == 0, "off", fifelse(behind > SP_CS_STOPPED_DAYS, "error",
                fifelse((!is.na(battery) & !is.na(battery_usual) & battery < battery_usual) | (!is.na(days) & days / span < need), "warn", "ok")))]
  f[, note := fifelse(level == "error", paste0("No data after ", format(last, "%Y-%m-%d"), "."),
              fifelse(level == "warn" & !is.na(battery) & !is.na(battery_usual) & battery < battery_usual,
                      sprintf("Battery fell to %.1f V; the project's guide gives %.1f V as the usual lowest.", battery, battery_usual),
              fifelse(level == "warn", sprintf("Days with data cover %.0f %% of the period.", 100 * days / span), "")))]
  list(f = f[order(match(level, c("error", "warn", "ok", "off")), station)], daily = daily, newest = newest)
}

# ---- interface ---------------------------------------------------------------------------------------------------------------
sp_cs_import_ui <- function() tagList(
  p(class = "sp-lead", "Point ovid at the folders that hold the logger files (.dat), then press Read the data. The raw files are never changed."),
  div(class = "sp-cols",
    div(class = "sp-card", div(class = "sp-h", "Folders"),
      textAreaInput("raw_dirs", "Logger files, one folder per line", paste(SETTINGS$raw_dirs, collapse = "\n"), rows = 3, width = "100%"),
      ovid_browse_button("raw_dirs"), uiOutput("raw_check"),
      textInput("project_dir", "Project folder (the results are kept here)", SETTINGS$project_dir, width = "100%"), ovid_browse_button("project_dir"),
      tags$details(tags$summary("More options"),
        textInput("skip_tables", "Logger tables to leave out", paste(SETTINGS$skip_tables, collapse = ", "), width = "100%"),
        ovid_switch("import_cr10x", "Include CR10X files (headerless mixed-array)", isTRUE(SETTINGS$import_cr10x), width = "100%"),
        ovid_switch("incremental", "Reuse what has not changed since the last read (much faster)", !isFALSE(SETTINGS$incremental), width = "100%"),
        selectInput("station_format", "Save station files as", width = "100%", selected = SETTINGS$station_format,
                    c("CSV" = "csv", "Parquet (smaller, faster plots)" = "parquet", "Both" = "both"))),
      div(class = "sp-row", if (VIEWER) span(class = "sp-dim", "A viewer copy: the data are already read.") else actionButton("run", "Read the data", class = "sp-btn"))),
    div(class = "sp-card", div(class = "sp-h", "What was read"), uiOutput("sp_read"))))

sp_cs_view_ui <- function() tagList(
  uiOutput("sp_hero"), uiOutput("sp_tiles", class = "sp-tiles"),
  div(class = "sp-card",
    div(class = "sp-row", style = "margin-top:0;justify-content:space-between",
      tags$b(textOutput("sp_plot_title", inline = TRUE)),
      radioButtons("sp_period", NULL, c("Last 30 days" = "30", "Last 12 months" = "365", "All" = "all"), selected = "365", inline = TRUE)),
    radioButtons("sp_param", NULL, names(SP_CS_PARAMS), inline = TRUE),
    ts_view("sp_plot", 260), p(class = "sp-dim sp-small", textOutput("sp_plot_note", inline = TRUE))),
  uiOutput("sp_next"))

sp_cs_export_ui <- function() div(class = "sp-cols",
  div(class = "sp-card", div(class = "sp-h", "Simple report"),
    p("One page: what was read, where and when, four figures, the daily air temperature, when each station has data, a table of the stations, and the methods with their sources."),
    radioButtons("sp_rpt_fmt", NULL, c("Web page (HTML)" = "html", "PDF" = "pdf"), inline = TRUE),
    div(class = "sp-row", downloadButton("sp_rpt", "Download the report", class = "sp-btn", icon = NULL))),
  div(class = "sp-card", div(class = "sp-h", "Daily values"),
    p("One CSV per station with a value per day (means, and totals for rain and snow), and a README with the methods, their sources and the run's record."),
    div(class = "sp-row", downloadButton("sp_csv", "Download the daily values (ZIP)", class = "sp-btn2", icon = NULL)),
    p(class = "sp-dim sp-small", "Any period, the raw records or the full report: the Standard view, Reports and export.")))

simple_ui <- function(req) {
  pages <- Filter(function(x) inherits(x, "ovid_tool"), cs_flow_pages(sources = FALSE))           # the Standard pages, hidden: their
  hidden <- lapply(pages, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))   # inputs exist for its server
  simple_page("CS", GB, import = sp_cs_import_ui(), view = sp_cs_view_ui(), edit = simple_edit_ui("spe", "station", "loggers"), export = sp_cs_export_ui(),
              project = if (!VIEWER) projects_switch_ui("proj"), hidden = hidden, head = std_head())
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

  last_run <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$runs)) NULL else o$runs[.N] })
  output$sp_read <- renderUI({ o <- outs(); r <- last_run()
    if (is.null(o)) return(p(class = "sp-dim", "Nothing read yet."))
    tags$dl(class = "sp-kv",
      tags$dt("Files"), tags$dd(if (!is.null(r)) sprintf("%s found, %s read", num(as.integer(r$files_found)), num(as.integer(r$files_read))) else num(nrow(o$manifest))),
      tags$dt("Loggers"), tags$dd(num(uniqueN(o$registry$logger_id))),
      tags$dt("Stations"), tags$dd(num(uniqueN(o$stations$station))),
      tags$dt("Records"), tags$dd(num(sum(o$stations$records))),
      tags$dt("Period"), tags$dd(if (nrow(o$stations)) paste(substr(min(o$stations$first), 1, 10), "to", substr(max(o$stations$last), 1, 10)) else "—"),
      tags$dt("Problems"), tags$dd(if (nrow(o$problems)) paste(nrow(o$problems), "(listed in the Standard view, Loggers and files)") else "none"),
      tags$dt("Read"), tags$dd(if (!is.null(r)) r$started else format(o$when, "%Y-%m-%d %H:%M")))
  })

  # ---- View
  facts <- reactive({ o <- outs(); req(o); sp_cs_facts(o, pd()) })
  pick <- reactiveVal(NULL)
  observeEvent(input$sp_pick, pick(input$sp_pick))
  cur <- reactive({ x <- facts(); req(x); k <- pick(); if (is.null(k) || !k %in% x$f$station) x$f[level != "off"]$station[1] %||% x$f$station[1] else k })
  needs <- reactive(tryCatch(nrow(std$review$items()), error = function(e) 0L))
  output$sp_hero <- renderUI({ o <- outs()
    if (is.null(o)) return(simple_hero("off", "Nothing read yet", "Choose the folders and press Read the data.", tags$button(type = "button", class = "btn sp-btn", onclick = go_step("import"), "Import")))
    x <- facts(); f <- x$f; r <- last_run(); bad <- f[level %in% c("error", "warn")]
    lv <- if (any(f$level == "error")) "error" else if (nrow(bad)) "warn" else "ok"
    head <- paste0(if (!is.null(r)) paste(num(as.integer(r$files_read)), "files read · ") else "", nrow(f), " stations · ",
                   if (!any(f$level == "error")) "all reporting" else paste(sum(f$level == "error"), "stopped"))
    sub <- paste0("Newest record ", format(x$newest, "%Y-%m-%d %H:%M"), ". ",
                  if (nrow(bad)) paste0(nrow(bad), if (nrow(bad) == 1) " station has" else " stations have", " something to look at.") else "Nothing to look at.")
    simple_hero(lv, head, sub, if (!VIEWER) tags$button(type = "button", class = "btn sp-btn2", onclick = "document.getElementById('run').click()", "Read again")) })
  output$sp_tiles <- renderUI({ x <- facts(); f <- x$f; k <- cur()
    lapply(seq_len(nrow(f)), function(i) { r <- f[i]; d <- x$daily[[r$station]]; a <- sp_cs_col(d, "Air temperature")
      simple_tile(r$station, r$level, spark = if (!is.null(a)) simple_spark(utils::tail(d[[a]], 365), level = r$level),
        facts = list(`Last record` = format(r$last, "%Y-%m-%d %H:%M"),
                     `Days with data` = if (is.na(r$days)) "—" else sprintf("%s / %s", num(r$days), num(r$span)),
                     `Battery, lowest` = if (is.na(r$battery)) "—" else sprintf("%.1f V", r$battery)),
        note = if (nzchar(r$note)) r$note, on = identical(r$station, k)) }) })
  sel_daily <- reactive({ x <- facts(); x$daily[[cur()]] })
  output$sp_plot_title <- renderText(cur())
  plot_data <- reactive({ d <- sel_daily(); w <- input$sp_param %||% names(SP_CS_PARAMS)[1]; p <- sp_cs_col(d, w)
    if (is.null(d) || is.null(p)) return(NULL)
    n <- input$sp_period %||% "365"; if (n != "all") d <- d[Date > max(Date) - as.integer(n)]
    list(d = d, p = p, w = w) })
  output$sp_plot_note <- renderText({ x <- plot_data()
    if (is.null(x)) return(paste0(cur(), " has no daily ", tolower(input$sp_param %||% ""), "."))
    paste0("Daily ", if (grepl("^Rain", x$w)) "total" else "mean", " of ", x$p, ", quality checked as the project sets (level L2); a day counts when at least ",
           round(100 * (CFG$min_complete %||% 0.8)), " % of its records are there (WMO-No. 1203).") })
  observe({ req(identical(input$simple_step, "view")); x <- plot_data(); req(x)
    s <- list(list(name = x$w, key = x$p, unit = SP_CS_PARAMS[[x$w]]$unit, v = as.numeric(x$d[[x$p]])))
    t <- as.POSIXct(x$d$Date) + 43200
    session$onFlushed(function() ts_view_send(session, "sp_plot", t, s, zone = "UTC"), once = TRUE) })
  output$sp_next <- renderUI({ req(outs()); n <- needs()
    div(class = "sp-row",
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("edit"), "✎ Fix a name or place"),
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("export"), "⤓ Export or report"),
      div(class = "sp-fill"),
      if (n) span(class = "sp-dim", paste(n, if (n == 1) "thing waits" else "things wait", "for a decision in Needs you:")),
      tags$a(href = "?ui=standard", class = "d-to-view", `data-ui` = "standard", if (n) "Open in Standard" else "Assess in Standard ›")) })

  # ---- Edit
  cfgp <- function(f) file.path(pd(), "config", f)
  rd <- function(f, cols) { p <- cfgp(f); sp_cs_chr(if (file.exists(p)) read_cfg(p) else data.table(), cols) }
  rows <- reactive({ std$rv$tick; o <- outs(); req(nzchar(pd()))
    st <- rd("stations.csv", STATION_COLS); si <- rd("station_info.csv", SITE_COLS)
    reg <- if (!is.null(o) && nrow(o$registry)) unique(o$registry[, .(logger_id = as.character(logger_id), station = as.character(station))]) else data.table(logger_id = character(), station = character())
    man <- if (!is.null(o) && nrow(o$manifest)) o$manifest[status == "ok" & !is.na(first_ts), .(first = as.character(min(first_ts)), last = as.character(max(last_ts))), by = .(logger_id = as.character(logger_id))] else data.table(logger_id = character(), first = character(), last = character())
    ids <- unique(c(st$logger_id, reg$logger_id)); ids <- ids[nzchar(ids)]
    r <- rbindlist(lapply(ids, function(id) { s <- st[logger_id == id & nzchar(station)]
      lab <- paste(c(id, unique(st[logger_id == id & nzchar(seen_as)]$seen_as)[1]), collapse = " · "); lab <- sub(" · NA$", "", lab)
      if (nrow(s)) return(s[, .(first = { v <- start[nzchar(start)]; if (length(v)) min(v) else "" }, last = { v <- end[nzchar(end)]; if (length(v) == .N) max(v) else "" }), by = .(group = station)][, `:=`(item = id, label = lab)])
      g <- reg[logger_id == id]$station; data.table(item = id, label = lab, group = if (length(g) && !is.na(g[1])) g[1] else "", first = "", last = "") }), fill = TRUE)
    if (!nrow(r)) return(NULL)
    m <- man[match(r$item, man$logger_id)]
    i <- !nzchar(r$first); r[i, first := fifelse(is.na(m$first[i]), "", m$first[i])]; i <- !nzchar(r$last); r[i, last := fifelse(is.na(m$last[i]), "", m$last[i])]
    r[, first := substr(first, 1, 16)][, last := substr(last, 1, 16)]
    r[, `:=`(lat = suppressWarnings(as.numeric(si$latitude[match(group, si$station)])), lon = suppressWarnings(as.numeric(si$longitude[match(group, si$station)])))]
    r[, .(item, label, group, first, last, lat, lon)] })
  gaps <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$manifest)) return(NULL); m <- o$manifest[status == "ok"]
    # safe: no suggestion about a long gap; a drag then moves the whole record
    tryCatch(group_split_suggestions(data.table(serial = as.character(m$logger_id), first = as.character(m$first_ts), last = as.character(m$last_ts)),
                                     tryCatch(group_splits_read(pd()), error = function(e) NULL)), error = function(e) NULL) })
  touched <- c("stations.csv", "station_info.csv", vapply(c(RENAMES$station$cols, RENAMES$station$lists), `[`, "", 1))
  busy <- reactive({ invalidateLater(3000); k <- ls(OVID_PENDING); if (!length(k)) return(character())
    k <- k[dirname(k) == live_path(file.path(pd(), "config"))]; intersect(unique(basename(k)), touched) })
  simple_edit_server("spe", rows, apply = function(ops, who) { r <- sp_cs_apply(pd(), ops, who, isolate(rows())); std$rv$tick <- isolate(std$rv$tick) + 1; std$mark(); r },
    gap = function(item) { g <- isolate(gaps()); if (is.null(g)) NA_character_ else { x <- g[serial == item]; if (nrow(x)) x$at[1] else NA_character_ } },
    busy = busy, noun = "station", items = "loggers")

  # ---- Export
  doc <- function() { x <- facts(); o <- outs(); f <- x$f[level != "off"]; r <- last_run(); nm <- basename(pd()); if (identical(nm, "project")) nm <- basename(dirname(pd()))
    air <- rbindlist(lapply(f$station, function(k) { d <- x$daily[[k]]; a <- sp_cs_col(d, "Air temperature"); if (is.null(a)) NULL else data.table(station = k, Date = d$Date, v = as.numeric(d[[a]])) }))
    avail <- rbindlist(lapply(f$station, function(k) { d <- x$daily[[k]]; if (is.null(d) || !"records" %in% names(d)) return(NULL)
      d <- d[records > 0][order(Date)]; if (!nrow(d)) return(NULL); g <- cumsum(c(1, diff(as.numeric(d$Date)) > 1))
      d[, .(start = min(Date), end = max(Date) + 1), by = .(g = g)][, .(name = k, start, end)] }))
    need <- round(100 * (CFG$min_complete %||% 0.8)); ok <- sum(f$level == "ok"); cov <- sum(f$days, na.rm = TRUE) / sum(f$span[!is.na(f$days)])
    plot <- if (nrow(air)) ggplot2::ggplot(air[!is.na(v)], ggplot2::aes(Date, v, colour = station)) + ggplot2::geom_line(linewidth = 0.35) +
      ggplot2::scale_colour_viridis_d(end = 0.85) + ggplot2::labs(x = NULL, y = "Air temperature (°C)", colour = NULL) + ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) else NULL
    simple_report_doc(paste("Field check:", nm),
      meta = c(paste(nrow(f), "stations"), paste(format(min(f$first), "%Y-%m-%d"), "to", format(max(f$last), "%Y-%m-%d")), if (!is.null(r)) paste(num(as.integer(r$files_read)), "files read")),
      summary = sprintf("%d of %d stations were recording up to the newest record (%s). Days with a daily value cover %.0f %% of the days from each station's first to last record.%s",
                        nrow(f) - sum(f$level == "error"), nrow(f), format(x$newest, "%Y-%m-%d"), 100 * cov,
                        if (nrow(f) - ok) paste0(" ", nrow(f) - ok, " station", if (nrow(f) - ok > 1) "s have" else " has", " something to look at: see the table.") else ""),
      figures = c(Stations = as.character(nrow(f)), `Days with data` = sprintf("%.0f %%", 100 * cov), Records = num(sum(f$records)),
                  `Air temperature` = if (nrow(air)) sprintf("%.1f °C", mean(air[, .(m = mean(v, na.rm = TRUE)), by = station]$m, na.rm = TRUE)) else "—"),
      notes = c(Stations = "with records", `Days with data` = paste0("a day counts at ≥ ", need, " % of its records"), Records = "as logged, after the quality rules",
                `Air temperature` = "mean of each station's daily means (L2)"),
      plot = plot, plot_caption = "Daily mean air temperature at each station, quality checked as the project sets (level L2).",
      avail = avail, table = f[, .(Station = station, First = format(first, "%Y-%m-%d"), Last = format(last, "%Y-%m-%d"), `Days with data` = days, `Of days` = span,
                                    `Battery lowest (V)` = round(battery, 1), Note = note)], table_caption = "Stations",
      methods = c(paste0("Records read as logged; duplicate records and corrupt timestamps left out and clocks corrected as the project sets, every change in output/audit (ovid CS pipeline ", APP_VERSION, ")."),
                  "Quality rules and removals as the project sets them: level L2 (L0 raw, L1 clock and duplicates, L2 quality checked).",
                  paste0("Daily mean or total given when at least ", need, " % of the day's records are there: WMO (2017) WMO Guidelines on the Calculation of Climate Normals, WMO-No. 1203; WMO (2018) Guide to Climatological Practices, WMO-No. 100."),
                  "Battery lowest: the 1st percentile of the logger's battery voltage, against the usual lowest in the project's parameter guide."),
      record = c(run = r$run_id %||% "", started = r$started %||% "", files = if (!is.null(r)) paste(r$files_found, "found,", r$files_read, "read") else "",
                 pipeline = r$pipeline_version %||% APP_VERSION, core = r$core_version %||% CORE_VERSION, `set-up` = r$config_hash %||% "",
                 audit = "output/audit: every removed or changed value", levels = "L0, L1A, L1B, L2"), pal = GB) }
  output$sp_rpt <- downloadHandler(filename = function() paste0("simple_report.", if (identical(input$sp_rpt_fmt, "pdf")) "pdf" else "html"),
    content = function(file) { req(outs()); fmt <- if (identical(input$sp_rpt_fmt, "pdf")) "pdf" else "html"
      withProgress(message = "Making the report", value = 0.5, { b <- tempfile("simple_report"); out <- simple_report_write(doc(), b, fmt, GB); file.copy(out[1], file, overwrite = TRUE) }) })
  output$sp_csv <- downloadHandler(filename = function() "daily_values.zip", contentType = "application/zip",
    content = function(file) { x <- facts(); r <- last_run(); dir <- tempfile("daily"); dir.create(dir)
      fs <- unlist(lapply(names(x$daily), function(k) { d <- x$daily[[k]]; if (is.null(d)) return(NULL); f <- file.path(dir, paste0("daily_", safe_name(k), ".csv")); fwrite(d, f); basename(f) }))
      writeLines(c(paste("Daily values from ovid CS, Simple view,", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
        "One file per station: its table with the most records. Date is the day; records the number of records that day.",
        "Each other column is the day's mean (or total for rain and snow; _min and _max the day's extremes) of that parameter,",
        "quality checked as the project sets (level L2).",
        paste0("A day has a value when at least ", round(100 * (CFG$min_complete %||% 0.8)), " % of its records are there: WMO (2017) WMO-No. 1203; WMO (2018) WMO-No. 100."), "",
        paste("Run:", r$run_id %||% ""), paste("Pipeline:", r$pipeline_version %||% APP_VERSION, " core:", r$core_version %||% CORE_VERSION),
        paste("Set-up:", r$config_hash %||% ""), "Every removed or changed value is in the project's output/audit folder."), file.path(dir, "README.txt"))
      zip::zip(file, c(fs, "README.txt"), root = dir) })
}

# Saves the Edit step's changes, in order: a rename goes through rename.R (every table RENAMES lists), and the loggers of
# a station that took its name from the data get it in stations.csv; a move sets the logger's station in stations.csv,
# for the whole record or, with a date, from then on (its rows end a minute before); a place goes in station_info.csv.
# Each table is written with the version before kept in config/history and the change log (backup_cfg). view: the rows
# before the changes (rows() above), for the stations named by the data alone.
sp_cs_apply <- function(pd, ops, who, view) {
  cfgp <- function(f) file.path(pd, "config", f); before <- list()
  keep <- function(f) if (is.null(before[[f]]) && file.exists(cfgp(f))) before[[f]] <<- copy(read_cfg(cfgp(f)))
  rd <- function(f, cols) sp_cs_chr(if (file.exists(cfgp(f))) read_cfg(cfgp(f)) else data.table(), cols)
  wr <- function(f, d, what) { keep(f); backup_cfg(cfgp(f), d, who, what); fwrite_safe(d, cfgp(f)) }
  eff <- copy(as.data.table(view))
  for (i in seq_len(nrow(ops))) { o <- ops[i]
    if (o$type == "rename") {
      plan <- rename_plan(pd, "station", o$group, o$to); for (f in unique(plan$file)) keep(f)
      if (nrow(plan)) rename_apply(pd, plan, who, "rename (Simple view)")
      ids <- unique(eff[group == o$group]$item); st <- rd("stations.csv", STATION_COLS)
      j <- which(st$logger_id %in% ids & !nzchar(st$station)); miss <- setdiff(ids, st$logger_id)
      if (length(j) || length(miss)) { if (length(j)) st[j, station := o$to]
        if (length(miss)) st <- rbind(st, rbindlist(lapply(miss, function(id) sp_cs_blank(st, logger_id = id, station = o$to, notes = "named in the Simple view"))))
        wr("stations.csv", st, "rename (Simple view)") }
    } else if (o$type == "move") {
      st <- rd("stations.csv", STATION_COLS)
      if (!o$item %in% st$logger_id) st <- rbind(st, sp_cs_blank(st, logger_id = o$item))
      j <- which(st$logger_id == o$item & st$station == o$group); if (!length(j)) j <- which(st$logger_id == o$item & !nzchar(st$station))
      if (!nzchar(o$at)) st[j, station := o$to]
      else { at <- as.POSIXct(o$at, tz = "UTC"); j <- j[!nzchar(st$end[j]) | st$end[j] >= o$at]
        nw <- copy(st[j]); nw[, `:=`(start = o$at, end = "", station = o$to, notes = "moved here in the Simple view")]
        st[j, end := format(at - 60, "%Y-%m-%d %H:%M")]; st <- rbind(st, nw) }
      wr("stations.csv", st, "move (Simple view)")
    } else if (o$type == "place") {
      si <- rd("station_info.csv", SITE_COLS); k <- which(si$station == o$group)
      if (!length(k)) { si <- rbind(si, sp_cs_blank(si, station = o$group)); k <- nrow(si) }
      si[k, `:=`(latitude = format(o$lat, digits = 9), longitude = format(o$lon, digits = 9))]
      wr("station_info.csv", si, "place (Simple view)") }
    eff <- simple_ops_preview(eff, ops[i]) }
  n <- nrow(ops)
  list(ok = TRUE, message = sprintf("Saved %d change%s. Read the data again (Import) to see %s in the results.", n, if (n > 1) "s" else "", if (n > 1) "them" else "it"),
       undo = function() for (f in names(before)) { backup_cfg(cfgp(f), before[[f]], who, "undo (Simple view)"); fwrite_safe(before[[f]], cfgp(f)) })
}
