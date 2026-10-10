# CS/pages/raw_files.R  --  Raw files.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Raw files -------------------------------------------------------------
  output$rf_empty <- renderUI(if (is.null(outs())) empty_msg())
  rf_all <- reactive({ o <- outs(); req(o, NROW(o$manifest), "path" %in% names(o$manifest)); m <- copy(o$manifest); m[, file := basename(as.character(path))]
    rg <- unique(o$registry[, .(logger_id, table, station, stream)], by = c("logger_id", "table"))
    m <- merge(m, rg, by = c("logger_id", "table"), all.x = TRUE, sort = FALSE)
    fa <- o$file_rules; cnt <- function(rules, by_stage = NULL) { if (!nrow(fa)) return(rep(0, nrow(m)))
      x <- if (is.null(by_stage)) fa[rule %in% rules] else fa[stage == by_stage]; x <- x[, .(n = sum(ifelse(level == "cell", entries, n), na.rm = TRUE)), by = .(file = basename(source_file))]
      v <- x$n[match(m$file, x$file)]; ifelse(is.na(v), 0, v) }
    m[, `:=`(dups = cnt(c("duplicate_record", "overlap_conflict_merged", "station_overlap_merged")), conflicts = cnt(c("overlap_conflict", "station_overlap")),
             bad_time = cnt(c("bad_timestamp", "clock_fix_removed")), qc = cnt(NULL, "qc"))]
    fa2 <- o$file_audit                                  # per file: read = in output + dropped (core file_reconcile)
    if (nrow(fa2)) m <- merge(m, unique(fa2[, .(path, table, read, in_output, dropped, values_removed, reconciles, md5)], by = c("path", "table")), by = c("path", "table"), all.x = TRUE, sort = FALSE)
    else m[, `:=`(read = NA_integer_, in_output = NA_integer_, dropped = NA_integer_, values_removed = NA_integer_, reconciles = NA, md5 = NA_character_)]
    fr <- m_fr$data(); m[, rule := ""]
    if (!is.null(fr) && nrow(fr)) for (i in seq_len(nrow(fr))) { h <- m$file == basename(fr$file[i]) & (fr$table[i] == "" | m$table == fr$table[i])
      m[h, rule := if (tolower(fr$action[i]) == "ignore") "left out" else paste0("sent to ", fr$station[i], if (nzchar(fr$stream[i])) paste0(" / ", fr$stream[i]) else "")] }
    m })
  rf_view <- reactive({ m <- rf_all(); switch(input$rf_show, ok = m[status == "ok"], bad = m[!status %in% c("ok", "duplicate_file")], rule = m[rule != ""], drop = m[dups + conflicts + bad_time > 0], m) })
  rf_show_tbl <- function(m) m[, .(File = file, Status = status, Rule = rule, Station = fifelse(is.na(station), "", paste0(station, " / ", stream)), Logger = logger_id, Table = table,
                                   Records = rows, Start = first_ts, End = last_ts, Read = read, `In output` = in_output, Dropped = dropped,
                                   `Adds up` = fifelse(is.na(reconciles), "", fifelse(reconciles %in% TRUE, "yes", "no")), `Dropped as duplicates` = dups, `Conflicting values` = conflicts, `Bad timestamps` = bad_time,
                                   `Removed by quality rules` = qc, `Name in header` = station_name, MD5 = md5, Folder = dirname(path))]
  output$rf_tbl <- renderDT({ m <- rf_view(); validate(need(nrow(m) > 0, "No files in this view.")); status_style(ovid_dt(rf_show_tbl(m), selection = "multiple", page = 12, opts = list(scrollX = TRUE)), "Status") |>
    formatStyle("Adds up", color = styleEqual(c("yes", "no"), c(GB$green, GB$red))) }, server = TRUE)
  output$rf_dl <- downloadHandler(filename = function() paste0("raw_files_", Sys.Date(), ".csv"), content = function(f) fwrite(rf_show_tbl(rf_view()), f, dateTimeAs = "write.csv"))
  rf_sel <- reactive({ m <- rf_view(); s <- input$rf_tbl_rows_selected; s <- s[s <= nrow(m)]; if (!length(s)) NULL else m[s] })
  rf_why <- c(duplicate_file = "A byte-for-byte copy of another file; read once.", skipped_table = "Its table is on the list of tables to leave out (Project step).",
              unsupported_binary_TOB = "A TOB1 or TOB2 binary file (or a TOB3 file with an unreadable header). Convert it to TOA5 with LoggerNet or CardConvert.", excluded_by_user = "Left out by a rule on this page.", set_aside = "Its logger is set aside on the Stations page.",
              no_data_rows = "Header only, no records.", error = "Could not be read in the last run; see Loggers and files, Problems.", cr10x_import_off = "CR10X file marked not to import.",
              no_usable_arrays = "CR10X file with no array that could be given a timestamp.", unrecognised_format = "Not a Campbell TOA5 or mixed-array file.", empty_file = "Empty file.",
              truncated_header = "The header is incomplete.", unreadable = "The file could not be opened.")
  rf_rule_text <- c(duplicate_record = "records identical to a copy in another file (dropped, nothing lost)", overlap_conflict_merged = "records sharing a timestamp with another file, merged into the kept record",
                    overlap_conflict = "values that disagreed with the kept record at the same timestamp (logged with the value)", bad_timestamp = "records with an unreadable or impossible date",
                    clock_fix_removed = "records removed by a rule you set", clock_fix_shifted = "records whose time was shifted by a correction", sentinel = "logger overflow codes blanked",
                    range_min = "values below a limit", range_max = "values above a limit", spike = "single-point spikes", manual = "values removed by hand", flat_line = "stuck-sensor values removed",
                    station_overlap = "values that disagreed with another logger's at the same station and time", station_overlap_merged = "records merged with another logger's at the same station",
                    ignored_source = "records left out because the station is set to ignore", alias_collision = "values dropped where two raw columns map to one parameter")
  output$rf_detail <- renderUI({ r <- rf_sel(); if (is.null(r)) return(div(class = "readout", "Select one or more files in the table to inspect them or to set a rule."))
    one <- r[1]; o <- outs(); fa <- o$file_rules; a <- if (nrow(fa)) fa[basename(source_file) == one$file, .(n = sum(ifelse(level == "cell", entries, n), na.rm = TRUE)), by = rule][n > 0][order(-n)] else data.table()
    stn <- sort(unique(o$stations$station))
    div(class = "brushbox",
      h4(if (nrow(r) > 1) paste0(nrow(r), " files selected; showing ", one$file) else one$file),
      p(class = "dim", one$path),
      if (!VIEWER) div(class = "btnrow", actionButton("rf_rescan", if (nrow(r) > 1) paste("Rescan these", nrow(r), "files") else "Rescan this file", icon = icon("arrows-rotate"), class = "btn-primary"),
        span(class = "dim", "Reads the file again and rebuilds only the stations it feeds; everything else is reused from the last run. ",
             "Use it after changing or replacing a file. A normal run also picks up changed files on its own.")),
      if (one$status != "ok") div(class = "check bad", rf_why[one$status] %||% one$status)
      else tagList(
        p(tags$b(num(one$rows)), " records, ", fmt_ts(one$first_ts), " to ", fmt_ts(one$last_ts), ". Logger ", tags$b(one$logger_id), ", table ", tags$b(one$table),
          if (!is.na(one$station)) tagList(", now in station ", tags$b(paste0(one$station, " / ", one$stream))), if (nzchar(one$rule)) tagList(". Rule: ", tags$b(one$rule)), "."),
        if (!is.na(one$read)) p(tags$b(num(one$read)), " records read: ", tags$b(num(one$in_output)), " in the station file, ", tags$b(num(one$dropped)), " dropped",
          if (one$values_removed > 0) paste0(", and ", num(one$values_removed), " single values blanked in kept records"), ". ",
          if (isTRUE(one$reconciles)) span(style = paste0("color:", GB$green), "Adds up.") else span(class = "check bad", "Does not add up: some records are not accounted for."),
          span(class = "dim", paste0("  md5 ", one$md5))),
        if (nrow(a)) tags$ul(lapply(seq_len(nrow(a)), function(i) tags$li(tags$b(num(a$n[i])), " ", rf_rule_text[a$rule[i]] %||% a$rule[i], span(class = "dim", paste0("  (", a$rule[i], ")")))))
        else p(class = "dim", "Nothing was dropped or removed from this file."),
        tabsetPanel(
          tabPanel("Look at the data",
            div(class = "btnrow", selectInput("rf_param", NULL, choices = NULL, width = "300px"),
                span(class = "dim", "yellow = records kept from this file; grey = the same station from other files. Drag across the plot to mark a stretch.")),
            plotOutput("rf_plot", height = "300px", brush = brushOpts("rf_brush", direction = "x", fill = GB$accent, stroke = GB$accent, opacity = 0.18, resetOnNew = TRUE)),
            uiOutput("rf_brush_ui")),
          tabPanel("Raw text", p(class = "dim", "The start and the end of the file exactly as it is on disk."), verbatimTextOutput("rf_text")))),
      div(class = "editor-only", hr(),
        fluidRow(
          column(3, selectizeInput("rf_station", "Send to station", choices = c("", stn), options = list(create = TRUE, placeholder = "pick or type a new name"), width = "100%")),
          column(2, textInput("rf_stream", "Stream (optional)", width = "100%")),
          column(4, textInput("rf_reason", "Reason (goes in the audit log)", width = "100%")),
          column(3, textInput("rf_author", "Your initials", isolate(who()), width = "100%"))),
        div(class = "btnrow", actionButton("rf_assign", "Send to this station", icon = icon("right-left"), class = "btn-primary"),
            actionButton("rf_ignore", "Leave out of the next run", icon = icon("eye-slash"), class = "btn-danger"),
            actionButton("rf_clear", "Clear the rule", icon = icon("rotate-left")),
            span(class = "dim", if (nrow(r) > 1) paste0("Applies to all ", nrow(r), " selected files.") else "Applies to this file only; the rest of the logger is untouched."))))
  })
  rf_data <- reactive({ r <- rf_sel(); req(r); one <- r[1]; req(one$status == "ok", !is.na(one$station)); rv$tick
    st <- strsplit(one$station, " | ", fixed = TRUE)[[1]][1]; sm <- strsplit(one$stream, " | ", fixed = TRUE)[[1]][1]
    d <- tryCatch(load_station(st, sm, pd()), error = function(e) NULL); req(d)
    mine <- basename(d$source_file) == one$file; pad <- max(86400, as.numeric(diff(range(d$TIMESTAMP[mine], na.rm = TRUE))) * 0.15)
    w <- if (any(mine)) range(d$TIMESTAMP[mine]) + c(-pad, pad) else range(d$TIMESTAMP)
    list(d = d[TIMESTAMP >= w[1] & TIMESTAMP <= w[2]][, .mine := basename(source_file) == one$file], n = sum(mine), station = st, stream = sm, row = one) })
  observe({ x <- rf_data(); v <- setdiff(names(x$d), c(META_COLS, ".mine")); v <- v[vapply(v, function(c) is.numeric(x$d[[c]]) && any(!is.na(x$d[[c]][x$d$.mine])), logical(1))]
    cur <- isolate(input$rf_param); updateSelectInput(session, "rf_param", choices = v, selected = if (isTRUE(cur %in% v)) cur else v[1]) })
  output$rf_plot <- renderPlot({ x <- rf_data(); p <- input$rf_param
    validate(need(x$n > 0, "No record from this file was kept: every one was a duplicate of, or overridden by, another file."), need(nzchar(p %||% "") && p %in% names(x$d), "No numeric column with values."))
    d <- x$d[!is.na(get(p)), .(TIMESTAMP, value = get(p), .mine)]; if (nrow(d) > 30000) d <- d[unique(round(seq(1, .N, length.out = 30000)))]
    ggplot(d, aes(TIMESTAMP, value)) + geom_point(data = d[.mine == FALSE], colour = GB$gray, size = 0.5) + geom_point(data = d[.mine == TRUE], colour = param_colours(p, GB)[[1]], size = 0.6) +
      labs(x = NULL, y = p, subtitle = paste0(x$station, " / ", x$stream, "   ", num(x$n), " records kept from this file")) }, bg = GB$bg, res = 96)
  output$rf_brush_ui <- renderUI({ b <- input$rf_brush; if (is.null(b) || VIEWER) return(NULL)
    t0 <- as.POSIXct(b$xmin, origin = "1970-01-01", tz = "UTC"); t1 <- as.POSIXct(b$xmax, origin = "1970-01-01", tz = "UTC")
    div(class = "btnrow editor-only", tags$b(fmt_ts(t0), " to ", fmt_ts(t1)),
        actionButton("rf_rm_rows", "Remove these records from this file", class = "btn-danger"), actionButton("rf_rm_vals", paste0("Blank only ", input$rf_param), class = "btn-danger"),
        span(class = "dim", "Uses the reason and initials below.")) })
  rf_range <- function() { b <- input$rf_brush; if (is.null(b)) NULL else as.POSIXct(c(b$xmin, b$xmax), origin = "1970-01-01", tz = "UTC") }
  rf_need_reason <- function() { if (!nzchar(trim(input$rf_reason %||% ""))) { note("Give a reason first; it goes in the audit log.", type = "warning"); FALSE } else TRUE }
  observeEvent(input$rf_rm_rows, { r <- rf_range(); x <- rf_data(); req(r); if (!rf_need_reason()) return()
    if (is.null(m_tc$data())) return(note("Run the pipeline once first.", type = "warning"))
    r <- logger_time(r, clock_shifts(pd(), x$row$logger_id, x$row$table, x$row$file), x$row$file)   # the file's own clock times (R/stitch.R)
    if (m_tc$append(data.frame(logger_id = x$row$logger_id, table = x$row$table, source_file = x$row$file, start = fmt_ts(r[1]), end = fmt_ts(r[2]), action = "remove", shift_minutes = "",
                               reason = trim(input$rf_reason), author = trim(input$rf_author)))) { mark(); session$resetBrush("rf_brush"); note("Those records will be removed from this file on the next run.") } })
  observeEvent(input$rf_rm_vals, { r <- rf_range(); x <- rf_data(); req(r, input$rf_param); if (!rf_need_reason()) return()
    if (m_mn$append(data.frame(station = x$station, stream = x$stream, parameter = rx_escape(input$rf_param), start = fmt_ts(r[1]), end = fmt_ts(r[2]),
                               reason = trim(input$rf_reason), author = trim(input$rf_author)))) { mark(); session$resetBrush("rf_brush"); note("Those values will be blanked on the next run.") } })
  output$rf_text <- renderText({ r <- rf_sel(); req(r); f <- r$path[1]; validate(need(file.exists(f), "The raw file is not on this machine (viewer copies hold the cleaned data only)."))
    head_l <- fix_enc(readLines(f, n = 14, warn = FALSE)); sz <- file.size(f); con <- file(f, "rb"); on.exit(close(con)); seek(con, max(0, sz - 3000))
    rw <- readBin(con, "raw", 3000); tl <- strsplit(fix_enc(rawToChar(rw[rw != as.raw(0)])), "\r?\n")[[1]]; if (sz > 3000) tl <- tl[-1]
    paste(c(substr(head_l, 1, 220), "   ...", substr(utils::tail(tl[nzchar(tl)], 8), 1, 220)), collapse = "\n") })
  rf_set <- function(action) { r <- rf_sel(); req(r); if (action != "clear" && !rf_need_reason()) return()
    if (action == "assign" && !nzchar(trim(input$rf_station %||% ""))) return(note("Pick or type the station first.", type = "warning"))
    if (is.null(m_fr$data())) return(note("Run the pipeline once first.", type = "warning"))
    ok <- m_fr$mutate(function(d) { keep <- !(paste(basename(d$file), d$table) %in% paste(r$file, r$table)); d <- d[keep, , drop = FALSE]
      if (action == "clear") return(d)
      rbind(d, data.frame(file = r$file, table = r$table, action = action, station = if (action == "assign") trim(input$rf_station) else "", stream = if (action == "assign") trim(input$rf_stream) else "",
                          reason = trim(input$rf_reason), author = trim(input$rf_author), stringsAsFactors = FALSE)[names(d)]) })
    if (ok) { mark(); note(switch(action, assign = paste0(nrow(r), " file(s) will go to ", trim(input$rf_station), "."), ignore = paste0(nrow(r), " file(s) will be left out."), clear = "Rule cleared."), " Run the pipeline to apply.") } }
  observeEvent(input$rf_rescan, { r <- rf_sel(); req(r); if (VIEWER) return()
    if (run_busy()) return(note("A run is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    cfg <- current_cfg(); if (!folders_ok(cfg)) return()
    n <- forget_files(pd(), r$path, r$logger_id, r$table)
    if (!isTRUE(cfg$incremental)) note("Reusing unchanged loggers is switched off on the Project step, so this is a full run.", type = "warning")
    do_run(cfg, stay = TRUE, after = function() if (isTRUE(cfg$incremental)) { m <- tryCatch(fread(file.path(pd(), "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) NULL)
      if (!is.null(m)) note(sprintf("Rescanned %s. %s of %s logger tables and %s of %s station tables were reused unchanged.", if (nrow(r) > 1) paste(nrow(r), "files") else r$file[1],
        m[key == "tables_reused", value], m[key == "logger_tables", value], m[key == "stations_reused", value], m[key == "station_tables", value]), duration = 10) }) })
  observeEvent(input$rf_assign, rf_set("assign")); observeEvent(input$rf_ignore, rf_set("ignore")); observeEvent(input$rf_clear, rf_set("clear"))

