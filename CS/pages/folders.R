# CS/pages/folders.R  --  Home: the folder checks, and the run's findings for Needs you (Home itself is CS/home.R).
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Home --------------------------------------------------------------
  raw_dirs_d <- debounce(reactive(input$raw_dirs), 700)
  # the folder check and what a scan of the folders will show (CS/home.R cs_scan_types), worked out once, shown with the folders
  output$raw_check <- renderUI({
    d <- trim(strsplit(raw_dirs_d() %||% "", "\n")[[1]]); d <- d[nzchar(d)]
    if (!length(d)) return(div(class = "check bad", "Add at least one folder."))
    ok <- dir.exists(path.expand(d))
    # safe: the folder summary is shown when it can be made
    o <- outs(); x <- tryCatch(cs_scan_types(d, m_st$data(), manifest = if (!is.null(o)) o$manifest), error = function(e) NULL)
    tagList(if (!all(ok)) div(class = "check bad", "Not found: ", paste(d[!ok], collapse = ", ")),
            h4("What the scan will show"),
            if (is.null(x)) p(class = "dim", "The data folders could not be read.")
            else tags$table(class = "table d-num", tags$thead(tags$tr(lapply(names(x), tags$th))), tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(lapply(x[i], function(v) tags$td(v)))))))
  })
  output$stats <- renderUI({
    o <- outs(); if (is.null(o)) return(if (VIEWER) empty_msg("No data was found for this viewer. Ask whoever published it to build the bundle again.") else div(class = "panel-x", h3("Get started"),
      step(1, "Point the app at your data", p(class = "dim", "On the Project step (1 Project), list the folders that hold your raw .dat files and choose a project folder for the results.")),
      step(2, "Press Run pipeline", p(class = "dim", "Top right. Files are read, stitched by logger, checked and cleaned. Raw files are never changed.")),
      step(3, "Name your stations", p(class = "dim", "Loggers start out named by serial number. Give them station names under Set up, then plot, clean and report from here."))))
    st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab)); a <- o$summary
    div(class = "stats", st(sum(o$manifest$status == "ok"), "files read"), st(uniqueN(o$registry$logger_id), "loggers"),
        st(uniqueN(o$stations$station), "stations"), st(sum(as.numeric(o$stations$records)), "clean records"),
        st(sum(as.numeric(a[level == "cell", n])), "values removed"), st(sum(as.numeric(a[level == "row", n])), "duplicate or bad records dropped"))
  })
  # what the last run found that needs a person, for the Needs you list (setup_flow.R): each with the page that settles it
  cs_notes <- reactive({
    o <- outs(); if (is.null(o) || VIEWER) return(NULL)
    # safe: nothing listed yet
    s <- tryCatch(st_sources(), error = function(e) NULL)
    n <- c(st = if (is.null(s)) 0 else uniqueN(s[station == "", logger_id]), al = uniqueN(o$dict[parameter == raw_column, raw_column]),
           fl = sum(!o$manifest$status %in% c("ok", "duplicate_file", "skipped_table")), md = if (nrow(o$gaps)) sum(o$gaps$gap_hours > 24) else 0,
           ck = nrow(o$checks), du = nrow(o$dups), pg = if (nrow(o$pstats) && "outside_typical" %in% names(o$pstats)) uniqueN(o$pstats[outside_typical == TRUE, parameter]) else 0)
    lab <- c(ck = "clock findings (power loss, clock resets, shifted files)", du = "pairs of station tables hold the same data", pg = "parameters have values outside their usual range",
             st = "loggers still use their serial number as a name", al = "raw columns have no common name", md = "gaps longer than a day", fl = "files could not be read")
    go <- c(ck = "Clock checks", du = "Metadata", pg = "Parameter guide", st = "Stations", al = "Column names", md = "Metadata", fl = "Raw files")
    k <- names(lab)[n[names(lab)] > 0]
    pr <- o$problems; pr <- if (!is.null(pr) && nrow(pr)) data.table(kind = "note", where = as.character(pr$where), what = as.character(pr$problem), detail = "the last run", go = "Metadata", to = "problems")
    rbind(pr, if (length(k)) data.table(kind = "note", where = num(n[k]), what = unname(lab[k]), detail = "", go = unname(go[k]), to = k), fill = TRUE) })
  cs_note_go <- function(r) switch(r$to %||% "", problems = md_go("problems"), du = md_go("dups"), md = md_go("gaps"),
    pg = { go("Parameter guide"); updateRadioButtons(session, "pg_show", selected = "out") }, st = { go("Stations"); updateRadioButtons(session, "st_show", selected = "unnamed") }, go(r$go))
  md_go <- function(v) { go("Metadata"); updateRadioButtons(session, "md_view", selected = v) }
  output$timeline <- renderPlot({
    o <- outs(); validate(need(!is.null(o), "The coverage chart appears after the first run."))
    m <- copy(o$manifest); for (v in c("first_ts", "last_ts")) if (!inherits(m[[v]], "POSIXct")) m[, (v) := parse_time(as.character(get(v)))]
    validate(need(nrow(m[status == "ok" & !is.na(first_ts)]) > 0, "No dated files to show."))
    plot_timeline(m) + labs(title = NULL, subtitle = "One row per logger, one bar per source file in alternating colour, so you can see how files were stitched")
  }, bg = GB$bg, res = 96, height = function() { o <- outs(); if (is.null(o)) 80 else 110 + 20 * uniqueN(o$registry$logger_id) })
  output$log <- renderUI(tags$pre(class = "log", if (length(rv$log)) paste(rv$log, collapse = "\n") else "Nothing has run in this session."))

