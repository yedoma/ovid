# CS/pages/setup_flow.R  --  the set-up flow .
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- the set-up flow (core flow.R): what shows, Needs you, Apply, the information panel, the station strip --------------
  st_all <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$stations)) character() else stn_names(o) })
  st_shown <- display_server("disp", pd, st_all)
  output$flow_ctx <- renderUI({ live_stamp(display_path(pd())); display_context(pd(), st_all(), "stations") })
  strip_cur <- reactiveVal(NULL)
  if (!embedded) {                                              # switching views keeps the station, parameters and period (core shell.R)
    q0 <- shiny::parseQueryString(isolate(session$clientData$url_search) %||% "")
    if (!is.null(q0$station)) { strip_cur(q0$station); session$onFlushed(function() updateSelectInput(session, "ex_station", selected = q0$station), once = TRUE) }
    if (!is.null(q0$from) && !is.null(q0$to)) session$onFlushed(function() updateDateRangeInput(session, "ex_dates", start = q0$from, end = q0$to), once = TRUE)
    observe(ovid_ctx(session, "station", strip_cur() %||% input$ex_station, input$ex_params, input$ex_dates[1], input$ex_dates[2])) }
  cs_home_server(input, output, session, outs, pd, strip_cur, st_shown, E, reactive(live_settings()))   # CS/home.R
  output$cs_strip <- renderUI({ s <- st_shown(); if (!length(s)) return(span(class = "dim", "No stations yet: bring in files and run once."))
    cur <- strip_cur() %||% isolate(input$ex_station) %||% s[1]; head_n <- 8
    pick <- function(v, lab = v) tags$button(type = "button", class = paste("btn btn-default", if (identical(v, cur)) "on"), title = paste("Show", v, "on every page"),
                                             onclick = sprintf("Shiny.setInputValue('strip_pick',%s,{priority:'event'})", jsonlite::toJSON(v, auto_unbox = TRUE)), lab)
    tagList(span(class = "lab", "Stations"), lapply(head(s, head_n), pick),
      if (length(s) > head_n) tags$details(class = "hm-more-st", tags$summary(class = "btn btn-default", sprintf("+%d", length(s) - head_n)),
        div(class = "hm-st-pop", tags$input(type = "search", class = "form-control input-sm", placeholder = "Find a station\u2026", oninput = "var q=this.value.toLowerCase();this.parentNode.querySelectorAll('button').forEach(function(b){b.style.display=b.textContent.toLowerCase().indexOf(q)>=0?'':'none'})"),
            div(class = "hm-st-list", lapply(s[-seq_len(head_n)], function(v) tags$button(type = "button", class = paste("btn btn-default btn-sm", if (identical(v, cur)) "on"),
              onclick = sprintf("Shiny.setInputValue('strip_pick',%s,{priority:'event'});this.closest('details').open=false", jsonlite::toJSON(v, auto_unbox = TRUE)), v)))))) })
  observeEvent(input$strip_pick, { v <- input$strip_pick; strip_cur(v)
    updateSelectInput(session, "ex_station", selected = v) })

  # Needs you: the one review list (core flow.R): the shown stations' flags, loggers that look moved and the run's findings
  rl_flags <- reactive({ o <- outs(); f <- o$flags; if (is.null(f) || !nrow(f)) return(NULL); f <- copy(f)[station %in% st_shown()]
    f[, key := paste0(station, "__", stream)]; review_flag_items(f, "station", keep = "key") })
  review <- review_list_server("rl", reactive(rbind(rl_flags(),
      tryCatch(gb$suggestions()[, .(kind = "moved", where = serial, what, serial, at)], error = function(e) NULL), cs_notes(), fill = TRUE)),
    reactive(review_path(pd())), period = if (!VIEWER) E$period, split = function(s, a) gb$accept(s, a), goto = cs_note_go,
    open = function(r) { strip_cur(r$where); updateSelectInput(session, "ex_station", selected = r$where)
      updateDateRangeInput(session, "ex_dates", start = as.Date(substr(r$start, 1, 10)) - 1, end = as.Date(substr(r$end, 1, 10)) + 1); go("Explore") },
    remove = function(r) for (p in strsplit(r$parameter, ", ", fixed = TRUE)[[1]])          # a removal per parameter (corrections over a period)
      E$manual$append(data.frame(station = r$where, stream = "", parameter = p, start = r$start, end = r$end, reason = paste("Needs you:", r$what), author = "", stringsAsFactors = FALSE)))
  if (!VIEWER) unsaved_server("uns", editors = E, on_apply = mark)
  output$saved_hist <- renderUI({ d <- hi_log(); if (!nrow(d)) return(p(class = "dim", "Nothing saved yet."))
    tagList(tags$table(class = "table", tags$thead(tags$tr(tags$th("When"), tags$th("Table"), tags$th("Rows after"))),
      tags$tbody(lapply(seq_len(min(8, nrow(d))), function(i) tags$tr(tags$td(d$time[i]), tags$td(d$table[i]), tags$td(d$rows_after[i]))))),
      tags$a(href = "#", `data-ovid-go` = "History", "Every change, and going back to one \u203a")) })

  # the information panel and the status line of each step
  output$flow_info <- renderUI({ o <- outs(); s <- st_all()
    h <- tryCatch(fread(file.path(pd(), "output", "audit", "run_history.csv"), colClasses = "character"), error = function(e) data.table())
    x <- review$items(); n_iss <- sum(x$kind == "flag")
    gp <- if (!is.null(o) && nrow(o$gaps)) { g <- copy(o$gaps)[order(-as.numeric(gap_hours))][as.numeric(gap_hours) >= 24]
      if (nrow(g)) data.table(where = paste(g$logger_id, g$table), what = paste(flow_span(g$gap_hours), "without records"),
                              when = paste(substr(g$gap_start, 1, 10), "to", substr(g$gap_end, 1, 10)), go = "Needs") else NULL } else NULL
    flow_aside(project = list(
        list("Data files", if (!is.null(o)) num(nrow(o$manifest)) else "\u2014"),
        list("Loggers", if (!is.null(o)) num(uniqueN(o$registry$logger_id)) else "\u2014"),
        list("Stations shown", sprintf("%d of %d", length(st_shown()), length(s))),
        list("Records", if (!is.null(o) && nrow(o$stations)) paste(substr(fmt_ts(min(o$stations$first)), 1, 7), "to", substr(fmt_ts(max(o$stations$last)), 1, 7)) else "\u2014"),
        list("Data check", if (is.null(o)) "\u2014" else if (n_iss) paste(n_iss, "to look at") else "nothing flagged", level = if (is.null(o)) NULL else if (n_iss) "warn" else "ok", go = "Needs"),
        list("Last run", if (nrow(h)) substr(utils::tail(h$started, 1), 1, 16) else "never")),
      gaps = gp) })
  observe({ o <- outs(); x <- review$items(); un <- if (!is.null(o) && nrow(o$registry)) sum(!nzchar(o$registry$station %|% "")) else 0
    ovid_steps(session, list(
      Project = if (nzchar(trim(input$project_dir %||% ""))) list("ok", "Folders set") else list("off", "Choose the folders"),
      `Raw files` = if (!is.null(o)) list("ok", paste(num(nrow(o$manifest)), "files")) else list("off", "Not scanned yet"),
      Stations = if (un) list("warn", paste(un, "without a station")) else if (!is.null(o)) list("ok", "Every logger named") else list("off", ""),
      Shown = list("info", sprintf("%d of %d shown", length(st_shown()), length(st_all()))),
      Corrections = list("info", "On by default"),
      Needs = if (nrow(x)) list("warn", paste(nrow(x), "to look at")) else if (!is.null(o)) list("ok", "Nothing waits") else list("off", "Run once first"),
      Export = if (!is.null(o)) list("info", "One report, every station") else list("off", "")))
  })
