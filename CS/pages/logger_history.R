# CS/pages/logger_history.R  --  Logger history.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Logger history ---------------------------------------------------------
  lg_data <- reactive({ o <- outs(); req(o); l <- o$lineage; validate(need(nrow(l) > 0, "No lineage yet. Run the pipeline once with this version.")); l <- copy(l)
    l[, `:=`(first = parse_time(substr(as.character(first), 1, 19)), last = parse_time(substr(as.character(last), 1, 19)))]
    l[, n_log := .N, by = station]; l[, n_st := uniqueN(station), by = logger_id]
    switch(input$lg_show %||% "multi", multi = l[n_log > 1], moved = l[station %in% l[n_st > 1, station]], l) })
  output$lg_plot_ui <- renderUI({ l <- tryCatch(lg_data(), error = function(e) NULL); if (is.null(l) || !nrow(l)) return(NULL)
    div(class = "scrollbox", style = "max-height:760px", plotOutput("lg_plot", height = paste0(round((110 + 30 * uniqueN(l$station)) * psz()), "px"))) })
  output$lg_plot <- renderPlot({ l <- lg_data(); validate(need(nrow(l) > 0, "No station matches this view."))
    l[, lab := trimws(paste(ifelse(is.na(model), "", model), logger_id))]; l[, k := seq_len(.N) %% 2, by = station]
    l[, station := factor(station, levels = rev(sort(unique(station))))]; l[, mid := first + (as.numeric(last) - as.numeric(first)) / 2]
    ggplot(l) + geom_segment(aes(x = first, xend = last, y = station, yend = station, colour = factor(k)), linewidth = 6, lineend = "butt") +
      geom_point(aes(first, station), shape = 124, size = 4, colour = GB$fg) + geom_text(aes(mid, station, label = lab), colour = GB$bg_hard, size = 2.9, fontface = "bold", check_overlap = TRUE) +
      scale_colour_manual(values = c(`0` = GB$accent, `1` = GB$yellow), guide = "none") + labs(x = NULL, y = NULL, subtitle = "One bar per logger; a light tick marks where a logger starts") +
      theme(panel.grid.major.y = element_blank())
  }, bg = GB$bg, res = 96)
  output$lg_story <- renderUI({ l <- lg_data(); if (!nrow(l) || uniqueN(l$station) > 12) return(NULL)
    div(class = "explain", lapply(split(l, l$station), function(x) { x <- x[order(first)]
      p(tags$b(x$station[1]), ": ", paste0(trimws(paste(ifelse(is.na(x$model), "", x$model), "serial", x$logger_id)), " (", format(x$first, "%Y-%m-%d"), " to ", format(x$last, "%Y-%m-%d"), ")", collapse = "  \u2192  ")) })) })
  lg_tbl_data <- reactive({ l <- lg_data(); l[order(station, first), .(Station = station, Logger = logger_id, Model = model, Serial = serial, From = format(first, "%Y-%m-%d"), To = format(last, "%Y-%m-%d"), Days = days,
                                                                         Records = records, `Logger tables` = tables, `What happened` = note)] })
  output$lg_tbl <- renderDT(formatRound(ovid_dt(lg_tbl_data(), selection = "none", page = 15), "Records", 0))
  output$lg_dl <- downloadHandler(filename = function() paste0("station_lineage_", Sys.Date(), ".csv"), content = function(f) fwrite(lg_tbl_data(), f))
  observe({ o <- outs(); req(o, nrow(o$proghist) > 0); update_selectize(session, "ph_station", choices = c("All stations", sort(unique(o$proghist$station[!is.na(o$proghist$station) & nzchar(o$proghist$station)]))), server = TRUE) })
  ph_data <- reactive({ o <- outs(); req(o); h <- o$proghist; validate(need(nrow(h) > 0, "No program history yet. Run the pipeline once with this version."))
    if (identical(input$ph_show, "changes")) h <- h[change != "first seen"]
    if (!input$ph_station %in% c("All stations", "")) h <- h[station == input$ph_station]
    h[order(station, logger_id, table, from), .(Station = station, Logger = logger_id, Table = table, From = substr(as.character(from), 1, 16), To = substr(as.character(to), 1, 16), `What changed` = change, Program = program,
                                                 `Columns added` = columns_added, `Columns removed` = columns_removed, Columns = n_columns, OS = os, Model = model, Files = files, `First file` = first_file)] })
  output$ph_note <- renderUI({ o <- outs(); req(o); h <- o$proghist; if (!nrow(h)) return(NULL)
    div(class = "readout", span(tags$b(num(sum(h$change != "first seen"))), " changes across ", tags$b(num(uniqueN(h[change != "first seen", paste(logger_id, table)]))), " logger tables"),
        span(tags$b(num(sum(grepl("program", h$change)))), " program changes"), span(tags$b(num(sum(grepl("columns", h$change)))), " changes of columns")) })
  output$ph_tbl <- renderDT({ d <- ph_data(); validate(need(nrow(d) > 0, "No changes: every logger table kept one program and one set of columns.")); ovid_dt(d, selection = "none", page = 20) })
  output$ph_dl <- downloadHandler(filename = function() paste0("program_history_", Sys.Date(), ".csv"), content = function(f) fwrite(ph_data(), f))

