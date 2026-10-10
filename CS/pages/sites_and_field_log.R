# CS/pages/sites_and_field_log.R  --  Sites and field log.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sites and field log -------------------------------------------------
  observe({ o <- outs(); req(o, nrow(o$stations) > 0); updateSelectInput(session, "fl_station", choices = sort(unique(o$stations$station))) })
  observeEvent(input$fl_add, { req(input$fl_station)
    if (is.na(parse_time(trim(input$fl_date)))) return(note("Give the date as YYYY-MM-DD, with a time if you have one.", type = "warning"))
    if (is.null(m_fl$data())) return(note("The field log is created the first time the pipeline runs.", type = "warning"))
    with_initials(function(who) if (m_fl$append(data.frame(station = input$fl_station, stream = "", date = trim(input$fl_date), type = input$fl_type, parameter = "", note = trim(input$fl_note),
                                                       author = who))) { note("Entry added."); updateTextInput(session, "fl_note", value = "") }) })

