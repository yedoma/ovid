# WLR/pages/readings_in_the_air.R  --  Readings in the air.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Readings in the air
  output$qa_air <- renderDT({ o <- outs(); req(o); a <- o$air; validate(need(nrow(a) > 0, "No readings in the air were found in the last run."))
    ovid_dt(a[order(site, year), .(site, year, serial, where, from, to, records, hours = round(as.numeric(difftime(to, from, units = "hours")), 1), first_in_water, action)], selection = "multiple", page = 12) })
  observeEvent(input$qa_drop, { o <- outs(); req(o); a <- o$air[order(site, year)]; s <- input$qa_air_rows_selected
    if (!length(s)) return(note("Select one or more runs first.", type = "warning"))
    if (!nzchar(trim(input$qa_reason %||% ""))) return(note("Give a reason first.", type = "warning"))
    rows <- data.frame(serial = a$serial[s], site = a$site[s], start = fmt_ts(a$from[s]), end = fmt_ts(a$to[s]), parameter = "records", min_value = "", max_value = "",
                       reason = trim(input$qa_reason), author = trim(input$qa_author), stringsAsFactors = FALSE)
    if (m_man$append(rows)) { mark(); note(nrow(rows), " run(s) will be dropped at the next run. They are listed under Corrections over a period.") } })
