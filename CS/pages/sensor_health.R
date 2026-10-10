# CS/pages/sensor_health.R  --  Sensor health.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sensor health -----------------------------------------------------------
  output$sh_empty <- renderUI(if (is.null(outs())) empty_msg())
  observe({ o <- outs(); req(o); updateSelectizeInput(session, "sh_station", choices = sort(unique(o$health$station)), selected = isolate(input$sh_station)) })
  sh_rows <- reactive({ o <- outs(); req(o); h <- o$health; validate(need(nrow(h) > 0 && "health" %in% names(h), "No sensor health yet: run the pipeline."))
    if (length(input$sh_station)) h <- h[station %in% input$sh_station]; if (isTRUE(input$sh_bad)) h <- h[health != "ok"]; h })
  output$sh_tbl <- renderDT({ h <- sh_rows(); validate(need(nrow(h) > 0, "Every sensor here is ok.")); ovid_dt(h, selection = "single", page = 15, opts = list(scrollX = TRUE)) })
  observeEvent(input$sh_tbl_rows_selected, { h <- sh_rows(); i <- input$sh_tbl_rows_selected; req(length(i) == 1)
    updateSelectInput(session, "ex_station", selected = h$station[i]); go("Explore")
    session$onFlushed(function() updateSelectizeInput(session, "ex_params", selected = h$parameter[i]), once = TRUE) })
