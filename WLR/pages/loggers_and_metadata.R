# WLR/pages/loggers_and_metadata.R  --  Loggers and metadata.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Loggers and metadata --------------------------------------------------------------------------------------------------------------
  output$md_loggers <- renderDT({ o <- outs(); req(o); ovid_dt(o$loggers, page = 15) })
  observe({ o <- outs(); req(o, nrow(o$md) > 0); y <- sort(unique(o$md$year), decreasing = TRUE); updateSelectInput(session, "md_year", choices = y) })
  output$md_book <- renderDT({ o <- outs(); req(o, input$md_year); yy <- as.integer(input$md_year)
    ovid_dt(o$md[year == yy, .(row, site, type, serial_raw, serial, logger, visit_date, visit_time, T_water_raw, T_TOC_raw, TOC_water_raw, T_ST_raw, tst_from_toc, tst_problem, remarks)], page = 15) })
  output$md_gaps <- renderDT({ o <- outs(); req(o); ovid_dt(o$gaps, page = 15) })
