# WLR/pages/deployment_history.R  --  Deployment history.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Deployment history -----------------------------------------------------------------------------------------------------
  output$dh_plot <- renderPlot({ o <- outs(); validate(need(!is.null(o), "Appears after the first run."))
    d <- o$deps[has_data == TRUE | input$dh_show == "all"]; plot_deployments(d, o$loggers[, .(serial, first, last)]) },
    height = function() { o <- outs(); if (is.null(o)) 200 else 120 + 22 * uniqueN(o$deps[has_data == TRUE | input$dh_show == "all", serial]) }, bg = NORD$bg)
  output$dh_tbl <- renderDT({ o <- outs(); req(o); d <- o$deps[has_data == TRUE | input$dh_show == "all"][order(serial, year)]
    ovid_dt(d[, .(serial, site, year, start, start_source, end, end_source, has_data, T_ST, T_ST_source, reference = ref_code, measured_to, offset_m, logger, visits)], page = 15) })
