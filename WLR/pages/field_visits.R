# WLR/pages/field_visits.R  --  Field visits.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Field visits ---------------------------------------------------------------------------------------------------------------
  # the sites of the workbook and of the deployments (a project with only a logger list has no workbook rows)
  observe({ o <- outs(); req(o); s <- sort(unique(c(if (nrow(o$md)) o$md[serial %in% o$summary$serial, site], if (nrow(o$summary)) o$summary$site)))
    s <- s[!is.na(s) & nzchar(s)]; req(length(s) > 0); updateSelectInput(session, "fv_site", choices = s, selected = isolate(input$fv_site)) })   # req(0) would pass
  observe({ o <- outs(); req(o); updateSelectizeInput(session, "sm_sites", choices = sort(unique(o$summary$site)), selected = isolate(input$sm_sites)) })
  sm_tab <- reactive({ f <- out_dir(pd(), "summaries", paste0(input$sm_by %||% "water_year", ".csv")); validate(need(file.exists(f), "No summaries yet: run the pipeline."))
    x <- fread(file = f); x <- x[parameter == input$sm_param]; if (length(input$sm_sites)) x <- x[site %in% input$sm_sites]
    if (isTRUE(input$sm_complete)) x <- x[complete == TRUE]; x[, parameter := NULL]; x })
  output$sm_tbl <- renderDT({ x <- sm_tab(); validate(need(nrow(x) > 0, "Nothing for this choice.")); ovid_dt(x, page = 15) })
  output$sm_dl <- downloadHandler(function() paste0("summary_", input$sm_by, "_", input$sm_param, ".csv"), function(file) fwrite(sm_tab(), file))
  output$fv_tbl <- renderDT({ o <- outs(); req(o, input$fv_site); st <- input$fv_site
    if (!nrow(o$md) || !nrow(o$md[site == st])) {            # no workbook rows: the deployments the run made, with their hang lengths
      d <- o$summary[site == st]; validate(need(nrow(d) > 0, "No visits or deployments at this site."))
      return(ovid_dt(d[order(-year, start), .(year, serial, from = fmt_ts(start), to = fmt_ts(end), T_ST)], page = 15)) }
    ovid_dt(o$md[site == st][order(-year, visit_time), .(year, visit_time, serial, logger, downloaded_by, T_water, T_TOC, TOC_water, T_veg, TOC_veg, T_ST, tst_from_toc, remarks)], page = 15) })
