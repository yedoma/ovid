# GT/pages/soil.R  --  Soil.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Soil --------------------------------------------------------------------------------------------------------------
  soil_sites <- reactive({ rv$tick; f <- list.files(file.path(pd(), "output", "soil"), "\\.rds$"); s <- site_names(); s[safe_name(s) %in% sub("\\.rds$", "", f)] })
  observe({ s <- soil_sites(); updateSelectInput(session, "so_site", choices = s, selected = if (isolate(input$so_site) %in% s) isolate(input$so_site) else s[1]) })
  output$so_empty <- renderUI(if (!length(soil_sites())) empty_msg("No soil moisture or conductivity was found (EM50 ports, or Campbell columns matched as vwc or ec)."))
  so_d <- reactive({ req(input$so_site); load_soil(input$so_site, pd()) })
  so_plot <- reactive({ s <- so_d(); req(s); plot_soil(s, input$so_var, input$so_res, paste(input$so_site, "\u00b7 soil"), if (input$so_axis == "vwc" && input$so_var == "vwc") c(0, 0.6) else NULL) })
  output$so_plot <- renderPlot(so_plot(), bg = MI$bg)
  output$so_temp <- renderPlot({ d <- load_site(input$so_site, pd()); req(d); plot_profile_lines(d, paste(input$so_site, "\u00b7 temperature at the same ports"), input$so_res) }, bg = MI$bg)
  output$so_tbl <- renderDT({ s <- so_d(); req(s); ovid_dt(s[, .(values = .N, from = fmt_ts(min(TIMESTAMP)), to = fmt_ts(max(TIMESTAMP)), min = round(min(value), 4), mean = round(mean(value), 4), max = round(max(value), 4)), by = .(variable, depth = depth_lab(depth_m))], page = 12) })
  output$so_png <- downloadHandler(function() paste0(safe_name(input$so_site), "_soil.png"), function(file) gt_save(so_plot(), file, 13, 6))
