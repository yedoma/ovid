# WLR/pages/explore_compare.R  --  Compare sites and years.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- compare sites and years
  observe({ o <- outs(); req(o); s <- sort(unique(o$summary$site))
    updateSelectizeInput(session, "cs_sites", choices = s, selected = isolate(input$cs_sites)); updateSelectInput(session, "cy_site", choices = s, selected = isolate(input$cy_site)) })
  site_data <- function(st) { d <- load_site(st, pd()); if (is.null(d)) NULL else d }
  cs_long <- reactive({ req(length(input$cs_sites) >= 1); dts <- setNames(lapply(input$cs_sites, site_data), input$cs_sites)
    x <- compare_sites_data(dts, input$cs_param, input$cs_res, input$cs_mode); validate(need(nrow(x) > 0, "None of these sites has that value."))
    dr <- input$cs_dates; if (length(dr) == 2 && !is.na(dr[1])) x <- x[TIMESTAMP >= as.POSIXct(dr[1])]; if (length(dr) == 2 && !is.na(dr[2])) x <- x[TIMESTAMP < as.POSIXct(dr[2] + 1)]
    x })
  output$cs_hint <- renderUI(p(class = "dim", "Each site is its whole record: every logger and every year stitched together. 'Difference from each site's median' lines up sites at different depths so their ups and downs can be compared."))
  cs_plot <- reactive(plot_compare_sites(cs_long(), input$cs_param, input$cs_mode))
  output$cs_plot <- renderPlot(cs_plot(), bg = NORD$bg)
  output$cs_stats <- renderTable(cs_long()[!is.na(value), .(Readings = num(.N), From = format(min(TIMESTAMP), "%Y-%m-%d"), To = format(max(TIMESTAMP), "%Y-%m-%d"), Min = round(min(value), 3),
                                                           Median = round(stats::median(value), 3), Max = round(max(value), 3)), by = .(Site = site)])
  output$cs_png <- downloadHandler("compare_sites.png", function(file) nord_save(cs_plot(), file, 12, 6))
  output$cs_dl <- downloadHandler("compare_sites.csv", function(file) { x <- dcast(cs_long(), TIMESTAMP ~ site, value.var = "value"); x[, TIMESTAMP := fmt_ts(TIMESTAMP)]; fwrite(x, file) })
  cy_long <- reactive({ req(input$cy_site); d <- site_data(input$cy_site); validate(need(!is.null(d) && input$cy_param %in% names(d), "This site has no such value."))
    compare_years_data(d, input$cy_param, input$cy_res, input$cy_axis, input$water_year_start %||% SETTINGS$water_year_start) })
  cy_plot <- reactive(plot_compare_years(cy_long(), input$cy_param, input$cy_axis, paste(input$cy_site, "year by year"), input$water_year_start %||% SETTINGS$water_year_start))
  output$cy_plot <- renderPlot(cy_plot(), bg = NORD$bg)
  output$cy_stats <- renderTable(cy_long()[!is.na(value), .(Readings = num(.N), From = format(min(TIMESTAMP), "%Y-%m-%d"), To = format(max(TIMESTAMP), "%Y-%m-%d"), Min = round(min(value), 3),
                                                           Median = round(stats::median(value), 3), Max = round(max(value), 3)), by = .(Year = year)])
  output$cy_png <- downloadHandler("compare_years.png", function(file) nord_save(cy_plot(), file, 12, 6))
  output$ex_stats <- renderTable({ d <- ex_data(); req(d)
    pp <- intersect(c(input$ex_params, "depth_m"), names(d)); pp <- unique(pp)
    rbindlist(lapply(pp, function(p) { x <- d[[p]]; ok <- !is.na(x)
      data.table(Value = param_label(p), Readings = num(sum(ok)), Missing = num(sum(!ok)), Min = if (any(ok)) round(min(x[ok]), 3) else NA, Median = if (any(ok)) round(stats::median(x[ok]), 3) else NA,
                 Max = if (any(ok)) round(max(x[ok]), 3) else NA) })) }, striped = FALSE, bordered = FALSE)
  output$ex_flags <- renderDT({ o <- outs(); req(o); sr <- ex_serials(); ovid_dt(o$flags[logger_id %in% sr], page = 8) })
  output$ex_dl <- downloadHandler(function() paste0(safe_name(input$ex_dep %||% "data"), ".csv"), function(file) fwrite(ex_data(), file, dateTimeAs = "write.csv"))
