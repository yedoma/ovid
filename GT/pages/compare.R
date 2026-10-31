# GT/pages/compare.R  --  Compare.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Compare ---------------------------------------------------------------------------------------------------------
  observe({ o <- outs(); req(o); updateSelectizeInput(session, "cp_type", choices = sort(unique(o$ss$instrument)), selected = isolate(input$cp_type))
    g <- sort(unique(c(o$ss$site_group[!is.na(o$ss$site_group)], paste("Landcover:", o$ss$landcover[!is.na(o$ss$landcover) & nzchar(o$ss$landcover)]))))
    updateSelectizeInput(session, "cp_group", choices = g, selected = isolate(input$cp_group)) })
  cp_choices <- reactive({ o <- outs(); req(o); x <- o$ss
    if (length(input$cp_type)) x <- x[instrument %in% input$cp_type]
    if (length(input$cp_group)) x <- x[site_group %in% input$cp_group | paste("Landcover:", landcover) %in% input$cp_group]
    sort(x$site) })
  observe({ s <- cp_choices(); updateSelectizeInput(session, "cp_sites", choices = s, selected = intersect(isolate(input$cp_sites), s)) })
  observe({ updateSelectInput(session, "cp_ref", choices = input$cp_sites) })
  cp_z <- reactive(typed_depth(input$cp_depth %||% 0.5, proj_sgn()))   # typed in the project's depth convention
  cp_long <- reactive({ req(length(input$cp_sites) >= 1); dts <- setNames(lapply(input$cp_sites, load_site, project_dir = pd()), input$cp_sites)
    dr <- input$cp_dates; if (length(dr) == 2 && !anyNA(dr)) dts <- lapply(dts, function(d) if (is.null(d)) NULL else d[TIMESTAMP >= as.POSIXct(dr[1]) & TIMESTAMP < as.POSIXct(dr[2] + 1)])
    compare_data(dts, cp_z(), input$cp_tol %||% 0.1, input$cp_res, input$cp_mode, input$cp_ref) })
  cp_plot <- reactive({ x <- cp_long(); plot_compare(x, cp_z(), input$cp_mode, input$cp_ref, paste("Sites compared at", depth_lab(cp_z())), if (nrow(x)) ylim_of("cpx", x$value) else NULL, input$cp_kind) })
  output$cp_plot <- renderPlot(cp_plot(), bg = MI$bg)
  output$cp_stats <- renderTable({ x <- cp_long(); req(nrow(x) > 0); o <- outs()
    s <- x[, .(Depth = depth_lab(actual_depth[1]), Values = num(.N), From = format(min(TIMESTAMP), "%Y-%m-%d"), To = format(max(TIMESTAMP), "%Y-%m-%d"), Min = round(min(value), 3),
               Mean = round(mean(value), 3), Max = round(max(value), 3)), by = .(Site = site)]
    merge(s, o$ss[, .(Site = site, Instrument = instrument, Landcover = landcover)], by = "Site", all.x = TRUE) })
  output$cp_annual <- renderDT({ o <- outs(); req(o, length(input$cp_sites)); x <- o$annual[site %in% input$cp_sites & year_type == "calendar" & abs(depth_m - cp_z()) <= (input$cp_tol %||% 0.1) + 1e-9]
    req(nrow(x) > 0); ovid_dt(x[order(year, site), .(year, site, depth_m, mean, min, max, range, days, complete)], page = 12) })
  output$bs_tbl <- renderDT({ o <- outs(); req(o); b <- o$between
    validate(need(nrow(b) > 0, "Nothing to compare yet: no depth is shared by three sites or more (or the check is off), or the pipeline has not run since it was added."))
    ovid_dt(b, selection = "single", page = 10) })
  output$bs_plot <- renderPlot({ o <- outs(); req(o, nrow(o$between)); i <- input$bs_tbl_rows_selected; req(length(i) == 1)
    r <- o$between[i]; dd <- load_daily(pd()); req(nrow(dd) > 0)
    days <- between_site_days(dd, r$depth_m); req(!is.null(days)); x <- days[id == r$site]; req(nrow(x) > 0)
    long <- rbind(x[, .(date, value, what = r$site)], x[, .(date, value = others, what = "median of the other sites")])
    ggplot(long, aes(date, value, colour = what)) + geom_line(linewidth = 0.4) +
      geom_point(data = x[bad == TRUE], aes(date, value), inherit.aes = FALSE, colour = MI$red, size = 1.6) +
      scale_colour_manual(values = setNames(c(param_colours("temp", MI)[[1]], MI$gray), c(r$site, "median of the other sites")), name = NULL) +
      labs(x = NULL, y = "Daily mean (\u00b0C)", title = paste0(r$site, " at ", r$depth_m, " m against the other sites"),
           subtitle = paste0(r$verdict, "; red points: days that depart from the usual offset")) + theme(legend.position = "top") }, bg = MI$bg, res = 96)
  output$cp_png <- downloadHandler("compare_sites.png", function(file) gt_save(cp_plot(), file, 13, 6))
  output$cp_dl <- downloadHandler("compare_sites.csv", function(file) { x <- copy(cp_long()); x[, TIMESTAMP := fmt_ts(TIMESTAMP)]; fwrite(x, file) })
