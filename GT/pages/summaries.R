# GT/pages/summaries.R  --  Summaries.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Summaries -------------------------------------------------------------------------------------------------------
  observe({ s <- site_names(); updateSelectizeInput(session, "sm_sites", choices = s, selected = isolate(input$sm_sites)); updateSelectizeInput(session, "xp_sites", choices = s, selected = isolate(input$xp_sites))
    updateSelectInput(session, "rp_pick", choices = report_choices(s), selected = isolate(input$rp_pick))
            updateSelectizeInput(session, "st_sites", choices = s); updateSelectInput(session, "sd_site", choices = s, selected = isolate(input$sd_site)) })
  # the water year as the last run used it (Summaries, Explore), and its months in the labels
  output$sm_wy <- renderText(wy_months(run_set("water_year_start")))
  observe({ m <- wy_months(run_set("water_year_start"))
    updateRadioButtons(session, "tr_yt", choices = c("Calendar" = "calendar", setNames("water", paste0("Water year (", m, ")"))), selected = isolate(input$tr_yt) %||% "calendar")
    updateRadioButtons(session, "se_axis", choices = c(setNames("water", m), "January to December" = "calendar"), selected = isolate(input$se_axis) %||% "water") })
  sm_f <- function(x) { if (length(input$sm_sites)) x <- x[site %in% input$sm_sites]; x }
  sm_annual <- reactive({ o <- outs(); req(o); x <- sm_f(o$annual)[year_type == input$sm_yt]; if (isTRUE(input$sm_complete)) x <- x[complete == TRUE]; x[order(site, year, depth_m), !"year_type"] })
  output$sm_annual <- renderDT(ovid_dt(sm_annual(), page = 15))
  output$sm_zaa <- renderDT({ o <- outs(); req(o); ovid_dt(sm_f(o$zaa), page = 8) })
  output$sm_alt <- renderDT({ o <- outs(); req(o); ovid_dt(sm_f(o$alt)[order(site, year)], page = 12) })
  output$sm_alt_plot <- renderPlot({ o <- outs(); req(o); a <- sm_f(o$alt)[!is.na(alt_m)]; validate(need(nrow(a) > 0, "No active-layer thickness for these sites (no permafrost within the sensors, or no complete year)."))
    ggplot(a, aes(year, alt_m, colour = site)) + geom_line(linewidth = 0.6) + geom_point(size = 2.2) + scale_y_reverse() + scale_colour_manual(values = rep(CAT_PAL, 10)[seq_len(uniqueN(a$site))], name = NULL) +
      scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 1)) + labs(title = "Active-layer thickness", x = NULL, y = "Active-layer thickness (m)") }, bg = MI$bg)
  output$sm_ft <- renderDT({ o <- outs(); req(o); ovid_dt(sm_f(o$ft)[order(site, water_year, depth_m)], page = 12) })
  output$sm_zc <- renderDT({ o <- outs(); req(o); ovid_dt(sm_f(o$zc)[order(site, start, depth_m)], page = 12) })
  emp <- function(x, msg) { validate(need(!is.null(x) && nrow(x) > 0, msg)); x }
  output$sm_nf <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$nf), "Run the pipeline to compute n-factors (a ground-surface sensor is needed)."), page = 10) })
  output$sm_dg <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$dg), "No degree days yet."), page = 10) })
  output$sm_talik <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$taliks), "No complete year with enough depths yet."), page = 10) })
  output$sm_gstate <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$gstate), "No complete year yet."), page = 12) })
  output$sm_trend <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$trends), "No trends yet."), page = 12) })
  output$sm_thermal <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$thermal), "No complete year at two depths or more."), page = 10) })
  output$sm_est <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$est), "No day estimated."), page = 10) })
  output$sm_zoff <- renderDT({ o <- outs(); req(o); ovid_dt(emp(sm_f(o$zoff), "No zero-curtain periods."), page = 10) })
