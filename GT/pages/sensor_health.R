# GT/pages/sensor_health.R  --  Sensor health.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sensor health -----------------------------------------------------------------------------------------------------
  output$he_cards <- renderUI({ o <- outs(); req(o); h <- o$health; st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab))
    div(class = "stats", st(nrow(h), "sensors"), st(sum(h$health == "ok"), "ok"), st(sum(h$health %in% c("suspect", "intermittent")), "suspect or intermittent"),
        st(sum(h$health %in% c("failing: mostly errors", "failed: nothing usable")), "failing or failed"), st(sum(h$errors), "error values"), st(nrow(o$gaps), "gaps")) })
  output$he_tbl <- renderDT({ o <- outs(); req(o); h <- o$health; if (input$he_show == "bad") h <- h[health != "ok"]
    ovid_dt(h[order(match(health, c("failed: nothing usable", "failing: mostly errors", "intermittent", "suspect", "ok")), logger_id, channel),
            .(health, logger_id, channel, site, depth_m, depth_source, values_read, errors, removed_qc, flagged, values_kept, pct_valid, gaps, longest_gap_h, first_valid, last_valid)], page = 15) |> health_style("health") })
  output$he_gaps <- renderDT({ o <- outs(); req(o); ovid_dt(o$gaps[order(-length_h)], page = 10) })
  output$he_events <- renderDT({ o <- outs(); req(o); validate(need(nrow(o$events) > 0, "No logger events.")); ovid_dt(o$events[order(logger_id, TIMESTAMP)], page = 10) })
  output$he_bat <- renderPlot({ o <- outs(); req(o); b <- o$battery; validate(need(nrow(b) > 0, "No battery readings (GeoPrecision loggers write them)."))
    b[, date := as.Date(date)]
    ggplot(b, aes(date, volts_min, colour = logger_id)) + geom_line(linewidth = 0.5) + scale_colour_manual(values = rep(CAT_PAL, 10)[seq_len(uniqueN(b$logger_id))], name = NULL) +
      labs(title = "Logger battery", subtitle = "Daily minimum (V)", x = NULL, y = "Battery (V)") }, bg = MI$bg)
