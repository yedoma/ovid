# GT/pages/clock.R  --  Clock.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Clock --------------------------------------------------------------------------------------------------------------
  output$ck_tbl <- renderDT({ o <- outs(); req(o); validate(need(nrow(o$clock) > 0, "No clock faults found.")); x <- copy(o$clock)
    if ("drift_s" %in% names(x)) x[, drift := fmt_span(drift_s)]; ovid_dt(x, page = 12) })
  output$ck_files <- renderDT({ o <- outs(); req(o); ovid_dt(o$files[use %in% TRUE, .(file = rel, format, serial, zone_read_in = zone, why = zone_why, stated_in_file = stated_offset, first_ts, last_ts)], page = 12) })
