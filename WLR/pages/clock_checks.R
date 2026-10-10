# WLR/pages/clock_checks.R  --  Clock checks.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Clock checks -----------------------------------------------------------------------------------------------------------------------
  output$ck_files <- renderDT({ o <- outs(); req(o); f <- o$files[use %in% TRUE]
    if (!"zone" %in% names(f)) f[, `:=`(zone = ifelse(is.na(tz_override), offset_hours, tz_override), zone_why = "")]
    ovid_dt(f[order(is.na(offset_hours), serial), .(name, serial, format, `zone in the file` = ifelse(is.na(offset_hours), "none", sprintf("UTC%+g", offset_hours)),
                                                     `read as` = ifelse(is.na(zone), zone_text(BASE_OFFSET), sprintf("UTC%+g", zone)), why = zone_why, first_ts, last_ts)], page = 12) })
