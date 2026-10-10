# CS/pages/station_map.R  --  Station map.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Station map --------------------------------------------------------------
  MAP_STATUS <- c(recording = "Recording (data in the last 60 days of the project)", ended = "Stopped recording", nodata = "No data in this project")
  map_rows <- reactive({
    si <- m_si$data(); if (is.null(si) || !nrow(si)) return(data.table(station = character(), records = numeric(), first = character(), last = character(), loggers = character(),
                                                                      lat = numeric(), lon = numeric(), elevation_m = character(), description = character(), status = character()))   # no stations yet
    si <- ensure_cols(as.data.table(si), SITE_COLS)
    si[, `:=`(lat = suppressWarnings(as.numeric(latitude)), lon = suppressWarnings(as.numeric(longitude)))]
    o <- outs(); st <- if (!is.null(o) && nrow(o$stations)) o$stations else data.table(station = character(), records = numeric(), first = character(), last = character())
    lg <- if (!is.null(o) && nrow(o$lineage) && "logger_id" %in% names(o$lineage)) o$lineage[, .(loggers = paste(unique(logger_id), collapse = ", ")), by = station] else data.table(station = character(), loggers = character())
    agg <- st[, .(records = sum(as.numeric(records), na.rm = TRUE), first = min(substr(first, 1, 10)), last = max(substr(last, 1, 10))), by = station]
    d <- merge(merge(agg, lg, by = "station", all.x = TRUE), si[, .(station, lat, lon, elevation_m, description)], by = "station", all = TRUE)
    newest <- suppressWarnings(max(as.Date(d$last), na.rm = TRUE))
    d[, status := fifelse(is.na(records) | records == 0, "nodata", fifelse(!is.na(as.Date(last)) & as.Date(last) >= newest - 60, "recording", "ended"))]
    d[]
  })
  output$map_note <- renderUI({ d <- map_rows(); n <- d[!is.na(lat) & !is.na(lon)]
    if (!nrow(d)) return(empty_msg("No stations yet. Run the pipeline, then give coordinates under Station details on Sites and field log."))
    p(class = "dim", sprintf("%d of %d stations with data have coordinates.", nrow(n[status != "nodata"]), nrow(d[status != "nodata"]))) })
  observe({
    req(identical(input$nav, "Map")); d <- map_rows(); req(nrow(d))
    d <- d[!is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180]
    if (!identical(input$map_show, "all")) d <- d[status != "nodata"]
    col <- c(recording = GB$green, ended = GB$orange, nodata = GB$gray)
    items <- if (nrow(d)) lapply(seq_len(nrow(d)), function(i) list(station = d$station[i], lat = d$lat[i], lon = d$lon[i], col = unname(col[d$status[i]]),
      status = unname(MAP_STATUS[d$status[i]]), records = if (is.na(d$records[i])) "" else num(d$records[i]), first = d$first[i] %||% "", last = d$last[i] %||% "",
      loggers = d$loggers[i] %||% "", elev = d$elevation_m[i] %||% "", desc = d$description[i] %||% "")) else list()
    shown <- intersect(names(col), unique(d$status))
    session$sendCustomMessage("csmap", list(items = items, labels = isTRUE(input$map_labels), edge = GB$bg,
      legend = lapply(shown, function(k) list(col = unname(col[k]), label = unname(MAP_STATUS[k])))))
  })
  observeEvent(input$map_pick, open_station(input$map_pick))
  output$map_missing <- renderDT({ d <- map_rows(); req(nrow(d)); d <- d[status != "nodata" & (is.na(lat) | is.na(lon))]
    validate(need(nrow(d) > 0, "Every station with data has coordinates."))
    ovid_dt(d[order(-records), .(station, records = num(records), first, last, loggers)], page = 10) })

