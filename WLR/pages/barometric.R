# WLR/pages/barometric.R  --  Barometric.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Barometric --------------------------------------------------------------------------------------------------------
  baros_disk <- reactive({ rv$tick; tryCatch(load_baro(pd()), error = function(e) list()) })
  # safe: an output of the last run, only shown
  stations_disk <- reactive({ rv$tick; st <- tryCatch(load_baro_stations(pd()), error = function(e) list()); b <- m_bstn$data()
    nm <- if (!is.null(b) && nrow(b)) unique(b$station) else character(); k <- match(names(st), safe_name(nm)); names(st)[!is.na(k)] <- nm[k[!is.na(k)]]; st })
  output$bo_empty <- renderUI(if (is.null(outs())) empty_msg() else if (!length(baros_disk())) empty_msg("No barologger records were found. Put them in baro/<year> inside the data folder, or mark a file as a barologger on the Raw files page."))
  bo_years <- reactive({ g <- m_bgrp$data(); o <- outs(); sort(unique(c(if (!is.null(g)) g$year, if (!is.null(o)) as.character(o$summary$year))), decreasing = TRUE) })
  observe({ y <- bo_years(); o <- outs(); busiest <- if (!is.null(o) && nrow(o$summary)) as.character(o$summary[, .N, by = year][order(-N, -year)]$year[1]) else y[1]
    updateSelectInput(session, "bo_year", choices = y, selected = if (isTRUE(isolate(input$bo_year) %in% y)) isolate(input$bo_year) else busiest) })
  baro_label <- reactive({ st <- stations_disk(); b <- baros_disk(); bs <- m_bstn$data()
    c(if (length(st)) setNames(names(st), paste0(names(st), "  (station: ", vapply(st, function(x) paste(unique(x$baro_serial), collapse = ", "), ""), ")")),
      if (length(b)) setNames(names(b), paste0(names(b), "  (this serial only)"))) })
  observe({ st <- names(stations_disk()); updateSelectInput(session, "bo_station", choices = st, selected = if (isTRUE(isolate(input$bo_station) %in% st)) isolate(input$bo_station) else st[1]) })
  output$bo_station_plot <- renderPlot({ st <- stations_disk(); validate(need(length(st) && isTruthy(input$bo_station), "No baro stations yet.")); plot_baro_station(st[[input$bo_station]], input$bo_station) }, bg = NORD$bg)
  output$bo_seams <- renderDT({ o <- outs(); req(o); validate(need(nrow(o$seams) > 0, "No station has more than one barologger yet.")); ovid_dt(o$seams, page = 8) })
  output$bo_note <- renderUI({ y <- input$bo_year; req(y); o <- outs(); req(o); s <- o$summary[year == as.integer(y) & kind == "water"]
    p(class = "dim", style = "margin-top:26px", sprintf("%d deployments in %s; %s readings had no barometric pressure within reach in the last run.", nrow(s), y, num(sum(s$baro_missing, na.rm = TRUE)))) })
  # the year's groups as saved or, with none saved for it, the run's suggestion for each group (Save choices puts them in place)
  bo_rows <- reactive({ y <- input$bo_year; req(y); g <- m_bgrp$data(); if (is.null(g)) return(NULL); g <- g[g$year == y, , drop = FALSE]
    o <- outs(); if (nrow(g) || is.null(o)) return(g); bo_suggested_groups(o$sugg, y) })
  output$bo_groups <- renderUI({ y <- input$bo_year; req(y); g <- bo_rows(); mem <- m_bmem$data(); lab <- baro_label()
    if (is.null(g)) return(empty_msg("Groups are suggested on the first run."))
    if (!nrow(g)) return(empty_msg(paste("No groups for", y, "yet. Run the pipeline to have them suggested.")))
    sc <- outs()$sugg; ch <- c("(none)" = "", lab)
    lapply(seq_len(nrow(g)), function(i) { gm <- if (!is.null(mem)) mem$site[mem$year == y & mem$group == g$group[i]] else character()
      best <- if (nrow(sc)) sc[as.character(year) == y & group == g$group[i]][order(-score)] else data.table()
      div(class = "grp", h3(g$group[i]), div(class = "members", length(gm), " site(s): ", paste(gm, collapse = ", ")),
          fluidRow(column(4, selectInput(paste0("bo_p_", i), "Primary barologger", ch, selected = g$primary[i], width = "100%")),
                   column(4, selectInput(paste0("bo_s_", i), "Secondary (fills gaps)", ch, selected = g$secondary[i], width = "100%")),
                   column(4, p(class = "dim", style = "margin-top:26px", if (nrow(best)) sprintf("Suggested: %s (%.0f%% coverage), then %s", best$serial[1], 100 * best$coverage[1], if (nrow(best) > 1) best$serial[2] else "none") else g$notes[i])))) }) })
  observeEvent(input$bo_save, { y <- input$bo_year; g <- m_bgrp$data(); req(g); if (!any(g$year == y)) g <- rbind(g, bo_rows()[, names(g), drop = FALSE])
    k <- which(g$year == y)
    for (j in seq_along(k)) { g$primary[k[j]] <- input[[paste0("bo_p_", j)]] %||% g$primary[k[j]]; g$secondary[k[j]] <- input[[paste0("bo_s_", j)]] %||% g$secondary[k[j]]
      g$notes[k[j]] <- paste("set on the Barometric page", format(Sys.Date())) }
    if (any(g$primary[k] == g$secondary[k] & nzchar(g$primary[k]))) note("A group has the same barologger as primary and secondary.", type = "warning")
    if (m_bgrp$replace(g)) { mark(); note("Barologger choices saved for ", y, ". Run the pipeline to apply them.") } })
  output$bo_plot <- renderPlot({ st <- stations_disk(); validate(need(length(st), "No barologger records."))
    y <- suppressWarnings(as.integer(input$bo_year)); o <- outs(); validate(need(!is.null(o) && !is.na(y), "Run the pipeline to see the barologgers over a year."))
    s <- o$summary[year == y]
    a <- if (nrow(s)) min(s$start) else as.POSIXct(paste0(y - 1, "-09-15"), tz = "UTC"); z <- if (nrow(s)) max(s$end) else as.POSIXct(paste0(y, "-09-15"), tz = "UTC")
    bb <- lapply(st, function(x) x[TIMESTAMP >= a & TIMESTAMP <= z, .(TIMESTAMP, baro_kpa)]); bb <- bb[vapply(bb, nrow, 1L) > 0]
    validate(need(length(bb), "No barologger covers this year.")); plot_baro(bb, paste("Baro stations,", y)) }, bg = NORD$bg)
  output$bo_diff <- renderPlot({ o <- outs(); req(o); validate(need(nrow(o$bcmp) > 0, "Two or more barologgers are needed for a comparison.")); d <- copy(o$bcmp); d[, day := as.Date(day)]; plot_baro_diff(d) }, bg = NORD$bg)
  output$bo_checks <- renderDT({ rv$tick; f <- file.path(pd(), "output", "audit", "baro_checks.csv"); validate(need(file.exists(f), "Run the pipeline to check the barologgers listed above."))
    d <- fread(file = f); keep <- intersect(c("barologger", "reference", "method", "applied", "offset_kpa", "offset_ci_low", "offset_ci_high", "drift_kpa_per_year", "drift_ci_low", "drift_ci_high", "mk_p", "offset_material", "drift_material", "days", "report", "note"), names(d))
    ovid_dt(d[, ..keep], page = 12) })
  output$bo_scores <- renderDT({ o <- outs(); req(o); ovid_dt(o$sugg[as.character(year) == input$bo_year][order(group, -score)], page = 12) })
