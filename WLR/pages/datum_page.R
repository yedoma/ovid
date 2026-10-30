# WLR/pages/datum_page.R  --  Datum.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Datum --------------------------------------------------------------------------------------------------------------------
  output$dt_deps <- renderDT({ o <- outs(); req(o); d <- o$deps[has_data == TRUE][order(site, year)]
    ovid_dt(d[, .(site, year, serial, reference = ref_code, T_ST, T_ST_source, measured_to, offset_m, offset_source, z_ref, z_kind, z_ref_source, z_ground)], page = 15) })
  output$dt_links <- renderDT({ o <- outs(); req(o); ovid_dt(o$links, page = 12) })
  observe({ o <- outs(); req(o); updateSelectizeInput(session, "gh_sites", choices = sort(unique(o$ghist$site)), selected = isolate(input$gh_sites)) })
  gh_data <- reactive({ o <- outs(); req(o); h <- o$ghist; if (!nrow(h)) return(h)
    if (length(input$gh_sites)) h <- h[site %in% input$gh_sites]
    if (isTRUE(input$gh_moved)) { mv <- unique(h[!is.na(status) & nzchar(status), .(site, code)]); h <- h[mv, on = .(site, code), nomatch = 0] }
    h[order(site, code, time)] })
  output$gh_plot <- renderPlot({ h <- gh_data(); validate(need(nrow(h) > 0, "No surveys to show.")); plot_gps_history(h) }, bg = NORD$bg)
  output$gh_tbl <- renderDT({ h <- gh_data(); req(nrow(h) > 0)
    ovid_dt(h[, .(site, code, gps_name, time, z, rms, z_kind, change_from_previous_m, change_from_first_m, n_surveys, sd_m, range_m, status, source)], selection = "multiple", page = 12) %>%
      formatStyle("status", color = NORD$yellow, fontWeight = 600) })
  observeEvent(input$gh_exclude, { h <- gh_data(); s <- input$gh_tbl_rows_selected
    if (!length(s)) return(note("Select one or more surveys first.", type = "warning"))
    if (!nzchar(trim(input$gh_reason %||% ""))) return(note("Give a reason first.", type = "warning"))
    rows <- data.frame(gps_name = h$gps_name[s], code = h$code[s], time = format(h$time[s], "%Y-%m-%d %H:%M"), reason = trim(input$gh_reason), author = trim(input$gh_author), stringsAsFactors = FALSE)
    if (m_gex$append(rows)) { mark(); note(nrow(rows), " survey(s) will be left out at the next run.") } })
  output$dt_gps <- renderDT({ o <- outs(); req(o); ovid_dt(o$dgps[code %in% DGPS_CODES, .(gps_name, code, z, z_kind, rms, time, easting, northing, source)], page = 15) })
