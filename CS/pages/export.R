# CS/pages/export.R  --  Export.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Export ---------------------------------------------------------------
  output$xp_empty <- renderUI(if (is.null(outs())) empty_msg())
  observe({ o <- outs(); req(o, nrow(o$stations) > 0); ch <- setNames(st_value(o), st_label(o))
    updateSelectizeInput(session, "xp_keys", choices = ch, selected = intersect(isolate(input$xp_keys), ch))
    r <- as.Date(range(c(o$stations$first, o$stations$last))); updateDateRangeInput(session, "xp_dates", start = r[1], end = r[2], min = r[1], max = r[2]) })
  observeEvent(input$xp_all, { o <- outs(); req(o); updateSelectizeInput(session, "xp_keys", selected = st_value(o)) })
  observeEvent(input$xp_none, updateSelectizeInput(session, "xp_keys", selected = character()))
  output$xp_size <- renderUI({ o <- outs(); k <- input$xp_keys; if (is.null(o) || !length(k)) return(span(class = "dim", "Pick at least one station table."))
    n <- sum(as.numeric(o$stations$records[st_value(o) %in% k])); span(class = "dim", num(length(k)), " tables, ", num(n), " records. Large selections take a minute or two.") })
  output$xp_dl <- downloadHandler(filename = function() paste0("campbell_export_", Sys.Date(), ".zip"), contentType = "application/zip",
    content = function(file) { k <- input$xp_keys; validate(need(length(k) > 0, "Pick at least one station table.")); o <- outs()
      keys <- rbindlist(lapply(strsplit(k, "|||", fixed = TRUE), function(z) data.table(station = z[1], stream = z[2])))
      r <- as.Date(range(c(o$stations$first, o$stations$last))); full <- identical(as.Date(input$xp_dates), r)
      withProgress(message = "Building export", value = 0, build_export(pd(), keys, file, from = if (full) NULL else input$xp_dates[1], to = if (full) NULL else input$xp_dates[2],
        flags = "flags" %in% input$xp_opts, flag_scheme = input$xp_scheme %||% "wmo",
        out_offset = suppressWarnings(as.numeric(input$xp_tz %||% "")), unzoned_offset = suppressWarnings(as.numeric(input$xp_uz %||% "")), summaries = "summaries" %in% input$xp_opts, report = "report" %in% input$xp_opts, formats = input$xp_rep %||% character(),
        progress = function(i, n, what) setProgress((i - 1) / n, detail = what), estimate = "estimate" %in% input$xp_opts, netcdf = "netcdf" %in% input$xp_opts,
        report_opts = rpo(), hide_accepted = isTRUE(corrections_cfg(CFG)$hide_accepted_flags))) })   # Automatic corrections switch (R/config.R)
  # the report layout (core report_options.R): titles, file names, sections and axes shared by the stations exported together
  rpo <- report_options_server("rpo", sections = CS_REPORT_SECTIONS, names_for = function(o) { k <- input$xp_keys; if (!length(k)) return(NULL)
    z <- rbindlist(lapply(strsplit(k, "|||", fixed = TRUE), function(z) data.table(station = z[1], stream = z[2]))); kk <- safe_name(paste(z$station, z$stream, sep = "__"))
    data.table(name = paste(z$station, z$stream, sep = " / "), title = vapply(seq_len(nrow(z)), function(i) report_title(o, paste0(z$station[i], "  /  ", z$stream[i]), name = z$station[i], app = "CS"), ""),
               file = vapply(seq_len(nrow(z)), function(i) report_file(o, kk[i], name = z$station[i], app = "CS"), "")) })

  # standardising times: stations with no UTC offset need a zone; when none of the chosen stations has one, ask
  xp_tz_need <- reactive({ k <- input$xp_keys; if (!nzchar(input$xp_tz %||% "") || !length(k)) return(NULL)
    st <- unique(vapply(strsplit(k, "|||", fixed = TRUE), `[`, "", 1)); live_stamp(file.path(pd(), "config", "station_info.csv")); so <- station_offsets(pd(), st); list(all = all(is.na(so)), some = st[is.na(so)]) })
  output$xp_tz_ask <- renderUI({ n <- xp_tz_need(); if (is.null(n) || !length(n$some)) return(NULL)
    div(class = if (n$all) "warnline" else "dim",
        p(if (n$all) "None of these stations has a UTC offset in Station details. Which zone were their loggers set to?"
          else paste0("No UTC offset in Station details for ", paste(n$some, collapse = ", "), ". Their times are left as recorded unless you choose a zone:")),
        radioButtons("xp_uz", NULL, c(if (!n$all) c("Leave as recorded" = ""), zone_choices()), if (n$all) as.character(BASE_OFFSET) else "", inline = TRUE)) })

