# GT/pages/setup_flow.R  --  The set-up flow: what shows, Needs you, Apply, the information panel.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- the set-up flow (core flow.R): what shows, Needs you, Apply, the information panel ------------------------------------
  site_all <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$ss)) character() else sort(unique(o$ss$site)) })
  site_shown <- display_server("disp", pd, site_all)
  output$flow_ctx <- renderUI({ live_stamp(display_path(pd())); display_context(pd(), site_all(), "sites") })

  # Needs you: the one review list (core flow.R): the shown sites' flags, loggers that look moved and the run's findings
  rl_flags <- reactive({ o <- outs(); f <- o$flags; if (is.null(f) || !nrow(f)) return(NULL); f <- copy(f)[site %in% site_shown()]
    ty <- if (!is.null(o$loggers) && nrow(o$loggers)) o$loggers$logger_type[match(as.character(f$logger_id), as.character(o$loggers$logger_id))] else rep(NA_character_, nrow(f))
    f[, sensor := ifelse(ty %in% "em50", sensor_of(channel, "em50"), channel)]   # an EM50 channel (P1_temp) is a variable of its sensor (P1): R/qc.R
    if (!"variable" %in% names(f)) f[, variable := ""]
    f[, key := paste0(ifelse(variable == "temp", "temp:", "soil:"), site)]                  # the Workbench's table of the record
    review_flag_items(f, "site", "channel", keep = c("key", "sensor", "variable"), join = c("sensor", "variable")) })
  review <- review_list_server("rl", reactive(rbind(rl_flags(),
      tryCatch(gb$suggestions()[, .(kind = "moved", where = serial, what, serial, at)], error = function(e) NULL), gt_notes(), fill = TRUE)),
    reactive(review_path(pd())), period = if (!VIEWER) E$period, split = function(s, a) gb$accept(s, a),
    open = function(r) { updateSelectInput(session, "ex_site", selected = r$where)
      updateDateRangeInput(session, "ex_dates", start = as.Date(substr(r$start, 1, 10)) - 1, end = as.Date(substr(r$end, 1, 10)) + 1); go("Explore") },
    remove = function(r) for (sn in strsplit(r$sensor, ", ", fixed = TRUE)[[1]])                # a removal per sensor (corrections over a period)
      m_man$append(data.frame(site = r$where, serial = "", sensor = sn, depth_m = "", variable = gsub(", ", "|", r$variable %|% "", fixed = TRUE), start = r$start, end = r$end,
        min_value = "", max_value = "", action = "remove", reason = paste("Needs you:", r$what), author = "", stringsAsFactors = FALSE)))
  if (!VIEWER) unsaved_server("uns", editors = E, on_apply = mark)
  output$saved_hist <- renderUI({ rv$tick; for (m in E) if (is.function(m$data)) m$data(); flow_saved(pd()) })

  # the information panel and the status line of each step
  output$flow_info <- renderUI({ o <- outs(); s <- site_all(); x <- review$items(); n_iss <- sum(x$kind == "flag")
    gp <- if (!is.null(o) && nrow(o$gaps)) { g <- copy(o$gaps)[order(-as.numeric(length_h))][as.numeric(length_h) >= 24]
      g <- g[, .(channel = paste(channel, collapse = ", "), length_h = length_h[1]), by = .(logger_id, from, to)]   # one row per logger and stretch, not per channel
      if (nrow(g)) data.table(where = paste(g$logger_id, ifelse(grepl(",", g$channel), paste0("(", lengths(strsplit(g$channel, ",")), " channels)"), g$channel)), what = paste(flow_span(g$length_h), "without records"),
                              when = paste(substr(g$from, 1, 10), "to", substr(g$to, 1, 10)), go = "Needs") else data.table() } else data.table()
    flow_aside(project = list(
        list("Data files", if (!is.null(o)) num(nrow(o$files)) else "\u2014"),
        list("Loggers", if (!is.null(o)) num(nrow(o$loggers)) else "\u2014"),
        list("Sites shown", sprintf("%d of %d", length(site_shown()), length(s))),
        list("Records", if (!is.null(o) && nrow(o$ss)) paste(substr(min(o$ss$first), 1, 7), "to", substr(max(o$ss$last), 1, 7)) else "\u2014"),
        list("Data check", if (is.null(o)) "\u2014" else if (n_iss) paste(n_iss, "to look at") else "nothing flagged", level = if (is.null(o)) NULL else if (n_iss) "warn" else "ok", go = "Needs"),
        list("Last run", if (!is.null(o)) format(o$when, "%Y-%m-%d %H:%M") else "never")),
      gaps = gp) })
  observe({ o <- outs(); x <- review$items()
    ovid_steps(session, list(
      Project = if (nzchar(trim(input$project_dir %||% ""))) list("ok", "Folders set") else list("off", "Choose the folders"),
      Files = if (!is.null(o)) list("ok", paste(num(nrow(o$files)), "files")) else list("off", "Not read yet"),
      Sensors = if (!is.null(o) && nrow(o$ss) && any(o$ss$assumed_depths > 0)) list("warn", paste(sum(o$ss$assumed_depths > 0), "sites with assumed depths")) else if (!is.null(o)) list("ok", "Every depth given") else list("off", ""),
      Shown = list("info", sprintf("%d of %d shown", length(site_shown()), length(site_all()))),
      Sites = if (!is.null(o) && nrow(o$ss) && any(is.na(o$ss$landcover) | !nzchar(as.character(o$ss$landcover)))) list("warn", "Landcover missing") else list("off", ""),
      Corrections = list("info", "On by default"),
      Needs = if (nrow(x)) list("warn", paste(nrow(x), "to look at")) else if (!is.null(o)) list("ok", "Nothing waits") else list("off", "Run once first"),
      Export = if (!is.null(o)) list("info", "One report, every site") else list("off", "")))
  })
