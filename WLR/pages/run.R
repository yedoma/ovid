# WLR/pages/run.R  --  Run.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- run ---------------------------------------------------------------------------------------------------------
  # No file states its time zone and none has been chosen for such files: ask which zone the loggers' clocks were in.
  ask_zone <- function(msg) showModal(modalDialog(title = "Which time zone were these loggers set to?", p(msg),
    radioButtons("uz_choice", NULL, c(zone_choices(), "Another offset" = "other"), as.character(BASE_OFFSET)),
    numericInput("uz_other", "Other offset (hours from UTC)", BASE_OFFSET, step = 1, width = "200px"),
    footer = tagList(modalButton("Cancel"), actionButton("uz_go", "Use this zone and run", class = "btn-primary")), easyClose = FALSE))
  observeEvent(input$uz_go, { z <- if (identical(input$uz_choice, "other")) input$uz_other else as.numeric(input$uz_choice)
    removeModal(); if (is.null(z) || is.na(z)) return(); SETTINGS$unzoned_offset_h <<- z   # kept with the settings by the run (save_settings)
    cfg <- current_cfg(); cfg$unzoned_offset_h <- z; do_run(cfg) })
  # Runs in the background (core runner.R): the app stays usable, the corner box shows progress and Cancel.
  do_run <- function(cfg, after = NULL) {
    if (run_busy()) return(note("A run is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    save_settings(cfg); LOG$lines <- character()
    run_start(session, cfg, function(r) {
      if (identical(r$status, "asked")) return(ask_zone(r$message))
      LOG$lines <- c(r$lines, if (r$status == "failed") paste("STOPPED:", r$message))
      rv$log <- LOG$lines; rv$tick <- rv$tick + 1; rv$zoom <- NULL; rv$pf <- rv$pf + 1
      if (identical(r$status, "cancelled")) return(note("Run cancelled. Files it had written are kept; the next run redoes what it did not finish.", type = "warning", duration = 10))
      if (r$status == "failed") return(note("The pipeline stopped: ", r$message, ". The full log is under More > Overview, in Log of the last run.", type = "error", duration = NULL))
      rv$stale <- FALSE; o <- outs(); redone <- run_redone(cfg$project_dir)
      if (!is.null(o) && nrow(o$problems)) note("Finished, with ", nrow(o$problems), " problem(s) that did not stop the run. See the Set-up check.", redone, type = "warning", duration = 10)
      else note("Pipeline finished.", redone)
      if (is.function(after)) after() })
  }
  observeEvent(input$run, {
    if (VIEWER) return(); cfg <- current_cfg()
    if (!length(cfg$raw_dirs) || !any(dir.exists(path.expand(cfg$raw_dirs)))) { note("Set the data folders ", if (embedded) "under Import data" else "on 1 Project", " first.", type = "warning"); go("Project"); return() }
    for (m in all_tables()) if (isTRUE(isolate(m$dirty()))) try(m$save(quiet = TRUE), silent = TRUE)   # unsaved edits in any table, as Apply saves them
    p <- tryCatch(setup_check(cfg), error = function(e) { showNotification(paste("The set-up check could not run:", conditionMessage(e)), type = "warning"); NULL }); e <- if (is.null(p)) NULL else p[level == "error"]
    if (!is.null(e) && nrow(e)) return(showModal(setup_gate(e, "run_review", "run_force")))   # core shell.R
    do_run(cfg)
  })
  observeEvent(input$run_review, { removeModal(); go("Preflight") })
  observeEvent(input$run_force, { removeModal(); cfg <- current_cfg()          # the run's log records what the set-up check found
    p <- tryCatch(setup_check(cfg), error = function(e) { showNotification(paste("The set-up check could not run:", conditionMessage(e)), type = "warning"); NULL }); cfg$run_anyway <- setup_problem_lines(if (is.null(p)) NULL else p[level == "error"]); do_run(cfg) })
  out_seen <- ovid_output_watch(session, pd)                    # the output folder deleted or replaced outside the app (core shell.R)
  applied_server("applied", pd, out_seen, edit = if (!VIEWER) "QC")   # the corrections applied, in the top bar (core applied.R)
  # safe: shown as no output yet; a run writes it again
  outs <- reactive({ rv$tick; out_seen(); tryCatch(load_outputs(pd()), error = function(e) NULL) })
  wl_files <- reactive({ outs(); f <- tryCatch(fread(file = file.path(pd(), "output", "audit", "files.csv"), colClasses = "character"), error = function(e) NULL)   # the logger list's grouping (core grouping.R)
    if (is.null(f) || !nrow(f)) NULL else f[nzchar(serial), .(serial, file = path, first = first_ts, last = last_ts)] })
  wl_deps <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$summary)) NULL else o$summary[, .(serial = as.character(serial), from = fmt_ts(start), to = fmt_ts(end), name = site)] })
  wl_level <- function(serial) { f <- list.files(file.path(pd(), "output", "sites"), paste0("^", rx_escape(serial), "__.*\\.csv$"), full.names = TRUE)   # water above the sensor, for moves
    # safe: a file that cannot be read adds nothing to this preview
    if (!length(f)) return(NULL); d <- rbindlist(lapply(f, function(x) tryCatch(fread(file = x, select = c("TIMESTAMP", "depth_m")), error = function(e) NULL)), fill = TRUE)
    if (!nrow(d)) NULL else d[, .(t = parse_dt(TIMESTAMP), v = depth_m)] }
  gb <- group_by_server("gb", wl_files, pd, wl_deps, level = wl_level, jump_min = 0.05, unit = "m", open = function(r) { s <- wl_deps(); s <- if (!is.null(s)) s[serial == r$serial] else NULL
    if (!is.null(s) && nrow(s)) { site_cur(s$name[1]); go("Site") } else showNotification(paste(r$serial, "has no site yet: give it one on Loggers and deployments."), type = "message") })
  output$has_out <- reactive(!is.null(outs())); outputOptions(output, "has_out", suspendWhenHidden = FALSE)
  output$runstate <- renderUI({ o <- outs()
    if (isTRUE(rv$stale)) ovid_status("Set-up changed. Run to apply.", "warn")
    else if (is.null(o)) ovid_status("Not run yet", "off")
    else ovid_status(paste("Up to date. Last run", format(o$when, "%Y-%m-%d %H:%M")), "ok") })
  output$log <- renderUI(pre(class = "log", paste(if (length(rv$log)) rv$log else "No run in this session yet.", collapse = "\n")))
