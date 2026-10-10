# CS/pages/run.R  --  run.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- run ---------------------------------------------------------------
  observeEvent(input$vb_build, {
    if (VIEWER) return()
    res <- withProgress(message = "Building viewer bundle", value = 0.5, safely(make_viewer_bundle(pd(), include_detail = isTRUE(input$vb_detail)), "Bundle not built"))
    if (!is.null(res)) { rv$bundle <- res; note("Viewer bundle built.") }
  })
  output$vb_out <- renderUI({ b <- rv$bundle; if (is.null(b)) return(NULL)
    div(style = "margin-top:10px", p(num(b$files), " files, ", num(b$mb), " MB, in ", code(b$dir)),
        if (!b$parquet) p(class = "dim", "Tip: choose Parquet above and run again before building; the bundle will be several times smaller and plots faster."),
        p(class = "dim", "Publish from R: ", code(paste0('rsconnect::deployApp("', b$dir, '")'))))
  })
  all_tables <- function() unname(E)                              # every set-up table editor (SETUP_TABLES, pages/utils_setup.R)
  do_run <- function(cfg, stay = embedded, after = NULL) {
    if (run_busy()) return(note("A run is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    save_settings(cfg)
    app_run(cfg, function(res, r) {
      rv$log <- LOG$lines; rv$tick <- rv$tick + 1; rv$zoom <- NULL; rv$detail <- NULL
      if (identical(r$status, "cancelled")) note("Run cancelled. Files it had written are kept; the next run redoes what it did not finish.", type = "warning", duration = 10)
      else if (is.null(res)) note("The pipeline stopped: ", r$message, ". The full log is at the bottom of the Home page.", type = "error", duration = NULL)
      else { rv$stale <- FALSE; redone <- run_redone(cfg$project_dir)
        if (nrow(res$problems)) note("Finished, with ", nrow(res$problems), " problem(s) that did not stop the run. See Loggers and files, Problems.", redone, type = "warning", duration = 10)
        else note("Pipeline finished.", redone)
        if (is.function(after)) after()
        if (!stay) go("Home") } })
  }
  observeEvent(input$run, {
    if (VIEWER) return()
    cfg <- current_cfg(); if (!folders_ok(cfg)) return()
    for (m in all_tables()) try(m$save(quiet = TRUE), silent = TRUE)
    p <- tryCatch(preflight(cfg), error = function(e) NULL); rv$pf_tick <- (rv$pf_tick %||% 0) + 1
    e <- if (is.null(p)) NULL else p[level == "error"]
    if (!is.null(e) && nrow(e)) return(showModal(setup_gate(e, "run_review", "run_force")))   # core shell.R
    do_run(cfg)
  })
  observeEvent(input$run_review, { removeModal(); go("Preflight") })
  observeEvent(input$run_force, { removeModal(); cfg <- current_cfg()          # the run's log records what the set-up check found
    p <- tryCatch(preflight(cfg), error = function(e) NULL); cfg$run_anyway <- setup_problem_lines(if (is.null(p)) NULL else p[level == "error"]); do_run(cfg) })
  out_seen <- ovid_output_watch(session, pd)                    # the output folder deleted or replaced outside the app (core shell.R)
  applied_server("applied", pd, out_seen, edit = if (!VIEWER) "Quality rules")   # the corrections applied, in the top bar (core applied.R)
  outs <- reactive({ rv$tick; out_seen(); tryCatch(load_outputs(pd()), error = function(e) NULL) })
  output$d_context <- renderUI(switch(input$nav %||% "",
    Explore = ovid_context(input$ex_tab, if (!identical(input$ex_tab, "Compare stations")) input$ex_station), NULL))
  output$has_out <- reactive(!is.null(outs())); outputOptions(output, "has_out", suspendWhenHidden = FALSE)
  output$runstate <- renderUI({
    o <- outs()
    if (VIEWER) ovid_status(if (is.null(o)) "No data" else paste("Data as of", format(o$when, "%b %d %Y, %H:%M")), if (is.null(o)) "off" else "ok")
    else if (isTRUE(rv$stale)) ovid_status("Set-up changed. Run to apply.", "warn")
    else if (is.null(o)) ovid_status("Not run yet", "off")
    else ovid_status(paste("Up to date. Last run", format(o$when, "%b %d, %H:%M")), "ok")
  })

