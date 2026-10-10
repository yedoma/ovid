# GT/pages/configuration.R  --  Configuration.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Configuration -----------------------------------------------------------------------------------------------------
  # the settings, the switches of the Automatic corrections step, the set-up tables, the deployment sheet's columns and the review decisions
  output$cf_export <- downloadHandler(function() paste0("ovid_gt_configuration_", format(Sys.Date()), ".zip"), function(file) export_config(pd(), current_cfg()[c(SET_KEYS, CORR_KEYS)], file))   # core config.R, with CONFIG_EXTRA
  observeEvent(input$cf_file, { f <- input$cf_file; rv$imp <- tryCatch(read_config_import(f$datapath, f$name), error = function(e) { note(conditionMessage(e), type = "error"); NULL }) })
  output$cf_preview <- renderDT({ imp <- rv$imp; req(imp)
    x <- config_import_preview(imp, pd(), input$cf_mode %||% "replace")                          # the tables, the sheet's columns, the review decisions
    if (!is.null(imp$settings)) x <- rbind(x, data.table(table = "Settings", added = nrow(imp$settings), removed = NA, unchanged = NA, rows_now = NA, rows_after = NA), fill = TRUE)
    ovid_dt(x, page = 12) })
  observeEvent(input$cf_apply, { imp <- rv$imp; if (is.null(imp)) return(note("Choose a file to import first.", type = "warning"))
    apply_config_import(imp, pd(), input$cf_mode, current_user(session))
    if (isTRUE(input$cf_settings) && !is.null(imp$settings)) { s <- list()
      for (i in seq_len(nrow(imp$settings))) { k <- imp$settings$setting[i]; v <- imp$settings$value[i]
        if (!k %in% c(SET_KEYS, CORR_KEYS) || k == "project_dir") next                       # the import stays in this working folder
        s[[k]] <- if (k %in% c(NUM_KEYS, "unzoned_offset_h")) suppressWarnings(as.numeric(v)) else if (k %in% CORR_KEYS) as.logical(v)
                  else if (k == "raw_dirs") strsplit(v, ";")[[1]] else v }
      # saved, and put into the pages' inputs, so the next autosave keeps them rather than the values shown before
      settings_save(SETTINGS_FILE, s); SETTINGS[names(s)] <<- s; live_touch()
      projects_inputs(session, s, setdiff(SET_KEYS, "project_dir"))
      note("Settings imported.") }
    rv$imp <- NULL; rv$tick <- rv$tick + 1; mark(); note("Configuration imported. The previous tables are in config/history. Run the pipeline to apply.") })
