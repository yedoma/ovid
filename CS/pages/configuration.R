# CS/pages/configuration.R  --  Configuration.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Configuration --------------------------------------------------------
  SET_KEYS <- c("raw_dirs", "project_dir", "skip_tables", "import_cr10x", "station_format", "incremental", "prog_dir",
                vapply(Filter(function(x) isTRUE(x$panel) && nzchar(x$setting %||% ""), CORRECTIONS), `[[`, "", "setting"))   # and the Automatic corrections switches, as Projects saves them
  cf_cols <- function(w) { cc <- CONFIG_FILES[[w]]$cols; if (is.function(cc)) cc() else cc }
  output$cf_export <- downloadHandler(function() paste0("ovid_cs_configuration_", format(Sys.Date()), ".zip"),
    function(file) export_config(pd(), c(current_cfg(), list(prog_dir = trim(input$prog_dir %||% "")))[SET_KEYS], file))
  observeEvent(input$cf_file, { f <- input$cf_file
    rv$imp <- tryCatch(read_config_import(f$datapath, f$name), error = function(e) { note(conditionMessage(e), type = "error"); NULL }) })
  output$cf_preview <- renderDT({ imp <- rv$imp; req(imp)
    x <- rbindlist(lapply(intersect(names(imp), names(CONFIG_FILES)), function(w) { p <- file.path(pd(), "config", CONFIG_FILES[[w]]$file); cc <- cf_cols(w)
      cur <- if (file.exists(p)) ensure_cols(read_cfg(p), cc)[, cc, with = FALSE] else empty_cfg(cc)
      cbind(data.table(table = CONFIG_FILES[[w]]$label), config_diff(cur, imp[[w]])) }))
    if (!is.null(imp$settings)) x <- rbind(x, data.table(table = "Folder settings", added = nrow(imp$settings)), fill = TRUE)
    validate(need(nrow(x) > 0, "The file holds no set-up tables."))
    ovid_dt(x, page = 15) })
  observeEvent(input$cf_apply, { imp <- rv$imp; if (VIEWER) return()
    if (is.null(imp)) return(note("Choose a file to import first.", type = "warning"))
    dir.create(file.path(pd(), "config"), recursive = TRUE, showWarnings = FALSE)
    if (is.null(safely(apply_config_import(imp, pd(), input$cf_mode, current_user(session)), "Import failed"))) return()
    if (isTRUE(input$cf_settings) && !is.null(imp$settings)) {
      s <- setNames(imp$settings$value, imp$settings$setting); val <- function(k) if (k %in% names(s)) s[[k]] else NULL
      if (!is.null(v <- val("raw_dirs"))) updateTextAreaInput(session, "raw_dirs", value = gsub(";", "\n", v))
      if (!is.null(v <- val("project_dir"))) updateTextInput(session, "project_dir", value = v)
      v <- val("prog_dir"); if (is.null(v) || !nzchar(v)) v <- val("fsl_dir")          # an older export's FSL folder is the programs folder now
      if (!is.null(v)) updateTextInput(session, "prog_dir", value = v)
      if (!is.null(v <- val("skip_tables"))) updateTextInput(session, "skip_tables", value = gsub(";", ", ", v))
      for (k in c("import_cr10x", "incremental")) if (!is.null(v <- val(k))) updateCheckboxInput(session, k, value = isTRUE(as.logical(v)))
      if (!is.null(v <- val("station_format"))) updateSelectInput(session, "station_format", selected = v)
      note("Folder settings filled in on the Project step; they are saved as they change.") }
    rv$imp <- NULL; rv$tick <- rv$tick + 1; mark(); note("Configuration imported. The previous tables are in config/history. Run the pipeline to apply.") })

