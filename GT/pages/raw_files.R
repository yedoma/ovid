# GT/pages/raw_files.R  --  Raw files.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Raw files --------------------------------------------------------------------------------------------------------
  output$rf_bundles <- renderDT({ o <- outs(); req(o); ovid_dt(o$bundles[order(status != "ok", type, serial), .(status, type, serial, folder, source_file, exports, members, advice)], page = 15) |> status_style("status") })
  output$rf_xchk <- renderDT({ o <- outs(); req(o); validate(need(nrow(o$xchk) > 0, "No RBR download has both a .hex and a Ruskin export.")); ovid_dt(o$xchk, page = 8) })
  rf_rows <- reactive({ o <- outs(); req(o); f <- o$files
    f <- switch(input$rf_show, used = f[use %in% TRUE], unused = f[!use %in% TRUE], bad = f[!status %in% c("ok", "duplicate_file", "export_not_used", "companion", "proprietary_not_readable", "water_logger", "raw_not_used")], f)
    f[, .(file = rel, status, format, type, serial, bundle, read = use, values_read, removed, zone, size, md5, model, title, dup_of)] })
  output$rf_tbl <- renderDT(ovid_dt(rf_rows(), selection = "multiple", page = 15) |> status_style("status"))
  rf_rule <- function(action, serial = "") { s <- input$rf_tbl_rows_selected; if (!length(s)) return(note("Select files first.", type = "warning"))
    rows <- data.frame(pattern = rf_rows()$file[s], action = action, serial = serial, reason = trim(input$rf_reason), author = trim(input$rf_author), stringsAsFactors = FALSE)
    if (m_fr$append(rows)) { mark(); note(length(s), " file rule(s) added. Run the pipeline to apply.") } }
  observeEvent(input$rf_ignore, rf_rule("ignore")); observeEvent(input$rf_use, rf_rule("use"))
  observeEvent(input$rf_rescan, { if (VIEWER) return(); s <- input$rf_tbl_rows_selected; if (!length(s)) return(note("Select files first.", type = "warning"))
    if (run_busy()) return(note("A run is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    o <- outs(); f <- o$files[match(rf_rows()$file[s], o$files$rel)]; cfg <- current_cfg()
    lids <- forget_gt_files(cfg$project_dir, file.path(f$dir, f$file))
    do_run(cfg, after = function() { m <- tryCatch(fread(file = file.path(path.expand(cfg$project_dir), "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
      g <- function(k) { v <- m$value[match(k, m$key)]; if (is.na(v)) "?" else v }
      note("Rescanned ", length(s), " file(s) (", length(lids), " logger(s) read again). Reused from the last run: ", g("loggers_reused"), " logger(s) and ", g("sites_reused"), " site(s).", duration = 10) }) })
  observeEvent(input$rf_setserial, { if (!nzchar(trim(input$rf_serial))) return(note("Type the serial first.", type = "warning")); rf_rule("serial", trim(input$rf_serial)) })
