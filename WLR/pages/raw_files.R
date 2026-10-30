# WLR/pages/raw_files.R  --  Raw files.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Raw files ----------------------------------------------------------------------------------------------------------------------
  rf_rows <- reactive({ o <- outs(); req(o); f <- o$files
    f <- switch(input$rf_show, used = f[use %in% TRUE], unused = f[!use %in% TRUE], baro = f[kind == "baro"], f)
    f[order(kind, serial, first_ts), .(name, rel, status, use, kind, year, format, serial, model, title, tz = ifelse(is.na(offset_hours), "none stated", sprintf("UTC%+g", offset_hours)),
                                       unit_pres, first_ts, last_ts, rows, interval = fmt_step(interval_s), pair = ifelse(is.na(pair) | !nzchar(as.character(pair)), "", basename(as.character(pair))), note)] })
  output$rf_tbl <- renderDT(ovid_dt(rf_rows(), selection = "multiple", page = 15) %>% status_style("status"))
  rf_rule <- function(action, serial = "", offset = "") { s <- input$rf_tbl_rows_selected; if (!length(s)) { note("Select files in the table first.", type = "warning"); return() }
    f <- rf_rows()[s]
    rows <- data.frame(file = f$rel, action = action, serial = serial, offset_hours = offset, reason = trim(input$rf_reason), author = trim(input$rf_author), stringsAsFactors = FALSE)
    if (m_fr$append(rows)) { mark(); note(nrow(rows), " rule(s) added. Run the pipeline to apply.") } }
  observeEvent(input$rf_ignore, rf_rule("ignore")); observeEvent(input$rf_use, rf_rule("use"))
  observeEvent(input$rf_rescan, { if (VIEWER) return(); s <- input$rf_tbl_rows_selected; if (!length(s)) return(note("Select files first.", type = "warning"))
    if (run_busy()) return(note("A run is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    cfg <- current_cfg(); n <- forget_wlr_files(cfg$project_dir, rf_rows()$rel[s])
    do_run(cfg, after = function() { m <- tryCatch(fread(file = file.path(path.expand(cfg$project_dir), "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
      g <- function(k) { v <- m$value[match(k, m$key)]; if (is.na(v)) "?" else v }
      note("Rescanned ", length(s), " file(s). Reused from the last run: ", g("files_reused"), " file(s) as read and ", g("plots_reused"), " plot(s).", duration = 10) }) })
  observeEvent(input$rf_setserial, { if (!nzchar(trim(input$rf_serial))) return(note("Type the serial first.", type = "warning")); rf_rule("serial", trim(input$rf_serial)) })
  observeEvent(input$rf_baro, rf_rule("baro")); observeEvent(input$rf_water, rf_rule("water"))
  output$rf_over <- renderDT({ o <- outs(); req(o); ovid_dt(o$overlaps, page = 12) })
