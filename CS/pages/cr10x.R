# CS/pages/cr10x.R  --  CR10X.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- CR10X -------------------------------------------------------------
  fsl_dir <- reactive(file.path(pd(), "config", "fsl"))
  fsl_files <- reactive({ rv$fsl_tick; rv$tick; list.files(fsl_dir(), pattern = "\\.(fsl|txt|csi|dld)$", ignore.case = TRUE) })
  observe({
    want <- rv$fsl_sel; cur <- isolate(input$fsl_pick)
    updateSelectInput(session, "fsl_pick", choices = c("(none)" = "", fsl_files()), selected = if (!is.null(want)) want else cur)
  })
  matches <- reactive({ rv$tick; rv$fsl_tick
    f <- file.path(pd(), "config", "cr10x_fsl_matches.csv"); live_stamp(f); if (file.exists(f)) tryCatch(fread(f, na.strings = ""), error = function(e) NULL) else NULL })
  observeEvent(input$cx_match, {
    if (VIEWER) return()
    cfg <- current_cfg(); if (!folders_ok(cfg)) return()
    fd <- path.expand(trim(input$prog_dir %||% ""))                     # the programs folder (Logger programs); else the files added there
    if (nzchar(fd) && !dir.exists(fd)) return(note("The programs folder on Logger programs was not found. Check the path.", type = "warning"))
    if (!nzchar(fd)) fd <- fsl_dir()
    m_cx$save(quiet = TRUE); save_settings(cfg)
    res <- withProgress(message = "Matching FSLs", value = 0, safely(
      cr10x_automatch(cfg, fd, isTRUE(input$cx_setid), function(i, n) setProgress(i / n, detail = paste(i, "of", n, "files"))), "Matching stopped"))
    if (is.null(res)) return()
    rv$cx_info <- res[c("fsl_found", "fsl_blank", "fsl_copies")]
    rv$tick <- rv$tick + 1; rv$stale <- TRUE
    note(sum(res$matches$status == "matched"), " of ", nrow(res$matches), " files matched.")
  })
  output$cx_match_summary <- renderUI({
    m <- matches(); if (is.null(m)) return(NULL)
    i <- rv$cx_info
    p(tags$b(sum(m$status == "matched")), " matched, ", tags$b(sum(m$status == "partly matched")), " partly, ",
      tags$b(sum(m$status == "no matching FSL")), " with no FSL, out of ", nrow(m), " files.",
      if (!is.null(i)) span(class = "dim", sprintf(" %d different FSLs found among %d copies%s.", i$fsl_found, i$fsl_copies,
                                                   if (i$fsl_blank) sprintf("; %d had blank labels and were ignored", i$fsl_blank) else "")))
  })
  output$cx_matches <- renderDT({
    m <- matches(); req(m)
    show <- m[, .(File = file_name, Match = status, FSL = fsl, Rows = rows, `Labelled %` = rows_labelled_pct, First = first, Last = last,
                  `Arrays with no FSL` = arrays_without_fsl, `Other FSLs that fit` = other_fsl_that_fit, `Copy of` = identical_copy_of)]
    status_style(ovid_dt(show, selection = "none", page = 8), "Match")
  })
  observeEvent(input$fsl_assign, {
    if (!m_cx$set_selected("fsl", input$fsl_pick)) note("Select one or more rows in the table below first.", type = "warning")
  })
  observeEvent(input$cx_assign, {
    id <- trim(input$cx_id)
    if (!nzchar(id)) return(note("Type a logger id first.", type = "warning"))
    if (!m_cx$set_selected("logger_id", id)) note("Select one or more rows in the table below first.", type = "warning")
  })
  output$fsl_preview <- renderTable({
    req(nzchar(input$fsl_pick)); f <- tryCatch(parse_fsl(file.path(fsl_dir(), input$fsl_pick)), error = function(e) list())
    validate(need(length(f) > 0, "No output arrays found in this file. Is it an Edlog .FSL, .CSI or .DLD?"))
    data.frame(Array = names(f), Fields = lengths(f), Labels = vapply(f, paste, "", collapse = ", "))
  }, striped = FALSE, bordered = FALSE, width = "100%", align = "l")
