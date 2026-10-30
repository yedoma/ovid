# CS/pages/logger_programs.R  --  Logger programs.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Logger programs ---------------------------------------------------
  progs <- reactive({ rv$prog_tick; tryCatch(load_programs(list(project_dir = pd())), error = function(e) list()) })
  prog_matches <- reactive({ rv$prog_tick; rv$tick
    # safe: an output of the last run, only shown
    f <- file.path(pd(), "config", "program_matches.csv"); live_stamp(f); if (file.exists(f)) tryCatch(fread(file = f, na.strings = ""), error = function(e) NULL) else NULL })
  observe({ p <- progs(); updateSelectInput(session, "prog_pick", choices = c("(none)" = "", vapply(p, `[[`, "", "name")), selected = isolate(input$prog_pick)) })
  prog_run <- function() {
    cfg <- current_cfg(); if (!folders_ok(cfg)) return()
    res <- withProgress(message = "Matching programs to files", value = 0.3, safely(program_report(cfg), "Matching stopped"))
    rv$prog_tick <- rv$prog_tick + 1; if (!is.null(res)) rv$stale <- TRUE
  }
  observeEvent(input$prog_up, {
    if (VIEWER) return()
    fsl <- grepl("\\.fsl$", input$prog_up$name, ignore.case = TRUE)        # Edlog label files: for the CR10X files page
    if (any(fsl)) { dir.create(fsl_dir(), recursive = TRUE, showWarnings = FALSE)
      file.copy(input$prog_up$datapath[fsl], file.path(fsl_dir(), input$prog_up$name[fsl]), overwrite = TRUE); rv$fsl_sel <- input$prog_up$name[fsl][1] }
    got <- c(add_programs(list(project_dir = pd()), input$prog_up$datapath[!fsl], input$prog_up$name[!fsl]), input$prog_up$name[fsl])
    rv$fsl_tick <- rv$fsl_tick + 1
    note(if (length(got)) paste("Added", paste(got, collapse = ", ")) else "Those programs are already here.")
    prog_run()
  })
  observeEvent(input$prog_scan, {
    if (VIEWER) return()
    fd <- path.expand(trim(input$prog_dir %||% ""))
    if (nzchar(fd)) {
      if (!dir.exists(fd)) return(note("That folder was not found. Check the path.", type = "warning"))
      f <- list.files(fd, pattern = PROG_EXT, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      got <- add_programs(list(project_dir = pd()), f); rv$fsl_tick <- rv$fsl_tick + 1
      note(length(f), " program files found, ", length(got), " new.")
    }
    prog_run()
  })
  output$prog_summary <- renderUI({
    m <- prog_matches(); p <- progs()
    if (!length(p)) return(p(class = "dim", "No programs yet."))
    if (is.null(m) || !nrow(m)) return(p(length(p), " programs. Press Copy and match to match them to the .dat files."))
    p(length(p), " programs. ", tags$b(sum(m$status == "matched")), " matched, ", tags$b(sum(grepl("^matched by", m$status))), " by columns only, ",
      tags$b(sum(grepl("differ", m$status))), " with different columns, ", tags$b(sum(m$status %in% c("no program", "program lacks this table"))), " with no program, out of ", nrow(m), ".")
  })
  output$prog_matches <- renderDT({
    m <- prog_matches(); req(m, nrow(m))
    v <- function(x) if (x %in% names(m)) m[[x]] else rep("", nrow(m))
    show <- data.table(File = m$file, Table = m$table, Match = m$status, Program = m$program, How = v("how"), `In header` = m$program_in_header,
                       `Written every` = v("interval"), Scan = v("scan"), `Not in program` = v("missing_in_program"), `Not in file` = v("missing_in_file"))
    status_style(ovid_dt(show, selection = "none", page = 10), "Match")
  })
  output$prog_cols <- renderDT({
    req(nzchar(input$prog_pick %||% "")); p <- Filter(function(x) x$name == input$prog_pick, progs()); req(length(p))
    d <- p[[1]]$columns; validate(need(nrow(d) > 0, "No output tables found in this program."))
    keep <- intersect(c("table", "position", "column", "units", "process", "interval", "scan", "measured_by", "source", "note"), names(d))
    d <- d[, ..keep]; nice <- c(table = "Table", position = "Position", column = "Column", units = "Units", process = "Processing", interval = "Written every",
                                scan = "Scan", measured_by = "Measured by", source = "Variable", note = "Note")
    setnames(d, keep, nice[keep]); ovid_dt(d, selection = "none", page = 15)
  })

