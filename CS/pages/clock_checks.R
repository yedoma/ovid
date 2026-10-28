# CS/pages/clock_checks.R  --  Clock checks.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Clock checks -------------------------------------------------------
  ck_names <- c(clock_went_backwards = "Clock went backwards", logger_restarted = "Logger restarted", impossible_date = "Impossible dates",
                off_the_time_grid = "Off the time grid", stray_records = "Corrupt timestamps left out", clock_differs_between_files = "Files shifted against each other", files_disagree = "Files disagree")
  ck_explain <- c(
    clock_went_backwards = "The logger's clock jumped back in time, which usually follows a power loss or someone setting the clock. Records written after the jump can sit on top of earlier ones. If that later block is wrong, remove it, or move it by the size of the jump.",
    logger_restarted = "The record counter started again. That happens when the logger loses power or is sent a new program. The data are usually fine, but look at the records just before and after for a gap or for corrupted values.",
    impossible_date = "These records are dated before the earliest plausible year or in the future, so they were left out of the cleaned data. After a power loss many loggers restart at a default date and keep counting from there. If you know when they were really recorded, move them back with a shift.",
    stray_records = "A failing logger memory or clock writes records stamped at random times among good ones. These were off the time grid and out of the order the file was written in, so they were left out and listed in the audit trail. To keep them, switch off this correction under Automatic corrections (6 Automatic corrections).",
    off_the_time_grid = "These records do not fall on the table's normal interval. They are typically written while the clock is being set or is drifting. They are kept; remove them if they repeat neighbouring records.",
    clock_differs_between_files = "Two downloads of the same logger disagree at the same timestamps, but agree once one of them is moved in time. One was collected with the clock off, often a daylight-saving or time-zone setting on the laptop. Shift the file named here to line the two up.",
    files_disagree = "This file disagrees with the logger's other files and no time shift explains it. It may belong to a different logger filed under the same id (split it on the Stations page) or be an edited copy.")
  ck_all <- reactive({ o <- outs(); req(o); d <- o$checks; if (!nrow(d)) return(d)
    d[, kind := fifelse(check %in% names(ck_names), ck_names[check], check)]; d })
  observe({ d <- ck_all(); ty <- if (nrow(d)) d[, .N, by = .(check, kind)][order(-N)] else NULL
    updateSelectInput(session, "ck_type", choices = c("Every kind of finding" = "all", if (!is.null(ty)) setNames(ty$check, paste0(ty$kind, " (", ty$N, ")")))) })
  ck_view <- reactive({ d <- ck_all(); if (nrow(d) && !identical(input$ck_type, "all")) d <- d[check == input$ck_type]; d })
  output$ck_summary <- renderUI({
    if (is.null(outs())) return(empty_msg()); d <- ck_all()
    if (!nrow(d)) return(div(class = "panel-x", span(class = "okline", icon("check")), " No clock problems were found in the last run."))
    s <- d[, .(n = .N, loggers = uniqueN(logger_id)), by = kind][order(-n)]
    div(class = "stats", lapply(seq_len(nrow(s)), function(i) div(class = "stat", tags$b(num(s$n[i])), span(paste0(s$kind[i], ", ", s$loggers[i], " logger", if (s$loggers[i] > 1) "s" else "")))))
  })
  output$ck_tbl <- renderDT({
    d <- ck_view(); validate(need(nrow(d) > 0, "Nothing to show."))
    ovid_dt(d[, .(Finding = kind, Station = station, Logger = logger_id, Table = table, From = from, To = to, Records = records,
                `What was seen` = detail, File = sub("#A[0-9_f]+$", "", basename(source_file)))], selection = "single", page = 12) |>
      formatStyle("Finding", fontWeight = 600, color = GB$yellow) |> formatRound("Records", 0)
  })
  output$ck_dl <- downloadHandler(filename = function() paste0("clock_checks_", Sys.Date(), ".csv"), content = function(f) fwrite(ck_view(), f))
  ck_sel <- reactive({ i <- input$ck_tbl_rows_selected; d <- ck_view(); if (length(i) != 1 || i > nrow(d)) NULL else d[i] })
  output$ck_fix <- renderUI({
    r <- ck_sel(); if (is.null(r)) return(p(class = "dim", style = "margin-top:10px", "Select a finding above to see what it means and what you can do."))
    sh <- r$suggest_shift_min; has <- !is.na(sh)
    div(class = "panel-x", style = "margin-top:14px",
      h3(r$kind, span(class = "dim", style = "font-weight:400", paste0("  logger ", r$logger_id, ", ", r$table))),
      div(class = "explain", unname(ck_explain[r$check]) %||% ""),
      p(r$detail),
      div(class = "editor-only",
        fluidRow(
          column(3, radioButtons("ckf_action", "Correction", c("Move the timestamps" = "shift", "Remove the records" = "remove"),
                                 selected = if (r$check %in% c("off_the_time_grid")) "remove" else "shift")),
          column(3, numericInput("ckf_min", "Move by (minutes, negative = earlier)", value = if (has) sh else NA, width = "100%"),
                    textInput("ckf_real", "or: real time of the first record", placeholder = "YYYY-MM-DD HH:MM", width = "100%")),
          column(3, textInput("ckf_from", "From (as written in the file)", r$from, width = "100%"),
                    textInput("ckf_to", "To", r$to, width = "100%")),
          column(3, ovid_switch("ckf_file", paste0("Only in ", sub("#A[0-9_f]+$", "", basename(r$source_file))), TRUE, width = "100%"),
                    textInput("ckf_reason", "Reason (goes in the audit trail)", width = "100%"),
                    textInput("ckf_author", "Your initials", isolate(who()), width = "100%"))),
        div(class = "btnrow d-hide-std", actionButton("ckf_add", "Add this correction", class = "btn-primary"),
            span(class = "dim", "It is applied the next time the pipeline runs, and recorded in the audit trail."))))
  })
  observeEvent(input$ckf_add, {
    r <- ck_sel(); req(r); if (!wb_made(m_tc)) return()
    if (!nzchar(trim(input$ckf_reason))) return(note("Give a reason for the correction.", type = "warning"))
    a <- trim(input$ckf_from); b <- trim(input$ckf_to); mins <- input$ckf_min
    if (input$ckf_action == "shift") {
      real <- trim(input$ckf_real)
      if (nzchar(real)) { rt <- parse_time(real); ft <- parse_time(a)
        if (is.na(rt) || is.na(ft)) return(note("To work out the shift, both the real time and the From time must look like 2019-06-30 14:00.", type = "warning"))
        mins <- as.numeric(difftime(rt, ft, units = "mins")) }
      if (is.null(mins) || is.na(mins) || mins == 0) return(note("Say how many minutes to move the timestamps, or give the real time of the first record.", type = "warning"))
    }
    if ((nzchar(a) && is.na(parse_time(a))) || (nzchar(b) && is.na(parse_time(b)))) return(note("From and To must look like 2019-06-30 or 2019-06-30 14:00.", type = "warning"))
    ok <- m_tc$append(data.frame(logger_id = r$logger_id, table = r$table,
      source_file = if (isTRUE(input$ckf_file)) sub("#A[0-9_f]+$", "", basename(r$source_file)) else "",
      start = a, end = b, action = input$ckf_action, shift_minutes = if (input$ckf_action == "shift") format(round(mins, 2), scientific = FALSE) else "",
      reason = trim(input$ckf_reason), author = trim(input$ckf_author)))
    if (ok) note("Correction added. Run the pipeline to apply it.")
  })

