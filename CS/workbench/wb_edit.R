# CS/workbench/wb_edit.R  --  Corrections from the Workbench: the rows it writes and the dialogs that write them.
# Part of Ovid Core // CS. Sourced by wb_cs.R. Every correction is a row in a set-up table, written through the same
# table editors as the Standard interface (editServer in app.R: history in config/history, saves from the other interface
# noticed), with the same columns; the pipeline applies it on the next run. Raw files are never written.

# ---- rows -------------------------------------------------------------------------------------------------------------
# Does one removal row (corrections over a period, as its view gives it) cover the values of parameters p at times t? As apply_qc() (R/qc.R) reads it: parameter
# blank, "*" or a pattern matching the whole name (ignoring case); start and end blank or around the time, an end given as
# a date covering that whole day. Station and stream are checked by wb_manual_rows().
wb_manual_hit <- function(row, p, t) {
  pat <- row$parameter %||% ""; if (is.na(pat)) pat <- ""
  okp <- if (pat %in% c("", "*")) rep(TRUE, length(p)) else
    tryCatch(grepl(paste0("^(", pat, ")$"), p, ignore.case = TRUE, perl = TRUE), error = function(e) rep(FALSE, length(p)))
  s <- parse_time(row$start %||% ""); e <- parse_time(row$end %||% "")
  if (!is.na(e) && nchar(row$end) <= 10) e <- e + 86399
  okp & (is.na(s) | t >= s) & (is.na(e) | t <= e)
}
# The removal rows (corrections over a period) that apply to one station table (station blank, equal or among a|b; stream blank or equal).
wb_manual_rows <- function(m, station, stream) {
  if (is.null(m) || !nrow(m)) return(integer())
  v <- function(k) { x <- if (k %in% names(m)) as.character(m[[k]]) else rep("", nrow(m)); x[is.na(x)] <- ""; x }
  which(vapply(v("station"), function(w) { w <- period_parts(w); !length(w) || "*" %in% w || station %in% w }, NA, USE.NAMES = FALSE) & (v("stream") == "" | v("stream") == stream))
}
# TRUE for each value (p[i], t[i]) a manual removal covers.
wb_manual_cover <- function(m, station, stream, p, t) {
  out <- rep(FALSE, length(t))
  for (i in wb_manual_rows(m, station, stream)) out <- out | wb_manual_hit(as.list(m[i, , drop = FALSE]), p, t)
  out
}
# The manual removal rows that cover any of the values (p, t).
wb_manual_rows_for <- function(m, station, stream, p, t) {
  i <- wb_manual_rows(m, station, stream)
  i[vapply(i, function(k) any(wb_manual_hit(as.list(m[k, , drop = FALSE]), p, t)), NA)]
}

# Removal rows (corrections over a period, in the old shape) that remove exactly the selected values: one row per run of consecutive records, start
# and end at its first and last record, with the parameters that share that run in one row. times_all: every record
# time of the station table, so that runs are consecutive in the table and not just in the rows shown.
wb_removal_rows <- function(times_all, p, t, station, stream, reason, author) {
  x <- unique(data.table(p = p, i = match(as.numeric(t), as.numeric(times_all))))[!is.na(i)][order(p, i)]
  if (!nrow(x)) return(data.table(station = character(), stream = character(), parameter = character(), start = character(), end = character(), reason = character(), author = character()))
  x[, run := cumsum(c(1L, diff(i) != 1L)), by = p]
  r <- x[, .(start = fmt_ts(times_all[min(i)]), end = fmt_ts(times_all[max(i)])), by = .(p, run)]
  r <- r[, .(parameter = paste(rx_escape(sort(unique(p))), collapse = "|")), by = .(start, end)][order(start, parameter)]
  data.table(station = station, stream = stream, parameter = r$parameter, start = r$start, end = r$end, reason = trim(reason), author = trim(author))
}

# A calibration row (CAL_COLS, a view of the corrections over a period): new value = value x multiplier + offset, between start and end, for the stations or
# loggers in applies_to; output blank changes the parameter in place.
wb_cal_row <- function(applies_to, params, start, end, multiplier, offset, output = "", notes = "")
  data.table(applies_to = trim(applies_to), parameter = paste(rx_escape(params), collapse = "|"), output = trim(output),
             multiplier = trim(as.character(multiplier)), offset = trim(as.character(offset)), start = trim(start), end = trim(end), notes = trim(notes))

# time_corrections.csv rows (TIME_COLS), one per logger table. start and end are the times as written in the raw file.
wb_clock_rows <- function(loggers, start, end, action, shift_minutes, source_file = "", reason = "", author = "")
  data.table(logger_id = loggers$logger_id, table = loggers$table, source_file = source_file, start = trim(start), end = trim(end), action = action,
             shift_minutes = if (identical(action, "shift")) { m <- suppressWarnings(as.numeric(shift_minutes)); if (is.na(m)) "" else format(round(m, 2), scientific = FALSE) } else "",
             reason = trim(reason), author = trim(author))

# A field_log.csv row (LOG_COLS).
wb_note_row <- function(station, stream, date, type, params, note, author)
  data.table(station = station, stream = stream, date = trim(date), type = type, parameter = paste(params, collapse = "|"), note = trim(note), author = trim(author))

# A station's own row for a quality rule: a copy of the rule that covers the parameter now, for this station only (a row
# with applies_to replaces the general row there; R/qc.R). A new rule_id is given on the next run.
wb_station_rule <- function(rule, station) {
  r <- as.data.frame(rule, stringsAsFactors = FALSE)[1, , drop = FALSE]
  r$applies_to <- station; if ("rule_id" %in% names(r)) r$rule_id <- ""
  if ("notes" %in% names(r)) { n <- trim(as.character(r$notes)); r$notes <- paste0("for ", station, if (!is.na(n) && nzchar(n)) paste0("; ", n) else "") }
  as.data.table(r)
}

# Logger tables behind the selected records, from the station metadata (one station table can hold several loggers).
wb_logger_tables <- function(meta, station, stream, loggers) {
  if (is.null(meta) || !nrow(meta)) return(data.table(logger_id = unique(loggers), table = ""))
  st <- station; sm <- stream
  x <- unique(meta[station == st & stream == sm & logger_id %in% loggers, .(logger_id, table)])
  if (!nrow(x)) data.table(logger_id = unique(loggers), table = "") else x
}


# ---- dialogs ----------------------------------------------------------------------------------------------------------
# ctx: what wb_server shares (pd, E: the table editors, part, dat, outs, sel, sel_info, open_table, clear_sel).
wb_edit_server <- function(input, output, session, ctx) {
  wb_undo_server(input, session)                                   # Undo after an action (core workbench.R)
  E <- ctx$E
  who <- reactiveVal("")                                   # initials, kept for the next dialog in this window
  done <- function(msg, id = NULL) { if (!is.null(id)) who(trim(input[[id]] %||% "")); removeModal(); ctx$clear_sel(); wb_undo_offer(session, msg) }
  need_sel <- function() { s <- ctx$sel(); if (is.null(s) || !length(s$t)) { showNotification("Select cells in the grid, or drag across the chart, first.", type = "warning"); NULL } else s }
  author_in <- function(id) textInput(id, "Initials", value = isolate(who()), width = "100%")
  span_of <- function(s) paste(format(min(s$t), "%Y-%m-%d %H:%M", tz = "UTC"), "\u2013", format(max(s$t), "%Y-%m-%d %H:%M", tz = "UTC"))
  head_of <- function(s) { p <- ctx$part(); div(class = "wb-p", style = "color:var(--wb-fg2)",
    paste0(p$station, " \u00b7 ", p$stream, " \u00b7 ", paste(unique(s$p), collapse = ", "), " \u00b7 ", span_of(s), " \u00b7 ", length(s$t), if (length(s$t) == 1) " value" else " values")) }

  # remove
  rm_rows <- reactive({ s <- ctx$sel(); req(s); p <- ctx$part()
    wb_removal_rows(ctx$dat()$TIMESTAMP, s$p, s$t, p$station, p$stream, input$wb_rm_reason %||% "", input$wb_rm_author %||% "") })
  observeEvent(input$wb_act_remove, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Remove values", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-form2", textInput("wb_rm_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_rm_author")),
      wb_sec("Rows for the corrections over a period"), uiOutput("wb_rm_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "The values are blanked on the next run; the old values stay in the audit trail and output/qc. Raw files are not changed."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_remove", "Write rows", class = "btn-primary")))) })
  output$wb_rm_preview <- renderUI(wb_preview(rm_rows()[, .(station, stream, parameter, start, end)]))
  observeEvent(input$wb_do_remove, { if (!nzchar(trim(input$wb_rm_reason %||% ""))) return(ovid_field_error("wb_rm_reason", "Give a reason; it goes in the audit trail."))
    r <- rm_rows(); req(nrow(r) > 0)
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d row(s) written to the corrections over a period. Run to apply.", nrow(r)), "wb_rm_author") })

  # restore (manual removals only; a value removed by a rule comes back when the rule is changed)
  rs_rows <- reactive({ s <- ctx$sel(); req(s); p <- ctx$part(); m <- E$manual$data()
    i <- wb_manual_rows_for(m, p$station, p$stream, s$p, s$t); list(i = i, rows = if (length(i)) as.data.table(m[i, , drop = FALSE]) else data.table()) })
  observeEvent(input$wb_act_restore, { s <- need_sel(); req(s, !VIEWER); r <- rs_rows()
    if (!length(r$i)) return(showNotification("No manual removal covers the selected values. A value removed by a quality rule comes back when the rule is changed: select it and open its rule.", type = "warning", duration = 8))
    showModal(modalDialog(title = "Restore values", size = "l", easyClose = TRUE, head_of(s),
      wb_sec(sprintf("%d row(s) of the corrections over a period cover them", length(r$i))), wb_preview(r$rows),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "Deleting a row brings back every value it removes, also outside the selection. The table's previous version is kept in config/history."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_restore", "Delete rows", class = "btn-danger")))) })
  observeEvent(input$wb_do_restore, { r <- rs_rows(); req(length(r$i))
    key <- function(d) do.call(paste, c(lapply(d, as.character), sep = "\r")); drop <- key(as.data.frame(r$rows))
    if (E$manual$mutate(function(d) d[!key(d) %in% drop, , drop = FALSE])) done(sprintf("%d row(s) deleted from the corrections over a period. Run to apply.", length(r$i))) })

  # calibrate
  cal_row <- reactive({ s <- ctx$sel(); req(s)
    wb_cal_row(input$wb_cal_applies %||% "", input$wb_cal_params %||% unique(s$p), input$wb_cal_start %||% "", input$wb_cal_end %||% "",
               input$wb_cal_mult %||% "1", input$wb_cal_off %||% "0", input$wb_cal_output %||% "", input$wb_cal_notes %||% "") })
  observeEvent(input$wb_act_cal, { s <- need_sel(); req(s, !VIEWER); p <- ctx$part()
    showModal(modalDialog(title = "Calibrate", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-form2", selectizeInput("wb_cal_params", "Parameters", choices = ctx$params_all(), selected = unique(s$p), multiple = TRUE, width = "100%"),
          textInput("wb_cal_applies", "Applies to (stations or loggers, | between)", p$station, width = "100%")),
      div(class = "wb-form4", textInput("wb_cal_mult", "Multiplier", "1"), textInput("wb_cal_off", "Offset", "0"),
          textInput("wb_cal_start", "Start", fmt_ts(min(s$t))), textInput("wb_cal_end", "End", fmt_ts(max(s$t)))),
      div(class = "wb-form2", textInput("wb_cal_output", "Output (blank: change in place)", "", width = "100%"), textInput("wb_cal_notes", "Notes", "", width = "100%")),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "New value = value \u00d7 multiplier + offset. Calibrations run before the quality rules."),
      wb_sec("Row for the corrections over a period"), uiOutput("wb_cal_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_cal", "Write row", class = "btn-primary")))) })
  output$wb_cal_preview <- renderUI(wb_preview(cal_row()))
  observeEvent(input$wb_do_cal, { r <- cal_row()
    if (!length(input$wb_cal_params)) return(ovid_field_error("wb_cal_params", "Pick at least one parameter."))
    if (anyNA(suppressWarnings(as.numeric(c(r$multiplier, r$offset))))) return(showNotification("Multiplier and offset must be numbers.", type = "warning"))
    if ((nzchar(r$start) && is.na(parse_time(r$start))) || (nzchar(r$end) && is.na(parse_time(r$end)))) return(showNotification("Start and end must look like 2019-06-30 or 2019-06-30 14:00.", type = "warning"))
    if (E$cal$append(as.data.frame(r))) done("Row written to the corrections over a period. Run to apply.") })

  # clock
  ck_loggers <- reactive({ s <- ctx$sel(); req(s); p <- ctx$part(); d <- ctx$dat()
    k <- match(as.numeric(unique(s$t)), as.numeric(d$TIMESTAMP)); k <- k[!is.na(k)]
    list(lt = wb_logger_tables(ctx$outs()$meta, p$station, p$stream, unique(as.character(d$logger_id[k]))),
         files = if ("source_file" %in% names(d)) unique(basename(as.character(d$source_file[k]))) else character()) })
  ck_rows <- reactive({ s <- ctx$sel(); req(s); x <- ck_loggers()
    wb_clock_rows(x$lt, input$wb_ck_start %||% "", input$wb_ck_end %||% "", input$wb_ck_action %||% "shift", input$wb_ck_min %||% "",
                  if (isTRUE(input$wb_ck_file) && length(x$files) == 1) sub("#A[0-9_f]+$", "", x$files) else "", input$wb_ck_reason %||% "", input$wb_ck_author %||% "") })
  observeEvent(input$wb_act_clock, { s <- need_sel(); req(s, !VIEWER); x <- ck_loggers(); o <- ctx$outs()
    moved <- if (!is.null(o) && nrow(o$index) && "rule" %in% names(o$index)) o$index[rule == "clock_fix_shifted" & logger_id %in% x$lt$logger_id] else data.table()
    rt <- range(s$t); if (nrow(x$lt) == 1) rt <- logger_time(rt, clock_shifts(ctx$pd(), x$lt$logger_id, x$lt$table, x$files), x$files)   # raw file times (R/stitch.R)
    showModal(modalDialog(title = "Clock correction", size = "l", easyClose = TRUE, head_of(s),
      if (nrow(moved)) div(class = "wb-warn", "Records of these loggers were already moved by a clock correction or a time-zone row. ", if (nrow(x$lt) == 1) "Start and end below are set back to the times as written in the raw file; check them."
        else "Start and end below are the times in the station table; set them to the times as written in the raw file."),
      div(class = "wb-form4", selectInput("wb_ck_action", "Action", c("Shift the timestamps" = "shift", "Remove the records" = "remove")),
          textInput("wb_ck_min", "Shift (minutes)", ""), textInput("wb_ck_start", "Start (raw file time)", fmt_ts(rt[1])), textInput("wb_ck_end", "End (raw file time)", fmt_ts(rt[2]))),
      if (length(x$files) == 1) checkboxInput("wb_ck_file", paste("Only file", x$files), FALSE, width = "100%"),
      div(class = "wb-form2", textInput("wb_ck_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_ck_author")),
      wb_sec("Rows for time_corrections.csv"), uiOutput("wb_ck_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "A clock correction applies to the logger's records before they are stitched, so it changes every parameter of those records."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_clock", "Write rows", class = "btn-primary")))) })
  output$wb_ck_preview <- renderUI(wb_preview(ck_rows()))
  observeEvent(input$wb_do_clock, { r <- ck_rows()
    if (!nzchar(trim(input$wb_ck_reason %||% ""))) return(ovid_field_error("wb_ck_reason", "Give a reason; it goes in the audit trail."))
    if (identical(input$wb_ck_action, "shift") && (is.na(suppressWarnings(as.numeric(input$wb_ck_min))) || as.numeric(input$wb_ck_min) == 0))
      return(showNotification("Say how many minutes to move the timestamps.", type = "warning"))
    if ((nzchar(r$start[1]) && is.na(parse_time(r$start[1]))) || (nzchar(r$end[1]) && is.na(parse_time(r$end[1])))) return(showNotification("Start and end must look like 2019-06-30 or 2019-06-30 14:00.", type = "warning"))
    if (E$timecorr$append(as.data.frame(r))) done(sprintf("%d row(s) written to time_corrections.csv. Run to apply.", nrow(r)), "wb_ck_author") })

  # field note
  note_row <- reactive({ s <- ctx$sel(); req(s); p <- ctx$part()
    wb_note_row(p$station, if (isTRUE(input$wb_nt_table)) p$stream else "", input$wb_nt_date %||% "", input$wb_nt_type %||% "note", unique(s$p), input$wb_nt_note %||% "", input$wb_nt_author %||% "") })
  observeEvent(input$wb_act_note, { s <- need_sel(); req(s, !VIEWER); p <- ctx$part()
    showModal(modalDialog(title = "Field log entry", size = "m", easyClose = TRUE, head_of(s),
      div(class = "wb-form2", textInput("wb_nt_date", "Date (and time)", format(min(s$t), "%Y-%m-%d %H:%M", tz = "UTC"), width = "100%"), selectInput("wb_nt_type", "What", LOG_TYPES, width = "100%")),
      textInput("wb_nt_note", "Note", width = "100%"), author_in("wb_nt_author"),
      checkboxInput("wb_nt_table", paste("Only this table,", p$stream), FALSE, width = "100%"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "Log entries are marked on the plots. An entry of type gauge serviced is also a servicing time for the station's precipitation gauge."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_note", "Add entry", class = "btn-primary")))) })
  observeEvent(input$wb_do_note, { r <- note_row()
    if (is.na(parse_time(r$date))) return(ovid_field_error("wb_nt_date", "Give a date such as 2019-06-30 or 2019-06-30 14:00.")); if (!nzchar(r$note)) return(ovid_field_error("wb_nt_note", "Write the note."))
    if (E$fieldlog$append(as.data.frame(r))) done("Entry added to field_log.csv.", "wb_nt_author") })

  # rules
  observeEvent(input$wb_rule_open, { req(nzchar(input$wb_rule_open)); d <- E$qc$data(); req(d)
    i <- which(as.character(d$rule_id) == input$wb_rule_open); ctx$open_table("qc"); if (length(i)) E$qc$select(i[1]) })
  observeEvent(input$wb_rule_station, { req(!VIEWER); info <- ctx$sel_info(); req(info$qc_rule)
    r <- wb_station_rule(info$qc_rule, ctx$part()$station)
    if (E$qc$append(as.data.frame(r[, intersect(names(E$qc$data()), names(r)), with = FALSE]))) {
      ctx$open_table("qc"); E$qc$select(nrow(E$qc$data()))
      showNotification(paste0("Row for ", ctx$part()$station, " added at the end of qc_rules.csv. Change its limits there, save, and run."), type = "message", duration = 8) } })
}
