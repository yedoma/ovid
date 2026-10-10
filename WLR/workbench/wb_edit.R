# WLR/workbench/wb_edit.R  --  Corrections from the Workbench: the rows it writes and the dialogs that write them.
# Part of Ovid Core // WLR. Sourced by wb_wlr.R. Every correction is a row in a set-up table, written through the same
# table editors as the Standard interface (Ovid/editor.R: history in config/history, saves from the other interface
# noticed), with the same columns as Plot and explore writes; the pipeline applies it on the next run. Raw files and the
# metadata workbook are never written.

WB_VALS <- c("pres_kpa", "depth_m", "elev_m", "temp_c", "spc_uscm")        # what a removal with a blank parameter takes (R/qc.R)
WB_OOW <- c("pres_kpa", "depth_m", "elev_m")                               # what an out-of-water period takes (R/qc.R)

# ---- rows -------------------------------------------------------------------------------------------------------------
# The rows of a manual removals or out-of-water table that apply to a logger (serial) at a site, with start and end as
# times (a and b) and their row number in the table (i), as the pipeline reads them (span_rows() in R/qc.R): the serial or
# the site matches; a blank start or end is open; an end given as a date covers that whole day. Rows it would skip are left out.
wb_spans <- function(tab, serial, site) {
  if (is.null(tab) || !NROW(tab)) return(NULL)
  tab <- as.data.table(tab); hit <- which(wlr_row_hits(tab, serial, site %||% ""))      # serial|site lists and "*" as well (R/qc.R)
  if (!length(hit)) return(NULL)
  r <- tab[hit][, i := hit]
  r[, `:=`(a = parse_date_cell(start), b = parse_date_cell(end, end = TRUE))]
  r <- r[!((nzchar(start) & is.na(a)) | (nzchar(end) & is.na(b)))]
  r[!nzchar(start), a := as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")][!nzchar(end), b := as.POSIXct(Inf, origin = "1970-01-01", tz = "UTC")][]
}
# The parameters a manual removal row takes ("records": every one), and through which of a record's values parameter p
# goes: itself, or the water pressure for the values worked out from it.
wb_takes <- function(parameter) { pr <- trim(parameter %||% "")
  if (tolower(pr) %in% c("records", "<records>", "<record>")) return("*")
  if (nzchar(pr)) trim(strsplit(pr, "|", fixed = TRUE)[[1]]) else WB_VALS }
wb_via <- function(p, cols) if ("*" %in% cols || p %in% cols) p else if (p %in% WB_DERIVED && "pres_kpa" %in% cols) "pres_kpa" else NA_character_

# TRUE for each record of w whose value of parameter p a saved manual removal or out-of-water period takes (its value
# band checked on the value it applies to).
wb_cover <- function(manual, oow, serial, site, w, p) {
  out <- rep(FALSE, nrow(w)); t <- w$TIMESTAMP
  m <- wb_spans(manual, serial, site)
  for (k in seq_len(NROW(m))) { hit <- t >= m$a[k] & t <= m$b[k]; if (!any(hit)) next
    cols <- wb_takes(m$parameter[k]); via <- wb_via(p, cols); if (is.na(via)) next
    if (!"*" %in% cols && via %in% names(w)) { v <- w[[via]]
      lo <- suppressWarnings(as.numeric(m$min_value[k] %||% "")); hi <- suppressWarnings(as.numeric(m$max_value[k] %||% ""))
      if (!is.na(lo)) hit <- hit & !is.na(v) & v >= lo; if (!is.na(hi)) hit <- hit & !is.na(v) & v <= hi }
    out <- out | hit }
  if (p %in% c(WB_OOW, WB_DERIVED)) { o <- wb_spans(oow, serial, site); for (k in seq_len(NROW(o))) out <- out | (t >= o$a[k] & t <= o$b[k]) }
  out
}
# The rows of a table (manual removals, or out-of-water periods with oow = TRUE) that take any of the values (p, t).
wb_covering <- function(tab, serial, site, p, t, oow = FALSE) {
  r <- wb_spans(tab, serial, site); if (is.null(r)) return(integer())
  r$i[vapply(seq_len(nrow(r)), function(k) { in_t <- t >= r$a[k] & t <= r$b[k]; if (!any(in_t)) return(FALSE)
    if (oow) return(any(p[in_t] %in% c(WB_OOW, WB_DERIVED)))
    cols <- wb_takes(r$parameter[k]); any(!is.na(vapply(p[in_t], wb_via, "", cols = cols))) }, NA)]
}

# Removal rows (MANUAL_COLS, a view of the corrections over a period) that remove exactly the selected values: for each logger, one row per run of
# consecutive records, the parameters that share a run in one row. A value worked out from the water pressure (depth,
# elevation, depth to water, water minus baro) is removed at its source, the pressure, as on Plot and explore; parameter
# "records" drops whole records. times_all: every record time of the table, so runs are consecutive in the table and not
# just in the rows shown.
wb_removal_rows <- function(times_all, p, t, serial, site, reason, author, parameter = NULL) {
  pp <- if (!is.null(parameter)) rep(parameter, length(p)) else ifelse(p %in% WB_DERIVED, "pres_kpa", p)
  x <- unique(data.table(p = pp, s = as.character(serial), i = match(as.numeric(t), as.numeric(times_all))))[!is.na(i)][order(s, p, i)]
  if (!nrow(x)) return(empty_cfg(MANUAL_COLS))
  x[, run := cumsum(c(1L, diff(i) != 1L)), by = .(s, p)]
  r <- x[, .(start = fmt_ts(times_all[min(i)]), end = fmt_ts(times_all[max(i)])), by = .(s, p, run)]
  r <- r[, .(parameter = paste(sort(unique(p)), collapse = "|")), by = .(s, start, end)][order(s, start, parameter)]
  data.table(serial = r$s, site = site, start = r$start, end = r$end, parameter = r$parameter, min_value = "", max_value = "", reason = trim(reason), author = trim(author))
}
# Rows for out_of_water.csv (OOW_COLS): one per logger and run of consecutive selected records.
wb_oow_rows <- function(times_all, t, serial, site, cause, reason, author) {
  r <- wb_removal_rows(times_all, rep("x", length(t)), t, serial, site, reason, author, parameter = "x")
  data.table(serial = r$serial, site = r$site, start = r$start, end = r$end, cause = cause, reason = r$reason, author = r$author)
}
# A time_corrections.csv row (TIME_COLS): shift the records of serial between start and end (times as in the record).
# mode drift spreads the shift over the span (none at its start, all of it at its end).
wb_clock_row <- function(serial, start, end, minutes, reason, author, mode = "shift")
  data.table(serial = serial, start = trim(start), end = trim(end), shift_minutes = { m <- suppressWarnings(as.numeric(minutes)); if (is.na(m)) "" else format(round(m, 2), scientific = FALSE) },
             reason = trim(reason), author = trim(author), mode = if (identical(mode, "drift")) "drift" else "")
# A deployments.csv row (DEPLOY_OVERRIDE_COLS) that sets when the logger went in (deployed) or came out (removed).
wb_dates_row <- function(serial, year, what, when, reason, author) {
  r <- data.table(serial = serial, year = as.character(year), site = "", deployed = "", removed = "", T_ST_m = "", T_ST_from_year = "", reference = "", measured_to = "",
                  reason = trim(reason), author = trim(author))
  set(r, j = what, value = fmt_ts(when)); r
}
# A manual_measurements.csv row (MEAS_COLS).
wb_meas_row <- function(site, time, parameter, value, method, author, notes)
  data.table(site = site, time = format(time, "%Y-%m-%d %H:%M", tz = "UTC"), parameter = parameter, value = trim(as.character(value)), method = trim(method), author = trim(author), notes = trim(notes))

# ---- dialogs ----------------------------------------------------------------------------------------------------------
# ctx: what wb_server shares (pd, E: the table editors, part, dat, outs, sel, grid, open_table, clear_sel).
wb_edit_server <- function(input, output, session, ctx) {
  wb_undo_server(input, session)                                   # Undo after an action (core workbench.R)
  E <- ctx$E
  who <- reactiveVal("")                                   # initials, kept for the next dialog in this window
  done <- function(msg, id = NULL) { if (!is.null(id)) who(trim(input[[id]] %||% "")); removeModal(); ctx$clear_sel(); wb_undo_offer(session, msg) }
  need_sel <- function() { s <- ctx$sel(); if (is.null(s) || !length(s$t)) { showNotification("Select cells in the grid, or drag across the chart, first.", type = "warning"); NULL } else s }
  author_in <- function(id) textInput(id, "Initials", value = isolate(who()), width = "100%")
  span_of <- function(s) paste(format(min(s$t), "%Y-%m-%d %H:%M", tz = "UTC"), "\u2013", format(max(s$t), "%Y-%m-%d %H:%M", tz = "UTC"))
  head_of <- function(s) { p <- ctx$part(); div(class = "wb-p", style = "color:var(--wb-fg2)",
    paste0(p$label, " \u00b7 ", paste(unique(s$p), collapse = ", "), " \u00b7 ", span_of(s), " \u00b7 ", length(s$t), if (length(s$t) == 1) " value" else " values")) }
  site_of <- function() { p <- ctx$part(); if (identical(p$kind, "baro")) "" else p$site %||% "" }   # a barologger's rows name its serial only
  times_all <- function() ctx$dat()$TIMESTAMP
  why_ok <- function(id) if (!nzchar(trim(input[[id]] %||% ""))) { ovid_field_error(id, "Give a reason; it goes in the audit trail."); FALSE } else TRUE
  note_derived <- function(s) if (any(s$p %in% WB_DERIVED)) div(class = "wb-warn", "Depth, elevation, depth to water and water minus baro are worked out from the water pressure, so they are removed at their source: the pressure at those times (as Plot and explore does).")

  # remove
  rm_rows <- reactive({ s <- ctx$sel(); req(s); wb_removal_rows(times_all(), s$p, s$t, s$serial, site_of(), input$wb_rm_reason %||% "", input$wb_rm_author %||% "") })
  observeEvent(input$wb_act_remove, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Remove values", size = "l", easyClose = TRUE, head_of(s), note_derived(s),
      div(class = "wb-form2", textInput("wb_rm_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_rm_author")),
      wb_sec("Rows for the corrections over a period"), uiOutput("wb_rm_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "The values are blanked on the next run; the old values stay in the audit trail. Raw files are not changed."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_remove", "Write rows", class = "btn-primary")))) })
  output$wb_rm_preview <- renderUI(wb_preview(rm_rows()[, .(serial, site, parameter, start, end)]))
  observeEvent(input$wb_do_remove, { if (!why_ok("wb_rm_reason")) return(); r <- rm_rows(); req(nrow(r))
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d row(s) written to the corrections over a period. Run to apply.", nrow(r)), "wb_rm_author") })

  # drop whole records
  dr_rows <- reactive({ s <- ctx$sel(); req(s); wb_removal_rows(times_all(), s$p, s$t, s$serial, site_of(), input$wb_dr_reason %||% "", input$wb_dr_author %||% "", parameter = "records") })
  observeEvent(input$wb_act_drop, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Drop records", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-p", "Every value of the selected records is dropped (a logger launched before it went into the water, a bad stretch), whichever parameters are selected."),
      div(class = "wb-form2", textInput("wb_dr_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_dr_author")),
      wb_sec("Rows for the corrections over a period"), uiOutput("wb_dr_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_drop", "Write rows", class = "btn-primary")))) })
  output$wb_dr_preview <- renderUI(wb_preview(dr_rows()[, .(serial, site, parameter, start, end)]))
  observeEvent(input$wb_do_drop, { if (!why_ok("wb_dr_reason")) return(); r <- dr_rows(); req(nrow(r))
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d row(s) written to the corrections over a period. Run to apply.", nrow(r)), "wb_dr_author") })

  # out of the water
  ow_rows <- reactive({ s <- ctx$sel(); req(s); wb_oow_rows(times_all(), s$t, s$serial, site_of(), input$wb_ow_cause %||% "retrieval", input$wb_ow_reason %||% "", input$wb_ow_author %||% "") })
  observeEvent(input$wb_act_oow, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Out of the water", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-p", "Pressure, depth and elevation are removed for the selected records (a retrieval, a download or a data error)."),
      div(class = "wb-form4", selectInput("wb_ow_cause", "Why", c("retrieval", "download", "launched before deployment", "data error", "other")),
          div(style = "grid-column:span 2", textInput("wb_ow_reason", "Reason (goes in the audit trail)", width = "100%")), author_in("wb_ow_author")),
      wb_sec("Rows for out_of_water.csv"), uiOutput("wb_ow_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_oow", "Write rows", class = "btn-primary")))) })
  output$wb_ow_preview <- renderUI(wb_preview(ow_rows()[, .(serial, site, start, end, cause)]))
  observeEvent(input$wb_do_oow, { if (!why_ok("wb_ow_reason")) return(); r <- ow_rows(); req(nrow(r))
    if (E$oow$append(as.data.frame(r))) done(sprintf("%d row(s) written to out_of_water.csv. Run to apply.", nrow(r)), "wb_ow_author") })

  # restore (manual removals and out-of-water periods only; a value removed by a quality rule comes back when the rule is changed)
  rs_rows <- reactive({ s <- ctx$sel(); req(s); p0 <- ctx$part()
    i <- unique(unlist(lapply(unique(s$serial), function(sn) { k <- s$serial == sn; wb_covering(E$manual$data(), sn, p0$site, s$p[k], s$t[k]) })))
    j <- unique(unlist(lapply(unique(s$serial), function(sn) { k <- s$serial == sn; wb_covering(E$oow$data(), sn, p0$site, s$p[k], s$t[k], oow = TRUE) })))
    list(i = sort(i), j = sort(j), manual = if (length(i)) as.data.table(E$manual$data()[sort(i), , drop = FALSE]) else data.table(),
         oow = if (length(j)) as.data.table(E$oow$data()[sort(j), , drop = FALSE]) else data.table()) })
  observeEvent(input$wb_act_restore, { s <- need_sel(); req(s, !VIEWER); r <- rs_rows()
    if (!length(r$i) && !length(r$j)) return(showNotification("No manual removal or out-of-water period covers the selected values. A value removed by a quality rule comes back when the rule is changed: open the quality rules.", type = "warning", duration = 8))
    showModal(modalDialog(title = "Restore values", size = "l", easyClose = TRUE, head_of(s),
      if (length(r$i)) tagList(wb_sec(sprintf("%d row(s) of the corrections over a period cover them", length(r$i))), wb_preview(r$manual)),
      if (length(r$j)) tagList(wb_sec(sprintf("%d row(s) of out_of_water.csv cover them", length(r$j))), wb_preview(r$oow)),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "Deleting a row brings back every value it removes, also outside the selection. The tables' previous versions are kept in config/history."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_restore", "Delete rows", class = "btn-danger")))) })
  observeEvent(input$wb_do_restore, { r <- rs_rows(); req(length(r$i) || length(r$j))
    key <- function(d) do.call(paste, c(lapply(d, as.character), sep = "\r"))
    ok1 <- !length(r$i) || E$manual$mutate(function(d) d[!key(d) %in% key(as.data.frame(r$manual)), , drop = FALSE])
    ok2 <- !length(r$j) || E$oow$mutate(function(d) d[!key(d) %in% key(as.data.frame(r$oow)), , drop = FALSE])
    if (isTRUE(ok1) && isTRUE(ok2)) done(sprintf("%d row(s) deleted. Run to apply.", length(r$i) + length(r$j))) })

  # clock
  ck_row <- reactive({ s <- ctx$sel(); req(s)
    wb_clock_row(input$wb_ck_serial %||% s$serial[1], input$wb_ck_start %||% "", input$wb_ck_end %||% "", input$wb_ck_min %||% "", input$wb_ck_reason %||% "", input$wb_ck_author %||% "", input$wb_ck_mode %||% "shift") })
  observeEvent(input$wb_act_clock, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Clock correction", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-form4", selectInput("wb_ck_serial", "Logger", unique(s$serial)), textInput("wb_ck_min", "Shift (minutes)", ""),
          textInput("wb_ck_start", "Start", fmt_ts(min(s$t))), textInput("wb_ck_end", "End", fmt_ts(max(s$t)))),
      radioButtons("wb_ck_mode", NULL, c("Move the whole span by the shift" = "shift", "Clock drift: spread the shift over the span (none at the start, all of it at the end)" = "drift"), "shift", width = "100%"),
      div(class = "wb-form2", textInput("wb_ck_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_ck_author")),
      wb_sec("Row for time_corrections.csv"), uiOutput("wb_ck_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "The shift is applied to the logger's stitched record before it is split into deployments, so it moves every value of those records. Start and end are times as in this record (after its time zone)."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_clock", "Write row", class = "btn-primary")))) })
  output$wb_ck_preview <- renderUI(wb_preview(ck_row()))
  observeEvent(input$wb_do_clock, { r <- ck_row(); if (!why_ok("wb_ck_reason")) return()
    if (!nzchar(r$shift_minutes) || as.numeric(r$shift_minutes) == 0) return(showNotification("Say how many minutes to move the timestamps.", type = "warning"))
    if ((nzchar(r$start) && is.na(parse_date_cell(r$start))) || (nzchar(r$end) && is.na(parse_date_cell(r$end)))) return(showNotification("Start and end must look like 2019-06-30 or 2019-06-30 14:00.", type = "warning"))
    if (E$time$append(as.data.frame(r))) done("Row written to time_corrections.csv. Run to apply.", "wb_ck_author") })

  # deployment dates: the logger went in after the selected records, or came out at the first of them
  dt_row <- reactive({ s <- ctx$sel(); req(s); p0 <- ctx$part(); r <- p0$row; req(nrow(r))
    after <- function(t) { a <- times_all(); k <- a[a > max(t)]; if (length(k)) min(k) else max(t) + 1 }
    if (identical(input$wb_dt_what %||% "deployed", "deployed")) wb_dates_row(r$serial[1], r$year[1], "deployed", after(s$t), input$wb_dt_reason %||% "", input$wb_dt_author %||% "")
    else wb_dates_row(r$serial[1], r$year[1], "removed", min(s$t), input$wb_dt_reason %||% "", input$wb_dt_author %||% "") })
  observeEvent(input$wb_act_dates, { s <- need_sel(); req(s, !VIEWER); p0 <- ctx$part()
    if (!identical(p0$kind, "dep")) return(showNotification("Deployment dates are set on a deployment, not a barologger.", type = "warning"))
    showModal(modalDialog(title = "Deployment dates", size = "l", easyClose = TRUE, head_of(s),
      radioButtons("wb_dt_what", NULL, c("The logger went into the water after the selected records (everything up to them is left out)" = "deployed",
                                         "The logger came out of the water at the first selected record (it and everything after are left out)" = "removed"), "deployed", width = "100%"),
      div(class = "wb-form2", textInput("wb_dt_reason", "Reason", width = "100%"), author_in("wb_dt_author")),
      wb_sec("Row for deployments.csv (Deployment history)"), uiOutput("wb_dt_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "The workbook is not changed: the row corrects it, as on Deployment history."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_dates", "Write row", class = "btn-primary")))) })
  output$wb_dt_preview <- renderUI(wb_preview(dt_row()[, .(serial, year, deployed, removed, reason)]))
  observeEvent(input$wb_do_dates, { if (!why_ok("wb_dt_reason")) return(); r <- dt_row()
    if (E$deps$append(as.data.frame(r))) done("Row written to deployments.csv. Run to apply.", "wb_dt_author") })

  # a manual measurement at the selected time
  ms_row <- reactive({ s <- ctx$sel(); req(s)
    wb_meas_row(site_of(), s$t[1], input$wb_ms_param %||% "depth_m", input$wb_ms_value %||% "", input$wb_ms_method %||% "", input$wb_ms_author %||% "", input$wb_ms_notes %||% "") })
  observeEvent(input$wb_act_meas, { s <- need_sel(); req(s, !VIEWER)
    if (!identical(ctx$part()$kind, "dep")) return(showNotification("Measurements belong to a site: pick a deployment.", type = "warning"))
    showModal(modalDialog(title = "Manual measurement", size = "m", easyClose = TRUE, head_of(s),
      div(class = "wb-p", "A reading taken by hand at this time (a tape, a thermometer, a conductivity meter). It is shown beside the logger and never changes the data."),
      div(class = "wb-form2", selectInput("wb_ms_param", "What", c("Water above the sensor (m)" = "depth_m", "Depth to water below the reference (m)" = "dtw_m", "Water elevation (m)" = "elev_m",
                                                                   "Temperature (\u00b0C)" = "temp_c", "Specific conductance (\u00b5S/cm)" = "spc_uscm"), if (s$p[1] %in% MEAS_PARAMS) s$p[1] else "depth_m"),
          textInput("wb_ms_value", "Value")),
      div(class = "wb-form2", textInput("wb_ms_method", "Method", "tape"), author_in("wb_ms_author")), textInput("wb_ms_notes", "Notes", width = "100%"),
      wb_sec("Row for manual_measurements.csv"), uiOutput("wb_ms_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("wb_do_meas", "Add row", class = "btn-primary")))) })
  output$wb_ms_preview <- renderUI(wb_preview(ms_row()))
  observeEvent(input$wb_do_meas, { r <- ms_row()
    if (is.na(suppressWarnings(as.numeric(r$value)))) return(showNotification("Type the value as a number.", type = "warning"))
    if (E$meas$append(as.data.frame(r))) done("Row added to manual_measurements.csv.", "wb_ms_author") })

  # the quality rules of the selected parameter, opened on Quality rules with their rows selected
  observeEvent(input$wb_rule_open, { s <- ctx$sel(); d <- E$qc$data(); req(d); ctx$open_table("qc")
    p <- if (!is.null(s)) unique(s$p) else character(); p <- unique(c(p, if (any(p %in% WB_DERIVED)) "pres_kpa"))
    i <- which(as.character(d$parameter) %in% p | !nzchar(as.character(d$parameter))); if (length(i)) E$qc$select(i) })
}
