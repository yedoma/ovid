# GT/workbench/wb_edit.R  --  Corrections from the Workbench: the rows it writes and the dialogs that write them.
# Part of Ovid Core // GT. Sourced by wb_gt.R. Every correction is a row in a set-up table, written through the same
# table editors as the Standard interface (Ovid/editor.R: history in config/history, saves from the other interface
# noticed), with the same columns as Plot and explore writes; the pipeline applies it on the next run. Raw files are
# never written.

# ---- rows -------------------------------------------------------------------------------------------------------------
# TRUE for each record of w whose value in column p (variable, depth) a saved manual removal takes, as apply_qc() (R/qc.R)
# reads the table: its target names this site, this value's logger or sensor (or is blank), its start and end hold the time
# (an end date covers that whole day), and its sensor, depth, variable and value band match where they are filled in.
# Rows with action flag keep the value and are left out. sgn: the project's depth convention, as the table's depths are typed.
wb_cover <- function(manual, site, w, p, variable, depth, sgn = 1) {
  out <- rep(FALSE, nrow(w)); if (is.null(manual) || !NROW(manual)) return(out)
  m <- as.data.table(manual); t <- w$TIMESTAMP; v <- w[[p]]
  ser <- as.character(w[[paste0("serial|", p)]]); sen <- as.character(w[[paste0("sensor|", p)]])
  split <- function(x) trim(strsplit(x, "|", fixed = TRUE)[[1]])
  for (i in seq_len(nrow(m))) { r <- m[i]; g <- function(k) { x <- r[[k]] %||% ""; if (is.na(x)) "" else trim(as.character(x)) }
    if (tolower(g("action")) == "flag") next
    tg <- period_parts(g("site")); kt <- rep(TRUE, length(t))                       # the target: sites, serials or sensors (blank or *: all)
    if (length(tg) && !"*" %in% tg && !site %in% tg) { kt <- (!is.na(ser) & norm_serial(ser) %in% norm_serial(tg)) | (!is.na(sen) & sen %in% tg); if (!any(kt)) next }
    if (nzchar(g("variable")) && g("variable") != "*" && !any(c(variable, p) %in% split(g("variable")))) next
    if (nzchar(g("depth_m")) && !round(depth, 3) %in% round(typed_depth(split(g("depth_m")), sgn), 3)) next
    a <- parse_date_cell(g("start")); b <- parse_date_cell(g("end"), end = TRUE)
    k <- kt & (is.na(a) | t >= a) & (is.na(b) | t <= b)
    if (nzchar(g("serial"))) k <- k & !is.na(ser) & norm_serial(ser) == norm_serial(g("serial"))
    if (nzchar(g("sensor"))) k <- k & !is.na(sen) & sen %in% split(g("sensor"))
    lo <- suppressWarnings(as.numeric(g("min_value"))); hi <- suppressWarnings(as.numeric(g("max_value")))
    if (!is.na(lo)) k <- k & !is.na(v) & v >= lo; if (!is.na(hi)) k <- k & !is.na(v) & v <= hi
    out <- out | k }
  out
}
# The removal rows (remove or flag, a view of the corrections over a period) that take any of the selected values (one value per element of s).
wb_covering <- function(manual, site, s, w, sgn = 1) {
  if (is.null(manual) || !NROW(manual)) return(integer())
  m <- as.data.table(manual); keep <- integer()
  for (i in seq_len(nrow(m))) { one <- m[i]; if (tolower(trim(one$action %||% "")) == "flag") one$action <- "remove"     # a flag is restored the same way
    hit <- FALSE
    for (p in unique(s$p)) { k <- which(s$p == p); rows <- match(as.numeric(s$t[k]), as.numeric(w$TIMESTAMP)); rows <- rows[!is.na(rows)]
      if (!length(rows)) next; ww <- w[rows]; setattr(ww, "where", attr(w, "where"))
      if (any(wb_cover(one, site, ww, p, s$variable[k[1]], s$depth[k[1]], sgn))) { hit <- TRUE; break } }
    if (hit) keep <- c(keep, i) }
  keep
}

# Removal rows (MANUAL_COLS, a view of the corrections over a period) for the selected values, as Plot and explore writes them: the site, the depth
# and the variable, one row per run of consecutive records; depths that share a run go in one row (depth_m "0.1|0.2").
# action: remove or flag. times_all: every record time of the site's table, so runs are consecutive in the table. sgn: the
# project's depth convention (depth_sign()), so the depths are written as the table is read.
wb_removal_rows <- function(times_all, t, depth, variable, site, reason, author, action = "remove", sgn = 1) {
  x <- unique(data.table(z = depth, v = variable, i = match(as.numeric(t), as.numeric(times_all))))[!is.na(i)][order(v, z, i)]
  if (!nrow(x)) return(empty_cfg(MANUAL_COLS))
  x[, run := cumsum(c(1L, diff(i) != 1L)), by = .(v, z)]
  r <- x[, .(start = fmt_ts(times_all[min(i)]), end = fmt_ts(times_all[max(i)])), by = .(v, z, run)]
  r <- r[, .(depth_m = paste(format(depth_typed(sort(unique(z)), sgn)), collapse = "|")), by = .(v, start, end)][order(v, start, depth_m)]
  data.table(site = site, serial = "", sensor = "", depth_m = r$depth_m, variable = r$v, start = r$start, end = r$end, min_value = "", max_value = "",
             action = action, reason = trim(reason), author = trim(author))
}
# A row that trims the record: it starts after the selected records (start), or ends at the first of them (end), every
# depth, as Plot and explore's Record starts / Record ends.
wb_trim_row <- function(times_all, t, what, site, variable, reason, author) {
  if (identical(what, "start")) { a <- min(times_all) - 1; b <- max(t) } else { a <- min(t); b <- max(times_all) + 1 }
  data.table(site = site, serial = "", sensor = "", depth_m = "", variable = variable, start = fmt_ts(a), end = fmt_ts(b), min_value = "", max_value = "",
             action = "remove", reason = trim(reason), author = trim(author))
}
# A time_corrections.csv row (TIME_COLS): shift one logger's records between start and end (times as in the record).
wb_clock_row <- function(serial, start, end, minutes, reason)
  data.table(serial = serial, start = trim(start), end = trim(end), shift_minutes = { m <- suppressWarnings(as.numeric(minutes)); if (is.na(m)) "" else format(round(m, 2), scientific = FALSE) },
             reason = trim(reason))

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
  head_of <- function(s) div(class = "wb-p", style = "color:var(--wb-fg2)",
    paste0(ctx$part()$label, " \u00b7 ", paste(unique(s$p), collapse = ", "), " \u00b7 ", span_of(s), " \u00b7 ", length(s$t), if (length(s$t) == 1) " value" else " values"))
  times_all <- function() ctx$dat()$TIMESTAMP
  why_ok <- function(id) if (!nzchar(trim(input[[id]] %||% ""))) { ovid_field_error(id, "Give a reason; it goes in the audit trail."); FALSE } else TRUE

  # remove, or flag
  rm_rows <- function(action, rid, aid) { s <- ctx$sel(); req(s); wb_removal_rows(times_all(), s$t, s$depth, s$variable, ctx$part()$site, input[[rid]] %||% "", input[[aid]] %||% "", action,
                                                             ctx$wsgn()) }
  dialog <- function(title, s, what, prefix, button) showModal(modalDialog(title = title, size = "l", easyClose = TRUE, head_of(s), what,
    div(class = "wb-form2", textInput(paste0(prefix, "_reason"), "Reason (goes in the audit trail)", value = isolate(input$wb_dec_reason %||% ""), width = "100%"), author_in(paste0(prefix, "_author"))),
    wb_sec("Rows for the corrections over a period"), uiOutput(paste0(prefix, "_preview")),
    footer = tagList(modalButton("Cancel"), actionButton(paste0(prefix, "_do"), button, class = "btn-primary"))))
  observeEvent(input$wb_act_remove, { s <- need_sel(); req(s, !VIEWER)
    dialog("Remove values", s, div(class = "wb-p", style = "color:var(--wb-fg2)", "The values are removed on the next run; the old values stay in the audit trail. Raw files are not changed."), "wb_rm", "Write rows") })
  output$wb_rm_preview <- renderUI(wb_preview(rm_rows("remove", "wb_rm_reason", "wb_rm_author")[, .(site, depth_m, variable, start, end, action)]))
  observeEvent(input$wb_rm_do, { if (!why_ok("wb_rm_reason")) return(); r <- rm_rows("remove", "wb_rm_reason", "wb_rm_author"); req(nrow(r) > 0)
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d row(s) written to the corrections over a period. Run to apply.", nrow(r)), "wb_rm_author") })
  observeEvent(input$wb_act_flag, { s <- need_sel(); req(s, !VIEWER)
    dialog("Flag values", s, div(class = "wb-p", style = "color:var(--wb-fg2)", "The values are kept and flagged (manual_flag) on the next run, as a suspect reading."), "wb_fl", "Write rows") })
  output$wb_fl_preview <- renderUI(wb_preview(rm_rows("flag", "wb_fl_reason", "wb_fl_author")[, .(site, depth_m, variable, start, end, action)]))
  observeEvent(input$wb_fl_do, { if (!why_ok("wb_fl_reason")) return(); r <- rm_rows("flag", "wb_fl_reason", "wb_fl_author"); req(nrow(r) > 0)
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d row(s) written to the corrections over a period. Run to apply.", nrow(r)), "wb_fl_author") })

  # trim the start or the end of the record
  tr_row <- reactive({ s <- ctx$sel(); req(s); p0 <- ctx$part()
    wb_trim_row(times_all(), s$t, input$wb_tr_what %||% "start", p0$site, if (identical(p0$kind, "temp")) "temp" else "vwc|ec", input$wb_tr_reason %||% "", input$wb_tr_author %||% "") })
  observeEvent(input$wb_act_trim, { s <- need_sel(); req(s, !VIEWER)
    showModal(modalDialog(title = "Trim the record", size = "l", easyClose = TRUE, head_of(s),
      radioButtons("wb_tr_what", NULL, c("The record starts after the selected records (the string was not in the ground yet)" = "start",
                                         "The record ends at the first selected record (the logger came out of the ground)" = "end"), "start", width = "100%"),
      div(class = "wb-form2", textInput("wb_tr_reason", "Reason (goes in the audit trail)", width = "100%"), author_in("wb_tr_author")),
      wb_sec("Row for the corrections over a period"), uiOutput("wb_tr_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "Every depth of the site is removed before the start or after the end, as Record starts / Record ends on Plot and explore."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_tr_do", "Write row", class = "btn-primary")))) })
  output$wb_tr_preview <- renderUI(wb_preview(tr_row()[, .(site, variable, start, end, action)]))
  observeEvent(input$wb_tr_do, { if (!why_ok("wb_tr_reason")) return(); r <- tr_row()
    if (E$manual$append(as.data.frame(r))) done("Row written to the corrections over a period. Run to apply.", "wb_tr_author") })

  # restore (manual removals and flags only; a value a quality rule removed comes back when the rule is changed)
  rs_rows <- reactive({ s <- ctx$sel(); req(s); i <- wb_covering(E$manual$data(), ctx$part()$site, s, ctx$grid()$w, ctx$wsgn())
    list(i = i, rows = if (length(i)) as.data.table(E$manual$data()[i, , drop = FALSE]) else data.table()) })
  observeEvent(input$wb_act_restore, { s <- need_sel(); req(s, !VIEWER); r <- rs_rows()
    if (!length(r$i)) return(showNotification("No manual removal or flag covers the selected values. A value removed by a quality rule comes back when the rule is changed: open the quality rules.", type = "warning", duration = 8))
    showModal(modalDialog(title = "Restore values", size = "l", easyClose = TRUE, head_of(s),
      wb_sec(sprintf("%d row(s) of the corrections over a period cover them", length(r$i))), wb_preview(r$rows),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "Deleting a row brings back every value it removes or flags, also outside the selection. The table's previous version is kept in config/history."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_rs_do", "Delete rows", class = "btn-danger")))) })
  observeEvent(input$wb_rs_do, { r <- rs_rows(); req(length(r$i))
    key <- function(d) do.call(paste, c(lapply(d, as.character), sep = "\r")); drop <- key(as.data.frame(r$rows))
    if (E$manual$mutate(function(d) d[!key(d) %in% drop, , drop = FALSE])) done(sprintf("%d row(s) deleted from the corrections over a period. Run to apply.", length(r$i))) })

  # clock
  ck_row <- reactive({ s <- ctx$sel(); req(s); wb_clock_row(input$wb_ck_serial %||% s$serial[1], input$wb_ck_start %||% "", input$wb_ck_end %||% "", input$wb_ck_min %||% "", input$wb_ck_reason %||% "") })
  observeEvent(input$wb_act_clock, { s <- need_sel(); req(s, !VIEWER); sn <- unique(s$serial[!is.na(s$serial) & nzchar(s$serial)])
    if (!length(sn)) return(showNotification("The selected values have no logger serial (removed values): select values that are kept.", type = "warning"))
    showModal(modalDialog(title = "Clock correction", size = "l", easyClose = TRUE, head_of(s),
      div(class = "wb-form4", selectInput("wb_ck_serial", "Logger (serial)", sn), textInput("wb_ck_min", "Shift (minutes)", ""),
          textInput("wb_ck_start", "Start", fmt_ts(min(s$t))), textInput("wb_ck_end", "End", fmt_ts(max(s$t)))),
      textInput("wb_ck_reason", "Reason", width = "100%"),
      wb_sec("Row for time_corrections.csv"), uiOutput("wb_ck_preview"),
      div(class = "wb-p", style = "color:var(--wb-fg2)", "The shift moves every value the logger recorded between start and end (times on the base clock, as shown here), before its sensors are given sites and depths."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_ck_do", "Write row", class = "btn-primary")))) })
  output$wb_ck_preview <- renderUI(wb_preview(ck_row()))
  observeEvent(input$wb_ck_do, { r <- ck_row(); if (!why_ok("wb_ck_reason")) return()
    if (!nzchar(r$shift_minutes) || as.numeric(r$shift_minutes) == 0) return(showNotification("Say how many minutes to move the timestamps.", type = "warning"))
    if ((nzchar(r$start) && is.na(parse_date_cell(r$start))) || (nzchar(r$end) && is.na(parse_date_cell(r$end)))) return(showNotification("Start and end must look like 2019-06-30 or 2019-06-30 14:00.", type = "warning"))
    if (E$time$append(as.data.frame(r))) done("Row written to time_corrections.csv. Run to apply.") })

  # the sensor rows of the selected value's logger, and the quality rules, opened where the Standard edits them
  observeEvent(input$wb_sensor_open, { s <- ctx$sel(); d <- E$sensors$data(); req(d); ctx$open_table("sensors")
    sn <- if (!is.null(s)) unique(s$serial[nzchar(s$serial)]) else character()
    i <- which(norm_serial(as.character(d$serial)) %in% norm_serial(sn) | as.character(d$site) == ctx$part()$site); if (length(i)) E$sensors$select(i) })
  observeEvent(input$wb_rule_open, { s <- ctx$sel(); d <- E$qc$data(); req(d); ctx$open_table("qc")
    v <- if (!is.null(s)) unique(s$variable) else if (identical(ctx$part()$kind, "temp")) "temp" else c("vwc", "ec")
    i <- which(as.character(d$variable) %in% v); if (length(i)) E$qc$select(i) })
}
