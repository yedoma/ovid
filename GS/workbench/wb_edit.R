# GS/workbench/wb_edit.R  --  Corrections from the Workbench: the rows it writes and the dialogs that write them.
# Part of Ovid Core // GS. Sourced by wb_gs.R. Every correction is a row in a set-up table, written through the same
# table editors as the Set-up tables page (Ovid/editor.R: history in config/history, saves from the other interface or
# another window noticed), in the columns the QC page and the pipeline use: manual_flags.csv for measurements flagged by hand,
# and electrode_status.csv when Unflag turns electrodes back on (they are turned off on the QC page). The pipeline applies
# them on the next rebuild. Measurements and raw files are never written.

# ---- rows -------------------------------------------------------------------------------------------------------------
# Measurement numbers as the tables write them: runs as ranges, 5-20,31.
wb_ids_text <- function(ids) {
  ids <- sort(unique(as.integer(ids[!is.na(ids)]))); if (!length(ids)) return("")
  run <- cumsum(c(1L, diff(ids) != 1L))
  paste(vapply(split(ids, run), function(x) if (length(x) == 1L) as.character(x) else paste0(x[1], "-", x[length(x)]), ""), collapse = ",")
}
active_rows <- function(x) !tolower(trim(as.character(x))) %in% c("false", "0", "no")

# A manual_flags.csv row (MANUAL_COLS) that flags measurements ids of survey sid.
wb_flag_row <- function(sid, ids, flag, reason, author)
  data.table(survey_id = sid, meas_ids = wb_ids_text(ids), electrode = "", flag = trim(flag), active = "TRUE", reason = trim(reason),
             author = trim(author), date = format(Sys.Date()))

# The measurements of survey sid that its active manual flags take (by number, or by an electrode they use), as qc_survey() reads them.
wb_flagged_by_hand <- function(manual, sid, d) {
  m <- as.data.table(manual); if (!nrow(m)) return(integer())
  m <- m[trim(survey_id) == sid & active_rows(active)]
  unique(unlist(lapply(seq_len(nrow(m)), function(i) { el <- suppressWarnings(ids_from(m$electrode[i]))
    c(suppressWarnings(ids_from(m$meas_ids[i])), if (length(el)) d$meas_id[d$a %in% el | d$b %in% el | d$m %in% el | d$n %in% el]) })))
}

# Survey sid's manual flags with the measurements ids taken out. A row by measurement number loses those numbers, and is
# deleted when it has none left; a row by electrode flags every measurement using the electrode, so it is deleted when one of
# ids uses it. Inactive rows are left alone. Returns the new table, and the rows changed as they were (before) and become (after).
wb_unflag <- function(manual, sid, ids, d) {
  m <- as.data.table(manual); empty <- m[0]
  if (!nrow(m)) return(list(table = m, before = empty, after = empty, hit = integer()))
  new <- copy(m); hit <- integer(); drop <- integer()
  for (i in which(trim(m$survey_id) == sid & active_rows(m$active))) {
    own <- suppressWarnings(ids_from(m$meas_ids[i])); el <- suppressWarnings(ids_from(m$electrode[i]))
    if (length(el) && any(d$meas_id %in% ids & (d$a %in% el | d$b %in% el | d$m %in% el | d$n %in% el))) { hit <- c(hit, i); drop <- c(drop, i); next }
    if (any(own %in% ids)) { hit <- c(hit, i); left <- wb_ids_text(setdiff(own, ids)); set(new, i, "meas_ids", left)
      if (!nzchar(left) && !length(el)) drop <- c(drop, i) }
  }
  list(table = new[!seq_len(nrow(new)) %in% drop], before = m[hit], after = new[setdiff(hit, drop)], hit = hit)
}

# Survey sid's rows of electrode_status.csv (its survey_id, status off) that turn off an electrode measurements ids use, with
# those electrodes taken out (a row left with none is deleted). Rows for a whole transect are not changed here: they are
# returned as `transect` when they apply to the survey (s1: its transect and date).
wb_electrodes_on <- function(status, sid, ids, d, s1 = NULL) {
  e <- as.data.table(status); empty <- e[0]
  used <- unique(unlist(d[meas_id %in% ids, .(a, b, m, n)])); used <- used[!is.na(used) & used != 0]
  if (!nrow(e) || !length(used)) return(list(table = e, before = empty, after = empty, hit = integer(), transect = empty))
  new <- copy(e); hit <- integer(); drop <- integer()
  off <- tolower(trim(e$status)) == "off"
  for (i in which(trim(e$survey_id) == sid & off)) { el <- suppressWarnings(ids_from(e$electrode[i])); if (!any(el %in% used)) next
    hit <- c(hit, i); left <- wb_ids_text(setdiff(el, used)); set(new, i, "electrode", left); if (!nzchar(left)) drop <- c(drop, i) }
  tr <- which(!nzchar(trim(e$survey_id)) & off & vapply(seq_len(nrow(e)), function(i) any(electrodes_off(e[i], sid, s1) %in% used), NA))
  list(table = new[!seq_len(nrow(new)) %in% drop], before = e[hit], after = new[setdiff(hit, drop)], hit = hit, transect = e[tr])
}

# ---- dialogs ----------------------------------------------------------------------------------------------------------
# ctx: what wb_server shares (E: the table editors, key, part, dat, sel, grid, open_table, set_view, clear_sel).
wb_edit_server <- function(input, output, session, ctx) {
  wb_undo_server(input, session)                                   # Undo after an action (core workbench.R)
  E <- ctx$E
  who <- reactiveVal("")                                   # initials, kept for the next dialog in this window
  done <- function(msg, id = NULL) { if (!is.null(id)) who(trim(input[[id]] %||% "")); removeModal(); ctx$clear_sel(); wb_undo_offer(session, msg) }
  need_sel <- function() { s <- ctx$sel(); if (is.null(s) || !length(s$ids)) { showNotification("Select measurements in the grid, or click or drag on the pseudosection, first.", type = "warning"); NULL } else s }
  author_in <- function(id) textInput(id, "Initials", value = isolate(who()), width = "100%")
  head_of <- function(s) div(class = "wb-p", style = "color:var(--wb-fg2)",
    paste0(ctx$part()$label, " \u00b7 ", length(s$ids), if (length(s$ids) == 1) " measurement" else " measurements", " (#", wb_ids_text(s$ids), ")"))
  why_ok <- function(id) if (!nzchar(trim(input[[id]] %||% ""))) { ovid_field_error(id, "Give a reason; it goes in the audit trail."); FALSE } else TRUE
  note <- function(...) div(class = "wb-p", style = "color:var(--wb-fg2)", ...)
  tab <- function(k) { d <- E[[k]]$data(); if (is.null(d)) load_configs(CFG)[[k]] else as.data.table(d) }

  # flag by hand
  fl_ids <- reactive({ s <- ctx$sel(); req(s); setdiff(s$ids, wb_flagged_by_hand(tab("manual"), ctx$key(), ctx$dat())) })
  fl_row <- reactive(wb_flag_row(ctx$key(), fl_ids(), input$wb_fl_flag %||% "manually excluded", input$wb_fl_reason %||% "", input$wb_fl_author %||% ""))
  observeEvent(input$wb_act_flag, { s <- need_sel(); req(s)
    if (!length(fl_ids())) return(showNotification("The selected measurements are flagged by hand already.", type = "warning"))
    showModal(modalDialog(title = "Flag measurements", size = "l", easyClose = TRUE, head_of(s),
      note("They are flagged in every QC profile on the next scan and left out of the measurements that pass; the measurements themselves are kept as read."),
      div(class = "wb-form2", selectInput("wb_fl_flag", "Flag", FLAG_REASONS, "manually excluded", width = "100%"), author_in("wb_fl_author")),
      textInput("wb_fl_reason", "Reason (goes in the audit trail)", width = "100%"),
      wb_sec("Row for manual_flags.csv"), uiOutput("wb_fl_preview"),
      if (length(fl_ids()) < length(s$ids)) note(length(s$ids) - length(fl_ids()), " of them are flagged by hand already and are left out."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_fl_do", "Write row", class = "btn-primary")))) })
  output$wb_fl_preview <- renderUI(wb_preview(fl_row()))
  observeEvent(input$wb_fl_do, { if (!why_ok("wb_fl_reason")) return(); r <- fl_row(); req(nzchar(r$meas_ids)); n <- length(fl_ids())
    if (E$manual$append(as.data.frame(r))) done(sprintf("%d measurement(s) flagged in manual_flags.csv. Scan to apply.", n), "wb_fl_author") })

  # unflag: out of the manual flags, and electrodes of this survey back on (a flag from a QC rule goes when the profile changes)
  un <- reactive({ s <- ctx$sel(); req(s); d <- ctx$dat()
    list(m = wb_unflag(tab("manual"), ctx$key(), s$ids, d), e = wb_electrodes_on(tab("electrodes"), ctx$key(), s$ids, d, ctx$part()$s1),
         rules = { f <- ctx$flags_now(); if (is.null(f)) character() else unique(f[meas_id %in% s$ids & !rule %in% c("manual", "electrode_off"), rule]) }) })
  observeEvent(input$wb_act_unflag, { s <- need_sel(); req(s); u <- un()
    if (!length(u$m$hit) && !length(u$e$hit)) return(showNotification(paste0("No manual flag of this survey, and no electrode turned off for it, takes the selected measurements.",
      if (length(u$rules)) paste0(" They are flagged by QC rules (", paste(u$rules, collapse = ", "), "): change the profile on the QC page.") else "",
      if (nrow(u$e$transect)) " An electrode turned off for the whole transect takes them: change it on the QC page." else ""), type = "warning", duration = 10))
    showModal(modalDialog(title = "Unflag measurements", size = "l", easyClose = TRUE, head_of(s),
      if (length(u$m$hit)) tagList(wb_sec(sprintf("manual_flags.csv: %d row(s) change", length(u$m$hit))), div(class = "wb-cards",
        div(span(class = "wb-eb", "now"), wb_preview(u$m$before[, .(meas_ids, electrode, flag, reason)])),
        div(span(class = "wb-eb", "after"), if (nrow(u$m$after)) wb_preview(u$m$after[, .(meas_ids, electrode, flag, reason)]) else note("the row(s) are deleted")))),
      if (length(u$e$hit)) tagList(wb_sec(sprintf("electrode_status.csv: %d row(s) change", length(u$e$hit))), div(class = "wb-cards",
        div(span(class = "wb-eb", "now"), wb_preview(u$e$before[, .(electrode, status, reason)])),
        div(span(class = "wb-eb", "after"), if (nrow(u$e$after)) wb_preview(u$e$after[, .(electrode, status, reason)]) else note("the row(s) are deleted")))),
      if (any(nzchar(trim(u$m$before$electrode)))) note("A row by electrode is deleted whole, and that brings back every measurement it flags, also outside the selection."),
      note("The tables' previous versions are kept in config/history."),
      if (length(u$rules)) note("Flags from QC rules (", paste(u$rules, collapse = ", "), ") stay: they change with the profile on the QC page."),
      if (nrow(u$e$transect)) note("An electrode turned off for the whole transect also takes some of them; it is not changed here (the QC page)."),
      footer = tagList(modalButton("Cancel"), actionButton("wb_un_do", "Change rows", class = "btn-danger")))) })
  observeEvent(input$wb_un_do, { s <- ctx$sel(); req(s); d <- ctx$dat(); sid <- ctx$key(); u <- un(); ok <- TRUE      # u: what the dialog showed
    if (length(u$m$hit)) ok <- E$manual$mutate(function(x) as.data.frame(wb_unflag(x, sid, s$ids, d)$table))
    if (ok && length(u$e$hit)) { s1 <- ctx$part()$s1; ok <- E$electrodes$mutate(function(x) as.data.frame(wb_electrodes_on(x, sid, s$ids, d, s1)$table)) }
    if (ok) done(paste0(paste(c(if (length(u$m$hit)) "manual_flags.csv", if (length(u$e$hit)) "electrode_status.csv"), collapse = " and "), " changed. Scan to apply.")) })

  # the QC page, the survey's manual flags and its details, opened where the Standard edits them
  observeEvent(input$wb_qc_open, { updateSelectInput(session, "qc_survey", selected = ctx$key()); ctx$set_view("QC") })
  observeEvent(input$wb_details_open, { updateSelectInput(session, "survey", selected = ctx$key()); ctx$set_view("Survey") })
  observeEvent(input$wb_flags_open, { d <- E$manual$data(); req(d); ctx$open_table("manual")
    i <- which(trim(as.character(d$survey_id)) == ctx$key()); if (length(i)) E$manual$select(i) })
}
