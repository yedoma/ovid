# RS/workbench/wb_edit.R  --  Corrections from the Workbench: the rows it writes and the dialogs that write them.
# Part of Ovid Core // RS. Sourced by wb_rs.R. Every correction is a row in a set-up table, written through the same table
# editors as the Set-up tables page (Ovid/editor.R: history in config/history, saves from the other interface or another
# window noticed): file_rules.csv for images left out, flights.csv for what people decide about a flight (as the Flight page
# writes it). The next rebuild applies them; sources are never written.

# ---- rows -------------------------------------------------------------------------------------------------------------
# file_rules.csv rows (FILE_RULE_COLS) that ignore the images rel (paths within their source): one row per folder, its
# files named in full, so nothing else in the folder is taken.
wb_ignore_rows <- function(rel, reason) {
  rel <- unique(rel[!is.na(rel) & nzchar(rel)]); if (!length(rel)) return(empty_cfg(FILE_RULE_COLS))
  d <- data.table(folder = dirname(rel), file = basename(rel))[folder == ".", folder := ""]
  d[, .(pattern = paste0("^", if (nzchar(folder[1])) paste0(rx_escape(folder[1]), "/") else "", "(", paste(rx_escape(sort(file)), collapse = "|"), ")$"),
        action = "ignore", reason = trim(reason)), by = folder][, folder := NULL][]
}
# flights.csv with the row of flight fid set to the values v (added when the flight has none yet).
wb_flight_set <- function(flights, fid, v) {
  cf <- ensure_cols(as.data.table(flights), FLIGHT_COLS); i <- which(cf$flight_id == fid)
  if (!length(i)) { cf <- rbind(cf, ensure_cols(data.table(flight_id = fid), FLIGHT_COLS)[, ..FLIGHT_COLS], fill = TRUE); i <- nrow(cf) }
  for (k in names(v)) set(cf, i, k, trim(v[[k]] %||% ""))
  cf[]
}

# ---- dialogs ----------------------------------------------------------------------------------------------------------
# ctx: what wb_server shares (E: the table editors, std, key, part, sel, dat, grid, open_table, set_view, clear_sel).
wb_edit_server <- function(input, output, session, ctx) {
  wb_undo_server(input, session)                                   # Undo after an action (core workbench.R)
  E <- ctx$E
  done <- function(msg) { removeModal(); ctx$clear_sel(); wb_undo_offer(session, msg) }
  need_sel <- function() { s <- ctx$sel(); if (is.null(s) || !length(s$rel)) { showNotification("Select images in the grid, or click or drag on the positions, first.", type = "warning"); NULL } else s }
  note <- function(...) div(class = "wb-p", style = "color:var(--wb-fg2)", ...)

  # leave images out (an ignore rule; applied on the next rebuild, from the sources' last scans)
  out_rows <- reactive({ s <- ctx$sel(); req(s); wb_ignore_rows(s$rel, input$wb_out_reason %||% "") })
  observeEvent(input$wb_act_out, { s <- need_sel(); req(s)
    showModal(modalDialog(title = "Leave images out", size = "l", easyClose = TRUE,
      note(ctx$part()$label, " \u00b7 ", length(s$rel), if (length(s$rel) == 1) " image" else " images"),
      note("The images are listed in the file audit as ignored and left out of the flight on the next rebuild (no source is read again). The files themselves are not touched. Taking the rule out brings them back when the source is next scanned."),
      if (1L %in% s$rows) div(class = "wb-warn", "The selection holds the flight's first image, which names the flight: without it the flight gets a new id, and what is saved for it in flights.csv and links.csv no longer applies."),
      textInput("wb_out_reason", "Reason (goes in file_rules.csv)", width = "100%"),
      wb_sec("Rows for file_rules.csv"), uiOutput("wb_out_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("wb_out_do", "Write rows", class = "btn-primary")))) })
  output$wb_out_preview <- renderUI(wb_preview(out_rows()))
  observeEvent(input$wb_out_do, { r <- out_rows(); n <- length(ctx$sel()$rel)
    if (!nzchar(trim(input$wb_out_reason %||% ""))) return(ovid_field_error("wb_out_reason", "Give a reason."))
    if (E$file_rules$append(as.data.frame(r))) done(sprintf("%d image(s) left out in file_rules.csv. Rebuild to apply.", n)) })

  # what people decide about the flight: the Flight page's form (pages/ui.R), saved as it saves it
  observeEvent(input$wb_act_edit, { p0 <- ctx$part()
    showModal(modalDialog(title = "Edit flight", size = "l", easyClose = TRUE, note(p0$label, " \u00b7 ", p0$fid),
      rs_flight_form("wb_ed_", ctx$std$flight_row(p0$fid)), footer = modalButton("Cancel"))) })
  observeEvent(input$wb_ed_save, { fid <- ctx$key()
    if (ctx$std$save_flight(fid, rs_flight_form_read(input, "wb_ed_"), wb = TRUE)) done("Saved to flights.csv.") })   # the Flight page's save (pages/flight.R)

  # the flight's links, the map and the file rules, where the Standard edits them
  observeEvent(input$wb_links_open, { ctx$std$sel(ctx$key()); ctx$set_view("Flight") })
  observeEvent(input$wb_map_open, { ctx$std$sel(ctx$key()); ctx$set_view("Map") })
  observeEvent(input$wb_rules_open, ctx$open_table("file_rules"))
}
