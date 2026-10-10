# RS/pages/flight.R  --  Flight page: what people decide, and links.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Flight page: what people decide, and links ----
  output$d_context <- renderUI(switch(input$nav %||% "",
    Map = ovid_context(sprintf("%s of %s flights", num(nrow(shown())), num(nrow(fl())))),
    Flight = { f <- cur(); if (!is.null(f) && nrow(f)) ovid_context(paste(f$date, f$name)) },
    Imagery = ovid_context(sprintf("%s files", num(nrow(dshown())))), NULL))
  output$flight_quality <- renderUI({ f <- cur(); if (is.null(f) || !nrow(f) || !"gsd_cm_median" %in% names(f)) return(NULL)
    v <- function(k, unit = "", d = 2) { x <- f[[k]][1]; if (is.null(x) || is.na(x)) "not known" else paste0(format(round(x, d), big.mark = ","), unit) }
    layout_columns(fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
      value_box("Ground sample distance", v("gsd_cm_median", " cm"), p(class = "muted", "median, from height above take-off"), theme = VB()),
      value_box("Forward overlap", v("fwd_overlap_pct_median", " %", 0), theme = VB()), value_box("Side overlap", v("side_overlap_pct_median", " %", 0), theme = VB()),
      value_box("Speed", v("speed_ms_median", " m/s", 1), p(class = "muted", paste("exposure", v("exposure_s_median", " s", 4))), theme = VB()),
      value_box("Motion blur", v("blur_px_median", " px"), p(class = "muted", paste(v("blur_over_limit_pct", " %", 0), "of images above", v("blur_limit_px", " px"))), theme = VB()),
      value_box("Sun elevation", paste(v("sun_elev_min_deg", "", 0), "to", v("sun_elev_max_deg", " deg", 0)), theme = VB())) })
  output$flight_title <- renderUI({ f <- cur(); if (is.null(f) || !nrow(f)) p(class = "muted", "Pick a flight on the Map page first.") else h3(paste(f$date, f$name, "\u00b7", f$sensor)) })
  e_shown <- reactiveVal(NULL)                                   # the flight's saved row as the form last showed it
  observeEvent(list(cur(), live_stamp(cfg_path(CFG$project_dir, "flights"))), { f <- cur(); if (is.null(f) || !nrow(f)) return()   # a change saved on any page shows here
    cf <- load_configs(CFG)$flights[flight_id == f$flight_id]; if (identical(list(f$flight_id, cf), e_shown())) return(); e_shown(list(f$flight_id, cf))
    rs_flight_form_update(session, "e_", if (nrow(cf)) as.list(cf[1]) else list()) })   # the form shared with the Workbench (pages/ui.R)
  observeEvent(input$e_save, { f <- cur(); if (is.null(f) || !nrow(f)) return()
    if (save_flight(f$flight_id, rs_flight_form_read(input, "e_"))) showNotification("Saved", type = "message") })
  # flights.csv is written only through its Set-up tables editor (core/editor.R): from this page, the Timeline's Set site and
  # the Workbench's Edit flight (the same form) alike, so an edit not yet saved in the editor is neither lost nor written over.
  # The reason for a status change is asked for and never required (where it came from and initials help). Missions are set
  # with Confirm this grouping and Split this sortie off (pages/missions.R).
  flight_row <- function(id) { d <- isolate(E$flights$data()); cf <- if (is.null(d)) data.table() else as.data.table(d)[flight_id == id]
    k <- setdiff(FLIGHT_COLS, "flight_id"); stats::setNames(lapply(k, function(x) if (nrow(cf) && x %in% names(cf) && !is.na(cf[[x]][1])) as.character(cf[[x]][1]) else ""), k) }
  save_flight <- function(id, v) {
    ok <- E$flights$mutate(function(d) as.data.frame(wb_flight_set(d, id, v)))      # workbench/wb_edit.R: the row set, added if new
    if (!ok) showNotification("flights.csv was not saved: it is open with other changes, or not made yet (rebuild first).", type = "warning")
    else show_edit(id, flight_row(id))
    ok }
  # a flight's edits shown at once (here and in the Workbench); the next rebuild writes them into the catalogue
  show_edit <- function(id, v) { x <- isolate(C()); if (is.null(x)) return(invisible()); x$flights <- copy(x$flights); r <- x$flights$flight_id == id
    for (k in intersect(c("display_name", "project", "notes", "status_note", "exclude_from_archive"), names(v))) set(x$flights, which(r), k, trim(v[[k]] %||% ""))
    set(x$flights, which(r), "site", trim(v$site %||% "") %|na|% x$flights$site_found[r])
    set(x$flights, which(r), "positioning_status", trim(v$positioning_status %||% "") %|na|% x$flights$positioning_found[r])
    set(x$flights, which(r), "name", fifelse(nzchar(trim(v$display_name %||% "")), trim(v$display_name %||% ""), x$flights$name[r]))
    C(x) }
  # flights.csv saved on any page (the Set-up tables, the Workbench, another window): every flight's edits shown at once
  observeEvent(live_stamp(cfg_path(CFG$project_dir, "flights")), { cf <- tryCatch(ensure_cols(load_configs(CFG)$flights, FLIGHT_COLS), error = function(e) NULL)
    if (is.null(cf) || is.null(isolate(C()))) return()
    for (i in seq_len(nrow(cf))) { v <- as.list(cf[i]); for (k in c("site", "positioning_status")) if (!nzchar(v[[k]] %||% "")) v[k] <- list(NA_character_)
      show_edit(cf$flight_id[i], v) } }, ignoreInit = TRUE)
