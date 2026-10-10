# CS/pages/stations.R  --  Stations.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Stations ----------------------------------------------------------
  st_cfg <- reactive({ st <- m_st$data(); req(st); ensure_cols(as.data.table(st), STATION_COLS) })
  st_sources <- reactive({
    o <- outs(); req(o, nrow(o$sources) > 0); st <- st_cfg(); s <- copy(o$sources)
    s[is.na(header_name), header_name := ""]
    base <- st[header_name == "" & table == "" & start == "" & end == ""]; base <- base[!duplicated(logger_id)]
    hdr <- st[header_name != "" & station != ""]
    s[, station := base$station[match(logger_id, base$logger_id)]]
    hs <- hdr$station[match(paste(s$logger_id, s$header_name), paste(hdr$logger_id, hdr$header_name))]
    s[, `:=`(own = !is.na(hs), station = fifelse(!is.na(hs), hs, fifelse(is.na(station), "", station)))]
    cf <- o$meta[, .(conflicts = sum(as.numeric(conflicting_values), na.rm = TRUE)), by = logger_id]
    s[, conflicts := cf$conflicts[match(logger_id, cf$logger_id)]]; s[is.na(conflicts), conflicts := 0]
    s[]
  })
  st_rows <- reactive({ s <- st_sources(); switch(input$st_show, unnamed = s[station == ""], conflict = s[conflicts > 0], aside = s[tolower(station) == "ignore"], s) })
  st_names <- reactive({ st <- st_cfg(); sort(unique(st$station[st$station != ""])) })
  observe({
    ch <- st_names()
    updateSelectizeInput(session, "st_name", choices = ch, selected = isolate(input$st_name), server = FALSE)
  })
  output$st_empty <- renderUI(if (is.null(outs()) || is.null(m_st$data())) empty_msg("Loggers appear here after the first run. Press Run pipeline at the top right."))
  output$st_tbl <- renderDT({
    s <- st_rows(); validate(need(nrow(s) > 0, "No rows in this view."))
    show <- s[, .(Station = fifelse(station == "", "not named", fifelse(tolower(station) == "ignore", "set aside", station)), Logger = logger_id, `Name in file header` = header_name,
                  Model = model, Files = files, Records = records, First = first, Last = last,
                  `Conflicting values` = conflicts, Tables = tables)]
    ovid_dt(show, selection = "multiple", page = 25) |>
      formatStyle("Station", fontWeight = 600, color = styleEqual(c("not named", "set aside"), c(GB$orange, GB$fg4))) |>
      formatStyle("Conflicting values", color = styleInterval(0, c(GB$fg4, GB$yellow))) |>
      formatRound(c("Records", "Conflicting values"), 0)
  })
  # one name for every file of each selected logger; header-specific names no longer override it
  set_station <- function(sel, value) m_st$mutate(function(d) {
    d <- ensure_cols(as.data.table(d), STATION_COLS)
    for (lid in unique(sel$logger_id)) {
      d <- d[!(logger_id == lid & header_name != "" & table == "")]
      b <- which(d$logger_id == lid & d$header_name == "" & d$table == "" & d$start == "" & d$end == "")
      if (length(b)) set(d, b[1], "station", value) else d <- rbind(d, data.table(logger_id = lid, station = value), fill = TRUE)
    }
    ensure_cols(d, STATION_COLS)[order(logger_id, header_name != "", start)]
  })
  observeEvent(input$st_apply, {
    value <- trim(input$st_name)
    if (!nzchar(value)) return(note("Choose or type a station name first.", type = "warning"))
    sel <- st_rows()[input$st_tbl_rows_selected]
    if (!nrow(sel)) return(note("Select one or more rows in the table first.", type = "warning"))
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9 _.-]*$", value))
      return(note("Station names can use letters, digits, spaces, dots, dashes and underscores.", type = "warning"))
    if (set_station(sel, value))
      note(if (tolower(value) == "ignore") paste0("Set aside ", uniqueN(sel$logger_id), " logger(s)") else paste0(value, " set on ", uniqueN(sel$logger_id), " logger(s)"),
           ". Run the pipeline to apply.")
  })
