# RS/pages/missions.R  --  Missions: confirm a grouping, or split a sortie off.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- missions: confirm a grouping, or split a sortie off (config/flights.csv, mission_set; the next rebuild regroups) ----
  output$mission <- renderUI({ f <- cur(); if (is.null(f) || !nrow(f)) return(NULL)
    if (!"mission" %in% names(f) || is.na(f$mission)) return(p(class = "muted", "Rebuild the catalogue to group sorties into missions."))
    m <- fl()[mission == f$mission][order(mission_part)]
    tagList(p(if (nrow(m) == 1L) "A mission of one sortie." else paste0(nrow(m), " sorties, ", f$mission_status, ". ", f$mission_evidence)),
      if (nrow(m) > 1L) lapply(seq_len(nrow(m)), function(i) div(class = "fact", span(class = "k", paste("Sortie", m$mission_part[i])),
        span(class = "v", if (m$flight_id[i] == f$flight_id) strong(m$start[i], m$name[i]) else paste(m$start[i], m$name[i]), br(),
             span(class = "muted", paste0(m$source_id[i], ":", m$folder[i], " \u00b7 ", num(m$n_images[i]), " images"))))))
  })
  set_mission <- function(ids, value, msg) {                    # through the flights.csv editor, as every save (pages/flight.R)
    ok <- E$flights$mutate(function(d) { cf <- ensure_cols(as.data.table(d), FLIGHT_COLS)
      new <- setdiff(ids, cf$flight_id); if (length(new)) cf <- rbind(cf, ensure_cols(data.table(flight_id = new), FLIGHT_COLS)[, ..FLIGHT_COLS], fill = TRUE)
      as.data.frame(cf[flight_id %in% ids, mission_set := value]) })
    if (!ok) return(showNotification("flights.csv was not saved: it is open with other changes, or not made yet (rebuild first).", type = "warning"))
    x <- C(); x$flights <- copy(x$flights); x$flights[flight_id %in% ids, `:=`(mission_set = value, mission_status = if (value == "own") "single" else "confirmed")]
    if (value == "own") x$flights[flight_id %in% ids, `:=`(mission = paste0(mission, "-", flight_id), mission_part = 1L, mission_parts = 1L)]
    C(x); showNotification(msg, type = "message") }
  observeEvent(input$m_confirm, { f <- cur(); if (is.null(f) || !nrow(f) || is.na(f$mission)) return()
    set_mission(fl()[mission == f$mission]$flight_id, f$mission, "Grouping confirmed") })
  observeEvent(input$m_split, { f <- cur(); if (is.null(f) || !nrow(f)) return()
    set_mission(f$flight_id, "own", "Sortie split off (the others regroup on the next rebuild)") })
  lk_cur <- reactive({ f <- cur(); if (is.null(f) || !nrow(f)) data.table() else C()$links[flight_id == f$flight_id] })
  output$links <- renderDT({ l <- lk_cur(); if (!nrow(l)) return(ovid_dt(data.table(` ` = "No linked files"), opts = list(dom = "t")))
    ovid_dt(l[, .(Role = role, File = file, State = state, Score = score, Evidence = evidence)], selection = "single", page = 10,
              opts = list(dom = "tip")) })
  set_link <- function(state) { l <- lk_cur(); i <- input$links_rows_selected; if (!length(i) || !nrow(l)) return(showNotification("Select a link first", type = "warning"))
    lc <- load_configs(CFG)$links; lc <- lc[!(flight_id == l$flight_id[i] & file == l$file[i])]
    lc <- rbind(lc, data.table(flight_id = l$flight_id[i], file = l$file[i], state = state, reason = trim(input$l_reason), author = WHO, date = format(Sys.Date())), fill = TRUE)
    write_cfg(lc[, ..LINK_COLS], CFG$project_dir, "links", who = WHO)
    x <- C(); x$links <- copy(x$links); x$links[flight_id == l$flight_id[i] & file == l$file[i], `:=`(state = state, evidence = paste0(state, " by ", WHO, if (nzchar(trim(input$l_reason))) paste0(": ", trim(input$l_reason)) else ""))]
    C(x); showNotification(paste("Link", state, "(statuses update on the next rebuild)"), type = "message") }
  observeEvent(input$l_confirm, set_link("confirmed")); observeEvent(input$l_reject, set_link("rejected"))
