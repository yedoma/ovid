# RS/pages/list_edits.R  --  The lists under the map (Flights edited in place, Photo sets, Other geodata), the Site names
# card on the Timeline, and the one Save bar for all of them.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, after flight.R (show_edit, flight_row).
# Changes wait in the Save bar (Undo, Discard) until Save or Cmd/Ctrl+S; Save writes each table once through its Set-up
# tables editor (core/editor.R: history in config/history): flights.csv, site_names.csv and sites.csv, file_rules.csv,
# and the review decisions for the folders kept. The plain functions are in pages/fct_edits.R.

  # ---- what waits to be saved: one step per action, so Undo takes back the last action whichever table it touched ----
  le_staged <- reactiveVal(edit_empty())                                                              # flights.csv
  le_names <- reactiveVal(data.table(found = character(), site = character(), step = integer()))   # site_names.csv
  le_geo_st <- reactiveVal(data.table(source_id = character(), folder = character(), kind = character(), decision = character(), step = integer()))
  le_step <- function() max(c(0L, le_staged()$step, le_names()$step, le_geo_st()$step)) + 1L
  le_last <- function(d, by) if (nrow(d)) unique(d[order(-step)], by = by) else d

  # ---- the Flights list ----
  le_view <- reactive(edit_view(shown(), le_staged()))                                 # the flights in view, with what waits to be saved
  le_list <- reactive({ v <- le_view(); fold_joined(v[kind != "photo set"]) })          # a joined flight is one row (pages/fct_edits.R)
  le_sel_ids <- reactive({ i <- input$flights_rows_selected; x <- isolate(le_list()); if (!length(i) || !nrow(x)) character() else unlist(strsplit(x$part_ids[i[i <= nrow(x)]], " ", fixed = TRUE)) })
  output$ed_n <- renderText({ n <- length(input$flights_rows_selected); if (n) paste(n, "selected") else "Select flights, then" })
  output$n_shown <- renderText(sprintf("Flights %s", num(nrow(le_list()))))
  le_flights_tbl <- reactive({ x <- le_list(); k <- fchk()[match(x$flight_id, flight_id)]; cells <- edit_cells(le_staged())
    mark <- function(v, col) { v <- as.character(v); v[is.na(v)] <- ""; v <- htmltools::htmlEscape(v); i <- paste(x$flight_id, col, sep = "|") %in% cells
      v[i] <- paste0('<span class="rs-staged">', v[i], '</span>'); v }
    x[, .(Date = date, Name = mark(paste0(name, fifelse(parts > 1, paste0(" (", parts, " sorties)"), "")), "display_name"), Site = mark(site, "site"), Project = mark(project, "project"),
          Aircraft = aircraft, Sensor = sensor, Images = n_images, GB = size_gb, Checks = if (nrow(x)) checks_html(k) else character(),
          Placed = if (nrow(x)) sprintf("%s (%s %%) · %s", num(k$n_placed), ifelse(is.na(k$placed_pct), "–", round(k$placed_pct)), k$placed_from) else character(),
          Positioning = mark(positioning_status, "positioning_status"), QA = qa_flags, Folder = paste0(source_id, ":", folder))] })
  output$flights <- renderDT({ shown(); f <- isolate(le_flights_tbl())                # redrawn when the filters change; edits replace the data in place
    ovid_long_table(f, selection = "multiple", escape = setdiff(names(f), c("Checks", "Name", "Site", "Project", "Positioning")), dom = "ti", height = "50vh",
                    options = list(order = list(0, "desc"))) })                   # core longtable.R
  le_fl_proxy <- dataTableProxy("flights")
  observeEvent(le_flights_tbl(), replaceData(le_fl_proxy, le_flights_tbl(), rownames = FALSE, resetPaging = FALSE, clearSelection = "none"), ignoreInit = TRUE)
  observeEvent(input$flights_rows_selected, { i <- input$flights_rows_selected; x <- le_list(); if (length(i) && i[length(i)] <= nrow(x)) sel(x$flight_id[i[length(i)]]) })
  observe({ f <- fl(); s <- tryCatch(load_configs(CFG)$sites, error = function(e) NULL); stamps()     # the dropdowns: known sites first, then values in use
    updateSelectizeInput(session, "ed_site", choices = edit_choices(f, "site", s), selected = "", server = FALSE)
    updateSelectizeInput(session, "ed_project", choices = edit_choices(f, "project"), selected = "", server = FALSE) })
  le_need_rows <- function(n = 1L) { ids <- le_sel_ids(); if (length(ids) < n) { showNotification(if (n > 1) "Select two or more flights in the list first." else "Select flights in the list first.", type = "warning"); NULL } else ids }
  observeEvent(input$ed_set, { ids <- le_need_rows(); req(ids); st <- le_staged(); s0 <- le_step()
    for (k in list(c("ed_site", "site"), c("ed_project", "project"), c("ed_name", "display_name"))) { v <- trim(input[[k[1]]] %||% ""); if (nzchar(v)) st <- edit_stage(st, ids, k[2], v, s0) }
    v <- input$ed_status %||% ""; if (nzchar(v)) st <- edit_stage(st, ids, "positioning_status", if (v == "(as found)") "" else v, s0)
    if (nrow(st) == nrow(le_staged())) return(showNotification("Pick a site, project, name or positioning to set first.", type = "warning"))
    le_staged(st); for (k in c("ed_site", "ed_project")) updateSelectizeInput(session, k, selected = ""); updateTextInput(session, "ed_name", value = ""); updateSelectInput(session, "ed_status", selected = "") })
  observeEvent(input$ed_photoset, { ids <- le_need_rows(); req(ids); le_staged(edit_stage(le_staged(), ids, "kind", "photo set", le_step())) })
  observeEvent(input$ed_out, { ids <- le_need_rows(); req(ids); le_staged(edit_stage(le_staged(), ids, "exclude_from_archive", "TRUE", le_step())) })
  # join: the sorties picked are one flight (flights.csv mission_set names it); Keep apart keeps each on its own
  observeEvent(input$ed_join, { ids <- le_need_rows(2L); req(ids); v <- le_view()[flight_id %in% ids][order(start)]
    gap <- c(NA, round(as.numeric(difftime(as.POSIXct(v$start[-1], tz = "UTC"), as.POSIXct(v$start[-nrow(v)], tz = "UTC"), units = "mins")) - v$duration_min[-nrow(v)] %|na|% 0))
    showModal(modalDialog(title = "Join into one flight", easyClose = TRUE,
      lapply(seq_len(nrow(v)), function(i) div(class = "fact", span(class = "k", paste("Sortie", i)), span(class = "v", paste0(v$start[i], " · ", num(v$n_images[i]), " images · ", v$duration_min[i] %|na|% "?", " min"),
        if (i > 1 && is.finite(gap[i])) span(class = "muted", paste0(" · ", gap[i], " min after sortie ", i - 1)), br(), span(class = "muted", paste0(v$source_id[i], ":", v$folder[i]))))),
      p(class = "muted mt-2", paste("Scan grouping:", paste(unique(v$mission_status %|na|% "single"), collapse = ", "), "·", paste(unique(v$mission_evidence[nzchar(v$mission_evidence %|na|% "")]), collapse = "; "))),
      textInput("join_name", "Name of the joined flight", trim(paste(v$site[1], v$date[1])), width = "100%"),
      footer = tagList(modalButton("Cancel"), actionButton("join_apart", "Keep apart"), actionButton("join_do", "Join", class = "btn-primary")))) })
  observeEvent(input$join_do, { ids <- le_sel_ids(); nm <- trim(input$join_name %||% ""); if (!nzchar(nm) || tolower(nm) == "own") return(ovid_field_error("join_name", "Give the joined flight a name."))
    le_staged(edit_stage(le_staged(), ids, "mission_set", nm, le_step())); removeModal() })
  observeEvent(input$join_apart, { le_staged(edit_stage(le_staged(), le_sel_ids(), "mission_set", "own", le_step())); removeModal() })

  # ---- Photo sets ----
  le_ps <- reactive(le_view()[kind == "photo set"])
  output$n_photosets <- renderText(sprintf("Photo sets %s", num(nrow(le_ps()))))
  output$ps_rule <- renderText(sprintf("Runs of fewer than %s images (photoset_max_images in the calibration table), or moved here by hand. They are not grouped into missions and are checked for their images only.",
                                       format(rs_calibration(cal_rs(), "photoset_max_images")$number)))
  output$photosets <- renderDT({ x <- le_ps()
    ovid_long_table(x[, .(Date = date, Name = name, Camera = sensor, Images = n_images, Minutes = duration_min %||% NA, Site = site, Folder = paste0(source_id, ":", folder))],
                    selection = "multiple", dom = "ti", height = "40vh") })
  observeEvent(input$ps_back, { i <- input$photosets_rows_selected; if (!length(i)) return(showNotification("Select photo sets first.", type = "warning"))
    le_staged(edit_stage(le_staged(), le_ps()$flight_id[i], "kind", "flight", le_step())) })

  # ---- Other geodata ----
  le_geo_all <- reactive({ x <- C(); if (is.null(x)) return(geodata_folders(NULL, NULL, fl()))
    g <- geodata_folders(x$datasets, x$gis_layers, fl()); if (!nrow(g)) return(g[, `:=`(id = character(), decision = character())])
    g[, id := geodata_id(source_id, folder, kind)]; d <- decisions(); g[, decision := fifelse(id %in% d$id, "kept", "")]
    s <- le_last(le_geo_st(), c("source_id", "folder", "kind")); if (nrow(s)) g[s, on = .(source_id, folder, kind), decision := paste0(i.decision, " (not saved)")]
    g })
  le_geo <- reactive(le_geo_all()[decision != "kept"])
  output$n_geodata <- renderText(sprintf("Other geodata %s", num(nrow(le_geo()))))
  le_geo_tbl <- reactive(le_geo()[, .(Folder = paste0(source_id, ":", folder), Kind = kind, Files = files, Where = where, Suggested = suggest, Decision = decision)])
  output$geodata <- renderDT({ C(); ovid_long_table(isolate(le_geo_tbl()), selection = "multiple", dom = "ti", height = "40vh") })
  le_geo_proxy <- dataTableProxy("geodata")
  observeEvent(le_geo_tbl(), replaceData(le_geo_proxy, le_geo_tbl(), rownames = FALSE, resetPaging = FALSE, clearSelection = "none"), ignoreInit = TRUE)
  le_geo_stage <- function(rows, decision) { if (!nrow(rows)) return(showNotification("Select folders first.", type = "warning"))
    le_geo_st(rbind(le_geo_st(), rows[, .(source_id, folder, kind, decision = decision, step = le_step())])) }
  observeEvent(input$gd_keep, le_geo_stage(le_geo()[input$geodata_rows_selected], "keep"))
  observeEvent(input$gd_remove, le_geo_stage(le_geo()[input$geodata_rows_selected], "remove"))
  observeEvent(input$gd_suggested, { g <- le_geo(); s0 <- le_step(); if (nrow(g)) le_geo_st(rbind(le_geo_st(), g[, .(source_id, folder, kind, decision = suggest, step = s0)])) })

  # ---- Site names found in folder names (the Timeline page) ----
  le_cfg <- reactive({ rv$tick; stamps(); tryCatch(load_configs(CFG), error = function(e) NULL) })
  le_sn_all <- reactive({ cf <- le_cfg(); s <- site_suggest(fl(), cf$sites, cf$site_names)
    n <- le_last(le_names(), "found"); if (nrow(n)) s[n, on = "found", mapped := paste0(fifelse(nzchar(i.site), i.site, "(not a site)"), " (not saved)")]
    s })
  le_sn_tbl <- reactive(le_sn_all()[, .(`Found name` = fifelse(nzchar(found), found, "(no name)"), Flights = flights, Suggested = suggested, Why = why, `Mapped to` = mapped)])
  output$sn_table <- renderDT({ le_cfg(); ovid_long_table(isolate(le_sn_tbl()), selection = "multiple", dom = "ti", height = "40vh") })
  le_sn_proxy <- dataTableProxy("sn_table")
  observeEvent(le_sn_tbl(), replaceData(le_sn_proxy, le_sn_tbl(), rownames = FALSE, resetPaging = FALSE, clearSelection = "none"), ignoreInit = TRUE)
  observe({ cf <- le_cfg(); s <- le_sn_all(); updateSelectizeInput(session, "sn_to", choices = unique(c(trim(cf$sites$site), s$suggested[nzchar(s$suggested)], s$found[nzchar(s$found)])), selected = "", server = FALSE) })
  # a found name gets a site (site_names.csv); the flights with no name at all get the site on their own row (flights.csv)
  le_sn_stage <- function(rows, to) { if (!nrow(rows)) return(showNotification("Select names first.", type = "warning")); s0 <- le_step()
    nm <- rows[nzchar(found)]; if (nrow(nm)) le_names(rbind(le_names(), data.table(found = nm$found, site = to(nm), step = s0)))
    if (any(!nzchar(rows$found))) { t <- to(rows[!nzchar(found)]); if (nzchar(t)) { f <- fl(); ids <- f$flight_id[(if ("site_folder" %in% names(f)) f$site_folder else f$site) %in% c("", NA)]
      le_staged(edit_stage(le_staged(), ids, "site", t, s0)) } } }
  le_sn_rows <- reactive(le_sn_all()[input$sn_table_rows_selected])
  observeEvent(input$sn_accept, { r <- le_sn_rows()[nzchar(suggested)]; if (!nrow(r)) return(showNotification("None of the selected names has a suggestion.", type = "warning")); le_sn_stage(r, function(x) x$suggested) })
  observeEvent(input$sn_map, { t <- trim(input$sn_to %||% ""); if (!nzchar(t)) return(showNotification("Pick or type the known site first.", type = "warning")); le_sn_stage(le_sn_rows(), function(x) rep(t, nrow(x))) })
  observeEvent(input$sn_keep, le_sn_stage(le_sn_rows()[nzchar(found)], function(x) x$found))
  observeEvent(input$sn_not, le_sn_stage(le_sn_rows()[nzchar(found)], function(x) rep("", nrow(x))))

  # ---- the Save bar ----
  le_pending <- reactive({ n <- c(flights.csv = edit_count(le_staged()), site_names.csv = nrow(le_last(le_names(), "found")), file_rules.csv = nrow(le_last(le_geo_st(), c("source_id", "folder", "kind")))); n[n > 0] })
  output$save_bar <- renderUI({ n <- le_pending(); session$sendCustomMessage("rs-unsaved", sum(n)); if (!length(n)) return(NULL)
    div(class = "rs-savebar d-flex flex-wrap gap-2 align-items-center", strong(sprintf("%d unsaved change%s", sum(n), if (sum(n) == 1) "" else "s")),
        span(class = "muted", paste(names(n), collapse = ", ")), span(class = "ms-auto"),
        actionButton("sv_undo", "Undo", class = "btn-sm"), actionButton("sv_discard", "Discard", class = "btn-sm"), actionButton("sv_save", "Save (⌘S)", class = "btn-sm btn-primary")) })
  observeEvent(input$sv_undo, { m <- le_step() - 1L; le_staged(le_staged()[step != m]); le_names(le_names()[step != m]); le_geo_st(le_geo_st()[step != m]) })
  observeEvent(input$sv_discard, { le_staged(edit_empty()); le_names(le_names()[0]); le_geo_st(le_geo_st()[0]) })
  le_save_all <- function() { if (!length(le_pending())) return()
    st <- le_staged(); nm <- le_last(le_names(), "found"); gs <- le_last(le_geo_st(), c("source_id", "folder", "kind")); bad <- character()
    if (nrow(st)) { if (E$flights$mutate(function(d) as.data.frame(edit_apply(d, st)))) {
        f <- edit_final(st); x <- C(); x$flights <- copy(x$flights)          # kind and joins shown at once; the rest through show_edit (pages/flight.R)
        for (i in which(f$col %in% c("kind", "mission_set"))) { r <- which(x$flights$flight_id == f$flight_id[i]); v <- f$value[i]
          if (f$col[i] == "kind" && !nzchar(v)) v <- x$flights$kind_found[r]; if (f$col[i] %in% names(x$flights)) set(x$flights, r, f$col[i], v) }
        C(x); for (id in unique(f$flight_id)) show_edit(id, flight_row(id)); le_staged(edit_empty()) } else bad <- c(bad, "flights.csv") }
    if (nrow(nm)) { ok <- E$site_names$mutate(function(d) as.data.frame(site_names_set(d, nm$found, nm$site))) && E$sites$mutate(function(d) as.data.frame(sites_add(d, nm$site)))
      if (ok) { x <- C(); x$flights <- copy(x$flights); for (i in seq_len(nrow(nm))) x$flights[tolower(site) == tolower(nm$found[i]), site := nm$site[i]]; C(x); le_names(le_names()[0]) }
      else bad <- c(bad, "site_names.csv") }
    if (nrow(gs)) { rm <- gs[decision == "remove"]; kp <- gs[decision == "keep"]
      ok <- !nrow(rm) || E$file_rules$append(as.data.frame(geodata_rule(rm$folder)))
      if (ok && nrow(kp)) review_write(review_path(CFG$project_dir), geodata_id(kp$source_id, kp$folder, kp$kind),
                                       data.table(id = geodata_id(kp$source_id, kp$folder, kp$kind), decision = "kept", note = paste0("Other geodata: ", kp$source_id, ":", kp$folder, " (", kp$kind, ")"),
                                                  author = WHO, at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      if (ok) le_geo_st(le_geo_st()[0]) else bad <- c(bad, "file_rules.csv") }
    if (length(bad)) showNotification(paste(paste(bad, collapse = ", "), "not saved: open with other changes on the Set-up tables page, or not made yet (rebuild first)."), type = "warning", duration = 8)
    else showNotification(paste0("Saved.", if (nrow(nm) || nrow(gs)) " Site names and removed folders apply on the next rebuild." else ""), type = "message") }
  observeEvent(input$sv_save, le_save_all()); observeEvent(input$sv_key, le_save_all())
