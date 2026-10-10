# ERT/pages/map_and_transects.R  --  Map and transects.
# Part of the ERT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, proj, E, mark, cat_s, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- map and transects
  transects <- reactiveVal(load_configs(CFG)$transects); t_sel <- reactiveVal("")
  observe({ t <- transects(); updateSelectInput(session, "t_pick", choices = c(`New transect` = "", setNames(t$transect_id, ifelse(nzchar(t$name), paste0(t$transect_id, " \u00b7 ", t$name), t$transect_id))),
                                                selected = t_sel()) })
  observe({ s <- cat_s()[kind %in% c("original", "merged")]; update_selectize(session, "t_surveys", choices = setNames(s$survey_id, paste(s$date, s$name)), selected = isolate(input$t_surveys), server = TRUE) })
  t_shown <- reactiveVal(NULL)
  observeEvent(list(input$t_pick, transects()), { t_sel(input$t_pick %||% ""); t <- transects()[transect_id == (input$t_pick %||% "")]; has <- nrow(t) == 1   # a change saved on any page shows here
    if (identical(list(input$t_pick, t), t_shown())) return(); t_shown(list(input$t_pick, t))
    pt <- function(a, b) if (has && nzchar(t[[a]]) && nzchar(t[[b]])) paste(t[[a]], t[[b]], sep = ", ") else ""
    updateTextInput(session, "t_id", value = if (has) t$transect_id else ""); updateTextInput(session, "t_name", value = if (has) t$name else "")
    updateTextInput(session, "t_site", value = if (has) t$site else ""); updateTextInput(session, "t_start", value = pt("start_lat", "start_lon"))
    updateTextInput(session, "t_end", value = pt("end_lat", "end_lon")); updateTextAreaInput(session, "t_notes", value = if (has) t$notes else "")
    update_selectize(session, "t_surveys", selected = if (has) cat_s()[transect_id == input$t_pick]$survey_id else character(), server = TRUE) }, ignoreInit = TRUE)
  output$t_geom <- renderUI({ a <- parse_latlon(input$t_start); b <- parse_latlon(input$t_end)
    if (nzchar(input$t_start %||% "") && is.null(a)) return(p(style = paste0("color:", PAL$red), "Start point not understood"))
    if (nzchar(input$t_end %||% "") && is.null(b)) return(p(style = paste0("color:", PAL$red), "End point not understood"))
    if (is.null(a) || is.null(b)) return(NULL); g <- geo_line(a[1], a[2], b[1], b[2])
    p(class = "muted", sprintf("Length %s m, bearing %s\u00b0 from north", fmtv(g$length_m), round(g$bearing))) })
  observe({ s <- cat_s()[kind %in% c("original", "merged")]
    yrs <- c(sort(unique(na.omit(s$year)), decreasing = TRUE), "undated", "no surveys")
    pj <- sort(unique(s$project[nzchar(s$project)]))
    updateSelectizeInput(session, "map_years", choices = yrs, selected = isolate(input$map_years))
    updateSelectizeInput(session, "map_projects", choices = c(pj, `No project` = "none"), selected = isolate(input$map_projects)) })
  # surveys not placed (no transect with start and end coordinates): say how many, and offer the Import (transects.csv)
  output$map_empty <- renderUI({ s <- cat_s()[kind %in% c("original", "merged")]; if (!nrow(s)) return(NULL); t <- transects()
    ok <- if (nrow(t)) t[nzchar(stack_blank(start_lat)) & nzchar(stack_blank(start_lon)) & nzchar(stack_blank(end_lat)) & nzchar(stack_blank(end_lon))]$transect_id else character()
    n <- sum(nzchar(stack_blank(s$transect_id)) & s$transect_id %in% ok); if (n == nrow(s)) return(NULL)
    div(class = "alert alert-secondary py-2 mb-2", if (n == 0) "No survey is on the map yet: none is on a transect with start and end coordinates. "
        else sprintf("%s of %s surveys are on the map; the others are not on a transect with start and end coordinates. ", num(n), num(nrow(s))),
        actionLink("map_import", "Import coordinates"), " (a CSV or Excel file with a transect id and its start and end latitude and longitude).") })
  observeEvent(input$map_import, open_import())
  output$map <- plotly::renderPlotly(transect_map(transects(), cat_s(), load_configs(CFG)$sites, input$t_pick %||% "",
                                                  years = if (length(input$map_years)) input$map_years, projects = if (length(input$map_projects)) input$map_projects,
                                                  basemap = input$map_base %||% "satellite", legend = input$map_legend %||% TRUE))
  t_rows <- reactive({ t <- transects(); s <- cat_s()[kind %in% c("original", "merged") & nzchar(transect_id)]
    s[, `:=`(yr = ifelse(is.na(year), "undated", as.character(year)), pj = ifelse(nzchar(project), project, "(none)"))]
    if (length(input$map_years)) s <- s[yr %in% input$map_years]
    if (length(input$map_projects)) s <- s[pj %in% sub("^none$", "(none)", input$map_projects)]
    n <- s[, .(Surveys = .N, Years = paste(sort(unique(yr)), collapse = ", "), Projects = paste(sort(unique(pj)), collapse = ", "),
               Names = paste(name, collapse = ", ")), by = "transect_id"]
    out <- merge(t, n, by = "transect_id", all = !length(input$map_years) && !length(input$map_projects))
    out[is.na(Surveys), `:=`(Surveys = 0L, Years = "", Projects = "", Names = "")]
    for (v in c("name", "site", "start_lat", "start_lon", "end_lat", "end_lon", "length_m")) { if (!v %in% names(out)) out[, (v) := ""]; out[is.na(get(v)), (v) := ""] }
    setorder(out, transect_id) })
  output$t_table <- renderDT({ o <- t_rows()
    d <- o[, .(Transect = transect_id, Name = name, Site = site, Surveys, `Survey names` = Names, Years, Projects, `Length (m)` = length_m,
               Located = ifelse(nzchar(start_lat) & nzchar(end_lat), "start and end", ifelse(nzchar(start_lat), "start only", "not yet")))]
    ovid_dt(d, selection = "single", page = 15, opts = list(dom = "tip")) })
  observeEvent(input$t_table_rows_selected, { id <- t_rows()$transect_id[input$t_table_rows_selected]; t_sel(id); updateSelectInput(session, "t_pick", selected = id) })
  observeEvent(input$t_save, {
    id <- safe_name(trim(input$t_id)); if (!nzchar(id)) return(ovid_field_error("t_id", "Give the transect an id."))
    a <- parse_latlon(input$t_start); b <- parse_latlon(input$t_end)
    if ((nzchar(input$t_start) && is.null(a)) || (nzchar(input$t_end) && is.null(b))) return(showNotification("A point is not understood: use 60.1089, -120.1045", type = "error"))
    t <- copy(transects()); old <- input$t_pick
    if (nzchar(old) && old != id) t <- t[transect_id != old]
    if (!id %in% t$transect_id) t <- rbind(t, ensure_cols(data.table(transect_id = id), TRANSECT_COLS)[, ..TRANSECT_COLS])
    g <- if (!is.null(a) && !is.null(b)) geo_line(a[1], a[2], b[1], b[2]) else NULL
    vals <- list(name = trim(input$t_name), site = trim(input$t_site), notes = trim(input$t_notes),
                 start_lat = if (is.null(a)) "" else format(a[1], digits = 8), start_lon = if (is.null(a)) "" else format(a[2], digits = 8),
                 end_lat = if (is.null(b)) "" else format(b[1], digits = 8), end_lon = if (is.null(b)) "" else format(b[2], digits = 8),
                 length_m = if (is.null(g)) "" else as.character(round(g$length_m, 1)), orientation_deg = if (is.null(g)) "" else as.character(round(g$bearing)))
    for (k in names(vals)) set(t, which(t$transect_id == id), k, vals[[k]])
    write_cfg(t, CFG$project_dir, "transects", who = "app"); transects(t)
    # link the surveys picked (and unlink those no longer picked)
    cf <- load_configs(CFG)$surveys; pick <- input$t_surveys %||% character()
    for (sid in setdiff(pick, cf$survey_id)) cf <- rbind(cf, ensure_cols(data.table(survey_id = sid), SURVEY_COLS)[, ..SURVEY_COLS])
    cf[transect_id %in% c(id, old) & !survey_id %in% pick, transect_id := ""]; cf[survey_id %in% pick, transect_id := id]
    write_cfg(cf, CFG$project_dir, "surveys", who = "app")
    x <- copy(cat_s()); x[transect_id %in% c(id, old) & !survey_id %in% pick, transect_id := ""]; x[survey_id %in% pick, transect_id := id]; cat_s(x)
    t_sel(id); updateSelectInput(session, "t_pick", selected = id)
    showNotification(sprintf("Saved transect %s with %d survey%s", id, length(pick), if (length(pick) == 1) "" else "s"), type = "message") })
  output$profile_help <- renderUI({ h <- PROFILE_HELP[input$profile %||% ""]; if (length(h) && !is.na(h)) p(class = "muted", h, " ", actionLink("to_ref", "More on the Reference page")) })
  observeEvent(input$to_ref, nav_select("nav", "Reference"))
  output$cat_path <- renderUI({ cat_s()
    if (is_imported()) span(class = "tag", style = paste0("color:", PAL$gold), "Viewing ", basename(PROJECT), " (imported) \u00b7 ", actionLink("back_home", "back to my catalogue"))
    else span(class = "muted", paste("catalogue:", CAT)) })
  switch_project <- function(dir) { CFG$project_dir <<- dir; set_paths(); proj(dir); cat_s(load_catalogue()); qc(load_configs(CFG)$qc); ELEV(load_configs(CFG)$elevations); transects(load_configs(CFG)$transects) }
  observeEvent(input$cat_import, {
    nm <- safe_name(sub("\\.zip$", "", input$cat_import$name, ignore.case = TRUE)); if (!nzchar(gsub("[._]", "", nm))) nm <- format(Sys.time(), "import_%Y%m%d_%H%M%S")   # never the imports folder itself
    dest <- file.path(dirname(path.expand(HOME_PROJECT)), "ERT_imports", nm)
    ok <- tryCatch({ import_catalogue(input$cat_import$datapath, dest); TRUE }, error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); FALSE })
    if (ok) { switch_project(dest); nav_select("nav", "Archive")
      showNotification(paste0("Opened ", input$cat_import$name, ". It is kept in ", dest, "; changes are saved in that copy."), type = "message", duration = 8) } })
  observeEvent(input$back_home, switch_project(HOME_PROJECT))
  # The set-up check comes first: with errors, a question (review, rebuild anyway, cancel); a rebuild anyway is logged.
  observeEvent(input$rebuild, do_rebuild(FALSE))
  observeEvent(input$rebuild_force, { removeModal(); do_rebuild(TRUE) })
  observeEvent(input$rebuild_review, { removeModal(); ovid_go(session, "Setup") })
  do_rebuild <- function(force) {
    if (run_busy()) return(showNotification("A scan is already going: wait for it, or cancel it in the box at the bottom right.", type = "warning"))
    if (is_imported()) return(showNotification("This is an imported catalogue: go back to your own catalogue to scan it.", type = "warning"))
    CFG$raw_dirs <<- trim(strsplit(input$set_raw, "\n")[[1]]); CFG$raw_dirs <<- CFG$raw_dirs[nzchar(CFG$raw_dirs)]; CFG$project_dir <<- ovid_project_dir(input$set_project)   # none set: the default folder (core util.R)
    SETTINGS <<- utils::modifyList(SETTINGS, list(raw_dirs = CFG$raw_dirs, project_dir = CFG$project_dir)); settings_save(SETTINGS_FILE, SETTINGS[c("raw_dirs", "project_dir")]); set_paths()   # only what changed: other keys (ui, time zone) may have been saved since start
    HOME_PROJECT <<- CFG$project_dir; proj(CFG$project_dir)                  # the folder set here is the user's own catalogue
    e <- tryCatch(setup_check(CFG), error = function(x) NULL); e <- if (is.null(e)) NULL else e[level == "error"]
    if (!force && !is.null(e) && nrow(e)) return(showModal(setup_gate(e, "rebuild_review", "rebuild_force", "Scan")))
    cfg <- CFG; if (force) cfg$run_anyway <- setup_problem_lines(e)
    run_start(session, cfg, label = "Scanning the archive", function(r) {             # in the background (core runner.R)
      rv$log <- r$lines
      if (r$status == "cancelled") return(showNotification("Scan cancelled. Files it had written are kept; the next scan redoes what it did not finish.", type = "warning", duration = 10))
      if (r$status != "finished") return(showNotification(r$message, type = "error", duration = NULL))
      rv$tick <- rv$tick + 1; showNotification(paste0("Archive scanned.", run_redone(PROJECT)), type = "message", duration = 8) }) }
  observeEvent(input$save, {
    nm <- safe_name(trim(input$save_name)); if (!nzchar(nm)) return(ovid_field_error("save_name", "Give the profile a name."))
    new <- copy(live())[, `:=`(profile = nm, applies_to = "all", notes = paste("saved from the QC sliders", format(Sys.Date())))]
    q <- rbind(qc()[profile != nm], new); write_cfg(q, CFG$project_dir, "qc", who = "app"); qc(q)
    showNotification(paste0("Saved profile '", nm, "' (", nrow(new), " rules)"), type = "message") })
  group_by_server("gb", reactive({ x <- tryCatch(cat_s(), error = function(e) NULL); if (is.null(x) || !nrow(x)) NULL else x[, .(serial = or_blank(serial, paste(or_blank(model, "instrument"), "(no serial)")), file = survey_id, first = datetime_start, last = datetime_end)] }), reactive({ proj(); PROJECT }), open = function(r) open_survey(strsplit(r$files, "; ")[[1]][1]))   # core grouping.R; the survey opens in this interface, the session kept
