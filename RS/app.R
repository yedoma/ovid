# =============================================================================
#  app.R  --  Shiny front end for rs_pipeline.R  (Ovid Core // RS)
# =============================================================================
#      shiny::runApp("path/to/Ovid_Apps/RS")      (or double-click "Open RS (Mac).command")
#  Reads the catalogue written by rs_pipeline.R. The pages:
#    Map      every flight on satellite imagery: derived extents, image positions, flown tracks, plans and receivers,
#             with filters and the flight table; a click on the map or a row opens the flight beside the map
#    Timeline sites by year, a square per flight; Set site for flights with none; coverage by site, year and sensor
#    Flight   what people decide about a flight (name, project, site, positioning status, notes; config/flights.csv)
#             and its linked files, each confirmed or rejected with a reason (config/links.csv)
#    Imagery  satellite, aerial and LiDAR files and GIS layers, with their footprints on the map
#    GNSS     survey files stitched into one table, their quality and base
#    QA       files tied to no flight, datasets and their datum, products and STAC
#    Sources  the drives and folders the archive is built from: scan a new one, rescan, rebuild, write the viewer to send
#    Set-up tables  every table in config/, with the editor shared by all the apps (core/editor.R)
#  Two interfaces on one project: these pages (Standard) and the Workbench (preview, workbench/wb_rs.R), which shows them
#  around a grid of one flight's images and their positions. The archive is never changed. Theme: accent.vim (vimcolorschemes). Map: Leaflet from cdnjs.
# =============================================================================

for (p in c("shiny", "bslib", "DT", "data.table", "ggplot2", "jsonlite"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(bslib); library(DT); library(ggplot2) })

APP_DIR <- getwd()
Sys.setenv(RS_NO_RUN = "1")
source(file.path(APP_DIR, "rs_pipeline.R"), local = TRUE)
log_app_errors()                                   # errors outside a run go to ~/.ovid/logs (core manifest.R)
source(file.path(CORE_DIR, "shell.R"), local = TRUE)
source(file.path(CORE_DIR, "table.R"), local = TRUE)        # Ovid/table.R: the core table (ovid_dt)
cfg_mtime_ignore("review_decisions.csv")   # decisions on Needs you: a rebuild does not read them, so no "Set-up changed" notice (core shell.R)
source(file.path(CORE_DIR, "reference.R"), local = TRUE)
source(file.path(CORE_DIR, "convert_page.R"), local = TRUE) # Ovid/convert_page.R: the Convert page (units, DMS, UTM)
source(file.path(CORE_DIR, "grouping.R"), local = TRUE)     # Ovid/grouping.R: flights grouped by aircraft, stay, download or folder    # Ovid/reference.R: the Reference page (the guide as a searchable index)
REF_GO <- list()   # where each reference topic is set
source(file.path(CORE_DIR, "runner.R"), local = TRUE)       # Ovid/runner.R: scans in the background, with progress and Cancel
source(file.path(CORE_DIR, "projects.R"), local = TRUE)     # Ovid/projects.R: the Projects list (new, open, delete to the Trash)
source(file.path(CORE_DIR, "files.R"), local = TRUE)        # Ovid/files.R: Browse buttons and export names
downloadHandler <- ovid_downloads("RS")                    # every download is named ovid_rs_<what>_<date>.<ext>
source(file.path(CORE_DIR, "editor.R"), local = TRUE)       # Ovid/editor.R: the set-up table editor
source(file.path(CORE_DIR, "metasheet.R"), local = TRUE)    # Ovid/metasheet.R and metasheet_ui.R: Paste and Import in every table editor
source(file.path(CORE_DIR, "import.R"), local = TRUE)           # Ovid/import.R: reading a spreadsheet, matching columns, checking rows
source(file.path(CORE_DIR, "metasheet_ui.R"), local = TRUE)
source(file.path(CORE_DIR, "longtable.R"), local = TRUE)        # Ovid/longtable.R: long tables drawn a screen at a time
SETTINGS_FILE <- file.path(APP_DIR, "app_settings.json")
SETTINGS <- settings_read(SETTINGS_FILE)                          # core util.R: the last good copy if this one is damaged
for (k in intersect(names(SETTINGS), c("project_dir", "viewer_title"))) CFG[[k]] <- SETTINGS[[k]]   # viewer_title: the viewer's and STAC's
if (!is.null(SETTINGS$base_offset_h)) BASE_OFFSET <- CFG$base_offset_h <- SETTINGS$base_offset_h   # Settings > Time zone
if (nzchar(Sys.getenv("RS_PROJECT_DIR"))) CFG$project_dir <- Sys.getenv("RS_PROJECT_DIR")   # as in CS: the environment wins
CFG$project_dir <- ovid_project_dir(CFG$project_dir)             # none set: ~/Projects/Ovid/Projects/<app>/Untitled (core util.R)
local({ vt <- project_settings_read(CFG$project_dir)$viewer_title; if (nzchar(vt %||% "")) CFG$viewer_title <<- vt })   # saved with the project (core projects.R)
VIEWER <- FALSE                                     # RS has no viewer mode in the app (the viewer it writes is for that)
CAT <- function() file.path(path.expand(CFG$project_dir), "output", "catalogue")
WHO <- Sys.getenv("USER", Sys.getenv("USERNAME", "app"))
`%|na|%` <- function(a, b) ifelse(is.na(a) | a == "", b, a)

source(file.path(APP_DIR, "pages", "fct_outputs.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_tables.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_setup.R"), local = TRUE)
source(file.path(APP_DIR, "pages", "fct_checks.R"), local = TRUE)          # RS/pages/fct_checks.R: placed, corrected and the five checks
source(file.path(APP_DIR, "pages", "fct_edits.R"), local = TRUE)           # RS/pages/fct_edits.R: list edits, joins, site names, logs, other geodata
source(file.path(APP_DIR, "pages", "mod_flight_summary.R"), local = TRUE)  # RS/pages/mod_flight_summary.R: the summary cards (a module)
source(file.path(APP_DIR, "pages", "fct_timeline.R"), local = TRUE)        # RS/pages/fct_timeline.R: flights by site and year, Set site
source(file.path(APP_DIR, "pages", "mod_timeline.R"), local = TRUE)        # RS/pages/mod_timeline.R: the Timeline page and the strip (modules)
source(file.path(APP_DIR, "pages", "fct_checkpoint.R"), local = TRUE)      # RS/pages/fct_checkpoint.R: a checkpoint file through the core import
source(file.path(APP_DIR, "pages", "mod_checkpoint.R"), local = TRUE)      # RS/pages/mod_checkpoint.R: checkpoint accuracy (a module, on the Workbench's Positioning)

# ---- UI ------------------------------------------------------------------------------------------------------------------
source(file.path(APP_DIR, "pages", "ui.R"), local = TRUE)   # RS/pages/ui.R: the pages and ui

# ---- server --------------------------------------------------------------------------------------------------------------
# embedded = TRUE: running inside the Workbench (workbench/wb_rs.R), which shows these pages in its own frame and gets the
# catalogue, the flight in view, the set-up table editors and the build state returned at the end.
server <- function(input, output, session, embedded = FALSE) {
  reference_server(input, output, session, "RS", REF_GO)        # core reference.R
  convert_server(input, output)                                             # core convert_page.R
  time_zone_server(input, on_change = function(z) CFG$base_offset_h <<- z)   # Settings > Time zone (core shell.R)
  colours_server(input, output, "RS")                                       # Settings > Colours (core shell.R)
  troubleshoot_server(output, function() CFG$project_dir, function() setup_check(CFG))   # the lifebuoy in the top bar (core shell.R)
  projects_server("proj", settings_now = function() utils::modifyList(SETTINGS, list(project_dir = CFG$project_dir, viewer_title = CFG$viewer_title, base_offset_h = CFG$base_offset_h)),
                  open = function(path, s) { CFG <<- project_apply_settings(CFG, s, path); projects_inputs(session, s, "base_offset_h"); rv$tick <- rv$tick + 1 })                   # the Projects list (core projects.R)
  run_server(input, output, session, busy_ids = c("s_scan", "s_rescan", "s_build", "scan_force", "x_make", "wb_x_make", "wb_export"))    # scans in the background (core runner.R)
  C <- reactiveVal(load_cat()); sel <- reactiveVal(NULL)
  rv <- reactiveValues(tick = 0)                               # tick: the catalogue was read again (after a build or a scan)
  reload <- function() { C(load_cat()); rv$tick <- isolate(rv$tick) + 1 }
  E <- setup_editors(function(k) cfg_path(CFG$project_dir, k), reactive(rv$tick), function() NULL)
  stamps <- reactivePoll(2000, session, function() paste(cfg_mtime(CFG$project_dir), run_mtime(CFG$project_dir)),
                         function() c(cfg = cfg_mtime(CFG$project_dir), run = run_mtime(CFG$project_dir)))
  stale <- reactive({ s <- stamps(); s[["cfg"]] > s[["run"]] + 1 })           # set-up saved after the last build
  # no catalogue yet (first start, nothing scanned): an empty table with the columns the pages use, so every page still draws
  NO_FLIGHTS <- data.table(flight_id = character(), date = character(), name = character(), aircraft = character(), sensor = character(), n_images = integer(),
                           n_copies = integer(), size_gb = numeric(), positioning_status = character(), qa_flags = character(), source_id = character(), folder = character(),
                           year = character(), site = character(), lat_min = numeric(), lon_min = numeric(), lat_max = numeric(), lon_max = numeric(),
                           kind = character(), kind_found = character(), project = character(), start = character(), duration_min = numeric(), mission_set = character(),
                           display_name = character(), notes = character())
  fl <- reactive({ x <- C(); if (is.null(x)) NO_FLIGHTS else x$flights })

  observe({ f <- fl(); if (!nrow(f)) return()
    for (k in list(c("f_year", "year"), c("f_site", "site"), c("f_aircraft", "aircraft"), c("f_sensor", "sensor"), c("f_status", "positioning_status"), c("f_source", "source_id")))
      updateSelectInput(session, k[1], choices = sort(unique(f[[k[2]]][nzchar(f[[k[2]]])])), selected = isolate(input[[k[1]]])) })
  shown <- reactive({ f <- fl(); if (!nrow(f)) return(f)
    for (k in list(c("f_year", "year"), c("f_site", "site"), c("f_aircraft", "aircraft"), c("f_sensor", "sensor"), c("f_status", "positioning_status"), c("f_source", "source_id")))
      if (length(input[[k[1]]])) f <- f[get(k[2]) %in% input[[k[1]]]]
    if (isTRUE(input$f_checks)) { k <- fchk(); f <- f[flight_id %in% k$flight_id[checks_open(k)]] }   # as the summary counts them (pages/fct_checks.R)
    q <- tolower(trim(input$f_text %||% "")); if (nzchar(q)) f <- f[grepl(q, tolower(paste(name, site, project, folder, notes, sensor, aircraft)), fixed = TRUE)]
    f })
  # safe: the calibration table's editor reports a read problem
  cal_rs <- reactive({ rv$tick; stamps(); tryCatch(load_configs(CFG)$calibration, error = function(e) NULL) })
  fchk_raw <- reactive(flight_checks(fl(), cal_rs()))            # placed, corrected and the five checks of every flight
  decisions <- reactive({ p <- file.path(path.expand(CFG$project_dir), "config", REVIEW_FILE); rv$tick; live_stamp(p); review_read(p) })   # core config.R
  fchk <- reactive(checks_decided(fl(), fchk_raw(), decisions()))  # with the checks accepted as is in Needs you (pages/fct_needs.R)
  mod_flight_summary_server("summary", shown, fchk)
  mod_timeline_strip_server("strip", fl, reactive(input$f_year),
    pick_year = function(y) { s <- input$f_year %||% character(); updateSelectInput(session, "f_year", selected = if (y %in% s) setdiff(s, y) else c(s, y)) },
    open_timeline = function() nav_select("nav", "Timeline"))
  mod_timeline_server("timeline", fl, fchk, reactive(C()$links),
    save_site = function(id, site) {                                         # through the flights.csv editor, as every save (pages/flight.R)
      if (!E$flights$mutate(function(d) as.data.frame(flights_set_site(as.data.table(d), id, site)))) stop("flights.csv was not saved: see the message about it.")
      show_edit(id, flight_row(id)) },                                         # the whole saved row, so a hand-set status stays
    open_flight = function(id) { sel(id); nav_select("nav", "Flight") })
  output$made <- renderText({ x <- C(); if (is.null(x)) "no catalogue yet" else paste("catalogue", format(x$made, "%Y-%m-%d %H:%M"), "\u00b7", CFG$project_dir) })

  # the map: everything shown, plus plans and receivers tied to no flight
  # big imports are not drawn all at once: above these counts the map waits for narrower filters or a click on "Draw all"
  MAP_MAX <- c(flights = 150, footprints = 200, gis = 1500)
  held <- reactiveValues(flights = 0, footprints = 0, gis = 0); mall <- reactiveVal(FALSE)
  observeEvent(C(), mall(FALSE))
  over <- function(k, n) { h <- n > MAP_MAX[[k]] && !mall(); held[[k]] <- if (h) n else 0; h }
  output$map_note <- renderUI({ h <- c(flights = held$flights, footprints = held$footprints, `GIS features` = held$gis); h <- h[h > 0]
    if (!length(h)) return(NULL)
    div(class = "map-note", span(paste0("Not drawn yet: ", paste(num(h), names(h), collapse = ", "), ". Narrow the filters to draw them, or ")),
        actionLink("m_all", "draw everything"), span(" (slow with a large archive).")) })
  observeEvent(input$m_all, mall(TRUE))
  observe({ f <- fold_joined(le_view()); x <- isolate(C()); if (is.null(x)) return()      # a joined flight is one (pages/list_edits.R)
    if (over("flights", nrow(f))) f <- f[0]
    g <- if (nrow(x$geometry)) x$geometry[kind != "derived" & (is.na(flight_id) | flight_id == "" | flight_id %in% f$flight_id)] else data.table()
    p <- x$points[flight_id %in% f$flight_id]
    session$sendCustomMessage("rsmap", list(
      flights = f[, .(id = flight_id, col = unname(STATUS_COL[positioning_status] %|na|% PAL$gray), name, date, year, site, aircraft, sensor, n = n_images,
                      gb = round(size_gb, 1), status = positioning_status, lat_min, lon_min, lat_max, lon_max)],
      pal = unname(c(PAL$blue, PAL$accent, PAL$green, PAL$red, PAL$purple, PAL$teal, PAL$gold, PAL$fg, PAL$fg4, mix_col(PAL$blue, PAL$bone, 0.5))),
      points = lapply(split(p[, .(lat, lon)], p$flight_id), function(d) unname(as.matrix(d))),
      geom = if (nrow(g)) g[, .(id = fifelse(is.na(flight_id), "", flight_id), kind, what, ref, line)] else list(), fit = FALSE)) })
  observeEvent(input$map_pick, sel(input$map_pick))
  observeEvent(sel(), session$sendCustomMessage("rsmap_select", sel()))
  cur <- reactive({ id <- sel(); f <- fl(); if (is.null(id) || !nrow(f)) NULL else f[flight_id == id] })

  output$detail <- renderUI({ f <- cur(); if (is.null(f) || !nrow(f)) return(p(class = "muted", "Click a flight on the map or in the table."))
    lk <- C()$links[flight_id == f$flight_id & state != "suggested"]
    tagList(h3(f$name), div(class = "mb-2", badge(f$positioning_status)), p(class = "muted", f$positioning_evidence),
      fact(paste0("Date (", zone_text(BASE_OFFSET), ")"), f$start), fact("Time from", f$time_source), fact("Flight log start (UTC)", f$log_start_utc %||% NA), fact("Flown track (KML) start (UTC)", f$kml_start_utc %||% NA), fact("Duration (min)", f$duration_min), fact("Aircraft", f$aircraft), fact("Sensor", f$sensor),
      fact("Multispectral", if (isTRUE(f$multispectral)) paste0("yes; ", if (isTRUE(f$irradiance_sensor)) "irradiance sensor readings in the images" else "no irradiance sensor readings")),
      fact("Calibration panel", if (isTRUE(f$multispectral) || isTRUE(!is.na(f$panel_id[1]) & nzchar(f$panel_id[1]))) (f$panel_id %|na|% "not recorded")),
      fact("Aircraft serial", f$aircraft_serial), fact("Flight number", f$flight_no), fact("Images", num(f$n_images)),
      fact("Captures", if (isTRUE(f$n_captures != f$n_images)) num(f$n_captures)), fact("Size (GB)", f$size_gb), fact("Project", f$project), fact("Site", f$site),
      fact("Mission", if (isTRUE(f$mission_parts > 1)) paste0("sortie ", f$mission_part, " of ", f$mission_parts, if (f$mission_status == "uncertain") " (uncertain)" else "")),
      { k <- fchk()[flight_id == f$flight_id]; if (nrow(k)) div(class = "fact", span(class = "k", "Checks"), span(class = "v", HTML(checks_html(k)), br(), span(class = "muted", k$chk_why))) },   # tags, so not fact() (one plain value)
      fact("Folder", paste0(f$source_id, ":", f$folder)), fact("Copies", f$copies), fact("QA", f$qa_flags), fact("Notes", f$notes),
      if (nrow(lk)) tagList(h3(class = "mt-3", "Linked"), lapply(seq_len(min(nrow(lk), 12)), function(i) div(class = "fact", span(class = "k", lk$role[i]), span(class = "v", lk$name[i], br(), span(class = "muted", lk$state[i]))))),
      actionLink("go_flight", "Edit this flight and its links"))
  })
  observeEvent(input$go_flight, nav_select("nav", "Flight"))

  # Each page of the app, in this order (RS/pages/; they share this function's variables)
  source(file.path(APP_DIR, "pages", "flight.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "missions.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "list_edits.R"), local = TRUE)   # the lists under the map, Site names and the Save bar
  source(file.path(APP_DIR, "pages", "qa.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "sources.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "imagery_datasets.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "gis_layers.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "gnss_files.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "browse.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "set_up_check.R"), local = TRUE)
  invisible(list(E = E, C = C, sel = sel, rv = rv, stale = stale, chk = chk, fchk = fchk, fchk_raw = fchk_raw, reload = reload, show_edit = show_edit, save_flight = save_flight, flight_row = flight_row, busy = run_busy))
}

# Two interfaces on one project: the Standard one above (the default) and the Workbench (preview, workbench/wb_rs.R).
# ?ui=workbench in the address opens the Workbench and ?ui=standard the Standard one; with neither, OVID_UI and then
# the "ui" setting decide (core/workbench.R).
source(file.path(APP_DIR, "workbench", "wb_rs.R"), local = TRUE)
shinyApp(ui = function(req) if (wb_wanted(req$QUERY_STRING, SETTINGS$ui)) wb_ui(req) else ui,
         server = function(input, output, session) {
           if (!wb_wanted(isolate(session$clientData$url_search), SETTINGS$ui)) return(server(input, output, session))
           wb_server(input, output, session, server(input, output, session, embedded = TRUE)) })
