# =============================================================================
#  app.R  --  Shiny front end for cs_pipeline.R
# =============================================================================
#  Keep this file in the same folder as cs_pipeline.R, then:
#      shiny::runApp("path/to/cs_app")
#  (or open app.R in RStudio and press "Run App").
#
#  The app does not re-implement anything: it calls the functions in
#  cs_pipeline.R and edits the same config CSVs, so the audit guarantees are
#  identical whether you run from here or from the command line.
# =============================================================================

for (p in c("shiny", "DT", "data.table", "ggplot2", "nanoparquet", "zip"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(DT) })

APP_DIR <- getwd()
if (!file.exists(file.path(APP_DIR, "cs_pipeline.R")))
  stop("cs_pipeline.R must sit in the same folder as app.R")
Sys.setenv(CS_NO_RUN = "1")                       # load the functions, don't run yet
source(file.path(APP_DIR, "cs_pipeline.R"), local = TRUE)
log_app_errors()                                   # errors outside a run go to ~/.ovid/logs (core manifest.R)
source(file.path(CORE_DIR, "shell.R"), local = TRUE)
source(file.path(CORE_DIR, "table.R"), local = TRUE)        # Ovid/table.R: the core table (ovid_dt)
source(file.path(CORE_DIR, "reference.R"), local = TRUE)    # Ovid/reference.R: the Reference page (the guide as a searchable index)
source(file.path(CORE_DIR, "convert_page.R"), local = TRUE) # Ovid/convert_page.R: the Convert page (units, DMS, UTM)
source(file.path(CORE_DIR, "grouping.R"), local = TRUE)     # Ovid/grouping.R: group the logger list by serial, deployment, download or file
REF_GO <- list("Calibration" = list(go = "Quality rules", tab = "Calibrations"), "Quality|removal|Flag" = list(go = "Quality rules"), "Clock|Time" = list(go = "Clock checks"), "Station" = list(go = "Stations"), "precipitation" = list(go = "Precipitation"), "Derived|Daily|summar" = list(go = "Explore", tab = "Summaries"), "Finding|Reading" = list(go = "Files"))   # where each reference topic is set
source(file.path(CORE_DIR, "tsview.R"), local = TRUE)       # Ovid/tsview.R: the Standard view's time-series plot
source(file.path(CORE_DIR, "flow.R"), local = TRUE)         # Ovid/flow.R: the set-up flow (unsaved changes, what shows, Check the data)
source(file.path(CORE_DIR, "runner.R"), local = TRUE)       # Ovid/runner.R: runs in the background, with progress and Cancel
source(file.path(CORE_DIR, "projects.R"), local = TRUE)     # Ovid/projects.R: the Projects list (new, open, delete to the Trash)
source(file.path(CORE_DIR, "rename.R"), local = TRUE)       # Ovid/rename.R: rename a site or station in every set-up table
source(file.path(CORE_DIR, "files.R"), local = TRUE)        # Ovid/files.R: Browse buttons and export names
source(file.path(CORE_DIR, "editor.R"), local = TRUE)       # Ovid/editor.R: the set-up table editor
source(file.path(CORE_DIR, "metasheet.R"), local = TRUE)    # Ovid/metasheet.R and metasheet_ui.R: Paste and Import in every table editor
source(file.path(CORE_DIR, "import.R"), local = TRUE)           # Ovid/import.R: reading a spreadsheet, matching columns, checking rows
source(file.path(CORE_DIR, "metasheet_ui.R"), local = TRUE)
source(file.path(APP_DIR, "home.R"), local = TRUE)          # CS/home.R: the Standard home (before import, and with data)
downloadHandler <- ovid_downloads("CS")                  # every download is named ovid_cs_<what>_<date>.<ext>

# Route the pipeline's console messages into the app (log panel + progress bar)
LOG <- new.env(); LOG$lines <- character()
say <- function(..., col = "fg") {
  msg <- paste0(...)
  LOG$lines <- c(LOG$lines, msg); cat(msg, "\n", sep = "")
  if (grepl("^\\[[0-9]/6\\]", msg) && !is.null(getDefaultReactiveDomain()))
    try(setProgress((as.numeric(substr(msg, 2, 2)) - 0.5) / 6, detail = sub("^\\[[0-9]/6\\] ", "", msg)), silent = TRUE)
}

SETTINGS_FILE <- file.path(APP_DIR, "app_settings.json")
SETTINGS <- settings_read(SETTINGS_FILE)                          # core util.R: the last good copy if this one is damaged
HAS_OUTPUT <- length(SETTINGS$project_dir) == 1 && file.exists(file.path(path.expand(SETTINGS$project_dir), "output", "audit", "audit_index.csv"))
for (k in c("raw_dirs", "project_dir", "skip_tables", "import_cr10x"))
  if (is.null(SETTINGS[[k]])) SETTINGS[[k]] <- CFG[[k]]
if (is.null(SETTINGS$prog_dir) || !nzchar(SETTINGS$prog_dir)) SETTINGS$prog_dir <- SETTINGS$fsl_dir %||% ""   # one programs folder: an old CR10X FSL folder becomes it
if (is.null(SETTINGS$station_format)) SETTINGS$station_format <- CFG$station_format
if (!is.null(SETTINGS$base_offset_h)) BASE_OFFSET <- SETTINGS$base_offset_h   # Settings > Time zone
if (nzchar(Sys.getenv("CS_PROJECT_DIR"))) SETTINGS$project_dir <- Sys.getenv("CS_PROJECT_DIR")
SETTINGS$project_dir <- ovid_project_dir(SETTINGS$project_dir)   # none set: ~/Desktop/Ovid_Projects/<app>/Untitled (core util.R)
# Viewer mode: colleagues can look at Metadata, Explore and Audit, but cannot
# change the set-up or run the pipeline. Turned on by CS_VIEWER=1 or by the
# app_settings.json that "Build viewer bundle" writes.
VIEWER <- identical(Sys.getenv("CS_VIEWER"), "1") || isTRUE(SETTINGS$viewer)
LOCAL <- identical(Sys.getenv("CS_LOCAL"), "1")                                   # started from Ovid_CS.bat
OPEN_SESSIONS <- new.env(); OPEN_SESSIONS$n <- 0L

source(file.path(APP_DIR, "pages", "fct_outputs.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_tables.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_setup.R"), local = TRUE)

# =============================================================================
#  UI
# =============================================================================
source(file.path(APP_DIR, "pages", "ui.R"), local = TRUE)   # CS/pages/ui.R: the pages, the Standard view and ui

# =============================================================================
#  Server
# =============================================================================
# embedded = TRUE: running inside the Workbench (workbench/wb_cs.R), which shows these pages in its own frame and gets the
# set-up table editors, outputs and run state returned at the end. A run there stays on the page it was started from.
server <- function(input, output, session, embedded = FALSE) {
  reference_server(input, output, session, "CS", REF_GO)        # core reference.R
  convert_server(input, output)                                             # core convert_page.R
  time_zone_server(input)                                                  # Settings > Time zone (core shell.R)
  colours_server(input, output, "CS")                                       # Settings > Colours (core shell.R)
  troubleshoot_server(output, function() isolate(pd()), function() isolate(preflight(current_cfg())))   # the lifebuoy in the top bar (core shell.R)
  projects_server("proj", settings_now = function() utils::modifyList(SETTINGS, c(current_cfg()[c("raw_dirs", "project_dir", "skip_tables", "import_cr10x", "station_format", "incremental")],
                    list(prog_dir = trim(input$prog_dir %||% ""), base_offset_h = BASE_OFFSET))),
                  open = function(path, s) projects_inputs(session, s, c("raw_dirs", "project_dir", "skip_tables", "import_cr10x", "station_format", "incremental", "prog_dir", "base_offset_h",
                                                                       names(Filter(function(x) isTRUE(x$panel), setNames(CORRECTIONS, vapply(CORRECTIONS, `[[`, "", "setting"))))),
                                                            join = c(raw_dirs = "\n", skip_tables = ", ")))   # the Projects list (core projects.R)
  corrections_server("corr", function() current_cfg())          # the Automatic corrections list, a step of the set-up flow (core corrections.R)
  run_server(input, output, session, busy_ids = c("run", "run_force", "rf_rescan", "pr_rerun", "xp_dl", "xp_pdf", "wb_export"))                       # runs in the background (core runner.R)
  # Browse: the system's folder picker (Finder, Explorer) or the in-app folder browser (Ovid/files.R)
  ovid_browse(input, output, session, c("raw_dirs", "project_dir", "prog_dir"), add = "raw_dirs", places = function() c(Project = SETTINGS$project_dir))
  observeEvent(input$size_stored, { if (isTRUE(input$size_stored %in% c("compact", "normal", "tall"))) updateSelectInput(session, "plot_size", selected = input$size_stored) }, once = TRUE)
  psz <- reactive(switch(input$plot_size %||% "normal", compact = 0.75, tall = 1.45, 1))
  rv <- reactiveValues(au_rule = NULL, pf_tick = 0, tick = 0, log = character(), stale = FALSE, fsl_tick = 0, prog_tick = 0, zoom = NULL, detail = NULL, ex_focus = NULL, ex_send = 0)
  pd <- reactive(if (VIEWER) normalizePath(path.expand(SETTINGS$project_dir), winslash = "/", mustWork = FALSE)
                 else sub("[/\\\\]+$", "", path.expand(ovid_project_dir(input$project_dir %|% SETTINGS$project_dir))))   # none set: the default folder (core util.R)
  dataset_card_server(input, output, pd)                                    # Settings > Dataset (core shell.R)
  cfg_path <- function(f) reactive(file.path(pd(), "config", f))
  tick <- reactive(rv$tick)
  mark <- function() rv$stale <- TRUE
  go <- function(tab) ovid_go(session, if (embedded && tab %in% c("Home", "Project")) "wb-import" else tab)   # the Workbench keeps the folders under Import data
  note <- function(..., type = "message", duration = 5) showNotification(paste0(...), type = type, duration = duration)
  safely <- function(expr, what) tryCatch(expr, error = function(e) { note(what, ": ", conditionMessage(e), type = "error", duration = NULL); NULL })
  # initials: asked once per session and filled in on every entry (field log; the Your initials fields start with them)
  who <- reactiveVal(""); who_then <- NULL
  with_initials <- function(then) { if (nzchar(who())) return(then(who())); who_then <<- then
    showModal(modalDialog(title = "Your initials", p(class = "dim", "Asked once per session: every entry you add carries them."),
                          textInput("who_initials", NULL, width = "100%"), footer = tagList(modalButton("Cancel"), actionButton("who_ok", "OK", class = "btn-primary")), easyClose = TRUE)) }
  observeEvent(input$who_ok, { v <- trim(input$who_initials); if (!nzchar(v)) return(note("Type your initials first.", type = "warning"))
    who(v); removeModal(); f <- who_then; who_then <<- NULL; if (is.function(f)) f(v) })
  for (id in c("ckf_author", "br_author", "rf_author")) local({ id <- id; observeEvent(input[[id]], { v <- trim(input[[id]]); if (nzchar(v)) who(v) }) })

  E <- setup_editors(cfg_path, tick, mark)
  E$manual <- period_view(E$period, "remove", cs_manual_to_period, cs_period_to_manual)      # removals and calibrations are rows of the
  E$cal <- period_view(E$period, c("scale", "offset"), cs_cal_to_period, cs_period_to_cal)   # corrections over a period (core periods.R)
  rename_server("ren", project_dir = pd, on_change = function() { rv$tick <- rv$tick + 1; mark() })   # core rename.R
  dcheck <- deploy_check_server("depcheck", pd, list(file = "output/audit/file_manifest.csv", serial = "logger_id", name = "station_name", file_col = "path"),   # core metasheet_ui.R
                      tick, on_change = function() { rv$tick <- rv$tick + 1; mark() })
  m_st <- E$stations; m_al <- E$aliases; m_qc <- E$qc; m_mn <- E$manual; m_cx <- E$cr10x; m_gr <- E$groups; m_tc <- E$timecorr
  m_cal <- E$cal; m_pi <- E$pinfo; m_pg <- E$gauges; m_si <- E$sites; m_fl <- E$fieldlog; m_fr <- E$filerules
  observeEvent(input$qc_suggest, { n <- 0L
    ok <- m_qc$mutate(function(d) { r <- qc_fill_suggested(d); n <<- r$filled; as.data.frame(r$table[, intersect(c(names(d), QC_COLS), names(r$table)), with = FALSE]) })
    showNotification(if (!isTRUE(ok)) "The table is not loaded yet." else if (n) sprintf("Filled %d blank cell(s). Run the pipeline to apply.", n) else "Nothing to fill: the standard rows already have spike and step limits.", type = "message") })
  info <- reactive({
    d <- m_pi$data(); d <- if (is.null(d) || !nrow(d)) default_parameter_info() else ensure_cols(as.data.table(d), INFO_COLS)
    ok <- vapply(d$pattern, function(p) nzchar(p) && !inherits(try(grepl(p, "x", perl = TRUE), silent = TRUE), "try-error"), logical(1))
    d[ok]
  })
  info_of <- function(p) { i <- info(); i[param_info_row(p, i)] }     # one row per parameter, NA where the guide has nothing

  current_cfg <- function() {
    cfg <- CFG
    cfg$raw_dirs <- trim(strsplit(input$raw_dirs, "\n")[[1]]); cfg$raw_dirs <- cfg$raw_dirs[nzchar(cfg$raw_dirs)]
    cfg$project_dir  <- trim(input$project_dir)
    cfg$skip_tables  <- trim(strsplit(input$skip_tables, ",")[[1]])
    cfg$import_cr10x <- isTRUE(input$import_cr10x)
    cfg$incremental  <- isTRUE(input$incremental)
    cfg$station_format <- input$station_format %||% "csv"
    cfg$make_plots   <- FALSE                      # the app draws its own
    cfg$base_offset_h <- BASE_OFFSET               # Settings > Time zone (core shell.R): the zone of stations with none of their own
    corrections_cfg(cfg)                           # switches on the Automatic corrections list (core corrections.R)
  }
  save_settings <- function(cfg) update_settings(c(cfg[c("raw_dirs", "project_dir", "skip_tables", "import_cr10x", "station_format", "incremental")],   # other saved settings are kept
                                                   list(prog_dir = trim(input$prog_dir %||% ""))))
  settings_autosave(c("raw_dirs", "project_dir", "skip_tables", "import_cr10x", "station_format", "incremental", "prog_dir"),
                    function() save_settings(current_cfg()))   # core editor.R: saved when changed, not at the next run
  folders_ok <- function(cfg) {
    if (!length(cfg$raw_dirs) || !nzchar(cfg$project_dir)) {
      note("Set the raw folders and the project folder first", if (embedded) " (Import data)." else " (Project step).", type = "warning"); go("Project"); return(FALSE) }
    if (!any(dir.exists(path.expand(cfg$raw_dirs)))) {
      note("None of the raw folders exist. Check the paths", if (embedded) " under Import data." else " on the Project step.", type = "warning"); go("Project"); return(FALSE) }
    TRUE
  }

  # Each page of the app, in this order (CS/pages/; they share this function's variables)
  source(file.path(APP_DIR, "pages", "run.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "folders.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "stations.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "logger_programs.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "cr10x.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "column_names.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "metadata.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_one_station.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_neighbours.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "precipitation.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "raw_files.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_summaries.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "sites_and_field_log.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "combined_records.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "station_map.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "configuration.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "history.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "export.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "hub.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "sensor_health.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "set_up_check.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "logger_history.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_compare_stations.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_availability.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "clock_checks.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "parameter_guide.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "audit.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "setup_flow.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "grouping.R"), local = TRUE)
  invisible(list(E = E, outs = outs, pd = pd, rv = rv, mark = mark, pf = pf, needs = dcheck, review = review))
}

# Two interfaces on one project: the Standard one above (the default) and the Workbench (preview, workbench/wb_cs.R).
# ?ui=workbench in the address opens the Workbench and ?ui=standard the Standard one; with neither, OVID_UI and then
# the "ui" setting decide (core/workbench.R).
source(file.path(APP_DIR, "workbench", "wb_cs.R"), local = TRUE)
shinyApp(ui = function(req) if (wb_wanted(req$QUERY_STRING, SETTINGS$ui)) wb_ui(req) else ui,
         server = function(input, output, session) { track_session(session)
           if (!wb_wanted(isolate(session$clientData$url_search), SETTINGS$ui)) return(server(input, output, session))
           wb_server(input, output, session, server(input, output, session, embedded = TRUE)) })
