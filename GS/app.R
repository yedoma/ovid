# =============================================================================
#  app.R  --  Shiny front end for gs_pipeline.R  (Ovid Core // GS)
# =============================================================================
#      shiny::runApp("path/to/Ovid_Apps/GS")      (or double-click "Open GS (Mac).command")
#  Reads the catalogue written by gs_pipeline.R (Scan on Home, or Rscript gs_pipeline.R). The pages (pages/ui.R):
#    Home     the folders, Scan, and counts of what the scan found
#    Archive  every survey, filtered by year, folder, instrument and quality; a row opens the survey
#    Stack    a project's surveys side by side on one colour scale, with Compare
#    Map      transects on a map, with their details
#    GPR      MALA radar lines: Map (with the folders and every line found) and Processing (viewing only)
#    Survey   what it is, its files, QC result, pseudosection, and the metadata people enter (saved to config/surveys.csv)
#    QC       sliders for every limit, re-flagging the survey live; "Save profile" adds rows to config/qc_profiles.csv
#    Set-up tables  every table in config/, with the editor shared by all the apps (core/editor.R)
#    Convert, Reference and Settings (colours, the catalogue's QC profile, the set-up check)
#  Two interfaces on one project: these pages (Standard) and the Workbench (preview, workbench/wb_gs.R), which shows
#  them around a grid of one survey's measurements. Measurements are never changed. Theme: kanagawa.nvim (vimcolorschemes).
# =============================================================================

for (p in c("shiny", "bslib", "DT", "data.table", "ggplot2", "plotly"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(bslib); library(DT) })

APP_DIR <- getwd()
Sys.setenv(GS_NO_RUN = "1")
source(file.path(APP_DIR, "gs_pipeline.R"), local = TRUE)
log_app_errors()                                   # errors outside a run go to ~/.ovid/logs (core manifest.R)
source(file.path(APP_DIR, "R", "views.R"), local = TRUE)
source(file.path(APP_DIR, "R", "reference.R"), local = TRUE)
source(file.path(APP_DIR, "R", "maps.R"), local = TRUE)
source(file.path(APP_DIR, "R", "gpr.R"), local = TRUE)
source(file.path(CORE_DIR, "shell.R"), local = TRUE)
source(file.path(CORE_DIR, "table.R"), local = TRUE)        # Ovid/table.R: the core table (ovid_dt)
cfg_mtime_ignore(c("gpr_picks.csv", "gpr_markers.csv", "wb_stack_view.csv", "gpr_flows"))   # files a rebuild does not read: no "Set-up changed" notice (core shell.R)
source(file.path(CORE_DIR, "reference.R"), local = TRUE)
source(file.path(CORE_DIR, "convert_page.R"), local = TRUE) # Ovid/convert_page.R: the Convert page (units, DMS, UTM)
source(file.path(CORE_DIR, "grouping.R"), local = TRUE)     # Ovid/grouping.R: surveys grouped by instrument, stay, download or file    # Ovid/reference.R: the Reference page (the guide as a searchable index)
REF_GO <- list()   # where each reference topic is set
source(file.path(CORE_DIR, "runner.R"), local = TRUE)       # Ovid/runner.R: runs in the background, with progress and Cancel       # Ovid/shell.R: the shared page shell
source(file.path(CORE_DIR, "projects.R"), local = TRUE)     # Ovid/projects.R: the Projects list (new, open, delete to the Trash)
source(file.path(CORE_DIR, "files.R"), local = TRUE)        # Ovid/files.R: Browse buttons and export names
source(file.path(CORE_DIR, "editor.R"), local = TRUE)       # Ovid/editor.R: the set-up table editor
source(file.path(CORE_DIR, "metasheet.R"), local = TRUE)    # Ovid/metasheet.R and metasheet_ui.R: Paste and Import in every table editor
source(file.path(CORE_DIR, "import.R"), local = TRUE)           # Ovid/import.R: reading a spreadsheet, matching columns, checking rows
source(file.path(CORE_DIR, "metasheet_ui.R"), local = TRUE)
source(file.path(CORE_DIR, "longtable.R"), local = TRUE)        # Ovid/longtable.R: long tables drawn a screen at a time
source(file.path(CORE_DIR, "rename.R"), local = TRUE)           # Ovid/rename.R: rename a site in every set-up table that names it
source(file.path(CORE_DIR, "report.R"), local = TRUE)           # Ovid/report.R: the report blocks the Simple report is built from
source(file.path(CORE_DIR, "simple.R"), local = TRUE)           # Ovid/simple.R: the Simple view's frame, Edit step and report
downloadHandler <- ovid_downloads("GS")                  # every download is named ovid_gs_<what>_<date>.<ext>
SETTINGS_FILE <- file.path(APP_DIR, "app_settings.json")
SETTINGS <- settings_read(SETTINGS_FILE)                          # core util.R: the last good copy if this one is damaged
for (k in intersect(names(SETTINGS), c("raw_dirs", "project_dir", "catalogue_profile", "base_offset_h"))) CFG[[k]] <- SETTINGS[[k]]
CFG$project_dir <- ovid_project_dir(CFG$project_dir)             # none set: ~/Projects/Ovid/Projects/<app>/project (core util.R)
VIEWER <- FALSE                                     # GS has no viewer mode: an imported catalogue is changed in its own copy
set_paths <- function() { PROJECT <<- path.expand(CFG$project_dir); CAT <<- file.path(PROJECT, "output", "catalogue") }
set_paths()
HOME_PROJECT <- CFG$project_dir                                              # the user's own catalogue; imported ones are opened beside it
is_imported <- function() file.exists(file.path(PROJECT, "IMPORTED.txt"))
has_meas <- function(sid) file.exists(file.path(PROJECT, "output", "surveys", sid, "measurements.csv"))
if (!file.exists(file.path(CAT, "surveys.csv")))                                 # first start: build the catalogue; with nothing to read
  tryCatch(run_pipeline(CFG), error = function(e) message("No catalogue yet: ", conditionMessage(e)))   # the app opens empty (Sources page) instead of stopping

source(file.path(APP_DIR, "pages", "fct_outputs.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_tables.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_setup.R"), local = TRUE)

# ---- UI ------------------------------------------------------------------------------------------------------------------
source(file.path(APP_DIR, "pages", "mod_archive.R"), local = TRUE)   # GS/pages/mod_archive.R: the Archive page (a module)
source(file.path(APP_DIR, "pages", "fct_stack.R"), local = TRUE)     # GS/pages/fct_stack.R: the figures behind Home and Stack
source(file.path(APP_DIR, "pages", "mod_home.R"), local = TRUE)      # GS/pages/mod_home.R: the Home page (a module)
source(file.path(APP_DIR, "pages", "mod_stack.R"), local = TRUE)     # GS/pages/mod_stack.R: the Stack page (a module)
source(file.path(APP_DIR, "pages", "fct_gpr_flow.R"), local = TRUE)  # GS/pages/fct_gpr_flow.R: a GPR line's saved flow, markers added by hand, picks
source(file.path(APP_DIR, "pages", "mod_gpr_map.R"), local = TRUE)   # GS/pages/mod_gpr_map.R: the GPR Map page (a module)
source(file.path(APP_DIR, "pages", "mod_gpr_flow.R"), local = TRUE)  # GS/pages/mod_gpr_flow.R: GPR processing (a module), in both interfaces
source(file.path(APP_DIR, "pages", "ui.R"), local = TRUE)   # GS/pages/ui.R: the pages and ui

# ---- server --------------------------------------------------------------------------------------------------------------
# embedded = TRUE: running inside the Workbench (workbench/wb_gs.R), which shows these pages in its own frame and gets the
# set-up table editors, the catalogue and the rebuild state returned at the end.
server <- function(input, output, session, embedded = FALSE) {
  reference_server(input, output, session, "GS", REF_GO)        # core reference.R
  convert_server(input, output)                                             # core convert_page.R
  time_zone_server(input, on_change = function(z) CFG$base_offset_h <<- z)   # Settings > Time zone (core shell.R)
  colours_server(input, output, "GS")                                       # Settings > Colours (core shell.R)
  troubleshoot_server(output, function() CFG$project_dir, function() setup_check(CFG))   # the lifebuoy in the top bar (core shell.R)
  projects_server("proj", settings_now = function() utils::modifyList(SETTINGS, list(raw_dirs = CFG$raw_dirs, project_dir = CFG$project_dir, base_offset_h = CFG$base_offset_h)),
                  open = function(path, s) { CFG$raw_dirs <<- as.character(unlist(s$raw_dirs)); HOME_PROJECT <<- path; switch_project(path); projects_inputs(session, s, "base_offset_h")
                    updateTextAreaInput(session, "set_raw", value = paste(CFG$raw_dirs, collapse = "\n")); updateTextInput(session, "set_project", value = path) })   # the Projects list (core projects.R)
  run_server(input, output, session, busy_ids = c("rebuild", "rebuild_force", "dl_archive", "dl_survey", "wb_export"))                               # rebuilds in the background (core runner.R)
  rv <- reactiveValues(tick = 0, log = character())          # tick: a rebuild finished (in this window, or another one the Workbench noticed)
  proj <- reactiveVal(CFG$project_dir)                         # the catalogue shown (an imported one is opened beside the user's own)
  E <- setup_editors(function(k) cfg_path(proj(), k), reactive(c(rv$tick, proj())), function() mark())
  wb_made <- function(ed) { if (ed$hand_ok()) return(TRUE)   # flags, electrodes, elevations and QC profiles change only in the Workbench (core editor.R wb_only)
    showNotification("Flags, electrodes turned off, elevations and QC profiles are changed in the Workbench (the switch at the top right).", type = "warning"); FALSE }
  if (!embedded) { q0 <- parseQueryString(isolate(session$clientData$url_search) %||% "")             # a link from the Simple view opens its page (?page=QC)
    if (!is.null(q0$page)) session$onFlushed(function() ovid_go(session, q0$page), once = TRUE) }
  stamps <- reactivePoll(2000, session, function() paste(cfg_mtime(PROJECT), run_mtime(PROJECT)), function() c(cfg = cfg_mtime(PROJECT), run = run_mtime(PROJECT)))
  stale <- reactive({ s <- stamps(); s[["cfg"]] > s[["run"]] + 1 })           # set-up saved after the last rebuild
  # a table saved on Set-up tables (or by the Workbench): what the pages show from it is read again
  mark <- function() { cf <- load_configs(CFG); qc(cf$qc); ELEV(cf$elevations); transects(cf$transects); eo_tick(eo_tick() + 1); cat_s(load_catalogue()) }
  observeEvent(rv$tick, { cat_s(load_catalogue()); qc(load_configs(CFG)$qc) }, ignoreInit = TRUE)
  # Browse: the system's folder picker (Finder, Explorer) or the in-app folder browser (Ovid/files.R)
  ovid_browse(input, output, session, c("set_raw", "set_project", "gpr_dirs"), add = c("set_raw", "gpr_dirs"), places = function() c(Project = HOME_PROJECT))
  settings_autosave(c("set_raw", "set_project"), keys = c(set_raw = "raw_dirs", set_project = "project_dir"), save = function() {   # core editor.R: the folders saved when changed; the catalogue switches at the next rebuild
    r <- trim(strsplit(input$set_raw %||% "", "\n")[[1]]); SETTINGS <<- utils::modifyList(SETTINGS, list(raw_dirs = r[nzchar(r)], project_dir = trim(input$set_project %||% "")))
    settings_save(SETTINGS_FILE, SETTINGS[c("raw_dirs", "project_dir")]) })

  # Each page of the app, in this order (GS/pages/; they share this function's variables)
  source(file.path(APP_DIR, "pages", "ground_radar.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "set_up_check.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "survey.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "quality_control.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "map_and_transects.R"), local = TRUE)
  open_survey <- function(sid) { updateSelectInput(session, "survey", selected = sid); nav_select("nav", "Survey", session = session) }
  open_import <- function() { nav_select("nav", "Setup", session = session); nav_select("setup_tab", "transects", session = session) }   # transects.csv's Import
  # safe: an output of the last scan, only shown
  stats <- reactive({ cat_s(); f <- file.path(CAT, "survey_stats.csv"); if (file.exists(f)) tryCatch(fread(file = f), error = function(e) NULL) })
  stack_pick <- reactiveVal("")
  mod_archive_server("archive", cat_s, transects, open_survey = open_survey)
  mod_home_server("home", cat_s, stats, transects, open_import = open_import)
  stk <- mod_stack_server("stack", cat_s, stats, transects, stack_pick, read_meas, has_meas, open_survey = open_survey, open_import = open_import, palette = reactive(input$v_pal))
  ovid_areas_server(reactive(c(ERT = { s <- cat_s(); if (is.null(s) || !nrow(s)) 0L else sum(s$kind %in% c("original", "merged")) }, GPR = nrow(gpr_ok()))), proj)   # the ERT | GPR switch (core shell.R)
  invisible(list(E = E, rv = rv, stale = stale, proj = proj, cat_s = cat_s, qc = qc, eo_tick = eo_tick, chk = chk, mark = mark, stats = stats, transects = transects,
                 gpr_ok = gpr_ok, gpr_tracks = gpr_tracks, gpr_tick = gpr_tick, cat_profile = cat_profile, cmp_kept = stk$cmp_kept))
}

# Three views of one project: Simple (simple/simple_gs.R), Standard (above) and the Workbench (workbench/wb_gs.R).
# ?ui=simple, ?ui=standard or ?ui=workbench in the address opens one; with none, OVID_UI and then the "ui" setting
# decide (core/workbench.R).
source(file.path(APP_DIR, "workbench", "wb_gs.R"), local = TRUE)
source(file.path(APP_DIR, "simple", "simple_gs.R"), local = TRUE)
# An install whose settings are still in the older app_settings.rds is not new: it keeps Standard, and nothing is written for it.
FIRST_VIEW_FILE <- if (file.exists(sub("\\.json$", ".rds", SETTINGS_FILE))) NULL else SETTINGS_FILE
if (is.null(SETTINGS$ui)) SETTINGS$ui <- ovid_first_view(FIRST_VIEW_FILE)   # a new install opens in Simple (core workbench.R)
shinyApp(ui = function(req) switch(ovid_view(req$QUERY_STRING, SETTINGS$ui), simple = simple_ui(req), workbench = wb_ui(req), ui),
         server = function(input, output, session) {
           v <- ovid_first_view_keep(FIRST_VIEW_FILE, ovid_view(isolate(session$clientData$url_search), SETTINGS$ui))   # a new install keeps Simple (core workbench.R)
           if (v == "simple") return(simple_server(input, output, session, server(input, output, session, embedded = TRUE)))
           if (v == "workbench") return(wb_server(input, output, session, server(input, output, session, embedded = TRUE)))
           server(input, output, session) })
