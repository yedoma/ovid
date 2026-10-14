# =============================================================================
#  app.R  --  Shiny front end for gt_pipeline.R  (Ovid Core // GT)
# =============================================================================
#  Keep this file in the same folder as gt_pipeline.R and the R/ folder, then:
#      shiny::runApp("path/to/ovid_GT")
#  (or open app.R in RStudio and press "Run App", or double-click Ovid_GT.bat on Windows).
#
#  The app does not re-implement anything: it calls the functions in gt_pipeline.R and edits the same set-up CSVs,
#  so the audit guarantees are identical whether you run from here or from the command line.
# =============================================================================

for (p in c("shiny", "DT", "data.table", "ggplot2", "readxl", "zip"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(DT) })

APP_DIR <- getwd()
if (!file.exists(file.path(APP_DIR, "gt_pipeline.R"))) stop("gt_pipeline.R must sit in the same folder as app.R")
Sys.setenv(GT_NO_RUN = "1")
source(file.path(APP_DIR, "gt_pipeline.R"), local = TRUE)
log_app_errors()                                   # errors outside a run go to ~/.ovid/logs (core manifest.R)
source(file.path(CORE_DIR, "shell.R"), local = TRUE)
source(file.path(CORE_DIR, "table.R"), local = TRUE)        # Ovid/table.R: the core table (ovid_dt)
source(file.path(CORE_DIR, "reference.R"), local = TRUE)
source(file.path(CORE_DIR, "convert_page.R"), local = TRUE) # Ovid/convert_page.R: the Convert page (units, DMS, UTM)
source(file.path(CORE_DIR, "grouping.R"), local = TRUE)     # Ovid/grouping.R: group the logger list by serial, deployment, download or file    # Ovid/reference.R: the Reference page (the guide as a searchable index)
REF_GO <- list("Sensor|depth" = list(go = "Sensors", tab = "Sensors"), "Quality|flag" = list(go = "QC"), "Clock|Time" = list(go = "Clock"), "Thaw|Active" = list(go = "Summ", tab = "B. Active-layer thickness"), "Freeze|zero curtain" = list(go = "Summ", tab = "C. Freeze / thaw and zero curtain"), "Between" = list(go = "Compare"), "Soil" = list(go = "Soil"))   # where each reference topic is set
source(file.path(CORE_DIR, "tsview.R"), local = TRUE)       # Ovid/tsview.R: the Standard view's time-series plot
source(file.path(CORE_DIR, "flow.R"), local = TRUE)         # Ovid/flow.R: the set-up flow (unsaved changes, what shows, Check the data)
source(file.path(CORE_DIR, "runner.R"), local = TRUE)       # Ovid/runner.R: runs in the background, with progress and Cancel
source(file.path(CORE_DIR, "projects.R"), local = TRUE)     # Ovid/projects.R: the Projects list (new, open, delete to the Trash)
source(file.path(CORE_DIR, "rename.R"), local = TRUE)       # Ovid/rename.R: rename a site or station in every set-up table
source(file.path(CORE_DIR, "files.R"), local = TRUE)        # Ovid/files.R: Browse buttons and export names
source(file.path(CORE_DIR, "editor.R"), local = TRUE)       # Ovid/editor.R: the set-up table editor
source(file.path(CORE_DIR, "metasheet_ui.R"), local = TRUE) # Ovid/metasheet_ui.R: the logger list (metadata sheet)
downloadHandler <- ovid_downloads("GT")                  # every download is named ovid_gt_<what>_<date>.<ext>

# The pipeline's console messages go to the app (log panel and progress bar).
LOG <- new.env(); LOG$lines <- character()
say <- function(..., col = "fg") {
  msg <- paste0(...); LOG$lines <- c(LOG$lines, msg); cat(msg, "\n", sep = "")
  if (grepl("^\\[[0-9]/8\\]", msg) && !is.null(getDefaultReactiveDomain()))
    try(setProgress((as.numeric(substr(msg, 2, 2)) - 0.5) / 8, detail = sub("^\\[[0-9]/8\\] ", "", msg)), silent = TRUE)
}

SETTINGS_FILE <- file.path(APP_DIR, "app_settings.json")
SETTINGS <- settings_read(SETTINGS_FILE)                          # core util.R: the last good copy if this one is damaged
# Only numbers are set on the Settings page (the expert limits under Advanced: ADV_KEYS); every on/off switch is on the
# Automatic corrections list (CORR_KEYS). Keys of older settings files that are no longer here are ignored.
ADV_KEYS <- c("min_year", "gap_factor", "spike_k", "zero_offset_limit", "surface_max_depth", "fill_gap_days", "fill_min_depth", "fill_r2_min",
              "air_like_daily_range", "air_like_min_depth", "pre_install_spread", "pre_install_days")
NUM_KEYS <- c("base_offset_h", "default_first_depth", "default_step", "min_complete", "year_complete", "water_year_start", "zero_band",
              "zero_curtain_days", "trend_min_years", "permafrost_years", ADV_KEYS)
SET_KEYS <- c("raw_dirs", "metadata_dir", "project_dir", NUM_KEYS, "unzoned_offset_h", "depth_positive")
CORR_KEYS <- vapply(Filter(function(x) isTRUE(x$panel) && nzchar(x$setting %||% ""), CORRECTIONS), function(x) x$setting, "")   # the Automatic corrections switches
for (k in SET_KEYS) if (is.null(SETTINGS[[k]])) SETTINGS[k] <- list(CFG[[k]])
if (nzchar(Sys.getenv("GT_PROJECT_DIR"))) SETTINGS$project_dir <- Sys.getenv("GT_PROJECT_DIR")
SETTINGS$project_dir <- ovid_project_dir(SETTINGS$project_dir)   # none set: ~/Desktop/Ovid_Projects/<app>/Untitled (core util.R)
SETTINGS$permafrost_years <- gt_permafrost_years(SETTINGS$permafrost_years, SETTINGS$project_dir)   # not saved yet: the old calibration_options.csv (R/config.R)
if (nzchar(Sys.getenv("GT_RAW_DIRS"))) SETTINGS$raw_dirs <- strsplit(Sys.getenv("GT_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("GT_METADATA_DIR"))) SETTINGS$metadata_dir <- Sys.getenv("GT_METADATA_DIR")
BASE_OFFSET <- SETTINGS$base_offset_h
VIEWER <- identical(Sys.getenv("GT_VIEWER"), "1") || isTRUE(SETTINGS$viewer)       # colleagues can look but not change the set-up
LOCAL <- identical(Sys.getenv("GT_LOCAL"), "1")                                   # started from Ovid_GT.bat
OPEN_SESSIONS <- new.env(); OPEN_SESSIONS$n <- 0L

source(file.path(APP_DIR, "pages", "fct_outputs.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_tables.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_setup.R"), local = TRUE)

# =============================================================================
#  UI
# =============================================================================
source(file.path(APP_DIR, "pages", "ui.R"), local = TRUE)   # GT/pages/ui.R: the pages and ui

# =============================================================================
#  Server
# =============================================================================
# embedded = TRUE: running inside the Workbench (workbench/wb_gt.R), which shows these pages in its own frame and gets the
# set-up table editors, outputs and run state returned at the end.
server <- function(input, output, session, embedded = FALSE) {
  reference_server(input, output, session, "GT", REF_GO)        # core reference.R
  convert_server(input, output)                                             # core convert_page.R
  colours_server(input, output, "GT")                                       # Settings > Colours (core shell.R)
  troubleshoot_server(output, function() isolate(pd()), function() isolate(setup_check(current_cfg())))   # the lifebuoy in the top bar (core shell.R)
  projects_server("proj", settings_now = function() utils::modifyList(SETTINGS, current_cfg()[intersect(SET_KEYS, names(current_cfg()))]),
                  open = function(path, s) { pd_open(path.expand(path)); projects_inputs(session, s, SET_KEYS) })    # the Projects list (core projects.R)
  corrections_server("corr", function() current_cfg())          # the Automatic corrections list, its own step of the set-up flow (core corrections.R)
  run_server(input, output, session, busy_ids = c("run", "run_force", "rf_rescan", "uz_go", "xp_dl", "rp_dl", "wb_export"))               # runs in the background (core runner.R)
  # Browse: the system's folder picker (Finder, Explorer) or the in-app folder browser (Ovid/files.R)
  ovid_browse(input, output, session, c("raw_dirs", "metadata_dir", "project_dir"), add = "raw_dirs", places = function() c(Project = SETTINGS$project_dir))
  rv <- reactiveValues(tick = 0, log = character(), stale = FALSE, zoom = NULL, pf = 0, au_rule = NULL, imp = NULL)
  pd <- reactive(sub("[/\\\\]+$", "", path.expand(ovid_project_dir(input$project_dir %|% SETTINGS$project_dir))))   # none set: the default folder (core util.R)
  dataset_card_server(input, output, pd)                                    # Settings > Dataset (core shell.R)
  # A project folder typed or browsed to, not opened from the Projects list: as the list does (core projects.R switch_to), the
  # project left keeps its settings and the one now open brings its own (config/project_settings.json). A folder with no saved
  # settings starts with the ones shown.
  pd_open <- reactiveVal(isolate(pd())); pd_typed <- debounce(pd, 1200)
  observeEvent(pd_typed(), ignoreInit = TRUE, {
    new <- pd_typed(); old <- pd_open(); if (identical(path_norm(new), path_norm(old))) return()
    pd_open(new); s <- project_settings_read(new); if (!length(s)) return()
    cfg <- current_cfg()
    if (dir.exists(path.expand(old))) project_settings_save(old, utils::modifyList(SETTINGS, c(cfg[intersect(SET_KEYS, names(cfg))], list(project_dir = old))))
    s <- s[intersect(names(s), setdiff(c(SET_KEYS, CORR_KEYS), "project_dir"))]
    settings_save(SETTINGS_FILE, s); SETTINGS[names(s)] <<- s; live_touch()
    projects_inputs(session, s, names(s))
    note("Opened the settings saved with ", project_name(new), ".") })
  cfgp <- function(what) reactive(cfg_path(pd(), what))
  tick <- reactive(rv$tick); mark <- function() rv$stale <- TRUE
  go <- function(tab) ovid_go(session, if (embedded && tab %in% c("Home", "Project")) "wb-import" else tab)   # the Workbench keeps the folders under Import data
  note <- function(..., type = "message", duration = 5) showNotification(paste0(...), type = type, duration = duration)
  # a setting as it stands on the Settings page, else as saved (another window too); never the value read at start
  set_now <- function(k) { v <- input[[k]]; if (is.null(v) || (length(v) == 1 && is.na(v))) v <- live_settings()[[k]] %||% SETTINGS[[k]]; v }
  # a setting as the last run used it (its run manifest), for the pages that show that run's results; else as it stands
  run_set <- function(k) { m <- tryCatch(outs()$manifest, error = function(e) NULL)
    v <- if (!is.null(m) && nrow(m) && "key" %in% names(m)) suppressWarnings(as.numeric(m[key == k, value][1])) else NA_real_
    if (length(v) == 1 && !is.na(v)) v else set_now(k) }
  proj_sgn <- function() depth_sign(list(depth_positive = set_now("depth_positive")))   # depths typed or written in the project's convention (R/util.R)

  E <- setup_editors(cfgp, tick, mark, skip = "depsheet")
  rename_server("ren", project_dir = pd, on_change = function() { rv$tick <- rv$tick + 1; mark() })   # core rename.R
  dcheck <- deploy_check_server("depcheck", pd, list(file = "output/audit/files.csv", serial = "serial", name = "title", file_col = "rel"),   # core metasheet_ui.R
                      tick, on_change = function() { rv$tick <- rv$tick + 1; mark() })
  # the deployment sheet with the logger list's columns (core metasheet.R): rows for the loggers the last run read, so people
  # only add what they know
  M <- meta_sheet_server("mlist", pd, meta_sheet_fields(META_FIELDS, GT_MAP), found = reactive({ o <- outs(); if (is.null(o) || !nrow(o$loggers)) return(NULL)
      unique(o$loggers[, .(serial = as.character(serial), model = as.character(model))], by = "serial") }), tick = tick, on_change = mark, dt = ovid_dt, what = "logger")
  E$depsheet <- M$E
  E$manual <- period_view(E$period, c("remove", "flag"), gt_manual_to_period, gt_period_to_manual)   # removals are rows of the corrections over a period (R/config.R)
  m_sens <- E$sensors; m_site <- E$sites; m_tmpl <- E$templates; m_tz <- E$tz; m_time <- E$time; m_fr <- E$frules; m_qc <- E$qc; m_man <- E$manual; m_camp <- E$campbell
  all_tables <- function() unname(E)                 # every editor (the set-up tables and the logger list): a run saves each one's unsaved edits

  current_cfg <- function() {
    cfg <- CFG
    cfg$raw_dirs <- trim(strsplit(input$raw_dirs %||% paste(SETTINGS$raw_dirs, collapse = "\n"), "\n")[[1]]); cfg$raw_dirs <- cfg$raw_dirs[nzchar(cfg$raw_dirs)]
    for (k in c("metadata_dir", "project_dir")) cfg[[k]] <- trim(input[[k]] %||% SETTINGS[[k]])
    for (k in NUM_KEYS) { v <- input[[k]]; cfg[[k]] <- if (!is.null(v) && !is.na(v)) v else SETTINGS[[k]] }
    uz <- SETTINGS$unzoned_offset_h; cfg$unzoned_offset_h <- if (is.null(uz) || is.na(uz)) NA_real_ else uz
    cfg$depth_positive <- input$depth_positive %||% SETTINGS$depth_positive %||% CFG$depth_positive   # kept per project (config/project_settings.json)
    cfg$ask_unzoned <- TRUE
    corrections_cfg(cfg)                           # switches on the Automatic corrections list (core corrections.R)
  }
  output$ll_depth_note <- renderUI(p(class = "dim", if (identical(input$depth_positive %||% SETTINGS$depth_positive, "up"))
    "Sensor depths: metres above the ground positive, below it negative; + is always above (e.g. +0.3, -0.1, -0.5, -1.0). Set on Settings, Time and depths."
    else "Sensor depths: metres below the ground; + for a sensor above it (e.g. +0.3, 0.1, 0.5, 1.0). Set on Settings, Time and depths."))
  save_settings <- function(cfg) { try(settings_save(SETTINGS_FILE, cfg[intersect(SET_KEYS, names(cfg))]), silent = TRUE); BASE_OFFSET <<- cfg$base_offset_h }   # other saved settings are kept
  settings_autosave(SET_KEYS, function() { cfg <- current_cfg(); k <- intersect(SET_KEYS, names(cfg)); save_settings(cfg)   # core editor.R: saved when changed, not at the next run
    SETTINGS[k] <<- cfg[k] })                                                                                         # and what the app's pages read

  # Each page of the app, in this order (GT/pages/; they share this function's variables)
  source(file.path(APP_DIR, "pages", "run.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "home.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "compare.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "summaries.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "soil.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "site_details.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "raw_files.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "sensor_health.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "clock.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "audit.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "sites_and_landcover.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "configuration.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "set_up_check.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "export_page.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "setup_flow.R"), local = TRUE)
  invisible(list(E = E, outs = outs, pd = pd, rv = rv, mark = mark, pf = pf_data, cfg = current_cfg, needs = dcheck, review = review))
}

# Two interfaces on one project: the Standard one above (the default) and the Workbench (preview, workbench/wb_gt.R).
# ?ui=workbench in the address opens the Workbench and ?ui=standard the Standard one; with neither, OVID_UI and then
# the "ui" setting decide (core/workbench.R).
source(file.path(APP_DIR, "workbench", "wb_gt.R"), local = TRUE)
shinyApp(ui = function(req) if (wb_wanted(req$QUERY_STRING, SETTINGS$ui)) wb_ui(req) else ui,
         server = function(input, output, session) { track_session(session)
           if (!wb_wanted(isolate(session$clientData$url_search), SETTINGS$ui)) return(server(input, output, session))
           wb_server(input, output, session, server(input, output, session, embedded = TRUE)) })
