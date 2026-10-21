# =============================================================================
#  app.R  --  Shiny front end for wlr_pipeline.R  (Ovid Core // WLR)
# =============================================================================
#  Keep this file in the same folder as wlr_pipeline.R and the R/ folder, then:
#      shiny::runApp("path/to/ovid_WLR")
#  (or open app.R in RStudio and press "Run App").
#
#  The app does not re-implement anything: it calls the functions in wlr_pipeline.R and edits the same set-up CSVs,
#  so the audit guarantees are identical whether you run from here or from the command line.
# =============================================================================

for (p in c("shiny", "DT", "data.table", "ggplot2", "xml2", "readxl", "zip"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(DT) })

APP_DIR <- getwd()
if (!file.exists(file.path(APP_DIR, "wlr_pipeline.R"))) stop("wlr_pipeline.R must sit in the same folder as app.R")
Sys.setenv(WLR_NO_RUN = "1")
source(file.path(APP_DIR, "wlr_pipeline.R"), local = TRUE)
log_app_errors()                                   # errors outside a run go to ~/.ovid/logs (core manifest.R)
source(file.path(CORE_DIR, "shell.R"), local = TRUE)
source(file.path(CORE_DIR, "table.R"), local = TRUE)        # Ovid/table.R: the core table (ovid_dt)
source(file.path(CORE_DIR, "reference.R"), local = TRUE)
source(file.path(CORE_DIR, "convert_page.R"), local = TRUE) # Ovid/convert_page.R: the Convert page (units, DMS, UTM)
source(file.path(CORE_DIR, "grouping.R"), local = TRUE)     # Ovid/grouping.R: group the logger list by serial, deployment, download or file    # Ovid/reference.R: the Reference page (the guide as a searchable index)
REF_GO <- list("Barometric|Compensation" = list(go = "Baro"), "Tape|manual|Drift" = list(go = "Visits"), "Depth to water|elevation|Datum" = list(go = "Datum"), "Quality|flag" = list(go = "QC"), "Clock|Time" = list(go = "Clock"), "Deployment" = list(go = "Deploy"), "conductance" = list(go = "Explore"))   # where each reference topic is set
source(file.path(CORE_DIR, "tsview.R"), local = TRUE)       # Ovid/tsview.R: the Standard view's time-series plot
source(file.path(CORE_DIR, "flow.R"), local = TRUE)         # Ovid/flow.R: the set-up flow (unsaved changes, what shows, Check the data)
source(file.path(CORE_DIR, "runner.R"), local = TRUE)       # Ovid/runner.R: runs in the background, with progress and Cancel       # Ovid/shell.R: the shared page shell
source(file.path(CORE_DIR, "projects.R"), local = TRUE)     # Ovid/projects.R: the Projects list (new, open, delete to the Trash)
source(file.path(CORE_DIR, "rename.R"), local = TRUE)       # Ovid/rename.R: rename a site or station in every set-up table
source(file.path(CORE_DIR, "files.R"), local = TRUE)        # Ovid/files.R: Browse buttons and export names
source(file.path(CORE_DIR, "editor.R"), local = TRUE)       # Ovid/editor.R: the set-up table editor
source(file.path(CORE_DIR, "metasheet_ui.R"), local = TRUE) # Ovid/metasheet_ui.R: the logger list (metadata sheet)
downloadHandler <- ovid_downloads("WLR")                  # every download is named ovid_wlr_<what>_<date>.<ext>

# The pipeline's console messages go to the app (log panel and progress bar).
LOG <- new.env(); LOG$lines <- character()
SAY_STEPS <- 7                                                    # core say(): "[k/7]" messages move the progress bar

SETTINGS_FILE <- file.path(APP_DIR, "app_settings.json")
SETTINGS <- settings_read(SETTINGS_FILE)                          # core util.R: the last good copy if this one is damaged
SET_KEYS <- c("raw_dirs", "metadata_dir", "dgps_dir", "project_dir", "years", "base_offset_h", "utm_zone", "solinst_kpa_per_m", "density_mode", "fixed_density",
              "gravity", "baro_max_gap_h", "service_window_h", "out_of_water_m", "ec_alpha", "check_window_h", "baro_fill", "auto_trim_air",
              "air_max_hours", "dgps_pick", "solinst_altitude", "unzoned_offset_h", "drift_correction",
              "hang_measured_to", "sigma_baro_m", "sigma_fill_m", "sigma_tape_m", "sigma_survey_m", "be_flag", "casing_tol_m", "precip_file", "water_year_start",
              "add_sensor_offset", "air_same_as_oow", "air_threshold_cm", "drift_min_m", "tape_warn_m", "drift_one_visit",
              "drop_stray_records", "drop_bad_clock", "baro_interp", "hide_accepted_flags")   # the last four: switches on the Automatic corrections list (R/config.R CORRECTIONS)
for (k in SET_KEYS) if (is.null(SETTINGS[[k]])) SETTINGS[k] <- list(CFG[[k]])
BASE_OFFSET <- SETTINGS$base_offset_h; UTM_ZONE <- SETTINGS$utm_zone   # exports made before a run use the saved zones
if (nzchar(Sys.getenv("WLR_PROJECT_DIR"))) SETTINGS$project_dir <- Sys.getenv("WLR_PROJECT_DIR")
SETTINGS$project_dir <- ovid_project_dir(SETTINGS$project_dir)   # none set: ~/Projects/Ovid/Projects/<app>/Untitled (core util.R)
if (nzchar(Sys.getenv("WLR_RAW_DIRS"))) SETTINGS$raw_dirs <- strsplit(Sys.getenv("WLR_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("WLR_METADATA_DIR"))) SETTINGS$metadata_dir <- Sys.getenv("WLR_METADATA_DIR")
if (nzchar(Sys.getenv("WLR_DGPS_DIR"))) SETTINGS$dgps_dir <- Sys.getenv("WLR_DGPS_DIR")
HAS_OUTPUT <- length(SETTINGS$project_dir) == 1 && file.exists(file.path(path.expand(SETTINGS$project_dir), "output", "audit", "summary.csv"))
# Viewer mode: colleagues can look but not change the set-up or run the pipeline (WLR_VIEWER=1).
VIEWER <- identical(Sys.getenv("WLR_VIEWER"), "1") || isTRUE(SETTINGS$viewer)
# Started from Ovid_WLR.bat: the app runs on this computer, so it can open a folder picker and should stop when its
# browser tab is closed (a reload reconnects within a few seconds and keeps it running).
LOCAL <- identical(Sys.getenv("WLR_LOCAL"), "1")
OPEN_SESSIONS <- new.env(); OPEN_SESSIONS$n <- 0L

source(file.path(APP_DIR, "pages", "fct_outputs.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_tables.R"), local = TRUE)

source(file.path(APP_DIR, "pages", "utils_setup.R"), local = TRUE)

# =============================================================================
#  UI
# =============================================================================
source(file.path(APP_DIR, "pages", "ui.R"), local = TRUE)   # WLR/pages/ui.R: the pages and ui

# =============================================================================
#  Server
# =============================================================================
# embedded = TRUE: running inside the Workbench (workbench/wb_wlr.R), which shows these pages in its own frame and gets the
# set-up table editors, outputs and run state returned at the end.
server <- function(input, output, session, embedded = FALSE) {
  reference_server(input, output, session, "WLR", REF_GO)        # core reference.R
  convert_server(input, output)                                             # core convert_page.R
  colours_server(input, output, "WLR")                                       # Settings > Colours (core shell.R)
  troubleshoot_server(output, function() isolate(pd()), function() isolate(setup_check(current_cfg())))   # the lifebuoy in the top bar (core shell.R)
  projects_server("proj", settings_now = function() utils::modifyList(SETTINGS, current_cfg()[intersect(SET_KEYS, names(current_cfg()))]),
                  open = function(path, s) projects_inputs(session, s, SET_KEYS, join = c(raw_dirs = "\n", years = ", ")))   # the Projects list (core projects.R)
  corrections_server("corr", function() current_cfg())          # the Automatic corrections list on the Settings page (core corrections.R)
  run_server(input, output, session, busy_ids = c("run", "run_force", "rf_rescan", "uz_go", "xp_dl", "wb_export"))               # runs in the background (core runner.R)
  # Browse: the system's folder picker (Finder, Explorer) or the in-app folder browser (Ovid/files.R)
  ovid_browse(input, output, session, c("raw_dirs", "metadata_dir", "dgps_dir", "project_dir"), add = "raw_dirs", places = function() c(Project = SETTINGS$project_dir))
  rv <- reactiveValues(tick = 0, log = character(), stale = FALSE, zoom = NULL, pf = 0, au_rule = NULL)
  pd <- reactive(sub("[/\\\\]+$", "", path.expand(ovid_project_dir(input$project_dir %|% SETTINGS$project_dir))))   # none set: the default folder (core util.R)
  dataset_card_server(input, output, pd)                                    # Settings > Dataset (core shell.R)
  cfgp <- function(what) reactive(cfg_path(pd(), what))
  tick <- reactive(rv$tick); mark <- function() rv$stale <- TRUE
  go <- function(tab) ovid_go(session, if (embedded && tab %in% c("Home", "Project")) "wb-import" else tab)   # the Workbench keeps the folders under Import data
  note <- function(..., type = "message", duration = 5) showNotification(paste0(...), type = type, duration = duration)

  E <- setup_editors(cfgp, tick, mark, skip = "depsheet")
  E$bgrp <- editServer("bgrp", cfgp("baro_groups"), tick, on_change = mark)   # set with the per-year pickers on Barometric (no table on screen)
  rename_server("ren", project_dir = pd, on_change = function() { rv$tick <- rv$tick + 1; mark() })   # core rename.R
  dcheck <- deploy_check_server("depcheck", pd, list(file = "output/audit/files.csv", serial = "serial", name = "title", file_col = "rel"),   # core metasheet_ui.R
                      tick, on_change = function() { rv$tick <- rv$tick + 1; mark() })
  # the deployment sheet with the logger list's columns (core metasheet.R): rows for the loggers the last run read (their serial,
  # type and model), so people only add what they know
  M <- meta_sheet_server("mlist", pd, meta_sheet_fields(META_FIELDS), found = reactive({ o <- outs(); if (is.null(o) || !nrow(o$loggers)) return(NULL)
      o$loggers[, .(serial = as.character(serial), type = ifelse(kind == "baro", "Baro", ""), model = as.character(model))] }),
    tick = tick, on_change = mark, dt = ovid_dt, what = "logger")
  E$depsheet <- M$E
  # manual removals and drift rows are rows of the corrections over a period (R/config.R): the pages write them as before
  E$manual <- period_view(E$period, "remove", wlr_manual_to_period, wlr_period_to_manual)
  E$drift <- period_view(E$period, WLR_DRIFT_ACTIONS, wlr_drift_to_period, wlr_period_to_drift)
  m_dep  <- E$deps
  m_bgrp <- E$bgrp
  m_bmem <- E$bmem
  m_bstn <- E$bstn
  m_bcor <- E$bcor
  m_gex  <- E$gex
  m_qc   <- E$qc
  m_man  <- E$manual
  m_oow  <- E$oow
  m_meas <- E$meas
  m_time <- E$time
  m_tzt  <- E$tzt
  m_gps  <- E$gpsp
  m_datm <- E$datm
  m_offs <- E$offs
  # Logger offsets form: a known offset from the drop-down, or a manual measurement, for a model or one serial
  of_known_tab <- reactive({ known_offsets(as.data.table(m_offs$data())) })
  observe({ k <- of_known_tab(); updateSelectInput(session, "of_known", choices = c("Manual measurement" = "manual", setNames(as.character(seq_len(nrow(k))), k$label)), selected = isolate(input$of_known) %||% "manual") })
  observe({ o <- outs(); tab <- as.data.table(m_offs$data()); lg <- if (!is.null(o) && nrow(o$loggers)) o$loggers[kind == "water"] else data.table(serial = character(), model = character())
    ch <- if (identical(input$of_for, "serial")) { if (nrow(lg)) setNames(lg$serial, paste0(lg$serial, " (", lg$model, ")")) else character() }
          else { pats <- if (!is.null(tab) && nrow(tab)) unique(tab$model[nzchar(tab$model)]) else character(); mods <- setdiff(unique(lg$model[!is.na(lg$model)]), pats)
                 c(setNames(pats, sprintf("pattern: %s", pats)), setNames(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", mods), mods)) }
    updateSelectizeInput(session, "of_target", choices = ch, selected = character(), server = FALSE) })
  observeEvent(input$of_target, { o <- outs(); k <- of_known_tab(); req(nzchar(input$of_target %||% ""), nrow(k) > 0)
    mod <- if (identical(input$of_for, "serial") && !is.null(o)) o$loggers$model[match(input$of_target, o$loggers$serial)] else gsub("\\\\", "", input$of_target)
    i <- offset_row(if (identical(input$of_for, "serial")) input$of_target else "", mod, k)
    if (!is.na(i)) updateSelectInput(session, "of_known", selected = as.character(i)) }, ignoreInit = TRUE)
  observeEvent(input$of_set, {
    tgt <- trim(input$of_target %||% ""); if (!nzchar(tgt)) { note("Pick or type a model or a serial first.", type = "warning"); return() }
    if (identical(input$of_known, "manual")) { cm <- input$of_cm
      if (is.null(cm) || is.na(cm) || cm < 0 || cm > 100) { note("Type the measured distance in cm (0 to 100).", type = "warning"); return() }
      off <- cm / 100; src <- paste0("manual measurement", if (nzchar(trim(input$of_author %||% ""))) paste0(" by ", trim(input$of_author)), ", ", format(Sys.Date())) }
    else { k <- of_known_tab()[as.integer(input$of_known)]; off <- as.numeric(k$offset_m); src <- k$source }
    tab <- set_offset_row(as.data.table(m_offs$data()), serial = if (identical(input$of_for, "serial")) tgt else "", model = if (identical(input$of_for, "model")) tgt else "",
                          offset_m = off, source = src, notes = trim(input$of_note %||% ""))
    if (isTRUE(m_offs$replace(tab))) { mark(); note(sprintf("Offset for %s set to %s cm. Run the pipeline to apply it.", tgt, format(round(off * 100, 3)))) } })
  m_drift <- E$drift
  m_fr   <- E$frules
  # every set-up table editor, the logger list included (a run saves their unsaved edits first, as Apply does)
  all_tables <- function() Filter(function(x) is.list(x) && is.function(x$save) && is.function(x$dirty), E)

  current_cfg <- function() {
    cfg <- CFG
    cfg$raw_dirs <- trim(strsplit(input$raw_dirs %||% "", "\n")[[1]]); cfg$raw_dirs <- cfg$raw_dirs[nzchar(cfg$raw_dirs)]
    for (k in c("metadata_dir", "dgps_dir", "project_dir")) cfg[[k]] <- trim(input[[k]] %||% SETTINGS[[k]])
    y <- suppressWarnings(as.integer(trim(strsplit(input$years %||% "", ",")[[1]]))); cfg$years <- if (length(y[!is.na(y)])) y[!is.na(y)] else NULL
    for (k in c("base_offset_h", "utm_zone", "solinst_kpa_per_m", "fixed_density", "gravity", "ec_alpha", "baro_max_gap_h", "service_window_h", "out_of_water_m", "check_window_h", "air_max_hours",
                "sigma_baro_m", "sigma_fill_m", "sigma_tape_m", "sigma_survey_m", "be_flag", "casing_tol_m", "air_threshold_cm", "drift_min_m", "tape_warn_m"))
      if (!is.null(input[[k]]) && !is.na(input[[k]])) cfg[[k]] <- input[[k]]
    cfg$density_mode <- input$density_mode %||% cfg$density_mode
    uz <- SETTINGS$unzoned_offset_h; cfg$unzoned_offset_h <- if (is.null(uz) || is.na(uz)) NA_real_ else uz; cfg$ask_unzoned <- TRUE
    for (k in c("baro_fill", "auto_trim_air", "add_sensor_offset", "air_same_as_oow")) if (!is.null(input[[k]])) cfg[[k]] <- isTRUE(input[[k]])
    cfg$dgps_pick <- input$dgps_pick %||% cfg$dgps_pick
    cfg$hang_measured_to <- input$hang_measured_to %||% cfg$hang_measured_to
    cfg$drift_one_visit <- if (identical(input$drift_one_visit %||% SETTINGS$drift_one_visit, "linear")) "linear" else "constant"
    cfg$drift_correction <- if (identical(input$drift_correction %||% cfg$drift_correction, "apply")) "apply" else "off"   # an old "check" is off
    cfg$solinst_altitude <- input$solinst_altitude %||% cfg$solinst_altitude
    wy <- trim(input$water_year_start %||% SETTINGS$water_year_start %||% cfg$water_year_start); cfg$water_year_start <- water_year_start_ok(wy)   # R/baro.R; a bad value falls back to 10-01
    cfg$make_plots <- TRUE
    cfg$precip_file <- trim(input$precip_file %||% SETTINGS$precip_file %||% "")    # bars under the plots only (core flow.R)
    corrections_cfg(cfg)                           # switches on the Automatic corrections list (core corrections.R)
  }
  save_settings <- function(cfg) { try(settings_save(SETTINGS_FILE, cfg[intersect(SET_KEYS, names(cfg))]), silent = TRUE)   # other saved settings are kept
    BASE_OFFSET <<- cfg$base_offset_h %||% BASE_OFFSET; UTM_ZONE <<- cfg$utm_zone %||% UTM_ZONE }
  # configuration: every set-up table and the settings as one zip (core config.R)
  output$cf_export <- downloadHandler(function() paste0("ovid_wlr_configuration_", format(Sys.Date()), ".zip"), function(file) export_config(pd(), current_cfg()[intersect(SET_KEYS, names(current_cfg()))], file))   # core config.R: with CONFIG_EXTRA (R/config.R)
  observeEvent(input$cf_file, { f <- input$cf_file; rv$imp <- tryCatch(read_config_import(f$datapath, f$name), error = function(e) { note(conditionMessage(e), type = "error"); NULL }) })
  output$cf_preview <- renderDT({ imp <- rv$imp; req(imp)
    x <- config_import_preview(imp, pd(), input$cf_mode %||% "replace")                                  # the tables, the sheet's columns, review decisions
    if (!is.null(imp$settings)) x <- rbind(x, data.table(table = "Settings", added = nrow(imp$settings)), fill = TRUE)
    validate(need(nrow(x) > 0, "The file holds no set-up tables of this app."))
    ovid_dt(x, page = 12) })
  observeEvent(input$cf_apply, { imp <- rv$imp; if (VIEWER) return(); if (is.null(imp)) return(note("Choose a file to import first.", type = "warning"))
    r <- tryCatch(apply_config_import(imp, pd(), input$cf_mode, current_user(session)), error = function(e) e)
    if (inherits(r, "error")) return(note("Import failed: ", conditionMessage(r), type = "error", duration = NULL))
    if (isTRUE(input$cf_settings) && !is.null(imp$settings)) {
      s <- config_settings_list(imp$settings, SET_KEYS)                                          # pages/utils_setup.R
      projects_inputs(session, s, SET_KEYS, join = c(raw_dirs = "\n", years = ", "))             # every setting with a control (core projects.R)
      sw <- intersect(names(s), vapply(Filter(function(x) isTRUE(x$panel), CORRECTIONS), `[[`, "", "setting"))   # the switches on the Automatic corrections list
      if (length(sw)) { tryCatch(settings_save(SETTINGS_FILE, s[sw]), error = function(e) note("The Automatic corrections switches could not be saved: ", conditionMessage(e), type = "warning"))
        SETTINGS[sw] <<- s[sw]; live_touch() }
      note("Settings filled in; they are saved as they change.") }
    rv$imp <- NULL; rv$tick <- rv$tick + 1; mark(); note("Configuration imported. The previous tables are in config/history. Run to apply.") })
  observeEvent(input$water_year_start, if (!water_year_start_valid(input$water_year_start))   # R/baro.R
    ovid_field_error("water_year_start", "Give a month and day as MM-DD, e.g. 10-01 for 1 October. Until then water years start on 1 October."), ignoreInit = TRUE)
  settings_autosave(SET_KEYS, function() { cfg <- current_cfg(); k <- intersect(SET_KEYS, names(cfg)); save_settings(cfg)   # core editor.R: saved when changed, not at the next run
    SETTINGS[k] <<- cfg[k] })                                                                                         # and what the app's pages read

  # Each page of the app, in this order (WLR/pages/; they share this function's variables)
  source(file.path(APP_DIR, "pages", "run.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "home.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "context_bar.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "explore_compare.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "barometric.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "deployment_history.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "datum_page.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "field_visits.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "readings_in_the_air.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "audit.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "export_page.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "set_up_check.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "raw_files.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "loggers_and_metadata.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "clock_checks.R"), local = TRUE)
  source(file.path(APP_DIR, "pages", "setup_flow.R"), local = TRUE)
  invisible(list(E = E, outs = outs, pd = pd, rv = rv, mark = mark, pf = pf_data, cfg = current_cfg, needs = dcheck, review = review))
}

# Two interfaces on one project: the Standard one above (the default) and the Workbench (preview, workbench/wb_wlr.R).
# ?ui=workbench in the address opens the Workbench and ?ui=standard the Standard one; with neither, OVID_UI and then
# the "ui" setting decide (core/workbench.R).
source(file.path(APP_DIR, "workbench", "wb_wlr.R"), local = TRUE)
shinyApp(ui = function(req) if (wb_wanted(req$QUERY_STRING, SETTINGS$ui)) wb_ui(req) else ui,
         server = function(input, output, session) { track_session(session, "WLR")
           if (!wb_wanted(isolate(session$clientData$url_search), SETTINGS$ui)) return(server(input, output, session))
           wb_server(input, output, session, server(input, output, session, embedded = TRUE)) })
