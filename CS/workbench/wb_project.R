# CS/workbench/wb_project.R  --  The Workbench's project views and navigation: Import data (the Standard folder and run
# settings with what a scan of them will show, the set-up check, Run), the tree of Standard pages and set-up tables, and the tabs.
# Part of Ovid Core // CS. Sourced by wb_cs.R. The Standard pages run on the Standard server (app.R, embedded = TRUE),
# so every Standard process works here the same way; this file only places them.

# Where each set-up table is edited: the page, and the tab or the folded section on it that holds its editor.
WB_OPEN <- list(
  stations  = list(go = "Stations"),
  depsheet  = list(go = "Stations"),
  aliases   = list(go = "Column names", tab = "Aliases"),
  groups    = list(go = "Column names", tab = "Groups"),
  qc        = list(go = "Quality rules", tab = "Automatic limits"),
  cr10x     = list(go = "CR10X files"),
  timecorr  = list(go = "Clock checks", details = "Corrections already set"),
  pinfo     = list(go = "Parameter guide", details = "Edit the guide"),
  gauges    = list(go = "Precipitation", tab = "Gauge set-up"),
  sites     = list(go = "Sites and field log", tab = "Station details"),
  fieldlog  = list(go = "Sites and field log", tab = "Field log"),
  filerules = list(go = "Raw files", details = "Rules for single files"),
  calib     = list(go = "Quality rules", tab = "Calibration options"),
  period    = list(go = "Quality rules", tab = "Corrections over a period"),
  timezones = list(go = "Clock checks", details = "Time zones"))

# The app's own processing steps, in the tree (core wb_process_tree): the page and tab where each is set.
WB_PROCESS <- list(
  list(label = "Gauge set-up", go = "Precipitation", tab = "Gauge set-up", title = "Each gauge, its model and shield: the model decides the corrections"),
  list(label = "Precipitation corrections by gauge model", go = "Precipitation", tab = "Method"),
  list(label = "Calibrations", go = "Quality rules", tab = "Corrections over a period"),
  list(label = "Combined records", go = "Composite", title = "One parameter combined from several stations into a single record"),
  list(label = "Clocks", go = "Clock checks"),
  list(label = "Corrections over a period", go = "Quality rules", tab = "Corrections over a period"),
  list(label = "Compare neighbours", go = "Explore", tab = "Against the neighbours"),
  list(label = "Monthly and annual", go = "Explore", tab = "Summaries"),
  list(label = "Automatic corrections", go = "Corrections"),
  list(label = "Needs you", go = "Needs"),
  list(label = "Reference: parameter guide", go = "Parameter guide"))

WB_PIPELINE <- list(                                     # the Workbench tabs in pipeline order (core wb_tabs_server)
  list(label = "Corrections", go = "Quality rules", tab = "Corrections over a period", title = "Corrections over a period, calibrations, manual removals and clocks"),
  list(label = "QA/QC", go = "wb-data"),
  list(label = "Compare neighbours", go = "Explore", tab = "Against the neighbours"),
  list(label = "Monthly and annual", go = "Explore", tab = "Summaries"),
  list(label = "Reference", go = "Reference"))

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_import_ui <- function() div(class = "wb-std d-tool wb-import",
  tags$h1(class = "wb-h1", "Import data"),
  p(class = "wb-lede", "Raw folders are read and never written. A run reads new and changed files, stitches them per logger table, applies the set-up tables and writes the station tables, the audit trail and the summaries to the project folder."),
  if (!VIEWER) std_folders(),
  div(style = "margin-top:14px", span(class = "wb-eb", "Set-up check"), uiOutput("wb_check_short")),
  if (!VIEWER) div(class = "wb-runrow", style = "margin-top:16px",
    tags$button(type = "button", class = "wb-btn primary big", onclick = "document.getElementById('run').click()", HTML("&#9656; Run")), uiOutput("wb_run_note", inline = TRUE)))

# ---- server -----------------------------------------------------------------------------------------------------------
# ctx: the environment of wb_server (std, E, pd, outs, view, key).
wb_project_server <- function(input, output, session, ctx) {
  E <- ctx$E; std <- ctx$std
  output$wb_check_short <- renderUI({ p <- tryCatch(std$pf(), error = function(e) NULL); if (is.null(p)) return(NULL)
    ne <- sum(p$level == "error"); nw <- sum(p$level == "warning")
    div(class = "wb-inline", if (!ne && !nw) span(class = "wb-good", "Safe to run.") else span(class = if (ne) "wb-bad" else "wb-warnt",
      paste0(if (ne) paste(ne, if (ne == 1) "error" else "errors") else "", if (ne && nw) ", " else "", if (nw) paste(nw, if (nw == 1) "warning" else "warnings") else "",
             if (ne) ": a run would ignore or misapply something." else ".")),
      if (nrow(p)) tags$a(href = "#", `data-ovid-go` = "Preflight", "Open the set-up check")) })
  output$wb_run_note <- renderUI({ o <- ctx$outs()
    if (isTRUE(std$rv$stale)) span(class = "wb-warnt", "Set-up changed since the last run.")
    else if (is.null(o)) span(class = "wb-p", "Not run yet.")
    else span(class = "wb-p", style = "color:var(--wb-fg2)", paste("Last run", format(o$when, "%Y-%m-%d %H:%M"))) })

  # the set-up tables: row counts, a dot for unsaved edits; a click opens the page and the tab with the editor
  output$wb_setup_tree <- renderUI(if (!VIEWER) wb_setup_tree(E, SETUP_TABLES, WB_OPEN, std$rv$stale))

  # tabs: Import, the station table, and the pages opened last (up to five)
  wb_tabs_server(input, output, session, std_pages(FALSE), reactive({ k <- ctx$key(); if (is.null(k)) "Data" else { p <- wb_split(k); paste(p$station, "\u00b7", p$stream) } }), pipeline = WB_PIPELINE)

  # outputs written by another window or the command line: read them again (this window's own runs already did)
  wb_watch_outputs(session, reactive(file.path(ctx$pd(), "output", "audit", "audit_index.csv")), std$rv)
  invisible(ctx)
}
