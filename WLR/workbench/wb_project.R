# WLR/workbench/wb_project.R  --  The Workbench's project views and navigation: Import data (the Standard folders, a
# scan of them, the set-up check, Run), the tree of Standard pages and set-up tables, and the tabs.
# Part of Ovid Core // WLR. Sourced by wb_wlr.R. The Standard pages run on the Standard server (app.R, embedded = TRUE),
# so every Standard process works here the same way; this file only places them.

# Where each set-up table is edited: the page, and the tab on it that holds its editor (see core/workbench.R).
WB_OPEN <- list(
  depsheet = list(go = "Metadata", tab = "Deployment sheet"),
  deps   = list(go = "Deploy"),
  bgrp   = list(go = "Baro"),
  bmem   = list(go = "Baro", tab = "Which site is in which group"),
  bstn   = list(go = "Baro", tab = "Baro stations across years"),
  xbaro  = list(go = "Baro", tab = "Baro stations across years"),
  bcor   = list(go = "Baro", tab = "Offset and drift"),
  gex    = list(go = "Datum", tab = "Survey history"),
  qc     = list(go = "QC", tab = "Automatic limits"),
  period = list(go = "QC", tab = "Corrections over a period"),
  oow    = list(go = "QC", tab = "Out of water"),
  meas   = list(go = "Visits"),
  time   = list(go = "Clock"),
  tzt    = list(go = "Clock"),
  gpsp   = list(go = "Datum", tab = "Survey points for each site"),
  datm   = list(go = "Datum", tab = "Elevations typed by hand"),
  offs   = list(go = "Datum", tab = "Logger offsets"),
  frules = list(go = "Files", tab = "Sorting rules"),
  acc    = list(go = "Hydro", tab = "Uncertainty"),
  pairs  = list(go = "Hydro", tab = "Gradients"),
  sy     = list(go = "Hydro", tab = "Recharge"),
  appr   = list(go = "Export"))

# The app's own processing steps, in the tree (core wb_process_tree): the page and tab where each is set.
WB_PROCESS <- list(
  list(label = "Barometric compensation", go = "Baro", title = "One barologger per site and season; the nearest elsewhere is only a suggestion"),
  list(label = "Barologger checks: offset and drift", go = "Baro", tab = "Offset and drift"),
  list(label = "Drift compensation and manual levels", go = "Visits", title = "Tape readings at each visit and the drift between them"),
  list(label = "Datum and offsets", go = "Datum", tab = "Logger offsets"),
  list(label = "Logger against tape", go = "Visits"),
  list(label = "Out of water", go = "QC", tab = "Out of water"),
  list(label = "Corrections over a period", go = "QC", tab = "Corrections over a period"),
  list(label = "Clocks", go = "Clock"),
  list(label = "Automatic corrections", go = "Corrections"),
  list(label = "Needs you", go = "Needs"))

WB_PIPELINE <- list(                                     # the Workbench tabs in pipeline order (core wb_tabs_server)
  list(label = "Barometric compensation", go = "Baro"),
  list(label = "Corrections", go = "QC", tab = "Corrections over a period", title = "Corrections over a period, manual removals and out of water"),
  list(label = "QA/QC", go = "wb-data"),
  list(label = "Seasons", go = "Explore", tab = "Compare years"),
  list(label = "Reference", go = "Reference"))

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_import_ui <- function() div(class = "wb-std d-tool wb-import",
  tags$h1(class = "wb-h1", "Import data"),
  p(class = "wb-lede", "Data folders, the metadata workbook and the dGPS exports are read and never written. A run reads new and changed files, stitches each serial across years, splits it into deployments, compensates it with its barologgers, applies the set-up tables and writes the site files, the corrected data, the audit trail and the summaries to the project folder."),
  if (!VIEWER) std_folders(),
  div(span(class = "wb-eb", "Scan"), uiOutput("wb_scan")),
  div(style = "margin-top:14px", span(class = "wb-eb", "Set-up check"), uiOutput("wb_check_short")),
  if (!VIEWER) div(class = "wb-runrow", style = "margin-top:16px",
    tags$button(type = "button", class = "wb-btn primary big", onclick = "document.getElementById('run').click()", HTML("&#9656; Run")), uiOutput("wb_run_note", inline = TRUE)))

# ---- server -----------------------------------------------------------------------------------------------------------
# ctx: the environment of wb_server (std, E, pd, outs, view, key, part).
wb_project_server <- function(input, output, session, ctx) {
  E <- ctx$E; std <- ctx$std
  dirs <- function(x) { d <- trim(strsplit(x %||% "", "\n")[[1]]); d[nzchar(d)] }
  raw_d <- debounce(reactive(input$raw_dirs), 700)
  found <- reactive({ d <- path.expand(dirs(raw_d())); d <- d[dir.exists(d)]
    f <- raw_files_found(d)                                                            # R/discover.R: the files a run reads
    data.table(path = f$path, size = as.numeric(f$size)) })
  output$wb_scan <- renderUI({ o <- ctx$outs(); f <- found()
    m <- if (!is.null(o) && nrow(o$files)) o$files else data.table(path = character(), size = numeric(), use = logical(), status = character())
    i <- match(f$path, normalizePath(as.character(m$path), winslash = "/", mustWork = FALSE))
    new <- sum(is.na(i)); changed <- sum(!is.na(i) & suppressWarnings(as.numeric(m$size[i])) != f$size, na.rm = TRUE)
    tile <- function(v, k, cls = "") div(class = paste("wb-tile", cls), div(class = "v", v), div(class = "k", k))
    div(class = "wb-tiles", tile(num(nrow(f)), "files in the folders"), tile(num(new), "new since the last run", if (new) "hi"), tile(num(changed), "changed size", if (changed) "hi"),
        tile(num(sum(m$use %in% TRUE)), "used in the last run"), tile(num(if (!is.null(o)) nrow(o$loggers) else 0), "loggers"),
        tile(num(if (!is.null(o)) nrow(o$summary) else 0), "deployments"), tile(num(sum(!m$use %in% TRUE & !m$status %in% c("paired_hobo", "companion_csv", "duplicate_file"))), "not used")) })
  # safe: no check result yet: nothing to show
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
  output$wb_setup_tree <- renderUI(if (!VIEWER) wb_setup_tree(E, SETUP_TREE, WB_OPEN, std$rv$stale))

  # tabs: Import, the record in view, and the pages opened last (up to five), titled as wb_ui shows them (the flow pages)
  wb_tabs_server(input, output, session, c(wlr_flow_pages(FALSE), list(convert_tool(), reference_tool("WLR"))), reactive({ if (is.null(ctx$key())) "Data" else ctx$part()$label }), pipeline = WB_PIPELINE)

  # outputs written by another window or the command line: read them again (this window's own runs already did)
  wb_watch_outputs(session, reactive(file.path(ctx$pd(), "output", "audit", "audit_index.csv")), std$rv)
  invisible(ctx)
}
