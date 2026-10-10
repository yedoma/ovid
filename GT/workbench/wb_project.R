# GT/workbench/wb_project.R  --  The Workbench's project views and navigation: Import data (the Standard folders, a scan
# of them, the set-up check, Run), the tree of Standard pages and set-up tables, and the tabs.
# Part of Ovid Core // GT. Sourced by wb_gt.R. The Standard pages run on the Standard server (app.R, embedded = TRUE),
# so every Standard process works here the same way; this file only places them.

# Where each set-up table is edited: the page, and the tab on it that holds its editor (see core/workbench.R).
WB_OPEN <- list(
  depsheet  = list(go = "Sensors", tab = "Deployment sheet"),
  sensors   = list(go = "Sensors", tab = "Sensors"),
  sites     = list(go = "Sites"),
  templates = list(go = "Sensors", tab = "Depth templates by model"),
  tz        = list(go = "Clock"),
  time      = list(go = "Clock"),
  frules    = list(go = "Files", tab = "File rules"),
  qc        = list(go = "QC", tab = "Automatic rules"),
  period    = list(go = "QC", tab = "Corrections over a period"),
  campbell  = list(go = "QC", tab = "Campbell columns"),
  air       = list(go = "QC", tab = "Air temperature"),
  surface   = list(go = "QC", tab = "Ground-surface changes"))

# The app's own processing steps, in the tree (core wb_process_tree): the page and tab where each is set.
WB_PROCESS <- list(
  list(label = "Sensor depths", go = "Sensors", tab = "Sensors"),
  list(label = "Offsets: corrections over a period", go = "QC", tab = "Corrections over a period", title = "A typed offset for a sensor is an offset row over its period"),
  list(label = "Zero curtain", go = "Summ", tab = "C. Freeze / thaw and zero curtain"),
  list(label = "Thaw depth (active layer)", go = "Summ", tab = "B. Active-layer thickness"),
  list(label = "Depth templates by model", go = "Sensors", tab = "Depth templates by model"),
  list(label = "Clocks", go = "Clock"),
  list(label = "Automatic corrections", go = "Corrections"),
  list(label = "Needs you", go = "Needs"))

WB_PIPELINE <- list(                                     # the Workbench tabs in pipeline order (core wb_tabs_server)
  list(label = "Corrections", go = "QC", tab = "Corrections over a period", title = "Corrections over a period and manual removals"),
  list(label = "QA/QC", go = "wb-data"),
  list(label = "Between sites", go = "Compare"),
  list(label = "Thaw and annual", go = "Summ", tab = "B. Active-layer thickness"),
  list(label = "Reference", go = "Reference"))

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_import_ui <- function() div(class = "wb-std d-tool wb-import",
  tags$h1(class = "wb-h1", "Import data"),
  p(class = "wb-lede", "Data folders and the metadata folder are read and never written. A run reads new and changed downloads, stitches each logger, gives every sensor a site and a depth, applies the quality rules and writes one record per site, the summaries and the audit trail to the working folder."),
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
    f <- unlist(lapply(d, list.files, recursive = TRUE, full.names = TRUE)); f <- f[!startsWith(basename(f), "._")]
    data.table(path = normalizePath(f, winslash = "/", mustWork = FALSE), size = file.size(f)) })
  output$wb_scan <- renderUI({ o <- ctx$outs(); f <- found()
    m <- if (!is.null(o) && nrow(o$files) && all(c("dir", "file") %in% names(o$files))) o$files[, .(path = normalizePath(file.path(dir, file), winslash = "/", mustWork = FALSE), size, use, status)]
         else data.table(path = character(), size = numeric(), use = logical(), status = character())
    i <- match(f$path, m$path)
    new <- sum(is.na(i)); changed <- sum(!is.na(i) & suppressWarnings(as.numeric(m$size[i])) != f$size, na.rm = TRUE)
    tile <- function(v, k, cls = "") div(class = paste("wb-tile", cls), div(class = "v", v), div(class = "k", k))
    nb <- if (!is.null(o) && nrow(o$bundles)) sum(o$bundles$status %in% c("needs_export", "unsupported")) else 0
    div(class = "wb-tiles", tile(num(nrow(f)), "files in the folders"), tile(num(new), "new since the last run", if (new) "hi"), tile(num(changed), "changed size", if (changed) "hi"),
        tile(num(sum(m$use %in% TRUE)), "read in the last run"), tile(num(if (!is.null(o)) nrow(o$loggers) else 0), "loggers"),
        tile(num(if (!is.null(o)) nrow(o$ss) else 0), "sites"), tile(num(nb), "downloads need an export", if (nb) "hi")) })
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

  # tabs: Import, the record in view, and the pages opened last (up to five)
  wb_tabs_server(input, output, session, std_pages(FALSE), reactive({ if (is.null(ctx$key())) "Data" else ctx$part()$label }), pipeline = WB_PIPELINE)

  # outputs written by another window or the command line: read them again (this window's own runs already did)
  wb_watch_outputs(session, reactive(file.path(ctx$pd(), "output", "audit", "audit_index.csv")), std$rv)
  invisible(ctx)
}
