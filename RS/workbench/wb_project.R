# RS/workbench/wb_project.R  --  The Workbench's project views and navigation: Import data (the sources, the set-up check,
# the last build), Products and STAC export, the tree of Standard pages and set-up tables, and the tabs.
# Part of Ovid Core // RS. Sourced by wb_rs.R. The Standard pages run on the Standard server (app.R, embedded = TRUE),
# so every Standard process (scans included) works here the same way; this file only places them.

# Where each set-up table is edited: the Set-up tables page, and the tab with its editor (see core/workbench.R).
WB_OPEN <- stats::setNames(lapply(SETUP_TABLES$id, function(k) list(go = "Setup", tab = k)), SETUP_TABLES$id)

# The Workbench tabs (core wb_tabs_server). Reference is in the Pages tree and behind the ? button.
WB_PIPELINE <- list(list(label = "Needs you", go = "wb-needs"), list(label = "Photos and reference", go = "wb-data"), list(label = "Positioning", go = "wb-positioning"),
                    list(label = "Products", go = "wb-products"), list(label = "STAC export", go = "wb-stac"))

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_import_ui <- function() div(class = "wb-std d-tool wb-import",
  tags$h1(class = "wb-h1", "Import data"),
  p(class = "wb-lede", "The archive is built from sources (drives, folders, backups) scanned one after another and never written. A scan reads each image folder's headers once and again only where a file changed; a rebuild puts the catalogue together from every source's last scan, so a drive that is not plugged in stays in it, and reads no source at all."),
  div(span(class = "wb-eb", "Sources"), uiOutput("wb_scan")),
  div(class = "wb-inline", tags$a(href = "#", `data-ovid-go` = "Sources", HTML("Scan a folder or drive, or rescan the plugged-in ones, on the Sources page &rarr;"))),
  div(style = "margin-top:14px", span(class = "wb-eb", "Set-up check"), uiOutput("wb_check_short")),
  div(class = "wb-runrow", style = "margin-top:16px", uiOutput("wb_run_note", inline = TRUE)))   # Rebuild: the header button and Sources

# The Products and STAC export tabs: only those parts of the QA and Sources pages (pages/ui.R), under their own ids.
wb_products_ui <- function() div(class = "wb-std d-tool", tags$h1(class = "wb-h1", "Products"), rs_products_ui("wb_products_tbl"))
wb_stac_ui <- function() div(class = "wb-std d-tool", tags$h1(class = "wb-h1", "STAC export"),
  p(class = "wb-lede", "What goes to people without the archive: the STAC catalogue every rebuild writes, and the shareable viewer."),
  div(span(class = "wb-eb", "STAC catalogue"), rs_stac_ui("wb_stac_dl")),
  div(style = "margin-top:16px", span(class = "wb-eb", "Shareable viewer"), rs_viewer_ui("wb_x_title", "wb_x_make", "wb_x_dl")))

# ---- server -----------------------------------------------------------------------------------------------------------
wb_project_server <- function(input, output, session, ctx) {
  E <- ctx$E; std <- ctx$std
  # safe: the sources table's editor reports a read problem
  output$wb_scan <- renderUI({ std$rv$tick; x <- std$C(); live_stamp(cfg_path(CFG$project_dir, "sources")); s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())
    plugged <- if (nrow(s)) sum(dir.exists(path.expand(s$path))) else 0
    tile <- function(v, k, cls = "") div(class = paste("wb-tile", cls), div(class = "v", v), div(class = "k", k))
    # safe: an output of the last run, only shown
    m <- tryCatch(fread(file.path(path.expand(CFG$project_dir), "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) NULL)
    v <- function(k) if (!is.null(m) && nrow(m)) m$value[match(k, m$key)] %|% "0" else "0"
    div(class = "wb-tiles", tile(num(nrow(s)), "sources"), tile(num(plugged), "plugged in now"), tile(num(as.numeric(v("files"))), "files"),
        tile(num(as.numeric(v("images"))), "images"), tile(num(if (is.null(x)) 0 else nrow(x$flights)), "flights"),
        tile(num(if (is.null(x) || is.null(x$unassigned)) 0 else nrow(x$unassigned)), "files tied to no flight", if (!is.null(x) && NROW(x$unassigned)) "hi")) })
  # safe: no check result yet: nothing to show
  output$wb_check_short <- renderUI({ x <- tryCatch(std$chk(), error = function(e) NULL); if (is.null(x)) return(NULL)
    ne <- sum(x$level == "error"); nw <- sum(x$level == "warning")
    div(class = "wb-inline", if (!ne && !nw) span(class = "wb-good", "Safe to rebuild.") else span(class = if (ne) "wb-bad" else "wb-warnt",
      paste0(if (ne) paste(ne, if (ne == 1) "error" else "errors") else "", if (ne && nw) ", " else "", if (nw) paste(nw, if (nw == 1) "warning" else "warnings") else "",
             if (ne) ": a rebuild would ignore or misapply something." else ".")),
      if (nrow(x)) tags$a(href = "#", `data-ovid-go` = "Sources", "Open the set-up check")) })
  output$wb_run_note <- renderUI({ std$rv$tick
    if (isTRUE(std$stale())) span(class = "wb-warnt", "Set-up changed since the last build.")
    else if (!run_mtime(CFG$project_dir)) span(class = "wb-p", "Not built yet.")
    else span(class = "wb-p", style = "color:var(--wb-fg2)", paste("Last build", format(as.POSIXct(run_mtime(CFG$project_dir), origin = "1970-01-01"), "%Y-%m-%d %H:%M"))) })

  output$wb_setup_tree <- renderUI(wb_setup_tree(E, SETUP_TABLES, WB_OPEN, std$stale()))
  wb_tabs_server(input, output, session, std_pages(), reactive({ if (is.null(ctx$key())) "Data" else ctx$part()$label }), pipeline = WB_PIPELINE)
  # a build in another window, a background scan, or the command line: read the catalogue again
  wb_watch_outputs(session, reactive(file.path(path.expand(CFG$project_dir), "output", "audit", "run_manifest.csv")), std$rv)
  seen_cat <- reactiveVal(file_stamp(file.path(CAT(), "flights.csv")))
  observeEvent(std$rv$tick, { now <- file_stamp(file.path(CAT(), "flights.csv")); if (!identical(now, isolate(seen_cat()))) { seen_cat(now); std$C(load_cat()) } }, ignoreInit = TRUE)
  invisible(ctx)
}
