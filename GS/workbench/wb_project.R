# GS/workbench/wb_project.R  --  The Workbench's project views and navigation: Import data (the folders, a scan of them,
# the set-up check, Scan), the tree of Standard pages and set-up tables, and the tabs.
# Part of Ovid Core // GS. Sourced by wb_gs.R. The Standard pages run on the Standard server (app.R, embedded = TRUE),
# so every Standard process works here the same way; this file only places them.

# Where each set-up table is edited: the Set-up tables page, and the tab with its editor (see core/workbench.R); one with no
# tab (electrode_status.csv) on the page that writes it.
WB_OPEN <- stats::setNames(lapply(SETUP_TABLES$id, function(k) if (SETUP_TABLES$tab[SETUP_TABLES$id == k]) list(go = "Setup", tab = k) else list(go = "QC")), SETUP_TABLES$id)

WB_PIPELINE <- list(list(label = "Panels", go = "wb-stack", area = "ERT"), list(label = "Edit data", go = "wb-editdata", area = "ERT"),
                    list(label = "Data", go = "wb-data", area = "ERT"), list(label = "Reference", go = "Reference"))   # the Workbench tabs (core wb_tabs_server)

# The Workbench's sides: its own tabs (by area above) first, then the Standard pages of each side (GS_AREAS, pages/ui.R).
WB_AREAS <- stats::setNames(lapply(names(GS_AREAS), function(a) c(unlist(lapply(WB_PIPELINE, function(x) if (identical(x$area, a)) x$go)), GS_AREAS[[a]])), names(GS_AREAS))

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_import_ui <- function() div(class = "wb-std d-tool wb-import",
  tags$h1(class = "wb-h1", "Import data"),
  p(class = "wb-lede", "The archive folders are read and never written. A scan reads new and changed files, works out which are surveys, copies, subsets and processed sets, reads their measurements, applies the QC profiles, the manual flags and the electrodes turned off, and writes the catalogue, a folder per survey and the audit trail to the project folder. A survey whose files and set-up are unchanged is reused."),
  div(class = "wb-card", style = "max-width:900px", std_folders()),
  div(span(class = "wb-eb", "Scan"), uiOutput("wb_scan")),
  div(style = "margin-top:14px", span(class = "wb-eb", "Set-up check"), uiOutput("wb_check_short")),
  div(class = "wb-runrow", style = "margin-top:16px",
    tags$button(type = "button", class = "wb-btn primary big", onclick = "document.getElementById('rebuild').click()", HTML("&#9656; Scan")), uiOutput("wb_run_note", inline = TRUE)))

# ---- server -----------------------------------------------------------------------------------------------------------
# ctx: the environment of wb_server (std, E, key, part, view).
wb_project_server <- function(input, output, session, ctx) {
  E <- ctx$E; std <- ctx$std
  dirs <- function(x) { d <- trim(strsplit(x %||% "", "\n")[[1]]); d[nzchar(d)] }
  raw_d <- debounce(reactive(input$set_raw), 700)
  found <- reactive({ d <- path.expand(dirs(raw_d())); d <- d[dir.exists(d)]
    rbindlist(lapply(d, function(r) { f <- list.files(r, recursive = TRUE); f <- f[!startsWith(basename(f), "._")]
      data.table(rel = f, size = file.size(file.path(r, f))) })) })
  output$wb_scan <- renderUI({ std$rv$tick; f <- found(); if (!nrow(f)) f <- data.table(rel = character(), size = numeric())
    cf <- file.path(PROJECT, "output", "catalogue", "files.csv")
    # safe: an output of the last run, only shown
    m <- if (file.exists(cf)) tryCatch(fread(cf, colClasses = "character", na.strings = NULL)[!grepl("!/", rel, fixed = TRUE)], error = function(e) NULL) else NULL
    i <- if (!is.null(m)) match(f$rel, m$rel) else rep(NA_integer_, nrow(f))
    new <- sum(is.na(i)); changed <- sum(!is.na(i) & suppressWarnings(as.numeric(m$size[i])) != f$size, na.rm = TRUE)
    s <- std$cat_s(); o <- s[kind %in% c("original", "merged")]
    tile <- function(v, k, cls = "") div(class = paste("wb-tile", cls), div(class = "v", v), div(class = "k", k))
    div(class = "wb-tiles", tile(num(nrow(f)), "files in the folders"), tile(num(new), "new since the last scan", if (new) "hi"), tile(num(changed), "changed size", if (changed) "hi"),
        tile(num(nrow(o)), "surveys"), tile(num(sum(o$n_read, na.rm = TRUE)), "measurements"),
        tile(num(sum(s$status %in% c("needs_reader", "unreadable"), na.rm = TRUE)), "files not read", if (any(s$status %in% c("needs_reader", "unreadable"))) "hi")) })
  # safe: no check result yet: nothing to show
  output$wb_check_short <- renderUI({ x <- tryCatch(std$chk(), error = function(e) NULL); if (is.null(x)) return(NULL)
    ne <- sum(x$level == "error"); nw <- sum(x$level == "warning")
    div(class = "wb-inline", if (!ne && !nw) span(class = "wb-p", "Nothing to fix before a scan.") else span(class = "wb-p",   # plain text: no colours (no lights in ERT)
      paste0(if (ne) paste(ne, if (ne == 1) "error" else "errors") else "", if (ne && nw) ", " else "", if (nw) paste(nw, if (nw == 1) "warning" else "warnings") else "",
             if (ne) ": a scan would ignore or misapply something." else ".")),
      if (nrow(x)) tags$a(href = "#", `data-ovid-go` = "Settings", "Open the set-up check")) })
  output$wb_run_note <- renderUI({ std$rv$tick
    if (isTRUE(std$stale())) span(class = "wb-warnt", "Set-up changed since the last scan.")
    else if (!run_mtime(PROJECT)) span(class = "wb-p", "Not scanned yet.")
    else span(class = "wb-p", style = "color:var(--wb-fg2)", paste("Last scan", format(as.POSIXct(run_mtime(PROJECT), origin = "1970-01-01"), "%Y-%m-%d %H:%M"))) })

  # the set-up tables: row counts, a dot for unsaved edits; a click opens Set-up tables on the table's tab
  output$wb_setup_tree <- renderUI(wb_setup_tree(E, SETUP_TABLES[(tab)], WB_OPEN, std$stale()))

  # tabs: Import, the survey in view, and the pages opened last (up to five)
  wb_tabs_server(input, output, session, std_pages(FALSE), reactive({ if (is.null(ctx$key())) "Data" else ctx$part()$label }), pipeline = WB_PIPELINE)

  # a rebuild in another window or from the command line: read the catalogue again (this window's own rebuilds already do)
  wb_watch_outputs(session, reactive({ std$proj(); file.path(PROJECT, "output", "audit", "run_manifest.csv") }), std$rv)
  invisible(ctx)
}
