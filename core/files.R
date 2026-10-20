# Ovid/files.R  --  One way to bring folders in and send files out, the same in every Ovid app (the Ovid RS pattern).
# Part of Ovid Core (shared). Sourced by each app's app.R after shell.R; UI and server helpers only.
#
# Import: a "Browse..." button beside a folder box opens the system's own folder picker (Finder on a Mac, Explorer on
#   Windows) when the app runs on this computer. Where neither is there (or the app is opened from another computer),
#   it opens the in-app folder browser instead: places on the left, a clickable path, a filter, then the folders.
#   Files (uploads) use Shiny's fileInput, which is already the browser's own file dialog.
# Export: every download is named ovid_<app>_<what>_<YYYY-MM-DD>.<ext> and its button reads "Export ..." with the
#   format in brackets, so a file always says which app and day it came from.

OVID_FILES_VERSION <- "1.0.0"

# ---- Export ---------------------------------------------------------------------------------------------------------
# The standard file name. what is a short description ("compare sites", a site name...); date = NULL leaves the date out.
ovid_file_name <- function(app, what, ext, date = Sys.Date()) {
  w <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9-]+", "_", paste(what, collapse = "_")))
  paste0("ovid_", tolower(app), if (nzchar(w)) paste0("_", w), if (!is.null(date)) paste0("_", format(date, "%Y-%m-%d")), ".", ext)
}

# A download button with the standard label: "Export <what> (<FORMAT>)", or "Export <FORMAT>" without what.
# ext is the format ("csv", "png", "zip", "pdf", "html"); use "report" for a report whose format the user picks.
ovid_export_button <- function(id, ext, what = NULL, primary = FALSE, class = NULL) {
  fmt <- if (identical(ext, "report")) NULL else toupper(ext)
  label <- if (is.null(what)) paste("Export", fmt %||% "report") else paste0("Export ", what, if (!is.null(fmt)) paste0(" (", fmt, ")"))
  downloadButton(id, label, class = paste(c(if (primary) "btn-primary", class), collapse = " "))
}

# The matching downloadHandler. what and ext may be functions (read when the download starts, so they can use inputs).
ovid_download <- function(app, what, ext, content, date = Sys.Date())
  downloadHandler(filename = function() ovid_file_name(app, if (is.function(what)) what() else what, if (is.function(ext)) ext() else ext, date),
                  content = content)

# For an app with many downloads: put `downloadHandler <- ovid_downloads("WLR")` at the top of app.R and every
# download it makes gets the standard name. The name the handler gives ("compare_sites.csv", "wlr_export_20261005.zip")
# becomes the what: a leading app or "ovid" prefix and a trailing date are dropped, today's date is added.
ovid_std_name <- function(app, name) {
  ext <- tools::file_ext(name); stem <- if (nzchar(ext)) substr(name, 1, nchar(name) - nchar(ext) - 1) else name
  stem <- sub(paste0("^(ovid[_ -]+)?(", tolower(app), "[_ -]+)?"), "", stem, ignore.case = TRUE)
  stem <- sub("[_ -]*(19|20)[0-9]{2}-?[01][0-9]-?[0-3][0-9]$", "", stem)
  ovid_file_name(app, stem, if (nzchar(ext)) ext else "csv")
}
ovid_downloads <- function(app) function(filename, content, ...)
  shiny::downloadHandler(filename = function() ovid_std_name(app, if (is.function(filename)) filename() else filename), content = content, ...)

# ---- Import: folders ----------------------------------------------------------------------------------------------
# The button. Put it beside the textInput / textAreaInput with the same id; ovid_browse() wires it up.
ovid_browse_button <- function(id, label = "Browse\u2026")
  actionButton(paste0("pick_", id), label, icon = icon("folder-open"), class = "btn-sm d-browse")

# The system's own folder picker: a path, NA when cancelled, NULL when there is none on this system.
ovid_pick_folder <- function(start = "", prompt = "Choose a folder") {
  if (Sys.info()[["sysname"]] == "Darwin") {
    cmd <- sprintf('POSIX path of (choose folder with prompt "%s"%s)', gsub('"', "'", prompt),
                   if (nzchar(start) && dir.exists(start)) sprintf(' default location (POSIX file "%s")', start) else "")
    # safe: a folder dialog that cannot open returns nothing chosen
    r <- suppressWarnings(tryCatch(system2("osascript", c("-e", shQuote(cmd)), stdout = TRUE, stderr = FALSE), error = function(e) character()))
    return(if (length(r) && nzchar(r[1])) sub("(.)/$", "\\1", r[1]) else NA_character_) }
  if (.Platform$OS.type == "windows") {
    ps <- paste0("Add-Type -AssemblyName System.Windows.Forms; $d = New-Object System.Windows.Forms.FolderBrowserDialog; ",
                 "$d.Description = '", gsub("'", "", prompt), "'; ",
                 if (nzchar(start) && dir.exists(start)) paste0("$d.SelectedPath = '", gsub("/", "\\\\", start), "'; ") else "",
                 "if ($d.ShowDialog() -eq 'OK') { $d.SelectedPath }")
    # safe: a folder dialog that cannot open returns nothing chosen
    r <- suppressWarnings(tryCatch(system2("powershell", c("-NoProfile", "-STA", "-Command", shQuote(ps)), stdout = TRUE), error = function(e) character()))
    return(if (length(r) && nzchar(r[1])) gsub("\\\\", "/", r[1]) else NA_character_) }
  NULL
}

# Wire up every Browse button on the page. ids: the folder boxes that have a ovid_browse_button. add: the boxes that
# hold several folders, one per line (a pick is added on a new line). prompts: optional named titles for the picker.
# places: a function returning extra named folders for the in-app browser (the app's project folder, its sources).
ovid_browse <- function(input, output, session, ids, add = character(), prompts = NULL, places = NULL) {
  bdir <- reactiveVal(NULL); target <- reactiveVal(NULL)
  local_app <- function() isTRUE(session$clientData$url_hostname %in% c("127.0.0.1", "localhost", "::1"))
  first <- function(id) { v <- trimws(strsplit(input[[id]] %||% "", "\n")[[1]]); v <- v[nzchar(v)]
    if (length(v)) path.expand(v[length(v)]) else "" }
  put <- function(id, d) {
    old <- trimws(input[[id]] %||% "")
    updateTextInput(session, id, value = if (id %in% add && nzchar(old)) paste(old, d, sep = "\n") else d) }
  all_places <- function() {
    v <- Sys.glob("/Volumes/*"); w <- Sys.glob("[A-Z]:/")
    p <- c(stats::setNames(v, basename(v)), Home = "~", Desktop = "~/Desktop", Documents = "~/Documents", Downloads = "~/Downloads",
           # safe: the shortcuts to the app's folders are a convenience
           stats::setNames(w, w), if (is.function(places)) tryCatch(places(), error = function(e) NULL))
    p <- vapply(p, path.expand, ""); p[dir.exists(p) & !duplicated(p)] }
  jsgo <- function(x) sprintf("Shiny.setInputValue('d_fb_go', %s, {priority: 'event'}); var f=document.getElementById('d_fb_filter'); if (f) f.value=''; return false;",
                              .d_json_str(x))
  lapply(ids, function(id) observeEvent(input[[paste0("pick_", id)]], {
    start <- first(id); prompt <- prompts[[id]] %||% "Choose a folder"
    if (local_app()) { r <- ovid_pick_folder(if (dir.exists(start)) start else "", prompt)
      if (!is.null(r)) { if (!is.na(r)) put(id, r); return() } }
    target(id); bdir(if (nzchar(start) && dir.exists(start)) normalizePath(start, winslash = "/") else NULL)
    pl <- all_places()
    showModal(modalDialog(title = prompt, easyClose = TRUE, size = "l",
      div(class = "d-fb", div(class = "d-fb-side", div(class = "d-fb-h", "Places"),
            lapply(names(pl), function(n) tags$a(href = "#", class = "d-fb-place", title = pl[[n]], onclick = jsgo(pl[[n]]), n))),
          div(class = "d-fb-main", uiOutput("d_fb_crumbs"),
            tags$input(id = "d_fb_filter", class = "form-control input-sm form-control-sm d-fb-filter", placeholder = "Filter these folders",
              oninput = "var q=this.value.toLowerCase();document.querySelectorAll('.d-fb-row').forEach(function(r){r.style.display=r.dataset.n.indexOf(q)<0?'none':''});"),
            uiOutput("d_fb_list"))),
      footer = tagList(div(class = "d-fb-sel", textOutput("d_fb_sel", inline = TRUE)), actionButton("d_fb_up", "Up", icon = icon("arrow-up")),
                       modalButton("Cancel"), actionButton("d_fb_ok", "Use this folder", class = "btn-primary")))) }))
  output$d_fb_crumbs <- renderUI({ d <- bdir(); if (is.null(d)) return(div(class = "d-fb-crumbs dim", "Pick a place on the left"))
    parts <- strsplit(d, "/", fixed = TRUE)[[1]]; parts[1] <- if (nzchar(parts[1])) parts[1] else "/"
    acc <- Reduce(function(a, b) if (a == "/") paste0("/", b) else paste0(a, "/", b), parts, accumulate = TRUE)
    div(class = "d-fb-crumbs", lapply(seq_along(parts), function(i) tagList(if (i > 1 && parts[i - 1] != "/") span(class = "dim", " / "),
      tags$a(href = "#", onclick = jsgo(acc[i]), parts[i])))) })
  output$d_fb_sel <- renderText(if (is.null(bdir())) "No folder open" else paste("Selected:", bdir()))
  output$d_fb_list <- renderUI({ d <- bdir(); if (is.null(d)) return(NULL)
    x <- list.dirs(d, recursive = FALSE, full.names = TRUE); x <- x[!startsWith(basename(x), ".")]; x <- x[order(tolower(basename(x)))]
    n_files <- length(list.files(d, all.files = FALSE, no.. = TRUE)) - length(x)
    div(class = "d-fb-list", if (!length(x)) p(class = "dim", style = "padding:8px", "No folders inside") else
      lapply(x, function(f) tags$a(href = "#", class = "d-fb-row", `data-n` = tolower(basename(f)), onclick = jsgo(f), icon("folder"), basename(f))),
      p(class = "dim", style = "margin:8px 0 0", sprintf("%s folders, %s files here", format(length(x), big.mark = ","), format(max(n_files, 0), big.mark = ",")))) })
  observeEvent(input$d_fb_go, if (dir.exists(input$d_fb_go)) bdir(normalizePath(input$d_fb_go, winslash = "/")))
  observeEvent(input$d_fb_up, { d <- bdir(); if (is.null(d)) return(); u <- dirname(d); if (u != d) bdir(u) })
  observeEvent(input$d_fb_ok, { d <- bdir(); if (is.null(d)) return(showNotification("Open a folder first", type = "warning"))
    put(target(), d); removeModal() })
  invisible(NULL)
}

.d_json_str <- function(x) paste0('"', gsub("([\"\\\\])", "\\\\\\1", x), '"')

# The browser's look, from the shell's tokens (ovid_page adds it to every page). Works on Bootstrap 3 and 5.
OVID_FILES_CSS <- paste(readLines(file.path(CORE_DIR, "www", "files.css"), encoding = "UTF-8"), collapse = "\n")
