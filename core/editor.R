# Ovid/editor.R  --  The set-up table editor: one Shiny module for every config CSV, in every app and both interfaces.
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): app.R sources it, no pipeline does.
#
# The same table can be open in the Standard interface, in the Workbench and in several windows. Each copy remembers the
# file as it read it (file_stamp); a copy with no unsaved edits takes up a newer saved version by itself at once (the live
# view below), and one with unsaved edits asks before it overwrites it. Changes made from code (append, mutate, replace)
# start from the saved version and are saved at once. Every save keeps the version before it in config/history
# (backup_cfg, config.R). In viewer mode (VIEWER set by the app) nothing is written.
#
#   ui:      ovid_edit_ui(id)
#   server:  E <- ovid_edit_server(id, path = reactive(<csv>), reload = reactive(<tick>), locked = <columns>,
#                                    on_change = <called after a save that changed the table>, dt = <the app's table renderer>)
#   E$data() the table; E$save(); E$dirty(); E$fresh(); E$select(rows); E$selected(); E$append(rows); E$mutate(fn);
#   E$replace(d); E$set_selected(col, value); E$discard()
# Paste and Import (core/metasheet_ui.R, when the app sources it) add rows copied from a spreadsheet, or a CSV or Excel
# file, with its columns matched to the table's by heading.

source(file.path(CORE_DIR, "sheet.R"), local = TRUE)       # Ovid/sheet.R: the Workbench shows each table as a spreadsheet

# Unsaved edits, kept for the app (both views run in one R process): a table changed and not saved in one view is opened
# with those changes in the other, still to Apply or Save. Keyed by the file, with the version of the file they started from.
OVID_PENDING <- new.env(parent = emptyenv())

file_stamp <- function(p) if (length(p) == 1 && !is.na(p) && file.exists(p)) paste(format(as.numeric(file.mtime(p)), digits = 15), file.size(p)) else ""

# ---- one live view of the set-up, shared by every page of a window ----------------------------------------------------------
# A page never keeps its own copy of a set-up table, the deployment sheet or the app's settings: it reads them through these,
# so an edit made anywhere (a table saved on another page, a rename, a switch, an import, another window, a run, the file
# changed by hand) shows on every page within LIVE_EVERY. One check per window looks at every file any page reads.
#   live_stamp(path)        the file as it stands (changes when it is saved); depend on it to redraw when it changes
#   live_table(path)        the table itself, read again when it changes (a data.table; empty when there is none)
#   live_config(project)    every set-up table of a project (config/*.csv): for what reads several of them
#   live_settings()         the app's settings (SETTINGS_FILE), read again when they are saved
#   live_touch()            look now (after a page writes a file, so the window does not wait for the next look)
LIVE_EVERY <- 1000
live_store <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  ud <- session$userData; if (!is.null(ud$ovid_live)) return(ud$ovid_live)
  w <- new.env(parent = emptyenv())                                   # key -> list(now = reactiveVal(stamp), stamp = function())
  look <- function() for (k in ls(w, all.names = TRUE)) { x <- w[[k]]; s <- tryCatch(x$stamp(), error = function(e) "")
    if (!identical(s, shiny::isolate(x$now()))) x$now(s) }
  watch <- function(key, stamp) { x <- w[[key]]
    if (is.null(x)) { x <- list(now = shiny::reactiveVal(tryCatch(stamp(), error = function(e) "")), stamp = stamp); assign(key, x, envir = w) }
    x$now() }
  shiny::isolate(observe({ shiny::invalidateLater(LIVE_EVERY, session); look() }, domain = session))
  st <- list(watch = watch, look = look, tables = new.env(parent = emptyenv()))
  ud$ovid_live <- st; st
}
live_path <- function(path) if (length(path) == 1 && !is.na(path) && nzchar(path)) normalizePath(path.expand(path), winslash = "/", mustWork = FALSE) else ""
live_stamp <- function(path) { st <- live_store(); p <- live_path(path)
  if (is.null(st) || !nzchar(p)) return(file_stamp(p)); st$watch(paste0("file:", p), function() file_stamp(p)) }
live_config <- function(project_dir) { if (length(project_dir) != 1 || is.na(project_dir) || !nzchar(project_dir)) return("")
  st <- live_store(); d <- live_path(file.path(project_dir, "config"))
  f <- function() { x <- list.files(d, "\\.csv$", full.names = TRUE); paste(basename(x), vapply(x, file_stamp, ""), collapse = ";") }
  if (is.null(st)) return(f()); st$watch(paste0("dir:", d), f) }
live_table <- function(path) { live_stamp(path); p <- path.expand(path %||% "")
  if (length(p) != 1 || !nzchar(p) || !file.exists(p)) return(data.table())
  tryCatch(read_cfg(p), error = function(e) data.table()) }
live_settings <- function(file = get0("SETTINGS_FILE")) { if (is.null(file)) return(list()); live_stamp(file); settings_read(file) }
live_touch <- function() { st <- live_store(); if (!is.null(st)) st$look(); invisible() }

# A setting is saved when it is changed, not at the next run: save() (the app's own, which keeps every other saved setting)
# is called a moment after the last change to any of the inputs `ids`. The values the page opens with are not saved again.
# The other way too: a setting saved in another window is put into this window's field, so a run started here never writes
# an old value back. keys: the saved setting each input holds, when its name differs (GS set_project = project_dir);
# a list setting is shown joined by sep (raw folders one per line, others with commas).
settings_autosave <- function(ids, save, keys = stats::setNames(ids, ids), sep = c(raw_dirs = "\n"), wait = 800,
                              settings_file = get0("SETTINGS_FILE"), session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session) || isTRUE(get0("VIEWER", inherits = TRUE))) return(invisible())
  keys <- keys[ids]; keys[is.na(keys)] <- ids[is.na(keys)]; names(keys) <- ids
  now <- shiny::debounce(shiny::reactive(lapply(stats::setNames(ids, ids), function(i) session$input[[i]])), wait)
  seen <- NULL
  mine <- if (is.null(settings_file)) list() else settings_read(settings_file)[unname(keys)]   # the saved values this window has
  observeEvent(now(), { x <- now(); x <- x[!vapply(x, is.null, NA)]
    old <- seen; seen <<- utils::modifyList(seen %||% list(), x)
    if (is.null(old) || !length(x)) return()                           # the values the page opened with
    if (identical(old[names(x)], x)) return()
    tryCatch({ save(); if (!is.null(settings_file)) mine <<- settings_read(settings_file)[unname(keys)]; live_touch() },
             error = function(e) shiny::showNotification(paste("The setting was not saved:", conditionMessage(e)), type = "error", duration = NULL)) },
    ignoreInit = FALSE)
  if (!is.null(settings_file)) observeEvent(live_settings(settings_file), {
    s <- live_settings(settings_file)
    for (i in ids) { k <- keys[[i]]; v <- s[[k]]
      if (is.null(v) || identical(v, mine[[k]]) || is.null(session$input[[i]])) next          # unchanged, or not on this page
      show <- if (is.logical(v) || (is.numeric(v) && length(v) == 1)) v else paste(as.character(v), collapse = if (k %in% names(sep)) sep[[k]] else ", ")
      session$sendInputMessage(i, list(value = show)); mine[k] <<- list(v) }
    }, ignoreInit = TRUE)
  invisible()
}

edit_user <- function(session) { u <- tryCatch(session$user, error = function(e) NULL)
  if (is.null(u) || !nzchar(u)) u <- tryCatch(session$rootScope()$user, error = function(e) NULL)
  if (is.null(u) || !nzchar(u)) u <- Sys.info()[["user"]]; u }

ovid_edit_ui <- function(id, hint = "Double-click a cell to edit. Click rows to select them.", import = exists("table_import_server", mode = "function")) { ns <- NS(id)
  tagList(div(class = "btnrow editor-only",
      actionButton(ns("add"), "Add row", icon = icon("plus")), actionButton(ns("dup"), "Duplicate"), actionButton(ns("del"), "Delete", class = "btn-danger"),
      if (import) actionButton(ns("paste"), "Paste\u2026", icon = icon("paste"), title = "Rows copied from Excel or typed, one per line"),
      if (import) actionButton(ns("import"), "Import\u2026", icon = icon("file-import"), title = "A CSV or Excel file, its columns matched to this table's"),
      actionButton(ns("undo"), "Undo", icon = icon("rotate-left"), title = "Take back the last change not yet saved"),
      actionButton(ns("save"), "Save changes", class = "btn-primary"), span(class = "dim", textOutput(ns("state"), inline = TRUE)),
      span(class = "dim push", hint)),
    div(class = "dt-host", DT::DTOutput(ns("tbl"))),
    div(class = "ds-host", style = "display:none", ovid_sheet_ui(ns("sheet"))),        # the Workbench's sheet (core/sheet.R)
    uiOutput(ns("checks"))) }

# dt: the app's table renderer, called as dt(d, selection =, page =, opts =, editable =) (ovid_dt, table.R).
# A set-up table as the editor reads it. One that cannot be read, or lacks columns the app expects (CONFIG_FILES, the
# app's list of set-up tables: damaged, cut short, or saved by another program), is shown with those columns added empty
# and a notice, so no page stops on it; the file itself is only changed when the table is saved.
edit_cols <- function(p) { reg <- get0("CONFIG_FILES", inherits = TRUE); if (!is.list(reg)) return(NULL)
  for (x in reg) if (identical(x$file, basename(p))) { cc <- x$cols; cc <- if (is.function(cc)) tryCatch(cc(), error = function(e) NULL) else cc
    op <- x$optional; if (is.function(op)) op <- tryCatch(op(), error = function(e) NULL)
    return(structure(cc, optional = op)) }                  # optional columns (an app's own) are added without a notice
  NULL }
edit_read <- function(p) {
  d <- tryCatch(suppressWarnings(read_cfg(p)), error = function(e) e)
  note <- function(...) if (!is.null(shiny::getDefaultReactiveDomain())) shiny::showNotification(paste0(...), type = "warning", duration = 20)
  if (inherits(d, "error")) { note(basename(p), " could not be read (", conditionMessage(d), "). It is shown empty; fix or replace the file, or save a new table over it."); d <- data.table() }
  d <- as.data.table(d); want <- edit_cols(p); miss <- setdiff(want, names(d)); opt <- intersect(miss, attr(want, "optional")); miss <- setdiff(miss, opt)
  for (k in opt) d[, (k) := ""]
  if (length(miss)) { if (length(want) && length(miss) < length(want) || nrow(d))
      note(basename(p), " is missing column", if (length(miss) > 1) "s", " ", paste(utils::head(miss, 6), collapse = ", "), if (length(miss) > 6) ", ...",
           ": shown added empty. It may be damaged or from another version; check the table, then save it.")
    for (k in miss) d[, (k) := ""] }
  as.data.frame(d, stringsAsFactors = FALSE) }

ovid_edit_server <- function(id, path, reload, locked = character(), on_change = function() NULL, dt = NULL) moduleServer(id, function(input, output, session) {
  ro <- function() isTRUE(get0("VIEWER", inherits = TRUE))
  show <- if (is.function(dt)) dt else function(d, ..., page = 25, opts = list())
    DT::datatable(d, rownames = FALSE, ..., options = utils::modifyList(list(pageLength = page, scrollX = TRUE), opts))
  data <- reactiveVal(NULL); ver <- reactiveVal(0); dirty <- reactiveVal(FALSE); loaded <- reactiveVal(""); want <- reactiveVal(NULL)
  undo <- reactiveVal(list())                                        # the table before each change since it was last read or saved (up to 30)
  change <- function(d, redraw = TRUE) { u <- isolate(undo()); undo(utils::tail(c(u, list(isolate(data()))), 30)); data(d); dirty(TRUE); if (redraw) bump() }
  bump <- function() ver(isolate(ver()) + 1)
  pkey <- function() live_path(isolate(path()))
  load_disk <- function(restore = TRUE) { p <- isolate(path())
    if (length(p) == 1 && exists("config_migrate", mode = "function") && basename(p) %in% vapply(config_folds(), function(f) f$into, "") && basename(dirname(p)) == "config")
      config_migrate(dirname(dirname(p)), "editor")                    # an older table that folds into this one moves in first (core config.R)
    data(if (file.exists(p)) edit_read(p) else NULL)
    loaded(file_stamp(p)); dirty(FALSE); undo(list())
    pe <- if (restore && nzchar(pkey())) OVID_PENDING[[pkey()]] else NULL                 # unsaved edits from the other view, on this version
    if (!is.null(pe) && identical(pe$stamp, file_stamp(p)) && !is.null(isolate(data())) && identical(names(pe$data), names(isolate(data())))) { data(pe$data); dirty(TRUE); held <<- TRUE }
    bump() }
  held <- FALSE                                                       # this copy put the unsaved edits there (only it takes them away)
  observe({ d <- data(); dr <- dirty(); k <- pkey(); if (!nzchar(k)) return()
    if (dr && !is.null(d)) { assign(k, list(stamp = isolate(loaded()), data = d), envir = OVID_PENDING); held <<- TRUE }
    else if (held && exists(k, envir = OVID_PENDING, inherits = FALSE)) { rm(list = k, envir = OVID_PENDING); held <<- FALSE } })
  read_from <- reactiveVal("")                                       # the file this copy was read from
  observe({ reload(); p <- path()                                    # a reload keeps unsaved edits to the same, unchanged file
    isolate(if (!identical(p, read_from()) || !dirty() || !identical(file_stamp(p), loaded())) { read_from(p); load_disk() }) })
  # the file saved by another page, another window or a run: taken up at once (core live_stamp), unless this copy has unsaved
  # edits (then Save asks); on_change tells the app's pages, as a save here does
  observeEvent(live_stamp(tryCatch(path(), error = function(e) "")), if (!identical(file_stamp(isolate(path())), isolate(loaded())) && !isolate(dirty())) { load_disk(); on_change() }, ignoreInit = TRUE)
  conflict <- function() showModal(modalDialog(title = paste(basename(isolate(path())), "was saved elsewhere"),
    p("Another window or the other interface saved this table after it was opened here, and this copy has unsaved edits."),
    footer = tagList(actionButton(session$ns("cf_mine"), "Save mine over it", class = "btn-danger"),
                     actionButton(session$ns("cf_theirs"), "Load the saved version (drops these edits)"), modalButton("Cancel")), easyClose = TRUE))
  fresh <- function() { p <- isolate(path())                # TRUE when this copy is the saved version (taking it up if it can)
    if (identical(file_stamp(p), isolate(loaded()))) return(TRUE)
    if (isolate(dirty())) { conflict(); return(FALSE) }
    load_disk(); TRUE }
  output$tbl <- DT::renderDT({
    ver(); d <- isolate(data())
    validate(need(!is.null(d), "This table is created the first time the pipeline runs."))
    w <- isolate(want()); w <- w[w <= nrow(d)]                           # rows asked for by select(), on their page
    show(d, selection = list(mode = "multiple", selected = if (length(w)) w else NULL), page = 25,
         opts = if (length(w)) list(displayStart = (min(w) - 1) %/% 25 * 25) else list(),
         editable = if (ro()) FALSE else list(target = "cell", disable = list(columns = which(names(d) %in% locked) - 1L)))
  }, server = FALSE)
  # In the Workbench the table is a sheet (core/sheet.R): sent whenever it is read or saved, and its Apply saves it here.
  in_wb <- isTRUE(tryCatch(exists("wb_wanted", mode = "function") && wb_wanted(isolate(session$clientData$url_search), get0("SETTINGS")$ui), error = function(e) FALSE))
  if (in_wb) observe({ ver(); d <- isolate(data()); p <- isolate(path())
    base <- if (isolate(dirty()) && length(p) == 1 && file.exists(p)) edit_read(p) else NULL       # the saved table, so unsaved edits show as changes
    ovid_sheet_send(session, session$ns("sheet"), d, locked, ro(), label = if (length(p) == 1) basename(p) else "", base = base) })
  # the sheet's changes not yet applied: held as this editor's unsaved edits (so the Standard view has them too)
  observeEvent(input$sheet_pending, { if (ro()) return(); m <- input$sheet_pending; d <- isolate(data()); if (is.null(d)) return()
    rows <- lapply(m$rows, function(r) vapply(r, function(v) if (is.null(v)) "" else as.character(v), ""))
    nd <- if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, function(r) { length(r) <- ncol(d); r[is.na(r)] <- ""; r })), stringsAsFactors = FALSE) else d[0, , drop = FALSE]
    names(nd) <- names(d); data(nd); dirty(isTRUE(m$dirty)) })
  observeEvent(input$sheet_apply, { if (ro()) return(); m <- input$sheet_apply; d <- isolate(data()); if (is.null(d)) return()
    rows <- lapply(m$rows, function(r) vapply(r, function(v) if (is.null(v)) "" else as.character(v), ""))
    nd <- if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, function(r) { length(r) <- ncol(d); r[is.na(r)] <- ""; r })), stringsAsFactors = FALSE) else d[0, , drop = FALSE]
    names(nd) <- names(d)
    if (!identical(file_stamp(isolate(path())), isolate(loaded()))) { if (isolate(dirty())) conflict() else { load_disk(); showNotification(paste(basename(isolate(path())), "was saved elsewhere and has been read again: make the changes once more."), type = "warning", duration = 8) }; return() }
    data(nd); dirty(TRUE); write_now(nd, quiet = TRUE)
    showNotification(paste("Saved", basename(isolate(path())), "\u00b7 the version before is in history. Run to apply."), type = "message", duration = 5); bump() })
  observeEvent(input$tbl_cell_edit, { if (ro()) return()
    e <- input$tbl_cell_edit; d <- data()
    d[e$row, e$col + 1L] <- trim(as.character(e$value)); change(d, redraw = FALSE) })
  observeEvent(input$add, { d <- data(); req(d); d[nrow(d) + 1L, ] <- ""; change(d) })
  observeEvent(input$dup, { d <- data(); s <- input$tbl_rows_selected; req(d, length(s) > 0)
    change(rbind(d, d[s, , drop = FALSE])) })
  observeEvent(input$del, { d <- data(); s <- input$tbl_rows_selected; req(d, length(s) > 0)
    change(d[-s, , drop = FALSE]) })
  observeEvent(input$undo, { u <- undo(); if (!length(u)) return(showNotification("Nothing to undo since the table was saved.", type = "message"))
    data(u[[length(u)]]); undo(u[-length(u)]); dirty(length(u) > 1L); bump() })   # back to the saved table: nothing to save
  # what the table's rules (core table_rules.R) find in the table as it stands, saved or not
  output$checks <- renderUI({ ver(); input$tbl_cell_edit; d <- data(); if (is.null(d) || !nrow(d) || !exists("table_check", mode = "function")) return(NULL)
    f <- basename(isolate(path())); x <- table_findings(as.data.table(d), f, tryCatch(dirname(dirname(isolate(path()))), error = function(e) NULL))
    if (!nrow(x)) return(NULL)
    div(class = "edit-checks", style = "margin-top:8px", tags$b(sprintf("%d thing(s) to look at in this table", nrow(x))),
        tags$ul(style = "margin:4px 0 0 0", lapply(seq_len(min(nrow(x), 12)), function(i) tags$li(class = if (x$level[i] == "error") "text-danger" else "text-warning",
          sprintf("Row %d, %s: %s", x$row[i], x$column[i], x$problem[i])))), if (nrow(x) > 12) p(class = "dim", "and ", nrow(x) - 12, " more.")) })
  write_now <- function(d, quiet) {
    p <- isolate(path()); was_dirty <- isolate(dirty())
    d <- d[rowSums(d != "") > 0, , drop = FALSE]
    try(backup_cfg(p, d, edit_user(session)), silent = TRUE)
    fwrite_safe(d, p); data(d); dirty(FALSE); loaded(file_stamp(p)); undo(list()); live_touch()
    if (was_dirty) on_change()
    if (!quiet) showNotification("Saved. Run the pipeline to apply.", type = "message")
  }
  save <- function(quiet = FALSE) {
    if (ro()) return(invisible())                         # viewer mode never writes
    d <- isolate(data()); if (is.null(d)) return(invisible())
    if (!identical(file_stamp(isolate(path())), isolate(loaded()))) {     # saved elsewhere since this copy was read
      if (isolate(dirty())) conflict() else load_disk()
      return(invisible())
    }
    write_now(d, quiet)
  }
  observeEvent(input$save, save())
  if (exists("table_import_server", mode = "function"))         # Paste and Import (core/metasheet_ui.R): rows added, or the table replaced
    table_import_server(input, output, session, reactive({ d <- data(); s <- get0("IMPORT_SPECS", ifnotfound = list())[[basename(isolate(path()))]]   # headings and aliases a table declares
      if (!is.null(s)) meta_defaults(s) else {                                              # no declared fields: types judged from the column names and the table's rules (core import.R)
        ent <- Filter(function(x) identical(x$file, basename(isolate(path()))), get0("CONFIG_FILES", ifnotfound = list())); meta_defaults(import_spec_guess(if (is.null(d)) character() else names(d), if (length(ent)) ent[[1]])) } }),
      put = function(new, mode, s) { if (ro() || !fresh()) return(FALSE); d <- isolate(data()); if (is.null(d)) return(FALSE)
        new <- as.data.frame(ensure_cols(as.data.table(new), names(d))[, names(d), with = FALSE], stringsAsFactors = FALSE)
        new <- new[rowSums(new != "") > 0, , drop = FALSE]
        data(if (identical(mode, "replace")) new else rbind(d, new)); dirty(TRUE); save(quiet = TRUE); bump(); TRUE },
      modes = c("add", "replace"), new_cols = FALSE,
      existing = function() { d <- data(); if (is.null(d)) NULL else as.data.table(d) },
      memory = { cfg <- function() { p <- tryCatch(path(), error = function(e) NULL); if (length(p) == 1 && !is.na(p) && basename(dirname(p)) == "config") p else NULL }      # a table kept in <project>/config: its matches are remembered for the project
        list(get = function() { p <- cfg(); if (!is.null(p)) imp_memory_get(dirname(dirname(p)), basename(p)) },
             put = function(h, m) { p <- cfg(); if (!is.null(p)) imp_memory_put(dirname(dirname(p)), basename(p), h, m) }) })
  observeEvent(input$cf_mine, { removeModal(); if (!ro() && !is.null(isolate(data()))) write_now(isolate(data()), FALSE) })
  observeEvent(input$cf_theirs, { removeModal(); dirty(FALSE); load_disk(restore = FALSE) })
  output$state <- renderText(if (is.null(data())) "" else if (dirty()) "Unsaved changes" else paste(num(nrow(data())), "rows"))
  # A change saved at once (mutate, replace, append: a Workbench action) can be taken back: the version before goes to
  # session$userData$ovid_undo, which the Workbench's Undo bar uses (wb_undo_offer, workbench.R; plot standards A11).
  keep_undo <- function(before) { p <- isolate(path()); session$userData$ovid_undo <- list(at = Sys.time(), file = basename(p), fn = function() {
    if (ro() || !identical(isolate(path()), p)) return(FALSE); data(before); dirty(TRUE); save(quiet = TRUE); bump(); TRUE }) }
  list(data = data, save = save, dirty = dirty, path = path, fresh = fresh, selected = reactive(input$tbl_rows_selected),
       discard = function() if (isolate(dirty())) { dirty(FALSE); load_disk(restore = FALSE) },        # unsaved edits dropped: the saved table again
       select = function(rows) { want(rows); bump() },
       mutate = function(fn) {                              # fn(saved table) -> new table, saved at once
         if (ro() || !fresh()) return(FALSE)
         d <- isolate(data()); if (is.null(d)) return(FALSE)
         data(as.data.frame(fn(d), stringsAsFactors = FALSE)); dirty(TRUE); save(quiet = TRUE); bump(); keep_undo(d); TRUE
       },
       replace = function(d) {                              # a whole new table worked out from data(): refused when the
         if (ro()) return(FALSE)                            # saved version changed since data() was read
         if (!identical(file_stamp(isolate(path())), isolate(loaded()))) {
           if (isolate(dirty())) conflict() else { load_disk(); showNotification(paste(basename(isolate(path())), "was saved elsewhere and has been read again: make the change once more."), type = "warning", duration = 8) }
           return(FALSE) }
         before <- isolate(data()); data(as.data.frame(d, stringsAsFactors = FALSE)); dirty(TRUE); save(quiet = TRUE); bump(); keep_undo(before); TRUE
       },
       set_selected = function(col, value) {
         d <- isolate(data()); s <- isolate(input$tbl_rows_selected)
         if (is.null(d) || !length(s) || ro()) return(FALSE)
         d[s, col] <- value; data(d); dirty(TRUE); bump(); TRUE
       },
       append = function(rows) {                            # rows added to the saved version, saved at once
         if (ro() || !fresh()) return(FALSE)
         d <- isolate(data()); if (is.null(d)) return(FALSE)
         rows <- as.data.frame(rows, stringsAsFactors = FALSE)
         for (v in setdiff(names(d), names(rows))) rows[[v]] <- ""
         data(rbind(d, rows[names(d)])); dirty(TRUE); save(quiet = TRUE); bump(); keep_undo(d); TRUE
       })
})
