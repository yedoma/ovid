# Ovid/sheet.R  --  The Workbench's spreadsheet: every set-up table edited as a sheet, with a value bar.
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): editor.R sources it, no pipeline does.
#
# In the Workbench the set-up table editor (core/editor.R) shows its table as a sheet instead of the Standard view's table:
#   - a value bar: the name box (cell, table and column), the cell's value, Find, and Go to date;
#   - a cell is edited in place (double-click, Enter or just type), arrows and Tab move, Shift with an arrow or a click
#     makes a range, and the status bar shows its count, sum, mean, min and max;
#   - copy (Cmd/Ctrl+C) gives tab-separated rows, paste fills from the selected cell, Delete empties the range;
#   - a click on a heading sorts the view (the file keeps its order), Find shows only rows that contain the text;
#   - Undo and Redo (Cmd/Ctrl+Z, Shift+Cmd/Ctrl+Z) step through the changes, Apply (Cmd/Ctrl+S) saves them; changed
#     cells are marked until then. The editor's save keeps the table before it in config/history as always.
# Cells hold values only: formulas were taken out (stored by row and column, they landed in the wrong cells after
# Delete, Duplicate or an import). A value typed with a leading = is kept as text.
#
#   ui:      ovid_sheet_ui(id)                     (editor.R puts it beside its table)
#   server:  ovid_sheet_send(session, id, d, locked)   sends the table; input[[<id>_apply]] brings it back

# The table to the sheet: its columns (name, editable, numbers or text, dates) and rows as text.
# The set choices of a table's columns, from the app's table rules (CONFIG_FILES rules$choices, as table_check() uses them):
# those columns get a dropdown in the sheet, and a value that is not one of them stops Apply until it is fixed.
sheet_choices <- function(file, nm) {
  e <- Filter(function(x) identical(x$file, file), get0("CONFIG_FILES", ifnotfound = list())); ch <- if (length(e)) e[[1]]$rules$choices
  if (!length(ch)) return(list()); ch <- ch[intersect(names(ch), nm)]
  Filter(length, lapply(ch, function(v) { v <- as.character(v); unique(v[!is.na(v) & nzchar(v)]) }))
}
ovid_sheet_send <- function(session, id, d, locked = character(), ro = FALSE, label = "", base = NULL) {
  if (is.null(d)) { session$sendCustomMessage("dsheet", list(id = id, none = TRUE)); return(invisible()) }
  d <- as.data.frame(d, stringsAsFactors = FALSE); nm <- names(d)
  rows <- lapply(seq_len(nrow(d)), function(i) vapply(d[i, , drop = FALSE], function(v) { v <- as.character(v); if (is.na(v)) "" else v }, "", USE.NAMES = FALSE))
  numcol <- vapply(d, function(v) { v <- trim(as.character(v)); v <- v[!is.na(v) & nzchar(v)]; length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v)))) }, NA)
  datecol <- grepl("(^|_)(date|from|to|start|end|visit|time|timestamp|deployed|retrieved)($|_)", tolower(nm))
  chs <- sheet_choices(label, nm)
  cols <- lapply(seq_along(nm), function(j) c(list(l = nm[j], ed = !ro && !nm[j] %in% locked, t = if (numcol[j]) "num" else if (datecol[j]) "date" else "text"),
                                              if (length(chs[[nm[j]]])) list(ch = as.list(chs[[nm[j]]]))))
  b <- if (!is.null(base)) { base <- as.data.frame(base, stringsAsFactors = FALSE); if (identical(names(base), nm)) lapply(seq_len(nrow(base)), function(i) vapply(base[i, , drop = FALSE], function(v) { v <- as.character(v); if (is.na(v)) "" else v }, "", USE.NAMES = FALSE)) }
  session$sendCustomMessage("dsheet", list(id = id, cols = cols, rows = rows, ro = ro, label = label, base = b))
  invisible()
}

ovid_sheet_ui <- function(id, label = "") div(class = "dsheet", id = id, `data-label` = label,
  div(class = "ds-fx",
    span(class = "ds-nm", "\u2014"),
    tags$input(class = "ds-in", placeholder = "select a cell; type its value", spellcheck = "false", autocomplete = "off"),
    tags$input(class = "ds-find", placeholder = "Find\u2026", spellcheck = "false", autocomplete = "off"),
    tags$input(class = "ds-repl", placeholder = "Replace with\u2026", spellcheck = "false", autocomplete = "off"),
    tags$button(type = "button", class = "ds-b ds-replb", title = "Replace the found text in every editable cell shown (Undo takes it back)", "Replace all"),
    tags$input(class = "ds-goto", type = "date", title = "Go to date: the first row on or after it")),
  div(class = "ds-grid", tabindex = "0"),
  div(class = "ds-sb",
    span(class = "ds-stats"), span(class = "ds-fill"),
    tags$details(class = "ds-cols", tags$summary(class = "ds-b", title = "Choose the columns to show (the table keeps them all)", "Columns"), div(class = "ds-cols-pop")),
    tags$button(type = "button", class = "ds-b", `data-ds` = "fill", title = "Fill down: the first selected cell's value into the cells below it (Cmd/Ctrl+D)", "Fill down"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "rh", title = "Row height", "Rows: normal"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "wrap", title = "Wrap long text in the cells (the row grows to fit)", "Wrap"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "export", title = "Save the table as it shows (filters, sort, columns) as CSV", "Export"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "addrow", title = "Add a row at the end", "+ Row"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "delrows", title = "Delete the rows of the selected cells", "Delete rows"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "undo", title = "Cmd/Ctrl+Z", "Undo"),
    tags$button(type = "button", class = "ds-b", `data-ds` = "redo", title = "Shift+Cmd/Ctrl+Z", "Redo"),
    tags$button(type = "button", class = "ds-b ds-ap", `data-ds` = "apply", title = "Save the changes (Cmd/Ctrl+S); the table before them is kept in history", "Apply")))

SHEET_CSS <- paste(readLines(file.path(CORE_DIR, "www", "sheet.css"), encoding = "UTF-8"), collapse = "\n")

SHEET_JS <- paste(readLines(file.path(CORE_DIR, "www", "sheet.js"), encoding = "UTF-8"), collapse = "\n")
