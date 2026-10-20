# Ovid/metasheet_ui.R  --  The metadata sheet's screen: the table with the project's own headings, and the ways to fill it
# without a metadata file: rows for what was found, rows pasted from a spreadsheet, a spreadsheet imported with its columns
# matched to the sheet's, and the columns themselves (relabel, hide, add your own).
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R); the sheet itself is read by metasheet.R.
#
#   ui:      meta_sheet_ui(id)
#   server:  M <- meta_sheet_server(id, project_dir = reactive(<folder>), app_fields = META_FIELDS, found = reactive(<rows>),
#                                   tick = reactive(<n>), on_change = <after a save>, dt = <the app's table renderer>, what = "logger")
#   M$E the sheet's editor (core/editor.R: data, append, mutate, replace ...); M$spec() the columns as the project has them.
#   found(): rows for what the last run read, in the sheet's fields (the key and whatever the app can tell: year, model ...).

meta_ensure_file <- function(project_dir, spec, who = "app") {
  if (length(project_dir) != 1 || is.na(project_dir) || !nzchar(trim(project_dir))) return(invisible(FALSE))   # no project folder yet: nothing to write
  p <- cfg_path_meta(project_dir, "sheet"); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(p)) || file.access(dirname(p), 2) != 0) return(invisible(FALSE))
  if (!file.exists(p)) { fwrite(empty_cfg(spec$field), p); return(invisible(TRUE)) }
  # safe: returns FALSE, which the caller shows as not saved
  d <- tryCatch(read_cfg(p), error = function(e) NULL); if (is.null(d)) return(invisible(FALSE))
  nm <- names(d); if (basename(p) == DEPLOY_SHEET) setnames(d, dep_names(nm, spec$field))          # headings as the sheet reads them, so none is doubled
  miss <- setdiff(spec$field, names(d)); if (length(miss) || !identical(nm, names(d))) { backup_cfg(p, ensure_cols(d, spec$field), who); fwrite_safe(ensure_cols(d, spec$field), p) }
  invisible(TRUE)
}

meta_sheet_ui <- function(id, intro = NULL) { ns <- NS(id)
  tagList(
    if (!is.null(intro)) p(class = "dim", intro),
    div(class = "btnrow editor-only",
      actionButton(ns("found"), "Add what was found", icon = icon("wand-magic-sparkles"), title = "A row for every one the last run read that the sheet does not list yet"),
      actionButton(ns("paste"), "Paste rows\u2026", icon = icon("paste"), title = "Rows copied from Excel or typed, one per line"),
      actionButton(ns("import"), "Import a spreadsheet\u2026", icon = icon("file-import"), title = "A CSV or Excel file you already have; its columns are matched to the sheet's"),
      actionButton(ns("cols"), "Columns\u2026", icon = icon("table-columns"), title = "Rename, hide or add columns"),
      downloadButton(ns("dl"), "Download"), span(class = "dim push", textOutput(ns("check"), inline = TRUE))),
    ovid_edit_ui(ns("ed"), hint = "Double-click a cell to edit. Every column is optional.", import = FALSE))
}

meta_sheet_server <- function(id, project_dir, app_fields, found = reactive(NULL), tick = reactive(0), on_change = function() NULL, dt = NULL, what = "row")
  moduleServer(id, function(input, output, session) {
  ns <- session$ns; ro <- function() isTRUE(get0("VIEWER", inherits = TRUE))
  spec_tick <- reactiveVal(0)
  spec <- reactive({ tick(); spec_tick(); live_stamp(cfg_path_meta(project_dir(), "fields")); s <- meta_spec(app_fields, project_dir()); if (!ro()) { meta_ensure_file(project_dir(), s); if (!file.exists(cfg_path_meta(project_dir(), "fields"))) meta_write_spec(s, project_dir()) }; s })
  heading <- function(s, cols) { i <- match(cols, s$field); lab <- ifelse(is.na(i), cols, s$label[i]); u <- ifelse(is.na(i), "", s$unit[i])
    ifelse(nzchar(u), paste0(lab, " (", u, ")"), lab) }
  show <- function(d, ..., page = 25, opts = list()) { s <- isolate(spec()); hid <- which(names(d) %in% s[!meta_true(show), field]) - 1L
    o <- utils::modifyList(opts, list(columnDefs = list(list(visible = FALSE, targets = if (length(hid)) hid else -999L))))
    if (is.function(dt)) dt(d, ..., colnames = heading(s, names(d)), page = page, opts = o)
    else DT::datatable(d, rownames = FALSE, ..., colnames = heading(s, names(d)), options = utils::modifyList(list(pageLength = page, scrollX = TRUE), o)) }
  E <- ovid_edit_server("ed", reactive({ spec(); cfg_path_meta(project_dir(), "sheet") }), reactive(c(tick(), spec_tick())), on_change = on_change, dt = show)
  keys <- reactive(meta_keys(spec()))

  output$check <- renderText({ d <- E$data(); s <- spec(); if (is.null(d) || !nrow(d)) return("")
    k <- keys()[1]; nk <- if (!is.na(k) && k %in% names(d)) sum(!nzchar(trim(d[[k]]))) else 0
    p <- meta_values(d, s)$problems
    paste(c(if (nrow(p)) paste0(nrow(p), if (nrow(p) == 1) " value" else " values", " not understood (row ", p$row[1], ", ", s[field == p$field[1], label], ": ", p$problem[1], ")"),
            if (nk) paste0(nk, if (nk == 1) " row has" else " rows have", " no ", s[field == k, label], " and is left aside")), collapse = "; ") })
  output$dl <- downloadHandler(function() paste0("metadata_", format(Sys.Date()), ".csv"), function(file) { d <- E$data(); fwrite(if (is.null(d)) data.table() else d, file) })

  # rows for what was found and is not listed yet (by every key field the found rows have)
  observeEvent(input$found, { f <- tryCatch(found(), error = function(e) NULL); s <- spec()
    if (is.null(f) || !nrow(f)) return(showNotification("Nothing found yet: run the pipeline first.", type = "warning"))
    k <- intersect(keys(), names(f)); d <- as.data.table(E$data() %||% data.frame())
    id <- function(x) do.call(paste, c(lapply(k, function(v) meta_norm(if (v %in% names(x)) x[[v]] else "")), sep = "\r"))
    new <- unique(f[!id(f) %in% if (nrow(d)) id(d) else character()], by = k)
    if (!nrow(new)) return(showNotification(paste0("Every ", what, " found is in the sheet already."), type = "message"))
    new <- ensure_cols(new[, lapply(.SD, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })], s$field)[, s$field, with = FALSE]
    if (E$append(as.data.frame(new))) showNotification(sprintf("%d %s%s added. Fill in what you know; every column is optional.", nrow(new), what, if (nrow(new) == 1) "" else "s"), type = "message") })

  # paste or import: a table, its columns matched to the sheet's, then added, updated or used instead
  table_import_server(input, output, session, spec, target = "the sheet", modes = c("add", "update", "replace"), new_cols = TRUE,
    existing = function() as.data.table(E$data() %||% data.frame()),
    memory = list(get = function() imp_memory_get(project_dir(), "metadata"), put = function(h, m) imp_memory_put(project_dir(), "metadata", h, m)),
    put = function(new, mode, s2) { s <- spec(); k <- keys()[1]
      if (!is.na(k) && !k %in% names(new)[vapply(new, function(v) any(nzchar(v)), NA)]) showNotification(paste0("No column was matched to ", s[field == k, label], ": the rows are added, but left aside until it is filled in."), type = "warning", duration = 8)
      if (nrow(s2) > nrow(s)) { meta_write_spec(s2, project_dir()); meta_ensure_file(project_dir(), s2); spec_tick(spec_tick() + 1) }
      if (!E$fresh()) return(FALSE)                            # an unsaved edit here, or a newer save elsewhere: the editor asks first
      E$replace(meta_merge(as.data.table(E$data() %||% data.frame()), new, s2, mode)) })

  # the columns: relabel, hide, set the choices of a list, add one of your own
  FE <- ovid_edit_server("fields", reactive({ spec(); cfg_path_meta(project_dir(), "fields") }), reactive(c(tick(), spec_tick())), locked = c("field", "use", "identifies"),
                           on_change = function() { spec_tick(isolate(spec_tick()) + 1) }, dt = dt)
  observeEvent(input$cols, {
    showModal(modalDialog(title = "Columns", size = "l", easyClose = TRUE,
      p(class = "dim", "Each row is a column of the sheet. Change its label, help or unit, set show to FALSE to hide it (its values are kept), or give a list's choices separated by |. Columns with a use are read by the app; their type stays as the app reads it. Columns you add are kept and exported, and not used."),
      div(class = "meta-addcol", style = "display:grid;grid-template-columns:2fr 1fr 2fr auto;gap:8px;align-items:end",
        textInput(ns("nc_label"), "New column", placeholder = "e.g. Owner"), selectInput(ns("nc_type"), "Type", META_TYPES), textInput(ns("nc_choices"), "Choices (a list: a|b|c)"),
        actionButton(ns("nc_add"), "Add column")),
      ovid_edit_ui(ns("fields"), hint = "Double-click a cell to edit.", import = FALSE),
      footer = modalButton("Done"))) })
  observeEvent(input$nc_add, { lab <- trim(input$nc_label %||% ""); if (!nzchar(lab)) return(ovid_field_error("nc_label", "Give the column a label."))
    s <- spec(); f <- sub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(lab))); if (!nzchar(f)) f <- "column"; while (f %in% s$field) f <- paste0(f, "_2")
    s2 <- rbind(s, meta_defaults(data.table(field = f, label = lab, type = input$nc_type %||% "text", choices = trim(input$nc_choices %||% ""), use = "")))
    meta_write_spec(s2, project_dir()); meta_ensure_file(project_dir(), s2); spec_tick(spec_tick() + 1); on_change(); live_touch()
    updateTextInput(session, "nc_label", value = ""); updateTextInput(session, "nc_choices", value = "")
    showNotification(paste0("Column '", lab, "' added."), type = "message") })

  list(E = E, spec = spec)
})

# ---- paste and import, for the sheet and for every set-up table (core/editor.R) ----------------------------------------
# Inside a module: buttons with the ids "paste" and "import" open a dialog; the rows (pasted, or read from a CSV, text or Excel file)
# have their columns matched to spec()'s fields by heading (core/import.R: the field, its label, an alias, a unit in brackets left
# out, a spelling slip), the person can change any match, and what the rows will do is checked and said before anything is written:
# errors stop it (two columns on one field, dates that could be day/month or month/day, no key column when updating), warnings say
# what will happen to rows and values that do not fit. put(rows, mode, spec) writes them.
#   new_cols  a column the table lacks can be kept as a new column (the sheet); else it is left out
#   existing  function() the table's current rows (for keys that are already there), or NULL
#   memory    list(get = function() data.table(heading, field), put = function(headings, map)): matches kept for next time
table_import_server <- function(input, output, session, spec, put, modes = c("add", "replace"), new_cols = FALSE, target = "the table", existing = NULL, memory = NULL) {
  ns <- session$ns; incoming <- reactiveVal(NULL); src <- reactiveVal(NULL); guess <- reactiveVal(NULL); trouble <- reactiveVal(NULL)
  options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 5 * 1024^2), 100 * 1024^2))     # the default 5 MB stops a larger file before it is read
  MODE_LAB <- c(add = "Add the rows", update = "Update rows with the same key, add the rest", replace = paste("Use them instead of", target))
  mode_ui <- function() radioButtons(ns("imp_mode"), NULL, stats::setNames(modes, MODE_LAB[modes]), if ("update" %in% modes) "update" else "add", inline = TRUE)
  go <- function() tagList(modalButton("Cancel"), actionButton(ns("imp_go"), paste("Put them in", target), class = "btn-primary"))
  # safe: the remembered column matches are a convenience
  mem <- function() if (is.null(memory)) NULL else tryCatch(memory$get(), error = function(e) NULL)
  fail <- function(e) { trouble(conditionMessage(e)); incoming(NULL); src(NULL); guess(NULL) }
  # a table read: its matches are worked out once, from the headings and what was matched last time
  take <- function(x, g = NULL) { trouble(NULL); incoming(x); guess(g %||% imp_guess(names(x), spec(), memory = mem())) }
  # rows pasted: headings if the first line has any that match; else every line is data, columns taken in the table's order
  read_paste <- function(txt, hdr = NULL) { s <- spec(); x <- tryCatch(meta_import_text(txt, s, hdr), error = function(e) e)
    if (!inherits(x, "error") && ncol(x) && (!is.null(hdr) || any(imp_guess(names(x), s, stages = c("exact", "units"))$field != ""))) return(take(x))
    # safe: fail() on the same line says nothing could be read
    g <- tryCatch(imp_grid_text(txt), error = function(e) NULL); if (is.null(g)) return(fail(if (inherits(x, "error")) x else simpleError("Nothing could be read from the pasted text.")))
    for (v in names(g)) set(g, j = v, value = imp_text(g[[v]])); g <- g[rowSums(as.matrix(g) != "") > 0]; g <- g[, vapply(g, function(z) any(nzchar(z)), NA), with = FALSE]
    if (!nrow(g) || !ncol(g)) return(fail(simpleError("Nothing could be read from the pasted text.")))
    s <- s[meta_true(show)]; setnames(g, paste0("column ", seq_len(ncol(g))))
    attr(g, "notes") <- "The first line is not a row of headings (none matches a column), so every line is a row and the columns are taken in the table's order. Change any match below."
    take(g, data.table(field = c(s$field, rep("", max(0, ncol(g) - nrow(s))))[seq_len(ncol(g))], how = "order")) }
  read_file <- function(f, sheet = NULL, hdr = NULL) { x <- tryCatch(meta_import_read(f$datapath, sheet, f$name, spec(), hdr), error = function(e) e); if (inherits(x, "error")) fail(x) else take(x) }
  boxes <- function() tagList(uiOutput(ns("imp_trouble")), uiOutput(ns("imp_notes")), uiOutput(ns("imp_hdr_ui")), uiOutput(ns("imp_map")), uiOutput(ns("imp_dates_ui")), uiOutput(ns("imp_check")), mode_ui())
  observeEvent(input$paste, { incoming(NULL); src(NULL); trouble(NULL); guess(NULL)
    showModal(modalDialog(title = "Paste rows", size = "l", easyClose = TRUE,
      p(class = "dim", paste0("Copy cells from Excel (with or without the heading row) and paste them here, or type one row per line with commas between values. A first line of headings is matched to ", target, "'s columns.")),
      textAreaInput(ns("paste_txt"), NULL, rows = 8, width = "100%", placeholder = paste(utils::head(isolate(spec())$label, 4), collapse = ", ")), boxes(), footer = go())) })
  observeEvent(input$paste_txt, { txt <- input$paste_txt %||% ""; if (!nzchar(trimws(txt))) { incoming(NULL); trouble(NULL); return() }; src(list(kind = "text", text = txt)); read_paste(txt) }, ignoreInit = TRUE)
  observeEvent(input$import, { incoming(NULL); src(NULL); trouble(NULL); guess(NULL)
    showModal(modalDialog(title = "Import a spreadsheet", size = "l", easyClose = TRUE,
      p(class = "dim", paste0("A CSV, text or Excel file you already have, laid out however it is. Its columns are matched to ", target, "'s by their headings", if (new_cols) "; a column it does not have is kept as a column of its own" else "",
                              ". Check the matches below and change any, or leave a column out. The file itself is only read.")),
      fileInput(ns("imp_file"), NULL, accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls"), width = "100%", buttonLabel = "Choose file…", placeholder = "CSV, text or Excel"),
      uiOutput(ns("imp_sheet")), boxes(), footer = go())) })
  sheets <- reactive({ f <- input$imp_file; req(f); x <- incoming(); attr(x, "sheets") })
  observeEvent(input$imp_file, { f <- input$imp_file; req(f); src(list(kind = "file", file = f)); read_file(f) })
  output$imp_sheet <- renderUI({ s <- sheets(); if (!is.null(s) && nrow(s) > 1) selectInput(ns("imp_sh"), "Sheet", stats::setNames(s$sheet, paste0(s$sheet, " (", s$rows, " rows)")), isolate(attr(incoming(), "sheet")) %||% s$sheet[1]) })
  observeEvent(input$imp_sh, { z <- src(); if (!is.null(z) && z$kind == "file" && !identical(input$imp_sh, attr(isolate(incoming()), "sheet"))) read_file(z$file, input$imp_sh) }, ignoreInit = TRUE)
  observeEvent(input$imp_hdr, { z <- src(); h <- suppressWarnings(as.integer(input$imp_hdr)); x <- isolate(incoming()); if (is.null(z) || is.na(h) || h < 1 || identical(h, attr(x, "header_row"))) return()
    if (z$kind == "file") read_file(z$file, input$imp_sh %||% NULL, h) else read_paste(z$text, h) }, ignoreInit = TRUE)
  output$imp_trouble <- renderUI({ m <- trouble(); if (is.null(m)) NULL else div(class = "imp-box imp-err", tags$b("This could not be read. "), m) })
  output$imp_notes <- renderUI({ x <- incoming(); n <- attr(x, "notes"); if (is.null(x) || !length(n)) return(NULL); div(class = "imp-box imp-note", lapply(n, function(m) div(m))) })
  output$imp_hdr_ui <- renderUI({ x <- incoming(); if (is.null(x)) return(NULL)
    tags$details(class = "imp-hdr", tags$summary("The headings are not on the right row?"), div(class = "imp-hdr-body",
      numericInput(ns("imp_hdr"), "Headings are on row", value = attr(x, "header_row") %||% 1, min = 1, step = 1, width = "140px"),
      span(class = "dim", "Counting every line of the file, blank ones included. The rows below it are the data."))) })
  # one line per column of the file: its heading and first values, and which field it is
  output$imp_map <- renderUI({ x <- incoming(); if (is.null(x) || !ncol(x)) return(NULL); s <- spec(); g <- guess() %||% imp_guess(names(x), s)
    ch <- c(`Leave out` = "", stats::setNames(s$field, paste0(s$label, ifelse(meta_true(s$identifies), " (key)", ""))))
    flag <- function(h) switch(h, memory = span(class = "imp-tag ok", "as last time"), fuzzy = span(class = "imp-tag chk", "check: close, not exact"), words = span(class = "imp-tag chk", "check"), order = span(class = "imp-tag chk", "by position"), NULL)
    tagList(h4(sprintf("%s, %s", imp_plural(nrow(x), "row"), imp_plural(ncol(x), "column"))),
      div(class = "meta-map", lapply(seq_len(ncol(x)), function(i) { ex <- utils::head(unique(x[[i]][nzchar(x[[i]])]), 3); f <- g$field[i]
        div(class = "meta-map-row", div(tags$b(names(x)[i]), flag(g$how[i]), div(class = "dim", if (length(ex)) paste(ex, collapse = " · ") else "(empty)")),
            selectInput(ns(paste0("map_", i)), NULL, c(ch, if (new_cols) stats::setNames(paste0("+", names(x)[i]), "Keep as a new column")), selected = if (nzchar(f)) f else if (new_cols && !startsWith(names(x)[i], "column ")) paste0("+", names(x)[i]) else "", width = "100%")) })))})
  current_map <- function(x) { g <- isolate(guess()) %||% imp_guess(names(x), spec()); vapply(seq_len(ncol(x)), function(i) { m <- input[[paste0("map_", i)]]; if (is.null(m)) { f <- g$field[i]; if (nzchar(f)) f else "" } else m }, "") }
  has_dates <- reactive({ x <- incoming(); req(x); s <- spec(); m <- current_map(x); any(s$type[match(m[nzchar(m) & !startsWith(m, "+")], s$field)] %in% c("date", "datetime")) })
  output$imp_dates_ui <- renderUI({ x <- incoming(); if (is.null(x) || !isTRUE(has_dates())) return(NULL)
    div(class = "imp-hdr-body", selectInput(ns("imp_dates"), "Dates written like 03/04/2024 are", c("Work it out from the file" = "auto", "Day / month / year" = "dmy", "Month / day / year" = "mdy"), isolate(input$imp_dates) %||% "auto", width = "260px"),
        span(class = "dim", "Only matters when a date's order cannot be seen in the file. 2024-08-31 is never ambiguous.")) })
  # safe: the existing rows are for comparing only
  checked <- reactive({ x <- incoming(); req(x); m <- current_map(x); mode <- input$imp_mode %||% modes[1]; ex <- if (is.null(existing)) NULL else tryCatch(existing(), error = function(e) NULL)
    pr <- imp_prepare(x, m, spec(), input$imp_dates %||% "auto"); list(prep = pr, issues = imp_check(pr, m, spec(), ex, mode), map = m) })
  output$imp_check <- renderUI({ x <- incoming(); if (is.null(x)) return(NULL); r <- checked(); iss <- r$issues; e <- iss[severity == "error"]; w <- iss[severity == "warning"]; n <- iss[severity == "note"]
    line <- function(d, cls, mark) lapply(seq_len(nrow(d)), function(i) div(class = paste("imp-line", cls), mark, " ", d$message[i]))
    s <- spec(); m <- r$map; k <- which(nzchar(m) & !startsWith(m, "+") & !duplicated(m)); pv <- r$prep$x[seq_len(min(5L, nrow(x)))]
    tagList(h4(if (nrow(e)) "Fix before importing" else if (nrow(w)) "Check before importing" else "Ready to import"),
      line(e, "e", "✖"), line(w, "w", "▲"), line(n, "n", "·"),
      if (length(k)) tagList(h4("What will be written (first rows)"), div(class = "imp-prev", tags$table(tags$thead(tags$tr(lapply(s$label[match(m[k], s$field)], tags$th))),
        tags$tbody(lapply(seq_len(nrow(pv)), function(i) tags$tr(lapply(k, function(j) tags$td(pv[[j]][i])))))))),
      tags$script(HTML(sprintf("$('#%s').prop('disabled', %s);", ns("imp_go"), if (nrow(e)) "true" else "false")))) })
  observeEvent(input$imp_go, { x <- incoming(); if (is.null(x) || !nrow(x)) return(showNotification("Nothing to add.", type = "warning"))
    r <- checked(); if (nrow(r$issues[severity == "error"])) return(showNotification(r$issues[severity == "error"]$message[1], type = "error", duration = 10))
    mode <- input$imp_mode %||% modes[1]; map <- r$map
    rr <- tryCatch(meta_from_import(r$prep$x, map, spec()), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }); if (is.null(rr)) return()
    if (isTRUE(put(rr$rows, mode, rr$spec))) { removeModal(); incoming(NULL); src(NULL)
      # safe: remembering the column matches is a convenience
      if (!is.null(memory)) tryCatch(memory$put(names(x), map), error = function(e) NULL)
      nw <- nrow(r$issues[severity == "warning"])
      showNotification(paste0(sprintf("%d row(s) %s.", nrow(rr$rows), c(add = "added", update = "added or updated", replace = paste("now make up", target))[[mode]]), if (nw) paste0(" ", nw, " thing", if (nw == 1) "" else "s", " to check were noted: see the Problems list.") else ""), type = if (nw) "warning" else "message", duration = if (nw) 10 else 5) } })
  invisible(list(incoming = incoming, guess = guess, trouble = trouble, checked = checked))     # for tests and for pages that want to look
}

# ---- the deployment sheet against the data (core deployments.R) ------------------------------------------------------------
# Under the sheet's editor: how each row matches what the last run read, and the deployments no row claims, which can be added
# (merged: blanks filled, nothing overwritten or deleted; the summary says what happened).
#   deploy_check_ui(id)   deploy_check_server(id, project_dir = reactive(<folder>), scan = list(file, serial, name, file_col),
#                                             tick = reactive(<n>), on_change = <after the sheet is written>)
deploy_check_ui <- function(id) { ns <- NS(id)
  div(class = "deploy-check", h4("Against the data"), uiOutput(ns("summary")), DT::DTOutput(ns("rows")),
      div(class = "btnrow", actionButton(ns("accept"), "Accept the suggested matches", title = "Matches by name, by year or month, or a serial's only deployment: the record's first and last day become the row's From and To. Select rows to accept only those")),
      h4("Found in the files, in no row"), DT::DTOutput(ns("found")),
      div(class = "btnrow", actionButton(ns("add"), "Add these to the sheet", title = "Each becomes a row with the name saved in its files as the station; check them"))) }
deploy_check_server <- function(id, project_dir, scan, tick = reactive(0), on_change = function() NULL) moduleServer(id, function(input, output, session) {
  ck <- reactive({ tick(); live_config(project_dir()); tryCatch(dep_check(project_dir(), scan), error = function(e) NULL) })   # the sheet saved on any page
  output$summary <- renderUI({ x <- ck(); if (is.null(x)) return(p(class = "dim", "Run the app once: the check needs the list of files it read."))
    if (is.null(x$rows)) return(p(class = "dim", "No rows with a serial yet. ", nrow(x$unclaimed), " deployment(s) found in the files."))
    k <- x$counts; lab <- c(dates = "by dates", year = "by year", month = "by month", name = "by name", only = "the serial's only one", choose = "to choose", `no data` = "no data yet", `no data in period` = "no data in its dates")
    p(paste(paste(as.integer(k), ifelse(is.na(lab[names(k)]), names(k), lab[names(k)])), collapse = ", "), "; ", nrow(x$unclaimed), " found in no row",
      if (nrow(x$unassigned)) paste0("; ", nrow(x$unassigned), " file(s) with no serial") else "") })
  output$rows <- DT::renderDT({ x <- ck(); r <- if (is.null(x) || is.null(x$rows)) data.table() else
    x$rows[, .(serial, station, from, to, match, `first record` = format(first_record, "%Y-%m-%d"), `last record` = format(last_record, "%Y-%m-%d"), files, `name in files` = names_in_files)]
    DT::datatable(r, rownames = FALSE, selection = "multiple", options = list(pageLength = 10, scrollX = TRUE, dom = "tip")) })
  observeEvent(input$accept, { x <- isolate(ck()); if (isTRUE(get0("VIEWER", inherits = TRUE))) return()
    r <- if (is.null(x) || is.null(x$rows)) data.table() else x$rows; sel <- input$rows_rows_selected
    if (length(sel)) r <- r[sel]; r <- r[match %in% DEP_SUGGESTED]
    if (!nrow(r)) return(showNotification(if (length(sel)) "None of the rows selected is a suggestion to accept." else "No suggested matches to accept.", type = "message"))
    d <- tryCatch(dep_accept_dates(project_dir(), r), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }); if (is.null(d)) return()
    live_touch(); showNotification(paste0(nrow(r), " match", if (nrow(r) == 1) "" else "es", " accepted: From and To are the first and last day of the record."), type = "message"); on_change() })
  output$found <- DT::renderDT({ x <- ck(); u <- if (is.null(x)) data.table() else x$unclaimed[, .(serial, `name in files` = names_in_files, `first record` = format(first_record, "%Y-%m-%d"),
                                                                                                    `last record` = format(last_record, "%Y-%m-%d"), files)]
    DT::datatable(u, rownames = FALSE, selection = "none", options = list(pageLength = 10, scrollX = TRUE, dom = "tip")) })
  observeEvent(input$add, { x <- isolate(ck()); if (is.null(x) || !nrow(x$unclaimed)) return(showNotification("Nothing to add.", type = "message"))
    if (isTRUE(get0("VIEWER", inherits = TRUE))) return()
    p <- dep_sheet_path(project_dir()); old <- if (file.exists(p)) read_cfg(p) else empty_cfg(DEPLOY_COLS)
    mg <- dep_merge(old, dep_rows_found(x$unclaimed)); backup_cfg(p, mg$rows, "app", "rows found in the files"); fwrite_safe(mg$rows, p); live_touch()
    showNotification(paste0("Deployment sheet: ", dep_merge_summary(mg$log), ". Check the stations named from the files."), type = "message", duration = 8); on_change() })
  invisible(list(check = ck))
})
