# Ovid/rename.R  --  Renaming a site or station once, in every set-up table that names it. Part of Ovid Core (shared).
# Sourced by the apps (screens and the tables they edit; no run reads it).
#
# Each app declares RENAMES (data-specific, so in the app): for each kind of name (site, station ...) its label, the table and
# column that lists them (`main`), and every table and column that refers to one: a cell that holds the name (`cols`) or a
# list of names separated by | (`lists`, as applies_to: a rule for "Plot 1|Plot 2"; "site:Plot 1" counts too). Names match
# ignoring case and spaces at the ends. The preview lists every cell that would change; applying keeps the tables before in
# config/history (as every edit) and can be undone. The outputs keep the old name until the next run. The core tables that
# every app has (RENAME_COMMON) and the review decisions are added here, so an app lists only its own.

rename_kinds <- function(spec = get0("RENAMES", ifnotfound = list())) vapply(spec, `[[`, "", "label")

# The core tables that name a site or station in every app, added to the app's first kind (its sites or stations): the
# corrections over a period (target), the calibration options (applies_to) and what shows (name). The review decisions
# (config/review_decisions.csv) follow too: their ids are made from the name (rename_review_plan()).
RENAME_COMMON <- list(cols = list(c("calibration_options.csv", "applies_to"), c("display.csv", "name")),
                      lists = list(c("period_corrections.csv", "target")))            # a target may name several, separated by |
rename_spec <- function(spec, kind) { k <- spec[[kind]]; if (is.null(k) || !identical(kind, names(spec)[1])) return(k)
  have <- vapply(c(list(k$main), k$cols, k$lists), paste, "", collapse = "|")
  k$cols <- c(k$cols, Filter(function(x) !paste(x, collapse = "|") %in% have, RENAME_COMMON$cols))
  k$lists <- c(k$lists, Filter(function(x) !paste(x, collapse = "|") %in% have, RENAME_COMMON$lists)); k }

# The review decisions on the last run's flags at the old name, under the ids they get with the new one. The flags are in
# output/audit/flags.csv with a column named as the kind (station, site) and the value's column (parameter or channel).
rename_review_plan <- function(project_dir, kind, old, new) {
  none <- data.table(file = character(), column = character(), row = integer(), before = character(), after = character())
  rp <- review_path(project_dir); f <- file.path(path.expand(project_dir), "output", "audit", "flags.csv")
  if (!file.exists(rp) || !file.exists(f)) return(none)
  # safe: a review file that cannot be read has nothing to rename
  d <- review_read(rp); fl <- tryCatch(fread(file = f, colClasses = "character", na.strings = NULL), error = function(e) NULL)
  pc <- intersect(c("parameter", "channel"), names(fl))[1]
  if (!nrow(d) || is.null(fl) || !kind %in% names(fl) || is.na(pc)) return(none)
  fl <- fl[tolower(trim(fl[[kind]])) == tolower(old)]; if (!nrow(fl)) return(none)
  k <- review_match(d, fl[[kind]], fl[[pc]], fl$start, fl$end, fl$check); ok <- !is.na(k); if (!any(ok)) return(none)
  x <- unique(data.table(row = k[ok], after = wb_queue_id(rep(new, sum(ok)), fl[[pc]][ok], fl$start[ok], fl$end[ok], fl$check[ok])), by = "row")
  data.table(file = REVIEW_FILE, column = "id", row = x$row, before = d$id[x$row], after = x$after)
}

# Every name of this kind the set-up tables hold (the main table's first).
rename_names <- function(project_dir, kind, spec = get0("RENAMES", ifnotfound = list())) {
  k <- spec[[kind]]; if (is.null(k)) return(character())
  rd <- function(f, col) { p <- file.path(path.expand(project_dir), "config", f); if (!file.exists(p)) return(character())
    # safe: names offered for renaming; a table that cannot be read offers none
    d <- tryCatch(fread(file = p, colClasses = "character", na.strings = NULL, select = col), error = function(e) NULL); if (is.null(d) || !ncol(d)) character() else trim(d[[1]]) }
  v <- c(rd(k$main[1], k$main[2]), unlist(lapply(k$cols, function(x) rd(x[1], x[2]))))
  v <- v[nzchar(v)]; v[!duplicated(tolower(v))]
}

# The cells that would change: file, column, row (in the table, from 1), before, after.
rename_plan <- function(project_dir, kind, old, new, spec = get0("RENAMES", ifnotfound = list())) {
  k <- rename_spec(spec, kind); old <- trim(old); new <- trim(new); out <- list()
  none <- data.table(file = character(), column = character(), row = integer(), before = character(), after = character())
  if (is.null(k) || !nzchar(old) || !nzchar(new) || identical(old, new)) return(none)
  same <- function(x) tolower(trim(x)) == tolower(old)
  for (x in c(list(k$main), k$cols)) { p <- file.path(path.expand(project_dir), "config", x[1]); if (!file.exists(p)) next
    # safe: a table that cannot be read is left out of the plan, and the rename does not touch it
    d <- tryCatch(read_cfg(p), error = function(e) NULL); if (is.null(d) || !x[2] %in% names(d)) next
    i <- which(same(d[[x[2]]])); if (length(i)) out[[length(out) + 1L]] <- data.table(file = x[1], column = x[2], row = i, before = d[[x[2]]][i], after = new) }
  for (x in k$lists) { p <- file.path(path.expand(project_dir), "config", x[1]); if (!file.exists(p)) next
    # safe: as above
    d <- tryCatch(read_cfg(p), error = function(e) NULL); if (is.null(d) || !x[2] %in% names(d)) next
    v <- as.character(d[[x[2]]]); v[is.na(v)] <- ""
    nv <- vapply(v, function(cell) { if (!nzchar(cell)) return(cell); parts <- strsplit(cell, "|", fixed = TRUE)[[1]]
      pre <- sub("^(\\s*[a-z_]+:)?.*$", "\\1", parts); body <- substring(parts, nchar(pre) + 1L)
      hit <- same(body); if (!any(hit)) return(cell); body[hit] <- new; paste0(pre, body, collapse = "|") }, "", USE.NAMES = FALSE)
    i <- which(nv != v); if (length(i)) out[[length(out) + 1L]] <- data.table(file = x[1], column = x[2], row = i, before = v[i], after = nv[i]) }
  # safe: the review files' part of the plan is extra
  if (exists("review_path", mode = "function")) out[[length(out) + 1L]] <- tryCatch(rename_review_plan(project_dir, kind, old, new), error = function(e) NULL)
  if (length(out)) unique(rbindlist(out)) else none
}

# Applies a plan: each table read, changed and written beside and renamed (fwrite_safe), the version before kept in
# config/history. Returns the tables before (for Undo).
rename_apply <- function(project_dir, plan, who = "", what = "rename") {
  before <- list()
  for (f in unique(plan$file)) { p <- file.path(path.expand(project_dir), "config", f); d <- read_cfg(p); before[[f]] <- copy(d)
    for (r in split(plan[file == f], by = "column")) { v <- as.character(d[[r$column[1]]]); v[r$row] <- r$after; set(d, j = r$column[1], value = v) }
    backup_cfg(p, d, who, what); fwrite_safe(d, p) }
  invisible(before)
}
rename_undo <- function(project_dir, before, who = "") {
  for (f in names(before)) { p <- file.path(path.expand(project_dir), "config", f); backup_cfg(p, before[[f]], who, "undo rename"); fwrite_safe(before[[f]], p) }
  invisible(names(before))
}

# ---- the panel ---------------------------------------------------------------------------------------------------------------
rename_ui <- function(id) { ns <- NS(id)
  div(class = "panel-x editor-only", h3("Rename a site or station"),
      p(class = "dim", "Changes the name in every set-up table that holds it: the list of sites or stations, sensors and loggers, quality rules, manual removals,",
        "the deployment sheet and the rest. Check the preview, then apply; Undo puts the tables back. The outputs take the new name at the next run."),
      fluidRow(column(3, selectInput(ns("kind"), "What", choices = NULL, width = "100%")),
               column(4, selectizeInput(ns("old"), "Name now", choices = NULL, width = "100%", options = list(placeholder = "Pick a name"))),
               column(3, textInput(ns("new"), "New name", width = "100%")),
               column(2, div(style = "margin-top:25px", actionButton(ns("preview"), "Preview", icon = icon("magnifying-glass"))))),
      uiOutput(ns("note")), DT::DTOutput(ns("plan")),
      div(class = "btnrow", style = "margin-top:8px", actionButton(ns("apply"), "Rename", class = "btn-primary", icon = icon("i-cursor")),
          actionButton(ns("undo"), "Undo the last rename", icon = icon("rotate-left")))) }

rename_server <- function(id, project_dir, on_change = function() NULL, who = function() "", spec = get0("RENAMES", ifnotfound = list())) moduleServer(id, function(input, output, session) {
  rv <- reactiveValues(plan = NULL, undo = NULL, tick = 0)
  observe({ ch <- rename_kinds(spec); updateSelectInput(session, "kind", choices = setNames(names(ch), ch), selected = { k <- isolate(input$kind) %||% ""; if (k %in% names(ch)) k else names(ch)[1] }) })
  observe({ rv$tick; req(input$kind); pd <- project_dir(); if (exists("live_config", mode = "function")) live_config(pd)   # names added or changed on any page
    # safe: names offered for renaming only
    nm <- tryCatch(rename_names(pd, input$kind, spec), error = function(e) character())
    keep <- isolate(input$old); updateSelectizeInput(session, "old", choices = nm, selected = if (!is.null(keep) && keep %in% nm) keep else character())
    # New name offers names of this kind the deployment sheet holds and the set-up tables do not yet (core shell.R)
    fresh <- if (exists("suggest_names", mode = "function")) suggest_names(pd, spec)[[tolower(spec[[input$kind]]$label)]] else character()
    session$sendCustomMessage("ovid-suggest", list(id = session$ns("new"), set = "names", values = as.list(fresh[!tolower(fresh) %in% tolower(nm)]))) })
  observeEvent(list(input$kind, input$old, input$new), rv$plan <- NULL, ignoreInit = TRUE)
  observeEvent(input$preview, { req(input$kind)
    if (!nzchar(trim(input$old %||% "")) || !nzchar(trim(input$new %||% ""))) return(showNotification("Pick the name now and type the new one.", type = "warning"))
    if (tolower(trim(input$new)) %in% setdiff(tolower(rename_names(project_dir(), input$kind, spec)), tolower(trim(input$old))))
      showNotification(paste0("'", trim(input$new), "' is already a name in this project: renaming joins the two."), type = "warning", duration = 10)
    rv$plan <- rename_plan(project_dir(), input$kind, input$old, input$new, spec) })
  output$note <- renderUI({ x <- rv$plan; if (is.null(x)) return(NULL)
    if (!nrow(x)) return(p(class = "dim", "No set-up table holds that name."))
    p(tags$b(paste0(nrow(x), " cell", if (nrow(x) != 1) "s", " in ", uniqueN(x$file), " table", if (uniqueN(x$file) != 1) "s")), " would change:") })
  output$plan <- DT::renderDT({ x <- rv$plan; req(!is.null(x) && nrow(x))
    DT::datatable(x[, .(Table = file, Column = column, Row = row, Now = before, Becomes = after)], rownames = FALSE, selection = "none",
                  options = list(dom = "tp", pageLength = 10, ordering = FALSE)) })
  observeEvent(input$apply, { x <- rv$plan
    if (is.null(x)) return(showNotification("Preview first, then rename.", type = "warning")); if (!nrow(x)) return(showNotification("Nothing to change.", type = "message"))
    if (run_busy()) return(showNotification("A run is going: rename when it has finished.", type = "warning"))
    r <- tryCatch(rename_apply(project_dir(), x, who(), paste0("rename ", trim(input$old), " to ", trim(input$new))), error = function(e) e)
    if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error", duration = NULL))
    rv$undo <- list(pd = project_dir(), before = r, what = paste0("'", trim(input$old), "' to '", trim(input$new), "'")); rv$plan <- NULL; rv$tick <- rv$tick + 1
    updateTextInput(session, "new", value = ""); on_change(); if (exists("live_touch", mode = "function")) live_touch()
    showNotification(paste0("Renamed ", rv$undo$what, " in ", length(r), " table", if (length(r) != 1) "s", ". Run the pipeline to rename the outputs."), type = "message", duration = 8) })
  observeEvent(input$undo, { u <- rv$undo; if (is.null(u)) return(showNotification("No rename to undo in this session.", type = "message"))
    if (!identical(path_norm(u$pd), path_norm(project_dir()))) return(showNotification("The last rename was in another project.", type = "warning"))
    rename_undo(u$pd, u$before, who()); rv$undo <- NULL; rv$tick <- rv$tick + 1; on_change(); if (exists("live_touch", mode = "function")) live_touch()
    showNotification(paste0("Undone: the rename of ", u$what, "."), type = "message") })
})
