# Ovid/projects.R  --  More than one project per app: a list of the projects this app has used, a new project (empty, or with
# the set-up tables of another), opening one, and deleting one (its outputs and cache, or the whole folder) to the Trash.
# Part of Ovid Core (shared). Sourced by every app.
#
# Each project keeps its own folders and settings: when another is opened, the app's settings are kept in the one left
# (config/project_settings.json) and the opened one's are put back, so new data worked on in a project of its own never mixes
# with the main one. What is deleted goes to the Trash (the Recycle Bin on Windows), never straight off the disk; raw data is
# never inside a project (run_project() refuses that), so it is never touched. Nothing is deleted while a run holds the project.
#
# The app gives how it opens a project (open(path, settings)): it puts the folders and settings into its own inputs.

PROJECT_SETTINGS <- file.path("config", "project_settings.json")
PROJECT_FOLDER_KEYS <- c("raw_dirs", "metadata_dir", "dgps_dir", "fsl_dir", "prog_dir", "meta_dir")   # a new project starts with none of these
PROJECTS_KEEP <- 30L

project_app <- function() sub("^.*// *", "", if (exists("APP_NAME")) APP_NAME else "app")
project_name <- function(path) {                                   # the name in .ovid_project, else the folder's
  mk <- file.path(path.expand(path), PROJECT_MARK)
  n <- if (file.exists(mk)) sub("^name: *", "", grep("^name:", readLines(mk, warn = FALSE), value = TRUE)[1] %|% "") else ""
  if (nzchar(n %|% "")) n else basename(path_norm(path))
}
project_owner <- function(path) { mk <- file.path(path.expand(path), PROJECT_MARK)
  if (file.exists(mk)) ovid_app_name(sub("^app: *", "", grep("^app:", readLines(mk, warn = FALSE), value = TRUE)[1] %|% "")) else "" }

# What the list shows of each project.
project_info <- function(paths) {
  if (!length(paths)) return(data.table(path = character(), name = character(), there = logical(), last_run = character(), output = character(), cache = character(), raw = character()))
  rbindlist(lapply(paths, function(p) { q <- path.expand(p); there <- dir.exists(q)
    # safe: the Projects list shows "never run" for a project whose history cannot be read
    h <- if (there) tryCatch(fread(file.path(q, "output", "audit", "run_history.csv"), colClasses = "character"), error = function(e) NULL) else NULL
    s <- project_settings_read(p)
    data.table(path = p, name = if (there) project_name(p) else basename(p), there = there,
               last_run = if (!is.null(h) && nrow(h) && "started" %in% names(h)) substr(utils::tail(h$started, 1), 1, 16) else "",
               output = if (there && dir.exists(file.path(q, "output"))) fmt_bytes(dir_bytes(file.path(q, "output"))) else "",
               cache = if (there && dir.exists(file.path(q, "cache"))) fmt_bytes(dir_bytes(file.path(q, "cache"))) else "",
               raw = paste(tilde(as.character(unlist(s$raw_dirs))), collapse = "; ")) }))
}

# The projects this app has used: the open one first, then the others as listed in the settings.
projects_known <- function(settings) { p <- unique(c(settings$project_dir, unlist(settings$projects))); p <- p[!is.na(p) & nzchar(p)]
  p[!duplicated(tolower(vapply(p, path_norm, "")))] }
projects_remember <- function(settings, path) utils::head(unique(c(path, projects_known(settings))), PROJECTS_KEEP)

project_settings_read <- function(path) {
  f <- file.path(path.expand(path), PROJECT_SETTINGS)
  project_settings_upgrade(.settings_rd(f) %||% .settings_rd(sub("\\.json$", ".rds", f)) %||% list()) # .rds: saved by an older version
}
# Settings saved by an older version under another name: CS's FSL folder (fsl_dir) is now its logger programs folder (prog_dir).
project_settings_upgrade <- function(s) {
  old <- trim(as.character(unlist(s$fsl_dir))[1] %|% ""); now <- trim(as.character(unlist(s$prog_dir))[1] %|% "")
  if (nzchar(old) && !nzchar(now)) s$prog_dir <- old
  s }
# An opened project's settings into an app's own list (cfg, such as RS's CFG): every setting the project keeps (keys: only
# these), a blank one leaving the app's value as it is; the project's folder always. Returns the list.
project_apply_settings <- function(cfg, s, path = s$project_dir, keys = NULL) {
  for (k in setdiff(if (is.null(keys)) names(s) else intersect(keys, names(s)), c("projects", "ui", "project_dir"))) { v <- s[[k]]
    if (is.null(v) || (length(v) == 1 && (is.na(v) || identical(trim(as.character(v)), "")))) next
    cfg[k] <- list(v) }
  if (!is.null(path)) cfg$project_dir <- path.expand(path)
  cfg }
project_settings_save <- function(path, settings) {
  q <- path.expand(path); if (!dir.exists(q)) return(invisible(FALSE))
  dir.create(file.path(q, "config"), showWarnings = FALSE)
  s <- settings[setdiff(names(settings), c("projects", "ui"))]; attr(s, "from_backup") <- NULL
  f <- file.path(q, PROJECT_SETTINGS)
  tmp <- paste0(f, ".part")
  writeLines(settings_json(s), tmp, useBytes = TRUE)
  if (!file.rename(tmp, f)) {
    file.copy(tmp, f, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(TRUE)
}

# A new project folder: refused when the folder holds other things or another app's project. copy_from: a project whose set-up
# tables (config/*.csv, not their history) are copied; settings: what the new project starts with (folders left out).
project_new <- function(path, name = "", copy_from = NULL, settings = list()) {
  q <- path.expand(trim(path)); if (!nzchar(q)) project_stop("Give the new project a folder.")
  if (dir.exists(q)) { other <- setdiff(list.files(q, all.files = TRUE, no.. = TRUE), PROJECT_KNOWN)
    if (length(other)) project_stop("The folder ", tilde(path_norm(q)), " already holds other files (", paste(utils::head(other, 3), collapse = ", "), "). Choose a new or empty folder.")
    ow <- project_owner(q); if (nzchar(ow) && !identical(ow, project_app())) project_stop("The folder ", tilde(path_norm(q)), " is a project of Ovid ", ow, ".") }
  dir.create(file.path(q, "config"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(paste0("app: ", project_app()), if (nzchar(trim(name))) paste0("name: ", trim(name)), "This folder is an Ovid project. Raw data is never kept here."), file.path(q, PROJECT_MARK))
  if (!is.null(copy_from) && nzchar(copy_from)) {
    f <- list.files(file.path(path.expand(copy_from), "config"), pattern = "\\.csv$", full.names = TRUE)
    if (length(f)) file.copy(f, file.path(q, "config"), overwrite = FALSE, copy.date = TRUE) }
  s <- settings; for (k in intersect(PROJECT_FOLDER_KEYS, names(s))) s[k] <- list(if (k == "raw_dirs") character() else "")
  s$project_dir <- q; project_settings_save(q, s)
  invisible(q)
}

# ---- renaming and duplicating ---------------------------------------------------------------------------------------------------
# A project name: not empty, no / \ or :, and not the name of another project in the list (others: their paths). "" when it is
# fine, else what is wrong (shown under the field).
project_name_problem <- function(name, others = character(), path = NULL) { n <- trim(name %||% "")
  if (!nzchar(n)) return("Give the project a name.")
  if (grepl("[/\\\\:]", n)) return("A name cannot hold / \\ or :.")
  o <- others[!is.na(others) & nzchar(others)]; if (!is.null(path)) o <- o[vapply(o, path_norm, "") != path_norm(path)]
  if (length(o) && tolower(n) %in% tolower(vapply(o, project_name, ""))) return(paste0("There is already a project called ", n, "."))
  "" }
# The folder new projects go in (~/Desktop/Ovid_Projects/<app>), and whether a project lives there (then renaming renames
# its folder too) or is the folder the app made itself when none was set (the automatic folder).
projects_root <- function(app = project_app()) path_norm(ovid_projects_home(app))
project_in_root <- function(path, app = project_app()) identical(path_norm(dirname(path.expand(path))), projects_root(app))
project_automatic <- function(path) identical(path_norm(path), path_norm(ovid_project_dir("")))
# Rename: under Ovid_Projects the folder is renamed with the project; elsewhere only the name it shows (in .ovid_project).
# Returns the project's path afterwards.
project_rename <- function(path, name, others = character()) {
  why <- project_name_problem(name, others, path); if (nzchar(why)) project_stop(why)
  q <- path.expand(path); if (!dir.exists(q)) project_stop("The folder ", tilde(path_norm(q)), " is not there (a drive not plugged in?).")
  if (!is.null(lock_holder(q))) project_stop("A run is going on this project. Wait for it to end.")
  n <- trim(name); mk <- file.path(q, PROJECT_MARK)
  old <- if (file.exists(mk)) readLines(mk, warn = FALSE) else paste0("app: ", project_app())
  writeLines(c(grep("^name:", old, value = TRUE, invert = TRUE)[1], paste0("name: ", n), grep("^(app|name):", old, value = TRUE, invert = TRUE)), mk)
  if (project_in_root(q)) { to <- file.path(dirname(q), n)
    if (!identical(path_norm(to), path_norm(q))) { if (file.exists(to)) project_stop("A folder called ", n, " is already in ", tilde(dirname(q)), ".")
      if (!file.rename(q, to)) project_stop("The folder could not be renamed (is a file in it open?). The name was changed; the folder kept its name.")
      return(invisible(to)) } }
  invisible(q) }
# Duplicate: a new project with the same settings and set-up tables (config/*.csv), not its outputs.
project_duplicate <- function(path, name, others = character(), dest = NULL, settings = project_settings_read(path)) {
  why <- project_name_problem(name, others); if (nzchar(why)) project_stop(why)
  to <- dest %||% file.path(projects_root(), trim(name))
  q <- project_new(to, trim(name), copy_from = path, settings = settings)
  s <- project_settings_read(path)                                  # the same raw folders: it works on the same data
  s$project_dir <- q; project_settings_save(q, s)
  invisible(q) }

# ---- deleting to the Trash ---------------------------------------------------------------------------------------------------
# One folder or file to the Trash: a Mac's own disk to ~/.Trash, another drive through the Finder, Windows to the Recycle Bin,
# Linux through gio. TRUE when it went; otherwise nothing is deleted.
to_trash <- function(path) {
  p <- path_norm(path); if (!file.exists(p)) return(TRUE)
  td <- getOption("ovid.trash_dir"); if (!is.null(td)) { dir.create(td, showWarnings = FALSE)            # tests: a folder of their own
    return(file.rename(p, file.path(td, paste0(basename(p), "_", as.integer(runif(1, 1, 1e9)))))) }
  os <- Sys.info()[["sysname"]]
  if (os == "Darwin") {
    home_trash <- file.path(path.expand("~"), ".Trash")
    if (dir.exists(home_trash) && startsWith(p, "/Users/")) { d <- file.path(home_trash, basename(p)); k <- 1L
      while (file.exists(d)) { k <- k + 1L; d <- file.path(home_trash, paste(basename(p), format(Sys.time(), "%H.%M.%S"), k)) }
      if (file.rename(p, d)) return(TRUE) }
    ok <- suppressWarnings(system2("osascript", c("-e", shQuote(sprintf('tell application "Finder" to delete (POSIX file "%s" as alias)', gsub('"', '\\\\"', p)))), stdout = FALSE, stderr = FALSE))
    return(identical(ok, 0L) && !file.exists(p)) }
  if (.Platform$OS.type == "windows") {
    cmd <- sprintf("Add-Type -AssemblyName Microsoft.VisualBasic; [Microsoft.VisualBasic.FileIO.FileSystem]::%s('%s','OnlyErrorDialogs','SendToRecycleBin')",
                   if (dir.exists(p)) "DeleteDirectory" else "DeleteFile", gsub("'", "''", normalizePath(p, winslash = "\\")))
    ok <- suppressWarnings(system2("powershell", c("-NoProfile", "-Command", shQuote(cmd)), stdout = FALSE, stderr = FALSE))
    return(identical(ok, 0L) && !file.exists(p)) }
  ok <- suppressWarnings(system2("gio", c("trash", shQuote(p)), stdout = FALSE, stderr = FALSE)); identical(ok, 0L) && !file.exists(p)
}

# What deleting would take: "outputs" (output/ and cache/, the set-up kept) or "project" (the whole folder). A list of the items
# (path, size) and why it cannot be done ("" when it can).
project_delete_plan <- function(path, what = c("outputs", "project")) {
  what <- match.arg(what); q <- path.expand(path); why <- ""
  if (!dir.exists(q)) return(list(items = character(), size = 0, why = "The folder is not there."))
  if (!is.null(lock_holder(q))) why <- "A run is going on this project. Wait for it to end."
  ow <- project_owner(q); if (!nzchar(why) && nzchar(ow) && !identical(ow, project_app())) why <- paste0("This folder is a project of Ovid ", ow, ".")
  if (what == "project") { other <- setdiff(list.files(q, all.files = TRUE, no.. = TRUE), PROJECT_KNOWN)
    if (!nzchar(why) && length(other)) why <- paste0("The folder also holds things Ovid did not make (", paste(utils::head(other, 3), collapse = ", "), "): delete them yourself first, or delete only the outputs.")
    items <- q }
  else items <- file.path(q, c("output", "cache", OUTPUT_KEPT, OUTPUT_FAILED))
  items <- items[file.exists(items)]
  list(items = items, size = sum(vapply(items, function(i) if (dir.exists(i)) dir_bytes(i) else file.size(i), 0)), why = why)
}
project_delete <- function(path, what = c("outputs", "project")) {
  pl <- project_delete_plan(path, what); if (nzchar(pl$why)) project_stop(pl$why)
  gone <- vapply(pl$items, to_trash, NA)
  if (!all(gone)) project_stop("These could not be moved to the Trash, so they were left as they are: ", paste(tilde(pl$items[!gone]), collapse = ", "), ".")
  invisible(pl$items)
}

# An opened project's settings into the app's inputs (those of `ids` the settings have). join: how a list is written in its
# text box (raw_dirs one per line).
projects_inputs <- function(session, s, ids, join = c(raw_dirs = "\n")) for (k in intersect(ids, names(s))) {
  v <- s[[k]]; if (is.null(v)) v <- ""
  if (k %in% names(join)) v <- paste(as.character(unlist(v)), collapse = join[[k]]) else if (length(v) > 1) v <- paste(as.character(v), collapse = ", ")
  if (length(v) == 1 && is.na(v)) v <- ""
  session$sendInputMessage(k, list(value = v)) }

# The project switcher in the top bar (ovid_page(project = projects_switch_ui("proj"))): the open project named with its
# folder; its menu opens another, makes a new one (empty or with this set-up), renames or duplicates this one, deletes its
# outputs or the project, and takes a project off the list.
projects_switch_ui <- function(id) { ns <- NS(id)
  tags$details(class = "d-pop d-pop-left d-projsw", tags$summary(class = "btn btn-default btn-sm", uiOutput(ns("sw_label"), inline = TRUE)),
    div(class = "d-pop-body", uiOutput(ns("sw_menu")))) }

# ---- the Projects list (Settings) ----------------------------------------------------------------------------------------------
# Read-only: every project this app has used, with its folders, last run and sizes. Opening, making, renaming and deleting
# are in the switcher in the top bar.
projects_list_ui <- function(id) { ns <- NS(id)
  tagList(p(class = "text-muted", "Each project keeps its own folders, set-up tables, settings and outputs: work new data in a project of its own so it never",
            "mixes with your main one. Open, make, rename or delete a project from the project menu in the top bar."),
          DT::DTOutput(ns("list"))) }
projects_panel <- function() div(class = "panel-x", h3("Projects"), projects_list_ui("proj"))

# The settings with the open project's folder filled in from the app (settings_now) when the saved settings have none yet
# (first start with raw data only, or the automatic folder), so the open project is always in the list.
projects_with_open <- function(saved, now) { if (nzchar(saved$project_dir %||% "")) return(saved); p <- now$project_dir
  if (length(p) == 1 && !is.na(p) && nzchar(p)) saved$project_dir <- p; saved }
# open(path, settings): the app opens a project (puts its folders and settings into the app's inputs). settings_now(): the
# app's settings as they stand (to keep in the project being left).
projects_server <- function(id, open, settings_now, settings_file = get0("SETTINGS_FILE")) moduleServer(id, function(input, output, session) {
  ns <- session$ns; tick <- reactiveVal(0)
  st <- function() { e <- env_with("SETTINGS", environment(projects_server)); if (is.null(e)) list() else get("SETTINGS", envir = e) }
  # safe: the list applies in this session; only remembering it failed
  st_set <- function(x) { if (!is.null(settings_file)) tryCatch(settings_save(settings_file, x), error = function(e) NULL); live_touch()
    e <- env_with("SETTINGS", environment(projects_server)); if (!is.null(e)) { s <- get("SETTINGS", envir = e); s[names(x)] <- x; assign("SETTINGS", s, envir = e) } }
  rows <- reactive({ tick(); live_settings(settings_file)                     # projects opened or made on another page
    # safe: as above
    s <- projects_with_open(st(), tryCatch(settings_now(), error = function(e) list())); x <- project_info(projects_known(s))
    if (nrow(x)) { x[, open := vapply(path, path_norm, "") == path_norm(s$project_dir %||% "")]
      x[open == TRUE, raw := paste(tilde(as.character(unlist(s$raw_dirs))), collapse = "; ")] }        # the open project's folders as they stand
    x })
  output$list <- DT::renderDT({ x <- rows(); validate(need(nrow(x) > 0, "No project yet: set a project folder, or make a new project."))
    DT::datatable(x[, .(Project = paste0(name, ifelse(open, "  (open)", ifelse(there, "", "  (folder not found)"))), Folder = paste0(tilde(path), ifelse(vapply(path, project_automatic, NA), "  (automatic folder)", "")), `Raw folders` = raw,
                        `Last run` = last_run, Output = output, Cache = cache)],
                  selection = "none", rownames = FALSE, options = list(dom = "t", pageLength = 50, ordering = FALSE)) })
  # the switcher in the top bar
  output$sw_label <- renderUI({ x <- rows(); o <- if (nrow(x)) x[open == TRUE] else x
    if (!nrow(o)) return(span(icon("folder-open"), "No project open"))
    tagList(span(class = "dim", style = "font-size:10.5px;letter-spacing:.08em;text-transform:uppercase", "Project"), tags$b(o$name[1]),
            span(class = "path", tilde(o$path[1]), if (project_automatic(o$path[1])) " \u00b7 automatic folder")) })
  output$sw_menu <- renderUI({ x <- rows()
    div(class = "d-proj-list",
      lapply(seq_len(nrow(x)), function(i) div(class = paste("d-proj-row", if (isTRUE(x$open[i])) "on"),
        div(class = "t", tags$b(x$name[i]), span(paste0(tilde(x$path[i]), if (!x$there[i]) " \u00b7 folder not found" else if (nzchar(x$last_run[i])) paste0(" \u00b7 run ", x$last_run[i]) else " \u00b7 never run"))),
        if (isTRUE(x$open[i])) span(class = "dim", "open") else span(
          if (x$there[i]) tags$button(type = "button", class = "btn btn-default btn-sm", onclick = sprintf("Shiny.setInputValue('%s',%s,{priority:'event'})", ns("sw_open"), jsonlite::toJSON(x$path[i], auto_unbox = TRUE)), "Open"),
          tags$button(type = "button", class = "btn btn-default btn-sm", title = "Remove from list (the folder is kept)", onclick = sprintf("Shiny.setInputValue('%s',%s,{priority:'event'})", ns("sw_forget"), jsonlite::toJSON(x$path[i], auto_unbox = TRUE)), "\u00d7")))),
      div(class = "d-proj-acts",
        tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',1,{priority:'event'});return false", ns("sw_new")), "\uff0b New project\u2026"),
        tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',Date.now(),{priority:'event'});return false", ns("sw_ren")), "Rename this project\u2026"),
        tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',Date.now(),{priority:'event'});return false", ns("sw_dup")), "Duplicate this project\u2026", span("settings and set-up, not outputs")),
        tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s','outputs',{priority:'event'});return false", ns("sw_del")), "Delete this project's outputs\u2026", span("set-up kept")),
        tags$a(href = "#", class = "danger", onclick = sprintf("Shiny.setInputValue('%s','project',{priority:'event'});return false", ns("sw_del")), "Delete this project\u2026", span("to the Trash; raw data never touched")))) })
  observeEvent(input$sw_open, { x <- rows(); r <- x[path == input$sw_open]; if (!nrow(r)) return()
    if (run_busy()) return(showNotification("A run is going: wait for it before opening another project.", type = "warning")); switch_to(r$path[1]) })
  switch_to <- function(path) {
    cur <- st()$project_dir %||% ""; now <- tryCatch(settings_now(), error = function(e) st())
    if (nzchar(cur) && dir.exists(path.expand(cur))) project_settings_save(cur, now)          # the project left keeps its settings
    s <- project_settings_read(path); s$project_dir <- path.expand(path)
    for (k in intersect(PROJECT_FOLDER_KEYS, names(now))) if (is.null(s[[k]])) s[k] <- list(if (k == "raw_dirs") character() else "")
    st_set(c(s, list(projects = projects_remember(st(), path.expand(path)))))
    open(path.expand(path), s); tick(tick() + 1)
    showNotification(paste0("Opened ", project_name(path), "."), type = "message") }
  observeEvent(input$sw_new, { x <- rows()
    showModal(modalDialog(title = "New project", easyClose = TRUE,
      textInput(ns("new_name"), "Name", placeholder = "e.g. 2026 field season, new site"),
      textInput(ns("new_dir"), "Folder (a new or empty one, outside the raw folders)", width = "100%"),
      selectInput(ns("new_copy"), "Start with the set-up tables of", c("None (empty)" = "", setNames(x$path[x$there], x$name[x$there])), width = "100%"),
      p(class = "text-muted", "The new project starts with no raw folders: set them on the Settings page. Processing settings are carried over from the open project."),
      footer = tagList(modalButton("Cancel"), actionButton(ns("new_ok"), "Make the project", class = "btn-primary")))) })
  observeEvent(input$new_ok, {
    r <- tryCatch(project_new(input$new_dir, input$new_name, if (nzchar(input$new_copy %||% "")) input$new_copy, tryCatch(settings_now(), error = function(e) st())),
                  error = function(e) e)
    if (inherits(r, "error")) return(ovid_field_error("new_dir", conditionMessage(r)))
    removeModal(); switch_to(r) })
  # rename and duplicate, with the name checked as it is typed (empty, / \\ :, a name already used)
  pending_nd <- NULL; nd_fail <- reactiveVal("")
  name_dialog <- function(x, what) { pending_nd <<- list(x = x, what = what); nd_fail("")
    showModal(modalDialog(title = if (what == "ren") paste0("Rename ", x$name) else paste0("Duplicate ", x$name), easyClose = TRUE,
      textInput(ns("nd_name"), "Name", value = if (what == "ren") x$name else paste(x$name, "copy"), width = "100%"), uiOutput(ns("nd_err")),
      p(class = "text-muted", if (what == "dup") paste0("A new project in ", tilde(projects_root()), " with the same settings, raw folders and set-up tables; outputs are not copied.")
        else if (project_in_root(x$path)) "The folder is renamed with the project." else paste0("The folder ", tilde(x$path), " keeps its name; the project shows the new one.")),
      footer = tagList(modalButton("Cancel"), actionButton(ns("nd_ok"), if (what == "ren") "Rename" else "Duplicate", class = "btn-primary")))) }
  output$nd_err <- renderUI({ x <- pending_nd; if (is.null(x)) return(NULL); req(!is.null(input$nd_name))
    why <- project_name_problem(input$nd_name, rows()$path, if (x$what == "ren") x$x$path); if (!nzchar(why)) why <- nd_fail()
    if (nzchar(why)) div(class = "text-danger", style = "font-size:12px;margin-top:-8px", why) })
  observeEvent(input$sw_ren, { x <- rows()[open == TRUE]; if (nrow(x)) name_dialog(x[1], "ren") })
  observeEvent(input$sw_dup, { x <- rows()[open == TRUE]; if (nrow(x)) name_dialog(x[1], "dup") })
  observeEvent(input$nd_ok, { d <- pending_nd; req(d); x <- d$x
    if (d$what == "ren" && isTRUE(x$open) && run_busy()) return(showNotification("A run is going: wait for it before renaming this project.", type = "warning"))
    r <- tryCatch(if (d$what == "ren") project_rename(x$path, input$nd_name, rows()$path) else project_duplicate(x$path, input$nd_name, rows()$path), error = function(e) e)
    if (inherits(r, "error")) { nd_fail(conditionMessage(r)); return() }
    removeModal(); pending_nd <<- NULL; nd_fail("")
    if (d$what == "dup") { st_set(list(projects = unique(c(projects_known(st()), r)))); tick(tick() + 1)
      return(showNotification(paste0("Made ", project_name(r), " in ", tilde(r), ". Open it from the project menu."), type = "message", duration = 8)) }
    if (!identical(path_norm(r), path_norm(x$path))) {                # the folder moved: the list (and the app, when it is the open one) follow it
      st_set(list(projects = sub(x$path, r, projects_known(st()), fixed = TRUE)))
      if (isTRUE(x$open)) { s <- st(); s$project_dir <- r; st_set(list(project_dir = r)); open(r, utils::modifyList(project_settings_read(r), list(project_dir = r))) } }
    tick(tick() + 1); showNotification(paste0("Renamed to ", project_name(r), "."), type = "message") })
  pending_del <- NULL
  del <- function(what, x) { pending_del <<- x
    pl <- project_delete_plan(x$path, what)
    if (nzchar(pl$why)) return(showNotification(pl$why, type = "warning", duration = NULL))
    if (!length(pl$items)) return(showNotification("There is nothing to delete.", type = "message"))
    showModal(modalDialog(title = if (what == "project") paste0("Delete the project ", x$name, "?") else paste0("Delete the outputs of ", x$name, "?"), easyClose = TRUE,
      p(if (what == "project") "The whole project folder goes to the Trash: its set-up tables, settings, outputs and cache." else
          "Its outputs, run logs and cache go to the Trash. The set-up tables and settings stay; the next run makes the outputs again from the raw files."),
      tags$ul(lapply(pl$items, function(i) tags$li(tilde(i)))), p(tags$b(fmt_bytes(pl$size)), " in all. Raw data is not touched."),
      footer = tagList(modalButton("Cancel"), actionButton(ns(paste0("del_ok_", what)), "Move to the Trash", class = "btn-danger")))) }
  observeEvent(input$sw_del, { x <- rows(); o <- x[open == TRUE]; if (!nrow(o)) return(showNotification("No project is open.", type = "message")); del(input$sw_del, o[1]) })
  for (w in c("outputs", "project")) local({ what <- w
    observeEvent(input[[paste0("del_ok_", what)]], { x <- pending_del; pending_del <<- NULL; removeModal(); if (is.null(x)) return()
      r <- tryCatch(project_delete(x$path, what), error = function(e) e)
      if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error", duration = NULL))
      if (what == "project") { st_set(list(projects = setdiff(projects_known(st()), x$path)))
        if (isTRUE(x$open)) showNotification("The open project was deleted: open or make another one.", type = "warning", duration = NULL) }
      tick(tick() + 1); showNotification("Moved to the Trash.", type = "message") }) })
  observeEvent(input$sw_forget, { x <- rows()[path == input$sw_forget]; if (!nrow(x)) return()
    if (isTRUE(x$open[1])) return(showNotification("The open project stays in the list.", type = "message"))
    st_set(list(projects = setdiff(projects_known(st()), x$path[1]))); tick(tick() + 1) })
  invisible(list(refresh = function() tick(tick() + 1)))
})
