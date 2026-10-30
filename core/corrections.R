# Ovid/corrections.R  --  Every change a run makes to the data on its own, listed in one place: what it does, whether it is on,
# and where it is switched. Part of Ovid Core (shared). Sourced by the processors' pipelines (CS, GT, WLR) and so by their apps.
#
# Each app declares CORRECTIONS (data-specific, so in the app): one entry per correction, a list of
#   id       short name
#   what     what it does to the data, in plain words
#   does     "removes", "changes", "fills" or "flags" (flags never change a value)
#   setting  the setting that switches it (TRUE/FALSE, or `on` below decides), or "" when it always runs
#   on       optional function(cfg) -> TRUE/FALSE, for a setting that is not TRUE/FALSE ("undo", "apply", ...)
#   table    optional: the set-up table (config/<file>) whose rows make it, instead of a setting
#   where    where in the app it is switched or edited ("" with `panel`: on the list itself)
#   panel    TRUE: the switch is on the list (a setting with no control elsewhere); it is saved with the app's settings
#   default  the setting's value when the settings do not have it (TRUE unless given)
#   choices  for a setting with more than on and off: its values (named by their labels); the list shows a menu
# Nothing here changes what a run does: it reads the same settings the run reads. corrections_cfg() copies the switches made
# on the list into a run's settings; the run log lists the state of each (corrections_log()).

CORRECTION_KINDS <- c(removes = "Removes values", changes = "Changes values", fills = "Fills in values", flags = "Flags values (none changed)")

correction_state <- function(x, cfg, project_dir = cfg$project_dir) {
  if (nzchar(x$table %||% "")) {
    f <- file.path(path.expand(project_dir %||% ""), "config", x$table)
    # safe: an unreadable table is shown as "by table, unreadable"
    n <- if (nzchar(project_dir %||% "") && file.exists(f)) tryCatch(nrow(fread(file = f, colClasses = "character", na.strings = NULL)), error = function(e) NA_integer_) else 0L
    return(if (is.na(n)) "by table, unreadable" else if (n) paste0("by table, ", n, " row", if (n != 1) "s") else "by table, none") }
  if (!nzchar(x$setting %||% "")) return("always")
  if (length(x$choices)) { v <- cfg[[x$setting]] %||% x$default %||% x$choices[[1]]; return(names(x$choices)[match(v, x$choices)] %|% as.character(v)) }
  on <- if (is.function(x$on)) isTRUE(try_or(x$on(cfg), FALSE, paste("correction", x$id %||% ""))) else isTRUE(cfg[[x$setting]] %||% x$default %||% TRUE)
  if (on) "on" else "off"
}

# The list as a table: does (in words), what, state, where.
corrections_table <- function(cfg, corr = get0("CORRECTIONS", ifnotfound = list())) {
  if (!length(corr)) return(data.table(id = character(), does = character(), what = character(), state = character(), where = character()))
  rbindlist(lapply(corr, function(x) data.table(id = x$id, does = CORRECTION_KINDS[[x$does]], what = x$what, state = correction_state(x, cfg),
                                                where = if (isTRUE(x$panel)) "this list" else x$where %||% "")))
}

# The switches made on the list, from the saved settings, into a run's settings (each app's current_cfg()).
corrections_cfg <- function(cfg, settings = get0("SETTINGS", ifnotfound = list()), corr = get0("CORRECTIONS", ifnotfound = list())) {
  for (x in Filter(function(x) isTRUE(x$panel) && nzchar(x$setting %||% ""), corr)) if (!is.null(settings[[x$setting]]))
    cfg[[x$setting]] <- if (length(x$choices)) as.character(settings[[x$setting]]) else isTRUE(settings[[x$setting]])
  cfg
}

# Lines for the run log: what the run changes on its own, by state.
corrections_log <- function(cfg, corr = get0("CORRECTIONS", ifnotfound = list())) {
  t <- try_or(corrections_table(cfg, corr), where = "corrections list"); if (is.null(t) || !nrow(t)) return(character())
  grp <- function(lab, i) if (any(i)) paste0("  ", lab, ": ", paste(t$id[i], collapse = ", "))
  ch <- !t$state %in% c("on", "off", "always") & !startsWith(t$state, "by table")
  c("Automatic corrections (see the Automatic corrections step of the set-up):", grp("on", t$state == "on"), grp("always", t$state == "always"),
    if (any(tb <- startsWith(t$state, "by table"))) paste0("  by set-up table: ", paste0(t$id[tb], " (", sub("^by table, ", "", t$state[tb]), ")", collapse = ", ")),
    grp("off", t$state == "off"), if (any(ch)) paste0("  set to: ", paste0(t$id[ch], " (", t$state[ch], ")", collapse = ", ")))
}

# ---- the list in the app ---------------------------------------------------------------------------------------------------
corrections_ui <- function(id) { ns <- NS(id)
  tagList(p(class = "text-muted", "Everything a run changes in the data on its own, without a row in a set-up table asking for it, and where each is switched.",
            "Flags never change a value. A switch here is saved with the app's settings and used from the next run."),
          uiOutput(ns("list"))) }
corrections_server <- function(id, cfg_now, settings_file = get0("SETTINGS_FILE")) moduleServer(id, function(input, output, session) {
  ns <- session$ns; L <- get0("CORRECTIONS", ifnotfound = list()); sw <- Filter(function(x) isTRUE(x$panel) && nzchar(x$setting %||% ""), L)
  for (x in sw) local({ k <- x$setting
    val <- function() if (length(x$choices)) as.character(input[[k]]) else isTRUE(input[[k]])
    now <- function() { v <- corrections_cfg(cfg_now())[[k]] %||% x$default %||% (if (length(x$choices)) x$choices[[1]] else TRUE); if (length(x$choices)) as.character(v) else isTRUE(v) }
    observeEvent(input[[k]], ignoreInit = TRUE, {
      if (identical(val(), now())) return()                                                         # drawn, not changed: nothing saved
      # safe: the switch applies in this session; only remembering it failed
      if (!is.null(settings_file)) tryCatch(settings_save(settings_file, setNames(list(val()), k)), error = function(e) NULL)
      if (exists("live_touch", mode = "function")) live_touch()
      e <- env_with("SETTINGS", environment(corrections_server))                                  # the app's own settings, where its code reads them
      if (!is.null(e)) { s <- get("SETTINGS", envir = e); s[[k]] <- val(); assign("SETTINGS", s, envir = e) } }) })
  output$list <- renderUI({
    s <- if (exists("live_settings", mode = "function")) live_settings(settings_file) else get0("SETTINGS", ifnotfound = list())   # a switch flipped on another page
    cfg <- cfg_now(); if (exists("live_config", mode = "function")) live_config(cfg$project_dir)                                       # rows added to a table on any page
    cfg <- corrections_cfg(cfg, s)        # the switches' own values come from the saved settings: the list never redraws on its own inputs
    t <- corrections_table(cfg, L)
    rows <- lapply(seq_len(nrow(t)), function(i) { x <- L[[i]]
      st <- if (isTRUE(x$panel) && length(x$choices)) selectInput(ns(x$setting), NULL, x$choices, cfg[[x$setting]] %||% x$default %||% x$choices[[1]], width = "100%")
            else if (isTRUE(x$panel)) ovid_switch(ns(x$setting), NULL, isTRUE(cfg[[x$setting]] %||% x$default %||% TRUE), width = "100%")
            else span(class = paste0("st-", gsub("[^a-z]", "", t$state[i])), t$state[i])
      tags$tr(tags$td(t$does[i]), tags$td(t$what[i]), tags$td(st), tags$td(class = "text-muted", if (isTRUE(x$panel)) "" else t$where[i])) })
    tags$table(class = "table table-sm d-corr", tags$thead(tags$tr(tags$th("Kind"), tags$th("What the run does"), tags$th("State"), tags$th("Where to change it"))), tags$tbody(rows))
  })
})
