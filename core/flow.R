# ovid core: the set-up flow's shared pieces (the page layouts are in shell.R).
#
#   unsaved_ui(id) / unsaved_server(id, editors, extra)
#       the "N unsaved changes" chip in the top bar: Apply saves every set-up table with unsaved edits (and what extra holds),
#       Discard drops them. Saving keeps the previous version in history (Undo).
#   display_*()
#       which sites or stations show (config/display.csv: name, shown). A hidden one is left out of the strip, Home and
#       Needs you on the Standard view; it is still processed, exported and in the reports.
#   flow_aside(project, gaps, sources)
#       the information panel: project figures, data gaps, the sources of the step and where values come from.
#   review_list_ui(id) / review_list_server(id, ...)
#       Needs you: one list of what waits for a person (the run's flags, loggers that look moved, other findings), each
#       decided there; the decisions on flags go in config/review_decisions.csv.

OVID_FLOW_VERSION <- "1.0.0"
DISPLAY_FILE <- "display.csv"

# ---- unsaved changes --------------------------------------------------------------------------------------------------------
unsaved_ui <- function(id) { ns <- NS(id); uiOutput(ns("chip"), inline = TRUE, class = "d-unsaved") }

# editors: a list (or a function returning one) of editors (core editor.R); extra: a list of list(label: one, or singular and plural; count = reactive,
# apply = function, discard = function) for other edits that wait for Apply.
unsaved_server <- function(id, editors, extra = list(), on_apply = function() NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  eds <- function() { e <- if (is.function(editors)) editors() else editors; Filter(function(x) is.list(x) && is.function(x$dirty), e) }
  pending <- reactive({ d <- eds(); n <- names(d) %||% rep("", length(d))
    a <- unlist(lapply(seq_along(d), function(i) if (isTRUE(d[[i]]$dirty())) n[i] else NULL))
    b <- unlist(lapply(extra, function(x) { k <- x$count(); if (k > 0) paste(k, x$label[min(length(x$label), 1 + (k > 1))]) else NULL }))
    c(if (length(a)) paste("table:", a), b) })
  output$chip <- renderUI({ p <- pending(); n <- length(p)
    if (!n) return(span(class = "dim", style = "font-size:12.5px", "No unsaved changes"))
    tags$details(class = "d-pop d-pop-right", tags$summary(class = "btn btn-primary btn-sm", paste("Apply", n)),
      div(class = "d-pop-body", tags$b(sprintf("%d unsaved change%s", n, if (n == 1) "" else "s")),
        tags$ul(style = "margin:6px 0 10px;padding-left:18px", lapply(p, tags$li)),
        div(class = "btnrow", actionButton(ns("apply"), paste("Apply", n, if (n == 1) "change" else "changes"), class = "btn-primary"),
            actionButton(ns("discard"), "Discard", class = "btn-danger")),
        p(class = "dim", style = "font-size:12px;margin:0", "Apply writes the set-up tables and keeps the previous versions in history (Undo). The next run uses them."))) })
  observeEvent(input$apply, { d <- eds(); k <- 0
    for (e in d) if (isTRUE(isolate(e$dirty()))) { e$save(quiet = TRUE); k <- k + 1 }
    for (x in extra) if (isolate(x$count()) > 0) { x$apply(); k <- k + 1 }
    on_apply(); showNotification(sprintf("%d change%s applied. The previous tables are kept in history; the next run uses them.", k, if (k == 1) "" else "s"), type = "message") })
  observeEvent(input$discard, { for (e in eds()) if (is.function(e$discard)) e$discard(); for (x in extra) x$discard()
    showNotification("Unsaved changes discarded.", type = "message") })
  invisible(list(count = reactive(length(pending()))))
})

# ---- what shows ----------------------------------------------------------------------------------------------------------------
display_path <- function(pd) file.path(path.expand(pd), "config", DISPLAY_FILE)
display_read <- function(pd) { p <- display_path(pd)
  # safe: a table that cannot be read shows nothing here; its editor says why
  d <- if (file.exists(p)) tryCatch(data.table::fread(p, colClasses = "character"), error = function(e) NULL) else NULL
  if (is.null(d) || !"name" %in% names(d)) return(data.table::data.table(name = character(), shown = character()))
  if (!"shown" %in% names(d)) d[["shown"]] <- ""
  d[, .(name = trimws(name), shown)] }
.no <- function(x) tolower(trimws(as.character(x))) %in% c("false", "no", "0", "n")
display_hidden <- function(pd) { d <- display_read(pd); d$name[.no(d$shown)] }          # still processed, not shown
display_shown <- function(pd, names) names[!names %in% display_hidden(pd)]
display_set <- function(pd, name, shown = NULL) {
  d <- display_read(pd); for (nm in name) { i <- which(d$name == nm)
    if (!length(i)) { d <- rbind(d, data.table::data.table(name = nm, shown = "TRUE")); i <- nrow(d) }
    if (!is.null(shown)) d$shown[i] <- if (isTRUE(shown)) "TRUE" else "FALSE" }
  p <- display_path(pd); dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  # safe: the save goes on; only the history copy is missed
  if (exists("backup_cfg", mode = "function") && file.exists(p)) try(backup_cfg(p, d, "display"), silent = TRUE)
  if (exists("fwrite_safe", mode = "function")) fwrite_safe(d, p) else data.table::fwrite(d, p)
  if (exists("live_touch", mode = "function")) live_touch()
  invisible(d) }
# "Showing 14 of 16 sites" for the top bar
display_context <- function(pd, names, what = "sites") { n <- length(unique(names)); if (!n) return(NULL)
  k <- length(display_shown(pd, unique(names))); span(class = "d-crumb", title = "Choose what shows on Sites: rename, what shows", sprintf("Showing %d of %d %s", k, n, what)) }

# ---- the information panel ---------------------------------------------------------------------------------------------------
# A length of time in words: "30 hours", "12 days", "7 months", "5.2 years"
flow_span <- function(hours) {
  h <- as.numeric(hours); d <- h / 24
  ifelse(h < 48, paste(round(h), "hours"), ifelse(d < 60, paste(round(d), "days"),
    ifelse(d < 730, paste(round(d / 30.44), "months"), paste(format(round(d / 365.25, 1), nsmall = 1), "years"))))
}

# project: a list of rows list(label, value, level, go); gaps: a table (where, what, when, go: the step that decides it);
# sources: a table (cite, what, ref).
flow_aside <- function(project = list(), gaps = NULL, sources = NULL, precedence = TRUE)
  tagList(
    if (length(project)) ovid_aside_sec("This project", lapply(project, function(x) ovid_kv(x[[1]], x[[2]], x$level, x$go))),
    if (!is.null(gaps)) ovid_aside_sec("Data gaps", n = if (NROW(gaps)) NROW(gaps) else NULL,
      if (!NROW(gaps)) div(class = "dim", "None found.") else lapply(seq_len(min(NROW(gaps), 12)), function(i)
        div(class = "d-gap", tags$i(), tags$b(gaps$where[i]), if (!is.null(gaps$go) && nzchar(gaps$go[i] %||% "")) tags$a(href = "#", `data-ovid-go` = gaps$go[i], style = "font-size:11.5px", "decide") else span(),
            div(class = "w", paste(c(gaps$what[i], gaps$when[i]), collapse = " \u00b7 ")))),
      if (NROW(gaps) > 12) div(class = "dim", "and ", NROW(gaps) - 12, " more")),
    if (!is.null(sources) && NROW(sources)) ovid_aside_sec("Sources for this step", lapply(seq_len(NROW(sources)), function(i)
      div(class = "d-src", tags$b(sources$cite[i]), " ", sources$what[i], if (!is.null(sources$ref) && nzchar(sources$ref[i] %||% "")) div(sources$ref[i])))),
    if (isTRUE(precedence)) ovid_aside_sec("Where values come from",
      p(style = "margin:0;color:var(--fg4)", "What you type wins over the metadata file, which wins over what the data say. Every difference is listed in Needs you.")))

# A precipitation record for the bars under the plots: any CSV with a time column and a column of millimetres (by name: mm,
# precip, rain; else the first numeric column). NULL when there is no file or it cannot be read.
flow_precip <- function(path) {
  path <- trimws(path %||% ""); if (!nzchar(path) || !file.exists(path.expand(path))) return(NULL)
  # safe: a preview only; nothing is shown for a file that cannot be read
  d <- tryCatch(data.table::fread(path.expand(path)), error = function(e) NULL); if (is.null(d) || !nrow(d)) return(NULL)
  nm <- names(d); tk <- grep("time|date|^ts$", nm, ignore.case = TRUE)[1]; if (is.na(tk)) tk <- 1L
  num <- which(vapply(d, is.numeric, NA)); vk <- intersect(grep("mm|precip|rain", nm, ignore.case = TRUE), num)[1]; if (is.na(vk)) vk <- setdiff(num, tk)[1]
  if (is.na(vk)) return(NULL)
  t <- if (exists("parse_dt", mode = "function")) parse_dt(as.character(d[[tk]])) else as.POSIXct(as.character(d[[tk]]), tz = "UTC")
  ok <- !is.na(t); if (!any(ok)) return(NULL)
  list(t = t[ok], v = as.numeric(d[[vk]])[ok], name = "Precipitation", unit = "mm", file = basename(path)) }

# The last changes saved to the set-up tables (config/history/change_log.csv, core config.R), newest first. more: a step to
# link to for every change, or NULL.
flow_saved <- function(pd, n = 8, more = NULL) {
  f <- file.path(path.expand(pd), "config", "history", "change_log.csv")
  # safe: as for line 50: its editor says why
  d <- if (file.exists(f)) tryCatch(data.table::fread(f, colClasses = "character"), error = function(e) NULL) else NULL
  if (is.null(d) || !nrow(d)) return(p(class = "dim", "Nothing saved yet."))
  d <- d[order(-seq_len(nrow(d)))][seq_len(min(n, nrow(d)))]
  tagList(tags$table(class = "table", tags$thead(tags$tr(tags$th("When"), tags$th("Table"), tags$th("Rows after"))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(tags$td(substr(d$time[i], 1, 16)), tags$td(d$table[i]), tags$td(d$rows_after[i]))))),
    if (!is.null(more)) tags$a(href = "#", `data-ovid-go` = more, "Every change \u203a")) }

# A second Run button where a step ends in a run (it presses the one in the top bar)
flow_run_once <- function(label = "Run once")
  tags$button(type = "button", class = "btn btn-primary", title = "Save the set-up tables and run the pipeline (the same as Run pipeline in the top bar)",
              onclick = "var b = document.getElementById('run'); if (b) b.click();", icon("play"), label)

# ---- Needs you: the one review list --------------------------------------------------------------------------------------------
# What waits for a person after a run, in one list on the Needs you step (both views show it; the Workbench's Needs you
# popover counts it): the last run's flags (Keep, Remove..., Correct..., Not a problem), loggers that look moved (Split here)
# and the run's other findings (each with the page that settles it). Every decision on a flag goes in config/review_decisions.csv
# (core config.R), whichever view it is made in. The app feeds the items in (review_flag_items() and its own findings).
#
# Items: a table with kind ("flag", "moved" or "note"), where, what, detail and, by kind:
#   flag   parameter, start, end, check, records, qids (the review ids of its flags, space-separated) and any column the app's
#          callbacks need (key, serial, sensor ...);
#   moved  serial, at (where to split);
#   note   go (the page that settles it).
# What a person does about an item (M 14 Oct): decide (a flag to keep, remove or note), fix (a set-up change ovid makes),
# raw (a problem in the files themselves, which ovid cannot fix) or add (a file that is missing and may exist). An app can
# name it per item (column fix); otherwise it follows from the kind and the wording.
NEEDS_LABELS <- c(decide = "decide", fix = "fix in ovid", raw = "raw data", add = "add a file")
NEEDS_LEVEL  <- c(decide = "warn", fix = "ok", raw = "error", add = "info")
needs_fix_of <- function(kind, text, fix = NULL) {
  kind <- as.character(kind); t <- tolower(as.character(text)); out <- ifelse(kind == "flag", "decide", "fix")
  note <- kind != "flag" & kind != "moved"
  out[note & grepl("does not exist|not found|cannot be read|could not be read|unreadable|damaged|no serial|empty file|corrupt", t)] <- "raw"
  out[note & grepl("^no .* found|missing .*(file|log|workbook)|no .*(flight )?log", t)] <- "add"
  if (length(fix)) { f <- as.character(fix); ok <- !is.na(f) & f %in% names(NEEDS_LABELS); out[ok] <- f[ok] }
  out }
# The group an item is listed under: its place (site, station, serial); the project's own findings under Project. An app can
# name it per item (column group).
needs_group_of <- function(kind, where, group = NULL) {
  g <- ifelse(as.character(kind) %in% c("flag", "moved"), as.character(where), "Project")
  if (length(group)) { x <- as.character(group); ok <- !is.na(x) & nzchar(x); g[ok] <- x[ok] }
  g }
REVIEW_KEEP <- "kept"                                        # Keep: the values stay, flagged; the item leaves the list
# The open flags of a run grouped into items: one per place, check and stretch (flags less than gap_h hours apart join).
# where, parameter: the columns of flags that name the place and the value (station and parameter, or site and channel);
# keep: other columns carried into the item (their first value, or all values joined with ", " when named in join).
review_flag_items <- function(flags, where, parameter = "parameter", keep = character(), join = character(), gap_h = 6) {
  empty <- data.table(kind = character(), where = character(), what = character(), detail = character(), parameter = character(), start = character(),
                      end = character(), check = character(), records = numeric(), qids = character())
  if (is.null(flags) || !NROW(flags)) return(empty)
  f <- copy(as.data.table(flags)); set(f, j = ".w", value = as.character(f[[where]])); set(f, j = ".p", value = as.character(f[[parameter]]))
  f[, `:=`(t0 = as.numeric(parse_dt(review_ts(start))), t1 = as.numeric(parse_dt(review_ts(end))))]; f[is.na(t1), t1 := t0]
  f <- f[!is.na(t0)][order(.w, check, t0)]; if (!nrow(f)) return(empty)                  # a flag with no time cannot be shown or removed
  f[, grp := cumsum(c(TRUE, diff(t0) > gap_h * 3600 | .w[-1] != .w[-.N] | check[-1] != check[-.N]))]
  if (!"detail" %in% names(f)) f[, detail := ""]
  fmt <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  kc <- intersect(keep, names(f))
  g <- f[, c(list(kind = "flag", where = .w[1], what = gsub("_", " ", check[1]), detail = as.character(detail[1]), parameter = paste(unique(.p), collapse = ", "),
                  start = fmt(min(t0)), end = fmt(max(t1)), check = check[1], records = sum(as.numeric(records), na.rm = TRUE),
                  qids = paste(wb_queue_id(.w, .p, start, end, check), collapse = " ")),
             lapply(stats::setNames(kc, kc), function(k) { v <- as.character(.SD[[k]])
               if (k %in% join) paste(unique(v[!is.na(v) & nzchar(v)]), collapse = ", ") else v[1] })), by = grp, .SDcols = kc][, grp := NULL]
  g[order(-records)][]
}
# Each item's decision (NA when none): a flag is decided when every one of its flags has a decision in d (the review table).
review_item_decision <- function(items, d) {
  if (!NROW(items)) return(character())
  vapply(seq_len(NROW(items)), function(i) { if (!identical(items$kind[i], "flag")) return(NA_character_)
    q <- strsplit(as.character(items$qids[i] %||% ""), " ", fixed = TRUE)[[1]]; q <- q[nzchar(q)]
    if (!length(q) || is.null(d) || !NROW(d) || !all(q %in% d$id)) return(NA_character_)
    paste(unique(d$decision[match(q, d$id)]), collapse = ", ") }, "")
}
# A row of the corrections over a period (core periods.R) for an item: action "remove", "offset", ... over its stretch.
review_period_row <- function(r, action, value = "", note = "", author = "")
  data.frame(target = r$where, parameter = paste(trim(strsplit(r$parameter, ",", fixed = TRUE)[[1]]), collapse = "|"), from = r$start, to = r$end,
             action = action, value = as.character(value), on = "", note = note, author = author, added = format(Sys.time(), "%Y-%m-%d %H:%M"), stringsAsFactors = FALSE)

review_list_ui <- function(id) { ns <- NS(id)
  tagList(div(class = "d-controls", style = "align-items:center",
      tags$label(class = "tsv-sw ovid-sw", tags$input(type = "checkbox", role = "switch", id = ns("only"), checked = NA), span("Only what needs a look")),
      uiOutput(ns("summary"), inline = TRUE)),
    uiOutput(ns("list"))) }
# items(): the items (above); file(): the path of config/review_decisions.csv. open(row): shows that record at that stretch;
# remove(row): removes a flag's values (the app's own removals); period: the corrections-over-a-period editor, for Correct...
# (and Remove... when the app gives no remove); split(serial, at): splits a logger that looks moved; goto(row): opens the page
# that settles a finding (else its go, a page value). The Workbench replaces
# open and gives preset (its period tool's reactiveVal) through the returned handlers, so Remove... and Correct... open its tool.
# Returns items (the open ones, for the Needs you popover) and handlers.
review_list_server <- function(id, items, file, open = NULL, remove = NULL, period = NULL, split = NULL, goto = NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns; h <- new.env(); h$open <- open; h$preset <- NULL
  done <- reactive({ p <- file(); live_stamp(p); review_read(p) })
  # safe: nothing to list yet
  rows <- reactive({ x <- tryCatch(items(), error = function(e) NULL); if (is.null(x) || !NROW(x)) return(data.table())
    x <- copy(as.data.table(x)); for (v in c("detail", "parameter", "start", "end", "check", "qids", "serial", "at", "go")) if (!v %in% names(x)) x[, (v) := ""]
    x[, id := ifelse(kind == "flag", review_hash(paste(qids)), ifelse(kind == "moved", paste("moved", serial, at), review_hash(paste(where, what))))]
    x[, decided := review_item_decision(x, done())][] })
  open_items <- reactive({ x <- rows(); if (!nrow(x)) x else x[is.na(decided)] })
  output$summary <- renderUI({ x <- rows(); n <- sum(is.na(x$decided))
    span(class = "dim", if (n) tags$b(style = "color:var(--yellow)", n, " to look at") else tags$b(style = "color:var(--green)", "\u2713 nothing waits for you"),
         if (any(!is.na(x$decided))) sprintf(" \u00b7 %d decided", sum(!is.na(x$decided)))) })
  btn <- function(i, act, lab, cls = "btn-default") tags$button(type = "button", class = paste("btn btn-sm", cls),
    onclick = sprintf("Shiny.setInputValue('%s',{id:%s,a:'%s'},{priority:'event'})", ns("act"), jsonlite::toJSON(i, auto_unbox = TRUE), act), lab)
  output$list <- renderUI({ x <- rows(); if (isTRUE(input$only) && nrow(x)) x <- x[is.na(decided)]   # an empty list has no columns
    validate(need(nrow(x) > 0, if (isTRUE(input$only)) "Nothing waits for you." else "Nothing found yet: bring in files and run once."))
    n <- nrow(x); x <- utils::head(x, 200); E <- !isTRUE(get0("VIEWER"))
    x[, `:=`(.g = needs_group_of(kind, where, if ("group" %in% names(x)) group), .f = needs_fix_of(kind, paste(what, detail), if ("fix" %in% names(x)) fix))]
    x <- x[order(.g == "Project", match(.g, unique(.g)))]                          # grouped by place, in the order found; the project last
    tagList(lapply(seq_len(nrow(x)), function(i) { r <- x[i]; dec <- !is.na(r$decided)
      head <- if (i == 1 || x$.g[i] != x$.g[i - 1]) div(class = "d-rl-group", tags$b(r$.g), span(class = "dim", sum(x$.g == r$.g)))
      when <- if (r$kind == "flag") paste(substr(r$start, 1, 16), if (!identical(r$end, r$start)) paste("to", substr(r$end, 1, 16))) else ""
      tagList(head, div(class = paste("d-chk-row", if (dec) "d-rl-done"),
        ovid_flag(NEEDS_LABELS[[r$.f]], NEEDS_LEVEL[[r$.f]]), if (r$kind != "note") span(class = "d-rl-what", switch(r$kind, flag = r$what, moved = "looks moved")),
        if (r$kind == "flag" && is.function(h$open)) tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',{id:%s,a:'open'},{priority:'event'});return false", ns("act"), jsonlite::toJSON(r$id, auto_unbox = TRUE)), title = "Look at the record over this stretch", tags$b(r$where))
        else tags$b(r$where),
        if (r$kind == "flag") span(class = "dim", paste0(r$parameter, " \u00b7 ", when, if (nzchar(r$records %||% "")) paste0(" \u00b7 ", r$records, " records")))
        else span(r$what),
        if (nzchar(r$detail %||% "")) span(class = "dim", r$detail),
        span(class = "push"),
        if (dec) span(class = "d-chk-done", tags$b(paste("\u2713", r$decided)), " ", if (E) tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',{id:%s,a:'undo'},{priority:'event'});return false", ns("act"), jsonlite::toJSON(r$id, auto_unbox = TRUE)), "Undo"))
        else if (r$kind == "flag" && E) span(class = "d-chk-act", btn(r$id, "keep", "Keep", "btn-ok"), btn(r$id, "remove", "Remove\u2026", "btn-reject"),
                                            if (!is.null(period) || !is.null(h$preset)) btn(r$id, "correct", "Correct\u2026"), btn(r$id, "ok", "Not a problem"))
        else if (r$kind == "moved" && E && is.function(split)) span(class = "d-chk-act", btn(r$id, "split", "Split here"))
        else if (r$kind == "note" && is.function(goto)) tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s',{id:%s,a:'go'},{priority:'event'});return false", ns("act"), jsonlite::toJSON(r$id, auto_unbox = TRUE)), "Open \u203a")
        else if (r$kind == "note" && nzchar(r$go %||% "")) tags$a(href = "#", `data-ovid-go` = r$go, "Open \u203a"))) }),
      if (n > 200) div(class = "dim", paste("The first 200 of", n))) })
  pend <- reactiveVal(NULL)
  decide <- function(r, decision, note, author = "") { q <- strsplit(r$qids, " ", fixed = TRUE)[[1]]; q <- q[nzchar(q)]; if (!length(q)) return()
    review_write(file(), q, data.table(id = q, decision = decision, note = paste0(note, " (", r$where, " \u00b7 ", r$parameter, " \u00b7 ", r$check, " \u00b7 ", r$start, ")"),
                                       author = author, at = format(Sys.time(), "%Y-%m-%d %H:%M"))) }
  stretch <- function(r) paste0(r$what, " at ", r$where, ", ", r$parameter, ", ", substr(r$start, 1, 16), " to ", substr(r$end, 1, 16), if (nzchar(r$detail)) paste0(": ", r$detail))
  observeEvent(input$act, { a <- input$act; r <- rows()[id == a$id]; req(nrow(r) == 1)
    if (identical(a$a, "open")) return(if (is.function(h$open)) h$open(r))
    if (identical(a$a, "go")) return(goto(r))
    if (isTRUE(get0("VIEWER"))) return()
    switch(a$a,
      keep = decide(r, REVIEW_KEEP, "kept as flagged"),
      undo = if (r$kind == "flag") review_write(file(), strsplit(r$qids, " ", fixed = TRUE)[[1]]),
      split = { split(r$serial, r$at); showNotification(paste0(r$serial, " split at ", r$at, ". Group loggers by Deployment to see its stays apart."), type = "message") },
      ok = { pend(r); showModal(modalDialog(title = "Not a problem", easyClose = TRUE, p(class = "dim", stretch(r)),
          textInput(ns("reason"), "Why it is not a problem (kept with the decision)", width = "100%"), textInput(ns("author"), "Initials", width = "100%"), uiOutput(ns("err")),
          footer = tagList(modalButton("Cancel"), actionButton(ns("ok_go"), "Mark not a problem", class = "btn-primary")))) },
      remove = , correct = {
        if (!is.null(h$preset)) { if (is.function(h$open)) h$open(r)              # the Workbench: its period tool with the stretch
          return(h$preset(list(target = r$where, parameter = trim(strsplit(r$parameter, ",", fixed = TRUE)[[1]])[1], from = r$start, to = r$end, action = if (a$a == "remove") "remove" else "offset",
            note = paste0(r$check, ": ", r$detail), lead = paste0("Flagged by ", r$check, " at ", r$where, ": ", r$records, " records. Pick the action; the stretch is the flag's."), n = Sys.time()))) }
        pend(r); showModal(modalDialog(title = if (a$a == "remove") "Remove these values" else "Correct these values", easyClose = TRUE, p(class = "dim", stretch(r)),
          if (a$a == "correct") tagList(selectInput(ns("c_action"), "Correction", PERIOD_ACTIONS[setdiff(names(PERIOD_ACTIONS), "remove")]),
                                        numericInput(ns("c_value"), "Value (offset added, or factor; drift: the change by the end)", NA, width = "100%")),
          p(class = "dim", if (a$a == "remove") "The values over this stretch are removed by the next run; the raw files are not changed." else "Kept in the corrections over a period; the next run applies it."),
          footer = tagList(modalButton("Cancel"), actionButton(ns(paste0(a$a, "_go")), if (a$a == "remove") "Remove" else "Add the correction", class = if (a$a == "remove") "btn-danger" else "btn-primary")))) }) })
  observeEvent(input$ok_go, { r <- pend(); req(r); why <- trim(input$reason %||% "")
    if (!nzchar(why)) { output$err <- renderUI(div(class = "text-danger", style = "font-size:12px;margin-top:-6px", "Say why, in a few words: it goes with the decision.")); return() }
    decide(r, "not a problem", why, trim(input$author %||% "")); removeModal(); pend(NULL) })
  observeEvent(input$remove_go, { r <- pend(); req(r); removeModal(); pend(NULL)
    ok <- if (is.function(remove)) remove(r) else if (!is.null(period)) period$append(review_period_row(r, "remove", note = paste("Needs you:", r$what))) else FALSE
    if (!isFALSE(ok)) showNotification(paste0("Removal added for ", r$where, ". The next run applies it."), type = "message") })
  observeEvent(input$correct_go, { r <- pend(); req(r, period); act <- input$c_action %||% "offset"; v <- input$c_value
    if (act %in% c("offset", "scale", "drift") && (is.null(v) || is.na(v))) return(showNotification("Give the value of the correction.", type = "warning"))
    removeModal(); pend(NULL)
    if (isTRUE(period$append(review_period_row(r, act, if (is.null(v) || is.na(v)) "" else v, paste("Needs you:", r$what)))))
      showNotification(paste0(PERIOD_ACTIONS[[act]], " added for ", r$where, ". The next run applies it."), type = "message") })
  list(items = open_items, handlers = h)
})

# ---- what shows: the table on Sites (or Stations): rename, what shows ----------------------------------------------------------
display_ui <- function(id, what = "site") { ns <- NS(id)
  tagList(p(class = "lede", sprintf("Untick Shown to leave a %s out of the strip, Home and Needs you on the Standard view. It is still processed, exported and in the reports, and ticking it brings it back.", what)),
          div(class = "btnrow", actionButton(ns("all"), "Show all"), uiOutput(ns("count"), inline = TRUE)), uiOutput(ns("tbl"))) }
display_server <- function(id, pd, names, on_change = function() NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns; tick <- reactiveVal(0)
  cur <- reactive({ tick(); if (exists("live_stamp", mode = "function")) live_stamp(display_path(pd())); d <- display_read(pd()); n <- sort(unique(names()))
    data.table::data.table(name = n, shown = !n %in% d$name[.no(d$shown)]) })
  output$count <- renderUI({ x <- cur(); span(class = "dim", sprintf("%d of %d shown", sum(x$shown), nrow(x))) })
  output$tbl <- renderUI({ x <- cur(); validate(need(nrow(x) > 0, "Nothing found yet: bring in files and run once."))
    cb <- function(k, i, on) tags$input(type = "checkbox", checked = if (on) NA, style = "accent-color:var(--acc)",
      onchange = sprintf("Shiny.setInputValue('%s',{n:%s,k:'%s',v:this.checked},{priority:'event'})", ns("set"), jsonlite::toJSON(x$name[i], auto_unbox = TRUE), k))
    tags$table(class = "table", style = "max-width:640px", tags$thead(tags$tr(tags$th("Name"), tags$th("Shown"))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(style = if (!x$shown[i]) "opacity:.6", tags$td(x$name[i]), tags$td(cb("shown", i, x$shown[i])))))) })
  observeEvent(input$set, { s <- input$set; if (identical(s$k, "shown")) display_set(pd(), s$n, shown = isTRUE(s$v))
    tick(tick() + 1); on_change() })
  observeEvent(input$all, { x <- cur(); if (nrow(x)) display_set(pd(), x$name, shown = TRUE); tick(tick() + 1); on_change() })
  reactive({ x <- cur(); x$name[x$shown] })
})

# Column headings for a table shown to people: snake_case names become words, and a unit suffix becomes the unit
# (depth_m -> "Depth (m)", gap_hours -> "Gap (h)"). A name that already has a space or a capital is left as it is.
OVID_UNIT_SUFFIX <- c(m = "m", cm = "cm", mm = "mm", km = "km", h = "h", hours = "h", min = "min", d = "days", days = "days",
                        c = "\u00b0C", degc = "\u00b0C", kpa = "kPa", pa = "Pa", pct = "%", percent = "%", mps = "m/s", wm2 = "W/m\u00b2", uscm = "\u00b5S/cm")
ovid_colnames <- function(n) {
  vapply(n, function(x) {
    if (is.na(x) || !grepl("^[a-z][a-z0-9_.]*$", x)) return(x)
    p <- strsplit(x, "[_.]")[[1]]; u <- NULL
    if (length(p) > 1 && p[length(p)] %in% names(OVID_UNIT_SUFFIX)) { u <- OVID_UNIT_SUFFIX[[p[length(p)]]]; p <- p[-length(p)] }
    p[p == "id"] <- "ID"; p[p == "ts"] <- "time"; p[p == "rel"] <- "path"
    s <- paste(p, collapse = " "); s <- paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
    if (is.null(u)) s else paste0(s, " (", u, ")") }, "", USE.NAMES = FALSE)
}
