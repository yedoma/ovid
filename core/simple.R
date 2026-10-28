# Ovid/simple.R  --  The Simple view: the first rung of the three views (M 28 Oct). For new users: a field check of the data.
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): the apps source it, no pipeline does.
#
# Simple | Standard | Workbench are one ladder: each view does more than the one before it. Simple reads the raw folder with
# the project's saved set-up (the processes run as the project sets them; no switches for them here), shows what is there
# and whether every logger is working, lets the user fix names, places and grouping, and exports a copy or a Simple report.
# Nothing here changes a data value or a processing choice: those are Standard (choices) and Workbench (values).
#
#   simple_page(app, pal, import, view, edit, export, project, hidden, head, theme)   the whole page: a top bar with the
#                       three-view switch, four large steps (Import, View, Edit, Export) and one column under them. hidden: the
#                       Standard pages the app's Standard server needs (their inputs), kept out of sight in the "nav" tab set.
#   simple_steps_server(session, done)   marks the steps done (done(): a named logical, import/view/edit/export)
#   simple_tile(), simple_spark(), simple_hero()   the status tile of a station or site (link: a Standard page from it), its
#                       sparkline, the line at the top
#   simple_edit_ui(id), simple_edit_server(id, rows, apply, ...)   names, places and grouping (below)
#   simple_report_doc(...), simple_report_write(doc, base, formats, pal)   the Simple report (below)
# The look: larger type, one column, rounded tiles on the scheme's raised ground, plain words. Colours from the app's palette
# (ovid_tokens), so each app keeps its scheme.

OVID_SIMPLE_VERSION <- "1.0.0"
SIMPLE_STEPS <- c(import = "Import", view = "View", edit = "Edit", export = "Export")
SIMPLE_CSS <- paste(readLines(file.path(CORE_DIR, "www", "simple.css"), encoding = "UTF-8"), collapse = "\n")
SIMPLE_JS <- paste(readLines(file.path(CORE_DIR, "www", "simple.js"), encoding = "UTF-8"), collapse = "\n")

simple_css <- function(pal) { tk <- ovid_tokens(pal)
  v <- c(bar = tk$bgh, bg = tk$bg, raised = tk$bg1, line = tk$bg2, line2 = tk$bg3, fg = tk$fg, fg2 = tk$fg4, mut = tk$gray, acc = tk$acc, onacc = tk$onacc,
         ok = tk$green, warn = tk$yellow, err = tk$red, info = tk$blue, kind2 = .or(pal$purple, pal$magenta, tk$blue))
  paste0(":root{", paste0("--sp-", names(v), ":", v, ";", collapse = ""), "}\n", SIMPLE_CSS) }

simple_page <- function(app, pal, import, view, edit, export, project = NULL, hidden = list(), head = NULL, theme = NULL, actions = NULL) {
  ic <- ovid_icon(app)
  if (!is.null(ovid_font_dir())) shiny::addResourcePath("ovid-fonts", ovid_font_dir())
  page <- if (is.null(theme)) fluidPage else function(...) fluidPage(..., theme = theme)
  step <- function(k, i) tags$button(type = "button", class = "sp-step", `data-step` = k, span(class = "sp-step-n", i), span(class = "sp-step-t", SIMPLE_STEPS[[k]]))
  page(title = paste0("ovid // ", app, " · simple"),
    tags$head(if (!is.null(ic)) tags$link(rel = "icon", type = "image/png", href = ic),
              tags$style(HTML(paste0(ovid_font_css(), ovid_css(ovid_tokens(pal))))), tags$style(HTML(OVID_PARTS_CSS)),
              if (exists("OVID_FILES_CSS")) tags$style(HTML(OVID_FILES_CSS)), tags$style(HTML(simple_css(pal))),
              if (exists("TSV_JS")) ts_view_head(), if (exists("DTBL_JS")) ovid_dt_head(),
              tags$script(HTML(OVID_SHELL_JS)), tags$script(HTML(SIMPLE_JS)), head),
    tags$script(HTML("document.documentElement.classList.add('sp-root');")),
    div(class = "sp",
      div(class = "sp-top",
        div(class = "sp-brand", if (!is.null(ic)) tags$img(src = ic, alt = ""), span("ovid"), span(class = "sep", "//"), span(class = "app", app)),
        if (!is.null(project)) div(class = "sp-project", project),
        div(class = "sp-fill"), actions,
        div(class = "d-views sp-views", role = "group", `aria-label` = "View", title = "Three views of the same project: Simple, Standard, Workbench",
          span(class = "on", "Simple"), tags$a(href = "?ui=standard", class = "d-to-view", `data-ui` = "standard", "Standard"),
          tags$a(href = "?ui=workbench", class = "d-to-view", `data-ui` = "workbench", "Workbench"))),
      div(class = "sp-steps", role = "tablist", step("import", 1), step("view", 2), step("edit", 3), step("export", 4)),
      div(class = "sp-body", tabsetPanel(id = "simple_step", type = "hidden", selected = "import",
        tabPanel("Import", value = "import", import), tabPanel("View", value = "view", view),
        tabPanel("Edit", value = "edit", edit), tabPanel("Export", value = "export", export)))),
    div(class = "sp-hidden", `aria-hidden` = "true", do.call(tabsetPanel, c(list(id = "nav", type = "hidden"), hidden))),
    if (exists("run_ui", mode = "function")) run_ui())
}
# done(): which steps are done (import: a run has outputs). go: a step to open now (once).
simple_steps_server <- function(session, done = function() c(import = FALSE)) {
  observe(session$sendCustomMessage("sp-done", as.list(done())))
  invisible(list(go = function(step) updateTabsetPanel(session, "simple_step", selected = step)))
}

# ---- tiles -------------------------------------------------------------------------------------------------------------------
# A sparkline of v (numbers in time order) as an inline SVG, at most n points; NULL when there is nothing to draw.
simple_spark <- function(v, n = 120, level = "ok") {
  v <- as.numeric(v); if (sum(is.finite(v)) < 2) return(NULL)
  if (length(v) > n) { i <- round(seq(1, length(v), length.out = n)); v <- v[i] }
  r <- range(v, na.rm = TRUE); if (!is.finite(diff(r)) || diff(r) == 0) r <- r + c(-1, 1)
  x <- seq(0, 300, length.out = length(v)); y <- 46 - 42 * (v - r[1]) / diff(r)
  seg <- split(seq_along(v), cumsum(!is.finite(v))); seg <- Filter(function(k) sum(is.finite(v[k])) > 1, seg)
  pts <- vapply(seg, function(k) { k <- k[is.finite(v[k])]; paste(sprintf("%.1f,%.1f", x[k], y[k]), collapse = " ") }, "")
  HTML(sprintf('<svg class="sp-spark" viewBox="0 0 300 50" preserveAspectRatio="none" aria-hidden="true">%s</svg>',
    paste0('<polyline fill="none" class="sp-spark-', level, '" points="', pts, '"/>', collapse = "")))
}
# One station or site: its name, a state (ok, warn, error, off) with a word or two, a sparkline, label/value pairs and a note.
# The card holds a button (selects it) and, outside the button, an optional link: list(label, page) opens that page of the
# Standard view (?ui=standard&page=; extra query fields as further named elements, e.g. site), so no control sits in another.
simple_tile <- function(title, level = "ok", state = NULL, spark = NULL, facts = list(), note = NULL, on = FALSE, value = title, input = "sp_pick", link = NULL) {
  level <- if (level %in% OVID_LEVELS) level else "info"
  div(class = paste("sp-tile", paste0("sp-", level), if (on) "on"),
    tags$button(type = "button", class = "sp-tile-sel", `aria-pressed` = if (on) "true" else "false", onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", input, jsonlite::toJSON(value, auto_unbox = TRUE)),
      div(class = "sp-tile-h", tags$b(title), span(class = paste0("sp-state sp-", level), span(class = "sp-dot"), state %||% c(ok = "all good", warn = "look at", error = "stopped", off = "no data", info = "")[[level]])),
      spark, if (length(facts)) tags$dl(lapply(names(facts), function(k) tagList(tags$dt(k), tags$dd(facts[[k]])))),
      if (!is.null(note)) p(class = "sp-tile-note", note)),
    if (length(link)) tags$a(class = "sp-tile-link", href = simple_std_href(link$page, link[setdiff(names(link), c("label", "page"))]), link$label))
}
# The address of a page of the Standard view, with any other query fields (named list).
simple_std_href <- function(page, extra = list()) { q <- c(list(ui = "standard", page = page), Filter(function(v) length(v) == 1 && !is.na(v) && nzchar(v), extra))
  paste0("?", paste0(names(q), "=", vapply(q, function(v) utils::URLencode(as.character(v), reserved = TRUE), ""), collapse = "&")) }
# The line at the top of View: a mark, a sentence and the detail under it.
simple_hero <- function(level, head, sub = NULL, action = NULL)
  div(class = paste("sp-hero", paste0("sp-", level)), span(class = "sp-hero-mark", c(ok = "✓", warn = "!", error = "×", off = "–", info = "i")[[level %||% "info"]]),
      div(tags$b(head), if (!is.null(sub)) span(sub)), div(class = "sp-fill"), action)

# ---- names, places and grouping ----------------------------------------------------------------------------------------------
# The Edit step. Each group (a station or a site) is a card holding its items (loggers): drag an item onto another card, or
# pick one from Move to, to group it there; click a card to rename it and to type its place (decimal degrees, WGS 84). An item
# whose files have a long gap (group_split_suggestions, grouping.R) asks once: its whole record (the default), or from the
# first record after the gap (a split). Changes wait in a list until Save; Save hands them to the app's apply(), which writes
# its own set-up tables (the deployment sheet, or the app's list of loggers) with the version before kept in config/history
# and the change log; Undo takes the last save back. Only names, places and grouping change here, never data values.
#
#   rows():   item, label, group ("" = in none yet), first, last (text dates), lat, lon (the group's place, numbers or NA);
#             optional kind: the item's kind, shown as an accent (class sp-chip-<kind> sp-chip-alt), e.g. "baro"
#   fields:   more per group, each list(id, label, type = "text" | "readonly", get = function(group) value, link = a Standard
#             page): shown on each card; a text field is edited with the name and place (a "field" op: item = id, to = value);
#             a read-only one shows its value and, with link, "set in Standard"
#   apply(ops, who): ops is a data.table (type: rename | move | place | field, group, to, item, at, lat, lon); returns
#             list(ok = TRUE/FALSE, message = "", undo = function() or NULL)
#   gap(item): the date (text) of the first record after a long gap in the item's files, or NA
#   busy():   the set-up tables with unsaved edits in another view (OVID_PENDING, editor.R): Save waits for them
#   noun, items: the words for a group and for what it holds ("station", "loggers")
SIMPLE_OPS <- data.table(type = character(), group = character(), to = character(), item = character(), at = character(), lat = numeric(), lon = numeric())
simple_edit_ui <- function(id, noun = "station", items = "loggers") { ns <- NS(id)
  div(class = "sp-edit", `data-ns` = ns(""),
    div(class = "sp-card sp-edit-intro", tags$b("Names, places and grouping."), span(class = "sp-dim", paste0(" Drag one of the ", items, " onto a ", noun,
      ", click a ", noun, " to rename it or give its place. Data values, quality rules and corrections are not changed here."))),
    div(class = "sp-edit-cols",
      div(class = "sp-edit-main", uiOutput(ns("ask")), uiOutput(ns("groups"))),
      div(class = "sp-edit-side", uiOutput(ns("detail")), uiOutput(ns("changes")))))
}
# Planned changes applied to the rows, so the cards show them before Save.
simple_ops_preview <- function(rows, ops) {
  r <- copy(as.data.table(rows)); if (!nrow(ops)) return(r)
  for (i in seq_len(nrow(ops))) { o <- ops[i]
    if (o$type == "rename") { r[group == o$group, group := o$to] }
    else if (o$type == "move") { if (nzchar(o$at)) { k <- r[item == o$item & group == o$group]; if (nrow(k)) { k2 <- copy(k[1]); k2[, `:=`(group = o$to, first = o$at)]
        r[item == o$item & group == o$group, last := o$at]; r <- rbind(r, k2) } } else r[item == o$item & group == o$group, group := o$to] }
    else if (o$type == "place") r[group == o$group, `:=`(lat = o$lat, lon = o$lon)]
    else if (o$type == "new") r <- rbind(r, data.table(item = "", label = "", group = o$to, first = "", last = "", lat = NA_real_, lon = NA_real_), fill = TRUE) }
  r }
simple_ops_text <- function(o, noun = "station", fields = list()) switch(o$type,
  field = { f <- Filter(function(f) identical(f$id, o$item), fields); sprintf("%s: %s set to %s", o$group, if (length(f)) f[[1]]$label else o$item, if (nzchar(o$to)) o$to else "nothing") },
  rename = sprintf("“%s” renamed %s", o$group, o$to),
  move = sprintf("%s moved to %s%s", o$item, if (nzchar(o$to)) o$to else paste("no", noun), if (nzchar(o$at)) paste(" from", substr(o$at, 1, 10)) else ", whole record"),
  place = sprintf("Place of %s set to %.5f, %.5f", o$group, o$lat, o$lon),
  new = sprintf("New %s “%s”", noun, o$to), "")
simple_coord <- function(x, lim) { v <- suppressWarnings(as.numeric(gsub(",", ".", trim(as.character(x %||% ""))))); if (length(v) == 1 && is.finite(v) && abs(v) <= lim) v else NA_real_ }

simple_edit_server <- function(id, rows, apply, gap = function(item) NA_character_, busy = function() character(), who = function() "",
                               noun = "station", items = "loggers", fields = list()) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  ops <- reactiveVal(SIMPLE_OPS); sel <- reactiveVal(NULL); ask <- reactiveVal(NULL); last <- reactiveVal(NULL)
  add <- function(o) { x <- ops(); ops(rbind(x, o, fill = TRUE)) }
  # a field's value: the last change waiting for it, else the app's, under the group's name before any rename waiting
  was <- function(k) { o <- ops()[type == "rename"]; for (i in rev(seq_len(nrow(o)))) if (identical(o$to[i], k)) k <- o$group[i]; k }
  fval <- function(f, k) { o <- ops()[type == "field" & group == k & item == f$id]; if (nrow(o)) return(o$to[nrow(o)])
    v <- tryCatch(f$get(was(k)), error = function(e) ""); if (length(v) != 1 || is.na(v)) "" else as.character(v) }
  view <- reactive({ r <- rows(); if (is.null(r) || !nrow(r)) return(data.table(item = character(), label = character(), group = character(), first = character(), last = character(), lat = numeric(), lon = numeric()))
    simple_ops_preview(r, ops()) })
  groups <- reactive({ v <- view(); g <- unique(v$group); c(sort(g[nzchar(g)]), if ("" %in% g) "") })
  output$groups <- renderUI({ v <- view(); g <- groups(); s <- sel()
    if (!nrow(v)) return(div(class = "sp-card sp-dim", paste0("Nothing to group yet: import the data first (step 1).")))
    card <- function(k) { x <- v[group == k & nzchar(item)]; none <- !nzchar(k); place <- x[is.finite(lat) & is.finite(lon)][1]
      div(class = paste("sp-group", if (none) "sp-tray", if (identical(s, k) && !none) "on"), `data-group` = k,
          onclick = if (!none) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("pick"), jsonlite::toJSON(k, auto_unbox = TRUE)),
        div(class = "sp-group-h", tags$b(if (none) paste("Not in a", noun, "yet") else k), span(class = "sp-dim", nrow(x), if (nrow(x) == 1) sub("s$", "", items) else items)),
        div(class = "sp-group-place sp-mono", if (none) paste0("Drag onto a ", noun, ", or use Move to") else if (nrow(place) && is.finite(place$lat[1]) && is.finite(place$lon[1])) sprintf("%.4f, %.4f", place$lat[1], place$lon[1]) else "no place yet"),
        if (!none) lapply(fields, function(f) { v <- fval(f, k)
          div(class = "sp-group-field", span(paste0(f$label, ":")), tags$b(if (nzchar(v)) v else "—"),
              if (identical(f$type, "readonly") && length(f$link)) tags$a(href = simple_std_href(f$link), onclick = "event.stopPropagation()", "set in Standard")) }),
        div(class = "sp-chips", lapply(seq_len(nrow(x)), function(i) span(class = paste("sp-chip", if ("kind" %in% names(x) && !is.na(x$kind[i]) && nzchar(x$kind[i])) paste0("sp-chip-", x$kind[i], " sp-chip-alt")), draggable = "true", `data-item` = x$item[i], `data-from` = k,
          title = paste0(x$label[i], if (nzchar(x$first[i]) || nzchar(x$last[i])) paste0(" · ", substr(x$first[i], 1, 10), " to ", substr(x$last[i], 1, 10)) else ""), x$label[i]))),
        if (none && nrow(x)) div(class = "sp-move", selectInput(ns("move_item"), NULL, stats::setNames(x$item, x$label), width = "100%"),
          selectInput(ns("move_to"), NULL, c(stats::setNames("", paste("Move to a", noun, "…")), stats::setNames(g[nzchar(g)], g[nzchar(g)])), width = "100%"))) }
    div(class = "sp-groups", lapply(g, card),
        div(class = "sp-group sp-new", textInput(ns("new_name"), NULL, placeholder = paste("New", noun, "name")), actionButton(ns("new_add"), paste("+ New", noun), class = "sp-btn2"))) })
  # a drop or a Move to: the whole record, unless the item's files have a long gap; then ask once
  move <- function(item, from, to) { if (!nzchar(item) || identical(from, to)) return()
    g <- gap(item); if (length(g) == 1 && !is.na(g) && nzchar(g)) ask(list(item = item, from = from, to = to, at = g)) else add(data.table(type = "move", group = from, to = to, item = item, at = "")) }
  observeEvent(input$drop, { d <- input$drop; move(d$item %||% "", d$from %||% "", d$to %||% "") })
  observeEvent(input$move_to, { if (nzchar(input$move_to %||% "") && nzchar(input$move_item %||% "")) move(input$move_item, "", input$move_to) }, ignoreInit = TRUE)
  output$ask <- renderUI({ a <- ask(); if (is.null(a)) return(NULL)
    div(class = "sp-ask", tags$b(a$item), sprintf(" to %s. Its files have a long gap before %s: it may have been moved then.", if (nzchar(a$to)) a$to else paste("no", noun), substr(a$at, 1, 10)),
      div(class = "sp-row", actionButton(ns("ask_all"), "Whole record", class = "sp-btn"), actionButton(ns("ask_from"), paste("From", substr(a$at, 1, 10)), class = "sp-btn2"),
          actionButton(ns("ask_no"), "Cancel", class = "sp-btn2"))) })
  observeEvent(input$ask_all, { a <- ask(); req(a); add(data.table(type = "move", group = a$from, to = a$to, item = a$item, at = "")); ask(NULL) })
  observeEvent(input$ask_from, { a <- ask(); req(a); add(data.table(type = "move", group = a$from, to = a$to, item = a$item, at = a$at)); ask(NULL) })
  observeEvent(input$ask_no, ask(NULL))
  observeEvent(input$pick, sel(input$pick))
  observeEvent(input$new_add, { n <- trim(input$new_name %||% ""); if (!nzchar(n)) return(showNotification(paste("Type a name for the new", noun, "first."), type = "warning"))
    if (tolower(n) %in% tolower(groups())) return(showNotification(paste("There is a", noun, "of that name already."), type = "warning"))
    add(data.table(type = "new", to = n)); sel(n); updateTextInput(session, "new_name", value = "") })
  output$detail <- renderUI({ k <- sel(); v <- view(); if (is.null(k) || !k %in% v$group) return(div(class = "sp-card sp-dim", paste("Click a", noun, "to rename it or give its place.")))
    p1 <- v[group == k & !is.na(lat)][1]
    div(class = "sp-card sp-detail", div(class = "sp-h", noun, " · ", k),
      textInput(ns("name"), "Name", k, width = "100%"),
      div(class = "sp-two", textInput(ns("lat"), "Latitude", if (nrow(p1) && is.finite(p1$lat)) format(p1$lat, digits = 9) else "", width = "100%", placeholder = "45.4215"),   # no place: empty, not "NA"
                            textInput(ns("lon"), "Longitude", if (nrow(p1) && is.finite(p1$lon)) format(p1$lon, digits = 9) else "", width = "100%", placeholder = "-75.6972")),
      p(class = "sp-dim sp-small", "Decimal degrees, WGS 84: north and east positive."),
      lapply(Filter(function(f) !identical(f$type, "readonly"), fields), function(f) textInput(ns(paste0("f_", f$id)), f$label, fval(f, k), width = "100%")),
      simple_locator(v, k),
      actionButton(ns("keep"), "Keep these", class = "sp-btn2")) })
  observeEvent(input$keep, { k <- sel(); req(k); n <- trim(input$name %||% "")
    if (nzchar(n) && !identical(n, k)) { if (tolower(n) %in% tolower(setdiff(groups(), k))) return(showNotification(paste("There is a", noun, "of that name already."), type = "warning"))
      add(data.table(type = "rename", group = k, to = n)); k <- n; sel(n) }
    la <- trim(input$lat %||% ""); lo <- trim(input$lon %||% "")
    if (nzchar(la) || nzchar(lo)) { a <- simple_coord(la, 90); b <- simple_coord(lo, 180)
      if (is.na(a) || is.na(b)) return(showNotification("Latitude is a number from -90 to 90 and longitude from -180 to 180, in decimal degrees.", type = "warning"))
      p0 <- view()[group == k & !is.na(lat)][1]; if (!nrow(p0) || !isTRUE(all.equal(c(p0$lat, p0$lon), c(a, b)))) add(data.table(type = "place", group = k, lat = a, lon = b)) }
    for (f in Filter(function(f) !identical(f$type, "readonly"), fields)) { x <- trim(input[[paste0("f_", f$id)]] %||% "")
      if (!identical(x, fval(f, k))) add(data.table(type = "field", group = k, item = f$id, to = x)) } })
  output$changes <- renderUI({ o <- ops(); b <- busy(); l <- last()
    div(class = "sp-card sp-changes", div(class = "sp-h", "Changes not saved · ", nrow(o[type != "new"])),
      if (!nrow(o)) p(class = "sp-dim", "None yet.") else tags$ul(lapply(seq_len(nrow(o)), function(i) tags$li(simple_ops_text(o[i], noun, fields)))),
      if (length(b)) p(class = "sp-warn-t", "Unsaved edits to ", paste(b, collapse = ", "), " are open in the Standard view or the Workbench. Save or drop them there first; they are not overwritten."),
      if (nrow(o[type != "new"])) textInput(ns("who"), "Your initials (go in the change log)", isolate(input$who) %||% who(), width = "220px"),
      div(class = "sp-row", actionButton(ns("save"), "Save", class = "sp-btn", disabled = if (!nrow(o[type != "new"]) || length(b)) NA),
          actionButton(ns("drop_all"), "Drop the changes", class = "sp-btn2", disabled = if (!nrow(o)) NA),
          if (!is.null(l$undo)) actionButton(ns("undo"), "Undo the last save", class = "sp-btn2")),
      p(class = "sp-dim sp-small", "Saved to the set-up tables; the version before is kept in the project's history with who and when. Run Import again to see them in the data.")) })
  observeEvent(input$drop_all, ops(SIMPLE_OPS))
  observeEvent(input$save, { o <- ops()[type != "new"]; if (!nrow(o) || length(busy())) return()
    # safe: a refused save leaves the changes in the list and says why
    w <- trim(input$who %||% ""); if (!nzchar(w)) w <- who()
    r <- tryCatch(apply(o, w), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    if (isTRUE(r$ok)) { ops(SIMPLE_OPS); last(r); showNotification(r$message %||% "Saved.", type = "message", duration = 6) }
    else showNotification(paste("Not saved:", r$message %||% "the tables could not be written."), type = "error", duration = NULL) })
  observeEvent(input$undo, { l <- last(); req(is.function(l$undo))
    # safe: as above
    ok <- tryCatch({ l$undo(); TRUE }, error = function(e) { showNotification(paste("Not undone:", conditionMessage(e)), type = "error", duration = NULL); FALSE })
    if (ok) { last(NULL); showNotification("The last save was taken back.", type = "message") } })
  invisible(list(pending = reactive(nrow(ops()[type != "new"]))))
})
# Where the groups lie relative to one another (no base map: the Simple view needs no network); the picked one stands out.
simple_locator <- function(v, k) {
  p <- unique(v[!is.na(lat) & !is.na(lon) & nzchar(group), .(group, lat, lon)], by = "group"); if (!nrow(p)) return(NULL)
  xr <- range(p$lon) + c(-1, 1) * max(0.002, diff(range(p$lon)) * 0.15); yr <- range(p$lat) + c(-1, 1) * max(0.002, diff(range(p$lat)) * 0.15)
  x <- 10 + 240 * (p$lon - xr[1]) / diff(xr); y <- 110 - 100 * (p$lat - yr[1]) / diff(yr)
  dots <- paste0(sprintf('<circle cx="%.1f" cy="%.1f" r="%s" class="%s"><title>%s</title></circle>', x, y, ifelse(p$group == k, 6, 3.5), ifelse(p$group == k, "on", ""), htmltools::htmlEscape(p$group)), collapse = "")
  div(class = "sp-locator", HTML(sprintf('<svg viewBox="0 0 260 120" role="img" aria-label="Where the places lie relative to one another">%s</svg>', dots))) }

# ---- the Simple report -------------------------------------------------------------------------------------------------------
# One page, printed or as PDF: what, where and when; a short summary; four figures; one figure; how complete the record is;
# one table; the methods with their sources; and the run's record (run, versions, files, set-up, audit trail, levels). Built
# from the same blocks as every report (report.R), so the PDF is report.R's own; the HTML is its own page, light like paper.
#   figures: a named character vector (label = value); notes: the same names, the line under each (period, method, level)
#   avail:   a data.table (name, start, end: POSIXct or Date) of the spans with data, one name per line
#   record:  a named character vector (run, files, settings, audit, levels ...)
simple_report_doc <- function(title, meta = character(), summary = "", figures = character(), notes = character(), plot = NULL, plot_caption = NULL,
                              avail = NULL, table = NULL, table_caption = NULL, methods = character(), record = character(), pal = PAL) {
  av <- if (!is.null(avail) && nrow(avail)) simple_avail_plot(avail, pal) else NULL
  doc <- rpt_doc(title, paste(meta, collapse = " · "), list(
    rpt_text(summary), rpt_cards(figures),
    if (!is.null(plot)) rpt_plot(plot, w = 10, h = 3.6, caption = plot_caption, inline = TRUE),
    if (!is.null(av)) list(rpt_heading("Data availability"), rpt_plot(av, w = 10, h = 0.5 + 0.28 * length(unique(avail$name)), inline = TRUE)),
    if (!is.null(table)) list(rpt_heading(table_caption %||% "Summary"), rpt_table(table, max_rows = 40)),
    if (length(methods)) list(rpt_heading("Methods and sources"), rpt_text(paste0(seq_along(methods), ". ", methods))),
    if (length(record)) list(rpt_heading("Record"), rpt_table(data.table(item = names(record), value = unname(record)), max_rows = 20, col_width = 80L))))
  doc$style <- "simple"; doc$meta <- meta; doc$summary <- summary; doc$figures <- figures; doc$notes <- notes; doc$plot <- plot; doc$plot_caption <- plot_caption
  doc$avail <- av; doc$table <- table; doc$table_caption <- table_caption; doc$methods <- methods; doc$record <- record
  doc
}
simple_avail_plot <- function(avail, pal) {
  a <- as.data.table(avail); a[, `:=`(start = as.POSIXct(start), end = as.POSIXct(end))]; a[, name := factor(name, levels = rev(unique(name)))]
  ink <- "#1d1f1b"; acc <- simple_ink(pal)
  ggplot2::ggplot(a) + ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = name, yend = name), linewidth = 3.2, colour = acc, lineend = "butt") +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text = ggplot2::element_text(colour = ink),
                   plot.background = ggplot2::element_rect(fill = "#fbfaf7", colour = NA))
}
# The app's accent darkened until it reads on white paper (4.5:1).
simple_ink <- function(pal) { a <- .or(pal$accent, pal$acc, pal$blue, "#3b6e86"); for (i in 0:10) { x <- mix_col(a, "#000000", i / 10); if (contrast_ratio(x, "#fbfaf7") >= 4.5) return(x) }; "#1d1f1b" }
simple_report_write <- function(doc, base, formats = "html", pal = PAL) {
  out <- character()
  if ("html" %in% formats) { f <- paste0(base, ".html"); render_simple_html(doc, f, pal); out <- c(out, f) }
  if ("pdf" %in% formats)  { f <- paste0(base, ".pdf"); render_pdf(doc, f, utils::modifyList(pal, list(bg = "#fbfaf7", bg1 = "#f1efe8", bg2 = "#d9d8d0", fg = "#1d1f1b", fg4 = "#5b5f57", accent = simple_ink(pal)))); out <- c(out, f) }
  invisible(out)
}
render_simple_html <- function(doc, file, pal = PAL) {
  e <- html_esc; acc <- simple_ink(pal)
  paper <- utils::modifyList(pal, list(bg = "#fbfaf7"))
  img <- function(p, w, h) { if (is.null(p)) return(""); u <- plot_png_uri(p, w, h, paper); if (is.null(u)) "" else paste0("<img alt='figure' src='", u, "'>") }
  figs <- if (length(doc$figures)) paste0("<div class='figs'>", paste0("<div><span>", e(names(doc$figures)), "</span><b>", e(unname(doc$figures)), "</b><em>",
    e(vapply(names(doc$figures), function(k) if (k %in% names(doc$notes)) doc$notes[[k]] else "", "")), "</em></div>", collapse = ""), "</div>") else ""
  css <- sprintf("
  :root{--acc:%s} *{box-sizing:border-box} body{margin:0;background:#e9e8e3;color:#1d1f1b;font:12px/1.5 'Inter','IBM Plex Sans','Segoe UI',-apple-system,Helvetica,Arial,sans-serif}
  .paper{max-width:860px;margin:24px auto;background:#fbfaf7;padding:40px 48px 28px;box-shadow:0 1px 2px rgba(0,0,0,.12),0 8px 24px rgba(0,0,0,.08);display:grid;gap:14px}
  .top{display:flex;justify-content:space-between;border-bottom:2px solid var(--acc);padding-bottom:6px;font:10.5px 'IBM Plex Mono',ui-monospace,Menlo,monospace;color:#5b5f57} .top b{color:var(--acc)}
  h1{font-size:22px;font-weight:600;margin:0;letter-spacing:-.01em} .meta{font:10.5px 'IBM Plex Mono',ui-monospace,Menlo,monospace;color:#5b5f57}
  .sum{font-size:13px;max-width:80ch;margin:0} .h{font-size:10.5px;text-transform:uppercase;letter-spacing:.08em;font-weight:600;color:#5b5f57;margin:0 0 4px}
  .figs{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));border-top:1px solid #d9d8d0;border-bottom:1px solid #d9d8d0}
  .figs div{padding:8px 10px;display:grid;gap:1px;border-left:1px solid #e4e3dc} .figs div:first-child{border-left:0;padding-left:0}
  .figs span{color:#5b5f57;font-size:10.5px} .figs b{font:500 18px 'IBM Plex Mono',ui-monospace,Menlo,monospace;font-variant-numeric:tabular-nums} .figs em{font-style:normal;font-size:10px;color:#75796f}
  img{display:block;width:100%%;height:auto} .cap{font-size:10.5px;color:#5b5f57;margin:2px 0 0} .cap b{color:#1d1f1b}
  table{width:100%%;border-collapse:collapse;font-size:11px;font-variant-numeric:tabular-nums} th{text-align:left;font-weight:600;border-bottom:1px solid #1d1f1b;padding:3px 8px 3px 0}
  td{border-bottom:1px solid #e4e3dc;padding:3px 8px 3px 0;font-family:'IBM Plex Mono',ui-monospace,Menlo,monospace} td:first-child{font-family:inherit} th.n,td.n{text-align:right}
  .foot{display:grid;grid-template-columns:1.5fr 1fr;gap:20px;border-top:1px solid #d9d8d0;padding-top:8px;font-size:10.5px;color:#3e423b}
  .foot ol{margin:0;padding-left:16px;display:grid;gap:2px} dl{margin:0;display:grid;grid-template-columns:auto 1fr;gap:1px 10px;font:10.5px 'IBM Plex Mono',ui-monospace,Menlo,monospace} dt{color:#75796f} dd{margin:0;overflow-wrap:anywhere}
  .pg{font:9.5px 'IBM Plex Mono',ui-monospace,Menlo,monospace;color:#8a8d84}
  @media (max-width:700px){.paper{padding:24px 18px;margin:0} .foot{grid-template-columns:1fr}}
  @page{size:A4 portrait;margin:12mm} @media print{body{background:#fff} .paper{box-shadow:none;margin:0;max-width:none;padding:0}}", acc)
  tbl <- if (!is.null(doc$table) && nrow(doc$table)) { d <- as.data.table(doc$table); num <- vapply(d, is.numeric, NA)
    paste0("<div><p class='h'>", e(doc$table_caption %||% "Summary"), "</p><table><tr>", paste0("<th", ifelse(num, " class='n'", ""), ">", e(names(d)), "</th>", collapse = ""), "</tr>",
           paste(apply(matrix(vapply(names(d), function(n) e(rpt_cell(d[[n]], n)), character(nrow(d))), nrow = nrow(d)), 1, function(r) paste0("<tr>", paste0("<td", ifelse(num, " class='n'", ""), ">", r, "</td>", collapse = ""), "</tr>")), collapse = ""),
           "</table></div>") } else ""
  html <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>", e(doc$title), "</title><style>",
    ovid_font_css(embed = c("400", "600")), css, "</style></head><body><div class='paper'>",
    "<div class='top'><span><b>ovid ", e(rpt_app()), "</b> · Simple report</span><span>", e(doc$record[["run"]] %||% ""), "</span></div>",
    "<h1>", e(doc$title), "</h1>", if (length(doc$meta)) paste0("<div class='meta'>", paste0("<span>", e(doc$meta), "</span>", collapse = " · "), "</div>"),
    if (nzchar(doc$summary)) paste0("<p class='sum'>", e(doc$summary), "</p>"), figs,
    if (!is.null(doc$plot)) paste0("<div>", img(doc$plot, 10, 3.6), if (!is.null(doc$plot_caption)) paste0("<p class='cap'><b>Figure 1.</b> ", e(doc$plot_caption), "</p>"), "</div>"),
    if (!is.null(doc$avail)) paste0("<div><p class='h'>Data availability</p>", img(doc$avail, 10, 0.5 + 0.28 * max(1, length(levels(doc$avail$data$name)))), "</div>"),
    tbl,
    "<div class='foot'><div><p class='h'>Methods and sources</p>", if (length(doc$methods)) paste0("<ol>", paste0("<li>", e(doc$methods), "</li>", collapse = ""), "</ol>") else "<p>None beyond the project's set-up.</p>",
    "</div><div><p class='h'>Record</p><dl>", paste0("<dt>", e(names(doc$record)), "</dt><dd>", e(unname(doc$record)), "</dd>", collapse = ""), "</dl></div></div>",
    "<div class='pg'>", e(paste0("Made ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by ovid // ", rpt_app(), " ", APP_VERSION, " (core ", CORE_VERSION, "), Simple view.")), "</div>",
    "</div></body></html>")
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(file)
}
