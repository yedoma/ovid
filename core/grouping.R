# Ovid/grouping.R  --  How the logger list is grouped: by serial number (the default), deployment, download or raw file.
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): the apps source it, no pipeline does.
#
# Display and selection only: the choice changes how the list of loggers, stations or sites is cut, never the processing
# or the outputs. A row opens that logger's record over its period.
#   Serial number  one row per logger serial, all its files and years together
#   Deployment     a row per stay of a logger: the deployment rows (metadata, deployment sheet) where there are any,
#                  otherwise the splits accepted in Needs you (rows of the deployment sheet)
#   Download       a row per download: the files of one serial that end on the same day (one trip to the logger)
#   Raw file       a row per file
# A long gap in a logger's record looks like a move: group_split_suggestions() lists each one for Needs you ("Looks like it
# was moved. Split here?"); accepting writes the split into the deployment sheet (core deployments.R dep_split(): a row
# that starts there), where it can be seen and changed like any deployment. Splits an older version kept in
# config/logger_splits.csv are moved into the sheet once (group_split_fold()). The choice of grouping is kept per project
# in config/view.json.
#
#   group_by_ui(id, label), group_by_server(id, files, project_dir, deployments, open, empty)   the control and the list
#   group_index(files, by, deployments, splits)                                  the rows (for tests and other pages)

GROUP_BY <- c("Serial number" = "serial", "Deployment" = "deployment", "Download" = "download", "Raw file" = "file")
GROUP_SPLIT_FILE <- "logger_splits.csv"
GROUP_GAP_DAYS <- 14                                             # a gap this long between two files of a serial looks like a move

# files: serial, file, first, last (POSIXct or text). deployments: serial, from, to and a name (site or station; optional).
# splits: serial, at (a time: the logger was moved then). Returns key, label, serial, first, last, files, n.
# The earliest and latest time of a group; NA (not -Inf/Inf with a warning) when it has none.
group_first <- function(x) if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = TRUE)
group_last  <- function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
group_index <- function(files, by = "serial", deployments = NULL, splits = NULL) {
  empty <- data.table(label = character(), serial = character(), first = as.POSIXct(character(), tz = "UTC"),
                      last = as.POSIXct(character(), tz = "UTC"), files = character(), n = integer())
  set(empty, j = "key", value = character()); setcolorder(empty, "key")      # not data.table(key = ...): that argument sets the sort key
  if (is.null(files) || !nrow(files)) return(empty)
  f <- data.table(serial = trim(as.character(files$serial)), file = as.character(files$file),
                  first = parse_date_cell(files$first), last = parse_date_cell(files$last, end = TRUE))
  f <- f[nzchar(serial) & !is.na(serial)]; if (!nrow(f)) return(empty)
  setorder(f, serial, first)
  fmt <- function(t) ifelse(is.na(t), "?", format(t, "%Y-%m-%d", tz = "UTC"))
  out <- if (by == "file") f[, .(key = paste0("file:", file), label = paste0(serial, " \u00b7 ", basename(file)), serial, first, last, files = file, n = 1L)]
  else if (by == "download") { f[, day := fmt(last)]
    f[, .(key = paste0("dl:", serial, ":", day[1]), label = paste0(serial, " \u00b7 downloaded ", day[1]), first = group_first(first), last = group_last(last),
          files = paste(basename(file), collapse = "; "), n = .N), by = .(serial, day)][, day := NULL][] }
  else if (by == "deployment") {
    sg <- group_segments(f, deployments, splits); nm <- attr(sg, "names_of"); f[, seg := as.integer(sg)]
    f[, .(key = paste0("dep:", serial, ":", seg), first = group_first(first), last = group_last(last), files = paste(basename(file), collapse = "; "), n = .N,
          name = nm[[paste0(serial, ":", seg)]] %||% ""), by = .(serial, seg)][,
      label := paste0(serial, ifelse(nzchar(name), paste0(" \u00b7 ", name), ""), " \u00b7 ", fmt(first), " \u2192 ", fmt(last))][, c("seg", "name") := NULL][] }
  else f[, .(key = paste0("serial:", serial[1]), label = serial[1], first = group_first(first), last = group_last(last), files = paste(basename(file), collapse = "; "), n = .N), by = serial]
  setcolorder(out, names(empty)); out[order(serial, first)]
}

# Which stay (segment) of its serial each file belongs to: by the deployment rows when the serial has any (the row whose
# period holds the file's first record), else cut at the accepted splits. names_of: "serial:segment" -> the stay's name.
group_segments <- function(f, deployments = NULL, splits = NULL) {
  seg <- integer(nrow(f)); nm <- list()
  dep <- if (!is.null(deployments) && nrow(deployments)) data.table(serial = trim(as.character(deployments$serial)), from = parse_date_cell(deployments$from),
           to = parse_date_cell(deployments$to, end = TRUE), name = trim(as.character(deployments$name %||% rep("", nrow(deployments))))) else NULL
  sp <- if (!is.null(splits) && nrow(splits)) data.table(serial = trim(as.character(splits$serial)), at = parse_date_cell(splits$at)) else NULL
  for (s in unique(f$serial)) { i <- which(f$serial == s)
    d <- if (!is.null(dep)) dep[serial == s][order(from)] else NULL
    if (!is.null(d) && nrow(d)) { k <- vapply(i, function(j) { t <- f$first[j]; h <- which((is.na(d$from) | t >= d$from - 86400) & (is.na(d$to) | t <= d$to)); if (length(h)) h[1] else 0L }, 0L)
      seg[i] <- k; for (r in unique(k)) nm[[paste0(s, ":", r)]] <- if (r > 0) d$name[r] else "outside the deployments" }
    else { cuts <- if (!is.null(sp)) sort(sp[serial == s & !is.na(at)]$at) else NULL
      seg[i] <- if (length(cuts)) findInterval(as.numeric(f$first[i]), as.numeric(cuts)) + 1L else 1L } }
  attr(seg, "names_of") <- nm; seg
}

# Long gaps between the files of a serial: where a logger looks moved. Returns serial, at (the first record after the gap),
# from, to (the gap) and a sentence for Needs you; splits already accepted are left out.
group_split_suggestions <- function(files, splits = NULL, gap_days = GROUP_GAP_DAYS) {
  if (is.null(files) || !nrow(files)) return(data.table(serial = character(), at = character(), from = character(), to = character(), what = character()))
  f <- data.table(serial = trim(as.character(files$serial)), first = parse_date_cell(files$first), last = parse_date_cell(files$last, end = TRUE))[!is.na(first) & !is.na(last)]
  setorder(f, serial, first)
  g <- f[, { e <- cummax(as.numeric(last)); gap <- c(NA, as.numeric(first)[-1] - e[-.N]); k <- which(gap > gap_days * 86400)
    .(at = first[k], from = as.POSIXct(e[k - 1], origin = "1970-01-01", tz = "UTC"), to = first[k]) }, by = serial]
  if (!is.null(splits) && nrow(splits)) { done <- paste(trim(splits$serial), format(parse_date_cell(splits$at), "%Y-%m-%d"))
    g <- g[!paste(serial, format(at, "%Y-%m-%d")) %in% done] }
  fmt <- function(t) format(t, "%Y-%m-%d", tz = "UTC")
  g[, .(serial, at = format(at, "%Y-%m-%d %H:%M", tz = "UTC"), from = fmt(from), to = fmt(to),
        what = paste0("Looks like it was moved: no records from ", fmt(from), " to ", fmt(to), " (", round((as.numeric(to) - as.numeric(from)) / 86400), " days). Split here?"))]
}
# A jump in level across a file boundary or a gap also looks like a move (the logger hung at another depth, a sensor moved):
# level(serial) gives the serial's record (t, v: a level such as water above the sensor). The median of the two days
# after a boundary is compared with the two days before; a step larger than jump_min and than 8 times the record's usual
# change between readings (median absolute difference) is suggested. Returns the same columns as group_split_suggestions().
group_level_jumps <- function(files, level, jump_min = 0, unit = "", splits = NULL) {
  out <- data.table(serial = character(), at = character(), from = character(), to = character(), what = character())
  if (is.null(files) || !nrow(files) || !is.function(level)) return(out)
  f <- data.table(serial = trim(as.character(files$serial)), first = parse_date_cell(files$first))[!is.na(first)]
  res <- list()
  # safe: a serial whose records cannot be read gets no split suggestion
  for (s in unique(f$serial)) { d <- tryCatch(level(s), error = function(e) NULL); if (is.null(d) || nrow(d) < 20) next
    d <- data.table(t = as.numeric(d$t), v = as.numeric(d$v))[is.finite(t) & is.finite(v)][order(t)]; if (nrow(d) < 20) next
    sc <- stats::median(abs(diff(d$v)), na.rm = TRUE); thr <- max(jump_min, 8 * sc)
    for (tb in sort(unique(as.numeric(f[serial == s]$first)))[-1]) {
      b4 <- d[t < tb & t >= tb - 2 * 86400]$v; af <- d[t >= tb & t < tb + 2 * 86400]$v; if (length(b4) < 6 || length(af) < 6) next
      st <- stats::median(af) - stats::median(b4); if (!is.finite(st) || abs(st) <= thr) next
      at <- as.POSIXct(tb, origin = "1970-01-01", tz = "UTC")
      res[[length(res) + 1]] <- data.table(serial = s, at = format(at, "%Y-%m-%d %H:%M", tz = "UTC"), from = format(at, "%Y-%m-%d", tz = "UTC"), to = format(at, "%Y-%m-%d", tz = "UTC"),
        what = paste0("Looks like it was moved: the level ", if (st > 0) "rose" else "dropped", " by ", signif(abs(st), 3), if (nzchar(unit)) paste0(" ", unit), " at ", format(at, "%Y-%m-%d", tz = "UTC"), " where a new file starts. Split here?")) } }
  x <- if (length(res)) rbindlist(res) else out
  if (nrow(x) && !is.null(splits) && nrow(splits)) x <- x[!paste(serial, substr(at, 1, 10)) %in% paste(trim(splits$serial), format(parse_date_cell(splits$at), "%Y-%m-%d"))]
  x }
group_split_write <- function(project_dir, serial, at, note = "") {
  if (!exists("dep_split", mode = "function")) return(invisible(FALSE))              # an app with no deployment sheet
  pl <- get0("dep_split_place", mode = "function")                                 # an app's own place for a logger the sheet does not list yet
  dep_split(project_dir, serial, at, paste(c(if (nzchar(note)) note, "split here: looks moved; check the station"), collapse = ": "), who = "Needs you",
            # safe: where a logger was placed is extra detail in the list
            place = if (is.function(pl)) tryCatch(pl(project_dir, serial), error = function(e) list()) else list())
}
# The splits: the serials with more than one row in the deployment sheet, at the start of each row after the first.
group_splits_read <- function(project_dir) { if (!exists("dep_sheet_raw", mode = "function") || !length(project_dir) || !nzchar(project_dir %||% "")) return(NULL)
  # safe: the deployment sheet's own page reports a read problem
  d <- tryCatch(dep_sheet_raw(project_dir), error = function(e) NULL); x <- dep_splits(d); if (nrow(x)) x else NULL }
# Splits an older version kept in config/logger_splits.csv (serial, at), into the sheet once (core config.R config_migrate()).
group_split_fold <- function() list(from = GROUP_SPLIT_FILE, into = "deployment_sheet.csv", fold = function(old, cur, project_dir = NULL) {
  d <- if (is.null(cur)) empty_cfg(DEPLOY_COLS) else { x <- copy(as.data.table(cur)); setnames(x, dep_names(names(x))); x }
  pl <- get0("dep_split_place", mode = "function")
  for (i in seq_len(nrow(old))) d <- dep_split_rows(d, old$serial[i], old$at[i], paste(c(if (nzchar(old$note[i] %||% "")) old$note[i], "split here: looks moved; check the station"), collapse = ": "),
                                                    # safe: as for line 107
                                                    if (is.function(pl) && length(project_dir)) tryCatch(pl(project_dir, old$serial[i]), error = function(e) list()) else list())
  list(rows = d, log = NULL) })

# The choice per project, in config/view.json.
group_by_read <- function(project_dir) { p <- file.path(path.expand(project_dir %||% ""), "config", "view.json")
  # safe: no saved grouping means the default one
  x <- if (file.exists(p)) tryCatch(jsonlite::fromJSON(p), error = function(e) list()) else list()
  if (isTRUE(x$group_by %in% GROUP_BY)) x$group_by else "serial" }
group_by_write <- function(project_dir, by) { p <- file.path(path.expand(project_dir), "config", "view.json"); if (!dir.exists(dirname(p))) return(invisible(FALSE))
  # safe: a damaged choices file is replaced
  x <- if (file.exists(p)) tryCatch(jsonlite::fromJSON(p), error = function(e) list()) else list(); x$group_by <- by
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE), p); invisible(TRUE) }

# ---- the control and the list --------------------------------------------------------------------------------------------------
# label: the control's name (what the rows are: "Group flights by" in RS).
group_by_ui <- function(id, wb = FALSE, label = "Group loggers by") { ns <- NS(id)
  div(class = if (wb) "wb-groupby" else "d-groupby", tags$style(HTML(GROUP_CSS)),
    tags$script(HTML("if (window.Shiny && !window.ovidGoUrl) { window.ovidGoUrl = true; Shiny.addCustomMessageHandler('ovid-go-url', function(u){ location.href = u; }); }")),
    div(class = "gb-head", span(class = if (wb) "wb-eb" else "dim", label),
        selectInput(ns("by"), NULL, GROUP_BY, width = "150px")),
    uiOutput(ns("list"))) }
# files(): the per-file index (serial, file, first, last); deployments(): serial, from, to, name (or NULL); open(row): what a
# click on a row does (the app shows that logger's record over the row's period); empty: what the list says with no rows.
# Returns the suggestions (for Needs you).
group_by_server <- function(id, files, project_dir, deployments = function() NULL, open = function(row) NULL, level = NULL, jump_min = 0, unit = "",
                            empty = "No loggers yet: run once.") moduleServer(id, function(input, output, session) {
  tick <- reactiveVal(0)
  observeEvent(project_dir(), updateSelectInput(session, "by", selected = group_by_read(project_dir())))
  # safe: the grouping applies in this session; only remembering it failed
  observeEvent(input$by, { pd <- project_dir(); if (!identical(input$by, group_by_read(pd))) try(group_by_write(pd, input$by), silent = TRUE) }, ignoreInit = TRUE)
  splits <- reactive({ tick(); live_stamp(file.path(path.expand(project_dir()), "config", "deployment_sheet.csv")); group_splits_read(project_dir()) })
  # safe: nothing to group yet
  rows <- reactive(group_index(tryCatch(files(), error = function(e) NULL), input$by %||% "serial", tryCatch(deployments(), error = function(e) NULL), splits()))
  output$list <- renderUI({ x <- rows(); if (!nrow(x)) return(div(class = "dim", style = "font-size:12px;padding:4px 0", empty))
    fmt <- function(t) ifelse(is.na(t), "", format(t, "%Y-%m-%d", tz = "UTC"))
    div(class = "gb-list", lapply(seq_len(min(nrow(x), 400)), function(i) tags$a(href = "#", class = "gb-row", title = paste0(x$n[i], " file", if (x$n[i] > 1) "s", ": ", x$files[i]),
      onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});return false", session$ns("pick"), x$key[i]),
      span(class = "l", x$label[i]), span(class = "n", paste0(fmt(x$first[i]), " \u2013 ", fmt(x$last[i]))))),
      if (nrow(x) > 400) div(class = "dim", paste("and", nrow(x) - 400, "more"))) })
  observeEvent(input$pick, { r <- rows()[key == input$pick]; if (nrow(r)) open(r[1]) })
  # safe: nothing to suggest yet
  sugg <- reactive({ f <- tryCatch(files(), error = function(e) NULL); g <- group_split_suggestions(f, splits())
    j <- if (is.function(level)) group_level_jumps(f, level, jump_min, unit, splits()) else NULL
    x <- rbind(g, j, fill = TRUE); if (nrow(x)) unique(x, by = c("serial", "at")) else x })
  list(rows = rows, suggestions = sugg, accept = function(serial, at) { group_split_write(project_dir(), serial, at, "accepted in Needs you"); tick(tick() + 1); live_touch() })
})
GROUP_CSS <- paste(readLines(file.path(CORE_DIR, "www", "grouping.css"), encoding = "UTF-8"), collapse = "\n")

# a, or b where a is blank (each element)
or_blank <- function(a, b) { a <- as.character(a); b <- rep_len(as.character(b), length(a)); ifelse(is.na(a) | !nzchar(trim(a)), b, a) }
