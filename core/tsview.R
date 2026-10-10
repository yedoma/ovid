# ovid core: the Standard view's time-series plot.
#
# A plot that opens on 3 months of half-hourly records (around the first thing to look at, else the latest) and scrolls
# through the rest: drag the time axis or the strip of the whole record under the plot (or hold Space and drag), the wheel
# (a little per notch, about the pointer; sideways or shift-wheel scrolls), the < > buttons and a window menu from 1 week
# to the whole record. Back (Backspace) returns to the previous view; double-click shows the whole record. The view bar above a page's
# plots (ts_viewbar) sets for all of them: half-hourly, as recorded, hourly, daily, monthly or annual (means, or totals for
# a series marked sum; hollow where a period is short of the completeness rule), the smoothing spline (display only, on by
# default), the ghost of the raw data and the colour of filled data.
#
# Selecting is the same on every ovid plot (_hub/Plot_table_standards.md A11): drag on the plot selects a stretch, a click
# the nearest record, Shift adds to the selection, Esc clears. A bar above the selection gives its size and Zoom to this,
# Look closer and Open in Workbench; each series's records, missing, mean (or total), min, max and change inside it are
# listed under the plot. Keys: arrows move the selection by a record (Shift extends it), + and - zoom, Enter looks closer.
# The page has one cursor: the moment under the pointer shows on every plot. The tooltip gives the time with its zone, each
# value with its unit, the record's status and (when sent) the logger and file it came from. Events (field visits,
# downloads, logger swaps) are dashed lines with a flag. Save image saves the view at 85 mm, 180 mm (300 dpi) or a 16:9
# slide in the light print scheme with a caption; Save data saves the records behind it as CSV (standards A10).
# Look closer (an element with data-tsv-close, or ts_view_close() from the server) zooms a plot to two days around a
# stretch, lists its records (raw and now) and links to the Workbench at that time (?ui=workbench&<key>=<value>&t=...).
#
#   ts_view(id)                          the plot (put it where it should show)
#   ts_viewbar(sum = FALSE, precip)      the view bar for every plot on the page
#   ts_view_send(session, id, t, series, step, zone, issues, wb, bars, focus, same_scale, events, sources, title, site, level, caption, file)
#                                        its data: t are the record times (POSIXct, logger time), series a list of
#                                        list(name, key = name (the Workbench's name for it), unit, v, raw = NULL, fill = NULL, sum = FALSE, digits = 2,
#                                        marks = list(t, v, label): points drawn as diamonds, such as manual levels;
#                                        ref = list(name, v): a second series drawn as its own thin line in the panel, such as the neighbour mean);
#                                        wb = list(key, value, params) for the Workbench link, with from (times) and
#                                        values when the record is made of parts (the part that holds the time is used);
#                                        bars = list(t, v, name, unit, file) drawn as columns under the plot
#                                        focus: a time to open on; same_scale = TRUE puts every panel on one vertical scale
#                                        A series may also give panel (series naming the same panel share it, each in
#                                        its colour col, under the panel's label plab), dots = list(list(t, v, kind =
#                                        "removed" or "flagged", label)): records drawn as grey crosses or gold rings, and
#                                        marks$text: a label for each mark. spans = data frame t0, t1, label: shaded in
#                                        grey (out of the water, removed by hand). height: each panel's height.
#   input$<id>_sel                       the selected stretch, list(t0, t1, key, lo, hi) in seconds, or NULL when there is
#                                        none; key, lo and hi are the value band's (dragged on a panel's value axis), else NULL
#   ts_view_close(session, id, t0, t1)   Look closer from the server
#
# Edits from the plot (Explore in WLR and GT): actions = list(list(id, label, kind = "bad" / "warn" / NULL, band = TRUE (only
# with a value band) / FALSE (only without) / NULL (either), label_band, title)) adds buttons to the bar above the selection,
# with a reason (needed unless ask$reason is FALSE), initials, and a choice when ask = list(choices, choices_label). A click
# sets input$<id>_act = list(act, t0, t1, key, lo, hi, n, reason, author, cause). After writing it, ts_view_done(session, id,
# msg) clears the selection and says what was done, with Undo while the set-up table can take it back (core editor.R);
# ts_view_undo(input, session, id) answers that Undo (input$<id>_undo).
#
# Raw data are never changed here; the spline, means and totals are drawn only.

OVID_TSVIEW_VERSION <- "1.5.0"
TSV_MAX_POINTS <- 250000                      # more records than this per series are sent as half-hourly means

ts_view <- function(id, height = 200, class = NULL)
  div(id = id, class = paste("tsv", class), `data-h` = height, div(class = "tsv-wait dim", "Loading the plot\u2026"))

ts_viewbar <- function(precip = FALSE, note = NULL)
  div(class = "tsv-bar",
    span(class = "dim", "View"),
    div(class = "tsv-seg", lapply(list(c("half", "half-hourly"), c("raw", "as recorded"), c("hourly", "hourly"), c("daily", "daily"),
                                       c("monthly", "monthly"), c("annual", "annual")),
      function(a) tags$button(type = "button", class = if (a[1] == "half") "on", `data-tsv-agg` = a[1], a[2]))),
    tags$label(class = "tsv-sw", title = "Draw the line as a smooth spline through the records (display only; the data are not changed)",
      tags$input(type = "checkbox", `data-tsv-opt` = "spline", checked = NA), span("Smoothing")),
    tags$label(class = "tsv-sw", title = "Show the raw data faintly behind, where a correction or removal changed them",
      tags$input(type = "checkbox", `data-tsv-opt` = "ghost", checked = NA), span("Ghost")),
    tags$label(class = "tsv-sw", title = "The colour of filled (interpolated) records",
      tags$input(type = "color", `data-tsv-opt` = "fill", value = "#5fb3b3"), span("Filled")),
    if (precip) tags$label(class = "tsv-sw", title = "Precipitation as bars under each plot",
      tags$input(type = "checkbox", `data-tsv-opt` = "bars", checked = NA), span("Precipitation")),
    span(class = "dim tsv-agnote", note))

# numbers for the browser: NA as null
.tsv_num <- function(x, digits = 6) { x <- as.numeric(x); x <- signif(x, digits); x[!is.finite(x)] <- NA; x }

# half-hourly means of v at t (seconds), for records too many to send; sum = TRUE gives totals
.tsv_bin <- function(t, v, sum = FALSE, w = 1800) {
  k <- floor(t / w) * w; d <- data.table::data.table(k = k, v = v)
  d <- d[, .(v = if (all(is.na(v))) NA_real_ else if (sum) base::sum(v, na.rm = TRUE) else mean(v, na.rm = TRUE)), by = k]
  list(t = d$k, v = d$v) }

# events: field visits, downloads, logger swaps, deployment starts and ends, as a data frame t (POSIXct or seconds), label,
# note; drawn as dashed lines with a flag (plot standards A11). sources: where each stretch came from, as from (time) and
# label ("Logger A, download_2024-06-22.csv"); shown in the tooltip. title, site, level, caption, file: for Save image and
# Save data (the title over the saved image, the site and processing level in its caption, a method line, the file name).
ts_view_send <- function(session, id, t, series, step = NULL, zone = "", issues = NULL, wb = NULL, bars = NULL, focus = NULL, same_scale = FALSE,
                         events = NULL, sources = NULL, title = NULL, site = NULL, level = NULL, caption = NULL, file = NULL,
                         spans = NULL, actions = NULL, ask = NULL, height = NULL) {
  tt <- as.numeric(t); o <- order(tt); tt <- tt[o]
  if (is.null(step)) step <- if (length(tt) > 1) stats::median(diff(tt), na.rm = TRUE) else 1800
  binned <- length(tt) > TSV_MAX_POINTS && step < 1800
  ser <- lapply(series, function(s) {
    v <- as.numeric(s$v)[o]; raw <- if (!is.null(s$raw)) as.numeric(s$raw)[o]; fill <- if (!is.null(s$fill)) as.integer(as.logical(s$fill)[o])
    ref <- if (!is.null(s$ref$v)) as.numeric(s$ref$v)[o]                     # a second series drawn as its own thin line in the same panel
    tv <- tt
    if (binned) { b <- .tsv_bin(tt, v, isTRUE(s$sum)); tv <- b$t; v <- b$v
      if (!is.null(raw)) raw <- .tsv_bin(tt, raw, isTRUE(s$sum))$v; if (!is.null(ref)) ref <- .tsv_bin(tt, ref, FALSE)$v; if (!is.null(fill)) fill <- as.integer(.tsv_bin(tt, fill, TRUE)$v > 0) }
    changed <- !is.null(raw) && any(xor(is.na(raw), is.na(v)) | (!is.na(raw) & !is.na(v) & abs(raw - v) > 1e-9))
    list(name = s$name, key = s$key %||% s$name, unit = s$unit %||% "", t = tv, v = .tsv_num(v), raw = if (changed) .tsv_num(raw), fill = if (!is.null(fill) && any(fill > 0, na.rm = TRUE)) fill,
         sum = isTRUE(s$sum), digits = s$digits %||% 2, ref = if (!is.null(ref) && any(!is.na(ref))) .tsv_num(ref), refname = s$ref$name %||% "",
         marks = if (length(s$marks$t)) list(t = I(as.numeric(s$marks$t)), v = I(.tsv_num(s$marks$v)), label = s$marks$label %||% "", text = if (!is.null(s$marks$text)) I(as.character(s$marks$text))),
         panel = s$panel, plab = s$plab, col = s$col,
         dots = lapply(Filter(function(x) length(x$t) > 0, s$dots %||% list()), function(x) { o2 <- order(as.numeric(x$t))
           list(t = I(as.numeric(x$t)[o2]), v = I(.tsv_num(x$v)[o2]), kind = x$kind %||% "removed", label = x$label %||% "") })) })
  iss <- if (!is.null(issues) && NROW(issues)) lapply(seq_len(NROW(issues)), function(i)
    list(t0 = as.numeric(issues$t0[i]), t1 = as.numeric(issues$t1[i]), label = as.character(issues$label[i] %||% ""), level = as.character(issues$level[i] %||% "warn")))
  bb <- if (!is.null(bars)) list(name = bars$name %||% "Precipitation", unit = bars$unit %||% "mm", t = as.numeric(bars$t), v = .tsv_num(bars$v), file = bars$file %||% "")
  ev <- if (NROW(events)) lapply(seq_len(NROW(events)), function(i) list(t = as.numeric(events$t[i]), label = as.character(events$label[i]), note = as.character(events$note[i] %||% "")))
  sp <- if (NROW(spans)) lapply(seq_len(NROW(spans)), function(i) list(t0 = as.numeric(spans$t0[i]), t1 = as.numeric(spans$t1[i]), label = as.character(spans$label[i] %||% "")))
  src <- if (NROW(sources)) { o2 <- order(as.numeric(sources$from)); lapply(o2, function(i) list(from = as.numeric(sources$from[i]), label = as.character(sources$label[i]))) }
  session$sendCustomMessage("tsv", list(id = id, step = if (binned) 1800 else step, binned = binned, zone = zone, series = ser,
                                        issues = iss, wb = wb, bars = bb, focus = if (!is.null(focus)) as.numeric(focus), same = isTRUE(same_scale),
                                        events = ev, sources = src, title = title, site = site, level = level, caption = caption, file = file,
                                        spans = sp, actions = actions, ask = ask, height = height))
}

# After an action from the plot was written: clear the selection and say so, with Undo when the set-up table store kept the
# version before (session$userData$ovid_undo, core editor.R) in the last few seconds
ts_view_done <- function(session, id, msg) { u <- session$userData$ovid_undo
  session$sendCustomMessage("tsv-done", list(id = id, msg = msg, undo = !is.null(u) && difftime(Sys.time(), u$at, units = "secs") < 5)) }
ts_view_undo <- function(input, session, id) observeEvent(input[[paste0(id, "_undo")]], { u <- session$userData$ovid_undo; session$userData$ovid_undo <- NULL
  if (!is.null(u) && isTRUE(u$fn())) showNotification(paste0("Undone: ", u$file, " is as it was before. Run to apply."), type = "message")
  else showNotification("Nothing to undo.", type = "message") })

ts_view_close <- function(session, id, t0, t1 = t0) session$sendCustomMessage("tsv-close", list(id = id, t0 = as.numeric(t0), t1 = as.numeric(t1)))

# Look closer as a link (for a problem row): opens the plot id at that stretch
ts_closer <- function(id, t0, t1 = t0, label = "Look closer \u203a", title = "Zoom to two days around this, list its records, and open the same moment in the Workbench")
  tags$a(href = "#", class = "tsv-closer", `data-tsv-close` = id, `data-t0` = as.numeric(t0), `data-t1` = as.numeric(t1), title = title, label)

ts_view_head <- function() tags$head(tags$style(HTML(TSV_CSS)), tags$script(HTML(TSV_JS)))

TSV_CSS <- paste(readLines(file.path(CORE_DIR, "www", "tsview.css"), encoding = "UTF-8"), collapse = "\n")

TSV_JS <- paste(readLines(file.path(CORE_DIR, "www", "tsview.js"), encoding = "UTF-8"), collapse = "\n")
