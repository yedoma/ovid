# Ovid/report_options.R  --  How reports come out: their titles, an added line under the title, the author, the file names,
# which sections they hold, and whether a parameter is drawn on the same axis at every site or station. Part of Ovid Core
# (shared). Sourced by the processors' pipelines (their report writers read the options) and so by their apps.
#
# The options are a list (report_opts()); the app keeps them with its settings (key report_opts), so they stay between runs
# and go with a project. Titles and file names take {name} (the site or station), {from} and {to} (the first and last day)
# and {app}; blank means the app's own. Every option left as it is gives the report as before.

REPORT_OPTS <- list(title = "", subtitle = "", author = "", file = "", axis = "same", sections = NULL)
REPORT_AXIS <- c("The same scale at every site or station" = "same", "The same span, centred on each one's values" = "span", "Each its own scale" = "free")

report_opts <- function(x = NULL) { o <- REPORT_OPTS; if (is.list(x)) o[intersect(names(x), names(o))] <- x[intersect(names(x), names(o))]
  if (!o$axis %in% REPORT_AXIS) o$axis <- "same"; o }
report_fill <- function(tpl, ...) { v <- list(...); for (k in names(v)) tpl <- gsub(paste0("{", k, "}"), as.character(v[[k]] %||% ""), tpl, fixed = TRUE); trim(tpl) }
report_title <- function(o, default, ...) { t <- trim(o$title %||% ""); if (nzchar(t)) report_fill(t, ...) else default }
report_subtitle <- function(o, default, ...) { s <- trim(o$subtitle %||% ""); a <- trim(o$author %||% "")
  paste(c(default, if (nzchar(s)) report_fill(s, ...), if (nzchar(a)) paste("Prepared by", a)), collapse = "  |  ") }
report_file <- function(o, default, ...) { f <- trim(o$file %||% ""); safe_name(if (nzchar(f)) report_fill(f, ...) else default) }
report_wants <- function(o, id) is.null(o$sections) || id %in% o$sections

# Axis limits shared between sites or stations. x: one row per value, with the columns unit (site or station), key (what is
# compared: a parameter, or a depth band) and value. "same": every unit gets the lowest and highest of all; "span": each its
# own middle, with the widest range any unit has; "free": none (NULL). Returns unit, key, lo, hi.
axis_limits <- function(x, mode = "same", pad = 0.03) {
  if (is.null(x) || !nrow(x) || identical(mode, "free")) return(NULL)
  x <- as.data.table(x)[is.finite(value)]; if (!nrow(x)) return(NULL)
  r <- x[, .(lo = min(value), hi = max(value)), by = .(unit, key)]
  if (identical(mode, "same")) { a <- r[, .(lo = min(lo), hi = max(hi)), by = key]; r <- merge(r[, .(unit, key)], a, by = "key") }
  else { w <- r[, .(w = max(hi - lo)), by = key]; r <- merge(r, w, by = "key"); r[, `:=`(m = (lo + hi) / 2)][, `:=`(lo = m - w / 2, hi = m + w / 2)][, c("m", "w") := NULL] }
  r[, d := pmax(hi - lo, 1e-9) * pad][, `:=`(lo = lo - d, hi = hi + d)][, d := NULL]
  r[, .(unit, key, lo, hi)]
}
axis_of <- function(lims, unit, key) { if (is.null(lims)) return(NULL); r <- lims[lims$unit == unit & lims$key == key]; if (nrow(r)) c(r$lo[1], r$hi[1]) else NULL }

# ---- the panel on the Export page ---------------------------------------------------------------------------------------------
# sections: the parts of a report the user can leave out (named: label = id). settings_file / key: where the options are kept.
report_options_ui <- function(id, sections = NULL, extra = NULL) { ns <- NS(id)
  div(class = "panel-x", h3("Report layout"),
      p(class = "dim", "Titles and file names may use {name} (the site or station), {from} and {to} (first and last day). Leave a box blank for the app's own."),
      fluidRow(column(6, textInput(ns("title"), "Title", width = "100%", placeholder = "e.g. {name}, {from} to {to}")),
               column(6, textInput(ns("file"), "File name", width = "100%", placeholder = "e.g. {name}_report"))),
      fluidRow(column(6, textInput(ns("subtitle"), "A line under the title", width = "100%", placeholder = "e.g. Prepared for the 2026 annual review")),
               column(6, textInput(ns("author"), "Prepared by", width = "100%", placeholder = "Name or organisation"))),
      fluidRow(column(6, selectInput(ns("axis"), "Axes across sites or stations", REPORT_AXIS, width = "100%")),
               column(6, if (length(sections)) checkboxGroupInput(ns("sections"), "Sections", sections, sections, inline = TRUE))),
      extra, uiOutput(ns("names"))) }

# names_for(o): a table of what the reports chosen would be called (name, title, file), shown under the boxes.
report_options_server <- function(id, names_for = function(o) NULL, sections = NULL, settings_file = get0("SETTINGS_FILE"), key = "report_opts") moduleServer(id, function(input, output, session) {
  e <- env_with("SETTINGS", environment(report_options_server)); saved <- report_opts(if (is.null(e)) NULL else get("SETTINGS", envir = e)[[key]])
  for (k in c("title", "file", "subtitle", "author")) updateTextInput(session, k, value = saved[[k]] %||% "")
  updateSelectInput(session, "axis", selected = saved$axis)
  if (!is.null(saved$sections)) updateCheckboxGroupInput(session, "sections", selected = saved$sections)
  opts <- reactive(report_opts(list(title = input$title %||% "", file = input$file %||% "", subtitle = input$subtitle %||% "", author = input$author %||% "",
                                    axis = input$axis %||% "same", sections = if (length(sections)) input$sections %||% character() else NULL)))
  keep <- debounce(opts, 1500)
  observeEvent(keep(), ignoreInit = TRUE, { o <- keep()
    old <- if (is.null(e)) NULL else get("SETTINGS", envir = e)[[key]]; if (identical(report_opts(old), o)) return()
    # safe: the choice applies in this session; only remembering it for the next failed
    if (!is.null(settings_file)) tryCatch(settings_save(settings_file, setNames(list(o), key)), error = function(err) NULL)
    if (!is.null(e)) { s <- get("SETTINGS", envir = e); s[[key]] <- o; assign("SETTINGS", s, envir = e) } })
  # safe: no preview of the names while the options are incomplete
  output$names <- renderUI({ x <- tryCatch(names_for(opts()), error = function(err) NULL); if (is.null(x) || !nrow(x)) return(NULL)
    tagList(p(class = "dim", style = "margin:6px 0 2px", "The reports would be called:"),
            tags$table(class = "table table-sm", tags$thead(tags$tr(tags$th("Site or station"), tags$th("Title"), tags$th("File"))),
                       tags$tbody(lapply(seq_len(min(nrow(x), 6)), function(i) tags$tr(tags$td(x$name[i]), tags$td(x$title[i]), tags$td(x$file[i])))),
                       if (nrow(x) > 6) tags$tfoot(tags$tr(tags$td(colspan = 3, class = "dim", paste("and", nrow(x) - 6, "more")))))) })
  opts
})
