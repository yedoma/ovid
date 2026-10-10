# Ovid/table.R  --  The core table: ovid_dt(), the one table wrapper every app uses (plot and table standards, A9).
# It replaces the apps' own copies (CS gb_dt, WLR nord_dt, GT mi_dt). What it keeps from them: first column frozen, a body of
# fixed height, column widths that never shrink below their first width, the Filter box, long text wrapped, times as text.
# What it adds (www/table.js, www/table.css): a sort and filter button on each column instead of sorting on a header click,
# column widths dragged from the header edge, a Columns panel, widths and hidden columns kept per table, value / block / row
# selection with count, mean, min, max and sum, Copy, and a tint on values outside a quality limit.
# Long tables (a whole archive) use ovid_long_table() (longtable.R) instead. Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): each app.R sources it, no pipeline does.

DTBL_CSS <- paste(readLines(file.path(CORE_DIR, "www", "table.css"), encoding = "UTF-8"), collapse = "\n")
DTBL_JS <- paste(readLines(file.path(CORE_DIR, "www", "table.js"), encoding = "UTF-8"), collapse = "\n")
ovid_dt_head <- function() tagList(tags$style(HTML(DTBL_CSS)), tags$script(HTML(DTBL_JS)))

# Column widths never shrink below what they were when first drawn (so a filter does not make the table jump; the content
# width, so padding is not counted twice), then the
# core table's behaviour (table.js) starts.
OVID_DT_INIT <- DT::JS("function(settings){var api=this.api();var id=api.table().container().id;setTimeout(function(){var css='';api.columns().every(function(i){var w=Math.floor($(api.column(i).header()).width());if(w>20)css+='#'+id+' th:nth-child('+(i+1)+'),#'+id+' td:nth-child('+(i+1)+'){min-width:'+w+'px}';});$('<style>').text(css).appendTo('head');api.columns.adjust();},200);if(window.ovidTable)window.ovidTable.init(api,settings);}")

# The table every page shows. d: the rows; page: rows per page; opts: more DataTables options; wrap: text longer than this
# many characters wraps; limits: quality limits to tint, as list(column = c(min = , max = )) (either end may be left out;
# an optional label = "..." names the rule in the tooltip); ...: passed to DT::datatable() (selection, colnames, escape,
# editable, ...). selection "single" or "multiple" keeps DataTables' row selection for pages that pick a record; otherwise
# (the default, or "none") values, blocks and rows are selected the ovid way and the selection is sent to Shiny as
# input$<outputId>_ovid_sel (rows and a count).
ovid_dt <- function(d, ..., page = 15, opts = list(), wrap = 48, limits = NULL) {
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  types <- vapply(d, function(v) if (inherits(v, c("POSIXct", "Date"))) "time" else if (is.numeric(v)) "num" else "chr", "")
  for (v in names(d)) {
    if (inherits(d[[v]], "POSIXct")) { s <- as.numeric(d[[v]]) %% 60                   # seconds only where records are faster than once a minute
      d[[v]] <- if (all(is.na(s) | s == 0)) format(d[[v]], "%Y-%m-%d %H:%M", tz = "UTC") else fmt_ts(d[[v]]) }
    else if (inherits(d[[v]], "Date")) d[[v]] <- format(d[[v]])
  }
  levels <- lapply(d[types == "chr"], function(v) { u <- unique(as.character(v)); u[is.na(u)] <- ""; if (length(u) <= 30) as.list(sort(u)) else NULL })
  levels <- levels[!vapply(levels, is.null, NA)]
  a <- list(...); if (is.null(a$colnames) && exists("ovid_colnames", mode = "function")) a$colnames <- ovid_colnames(names(d))
  sel <- a$selection
  cells <- is.null(sel) || identical(sel, "none") || (is.list(sel) && identical(sel$mode, "none"))
  if (cells) a$selection <- "none"
  lim <- if (length(limits)) lapply(limits[intersect(names(limits), names(d))], function(l) as.list(l)) else NULL
  base <- list(pageLength = page, scrollX = TRUE, dom = "frtip", lengthChange = FALSE, autoWidth = FALSE, language = list(search = "Filter:"),
               initComplete = OVID_DT_INIT,
               columnDefs = list(list(targets = "_all", render = DT::JS(sprintf(
                 "function(d,t){if(t==='display'&&d===null)return '—';return (t==='display'&&typeof d==='string'&&d.length>%d)?'<span class=\"wrapcell\">'+d+'</span>':d;}", as.integer(wrap))))),
               ovid = list(names = as.list(names(d)), types = as.list(unname(types)), levels = levels, limits = lim, cells = cells))
  if (nrow(d) > 8) base <- c(base, list(scrollY = paste0(36 * min(page, 14) + 4, "px"), scrollCollapse = FALSE))
  do.call(DT::datatable, c(list(d, rownames = FALSE, class = paste("hover d-tbl", if (cells) "d-cells")), a, list(options = utils::modifyList(base, opts))))
}
