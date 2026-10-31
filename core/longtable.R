# Ovid/longtable.R  --  Long tables (a whole archive, every survey of a project, a line list, a measurement sheet) drawn a
# screen at a time: rows of one height (28 px), the heading fixed at the top, continuous scrolling with no pages, and a footer
# saying which rows are drawn of how many. Find and sort work on every row, on the server (DT's server-side processing,
# renderDT()'s default), not on the rows drawn. Built on DT's Scroller extension.
# Part of Ovid Core (shared). Sourced by each app's app.R; nothing else here.

OVID_ROW_PX <- 28L

# A datatable() for a long table. d: the rows; height: the scrolling area (CSS); dom: what is shown around it ("f" find,
# "t" table, "i" the drawn-of-total footer); options: more DataTables options; ...: passed to DT::datatable() (selection,
# callback, escape, colnames, ...).
ovid_long_table <- function(d, ..., height = "60vh", dom = "fti", options = list()) {
  w <- DT::datatable(d, rownames = FALSE, extensions = "Scroller", class = "compact stripe d-long", ...,
                     options = utils::modifyList(list(dom = dom, scrollX = TRUE, scrollY = height, scrollCollapse = TRUE, deferRender = TRUE,
                                                      columnDefs = (if (exists("ovid_right_defs", mode = "function")) ovid_right_defs(d)) %||% list(),   # numbers right-aligned (table.R)
                                                      scroller = list(rowHeight = OVID_ROW_PX, displayBuffer = 6), pageLength = 50,
                                                      language = list(info = "Rows _START_ to _END_ of _TOTAL_", infoFiltered = "(found in _MAX_)")), options))
  htmlwidgets::prependContent(w, htmltools::tags$style(htmltools::HTML(sprintf(
    "table.d-long tbody td{height:%dpx;max-height:%dpx;padding-top:0;padding-bottom:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:28em}", OVID_ROW_PX, OVID_ROW_PX))))
}
