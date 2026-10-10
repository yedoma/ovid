# RS/pages/mod_flight_summary.R  --  The summary cards above the map, as a Shiny module: flights, images, placed, RTK/PPK
# corrected and flights with a check to look at, for the flights in view. Placed and corrected are separate cards: a log
# can place every image without any position being RTK or PPK.

mod_flight_summary_ui <- function(id) {
  ns <- NS(id)
  layout_columns(fill = FALSE, col_widths = c(2, 2, 3, 3, 2),
    value_box("Flights", textOutput(ns("n")), p(class = "muted", "in view"), theme = VB()),
    value_box("Images", textOutput(ns("img")), theme = VB()),
    value_box("Placed", textOutput(ns("placed")), p(class = "muted", textOutput(ns("placed_sub"), inline = TRUE)), theme = VB()),
    value_box("RTK / PPK corrected", textOutput(ns("corr")), p(class = "muted", "centimetre positions, fit for mapping"), theme = VB()),
    value_box("Checks to look at", textOutput(ns("look")), p(class = "muted", "flights with ! or ✕"), theme = VB()))
}

# flights: the flights in view (a reactive); checks: flight_checks() of all flights (a reactive).
mod_flight_summary_server <- function(id, flights, checks) {
  moduleServer(id, function(input, output, session) {
    s <- reactive({ f <- flights(); checks_summary(f, checks()[flight_id %in% f$flight_id]) })
    pc <- function(x) if (is.finite(x)) sprintf("%.0f %%", x) else "–"
    output$n <- renderText(num(s()$flights))
    output$img <- renderText(num(s()$images))
    output$placed <- renderText(pc(s()$placed_pct))
    output$placed_sub <- renderText({ x <- s(); paste0("image tags or a log", if (x$n_log > 0) paste0(" (", num(x$n_log), " from a log)") else "",
                                                       if (x$not_placed > 0) paste0("; ", num(x$not_placed), " with no position") else "") })
    output$corr <- renderText(pc(s()$corrected_pct))
    output$look <- renderText(num(s()$to_look))
  })
}
