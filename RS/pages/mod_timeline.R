# RS/pages/mod_timeline.R  --  The Timeline page and the timeline strip under the map, as Shiny modules. Rows are sites, columns
# the years from the first flight to the last (empty years kept), a square per flight coloured by sensor. Set site writes the
# flights table through a callback from app.R; a site is never filled in on a guess.

timeline_css <- function(pal = PAL) tags$style(HTML(sprintf("
.tl-wrap{overflow-x:auto} table.tl{border-collapse:collapse;font-size:12.5px} table.tl th,table.tl td{border:1px solid %1$s;padding:3px 4px;vertical-align:top}
table.tl thead th,table.tl tr:first-child th{text-align:center;font-weight:500} table.tl td{min-width:34px;max-width:150px} table.tl td.tl-empty{background:%2$s}
th.tl-site{cursor:pointer;white-space:nowrap;text-align:left;font-weight:500} tr.tl-row.on th.tl-site{color:%3$s} tr.tl-row.on td{background:%2$s}
.tl-sq{display:inline-block;margin:1px;border-radius:2px;cursor:pointer;vertical-align:middle} .tl-sq.on{outline:2px solid %4$s;outline-offset:1px}
.tl-key{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:6px;font-size:12.5px}
.tl-strip{display:flex;gap:4px;align-items:flex-end;overflow-x:auto;padding:4px 0} .tl-yr{flex:1 0 46px;cursor:pointer;border-radius:4px;padding:2px}
.tl-yr.on{background:%2$s;outline:1px solid %3$s} .tl-sqs{display:flex;flex-wrap:wrap-reverse;align-content:flex-start;min-height:10px}
.tl-lab{font-size:11px;text-align:center;border-top:1px solid %1$s;margin-top:2px}", pal$bg3, pal$bg2, pal$accent, pal$fg)))

# ---- the Timeline page -------------------------------------------------------------------------------------------------------
mod_timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(timeline_css(),
    layout_columns(fill = FALSE, col_widths = c(2, 2, 3, 3, 2),
      value_box("Sites", textOutput(ns("v_sites")), theme = VB()),
      value_box("Flights", textOutput(ns("v_flights")), p(class = "muted", textOutput(ns("v_flights_sub"), inline = TRUE)), theme = VB()),
      value_box("Revisited sites", textOutput(ns("v_rev")), p(class = "muted", "flown in two or more years"), theme = VB()),
      value_box("Longest gap", textOutput(ns("v_gap")), p(class = "muted", textOutput(ns("v_gap_sub"), inline = TRUE)), theme = VB()),
      value_box("Flights needing a site", textOutput(ns("v_nosite")), theme = VB())),
    layout_columns(col_widths = c(8, 4),
      card(card_header("Sites by year", span(class = "muted", " · click a site or a square")), card_body(uiOutput(ns("grid")))),
      card(card_header(textOutput(ns("row_title"), inline = TRUE)), card_body(uiOutput(ns("row")), uiOutput(ns("flight"))))))
}

# flights: all flights (a reactive); checks: flight_checks() of all flights; links: the catalogue's links (a reactive);
# save_site(ids, site): writes the site and shows it at once; open_flight(id): opens the Flight page on that flight.
mod_timeline_server <- function(id, flights, checks, links, save_site, open_flight) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    d <- reactive(timeline_flights(flights()))
    s <- reactive(timeline_summary(d()))
    row <- reactiveVal(NULL); pick <- reactiveVal(NULL)
    observeEvent(input$row, { row(input$row); pick(NULL) })
    observeEvent(input$pick, { pick(input$pick); x <- d()[flight_id == input$pick]; if (nrow(x)) row(x$site[1]) })
    output$v_sites <- renderText(num(s()$sites))
    output$v_flights <- renderText(num(s()$flights))
    output$v_flights_sub <- renderText({ n <- nrow(flights()) - nrow(d()); if (n > 0) paste(num(n), "more with no date") else "with a date" })
    output$v_rev <- renderText(num(s()$revisited))
    output$v_gap <- renderText(if (is.na(s()$gap)) "–" else paste(s()$gap, if (s()$gap == 1) "year" else "years"))
    output$v_gap_sub <- renderText(if (is.na(s()$gap)) "no site flown twice" else sprintf("%s: %d to %d", s()$gap_site, s()$gap_from, s()$gap_to))
    output$v_nosite <- renderText(num(s()$no_site))
    output$grid <- renderUI(HTML(timeline_html(d(), ns("row"), ns("pick"), row(), pick())))
    output$row_title <- renderText(row() %||% "Selected row")
    output$row <- renderUI({ r <- row(); if (is.null(r)) return(p(class = "muted", "Click a site or a square."))
      x <- d()[site == r]; f <- flights()[flight_id %in% x$flight_id]; k <- checks()[flight_id %in% x$flight_id]
      lk <- links(); lk <- if (!is.null(lk) && nrow(lk)) lk[flight_id %in% x$flight_id & state != "suggested"] else data.table()
      tab <- function(v) { t <- sort(table(v[nzchar(v)]), decreasing = TRUE); if (length(t)) paste(sprintf("%s (%d)", names(t), t), collapse = ", ") }
      tagList(fact("Flights", num(nrow(x))), fact("Years", sprintf("%d to %d (%d with flights)", min(x$year), max(x$year), uniqueN(x$year))),
        fact("Sensors", tab(x$sensor)), fact("Images", num(sum(x$n_images, na.rm = TRUE))),
        fact("Footprints", sprintf("%d of %d flights", sum(!is.na(f$lat_min)), nrow(f))),
        fact("Positions", tab(f$positioning_status)), fact("Placed from", tab(k$placed_from)),
        fact("Linked files", if (nrow(lk)) tab(lk$role)),
        if (r == NO_SITE) p(class = "muted mt-2", "These flights have no site. Click one and set its site below; nothing is filled in on a guess.")) })
    output$flight <- renderUI({ id <- pick(); if (is.null(id)) return(NULL); x <- d()[flight_id == id]; if (!nrow(x)) return(NULL)
      sites <- sort(unique(d()$site[d()$site != NO_SITE]))
      tagList(hr(), h3(x$name), fact("Date", x$date), fact("Sensor", x$sensor), fact("Images", num(x$n_images)), fact("Site", if (x$site != NO_SITE) x$site else "none"),
        selectizeInput(ns("site"), "Set site…", choices = c("", sites), selected = if (x$site != NO_SITE) x$site else "",
                       options = list(create = TRUE, placeholder = "Pick a site or type a new one")),
        actionButton(ns("save"), "Save site", class = "btn-primary btn-sm"), " ", actionLink(ns("open"), "Open this flight")) })
    observeEvent(input$save, { id <- pick(); req(id)
      tryCatch({ save_site(id, input$site); showNotification("Site saved", type = "message") },
               error = function(e) showNotification(conditionMessage(e), type = "error")) })
    observeEvent(input$open, { req(pick()); open_flight(pick()) })
  })
}

# ---- the strip under the map -------------------------------------------------------------------------------------------------
mod_timeline_strip_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Flights by year", span(class = "muted", " · click a year to show only its flights · "), actionLink(ns("open"), "Open the full timeline")),
       card_body(timeline_css(), uiOutput(ns("strip"))))
}

# flights: all flights; years: the years picked in the map's filter (a reactive); pick_year(y): toggles a year in that filter.
mod_timeline_strip_server <- function(id, flights, years, pick_year, open_timeline) {
  moduleServer(id, function(input, output, session) {
    output$strip <- renderUI(HTML(timeline_strip_html(timeline_flights(flights()), session$ns("year"), years() %||% character())))
    observeEvent(input$year, pick_year(input$year))
    observeEvent(input$open, open_timeline())
  })
}
