# GS/pages/mod_gpr_map.R  --  The GPR Map page as a Shiny module: every line with GPS as a track (the selected one highlighted),
# filters (antenna, year, with markers), the line table, and under the map a read-only radargram of the selected line. The cursor
# is shared: moving along the radargram moves a dot on the map, moving along the selected track moves a line on the radargram
# (Aarhus Workbench "Views"). Lines without GPS are listed, not placed. What it needs from the rest of the app is passed in.

mod_gpr_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ovid_controls(selectInput(ns("ant"), "Antenna", c(All = ""), width = "200px"),
      selectInput(ns("year"), "Year", c(All = ""), width = "120px"),
      ovid_switch(ns("mk"), "With markers only", FALSE),
      radioButtons(ns("base"), "Map", c(Satellite = "satellite", Streets = "streets"), inline = TRUE)),
    uiOutput(ns("head")),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Lines with GPS"), plotly::plotlyOutput(ns("map"), height = "440px")),
      card(card_header("Lines", span(class = "muted float-end", "click a row to select the line")),
        DTOutput(ns("tbl")),
        actionButton(ns("open"), "Open in Processing", class = "btn-sm btn-primary mt-2"))),
    card(card_header("Radargram of the selected line", span(class = "muted float-end", "read only; processed as under Processing")),
      plotly::plotlyOutput(ns("rg"), height = "340px")))
}

# lines: the viewable lines (gpr_scan rows); tracks: their GPS (named list of read_cor tables); flows: saved flow versions per
# line (named integer); selected: the selected line; markers: the selected line's field markers placed on the map (or NULL);
# radargram: list(fig, to_x(trace), to_trace(x)) of the selected line. select_line(line), open_processing(): act on the app.
mod_gpr_map_server <- function(id, lines, tracks, flows, selected, markers, radargram, select_line, open_processing) {
  moduleServer(id, function(input, output, session) {
    info <- reactive({ g <- lines(); trk <- tracks(); fl <- flows(); if (!nrow(g)) return(data.table())
      first <- function(k) { t <- trk[[k]]; if (is.null(t) || !nrow(t) || all(is.na(t$datetime))) "" else format(min(t$datetime, na.rm = TRUE), "%Y-%m-%d") }
      d <- g[, .(line, antennas, markers = !is.na(mrk))]
      d[, fixes := vapply(line, function(k) if (is.null(trk[[k]])) 0L else nrow(trk[[k]]), 0L)]
      d[, dropped := vapply(line, function(k) as.integer(attr(trk[[k]], "dropped") %||% 0L), 0L)]
      d[, date := vapply(line, first, "")][, year := substr(date, 1, 4)]
      d[, flow := vapply(line, function(k) { v <- fl[k]; if (length(v) && !is.na(v)) as.integer(v) else 0L }, 0L)]
      d })
    observe({ d <- info(); a <- sort(unique(d$antennas[nzchar(d$antennas %||% "")])); y <- sort(unique(d$year[nzchar(d$year %||% "")]))
      updateSelectInput(session, "ant", choices = c(All = "", a), selected = isolate(input$ant))
      updateSelectInput(session, "year", choices = c(All = "", y), selected = isolate(input$year)) })
    shown <- reactive({ d <- info(); if (!nrow(d)) return(d); a <- input$ant %||% ""; y <- input$year %||% ""
      if (nzchar(a)) d <- d[antennas == a]; if (nzchar(y)) d <- d[year == y]; if (isTRUE(input$mk)) d <- d[markers == TRUE]; d })

    output$head <- renderUI({ d <- info()
      if (!nrow(d)) return(p(class = "muted", "No GPR line can be viewed yet. Add the folder that holds the .rad and .rd3 files under GPR folders below."))
      s <- shown(); p(class = "muted", sprintf("%d of %d lines shown: %d placed from their GPS, %d without GPS (listed, not placed).%s", nrow(s), nrow(d), sum(s$fixes > 0), sum(s$fixes == 0),
        if (sum(s$dropped)) sprintf(" %d GPS fixes left out as impossible or stray (about a kilometre or more from the fixes around them).", sum(s$dropped)) else "")) })

    # ---- map, with a dot for the shared cursor as its last trace ----
    map_n <- reactiveVal(0L)
    output$map <- plotly::renderPlotly({ s <- shown(); trk <- tracks(); trk <- trk[intersect(s$line[s$fixes > 0], names(trk))]
      # safe: no markers: none drawn
      sel <- selected() %||% ""; mk <- tryCatch(markers(), error = function(e) NULL)
      g <- gpr_map(trk, sel, input$base %||% "satellite", mk); t0 <- trk[[sel]]
      g <- plotly::add_trace(g, type = "scattermapbox", mode = "markers", lat = if (!is.null(t0) && nrow(t0)) t0$lat[1] else 0, lon = if (!is.null(t0) && nrow(t0)) t0$lon[1] else 0,
                             hoverinfo = "skip", marker = list(size = 13, color = PAL$fg, opacity = 0), showlegend = FALSE, name = "cursor")
      g$x$source <- session$ns("map"); b <- plotly::plotly_build(g) |> plotly::event_register("plotly_click") |> plotly::event_register("plotly_hover"); map_n(length(b$x$data) - 1L); b })
    observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = session$ns("map"))), {
      cd <- plotly::event_data("plotly_click", source = session$ns("map"))$customdata; if (length(cd) && grepl("|", cd[1], fixed = TRUE)) select_line(sub("\\|.*$", "", cd[1])) })

    output$tbl <- renderDT({ s <- shown(); validate(need(nrow(s) > 0, "No line matches these filters."))
      ovid_long_table(s[, .(Line = line, `GPS fixes` = fixes, Date = date, Antenna = antennas, Flow = ifelse(flow > 0, paste0("v", flow), ""))],
                        selection = list(mode = "single", selected = which(s$line == isolate(selected() %||% ""))), height = "34vh") })
    observeEvent(input$tbl_rows_selected, { s <- shown(); i <- input$tbl_rows_selected; if (length(i) && i[1] <= nrow(s)) select_line(s$line[i[1]]) })
    observeEvent(input$open, open_processing())

    # ---- read-only radargram, with a line for the shared cursor added as one more shape ----
    rg_n <- reactiveVal(0L)
    output$rg <- plotly::renderPlotly({ R <- radargram(); f <- R$fig; f$x$source <- session$ns("rg"); b <- plotly::event_register(plotly::plotly_build(f), "plotly_hover"); rg_n(length(b$x$layout$shapes)); b })
    observeEvent(suppressWarnings(plotly::event_data("plotly_hover", source = session$ns("rg"))), {     # radargram -> dot on the map
      e <- plotly::event_data("plotly_hover", source = session$ns("rg")); t <- tracks()[[selected() %||% ""]]; if (is.null(e$x) || is.null(t) || nrow(t) < 2L) return()
      tr <- radargram()$to_trace(e$x[1]); c <- t[order(trace)][!duplicated(trace)]
      if (tr < min(c$trace) || tr > max(c$trace)) return()
      plotly::plotlyProxyInvoke(plotly::plotlyProxy("map", session), "restyle",
        list(lat = list(list(approx(c$trace, c$lat, tr)$y)), lon = list(list(approx(c$trace, c$lon, tr)$y)), `marker.opacity` = 1), list(map_n())) })
    observeEvent(suppressWarnings(plotly::event_data("plotly_hover", source = session$ns("map"))), {    # selected track -> line on the radargram
      cd <- plotly::event_data("plotly_hover", source = session$ns("map"))$customdata; if (!length(cd) || !grepl("|", cd[1], fixed = TRUE)) return()
      if (!identical(sub("\\|.*$", "", cd[1]), selected())) return()
      x <- radargram()$to_x(as.integer(sub("^.*\\|", "", cd[1])))
      plotly::plotlyProxyInvoke(plotly::plotlyProxy("rg", session), "relayout", setNames(list(list(type = "line", xref = "x", yref = "paper", x0 = x, x1 = x, y0 = 0, y1 = 1,
        line = list(color = "#ffffff", width = 1.5))), sprintf("shapes[%d]", rg_n()))) })
  })
}
