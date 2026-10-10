# CS/pages/explore_availability.R  --  Explore: availability.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore: availability ---------------------------------------------
  # One grid for the whole project: a row per station (open it for its parameters), a column per month.
  # A cell is the share of days in the month with a daily value, read from the daily summaries
  # (output/derived/<station>__<table>_daily.csv), which hold a day only when it is at least 80 % recorded.
  av_data <- reactive({
    o <- outs(); req(o, nrow(o$stations) > 0); rv$tick
    rbindlist(lapply(sort(unique(o$stations$station)), function(k) {
      f <- file.path(pd(), "output", "derived", paste0(safe_name(paste(k, station_streams(k, pd()), sep = "__")), "_daily.csv"))
      f <- f[file.exists(f)]; if (!length(f)) return(NULL)
      d <- rbindlist(lapply(f, fread), fill = TRUE); if (!"Date" %in% names(d) || !nrow(d)) return(NULL)
      v <- setdiff(names(d)[vapply(d, is.numeric, NA)], "records"); if (!length(v)) return(NULL)
      d[, Date := as.Date(Date)]
      d <- d[, lapply(.SD, function(x) as.integer(any(!is.na(x)))), by = Date, .SDcols = v]   # tables of one station: a day counts once
      long <- melt(d, id.vars = "Date", variable.name = "parameter", value.name = "has")
      long[, month := as.Date(format(Date, "%Y-%m-01"))]
      a <- long[, .(n = sum(has)), by = .(month, parameter)]
      a[, `:=`(station = k, parameter = as.character(parameter))]
    }))
  })
  av_shade <- function(s) if (is.na(s)) "" else sprintf("background:color-mix(in srgb, var(--aqua) %d%%, var(--bg1))", round(8 + 82 * s))
  output$av_plot_ui <- renderUI({
    if (is.null(outs())) return(empty_msg())
    a <- av_data(); if (!nrow(a)) return(empty_msg("No daily summaries yet. Run the pipeline once with this version."))
    months <- seq(min(a$month), max(a$month), by = "month")
    days <- as.numeric(as.Date(format(months + 32, "%Y-%m-01")) - months)
    a[, share := pmin(1, n / days[match(month, months)])]
    st <- a[, .(share = mean(share)), by = .(station, month)]          # a station's month: the mean over its parameters
    yrs <- rle(format(months, "%Y"))
    cell <- function(k, s, m, n = NULL) {
      ttl <- paste0(k, " · ", format(m, "%b %Y"), ": ", if (is.na(s)) "no data" else paste0(round(100 * s), " % of days"), if (!is.null(n)) paste0(" (", n, ")"))
      tags$td(class = "av-c", style = av_shade(s), title = ttl, `data-av-st` = k, `data-av-m` = format(m))
    }
    row <- function(k, lab, x, cls, n = NULL) tags$tr(class = cls, `data-av-row` = k, tags$th(lab),
      lapply(seq_along(months), function(i) { s <- x$share[match(months[i], x$month)]; cell(k, s, months[i], n) }))
    body <- lapply(sort(unique(st$station)), function(k) {
      ps <- a[station == k]; pn <- sort(unique(ps$parameter))
      tags$tbody(class = "av-st",
        row(k, tagList(tags$span(class = "av-tw", "›"), k, span(class = "dim", paste0("  ", length(pn), " parameters"))), st[station == k], "av-top"),
        lapply(pn, function(p) row(k, p, ps[parameter == p], "av-par", p)))
    })
    tagList(
      div(class = "av-key dim", "Share of days with a daily value:", "0 %", span(class = "av-ramp"), "100 %",
          span(class = "dim", style = "margin-left:14px", "Open a station for its parameters. Click a month to plot it on One station.")),
      div(class = "av-wrap", tags$table(class = "av-grid",
        tags$thead(tags$tr(tags$th(""), lapply(seq_along(yrs$lengths), function(i) tags$th(colspan = yrs$lengths[i], class = "av-yr", yrs$values[i]))),
                   tags$tr(tags$th(""), lapply(months, function(m) tags$th(class = "av-mo", substr(format(m, "%b"), 1, 1))))),
        body)),
      tags$script(HTML("$('.av-grid').off('click.av').on('click.av', 'tr.av-top th', function(){ $(this).closest('tbody').toggleClass('open'); })
        .on('click.av', 'td.av-c', function(){ Shiny.setInputValue('av_pick', {station: this.getAttribute('data-av-st'), month: this.getAttribute('data-av-m'), n: Date.now()}, {priority: 'event'}); });")))
  })
  # a month clicked: One station, over time, opened on that month
  observeEvent(input$av_pick, {
    k <- input$av_pick$station; m <- as.Date(input$av_pick$month); req(k, !is.na(m))
    rv$ex_focus <- as.POSIXct(format(m + 15), tz = "UTC")
    updateSelectInput(session, "ex_kind", selected = "ts")
    if (!identical(input$ex_station, k)) updateSelectInput(session, "ex_station", selected = k) else rv$ex_send <- Sys.time()
    updateTabsetPanel(session, "ex_tab", selected = "One station")
  })
