# CS/pages/explore_compare_stations.R  --  Explore: compare stations.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore: compare stations -----------------------------------------
  cp_map <- reactive({                              # which station/stream holds which parameter
    o <- outs(); req(o, nrow(o$meta) > 0)
    m <- unique(o$meta[, .(logger_id, table, station, stream)])
    d <- merge(unique(o$dict[, .(logger_id, table, parameter)]), m, by = c("logger_id", "table"), allow.cartesian = TRUE)
    d <- d[!grepl(" | ", station, fixed = TRUE) & !grepl(" | ", stream, fixed = TRUE)]
    unique(d[, .(parameter, station, stream)])
  })
  observe({
    m <- unique(cp_map()[, .(parameter, station)]); n <- m[, .(n = .N), by = parameter][n > 1][order(-n)]
    if (!nrow(n)) return(update_selectize(session, "cp_param", choices = character(), server = TRUE))
    cur <- isolate(input$cp_param); if (!isTRUE(cur %in% n$parameter)) cur <- cs_home_pick(n$parameter, CS_HOME_RX$air) %||% n$parameter[1]   # opens on air temperature
    updateSelectizeInput(session, "cp_param", choices = setNames(n$parameter, paste0(n$parameter, "  (", n$n, " stations)")),
                         selected = cur, server = TRUE)
  })
  observeEvent(input$cp_param, {
    m <- cp_map()[parameter == input$cp_param]; req(nrow(m) > 0); ch <- sort(unique(m$station))
    updateSelectizeInput(session, "cp_stations", choices = ch, selected = head(ch, 12))
  })
  observe({ ch <- input$cp_stations; cur <- isolate(input$cp_diff)
    updateSelectInput(session, "cp_diff", choices = c("(none)" = "", ch), selected = if (isTRUE(cur %in% ch)) cur else "") })
  cp_data <- reactive({
    req(input$cp_param, length(input$cp_stations) > 0); p <- input$cp_param; rv$tick
    validate(need(length(input$cp_stations) <= 12, "Choose 12 stations or fewer."))
    rbindlist(lapply(input$cp_stations, function(v) {
      d <- tryCatch(load_station_all(v, pd(), columns = p), error = function(e) NULL)
      if (is.null(d) || !p %in% names(d) || !is.numeric(d[[p]])) return(NULL)
      d <- d[!is.na(get(p))]; if (!nrow(d)) return(NULL); d[, `:=`(station = v, stream = "", logger_id = "")]
      a <- aggregate_records(d, p, input$cp_res)
      data.table(TIMESTAMP = a$TIMESTAMP, value = a[[p]], station = v)[!is.na(value)]
    }))
  })
  # one panel per station on one time axis (the shared plot, core/tsview.R); one pointer line reads every station out
  output$cp_plot_ui <- renderUI(if (is.null(outs())) empty_msg() else tagList(ts_viewbar(), ts_view("cp_tsv", round(130 * psz()))))
  cp_wide <- reactive({ d <- cp_data(); validate(need(nrow(d) > 0 && any(!is.na(d$value)), "No values to compare for this choice."))
    dcast(d, TIMESTAMP ~ station, value.var = "value", fun.aggregate = function(x) x[1]) })
  observe({
    req(identical(input$ex_tab, "Compare stations")); w <- cp_wide(); st <- intersect(input$cp_stations, names(w)); req(length(st) > 0)
    u <- unit_of(input$cp_param); u <- if (is.na(u)) "" else unname(u); ref <- input$cp_diff
    if (nzchar(ref %||% "") && ref %in% st && length(st) > 1) st <- setdiff(st, ref)   # "Difference from": each other station minus that one
    series <- lapply(st, function(k) { v <- w[[k]]; nm <- k
      if (nzchar(ref %||% "") && ref %in% names(w) && k != ref) { v <- v - w[[ref]]; nm <- paste(k, "minus", ref) }
      list(name = nm, key = input$cp_param, unit = u, v = v, sum = grepl(TOTAL_RX, input$cp_param, ignore.case = TRUE)) })
    same <- isTRUE(input$cp_same)
    session$onFlushed(function() ts_view_send(session, "cp_tsv", w$TIMESTAMP, series, zone = "logger time", same_scale = same), once = TRUE)
  })
  output$cp_stats <- renderTable({
    d <- cp_data(); req(nrow(d) > 0)
    d[, .(Values = num(sum(!is.na(value))), From = format(min(TIMESTAMP), "%Y-%m-%d"), To = format(max(TIMESTAMP), "%Y-%m-%d"),
          Min = signif(suppressWarnings(min(value, na.rm = TRUE)), 5), Mean = signif(mean(value, na.rm = TRUE), 5),
          Max = signif(suppressWarnings(max(value, na.rm = TRUE)), 5)), by = .(Station = station)]
  }, striped = FALSE, bordered = FALSE, width = "100%", align = "l")
  output$cp_dl <- downloadHandler(filename = function() paste0("compare_", safe_name(input$cp_param), "_", Sys.Date(), ".csv"),
    content = function(f) fwrite(dcast(cp_data(), TIMESTAMP ~ station, value.var = "value"), f, dateTimeAs = "write.csv"))

