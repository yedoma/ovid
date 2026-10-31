# CS/pages/explore_one_station.R  --  Explore: one station.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore: one station ----------------------------------------------
  st_label <- function(o) paste(o$stations$station, "/", o$stations$stream)
  st_value <- function(o) paste(o$stations$station, o$stations$stream, sep = "|||")
  observe({
    o <- outs(); req(o, nrow(o$stations) > 0)
    ch <- stn_names(o); cur <- isolate(input$ex_station)
    updateSelectInput(session, "ex_station", choices = ch, selected = if (isTRUE(cur %in% ch)) cur else ch[[1]])
  })
  stn_names <- function(o) sort(unique(o$stations$station))
  # a station is ONE record: every logger table it has is read and joined into one continuous series per parameter
  ex_key <- reactive({ req(input$ex_station); input$ex_station })
  # safe: a station that cannot be loaded shows no data; the run's problems say why
  ex_data <- reactive({ rv$tick; k <- ex_key(); tryCatch(load_station_all(k, pd()), error = function(e) NULL) })
  ex_numeric <- reactive({
    dt <- ex_data(); req(dt); v <- setdiff(names(dt), META_COLS)
    v[vapply(v, function(x) is.numeric(dt[[x]]) && any(!is.na(dt[[x]])), logical(1))]
  })
  grp_of <- function(p) {
    g <- m_gr$data(); if (is.null(g) || !nrow(g)) g <- default_groups()
    setNames(tryCatch(param_group(p, as.data.table(g)), error = function(e) rep("Other", length(p))), p)
  }
  unit_of <- function(p) {
    o <- outs(); if (is.null(o) || !NROW(o$dict) || !all(c("parameter", "unit") %in% names(o$dict))) return(setNames(rep(NA_character_, length(p)), p))
    u <- o$dict[parameter %in% p & !is.na(unit) & unit != "", .(unit = { t <- names(sort(base::table(unit), decreasing = TRUE)); if (length(t)) t[1] else NA_character_ }), by = parameter]
    setNames(sub("^[Dd]eg ?C$", "\u00b0C", u$unit[match(p, u$parameter)]), p)   # the logger's "deg C" shown as °C
  }
  observeEvent(input$ex_station, rv$zoom <- NULL)
  observe({
    v <- ex_numeric(); dt <- ex_data(); o <- isolate(outs())
    mapped <- intersect(v, unique(o$dict[parameter != raw_column, parameter]))
    keep <- intersect(isolate(input$ex_params), v)
    updateSelectizeInput(session, "ex_params", choices = list(`Common parameters` = as.list(mapped), `Raw columns` = as.list(setdiff(v, mapped))),
                         selected = if (length(keep)) keep else head(c(mapped, v), 4))
    r <- as.Date(range(dt$TIMESTAMP))
    updateDateRangeInput(session, "ex_dates", start = r[1], end = r[2], min = r[1], max = r[2])
  })
  ex_window <- reactive({
    if (!is.null(rv$zoom)) return(rv$zoom)
    req(input$ex_dates[1], input$ex_dates[2])
    c(as.POSIXct(format(input$ex_dates[1]), tz = "UTC"), as.POSIXct(format(input$ex_dates[2]), tz = "UTC") + 86399)
  })
  ex_view <- reactive({ dt <- ex_data(); req(dt); w <- ex_window(); dt[TIMESTAMP >= w[1] & TIMESTAMP <= w[2]] })
  ex_shown <- reactive({
    d <- ex_view(); p <- intersect(input$ex_params, names(d)); req(length(p) > 0)
    # long records are drawn as hourly or daily means unless the user zooms in or asks for every record
    res <- input$ex_res
    list(data = aggregate_records(d, p, res, "auto"), params = p, res = res)
  })
  ex_flags <- reactive({ o <- outs(); k <- ex_key(); if (is.null(o) || !nrow(o$flags)) return(data.table())
    f <- o$flags[station == k[1]]; if (nrow(f)) f[, `:=`(t0 = parse_time(start), t1 = parse_time(end))]; f })
  ex_lowbatt <- reactive({ dt <- ex_data(); req(dt); k <- ex_key(); thr <- LOW_BATT_V
    pick <- function(n) { b <- grep("batt", n, ignore.case = TRUE, value = TRUE); if (!length(b)) return(NULL); m <- grep("min", b, ignore.case = TRUE, value = TRUE); if (length(m)) m[1] else b[1] }
    col <- pick(names(dt)); x <- NULL
    if (!is.null(col)) x <- dt[, .(TIMESTAMP, v = get(col))]
    else { o <- outs(); if (!is.null(o) && all(c("station", "stream", "parameter") %in% names(o$pstats))) { c2 <- o$pstats[station == k[1] & grepl("batt", parameter, ignore.case = TRUE)]
        if (nrow(c2)) { col <- pick(c2$parameter); sm <- c2[parameter == col, stream][1]
          # safe: a station that cannot be loaded adds no line
          x <- tryCatch(load_station(k[1], sm, pd(), columns = col)[, .(TIMESTAMP, v = get(col))], error = function(e) NULL) } } }
    if (is.null(x) || !is.numeric(x$v)) return(NULL)
    i <- which(!is.na(x$v) & x$v < thr); if (!length(i)) return(data.table(t0 = as.POSIXct(character(), tz = "UTC"), t1 = as.POSIXct(character(), tz = "UTC")))
    r <- cumsum(c(1L, diff(i) > 2L)); data.table(t0 = x$TIMESTAMP[i[!duplicated(r)]], t1 = x$TIMESTAMP[i[!duplicated(r, fromLast = TRUE)]], column = col) })
  ex_log <- reactive({ l <- m_fl$data(); k <- ex_key(); if (is.null(l) || !nrow(l)) return(data.table())
    l <- as.data.table(l)[station == k[1]]; if (nrow(l)) l[, t := parse_time(date)]; l[!is.na(t)] })
  ex_files <- reactive({ d <- ex_view(); p <- intersect(input$ex_params, names(d)); if (!length(p)) return(data.table()); files_used(d, p) })
  output$ex_src_note <- renderUI({ d <- ex_data(); req(d); tb <- attr(d, "tables"); f <- ex_files(); k <- ex_key()
    fmt_step <- function(s) ifelse(!is.finite(s), "", ifelse(s < 3600, paste(round(s / 60), "min"), ifelse(s < 86400, paste(round(s / 3600, 1), "h"), paste(round(s / 86400, 1), "d"))))
    p(class = "dim", tags$b(style = "color:var(--fg)", k), " is plotted as one continuous record per parameter, joined from ", tags$b(style = "color:var(--fg)", num(nrow(f)), if (nrow(f) == 1) " file" else " files"),
      " for the parameters and period shown.",
      if (!is.null(tb) && nrow(tb) > 1) tagList(" It has ", nrow(tb), " logger tables (", paste0(tb$stream, " every ", fmt_step(tb$step_seconds), collapse = ", "),
        "). Where two tables record the same parameter on the same day, the one with the shorter interval is used."),
      " Where files overlap, the first and last record used are what was kept after overlaps were resolved.") })
  output$ex_src <- renderDT({ f <- ex_files(); validate(need(nrow(f) > 0, "Pick at least one parameter.")); ovid_dt(f, selection = "none", page = 8, opts = list(scrollX = TRUE)) })
  ex_removed <- reactive({ k <- ex_key(); rv$tick
    d <- read_detail(pd(), paste0(safe_name(paste0("station_", k[1], "__", station_streams(k[1], pd()))), ".csv"))
    if (nrow(d)) d[stage == "qc"] else d })
  output$ex_plot_ui <- renderUI({
    if (is.null(outs())) return(empty_msg())
    kind <- input$ex_kind %||% "ts"
    if (kind == "ts") return(tagList(ts_viewbar(), ts_view("ex_tsv", round(190 * psz()))))   # Over time: the shared plot (core/tsview.R)
    np <- max(1, length(input$ex_params)); z <- psz()
    h <- switch(kind, daily = 120 + 230 * np, calendar = 120 + 260 * np, hist = 130 + 250 * ceiling(np / 2), duration = 130 + 280 * ceiling(np / 2), scatter = 130 + 320 * ceiling(max(1, np - 1) / 2),
                box = 130 + 270 * ceiling(np / 2), diurnal = 130 + 270 * ceiling(np / 2), windrose = 620, 120 + 270 * np)
    plotOutput("ex_plot2", height = paste0(round(h * z), "px"))
  })
  # a nudge when the chart does not suit what is selected
  output$ex_fit <- renderUI({ p <- input$ex_params; k <- input$ex_kind %||% "ts"; if (!length(p) || is.null(outs())) return(NULL)
    tot <- grepl(TOTAL_RX, p, ignore.case = TRUE); dirp <- grepl("wind_?dir", p, ignore.case = TRUE) & !grepl("sd|std", p, ignore.case = TRUE)
    msg <- if (k == "cum" && !any(tot)) "Running total adds every record up. It suits totals such as rain; the parameters chosen are averages, so the result has no physical meaning."
      else if (k %in% c("band", "years", "box", "diurnal") && any(dirp)) "Wind direction is a compass angle, so its mean, minimum and maximum mislead here. Try the wind rose."
      else if (k == "ts" && any(tot)) "Totals such as rain are drawn as bars, and summed when shown hourly, daily or monthly."
      else NULL
    if (is.null(msg)) NULL else div(class = "fitnote", icon("circle-info"), " ", msg) })
  bands_of <- function(p) { i <- info_of(p)
    data.table(parameter = p, lo = suppressWarnings(as.numeric(i$typical_min)), hi = suppressWarnings(as.numeric(i$typical_max))) }
  ex_g <- reactive({
    validate(need(length(input$ex_params) > 0, "Pick at least one parameter."))
    s <- ex_shown(); validate(need(nrow(s$data) > 0, "No records in this period."))
    rem <- ex_removed(); b <- bands_of(s$params)
    g <- plot_station(s$data, s$params, removed = if (nrow(rem)) rem else NULL, bands = b, style = "auto", units = unit_of(s$params))
    validate(need(!is.null(g), "These parameters have no values in this period."))
    has <- rowSums(!is.na(s$data[, s$params, with = FALSE])) > 0      # shading and marks stay inside the period that has data for what is plotted
    w <- range(s$data$TIMESTAMP[has]); mk <- input$ex_marks; under <- list(); u <- unit_of(s$params)
    panel_of <- function(p) ifelse(!is.na(u[p]) & nzchar(u[p]), paste0(p, "   (", u[p], ")"), p)
    lev <- unique(panel_of(s$params))
    if ("flags" %in% mk) { f <- ex_flags(); if (nrow(f)) { f <- f[parameter %in% s$params & t1 >= w[1] & t0 <= w[2]]
      if (nrow(f)) { f <- f[order(-as.numeric(records))][seq_len(min(.N, 400))]; f[, panel := factor(panel_of(parameter), levels = lev)]
        f[, `:=`(t0 = pmax(t0, w[1]), t1 = pmin(pmax(t1, t0 + diff(as.numeric(w)) / 800), w[2]))]
        under <- c(under, list(geom_rect(data = f, aes(xmin = t0, xmax = t1, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = GB$orange, alpha = 0.22))) } } }
    if ("batt" %in% mk) { b <- ex_lowbatt(); if (!is.null(b) && nrow(b)) { b <- b[t1 >= w[1] & t0 <= w[2]]
      if (nrow(b)) { b <- b[seq_len(min(.N, 400))]; b[, `:=`(t0 = pmax(t0, w[1]), t1 = pmin(pmax(t1, t0 + diff(as.numeric(w)) / 800), w[2]))]
        under <- c(under, list(geom_rect(data = b, aes(xmin = t0, xmax = t1, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = GB$purple, alpha = 0.16))) } } }
    if (length(under)) g$layers <- c(under, g$layers)
    if ("log" %in% mk) { l <- ex_log(); if (nrow(l)) { l <- l[t >= w[1] & t <= w[2]]
      if (nrow(l)) g <- g + geom_vline(data = l, aes(xintercept = t), colour = GB$fg4, linetype = "dotted", linewidth = 0.4) } }
    g + labs(title = NULL, caption = "orange = suspect values (kept);  purple = battery low;  dotted line = field-log entry\ngreen tint = usual range for this kind of measurement, shown where values fall outside it; red ticks = values removed by quality rules",
           subtitle = paste0(paste(ex_key(), collapse = " / "), "   ", format(min(s$data$TIMESTAMP), "%Y-%m-%d"), " to ",
                             format(max(s$data$TIMESTAMP), "%Y-%m-%d"), "   ", names(res_choices)[res_choices == s$res],
                             if (!is.null(rv$zoom)) "   (zoomed)" else ""))
  })
  # ex_g() is the Over time picture for the PNG and PDF exports; on screen it is the shared plot below
  # Over time on the shared plot: suspect values and low battery are shaded, field-log entries marked
  ex_issues <- reactive({ p <- input$ex_params; mk <- input$ex_marks; out <- list()
    if ("flags" %in% mk) { f <- ex_flags(); if (nrow(f)) { f <- f[parameter %in% p & !is.na(t0) & !is.na(t1)][order(-as.numeric(records))][seq_len(min(.N, 400))]
      if (nrow(f)) out[[1]] <- f[, .(t0, t1, label = paste0("suspect values kept: ", parameter), level = "warn")] } }
    if ("batt" %in% mk) { b <- ex_lowbatt(); if (!is.null(b) && nrow(b)) out[[2]] <- b[seq_len(min(.N, 400)), .(t0, t1, label = paste0("battery low (", column, ")"), level = "error")] }
    if ("log" %in% mk) { l <- ex_log(); if (nrow(l)) out[[3]] <- l[, .(t0 = t, t1 = t, label = trim(paste("field log:", type, note)), level = "warn")] }
    rbindlist(out) })
  observe({
    req(identical(input$ex_tab, "One station"), identical(input$ex_kind %||% "ts", "ts")); rv$ex_send
    d <- ex_view(); p <- intersect(input$ex_params, names(d)); req(length(p) > 0, nrow(d) > 0)
    u <- unit_of(p); k <- ex_key(); iss <- ex_issues(); focus <- isolate(rv$ex_focus)
    series <- lapply(p, function(x) list(name = x, key = x, unit = if (is.na(u[x])) "" else unname(u[x]), v = as.numeric(d[[x]]),
                                         sum = grepl(TOTAL_RX, x, ignore.case = TRUE)))
    # events (plot standards A11): where a new source file starts and each field-log entry; sources: the logger and file of each stretch
    fu <- ex_files(); lg <- ex_log(); f0 <- if (nrow(fu)) parse_time(fu$`First record used`)
    ev <- rbindlist(list(if (nrow(fu) > 1) data.table(t = as.numeric(f0[-1]), label = "New file", note = fu$File[-1]),
                         if (nrow(lg)) data.table(t = as.numeric(lg$t), label = "Field log", note = trim(paste(lg$type %||% "", lg$note %||% "")))), fill = TRUE)
    src <- if (nrow(fu)) data.table(from = as.numeric(f0), label = paste0(fu$Logger, " \u00b7 ", fu$File))
    session$onFlushed(function() { ts_view_send(session, "ex_tsv", d$TIMESTAMP, series, zone = "logger time", issues = if (nrow(iss)) iss,
                                                wb = list(key = "station", value = k, params = p), focus = focus, events = if (nrow(ev)) ev[!is.na(t)][seq_len(min(.N, 300))],
                                                sources = src, title = paste(p, collapse = ", "), site = k[1], file = paste0("cs_", k[1], "_timeseries")); isolate(rv$ex_focus <- NULL) }, once = TRUE)
  })
  # the other three charts
  ex2_g <- reactive({
    d <- ex_view(); p <- intersect(input$ex_params, names(d)); kind <- input$ex_kind
    validate(need(length(p) > 0, "Pick at least one parameter."), need(nrow(d) > 1, "No records in this period."))
    u <- unit_of(p); lab <- function(v) ifelse(!is.na(u[v]) & nzchar(u[v]), paste0(v, " (", u[v], ")"), v)
    ttl <- paste(ex_key(), collapse = " / ")
    pcol <- param_colours(p, GB)                     # each parameter in the colour of what it measures (Ovid/theme.R)
    ramp_fill <- function(par, x) { out <- character(length(x)); for (q in unique(as.character(par))) { i <- as.character(par) == q
      out[i] <- param_ramp(pcol[[q]], GB, 64)[pmin(64, pmax(1, round(x[i] * 63) + 1))] }; out }
    if (kind == "daily") {
      step <- stats::median(diff(as.numeric(d$TIMESTAMP)))
      validate(need(is.finite(step) && step < 86400, "This table has one record a day or fewer, so there is no time-of-day pattern to show."))
      long <- melt(d[, c("TIMESTAMP", p), with = FALSE], id.vars = "TIMESTAMP", variable.name = "parameter", na.rm = TRUE)
      long[, `:=`(day = as.Date(TIMESTAMP), hour = as.integer(format(TIMESTAMP, "%H", tz = "UTC")))]
      a <- long[, .(value = mean(value)), by = .(parameter, day, hour)]
      a[, scaled := { q <- stats::quantile(value, c(0.01, 0.99), names = FALSE); if (q[2] > q[1]) pmin(1, pmax(0, (value - q[1]) / (q[2] - q[1]))) else 0.5 }, by = parameter]
      a[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]; a[, fillc := ramp_fill(parameter, scaled)]
      return(ggplot(a, aes(day, hour, fill = fillc)) + geom_tile(width = 1, height = 1) + facet_wrap(~panel, ncol = 1) + scale_fill_identity() +
        scale_y_continuous(breaks = c(0, 6, 12, 18, 23), expand = c(0, 0)) + scale_x_date(expand = c(0, 0)) +
        labs(x = NULL, y = "Hour of day (logger time)", subtitle = paste0(ttl, "   dark = the parameter's low, bright = its high"),
             caption = "Sunlight and temperature should peak at the same hour all year. A band that steps sideways means the clock moved.") +
        theme(panel.grid = element_blank()))
    }
    if (kind == "hist") {
      long <- melt(d[, p, with = FALSE], measure.vars = p, variable.name = "parameter", na.rm = TRUE)
      long[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]
      b <- bands_of(p)[!is.na(lo) & !is.na(hi)]
      b <- merge(b, long[, .(dlo = min(value), dhi = max(value)), by = parameter], by = "parameter")
      v <- rbind(b[lo > dlo, .(parameter, x = lo)], b[hi < dhi, .(parameter, x = hi)])
      if (nrow(v)) v[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]
      g <- ggplot(long, aes(value, fill = parameter)) + geom_histogram(bins = 60, colour = GB$bg, linewidth = 0.15) + scale_fill_manual(values = pcol, guide = "none") +
        facet_wrap(~panel, ncol = 2, scales = "free") + labs(x = NULL, y = "Records", subtitle = ttl,
          caption = if (nrow(v)) "dashed line = edge of the usual range for this kind of measurement" else NULL)
      if (nrow(v)) g <- g + geom_vline(data = v, aes(xintercept = x), colour = GB$fg, linetype = "dashed", linewidth = 0.4)
      return(g)
    }
    if (kind == "band") {
      long <- melt(d[, c("TIMESTAMP", p), with = FALSE], id.vars = "TIMESTAMP", variable.name = "parameter", na.rm = TRUE); validate(need(nrow(long) > 0, "These parameters have no values in this period."))
      a <- long[, .(lo = min(value), mid = mean(value), hi = max(value)), by = .(parameter, day = as.Date(TIMESTAMP))]; setorder(a, parameter, day)
      a[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]; a[, seg := seg_id(as.numeric(day) * 86400, 86400), by = parameter]; a[, n := .N, by = .(parameter, seg)]
      return(ggplot(a, aes(day, mid, group = interaction(parameter, seg), colour = parameter, fill = parameter)) + geom_ribbon(data = a[n > 1], aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.3) +
        geom_line(data = a[n > 1], linewidth = 0.45) + geom_linerange(data = a[n == 1], aes(ymin = lo, ymax = hi), alpha = 0.6) + geom_point(data = a[n == 1], size = 0.5) +
        scale_colour_manual(values = pcol, guide = "none") + scale_fill_manual(values = pcol, guide = "none") +
        facet_wrap(~panel, ncol = 1, scales = "free_y") + labs(x = NULL, y = NULL, subtitle = paste0(ttl, "   each day: lowest to highest record (band) and the mean (line)"),
          caption = "A day with a single record, such as a daily table, has no band."))
    }
    if (kind == "calendar") {
      long <- melt(d[, c("TIMESTAMP", p), with = FALSE], id.vars = "TIMESTAMP", variable.name = "parameter", na.rm = TRUE); validate(need(nrow(long) > 0, "These parameters have no values in this period."))
      tot <- grepl(TOTAL_RX, p, ignore.case = TRUE); names(tot) <- p
      a <- long[, .(value = if (tot[[as.character(parameter[1])]]) sum(value) else mean(value)), by = .(parameter, day = as.Date(TIMESTAMP))]
      a[, `:=`(Year = year(day), doy = as.Date("2000-01-01") + yday(day) - 1, panel = factor(lab(as.character(parameter)), levels = unique(lab(p))))]
      a[, scaled := { q <- stats::quantile(value, c(0.01, 0.99), names = FALSE); if (q[2] > q[1]) pmin(1, pmax(0, (value - q[1]) / (q[2] - q[1]))) else 0.5 }, by = parameter]
      a[, fillc := ramp_fill(parameter, scaled)]
      return(ggplot(a, aes(doy, Year, fill = fillc)) + geom_tile(width = 1, height = 0.9) + facet_wrap(~panel, ncol = 1) + scale_fill_identity() +
        scale_y_reverse(breaks = function(l) seq(ceiling(l[2]), floor(l[1]))) + scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
        labs(x = NULL, y = NULL, subtitle = paste0(ttl, "   one row per year, one cell per day (daily mean; totals are summed)"), caption = "Blank cells are days with no data. Dark = the parameter's low, bright = its high.") +
        theme(panel.grid = element_blank()))
    }
    if (kind == "duration") {
      long <- melt(d[, p, with = FALSE], measure.vars = p, variable.name = "parameter", na.rm = TRUE); validate(need(nrow(long) > 0, "These parameters have no values in this period."))
      a <- long[, { v <- sort(value, decreasing = TRUE); i <- unique(round(seq(1, length(v), length.out = min(length(v), 1500)))); .(pct = 100 * i / length(v), value = v[i]) }, by = parameter]
      a[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]
      return(ggplot(a, aes(pct, value, colour = parameter)) + geom_line(linewidth = 0.6) + scale_colour_manual(values = pcol, guide = "none") + facet_wrap(~panel, ncol = 2, scales = "free_y") +
        scale_x_continuous(breaks = c(0, 10, 25, 50, 75, 90, 100)) + labs(x = "Records at or above this value (%)", y = NULL, subtitle = paste0(ttl, "   read across: the value exceeded that share of the time")))
    }
    if (kind == "windrose") {
      full <- ex_view(); n <- names(full); pick <- function(rx, not = "$^") { x <- grep(rx, n, ignore.case = TRUE, value = TRUE); x <- x[!grepl(not, x, ignore.case = TRUE)]; x <- x[vapply(x, function(c) is.numeric(full[[c]]) && any(!is.na(full[[c]])), logical(1))]; if (length(x)) x[1] else NA }
      ws <- intersect(p, grep("wind_?sp|^ws_|wnd_spd|windsp", n, ignore.case = TRUE, value = TRUE))[1]; if (is.na(ws)) ws <- pick("^wind_speed$|wind_?sp|^ws_|wnd_spd", "max|min|sd|std")
      wd <- intersect(p, grep("wind_?dir|wnd_dir", n, ignore.case = TRUE, value = TRUE))[1]; if (is.na(wd)) wd <- pick("^wind_dir$|wind_?dir|wnd_dir", "sd|std")
      validate(need(!is.na(ws) && !is.na(wd), "A wind rose needs a wind speed and a wind direction; this station has no such pair."))
      w <- full[!is.na(get(ws)) & !is.na(get(wd)), .(sp = get(ws), dir = get(wd) %% 360)]; validate(need(nrow(w) > 10, "Too few wind records in this period."))
      w[, sector := factor(c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")[(floor((dir + 11.25) / 22.5) %% 16) + 1],
                           levels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))]
      br <- unique(c(0, 1, 2, 4, 6, 8, 12, Inf)); w[, cls := cut(sp, br, right = FALSE, labels = c("under 1", "1 to 2", "2 to 4", "4 to 6", "6 to 8", "8 to 12", "12 and over"))]
      a <- w[, .(pct = 100 * .N / nrow(w)), by = .(sector, cls)]
      return(ggplot(a, aes(sector, pct, fill = cls)) + geom_col(width = 0.9, colour = GB$bg, linewidth = 0.2, position = position_stack(reverse = TRUE), show.legend = TRUE) + coord_polar(start = -pi / 16) +
        scale_x_discrete(drop = FALSE) + scale_fill_manual(values = grDevices::colorRampPalette(c(GB$gray, GB$blue, GB$aqua, GB$yellow, GB$orange, GB$red))(7), drop = FALSE, name = "Wind speed (m/s)") +
        labs(x = NULL, y = "Records (%)", subtitle = paste0(ttl, "   where the wind comes from, ", ws, " by ", wd, ", ", num(nrow(w)), " records")) + theme(panel.grid.major = element_line(colour = GB$bg2)))
    }
    if (kind %in% c("years", "box", "diurnal", "cum")) {
      long <- melt(d[, c("TIMESTAMP", p), with = FALSE], id.vars = "TIMESTAMP", variable.name = "parameter", na.rm = TRUE)
      validate(need(nrow(long) > 0, "These parameters have no values in this period."))
      long[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p)))]
      if (kind == "years") {                      # one line per year on a shared axis that starts in the month chosen
        m0 <- as.integer(input$ex_ystart %||% "1"); an <- identical(input$ex_ymode, "anom")
        a <- long[, .(value = mean(value)), by = .(panel, day = as.Date(TIMESTAMP))]
        a[, yr := year(day) + (m0 > 1 & month(day) >= m0)]; a[, start := as.Date(sprintf("%d-%02d-01", yr - (m0 > 1), m0))]
        a[, `:=`(off = as.integer(day - start), Year = factor(if (m0 > 1) paste0(yr - 1, "-", substr(yr, 3, 4)) else as.character(yr)))]
        a <- a[off <= 365]
        if (an) { a[, avg := mean(value), by = .(panel, off)]; a[, value := value - avg] }
        ref <- as.Date(sprintf("2001-%02d-01", m0)); a[, doy := ref + off]
        cols <- grDevices::colorRampPalette(c(GB$blue, GB$aqua, GB$yellow, GB$orange, GB$red))(nlevels(a$Year))
        setorder(a, panel, day); a[, seg := seg_id(as.numeric(day) * 86400, 86400), by = .(panel, Year)]; a[, n := .N, by = .(panel, Year, seg)]
        g <- ggplot(a, aes(doy, value, colour = Year, group = interaction(Year, seg)))
        if (an) g <- g + geom_hline(yintercept = 0, colour = GB$fg4, linewidth = 0.3)
        return(g + geom_line(data = a[n > 1], linewidth = 0.4, alpha = 0.9) + geom_point(data = a[n == 1], size = 0.5) +
          facet_wrap(~panel, ncol = 1, scales = "free_y") + scale_colour_manual(values = cols) +
          scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.01, 0)) +
          labs(x = if (m0 > 1) paste("from 1", month.name[m0]) else NULL, y = NULL,
               subtitle = paste0(ttl, if (an) "   daily means minus the average of every year on that day" else "   daily means, one line per year")))
      }
      if (kind == "box") {
        long[, Month := factor(month.abb[month(TIMESTAMP)], levels = month.abb)]
        return(ggplot(long, aes(Month, value, colour = parameter)) + geom_boxplot(fill = GB$bg1, outlier.size = 0.3, outlier.alpha = 0.3, outlier.colour = GB$fg4, linewidth = 0.35) +
          scale_colour_manual(values = pcol, guide = "none") +
          facet_wrap(~panel, ncol = 2, scales = "free_y") + labs(x = NULL, y = NULL, subtitle = paste0(ttl, "   every record in the period, by calendar month"),
            caption = "box = middle half of the values, line = median, dots = far from the rest"))
      }
      if (kind == "diurnal") {
        step <- stats::median(diff(as.numeric(d$TIMESTAMP)))
        validate(need(is.finite(step) && step < 86400, "This table has one record a day or fewer, so there is no time-of-day pattern to show."))
        long[, `:=`(hour = as.integer(format(TIMESTAMP, "%H", tz = "UTC")),
                    Season = factor(c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter")[month(TIMESTAMP)],
                                    levels = c("Winter", "Spring", "Summer", "Autumn")))]
        a <- long[, .(value = mean(value)), by = .(panel, Season, hour)]
        return(ggplot(a, aes(hour, value, colour = Season)) + geom_line(linewidth = 0.6) + geom_point(size = 0.8) +
          facet_wrap(~panel, ncol = 2, scales = "free_y") + scale_x_continuous(breaks = c(0, 6, 12, 18, 23)) +
          scale_colour_manual(values = c(Winter = GB$blue, Spring = GB$green, Summer = GB$yellow, Autumn = GB$orange)) +
          labs(x = "Hour of day (logger time)", y = NULL, subtitle = paste0(ttl, "   mean for each hour of the day, by season")))
      }
      setorder(long, parameter, TIMESTAMP); long[, total := cumsum(value), by = parameter]
      if (nrow(long) > 40000) long <- long[, .SD[unique(round(seq(1, .N, length.out = 8000)))], by = parameter]
      return(ggplot(long, aes(TIMESTAMP, total, colour = parameter)) + geom_line(linewidth = 0.5) + scale_colour_manual(values = pcol, guide = "none") + facet_wrap(~panel, ncol = 1, scales = "free_y") +
        labs(x = NULL, y = "Running total", subtitle = paste0(ttl, "   sum of every record since the start of the period"),
             caption = "Meant for totals such as rain or flow. Missing records add nothing, so a gap shows as a flat stretch."))
    }
    validate(need(length(p) >= 2, "Pick two or more parameters. The first is the horizontal axis; each of the others is plotted against it."))
    dd <- d[, p, with = FALSE]; if (nrow(dd) > 30000) dd <- dd[sample(.N, 30000)]
    long <- melt(dd, id.vars = p[1], measure.vars = p[-1], variable.name = "parameter", na.rm = TRUE)
    setnames(long, p[1], "x"); long <- long[!is.na(x)]
    long[, panel := factor(lab(as.character(parameter)), levels = unique(lab(p[-1])))]
    ggplot(long, aes(x, value, colour = parameter)) + geom_point(alpha = 0.18, size = 0.6, stroke = 0) + scale_colour_manual(values = pcol, guide = "none") +
      facet_wrap(~panel, ncol = 2, scales = "free_y") + labs(x = lab(p[1]), y = NULL, subtitle = ttl,
        caption = if (nrow(d) > 30000) "a random 30,000 records are drawn" else NULL)
  })
  output$ex_plot2 <- renderPlot(ex2_g(), bg = GB$bg, res = 96)
  output$ex_about <- renderUI({
    p <- input$ex_params; if (!length(p) || is.null(outs())) return(NULL)
    i <- unique(info_of(p)[!is.na(topic)], by = "topic"); if (!nrow(i)) return(NULL)
    tags$details(tags$summary("About these measurements"), lapply(seq_len(nrow(i)), function(k) div(class = "explain",
      tags$b(i$topic[k]), if (nzchar(i$unit[k])) span(class = "dim", paste0("  (", i$unit[k], ")")), tags$br(),
      i$reasonable[k], tags$br(), span(class = "dim", i$watch_for[k]))),
      actionLink("ex_to_guide", "Open the parameter guide"))
  })
  observeEvent(input$ex_to_guide, go("Parameter guide"))
  output$ex_brush_ui <- renderUI({
    r <- brush_range(); if (is.null(r)) return(NULL); t0 <- r[1]; t1 <- r[2]
    div(class = "brushbox editor-only",
        div(class = "btnrow", style = "margin-top:0", tags$b(fmt_ts(t0), " to ", fmt_ts(t1)), span(class = "dim", "Remove the values in this stretch:")),
        fluidRow(class = "editor-only",
          column(4, selectizeInput("br_params", "Parameters", choices = c("Every parameter" = "*", ex_numeric()),
                                   selected = input$ex_params, multiple = TRUE, width = "100%")),
          column(4, textInput("br_reason", "Reason (goes in the audit log)", width = "100%")),
          column(2, textInput("br_author", "Your initials", isolate(who()), width = "100%")),
          column(2, div(style = "margin-top:24px", actionButton("br_add", "Remove values", class = "btn-danger d-hide-std")))))
  })
  # the stretch selected on Over time with a drag (input$ex_tsv_sel from the shared plot)
  brush_range <- function() { b <- input$ex_tsv_sel
    if (!identical(input$ex_kind %||% "ts", "ts") || is.null(b$t0)) return(NULL)
    as.POSIXct(round(c(b$t0, b$t1)), origin = "1970-01-01", tz = "UTC") }
  observeEvent(input$br_add, {
    r <- brush_range(); req(r); if (!wb_made(E$period)) return()
    if (!nzchar(trim(input$br_reason)) || !length(input$br_params)) return(note("Give a reason and at least one parameter.", type = "warning"))
    k <- ex_key()
    par <- if ("*" %in% input$br_params) "" else paste(rx_escape(input$br_params), collapse = "|")
    ok <- m_mn$append(data.frame(station = k[1], stream = "", parameter = par, start = fmt_ts(r[1]), end = fmt_ts(r[2]),
                                 reason = trim(input$br_reason), author = trim(input$br_author)))
    if (ok) note("Removal added. Run the pipeline to apply it.")
  })
  output$ex_stats <- renderTable({
    d <- ex_view(); r <- brush_range(); if (!is.null(r)) d <- d[TIMESTAMP >= r[1] & TIMESTAMP <= r[2]]   # a selected stretch, else the period
    p <- intersect(input$ex_params, names(d)); req(length(p) > 0, nrow(d) > 0)
    rem <- ex_removed(); rem <- if (nrow(rem)) rem[, .N, by = column] else data.table(column = character(), N = integer())
    u <- unit_of(p)
    rbindlist(lapply(p, function(v) { x <- d[[v]]; ok <- any(!is.na(x)); data.table(Parameter = v, Unit = if (is.na(u[v])) "" else u[v], Group = grp_of(v),
      Records = num(sum(!is.na(x))), `Missing %` = sprintf("%.1f", miss_pct(d, v)),
      Min = if (ok) signif(min(x, na.rm = TRUE), 5) else NA, Mean = if (ok) signif(mean(x, na.rm = TRUE), 5) else NA,
      Max = if (ok) signif(max(x, na.rm = TRUE), 5) else NA,
      `Removed by quality rules (whole record)` = num(sum(rem[column == v, N]))) }))
  }, striped = FALSE, bordered = FALSE, width = "100%", align = "l", na = "")
  output$ex_dl <- downloadHandler(
    filename = function() paste0(safe_name(paste(ex_key(), collapse = "__")), "_", Sys.Date(), ".csv"),
    content = function(f) { d <- ex_view()
      fwrite(d[, c(intersect(META_COLS, names(d)), intersect(input$ex_params, names(d))), with = FALSE], f, dateTimeAs = "write.csv") })

