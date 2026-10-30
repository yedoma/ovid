# CS/pages/precipitation.R  --  Precipitation.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Precipitation ------------------------------------------------------
  pr_sum <- reactive({ o <- outs(); if (is.null(o) || is.null(o$precip) || !nrow(o$precip)) NULL else o$precip })
  output$pr_empty <- renderUI(if (is.null(pr_sum())) empty_msg(if (is.null(outs())) "Nothing to show yet. Press Run pipeline at the top right."
    else "No weighing gauge was recognised in the last run. Add one under Gauge set-up below, then press Reprocess."))
  observe({ s <- pr_sum(); req(s)
    ch <- setNames(paste(s$station, s$stream, sep = "|||"), paste0(s$station, " / ", s$stream, "   (", ifelse(s$gauge_type == "geonor", "Geonor", "Pluvio2"), ")"))
    cur <- isolate(input$pr_gauge); updateSelectInput(session, "pr_gauge", choices = ch, selected = if (isTRUE(cur %in% ch)) cur else ch[[1]]) })
  pr_key <- reactive({ req(input$pr_gauge, pr_sum()); strsplit(input$pr_gauge, "|||", fixed = TRUE)[[1]] })
  pr_info <- reactive({ k <- pr_key(); pr_sum()[station == k[1] & stream == k[2]][1] })
  pr_file <- function(kind) file.path(pd(), "output", "precip", paste0(safe_name(paste(pr_key(), collapse = "__")), "_", kind, ".csv"))
  # safe: an output table that cannot be read shows as empty until the next run writes it
  pr_read <- function(kind, ...) { rv$tick; f <- pr_file(kind); if (!file.exists(f)) return(data.table()); tryCatch(fread(file = f, ...), error = function(e) data.table()) }
  pr_daily <- reactive({ d <- pr_read("daily", colClasses = list(character = "Date")); req(nrow(d) > 0)
    validate(need(all(c("Date", "precip_mm") %in% names(d)), paste0("The daily precipitation table for this gauge is not in the expected form (", basename(pr_file("daily")), " has columns: ", paste(utils::head(names(d), 6), collapse = ", "), "). Press Reprocess precipitation under Gauge set-up.")))
    d[, Date := as.Date(Date)]; d <- d[!is.na(Date)]; req(nrow(d) > 0); d })
  pr_iv <- reactive({ d <- pr_read("interval"); req(nrow(d) > 0); validate(need("TIMESTAMP" %in% names(d), "The record-level precipitation table for this gauge could not be read.")); if (!inherits(d$TIMESTAMP, "POSIXct")) d[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]; d })
  pr_faults <- reactive({ d <- pr_read("faulty_periods", colClasses = "character"); if (nrow(d)) d[, `:=`(Start = as.Date(Start), End = as.Date(End))]; d })
  # safe: req() leaves the dates as they are
  observe({ d <- tryCatch(pr_daily(), error = function(e) NULL); req(d); r <- range(d$Date); updateDateRangeInput(session, "pr_dates", start = r[1], end = r[2], min = r[1], max = r[2]) })
  pr_win <- reactive({ req(input$pr_dates[1], input$pr_dates[2]); input$pr_dates })
  pr_names <- c(precip_mm = "Final", precip_acc_total_mm = "Running total (raw)", precip_bucket_mm = "Bucket cummax", precip_corrected_mm = "Wind-corrected")
  pr_cols <- setNames(param_colours(c("precip_final", "precip_raw", "precip_bucket", "precip_corrected"), GB), c("Final", "Running total (raw)", "Bucket cummax", "Wind-corrected"))   # four shades of the precipitation blue
  pr_sel <- reactive({ s <- input$pr_series; if (!isTRUE(pr_info()$has_acc)) s <- setdiff(s, "precip_acc_total_mm")
    s <- intersect(s, names(pr_daily())); if (!length(s)) "precip_mm" else s })
  output$pr_cards <- renderUI({
    i <- pr_info(); d <- pr_daily(); w <- pr_win(); d <- d[Date >= w[1] & Date <= w[2]]
    card <- function(v, l) div(class = "stat", tags$b(v), span(l))
    div(class = "stats", card(paste0(num(round(sum(d$precip_mm, na.rm = TRUE), 1)), " mm"), "final total in the period"),
        card(num(sum(d$precip_mm > 0, na.rm = TRUE)), "days with precipitation"),
        card(paste0(round(max(c(0, d$precip_mm), na.rm = TRUE), 1), " mm"), "wettest day"),
        card(num(sum(d$qc_flag == "missing")), "days with no records"),
        card(num(sum(d$qc_flag == "low_confidence")), "low-confidence days"),
        card(paste0(i$emptied, " / ", i$topped_up), "times emptied / topped up"),
        card(num(i$out_of_range + i$spikes), "bucket readings rejected"),
        if (isTRUE(i$bucket_only)) card(num(sum(d$qc_flag == "bucket_only_window")), "days from the bucket-only window (bucket only)"),
        if (isTRUE(i$has_acc)) card(num(sum(d$qc_flag %in% c("servicing_day", "power_or_outage_day", "acc_jump"))), "single days taken from the bucket"),
        if ("precip_corrected_mm" %in% names(d)) card(paste0(num(round(sum(d$precip_corrected_mm, na.rm = TRUE), 1)), " mm"), "wind-corrected total"))
  })
  output$pr_plot_ui <- renderUI({ k <- input$pr_chart %||% "daily"; n <- if (k %in% c("daily", "interval")) max(1, length(tryCatch(pr_sel(), error = function(e) 1))) else 1
    plotOutput("pr_plot", height = paste0(round((if (k %in% c("daily", "interval")) 130 + 260 * n else 520) * psz()), "px")) })
  pr_g <- reactive({
    validate(need(!is.null(pr_sum()), "")); d <- pr_daily(); w <- pr_win(); kind <- input$pr_chart; s <- pr_sel(); i <- pr_info()
    d <- d[Date >= w[1] & Date <= w[2]]; validate(need(nrow(d) > 0, "No days in this period."))
    ttl <- paste0(paste(pr_key(), collapse = " / "), "   ", if (i$gauge_type == "geonor") "Geonor T-200B" else "OTT Pluvio2")
    fl <- pr_faults(); if (nrow(fl)) fl <- fl[End >= w[1] & Start <= w[2]]
    shade <- function(g, posix = FALSE) if (!nrow(fl)) g else g + annotate("rect", xmin = if (posix) as.POSIXct(format(fl$Start), tz = "UTC") else fl$Start,
                xmax = if (posix) as.POSIXct(format(fl$End), tz = "UTC") + 86400 else fl$End + 1, ymin = -Inf, ymax = Inf, fill = GB$red, alpha = 0.13)
    cap <- if (nrow(fl)) "red shading = period where the running total was inflated; the daily total comes from the bucket (low confidence)" else NULL
    long <- function(x, id) { m <- melt(x[, c(id, s), with = FALSE], id.vars = id, variable.name = "Series"); m[, Series := factor(pr_names[as.character(Series)], levels = pr_names[s])]; m }
    sc <- list(scale_fill_manual(values = pr_cols), scale_colour_manual(values = pr_cols))
    if (kind == "daily") { m <- long(d, "Date")[!is.na(value)]
      return(shade(ggplot(m, aes(Date, value, fill = Series))) + geom_col(width = 1) + facet_wrap(~Series, ncol = 1) + sc + guides(fill = "none") +
               labs(x = NULL, y = "mm per day", subtitle = ttl, caption = cap)) }
    if (kind == "interval") {
      validate(need(as.numeric(diff(w)) <= 370, "Every record is drawn for periods of a year or less. Narrow the period."))
      v <- pr_iv()[TIMESTAMP >= as.POSIXct(format(w[1]), tz = "UTC") & TIMESTAMP < as.POSIXct(format(w[2] + 1), tz = "UTC")]
      m <- long(v, "TIMESTAMP")[!is.na(value) & value != 0]; validate(need(nrow(m) > 0, "No precipitation in this period."))
      return(shade(ggplot(m, aes(TIMESTAMP, value, colour = Series)), TRUE) + geom_linerange(aes(ymin = 0, ymax = value), linewidth = 0.4) +
               facet_wrap(~Series, ncol = 1) + sc + guides(colour = "none") + labs(x = NULL, y = paste0("mm per ", i$interval_min, " min"), subtitle = ttl, caption = cap)) }
    if (kind == "cum") { m <- long(d, "Date"); m[is.na(value), value := 0]; m[, value := cumsum(value), by = Series]
      return(shade(ggplot(m, aes(Date, value, colour = Series))) + geom_line(linewidth = 0.6) + sc +
               labs(x = NULL, y = "mm since the start of the period", subtitle = ttl, caption = cap)) }
    if (kind %in% c("month", "year")) {
      d[, P := if (kind == "month") as.Date(format(Date, "%Y-%m-01")) else as.Date(sprintf("%d-01-01", year(Date)))]
      m <- long(d, "P")[, .(value = sum(value, na.rm = TRUE)), by = .(P, Series)]
      g <- ggplot(m, aes(P, value, fill = Series)) + geom_col(position = "dodge") + sc + labs(x = NULL, y = paste("mm per", kind), subtitle = ttl,
                 caption = "Months and years with missing days are under-counted; see the By year table.")
      if (kind == "year") g <- g + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
          geom_text(aes(label = round(value)), position = position_dodge(width = 365), vjust = -0.4, size = 2.8, colour = GB$fg4)
      return(g) }
    if (kind == "cumyear") { m <- d[, .(Date, value = fifelse(is.na(get(s[1])), 0, get(s[1])))]
      m[, `:=`(Year = factor(year(Date)), doy = as.Date("2000-01-01") + yday(Date) - 1)]; m[, value := cumsum(value), by = Year]
      cols <- grDevices::colorRampPalette(c(GB$blue, GB$aqua, GB$yellow, GB$orange, GB$red))(nlevels(m$Year))
      return(ggplot(m, aes(doy, value, colour = Year, group = Year)) + geom_line(linewidth = 0.6) + scale_colour_manual(values = cols) +
               scale_x_date(date_labels = "%b", date_breaks = "1 month") + labs(x = NULL, y = "mm since 1 January", subtitle = paste0(ttl, "   ", pr_names[s[1]]),
                 caption = "A flat stretch is either a dry spell or missing data.")) }
    if (kind == "bucket") {
      v <- pr_iv()[TIMESTAMP >= as.POSIXct(format(w[1]), tz = "UTC") & TIMESTAMP < as.POSIXct(format(w[2] + 1), tz = "UTC")]
      if (nrow(v) > 30000) v <- v[, .(TIMESTAMP = TIMESTAMP[1], bucket = suppressWarnings(stats::median(bucket, na.rm = TRUE)), bucket_filled = suppressWarnings(max(bucket_filled, na.rm = TRUE))), by = .(day = as.Date(TIMESTAMP))]
      v[!is.finite(bucket_filled), bucket_filled := NA]; v[!is.na(bucket) & (bucket < -5 | bucket > 1500), bucket := NA]
      sv <- pr_read("servicing"); if (nrow(sv)) { sv[, t := as.POSIXct(Time, tz = "UTC")]; sv <- sv[as.Date(t) >= w[1] & as.Date(t) <= w[2]] }
      g <- ggplot(v, aes(TIMESTAMP)) + geom_line(aes(y = bucket, colour = "Bucket as logged"), linewidth = 0.4, na.rm = TRUE) +
        geom_line(aes(y = bucket_filled, colour = "Bucket used (never falls within a cycle)"), linewidth = 0.4, na.rm = TRUE) +
        scale_colour_manual(values = c(`Bucket as logged` = pr_cols[["Running total (raw)"]], `Bucket used (never falls within a cycle)` = pr_cols[["Final"]]), name = NULL) +
        labs(x = NULL, y = "mm in the bucket", subtitle = ttl, caption = "dashed lines = bucket emptied (red) or topped up (gold); each starts a new cycle")
      if (nrow(sv)) g <- g + geom_vline(data = sv, aes(xintercept = t, linetype = Event), colour = ifelse(sv$Event == "Emptied", GB$red, GB$yellow), linewidth = 0.3) +
          scale_linetype_manual(values = c(Emptied = "dashed", `Topped up` = "dashed", `Entered by hand` = "dotted"), guide = "none")
      return(shade(g, TRUE)) }
    if (kind == "diff") { validate(need(isTRUE(i$has_acc), "This gauge has no running total to compare with the bucket."))
      d[, diff := precip_acc_total_mm - precip_bucket_mm]
      return(shade(ggplot(d[!is.na(diff)], aes(Date, diff))) + geom_col(width = 1, fill = GB$blue) + geom_hline(yintercept = 2, colour = GB$yellow, linetype = "dashed", linewidth = 0.4) +
               labs(x = NULL, y = "running total minus bucket, mm per day", subtitle = ttl,
                    caption = "Above the dashed line the running total reports more than the bucket gained. Runs of such days become a faulty period (red).")) }
    d[, M := as.Date(format(Date, "%Y-%m-01"))]; m <- d[, .N, by = .(M, Flag = qc_flag)]
    ggplot(m, aes(M, N, fill = Flag)) + geom_col(width = 28) +
      scale_fill_manual(values = c(good = GB$aqua, partial_day = GB$yellow, acc_bucket_mismatch = GB$orange, low_confidence = GB$red, missing = GB$bg2,
                                   bucket_only_window = GB$green, servicing_day = GB$blue, power_or_outage_day = GB$purple, acc_jump = GB$yellow)) +
      labs(x = NULL, y = "days", subtitle = ttl)
  })
  output$pr_plot <- renderPlot(pr_g(), bg = GB$bg, res = 96)
  pr_tbl <- function(kind, empty) renderDT({ validate(need(!is.null(pr_sum()), "")); d <- pr_read(kind); validate(need(nrow(d) > 0, empty)); ovid_dt(d, page = 15, opts = list(scrollX = TRUE)) })
  output$pr_yearly <- pr_tbl("yearly", "No yearly totals."); output$pr_gaps <- pr_tbl("gaps", "No gaps in the record.")
  output$pr_serv <- pr_tbl("servicing", "No servicing was detected."); output$pr_cycles <- pr_tbl("cycles", "No bucket cycles.")
  output$pr_events <- pr_tbl("events", "No events."); output$pr_wy <- pr_tbl("water_year", "No water-year totals. Run the pipeline or press Reprocess.")
  pr_dl_kind <- reactive(if (isTRUE(input$pr_chart %in% c("interval", "bucket"))) "interval" else "daily")   # the table behind the chart shown
  output$pr_dl <- downloadHandler(filename = function() paste0(safe_name(paste(pr_key(), collapse = "__")), "_precip_", pr_dl_kind(), ".csv"),
                                  content = function(f) file.copy(pr_file(pr_dl_kind()), f))
  output$pr_method <- renderUI({ req(pr_sum()); i <- pr_info(); ge <- i$gauge_type == "geonor"
    div(class = "explain", tags$ol(
      tags$li("Records are placed on a regular ", i$interval_min, "-minute grid. Records off the grid are left out (", num(i$off_grid_dropped), " here). The cleaned station table itself is not changed; these are derived tables in ", code("output/precip"), "."),
      tags$li("Bucket readings that cannot be real are set aside: below \u22125 mm or above the capacity (overflow codes such as 6999 and 99999), and one- or two-record spikes more than the spike limit away from the readings around them. ",
              num(i$out_of_range), " out of range and ", num(i$spikes), " spikes here.", if (!ge) paste0(" Records with a non-zero status code are also set aside (", num(i$status_errors), ").")),
      tags$li("A drop in the bucket of more than ", i$service_step_mm, " mm is read as the bucket being emptied; a rise of more than that in one record as a top-up (antifreeze, oil). Each starts a new bucket cycle and contributes no precipitation."),
      tags$li("Within a cycle the bucket can only gain: evaporation and noise are removed by taking the running maximum, and precipitation is the increase in that maximum."),
      if (ge) tags$li("Geonor: the vibrating-wire signal is noisy, so the bucket is first smoothed with a ", i$smooth_records, "-record running median and precipitation is only booked in steps of ", i$noise_mm, " mm. These two settings trade sensitivity to light snowfall against false precipitation from wind and temperature.")
      else tags$li("Pluvio2: the logger's running total (", code("acc_total"), ") is differenced, allowing for its roll-over and for resets to zero after a power loss. It is the final series wherever it agrees with the bucket."),
      if (!ge) tags$li("Where the running total reports more than the bucket gained, by more than the mismatch limit a day, for a run of days, the period is marked faulty and the daily total is taken from the bucket instead (low confidence). ", i$faulty_periods, " such periods here."),
      if (!ge) tags$li("Single days: emptying or refilling the bucket, a power loss or an outage can make the running total jump while the bucket gains nothing. On a mismatch day with servicing, a power reset, an outage or a status error that day or the day either side, and on any day where the running total exceeds the bucket by more than ", code("jump_mm"),
                       ", the bucket is used for that day. ", num(i$single_days_from_bucket %||% 0), " such days here; each is listed under Events."),
      if (!ge && isTRUE(i$bucket_only)) tags$li(tags$b("Bucket-only window: "), "from ", substr(i$bucket_only_start, 1, 16), " to ", substr(i$bucket_only_end, 1, 10), " the running total is not used at all. Every record and day in that period comes from the bucket (running maximum within each fill cycle, differenced), and is flagged ",
                                        code("bucket_only_window"), ". Emptying and refilling the bucket are servicing events and add nothing."),
      if (!ge) tags$li("Final series for this gauge: ", tags$b(if (identical(i$final_source, "bucket")) "bucket cummax throughout" else "running total where healthy, bucket cummax where it fails"),
                       ". Change it under Gauge set-up."),
      tags$li("Wind correction (off unless ", code("undercatch"), " is set to ", code("single_alter"), " or ", code("unshielded"), " in the gauge set-up): each record is divided by a collection efficiency CE = exp(\u2212a\u00b7U\u00b7(1 \u2212 atan(b\u00b7T) + c)), the form of Kochendorfer et al. (2017), using wind speed and air temperature from the station named in ",
              code("wind_station"), ". The uncorrected series is always kept. Check the coefficients against the paper before relying on the corrected totals. With ", code("smith2007"),
              " (an Alter-shielded Geonor, wind at gauge height), snow uses CE = 1.18\u00b7exp(\u22120.18\u00b7U) of Smith (2007) and rain CE = 0.95, as in Pan et al. (2016); it also needs relative humidity (",
              code("rh"), ") for the phase."),
      tags$li("Phase (Snow, Mixed, Rain): from the hydrometeor temperature, which uses air temperature and relative humidity (Harder and Pomeroy 2013), and its rain fraction; below 0.05 is snow and above 0.95 rain. ",
              "Without humidity, snow below \u22122 \u00b0C and rain above 2 \u00b0C, an Ovid default with no published source."),
      tags$li("After an outage longer than ", i$long_gap_hours, " hours the first record is not counted. What the bucket gained across the outage is reported separately as outage accumulation, not added to the totals.")),
      p(class = "dim", "Every limit above is a column in the Gauge set-up table. Change it, save, and press Reprocess. The limits are Ovid's own quality rules (defaults chosen for these gauges), not published values; the Geonor booking step follows WMO-SPICE practice.")) })
  # gauge set-up
  observe({ o <- outs(); req(o, nrow(o$stations) > 0); updateSelectInput(session, "pg_station", choices = setNames(st_value(o), st_label(o))) })
  observe({ o <- outs(); req(o, input$pg_station); k <- strsplit(input$pg_station, "|||", fixed = TRUE)[[1]]
    p <- if (all(c("station", "stream", "parameter") %in% names(o$pstats))) o$pstats[station == k[1] & stream == k[2], parameter] else character()
    # safe: a parameter list for choosing only
    if (!length(p)) p <- tryCatch(setdiff(names(load_station(k[1], k[2], pd())), META_COLS), error = function(e) character())
    def <- GAUGE_DEFAULTS[[input$pg_type]]
    pick <- function(want, rx) { if (nzchar(want) && want %in% p) want else { m <- grep(rx, p, ignore.case = TRUE, value = TRUE); if (length(m)) m[1] else "" } }
    updateSelectInput(session, "pg_bucket", choices = p, selected = pick(def[["bucket_col"]], "bucket|GN_mm|geonor|precip|depth"))
    updateSelectInput(session, "pg_acc", choices = c("(none)" = "", p), selected = if (input$pg_type == "geonor") "" else pick(def[["acc_col"]], "acc"))
    updateSelectInput(session, "pg_status", choices = c("(none)" = "", p), selected = if (input$pg_type == "geonor") "" else pick(def[["status_col"]], "status|Result9")) })
  observeEvent(input$pg_add, { req(input$pg_station, nzchar(input$pg_bucket)); k <- strsplit(input$pg_station, "|||", fixed = TRUE)[[1]]
    row <- c(list(station = k[1], stream = k[2], gauge_type = input$pg_type), as.list(GAUGE_DEFAULTS[[input$pg_type]]))
    row$bucket_col <- input$pg_bucket; row$acc_col <- input$pg_acc; row$status_col <- input$pg_status
    if (is.null(m_pg$data())) { dir.create(dirname(cfg_path("precip_gauges.csv")()), recursive = TRUE, showWarnings = FALSE)
      fwrite_safe(as.data.table(setNames(rep(list(character()), length(PRECIP_COLS)), PRECIP_COLS)), cfg_path("precip_gauges.csv")()); rv$tick <- rv$tick + 1
      return(note("The gauge table was just created. Press Add once more.")) }
    if (m_pg$append(as.data.frame(row, stringsAsFactors = FALSE))) note("Gauge added. Press Reprocess precipitation to see it.") })
  pr_reprocess <- function() { if (VIEWER) return(); m_pg$save(quiet = TRUE); f <- cfg_path("precip_gauges.csv")()
    if (!file.exists(f)) return(note("Add a gauge first.", type = "warning"))
    r <- safely(withProgress(message = "Processing gauges", value = 0.5, { AUDIT$problems <- list()
      list(s = process_precip(list(project_dir = pd()), ensure_cols(read_cfg(f), PRECIP_COLS), if (is.null(m_fl$data())) NULL else ensure_cols(as.data.table(m_fl$data()), LOG_COLS)),
           p = rbindlist(AUDIT$problems)) }), "Precipitation")
    if (is.null(r)) return(); rv$tick <- rv$tick + 1
    if (nrow(r$p)) note(paste(r$p$where, r$p$problem, sep = ": ", collapse = "\n"), type = "warning", duration = NULL)
    else note(nrow(r$s), " gauge(s) processed.") }
  observeEvent(input$pr_rerun, { on.exit(once_done("pr_rerun"))                 # one click, one reprocess (core shell.R)
    if (run_busy()) return(note("A run is going: reprocess when it ends.", type = "warning")); pr_reprocess() })
