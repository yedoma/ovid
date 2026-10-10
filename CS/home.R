# CS/home.R  --  The Standard view's home for CS: before import, three
# ways in; with data, the station picked on the strip: info cards, the main
# plot (air temperature with the mean of the other stations as its own line, other parameters as chips, Smoothing in the
# view bar, display only), two small plots, the station panel and the daily summary table.
# Part of Ovid Core // CS. Sourced by app.R. Only reads outputs and set-up tables; it never changes them.

# The first column of a record whose name matches one of the patterns, in their order (exact names first).
cs_home_pick <- function(nm, rx) { for (r in rx) { h <- grep(r, nm, value = TRUE, ignore.case = TRUE); if (length(h)) return(h[1]) }; NULL }
CS_HOME_RX <- list(air = c("^air_temp$", "^air_temp(_avg)?$", "^air_temp"), rh = c("^rh$", "^rh(_avg)?$", "^rh"),
                   wind = c("^wind_speed$", "^wind_speed(_avg)?$", "^wind_speed"), gust = c("^wind_speed_max$", "^wind_gust", "^wind_speed_max"),
                   rn = c("^net_rad$", "^net_rad", "^rn$"))

# What a scan of the data folders will show, by file type, from the first line of each file: found, readable, matched to
# a station (a logger already in stations.csv), new since the last run and changed in size (against the run's manifest),
# and notes. Other files are listed, never read. Shown with the folders (Project, and Import data in the Workbench).
cs_scan_types <- function(dirs, stations = NULL, max_files = 4000, manifest = NULL) {
  dirs <- dirs[nzchar(dirs) & dir.exists(path.expand(dirs))]
  types <- c("Campbell TOA5", "Campbell TOB3", "CR10X (no header)", "Other files")
  if (!length(dirs)) return(data.table(`File type` = types, Found = "\u2014", Readable = "\u2014", `Matched to station` = "\u2014", New = "\u2014", `Changed size` = "\u2014",
                                       Notes = c("Waiting for a folder", "", "", "Listed, never read")))
  f <- utils::head(unlist(lapply(path.expand(dirs), list.files, recursive = TRUE, full.names = TRUE, all.files = FALSE)), max_files)
  f <- f[!grepl("(^|/)\\._", f)]
  head1 <- vapply(f, function(p) tryCatch(paste(readLines(p, n = 1L, warn = FALSE), collapse = ""), error = function(e) NA_character_), "", USE.NAMES = FALSE)
  ty <- ifelse(is.na(head1), "Other files", ifelse(grepl("^\"?TOA5", head1), "Campbell TOA5", ifelse(grepl("^\"?TOB3", head1), "Campbell TOB3",
        ifelse(grepl("\\.dat$", f, ignore.case = TRUE) & grepl("^[0-9]+,", head1), "CR10X (no header)", "Other files"))))
  lid <- ifelse(ty %in% c("Campbell TOA5", "Campbell TOB3"), vapply(strsplit(gsub("\"", "", head1), ","), function(x) if (length(x) >= 4) trim(x[4]) else "", ""), "")
  known <- if (!is.null(stations) && NROW(stations) && "logger_id" %in% names(stations)) trim(as.character(stations$logger_id)) else character()
  m <- if (!is.null(manifest) && NROW(manifest) && all(c("path", "size") %in% names(manifest))) manifest else data.table(path = character(), size = numeric())
  i <- match(normalizePath(f, winslash = "/", mustWork = FALSE), normalizePath(as.character(m$path), winslash = "/", mustWork = FALSE))
  is_new <- is.na(i); changed <- !is.na(i) & suppressWarnings(as.numeric(m$size[i])) != file.size(f); changed[is.na(changed)] <- FALSE
  out <- rbindlist(lapply(types, function(t) { k <- ty == t; n <- sum(k); read <- t != "Other files"
    data.table(`File type` = t, Found = format(n, big.mark = ","), Readable = if (read) format(sum(k & !is.na(head1)), big.mark = ",") else "\u2014",
               `Matched to station` = if (t %in% c("Campbell TOA5", "Campbell TOB3")) format(sum(k & lid %in% known & nzchar(lid)), big.mark = ",") else "\u2014",
               New = if (read) format(sum(k & is_new), big.mark = ",") else "\u2014", `Changed size` = if (read) format(sum(k & changed), big.mark = ",") else "\u2014",
               Notes = if (t == "Other files") "Listed, never read" else if (t == "CR10X (no header)") "Read when CR10X files are switched on (Project)" else if (!n) "" else "") }))
  if (length(f) >= max_files) out[nrow(out), Notes := paste0("Listed, never read; the first ", max_files, " files were looked at")]
  out
}

cs_home_ui <- function() tagList(tags$style(HTML(CS_HOME_CSS)),
  conditionalPanel("!output.has_out", div(class = "hm-before",
    div(class = "hm-three",
      div(class = "d-cardx hm-choice", tags$b("Choose a data folder"), p(class = "dim", "Campbell TOA5 and TOB3 .dat files, and CR10X mixed-array files. Nothing is changed in your folder."),
          tags$a(class = "btn btn-default", href = "#", `data-ovid-go` = "Project", icon("folder-open"), " Choose folder\u2026"),
          div(class = "dim hm-drop", "or put the files in a folder and choose it: every sub-folder is read")),
      div(class = "d-cardx hm-choice", tags$b("Bring your own metadata"), p(class = "dim", "A CSV or Excel sheet of deployments (serial, station, dates). Import it into the Deployment sheet on Stations. You say which column is which; the file is only read."),
          tags$a(class = "btn btn-default", href = "#", `data-ovid-go` = "Stations", icon("table-list"), " Import a table\u2026")),
      div(class = "d-cardx hm-choice", tags$b("Open a project"), uiOutput("hm_recent"), tags$a(class = "btn btn-default", href = "#", `data-ovid-go` = "Project", "Open\u2026"))))),
  conditionalPanel("output.has_out", div(class = "hm",
    uiOutput("hm_cards", class = "hm-cards"),
    div(class = "hm-grid",
      div(class = "hm-main",
        div(class = "d-cardx", div(class = "hm-chips", span(class = "dim", "Parameters"), uiOutput("hm_chip_ui", inline = TRUE)), ts_viewbar(), ts_view("hm_plot", 230),
            uiOutput("hm_small", class = "hm-small")),
        div(class = "d-cardx", div(class = "hm-thead", h4("Daily summary"), span(class = "dim", textOutput("hm_daily_note", inline = TRUE)), div(class = "d-fill"),
            downloadButton("hm_daily_dl", "Export", class = "btn-sm")), DTOutput("hm_daily"))),
      div(class = "hm-side d-cardx", uiOutput("hm_station"))))))

CS_HOME_CSS <- paste(readLines(file.path(APP_DIR, "www", "home.css"), encoding = "UTF-8"), collapse = "\n")

# cur(): the station picked on the strip; st_shown(): the stations shown; settings(): the app's settings (raw folders, projects).
cs_home_server <- function(input, output, session, outs, pd, cur, st_shown, E, settings) {
  output$hm_recent <- renderUI({ s <- settings(); r <- tryCatch(projects_known(s), error = function(e) character())
    if (!length(r)) return(p(class = "dim", "No projects yet."))
    p(class = "dim", "Recent: ", paste(utils::head(basename(r), 4), collapse = " \u00b7 ")) })

  st <- reactive({ s <- cur(); if (is.null(s) || !nzchar(s)) { sh <- st_shown(); if (length(sh)) sh[1] else NULL } else s })
  tab <- reactive({ o <- outs(); s <- st(); req(o, s); x <- o$stations[station == s]; req(nrow(x))
    hits <- vapply(seq_len(nrow(x)), function(i) { m <- o$pstats; if (is.null(m) || !nrow(m)) return(0L); sum(grepl(CS_HOME_RX$air[3], m[station == s & stream == x$stream[i]]$parameter)) }, 0L)
    x[order(-hits, -records)][1] })
  dat <- reactive({ k <- tab(); d <- tryCatch(load_station(k$station, k$stream, pd()), error = function(e) NULL); req(!is.null(d), nrow(d)); d })
  cols <- reactive({ d <- dat(); nm <- setdiff(names(d)[vapply(d, is.numeric, NA)], c(WB_META, "RECORD")); lapply(CS_HOME_RX, function(rx) cs_home_pick(nm, rx)) })
  # the mean of the same parameter at the other shown stations, hour by hour, at this record's times
  nbr <- reactive({ k <- tab(); a <- cols()$air; if (is.null(a)) return(NULL); o <- outs(); others <- setdiff(st_shown(), k$station); if (!length(others)) return(NULL)
    xs <- rbindlist(lapply(others, function(s) { y <- o$stations[station == s][order(-records)]; if (!nrow(y)) return(NULL)
      d <- tryCatch(load_station(s, y$stream[1], pd()), error = function(e) NULL); if (is.null(d)) return(NULL); p <- cs_home_pick(names(d), CS_HOME_RX$air); if (is.null(p)) return(NULL)
      data.table(h = floor(as.numeric(d$TIMESTAMP) / 3600), v = as.numeric(d[[p]]), s = s)[!is.na(v)][, .(v = mean(v)), by = .(h, s)] }), fill = TRUE)
    if (!nrow(xs)) return(NULL); m <- xs[, .(v = mean(v), n = uniqueN(s)), by = h]
    list(v = m$v[match(floor(as.numeric(dat()$TIMESTAMP) / 3600), m$h)], n = uniqueN(xs$s)) })
  last30 <- reactive({ d <- dat(); t1 <- max(d$TIMESTAMP, na.rm = TRUE); d[TIMESTAMP > t1 - 30 * 86400] })

  output$hm_cards <- renderUI({ w <- last30(); cc <- cols(); o <- outs(); k <- tab(); f1 <- function(x, d = 1) formatC(x, format = "f", digits = d)
    st <- function(p) { if (is.null(p)) return(NULL); v <- as.numeric(w[[p]]); v <- v[!is.na(v)]; if (!length(v)) NULL else list(m = mean(v), lo = min(v), hi = max(v)) }
    a <- st(cc$air); r <- st(cc$rh); ws <- st(cc$wind); g <- st(cc$gust); n <- st(cc$rn)
    m <- if (nrow(o$meta)) o$meta[station == k$station & stream == k$stream] else data.table(); fl <- if (nrow(o$flags)) o$flags[station == k$station & stream == k$stream] else data.table()
    sub30 <- paste("last 30 days to", format(max(w$TIMESTAMP), "%Y-%m-%d"))
    tagList(
      if (!is.null(a)) ovid_kpi("Air temperature", f1(a$m), "\u00b0C", paste0("min ", f1(a$lo), " \u00b7 max ", f1(a$hi))),
      if (!is.null(r)) ovid_kpi("Relative humidity", f1(r$m, 0), "%", paste0("min ", f1(r$lo, 0), " \u00b7 max ", f1(r$hi, 0))),
      if (!is.null(ws)) ovid_kpi("Wind speed", f1(ws$m), "m/s", paste0("gust max ", f1((g %||% ws)$hi))),
      if (!is.null(n)) ovid_kpi("Net radiation", f1(n$m, 0), "W/m\u00b2", paste0("peak ", f1(n$hi, 0))),
      ovid_kpi("Records", num(k$records), NULL, paste0(if (nrow(m) && "complete_pct" %in% names(m)) paste0(m$complete_pct[1], " % complete \u00b7 ") else "", num(sum(as.numeric(fl$records %||% 0))), " flagged"),
                 level = if (nrow(fl)) "warn"),
      div(class = "dim", style = "grid-column:1/-1;font-size:12px", paste0(k$station, " \u00b7 ", k$stream, " \u00b7 cards: ", sub30))) })

  output$hm_small <- renderUI({ cc <- cols(); tagList(if (!is.null(cc$rh)) div(ts_view("hm_rh", 110)), if (!is.null(cc$rn)) div(ts_view("hm_rn", 110)),
    if (is.null(cc$rh) && is.null(cc$rn)) div(class = "dim", "This table has no humidity or net radiation."), tags$script(HTML("Shiny.setInputValue('hm_small_on', Date.now())"))) })
  chips <- reactive({ cc <- cols(); x <- c(cc$rh, cc$wind, cc$rn); x[!vapply(x, is.null, NA)] })
  output$hm_chip_ui <- renderUI({ cc <- cols(); ex <- unlist(chips())
    tagList(if (!is.null(cc$air)) span(class = "badge", style = "margin-right:6px", paste(cc$air, "+ neighbour mean")),
            checkboxGroupInput("hm_chips", NULL, choices = ex, selected = ex, inline = TRUE)) })
  sendp <- function(id, series) { d <- dat(); wb <- list(key = "station", value = tab()$station, params = vapply(series, `[[`, "", "key"))
    session$onFlushed(function() ts_view_send(session, id, d$TIMESTAMP, series, zone = "logger time", wb = wb), once = TRUE) }
  ser <- function(p, unit = "", name = p, ref = NULL) list(name = name, key = p, unit = unit, v = as.numeric(dat()[[p]]), ref = ref)
  observe({ req(identical(input$nav, "Home")); d <- dat(); cc <- cols(); nb <- nbr()
    s <- list(); if (!is.null(cc$air)) s[[1]] <- ser(cc$air, "\u00b0C", "Air temperature", if (!is.null(nb)) list(name = paste0("mean of ", nb$n, " other station", if (nb$n > 1) "s"), v = nb$v))
    for (p in intersect(input$hm_chips %||% character(), names(d))) s[[length(s) + 1]] <- ser(p, if (identical(p, cc$rh)) "%" else if (identical(p, cc$wind)) "m/s" else if (identical(p, cc$rn)) "W/m\u00b2" else "")
    if (!length(s)) { nm <- setdiff(names(d)[vapply(d, is.numeric, NA)], c(WB_META, "RECORD")); if (length(nm)) s[[1]] <- ser(nm[1]) }
    sendp("hm_plot", s)
    req(!is.null(input$hm_small_on))
    if (!is.null(cc$rh)) sendp("hm_rh", list(ser(cc$rh, "%", "Relative humidity")))
    if (!is.null(cc$rn)) sendp("hm_rn", list(ser(cc$rn, "W/m\u00b2", "Net radiation"))) })

  daily <- reactive({ d <- dat(); cc <- cols(); k <- tab(); req(nrow(d))
    x <- data.table(Day = as.Date(d$TIMESTAMP)); add <- function(p, nm) if (!is.null(p)) x[, (nm) := as.numeric(d[[p]])]
    add(cc$air, "a"); add(cc$rh, "r"); add(cc$wind, "w"); add(cc$rn, "n")
    s <- function(v, f, dg) { v <- v[!is.na(v)]; if (length(v)) round(f(v), dg) else NA_real_ }
    out <- x[, c(list(Records = .N),
                 if ("a" %in% names(x)) list(a_mean = s(a, mean, 1), a_min = s(a, min, 1), a_max = s(a, max, 1)),
                 if ("r" %in% names(x)) list(r_mean = s(r, mean, 0)), if ("w" %in% names(x)) list(w_mean = s(w, mean, 1)),
                 if ("n" %in% names(x)) list(n_mj = round(s(n, mean, 3) * 0.0864, 2))), by = Day]    # mean W/m2 x 86400 s / 1e6 = MJ/m2 per day
    fl <- outs()$flags; fl <- if (nrow(fl)) fl[station == k$station & stream == k$stream] else data.table()
    fd <- if (nrow(fl)) fl[, .(day = seq(as.Date(substr(start, 1, 10)), as.Date(substr(end, 1, 10)), by = "day")), by = seq_len(nrow(fl))][, .N, by = day] else data.table(day = as.Date(character()), N = integer())
    out[, `QA/QC` := { n <- fd$N[match(Day, fd$day)]; ifelse(is.na(n), "ok", paste(n, "flagged")) }]
    setnames(out, c("a_mean", "a_min", "a_max", "r_mean", "w_mean", "n_mj"), c("Mean \u00b0C", "Min \u00b0C", "Max \u00b0C", "RH %", "Wind m/s", "Rn MJ/m\u00b2"), skip_absent = TRUE)
    out[order(-Day)] })
  output$hm_daily_note <- renderText("from the station record; Rn as daily energy: mean W/m\u00b2 \u00d7 0.0864 = MJ/m\u00b2")
  output$hm_daily <- renderDT(DT::datatable(daily(), rownames = FALSE, selection = "none", extensions = "Buttons",
    options = list(pageLength = 10, scrollX = TRUE, dom = "Bftip", buttons = list(list(extend = "colvis", text = "Columns")), language = list(search = "Find:"))), server = FALSE)
  output$hm_daily_dl <- downloadHandler(function() paste0("cs_daily_", safe_name(tab()$station), ".csv"), function(file) fwrite(daily(), file))

  output$hm_station <- renderUI({ k <- tab(); o <- outs(); m <- if (nrow(o$meta)) o$meta[station == k$station] else data.table()
    si <- tryCatch(as.data.table(E$sites$data()), error = function(e) data.table()); si <- if (nrow(si) && "station" %in% names(si)) si[station == k$station] else data.table()
    lg <- tryCatch(as.data.table(E$fieldlog$data()), error = function(e) data.table()); lg <- if (nrow(lg) && "station" %in% names(lg)) lg[station == k$station & type %in% c("site visit", "data downloaded", "gauge serviced")] else data.table()
    cal <- tryCatch(as.data.table(E$cal$data()), error = function(e) data.table()); ncal <- if (nrow(cal)) sum(vapply(strsplit(as.character(cal$applies_to), "|", fixed = TRUE), function(w) { w <- trim(w); !any(nzchar(w)) || k$station %in% w || any(m$logger_id %in% w) }, NA)) else 0
    pc <- tryCatch(as.data.table(E$period$data()), error = function(e) data.table()); npc <- if (nrow(pc) && "target" %in% names(pc)) sum(!nzchar(pc$target) | pc$target == k$station) else 0
    a <- o$index; rm <- if (!is.null(a) && nrow(a) && "station" %in% names(a) && "n" %in% names(a)) a[station == k$station] else data.table()
    kv <- function(a, b) div(class = "kv", span(a), span(b))
    sensors <- tryCatch(paste(utils::head(setdiff(names(dat())[vapply(dat(), is.numeric, NA)], c(WB_META, "RECORD")), 6), collapse = ", "), error = function(e) "")
    tagList(h4(k$station), div(class = "dim", style = "margin-bottom:6px", k$stream),
      kv("Logger", if (nrow(m)) paste(unique(paste(m$model, m$logger_id)), collapse = ", ") else "\u2014"),
      kv("Elevation", if (nrow(si) && nzchar(si$elevation_m[1] %||% "")) paste(si$elevation_m[1], "m") else "\u2014"),
      kv("Clock", if (nrow(si) && nzchar(si$utc_offset_hours[1] %||% "")) paste0("UTC", sprintf("%+g", as.numeric(si$utc_offset_hours[1]))) else "logger time"),
      kv("Last visit", if (nrow(lg)) max(substr(lg$date, 1, 10)) else "\u2014"),
      kv("Sensors", if (nzchar(sensors)) sensors else "\u2014"),
      kv("Calibrations", paste(ncal, if (ncal == 1) "row" else "rows")),
      h4(style = "margin-top:12px", "Done to this data"),
      p(class = "dim", style = "font-size:12.5px", paste0("Read ", if (nrow(m) && "files" %in% names(m)) sum(as.numeric(m$files), na.rm = TRUE) else "its", " files \u00b7 ",
        if (nrow(rm)) paste0(num(sum(as.numeric(rm$n %||% 0), na.rm = TRUE)), " values changed or removed in the audit trail") else "nothing changed in the audit trail",
        if (npc) paste0(" \u00b7 ", npc, " correction", if (npc > 1) "s", " over a period") else "")),
      div(class = "btnrow", tags$a(class = "btn btn-default btn-sm", href = "#", `data-ovid-go` = "Shown", title = "Rename the station, or choose what shows", "Edit station"),
          tags$a(class = "btn btn-default btn-sm", href = paste0("?ui=workbench&station=", utils::URLencode(k$station, reserved = TRUE)), "Open in Workbench"))) })
}
