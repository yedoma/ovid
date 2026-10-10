# GT/pages/explore.R  --  Explore.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore -----------------------------------------------------------------------------------------------------------
  q0 <- shiny::parseQueryString(isolate(session$clientData$url_search) %||% ""); q0site <- reactiveVal(q0$site)   # switching views keeps the site and period (core shell.R)
  observe({ s <- site_names(); cur <- isolate(input$ex_site) %||% isolate(q0site()); updateSelectInput(session, "ex_site", choices = s, selected = if (isTRUE(cur %in% s)) cur else s[1]) })
  if (!embedded) { if (!is.null(q0$from) && !is.null(q0$to)) session$onFlushed(function() updateDateRangeInput(session, "ex_dates", start = q0$from, end = q0$to), once = TRUE)
    observe(ovid_ctx(session, "site", input$ex_site, NULL, input$ex_dates[1], input$ex_dates[2])) }
  site_raw <- reactive({ req(input$ex_site); rv$tick; load_site(input$ex_site, pd()) })
  site_removed <- reactive({ req(input$ex_site); rv$tick; load_removed(input$ex_site, pd()) })
  observe({ d <- site_raw(); req(d); z <- sort(unique(d$depth_m)); updateSelectizeInput(session, "ex_depths", choices = setNames(z, depth_lab(z)), selected = character())
    updateSelectInput(session, "se_depth", choices = setNames(z, depth_lab(z)), selected = z[1])
    updateDateRangeInput(session, "ex_dates", start = as.Date(min(d$TIMESTAMP)), end = as.Date(max(d$TIMESTAMP))) })
  observeEvent(input$ex_all, { d <- site_raw(); req(d); updateDateRangeInput(session, "ex_dates", start = as.Date(min(d$TIMESTAMP)), end = as.Date(max(d$TIMESTAMP))) })
  observeEvent(input$ex_lastyr, { d <- site_raw(); req(d); e <- as.Date(max(d$TIMESTAMP)); updateDateRangeInput(session, "ex_dates", start = e - 365, end = e) })
  observeEvent(input$ex_lastwy, { d <- site_raw(); req(d); m <- run_set("water_year_start"); y <- water_year(max(d$TIMESTAMP), m) - 1L
    updateDateRangeInput(session, "ex_dates", start = as.Date(sprintf("%d-%02d-01", y - 1L, m)), end = as.Date(sprintf("%d-%02d-01", y, m)) - 1) })
  output$ex_site_note <- renderUI({ o <- outs(); req(o, input$ex_site); r <- o$ss[site == input$ex_site]; if (!nrow(r)) return(NULL)
    div(class = "dim", style = "margin-top:-8px", span(class = "tag acc", r$instrument), if (nzchar(r$landcover %||% "") && !is.na(r$landcover)) span(class = "tag", r$landcover),
        if (r$assumed_depths > 0) span(class = "tag warn", paste(r$assumed_depths, "assumed depths"))) })
  site_d <- reactive({ d <- site_raw(); req(d); dr <- input$ex_dates
    if (length(dr) == 2 && !anyNA(dr)) d <- d[TIMESTAMP >= as.POSIXct(dr[1]) & TIMESTAMP < as.POSIXct(dr[2] + 1)]
    z <- as.numeric(input$ex_depths); if (length(z)) d <- d[round(depth_m, 4) %in% round(z, 4)]
    d })
  site_daily <- reactive({ o <- outs(); req(o, input$ex_site); x <- o$daily[site == input$ex_site]; dr <- input$ex_dates
    if (length(dr) == 2 && !anyNA(dr)) x <- x[date >= dr[1] & date <= dr[2]]
    z <- as.numeric(input$ex_depths); if (length(z)) x <- x[round(depth_m, 4) %in% round(z, 4)]
    x[complete == TRUE] })
  ylim_of <- function(id, v) temp_limits(v, input[[paste0(id, "_mode")]] %||% "auto", input[[paste0(id, "_lo")]], input[[paste0(id, "_hi")]], input[[paste0(id, "_pct")]] %||% 1)
  ttl <- function(...) paste0(input$ex_site, ...)
  cap <- reactive(assumed_caption(site_raw()))
  res <- reactive(input$ex_res %||% "day")

  # before import: recent projects and what the data folders hold, by kind of file (looked at by name only, nothing is read)
  output$bi_recent <- renderUI({ r <- tryCatch(projects_known(SETTINGS), error = function(e) character()); if (!length(r)) p(class = "dim", "No projects yet.") else p(class = "dim", "Recent: ", paste(utils::head(basename(r), 4), collapse = " · ")) })
  output$bi_scan <- renderUI({ d <- trim(unlist(strsplit(input$raw_dirs %||% paste(SETTINGS$raw_dirs, collapse = "\n"), "\n"))); d <- d[nzchar(d)]
    if (!length(d)) return(p(class = "dim", "No data folder yet. Choose one above and this lists what is in it."))
    if (!any(dir.exists(path.expand(d)))) return(p(class = "dim", "None of the data folders exists."))
    f <- tryCatch(raw_file_list(d, skip_dirs = c("output", "config", "library", "R-portable")), error = function(e) NULL); if (is.null(f) || !nrow(f)) return(p(class = "dim", "The folders are empty."))
    ext <- tolower(tools::file_ext(f$file)); kinds <- list(
      list(label = "RBR logger file (.hex)", ext = "hex", ok = TRUE, note = "Read directly."),
      list(label = "Ruskin export (.txt, .xls)", ext = c("txt", "xls"), ok = TRUE, note = "Used when there is no .hex for that download."),
      list(label = "GeoPrecision logger file (.f2b)", ext = "f2b", ok = TRUE, note = "Decoded by GT; an FG2-Shell .csv, if present, is preferred."),
      list(label = "FG2-Shell or HOBOware export (.csv)", ext = "csv", ok = TRUE, note = "Read; a water-level or conductivity logger is left to WLR."),
      list(label = "HOBO logger file (.hobo)", ext = "hobo", ok = FALSE, note = "Not read: export it to CSV in HOBOware into the same folder."),
      list(label = "EM50 workbook (.xlsx)", ext = "xlsx", ok = TRUE, note = "Read as an ECH2O Utility workbook."),
      list(label = "Campbell TOA5 (.dat)", ext = "dat", ok = TRUE, note = "TOA5 is read; mixed-array and binary tables are not."))
    known <- unlist(lapply(kinds, `[[`, "ext")); other <- sum(!ext %in% known)
    rows <- lapply(kinds, function(k) { n <- sum(ext %in% k$ext); if (!n) NULL else tags$tr(tags$td(k$label), tags$td(num(n)), tags$td(class = if (k$ok) "ok" else "no", if (k$ok) "readable" else "needs export"), tags$td(class = "dim", k$note)) })
    rows <- c(rows, if (other) list(tags$tr(tags$td("Other files"), tags$td(num(other)), tags$td(class = "dim", "listed"), tags$td(class = "dim", "Listed, never read."))))
    tags$table(class = "table d-num bi-scan", tags$thead(tags$tr(tags$th("Kind of file"), tags$th("Found"), tags$th("Read?"), tags$th("Notes"))), tags$tbody(Filter(Negate(is.null), rows))) })

  # site notebook (Site results)
  observeEvent(input$nb_pick, updateSelectInput(session, "ex_site", selected = input$nb_pick))
  observeEvent(input$nb_more, { req(nzchar(input$nb_more %||% "")); updateSelectInput(session, "ex_site", selected = input$nb_more) })
  output$nb_chips <- renderUI({ s <- site_names(); req(length(s)); cur <- input$ex_site
    shown <- unique(c(head(s, 8), if (!is.null(cur) && cur %in% s) cur)); rest <- setdiff(s, shown)
    div(class = "nb-chips", lapply(shown, function(x) tags$a(href = "#", class = paste("nb-chip", if (identical(x, cur)) "on"), `data-site` = x, x)),
        if (length(rest)) selectizeInput("nb_more", NULL, choices = c("", rest), selected = "", options = list(placeholder = paste0("+", length(rest), " sites")))) })
  nb_year_of <- function(s, z) { o <- outs(); a <- o$annual[site == s & year_type == "calendar" & complete == TRUE & abs(depth_m - z) < 1e-6]; if (nrow(a)) max(a$year) else NA_integer_ }
  observe({ d <- site_raw(); req(d); z <- sort(unique(d$depth_m)); updateSelectInput(session, "nb_depth", choices = setNames(z, depth_lab(z)), selected = z[which.min(abs(z - 1))]) })
  observe({ o <- outs(); req(o, input$ex_site); y <- sort(unique(o$annual[site == input$ex_site & year_type == "calendar", year]), decreasing = TRUE)
    ok <- sort(unique(o$annual[site == input$ex_site & year_type == "calendar" & complete == TRUE, year]), decreasing = TRUE)
    updateSelectInput(session, "nb_year", choices = y, selected = if (length(ok)) ok[1] else if (length(y)) y[1] else NULL) })
  output$nb_head <- renderUI({ o <- outs(); d <- site_raw(); req(o, d, input$ex_site); r <- o$ss[site == input$ex_site]
    z <- sort(unique(d$depth_m)); tt <- sort(unique(as.numeric(d$TIMESTAMP[seq_len(min(nrow(d), 20000))]))); iv <- if (length(tt) > 1) stats::median(diff(tt)) else NA_real_
    every <- if (is.na(iv)) "an unknown interval" else if (iv %% 86400 == 0) "day" else if (iv %% 3600 == 0) paste(iv / 3600, "h") else paste(round(iv / 60), "min")
    tagList(h2(class = "nb-site", input$ex_site),
      p(class = "nb-desc", sprintf("%d sensors from %s to %s, %s to %s, one record every %s.", length(z), depth_lab(min(z)), depth_lab(max(z)), format(as.Date(min(d$TIMESTAMP))), format(as.Date(max(d$TIMESTAMP))), every),
        if (nrow(r)) tagList(" ", span(class = "tag acc", r$instrument), if (nzchar(r$landcover %||% "") && !is.na(r$landcover)) span(class = "tag", r$landcover),
                             if (r$assumed_depths > 0) span(class = "tag warn", paste(r$assumed_depths, "assumed depths"))))) })
  output$nb_facts <- renderUI({ o <- outs(); req(o, input$ex_site, input$nb_depth); s <- input$ex_site; z <- as.numeric(input$nb_depth); dd <- o$daily[site == s & complete == TRUE]
    yr <- nb_year_of(s, z); a <- o$annual[site == s & year_type == "calendar" & complete == TRUE & abs(depth_m - z) < 1e-6 & year %in% yr]
    td <- o$td[site == s & !is.na(thaw_m)]; al <- o$alt[site == s & !is.na(alt_m)]
    below <- nrow(o$td[site == s & grepl("below the deepest", state)]) > 0
    surf <- if (nrow(dd)) min(dd$depth_m) else NA_real_; y3 <- if (!is.na(yr)) yr else if (nrow(dd)) max(as.integer(format(as.Date(dd$date), "%Y"))) else NA_integer_
    days <- if (is.na(surf)) NA_integer_ else sum(dd$depth_m == surf & dd$temp > 0 & as.integer(format(as.Date(dd$date), "%Y")) == y3)
    all_d <- o$daily[site == s]; pct <- if (nrow(all_d)) { span <- as.numeric(max(as.Date(all_d$date)) - min(as.Date(all_d$date))) + 1; 100 * sum(all_d$complete) / (uniqueN(all_d$depth_m) * span) } else NA_real_
    div(class = "d-kpis",
      ovid_kpi(paste("Mean ground temperature,", depth_lab(z)), if (nrow(a)) sprintf("%.2f", a$mean[1]) else "\u2014", if (nrow(a)) "\u00b0C", if (nrow(a)) as.character(yr) else "no complete calendar year"),
      ovid_kpi("Max thaw depth", if (nrow(td)) sprintf("%.2f", max(td$thaw_m)) else if (nrow(al)) sprintf("%.2f", max(al$alt_m)) else "\u2014", if (nrow(td) || nrow(al)) "m",
                 if (nrow(td)) paste0(substr(td$date[which.max(td$thaw_m)], 1, 4), ", from the sensor profile") else if (nrow(al)) paste0(al$year[which.max(al$alt_m)], ", active layer") else if (below) "thawed below the deepest sensor" else "no thaw between sensors"),
      ovid_kpi(paste("Days thawed at", if (is.na(surf)) "the surface" else depth_lab(surf)), if (is.na(days)) "\u2014" else days, if (!is.na(days)) "d", if (!is.na(y3)) as.character(y3)),
      ovid_kpi("Completeness", if (is.na(pct)) "\u2014" else sprintf("%.1f", pct), if (!is.na(pct)) "%", "of daily means, every depth, whole record", level = if (!is.na(pct) && pct < 85) "warn")) })
  output$nb_hm <- renderPlot({ d <- site_raw(); req(d); validate(need(nrow(d) > 0, "No data for this site."))
    plot_heatmap(d, paste0(input$ex_site, " \u00b7 ground temperature"), "day", 0.05, smooth_steps(input$nb_smooth), TRUE, NULL, numeric(0), TRUE, TRUE, "linear", assumed_caption(d)) }, bg = MI$bg)
  output$nb_hm_cap <- renderUI({ d <- site_raw(); req(d); c <- assumed_caption(d); if (is.null(c) || !nzchar(c)) NULL else p(class = "dim", c) })
  output$nb_td <- renderPlot({ o <- outs(); req(o, input$ex_site); td <- copy(o$td[site == input$ex_site]); td[, date := as.Date(date)]; a <- copy(o$alt[site == input$ex_site]); a[, date_alt := as.Date(date_alt)]
    plot_thaw_depth(td, a, paste0(input$ex_site, " \u00b7 thaw depth and active layer")) }, bg = MI$bg)
  nb_annual <- reactive({ o <- outs(); req(o, input$ex_site, input$nb_year); x <- o$annual[site == input$ex_site & year_type == "calendar" & year == as.integer(input$nb_year)][order(depth_m)]
    x[, .(depth_m, mean, min, max, state = fifelse(max <= 0, "frozen", fifelse(min >= 0, "thawed", "thaws")), days, `QA/QC` = fifelse(complete, "complete", if ("days_estimated" %in% names(x)) paste(days_estimated, "days estimated") else "incomplete"))] })
  output$nb_annual <- renderDT(ovid_dt(nb_annual(), page = 12))

  # heatmap. Smoothing is on by default and changes only the picture (and its PNG), never the gridded values that are exported.
  hm_smooth_eff <- reactive(smooth_steps(input$hm_smooth))
  hm_plot <- reactive({ d <- site_d(); validate(need(nrow(d) > 0, "No data in this period."))
    lim <- switch(input$hm_scale, fixed = c(input$hm_lo, input$hm_hi), sym = { m <- stats::quantile(abs(d$value), 0.99); c(-m, m) }, NULL)
    iso <- suppressWarnings(as.numeric(trim(strsplit(input$hm_iso %||% "", ",")[[1]]))); iso <- iso[!is.na(iso)]
    plot_heatmap(d, ttl(" \u00b7 ground temperature"), res(), max(0.01, (input$hm_dz %||% 5) / 100), hm_smooth_eff(), input$hm_interp == "TRUE", lim, iso,
                 isTRUE(input$hm_zero), isTRUE(input$hm_lines), input$hm_trans, cap()) })
  output$hm_plot <- renderPlot(hm_plot(), bg = MI$bg)
  output$hm_png <- downloadHandler(function() paste0(safe_name(input$ex_site), "_heatmap.png"), function(file) gt_save(hm_plot(), file, 13, 6.5))
  output$hm_dl <- downloadHandler(function() paste0(safe_name(input$ex_site), "_heatmap_grid.csv"), function(file) {
    g <- heatmap_grid(site_d(), res(), max(0.01, (input$hm_dz %||% 5) / 100), 1, input$hm_interp == "TRUE"); g[, TIMESTAMP := fmt_ts(TIMESTAMP)]; fwrite(g, file) })

  # Over time: the standard plot (core/tsview.R) shown two ways: every depth on one axis, or one panel per depth with the
  # flagged and removed readings. Drag selects a stretch; dragging up a panel's value axis narrows it to a band of
  # temperatures (the old box). The edits are on the bar above the selection, each with a reason and initials; they go to
  # Quality rules > Corrections over a period and apply at the next run (the raw files are not touched), with Undo.
  ot_acts <- function(all) if (!VIEWER) list(
    list(id = "box", label = "Remove the points in the band", kind = "bad", band = TRUE),
    list(id = "flag", label = "Flag them", kind = "warn", band = TRUE, title = "Flag the points in the band; they are kept"),
    list(id = "period", label = "Remove every depth in this period", label_band = if (!all) "Remove {panel} in this period", kind = "bad"),
    list(id = "start", label = "Record starts here", title = "Removes everything before the end of the selection (a logger reading before it went into the ground)"),
    list(id = "end", label = "Record ends here", title = "Removes everything after the start of the selection (a logger reading after it came out)"))
  observe({ d <- site_d(); req(d, nrow(d) > 0); v <- input$ot_view %||% "all"; st <- input$ex_site; all <- v == "all"
    x <- d[!is.na(TIMESTAMP) & !is.na(depth_m)]; tt <- sort(unique(x$TIMESTAMP)); z <- sort(unique(x$depth_m)); cols <- depth_ramp(length(z))
    rm <- if (v == "panels") { r <- site_removed(); if (!is.null(r) && nrow(r)) { if ("variable" %in% names(r)) r <- r[variable == "temp"]; r[TIMESTAMP >= min(tt) & TIMESTAMP <= max(tt)] } }
    thin_i <- function(n, m = 20000) if (n > m) round(seq(1, n, length.out = m)) else seq_len(n)
    series <- lapply(seq_along(z), function(k) { xz <- x[depth_m == z[k]]; val <- rep(NA_real_, length(tt)); val[match(xz$TIMESTAMP, tt)] <- xz$value
      dots <- if (v == "panels") Filter(Negate(is.null), list(
        if (!is.null(rm) && nrow(r <- rm[round(depth_m, 4) == round(z[k], 4) & !is.na(value)])) { r <- r[thin_i(nrow(r))]; list(t = r$TIMESTAMP, v = r$value, kind = "removed", label = "removed by a rule or by hand") },
        if (nrow(f <- xz[nzchar(flag) & !is.na(value)])) { f <- f[thin_i(nrow(f))]; list(t = f$TIMESTAMP, v = f$value, kind = "flagged", label = "flagged (kept)") }))
      list(name = depth_lab(z[k]), key = format(z[k]), unit = "°C", v = val, digits = 2, col = cols[k], dots = dots,
           panel = if (all) "all", plab = if (all) "Temperature, every depth (°C)") })
    # events: where a logger takes over (a swap or a new deployment); sources: the logger of each stretch
    lg <- x[, .(t = min(TIMESTAMP)), by = logger_id][order(t)]
    ev <- if (nrow(lg) > 1) data.table(t = as.numeric(lg$t[-1]), label = "Logger change", note = paste("logger", lg$logger_id[-1]))
    same <- !all && isTRUE(input$ot_same); cp <- cap()
    session$onFlushed(function() ts_view_send(session, "ot_tsv", tt, series, zone = zone_text(BASE_OFFSET), wb = list(key = "site", value = st),
      events = ev, sources = if (nrow(lg)) data.table(from = as.numeric(lg$t), label = paste("Logger", lg$logger_id)), same_scale = same,
      height = if (all) 440 else 170, title = paste(st, "· ground temperature"), site = st, caption = cp, file = paste0("gt_", st, "_", v),
      actions = ot_acts(all), ask = list(note = "Added to Corrections over a period; applied at the next run")), once = TRUE) })
  output$ex_dl <- downloadHandler(function() paste0(safe_name(input$ex_site), "_values.csv"), function(file) fwrite(site_table(site_d(), res(), "wide", TRUE, BASE_OFFSET, BASE_OFFSET), file))
  output$qc_flags <- renderDT({ o <- outs(); req(o); ovid_dt(o$flags[site == input$ex_site][order(start)], page = 10) })
  output$qc_removed <- renderDT({ o <- outs(); req(o); ix <- o$index[site == input$ex_site | (is.na(site) & logger_id %in% unique(site_raw()$logger_id))]
    req(nrow(ix) > 0); x <- ix[, .(values = sum(n)), by = .(stage, rule, logger_id, channel)][order(-values)]; ovid_dt(cbind(x, rule_info(x$rule)[, .(what = title)]), page = 10) })
  ts_view_undo(input, session, "ot_tsv")
  observeEvent(input$ot_tsv_act, { a <- input$ot_tsv_act; if (VIEWER) return()
    why <- trim(a$reason %||% ""); if (!nzchar(why)) return(note("Give a reason first.", type = "warning"))
    r <- as.POSIXct(c(a$t0, a$t1), origin = "1970-01-01", tz = "UTC"); k <- a$key %||% ""; z <- if (nzchar(k) && k != "all") format(depth_typed(as.numeric(k), proj_sgn())) else ""   # in the project's depth convention
    d <- site_raw(); req(d); first <- min(d$TIMESTAMP); last <- max(d$TIMESTAMP)
    row <- data.frame(site = input$ex_site, serial = "", sensor = "", depth_m = z, variable = "temp", start = fmt_ts(r[1]), end = fmt_ts(r[2]),
                      min_value = "", max_value = "", action = "remove", reason = why, author = trim(a$author %||% ""), stringsAsFactors = FALSE)
    kind <- a$act
    if (kind %in% c("box", "flag")) { req(is.finite(as.numeric(a$lo %||% NA)), is.finite(as.numeric(a$hi %||% NA))); row$min_value <- sprintf("%.6g", as.numeric(a$lo)); row$max_value <- sprintf("%.6g", as.numeric(a$hi)) }
    if (kind == "flag") row$action <- "flag"
    if (kind == "start") { row$start <- fmt_ts(first - 1); row$end <- fmt_ts(r[2]); row$depth_m <- "" }
    if (kind == "end") { row$start <- fmt_ts(r[1]); row$end <- fmt_ts(last + 1); row$depth_m <- "" }
    if (m_man$append(row)) { mark(); ts_view_done(session, "ot_tsv", paste0(switch(kind, flag = "Flag", start = "Record start", end = "Record end", "Removal"), " added. Run the pipeline to apply it.")) }
  })

  # box plots
  dp_n <- reactive({ d <- site_d(); if (is.null(d)) 1 else uniqueN(d$depth_m) })
  bx_n <- reactive(if (identical(input$bx_layout, "facet") && input$bx_group != "none") ceiling(dp_n() / 3) else 1)
  output$bx_plot_ui <- renderUI(plotOutput("bx_plot", height = paste0(if (bx_n() > 1) 140 + 280 * bx_n() else 540, "px")))
  bx_plot <- reactive({ d <- site_d(); validate(need(nrow(d) > 0, "No data in this period.")); plot_boxplots(d, ttl(" \u00b7 distribution by depth"), res(), input$bx_group, ylim_of("bx", d$value), input$bx_layout, run_set("water_year_start")) })
  output$bx_plot <- renderPlot(bx_plot(), bg = MI$bg)
  bx_tab <- reactive(box_stats(site_d(), res(), input$bx_group, run_set("water_year_start")))
  output$bx_tbl <- renderDT(ovid_dt(bx_tab(), page = 12))
  output$bx_png <- downloadHandler(function() paste0(safe_name(input$ex_site), "_boxplots.png"), function(file) gt_save(bx_plot(), file, 13, if (bx_n() > 1) 1.5 + 3 * bx_n() else 6))
  output$bx_dl <- downloadHandler(function() paste0(safe_name(input$ex_site), "_box_statistics.csv"), function(file) fwrite(bx_tab(), file))

  # profile on dates
  observe({ x <- site_daily(); req(nrow(x) > 0); dts <- sort(unique(x$date)); update_selectize(session, "pd_dates", choices = format(rev(dts)), selected = isolate(input$pd_dates), server = TRUE) })
  observeEvent(input$pd_season, { x <- site_daily(); req(nrow(x) > 0); e <- max(x$date); d <- seq(as.Date(format(e - 365, "%Y-%m-15")), e, by = "1 month")
    d <- d[d %in% x$date]; update_selectize(session, "pd_dates", choices = format(rev(sort(unique(x$date)))), selected = format(d), server = TRUE) })
  pd_plot <- reactive({ x <- site_daily(); validate(need(length(input$pd_dates) > 0, "Pick one or more dates.")); plot_profile_dates(x, as.Date(input$pd_dates), ttl(" \u00b7 temperature profile"), ylim_of("pdx", x$temp)) })
  output$pd_plot <- renderPlot(pd_plot(), bg = MI$bg)
  output$pd_png <- downloadHandler(function() paste0(safe_name(input$ex_site), "_profiles.png"), function(file) gt_save(pd_plot(), file, 9, 7))

  # trumpet
  site_annual <- reactive({ o <- outs(); req(o, input$ex_site); x <- o$annual[site == input$ex_site]; z <- as.numeric(input$ex_depths); if (length(z)) x <- x[round(depth_m, 4) %in% round(z, 4)]; x })
  observe({ x <- site_annual(); y <- sort(unique(x[year_type == (input$tr_yt %||% "calendar"), year])); updateSelectizeInput(session, "tr_years", choices = y, selected = intersect(isolate(input$tr_years), y)) })
  tr_ny <- reactive({ x <- site_annual()[year_type == input$tr_yt]; if (isTRUE(input$tr_complete)) x <- x[complete == TRUE]; n <- if (length(input$tr_years)) length(input$tr_years) else uniqueN(x$year); max(1, n) })
  output$tr_plot_ui <- renderUI(plotOutput("tr_plot", height = paste0(if (input$tr_layout == "facet") 120 + 360 * ceiling(tr_ny() / 3) else 600, "px")))
  tr_plot <- reactive({ x <- site_annual(); validate(need(nrow(x) > 0, "No annual statistics for this site."))
    plot_trumpet(x, as.integer(input$tr_years), input$tr_yt, ttl(" \u00b7 trumpet curves"), input$tr_layout, ylim_of("trx", c(x$min, x$max)), isTRUE(input$tr_complete)) })
  output$tr_plot <- renderPlot(tr_plot(), bg = MI$bg)
  output$tr_tbl <- renderDT(ovid_dt(site_annual()[year_type == input$tr_yt][order(year, depth_m), !c("site", "year_type")], page = 12))
  output$tr_png <- downloadHandler(function() paste0(safe_name(input$ex_site), "_trumpet.png"), function(file) gt_save(tr_plot(), file, 12, if (input$tr_layout == "facet") 1.5 + 4 * ceiling(tr_ny() / 3) else 7))

  # seasons and years
  observe({ x <- site_daily(); req(nrow(x) > 0); y <- sort(unique(format(x$date, "%Y"))); updateSelectInput(session, "se_hl", choices = c("(none)" = "", rev(y)), selected = if (length(y)) y[length(y)] else "") })
  se_dd <- reactive({ o <- outs(); req(o, input$ex_site); o$daily[site == input$ex_site & complete == TRUE] })
  output$se_plot <- renderPlot({ x <- se_dd(); req(input$se_depth); plot_seasonal(x, as.numeric(input$se_depth), input$se_axis, ttl(" \u00b7 year by year"), ylim_of("sex", x$temp), run_set("water_year_start")) }, bg = MI$bg)
  output$cl_plot <- renderPlot({ x <- se_dd(); req(input$se_depth); plot_climatology(x, as.numeric(input$se_depth), input$se_hl, ttl(" \u00b7 climatology"), ylim_of("sex", x$temp)) }, bg = MI$bg)
  output$an_plot <- renderPlot(plot_annual(site_annual(), "calendar", ttl(" \u00b7 annual mean by depth"), TRUE), bg = MI$bg)

  # freeze / thaw
  output$ft_plot <- renderPlot(plot_freeze_thaw(site_daily(), run_set("zero_band"), ttl(" \u00b7 freeze and thaw")), bg = MI$bg, height = function() 140 + 26 * max(1, dp_n()))
  output$td_plot <- renderPlot({ o <- outs(); req(o); td <- o$td[site == input$ex_site]; td[, date := as.Date(date)]; a <- o$alt[site == input$ex_site]; a[, date_alt := as.Date(date_alt)]
    plot_thaw_depth(td, a, ttl(" \u00b7 thaw depth and active layer")) }, bg = MI$bg)
  output$ft_tbl <- renderDT({ o <- outs(); req(o); ovid_dt(o$ft[site == input$ex_site, !"site"][order(water_year, depth_m)], page = 10) })
  output$zc_tbl <- renderDT({ o <- outs(); req(o); ovid_dt(o$zc[site == input$ex_site, !"site"][order(start, depth_m)], page = 10) })

  # availability
  output$av_plot <- renderPlot(plot_availability(site_raw(), ttl(" \u00b7 data availability")), bg = MI$bg, height = function() 140 + 26 * max(1, dp_n()))
  output$av_gaps <- renderDT({ o <- outs(); req(o); l <- unique(site_raw()$logger_id); ovid_dt(o$gaps[logger_id %in% l][order(-length_h)], page = 10) })

  # by-depth table
  output$ex_stats <- renderDT({ d <- site_d(); req(nrow(d) > 0)
    ovid_dt(d[, .(sensor = paste(unique(sensor), collapse = ", "), logger = paste(unique(serial), collapse = ", "), depth_source = paste(unique(depth_source), collapse = ", "),
                values = .N, flagged = sum(nzchar(flag)), from = fmt_ts(min(TIMESTAMP)), to = fmt_ts(max(TIMESTAMP)), min = round(min(value), 3), mean = round(mean(value), 3),
                max = round(max(value), 3), sd = round(stats::sd(value), 3)), by = .(depth = depth_lab(depth_m))], page = 15) })
