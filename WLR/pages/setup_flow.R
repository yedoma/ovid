# WLR/pages/setup_flow.R  --  The set-up flow: what shows, the site dashboard, Needs you, Apply, the information panel.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- the set-up flow (core flow.R): what shows, the site dashboard, Needs you, Apply, the information panel ---------------
  site_all <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$summary)) character() else sort(unique(o$summary[kind == "water", site])) })
  site_shown <- display_server("disp", pd, site_all)
  output$flow_ctx <- renderUI({ live_stamp(display_path(pd())); display_context(pd(), site_all(), "sites") })
  zone_lab <- function() zone_text(SETTINGS$base_offset_h %||% BASE_OFFSET)
  # safe: no precipitation file read: none shown
  precip <- reactive({ input$precip_file; tryCatch(flow_precip(input$precip_file %||% SETTINGS$precip_file), error = function(e) NULL) })
  output$precip_check <- renderUI({ f <- trim(input$precip_file %||% ""); if (!nzchar(f)) return(NULL); b <- precip()
    if (is.null(b)) div(class = "check bad", "Not read: a CSV with a time column and a column of millimetres is needed.")
    else div(class = "check good", icon("check"), sprintf(" %s records, %s to %s, %s mm in all", num(length(b$t)), substr(fmt_ts(min(b$t)), 1, 10), substr(fmt_ts(max(b$t)), 1, 10),
                                                       format(round(sum(b$v, na.rm = TRUE), 1)))) })

  # one site's record for a plot: the water level (elevation where the datum is known, else water above the sensor), the ghost
  # of what was removed, the manual levels on it, and the barologger and water temperature under it
  lv_main <- function(d) if ("elev_m" %in% names(d) && any(!is.na(d$elev_m))) "elev_m" else "depth_m"
  lv_cache <- new.env()
  lv_load <- function(st, extra = FALSE) { k <- paste(st, extra, rv$tick); if (!is.null(lv_cache[[k]])) return(lv_cache[[k]])
    o <- outs(); d <- load_site(st, pd()); if (is.null(d) || !nrow(d)) return(NULL); d <- d[!is.na(TIMESTAMP)][order(TIMESTAMP)][!duplicated(TIMESTAMP)]
    p <- lv_main(d); v <- d[[p]]; raw <- v
    sr <- unique(o$summary[site == st, serial]); rm <- rbindlist(lapply(sr, removed_values, project_dir = pd()), fill = TRUE)
    if (nrow(rm)) try({ i <- match(as.numeric(rm$TIMESTAMP), as.numeric(d$TIMESTAMP)); ok <- !is.na(i) & is.na(v[i])   # a level is removed at the water pressure:
      if (any(ok)) { r <- rm[ok]; i <- i[ok]                                                                              # the ghost is the level that pressure gave
        kp <- stats::median(d$depth_m / d$dp_kpa, na.rm = TRUE); off <- if (p == "elev_m") stats::median(d$elev_m - d$depth_m, na.rm = TRUE) else 0
        g <- ifelse(r$parameter == p, r$old_value, ifelse(r$parameter == "pres_kpa" & is.finite(kp), (r$old_value - d$baro_kpa[i]) * kp + off, NA_real_))
        raw[i] <- ifelse(is.na(raw[i]), g, raw[i]) } }, silent = TRUE)
    lab <- c(elev_m = "Water elevation", depth_m = "Water above sensor")[[p]]
    ms <- if (nrow(o$meas) && all(c("site", "parameter", "time", "manual") %in% names(o$meas))) o$meas[site == st & parameter == p] else data.table()
    ser <- list(list(name = lab, key = p, unit = "m", v = v, raw = raw, digits = 3, marks = if (nrow(ms)) list(t = ms$time, v = ms$manual, label = "manual")))
    if (extra) { if ("baro_kpa" %in% names(d)) ser[[2]] <- list(name = "Barometric pressure", unit = "kPa", v = d$baro_kpa, digits = 2)
                 if ("temp_c" %in% names(d)) ser[[length(ser) + 1]] <- list(name = "Water temperature", unit = "\u00b0C", v = d$temp_c, digits = 2) }
    dd <- o$summary[site == st][order(start)]
    # events (plot standards A11): each deployment's start, and each field visit with a manual level; sources: the logger of each stretch
    ev <- rbindlist(list(if (nrow(dd) && "start" %in% names(dd)) data.table(t = as.numeric(dd$start), label = "Deployment starts", note = paste("logger", dd$serial %||% "")),
                         if (nrow(ms)) data.table(t = as.numeric(ms$time), label = "Field visit", note = paste0("manual level ", formatC(as.numeric(ms$manual), format = "f", digits = 3), " m"))), fill = TRUE)
    src <- if (nrow(dd) && "start" %in% names(dd)) data.table(from = as.numeric(dd$start), label = paste0("Logger ", dd$serial %||% "", " \u00b7 deployment ", dd$deployment))
    m <- list(t = d$TIMESTAMP, series = ser, zone = zone_lab(), bars = precip(), events = if (nrow(ev)) ev[!is.na(t)][seq_len(min(.N, 300))], sources = src,
              wb = list(key = "deployment", value = dd$deployment[nrow(dd)], params = p, from = I(as.numeric(dd$start)), values = I(dd$deployment)))
    assign(k, m, envir = lv_cache); m }

  # the site dashboard
  site_cur <- reactiveVal(NULL)
  if (!embedded) {                                              # switching views keeps the site and period (core shell.R)
    q0 <- shiny::parseQueryString(isolate(session$clientData$url_search) %||% "")
    if (!is.null(q0$site)) site_cur(q0$site)
    if (!is.null(q0$page)) session$onFlushed(function() go(q0$page), once = TRUE)   # a link from the Simple view opens its page (?page=Visits)
    observe(ovid_ctx(session, "site", site_cur(), NULL, q0$from, q0$to)) }
  observe({ s <- site_shown(); cur <- isolate(site_cur()); if (!length(s)) return(); if (is.null(cur) || !cur %in% s) site_cur(s[1])
    updateSelectInput(session, "site_pick", choices = s, selected = isolate(site_cur())) })
  observeEvent(input$site_pick, if (nzchar(input$site_pick %||% "")) { site_cur(input$site_pick); updateSelectInput(session, "ex_site", selected = input$site_pick) }, ignoreInit = TRUE)
  step_site <- function(k) { s <- site_shown(); i <- match(site_cur(), s); if (!length(s) || is.na(i)) return(); v <- s[(i - 1 + k) %% length(s) + 1]
    site_cur(v); updateSelectInput(session, "site_pick", selected = v) }
  observeEvent(input$site_prev, step_site(-1)); observeEvent(input$site_next, step_site(1))
  output$site_title <- renderUI(if (is.null(site_cur())) "Site" else paste("Site", site_cur()))
  # Before import: the drop area, a checklist of what compensation needs (each with its state and page) and
  # blank template tables with the right columns
  # A barologger shift or drift changes every deployment compensated with it: listed before the next run applies it
  output$bo_check_sites <- renderUI({ o <- outs(); d <- tryCatch(as.data.table(E$bcor$data()), error = function(e) data.table())
    if (is.null(o) || !nrow(o$summary)) return(p(class = "dim", "Run once: each barologger's sites are then listed here."))
    if (!nrow(d) || !"barologger" %in% names(d)) return(p(class = "dim", "No barologger checks yet. Each row added above lists here the sites it would change."))
    sm <- o$summary; used <- function(b) { b <- trim(b); sm[(trim(primary) == b | grepl(b, primary, fixed = TRUE) | trim(secondary) == b) & nzchar(b)] }
    rows <- lapply(seq_len(nrow(d)), function(i) { b <- as.character(d$barologger[i]); x <- used(b)
      tags$tr(tags$td(tags$b(b)), tags$td(paste(d$from[i] %|% "", "to", d$to[i] %|% "")), tags$td(d$method[i] %|% ""),
              tags$td(if (nrow(x)) paste(unique(x$site), collapse = ", ") else span(class = "dim", "no deployment uses it")),
              tags$td(num(nrow(x)), " deployment", if (nrow(x) == 1) "" else "s")) })
    tags$table(class = "table d-num", tags$thead(tags$tr(tags$th("Barologger"), tags$th("Period"), tags$th("Method"), tags$th("Sites it changes"), tags$th("Deployments"))), tags$tbody(rows)) })
  output$site_empty <- renderUI(if (!length(site_all())) {
    d <- trim(unlist(strsplit(paste(input$raw_dirs %||% "", collapse = "\n"), "\n"))); d <- d[nzchar(d) & dir.exists(path.expand(d))]
    nf <- nrow(raw_files_found(d))                                                     # R/discover.R: the files a run reads
    # safe: a count for the flow only; the table's editor reports a read problem
    rows <- function(id) tryCatch(NROW(E[[id]]$data()), error = function(e) 0L)
    item <- function(ok, req, title, sub, go, tab = NULL) div(class = "wl-chk", span(class = if (ok) "ok" else if (req) "w" else "m", if (ok) "\u2713" else if (req) "\u25b2" else "\u25cb"),
      div(tags$b(title), span(class = if (req) "req" else "opt", if (req) " required" else " optional"), div(class = "dim", sub)),
      tags$a(href = "#", class = "btn btn-default btn-sm", `data-wb-open` = as.character(jsonlite::toJSON(c(list(go = go), if (!is.null(tab)) list(tab = tab)), auto_unbox = TRUE)), `data-ovid-go` = if (is.null(tab)) go, "Open"))
    div(class = "wl-before", tags$style(HTML(".wl-before{display:grid;grid-template-columns:minmax(0,1.1fr) minmax(0,1fr);gap:12px}.wl-drop{border:2px dashed var(--bg3);border-radius:10px;padding:26px;text-align:center;display:flex;flex-direction:column;gap:10px;align-items:center;justify-content:center}.wl-chk{display:grid;grid-template-columns:22px 1fr auto;gap:8px;align-items:start;padding:8px 0;border-bottom:1px solid var(--bg2)}.wl-chk .ok{color:var(--green)}.wl-chk .w{color:var(--yellow)}.wl-chk .m{color:var(--fg4)}.wl-chk .req{color:var(--yellow);font-size:11.5px}.wl-chk .opt{color:var(--fg4);font-size:11.5px}@media (max-width:1000px){.wl-before{grid-template-columns:1fr}}")),
      div(class = "d-cardx wl-drop", icon("water", class = "fa-2x"), h3("Bring in water level and barologger files"),
        p(class = "dim", "Water and baro loggers may be mixed in one folder: their roles come from the deployments. Nothing in the folder is changed."),
        div(class = "btnrow", tags$a(class = "btn btn-primary", href = "#", `data-ovid-go` = if (VIEWER) "Home" else "Project", icon("folder-open"), " Choose folder\u2026"),
            tags$a(class = "btn btn-default", href = "#", `data-ovid-go` = "Metadata", icon("table-list"), " Import metadata (CSV)\u2026")),
        p(class = "dim", if (length(d)) sprintf("%d data file%s in the folders so far. Run once to build each site's dashboard.", nf, if (nf == 1) "" else "s") else "No data folder set yet.")),
      div(class = "d-cardx", h3("What compensation needs"),
        item(nf > 0, TRUE, "Water level loggers", if (nf) sprintf("%d files found", nf) else "none found yet", "Project"),
        item(rows("bstn") > 0 || rows("bmem") > 0, TRUE, "A barologger per study area", if (rows("bmem")) sprintf("%d sites in baro groups", rows("bmem")) else "found from the files, or set on Barometric compensation", "Baro"),
        item(rows("deps") > 0 || rows("depsheet") > 0, TRUE, "Deployments (dates, hang lengths, tip or sensor)", sprintf("%d rows", rows("deps") + rows("depsheet")), "Metadata", "Deployment sheet"),
        item(rows("meas") > 0, FALSE, "Manual levels (plotted as reference)", sprintf("%d readings", rows("meas")), "Visits"),
        item(rows("datm") > 0 || rows("gpsp") > 0, FALSE, "Datum per site", sprintf("%d elevations", rows("datm") + rows("gpsp")), "Datum"),
        div(style = "margin-top:10px;display:flex;gap:8px;align-items:center;flex-wrap:wrap", span(class = "dim", "Use a template table:"),
            selectInput("wl_tpl", NULL, c("Deployments" = "deployments", "Manual levels" = "measurements", "Datum per site" = "datum", "Baro groups" = "baro_members"), width = "200px"),
            downloadButton("wl_tpl_dl", "Blank table", class = "btn-sm"))))
  })
  output$wl_tpl_dl <- downloadHandler(function() paste0(CONFIG_FILES[[input$wl_tpl %||% "deployments"]]$file %||% "table.csv"),
    function(file) { k <- input$wl_tpl %||% "deployments"; cc <- CONFIG_FILES[[k]]$cols; cc <- if (is.function(cc)) cc() else cc
      fwrite(as.data.table(setNames(rep(list(character()), length(cc)), cc)), file) })
  output$site_chips <- renderUI({ o <- outs(); st <- site_cur(); req(o, st); r <- o$summary[site == st][order(start)]; if (!nrow(r)) return(NULL); r <- r[nrow(r)]
    span(class = "d-chip", title = paste("Compensated with", r$primary %|% "no barologger", if (nzchar(r$secondary %|% "")) paste("and", r$secondary) else ""), r$baro_group %|% "no baro group") })
  output$site_kpis <- renderUI({ o <- outs(); st <- site_cur(); req(o, st); m <- lv_load(st, TRUE); req(m)
    lv <- m$series[[1]]$v; t <- m$t; k <- which(!is.na(lv)); if (!length(k)) return(p(class = "dim", "No water level at this site."))
    last <- max(k); pk <- k[which.max(lv[k])]
    ch <- settled_change(t, lv, o$summary[site == st]$start, SETTINGS$service_window_h %||% 2)   # R/summaries.R
    tc <- if (length(m$series) > 1) m$series[[length(m$series)]] else NULL; tv <- if (!is.null(tc) && identical(tc$name, "Water temperature")) tc$v
    ms <- if (nrow(o$meas) && "difference" %in% names(o$meas)) o$meas[site == st & !is.na(difference)] else data.table()
    fl <- if (nrow(o$flags)) o$flags[site == st] else data.table()
    f3 <- function(x) formatC(x, format = "f", digits = 3)
    tagList(
      ovid_kpi(paste("Latest", tolower(m$series[[1]]$name)), f3(lv[last]), "m", format(t[last], "%Y-%m-%d %H:%M")),
      if (!is.null(ch)) ovid_kpi(if (ch$full) "Change, 60 days" else "Change in the record", sub("^-", "\u2212", sprintf("%+.3f", lv[ch$last] - lv[ch$first])), "m",
        paste("since", format(t[ch$first], "%Y-%m-%d %H:%M"), if (!ch$full) "(first settled reading)")),
      ovid_kpi("Peak level", f3(lv[pk]), "m", format(t[pk], "%Y-%m-%d %H:%M")),
      if (length(tv) && any(!is.na(tv))) { j <- max(which(!is.na(tv))); ovid_kpi("Water temperature", formatC(tv[j], format = "f", digits = 1), "\u00b0C",
          sprintf("range %s to %s", formatC(min(tv, na.rm = TRUE), format = "f", digits = 1), formatC(max(tv, na.rm = TRUE), format = "f", digits = 1))) },
      ovid_kpi("Manual vs logger", if (nrow(ms)) sub("^-", "\u2212", sprintf("%+.3f", stats::median(ms$difference))) else "\u2014", if (nrow(ms)) "m",
        if (nrow(ms)) sprintf("median, logger \u2212 manual; n = %d, %s", nrow(ms), if (all(abs(ms$difference) <= tape_lim())) paste("all within", tape_cm()) else sprintf("%d beyond %s", sum(abs(ms$difference) > tape_lim()), tape_cm())) else "no manual levels yet",
        level = if (nrow(ms) && any(abs(ms$difference) > tape_lim())) "warn"),   # tape_lim(): Settings tape_warn_m (home.R)
      ovid_kpi("Problems", if (nrow(fl)) num(nrow(fl)) else "none", NULL,
        if (nrow(fl)) tagList(paste(fl[, .(w = paste0(if ("detail" %in% names(fl)) detail[1] else gsub("_", " ", check[1]), ", ", format(as.Date(substr(min(start), 1, 10)), "%d %b"), " to ", format(as.Date(substr(max(end), 1, 10)), "%d %b"))), by = check]$w, collapse = "; "), tags$br(), tags$a(href = "#", `data-ovid-go` = "Needs", "Decide on Needs you \u203a")) else "nothing flagged",
        level = if (nrow(fl)) "warn")) })
  observe({ input$nav; st <- site_cur(); req(identical(input$nav, "Site"), st); m <- lv_load(st, TRUE); req(m)
    fl <- outs()$flags; iss <- if (nrow(fl)) fl[site == st, .(t0 = as.numeric(parse_dt(start)), t1 = as.numeric(parse_dt(end)), label = gsub("_", " ", check), level = "warn")] else NULL
    session$onFlushed(function() ts_view_send(session, "site_plot", m$t, m$series, zone = m$zone, issues = iss, wb = m$wb, bars = m$bars,
                                                    events = m$events, sources = m$sources, title = m$series[[1]]$name, site = st, file = paste0("wlr_", st, "_level")), once = TRUE) })
  output$site_deps_dl <- downloadHandler(function() paste0(safe_name(site_cur()), "_deployments.csv"), function(file) { o <- outs(); st <- site_cur(); req(o, st)
    fwrite(o$summary[site == st][order(start)], file) })
  output$site_deps <- renderUI({ o <- outs(); st <- site_cur(); req(o, st); r <- o$summary[site == st][order(start)]; if (!nrow(r)) return(p(class = "dim", "No deployments."))
    md <- o$loggers$model[match(r$serial, o$loggers$serial)]; fl <- if (nrow(o$flags)) o$flags[site == st] else data.table()
    stat <- vapply(seq_len(nrow(r)), function(i) { f <- fl[logger_id == r$serial[i]]; if (nrow(f)) paste(unique(gsub("_", " ", f$check)), collapse = ", ") else "ok" }, "")
    tags$table(class = "table d-num", tags$thead(tags$tr(lapply(c("Logger", "Model", "Role", "From", "To", "Hang (m)", "Measured to", "Records", "Status"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(tags$td(tags$b(r$serial[i])), tags$td(md[i] %|% ""), tags$td(r$kind[i]), tags$td(substr(fmt_ts(r$start[i]), 1, 10)),
        tags$td(substr(fmt_ts(r$end[i]), 1, 10)), tags$td(if (is.na(r$T_ST[i])) "\u2014" else formatC(as.numeric(r$T_ST[i]), format = "f", digits = 2)), tags$td(r$measured_to[i] %|% ""),
        tags$td(num(r$records[i])), tags$td(ovid_flag(stat[i], if (stat[i] == "ok") "ok" else "warn")))))) })
  output$site_dl <- downloadHandler(function() paste0("site_", safe_name(site_cur() %||% "site"), ".csv"), function(file) { d <- load_site(site_cur(), pd())
    keep <- intersect(c("TIMESTAMP", "site", "deployment", "logger_id", "depth_m", "elev_m", "dtw_m", "temp_c", "baro_kpa", "pres_kpa", grep("_flag$", names(d), value = TRUE)), names(d))
    x <- d[, ..keep]; x[, TIMESTAMP := fmt_ts(TIMESTAMP)]; fwrite(x, file) })

  # Needs you: the one review list (core flow.R): the shown sites' flags, loggers that look moved and the run's findings
  rl_flags <- reactive({ o <- outs(); f <- o$flags; if (is.null(f) || !nrow(f)) return(NULL); f <- copy(f)[site %in% site_shown()]
    f[, serial := as.character(logger_id)]; review_flag_items(f, "site", keep = "serial") })
  review <- review_list_server("rl", reactive(rbind(rl_flags(),
      # safe: suggestions only
      tryCatch(gb$suggestions()[, .(kind = "moved", where = serial, what, serial, at)], error = function(e) NULL), wl_notes(), fill = TRUE)),
    reactive(review_path(pd())), period = if (!VIEWER) E$period, split = function(s, a) gb$accept(s, a),
    open = function(r) { updateSelectInput(session, "ex_site", selected = r$where); go("Explore") },
    remove = function(r) {                                                                     # a removal (corrections over a period)
      ps <- unique(ifelse(strsplit(r$parameter, ", ", fixed = TRUE)[[1]] %in% c("depth_m", "elev_m", "dtw_m", "dp_kpa"), "pres_kpa", strsplit(r$parameter, ", ", fixed = TRUE)[[1]]))  # a level is removed at its source
      for (p in ps) m_man$append(data.frame(serial = r$serial, site = r$where, start = r$start, end = r$end, parameter = p, min_value = "", max_value = "",
        reason = paste("Needs you:", r$what), author = "", stringsAsFactors = FALSE)) })
  if (!VIEWER) unsaved_server("uns", editors = E, on_apply = mark)
  output$saved_hist <- renderUI({ rv$tick; for (m in E) if (is.function(m$data)) m$data(); flow_saved(pd()) })

  # the information panel and the status line of each step
  output$flow_info <- renderUI({ o <- outs(); s <- site_all(); x <- review$items(); n_iss <- sum(x$kind == "flag")
    gp <- if (!is.null(o) && nrow(o$gaps)) { g <- copy(o$gaps)[order(-as.numeric(gap_hours))][as.numeric(gap_hours) >= 24]
      if (nrow(g)) data.table(where = g$logger_id, what = paste(flow_span(g$gap_hours), "without records"),
                              when = paste(substr(fmt_ts(g$gap_start), 1, 10), "to", substr(fmt_ts(g$gap_end), 1, 10)), go = "Needs") else NULL } else data.table()
    flow_aside(project = list(
        list("Data files", if (!is.null(o)) num(sum(o$files$use %in% TRUE)) else "\u2014"),
        list("Loggers", if (!is.null(o)) sprintf("%d water, %d baro", nrow(o$loggers[kind != "baro"]), nrow(o$loggers[kind == "baro"])) else "\u2014"),
        list("Sites shown", sprintf("%d of %d", length(site_shown()), length(s))),
        list("Deployments", if (!is.null(o)) num(nrow(o$summary)) else "\u2014"),
        list("Records", if (!is.null(o) && nrow(o$summary)) paste(substr(fmt_ts(min(o$summary$start)), 1, 7), "to", substr(fmt_ts(max(o$summary$end)), 1, 7)) else "\u2014"),
        list("Data check", if (is.null(o)) "\u2014" else if (n_iss) paste(n_iss, "to look at") else "nothing flagged", level = if (is.null(o)) NULL else if (n_iss) "warn" else "ok", go = "Needs"),
        list("Last run", if (!is.null(o)) format(o$when, "%Y-%m-%d %H:%M") else "never")),
      gaps = gp) })
  observe({ o <- outs(); x <- review$items(); s <- if (!is.null(o)) o$summary else data.table()
    ovid_steps(session, list(
      Project = if (nzchar(trim(input$project_dir %||% ""))) list("ok", "Folders set") else list("off", "Choose the folders"),
      Files = if (!is.null(o)) list("ok", paste(num(sum(o$files$use %in% TRUE)), "files used")) else list("off", "Not read yet"),
      Metadata = if (!is.null(o) && nrow(o$problems) && any(grepl("not in the metadata", o$problems$problem))) list("warn", paste(sum(grepl("not in the metadata", o$problems$problem)), "loggers without a site"))
                 else if (!is.null(o)) list("ok", "Every logger placed") else list("off", ""),
      Shown = list("info", sprintf("%d of %d shown", length(site_shown()), length(site_all()))),
      Baro = if (nrow(s) && any(s$kind == "water" & s$baro_missing > 0, na.rm = TRUE)) list("warn", "Gaps without a barologger") else if (nrow(s)) list("ok", "") else list("off", ""),
      Datum = if (nrow(s) && any(s$kind == "water" & is.na(s$T_ST))) list("warn", paste(sum(s$kind == "water" & is.na(s$T_ST)), "without a hang length")) else if (nrow(s)) list("ok", "") else list("off", ""),
      Corrections = corrections_status(corrections_cfg(current_cfg(), live_settings(SETTINGS_FILE))),   # pages/utils_setup.R; a switch flipped on the list counts at once
      Needs = if (nrow(x)) list("warn", paste(nrow(x), "to look at")) else if (!is.null(o)) list("ok", "Nothing waits") else list("off", "Run once first"),
      Export = if (!is.null(o)) list("info", "One report, every site") else list("off", "")))
  })
