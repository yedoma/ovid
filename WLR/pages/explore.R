# WLR/pages/explore.R  --  Explore.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore -----------------------------------------------------------------------------------------------------------
  observe({ o <- outs(); req(o); s <- sort(unique(o$summary$site)); cur <- isolate(input$ex_site); updateSelectInput(session, "ex_site", choices = s, selected = if (isTRUE(cur %in% s)) cur else s[1]) })
  observe({ o <- outs(); req(o, input$ex_site); st <- input$ex_site; d <- o$summary[site == st][order(start)]
    ch <- c(setNames(d$deployment, paste0(d$year, "  (serial ", d$serial, ")")), if (nrow(d) > 1) c("Every year together" = "all"))
    updateSelectInput(session, "ex_dep", choices = ch, selected = if (isolate(input$ex_dep) %in% ch) isolate(input$ex_dep) else ch[length(d$deployment)]); rv$zoom <- NULL })
  ex_data <- reactive({ req(input$ex_dep); if (input$ex_dep == "all") load_site(input$ex_site, pd()) else load_deployment(input$ex_dep, pd()) })
  ex_serials <- reactive({ o <- outs(); st <- input$ex_site; if (identical(input$ex_dep, "all")) unique(o$summary[site == st, serial]) else o$summary[deployment == input$ex_dep, serial] })
  ex_spans <- reactive({ if (!isTRUE(input$ex_marks)) return(NULL); sr <- ex_serials(); st <- input$ex_site
    mk <- function(tab, what) { if (is.null(tab) || !nrow(tab)) return(NULL); t <- as.data.table(tab)
      t <- t[(nzchar(serial) & norm_serial(serial) %in% sr) | (nzchar(site) & site == st)]; if (!nrow(t)) return(NULL)
      data.table(a = parse_dt(t$start), b = parse_dt(t$end), what = what)[!is.na(a) & !is.na(b)] }
    rbind(mk(m_oow$data(), "out of water"), mk(m_man$data(), "removed by hand")) })
  # The plot: the standard plot (core/tsview.R), one panel per value. Drag selects a stretch; dragging up a panel's value axis
  # narrows it to a band of values (the old box). The edits are on the bar above the selection, each with a reason and initials,
  # and go to the set-up tables (Corrections over a period, Out of water, Deployments) to apply at the next run, with Undo (plot standards A11).
  ex_lab <- function(p) { l <- param_label(p); list(name = sub(" \\([^)]*\\)$", "", l), unit = if (grepl("\\)$", l)) sub("^.*\\(([^)]*)\\)$", "\\1", l) else "") }
  ex_acts <- function(one) if (!VIEWER) c(list(
      list(id = "box", label = "Remove the points in the band", kind = "bad", band = TRUE, title = "Only the points of that panel inside the band; a level, depth or elevation is removed at its water pressure"),
      list(id = "remove", label = "Remove every value in this period", kind = "bad"),
      list(id = "oow", label = "Out of the water", kind = "warn")),
    if (one) list(list(id = "in", label = "Logger went in here", title = "The deployment starts at the end of the selection: everything before it is dropped"),
                  list(id = "out", label = "Logger came out here", title = "The deployment ends at the start of the selection: everything after it is dropped")))
  observe({ d <- ex_data(); req(d, nrow(d) > 0); o <- outs(); sr <- ex_serials(); st <- input$ex_site; one <- !identical(input$ex_dep, "all")
    d <- d[!is.na(TIMESTAMP)][order(TIMESTAMP)][!duplicated(TIMESTAMP)]
    pp <- intersect(input$ex_params %||% "depth_m", names(d)); if (!length(pp)) pp <- intersect("depth_m", names(d)); req(length(pp) > 0)
    rm <- rbindlist(lapply(sr, removed_values, project_dir = pd()), fill = TRUE)
    has <- function(x, k) !is.null(x) && nrow(x) && all(k %in% names(x))                       # no workbook, no tape checks: nothing to mark
    vis <- if (has(o$md, c("serial", "site", "visit_time"))) o$md[serial %in% sr & site == st, visit_time] else NULL
    tp <- if (has(o$checks, c("serial", "site", "visit_time", "h_tape"))) o$checks[serial %in% sr & site == st & !is.na(h_tape)] else NULL
    ms <- if (isTRUE(input$ex_meas) && has(o$meas, c("site", "parameter", "time", "manual"))) o$meas[site == st] else NULL
    series <- lapply(pp, function(p) { lb <- ex_lab(p); fc <- paste0("flag_", p)
      dots <- list(if (has(rm, c("parameter", "old_value"))) { r <- rm[parameter == p & !is.na(old_value)]; if (nrow(r)) list(t = r$TIMESTAMP, v = r$old_value, kind = "removed", label = "removed by a rule or by hand") },
                   if (fc %in% names(d)) { k <- which(!is.na(d[[fc]]) & nzchar(d[[fc]]) & !is.na(d[[p]])); if (length(k)) list(t = d$TIMESTAMP[k], v = d[[p]][k], kind = "flagged", label = "flagged") })
      mt <- numeric(); mv <- numeric(); mx <- character()                                         # tape checks and manual readings: diamonds
      if (p == "depth_m" && !is.null(tp) && nrow(tp)) { mt <- as.numeric(tp$visit_time); mv <- as.numeric(tp$h_tape); mx <- rep("tape", nrow(tp)) }
      if (!is.null(ms) && nrow(m <- ms[parameter == p])) { mt <- c(mt, as.numeric(m$time)); mv <- c(mv, as.numeric(m$manual))
        mx <- c(mx, if ("difference" %in% names(m)) ifelse(is.na(m$difference), "", sprintf("%+.3g", m$difference)) else rep("", nrow(m))) }
      list(name = lb$name, key = p, unit = lb$unit, v = d[[p]], digits = if (lb$unit == "m") 3 else 2, dots = Filter(Negate(is.null), dots),
           marks = if (length(mt)) list(t = mt, v = mv, label = "tape check or manual reading", text = mx)) })
    dd <- o$summary[site == st][order(start)]
    ev <- rbindlist(list(if (length(vis)) data.table(t = as.numeric(vis[!is.na(vis)]), label = "Field visit", note = ""),
                         if (!one && nrow(dd) > 1) data.table(t = as.numeric(dd$start[-1]), label = "Deployment starts", note = paste("logger", dd$serial[-1]))), fill = TRUE)
    sp <- ex_spans(); sp <- if (NROW(sp)) data.frame(t0 = as.numeric(sp$a), t1 = as.numeric(sp$b), label = sp$what)
    wb <- if (one) list(key = "deployment", value = input$ex_dep, params = pp) else if (nrow(dd)) list(key = "deployment", value = dd$deployment[nrow(dd)], params = pp, from = I(as.numeric(dd$start)), values = I(dd$deployment))
    fn <- paste0("wlr_", st, if (one) paste0("_", input$ex_dep) else "_every_year")
    session$onFlushed(function() ts_view_send(session, "ex_tsv", d$TIMESTAMP, series, zone = zone_text(SETTINGS$base_offset_h %||% BASE_OFFSET), wb = wb,
      events = if (nrow(ev)) ev[!is.na(t)][seq_len(min(.N, 300))], spans = sp, title = st, site = st, file = fn,
      actions = ex_acts(one), ask = list(choices = c("retrieval", "download", "launched before deployment", "data error", "other"), choices_label = "If out of the water, why",
                                         note = "Added to the set-up tables; applied at the next run")), once = TRUE) })
  ts_view_undo(input, session, "ex_tsv")
  observeEvent(input$ex_tsv_act, { a <- input$ex_tsv_act; if (VIEWER) return()
    why <- trim(a$reason %||% ""); who <- trim(a$author %||% ""); if (!nzchar(why)) return(note("Give a reason first.", type = "warning"))
    r <- as.POSIXct(c(a$t0, a$t1), origin = "1970-01-01", tz = "UTC"); d <- ex_data(); req(d); sr <- ex_serials()
    done <- function(ok, msg) if (isTRUE(ok)) { mark(); ts_view_done(session, "ex_tsv", paste(msg, "Run the pipeline to apply it.")) }
    rows_for <- function() { k <- d$TIMESTAMP >= r[1] & d$TIMESTAMP <= r[2]; ser <- unique(d$logger_id[k]); if (!length(ser)) ser <- sr
      data.frame(serial = as.character(ser), site = input$ex_site, start = fmt_ts(r[1]), end = fmt_ts(r[2]), reason = why, author = who, stringsAsFactors = FALSE) }
    add <- function(parameter, lo = "", hi = "", msg) { rows <- rows_for(); rows$parameter <- parameter; rows$min_value <- lo; rows$max_value <- hi; done(m_man$append(rows), msg) }
    set_dep_date <- function(col, when) { o <- outs(); dp <- o$summary[deployment == input$ex_dep]; req(nrow(dp) > 0)
      row <- data.frame(serial = dp$serial, year = as.character(dp$year), site = "", deployed = "", removed = "", T_ST_m = "", T_ST_from_year = "", reference = "",
                        reason = why, author = who, stringsAsFactors = FALSE); row[[col]] <- fmt_ts(when)
      done(m_dep$append(row), paste0("Deployment ", if (col == "deployed") "start" else "end", " set to ", fmt_ts(when), ".")) }
    switch(a$act,
      box = { p <- a$key %||% ""; lo <- as.numeric(a$lo %||% NA); hi <- as.numeric(a$hi %||% NA); req(p %in% names(d), is.finite(lo), is.finite(hi))
        if (p %in% c("depth_m", "elev_m", "dtw_m", "dp_kpa")) {
          # derived values are removed at their source: the water pressure readings at those times
          i <- which(d$TIMESTAMP >= r[1] & d$TIMESTAMP <= r[2] & !is.na(d[[p]]) & d[[p]] >= lo & d[[p]] <= hi); if (!length(i)) return(note("No points inside the band.", type = "warning"))
          run <- split(i, cumsum(c(1, diff(i) != 1)))                      # consecutive points become one span
          rows <- data.frame(serial = vapply(run, function(j) as.character(d$logger_id[j[1]]), ""), site = input$ex_site, start = vapply(run, function(j) fmt_ts(d$TIMESTAMP[j[1]]), ""),
                             end = vapply(run, function(j) fmt_ts(d$TIMESTAMP[j[length(j)]]), ""), parameter = "pres_kpa", min_value = "", max_value = "", reason = why, author = who, stringsAsFactors = FALSE)
          done(m_man$append(rows), paste0(length(i), " point", if (length(i) != 1) "s", " will be removed."))
        } else add(p, sprintf("%.6g", lo), sprintf("%.6g", hi), "Points in the band will be removed.") },
      remove = add("", msg = "Removal added."),
      oow = { rows <- rows_for(); rows$cause <- a$cause %||% "other"; done(m_oow$append(rows), "Out-of-water period added.") },
      `in` = set_dep_date("deployed", r[2]),
      out = set_dep_date("removed", r[1])) })
