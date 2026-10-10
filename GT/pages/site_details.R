# GT/pages/site_details.R  --  Site details.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Site details ------------------------------------------------------------------------------------------------------
  sd_sens <- reactive({ req(input$sd_site); s <- as.data.table(E$sensors$data() %||% data.frame()); if (nrow(s) && "site" %in% names(s)) s[site == input$sd_site] else s })
  output$sd_site_ui <- renderUI({ req(input$sd_site); sc <- as.data.table(E$sites$data() %||% data.frame()); r <- if (nrow(sc) && "site" %in% names(sc)) sc[site == input$sd_site] else sc
    if (!nrow(r)) return(p(class = "dim", "This site has no row on the Sites page yet."))
    r <- r[1]; f <- function(v) { x <- r[[v]]; if (is.null(x) || !nzchar(x)) span(class = "dim", "not recorded") else x }
    kv(Site = r$site, `Site group` = f("site_group"), Landcover = f("landcover"), Instrument = f("instrument"), Status = f("status"),
       Location = if (nzchar(r$easting)) paste0("UTM ", r$utm_zone, " ", r$easting, " E, ", r$northing, " N") else if (nzchar(r$latitude)) paste0(r$latitude, ", ", r$longitude) else span(class = "dim", "not recorded"),
       `Installed` = f("install_date"), `Drilling method` = f("drilling_method"), `Borehole depth (m)` = f("borehole_depth_m"), `Sensor type` = f("sensor_type"),
       `Sensor accuracy` = f("sensor_accuracy"), Calibration = f("calibration"), Description = f("description"), `From` = f("source")) })
  output$sd_record_ui <- renderUI({ o <- outs(); req(o, input$sd_site); s <- o$ss[site == input$sd_site]; if (!nrow(s)) return(p(class = "dim", "No data for this site in the last run."))
    lg <- unique(trim(strsplit(s$loggers, ",")[[1]])); fl <- o$files[serial %in% lg & use %in% TRUE]; gp <- o$gaps[logger_id %in% paste(s$instrument, lg)]
    ck <- o$clock[logger_id %in% paste(s$instrument, lg)]; d <- o$daily[site == input$sd_site]
    kv(Loggers = paste(lg, collapse = ", "), `First reading` = substr(s$first, 1, 16), `Last reading` = substr(s$last, 1, 16),
       `Logging interval` = paste(unique(fmt_step(o$loggers[serial %in% lg, interval_s])), collapse = ", "),
       `Downloads read` = paste0(nrow(fl), " (", paste(unique(fl$format), collapse = ", "), ")"),
       Depths = paste0(s$depths, " (", s$depth_list, ")", if (s$assumed_depths > 0) paste0(" \u00b7 ", s$assumed_depths, " assumed") else ""),
       `Values kept` = num(s$values), Flagged = num(s$flagged),
       `Complete days` = if (nrow(d)) paste0(num(d[complete == TRUE, uniqueN(date)]), " of ", num(uniqueN(d$date))) else "",
       `Gaps` = if (nrow(gp)) paste0(nrow(gp), ", longest ", round(max(gp$length_h) / 24, 1), " days") else "none",
       `Clock events` = if (nrow(ck)) paste(paste0(ck$rule, " (", ck$n, ")"), collapse = ", ") else "none") })
  output$sd_stats <- renderDT({ o <- outs(); req(o, input$sd_site); h <- o$health[site == input$sd_site]; a <- o$annual[site == input$sd_site & year_type == "calendar" & complete == TRUE]
    last <- if (nrow(a)) a[year == max(year), .(depth_m, last_complete_year = year, MAGT = mean, annual_min = min, annual_max = max)] else data.table(depth_m = numeric())
    d <- load_site(input$sd_site, pd()); req(d)
    st <- d[, .(values = .N, flagged = sum(nzchar(flag)), min = round(min(value), 3), mean = round(mean(value), 3), max = round(max(value), 3), sd = round(stats::sd(value), 3),
                days_below_0 = uniqueN(as.Date(TIMESTAMP)[value < 0]), first = fmt_ts(min(TIMESTAMP)), last = fmt_ts(max(TIMESTAMP))), by = .(depth_m, depth_source)]
    st <- merge(st, h[, .(depth_m, values_read, errors, removed_qc, pct_valid, health)], by = "depth_m", all.x = TRUE)
    st <- merge(st, last, by = "depth_m", all.x = TRUE)
    ovid_dt(st[order(depth_m)], page = 15) |> health_style("health") })
  output$sd_sensors <- renderDT(ovid_dt(sd_sens(), page = 10))
  output$sd_meta <- renderDT({ o <- outs(); req(o, input$sd_site); s <- input$sd_site; lg <- unique(sd_sens()$serial)
    x <- o$mix[site_key %in% c(s, lg, sub(" \\[.*$", "", s))]; r <- o$registry
    if (nrow(r)) { rr <- r[name == s | sn %in% lg]; if (nrow(rr)) x <- rbind(x, rbindlist(lapply(seq_len(nrow(rr)), function(i) data.table(site_key = rr$name[i], field = names(rr), value = as.character(unlist(rr[i])), file = "registry", sheet = ""))), fill = TRUE) }
    req(nrow(x) > 0); ovid_dt(unique(x[!is.na(value) & nzchar(value) & value != "-999"]), page = 15) })
  output$sd_notes <- renderUI({ o <- outs(); req(o, input$sd_site); lg <- unique(sd_sens()$serial)
    terms <- c(input$sd_site, lg, sub("^0+", "", lg), o$ss[site == input$sd_site, site_group])
    n <- notes_for(o$notes, terms)
    if (!nrow(n)) return(empty_msg("No paragraph of the field notes mentions this site or its serial numbers."))
    tagList(lapply(seq_len(min(nrow(n), 60)), function(i) div(class = "note", tags$b(paste0(n$file[i], if (!is.na(n$year[i])) paste0(", ", n$year[i]) else "")), n$text[i]))) })
