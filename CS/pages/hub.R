# CS/pages/hub.R  --  Hub: station list, downloads.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Hub: station list, downloads ------------------------------------
  home_rows <- reactive({ o <- outs(); req(o, nrow(o$stations) > 0); s <- copy(o$stations); if (!"values_flagged" %in% names(s)) s[, values_flagged := NA_real_]
    s[, .(first = min(first), last = max(last), records = sum(as.numeric(records)), tables = .N, loggers = paste(unique(unlist(strsplit(as.character(loggers), ", "))), collapse = ", "),
          values_removed = sum(as.numeric(values_removed)), values_flagged = sum(as.numeric(values_flagged))), by = station][order(station)] })
  output$home_st <- renderDT({ s <- home_rows()
    d <- data.table(Station = s$station, From = substr(fmt_ts(s$first), 1, 10), To = substr(fmt_ts(s$last), 1, 10),
                    Years = round(as.numeric(difftime(s$last, s$first, units = "days")) / 365.25, 1), Records = s$records, `Logger tables` = s$tables, Loggers = s$loggers,
                    `Values removed` = s$values_removed, `Values flagged` = s$values_flagged)
    formatStyle(formatRound(ovid_dt(d, selection = "single", page = 10), c("Records", "Values removed", "Values flagged"), 0), "Station", fontWeight = 600) })
  open_station <- function(v) { updateSelectInput(session, "ex_station", selected = v); go("Explore") }
  observeEvent(input$home_st_rows_selected, { s <- home_rows(); i <- input$home_st_rows_selected; req(length(i) == 1, i <= nrow(s)); open_station(s$station[i]) })
  save_png <- function(g, file) { n <- tryCatch(length(unique(ggplot_build(g)$layout$layout$ROW)), error = function(e) 1)
    ggsave(file, g, width = 12, height = min(48, 1.8 + 2.9 * n * psz()), dpi = 160, bg = GB$bg, limitsize = FALSE) }
  output$ex_png <- downloadHandler(filename = function() paste0(safe_name(paste(ex_key(), collapse = "__")), "_plot_", Sys.Date(), ".png"),
    content = function(f) save_png(if (identical(input$ex_kind %||% "ts", "ts")) ex_g() else ex2_g(), f))
  output$pr_png <- downloadHandler(filename = function() paste0(safe_name(paste(pr_key(), collapse = "__")), "_precip_", input$pr_chart, ".png"), content = function(f) save_png(pr_g(), f))
  pdf_for <- function(k, f, fmt = "pdf") { g <- m_gr$data(); withProgress(message = "Writing the report", value = 0.5, {
    r <- station_report(k[1], k[2], pd(), tempfile("report_"), groups = if (is.null(g) || !nrow(g)) default_groups() else as.data.table(g), formats = fmt, opts = rpo())
    file.copy(r[1], f, overwrite = TRUE) }) }
  # Export report: the first station picked on Export, in the one report format chosen there
  xp_one <- reactive({ k <- input$xp_keys; validate(need(length(k) > 0, "Pick a station table first.")); strsplit(k[1], "|||", fixed = TRUE)[[1]][1] })
  output$xp_pdf <- downloadHandler(filename = function() paste0(report_file(rpo(), paste0(safe_name(xp_one()), "_report"), name = xp_one(), app = "CS"), ".", one_report_format(input$xp_rep)),
                                   content = function(f) pdf_for(c(xp_one(), ""), f, one_report_format(input$xp_rep)))
