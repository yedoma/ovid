# WLR/pages/export_page.R  --  Export.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Export ----------------------------------------------------------------------------------------------------------------------
  # One export: the sites picked (blank = every site) in the format chosen (the package, one CSV, one file per site, or a report)
  xp_sites <- function() { o <- outs(); if (length(input$cd_sites)) input$cd_sites else if (!is.null(o)) sort(unique(o$summary$site)) else character() }
  # the report layout (core report_options.R): titles, file names, sections and axes shared by the sites exported together
  rpo <- report_options_server("rpo", sections = WLR_REPORT_SECTIONS, names_for = function(o) {
    # safe: a site list for choosing only
    s <- tryCatch({ x <- read_out(pd(), "summary.csv"); if (length(input$cd_sites)) x <- x[site %in% input$cd_sites]; unique(x$site) }, error = function(e) character()); if (!length(s)) return(NULL)
    data.table(name = s, title = vapply(s, function(x) report_title(o, paste0(x, ": water level and temperature"), name = x, app = "WLR"), ""),
               file = vapply(s, function(x) report_file(o, safe_name(x), name = x, app = "WLR"), "")) })
  observe({ o <- outs(); req(o); updateSelectizeInput(session, "cd_sites", choices = sort(unique(o$summary$site)), selected = isolate(input$cd_sites)) })
  observeEvent(input$cd_sites, { o <- outs(); req(o); d <- o$summary[site %in% xp_sites()]; req(nrow(d) > 0)
    updateDateRangeInput(session, "cd_dates", start = as.Date(min(d$start, na.rm = TRUE)), end = as.Date(max(d$end, na.rm = TRUE))) }, ignoreNULL = FALSE)
  cd_from_to <- function() { dr <- input$cd_dates; list(if (length(dr) && !is.na(dr[1])) dr[1], if (length(dr) > 1 && !is.na(dr[2])) dr[2]) }
  cd_get <- function() { st <- xp_sites(); req(length(st), length(input$cd_values)); ft <- cd_from_to()
    corrected_data(pd(), st, input$cd_values, input$cd_res, ft[[1]], ft[[2]], input$cd_layout, isTRUE(input$cd_flags), input$cd_scheme %||% "wmo", input$cd_versions %||% "both") }
  output$cd_count <- renderText({ if (!length(xp_sites())) return("Run the pipeline first.")
    if (!(input$xp_fmt %||% "package") %in% c("csv", "sites")) return(sprintf("%d site(s)", length(xp_sites())))
    # safe: nothing to export yet
    d <- tryCatch(cd_get(), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) "Nothing to export for this choice." else sprintf("%s rows, %d columns", num(nrow(d)), ncol(d)) })
  observeEvent(input$cd_all, { o <- outs(); req(o); updateSelectizeInput(session, "cd_sites", selected = sort(unique(o$summary$site))) })
  # a report: the one site picked, else the run report
  rp_site <- function() if (length(input$cd_sites) == 1) input$cd_sites else ""
  xp_name <- function() switch(input$xp_fmt %||% "package",
    package = paste0("wlr_export_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    csv = paste0("wlr_corrected_", if (length(input$cd_sites) == 1) safe_name(input$cd_sites) else "sites", "_", input$cd_res, ".csv"),
    sites = paste0("wlr_corrected_all_sites_", input$cd_res, "_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    report = paste0(if (nzchar(rp_site())) report_file(rpo(), safe_name(rp_site()), name = rp_site(), app = "WLR") else "run_report", ".", input$rp_fmt))
  xp_write <- function(file) switch(input$xp_fmt %||% "package",
    package = { s <- read_out(pd(), "summary.csv"); deps <- if (length(input$cd_sites)) s[site %in% input$cd_sites, deployment] else NULL
      withProgress(message = "Building the export", value = 0.1, make_export_zip(pd(), file, deps, reports = input$xp_rep %||% character(),
        progress = function(i, n, what) setProgress(0.1 + 0.8 * i / n, detail = paste("report:", what)), versions = input$cd_versions %||% "both", report_opts = rpo(),
        hide_accepted = isTRUE(corrections_cfg(CFG)$hide_accepted_flags))) },   # Automatic corrections switch (R/config.R)
    csv = fwrite(cd_get(), file),
    sites = { st <- xp_sites(); req(length(st), length(input$cd_values)); ft <- cd_from_to()
      withProgress(message = "Building the export", value = 0, corrected_data_zip(pd(), st, file, input$cd_values, input$cd_res, ft[[1]], ft[[2]],
        input$cd_layout, isTRUE(input$cd_flags), input$cd_scheme %||% "wmo", input$cd_versions %||% "both", progress = function(i, n, what) setProgress(i / n, detail = what), netcdf = isTRUE(input$cd_nc))) },
    report = withProgress(message = "Building the report", value = 0.4, {
      doc <- if (nzchar(rp_site())) wlr_site_report_doc(rp_site(), pd(), rpo()) else wlr_run_report_doc(pd())
      f <- report_write(doc, tempfile("report_"), input$rp_fmt, NORD); file.copy(f[1], file, overwrite = TRUE) }))
  output$xp_dl <- downloadHandler(xp_name, xp_write)
  fv_tape_tab <- reactive({ o <- outs(); req(o); f <- file.path(out_dir(pd(), "audit"), "tape_offsets.csv")
    if (!file.exists(f)) return(data.table()); x <- fread(file = f, colClasses = list(character = c("serial", "year")))
    if (!isTRUE(input$fv_tape_all)) x <- x[status != "ok"]
    if (all(c("checks", "largest_diff_m") %in% names(x))) x[checks < 2, largest_diff_m := NA]   # one check has no spread: its median is the check
    x[order(status != "check the metadata", -abs(median_diff_m))] })
  output$fv_tape <- renderDT({ x <- fv_tape_tab()
    validate(need(nrow(x), if (isTRUE(input$fv_tape_all)) "No tape checks in the last run." else sprintf("No deployment differs from the tape by more than %g m.", CFG$tape_warn_m)))
    ovid_dt(x, selection = "multiple", page = 10) })
  observeEvent(input$fv_keep, { x <- fv_tape_tab(); s <- input$fv_tape_rows_selected
    if (!length(s)) { showNotification("Select the deployments to keep first.", type = "warning"); return() }
    x <- x[s]; ok <- m_drift$append(data.frame(serial = x$serial, site = x$site, year = x$year, mode = "keep",
      reason = sprintf("tape offset %+.2f m checked and kept as it is", x$median_diff_m), author = current_user(session), stringsAsFactors = FALSE))
    if (isTRUE(ok)) showNotification(sprintf("%d deployment(s) kept as they are. Run the pipeline to clear the warning.", nrow(x)), type = "message") })
  output$fv_drift <- renderDT({ o <- outs(); req(o); f <- file.path(out_dir(pd(), "audit"), "drift_corrections.csv")
    validate(need(file.exists(f), "Nothing yet. Drift is off unless you turn it on in Settings or add a drift row under Quality rules, Corrections over a period; then run the pipeline.")); ovid_dt(fread(file = f), page = 10) })
  hy_rd <- function(dir, f) { o <- outs(); req(o); p <- file.path(out_dir(pd(), dir), f); if (file.exists(p)) fread(file = p) else data.table() }
  hy_dt <- function(x, none) { validate(need(nrow(x) > 0, none)); ovid_dt(x, page = 12) }
  output$hy_unc <- renderDT(hy_dt(hy_rd("audit", "uncertainty.csv"), "No water deployment in the last run."))
  output$hy_be <- renderDT(hy_dt(hy_rd("audit", "barometric_efficiency.csv"), "Needs a week or more of hourly readings with the air pressure moving."))
  output$hy_casing <- renderDT(hy_dt(hy_rd("audit", "casing_movement.csv"), "Needs the casing's height above the ground (TOC_veg) at two visits or more."))
  output$hy_hp <- renderDT(hy_dt(hy_rd("summaries", "hydroperiod.csv"), "Needs the T-coupler to vegetation height (T_veg) or a surveyed ground point (GS)."))
  output$hy_grad <- renderDT(hy_dt(hy_rd("summaries", "gradients.csv"), "Add pairs of wells below (with elevations for both), then run."))
  output$hy_rch <- renderDT(hy_dt(hy_rd("summaries", "recharge.csv"), "Add a specific yield below, then run."))
  output$xp_sum <- renderDT({ o <- outs(); req(o); ovid_dt(o$summary, page = 12) })
