# GT/pages/export_page.R  --  Export.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Export ---------------------------------------------------------------------------------------------------------------
  output$xp_dl <- downloadHandler(function() paste0("ovid_gt_export_", format(Sys.Date()), ".zip"), function(file) {
    s <- input$xp_sites; if (!length(s)) s <- site_names(); dr <- input$xp_dates
    withProgress(message = "Building the export", value = 0, build_export(pd(), s, file, if (length(dr) == 2) dr[1] else NULL, if (length(dr) == 2) dr[2] else NULL,
      input$xp_res, input$xp_layout, isTRUE(input$xp_flags), input$xp_tz %||% BASE_OFFSET, isTRUE(input$xp_soil), isTRUE(input$xp_summ), isTRUE(input$xp_audit),
      progress = function(i, n, what) setProgress(i / n, detail = what), reports = character(), flag_scheme = input$xp_scheme %||% "wmo", netcdf = isTRUE(input$xp_nc),
      report_opts = rpo(), depth_positive = set_now("depth_positive"))) })
  # the report layout (core report_options.R): titles, file names, sections and axes shared by the sites exported together
  rpo <- report_options_server("rpo", sections = GT_REPORT_SECTIONS, names_for = function(o) { s <- input$xp_sites; if (!length(s)) s <- site_names(); if (!length(s)) return(NULL)
    # safe: no profiles: the export has no profile column
    pr <- tryCatch(gt_profiles(pd(), s), error = function(e) NULL)
    data.table(name = paste0(s, if (!is.null(pr)) paste0("  (", pr$profile[match(s, pr$site)], " profile)") else ""),
               title = vapply(s, function(x) report_title(o, paste0(x, ": ground temperature"), name = x, app = "GT"), ""),
               file = vapply(s, function(x) report_file(o, safe_name(x), name = x, app = "GT"), "")) })
  # one report, or every report of the sites above as a ZIP (the report picker, report_choices())
  output$rp_dl <- downloadHandler(function() { k <- report_pick(input$rp_pick)
      if (k$what == "*") "reports.zip" else paste0(if (nzchar(k$what)) report_file(rpo(), safe_name(k$what), name = k$what, app = "GT") else "run_report", ".", k$format) },
    function(file) withProgress(message = "Building the report", value = 0.4, { k <- report_pick(input$rp_pick)
      if (k$what == "*") { s <- input$xp_sites; if (!length(s)) s <- site_names(); d <- tempfile("gtrep_")
        f <- write_gt_reports(pd(), d, s, k$format, progress = function(i, n, what) setProgress(i / n, detail = what), opts = rpo())
        if (!length(f)) stop("No report could be built.")
        zip::zip(file, list.files(d, recursive = TRUE), root = d); unlink(d, recursive = TRUE)
      } else {
        doc <- if (nzchar(k$what)) gt_site_report_doc(k$what, pd(), rpo(), gt_report_limits(pd(), k$what, rpo()$axis)) else gt_run_report_doc(pd())
        if (is.null(doc)) stop("No data for this site.")
        f <- report_write(doc, tempfile("report_"), k$format, MI); file.copy(f[1], file, overwrite = TRUE) } }))
