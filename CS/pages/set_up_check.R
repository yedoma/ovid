# CS/pages/set_up_check.R  --  Set-up check.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Set-up check -----------------------------------------------------------
  pf <- reactive({ rv$tick; rv$pf_tick; input$pf_again; for (m in all_tables()) m$data()
    if (VIEWER) return(preflight(list(raw_dirs = "", project_dir = pd()))[0]); tryCatch(preflight(current_cfg()), error = function(e) data.table(level = "error", table = "check", row = "", problem = conditionMessage(e), fix = "")) })
  output$pf_banner <- renderUI({ p <- pf(); k <- preflight_status(p); ne <- sum(p$level == "error"); nw <- sum(p$level == "warning")
    ovid_notice(switch(k, safe = "ok", warnings = "warn", "error"),
        switch(k, safe = "Safe to run", warnings = "Safe to run, with things worth a look", "Needs attention"),
        (switch(k, safe = "Nothing in the set-up tables would be ignored or misapplied.", warnings = paste0(nw, " warning", if (nw != 1) "s", ". Nothing would be ignored, but check that these are what you intend."),
                    paste0(ne, " error", if (ne != 1) "s", if (nw) paste0(" and ", nw, " warning", if (nw != 1) "s"), ". Each error is a row that the run would ignore or misapply.")))) })
  output$pf_tbl <- renderDT({ p <- pf(); validate(need(nrow(p) > 0, "No findings."))
    lab <- c(error = "<span class='d-flag error'>error</span>", warning = "<span class='d-flag warn'>warning</span>")
    ovid_dt(data.table(Table = p$table, Level = unname(lab[p$level]), Row = p$row, Problem = p$problem, `How to fix it` = p$fix), selection = "none", page = 25, escape = FALSE) })
  output$pf_dl <- downloadHandler(filename = function() paste0("setup_check_", Sys.Date(), ".csv"), content = function(f) fwrite(pf(), f))
  output$ver <- renderUI({ o <- outs(); m <- if (is.null(o)) data.table() else o$manifest_run; g <- function(k) if (nrow(m) && k %in% m$key) m[key == k, value][1] else ""
    p(class = "dim", style = "margin-top:18px", "Ovid Core // CS, pipeline version ", PIPELINE_VERSION, if (nzchar(g("run_id"))) tagList(". Last run ", tags$b(g("run_id")), " with version ", g("pipeline_version"), ", set-up fingerprint ", code(g("config_hash")), "."),
      if (nzchar(g("pipeline_version")) && g("pipeline_version") != PIPELINE_VERSION) span(class = "warnline", " The output was made with a different version; run the pipeline to refresh it.")) })

