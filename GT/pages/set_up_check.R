# GT/pages/set_up_check.R  --  Set-up check.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Set-up check -------------------------------------------------------------------------------------------------------
  pf_data <- reactive({ rv$pf; input$pf_again; live_config(pd()); tryCatch(setup_check(current_cfg()), error = function(e) data.table(level = "error", area = "Set-up check", item = "", problem = conditionMessage(e))) })
  output$pf_banner <- renderUI({ p <- pf_data(); ne <- sum(p$level == "error"); nw <- sum(p$level == "warning")
    if (ne) ovid_notice("error", paste(ne, "thing(s) would be ignored or misapplied"), "Fix them below before the next run.")
    else if (nw) ovid_notice("warn", paste(nw, "warning(s)"), "The run will work; these are worth a look.")
    else ovid_notice("ok", "The set-up looks right", paste(nrow(p), "note(s).")) })
  output$pf_tbl <- renderDT(ovid_dt(pf_data(), page = 15) |> formatStyle("level", color = styleEqual(c("error", "warning", "note"), c(MI$red, MI$gold, MI$fg4)), fontWeight = 600))
  output$pf_problems <- renderDT({ o <- outs(); req(o); validate(need(nrow(o$problems) > 0, "None.")); ovid_dt(o$problems, page = 10) })
