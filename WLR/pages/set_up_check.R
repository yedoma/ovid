# WLR/pages/set_up_check.R  --  Set-up check.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Set-up check -------------------------------------------------------------------------------------------------------------------
  pf_data <- reactive({ rv$pf; input$pf_again; live_config(pd()); tryCatch(setup_check(current_cfg()), error = function(e) data.table(level = "error", area = "Set-up check", item = "", problem = conditionMessage(e), fix = "")) })
  output$pf_banner <- renderUI({ p <- pf_data(); ne <- sum(p$level == "error"); nw <- sum(p$level == "warning")
    if (ne) ovid_notice("error", paste(ne, if (ne == 1) "thing would be ignored or misapplied" else "things would be ignored or misapplied"), paste(nw, if (nw == 1) "warning as well" else "warnings as well"))
    else if (nw) ovid_notice("warn", paste(nw, if (nw == 1) "warning" else "warnings"), "The run will work; some values will be blank.")
    else ovid_notice("ok", "Nothing to fix", "The set-up is consistent.") })
  output$pf_tbl <- renderDT(ovid_dt(pf_data(), page = 10) %>% formatStyle("level", fontWeight = 600, color = styleEqual(c("error", "warning", "info"), c(NORD$red, NORD$yellow, NORD$frost))))
  tst_rows <- reactive({ o <- outs(); rv$pf; md <- if (!is.null(o)) o$md else NULL; if (is.null(md) || !nrow(md)) return(data.table())
    hang_lengths(md, o$deps) })
  output$pf_tst <- renderDT({ h <- tst_rows(); validate(need(nrow(h) > 0, "Appears after the first run."))
    ovid_dt(h[, .(site, year, serial, `T_ST (m)` = T_ST, source = T_ST_source, status, `other years (cm)` = T_ST_years)], selection = "single", page = 12) %>%
      formatStyle("status", color = styleEqual("ok", NORD$green, default = NORD$yellow)) })
  observeEvent(input$pf_tst_rows_selected, { h <- tst_rows()[input$pf_tst_rows_selected]; o <- outs()
    yrs <- o$md[site == h$site & !is.na(T_ST) & year != h$year][order(-year)]
    updateSelectInput(session, "pf_tst_year", choices = c("(type a length instead)" = "", setNames(as.character(yrs$year), sprintf("%d sheet: %.1f cm", yrs$year, yrs$T_ST * 100)))) })
  observeEvent(input$pf_tst_apply, { s <- input$pf_tst_rows_selected; if (!length(s)) return(note("Select a deployment in the table first.", type = "warning"))
    h <- tst_rows()[s]; cm <- input$pf_tst_cm; yr <- input$pf_tst_year %||% ""
    if ((is.null(cm) || is.na(cm)) && !nzchar(yr) && !nzchar(input$pf_tst_ref %||% "")) return(note("Type a length, pick a year, or set where it was measured from.", type = "warning"))
    row <- data.frame(serial = h$serial, year = as.character(h$year), site = "", deployed = "", removed = "", T_ST_m = if (!is.null(cm) && !is.na(cm)) as.character(cm / 100) else "",
                      T_ST_from_year = if (is.null(cm) || is.na(cm)) yr else "", reference = input$pf_tst_ref %||% "", reason = trim(input$pf_tst_why), author = current_user(session), stringsAsFactors = FALSE)
    if (m_dep$append(row)) { mark(); note("Hang length set for ", h$site, " ", h$year, ". Run the pipeline to apply it.") } })
