# GS/pages/quality_control.R  --  QC.
# Part of the GS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, proj, E, mark, cat_s, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- QC
  q1 <- reactive({ req(input$qc_survey); as.list(cat_s()[survey_id == input$qc_survey]) })
  qmeas <- reactive({ req(input$qc_survey); ELEV(); validate(need(has_meas(input$qc_survey), "This catalogue was exported without measurements, so QC cannot be re-run.")); read_meas(input$qc_survey) })
  observeEvent(list(input$profile, input$qc_survey), {
    req(input$profile, input$qc_survey); r <- profile_rows(qc(), input$profile, q1()$type)
    for (i in seq_len(nrow(SLIDERS))) { k <- SLIDERS$rule[i]; v <- suppressWarnings(as.numeric(r[rule == k]$value[1]))
      updateCheckboxInput(session, paste0("on_", k), value = k %in% r$rule)
      if (!is.na(v)) updateSliderInput(session, paste0("v_", k), value = v / SLIDERS$scale[i]) }
    rr <- suppressWarnings(as.numeric(strsplit(r[rule == "rhoa_range"]$value[1] %||% "", ";")[[1]]))
    updateCheckboxInput(session, "on_rhoa_range", value = "rhoa_range" %in% r$rule)
    if (length(rr) == 2 && all(rr > 0)) updateSliderInput(session, "v_rhoa_range", value = log10(rr))
    for (k in TOGGLES$rule) updateCheckboxInput(session, paste0("on_", k), value = k %in% r$rule) }, ignoreInit = TRUE)
  live <- reactive({ rows <- list()
    for (i in seq_len(nrow(SLIDERS))) { k <- SLIDERS$rule[i]
      if (isTRUE(input[[paste0("on_", k)]])) rows[[length(rows) + 1L]] <- data.table(rule = k, value = paste0(if (k == "moving_median") "5;" else "", format(input[[paste0("v_", k)]] * SLIDERS$scale[i], scientific = FALSE, trim = TRUE)), flag = SLIDERS$flag[i]) }
    if (isTRUE(input$on_rhoa_range)) rows[[length(rows) + 1L]] <- data.table(rule = "rhoa_range", value = paste(signif(10^input$v_rhoa_range, 3), collapse = ";"), flag = "extreme apparent resistivity")
    for (i in seq_len(nrow(TOGGLES))) if (isTRUE(input[[paste0("on_", TOGGLES$rule[i])]])) rows[[length(rows) + 1L]] <- data.table(rule = TOGGLES$rule[i], value = "", flag = TOGGLES$flag[i])
    r <- rbindlist(rows); if (!nrow(r)) r <- data.table(rule = "none", value = "", flag = "")
    r[, `:=`(profile = "live", applies_to = "all", enabled = "TRUE", notes = "", rule_id = rule)][, ..QC_COLS] })
  eo_tick <- reactiveVal(0)
  eo_rows <- reactive({ eo_tick(); e <- load_configs(CFG)$electrodes; if (is.null(e) || !nrow(e)) ensure_cols(data.table(), ELECTRODE_COLS)[0] else e })
  # electrodes turned off: in this survey, or in every survey on its transect over a period (pages/utils_setup.R); written
  # through the table's editor, so the Workbench sees the row at once
  eo_here <- reactive({ s <- q1(); electrode_rows_for(eo_rows(), input$qc_survey %||% "", s) })
  output$eo_tbl <- renderDT({ e <- eo_here(); ovid_dt(e[, .(`Applies to` = applies_to, electrode, status, reason, author)], page = 20, opts = list(dom = "t")) })
  output$eo_tr <- renderUI({ tr <- q1()$transect_id %||% ""; span(class = "muted mb-3 d-inline-block", if (nzchar(tr)) paste("Transect", tr) else "This survey is on no transect: set one on the Map or in Archive.") })
  observeEvent(input$qc_survey, { s <- q1(); d <- substr(as.character(s$date %||% ""), 1, 10); d <- if (grepl("^[0-9]{4}-", d)) d else ""
    updateTextInput(session, "eo_from", value = d); updateTextInput(session, "eo_to", value = d) })
  eo_write <- function(rows) { ok <- tryCatch(isTRUE(E$electrodes$append(as.data.frame(rows))), error = function(e) FALSE)
    if (!ok) write_cfg(rbind(ensure_cols(eo_rows(), ELECTRODE_COLS)[, ..ELECTRODE_COLS], rows[, ..ELECTRODE_COLS]), CFG$project_dir, "electrodes", who = "app")
    eo_tick(eo_tick() + 1) }
  observeEvent(input$eo_add, { req(nzchar(trim(input$eo_list)), nzchar(input$qc_survey %||% ""))
    el <- tryCatch(ids_from(gsub(" ", "", input$eo_list)), error = function(e) integer()); if (!length(el) || anyNA(el)) { ovid_field_error("eo_list", "Give electrode numbers such as 12, 15-17."); return() }
    scope <- input$eo_scope %||% "survey"; tr <- q1()$transect_id %||% ""
    if (scope == "transect") {
      if (!nzchar(tr)) return(showNotification("This survey is on no transect: set one on the Map or in Archive first.", type = "warning"))
      bad <- function(x) nzchar(trim(x %||% "")) && is.na(suppressWarnings(as.Date(trim(x), optional = TRUE)))
      if (bad(input$eo_from) || bad(input$eo_to)) return(showNotification("From and to must look like 2024-08-31.", type = "warning")) }
    eo_write(electrode_status_row(input$qc_survey, tr, input$eo_list, input$eo_from, input$eo_to, input$eo_reason, "app", scope)) })
  # back on: this survey's own rows; rows of its transect over a period apply to other surveys too, so they are asked about
  eo_clear_own <- function(also_transect = FALSE) { e <- eo_rows(); sid <- input$qc_survey %||% ""; if (!nrow(e)) return()
    drop <- trim(e$survey_id) == sid
    if (also_transect) { h <- eo_here(); drop <- drop | (do.call(paste, e[, ..ELECTRODE_COLS]) %in% do.call(paste, h[applies_to != "this survey", ..ELECTRODE_COLS])) }
    write_cfg(e[!drop], CFG$project_dir, "electrodes", who = "app"); eo_tick(eo_tick() + 1) }
  observeEvent(input$eo_clear, { h <- eo_here(); nt <- sum(h$applies_to != "this survey")
    if (!nt) return(eo_clear_own())
    showModal(modalDialog(title = "Turn the electrodes back on", easyClose = TRUE,
      p(sprintf("%d row(s) turn electrodes off on this survey's whole transect over a period; they apply to every survey on it in that period.", nt)),
      footer = tagList(modalButton("Cancel"), actionButton("eo_clear_own", "This survey's rows only", class = "btn-outline-primary"),
                       actionButton("eo_clear_all", "Those rows too", class = "btn-danger")))) })
  observeEvent(input$eo_clear_own, { removeModal(); eo_clear_own() })
  observeEvent(input$eo_clear_all, { removeModal(); eo_clear_own(TRUE) })
  qflags <- reactive({ eo_tick(); cf <- load_configs(CFG); qc_survey(qmeas(), input$qc_survey, q1()$type, live(), "live", cf$manual, q1(), cf$electrodes) })
  output$qc_summary <- renderUI({ d <- qmeas(); n <- uniqueN(qflags()$meas_id)
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      value_box("Kept %", sprintf("%.1f%%", 100 * (1 - n / max(nrow(d), 1))), theme = VB()),
      value_box("Kept", num(nrow(d) - n), theme = VB()), value_box("Flagged", num(n), theme = VB())) })
  output$qc_plot <- plotly::renderPlotly(view_pseudosection(view_data(qmeas(), qflags()), input$v_colour %||% "rhoa_ohmm", input$v_show %||% "all",
                                                            PSEUDO_FLAG, input$v_pal %||% "Spectral", !identical(input$v_scale, "linear"), PSEUDO_SIZE, c(input$v_min %||% RHOA_RANGE[1], input$v_max %||% RHOA_RANGE[2])))
  output$qc_flags <- renderDT(ovid_dt(qflags()[, .(Measurements = uniqueN(meas_id)), by = .(Flag = flag, Rule = RULE_HELP$name[match(rule, RULE_HELP$rule)] %|na|% rule, Limit = value)][order(-Measurements)], page = 20, opts = list(dom = "t")))
  output$dl_archive <- downloadHandler(filename = function() paste0("ovid_gs_catalogue_", format(Sys.Date()), ".zip"),
    content = function(file) withProgress(message = "Packing the catalogue", export_archive(file, CFG$project_dir, isTRUE(input$x_meas))))
  # deep links: ?page=Survey&survey=ERT-...&tab=Details&profile=strict (used for the guide's screenshots; handy for bookmarks)
  observeEvent(session$clientData$url_search, { q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$survey)) { updateSelectInput(session, "survey", selected = q$survey); updateSelectInput(session, "qc_survey", selected = q$survey) }
    if (!is.null(q$profile)) updateSelectInput(session, "profile", selected = q$profile)
    if (!is.null(q$page)) nav_select("nav", q$page)
    if (!is.null(q$tab)) nav_select("view_tab", if (identical(q$tab, "About")) "Details" else q$tab)   # "About" is now "Details"
    if (!is.null(q$ref)) nav_select("ref_tab", q$ref) }, once = TRUE)
