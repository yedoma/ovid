# GS/pages/survey.R  --  Survey.
# Part of the GS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, proj, E, mark, cat_s, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- survey
  s1 <- reactive({ req(input$survey); as.list(cat_s()[survey_id == input$survey]) })
  m_shown <- reactiveVal(NULL)                                   # the survey's row as the form last showed it
  observeEvent(list(input$survey, s1()), { s <- s1(); if (identical(s, m_shown())) return(); m_shown(s); l <- survey_label(s)   # a change saved on any page shows here
    updateTextInput(session, "m_name", value = l$name)
    updateDateInput(session, "m_date", value = if (l$file_date == "undated") NA else as.Date(l$file_date))
    updateSelectInput(session, "m_quality", selected = s$quality)
    updateSelectizeInput(session, "m_reasons", selected = if (nzchar(s$quality_reasons)) strsplit(s$quality_reasons, "; ")[[1]] else character())
    updateTextInput(session, "m_instrument", value = s$model %|na|% "");
    updateTextInput(session, "m_project", value = s$project); updateTextInput(session, "m_site", value = s$site)
    updateTextInput(session, "m_operator", value = s$operator)
    updateTextInput(session, "m_conditions", value = s$conditions); updateTextInput(session, "m_ground", value = s$ground_conditions)
    updateTextAreaInput(session, "m_qnotes", value = s$quality_notes); updateTextAreaInput(session, "m_notes", value = s$notes)
    updateTextInput(session, "m_contact", value = s$electrode_contact %||% "")
    updateSelectInput(session, "qc_survey", selected = input$survey) })
  output$m_transect <- renderUI({ tr <- s1()$transect_id %||% ""                 # read only: set on the Map or in Archive
    div(class = "mb-3", tags$label(class = "form-label", "Transect"), div(if (nzchar(tr)) tr else span(class = "muted", "none")),
        p(class = "muted small mb-0", "Set on the Map (Transect details) or in Archive (Add to transect).")) })
  no_meas_msg <- "This catalogue was exported without measurements: the preview and the details are shown instead."
  vdat <- reactive({ ELEV(); s <- s1(); validate(need(has_meas(s$survey_id), no_meas_msg)); view_data(read_meas(s$survey_id), survey_flags(s)) })
  output$pseudo_area <- renderUI(if (has_meas(s1()$survey_id)) plotly::plotlyOutput("survey_plot", height = "580px", fill = FALSE) else
    tagList(p(class = "muted", no_meas_msg), imageOutput("survey_preview", height = "560px")))
  output$survey_preview <- renderImage({ f <- file.path(CAT, "previews", s1()$preview %||% "")
    validate(need(length(f) == 1 && file_test("-f", f), "No preview for this survey")); list(src = f, height = "560px", alt = "pseudosection preview") }, deleteFile = FALSE)
  output$survey_plot <- plotly::renderPlotly(view_pseudosection(vdat(), input$v_colour, input$v_show, PSEUDO_FLAG, input$v_pal, !identical(input$v_scale, "linear"), PSEUDO_SIZE, c(input$v_min, input$v_max)))
  output$survey_hist <- plotly::renderPlotly(view_histogram(vdat(), input$v_colour, !identical(input$v_scale, "linear"), c(input$v_min, input$v_max)))
  output$survey_contact <- plotly::renderPlotly(view_contact(read_part(input$survey, "contact.csv")))
  output$survey_errcur <- plotly::renderPlotly(view_error_current(vdat()))
  output$survey_geom <- plotly::renderPlotly({ ELEV(); validate(need(has_meas(input$survey), no_meas_msg)); view_geometry(electrodes_of(read_meas(input$survey))) })
  save_elev <- function(e) { write_cfg(e, CFG$project_dir, "elevations", who = "app"); ELEV(e) }
  observeEvent(input$elev_file, { if (!wb_made(E$elevations)) return()                                 # an elevation table or a GNSS file (R/inversion.R)
    r <- tryCatch(elevation_file_rows(input$elev_file$datapath, input$survey, input$elev_file$name, author = "app"), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(r)) return()
    e <- ELEV(); save_elev(rbind(e[survey_id != input$survey], r$rows))
    showNotification(sprintf(if (r$kind == "gnss") "Added %d electrode heights from %s" else "Added %d elevations from %s", nrow(r$rows), input$elev_file$name), type = "message") })
  output$survey_errmodel <- renderText({ req(has_meas(input$survey)); e <- error_model(read_meas(input$survey))
    if (e$model == "fixed") e$source else sprintf("%s, e = %s, r\u00b2 %.2f; %s.", e$model, if (e$model == "power") sprintf("%.3g |R|^%.3g", e$a, e$b) else sprintf("%.3g + %.3g |R|", e$a, e$b), e$r2, e$source) })
  observeEvent(input$elev_clear, { if (!wb_made(E$elevations)) return(); e <- ELEV(); if (input$survey %in% e$survey_id) { save_elev(e[survey_id != input$survey]); showNotification("Added elevations removed", type = "message") } })
  output$survey_table <- renderDT({ p <- vdat()
    ovid_dt(p[, .(`#` = meas_id, A = a, B = b, M = m, N = n, `App. resistivity (ohm-m)` = signif(rhoa_ohmm, 4), `V/I (ohm)` = signif(resistance_ohm, 4),
                    `Current (mA)` = signif(current_ma, 4), `Voltage (mV)` = signif(voltage_mv, 4), `Error (%)` = error_pct, `Contact (ohm)` = signif(contact_ohm, 4),
                    Array = nice_array(array), Time = if (inherits(datetime, "POSIXct")) format(datetime, "%H:%M:%S", tz = "UTC") else substr(as.character(datetime), 12, 19), Flags = flag)], page = 50, opts = list(scrollY = "520px", dom = "tip")) })
  output$survey_about <- renderUI(facts_ui(survey_facts(s1())))
  output$survey_head <- renderUI({ s <- s1(); l <- survey_label(s)
    tg <- c(s$model_lbl, s$array_lbl, if (!is.na(s$n_electrodes)) sprintf("%d electrodes at %g m", s$n_electrodes, s$spacing_m),
            if (!is.na(s$length_m)) sprintf("%g m line", s$length_m), s$kind_lbl, if (nzchar(s$transect_id)) paste("Transect", s$transect_id))
    layout_columns(fill = FALSE, col_widths = c(6, 2, 2, 2),
      div(h3(class = "mb-1", l$name), p(class = "muted mb-1", l$date, " \u00b7 ", dirname(sub("!/.*", "", s$primary_file))),
          lapply(tg[!is.na(tg) & nzchar(tg)], function(x) span(class = "tag", x))),
      value_box("Measurements", num(s$n_read), theme = VB()),
      value_box("Kept %", if (is.na(s$pct_retained)) "-" else paste0(round(s$pct_retained), "%"), theme = VB()),
      value_box("Quality", s$quality, theme = VB())) })
  output$survey_files <- renderUI({ s <- s1()
    f <- c(s$primary_file, if (nzchar(s$other_paths)) strsplit(s$other_paths, "; ")[[1]], if (nzchar(s$companions)) paste("companion:", strsplit(s$companions, "; ")[[1]]))
    tagList(lapply(f, function(x) div(class = "muted", style = "word-break:break-all", x)),
            if (nzchar(s$qc_flags)) div(class = "mt-2", strong("QC flags: "), span(class = "muted", s$qc_flags)),
            if (nzchar(s$message)) div(class = "mt-2", style = paste0("color:", PAL$gold), s$message)) })
  observeEvent(input$m_save, {
    sid <- input$survey; s <- s1(); file_name <- sub("\\.(stg|csv|txt|dat)$", "", basename(s$primary_file), ignore.case = TRUE)
    first <- if (is.na(s$datetime_start)) "" else substr(s$datetime_start, 1, 10); d <- if (length(input$m_date) && !is.na(input$m_date)) format(input$m_date) else ""
    vals <- list(display_name = if (trim(input$m_name) == file_name) "" else trim(input$m_name),       # only what differs from the files is stored
                 acquired_date = if (d == first) "" else d, quality = input$m_quality, quality_reasons = paste(input$m_reasons, collapse = "; "),
                 quality_notes = trim(input$m_qnotes), project = trim(input$m_project), site = trim(input$m_site),
                 operator = trim(input$m_operator), instrument_model = { m <- trim(input$m_instrument %||% ""); if (m == (s$file_model %|na|% "")) "" else m }, conditions = trim(input$m_conditions), ground_conditions = trim(input$m_ground), notes = trim(input$m_notes),
                 electrode_contact = trim(input$m_contact %||% ""))   # the transect and the inversion fields are kept as they are
    cf <- load_configs(CFG)$surveys
    if (!sid %in% cf$survey_id) cf <- rbind(cf, ensure_cols(data.table(survey_id = sid), SURVEY_COLS)[, ..SURVEY_COLS])
    for (k in names(vals)) set(cf, which(cf$survey_id == sid), k, vals[[k]])
    write_cfg(cf, CFG$project_dir, "surveys", who = "app")
    x <- copy(cat_s()); for (k in names(vals)) set(x, which(x$survey_id == sid), k, vals[[k]])
    x[survey_id == sid, `:=`(name = fifelse(nzchar(display_name), display_name, file_name), date = fifelse(nzchar(acquired_date), acquired_date, fifelse(first == "", "undated", first)))]
    cat_s(x); showNotification("Saved to config/surveys.csv", type = "message") })
  # what a report of the survey should state (Herring et al. 2023, Table 1; CPERS metadata), and whether the catalogue has it
  output$survey_checklist <- renderUI({ s <- s1(); h <- function(x) length(x) && !is.na(x[1]) && nzchar(as.character(x[1]))
    items <- list(c("Instrument", h(s$model)), c("Array", h(s$array_lbl)), c("Electrodes and spacing", h(s$n_electrodes) && h(s$spacing_m)), c("Date", h(s$date) && s$date != "undated"),
                  c("Location (transect or site)", h(s$transect_id) || h(s$site)), c("Filtering (QC profile and flags)", h(s$qc_profile)),
                  c("Electrode contact", h(s$electrode_contact)))   # the inversion is reported where it is run: GS does not invert
    tagList(p(class = "muted mt-2 mb-1", "Reporting checklist (Herring et al. 2023, Table 1):"),
      lapply(items, function(x) div(class = "muted", paste(if (identical(x[2], "TRUE")) "\u2713" else "\u2013", x[1])))) })
  survey_flags <- function(s) { f <- file.path(PROJECT, "output", "surveys", s$survey_id, "flags.csv")
    x <- if (file.exists(f)) fread(file = f, colClasses = "character", na.strings = NULL) else data.table()   # a survey with no flags has a header-only file
    if (!nrow(x)) return(data.table(meas_id = integer(), flag = character(), rule = character(), value = character()))
    x[profile == cat_profile(s)][, meas_id := as.integer(meas_id)][] }
  fname <- function(s, what) paste0(survey_label(s)$file_date, "_", gsub("[^A-Za-z0-9._-]+", "_", survey_label(s)$name), "_", what, ".zip")
  # Export survey (ZIP) with the QC profile picked beside it: the catalogue's (the flags of the last scan, as before), another
  # profile of qc_profiles.csv, or the QC page's limits as they are now (not saved); the others are checked with the set-up as saved
  dl_sid <- reactiveVal("")                                       # the survey the choice was made for: another one starts from its catalogue profile
  observe({ s <- s1(); ch <- unique(c(cat_profile(s), qc()$profile)); ch <- ch[nzchar(ch) & ch != "live"]; cur <- isolate(input$dl_profile) %||% ""
    same <- identical(isolate(dl_sid()), s$survey_id); dl_sid(s$survey_id)
    updateSelectInput(session, "dl_profile", choices = c(stats::setNames(ch, gsub("_", " ", ch)), `QC page's limits (not saved)` = "live"),
                      selected = if (same && cur %in% c(ch, "live")) cur else cat_profile(s)) })
  dl_label <- function(s) { p <- input$dl_profile %||% ""
    if (identical(p, "live")) paste0(if (nzchar(input$profile %||% "")) input$profile else "no_profile", "_adjusted") else if (nzchar(p)) p else cat_profile(s) }
  output$dl_survey <- downloadHandler(filename = function() fname(s1(), dl_label(s1())), content = function(file) {   # named by the profile it uses
    s <- s1(); p <- input$dl_profile %||% ""; d <- read_meas(s$survey_id); cf <- load_configs(CFG)
    if (identical(p, "live")) return(export_survey(s, d, qc_survey(d, s$survey_id, s$type, live(), "live", cf$manual, s, cf$electrodes), live(),
                                                   paste0(input$profile, " (adjusted)"), file, manual = cf$manual))
    prof <- if (nzchar(p)) p else cat_profile(s)
    fl <- if (identical(prof, cat_profile(s))) survey_flags(s) else qc_survey(d, s$survey_id, s$type, qc(), prof, cf$manual, s, cf$electrodes)
    export_survey(s, d, fl, profile_rows(qc(), prof, s$type), prof, file, manual = cf$manual) })
