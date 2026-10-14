# GS/pages/mod_archive.R  --  The Archive page as a Shiny module: every survey, filtered, with bulk project/transect edits.
# Its inputs are its own (namespaced); what it needs from the rest of the app is passed in: the catalogue (cat_s),
# the transects table (transects) and how to open a survey on the Survey page (open_survey).

# sidebar_extra: what the page's filter sidebar shows under the filters (the catalogue import and export, kept by app.R).
mod_archive_ui <- function(id, sidebar_extra = NULL) {
  ns <- NS(id)
  tagList(
    layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box("Surveys", textOutput(ns("vb_n")), p(class = "muted", textOutput(ns("vb_n_sub"), inline = TRUE)), theme = VB()),
      value_box("Years", textOutput(ns("vb_years")), p(class = "muted", textOutput(ns("vb_years_sub"), inline = TRUE)), theme = VB()),
      value_box("Measurements", textOutput(ns("vb_meas")), p(class = "muted", "in original surveys"), theme = VB()),
      value_box("Median kept %", textOutput(ns("vb_pass")), p(class = "muted", textOutput(ns("vb_pass_sub"), inline = TRUE)), theme = VB())),
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(title = "Filter", width = 260,
        textInput(ns("f_text"), NULL, placeholder = "Search name, folder, notes"),
        selectInput(ns("f_year"), "Year", choices = NULL, multiple = TRUE),
        selectInput(ns("f_project"), "Project", choices = NULL, multiple = TRUE),
        selectInput(ns("f_transect"), "Transect", choices = NULL, multiple = TRUE),
        selectInput(ns("f_folder"), "Folder", choices = NULL, multiple = TRUE),
        checkboxGroupInput(ns("f_instr"), "Instrument", choices = NULL),
        checkboxGroupInput(ns("f_quality"), "Quality", choices = names(QUALITY_COL)),
        checkboxInput(ns("f_derived"), "Show copies and processed sets", FALSE),
        sidebar_extra),
      card(card_header("Surveys", span(class = "muted float-end", "click rows to select them; double-click a row to open it")),
        div(class = "viewbar",
          span(class = "muted", style = "padding-bottom:8px", textOutput(ns("sel_n"), inline = TRUE)),
          selectizeInput(ns("bulk_project"), "Project", choices = NULL,
                         options = list(create = TRUE, placeholder = "Pick or type a new project"), width = "240px"),
          actionButton(ns("bulk_project_go"), "Add to project", class = "btn-sm btn-primary mb-1"),
          selectizeInput(ns("bulk_transect"), "Transect", choices = NULL,
                         options = list(create = TRUE, placeholder = "Pick or type a new transect"), width = "240px"),
          actionButton(ns("bulk_transect_go"), "Add to transect", class = "btn-sm btn-primary mb-1"),
          actionButton(ns("bulk_open"), "Open", class = "btn-sm btn-outline-primary mb-1"),
          actionLink(ns("bulk_clear"), "Clear selection", class = "mb-2")),
        DTOutput(ns("archive")))))
}

# cat_s, transects: reactiveVals (read, and written when surveys are added to a project or transect).
# open_survey(sid): shows that survey on the Survey page.
mod_archive_server <- function(id, cat_s, transects, open_survey) {
  moduleServer(id, function(input, output, session) {
    # the filter and bulk choices follow the catalogue and the project and transect tables
    observe({ s <- cat_s()
      updateSelectInput(session, "f_year", choices = sort(unique(na.omit(s$year)), decreasing = TRUE), selected = isolate(input$f_year))
      updateSelectInput(session, "f_folder", choices = sort(unique(s$folder)))
      pj <- sort(unique(c(s$project[nzchar(s$project)], isolate(load_configs(CFG)$projects$project))))
      tr <- sort(unique(c(s$transect_id[nzchar(s$transect_id)], isolate(transects())$transect_id)))
      updateSelectInput(session, "f_project", choices = c(pj, `No project` = "(none)"), selected = isolate(input$f_project))
      updateSelectInput(session, "f_transect", choices = c(tr, `No transect` = "(none)"), selected = isolate(input$f_transect))
      updateSelectizeInput(session, "bulk_project", choices = pj, selected = isolate(input$bulk_project))
      updateSelectizeInput(session, "bulk_transect", choices = tr, selected = isolate(input$bulk_transect))
      updateCheckboxGroupInput(session, "f_instr", choices = sort(unique(s$instrument)))
    })

    orig <- reactive(cat_s()[kind %in% c("original", "merged")])
    output$vb_n <- renderText(num(nrow(orig())))
    output$vb_n_sub <- renderText(sprintf("%s entries in the catalogue", num(nrow(cat_s()))))
    output$vb_years <- renderText({ y <- range(orig()$year, na.rm = TRUE); if (all(is.finite(y))) paste(y, collapse = " - ") else "-" })
    output$vb_years_sub <- renderText(sprintf("%d undated", sum(is.na(orig()$year))))
    output$vb_meas <- renderText(num(sum(orig()$n_read, na.rm = TRUE)))
    output$vb_pass <- renderText(sprintf("%.0f%%", median(orig()$pct_retained, na.rm = TRUE)))
    output$vb_pass_sub <- renderText({ p <- unique(stack_blank(orig()$qc_profile)); p <- p[nzchar(p)]   # the profile the catalogue was scanned with
      if (length(p)) paste("profile:", paste(gsub("_", " ", p), collapse = ", ")) else "no QC yet" })

    filtered <- reactive(archive_filter(cat_s(), list(f_derived = input$f_derived, f_year = input$f_year, f_folder = input$f_folder,
      f_project = input$f_project, f_transect = input$f_transect, f_instr = input$f_instr, f_quality = input$f_quality, f_text = input$f_text)))
    dbl <- sprintf("table.on('dblclick', 'tbody tr', function() { Shiny.setInputValue('%s', table.row(this).index() + 1, {priority: 'event'}); });",
                   session$ns("archive_dbl"))
    output$archive <- renderDT({ s <- filtered()
      ovid_long_table(s[, .(Date = date, Name = name, Project = project, Transect = transect_id, Folder = folder, Instrument = instrument,
                      Array = array_lbl, Measurements = n_read, `Kept %` = ifelse(is.na(pct_retained), "", paste0(round(pct_retained), "%")),
                      Quality = quality, Type = kind_lbl)],
                selection = "multiple", callback = JS(dbl), dom = "ti") |>                    # core longtable.R
        formatStyle("Quality", fontWeight = "600") })
    observeEvent(input$archive_dbl, open_survey(filtered()$survey_id[input$archive_dbl]))
    observeEvent(input$bulk_open, { r <- input$archive_rows_selected
      if (length(r)) open_survey(filtered()$survey_id[r[1]]) else showNotification("Select a survey first", type = "warning") })
    observeEvent(input$bulk_clear, DT::selectRows(DT::dataTableProxy("archive"), NULL))
    output$sel_n <- renderText({ n <- length(input$archive_rows_selected); if (n) sprintf("%d selected", n) else "Select surveys:" })

    # assign the selected surveys to a project or a transect (config/surveys.csv; new projects and transects are added to their tables)
    assign_selected <- function(field, value) {
      r <- input$archive_rows_selected; if (!length(r)) return(showNotification("Select one or more surveys first", type = "warning"))
      value <- trim(value %||% ""); if (!nzchar(value)) return(showNotification("Pick or type a name first", type = "warning"))
      sids <- filtered()$survey_id[r]; cf <- load_configs(CFG)$surveys
      for (sid in setdiff(sids, cf$survey_id)) cf <- rbind(cf, ensure_cols(data.table(survey_id = sid), SURVEY_COLS)[, ..SURVEY_COLS])
      set(cf, which(cf$survey_id %in% sids), field, value); write_cfg(cf, CFG$project_dir, "surveys", who = "app")
      if (field == "project") { pj <- load_configs(CFG)$projects
        if (!value %in% pj$project) write_cfg(rbind(pj, ensure_cols(data.table(project = value), PROJECT_COLS)[, ..PROJECT_COLS]), CFG$project_dir, "projects", who = "app") }
      if (field == "transect_id") { t <- transects()
        if (!value %in% t$transect_id) { t <- rbind(t, ensure_cols(data.table(transect_id = value), TRANSECT_COLS)[, ..TRANSECT_COLS])
          write_cfg(t, CFG$project_dir, "transects", who = "app"); transects(t) } }
      x <- copy(cat_s()); set(x, which(x$survey_id %in% sids), field, value); cat_s(x)
      showNotification(sprintf("%d survey%s added to %s %s", length(sids), if (length(sids) == 1) "" else "s", if (field == "project") "project" else "transect", value), type = "message") }
    observeEvent(input$bulk_project_go, assign_selected("project", input$bulk_project))
    observeEvent(input$bulk_transect_go, assign_selected("transect_id", input$bulk_transect))
  })
}

# The catalogue rows the Archive filters leave (no Shiny: a test can call it). f is the filter inputs by name (f_year, f_folder, ...);
# an empty filter keeps everything.
archive_filter <- function(s, f) {
  if (!isTRUE(f$f_derived)) s <- s[kind %in% c("original", "merged")]
  if (length(f$f_year)) s <- s[as.character(year) %in% f$f_year]
  if (length(f$f_folder)) s <- s[folder %in% f$f_folder]
  if (length(f$f_project)) s <- s[ifelse(nzchar(project), project, "(none)") %in% f$f_project]
  if (length(f$f_transect)) s <- s[ifelse(nzchar(transect_id), transect_id, "(none)") %in% f$f_transect]
  if (length(f$f_instr)) s <- s[instrument %in% f$f_instr]
  if (length(f$f_quality)) s <- s[quality %in% f$f_quality]
  if (nzchar(f$f_text %||% "")) { t <- tolower(f$f_text); s <- s[grepl(t, tolower(paste(name, primary_file, notes, project, site)), fixed = TRUE)] }
  s
}
