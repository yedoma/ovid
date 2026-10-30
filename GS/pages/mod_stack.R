# GS/pages/mod_stack.R  --  The Stack page as a Shiny module: a project's surveys side by side on one colour scale, sorted by
# date, with their figures, and Compare with..., which shows a change image only when two surveys can be compared. A survey's
# transect is shown here, and set on the Map or in Archive. Raw values throughout. What it needs from the rest of the app is passed in.

STACK_MAX <- 24      # panels drawn at once; more are listed in the table and the page says so

# The one Compare control, the same on Stack and the Workbench's Panels: Compare with... (off) and Both kept at least (%).
stack_compare_ui <- function(id, kept_id) tagList(
  selectInput(id, NULL, c(`Compare with… (off)` = ""), width = "300px"),
  numericInput(kept_id, "Both kept at least (%)", STACK_MIN_KEPT, min = 0, max = 100, step = 5, width = "170px"))

mod_stack_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ovid_controls(tagAppendAttributes(selectInput(ns("project"), "Project", choices = NULL, width = "100%"), class = "d-wide"),
      numericInput(ns("lim_lo"), "Colour scale from (ohm-m)", RHOA_RANGE[1], min = 0.01, width = "170px"),   # the shared default (R/plots.R)
      numericInput(ns("lim_hi"), "to (ohm-m)", RHOA_RANGE[2], min = 0.02, width = "130px"),
      stack_compare_ui(ns("cmp"), ns("cmp_kept"))),
    uiOutput(ns("head")),
    conditionalPanel("input.cmp != ''", ns = ns,
      card(card_header("Compare", span(class = "muted float-end", "from the survey selected in the table below (else the first)")),
        uiOutput(ns("cmp_why")),
        plotOutput(ns("cmp_plot"), height = "360px"))),
    card(card_header("Surveys of the project", span(class = "muted float-end", textOutput(ns("shown"), inline = TRUE))),
      uiOutput(ns("stack_area"))),
    card(card_header("Surveys", span(class = "muted float-end", "click rows to select them; double-click opens one in Survey")),
      DTOutput(ns("tbl")),
      div(class = "d-flex gap-2 align-items-end flex-wrap mt-2",
        actionButton(ns("open"), "Open in Survey", class = "btn-sm btn-primary mb-3")),
      p(class = "muted", "A transect groups surveys of one line; it is set on the Map (Transect details) or in Archive (Add to transect).")))
}

# cat_s, stats, transects: reactives. pick: a reactiveVal with the project to show. read_meas(sid): a survey's measurements.
# open_survey(sid), open_import(): act on the rest of the app.
mod_stack_server <- function(id, cat_s, stats, transects, pick, read_meas, has_meas, open_survey, open_import) {
  moduleServer(id, function(input, output, session) {
    figs_all <- reactive(survey_figures(cat_s(), stats(), transects()))
    cmp_kept <- reactive(stack_min_kept(input$cmp_kept))   # also the Workbench's Compare with (returned below)
    observe({ p <- sort(unique(figs_all()$project)); sel <- pick() %||% isolate(input$project)
      updateSelectInput(session, "project", choices = p, selected = if (length(sel) && sel %in% p) sel else p[1]) })
    observeEvent(input$project, if (!identical(input$project, pick())) pick(input$project), ignoreInit = TRUE)   # the choice stays when the catalogue changes
    figs <- reactive({ req(input$project); figs_all()[project == input$project][order(date, name)] })
    ref <- reactive({ f <- figs(); r <- input$tbl_rows_selected; if (length(r) && r[1] <= nrow(f)) f$survey_id[r[1]] else f$survey_id[1] })   # Compare from
    observe({ ch <- stack_compare_choices(figs(), ref()); cur <- isolate(input$cmp) %||% ""
      updateSelectInput(session, "cmp", choices = ch, selected = if (cur %in% ch) cur else "") })
    output$head <- renderUI({ f <- figs(); lay <- unique(f$layout[nzchar(f$layout)])
      tagList(
        p(class = "mb-1", strong(input$project), sprintf("  ·  %d survey%s  ·  %s", nrow(f), if (nrow(f) == 1) "" else "s",
          if (nrow(f)) paste(range(f$date), collapse = " to ") else ""), if (length(lay)) span(class = "muted", "  ·  ", paste(lay, collapse = "; "))),
        if (!any(f$placed)) div(class = "alert alert-secondary d-flex align-items-center gap-2 py-2",
          span("Not placed: these surveys have no coordinates, so they are not on the map."),
          actionButton(session$ns("go_coords"), "Import coordinates (CSV or Excel)…", class = "btn-sm btn-outline-primary ms-auto"))) })
    observeEvent(input$go_coords, open_import())
    lim <- reactive({ a <- input$lim_lo; b <- input$lim_hi
      if (!is.numeric(a) || !is.numeric(b) || is.na(a) || is.na(b) || a <= 0 || b <= a) RHOA_RANGE else c(a, b) })
    shown <- reactive(head(figs()[vapply(survey_id, has_meas, TRUE)], STACK_MAX))
    output$shown <- renderText({ n <- nrow(figs()); k <- nrow(shown())
      if (k < n) sprintf("showing %d of %d (surveys exported without measurements, or more than %d, are in the table only)", k, n, STACK_MAX) else sprintf("%d shown", k) })
    output$stack_area <- renderUI({ k <- nrow(shown())
      if (!k) return(p(class = "muted", "No measurements to draw for this project."))
      plotOutput(session$ns("stack_plot"), height = paste0(140 + 230 * ceiling(k / 3), "px")) })
    output$stack_plot <- renderPlot({ f <- shown(); req(nrow(f) > 0)
      d <- rbindlist(lapply(f$survey_id, function(s) { x <- read_meas(s); cbind(survey_id = s, x[, .(rhoa_ohmm)], pseudo_xy(x)) }))
      plot_stack(d, f, lim()) }, res = 96)
    cmp <- reactive({ req(nzchar(input$cmp %||% ""), ref()); f <- figs_all()
      r1 <- f[survey_id == ref()]; r2 <- f[survey_id == input$cmp]; req(nrow(r1) == 1, nrow(r2) == 1)
      if (r1$survey_id == r2$survey_id) return(list(ok = FALSE, why = "pick two different surveys"))
      k <- stack_comparable(r1, r2, cmp_kept())
      if (k$ok && !(has_meas(r1$survey_id) && has_meas(r2$survey_id))) k <- list(ok = FALSE, why = "a survey was exported without measurements")
      c(k, list(r1 = r1, r2 = r2)) })
    output$cmp_why <- renderUI({ k <- cmp()
      if (k$ok) p(class = "muted", sprintf("Change from %s %s to %s %s. Comparable: the same electrode layout and both above the kept limit.", k$r1$date, k$r1$name, k$r2$date, k$r2$name))
      else div(class = "alert alert-warning py-2", strong("No change image: "), paste(k$why, collapse = "; "), ".",
               p(class = "mb-0 mt-1 small", "A change image of badly contacted surveys shows the contacts, not the ground.")) })
    output$cmp_plot <- renderPlot({ k <- cmp(); req(k$ok)
      ch <- stack_change(read_meas(k$r1$survey_id), read_meas(k$r2$survey_id))
      validate(need(ch$n_matched > 0, "The two surveys have no quadrupole in common."))
      plot_change(ch, paste(k$r1$name, k$r1$date), paste(k$r2$name, k$r2$date)) }, res = 96)
    fmt <- function(x, d) ifelse(is.na(x), "", format(round(x, d), nsmall = d, big.mark = ","))
    dbl <- sprintf("table.on('dblclick', 'tbody tr', function() { Shiny.setInputValue('%s', table.row(this).index() + 1, {priority: 'event'}); });", session$ns("tbl_dbl"))
    output$tbl <- renderDT({ f <- figs()
      ovid_long_table(f[, .(Date = date, Survey = name, Measurements = n_meas, `Kept %` = fmt(kept_pct, 1), `Contact (median, kohm)` = fmt(contact_ohm / 1000, 1),
                      `Repeat error (median, %)` = fmt(error_pct, 2), `QC profile` = gsub("_", " ", qc_profile), Transect = transect_id, Layout = layout)],
                selection = "multiple", callback = JS(dbl), height = "45vh") })   # core longtable.R
    observeEvent(input$tbl_dbl, open_survey(figs()$survey_id[input$tbl_dbl]))
    observeEvent(input$open, { r <- input$tbl_rows_selected
      if (length(r)) open_survey(figs()$survey_id[r[1]]) else showNotification("Select a survey first", type = "warning") })
    list(cmp_kept = cmp_kept)
  })
}
