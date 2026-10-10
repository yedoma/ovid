# ERT/pages/mod_home.R  --  The Home page as a Shiny module: where data come in (the folders and Scan) and the counts of what
# the last scan found; the surveys themselves are listed on Archive. ERT opens here. Its inputs are its own (namespaced); the
# folders and Scan are the app's own inputs, passed in as `folders`, and what it does to the rest of the app is passed in as functions.

# folders: the folder fields and the Scan button (app inputs), or NULL in the Workbench, where Import data holds them.
mod_home_ui <- function(id, folders = NULL) {
  ns <- NS(id)
  layout_columns(col_widths = c(4, 8), fill = FALSE,
    card(card_header("Bring data in"),
      if (is.null(folders)) p(class = "muted", "The folders and Scan are under Import data.") else folders,
      hr(), h6("Coordinates and other metadata (optional)"),
      actionButton(ns("go_coords"), "Import coordinates (CSV or Excel)…", class = "btn-sm btn-outline-primary"),
      p(class = "muted mt-2", "Coordinates go into config/transects.csv through that table's Import: the columns are matched and errors and warnings show before anything is written.")),
    div(
      uiOutput(ns("empty")),
      layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box("Surveys", textOutput(ns("vb_n")), p(class = "muted", textOutput(ns("vb_n_sub"), inline = TRUE)), theme = VB()),
        value_box("Placed on the map", textOutput(ns("vb_placed")), p(class = "muted", textOutput(ns("vb_placed_sub"), inline = TRUE)), theme = VB()),
        value_box("Median kept %", textOutput(ns("vb_kept")), p(class = "muted", textOutput(ns("vb_prof"), inline = TRUE)), theme = VB()),
        value_box("Median contact", textOutput(ns("vb_contact")), p(class = "muted", "median of the surveys' medians"), theme = VB()))))
}

# cat_s, stats, transects: the catalogue, survey_stats.csv and transects.csv (reactives). open_import() acts on the rest of the app.
mod_home_server <- function(id, cat_s, stats, transects, open_import) {
  moduleServer(id, function(input, output, session) {
    figs <- reactive(survey_figures(cat_s(), stats(), transects()))
    output$vb_n <- renderText(num(nrow(figs())))
    output$vb_n_sub <- renderText({ f <- figs(); sprintf("in %d project%s", uniqueN(f$project), if (uniqueN(f$project) == 1) "" else "s") })
    output$vb_placed <- renderText(num(sum(figs()$placed)))
    output$vb_placed_sub <- renderText({ n <- sum(!figs()$placed); if (n) sprintf("%s with no coordinates", num(n)) else "all placed" })
    output$vb_kept <- renderText({ k <- median(figs()$kept_pct, na.rm = TRUE); if (is.finite(k)) sprintf("%.0f %%", k) else "–" })
    output$vb_prof <- renderText({ p <- unique(figs()$qc_profile[nzchar(figs()$qc_profile)]); if (length(p)) paste("QC profile:", paste(gsub("_", " ", p), collapse = ", ")) else "no QC yet" })
    output$vb_contact <- renderText({ k <- median(figs()$contact_ohm, na.rm = TRUE); if (is.finite(k)) sprintf("%.1f kohm", k / 1000) else "–" })
    output$empty <- renderUI(if (!nrow(figs())) div(class = "alert alert-info", "Nothing scanned yet. Set the raw ERT archive folder, then press Scan the archive."))
    observeEvent(input$go_coords, open_import())
  })
}
