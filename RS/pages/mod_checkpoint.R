# RS/pages/mod_checkpoint.R  --  Checkpoint accuracy (ASPRS Edition 2, R/gnss.R), as a Shiny module on the Workbench's
# Positioning tab. The file is read through the core import (pages/fct_checkpoint.R): the matched columns and any errors and
# warnings show first, and nothing is worked out while an error stands.

mod_checkpoint_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Checkpoint accuracy (ASPRS 2023)"),
    p(class = "muted", "A product (orthomosaic, DSM, point cloud) against surveyed checkpoints: a CSV or Excel file with each point's surveyed and product easting, northing and elevation, in metres; columns are matched by their headings. RMSE_H, RMSE_V and RMSE_3D as Edition 2 asks (no 95 % statistic), with the checkpoints' own survey error added; at least 30 checkpoints."),
    layout_columns(col_widths = c(6, 3, 3), fileInput(ns("file"), NULL, accept = c(".csv", ".txt", ".xlsx", ".xls")),
      numericInput(ns("sh"), "Checkpoint survey RMSE, horizontal (cm; default from Set-up › Calibration options)", 2, min = 0, step = 0.5),
      numericInput(ns("sv"), "Checkpoint survey RMSE, vertical (cm; default from Set-up › Calibration options)", 3, min = 0, step = 0.5)),
    uiOutput(ns("msgs")), DTOutput(ns("summary")), DTOutput(ns("points")), ovid_export_button(ns("dl"), "csv", "checkpoint accuracy"))
}

# cal: the project's calibration table (a reactive), for the survey RMSE defaults.
mod_checkpoint_server <- function(id, cal) {
  moduleServer(id, function(input, output, session) {
    observe({ k <- cal(); updateNumericInput(session, "sh", value = cal_num(k, "checkpoint_survey_rmse_h_cm")); updateNumericInput(session, "sv", value = cal_num(k, "checkpoint_survey_rmse_v_cm")) })
    # safe: a file that cannot be read gives nothing to import
    rd <- reactive({ f <- input$file; req(f); x <- tryCatch(meta_import_read(f$datapath, NULL, f$name), error = function(e) NULL)
      validate(need(!is.null(x) && nrow(x) > 0, "Could not read that file.")); checkpoint_read(x) })
    res <- reactive({ r <- rd(); req(r$ok); checkpoint_accuracy(r$rows, (input$sh %||% 0) / 100, (input$sv %||% 0) / 100) })
    output$msgs <- renderUI({ r <- rd(); m <- r$map[nzchar(r$map)]; spec <- meta_defaults(CHECKPOINT_FIELDS)
      cls <- c(error = "text-danger", warning = "text-warning", note = "muted")
      tagList(p(class = "muted", "Columns used: ", paste(sprintf("%s → %s", names(m), spec$label[match(m, spec$field)]), collapse = "; ")),
        lapply(seq_len(nrow(r$issues)), function(i) p(class = cls[[r$issues$severity[i]]], r$issues$message[i])),
        if (r$ok) tagList(p(strong(res()$statement)), lapply(res()$notes, function(x) p(class = "text-warning", x)))) })
    output$summary <- renderDT(ovid_dt(res()$summary, selection = "none", opts = list(dom = "t")))
    output$points <- renderDT(ovid_dt(res()$points, selection = "none", page = 10))
    output$dl <- ovid_download("RS", "checkpoint accuracy", "csv", function(file) { k <- res(); fwrite(cbind(k$points, k$summary[rep(1, nrow(k$points))]), file) })
  })
}
