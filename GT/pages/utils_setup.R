# GT: set-up table editors, background runs, open sessions and the axis controls. Sourced by app.R.
# =============================================================================
#  Editable set-up table (one module for every config CSV)
# =============================================================================

# Every set-up table the interfaces edit: module id, the table (CONFIG_FILES) and its file. Both interfaces build their
# editors from this list; a saved change to any of them needs a run to apply.
SETUP_TABLES <- data.table(
  id  = c("sensors", "sites", "templates", "tz", "time", "frules", "qc", "campbell", "air", "surface", "depsheet", "period"),
  cfg = c("sensors", "sites", "templates", "timezones", "time", "file_rules", "qc", "campbell", "air", "surface", "deploy_sheet", "period"))
SETUP_TABLES[, `:=`(file = vapply(cfg, function(k) CONFIG_FILES[[k]]$file, ""), label = vapply(cfg, function(k) CONFIG_FILES[[k]]$label, ""))]
# skip: tables another editor keeps (the deployment sheet: meta_sheet_server() in app.R), so no two editors save one file.
setup_editors <- function(cfgp, tick, mark, skip = character()) { i <- which(!SETUP_TABLES$id %in% skip)
  setNames(lapply(i, function(i) editServer(SETUP_TABLES$id[i], cfgp(SETUP_TABLES$cfg[i]), tick, on_change = mark)), SETUP_TABLES$id[i]) }

# The tables the Workbench lists: the set-up tables (the deployment sheet with the logger list's columns, core/metasheet_ui.R).
SETUP_TREE <- SETUP_TABLES
LOGGER_LIST_INTRO <- paste(DEPLOY_SHEET_INTRO, "Instead of (or as well as) a registry workbook, the optional columns say what you know about each logger:",
  "a name, the sensor depths and the model are used when given (rows set by hand on the Sensors tab win). Start from the loggers found,",
  "paste rows from a spreadsheet, or import one, and add columns of your own with Columns.")

# What the last run redid, from its manifest: loggers whose files and set-up had not changed are reused.
run_redone <- function(pd) {
  f <- file.path(path.expand(pd), "output", "audit", "run_manifest.csv"); if (!file.exists(f)) return("")
  m <- tryCatch(fread(f, colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
  v <- function(k) suppressWarnings(as.integer(m[key == k, value][1]))
  n <- v("loggers_checked"); r <- v("loggers_reused"); if (anyNA(c(n, r))) return("")
  wb_redone(n - r, n, "loggers")
}

res_choices <- c("Every reading" = "raw", "Hourly mean" = "hour", "Daily mean" = "day", "Weekly mean" = "week", "Monthly mean" = "month")
axis_ui <- function(id, mode = "auto") tagList(
  radioButtons(paste0(id, "_mode"), "Temperature axis", setNames(c("auto", "fixed", "sym", "pct"), c("Auto", "Fixed", "\u00b1 around 0", "Clip to percentiles")), mode, inline = TRUE),
  conditionalPanel(sprintf("input.%s_mode == 'fixed'", id), fluidRow(column(6, numericInput(paste0(id, "_lo"), "From (\u00b0C)", -10, step = 1, width = "100%")),
                                                                    column(6, numericInput(paste0(id, "_hi"), "To (\u00b0C)", 20, step = 1, width = "100%")))),
  conditionalPanel(sprintf("input.%s_mode == 'pct'", id), sliderInput(paste0(id, "_pct"), "Clip below and above this percentile", 0, 10, 1, step = 0.5, width = "100%")))
