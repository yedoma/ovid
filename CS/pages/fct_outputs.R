# CS: reading the project's outputs (audit, detail, records) and building the viewer bundle. Sourced by app.R.
# A self-contained copy of the app plus the current output, ready to publish
# (rsconnect::deployApp) or to copy to a shared machine. It opens in viewer mode.
make_viewer_bundle <- function(project_dir, dest = file.path(project_dir, "viewer_app"), include_detail = FALSE) {
  project_dir <- path.expand(project_dir); out <- file.path(project_dir, "output")
  if (!file.exists(file.path(out, "audit", "audit_index.csv"))) stop("There is no output yet. Run the pipeline first.")
  unlink(dest, recursive = TRUE)
  for (d in c("data/output/stations", "data/output/audit/detail", "data/config")) dir.create(file.path(dest, d), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(APP_DIR, c("app.R", "cs_pipeline.R", "home.R")), dest, overwrite = TRUE)
  dir.create(file.path(dest, "pages"), showWarnings = FALSE)
  file.copy(list.files(file.path(APP_DIR, "pages"), full.names = TRUE), file.path(dest, "pages"), overwrite = TRUE)
  dir.create(file.path(dest, "R"), showWarnings = FALSE); file.copy(list.files(file.path(APP_DIR, "R"), full.names = TRUE), file.path(dest, "R"), overwrite = TRUE)
  if (dir.exists(file.path(APP_DIR, "www"))) { dir.create(file.path(dest, "www"), showWarnings = FALSE); file.copy(list.files(file.path(APP_DIR, "www"), full.names = TRUE), file.path(dest, "www"), overwrite = TRUE) }
  dir.create(file.path(dest, "core"), showWarnings = FALSE); file.copy(list.files(CORE_DIR, "\\.R$", full.names = TRUE), file.path(dest, "core"), overwrite = TRUE)   # the shared Ovid Core files
  file.copy(file.path(CORE_DIR, "www"), file.path(dest, "core"), recursive = TRUE, overwrite = TRUE)   # and their CSS and JavaScript
  if (dir.exists(file.path(APP_DIR, "workbench"))) { dir.create(file.path(dest, "workbench"), showWarnings = FALSE); file.copy(list.files(file.path(APP_DIR, "workbench"), full.names = TRUE), file.path(dest, "workbench"), overwrite = TRUE) }
  st <- list.files(file.path(out, "stations"), full.names = TRUE); pq <- st[grepl("\\.parquet$", st)]
  use <- c(pq, st[grepl("\\.csv$", st) & !tools::file_path_sans_ext(st) %in% tools::file_path_sans_ext(pq)])
  file.copy(use, file.path(dest, "data/output/stations"))
  au <- list.files(file.path(out, "audit"), pattern = "\\.csv$", full.names = TRUE)
  file.copy(au[include_detail | basename(au) != "audit_log.csv"], file.path(dest, "data/output/audit"))
  if (include_detail) file.copy(list.files(file.path(out, "audit", "detail"), full.names = TRUE), file.path(dest, "data/output/audit/detail"))
  pr <- list.files(file.path(out, "precip"), full.names = TRUE)
  if (length(pr)) { dir.create(file.path(dest, "data/output/precip"), showWarnings = FALSE); file.copy(pr, file.path(dest, "data/output/precip")) }
  qq <- list.files(file.path(out, "qc"), full.names = TRUE)
  if (length(qq)) { dir.create(file.path(dest, "data/output/qc"), showWarnings = FALSE); file.copy(qq, file.path(dest, "data/output/qc")) }
  dr <- list.files(file.path(out, "derived"), full.names = TRUE)
  if (length(dr)) { dir.create(file.path(dest, "data/output/derived"), showWarnings = FALSE); file.copy(dr, file.path(dest, "data/output/derived")) }
  for (g in file.path(project_dir, "config", c("groups.csv", "parameter_info.csv", "precip_gauges.csv", "station_info.csv", "field_log.csv"))) if (file.exists(g)) file.copy(g, file.path(dest, "data/config"))
  settings_save(file.path(dest, "app_settings.json"), list(viewer = TRUE, project_dir = "data"))
  writeLines(c("Ovid Core // CS - viewer bundle", paste("Built", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    "This folder holds the app and a snapshot of the cleaned data. It opens in viewer mode:",
    "colleagues can browse Metadata, Explore and Audit but cannot change the set-up or run the pipeline.", "",
    "Publish to Posit Connect or shinyapps.io from R:", "    rsconnect::deployApp(\"<this folder>\")", "",
    "Or run it on any machine with R:", "    shiny::runApp(\"<this folder>\")", "",
    "To update the data: run the pipeline in the full app, build the bundle again, and publish again."),
    file.path(dest, "README.txt"))
  f <- list.files(dest, recursive = TRUE, full.names = TRUE)
  list(dir = dest, files = length(f), mb = round(sum(file.size(f)) / 1e6), parquet = length(pq) > 0, detail = include_detail)
}

load_outputs <- function(pd) {
  a <- file.path(pd, "output", "audit")
  if (!file.exists(file.path(a, "audit_index.csv"))) return(NULL)
  # safe: an output table that cannot be read shows as empty until the next run writes it
  rd <- function(f, ...) if (file.exists(file.path(a, f))) tryCatch(fread(file.path(a, f), ...), error = function(e) data.table()) else data.table()
  chr <- list(character = "logger_id")
  src <- rd("sources.csv", colClasses = list(character = c("logger_id", "header_name")), na.strings = NULL)
  meta <- rd("metadata.csv", colClasses = list(character = c("station", "stream", "logger_id", "serial", "table")))
  if (nrow(meta) && !"conflicting_values" %in% names(meta)) meta[, conflicting_values := 0L]
  list(index = rd("audit_index.csv", colClasses = list(character = c("station", "logger_id", "stream", "column", "first", "last"))),
       summary = rd("audit_summary.csv"), manifest = rd("file_manifest.csv", colClasses = chr),
       registry = rd("logger_registry.csv", colClasses = list(character = c("logger_id", "table", "station", "stream", "serial"))), gaps = rd("gaps.csv", colClasses = chr),
       dict = rd("column_dictionary.csv", colClasses = chr), meta = meta, sources = src,
       problems = rd("problems.csv", colClasses = "character"),
       checks = rd("timestamp_checks.csv", colClasses = list(character = c("logger_id", "table", "from", "to", "station", "stream")), na.strings = ""),
       pstats = rd("parameter_stats.csv", colClasses = list(character = c("station", "stream", "parameter"))),
       health = rd("sensor_health.csv", colClasses = list(character = c("station", "stream", "parameter"))),
       flags = rd("flags.csv", colClasses = list(character = c("station", "stream", "parameter", "start", "end"))),
       dups = rd("duplicate_sources.csv", colClasses = list(character = c("station_a", "stream_a", "station_b", "stream_b"))),
       derived = rd("derived_summary.csv", colClasses = list(character = c("station", "stream"))),
       breaks = rd("breakpoints.csv", colClasses = list(character = c("station", "stream", "break_month"))),
       estimated = { f <- file.path(path.expand(pd), "output", "derived", "daily_estimated.csv"); if (file.exists(f)) fread(f, colClasses = list(character = c("station", "stream"))) else data.table() },
       file_rules = { x <- rd("file_rule_counts.csv"); if (!nrow(x)) { y <- rd("file_audit.csv"); if ("rule" %in% names(y)) x <- y }; x },   # file_audit.csv held these before core 1.4
       file_audit = { x <- rd("file_audit.csv", colClasses = list(character = c("logger_id", "table"))); if ("reconciles" %in% names(x)) x else data.table() },
       proghist = rd("program_history.csv", colClasses = list(character = c("station", "logger_id", "table", "program", "os", "columns_added", "columns_removed"))),
       lineage = rd("lineage.csv", colClasses = list(character = c("station", "logger_id", "model", "serial", "tables", "note"))),
       manifest_run = rd("run_manifest.csv", colClasses = "character"), runs = rd("run_history.csv", colClasses = "character"),
       qc_summary = rd("qc_summary.csv", colClasses = list(character = c("station", "stream", "parameter"))),
       precip = rd("precip_summary.csv", colClasses = list(character = c("station", "stream"))),
       stations = rd("station_summary.csv", colClasses = list(character = c("station", "stream"))),
       when = file.mtime(file.path(a, "audit_index.csv")))
}
read_detail <- function(pd, files) {
  f <- file.path(pd, "output", "audit", "detail", files); f <- f[file.exists(f)]
  if (!length(f)) return(data.table())
  d <- rbindlist(lapply(f, fread, colClasses = "character", na.strings = ""), fill = TRUE)
  d[, n := as.integer(n)]; d
}
# Explore > Summaries, broken down: one statistic of every record of each parameter, by year, calendar month or water
# year (ending 30 September). Missing % counts blanks; the others ignore them.
STAT_WHAT <- c("Mean" = "mean", "Minimum" = "min", "Maximum" = "max", "Median" = "median", "Total" = "sum", "Standard deviation" = "sd", "Records with a value" = "n", "Missing %" = "miss")
stat_of <- function(x, what) { ok <- !is.na(x); if (what == "n") return(as.numeric(sum(ok))); if (what == "miss") return(round(100 * mean(!ok), 1)); if (!any(ok)) return(NA_real_)
  signif(switch(what, mean = mean(x[ok]), min = min(x[ok]), max = max(x[ok]), median = stats::median(x[ok]), sum = sum(x[ok]), sd = stats::sd(x[ok])), 5) }
stat_breakdown <- function(d, p, by, what, units = NULL) {
  t <- d$TIMESTAMP
  lab <- switch(by, year = as.character(year(t)), month = month.abb[month(t)], wy = as.character(year(t) + (month(t) >= 10)))
  lev <- if (by == "month") month.abb[month.abb %in% lab] else sort(unique(lab))
  u <- if (is.null(units)) rep(NA_character_, length(p)) else unname(units[p])
  out <- data.table(Parameter = p, Unit = ifelse(is.na(u), "", u))
  for (l in lev) { i <- which(lab == l); set(out, j = l, value = vapply(p, function(c) stat_of(d[[c]][i], what), 0)) }
  out
}
# the one format of a single-station report, from the Report formats ticked on Export: PDF when both or neither are
one_report_format <- function(formats) if (identical(as.character(formats), "html")) "html" else "pdf"
# Plot and explore shades a battery below this many volts (a usual low-voltage warning for 12 V loggers)
LOW_BATT_V <- 11.5
# means over a coarser time step; wind direction is averaged as a direction
aggregate_records <- function(dt, params, res, fun = "auto") {
  if (res == "raw" || !nrow(dt)) return(dt)
  t <- dt$TIMESTAMP
  bin <- switch(res, hour = as.POSIXct(floor(as.numeric(t) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
                day = as.POSIXct(floor(as.numeric(t) / 86400) * 86400, origin = "1970-01-01", tz = "UTC"),
                month = as.POSIXct(paste0(format(t, "%Y-%m", tz = "UTC"), "-01"), tz = "UTC"))
  d <- dt[, c(intersect(c("station", "stream", "logger_id"), names(dt)), params), with = FALSE][, TIMESTAMP := bin]
  circ <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_real_)
    (atan2(mean(sin(x * pi / 180)), mean(cos(x * pi / 180))) * 180 / pi) %% 360 }
  f1 <- function(x, how) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else switch(how, mean = mean(x), min = min(x), max = max(x), sum = sum(x)) }
  isdir <- grepl("wind_?dir", params, ignore.case = TRUE) & !grepl("sd|std", params, ignore.case = TRUE)
  how <- if (fun == "auto") ifelse(grepl(TOTAL_RX, params, ignore.case = TRUE), "sum", "mean") else rep(fun, length(params))
  out <- d[, c(list(station = station[1], stream = stream[1], logger_id = logger_id[1]),
               Map(f1, .SD, how)), by = TIMESTAMP, .SDcols = params]
  if (any(isdir)) { dd <- d[, lapply(.SD, circ), by = TIMESTAMP, .SDcols = params[isdir]]
    for (v in params[isdir]) set(out, j = v, value = dd[[v]][match(out$TIMESTAMP, dd$TIMESTAMP)]) }
  setorder(out, TIMESTAMP); out
}
