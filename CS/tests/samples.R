# tests/samples.R  --  How the reader tests read CS's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
SAMPLE_KIND <- function(p) { h <- read_header(p); if (identical(h$status, "ok") && "TIMESTAMP" %in% h$columns[[1]] && !grepl("const|status|tableinfo|public", h$table %||% "", ignore.case = TRUE)) h$format else NA_character_ }   # measurement tables
SAMPLE_READ <- function(p, k) { h <- read_header(p)
  list(header = h[intersect(c("format", "station_name", "model", "serial", "os", "program", "table", "columns", "units", "process", "sep", "status"), names(h))],
       data = if (k == "TOB3") read_tob3(p) else read_toa5(p, h$columns[[1]], NA, h$sep)) }
SAMPLE_TRIM <- "TOA5"; SAMPLE_NO_ROWS <- character()
