# tests/samples.R  --  How the reader tests read CS's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
SAMPLE_KIND <- function(p) { h <- read_header(p); if (identical(h$status, "unsupported_mixed_array")) return("mixed-array")
  if (identical(h$status, "ok") && "TIMESTAMP" %in% h$columns[[1]] && !grepl("const|status|tableinfo|public", h$table %||% "", ignore.case = TRUE)) h$format else NA_character_ }   # measurement tables
# A mixed-array (CR10X) sample is read with the .FSL beside it of the same name, without "<kind>__" (its labels, as cr10x.csv would give).
SAMPLE_FSL <- function(p) sub("\\.dat$", ".fsl", sub("^[^_]+(-[^_]+)?__", "", basename(p)), ignore.case = TRUE)
SAMPLE_READ <- function(p, k) { h <- read_header(p)
  if (k == "mixed-array") { f <- file.path(dirname(p), SAMPLE_FSL(p))
    tabs <- parse_array_file(p, "CR10X_SAMPLE", if (file.exists(f)) load_fsl_set(f, list(project_dir = tempdir())) else list(), NA, NA)
    return(c(list(header = h[c("format", "status")], data = if (length(tabs)) tabs[[1]]), tabs[-1])) }   # the first array as data, any others after it
  list(header = h[intersect(c("format", "station_name", "model", "serial", "os", "program", "table", "columns", "units", "process", "sep", "status"), names(h))],
       data = if (k == "TOB3") read_tob3(p) else read_toa5(p, h$columns[[1]], NA, h$sep)) }
SAMPLE_TRIM <- "TOA5"; SAMPLE_NO_ROWS <- character()
