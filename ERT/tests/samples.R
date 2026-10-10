# tests/samples.R  --  How the reader tests read ERT's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
SAMPLE_KIND <- function(p) { f <- sniff_format(p); d <- Filter(function(x) identical(x$format, f), FORMATS); if (length(d) && isTRUE(d[[1]]$readable)) f else NA_character_ }
SAMPLE_READ <- function(p, k) { r <- read_format(p, k); list(meta = r$meta, meas = r$meas) }
SAMPLE_TRIM <- character(); SAMPLE_NO_ROWS <- character()
