# tests/samples.R  --  How the reader tests read WLR's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
SAMPLE_KIND <- function(p) sniff_format(p)
SAMPLE_READ <- function(p, k) read_logger_file(p, k)
SAMPLE_TRIM <- c("hobo_csv", "solinst_csv"); SAMPLE_NO_ROWS <- "hobo_binary"
