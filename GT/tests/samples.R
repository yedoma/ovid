# tests/samples.R  --  How the reader tests read GT's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
SAMPLE_KIND <- function(p) sniff_format(p)
SAMPLE_READ <- function(p, k) read_logger_file(p, k)
SAMPLE_TRIM <- c("fg2_csv", "hobo_csv", "rbr_txt", "rbr_hex", "campbell_toa5"); SAMPLE_NO_ROWS <- "hobo_binary"; SAMPLE_MAX_BYTES <- 2e6
