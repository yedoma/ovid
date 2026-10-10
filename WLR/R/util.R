# R/util.R  --  Helpers whose rules are WLR's own: date styles in HOBOware, Solinst and survey exports; serial numbers.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R after the shared core (Ovid/util.R); not meant to be run on its own.


# Date-time styles seen in HOBOware, Solinst and survey exports, in the order parse_dt() (shared core) tries them.
DT_FORMATS <- c("%m/%d/%y %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
                "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")

SERIAL_KEEP <- "digits"           # norm_serial() (core util.R): WLR serials are digits only
# Serial numbers: digits only. Solinst serials sometimes carry a prefix such as 007 that the metadata
# workbook keeps or drops, so two serials match when one ends with the other (at least 6 digits).
serial_match <- function(a, b) {
  a <- norm_serial(a); b <- norm_serial(b)
  ok <- nzchar(a) & nzchar(b)
  same <- ok & a == b
  pre <- ok & !same & pmin(nchar(a), nchar(b)) >= 6 & (endsWith(a, b) | endsWith(b, a))
  ifelse(same, "exact", ifelse(pre, "prefix", "none"))
}
