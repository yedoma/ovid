# tests/samples.R  --  How the reader tests read GS's sample files (core testkit.R). Sourced by run_tests.R and make_samples.R.
# Survey files go through read_format(); the AGI companions (.crs, .cmd) and the MALA GPR files (R/gpr.R: .rad header, .rd3
# traces read with the samples of the .rad of the same name, .cor GPS fixes, .mrk markers) through their own readers.
if (!exists("read_rad")) source(file.path(here, "R", "gpr.R"))
SAMPLE_KIND <- function(p) { if (grepl("\\.(rad|rd3|cor|mrk)$", p, TRUE)) return(paste0("gpr_", tolower(tools::file_ext(p))))
  f <- sniff_format(p); d <- Filter(function(x) identical(x$format, f), FORMATS); if (length(d) && isTRUE(d[[1]]$readable)) f else NA_character_ }
SAMPLE_READ <- function(p, k) switch(k,
  agi_crs = read_agi_crs(p), agi_cmd = { x <- read_agi_cmd(p); list(geometry = x$geometry, header = as.data.table(x$header), commands = x$commands) },
  gpr_rad = as.data.table(lapply(read_rad(p), function(v) v %||% NA)), gpr_cor = read_cor(p), gpr_mrk = read_mrk(p),
  gpr_rd3 = { h <- read_rad(sub("gpr_rd3__(.*)\\.rd3$", "gpr_rad__\\1.rad", p, ignore.case = TRUE)); m <- read_gpr_data(p, h$samples)
              data.table(trace = seq_len(ncol(m)), first = m[1, ], s100 = m[min(100L, nrow(m)), ], min = apply(m, 2, min), max = apply(m, 2, max), sum = colSums(m)) },
  { r <- read_format(p, k); list(meta = r$meta, meas = r$meas) })
SAMPLE_TRIM <- character(); SAMPLE_NO_ROWS <- character()
