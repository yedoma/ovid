# Ovid/hobo.R  --  Reading the records of an Onset .hobo file directly, for a download that has no HOBOware CSV export.
# Part of Ovid Core (shared). Sourced by the apps that read HOBO loggers; not meant to be run on its own.
#
# The .hobo format is Onset's own and is not published; this reader follows what the files themselves show, checked
# against the HOBOware CSV exported from the same files (core/tests: hobo_decode).
#   Header: "HOBO", then records 0x88 tag length value. Tag 0x05 the model, 0x06 the serial, 0x07 the launch time (year as
#   two bytes b0*100+b1, then month, day, hour, minute, second), 0x08 the logging interval in seconds (big-endian), 0x22
#   bytes 5 to 8 the launch time in UTC (seconds since 2000-01-01; with tag 0x07 it gives the clock's UTC offset), bytes 9 to 11 the start delay in seconds, 0x14 the time zone text, 0x31 the calibration (big-endian float32s).
#   Data: after the header's 0xff padding, a bit stream (most significant bit first) of 24-bit records and of events. A
#   record is a 14-bit pressure count x then a 10-bit thermistor count, then 8 bits of battery voltage when that channel
#   was logged (the third of the channel flags in tag 0x0b; skipped here). An event is 14 one bits (a count no record can hold),
#   a 4-bit type (0 coupler detached, 1 coupler attached, 3 stopped, 6 host connected), a 4-bit length n, then the seconds
#   since the record or event before it in 4n bits. Events take no slot: record i is at launch + delay + i * interval. A
#   stopped event, or a type of 15 (erased memory, all ones), ends the records; what follows is old memory. Events are
#   not whole bytes or nibbles long, so every record after one sits at another bit offset (read in bytes it looks packed
#   another way). Checked record for record against the HOBOware CSV of 30 real U20 files.
# U20 and U20L water level loggers: temperature from the thermistor count
#   t = count + 0.5, L = ln(t / (1023 - t)), T = 1 / (3.35371024e-3 + 2.78596313e-4 L + 4.15849337e-6 L^2 + 1.35850424e-7 L^3) - 273.15
#   (the Steinhart-Hart form; Steinhart and Hart 1968), and absolute pressure in psi from the logger's own calibration
#   P = f0 + f1 T + f2 x + f3 T^2 + f4 x^2 + f5 x T, then kPa = 6.894757 psi.
# Other models are not read yet (their files keep needing the CSV export).

HOBO_DECODE_MODELS <- "U20"
HOBO_PSI_KPA <- 6.894757

hobo_header <- function(b) {
  if (length(b) < 8 || !identical(rawToChar(b[1:4]), "HOBO")) return(NULL)
  i <- 5L; h <- list()
  while (i + 2L <= length(b) && b[i] == as.raw(0x88)) { tag <- as.integer(b[i + 1L]); n <- as.integer(b[i + 2L])
    v <- if (n) b[(i + 3L):(i + 2L + n)] else raw(); h[[as.character(tag)]] <- c(h[[as.character(tag)]], list(v)); i <- i + 3L + n }
  list(h = h, data_start = i) }
hobo_be <- function(r) sum(as.integer(r) * 256^(rev(seq_along(r)) - 1))

# The records of a .hobo file as a data.table (TIMESTAMP in the logger's clock, pres_kpa, temp_c), with attributes model,
# serial, zone (the time zone text the file holds), utc_offset_h (the clock's offset, from the launch time in UTC), interval_s and stopped (TRUE when the stream ended at something not
# understood, so the later records are missing). NULL for a model it cannot read.
hobo_decode <- function(path) {
  b <- readBin(path, "raw", file.size(path)); hh <- hobo_header(b); if (is.null(hh)) return(NULL); h <- hh$h
  txt <- function(tag) if (is.null(h[[tag]])) NA_character_ else trimws(rawToChar(h[[tag]][[1]][h[[tag]][[1]] != as.raw(0)]))
  model <- txt("5"); if (is.na(model) || !grepl(HOBO_DECODE_MODELS, model) || is.null(h[["49"]]) || is.null(h[["7"]]) || is.null(h[["8"]])) return(NULL)
  lt <- as.integer(h[["7"]][[1]]); launch <- ISOdatetime(lt[1] * 100 + lt[2], lt[3], lt[4], lt[5], lt[6], lt[7], tz = "UTC")
  iv <- hobo_be(h[["8"]][[length(h[["8"]])]]);   # a file can hold an earlier interval too: the last one is the launch's (as HOBOware reads it)
  delay <- if (!is.null(h[["34"]]) && length(h[["34"]][[1]]) >= 11) hobo_be(h[["34"]][[1]][9:11]) else 0
  cr <- h[["49"]][[1]]; cal <- readBin(cr, "numeric", n = length(cr) %/% 4, size = 4, endian = "big")
  bits <- as.integer(matrix(rawToBits(b[hh$data_start:length(b)]), 8L)[8:1, ]); N <- length(bits)
  rd <- function(p, n) if (n) sum(bits[p:(p + n - 1L)] * 2^((n - 1L):0)) else 0
  phys <- function(x, t) { L <- log(t / (1023 - t)); Tc <- 1 / (3.35371024e-3 + 2.78596313e-4 * L + 4.15849337e-6 * L^2 + 1.35850424e-7 * L^3) - 273.15
    c(cal[1] + cal[2] * Tc + cal[3] * x + cal[4] * Tc^2 + cal[5] * x^2 + cal[6] * x * Tc, Tc) }
  p <- 1L; while (p + 3L <= N && all(bits[p:(p + 3L)] == 1L)) p <- p + 4L      # the header's 0xff padding
  ch <- vapply(h[["11"]] %||% list(), function(v) length(v) > 0 && as.integer(v[1]) == 1L, NA); extra <- 8L * max(0L, sum(ch) - 2L)   # battery: 8 more bits a record
  X <- numeric(); C <- numeric(); stopped <- FALSE; k <- 0L
  while (p + 23L <= N) {
    if (all(bits[p:(p + 13L)] == 1L)) {                              # an event: skipped, it takes no slot
      ty <- rd(p + 14L, 4L); n <- rd(p + 18L, 4L); if (ty == 15) break      # erased memory: the end of the records
      p <- p + 22L + 4L * n; if (ty == 3) break; next }                # stopped: what follows is old memory
    x <- rd(p, 14L); t <- rd(p + 14L, 10L) + 0.5; if (!(t > 1 && t < 1022)) { stopped <- TRUE; break }
    k <- k + 1L; X[k] <- x; C[k] <- t; p <- p + 24L + extra }
  v <- if (k) vapply(seq_len(k), function(i) phys(X[i], C[i]), numeric(2)) else matrix(numeric(), 2)
  P <- v[1, ]; Tt <- v[2, ]
  out <- data.table::data.table(TIMESTAMP = launch + delay + (seq_along(P) - 1) * iv, pres_kpa = P * HOBO_PSI_KPA, temp_c = Tt)
  utc <- if (!is.null(h[["34"]]) && length(h[["34"]][[1]]) >= 8) as.POSIXct("2000-01-01", tz = "UTC") + hobo_be(h[["34"]][[1]][5:8]) else NA
  off <- round(as.numeric(difftime(launch, utc, units = "hours")) * 4) / 4; if (!isTRUE(abs(off) <= 14)) off <- NA_real_   # the logger clock's offset from UTC
  attr(out, "utc_offset_h") <- off; attr(out, "model") <- model; attr(out, "serial") <- txt("6"); attr(out, "zone") <- txt("20"); attr(out, "interval_s") <- iv; attr(out, "stopped") <- stopped
  out }
