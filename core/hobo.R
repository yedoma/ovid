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

# ---- Other HOBO models (U12-008, U24-001, TidbiT UTBI-001) --------------------------------------------------------------
# Same file layout as the U20: the tag header, 0xff padding, then a bit stream of records and events, record i at
# launch + delay + i * interval, events taking no slot. What differs is the record, and the event mark is as many one
# bits as the record's first field (as the U20's 14):
#   TidbiT UTBI-001  one 12-bit thermistor count.
#   U12-008          a 12-bit thermistor count per external channel (tag 0x0b on, sensor type 0x95 in tag 0x0c), then
#                    8 bits of battery when the fifth channel is on (skipped, as the U20's battery).
#   U24-001          a 16-bit low-range count, a 16-bit full-range count and a 12-bit temperature count (44 bits).
# Thermistor (TidbiT, U12 probes): 1/T = sum c_k L^k (k = 0..7) with L = ln((count + 0.5) / (4095.5 - count)), the ratio
#   of a 12-bit divider (c0 = 1/298.15 K: 25 C at mid-scale). Fitted (minimax) to HOBOware's exports, which give C to 0.001
#   (its F values are C rounded to 0.001, then converted): every one of the 2,723 counts seen from -55 to 85 C comes out
#   exactly as HOBOware prints it. Outside that range the values mostly come from an open or shorted probe (HOBOware prints
#   them, up to 537 C); they are left NA and counted in attr "out_of_range".
# U24: temperature = t0 + t1 * count (tag 0x31, third block); each conductivity range = sum c_k x^k (k = 0..5, tag 0x31) with
#   x = min(count, c9) * (1 + (c7 + c8 * temperature) / 100), in uS/cm. Checked against the HOBOware export of 53 files:
#   every record within the export's 0.1 uS/cm and 0.01 C.
HOBO_DECODE_MORE <- "U12-008|U24-001|UTBI-001"
HOBO_TMC <- list(c = c(0.0033542432429378107, 0.00027892603891183532, 4.2456309656381064e-06, 2.2303027410495148e-07,
                       1.5458333292268499e-08, -6.3590643709237659e-09, -6.5028069971222435e-10, -2.3991770070640607e-11),
                 off = 0.5, full = 4096, range = c(-55, 85))

hobo_tmc_c <- function(x) { L <- log((x + HOBO_TMC$off) / (HOBO_TMC$full - x - HOBO_TMC$off))
  t <- 1 / Reduce(`+`, lapply(seq_along(HOBO_TMC$c), function(k) HOBO_TMC$c[k] * L^(k - 1L))) - 273.15
  t[!is.finite(t) | t < HOBO_TMC$range[1] | t > HOBO_TMC$range[2]] <- NA_real_; t }

# The records of a U12-008, U24-001 or TidbiT .hobo file as a data.table: TIMESTAMP (logger clock) and temp_c (TidbiT),
# temp_c_<label> per channel (U12-008), or ec_low_us, ec_full_us, temp_c (U24-001); attributes as hobo_decode's, plus
# out_of_range. NULL for any other model or layout.
hobo_decode_more <- function(path) {
  b <- readBin(path, "raw", file.size(path)); hh <- hobo_header(b); if (is.null(hh)) return(NULL); h <- hh$h
  txt <- function(tag) if (is.null(h[[tag]])) NA_character_ else trimws(rawToChar(h[[tag]][[1]][h[[tag]][[1]] != as.raw(0)]))
  model <- txt("5"); if (is.na(model) || !grepl(HOBO_DECODE_MORE, model) || is.null(h[["7"]]) || is.null(h[["8"]])) return(NULL)
  on <- vapply(h[["11"]] %||% list(), function(v) length(v) > 0 && as.integer(v[1]) == 1L, NA)
  ty <- vapply(h[["12"]] %||% list(), function(v) if (length(v)) as.integer(v[1]) else 0L, 0L)
  lab <- vapply(h[["181"]] %||% list(), function(v) trimws(rawToChar(v[v != as.raw(0)])), "")
  if (grepl("UTBI-001", model)) { if (sum(on) != 1L) return(NULL); fields <- 12L; extra <- 0L; first <- 12L
  } else if (grepl("U12-008", model)) { k <- which(on[1:min(4L, length(on))] & ty[seq_along(on)][1:min(4L, length(on))] == 0x95)
    if (!length(k) || sum(on[1:4]) != length(k)) return(NULL)                    # a channel that is not a thermistor probe: not read
    fields <- rep(12L, length(k)); extra <- if (length(on) >= 5 && on[5]) 8L else 0L; first <- 12L
  } else { if (sum(on) != 3L || length(h[["49"]]) < 3) return(NULL); fields <- c(16L, 16L, 12L); extra <- 0L; first <- 16L }
  lt <- as.integer(h[["7"]][[1]]); launch <- ISOdatetime(lt[1] * 100 + lt[2], lt[3], lt[4], lt[5], lt[6], lt[7], tz = "UTC")
  iv <- hobo_be(h[["8"]][[length(h[["8"]])]])
  delay <- if (!is.null(h[["34"]]) && length(h[["34"]][[1]]) >= 11) hobo_be(h[["34"]][[1]][9:11]) else 0
  s0 <- 4L * ((hh$data_start + 4L) %/% 4L) + 1L                       # padding: the stream starts on the first 4-byte boundary
  bits <- as.integer(matrix(rawToBits(b[s0:length(b)]), 8L)[8:1, ]); N <- length(bits)   # two or more bytes after the header
  rd <- function(p, n) sum(bits[p:(p + n - 1L)] * 2^((n - 1L):0))
  p <- 1L; t0 <- launch + delay
  W <- sum(fields) + extra; off <- cumsum(c(0L, fields))[seq_along(fields)]
  rows <- list(); k <- 0L; stopped <- FALSE
  while (p + W - 1L <= N) {
    if (all(bits[p:(p + first - 1L)] == 1L)) {                       # an event: skipped, it takes no slot
      ty_ <- rd(p + first, 4L); n <- rd(p + first + 4L, 4L); if (ty_ == 15) break
      if (ty_ == 2 && k == 0L && n) t0 <- launch + rd(p + first + 8L, 4L * n)   # started (button or set time): seconds after the launch
      p <- p + first + 8L + 4L * n; if (ty_ == 3) break; next }
    k <- k + 1L; rows[[k]] <- vapply(seq_along(fields), function(j) rd(p + off[j], fields[j]), 0); p <- p + W }
  if (p + W - 1L > N && k > 0 && p <= N && !all(bits[p:N] == 1L)) stopped <- TRUE
  M <- if (k) do.call(rbind, rows) else matrix(numeric(), 0, length(fields))
  ts <- t0 + (seq_len(k) - 1) * iv
  if (grepl("U24-001", model)) {
    cal <- lapply(h[["49"]][1:3], function(r) readBin(r, "numeric", n = length(r) %/% 4, size = 4, endian = "big"))
    tc <- cal[[3]][1] + cal[[3]][2] * M[, 3]
    ec <- function(cc, x) { x <- pmin(x, cc[10]) * (1 + (cc[8] + cc[9] * tc) / 100); Reduce(`+`, lapply(0:5, function(i) cc[i + 1] * x^i)) }
    out <- data.table::data.table(TIMESTAMP = ts, ec_low_us = ec(cal[[1]], M[, 1]), ec_full_us = ec(cal[[2]], M[, 2]), temp_c = tc); oor <- 0L
  } else {
    tt <- hobo_tmc_c(as.vector(M)); dim(tt) <- dim(M); oor <- sum(is.na(tt))
    out <- data.table::data.table(TIMESTAMP = ts)
    lb <- lab[seq_len(ncol(M))]; lb[is.na(lb) | !nzchar(lb)] <- paste0("ch", seq_len(ncol(M)))[is.na(lb) | !nzchar(lb)]   # no label: ch1, ch2, ...
    nm <- if (grepl("UTBI-001", model)) "temp_c" else paste0("temp_c_", make.unique(lb))
    for (j in seq_len(ncol(M))) data.table::set(out, j = nm[j], value = tt[, j])
  }
  utc <- if (!is.null(h[["34"]]) && length(h[["34"]][[1]]) >= 8) as.POSIXct("2000-01-01", tz = "UTC") + hobo_be(h[["34"]][[1]][5:8]) else NA
  offh <- round(as.numeric(difftime(launch, utc, units = "hours")) * 4) / 4; if (!isTRUE(abs(offh) <= 14)) offh <- NA_real_
  attr(out, "utc_offset_h") <- offh; attr(out, "model") <- model; attr(out, "serial") <- txt("6"); attr(out, "zone") <- txt("20")
  attr(out, "interval_s") <- iv; attr(out, "stopped") <- stopped; attr(out, "out_of_range") <- oor
  out }
