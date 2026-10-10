# Ovid/hobo.R  --  Reading the records of an Onset .hobo file directly, for a download that has no HOBOware CSV export.
# Part of Ovid Core (shared). Sourced by the apps that read HOBO loggers; not meant to be run on its own.
#
# The .hobo format is Onset's own and is not published; this reader follows what the files themselves show, checked
# against the HOBOware CSV exported from the same files (core/tests: hobo_decode).
#   Header: "HOBO", then records 0x88 tag length value. Tag 0x05 the model, 0x06 the serial, 0x07 the launch time (year as
#   two bytes b0*100+b1, then month, day, hour, minute, second), 0x08 the logging interval in seconds (big-endian), 0x22
#   bytes 9 to 11 the start delay in seconds, 0x14 the time zone text, 0x31 the calibration (big-endian float32s).
#   Data: after the header's 0xff padding, a nibble stream of 3-byte records; an escape starts with the nibbles fff (the
#   records after the first, events such as coupler attached or host connected) and the stream resumes on the next record.
#   Record i is at launch + delay + i * interval (events take no slot).
# U20 and U20L water level loggers: a record is two 12-bit values v1, v2. Temperature from the thermistor count
#   t = (v2 >> 2) + 0.5, L = ln(t / (1023 - t)), T = 1 / (3.35371024e-3 + 2.78596313e-4 L + 4.15849337e-6 L^2 + 1.35850424e-7 L^3) - 273.15
#   (the Steinhart-Hart form; Steinhart and Hart 1968), and absolute pressure in psi from the logger's own calibration
#   x = v1 + 4096 (v2 & 3), P = f0 + f1 T + f2 x + f3 T^2 + f4 x^2 + f5 x T, then kPa = 6.894757 psi.
# Other models are not read yet (their files keep needing the CSV export). The first record of a download is often taken
# before the logger settles and is kept as read.

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
# serial, zone (the time zone text the file holds), interval_s and stopped (TRUE when the stream ended at something not
# understood, so the later records are missing). NULL for a model it cannot read.
hobo_decode <- function(path) {
  b <- readBin(path, "raw", file.size(path)); hh <- hobo_header(b); if (is.null(hh)) return(NULL); h <- hh$h
  txt <- function(tag) if (is.null(h[[tag]])) NA_character_ else trimws(rawToChar(h[[tag]][[1]][h[[tag]][[1]] != as.raw(0)]))
  model <- txt("5"); if (is.na(model) || !grepl(HOBO_DECODE_MODELS, model) || is.null(h[["49"]]) || is.null(h[["7"]]) || is.null(h[["8"]])) return(NULL)
  lt <- as.integer(h[["7"]][[1]]); launch <- ISOdatetime(lt[1] * 100 + lt[2], lt[3], lt[4], lt[5], lt[6], lt[7], tz = "UTC")
  iv <- hobo_be(h[["8"]][[1]]); delay <- if (!is.null(h[["34"]]) && length(h[["34"]][[1]]) >= 11) hobo_be(h[["34"]][[1]][9:11]) else 0
  cr <- h[["49"]][[1]]; cal <- readBin(cr, "numeric", n = length(cr) %/% 4, size = 4, endian = "big")
  nib <- paste(sprintf("%02x", as.integer(b[hh$data_start:length(b)])), collapse = ""); N <- nchar(nib)
  val <- function(q) { if (q + 5 > N) return(NULL); s <- substr(nib, q, q + 5); if (substr(s, 1, 3) == "fff") return(NULL)
    v1 <- strtoi(substr(s, 1, 3), 16L); v2 <- strtoi(substr(s, 4, 6), 16L); t <- bitwShiftR(v2, 2) + 0.5; if (!(t > 0 && t < 1023)) return(NULL)
    L <- log(t / (1023 - t)); Tc <- 1 / (3.35371024e-3 + 2.78596313e-4 * L + 4.15849337e-6 * L^2 + 1.35850424e-7 * L^3) - 273.15
    x <- v1 + 4096 * bitwAnd(v2, 3L); c(cal[1] + cal[2] * Tc + cal[3] * x + cal[4] * Tc^2 + cal[5] * x^2 + cal[6] * x * Tc, Tc) }
  p <- 1L; while (p <= N && substr(nib, p, p) == "f") p <- p + 1L
  P <- numeric(); Tt <- numeric(); prev <- NULL; stopped <- FALSE; chain <- 0
  while (p + 5 <= N) {
    if (substr(nib, p, p + 2) == "fff") {                            # an escape: resume where the records carry on
      best <- NA
      ty <- substr(nib, p + 3, p + 3)
      for (L in c(6, 7, 8, 5, 4, 9, 10, 11)) {
        if (substr(nib, p + L, p + L + 2) == "fff") { if ((ty == "1" && L == 7) || (ty %in% c("c", "d") && L == 8)) { best <- L; break } else next }   # one event after another
        sq <- lapply(0:3, function(j) val(p + L + 6 * j)); if (any(vapply(sq, is.null, NA))) next
        m <- do.call(rbind, sq); if (any(!(m[, 1] > 0 & m[, 1] < 60 & m[, 2] > -40 & m[, 2] < 60))) next
        if (length(P) > 1 && (any(abs(diff(m[, 1])) >= 2) || any(abs(diff(m[, 2])) >= 12))) next
        if (length(P) > 1 && !is.null(prev) && chain == 0 && (abs(m[1, 1] - prev[1]) >= 2.5 || abs(m[1, 2] - prev[2]) >= 12)) next
        best <- L; break }
      if (is.na(best)) break                                            # the end of the records (what follows is old memory)
      chain <- chain + 1; p <- p + best; next }                      # after a chain of events (a download) the level may jump
    v <- val(p); if (is.null(v)) { stopped <- TRUE; break }
    P <- c(P, v[1]); Tt <- c(Tt, v[2]); prev <- v; chain <- 0; p <- p + 6L }
  out <- data.table::data.table(TIMESTAMP = launch + delay + (seq_along(P) - 1) * iv, pres_kpa = P * HOBO_PSI_KPA, temp_c = Tt)
  attr(out, "model") <- model; attr(out, "serial") <- txt("6"); attr(out, "zone") <- txt("20"); attr(out, "interval_s") <- iv; attr(out, "stopped") <- stopped
  out }
