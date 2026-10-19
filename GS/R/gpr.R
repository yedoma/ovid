# R/gpr.R  --  Ground-penetrating radar (MALA): finding the files of each line, reading them, and drawing the radargram and the
# GPS track. Sourced by app.R only. GPR lines are viewed beside the ERT surveys, not catalogued as surveys: they have no
# quadrupole measurements, so the survey, QC and inversion steps do not apply to them.
#
# A MALA line is three files that sit in different folders: the .rad header (text, KEY:VALUE), the trace data (.rd3 = 16-bit,
# .rd7 = 32-bit integers, little-endian, samples x traces) and, when a GPS was connected, the .cor positions (one row per fix,
# not per trace: trace, date, time, lat, N/S, lon, E/W, elevation, M, HDOP). gpr_scan() puts them together from what the
# header says (see there). Raw files are never changed.

# ---- readers ----------------------------------------------------------------------------------------------------------------
read_rad <- function(path) {
  L <- readLines(path, warn = FALSE, encoding = "latin1"); i <- regexpr(":", L, fixed = TRUE); ok <- i > 0
  h <- setNames(trimws(substring(L[ok], i[ok] + 1L)), toupper(trimws(substr(L[ok], 1L, i[ok] - 1L))))
  num <- function(k) suppressWarnings(as.numeric(h[k])); if (is.na(h["ANTENNAS"])) h["ANTENNAS"] <- ""
  list(samples = as.integer(num("SAMPLES")), last_trace = as.integer(num("LAST TRACE")), timewindow_ns = num("TIMEWINDOW"),
       sampling_mhz = num("FREQUENCY"), antennas = unname(h["ANTENNAS"]), antenna_sep_m = num("ANTENNA SEPARATION"), stacks = as.integer(num("STACKS")),
       time_interval_s = num("TIME INTERVAL"), distance_interval_m = num("DISTANCE INTERVAL"), distance_flag = num("DISTANCE FLAG"),
       operator = unname(h["OPERATOR"]), site = unname(h["SITE"]), comment = unname(h["COMMENT"]))
}

# GPS fixes. Rows without a usable position are dropped.
read_cor <- function(path) {
  L <- trimws(readLines(path, warn = FALSE)); L <- L[nzchar(L)]
  e <- data.table(trace = integer(), datetime = as.POSIXct(character(), tz = "UTC"), lat = numeric(), lon = numeric(), elev_m = numeric(), hdop = numeric())
  if (!length(L)) return(e)
  t <- tstrsplit(L, "[ \t]+", fill = NA)
  if (length(t) < 7) return(e)
  g <- function(i) if (length(t) >= i) t[[i]] else rep(NA_character_, length(L))
  d <- data.table(trace = suppressWarnings(as.integer(g(1))), datetime = as.POSIXct(paste(g(2), g(3)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
                  lat = suppressWarnings(as.numeric(g(4))) * ifelse(g(5) == "S", -1, 1), lon = suppressWarnings(as.numeric(g(6))) * ifelse(g(7) == "W", -1, 1),
                  elev_m = suppressWarnings(as.numeric(g(8))), hdop = suppressWarnings(as.numeric(g(10))))
  n0 <- nrow(d)                                                                     # fixes in the file, to say how many are left out
  d <- d[!is.na(trace) & !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180 & !(lat == 0 & lon == 0)]
  # Some loggers drop digits from a coordinate now and then (latitude 4.5 for 45.5; longitude -7.2 for -72.2). A fix more
  # than 0.01 degree (about a kilometre) from the running median of the 11 around it is taken out.
  if (nrow(d) >= 5L) { k <- min(11L, 2L * ((nrow(d) - 1L) %/% 2L) + 1L)
    d <- d[abs(lat - runmed(lat, k, endrule = "median")) <= 0.01 & abs(lon - runmed(lon, k, endrule = "median")) <= 0.01] }
  setattr(d, "dropped", n0 - nrow(d))                                                # impossible values and stray fixes left out (for display)
  d
}

# Traces as a samples x traces matrix. A file a few traces longer than the header says (seen on MALA ProEx) is read in full.
read_gpr_data <- function(path, samples, bytes = 2L) {
  n <- file.size(path) %/% (samples * bytes)
  matrix(as.numeric(readBin(path, "integer", n = n * samples, size = bytes, signed = TRUE, endian = "little")), nrow = samples)
}

# ---- matching the files of a line ---------------------------------------------------------------------------------------------
# Each .rad is the line. Its data file is the .rd3/.rd7 with the same name. The size must fit the header: traces = size / (samples x bytes),
# a whole number from LAST TRACE to LAST TRACE + 10 ("ok"); fewer traces than the header says is shown as "truncated" (an acquisition
# stopped early, or a copy cut short). If several files share the name, the best fit, then the one in the same folder. A header whose
# name has no data file is matched by an exact size alone, when exactly one free file fits. The .cor (GPS) and .mrk (markers) are matched
# by name, then by folder. Nothing is guessed: a line that cannot be matched says why.
#
# A hidden ".NAME.rd3.partial" is the file the Drive scraper (Logger Consolidator) writes before it renames the copy into place. Where the
# rename failed, the file is whole: the copy had already been re-hashed against the source, and the failure is that a "uchg" (immutable) flag
# copied from the source stops it being renamed. Such a file is read in place, flagged as read from a .partial copy; recover_partials.py
# (Logger_tools) verifies and renames them.
GPR_SLACK <- 10L

gpr_scan <- function(dirs) {
  dirs <- path.expand(dirs); dirs <- dirs[dir.exists(dirs)]; if (!length(dirs)) return(data.table())   # every folder missing (a drive not plugged in)
  ls <- function(pat, all = FALSE) unlist(lapply(dirs, function(d) list.files(d, pattern = pat, recursive = TRUE, full.names = TRUE, all.files = all, ignore.case = TRUE)))
  fl <- function(p) { p <- unique(p); data.table(path = p, stem = tools::file_path_sans_ext(basename(p)), folder = dirname(p), size = file.size(p)) }
  rad <- fl(ls("\\.rad$")); cor <- fl(ls("\\.cor$")); mrk <- fl(ls("\\.mrk$"))
  if (!nrow(rad)) return(data.table())
  pp <- ls("\\.rd[37]\\.partial$", all = TRUE)
  dat <- rbindlist(list(fl(ls("\\.rd[37]$"))[, partial := FALSE],
                        if (length(pp)) fl(pp)[, `:=`(stem = sub("^\\.", "", sub("\\.rd[37]\\.partial$", "", basename(path), ignore.case = TRUE)), partial = TRUE)]), fill = TRUE)
  if (!nrow(dat)) dat <- data.table(path = character(), stem = character(), folder = character(), size = numeric(), partial = logical())
  dat[, bytes := ifelse(grepl("\\.rd7(\\.partial)?$", path, ignore.case = TRUE), 4L, 2L)]
  H <- lapply(rad$path, function(p) tryCatch(read_rad(p), error = function(e) NULL))
  rad[, `:=`(samples = vapply(H, function(h) h$samples %||% NA_integer_, 0L), traces = vapply(H, function(h) h$last_trace %||% NA_integer_, 0L))]
  ntr <- function(size, bytes, s) floor(size / (s * bytes))                         # whole traces in the file
  fit <- function(size, bytes, s, n) { r <- size / (s * bytes)                        # 1 ok (whole traces, LAST TRACE to + slack), 2 truncated (fewer than the header says; a cut last trace is dropped)
    fcase(!is.na(r) & r >= 1 & r == round(r) & r >= n & r <= n + GPR_SLACK, 1L, !is.na(r) & r < n & floor(r) >= 1, 2L, default = 0L) }   # no traces is never a fit
  take <- rep(NA_character_, nrow(rad)); how <- rep("", nrow(rad)); msg <- rep("", nrow(rad)); kind <- rep(0L, nrow(rad))
  # pass 1: by name
  for (i in seq_len(nrow(rad))) {
    r <- rad[i]; if (is.na(r$samples) || is.na(r$traces)) { msg[i] <- "header not read"; next }
    if (r$samples < 1L || r$traces < 1L) { msg[i] <- "no traces (the header says 0)"; next }
    c1 <- dat[stem == r$stem]; if (!nrow(c1)) next
    c1[, k := fit(size, bytes, r$samples, r$traces)]; c1 <- c1[k > 0]
    if (!nrow(c1)) { msg[i] <- "data file size does not fit the header"; next }
    setorder(c1, k, partial); b <- c1[k == k[1] & partial == partial[1]]; if (nrow(b) > 1L) b <- b[folder == r$folder]
    if (nrow(b) == 1L) { take[i] <- b$path; how[i] <- "name"; kind[i] <- b$k } else msg[i] <- "several data files fit"
  }
  # pass 2: by exact size, for headers with no data file of their name. A data file is used only when it is the one fit for that header
  # and that header is the only one it fits.
  free <- dat[!path %in% take]; todo <- which(is.na(take) & !rad$stem %in% dat$stem & !is.na(rad$samples) & rad$samples > 0 & !is.na(rad$traces) & rad$traces > 0)
  fitp <- lapply(todo, function(i) free[fit(size, bytes, rad$samples[i], rad$traces[i]) == 1L, path])
  for (j in seq_along(todo)) { i <- todo[j]; f <- fitp[[j]]
    if (length(f) > 1L) { same <- f[dirname(f) == rad$folder[i]]; if (length(same) == 1L) f <- same }
    if (length(f) == 1L && sum(vapply(fitp, function(x) f %in% x, TRUE)) == 1L) { take[i] <- f; how[i] <- "size"; kind[i] <- 1L }
    else if (length(f)) msg[i] <- "several lines fit the same data file" }
  pick <- function(tab, i) { c3 <- tab[stem == rad$stem[i]]; if (nrow(c3) > 1L) c3 <- c3[folder == rad$folder[i]]; if (nrow(c3) == 1L) c3$path else NA_character_ }
  out <- data.table(rad = rad$path, stem = rad$stem, folder = rad$folder, samples = rad$samples, traces = rad$traces, data = take, matched_by = how,
                    cor = vapply(seq_len(nrow(rad)), function(i) pick(cor, i), ""), mrk = vapply(seq_len(nrow(rad)), function(i) pick(mrk, i), ""))
  di <- match(out$data, dat$path)
  out[, `:=`(partial = dat$partial[di] %in% TRUE, file_traces = ntr(dat$size[di], dat$bytes[di], samples))]
  out[, status := fifelse(!is.na(data), fifelse(kind == 2L, "truncated", "ok"), fifelse(nzchar(msg), msg, "no data file"))]
  out[, note := fifelse(!is.na(data) & partial, "read from the unrenamed .partial copy", "")]
  out[status == "truncated", note := trimws(paste(note, sprintf("%d of %d traces in the file", as.integer(file_traces), traces)))]
  out[, `:=`(antennas = vapply(H, function(h) h$antennas %||% "", ""), timewindow_ns = vapply(H, function(h) h$timewindow_ns %||% NA_real_, 0))]
  dup <- duplicated(out$stem) | duplicated(out$stem, fromLast = TRUE)
  out[, line := ifelse(dup, paste0(stem, " (", basename(folder), ")"), stem)]
  setorder(out, stem); out[]
}

# Markers (.mrk): a header line ("HEADER VERSION:100"), then one line per marker: trace number, then two more numbers (30 and 1 in these
# files; what they are is not given in the manuals read, so they are kept as a and b). REFLEXW reads the first column as the trace number.
read_mrk <- function(path) {
  L <- trimws(readLines(path, warn = FALSE)); L <- L[grepl("^[0-9]", L)]
  e <- data.table(marker = integer(), trace = integer(), a = numeric(), b = numeric())
  if (!length(L)) return(e)
  t <- tstrsplit(L, "[ \t]+", fill = NA); g <- function(i) if (length(t) >= i) suppressWarnings(as.numeric(t[[i]])) else rep(NA_real_, length(L))
  d <- data.table(trace = as.integer(g(1)), a = g(2), b = g(3)); d <- d[!is.na(trace)][order(trace)]; d[, marker := .I][, .(marker, trace, a, b)]
}

# ---- positions ------------------------------------------------------------------------------------------------------------
haversine_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371000 * asin(sqrt(a)) }

# Distance along the line (m) for every trace 1..n: cumulative distance between the fixes, interpolated; before the first fix and
# after the last one the average pace between the fixes continues it.
gpr_distance <- function(cor, n) {
  if (is.null(cor) || nrow(cor) < 2L) return(NULL)
  cor <- cor[order(trace)][!duplicated(trace)]; if (nrow(cor) < 2L) return(NULL)
  d <- c(0, cumsum(haversine_m(cor$lat[-nrow(cor)], cor$lon[-nrow(cor)], cor$lat[-1], cor$lon[-1])))
  tr <- seq_len(n); pace <- (max(d) - min(d)) / (max(cor$trace) - min(cor$trace))
  x <- approx(cor$trace, d, tr, rule = 1)$y
  lo <- tr < min(cor$trace); hi <- tr > max(cor$trace)
  x[lo] <- d[1] - (min(cor$trace) - tr[lo]) * pace; x[hi] <- max(d) + (tr[hi] - max(cor$trace)) * pace
  x <- x - min(x); if (max(x) < 1) NULL else x       # fixes that never move (a GPS logged at one spot) give no distance
}

# Surface elevation (m) for every trace 1..n from the GPS heights of the .cor fixes: the fixes are smoothed with a running median of
# `smooth` fixes (a handheld GPS height is good to a few metres, much worse than its position), then interpolated along the trace number.
gpr_elevation <- function(cor, n, smooth = 11L) {
  if (is.null(cor) || nrow(cor) < 2L) return(NULL); cor <- cor[!is.na(elev_m)][order(trace)][!duplicated(trace)]; if (nrow(cor) < 2L) return(NULL)
  k <- min(as.integer(smooth) %/% 2L * 2L + 1L, 2L * ((nrow(cor) - 1L) %/% 2L) + 1L); z <- if (k >= 3L) stats::runmed(cor$elev_m, k, endrule = "median") else cor$elev_m
  approx(cor$trace, z, seq_len(n), rule = 2)$y
}
# Distance along the line (m) from the markers: the k-th marker is at (k - 1) x spacing; between and beyond the markers the pace of the
# nearest pair continues (REFLEXW resamples the traces between markers the same way, "mark interpol").
gpr_marker_distance <- function(mrk, spacing, n) {
  if (is.null(mrk) || nrow(mrk) < 2L || !isTRUE(spacing > 0)) return(NULL); tr <- mrk$trace; d <- (seq_along(tr) - 1) * spacing; k <- length(tr); i <- seq_len(n)
  x <- approx(tr, d, i, rule = 1)$y; lo <- i < tr[1]; hi <- i > tr[k]
  x[lo] <- d[1] - (tr[1] - i[lo]) * (d[2] - d[1]) / (tr[2] - tr[1]); x[hi] <- d[k] + (i[hi] - tr[k]) * (d[k] - d[k - 1]) / (tr[k] - tr[k - 1]); x
}
# The radargram on a common elevation grid: each trace is placed at its surface elevation less the depth of each sample. depth: m per sample
# (rows of m); elev: surface elevation per column. Rows outside a trace are NA.
gpr_topo <- function(m, depth, elev, max_rows = 2500L) {
  dz <- max(stats::median(diff(depth)), (diff(range(elev)) + max(depth)) / max_rows); top <- max(elev); bot <- min(elev) - max(depth); z <- seq(top, bot, by = -dz)
  out <- matrix(NA_real_, length(z), ncol(m)); for (j in seq_len(ncol(m))) out[, j] <- approx(elev[j] - depth, m[, j], z, rule = 1)$y
  list(m = out, z = z)
}

# ---- processing for display -----------------------------------------------------------------------------------------------
# The steps, their order and their parameters follow the REFLEXW guide and manual (Sandmeier) and the MALA GroundVision manual
# (v1.4): DC / dewow first, then the time zero, then a frequency filter and the gain, and clutter removal last. Every step works
# on one trace (A-scan) except background removal, which uses neighbouring traces. Nothing is written; the raw matrix is untouched.
# Sources are listed in GPR_REFERENCES below and on the GPR page.

# Block-average traces down to about max_cols columns (a stack of neighbouring traces), so a 27,000-trace line draws.
gpr_decimate <- function(m, max_cols = 2000L) {
  n <- ncol(m); k <- max(1L, ceiling(n / max_cols)); if (k == 1L) return(list(m = m, k = 1L, tr = seq_len(n)))
  g <- n %/% k; a <- array(m[, seq_len(g * k), drop = FALSE], c(nrow(m), k, g))
  list(m = colMeans(aperm(a, c(2, 1, 3))), k = k, tr = (seq_len(g) - 0.5) * k + 0.5)
}
# Running mean over a window of w samples (shrinking at the ends), per column.
gpr_runmean <- function(m, w) {
  s <- nrow(m); h <- max(1L, as.integer(w) %/% 2L); i <- seq_len(s); lo <- pmax(i - h, 1L); hi <- pmin(i + h, s)
  cs <- rbind(0, apply(m, 2, cumsum)); (cs[hi + 1L, , drop = FALSE] - cs[lo, , drop = FALSE]) / (hi - lo + 1L)
}
# Antenna centre frequency (MHz) from the header's ANTENNAS text ("100 MHz shielded", "500 MHz", "1.0 GHz"); NA when it is not given.
gpr_antenna_mhz <- function(antennas) {
  x <- suppressWarnings(as.numeric(sub("^\\s*([0-9.]+).*", "\\1", antennas))); if (is.na(x) || x <= 0) return(NA_real_)
  if (grepl("GHz", antennas, ignore.case = TRUE)) x * 1000 else x
}
# DC: the mean of the later part of each trace (from `from` of its length to the end, where no signal is left) is taken off, as the
# GroundVision DC filter does over a sample interval ending at the last sample; REFLEXW's subtract-DC-shift uses a time range.
gpr_dc <- function(m, from = 0.75) { i <- seq.int(max(1L, floor(nrow(m) * from)), nrow(m)); sweep(m, 2, colMeans(m[i, , drop = FALSE])) }
# Dewow: the running mean over `periods` periods of the antenna's centre frequency is taken off each trace (REFLEXW: about one principal
# period in the manual, one or two in the guide). Also removes a constant shift.
gpr_dewow <- function(m, periods, dt_ns, mhz) { if (!isTRUE(periods > 0) || is.na(mhz)) return(m); m - gpr_runmean(m, max(5L, round(periods * 1000 / mhz / dt_ns))) }
# Time zero: the zero crossing before the first strong extremum (above `thr` of the largest amplitude) of the mean trace, in samples;
# the guide's "correction to the first zero crossing". The data before it are dropped by the caller (REFLEXW "move starttime").
gpr_time_zero <- function(m, thr = 0.25, search = 0.4) {
  x <- rowMeans(m); n <- max(5L, floor(length(x) * search)); a <- abs(x[seq_len(n)]); hit <- which(a >= thr * max(a))[1]
  if (is.na(hit)) return(1L); z <- which(sign(x[seq_len(hit)]) != sign(x[hit])); if (length(z)) max(z) + 1L else 1L
}
# Ormsby band-pass in the frequency domain: zero below f1, rising to f2, flat to f3, falling to f4 (MHz). Gusmeroli et al. (2015) used corners of
# 0.25, 1, 2 and 4 times the antenna frequency (125-500 / 1000-2000 MHz on a 500 MHz antenna); the corners are held under the Nyquist frequency.
gpr_bandpass <- function(m, dt_ns, corners_mhz) {
  n <- nrow(m); nyq <- 500 / dt_ns; f <- pmin(corners_mhz, 0.98 * nyq); k <- c(0:(n %/% 2), rev(seq_len(ceiling(n / 2) - 1L))); fr <- k / (n * dt_ns) * 1000
  w <- pmin(pmax((fr - f[1]) / max(f[2] - f[1], 1e-9), 0), pmax((f[4] - fr) / max(f[4] - f[3], 1e-9), 0), 1)
  Re(mvfft(mvfft(m) * w, inverse = TRUE)) / n
}
# Energy decay (REFLEXW 1.11.2.2): the mean decay curve of all traces, median-smoothed, divides every trace. No parameters; true amplitudes are lost.
gpr_energy_decay <- function(m) { d <- rowMeans(abs(m)); d <- stats::runmed(d, min(2L * (length(d) %/% 20) + 1L, 101L), endrule = "median"); m / pmax(d, 1e-9) }
# Gain function (REFLEXW 1.11.2.3 and GroundVision's time-gain): g(t) = (1 + a t) exp(b t), a = a'/pulse width, b = b' v / 8.69 (b' in dB/m, v = 0.1 m/ns),
# from the start time, capped at max_gain. a' = 0 and b' = 0 leave the data as they are.
gpr_gain_fn <- function(m, dt_ns, mhz, a1, b1, start_ns = 0, max_gain = 1000) {
  t <- pmax((seq_len(nrow(m)) - 1) * dt_ns - start_ns, 0); pw <- if (is.na(mhz)) 10 else 1000 / mhz
  g <- pmin((1 + a1 / pw * t) * exp(b1 * 0.1 / 8.69 * t), max_gain); m * g
}
# AGC (REFLEXW 1.11.2.1): each sample divided by the RMS of the window around it on the same trace. True amplitudes are lost.
gpr_agc <- function(m, w) { s <- nrow(m); h <- max(1L, as.integer(w) %/% 2L); i <- seq_len(s); lo <- pmax(i - h, 1L); hi <- pmin(i + h, s)
  cs <- rbind(0, apply(m^2, 2, cumsum)); m / pmax(sqrt((cs[hi + 1L, , drop = FALSE] - cs[lo, , drop = FALSE]) / (hi - lo + 1L)), 1e-9) }
# Background removal: the mean trace of the whole line ("mean"), or of the n traces around each one ("moving"), is taken off. It removes
# the system's ringing but also every flat, continuous reflector, which is what a frost table looks like, so it is off unless chosen.
gpr_background <- function(m, mode = "none", n = 50L) {
  if (mode == "mean") return(sweep(m, 1, rowMeans(m)))
  if (mode == "moving") { h <- max(1L, as.integer(n) %/% 2L); j <- seq_len(ncol(m)); lo <- pmax(j - h, 1L); hi <- pmin(j + h, ncol(m))
    cs <- cbind(0, t(apply(m, 1, cumsum))); return(m - (cs[, hi + 1L, drop = FALSE] - cs[, lo, drop = FALSE]) / rep(hi - lo + 1L, each = nrow(m))) }
  m
}

# Presets: each is one published or manual-based processing flow, with where it comes from. Parameters match the page's controls.
GPR_PRESETS <- list(
  `Basic (REFLEXW / MALA GroundVision)` = list(dc = FALSE, dewow = 1, t0 = "auto", bp = FALSE, gain = "decay", bg = "none",
    ref = "REFLEXW manual 0.15.6.4 and 1.11.1.8: dewow over about one period, then a gain function; time zero to the first zero crossing (REFLEXW guide II.2). GroundVision manual 3.3.2: DC and time gain first, then a frequency or background filter if needed."),
  `Active layer (Gusmeroli et al. 2015)` = list(dc = FALSE, dewow = 1, t0 = "none", bp = TRUE, gain = "decay", bg = "mean",
    ref = "Gusmeroli et al. (2015), 500 MHz, thermokarst site, Alaska: dewow, background removal, Ormsby band-pass with corners 125-500 / 1000-2000 MHz (0.25, 1, 2, 4 x the antenna frequency) and a time-scaled gain; thaw depth within 5 cm of probing. Their dewow window and gain are not stated here: one period and REFLEXW's energy decay are used. Background removal can erase a flat frost table."),
  `Dewow only (Wollschlager et al. 2010)` = list(dc = FALSE, dewow = 1, t0 = "none", bp = FALSE, gain = "none", bg = "none",
    ref = "Wollschlager et al. (2010), 200 MHz multi-channel radar, frozen ground below a thawed active layer: a dewow filter only, then the time window clipped. Dewow window not stated here: one period used."),
  `Raw with DC removed` = list(dc = TRUE, dewow = 0, t0 = "none", bp = FALSE, gain = "none", bg = "none",
    ref = "GroundVision filter manager (Appendix 1.2): the DC component of each trace removed. Use this to look at the data before any other step; GroundVision advises no filter but DC when reading trace details."))

# Typical radar velocities (m/ns) for permafrost terrain, each with where it comes from. v = 0.3 / sqrt(epsilon), c = 0.3 m/ns (REFLEXW manual, Wollschlager et al. 2010).
# They are starting values: measure the velocity on site (hyperbola fit, CMP, or a probe depth to the frost table) and replace them.
GPR_GROUND <- c(`Frozen ground, permittivity 4-5 (0.14)` = 0.141, `Thawed wet sand / gravel, permittivity 10-20 (0.08)` = 0.0775,
                `Peat, thawed active layer (0.039; CMP, Alaska)` = 0.039, `Silt, thawed active layer (0.054; CMP, Alaska)` = 0.054,
                `Whole active layer, peat over silt (0.045)` = 0.045, `Air (0.30)` = 0.30)
GPR_REFERENCES <- c(
  "Gusmeroli, A., Liu, L., Schaefer, K. M., Zhang, T., Schaefer, T. and Grosse, G. (2015). Active layer stratigraphy and organic layer thickness at a thermokarst site in Arctic Alaska identified using ground penetrating radar. Arctic, Antarctic, and Alpine Research 47(2). doi:10.1657/AAAR00C-13-301. Peat 0.039, silt 0.054 and whole active layer 0.045 m/ns (CMP); 500 MHz; dewow, background removal, Ormsby band-pass, time-scaled gain; thaw depth within 5 cm of probing.",
  "Wollschlager, U., Gerhards, H., Yu, Q. and Roth, K. (2010). Multi-channel ground-penetrating radar to explore spatial variations in thaw depth and moisture content in the active layer of a permafrost site. The Cryosphere 4, 269-283. doi:10.5194/tc-4-269-2010. Permittivity of permafrost 4-5 and of wet sand / gravel 10-20 (after Sharma 1997); 0.08 m/ns for permittivity 15; 200 MHz; dewow only; a constant velocity moved the frost table 0.5 m under a vegetated patch.",
  "Moorman, B. J., Robinson, S. D. and Burgess, M. M. (2003). Imaging periglacial conditions with ground-penetrating radar. Permafrost and Periglacial Processes 14(4), 319-329. doi:10.1002/ppp.463. Review of over 100 permafrost surveys: the frozen / unfrozen interface gives a strong, laterally coherent reflection.",
  "Sandmeier, K. J. REFLEXW manual, version 10.3 (sandmeier-geo.de/Download/reflexw_manual.pdf), and 'Introduction to the processing of GPR-data within REFLEXW' (sandmeier-geo.de/Download/gpr_2d_import_processing.pdf): order of steps, dewow window, time zero, energy decay, gain function, AGC, background removal, band-pass, time-depth conversion, MALA RD3 / RD7 and .cor / .mrk import.",
  "MALA GeoScience. GroundVision Software Operating Manual v. 1.4 (2009): file types (.rd3 16-bit, .rad, .cor, .mrk), the .cor layout, DC filter, time-gain filter, subtract mean trace (removes horizontal and nearly horizontal features), sampling frequency about 10 x antenna frequency, about 500 samples.",
  "Davis, J. L. and Annan, A. P. (1989). Ground-penetrating radar for high-resolution mapping of soil and rock stratigraphy. Geophysical Prospecting 37(5), 531-551. Velocity depends on the permittivity, which depends on water and ice content.")

# The processing steps in their usual order (REFLEXW, GroundVision): DC removal, dewow, time zero, band-pass, gain, background removal.
GPR_STEPS <- c(dc = "DC removal", dewow = "Dewow", t0 = "Time zero", bp = "Band-pass", gain = "Gain", bg = "Background removal")

# Process the matrix for display. p: a list of the steps (see GPR_PRESETS and the page's controls), and p$order the order to run
# them in (names of GPR_STEPS; the usual order when missing); h: the header; returns the matrix, time zero and a log.
gpr_process <- function(m, h, p) {
  s <- nrow(m); dt <- h$timewindow_ns / s; mhz <- gpr_antenna_mhz(h$antennas); log <- character(); i0 <- 1L
  ord <- unlist(p$order) %||% names(GPR_STEPS); ord <- c(intersect(ord, names(GPR_STEPS)), setdiff(names(GPR_STEPS), ord))
  for (st in ord) switch(st,
    dc = { if (isTRUE(p$dc)) { m <- gpr_dc(m); log <- c(log, "DC removed (mean of the last quarter of each trace)") } else m <- sweep(m, 2, colMeans(m)) },
    dewow = { if (isTRUE(p$dewow > 0)) { m <- gpr_dewow(m, p$dewow, dt, mhz); log <- c(log, sprintf("dewow, window %g period(s) = %.1f ns", p$dewow, p$dewow * 1000 / (mhz %||% NA))) } },
    t0 = { if (identical(p$t0, "auto")) { i0 <- gpr_time_zero(m); log <- c(log, sprintf("time zero at the first zero crossing, %.1f ns", (i0 - 1) * dt)) }
           if (identical(p$t0, "manual")) { i0 <- max(1L, round(p$t0_ns / dt) + 1L); log <- c(log, sprintf("time zero set to %.1f ns", p$t0_ns)) }
           if (i0 > 1L) m <- m[i0:s, , drop = FALSE] },
    bp = { if (isTRUE(p$bp) && !is.na(mhz)) { cr <- mhz * c(p$bp1, p$bp2, p$bp3, p$bp4); m <- gpr_bandpass(m, dt, cr)
           log <- c(log, sprintf("Ormsby band-pass %s MHz", paste(round(pmin(cr, 0.98 * 500 / dt)), collapse = "-"))) } },
    gain = { g <- p$gain %||% "none"
      if (g == "decay") { m <- gpr_energy_decay(m); log <- c(log, "energy decay gain") }
      if (g == "fn") { m <- gpr_gain_fn(m, dt, mhz, p$ga %||% 0, p$gb %||% 0); log <- c(log, sprintf("gain function a' = %g, b' = %g dB/m", p$ga %||% 0, p$gb %||% 0)) }
      if (g == "agc") { m <- gpr_agc(m, max(3L, round((p$agc_ns %||% 25) / dt))); log <- c(log, sprintf("AGC, window %g ns", p$agc_ns %||% 25)) } },
    bg = { bg <- p$bg %||% "none"
      if (bg != "none") { m <- gpr_background(m, bg, p$bgn %||% 50L); log <- c(log, if (bg == "mean") "background removal (mean trace)" else sprintf("background removal (moving mean, %d traces)", p$bgn %||% 50L)) } })
  list(m = m, t0_ns = (i0 - 1) * dt, dt_ns = dt, log = log)
}

# ---- views ------------------------------------------------------------------------------------------------------------------
GPR_SCALES <- list(Grey = list(c(0, "#000000"), c(1, "#ffffff")), `Blue-white-red` = list(c(0, "#2c7bb6"), c(0.5, "#ffffff"), c(1, "#d7191c")))

# Depth (m) of two-way times (ns): one velocity throughout, or, with a thaw depth, the thawed layer at v_top over the frozen ground
# at v (the reflector at the thaw depth is the permafrost table, so the time to it is 2 x thaw depth / v_top).
gpr_depth <- function(t_ns, v, thaw_m = 0, v_top = v) {
  if (!isTRUE(thaw_m > 0)) return(t_ns * v / 2)
  ts <- 2 * thaw_m / v_top; ifelse(t_ns <= ts, t_ns * v_top / 2, thaw_m + (t_ns - ts) * v / 2)
}

# xaxis: "trace" or "distance" (m). vaxis: "time" (ns) or "depth" (m; see gpr_depth). clip: percentile of |amplitude| where the colours stop.
# max_depth (m, 0 = all) cuts the view; a dashed line marks the thaw depth. elev (per column of m): draw on an elevation axis (m a.s.l.), depth only.
# markers: x positions (and their numbers) of the field markers, drawn as dotted lines.
view_radargram <- function(m, dt_ns, x = NULL, xlab = "Trace", vaxis = "time", velocity = 0.1, clip = 0.99, scale = "Grey", thaw_m = 0, v_top = velocity, max_depth = 0,
                           elev = NULL, markers = NULL) {
  t <- (seq_len(nrow(m)) - 1) * dt_ns; dep <- vaxis == "depth"
  y <- if (dep) gpr_depth(t, velocity, thaw_m, v_top) else t
  zmax <- unname(quantile(abs(m), clip, na.rm = TRUE)); if (!is.finite(zmax) || zmax <= 0) zmax <- 1
  topo <- dep && !is.null(elev) && length(elev) == ncol(m)
  if (topo) { keep <- if (max_depth > 0) y <= max_depth else rep(TRUE, length(y)); g <- gpr_topo(m[keep, , drop = FALSE], y[keep], elev); m <- g$m; y <- g$z }
  ylab <- if (topo) "Elevation (m a.s.l., GPS)" else if (dep) "Depth (m)" else "Two-way time (ns)"
  p <- plotly::plot_ly(x = x, y = y, z = m, type = "heatmap", zmin = -zmax, zmax = zmax, colorscale = GPR_SCALES[[scale]] %||% GPR_SCALES$Grey, showscale = FALSE,
                       hovertemplate = paste0(xlab, " %{x:.1f}<br>", if (topo) "elevation %{y:.2f} m" else if (dep) "depth %{y:.2f} m" else "%{y:.1f} ns", "<extra></extra>")) |>
    plotly_layout(xlab, ylab, yrev = !topo) |>
    plotly::layout(yaxis = list(autorange = if (topo) TRUE else NULL, range = if (topo) NULL else c(if (dep && max_depth > 0) min(max_depth, max(y)) else max(y), 0), title = ylab),
                   margin = list(l = 60, r = 20, t = 10, b = 50))
  sh <- list(); an <- list()                                               # one layout() call at the end: a second one would replace these
  if (dep && isTRUE(thaw_m > 0)) {
    if (topo) p <- plotly::add_lines(p, x = x, y = elev - thaw_m, inherit = FALSE, line = list(color = "#E6C384", width = 1.5, dash = "dash"), name = "thaw depth", hoverinfo = "skip", showlegend = FALSE)
    else { sh <- list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = thaw_m, y1 = thaw_m, line = list(color = "#E6C384", width = 1.5, dash = "dash")))
           an <- list(list(xref = "paper", x = 0.995, xanchor = "right", y = thaw_m, yanchor = "bottom", text = sprintf("thaw depth %.2f m", thaw_m), showarrow = FALSE, font = list(color = "#E6C384", size = 12))) } }
  if (!is.null(markers) && nrow(markers)) {
    sh <- c(sh, lapply(markers$x, function(v) list(type = "line", xref = "x", yref = "paper", x0 = v, x1 = v, y0 = 0, y1 = 1, line = list(color = "#FFA066", width = 1, dash = "dot"))))
    an <- c(an, lapply(seq_len(nrow(markers)), function(i) list(x = markers$x[i], xref = "x", yref = "paper", y = 1, yanchor = "bottom", text = markers$marker[i], showarrow = FALSE, font = list(color = "#FFA066", size = 10)))) }
  if (length(sh)) p <- plotly::layout(p, shapes = sh, annotations = an)
  p
}

# The GPS tracks of the lines that have a .cor, the picked line in the accent colour. tracks: named list of read_cor() tables.
gpr_map <- function(tracks, selected = "", basemap = "satellite", markers = NULL) {
  g <- plotly::plot_ly(); ll <- list()
  for (k in c(setdiff(names(tracks), selected), intersect(names(tracks), selected))) { d <- tracks[[k]]; if (is.null(d) || nrow(d) < 1L) next; sel <- k == selected      # the picked line is drawn last, on top
    g <- plotly::add_trace(g, type = "scattermapbox", mode = if (nrow(d) > 1L) "lines" else "markers", lat = d$lat, lon = d$lon, hoverinfo = "text", name = k, showlegend = FALSE,
                           text = sprintf("<b>%s</b><br>trace %d<br>%s", k, d$trace, format(d$datetime, "%Y-%m-%d %H:%M:%S")), customdata = paste(k, d$trace, sep = "|"),   # line|trace, for a click or the shared cursor
                           line = list(width = if (sel) 6 else 2, color = if (sel) "#E6C384" else "#7E9CD8"))
    ll[[k]] <- d }
  if (!is.null(markers) && nrow(markers)) g <- plotly::add_trace(g, type = "scattermapbox", mode = "markers+text", lat = markers$lat, lon = markers$lon, text = markers$marker, textposition = "top right",
    hoverinfo = "text", hovertext = sprintf("marker %d, trace %d", markers$marker, markers$trace), marker = list(size = 9, color = "#FFA066"), textfont = list(color = "#ffffff"), showlegend = FALSE)
  sel <- if (selected %in% names(ll)) ll[[selected]] else rbindlist(ll)
  none <- !nrow(sel); if (none) sel <- data.table(lat = 0, lon = 0)                    # nothing placed: the whole world
  span <- max(diff(range(sel$lat)), diff(range(sel$lon)) / 2, 1e-4); zoom <- max(3, min(15, floor(log2(360 / span)) - 2))   # Esri imagery stops near zoom 17 in the north; mapbox fetches 256 px tiles one or two levels finer (more on a Retina screen)
  if (none) zoom <- 1
  if (!length(ll)) g <- plotly::add_trace(g, type = "scattermapbox", mode = "markers", lat = sel$lat[1], lon = sel$lon[1], hoverinfo = "none", marker = list(size = 1, opacity = 0), showlegend = FALSE)
  b <- BASEMAPS[[basemap]] %||% BASEMAPS$satellite
  plotly::layout(g, mapbox = list(style = b$style, layers = b$layers, center = list(lat = mean(range(sel$lat)), lon = mean(range(sel$lon))), zoom = zoom),
                 margin = list(l = 0, r = 0, t = 0, b = 0), paper_bgcolor = PAL$bg1, font = list(color = PAL$fg)) |> plotly::config(displaylogo = FALSE)
}
