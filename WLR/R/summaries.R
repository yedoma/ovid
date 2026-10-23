# R/summaries.R  --  Monthly and water-year summaries, and manual measurements set against the logger.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

SUMMARY_PARAMS <- c("depth_m", "elev_m", "dtw_m", "wt_ground_m", "temp_c", "spc_uscm")

# Daily means of one deployment's values (a day counts when it holds min_complete of its usual records).
daily_of <- function(d, min_complete = 0.8) {
  pp <- intersect(SUMMARY_PARAMS, names(d)); if (!nrow(d) || !length(pp)) return(NULL)
  step <- stats::median(diff(as.numeric(d$TIMESTAMP))); per_day <- if (is.finite(step) && step > 0) 86400 / step else 48
  long <- melt(d[, c("site", "TIMESTAMP", pp), with = FALSE], id.vars = c("site", "TIMESTAMP"), variable.name = "parameter", variable.factor = FALSE, na.rm = TRUE)
  if (!nrow(long)) return(NULL)
  long[, .(mean = mean(value), min = min(value), max = max(value), n = .N), by = .(site, date = as.Date(TIMESTAMP), parameter)][n >= min_complete * per_day][, n := NULL][]
}

# Months and water years (from water_year_start, 1 October by default; see water_year_of() in baro.R) from daily means.
# A month or year is complete when it holds year_complete of its days.
# sig: the uncertainty of each deployment (deployment_uncertainty()); a month or year takes the largest of its site's
# deployments that season, as sigma_m (the errors are systematic, so a mean keeps them).
SIGMA_OF <- c(depth_m = "depth_sigma_m", dtw_m = "dtw_sigma_m", wt_ground_m = "dtw_sigma_m", elev_m = "elev_sigma_m")
summaries_of <- function(daily, year_complete = 0.9, sig = NULL, water_year_start = WATER_YEAR_START) {
  if (is.null(daily) || !nrow(daily)) return(list(monthly = data.table(), water_year = data.table()))
  dly <- daily[, .(mean = mean(mean), min = min(min), max = max(max)), by = .(site, date, parameter)]      # two loggers on one day are pooled
  mon <- dly[, .(mean = round(mean(mean), 3), min = round(min(min), 3), max = round(max(max), 3), days = .N), by = .(site, parameter, year = year(date), month = month(date))]
  mon[, days_in_month := as.integer(format(as.Date(sprintf("%d-%02d-01", year + (month == 12), month %% 12 + 1)) - 1, "%d"))]
  mon[, complete := days >= year_complete * days_in_month]
  dly[, season := water_year_of(date, water_year_start)]
  wy <- dly[, .(mean = round(mean(mean), 3), min = round(min(min), 3), date_min = date[which.min(min)], max = round(max(max), 3), date_max = date[which.max(max)],
                range = round(max(max) - min(min), 3), days = .N), by = .(site, parameter, water_year = season)]
  wy[, complete := days >= year_complete * 365]
  if (!is.null(sig) && nrow(sig)) {
    s <- rbindlist(lapply(names(SIGMA_OF), function(p) if (SIGMA_OF[[p]] %in% names(sig)) sig[, .(site, water_year = year, parameter = p, sigma_m = get(SIGMA_OF[[p]]))]))
    s <- s[is.finite(sigma_m), .(sigma_m = max(sigma_m)), by = .(site, water_year, parameter)]
    if (nrow(s)) { wy <- merge(wy, s, by = c("site", "water_year", "parameter"), all.x = TRUE)
      mon[, water_year := water_year_of(as.Date(sprintf("%d-%02d-15", year, month)), water_year_start)]   # sig is per deployment year: matched by year number
      mon <- merge(mon, s, by = c("site", "water_year", "parameter"), all.x = TRUE)[, water_year := NULL] } }
  list(monthly = mon[order(site, parameter, year, month)], water_year = wy[order(site, parameter, water_year)])
}

# ---- the change over the last days, from settled readings ------------------------------------------------------------
# The change of a level over the last `days` before its latest reading, for the Site page. A logger settles for a while after
# it goes into the well, so readings within `window_h` of a deployment's start are not used as the start of the change (the
# same window the service-visit check flags after a download). Returns the indices of the first and last readings and
# whether the record covers the whole span (else the change is since the first settled reading); NULL with fewer than two.
settled_change <- function(t, v, starts = numeric(), window_h = 2, days = 60) {
  k <- which(!is.na(v) & !is.na(t)); if (length(k) < 2) return(NULL)
  tn <- as.numeric(t); last <- k[which.max(tn[k])]; from <- tn[last] - days * 86400
  st <- as.numeric(starts); st <- st[!is.na(st)]
  settling <- vapply(tn[k], function(x) any(x >= st & x < st + window_h * 3600), NA)
  ok <- k[!settling & tn[k] >= from & k != last]; if (!length(ok)) return(NULL)
  first <- ok[which.min(tn[ok])]
  list(first = first, last = last, full = min(tn[k]) <= from) }

# ---- manual measurements -------------------------------------------------------------------------------------------
# Readings taken by hand (a tape, a handheld thermometer or conductivity meter, a survey of the water surface). They are
# reference points only: shown beside the logger with the difference, never used to change the data.
MEAS_COLS <- c("site", "time", "parameter", "value", "method", "author", "notes")
MEAS_PARAMS <- c("depth_m", "dtw_m", "elev_m", "temp_c", "spc_uscm")

# The logger's value at each measurement's time (straight line between the two nearest records, at most max_gap_h away).
compare_measurements <- function(meas, d, serial, max_gap_h = 1) {
  if (is.null(meas) || !nrow(meas) || !nrow(d)) return(NULL)
  m <- meas[site == d$site[1] & !is.na(t) & t >= min(d$TIMESTAMP) - max_gap_h * 3600 & t <= max(d$TIMESTAMP) + max_gap_h * 3600 & parameter %in% names(d)]
  if (!nrow(m)) return(NULL)
  m[, logger_value := mapply(function(p, tt) { x <- d[!is.na(d[[p]]), c("TIMESTAMP", p), with = FALSE]; if (!nrow(x)) return(NA_real_)
    i <- findInterval(as.numeric(tt), as.numeric(x$TIMESTAMP)); a <- max(i, 1L); b <- min(i + 1L, nrow(x))
    ta <- as.numeric(x$TIMESTAMP[a]); tb <- as.numeric(x$TIMESTAMP[b]); s <- as.numeric(tt)
    if (min(abs(s - ta), abs(s - tb)) > max_gap_h * 3600) return(NA_real_)
    if (a == b || tb == ta) return(x[[p]][a]); x[[p]][a] + (x[[p]][b] - x[[p]][a]) * (s - ta) / (tb - ta) }, parameter, t)]
  m[, `:=`(deployment = if ("deployment" %in% names(d)) d$deployment[1] else NA_character_, serial = serial, difference = round(logger_value - value, 3))]
  m[, .(site, deployment, serial, time = t, parameter, manual = value, logger = round(logger_value, 3), difference, method, author, notes)]
}

read_measurements <- function(tab) {
  if (is.null(tab) || !nrow(tab)) return(data.table())
  m <- copy(as.data.table(tab)); m[, `:=`(t = parse_date_cell(time), value = suppressWarnings(as.numeric(value)))]
  m[!is.na(t) & !is.na(value) & parameter %in% MEAS_PARAMS]
}
