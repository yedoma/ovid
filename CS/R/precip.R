# R/precip.R  --  Weighing precipitation gauges: OTT Pluvio2 and Geonor T-200B.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

#  A weighing gauge reports how much is in its bucket, not how much fell. This
#  turns the bucket record into precipitation per interval and per day.
#
#  Both gauges
#    1. The station record is put on a regular grid; missing times stay blank.
#    2. Servicing is found on the bucket content: a drop larger than
#       service_step_mm is an emptying, a single-interval rise larger than that
#       is a top-up. Each starts a new bucket cycle and is never counted.
#    3. "Bucket" series: running maximum of the bucket content within each cycle,
#       differenced. It ignores evaporation and oscillation of the weight.
#    4. After an outage longer than long_gap_hours the first value is not used as
#       an interval amount; the bucket gain across the outage is reported apart.
#  Pluvio2 only (it also logs its own filtered running total, acc_total)
#    5. acc_total is differenced (its 500 mm rollover and power resets handled).
#    6. Days where acc_total reports far more than the bucket gained are suspect;
#       runs of them are faulty periods. Final series = acc_total in healthy
#       periods, bucket daily totals (low confidence) in faulty periods.
#  Geonor only (bucket content is all there is, and the vibrating wire is noisy)
#    5. The bucket content is smoothed with a running median (smooth_records) and
#       precipitation is booked only in steps of noise_mm, so wind pumping and
#       the daily temperature wobble are not counted as precipitation.
#
#  Set up in config/precip_gauges.csv (one row per gauge; found automatically
#  when the columns are recognised). Output in output/precip/.
PRECIP_COLS <- c("station", "stream", "gauge_type", "bucket_col", "acc_col", "status_col", "interval_min",
                 "service_step_mm", "acc_rollover_mm", "acc_rollover_min", "long_gap_hours", "mismatch_mm",
                 "bridge_days", "min_fault_days", "noise_mm", "smooth_records", "max_bucket_mm", "spike_mm", "jump_mm", "final_source", "bucket_only", "bucket_only_start", "bucket_only_end", "manual_servicing",
                 "undercatch", "wind_station", "wind_stream", "wind_col", "wind_height_m", "temp_col", "ce_a", "ce_b", "ce_c", "ce_u_max", "enabled", "notes")

# Bucket-only window: for one Pluvio whose running total (acc_total) went wrong after a field service, the bucket weight
# is the only precipitation signal between bucket_only_start and bucket_only_end. Outside that window the gauge is processed as usual.
# There are no default dates: a project sets the window for its own gauge.
GAUGE_DEFAULTS <- list(
  pluvio2 = c(bucket_col = "bucket_nr", acc_col = "acc_total", status_col = "Result9", interval_min = "", service_step_mm = "50",
              acc_rollover_mm = "500", acc_rollover_min = "450", long_gap_hours = "6", mismatch_mm = "2", bridge_days = "3",
              min_fault_days = "3", noise_mm = "0", smooth_records = "1", max_bucket_mm = "1500", spike_mm = "50", jump_mm = "10"),
  geonor  = c(bucket_col = "GN_mm_AVG", acc_col = "", status_col = "", interval_min = "", service_step_mm = "50",
              acc_rollover_mm = "", acc_rollover_min = "", long_gap_hours = "6", mismatch_mm = "", bridge_days = "",
              min_fault_days = "", noise_mm = "0.2", smooth_records = "5", max_bucket_mm = "1500", spike_mm = "20", jump_mm = ""))

# Wind undercatch: collection efficiency CE = exp(-a * U * (1 - atan(b * T) + c)), U capped at u_max, the form of
# Kochendorfer et al. (2017, HESS 21, 3525-3542) for 30-minute data. The coefficients are Table 2 of that paper (Eq. 3), checked
# against it. The set depends on where the wind was measured: at gauge height (the default) or at 10 m (set wind_height_m to 5 or
# more on the gauge). Each gauge can override them. The paper gives 0.2 (in units of CE) as a representative uncertainty of every
# transfer function, so each corrected value also has a low and a high limit, from CE + 0.2 and CE - 0.2.
UNDERCATCH <- list(single_alter     = c(a = 0.0348, b = 1.366, c = 0.779, u_max = 7.2),
                   unshielded       = c(a = 0.0785, b = 0.729, c = 0.407, u_max = 7.2),
                   single_alter_10m = c(a = 0.0281, b = 1.628, c = 0.837, u_max = 9),
                   unshielded_10m   = c(a = 0.0623, b = 0.776, c = 0.431, u_max = 9))
CE_SIGMA <- 0.2
# Smith (2007), for an Alter-shielded Geonor T-200B with wind at gauge height, as applied by Pan et al. (2016, The
# Cryosphere 10, 2347-2360, eq. 1): snow CE = 1.18 exp(-0.18 U), 1 below 1.2 m/s and 0.23 above 9 m/s; rain CE 0.95 (Devine
# and Mekis 2008, in Pan et al. 2016). Mixed precipitation is split by the rain fraction below.
smith2007_ce <- function(u, fr) {
  snow <- ifelse(u < 1.2, 1, ifelse(u > 9, 0.23, 1.18 * exp(-0.18 * u)))
  1 / (fr / 0.95 + (1 - fr) / snow)
}

# Precipitation phase. The hydrometeor temperature Ti, the temperature of a falling drop or flake, from air temperature and
# relative humidity by the psychrometric energy balance of Harder and Pomeroy (2013, Hydrol. Process. 27, 1901-1914), with the
# constants of Table 2 in Pan et al. (2016); solved by bisection between Ta - 40 and Ta. The rain fraction is
# 1 / (1 + b c^Ti), b 2.630006 and c 0.09336 (Harder and Pomeroy 2013, as used by Pan et al. 2016).
hydrometeor_temp <- function(ta, rh) {
  e_kpa <- function(t, h) h / 100 * 0.611 * exp(17.3 * t / (237.3 + t))
  rho <- function(t, h) e_kpa(t, h) * 1000 * 0.01801528 / (8.31441 * (t + 273.15)) # water vapour density, kg/m3
  d <- 2.06e-5 * ((ta + 273.15) / 273.15)^1.75 # diffusivity of water vapour in air
  lam <- 0.000063 * (ta + 273.15) + 0.00673 # thermal conductivity of air
  l <- ifelse(ta < 0, 1000 * (2834.1 - 0.29 * ta - 0.004 * ta^2), 1000 * (2501 - 2.36 * ta)) # latent heat
  rh <- pmin(pmax(rh, 0), 100)
  f <- function(ti) ti - ta - d / lam * l * (rho(ta, rh) - rho(ti, 100))
  lo <- ta - 40
  hi <- ta
  for (k in 1:60) {
    m <- (lo + hi) / 2
    up <- f(m) > 0
    hi <- ifelse(up, m, hi)
    lo <- ifelse(up, lo, m)
  }
  (lo + hi) / 2
}
rain_fraction <- function(ta, rh) 1 / (1 + 2.630006 * 0.09336^hydrometeor_temp(ta, rh))
# Snow, Mixed or Rain for the tables and plots: the rain fraction below 0.05 is snow, above 0.95 rain (Ovid's bins).
# Without humidity, air temperature alone: snow below -2 C, rain above 2 C (an ovid default, no published source).
precip_phase <- function(ta, rh = NULL) {
  if (is.null(rh)) return(ifelse(is.na(ta), NA_character_, ifelse(ta < -2, "Snow", ifelse(ta > 2, "Rain", "Mixed"))))
  fr <- rain_fraction(ta, rh)
  ifelse(is.na(fr), NA_character_, ifelse(fr < 0.05, "Snow", ifelse(fr > 0.95, "Rain", "Mixed")))
}

# Which station tables look like a weighing gauge, judged by their column names.
detect_gauges <- function(pstats) {
  if (is.null(pstats) || !nrow(pstats)) return(NULL)
  out <- list()
  for (k in split(pstats, by = c("station", "stream"))) {
    p <- k$parameter
    if (all(c("acc_total", "bucket_nr") %in% p))
      out[[length(out) + 1L]] <- c(list(station = k$station[1], stream = k$stream[1], gauge_type = "pluvio2"), as.list(GAUGE_DEFAULTS$pluvio2))
    else { b <- grep("^GN_mm|geonor.*(mm|depth)|^T200B|^Geonor_?(mm|Depth)", p, ignore.case = TRUE, value = TRUE)
      if (length(b)) { d <- GAUGE_DEFAULTS$geonor; d["bucket_col"] <- b[1]
        out[[length(out) + 1L]] <- c(list(station = k$station[1], stream = k$stream[1], gauge_type = "geonor"), as.list(d)) } }
  }
  if (length(out)) rbindlist(out, fill = TRUE) else NULL
}

cummax_na <- function(x) { y <- x; y[is.na(y)] <- -Inf; z <- cummax(y); z[is.infinite(z)] <- NA_real_; z }

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

hours_since_prev <- function(time_num, valid) {
  prev_t <- shift(nafill(ifelse(valid, time_num, NA_real_), type = "locf"))
  ifelse(valid, (time_num - prev_t) / 3600, NA_real_)
}

process_gauge <- function(d, g) {
  g <- as.list(g); type <- tolower(g$gauge_type %||% "pluvio2")
  def <- GAUGE_DEFAULTS[[if (type %in% names(GAUGE_DEFAULTS)) type else "pluvio2"]]
  num <- function(k) { v <- suppressWarnings(as.numeric(g[[k]])); if (!length(v) || is.na(v)) v <- suppressWarnings(as.numeric(def[[k]])); v }
  bcol <- g$bucket_col; acol <- g$acc_col %||% ""; scol <- g$status_col %||% ""
  if (!nzchar(bcol) || !bcol %in% names(d)) stop("bucket column '", bcol, "' is not in this station table")
  has_acc <- nzchar(acol) && acol %in% names(d); has_status <- nzchar(scol) && scol %in% names(d)
  if (nzchar(acol) && !has_acc) stop("running-total column '", acol, "' is not in this station table")
  d <- d[!is.na(TIMESTAMP)][!duplicated(TIMESTAMP)]; setorder(d, TIMESTAMP)
  if (nrow(d) < 3) stop("fewer than 3 records")
  interval_min <- num("interval_min")
  if (is.na(interval_min) || interval_min <= 0) interval_min <- round(stats::median(diff(as.numeric(d$TIMESTAMP))) / 60)
  service_step_mm <- num("service_step_mm"); long_gap_hours <- num("long_gap_hours")
  noise_mm <- num("noise_mm"); if (is.na(noise_mm)) noise_mm <- 0
  smooth_n <- num("smooth_records"); if (is.na(smooth_n) || smooth_n < 1) smooth_n <- 1
  man_txt <- trim(strsplit(g$manual_servicing %||% "", ";", fixed = TRUE)[[1]]); man_txt <- man_txt[nzchar(man_txt)]
  manual <- parse_time(man_txt); man_day <- nchar(man_txt) <= 10; man_day <- man_day[!is.na(manual)]; manual <- manual[!is.na(manual)]

  raw <- data.table(TIMESTAMP = d$TIMESTAMP, bucket_raw = suppressWarnings(as.numeric(d[[bcol]])),
                    acc_raw = if (has_acc) suppressWarnings(as.numeric(d[[acol]])) else NA_real_,
                    status = if (has_status) suppressWarnings(as.numeric(d[[scol]])) else 0, .in = TRUE,
                    source_file = if ("source_file" %in% names(d)) basename(d$source_file) else NA_character_)
  off_grid <- as.numeric(raw$TIMESTAMP) %% (interval_min * 60) != 0
  n_off <- sum(off_grid); raw <- raw[!off_grid]
  if (nrow(raw) < 3) stop("no records on the ", interval_min, "-minute grid")
  grid <- data.table(TIMESTAMP = seq(min(raw$TIMESTAMP), max(raw$TIMESTAMP), by = interval_min * 60))
  dt <- raw[grid, on = "TIMESTAMP"]; setorder(dt, TIMESTAMP)
  dt[, `:=`(time_num = as.numeric(TIMESTAMP), logged = !is.na(.in))]
  dt[, status_bad := logged & !is.na(status) & status != 0]
  dt[, bucket := ifelse(logged & !status_bad, bucket_raw, NA_real_)]
  # readings that cannot be a bucket content: overflow codes (6999, 99999), negative, beyond the capacity
  max_b <- num("max_bucket_mm"); if (is.na(max_b)) max_b <- 1500
  dt[, bad_range := !is.na(bucket) & (bucket < -5 | bucket > max_b)]
  dt[bad_range == TRUE, bucket := NA_real_]
  # short spikes: one or two records far from the 5-record median around them (a real servicing step survives this)
  spike_mm <- num("spike_mm"); dt[, bad_spike := FALSE]
  if (!is.na(spike_mm) && spike_mm > 0) { vi <- which(!is.na(dt$bucket))
    if (length(vi) >= 5) { v <- dt$bucket[vi]; m <- frollapply(v, 5, stats::median, align = "center")
      sp <- !is.na(m) & abs(v - m) > spike_mm
      if (any(sp)) { set(dt, vi[sp], "bad_spike", TRUE); set(dt, vi[sp], "bucket", NA_real_) } } }
  dt[, acc := ifelse(logged, acc_raw, NA_real_)]

  # --- servicing and bucket cycles ---
  dt[, bucket_valid := !is.na(bucket)]
  dt[, gap_h_bucket := hours_since_prev(time_num, bucket_valid)]
  dt[, gap_h_acc := hours_since_prev(time_num, logged)]
  # the running total can be missing while records are still logged; that is an outage as far as the running total goes
  dt[, gap_h_accv := if (has_acc) hours_since_prev(time_num, !is.na(acc)) else gap_h_acc]
  dt[, prev_bucket := shift(nafill(bucket, type = "locf"))]
  dt[, d_bucket := bucket - prev_bucket]
  dt[, is_empty := !is.na(d_bucket) & d_bucket < -service_step_mm]
  # servicing entered by hand (gauge set-up or field log): a time marks the first record at or after it,
  # a date alone marks the largest bucket step of that day
  dt[, is_manual := FALSE]
  for (m in seq_along(manual)) {
    i <- if (man_day[m]) { w <- which(as.Date(dt$TIMESTAMP) == as.Date(manual[m]) & !is.na(dt$d_bucket)); if (length(w)) w[which.max(abs(dt$d_bucket[w]))] else integer() }
         else which(dt$TIMESTAMP >= manual[m] & !is.na(dt$bucket))[1]
    if (length(i) && !is.na(i)) set(dt, i, "is_manual", TRUE)
  }
  dt[, cycle0 := cumsum(is_empty | is_manual)]
  dt[, filled0 := cummax_na(bucket), by = cycle0]
  dt[, step0 := filled0 - shift(filled0), by = cycle0]
  dt[, is_topup := !is.na(step0) & step0 > service_step_mm & !is.na(gap_h_bucket) & gap_h_bucket <= long_gap_hours]
  dt[, bucket_cycle := cumsum(is_empty | is_manual | is_topup)]
  # Geonor: smooth within each cycle, then book precipitation only in steps of noise_mm
  dt[, bucket_use := bucket]
  if (smooth_n > 1) dt[, bucket_use := { m <- frollapply(bucket, smooth_n, stats::median, na.rm = TRUE, align = "center")
                                         ifelse(is.na(bucket), NA_real_, ifelse(is.na(m), bucket, m)) }, by = bucket_cycle]
  dt[, bucket_filled := cummax_na(bucket_use), by = bucket_cycle]
  if (noise_mm > 0) dt[, bucket_filled := { b0 <- bucket_filled[!is.na(bucket_filled)][1]
                                            if (is.na(b0)) bucket_filled else b0 + floor((bucket_filled - b0) / noise_mm + 1e-9) * noise_mm }, by = bucket_cycle]

  # --- candidate series ---
  dt[, p_bucket := bucket_filled - shift(bucket_filled), by = bucket_cycle]
  dt[, p_bucket := ifelse(!bucket_valid, NA_real_, ifelse(is.na(p_bucket), 0, p_bucket))]
  if (has_acc) {
    roll_mm <- num("acc_rollover_mm"); roll_min <- num("acc_rollover_min")
    dt[, prev_acc := shift(nafill(acc, type = "locf"))]
    dt[, d_acc := acc - prev_acc]
    dt[, p_acc := ifelse(is.na(acc), NA_real_, ifelse(is.na(prev_acc), 0, ifelse(d_acc >= 0, d_acc,
                  ifelse(prev_acc >= roll_min, (roll_mm - prev_acc) + acc, acc))))]
    dt[, power_reset := !is.na(d_acc) & d_acc < 0 & prev_acc < roll_min]
  } else dt[, `:=`(prev_acc = NA_real_, d_acc = NA_real_, p_acc = NA_real_, power_reset = FALSE)]
  dt[, long_gap_bucket := !is.na(gap_h_bucket) & gap_h_bucket > long_gap_hours]
  dt[, long_gap_acc := !is.na(gap_h_accv) & gap_h_accv > long_gap_hours]
  dt[, short_gap := !is.na(gap_h_acc) & gap_h_acc > (interval_min / 60) + 1e-6 & !long_gap_acc]
  dt[, gap_accum_mm := ifelse(long_gap_bucket, p_bucket, NA_real_)]
  dt[long_gap_bucket == TRUE, p_bucket := NA_real_]
  dt[long_gap_acc == TRUE, p_acc := NA_real_]

  # --- daily totals and faulty periods ---
  dt[, Date := as.Date(TIMESTAMP)]
  per_day <- 24 * 60 / interval_min
  daily <- dt[, .(precip_acc_total_mm = if (has_acc) sum(p_acc, na.rm = TRUE) else NA_real_,
                  precip_bucket_mm = sum(p_bucket, na.rm = TRUE), gap_accum_mm = sum_or_na(gap_accum_mm),
                  n_records = sum(logged)), by = Date]
  setorder(daily, Date)
  daily[, `:=`(mismatch = FALSE, fault = FALSE)]
  fault_periods <- data.table(Start = as.Date(character(0)), End = as.Date(character(0)))
  if (has_acc) {
    daily[, mismatch := (precip_acc_total_mm - precip_bucket_mm) > num("mismatch_mm")]
    idx <- which(daily$mismatch)
    if (length(idx)) {
      run_id <- cumsum(c(1, diff(idx) > (num("bridge_days") + 1)))
      for (r in unique(run_id)) { i <- idx[run_id == r]
        if (length(i) >= num("min_fault_days")) { daily[min(i):max(i), fault := TRUE]
          fault_periods <- rbind(fault_periods, data.table(Start = daily$Date[min(i)], End = daily$Date[max(i)])) } }
    }
    # Single days: emptying or refilling the bucket, a power loss or an outage can make the running total jump
    # while the bucket gains nothing. On a mismatch day with such an event that day or the day either side, and on
    # any day where the running total exceeds the bucket by more than jump_mm, the bucket is used instead.
    de <- dt[, .(svc = any(is_empty | is_topup | is_manual), pwr = any(power_reset | long_gap_acc | long_gap_bucket | status_bad | bad_range | bad_spike)), by = Date]
    daily[de, on = "Date", `:=`(svc = i.svc, pwr = i.pwr)]
    near <- function(x) x | shift(x, fill = FALSE) | shift(x, type = "lead", fill = FALSE)
    jump_mm <- num("jump_mm"); if (is.na(jump_mm)) jump_mm <- Inf
    daily[, day_fix := fifelse(fault | !mismatch, "", fifelse(near(svc), "servicing_day", fifelse(near(pwr), "power_or_outage_day",
                       fifelse(precip_acc_total_mm - precip_bucket_mm > jump_mm, "acc_jump", ""))))]
    # Bucket-only window: inside its window the bucket is used for every record and day, whatever the running total says
    bucket_only_on <- tolower(trim(g$bucket_only %||% "")) %in% c("yes", "y", "true", "1", "on"); bucket_only_win <- NULL
    if (bucket_only_on) {
      gs <- parse_time(trim(g$bucket_only_start %||% ""))
      ge <- trim(g$bucket_only_end %||% "")
      ge_t <- parse_time(ge); if (!is.na(ge_t) && nchar(ge) <= 10) ge_t <- ge_t + 86399
      if (is.na(gs) || is.na(ge_t) || gs > ge_t) stop("Bucket-only window: the start and end must be dates (YYYY-MM-DD, with a time if wanted), start before end")
      bucket_only_win <- c(gs, ge_t); gd <- daily$Date >= as.Date(gs) & daily$Date <= as.Date(ge_t) & daily$n_records > 0      # days with no records stay missing
      daily[gd, `:=`(fault = FALSE, day_fix = "bucket_only_window")]
      i <- which(daily$fault); fault_periods <- if (!length(i)) fault_periods[0] else { r <- cumsum(c(1, diff(i) != 1)); data.table(Start = daily$Date[i[!duplicated(r)]], End = daily$Date[i[!duplicated(r, fromLast = TRUE)]]) } }
    daily[, use_bucket := fault | nzchar(day_fix)]
    daily[, precip_mm := ifelse(use_bucket, precip_bucket_mm, ifelse(n_records == 0, NA_real_, precip_acc_total_mm))]
    daily[, source := ifelse(use_bucket, "bucket", ifelse(n_records == 0, "none", "acc_total"))]
    daily[, qc_flag := ifelse(fault, "low_confidence", ifelse(nzchar(day_fix), day_fix, ifelse(n_records == 0, "missing",
                       ifelse(mismatch, "acc_bucket_mismatch", ifelse(n_records < per_day, "partial_day", "good")))))]
  } else {
    bucket_only_on <- FALSE; bucket_only_win <- NULL
    daily[, day_fix := ""]
    daily[, precip_mm := ifelse(n_records == 0, NA_real_, precip_bucket_mm)]
    daily[, source := ifelse(n_records == 0, "none", "bucket")]
    daily[, qc_flag := ifelse(n_records == 0, "missing", ifelse(n_records < per_day, "partial_day", "good"))]
  }
  # final_source: blank = the running total where it is healthy, the bucket otherwise; "bucket" = bucket cummax throughout
  fsrc <- tolower(trim(g$final_source %||% "")); bucket_only <- has_acc && fsrc %in% c("bucket", "bucket_cummax", "cummax")
  if (bucket_only) {
    daily[, precip_mm := ifelse(n_records == 0, NA_real_, precip_bucket_mm)]; daily[, source := ifelse(n_records == 0, "none", "bucket")]
    daily[, qc_flag := ifelse(n_records == 0, "missing", ifelse(n_records < per_day, "partial_day", "good"))]; daily[, day_fix := ""]
  }
  dt[daily, on = "Date", `:=`(fault_day = i.fault & !bucket_only, mismatch_day = i.mismatch & !bucket_only, day_fix = i.day_fix)]
  has_acc_final <- has_acc && !bucket_only
  if (has_acc_final) {
    dt[, precip_mm := ifelse(fault_day | !logged, NA_real_, ifelse(nzchar(day_fix), p_bucket, p_acc))]
    dt[, source := ifelse(fault_day | !logged | is.na(precip_mm), "none", ifelse(nzchar(day_fix), "bucket", "acc_total"))]
  } else {
    dt[, precip_mm := ifelse(!logged, NA_real_, p_bucket)]
    dt[, source := ifelse(!logged | is.na(p_bucket), "none", "bucket")]
  }
  dt[, qc_flag := ifelse(fault_day, "low_confidence_period", ifelse(!logged, "missing", ifelse(nzchar(day_fix), day_fix,
                  ifelse(if (has_acc_final) long_gap_acc else long_gap_bucket, "after_outage", ifelse(status_bad, "status_error", ifelse(bad_range | bad_spike, "bucket_reading_rejected",
                  ifelse(short_gap, "after_short_gap", ifelse(mismatch_day, "acc_bucket_mismatch", "good"))))))))]

  # --- tables ---
  r3 <- function(x) round(x, 3); ft <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")
  # phase from air temperature and, where the table has it, relative humidity (precip_phase above)
  tcol <- intersect(c(trim(g$temp_col %||% ""), "pluvio_temp", "temp", "air_temp", "AirTC_Avg"), names(d))[1]
  hcol <- intersect(c("rh", "RH", "RH_Avg"), names(d))[1]
  dt[, precip_class := NA_character_]
  if (!is.na(tcol) && is.numeric(d[[tcol]])) {
    ta <- d[[tcol]][match(dt$TIMESTAMP, d$TIMESTAMP)]
    rh <- if (!is.na(hcol) && is.numeric(d[[hcol]])) d[[hcol]][match(dt$TIMESTAMP, d$TIMESTAMP)] else NULL
    ph <- precip_phase(ta, rh)
    if (!is.null(rh)) ph[is.na(rh)] <- precip_phase(ta[is.na(rh)]) # a record with no humidity: air temperature alone
    dt[, precip_class := ifelse(is.na(precip_mm) | precip_mm <= 0, NA_character_, ph)]
  }
  out_iv <- dt[, .(TIMESTAMP, precip_mm = r3(precip_mm), source, qc_flag, precip_class, precip_acc_total_mm = r3(p_acc), precip_bucket_mm = r3(p_bucket),
                   acc_total = acc_raw, bucket = bucket_raw, bucket_filled = r3(bucket_filled), bucket_cycle, source_file)]
  out_daily <- daily[, .(Date, precip_mm = r3(precip_mm), source, qc_flag, n_records, precip_acc_total_mm = r3(precip_acc_total_mm),
                         precip_bucket_mm = r3(precip_bucket_mm), gap_accum_mm = r3(gap_accum_mm))]
  ev <- function(sel, type, detail) if (!any(sel)) NULL else
    data.table(Start = fmt_ts(dt$TIMESTAMP[sel]), End = fmt_ts(dt$TIMESTAMP[sel]), Event = type, Detail = detail[sel])
  events <- rbindlist(list(
    ev(dt$is_empty, "bucket_emptied", paste0(round(dt$d_bucket, 1), " mm step")),
    ev(dt$is_topup, "bucket_topped_up", paste0("+", round(dt$step0, 1), " mm step")),
    ev(dt$is_manual, "manual_servicing", rep("entered by hand", nrow(dt))),
    ev(dt$status_bad, "status_error", paste0(scol, " = ", dt$status)),
    ev(dt$bad_range, "bucket_reading_out_of_range", paste0(bcol, " = ", dt$bucket_raw)),
    ev(dt$bad_spike, "bucket_spike", paste0(bcol, " = ", dt$bucket_raw)),
    ev(dt$power_reset, "acc_total_power_reset", paste0("acc_total ", dt$prev_acc, " -> ", dt$acc)),
    ev(dt$long_gap_acc, "outage_end", paste0(round(dt$gap_h_acc, 1), " h without records")),
    ev(dt$long_gap_bucket & !is.na(dt$gap_accum_mm) & dt$gap_accum_mm > 0, "outage_accumulation",
       paste0(round(dt$gap_accum_mm, 1), " mm gained in the bucket across the outage")),
    if (!is.null(bucket_only_win)) data.table(Start = fmt_ts(bucket_only_win[1]), End = fmt_ts(bucket_only_win[2]), Event = "bucket_only_window", Detail = "Bucket-only window: the bucket weight is the only precipitation signal in this period; the running total is not used"),
    if (any(nzchar(daily$day_fix) & daily$day_fix != "bucket_only_window")) daily[nzchar(day_fix) & day_fix != "bucket_only_window", .(Start = format(Date), End = format(Date), Event = paste0("bucket_used_", day_fix),
        Detail = paste0("running total ", round(precip_acc_total_mm, 1), " mm, bucket ", round(precip_bucket_mm, 1), " mm; bucket used"))],
    if (nrow(fault_periods)) data.table(Start = format(fault_periods$Start), End = format(fault_periods$End), Event = "faulty_period",
                                        Detail = "acc_total inflated; daily totals from the bucket series (low confidence)")), use.names = TRUE, fill = TRUE)
  if (nrow(events)) setorder(events, Start)
  dt[, next_bucket := nafill(bucket, type = "nocb")]
  gp <- dt[logged == TRUE & !is.na(gap_h_acc) & gap_h_acc > (interval_min / 60) + 1e-6]
  gaps <- gp[, .(Last_Record_Before = ft(TIMESTAMP - gap_h_acc * 3600), First_Record_After = ft(TIMESTAMP),
                 Duration_Hours = round(gap_h_acc, 1), Duration_Days = round(gap_h_acc / 24, 1),
                 Missing_Records = as.integer(round(gap_h_acc * 60 / interval_min) - 1),
                 Type = ifelse(gap_h_acc > long_gap_hours, "Outage", "Short gap"),
                 Bucket_Before_mm = round(prev_bucket, 1), Bucket_After_mm = round(next_bucket, 1),
                 Bucket_Change_mm = round(next_bucket - prev_bucket, 1))]
  if (nrow(gaps)) gaps[, Note := ifelse(is.na(Bucket_Change_mm), "", ifelse(Bucket_Change_mm < -service_step_mm, "Bucket emptied during the gap",
      ifelse(Bucket_Change_mm > service_step_mm, "Bucket topped up during the gap",
      ifelse(Type == "Outage" & Bucket_Change_mm > 0, "Bucket gain reported as outage accumulation", ""))))]
  sv <- dt[is_empty | is_topup | is_manual]
  servicing <- sv[, .(Time = ft(TIMESTAMP), Event = ifelse(is_empty, "Emptied", ifelse(is_topup, "Topped up", "Entered by hand")),
                      Bucket_Before_mm = round(prev_bucket, 1), Bucket_After_mm = round(bucket, 1),
                      Bucket_Change_mm = round(bucket - prev_bucket, 1), Hours_Since_Previous_Record = round(gap_h_bucket, 1),
                      New_Bucket_Cycle = bucket_cycle)]
  cycles <- dt[bucket_valid == TRUE, .(Start = ft(min(TIMESTAMP)), End = ft(max(TIMESTAMP)),
      Days = round(as.numeric(difftime(max(TIMESTAMP), min(TIMESTAMP), units = "days")), 1),
      Bucket_Start_mm = round(bucket[1], 1), Bucket_End_mm = round(bucket[.N], 1), Bucket_Max_mm = round(max(bucket), 1)), by = .(Bucket_Cycle = bucket_cycle)]
  yearly <- daily[, .(Final_mm = round(sum(precip_mm, na.rm = TRUE), 1),
                      Raw_acc_total_mm = if (has_acc) round(sum(precip_acc_total_mm, na.rm = TRUE), 1) else NA_real_,
                      Bucket_mm = round(sum(precip_bucket_mm, na.rm = TRUE), 1), Outage_accum_mm = round(sum(gap_accum_mm, na.rm = TRUE), 1),
                      Low_confidence_days = sum(fault), Single_days_from_bucket = sum(nzchar(day_fix) & day_fix != "bucket_only_window"), Bucket_only_days = sum(day_fix == "bucket_only_window"), Missing_days = sum(n_records == 0), Days = .N), by = .(Year = year(Date))]
  wy0 <- CFG$water_year_start_month %||% 10
  daily[, WY := year(Date) + (month(Date) >= wy0 & wy0 > 1)]
  water_year <- daily[, .(Start = format(min(Date)), End = format(max(Date)), Final_mm = round(sum(precip_mm, na.rm = TRUE), 1), Bucket_mm = round(sum(precip_bucket_mm, na.rm = TRUE), 1),
                          Missing_days = sum(n_records == 0), Low_confidence_days = sum(fault), Days = .N), by = .(Water_year = WY)]
  daily[, WY := NULL]
  list(interval = out_iv, daily = out_daily, events = events, gaps = gaps, servicing = servicing, cycles = cycles,
       yearly = yearly, water_year = water_year, faults = fault_periods,
       info = data.table(gauge_type = type, final_source = if (bucket_only) "bucket" else if (has_acc) "auto" else "bucket", interval_min = interval_min, has_acc = has_acc, records = sum(dt$logged), off_grid_dropped = n_off,
                         status_errors = sum(dt$status_bad), out_of_range = sum(dt$bad_range), spikes = sum(dt$bad_spike), emptied = sum(dt$is_empty), topped_up = sum(dt$is_topup),
                         faulty_periods = nrow(fault_periods), single_days_from_bucket = sum(nzchar(daily$day_fix) & daily$day_fix != "bucket_only_window"), bucket_only = bucket_only_on, bucket_only_start = if (is.null(bucket_only_win)) "" else fmt_ts(bucket_only_win[1]),
                         bucket_only_end = if (is.null(bucket_only_win)) "" else fmt_ts(bucket_only_win[2]), bucket_only_days = sum(daily$day_fix == "bucket_only_window"), total_mm = round(sum(daily$precip_mm, na.rm = TRUE), 1),
                         first = fmt_ts(min(dt$TIMESTAMP)), last = fmt_ts(max(dt$TIMESTAMP)), noise_mm = noise_mm, smooth_records = smooth_n,
                         service_step_mm = service_step_mm, long_gap_hours = long_gap_hours))
}

# Run every enabled gauge and write output/precip/<station>__<stream>_*.csv
# Optional wind correction. Adds precip_corrected_mm and collection_efficiency; the uncorrected series is kept.
apply_undercatch <- function(r, g, cfg) {
  kind <- tolower(trim(g$undercatch %||% "")); r$info[, undercatch := kind]
  if (!nzchar(kind) || kind %in% c("no", "none", "off")) return(r)
  wh <- suppressWarnings(as.numeric(g$wind_height_m)); set10 <- length(wh) == 1 && !is.na(wh) && wh >= 5
  smith <- kind == "smith2007"
  if (smith && set10) stop("undercatch smith2007 needs the wind at gauge height, not at 10 m")
  co <- if (smith) UNDERCATCH$single_alter else UNDERCATCH[[if (set10) paste0(kind, "_10m") else kind]]
  if (is.null(co)) stop("undercatch must be single_alter, unshielded or smith2007, not '", kind, "'")
  for (k in c("a", "b", "c", "u_max")) { v <- suppressWarnings(as.numeric(g[[paste0("ce_", k)]])); if (length(v) && !is.na(v)) co[k] <- v }
  ws <- if (nzchar(g$wind_station %||% "")) g$wind_station else g$station; wst <- if (nzchar(g$wind_stream %||% "")) g$wind_stream else g$stream
  wc <- if (nzchar(g$wind_col %||% "")) g$wind_col else "wind_speed"; tc <- if (nzchar(g$temp_col %||% "")) g$temp_col else "air_temp"
  need <- c(wc, tc, if (smith) "rh")
  w <- load_station(ws, wst, cfg$project_dir, columns = need)
  if (!all(need %in% names(w))) stop("columns '", paste(need, collapse = "', '"), "' are needed in ", ws, " / ", wst)
  w <- w[, .(TIMESTAMP, U = as.numeric(get(wc)), Ta = as.numeric(get(tc)), RH = if (smith) as.numeric(rh) else NA_real_)]
  iv <- r$interval; step <- r$info$interval_min * 60
  # mean wind and temperature over each gauge interval (the interval ends at its timestamp)
  w[, TIMESTAMP := as.POSIXct(ceiling(as.numeric(TIMESTAMP) / step) * step, origin = "1970-01-01", tz = "UTC")]
  w <- w[, .(U = mean(U, na.rm = TRUE), Ta = mean(Ta, na.rm = TRUE), RH = mean(RH, na.rm = TRUE)), by = TIMESTAMP]
  iv[w, on = "TIMESTAMP", `:=`(U = i.U, Ta = i.Ta, RH = i.RH)]
  if (smith) {
    iv[, collection_efficiency := round(smith2007_ce(pmax(U, 0), rain_fraction(Ta, RH)), 3)]
  } else {
    iv[, collection_efficiency := round(exp(-co[["a"]] * pmin(pmax(U, 0), co[["u_max"]]) * (1 - atan(co[["b"]] * Ta) + co[["c"]])), 3)]
  }
  iv[!is.finite(collection_efficiency), collection_efficiency := NA_real_]
  iv[, precip_corrected_mm := round(ifelse(is.na(precip_mm), NA_real_, ifelse(is.na(collection_efficiency), precip_mm, precip_mm / collection_efficiency)), 3)]
  lim <- function(ce, d) pmin(1, pmax(0.05, ce + d))                                 # CE plus or minus its uncertainty, kept between 0.05 and 1
  iv[, `:=`(precip_corrected_low_mm = round(ifelse(is.na(precip_mm), NA_real_, ifelse(is.na(collection_efficiency), precip_mm, precip_mm / lim(collection_efficiency, CE_SIGMA))), 3),
            precip_corrected_high_mm = round(ifelse(is.na(precip_mm), NA_real_, ifelse(is.na(collection_efficiency), precip_mm, precip_mm / lim(collection_efficiency, -CE_SIGMA))), 3))]
  dd <- iv[, .(corr = sum(precip_corrected_mm, na.rm = TRUE), raw = sum(precip_mm, na.rm = TRUE), ce = mean(collection_efficiency, na.rm = TRUE),
               lo = sum(precip_corrected_low_mm, na.rm = TRUE), hi = sum(precip_corrected_high_mm, na.rm = TRUE),
               uncorrected = sum(!is.na(precip_mm) & precip_mm > 0 & is.na(collection_efficiency))), by = .(Date = as.Date(TIMESTAMP))]
  da <- r$daily; da[dd, on = "Date", `:=`(.corr = i.corr, .raw = i.raw, .ce = i.ce, .lo = i.lo, .hi = i.hi)]
  # days taken from the bucket as a whole (faulty periods) have no interval values: use the day's mean efficiency
  da[, precip_corrected_mm := round(ifelse(is.na(precip_mm), NA_real_, ifelse(abs(.raw - precip_mm) < 0.01, .corr,
                                    ifelse(is.finite(.ce), precip_mm / .ce, precip_mm))), 3)]
  da[, collection_efficiency := round(ifelse(is.finite(.ce), .ce, NA_real_), 3)]
  band <- function(x, d) round(ifelse(is.na(da$precip_mm), NA_real_, ifelse(abs(da$.raw - da$precip_mm) < 0.01, x, ifelse(is.finite(da$.ce), da$precip_mm / lim(da$.ce, d), da$precip_mm))), 3)
  da[, `:=`(precip_corrected_low_mm = band(.lo, CE_SIGMA), precip_corrected_high_mm = band(.hi, -CE_SIGMA))]; da[, c(".corr", ".raw", ".ce", ".lo", ".hi") := NULL]
  iv[, c("U", "Ta", "RH") := NULL]
  yy <- da[, .(Corrected_mm = round(sum(precip_corrected_mm, na.rm = TRUE), 1)), by = .(Year = year(Date))]
  r$yearly[yy, on = "Year", Corrected_mm := i.Corrected_mm]
  r$info[, `:=`(corrected_total_mm = round(sum(da$precip_corrected_mm, na.rm = TRUE), 1), corrected_low_mm = round(sum(da$precip_corrected_low_mm, na.rm = TRUE), 1),
                corrected_high_mm = round(sum(da$precip_corrected_high_mm, na.rm = TRUE), 1), ce_set = if (smith) "Smith (2007), gauge-height wind" else if (set10) "10 m wind" else "gauge-height wind", ce_uncertainty = CE_SIGMA, wind_from = paste(ws, wst, wc, sep = " / "),
                wet_records_without_wind = sum(dd$uncorrected), ce_a = co[["a"]], ce_b = co[["b"]], ce_c = co[["c"]], ce_u_max = co[["u_max"]])]
  r
}

process_precip <- function(cfg, gauges, field_log = NULL) {
  pdir <- file.path(cfg$project_dir, "output", "precip"); dir.create(pdir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(pdir, full.names = TRUE)); summ <- list()
  g <- gauges[!tolower(enabled) %in% c("no", "n", "false", "0") & nzchar(station)]
  for (i in seq_len(nrow(g))) {
    key <- paste(g$station[i], g$stream[i], sep = "__")
    tryCatch({
      if (!length(list.files(file.path(cfg$project_dir, "output", "stations"), pattern = paste0("^", safe_name(key), "\\.(csv|parquet)$"))))
        stop("there is no station table called ", g$station[i], " / ", g$stream[i], " any more (renamed or left out?). Point this gauge at the right station under Precipitation, Gauge set-up, or delete its row.")
      cols <- c(g$bucket_col[i], g$acc_col[i], g$status_col[i], g$temp_col[i], "pluvio_temp", "temp", "air_temp", "source_file"); cols <- unique(cols[nzchar(cols)])
      d <- load_station(g$station[i], g$stream[i], cfg$project_dir, columns = cols)
      gi <- as.list(g[i])
      if (!is.null(field_log) && nrow(field_log)) { fl <- field_log[station == gi$station & (stream == "" | stream == gi$stream) & grepl("gauge|servic", type, ignore.case = TRUE) & nzchar(date), date]
        if (length(fl)) gi$manual_servicing <- paste(c(gi$manual_servicing[nzchar(gi$manual_servicing)], fl), collapse = ";") }
      r <- process_gauge(d, gi)
      r <- tryCatch(apply_undercatch(r, gi, cfg), error = function(e) { problem(paste("undercatch correction", key), conditionMessage(e)); r })
      b <- file.path(pdir, safe_name(key))
      fwrite(r$interval, paste0(b, "_interval.csv"), dateTimeAs = "write.csv"); fwrite(r$daily, paste0(b, "_daily.csv"))
      for (nm in c("events", "gaps", "servicing", "cycles", "yearly", "water_year")) fwrite(r[[nm]], paste0(b, "_", nm, ".csv"))
      fwrite(data.table(Start = format(r$faults$Start), End = format(r$faults$End)), paste0(b, "_faulty_periods.csv"))
      summ[[key]] <- cbind(data.table(station = g$station[i], stream = g$stream[i]), r$info)
      say(sprintf("  %-36s %s: %s mm over %s records", key, r$info$gauge_type, format(r$info$total_mm, big.mark = ","),
                  format(r$info$records, big.mark = ",")), col = "fg4")
    }, error = function(e) problem(paste("precipitation gauge", key), conditionMessage(e)))
  }
  summ <- rbindlist(summ, fill = TRUE); if (!nrow(summ)) summ <- data.table(station = character(), stream = character())
  fwrite(summ, file.path(cfg$project_dir, "output", "audit", "precip_summary.csv"))
  summ
}

