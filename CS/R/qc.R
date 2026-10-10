# R/qc.R  --  Quality rules: removals, suspect flags, cross-sensor and between-station checks, duplicate tables.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# Suspect values are flagged, not removed: the value stays in the station table and the period is
# listed in output/audit/flags.csv (and as a flag column in exports). A rule removes a value only
# when its action says so (range limits and spikes remove by default; set action = flag to keep them).
# (FLAGS, flag_add() and flag_mark() are core qc.R.)
flag_ranges <- function(station, stream, parameter, start, end, records, check, detail, rule_id = check) {
  if (!length(start)) return(invisible(0L))
  flag_add(data.table(station = as.character(station), stream = as.character(stream), parameter = parameter,
    start = fmt_ts(start), end = fmt_ts(end), records = as.integer(records), check = check, detail = detail, rule_id = rule_id))
  invisible(sum(records))
}

add_flag <- function(dt, col, idx, check, detail, rule_id = check) {
  if (is.logical(idx)) idx <- which(idx)
  if (!length(idx)) return(invisible(0L))
  r <- qc_runs(idx)
  flag_ranges(dt$station[1], dt$stream[1], col, dt$TIMESTAMP[r$start], dt$TIMESTAMP[r$end], r$end - r$start + 1L, check, detail, rule_id)
}

# Sun elevation in degrees. Logger time minus the UTC offset is UTC; accurate to about a degree, which is enough to tell night from day.
sun_elevation <- function(t, lat, lon, utc_offset = 0) {
  u <- as.numeric(t) - utc_offset * 3600; doy <- as.POSIXlt(u, origin = "1970-01-01", tz = "UTC")$yday + 1
  dec <- 23.44 * pi / 180 * sin(2 * pi * (284 + doy) / 365); h <- ((u %% 86400) / 3600 + lon / 15 - 12) * 15 * pi / 180; la <- lat * pi / 180
  asin(sin(la) * sin(dec) + cos(la) * cos(dec) * cos(h)) * 180 / pi
}

cross_checks <- function(dt, site) {
  has <- function(v) v %in% names(dt) && is.numeric(dt[[v]])
  if (has("sw_in") && has("sw_out"))
    add_flag(dt, "sw_out", !is.na(dt$sw_in) & !is.na(dt$sw_out) & dt$sw_in > 50 & dt$sw_out > dt$sw_in + 10, "outgoing_exceeds_incoming",
             "reflected sunlight greater than incoming: usually snow or frost on the upward sensor, or a tilted mount")
  lat <- suppressWarnings(as.numeric(site$latitude)); lon <- suppressWarnings(as.numeric(site$longitude)); off <- suppressWarnings(as.numeric(site$utc_offset_hours))
  if (has("sw_in") && length(lat) == 1 && !is.na(lat) && !is.na(lon)) {
    el <- sun_elevation(dt$TIMESTAMP, lat, lon, if (length(off) && !is.na(off)) off else round(lon / 15))
    add_flag(dt, "sw_in", !is.na(dt$sw_in) & dt$sw_in > 20 & el < -6, "sunlight_at_night",
             "more than 20 W/m2 with the sun well below the horizon: the clock is off, or the sensor has an offset")
  }
  # physically possible limits of the Baseline Surface Radiation Network (Long and Dutton 2002, recommended QC tests V2.0):
  # incoming sunlight cannot exceed Sa * 1.5 * mu0^1.2 + 100 W/m2, and incoming longwave stays between 40 and 700 W/m2
  if (has("sw_in") && length(lat) == 1 && !is.na(lat) && !is.na(lon)) {
    u <- as.numeric(dt$TIMESTAMP) - (if (length(off) && !is.na(off)) off else round(lon / 15)) * 3600; doy <- as.POSIXlt(u, origin = "1970-01-01", tz = "UTC")$yday + 1
    mu0 <- pmax(sin(sun_elevation(dt$TIMESTAMP, lat, lon, if (length(off) && !is.na(off)) off else round(lon / 15)) * pi / 180), 0)
    ppl <- 1367 * (1 + 0.033 * cos(2 * pi * doy / 365)) * 1.5 * mu0^1.2 + 100
    add_flag(dt, "sw_in", !is.na(dt$sw_in) & dt$sw_in > ppl, "sw_above_physical_limit", "incoming sunlight above what the sun can deliver at that angle (BSRN physically possible limit)")
  }
  if (has("lw_in")) add_flag(dt, "lw_in", !is.na(dt$lw_in) & (dt$lw_in < 40 | dt$lw_in > 700), "lw_outside_physical_limit", "incoming longwave outside 40 to 700 W/m2 (BSRN physically possible limit)")
  if (has("rh")) add_flag(dt, "rh", !is.na(dt$rh) & dt$rh > 103, "rh_above_103", "relative humidity more than 3 percent above saturation: the sensor is faulty or wet")
  if (has("wind_speed") && has("air_temp") && has("rh"))                   # a frozen anemometer reads zero in humid freezing air
    add_flag(dt, "wind_speed", qc_cond_runs_idx(!is.na(dt$wind_speed) & dt$wind_speed <= 0.01 & !is.na(dt$air_temp) & dt$air_temp < 0 & !is.na(dt$rh) & dt$rh > 90, dt$TIMESTAMP, 3),
             "possible_icing", "no wind for 3 hours or more while below freezing and humid: the anemometer may be frozen")
  for (rv in intersect(c("rain", "rain_tips"), names(dt))) if (has(rv) && has("air_temp"))
    add_flag(dt, rv, !is.na(dt[[rv]]) & dt[[rv]] > 0 & !is.na(dt$air_temp) & dt$air_temp < 0, "tips_below_freezing", "a tipping bucket records while it is below freezing: snow or ice may be melting in it, or the gauge is heated")
  pairs <- list(c("air_temp", "air_temp_2"), c("air_temp_high_avg", "air_temp_low_avg"), c("rh", "rh_2"), c("rh_high_avg", "rh_low_avg"))
  lim <- c(5, 5, 25, 25)
  for (k in seq_along(pairs)) { a <- pairs[[k]][1]; b <- pairs[[k]][2]
    if (has(a) && has(b)) { bad <- !is.na(dt[[a]]) & !is.na(dt[[b]]) & abs(dt[[a]] - dt[[b]]) > lim[k]
      for (v in c(a, b)) add_flag(dt, v, bad, "sensors_disagree", paste0(a, " and ", b, " differ by more than ", lim[k])) } }
  if (has("air_temp") && has("air_temp_min") && has("air_temp_max"))
    add_flag(dt, "air_temp", !is.na(dt$air_temp) & ((!is.na(dt$air_temp_min) & dt$air_temp < dt$air_temp_min - 0.05) | (!is.na(dt$air_temp_max) & dt$air_temp > dt$air_temp_max + 0.05)),
             "mean_outside_min_max", "the average lies outside the minimum and maximum logged for the same interval")
  invisible(NULL)
}

apply_qc <- function(dt, cfgs, cfg) {
  vals <- setdiff(names(dt), META_COLS)
  nums <- vals[vapply(vals, function(v) is.numeric(dt[[v]]), logical(1))]
  for (v in nums) {
    x <- dt[[v]]
    remove_cells(dt, v, !is.na(x) & (is.infinite(x) | (!isFALSE(cfg$remove_error_codes) & x %in% cfg$sentinels)), "qc", "sentinel",
                 "logger out-of-range / overflow code")
  }
  q <- cfgs$qc; tnum <- as.numeric(dt$TIMESTAMP)
  # spike and step limits are set for 30-minute and hourly records: a daily maximum that jumps 10 degrees and back is weather
  step_h <- stats::median(diff(tnum), na.rm = TRUE) / 3600; lim_h <- cfg$spike_step_max_interval_h %||% 3
  fast <- !is.finite(step_h) || !is.finite(lim_h) || lim_h <= 0 || step_h <= lim_h
  ap <- if ("applies_to" %in% names(q)) trim(as.character(q$applies_to)) else rep("", nrow(q)); ap[is.na(ap)] <- ""
  here <- function(a) { w <- trim(strsplit(a, "|", fixed = TRUE)[[1]]); as.character(dt$station) %in% w | as.character(dt$logger_id) %in% w }
  for (i in seq_len(nrow(q))) {
    lo <- suppressWarnings(as.numeric(q$min[i])); hi <- suppressWarnings(as.numeric(q$max[i]))
    sp <- suppressWarnings(as.numeric(q$max_spike[i])); fh <- suppressWarnings(as.numeric(q$max_flat_hours[i]))
    act <- tolower(trim(q$action[i] %||% "")); rid <- if ("rule_id" %in% names(q) && nzchar(q$rule_id[i])) q$rule_id[i] else NULL
    drop <- function(v, idx, rule, reason) if (act == "flag") add_flag(dt, v, idx, rule, reason, rid %||% rule) else remove_cells(dt, v, idx, "qc", rule, reason)
    for (v in nums[grepl(q$pattern[i], nums, ignore.case = TRUE, perl = TRUE)]) {
      x <- dt[[v]]
      # a rule with applies_to covers only those stations or loggers, and there it replaces the general rule for the same column
      m <- if (nzchar(ap[i])) here(ap[i]) else { o <- which(nzchar(ap) & seq_along(ap) != i); o <- o[vapply(o, function(j) grepl(q$pattern[j], v, ignore.case = TRUE, perl = TRUE), logical(1))]
        if (length(o)) !Reduce(`|`, lapply(ap[o], here)) else rep(TRUE, nrow(dt)) }
      if (!any(m)) next
      if (!is.na(lo)) drop(v, m & !is.na(x) & x < lo, "range_min", paste0("below ", lo, " (", q$pattern[i], ")"))
      if (!is.na(hi)) drop(v, m & !is.na(x) & x > hi, "range_max", paste0("above ", hi, " (", q$pattern[i], ")"))
      if (!all(m)) next
      if (!is.na(sp) && fast) {                                      # confirm = 5: a good reading between two dropouts is kept
        k <- qc_spike_idx(dt[[v]], sp, skip_na = TRUE, confirm = 5L)  # a value next to a gap is compared with the nearest reading
        drop(v, k, "spike", paste0("single-point spike larger than ", sp))
      }
      stp <- if ("max_step_per_hour" %in% names(q)) suppressWarnings(as.numeric(q$max_step_per_hour[i])) else NA_real_
      if (!is.na(stp) && fast) {                                            # a change faster than max_step_per_hour between consecutive readings; flagged, never removed
        gap <- 3 * stats::median(diff(tnum), na.rm = TRUE) / 3600
        k <- qc_step_idx(dt[[v]], tnum, stp, if (is.finite(gap) && gap > 0) gap else Inf, min_hours = 1)   # no rate over less than an hour
        add_flag(dt, v, k, "step", paste0("changed by more than ", stp, " per hour between two readings (", q$pattern[i], ")"), rid %||% "step") }
      if (!is.na(fh) && fh > 0) {                                   # stuck sensor: the same value for longer than max_flat_hours
        # a minimum that sits at zero (calm wind within the interval) is not a stuck sensor, so a _min column exempts zero
        idx <- qc_flat_idx(dt[[v]], tnum, fh, min_n = 3L, skip_na = FALSE, band = if (grepl("_min$", v, ignore.case = TRUE)) 0 else NULL)
        if (length(idx)) { why <- paste0("unchanged for more than ", fh, " hours (", q$pattern[i], ")")
          if (act == "remove") remove_cells(dt, v, idx, "qc", "flat_line", why) else add_flag(dt, v, idx, "flat_line", why, rid %||% "flat_line") }
      }
    }
  }
  m <- cfgs$manual
  m <- m[(stream == "" | stream == dt$stream[1])]
  lg <- if ("logger_id" %in% names(dt)) as.character(dt$logger_id) else rep(NA_character_, nrow(dt))
  for (i in seq_len(nrow(m))) {
    w <- period_parts(m$station[i])                                          # rows of corrections over a period: station, logger or "a|b"
    mine <- if (!length(w) || "*" %in% w || dt$station[1] %in% w) rep(TRUE, nrow(dt)) else !is.na(lg) & lg %in% w
    if (!any(mine)) next
    s <- parse_time(m$start[i]); e <- parse_time(m$end[i])
    if (!is.na(e) && nchar(m$end[i]) <= 10) e <- e + 86399
    hit <- mine & (is.na(s) | dt$TIMESTAMP >= s) & (is.na(e) | dt$TIMESTAMP <= e)
    target <- if (m$parameter[i] %in% c("", "*")) vals else
      tryCatch(vals[grepl(paste0("^(", m$parameter[i], ")$"), vals, ignore.case = TRUE, perl = TRUE)],
               error = function(e) { problem(PERIOD_FILE, paste0("row ", m$row[i] %||% i, ": parameter '", m$parameter[i], "' is not a valid pattern; row ignored")); character() })
    why <- paste0(m$reason[i], if (nzchar(m$author[i])) paste0(" [", m$author[i], "]") else "", if (!is.null(m$row)) paste0(" (row ", m$row[i], " of ", PERIOD_FILE, ")") else "")
    for (v in target) remove_cells(dt, v, hit & !is.na(dt[[v]]), "qc", "manual", why)
  }
  if (isTRUE(cfg$cross_checks)) tryCatch(cross_checks(dt, cfgs$sites[station == dt$station[1]][1]), error = function(e) problem(paste("cross-sensor checks", dt$station[1]), conditionMessage(e)))
  dt
}

# Days on which a station departs from the same parameter at the other stations: core neighbours.R, as GT compares sites.
# Works on daily means and needs three stations or more on a day. The usual difference between two stations drifts with
# the season, so it is a 31-day running median, and while everything warms or cools fast a few days' difference in timing
# is allowed for. floor_by: the smallest departure flagged, per parameter.
neighbour_checks <- function(daily, cfg) {
  if (length(daily) < 3) return(invisible(0L))
  L <- rbindlist(daily, idcol = "key"); floor_by <- c(air_temp = 6, rh = 30, sw_in = 80, lw_in = 50)
  for (p in unique(L$parameter)) {
    w <- L[parameter == p]; if (uniqueN(w$key) < 3) next
    per <- neighbour_periods(neighbour_days(w[, .(id = key, date = Date, value)], floor = if (p %in% names(floor_by)) floor_by[[p]] else 0))
    for (i in seq_len(nrow(per))) { ks <- strsplit(per$id[i], "__", fixed = TRUE)[[1]]
      flag_ranges(ks[1], ks[2], p, as.POSIXct(format(per$start[i]), tz = "UTC"), as.POSIXct(format(per$end[i]), tz = "UTC") + 86399, per$days[i], "differs_from_other_stations",
                  paste0("daily mean departs from the other stations by up to ", per$largest[i], " more than usual (days, not records)")) }
  }
  invisible(NULL)
}

# Two station tables that hold the same measurements over the same period (the same logger read twice under
# two names, or a file converted and kept beside its original). Works on a sample: midnight every tenth day.
detect_duplicates <- function(smp) {
  none <- data.table(station_a = character(), stream_a = character(), station_b = character(), stream_b = character(), common_days_sampled = integer(),
                     columns_matching = integer(), columns_a = integer(), columns_b = integer(), share = numeric(), from = character(), to = character(), examples = character())
  smp <- Filter(Negate(is.null), smp); if (length(smp) < 2) return(none)
  # plain doubles only: a column of another class (integer64, difftime, a duplicated name) must not stop the check
  num <- function(x) { y <- suppressWarnings(tryCatch(as.numeric(x), error = function(e) rep(NA_real_, length(x)))); attributes(y) <- NULL; y }
  L <- rbindlist(lapply(names(smp), function(k) { d <- smp[[k]]; if (is.null(d) || !nrow(d) || !"TIMESTAMP" %in% names(d)) return(NULL)
    ts <- num(d$TIMESTAMP); nm <- names(d)
    rbindlist(lapply(setdiff(seq_along(nm), match("TIMESTAMP", nm)), function(j) { x <- num(d[[j]]); ok <- !is.na(x) & !is.na(ts) & x != 0
      if (sum(ok) < 10 || length(unique(x[ok])) < 10) return(NULL)
      setDT(list(key = rep(k, sum(ok)), col = rep(nm[j], sum(ok)), ts = ts[ok], val = signif(x[ok], 5))) })) }))
  if (is.null(L) || !nrow(L)) return(none)
  L[, N := .N, by = .(ts, val)]; ncol_of <- L[, .(nc = uniqueN(col)), by = key]
  L <- L[N >= 2 & N <= 12]; if (!nrow(L)) return(none)
  J <- L[L, on = .(ts, val), allow.cartesian = TRUE, nomatch = NULL][key < i.key]
  if (!nrow(J)) return(none)
  cp <- J[, .(n = .N, t0 = min(ts), t1 = max(ts)), by = .(a = key, ca = col, b = i.key, cb = i.col)]
  tsl <- lapply(smp, function(d) as.numeric(d$TIMESTAMP))
  pr <- unique(cp[, .(a, b)]); pr[, nT := mapply(function(x, y) length(intersect(tsl[[x]], tsl[[y]])), a, b)]
  cp <- cp[pr, on = .(a, b)][nT >= 15 & n >= pmax(12, 0.8 * nT)]
  if (!nrow(cp)) return(none)
  out <- cp[, .(common_days_sampled = nT[1], columns_matching = uniqueN(ca), from = format(as.Date(as.POSIXct(min(t0), origin = "1970-01-01", tz = "UTC"))),
                to = format(as.Date(as.POSIXct(max(t1), origin = "1970-01-01", tz = "UTC"))), examples = paste(head(unique(paste0(ca, " = ", cb)), 4), collapse = "; ")), by = .(a, b)]
  out[, `:=`(columns_a = ncol_of$nc[match(a, ncol_of$key)], columns_b = ncol_of$nc[match(b, ncol_of$key)])]
  out[, share := round(columns_matching / pmin(columns_a, columns_b), 2)]
  out <- out[columns_matching >= 2 & share >= 0.5]
  if (!nrow(out)) return(none)
  sp <- function(x, i) vapply(strsplit(x, "__", fixed = TRUE), function(z) z[i], "")
  out[, .(station_a = sp(a, 1), stream_a = sp(a, 2), station_b = sp(b, 1), stream_b = sp(b, 2), common_days_sampled, columns_matching, columns_a, columns_b, share, from, to, examples)][order(-share)]
}


# ---- sensor health -------------------------------------------------------------------------------------------------
# One row per parameter of a station table: how much of the record it holds, and whether the sensor looks dead,
# stopped, failing, dying (values thinning out towards the end), stuck, intermittent or suspect. Totals such as rain
# are not called stuck, since they stay at zero on dry days, nor snow depth at zero through the summer.
sensor_health <- function(dt, vals, removed = NULL, flagged = NULL, stop_days = 30, stuck_days = 7) {
  if (!nrow(dt)) return(NULL)
  t <- dt$TIMESTAMP; t0 <- min(t); t1 <- max(t); tail <- t >= t1 - 0.1 * (t1 - t0)
  get <- function(x, v) if (!is.null(x) && v %in% names(x)) as.numeric(x[[v]]) else 0
  rbindlist(lapply(vals, function(v) { x <- dt[[v]]; if (!is.numeric(x)) return(NULL)
    ok <- !is.na(x); nv <- sum(ok); rm <- get(removed, v); fl <- get(flagged, v)
    last <- if (nv) max(t[ok]) else as.POSIXct(NA); first <- if (nv) min(t[ok]) else as.POSIXct(NA)
    flat_d <- 0; if (nv > 2 && !grepl(TOTAL_RX, v, ignore.case = TRUE)) { y <- x[ok]; ty <- t[ok]; r <- rle(y); e <- cumsum(r$lengths); b <- e - r$lengths + 1L
      keep <- !(grepl("snow|swe", v, ignore.case = TRUE) & r$values == 0)          # no snow all summer is not a stuck sensor
      if (any(keep)) flat_d <- max(as.numeric(difftime(ty[e[keep]], ty[b[keep]], units = "days"))) }
    miss_tail <- mean(!ok[tail]); miss_before <- mean(!ok[!tail]); silent <- if (nv) as.numeric(difftime(t1, last, units = "days")) else NA_real_
    health <- if (!nv) "dead: no values"
      else if (silent > stop_days) sprintf("stopped: nothing in the last %.0f days of the table", silent)
      else if (rm > 0.5 * (nv + rm)) "failing: most values removed"
      else if (miss_tail > 0.5 && miss_tail > 3 * miss_before) "dying: values thin out towards the end"
      else if (flat_d > stuck_days) sprintf("stuck: unchanged for %.0f days", flat_d)
      else if (mean(!ok) > 0.1 || rm > 0.02 * (nv + rm)) "intermittent"
      else if (fl > 0.1 * nv) "suspect"
      else "ok"
    data.table(station = dt$station[1], stream = dt$stream[1], parameter = v, health = health, values = nv, missing_pct = round(100 * mean(!ok), 1),
               removed = rm, flagged = fl, first_value = first, last_value = last, days_silent_at_end = round(silent, 1),
               missing_pct_last_tenth = round(100 * miss_tail, 1), longest_unchanged_days = round(flat_d, 1)) }))
}
