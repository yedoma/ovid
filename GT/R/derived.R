# R/derived.R  --  Daily means, annual statistics (trumpet curves), thaw depth and active-layer thickness, freeze / thaw dates
# and zero-curtain periods. Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# Every summary starts from daily means. A day counts only if it holds at least min_complete of the readings the logging
# interval allows; a year counts only if it holds at least year_complete of its days (otherwise it is reported, marked incomplete).

daily_means <- function(s, min_complete = 0.8) {
  if (!nrow(s)) return(data.table())
  st <- s[, .(step = step_of(TIMESTAMP[seq_len(min(.N, 5000))])), by = .(site, depth_m)]
  x <- s[, .(temp = mean(value), tmin = min(value), tmax = max(value), n = .N), by = .(site, depth_m, date = as.Date(TIMESTAMP))]
  x <- merge(x, st, by = c("site", "depth_m"))
  x[, expected := pmax(1, round(86400 / step))][, complete := n >= min_complete * expected][, step := NULL]
  setorder(x, site, depth_m, date)
  x[]
}

# A. Annual statistics per site, depth and year (calendar and water year): mean (MAGT), min, max, range of the daily means.
annual_stats <- function(dd, year_complete = 0.9, wy_month = 10) {
  if (!nrow(dd)) return(data.table())
  x <- dd[complete == TRUE]
  one <- function(x, kind) {
    yr <- if (kind == "calendar") as.integer(format(x$date, "%Y")) else water_year(as.POSIXct(x$date), wy_month)
    x[, year := yr][, .(mean = mean(temp), min = min(temp), max = max(temp), range = max(temp) - min(temp), days = .N, days_estimated = if ("estimated" %in% names(x)) sum(estimated) else 0L,
                        date_min = date[which.min(temp)], date_max = date[which.max(temp)]), by = .(site, depth_m, year)][
      , `:=`(year_type = kind, days_in_year = ifelse(kind == "calendar", 365 + (year %% 4 == 0), 365 + (year %% 4 == 0)))][
      , complete := days >= year_complete * days_in_year][, days_in_year := NULL][] }
  out <- rbind(one(copy(x), "calendar"), one(copy(x), "water"))
  out[, position := depth_position(depth_m)]          # an above-ground sensor's annual mean is not a ground temperature (MAGT)
  out[, `:=`(mean = round(mean, 3), min = round(min, 3), max = round(max, 3), range = round(range, 3))]
  setorder(out, site, year_type, year, depth_m)
  out
}

# Depth of zero annual amplitude: the shallowest depth whose annual range is below 0.1 C (a complete year, deep enough record).
zaa_depth <- function(an, thr = 0.1) an[complete == TRUE & year_type == "calendar", .(zaa_m = if (any(range < thr)) min(depth_m[range < thr]) else NA_real_), by = .(site, year)]

# Thaw depth on each day: going down from the shallowest sensor, the depth where the daily mean first drops to 0 C
# or below, interpolated linearly between the two sensors around it. NA when every sensor is above zero (thawed
# deeper than the deepest sensor) or the shallowest is at or below zero.
thaw_depth <- function(dd) {
  if (!nrow(dd)) return(data.table())
  x <- dd[complete == TRUE & depth_m >= 0][order(site, date, depth_m)]
  x[, {
    z <- depth_m; t <- temp; n <- length(z)
    if (n < 2 || t[1] <= 0) list(thaw_m = NA_real_, state = if (n >= 1 && t[1] <= 0) "frozen at the shallowest sensor" else "too few sensors", deepest_m = max(z))
    else { k <- which(t <= 0)[1]
      if (is.na(k)) list(thaw_m = NA_real_, state = "thawed below the deepest sensor", deepest_m = max(z))
      else list(thaw_m = z[k - 1] + (z[k] - z[k - 1]) * t[k - 1] / (t[k - 1] - t[k]), state = "between sensors", deepest_m = max(z)) } }, by = .(site, date)]
}

# B. Active-layer thickness per site and year: the deepest daily thaw depth of the year. Reported only where permafrost
# is present; otherwise the reason is given. Permafrost is ground at or below 0 C for at least two consecutive years
# (van Everdingen 1998, IPA glossary), so by default a depth must stay at or below 0 C all year in pf_years (2) consecutive
# complete calendar years. A depth frozen in one year whose neighbouring years are not in the record is reported with its
# ALT but marked "permafrost not yet confirmed"; one frozen between thawed years is not permafrost (pereletok).
# pf_years = 1 gives the one-year test used before (Settings, permafrost_years); a vector named by site sets it per site.
active_layer <- function(td, an, pf_years = 2) {
  if (!nrow(td)) return(data.table())
  need <- function(s) { n <- if (!is.null(names(pf_years))) pf_years[s] else rep(pf_years[1], length(s)); n[is.na(n)] <- 2; pmax(1L, as.integer(n)) }   # per site
  td[, year := as.integer(format(date, "%Y"))]
  yr <- an[year_type == "calendar" & complete == TRUE & depth_m >= 0, .(site, depth_m, year, frozen = max <= 0)][order(site, depth_m, year)][, n_need := need(site)]
  yr[, run := { r <- rleid(frozen, cumsum(c(1L, diff(year) != 1L))); ave(seq_along(r), r, FUN = length) }, by = .(site, depth_m)]
  yr[, open_end := { s <- paste(year); frozen & (!paste(year - 1L) %in% s | !paste(year + 1L) %in% s) }, by = .(site, depth_m)]
  pf <- yr[, .(permafrost = any(frozen & run >= n_need), frozen_once = any(frozen), unconfirmed = any(frozen & run < n_need & open_end), years_needed = n_need[1],
               frozen_years = if (any(frozen)) max(run[frozen]) else 0L, deepest_frozen_m = if (any(frozen)) min(depth_m[frozen]) else NA_real_,
               deepest_m = max(depth_m), shallowest_m = min(depth_m), sensors = .N), by = .(site, year)]
  a <- td[, .(alt_m = if (any(!is.na(thaw_m))) max(thaw_m, na.rm = TRUE) else NA_real_, date_alt = if (any(!is.na(thaw_m))) date[which.max(thaw_m)] else as.Date(NA),
              days = .N, below_deepest = sum(state == "thawed below the deepest sensor")), by = .(site, year)]
  a <- merge(a, pf, by = c("site", "year"), all = TRUE)
  unc <- "ALT, permafrost not yet confirmed (frozen in fewer consecutive years of record than years_needed)"
  a[, result := fifelse(is.na(permafrost), "year incomplete at one or more depths",
                fifelse(!permafrost & unconfirmed, unc,
                fifelse(!permafrost & frozen_once, "no permafrost: frozen for fewer consecutive years than years_needed (pereletok)",
                fifelse(!permafrost & below_deepest > 0, "no permafrost within the sensors: thawed below the deepest sensor",
                fifelse(!permafrost, "no permafrost within the sensors (no depth stays at or below 0 \u00b0C all year)",
                fifelse(is.na(alt_m), "no thaw found", "ALT")))))) ]
  a[!result %in% c("ALT", unc), alt_m := NA_real_]
  a[, c("frozen_once", "unconfirmed") := NULL]
  a[, alt_m := round(alt_m, 3)]
  a[]
}

# C. Freeze / thaw per depth and water year (October to September), from daily means, and zero-curtain periods
# (at least zc_days in a row within +/- band of 0 C).
freeze_thaw <- function(dd, band = 0.2, zc_days = 3, wy_month = 10) {
  if (!nrow(dd)) return(list(dates = data.table(), curtain = data.table()))
  x <- dd[complete == TRUE][order(site, depth_m, date)]
  x[, wy := water_year(as.POSIXct(date), wy_month)]
  dates <- x[, {
    fr <- which(temp <= 0); th <- which(temp > 0)
    ff <- if (length(fr)) date[fr[1]] else as.Date(NA)
    lf <- if (length(fr)) date[fr[length(fr)]] else as.Date(NA)
    ta <- if (length(fr)) { k <- which(temp > 0 & date > lf); if (length(k)) date[k[1]] else as.Date(NA) } else as.Date(NA)
    list(days = .N, first_day_at_or_below_0 = ff, last_day_at_or_below_0 = lf, first_day_above_0_after_freezing = ta,
         days_frozen = sum(temp < -band), days_thawed = sum(temp > band), days_zero_curtain = sum(abs(temp) <= band),
         mean = round(mean(temp), 3)) }, by = .(site, depth_m, water_year = wy)]
  zc_one <- function(date, temp) { ib <- abs(temp) <= band; if (!any(ib)) return(NULL); g <- cumsum(!ib | c(TRUE, diff(date) != 1))
    y <- data.table(date, temp, g)[ib, .(start = min(date), end = max(date), days = .N, mean = round(mean(temp), 3)), by = g][days >= zc_days]
    y[, g := NULL]; y }
  zc <- x[, zc_one(date, temp), by = .(site, depth_m)]
  list(dates = dates, curtain = zc)
}

# ---- D. Degree days, n-factors and the surface offset (Klene et al. 2001) ---------------------------
# Freezing and thawing degree days per series and water year: the sums of the daily means below and above 0 C (degree days are
# the area under the daily-mean curve, which for daily values is their sum). Only complete days count; a year is complete when
# it holds at least year_complete of its days. dd: daily means (site, depth_m, date, temp, complete), or air (site, date, temp).
degree_days <- function(dd, year_complete = 0.9, wy_month = 10, by = c("site", "depth_m")) {
  if (is.null(dd) || !nrow(dd)) return(data.table())
  x <- dd[complete %in% TRUE][, water_year := water_year(as.POSIXct(date), wy_month)]
  x[, .(days = .N, fdd = round(sum(-temp[temp < 0]), 1), tdd = round(sum(temp[temp > 0]), 1), mean = round(mean(temp), 3)), by = c(by, "water_year")][
    , complete := days >= year_complete * (365 + (water_year %% 4 == 0))][]
}

# Air temperature for the sites: a sensor above the ground at the site (the highest one), else the series named in
# air_temperature.csv (site, file, time_column, value_column; site * for every site), as daily means. Returns site, date,
# temp, complete, source.
air_daily <- function(dd, air_cfg, min_complete = 0.8) {
  out <- list()
  above <- if (nrow(dd)) dd[depth_m < 0] else data.table()
  if (nrow(above)) { top <- above[, .(depth_m = min(depth_m)), by = site]
    out[[1]] <- above[top, on = c("site", "depth_m")][, .(site, date, temp, complete, source = paste0("sensor ", -depth_m, " m above the ground"))] }
  if (!is.null(air_cfg) && nrow(air_cfg)) for (i in seq_len(nrow(air_cfg))) { r <- air_cfg[i]; f <- path.expand(trim(r$file))
    if (!nzchar(f)) next
    if (!file.exists(f)) { problem("air_temperature.csv", paste("row", i, "file not found:", r$file)); next }
    d <- tryCatch(fread(file = f, showProgress = FALSE), error = function(e) { problem("air_temperature.csv", paste("row", i, conditionMessage(e))); NULL })
    if (is.null(d) || !nrow(d)) next
    tc <- if (nzchar(trim(r$time_column))) trim(r$time_column) else names(d)[grepl("^(timestamp|datetime|date ?time|time|date)$", names(d), ignore.case = TRUE)][1]
    vc <- trim(r$value_column)
    if (is.na(tc) || !tc %in% names(d) || !vc %in% names(d)) { problem("air_temperature.csv", paste0("row ", i, ": columns '", tc, "' / '", vc, "' not found in ", basename(f))); next }
    t <- parse_date_cell(as.character(d[[tc]])); v <- suppressWarnings(as.numeric(d[[vc]])); ok <- !is.na(t) & is.finite(v)
    if (!any(ok)) next
    s <- data.table(TIMESTAMP = t[ok], value = v[ok]); st <- step_of(s$TIMESTAMP[seq_len(min(nrow(s), 5000))])
    a <- s[, .(temp = mean(value), n = .N), by = .(date = as.Date(TIMESTAMP))][, complete := n >= min_complete * pmax(1, round(86400 / st))]
    sites <- if (trim(r$site) %in% c("", "*")) unique(dd$site) else trim(r$site)
    for (sn in sites) if (!sn %in% vapply(out, function(o) o$site[1], "")) out[[length(out) + 1L]] <- a[, .(site = sn, date, temp, complete, source = basename(f))] }
  if (!length(out)) return(data.table(site = character(), date = as.Date(character()), temp = numeric(), complete = logical(), source = character()))
  unique(rbindlist(out), by = c("site", "date"))
}

# Per site and water year: the ground-surface series (the shallowest sensor at or below surface_max_depth), its degree days,
# the air's, the n-factors n_f = FDD_s / FDD_a and n_t = TDD_s / TDD_a, and the surface offset MAGST - MAAT.
n_factors <- function(dd, air, year_complete = 0.9, wy_month = 10, surface_max_depth = 0.1) {
  if (!nrow(dd)) return(data.table())
  gs <- dd[depth_m >= 0 & depth_m <= surface_max_depth, .(gst_depth_m = min(depth_m)), by = site]
  if (!nrow(gs)) return(data.table())
  sdd <- degree_days(dd[gs, on = c("site", depth_m = "gst_depth_m")], year_complete, wy_month)
  setnames(sdd, c("depth_m", "days", "fdd", "tdd", "mean", "complete"), c("gst_depth_m", "days_s", "fdd_s", "tdd_s", "magst", "complete_s"))
  adt <- if (nrow(air)) degree_days(air, year_complete, wy_month, by = c("site", "source")) else data.table()
  out <- if (nrow(adt)) merge(sdd, setnames(adt, c("days", "fdd", "tdd", "mean", "complete"), c("days_a", "fdd_a", "tdd_a", "maat", "complete_a")), by = c("site", "water_year"), all.x = TRUE)
         else sdd[, `:=`(source = NA_character_, days_a = NA_integer_, fdd_a = NA_real_, tdd_a = NA_real_, maat = NA_real_, complete_a = NA)]
  setnames(out, "source", "air_source")
  out[, `:=`(n_f = ifelse(complete_s & complete_a %in% TRUE & fdd_a > 0, round(fdd_s / fdd_a, 3), NA_real_),
             n_t = ifelse(complete_s & complete_a %in% TRUE & tdd_a > 0, round(tdd_s / tdd_a, 3), NA_real_))]
  out[, surface_offset := ifelse(complete_s & complete_a %in% TRUE, round(magst - maat, 3), NA_real_)]
  setcolorder(out, c("site", "water_year", "gst_depth_m", "air_source", "days_s", "days_a", "fdd_s", "tdd_s", "fdd_a", "tdd_a", "n_f", "n_t", "magst", "maat", "surface_offset"))
  out[order(site, water_year)]
}

# ---- E. Ground state by depth, and taliks ----------------------------------------------------------------------------------
# Each depth per complete calendar year: permafrost (at or below 0 C all year, in this and the next or previous year: two years
# in a row), seasonally frozen (below 0 C on some days), perennially unfrozen (above 0 C all year), or frozen this year only.
# A supra-permafrost talik is a layer unfrozen all year between the seasonal frost above and permafrost below; its top and
# bottom are where the year's minimum and maximum cross 0 C (interpolated). The whole profile is scanned, not only down to the
# first frozen sensor.
ground_state <- function(an) {
  if (!nrow(an)) return(data.table())
  x <- an[year_type == "calendar" & complete == TRUE & depth_m >= 0, .(site, depth_m, year, gs_min = min, gs_max = max)][order(site, depth_m, year)]
  x[, frozen_all := gs_max <= 0]
  x[, pf := frozen_all & ((shift(frozen_all, 1, fill = FALSE) & shift(year, 1) == year - 1) | (shift(frozen_all, -1, fill = FALSE) & shift(year, -1) == year + 1)), by = .(site, depth_m)]
  x[, state := fifelse(pf, "permafrost", fifelse(frozen_all, "frozen all year (one year only)", fifelse(gs_min > 0, "perennially unfrozen", "seasonally frozen")))]
  x[, c("frozen_all", "pf") := NULL][]
}
taliks <- function(gs) {
  if (!nrow(gs)) return(data.table())
  cross <- function(z, v) { k <- which(diff(sign(v)) != 0)[1]; if (is.na(k)) NA_real_ else round(z[k] + (z[k + 1] - z[k]) * v[k] / (v[k] - v[k + 1]), 3) }
  gs[order(site, year, depth_m), {
    z <- depth_m; lo <- gs_min; hi <- gs_max
    uf <- which(state == "perennially unfrozen"); pf <- which(state == "permafrost"); sf <- which(state == "seasonally frozen")
    below_sf <- if (length(sf)) uf[uf > min(sf)] else integer()           # unfrozen all year, under some seasonal frost
    pf_below <- if (length(below_sf)) pf[pf > min(below_sf)] else integer()
    res <- "no talik within the sensors"; top <- bot <- NA_real_
    if (length(below_sf)) { u1 <- min(below_sf); top <- cross(z[(u1 - 1):u1], lo[(u1 - 1):u1])
      if (length(pf_below)) { p1 <- min(pf_below); u2 <- max(uf[uf < p1]); bot <- cross(z[u2:p1], hi[u2:p1]); res <- "supra-permafrost talik" }
      else res <- "unfrozen below the seasonal frost, no permafrost within the sensors" }
    list(sensors = .N, seasonal_frost_to_m = if (length(sf)) max(z[sf]) else NA_real_, talik_top_m = top, talik_bottom_m = bot,
         talik_thickness_m = if (!is.na(top) && !is.na(bot)) round(bot - top, 3) else NA_real_, permafrost_from_m = if (length(pf)) min(z[pf]) else NA_real_, result = res)
  }, by = .(site, year)]
}

# ---- F. Sensor offsets during the zero curtain -------------------------------------------------------------------------------
# In saturated ground the zero curtain holds the temperature at (just below) 0 C, so a sensor's median reading over its
# zero-curtain days estimates its offset. Flagged above `limit` (0.1 C); a field heuristic, reported only, never applied.
zero_curtain_offsets <- function(dd, curtain, limit = 0.1) {
  if (!nrow(curtain) || !nrow(dd)) return(data.table())
  days <- curtain[, .(date = seq(start, end, by = "day")), by = .(site, depth_m, start)]
  x <- dd[days, on = c("site", "depth_m", "date"), nomatch = 0L]
  x[, .(periods = uniqueN(start), days = .N, median_c = round(stats::median(temp), 3)), by = .(site, depth_m)][
    , check := fifelse(abs(median_c) > limit, sprintf("median %+.2f C in the zero curtain: check the sensor's offset", median_c), "")][]
}

# ---- G. Trends: Theil-Sen slope and the Mann-Kendall test (core/trend.R) on annual values -----------------------------------
# MAGT per depth over complete calendar years, and the active-layer thickness. Slopes per decade; at least min_years values.
trends <- function(an, alt, min_years = 4) {
  one <- function(y, v, what) { ok <- is.finite(v); y <- y[ok]; v <- v[ok]; n <- length(v)
    if (n < min_years) return(list(quantity = what, years = n, first = if (n) min(y) else NA_integer_, last = if (n) max(y) else NA_integer_,
                                   slope_per_decade = NA_real_, mk_S = NA_real_, mk_p = NA_real_, autocorrelation_corrected = NA, result = paste("fewer than", min_years, "years")))
    ts <- theil_sen(y, v); mk <- mk_hamed_rao(v, y, min_n = min_years)
    list(quantity = what, years = n, first = min(y), last = max(y), slope_per_decade = round(10 * ts[["slope"]], 3), mk_S = mk[["S"]], mk_p = round(mk[["p"]], 4),
         autocorrelation_corrected = n >= 10, result = if (is.na(mk[["p"]]) || mk[["p"]] >= 0.05) "no trend at 5 %" else if (ts[["slope"]] > 0) "increasing" else "decreasing") }
  a <- if (nrow(an)) an[year_type == "calendar" & complete == TRUE & depth_m >= 0][order(site, depth_m, year), one(year, mean, "MAGT"), by = .(site, depth_m)] else data.table()
  b <- if (nrow(alt)) alt[order(site, year), one(year, alt_m, "ALT"), by = site][, depth_m := NA_real_] else data.table()
  rbind(a, b, fill = TRUE)
}

# ---- H. Gap filling of daily means (estimated values, processing level L3) ----------------------------------------------------
# A missing or incomplete day is estimated only when a method fits it, and is listed with the method; nothing here is ever
# written over a measured day.
#   max_days   a gap of up to this many days at or below min_depth is filled linearly in time between the days around it
#              (deeper ground changes slowly; near the surface a week can hold a freeze-up, so shallow gaps are not drawn in)
#   r2_min     a longer gap (or a shallow one) is estimated from the sensors just above and below in the same borehole, by a
#              regression on the days all three have, when it explains at least this share of the variance (r2) over at
#              least 60 shared days
# Returns the estimated days: site, depth_m, date, temp, method, r2, gap_days.
fill_daily <- function(dd, max_days = 5, min_depth = 0.5, r2_min = 0.95) {
  if (!nrow(dd)) return(data.table())
  x <- dd[complete == TRUE & depth_m >= 0, .(site, depth_m, date, temp)]
  out <- list()
  for (sd in split(x, by = c("site", "depth_m"), keep.by = TRUE)) {
    s <- sd$site[1]; z <- sd$depth_m[1]; setorder(sd, date)
    full <- seq(min(sd$date), max(sd$date), by = "day"); miss <- full[!full %in% sd$date]
    if (!length(miss)) next
    run <- cumsum(c(1L, diff(as.integer(miss)) != 1L)); gaps <- split(miss, run)
    nb <- sort(unique(x[site == s & depth_m != z, depth_m])); up <- rev(nb[nb < z])[1]; dn <- nb[nb > z][1]
    model <- NULL
    if (!is.na(up) || !is.na(dn)) {
      w <- sd[, .(date, y = temp)]
      for (k in c(up, dn)) if (!is.na(k)) w <- merge(w, x[site == s & depth_m == k, .(date, v = temp)], by = "date", all.x = TRUE) |> setnames("v", paste0("v", match(k, c(up, dn))))
      vs <- intersect(c("v1", "v2"), names(w)); ok <- stats::complete.cases(w[, c("y", vs), with = FALSE])
      if (sum(ok) >= 60) { f <- stats::lm(stats::reformulate(vs, "y"), data = w[ok]); r2 <- summary(f)$r.squared
        if (is.finite(r2) && r2 >= r2_min) model <- list(fit = f, vs = vs, r2 = r2, d = c(up, dn)) } }
    for (g in gaps) { n <- length(g)
      if (n <= max_days && z >= min_depth) {
        a <- sd[date == min(g) - 1, temp]; b <- sd[date == max(g) + 1, temp]
        if (length(a) && length(b)) { out[[length(out) + 1]] <- data.table(site = s, depth_m = z, date = g, temp = round(a + (b - a) * seq_len(n) / (n + 1), 3),
                                                                          method = "linear in time", r2 = NA_real_, gap_days = n); next } }
      if (!is.null(model)) { nd <- data.table(date = g)
        for (j in seq_along(model$vs)) { k <- model$d[match(model$vs[j], c("v1", "v2"))]; nd <- merge(nd, x[site == s & depth_m == k, .(date, v = temp)], by = "date", all.x = TRUE) |> setnames("v", model$vs[j]) }
        p <- stats::predict(model$fit, nd); ok <- is.finite(p)
        if (any(ok)) out[[length(out) + 1]] <- data.table(site = s, depth_m = z, date = nd$date[ok], temp = round(p[ok], 3),
                                                         method = paste0("from the sensors at ", paste(model$d[!is.na(model$d)], collapse = " and "), " m"), r2 = round(model$r2, 3), gap_days = n) } }
  }
  if (!length(out)) return(data.table(site = character(), depth_m = numeric(), date = as.Date(character()), temp = numeric(), method = character(), r2 = numeric(), gap_days = integer()))
  rbindlist(out)[order(site, depth_m, date)]
}

# Daily means with the estimated days added as complete (estimated = TRUE), for annual statistics that are allowed to use them.
with_estimates <- function(dd, est) {
  if (!nrow(est)) return(dd[, estimated := FALSE][])
  e <- est[, .(site, depth_m, date, temp, tmin = temp, tmax = temp, n = 0L, expected = 0, complete = TRUE, estimated = TRUE)]
  rbind(dd[!e, on = c("site", "depth_m", "date")][, estimated := FALSE], e, fill = TRUE)[order(site, depth_m, date)]
}

# ---- I. Apparent thermal diffusivity and the geothermal gradient --------------------------------------------------------------
# The annual temperature wave fitted at each depth (T = a + b cos wt + c sin wt, w = 2 pi / 365.25 days) gives an amplitude A
# and a phase. Between two depths dz apart, conduction alone gives
#   amplitude:  alpha = w dz^2 / (2 ln^2(A1 / A2))       phase:  alpha = w dz^2 / (2 dphi^2)
# Where the two differ, heat is not moving by conduction alone (latent heat in the zero curtain, water flow): that difference
# is the diagnostic. Only complete calendar years at depths below the surface; mm2/s (typical soils 0.1 to 1).
# The gradient is the change of the annual mean with depth between the two sensors (C/m); `deepest` marks the deepest pair.
thermal_props <- function(dd, an, year_complete = 0.9) {
  if (!nrow(dd) || !nrow(an)) return(data.table())
  ok <- an[year_type == "calendar" & complete == TRUE & depth_m > 0, .(site, depth_m, year, mean)]
  if (!nrow(ok)) return(data.table())
  w <- 2 * pi / 365.25
  x <- dd[complete == TRUE & depth_m > 0][, year := as.integer(format(date, "%Y"))][ok, on = c("site", "depth_m", "year"), nomatch = 0L]
  h <- x[, { tt <- as.numeric(date - as.Date(paste0(year[1], "-01-01"))); f <- stats::lm(temp ~ cos(w * tt) + sin(w * tt)); b <- stats::coef(f)
             list(amp = sqrt(b[[2]]^2 + b[[3]]^2), phase = atan2(b[[3]], b[[2]]), r2 = summary(f)$r.squared) }, by = .(site, depth_m, year)]
  h <- merge(h, ok, by = c("site", "depth_m", "year"))[order(site, year, depth_m)]
  h[, {
    n <- .N; if (n < 2) return(NULL)
    i <- seq_len(n - 1); dz <- depth_m[i + 1] - depth_m[i]
    la <- log(amp[i] / amp[i + 1]); dp <- (phase[i + 1] - phase[i]) %% (2 * pi); dp[dp > pi] <- NA_real_   # the deeper wave lags
    a_amp <- ifelse(la > 0, w * dz^2 / (2 * la^2), NA_real_) / 86400 * 1e6; a_ph <- ifelse(!is.na(dp) & dp > 0, w * dz^2 / (2 * dp^2), NA_real_) / 86400 * 1e6
    list(depth_top_m = depth_m[i], depth_bottom_m = depth_m[i + 1], amplitude_top_c = round(amp[i], 3), amplitude_bottom_c = round(amp[i + 1], 3),
         lag_days = round(ifelse(is.na(dp), NA_real_, dp / w), 1), alpha_amplitude_mm2_s = signif(a_amp, 3), alpha_phase_mm2_s = signif(a_ph, 3),
         fit_r2 = round(pmin(r2[i], r2[i + 1]), 3), gradient_c_per_m = round((mean[i + 1] - mean[i]) / dz, 3), deepest = i == n - 1,
         note = fifelse(is.na(a_amp) | is.na(a_ph), "the wave does not decay or lag with depth here",
                fifelse(pmax(a_amp, a_ph) / pmin(a_amp, a_ph) > 2, "amplitude and phase disagree by more than 2x: not conduction alone (latent heat, water flow)", ""))) }, by = .(site, year)]
}

# ---- J. Ground-surface change (subsidence, heave) -----------------------------------------------------------------------------
# surface_changes.csv: site, date, surface_change_m (how far the ground surface has gone down since the sensors were
# installed; negative = up, as with frost heave), method, notes. Between two surveys the change is interpolated in time;
# before the first it is 0 (give a row of 0 at the installation date for a change that builds up from there) and after the
# last it is held. Sensor depths stay as installed everywhere else; the active layer
# is reported also from the surface of the day, since a sensor 1 m down at installation is 0.9 m down after 10 cm of subsidence.
surface_change_at <- function(sc, site, dates) {
  out <- rep(0, length(dates)); if (is.null(sc) || !nrow(sc)) return(out)
  s <- site; r <- sc[sc$site %in% c(s, "*")]; if (!nrow(r)) return(out)
  r <- r[, .(d = as.Date(parse_date_cell(date)), v = suppressWarnings(as.numeric(surface_change_m)))][!is.na(d) & is.finite(v)][order(d)]
  if (!nrow(r)) return(out)
  out <- stats::approx(as.numeric(r$d), r$v, as.numeric(dates), rule = 2, ties = "ordered")$y
  out[dates < r$d[1]] <- 0; out
}
alt_from_surface <- function(alt, sc) {
  if (!nrow(alt)) return(alt)
  if (is.null(sc) || !nrow(sc)) return(alt[, `:=`(surface_change_m = 0, alt_from_surface_m = alt_m)][])
  alt[, surface_change_m := { v <- numeric(.N); for (i in seq_len(.N)) v[i] <- surface_change_at(sc, site[i], if (!is.na(date_alt[i])) date_alt[i] else as.Date(paste0(year[i], "-09-01"))); round(v, 3) }]
  alt[, alt_from_surface_m := round(alt_m - surface_change_m, 3)][]
}

# ---- K. Soil water content in frozen ground -----------------------------------------------------------------------------------
# A dielectric probe sees only the liquid water, so in frozen ground its water content is the unfrozen water, not the total
# (Spaans and Baker 1995; Campbell CS616/CS650 manuals). Each vwc reading is marked liquid_only when the ground temperature
# at the nearest sensor of the same site (within 0.15 m) had a daily mean at or below 0 C that day.
soil_phase <- function(soil, temp, max_dz = 0.15) {
  if (!nrow(soil)) return(soil)
  soil[, liquid_only := FALSE]
  if (is.null(temp) || !nrow(temp)) return(soil[])
  dt <- temp[!is.na(value), .(t = mean(value)), by = .(depth_m, date = as.Date(TIMESTAMP))]
  zs <- sort(unique(dt$depth_m)); if (!length(zs)) return(soil[])
  near <- vapply(soil$depth_m, function(z) { k <- which.min(abs(zs - z)); if (length(k) && abs(zs[k] - z) <= max_dz) zs[k] else NA_real_ }, 0)
  key <- data.table(i = seq_len(nrow(soil)), depth_m = near, date = as.Date(soil$TIMESTAMP))
  t <- dt[key, on = c("depth_m", "date")]$t
  soil[, liquid_only := variable == "vwc" & !is.na(t) & t <= 0][]
}
