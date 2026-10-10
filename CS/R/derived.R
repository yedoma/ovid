# R/derived.R  --  Derived variables and daily, monthly and water-year summaries.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# Derived variables and daily / monthly / water-year summaries of one station table (output/derived/).
# A day is the calendar date of the timestamp. A value is given only when at least min_complete of the
# expected records (or days) are present.
TOTAL_RX <- "^(rain|precip)|_Tot$|_mm_Tot|^et0?_"

# A wind direction is a compass angle, so its mean is taken on the circle: 359, 0 and 1 degree average to 0, not to 120. The
# mean direction is that of the average unit vector (the method the US EPA prefers); Yamartino (1984) gives the spread, and
# the resultant is the direction and speed of the average wind vector. A direction column is wind_dir or its high / low value.
WIND_DIR_RX <- "^wind_dir(_(high|low))?$"
circ_mean_deg <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_real_); r <- x * pi / 180; (atan2(mean(sin(r)), mean(cos(r))) * 180 / pi) %% 360 }
yamartino_sd <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) return(NA_real_); r <- x * pi / 180; e <- sqrt(max(0, 1 - (mean(sin(r))^2 + mean(cos(r))^2))); asin(min(1, e)) * (1 + 0.1547 * e^3) * 180 / pi }
wind_resultant <- function(speed, dir) { k <- !is.na(speed) & !is.na(dir); if (!any(k)) return(c(speed = NA_real_, dir = NA_real_)); r <- dir[k] * pi / 180
  u <- mean(-speed[k] * sin(r)); v <- mean(-speed[k] * cos(r)); c(speed = sqrt(u^2 + v^2), dir = (atan2(-u, -v) * 180 / pi) %% 360) }

# ---- Evapotranspiration ----------------------------------------------------------------------------------------------------
# Daily reference evapotranspiration by FAO-56 Penman-Monteith (Allen et al. 1998, eq. 6, G = 0 for a day) and Priestley-Taylor
# (alpha 1.26, or the station's value in calibration_options.csv), for wetlands and open water, in mm per day. Inputs: the day's mean, minimum and maximum air temperature (C),
# mean relative humidity (%), mean wind speed (m/s) at zu metres (taken to 2 m by FAO-56 eq. 47), incoming shortwave (W/m2,
# daily mean) and, if measured, net radiation (W/m2, daily mean; otherwise it is estimated from shortwave, temperature and
# humidity, eq. 39 and 40), the pressure (kPa; from the elevation when not measured), latitude (degrees) and elevation (m).
# A negative day (net loss of energy in winter) is 0.
fao56_et <- function(date, tmean, tmax, tmin, rh, u, rs, rn = NULL, p = NULL, lat, z = 0, zu = 2, alpha = 1.26) {
  es0 <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))
  J <- as.integer(format(date, "%j")); phi <- lat * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi * J / 365); dl <- 0.409 * sin(2 * pi * J / 365 - 1.39)
  ws <- acos(pmin(1, pmax(-1, -tan(phi) * tan(dl))))
  Ra <- 24 * 60 / pi * 0.0820 * dr * (ws * sin(phi) * sin(dl) + cos(phi) * cos(dl) * sin(ws))
  Rs <- rs * 0.0864; Rso <- (0.75 + 2e-5 * z) * Ra
  # Cloudiness Rs/Rso. When the sun is too low for it (Rso small, the polar night), the last day's ratio is carried forward
  # (ASCE-EWRI 2005, The ASCE Standardized Reference Evapotranspiration Equation, for low sun angles); 0.5 only before any.
  ratio <- ifelse(Rso > 0.5, pmin(1, pmax(0.3, Rs / Rso)), NA_real_)
  ratio <- data.table::nafill(data.table::nafill(ratio, "locf"), "nocb")
  ratio[is.na(ratio)] <- 0.5
  es <- (es0(tmax) + es0(tmin)) / 2; ea <- pmin(100, pmax(0, rh)) / 100 * es
  Rnl <- 4.903e-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 * (0.34 - 0.14 * sqrt(pmax(ea, 0))) * (1.35 * ratio - 0.35)
  Rn <- if (!is.null(rn) && any(is.finite(rn))) ifelse(is.finite(rn), rn * 0.0864, 0.77 * Rs - Rnl) else 0.77 * Rs - Rnl
  P <- if (!is.null(p) && any(is.finite(p))) ifelse(is.finite(p), p, 101.3 * ((293 - 0.0065 * z) / 293)^5.26) else 101.3 * ((293 - 0.0065 * z) / 293)^5.26
  gam <- 0.000665 * P; D <- 4098 * es0(tmean) / (tmean + 237.3)^2
  u2 <- if (abs(zu - 2) < 0.01) u else u * 4.87 / log(67.8 * zu - 5.42)
  et0 <- (0.408 * D * Rn + gam * 900 / (tmean + 273) * u2 * (es - ea)) / (D + gam * (1 + 0.34 * u2))
  pt <- alpha * D / (D + gam) * 0.408 * Rn
  list(et0 = round(pmax(0, et0), 2), et_pt = round(pmax(0, pt), 2), rn = round(Rn, 2))
}

# The SR50 distance to the snow, corrected for air temperature. Returns list(dist, corrected) or NULL when no distance is logged.
# mode "auto": a TCDT column as it is, else DT x sqrt((air_temp + 273.15) / 273.15); "none": the first distance column as logged.
sr50_distance <- function(d, nv, mode = "auto") {
  tc <- grep("(^|_)TCDT", nv, ignore.case = TRUE, value = TRUE); dc <- grep("(^|_)TCDT|^DT(_|$)|SR50.*Dist|Dist.*snow", nv, ignore.case = TRUE, value = TRUE)
  if (!length(dc)) return(NULL)
  if (identical(mode, "none")) return(list(dist = d[[dc[1]]], corrected = FALSE))
  if (length(tc)) return(list(dist = d[[tc[1]]], corrected = FALSE))
  if (!"air_temp" %in% nv) return(list(dist = d[[dc[1]]], corrected = FALSE))
  list(dist = d[[dc[1]]] * sqrt((d$air_temp + 273.15) / 273.15), corrected = TRUE)
}

derive_products <- function(dt, cfgs, cfg) {
  vals <- setdiff(names(dt), META_COLS); nv <- vals[vapply(vals, function(v) is.numeric(dt[[v]]) && any(!is.na(dt[[v]])), logical(1))]
  if (!length(nv) || nrow(dt) < 2) return(NULL)
  d <- dt[, c("TIMESTAMP", nv), with = FALSE]; has <- function(...) all(c(...) %in% nv); made <- character()
  if (has("air_temp", "rh")) { d[, vpd_kpa := round(0.6108 * exp(17.27 * air_temp / (air_temp + 237.3)) * (1 - pmin(pmax(rh, 0), 100) / 100), 4)]; made <- c(made, "vpd_kpa") }
  if (has("air_temp", "rh") && !"dew_point" %in% nv) {                          # Magnus formula, Alduchov and Eskridge (1996) constants
    g <- log(pmin(pmax(d$rh, 0.1), 100) / 100) + 17.625 * d$air_temp / (243.04 + d$air_temp); d[, dew_point_calc := round(243.04 * g / (17.625 - g), 2)]; made <- c(made, "dew_point_calc") }
  if (has("sw_in", "sw_out") && !"albedo" %in% nv) { d[, albedo_calc := ifelse(sw_in > 50 & sw_out >= 0 & sw_out <= sw_in, round(sw_out / sw_in, 3), NA_real_)]; made <- c(made, "albedo_calc") }
  if (has("sw_in", "sw_out", "lw_in", "lw_out") && !"net_rad" %in% nv) { d[, net_rad_calc := round(sw_in - sw_out + lw_in - lw_out, 2)]; made <- c(made, "net_rad_calc") }
  # Snow depth = sensor height - distance to the snow. The SR50A's distance assumes sound at 0 C, so the raw distance (DT) must be
  # scaled by sqrt((Ta + 273.15) / 273.15) with the air temperature (Campbell Scientific SR50A manual, "Temperature correction").
  # A TCDT column is already corrected and is preferred; with sr50_temp_correction = "none" the distance is used as logged.
  h <- suppressWarnings(as.numeric(cfgs$sites[station == dt$station[1], snow_sensor_height_m][1]))
  dc <- sr50_distance(d, nv, calib_get(cfgs$calib, CS_CALIBRATION, "sr50_temp_correction", dt$station[1])$value)
  if (length(h) == 1 && !is.na(h) && !is.null(dc) && !"snow_depth" %in% nv) { d[, snow_depth_calc := round(h - dc$dist, 3)]; made <- c(made, "snow_depth_calc")
    if (dc$corrected) { d[, snow_dist_tc_calc := round(dc$dist, 3)]; made <- c(made, "snow_dist_tc_calc") } }
  cols <- setdiff(names(d), "TIMESTAMP"); tot <- grepl(TOTAL_RX, cols, ignore.case = TRUE)
  step <- stats::median(diff(as.numeric(d$TIMESTAMP))); per_day <- if (is.finite(step) && step > 0) max(1, round(86400 / step)) else 1
  d[, Date := as.Date(TIMESTAMP)]
  dirc <- grepl(WIND_DIR_RX, cols)
  f_val <- function(x, is_tot, is_dir = FALSE) if (all(is.na(x))) NA_real_ else if (is_tot) sum(x, na.rm = TRUE) else if (is_dir) circ_mean_deg(x) else mean(x, na.rm = TRUE)
  agg <- function(x, by, need) {
    n <- x[, lapply(.SD, function(v) sum(!is.na(v))), by = by, .SDcols = cols]
    v <- x[, Map(f_val, .SD, tot, dirc), by = by, .SDcols = cols]
    for (j in cols) set(v, which(n[[j]] < need(n) * cfg$min_complete), j, NA_real_)
    for (j in cols) set(v, j = j, value = signif(v[[j]], 6))
    list(v = v, n = n)
  }
  day <- agg(d, "Date", function(n) per_day)
  dv <- day$v; dv[, records := d[, .N, by = Date]$N]
  # A day's albedo is the day's reflected over its incoming shortwave (Oke 1987, Boundary Layer Climates), not the mean of
  # each record's ratio, which weights low sun too much. Records with sunlight above 50 W/m2 only (an Ovid default), on days
  # with SW in and out complete.
  if ("albedo_calc" %in% names(dv)) {
    a <- d[!is.na(albedo_calc), .(albedo_day = round(sum(sw_out) / sum(sw_in), 3)), by = Date]
    dv[, albedo_calc := NA_real_]
    dv[a, on = "Date", albedo_calc := i.albedo_day]
    dv[is.na(sw_in) | is.na(sw_out), albedo_calc := NA_real_]
  }
  if (has("wind_speed", "wind_dir")) {                          # the spread and the resultant of the wind, from the records of each day
    w <- d[!is.na(wind_speed) & !is.na(wind_dir), .(wind_dir_sd_calc = yamartino_sd(wind_dir), wind_speed_resultant_calc = wind_resultant(wind_speed, wind_dir)[["speed"]],
                                                     wind_dir_resultant_calc = wind_resultant(wind_speed, wind_dir)[["dir"]], n = .N), by = Date][n >= per_day * cfg$min_complete][, n := NULL]
    if (nrow(w)) { dv[w, on = "Date", `:=`(wind_dir_sd_calc = signif(i.wind_dir_sd_calc, 6), wind_speed_resultant_calc = signif(i.wind_speed_resultant_calc, 6), wind_dir_resultant_calc = signif(i.wind_dir_resultant_calc, 6))]
      made <- c(made, "wind_dir_sd_calc", "wind_speed_resultant_calc", "wind_dir_resultant_calc") } }
  # evapotranspiration, where the station has what it needs (latitude in Station details; wind_height_m, else 2 m)
  si <- cfgs$sites[station == dt$station[1]][1]; lat <- suppressWarnings(as.numeric(si$latitude)); z <- suppressWarnings(as.numeric(si$elevation_m))
  zu <- suppressWarnings(as.numeric(si$wind_height_m %||% NA)); if (!length(zu) || is.na(zu)) zu <- 2
  al <- calib_get(cfgs$calib, CS_CALIBRATION, "pt_alpha", dt$station[1]); if (nzchar(al$note)) problem("calibration_options.csv", al$note)
  if (has("air_temp", "rh") && (has("sw_in") || has("net_rad")) && length(lat) && is.finite(lat)) {
    mm <- d[!is.na(air_temp), .(tmax = max(air_temp), tmin = min(air_temp)), by = Date]
    x <- merge(dv[, c("Date", intersect(c("air_temp", "rh", "wind_speed", "sw_in", "net_rad"), names(dv))), with = FALSE], mm, by = "Date", all.x = TRUE)
    pc <- grep("^(baro|press|bp|atm_?press|pressure)", nv, ignore.case = TRUE, value = TRUE)
    pk <- if (length(pc)) { v <- dv[[pc[1]]]; if (stats::median(v, na.rm = TRUE) > 200) v / 10 else v } else NULL
    e <- fao56_et(x$Date, (x$tmax + x$tmin) / 2, x$tmax, x$tmin,                            # FAO-56 eq. 9: Tmean from Tmax, Tmin x$rh, if ("wind_speed" %in% names(x)) x$wind_speed else NA_real_, if ("sw_in" %in% names(x)) x$sw_in else NA_real_,
                  if ("net_rad" %in% names(x)) x$net_rad else NULL, pk, lat, if (length(z) && is.finite(z)) z else 0, zu, al$number)
    new <- character()
    if (has("wind_speed")) { dv[, et0_fao56_mm := e$et0]; new <- c(new, "et0_fao56_mm") }
    if (has("net_rad") || has("sw_in")) { dv[, et_pt_mm := e$et_pt]; new <- c(new, "et_pt_mm") }
    if (length(new)) { made <- c(made, paste0(new, if (abs(zu - 2) > 0.01) sprintf(" (wind at %g m)", zu) else " (wind at 2 m)", ifelse(new == "et_pt_mm", sprintf(", alpha %g (%s)", al$number, al$choice), "")))
      cols <- c(cols, new); tot <- c(tot, rep(TRUE, length(new))); dirc <- c(dirc, rep(FALSE, length(new))) } }
  # soil water seen by a dielectric probe in frozen ground is the liquid water only: 1 on days the soil at that depth froze
  for (v in grep("^vwc(_([0-9]+)cm)?$", names(dv), value = TRUE)) {
    tc <- if (grepl("cm$", v)) sub("^vwc", "soil_temp", v) else grep("^soil_temp", names(dv), value = TRUE)[1]
    if (length(tc) && !is.na(tc) && tc %in% names(dv)) { k <- paste0(v, "_liquid_only"); dv[, (k) := as.integer(get(tc) <= 0)]; made <- c(made, k) } }
  dv[, Month := format(Date, "%Y-%m")]; dim <- function(m) as.integer(format(as.Date(paste0(m, "-01")) + 31 - as.integer(format(as.Date(paste0(m, "-01")) + 31, "%d")), "%d"))
  mon <- agg(dv, "Month", function(n) dim(n$Month))$v
  wy0 <- cfg$water_year_start_month %||% 10; dv[, WY := year(Date) + (month(Date) >= wy0 & wy0 > 1)]
  wyl <- melt(dv[, c("WY", cols), with = FALSE], id.vars = "WY", variable.name = "parameter", variable.factor = FALSE)
  wy <- wyl[, .(value = signif(f_val(value, grepl(TOTAL_RX, parameter[1], ignore.case = TRUE), grepl(WIND_DIR_RX, parameter[1])), 6), days_with_value = sum(!is.na(value))), by = .(water_year = WY, parameter)]
  wy[, `:=`(statistic = ifelse(grepl(TOTAL_RX, parameter, ignore.case = TRUE), "total", "mean"), complete = days_with_value >= 365 * cfg$min_complete)]
  dv[, c("Month", "WY") := NULL]; setcolorder(dv, c("Date", "records"))
  list(daily = dv, monthly = mon, water_year = wy[!is.na(value)], made = made, totals = cols[tot])
}

