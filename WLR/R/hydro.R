# R/hydro.R  --  Uncertainty of each deployment, barometric efficiency, casing movement, reference points that change in time,
# derived hydrology (water table against the ground, hydroperiod, gradients between wells, recharge) and approval of records.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# None of this changes a reading. Every result is a separate table, and every input it needs that the workbook does not hold
# (accuracy of a logger model, pairs of wells, specific yield, approval dates) is an optional set-up table.

# ---- Uncertainty budget ------------------------------------------------------------------------------------------------------
# config/logger_accuracy.csv: the stated accuracy of a logger as one standard uncertainty in metres of water. `pattern` is
# tried against the serial and then the model (regular expression, any case), first row first. The defaults are typical
# values from the manufacturers' sheets for the common models; check them against your model's sheet.
ACCURACY_COLS <- c("pattern", "sigma_m", "source", "notes")
default_accuracy <- function() data.table(
  pattern = c("U20L-0?4", "U20L-0?2", "U20L", "U20", "Levelogger ?5|LT5|M5\\b", "Levelogger|Edge|LTC|Junior", "."),
  sigma_m = c("0.004", "0.03", "0.01", "0.005", "0.003", "0.005", "0.01"),
  source = c("HOBO U20L-04 (4 m) water level logger, typical error 0.4 cm (Onset U20L specifications)",
             "HOBO U20L-02 (30 m) water level logger, typical error 3.0 cm (Onset U20L specifications)",
             "HOBO U20L-01 (9 m) water level logger, typical error 1.0 cm (Onset U20L specifications); also any U20L not named",
             "HOBO U20 water level logger, typical error (Onset)",
             "Solinst Levelogger 5, 0.05 % of a 5 m range (Solinst)", "Solinst Levelogger Edge, Junior or LTC, 0.05 % of a 10 m range (Solinst)",
             "any other logger: a cautious 1 cm"),
  notes = "typical value; check the model's sheet")

accuracy_for <- function(serial, model, tab) {
  if (is.null(tab) || !nrow(tab)) tab <- default_accuracy()
  for (k in seq_len(nrow(tab))) { p <- trim(tab$pattern[k]); v <- suppressWarnings(as.numeric(tab$sigma_m[k]))
    if (!nzchar(p) || !is.finite(v)) next
    if (grepl(p, serial %||% "", ignore.case = TRUE, perl = TRUE) || grepl(p, model %||% "", ignore.case = TRUE, perl = TRUE)) return(list(sigma = v, source = tab$source[k] %||% p)) }
  list(sigma = 0.01, source = "no accuracy row matched: 1 cm assumed") }

# The standard uncertainty (1 sigma, metres) of a deployment's depth of water, depth to water and elevation, from
#   logger   the logger's stated accuracy (logger_accuracy.csv)
#   baro     the barometric record: sigma_baro_m for a barologger at the site, sigma_fill_m for the share of the record
#            compensated with a fill-in (another station, the hypsometric reduction)
#   density  0.2 % of the median depth with a fixed density (cold fresh water differs from 1000 kg/m3 by up to that), 0.05 %
#            when the density follows the logger's temperature
#   tape     the spread of the tape checks against the logger (it holds the hang length, the logger offset and the tape
#            itself); one check or none: sigma_tape_m
#   survey   the reference point's survey (its rms, or the spread of the surveys for a mean), else sigma_survey_m
# The components are independent and add in quadrature. Their sum is the same for every reading of the deployment, and stays
# whole in a daily mean: the errors are systematic over a deployment, so they do not average out.
deployment_uncertainty <- function(dep, d, chk, model, tab, cfg) {
  a <- accuracy_for(dep$serial, model, tab)
  fill <- if ("baro_source" %in% names(d)) mean(d$baro_source == "fill", na.rm = TRUE) else 0; if (!is.finite(fill)) fill <- 0
  sb <- sqrt((1 - fill) * (cfg$sigma_baro_m %||% 0.005)^2 + fill * (cfg$sigma_fill_m %||% 0.02)^2)
  dm <- if ("depth_m" %in% names(d)) stats::median(abs(d$depth_m), na.rm = TRUE) else NA_real_; if (!is.finite(dm)) dm <- 0
  sd_ <- dm * if (identical(cfg$density_mode, "temperature")) 0.0005 else 0.002
  td <- if (!is.null(chk) && "difference_m" %in% names(chk)) chk$difference_m[is.finite(chk$difference_m)] else numeric()
  st <- if (length(td) >= 2) stats::sd(td) else cfg$sigma_tape_m %||% 0.01
  ss <- if (is.finite(dep$z_ref_rms %||% NA_real_) && dep$z_ref_rms > 0) dep$z_ref_rms else cfg$sigma_survey_m %||% 0.02
  s_depth <- sqrt(a$sigma^2 + sb^2 + sd_^2); s_dtw <- sqrt(s_depth^2 + st^2)
  data.table(deployment = dep$deployment, site = dep$site, serial = dep$serial, year = dep$year,
             sigma_logger_m = round(a$sigma, 4), logger_accuracy_from = a$source, sigma_baro_m = round(sb, 4), baro_fill_share = round(fill, 3),
             sigma_density_m = round(sd_, 4), sigma_tape_m = round(st, 4), tape_checks = length(td), sigma_survey_m = if (is.finite(dep$z_ref %||% NA_real_)) round(ss, 4) else NA_real_,
             depth_sigma_m = round(s_depth, 4), dtw_sigma_m = round(s_dtw, 4), elev_sigma_m = if (is.finite(dep$z_ref %||% NA_real_)) round(sqrt(s_dtw^2 + ss^2), 4) else NA_real_)
}

# ---- Barometric efficiency and lagged response --------------------------------------------------------------------------------
# A water-table well in open peat does not answer the air pressure once the record is compensated. One screened in a
# confined or semi-confined layer does: the level falls by BE times a rise in air pressure, at once or with a delay. Hourly
# changes of the compensated depth are regressed on the changes of air pressure (as metres of water) at lags 0 to max_lag_h
# (regression deconvolution, Rasmussen and Crawford 1997; Butler et al. 2011): BE is minus the lag-0 coefficient, and the
# cumulative response at the last lag says whether it lasts. A water table answers at once and then not at all (the response
# dies away as air reaches it, or two loggers out of step); a confined layer keeps answering. Read only when the fit explains
# at least min_r2 of the hourly changes; a lasting response above be_flag (0.1) is listed.
# NULL with fewer than min_days of hourly pairs, or when the air pressure barely moved.
barometric_efficiency <- function(d, cfg, max_lag_h = 24, min_days = 7, be_flag = 0.1, min_r2 = 0.2) {
  if (!all(c("depth_m", "baro_kpa") %in% names(d))) return(NULL)
  h <- d[is.finite(depth_m) & is.finite(baro_kpa), .(depth = mean(depth_m), baro = mean(baro_kpa)), by = .(t = floor(as.numeric(TIMESTAMP) / 3600))]
  if (nrow(h) < min_days * 24) return(NULL)
  full <- data.table(t = seq(min(h$t), max(h$t)))[h, on = "t", `:=`(depth = i.depth, baro = i.baro)]
  dy <- c(NA, diff(full$depth)); dx <- c(NA, diff(kpa_to_m(full$baro, NULL, cfg)))
  sdx <- stats::sd(dx, na.rm = TRUE); if (!is.finite(sdx) || sdx < 1e-4) return(NULL)       # no consecutive hours, or still air
  L <- min(max_lag_h, floor(nrow(full) / 20))
  X <- sapply(0:L, function(k) shift(dx, k)); colnames(X) <- paste0("l", 0:L)
  ok <- stats::complete.cases(cbind(dy, X)); if (sum(ok) < min_days * 24) return(NULL)
  f <- stats::lm.fit(cbind(1, X[ok, , drop = FALSE]), dy[ok]); a <- f$coefficients[-1]
  r2 <- 1 - sum(f$residuals^2) / sum((dy[ok] - mean(dy[ok]))^2)
  be0 <- -a[1]; cum <- -sum(a)
  data.table(hours = sum(ok), max_lag_h = L, be_lag0 = round(be0, 3), be_cumulative = round(cum, 3), r2 = round(r2, 3),
             verdict = if (!is.finite(r2) || r2 < min_r2) "no clear barometric response (the air pressure explains little of the changes)"
                       else if (cum > be_flag) "lasting response to the air pressure: confined or semi-confined"
                       else if (be0 > be_flag) "short response that dies away within a day: a water table with a slow air path, or the two loggers' clocks or intervals differ"
                       else "no barometric response (water table)")
}

# ---- Casing movement ---------------------------------------------------------------------------------------------------------
# The casing's height above the ground (TOC_veg in the workbook) should not change between visits. A change of more than
# tol_m means the casing moved against the ground: frost jacking (it rose) or the ground sank or rose around it. Hang lengths
# measured from the casing and surveyed casing tops are then out of date. One row per visit with a previous visit.
casing_movement <- function(md, tol_m = 0.02) {
  none <- data.table(site = character(), year = integer(), visit_time = as.POSIXct(character(), tz = "UTC"), stickup_m = numeric(),
                     previous_visit = as.POSIXct(character(), tz = "UTC"), change_m = numeric(), status = character())
  if (is.null(md) || !nrow(md) || !"TOC_veg" %in% names(md)) return(none)
  x <- md[!is.na(site) & is.finite(TOC_veg), .(site, year, visit_time, stickup_m = TOC_veg)][order(site, visit_time)]
  x <- unique(x, by = c("site", "visit_time"))
  x[, `:=`(previous_visit = shift(visit_time), change_m = round(stickup_m - shift(stickup_m), 3)), by = site]
  x <- x[!is.na(change_m)]; if (!nrow(x)) return(none)
  x[, status := fifelse(change_m > tol_m, "casing rose against the ground (frost jacking?)", fifelse(change_m < -tol_m, "casing sank, or the ground rose around it", "steady"))][]
}

# ---- Reference points that change in time -------------------------------------------------------------------------------------
# With dgps_pick = "interpolate" a deployment's reference elevation follows its surveys in time: linear between two surveys,
# held before the first and after the last. Subsiding peat then gives elevations that sink with the ground, not a step at each
# survey. pts: the survey points of one site and code (time, z).
ref_series <- function(pts, t, fallback = NA_real_) {
  if (is.null(pts) || !nrow(pts)) return(rep(fallback, length(t)))
  p <- pts[is.finite(z) & !is.na(time)][order(time)]; p <- p[, .(z = mean(z)), by = time]
  if (!nrow(p)) return(rep(fallback, length(t)))
  if (nrow(p) == 1L) return(rep(p$z, length(t)))
  stats::approx(as.numeric(p$time), p$z, as.numeric(t), rule = 2)$y
}

# ---- Water table against the ground -------------------------------------------------------------------------------------------
# The water level above (+) or below (-) the ground surface: from the T-coupler to vegetation measurement of the visit
# (T_veg) when the workbook has it, else from the elevation and a surveyed ground point (GS). That is what the hydroperiod
# and the water table of a peatland are reported in.
wt_ground_of <- function(d, dep, zg = NULL) {
  if (is.finite(dep$T_veg %||% NA_real_) && "dtw_m" %in% names(d)) return(dep$T_veg - d$dtw_m)
  if (!is.null(zg) && "elev_m" %in% names(d)) return(d$elev_m - zg)
  rep(NA_real_, nrow(d))
}

# ---- Derived hydrology from daily means ----------------------------------------------------------------------------------------
# Hydroperiod: per site and water year, the days with the daily mean water table at or above the ground, the longest run of
# them, and the days with data.
hydroperiod <- function(daily, year_complete = 0.9, water_year_start = WATER_YEAR_START) {
  x <- if (nrow(daily)) daily[parameter == "wt_ground_m"] else data.table()
  if (!nrow(x)) return(data.table(site = character(), water_year = integer(), days = integer(), days_flooded = integer(), longest_flooded_days = integer(), complete = logical()))
  x <- x[, .(v = mean(mean)), by = .(site, date)][order(site, date)][, water_year := water_year_of(date, water_year_start)]
  x[, { up <- v >= 0; r <- rle(up & c(TRUE, diff(as.integer(date)) == 1L))
        .(days = .N, days_flooded = sum(up), longest_flooded_days = if (any(r$values)) max(r$lengths[r$values]) else 0L,
          mean_wt_ground_m = round(mean(v), 3), complete = .N >= year_complete * 365) }, by = .(site, water_year)]
}

# config/well_pairs.csv: two wells and the distance between them. The hydraulic gradient is the difference of their daily
# mean water elevations over that distance (positive: flow from a to b), with its uncertainty from the two elevations'.
# A vertical pair (a nest, distance the difference of the screen depths) gives the vertical gradient the same way.
PAIR_COLS <- c("pair", "site_a", "site_b", "distance_m", "direction", "notes")
gradients <- function(daily, pairs, sig = NULL) {
  none <- data.table(pair = character(), site_a = character(), site_b = character(), direction = character(), date = as.Date(character()),
                     head_a_m = numeric(), head_b_m = numeric(), gradient = numeric(), gradient_sigma = numeric())
  if (is.null(pairs) || !nrow(pairs) || !nrow(daily)) return(none)
  e <- daily[parameter == "elev_m", .(v = mean(mean)), by = .(site, date)]
  s_of <- function(s) { if (is.null(sig) || !nrow(sig)) return(NA_real_); v <- sig[site == s, elev_sigma_m]; if (length(v) && any(is.finite(v))) max(v, na.rm = TRUE) else NA_real_ }
  rbindlist(lapply(seq_len(nrow(pairs)), function(i) { p <- pairs[i]; L <- suppressWarnings(as.numeric(p$distance_m))
    if (!is.finite(L) || L <= 0) return(NULL)
    m <- merge(e[site == p$site_a, .(date, a = v)], e[site == p$site_b, .(date, b = v)], by = "date")
    if (!nrow(m)) return(NULL)
    sg <- sqrt(s_of(p$site_a)^2 + s_of(p$site_b)^2) / L
    m[, .(pair = p$pair %||% paste(p$site_a, p$site_b), site_a = p$site_a, site_b = p$site_b, direction = p$direction %||% "", date,
          head_a_m = round(a, 3), head_b_m = round(b, 3), gradient = signif((a - b) / L, 4), gradient_sigma = signif(sg, 3))] }), fill = TRUE) -> out
  if (nrow(out)) out else none
}

# config/specific_yield.csv: specific yield per site (* for every site). Recharge by the water-table fluctuation method
# (Healy and Cook 2002), in its simplest form: the specific yield times the sum of the daily rises of the water table, per
# water year. Without the extrapolated recession this is a lower bound in a draining peat and an upper bound where rises
# come from lateral inflow; peat's specific yield also falls with depth, so a single value is a first estimate.
SY_COLS <- c("site", "specific_yield", "notes")
wtf_recharge <- function(daily, sy, water_year_start = WATER_YEAR_START) {
  none <- data.table(site = character(), water_year = integer(), specific_yield = numeric(), days = integer(), sum_of_rises_m = numeric(), recharge_mm = numeric())
  if (is.null(sy) || !nrow(sy) || !nrow(daily)) return(none)
  x <- daily[parameter %in% c("wt_ground_m", "elev_m")]
  if (!nrow(x)) return(none)
  pick <- x[, .(param = if ("elev_m" %in% parameter) "elev_m" else "wt_ground_m"), by = site]
  x <- x[pick, on = .(site, parameter = param)][, .(v = mean(mean)), by = .(site, date)][order(site, date)]
  syv <- function(s) { k <- which(trim(sy$site) == s); if (!length(k)) k <- which(trim(sy$site) == "*"); if (length(k)) suppressWarnings(as.numeric(sy$specific_yield[k[1]])) else NA_real_ }
  x[, `:=`(rise = pmax(0, v - shift(v)), next_day = c(NA, diff(as.integer(date))) == 1L), by = site]
  x[, water_year := water_year_of(date, water_year_start)]
  x[next_day == TRUE, .(specific_yield = syv(site[1]), days = .N, sum_of_rises_m = round(sum(rise), 3)), by = .(site, water_year)][
    is.finite(specific_yield)][, recharge_mm := round(1000 * specific_yield * sum_of_rises_m, 1)][]
}

# ---- Approval of records ---------------------------------------------------------------------------------------------------------
# config/approvals.csv: a site's record is approved up to a date, by someone, after a review (as in the USGS and Water Survey of
# Canada practice); everything after it is provisional. Exports carry the state of every value.
APPROVAL_COLS <- c("site", "approved_through", "by", "notes")
approval_of <- function(s, t, tab) {
  out <- rep("provisional", length(t)); if (is.null(tab) || !nrow(tab)) return(out)
  r <- tab[trim(tab$site) %in% c(s, "*")]
  if (!nrow(r)) return(out)
  thru <- suppressWarnings(max(as.Date(parse_date_cell(r$approved_through)), na.rm = TRUE))
  if (is.finite(thru)) out[as.Date(t) <= thru] <- "approved"
  out
}
