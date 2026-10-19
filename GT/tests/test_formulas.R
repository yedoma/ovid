# tests/test_formulas.R  --  Each derived value GT shows, against a worked case: the analytic solution given in a cited source
# where there is one, else a case computed by hand from the definition (each check says which). Sources: the hub file
# Derived_values_references.md, section 4 (GT).
#   Rscript tests/test_formulas.R              (from the GT folder; also run by tests/run_tests.R)
# Covered in run_tests.R already, not repeated here: permafrost in two consecutive years and pereletok (van Everdingen 1998),
# taliks, gap filling from the sensors above and below, ground-surface change, soil water marked liquid only. The RBR thermistor
# equation is checked against Ruskin's own export by the reader samples (tests/samples/reference.csv).

if (!exists("check")) {
  invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
  Sys.setenv(GT_NO_RUN = "1")
  here <- if (file.exists("gt_pipeline.R")) "." else ".."; suppressMessages(source(file.path(here, "gt_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; FM_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("derived values against worked cases\n")
local({
  eq <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1))
  wy <- seq(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day")                                 # one water year, 365 days

  # daily means and completeness (common practice; hand): hourly 0..23 is a mean of 11.5; 19 of 24 readings is under 80 %
  t0 <- as.POSIXct("2021-06-01", tz = "UTC"); s <- data.table(site = "S", depth_m = 0.5, TIMESTAMP = c(t0 + 3600 * 0:23, t0 + 86400 + 3600 * 0:18), value = c(0:23, rep(1, 19)))
  d <- daily_means(s, 0.8)
  check("daily mean, min, max and completeness (hand: 24 hourly values 0..23 give 11.5; 19 of 24 is under 80 %)",
        eq(d$temp[1], 11.5) && d$tmin[1] == 0 && d$tmax[1] == 23 && d$expected[1] == 24 && identical(d$complete, c(TRUE, FALSE)))

  # annual statistics (MAGT; Romanovsky et al. 2010, Biskaborn et al. 2019; hand): 365 daily means, -3 then +5
  yr <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")
  dd <- data.table(site = "S", depth_m = 1, date = yr, temp = c(rep(-3, 200), rep(5, 165)), tmin = 0, tmax = 0, n = 24L, expected = 24, complete = TRUE)
  an <- annual_stats(dd, 0.9)[year_type == "calendar"]
  check("MAGT, minimum, maximum and range of a calendar year (hand: (200 x -3 + 165 x 5) / 365 = 0.616)",
        eq(an$mean, round((200 * -3 + 165 * 5) / 365, 3)) && an$min == -3 && an$max == 5 && an$range == 8 && an$complete)
  check("a year counts as complete from 90 % of its days (hand: 329 of 365 days complete, 328 not)",
        annual_stats(dd[1:329], 0.9)[year_type == "calendar", complete] && !annual_stats(dd[1:328], 0.9)[year_type == "calendar", complete])

  # zero annual amplitude (van Everdingen 1998; Romanovsky et al. 2010: a range under 0.1 C; hand)
  za <- data.table(site = "S", year = 2021L, year_type = "calendar", complete = TRUE, depth_m = c(5, 10, 15), range = c(0.5, 0.08, 0.02))
  check("depth of zero annual amplitude: the shallowest depth whose annual range is under 0.1 C (hand: 10 m)", zaa_depth(za)$zaa_m == 10)

  # thaw depth (Nelson and Hinkel 2003: the 0 C crossing interpolated between sensors; hand)
  td <- thaw_depth(data.table(site = "S", date = as.Date("2021-08-01"), depth_m = c(0.1, 0.5, 1), temp = c(6, 1, -0.5), complete = TRUE))
  check("thaw depth by linear interpolation between the sensors around 0 C (hand: 0.5 + 0.5 x 1 / 1.5 = 0.8333 m)", eq(td$thaw_m, 0.5 + 0.5 * 1 / 1.5) && td$state == "between sensors")

  # degree days and n-factors (Lunardini 1978, Klene et al. 2001: n_f = FDD_s / FDD_a, n_t = TDD_s / TDD_a; hand)
  sf <- data.table(site = "S", depth_m = 0.05, date = wy, temp = c(rep(-2, 182), rep(5, 183)), complete = TRUE)
  air <- data.table(site = "S", date = wy, temp = c(rep(-10, 182), rep(8, 183)), complete = TRUE, source = "test")
  dg <- degree_days(sf, 0.9, 10); nf <- n_factors(sf, air, 0.9, 10, 0.1)
  check("freezing and thawing degree days are the sums of the daily means below and above 0 C (hand: 182 x 2 = 364, 183 x 5 = 915)",
        dg$water_year == 2021 && dg$fdd == 364 && dg$tdd == 915 && dg$complete)
  check("n-factors and the surface offset (hand: n_f = 364 / 1820 = 0.2, n_t = 915 / 1464 = 0.625, MAGST - MAAT = 551 / 365 + 356 / 365 = 2.485)",
        eq(nf$n_f, 0.2) && eq(nf$n_t, 0.625) && eq(nf$fdd_a, 1820) && eq(nf$tdd_a, 1464) && eq(nf$surface_offset, round(551 / 365 - (-356 / 365), 3)))

  # freeze / thaw dates and the zero curtain (Outcalt et al. 1990; band and length are GT settings; hand)
  zt <- c(rep(3, 10), 0.15, 0.1, -0.05, -0.1, rep(-4, 10), 0.1, -0.1, rep(-5, 339))
  fz <- freeze_thaw(data.table(site = "S", depth_m = 0.5, date = wy, temp = zt, complete = TRUE), band = 0.2, zc_days = 3)
  check("zero curtain: 3 or more days in a row within 0.2 C of 0 (hand: one 4-day period; the 2-day one is too short)",
        nrow(fz$curtain) == 1 && fz$curtain$days == 4 && fz$curtain$start == wy[11] && eq(fz$curtain$mean, round(mean(c(0.15, 0.1, -0.05, -0.1)), 3)))
  check("freeze dates: the first and last day at or below 0 C (hand: day 13 and the last day)",
        fz$dates$first_day_at_or_below_0 == wy[13] && fz$dates$last_day_at_or_below_0 == wy[365] && fz$dates$days_zero_curtain == 6)

  # sensor offset in the zero curtain (Luethi and Phillips 2016; the 0.1 C limit is a GT setting; hand)
  zc <- data.table(site = "S", depth_m = 0.5, start = wy[1], end = wy[4], days = 4L)
  zo <- zero_curtain_offsets(data.table(site = "S", depth_m = 0.5, date = wy[1:4], temp = c(0.15, 0.12, 0.18, 0.11)), zc, 0.1)
  check("zero-curtain offset is the median reading over the curtain days, flagged above 0.1 C (hand: median 0.135)", eq(zo$median_c, 0.135) && nzchar(zo$check))

  # apparent thermal diffusivity (Carslaw and Jaeger 1959 sec. 2.6; Hillel 1998): for a surface wave A0 sin(w t), conduction gives
  # T(z, t) = A0 exp(-z / d) sin(w t - z / d), d = sqrt(2 alpha / w). With alpha = 0.5 mm2/s and the annual w, d = 2.2411 m, so between
  # 0.5 and 1 m the amplitude falls by exp(-0.5 / d) = 0.8000 and the wave lags (0.5 / d) / w = 12.97 days.
  a0 <- 0.5e-6 * 86400; w <- 2 * pi / 365.25; dp <- sqrt(2 * a0 / w); tt <- as.numeric(yr - yr[1])
  wave <- rbindlist(lapply(c(0.5, 1), function(z) data.table(site = "S", depth_m = z, date = yr, temp = c(-2, -1.5)[match(z, c(0.5, 1))] + 8 * exp(-z / dp) * sin(w * tt - z / dp),
                                                                  tmin = 0, tmax = 0, n = 24L, expected = 24, complete = TRUE)))
  th <- thermal_props(wave, annual_stats(wave, 0.9))
  check("thermal diffusivity from amplitude and from phase (analytic solution, Carslaw and Jaeger 1959: 0.5 mm2/s; ratio 0.800, lag 13.0 days)",
        nrow(th) == 1 && abs(th$alpha_amplitude_mm2_s - 0.5) < 0.005 && abs(th$alpha_phase_mm2_s - 0.5) < 0.005 && th$lag_days == round((0.5 / dp) / w, 1) &&
        abs(th$amplitude_bottom_c / th$amplitude_top_c - exp(-0.5 / dp)) < 0.002 && th$note == "")
  check("geothermal gradient: the change of the annual mean with depth (hand: -2 C at 0.5 m, -1.5 C at 1 m gives 1 C/m)", abs(th$gradient_c_per_m - 1) < 0.01)

  # trends (Sen 1968: the median of the pairwise slopes; Mann 1945, Kendall 1975: S and its variance n(n-1)(2n+5)/18; hand)
  # values 1, 3, 2, 5, 4, 6 in six years: 15 pairwise slopes with median 1 per year; S = 11, var = 28.33, z = 10 / 5.323 = 1.879, p = 0.0603
  ta <- data.table(site = "S", depth_m = 1, year = 2015:2020, year_type = "calendar", complete = TRUE, mean = c(1, 3, 2, 5, 4, 6))
  tr <- trends(ta, data.table(), 4)
  check("trend: Theil-Sen slope and the Mann-Kendall test (hand: slope 10 per decade, S = 11, p = 0.0603, no trend at 5 %)",
        tr$slope_per_decade == 10 && tr$mk_S == 11 && eq(tr$mk_p, round(2 * stats::pnorm(-10 / sqrt(6 * 5 * 17 / 18)), 4)) && tr$mk_p == 0.0603 && tr$result == "no trend at 5 %")

  # gap filling, linear in time (Staub et al. 2017 for the method; hand): a 2-day gap between 1.0 and 1.6 at 1 m
  gd <- data.table(site = "S", depth_m = 1, date = wy[1:10], temp = c(1, 1, 1, 1, NA, NA, 1.6, 1.6, 1.6, 1.6), complete = TRUE)[!is.na(temp)]
  gf <- fill_daily(gd, 5, 0.5, 0.95)
  check("a short deep gap is filled linearly in time (hand: 1.2 and 1.4)", nrow(gf) == 2 && eq(gf$temp, c(1.2, 1.4)) && all(gf$method == "linear in time"))
})

if (exists("FM_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
