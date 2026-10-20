# tests/test_formulas.R  --  Each derived value CS shows, against a worked case: a worked example in a cited source where there is
# one (FAO-56), else a case computed by hand from the cited formula (each check says which). Sources: the hub file
# Derived_values_references.md, section 2 (CS).
#   Rscript tests/test_formulas.R              (from the CS folder; also run by tests/run_tests.R)
# No published source, so only what the code does is checked (the hub lists them as gaps): the phase from air temperature alone
# (snow below -2 C, rain above 2 C), the Pluvio2 and Geonor bucket rules, the 50 W/m2 daylight floor for albedo at a station
# with no coordinates (with them, the sun within 70 degrees of the zenith, as published), the completeness shares.

if (!exists("check")) {
  Sys.setenv(CS_NO_RUN = "1", OVID_RUN_SYNC = "1")
  .app <- if (file.exists("cs_pipeline.R")) "." else ".."; suppressMessages(source(file.path(.app, "cs_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; FM_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("derived values against worked cases\n")
local({
  eq <- function(a, b, tol) isTRUE(all(abs(as.numeric(a) - as.numeric(b)) <= tol))

  # FAO-56 (Allen et al. 1998), Example 18: Brussels, 6 July, 50 deg 48' N, 100 m; Tmax 21.5, Tmin 12.3 C, ea 1.409 kPa (here as the
  # mean humidity that gives it, 1.409 / 1.997), u2 2.078 m/s, Rs 22.07 MJ/m2/day. FAO-56 gives Rn 13.28 MJ/m2/day and ET0 3.9 mm/day.
  e <- fao56_et(as.Date("2014-07-06"), (21.5 + 12.3) / 2, 21.5, 12.3, 100 * 1.409 / 1.997, 2.078, 22.07 / 0.0864, NULL, NULL, 50 + 48 / 60, 100)
  check("FAO-56 Penman-Monteith reproduces FAO-56 Example 18 (Brussels, 6 July): Rn 13.28 MJ/m2/day and ET0 3.9 mm/day", eq(e$rn, 13.28, 0.05) && eq(e$et0, 3.9, 0.05))
  check("Priestley-Taylor with alpha 1.26 on the same day (hand: 1.26 x D/(D + gamma) x 0.408 Rn = 4.43 mm/day from FAO-56's D 0.122 and gamma 0.0666)",
        eq(e$et_pt, 1.26 * 0.1224 / (0.1224 + 0.0666) * 0.408 * 13.28, 0.05))

  # vapour pressure deficit (FAO-56 eq. 11 and Annex 2: e0(25 C) = 3.168 kPa), dew point (Magnus form, Alduchov and Eskridge 1996
  # constants; hand), albedo and net radiation (Oke 1987; hand), wind (Yamartino 1984, US EPA 2000; hand), from a day of records
  tt <- as.POSIXct("2024-07-01", tz = "UTC") + (0:47) * 1800
  dd <- data.table(station = "S", stream = "T", TIMESTAMP = tt, logger_id = "1", air_temp = 25, rh = 50, sw_in = c(rep(0, 12), rep(400, 24), rep(0, 12)), sw_out = c(rep(0, 12), rep(80, 24), rep(0, 12)),
                   lw_in = 300, lw_out = 350, wind_speed = 2, wind_dir = rep(c(350, 10), 24))
  cf <- list(sites = data.table(station = character(), latitude = character(), elevation_m = character(), wind_height_m = character(), snow_sensor_height_m = character()), calib = NULL)
  dp <- derive_products(dd, cf, CFG); dv <- dp$daily
  check("vapour pressure deficit (FAO-56: es(25 C) = 3.168 kPa, so 1.584 kPa at 50 %)", eq(dv$vpd_kpa, 3.168 * 0.5, 5e-4))
  check("dew point by the Magnus formula (hand: 25 C at 50 % is 13.86 C)", eq(dv$dew_point_calc, 13.86, 0.01))
  check("daily albedo is the day's reflected over its incoming shortwave (hand: 80 / 400 = 0.2), net radiation the sum of the four (hand: 320 - 50 = 270 by day, -50 at night)",
        eq(dv$albedo_calc, 0.2, 1e-9) && eq(dp$daily$net_rad_calc, mean(c(rep(-50, 24), rep(270, 24))), 1e-6))
  da <- copy(dd)[, `:=`(sw_in = c(rep(0, 10), rep(30, 2), rep(400, 24), rep(30, 2), rep(0, 10)), sw_out = c(rep(0, 10), rep(20, 2), rep(80, 24), rep(20, 2), rep(0, 10)),
                         albedo = c(rep(-0.5, 10), rep(0.67, 2), rep(0.2, 24), rep(0.67, 2), rep(-0.5, 10)))]
  aa <- derive_products(da, cf, CFG)$daily
  check("with a logger albedo column too, the day's albedo is the sum of SW out over the sum of SW in on daylight records, above 50 W/m2 (hand: 1920 / 9600 = 0.2; the logger column's mean, -0.05, is not used)",
        eq(aa$albedo_calc, 0.2, 1e-9) && !"albedo" %in% names(aa))
  # sun position by Spencer (1971): at 61.3 N, 121.3 W on 20 June 2024 the sun is highest at 20:07 UTC (hand: 12:00 + 121.3 x 4 min
  # - the equation of time, -1.6 min), at 90 - 61.3 + 23.44 = 52.1 degrees; daylight for albedo is the sun within 70 degrees of the
  # zenith (Fausto et al. 2026), at the middle of each record, so a dim record at high sun counts and a bright one at low sun does not
  el <- sun_elevation(as.POSIXct("2024-06-20 20:07", tz = "UTC") + c(-600, 0, 600), 61.3, -121.3, 0)
  check("the sun's height by Spencer's series: 52.1 degrees at local noon on the June solstice at 61.3 N, highest at 20:07 UTC (hand)", eq(el[2], 52.1, 0.1) && el[2] > el[1] && el[2] > el[3])
  tz <- as.POSIXct("2024-06-20", tz = "UTC") + (0:47) * 1800; st <- data.table(station = "S", latitude = "61.3", longitude = "-121.3", utc_offset_hours = "-7", snow_sensor_height_m = NA_character_)
  zn <- site_zenith(tz - 900, st); si_ <- ifelse(zn < 70, 400, ifelse(zn < 90, 200, 0)); si_[which(zn < 70)[1]] <- 30; so_ <- ifelse(zn < 70, 0.2 * si_, 0.75 * si_)
  az <- derive_products(data.table(station = "S", stream = "T", TIMESTAMP = tz, logger_id = "1", sw_in = si_, sw_out = so_), list(sites = st, calib = NULL), CFG)
  check("with station coordinates, the day's albedo uses the records with the sun within 70 degrees of the zenith, also below 50 W/m2 (hand: 0.2; low-sun records at 0.75 left out)",
        sum(zn < 70) > 10 && sum(zn >= 70 & zn < 90) > 4 && eq(az$daily$albedo_calc[1], 0.2, 1e-9))
  check("wind direction 350 and 10 degrees: the mean is 0 (north), not 180; the resultant 2 m/s from the north, spread small (Yamartino)",
        eq(min(dv$wind_dir %% 360, 360 - dv$wind_dir %% 360), 0, 1e-6) && eq(dv$wind_speed_resultant_calc, 2 * cos(10 * pi / 180), 1e-4) && dv$wind_dir_sd_calc < 11 && dv$wind_dir_sd_calc > 9)

  cf2 <- list(sites = data.table(station = "S", latitude = "50.8", elevation_m = "100", wind_height_m = "2", snow_sensor_height_m = ""), calib = NULL)
  d2 <- copy(dd)[, air_temp := 15 + 5 * sin(seq_len(.N) / 48 * 2 * pi)]; e2 <- derive_products(d2, cf2, CFG)$daily
  ref <- fao56_et(as.Date("2024-07-01"), (max(d2$air_temp) + min(d2$air_temp)) / 2, max(d2$air_temp), min(d2$air_temp), 50, 2, mean(d2$sw_in), NULL, NULL, 50.8, 100)
  check("a station day's ET0 and Priestley-Taylor get the day's humidity, wind and shortwave (they were passed shifted before 2026-10-19)",
        eq(e2$et0_fao56_mm, ref$et0, 1e-9) && eq(e2$et_pt_mm, ref$et_pt, 1e-9) && e2$et0_fao56_mm > 0)

  # wind undercatch (Kochendorfer et al. 2017, single Alter, gauge-height wind; hand): CE = exp(-a U (1 - atan(b T) + c))
  ce <- function(u, t, co = UNDERCATCH$single_alter) exp(-co[["a"]] * min(u, co[["u_max"]]) * (1 - atan(co[["b"]] * t) + co[["c"]]))
  check("collection efficiency, single Alter (hand: 5 m/s at -5 C is 0.573; at +5 C 0.940; the wind capped at 7.2 m/s)",
        eq(ce(5, -5), 0.573, 5e-4) && eq(ce(5, 5), 0.940, 5e-4) && eq(ce(9, -5), ce(7.2, -5), 1e-12))
  check("Smith (2007) for an Alter-shielded Geonor: snow 1.18 exp(-0.18 U), rain 0.95, mixed by the rain fraction (hand: snow at 5 m/s 0.480, all rain 0.95)",
        eq(smith2007_ce(5, 0), 1.18 * exp(-0.9), 1e-12) && eq(smith2007_ce(5, 1), 0.95, 1e-12) && eq(smith2007_ce(0.5, 0), 1, 1e-12))

  # precipitation phase (Harder and Pomeroy 2013): rain fraction 1 / (1 + 2.630006 x 0.09336^Ti); at 100 % humidity Ti is the air
  # temperature (hand: 0 C gives 0.2755); drier air cools the hydrometeor, so less rain
  check("rain fraction from the hydrometeor temperature (hand: 0.2755 at 0 C and 100 %; below that at 60 %; snow under -2, rain over 2 C without humidity)",
        eq(rain_fraction(0, 100), 1 / (1 + 2.630006), 1e-4) && rain_fraction(0, 60) < rain_fraction(0, 100) && identical(precip_phase(c(-3, 0, 3)), c("Snow", "Mixed", "Rain")))

  # snow depth from an SR50A (Campbell Scientific manual: distance x sqrt(T_K / 273.15); hand: 1 m at -20 C is 0.9627 m)
  sr <- sr50_distance(data.table(DT = 1, air_temp = -20), c("DT", "air_temp"))
  check("SR50A distance corrected for the speed of sound at the air temperature (hand: 1 m at -20 C is 0.9627 m)", sr$corrected && eq(sr$dist, sqrt(253.15 / 273.15), 1e-9))
})

if (exists("FM_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
