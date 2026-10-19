# tests/test_formulas.R  --  Each derived value WLR shows, against a worked case: a published table or a vendor's own output
# where there is one, else a case computed by hand from the definition (each check says which). Sources: the hub file
# Derived_values_references.md, section 3 (WLR).
#   Rscript tests/test_formulas.R              (from the WLR folder; also run by tests/run_tests.R)
# Covered in run_tests.R already, not repeated here: drift proration between tape checks, the tape check itself (h = T_ST -
# offset - T_water), the Solinst Barologger's 9.5 m, psi to kPa, a fill-in barologger shifted onto the primary's level.

if (!exists("check")) {
  invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
  Sys.setenv(WLR_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- if (file.exists("wlr_pipeline.R")) "." else ".."; suppressMessages(source(file.path(here, "wlr_pipeline.R"))); say <- function(...) invisible(NULL)
  n_ok <- 0L; n_bad <- 0L; FM_ALONE <- TRUE
  check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
}
cat("derived values against worked cases\n")
local({
  eq <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1))
  cfg <- CFG

  # water density (Tanaka et al. 2001, Metrologia 38: the recommended table for SMOW, kg/m3)
  check("water density follows the published table of Tanaka et al. (2001): 999.8428 at 0 C, 999.9750 at 4, 999.7026 at 10, 998.2067 at 20, 997.0470 at 25",
        max(abs(water_density(c(0, 4, 10, 20, 25)) - c(999.8428, 999.9750, 999.7026, 998.2067, 997.0470))) < 5e-4)
  # pressure to water depth (Freeman et al. 2004; hand): h = dp 1000 / (rho g)
  check("water depth from pressure (hand: 9.80665 kPa is 1 m at 1000 kg/m3, and 1 / 0.9982067 = 1.0017965 m at 20 C with the density from temperature)",
        eq(kpa_to_m(9.80665, NULL, cfg), 1) && eq(kpa_to_m(9.80665, 20, modifyList(cfg, list(density_mode = "temperature"))), 1000 / water_density(20)) &&
        abs(kpa_to_m(9.80665, 20, modifyList(cfg, list(density_mode = "temperature"))) - 1.0017965) < 1e-6)

  # barometric compensation against the logger maker's own: Solinst Levelogger software 4.1.0 compensated the golden project's
  # Levelogger with its Barologger (8253Compensated.xle, kept outside the repo); its values at three times, in m of water
  g <- file.path(here, "tests", "golden", "raw"); p <- tempfile("wlr_comp")
  r <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(cfg, list(raw_dirs = g, metadata_dir = "", dgps_dir = "", project_dir = p, make_plots = FALSE, incremental = FALSE, unzoned_offset_h = -7))))); "" },
                error = function(e) conditionMessage(e))
  s <- list.files(file.path(p, "output", "sites"), "\\.csv$", full.names = TRUE); d <- if (length(s)) fread(s[1]) else data.table(TIMESTAMP = character(), depth_m = numeric())
  vend <- data.table(t = c("2014-02-11 19:00:00", "2014-02-14 12:00:00", "2014-02-17 17:30:00"), solinst_m = c(0.2226, 0.2693, 0.2640))   # Log id 3, 133 and 288
  m <- merge(vend, d[, .(t = format(as.POSIXct(TIMESTAMP, tz = "UTC"), "%Y-%m-%d %H:%M:%S"), depth_m)], by = "t"); off <- m$depth_m - m$solinst_m
  cat(sprintf("  (WLR minus Solinst: %s m: nearly constant, the altitude step, %s m programmed, taken off as altitude/826 by WLR and about altitude/832 by Solinst)\n",
              paste(sprintf("%+.4f", off), collapse = ", "), 269))
  check("barometric compensation agrees with the maker's software (Solinst 4.1.0) within 3 mm at three times, by a nearly constant step (the altitude; the rest is the files' 4-decimal rounding)",
        !nzchar(r) && nrow(m) == 3 && all(abs(off) < 0.003) && diff(range(off)) < 5e-4)
  unlink(p, recursive = TRUE)

  # specific conductance at 25 C (APHA 2510 B; hand): 500 uS/cm at 15 C is 500 / (1 + 0.0191 x -10) = 618.047
  check("specific conductance at 25 C (hand: 500 at 15 C is 618.047, and at 25 C stays 500)", abs(spc25(500, 15) - 618.047) < 5e-4 && spc25(500, 25) == 500)

  # hypsometric reduction of another station's pressure (Wallace and Hobbs 2006; hand): 100 kPa 50 m above, at 5 C, is
  # 100 exp(9.80665 x 50 / (287.05 x 278.15)) = 100.6160 kPa at the barologgers' elevation
  xf <- tempfile(fileext = ".csv"); fwrite(data.table(TIMESTAMP = "2025-06-01 00:00:00", P = 100), xf)
  xb <- read_external_baro(data.table(name = "X", file = xf, time_column = "TIMESTAMP", pressure_column = "P", units = "kPa", utc_offset_h = "", station_elevation_m = "300",
                                      reference_elevation_m = "250", temp_column = "", notes = ""))
  check("another station's pressure brought down 50 m by the hypsometric equation (hand: 100 kPa becomes 100.6160 at 5 C)", abs(xb[[1]]$baro_kpa - 100 * exp(9.80665 * 50 / (287.05 * 278.15))) < 1e-4 &&
        abs(xb[[1]]$baro_kpa - 100.6160) < 1e-4)

  # logger accuracy by model and range (Onset U20L and Solinst specification sheets, as default_accuracy() cites them)
  check("logger accuracy by model: U20L-04 0.4 cm, U20L-02 3.0 cm, U20L-01 1.0 cm, U20 0.5 cm, Levelogger Edge 0.5 cm, anything else 1 cm",
        accuracy_for("1", "HOBO U20L-04 Water Level", NULL)$sigma == 0.004 && accuracy_for("1", "HOBO U20L-02 Water Level", NULL)$sigma == 0.03 &&
        accuracy_for("1", "HOBO U20L-01 Water Level", NULL)$sigma == 0.01 && accuracy_for("1", "HOBO U20-001-04 Water Level", NULL)$sigma == 0.005 &&
        accuracy_for("1", "LT_EDGE M2", NULL)$sigma == 0.005 && accuracy_for("1", "Unknown", NULL)$sigma == 0.01)
  # the uncertainty budget (JCGM 100:2008, independent components in quadrature; hand): logger 0.005, barologger 0.005, density
  # 0.2 % of 1 m; with no tape check 0.01 m for the tape: depth sqrt(0.005^2 + 0.005^2 + 0.002^2) = 0.0073, depth to water 0.0124
  u <- deployment_uncertainty(data.table(deployment = "x", site = "S", serial = "1", year = 2025L), data.table(depth_m = 1, baro_source = "primary"), NULL, "LT_EDGE M2", NULL, cfg)
  check("uncertainty components add in quadrature (hand: depth 0.0073 m, depth to water 0.0124 m)", u$depth_sigma_m == 0.0073 && u$dtw_sigma_m == 0.0124)

  # barometric efficiency (Rasmussen and Crawford 1997; hand): a well whose level falls by 0.4 of every rise in air pressure
  # (as metres of water), at once and for good, has BE 0.4 at lag 0 and cumulatively
  set.seed(1); tt <- as.POSIXct("2025-06-01", tz = "UTC") + (0:(24 * 20 - 1)) * 3600; bk <- 100 + cumsum(rnorm(length(tt), 0, 0.05))
  be <- barometric_efficiency(data.table(TIMESTAMP = tt, baro_kpa = bk, depth_m = 2 - 0.4 * kpa_to_m(bk - 100, NULL, cfg)), cfg)
  check("barometric efficiency by regression deconvolution (hand: 0.4 at once and for good is a confined response)",
        abs(be$be_lag0 - 0.4) < 0.002 && abs(be$be_cumulative - 0.4) < 0.002 && grepl("confined", be$verdict))

  # hydroperiod, gradient and recharge from daily means (Mitsch and Gosselink 2015; Freeze and Cherry 1979; Healy and Cook 2002; hand)
  dts <- seq(as.Date("2024-10-01"), as.Date("2025-09-30"), by = "day"); wt <- rep(-0.2, 365); wt[101:160] <- 0.05; wt[200:239] <- 0
  hp <- hydroperiod(data.table(site = "S", date = dts, parameter = "wt_ground_m", mean = wt), 0.9, "10-01")
  check("hydroperiod: the days with the water table at or above the ground, and the longest run (hand: 100 days, 60 in a row)",
        hp$water_year == 2025 && hp$days == 365 && hp$days_flooded == 100 && hp$longest_flooded_days == 60 && hp$complete)
  ev <- rbind(data.table(site = "A", date = dts[1:3], parameter = "elev_m", mean = 100.5), data.table(site = "B", date = dts[1:3], parameter = "elev_m", mean = 100))
  gr <- gradients(ev, data.table(pair = "A-B", site_a = "A", site_b = "B", distance_m = "50", direction = "", notes = ""))
  check("hydraulic gradient: the head difference over the distance (hand: 0.5 m over 50 m is 0.01, from A to B)", nrow(gr) == 3 && all(gr$gradient == 0.01))
  el <- 100 + c(0, 0.1, 0.05, 0.25, 0.2, 0.2, 0.15)
  rc <- wtf_recharge(data.table(site = "S", date = dts[1:7], parameter = "elev_m", mean = el), data.table(site = "*", specific_yield = "0.06", notes = ""), "10-01")
  check("recharge by the water-table fluctuation method: specific yield times the sum of the daily rises (hand: 0.06 x 0.3 m = 18 mm)",
        rc$sum_of_rises_m == 0.3 && rc$recharge_mm == 18 && rc$days == 6)

  # daily means and water-year summaries (common practice; hand): 48 half-hours of 1 and 3 on one day, 46 the next
  t2 <- as.POSIXct("2024-10-01", tz = "UTC") + (0:93) * 1800
  dl <- daily_of(data.table(site = "S", TIMESTAMP = t2, depth_m = rep(c(1, 3), length.out = 94)), 0.8)
  check("daily means count a day with 80 % of its usual records (hand: mean 2 on both days; 46 of 48 records is enough)", nrow(dl) == 2 && all(dl$mean == 2))
  sm <- summaries_of(data.table(site = "S", date = dts, parameter = "depth_m", mean = rep(c(0.5, 1.5), length.out = 365), min = 0.4, max = 1.6), 0.9, NULL, "10-01")
  check("a water year from 1 October: mean, extremes and completeness (hand: 365 days averaging 0.997, complete)",
        sm$water_year$water_year == 2025 && sm$water_year$mean == round(mean(rep(c(0.5, 1.5), length.out = 365)), 3) && sm$water_year$complete && sm$water_year$range == 1.2 &&
        nrow(sm$monthly) == 12 && all(sm$monthly$complete))
})

if (exists("FM_ALONE")) { cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad)); quit(status = if (n_bad) 1 else 0) }
