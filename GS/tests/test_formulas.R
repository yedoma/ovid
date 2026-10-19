# tests/test_formulas.R  --  Each derived value GS shows, against a worked case: a published figure or an instrument's own
# computation where there is one, else a case computed by hand from the cited formula (each check says which). Sources: the hub
# file Derived_values_references.md, section 5 (GS: ERT), and R/gpr.R's references (GPR).
#   Rscript tests/test_formulas.R   (exit 1 on a failure; also run by run_tests.R)
# No published source, so only what the code does is checked: the array names of quadrupoles that fit no standard array
# ("mixed", among them the inverse Schlumberger), the GPR running-median smoothing of GPS heights, the 5 % geometry tolerance.

if (!exists("check")) {
  Sys.setenv(GS_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
  source(file.path(here, "gs_pipeline.R"), chdir = TRUE)
  fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
  FM_ALONE <- TRUE
}
if (!exists("read_rad")) source(file.path(here, "R", "gpr.R"))
say("derived values against worked cases")
local({
  eq <- function(a, b, tol) isTRUE(all(abs(as.numeric(a) - as.numeric(b)) <= tol))
  quad <- function(A, B, M, N, R = 1) { d <- data.table(meas_id = seq_along(A), rec_no = seq_along(A), ax = A, bx = B, mx = M, nx = N, ay = 0, by = 0, my = 0, ny = 0, az = 0, bz = 0, mz = 0, nz = 0,
    resistance_ohm = R, datetime = as.POSIXct(NA, tz = "UTC")); finish_meas(d) }

  # geometric factor K = 2 pi / (1/AM - 1/BM - 1/AN + 1/BN) (Telford et al. 1990, ch. 8; Loke 2004), per array, a = 2 m, n = 3:
  # Wenner 2 pi a = 12.566; Schlumberger pi n (n + 1) a = 75.40; dipole-dipole pi n (n + 1)(n + 2) a = 376.99; pole-dipole
  # 2 pi n (n + 1) a = 150.80; pole-pole 2 pi a = 12.566 (remote electrodes at 99999 m, read as infinitely far)
  w <- quad(0, 6, 2, 4); s <- quad(c(0, 0, 4, 8), c(14, 6, 10, 14), c(6, 2, 6, 10), c(8, 4, 8, 12))[1];   # electrodes every 2 m, so the numbers are 1 to 8
   dd <- quad(0, 2, 8, 10); pd <- quad(0, 99999, 6, 8); pp <- quad(0, 99999, 2, 99999)
  check(eq(w$geom_factor_m, 2 * pi * 2, 1e-9) && eq(s$geom_factor_m, pi * 3 * 4 * 2, 1e-9) && eq(abs(dd$geom_factor_m), pi * 3 * 4 * 5 * 2, 1e-9) &&
        eq(pd$geom_factor_m, 2 * pi * 3 * 4 * 2, 1e-9) && eq(pp$geom_factor_m, 2 * pi * 2, 1e-9),
        "geometric factor per array (Telford 1990; hand, a = 2 m, n = 3): Wenner 12.57, Schlumberger 75.40, dipole-dipole 376.99, pole-dipole 150.80, pole-pole 12.57 m")
  check(identical(c(w$array, s$array, dd$array, pd$array, pp$array), c("wenner", "schlumberger", "dipole-dipole", "pole-dipole", "pole-pole")),
        "each standard array is named from its electrode numbers")
  check(sign(dd$geom_factor_m) == -1 && eq(dd$geom_factor_m * -2, abs(dd$geom_factor_m) * 2, 1e-9),
        "dipole-dipole written A B M N has a negative K; the instrument's voltage is negative too, so rho_a = K R stays positive")

  # apparent resistivity rho_a = K R against the instruments' own: IRIS Prosys (Rho from Vp / In, 60 quadrupoles) and AGI
  # SuperSting (21 records; the instrument rounds to 6 digits)
  sd <- file.path(here, "tests", "samples")
  ir <- read_syscal_prosys(file.path(sd, "syscal_prosys_txt__dipdip_A.csv"))$meas; ag <- read_agi_stg(file.path(sd, "agi_stg__SCH-INV.stg"))$meas
  check(max(abs(ir$geom_factor_m * ir$resistance_ohm / ir$rhoa_ohmm - 1)) < 1e-4 && nrow(ir) == 60,
        "rho_a = K x Vp / In matches the IRIS Prosys export's Rho on all 60 quadrupoles (within its 2 decimals)")
  check(max(abs(ag$geom_factor_m * ag$resistance_ohm / ag$rhoa_ohmm - 1)) < 1e-5 && nrow(ag) == 21,
        "rho_a = K x R matches the AGI SuperSting's own apparent resistivity on all 21 records (within its 6 digits)")

  # QC (Herring et al. 2023; CPERS, Herring et al. 2024)
  q <- function(rule, d, v) sort(QC_RULES[[rule]](d, v))
  m <- quad(rep(0, 100), rep(6, 100), rep(2, 100), rep(4, 100)); m[, `:=`(meas_id = .I, rhoa_ohmm = c(1e6, rep(100, 99)))]
  m50 <- m[1:50]
  check(identical(q("magnitude_sd", m, 9), 1L) && !length(q("magnitude_sd", m50, 9)),
        "9 SD from the mean (Herring 2023; hand): one spike among 100 equal values lies 9.9 SD out and is flagged; among 50, at most 6.9 SD, so nothing can be")
  r <- data.table(meas_id = 1:4, a = c(1, 3, 1, 5), b = c(2, 4, 2, 6), m = c(3, 1, 5, 7), n = c(4, 2, 6, 8), resistance_ohm = c(10, 11, 10, 20))
  check(identical(q("reciprocal_pct", r, 5), 1:2) && !length(q("reciprocal_pct", r, 10)),
        "reciprocal error |R - R'| / mean (hand: 10 and 11 ohm is 9.5 %): flagged at 5 %, not at 10 %")
  ll <- quad(0:6, 3:9, 1:7, 2:8); ll[, rhoa_ohmm := c(100, 100, 100, 200, 100, 110, 100)]
  check(identical(q("moving_median", ll, c(5, 7)), 4L),
        "5-point moving median of log10 rho_a per level, 7 % (Herring 2023; hand): 200 among 100s departs by 0.30 > 0.14 and is flagged; 110 (0.04) is not")
  be <- quad(c(0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5), c(3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8), c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5, 6), c(2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 6, 7))
  be[, `:=`(meas_id = .I, rhoa_ohmm = 100, error_pct = c(20, 20, 20, rep(0, 9)))]
  qp <- data.table(profile = "t", applies_to = "all", rule = c("max_error_pct", "bad_electrode"), value = c("10", "40;4"), flag = c("high error", "bad electrode"), enabled = "TRUE")
  fq <- tryCatch(qc_survey(be, "S", "agi", qp, "t", data.table(survey_id = character(), active = character(), meas_ids = character(), electrode = character(), flag = character(), reason = character())),
                 error = function(e) NULL)
  check(!is.null(fq) && setequal(fq[rule == "bad_electrode"]$meas_id, 1:7) && setequal(fq[rule == "max_error_pct"]$meas_id, 1:3),
        "bad electrode (Herring 2023, as a share of an electrode's data flagged; hand at 40 %): electrode 1 has 3 of 4 flagged (75 %), electrode 2 3 of 7 (43 %): their 7 measurements are flagged; electrode 3, 3 of 9 (33 %), is not")

  # reciprocal error model (Slater et al. 2000; ResIPy, Blanchy et al. 2020; hand): fewer than 40 pairs gives 4 % (CPERS) or the
  # stacking error when larger; pairs whose error is 1 % + 0.01 ohm per ohm fit a line e = a + b R with a = 0.01, b = 0.01
  check(eq(error_model(be)$err, pmax(0.04, be$error_pct / 100), 1e-12) && error_model(be)$model == "fixed", "without reciprocals each datum gets 4 % (CPERS) or its stacking error when larger")
  Rr <- seq(1, 100, length.out = 60); nm <- data.table(a = 1:60, b = 61:120, m = 121:180, n = 181:240)
  rc <- rbind(cbind(nm, resistance_ohm = Rr), cbind(nm[, .(a = m, b = n, m = a, n = b)], resistance_ohm = Rr + 0.01 + 0.01 * Rr))
  em <- error_model(rc)
  check(em$model %in% c("linear", "power") && em$pairs == 60 && eq(em$b, 0.01, 2e-3) && (em$model == "power" || eq(em$a, 0.01 + 0.005 * 0.01, 2e-3)),
        "reciprocal pairs with an error of 0.01 + 0.01 R are fitted as a line with slope 0.01 (60 pairs, 20 bins)")

  # topography (hand): heights by distance along the line are interpolated to every electrode: 100 m at 0 m and 102 m at 10 m
  # put the electrode at 4 m at 100.8 m; electrode 4 (at 6 m) keeps its own 99 m
  tp <- apply_elevations(quad(0, 6, 2, 4), data.table(electrode = c("", "", "4"), distance_m = c("0", "10", ""), elevation_m = c("100", "102", "99")))
  check(eq(tp$az, 100, 1e-9) && eq(tp$mz, 100.4, 1e-9) && eq(tp$nz, 100.8, 1e-9) && eq(tp$bz, 99, 1e-9),
        "electrode heights interpolated along the line (hand: 4 m between 100 m at 0 and 102 m at 10 is 100.8 m), an electrode's own height kept")

  # GPR: velocity v = c / sqrt(epsilon), c = 0.2998 m/ns (Davis and Annan 1989); depth = v t / 2 (REFLEXW); two layers (hand)
  check(eq(GPR_GROUND[["Frozen ground, permittivity 4-5 (0.14)"]], 0.3 / sqrt(4.5), 5e-4) && eq(GPR_GROUND[["Thawed wet sand / gravel, permittivity 10-20 (0.08)"]], 0.3 / sqrt(15), 5e-4) &&
        GPR_GROUND[["Air (0.30)"]] == 0.30, "radar velocity table: 0.3 / sqrt(4.5) = 0.141 m/ns (frozen), 0.3 / sqrt(15) = 0.077 (wet sand, Wollschlager 2010), 0.30 in air")
  check(eq(gpr_depth(20, 0.1), 1, 1e-12) && eq(gpr_depth(c(20, 60), 0.14, thaw_m = 0.5, v_top = 0.05), c(0.5, 0.5 + 40 * 0.14 / 2), 1e-12),
        "depth = v t / 2 (hand: 20 ns at 0.1 m/ns is 1.0 m); under a 0.5 m thawed layer at 0.05 m/ns (20 ns two-way), 60 ns is 0.5 + 40 x 0.14 / 2 = 3.3 m")
  # distance along a GPR line (haversine, mean Earth radius 6371 km; hand): 0.001 degree of latitude is 111.19 m
  check(eq(haversine_m(61, -121, 61.001, -121), 6371000 * 0.001 * pi / 180, 1e-6), "great-circle distance (hand: 0.001 degree of latitude is 111.19 m)")
  cr <- read_cor(file.path(sd, "gpr_cor__DAT_0204.cor")); dg <- gpr_distance(cr, 223)
  check(length(dg) == 223 && eq(max(dg), sum(haversine_m(cr$lat[-nrow(cr)], cr$lon[-nrow(cr)], cr$lat[-1], cr$lon[-1])) + (223 - max(cr$trace)) * (max(dg[cr$trace]) - min(dg[cr$trace])) / (max(cr$trace) - min(cr$trace)), 1e-6),
        "a GPR line's length is the sum of the distances between its GPS fixes, continued at the same pace past the last fix (sample line)")
})
if (exists("FM_ALONE")) { if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green") }
