# tests/test_formulas.R  --  Each derived value RS shows, against a worked case: a published figure where there is one, else a
# case computed by hand from the cited formula (each check says which). Sources: the hub file Derived_values_references.md,
# section 6 (RS).   Rscript tests/test_formulas.R   (exit 1 on a failure; also run by run_tests.R)
# No published source, so only what the code does is checked: the GNSS limits (calibration defaults) and the flight QA flags.

if (!exists("check")) {
  Sys.setenv(RS_NO_RUN = "1")
  here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
  source(file.path(here, "rs_pipeline.R"), chdir = TRUE)
  fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
  FM_ALONE <- TRUE
}
say("derived values against worked cases")
local({
  eq <- function(a, b, tol) isTRUE(all(abs(as.numeric(a) - as.numeric(b)) <= tol))

  # ground sample distance (McGlone 2013; Pix4D documentation): GSD = sensor width x height / (focal length x image width).
  # DJI's Phantom 4 RTK specification: 1" sensor 13.2 mm wide, 8.8 mm lens, 5472 px, GSD 2.74 cm/px at 100 m.
  d <- data.table(flight_id = "f", source_id = "s", folder = "a", t = as.POSIXct("2024-06-21 18:00:00", tz = "UTC") + c(0, 2, 4), lat = 61.3 + c(0, 1, 2) * 2e-4, lon = -121.3,
                  sensor_w_mm = 13.2, alt_rel = 100, focal_mm = 8.8, px_x = 5472, px_y = 3648, speed_x = 10, speed_y = 0, exposure_s = 1 / 1000, primary = TRUE)
  m <- flight_metrics(d)
  check(eq(d$gsd_cm, 2.74, 0.005) && eq(m$gsd_cm_median, 2.74, 0.005), "GSD reproduces DJI's Phantom 4 RTK figure: 2.74 cm/px at 100 m (13.2 mm sensor, 8.8 mm lens, 5472 px)")
  # motion blur (O'Connor et al. 2017; hand): 10 m/s x 1/1000 s over a 2.741 cm pixel is 0.365 px
  check(eq(m$blur_px_median, 10 * 0.001 / (13.2 * 100 / (8.8 * 5472)), 0.005) && eq(m$speed_ms_median, 10, 1e-9), "motion blur is speed x exposure over the GSD (hand: 0.36 px at 10 m/s, 1/1000 s, 2.74 cm)")
  # forward overlap (McGlone 2013; hand): images 22.24 m apart (2e-4 deg of latitude) on a footprint along the track of 0.02741 x 3648 = 100.0 m: 77.8 %
  check(eq(m$fwd_overlap_pct_median, 100 * (1 - (2e-4 * pi / 180 * 6371000) / (13.2 * 100 / (8.8 * 5472) * 3648)), 0.1), "forward overlap is 1 - spacing / footprint (hand: 22.24 m on 100.0 m, 77.8 %)")

  # sun elevation: at local solar noon on the June solstice the sun stands at 90 - latitude + 23.44 degrees (52.1 at 61.3 N);
  # the code's approximation is to about a degree (gap 6 in the references file)
  noon <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC") + 121.3 / 15 * 3600
  check(eq(sun_elevation_deg(noon, 61.3, -121.3), 90 - 61.3 + 23.44, 1), "sun elevation at solar noon on the June solstice is 90 - latitude + 23.44 (52.1 degrees at 61.3 N), within a degree")

  # checkpoint accuracy (ASPRS 2023, Edition 2; hand): RMSE_H = sqrt(RMSE_x^2 + RMSE_y^2), RMSE_3D = sqrt(RMSE_H^2 + RMSE_V^2),
  # the survey's own error in quadrature. dx +-3 cm, dy +-4 cm, dz 2, 4, 2, 4 cm: RMSE_x 3, RMSE_y 4, RMSE_H 5, RMSE_V 3.162, 3D 5.916
  cp <- data.table(id = 1:4, x_survey = 0, y_survey = 0, z_survey = 0, x_product = c(0.03, -0.03, 0.03, -0.03), y_product = c(0.04, -0.04, -0.04, 0.04), z_product = c(0.02, 0.04, 0.02, 0.04))
  a <- checkpoint_accuracy(cp)$summary
  check(eq(a$rmse_x_cm, 3, 1e-9) && eq(a$rmse_y_cm, 4, 1e-9) && eq(a$rmse_h_cm, 5, 1e-9) && eq(a$rmse_v_cm, 3.2, 1e-9) && eq(a$rmse_3d_cm, 5.9, 1e-9),
        "checkpoint RMSE per ASPRS Edition 2 (hand: RMSE_x 3, RMSE_y 4, RMSE_H 5, RMSE_V 3.2, RMSE_3D 5.9 cm)")
  check(eq(checkpoint_accuracy(cp, 0.03, 0)$summary$rmse_h_cm, sqrt(0.05^2 + 0.03^2) * 100, 0.05), "a checkpoint survey's own error is added in quadrature (hand: 5 and 3 cm give 5.8 cm)")
  check(isTRUE(a$vertical_bias) && grepl("Only 4 checkpoints", paste(checkpoint_accuracy(cp)$notes, collapse = " ")),
        "a mean vertical error of 3 cm over a standard error of 0.58 cm is a vertical bias (James & Robson 2014); fewer than 30 checkpoints is said")

  # GNSS: share of fixed positions (hand: 3 of 4), and a base set by averaging and not post-processed is flagged
  g <- data.table(device = "Emlid", source_file = "a.csv", point = paste0("P", 1:4), code = "", solution = c("FIX", "FIX", "FLOAT", "FIX"), rms_h = 0.01, rms_v = 0.02,
                  pdop = 1.2, hdop = 0.8, n_sat = 20, age_s = 1, baseline_m = 500, correction = "NTRIP")
  check(gnss_quality(g)$fixed_pct == 75, "share of fixed positions (hand: 3 of 4 is 75 %)")
  bc <- gnss_base_check(g, data.table(file = "a.csv", base = "B1", method = "single (averaged)", post_processed = "no", service = "", notes = ""))
  check(nrow(bc) == 1 && (bc$base_check == "flag" || !"single (averaged)" %in% names(BASE_LIMITS)), "a base set by averaging and not post-processed is flagged (its absolute position is metres off)")

  # GPS time to UTC (IERS Bulletin C, IS-GPS-200): DJI's XMP UTCAtExposure is GPS time. The sample M350 photo's is 18:15:47.141386,
  # the MRK's shot 3 is GPS week 2332, 584147.141386 s (the same instant); less the 18 leap seconds that is 18:15:29 UTC, and the
  # camera's EXIF clock, 12:15:27 local (UTC-6), agrees to 2 s
  im <- read_image_meta(file.path(here, "tests", "samples", "image_jpg__DJI_20240921121527_0003_D.JPG"))
  mk <- gps_to_utc(as.POSIXct("1980-01-06", tz = "UTC") + 2332 * 604800 + 584147.141386)
  check(identical(format(im$time_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2024-09-21 18:15:29") && abs(as.numeric(im$time_utc) - floor(as.numeric(mk))) < 1 &&
        abs(as.numeric(difftime(im$time_exif + 6 * 3600, im$time_utc, units = "secs"))) <= 2,
        "a DJI photo's time is its XMP GPS time less 18 leap seconds: 18:15:29 UTC, as the MRK gives it, and within 2 s of the EXIF clock at UTC-6")
  rx <- read_rinex(file.path(here, "tests", "samples", "rinex__rnx_bas.26O")); gl <- tempfile(fileext = ".26O")
  rl <- readLines(file.path(here, "tests", "samples", "rinex__rnx_bas.26O"), warn = FALSE); writeLines(c(sub("^(.{40}).", "\\1R", rl[1]), rl[-1]), gl)   # marked GLONASS only
  check(identical(rx$obs_start_utc, "2026-09-01 18:08:27") && identical(read_rinex(gl)$obs_start_utc, "2026-09-01 18:08:45"),
        "a RINEX file's epochs in GPS time less 18 leap seconds (sample base file: 18:08:45 GPS is 18:08:27 UTC); a GLONASS-only file's are left as written")
  unlink(gl)
  check(as.numeric(as.POSIXct("2016-12-31 12:00:00", tz = "UTC") - gps_to_utc(as.POSIXct("2016-12-31 12:00:00", tz = "UTC")), units = "secs") == 17 &&
        as.numeric(as.POSIXct("2017-01-02", tz = "UTC") - gps_to_utc(as.POSIXct("2017-01-02", tz = "UTC")), units = "secs") == 18, "GPS - UTC is 17 s in 2016 and 18 s from 2017")

  # UTM (core convert.R, used for GNSS files and GIS layers): on the central meridian at the equator E = 500000, N = 0; at 45 N on
  # it N = 0.9996 x the WGS84 meridian arc of 4984944.38 m = 4982950.40 m; a point goes there and back unchanged (hand)
  # photo footprint (pinhole camera; McGlone 2013, Wolf et al. 2014; hand): across = 13.2 mm x 100 m / 8.8 mm = 150 m, along
  # 150 x 3648 / 5472 = 100 m; three photos 22.24 m apart on one line cover 100 + 2 x 22.24 = 144.5 m along by 150 m
  fp <- flight_footprint(d[, `:=`(alt = NA_real_, height = 100, model = "FC6310R", file = c("a", "b", "c"))], cell = 0.5)
  check(eq(fp$summary$fp_width_m, 150, 1e-9) && eq(fp$summary$fp_area_m2, 150 * (100 + 2 * 2e-4 * pi / 180 * FP_R), 150),
        "photo footprint: 150 m across (13.2 mm x 100 m / 8.8 mm), 100 m along; three photos 22.24 m apart cover 150 x 144.5 m (hand, within the outline's one-cell edge)")
  # each flight's footprint is cached (R/footprints.R): the same inputs are not traced again, a moved photo is
  fpc <- tempfile(fileext = ".rds"); n <- 0L; of <- flight_footprint
  assign("flight_footprint", function(...) { n <<- n + 1L; of(...) }, envir = globalenv())
  im <- copy(d)[, `:=`(file = c("a", "b", "c"), rel = c("a", "b", "c"), model = "FC6310R", alt = NA_real_)]
  b1 <- footprints_build(copy(im), data.table(flight_id = "f"), cache_file = fpc); b2 <- footprints_build(copy(im), data.table(flight_id = "f"), cache_file = fpc); n2 <- n
  im2 <- copy(im)[2, lon := lon + 1e-3]; b3 <- footprints_build(copy(im2), data.table(flight_id = "f"), cache_file = fpc); b4 <- footprints_build(copy(im2), data.table(flight_id = "f"))
  assign("flight_footprint", of, envir = globalenv())
  check(n2 == 1L && n == 3L && identical(b1$outlines, b2$outlines) && identical(b3$outlines, b4$outlines) && !identical(b3$outlines, b1$outlines),
        "footprints: a flight whose photos have not changed is not traced again; one whose photos moved is, and matches tracing without the cache")
  # LiDAR swath (Baltsavias 1999; hand): 2 x 60 m x tan(70 / 2 deg) = 84.03 m for the Zenmuse L2 at 60 m
  ld <- data.table(lat = 61.3 + (0:10) * 1e-4, lon = -121.3, t = as.POSIXct("2026-09-01 18:00:00", tz = "UTC") + (0:10), height = 60, file = letters[1:11])
  fl2 <- flight_footprint(ld, LIDAR_FOV_DEG[["L2"]], cell = 0.5)
  check(eq(fl2$summary$fp_width_m, 84.02, 0.01) && fl2$summary$fp_kind == "lidar", "LiDAR swath is 2 x height x tan(FOV / 2) (hand: 84.02 m at 60 m with the L2's 70 deg)")
  # one footprint per year (hand): two 100 m squares overlapping by half cover 15000 m2: 10000 under one flight, 5000 under two;
  # their footprints sum to 20000 m2. A 10 m square apart (100 m2, under FOOTPRINT_MIN_M2) is dropped; a 40 m hole is kept.
  sq <- function(id, x0, y0, s, part = 1L, ring = 0L) { p <- fp_ll(x0 + c(0, s, s, 0, 0), y0 + c(0, 0, s, s, 0), 61.3, -121.3)
    data.table(flight_id = id, kind = "optical", part = part, ring = ring, line = paste(sprintf("%.9f %.9f", p$lat, p$lon), collapse = ";")) }
  u <- footprint_union(rbind(sq("a", 0, 0, 100), sq("b", 50, 0, 100), sq("c", 500, 500, 10)), cell = 0.5)
  check(eq(u$area_m2, 15000, 150) && eq(u$by_n[n == 1, m2], 10000, 150) && eq(u$by_n[n == 2, m2], 5000, 100) && u$parts == 1L && eq(u$sum_m2, 20100, 1),
        "year footprint: two half-overlapping 100 m squares cover 15000 m2 (10000 under one flight, 5000 under two); a 100 m2 piece is dropped (hand)")
  h <- footprint_union(rbind(sq("a", 0, 0, 100), sq("a", 30, 30, 40, ring = 1L)), cell = 0.5)
  check(h$holes == 1L && eq(h$area_m2, 10000 - 1600, 150), "a flight's hole stays a hole in the year footprint (hand: 10000 - 1600 m2)")
  # take-off ground from an eBee log's track (EP-01-23794_0345, 2026): the first 3 heights are 0.35, 0.24, 0.06 m before the GNSS
  # fix, then 272.99 m on the ground; the take-off is the median of the first 5 fixed heights, not of 3 zeros and 2 heights
  check(eq(fp_takeoff(c(0.35, 0.24, 0.06, 272.99, 272.97, 273.01, 273.00, 272.98, 280, 300)), 272.99, 1e-9) && eq(fp_takeoff(c(0.4, -0.2, 0.1, 0.3, 0.2, 0.0)), 0.2, 1e-9),
        "take-off height leaves out the track's heights before the GNSS fix (hand: 272.99 m, not 0.35 m); ground at the ellipsoid is kept")
  # ellipsoidal height from ECEF (WGS 84; Bowring 1976), against h iterated to convergence by hand: 272.6319 m
  check(eq(ecef_height(c(-1595452.6658, -2624073.1951, 5571684.6973)), 272.6319, 0.001), "height above the WGS 84 ellipsoid from a RINEX APPROX POSITION XYZ (hand: 272.6319 m)")
  u0 <- ll_to_utm(-123, 0, 10); u45 <- ll_to_utm(-123, 45, 10); back <- utm_to_ll(ll_to_utm(-121.3, 61.3, 10)[[1]], ll_to_utm(-121.3, 61.3, 10)[[2]], 10)
  check(eq(u0[[1]], 5e5, 1e-3) && eq(u0[[2]], 0, 1e-3) && eq(u45[[2]], 0.9996 * 4984944.378, 0.05) && eq(back[1], 61.3, 1e-7) && eq(back[2], -121.3, 1e-7),
        "UTM: 500000 E on the central meridian, N = 0.9996 x the meridian arc at 45 N (4982950.40 m), and back to the same latitude and longitude")
})
if (exists("FM_ALONE")) { if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green") }
