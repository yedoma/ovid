# tests/run_tests.R  --  Checks for the parts of Ovid Core // WLR that are easy to get wrong.
#   Rscript tests/run_tests.R                  (from the ovid_WLR folder)
# Set WLR_TEST_DATA to a folder laid out like the real data (2025/, baro/2025/) plus WLR_TEST_METADATA to a workbook
# folder to also run the whole pipeline end to end.

invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
Sys.setenv(WLR_NO_RUN = "1", OVID_RUN_SYNC = "1")   # app runs happen in this process (core runner.R), so a test sees their result at once
here <- if (file.exists("wlr_pipeline.R")) "." else ".."
source(file.path(here, "wlr_pipeline.R"))
audit_reset()

n_ok <- 0L; n_bad <- 0L
check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
near <- function(a, b, tol = 1e-6) isTRUE(all(abs(a - b) < tol))

cat("time\n")
check("GMT-07:00 is -7", parse_offset("Date Time, GMT-07:00") == -7)
check("UTC-06:00 is -6", parse_offset("2026-09-01 12:55:22.8 UTC-06:00") == -6)
check("Mountain Daylight Time is -6", parse_offset("Mountain Daylight Time") == -6)
check("no zone is NA", is.na(parse_offset("Date Time")))
t <- as.POSIXct("2025-06-01 12:00:00", tz = "UTC")
check("UTC-6 clock 12:00 is 11:00 at UTC-7", to_base(t, -6) == as.POSIXct("2025-06-01 11:00:00", tz = "UTC"))
check("no zone stays as it is", to_base(t, NA) == t)
check("HOBO dates read", parse_dt("09/03/24 01:00:00 PM") == as.POSIXct("2024-09-03 13:00:00", tz = "UTC"))
check("Solinst dates read", parse_dt("9/29/2023 11:00:00 am") == as.POSIXct("2023-09-29 11:00:00", tz = "UTC"))

cat("serials\n")
check("007 prefix matches", serial_match("72028253", "2028253") == "prefix")
check("leading zeros dropped", norm_serial("0020623546") == "20623546")
check("different serials do not match", serial_match("2028253", "2028257") == "none")
cs <- canonical_serials(c("72028253", "2028253", "10097942"))
check("prefixed serial becomes the short one", cs("72028253") == "2028253" && cs("10097942") == "10097942")

cat("units\n")
cfg <- CFG
check("psi to kPa", near(to_kpa(1, "psi", cfg), 6.894757))
check("Solinst m to kPa uses the setting", near(to_kpa(10, "m", modifyList(cfg, list(solinst_kpa_per_m = 9.8))), 98))
check("Solinst Barologger LEVEL gets its 9.5 m back", near((0.45 + solinst_baro_offset(c(0.4, 0.45, 0.5), "m", cfg)) * 9.80665, 97.57, 0.05))
check("Solinst Barologger in cm gets 950 cm", near(solinst_baro_offset(45, "cm", cfg), 950))
check("a barologger already in kPa is left alone", solinst_baro_offset(c(97, 98), "kPa", cfg) == 0 && solinst_baro_offset(c(10.2, 10.3), "m", cfg) == 0)
sr <- span_rows(data.table(serial = c("2028260", "2028260"), site = "", start = c("2023-03-01", "3/5/2023 13:30"), end = c("2023-03-31", ""), parameter = ""), "2028260", "x")
check("set-up dates without a time are read, and an end date covers its day", nrow(sr) == 2 && fmt_ts(sr$a[1]) == "2023-03-01 00:00:00" && fmt_ts(sr$b[1]) == "2023-03-31 23:59:59" && fmt_ts(sr$a[2]) == "2023-03-05 13:30:00" && is.infinite(as.numeric(sr$b[2])))
check("1 m of water at 1000 kg/m3", near(kpa_to_m(9.80665, NULL, cfg), 1))
check("density at 4 C is the maximum", water_density(4) > water_density(0) && water_density(4) > water_density(10))
check("temperature density gives a little more depth at 20 C", kpa_to_m(9.80665, 20, modifyList(cfg, list(density_mode = "temperature"))) > 1)
check("specific conductance at 25 C equals EC", near(spc25(500, 25), 500))

cat("barometric\n")
bt <- as.POSIXct("2025-01-01", tz = "UTC") + c(0, 1800, 3600, 3 * 86400)
bv <- c(100, 101, 102, 99)
v <- baro_at(bt, bv, as.POSIXct("2025-01-01", tz = "UTC") + c(900, 2700, 86400), 3 * 3600)
check("interpolates between readings", near(v[1:2], c(100.5, 101.5)))
check("leaves a hole across a long gap", is.na(v[3]))
wt <- as.POSIXct("2025-01-01", tz = "UTC") + c(0, 1800, 86400)
ch <- list(list(series = data.table(TIMESTAMP = bt[1:3], baro_kpa = bv[1:3]), role = "primary", name = "P"),
           list(series = data.table(TIMESTAMP = as.POSIXct("2025-01-01", tz = "UTC") + c(0, 86400), baro_kpa = 98), role = "secondary", name = "S"))
cc <- compensate_chain(wt, ch, cfg)
check("secondary fills the primary's gap", cc$baro_source[3] == "secondary" && cc$baro_source[1] == "primary" && cc$baro_kpa[3] == 98)
ft <- as.POSIXct("2025-01-01", tz = "UTC") + (0:99) * 1800
ref <- data.table(TIMESTAMP = ft[1:60], baro_kpa = 100)
ch2 <- list(list(series = ref, role = "primary", name = "P"), list(series = data.table(TIMESTAMP = ft, baro_kpa = 99.5), role = "fill", name = "F"))
cc2 <- compensate_chain(ft, ch2, cfg, ref = ref)
check("a fill-in station is shifted onto the primary's level", near(cc2$baro_kpa[90], 100) && cc2$baro_source[90] == "fill" && near(cc2$baro_shift[90], 0.5))
check("season year turns over after mid-September", season_year(as.POSIXct("2024-09-20", tz = "UTC")) == 2025 && season_year(as.POSIXct("2024-09-10", tz = "UTC")) == 2024)
check("water year runs 1 October to 30 September by default", water_year_of(as.Date("2024-09-30")) == 2024 && water_year_of(as.Date("2024-10-01")) == 2025 &&
  water_year_of(as.POSIXct("2024-10-01 00:00:00", tz = "UTC")) == 2025 && water_year_first_day(2025) == as.POSIXct("2024-10-01", tz = "UTC"))
check("water year start can be set; a bad one falls back to 1 October", water_year_of(as.Date("2024-09-20"), "09-15") == 2025 &&
  water_year_of(as.Date("2024-06-30"), "01-01") == 2024 && water_year_of(as.Date("2024-10-02"), "13-40") == 2025 && water_year_label("9-1") == "1 September")

cat("time-zone table\n")
tz <- data.table(serial = c("2028253", "", ""), model = c("", "Levelogger", ""), file_pattern = c("", "", "2019/"), start = c("", "", ""),
                 end = c("", "", ""), offset_hours = c("-6", "-8", "-5"), reason = c("one logger", "a model", "a year"))
t19 <- as.POSIXct("2019-07-01", tz = "UTC")
check("a serial row matches its logger", zone_for("2028253", "2020/a.xle", NA, tz, -7, stated_wins = TRUE)$offset == -6)
check("a model row matches part of the name", zone_for("999", "2020/b.xle", NA, tz, -7, model = "Levelogger Edge", stated_wins = TRUE)$offset == -8)
check("a folder row matches by path", zone_for("999", "baro/2019/c.xle", NA, tz, -7, stated_wins = TRUE)$offset == -5)
check("a zone stated in the file wins in WLR", zone_for("2028253", "x.csv", -6.5, tz, -7, stated_wins = TRUE)$why == "stated in the file")
check("no match is the base zone", zone_for("999", "2020/d.xle", NA, tz, -7, stated_wins = TRUE)$offset == -7)
tzd <- data.table(serial = "", model = "", file_pattern = "", start = "2019-01-01", end = "2019-12-31", offset_hours = "-6", reason = "")
check("a date row matches a file starting in its span", zone_for("1", "e.xle", NA, tzd, -7, first_ts = as.POSIXct("2019-12-31 23:00", tz = "UTC"), stated_wins = TRUE)$offset == -6)
check("a date row skips a file starting after it", zone_for("1", "e.xle", NA, tzd, -7, first_ts = as.POSIXct("2020-01-01 00:30", tz = "UTC"), stated_wins = TRUE)$offset == -7)
check("a bad row is reported", nrow(tz_check(data.table(serial = "1", model = "", file_pattern = "(", start = "soon", end = "", offset_hours = "x", reason = ""))) == 3)
mz <- data.table(serial = c("2028253", "2028253", "555"), rel = c("a.xle", "b.csv", "c.xle"), model = NA_character_, offset_hours = c(NA, -6, NA),
                 first_ts = t19, tz_override = c(NA, NA, -8))
mz <- apply_zone_table(mz, tz, canon = function(x) sub("^7(\\d{7})$", "\\1", x))
check("a typed 7-prefixed serial is matched through canon", apply_zone_table(copy(mz)[1], data.table(serial = "72028253", model = "", file_pattern = "", start = "", end = "", offset_hours = "-5", reason = ""), canon = function(x) sub("^7(\\d{7})$", "\\1", x))$zone == -5)
check("the table zone is used only where the file states none and no hand rule applies", identical(mz$tz_table, c(-6, NA, NA)) && identical(mz$zone, c(-6, -6, -8)))
old_b <- data.table(TIMESTAMP = as.POSIXct("2023-09-01", tz = "UTC") + (0:47) * 3600, baro_kpa = 100.1)
new_b <- data.table(TIMESTAMP = as.POSIXct("2023-09-02", tz = "UTC") + (0:47) * 3600, baro_kpa = 100.0)
st <- stitch_baro_station("Camp", c("A", "B"), list(A = old_b, B = new_b))
check("a station stitches two barologgers, the newer winning the overlap", nrow(st$series) == 72 && all(st$series[TIMESTAMP >= as.POSIXct("2023-09-02", tz = "UTC"), baro_serial] == "B"))
check("the seam between them is reported with its difference", nrow(st$seams) == 1 && near(abs(st$seams$difference_kpa), 0.1, 1e-3))

cat("deployments\n")
md <- data.table(serial = "1", site = c("A", "A", "B"), year = 2023:2025, type = "WLR", logger = "HOBO",
                 visit_time = as.POSIXct(c("2023-09-01 12:00", NA, "2025-09-01 12:00"), tz = "UTC"), visit_time_known = TRUE,
                 install_date = as.Date(NA), T_ST = c(1.5, NA, 1.6), T_ST_raw = "", tst_from_toc = FALSE, tst_problem = "",
                 easting = 1, northing = 1, land_cover = "", alias = "")
d <- build_deployments(md)
check("a year with no visit date ends mid-September", d[year == 2024, end] == as.POSIXct("2024-09-15 12:00:00", tz = "UTC"))
check("next year starts where it ended", d[year == 2025, start] == d[year == 2024, end])
check("the logger moved to site B in 2025", d[year == 2025, site] == "B")
ov <- data.table(serial = "1", year = "2024", site = "", deployed = "", removed = "", T_ST_m = "1.55", T_ST_from_year = "", reference = "", reason = "", author = "")
check("a typed hang length replaces a missing one", build_deployments(md, ov)[year == 2024, T_ST] == 1.55)
ov$T_ST_m <- ""; ov$T_ST_from_year <- "2023"
check("a hang length can come from another year", build_deployments(md, ov)[year == 2024, T_ST] == 1.5)

cat("datum\n")
dep <- data.table(deployment = "x", site = "A", serial = "1", year = 2025, T_ST = 1.6, tst_from_toc = FALSE)
vis <- data.table(visit_time = as.POSIXct("2025-09-01 12:00", tz = "UTC"), visit_time_known = TRUE, T_water = 0.6, T_TOC = 0.2, TOC_water = 0.4, T_ST = 1.6)
rec <- data.table(TIMESTAMP = as.POSIXct("2025-09-01 12:00", tz = "UTC") - c(3, 2, 1) * 1800, depth_m = 1.0)
chk <- manual_check(dep, vis, rec, 0)
check("tape and logger agree when they should", near(chk$h_tape, 1) && near(chk$difference_m, 0))
u <- ll_to_utm(-120.1078635, 60.10920954, 10)
check("lat/lon to UTM 10 lands near the workbook coordinates", abs(u[[1]] - 660750) < 5000 && abs(u[[2]] - 6667092) < 5000)

cat("audit\n")
audit_reset()
x <- data.table(TIMESTAMP = as.POSIXct("2025-01-01", tz = "UTC") + 0:9 * 1800, logger_id = "1", site = "A", pres_kpa = c(100, 101, 300, 102, 103, 104, 105, 106, 107, 108))
x <- apply_limits(x, default_qc(), "water")
check("a value above the limit is removed and logged", is.na(x$pres_kpa[3]) && nrow(audit_table()[rule == "range_max"]) == 1)
FLAGS$list <- list(); y <- data.table(TIMESTAMP = as.POSIXct("2025-01-01", tz = "UTC") + 0:9 * 1800, logger_id = "1", site = "A", depth_m = c(1, 1, -1, 1, 1, 1, 1, 1, 1, 1))
dq <- default_qc(); y <- apply_limits(y, dq, "water"); fl <- qc_flags_table(FLAGS$list, data.table(), "WLR")
check("default rules carry rule ids, and a flag names its rule with its WMO code", all(grepl("^Q-[0-9]+$", dq$rule_id)) && !anyDuplicated(dq$rule_id) &&
      fl[check == "range_min", rule_id] == dq[applies_to == "water" & parameter == "depth_m", rule_id] && fl[check == "range_min", qc_code] == 3L && fl$app[1] == "WLR")
qd <- file.path(tempdir(), "rid_test.csv"); fwrite(dq[, rule_id := NULL][, rule_id := c("Q-07", "", "Q-07", rep("", 5))], qd); rr <- qc_read_rules(qd, QC_COLS)
check("reading a rule table keeps the ids it has, gives the rest new ones and saves them", rr$rule_id[1] == "Q-07" && !anyDuplicated(rr$rule_id) && identical(read_cfg(qd)$rule_id, rr$rule_id))
x <- remove_rows(x, 1:2, "deploy", "outside_deployment", "test", detail = "summary")
check("dropped records are logged with their count", sum(audit_table()[rule == "outside_deployment", n]) == 2 && nrow(x) == 8)

cat("clock drift\n")
t0c <- as.POSIXct("2025-06-01 00:00", tz = "UTC"); dtc <- data.table(logger_id = "123", TIMESTAMP = t0c + 0:10 * 3600, v = 1:11)
tcd <- data.table(serial = "123", start = "2025-06-01 00:00", end = "2025-06-01 10:00", shift_minutes = "-10", reason = "4 min fast at download", author = "t", mode = "drift")
dc <- apply_time_corrections(copy(dtc), tcd)
check("a clock drift is spread over the span: none at its start, all of it at its end", dc$TIMESTAMP[1] == t0c && dc$TIMESTAMP[11] == t0c + 10 * 3600 - 600 && dc$TIMESTAMP[6] == t0c + 5 * 3600 - 300)
dc2 <- apply_time_corrections(copy(dtc), tcd[, mode := ""])
check("without the drift mode the whole span is moved, as before", all(dc2$TIMESTAMP == dtc$TIMESTAMP - 600))

cat("heights and frames\n")
gf <- tempfile(fileext = ".csv")
writeLines(c("Name,Location,Elevation,Ellipsoidal height,Elevation RMS,Solution status,Averaging start,Longitude,Latitude,CS name",
             "W1,T,250.000,234.500,0.01,FIX,2025-09-01 12:00:00.0 UTC-06:00,-120.1,60.1,NAD83(CSRS) / UTM zone 10N + CGVD2013(CGG2013a) height",
             "W2,T,,235.000,0.01,FIX,2025-09-01 12:10:00.0 UTC-06:00,-120.1,60.1,NAD83(CSRS) / UTM zone 10N + CGVD2013(CGG2013a) height"), gf)
gd <- read_dgps_file(gf)
check("an ellipsoidal height alone becomes an elevation with the geoid separation of the same survey (H = h - N)",
      isTRUE(all.equal(gd[gps_name == "W2", z], 250.5)) && grepl("h - N", gd[gps_name == "W2", z_kind]) && gd[gps_name == "W1", z_kind] == "elevation")
dpt <- data.table(serial = "1", site = "S", year = 2025L, tst_from_toc = FALSE, end = as.POSIXct("2025-09-01", tz = "UTC"))
pts <- data.table(gps_name = c("A", "B"), code = "T", z = c(250, 235), z_kind = c("elevation", "ellipsoidal"), rms = 0.01, time = as.POSIXct(c("2025-08-01", "2025-08-31"), tz = "UTC"),
                  source = "x", cs_name = c("NAD83(CSRS) / UTM 10N", "WGS 84"))
d1 <- datum_for(dpt, pts, data.table(site = "S", gps_name = c("A", "B"), code = "T"))
check("an ellipsoidal height is never mixed with elevations at a site, even when it is the nearest survey", d1$z_ref == 250 && d1$z_kind == "elevation")
tm <- data.table(site = "S2", code = "T", elevation_m = "230.0", date = "2025-08-01", reason = "PPP", author = "t", height_type = "ellipsoidal", frame = "NAD83(CSRS) v8", epoch = "2010.0", geoid_model = "CGG2013a", geoid_N_m = "-16.2")
d2 <- datum_for(copy(dpt)[, site := "S2"], data.table(), data.table(), tm)
check("a typed ellipsoidal height with its geoid separation is used as H = h - N, its frame and epoch kept", isTRUE(all.equal(d2$z_ref, 246.2)) && grepl("NAD83\\(CSRS\\) v8 epoch 2010.0 CGG2013a", d2$z_ref_source))
check("the frame of a coordinate-system label", identical(frame_of(c("NAD83(CSRS) / UTM zone 10N", "WGS 84", "ITRF2014", "")), c("NAD83(CSRS)", "WGS 84", "ITRF2014", "")))

cat("removals\n")
audit_reset()
y <- data.table(TIMESTAMP = as.POSIXct("2025-06-01", tz = "UTC") + 0:9 * 3600, logger_id = "7", site = "A", pres_kpa = 100 + 0:9, depth_m = c(0, 0, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2))
man <- data.table(serial = c("7", "7", "8"), site = "", start = c("2025-06-01 00:00", "2025-06-01 05:00", "2025-06-01 00:00"), end = c("2025-06-01 01:00", "2025-06-01 09:00", "2025-06-01 09:00"),
                  parameter = c("records", "pres_kpa", "records"), min_value = c("", "107", ""), max_value = c("", "108", ""), reason = "t", author = "")
y2 <- apply_manual(copy(y), man, NULL, "water")
check("records dropped by hand leave the record and are logged", nrow(y2) == 8 && sum(audit_table()[rule == "manual_records", n]) == 2)
check("a value band removes only the values inside it", sum(is.na(y2$pres_kpa)) == 2 && all(!is.na(y2[pres_kpa > 108 | pres_kpa < 107, pres_kpa])))
check("a removal for another logger leaves this one alone", nrow(apply_manual(copy(y), man[3], NULL, "water")) == 10)
ar <- detect_air_runs(y, modifyList(CFG, list(out_of_water_m = 0.03, air_max_hours = 72)))
check("readings in the air at the start are found", nrow(ar) == 1 && ar$where == "start" && ar$records == 2)
check("one level limit serves the in-the-air check: an old air_threshold_m is ignored",
      identical(detect_air_runs(y, modifyList(CFG, list(out_of_water_m = 0.03, air_threshold_m = 99, air_max_hours = 72))), ar) && is.null(CFG$air_threshold_m))
check("with the in-air switch off, the in-air limit is its own (in cm), and the out-of-water limit no longer decides",
      identical(detect_air_runs(y, modifyList(CFG, list(out_of_water_m = 99, air_same_as_oow = FALSE, air_threshold_cm = 3, air_max_hours = 72))), ar) &&
      nrow(detect_air_runs(y, modifyList(CFG, list(out_of_water_m = 0.03, air_same_as_oow = FALSE, air_threshold_cm = 0, air_max_hours = 72)))) == 0)
check("barometric interpolation across gaps can be switched off (then only readings at a barometric time are compensated)",
      baro_gap_s(as.POSIXct("2025-01-01", tz = "UTC") + 900 * 0:9, list(baro_max_gap_h = 3, baro_interp = FALSE)) == 0 &&
      baro_gap_s(as.POSIXct("2025-01-01", tz = "UTC") + 900 * 0:9, list(baro_max_gap_h = 3)) == 3 * 3600 &&
      identical(baro_at(c(0, 3600), c(100, 101), c(0, 1800, 3600), 0), c(100, NA, 101)))
check("a long dry run is not taken for a launch", nrow(detect_air_runs(y, modifyList(CFG, list(air_max_hours = 1)))) == 0)
man_f <- data.table(path = c("a.csv", "b.csv"), name = c("a.csv", "b.csv"), year = 2025, kind = "water", format = "hobo_csv", status = "ok", use = TRUE, serial = "7", model = "",
                    md5 = "x", size = 1, first_ts = Sys.time(), last_ts = Sys.time(), rows = c(10L, 5L), pair = "", note = "")
fa <- file_audit(man_f, data.table(source_file = c("a.csv", "b.csv"), records = c(8L, 4L), used_in = "A"), data.table(source_file = c("a.csv", "b.csv"), level = "row", rule = "x", n = c(2L, 0L)))
check("the file audit reconciles read = in output + dropped", isTRUE(fa[file == "a.csv", reconciles]) && !isTRUE(fa[file == "b.csv", reconciles]))

cat("files read in parallel (core util.R par_map)\n")
local({ r <- par_map(as.list(1:40), function(x) if (x == 7) stop("bad file") else x * 2, workers = 4)
  s <- par_map(as.list(1:3), function(x) x, workers = 1)
  check("files read in parallel come back in order, and one that fails returns its error without stopping the others",
        length(r) == 40 && inherits(r[[7]], "error") && identical(unlist(r[-7]), (1:40)[-7] * 2) && identical(unlist(s), 1:3)) })
cat("uncertainty, barometric efficiency, casing, hydrology, approvals\n")
dep_u <- data.table(deployment = "d", site = "S", serial = "20012345", year = 2025L, z_ref = 250, z_ref_rms = 0.01)
du <- data.table(depth_m = rep(0.5, 100), baro_source = rep(c("primary", "fill"), c(75, 25)))
uu <- deployment_uncertainty(dep_u, du, data.table(difference_m = c(0.01, -0.01, 0.02)), "HOBO U20L-04", NULL, CFG)
check("the uncertainty adds its parts in quadrature and keeps the survey for the elevation", near(uu$depth_sigma_m, sqrt(0.004^2 + (0.75 * 0.005^2 + 0.25 * 0.02^2) + 0.001^2), 1e-4) &&
  near(uu$elev_sigma_m, sqrt(uu$dtw_sigma_m^2 + 0.01^2), 1e-4) && uu$tape_checks == 3 && grepl("U20L", uu$logger_accuracy_from))
check("each HOBO U20L range gets its own stated accuracy (Onset: -01 1.0 cm, -02 3.0 cm, -04 0.4 cm), and an unnamed U20L the cautious 1.0 cm",
      near(accuracy_for("1", "HOBO U20L-01 Water Level", NULL)$sigma, 0.01) && near(accuracy_for("1", "HOBO U20L-02 Water Level", NULL)$sigma, 0.03) &&
      near(accuracy_for("1", "HOBO U20L-04 Water Level", NULL)$sigma, 0.004) && near(accuracy_for("1", "HOBO U20L", NULL)$sigma, 0.01))
tb <- as.POSIXct("2025-06-01", tz = "UTC") + 0:(24 * 20 - 1) * 3600; set.seed(4); bk <- 100 + cumsum(rnorm(length(tb), 0, 0.03))
dconf <- data.table(TIMESTAMP = tb, baro_kpa = bk, depth_m = 2 - 0.4 * kpa_to_m(bk - 100, NULL, CFG) + rnorm(length(tb), 0, 1e-4))
dwt <- data.table(TIMESTAMP = tb, baro_kpa = bk, depth_m = 2 + rnorm(length(tb), 0, 1e-4))
e1 <- barometric_efficiency(dconf, CFG); e2 <- barometric_efficiency(dwt, CFG)
check("barometric efficiency: a confined well's 0.4 is found, a water-table well has none", near(e1$be_lag0, 0.4, 0.02) && grepl("^lasting", e1$verdict) && abs(e2$be_lag0) < 0.02 && grepl("^no", e2$verdict) &&
  is.null(barometric_efficiency(dwt[1:50], CFG)))
mdc <- data.table(site = "W1", year = 2023:2025, visit_time = as.POSIXct(c("2023-09-10", "2024-09-10", "2025-09-10"), tz = "UTC"), TOC_veg = c(0.50, 0.51, 0.56))
cmv <- casing_movement(mdc)
check("a casing that rises 5 cm against the ground between visits is reported, 1 cm is steady", nrow(cmv) == 2 && cmv$status[1] == "steady" && grepl("frost jacking", cmv$status[2]))
pts <- data.table(site = "W1", code = "T", time = as.POSIXct(c("2020-01-01", "2022-01-01"), tz = "UTC"), z = c(250, 249.8))
rs <- ref_series(pts, as.POSIXct(c("2019-01-01", "2021-01-01", "2023-01-01"), tz = "UTC"))
check("a reference point follows its surveys in time, held outside them", near(rs[1], 250) && near(rs[2], 249.9, 0.001) && near(rs[3], 249.8) && all(ref_series(pts[0], Sys.time(), 1) == 1))
check("the water table against the ground comes from the T-coupler to vegetation height, else from the ground survey",
  near(wt_ground_of(data.table(dtw_m = c(0.3, 0.6)), list(T_veg = 0.5)), c(0.2, -0.1)) && near(wt_ground_of(data.table(elev_m = 100.2), list(T_veg = NA), 100), 0.2))
dd5 <- rbind(data.table(site = "A", date = as.Date("2024-10-01") + 0:9, parameter = "wt_ground_m", mean = c(-0.1, 0.02, 0.05, 0.01, -0.02, 0.03, 0.04, 0.06, 0.07, -0.1), min = 0, max = 0),
             data.table(site = "A", date = as.Date("2024-10-01") + 0:9, parameter = "elev_m", mean = 100 + c(0, 0.1, 0.05, 0.08, 0.08, 0.1, 0.12, 0.12, 0.1, 0.1), min = 0, max = 0),
             data.table(site = "B", date = as.Date("2024-10-01") + 0:9, parameter = "elev_m", mean = 99.5, min = 0, max = 0))
hp <- hydroperiod(dd5)
gr <- gradients(dd5, data.table(pair = "AB", site_a = "A", site_b = "B", distance_m = "50", direction = "", notes = ""), data.table(site = c("A", "B"), elev_sigma_m = c(0.03, 0.04)))
rc <- wtf_recharge(dd5, data.table(site = "*", specific_yield = "0.3", notes = ""))
check("hydroperiod counts the days at or above the ground and the longest run", hp$days_flooded == 7 && hp$longest_flooded_days == 4 && hp$days == 10)
check("the gradient between two wells is their head difference over the distance, with its uncertainty", near(gr$gradient[1], 0.01) && near(gr$gradient_sigma[1], 0.05 / 50, 1e-6))
check("recharge by water-table fluctuation is Sy times the sum of the rises", near(rc[site == "A", sum_of_rises_m], 0.17) && near(rc[site == "A", recharge_mm], 51) && rc[site == "B", recharge_mm] == 0)
ap <- data.table(site = "A", approved_through = "2024-12-31", by = "x", notes = "")
check("a record is approved up to its date and provisional after", identical(approval_of("A", as.Date(c("2024-06-01", "2025-01-02")), ap), c("approved", "provisional")) &&
  all(approval_of("B", Sys.Date(), ap) == "provisional"))
sgs <- summaries_of(dd5[site == "A"][, `:=`(min = mean, max = mean)], 0.9, data.table(site = "A", year = 2025L, depth_sigma_m = 0.01, dtw_sigma_m = 0.02, elev_sigma_m = 0.03))
check("a water year and its months carry the deployment's uncertainty", all(sgs$water_year[parameter == "elev_m", sigma_m] == 0.03) && all(sgs$monthly[parameter == "wt_ground_m", sigma_m] == 0.02))
check("summaries, hydroperiod and recharge follow the water year start", all(summaries_of(dd5, 0.9)$water_year$water_year == 2025) &&
  all(summaries_of(dd5, 0.9, NULL, "01-01")$water_year$water_year == 2024) && all(hydroperiod(dd5, 0.9, "01-01")$water_year == 2024) &&
  all(wtf_recharge(dd5, data.table(site = "*", specific_yield = "0.3", notes = ""), "01-01")$water_year == 2024) && !water_year_start_valid("10/01"))

# ---- end to end, when sample data is available ---------------------------------------------------------------
if (nzchar(Sys.getenv("WLR_TEST_DATA"))) {
  cat("end to end\n")
  c2 <- modifyList(CFG, list(raw_dirs = Sys.getenv("WLR_TEST_DATA"), metadata_dir = Sys.getenv("WLR_TEST_METADATA"), dgps_dir = Sys.getenv("WLR_TEST_DGPS"),
                             project_dir = file.path(tempdir(), "wlr_test"), make_plots = FALSE))
  r <- tryCatch(run_pipeline(c2), error = function(e) { cat("  stopped:", conditionMessage(e), "\n"); NULL })
  check("the pipeline runs and every record reconciles", !is.null(r))
  if (!is.null(r)) {
    s <- r$summary[kind == "water"]
    odd <- s[!(depth_median > -0.5 & depth_median < 5)]
    if (nrow(odd)) cat("   ", paste(odd$deployment, odd$depth_median), sep = "\n    ")
    check("depths are plausible (median between -0.5 and 5 m), or the deployment is reported as reading below the air", all(is.na(odd$below_air_share) | odd$below_air_share > CFG$below_air_share))
    check("most deployments are compensated", mean(s$baro_primary_share > 0.5, na.rm = TRUE) > 0.5)
  }
  check("the run writes output/report.html", file.exists(file.path(c2$project_dir, "output", "report.html")))
  check("run and site reports are written in both formats", { f <- write_wlr_reports(c2$project_dir, file.path(tempdir(), "wlr_rep"), sites = unique(read_out(c2$project_dir, "summary.csv")$site)[1])
    length(f) == 4 && all(file.exists(f)) })
}

check("monthly and water-year summaries come from complete days", {
  d <- data.table(site = "S", TIMESTAMP = as.POSIXct("2023-09-01", tz = "UTC") + seq(0, by = 1800, length.out = 48 * 40))
  d[, `:=`(depth_m = 0.5 + as.numeric(TIMESTAMP - TIMESTAMP[1]) / 86400 / 100, temp_c = 4)]
  d[TIMESTAMP >= as.POSIXct("2023-09-05 06:00", tz = "UTC") & TIMESTAMP < as.POSIXct("2023-09-05 18:00", tz = "UTC"), depth_m := NA]
  dl <- daily_of(d); sm <- summaries_of(dl)
  !as.Date("2023-09-05") %in% dl[parameter == "depth_m", date] && nrow(sm$water_year[parameter == "depth_m"]) == 2 &&
    sm$monthly[parameter == "temp_c" & month == 9, days] == 30 && isTRUE(sm$monthly[parameter == "temp_c" & month == 9, complete]) })
check("a manual measurement is set against the logger at the same time", {
  d <- data.table(site = "S", TIMESTAMP = as.POSIXct("2023-09-01", tz = "UTC") + seq(0, by = 1800, length.out = 10), depth_m = seq(1, 1.9, by = 0.1))
  m <- read_measurements(data.table(site = c("S", "S", "T"), time = c("2023-09-01 00:45", "2023-09-03 00:00", "2023-09-01 01:00"), parameter = "depth_m", value = c("1.10", "1", "1"), method = "", author = "", notes = ""))
  r <- compare_measurements(m, d, "123")
  nrow(r) == 1 && abs(r$logger - 1.15) < 1e-9 && abs(r$difference - 0.05) < 1e-9 })
check("a report renders as HTML and as PDF", { d <- rpt_doc("Test", "sub", list(rpt_cards(c(a = 1)), rpt_heading("T"), rpt_table(data.table(year = 2023, x = 1234.5)), rpt_text("note")))
  f <- report_write(d, file.path(tempdir(), "rpt_test"), c("html", "pdf")); h <- paste(readLines(f[1], warn = FALSE), collapse = "")
  length(f) == 2 && grepl("<table", h) && grepl(">2023<", h) && identical(readBin(f[2], "raw", 4), charToRaw("%PDF")) })

cat("corrupt time stamps (shared core)\n")
tg <- seq(as.POSIXct("2020-10-01", tz = "UTC"), by = 3600, length.out = 2000); junk <- as.POSIXct("2020-10-05 00:00:17", tz = "UTC") + (0:9) * 86400 + 7
tt <- c(tg[1:1500], junk, tg[1501:2000]); sr <- stray_records(tt)
check("readings stamped at random times among good ones are found", all(sr[1501:1510]) && !any(sr[-(1501:1510)]))
check("a reading written in order but off the grid is kept", !any(stray_records(c(tg[1:100], tg[100] + 1337, tg[101:200]))))
cat("a logger redeployed from water to baro\n")
rmd <- data.table(serial = c("559", "559", "559", "573"), site = c("Fen", "Fen", "Baro Ground", "Baro Ground"), year = c(2022L, 2024L, 2026L, 2024L),
                  type = c("WLR", "WLR", "Baro", "Baro"), logger = "HOBO", visit_time = as.POSIXct(c("2022-08-17 12:00", "2024-09-28 12:00", "2026-08-31 12:00", "2024-09-28 12:00"), tz = "UTC"),
                  visit_time_known = TRUE, install_date = as.Date(NA), T_ST = NA_real_, T_ST_raw = NA_character_, tst_from_toc = FALSE, tst_problem = "",
                  easting = NA_real_, northing = NA_real_, land_cover = "", alias = "")
sw <- redeployed_serials(rmd, c("559", "573"))
check("a serial typed WLR in some years and Baro in others is redeployed; a baro-only serial is not", identical(sw, "559"))
check("one in a baro folder stays a barologger", !length(redeployed_serials(rmd, c("559", "573"), folder_baro = "559")))
ww <- water_windows_of(rmd, NULL, sw); tq <- as.POSIXct(c("2023-06-01 00:00", "2024-09-28 11:00", "2025-06-01 00:00", "2026-09-30 00:00"), tz = "UTC")
check("it is a water logger inside its WLR deployments and a barologger after them", identical(!in_windows(ww, "559", tq), c(FALSE, FALSE, TRUE, TRUE)))
rmd2 <- rbind(rmd, rmd[3][, `:=`(serial = "904", logger = "HOBO")], rmd[2][, `:=`(serial = "904", logger = "Solinst")])
check("a Solinst WLR row for a HOBO barologger's short serial is set aside, not a redeployment", identical(rmd2$serial[role_conflicts(rmd2, redeployed_serials(rmd2, c("559", "573", "904")))], "904"))

fm <- data.table(path = "x/a.xle", rel = "a.xle", name = "a.xle", format = "solinst_xle", status = "no_serial", use = FALSE, serial = NA_character_, note = "", kind = "water")
fm <- apply_file_rules(fm, data.table(file = "a.xle", action = "serial", serial = "2034139", offset_hours = "", reason = "", author = ""))
check("a file without a serial is read once a rule gives it one", fm$status == "ok" && fm$use && fm$serial == "2034139")

cat("barologger offset and drift\n")
set.seed(1); th <- seq(as.POSIXct("2016-01-01", tz = "UTC"), by = 3600, length.out = 24 * 365 * 3)
air <- 97.5 + cumsum(rnorm(length(th), 0, 0.05)) * 0.02 + 0.3 * sin(2 * pi * as.numeric(th) / 86400)
noise <- as.numeric(stats::filter(rnorm(length(th), 0, 0.05), 0.9, method = "recursive"))
ref <- data.table(TIMESTAMP = th, baro_kpa = air + rnorm(length(th), 0, 0.08))
yrs <- (as.numeric(th) - as.numeric(th[1])) / 86400 / 365.2425
tg_off <- data.table(TIMESTAMP = th, baro_kpa = air + 3.19 + noise)
tg_dr  <- data.table(TIMESTAMP = th, baro_kpa = air + 0.5 + 0.3 * yrs + noise)
r1 <- baro_check(tg_off, ref); r2 <- baro_check(tg_dr, ref); r0 <- baro_check(data.table(TIMESTAMP = th, baro_kpa = air + noise), ref)
check("a constant offset is recovered with an interval that holds it", r1$ok && abs(r1$est$offset_kpa - 3.19) < 0.05 && r1$est$offset_ci_low <= 3.19 && r1$est$offset_ci_high >= 3.19 && r1$est$offset_material)
check("a constant offset is not taken for a drift", !r1$est$drift_material)
check("a drift of 0.3 kPa/yr is recovered and called material", r2$ok && abs(r2$est$drift_kpa_per_year - 0.3) < 0.03 && r2$est$drift_material)
check("two barologgers that agree show neither offset nor drift", r0$ok && !r0$est$offset_material && !r0$est$drift_material)
r6 <- baro_check(tg_off, ref[seq(1, .N, 6)])
check("a reference that records only every 6 hours still gives the offset", r6$ok && abs(r6$est$offset_kpa - 3.19) < 0.06)
cc <- baro_correction_at(r2$est, "offset_drift", th[c(1, length(th))])
check("the offset_drift correction follows the fitted line", abs(cc[2] - cc[1] - 0.3 * 3) < 0.1)
check("Theil-Sen recovers an exact line", isTRUE(all.equal(unname(theil_sen(1:20, 2 + 0.5 * (1:20), 0)), c(0.5, 2))))
xf <- file.path(tempdir(), "alt_test.xle")
writeLines(c('<?xml version="1.0" ?>', '<Body_xle><Instrument_info><Instrument_type>LT_Gold</Instrument_type><Model_number>M1.5</Model_number><Serial_number>1039555</Serial_number></Instrument_info>',
             '<Instrument_info_data_header><Location>Test</Location><Sample_rate>180000</Sample_rate></Instrument_info_data_header>',
             '<Ch1_data_header><Identification>Level</Identification><Unit>m</Unit><Parameters><Offset Val="0.0000" Unit="m" /><Altitude Val="269" Unit="m" /></Parameters></Ch1_data_header>',
             '<Data><Log id="1"><Date>2014/08/26</Date><Time>12:00:00</Time><ms>0</ms><ch1>0.7133</ch1></Log></Data></Body_xle>'), xf)
check("the altitude programmed into a Solinst file is read from its header", isTRUE(read_solinst_xle(xf)$meta$altitude_m == 269))
writeLines(sub('Val="269" Unit="m"', 'Val="882.5" Unit="ft"', readLines(xf)), xf)
check("an altitude in feet is turned into metres", isTRUE(abs(read_solinst_xle(xf)$meta$altitude_m - 268.99) < 0.01))
writeLines(sub('Val="882.5" Unit="ft"', 'Val="6.4639e+35" Unit="m"', readLines(xf)), xf)
check("a corrupt altitude is ignored", is.na(read_solinst_xle(xf)$meta$altitude_m))
rf <- report_write(baro_check_report("Test_A", "Test_B", r1, "check", FALSE, NULL), file.path(tempdir(), "baro_check_test"), "html", NORD)
check("the check's report renders, with the exact R code and the decision", { h <- paste(readLines(rf[1], warn = FALSE), collapse = ""); grepl("theil_sen", h) && grepl("mk_hamed_rao", h) && grepl("Offset found", h) })

cat("temperature loggers are left to GT\n")
td <- file.path(tempdir(), paste0("wlr_temp_", as.integer(Sys.time()))); dir.create(file.path(td, "2025"), recursive = TRUE)
hrow <- function(n, v) sprintf("%d,09/0%d/25 01:00:00 PM,%s", n, n, v)
writeLines(c("\"Plot Title: 1000004\"", "\"#\",\"Date Time, GMT-07:00\",\"Temp, \u00b0F (LGR S/N: 1000004, SEN S/N: 1000004, LBL: T1)\",\"Temp, \u00b0F (LGR S/N: 1000004, SEN S/N: 1000004, LBL: T2)\"",
             hrow(1:3, "33.1,33.4")), file.path(td, "2025", "1000004.csv"))
writeBin(charToRaw("\001HOBO U12-008 4-Ch Ext Out/Ind\0021000004\002Upland\002"), file.path(td, "2025", "1000004.hobo"))
writeBin(charToRaw("\001HOBO U12-008 4-Ch Ext Out/Ind\0021000006\002Transect\002"), file.path(td, "2025", "1000006.hobo"))
writeLines(c("\"Plot Title: 20000001\"", "\"#\",\"Date Time, GMT-07:00\",\"Abs Pres, kPa (LGR S/N: 20000001, SEN S/N: 20000001)\",\"Temp, \u00b0C (LGR S/N: 20000001, SEN S/N: 20000001)\"",
             hrow(1:3, "101.2,5.1")), file.path(td, "2025", "20000001.csv"))
tm <- discover_files(list(raw_dirs = td))
check("a HOBO CSV with temperature only is a temperature logger, not read", tm[name == "1000004.csv", status == "temperature_logger" && !use])
check("its .hobo goes with it", tm[name == "1000004.hobo", status] == "temperature_logger")
check("a U12 .hobo with no CSV is a temperature logger, not one needing an export", tm[name == "1000006.hobo", status] == "temperature_logger")
check("a water-level CSV is still read", tm[name == "20000001.csv", status == "ok" && use])
cat("change over the last 60 days (Site page)\n")
local({ t <- as.POSIXct("2014-02-11 18:00", tz = "UTC") + (0:287) * 1800; v <- c(0.02, 0.1, 0.22, rep(0.25, 285)); v[288] <- 0.27
  ch <- settled_change(t, v, starts = t[1], window_h = 2)
  check("the change starts after the logger has settled (not at its first reading in the well) and says the record is shorter than 60 days",
        !is.null(ch) && t[ch$first] >= t[1] + 2 * 3600 && abs(v[ch$last] - v[ch$first] - 0.02) < 1e-9 && !ch$full)
  t2 <- as.POSIXct("2014-01-01", tz = "UTC") + (0:99) * 86400; v2 <- seq_len(100) / 100
  ch2 <- settled_change(t2, v2, starts = t2[1])
  check("on a long record the change spans the last 60 days", ch2$full && as.numeric(difftime(t2[ch2$last], t2[ch2$first], units = "days")) == 60)
  check("fewer than two readings give no change", is.null(settled_change(t[1], 0.2))) })

cat("service visits include the installation\n")
local({ t <- as.POSIXct("2014-02-11 18:00", tz = "UTC") + (0:47) * 1800; d <- data.table(TIMESTAMP = t, depth_m = 0.3, pres_kpa = 100)
  d <- apply_auto_flags(d, as.POSIXct(character(), tz = "UTC"), empty_events(), modifyList(CFG, list(service_window_h = 2)))
  fl <- d$flag_depth_m; ok <- function(x) !is.na(x) & nzchar(x)
  check("readings within the service window after a deployment's first reading are flagged as a service visit, later ones are not",
        !is.null(fl) && all(ok(fl[1:5])) && !any(ok(fl[6:48]))) })

cat("WMO quality codes\n")
tt <- as.POSIXct("2025-06-01 00:00:00", tz = "UTC") + 3600 * (0:5)
wd <- data.table(TIMESTAMP = tt, depth_m = c(1, 1, 1, 1, NA, 1), temp_c = 5, flag_depth_m = c("", "out_of_water", "service_visit;freezing", "baro_fill", "", "baro_secondary"))
wrm <- data.table(TIMESTAMP = tt[1], parameter = "depth_m", rule = "range_max")
wc <- wmo_codes(wd, "depth_m", wrm, c("depth_m"))
check("WMO: removed above its limit is 3, out of water is 3, service visit with freezing is 2", wc[1] == 3L && wc[2] == 3L && wc[3] == 2L)
check("WMO: a barometer fill-in is 6, the secondary barologger leaves it good, a missing value is 7", wc[4] == 6L && wc[6] == 0L && wc[5] == 7L)
check("WMO: a parameter with no quality rule is 4, not checked, and a checked clean value is 0", all(wmo_codes(wd, "temp_c", data.table(), "depth_m") == 4L) && wmo_codes(wd, "temp_c", data.table(), c("temp_c"))[1] == 0L)
cat("drift and offset (off by default)\n")
t0 <- as.POSIXct("2025-06-01 00:00:00", tz = "UTC")
check("the offset is prorated linearly between two checks and held outside them", { v <- c(0, 86400 * 10); s <- drift_offset_series(t0 + 86400 * c(-1, 5, 10, 12), as.numeric(t0) + v, c(0.02, 0.04)); near(s, c(0.02, 0.03, 0.04, 0.04), 1e-9) })
check("with one check the offset is held, or grows from zero if asked", near(drift_offset_series(t0 + 86400 * c(0, 5, 10), as.numeric(t0) + 86400 * 10, 0.03), rep(0.03, 3), 1e-9) &&
      near(drift_offset_series(t0 + 86400 * c(0, 5, 10), as.numeric(t0) + 86400 * 10, 0.03, "linear", as.numeric(t0)), c(0, 0.015, 0.03), 1e-9))
mk <- function() data.table(TIMESTAMP = t0 + 86400 * (0:10), site = "S", logger_id = "1", year = 2025L, depth_m = 1, dtw_m = 2, elev_m = 100, flag_depth_m = "")
dep <- data.table(deployment = "S_1_2025", site = "S", serial = "1", year = 2025L)
chk <- data.table(visit_time = t0 + c(0, 86400 * 10), difference_m = c(-0.02, -0.04))     # logger minus tape: the logger read 2 and 4 cm low
d <- mk(); i0 <- apply_drift(d, dep, chk, NULL, list())
check("drift is off by default: nothing is added and the values are as read", !i0$applied && i0$mode == "off" && !any(c("depth_m_corr", "drift_offset_m") %in% names(d)))
d <- mk(); i1 <- apply_drift(d, dep, chk, NULL, list(drift_correction = "check"))
check("drift is off or apply: an old 'check' setting is off and changes nothing", !i1$applied && i1$mode == "off" && !"depth_m_corr" %in% names(d))
d <- mk(); i2 <- apply_drift(d, dep, chk, NULL, list(drift_correction = "apply", drift_min_m = 0.01))
check("'apply' adds corrected depth, elevation and depth to water, and leaves the logger's values alone", i2$applied && near(d$depth_m, rep(1, 11)) && near(d$depth_m_corr[c(1, 6, 11)], c(1.02, 1.03, 1.04), 1e-9) &&
      near(d$elev_m_corr[11], 100.04, 1e-9) && near(d$dtw_m_corr[11], 1.96, 1e-9) && near(d$drift_offset_m[6], 0.03, 1e-9))
d <- mk(); i3 <- apply_drift(d, dep, chk, NULL, list(drift_correction = "apply", drift_min_m = 0.05))
check("offsets below the threshold are not applied, and the reason is given", !i3$applied && grepl("below", i3$how) && !"depth_m_corr" %in% names(d))
d <- mk(); i4 <- apply_drift(d, dep, chk, data.table(serial = "1", site = "", year = "", mode = "manual", offset_m = "0.05", offset_end_m = "0.07", start = "", end = "", reason = "typed", author = "t"), list())
check("a typed offset is applied on its own, even with drift off, and runs linearly from start to end", i4$applied && i4$mode == "manual" && near(d$drift_offset_m[c(1, 6, 11)], c(0.05, 0.06, 0.07), 1e-9))
d <- mk(); i5 <- apply_drift(d, dep, chk, data.table(serial = "", site = "S", year = "", mode = "none", offset_m = "", offset_end_m = "", start = "", end = "", reason = "", author = ""), list(drift_correction = "apply"))
check("a 'none' row keeps a site from being corrected even when drift is on", !i5$applied && !"depth_m_corr" %in% names(d))
dep2 <- data.table(deployment = "S_1_2025", site = "S", serial = "1", year = 2025L, T_ST = 1.5, T_ST_source = "workbook", offset_m = 0.02, ref_code = "T")
np <- length(AUDIT$problems); t_ok <- tape_offset_check(dep2, chk, NULL, list())
check("a tape offset within 5 cm is fine", t_ok$status == "ok" && length(AUDIT$problems) == np)
big <- data.table(visit_time = t0 + c(0, 86400 * 10), difference_m = c(-0.15, -0.16))
t_big <- tape_offset_check(dep2, big, NULL, list()); np2 <- length(AUDIT$problems)
check("a larger tape offset is marked as a likely metadata error, with T_ST, the offset, the reference and a hint", t_big$status == "check the metadata" &&
      t_big$T_ST == 1.5 && t_big$reference == "T" && nzchar(t_big$hint) && near(t_big$median_diff_m, -0.155))
t_keep <- tape_offset_check(dep2, big, data.table(serial = "1", site = "", year = "2025", mode = "keep", offset_m = "", offset_end_m = "", start = "", end = "", reason = "", author = ""), list())
check("a 'keep' row keeps the offset as it is", t_keep$status == "kept as it is")
d <- mk(); i6 <- apply_drift(d, dep, big, data.table(serial = "1", site = "", year = "", mode = "keep", offset_m = "", offset_end_m = "", start = "", end = "", reason = "", author = ""), list())
check("'keep' does not turn on a correction", !i6$applied && !"depth_m_corr" %in% names(d))
# An older version's drift_corrections.csv: moved once into the corrections over a period, same results
old_dr <- data.table(serial = c("1", "", "9"), site = c("", "S", ""), year = c("2025", "", ""), mode = c("tape", "manual", "keep"), offset_m = c("", "0.05", ""),
                     offset_end_m = c("", "0.07", ""), start = "", end = "", reason = c("tape", "typed", "kept"), author = "t")
pdr <- file.path(tempdir(), "driftproj"); dir.create(file.path(pdr, "config"), recursive = TRUE); fwrite(old_dr, file.path(pdr, "config", "drift_corrections.csv"))
invisible(config_migrate(pdr, "test")); pfr <- file.path(pdr, "config", PERIOD_FILE); back_dr <- wlr_period_to_drift(period_own_rows(pfr, WLR_DRIFT_ACTIONS))
check("an older project's drift rows move in once as drift actions, the old file kept", file.exists(pfr) && file.exists(file.path(pdr, "config", "drift_corrections.csv")) &&
      identical(period_action(read_cfg(pfr)$action), c("drift_tape", "drift_manual", "drift_keep")) && identical(back_dr$mode, c("tape", "manual", "keep")))
invisible(config_migrate(pdr, "test"))
check("the old drift rows are not moved twice", nrow(read_cfg(pfr)) == 3)
d_old <- mk(); i_old <- apply_drift(d_old, dep, chk, old_dr, list()); d_new <- mk(); i_new <- apply_drift(d_new, dep, chk, back_dr, list())
check("tape and typed drift give the same corrected values before and after the move", i_old$applied && i_new$applied && i_new$mode == i_old$mode &&
      isTRUE(all.equal(d_new$drift_offset_m, d_old$drift_offset_m)) && isTRUE(all.equal(d_new$depth_m_corr, d_old$depth_m_corr)))
d_t1 <- mk(); i_t1 <- apply_drift(d_t1, dep, chk, old_dr[1], list(drift_min_m = 0.01)); d_t2 <- mk(); i_t2 <- apply_drift(d_t2, dep, chk, back_dr[1], list(drift_min_m = 0.01))
check("a tape row alone still turns on the tape offsets after the move", i_t1$applied && i_t2$applied && grepl("tape", i_t2$mode) && isTRUE(all.equal(d_t2$depth_m_corr, d_t1$depth_m_corr)))
dx <- mk(); invisible(apply_drift(dx, dep, chk, NULL, list(drift_correction = "apply"))); et <- export_table(dx, "1", file.path(tempdir(), "nodrift"))
check("the export has the logger's values, the corrected ones, the offset and a WMO code of 5 where it was changed", all(c("depth_m", "depth_m_corr", "drift_offset_m", "depth_m_corr_qc", "depth_m_qc") %in% names(et)) && all(et$depth_m_corr_qc == 5L) && all(et$depth_m_qc == 4L | et$depth_m_qc == 0L))
eo <- export_table(dx, "1", file.path(tempdir(), "nodrift"), "original"); ec <- export_table(dx, "1", file.path(tempdir(), "nodrift"), "corrected")
check("'original' drops the corrected columns, and 'corrected' puts corrected values in the usual columns with code 5", !any(grepl("_corr|drift_offset", names(eo))) && near(eo$depth_m, rep(1, 11)) && !any(grepl("_corr", names(ec))) && near(ec$depth_m[11], 1.04, 1e-9) && ec$depth_m_qc[11] == 5L)
cdir <- file.path(tempdir(), "bulkproj"); dir.create(file.path(cdir, "output", "corrected"), recursive = TRUE)
for (st in c("S1", "S2")) { dd <- mk(); dd[, site := st]; apply_drift(dd, dep, chk, NULL, list(drift_correction = "apply")); fwrite(export_table(dd, "1", cdir), file.path(cdir, "output", "corrected", paste0(st, ".csv"))) }
bz <- file.path(tempdir(), "bulk.zip"); corrected_data_zip(cdir, c("S1", "S2"), bz, c("depth_m", "temp_c"), "raw"); zl <- zip::zip_list(bz)$filename
check("the bulk export is one file per site, all sites together and a readme", all(c("sites/S1.csv", "sites/S2.csv", "all_sites.csv", "README.txt") %in% zl))
cd1 <- corrected_data(cdir, "S1", "depth_m", "raw", versions = "both"); cd2 <- corrected_data(cdir, "S1", "depth_m", "raw", versions = "original")
check("corrected data can give both versions or the logger's values only", any(grepl("_corr", names(cd1))) && !any(grepl("_corr", names(cd2))))
cat("files that state no time zone\n")
check("with no zone chosen a file that states none is read in the base zone, as before", zone_for("1", "a.csv", NA, NULL, -7)$offset == -7)
check("a zone chosen for such files is used for them, and a stated zone still wins", zone_for("1", "a.csv", NA, NULL, -7, unzoned = -6)$offset == -6 && zone_for("1", "a.csv", -7, NULL, -6, unzoned = -6, stated_wins = TRUE)$offset == -7)
check("when every file states none and no zone is chosen, the app is asked", inherits(tryCatch(unzoned_check(5, 5, NA, ask = TRUE), condition = function(e) e), "ovid_ask_zone") &&
      isFALSE(unzoned_check(5, 5, -7, ask = TRUE)) && isFALSE(unzoned_check(3, 5, NA, ask = TRUE)))
check("a script is not stopped: it carries on in the base zone and says so", { AUDIT$problems <- list(); isTRUE(suppressMessages(unzoned_check(4, 4, NA, ask = FALSE))) })
check("the tape-check chart marks each visit against the drift threshold and the working tolerance", { chk <- data.table(visit_time = as.POSIXct(c("2024-05-01", "2025-05-01", "2026-05-01"), tz = "UTC"), difference_m = c(0.004, -0.02, 0.06))
  b <- wlr_tape_plot(chk, NORD); b$type == "plot" && isTRUE(b$inline) && identical(levels(b$plot$data$status)[1], "within the drift threshold") && WORKING_TOL_M == 0.03 && is.null(wlr_tape_plot(chk[0], NORD)) })
cat("tip-to-sensor offsets\n")
ot <- default_offsets()
check("HOBO U20 and U20L get the maker's 1.25 cm, matched on the model name the files state",
      near(offset_for("1", "HOBO U20-001-04 Water Level", ot), 0.0125) && near(offset_for("1", "HOBO U20L-04 Water Level", ot), 0.0125) && near(offset_for("1", "HOBO U20-001-01-Ti Water Level", ot), 0.0125))
check("a model row with no distance yet gives 0, and so does an unknown model", offset_for("1", "LT_EDGE M2", ot) == 0 && offset_for("1", "LT_Gold M10", ot) == 0 && offset_for("1", "Something else", ot) == 0)
ot2 <- rbind(data.table(serial = "77", model = "", offset_m = "0.02", source = "measured", notes = ""), ot); ot2[model == "LT_?EDGE|Levelogger ?Edge|Junior ?Edge", offset_m := "0.01"]
check("a serial row wins over the model, and a value typed for a model is used", near(offset_for("77", "HOBO U20-001-04", ot2), 0.02) && near(offset_for("5", "LT_EDGE M2", ot2), 0.01))
check("the old all-zero table is replaced by the seeded one, and a table with a value typed in is left alone",
      !is.null(upgrade_offsets(data.table(serial = "", model = c("U20", "LTC"), offset_m = "0", notes = OLD_OFFSET_NOTE))) &&
      is.null(upgrade_offsets(data.table(serial = "", model = c("U20", "LTC"), offset_m = c("0.01", "0"), notes = OLD_OFFSET_NOTE))))
dd <- data.table(deployment = c("a", "b", "c"), serial = c("1", "2", "3"), measured_to = c("", "sensor", "tip"))
mods <- c("HOBO U20-001-04 Water Level", "HOBO U20L-04 Water Level", "Unknown")
so <- apply_sensor_offsets(dd, mods, ot, list(add_sensor_offset = TRUE, hang_measured_to = "tip"))
check("the offset applies where the hang length is to the tip, not where it is to the sensor", near(so$offset_m, c(0.0125, 0, 0)) && near(so$offset_spec_m, c(0.0125, 0.0125, 0)) &&
      identical(so$measured_to, c("tip", "sensor", "tip")) && grepl("Onset", so$offset_source[1]) && grepl("no row", so$offset_source[3]))
check("the offset is off by default (an old sensor_offset = TRUE does not turn it on), and the table's value is still reported",
      near(apply_sensor_offsets(dd, mods, ot, list(sensor_offset = TRUE, hang_measured_to = "tip"))$offset_m, c(0, 0, 0)) &&
      near(apply_sensor_offsets(dd, mods, ot, list(hang_measured_to = "tip"))$offset_spec_m, c(0.0125, 0.0125, 0)))
check("with hang lengths measured to the sensor by default, only a deployment marked tip gets it",
      near(apply_sensor_offsets(dd[, measured_to := c("", "", "tip")], c(mods[1:2], mods[1]), ot, list(add_sensor_offset = TRUE, hang_measured_to = "sensor"))$offset_m, c(0, 0, 0.0125)))
ov <- data.table(serial = c("9", ""), year = c("2025", ""), site = c("S", "S"), deployed = "", removed = "", T_ST_m = c("1.5", ""), T_ST_from_year = "", reference = "",
                 measured_to = c("", "sensor"), reason = "", author = "")
bd <- build_deployments(NULL, ov)
check("a row with a site and no serial sets measured_to for every logger at that site", nrow(bd) == 1 && bd$measured_to == "sensor" && bd$T_ST == 1.5)
kn <- known_offsets(rbind(ot2, data.table(serial = "", model = "Odd", offset_m = "", source = "", notes = ""), fill = TRUE))
check("the drop-down lists the makers' values and the project's own, not rows without a value",
      any(grepl("U20L.*1.25 cm", kn$label)) && any(kn$serial == "77") && !"Odd" %in% kn$model && all(!is.na(as.numeric(kn$offset_m))))
t1 <- set_offset_row(ot, model = "LT_?EDGE|Levelogger ?Edge|Junior ?Edge", offset_m = 0.009, source = "manual measurement", notes = "calipers")
t2 <- set_offset_row(t1, serial = "2045", offset_m = 0.011, source = "manual measurement")
check("a manual measurement replaces the model's row, and a serial row goes first and wins",
      nrow(t1) == nrow(ot) && near(offset_for("5", "LT_EDGE M2", t1), 0.009) && t2$serial[1] == "2045" && near(offset_for("2045", "LT_EDGE M2", t2), 0.011) &&
      near(offset_for("6", "LT_EDGE M2", t2), 0.009))
check("setting the same serial again replaces it", nrow(set_offset_row(t2, serial = "2045", offset_m = 0.012)) == nrow(t2))
cat("only what changed is redone\n")
wb_root <- file.path(tempdir(), paste0("wlr_wb_", as.integer(Sys.time()))); wraw <- file.path(wb_root, "raw")
dir.create(file.path(wraw, "2025"), recursive = TRUE); dir.create(file.path(wraw, "baro", "2025"), recursive = TRUE)
hobo_kpa <- function(f, serial, t, p, temp) writeLines(c(sprintf("\"Plot Title: %s\"", serial),
  sprintf("\"#\",\"Date Time, GMT-07:00\",\"Abs Pres, kPa (LGR S/N: %s, SEN S/N: %s)\",\"Temp, \u00b0C (LGR S/N: %s, SEN S/N: %s)\"", serial, serial, serial, serial),
  sprintf("%d,%s,%.3f,%.2f", seq_along(t), format(t, "%m/%d/%y %I:%M:%S %p"), p, temp)), f)
wt <- as.POSIXct("2025-06-01 00:00", tz = "UTC") + 0:239 * 3600
wbaro <- 100 + 0.3 * sin(seq_along(wt) / 12)
hobo_kpa(file.path(wraw, "baro", "2025", "20000002.csv"), "20000002", wt, wbaro, 15)
w1 <- wbaro + 5 + 0.01 * seq_along(wt); w1[100] <- 300                               # above the 250 kPa limit: removed, and its depth with it
hobo_kpa(file.path(wraw, "2025", "20000001.csv"), "20000001", wt, w1, 8)
hobo_kpa(file.path(wraw, "2025", "20000003.csv"), "20000003", wt, wbaro + 3, 9)
wcfg <- function(p, inc = TRUE) modifyList(CFG, list(raw_dirs = wraw, metadata_dir = file.path(wb_root, "no_workbook"), dgps_dir = "", project_dir = file.path(wb_root, p), incremental = inc))
quiet_run <- function(cfg) { r <- NULL; invisible(capture.output(r <- run_pipeline(cfg))); r }
mf <- function(p) { m <- fread(file.path(wb_root, p, "output", "audit", "run_manifest.csv"), colClasses = "character"); function(k) m[key == k, value] }
outs_of <- function(p) { o <- file.path(wb_root, p, "output"); f <- c(file.path("sites", list.files(file.path(o, "sites"))), file.path("corrected", list.files(file.path(o, "corrected"))),
    file.path("audit", c("summary.csv", "audit_index.csv", "flags.csv", "reconciliation.csv", "file_audit.csv")), file.path("audit", "detail", list.files(file.path(o, "audit", "detail"))))
  stats::setNames(lapply(f, function(x) { d <- fread(file.path(o, x), colClasses = "character", na.strings = NULL); if ("run_id" %in% names(d)) d[, run_id := NULL]; d }), f) }
invisible(quiet_run(wcfg("p"))); a1 <- outs_of("p"); m1 <- mf("p")
check("a first run compensates every water logger", m1("water_loggers") == "2" && m1("water_loggers_reused") == "0" && m1("loggers_stitched_reused") == "0")
invisible(quiet_run(wcfg("p"))); m2 <- mf("p")
check("with nothing changed, every logger is read back from the cache and the output is the same, file by file",
      m2("water_loggers_reused") == "2" && m2("loggers_stitched_reused") == "3" && m2("sites_corrected_reused") == "2" && identical(outs_of("p"), a1))
fwrite(data.table(serial = "20000001", site = "", start = "2025-06-03 00:00", end = "2025-06-03 05:00", parameter = "", min_value = "", max_value = "", reason = "test", author = "t"),
       file.path(wb_root, "p", "config", "manual_removals.csv"))
invisible(quiet_run(wcfg("p"))); m3 <- mf("p")
dir.create(file.path(wb_root, "full")); file.copy(file.path(wb_root, "p", "config"), file.path(wb_root, "full"), recursive = TRUE); invisible(quiet_run(wcfg("full", FALSE)))
check("a removal on one logger redoes that logger only, and gives what a run from scratch gives", m3("water_loggers_reused") == "1" && m3("loggers_stitched_reused") == "3" && identical(outs_of("p"), outs_of("full")))
pcf <- file.path(wb_root, "p", "config", PERIOD_FILE); m <- fread(pcf, colClasses = "character", na.strings = NULL)
check("an older version's manual_removals.csv is moved once into the corrections over a period, and left in place",
      nrow(m[action == "remove"]) == 1 && m[action == "remove", target] == "20000001" && file.exists(file.path(wb_root, "p", "config", "manual_removals.csv")))
fwrite(rbind(m, data.table(target = "20000002", from = "2025-06-05 00:00", to = "2025-06-05 02:00", action = "remove", note = "baro", author = "t"), fill = TRUE), pcf)
invisible(quiet_run(wcfg("p"))); m4 <- mf("p")
file.copy(pcf, file.path(wb_root, "full", "config"), overwrite = TRUE); invisible(quiet_run(wcfg("full", FALSE)))
check("a removal on a barologger redoes the water loggers it compensates, and still matches a run from scratch", m4("water_loggers_reused") == "0" && identical(outs_of("p"), outs_of("full")))
check("every water deployment gets an uncertainty and a barometric check", { u <- fread(file.path(wb_root, "p", "output", "audit", "uncertainty.csv")); b <- fread(file.path(wb_root, "p", "output", "audit", "barometric_efficiency.csv"))
  nrow(u) == 2 && all(u$depth_sigma_m > 0) && all(u$depth_sigma_m < 0.05) && nrow(b) == 2 && all(abs(b$be_lag0) < 0.1) && all(grepl("^no (clear )?barometric", b$verdict)) })
local({                                                                # set-up tables people edit by hand must not stop a run
  dir.create(file.path(wb_root, "edits")); file.copy(file.path(wb_root, "p", "config"), file.path(wb_root, "edits"), recursive = TRUE)
  ok <- function() !is.null(tryCatch(quiet_run(wcfg("edits")), error = function(e) { cat("  stopped:", conditionMessage(e), "\n"); NULL }))
  fwrite(data.table(site = c("A", "A", "B"), code = "TOC", elevation_m = c("250.1", "250.3", "251"), date = c("2023-06-01", "2024-06-01", "2024-06-01"), reason = "survey",
                    author = "t", height_type = "", frame = c("NAD83(CSRS)", "", ""), epoch = c("2010", "", ""), geoid_model = "", geoid_N_m = ""), file.path(wb_root, "edits", "config", "datum_manual.csv"))
  a <- ok(); file.create(file.path(wb_root, "edits", "config", "datum_manual.csv")); b <- ok()
  check("typed datum rows (several, for one site or many) and an emptied datum table do not stop a run", a && b)
  fwrite(data.table(barologger = "99999999", reference = "20000002", method = "offset", from = "", to = "", apply = "yes", notes = "a typo"), file.path(wb_root, "edits", "config", "baro_corrections.csv"))
  check("a barologger check naming an unknown barologger is reported as a problem, and the run goes on",
        ok() && any(grepl("99999999 not found", fread(file.path(wb_root, "edits", "output", "audit", "problems.csv"))$problem))) })
fwrite(data.table(site = "*", approved_through = "2025-06-05", by = "t", notes = ""), file.path(wb_root, "p", "config", "approvals.csv"))
cdx <- corrected_data(file.path(wb_root, "p"), unique(fread(file.path(wb_root, "p", "output", "audit", "summary.csv"))$site)[1], c("depth_m", "temp_c"))
ncf <- tempfile(fileext = ".nc"); ncs <- unique(fread(file.path(wb_root, "p", "output", "audit", "summary.csv"))$site)[1]
site_netcdf(file.path(wb_root, "p"), ncs, ncf, c("depth_m", "temp_c")); ncx <- nc_read(ncf)
check("exports mark each time approved or provisional, and a site's NetCDF holds its values, codes, uncertainty and approval",
  all(c("approved", "provisional") %in% cdx$approval) && all(c("depth_m", "depth_m_qc", "depth_m_sigma", "temp_c", "approval") %in% names(ncx$vars)) &&
  ncx$dims[["time"]] == nrow(cdx) && all(is.finite(ncx$vars$depth_m_sigma$data)) && ncx$atts$featureType == "timeSeries" && ncx$atts$approval == "approved in part" &&
  ncx$vars$time$data[1] == as.numeric(as.POSIXct(cdx[[1]][1], tz = "UTC")) - BASE_OFFSET * 3600)
k1 <- key_rows(data.table(a = character())); k2 <- key_rows(data.table(a = "x")[a == "y"])
check("a set-up table with no rows that apply keys the same however it was made", is.null(k1) && is.null(k2) && identical(key_rows(data.table(a = "1")), list(a = "1")))

cat("no metadata workbook: optional, and a logger list instead\n")
invisible(quiet_run(wcfg("ll"))); pr <- file.path(wb_root, "ll", "output", "audit", "problems.csv")
pl <- if (file.exists(pr)) fread(pr, colClasses = "character") else data.table(where = character(), problem = character())
sc <- setup_check(wcfg("ll"))
check("without a workbook or a logger list nothing is reported as a problem, and the set-up check only notes it",
      !any(grepl("metadata|workbook", paste(pl$where, pl$problem), ignore.case = TRUE)) && !any(sc$level %in% c("error", "warning") & grepl("Metadata", sc$area)) && any(sc$level == "info" & sc$area == "Metadata"))
fwrite(data.table(serial = c("20000001", "20000003"), year = "", name = c("Lake 3 shallow", "Pond"), site = c("Lake 3", ""), T_ST = c("45 cm", ""), owner = c("A", "")),
       file.path(wb_root, "ll", "config", "metadata.csv"))
invisible(quiet_run(wcfg("ll"))); dl <- fread(file.path(wb_root, "ll", "output", "audit", "deployments.csv"), colClasses = list(character = "serial"))
check("a logger list with a serial and a name, a site or a hang length places each logger, with no workbook",
      dl[serial == "20000001", site] == "Lake 3" && isTRUE(all.equal(dl[serial == "20000001", T_ST], 0.45)) && dl[serial == "20000003", site] == "Pond" && dl[serial == "20000001", year] == 2025)
mr <- meta_read(file.path(wb_root, "ll"), meta_spec(META_FIELDS, file.path(wb_root, "ll")))
check("a column people added to the list is kept, and the app's own columns are read beside it", all(c("owner", "serial", "visit", "type") %in% names(mr)) && mr[serial == "20000001", owner] == "A")
dsf <- file.path(wb_root, "ll", "config", DEPLOY_SHEET); ml <- fread(dsf, colClasses = "character", na.strings = NULL)
check("an older version's logger list is moved once into the deployment sheet, every column kept, and left in place",
      setequal(ml$serial, c("20000001", "20000003")) && all(c("name", "T_ST", "owner") %in% names(ml)) && file.exists(file.path(wb_root, "ll", "config", "metadata.csv")))
ml[serial == "20000003", name := "Pond B"]; fwrite(ml, dsf)
invisible(quiet_run(wcfg("ll"))); m5 <- mf("ll")
check("renaming one logger on the deployment sheet redoes that logger only", m5("water_loggers_reused") == "1" && "Pond B" %in% fread(file.path(wb_root, "ll", "output", "audit", "deployments.csv"))$site)

cat("other barometric records as fill-ins\n")
xf <- file.path(wb_root, "cs_station.csv")
fwrite(data.table(TIMESTAMP = format(wt + 3600, "%Y-%m-%d %H:%M:%S"), BP_mbar = round(wbaro * 10, 3), AirTC = 10), xf)        # its clock in UTC-6: an hour ahead of UTC-7
xb <- read_external_baro(data.table(name = "CS", file = xf, time_column = "", pressure_column = "BP_mbar", units = "mbar", utc_offset_h = "-6", station_elevation_m = "", reference_elevation_m = "", temp_column = "", notes = ""))
check("another instrument's pressure is read in its units and zone", length(xb) == 1 && max(abs(xb[["ext: CS"]]$baro_kpa - wbaro)) < 1e-3 && xb[["ext: CS"]]$TIMESTAMP[1] == wt[1])
xb2 <- read_external_baro(data.table(name = "CS", file = xf, time_column = "TIMESTAMP", pressure_column = "BP_mbar", units = "hPa", utc_offset_h = "-6", station_elevation_m = "300", reference_elevation_m = "250", temp_column = "AirTC", notes = ""))
check("given both elevations it is brought down to the barologgers' (higher pressure 50 m lower)", all(xb2[["ext: CS"]]$baro_kpa > xb[["ext: CS"]]$baro_kpa) && abs(mean(xb2[["ext: CS"]]$baro_kpa / xb[["ext: CS"]]$baro_kpa) - exp(9.80665 * 50 / (287.05 * 283.15))) < 1e-4)
xr <- file.path(wb_root, "raw_nobaro"); dir.create(file.path(xr, "2025"), recursive = TRUE); file.copy(file.path(wraw, "2025", "20000001.csv"), file.path(xr, "2025"))
xp <- file.path(wb_root, "xb"); dir.create(file.path(xp, "config"), recursive = TRUE)
fwrite(data.table(name = "CS", file = xf, time_column = "", pressure_column = "BP_mbar", units = "mbar", utc_offset_h = "-6", station_elevation_m = "", reference_elevation_m = "", temp_column = "", notes = ""), file.path(xp, "config", "external_baro.csv"))
invisible(quiet_run(modifyList(wcfg("xb"), list(raw_dirs = xr)))); xs <- fread(list.files(file.path(xp, "output", "sites"), full.names = TRUE)[1])
check("with no barologger, a water logger is compensated from the other record as a fill-in, and flagged so", nrow(xs) > 0 && any(!is.na(xs$depth_m)) && all(grepl("baro_fill", xs[!is.na(depth_m), flag_depth_m])) && all(xs[!is.na(depth_m), baro_name] == "ext: CS") && max(abs(xs$baro_kpa - wbaro[match(as.numeric(as.POSIXct(xs$TIMESTAMP, tz = "UTC")), as.numeric(wt))]), na.rm = TRUE) < 1e-3)

cat("Workbench interface (preview)\n")
app <- new.env(); assign("shinyApp", function(...) invisible(NULL), envir = app)
local({ old <- setwd(here); on.exit(setwd(old)); suppressMessages(sys.source("app.R", envir = app, keep.source = FALSE)) })
app$SETTINGS_FILE <- file.path(wb_root, "app_settings.json"); app$VIEWER <- FALSE
app$SETTINGS <- as.list(app$CFG)                     # the defaults, not the settings saved on this computer (a test never reads or writes them)
pw <- file.path(wb_root, "p")
local({ src <- unlist(lapply(list.files(file.path(here, "pages"), "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  go <- unlist(regmatches(src, gregexpr('`data-ovid-go` = "[A-Za-z]+"', src)))
  pages <- c(vapply(Filter(function(x) inherits(x, "ovid_tool"), app$wlr_flow_pages()), `[[`, "", "value"), "Settings", "Reference", "Conversions")
  check("every link to a page names a page the app has (\"Decide on Check the data\" went nowhere)", length(go) > 0 && all(sub('.*"([A-Za-z]+)"$', "\\1", go) %in% pages))
  check("no page names \"Check the data\" any more (its findings are on Needs you)", !any(grepl("Check the data", src, fixed = TRUE))) })
check("both interfaces build their table editors from one list, and the Workbench opens each where the Standard edits it",
      all(app$SETUP_TABLES$cfg %in% names(CONFIG_FILES)) && !anyDuplicated(app$SETUP_TABLES$id) && setequal(names(app$WB_OPEN), app$SETUP_TREE$id) &&
      any(grepl("setup_editors(cfgp, tick, mark, skip = \"depsheet\")", deparse(app$server), fixed = TRUE)))
app$SETTINGS$project_dir <- pw; app$SETTINGS$raw_dirs <- wraw; app$SETTINGS$metadata_dir <- file.path(wb_root, "no_workbook"); app$SETTINGS$dgps_dir <- ""
cat("the Simple view\n")
local({
  h <- as.character(htmltools::renderTags(app$simple_ui(NULL))$html); ids <- regmatches(h, gregexpr(' id="[^"]+"', h))[[1]]
  check("the Simple view builds: four steps, its own Import with the folder inputs, and no input twice", grepl('data-step="export"', h, fixed = TRUE) &&
        grepl('id="raw_dirs"', h, fixed = TRUE) && grepl('id="metadata_dir"', h, fixed = TRUE) && grepl('id="run"', h, fixed = TRUE) && !anyDuplicated(ids))
  check("values are edited by hand only in the Workbench: quality rules, out of water, clock and barologger corrections, corrections over a period",
        setequal(app$SETUP_TABLES[wb_only == TRUE]$id, c("qc", "oow", "time", "bcor", "period")))
  ps <- tempfile("wlr_simple"); dir.create(ps); invisible(file.copy(list.files(pw, full.names = TRUE), ps, recursive = TRUE)); cfg <- file.path(ps, "config")
  md5 <- function() { f <- list.files(cfg, full.names = TRUE, recursive = FALSE); f <- f[!dir.exists(f)]; stats::setNames(tools::md5sum(f), basename(f)) }
  o <- suppressWarnings(app$load_outputs(ps)); s1 <- o$summary[serial == "20000001"]; g0 <- s1$site[1]
  view <- data.table(item = "20000001", label = "20000001", group = g0, first = substr(as.character(s1$start[1]), 1, 16), last = substr(as.character(s1$end[1]), 1, 16), lat = NA_real_, lon = NA_real_)
  ops <- rbind(app$SIMPLE_OPS, data.table(type = c("rename", "place", "field", "move"), group = c(g0, "Pond A", "Pond A", "Pond A"), to = c("Pond A", "", "Bog", "Pond B"),
                                      item = c("", "", "land_cover", "20000001"), at = c("", "", "", "2025-06-05 00:00"), lat = c(NA, 60.1, NA, NA), lon = c(NA, -120.2, NA, NA)), fill = TRUE)
  sh0 <- if (file.exists(file.path(cfg, "deployment_sheet.csv"))) fread(file.path(cfg, "deployment_sheet.csv"), colClasses = "character", na.strings = NULL) else NULL
  b0 <- md5(); r <- app$sp_wlr_apply(ps, ops, "tt", view, o); b1 <- md5()
  ch <- names(b1)[is.na(b0[names(b1)]) | b0[names(b1)] != b1]
  k <- app$rename_spec(app$RENAMES, "site"); ok_files <- unique(c("deployment_sheet.csv", k$main[1], vapply(c(k$cols, k$lists), `[`, "", 1)))
  check("Simple's Edit writes only the deployment sheet and the tables that name a site, never a rule or a correction", isTRUE(r$ok) && length(ch) && all(ch %in% ok_files))
  sh <- fread(file.path(cfg, "deployment_sheet.csv"), colClasses = "character", na.strings = NULL)
  check("a rename, a place, a land cover and a move from a date land in the deployment sheet: the logger's row ends the day before, a new one starts at the new site",
        any(sh$serial == "20000001" & sh$site == "Pond A" & sh$to == "2025-06-04") && any(sh$serial == "20000001" & sh$site == "Pond B" & sh$from == "2025-06-05") &&
        any(sh$site == "Pond A" & sh$latitude == "60.1") && any(sh$site == "Pond A" & sh$land_cover == "Bog"))
  check("each save keeps the version before, with who changed it", any(grepl("Simple view", readLines(file.path(cfg, "history", "change_log.csv"), warn = FALSE), fixed = TRUE)))
  invisible(quiet_run(modifyList(wcfg("x"), list(project_dir = ps)))); o2 <- suppressWarnings(app$load_outputs(ps))
  check("the next run reads them: the logger's record is split between the two sites", all(c("Pond A", "Pond B") %in% o2$summary[serial == "20000001"]$site))
  r$undo(); sh1 <- if (file.exists(file.path(cfg, "deployment_sheet.csv"))) fread(file.path(cfg, "deployment_sheet.csv"), colClasses = "character", na.strings = NULL) else NULL
  check("Undo puts the deployment sheet back as it was", if (is.null(sh0)) is.null(sh1) || !nrow(sh1) else isTRUE(all.equal(sh1, sh0, check.attributes = FALSE)))
  f <- app$sp_wlr_facts(o2, ps)
  check("View has a tile per water site with its barologger, and none for the barologger itself", !is.null(f) && all(c("Pond A", "Pond B") %in% f$f$site) && all(nzchar(f$f$baro)) &&
        !any(grepl("20000002", f$f$site)))
})
hw <- as.character(htmltools::renderTags(app$wb_ui(list(QUERY_STRING = "ui=workbench")))$html)
idw <- regmatches(hw, gregexpr(' id="[^"]+"', hw))[[1]]
toolw <- vapply(Filter(function(x) inherits(x, "ovid_tool"), app$std_pages(FALSE)), `[[`, "", "value")
check("the Workbench holds every page of the Standard interface, its data view, Import, Run (the Standard's) and Export, and no input twice",
      all(vapply(c(toolw, "wb-data", "wb-import"), function(v) grepl(paste0('data-value="', v, '"'), hw, fixed = TRUE), NA)) && !anyDuplicated(idw) &&
      grepl('id="run"', hw, fixed = TRUE) && grepl('id="wb_export"', hw, fixed = TRUE) && grepl('href="?ui=standard"', hw, fixed = TRUE) &&
      all(vapply(setdiff(app$SETUP_TABLES$id, "depsheet"), function(i) grepl(paste0('id="', i, '-tbl"'), hw, fixed = TRUE), NA)) && grepl('id="mlist-ed-tbl"', hw, fixed = TRUE) &&
      all(vapply(c("wb_act_remove", "wb_act_restore", "wb_act_oow", "wb_act_drop", "wb_act_clock", "raw_dirs", "metadata_dir", "project_dir", "wb_chart"), function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)))
check("the folded settings are gone, the expert constants sit under Advanced, and the switches are back (on by default but the offset)",
      !any(c("sensor_offset", "air_threshold_m", "drift_columns") %in% app$SET_KEYS) &&
      all(c("add_sensor_offset", "air_same_as_oow", "air_threshold_cm", "drift_min_m", "drift_one_visit", "tape_warn_m", "baro_interp", "hide_accepted_flags") %in% app$SET_KEYS) &&
      isFALSE(app$CFG$add_sensor_offset) && isTRUE(app$CFG$air_same_as_oow) && isTRUE(app$CFG$baro_interp) && isTRUE(app$CFG$hide_accepted_flags) &&
      all(vapply(c("add_sensor_offset", "air_same_as_oow", "air_threshold_cm", "drift_min_m", "tape_warn_m", "drift_one_visit"), function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)) &&
      !any(vapply(c("sensor_offset", "air_threshold_m", "drift_columns", "bo_suggest", "br_drop", "md_problems", "md_runs", "dt_chk", "fv_meas", "cv_dep", "ec_dep", "rp_dl", "cd_zip"),
                  function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)) && !any(c("Convert", "EC") %in% toolw) &&
      grepl("<summary>Advanced</summary>", hw, fixed = TRUE) && grepl('id="xp_fmt"', hw, fixed = TRUE) && grepl('id="appr-tbl"', hw, fixed = TRUE))
sg <- data.table(serial = c("B1", "B2", "B3", "B9"), group = c("g1", "g1", "g2", "g1"), score = c(0.5, 0.9, 0.7, 1), year = c(2025L, 2025L, 2025L, 2024L))
bs <- app$bo_suggested_groups(sg, "2025")
check("a year with no saved groups starts from the suggestion: the best-scored primary, the next secondary",
      identical(bs$group, c("g1", "g2")) && identical(bs$primary, c("B2", "B3")) && identical(bs$secondary, c("B1", "")) && all(bs$year == "2025") && !nrow(app$bo_suggested_groups(sg, "2023")))
ow <- suppressWarnings(app$load_outputs(pw)); tw <- app$wb_tables(ow, pw)
check("the tree lists every deployment by site and every barologger", setequal(tw$key, c("dep:20000001__Unlisted_20000001__2025", "dep:20000003__Unlisted_20000003__2025", "baro:20000002")) &&
      tw[key == "baro:20000002", group] == "Barologgers")
dw <- app$wb_load("dep:20000001__Unlisted_20000001__2025", pw); rw <- removed_values("20000001", pw)
stw <- app$wb_states(dw, c("pres_kpa", "depth_m", "temp_c"), rw, wlr_period_to_manual(period_own_rows(file.path(pw, "config", PERIOD_FILE), "remove")), NULL, "20000001", "Unlisted 20000001")
i300 <- which(dw$TIMESTAMP == wt[100]); i0 <- which(dw$TIMESTAMP == as.POSIXct("2025-06-03 02:00", tz = "UTC"))
check("a value removed by a limit shows its old value; the depth worked out from it shows as removed with it; a removal by hand is marked as such",
      stw$pres_kpa$state[i300] == "rem" && stw$pres_kpa$old[i300] == 300 && stw$pres_kpa$rule[i300] == "range_max" && stw$depth_m$state[i300] == "rem" &&
      stw$temp_c$state[i0] == "rem" && stw$temp_c$rule[i0] == "manual" && all(stw$temp_c$state[-c(i0 + (-2:3))] == ""))
stw2 <- app$wb_states(dw, c("pres_kpa", "temp_c"), rw, data.table(serial = character(), site = character(), start = character(), end = character(), parameter = character(), min_value = character(), max_value = character()),
                      data.table(serial = "20000001", site = "", start = "2025-06-08 00:00", end = "2025-06-08 01:00"), "20000001", "")
check("with its row deleted a removal by hand is marked to come back; a saved out-of-water period is pending on the pressure, not on the temperature",
      stw2$temp_c$state[i0] == "back" && stw2$pres_kpa$state[which(dw$TIMESTAMP == as.POSIXct("2025-06-08 01:00", tz = "UTC"))] == "pend" && all(stw2$temp_c$state != "pend"))
tt <- dw$TIMESTAMP[1:10]
rr <- app$wb_removal_rows(dw$TIMESTAMP, c("depth_m", "depth_m", "temp_c", "temp_c", "pres_kpa"), tt[c(2, 3, 2, 3, 7)], "20000001", "S", " why ", "me")
check("removal rows: a depth is removed at its source, the pressure; one row per run of records, parameters sharing a run in one row",
      identical(rr$parameter, c("pres_kpa|temp_c", "pres_kpa")) && identical(rr$start, fmt_ts(tt[c(2, 7)])) && identical(rr$end, fmt_ts(tt[c(3, 7)])) && all(rr$serial == "20000001") && all(rr$reason == "why") &&
      identical(app$wb_removal_rows(dw$TIMESTAMP, c("temp_c", "depth_m"), tt[c(4, 5)], "20000001", "S", "x", "", parameter = "records")$parameter, "records") && identical(names(rr), MANUAL_COLS))
if (!requireNamespace("xml2", quietly = TRUE)) cat("  skip  the Workbench server checks need the xml2 package\n") else {
  ui_defaults <- function(html) {         # the value a browser sends for each input when the page opens
    x <- xml2::read_html(paste0("<html><body>", html, "</body></html>")); a <- xml2::xml_attr; f <- xml2::xml_find_all; out <- list()
    for (n in f(x, "//input[@id][not(@type) or @type='text' or @type='number' or @type='password']")) {
      v <- a(n, "value"); if (is.na(v)) v <- ""; if (identical(a(n, "type"), "number")) v <- suppressWarnings(as.numeric(v)); out[[a(n, "id")]] <- v }
    for (n in f(x, "//textarea[@id]")) out[[a(n, "id")]] <- xml2::xml_text(n)
    for (n in f(x, "//input[@type='checkbox'][@id]")) out[[a(n, "id")]] <- !is.na(a(n, "checked"))
    for (n in f(x, "//div[contains(@class,'shiny-input-radiogroup')][@id]")) { v <- a(f(n, ".//input[@checked]"), "value"); if (length(v)) out[[a(n, "id")]] <- v[1] }
    for (n in f(x, "//div[contains(@class,'shiny-input-checkboxgroup')][@id]")) out[[a(n, "id")]] <- a(f(n, ".//input[@checked]"), "value")
    for (n in f(x, "//select[@id]")) { o <- f(n, ".//option[@selected]"); if (!length(o)) o <- f(n, ".//option"); v <- a(o, "value"); out[[a(n, "id")]] <- if (length(v)) v[1] else "" }
    out }
  dvw <- ui_defaults(hw); rdw <- function(f) fread(file.path(pw, "config", f), colClasses = "character", na.strings = NULL)
  selw <- function(g, c, r1, r2 = r1) list(sig = g$sig, c = as.list(c), r1 = as.list(r1), r2 = as.list(r2), ar = r1[1], ac = c[1])
  both <- function(input, output, session) { std <- app$server(input, output, session, embedded = TRUE); wb <- app$wb_server(input, output, session, std) }
  testServer(both, {
    suppressWarnings({ do.call(session$setInputs, dvw); session$flushReact() })
    session$setInputs(wb_table = "dep:20000001__Unlisted_20000001__2025")
    session$setInputs(wb_params = c("depth_m", "pres_kpa", "temp_c"), wb_period = as.Date(c("2025-06-01", "2025-06-10")))
    g <- wb$grid(); r300 <- which(g$w$TIMESTAMP == wt[100])
    check("the Workbench runs on the Standard server: its editors and outputs, the deployment charted over the grid",
          identical(wb$E, std$E) && g$n == nrow(dw) && identical(g$params, c("depth_m", "pres_kpa", "temp_c")) && g$states$pres_kpa$state[r300] == "rem" &&
          grepl("No cell selected", as.character(output$wb_inspector$html)))
    session$setInputs(wb_grid_sel = selw(g, 1, r300))
    ins <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(output$wb_inspector$html)))
    check("a selected depth shows its lineage: the file, the logger, the barologger it was compensated with, the conversion and the rule",
          grepl("20000001.csv", ins, fixed = TRUE) && grepl("Stitched into serial 20000001", ins, fixed = TRUE) && grepl("Compensated with Baro 20000002", ins, fixed = TRUE) &&
          grepl("Converted", ins, fixed = TRUE) && grepl("Removed by range_max", ins, fixed = TRUE))
    cb <- wb$chart_base()
    check("the chart gets the period's records with the removed pressure's old value", cb$type == "base" && length(cb$series[[2]]$t) == g$n && 300 %in% as.numeric(cb$rem[[2]]$v))
    session$setInputs(wb_grid_sel = selw(g, c(1, 3), c(30, 30), c(32, 31)))
    session$setInputs(wb_act_remove = 1); session$setInputs(wb_rm_reason = "bad", wb_rm_author = "WB", wb_do_remove = 1)
    rmw <- function() { d <- rdw(PERIOD_FILE); d[action == "remove"] }
    m <- rmw()[note == "bad"]
    check("Remove writes removal rows into the corrections over a period: the depth at its source, the pressure, one row per run",
          nrow(m) == 2 && setequal(m$parameter, c("pres_kpa", "temp_c")) && setequal(m$to, fmt_ts(g$w$TIMESTAMP[c(32, 31)])) && all(m$target == "20000001|Unlisted 20000001") && all(m$author == "WB"))
    check("the removal shows as saved but not applied, in the Standard run state too",
          all(wb$grid()$states$depth_m$state[30:32] == "pend") && wb$grid()$states$temp_c$state[31] == "pend" && isTRUE(std$rv$stale) && grepl("Set-up changed", output$runstate$html))
    session$setInputs(wb_grid_sel = selw(g, 3, 31))
    session$setInputs(wb_act_restore = 1); session$setInputs(wb_do_restore = 1)
    check("Restore deletes the rows that cover the selected values, and only those", identical(rmw()[note == "bad", parameter], "pres_kpa"))
    invisible(capture.output({ session$setInputs(run = 1); if (isTRUE(std$rv$stale)) session$setInputs(run_force = 1) }))   # no workbook: the set-up check asks first
    d <- app$wb_load("dep:20000001__Unlisted_20000001__2025", pw)
    check("Run in the Workbench is the Standard run, and it redoes only the logger with the new removal",
          all(is.na(d$pres_kpa[30:32])) && all(is.na(d$depth_m[30:32])) && !is.na(d$temp_c[31]) && !isTRUE(std$rv$stale) && grepl("Up to date", output$runstate$html) &&
          identical(app$run_redone(pw), " Redone: 1 of 2 water loggers; the rest was unchanged and reused.") && identical(settings_read(app$SETTINGS_FILE)$project_dir, pw))
    session$setInputs(`mlist-found` = 1); ml <- rdw(DEPLOY_SHEET)
    check("the deployment sheet starts from the loggers found: a row each, barologgers typed Baro, the rest left for people to fill in",
          setequal(ml$serial, c("20000001", "20000002", "20000003")) && ml[serial == "20000002", type] == "Baro" && all(ml$site == "") && all(c("name", "T_ST", "visit") %in% names(ml)))
    session$setInputs(`mlist-paste` = 1); session$setInputs(`mlist-paste_txt` = "serial\tname\tsite\tcolour\n20000001\tShallow well\tLake 3\tblue")
    session$setInputs(`mlist-map_1` = "serial", `mlist-map_2` = "name", `mlist-map_3` = "site", `mlist-map_4` = "+colour", `mlist-imp_mode` = "update"); session$setInputs(`mlist-imp_go` = 1)
    ml <- rdw(DEPLOY_SHEET); fs <- rdw("metadata_fields.csv")
    check("rows pasted from a spreadsheet update the logger's row, and a column kept as new is added to the sheet and its columns",
          nrow(ml) == 3 && ml[serial == "20000001", name] == "Shallow well" && ml[serial == "20000001", site] == "Lake 3" && ml[serial == "20000001", colour] == "blue" &&
          "colour" %in% fs$field && fs[field == "colour", use] == "" && isTRUE(std$rv$stale))
    g <- wb$grid(); session$setInputs(wb_grid_sel = selw(g, 2, 60, 62))
    session$setInputs(wb_act_oow = 1); session$setInputs(wb_ow_cause = "download", wb_ow_reason = "downloading", wb_ow_author = "WB", wb_do_oow = 1)
    o <- rdw("out_of_water.csv")
    check("Out of water writes an out_of_water.csv row for the run of records", identical(names(o), OOW_COLS) && nrow(o) == 1 && o$cause == "download" && o$start == fmt_ts(g$w$TIMESTAMP[60]) && o$end == fmt_ts(g$w$TIMESTAMP[62]))
    session$setInputs(wb_act_drop = 1); session$setInputs(wb_dr_reason = "in the air", wb_dr_author = "WB", wb_do_drop = 1)
    check("Drop records writes a manual removal of whole records", nrow(rmw()[parameter == "records" & note == "in the air"]) == 1)
    session$setInputs(wb_act_clock = 1)
    session$setInputs(wb_ck_serial = "20000001", wb_ck_min = "60", wb_ck_start = "2025-06-04 00:00:00", wb_ck_end = "2025-06-05 00:00:00", wb_ck_reason = "clock test", wb_ck_author = "WB", wb_do_clock = 1)
    tc <- rdw("time_corrections.csv")
    check("Clock writes a time_corrections.csv row for the logger", identical(names(tc), TIME_COLS) && nrow(tc) == 1 && tc$serial == "20000001" && tc$shift_minutes == "60")
    session$setInputs(wb_grid_sel = selw(g, 1, 1, 3))
    session$setInputs(wb_act_dates = 1); session$setInputs(wb_dt_what = "deployed", wb_dt_reason = "in the air at the start", wb_dt_author = "WB", wb_do_dates = 1)
    dp <- rdw("deployments.csv")
    check("In / out dates writes a deployments.csv row: the logger went in after the selected records",
          identical(names(dp), DEPLOY_OVERRIDE_COLS) && nrow(dp) == 1 && dp$serial == "20000001" && dp$year == "2025" && dp$deployed == fmt_ts(g$w$TIMESTAMP[4]))
    session$setInputs(wb_act_meas = 1); session$setInputs(wb_ms_param = "depth_m", wb_ms_value = "0.61", wb_ms_method = "tape", wb_ms_author = "WB", wb_ms_notes = "", wb_do_meas = 1)
    ms <- rdw("manual_measurements.csv")
    check("Measurement writes a manual_measurements.csv row at the selected time", identical(names(ms), MEAS_COLS) && nrow(ms) == 1 && ms$value == "0.61" && ms$site == "Unlisted 20000001")
    session$setInputs(wb_table = "baro:20000002"); session$flushReact()
    check("a barologger opens in the same view", identical(wb$part()$kind, "baro") && "pres_kpa" %in% wb$params_all())
  })
}

check("every run leaves a log in output/logs that says it finished", grepl("^Run finished", utils::tail(readLines(sort(list.files(file.path(file.path(wb_root, "p"), "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]), 1)))
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE); source(file.path(here, "tests", "samples.R"), local = TRUE)
  r <- check_reader_samples(file.path(here, "tests", "samples"), SAMPLE_READ); new <- r[is.na(matches_expected), file]
  if (length(new)) cat("  (expected results saved for", length(new), "new sample(s): check them in", file.path("tests", "samples", "expected"), ")\n")
  for (f in r[!(accepted), file]) cat("  ", f, ": not accepted. Check tests/samples/expected/", f, ".rds, then accept_sample(\"tests/samples\", \"", f, "\", \"<your name>\")\n", sep = "")
  for (f in r[reference_ok %in% FALSE, file]) cat("  ", f, ": a value in tests/samples/reference.csv differs from what the reader makes\n", sep = "")
  check("every sample file reads, with rows (or a header for a format read only that far), the same twice and the same as its expected result, accepted, and equal to its hand-checked reference values",
        nrow(r) > 0 && all(r$rerun_identical) && !any(r$matches_expected %in% FALSE) && all(r$rows > 0 | r$kind %in% SAMPLE_NO_ROWS) && all(r$accepted) && !any(r$reference_ok %in% FALSE)) })
local({ sd <- file.path(here, "tests", "samples"); hd <- list.files(sd, "^hobo_decoded__", full.names = TRUE); ex <- list.files(sd, "^hobo_decoded_export_", full.names = TRUE)
  if (!length(hd) || !length(ex)) return(invisible())                  # the decoded .hobo against HOBOware's export of the same file, row by row
  a <- read_hobo_decoded(hd[1])$data; e <- read_hobo_csv(ex[1]); u <- e$meta$units
  b <- e$data[, .(TIMESTAMP, pres = to_kpa(pres, u[["pres"]], CFG), temp = if (grepl("F$", u[["temp"]])) convert_unit(temp, "degF", "degC") else temp)]
  m <- merge(a, b, by = "TIMESTAMP"); inner <- m[-c(1L, .N)]
  check("a .hobo read directly gives HOBOware's records: the same times, and every reading but the first and last within the export's rounding (0.001 kPa and C)",
        nrow(m) == nrow(a) && nrow(m) == nrow(b) && max(abs(inner$pres.x - inner$pres.y)) < 0.001 && max(abs(inner$temp.x - inner$temp.y)) < 0.001)
  if (abs(m$pres.x[1] - m$pres.y[1]) > 0.01 || abs(m$pres.x[nrow(m)] - m$pres.y[nrow(m)]) > 0.01) cat(sprintf("  known  core hobo.R: the first and last records differ from HOBOware (%.1f and %.1f kPa against %.1f and %.1f)\n",
        m$pres.x[1], m$pres.x[nrow(m)], m$pres.y[1], m$pres.y[nrow(m)]))
  h <- read_hobo_binary_header(list.files(sd, "^hobo_binary__", full.names = TRUE)[1])
  check("the .hobo header gives the serial and time zone its HOBOware export states", identical(h$serial, e$meta$serial) && identical(h$offset_hours, e$meta$offset_hours) && grepl("^HOBO U20", h$model)) })
local(source(file.path(here, "tests", "test_bad_files.R"), local = TRUE))   # damaged raw files and barometric records that do not fit: a clear problem or flag
local(source(file.path(here, "tests", "test_golden.R"), local = TRUE))      # the golden project: a whole run against accepted outputs
local(source(file.path(here, "tests", "test_formulas.R"), local = TRUE))    # derived values against published tables, the maker's software or hand-computed cases
check("no app function replaces a core function of the same name", !length(core_shadows(app_code_files(PIPELINE_DIR))))
check("each cached step has its own code fingerprint, the same when computed again", identical(STEP_HASH, step_hashes(globalenv())) && all(nchar(unlist(STEP_HASH)) == 12))
local({                                                               # the shared deployment sheet (core deployments.R)
  r <- dep_clean(data.table(serial = c("1000002", "1000002", "777"), station = c("ST01", "ST02", "Pond"), from = c("2022-06-01", "2023-09-22", ""), to = c("2023-09-21", "", "")))$rows
  w <- wlr_deploy_rows(r)
  check("deployment sheet rows become one logger-list row per season: a logger moved in September changes site with the season; no dates, every season",
        identical(w[site == "ST01", year], c("2022", "2023")) && w[site == "ST02", year][1] == "2024" && w[site == "ST01"][2, visit] == "2023-09-21" && w[serial == "777", year] == "") })
local({                                                               # damaged input (core testkit.R): runs that must not stop
  source(file.path(CORE_DIR, "testkit.R"), local = TRUE); sd <- file.path(here, "tests", "samples")
  if (!length(list.files(sd, "__"))) { cat("  (no sample files in tests/samples: damaged-input runs skipped)\n"); return(invisible()) }
  rn <- function(raw, p) invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", dgps_dir = "", project_dir = p, make_plots = FALSE)))))
  d <- damaged_file_runs(sd, rn); bad <- d[result != "ok"]; if (nrow(bad)) print(bad)
  check(paste0("a damaged raw file (", paste(unique(d$damage), collapse = ", "), ") never stops a run: ", nrow(d), " runs"), !nrow(bad))
  raw0 <- tempfile("raw0"); dir.create(raw0); for (f in list.files(sd, "__", full.names = TRUE)) if (!dir.exists(f)) file.copy(f, file.path(raw0, sub("^.*?__", "", basename(f))))
  p0 <- tempfile("p0"); rn(raw0, p0); t <- damaged_table_runs(p0, function() rn(raw0, p0)); bad <- t[result != "ok" & !startsWith(result, "The set-up table")]; if (nrow(bad)) print(bad)
  check(paste0("a damaged or emptied set-up table never stops a run without saying which table: ", nrow(t), " runs"), !nrow(bad)) })
if (!identical(Sys.getenv("OVID_SKIP_UI"), "1") && requireNamespace("chromote", quietly = TRUE) && !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) local({
  source(file.path(CORE_DIR, "uitest.R"), local = TRUE)
  none <- tempfile("none_"); dir.create(none)                                    # no metadata or dGPS folder: only the test project is read
  ui <- tryCatch(ui_click_through(normalizePath(here), c(WLR_PROJECT_DIR = file.path(wb_root, "p"), WLR_RAW_DIRS = wraw, WLR_METADATA_DIR = none, WLR_DGPS_DIR = none)), error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check("every page opens in a browser with no output, JavaScript or server error (the test project)", nrow(ui) > 5 && !nrow(bad))
  # a run with nothing to note (no problems, every deployment within the tape limit): Needs you and the side panel were "Error: ..."
  gp <- tempfile("wlr_golden_ui"); dir.create(file.path(gp, "config"), recursive = TRUE)
  file.copy(list.files(file.path(here, "tests", "golden", "config"), full.names = TRUE), file.path(gp, "config"))
  invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = file.path(here, "tests", "golden", "raw"), metadata_dir = "", dgps_dir = "", project_dir = gp,
                                                             unzoned_offset_h = -7, make_plots = FALSE)))))
  ui <- tryCatch(ui_click_through(normalizePath(here), c(WLR_PROJECT_DIR = gp, WLR_RAW_DIRS = file.path(here, "tests", "golden", "raw"), WLR_METADATA_DIR = none, WLR_DGPS_DIR = none), workbench = FALSE),
                 error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check("every page opens with no error after a run that found nothing to note (the golden project)", nrow(ui) > 5 && !nrow(bad)) })
local({                                                               # the configuration zip and the app's helpers (Standard interface)
  cp <- tempfile("cfgp"); dir.create(file.path(cp, "config"), recursive = TRUE)
  fwrite(data.table(serial = "20000001", site = "Lake 3", owner = "A"), file.path(cp, "config", "metadata.csv"))
  fwrite(data.table(serial = "20000001", site = "S", from = "2025-01-01", to = "2025-01-02"), file.path(cp, "config", "review_decisions.csv"))
  fwrite(data.table(serial = "1", site = "Pond", deployed = "", removed = "", T_ST = "", notes = ""), file.path(cp, "config", "deployments.csv"))
  z <- tempfile(fileext = ".zip"); export_config(cp, list(drop_stray_records = FALSE, raw_dirs = c("a", "b")), z)
  imp <- read_config_import(z)
  check("the configuration zip carries the deployment sheet (the logger list moved in), the review decisions and the correction switches, and none is read as another table",
        identical(names(imp$extra), "review_decisions.csv") && "deployments" %in% names(imp) && imp$deploy_sheet$owner == "A" && imp$deploy_sheet$site == "Lake 3" &&
        "drop_stray_records" %in% imp$settings$setting && "drop_stray_records" %in% app$SET_KEYS && "drop_bad_clock" %in% app$SET_KEYS)
  oz <- tempfile(fileext = ".zip"); od <- tempfile("oldz"); dir.create(od)
  fwrite(data.table(serial = "20000009", name = "Old one"), file.path(od, "metadata.csv")); fwrite(data.table(serial = "20000009", site = "", start = "", end = "", parameter = "temp_c", reason = "old"), file.path(od, "manual_removals.csv"))
  zip::zip(oz, c("metadata.csv", "manual_removals.csv"), root = od); io <- read_config_import(oz)
  check("an older version's configuration zip is read into the tables they are part of now", io$deploy_sheet$name == "Old one" && io$period$action == "remove" && io$period$parameter == "temp_c")
  np <- tempfile("cfgq"); dir.create(np)
  check("importing it writes the deployment sheet and the review decisions into the new project", all(c("deploy_sheet", "review_decisions.csv") %in% apply_config_import(imp, np)) &&
        fread(file.path(np, "config", DEPLOY_SHEET), colClasses = "character")$owner == "A")
  s <- app$config_settings_list(imp$settings, app$SET_KEYS)
  check("its settings come back as settings: lists split again, switches as TRUE / FALSE", identical(s$raw_dirs, c("a", "b")) && identical(s$drop_stray_records, FALSE))
  check("the Automatic corrections step says how many switches are on, not always on by default",
        identical(app$corrections_status(modifyList(CFG, list(project_dir = cp, drop_bad_clock = FALSE)))[[1]], "info") &&
        grepl("off", app$corrections_status(modifyList(CFG, list(project_dir = cp, drop_bad_clock = FALSE)))[[2]]))
  rd <- tempfile("rawc"); dir.create(file.path(rd, "2025"), recursive = TRUE)
  for (f in c("a.csv", "b.txt", "c.xle", "d.hobo", "._a.csv", "e.pdf")) writeLines("x", file.path(rd, "2025", f))
  check("the raw files counted on Home, the Site page and the Workbench are the ones a run reads, without ._ copies", nrow(raw_files_found(rd)) == 4)
  check("the audit explains the air, drift and period-correction rules", all(c("launched_in_air", "drift_correction", paste0("period_", names(PERIOD_ACTIONS))) %in% app$RULE_INFO$rule))
})
local({                                                                         # values typed by hand, whole records (2026-10-14)
  t <- as.POSIXct("2026-07-14 00:00", tz = "UTC") + 1800 * 0:3
  d <- data.table(TIMESTAMP = t, site = "Moose", logger_id = "22055246", depth_m = c(0.40, NA, NA, 0.43))
  rows <- period_read_rows(data.table(target = c("22055246", "99999999"), parameter = "depth_m", from = format(t[2], "%Y-%m-%d %H:%M"), to = format(t[2], "%Y-%m-%d %H:%M"),
                                      action = "set_value", value = c("0.41", "0.5"), on = "on", note = "manual reading", author = "M", added = ""))$rows
  got <- list(); fl <- function(dt, p, k, check, detail) got[[length(got) + 1L]] <<- list(p = p, k = k, check = check)
  x <- suppressWarnings(wlr_set_values(copy(d), rows, c("22055246", "Moose"), "depth_m", fl))
  check("a typed value fills only the missing value of its own logger and time, and is flagged as typed by hand",
        isTRUE(all.equal(x$depth_m, c(0.40, 0.41, NA, 0.43))) && length(got) == 1 && got[[1]]$check == "hand_value" && got[[1]]$k == 2)
  x2 <- suppressWarnings(wlr_set_values(copy(d), period_read_rows(data.table(target = "22055246", parameter = "depth_m", from = "", to = "", action = "set_value", value = "9",
                                         on = "on", note = "", author = "", added = ""))$rows, "22055246", "depth_m", function(...) NULL))
  check("a typed value never replaces a value that is there", x2$depth_m[1] == 0.40 && x2$depth_m[4] == 0.43)
  check("set_value is one of the corrections over a period, not a drift choice", "set_value" %in% names(period_actions()) && !"set_value" %in% WLR_DRIFT_ACTIONS)
  if (exists("tw")) check("the Workbench lists a whole record first for each site with more than one deployment, and none for a site with one",
    { m <- tw[kind %in% c("dep", "site")]; n <- m[kind == "dep", .N, by = group]
      setequal(m[kind == "site", group], n[N > 1, group]) && all(m[, kind[1] == "site" || .N == 1, by = group]$V1) })
})
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad))
if (n_bad) quit(status = 1)
