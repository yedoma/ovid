# Tests for Ovid Core // CS. Run from the app folder:   Rscript tests/run_tests.R
# They build small synthetic logger files in a temporary folder, run the pipeline on them and check the results.
# Nothing outside the temporary folder is read or written. Exit status is 1 if any check fails.
Sys.setenv(CS_NO_RUN = "1", OVID_RUN_SYNC = "1")   # app runs happen in this process (core runner.R), so a test sees their result at once
.app <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
suppressMessages(source(file.path(.app, "cs_pipeline.R")))
say <- function(...) invisible(NULL)                                 # keep the pipeline quiet while testing
RES <- new.env(); RES$pass <- 0L; RES$fail <- character(); RES$group <- ""
group <- function(x) { RES$group <- x; cat("\n", x, "\n", sep = "") }
check <- function(label, cond) { ok <- tryCatch(isTRUE(cond), error = function(e) FALSE)
  if (ok) RES$pass <- RES$pass + 1L else RES$fail <- c(RES$fail, paste0(RES$group, ": ", label)); cat(if (ok) "  ok    " else "  FAIL  ", label, "\n", sep = "") }
root <- file.path(tempdir(), "dc_tests"); unlink(root, recursive = TRUE); dir.create(root)
toa5 <- function(file, serial, rows, table = "Table1", cols = c("AirTC_Avg", "RH"), program = "CPU:prog1.CR1", station = "TestStn", mtime = NULL) {
  hdr <- c(sprintf('"TOA5","%s","CR1000","%s","CR1000.Std.27","%s","1234","%s"', station, serial, program, table),
           paste0('"TIMESTAMP","RECORD",', paste0('"', cols, '"', collapse = ",")), paste0('"TS","RN",', paste(rep('""', length(cols)), collapse = ",")),
           paste0('"","",', paste(rep('"Avg"', length(cols)), collapse = ",")))
  writeLines(c(hdr, rows), file); if (!is.null(mtime)) Sys.setFileTime(file, mtime); invisible(file) }
row <- function(ts, rec, ...) paste0('"', ts, '",', rec, ",", paste(c(...), collapse = ","))
hr <- function(h, day = "2020-01-01") sprintf("%s %02d:00:00", day, h)
run <- function(raw, proj, ...) { cfg <- CFG; cfg$raw_dirs <- raw; cfg$project_dir <- proj; cfg$make_plots <- FALSE; cfg$write_stitched <- FALSE
  for (n in names(list(...))) cfg[[n]] <- list(...)[[n]]; invisible(capture.output(r <- run_pipeline(cfg))); r }
aud <- function(proj) load_audit(proj)
now <- Sys.time()

# ---------------------------------------------------------------------------------------------------------
group("1. Stitching and duplicate records")
raw <- file.path(root, "raw1"); dir.create(raw); proj <- file.path(root, "p1")
toa5(file.path(raw, "A_old.dat"), "99001", vapply(1:10, function(i) row(hr(i), i, sprintf("%.1f", -10 + i), 50 + i), ""), mtime = now - 9000)
invisible(file.copy(file.path(raw, "A_old.dat"), file.path(raw, "A_copy.dat"))); Sys.setFileTime(file.path(raw, "A_copy.dat"), now - 8000)
toa5(file.path(raw, "B_new.dat"), "99001", vapply(6:15, function(i) row(hr(i), if (i == 10) 1000 else i, if (i == 8) "7999" else if (i == 7) "5.5" else sprintf("%.1f", -10 + i), if (i == 9) '"NAN"' else 50 + i), ""), mtime = now)
toa5(file.path(raw, "C.dat"), "99001", c(row(hr(20), 20, "1.0", 60), row(hr(21), 21, "2.0", 61), row(hr(21), 22, "3.0", 62), row(hr(22), 23, "4.0", 63)), mtime = now - 5000)
r <- run(raw, proj); d <- load_station("99001", "Table1", proj); a <- aud(proj)
check("a byte-identical copy of a file is read once", sum(r$manifest$status == "duplicate_file") == 1)
check("one record per timestamp (18 distinct hours)", nrow(d) == 18 && !anyDuplicated(d$TIMESTAMP))
check("identical overlapping records are dropped and counted", sum(as.integer(a[rule == "duplicate_record", n])) == 1)
check("a real value beats a logger overflow code from a newer file", d[TIMESTAMP == as.POSIXct(hr(8), tz = "UTC"), air_temp] == -2)
check("a blank in the newer file is filled from the older file", d[TIMESTAMP == as.POSIXct(hr(9), tz = "UTC"), rh] == 59)
check("where two files disagree the newest file wins", d[TIMESTAMP == as.POSIXct(hr(7), tz = "UTC"), air_temp] == 5.5)
check("the value that lost is in the audit trail", any(a$rule == "overlap_conflict" & a$old_value == "-3" & grepl("07:00", a$timestamp)))
check("a repeated timestamp inside one file keeps the first record", d[TIMESTAMP == as.POSIXct(hr(21), tz = "UTC"), air_temp] == 2)
check("the run finished with no problems", nrow(r$problems) == 0)
fa <- fread(file.path(proj, "output", "audit", "file_audit.csv"))
check("the file audit has every .dat with its md5", nrow(fa) == length(list.files(raw, "\\.dat$")) && all(nchar(fa$md5) == 32))
check("each file read: records read = in a station file + dropped", all(fa[used == TRUE, read == in_output + dropped]) && all(fa[used == TRUE, reconciles]))
check("a byte-identical copy is listed but not read", any(fa$status == "duplicate_file" & !fa$used))
check("records read = records kept + records audited", { g <- r$registry; all(g$rows_read == g$rows_stitched + vapply(seq_len(nrow(g)), function(i) sum(as.integer(a[stage == "stitch" & level == "row" & logger_id == g$logger_id[i], n])), 0)) })

group("2. Run manifest, QC state and history outputs")
ad <- file.path(proj, "output", "audit"); m <- fread(file.path(ad, "run_manifest.csv"))
check("run_manifest.csv records the pipeline version", m[key == "pipeline_version", value] == PIPELINE_VERSION)
check("the manifest fingerprints every set-up table", all(c("config/aliases.csv", "config/qc_rules.csv", "config/stations.csv") %in% m$key) && all(nchar(m[grepl("^config/", key), value]) == 32))
check("run_history.csv has one row per run", nrow(fread(file.path(ad, "run_history.csv"))) == 1)
check("lineage.csv and program_history.csv are written", all(file.exists(file.path(ad, c("lineage.csv", "program_history.csv", "qc_summary.csv")))))

group("3. Incremental runs")
before <- tools::md5sum(list.files(file.path(proj, "output", "stations"), full.names = TRUE))
out <- capture.output(r2 <- { cfg <- CFG; cfg$raw_dirs <- raw; cfg$project_dir <- proj; cfg$make_plots <- FALSE; cfg$write_stitched <- FALSE; run_pipeline(cfg) })
check("a second run gives byte-identical station files", identical(unname(before), unname(tools::md5sum(list.files(file.path(proj, "output", "stations"), full.names = TRUE)))))
check("the second run added a row to the run history", nrow(fread(file.path(ad, "run_history.csv"))) == 2)
check("unchanged logger tables were reused", fread(file.path(ad, "run_manifest.csv"))[key == "tables_reused", as.integer(value)] == nrow(r2$registry))
check("unchanged stations were reused", fread(file.path(ad, "run_manifest.csv"))[key == "stations_reused", as.integer(value)] == nrow(r2$stations))
st_files <- function(p) { f <- list.files(file.path(p, "output", "stations"), full.names = TRUE); setNames(unname(tools::md5sum(f)), basename(f)) }
raw3 <- file.path(root, "raw3"); dir.create(raw3); p3 <- file.path(root, "p3"); p3f <- file.path(root, "p3_full")
for (s in 1:2) for (m in 1:3) toa5(file.path(raw3, sprintf("S%d_%d.dat", s, m)), sprintf("9910%d", s), vapply(1:24, function(h) row(sprintf("2020-0%d-01 %02d:00:00", m, h - 1), h, sprintf("%.1f", s + m + h / 10), 50), ""), station = paste0("S", s), mtime = now - 9000)
invisible(run(raw3, p3))
toa5(file.path(raw3, "S2_2.dat"), "99102", vapply(1:24, function(h) row(sprintf("2020-02-01 %02d:00:00", h - 1), h, if (h == 5) "-3.0" else sprintf("%.1f", 4 + h / 10), 50), ""), station = "S2", mtime = now)
out <- capture.output(r3 <- { cfg <- CFG; cfg$raw_dirs <- raw3; cfg$project_dir <- p3; cfg$make_plots <- FALSE; cfg$write_stitched <- FALSE; run_pipeline(cfg) })
mf <- fread(file.path(p3, "output", "audit", "run_manifest.csv"))
check("after one file changes, only its logger table is stitched again", mf[key == "tables_reused", as.integer(value)] == 1)
check("and only the station it feeds is rebuilt", mf[key == "stations_reused", as.integer(value)] == 1)
invisible(run(raw3, p3f, incremental = FALSE))
check("the reused run writes the same station files as a full run", identical(st_files(p3), st_files(p3f)))
forget_files(p3, file.path(normalizePath(raw3), "S1_1.dat"), "99101", "Table1"); invisible(run(raw3, p3))
mf <- fread(file.path(p3, "output", "audit", "run_manifest.csv"))
check("rescanning one file stitches its logger table again and reuses the rest", mf[key == "tables_reused", as.integer(value)] == 1 && mf[key == "stations_reused", as.integer(value)] == 2)
check("the changed value reached the station file", any(load_station("99102", "Table1", p3)$air_temp == -3, na.rm = TRUE))
st3 <- read_cfg(file.path(p3, "config", "stations.csv")); st3[logger_id == "99102", station := "ignore"]; fwrite(st3, file.path(p3, "config", "stations.csv"))
r4 <- run(raw3, p3); src3 <- fread(file.path(p3, "output", "audit", "sources.csv"), colClasses = "character")
check("a logger set aside on the Stations page is not read, and its files are recorded", sum(r4$manifest$status == "set_aside") == 3 && !"99102" %in% r4$stations$station &&
        any(aud(p3)$rule == "set_aside") && "99102" %in% src3$logger_id)
st3[logger_id == "99102", station := ""]; fwrite(st3, file.path(p3, "config", "stations.csv")); r4 <- run(raw3, p3)
check("bringing it back reads it again", "99102" %in% r4$stations$station && !any(r4$manifest$status == "set_aside"))

group("3b. Only what changed is redone")
raw3b <- file.path(root, "raw3b"); dir.create(raw3b); p3b <- file.path(root, "p3b"); t3b <- function(h) format(as.POSIXct("2021-01-01", tz = "UTC") + h * 3600, "%Y-%m-%d %H:%M:%S")
toa5(file.path(raw3b, "A.dat"), "99201", vapply(1:48, function(h) row(t3b(h), h, sprintf("%.1f", 2 + h / 10), 60 + h %% 5), ""), mtime = now - 9000)
toa5(file.path(raw3b, "B.dat"), "99202", vapply(1:48, function(h) row(t3b(h), h, sprintf("%.1f", 1 + h / 10), sprintf("%.2f", 0.5 + (h %% 7) / 10)), ""), cols = c("AirTC_Avg", "Odd_Avg"), mtime = now - 9000)
invisible(run(raw3b, p3b))
odd <- setdiff(names(load_station("99202", "Table1", p3b)), c(META_COLS, "air_temp"))      # the parameter only the second station has
reused3b <- function() { m <- fread(file.path(p3b, "output", "audit", "run_manifest.csv")); c(tables = m[key == "tables_reused", as.integer(value)], stations = m[key == "stations_reused", as.integer(value)]) }
cf3b <- function(f) file.path(p3b, "config", f)
q3b <- read_cfg(cf3b("qc_rules.csv")); fwrite(rbind(q3b, data.table(pattern = paste0("^", odd, "$"), max = "100"), fill = TRUE), cf3b("qc_rules.csv")); invisible(run(raw3b, p3b))
check("a quality rule for a parameter only one station has rebuilds only that station", length(odd) == 1 && identical(reused3b(), c(tables = 2L, stations = 1L)))
pc3b <- function(...) { p <- cf3b(PERIOD_FILE); d <- if (file.exists(p)) read_cfg(p) else NULL; fwrite(rbind(d, data.table(...), fill = TRUE), p) }
pc3b(target = "99201", parameter = "air_temp", from = t3b(5), to = t3b(6), action = "remove", note = "t"); invisible(run(raw3b, p3b))
check("a manual removal rebuilds only its station, and is applied", identical(reused3b(), c(tables = 2L, stations = 1L)) && sum(is.na(load_station("99201", "Table1", p3b)$air_temp)) == 2)
pc3b(target = "", parameter = odd, action = "scale", value = "2", note = "t"); invisible(run(raw3b, p3b))
check("a calibration for a parameter only one station has rebuilds only that station", identical(reused3b(), c(tables = 2L, stations = 1L)))
# An older version's calibrations.csv and manual_removals.csv: moved once into the corrections over a period, same results
p3m <- file.path(root, "p3m"); invisible(run(raw3b, p3m)); cf3m <- function(f) file.path(p3m, "config", f)
fwrite(data.table(applies_to = "99201", parameter = "air_temp", output = c("", "air3"), multiplier = c("2", "3"), offset = c("1", "0"), start = c(t3b(10), ""), end = c(t3b(20), ""), notes = "old"), cf3m("calibrations.csv"))
fwrite(data.table(station = "99201", stream = "", parameter = "air_temp", start = t3b(30), end = t3b(31), reason = "old", author = ""), cf3m("manual_removals.csv"))
invisible(run(raw3b, p3m)); d3m <- load_station("99201", "Table1", p3m); h3m <- as.numeric(difftime(d3m$TIMESTAMP, as.POSIXct("2021-01-01", tz = "UTC"), units = "hours"))
a3m <- ifelse(h3m >= 10 & h3m < 20, (2 + h3m / 10) * 2 + 1, 2 + h3m / 10); pc3m <- read_cfg(cf3m(PERIOD_FILE))
check("an older project's calibrations and manual removals give the results they gave: offset and scale until the end, a new column, the removal",
      isTRUE(all.equal(d3m$air_temp[!h3m %in% 30:31], a3m[!h3m %in% 30:31])) && all(is.na(d3m$air_temp[h3m %in% 30:31])) && isTRUE(all.equal(d3m$air3, a3m * 3)) &&
      identical(pc3m$action, c("scale", "offset", "scale", "remove")) && pc3m$to[1] == "2021-01-01 19:59:59" && file.exists(cf3m("calibrations.csv")) && file.exists(cf3m("manual_removals.csv")))
invisible(run(raw3b, p3m)); d3m2 <- load_station("99201", "Table1", p3m)
check("the old rows are moved once and never applied twice", nrow(read_cfg(cf3m(PERIOD_FILE))) == 4 && isTRUE(all.equal(d3m2$air_temp, d3m$air_temp)) &&
      file.exists(file.path(p3m, "output", "audit", "calibrations_applied.csv")))
g3b <- read_cfg(cf3b("groups.csv")); fwrite(rbind(data.table(pattern = "^odd", group = "Odd things"), g3b, fill = TRUE), cf3b("groups.csv"))
i3b <- read_cfg(cf3b("parameter_info.csv")); i3b$notes[1] <- "edited"; fwrite(i3b, cf3b("parameter_info.csv")); invisible(run(raw3b, p3b))
check("editing parameter groups or the parameter guide rebuilds no station", identical(reused3b(), c(tables = 2L, stations = 2L)))
a3b <- read_cfg(cf3b("aliases.csv")); fwrite(rbind(data.table(pattern = "^Odd_Avg$", parameter = "odd_named"), a3b, fill = TRUE), cf3b("aliases.csv")); invisible(run(raw3b, p3b))
check("an alias for a column only one logger has rebuilds only the station it feeds", identical(reused3b(), c(tables = 2L, stations = 1L)) && "odd_named" %in% names(load_station("99202", "Table1", p3b)))
p3bf <- file.path(root, "p3b_full"); dir.create(p3bf); invisible(file.copy(file.path(p3b, "config"), p3bf, recursive = TRUE)); invisible(run(raw3b, p3bf, incremental = FALSE))
check("after all that, the station files are the ones a full run writes", identical(st_files(p3b), st_files(p3bf)))
tc3b <- file.path(root, "core_copy"); dir.create(tc3b); invisible(file.copy(file.path(CORE_DIR, c("util.R", "shell.R", "workbench.R")), tc3b)); core_real <- CORE_DIR; CORE_DIR <- tc3b
h1 <- code_hash(tc3b); cat("\n# a screen change\n", file = file.path(tc3b, "shell.R"), append = TRUE); cat("\n# a screen change\n", file = file.path(tc3b, "workbench.R"), append = TRUE)
h2 <- code_hash(tc3b); cat("\n# a pipeline change\n", file = file.path(tc3b, "util.R"), append = TRUE); h3 <- code_hash(tc3b); CORE_DIR <- core_real
check("the code fingerprint ignores the core files that only build screens, so an interface change redoes nothing", identical(h1, h2) && !identical(h2, h3))

group("4. Rules for single files and clock corrections")
fwrite(data.table(file = c("B_new.dat", "C.dat"), table = "Table1", action = c("assign", "ignore"), station = c("OtherSite", ""), stream = "", reason = "test", author = "t"), file.path(proj, "config", "file_rules.csv"))
fwrite(data.table(logger_id = "99001", table = "Table1", source_file = "A_copy.dat", start = hr(2), end = hr(3), action = "remove", shift_minutes = "", reason = "test", author = "t"), file.path(proj, "config", "time_corrections.csv"))
r <- run(raw, proj); a <- aud(proj)
check("a file sent to another station becomes its own station", "OtherSite" %in% r$stations$station && r$stations[station == "OtherSite", records] == 10)
check("a file left out is recorded, not silently dropped", any(r$manifest$status == "excluded_by_user") && any(a$rule == "excluded_by_user"))
check("a 'remove' correction drops exactly the records in its period", r$stations[station == "99001", records] == 8 && sum(as.integer(a[rule == "clock_fix_removed", n])) == 2)
fa <- fread(file.path(proj, "output", "audit", "file_audit.csv"))
check("the file audit gives the reason records were dropped", grepl("2 clock_fix_removed", fa[name == "A_copy.dat", dropped_why]) && all(fa[used == TRUE, reconciles]))
check("a file left out is in the file audit and not read", fa[name == "C.dat", status] == "excluded_by_user" && !fa[name == "C.dat", used])
fwrite(data.table(logger_id = "99001", table = "Table1", source_file = "A_copy.dat", start = "", end = "", action = "shift", shift_minutes = "60", reason = "test", author = "t"), file.path(proj, "config", "time_corrections.csv"))
r <- run(raw, proj); d <- load_station("99001", "Table1", proj)
check("a 'shift' correction moves the timestamps", min(d$TIMESTAMP) == as.POSIXct(hr(2), tz = "UTC") && nrow(d) == 10)
fwrite(data.table(logger_id = character(), table = character(), source_file = character(), start = character(), end = character(), action = character(), shift_minutes = character(), reason = character(), author = character()), file.path(proj, "config", "time_corrections.csv"))
fwrite(data.table(station = "99001", latitude = "", longitude = "", elevation_m = "", utc_offset_hours = "-7", snow_sensor_height_m = "", description = ""), file.path(proj, "config", "station_info.csv"))
fwrite(data.table(serial = "", model = "", file_pattern = "A_copy", start = "", end = "", offset_hours = "-6", reason = "clock left on MDT"), file.path(proj, "config", "timezones.csv"))
r <- run(raw, proj); d2 <- load_station("99001", "Table1", proj); a <- aud(proj)
check("the time-zone table moves a file on another zone into its station's zone", any(a$rule == "clock_fix_shifted" & grepl("time zone table", a$reason)) && nrow(d2) == nrow(d))
fwrite(data.table(serial = "", model = "", file_pattern = "A_copy", start = c("2019-12-01", "2020-02-01"), end = c("2019-12-31 23:00", ""), offset_hours = c("-5", "-6"), reason = "dated"), file.path(proj, "config", "timezones.csv"))
check("a dated time-zone row is matched by the file's first reading before the file is read",
      { g <- data.table(logger_id = "99001", table = "Table1", path = file.path(raw, "A_copy.dat"), first_ts = NA); t1 <- read_cfg(file.path(proj, "config", "timezones.csv"))
        !NROW(zone_corrections(g, t1, data.table(logger_id = "99001", table = "", station = "99001"), read_cfg(file.path(proj, "config", "station_info.csv")))) &&
        { t1[2, start := "2019-12-31"]; z <- zone_corrections(g, t1[2], data.table(logger_id = "99001", table = "", station = "99001"), read_cfg(file.path(proj, "config", "station_info.csv"))); NROW(z) == 1 && z$shift_minutes == "-60" } })
check("a time picked in a station table goes back to the logger's own clock for time_corrections.csv", {
  tc <- data.table(logger_id = "1", table = "", source_file = c("", "a.dat"), start = c("2020-01-01", ""), end = c("2020-12-31", ""), action = "shift",
                   shift_minutes = c("60", "-60"), reason = "", author = ""); t <- as.POSIXct("2020-06-01 12:00", tz = "UTC"); t9 <- as.POSIXct("2019-06-01", tz = "UTC")
  logger_time(t, tc, "a.dat") == t && logger_time(t, tc, "b.dat") == t - 3600 && logger_time(t9, tc, "b.dat") == t9 })
check("a station with no utc_offset_hours is taken to be in the base zone", {
  g <- data.table(logger_id = "99001", table = "Table1", path = file.path(raw, "A_copy.dat"), first_ts = NA)
  z <- zone_corrections(g, data.table(serial = "", model = "", file_pattern = "A_copy", start = "", end = "", offset_hours = "-6", reason = ""),
                        data.table(logger_id = "99001", table = "", station = "99001"), data.table(station = "99001", utc_offset_hours = ""), base_zone = -7)
  NROW(z) == 1 && z$shift_minutes == "-60" && !NROW(zone_corrections(g, data.table(serial = "", model = "", file_pattern = "A_copy", start = "", end = "", offset_hours = "-6", reason = ""),
                        data.table(logger_id = "99001", table = "", station = "99001"), data.table(station = "99001", utc_offset_hours = ""), base_zone = -6)) })
check("the duplicate check copes with a station whose sample is missing", nrow(detect_duplicates(list(a = NULL, b = data.table(TIMESTAMP = Sys.time())))) == 0)

group("5. Quality rules: remove, flag, flat line, manual")
raw <- file.path(root, "raw5"); dir.create(raw); proj <- file.path(root, "p5")
temp <- c(5, 6, 99, 7, rep(3.3, 30), 8, 9); toa5(file.path(raw, "Q.dat"), "99005", vapply(seq_along(temp), function(i) row(format(as.POSIXct("2021-06-01", tz = "UTC") + i * 3600, "%Y-%m-%d %H:%M:%S"), i, temp[i], 40 + i %% 7), ""))
r <- run(raw, proj); d <- load_station("99005", "Table1", proj); fl <- fread(file.path(proj, "output", "audit", "flags.csv")); a <- aud(proj)
check("a value above the limit is removed and audited with its old value", is.na(d$air_temp[3]) && any(a$rule == "range_max" & a$old_value == "99"))
check("a stuck sensor is flagged, and the values are kept", nrow(fl[check == "flat_line" & parameter == "air_temp"]) == 1 && sum(d$air_temp == 3.3, na.rm = TRUE) == 30)
qr <- read_cfg(file.path(proj, "config", "qc_rules.csv"))
check("every quality rule has a lasting rule_id, and flags.csv names it with the WMO code, app and run", all(grepl("^Q-[0-9]+$", qr$rule_id)) && !anyDuplicated(qr$rule_id) &&
      fl[check == "flat_line" & parameter == "air_temp", rule_id] == qr[pattern == "^air_temp.*", rule_id] && fl[check == "flat_line" & parameter == "air_temp", qc_code] == 2L &&
      all(fl$app == "CS") && all(nzchar(fl$run_id)) && identical(tail(names(fl), 4), c("rule_id", "qc_code", "app", "run_id")))
qf <- file.path(proj, "output", "qc", "99005__Table1_removed.csv")
check("the removed value is kept in output/qc with the value it had", file.exists(qf) && fread(qf)[parameter == "air_temp", old_value] == 99)
q <- read_cfg(file.path(proj, "config", "qc_rules.csv")); q[pattern == "^air_temp.*", `:=`(action = "flag", max_step_per_hour = "")];   # no step test here: it would also flag the jump to 99
 fwrite(q, file.path(proj, "config", "qc_rules.csv"))
fwrite(data.table(station = "99005", stream = "", parameter = "rh", start = "2021-06-01 05:00", end = "2021-06-01 06:00", reason = "test", author = "t"), file.path(proj, "config", "manual_removals.csv"))   # an older version's table: moved in on the run
r <- run(raw, proj); d <- load_station("99005", "Table1", proj); fl <- fread(file.path(proj, "output", "audit", "flags.csv"))
check("with action = flag the out-of-range value is kept and flagged", d$air_temp[3] == 99 && nrow(fl[check == "range_max"]) == 1)
check("a manual removal blanks only its parameter and period", sum(is.na(d$rh)) == 2 && !anyNA(d$air_temp))
x <- flagged_station("99005", "Table1", proj, scheme = "ovid")
check("the letter style marks suspect values S and removed values R", "air_temp_flag" %in% names(x) && x$air_temp_flag[3] == "S" && sum(x$rh_flag == "R") == 2)
ff <- file.path(proj, "output", "audit", "flags.csv"); fl0 <- readLines(ff); fa <- read_flags(proj)[, accepted := parameter == "air_temp"]; fwrite(fa, ff)
check("an accepted flag leaves the export's flag column, unless that switch is off",
      !any(flagged_station("99005", "Table1", proj, scheme = "ovid")$air_temp_flag %in% "S") && any(flagged_station("99005", "Table1", proj, scheme = "ovid", hide_accepted = FALSE)$air_temp_flag %in% "S"))
writeLines(fl0, ff)
w <- flagged_station("99005", "Table1", proj)
x5 <- c(1.449, 4.222, 1.450, 4.225, 1.498, 1.446, 1.436)
check("a spike test with confirm keeps a good reading between two dropouts", identical(qc_spike_idx(x5, 0.2), 2:4) && identical(qc_spike_idx(x5, 0.2, confirm = 5L), c(2L, 4L)))
tm <- as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
check("a step over less than an hour is judged on the change, not a rate (min_hours = 1)", identical(qc_step_idx(c(0, 0.3, 0.6), tm, 10), 2:3) && !length(qc_step_idx(c(0, 0.3, 0.6), tm, 10, min_hours = 1)))
dq <- default_qc()
check("new projects get spike and step limits for air temperature and snow depth", dq[pattern == "^air_temp.*", max_spike] == "10" && dq[pattern == "^snow_depth$", max_step_per_hour] == "0.3")
old <- copy(dq)[, `:=`(max_spike = "", max_step_per_hour = "")]; old[pattern == "^rh(_.*)?$", max_spike := "25"]
fs <- qc_fill_suggested(old)
check("filling suggested limits fills only blank cells and keeps what was typed", fs$filled > 0 && fs$table[pattern == "^rh(_.*)?$", max_spike] == "25" && fs$table[pattern == "^air_temp.*", max_step_per_hour] == "10" &&
      qc_fill_suggested(fs$table)$filled == 0L)
check("the export writes WMO codes unless told otherwise", "air_temp_qc" %in% names(w) && !"air_temp_flag" %in% names(w))
check("WMO export: a value above its limit is 3 (wrong) and a stuck run is 2 (doubtful)", w$air_temp_qc[3] == 3L && sum(w$air_temp_qc == 2L) == 30L)
check("WMO export: values a rule removed are 3 and an untouched checked value is 0", sum(w$rh_qc == 3L) == 2L && w$rh_qc[1] == 0L)
check("WMO export: every parameter has a _qc column, with only the codes of WMO table 0 33 020", all(setdiff(names(x), c(META_COLS, grep("_flag$", names(x), value = TRUE), "station", "stream", "TIMESTAMP", "RECORD", "logger_id", "source_file")) %in% sub("_qc$", "", grep("_qc$", names(w), value = TRUE))) && all(unlist(w[, grep("_qc$", names(w)), with = FALSE]) %in% WMO_QC$code))
check("WMO codes: the table is the official eight, and a failure outranks good, estimated and missing",
      identical(WMO_QC$code, 0:7) && identical(WMO_QC$name[c(1, 4, 5, 8)], c("good", "wrong", "not_checked", "missing")) &&
      qc_wmo_combine(0L, 3L) == 3L && qc_wmo_combine(6L, 2L) == 2L && qc_wmo_combine(7L, 4L) == 4L && qc_wmo_combine(1L, 3L) == 3L && qc_wmo_class("a_check_nobody_registered") == 2L)

group("5b. Wind direction is averaged on the circle")
check("a mean of 359, 0 and 1 degree is north, not 120", { m <- circ_mean_deg(c(359, 0, 1)); m < 1e-9 || m > 360 - 1e-9 })
check("the Yamartino spread is 0 for a steady wind and about 104 degrees for opposite winds", yamartino_sd(rep(90, 10)) < 1e-6 && abs(yamartino_sd(c(0, 180)) - 103.92) < 0.1)
check("the resultant of two equal winds from 350 and 10 degrees blows from north at 10 cos 10", { r <- wind_resultant(c(10, 10), c(350, 10)); abs(r[["speed"]] - 9.848) < 0.001 && (r[["dir"]] < 1e-6 || r[["dir"]] > 360 - 1e-6) })
wt <- as.POSIXct("2022-06-01", tz = "UTC") + 3600 * (0:47)
wdt <- data.table(station = "W1", stream = "t", logger_id = "1", TIMESTAMP = wt, wind_speed = 5, wind_dir = rep(c(359, 1), 24))
wp <- derive_products(wdt, list(sites = data.table(station = "W1", snow_sensor_height_m = NA)), list(min_complete = 0.5, water_year_start_month = 10))
check("the daily wind direction is north and the spread and resultant are written", all(wp$daily$wind_dir < 1 | wp$daily$wind_dir > 359) && all(c("wind_dir_sd_calc", "wind_speed_resultant_calc", "wind_dir_resultant_calc") %in% names(wp$daily)) && all(abs(wp$daily$wind_speed_resultant_calc - 5 * cos(pi / 180)) < 0.001))

group("5c. Radiation, humidity, icing and rain checks; gauge correction constants")
rt <- as.POSIXct("2022-06-21", tz = "UTC") + 1800 * (0:95)
cdt <- data.table(station = "R1", stream = "t", logger_id = "1", TIMESTAMP = rt, sw_in = pmax(0, 800 * sin(pi * ((as.numeric(rt) %% 86400) / 3600 - 4) / 16)), lw_in = 300, rh = 60, air_temp = 5, wind_speed = 3, rain = 0)
cdt[10, sw_in := 1500]; cdt[11, lw_in := 800]; cdt[12, rh := 110]
cdt[20:30, `:=`(air_temp = -5, rh = 95, wind_speed = 0)]; cdt[60:62, `:=`(air_temp = -3, rain = 0.2)]
FLAGS$list <- list(); invisible(cross_checks(cdt, data.table(latitude = 61, longitude = -121, utc_offset_hours = -7)))
fl5 <- rbindlist(FLAGS$list, fill = TRUE)
check("sunlight above the BSRN limit, longwave outside 40 to 700 and humidity above 103 are flagged", sum(fl5[check == "sw_above_physical_limit", records]) == 1 && sum(fl5[check == "lw_outside_physical_limit", records]) == 1 && sum(fl5[check == "rh_above_103", records]) == 1)
check("a frozen anemometer is flagged after 3 hours and rain below freezing is flagged", sum(fl5[check == "possible_icing", records]) == 11 && sum(fl5[check == "tips_below_freezing", records]) == 3)
check("normal days and nights raise none of them", !any(fl5[check %in% c("sw_above_physical_limit", "possible_icing"), records] > 11))
ce <- function(a, b, cc, U, Ta) exp(-a * U * (1 - atan(b * Ta) + cc))
check("the undercatch constants are Table 2 of Kochendorfer et al. (2017), gauge height and 10 m", identical(unname(UNDERCATCH$single_alter), c(0.0348, 1.366, 0.779, 7.2)) && identical(unname(UNDERCATCH$unshielded), c(0.0785, 0.729, 0.407, 7.2)) &&
      identical(unname(UNDERCATCH$single_alter_10m), c(0.0281, 1.628, 0.837, 9)) && identical(unname(UNDERCATCH$unshielded_10m), c(0.0623, 0.776, 0.431, 9)) && CE_SIGMA == 0.2)
dpd <- derive_products(data.table(station = "D", stream = "t", logger_id = "1", TIMESTAMP = as.POSIXct("2022-06-01", tz = "UTC") + 3600 * (0:47), air_temp = 20, rh = c(rep(50, 24), rep(100, 24))), list(sites = data.table(station = "D", snow_sensor_height_m = NA)), list(min_complete = 0.5, water_year_start_month = 10))
local({
  tm <- as.POSIXct("2022-06-01", tz = "UTC") + 3600 * (0:23)
  sw <- c(rep(0, 6), 100, 400, rep(800, 8), 400, 100, rep(0, 6))
  so <- c(rep(0, 6), 60, 120, rep(160, 8), 120, 60, rep(0, 6)) # high reflection at low sun
  da <- derive_products(data.table(station = "A", stream = "t", logger_id = "1", TIMESTAMP = tm, sw_in = sw, sw_out = so),
                        list(sites = data.table(station = "A", snow_sensor_height_m = NA)), list(min_complete = 0.5, water_year_start_month = 10))
  check("daily albedo is the day's reflected over incoming shortwave (Oke 1987), not the mean of each record's ratio",
        isTRUE(all.equal(da$daily$albedo_calc[1], round(sum(so[sw > 50]) / sum(sw[sw > 50]), 3))))
})
check("the dew point is 9.3 C at 20 C and 50 percent, and equals the air temperature at saturation", abs(dpd$daily$dew_point_calc[1] - 9.27) < 0.05 && abs(dpd$daily$dew_point_calc[2] - 20) < 0.01)
check("a step test is a rate: 6 degrees in 30 minutes is flagged at 8 per hour, and the same limit at 2-hour spacing is not", {
  tm <- as.POSIXct("2022-01-01", tz = "UTC"); x <- c(0, 0.5, 6.5, 7, 7.2)
  a <- qc_step_idx(x, tm + 1800 * (0:4), 8); b <- qc_step_idx(x, tm + 7200 * (0:4), 8)
  identical(a, 3L) && length(b) == 0L && length(qc_step_idx(x, tm + c(0, 1800, 36000, 37800, 39600), 8, max_gap_hours = 1.5)) == 0L })
check("collection efficiency at 3 m/s and -5 C for a single-Alter gauge is 0.716", abs(ce(0.0348, 1.366, 0.779, 3, -5) - 0.7157) < 0.001)

group("5d. Exports can put every station in UTC-6")
fwrite(data.table(station = "99005", latitude = "", longitude = "", elevation_m = "", utc_offset_hours = "-7", snow_sensor_height_m = "", description = ""), file.path(proj, "config", "station_info.csv"))
ez <- file.path(root, "tz_export.zip"); build_export(proj, data.table(station = "99005", stream = "Table1"), ez, out_offset = -6, summaries = FALSE, report = FALSE)
ux <- file.path(root, "tz_unzip"); utils::unzip(ez, exdir = ux); tx <- fread(list.files(ux, "99005__Table1.csv$", recursive = TRUE, full.names = TRUE)[1])
d0 <- load_station("99005", "Table1", proj)
check("a station in UTC-7 is moved one hour later to UTC-6, and says so", all(tx$time_zone == "UTC-6") && abs(as.numeric(difftime(as.POSIXct(tx$TIMESTAMP[1], tz = "UTC"), d0$TIMESTAMP[1], units = "hours")) - 1) < 1e-9)
fwrite(data.table(station = "99005", utc_offset_hours = ""), file.path(proj, "config", "station_info.csv"))
check("with no station offset and no zone chosen, the export asks which zone", inherits(tryCatch(build_export(proj, data.table(station = "99005", stream = "Table1"), ez, out_offset = -6, summaries = FALSE, report = FALSE), condition = function(e) e), "ovid_ask_zone"))
check("as stored, nothing is moved", { e2 <- file.path(root, "tz2.zip"); build_export(proj, data.table(station = "99005", stream = "Table1"), e2, summaries = FALSE, report = FALSE); u2 <- file.path(root, "tz2"); utils::unzip(e2, exdir = u2)
  t2 <- fread(list.files(u2, "99005__Table1.csv$", recursive = TRUE, full.names = TRUE)[1]); !"time_zone" %in% names(t2) && as.POSIXct(t2$TIMESTAMP[1], tz = "UTC") == d0$TIMESTAMP[1] })

group("5e. NetCDF, estimates, evapotranspiration, neighbours and breakpoints")
fwrite(data.table(station = "99005", latitude = "60.1", longitude = "-120.1", elevation_m = "280", utc_offset_hours = "-7", snow_sensor_height_m = "", wind_height_m = "", description = ""), file.path(proj, "config", "station_info.csv"))
e3 <- file.path(root, "nc.zip"); build_export(proj, data.table(station = "99005", stream = "Table1"), e3, summaries = FALSE, report = FALSE, netcdf = TRUE)
u3 <- file.path(root, "nc_unzip"); utils::unzip(e3, exdir = u3); ncf <- list.files(u3, "\\.nc$", recursive = TRUE, full.names = TRUE)
nx <- if (length(ncf)) nc_read(ncf[1]) else NULL
check("the export can add a CF NetCDF file per station table, in UTC with CF names and WMO codes", !is.null(nx) && nx$vars$air_temp$atts$standard_name == "air_temperature" &&
  nx$vars$time$data[1] == as.numeric(d0$TIMESTAMP[1]) + 7 * 3600 && sum(nx$vars$rh_qc$data == 3L, na.rm = TRUE) == 2 && nx$vars$lat$data == 60.1 && nx$atts$featureType == "timeSeries")
tg <- as.POSIXct("2021-01-01", tz = "UTC") + 1800 * 0:9
check("short gaps are filled only up to their limit", identical(fill_short(tg, c(1, NA, NA, 4, NA, NA, NA, NA, NA, 10), 2), 2:3) && !length(fill_short(tg, c(1, NA, NA, NA, NA, NA, 7, 8, 9, 10), 2)))
fe <- fao56_et(as.Date("2021-07-06"), 16.9, 21.5, 12.3, 73.5, 10 * 1000 / 3600, 22.07 / 0.0864, NULL, NULL, 50.8, 100, 10)
check("FAO-56 Penman-Monteith reproduces the Brussels example (Allen et al. 1998, example 18: 3.9 mm)", abs(fe$et0 - 3.9) < 0.2 && fe$et_pt > 0)
fe2 <- fao56_et(as.Date("2021-07-06"), 16.9, 21.5, 12.3, 73.5, 10 * 1000 / 3600, 22.07 / 0.0864, NULL, NULL, 50.8, 100, 10, alpha = 0.69)
pn <- fao56_et(as.Date(c("2021-10-15", "2021-12-21")), c(-20, -25), c(-15, -20), c(-25, -30), 80, 2, c(20, 0), NULL, NULL, 69, 0, 2)
pn1 <- fao56_et(as.Date("2021-12-21"), -25, -20, -30, 80, 2, 0, NULL, NULL, 69, 0, 2)
check("in the polar night the last day's cloudiness is carried forward (ASCE-EWRI 2005), not a fixed 0.5",
      pn$rn[2] != pn1$rn && is.finite(pn$rn[2]))
check("Priestley-Taylor ET scales with alpha (Scotty Creek bog 0.69, Connon et al. 2015)", isTRUE(all.equal(fe2$et_pt, round(fe$et_pt / 1.26 * 0.69, 2), tolerance = 0.02)))
sd0 <- data.table(DT = c(1.5, 1.5), air_temp = c(-30, 0), TCDT = c(1.4, 1.5))
check("the SR50 distance is corrected for air temperature (SR50A manual), a TCDT column is used as it is, and none leaves it as logged",
      isTRUE(all.equal(sr50_distance(sd0, c("DT", "air_temp"))$dist, 1.5 * sqrt(c(243.15, 273.15) / 273.15))) && identical(sr50_distance(sd0, c("DT", "air_temp", "TCDT"))$dist, c(1.4, 1.5)) &&
      identical(sr50_distance(sd0, c("DT", "air_temp"), "none")$dist, c(1.5, 1.5)) && is.null(sr50_distance(sd0, "air_temp")))
set.seed(2); dts <- as.Date("2019-01-01") + 0:1460; base <- 10 * sin(2 * pi * as.numeric(dts) / 365.25)
L <- rbindlist(lapply(1:4, function(k) data.table(kk = paste0("S", k, "__t"), Date = dts, parameter = "air_temp", value = base + k + rnorm(length(dts), 0, 0.3) + (k == 4) * (dts >= as.Date("2021-07-01")) * 1.5))); setnames(L, "kk", "key")
L <- L[!(key == "S1__t" & Date %in% (as.Date("2020-03-01") + 0:4))]
ne <- neighbour_estimates(L, 0.9, 60, 31); bp <- breakpoints(L)
check("a missing daily mean is estimated from the neighbour that tracks it, within its noise", nrow(ne[station == "S1"]) == 5 && max(abs(ne[station == "S1", value] - (base[match(ne[station == "S1", Date], dts)] + 1))) < 1.5 && all(ne$r2 > 0.9))
check("a station that shifts against the others is found at the right month, and the others are not", bp[station == "S4", result] == "break against the other stations" &&
  bp[station == "S4", break_month] %in% c("2021-06", "2021-07", "2021-08") && abs(bp[station == "S4", shift] - 1.5) < 0.4 && all(bp[station != "S4", p] > 0.01))

group("6. One record per station from several logger tables")
raw <- file.path(root, "raw6"); dir.create(raw); proj <- file.path(root, "p6")
d0 <- as.POSIXct("2022-03-01", tz = "UTC")
toa5(file.path(raw, "H.dat"), "99006", vapply(1:48, function(i) row(format(d0 + i * 3600, "%Y-%m-%d %H:%M:%S"), i, i / 10, 50), ""), table = "Hourly")
toa5(file.path(raw, "D.dat"), "99006", vapply(0:5, function(i) row(format(d0 + i * 86400, "%Y-%m-%d %H:%M:%S"), i, 20 + i, 60), ""), table = "Daily")
r <- run(raw, proj); d <- load_station_all("99006", proj)
check("both tables are joined into one record", nrow(attr(d, "tables")) == 2 && nrow(d) == 54)
check("on a day both tables cover, the shorter interval is used", !any(d[stream == "Daily" & as.Date(TIMESTAMP) %in% as.Date(c("2022-03-01", "2022-03-02")), !is.na(air_temp)]))
check("days only the coarser table covers are kept", sum(d[stream == "Daily", !is.na(air_temp)]) == 3)
broken_table <- function() {   # the Daily table cannot be read: the record is built from the rest, and the problem list says so
  real <- load_station; on.exit(assign("load_station", real, envir = globalenv()))
  assign("load_station", function(station, stream, ...) if (grepl("Daily", stream)) stop("damaged file") else real(station, stream, ...), envir = globalenv())
  n0 <- length(AUDIT$problems); d1 <- load_station_all("99006", proj); p <- problems_since(n0)
  nrow(attr(d1, "tables")) == 1 && nrow(p) == 1 && grepl("Daily left out", p$where) && grepl("damaged file", p$problem)
}
check("a table that cannot be read is left out and named in the problems, not dropped silently", broken_table())
check("files_used lists each file with its first and last record", { f <- files_used(d, "air_temp"); nrow(f) == 2 && all(c("First record used", "Last record used") %in% names(f)) })

group("7. Program history and station lineage")
raw <- file.path(root, "raw7"); dir.create(raw); proj <- file.path(root, "p7")
toa5(file.path(raw, "L1a.dat"), "70001", vapply(1:24, function(i) row(hr(i %% 24, "2019-05-01"), i, i, 50), "")[1:23], program = "CPU:v1.CR1")
toa5(file.path(raw, "L1b.dat"), "70001", vapply(1:23, function(i) row(hr(i, "2019-06-01"), i, i, 50, 12.5), ""), program = "CPU:v2.CR1", cols = c("AirTC_Avg", "RH", "BattV"))
toa5(file.path(raw, "L2.dat"), "70002", vapply(1:23, function(i) row(hr(i, "2019-09-01"), i, i, 50), ""), program = "CPU:v2.CR1")
r <- run(raw, proj); st <- read_cfg(file.path(proj, "config", "stations.csv")); st[, station := "SiteA"]; fwrite(st, file.path(proj, "config", "stations.csv")); r <- run(raw, proj)
ph <- fread(file.path(proj, "output", "audit", "program_history.csv"), colClasses = list(character = "logger_id")); ln <- fread(file.path(proj, "output", "audit", "lineage.csv"), colClasses = list(character = "logger_id"))
check("a program change is found, with the column that was added", nrow(ph[logger_id == "70001"]) == 2 && ph[logger_id == "70001"][2, change] == "program and columns changed" && ph[logger_id == "70001"][2, columns_added] == "BattV")
check("the lineage shows the second logger replacing the first", nrow(ln) == 2 && grepl("^first logger", ln$note[1]) && grepl("^replaced .*70001", ln$note[2]))

group("8. CR10X mixed-array files with an FSL")
raw <- file.path(root, "raw8"); dir.create(raw); proj <- file.path(root, "p8"); dir.create(file.path(proj, "config", "fsl"), recursive = TRUE)
writeLines(vapply(1:20, function(i) sprintf("101,2020,%d,1200,%.1f,%d", i, i / 2, 40 + i), ""), file.path(raw, "CR10X_site_final_storage_1.dat"))
writeLines(c("101 Output_Table  1440.00 Min", "1 101 L", "2 Year_RTM  L", "3 Day_RTM  L", "4 Hour_Minute_RTM  L", "5 AirT_AVG  L", "6 RH_AVG  L"), file.path(proj, "config", "fsl", "site.fsl"))
r <- tryCatch(run(raw, proj), error = function(e) NULL)
cx <- if (file.exists(file.path(proj, "config", "cr10x.csv"))) read_cfg(file.path(proj, "config", "cr10x.csv")) else NULL
check("a header-less mixed-array file is recognised and listed in cr10x.csv", !is.null(cx) && nrow(cx) == 1)
if (!is.null(cx)) { cx[, fsl := "site.fsl"]; fwrite(cx, file.path(proj, "config", "cr10x.csv")); r <- tryCatch(run(raw, proj), error = function(e) NULL) }
s <- if (!is.null(r)) r$stations else NULL; d <- if (!is.null(s) && nrow(s)) load_station(s$station[1], s$stream[1], proj) else NULL
check("its 20 daily records are read and timestamped from year, day and time", !is.null(d) && nrow(d) == 20 && min(d$TIMESTAMP) == as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))
check("its columns are named from the FSL (AirT_AVG and RH_AVG, then their common names)", !is.null(d) && all(c("air_temp", "rh") %in% names(d)) && all(c("AirT_AVG", "RH_AVG") %in% r$dictionary$raw_column))
fa <- if (file.exists(file.path(proj, "output", "audit", "file_audit.csv"))) fread(file.path(proj, "output", "audit", "file_audit.csv")) else data.table()
check("each array of a CR10X file has its own row in the file audit, and adds up", nrow(fa) >= 1 && all(grepl("#", fa[used == TRUE, file])) && all(fa[used == TRUE, reconciles]) && sum(fa$in_output, na.rm = TRUE) == 20)

group("9. Weighing gauges")
t <- seq(as.POSIXct("2023-01-01", tz = "UTC"), by = 1800, length.out = 48 * 20)
b <- cumsum(ifelse(seq_along(t) %% 48 == 10, 2, 0)); b[seq_along(t) > 48 * 10] <- b[seq_along(t) > 48 * 10] - 300 + 0     # 2 mm a day; emptied on day 11
b <- b + 400; b[200] <- b[200] - 0.3                                                                                     # a small dip (evaporation / noise) must not count
g <- c(list(station = "G", stream = "T", gauge_type = "geonor"), as.list(GAUGE_DEFAULTS$geonor), list(manual_servicing = "")); g$bucket_col <- "b"; g$noise_mm <- "0"; g$smooth_records <- "1"
p <- process_gauge(data.table(TIMESTAMP = t, b = b, source_file = "x.dat"), g)
check("precipitation is the gain in the bucket (2 mm a day for 20 days)", abs(p$info$total_mm - 40) < 0.01)
check("emptying the bucket is found and adds nothing", p$info$emptied == 1 && nrow(p$servicing) == 1)
check("a dip in the bucket is not counted when it recovers", max(p$daily$precip_mm, na.rm = TRUE) <= 2.001)
acc <- cumsum(ifelse(seq_along(t) %% 48 == 10, 2, 0)); acc[seq_along(t) >= 48 * 5 + 20] <- acc[seq_along(t) >= 48 * 5 + 20] + 150      # the running total jumps 150 mm on day 6
g2 <- c(list(station = "G", stream = "T", gauge_type = "pluvio2"), as.list(GAUGE_DEFAULTS$pluvio2), list(manual_servicing = "")); g2$bucket_col <- "b"; g2$acc_col <- "a"; g2$status_col <- ""
p2 <- process_gauge(data.table(TIMESTAMP = t, b = b, a = acc, source_file = "x.dat"), g2)
check("a one-day jump in the running total with no gain in the bucket is not counted", abs(p2$info$total_mm - 40) < 0.5 && p2$info$single_days_from_bucket >= 1)
g2$final_source <- "bucket"; p3 <- process_gauge(data.table(TIMESTAMP = t, b = b, a = acc, source_file = "x.dat"), g2)
check("final_source = bucket gives the bucket cummax throughout", abs(p3$info$total_mm - 40) < 0.01 && all(p3$daily$source %in% c("bucket", "none")))

acc2 <- cumsum(ifelse(seq_along(t) %% 48 == 10, 2, 0) + ifelse(seq_along(t) > 48 * 8 & seq_along(t) <= 48 * 16, 0.15, 0))   # days 9 to 16: 7 mm a day of phantom precipitation, too little for the single-day rule
g4 <- c(list(station = "G", stream = "T", gauge_type = "pluvio2"), as.list(GAUGE_DEFAULTS$pluvio2), list(manual_servicing = "")); g4$bucket_col <- "b"; g4$acc_col <- "a"; g4$status_col <- ""; g4$min_fault_days <- "99"
p4 <- process_gauge(data.table(TIMESTAMP = t, b = b, a = acc2, source_file = "x.dat"), g4)
g4$bucket_only <- "yes"; g4$bucket_only_start <- "2023-01-09"; g4$bucket_only_end <- "2023-01-16"; p5 <- process_gauge(data.table(TIMESTAMP = t, b = b, a = acc2, source_file = "x.dat"), g4)
check("without the bucket-only window the phantom precipitation is counted", p4$info$total_mm > 60)
check("with the bucket-only window the bucket is used inside its window only", abs(p5$info$total_mm - 40) < 0.01 && p5$info$bucket_only_days == 8 && all(p5$daily[qc_flag == "bucket_only_window", source] == "bucket") &&
        all(p5$daily[Date < as.Date("2023-01-09") & precip_mm > 0, source] == "acc_total"))
check("the bucket-only window dates can be moved", { g4$bucket_only_end <- "2023-01-12"; process_gauge(data.table(TIMESTAMP = t, b = b, a = acc2, source_file = "x.dat"), g4)$info$bucket_only_days == 4 })
check("the bucket-only window with no dates stops with a message, not a date written in the code", {
  g4$bucket_only_start <- ""
  e <- tryCatch(process_gauge(data.table(TIMESTAMP = t, b = b, a = acc2, source_file = "x.dat"), g4), error = function(e) conditionMessage(e))
  is.character(e) && grepl("Bucket-only window", e)
})
ti <- hydrometeor_temp(c(0, 2, 2), c(100, 100, 50))
check("the hydrometeor temperature is the air temperature in saturated air and colder in dry air (Harder and Pomeroy 2013)",
      isTRUE(all.equal(ti[1:2], c(0, 2), tolerance = 1e-6)) && ti[3] < 0)
check("the rain fraction is 1 / (1 + b c^Ti): 0.28 at 0 C saturated, near 0 well below and near 1 well above",
      isTRUE(all.equal(rain_fraction(0, 100), 1 / 3.630006)) && rain_fraction(-5, 90) < 0.01 && rain_fraction(5, 90) > 0.99)
check("phase: humid air at +1 C is mixed or rain, dry air at +1 C is snow; without humidity the -2/+2 C rule",
      precip_phase(1, 100) != "Snow" && precip_phase(1, 40) == "Snow" && identical(precip_phase(c(-3, 0, 3)), c("Snow", "Mixed", "Rain")))
check("Smith (2007) catch efficiency: 1.18 exp(-0.18 U) for snow, 1 below 1.2 m/s, 0.23 above 9 m/s, 0.95 for rain",
      isTRUE(all.equal(smith2007_ce(c(5, 0.5, 12), 0), c(1.18 * exp(-0.9), 1, 0.23))) && isTRUE(all.equal(smith2007_ce(5, 1), 0.95)))

group("10. Preflight")
proj <- file.path(root, "p1"); cfg <- CFG; cfg$raw_dirs <- file.path(root, "raw1"); cfg$project_dir <- proj
fwrite(data.table(file = character(), table = character(), action = character(), station = character(), stream = character(), reason = character(), author = character()), file.path(proj, "config", "file_rules.csv"))
fwrite(data.table(logger_id = character(), table = character(), source_file = character(), start = character(), end = character(), action = character(), shift_minutes = character(), reason = character(), author = character()), file.path(proj, "config", "time_corrections.csv"))
p <- preflight(cfg); check("an untouched set-up is safe to run", preflight_status(p) == "safe")
q <- read_cfg(file.path(proj, "config", "qc_rules.csv")); keep <- copy(q); q <- rbind(q, data.table(pattern = c("^bad(", "^x$"), min = c("", "10"), max = c("", "5")), fill = TRUE); fwrite(q, file.path(proj, "config", "qc_rules.csv"))
st <- read_cfg(file.path(proj, "config", "stations.csv")); keep_st <- copy(st)
fwrite(rbind(st, data.table(logger_id = "99001", station = c("A", "B"), start = c("2020-01-01", "2020-01-01"), end = c("2020-12-31", "2020-06-30")), fill = TRUE), file.path(proj, "config", "stations.csv"))
fwrite(data.table(logger_id = "99001", table = "", source_file = "", start = c("2020-01-01", "2020-01-01 06:00"), end = c("2020-01-02", "2020-01-03"), action = "shift", shift_minutes = c("60", "x"), reason = "", author = ""), file.path(proj, "config", "time_corrections.csv"))
p <- preflight(cfg)
check("a broken regular expression is an error", any(p$level == "error" & p$table == "qc_rules.csv" & grepl("regular expression", p$problem)))
check("min not below max is an error", any(p$table == "qc_rules.csv" & grepl("not below max", p$problem)))
check("one logger sent to two stations over the same dates is an error", any(p$table == "stations.csv" & grepl("sent to both", p$problem)))
check("a shift with no number of minutes is an error", any(p$table == "time_corrections.csv" & grepl("number of minutes", p$problem)))
check("the status is 'needs attention'", preflight_status(p) == "attention")
check("a raw folder that does not exist is reported", { c2 <- cfg; c2$raw_dirs <- file.path(root, "nowhere"); any(preflight(c2)$table == "folders") })
fwrite(keep, file.path(proj, "config", "qc_rules.csv")); fwrite(keep_st, file.path(proj, "config", "stations.csv"))

group("11. Small helpers")
check("seg_id starts a new segment after a missing record", all(seg_id(c(0, 10, 20, 40, 50), 10) == c(0, 0, 0, 1, 1)))
check("totals are drawn as bars and wind direction as points", identical(unname(plot_style_of(c("rain", "wind_dir", "air_temp"))), c("bar", "point", "line")))
check("parse_time reads dates with and without a time", !is.na(parse_time("2020-01-02")) && !is.na(parse_time("2020-01-02 03:04")) && is.na(parse_time("02/01/2020")))
check("safe_name makes a file-safe name", safe_name("Site A/1") == "Site_A_1")

check("sensor health finds a dead, a stopped and a stuck sensor", {
  tt <- as.POSIXct("2022-01-01", tz = "UTC") + 3600 * 0:(24 * 120 - 1); n <- length(tt)
  d <- data.table(station = "S", stream = "T", TIMESTAMP = tt, air_temp = sin(seq_len(n) / 10), dead = NA_real_, gone = sin(seq_len(n) / 7), rh = 50 + sin(seq_len(n)), rain_mm_Tot = 0)
  d[TIMESTAMP > tt[1] + 86400 * 60, gone := NA]; d[TIMESTAMP > tt[1] + 86400 * 20 & TIMESTAMP < tt[1] + 86400 * 40, rh := 80]
  h <- sensor_health(d, c("air_temp", "dead", "gone", "rh", "rain_mm_Tot")); g <- setNames(h$health, h$parameter)
  g[["air_temp"]] == "ok" && startsWith(g[["dead"]], "dead") && startsWith(g[["gone"]], "stopped") && startsWith(g[["rh"]], "stuck") && g[["rain_mm_Tot"]] == "ok" })
check("a report renders as HTML and as PDF", { d <- rpt_doc("Test", "sub", list(rpt_cards(c(a = 1)), rpt_heading("T"), rpt_table(data.table(year = 2023, x = 1234.5)), rpt_text("note")))
  f <- report_write(d, file.path(tempdir(), "rpt_test"), c("html", "pdf")); h <- paste(readLines(f[1], warn = FALSE), collapse = "")
  length(f) == 2 && grepl("<table", h) && grepl(">2023<", h) && identical(readBin(f[2], "raw", 4), charToRaw("%PDF")) })
check("the parameter overview groups parameters, pages them and thins large series", { set.seed(1); tt <- seq(as.POSIXct("2010-01-01", tz = "UTC"), by = "day", length.out = 4000)
  L <- rbindlist(lapply(1:30, function(i) data.table(group = if (i <= 24) "Ground temperature" else "Air temperature", parameter = paste0("p", i), time = tt, value = rnorm(4000))))
  pg <- rpt_overview(L, GB, per_page = 20L); length(pg) == 2 && all(vapply(pg, function(b) b$type == "plot", NA)) && length(rpt_doc("t", "", list(pg, NULL))$blocks) == 2 })
check("availability, statistics and quality charts are built from long and wide data", { set.seed(2); tt <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "6 hours", length.out = 800)
  w <- data.table(TIMESTAMP = tt, a = rnorm(800), b = rnorm(800)); w$b[100:300] <- NA; g <- c(a = "G1", b = "G2")
  av <- rpt_availability(w, c("a", "b"), g, "TIMESTAMP", GB); st <- rpt_stats(w, c("a", "b"), g)
  L <- data.table(row = rep(c("x", "y"), each = 800), group = "G", time = rep(tt, 2), status = sample(c("Valid", "Flagged"), 1600, TRUE))
  al <- rpt_availability_long(L, GB, c(Valid = "#00aa00", Flagged = "#ccaa00")); qb <- rpt_quality_bars(data.table(r = "a", v = 5), data.table(r = "b", v = 2), GB)
  length(av) == 1 && st$type == "table" && nrow(st$data) == 2 && st$data$values[2] == 599 && length(al) == 1 && qb$inline && all(c("ok", "warn", "error", "info", "off") %in% names(rpt_levels(GB))) })
check("a station report is written in the chosen formats", { ss <- fread(file.path(root, "p1", "output", "audit", "station_summary.csv"), colClasses = list(character = c("station", "stream")))
  f <- station_report(ss$station[1], ss$stream[1], file.path(root, "p1"), file.path(tempdir(), "st_rep.pdf"), formats = c("html", "pdf")); all(file.exists(f)) && length(f) == 2 })

# ---------------------------------------------------------------------------------------------------------
group("Logger programs")
raw <- file.path(root, "raw_prog"); dir.create(raw); proj <- file.path(root, "p_prog")
prog <- c("Public AirTC, RH", "Units AirTC = Deg C", "Units RH = %", "DataTable(Table1, True, -1)", "  DataInterval(0, 60, Min, 10)",
          "  Average(1, AirTC, FP2, False)", "  Sample(1, RH, FP2)", "EndTable", "BeginProg", "  Scan(10, Sec, 0, 0)",
          "    VoltSe(AirTC, 1, mV2500, 1, 0, 0, _60Hz, 0.1, -40)", "    CallTable Table1", "  NextScan", "EndProg")
pdir <- file.path(root, "progs"); dir.create(pdir)
writeLines(prog, file.path(pdir, "prog1.CR1")); writeLines(c("' renamed copy", prog), file.path(pdir, "Other.CR1"))
writeLines(c(";{CR10X}", "*Table 1 Program", "  01: 60        Execution Interval (seconds)", "1:  If time is (P92)", " 1: 0        Minutes", " 2: 60       Interval",
             " 3: 10       Set Output Flag High", "2:  Set Active Storage Area (P80)", " 1: 1        Final Storage Area 1", " 2: 101      Array ID",
             "3:  Real Time (P77)", " 1: 1220     Year,Day,Hour/Minute", "4:  Average (P71)", " 1: 2        Reps", " 2: 1        Loc [ Batt_Volt ]",
             "5:  Maximize (P73)", " 1: 1        Reps", " 2: 10       Value with Hr-Min", " 3: 2        Loc [ AirTemp ]", "End Program",
             "-Input Locations-", "1 Batt_Volt 1 1 1", "2 AirTemp 1 1 1", "-Program Security-"), file.path(pdir, "ST01_240.CSI"))
check("program files are copied into the project once", length(add_programs(list(project_dir = proj), list.files(pdir, full.names = TRUE))) == 3 &&
        !length(add_programs(list(project_dir = proj), list.files(pdir, full.names = TRUE))))
toa5(file.path(raw, "a.dat"), "99101", vapply(1:3, function(i) row(hr(i), i, "1.5", 50), ""))
toa5(file.path(raw, "b.dat"), "99102", vapply(1:3, function(i) row(hr(i), i, "1.5", 50), ""), program = "CPU:gone.CR1")
toa5(file.path(raw, "c.dat"), "99103", vapply(1:3, function(i) row(hr(i), i, "1.5", 50, 3), ""), cols = c("AirTC_Avg", "RH", "Extra"))
r <- run(raw, proj); pm <- fread(file.path(proj, "output", "audit", "program_matches.csv")); pm[, f := basename(path)]
check("a file is matched to the program named in its header", pm[f == "a.dat", status == "matched" & program == "prog1.CR1"])
check("a renamed program is found by its columns", pm[f == "b.dat", grepl("^matched by columns", status)])
check("a file whose columns the program does not write is reported", pm[f == "c.dat", grepl("differ", status)])
dc <- r$dictionary[logger_id == "99101" & raw_column == "AirTC_Avg"]
check("blank header units are filled from the program", dc$unit == "Deg C")
check("the column dictionary says how a column was measured and how often it was written", dc$measured_by == "VoltSe" && dc$program_interval == "60 min")
e <- parse_fsl(file.path(pdir, "ST01_240.CSI"))
check("an Edlog .CSI gives the labels Edlog writes in an .FSL", identical(e[["101"]], c("101", "Year_RTM", "Day_RTM", "Hour_Minute_RTM", "Batt_Volt_AVG", "AirTemp_AVG", "AirTemp_MAX", "AirTemp_Hr_Min_MAX")))
check("the program signature is computed as in a TOA5 header", is.numeric(cs_signature(charToRaw("x"))) && cs_signature(raw(0)) == 0xAAAA)

group("13. Corrupt timestamps, paired sensors and sensor swaps")
raw13 <- file.path(root, "raw13"); dir.create(raw13); p13 <- file.path(root, "p13")
tg <- seq(as.POSIXct("2020-10-01", tz = "UTC"), as.POSIXct("2020-12-31 23:00", tz = "UTC"), by = 3600)
tm <- seq(as.POSIXct("2021-03-01", tz = "UTC"), as.POSIXct("2021-03-10", tz = "UTC"), by = 3600)
set.seed(4); junk <- as.POSIXct("2020-10-01", tz = "UTC") + runif(30, 0, 75 * 86400)          # random stamps with odd seconds, inside the good months, written after them
ts13 <- c(format(tg, "%Y-%m-%d %H:%M:%S"), format(junk, "%Y-%m-%d %H:%M:%S"), format(tm, "%Y-%m-%d %H:%M:%S"))
toa5(file.path(raw13, "G.dat"), "99301", vapply(seq_along(ts13), function(i) row(ts13[i], i, if (i > length(tg) && i <= length(tg) + 30) "99.9" else "1.5", 60), ""))
r13 <- run(raw13, p13); a13 <- aud(p13); d13 <- load_station("99301", "Table1", p13)
check("corrupt timestamps among good records are left out and audited", sum(as.integer(a13[rule == "stray_record", n])) == 30 && !any(d13$air_temp == 99.9, na.rm = TRUE))
check("the good records before and after the corrupt stretch are all kept", nrow(d13) == length(tg) + length(tm))
cols2 <- c("AirTC_1_Avg", "AirTC_2_Avg", "AirT_High_Avg", "AirT_Low_Avg", "RH", "RH_Avg", "WS_ms_Avg", "WS_ms_2_Avg")
toa5(file.path(raw13, "P.dat"), "99302", vapply(1:24, function(h) row(hr(h - 1), h, -5, -6, -7, -8, 80, 81, 2, 3), ""), cols = cols2)
toa5(file.path(raw13, "W1.dat"), "99303", vapply(1:24, function(h) row(hr(h - 1), h, 1, 2.5), ""), cols = c("AirTC_Avg", "WS_ms_Avg"), program = "CPU:cup.CR1")
toa5(file.path(raw13, "W2.dat"), "99303", vapply(1:24, function(h) row(hr(h - 1, "2020-01-02"), h, 1, 4.5), ""), cols = c("AirTC_Avg", "WindSpeed_S_WVT"), program = "CPU:young.CR1")
r13 <- run(raw13, p13); a13 <- aud(p13); dp <- load_station("99302", "Table1", p13); dw <- load_station("99303", "Table1", p13)
check("two sensors and two heights keep separate names", all(c("air_temp", "air_temp_2", "air_temp_high", "air_temp_low", "wind_speed", "wind_speed_2") %in% names(dp)) &&
        all(dp$air_temp == -5) && all(dp$air_temp_2 == -6) && all(dp$wind_speed_2 == 3))
check("a sample written beside its average is kept apart, not merged", all(dp$rh == 81) && all(dp$rh_sample == 80) && any(a13$rule == "alias_kept_apart"))
check("a new wind sensor under a new column joins the old one in one series", all(dw$wind_speed[as.Date(dw$TIMESTAMP) == as.Date("2020-01-01")] == 2.5) &&
        all(dw$wind_speed[as.Date(dw$TIMESTAMP) == as.Date("2020-01-02")] == 4.5) && !"WindSpeed_S_WVT" %in% names(dw) && any(a13$rule == "sensor_change"))

# ---------------------------------------------------------------------------------------------------------
group("14. Campbell TOB3 binary files")
tob3 <- function(file, table, interval, frames, fields = c("AirTC_Avg", "RH"), types = c("IEEE4B", "FP2"), stamp = 4321L) {
  rs <- sum(tob3_type_size(types)); nrec <- 4L; fs <- 16L + nrec * rs
  hdr <- c('"TOB3","TestStn","CR1000","99401","CR1000.Std.32","CPU:tob.CR1","1234","2020-01-01 00:00:00"',
           sprintf('"%s","%s","%d","1000","%d","Sec100Usec","0","0"', table, interval, fs, stamp),
           paste0('"', fields, '"', collapse = ","), paste0('"', rep("", length(fields)), '"', collapse = ","),
           paste0('"', rep("Avg", length(fields)), '"', collapse = ","), paste0('"', types, '"', collapse = ","))
  u4 <- function(x) writeBin(as.integer(if (x >= 2^31) x - 2^32 else x), raw(), size = 4, endian = "little")
  fp2 <- function(x) { e <- 2L; m <- as.integer(round(abs(x) * 100)); writeBin(as.integer(bitwOr(bitwShiftL(e, 13L), m) + if (x < 0) 32768L else 0L), raw(), size = 2, endian = "big") }
  rec <- function(v) c(writeBin(v[1], raw(), size = 4, endian = "big"), fp2(v[2]))
  t90 <- function(t) as.numeric(as.POSIXct(t, tz = "UTC")) - as.numeric(as.POSIXct("1990-01-01", tz = "UTC"))
  body <- unlist(lapply(frames, function(f) {
    if (is.null(f)) return(rep(as.raw(255), fs))                                    # a never-written frame
    k <- nrow(f$v); recs <- unlist(lapply(seq_len(k), function(i) rec(f$v[i, ])))
    if (k == nrec) return(c(u4(t90(f$t)), u4(0), u4(f$r), recs, u4(stamp * 65536)))
    msz <- 16L + k * rs                                                              # a partly filled frame: one minor frame and an unused tail
    c(u4(t90(f$t)), u4(0), u4(f$r), recs, u4(stamp * 65536 + msz), rep(as.raw(0), fs - msz - 4L), u4(stamp * 65536 + 0x4000 + (fs - msz))) }))
  con <- file(file, "wb"); writeBin(charToRaw(paste0(paste(hdr, collapse = "\r\n"), "\r\n")), con); writeBin(body, con); close(con)
}
raw14 <- file.path(root, "raw14"); p14 <- file.path(root, "p14"); dir.create(raw14)
v <- cbind(seq(-10, 0.5, length.out = 9), seq(50, 58, length.out = 9))
tob3(file.path(raw14, "T.dat"), "Hourly", "60 MIN", list(list(t = "2020-03-01 00:00:00", r = 0, v = v[1:4, ]), NULL,
     list(t = "2020-03-01 04:00:00", r = 4, v = v[5:8, ]), list(t = "2020-03-01 08:00:00", r = 8, v = v[9, , drop = FALSE])))
tob3(file.path(raw14, "F.dat"), "ts_data", "100 MSEC", list(list(t = "2020-03-01 00:00:00", r = 0, v = v[1:4, ]), list(t = "2020-03-01 00:00:01", r = 4, v = v[5:8, ])))
x <- read_tob3(file.path(raw14, "T.dat"))
check("a TOB3 table is read with its timestamps and record numbers", nrow(x) == 9 && x$TIMESTAMP[9] == "2020-03-01 08:00:00" && identical(x$RECORD, as.numeric(0:8)))
check("TOB3 IEEE4B and FP2 values decode", isTRUE(all.equal(x$AirTC_Avg, v[, 1], tolerance = 1e-6)) && isTRUE(all.equal(x$RH, round(v[, 2], 2))))
r14 <- run(raw14, p14); m14 <- r14$manifest; d14 <- load_station("99401", "Hourly", p14)
check("a TOB3 file runs through the pipeline like a TOA5 file", m14[basename(path) == "T.dat", status] == "ok" && nrow(d14) == 9)
check("a 10 Hz TOB3 table is listed with its time span but not read", m14[basename(path) == "F.dat", status] == "skipped_table" &&
        format(m14[basename(path) == "F.dat", first_ts], tz = "UTC") == "2020-03-01")
check("the duplicate check copes with columns of other classes", { s <- data.table(TIMESTAMP = as.POSIXct("2020-01-01", tz = "UTC") + 0:30 * 864000, a = as.difftime(0:30 + 1, units = "secs"), b = 0:30 + 0.5)
  nrow(detect_duplicates(list(`A__T` = s, `B__T` = copy(s)))) == 1 })

group("15. Importing set-up tables, and combined records")
raw15 <- file.path(root, "raw15"); p15 <- file.path(root, "p15"); ex15 <- file.path(root, "ex15"); dir.create(raw15); dir.create(ex15)
toa5(file.path(raw15, "a.dat"), "501", c(row(hr(0), 1, 1.0, 50), row(hr(1), 2, 2.0, 50), row(hr(2), 3, 3.0, 50)), station = "OldA")
toa5(file.path(raw15, "b.dat"), "502", c(row(hr(2), 1, 3.5, 60), row(hr(3), 2, 4.5, 60), row(hr(4), 3, 5.5, 60)), table = "Hourly", station = "NewB")
invisible(run(raw15, p15))
fwrite(data.table(logger_id = c("501", "502", "502"), header_name = "", table = c("", "", "Hourly"), start = "", end = "", station = c("SiteA", "SiteB", ""),
                  stream = c("", "", "Table1"), seen_as = "", notes = ""), file.path(ex15, "stations.csv"))
fwrite(data.table(station = "SiteA", latitude = "60.1", longitude = "-120.1", elevation_m = "", utc_offset_hours = "-7", snow_sensor_height_m = "", description = ""), file.path(ex15, "station_info.csv"))
m15 <- list(stations = ensure_cols(read_cfg(file.path(ex15, "stations.csv")), cfg_cols("stations"))[, cfg_cols("stations"), with = FALSE],
            sites = ensure_cols(read_cfg(file.path(ex15, "station_info.csv")), cfg_cols("sites"))[, cfg_cols("sites"), with = FALSE])
done15 <- apply_config_import(m15, p15, "replace", "test", "imported rows")
st15 <- read_cfg(file.path(p15, "config", "stations.csv"))
check("an import replaces the tables it holds", setequal(done15, c("stations", "sites")) && nrow(st15[logger_id == "502"]) == 2 && st15[logger_id == "501", station] == "SiteA")
check("applying the same rows again changes nothing", length(apply_config_import(m15, p15, "replace", "test", "imported rows")) == 0)
check("an import keeps the previous table in config/history", length(list.files(file.path(p15, "config", "history"), "^stations__")) >= 1)
fwrite(data.table(composite = "airT", parameter = "air_temp", station = c("SiteA", "SiteB"), stream = "Table1", start = "", end = "", priority = c("1", "2"), offset = c("", "-0.5"), step = "", notes = ""),
       file.path(p15, "config", "composites.csv"))
r15 <- run(raw15, p15); c15 <- fread(file.path(p15, "output", "composites", "airT.csv"))
check("a combined record takes each time from the preferred station that has it", nrow(c15) == 5 && c15[TIMESTAMP == as.POSIXct(hr(2), tz = "UTC"), station] == "SiteA" && c15[4, station] == "SiteB")
check("a source's offset is added to its values", isTRUE(all.equal(c15$air_temp[4:5], c(4.0, 5.0))))
s15 <- fread(file.path(p15, "output", "audit", "composites.csv"))
check("the summary says what each source supplied and how it differs where they overlap", s15[station == "SiteB", records] == 2 && s15[station == "SiteB", median_difference] == 0)
check("the export zip carries the combined records", { z15 <- file.path(root, "ex15.zip"); build_export(p15, data.table(station = "SiteA", stream = "Table1"), z15, summaries = FALSE, report = FALSE)
  "campbell_export/composites/airT.csv" %in% utils::unzip(z15, list = TRUE)$Name })

group("16. Workbench interface (preview)")
# app.R in a sandbox: everything it defines, without starting the app. Settings go to a temporary file.
app <- new.env(); assign("shinyApp", function(...) invisible(NULL), envir = app)
local({ old <- setwd(.app); on.exit(setwd(old)); suppressMessages(sys.source("app.R", envir = app, keep.source = FALSE)) })
app$SETTINGS_FILE <- file.path(root, "app_settings.json"); app$VIEWER <- FALSE
app$SETTINGS <- as.list(app$CFG)                     # the defaults, not the settings saved on this computer (a test never reads or writes them)
ui_env <- Sys.getenv("OVID_UI"); Sys.unsetenv("OVID_UI")
check("the interface comes from ?ui= first, then OVID_UI, then the setting; Standard is the default", with(app,
      wb_wanted("?ui=workbench") && !wb_wanted("?ui=standard", "workbench") && wb_wanted("", "workbench") && !wb_wanted("") && !wb_wanted(NULL) &&
      { Sys.setenv(OVID_UI = "workbench"); x <- wb_wanted("") && !wb_wanted("?ui=standard"); Sys.unsetenv("OVID_UI"); x }))
if (nzchar(ui_env)) Sys.setenv(OVID_UI = ui_env)
check("both interfaces build their table editors from one list of set-up tables, and the Workbench opens each one where the Standard edits it",
      all(app$SETUP_TABLES$file %in% vapply(CONFIG_FILES, `[[`, "", "file")) && !anyDuplicated(app$SETUP_TABLES$id) && setequal(names(app$WB_OPEN), app$SETUP_TABLES$id) &&
      any(grepl("setup_editors(cfg_path, tick, mark)", deparse(app$server), fixed = TRUE)))
raw16 <- file.path(root, "raw16"); dir.create(raw16); p16 <- file.path(root, "p16")
t16 <- as.POSIXct("2021-06-01 12:00", tz = "UTC") + 0:71 * 3600
temp16 <- round(5 + sin(seq_along(t16) / 4), 2); temp16[c(13, 18)] <- 99; temp16[40:69] <- 3.3      # 99 at midnight and 05:00; a stuck run
toa5(file.path(raw16, "W.dat"), "99016", vapply(seq_along(t16), function(i) row(format(t16[i], "%Y-%m-%d %H:%M:%S"), i, temp16[i], 50 + i %% 5), ""))
invisible(run(raw16, p16))
rm16 <- app$wb_removed(p16, "99016__Table1")
check("removed values keep their time of day, also when one of them is at midnight",
      identical(format(rm16[parameter == "air_temp", timestamp], "%Y-%m-%d %H:%M", tz = "UTC"), c("2021-06-02 00:00", "2021-06-02 05:00")) && all(rm16$old_value == 99))
d16 <- load_station("99016", "Table1", p16); o16 <- suppressWarnings(app$load_outputs(p16)); fl16 <- app$wb_flags(o16$flags, "99016", "Table1")
x16 <- app$cs_scan_types(raw16, data.table(logger_id = "99016"), manifest = o16$manifest)
check("the scan shown with the folders counts files by type, matched to a station, new since the last run and changed in size",
      x16[`File type` == "Campbell TOA5", Found == "1" && `Matched to station` == "1" && New == "0" && `Changed size` == "0"] && app$cs_scan_types(raw16)[`File type` == "Campbell TOA5", New == "1"])
check("Summaries broken down give one statistic per year or water year; a one-station report is PDF unless only HTML is ticked", {
  d <- data.table(TIMESTAMP = as.POSIXct(c("2020-09-30 12:00", "2020-10-01 12:00", "2021-01-05 00:00"), tz = "UTC"), a = c(1, 3, NA))
  wy <- app$stat_breakdown(d, "a", "wy", "mean"); yr <- app$stat_breakdown(d, "a", "year", "miss")
  identical(names(wy), c("Parameter", "Unit", "2020", "2021")) && wy$`2020` == 1 && wy$`2021` == 3 && yr$`2020` == 0 && yr$`2021` == 100 &&
    app$one_report_format("html") == "html" && app$one_report_format(c("html", "pdf")) == "pdf" && app$one_report_format(character()) == "pdf" })
st16 <- app$wb_states(d16, c("air_temp", "rh"), rm16, fl16); gc16 <- app$wb_grid_cells(d16, c("air_temp", "rh"), st16)
check("each grid cell is passed, flagged or removed, on the row of its own time",
      identical(which(st16$air_temp$state == "rem"), c(13L, 18L)) && identical(which(st16$air_temp$state == "flag"), 40:69) && all(st16$rh$state == "") &&
      st16$air_temp$rule[13] == "range_max" && st16$air_temp$old[18] == 99 && grepl("Q-", st16$air_temp$rule[40]))
check("a removed cell shows its old value struck through, and a flagged cell is marked",
      grepl("\u2715</span><s>99.000</s>", gc16$txt[[1]][13], fixed = TRUE) && gc16$cls[[1]][13] == "rem" && gc16$cls[[1]][40] == "flag" && gc16$cls[[2]][13] == "")
gh16 <- as.character(app$wb_grid(c("t1", "t2", "t3"), c("a", "b"), list(c("1", "2", "3"), c("4", "5", "6")), list(c("", "rem", ""), c("", "", "flag")), "sig1",
                                  times = c(100, 200, 300), vals = list(c(1, 9, 3), c(4, NA, 6))))
check("the grid is a table of records by parameters, with row and column headings to select by, the state of each cell, and its time, parameter and value for the chart",
      lengths(regmatches(gh16, gregexpr('<th class="rh">', gh16))) == 3 && lengths(regmatches(gh16, gregexpr('<th class="ch" data-c=', gh16))) == 2 &&
      grepl('<td data-c="1" class="rem" data-v="9">2</td>', gh16, fixed = TRUE) && grepl('<td data-c="2">5</td>', gh16, fixed = TRUE) && grepl('data-sig="sig1"', gh16, fixed = TRUE) &&
      grepl('data-nr="3" data-nc="2"', gh16, fixed = TRUE) && grepl('<tr data-r="2" data-t="200">', gh16, fixed = TRUE) && grepl('data-c="2" data-p="b"', gh16, fixed = TRUE))
tc16 <- as.POSIXct("2021-01-01", tz = "UTC") + 0:9999 * 600; vc16 <- sin(seq_along(tc16) / 50); vc16[5000] <- 40; vc16[100:120] <- NA
dc16 <- data.table(TIMESTAMP = tc16, a = vc16); rc16 <- data.table(timestamp = tc16[c(10, 9000)], parameter = "a", rule = "x", old_value = c(-5, 7), reason = "")
fc16 <- data.table(parameter = "a", s = tc16[200], e = tc16[300]); pc16 <- data.table(p = "a", t = as.numeric(tc16[50]), v = vc16[50])
ch_thin <- app$wb_chart_data(dc16, "a", rc16, fc16, pc16, max = 2000); ch_full <- app$wb_chart_data(dc16[1:1000], "a", rc16, fc16, pc16, max = 2000)
check("the chart gets every record of a short period, missing ones as gaps; a long one thinned to the low and high of each stretch, so a spike still shows",
      !ch_full$decimated && length(ch_full$series[[1]]$t) == 1000 && sum(is.na(ch_full$series[[1]]$v)) == 21 && ch_full$step == 600 &&
      ch_thin$decimated && length(ch_thin$series[[1]]$t) <= 2000 && max(ch_thin$series[[1]]$v) == 40 && min(ch_thin$series[[1]]$v) == min(vc16, na.rm = TRUE) &&
      identical(as.numeric(ch_full$rem[[1]]$t), as.numeric(tc16[10])) && ch_thin$rem[[1]]$v[2] == 7 && ch_full$pend[[1]]$v == vc16[50] && ch_full$flags[[1]]$s == as.numeric(tc16[200]))
g0 <- list(params = c("a", "b"), w = data.table(TIMESTAMP = t16[1:10]), sig = "s")
sc <- app$wb_sel_cells(list(sig = "s", c = list(1, 2), r1 = list(2, 9), r2 = list(4, 12), ar = 2, ac = 1), g0)
check("a selection comes back as cells: runs of rows per column, cut to the rows shown; one made on other rows is ignored",
      identical(sc$r, c(2L, 3L, 4L, 9L, 10L)) && identical(sc$p, c("a", "a", "a", "b", "b")) && !nrow(app$wb_sel_cells(list(sig = "old", c = 1, r1 = 1, r2 = 2), g0)))
r16 <- data.table(pattern = c("^air_temp.*", "^air_temp.*"), applies_to = c("", "99016"), rule_id = c("Q-01", "Q-02"))
check("the rule shown for a cell is the station's own row before the general one",
      app$wb_rule_for(r16, "air_temp", "99016", "x")$rule_id == "Q-02" && app$wb_rule_for(r16, "air_temp", "other", "x")$rule_id == "Q-01" &&
      is.null(app$wb_rule_for(r16, "rh", "99016", "x")) && is.null(app$wb_rule_for(r16[2], "air_temp", "other", "x")))
check("times are labelled with the station's offset from Station details, or as logger time",
      app$wb_zone(-7) == "UTC\u22127" && app$wb_zone(5.5) == "UTC+5.5" && app$wb_zone(0) == "UTC" && app$wb_zone(NA_real_) == "logger time" && is.na(station_offsets(p16, "99016")))
m16 <- data.frame(station = c("99016", "", "other"), stream = c("", "Table1", ""), parameter = c("air_temp", "^rh$", ""), start = c("2021-06-01 16:00", "2021-06-02", ""),
                  end = c("2021-06-01 17:00:00", "2021-06-02", ""), reason = "", author = "", stringsAsFactors = FALSE)
check("manual removals are matched as the pipeline reads them: blank means all, a date-only end covers the day, other stations ignored",
      identical(app$wb_manual_cover(m16, "99016", "Table1", c("air_temp", "air_temp", "AIR_TEMP", "rh", "rh", "rh"), as.POSIXct(c("2021-06-01 16:00", "2021-06-01 17:30", "2021-06-01 17:00",
        "2021-06-02 23:30", "2021-06-03 00:00", "2021-06-01 16:30"), tz = "UTC")), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)))
tt <- as.POSIXct("2021-06-01", tz = "UTC") + 0:9 * 3600
rr <- app$wb_removal_rows(tt, c("a", "a", "a", "b", "b", "a"), tt[c(2, 3, 4, 3, 4, 8)], "S", "T", " why ", "me")
check("removal rows: one per run of consecutive records, parameters sharing a run in one row",
      identical(rr$parameter, c("a", "b", "a")) && identical(rr$start, fmt_ts(tt[c(2, 3, 8)])) && identical(rr$end, fmt_ts(tt[c(4, 4, 8)])) && all(rr$reason == "why") &&
      identical(app$wb_removal_rows(tt, c("a", "b"), tt[c(5, 5)], "S", "T", "x", "")$parameter, "a|b"))
app$SETTINGS$project_dir <- p16; app$SETTINGS$raw_dirs <- raw16; app$SETTINGS$skip_tables <- character(); app$SETTINGS$incremental <- TRUE; app$SETTINGS$import_cr10x <- FALSE
h16 <- as.character(htmltools::renderTags(app$wb_ui(list(QUERY_STRING = "ui=workbench")))$html)
ids16 <- regmatches(h16, gregexpr(' id="[^"]+"', h16))[[1]]
tools16 <- vapply(Filter(function(x) inherits(x, "ovid_tool"), app$std_pages(FALSE)), `[[`, "", "value")
check("the Workbench holds every page of the Standard interface, its own views, Import, Run (the Standard's) and Export, and no input twice",
      all(vapply(c(tools16, "wb-data", "wb-import"), function(v) grepl(paste0('class="tab-pane" data-value="', v, '"'), h16, fixed = TRUE) || grepl(paste0('data-value="', v, '"'), h16, fixed = TRUE), NA)) &&
      !anyDuplicated(ids16) && grepl('id="run"', h16, fixed = TRUE) && grepl('id="wb_export"', h16, fixed = TRUE) && grepl('data-ovid-go="wb-import"', h16, fixed = TRUE) &&
      grepl('href="?ui=standard"', h16, fixed = TRUE) && all(vapply(app$SETUP_TABLES$id, function(i) grepl(paste0('id="', i, '-tbl"'), h16, fixed = TRUE), NA)) &&
      all(vapply(c("wb_act_remove", "wb_act_restore", "wb_act_cal", "wb_act_clock", "wb_act_note", "raw_dirs", "project_dir"), function(i) grepl(paste0('id="', i, '"'), h16, fixed = TRUE), NA)) &&
      grepl('id="wb_chart"', h16, fixed = TRUE) && all(vapply(c("in", "out", "reset", "period"), function(a) grepl(paste0('data-wb-chart="', a, '"'), h16, fixed = TRUE), NA)))
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
    for (n in f(x, "//div[contains(@class,'shiny-date-input')][@id]")) { v <- a(f(n, ".//input"), "data-initial-date"); if (length(v) && !is.na(v[1]) && nzchar(v[1])) out[[a(n, "id")]] <- as.Date(v[1]) }
    for (n in f(x, "//div[contains(@class,'shiny-date-range-input')][@id]")) { v <- a(f(n, ".//input"), "data-initial-date"); if (length(v) == 2 && !anyNA(v) && all(nzchar(v))) out[[a(n, "id")]] <- as.Date(v) }
    out }
  dv16 <- ui_defaults(h16); rd16 <- function(f) fread(file.path(p16, "config", f), colClasses = "character", na.strings = NULL)
  sel16 <- function(g, c, r1, r2 = r1, ar = r1[1], ac = c[1]) list(sig = g$sig, c = as.list(c), r1 = as.list(r1), r2 = as.list(r2), ar = ar, ac = ac)
  both <- function(input, output, session) { std <- app$server(input, output, session, embedded = TRUE); wb <- app$wb_server(input, output, session, std) }
  testServer(both, {
    do.call(session$setInputs, dv16); session$flushReact()
    session$setInputs(wb_params = c("air_temp", "rh"), wb_period = as.Date(c("2021-06-01", "2021-06-04")))
    g <- wb$grid(); ins0 <- as.character(output$wb_inspector$html)
    check("the Workbench runs on the Standard server: its editors, outputs and project, the station table charted over the grid",
          identical(wb$E, std$E) && identical(wb$key(), "99016__Table1") && identical(g$params, c("air_temp", "rh")) && g$n == 72 && sum(g$states$air_temp$state == "rem") == 2 &&
          grepl("No cell selected", ins0) && grepl('data-nr="72" data-nc="2"', as.character(output$wb_grid$html), fixed = TRUE))
    session$setInputs(wb_grid_sel = sel16(g, 1, 13))
    ins <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(output$wb_inspector$html)))
    check("selecting a removed cell shows its time, its old value, its lineage and the rule that removed it, with the ways to correct it",
          grepl("2021-06-02 00:00 logger time", ins, fixed = TRUE) && grepl("Read 99.000 W.dat", ins, fixed = TRUE) && grepl("Removed by range_max", ins, fixed = TRUE) &&
          grepl("Rule Q-", ins, fixed = TRUE) && grepl("Rule for 99016", ins, fixed = TRUE))
    session$setInputs(wb_grid_sel = sel16(g, c(1, 2), c(5, 5), c(7, 6)))
    insm <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(output$wb_inspector$html))); cb <- wb$chart_base()
    check("a selection of records and parameters is summed up per parameter",
          nrow(wb$cells()) == 5 && grepl("5 values", insm, fixed = TRUE) && grepl("air_temp 3", insm, fixed = TRUE) && grepl("rh 2", insm, fixed = TRUE))
    check("the chart gets the period's records of the charted parameters, with the removed values and their old values",
          cb$type == "base" && identical(vapply(cb$series, `[[`, "", "p"), c("air_temp", "rh")) && length(cb$series[[1]]$t) == 72 && !cb$decimated &&
          identical(as.numeric(cb$rem[[1]]$v), c(99, 99)) && identical(as.numeric(cb$period), as.numeric(wb$period())) && cb$zone == "logger time")
    rl16 <- as.character(output$`rl-list`$html); nd16 <- as.character(output$wb_needs$html)
    check("Needs you is one list in both views: the run's flags and findings on the Needs you page, counted in the Workbench's Needs you",
          nrow(std$review$items()) > 0 && grepl("d-chk-row", rl16, fixed = TRUE) && grepl(paste0("<b class=\"w\">", nrow(app$wb_needs_items(std$needs$check(), std$review$items())), "</b>"), nd16, fixed = TRUE))
    check("the status bar sums up the selection and reports problems, reconciliation and the versions",
          grepl("5 selected", output$wb_status$html) && grepl("No problems", output$wb_status$html) && grepl(paste("workbench", app$OVID_WORKBENCH_VERSION), output$wb_status$html))

    session$setInputs(wb_grid_sel = sel16(g, c(1, 1, 2), c(5, 6, 6)))
    session$setInputs(wb_act_remove = 1); session$setInputs(wb_rm_reason = "bad", wb_rm_author = "WB", wb_do_remove = 1)
    rm16 <- function() { d <- rd16(PERIOD_FILE); d[action == "remove"] }
    m <- cs_period_to_manual(rm16())
    check("Remove writes removal rows into the corrections over a period: one per run of records, for this station table",
          identical(rm16()$target, c("99016", "99016")) && identical(m$parameter, c("air_temp", "rh")) && all(m$station == "99016") && all(m$stream == "Table1") &&
          identical(m$start, c("2021-06-01 16:00:00", "2021-06-01 17:00:00")) && identical(m$end, c("2021-06-01 17:00:00", "2021-06-01 17:00:00")) && all(m$reason == "bad") && all(m$author == "WB"))
    check("the removal shows as saved but not applied, in the Standard run state too",
          identical(wb$grid()$states$air_temp$state[5:6], c("pend", "pend")) && wb$grid()$states$rh$state[6] == "pend" && isTRUE(std$rv$stale) &&
          grepl("Set-up changed", output$runstate$html) && nrow(std$E$manual$data()) == 2)
    session$setInputs(wb_grid_sel = sel16(g, 2, 6))
    session$setInputs(wb_act_restore = 1); session$setInputs(wb_do_restore = 1)
    check("Restore deletes the removal rows that cover the selected values, and only those", identical(rm16()$parameter, "air_temp"))
    invisible(capture.output(session$setInputs(run = 1)))
    d <- load_station("99016", "Table1", p16)
    check("Run in the Workbench is the Standard run: it saves, checks and runs, and the saved removal is applied",
          all(is.na(d$air_temp[5:6])) && !is.na(d$rh[6]) && !isTRUE(std$rv$stale) && "manual" %in% fread(file.path(p16, "output", "qc", "99016__Table1_removed.csv"))[parameter == "air_temp", rule] &&
          identical(settings_read(app$SETTINGS_FILE)$project_dir, p16) && grepl("Up to date", output$runstate$html))
    check("the run reports what it redid: the one station table with the new removal, its logger table reused",
          identical(app$run_redone(p16), " Redone: 1 of 1 station tables; the rest was unchanged and reused."))
    check("after the run the grid shows the values as removed by hand", identical(wb$grid()$states$air_temp$state[5:6], c("rem", "rem")) && wb$grid()$states$air_temp$rule[5] == "manual")

    g <- wb$grid(); session$setInputs(wb_grid_sel = sel16(g, 2, 20, 24))
    session$setInputs(wb_act_cal = 1)
    session$setInputs(wb_cal_params = "rh", wb_cal_applies = "99016", wb_cal_mult = "1", wb_cal_off = "-2", wb_cal_start = "2021-06-02 07:00:00", wb_cal_end = "2021-06-02 11:00:00",
                      wb_cal_output = "", wb_cal_notes = "drift", wb_do_cal = 1)
    cal <- rd16(PERIOD_FILE)[.N]
    check("Calibrate adds an offset row to the corrections over a period", cal$parameter == "rh" && cal$action == "offset" && cal$value == "-2" &&
          cal$target == "99016" && cal$from == "2021-06-02 07:00:00" && cal$output == "")
    session$setInputs(wb_grid_sel = sel16(g, 1, 30))
    session$setInputs(wb_act_clock = 1)
    session$setInputs(wb_ck_action = "shift", wb_ck_min = "60", wb_ck_start = "2021-06-02 17:00:00", wb_ck_end = "2021-06-02 18:00:00", wb_ck_reason = "clock test", wb_ck_author = "WB", wb_do_clock = 1)
    tc <- rd16("time_corrections.csv")
    check("Clock writes a time_corrections.csv row for the logger table behind the selected records",
          identical(names(tc), TIME_COLS) && nrow(tc) == 1 && tc$logger_id == "99016" && tc$table == "Table1" && tc$action == "shift" && tc$shift_minutes == "60" && tc$reason == "clock test")
    session$setInputs(wb_act_note = 1)
    session$setInputs(wb_nt_date = "2021-06-02 17:00", wb_nt_type = "site visit", wb_nt_note = "checked sensor", wb_nt_author = "WB", wb_do_note = 1)
    fl <- rd16("field_log.csv")
    check("Note writes a field_log.csv row", identical(names(fl), LOG_COLS) && nrow(fl) == 1 && fl$type == "site visit" && fl$station == "99016" && fl$stream == "" && fl$parameter == "air_temp")
    session$setInputs(wb_grid_sel = sel16(g, 1, 13))
    session$setInputs(wb_rule_station = 1)
    q <- rd16("qc_rules.csv")
    check("Rule for this station adds a copy of the rule that removed the value, for this station only",
          nrow(q[applies_to == "99016"]) == 1 && q[applies_to == "99016", pattern] == "^air_temp.*" && q[applies_to == "99016", rule_id] == "")

    h <- app$hist_log(p16); i <- which(sub(" \\(.*$", "", h$table) == PERIOD_FILE)[2]   # [1] is the calibration written after it
    session$setInputs(hi_tbl_rows_selected = i); session$setInputs(hi_restore = 1)
    check("the Standard History page, inside the Workbench, goes back to an earlier version of a table", !is.na(i) && nrow(rm16()) == 2)
  })
}
f17 <- file.path(root, "two_windows.csv"); fwrite(data.table(a = c("1", "2")), f17)
testServer(app$editServer, args = list(id = "t", path = reactive(f17), reload = reactive(0)), {
  session$flushReact()
  fwrite(data.table(a = c("1", "2", "3")), f17); session$elapse(2500)
  check("a table with no unsaved edits takes up a version saved by the other interface by itself", nrow(data()) == 3 && !dirty())
  session$setInputs(tbl_cell_edit = list(row = 1, col = 0, value = "mine"))
  fwrite(data.table(a = c("1", "2", "3", "4")), f17); save(quiet = TRUE)
  check("a save over a version saved elsewhere since asks first and writes nothing", nrow(fread(f17)) == 4 && dirty())
  session$setInputs(cf_theirs = 1)
  check("taking the saved version drops this window's edits", nrow(data()) == 4 && !dirty() && !"mine" %in% data()$a)
  fwrite(data.table(a = c("1", "2", "3", "4", "5")), f17)
  ok <- session$returned$mutate(function(d) rbind(d, data.frame(a = "z")))
  check("a correction added from either interface starts from the saved version", isTRUE(ok) && identical(fread(f17, colClasses = "character")$a, c("1", "2", "3", "4", "5", "z")))
})

group("6. Each cached step depends only on the code it runs")
local({ e <- globalenv(); h0 <- step_hashes(e)
  ps <- plot_station; assign("plot_station", function(...) NULL, envir = e); h1 <- step_hashes(e); assign("plot_station", ps, envir = e)
  rr <- rpt_stats; assign("rpt_stats", function(...) NULL, envir = e); h2 <- step_hashes(e); assign("rpt_stats", rr, envir = e)
  rh <- read_header; assign("read_header", function(path) NULL, envir = e); h3 <- step_hashes(e); assign("read_header", rh, envir = e)
  check("a changed plot redoes the station tables only, not the reading or the stitching", h1$station != h0$station && identical(h1[c("headers", "programs", "stitch", "pieces")], h0[c("headers", "programs", "stitch", "pieces")]))
  check("a changed report (core) redoes nothing", identical(h2, h0))
  check("the step hashes kept by the pipeline are the ones computed now (set after every function is defined)", identical(STEP_HASH, h0))
  check("no app function replaces a core function of the same name", !length(core_shadows(app_code_files(PIPELINE_DIR))))
  check("every run leaves a log in output/logs that says it finished", grepl("^Run finished", utils::tail(readLines(sort(list.files(file.path(p16, "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]), 1)))
  check("a failed run's log keeps the error and the calls that led to it, and the error still stops the run", { lp <- file.path(tempdir(), "logtest")
    r <- tryCatch(run_logged(list(project_dir = lp), function() { cat("step one\n"); warning("odd value"); (function() stop("boom"))() }), error = function(e) conditionMessage(e))
    l <- readLines(list.files(file.path(lp, "output", "logs"), full.names = TRUE)[1])
    identical(r, "boom") && any(l == "step one") && any(grepl("^WARNING: odd value", l)) && any(grepl("^ERROR \\[E-[0-9a-f]{6}\\]: boom$", l)) && any(grepl(error_ref("boom"), l, fixed = TRUE)) && any(grepl("calls \\(innermost last\\)", l)) && any(grepl("stop(\"boom\")", l, fixed = TRUE)) && !any(grepl("handleSimpleError|withCallingHandlers", l)) && grepl("^Run failed", utils::tail(l, 1)) })
  check("the troubleshooting zip holds versions, logs, problems, the set-up and its changes, with no raw data, no secrets and home shown as ~", {
    fwrite(data.table(user = "a", password = "hunter2", path = path.expand("~/x")), file.path(p16, "config", "zz_secret_test.csv"))
    zf <- tempfile(fileext = ".zip"); ref <- troubleshooting_zip(p16, zf, function() data.table(level = "warning", problem = path.expand("~/somewhere")))
    unlink(file.path(p16, "config", "zz_secret_test.csv")); ux <- tempfile(); utils::unzip(zf, exdir = ux); fl <- list.files(ux, recursive = TRUE)
    zs <- fread(file.path(ux, ref, "config", "zz_secret_test.csv")); sc <- fread(file.path(ux, ref, "setup_check_now.csv"))
    grepl("^OVID-CS-", ref) && all(paste0(ref, "/", c("README.txt", "versions.txt", "setup_check_now.csv")) %in% fl) && any(grepl("^[^/]+/logs/.*[.]log$", fl)) &&
      any(grepl("/audit/run_manifest.csv$", fl)) && !"password" %in% names(zs) && zs$path == "~/x" && sc$problem == "~/somewhere" && !any(grepl("[.]dat$|stations/", fl)) })
  check("a changed reader rereads the headers", h3$headers != h0$headers && identical(h3$station, h0$station))
  check("the step hashes are the same wherever the pipeline is sourced (objects outside the code files are never followed)", { gg <- group; assign("group", function(...) 1, envir = e); h4 <- step_hashes(e); assign("group", gg, envir = e); identical(h4, h0) })
})
local({                                                               # between stations (core neighbours.R)
  d <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"); base <- -15 * cos(2 * pi * as.numeric(d - d[1]) / 365)
  mk <- function(off, extra = 0) data.table(Date = d, parameter = "air_temp", value = base + off + extra + stats::rnorm(length(d), 0, 0.3))
  set.seed(4); sea <- 3 * sin(2 * pi * as.numeric(d - d[1]) / 365)            # a site whose difference from the others drifts with the season
  bump <- ifelse(d >= as.Date("2022-07-10") & d <= as.Date("2022-07-14"), 9, 0)
  daily <- list(`A__T` = mk(0), `B__T` = mk(2), `C__T` = mk(-1, sea), `D__T` = mk(1, bump))
  FLAGS$list <- list(); neighbour_checks(daily, CFG); f <- rbindlist(FLAGS$list); FLAGS$list <- list()
  check("a station departing from the others for a few days is flagged for those days; a steady or seasonal difference is not",
    nrow(f) >= 1 && all(f$station == "D") && any(f$start == "2022-07-10 00:00:00" & f$end == "2022-07-14 23:59:59") && all(f$check == "differs_from_other_stations")) })
if (requireNamespace("callr", quietly = TRUE)) local({                  # a run from the app, in its own R process (core runner.R)
  source(file.path(CORE_DIR, "runner.R"), local = TRUE)
  bg <- function(proj, cancel_first = FALSE) { cf <- tempfile(); if (cancel_first) file.create(cf); of <- tempfile()
    cfg <- CFG; cfg$raw_dirs <- raw16; cfg$project_dir <- proj; cfg$make_plots <- FALSE; cfg$write_stitched <- FALSE
    p <- callr::r_bg(.run_child, list(app_dir = .app, pipeline = file.path(.app, "cs_pipeline.R"), no_run = "CS_NO_RUN", cfg = cfg, cancel_file = cf, ask = "ovid_ask_zone"),
                     stdout = of, stderr = "2>&1", package = FALSE, env = c(callr::rcmd_safe_env(), OVID_CORE = CORE_DIR))
    p$wait(120000); r <- p$get_result(); r$lines <- readLines(of, warn = FALSE)
    r$log <- readLines(sort(list.files(file.path(proj, "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]); r }
  a <- bg(file.path(root, "p16bg")); w <- run_where(a$lines)
  check("a run in the background finishes with the same station table as one in the app, and its progress shows each stage and item",
    { invisible(run(raw16, file.path(root, "p16fg"))); st <- function(p) fread(file.path(root, p, "output", "stations", "99016__Table1.csv"))
      a$status == "finished" && identical(st("p16bg"), st("p16fg")) } &&
      w$k == 6 && w$n == 6 && any(grepl("^@@ 1 1 ", a$lines)) && !any(grepl("^@@", a$log)))
  b <- bg(file.path(root, "p16cancel"), cancel_first = TRUE)
  check("Cancel stops the run between steps: it is reported as cancelled (not as a failure or a problem) and its log says so",
    b$status == "cancelled" && grepl("^Run cancelled by the user", utils::tail(b$log, 1)) && !any(grepl("^ERROR", b$log))) })
group("6b. Guardrails before and during a run (core)")
local({
  sf <- file.path(root, "g_settings.json"); settings_save(sf, list(project_dir = "a", ui = "workbench")); settings_save(sf, list(project_dir = "b"))
  s1 <- settings_read(sf); writeBin(as.raw(1:20), sf); s2 <- settings_read(sf)
  check("settings: a save changes only its own keys, and a damaged file falls back to the last good copy (and says so)",
        identical(s1$ui, "workbench") && s1$project_dir == "b" && s2$project_dir == "a" && isTRUE(attr(s2, "from_backup")))
  cd <- file.path(root, "g_cfg"); dir.create(file.path(cd, "history"), recursive = TRUE); writeBin(as.raw(c(97, 0, 98)), file.path(cd, "qc_rules.csv"))
  file.create(file.path(cd, "history", "qc_rules__20260101_000000.csv"))
  e <- tryCatch(read_cfg(file.path(cd, "qc_rules.csv")), error = function(e) e)
  check("a damaged set-up table stops with a message naming it and its last good copy", inherits(e, "ovid_bad_table") && grepl("qc_rules__20260101_000000.csv", conditionMessage(e)))
  x <- c(sprintf("09/%02d/23 03:00:00 PM", 1:20), "09/21/23 03:3")
  check("dates: a short file cut off in its last line keeps its year and its PM", format(parse_dt(x)[1]) == "2023-09-01 15:00:00" && is.na(parse_dt(x)[21]))
  check("a file with no checksum (unreadable) is not a copy of another", all(is.na(copy_of(c(NA, NA, "x", "x"), c("a", "b", "c", "d"))[1:2])) && copy_of(c(NA, NA, "x", "x"), c("a", "b", "c", "d"))[4] == "c")
  g <- function(cfg) tryCatch({ project_check(cfg); "ok" }, error = function(e) conditionMessage(e))
  r0 <- file.path(root, "g_raw"); dir.create(r0); pr <- file.path(root, "g_proj")
  busy <- file.path(root, "g_busy"); dir.create(busy); writeLines("x", file.path(busy, "notes.txt"))
  dir.create(file.path(root, "g_other")); writeLines("app: GT", file.path(root, "g_other", ".ovid_project"))
  check("folders: no raw folder, a project inside a raw folder or around one, a missing raw folder, another app's project and a folder with other files are each refused, with the reason (no project folder is not: the default one is used)",
        grepl("No raw data folder", g(list(project_dir = pr, raw_dirs = character()))) && grepl("inside the raw folder", g(list(project_dir = file.path(r0, "p"), raw_dirs = r0))) &&
        grepl("inside the project folder", g(list(project_dir = root, raw_dirs = r0))) && grepl("not found", g(list(project_dir = pr, raw_dirs = c(r0, file.path(root, "unplugged"))))) &&
        grepl("project of Ovid GT", g(list(project_dir = file.path(root, "g_other"), raw_dirs = r0))) && grepl("other files", g(list(project_dir = busy, raw_dirs = r0))) &&
        g(list(project_dir = pr, raw_dirs = r0)) == "ok")
  lk <- project_lock(list(project_dir = pr)); held <- tryCatch({ project_lock(list(project_dir = pr)); "taken" }, error = function(e) conditionMessage(e))
  writeLines(c("pid: 999999", paste0("host: ", Sys.info()[["nodename"]]), "started: then"), file.path(lk, "run.txt")); stale <- tryCatch({ project_lock(list(project_dir = pr)); "taken" }, error = function(e) "refused")
  unlink(lk, recursive = TRUE)
  check("one run at a time on a project; a lock left by a run that has gone is taken over", grepl("Another run is going", held) && stale == "taken" &&
        grepl("app: CS", readLines(file.path(pr, ".ovid_project"))[1]))
  # a cached file cut short is cleared and the run starts again
  invisible(run(raw16, file.path(root, "g_cache"))); df <- list.files(file.path(root, "g_cache", "cache", "stitch"), "data", full.names = TRUE)
  if (!length(df)) df <- list.files(file.path(root, "g_cache", "cache"), "\\.rds$", recursive = TRUE, full.names = TRUE)
  for (f in df) writeBin(as.raw(1:10), f)
  r2 <- tryCatch(run(raw16, file.path(root, "g_cache")), error = function(e) NULL)
  check("a damaged cache file does not stop the run: the cache is cleared and the run redone from the raw files", !is.null(r2) && file.exists(file.path(root, "g_cache", "output", "stations", "99016__Table1.csv")))
  # a raw file gone since the last run is named
  rg <- file.path(root, "g_raw2"); dir.create(rg); file.copy(list.files(raw16, full.names = TRUE), rg); file.copy(file.path(root, "raw6", "H.dat"), rg)
  invisible(run(rg, file.path(root, "g_gone"))); unlink(file.path(rg, "H.dat")); invisible(run(rg, file.path(root, "g_gone")))
  check("a raw file the last run used and that is gone is reported", any(grepl("are gone.*H.dat", fread(file.path(root, "g_gone", "output", "audit", "problems.csv"))$problem)))
})

local({                                                               # the shared deployment sheet (core deployments.R)
  pd <- file.path(root, "dep_cs"); dir.create(file.path(pd, "config"), recursive = TRUE)
  fwrite(data.table(serial = "99016", site = "Basin", station = "Ridge", latitude = "", longitude = "", from = "1990-01-01", to = "", notes = ""), file.path(pd, "config", "deployment_sheet.csv"))
  invisible(run(raw16, pd))
  check("a logger with no station in stations.csv takes the one a dated deployment sheet row gives it", any(startsWith(list.files(file.path(pd, "output", "stations")), "Ridge__")))
  st <- data.table(logger_id = c("1", "2"), station = c("", "Own"), start = "", end = "", notes = "")
  dr <- dep_clean(data.table(serial = c("1", "1", "2"), station = c("A", "B", "X"), from = c("2020-01-01", "2021-06-01", ""), to = c("2021-05-31", "", "")))$rows
  x <- cs_deploy_stations(st, dr)
  sg <- cs_deploy_stations(data.table(logger_id = c("3", "4"), station = "", start = "", end = "", notes = ""),
                           dep_clean(data.table(serial = c("3", "4"), station = c("Y", "Z"), from = c("", "2020"), to = ""))$rows)
  check("a deployment sheet row with no dates, or only a year, is a suggestion, not applied", all(sg$station == ""))
  check("a logger with two deployments gets a station row for each period; one stations.csv names keeps its name", nrow(x) == 3 && identical(x[logger_id == "1", station], c("A", "B")) &&
        x[logger_id == "1"][1, end] == "2021-05-31" && x[logger_id == "2", station] == "Own") })

group("7. Readers on real sample files")
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE); source(file.path(.app, "tests", "samples.R"), local = TRUE)
  r <- check_reader_samples(file.path(.app, "tests", "samples"), SAMPLE_READ); new <- r[is.na(matches_expected), file]
  if (length(new)) cat("  (expected results saved for", length(new), "new sample(s): check them in", file.path("tests", "samples", "expected"), ")\n")
  for (f in r[!(accepted), file]) cat("  ", f, ": not accepted. Check tests/samples/expected/", f, ".rds, then accept_sample(\"tests/samples\", \"", f, "\", \"<your name>\")\n", sep = "")
  for (f in r[reference_ok %in% FALSE, file]) cat("  ", f, ": a value in tests/samples/reference.csv differs from what the reader makes\n", sep = "")
  check("every sample file reads, with rows (or a header for a format read only that far), the same twice and the same as its expected result, accepted, and equal to its hand-checked reference values",
        nrow(r) > 0 && all(r$rerun_identical) && !any(r$matches_expected %in% FALSE) && all(r$rows > 0 | r$kind %in% SAMPLE_NO_ROWS) && all(r$accepted) && !any(r$reference_ok %in% FALSE)) 
  r2 <- check_reader_samples(file.path(.app, "tests", "samples"), function(p, k) { x <- SAMPLE_READ(p, k); x$data <- x$data[-1]; x }, update = FALSE)
  check("a reader that changes what it makes is caught against the expected results", all(r2$matches_expected %in% FALSE)) })
local({                                                               # damaged input (core testkit.R): runs that must not stop
  source(file.path(CORE_DIR, "testkit.R"), local = TRUE); sd <- file.path(.app, "tests", "samples")
  if (!length(list.files(sd, "__"))) { cat("  (no sample files in tests/samples: damaged-input runs skipped)\n"); return(invisible()) }
  rn <- function(raw, p) invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = p, make_plots = FALSE, write_stitched = FALSE)))))
  # with one sample the damaged file would be the only one, so a healthy copy sits beside it, as a second sample would
  rn1 <- function(raw, p) { s <- list.files(sd, "__", full.names = TRUE); s <- s[!dir.exists(s)]
    if (length(s) == 1L) file.copy(s, file.path(raw, paste0("healthy_", sub("^.*?__", "", basename(s))))); rn(raw, p) }
  d <- damaged_file_runs(sd, rn1); bad <- d[result != "ok"]; if (nrow(bad)) print(bad)
  check(paste0("a damaged raw file (", paste(unique(d$damage), collapse = ", "), ") never stops a run: ", nrow(d), " runs"), !nrow(bad))
  raw0 <- tempfile("raw0"); dir.create(raw0); for (f in list.files(sd, "__", full.names = TRUE)) if (!dir.exists(f)) file.copy(f, file.path(raw0, sub("^.*?__", "", basename(f))))
  p0 <- tempfile("p0"); rn(raw0, p0); t <- damaged_table_runs(p0, function() rn(raw0, p0)); bad <- t[result != "ok" & !startsWith(result, "The set-up table")]; if (nrow(bad)) print(bad)
  check(paste0("a damaged or emptied set-up table never stops a run without saying which table: ", nrow(t), " runs"), !nrow(bad)) })
group("7b. Station names and text in any language or encoding")
local({ nm <- c(dene = "\u0141\u0131\u0301\u0131\u0301ta\u0131\u0328\u0131\u0328 X\u0173\u0301\u0119\u0301", order = "a\u0328\u0301 a\u0301\u0328 o\u0328\u0300",
                syllabics = "\u14c0\u1426\u1403\u152d\u140d\u140f\u1423", cjk = "\u6c14\u8c61\u7ad9", emoji = "\u2744\ufe0f \U0001F321", units = "W/m\u00b2 \u00b0C \u00b5mol")
  wr <- function(f, st, val, enc = "UTF-8", nl = "\r\n") { l <- c(sprintf('"TOA5","%s","CR1000X","9","os","CPU:p.CR1X","1","T"', st), '"TIMESTAMP","RECORD","AirT","Name"',
      sprintf('"TS","RN","%s",""', nm[["units"]]), '"","","Avg","Smp"', sprintf('"2025-07-0%d 00:00:00",%d,1.5,"%s"', 1:3, 0:2, val))
    writeBin(unlist(lapply(paste0(l, nl), function(x) charToRaw(iconv(x, "UTF-8", enc, sub = "?")))), f) }
  d <- file.path(root, "enc"); dir.create(d); good <- function(x) all(validUTF8(x)) && !inherits(try(gsub("x", "x", x), silent = TRUE), "try-error")
  for (k in names(nm)) { f <- file.path(d, paste0(k, ".dat")); wr(f, nm[[k]], nm[[k]], nl = if (k == "cjk") "\n" else "\r\n")
    h <- read_header(f); r <- read_toa5(f, h$columns[[1]], 0, h$sep)
    check(paste0("a TOA5 file with ", k, " text reads unchanged (header, rows, first time)"), h$status == "ok" && identical(h$station_name, nm[[k]]) &&
          nrow(r) == 3 && identical(unique(r$Name), nm[[k]]) && good(r$Name) && !is.na(peek_first_ts(f))) }
  f <- file.path(d, "latin1.dat"); wr(f, "Caf\u00e9", "cr\u00e8me", enc = "latin1"); h <- read_header(f); r <- read_toa5(f, h$columns[[1]], 0, h$sep)
  check("a Latin-1 file still reads (converted to UTF-8)", identical(h$station_name, "Caf\u00e9") && identical(unique(r$Name), "cr\u00e8me") && good(r$Name)) })
group("8. Every page in a browser")
if (!identical(Sys.getenv("OVID_SKIP_UI"), "1") && requireNamespace("chromote", quietly = TRUE) && !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) local({
  source(file.path(CORE_DIR, "uitest.R"), local = TRUE)
  ui <- tryCatch(ui_click_through(normalizePath(.app), c(CS_PROJECT_DIR = p16)), error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check("every page opens in a browser with no output, JavaScript or server error (the test project)", nrow(ui) > 5 && !nrow(bad)) })
cat(sprintf("\n%d passed, %d failed\n", RES$pass, length(RES$fail))); if (length(RES$fail)) { cat("Failed:\n", paste0("  ", RES$fail, collapse = "\n"), "\n", sep = ""); quit(status = 1) }
