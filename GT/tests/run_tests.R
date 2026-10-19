# tests/run_tests.R  --  Checks for the parts of Ovid Core // GT that are easy to get wrong.
#   Rscript tests/run_tests.R                  (from the ovid_GT folder)
# Builds small synthetic logger files of every format in a temporary folder, runs the whole pipeline on them and checks
# the results. Exit status 1 if anything fails.

invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "English_United States.utf8" else "C.UTF-8"))
Sys.setenv(GT_NO_RUN = "1", OVID_RUN_SYNC = "1")   # app runs happen in this process (core runner.R), so a test sees their result at once
here <- if (file.exists("gt_pipeline.R")) "." else ".."
source(file.path(here, "gt_pipeline.R"))
audit_reset()
say <- function(...) invisible(NULL)

n_ok <- 0L; n_bad <- 0L
check <- function(what, cond) { if (isTRUE(cond)) { n_ok <<- n_ok + 1L; cat("  ok   ", what, "\n") } else { n_bad <<- n_bad + 1L; cat("  FAIL ", what, "\n") } }
near <- function(a, b, tol = 1e-6) isTRUE(all(abs(a - b) < tol))

root <- if (nzchar(Sys.getenv("GT_TEST_DIR"))) Sys.getenv("GT_TEST_DIR") else file.path(tempdir(), paste0("gt_test_", as.integer(Sys.time())))
unlink(root, recursive = TRUE); data <- file.path(root, "data"); meta <- file.path(root, "metadata"); proj <- file.path(root, "project")
for (d in c(file.path(data, c("rbr", "cryogeek", "hobo", "em50", "campbell")), meta)) dir.create(d, recursive = TRUE)

cat("time and names\n")
check("GMT-07:00 is -7", parse_offset("Date Time, GMT-07:00") == -7)
check("no zone is NA", is.na(parse_offset("Date Time")))
check("UTC 05:00 is 22:00 the day before at UTC-7", to_base(as.POSIXct("2021-04-16 05:00:00", tz = "UTC"), 0) == as.POSIXct("2021-04-15 22:00:00", tz = "UTC"))
tzg <- data.table(serial = "", model = "U12", file_pattern = "", start = "", end = "", offset_hours = "-6", reason = "set to MDT")
check("GT: a zone table row wins over the zone the file states", zone_for("1", "a.csv", 0, tzg, -7, model = "U12-008")$offset == -6)
check("GT: a file the table does not match keeps its stated zone", zone_for("1", "a.csv", 0, tzg, -7, model = "U23")$why == "stated in the file")
check("RBR 000123 and registry 123 are one serial", norm_serial("000123") == norm_serial("123"))
check("GeoPrecision ids keep their letters", norm_serial("e00004") == "E00004")
check("sensor numbers are positions, not ranks", identical(sensor_rank(c("P1", "P2", "P4")), c(1L, 2L, 4L)))
check("Campbell T109_C_Avg is sensor 1, T109_C_3_Avg sensor 3", identical(sensor_rank(c("T109_C_Avg", "T109_C_3_Avg")), c(1L, 3L)))

# ---- synthetic files ------------------------------------------------------------------------------------------------
t0 <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC"); n <- 24 * 400                     # 400 days, hourly
tt <- t0 + (seq_len(n) - 1) * 3600
sea <- function(amp, z) amp * exp(-z) * sin(2 * pi * as.numeric(tt - t0) / (365 * 86400)) - 0.3

# RBR .hex with 3 thermistor channels (the .txt export carries one header column too many and is in UTC)
cf <- c(0.004322952895280, -0.000277855717110, 0.000003637640430, -0.000000128483870)
temp_to_count <- function(T) { f <- function(r) 1 / (cf[1] + cf[2] * log((1 - r) / r) + cf[3] * log((1 - r) / r)^2 + cf[4] * log((1 - r) / r)^3) - 273.15 - T
  round(uniroot(f, c(0.001, 0.999), tol = 1e-13)$root * 2^24) }
rbr_T <- cbind(sea(8, 1), sea(8, 2), sea(8, 3))[seq(1, n, 4), ]; ns <- nrow(rbr_T)
cnt <- matrix(vapply(rbr_T, temp_to_count, 0), ncol = 3)
cnt[100, 2] <- 0                                                                       # an open thermistor: raw count 0
hexw <- toupper(sprintf("%06x", as.integer(t(cnt))))
body <- paste0(paste(rep("00", 48), collapse = ""), "00000054494D", "220101000000", paste(hexw, collapse = ""))
lines <- substring(body, seq(1, nchar(body), 48), pmin(seq(48, nchar(body) + 47, 48), nchar(body)))
hdr <- c("RBR XR-420 6.75 012345   (Ruskin version number - test)", "Host time     23/02/05 12:10:00", "Logger time   23/02/05 12:00:00",
         "Logging start 22/01/01 00:00:00", "Logging end   23/02/05 00:00:00", "Sample period          04:00:00",
         sprintf("Number of channels =  3, number of samples =   %d, mode: Stopped", ns), "E08%9.4f",
         unlist(lapply(1:3, function(k) c(sprintf("Calibration  %d: %.15f", k, cf[1]), sprintf("                %.15f", cf[2]), sprintf("                %.15f", cf[3]), sprintf("                %.15f Degrees_C", cf[4])))),
         "Memory type: 6 test", "Timestamp 2022/01/01 00:00:00 at sample 1 of type: TIME STAMP", "Number of bytes in header 48", "", "  Temp Temp Temp",
         sprintf("Number of bytes of data %d", nchar(body) / 2 - 48), "|     |     |")
writeLines(c(hdr, lines), file.path(data, "rbr", "012345_20230205_1210.hex"))
odv <- c("//<Creator>RBR</Creator>", "//<CreateTime>2023-02-05T19:12:00.000</CreateTime>", "//<Software>Ruskin</Software>", "// Model=XR-420", "// Serial=012345",
         paste(c("Cruise", "Station", "Type", "yyyy-mm-ddThh:mm:ss.sss", "Longitude [degrees_east]", "Latitude [degrees_north]", "Bot. Depth [m]", "Depth [m]", rep("Temp [Degrees_C]", 3), ""), collapse = "\t"),
         sprintf("C1\tS1\t*\t%s\t0.0\t0.0\t0\t%.7f\t%.7f\t%.7f\t", format(tt[seq(1, n, 4)] + 7 * 3600, "%Y-%m-%dT%H:%M:%OS3"), rbr_T[, 1], rbr_T[, 2], rbr_T[, 3]))
writeLines(odv, file.path(data, "rbr", "012345_20230205_1210_SiteName.txt"))

# GeoPrecision FG2 export: tags between rows, a resync step backwards, a firmware reset to 1998 and a dead node
g <- data.table(t = tt[seq(1, 2000, 2)]); g[, `:=`(a = round(sea(6, 0.5)[seq(1, 2000, 2)], 3), b = round(sea(6, 1)[seq(1, 2000, 2)], 3))]
fl <- c("<FG2 'FG2-SHELL V3.93a' (Jul  3 2017) NOW(UTC):01.04.2022 07:00:00>", "<LOGGER: $E5AB01>", "<MEMORY CLEAR>", "<PERIOD: 7200 sec>", "NO,TIME,#1(oC),#2(oC),HK-BAT(V)")
rows <- sprintf("%d,%s,%s,%s", seq_len(nrow(g)), format(g$t, "%d.%m.%Y %H:%M:%S"), g$a, ifelse(seq_len(nrow(g)) %in% 300:310, "(NoReply)", as.character(g$b)))
rows[1] <- paste0(rows[1], ",3.4")
fl <- c(fl, rows[1:500], "<NEW PAR>", "<PERIOD: 7200 sec>", "NO,TIME,#1(oC),#2(oC),HK-BAT(V)",
        sprintf("%d,%s,1.000,1.000", 501:503, format(g$t[498:500], "%d.%m.%Y %H:%M:%S")),                     # clock set back: rows 498-500 again
        "<FIRMWARE RESET>", "504,01.01.1998 00:00:30,(NoReply),(NoReply),3.3", rows[501:1000], "<INFO: Decode OK, 1004 Lines>")
writeLines(fl, file.path(data, "cryogeek", "E5AB01_20220401000000.csv"))
file.create(file.path(data, "cryogeek", "E5AB02_20220401000000.f2b")); writeBin(as.raw(c(0xfe, 1, 2, 3)), file.path(data, "cryogeek", "E5AB02_20220401000000.f2b"))

# GeoPrecision .f2b with no export, written byte by byte as the logger does: a block marker every 3 records, the time and battery
# on the first record of each block, escaped fe/ff bytes inside values and a dead node
f2b_esc <- function(x) unlist(lapply(as.integer(x), function(b) if (b == 0xfe) c(0xfe, 0) else if (b == 0xff) c(0xfe, 1) else b))
f2b_u <- function(x, k) as.integer(rev(sapply(0:(k - 1), function(j) (x %/% 256^j) %% 256)))
f2b_t <- tt[seq(1, 600, 2)]; f2b_p <- as.numeric(difftime(f2b_t[2], f2b_t[1], units = "secs")); f2b_v <- cbind(round(sea(4, 0.3)[seq(1, 600, 2)], 3), round(sea(4, 0.8)[seq(1, 600, 2)], 3))
f2b_v[5, 1] <- 1.984375; f2b_v[6, 2] <- 1.9921875; f2b_v[40:42, 2] <- -1e37          # bytes 3ffe..., 3fff... and NoReply
f2b_bat <- ifelse(seq_along(f2b_t) %% 3 == 1, (3300 + seq_along(f2b_t)) / 1000, NA)
fb <- c(0xfe, 0x05)
for (k in seq_along(f2b_t)) {
  if (k > 1 && k %% 3 == 1) fb <- c(fb, 0xfe, 0x02)
  for (j in 1:2) fb <- c(fb, j - 1L, f2b_esc(writeBin(f2b_v[k, j], raw(), size = 4, endian = "big")))
  if (k == 1) fb <- c(fb, 0xfa, f2b_esc(f2b_u(f2b_p / 2 + 32768, 2)))   # the period, in 2 s steps
  if (k %% 3 == 1) fb <- c(fb, 0xfb, f2b_esc(f2b_u(round(f2b_bat[k] * 1000) + 32768, 2)), 0xbf, f2b_esc(f2b_u(as.numeric(f2b_t[k] - as.POSIXct("1998-01-01", tz = "UTC"), units = "secs") / 2, 4)))
}
writeBin(as.raw(fb), file.path(data, "cryogeek", "E5AB03_20220401000000.f2b"))
writeLines(c("/*! a web page saved under a logger's name */", "var x = 1;"), file.path(data, "cryogeek", "E5AB03_20220401000000_1.f2b"))

# HOBOware CSV (stated zone GMT-06:00, one hour ahead of UTC-7) with a dying battery at the end
hb <- tt[1:1000]
hcsv <- c("\"Plot Title: TestPit\"", "\"#\",\"Date Time, GMT-06:00\",\"Temp, \u00b0C (LGR S/N: 1234567, SEN S/N: 1234567, LBL: T1)\",\"Temp, \u00b0C (LGR S/N: 1234567, SEN S/N: 1234567, LBL: T2)\",\"Bad Battery (LGR S/N: 1234567)\"",
          sprintf("%d,%s,%.3f,%.3f,%s", 1:1000, format(hb, "%m/%d/%y %I:%M:%S %p"), c(sea(10, 0.05)[1:990], rep(-70.5, 10)), sea(10, 0.4)[1:1000], ifelse(1:1000 == 991, "Logged", "")))
writeLines(hcsv, file.path(data, "hobo", "1234567_TestPit.csv"))

# Campbell TOA5 with two 109 thermistors and a NAN
toa <- c("\"TOA5\",\"TestStation\",\"CR1000\",\"777\",\"CR1000.Std\",\"prog.CR1\",\"1\",\"Table1\"", "\"TIMESTAMP\",\"RECORD\",\"T109_C_Avg\",\"T109_C_2_Avg\",\"BattV_Min\"",
         "\"TS\",\"RN\",\"Deg C\",\"Deg C\",\"Volts\"", "\"\",\"\",\"Avg\",\"Avg\",\"Min\"",
         sprintf("\"%s\",%d,%s,%.3f,12.5", format(tt[1:500], "%Y-%m-%d %H:%M:%S"), 0:499, ifelse(1:500 == 50, "\"NAN\"", sprintf("%.3f", sea(5, 0.1)[1:500])), sea(5, 0.2)[1:500]))
writeLines(toa, file.path(data, "campbell", "TestStation_Table1.dat"))
writeLines(toa, file.path(data, "campbell", "TestStation_Table1 copy.dat"))         # a byte-identical copy

# registry: the HOBO pit at a site with landcover, depths for the RBR
reg <- data.table(Name = c("PIT1", "DEEP1"), Type = c("ShallowHOBO", "RBR"), LC = c("Bog", "Plateau"), SN = c("1234567", "12345"), Status = "Active",
                  all_depths_sep_by_comma = c("", "1, 2, 3"), Record_start = c("", ""), Record_end = c("", ""))
if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(reg, file.path(meta, "registry.xlsx"))

cat("readers\n")
r <- read_rbr_hex(file.path(data, "rbr", "012345_20230205_1210.hex"))
check("RBR .hex decodes to the temperatures it was made from", near(r$data[channel == "T1" & !is.na(value), value][1:50], rbr_T[1:50, 1], 1e-3))
check("RBR .hex: an open thermistor is an error, not a value", r$data[channel == "T2" & ord == 100, !is.na(err) & is.na(value)])
check("RBR .hex: clock drift read from the header (10 min)", r$meta$drift_s == 600)
x <- read_rbr_txt(file.path(data, "rbr", "012345_20230205_1210_SiteName.txt"))
check("Ruskin text: values read by position despite the extra header column", near(x$data[channel == "T1", value][1:20], rbr_T[1:20, 1], 1e-6))
check("Ruskin text is in UTC", x$meta$offset_hours == 0)
f <- read_fg2_csv(file.path(data, "cryogeek", "E5AB01_20220401000000.csv"))
check("FG2: (NoReply) is an error", f$data[!is.na(err), .N] >= 11)
check("FG2: tags become events", any(grepl("NEW PAR", f$events$event)) && any(grepl("FIRMWARE RESET", f$events$event)))
check("FG2: battery is its own variable", any(f$data$variable == "battery"))
x <- read_f2b(file.path(data, "cryogeek", "E5AB03_20220401000000.f2b"))
xw <- dcast(x$data[variable == "temp"], ord + TIMESTAMP ~ channel, value.var = "value")
check(".f2b: every record decoded with its time", nrow(xw) == length(f2b_t) && all(xw$TIMESTAMP == f2b_t))
check(".f2b: values decoded, including escaped bytes", near(xw$T1, f2b_v[, 1], 1e-6) && near(xw$T2[-(40:42)], f2b_v[-(40:42), 2], 1e-6))
check(".f2b: NoReply is an error, not a value", x$data[channel == "T2" & ord %in% 40:42, all(is.na(value) & !is.na(err))])
check(".f2b: battery read where the logger wrote it", near(x$data[variable == "battery", value], f2b_bat[!is.na(f2b_bat)], 1e-6) && x$meta$interval_s == f2b_p)
check(".f2b: a file that is not an .f2b is not taken for one", sniff_format(file.path(data, "cryogeek", "E5AB03_20220401000000_1.f2b")) == "unrecognised_format" &&
      sniff_format(file.path(data, "cryogeek", "E5AB03_20220401000000.f2b")) == "fg2_f2b")
h <- read_hobo_csv(file.path(data, "hobo", "1234567_TestPit.csv"))
check("HOBO: zone from the header", h$meta$offset_hours == -6 && h$meta$serial == "1234567")
check("HOBO: battery event read", any(h$events$event == "Bad Battery"))
cb <- read_campbell_toa5(file.path(data, "campbell", "TestStation_Table1.dat"))
check("Campbell: thermistor columns only", setequal(unique(cb$data$channel), c("T109_C_Avg", "T109_C_2_Avg")))
check("Campbell: NAN is an error", cb$data[channel == "T109_C_Avg" & ord == 50, !is.na(err)])

cat("pipeline\n")
cfg <- modifyList(CFG, list(raw_dirs = data, metadata_dir = meta, project_dir = proj))
res <- tryCatch(run_pipeline(cfg), error = function(e) { cat("  pipeline stopped:", conditionMessage(e), "\n"); NULL })
check("pipeline ran", !is.null(res))
if (!is.null(res)) {
  a <- file.path(proj, "output", "audit"); rec <- fread(file.path(a, "reconciliation.csv")); ix <- fread(file.path(a, "audit_index.csv"))
  b <- fread(file.path(a, "bundles.csv")); fl <- fread(file.path(a, "files.csv")); ss <- fread(file.path(a, "sites_summary.csv"))
  check("every logger sensor reconciles (read = kept + removed)", all(rec$reconciles))
  fg <- fread(file.path(a, "flags.csv")); qr <- read_cfg(file.path(proj, "config", "qc_rules.csv"))
  check("quality rules carry lasting rule ids; flags.csv ends with rule_id, qc_code, app and run_id", all(grepl("^Q-[0-9]+$", qr$rule_id)) && !anyDuplicated(qr$rule_id) &&
        identical(tail(names(fg), 4), c("rule_id", "qc_code", "app", "run_id")) && (!nrow(fg) || all(fg$app == "GT")) &&
        (!nrow(fg[check == "flat_line"]) || all(fg[check == "flat_line", rule_id] %in% qr$rule_id)))
  fa <- fread(file.path(a, "file_audit.csv")); bu <- fread(file.path(a, "bundles.csv"))
  check("every raw file is in the file audit with its md5", nrow(fa) == nrow(fread(file.path(a, "files.csv"))) && all(nchar(fa[used == TRUE, md5]) == 32))
  check("each file read: values read = in a site record + dropped", any(fa$used) && all(fa[used == TRUE, read == in_output + dropped]) && sum(fa$read, na.rm = TRUE) == sum(rec$values_read))
  check("each download adds up too", all(bu[status == "ok", reconciles]))
  check("RBR bundle reads the .hex and checks the .txt against it", b[type == "rbr", source_file] == "012345_20230205_1210.hex" && fread(file.path(a, "export_checks.csv"))$result[1] == "agree")
  check("an .f2b that cannot be decoded is listed as needing an export", b[serial == "E5AB02", status] == "needs_export")
  check("an .f2b with no export is read directly", b[serial == "E5AB03", status] == "ok" && b[serial == "E5AB03", source_file] == "E5AB03_20220401000000.f2b" &&
        fa[file %like% "E5AB03_20220401000000.f2b$", read] > 0)
  check("the byte-identical Campbell copy is read once", fl[file == "TestStation_Table1 copy.dat", status] == "duplicate_file" || fl[file == "TestStation_Table1.dat", status] == "duplicate_file")
  check("the 1998 reset reading is removed as an impossible time", ix[rule == "impossible_time", sum(n)] >= 1)
  check("the readings superseded when the clock was set back are removed", ix[rule == "clock_resync", sum(n)] >= 1)
  check("the dying HOBO's -70 readings are removed by the range limit", ix[rule == "range_min", sum(n)] >= 10)
  check("sensor errors are audited with the logger's reason", ix[rule == "sensor_error", sum(n)] >= 12)
  sens <- fread(file.path(proj, "config", "sensors.csv"), colClasses = "character")
  check("sensors with no depth get the default steps", sens[serial == "E5AB01" & sensor == "T2", depth_m] == "0.2" && sens[serial == "E5AB01" & sensor == "T2", depth_source] == "default")
  h1 <- load_site(ss[grepl("1234567", loggers), site], proj)
  check("HOBO times moved from UTC-6 to UTC-7", !is.null(h1) && min(h1$TIMESTAMP) == min(hb) - 3600)
  if (requireNamespace("writexl", quietly = TRUE)) {
    check("registry depths used for the RBR (1, 2, 3 m)", setequal(sens[serial == "12345", depth_m], c("1", "2", "3")))
    check("registry site and landcover used", "PIT1" %in% ss$site && fread(file.path(proj, "config", "sites.csv"))[site == "PIT1", landcover] == "Bog")
  }
  dd <- load_daily(proj); check("daily means made", nrow(dd) > 100)
  an <- fread(file.path(proj, "output", "summaries", "annual_stats.csv")); check("annual statistics made", nrow(an) > 0 && all(c("mean", "min", "max") %in% names(an)))
  # a manual removal is applied and audited (an older version's manual_removals.csv, moved into the corrections over a period)
  fwrite(data.table(site = ss$site[1], serial = "", sensor = "", depth_m = "", variable = "", start = "2022-01-05", end = "2022-01-06", min_value = "", max_value = "", action = "remove", reason = "test", author = "t"),
         file.path(proj, "config", "manual_removals.csv"))
  res2 <- tryCatch(run_pipeline(cfg), error = function(e) { cat("  second run stopped:", conditionMessage(e), "\n"); NULL })
  ix2 <- fread(file.path(a, "audit_index.csv"))
  check("a manual removal is applied and audited on the next run", !is.null(res2) && ix2[rule == "manual", sum(n)] > 0)
  pc2 <- read_cfg(cfg_path(proj, "period"))
  check("an older version's manual_removals.csv is moved once into the corrections over a period, and left in place",
        nrow(pc2[action == "remove"]) == 1 && pc2[action == "remove", target] == ss$site[1] && file.exists(file.path(proj, "config", "manual_removals.csv")))
  check("a second run adds no duplicate sensor rows", nrow(fread(file.path(proj, "config", "sensors.csv"))) == nrow(sens))
  sf <- function() { f <- list.files(file.path(proj, "output", c("sites", "soil")), full.names = TRUE); setNames(unname(tools::md5sum(f)), basename(f)) }
  before <- sf(); res3 <- tryCatch(run_pipeline(cfg), error = function(e) NULL); mf <- fread(file.path(a, "run_manifest.csv"), colClasses = "character")
  check("an unchanged run reuses every logger and site", !is.null(res3) && mf[key == "loggers_reused", value] == mf[key == "loggers", value] && mf[key == "sites_reused", value] == mf[key == "sites", value])
  check("and leaves the site files as they were", identical(before, sf()))
  lids <- forget_gt_files(proj, file.path(data, "hobo", "1234567_TestPit.csv")); res4 <- tryCatch(run_pipeline(cfg), error = function(e) NULL)
  mf <- fread(file.path(a, "run_manifest.csv"), colClasses = "character")
  check("rescanning a file reads its logger again and gives the same site files", length(lids) == 1 && !is.null(res4) && as.integer(mf[key == "loggers_reused", value]) == as.integer(mf[key == "loggers", value]) - 1L && identical(before, sf()))
  # configuration round trip
  z <- file.path(root, "cfg.zip"); export_config(proj, cfg, z); imp <- read_config_import(z)
  check("configuration export and import round-trip", all(c("sensors", "sites", "qc", "settings") %in% names(imp)) && config_diff(read_cfg(cfg_path(proj, "sensors")), imp$sensors)$added == 0)
  ez <- file.path(root, "export.zip"); build_export(proj, ss$site, ez, res = "day", layout = "wide", out_offset = 0)
  check("export zip holds sites, summaries, metadata and audit", all(c("README.txt", "metadata/sites.csv", "audit/audit_index.csv") %in% zip::zip_list(ez)$filename))
  ez2 <- file.path(root, "export_nc.zip"); build_export(proj, ss$site[1], ez2, res = "day", reports = character(), netcdf = TRUE)
  check("the export can add a CF NetCDF file per site", any(grepl("^netcdf/.*_ground_temp\\.nc$", zip::zip_list(ez2)$filename)))
  check("the run writes output/report.html", file.exists(file.path(proj, "output", "report.html")))
  check("run and site reports are written in both formats", { f <- write_gt_reports(proj, file.path(tempdir(), "gt_rep"), sites = gt_read(proj, "audit", "sites_summary.csv")$site[1]); length(f) == 4 && all(file.exists(f)) })
}
check("WMO codes: pre-install is 3, flat line and air-like are 2, a checked clean value 0, none-checked 4, no value 7", {
  fl <- c("", "pre_install", "flat_line;air_like", "spike", ""); v <- c(1, 1, 1, 1, NA)
  identical(wmo_code(v, fl), c(0L, 3L, 2L, 2L, 7L)) && all(wmo_code(c(1, 2), c("", ""), FALSE) == 4L) })
check("WMO export: the site table has a qc column, wide and long, and a daily mean takes the worst code of its values", {
  tt <- as.POSIXct("2022-06-01", tz = "UTC") + 3600 * (0:47)
  d <- data.table(TIMESTAMP = rep(tt, 2), depth_m = rep(c(0.1, 0.5), each = 48), value = 5, flag = "", serial = "s", sensor = "x", depth_source = "manual")
  d[TIMESTAMP == tt[3] & depth_m == 0.1, flag := "flat_line"]; d[TIMESTAMP == tt[30] & depth_m == 0.5, flag := "pre_install"]
  r <- site_table(d); w <- site_table(d, layout = "wide"); y <- site_table(d, res = "day")
  all(c("qc", "flag") %in% names(r)) && sum(r$qc == 2L) == 1L && sum(r$qc == 3L) == 1L && all(c("T_0.1m_qc", "T_0.5m_qc") %in% names(w)) &&
    all(y$qc[y$depth_m == 0.1] == c(2L, 0L)) && all(y$qc[y$depth_m == 0.5] == c(0L, 3L)) && !"qc" %in% names(site_table(d, scheme = "ovid")) })
check("gap filling: a short deep gap is linear in time, a shallow one comes from its neighbours, measured days stay", {
  dts <- as.Date("2021-01-01") + 0:199
  dd <- rbindlist(lapply(c(0.2, 0.4, 1), function(z) data.table(site = "S", depth_m = z, date = dts, temp = 5 * sin(as.numeric(dts) / 30) * exp(-z) + z, tmin = 0, tmax = 0, n = 24L, expected = 24, complete = TRUE)))
  dd <- dd[!(depth_m == 1 & date %in% (as.Date("2021-03-01") + 0:2)) & !(depth_m == 0.4 & date %in% (as.Date("2021-05-01") + 0:9))]
  e <- fill_daily(dd, 5, 0.5, 0.95)
  a <- e[depth_m == 1]; b <- e[depth_m == 0.4]; tru <- function(z, d) 5 * sin(as.numeric(d) / 30) * exp(-z) + z
  nrow(a) == 3 && all(a$method == "linear in time") && nrow(b) == 10 && all(grepl("0.2 and 1", b$method)) && max(abs(b$temp - tru(0.4, b$date))) < 0.05 &&
    nrow(with_estimates(copy(dd), e)) == 600 && sum(with_estimates(copy(dd), e)$estimated) == 13 })
check("diffusivity: an ideal conducting wave gives the same diffusivity from amplitude and phase", {
  a0 <- 0.5e-6 * 86400; w <- 2 * pi / 365.25; dts <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"); tt <- as.numeric(dts - as.Date("2020-01-01"))
  zs <- c(0.5, 1, 2); dd <- rbindlist(lapply(zs, function(z) { k <- z * sqrt(w / (2 * a0)); data.table(site = "S", depth_m = z, date = dts, temp = -2 + 10 * exp(-k) * cos(w * tt - k), complete = TRUE) }))
  an <- annual_stats(dd[, `:=`(tmin = temp, tmax = temp, n = 24L, expected = 24)], 0.9)
  th <- thermal_props(dd, an); r <- th[year == 2020]
  nrow(r) == 2 && all(abs(r$alpha_amplitude_mm2_s - 0.5) < 0.02) && all(abs(r$alpha_phase_mm2_s - 0.5) < 0.02) && all(r$note == "") && r$deepest[2] && abs(r$gradient_c_per_m[1]) < 0.01 })
check("surface change: interpolated between surveys, 0 before the first, held after; the active layer from the surface of the day", {
  sc <- data.table(site = c("S", "S"), date = c("2020-08-01", "2022-08-01"), surface_change_m = c("0", "0.2"), method = "", notes = "")
  v <- surface_change_at(sc, "S", as.Date(c("2019-01-01", "2021-08-01", "2024-01-01")))
  alt <- data.table(site = "S", year = 2021L, alt_m = 0.8, date_alt = as.Date("2021-08-01"))
  abs(v[2] - 0.1) < 0.001 && v[1] == 0 && v[3] == 0.2 && alt_from_surface(alt, sc)$alt_from_surface_m == 0.7 && all(surface_change_at(NULL, "S", Sys.Date()) == 0) })
check("soil water in frozen ground is marked liquid only", {
  tt <- as.POSIXct("2021-01-01", tz = "UTC") + 86400 * 0:3
  so <- data.table(TIMESTAMP = rep(tt, 2), variable = rep(c("vwc", "ec"), each = 4), depth_m = 0.3, value = 0.3)
  te <- data.table(TIMESTAMP = tt, depth_m = 0.25, value = c(-1, -0.5, 0.5, 2))
  r <- soil_phase(copy(so), te); identical(r$liquid_only, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)) && !any(soil_phase(copy(so), te[0])$liquid_only) })
check("NetCDF: a site file is time x depth in UTC, with fills for removed values and its WMO codes as CF flags", {
  tt <- as.POSIXct("2022-06-01", tz = "UTC") + 3600 * (0:5)
  d <- data.table(TIMESTAMP = rep(tt, 2), depth_m = rep(c(0.1, 0.5), each = 6), value = c(1:6, 11:16) / 2, flag = "")
  d[3, flag := "flat_line"]; d[8, value := NA]
  f <- tempfile(fileext = ".nc"); site_netcdf(d, f, "Pit A", list(latitude = "60.1", longitude = "-120.1"), base = -7); x <- nc_read(f)
  v <- matrix(x$vars$soil_temperature$data, 6, 2, byrow = TRUE); q <- matrix(x$vars$soil_temperature_qc$data, 6, 2, byrow = TRUE)
  identical(unname(x$dims[c("time", "depth")]), c(6L, 2L)) && x$vars$time$data[1] == as.numeric(tt[1]) + 7 * 3600 &&
    is.na(v[2, 2]) && v[6, 2] == 8 && q[3, 1] == 2L && q[2, 2] == 7L && x$atts$featureType == "timeSeriesProfile" &&
    x$vars$soil_temperature_qc$atts$flag_meanings == "good inconsistent doubtful wrong not_checked changed estimated missing" &&
    x$vars$lat$data == 60.1 && x$vars$station_name$data == "Pit A" && grepl("^L1", x$atts$processing_level) })
check("a site that jumps away from the others is flagged on those days only", {
  dd <- CJ(id = c("A", "B", "C", "D"), date = as.Date("2022-01-01") + 0:199)
  dd[, value := 10 * sin(as.integer(date) / 30) + match(id, c("A", "B", "C", "D")) + rep(c(0, 0.05, -0.05, 0.02), 50)]
  dd[id == "C" & date >= as.Date("2022-03-01") & date <= as.Date("2022-03-03"), value := value - 12]
  b <- neighbour_days(dd, floor = 3); p <- neighbour_periods(b); s <- neighbour_summary(b)
  nrow(p) == 1 && p$id == "C" && p$days == 3 && s[id == "C", verdict] == "departs at times" && s[id == "A", verdict] == "ok" })
check("a report renders as HTML and as PDF", { d <- rpt_doc("Test", "sub", list(rpt_cards(c(a = 1)), rpt_heading("T"), rpt_table(data.table(year = 2023, x = 1234.5)), rpt_text("note")))
  f <- report_write(d, file.path(tempdir(), "rpt_test"), c("html", "pdf")); h <- paste(readLines(f[1], warn = FALSE), collapse = "")
  length(f) == 2 && grepl("<table", h) && grepl(">2023<", h) && identical(readBin(f[2], "raw", 4), charToRaw("%PDF")) })
cat("corrupt time stamps (shared core)\n")
tg <- seq(as.POSIXct("2020-10-01", tz = "UTC"), by = 3600, length.out = 2000); junk <- as.POSIXct("2020-10-05 00:00:17", tz = "UTC") + (0:9) * 86400 + 7
tt <- c(tg[1:1500], junk, tg[1501:2000]); sr <- stray_records(tt)
check("readings stamped at random times among good ones are found", all(sr[1501:1510]) && !any(sr[-(1501:1510)]))
check("a reading written in order but off the grid is kept", !any(stray_records(c(tg[1:100], tg[100] + 1337, tg[101:200]))))
check("the depth statistics and the availability chart are built from a site's readings", { set.seed(3); tt <- seq(as.POSIXct("2022-01-01", tz = "UTC"), by = "6 hours", length.out = 600)
  d <- rbindlist(lapply(c(0.1, 1.2), function(z) data.table(TIMESTAMP = tt, depth_m = z, value = rnorm(600), flag = c(rep("pre_install", 50), rep("", 500), rep("air_like", 50)))))
  an <- data.table(depth_m = c(0.1, 1.2), year_type = "calendar", complete = TRUE, mean = c(1, 2), range = c(10, 3)); ft <- data.table(depth_m = c(0.1, 1.2), days_frozen = c(5, 0), days_thawed = c(95, 100), days_zero_curtain = c(0, 0))
  st <- gt_depth_stats(d, an, ft); av <- gt_availability(d, MI)
  st$type == "table" && nrow(st$data) == 2 && all(c("mean annual", "days frozen") %in% names(st$data)) && length(av) >= 1 && is.null(kv_table(list(a = NULL, b = ""))) })
if (!is.null(res)) {
  cat("only what changed is redone\n")
  mf_of <- function(p) { m <- fread(file.path(p, "output", "audit", "run_manifest.csv"), colClasses = "character"); function(k) m[key == k, value] }
  outs_of <- function(p) { f <- list.files(file.path(p, "output", c("sites", "soil", "summaries")), full.names = TRUE)
    stats::setNames(lapply(f, function(x) if (grepl("\\.rds$", x)) readRDS(x) else fread(x, colClasses = "character", na.strings = NULL)), sub(".*/output/", "", f)) }
  s_hobo <- ss[grepl("1234567", loggers), site]
  m0 <- read_cfg(cfg_path(proj, "period"))
  fwrite(rbind(m0, data.table(target = s_hobo, parameter = "temp", from = "2022-01-10", to = "2022-01-11", action = "remove", note = "one site", author = "t"), fill = TRUE), cfg_path(proj, "period"))
  invisible(capture.output(r5 <- tryCatch(run_pipeline(cfg), error = function(e) NULL))); v5 <- mf_of(proj)
  full <- file.path(root, "full"); unlink(full, recursive = TRUE); dir.create(full); file.copy(file.path(proj, "config"), full, recursive = TRUE)
  invisible(capture.output(rf <- tryCatch(run_pipeline(modifyList(cfg, list(project_dir = full, incremental = FALSE))), error = function(e) NULL)))
  check("a removal on one site redoes the logger there only, and gives what a run from scratch gives",
        !is.null(r5) && !is.null(rf) && as.integer(v5("loggers_reused")) == as.integer(v5("loggers_checked")) - 1L &&
        as.integer(v5("sites_reused")) == as.integer(v5("sites_total")) - 1L && identical(outs_of(proj), outs_of(full)))
  q0 <- read_cfg(cfg_path(proj, "qc"))
  fwrite(rbind(q0, data.table(variable = "temp", applies_to = s_hobo, min = "-30", max = "30", max_spike = "", max_flat_hours = "", flat_exempt_band = "", action = "flag",
                              notes = "for one site", rule_id = ""), fill = TRUE), cfg_path(proj, "qc"))
  invisible(capture.output(r6 <- tryCatch(run_pipeline(cfg), error = function(e) NULL))); v6 <- mf_of(proj)
  check("a quality rule for one site redoes the logger there only", !is.null(r6) && as.integer(v6("loggers_reused")) == as.integer(v6("loggers_checked")) - 1L)
  tz <- data.table(serial = "999999", model = "", file_pattern = "", start = "", end = "", offset_hours = "-5", reason = "")
  check("the time-zone rows a logger's zone can depend on: its serial, model or path", !nrow(tz_rows_for(tz, "1234567", "hobo/a.csv")) && nrow(tz_rows_for(tz, "999999", "x.csv")) == 1 &&
        nrow(tz_rows_for(data.table(serial = "", model = "", file_pattern = "^hobo/", offset_hours = "-6"), c("1", "2"), c("rbr/x.hex", "hobo/y.csv"))) == 1)

  cat("degree days, n-factors, taliks, offsets, trends\n")
  dz <- seq(as.Date("2018-10-01"), as.Date("2022-09-30"), by = "day"); doy <- as.numeric(format(dz, "%j"))
  prof <- function(z, m, a) data.table(site = "P", depth_m = z, date = dz, temp = m + a * sin(2 * pi * (doy - 120) / 365.25), complete = TRUE)
  ddp <- rbind(prof(0.05, -0.5, 10), prof(0.5, 1, 0.6), prof(1.0, 0.5, 0.3), prof(2.0, -0.8, 0.2))
  airp <- data.table(site = "P", date = dz, temp = -3 + 18 * sin(2 * pi * (doy - 120) / 365.25), complete = TRUE, source = "test")
  anp <- annual_stats(ddp, 0.9, 10); tkp <- taliks(ground_state(anp))[year == 2020]
  check("a layer unfrozen all year between seasonal frost and permafrost is a supra-permafrost talik, its top and bottom where the year's minimum and maximum cross 0",
        tkp$result == "supra-permafrost talik" && tkp$talik_top_m > 0.05 && tkp$talik_top_m < 0.5 && tkp$talik_bottom_m > 1 && tkp$talik_bottom_m < 2 && tkp$permafrost_from_m == 2)
  dgp <- degree_days(ddp[depth_m == 0.05], 0.9, 10)[water_year == 2020]
  nfp <- n_factors(ddp, airp, 0.9, 10, 0.1)[water_year == 2020]
  check("degree days are the sums of the daily means below and above 0; n-factors are surface over air; the surface offset follows; no TTOP is given",
        isTRUE(all.equal(dgp$fdd, round(sum(-ddp[depth_m == 0.05 & date >= as.Date("2019-10-01") & date <= as.Date("2020-09-30") & temp < 0, temp]), 1))) &&
        isTRUE(all.equal(nfp$n_f, round(nfp$fdd_s / nfp$fdd_a, 3))) && nfp$surface_offset > 0 && !any(c("ttop_measured", "ttop_model", "thermal_offset", "rk") %in% names(nfp)))
  trp <- trends(anp, data.table(site = "P", year = 2015:2022, alt_m = 1 + 0.02 * (0:7)), 4)
  check("a steady deepening of the active layer is a trend; three years of MAGT are too few", trp[quantity == "ALT", result] == "increasing" && isTRUE(all.equal(trp[quantity == "ALT", slope_per_decade], 0.2)) &&
        all(trp[quantity == "MAGT", result] == "fewer than 4 years"))
  check("the run writes degree days, n-factors, ground state, taliks, zero-curtain offsets and trends",
        all(file.exists(file.path(proj, "output", "summaries", c("degree_days.csv", "n_factors.csv", "ground_state.csv", "taliks.csv", "zero_curtain_offsets.csv", "trends.csv")))))

  cat("the logger list (optional metadata)\n")
  sx <- read_cfg(cfg_path(proj, "sensors")); auto <- sx[grepl("not in the registry", notes) & nzchar(sensor)]
  if (!nrow(auto)) check("a logger named after itself to list", FALSE) else {
    sn <- auto$serial[1]; ty <- auto$logger_type[1]; nsens <- uniqueN(auto[serial == sn, sensor])
    fwrite(data.table(serial = sn, name = "Listed pit", site = "", depths = paste(c(0.02, 0.3, 0.6, 1.2, 2.5, 4, 6, 8)[seq_len(nsens)], collapse = ", "), owner = "lab"), cfg_path_meta(proj))
    invisible(capture.output(r7 <- tryCatch(run_pipeline(cfg), error = function(e) NULL))); sy <- read_cfg(cfg_path(proj, "sensors"))[serial == sn & nzchar(sensor)]
    check("a logger in the list with a name and a depth list is placed at that name with those depths, with no registry row",
          !is.null(r7) && nrow(sy) == nsens && all(sy$site == "Listed pit") && all(sy$depth_source == "logger list") && "0.02" %in% sy$depth_m)
    fwrite(data.table(serial = sn, name = "Listed pit", site = "Site 2", depths = "", owner = "lab"), cfg_path_meta(proj))
    invisible(capture.output(r8 <- tryCatch(run_pipeline(cfg), error = function(e) NULL))); sz <- read_cfg(cfg_path(proj, "sensors"))[serial == sn & nzchar(sensor)]
    check("changing the list moves the logger and its depths go back to the assumed ones; rows set by hand are not touched",
          !is.null(r8) && all(sz$site == "Site 2") && all(sz$depth_source %in% c("template", "default")) && identical(sx[serial != sn], read_cfg(cfg_path(proj, "sensors"))[serial != sn]))
  }

  cat("Workbench interface (preview)\n")
  app <- new.env(); assign("shinyApp", function(...) invisible(NULL), envir = app)
  local({ old <- setwd(here); on.exit(setwd(old)); suppressMessages(sys.source("app.R", envir = app, keep.source = FALSE)) })
  app$SETTINGS_FILE <- file.path(root, "app_settings.json"); app$VIEWER <- FALSE
  app$SETTINGS <- as.list(app$CFG)                     # the defaults, not the settings saved on this computer (a test never reads or writes them)
  check("both interfaces build their table editors from one list, and the Workbench opens each where the Standard edits it",
        all(app$SETUP_TABLES$cfg %in% names(CONFIG_FILES)) && !anyDuplicated(app$SETUP_TABLES$id) && setequal(names(app$WB_OPEN), app$SETUP_TREE$id) &&
        any(grepl("setup_editors(cfgp, tick, mark, skip = \"depsheet\")", deparse(app$server), fixed = TRUE)))
  app$SETTINGS$project_dir <- proj; app$SETTINGS$raw_dirs <- data; app$SETTINGS$metadata_dir <- meta
  hw <- as.character(htmltools::renderTags(app$wb_ui(list(QUERY_STRING = "ui=workbench")))$html)
  idw <- regmatches(hw, gregexpr(' id="[^"]+"', hw))[[1]]
  toolw <- vapply(Filter(function(x) inherits(x, "ovid_tool"), app$std_pages(FALSE)), `[[`, "", "value")
  check("the Workbench holds every page of the Standard interface, its data view, Import, Run (the Standard's) and Export, and no input twice",
        all(vapply(c(toolw, "wb-data", "wb-import"), function(v) grepl(paste0('data-value="', v, '"'), hw, fixed = TRUE), NA)) && !anyDuplicated(idw) &&
        grepl('id="run"', hw, fixed = TRUE) && grepl('id="wb_export"', hw, fixed = TRUE) && grepl('href="?ui=standard"', hw, fixed = TRUE) &&
        all(vapply(setdiff(app$SETUP_TABLES$id, "depsheet"), function(i) grepl(paste0('id="', i, '-tbl"'), hw, fixed = TRUE), NA)) && grepl('id="mlist-ed-tbl"', hw, fixed = TRUE) &&
        all(vapply(c("wb_act_remove", "wb_act_flag", "wb_act_restore", "wb_act_trim", "wb_act_clock", "raw_dirs", "metadata_dir", "project_dir", "wb_chart"), function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)))
  has_id <- function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE)
  check("removed controls are gone (one-logger depth form, profile cut, calibration tab, Settings switches, QC view, heatmap On/Off, summaries ZIP, per-site CSV, report checkboxes)",
        !any(vapply(c("sn_apply", "profile_cut_m", "calib-tbl", "correct_rbr_drift", "check_exports", "qc_plot_ui", "hm_sm_on", "sm_dl", "sm_soil", "nb_dl", "xp_rep", "rp_fmt", "Shown"), has_id, NA)) &&
        all(vapply(app$ADV_KEYS, has_id, NA)) && length(app$ADV_KEYS) == 12 && "assign_default_depths" %in% app$CORR_KEYS && "hide_accepted_flags" %in% app$CORR_KEYS &&
        all(vapply(c("permafrost_years", "hm_smooth", "nb_smooth", "rp_pick", "qc_flags", "qc_removed", "ren-", "st_apply"), function(i) grepl(i, hw, fixed = TRUE), NA)))
  og <- suppressWarnings(app$load_outputs(proj)); tg <- app$wb_tables(og, proj)
  s_rbr <- ss[grepl("12345", loggers) & !grepl("1234567", loggers), site][1]
  check("the tree lists every site", setequal(tg[kind == "temp", site], fread(file.path(proj, "output", "audit", "sites_summary.csv"))$site))
  wg <- app$wb_load(paste0("temp:", s_hobo), proj); hs <- load_site(s_hobo, proj)
  check("a site's record is a table with a column per depth, each value with its flag, logger and sensor beside it, and a row for every time a value was removed",
        identical(app$wb_params(wg), depth_lab(sort(unique(hs$depth_m)))) && nrow(wg) == uniqueN(c(as.numeric(hs$TIMESTAMP), as.numeric(load_removed(s_hobo, proj)[variable == "temp", TIMESTAMP]))) &&
        all(c(paste0("flag|", app$wb_params(wg)[1]), paste0("serial|", app$wb_params(wg)[1])) %in% names(wg)) && sum(!is.na(as.matrix(wg[, app$wb_params(wg), with = FALSE]))) == nrow(hs))
  rg <- app$wb_removed(s_hobo, "temp", proj); p1 <- app$wb_params(wg)[1]
  rmp <- function() gt_period_to_manual(period_own_rows(cfg_path(proj, "period"), c("remove", "flag")))
  sg <- app$wb_states(wg, app$wb_params(wg), rg, rmp(), s_hobo)
  t10 <- which(wg$TIMESTAMP >= as.POSIXct("2022-01-10", tz = "UTC") & wg$TIMESTAMP <= as.POSIXct("2022-01-11 23:59:59", tz = "UTC"))
  check("values removed by the range limit and by hand show as removed, with their old values", any(sg[[p1]]$state == "rem" & sg[[p1]]$old == -70.5) &&
        all(sg[[p1]]$state[t10] == "rem") && all(sg[[p1]]$rule[t10] == "manual"))
  sg2 <- app$wb_states(wg, app$wb_params(wg), rg, rmp()[0], s_hobo)
  check("with its row deleted a removal by hand is marked to come back", all(sg2[[p1]]$state[t10] == "back"))
  tt2 <- wg$TIMESTAMP[1:6]
  rr <- app$wb_removal_rows(wg$TIMESTAMP, tt2[c(2, 3, 2, 3, 5)], c(0.1, 0.1, 0.2, 0.2, 0.1), "temp", s_hobo, " why ", "me")
  check("removal rows: the site, the depth and the variable, one per run of records, depths that share a run in one row",
        identical(names(rr), MANUAL_COLS) && identical(rr$depth_m, c("0.1|0.2", "0.1")) && identical(rr$start, fmt_ts(tt2[c(2, 5)])) && all(rr$site == s_hobo) && all(rr$action == "remove") && all(rr$reason == "why"))
  rr2 <- app$wb_removal_rows(wg$TIMESTAMP, tt2[c(2, 3)], c(0.1, -0.3), "temp", s_hobo, "why", "me", sgn = -1)
  check("removal rows written in a project with depths positive up read back as the same depths",
        identical(sort(typed_depth(unlist(strsplit(rr2$depth_m, "|", fixed = TRUE)), -1)), c(-0.3, 0.1)))
  check("a run saves every editor: the set-up tables and the logger list", grepl("unname(E)", paste(deparse(body(app$server)), collapse = " "), fixed = TRUE))
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
    dvg <- ui_defaults(hw); rdg <- function(w) fread(cfg_path(proj, w), colClasses = "character", na.strings = NULL)
    selg <- function(g, c, r1, r2 = r1) list(sig = g$sig, c = as.list(c), r1 = as.list(r1), r2 = as.list(r2), ar = r1[1], ac = c[1])
    both <- function(input, output, session) { std <- app$server(input, output, session, embedded = TRUE); wb <- app$wb_server(input, output, session, std) }
    testServer(both, {
      suppressWarnings({ do.call(session$setInputs, dvg); session$flushReact() })
      session$setInputs(wb_table = paste0("temp:", s_hobo))
      ps <- app$wb_params(wg)
      session$setInputs(wb_params = ps, wb_period = as.Date(range(wg$TIMESTAMP)))
      g <- wb$grid(); r10 <- which(g$w$TIMESTAMP == wg$TIMESTAMP[t10[1]])
      check("the Workbench runs on the Standard server: its editors and outputs, the site's depths charted over the grid",
            !session$isClosed() && identical(wb$E, std$E) && g$n == nrow(wg) && identical(g$params, ps) && g$states[[p1]]$state[r10] == "rem")
      session$setInputs(wb_grid_sel = selg(g, 1, r10))
      ins <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(output$wb_inspector$html)))
      check("a selected value shows its logger, its depth and where the depth came from, and what removed it",
            grepl("Read by HOBO 1234567", ins, fixed = TRUE) && grepl(paste("At", p1), ins, fixed = TRUE) && grepl("Removed by manual", ins, fixed = TRUE))
      session$setInputs(wb_grid_sel = selg(g, c(1, 2), c(20, 20), c(22, 21)))
      session$setInputs(wb_act_remove = 1); session$setInputs(wb_rm_reason = "bad", wb_rm_author = "WB", wb_rm_do = 1)
      m <- rmp()[reason == "bad"]
      check("Remove writes removal rows into the corrections over a period: the site, the depths and the variable, one row per run",
            nrow(m) == 2 && all(m$site == s_hobo) && setequal(m$depth_m, c("0.1", "0.2")) && all(m$variable == "temp") && all(m$action == "remove"))
      check("the removal shows as saved but not applied, in the Standard run state too", all(wb$grid()$states[[p1]]$state[20:22] == "pend") && isTRUE(std$rv$stale))
      session$setInputs(wb_grid_sel = selg(g, 2, 21)); session$setInputs(wb_act_restore = 1); session$setInputs(wb_rs_do = 1)
      check("Restore deletes the rows that cover the selected values, and only those", identical(rmp()[reason == "bad", depth_m], "0.1"))
      session$setInputs(wb_grid_sel = selg(g, 1, 30, 31)); session$setInputs(wb_act_flag = 1); session$setInputs(wb_fl_reason = "odd", wb_fl_author = "WB", wb_fl_do = 1)
      check("Flag writes a row with action flag", nrow(rmp()[reason == "odd" & action == "flag" & depth_m == "0.1"]) == 1)
      invisible(capture.output({ session$setInputs(run = 1); if (isTRUE(std$rv$stale)) session$setInputs(run_force = 1) }))
      d <- app$wb_load(paste0("temp:", s_hobo), proj)
      check("Run in the Workbench is the Standard run, and it redoes only the logger at this site",
            all(is.na(d[[p1]][20:22])) && !isTRUE(std$rv$stale) && identical(app$run_redone(proj), paste0(" Redone: 1 of ", mf_of(proj)("loggers_checked"), " loggers; the rest was unchanged and reused.")) &&
            all(grepl("manual_flag", d[[paste0("flag|", p1)]][30:31])))
      g <- wb$grid(); session$setInputs(wb_grid_sel = selg(g, 1, 1, 3))
      session$setInputs(wb_act_trim = 1); session$setInputs(wb_tr_what = "start", wb_tr_reason = "not in the ground", wb_tr_author = "WB", wb_tr_do = 1)
      tr <- rmp()[reason == "not in the ground"]
      check("Trim writes a row that removes every depth before the record starts", nrow(tr) == 1 && tr$depth_m == "" && tr$end == fmt_ts(g$w$TIMESTAMP[3]) && tr$start == fmt_ts(min(g$w$TIMESTAMP) - 1))
      session$setInputs(wb_act_clock = 1)
      session$setInputs(wb_ck_serial = "1234567", wb_ck_min = "-60", wb_ck_start = "", wb_ck_end = "", wb_ck_reason = "clock an hour fast", wb_ck_do = 1)
      tc <- rdg("time")
      check("Clock writes a time_corrections.csv row for the logger", identical(names(tc), TIME_COLS) && nrow(tc) == 1 && tc$serial == "1234567" && tc$shift_minutes == "-60")
    })
  }
}
check("every run leaves a log in output/logs that says it finished", grepl("^Run finished", utils::tail(readLines(sort(list.files(file.path(proj, "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]), 1)))
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE); source(file.path(here, "tests", "samples.R"), local = TRUE)
  r <- check_reader_samples(file.path(here, "tests", "samples"), SAMPLE_READ); new <- r[is.na(matches_expected), file]
  if (length(new)) cat("  (expected results saved for", length(new), "new sample(s): check them in", file.path("tests", "samples", "expected"), ")\n")
  for (f in r[!(accepted), file]) cat("  ", f, ": not accepted. Check tests/samples/expected/", f, ".rds, then accept_sample(\"tests/samples\", \"", f, "\", \"<your name>\")\n", sep = "")
  for (f in r[reference_ok %in% FALSE, file]) cat("  ", f, ": a value in tests/samples/reference.csv differs from what the reader makes\n", sep = "")
  check("every sample file reads, with rows (or a header for a format read only that far), the same twice and the same as its expected result, accepted, and equal to its hand-checked reference values",
        nrow(r) > 0 && all(r$rerun_identical) && !any(r$matches_expected %in% FALSE) && all(r$rows > 0 | r$kind %in% SAMPLE_NO_ROWS) && all(r$accepted) && !any(r$reference_ok %in% FALSE)) })
local({ sd <- file.path(here, "tests", "samples"); hb <- list.files(sd, "^hobo_binary__", full.names = TRUE); hc <- list.files(sd, "^hobo_csv__", full.names = TRUE)   # a header has no table for reference.csv
  if (length(hb) && length(hc)) { b <- read_hobo_binary_header(hb[1])$meta; c <- read_hobo_csv(hc[1], header_only = TRUE)$meta
    check("the .hobo header gives the serial, plot title and time zone its HOBOware export states", identical(b$serial, c$serial) && identical(b$title, c$title) && identical(b$offset_hours, c$offset_hours) && grepl("^HOBO U", b$model)) } })
check("no app function replaces a core function of the same name", !length(core_shadows(app_code_files(PIPELINE_DIR))))
check("each cached step has its own code fingerprint, the same when computed again", identical(STEP_HASH, step_hashes(globalenv())) && all(nchar(unlist(STEP_HASH)) == 12))
local({                                                               # the shared deployment sheet (core deployments.R)
  pd <- tempfile("dep_gt"); dir.create(file.path(pd, "config"), recursive = TRUE)
  fwrite(data.table(serial = c("E00001", "1000004"), site = "Plot", station = c("Borehole 3", ""), from = c("2019", ""), to = "", notes = ""), file.path(pd, "config", "deployment_sheet.csv"))
  fwrite(data.table(serial = "1000004", name = "", site = "Own site", depths = "", model = "", start = "", end = "", notes = ""), file.path(pd, "config", "metadata.csv"))
  config_migrate(pd); ms <- meta_sheet(pd, META_FIELDS, report = FALSE, deploy = gt_deploy_rows, map = GT_MAP); dsh <- read_cfg(file.path(pd, "config", DEPLOY_SHEET))
  check("an older version's logger list is moved once into the deployment sheet (station as the site, From as the start); what the list said is kept",
        nrow(ms) == 2 && ms[serial == "E00001", site] == "Borehole 3" && format(ms[serial == "E00001", start]) == "2019-01-01" && ms[serial == "1000004", site] == "Own site" &&
        nrow(dsh) == 2 && all(c("name", "depths", "model") %in% names(dsh)) && file.exists(file.path(pd, "config", "metadata.csv")) && identical(config_migrate(pd), character())) })
local({                                                               # damaged input (core testkit.R): runs that must not stop
  source(file.path(CORE_DIR, "testkit.R"), local = TRUE); sd <- file.path(here, "tests", "samples")
  if (!length(list.files(sd, "__"))) { cat("  (no sample files in tests/samples: damaged-input runs skipped)\n"); return(invisible()) }
  rn <- function(raw, p) invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, metadata_dir = "", project_dir = p, make_plots = FALSE)))))
  d <- damaged_file_runs(sd, rn); bad <- d[result != "ok"]; if (nrow(bad)) print(bad)
  check(paste0("a damaged raw file (", paste(unique(d$damage), collapse = ", "), ") never stops a run: ", nrow(d), " runs"), !nrow(bad))
  raw0 <- tempfile("raw0"); dir.create(raw0); for (f in list.files(sd, "__", full.names = TRUE)) if (!dir.exists(f)) file.copy(f, file.path(raw0, sub("^.*?__", "", basename(f))))
  p0 <- tempfile("p0"); rn(raw0, p0); t <- damaged_table_runs(p0, function() rn(raw0, p0)); bad <- t[result != "ok" & !startsWith(result, "The set-up table")]; if (nrow(bad)) print(bad)
  check(paste0("a damaged or emptied set-up table never stops a run without saying which table: ", nrow(t), " runs"), !nrow(bad)) })
local(source(file.path(here, "tests", "test_bad_files.R"), local = TRUE))   # damaged raw files: a clear problem or flag, never a stop or a silent loss
local(source(file.path(here, "tests", "test_golden.R"), local = TRUE))      # the golden project: a whole run against accepted outputs
local(source(file.path(here, "tests", "test_formulas.R"), local = TRUE))    # derived values against worked examples from their sources
cat("sensors above the ground (a chain in a casing that stands above the surface)\n")
local({
  check("depth convention: \"down\" (the default) keeps typed depths, \"up\" turns them over", depth_sign(list()) == 1 && depth_sign(list(depth_positive = "down")) == 1 && depth_sign(list(depth_positive = "Up")) == -1)
  typed <- c(-1.27, -0.5, 0.2, 1.58)                                       # above the surface positive, as on a field sheet
  sens <- data.table(logger_type = "geoprecision", make = "GeoPrecision", model = "", serial = "E00001", sensor = paste0("T", 1:4), depth_m = format(typed),
                     depth_source = "logger list", site = "BH", start = "", end = "", notes = "")
  d <- data.table(sensor = rep(paste0("T", 1:4), each = 2), TIMESTAMP = rep(as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "UTC"), 4), value = 1, variable = "temp")
  up <- assign_sensors(copy(d), sens, "geoprecision", "E00001", depth_sign(list(depth_positive = "up")))
  dn <- assign_sensors(copy(d), sens, "geoprecision", "E00001")
  check("with \"up\", a sensor typed 1.58 is 1.58 m above the ground and one typed -1.27 is 1.27 m down", isTRUE(all.equal(sort(unique(up$depth_m)), sort(-typed))))
  check("with the default, typed depths are used as they are (metres below the surface)", isTRUE(all.equal(sort(unique(dn$depth_m)), sort(typed))))
  seen <- data.table(logger_type = "geoprecision", serial = "E00002", sensor = paste0("T", 1:3), logger_id = "x", model = "")
  cs <- complete_sensors(ensure_cols(data.table(), SENSOR_COLS)[0], seen, data.table(make = character(), model = character(), depths = character()),
                         list(default_first_depth = 0.1, default_step = 0.1, depth_positive = "up"))$sensors
  check("with \"up\", default depths are written below the surface in the project's convention (negative)", isTRUE(all.equal(as.numeric(cs$depth_m), c(-0.1, -0.2, -0.3))))
  cs2 <- complete_sensors(ensure_cols(data.table(), SENSOR_COLS)[0], seen, data.table(make = character(), model = character(), depths = character()),
                          list(default_first_depth = 0.1, default_step = 0.1, depth_positive = "up"), list(E00002 = c("1.2", "0.5", "-0.4")))$sensors
  check("with \"up\", logger-list depths are kept as they were typed", isTRUE(all.equal(as.numeric(cs2$depth_m), c(1.2, 0.5, -0.4))))
  check("a leading + always means above the ground: \"+1.58, 0.5, 1.0\" is -1.58, 0.5, 1.0 inside GT, with either convention",
        isTRUE(all.equal(typed_depth(c("+1.58", "0.5", "1.0")), c(-1.58, 0.5, 1))) && isTRUE(all.equal(typed_depth(c("+1.58", "-0.5", "-1.0"), -1), c(-1.58, 0.5, 1))) &&
        isTRUE(all.equal(typed_depth(" +0.2 "), -0.2)) && !depth_ok("+") && !depth_ok("abc") && depth_ok("+0.5"))
  sp <- copy(sens); sp[, depth_m := c("1.27", "0.5", "+0.2", "+1.58")]
  pl <- assign_sensors(copy(d), sp, "geoprecision", "E00001")
  check("a + depth in sensors.csv reaches the data as above the ground", isTRUE(all.equal(sort(unique(pl$depth_m)), c(-1.58, -0.2, 0.5, 1.27))))
  cs3 <- complete_sensors(ensure_cols(data.table(), SENSOR_COLS)[0], seen, data.table(make = character(), model = character(), depths = character()),
                          list(default_first_depth = 0.1, default_step = 0.1), list(E00002 = c("+0.3", "0.5", "1")))$sensors
  check("a logger-list depth written with + keeps its + in sensors.csv", identical(cs3$depth_m[1], "+0.3") && isTRUE(all.equal(as.numeric(cs3$depth_m[2:3]), c(0.5, 1))))
  cs4 <- complete_sensors(ensure_cols(data.table(), SENSOR_COLS)[0], seen, data.table(make = character(), model = character(), depths = character()),
                          list(default_first_depth = 0.1, default_step = 0.1, assign_default_depths = FALSE))$sensors
  cs5 <- complete_sensors(cs4, seen, data.table(make = character(), model = character(), depths = character()), list(default_first_depth = 0.1, default_step = 0.1))$sensors
  check("with default depths off, sensors with no depth are left blank, and they get the defaults when it is on again",
        all(cs4$depth_m == "") && all(cs4$depth_source == "default") && isTRUE(all.equal(as.numeric(cs5$depth_m), c(0.1, 0.2, 0.3))))
  sb <- copy(sens); sb[1, `:=`(depth_m = "", depth_source = "default")]; bl <- assign_sensors(copy(d)[, `:=`(logger_id = "E00001", channel = "temp")], sb, "geoprecision", "E00001")
  check("the values of a sensor left without a depth are removed, not kept at no depth", !anyNA(bl$depth_m) && nrow(bl) < nrow(assign_sensors(copy(d), sens, "geoprecision", "E00001")))
  ms <- data.table(serial = "E00003", name = "", site = "BH2", depths = list(c("+0.4", "0.5")), model = "", start = as.Date(NA), end = as.Date(NA), notes = "")
  check("the logger list's depths are read as text, so a + is not lost", identical(sheet_sensors(ensure_cols(data.table(), SENSOR_COLS)[0], ms)$depths$E00003, c("+0.4", "0.5")))
  check("labels and report bands name sensors above the ground", depth_lab(-1.58) == "1.58 m above ground" && depth_lab(0.5) == "0.50 m" &&
        gt_band(-0.2) == "Above ground" && gt_band(0.2) == "Near surface (above 0.5 m)" && identical(depth_position(c(-0.2, 0, 1)), c("above ground", "ground", "ground")))
  dz <- seq(as.Date("2018-10-01"), as.Date("2021-09-30"), by = "day"); doy <- as.numeric(format(dz, "%j"))
  prof <- function(z, m, a) data.table(site = "BH", depth_m = z, date = dz, temp = m + a * sin(2 * pi * (doy - 120) / 365.25), complete = TRUE)
  dd <- rbind(prof(-1.58, -3, 18), prof(-0.2, -2.5, 14), prof(0.5, 0.5, 4), prof(1.27, -0.8, 0.3))   # two in the casing, two in the ground
  an <- annual_stats(dd, 0.9, 10)
  check("annual statistics keep above-ground sensors, marked so their mean is not taken as a ground temperature (MAGT)",
        all(an[depth_m < 0, position] == "above ground") && all(an[depth_m >= 0, position] == "ground"))
  td <- thaw_depth(dd); gs <- ground_state(an); fd <- fill_daily(dd[!(depth_m == 0.5 & date > as.Date("2020-07-01") & date < as.Date("2020-07-04"))], 5, 0, 0.95)
  check("thaw depth, ground state and gap fill use only sensors in the ground", (!nrow(td) || all(td$thaw_m >= 0, na.rm = TRUE)) && all(gs$depth_m >= 0) && (!nrow(fd) || all(fd$depth_m >= 0)))
  al <- active_layer(copy(td), an)
  check("the active layer counts only the sensors in the ground", all(al$shallowest_m >= 0, na.rm = TRUE) && all(al$sensors == 2, na.rm = TRUE))
  mk_an <- function(s, z, y, fr) data.table(site = s, depth_m = z, year = y, year_type = "calendar", complete = TRUE, max = ifelse(fr, -0.5, 1))
  pa <- rbind(mk_an("P", 1, 2019:2021, TRUE), mk_an("Q", 1, 2019:2021, c(FALSE, TRUE, FALSE)), mk_an("R", 1, 2020L, TRUE))
  ptd <- data.table(site = rep(c("P", "Q", "R"), each = 3), date = rep(as.Date(c("2019-08-30", "2020-08-30", "2021-08-30")), 3), thaw_m = 0.6, state = "between sensors")
  p2 <- active_layer(copy(ptd), pa); p1 <- active_layer(copy(ptd), pa, c(P = 2, Q = 1, R = 1))
  check("permafrost needs two consecutive frozen years (van Everdingen 1998): one frozen year between thawed ones is not permafrost, one year alone is unconfirmed",
        all(p2[site == "P", result] == "ALT") && grepl("pereletok", p2[site == "Q" & year == 2020, result]) && is.na(p2[site == "Q" & year == 2020, alt_m]) &&
        grepl("not yet confirmed", p2[site == "R" & year == 2020, result]) && p2[site == "R" & year == 2020, alt_m] == 0.6)
  check("the number of years can be set per site (calibration options); 1 is the old one-year test", p1[site == "Q" & year == 2020, result] == "ALT" && p1[site == "R" & year == 2020, result] == "ALT")
  check("trends of the annual mean leave sensors above the ground out", !nrow(trends(an, data.table(site = "BH", year = 2019:2021, alt_m = 1), 2)[depth_m < 0]))
})
local({                                                               # depths, between-site flags, the water year, the configuration
  check("a depth written back in the project's convention reads back the same (up and down; + always above)",
        identical(typed_depth(depth_typed(c(0.5, -0.3), -1), -1), c(0.5, -0.3)) && identical(typed_depth(depth_typed(c(0.5, -0.3), 1), 1), c(0.5, -0.3)) &&
        typed_depth("+0.3", -1) == -0.3 && typed_depth("+0.3", 1) == -0.3)
  e <- export_depths(data.table(site = "A", depth_m = c(0.5, -0.2), gst_depth_m = 0.05, alt_m = 0.8), -1)
  check("the export turns depth columns into a project's up-positive convention and leaves thicknesses", identical(e$depth_m, c(-0.5, 0.2)) && all(e$gst_depth_m == -0.05) && all(e$alt_m == 0.8))
  check("export_depths leaves a down-positive project's tables as they are", identical(export_depths(data.table(depth_m = 0.5), 1)$depth_m, 0.5))
  t0 <- as.POSIXct("2020-01-01", tz = "UTC")
  x <- data.table(site = "A", logger_id = "L", channel = c("1", "1", "2"), variable = "temp", TIMESTAMP = t0 + c(0, 86400 * 3, 0), value = 1, flag = c("", "spike", ""))
  per <- data.table(site = "A", logger_id = "L", channel = "1", start = t0 - 3600, end = t0 + 86399)
  y <- between_site_mark(copy(x), per)
  check("between-site flags go into the records' flag column, only in their stretch and channel", identical(y$flag, c("differs_from_other_sites", "spike", "")))
  check("between-site key: empty with no flags for the site, set with some", identical(between_site_key(per, "B"), "") && nzchar(between_site_key(per, "A")) &&
        identical(between_site_key(NULL, "A"), ""))
  check("summary-only settings and Run anyway are left out of the cache key", all(c("water_year_start", "permafrost_years", "run_anyway", "fill_min_depth") %in% SUMMARY_KEYS) &&
        !any(c("spike_k", "depth_positive", "air_like_min_depth", "min_year") %in% SUMMARY_KEYS))
  check("water-year months follow the start month", wy_months(10) == "October to September" && wy_months(7) == "July to June" && wy_months(1) == "January to December")
  check("the seasons plot's axis starts at the water year's first month", format(doy_axis(as.Date("2020-07-01"), "water", 7), "%m-%d") == "07-01")
  pd <- file.path(tempdir(), paste0("gt_cfgx_", as.integer(Sys.time()))); dir.create(file.path(pd, "config"), recursive = TRUE)
  fwrite(data.table(serial = "123", site = "A"), file.path(pd, "config", "metadata.csv")); fwrite(data.table(id = "x1", decision = "not a problem"), file.path(pd, "config", "review_decisions.csv"))
  fwrite(data.table(site = "A", landcover = "Bog"), cfg_path(pd, "sites"))
  z <- file.path(tempdir(), "gt_cfgx.zip"); export_config(pd, list(min_year = 2000, drop_stray_records = FALSE), z)
  inz <- zip::zip_list(z)$filename; imp <- read_config_import(z, "cfg.zip")
  check("the configuration export holds the deployment sheet (the logger list moved in) and the review decisions beside the set-up tables and settings",
        all(c(DEPLOY_SHEET, "review_decisions.csv", "sites.csv", "settings.csv") %in% inz) && !"metadata.csv" %in% inz && "drop_stray_records" %in% imp$settings$setting)
  check("the import reads them back by name, not as a set-up table", identical(names(imp$extra), "review_decisions.csv") && imp$deploy_sheet$serial == "123" && !is.null(imp$sites) && is.null(imp$sensors))
  pd2 <- file.path(tempdir(), paste0("gt_cfgy_", as.integer(Sys.time()))); dir.create(pd2)
  apply_config_import(imp, pd2, "replace", "me")
  check("and writes them into the project's config folder", identical(read_cfg(file.path(pd2, "config", DEPLOY_SHEET))$serial, "123") && file.exists(file.path(pd2, "config", "review_decisions.csv")))
  fwrite(data.table(logger_type = "rbr", serial = c("11", "22"), sensor = "T1", site = c("Here", auto_site("rbr", "22"))), cfg_path(pd2, "sensors"))
  ck <- gt_needs_check(list(unclaimed = data.table(serial = c("11", "22", "123", "99"))), pd2)
  check("Needs you leaves out loggers the logger list or sensors.csv places (not a site named after the logger)", identical(ck$unclaimed$serial, c("22", "99")))
  # the permafrost test is a setting; an old calibration_options.csv gives its value until the setting is saved
  fwrite(data.table(app = "GT", value = "permafrost_years", applies_to = c("*", "A"), choice = "custom", number = c("1", "3"), source = "", notes = ""), file.path(pd2, "config", CALIB_FILE))
  check("permafrost years: the setting, else the old calibration table's row for all sites, else 2",
        gt_permafrost_years("3", pd2) == 3L && gt_permafrost_years(NA, pd2) == 1L && gt_permafrost_years(NULL, pd) == 2L && gt_permafrost_years("9", pd) == 2L)
  ci <- read_config_import(file.path(pd2, "config", CALIB_FILE))
  check("an old calibration_options.csv imports as the permafrost setting", identical(ci$settings$setting, "permafrost_years") && identical(ci$settings$value, "1"))
  sw <- vapply(Filter(function(x) isTRUE(x$panel), CORRECTIONS), function(x) x$setting, "")
  check("every on/off switch is on the Automatic corrections list, with the old defaults",
        all(c("correct_rbr_drift", "fill_in_annual", "check_air_like", "check_pre_install", "check_exports") %in% sw) &&
        identical(corrections_table(CFG)[match(c("rbr_drift", "fill_in_annual", "air_like", "check_exports"), id), state], c("off", "off", "on", "on")))
  check("the heatmap's smoothing is one number, 0 = off", smooth_steps(0) == 1 && smooth_steps(1) == 1 && smooth_steps(7) == 7 && smooth_steps(NA) == 7 && smooth_steps("") == 7)
  rc <- report_choices(c("A", "B|2"))
  check("one report picker: each report in HTML and PDF, and every report as a ZIP",
        length(unlist(rc)) == 8 && identical(report_pick(rc$PDF[["B|2 (PDF)"]]), list(format = "pdf", what = "B|2")) && identical(report_pick("html|"), list(format = "html", what = "")) &&
        identical(report_pick(NULL)$format, "html") && report_pick(rc$HTML[["Every report, as a ZIP (HTML)"]])$what == "*")
  check("the shallow/deep cut is fixed at 3 m; the profile column of sites.csv still decides", PROFILE_CUT_M == 3 && is.null(formals(write_gt_reports)$profile_cut))
})
if (!identical(Sys.getenv("OVID_SKIP_UI"), "1") && requireNamespace("chromote", quietly = TRUE) && !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) local({
  source(file.path(CORE_DIR, "uitest.R"), local = TRUE)
  ui <- tryCatch(ui_click_through(normalizePath(here), c(GT_PROJECT_DIR = proj)), error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check("every page opens in a browser with no output, JavaScript or server error (the test project)", nrow(ui) > 5 && !nrow(bad)) })
cat(sprintf("\n%d passed, %d failed\n", n_ok, n_bad))
quit(status = if (n_bad) 1 else 0)
