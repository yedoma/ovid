# tests/test_bad_files.R  --  Damaged copies of the reader samples, and survey files that do not fit together, made in a
# temporary folder at test time: each must give a clear finding (a status, a count, a note), never a stopped run and never a
# file that disappears from the file audit.   Rscript tests/test_bad_files.R   (exit 1 on a failure; also run by run_tests.R)
#   GS_BAD_FILES_TABLE=1 Rscript ...   prints what GS does with each damaged file
# Every sample is emptied, cut in half and cut to its first 3 lines (an .stg's header). Then: an .stg in feet, a line with an
# electrode never used and one with a record whose positions are missing, a GPR line with no traces and one whose header has no
# sample count, and coordinates typed in the wrong system (UTM, latitude and longitude swapped).
# Core candidate: the damage set (as GT, WLR, CS and RS tests/test_bad_files.R).

if (!exists("check")) {
  Sys.setenv(GS_NO_RUN = "1", OVID_RUN_SYNC = "1")
  here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
  source(file.path(here, "gs_pipeline.R"), chdir = TRUE)
  fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
  BF_ALONE <- TRUE
}
if (!exists("read_rad")) source(file.path(here, "R", "gpr.R")); if (!exists("parse_latlon")) source(file.path(here, "R", "maps.R"))   # sourced by app.R only
say("damaged raw files")
local({
  work <- tempfile("gs_bad"); dir.create(work)
  sd <- file.path(here, "tests", "samples"); smp <- list.files(sd, "__", full.names = TRUE); smp <- smp[!dir.exists(smp)]; nm <- sub("^.*?__", "", basename(smp))
  lines_of <- function(b, n) { i <- which(b == as.raw(10L)); if (length(i) >= n) b[seq_len(i[n])] else b }
  dmg <- list(empty = function(b) raw(0), truncated = function(b) b[seq_len(length(b) %/% 2)], header_only = function(b) lines_of(b, 3L))
  run_raw <- function(raw, p) tryCatch({ invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = p))))); "" }, error = function(e) conditionMessage(e))
  rd <- function(p, f) { x <- file.path(p, "output", f); if (file.exists(x) && file.size(x) > 0) fread(x, colClasses = "character") else data.table() }
  res <- list()
  for (dm in names(dmg)) {
    raw <- file.path(work, dm); dir.create(raw)
    for (i in seq_along(smp)) writeBin(dmg[[dm]](readBin(smp[i], "raw", file.size(smp[i]))), file.path(raw, nm[i]))
    p <- file.path(work, paste0("p_", dm)); err <- run_raw(raw, p); fa <- rd(p, "audit/file_audit.csv"); sv <- rd(p, "catalogue/surveys.csv")
    for (i in seq_along(nm)) { s <- if (nrow(fa)) fa[basename(file) == nm[i]] else data.table()
      res[[length(res) + 1L]] <- data.table(damage = dm, kind = sub("__.*$", "", basename(smp[i])), file = nm[i], run_error = err, listed = nrow(s) > 0,
        status = if (nrow(s)) s$status[1] else NA_character_, used = if (nrow(s)) s$used[1] else NA_character_, reconciles = if (nrow(s)) s$reconciles[1] else NA_character_,
        what = if (nrow(s)) s$what_happened[1] else NA_character_) } }
  bf <- rbindlist(res, fill = TRUE)
  if (identical(Sys.getenv("GS_BAD_FILES_TABLE"), "1")) { op <- options(width = 250); print(bf[, !"run_error"], nrows = 200); print(unique(bf[, .(damage, run_error)])); options(op) }
  check(all(!nzchar(bf$run_error)), paste0("no damaged file stops a run (", uniqueN(bf$damage), " runs, ", nrow(bf), " damaged files)"))
  check(all(bf$listed), "every damaged file stays in the file audit (none disappears)")
  check(all(bf[damage == "empty", used != "TRUE" & status == "empty file"]), "an empty file is never taken for a survey, and the file audit says it is empty")
  check(all(bf[used == "TRUE", reconciles == "TRUE"]), "every damaged survey file that was read adds up: read = in the output + left out")

  # an .stg whose positions are in feet: GS keeps positions, spacing and length in metres
  stg <- readLines(file.path(sd, "agi_stg__SCH-INV.stg"), warn = FALSE)
  ft <- file.path(work, "feet"); dir.create(ft); writeLines(sub("Unit: meter", "Unit: feet", stg), file.path(ft, "SCH-INV.stg"))
  m <- read_agi_stg(file.path(sd, "agi_stg__SCH-INV.stg"))$meas; f <- read_agi_stg(file.path(ft, "SCH-INV.stg"))
  check(identical(f$meta$unit, "feet") && isTRUE(all.equal(f$meas$ax, m$ax * 0.3048)) && isTRUE(all.equal(f$meas$geom_factor_m, m$geom_factor_m * 0.3048)) && grepl("feet", f$meta$processing %||% ""),
        "an .stg in feet: positions and geometric factors in metres (x 0.3048), and the survey says it was converted")

  # a line with an electrode never used (a gap), and a record with no positions (a broken line)
  row <- function(i, a, b, mm, n, R) sprintf("%4d,USER   ,20240831,14:00:%02d, %.5E,   2, 50, %.5E,LINEG    , %.5E, 0, 0, %.5E, 0, 0, %.5E, 0, 0, %.5E, 0, 0", i, i, R, 2 * pi * (mm - a) * R, a, b, mm, n)
  hdr <- function(n) c("Advanced Geosciences, Inc. SuperSting R8-IP Resistivity meter. S/N: SS0000001 Type: 3D", sprintf("Firmware version: 01.33.73E Survey period: 20240831 Records: %d", n), "Unit: meter")
  g <- file.path(work, "lines"); dir.create(g)
  writeLines(c(hdr(4), row(1, 0, 3, 1, 2, 10), row(2, 1, 4, 2, 3, 11), row(3, 2, 7, 3, 6, 12), row(4, 6, 9, 7, 8, 5)), file.path(g, "GAP.stg"))   # electrode at 5 m never used
  br <- c(hdr(4), row(1, 0, 3, 1, 2, 10), row(2, 1, 4, 2, 3, 11), sub("^(([^,]*,){9})[^,]*,", "\\1 ,", row(3, 2, 5, 3, 4, 12)), row(4, 3, 6, 4, 5, 9))
  writeLines(br, file.path(g, "BROKEN.stg"))
  p <- file.path(work, "p_lines"); err <- run_raw(g, p); sv <- rd(p, "catalogue/surveys.csv"); fa <- rd(p, "audit/file_audit.csv")
  gp <- sv[basename(primary_file) == "GAP.stg"]; bk <- fa[basename(file) == "BROKEN.stg"]
  check(!nzchar(err) && nrow(gp) == 1 && gp$electrode_gaps == "1" && gp$n_electrodes == "9", "a line with an electrode never used: 9 electrodes and 1 gap in the survey")
  check(nrow(bk) == 1 && bk$used == "TRUE" && bk$reconciles == "TRUE" && (as.integer(bk$dropped) > 0 || nrow(sv[basename(primary_file) == "BROKEN.stg" & n_read == "4"]) == 1),
        "a record with a missing position is either left out and counted or kept, and the file adds up")

  # GPR: a line with no traces, a header with no sample count, a data file of another size
  gd <- file.path(work, "gpr"); dir.create(gd); rad <- readLines(file.path(sd, "gpr_rad__DAT_0204.rad"), warn = FALSE)
  writeLines(sub("^LAST TRACE:.*", "LAST TRACE:0", rad), file.path(gd, "ZERO.rad")); file.create(file.path(gd, "ZERO.rd3"))
  writeLines(rad[!grepl("^SAMPLES:", rad)], file.path(gd, "NOHDR.rad")); file.copy(file.path(sd, "gpr_rd3__DAT_0204.rd3"), file.path(gd, "NOHDR.rd3"))
  writeLines(rad, file.path(gd, "SHORT.rad")); writeBin(readBin(file.path(sd, "gpr_rd3__DAT_0204.rd3"), "raw", 447L * 2L * 10L + 5L), file.path(gd, "SHORT.rd3"))
  x <- tryCatch(gpr_scan(gd), error = function(e) conditionMessage(e)); st <- if (is.data.table(x)) setNames(x$status, x$stem) else character()
  check(is.data.table(x) && nrow(x) == 3, "GPR lines with damaged files are listed, not a stopped scan")
  check(identical(st[["ZERO"]], "no traces (the header says 0)") && identical(st[["NOHDR"]], "header not read"), "a GPR line with no traces says so; a header with no sample count says 'header not read'")
  check(identical(st[["SHORT"]], "truncated") && x[stem == "SHORT"]$file_traces == 10, "a GPR data file cut to 10 traces and a few bytes is 'truncated', 10 of 223 traces")
  check(is.null(gpr_distance(read_cor(file.path(gd, "ZERO.rad")), 0)) && !nrow(read_mrk(file.path(gd, "ZERO.rd3"))), "a text file read as GPS fixes or markers gives none")

  # coordinates in the wrong system: UTM or swapped latitude/longitude are refused, a GNSS file with no elevation column says what it needs
  check(is.null(parse_latlon("500000 6800000")) && is.null(parse_latlon("-121.3, 61.3")) && identical(parse_latlon("61.3, -121.3"), c(61.3, -121.3)),
        "a position in UTM metres, or longitude before latitude, is refused; latitude, longitude is read")
  ut <- file.path(work, "utm.csv"); fwrite(data.table(Name = c("E1", "E2"), Easting = c(500000, 500001), Northing = c(6800000, 6800000)), ut)
  e <- tryCatch(elevation_file_rows(ut, "S1"), error = function(e) conditionMessage(e))
  check(is.character(e) && grepl("elevation", e), "a GNSS file with eastings and northings and no elevation is refused, saying it needs an elevation column")

  # where the map opens: nothing located shows the whole world (no region assumed); sites, then transects, take over
  lay0 <- data.table(transect_id = character(), start_lat = numeric(), end_lat = numeric(), start_lon = numeric(), end_lon = numeric())
  v0 <- map_view(lay0)
  vs <- map_view(lay0, data.table(site = "A", latitude = c("45.4", "45.6"), longitude = c("-75.6", "-75.8")))
  vt <- map_view(data.table(transect_id = "T1", start_lat = 50, end_lat = 50.001, start_lon = 10, end_lon = 10.002), NULL, "T1")
  check(identical(v0, list(center = list(lat = 0, lon = 0), zoom = 0)) && vs$zoom == 6 &&
          isTRUE(all.equal(unlist(vs$center), c(lat = 45.5, lon = -75.7))) && vt$zoom == 16 && isTRUE(all.equal(vt$center$lat, 50.0005)),
        "the map opens on the whole world with nothing located, on the sites with sites only, and on a picked transect")
  unlink(work, recursive = TRUE)
})
if (exists("BF_ALONE")) { if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green") }
