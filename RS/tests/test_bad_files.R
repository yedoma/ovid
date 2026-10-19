# tests/test_bad_files.R  --  Damaged copies of the reader samples, and drone sources that do not fit together, made in a
# temporary folder at test time: each must give a clear finding (a status, a reason, a flag), never a stopped run and never a
# file that disappears from the file audit.   Rscript tests/test_bad_files.R   (exit 1 on a failure; also run by run_tests.R)
#   RS_BAD_FILES_TABLE=1 Rscript ...   prints what RS does with each damaged file
# Every sample is emptied, cut in half and cut to its first 1,000 bytes (a JPG then has no EXIF or XMP); text records also lose
# their body. The drone cases are small JPGs written here (as tests/run_tests.R writes them): photos with no GPS, a log that
# matches no photo, a camera clock in local time against UTC, and the same photo stored twice.
# Core candidate: the damage set (as GT, WLR and CS tests/test_bad_files.R).

if (!exists("check")) {
  Sys.setenv(RS_NO_RUN = "1")
  here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
  source(file.path(here, "rs_pipeline.R"), chdir = TRUE)
  fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
  BF_ALONE <- TRUE
}
say("damaged raw files")
local({
  ex <- parse(file.path(here, "tests", "run_tests.R"))                    # the JPG writers of the main suite (EXIF and XMP as cameras write them)
  for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% c("le16", "le32", "exif_app1", "jpg", "dji_xmp")) eval(e, environment())
  work <- tempfile("rs_bad"); dir.create(work)
  sd <- file.path(here, "tests", "samples"); smp <- list.files(sd, "__", full.names = TRUE); smp <- smp[!dir.exists(smp)]; nm <- sub("^.*?__", "", basename(smp))
  dmg <- list(empty = function(b) raw(0), truncated = function(b) b[seq_len(length(b) %/% 2)], header_only = function(b) b[seq_len(min(length(b), 1000L))])
  run_src <- function(src, p) tryCatch({ invisible(capture.output(run_pipeline(list(project_dir = p), scan = src, source_id = "S"))); "" }, error = function(e) conditionMessage(e))
  rd <- function(p, f) { x <- file.path(p, "output", f); if (file.exists(x) && file.size(x) > 0) fread(x, colClasses = "character") else data.table() }
  res <- list()
  for (dm in names(dmg)) {
    src <- file.path(work, dm, "Flight A"); dir.create(src, recursive = TRUE)
    for (i in seq_along(smp)) writeBin(dmg[[dm]](readBin(smp[i], "raw", file.size(smp[i]))), file.path(src, nm[i]))
    for (i in 1:3) jpg(file.path(dirname(src), "Good", sprintf("DJI_%04d.JPG", i)) |> (\(f) { dir.create(dirname(f), showWarnings = FALSE); f })(),
                       exif_app1("DJI", "FC6310R", "GOOD1", sprintf("2020:07:15 10:00:%02d", 2 * i), 45.5 + i / 1e4, -72.2, 100), dji_xmp(sprintf("2020-07-15T14:00:%02d", 2 * i), 50, "GOOD1"), pad = 36000L)
    p <- file.path(work, paste0("p_", dm)); err <- run_src(dirname(src), p); fa <- rd(p, "audit/file_audit.csv")
    for (i in seq_along(nm)) { s <- if (nrow(fa)) fa[basename(rel) == nm[i]] else data.table()
      res[[length(res) + 1L]] <- data.table(damage = dm, kind = sub("__.*$", "", basename(smp[i])), file = nm[i], run_error = err, listed = nrow(s) > 0,
        status = if (nrow(s)) s$status[1] else NA_character_, used = if (nrow(s)) s$used[1] else NA_character_, used_in = if (nrow(s)) s$used_in[1] else NA_character_,
        why = if (nrow(s) && "why" %in% names(s)) s$why[1] else if (nrow(s) && "what_happened" %in% names(s)) s$what_happened[1] else "") } }
  bf <- rbindlist(res, fill = TRUE)
  if (identical(Sys.getenv("RS_BAD_FILES_TABLE"), "1")) { op <- options(width = 250); print(bf[, !"run_error"], nrows = 200); print(unique(bf[, .(damage, run_error)])); options(op) }
  check(all(!nzchar(bf$run_error)), paste0("no damaged file stops a run (", uniqueN(bf$damage), " runs, ", nrow(bf), " damaged files, each beside a good flight)"))
  check(all(bf$listed), "every damaged file stays in the file audit (none disappears)")
  check(all(bf[damage == "empty", status == "empty file" & used == "FALSE"]), "an empty file is listed as an empty file and never catalogued (not a dataset, an other photo or an unlinked record)")

  # drone sources that do not fit together
  dcase <- function(name, build) { s <- file.path(work, name); dir.create(s); build(s); p <- file.path(work, paste0("p_", name)); e <- run_src(s, p)
    list(error = e, fl = rd(p, "catalogue/flights.csv"), im = rd(p, "catalogue/images.csv"), du = rd(p, "catalogue/duplicates.csv"), un = rd(p, "catalogue/unassigned.csv"), fa = rd(p, "audit/file_audit.csv")) }
  photos <- function(dir, n, lat = TRUE, local_h = 0, serial = "CAM1") { dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_len(n)) jpg(file.path(dir, sprintf("DJI_%04d.JPG", i)), exif_app1("DJI", "FC6310R", serial, format(as.POSIXct("2020-07-15 14:00:00", tz = "UTC") + 2 * i + local_h * 3600, "%Y:%m:%d %H:%M:%S"),
      if (lat) 45.5 + i / 1e4 else NA, -72.2, 100), dji_xmp(format(as.POSIXct("2020-07-15 14:00:00", tz = "UTC") + 2 * i, "%Y-%m-%dT%H:%M:%S"), 50, serial), pad = 36000L) }
  a <- dcase("no_gps", function(s) photos(file.path(s, "Flight"), 4, lat = FALSE))
  check(!nzchar(a$error) && nrow(a$im) == 4 && all(is.na(suppressWarnings(as.numeric(a$im$lat)))) && nrow(a$fl) == 1 && nzchar(a$fl$positioning_status),
        "photos with no GPS tags are kept as a flight with no positions, and its positioning says so")
  b <- dcase("lonely_log", function(s) { photos(file.path(s, "Flight"), 3); d <- file.path(s, "Other"); dir.create(d)
    file.copy(smp[grepl("^dji_mrk__", basename(smp))], file.path(d, sub("^.*?__", "", basename(smp[grepl("^dji_mrk__", basename(smp))])))) })
  mrk <- b$fa[grepl("\\.MRK$", rel)]
  check(!nzchar(b$error) && nrow(mrk) == 1 && (grepl("not linked|unassigned|no flight", paste(mrk$used_in, mrk$status), ignore.case = TRUE) || nrow(b$un) > 0 || any(grepl("MRK", b$fl$qa_flags))),
        "an MRK that matches no photo is listed as not linked (or its own flight is flagged), not taken for the photos' log")
  z <- dcase("local_clock", function(s) photos(file.path(s, "Flight"), 4, local_h = -6))
  check(!nzchar(z$error) && nrow(z$fl) == 1 && identical(as.numeric(z$fl$utc_offset_h), -6) && identical(z$fl$clock_ok, "TRUE"),
        "a camera clock in local time (EXIF 6 hours behind its XMP UTC) gives the flight's UTC offset of -6 and a sound clock")
  d <- dcase("copies", function(s) { photos(file.path(s, "Flight"), 3); dir.create(file.path(s, "Backup")); file.copy(list.files(file.path(s, "Flight"), full.names = TRUE), file.path(s, "Backup")) })
  check(!nzchar(d$error) && nrow(d$du) >= 1 && nrow(d$fl) == 1, "the same photos stored twice are one flight, the copies listed in duplicates.csv")
  unlink(work, recursive = TRUE)
})
if (exists("BF_ALONE")) { if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green") }
