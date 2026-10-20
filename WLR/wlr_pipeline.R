#!/usr/bin/env Rscript
# =============================================================================
#  wlr_pipeline.R  --  Water level recorder ingest / stitch / compensate / QC pipeline
# =============================================================================
#  What it does
#    1. Finds every HOBO (.hobo + HOBOware CSV) and Solinst (.xle + CSV) file under RAW_DIRS, reads each one's
#       identity, pairs binaries with their CSV exports and spots duplicate files.
#    2. Reads the WL_Metadata workbook (one sheet per year) for sites, serials, visits and hang lengths (T_ST).
#    3. Stitches every file of one serial, from every year, into one continuous record in kPa.
#    4. Splits each logger's record into deployments (serial at a site for a year) from the workbook visits.
#    5. Subtracts barometric pressure from the primary (or, in its gaps, the secondary) barologger of the site's
#       baro group for that year, and converts to metres of water above the sensor.
#    6. Adds the elevation of the water from the dGPS survey of the T-coupler (or top of casing), T_ST and the
#       logger offset; compares the logger with the tape at every visit.
#    7. Applies quality rules, manual removals and out-of-water periods, and writes site files, plots and the audit.
#
#  The audit rule
#    Nothing is ever dropped or blanked except through remove_rows() and remove_cells(). Both write to
#    output/audit, and the run stops if record counts do not reconcile against that log. Raw files are never modified.
#
#  Use
#    Rscript wlr_pipeline.R            (or Source it in RStudio, or press Run in the app)
#    First run creates the config CSVs (and suggests baro groups); edit them in the app and re-run.
#
#  Needs: data.table, ggplot2, xml2, readxl
# =============================================================================

for (.l in if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8", "UTF-8"))   # UTF-8 text ( deg C, uS/cm);
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break                               # macOS has no C.UTF-8

OVID_APP <- "WLR"                                          # its projects go in ~/Desktop/Ovid_Projects/WLR/ (core util.R)
CFG <- list(
  raw_dirs     = c("~/WLR/data"),           # holds 2025/, 2024/ ... and baro/2025/, baro/2024/ ...
  metadata_dir = "~/WLR/metadata",          # the newest .xlsx here is the WL_Metadata workbook
  dgps_dir     = "~/WLR/dgps",              # dGPS survey exports (.csv)
  project_dir  = "",        # config/ and output/ live here
  years        = NULL,                      # process only these deployment years, e.g. c(2024, 2025); NULL = every year found

  min_year          = 2000,                 # time stamps before this year or in the future are a reset clock, and are left out
  drop_stray_records = TRUE,                # leave out records with corrupt time stamps among good ones (off the time grid and out of order); each is audited
  base_offset_h     = -7,                   # every time stamp is moved to UTC-7; set -6 to standardise every record to UTC-6
  utm_zone          = 10,                   # the UTM zone of the workbook and survey eastings/northings (Settings); north of the equator
  unzoned_offset_h  = NA,                   # the zone of files that state none; NA = the base zone (as before). If no file states one, the app asks
  solinst_kpa_per_m = 9.80665,              # kPa per metre of Solinst LEVEL (adjust to match Solinst's own conversion)
  solinst_altitude  = "undo",               # a Levelogger Gold / Junior programmed with an altitude adds altitude/826 m of water to every reading:
                                            # "undo" takes it off again (from each file's own header), "keep" uses the readings as stored
  density_mode      = "fixed",              # "fixed" or "temperature" (water density from the logger's temperature)
  fixed_density     = 1000,                 # kg/m3, used when density_mode = "fixed"
  gravity           = 9.80665,              # m/s2
  baro_max_gap_h    = 3,                    # barometric readings are interpolated across gaps up to this long ...
  baro_interp       = TRUE,                 # ... when on (Automatic corrections); off: only water readings at a barometric reading's time are compensated
  baro_fill         = TRUE,                 # where neither chosen barologger has a reading, fill from another one (shifted onto the primary's level, flagged)
  auto_trim_air     = FALSE,                # drop readings taken in the air at the start or end of a deployment (FALSE = flag them and list them as suggestions)
  air_max_hours     = 72,
  below_air_m       = 0.1,                  # a water logger reading this far below the air pressure cannot be right (a failing or frozen sensor): flagged
  below_air_share   = 0.05,                 # ... and reported as a problem when that holds for this share of a deployment's records                   # ... for at most this long (longer runs are left to the other checks)
  dgps_pick         = "nearest",            # which survey of a reference point to use: "nearest" (in time to the deployment), "latest", "mean", or "interpolate" (follow the surveys in time)
  sigma_baro_m      = 0.005,                # uncertainty (1 sigma, m of water) of compensation with a barologger at the site ...
  sigma_fill_m      = 0.02,                 # ... and with a fill-in (another station, reduced to the site's elevation)
  sigma_tape_m      = 0.01,                 # a tape check, when a deployment has fewer than two to take the spread from
  sigma_survey_m    = 0.02,                 # a reference point's survey, when it gives no rms
  be_flag           = 0.1,                  # a deployment whose lasting barometric efficiency is above this is listed ...
  be_min_r2         = 0.2,                  # ... when the air pressure explains at least this share of the hourly changes
  casing_tol_m      = 0.02,                 # a change in the casing's height above the ground between visits beyond this is reported
  service_window_h  = 2,                    # readings this close to a download or field visit are flagged
  out_of_water_m    = 0.02,                 # less water than this above the sensor is flagged as out of water ...
  air_same_as_oow   = TRUE,                 # ... and counts as in the air at a deployment's start or end; off: air_threshold_cm does
  air_threshold_cm  = 3,                    # in the air below this (cm of water above the sensor), when air_same_as_oow is off
  ec_alpha          = 0.0191,               # temperature coefficient for specific conductance at 25 deg C
  min_complete      = 0.8,                  # share of a day's usual records needed for a daily mean (summaries)
  year_complete     = 0.9,                  # share of a month's or a water year's days needed for it to count as complete
  water_year_start  = "10-01",              # water years (summaries, hydroperiod, recharge, season plots) run from this day (MM-DD); 1 October = the hydrological year (USGS, AMS)
  meas_window_h     = 1,                    # a manual measurement is compared with logger records at most this far away
  check_window_h    = 3,                    # logger readings this far before a visit are compared with the tape
  hang_measured_to  = "tip",                # what T_ST is measured to unless a deployment says otherwise: "tip" (end of the logger) or "sensor"
  add_sensor_offset = FALSE,                # add the tip-to-sensor distance (config/logger_offsets.csv) where the hang length was measured to the tip
  drift_correction  = "off",                # off (default) or apply (apply the offsets from the tape checks); see R/drift.R
  drift_min_m       = 0.01,                 # apply tape offsets only where the largest is at least this many metres
  drift_one_visit   = "constant",           # with one tape check: hold the offset all through ("constant") or grow it from zero at the start ("linear")
  tape_warn_m       = 0.05,                 # tape and logger differing by more than this (median of a deployment's checks) is listed as a likely metadata error
  hide_accepted_flags = TRUE,               # flags decided not a problem leave the exported flag columns (flags.csv keeps them, marked accepted)
  make_plots        = TRUE,
  incremental       = TRUE,                 # keep each file as read, and each plot, in <project>/cache; a rerun reads and draws only what changed
  plot_max_points   = 20000
)
UTM_ZONE <- CFG$utm_zone                    # set from the settings at each run (and by the app when they change)
if (nzchar(Sys.getenv("WLR_RAW_DIRS")))     CFG$raw_dirs     <- strsplit(Sys.getenv("WLR_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("WLR_METADATA_DIR"))) CFG$metadata_dir <- Sys.getenv("WLR_METADATA_DIR")
if (nzchar(Sys.getenv("WLR_DGPS_DIR")))     CFG$dgps_dir     <- Sys.getenv("WLR_DGPS_DIR")
if (nzchar(Sys.getenv("WLR_PROJECT_DIR")))  CFG$project_dir  <- Sys.getenv("WLR_PROJECT_DIR")
RUN_ON_SOURCE <- !identical(Sys.getenv("WLR_NO_RUN"), "1")

for (p in c("data.table", "ggplot2", "xml2", "readxl"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

PIPELINE_DIR <- local({ f <- NULL
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { f <- o; break } }
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) getwd() else dirname(normalizePath(f)) })
if (!dir.exists(file.path(PIPELINE_DIR, "R"))) stop("The R/ folder that belongs beside wlr_pipeline.R was not found in ", PIPELINE_DIR)
APP_NAME <- "Ovid Core // WLR"
APP_VERSION <- PIPELINE_VERSION <- "0.12.0"          # bump when a change alters the output

# The shared Ovid Core files (audit trail, helpers, set-up tables, run manifest, set-up check): from OVID_CORE if
# set, else core/ beside this file (a portable copy), else ~/Desktop/Ovid. They are sourced first; R/ holds what is WLR's own.
CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(PIPELINE_DIR, "core"), file.path(dirname(PIPELINE_DIR), "core"), "~/Desktop/Ovid")
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "util.R"))]
  if (!length(hit)) stop("The shared Ovid Core folder was not found. Looked in: ", paste(d, collapse = ", "), ". Set OVID_CORE to its path.")
  normalizePath(hit[1]) })
for (.f in c("util", "convert", "audit", "qc", "trend", "standards", "file_audit", "logger_files", "hobo", "cache", "timezones", "timestamps", "time_corrections", "config", "periods", "table_rules", "metasheet", "import", "deployments", "corrections", "applied", "manifest", "preflight", "theme", "report", "report_options"))
  source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
CFG$project_dir <- ovid_project_dir(CFG$project_dir)               # none set: ~/Desktop/Ovid_Projects/<app>/Untitled (core util.R)
for (.f in c("util", "plots", "readers", "units", "discover", "metadata", "stitch", "baro", "baro_check", "qc", "datum", "summaries", "drift", "hydro", "config", "export", "preflight", "report"))
  source(file.path(PIPELINE_DIR, "R", paste0(.f, ".R")), local = TRUE)
CODE_HASH <- code_hash(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "wlr_pipeline.R"))   # every file's code (the run manifest)
# What each cached step depends on (core code_fingerprint()): only the code it runs, so an edit to a report, a page or a core
# file a step does not use redoes nothing. Reading follows the readers only; the steps made inline in run_pipeline() also
# take its own text.
CODE_NAMES <- code_names(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "wlr_pipeline.R"))
step_hashes <- function(env = parent.frame()) list(
  read      = code_fingerprint("read_logger_file", env = env),
  stitch    = code_fingerprint(c("apply_time_corrections", "stitch_serial", "solinst_baro_offset", "kpa_factor", "in_baro_window"), shallow = "run_pipeline_steps", env = env),
  agree     = code_fingerprint("baro_agreement", env = env),
  plots     = code_fingerprint(c("plot_logger", "nord_save"), env = env),
  deploy    = code_fingerprint(c("apply_limits", "apply_manual", "period_step", "period_apply", "compensate_chain", "kpa_to_m", "ref_series", "wt_ground_of", "detect_air_runs", "add_flag",
                                 "apply_auto_flags", "manual_check", "tape_offset_check", "apply_drift", "deployment_uncertainty", "barometric_efficiency",
                                 "daily_of", "compare_measurements", "audit_add", "remove_rows", "empty_events", "season_year"), shallow = "run_pipeline_steps", env = env),
  corrected = code_fingerprint(c("export_table", "fix_ts_cols"), shallow = "run_pipeline_steps", env = env))
CACHE_VERSION <- 1L                                                    # the layout of cache/stitch, cache/logger and cache/corrected
STITCH_KEY_COLS <- c("path", "md5", "format", "kind", "year", "model", "offset_hours", "unit_pres", "unit_temp", "unit_ec", "first_ts", "last_ts",
                     "tz_override", "tz_table", "altitude_m")                # what a logger's stitched record depends on in files.csv

# WLR records are wide (one row per reading time): an audit line names the site, logger and column.
audit_schema(keys = c("site", "logger_id", "timestamp", "column", "old_value"), index_by = c("site", "logger_id", "column"),
             summary_by = c("site", "logger_id", "source_file"), cells_skip_na = TRUE)

SKIP_WHY <- c(duplicate_file = "byte-identical copy of another file", empty_file = "empty file", unrecognised_format = "not a HOBO or Solinst file",
              unreadable = "file could not be read", no_data_rows = "file holds no readings", no_serial = "no serial number in the file",
              paired_hobo = "HOBO binary; its HOBOware CSV export is read instead", hobo_without_csv = "HOBO binary of a model not read directly, with no CSV export: export it from HOBOware",
              companion_csv = "Solinst CSV export of an .xle that is read instead",
              temperature_logger = "HOBO temperature logger (no pressure or conductivity channel): read by GT, not WLR")

# One serial per logger: Solinst serials with and without a prefix (72028253 / 2028253) become the shorter one.
canonical_serials <- function(x) {
  u <- unique(x[!is.na(x) & nzchar(x)]); u <- u[order(nchar(u))]
  map <- setNames(u, u)
  for (s in u) { m <- u[serial_match(s, u) != "none"]; map[s] <- m[order(nchar(m))][1] }
  function(v) { out <- unname(map[v]); out[is.na(out)] <- v[is.na(out)]; out }
}

# =============================================================================
#  RUN
# =============================================================================
run_pipeline_steps <- function(cfg = CFG) {
  t0 <- Sys.time()
  BASE_OFFSET <<- cfg$base_offset_h %||% CFG$base_offset_h
  UTM_ZONE <<- cfg$utm_zone %||% CFG$utm_zone
  cfg$project_dir <- sub("[/\\\\]+$", "", path.expand(trim(cfg$project_dir)))
  out <- file.path(cfg$project_dir, "output")
  project_dirs(cfg$project_dir, c("config", "output/sites", "output/loggers", "output/baro", "output/audit/detail", "output/plots", "output/qc"))   # core manifest.R
  unlink(list.files(file.path(out, c("sites", "loggers", "baro", "plots", "audit/detail", "qc")), full.names = TRUE))
  FLAGS$list <- list(); rm(list = ls(FILE_CACHE), envir = FILE_CACHE)
  # the cache: each file as read (by its md5) and each deployment's plot (by its data); what this run does not use is removed at the end
  inc <- isTRUE(cfg$incremental); cdir <- file.path(cfg$project_dir, "cache")
  READ_CACHE$dir <- if (inc) file.path(cdir, "read") else NULL; READ_CACHE$used <- character(); READ_CACHE$hits <- 0L
  READ_CACHE$key <- hash_of(list(STEP_HASH$read, BASE_OFFSET)); p_used <- character(); p_hit <- 0L
  if (inc) for (d in c("read", "plots")) dir.create(file.path(cdir, d), recursive = TRUE, showWarnings = FALSE) else unlink(cdir, recursive = TRUE)
  audit_reset(file.path(out, "audit", "detail"))
  # A step read back from the cache leaves in the audit trail, the flags and the problems what it left when it ran.
  replay <- function(x) {
    AUDIT$log <- c(AUDIT$log, audit_restamp(x$log))
    if (length(x$flags)) FLAGS$list <- c(FLAGS$list, x$flags)
    for (pp in x$problems) problem(pp$where, pp$problem)
  }
  c_used <- character(); series_hash <- new.env()
  if (inc) for (d in c("stitch", "logger", "corrected", "agree")) dir.create(file.path(cdir, d), showWarnings = FALSE)

  say("Ovid Core // WLR  run ", AUDIT$run_id, col = "frost")

  # ---- 1. files ---------------------------------------------------------------------------------------------
  say("[1/7] scanning files", col = "aqua")
  man <- discover_files(cfg)
  files_gone_check(files_prev(file.path(out, "audit", "files.csv"), "path"), man$path)       # core manifest.R
  say(sprintf("  %d files: %s", nrow(man), paste(man[, .N, by = status][, paste0(N, " ", status)], collapse = ", ")), col = "fg4")
  sk <- man[status %in% names(SKIP_WHY) & !status %in% c("paired_hobo", "companion_csv")]
  if (nrow(sk)) audit_add("discover", sk$status, "file", NA, logger_id = sk$serial, column = "<file>", reason = SKIP_WHY[sk$status], source_file = sk$path)

  # ---- 2. metadata and set-up tables ----------------------------------------------------------------------------
  say("[2/7] metadata workbook and set-up tables", col = "aqua")
  mfile <- find_metadata_file(cfg$metadata_dir)
  md <- read_metadata(mfile)                                 # the workbook and the metadata sheet are both optional
  config_migrate(cfg$project_dir, "pipeline")                                  # an older version's logger list is moved into the sheet first (core config.R)
  ms <- meta_sheet(cfg$project_dir, META_FIELDS, deploy = wlr_deploy_rows)   # the logger list's columns on the shared deployment sheet (core deployments.R, metasheet.R)
  if (is.null(md)) { md <- data.table(); if (is.null(ms)) say("  no metadata workbook or deployment sheet: each logger is processed under its serial until a site is given", col = "fg4") }
  else say(sprintf("  %s: %d rows on %d year sheets", basename(mfile), nrow(md), uniqueN(md$year)), col = "fg4")
  if (!is.null(ms)) say(sprintf("  deployment sheet: %d rows", nrow(ms)), col = "fg4")
  cfgs <- load_configs(cfg)
  pfile <- cfg_path(cfg$project_dir, "period")                                         # corrections over a period (core periods.R): removals and drift rows
  cfgs$manual <- wlr_period_to_manual(period_own_rows(pfile, "remove"))                  # are applied by apply_manual() and apply_drift() as before,
  cfgs$drift <- wlr_period_to_drift(period_own_rows(pfile, WLR_DRIFT_ACTIONS))           # the rest by period_step()
  cfgs$period_rows <- period_load(pfile, skip = c("remove", WLR_DRIFT_ACTIONS))
  if (!is.null(up <- upgrade_offsets(cfgs$offsets))) { write_cfg(up, cfg$project_dir, "offsets"); cfgs$offsets <- up
    say("  logger_offsets.csv held only zeros: filled with the makers' tip-to-sensor distances (Datum and dGPS > Logger offsets)", col = "yellow") }
  man <- apply_file_rules(man, cfgs$file_rules)
  audit_flush("_files")

  canon <- canonical_serials(c(man$serial, md$serial, if (!is.null(ms)) norm_serial(ms$serial), norm_serial(cfgs$deployments$serial), norm_serial(cfgs$timezones$serial)))
  man[, serial := canon(serial)]
  md <- merge_metadata(md, sheet_metadata(ms, canon, function(s) sort(unique(man[serial == s & use %in% TRUE & !is.na(year), year]))))
  has_meta <- nrow(md) > 0
  if (nrow(md)) md[, serial := canon(serial)]
  man <- apply_zone_table(man, cfgs$timezones, canon, cfg$unzoned_offset_h %||% NA)
  u <- if ("use" %in% names(man)) man$use %in% TRUE else rep(TRUE, nrow(man))
  unzoned_check(sum(u & man$zone_why %in% c("logger clock, taken as the base zone", "no zone stated; taken as the zone chosen for such files") & is.na(man$offset_hours) & is.na(man$tz_override)),
                sum(u), cfg$unzoned_offset_h %||% NA, isTRUE(cfg$ask_unzoned), BASE_OFFSET)
  if (nrow(cfgs$deployments)) cfgs$deployments[, serial := canon(norm_serial(serial))]
  # barologgers: in a baro folder, or typed Baro in the workbook
  baro_serials <- unique(c(man[kind == "baro", serial], if (nrow(md)) md[type == "Baro" & nzchar(serial), serial]))
  # a logger redeployed between roles (a water logger in some years, a barologger in others, by the workbook's Type) is a
  # barologger only inside its Baro deployments and a water logger in the rest; one in a baro folder stays a barologger
  switching <- redeployed_serials(md, baro_serials, man[kind == "baro", serial])
  cx <- role_conflicts(md, switching)
  for (i in cx) problem(paste("metadata", md$sheet[i], "row", md$row[i]), sprintf("serial %s is a %s barologger in other years but this row names a %s %s; the row was set aside (two loggers sharing a short serial?)",
                                                                      md$serial[i], md[serial == md$serial[i] & type == "Baro", logger][1], md$logger[i], md$type[i]))
  if (length(cx)) md <- md[-cx]
  switching <- redeployed_serials(md, baro_serials, man[kind == "baro", serial])
  baro_serials <- setdiff(baro_serials, switching)
  water_windows <- water_windows_of(md, cfgs$deployments, switching)
  in_baro_window <- function(s, t) !in_windows(water_windows, s, t)
  if (length(switching)) say(sprintf("  redeployed between water and baro: %s", paste(switching, collapse = ", ")), col = "fg4")
  ruled <- man[use == TRUE & kind == "water" & serial %in% baro_serials & name %in% cfgs$file_rules[tolower(action) == "water", file]]
  man[use == TRUE & serial %in% setdiff(baro_serials, ruled$serial), kind := "baro"]

  # ---- 3. stitch ------------------------------------------------------------------------------------------------
  say("[3/7] stitching each logger", col = "aqua")
  use <- man[use == TRUE & !is.na(serial) & nzchar(serial)]
  if (!nrow(use)) stop("None of the files can be used. See the Raw files page for why.")
  recs <- list(); gaps <- list(); events <- list(); lstats <- list(); recon <- list(); contrib <- list(); alt_log <- list()
  # A logger whose files, clock corrections, role and settings are the same as in the last run is read back from the
  # cache (cache/stitch) as it was stitched then, with its audit lines, instead of being read and stitched again.
  key3 <- list(); n3_hit <- 0L
  cfg3 <- key_cfg(cfg[intersect(c("min_year", "drop_stray_records", "drop_bad_clock", "solinst_kpa_per_m", "solinst_altitude", "ec_alpha"), names(cfg))])
  stitch_one <- function(s, g) {
    r <- tryCatch(stitch_serial(g, cfg), error = function(e) { problem(paste("logger", s), conditionMessage(e)); NULL })
    if (is.null(r)) return(NULL)
    tcr <- if (NROW(cfgs$time)) which(norm_serial(cfgs$time$serial) == r$data$logger_id[1]) else integer()   # core time_corrections.R
    dt <- apply_time_corrections(r$data, cfgs$time, tcr, dedupe = TRUE); al <- list()
    # units: pressure to kPa, temperature to deg C, file by file (a logger's files can be in different units)
    if (!"pres" %in% names(dt)) dt[, pres := NA_real_]
    dt[, pres_raw := pres]
    for (u in unique(dt$unit_pres[!is.na(dt$pres)])) { k <- which(dt$unit_pres %in% u)
      f <- kpa_factor(u %||% "kPa", cfg)
      if (is.na(f)) { problem(paste("logger", s), paste0("unknown pressure unit '", u, "'; those readings were left as they are")); f <- NA_real_ }
      add <- if (s %in% baro_serials) solinst_baro_offset(dt$pres[k], u, cfg) else 0   # a Solinst Barologger writes LEVEL less 9.5 m
      if (s %in% switching) add <- ifelse(in_baro_window(s, dt$TIMESTAMP[k]), solinst_baro_offset(dt$pres[k], u, cfg), 0)
      set(dt, k, "pres_kpa", (dt$pres[k] + add) * f) }
    if (!"pres_kpa" %in% names(dt)) dt[, pres_kpa := NA_real_]
    # Solinst altitude: the logger added altitude/826 m of water to each reading; take it off again unless the setting says keep
    alt <- g$altitude_m[match(dt$source_file, g$path)]
    if (any(!is.na(alt) & alt != 0)) for (f in unique(dt$source_file[!is.na(alt) & alt != 0])) {
      k <- which(dt$source_file == f); a <- alt[k[1]]; corr <- a / 826 * (cfg$solinst_kpa_per_m %||% 9.80665)
      undo <- !identical(cfg$solinst_altitude %||% "undo", "keep")
      if (undo) set(dt, k, "pres_kpa", dt$pres_kpa[k] - corr)
      al[[length(al) + 1L]] <- data.table(serial = s, file = basename(f), altitude_m = a, correction_kpa = round(corr, 4), correction_m_water = round(a / 826, 4),
                                          records = length(k), applied = undo)
    }
    if ("temp" %in% names(dt)) dt[, temp_c := ifelse(grepl("F$", unit_temp %||% ""), convert_unit(temp, "degF", "degC"), temp)]
    ecs <- intersect(c("ec_low", "ec_full", "ec"), names(dt))
    if (length(ecs)) {
      lo <- if ("ec_low" %in% ecs) dt$ec_low else NA_real_; hi <- if ("ec_full" %in% ecs) dt$ec_full else if ("ec" %in% ecs) dt$ec else NA_real_
      dt[, ec_uscm := ifelse(!is.na(lo) & (is.na(hi) | hi < 1000), lo, hi)]
      dt[, spc_uscm := if ("ec_spc" %in% names(dt)) ec_spc else spc25(ec_uscm, temp_c %||% 25, cfg$ec_alpha)]
      if (!"temp_c" %in% names(dt)) dt[, spc_uscm := ec_uscm]
    }
    dt[, c(intersect(c("pres", "temp", "ec_low", "ec_full", "ec", "ec_spc", "unit_temp"), names(dt))) := NULL]
    kind <- if (s %in% baro_serials) "baro" else if (length(ecs) && all(is.na(dt$pres_kpa))) "ec" else "water"
    dt[, kind := kind]
    list(dt = dt, events = r$events, gaps = r$gaps, alt = rbindlist(al, fill = TRUE),
         lstats = data.table(serial = s, kind = kind, model = g$model[!is.na(g$model)][1] %||% NA_character_, files = nrow(g),
                             first = min(dt$TIMESTAMP), last = max(dt$TIMESTAMP), records = nrow(dt), read = r$n_read,
                             duplicates = r$duplicates, conflicts = r$conflicts, interval = fmt_step(r$step), years = paste(sort(unique(g$year)), collapse = ", ")),
         recon = list(read = r$n_read, removed = r$duplicates + r$conflicts + (r$n_read - r$duplicates - r$conflicts - nrow(dt)), kept = nrow(dt)))
  }
  for (s in sort(unique(use$serial))) {
    run_tick(match(s, sort(unique(use$serial))), uniqueN(use$serial), s)
    g <- use[serial == s][order(first_ts)]
    key3[[s]] <- k3 <- hash_of(list(CACHE_VERSION, STEP_HASH$stitch, BASE_OFFSET, cfg3, key_rows(g[, intersect(STITCH_KEY_COLS, names(g)), with = FALSE]), key_rows(cfgs$time[norm_serial(serial) == s]),
                                    s %in% baro_serials, s %in% switching, key_rows(if (s %in% switching) water_windows[serial == s])))
    x <- if (inc) cache_read(file.path(cdir, "stitch"), s, k3) else NULL
    if (!is.null(x)) {
      n3_hit <- n3_hit + 1L; x$dt <- alloc.col(x$dt); replay(x)
      READ_CACHE$used <- c(READ_CACHE$used, cache_file(READ_CACHE$dir, paste(g$md5, g$format, sep = "_"), READ_CACHE$key))   # its files as read stay in the cache
    } else {
      n0 <- length(AUDIT$log); p0 <- length(AUDIT$problems)
      x <- stitch_one(s, g)
      if (!is.null(x) && inc) cache_write(file.path(cdir, "stitch"), s, k3, c(x, list(log = AUDIT$log[seq_along(AUDIT$log) > n0], problems = AUDIT$problems[seq_along(AUDIT$problems) > p0])))
    }
    if (inc) c_used <- c(c_used, cache_file(file.path(cdir, "stitch"), s, k3))
    if (!is.null(x)) {
      recs[[s]] <- x$dt; events[[s]] <- x$events
      if (!is.null(x$gaps)) gaps[[s]] <- x$gaps
      if (nrow(x$alt)) alt_log[[length(alt_log) + 1L]] <- x$alt
      lstats[[s]] <- x$lstats; recon[[s]] <- x$recon
    }
    audit_flush(paste0("logger_", s))
  }
  loggers <- rbindlist(lstats, fill = TRUE)
  alt_log <- rbindlist(alt_log, fill = TRUE)
  if (nrow(alt_log)) say(sprintf("  Solinst altitude in %d file(s) of %d logger(s): %s", nrow(alt_log), uniqueN(alt_log$serial),
                                 if (all(alt_log$applied)) "taken off the readings (altitude/826 m of water)" else "kept as stored (setting solinst_altitude = keep)"), col = "fg4")
  say(sprintf("  %d loggers (%d water, %d conductivity, %d baro)", nrow(loggers), sum(loggers$kind == "water"), sum(loggers$kind == "ec"), sum(loggers$kind == "baro")), col = "fg4")

  # ---- 4. barologgers -------------------------------------------------------------------------------------------
  say("[4/7] barologgers", col = "aqua")
  bser <- intersect(names(recs), c(baro_serials, switching))
  bst <- copy(cfgs$baro_stations)[nzchar(serial)][, serial := canon(norm_serial(serial))]
  new_b <- setdiff(bser, bst$serial)
  if (length(new_b)) { bst <- rbind(bst, default_baro_stations(new_b, if (nrow(md)) md[!(serial %in% switching & type != "Baro")] else md), fill = TRUE); write_cfg(bst, cfg$project_dir, "baro_stations")
    say(sprintf("  %d barologger(s) added to baro stations; change them on the Barometric page", length(new_b)), col = "yellow") }
  cleaned <- list(); baro_kept <- list()
  for (s in bser) {
    b <- copy(recs[[s]]); if (s %in% switching) b <- b[in_baro_window(s, TIMESTAMP)]
    if (!nrow(b)) { baro_kept[[s]] <- 0L; next }                                              # a redeployed logger with no records in its Baro years
    b[, site := bst$station[match(s, bst$serial)]]
    b <- apply_limits(b, cfgs$qc, "baro"); b <- apply_manual(b, cfgs$manual, cfgs$oow, "baro")
    b <- period_step(b, cfgs$period_rows, c(s, b$site[1]), flag = add_flag, where = list(site = b$site[1], logger_id = s), only = c("pres_kpa", "temp_c"))   # core periods.R
    fwrite(b, file.path(out, "baro", paste0(safe_name(paste(b$site[1], s, sep = "__")), ".csv")), dateTimeAs = "write.csv")
    cleaned[[s]] <- b[!is.na(pres_kpa), .(TIMESTAMP, baro_kpa = pres_kpa)]
    recon[[s]]$kept_final <- nrow(b); baro_kept[[s]] <- nrow(b); contrib[[paste0("baro_", s)]] <- b[, .(records = .N), by = source_file][, used_in := paste("baro station", b$site[1])]
    audit_flush(paste0("logger_", s))
  }
  # barologger checks (config/baro_corrections.csv): offset and drift against a chosen reference, applied only when asked
  bchk <- run_baro_checks(cfgs$baro_corrections, cleaned, bst, canon, file.path(out, "reports"), alt_log, cfg)
  cleaned <- bchk$cleaned
  # stations: every serial at one place stitched into one series, across years and instruments
  baros <- list(); seams <- list()
  for (stn in unique(bst[serial %in% names(cleaned), station])) {
    r <- stitch_baro_station(stn, bst[station == stn & serial %in% names(cleaned), serial], cleaned)
    if (is.null(r$series)) next
    baros[[stn]] <- r$series; if (nrow(r$seams)) seams[[stn]] <- r$seams
    fwrite(r$series, file.path(out, "baro", paste0("station__", safe_name(stn), ".csv")), dateTimeAs = "write.csv")
  }
  seams <- rbindlist(seams, fill = TRUE)
  if (nrow(seams)) for (i in which(abs(seams$difference_kpa) > 0.3)) problem(paste("baro station", seams$station[i]),
    sprintf("serials %s and %s differ by %.2f kPa where one replaced the other; check the Barometric page", seams$from_serial[i], seams$to_serial[i], seams$difference_kpa[i]))
  if (!length(baros)) problem("barologgers", "no barologger records were found (expected in a baro/<year> folder); water levels cannot be compensated")
  else say(sprintf("  %d station(s) from %d barologger(s)", length(baros), length(cleaned)), col = "fg4")
  bmeta <- rbindlist(lapply(names(baros), function(stn) { ss <- bst[station == stn, serial]
    r <- if (nrow(md)) md[(serial %in% ss | site == stn) & !is.na(easting)] else data.table()
    data.table(serial = stn, site = stn, easting = if (nrow(r)) r$easting[1] else NA_real_, northing = if (nrow(r)) r$northing[1] else NA_real_) }))
  # a baro choice can name a station or a single serial
  ext_baros <- read_external_baro(cfgs$external_baro)          # other barometric records, the last fill-ins (external_baro.csv)
  if (length(ext_baros)) say(sprintf("  %d other barometric record(s) as fill-ins: %s", length(ext_baros), paste(names(ext_baros), collapse = ", ")), col = "fg4")
  baro_src <- function(name) { if (!nzchar(name %||% "")) return(NULL); if (!is.null(baros[[name]])) return(baros[[name]]); if (!is.null(ext_baros[[name]])) return(ext_baros[[name]])
    k <- canon(norm_serial(name)); if (nzchar(k) && !is.null(cleaned[[k]])) cleaned[[k]][, baro_serial := k] else NULL }
  agreement <- function(b) {                    # baro_agreement() for a set of stations, once per run and kept in cache/agree
    k <- hash_of(list(STEP_HASH$agree, names(b), vapply(names(b), function(n) { h <- paste0("agree:", n); if (is.null(series_hash[[h]])) series_hash[[h]] <- hash_of(b[[n]]); series_hash[[h]] }, "")))
    if (!is.null(series_hash[[k]])) return(series_hash[[k]])
    a <- if (inc) cache_read(file.path(cdir, "agree"), "agree", k) else NULL
    if (is.null(a)) { a <- baro_agreement(b); if (inc) cache_write(file.path(cdir, "agree"), "agree", k, a) }
    if (inc) c_used <<- c(c_used, cache_file(file.path(cdir, "agree"), "agree", k))
    series_hash[[k]] <- a; a }
  agree <- agreement(baros)

  # ---- 5. deployments and baro groups --------------------------------------------------------------------------------
  say("[5/7] deployments and baro groups", col = "aqua")
  deps <- build_deployments(md, cfgs$deployments)
  water <- setdiff(names(recs), baro_serials)
  if (nrow(deps)) deps <- deps[!serial %in% baro_serials & !(serial %in% switching & type == "Baro")]
  # loggers with records but no deployment in the workbook: one deployment per folder year, site unknown until set by hand
  for (s in setdiff(water, deps$serial)) {
    yrs <- sort(unique(use[serial == s, year])); yrs <- yrs[!is.na(yrs)]; if (!length(yrs)) yrs <- as.integer(format(max(recs[[s]]$TIMESTAMP), "%Y"))
    ttl <- use[serial == s & !is.na(title), title][1] %||% ""
    if (has_meta) problem(paste("logger", s), sprintf("not in the metadata workbook or the deployment sheet; processed as '%s' until a site is given (Loggers and metadata > Deployment sheet)", paste("Unlisted", s)))
    deps <- rbind(deps, data.table(serial = s, site = paste("Unlisted", s), year = yrs, type = "WLR", visits = 0L, tst_from_toc = FALSE,
                                   start_source = "first record", end_source = "last record", T_ST_source = "", alias = ttl,
                                   deployment = paste(s, safe_name(paste("Unlisted", s)), yrs, sep = "__"), added_by_hand = FALSE), fill = TRUE)
  }
  if (!nrow(deps)) stop(if (length(water)) "No deployments: the workbook lists none of the loggers found." else
    paste0("No water logger in the raw folders holds readings (", paste(man[kind == "water", paste0(rel, ": ", fifelse(status == "ok", "no readings", gsub("_", " ", status)))], collapse = "; "), "); nothing to compensate"))
  # deployment windows that the workbook leaves open come from the records in that year's folder
  for (i in which(deps$serial %in% water & (is.na(deps$start) | is.na(deps$end)))) {
    f <- use[serial == deps$serial[i] & year == deps$year[i]]
    if (!nrow(f)) next
    if (is.na(deps$start[i])) set(deps, i, "start", suppressWarnings(min(f$first_ts, na.rm = TRUE)))
    if (is.na(deps$end[i])) set(deps, i, "end", suppressWarnings(max(f$last_ts, na.rm = TRUE)))
  }
  # a deployment has data when its window overlaps the logger's record
  rng <- loggers[, .(serial, first, last)]
  deps[, has_data := serial %in% water & {
    k <- match(serial, rng$serial); (is.na(start) | start <= rng$last[k]) & (is.na(end) | end >= rng$first[k]) & !(is.na(start) & is.na(end)) } %in% TRUE]
  years <- sort(unique(deps[has_data == TRUE, year])); if (!is.null(cfg$years)) years <- intersect(years, cfg$years)
  gps <- exclude_dgps(read_dgps(cfg$dgps_dir), cfgs$dgps_exclude)
  sites <- unique(deps[, .(site, alias, easting, northing)], by = "site")
  links <- suggest_gps_points(sites, gps, cfgs$gps_points[nzchar(site) & nzchar(gps_name)])
  deps <- datum_for(deps, gps, links, cfgs$datum, cfg$dgps_pick %||% "nearest"); dpts <- attr(deps, "points")
  interp <- identical(cfg$dgps_pick, "interpolate") && !is.null(dpts) && nrow(dpts) > 0
  ghist <- gps_history(gps, links)
  deps <- apply_sensor_offsets(deps, loggers$model[match(deps$serial, loggers$serial)], cfgs$offsets, cfg)

  groups <- copy(cfgs$baro_groups); members <- copy(cfgs$baro_members); sugg_all <- list(); changed <- FALSE
  for (y in years) {
    dy <- deps[year == y & has_data == TRUE]
    avail <- names(baros)[vapply(baros, function(b) any(b$TIMESTAMP >= min(dy$start, na.rm = TRUE) & b$TIMESTAMP <= max(dy$end, na.rm = TRUE)), logical(1))]
    sg <- suggest_baro(y, dy, baros[avail], bmeta, agreement)
    if (nrow(sg$scores)) sugg_all[[as.character(y)]] <- sg$scores[, year := y]
    if (!nrow(groups[year == as.character(y)]) && nrow(sg$groups)) {
      groups <- rbind(groups, sg$groups[, lapply(.SD, as.character)], fill = TRUE); members <- rbind(members, sg$members[, lapply(.SD, as.character)], fill = TRUE)
      changed <- TRUE; say(sprintf("  %d: suggested %d baro group(s); change them on the Barometric page", y, nrow(sg$groups)), col = "yellow")
    }
    gy <- groups[year == as.character(y)]
    miss <- dy[!site %in% members[year == as.character(y), site]]
    if (nrow(miss) && nrow(gy)) {
      sm <- sg$members[site %in% miss$site]
      add <- data.table(year = as.character(y), group = ifelse(sm$group %in% gy$group, sm$group, gy$group[1]), site = sm$site, serial = sm$serial)
      left <- miss[!site %in% add$site]
      if (nrow(left)) add <- rbind(add, data.table(year = as.character(y), group = gy$group[1], site = left$site, serial = left$serial))
      members <- rbind(members, add, fill = TRUE); changed <- TRUE
      problem(paste("baro groups", y), paste0(nrow(add), " site(s) were not in a group and were added: ", paste(unique(add$site), collapse = ", ")))
    }
  }
  if (changed) { write_cfg(groups, cfg$project_dir, "baro_groups"); write_cfg(unique(members, by = c("year", "site", "serial")), cfg$project_dir, "baro_members") }
  group_of <- function(y, site) { st <- site; members[year == as.character(y) & members$site == st, group][1] }
  # The chain for one site in one processing year: primary, secondary, then (if baro_fill) every other station, best agreement first.
  baro_chain <- function(y, site, dep_year, quiet = FALSE) {
    g <- group_of(y, site); if (is.na(g)) { g <- group_of(dep_year, site); y <- dep_year }
    r <- groups[year == as.character(y) & group == g]
    p <- if (nrow(r)) r$primary[1] else ""; s2 <- if (nrow(r)) r$secondary[1] else ""
    if (!quiet && nzchar(p) && is.null(baro_src(p))) problem(paste("baro groups", y), sprintf("primary barologger %s of group %s has no records", p, g))
    ch <- list(list(name = p, role = "primary", series = baro_src(p)), list(name = s2, role = "secondary", series = baro_src(s2)))
    if (isTRUE(cfg$baro_fill)) { others <- setdiff(agree[order(departure_kpa)]$serial, c(p, s2))
      ch <- c(ch, lapply(others, function(o) list(name = o, role = "fill", series = baros[[o]])),
              lapply(names(ext_baros), function(o) list(name = o, role = "fill", series = ext_baros[[o]]))) }
    list(group = g %||% NA_character_, chain = ch, p = p, s = s2, ref = ch[[1]]$series %||% ch[[2]]$series)
  }

  # ---- 6. each deployment -----------------------------------------------------------------------------------------
  # One water logger at a time, with all its deployments. A logger whose stitched record, deployments, workbook rows,
  # the set-up rows that apply to it (quality rules, manual removals, out-of-water periods, drift rows, manual
  # measurements), the settings and the barometric records it is compensated with are the same as in the last run is
  # read back from the cache (cache/logger): its site files, summaries, flags and audit lines as they were.
  say("[6/7] compensating and checking each deployment", col = "aqua")
  summ <- list(); checks <- list(); drift_log <- list(); tape_log <- list(); written <- list(); air <- list(); dly <- list(); meas_cmp <- list(); meas <- read_measurements(cfgs$measurements)
  unc <- list(); bef <- list()
  key6 <- list(); n6_hit <- 0L; n6 <- 0L; site_file <- function(dep) file.path(out, "sites", paste0(safe_name(dep), ".csv"))
  cfg6 <- key_cfg(cfg, c("raw_dirs", "metadata_dir", "dgps_dir", "project_dir", "incremental", "ask_unzoned", "years", "unzoned_offset_h", "precip_file"))
  # a barometric series by name, hashed as a whole (fill-ins, and the reference they are shifted onto) or over one season
  # (primary, secondary)
  bh_all <- function(name) { k <- paste0("all:", name); if (is.null(series_hash[[k]])) { x <- baro_src(name); series_hash[[k]] <- if (is.null(x)) "" else hash_of(x) }; series_hash[[k]] }
  bh_part <- function(name, x, y) { k <- paste0(y, ":", name); if (!is.null(series_hash[[k]])) return(series_hash[[k]])     # over season year y
    series_hash[[k]] <- if (is.null(x) || !nrow(x)) "" else {
      gap <- max(cfg$baro_max_gap_h * 3600, 1.5 * step_of(x$TIMESTAMP), na.rm = TRUE); tt <- as.numeric(x$TIMESTAMP)
      t0 <- as.numeric(as.POSIXct(paste0(y - 1L, "-09-15 12:00:00"), tz = "UTC")); t1 <- as.numeric(as.POSIXct(paste0(y, "-09-15 12:00:00"), tz = "UTC"))
      i <- max(1L, findInterval(t0 - gap, tt)); j <- min(length(tt), findInterval(t1 + gap, tt) + 1L)
      hash_of(list(gap, if (j >= i) x[i:j] else x[0])) }
    series_hash[[k]] }
  # the deployments of a logger, which deployment each record belongs to, and the barologgers each season of it uses
  owner_of <- function(dt, ds) { owner <- rep(NA_integer_, nrow(dt))
    for (i in seq_len(nrow(ds))) { last <- ds$end_source[i] %in% "last record"                     # an end taken from the last record holds that record
      k <- is.na(owner) & (is.na(ds$start[i]) | dt$TIMESTAMP >= ds$start[i]) & (is.na(ds$end[i]) | dt$TIMESTAMP < ds$end[i] | (last & dt$TIMESTAMP == ds$end[i])); owner[k] <- i }
    owner }
  chains_of <- function(dt, ds, owner) {
    if (!nrow(dt) || !nrow(ds) || dt$kind[1] != "water") return(list())
    out <- list()
    for (i in seq_len(nrow(ds))) { t <- dt$TIMESTAMP[owner %in% i]; if (!length(t)) next; sy <- season_year(t)
      for (y in unique(sy)) { b <- baro_chain(y, ds$site[i], ds$year[i], quiet = TRUE)
        out[[length(out) + 1L]] <- list(dep = i, y = y, group = b$group, names = vapply(b$chain, `[[`, "", "name"), roles = vapply(b$chain, `[[`, "", "role"),
                                        p = bh_part(b$p, b$chain[[1]]$series, y), s = bh_part(b$s, b$chain[[2]]$series, y)) } }
    out }
  water_one <- function(s, dt, ds, owner) {
    R <- list(deps = list(), fills = character(), plots = list())
    if (!nrow(dt)) { R$recon <- list(kept_final = baro_kept[[s]] %||% 0L, outside = 0L); return(R) }
    if (!nrow(ds)) { R$recon <- list(kept_final = baro_kept[[s]] %||% 0L, outside = nrow(dt))
      problem(paste("logger", s), sprintf("all %s records (%s to %s) fall outside every deployment in the workbook and were left out; add a deployment on the Deployment history page",
                                          num(nrow(dt)), format(min(dt$TIMESTAMP), "%Y-%m-%d"), format(max(dt$TIMESTAMP), "%Y-%m-%d")))
      dt <- remove_rows(dt, seq_len(nrow(dt)), "deploy", "outside_deployment", "no deployment of this logger in the years processed", detail = "summary")
      return(R) }
    nout <- sum(is.na(owner))
    if (nout > 48) { tt <- dt$TIMESTAMP[is.na(owner)]
      problem(paste("logger", s), sprintf("%s records (%s to %s) fall outside every deployment in the workbook and were left out; add a deployment on the Deployment history page if the logger was in the water",
                                          num(nout), format(min(tt), "%Y-%m-%d"), format(max(tt), "%Y-%m-%d"))) }
    if (nout) { dt[, .own := owner]; dt <- remove_rows(dt, which(is.na(owner)), "deploy", "outside_deployment",
                                                      "before the logger went in or after it came out (deployment dates)", detail = "summary")
                owner <- dt$.own; dt[, .own := NULL] }
    kept <- 0L
    for (i in seq_len(nrow(ds))) {
      dep <- ds[i]; d <- dt[owner == i]
      if (!nrow(d)) { problem(dep$deployment, "no records inside the deployment dates"); next }
      d[, `:=`(site = dep$site, deployment = dep$deployment, year = dep$year)]
      kind <- d$kind[1]
      d <- apply_limits(d, cfgs$qc[!parameter %in% c("depth_m", "elev_m")], kind)
      d <- apply_manual(d, cfgs$manual, cfgs$oow, kind)
      pstep <- function(d, only) { keys <- c(s, dep$site, dep$deployment); was <- lapply(stats::setNames(nm = intersect(only, names(d))), function(p) is.na(d[[p]]))
        d <- period_step(d, cfgs$period_rows, keys, flag = add_flag, where = list(site = dep$site, logger_id = s), only = only)
        for (p in names(was)) { k <- which(was[[p]] & !is.na(d[[p]]))                  # filled by interpolation (the one core action that fills): flagged
          if (length(k)) add_flag(d, p, k, "period_interpolate", paste("filled by linear interpolation, a row of", PERIOD_FILE)) }
        wlr_set_values(d, cfgs$period_rows, keys, only, add_flag, where = list(site = dep$site, logger_id = s)) }
      d <- pstep(d, c("pres_kpa", "temp_c", "spc_uscm"))                               # corrections over a period on what the logger measured
      used <- list(); nbr <- 0
      if (kind == "water") {
        sy <- season_year(d$TIMESTAMP)
        for (y in unique(sy)) { k <- which(sy == y); b <- baro_chain(y, dep$site, dep$year); used[[as.character(y)]] <- b
          r <- compensate_chain(d$TIMESTAMP[k], b$chain, cfg, b$ref)
          for (v in names(r)) set(d, k, v, r[[v]]); nbr <- nbr + attr(r, "bridged")
          f_used <- unique(r$baro_name[r$baro_source == "fill"])            # fill-ins used, and every fill-in ranked before them
          if (length(f_used)) { nm <- vapply(b$chain, `[[`, "", "name"); last <- max(match(f_used, nm), na.rm = TRUE)
            R$fills <- setdiff(unique(c(R$fills, nm[seq_len(last)], if (!is.null(b$ref)) nm[if (!is.null(b$chain[[1]]$series)) 1L else 2L])), "") } }
        d[, dp_kpa := pres_kpa - baro_kpa]
        d[, depth_m := kpa_to_m(dp_kpa, if ("temp_c" %in% names(d)) temp_c else NULL, cfg)]
        d <- pstep(d, c("baro_kpa", "dp_kpa", "depth_m"))                                 # ... on the depth, so the elevations below follow it
        d[, dtw_m := if (!is.na(dep$T_ST)) dep$T_ST - dep$offset_m - depth_m else NA_real_]
        zr <- if (interp && nzchar(dep$z_ref_code %||% "")) ref_series(dpts[site == dep$site & code == dep$z_ref_code], d$TIMESTAMP, dep$z_ref) else dep$z_ref   # follows the surveys in time
        d[, elev_m := if (!is.na(dep$T_ST) && !is.na(dep$z_ref)) zr - dep$T_ST + dep$offset_m + depth_m else NA_real_]
        gs <- if (!is.null(dpts) && nrow(dpts)) dpts[site == dep$site & code == "GS"] else NULL
        zg <- if (!is.null(gs) && nrow(gs)) { if (interp) ref_series(gs, d$TIMESTAMP) else rep(dep$z_ground, nrow(d)) } else NULL
        wg <- wt_ground_of(d, dep, zg); if (any(is.finite(wg))) d[, wt_ground_m := wg]                 # water table above (+) or below the ground
        d <- apply_limits(d, cfgs$qc[parameter %in% c("depth_m", "elev_m")], kind)
        d <- apply_manual(d, cfgs$manual, cfgs$oow, kind)
        d <- pstep(d, c("dtw_m", "elev_m", "wt_ground_m"))                                # ... and on what is worked out from it
      }
      X <- list()
      ar <- if (kind == "water") detect_air_runs(d, cfg) else data.table()
      if (nrow(ar)) { X$air <- cbind(data.table(deployment = dep$deployment, site = dep$site, serial = s, year = dep$year), ar,
                                     action = if (isTRUE(cfg$auto_trim_air)) "dropped" else "flagged")
        for (j in seq_len(nrow(ar))) { k <- which(d$TIMESTAMP >= ar$from[j] & d$TIMESTAMP <= ar$to[j])
          if (isTRUE(cfg$auto_trim_air)) d <- remove_rows(d, k, "qc", "launched_in_air", sprintf("logger in the air at the %s of the deployment", ar$where[j]), detail = "summary")
          else add_flag(d, "depth_m", k, "in_air", sprintf("logger probably in the air at the %s of the deployment", ar$where[j])) } }
      if (kind == "water" && "depth_m" %in% names(d)) {
        k <- which(d$depth_m < -cfg$below_air_m)
        if (length(k)) add_flag(d, "depth_m", k, "below_air", "the logger reads below the air pressure: a failing or frozen sensor, or a wrong unit")
        if (length(k) > cfg$below_air_share * nrow(d)) problem(dep$deployment, sprintf("%s records (%.0f%%, %s to %s) read below the air pressure, down to %.2f m; the sensor is probably failing or froze, check it before using this season",
          format(length(k), big.mark = ","), 100 * length(k) / nrow(d), format(min(d$TIMESTAMP[k]), "%Y-%m-%d"), format(max(d$TIMESTAMP[k]), "%Y-%m-%d"), min(d$depth_m[k]))) }
      vis <- if (nrow(md)) md[serial == s & year == dep$year & site == dep$site] else data.table()
      d <- apply_auto_flags(d, if (nrow(vis)) vis$visit_time else as.POSIXct(character(), tz = "UTC"), events[[s]] %||% empty_events(), cfg)
      bsum <- function(f) paste(unique(vapply(used, f, "")), collapse = " | ")
      chk <- if (kind == "water" && nrow(vis)) manual_check(dep, vis, d, dep$offset_m, cfg$check_window_h) else NULL
      X$chk <- chk
      X$tape <- tape_offset_check(dep, chk, cfgs$drift, cfg)                     # warns on a large tape offset; changes nothing
      if (kind == "water") {                                                          # extras: a failure here is a problem line, never a stopped run
        X$unc <- tryCatch(deployment_uncertainty(dep, d, chk, loggers$model[match(s, loggers$serial)], cfgs$accuracy, cfg), error = function(e) { problem(dep$deployment, paste("uncertainty not worked out:", conditionMessage(e))); NULL })
        b <- tryCatch(barometric_efficiency(d, cfg, be_flag = cfg$be_flag %||% 0.1, min_r2 = cfg$be_min_r2 %||% 0.2), error = function(e) { problem(dep$deployment, paste("barometric efficiency not worked out:", conditionMessage(e))); NULL })
        if (!is.null(b)) X$be <- cbind(data.table(deployment = dep$deployment, site = dep$site, serial = s, year = dep$year), b) }
      di <- apply_drift(d, dep, chk, cfgs$drift, cfg); if (di$mode != "off" || nzchar(di$how)) X$drift <- di          # off by default: adds nothing unless asked
      if (isTRUE(di$applied)) audit_add("correct", "drift_correction", "summary", n = di$rows_corrected, site = dep$site, logger_id = s, reason = di$how)
      fwrite(d, site_file(dep$deployment), dateTimeAs = "write.csv"); kept <- kept + nrow(d)
      if (kind == "water") { X$dly <- daily_of(d, cfg$min_complete); X$meas <- compare_measurements(meas, d, s, cfg$meas_window_h) }
      X$contrib <- d[, .(records = .N), by = source_file][, used_in := dep$deployment]
      ok_d <- if ("depth_m" %in% names(d)) d$depth_m[!is.na(d$depth_m)] else numeric()
      X$summ <- data.table(deployment = dep$deployment, site = dep$site, serial = s, year = dep$year, kind = kind,
        start = min(d$TIMESTAMP), end = max(d$TIMESTAMP), records = nrow(d), baro_group = bsum(function(b) b$group %||% ""), primary = bsum(function(b) b$p), secondary = bsum(function(b) b$s),
        baro_primary_share = if ("baro_source" %in% names(d)) round(mean(d$baro_source == "primary"), 3) else NA_real_,
        baro_missing = if ("baro_source" %in% names(d)) sum(d$baro_source == "" & !is.na(d$pres_kpa)) else NA_integer_,
        baro_filled = if ("baro_source" %in% names(d)) sum(d$baro_source == "fill") else NA_integer_, baro_bridged = nbr,
        depth_min = if (length(ok_d)) round(min(ok_d), 3) else NA_real_, depth_median = if (length(ok_d)) round(stats::median(ok_d), 3) else NA_real_,
        depth_max = if (length(ok_d)) round(max(ok_d), 3) else NA_real_, below_air_share = if ("depth_m" %in% names(d)) round(mean(d$depth_m < -cfg$below_air_m, na.rm = TRUE), 3) else NA_real_, T_ST = dep$T_ST, T_ST_source = dep$T_ST_source, reference = dep$ref_code,
        z_ref = dep$z_ref, measured_to = dep$measured_to, offset_m = dep$offset_m, offset_source = dep$offset_source, tape_diff_median = if (!is.null(chk)) round(stats::median(chk$difference_m, na.rm = TRUE), 3) else NA_real_,
        values = sum(vapply(intersect(EXPORT_VALUES, names(d)), function(v) sum(!is.na(d[[v]])), 0)))      # values in the output (corrections applied)
      if (isTRUE(cfg$make_plots)) {
        pp <- intersect(c("depth_m", "elev_m", "baro_kpa", "temp_c", "spc_uscm"), names(d)); pp <- pp[vapply(pp, function(p) any(!is.na(d[[p]])), logical(1))]
        if (!length(pp)) pp <- intersect(c("pres_kpa", "temp_c"), names(d))
        pf <- file.path(out, "plots", paste0(safe_name(dep$deployment), ".png"))
        pc <- if (inc) cache_file(file.path(cdir, "plots"), dep$deployment, hash_of(list(STEP_HASH$plots, d, pp, dep$site, dep$year, s, vis$visit_time, chk, cfg$plot_max_points))) else ""
        pc <- sub("\\.rds$", ".png", pc)
        if (inc && file.exists(pc)) { file.copy(pc, pf, overwrite = TRUE); p_hit <<- p_hit + 1L }
        else { g <- plot_logger(d, pp, title = paste(dep$site, dep$year), subtitle = paste("serial", s), visits = vis$visit_time[!is.na(vis$visit_time)], tape = chk, max_points = cfg$plot_max_points)
          if (!is.null(g)) { nord_save(g, pf, 11, 2 + 2.2 * length(pp)); if (inc && file.exists(pf)) file.copy(pf, pc, overwrite = TRUE) } }
        if (inc) { p_used <<- c(p_used, pc); if (file.exists(pc)) X$plot <- basename(pc) }
      }
      R$deps[[dep$deployment]] <- X
    }
    R$recon <- list(kept_final = kept + (baro_kept[[s]] %||% 0L), outside = nout)
    R
  }
  for (s in water) {
    run_tick(match(s, water), length(water), s)
    dt <- recs[[s]]; if (s %in% switching) dt <- dt[!in_baro_window(s, TIMESTAMP)]          # its barologger years went to step 4
    ds <- deps[serial == s & year %in% years & has_data == TRUE][order(start)]
    owner <- owner_of(dt, ds); sites_s <- unique(ds$site); kind_s <- if (nrow(dt)) dt$kind[1] else ""
    hit_rows <- function(tab) if (is.null(tab) || !nrow(tab)) tab else tab[wlr_row_hits(tab, s, "", c(sites_s, ds$deployment)), setdiff(names(tab), "row"), with = FALSE]   # R/qc.R
    key6[[s]] <- k6 <- hash_of(list(CACHE_VERSION, STEP_HASH$deploy, BASE_OFFSET, cfg6, key3[[s]], key_rows(if (s %in% switching) water_windows[serial == s]), key_rows(ds),
      key_rows(if (nrow(md)) md[serial == s]), key_rows(cfgs$qc[vapply(applies_to, function(a) any(vapply(c(sites_s, ""), function(st) applies(a, kind_s, s, st), NA)), NA)]),
      key_rows(hit_rows(cfgs$manual)), key_rows(period_for(cfgs$period_rows, c(s, sites_s, ds$deployment, bser, bst$station))), key_rows(hit_rows(cfgs$oow)), key_rows(hit_rows(cfgs$drift)), key_rows(if (nrow(meas)) meas[site %in% sites_s]), baro_kept[[s]], chains_of(dt, ds, owner),
      key_rows(if (!is.null(dpts) && nrow(dpts)) dpts[site %in% sites_s]), key_rows(cfgs$accuracy), loggers$model[match(s, loggers$serial)]))
    cf <- function(dep) sub("\\.rds$", paste0("__", safe_name(dep), ".csv"), cache_file(file.path(cdir, "logger"), s, k6))
    x <- if (inc) cache_read(file.path(cdir, "logger"), s, k6) else NULL
    if (!is.null(x)) { ok <- all(file.exists(vapply(names(x$deps), cf, ""))) && all(vapply(x$deps, function(X) is.null(X$plot) || file.exists(file.path(cdir, "plots", X$plot)), NA)) &&
                             all(vapply(names(x$fills), function(n) identical(bh_all(n), x$fills[[n]]), NA))
      if (!ok) x <- NULL }
    n6 <- n6 + 1L
    if (!is.null(x)) {
      n6_hit <- n6_hit + 1L; replay(x)
      for (dep in names(x$deps)) { X <- x$deps[[dep]]; file.copy(cf(dep), site_file(dep), overwrite = TRUE)
        if (!is.null(X$plot)) { pc <- file.path(cdir, "plots", X$plot); file.copy(pc, file.path(out, "plots", paste0(safe_name(dep), ".png")), overwrite = TRUE)
                                p_hit <- p_hit + 1L; p_used <- c(p_used, pc) } }
    } else {
      n0 <- length(AUDIT$log); f0 <- length(FLAGS$list); p0 <- length(AUDIT$problems)
      x <- water_one(s, dt, ds, owner)
      x$fills <- vapply(stats::setNames(x$fills, x$fills), bh_all, "")
      if (inc) { for (dep in names(x$deps)) file.copy(site_file(dep), cf(dep), overwrite = TRUE)
        cache_write(file.path(cdir, "logger"), s, k6, c(x, list(log = AUDIT$log[seq_along(AUDIT$log) > n0], flags = FLAGS$list[seq_along(FLAGS$list) > f0],
                                                                problems = AUDIT$problems[seq_along(AUDIT$problems) > p0]))) }
    }
    if (inc) c_used <- c(c_used, cache_file(file.path(cdir, "logger"), s, k6), vapply(names(x$deps), cf, ""))
    recon[[s]]$kept_final <- x$recon$kept_final; recon[[s]]$outside <- x$recon$outside
    for (dep in names(x$deps)) { X <- x$deps[[dep]]
      written[[dep]] <- site_file(dep); summ[[dep]] <- X$summ; contrib[[dep]] <- X$contrib
      if (!is.null(X$chk)) checks[[dep]] <- X$chk
      if (!is.null(X$tape)) tape_log[[dep]] <- X$tape
      if (!is.null(X$drift)) drift_log[[dep]] <- X$drift
      if (!is.null(X$air)) air[[dep]] <- X$air
      if (!is.null(X$dly)) dly[[dep]] <- X$dly
      if (!is.null(X$unc)) unc[[dep]] <- X$unc
      if (!is.null(X$be)) bef[[dep]] <- X$be
      if (!is.null(X$meas)) meas_cmp[[dep]] <- X$meas }
    audit_flush(paste0("logger_", s))
  }
  if (n6_hit) say(sprintf("  %d of %d water loggers unchanged, reused", n6_hit, n6), col = "fg4")

  # ---- 7. outputs and reconciliation -------------------------------------------------------------------------------
  say("[7/7] writing outputs", col = "aqua")
  if (inc) say(sprintf("  reused from the last run: %d of %d loggers as stitched, %d of %d water loggers as compensated, %d of %d files as read, %d of %d plots",
                       n3_hit, length(key3), n6_hit, n6, READ_CACHE$hits, length(unique(READ_CACHE$used)), p_hit, length(p_used)), col = "fg4")
  adir <- file.path(out, "audit")
  idx <- if (length(AUDIT$index)) rbindlist(AUDIT$index, fill = TRUE) else data.table()
  rowrm <- if (nrow(idx)) idx[level == "row", .(n = sum(n)), by = .(logger_id, stage)] else data.table(logger_id = character(), stage = character(), n = integer())
  rc <- rbindlist(lapply(names(recon), function(s) { r <- recon[[s]]
    logged <- sum(rowrm[logger_id == s, n])
    data.table(logger_id = s, read = r$read, written = r$kept_final %||% NA_integer_, removed_logged = logged,
               ok = !is.null(r$kept_final) && r$read - logged == r$kept_final) }))
  fwrite(rc, file.path(adir, "reconciliation.csv"))
  fwrite(man[, !c("root"), with = FALSE], file.path(adir, "files.csv"), dateTimeAs = "write.csv")
  fwrite(loggers, file.path(adir, "loggers.csv"), dateTimeAs = "write.csv")
  fwrite(deps, file.path(adir, "deployments.csv"), dateTimeAs = "write.csv")
  if (nrow(md)) fwrite(md, file.path(adir, "metadata.csv"), dateTimeAs = "write.csv")
  fwrite(rbindlist(summ, fill = TRUE), file.path(adir, "summary.csv"), dateTimeAs = "write.csv")
  fwrite(rbindlist(checks, fill = TRUE), file.path(adir, "manual_check.csv"), dateTimeAs = "write.csv")
  tape_f <- file.path(adir, "tape_offsets.csv"); unlink(tape_f)
  if (length(tape_log)) { tl <- rbindlist(tape_log, fill = TRUE); fwrite(tl, tape_f, dateTimeAs = "write.csv"); nb <- sum(tl$status == "check the metadata")
    if (nb) problem("tape checks", sprintf("%d of %d deployments differ from the tape by more than %g m (median of their checks), more than drift usually explains; most likely a metadata error (hang length, logger offset, reference point or visit time). Review them on Field visits: adjust the metadata or keep them as they are",
      nb, nrow(tl), cfg$tape_warn_m %||% 0.05)) }
  if (length(drift_log)) fwrite(rbindlist(drift_log, fill = TRUE), file.path(adir, "drift_corrections.csv"), dateTimeAs = "write.csv")      # only when a drift check or correction was asked for
  sig <- if (length(unc)) rbindlist(unc, fill = TRUE) else data.table()
  sm <- summaries_of(rbindlist(dly), cfg$year_complete, sig, cfg$water_year_start); sdir <- file.path(out, "summaries"); dir.create(sdir, showWarnings = FALSE)
  fwrite(sm$monthly, file.path(sdir, "monthly.csv")); fwrite(sm$water_year, file.path(sdir, "water_year.csv"))
  # uncertainty, barometric response, casing movement and the derived hydrology (each a table of its own; nothing is changed)
  fwrite(if (nrow(sig)) sig else data.table(deployment = character(), site = character(), depth_sigma_m = numeric()), file.path(adir, "uncertainty.csv"))
  be <- if (length(bef)) rbindlist(bef, fill = TRUE) else data.table(deployment = character(), site = character(), be_lag0 = numeric(), verdict = character())
  fwrite(be, file.path(adir, "barometric_efficiency.csv"))
  if (nrow(be) && any(grepl("^lasting", be$verdict))) say("  ", sum(grepl("^lasting", be$verdict)), " deployment(s) keep answering the air pressure (barometric efficiency above ", cfg$be_flag %||% 0.1, "): see audit/barometric_efficiency.csv", col = "yellow")
  cm <- casing_movement(md, cfg$casing_tol_m %||% 0.02); fwrite(cm, file.path(adir, "casing_movement.csv"), dateTimeAs = "write.csv")
  for (i in which(cm$status != "steady")) problem(paste("casing", cm$site[i]), sprintf("the casing stands %+.0f cm higher above the ground at the %s visit than at the one before: %s; hang lengths from the casing and surveys of its top are out of date",
                                                                                     100 * cm$change_m[i], format(cm$visit_time[i], "%Y-%m-%d"), cm$status[i]))
  ad <- rbindlist(dly, fill = TRUE)
  fwrite(hydroperiod(ad, cfg$year_complete, cfg$water_year_start), file.path(sdir, "hydroperiod.csv"))
  fwrite(gradients(ad, cfgs$well_pairs, sig), file.path(sdir, "gradients.csv"))
  fwrite(wtf_recharge(ad, cfgs$specific_yield, cfg$water_year_start), file.path(sdir, "recharge.csv"))
  fwrite(if (length(meas_cmp)) rbindlist(meas_cmp, fill = TRUE) else data.table(site = character(), deployment = character(), serial = character(), time = character(), parameter = character(),
         manual = numeric(), logger = numeric(), difference = numeric(), method = character(), author = character(), notes = character()), file.path(adir, "measurements.csv"), dateTimeAs = "write.csv")
  fwrite(if (length(gaps)) rbindlist(gaps, fill = TRUE) else data.table(logger_id = character(), gap_start = character(), gap_end = character(), gap_hours = numeric(), missing_records = integer()),
         file.path(adir, "gaps.csv"), dateTimeAs = "write.csv")
  fwrite(rbindlist(sugg_all, fill = TRUE), file.path(adir, "baro_suggestions.csv"))
  fwrite(bchk$table, file.path(adir, "baro_checks.csv"), dateTimeAs = "write.csv")
  fwrite(if (nrow(alt_log)) alt_log else data.table(serial = character(), file = character(), altitude_m = numeric(), correction_kpa = numeric(),
         correction_m_water = numeric(), records = integer(), applied = logical()), file.path(adir, "solinst_altitude.csv"))
  fwrite(baro_compare(baros), file.path(adir, "baro_compare.csv"))
  fwrite(if (nrow(seams)) seams else data.table(station = character(), from_serial = character(), to_serial = character(), change_at = character(),
         overlap_days = numeric(), difference_kpa = numeric(), gap_hours = numeric()), file.path(adir, "baro_seams.csv"), dateTimeAs = "write.csv")
  fwrite(bst, file.path(adir, "baro_stations.csv"))
  fwrite(ghist, file.path(adir, "dgps_history.csv"), dateTimeAs = "write.csv")
  fwrite(if (length(air)) rbindlist(air, fill = TRUE) else data.table(deployment = character(), site = character(), serial = character(), year = integer(), where = character(),
         from = character(), to = character(), records = integer(), first_in_water = character(), action = character()), file.path(adir, "air_runs.csv"), dateTimeAs = "write.csv")
  # every raw file: what it is, how many readings it gave, how many reached the output, and what happened to the rest
  fa <- file_audit(man, rbindlist(contrib, fill = TRUE), audit_by_file())
  fwrite(fa, file.path(adir, "file_audit.csv"), dateTimeAs = "write.csv")
  file_reconcile_check(fa, "readings")
  # flags.csv before the corrected data: a flag decided not a problem (config/review_decisions.csv) is kept there, marked
  # accepted, and export_table() leaves it out of the flag columns (core config.R)
  flags_out <- review_accept(qc_flags_table(FLAGS$list, data.table(site = character(), logger_id = character(), parameter = character(), start = character(),
                             end = character(), records = integer(), check = character(), detail = character()), "WLR"), review_read(review_path(cfg$project_dir)), "site")   # + rule_id, qc_code, app, run_id, accepted
  fwrite(flags_out, file.path(adir, "flags.csv"))
  # corrected data: one file per site, every year and logger together, with flag columns
  # (a site whose loggers were all reused, with the same quality-rule parameters, is copied from cache/corrected)
  dir.create(file.path(out, "corrected"), showWarnings = FALSE); unlink(list.files(file.path(out, "corrected"), full.names = TRUE))
  sm <- rbindlist(summ, fill = TRUE); n7_hit <- 0L
  tested <- { qr <- cfg_path(cfg$project_dir, "qc"); rules <- if (file.exists(qr)) fread(qr, colClasses = "character", na.strings = "") else data.table(); has <- function(x) !is.na(x) & nzchar(x)
    if (nrow(rules)) sort(unique(rules[has(min) | has(max) | has(max_spike) | has(max_flat_hours), parameter])) else character() }      # as export_table() reads them
  for (st in unique(sm$site)) { r <- sm[site == st][order(start)]; f <- file.path(out, "corrected", paste0(safe_name(st), ".csv"))
    k7 <- hash_of(list(CACHE_VERSION, STEP_HASH$corrected, BASE_OFFSET, st, r$deployment, r$serial, unlist(key6[unique(r$serial)]), tested, "both",
                    flags_out[accepted & site == st, .(logger_id, parameter, start, end, check)]))
    cf <- sub("\\.rds$", ".csv", cache_file(file.path(cdir, "corrected"), st, k7))
    if (inc && file.exists(cf)) { file.copy(cf, f, overwrite = TRUE); n7_hit <- n7_hit + 1L }
    else { x <- rbindlist(lapply(seq_len(nrow(r)), function(i) export_table(fix_ts_cols(fread(written[[r$deployment[i]]], na.strings = c("", "NA"))), r$serial[i], cfg$project_dir, "both", !isFALSE(cfg$hide_accepted_flags))), fill = TRUE)   # both versions: the export picks one
           fwrite(x, f); if (inc) file.copy(f, cf, overwrite = TRUE) }
    if (inc) c_used <- c(c_used, cf) }
  if (inc) { cache_prune(file.path(cdir, "read"), READ_CACHE$used); cache_prune(file.path(cdir, "plots"), p_used)
             for (d in c("stitch", "logger", "corrected", "agree")) cache_prune(file.path(cdir, d), c_used) }
  fwrite(gps, file.path(adir, "dgps.csv"), dateTimeAs = "write.csv"); fwrite(links, file.path(adir, "gps_links.csv"))
  fwrite(near_duplicates(man), file.path(adir, "overlaps.csv"), dateTimeAs = "write.csv")
  fwrite(idx, file.path(adir, "audit_index.csv"))
  fwrite(if (length(AUDIT$problems)) rbindlist(AUDIT$problems) else data.table(where = character(), problem = character()), file.path(adir, "problems.csv"))
  counts <- list(files = nrow(man), files_used = nrow(use), loggers = nrow(loggers), deployments = length(summ), audit_entries = AUDIT$n,
                 problems = length(AUDIT$problems), reconciled = all(rc$ok),
                 files_reused = READ_CACHE$hits, plots_reused = p_hit, loggers_stitched_reused = n3_hit,
                 water_loggers = n6, water_loggers_reused = n6_hit, sites_corrected_reused = n7_hit)
  cnt <- list(base_zone = sum(man[use %in% TRUE & !is.na(zone) & zone != BASE_OFFSET, rows], na.rm = TRUE),      # readings whose time stamps were moved
              solinst_altitude = if (nrow(alt_log)) sum(alt_log[applied == TRUE, records]) else 0, baro_gaps = sum(sm$baro_bridged, na.rm = TRUE),
              sensor_offset = if (nrow(sm)) sum(sm[!is.na(T_ST) & !is.na(offset_m) & offset_m != 0, records]) else 0)
  try_or(applied_write(cfg, values = if (nrow(sm)) sum(sm$values, na.rm = TRUE) else NA, counts = cnt), where = "corrections applied")   # output/audit/corrections_applied.csv (core applied.R)
  write_run_manifest(cfg, t0, counts, utc = FALSE, packages = c("data.table", "ggplot2", "xml2", "readxl"),
    settings = list(raw_dirs = cfg$raw_dirs, metadata_file = mfile %||% NA, metadata_md5 = if (!is.na(mfile %||% NA)) file_md5(mfile) else "",
                    dgps_dir = cfg$dgps_dir, project_dir = cfg$project_dir, years = paste(cfg$years %||% "all", collapse = ", "),
                    base_offset_h = cfg$base_offset_h, solinst_kpa_per_m = cfg$solinst_kpa_per_m, density_mode = cfg$density_mode,
                    fixed_density = cfg$fixed_density, gravity = cfg$gravity, baro_max_gap_h = cfg$baro_max_gap_h,
                    service_window_h = cfg$service_window_h, out_of_water_m = cfg$out_of_water_m, ec_alpha = cfg$ec_alpha,
                    water_year_start = water_year_start_ok(cfg$water_year_start)))
  if (!all(rc$ok)) stop("Record counts do not reconcile with the audit log for: ", paste(rc[ok == FALSE, logger_id], collapse = ", "),
                        ". See output/audit/reconciliation.csv.")
  tryCatch(report_write(wlr_run_report_doc(cfg$project_dir), file.path(out, "report"), "html", NORD),
           error = function(e) say("  the run report could not be written: ", conditionMessage(e), col = "orange"))
  say(sprintf("Done in %.0f s: %d deployments written, %d audit entries, %d problems. Every record is accounted for.",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), length(summ), AUDIT$n, length(AUDIT$problems)), col = "green")
  invisible(list(files = man, loggers = loggers, deployments = deps, summary = rbindlist(summ, fill = TRUE)))
}

run_pipeline <- function(cfg = CFG, ...) { cfg$project_dir <- ovid_project_dir(cfg$project_dir); run_project(cfg, function() run_pipeline_steps(cfg, ...)) }   # folders checked, project locked, run logged (core manifest.R)   # every run leaves output/logs/<start>.log (core manifest.R)
STEP_HASH <- step_hashes(environment())          # after every function is defined, run_pipeline() included
warn_core_shadows(PIPELINE_DIR)                  # an app function that replaces a core one (core/cache.R)
if (RUN_ON_SOURCE && sys.nframe() == 0L) run_pipeline()
