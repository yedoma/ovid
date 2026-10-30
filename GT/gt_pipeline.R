#!/usr/bin/env Rscript
# =============================================================================
#  gt_pipeline.R  --  Ground temperature ingest / stitch / QC / summary pipeline   (Ovid Core // GT)
# =============================================================================
#  What it does
#    1. Reads the set-up tables (seeding sites and sensors from the registry workbook on the first run) and indexes the
#       metadata folder (workbooks and field notes).
#    2. Finds every logger file under the data folders, tells what each one is, drops byte-identical copies and groups
#       each download into a bundle (the logger's own file + its exports). The best readable file of each bundle is read;
#       a bundle with nothing readable is listed as "needs export".
#    3. Reads each file, moves its clock onto UTC-7, removes values with impossible times or from clock resets, and
#       stitches every download of a logger into one record (repeats kept once; newest download wins).
#    4. Gives every sensor a site and a depth (sensors.csv: set by hand, from the registry, from a template, or the
#       default steps), and drops values outside each sensor's deployment.
#    5. Applies the quality rules (limits, spikes, stuck sensors, exposed sensors, manual removals) and lists sensor health.
#    6. Builds one record per site, joining the loggers that replaced each other at a site (never different instrument
#       types), and writes soil moisture and conductivity separately.
#    7. Daily means, annual statistics (trumpet curves), thaw depth and active-layer thickness, freeze/thaw and zero curtain.
#    8. Writes the audit trail and stops if the value count of any logger does not reconcile.
#
#  The audit rule
#    Nothing is ever dropped except through remove_rows(), which writes to output/audit. Raw files are never modified.
#
#  Use
#    Rscript gt_pipeline.R            (or Source it in RStudio, or press Run in the app)
#    Folders: CFG below, or GT_RAW_DIRS (separated by ;), GT_METADATA_DIR, GT_PROJECT_DIR.
#
#  Needs: data.table, ggplot2, readxl, zip
# =============================================================================

for (.l in if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8", "UTF-8"))   # UTF-8 text ( deg C, uS/cm);
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break                               # macOS has no C.UTF-8

GT_VERSION <- "1.8.0"

OVID_APP <- "GT"                                          # its projects go in ~/Projects/Ovid/Projects/GT/ (core util.R)
CFG <- list(
  raw_dirs      = c("~/Projects/Ovid/Projects/GT/raw"),  # logger files, in any folder layout (type/year folders are fine)
  metadata_dir  = "",                                            # registry workbook, NTGS metadata workbooks, field notes (.docx); none unless set
  project_dir   = "",       # config/ and output/ live here

  base_offset_h = -7,                           # every time stamp is moved to UTC-7 (no daylight saving); set -6 to standardise every record to UTC-6
  unzoned_offset_h = NA,                        # the zone of files that state none; NA = the base zone (as before). If no file states one, the app asks
  # min_year, zero_offset_limit, surface_max_depth, fill_gap_days, fill_min_depth, fill_r2_min, gap_factor, spike_k, air_like_*
  # and pre_install_* are under Settings, Advanced; their depths are in metres below the ground in every project
  min_year      = 2000,                         # time stamps before this are a reset clock
  drop_stray_records = TRUE,                    # leave out readings with corrupt time stamps among good ones (off the time grid and out of order); each is audited
  correct_rbr_drift = FALSE,                    # spread the RBR clock error found at download (host vs logger time) over the record
  depth_positive = "down",                      # depths typed in the set-up: "down" = metres below the surface (negative above); "up" = above the surface positive, below negative
  assign_default_depths = TRUE,                 # sensors with no depth get default depths (Automatic corrections); off: their values are left out
  default_first_depth = 0.10,                   # sensors with no depth: first at 0.10 m ...
  default_step  = 0.10,                         # ... then every 0.10 m
  min_complete  = 0.8,                          # share of a day's readings needed for a daily mean
  year_complete = 0.9,                          # share of a year's days needed for a complete year
  water_year_start = 10,                        # water year starts in October
  zero_band     = 0.2,                          # zero curtain: daily mean within this of 0 C ...
  zero_curtain_days = 3,                        # ... for at least this many days
  zero_offset_limit = 0.1,                      # a sensor whose median in the zero curtain is further than this from 0 C is listed (C)
  surface_max_depth = 0.10,                     # the ground-surface temperature (GST) is the shallowest sensor at or above this depth (m)
  trend_min_years = 4,                          # trends in MAGT and ALT need at least this many complete years
  permafrost_years = NA,                        # permafrost: frozen in this many consecutive years (1 to 3); NA: the project's old calibration_options.csv, else 2
  fill_gap_days = 5,                            # daily gaps up to this long, at fill_min_depth or deeper, are estimated linearly in time ...
  fill_min_depth = 0.5,                         # ... (m)
  fill_r2_min = 0.95,                           # longer gaps from the sensors above and below, when their regression explains this much
  fill_in_annual = FALSE,                       # TRUE: annual statistics (and what follows from them) may use the estimated days
  gap_factor    = 3,                            # a gap is a hole longer than this many logging intervals
  spike_k       = 10,                           # a spike also has to be this many robust SDs of the sensor's usual step
  check_air_like = TRUE, air_like_daily_range = 15, air_like_min_depth = 0.2,
  check_pre_install = TRUE, pre_install_spread = 0.3, pre_install_days = 60,
  between_site_check = TRUE,                   # flag days on which a site departs from the other sites at the same depth
  between_site_k = 6, between_site_window = 31, between_site_r_min = 0.5,
  hide_accepted_flags = TRUE,                   # flags decided not a problem leave the records' flag column (flags.csv keeps them, marked accepted)
  check_exports = TRUE,                         # compare an RBR .hex with its Ruskin export where both exist
  incremental   = TRUE                          # reuse loggers whose files and set-up have not changed (kept in <project>/cache)
)
# Settings that only the summaries and reports use, and a run's own notes (run_anyway): changing one redoes no cached read,
# quality check or site record (the cache keys leave them out).
SUMMARY_KEYS <- c("min_complete", "year_complete", "water_year_start", "zero_band", "zero_curtain_days", "zero_offset_limit",
                  "surface_max_depth", "trend_min_years", "permafrost_years", "fill_gap_days", "fill_min_depth", "fill_r2_min", "fill_in_annual",
                  "between_site_check", "between_site_k", "between_site_window", "between_site_r_min", "run_anyway")
if (nzchar(Sys.getenv("GT_RAW_DIRS")))     CFG$raw_dirs     <- strsplit(Sys.getenv("GT_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("GT_METADATA_DIR"))) CFG$metadata_dir <- Sys.getenv("GT_METADATA_DIR")
if (nzchar(Sys.getenv("GT_PROJECT_DIR")))  CFG$project_dir  <- Sys.getenv("GT_PROJECT_DIR")
if (nzchar(Sys.getenv("GT_DEPTH_POSITIVE"))) CFG$depth_positive <- Sys.getenv("GT_DEPTH_POSITIVE")
RUN_ON_SOURCE <- !identical(Sys.getenv("GT_NO_RUN"), "1")

for (p in c("data.table", "ggplot2", "readxl", "zip"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

PIPELINE_DIR <- local({ f <- NULL
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { f <- o; break } }
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) getwd() else dirname(normalizePath(f)) })
if (!dir.exists(file.path(PIPELINE_DIR, "R"))) stop("The R/ folder that belongs beside gt_pipeline.R was not found in ", PIPELINE_DIR)
APP_NAME <- "Ovid Core // GT"
APP_VERSION <- GT_VERSION

# The shared Ovid Core files (audit trail, helpers, set-up tables, run manifest, set-up check): from OVID_CORE if
# set, else core/ beside this file (a portable copy), else ../core (the Ovid_Apps bundle).
# They are sourced first; R/ holds what is GT's own.
CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(PIPELINE_DIR, "core"), file.path(dirname(PIPELINE_DIR), "core"))
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "util.R"))]
  if (!length(hit)) stop("The shared Ovid Core folder was not found. Looked in: ", paste(d, collapse = ", "), ". Set OVID_CORE to its path.")
  normalizePath(hit[1]) })
for (.f in c("util", "convert", "audit", "qc", "trend", "standards", "file_audit", "logger_files", "cache", "timezones", "timestamps", "time_corrections", "neighbours", "config", "calibration", "periods", "table_rules", "metasheet", "import", "deployments", "corrections", "applied", "manifest", "preflight", "theme", "report", "report_options"))
  source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
CFG$project_dir <- ovid_project_dir(CFG$project_dir)               # none set: ~/Projects/Ovid/Projects/<app>/project (core util.R)
for (.f in c("util", "readers", "f2b", "config", "metadata", "discover", "stitch", "qc", "derived", "plots", "export", "preflight", "report"))
  source(file.path(PIPELINE_DIR, "R", paste0(.f, ".R")), local = TRUE)
CODE_HASH <- code_hash(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "gt_pipeline.R"))   # every file's code (the run manifest)
# What each cached step depends on (core code_fingerprint()): only the code it runs, so an edit to a summary, a report or a
# core file a step does not use redoes nothing (summaries are made fresh on every run). STEP_HASH is set at the end of this
# file, once every function is defined.
CODE_NAMES <- code_names(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "gt_pipeline.R"))
step_hashes <- function(env = parent.frame()) list(
  files = code_fingerprint("discover_files", env = env),
  read  = code_fingerprint(c("read_source", "clock_checks", "tz_rows_for"), shallow = "run_pipeline_steps", env = env),
  qc    = code_fingerprint(c("assign_sensors", "apply_qc", "gt_period", "period_step", "period_apply", "find_gaps", "rule_applies", "remove_rows", "site_groups"), shallow = "run_pipeline_steps", env = env),
  site  = code_fingerprint(c("soil_phase", "remove_rows"), shallow = "run_pipeline_steps", env = env))

# Ground temperature records are kept long (one row per value: logger, channel, time), so remove_rows() covers
# removing single values and whole periods. "auto" writes a line per value (old value kept) up to 200 values, else one
# line per logger, channel and source file with a count, a time span and the range of values removed.
# Values removed by quality rules and site joins are also kept (AUDIT$stash) so the QC view can show them.
STASH_STAGES <- c("qc", "site")
audit_schema(keys = c("site", "logger_id", "channel", "variable", "timestamp", "old_value"), index_by = c("site", "logger_id", "channel", "variable"),
             summary_by = c("site", "logger_id", "channel", "variable", "source_file"), long = TRUE, detail = "auto", auto_limit = 200L,
             level_each = "value", level_summary = "values",
             on_remove = function(d, stage, rule) if (stage %in% STASH_STAGES && all(c("site", "depth_m") %in% names(d)))
               AUDIT$stash[[length(AUDIT$stash) + 1L]] <- d[, .(site, depth_m, variable, TIMESTAMP, value, logger_id, channel, rule = rule)])


# =============================================================================
#  RUN
# =============================================================================
run_pipeline_steps <- function(cfg = CFG) {
  t0 <- Sys.time()
  BASE_OFFSET <<- cfg$base_offset_h
  od <- out_dir(cfg$project_dir); ad <- file.path(od, "audit")
  project_dirs(od, c("sites", "soil", "summaries", "audit", file.path("audit", "detail")))   # core manifest.R
  unlink(list.files(file.path(ad, "detail"), full.names = TRUE))
  # site files of the last run stay until this run has rebuilt or reused them
  so_file <- file.path(ad, "site_outputs.rds"); prev_out <- outputs_prev(so_file, cfg$incremental)
  outputs_prune(od, c("sites", "soil"), prev_out)
  audit_reset(file.path(ad, "detail")); FLAGS$list <- list()

  # ---- 1. set-up ------------------------------------------------------------------------------------------------
  say("[1/8] Reading the set-up and the metadata folder", col = "accent")
  first_run <- !file.exists(cfg_path(cfg$project_dir, "sensors"))
  cfgs <- load_configs(cfg)
  pfile <- cfg_path(cfg$project_dir, "period")                                         # corrections over a period (core periods.R): rows that remove
  cfgs$manual <- gt_period_to_manual(period_own_rows(pfile, c("remove", "flag")))        # or flag go with the quality rules (apply_qc()), as the manual
  cfgs$period_rows <- period_load(pfile, skip = c("remove", "flag"))                     # removals did; the rest after them (gt_period())
  reg <- tryCatch(find_registry(cfg$metadata_dir), error = function(e) { problem("metadata", conditionMessage(e)); NULL })
  meta_ix <- tryCatch(index_metadata(cfg$metadata_dir), error = function(e) { problem("metadata", conditionMessage(e)); data.table() })
  notes <- tryCatch(read_field_notes(cfg$metadata_dir), error = function(e) { problem("field notes", conditionMessage(e)); data.table() })
  if (first_run) {
    if (!is.null(reg)) {
      sd <- seed_from_registry(reg, meta_ix)
      cfgs$sites <- sd$sites; cfgs$sensors <- sd$sensors
      write_cfg(cfgs$sites, cfg$project_dir, "sites"); write_cfg(cfgs$sensors, cfg$project_dir, "sensors")
      say("  seeded ", nrow(sd$sites), " sites and ", nrow(sd$sensors), " sensor rows from ", basename(reg$file))
    }
    if (!nrow(cfgs$templates)) { cfgs$templates <- default_templates(); write_cfg(cfgs$templates, cfg$project_dir, "templates") }
  }
  fwrite(meta_ix, file.path(ad, "metadata_index.csv")); fwrite_out(notes, file.path(ad, "field_notes.csv"))
  if (!is.null(reg)) fwrite(reg$data, file.path(ad, "registry.csv"))

  # ---- 2. files and bundles ---------------------------------------------------------------------------------------
  say("[2/8] Finding files", col = "accent")
  m <- discover_files(cfg, cfgs$file_rules)
  if (nrow(m)) files_gone_check(files_prev(file.path(ad, "files.csv"), "rel"), m$rel)          # core manifest.R
  if (!nrow(m)) stop("No files found in ", paste(cfg$raw_dirs, collapse = ", "))
  bb <- make_bundles(m); m <- bb$files; bundles <- bb$bundles
  say("  ", nrow(m), " files, ", nrow(bundles), " downloads; ", sum(bundles$status == "needs_export"), " need an export", if (any(bundles$status == "water_logger")) paste0("; ", sum(bundles$status == "water_logger"), " are HOBO water-level or conductivity loggers (read by WLR, not GT)") else "")
  for (i in which(!bundles$status %in% c("ok", "water_logger"))) problem(paste(TYPE_LABEL[bundles$type[i]], bundles$serial[i]), paste0(bundles$members[i], ": ", bundles$advice[i]))
  for (i in which(m$status %in% c("empty_file", "unrecognised_format", "unreadable")))
    problem(m$rel[i], switch(m$status[i], empty_file = "the file is empty; skipped", unrecognised_format = "not a logger file this app can read; skipped", "could not be read; skipped"))

  # ---- 3. read and stitch every logger ----------------------------------------------------------------------------
  say("[3/8] Reading and stitching loggers", col = "accent")
  src <- m[use == TRUE][order(type, serial, rel)]
  why0 <- vapply(seq_len(nrow(src)), function(i) zone_for(src$serial[i], src$rel[i], src$stated_offset[i], cfgs$timezones, cfg$base_offset_h, model = src$model[i] %||% NA)$why, "")
  unzoned_check(sum(why0 == "logger clock, taken as the base zone"), nrow(src), cfg$unzoned_offset_h %||% NA, isTRUE(cfg$ask_unzoned), cfg$base_offset_h)
  src[, logger_id := paste(TYPE_LABEL[type], serial)]
  src[, dl := as.POSIXct(ifelse(is.na(download), as.numeric(mtime), as.numeric(download)), origin = "1970-01-01", tz = "UTC")]
  loggers <- list(); read_counts <- list(); events <- list(); clock <- list(); file_read <- list(); xchk <- list(); meta_rows <- list()
  # a logger whose files, time-zone and clock rows and settings are unchanged is read back from the cache
  inc <- isTRUE(cfg$incremental); cdir <- file.path(cfg$project_dir, "cache"); c_used <- character(); n_hit <- c(read = 0L, qc = 0L)
  for (d in c("read", "qc")) dir.create(file.path(cdir, d), recursive = TRUE, showWarnings = FALSE)
  cfg_key <- key_cfg(cfg, c("raw_dirs", "metadata_dir", "project_dir", "incremental", "ask_unzoned", SUMMARY_KEYS))
  SIDE3 <- c("read_counts", "events", "clock", "file_read", "xchk", "meta_rows")
  key3 <- list(); data3 <- list(); seen_l <- list(); lids <- unique(src$logger_id)
  for (lid in lids) {
    fr <- src[logger_id == lid][order(dl)]
    sn <- fr$serial[1]
    key3[[lid]] <- k3 <- hash_of(list(STEP_HASH$read, fr[, .(rel, md5, format, serial, type, dl)], m[bundle %in% fr$bundle, .(rel, md5, format, model, status, role)],
                                      if (any(fr$format == "campbell_toa5")) cfgs$campbell, key_rows(tz_rows_for(cfgs$timezones, fr$serial, fr$rel, if ("model" %in% names(fr)) fr$model else NA)),
                                      key_rows(cfgs$time[norm_serial(serial) == sn]), cfg_key))
    data3[[lid]] <- sub("\\.rds$", ".data.rds", cache_file(file.path(cdir, "read"), lid, k3))
  }
  # the files of loggers that are read, not reused, are read ahead in parallel a few loggers at a time (R/stitch.R)
  todo <- lids[!(inc & file.exists(unlist(data3[lids])))]; pre <- new.env(); par <- run_workers() > 1L
  for (lid in lids) {
    run_tick(match(lid, lids), length(lids), lid)
    fr <- src[logger_id == lid][order(dl)]
    k3 <- key3[[lid]]; d3 <- data3[[lid]]
    c3 <- if (inc && file.exists(d3)) cache_read(file.path(cdir, "read"), lid, k3) else NULL
    if (!is.null(c3)) {
      for (n in SIDE3) assign(n, c(get(n), c3$side[[n]]))
      for (pp in c3$problems) problem(pp$where, pp$problem)
      if (length(c3$unreadable)) m[path %in% c3$unreadable, status := "unreadable"]
      AUDIT$log <- audit_restamp(c3$log)
      seen_l[[lid]] <- c3$seen; loggers[lid] <- list(NULL); n_hit[["read"]] <- n_hit[["read"]] + 1L
      c_used <- c(c_used, cache_file(file.path(cdir, "read"), lid, k3), d3)
      audit_flush(paste0("1_", lid)); next
    }
    n0 <- vapply(SIDE3, function(n) length(get(n)), 0L); np0 <- length(AUDIT$problems); unreadable <- character()
    if (par && !exists(lid, envir = pre, inherits = FALSE)) read_ahead(c(lid, todo[match(todo, lids) > match(lid, lids)]), src, cfgs, cfg, pre)
    pr <- if (par) get0(lid, envir = pre, inherits = FALSE); if (par) suppressWarnings(rm(list = lid, envir = pre))
    parts <- list()
    for (i in seq_len(nrow(fr))) {
      r <- tryCatch(read_pre(pr[[fr$path[i]]]) %||% read_source(fr[i], cfgs, cfg), error = function(e) { problem(fr$rel[i], paste("could not be read:", conditionMessage(e))); NULL })
      # a file with no data rows at all is skipped and reported (a table that holds only other channels is not this)
      if (!is.null(r) && !nrow(r$data) && identical(as.integer(r$meta$rows), 0L)) { problem(fr$rel[i], "holds no readings; skipped"); r <- NULL }
      if (is.null(r)) { m[path == fr$path[i], status := "unreadable"]; unreadable <- c(unreadable, fr$path[i]); next }
      # newest download wins: by the download time the file states, else by its own last time stamp (not the file's
      # modification time, which changes when files are copied to another drive)
      d <- r$data; d[, `:=`(logger_id = lid, file_rank = if (!is.na(fr$download[i])) as.numeric(fr$download[i]) else if (nrow(d)) suppressWarnings(max(as.numeric(TIMESTAMP), na.rm = TRUE)) else -Inf)]
      file_read[[length(file_read) + 1L]] <- data.table(rel = fr$rel[i], values_read = nrow(d), zone = r$zone$offset, zone_why = r$zone$why,
                                                        first_ts = if (nrow(d)) min(d$TIMESTAMP, na.rm = TRUE) else .POSIXct(NA_real_, tz = "UTC"),   # a file with no records: NA of the same class
                                                        last_ts = if (nrow(d)) max(d$TIMESTAMP, na.rm = TRUE) else .POSIXct(NA_real_, tz = "UTC"))
      read_counts[[length(read_counts) + 1L]] <- d[, .(values_read = .N), by = .(logger_id, channel)]
      cc <- clock_checks(d, cfg, lid); d <- cc$data
      if (nrow(cc$events)) clock[[length(clock) + 1L]] <- cc$events[, source_file := fr$rel[i]]
      if (nrow(r$events)) events[[length(events) + 1L]] <- r$events[, `:=`(logger_id = lid, source_file = fr$rel[i])]
      if (!is.null(r$meta$drift_s)) clock[[length(clock) + 1L]] <- data.table(rule = "clock_drift", first = NA_integer_, last = NA_integer_, n = 0L,
                                     t0 = r$meta$host_time, t1 = r$meta$logger_time, logger_id = lid, source_file = fr$rel[i], drift_s = r$meta$drift_s)
      mods <- c(r$meta$model, m[bundle == fr$bundle[i] & !is.na(model), model]); mods <- mods[!is.na(mods) & nzchar(mods)]   # the .hobo names the model; its CSV does not
      meta_rows[[length(meta_rows) + 1L]] <- data.table(logger_id = lid, model = if (length(mods)) mods[which.max(nchar(mods))] else NA_character_, title = r$meta$title %||% NA_character_,
                                                        interval_s = r$meta$interval_s %||% NA_real_)
      # an RBR .hex checked against its Ruskin export from the same download
      if (isTRUE(cfg$check_exports) && fr$format[i] == "rbr_hex") {
        ex <- m[bundle == fr$bundle[i] & format %in% c("rbr_txt", "rbr_xls") & status != "duplicate_file"]
        for (j in seq_len(nrow(ex))) xchk[[length(xchk) + 1L]] <- tryCatch({
          e <- read_source(ex[j], cfgs, cfg)$data; a <- merge(d[!is.na(value), .(channel, TIMESTAMP, value)], e[!is.na(value), .(channel, TIMESTAMP, value)], by = c("channel", "TIMESTAMP"))
          data.table(logger_id = lid, raw_file = fr$rel[i], export_file = ex$rel[j], values_compared = nrow(a), max_abs_diff = if (nrow(a)) signif(max(abs(a$value.x - a$value.y)), 3) else NA_real_,
                     export_values = nrow(e), result = if (!nrow(a)) "no matching times: the export may be in another time zone" else if (max(abs(a$value.x - a$value.y)) < 1e-4) "agree" else "disagree") },
          error = function(e) data.table(logger_id = lid, raw_file = fr$rel[i], export_file = ex$rel[j], values_compared = 0L, max_abs_diff = NA_real_, export_values = NA_integer_,
                                         result = paste("export could not be read:", conditionMessage(e))))
      }
      parts[[length(parts) + 1L]] <- list(data = d)
    }
    if (!length(parts)) next
    d <- stitch_logger(parts, lid)
    tcr <- if (NROW(cfgs$time)) which(norm_serial(cfgs$time$serial) == src[logger_id == lid, serial][1]) else integer()
    d <- apply_time_corrections(d, cfgs$time, tcr, audit = FALSE)    # core time_corrections.R; GT lists the shifts in clock.csv
    n_shift <- attr(d, "shifted")
    if (n_shift) clock[[length(clock) + 1L]] <- data.table(rule = "clock_shift_by_hand", first = NA_integer_, last = NA_integer_, n = n_shift, t0 = min(d$TIMESTAMP), t1 = max(d$TIMESTAMP), logger_id = lid, source_file = NA_character_)
    ty <- fr$type[1]
    d[, `:=`(logger_type = ty, serial = fr$serial[1], sensor = sensor_of(channel, ty))]
    loggers[[lid]] <- d; seen_l[[lid]] <- seen_of(d)
    if (inc) { saveRDS_safe(d, d3, compress = FALSE)
      c_used <- c(c_used, d3, cache_write(file.path(cdir, "read"), lid, k3, list(side = lapply(setNames(SIDE3, SIDE3), function(n) { x <- get(n); x[seq_along(x) > n0[[n]]] }),
        problems = AUDIT$problems[seq_along(AUDIT$problems) > np0], unreadable = unreadable, log = AUDIT$log, seen = seen_l[[lid]]))) }
    audit_flush(paste0("1_", lid))
  }
  if (!length(seen_l)) stop("None of the ", nrow(m), " files in ", paste(cfg$raw_dirs, collapse = ", "), " holds readings GT can use (",
                             paste(vapply(AUDIT$problems, function(x) paste0(x$where, ": ", x$problem), ""), collapse = "; ") %||% "", "); nothing to process")
  say("  ", length(loggers), " loggers stitched", if (n_hit[["read"]]) paste0(" (", n_hit[["read"]], " unchanged, reused)"))
  read_counts <- if (length(read_counts)) rbindlist(read_counts)[, .(values_read = sum(values_read)), by = .(logger_id, channel)] else data.table(logger_id = character(), channel = character(), values_read = integer())
  meta_rows <- if (length(meta_rows)) rbindlist(meta_rows)[, .SD[.N], by = logger_id] else data.table(logger_id = character(), model = character(), title = character(), interval_s = numeric())

  # ---- 4. sensors and depths ---------------------------------------------------------------------------------------
  say("[4/8] Sensors, sites and depths", col = "accent")
  seen <- unique(rbindlist(seen_l))
  if (nrow(seen)) seen[, model := meta_rows$model[match(logger_id, meta_rows$logger_id)]]
  ss <- sheet_sensors(cfgs$sensors, meta_sheet(cfg$project_dir, META_FIELDS, deploy = gt_deploy_rows, map = GT_MAP))   # the deployment sheet with the logger list's columns (optional)
  cs <- complete_sensors(ss$sens, seen, cfgs$templates, cfg, ss$depths)
  if (!identical(cs$sensors, ensure_cols(copy(cfgs$sensors), SENSOR_COLS)[, SENSOR_COLS, with = FALSE])) { write_cfg(cs$sensors, cfg$project_dir, "sensors"); if (cs$added) say("  ", cs$added, " sensors added to sensors.csv (default steps or template; edit them on the Sensors and depths page)") }
  sens <- cs$sensors
  # a site holds one instrument type: a name used for two types is split, with a note
  st <- unique(sens[nzchar(site) & !is.na(logger_type) & nzchar(logger_type), .(site, logger_type)])   # a sensor whose logger type is not known yet is not another instrument
  mixed <- st[, .N, by = site][N > 1, site]
  for (s in mixed) problem(s, paste0("this site name is used for ", paste(TYPE_LABEL[st[site == s, logger_type]], collapse = " and "),
                                    " loggers; they are kept as separate sites (", paste0(s, " [", TYPE_LABEL[st[site == s, logger_type]], "]", collapse = ", "), "), never merged"))
  sens[site %in% mixed & !is.na(logger_type) & nzchar(logger_type), site := paste0(site, " [", fifelse(is.na(TYPE_LABEL[logger_type]), logger_type, TYPE_LABEL[logger_type]), "]")]
  sites_cfg <- copy(cfgs$sites)
  new_sites <- setdiff(unique(sens$site[nzchar(sens$site)]), sites_cfg$site)
  if (length(new_sites)) {
    add <- unique(sens[site %in% new_sites, .(site, instrument = TYPE_LABEL[logger_type][1], site_group = sub(" \\[.*$", "", site[1])), by = site][, .(site, instrument, site_group)])
    sites_cfg <- rbind(ensure_cols(sites_cfg, SITE_COLS), ensure_cols(add, SITE_COLS)[, SITE_COLS, with = FALSE], use.names = TRUE, fill = TRUE)
    write_cfg(sites_cfg, cfg$project_dir, "sites"); say("  ", length(new_sites), " new sites added to sites.csv")
  }

  # ---- 5. quality rules ----------------------------------------------------------------------------------------------
  say("[5/8] Quality rules", col = "accent")
  gaps <- list(); bat <- list(); bat_counts <- list(); kept <- list(); bat_files <- list()
  SIDE5 <- c("gaps", "bat", "bat_counts", "bat_files"); kept_file <- list(); key5 <- list(); kept_sites <- list()
  for (lid in names(loggers)) {
    run_tick(match(lid, names(loggers)), length(loggers), lid)
    ty <- src[logger_id == lid, type][1]; sn <- src[logger_id == lid, serial][1]
    # of the quality rules and manual removals, only the rows that can apply to this logger: its type, serial or sites
    sites_l <- unique(sens[logger_type == ty & norm_serial(serial) == sn & nzchar(site), site])
    qc_l <- cfgs$qc[vapply(applies_to, function(a) any(vapply(c(sites_l, ""), function(st) rule_applies(a, ty, sn, st), NA)), NA)]
    sen_l <- unique(sens[logger_type == ty & norm_serial(serial) == sn, sensor])
    man_l <- cfgs$manual[(!nzchar(serial) | norm_serial(serial) == sn) & vapply(site, function(x) { w <- period_parts(x); !length(w) || "*" %in% w || sn %in% norm_serial(w) || any(w %in% c(sites_l, sen_l)) }, NA)]
    man_l <- man_l[, setdiff(names(man_l), "row"), with = FALSE]
    k5 <- hash_of(list(STEP_HASH$qc, key3[[lid]], key_rows(sens[logger_type == ty & norm_serial(serial) == sn]), key_rows(qc_l), key_rows(man_l), key_rows(cfgs$period_rows)))
    d5 <- sub("\\.rds$", ".data.rds", cache_file(file.path(cdir, "qc"), lid, k5))
    c5 <- if (inc && file.exists(d5)) cache_read(file.path(cdir, "qc"), lid, k5) else NULL
    if (!is.null(c5)) {
      for (n in SIDE5) { x <- get(n); x[lid] <- list(c5$side[[n]]); assign(n, x) }
      for (pp in c5$problems) problem(pp$where, pp$problem)
      FLAGS$list <- c(FLAGS$list, c5$flags); AUDIT$stash <- c(AUDIT$stash, c5$stash)
      AUDIT$log <- audit_restamp(c5$log)
      kept[lid] <- list(NULL); kept_file[[lid]] <- d5; key5[[lid]] <- k5; kept_sites[[lid]] <- c5$sites; n_hit[["qc"]] <- n_hit[["qc"]] + 1L
      c_used <- c(c_used, cache_file(file.path(cdir, "qc"), lid, k5), d5)
      audit_flush(paste0("2_", lid)); next
    }
    nf0 <- length(FLAGS$list); ns0 <- length(AUDIT$stash); np0 <- length(AUDIT$problems)
    d <- loggers[[lid]] %||% cache_load(data3[[lid]])
    d <- remove_rows(d, which(!d$variable %in% c("temp", "vwc", "ec", "battery")), "read", "not_a_sensor",
                     "a channel that is not temperature, moisture or conductivity (unsupported RBR channel type)", detail = "summary")
    b <- d[variable == "battery"]                                 # battery readings go to the battery table, not the site records
    if (nrow(b)) { bat[[lid]] <- b[, .(volts_min = min(value), volts_max = max(value)), by = .(logger_id, date = as.Date(TIMESTAMP))]
                   bat_counts[[lid]] <- b[, .(values_out = .N), by = .(logger_id, channel)]
                   bat_files[[lid]] <- b[, .(in_output = .N), by = source_file][, used_in := "battery table"] }
    d <- d[variable != "battery"]
    d <- assign_sensors(d, sens, ty, sn, depth_sign(cfg))
    d <- apply_qc(d, cfgs$qc, cfgs$manual, ty, sn, cfg)
    d <- gt_period(d, cfgs$period_rows, sn)                                             # corrections over a period (R/qc.R)
    g <- find_gaps(d, cfg$gap_factor); if (nrow(g)) gaps[[lid]] <- g
    kept[[lid]] <- d; kept_file[[lid]] <- d5; key5[[lid]] <- k5; kept_sites[[lid]] <- site_groups(d$site)
    if (inc) { saveRDS_safe(d, d5, compress = FALSE)
      c_used <- c(c_used, d5, cache_write(file.path(cdir, "qc"), lid, k5, list(side = lapply(setNames(SIDE5, SIDE5), function(n) get(n)[[lid]]),
        flags = FLAGS$list[seq_along(FLAGS$list) > nf0], stash = AUDIT$stash[seq_along(AUDIT$stash) > ns0],
        problems = AUDIT$problems[seq_along(AUDIT$problems) > np0], log = AUDIT$log, sites = kept_sites[[lid]]))) }
    loggers[lid] <- list(NULL)
    audit_flush(paste0("2_", lid))
  }
  if (n_hit[["qc"]]) say("  ", n_hit[["qc"]], " of ", length(kept), " loggers unchanged, reused")
  loggers <- NULL; gc(verbose = FALSE)
  gaps <- if (length(gaps)) rbindlist(gaps) else data.table(logger_id = character(), channel = character(), from = as.POSIXct(character()), to = as.POSIXct(character()), length_h = numeric(), step_min = numeric())

  # ---- 6. sites ----------------------------------------------------------------------------------------------------
  say("[6/8] Building site records", col = "accent")
  # one site at a time: the records of the loggers that were there; a site whose loggers are all unchanged is read
  # back from the cache and its files are left as they are
  site_lids <- list(); for (lid in names(kept_sites)) for (sg in kept_sites[[lid]]) site_lids[[sg]] <- c(site_lids[[sg]], lid)
  sgs <- names(site_lids); sgs <- sgs[order(sgs != SITE_NONE, sgs, method = "radix")]       # the order a sort of all records by site gives
  removed_qc <- if (length(AUDIT$stash)) rbindlist(AUDIT$stash, fill = TRUE) else data.table()
  dir.create(file.path(cdir, "site"), showWarnings = FALSE); parts <- list(); site_out <- list(); n_site_hit <- 0L; log6 <- list(); site_done <- list()
  kept_of <- function(lid) { if (is.null(kept[[lid]])) kept[[lid]] <<- cache_load(kept_file[[lid]]); kept[[lid]] }
  for (sg in sgs) {
    run_tick(match(sg, sgs), length(sgs), sg)
    s <- if (sg == SITE_NONE) NA_character_ else sg
    k6 <- hash_of(list(STEP_HASH$site, sg, unlist(key5[sort(site_lids[[sg]])])))
    c6 <- if (inc) cache_read(file.path(cdir, "site"), sg, k6) else NULL
    if (!is.null(c6) && all(file.exists(file.path(od, c6$outputs)))) {
      parts[[sg]] <- cache_load(sub("\\.rds$", ".data.rds", cache_file(file.path(cdir, "site"), sg, k6)))
      log6 <- c(log6, audit_restamp(c6$log)); AUDIT$stash <- c(AUDIT$stash, c6$stash)
      site_out[[sg]] <- c6$outputs; site_done[[sg]] <- c(c6, list(k6 = k6, hit = TRUE)); n_site_hit <- n_site_hit + 1L
      c_used <- c(c_used, cache_file(file.path(cdir, "site"), sg, k6), sub("\\.rds$", ".data.rds", cache_file(file.path(cdir, "site"), sg, k6))); next
    }
    x <- rbindlist(lapply(site_lids[[sg]], function(l) { d <- kept_of(l); d[if (is.na(s)) is.na(site) else site %in% s] }), fill = TRUE)
    if (!"flag" %in% names(x)) x[, flag := ""]
    x[is.na(flag), flag := ""]
    # loggers that replaced each other at a site: where two cover the same depth and time, the later deployment wins
    setorder(x, variable, depth_m, TIMESTAMP, -dep_start, -file_rank)
    set(x, j = "k", value = run_rank(run_starts(x$variable, x$depth_m, x$TIMESTAMP)))     # as seq_len(.N) by variable, depth and time (R/stitch.R)
    AUDIT$log <- list(); ns0 <- length(AUDIT$stash)
    x <- remove_rows(x, which(x$k > 1L), "site", "site_overlap", "two loggers of this site cover the same depth and time; the later deployment is kept", detail = "summary")
    x[, k := NULL]; l6 <- AUDIT$log; st6 <- AUDIT$stash[seq_along(AUDIT$stash) > ns0]; log6 <- c(log6, l6); parts[[sg]] <- x
    site_done[[sg]] <- list(k6 = k6, log = l6, stash = st6, hit = FALSE)
    if (inc) { df <- sub("\\.rds$", ".data.rds", cache_file(file.path(cdir, "site"), sg, k6)); saveRDS_safe(x, df, compress = FALSE); c_used <- c(c_used, df) }
  }
  kept <- NULL
  AUDIT$log <- log6; audit_flush("3_sites")
  # The between-site check compares the daily means of every site, so it runs once all are built, and its flags go into the
  # records' flag column before the site files are written: the files, the export and its WMO codes carry them. A cached
  # site whose between-site flags are the same as last time keeps its files.
  tp <- rbindlist(lapply(parts, function(x) x[variable == "temp", .(site, depth_m, TIMESTAMP, value, logger_id, channel, variable)]))
  dd <- daily_means(tp[, .(site, depth_m, TIMESTAMP, value)], cfg$min_complete)
  bs <- if (isTRUE(cfg$between_site_check)) tryCatch(between_site_checks(dd, tp, cfg), error = function(e) { problem("between-site check", conditionMessage(e)); NULL }) else NULL
  tp <- NULL; bsp <- attr(bs, "periods")
  # flags decided not a problem (config/review_decisions.csv) leave the records' flag column; flags.csv keeps them, marked accepted (core config.R)
  acc <- review_accept(qc_flags_table(FLAGS$list, data.table(site = character(), logger_id = character(), channel = character(), start = character(), end = character(),
                       check = character()), "GT", FALSE), review_read(review_path(cfg$project_dir)), "site", "channel")[accepted == TRUE][, parameter := paste(logger_id, channel)]
  for (sg in sgs) {
    s <- if (sg == SITE_NONE) NA_character_ else sg; h <- site_done[[sg]]
    parts[[sg]] <- between_site_mark(parts[[sg]], bsp); bk <- between_site_key(bsp, s)
    a <- if (isFALSE(cfg$hide_accepted_flags)) acc[0] else acc[site %in% s]; if (nrow(a)) { y <- parts[[sg]]; lc <- paste(y$logger_id, y$channel)
      parts[[sg]] <- review_unmark(y, a, function(p) "flag", function(p) lc == p); bk <- paste0(bk, hash_of(a[, .(parameter, start, end, check)])) }
    if (isTRUE(h$hit) && identical(h$between %||% "", bk)) next
    x <- parts[[sg]]; st6 <- h$stash; o <- character()
    if (!is.na(s)) {
      tmp <- x[variable == "temp", .(TIMESTAMP, depth_m, value, flag, serial, sensor, depth_source, logger_type, logger_id)]
      if (nrow(tmp)) { setorder(tmp, depth_m, TIMESTAMP); saveRDS(tmp, file.path(od, "sites", paste0(safe_name(s), ".rds")))
        w <- site_wide(tmp)                                                                       # R/export.R
        w[, TIMESTAMP := fmt_ts(TIMESTAMP)]; setnames(w, "TIMESTAMP", "timestamp_utc-07"); fwrite(w, file.path(od, "sites", paste0(safe_name(s), ".csv")))
        o <- c(o, file.path("sites", paste0(safe_name(s), c(".rds", ".csv")))) }
      soil <- x[variable %in% c("vwc", "ec"), .(TIMESTAMP, variable, depth_m, value, flag, serial, sensor, depth_source)]
      if (nrow(soil)) { soil <- soil_phase(soil, tmp); saveRDS(soil, file.path(od, "soil", paste0(safe_name(s), ".rds")))
        ws <- dcast(soil, TIMESTAMP ~ variable + depth_m, value.var = "value"); ws[, TIMESTAMP := fmt_ts(TIMESTAMP)]; setnames(ws, "TIMESTAMP", "timestamp_utc-07")
        fwrite(ws, file.path(od, "soil", paste0(safe_name(s), ".csv"))); o <- c(o, file.path("soil", paste0(safe_name(s), c(".rds", ".csv")))) }
      rm_s <- rbindlist(c(list(if (nrow(removed_qc)) removed_qc[site == s] else NULL), st6), fill = TRUE)
      if (nrow(rm_s)) { saveRDS(rm_s, file.path(od, "sites", paste0(safe_name(s), "__removed.rds"))); o <- c(o, file.path("sites", paste0(safe_name(s), "__removed.rds"))) }
    }
    site_out[[sg]] <- o
    if (inc) c_used <- c(c_used, cache_write(file.path(cdir, "site"), sg, h$k6, list(log = h$log, stash = st6, outputs = o, between = bk)))
  }
  all <- rbindlist(parts, fill = TRUE); parts <- NULL
  if (!"flag" %in% names(all)) all[, flag := ""]
  if (n_site_hit) say("  ", n_site_hit, " of ", length(sgs), " sites unchanged, reused")
  if (inc) for (d in c("read", "qc", "site")) cache_prune(file.path(cdir, d), c_used) else unlink(list.files(file.path(cdir, c("read", "qc", "site")), full.names = TRUE))
  outputs_done(od, so_file, prev_out, site_out)
  site_list <- sort(unique(all$site))
  say("  ", length(site_list), " sites")

  # ---- 7. summaries --------------------------------------------------------------------------------------------------
  say("[7/8] Daily means and summaries", col = "accent")
  saveRDS(dd, file.path(od, "summaries", "daily.rds")); fwrite(dd, file.path(od, "summaries", "daily_means.csv"))
  est <- fill_daily(dd, cfg$fill_gap_days %||% 5, cfg$fill_min_depth %||% 0.5, cfg$fill_r2_min %||% 0.95); fwrite(est, file.path(od, "summaries", "daily_estimated.csv"))
  if (nrow(est)) say("  ", nrow(est), " missing day(s) estimated (summaries/daily_estimated.csv)", if (isTRUE(cfg$fill_in_annual)) "; annual statistics use them" else "; not used in annual statistics", col = "fg4")
  an <- annual_stats(if (isTRUE(cfg$fill_in_annual)) with_estimates(copy(dd), est) else dd, cfg$year_complete, cfg$water_year_start); fwrite(an, file.path(od, "summaries", "annual_stats.csv"))
  td <- thaw_depth(dd); fwrite(td, file.path(od, "summaries", "thaw_depth_daily.csv"))
  alt <- alt_from_surface(active_layer(copy(td), an, gt_permafrost_years(cfg$permafrost_years, cfg$project_dir)), cfgs$surface); fwrite(alt, file.path(od, "summaries", "active_layer.csv"))
  ft <- freeze_thaw(dd, cfg$zero_band, cfg$zero_curtain_days, cfg$water_year_start)
  fwrite(ft$dates, file.path(od, "summaries", "freeze_thaw.csv")); fwrite(ft$curtain, file.path(od, "summaries", "zero_curtain.csv"))
  fwrite(zaa_depth(an[depth_m >= 0]), file.path(od, "summaries", "zero_annual_amplitude.csv"))
  # degree days and n-factors (air from a sensor above the ground, or air_temperature.csv), taliks, sensor offsets, trends
  air <- air_daily(dd, cfgs$air, cfg$min_complete)
  dg <- rbind(degree_days(dd, cfg$year_complete, cfg$water_year_start)[, series := "ground"],
              if (nrow(air)) degree_days(air, cfg$year_complete, cfg$water_year_start, by = c("site", "source"))[, `:=`(series = "air", depth_m = NA_real_)], fill = TRUE)
  fwrite(dg, file.path(od, "summaries", "degree_days.csv"))
  nf <- n_factors(dd, air, cfg$year_complete, cfg$water_year_start, cfg$surface_max_depth)
  fwrite(nf, file.path(od, "summaries", "n_factors.csv"))
  if (nrow(nf) && !nrow(air)) say("  n-factors need air temperature: a sensor above the ground, or a series named in air_temperature.csv", col = "fg4")
  gst <- ground_state(an); fwrite(gst, file.path(od, "summaries", "ground_state.csv"))
  tk <- taliks(gst); fwrite(tk, file.path(od, "summaries", "taliks.csv"))
  if (nrow(tk) && any(tk$result == "supra-permafrost talik")) say("  supra-permafrost talik in ", uniqueN(tk[result == "supra-permafrost talik", site]), " site(s): see summaries/taliks.csv", col = "yellow")
  zo <- zero_curtain_offsets(dd[depth_m >= 0], ft$curtain[depth_m >= 0], cfg$zero_offset_limit); fwrite(zo, file.path(od, "summaries", "zero_curtain_offsets.csv"))
  if (nrow(zo) && any(nzchar(zo$check))) say("  ", sum(nzchar(zo$check)), " sensor(s) read more than ", cfg$zero_offset_limit, " C from 0 in the zero curtain: see summaries/zero_curtain_offsets.csv", col = "yellow")
  fwrite(trends(an, alt, cfg$trend_min_years), file.path(od, "summaries", "trends.csv"))
  fwrite(thermal_props(dd, an), file.path(od, "summaries", "thermal.csv"))
  if (!is.null(bs)) { fwrite(bs, file.path(ad, "between_sites.csv")); n1 <- sum(bs$verdict == "out of step"); n2 <- sum(bs$verdict == "departs at times")
    if (n1 + n2) say(sprintf("  between sites: of %d site-depths, %d are out of step and %d depart from the others on some days - see output/audit/between_sites.csv", nrow(bs), n1, n2), col = "yellow") }
  sm <- all[variable %in% c("vwc", "ec")]
  if (nrow(sm)) sm <- rbindlist(lapply(split(sm, by = "site"), function(z) soil_phase(z, all[site == z$site[1] & variable == "temp", .(TIMESTAMP, depth_m, value)])))
  if (nrow(sm)) fwrite(sm[, .(mean = round(mean(value), 4), min = round(min(value), 4), max = round(max(value), 4), n = .N),
                          by = .(site, variable, depth_m, year = as.integer(format(TIMESTAMP, "%Y")), water = fifelse(liquid_only, "liquid only (ground frozen)", "total"))],
                       file.path(od, "summaries", "soil_annual.csv"))

  # ---- 8. audit, reconciliation, catalogue --------------------------------------------------------------------------
  say("[8/8] Writing the audit trail", col = "accent")
  ix <- if (length(AUDIT$index)) rbindlist(AUDIT$index) else data.table(stage = character(), rule = character(), level = character(), site = character(), logger_id = character(),
                                                                         channel = character(), variable = character(), n = integer(), entries = integer(), first = character(), last = character(), detail_file = character())
  fwrite(ix, file.path(ad, "audit_index.csv"))
  rem <- ix[, .(removed = sum(n)), by = .(logger_id, channel)]
  out_n <- all[, .(values_out = .N), by = .(logger_id, channel)]
  if (length(bat_counts)) out_n <- rbind(out_n, rbindlist(bat_counts))
  rec <- merge(merge(read_counts, rem, by = c("logger_id", "channel"), all = TRUE), out_n[, .(values_out = sum(values_out)), by = .(logger_id, channel)], by = c("logger_id", "channel"), all = TRUE)
  for (v in c("values_read", "removed", "values_out")) set(rec, which(is.na(rec[[v]])), v, 0L)
  rec[, reconciles := values_read == values_out + removed]
  fwrite(rec, file.path(ad, "reconciliation.csv"))
  bad <- rec[reconciles == FALSE]
  # sensor health
  ec <- ix[stage %in% c("read") & rule %in% c("sensor_error", "missing_value"), .(errors = sum(n)), by = .(logger_id, channel)]
  qr <- ix[stage %in% c("qc", "site", "stitch"), .(removed_qc = sum(n)), by = .(logger_id, channel)]
  h <- sensor_health(read_counts[channel != "HK-BAT"], all, merge(ec, qr, by = c("logger_id", "channel"), all = TRUE), gaps)
  h <- merge(h, unique(all[, .(logger_id, channel, site, depth_m, depth_source)]), by = c("logger_id", "channel"), all.x = TRUE)
  fwrite(h, file.path(ad, "sensor_health.csv"))
  fwrite(gaps, file.path(ad, "gaps.csv"))
  fl <- qc_flags_table(FLAGS$list, data.table(site = character(), logger_id = character(), channel = character(), variable = character(),
                                              start = character(), end = character(), records = integer(), check = character(), detail = character()), "GT")   # + rule_id, qc_code, app, run_id
  fl <- review_accept(fl, review_read(review_path(cfg$project_dir)), "site", "channel")   # + accepted: decided not a problem (core config.R)
  fwrite(fl, file.path(ad, "flags.csv"))
  fwrite_out(if (length(clock)) rbindlist(clock, fill = TRUE) else data.table(), file.path(ad, "clock.csv"))
  fwrite_out(if (length(events)) rbindlist(events, fill = TRUE) else data.table(), file.path(ad, "events.csv"))
  fwrite_out(if (length(xchk)) rbindlist(xchk, fill = TRUE) else data.table(), file.path(ad, "export_checks.csv"))
  fwrite_out(if (length(bat)) rbindlist(bat) else data.table(), file.path(ad, "battery.csv"))
  fwrite(if (length(AUDIT$problems)) rbindlist(AUDIT$problems) else data.table(where = character(), problem = character()), file.path(ad, "problems.csv"))
  fr <- if (length(file_read)) rbindlist(file_read) else data.table(rel = character(), values_read = integer())
  m2 <- merge(m, fr, by = "rel", all.x = TRUE)
  byf <- if (length(AUDIT$by_file)) rbindlist(AUDIT$by_file)[, .(removed = sum(n)), by = .(rel = source_file)] else data.table(rel = character(), removed = integer())
  m2 <- merge(m2, byf, by = "rel", all.x = TRUE); m2[is.na(removed) & use == TRUE, removed := 0L]
  fwrite(m2[, !c("path")], file.path(ad, "files.csv"))
  # every raw file and every download: values read = values in a site record or the battery table + values dropped
  files_fa <- m2[, .(file = rel, bundle, status, used = use %in% TRUE & !is.na(values_read), type, serial, format, md5, size_bytes = size, first_ts, last_ts, read = values_read,
                     copy_of = dup_of)]
  kept_f <- rbind(all[, .(in_output = .N, used_in = paste(sort(unique(site)), collapse = " | ")), by = source_file], rbindlist(bat_files), fill = TRUE)
  fa <- file_reconcile(files_fa, kept_f, audit_by_file(), unit = "values")
  fwrite(fa[order(type, serial, first_ts)], file.path(ad, "file_audit.csv"), dateTimeAs = "write.csv")
  file_reconcile_check(fa, "values")
  if (nrow(bundles)) { bt <- file_reconcile_by(fa, "bundle")[, .(bundle, values_read = read, values_out = in_output, dropped, reconciles)]
    bundles <- merge(bundles, bt, by = "bundle", all.x = TRUE, sort = FALSE)
    bundles[, source_md5 := fa$md5[match(paste(bundle, source_file), paste(fa$bundle, basename(fa$file)))]] }
  fwrite(bundles, file.path(ad, "bundles.csv"))
  lg <- all[, .(sites = paste(unique(site), collapse = ", "), sensors = uniqueN(channel), first = min(TIMESTAMP), last = max(TIMESTAMP), values = .N), by = .(logger_id, logger_type, serial)]
  lg <- merge(lg, meta_rows, by = "logger_id", all.x = TRUE)
  fwrite(lg, file.path(ad, "loggers.csv"))
  ss <- all[variable == "temp", .(instrument = TYPE_LABEL[logger_type[1]], loggers = paste(unique(serial), collapse = ", "), depths = uniqueN(depth_m),
                                  depth_list = paste(format(sort(unique(depth_m))), collapse = ", "), assumed_depths = uniqueN(depth_m[depth_source %in% c("default", "template")]),
                                  first = min(TIMESTAMP), last = max(TIMESTAMP), values = .N, flagged = sum(nzchar(flag))), by = site]
  ss <- merge(ss, sites_cfg[, .(site, site_group, landcover)], by = "site", all.x = TRUE)
  fwrite(ss, file.path(ad, "sites_summary.csv"))
  ck <- if (length(clock)) rbindlist(clock, fill = TRUE) else data.table(); xc <- if (length(xchk)) rbindlist(xchk, fill = TRUE) else data.table(result = character(), values_compared = integer())
  cnt <- list(base_zone = if ("zone" %in% names(fr)) sum(fr[!is.na(zone) & zone != (cfg$base_offset_h %||% BASE_OFFSET), values_read]) else 0,   # values whose time stamps were moved
              rbr_drift = if ("drift_s" %in% names(ck)) sum(fr[rel %in% ck[abs(drift_s) > 1, source_file], values_read]) else 0, default_depths = uniqueN(all[depth_source %in% "default", .(logger_id, channel)]),
              gap_fill = nrow(est), fill_in_annual = nrow(est), check_exports = sum(xc[result == "disagree", values_compared]))
  try_or(applied_write(cfg, values = nrow(all), counts = cnt), where = "corrections applied")   # output/audit/corrections_applied.csv (core applied.R)
  write_run_manifest(cfg, t0, list(files = nrow(m), loggers = nrow(lg), sites = length(site_list), values_out = nrow(all),
                                   values_removed = sum(rec$removed), loggers_checked = length(key5), loggers_reused = n_hit[["qc"]], sites_total = length(sgs),
                                   sites_reused = n_site_hit, problems = length(AUDIT$problems)),
                     settings = cfg, packages = c("data.table", "ggplot2", "readxl", "zip"))
  fwrite(data.table(finished = fmt_ts(Sys.time())), file.path(ad, "summary.csv"))
  if (nrow(bad)) stop("Value counts do not reconcile for ", paste(unique(bad$logger_id), collapse = ", "), " (see output/audit/reconciliation.csv). Nothing was published from this run.")
  tryCatch(report_write(gt_run_report_doc(cfg$project_dir), out_dir(cfg$project_dir, "report"), "html", MI),
           error = function(e) say("  the run report could not be written: ", conditionMessage(e), col = "orange"))
  say(sprintf("Done in %.0f s: %d sites, %s values kept, %s removed (every removal is in output/audit).", as.numeric(Sys.time() - t0, units = "secs"),
              length(site_list), num(nrow(all)), num(sum(rec$removed))), col = "green")
  invisible(list(sites = site_list, loggers = lg))
}

# =============================================================================
#  Reading results back (used by the app and from R)
# =============================================================================
load_site <- function(site, project_dir = CFG$project_dir) { f <- out_dir(project_dir, "sites", paste0(safe_name(site), ".rds")); if (file.exists(f)) readRDS(f) else NULL }
load_removed <- function(site, project_dir = CFG$project_dir) { f <- out_dir(project_dir, "sites", paste0(safe_name(site), "__removed.rds")); if (file.exists(f)) readRDS(f) else NULL }
load_soil <- function(site, project_dir = CFG$project_dir) { f <- out_dir(project_dir, "soil", paste0(safe_name(site), ".rds")); if (file.exists(f)) readRDS(f) else NULL }
load_daily <- function(project_dir = CFG$project_dir) { f <- out_dir(project_dir, "summaries", "daily.rds"); if (file.exists(f)) readRDS(f) else data.table() }

# an output table that is empty this run: the file from an earlier run is removed, so nothing stale is read back
fwrite_out <- function(x, f) if (NCOL(x)) fwrite(x, f) else unlink(f)
run_pipeline <- function(cfg = CFG, ...) { cfg$project_dir <- ovid_project_dir(cfg$project_dir); run_project(cfg, function() run_pipeline_steps(cfg, ...)) }   # folders checked, project locked, run logged (core manifest.R)   # every run leaves output/logs/<start>.log (core manifest.R)
STEP_HASH <- step_hashes(environment())          # after every function is defined, run_pipeline() included
warn_core_shadows(PIPELINE_DIR)                  # an app function that replaces a core one (core/cache.R)
if (sys.nframe() == 0L && RUN_ON_SOURCE) run_pipeline(CFG)
