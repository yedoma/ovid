#!/usr/bin/env Rscript
# =============================================================================
#  cs_pipeline.R  --  Campbell Scientific .dat ingest / stitch / QC pipeline
# =============================================================================
#  What it does
#    1. Finds every .dat file under RAW_DIRS and reads the TOA5 header.
#    2. Identifies the logger by SERIAL NUMBER (falls back to a number in the
#       station name when the header says "No_SN" / blank, e.g. CR200 series).
#    3. Stitches all files for the same logger + table into one continuous
#       record, however the files overlap or how the program changed.
#    4. Renames loggers to STATIONS            (config/stations.csv)
#    5. Maps raw columns to COMMON PARAMETERS  (config/aliases.csv)
#    6. Applies QC removals                    (config/qc_rules.csv,
#                                               config/period_corrections.csv)
#    7. Writes clean station files, plots and an HTML report.
#
#  The audit rule
#    Nothing is ever dropped or blanked except through remove_rows() and
#    remove_cells(). Both write to output/audit/audit_log.csv, and the run
#    stops if row/cell counts do not reconcile against that log.
#    Raw files are never modified.
#
#  Use
#    Rscript cs_pipeline.R            (or Source it in RStudio)
#    First run creates the config CSVs; edit them and re-run.
#    Afterwards:  bog <- load_station("Bog");  plot_station(bog, c("air_temp","rh"))
#
#  Needs: data.table, ggplot2
# =============================================================================

for (.l in if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8", "UTF-8"))   # UTF-8 text ( deg C, uS/cm);
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break                               # macOS has no C.UTF-8

OVID_APP <- "CS"                                          # its projects go in ~/Desktop/Ovid_Projects/CS/ (core util.R)
CFG <- list(
  raw_dirs    = c("~/Desktop/dat/RAW", "~/Desktop/Dat_Files"),
  project_dir = "",     # config/ and output/ live here

  # Diagnostic / high-frequency tables that are not station data.
  skip_tables = c("Public", "Status", "DataTableInfo", "ts_data"),

  # CR10X / CR23X / 21X "mixed-array" files have no header. When TRUE they are
  # imported using config/cr10x.csv, where each file is given a logger id and
  # (optionally) the .FSL file that names its columns.
  import_cr10x = TRUE,

  # Logger clocks have no time zone in the file. Timestamps are kept exactly
  # as logged (stored as UTC so R never applies daylight-saving shifts).
  min_year    = 1995,                 # timestamps outside min_year..now+2d are bad
  hide_accepted_flags = TRUE,         # flags decided not a problem leave the exported flag columns (flags.csv keeps them, marked accepted)
  drop_stray_records = TRUE,          # leave out records with corrupt timestamps among good ones (off the time grid and out of order); each is audited
  sentinels   = c(-7999, 7999, -6999, 6999),   # Campbell out-of-range codes
  prefer      = "newest",             # which file wins when records disagree
  numeric_min = 0.99,                 # share of values that must parse for a numeric column

  station_format  = "csv",            # "csv", "parquet" (smaller and faster; needs the nanoparquet package) or "both"
  write_stitched  = TRUE,             # also write one CSV per logger table to output/stitched
  audit_combined_max = 1e6,           # write one combined audit_log.csv only up to this many entries
  incremental     = TRUE,             # reuse the stitched record of loggers whose files and corrections are unchanged (kept in <project>/cache)
  min_complete    = 0.8,              # share of records a day (or days a month) needs before a daily / monthly value is given
  water_year_start_month = 10,        # water year runs from 1 October
  cross_checks    = TRUE,             # flag values that disagree with another sensor on the same station
  neighbour_check = TRUE,             # flag days that depart from the same parameter at the other stations
  spike_step_max_interval_h = 3,      # spike and step limits apply to tables recorded this often or more (not daily tables); 0 = every table
  neighbour_params = "^(air_temp|rh|sw_in|lw_in)$",
  fill_r2_min = 0.9,                  # a missing daily mean is estimated from the neighbour that explains at least this share of its variance ...
  fill_max_days = 31,                 # ... in gaps up to this many days (output/derived/daily_estimated.csv; nothing is changed)
  make_plots      = TRUE,
  plot_max_params = 8,
  plot_max_points = 20000
)
# Optional overrides without editing the file
if (nzchar(Sys.getenv("CS_RAW_DIRS")))    CFG$raw_dirs    <- strsplit(Sys.getenv("CS_RAW_DIRS"), ";")[[1]]
if (nzchar(Sys.getenv("CS_PROJECT_DIR"))) CFG$project_dir <- Sys.getenv("CS_PROJECT_DIR")
RUN_ON_SOURCE <- !identical(Sys.getenv("CS_NO_RUN"), "1")

for (p in c("data.table", "ggplot2"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

META_COLS <- c("station", "stream", "logger_id", "table", "TIMESTAMP", "RECORD", "source_file", ".mtime", ".ord", ".raw_ts")

# =============================================================================
#  The components live in R/. This file keeps the settings above and run_pipeline(), the orchestration layer.
# =============================================================================
PIPELINE_DIR <- local({ f <- NULL
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { f <- o; break } }
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) getwd() else dirname(normalizePath(f)) })
if (!dir.exists(file.path(PIPELINE_DIR, "R"))) stop("The R/ folder that belongs beside cs_pipeline.R was not found in ", PIPELINE_DIR)
APP_NAME <- "Ovid Core // CS"
APP_VERSION <- PIPELINE_VERSION <- "1.11.0"          # bump when a change alters the output

# The shared Ovid Core files (audit trail, helpers, set-up tables, run manifest, set-up check): from OVID_CORE if
# set, else core/ beside this file (a portable copy), else ~/Desktop/Ovid. They are sourced first; R/ holds what is CS's own.
CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(PIPELINE_DIR, "core"), file.path(dirname(PIPELINE_DIR), "core"), "~/Desktop/Ovid")
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "util.R"))]
  if (!length(hit)) stop("The shared Ovid Core folder was not found. Looked in: ", paste(d, collapse = ", "), ". Set OVID_CORE to its path.")
  normalizePath(hit[1]) })
for (.f in c("util", "convert", "audit", "qc", "standards", "file_audit", "logger_files", "cache", "timezones", "timestamps", "neighbours", "config", "calibration", "periods", "table_rules", "metasheet", "import", "deployments", "corrections", "applied", "manifest", "preflight", "theme", "report", "report_options"))
  source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
CFG$project_dir <- ovid_project_dir(CFG$project_dir)               # none set: ~/Desktop/Ovid_Projects/<app>/Untitled (core util.R)
for (.f in c("plots", "toa5", "tob3", "cr10x", "program", "stitch", "config", "qc", "derived", "homog", "precip", "history", "preflight", "io", "export", "composite"))
  source(file.path(PIPELINE_DIR, "R", paste0(.f, ".R")), local = TRUE)
CODE_HASH <- code_hash(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "cs_pipeline.R"))   # every file's code (the run manifest)
# What each cached step depends on (core code_fingerprint()): only the code it runs, so an edit to a report, a plot or a core
# file a step does not use redoes nothing. Reading and stitching (the slow steps) follow their functions only; the steps
# made inline in run_pipeline() also take its own text.
CODE_NAMES <- code_names(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "cs_pipeline.R"))
step_hashes <- function(env = parent.frame()) list(
  headers  = code_fingerprint("read_header", env = env),
  programs = code_fingerprint("read_program", env = env),
  stitch   = code_fingerprint("stitch_group", env = env),
  pieces   = code_fingerprint(c("assign_stations", "apply_aliases", "map_parameters", "remove_rows"), shallow = "run_pipeline_steps", env = env),
  station  = code_fingerprint(c("resolve_overlaps", "period_step", "period_apply", "apply_qc", "sensor_health", "write_station", "derive_products",
                                "plot_station", "gb_save", "remove_rows", "station_outputs"), shallow = "run_pipeline_steps", env = env))

# CS records are wide (one row per logger record): an audit line names the station, logger, table and record.
audit_schema(keys = c("station", "logger_id", "stream", "timestamp", "record", "column", "old_value"),
             index_by = c("station", "logger_id", "stream", "column"), summary_by = c("logger_id", "stream", "source_file"),
             cells_skip_na = FALSE)

# =============================================================================
#  RUN
# =============================================================================
run_pipeline_steps <- function(cfg = CFG) {
  t0 <- Sys.time()
  cfg$project_dir <- sub("[/\\\\]+$", "", path.expand(trim(cfg$project_dir)))
  if (!nzchar(cfg$project_dir)) stop("project_dir is empty.")
  if (length(cfg$base_offset_h) == 1 && !is.na(cfg$base_offset_h)) BASE_OFFSET <<- as.numeric(cfg$base_offset_h)   # Settings > Time zone: stations with no zone of their own
  out <- file.path(cfg$project_dir, "output"); adir <- file.path(out, "audit"); tmp <- file.path(out, ".tmp_run")
  project_dirs(cfg$project_dir, c("config", "output/stitched", "output/stations", "output/audit/detail", "output/plots", "output/.tmp_run", "output/derived", "output/qc", "cache/stitch", "cache/station", "cache/pieces"))   # core manifest.R
  # station files, their summaries, QC tables and plots of the last run stay until this run has rebuilt or reused them
  so_file <- file.path(adir, "station_outputs.rds"); prev_out <- outputs_prev(so_file, cfg$incremental)
  unlink(list.files(file.path(out, c("audit/detail", ".tmp_run")), full.names = TRUE))
  if (!isTRUE(cfg$incremental) || !isTRUE(cfg$write_stitched)) unlink(list.files(file.path(out, "stitched"), full.names = TRUE)); stitched_now <- character()
  outputs_prune(out, c("stations", "plots", "derived", "qc"), prev_out)
  lin <- list(); qc_rm <- list()
  FLAGS$list <- list(); cache_dir <- file.path(cfg$project_dir, "cache", "stitch"); cache_used <- character(); n_hit <- 0L
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  audit_reset(file.path(adir, "detail"))

  say("Ovid Core // CS  run ", AUDIT$run_id, col = "yellow")
  say("[1/6] scanning files", col = "aqua")
  man <- discover_files(cfg)
  files_gone_check(files_prev(file.path(adir, "file_manifest.csv"), "path"), man$path)   # core manifest.R
  say(sprintf("  %d files: %s", nrow(man), paste(man[, .N, by = status][, paste0(N, " ", status)], collapse = ", ")), col = "fg4")
  why <- c(duplicate_file = "byte-identical copy of another file", skipped_table = "table listed in skip_tables",
           unsupported_binary_TOB = "binary TOB1/TOB2 file, or a TOB3 file whose header could not be read; convert to TOA5 with LoggerNet/CardConvert",
           unsupported_mixed_array = "header-less mixed-array (CR10X) file; set import_cr10x = TRUE to import",
           cr10x_import_off = "mixed-array file marked import = no in cr10x.csv",
           no_usable_arrays = "mixed-array file with no array that could be timestamped (see malformed_row entries)",
           unrecognised_format = "not a Campbell TOA5 file", empty_file = "empty file", truncated_header = "header incomplete",
           unreadable = "file could not be opened")
  sk <- man[status != "ok"]
  if (nrow(sk)) audit_add("discover", sk$status, "file", NA, logger_id = sk$logger_id, stream = sk$table,
                          column = "<file>", reason = why[sk$status], source_file = sk$path)
  audit_flush("_files")
  ok <- man[status == "ok"]
  if (!nrow(ok)) stop("None of the ", nrow(man), " .dat files could be read as Campbell data. See the file list for why.")

  say("[2/6] set-up tables", col = "aqua")
  loggers <- ok[, .(seen_as = paste(unique(station_name[!is.na(station_name) & nzchar(station_name)]), collapse = " | ")), by = logger_id]
  cfgs <- cs_load_configs(cfg, loggers)
  ok <- source_station(ok, cfgs$stations)
  # rules for single files (Raw files page): send one file to another station, or leave it out
  fr <- cfgs$file_rules[nzchar(file)]
  for (i in seq_len(nrow(fr))) {
    hit <- (if (grepl("/", fr$file[i], fixed = TRUE)) ok$path == fr$file[i] else basename(ok$path) == fr$file[i]) & (fr$table[i] == "" | ok$table == fr$table[i])
    if (!any(hit)) next
    act <- tolower(fr$action[i]); why <- paste0(fr$reason[i], if (nzchar(fr$author[i])) paste0(" [", fr$author[i], "]") else "")
    if (act == "ignore") {
      audit_add("discover", "excluded_by_user", "file", NA, logger_id = ok$logger_id[hit], stream = ok$table[hit], column = "<file>",
                reason = paste0("left out on the Raw files page: ", why), source_file = ok$path[hit])
      man[src %in% ok$src[hit], status := "excluded_by_user"]; ok <- ok[!hit]
    } else if (act == "assign") {
      if (nzchar(fr$station[i])) ok[hit, src_station := fr$station[i]]
      if (nzchar(fr$stream[i])) ok[hit, src_stream := fr$stream[i]]
    } else problem("file_rules.csv", paste0("row ", i, ": action must be assign or ignore"))
  }
  # loggers set aside on the Stations page (station "ignore": one-off experiments, loggers of no interest): their files are not read at all
  st0 <- cfgs$stations; whole <- st0[tolower(station) == "ignore" & header_name == "" & table == "" & start == "" & end == "", logger_id]
  aside <- tolower(ok$src_station) == "ignore" | (ok$logger_id %in% whole & ok$src_station == "")
  if (any(aside)) {
    audit_add("discover", "set_aside", "file", NA, logger_id = ok$logger_id[aside], stream = ok$table[aside], column = "<file>",
              reason = "logger set aside on the Stations page (station 'ignore')", source_file = ok$path[aside])
    man[src %in% ok$src[aside], status := "set_aside"]; ok <- ok[!aside]
    say(sprintf("  %d file(s) from %d logger(s) set aside on the Stations page were not read", sum(aside), uniqueN(man[status == "set_aside", logger_id])), col = "fg4")
  }
  audit_flush("_files")
  if (!nrow(ok)) stop("Every readable file is left out by a rule on the Raw files page or a logger set aside on the Stations page.")
  # logger programs (config/programs): fill blank header units, and note how each column was measured and written
  pr <- tryCatch(apply_programs(ok, cfg), error = function(e) { problem("logger programs", conditionMessage(e)); list(ok = ok) })
  ok <- pr$ok; pm_file <- file.path(adir, "program_matches.csv")
  if (!is.null(pr$matches) && nrow(pr$matches)) fwrite(pr$matches, pm_file) else unlink(pm_file)
  dict <- rbindlist(lapply(seq_len(nrow(ok)), function(i) data.table(
    logger_id = ok$logger_id[i], table = ok$table[i], raw_column = ok$columns[[i]],
    unit = c(ok$units[[i]], rep("", 999))[seq_along(ok$columns[[i]])],
    process = c(ok$process[[i]], rep("", 999))[seq_along(ok$columns[[i]])])))
  dict <- unique(dict[!raw_column %in% c("TIMESTAMP", "RECORD") & nzchar(raw_column)], by = c("logger_id", "table", "raw_column"))
  dict[, parameter := map_parameters(raw_column, cfgs$aliases, logger_id[1], table[1]), by = .(logger_id, table)]
  dict[, group := param_group(parameter, cfgs$groups)]
  if (!is.null(pr$columns) && nrow(pr$columns)) dict <- merge(dict, pr$columns, by = c("logger_id", "table", "raw_column"), all.x = TRUE, sort = FALSE)

  say("[3/6] stitching by logger and table", col = "aqua")
  keys <- unique(ok[, .(logger_id, table, src_station, src_stream)])[order(logger_id, table, src_station)]
  gaps <- list(); spans <- list(); reg <- list(); pieces <- list(); failed <- character(); checks <- list(); pstats <- list(); health <- list()
  file_read <- list(); file_kept <- list(); piece_loggers <- list(); group_src <- list(); p_used <- character()
  pcache <- file.path(cfg$project_dir, "cache", "pieces")
  piece_file <- function(i, kk) file.path(tmp, sprintf("p%05d_%s.rds", i, safe_name(kk)))
  # one logger table's records, named and sent to their stations (the stations table can split a logger by date),
  # written as one piece per station; returns each piece's hash
  make_pieces <- function(data, i) {
    d <- assign_stations(apply_aliases(data, cfgs$aliases), cfgs$stations, keys$src_station[i], keys$src_stream[i])
    # a source the user has set aside (station name "ignore"): left out, and recorded
    d <- remove_rows(d, which(tolower(d$station) == "ignore"), "station", "ignored_source", "left out on request: station set to 'ignore' on the Stations page", detail = "summary")
    h <- list(); cols <- list()
    for (s in if (nrow(d)) split(d, by = c("station", "stream")) else list()) {
      kk <- paste(s$station[1], s$stream[1], sep = "__"); f <- piece_file(i, kk)
      saveRDS_safe(s, f, compress = FALSE); h[[kk]] <- unname(tools::md5sum(f)); cols[[kk]] <- setdiff(names(s), META_COLS) }
    list(pieces = h, cols = cols, station = paste(unique(d$station), collapse = " | "), stream = paste(unique(d$stream), collapse = " | "))
  }
  # the pieces of a station that has to be rebuilt, written from the cached records where this run did not need them yet
  pieces_of <- function(kk) vapply(pieces[[kk]], function(p) { if (!file.exists(p$file)) { gs <- group_src[[p$group]]
      lg <- AUDIT$log; make_pieces(setDT(cache_load(gs$dfile)), p$group); AUDIT$log <- lg }   # its removals are already logged
    p$file }, "")
  for (i in seq_len(nrow(keys))) {
    run_tick(i, nrow(keys), paste(keys$logger_id[i], keys$table[i]))
    g <- ok[logger_id == keys$logger_id[i] & table == keys$table[i] & src_station == keys$src_station[i] & src_stream == keys$src_stream[i]]
    k <- paste(c(keys$logger_id[i], keys$table[i], if (nzchar(keys$src_station[i])) keys$src_station[i]), collapse = "__")
    done <- tryCatch({
      tcr <- cfgs$time[logger_id == keys$logger_id[i] & (table == "" | table == keys$table[i])]
      tcr <- rbind(tcr, zone_corrections(g, cfgs$timezones, cfgs$stations, cfgs$sites), fill = TRUE)   # the time-zone table
      # a logger table whose files, corrections and settings are unchanged is read back from the cache: its summary
      # at once, its records (.data.rds) only if a station it feeds has to be rebuilt
      ck <- cache_key(g, tcr, cfg); cf <- cache_file(cache_dir, k, ck); dfile <- sub("\\.rds$", ".data.rds", cf); hit <- FALSE
      if (isTRUE(cfg$incremental) && file.exists(cf) && file.exists(dfile)) {
        cc <- tryCatch(readRDS(cf), error = function(e) NULL)
        if (!is.null(cc)) { res <- cc$res
          AUDIT$log <- audit_restamp(cc$log)
          for (pp in cc$problems) problem(pp$where, pp$problem)
          hit <- TRUE; n_hit <- n_hit + 1L }
      }
      if (!hit) { np0 <- length(AUDIT$problems)
        res <- stitch_group(g, cfg, tcr)
        if (!is.null(res$data)) res$info <- list(n = nrow(res$data), first = min(res$data$TIMESTAMP), last = max(res$data$TIMESTAMP), cols = names(res$data))
        if (isTRUE(cfg$incremental)) {
          tryCatch({ saveRDS_safe(res$data, dfile, compress = FALSE)
                     saveRDS_safe(list(res = res[setdiff(names(res), "data")], log = AUDIT$log, problems = AUDIT$problems[seq_along(AUDIT$problems) > np0]), cf, compress = FALSE) },
                   error = function(e) problem("cache", paste("could not be written:", conditionMessage(e)))) } }
      cache_used <- c(cache_used, cf, dfile)
      if (!is.null(res$checks) && nrow(res$checks)) checks[[k]] <- res$checks
      if (!is.null(res$read_by_file)) file_read[[k]] <- res$read_by_file
      if (length(res$empty)) {
        man[path %in% res$empty, status := "no_data_rows"]
        audit_add("discover", "no_data_rows", "file", 0L, logger_id = g$logger_id[1], stream = g$table[1],
                  column = "<file>", reason = "header only, no records", source_file = res$empty)
      }
      if (!is.null(res$info)) {
        a <- audit_table(); a <- a[stage == "stitch" & level == "row"]
        if (res$n_read != res$info$n + sum(a$n))
          stop("records do not reconcile: read ", res$n_read, ", kept ", res$info$n, ", audited ", sum(a$n))
        data_of <- function() { if (is.null(res$data)) res$data <<- setDT(cache_load(dfile)); res$data }
        if (isTRUE(cfg$write_stitched)) { sf <- file.path(out, "stitched", paste0(safe_name(k), ".csv")); stitched_now <- c(stitched_now, sf)
          if (!hit || !file.exists(sf)) fwrite(data_of()[, !".mtime"], sf, dateTimeAs = "write.csv") }
        r <- data.table(group = k, logger_id = keys$logger_id[i], table = keys$table[i], model = paste(unique(g$model), collapse = " | "),
          id_source = g$id_source[1], files = nrow(g), rows_read = res$n_read, rows_stitched = res$info$n,
          first_ts = res$info$first, last_ts = res$info$last, step_seconds = res$step,
          gaps = if (is.null(res$gaps)) 0L else nrow(res$gaps), logger_nan_values = res$n_nan,
          seen_as = paste(unique(g$station_name), collapse = " | "),
          serial = paste(unique(g$serial[!is.na(g$serial) & nzchar(g$serial)]), collapse = " | "),
          os = paste(unique(g$os[!is.na(g$os) & nzchar(g$os)]), collapse = " | "),
          program = paste(unique(sub("^CPU:", "", g$program[!is.na(g$program) & nzchar(g$program)])), collapse = " | "),
          format = g$format[1], columns = length(setdiff(res$info$cols, META_COLS)),
          largest_gap_hours = if (is.null(res$gaps)) 0 else max(res$gaps$gap_hours),
          conflicts = sum(vapply(AUDIT$log, function(x) if (x$rule[1] == "overlap_conflict") nrow(x) else 0L, 0L)))
        # the records split by station: which stations get which records (hash per piece), cached by the stations set-up
        # of this logger and the names its own columns get, so an alias for other loggers' columns changes nothing here
        group_src[[i]] <- list(k = k, dfile = dfile)
        raw_cols <- setdiff(res$info$cols, META_COLS)
        pk <- hash_of(list(ck, STEP_HASH$pieces, setNames(map_parameters(raw_cols, cfgs$aliases, keys$logger_id[i], keys$table[i]), raw_cols),
                           cfgs$stations[logger_id == keys$logger_id[i]], keys$src_station[i], keys$src_stream[i]))
        pc <- if (isTRUE(cfg$incremental)) cache_read(pcache, k, pk) else NULL
        if (!is.null(pc)) AUDIT$log <- c(AUDIT$log, audit_restamp(pc$log))
        else { n0 <- length(AUDIT$log); pc <- make_pieces(data_of(), i)
          pc$log <- AUDIT$log[seq_along(AUDIT$log) > n0]; if (isTRUE(cfg$incremental)) cache_write(pcache, k, pk, pc) }
        p_used <- c(p_used, cache_file(pcache, k, pk))
        for (kk in names(pc$pieces)) { pieces[[kk]] <- c(pieces[[kk]], list(list(file = piece_file(i, kk), hash = pc$pieces[[kk]], group = i, cols = pc$cols[[kk]])))
          piece_loggers[[kk]] <- unique(c(piece_loggers[[kk]], keys$logger_id[i])) }
        r[, `:=`(station = pc$station, stream = pc$stream)]
        res$data <- NULL
        reg[[k]] <- r; gaps[[k]] <- res$gaps; spans[[k]] <- res$spans
        say(sprintf("  %-36s %3d file(s) %10s -> %10s records%s", k, nrow(g), format(res$n_read, big.mark = ","),
                    format(res$info$n, big.mark = ","), if (hit) "  (unchanged, reused)" else ""), col = "fg4")
      }
      TRUE
    }, error = function(e) {
      AUDIT$log <- list()
      problem(k, paste0("left out: ", conditionMessage(e)))
      audit_add("stitch", "group_failed", "file", NA, logger_id = keys$logger_id[i], stream = keys$table[i], column = "<file>",
                reason = paste("this logger table was left out of the run:", conditionMessage(e)), source_file = g$path)
      FALSE
    })
    if (!done) { failed <- c(failed, k); man[path %in% g$path & status == "ok", status := "error"] }
    audit_flush(paste0("logger_", keys$logger_id[i]))
  }
  unlink(unlist(mget(ls(ARRAY_CACHE), envir = ARRAY_CACHE))); rm(list = ls(ARRAY_CACHE), envir = ARRAY_CACHE)
  cache_prune(file.path(out, "stitched"), stitched_now)
  if (isTRUE(cfg$incremental)) { cache_prune(cache_dir, cache_used); cache_prune(pcache, p_used)
    say(sprintf("  %d of %d logger tables were unchanged and reused", n_hit, nrow(keys)), col = "fg4") }
  if (!length(reg)) stop("No logger table could be stitched. See the problems listed above.")
  reg <- rbindlist(reg, fill = TRUE); gaps <- rbindlist(gaps); spans <- rbindlist(spans)
  man <- merge(man, spans, by.x = "src", by.y = "source_file", all.x = TRUE)
  if ("tob3_first" %in% names(man))                        # TOB3 tables that are only listed (10 Hz ts_data) still show their time span
    man[is.na(first_ts) & !is.na(tob3_first), `:=`(first_ts = as.POSIXct(tob3_first, origin = "1970-01-01", tz = "UTC"),
                                                   last_ts = as.POSIXct(tob3_last, origin = "1970-01-01", tz = "UTC"))]

  say("[4/6] quality rules + station files", col = "aqua")
  summ <- list(); station_plots <- character(); smp <- list(); ndaily <- list(); derived_made <- list()
  cal_log <- list(); outputs <- list(); s_used <- character(); s_hit <- 0L; scache <- file.path(cfg$project_dir, "cache", "station")
  STATE <- c("summ", "smp", "ndaily", "derived_made", "cal_log", "qc_rm", "lin", "pstats", "file_kept", "health")   # what each station adds to the run's tables
  for (kk in names(pieces)) {
    run_tick(match(kk, names(pieces)), length(pieces), kk)
    # a station whose records, set-up rows and settings are unchanged is read back from the cache, not rebuilt
    pcols <- lapply(pieces[[kk]], `[[`, "cols")                  # NULL from a piece cached before columns were kept: no narrowing
    skey <- if (isTRUE(cfg$incremental)) station_cache_key(kk, vapply(pieces[[kk]], `[[`, "", "hash"), piece_loggers[[kk]], cfgs, cfg,
                                                           if (any(vapply(pcols, is.null, NA))) NULL else unique(unlist(pcols))) else NA_character_
    sc <- if (!is.na(skey)) cache_read(scache, kk, skey) else NULL
    if (!is.null(sc) && all(file.exists(file.path(out, sc$outputs)))) {
      for (n in STATE) { x <- get(n); x[kk] <- list(sc$state[[n]]); assign(n, x) }
      FLAGS$list <- c(FLAGS$list, sc$flags); for (pp in sc$problems) problem(pp$where, pp$problem)
      AUDIT$log <- audit_restamp(sc$log)
      station_plots <- c(station_plots, sc$plots); outputs[[kk]] <- sc$outputs
      s_used <- c(s_used, cache_file(scache, kk, skey)); s_hit <- s_hit + 1L
      say(sprintf("  %-36s %10s records  (unchanged, reused)", kk, format(sc$state$summ$records, big.mark = ",")), col = "fg4")
      audit_flush(paste0("station_", kk)); next
    }
    nf_start <- length(FLAGS$list); np_start <- length(AUDIT$problems); npl_start <- length(station_plots)
    tryCatch({
      dt <- rbindlist(lapply(pieces_of(kk), readRDS), use.names = TRUE, fill = TRUE)
      setorderv(dt, c("TIMESTAMP", ".mtime"), c(1L, if (identical(cfg$prefer, "oldest")) 1L else -1L))   # which file wins (Automatic corrections)
      dt <- resolve_overlaps(dt, "station", "station_overlap")
      # corrections over a period (core periods.R), the calibrations among them: rows for the station, or for one of its loggers
      # (that logger's records only), and for this stream when they name one; the removals come with the quality rules
      prow <- if ("stream" %in% names(cfgs$period)) cfgs$period[!nzchar(stream) | stream == sub("^.*__", "", kk)] else cfgs$period
      dt <- period_step(dt, prow, sub("__.*$", "", kk), flag = add_flag, by = "logger_id",
                        where = list(station = sub("__.*$", "", kk), stream = sub("^.*__", "", kk)), skip = META_COLS,
                        on_log = function(l) { l <- l[action %in% c("scale", "offset")]; if (nrow(l)) cal_log[[kk]] <<- rbind(cal_log[[kk]], cs_cal_log(l, dt)) })
      vals <- setdiff(names(dt), META_COLS)
      before <- sum(vapply(vals, function(v) sum(!is.na(dt[[v]])), 0))
      n_aud0 <- sum(vapply(AUDIT$log, function(a) if (a$stage[1] == "qc") nrow(a) else 0L, 0L))
      nf0 <- length(FLAGS$list)
      dt <- apply_qc(dt, cfgs, cfg)
      n_flag <- if (length(FLAGS$list) > nf0) sum(vapply(FLAGS$list[(nf0 + 1L):length(FLAGS$list)], function(x) sum(x$records), 0)) else 0
      after <- sum(vapply(vals, function(v) sum(!is.na(dt[[v]])), 0))
      n_aud <- sum(vapply(AUDIT$log, function(a) if (a$stage[1] == "qc") nrow(a) else 0L, 0L)) - n_aud0
      # QC state kept beside the data: every value a quality rule removed, with the value it had
      qa <- rbindlist(Filter(function(a) a$stage[1] == "qc", AUDIT$log))
      if (nrow(qa)) { fwrite(qa[, .(timestamp, parameter = column, state = "removed", rule, old_value, reason)], file.path(out, "qc", paste0(safe_name(kk), "_removed.csv")))
        qc_rm[[kk]] <- qa[, .(removed = .N), by = .(parameter = column)][, `:=`(station = dt$station[1], stream = dt$stream[1])] }
      lin[[kk]] <- dt[, .(first = min(TIMESTAMP), last = max(TIMESTAMP), records = .N), by = .(station, stream, logger_id)]
      if (before - after != n_aud) stop("values do not reconcile: ", before - after, " removed but ", n_aud, " audited")
      dt[, c(".mtime", "table") := NULL]
      nv <- vals[vapply(vals, function(v) is.numeric(dt[[v]]) && any(!is.na(dt[[v]])), logical(1))]
      if (length(nv)) pstats[[kk]] <- rbindlist(lapply(nv, function(v) { x <- dt[[v]]; q <- stats::quantile(x, c(0.01, 0.5, 0.99), na.rm = TRUE, names = FALSE)
        data.table(station = dt$station[1], stream = dt$stream[1], parameter = v, values = sum(!is.na(x)), missing_pct = round(100 * mean(is.na(x)), 1),
                   min = min(x, na.rm = TRUE), p01 = q[1], median = q[2], p99 = q[3], max = max(x, na.rm = TRUE)) }))
      fn <- if (length(FLAGS$list) > nf0) rbindlist(FLAGS$list[(nf0 + 1L):length(FLAGS$list)], fill = TRUE) else NULL
      health[[kk]] <- tryCatch(sensor_health(dt, vals, if (nrow(qa)) qa[, .N, by = column][, setNames(N, column)] else NULL,
                                             if (!is.null(fn) && nrow(fn)) fn[, sum(as.numeric(records)), by = parameter][, setNames(V1, parameter)] else NULL),
                               error = function(e) { problem(paste("sensor health", kk), conditionMessage(e)); NULL })
      setcolorder(dt, c("station", "stream", "TIMESTAMP", "logger_id", intersect("RECORD", names(dt))))
      setcolorder(dt, c(setdiff(names(dt), "source_file"), "source_file"))
      write_station(dt, file.path(out, "stations", safe_name(kk)), cfg$station_format)
      file_kept[[kk]] <- dt[, .(in_output = .N), by = source_file][, used_in := sub("__", " / ", kk)]
      if (length(nv)) smp[[kk]] <- dt[as.numeric(TIMESTAMP) %% 864000 == 0, c("TIMESTAMP", nv), with = FALSE]
      tryCatch({ dp <- derive_products(dt, cfgs, cfg)
        if (!is.null(dp)) { b <- file.path(out, "derived", safe_name(kk))
          fwrite(dp$daily, paste0(b, "_daily.csv")); fwrite(dp$monthly, paste0(b, "_monthly.csv")); fwrite(dp$water_year, paste0(b, "_water_year.csv"))
          derived_made[[kk]] <- data.table(station = dt$station[1], stream = dt$stream[1], derived = paste(dp$made, collapse = ", "), totals = paste(dp$totals, collapse = ", "), days = nrow(dp$daily))
          np <- grep(cfg$neighbour_params, setdiff(names(dp$daily), c("Date", "records")), value = TRUE, perl = TRUE)
          if (isTRUE(cfg$neighbour_check) && length(np)) ndaily[[kk]] <- melt(dp$daily[, c("Date", np), with = FALSE], id.vars = "Date", variable.name = "parameter", na.rm = TRUE, variable.factor = FALSE) }
      }, error = function(e) problem(paste("daily and monthly summaries", kk), conditionMessage(e)))
      summ[[kk]] <- data.table(station = dt$station[1], stream = dt$stream[1], records = nrow(dt),
        first = min(dt$TIMESTAMP), last = max(dt$TIMESTAMP), parameters = length(vals),
        values_removed = n_aud, values_flagged = n_flag, loggers = paste(unique(dt$logger_id), collapse = ", "))
      if (isTRUE(cfg$make_plots)) {
        rem <- rbindlist(Filter(function(a) a$stage[1] == "qc", AUDIT$log))
        p <- tryCatch(plot_station(dt, max_params = cfg$plot_max_params, removed = if (nrow(rem)) rem else NULL), error = function(e) NULL)
        if (!is.null(p)) station_plots <- c(station_plots, gb_save(p, file.path(out, "plots", paste0("station_", safe_name(kk), ".png")),
                                                                  11, 1.2 + 1.35 * min(cfg$plot_max_params, length(vals))))
      }
      say(sprintf("  %-36s %10s records  %8s values removed", kk, format(nrow(dt), big.mark = ","), format(n_aud, big.mark = ",")), col = "fg4")
      outputs[[kk]] <- station_outputs(out, kk)
      if (!is.na(skey)) s_used <- c(s_used, cache_write(scache, kk, skey, list(state = lapply(setNames(STATE, STATE), function(n) get(n)[[kk]]),
        flags = FLAGS$list[seq_along(FLAGS$list) > nf_start], log = AUDIT$log, problems = AUDIT$problems[seq_along(AUDIT$problems) > np_start],
        plots = station_plots[seq_along(station_plots) > npl_start], outputs = outputs[[kk]])))
    }, error = function(e) {
      AUDIT$log <- list(); failed <<- c(failed, kk)
      problem(kk, paste0("station file not written: ", conditionMessage(e)))
      audit_add("station", "station_failed", "file", NA, station = sub("__.*$", "", kk), stream = sub("^.*__", "", kk), column = "<file>",
                reason = paste("this station was left out of the run:", conditionMessage(e)))
    })
    audit_flush(paste0("station_", kk))
    unlink(vapply(pieces[[kk]], `[[`, "", "file"))
  }
  if (s_hit) say(sprintf("  %d of %d station tables were unchanged and reused", s_hit, length(pieces)), col = "fg4")
  if (isTRUE(cfg$incremental)) cache_prune(scache, s_used) else unlink(list.files(scache, full.names = TRUE))
  outputs_done(out, so_file, prev_out, outputs)
  if (!length(summ)) stop("No station file could be written. See the problems listed above.")
  summ <- rbindlist(summ)
  # a parameter recorded by two tables of one station is compared once per station (the table with most days)
  nn <- NULL
  if (isTRUE(cfg$neighbour_check)) tryCatch({ nn <- rbindlist(ndaily, idcol = "key")
    if (nrow(nn)) { nn[, st := sub("__.*$", "", key)]; best <- nn[, .N, by = .(st, key, parameter)][order(-N)][!duplicated(paste(st, parameter))]
      nn <- nn[best, on = .(key, parameter), nomatch = NULL]; neighbour_checks(split(nn[, .(Date, parameter, value)], nn$key), cfg) } },
    error = function(e) problem("comparison between stations", conditionMessage(e)))
  # daily means estimated from the neighbours (L3), and breakpoints against them: tables of their own, nothing is changed
  tryCatch({ L <- if (!is.null(nn) && nrow(nn)) nn[, .(key, Date, parameter, value)] else NULL
    est <- neighbour_estimates(L, cfg$fill_r2_min %||% 0.9, 60, cfg$fill_max_days %||% 31); fwrite(est, file.path(out, "derived", "daily_estimated.csv"))
    if (nrow(est)) say("  ", nrow(est), " missing daily mean(s) estimated from neighbouring stations (output/derived/daily_estimated.csv)", col = "fg4")
    bp <- breakpoints(L); fwrite(bp, file.path(adir, "breakpoints.csv"))
    nb <- sum(bp$result == "break against the other stations"); if (nb) say("  ", nb, " series shift against the other stations: see output/audit/breakpoints.csv", col = "yellow") },
    error = function(e) problem("estimates and breakpoints", conditionMessage(e)))
  flags <- qc_flags_table(FLAGS$list, data.table(station = character(), stream = character(), parameter = character(), start = character(),
                                                  end = character(), records = integer(), check = character(), detail = character()), "CS")   # + rule_id, qc_code, app, run_id
  flags <- review_accept(flags, review_read(review_path(cfg$project_dir)), "station")   # + accepted: decided not a problem (config/review_decisions.csv, core config.R)
  dups <- tryCatch(detect_duplicates(smp), error = function(e) { problem("duplicate sources", paste0(conditionMessage(e), " (in ", paste(deparse(conditionCall(e)), collapse = " "), ")")); detect_duplicates(list()) }); rm(smp)
  if (nrow(dups)) say(nrow(dups), " pair(s) of station tables hold the same data - see output/audit/duplicate_sources.csv", col = "yellow")
  # every station gets a row in station_info.csv for coordinates, heights and notes
  si <- cfgs$sites; new_st <- setdiff(unique(summ$station), si$station)
  if (length(new_st)) fwrite_safe(ensure_cols(rbind(si, data.table(station = new_st), fill = TRUE), SITE_COLS)[, SITE_COLS, with = FALSE], cfgs$sites_file)

  say("[5/6] audit and metadata", col = "aqua")
  idx <- rbindlist(AUDIT$index)
  if (!nrow(idx)) idx <- data.table(stage = character(), rule = character(), level = character(), station = character(), logger_id = character(), stream = character(),
                                    column = character(), n = integer(), entries = integer(), first = character(), last = character(), detail_file = character())   # a clean archive: nothing to audit
  aud_summ <- idx[, .(n = sum(n, na.rm = TRUE), entries = sum(entries)), by = .(stage, rule, level)][order(stage, -n)]
  fwrite(idx, file.path(adir, "audit_index.csv"))
  fwrite(aud_summ, file.path(adir, "audit_summary.csv"))
  fwrite(idx[level == "cell", .(n = sum(entries)), by = .(station, logger_id, stream, column, rule)][order(-n)], file.path(adir, "audit_by_column.csv"))
  comb <- file.path(adir, "audit_log.csv"); unlink(comb)
  if (AUDIT$n <= cfg$audit_combined_max) {
    for (f in list.files(file.path(adir, "detail"), full.names = TRUE)) fwrite(fread(f, colClasses = "character", na.strings = ""), comb, append = file.exists(comb))
  } else say(sprintf("  %s audit entries: kept as one file per logger and station in audit/detail/ (no combined audit_log.csv)",
                     format(AUDIT$n, big.mark = ",")), col = "fg4")
  fwrite(man[, .(path, status, format, logger_id, id_source, station_name, model, serial, table, program, size, rows, first_ts, last_ts, dup_of)][order(logger_id, table, first_ts)],
         file.path(adir, "file_manifest.csv"), dateTimeAs = "write.csv")
  fwrite(reg, file.path(adir, "logger_registry.csv"), dateTimeAs = "write.csv")
  fmt_step <- function(s) ifelse(is.na(s), "", ifelse(s < 60, paste(s, "s"), ifelse(s < 3600, paste(round(s / 60, 1), "min"),
                          ifelse(s < 86400, paste(round(s / 3600, 1), "h"), paste(round(s / 86400, 1), "d")))))
  meta <- copy(reg)
  meta[, span_days := round(as.numeric(difftime(last_ts, first_ts, units = "days")), 1)]
  meta[, expected := ifelse(is.na(step_seconds) | step_seconds <= 0, NA, round(span_days * 86400 / step_seconds) + 1)]
  meta[, complete_pct := round(pmin(100, 100 * rows_stitched / expected), 1)]
  meta <- meta[, .(station, stream, logger_id, serial, model, table, program, os, interval = fmt_step(step_seconds),
                   records = rows_stitched, start = first_ts, end = last_ts, span_days, complete_pct, gaps,
                   largest_gap_hours, columns, files, duplicate_or_bad_rows = rows_read - rows_stitched,
                   conflicting_values = conflicts, logger_nan_values, format, id_source, header_names = seen_as)]
  fwrite(meta[order(station, stream)], file.path(adir, "metadata.csv"), dateTimeAs = "write.csv")
  fwrite(summ, file.path(adir, "station_summary.csv"), dateTimeAs = "write.csv")
  fwrite(flags[order(station, stream, parameter, start)], file.path(adir, "flags.csv")); fwrite(dups, file.path(adir, "duplicate_sources.csv"))
  fwrite(rbindlist(derived_made, fill = TRUE), file.path(adir, "derived_summary.csv"))
  bf <- rbindlist(AUDIT$by_file)
  bf <- if (nrow(bf)) bf[, .(n = sum(n), entries = sum(entries)), by = .(source_file, stage, rule, level)]
        else data.table(source_file = character(), stage = character(), rule = character(), level = character(), n = integer(), entries = integer())
  fwrite(bf, file.path(adir, "file_rule_counts.csv"))
  # every raw file (each array of a CR10X file on its own row): records read = records in a station file + records dropped
  fr <- if (length(file_read)) rbindlist(file_read)[, .(read = sum(read)), by = source_file] else data.table(source_file = character(), read = integer())
  files <- man[, .(file = src, path, name = basename(path), status, used = status == "ok", logger_id, table, format, md5, size_bytes = size, first_ts, last_ts,
                   copy_of = ifelse(status == "duplicate_file", basename(dup_of), NA_character_))]
  files[is.na(md5) & file.exists(path), md5 := unname(tools::md5sum(path))]
  files[, read := fr$read[match(file, fr$source_file)]]
  files[used & is.na(read), used := FALSE]
  ab <- audit_by_file()
  unparsed <- ab[stage == "parse" & level == "row", .(unparsed_lines = sum(n)), by = source_file]   # CR10X lines that never became records
  files[, unparsed_lines := unparsed$unparsed_lines[match(path, unparsed$source_file)]]
  fa <- file_reconcile(files, rbindlist(file_kept), ab, skip_stages = "parse")
  fwrite(fa[order(logger_id, table, first_ts, path)], file.path(adir, "file_audit.csv"), dateTimeAs = "write.csv")
  file_reconcile_check(fa)
  fwrite(if (nrow(gaps)) gaps else data.table(logger_id = character(), table = character(), gap_start = character(),
                                              gap_end = character(), gap_hours = numeric(), missing_records = numeric()),
         file.path(adir, "gaps.csv"), dateTimeAs = "write.csv")
  fwrite(dict[order(parameter == raw_column, logger_id, table)], file.path(adir, "column_dictionary.csv"))
  ck <- rbindlist(checks, fill = TRUE)
  if (!nrow(ck)) ck <- data.table(logger_id = character(), table = character(), check = character(), source_file = character(),
                                  from = character(), to = character(), records = integer(), detail = character(), suggest_shift_min = numeric())
  ck <- merge(ck, unique(reg[, .(logger_id, table, station, stream)], by = c("logger_id", "table")), by = c("logger_id", "table"), all.x = TRUE, sort = FALSE)
  fwrite(ck[order(-records)], file.path(adir, "timestamp_checks.csv"))
  cl <- rbindlist(cal_log, fill = TRUE); if (!nrow(cl)) cl <- data.table(station = character(), stream = character(), parameter = character(), output = character(), multiplier = numeric(), offset = numeric(), start = character(), end = character(), values = integer(), notes = character())
  fwrite(cl, file.path(adir, "calibrations_applied.csv")); if (nrow(cl)) say("  calibrations: ", format(sum(cl$values), big.mark = ","), " values in ", nrow(cl), " station table(s) - see output/audit/calibrations_applied.csv")
  ps <- rbindlist(pstats, fill = TRUE)
  if (nrow(ps)) { ps[, group := param_group(parameter, cfgs$groups)]
    ir <- param_info_row(unique(ps$parameter), cfgs$info); names(ir) <- unique(ps$parameter)
    ps[, topic := cfgs$info$topic[ir[parameter]]]
    lo <- suppressWarnings(as.numeric(cfgs$info$typical_min[ir[ps$parameter]])); hi <- suppressWarnings(as.numeric(cfgs$info$typical_max[ir[ps$parameter]]))
    ps[, `:=`(typical_min = lo, typical_max = hi, outside_typical = !is.na(lo) & !is.na(hi) & (p01 < lo | p99 > hi))] }
  fwrite(ps, file.path(adir, "parameter_stats.csv"))
  sh <- rbindlist(health, fill = TRUE)
  fwrite(if (nrow(sh)) sh[order(health == "ok", station, stream, parameter)] else data.table(station = character(), stream = character(), parameter = character(), health = character()),
         file.path(adir, "sensor_health.csv"), dateTimeAs = "write.csv")
  if (nrow(sh) && any(sh$health != "ok")) say("  sensor health: ", sum(sh$health != "ok"), " of ", nrow(sh), " parameters are not ok - see output/audit/sensor_health.csv", col = "yellow")
  # QC state per parameter: how many values are good, suspect (kept) and removed
  if (nrow(ps)) { qs <- ps[, .(station, stream, parameter, values)]
    rmv <- rbindlist(qc_rm); if (nrow(rmv)) qs <- merge(qs, rmv, by = c("station", "stream", "parameter"), all = TRUE) else qs[, removed := 0L]
    sus <- if (nrow(flags[accepted == FALSE])) flags[accepted == FALSE, .(suspect = sum(as.numeric(records))), by = .(station, stream, parameter)] else data.table(station = character(), stream = character(), parameter = character(), suspect = numeric())
    qs <- merge(qs, sus, by = c("station", "stream", "parameter"), all.x = TRUE); for (v in c("values", "removed", "suspect")) set(qs, which(is.na(qs[[v]])), v, 0)
    qs[, good := pmax(0, values - pmin(suspect, values))]; fwrite(qs[order(station, stream, parameter)], file.path(adir, "qc_summary.csv")) }
  fwrite(tryCatch(program_history(man, reg), error = function(e) { problem("program history", conditionMessage(e)); data.table() }), file.path(adir, "program_history.csv"), dateTimeAs = "write.csv")
  fwrite(tryCatch(station_lineage(rbindlist(lin), reg), error = function(e) { problem("station lineage", conditionMessage(e)); data.table() }), file.path(adir, "lineage.csv"), dateTimeAs = "write.csv")
  # weighing precipitation gauges: add any newly recognised gauge to the set-up table, then process
  gz <- cfgs$gauges; found <- detect_gauges(ps); changed <- FALSE
  # a gauge row whose station was renamed or reassigned since it was set up follows the gauge to its new name
  have <- paste(summ$station, summ$stream); stale <- which(nzchar(gz$station) & !paste(gz$station, gz$stream) %in% have)
  for (i in stale) { cand <- if (is.null(found)) NULL else found[gauge_type == gz$gauge_type[i] & bucket_col == gz$bucket_col[i] & !paste(station, stream) %in% paste(gz$station, gz$stream)]
    if (!is.null(cand) && nrow(cand)) { say("  precip_gauges.csv: ", gz$station[i], " / ", gz$stream[i], " is now ", cand$station[1], " / ", cand$stream[1], col = "yellow")
      set(gz, i, "station", cand$station[1]); set(gz, i, "stream", cand$stream[1]); changed <- TRUE } }
  if (changed) fwrite_safe(gz, cfgs$gauges_file)
  if (!is.null(found)) { new <- found[!paste(station, stream) %in% paste(gz$station, gz$stream)]
    if (nrow(new)) { new[, enabled := "yes"]; gz <- ensure_cols(rbind(gz, new, fill = TRUE), PRECIP_COLS)[, PRECIP_COLS, with = FALSE]
      fwrite_safe(gz, cfgs$gauges_file); say("  precip_gauges.csv: added ", nrow(new), " gauge(s) recognised from their columns", col = "yellow") } }
  if (nrow(gz)) { say("  precipitation gauges", col = "fg4"); tryCatch(process_precip(cfg, gz, cfgs$field_log), error = function(e) problem("precipitation gauges", conditionMessage(e))) }
  else fwrite(data.table(station = character(), stream = character()), file.path(adir, "precip_summary.csv"))
  # one row per logger and header name: what the Stations page of the app lists (set-aside loggers too, so they can be brought back)
  src <- man[status %in% c("ok", "set_aside"), .(files = .N, records = sum(rows, na.rm = TRUE), first = suppressWarnings(min(first_ts, na.rm = TRUE)),
                               last = suppressWarnings(max(last_ts, na.rm = TRUE)), model = paste(unique(model), collapse = " | "),
                               tables = paste(unique(table), collapse = ", "), aside = all(status == "set_aside")), by = .(logger_id, header_name = station_name)]
  src[aside == TRUE, records := NA]; for (v in c("first", "last")) src[!is.finite(as.numeric(get(v))), (v) := NA]; src[, aside := NULL]   # set-aside files are not read
  fwrite(src[order(logger_id, first)], file.path(adir, "sources.csv"), dateTimeAs = "write.csv")
  # combined records: one parameter stitched from several stations (config/composites.csv), from the station files just written
  n_comp <- tryCatch(write_composites(cfgs$composites, cfg$project_dir), error = function(e) { problem("combined records", conditionMessage(e)); 0L })
  if (n_comp) say("  combined records: ", n_comp, " written to output/composites", col = "fg4")
  probs <- if (length(AUDIT$problems)) unique(rbindlist(AUDIT$problems)) else data.table(where = character(), problem = character())
  fwrite(probs, file.path(adir, "problems.csv"))

  try_or(applied_write(cfg, values = if ("values" %in% names(ps)) sum(ps$values) else NA), where = "corrections applied")   # output/audit/corrections_applied.csv (core applied.R)
  tryCatch(write_run_manifest(cfg, t0, list(files_found = nrow(man), files_read = sum(man$status == "ok"), loggers = uniqueN(reg$logger_id), logger_tables = nrow(reg),
      stations = uniqueN(summ$station), station_tables = nrow(summ), records = sum(summ$records), values_removed = sum(summ$values_removed), values_flagged = sum(summ$values_flagged),
      tables_reused = n_hit, stations_reused = s_hit, problems = nrow(probs)),
    settings = list(raw_dirs = cfg$raw_dirs, project_dir = cfg$project_dir, skip_tables = paste(cfg$skip_tables, collapse = ", "), import_cr10x = cfg$import_cr10x,
                    incremental = isTRUE(cfg$incremental), prefer = cfg$prefer, min_year = cfg$min_year, min_complete = cfg$min_complete,
                    sentinels = paste(cfg$sentinels, collapse = ", "), station_format = cfg$station_format),
    packages = c("data.table", "ggplot2", "nanoparquet", "shiny")), error = function(e) problem("run manifest", conditionMessage(e)))
  say("[6/6] report", col = "aqua")
  plots <- list(stations = station_plots)
  if (isTRUE(cfg$make_plots)) {
    plots$timeline <- gb_save(plot_timeline(man), file.path(out, "plots", "stitch_timeline.png"), 11, 1.5 + 0.2 * uniqueN(reg$logger_id))
    a <- idx[!is.na(n) & n > 0]
    if (nrow(a)) plots$audit <- gb_save(plot_audit(a), file.path(out, "plots", "audit_summary.png"), 10, 1.5 + 0.4 * uniqueN(a$rule))
  }
  tryCatch(write_report(out, cfg, man, summ, aud_summ, if (nrow(gaps)) gaps else data.table(gap_hours = numeric()), dict, plots),
           error = function(e) problem("report.html", conditionMessage(e)))
  say(sprintf("done in %.0fs  ->  %s", as.numeric(difftime(Sys.time(), t0, units = "secs")), out), col = "green")
  if (nrow(probs)) say(nrow(probs), " problem(s) did not stop the run - see output/audit/problems.csv", col = "orange")
  n_blank <- sum(!loggers$logger_id %in% cfgs$stations[station != "", logger_id])
  if (n_blank) say(n_blank, " logger(s) have no station name yet", col = "yellow")
  if (nrow(ck)) say(nrow(ck), " clock finding(s) - see output/audit/timestamp_checks.csv", col = "yellow")
  invisible(list(stations = summ, audit_index = idx, manifest = man, registry = reg, gaps = gaps, dictionary = dict, clock_checks = ck,
                 problems = probs, failed = failed))
}

run_pipeline <- function(cfg = CFG, ...) { cfg$project_dir <- ovid_project_dir(cfg$project_dir); run_project(cfg, function() run_pipeline_steps(cfg, ...)) }   # folders checked, project locked, run logged (core manifest.R)   # every run leaves output/logs/<start>.log (core manifest.R)
STEP_HASH <- step_hashes(environment())          # after every function is defined, run_pipeline() included
warn_core_shadows(PIPELINE_DIR)                  # an app function that replaces a core one (core/cache.R)
if (RUN_ON_SOURCE) result <- run_pipeline()

