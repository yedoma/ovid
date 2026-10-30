# R/stitch.R  --  One record per logger: time zones, clock faults, repeat downloads and disagreements between files.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.

# zone_for() and the time-zone table are in the core (Ovid/timezones.R). In GT a table row wins over what the file
# states (HOBOware header, Ruskin text = UTC); a file with neither is taken as the base zone (logger clocks are set to UTC-7).

# Reads one source file and moves its clock onto the base zone. Returns data with source_file, download and file_rank.
read_source <- function(fr, cfgs, cfg) {
  r <- read_logger_file(fr$path, fr$format, campbell_patterns = cfgs$campbell)
  unread_report(r$meta, fr$rel)                                           # cells that were not numbers (core audit.R)
  d <- r$data
  t0 <- if (nrow(d)) suppressWarnings(min(d$TIMESTAMP, na.rm = TRUE)) else NA
  z <- zone_for(fr$serial, fr$rel, if (is.null(r$meta$offset_hours)) NA else r$meta$offset_hours, cfgs$timezones, cfg$base_offset_h,
                model = fr$model %||% NA, first_ts = t0, unzoned = cfg$unzoned_offset_h %||% NA)
  d[, TIMESTAMP := to_base(TIMESTAMP, z$offset, cfg$base_offset_h)]
  if (nrow(r$events)) r$events[, TIMESTAMP := to_base(TIMESTAMP, z$offset, cfg$base_offset_h)]
  drift <- r$meta$drift_s %||% NA_real_
  if (isTRUE(cfg$correct_rbr_drift) && fr$format == "rbr_hex" && !is.na(drift) && abs(drift) > 1 && nrow(d)) {
    t0 <- min(d$TIMESTAMP, na.rm = TRUE); t1 <- to_base(r$meta$logger_time, z$offset, cfg$base_offset_h)
    if (!is.na(t1) && t1 > t0) d[, TIMESTAMP := TIMESTAMP + drift * pmax(0, pmin(1, as.numeric(TIMESTAMP - t0, units = "secs") / as.numeric(t1 - t0, units = "secs")))]
  }
  d[, `:=`(source_file = fr$rel)]
  list(data = d, events = r$events, meta = r$meta, zone = z)
}

# Reading ahead in parallel (core util.R par_map). read_quiet() reads one file and keeps what read_source() would have
# recorded (problems, warnings) instead of recording it; its console text is dropped. read_pre() records that in the main
# process when the file's turn comes, so the problem list and the log are in the same order as a run one file at a time.
read_quiet <- function(fr, cfgs, cfg) {
  np0 <- length(AUDIT$problems); lg <- get0("LOG", ifnotfound = NULL); nl0 <- if (is.environment(lg)) length(lg$lines) else 0L; w <- list()
  on.exit({ AUDIT$problems <- AUDIT$problems[seq_len(np0)]; if (is.environment(lg)) lg$lines <- lg$lines[seq_len(nl0)] })   # also when par_map reads a small batch here, not in workers
  utils::capture.output(r <- withCallingHandlers(read_source(fr, cfgs, cfg), warning = function(e) { w[[length(w) + 1L]] <<- e; invokeRestart("muffleWarning") }))
  list(r = r, problems = AUDIT$problems[seq_along(AUDIT$problems) > np0], warnings = w)
}
read_pre <- function(x) {
  if (is.null(x) || inherits(x, "error")) return(NULL)                    # not read ahead, or failed there: read in its turn
  for (p in x$problems) problem(p$where, p$problem)
  for (w in x$warnings) warning(w)
  x$r
}
# The files of the next loggers to read, at least four per worker but whole loggers, so memory stays bounded; into
# pre[[logger]], by path.
read_ahead <- function(lids, src, cfgs, cfg, pre) {
  n <- cumsum(vapply(lids, function(l) sum(src$logger_id == l), 0L)); k <- which(n >= 4L * run_workers())[1]
  lids <- lids[seq_len(if (is.na(k)) length(lids) else k)]
  fr <- src[logger_id %in% lids]
  got <- par_map(seq_len(nrow(fr)), function(i) read_quiet(fr[i], cfgs, cfg), what = "reading files")
  for (l in lids) assign(l, setNames(got[fr$logger_id == l], fr$path[fr$logger_id == l]), envir = pre)
}

# Clock faults inside one file, found from the order the rows were written in:
#   impossible_time  before min_year or in the future
#   stray_record     a corrupt time stamp among good readings (off the time grid and out of written order)
#   clock_reset      the clock jumped back by more than a day (a firmware reset or a flat clock battery: 1998, 2000)
#   clock_resync     the clock was set back by less than a day; readings written before that and stamped at or after
#                    the new time are superseded by the later ones
clock_checks <- function(d, cfg, logger_id) {
  if (!nrow(d)) return(list(data = d, events = data.table()))
  u <- unique(d[, .(ord, TIMESTAMP)])
  keep_clock <- isFALSE(cfg$drop_bad_clock)                                                     # switch: Automatic corrections
  rows <- clock_faults(u$TIMESTAMP, u$ord, if (keep_clock) -Inf else as.POSIXct(paste0(cfg$min_year, "-01-01"), tz = "UTC"), if (keep_clock) Inf else Sys.time() + 2 * 86400,
                       stray = !isFALSE(cfg$drop_stray_records))                                      # core timestamps.R
  sm <- function(k, rule) if (!any(k)) NULL else rows[k, .(rule = rule, first = min(ord), last = max(ord), n = .N, t0 = min(TIMESTAMP, na.rm = TRUE), t1 = max(TIMESTAMP, na.rm = TRUE))]
  ev <- rbindlist(list(sm(rows$bad, "impossible_time"), sm(rows$stray, "stray_record"), sm(rows$reset, "clock_reset"), sm(rows$resync, "clock_resync")))
  if (!nrow(ev)) ev <- data.table(rule = character(), first = integer(), last = integer(), n = integer(), t0 = as.POSIXct(character(), tz = "UTC"), t1 = as.POSIXct(character(), tz = "UTC"))
  ev <- ev[n > 0]
  why <- c(impossible_time = paste0("time stamp before ", cfg$min_year, " or in the future (clock not set, or reset)"),
           clock_reset = "the logger clock jumped back by more than a day (reset); these readings cannot be placed in time",
           clock_resync = "the logger clock was set back; these earlier readings carry times that later readings also have",
           stray_record = STRAY_REASON)
  for (r in c("impossible_time", "stray_record", "clock_reset", "clock_resync")) {
    o <- rows$ord[rows[[switch(r, impossible_time = "bad", stray_record = "stray", clock_reset = "reset", clock_resync = "resync")]]]
    if (length(o)) d <- remove_rows(d, which(d$ord %in% o), "stitch", r, why[[r]])
  }
  if (nrow(ev)) ev[, logger_id := logger_id]
  list(data = d, events = ev)
}


# Rows sorted by the given columns: TRUE where a run of equal values (NA equal to NA) starts. With it, a row's place in its
# run and the run's first value are worked out in one pass over rows already sorted, not by grouping them again.
run_starts <- function(...) { v <- list(...); n <- length(v[[1]]); if (!n) return(logical())
  same <- function(x) { x <- unclass(x); x1 <- x[-1L]; x0 <- x[-n]; (!is.na(x1) & !is.na(x0) & x1 == x0) | (is.na(x1) & is.na(x0)) }
  s <- rep(TRUE, n - 1L); for (x in v) s <- s & same(x); c(TRUE, !s) }
run_rank <- function(st) { g <- cumsum(st); seq_along(st) - which(st)[g] + 1L }       # seq_len(.N) by run

# All files of one logger into one record. Values the logger marked as errors leave first (audited with the logger's
# own reason); then repeated readings across downloads are kept once (newest download wins where they disagree).
stitch_logger <- function(parts, logger_id) {
  d <- rbindlist(lapply(parts, `[[`, "data"), fill = TRUE)
  if (!nrow(d)) return(d)
  d[, logger_id := logger_id]
  e <- which(!is.na(d$err))
  if (length(e)) for (msg in unique(d$err[e])) d <- remove_rows(d, which(d$err %in% msg), "read", "sensor_error", msg)
  d <- remove_rows(d, which(is.na(d$value)), "read", "missing_value", "the file has no value here (blank or NaN)")
  setorder(d, channel, TIMESTAMP, -file_rank, ord)
  st <- run_starts(d$channel, d$TIMESTAMP); set(d, j = "k", value = run_rank(st))             # as seq_len(.N) by channel and time
  if (any(d$k > 1L)) {
    set(d, j = "first", value = d$value[which(st)][cumsum(st)])                                  # as value[1L] by channel and time
    dup <- which(d$k > 1L & abs(d$value - d$first) < 1e-6); con <- which(d$k > 1L & !(abs(d$value - d$first) < 1e-6))
    rm_dup <- dup; rm_con <- con
    if (length(rm_con)) { d[, conflict := FALSE]; d[rm_con, conflict := TRUE] }
    d <- remove_rows(d, c(rm_dup), "stitch", "duplicate_value", "the same reading is in more than one download; one copy is kept", detail = "summary")
    if ("conflict" %in% names(d)) { d <- remove_rows(d, which(d$conflict), "stitch", "overlap_conflict", "another download has a different value at this time; the newest download is kept"); d[, conflict := NULL] }
    d[, first := NULL]
  }
  d[, k := NULL]
  setorder(d, channel, TIMESTAMP)
  d
}

# Gaps: a hole longer than gap_factor x the logging interval, per logger channel.
find_gaps <- function(d, gap_factor = 3) {
  if (!nrow(d)) return(data.table())
  d[order(TIMESTAMP), {
    st <- step_of(TIMESTAMP); g <- gap_rows(TIMESTAMP, st, gap_factor)          # core logger_files.R
    if (is.null(g)) NULL else g[, .(from = gap_start, to = gap_end, length_h = round(gap_s / 3600, 1), step_min = st / 60)]
  }, by = .(logger_id, channel)]
}

# The sensors a stitched logger record holds (for sensors.csv), kept with the cached record.
seen_of <- function(d) unique(d[variable %in% c("temp", "vwc", "ec"), .(logger_type, serial, sensor, logger_id)])

# Records with no site (a sensor not on the Sensors and depths page) are kept together under this name.
SITE_NONE <- "\u2205 no site"
site_groups <- function(site) { u <- unique(site); ifelse(is.na(u), SITE_NONE, u) }

# "Rescan": forgets what the cache holds for these files (their details, and the read, quality-checked records of
# their loggers), so the next run reads them again. Sites are rebuilt only if their records changed.
forget_gt_files <- function(project_dir, paths) {
  pd <- path.expand(project_dir); cd <- file.path(pd, "cache"); paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  # safe: the last run's file list, to find what to forget; with none there is nothing to forget
  fa <- tryCatch(fread(file = file.path(pd, "output", "audit", "files.csv"), colClasses = list(character = c("rel", "type", "serial"))), error = function(e) data.table())
  lids <- character()
  if (nrow(fa) && all(c("rel", "type", "serial") %in% names(fa))) {
    hit <- fa[vapply(rel, function(r) any(paths == r | endsWith(paths, paste0("/", r))), logical(1)) & !is.na(type) & !is.na(serial)]
    lids <- unique(paste(TYPE_LABEL[hit$type], hit$serial)) }
  for (d in c("read", "qc")) { f <- list.files(file.path(cd, d))
    unlink(file.path(cd, d, f[sub("__[^_]*$", "", sub("(\\.data)?\\.rds$", "", f)) %in% safe_name(lids)])) }
  file_known_drop(file.path(cd, "files.rds"), paths)                # core logger_files.R
  invisible(lids)
}
