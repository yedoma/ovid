# R/stitch.R  --  One continuous record per logger serial, from every file and every year that holds it.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# Repeat downloads overlap (a Solinst logger that is not cleared carries two seasons; the same HOBO file is exported
# twice). Records that are identical in every value are kept once; records that disagree at the same time stamp are
# resolved in favour of the newest download (the file whose record runs latest), and the losing records are audited.

VALUE_COLS <- c("pres", "temp", "ec_low", "ec_full", "ec_spc", "ec")

stitch_serial <- function(files, cfg) {
  parts <- list(); n_read <- 0L; evs <- list()
  for (i in seq_len(nrow(files))) {
    r <- read_cached(files$path[i], files$md5[i], files$format[i])
    d <- copy(r$data); if (!nrow(d)) next
    # a zone set by hand for this file, or from the time-zone table, replaces what the file states (or the UTC-7 assumption)
    ov <- if ("tz_override" %in% names(files) && !is.na(files$tz_override[i])) files$tz_override[i]
          else if ("tz_table" %in% names(files) && !is.na(files$tz_table[i])) files$tz_table[i] else NA_real_
    if (!is.na(ov)) {
      stated <- if (is.na(files$offset_hours[i])) BASE_OFFSET else files$offset_hours[i]
      d[, TIMESTAMP := to_base(TIMESTAMP + (stated - BASE_OFFSET) * 3600, ov)]
    }
    nc <- r$meta$nan_cells                                                # cells the logger wrote as NaN: audited with that reason
    if (!is.null(nc)) for (k in seq_len(nrow(nc))) audit_add("read", "logger_nan", "cell", nc$n[k], logger_id = files$serial[i], timestamp = nc$first[k], column = nc$column[k],
                                                        reason = "the logger wrote NaN (no reading); the cell is left empty", source_file = files$path[i])
    d[, `:=`(source_file = files$path[i], .rank = as.numeric(files$last_ts[i]) + i * 1e-6, .ord = seq_len(.N))]
    d[, unit_pres := files$unit_pres[i]][, unit_temp := files$unit_temp[i]]
    n_read <- n_read + nrow(d); parts[[length(parts) + 1L]] <- d
    if (nrow(r$events)) evs[[length(evs) + 1L]] <- copy(r$events)[, source_file := files$path[i]]
  }
  if (!length(parts)) return(NULL)
  dt <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  dt[, logger_id := files$serial[1]]
  # clock faults: impossible dates, then corrupt time stamps among good records (Ovid/timestamps.R)
  lo <- as.POSIXct(paste0(cfg$min_year %||% 2000, "-01-01"), tz = "UTC")
  bad <- is.na(dt$TIMESTAMP) | (!isFALSE(cfg$drop_bad_clock) & (dt$TIMESTAMP < lo | dt$TIMESTAMP > Sys.time() + 2 * 86400))   # switch: Automatic corrections
  n_bad <- sum(bad); if (n_bad) dt <- remove_rows(dt, which(bad), "stitch", "bad_timestamp", paste0("time stamp before ", cfg$min_year %||% 2000, " or in the future (clock not set, or reset)"), detail = "summary")
  sr <- if (!isFALSE(cfg$drop_stray_records) && nrow(dt)) stray_records(dt$TIMESTAMP, dt$source_file, dt$.ord) else logical(nrow(dt))
  n_stray <- sum(sr); if (n_stray) dt <- remove_rows(dt, which(sr), "stitch", "stray_record", STRAY_REASON, detail = "summary")
  rs <- logical(nrow(dt))                                                 # the clock jumped back by more than a day within a file (core timestamps.R)
  if (!isFALSE(cfg$drop_bad_clock)) for (ix in split(seq_len(nrow(dt)), dt$source_file)) { o <- ix[order(dt$.ord[ix])]; rs[o] <- clock_faults(dt$TIMESTAMP[o], dt$.ord[o], stray = FALSE)$reset }
  n_reset <- sum(rs); if (n_reset) dt <- remove_rows(dt, which(rs), "stitch", "clock_reset", "the logger clock jumped back by more than a day (a reset); these readings cannot be placed in time", detail = "summary")
  dt[, .ord := NULL]
  if (!nrow(dt)) return(NULL)
  vals <- intersect(VALUE_COLS, names(dt))
  setorderv(dt, c("TIMESTAMP", ".rank"), c(1L, -1L))
  key <- do.call(paste, c(list(as.numeric(dt$TIMESTAMP)), lapply(vals, function(v) round(dt[[v]], 6)), sep = "|"))
  dup <- duplicated(key)
  dt <- remove_rows(dt, which(dup), "stitch", "duplicate_record", "identical record in a repeat download; one copy kept", detail = "summary")
  clash <- duplicated(dt$TIMESTAMP)                       # same time, different values: the newest download (first after sorting) wins
  if (any(clash)) dt <- remove_rows(dt, which(clash), "stitch", "overlap_conflict",
                                    "another file has a different value at this time; the newest download was kept", detail = "summary")
  stopifnot(nrow(dt) + sum(dup) + sum(clash) + n_bad + n_stray + n_reset == n_read)
  dt[, .rank := NULL]
  step <- step_of(dt$TIMESTAMP)
  gaps <- gap_rows(dt$TIMESTAMP, step)                                 # core logger_files.R
  if (!is.null(gaps)) gaps <- gaps[, .(logger_id = files$serial[1], gap_start, gap_end, gap_hours = round(gap_s / 3600, 2), missing_records)]
  list(data = dt, n_read = n_read, duplicates = sum(dup), conflicts = sum(clash), bad_times = n_bad, stray = n_stray, resets = n_reset, step = step, gaps = gaps,
       events = if (length(evs)) unique(rbindlist(evs), by = c("TIMESTAMP", "event")) else empty_events())
}

# Clock corrections from the Clock checks page: shift a span of one logger's records by a number of minutes (mode shift, or
# blank), or spread a clock error found at download over the span (mode drift: no shift at start, shift_minutes at end, linear
# in between; the USGS proration, as GT does for RBR loggers). Applied by core time_corrections.R.
TIME_COLS <- c("serial", "start", "end", "shift_minutes", "reason", "author", "mode")
