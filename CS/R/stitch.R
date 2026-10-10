# R/stitch.R  --  One continuous record per logger table: overlaps, clock checks, time corrections.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

same_value <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) abs(a - b) <= 1e-9 * pmax(1, abs(a), abs(b)) else as.character(a) == as.character(b)
}

# Records sharing a timestamp are merged into one. dt must already be ordered
# so the preferred record comes first. Gaps in the preferred record are filled
# from the others (no loss); values that DISAGREE with the kept one are logged.
resolve_overlaps <- function(dt, stage, rule) {
  dup_ts <- dt$TIMESTAMP[duplicated(dt$TIMESTAMP)]
  if (!length(dup_ts)) return(dt)
  isdup <- dt$TIMESTAMP %in% dup_ts
  d <- dt[isdup]
  keep <- d[!duplicated(TIMESTAMP)]
  for (v in setdiff(names(d), META_COLS)) {
    x <- d[[v]]; nn <- which(!is.na(x))
    if (!length(nn)) next
    # first non-missing value per timestamp; a logger overflow code (7999, -6999 ...) only wins when no copy has a real value
    if (is.numeric(x)) { code <- x[nn] %in% CFG$sentinels | !is.finite(x[nn]); nn <- c(nn[!code], nn[code]) }
    f  <- nn[!duplicated(d$TIMESTAMP[nn])]
    kv <- x[f][match(d$TIMESTAMP, d$TIMESTAMP[f])]
    conflict <- which(!is.na(x) & !same_value(x, kv))
    if (length(conflict))
      remove_cells(d, v, conflict, stage, rule,
                   paste0("record overlaps another with the same timestamp; kept ", kv[conflict]))
    set(keep, j = v, value = x[f][match(keep$TIMESTAMP, d$TIMESTAMP[f])])
  }
  dropped <- d[duplicated(TIMESTAMP)]
  s <- dropped[, .N, by = .(logger_id, source_file)]
  audit_add(stage, paste0(rule, "_merged"), "row", s$N, logger_id = s$logger_id,
            stream = if ("stream" %in% names(d)) d$stream[1] else .col(d, "table", 1L), column = "<record>",
            station = .col(d, "station", 1L),
            reason = paste0(s$N, " overlapping records merged into the kept record; ",
                            "any values that differed are logged individually as '", rule, "'"),
            source_file = s$source_file)
  out <- rbind(dt[!isdup], keep)
  setorder(out, TIMESTAMP)
  out
}

to_numeric_columns <- function(dt, cfg, stage) {
  n_nan <- 0L
  for (v in setdiff(names(dt), c("TIMESTAMP", "source_file", ".mtime", "logger_id", "table"))) {
    x <- dt[[v]]
    if (!is.character(x)) next
    x <- fix_enc(x)
    nanv <- !is.na(x) & (x == "" | toupper(x) %in% c("NAN", "-NAN", "NA"))
    n_nan <- n_nan + sum(nanv & x != "", na.rm = TRUE)
    x[nanv] <- NA_character_
    num <- suppressWarnings(as.numeric(x))
    bad <- !is.na(x) & is.na(num)
    nonmiss <- sum(!is.na(x))
    if (nonmiss == 0L || sum(bad) == 0L || (nonmiss - sum(bad)) / nonmiss >= cfg$numeric_min) {
      set(dt, j = v, value = x)
      if (any(bad)) remove_cells(dt, v, bad, stage, "non_numeric", "text in a numeric column")
      set(dt, j = v, value = suppressWarnings(as.numeric(dt[[v]])))
    } else set(dt, j = v, value = x)                 # genuinely a text column
  }
  n_nan
}

# ---- time zones (config/timezones.csv, Ovid/timezones.R) ----
# Each station keeps its times in its own zone, utc_offset_hours in station_info.csv. A row of the time-zone table says
# which zone a file's clock was in (by logger serial, model, file or folder, date); when that differs from its station's
# zone, the file's records are moved into the station's zone. This is done as a clock correction (shift), so it is in the
# audit and the cache key like one. A station with no zone is taken to be in the base zone (Settings > Time zone,
# BASE_OFFSET); its own utc_offset_hours always wins.
# A file's first reading before it is read: the TOB3 span from the scan, else the first data line of a TOA5 file.
# NA where it cannot be told (CR10X arrays: give their time-zone rows a file_pattern).
peek_first_ts <- function(path, tob3_first = NA) {
  if (!is.na(tob3_first)) return(as.POSIXct(as.numeric(tob3_first), origin = "1970-01-01", tz = "UTC"))
  l <- tryCatch(fix_enc(suppressWarnings(readLines(path, n = 12L, warn = FALSE, skipNul = TRUE, encoding = "latin1"))), error = function(e) character())
  if (length(l) < 5 || !grepl("^\"?TOA5", l[1])) return(as.POSIXct(NA, tz = "UTC"))
  for (x in l[5:length(l)]) {
    t <- normalise_timestamps(gsub('"', "", strsplit(x, "[,\t]")[[1]][1]))
    if (is.character(t)) t <- as.POSIXct(t, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
    if (length(t) && !is.na(t[1])) return(t[1])
  }
  as.POSIXct(NA, tz = "UTC")
}

zone_corrections <- function(g, tz, stations, sites, base_zone = BASE_OFFSET) {
  if (is.null(tz) || !nrow(tz) || !nrow(g)) return(NULL)
  st <- if ("src_station" %in% names(g) && nzchar(g$src_station[1])) g$src_station[1] else {
    s <- stations[logger_id == g$logger_id[1] & nzchar(station) & (table == "" | table == g$table[1])]$station; if (length(s)) s[1] else g$logger_id[1] }
  base <- suppressWarnings(as.numeric(sites[station == st]$utc_offset_hours[1]))
  if (!length(base) || is.na(base)) base <- suppressWarnings(as.numeric(base_zone[1]))   # no zone of its own: the base zone
  col <- function(v) if (v %in% names(g)) g[[v]] else rep(NA, nrow(g))
  ser <- col("serial"); mod <- col("model"); t0 <- as.POSIXct(as.numeric(col("first_ts")), origin = "1970-01-01", tz = "UTC")
  if (any(nzchar(paste0(tz$start, tz$end))) && any(is.na(t0)))   # rows with dates need each file's first reading, not known until it is read
    for (i in which(is.na(t0))) t0[i] <- peek_first_ts(g$path[i], if ("tob3_first" %in% names(g)) g$tob3_first[i] else NA)
  if (!length(base) || is.na(base)) {         # nothing to move into: say so, rather than ignore the user's row in silence
    hit <- vapply(seq_len(nrow(g)), function(i) !is.na(tz_match(tz, ser[i], g$path[i], mod[i], t0[i])), logical(1))
    if (any(hit)) problem("timezones.csv", sprintf("%d file(s) of logger %s (%s) match a time-zone row, but station %s has no utc_offset_hours in Station details and no base zone is set, so they were not moved",
                                                    sum(hit), g$logger_id[1], g$table[1], st))
    return(NULL)
  }
  out <- lapply(seq_len(nrow(g)), function(i) {
    z <- zone_for(ser[i], g$path[i], NA, tz, base, mod[i], t0[i])
    if (is.na(z$row) || z$offset == base) return(NULL)
    data.table(logger_id = g$logger_id[1], table = g$table[1], source_file = basename(g$path[i]), start = "", end = "", action = "shift",
               shift_minutes = as.character((base - z$offset) * 60), reason = sprintf("%s; clock at UTC%+g, station %s at UTC%+g", z$why, z$offset, st, base), author = "") })
  rbindlist(out)
}

# ---- clock corrections you have asked for (config/time_corrections.csv) ----
# action "shift" moves timestamps by shift_minutes; action "remove" drops the
# records. start/end refer to the timestamps as they are in the raw file.
apply_time_corrections <- function(dt, tc) {
  if (is.null(tc) || !nrow(tc)) return(dt)
  for (i in seq_len(nrow(tc))) {
    hit <- correction_span(dt$TIMESTAMP, tc$start[i], tc$end[i])$hit                       # core timestamps.R
    if (nzchar(tc$source_file[i])) hit <- hit & grepl(tc$source_file[i], basename(dt$source_file), fixed = TRUE)
    if (!any(hit)) next
    why <- paste0(tc$reason[i], if (nzchar(tc$author[i])) paste0(" [", tc$author[i], "]") else "")
    if (tolower(tc$action[i]) == "remove") {
      dt <- remove_rows(dt, hit, "stitch", "clock_fix_removed", paste0("removed by a clock correction: ", why), detail = "summary")
    } else {
      mins <- suppressWarnings(as.numeric(tc$shift_minutes[i]))
      if (is.na(mins) || mins == 0) { problem("time_corrections.csv", paste0("row ", i, ": shift_minutes is not a number; row ignored")); next }
      sm <- dt[hit, .(n = .N, t0 = fmt_ts(min(TIMESTAMP)), t1 = fmt_ts(max(TIMESTAMP))), by = source_file]
      set(dt, which(hit), "TIMESTAMP", shift_times(dt$TIMESTAMP[hit], mins))
      audit_add("stitch", "clock_fix_shifted", "change", sm$n, logger_id = dt$logger_id[1], stream = dt$table[1],
                timestamp = sm$t0, column = "TIMESTAMP",
                reason = sprintf("%s records from %s to %s moved by %+g minutes: %s", sm$n, sm$t0, sm$t1, mins, why),
                source_file = sm$source_file)
    }
  }
  dt
}

# Station-table times back to the logger's own clock, the times time_corrections.csv takes: the shifts a run applies (shift
# rows, as apply_time_corrections() applies them, in order) undone last first. files: the raw files the records came from;
# a row for one file applies only when it is one of them.
logger_time <- function(t, tc, files = character()) {
  if (is.null(tc) || !nrow(tc) || !length(t)) return(t)
  tc <- ensure_cols(as.data.table(tc), TIME_COLS)
  for (i in rev(seq_len(nrow(tc)))) {
    mins <- suppressWarnings(as.numeric(tc$shift_minutes[i]))
    if (tolower(tc$action[i]) == "remove" || is.na(mins) || mins == 0) next
    sf <- tc$source_file[i]; if (!is.na(sf) && nzchar(sf) && !any(grepl(sf, basename(files), fixed = TRUE))) next
    back <- t - mins * 60; hit <- correction_span(back, tc$start[i], tc$end[i])$hit        # where the row would have moved it from
    t[hit] <- back[hit]
  }
  t
}
# The shifts a run applies to one logger table: its rows in time_corrections.csv, then the time-zone table's (zone_corrections()),
# for the files listed in the last run's file manifest.
clock_shifts <- function(project_dir, logger_id, table, files = character()) {
  cdir <- file.path(path.expand(project_dir), "config"); rd <- function(f, cols) { p <- file.path(cdir, f)
    ensure_cols(if (file.exists(p)) tryCatch(read_cfg(p), error = function(e) data.table()) else data.table(), cols) }
  lg <- logger_id; tb <- table; files <- sub("#A[0-9_f]+$", "", files)                 # a CR10X array's records name their file with #A<n>
  tc <- rd("time_corrections.csv", TIME_COLS)[logger_id == lg & (table == "" | table == tb)]
  mf <- file.path(path.expand(project_dir), "output", "audit", "file_manifest.csv")
  g <- if (file.exists(mf)) tryCatch(fread(mf, colClasses = "character", na.strings = ""), error = function(e) data.table()) else data.table()
  if (nrow(g) && all(c("path", "logger_id", "table") %in% names(g))) {
    g <- g[logger_id == lg & table == tb & (!length(files) | basename(path) %in% basename(files))]
    if ("first_ts" %in% names(g)) g[, first_ts := as.numeric(parse_time(first_ts))]
    z <- tryCatch(zone_corrections(g, rd("timezones.csv", TZ_COLS), rd("stations.csv", STATION_COLS), rd("station_info.csv", SITE_COLS)), error = function(e) NULL)
    if (NROW(z)) tc <- rbind(tc, z, fill = TRUE)
  }
  tc
}

# ---- clock checks: things in the timestamps that suggest a clock problem ----
# Looks at records in the order the logger wrote them (after identical copies
# are gone), so a power loss or clock reset shows up as time running backwards
# or the record counter starting again.
clock_checks <- function(d, lid, tb) {
  out <- list()
  add <- function(check, sf, from, to, n, detail) out[[length(out) + 1L]] <<- data.table(logger_id = lid, table = tb, check = check,
    source_file = sf, from = fmt_ts(from), to = fmt_ts(to), records = as.integer(n), detail = detail, suggest_shift_min = NA_real_)
  rec <- if ("RECORD" %in% names(d)) suppressWarnings(as.numeric(d$RECORD)) else NULL
  for (ix in split(seq_len(nrow(d)), d$source_file)) {
    if (length(ix) < 2) next
    t <- d$TIMESTAMP[ix]; sf <- d$source_file[ix[1]]; dn <- diff(as.numeric(t))
    b <- which(dn < 0)
    if (length(b)) {
      if (length(b) <= 8) for (j in b) add("clock_went_backwards", sf, t[j + 1L], t[j], 1L,
          sprintf("after %s the next record is stamped %s, %s earlier", fmt_ts(t[j]), fmt_ts(t[j + 1L]), fmt_span(abs(dn[j]))))
      else add("clock_went_backwards", sf, min(t[b + 1L]), max(t[b]), length(b),
               sprintf("time runs backwards %d times in this file (largest jump %s); the clock was unstable", length(b), fmt_span(abs(min(dn)))))
    }
    if (!is.null(rec)) {
      r <- rec[ix]; rr <- which(diff(r) < 0)
      rr <- rr[!rr %in% b]                              # a backwards jump is already reported
      if (length(rr)) {
        if (length(rr) <= 8) for (j in rr) add("logger_restarted", sf, t[j], t[j + 1L], 1L,
            sprintf("record number went from %s to %s at %s: the logger restarted (power loss or a new program)", r[j], r[j + 1L], fmt_ts(t[j + 1L])))
        else add("logger_restarted", sf, min(t[rr]), max(t[rr + 1L]), length(rr), sprintf("the record counter restarted %d times in this file", length(rr)))
      }
    }
  }
  if (length(out)) unique(rbindlist(out), by = c("check", "from", "to")) else NULL
}

# Do the values of one file line up with the others when moved in time?
detect_clock_shift <- function(pre, step, lid, tb) {
  cf <- rbindlist(Filter(function(a) a$rule[1] == "overlap_conflict", AUDIT$log))
  if (nrow(cf) < 50 || is.na(step) || step <= 0) return(NULL)
  B <- cf[, .N, by = source_file][order(-N)]$source_file[1]
  num <- names(pre)[vapply(pre, is.numeric, logical(1))]
  v <- cf[source_file == B & column %in% num, .N, by = column][order(-N)]$column[1]
  if (is.na(v)) return(NULL)
  b <- pre[source_file == B, .(t = as.numeric(TIMESTAMP), x = get(v))][!is.na(x)]
  o <- pre[source_file != B, .(t = as.numeric(TIMESTAMP), x = get(v))][!is.na(x)]; o <- o[!duplicated(t)]
  if (nrow(b) < 30 || nrow(o) < 30) return(NULL)
  lags <- c(0, c(-48, -24, -12:-1, 1:12, 24, 48) * step)
  frac <- vapply(lags, function(L) { m <- match(b$t + L, o$t); ok <- !is.na(m)
    if (sum(ok) < 30) return(NA_real_); mean(abs(b$x[ok] - o$x[m[ok]]) <= 1e-6 * pmax(1, abs(b$x[ok]))) }, 0)
  n_cf <- cf[source_file == B, .N]; rng <- range(parse_time(cf[source_file == B, timestamp]), na.rm = TRUE)
  best <- which.max(replace(frac, 1, NA))
  base <- data.table(logger_id = lid, table = tb, source_file = B, from = fmt_ts(rng[1]), to = fmt_ts(rng[2]), records = n_cf)
  if (length(best) && !is.na(frac[best]) && frac[best] >= 0.9 && (is.na(frac[1]) || frac[1] < 0.6))
    return(cbind(base, check = "clock_differs_between_files", suggest_shift_min = lags[best] / 60,
      detail = sprintf("%s values in this file disagree with the logger's other files at the same timestamps, but %d%% agree once its records are moved by %+g minutes (checked on %s)",
                       format(n_cf, big.mark = ","), round(100 * frac[best]), lags[best] / 60, v)))
  cbind(base, check = "files_disagree", suggest_shift_min = NA_real_,
        detail = sprintf("%s values in this file disagree with the logger's other files at the same timestamps and no time shift up to a day explains it; this may be a different logger filed under the same id, or an edited copy", format(n_cf, big.mark = ",")))
}

# Corrupt timestamps among good records: stray_records() in the core (Ovid/timestamps.R).

stitch_group <- function(g, cfg, tc = NULL) {
  parts <- lapply(seq_len(nrow(g)), function(i) tryCatch(
    if (g$format[i] %in% "mixed-array") { f <- get0(g$src[i], envir = ARRAY_CACHE, inherits = FALSE)
      if (is.null(f) || !file.exists(f)) NULL else { d <- readRDS(f); unlink(f); d } }
    else if (g$format[i] %in% "TOB3") read_tob3(g$path[i], g$mtime[i])
    else read_toa5(g$path[i], g$columns[[i]], g$mtime[i], g$sep[i]),
    error = function(e) { problem(basename(g$path[i]), paste("could not be read and was left out:", conditionMessage(e))); NULL }))
  empty <- vapply(parts, is.null, logical(1))
  dt <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(list(data = NULL, empty = g$path[empty]))
  n_read <- nrow(dt); lid <- g$logger_id[1]; tb <- g$table[1]
  read_by_file <- dt[, .(read = .N), by = source_file]          # for the per-file audit: read = in output + dropped
  dt[, `:=`(logger_id = lid, table = tb)]
  checks <- list()

  # --- timestamps ---------------------------------------------------------
  raw_ts <- fix_enc(dt$TIMESTAMP)
  set(dt, j = ".raw_ts", value = raw_ts)
  set(dt, j = "TIMESTAMP", value = as.POSIXct(raw_ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"))
  dt <- apply_time_corrections(dt, tc)                 # your fixes first, so a logger that woke up in 1990 can be moved back
  yr <- as.integer(format(dt$TIMESTAMP, "%Y", tz = "UTC"))
  bad <- is.na(dt$TIMESTAMP) | (!isFALSE(cfg$drop_bad_clock) & (yr < cfg$min_year | dt$TIMESTAMP > Sys.time() + 2 * 86400))   # switch: Automatic corrections
  if (any(bad)) {
    b <- dt[bad]
    audit_add("stitch", "bad_timestamp", "row", 1L, logger_id = b$logger_id, stream = b$table,
              timestamp = b$.raw_ts, record = .col(b, "RECORD", TRUE), column = "<record>",
              old_value = b$.raw_ts, reason = "timestamp unreadable or outside plausible range",
              source_file = b$source_file)
    bs <- b[, .(n = .N, a = min(.raw_ts), z = max(.raw_ts), unread = sum(is.na(TIMESTAMP))), by = source_file]
    checks[[1]] <- data.table(logger_id = lid, table = tb, check = "impossible_date", source_file = bs$source_file,
      from = bs$a, to = bs$z, records = bs$n, suggest_shift_min = NA_real_,
      detail = sprintf("%s records are stamped outside %d to today (%s to %s)%s; they were left out. A logger that lost power often restarts at a default date",
                       format(bs$n, big.mark = ","), cfg$min_year, substr(bs$a, 1, 16), substr(bs$z, 1, 16),
                       ifelse(bs$unread > 0, sprintf(", %d unreadable", bs$unread), "")))
    dt <- dt[!bad]
  }
  dt[, .raw_ts := NULL]
  if (!nrow(dt)) return(list(data = NULL, empty = g$path[empty], checks = rbindlist(checks, fill = TRUE), read_by_file = read_by_file))

  n_nan <- to_numeric_columns(dt, cfg, "stitch")
  spans <- dt[, .(first_ts = min(TIMESTAMP), last_ts = max(TIMESTAMP), rows = .N), by = source_file]

  # --- stitch -------------------------------------------------------------
  dt[, .ord := seq_len(.N)]
  # "newest" is the download whose record runs latest (its own last time stamp), not the file's modification time, which
  # changes when files are copied to another drive; the modification time only where a file has no time stamps
  dt[, .mtime := { m <- suppressWarnings(max(as.numeric(TIMESTAMP), na.rm = TRUE)); if (is.finite(m)) m else as.numeric(.mtime[1]) }, by = source_file]
  setorderv(dt, c("TIMESTAMP", ".mtime"), c(1L, if (cfg$prefer == "newest") -1L else 1L))
  dup <- duplicated(dt, by = setdiff(names(dt), c("source_file", ".mtime", ".ord")))
  dt  <- remove_rows(dt, dup, "stitch", "duplicate_record",
                     "identical copy of a record that is kept", detail = "summary")
  checks[[2]] <- try_or(clock_checks(dt[order(.ord)], lid, tb), NULL, paste0(lid, " ", tb, ": clock checks not done"))
  if (!isFALSE(cfg$drop_stray_records)) {
    sr <- try_or(stray_records(dt$TIMESTAMP, dt$source_file, dt$.ord), logical(nrow(dt)), paste0(lid, " ", tb, ": stray-record check not done"))
    if (any(sr)) {
      ss <- dt[sr, .(n = .N, a = min(TIMESTAMP), z = max(TIMESTAMP)), by = source_file]
      checks[[5]] <- data.table(logger_id = lid, table = tb, check = "stray_records", source_file = ss$source_file,
        from = fmt_ts(ss$a), to = fmt_ts(ss$z), records = ss$n, suggest_shift_min = NA_real_,
        detail = sprintf("%s records with corrupt timestamps (off the time grid and out of written order) were found among good records and left out", format(ss$n, big.mark = ",")))
      dt <- remove_rows(dt, sr, "stitch", "stray_record", STRAY_REASON, detail = "summary")
    }
  }
  dt[, .ord := NULL]
  pre <- dt
  dt  <- resolve_overlaps(dt, "stitch", "overlap_conflict")
  setcolorder(dt, intersect(c("logger_id", "table", "TIMESTAMP", "RECORD"), names(dt)))

  # --- gaps and interval --------------------------------------------------
  gaps <- NULL; step <- NA_real_
  if (nrow(dt) > 2) {
    dd <- diff(as.numeric(dt$TIMESTAMP))
    step <- as.numeric(names(which.max(table(dd))))
    gaps <- gap_rows(dt$TIMESTAMP, step)                               # core logger_files.R
    if (!is.null(gaps)) gaps <- gaps[, .(logger_id = lid, table = tb, gap_start, gap_end, gap_hours = round(gap_s / 3600, 2), missing_records)]
    if (step >= 60) {                                   # records that are not on the logger's time grid
      ph <- as.numeric(dt$TIMESTAMP) %% step; home <- as.numeric(names(which.max(table(ph))))
      off <- ph != home
      if (any(off) && mean(off) < 0.5) {
        os <- dt[off, .(n = .N, a = min(TIMESTAMP), z = max(TIMESTAMP)), by = source_file]
        checks[[3]] <- data.table(logger_id = lid, table = tb, check = "off_the_time_grid", source_file = os$source_file,
          from = fmt_ts(os$a), to = fmt_ts(os$z), records = os$n, suggest_shift_min = NA_real_,
          detail = sprintf("%s records are not on the %s grid the rest of the table follows (for example %s); the clock was off or was being set",
                           format(os$n, big.mark = ","), fmt_span(step), fmt_ts(os$a)))
      }
    }
  }
  checks[[4]] <- try_or(detect_clock_shift(pre, step, lid, tb), NULL, paste0(lid, " ", tb, ": clock-shift check not done"))
  list(data = dt, spans = spans, gaps = gaps, step = step, n_read = n_read, n_nan = n_nan, read_by_file = read_by_file,
       empty = g$path[empty], checks = rbindlist(checks, fill = TRUE))
}

# What a logger table's stitched record depends on. Unchanged key = the cached record can be reused.
# STEP_HASH$stitch (cs_pipeline.R) changes whenever the code that stitches does, so a cached result is never older than it.
CACHE_VERSION <- 4L

cache_key <- function(g, tc, cfg) {
  f <- tempfile(); on.exit(unlink(f))
  saveRDS(list(CACHE_VERSION, STEP_HASH$stitch, as.list(g[, intersect(c("path", "size", "mtime", "logger_id", "table", "station_name", "src_station", "src_stream", "format", "sep", "status"), names(g)), with = FALSE]),
               g$columns, as.list(tc), cfg[c("min_year", "sentinels", "prefer", "numeric_min")],
               if (isFALSE(cfg$drop_bad_clock)) list(drop_bad_clock = FALSE)), f, version = 2)   # a switch turned off counts; left on, keys stay as they were
  substr(unname(tools::md5sum(f)), 1, 16)
}


# What one station table depends on besides the code: the records that reached it (hash of each piece), and of the
# quality rules, removals and corrections over a period only the rows that apply to it or its loggers and that name one of its
# parameters (cols: its columns; NULL keeps every row that applies), its site row and the settings. So a rule, removal or
# calibration redoes only the station tables it can change. Groups and the parameter guide are used after the stations.
station_cache_key <- function(kk, piece_hashes, loggers, cfgs, cfg, cols = NULL) {
  st <- sub("__.*$", "", kk); sm <- sub("^.*__", "", kk); who <- unique(c(st, loggers))
  hits <- function(a) { a <- as.character(a); a[is.na(a)] <- ""
    vapply(strsplit(a, "|", fixed = TRUE), function(w) { w <- trim(w); !any(nzchar(w)) || any(w %in% who) }, logical(1)) }
  any_of <- function(rx, v) vapply(rx, function(p) { p <- if (is.na(p)) "" else p      # as apply_qc() matches; a bad pattern counts
    !nzchar(p) || isTRUE(tryCatch(any(grepl(p, v, ignore.case = TRUE, perl = TRUE)), error = function(e) TRUE)) }, logical(1), USE.NAMES = FALSE)
  qc <- cfgs$qc[hits(applies_to)]
  m <- cfgs$manual[hits(station) & (stream == "" | stream == sm)]
  pc <- period_for(cfgs$period, who)                                                     # corrections over a period (core periods.R)
  if ("stream" %in% names(pc)) pc <- pc[!nzchar(stream) | stream == sm]
  if (!is.null(cols)) {
    if (NROW(pc)) pc <- pc[vapply(parameter, function(x) { w <- period_parts(x); !length(w) || "*" %in% w || any(w %in% cols) }, NA)]   # period_apply() takes exact names
    vals <- unique(c(cols, trim(pc$output[nzchar(trim(pc$output %||% character()))])))   # a row may write a new column (output)
    qc <- qc[any_of(pattern, vals)]
    m <- m[parameter %in% c("", "*") | any_of(paste0("^(", parameter, ")$"), vals)]
  }
  site_row <- cfgs$sites[station == st]; site_row <- site_row[rowSums(as.matrix(site_row[, !"station"]) != "", na.rm = TRUE) > 0]   # a row added with only the name counts as none
  m <- if (NROW(m)) m[, setdiff(names(m), "row"), with = FALSE]; pc <- if (NROW(pc)) pc[, setdiff(names(pc), "row"), with = FALSE]   # none: the same however the table looks
  hash_of(list(CACHE_VERSION, STEP_HASH$station, piece_hashes, qc, m, site_row, pc,
               cfg[setdiff(names(cfg), c("raw_dirs", "project_dir", "incremental", "write_stitched"))]))
}

# The files a station table wrote (relative to output/), so a reused station can be checked and old ones removed.
station_outputs <- function(out, kk) {
  s <- safe_name(kk)                                   # letters, digits, _ and - only: safe inside a pattern
  ls <- function(d, re) { f <- list.files(file.path(out, d), paste0(re, "$")); if (length(f)) file.path(d, f) else character() }
  c(ls("stations", paste0("^", s, "\\.[a-z]+")), ls("derived", paste0("^", s, "_(daily|monthly|water_year)\\.csv")),
    ls("qc", paste0("^", s, "_removed\\.csv")), ls("plots", paste0("^station_", s, "\\.png")))
}

# "Rescan": forgets what the cache holds for these files (their headers and checksums, and the stitched record of
# their logger tables), so the next run reads them again. Stations are rebuilt only if their records changed.
forget_files <- function(project_dir, paths, logger_id, table) {
  cd <- file.path(path.expand(project_dir), "cache"); k <- !is.na(logger_id) & !is.na(table)
  pre <- unique(paste0(safe_name(paste(logger_id[k], table[k], sep = "__")), "__")); n <- 0L
  for (d in c("stitch", "pieces")) { f <- list.files(file.path(cd, d))
    hit <- f[vapply(f, function(x) any(startsWith(x, pre)), logical(1))]; unlink(file.path(cd, d, hit)); n <- n + length(hit) }
  file_known_drop(file.path(cd, "headers.rds"), paths)              # core logger_files.R
  invisible(n)
}
