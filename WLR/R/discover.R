# R/discover.R  --  Finding files, reading their identity, pairing .hobo / .xle with their CSV exports,
# spotting duplicates, and applying the user's file sorting rules.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# Folder layout expected (any depth below each raw folder):
#   <raw>/2025/...          water level and conductivity loggers for that year
#   <raw>/baro/2025/...     barologgers for that year
# A file is a barologger record when "baro" is one of its folder names, or when the metadata calls its serial a Baro.

FILE_CACHE <- new.env()            # md5 -> parsed file, so each file is read once per run

READ_CACHE <- new.env()            # dir (NULL = no disk cache), key (code and base zone), used, hits: set by run_pipeline()
read_cached <- function(path, md5, format) {
  if (!is.null(FILE_CACHE[[md5]])) return(FILE_CACHE[[md5]])
  d <- READ_CACHE$dir; nm <- paste(md5, format, sep = "_")
  r <- if (!is.null(d)) cache_read(d, nm, READ_CACHE$key) else NULL          # a file read in an earlier run, unchanged since
  if (!is.null(r)) READ_CACHE$hits <- READ_CACHE$hits + 1L
  else { r <- READ_CACHE$pre[[nm]]; if (!is.null(r)) rm(list = nm, envir = READ_CACHE$pre)       # read ahead in parallel (read_ahead)
    if (is.null(r)) r <- read_logger_file(path, format)
    if (!is.null(d)) cache_write(d, nm, READ_CACHE$key, r) }
  if (!is.null(d)) READ_CACHE$used <- c(READ_CACHE$used, cache_file(d, nm, READ_CACHE$key))
  FILE_CACHE[[md5]] <- r; r
}

# The files of m that are neither in memory nor in the cache are read in parallel (core util.R par_map) into READ_CACHE$pre,
# where read_cached() then finds them; a file that fails here is read again there, and its error recorded as before.
read_ahead <- function(m) {
  READ_CACHE$pre <- new.env()
  i <- which(m$status == "ok" & m$format != "hobo_binary" & !duplicated(m$md5))
  nm <- paste(m$md5[i], m$format[i], sep = "_")
  i <- i[vapply(seq_along(i), function(k) is.null(FILE_CACHE[[m$md5[i[k]]]]) && (is.null(READ_CACHE$dir) || !file.exists(cache_file(READ_CACHE$dir, nm[k], READ_CACHE$key))), NA)]
  if (!length(i)) return(invisible(0L))
  got <- par_map(i, function(k) read_logger_file(m$path[k], m$format[k]), what = "reading files")
  for (k in seq_along(i)) if (!inherits(got[[k]], "error")) assign(paste(m$md5[i[k]], m$format[i[k]], sep = "_"), got[[k]], envir = READ_CACHE$pre)
  invisible(length(i))
}

# Rescan: forgets the selected files as read (and the files paired with them), and their loggers as stitched and
# compensated, so the next run reads and redoes them.
forget_wlr_files <- function(project_dir, rels) {
  # safe: forgetting files needs the last run's list; with none there is nothing to forget
  pd <- path.expand(project_dir); fa <- tryCatch(fread(file.path(pd, "output", "audit", "files.csv"), colClasses = "character"), error = function(e) data.table())
  if (!nrow(fa) || !all(c("rel", "md5", "path", "pair") %in% names(fa))) return(invisible(0L))
  hit <- fa[rel %in% rels]; hit <- rbind(hit, fa[path %in% hit$pair | pair %in% hit$path])
  f <- list.files(file.path(pd, "cache", "read"), full.names = TRUE)
  gone <- f[substr(basename(f), 1, 32) %in% unique(hit$md5)]          # cache names start with the md5
  unlink(gone)
  if ("serial" %in% names(hit)) { sn <- unique(hit$serial[!is.na(hit$serial) & nzchar(hit$serial)])
    for (d in c("stitch", "logger")) cache_forget(file.path(pd, "cache", d), sn) }
  invisible(length(gone))
}

year_of_path <- function(rel) { p <- strsplit(rel, "[/\\\\]")[[1]]; y <- rev(p[grepl("^(19|20)[0-9]{2}$", p)])[1]; if (is.na(y)) NA_integer_ else as.integer(y) }

# The raw files a run reads: these extensions, without the ._ and ~$ companions (core logger_files.R RAW_SKIP_FILES).
RAW_PATTERN <- "\\.(csv|txt|xle|hobo)$"
raw_files_found <- function(dirs) raw_file_list(dirs, RAW_PATTERN)   # path, rel, ...: the same count on Home, the Site page and the Workbench

discover_files <- function(cfg) {
  for (root in path.expand(cfg$raw_dirs)) if (!dir.exists(root)) problem("raw folder", paste(root, "does not exist"))
  m <- raw_file_list(cfg$raw_dirs, RAW_PATTERN)                                         # core logger_files.R
  if (!nrow(m)) stop("No .csv, .xle or .hobo files were found in the raw folders.")
  m[, c("file", "size", "mtime") := NULL]                                             # set again below, in the order files.csv has always had
  m[, `:=`(name = basename(path), ext = tolower(tools::file_ext(path)), size = file.info(path)$size,
           kind = ifelse(vapply(strsplit(tolower(rel), "[/\\\\]"), function(p) "baro" %in% p, logical(1)), "baro", "water"),
           year = vapply(rel, year_of_path, 1L))]
  m[, md5 := raw_md5(path)]
  m[, format := vapply(path, sniff_format, "")]
  m[, `:=`(status = "ok", dup_of = NA_character_, serial = NA_character_, model = NA_character_, title = NA_character_,
           offset_hours = NA_real_, unit_pres = NA_character_, unit_temp = NA_character_, unit_ec = NA_character_,
           first_ts = as.POSIXct(NA, tz = "UTC"), last_ts = as.POSIXct(NA, tz = "UTC"), rows = NA_integer_, interval_s = NA_real_,
           n_events = 0L, pair = NA_character_, use = FALSE, tz_override = NA_real_, altitude_m = NA_real_, note = "")]
  m[size == 0, status := "empty_file"]
  m[format %in% c("unrecognised"), status := "unrecognised_format"]
  setorder(m, kind, year, path)
  m[, dup_of := copy_of(md5, path, status == "ok")][!is.na(dup_of), status := "duplicate_file"]
  read_ahead(m)
  for (i in which(m$status == "ok")) {
    r <- tryCatch(if (m$format[i] == "hobo_binary") read_logger_file(m$path[i], "hobo_binary") else read_cached(m$path[i], m$md5[i], m$format[i]),
                  error = function(e) { set(m, i, "note", conditionMessage(e)); NULL })
    if (is.null(r)) { set(m, i, "status", "unreadable"); next }
    mt <- r$meta; unread_report(mt, m$rel[i])                                # cells that were not numbers (core audit.R)
    if (isTRUE(mt$cut_last)) problem(m$rel[i], "the file ends inside its last row (cut off): that row was left out")
    if (!is.null(mt$header_change)) problem(m$rel[i], sprintf("a second header with other columns at line %d: the %d rows after it were left out (export them as a file of their own)", mt$header_change$line, mt$header_change$rows))
    set(m, i, "serial", mt$serial %||% NA); set(m, i, "model", mt$model %||% NA); set(m, i, "title", mt$title %||% NA)
    set(m, i, "offset_hours", mt$offset_hours %||% NA_real_); set(m, i, "altitude_m", as.numeric(mt$altitude_m %||% NA_real_))
    set(m, i, "unit_pres", unname(mt$units["pres"]) %||% NA); set(m, i, "unit_temp", unname(mt$units["temp"]) %||% NA)
    set(m, i, "unit_ec", unname(mt$units["ec"]) %||% NA)
    if (!is.null(r$data)) {
      set(m, i, "rows", nrow(r$data)); set(m, i, "interval_s", mt$interval_s %||% NA_real_); set(m, i, "n_events", nrow(r$events))
      if (nrow(r$data)) { set(m, i, "first_ts", min(r$data$TIMESTAMP)); set(m, i, "last_ts", max(r$data$TIMESTAMP)) }
      if (!nrow(r$data)) set(m, i, "status", "no_data_rows")
    }
    if (!nzchar(m$serial[i] %||% "") || is.na(m$serial[i])) set(m, i, "status", if (m$status[i] == "ok") "no_serial" else m$status[i])
  }
  READ_CACHE$pre <- NULL
  # a HOBO CSV with temperature but no pressure or conductivity channel is a ground-temperature logger: GT reads it, not WLR
  m[status == "ok" & format == "hobo_csv" & is.na(unit_pres) & is.na(unit_ec) & !is.na(unit_temp), status := "temperature_logger"]
  pair_files(m)
  m
}

# .hobo -> its HOBOware CSV (same serial; same file stem first, then same plot title, then any CSV of that serial).
#          A .hobo paired with a temperature logger's CSV, or of a temperature-only model with no CSV, is a temperature logger too.
# .xle  -> its Solinst CSV (same serial and same first and last time stamps); the .xle supplies the data, the CSV is
#          a cross-check. A Solinst CSV with no .xle supplies the data itself.
HOBO_TEMP_MODELS <- "U12|U23|UA-00[12]|UX100|MX220[12]|TidbiT|Pendant"   # temperature-only HOBO models
pair_files <- function(m) {
  stem <- function(x) tolower(tools::file_path_sans_ext(basename(x)))
  ok <- m$status == "ok"
  m[ok & format %in% c("hobo_csv", "solinst_xle"), use := TRUE]
  hc <- which((ok | m$status == "temperature_logger") & m$format == "hobo_csv")
  for (i in which(ok & m$format == "hobo_binary")) {
    cand <- hc[m$serial[hc] == m$serial[i]]
    pick <- cand[stem(m$path[cand]) == stem(m$path[i])]
    if (!length(pick)) pick <- cand[!is.na(m$title[cand]) & trim(m$title[cand]) == trim(m$title[i] %||% "")]
    if (!length(pick)) pick <- cand
    if (length(pick)) { set(m, i, "pair", m$path[pick[1]]); set(m, i, "status", if (m$status[pick[1]] == "temperature_logger") "temperature_logger" else "paired_hobo")
      if (is.na(m$model[pick[1]])) set(m, pick[1], "model", m$model[i])
      if (is.na(m$pair[pick[1]])) set(m, pick[1], "pair", m$path[i]) }
    else if (grepl(HOBO_DECODE_MODELS, m$model[i] %||% "")) { set(m, i, "format", "hobo_decoded"); set(m, i, "use", TRUE)     # read directly (core hobo.R)
      r <- tryCatch(read_cached(m$path[i], m$md5[i], "hobo_decoded"), error = function(e) { set(m, i, "note", conditionMessage(e)); NULL })   # its units (kPa, C), times and rows,
      if (is.null(r)) { set(m, i, "status", "unreadable"); set(m, i, "use", FALSE); next }                                                  # as a CSV's come from the CSV
      set(m, i, "unit_pres", unname(r$meta$units["pres"])); set(m, i, "unit_temp", unname(r$meta$units["temp"])); set(m, i, "rows", nrow(r$data)); set(m, i, "interval_s", r$meta$interval_s %||% NA_real_)
      if (nrow(r$data)) { set(m, i, "first_ts", min(r$data$TIMESTAMP)); set(m, i, "last_ts", max(r$data$TIMESTAMP)) }
      set(m, i, "note", trim(paste(m$note[i] %|% "", "no CSV export: records read from the .hobo file itself"))) }
    else set(m, i, "status", if (grepl(HOBO_TEMP_MODELS, m$model[i] %||% "")) "temperature_logger" else "hobo_without_csv")
  }
  xl <- which(ok & m$format == "solinst_xle")
  for (i in which(ok & m$format == "solinst_csv")) {
    cand <- xl[m$serial[xl] == m$serial[i] & abs(as.numeric(m$first_ts[xl]) - as.numeric(m$first_ts[i])) < 1 &
               abs(as.numeric(m$last_ts[xl]) - as.numeric(m$last_ts[i])) < 1]
    if (length(cand)) {
      set(m, i, "pair", m$path[cand[1]]); set(m, i, "status", "companion_csv")
      if (is.na(m$pair[cand[1]])) set(m, cand[1], "pair", m$path[i])
      if (!is.na(m$rows[i]) && !is.na(m$rows[cand[1]]) && m$rows[i] != m$rows[cand[1]])
        set(m, i, "note", sprintf("row count differs from its .xle (%d vs %d)", m$rows[i], m$rows[cand[1]]))
    } else set(m, i, "use", TRUE)
  }
  for (i in xl) if (is.na(m$pair[i])) set(m, i, "note", trim(paste(m$note[i], "no matching CSV export")))
  invisible(m)
}

# User rules from the Sort files page: ignore a file, give it a different serial, or keep one of a set of duplicates.
FILE_RULE_COLS <- c("file", "action", "serial", "offset_hours", "reason", "author")
apply_file_rules <- function(m, rules) {
  if (is.null(rules) || !nrow(rules)) return(m)
  for (k in seq_len(nrow(rules))) {
    f <- rules$file[k]; hit <- if (grepl("[/\\\\]", f)) m$path == f | m$rel == f else m$name == f
    if (!any(hit)) next
    a <- tolower(rules$action[k]); why <- paste0(rules$reason[k], if (nzchar(rules$author[k] %||% "")) paste0(" [", rules$author[k], "]") else "")
    if (a == "ignore") {
      audit_add("discover", "excluded_by_user", "file", NA, logger_id = m$serial[hit], column = "<file>",
                reason = paste("left out on the Sort files page:", why), source_file = m$path[hit])
      m[hit, `:=`(use = FALSE, status = "excluded_by_user")]
    } else if (a == "serial") {
      m[hit, `:=`(note = trim(paste(note, sprintf("serial set to %s by hand (was %s)", rules$serial[k], serial))), serial = norm_serial(rules$serial[k]))]
      m[hit & status == "no_serial" & format != "hobo_binary", `:=`(status = "ok", use = TRUE)]   # a file that had no serial is read once it has one
    } else if (a == "use") {
      m[hit & status %in% c("companion_csv", "paired_hobo", "ok"), `:=`(use = format != "hobo_binary", status = "ok")]
    } else if (a == "tz") {
      off <- suppressWarnings(as.numeric(rules$offset_hours[k]))
      if (is.na(off)) problem("file_rules.csv", paste0("row ", k, ": action tz needs offset_hours, e.g. -6"))
      else m[hit, `:=`(tz_override = off, note = trim(paste(note, sprintf("time zone set to UTC%+g by hand", off))))]
    } else if (a == "baro") m[hit, kind := "baro"] else if (a == "water") m[hit, kind := "water"]
    else problem("file_rules.csv", paste0("row ", k, ": action must be ignore, serial, use, tz, baro or water"))
  }
  m
}

# The zone each file's clock is read in: a zone set by hand for the file (Sorting rules) wins, then the zone the file
# states (HOBOware header), then the first matching row of the time-zone table (Clock checks), then the base zone.
# tz_table holds the table's zone only where it is used, so stitch_serial() can apply it like a hand-set zone.
apply_zone_table <- function(m, tz, canon = identity, unzoned = NA) {
  if (!is.null(tz) && nrow(tz)) { tz <- copy(tz); tz[nzchar(trim(serial)), serial := canon(norm_serial(serial))] }
  m[, `:=`(tz_table = NA_real_, zone = NA_real_, zone_why = "")]
  for (i in seq_len(nrow(m))) {
    if (!is.na(m$tz_override[i])) { set(m, i, "zone", m$tz_override[i]); set(m, i, "zone_why", "set by hand for this file (Sorting rules)"); next }
    z <- zone_for(m$serial[i], m$rel[i], m$offset_hours[i], tz, BASE_OFFSET, model = m$model[i], first_ts = m$first_ts[i], stated_wins = TRUE, unzoned = unzoned)
    set(m, i, "zone", z$offset); set(m, i, "zone_why", z$why)
    if (!is.na(z$row) || startsWith(z$why, "no zone stated")) set(m, i, "tz_table", z$offset)   # applied like a table row: the clock is read in that zone
  }
  m
}

# Groups of files that hold the same logger over overlapping times: shown on the Sort files page.
near_duplicates <- function(m) {
  u <- m[use == TRUE & !is.na(first_ts)]
  if (!nrow(u)) return(data.table())
  setorder(u, serial, first_ts)
  u[, grp := cumsum(c(TRUE, as.numeric(first_ts[-1]) > cummax(as.numeric(last_ts))[-.N])), by = serial]
  g <- u[, if (.N > 1) .(files = .N, names = paste(name, collapse = " | "), first = min(first_ts), last = max(last_ts),
                          rows = paste(rows, collapse = " | ")), by = .(serial, grp)]
  g[, grp := NULL][]
}
