# R/preflight.R  --  Preflight: check the set-up tables before running.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# Returns one row per finding: level is "error" (the run would ignore or misapply something) or "warning"
# (worth a look). No finding = safe to run. Nothing is changed.
preflight <- function(cfg = CFG) {
  pdir <- path.expand(trim(cfg$project_dir)); cdir <- file.path(pdir, "config"); config_migrate(pdir, "preflight")   # older tables moved in once (core config.R)
  chk <- check_collector(c("level", "table", "row", "problem", "fix")); add <- chk$add
  rd <- function(f) { p <- file.path(cdir, f); if (!file.exists(p)) return(NULL); tryCatch(read_cfg(p), error = function(e) { add("error", f, "", paste("the table cannot be read:", conditionMessage(e)), "Open it in a spreadsheet and fix it, or go back to an earlier version on the History page."); NULL }) }
  bad_rx <- bad_regex; numbad <- bad_number; datebad <- function(x) bad_date(x, parse_time)
  span <- function(s, e) { a <- parse_time(s); b <- parse_time(e); b <- fifelse(!is.na(b) & nchar(e) <= 10, b + 86399, b)
    list(a = fifelse(is.na(a), as.POSIXct("1900-01-01", tz = "UTC"), a), b = fifelse(is.na(b), as.POSIXct("2200-01-01", tz = "UTC"), b)) }
  # folders
  check_folders(add, cfg$raw_dirs, cfg$project_dir, what = "folders")
  # patterns
  for (f in c("aliases.csv", "qc_rules.csv", "groups.csv", "parameter_info.csv")) { d <- rd(f); if (is.null(d) || !"pattern" %in% names(d)) next
    for (i in which(bad_rx(d$pattern))) add("error", f, i, paste0("pattern '", d$pattern[i], "' is not a valid regular expression; the row is ignored"), "Fix the pattern. Special characters such as ( [ + need a backslash in front when meant literally.")
    k <- d$pattern[nzchar(d$pattern)]; scope <- if ("logger_id" %in% names(d)) d$logger_id[nzchar(d$pattern)] else if ("applies_to" %in% names(d)) d$applies_to[nzchar(d$pattern)] else ""
    dup <- duplicated(paste(tolower(k), scope)); for (x in unique(k[dup])) add("warning", f, paste(which(d$pattern == x), collapse = ", "), paste0("pattern '", x, "' appears more than once; only the first row is used"), "Delete the extra row.") }
  al <- rd("aliases.csv")
  if (!is.null(al) && nrow(al)) { for (i in which(nzchar(al$pattern) & !nzchar(al$parameter))) add("error", "aliases.csv", i, "the row has a pattern but no parameter name", "Give the common name, or delete the row.")
    lit <- al[!grepl("\\\\[0-9]|\\\\L|\\\\U", parameter) & nzchar(parameter) & !bad_rx(pattern)]
    # two different raw patterns that give the same name are fine; one raw column that matches two rows with different names is not checked here (first match wins)
  }
  qc <- rd("qc_rules.csv")
  if (!is.null(qc) && nrow(qc)) { for (v in intersect(c("min", "max", "max_spike", "max_flat_hours"), names(qc))) for (i in which(numbad(qc[[v]]))) add("error", "qc_rules.csv", i, paste0(v, " = '", qc[[v]][i], "' is not a number; that limit is ignored"), "Type a number or leave it blank.")
    lo <- suppressWarnings(as.numeric(qc$min)); hi <- suppressWarnings(as.numeric(qc$max))
    for (i in which(!is.na(lo) & !is.na(hi) & lo >= hi)) add("error", "qc_rules.csv", i, paste0("min (", qc$min[i], ") is not below max (", qc$max[i], "): every value of ", qc$pattern[i], " would be removed"), "Swap or correct the limits.")
    for (v in intersect(c("max_spike", "max_flat_hours"), names(qc))) for (i in which(suppressWarnings(as.numeric(qc[[v]])) <= 0)) add("error", "qc_rules.csv", i, paste0(v, " must be greater than zero"), "Use a positive number or leave it blank.")
    if ("action" %in% names(qc)) for (i in which(!tolower(qc$action) %in% c("", "flag", "remove"))) add("error", "qc_rules.csv", i, paste0("action '", qc$action[i], "' is not known"), "Use flag, remove, or leave it blank.") }
  # stations: bad dates, and two rows that send the same records to different stations
  st <- rd("stations.csv")
  if (!is.null(st) && nrow(st)) { st <- ensure_cols(st, STATION_COLS)
    for (v in c("start", "end")) for (i in which(datebad(st[[v]]))) add("error", "stations.csv", i, paste0(v, " '", st[[v]][i], "' is not a date; it is ignored"), "Use YYYY-MM-DD.")
    sp <- span(st$start, st$end); for (i in which(sp$a > sp$b)) add("error", "stations.csv", i, "start is after end", "Swap the dates.")
    a <- st[, .(i = .I, logger_id, header_name, table, station, stream, a = sp$a, b = sp$b)][nzchar(station) | nzchar(stream)]
    if (nrow(a) > 1) { x <- merge(a, a, by = c("logger_id", "header_name", "table"), allow.cartesian = TRUE)[i.x < i.y & a.x <= b.y & a.y <= b.x & (station.x != station.y | stream.x != stream.y)]
      for (k in seq_len(nrow(x))) add("error", "stations.csv", paste(x$i.x[k], x$i.y[k], sep = ", "), paste0("logger ", x$logger_id[k], if (nzchar(x$table[k])) paste0(" table ", x$table[k]) else "", " is sent to both '", x$station.x[k], "' and '", x$station.y[k], "' over the same dates; the later row wins"), "Give the rows date ranges that do not overlap, or delete one.") }
    for (i in which(grepl("[/\\\\:*?\"<>|]", st$station))) add("warning", "stations.csv", i, paste0("station name '", st$station[i], "' has characters that are replaced in file names"), "Letters, digits, - and _ are safest.") }
  # time corrections
  tc <- rd("time_corrections.csv")
  if (!is.null(tc) && nrow(tc)) { tc <- ensure_cols(tc, TIME_COLS)
    for (v in c("start", "end")) for (i in which(datebad(tc[[v]]))) add("error", "time_corrections.csv", i, paste0(v, " '", tc[[v]][i], "' is not a date and time"), "Use YYYY-MM-DD HH:MM.")
    for (i in which(!tolower(tc$action) %in% c("shift", "remove", ""))) add("error", "time_corrections.csv", i, paste0("action '", tc$action[i], "' is not known"), "Use shift or remove.")
    sh <- tolower(tc$action) != "remove"; for (i in which(sh & (numbad(tc$shift_minutes) | !nzchar(tc$shift_minutes) | suppressWarnings(as.numeric(tc$shift_minutes)) %in% 0))) add("error", "time_corrections.csv", i, "a shift needs a number of minutes other than zero; the row is ignored", "Fill in shift_minutes.")
    for (i in which(!nzchar(tc$logger_id))) add("error", "time_corrections.csv", i, "no logger_id, so the correction applies to nothing", "Fill in the logger.")
    sp <- span(tc$start, tc$end); a <- tc[, .(i = .I, logger_id, table, source_file, action = tolower(action), a = sp$a, b = sp$b)][action != "remove"]
    if (nrow(a) > 1) { x <- merge(a, a, by = "logger_id", allow.cartesian = TRUE)[i.x < i.y & a.x <= b.y & a.y <= b.x & (table.x == "" | table.y == "" | table.x == table.y) & (source_file.x == "" | source_file.y == "" | source_file.x == source_file.y)]
      for (k in seq_len(nrow(x))) add("warning", "time_corrections.csv", paste(x$i.x[k], x$i.y[k], sep = ", "), paste0("two shifts for logger ", x$logger_id[k], " cover the same period; records there are moved twice"), "Make the periods meet without overlapping, or merge the rows.") } }
  tzc <- tz_check(rd("timezones.csv")); for (i in seq_len(nrow(tzc))) add("error", "timezones.csv", tzc$row[i], tzc$problem[i], "Fix it under Time zones on the Clock checks page.")   # core timezones.R
  # corrections over a period (removals and calibrations among them): rows that cannot be used, then the removals as before
  pr <- period_read(file.path(cdir, PERIOD_FILE)); for (i in seq_len(nrow(pr$bad))) add("error", PERIOD_FILE, pr$bad$row[i], paste0(pr$bad$problem[i], "; the row is not applied"), "Fix the row, or set on to off.")
  mn <- cs_period_to_manual(period_own_rows(file.path(cdir, PERIOD_FILE), "remove")); mi <- mn$row
  if (nrow(mn)) { for (i in which(bad_rx(ifelse(mn$parameter %in% c("", "*"), "", mn$parameter)))) add("error", PERIOD_FILE, mi[i], paste0("parameter '", mn$parameter[i], "' is not a valid pattern; the row is ignored"), "Fix the name.")
    for (i in which(!nzchar(mn$start) & !nzchar(mn$end) & !nzchar(mn$station))) add("warning", PERIOD_FILE, mi[i], "a removal with no target and no dates: this removes the parameter everywhere", "Add a station or dates if that is not intended.") }
  # CR10X files and their FSLs
  cx <- rd("cr10x.csv")
  if (!is.null(cx) && nrow(cx) && isTRUE(cfg$import_cr10x)) { on <- !tolower(cx$import) %in% c("no", "n", "false", "0")
    nofsl <- sum(on & !nzchar(cx$fsl)); if (nofsl) add("warning", "cr10x.csv", "", paste0(nofsl, " CR10X file(s) have no FSL, so their columns are only numbered"), "Match FSLs on the CR10X files page.")
    for (i in which(on & nzchar(cx$fsl))) for (f in trim(strsplit(cx$fsl[i], ";", fixed = TRUE)[[1]])) if (nzchar(f) && !file.exists(path.expand(f)) && !file.exists(file.path(cdir, "fsl", f)))
      add("error", "cr10x.csv", i, paste0("FSL '", f, "' was not found; ", cx$file[i], " falls back to numbered columns"), "Put the file in config/fsl or correct the name.")
    for (i in which(on & !nzchar(cx$logger_id))) add("error", "cr10x.csv", i, paste0(cx$file[i], " has no logger id"), "Give it one on the CR10X files page.")
    for (i in which(numbad(cx$start_year))) add("error", "cr10x.csv", i, paste0("start_year '", cx$start_year[i], "' is not a year"), "Use a four-digit year or leave it blank.") }
  fr <- rd("file_rules.csv")
  if (!is.null(fr) && nrow(fr)) { for (i in which(!tolower(fr$action) %in% c("assign", "ignore"))) add("error", "file_rules.csv", i, paste0("action '", fr$action[i], "' is not known"), "Use assign or ignore.")
    for (i in which(tolower(fr$action) == "assign" & !nzchar(fr$station) & !nzchar(fr$stream))) add("error", "file_rules.csv", i, "assign needs a station", "Fill in the station.")
    d <- fr[, .N, by = .(file, table)][N > 1]; for (k in seq_len(nrow(d))) add("warning", "file_rules.csv", "", paste0(d$file[k], " has ", d$N[k], " rules; they are applied in order"), "Keep one.") }
  # gauges and sites, against the last run where there is one
  ss <- file.path(pdir, "output", "audit", "station_summary.csv"); have <- if (file.exists(ss)) fread(file = ss, colClasses = list(character = c("station", "stream"))) else NULL
  pg <- rd("precip_gauges.csv")
  if (!is.null(pg) && nrow(pg)) { pg <- ensure_cols(pg, PRECIP_COLS)
    for (i in which(!tolower(pg$gauge_type) %in% c("pluvio2", "geonor"))) add("error", "precip_gauges.csv", i, paste0("gauge_type '", pg$gauge_type[i], "' is not known"), "Use pluvio2 or geonor.")
    for (i in which(!nzchar(pg$bucket_col))) add("error", "precip_gauges.csv", i, "no bucket column", "Pick it under Precipitation, Gauge set-up.")
    for (i in which(!tolower(pg$undercatch) %in% c("", "no", "none", "off", "single_alter", "unshielded", "smith2007"))) add("error", "precip_gauges.csv", i, paste0("undercatch '", pg$undercatch[i], "' is not known"), "Use single_alter, unshielded, smith2007, or leave it blank.")
    on <- tolower(pg$bucket_only) %in% c("yes", "y", "true", "1", "on")
    for (i in which(on & ((nzchar(pg$bucket_only_start) & is.na(parse_time(pg$bucket_only_start))) | (nzchar(pg$bucket_only_end) & is.na(parse_time(pg$bucket_only_end)))))) add("error", "precip_gauges.csv", i, "the bucket-only window dates are not dates", "Use YYYY-MM-DD, with a time if wanted.")
    for (i in which(on & (!nzchar(trim(pg$bucket_only_start)) | !nzchar(trim(pg$bucket_only_end))))) {
      add("error", "precip_gauges.csv", i, "the bucket-only window is on but its start or end date is missing", "Set both dates under Precipitation, Bucket-only window.")
    }
    for (i in which(on & !nzchar(pg$acc_col))) add("warning", "precip_gauges.csv", i, "the bucket-only window is on for a gauge with no running total, so it changes nothing", "Turn it off for this gauge.")
    if (!is.null(have)) for (i in which(nzchar(pg$station) & !paste(pg$station, pg$stream) %in% paste(have$station, have$stream)))
      add("warning", "precip_gauges.csv", i, paste0("there was no station table ", pg$station[i], " / ", pg$stream[i], " in the last run"), "It is re-pointed automatically if the station was renamed; otherwise fix or delete the row.") }
  si <- rd("station_info.csv")
  if (!is.null(si) && nrow(si)) { la <- suppressWarnings(as.numeric(si$latitude)); lo <- suppressWarnings(as.numeric(si$longitude))
    for (i in which(numbad(si$latitude) | (!is.na(la) & abs(la) > 90))) add("error", "station_info.csv", i, paste0("latitude '", si$latitude[i], "' is not between -90 and 90"), "Use decimal degrees.")
    for (i in which(numbad(si$longitude) | (!is.na(lo) & abs(lo) > 180))) add("error", "station_info.csv", i, paste0("longitude '", si$longitude[i], "' is not between -180 and 180"), "Use decimal degrees, west negative.")
    for (i in which(xor(nzchar(si$latitude), nzchar(si$longitude)))) add("warning", "station_info.csv", i, "only one of latitude and longitude is given, so the night-time sunlight check is off for this station and albedo uses sunlight above 50 W/m2 for daylight", "Fill in both.") }
  fl <- rd("field_log.csv"); if (!is.null(fl) && nrow(fl)) for (i in which(datebad(fl$date) | !nzchar(fl$date))) add("error", "field_log.csv", i, paste0("date '", fl$date[i], "' is not a date"), "Use YYYY-MM-DD, with a time if you have one.")
  res <- with_declared(chk$result(), cfg$project_dir, area = "table", item = "row")       # with every table's declared rules (core table_rules.R)
  res[order(match(level, c("error", "warning")), table)]
}

preflight_status <- function(p) if (!nrow(p)) "safe" else if (any(p$level == "error")) "attention" else "warnings"

