# Ovid/deployments.R  --  The starting metadata sheet: one row per deployment (a logger, by its serial, at a station for a
# period), the eight columns of templates/metadata_template.csv. Part of Ovid Core (shared). Needs util.R and metasheet.R.
# Sourced by the processors' pipelines (CS, GT, WLR) after metasheet.R; tests in core/tests/run_tests.R.
#
# Only the serial is required. From and To left blank cover the logger's whole record; a year ("2019") or a month ("2024-09",
# "Sept 2024") stands for the whole of it. A logger moved or reused gets a second row; a logger swapped for another leaves the
# station on both serials. The scan (what the raw files say) proposes deployments by serial; a sheet row is matched to them by
# its dates, else its year, else its station or site name against the name saved in the files, else by being the serial's only
# one; what is left is for the user to choose. New rows merge into a sheet without deleting anything.

DEPLOY_FIELDS <- data.table(
  field   = c("serial", "site", "station", "latitude", "longitude", "from", "to", "notes"),
  label   = c("Serial number", "Site", "Station", "Latitude", "Longitude", "From", "To", "Notes"),
  type    = c("text", "text", "text", "number", "number", "date", "date", "text"),
  unit    = "",
  use     = c("ties the row to the logger's files", "the area the station is in", "where the logger was; its records are kept under this name",
              "decimal degrees, north positive", "decimal degrees, east positive (west is negative)",
              "the first day it was there (blank: its first record; a year alone is fine)", "the last day it was there (blank: its last record)", "kept with the row"),
  aliases = c("sn|serial|serial no|serial number|s/n|logger serial|logger sn|logger id|instrument serial|lgr s/n",
              "site name|area|study area|project site|field site",
              "station name|location|location name|locationname|well|borehole|hole|plot|site id|logger name|name|monitoringlocationname",
              "lat|latitude dd|decimal latitude|lat dd|y coordinate",
              "lon|long|lng|longitude dd|decimal longitude|lon dd|x coordinate",
              "start|start date|from date|installed|install date|deployed|record start|record_start|in",
              "end|end date|to date|until|removed|retrieved|record end|record_end|out",
              "note|remarks|comments|comment|description"),
  show    = "TRUE",
  identifies = c("TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "FALSE", "FALSE"))
DEPLOY_COLS <- DEPLOY_FIELDS$field
DEPLOY_NO_SERIAL <- c("", "0", "NOSN", "16777215")                 # serial fields that name no logger (16777215 = 0xFFFFFF)

dep_serial <- function(x) { s <- norm_serial(sub("^(s/?n|serial( no)?)[: #]*", "", trim(as.character(x)), ignore.case = TRUE), keep = "alnum")
  s[is.na(s) | s %in% DEPLOY_NO_SERIAL] <- ""; s }

# A From or To cell as a time and how precise it is ("day", "month", "year" or "" when blank or not understood). end = TRUE: the
# last second of the day, month or year, so a To is inclusive.
DEP_MONTHS <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, sept = 9, oct = 10, nov = 11, dec = 12)
dep_date <- function(x, end = FALSE) {
  x <- trim(as.character(x)); x[is.na(x)] <- ""; n <- length(x)
  t <- rep(NA_real_, n); prec <- rep("", n)
  edge <- function(y, m) { if (is.na(m)) { a <- ISOdate(y, 1, 1, 0, tz = "UTC"); b <- ISOdate(y + 1, 1, 1, 0, tz = "UTC") }
    else { a <- ISOdate(y, m, 1, 0, tz = "UTC"); b <- ISOdate(y + (m == 12), m %% 12 + 1, 1, 0, tz = "UTC") }
    as.numeric(if (end) b - 1 else a) }
  for (i in seq_len(n)) { v <- x[i]; if (!nzchar(v)) next
    if (grepl("^(19|20)\\d{2}$", v)) { t[i] <- edge(as.integer(v), NA); prec[i] <- "year"; next }
    m <- regmatches(v, regexec("^((19|20)\\d{2})[-/](\\d{1,2})$", v))[[1]]
    if (length(m) && as.integer(m[4]) %in% 1:12) { t[i] <- edge(as.integer(m[2]), as.integer(m[4])); prec[i] <- "month"; next }
    m <- regmatches(v, regexec("^([A-Za-z]{3,9})\\.?,? +((19|20)\\d{2})$", v))[[1]]
    if (length(m)) { k <- DEP_MONTHS[substr(tolower(m[2]), 1, if (startsWith(tolower(m[2]), "sept")) 4 else 3)]
      if (!is.na(k)) { t[i] <- edge(as.integer(m[3]), k); prec[i] <- "month"; next } }
    p <- parse_date_cell(v, end = end); if (!is.na(p)) { t[i] <- as.numeric(p); prec[i] <- "day" } }
  list(t = as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), prec = prec)
}

# A latitude or longitude cell in decimal degrees: "60.1031", "-120.1082", "60 06 11 N", "60 deg 06'11.2\"N", "120 deg  06.5' W".
dep_coord <- function(x) dms_to_dd(x)   # core convert.R

# The sheet read and checked: rows as text (every column of DEPLOY_COLS, extra columns kept), plus the typed values the matching
# uses (.serial, .from, .to, .from_prec, .lat, .lon) and the problems found (row, field, value, problem). A cell not understood
# is reported and left out of the typed values, never changed in the sheet.
dep_clean <- function(d) {
  d <- ensure_cols(copy(as.data.table(d))[, lapply(.SD, function(v) { v <- trim(as.character(v)); v[is.na(v)] <- ""; v })], DEPLOY_COLS)
  probs <- list(); bad <- function(i, f, why) if (length(i)) probs[[length(probs) + 1L]] <<- data.table(row = i, field = f, value = d[[f]][i], problem = why)
  d[, .serial := dep_serial(serial)]
  bad(which(!nzchar(d$.serial)), "serial", "no serial: the row is left aside until one is given")
  f <- dep_date(d$from); u <- dep_date(d$to, end = TRUE)
  bad(which(nzchar(d$from) & !nzchar(f$prec)), "from", "not a date (2024-09-20, 2024-09 or 2024)")
  bad(which(nzchar(d$to) & !nzchar(u$prec)), "to", "not a date (2024-09-20, 2024-09 or 2024)")
  bad(which(!is.na(f$t) & !is.na(u$t) & u$t < f$t), "to", "before From")
  d[, `:=`(.from = f$t, .to = u$t, .from_prec = f$prec, .lat = dep_coord(latitude), .lon = dep_coord(longitude))]
  bad(which(nzchar(d$latitude) & is.na(d$.lat)), "latitude", "not a coordinate")
  bad(which(nzchar(d$longitude) & is.na(d$.lon)), "longitude", "not a coordinate")
  bad(which(!is.na(d$.lat) & abs(d$.lat) > 90), "latitude", "outside -90 to 90")
  bad(which(!is.na(d$.lon) & abs(d$.lon) > 180), "longitude", "outside -180 to 180")
  bad(which(!is.na(d$.lat) & !is.na(d$.lon) & d$.lat == 0 & d$.lon == 0), "latitude", "0, 0 is a logger's blank position, not a place")
  lon <- d$.lon[!is.na(d$.lon) & abs(d$.lon) <= 180]                      # a sign that differs from most of the sheet (a minus left off)
  if (length(lon) >= 2) { w <- mean(lon < 0) > 0.5
    bad(which(!is.na(d$.lon) & d$.lon != 0 & (d$.lon < 0) != w), "longitude", if (w) "east of Greenwich, unlike the other rows: a west longitude needs a minus sign" else "west of Greenwich, unlike the other rows") }
  d[abs(.lat) > 90 | (.lat == 0 & .lon == 0), `:=`(.lat = NA_real_, .lon = NA_real_)]; d[abs(.lon) > 180, .lon := NA_real_]
  # the same logger in two places at once
  k <- d[nzchar(.serial), .(i = .I[nzchar(.serial)], s = .serial, a = fcoalesce(as.numeric(.from), -Inf), b = fcoalesce(as.numeric(.to), Inf))]
  if (nrow(k) > 1) { k[, i := which(nzchar(d$.serial))]
    for (sn in unique(k$s[duplicated(k$s)])) { r <- k[k$s == sn][order(a)]
      for (j in seq_len(nrow(r) - 1L)) if (r$b[j] >= r$a[j + 1L]) bad(r$i[j + 1L], "from", paste0("overlaps the row above it for the same serial (row ", r$i[j], ")")) } }
  list(rows = d, problems = if (length(probs)) rbindlist(probs)[order(row)] else data.table(row = integer(), field = character(), value = character(), problem = character()))
}

# ---- the scan ------------------------------------------------------------------------------------------------------------
# Deployments the raw files suggest. files: one row per file read, with serial, name (the station name or plot title saved in it),
# first and last (POSIXct) and file. A serial's files, in time order, stay one deployment while each starts within gap_days of
# the last record so far; a file that starts later starts a new one, and so does one saved under another name that does not
# overlap what came before (a logger moved). Files with no serial are returned apart, for the user to assign.
dep_propose <- function(files, gap_days = 60) {
  f <- copy(as.data.table(files)); f[, serial := dep_serial(serial)]
  if (!"name" %in% names(f)) f[, name := ""]; if (!"file" %in% names(f)) f[, file := ""]
  f[is.na(name), name := ""]
  none <- f[!nzchar(serial) | is.na(first)]; f <- f[nzchar(serial) & !is.na(first)]
  if (!nrow(f)) return(list(deployments = data.table(dep_id = character(), serial = character(), names_in_files = character(), first_record = as.POSIXct(character()),
                                                       last_record = as.POSIXct(character()), record_days = numeric(), files = integer()), unassigned = none))
  f[is.na(last), last := first]; setorder(f, serial, first, last)
  f[, g := { g <- integer(.N); k <- 1L; hi <- last[1]; nm <- meta_norm(name[1])
    for (i in seq_len(.N)) { if (i > 1L) {
        moved <- nzchar(meta_norm(name[i])) && nzchar(nm) && meta_norm(name[i]) != nm && first[i] > hi
        if (moved || as.numeric(difftime(first[i], hi, units = "days")) > gap_days) { k <- k + 1L; nm <- meta_norm(name[i]); hi <- last[i] } }
      g[i] <- k; if (last[i] > hi) hi <- last[i]; if (!nzchar(nm)) nm <- meta_norm(name[i]) }
    g }, by = serial]
  d <- f[, .(names_in_files = paste(unique(name[nzchar(name)]), collapse = "; "), first_record = min(first), last_record = max(last), files = .N), by = .(serial, g)]
  d[, `:=`(dep_id = paste0(serial, "#", g), record_days = round(as.numeric(difftime(last_record, first_record, units = "days")), 1))]
  list(deployments = d[, .(dep_id, serial, names_in_files, first_record, last_record, record_days, files)], unassigned = none)
}

# ---- matching ------------------------------------------------------------------------------------------------------------
# Each sheet row (from dep_clean()$rows) against the scan's deployments of its serial, in this order: its dates overlap them
# ("dates", or "year" / "month" when From was only that); else its station or site is the name saved in the files ("name"); else
# the serial has one deployment ("only"). Otherwise: "choose" (several, nothing tells them apart), "no data" (no file has the
# serial) or "no data in period" (files, but none in the row's dates). Adds match, dep_ids and the scan columns: first_record,
# last_record, record_days, files, names_in_files (over the deployments matched).
dep_match <- function(rows, deps) {
  r <- copy(rows); dp <- as.data.table(deps)
  out <- lapply(seq_len(nrow(r)), function(i) { s <- r$.serial[i]; c0 <- dp[serial == s]
    if (!nzchar(s)) return(list(match = "no serial", ids = character()))
    if (!nrow(c0)) return(list(match = "no data", ids = character()))
    a <- r$.from[i]; b <- r$.to[i]
    if (!is.na(a) || !is.na(b)) {
      hit <- c0[(is.na(a) | last_record >= a) & (is.na(b) | first_record <= b)]
      if (!nrow(hit)) return(list(match = "no data in period", ids = character()))
      p <- r$.from_prec[i]; return(list(match = if (p %in% c("year", "month")) p else "dates", ids = hit$dep_id)) }
    nm <- unique(meta_norm(c(r$station[i], r$site[i]))); nm <- nm[nchar(nm) >= 3]
    if (length(nm)) { hit <- c0[vapply(strsplit(names_in_files, "; ", fixed = TRUE), function(x) { x <- meta_norm(x)
        any(vapply(nm, function(n) any(nzchar(x) & (grepl(n, x, fixed = TRUE) | vapply(x, function(y) grepl(y, n, fixed = TRUE), NA))), NA)) }, NA)]
      if (nrow(hit)) return(list(match = "name", ids = hit$dep_id)) }
    if (nrow(c0) == 1L) return(list(match = "only", ids = c0$dep_id))
    list(match = "choose", ids = c0$dep_id) })
  r[, match := vapply(out, `[[`, "", "match")]
  r[, dep_ids := vapply(out, function(o) paste(o$ids, collapse = "; "), "")]
  sc <- lapply(out, function(o) { h <- dp[dep_id %in% o$ids]; if (!length(o$ids) || !nrow(h) || identical(o$match, "choose")) return(NULL)
    data.table(first_record = min(h$first_record), last_record = max(h$last_record), files = sum(h$files),
               names_in_files = paste(unique(unlist(strsplit(h$names_in_files[nzchar(h$names_in_files)], "; ", fixed = TRUE))), collapse = "; ")) })
  na_t <- as.POSIXct(NA, tz = "UTC")
  sc <- rbindlist(lapply(sc, function(x) if (is.null(x)) data.table(first_record = na_t, last_record = na_t, files = NA_integer_, names_in_files = "") else x))
  r[, `:=`(first_record = sc$first_record, last_record = sc$last_record, files = sc$files, names_in_files = sc$names_in_files)]
  r[, record_days := round(as.numeric(difftime(last_record, first_record, units = "days")), 1)]
  r[]
}
# Deployments the scan found that no sheet row claims: rows to offer ("Add what was found"), serial and the name in its files.
dep_unclaimed <- function(matched, deps) {
  used <- unlist(strsplit(matched$dep_ids[!matched$match %in% "choose"], "; ", fixed = TRUE))
  as.data.table(deps)[!dep_id %in% used]
}

# ---- merging -------------------------------------------------------------------------------------------------------------
# New rows into the sheet, never deleting a row. A new row updates the row with the same serial and From (a blank From: the
# serial's only row, else its open one, with To blank): blank cells are filled; a filled cell that differs is a conflict, kept as
# it was and listed. A new row with a later From than the serial's open row at another station closes that row the day before
# (a logger moved). Any other new row is added. Returns rows (text, DEPLOY_COLS first, extra columns kept) and log (one row per
# action: serial, from, action = added | updated | closed | conflict | unchanged | left aside, field, was, now).
dep_merge <- function(old, new) {
  cols <- unique(c(DEPLOY_COLS, names(old), names(new))); cols <- cols[!startsWith(cols, ".")]
  txt <- function(d) ensure_cols(copy(as.data.table(d))[, lapply(.SD, function(v) { v <- trim(as.character(v)); v[is.na(v)] <- ""; v })], cols)[, cols, with = FALSE]
  o <- txt(if (is.null(old)) data.table() else old); n <- txt(new); log <- list()
  lg <- function(s, f, a, field = "", was = "", now = "") log[[length(log) + 1L]] <<- data.table(serial = s, from = f, action = a, field = field, was = was, now = now)
  day <- function(x) { t <- dep_date(x)$t; ifelse(is.na(t), "", format(t, "%Y-%m-%d")) }
  for (i in seq_len(nrow(n))) { s <- dep_serial(n$serial[i]); f <- n$from[i]
    if (!nzchar(s)) { lg(n$serial[i], f, "left aside", "serial", "", "no serial"); next }
    os <- dep_serial(o$serial); mine <- which(os == s)
    hit <- mine[day(o$from[mine]) == day(f)]
    if (!length(hit) && !nzchar(f) && length(mine)) { open <- mine[!nzchar(o$to[mine])]; hit <- if (length(mine) == 1L) mine else if (length(open) == 1L) open else integer() }
    if (length(hit)) { h <- hit[1]; ch <- FALSE
      for (v in setdiff(cols, "serial")) { a <- o[[v]][h]; b <- n[[v]][i]; if (!nzchar(b) || identical(a, b)) next
        if (!nzchar(a)) { set(o, h, v, b); lg(s, o$from[h], "updated", v, "", b); ch <- TRUE }
        else if (v %in% c("from", "to") && identical(day(a), day(b))) next
        else lg(s, o$from[h], "conflict", v, a, b) }
      if (!ch && !any(vapply(log, function(x) x$serial == s && x$action == "conflict", NA))) lg(s, o$from[h], "unchanged")
      next }
    ft <- dep_date(f)$t
    if (!is.na(ft)) { open <- mine[!nzchar(o$to[mine])]
      for (h in open) { st <- dep_date(o$from[h])$t
        if ((is.na(st) || st < ft) && meta_norm(o$station[h]) != meta_norm(n$station[i])) {
          to <- format(ft - 86400, "%Y-%m-%d"); set(o, h, "to", to); lg(s, o$from[h], "closed", "to", "", to) } } }
    o <- rbind(o, n[i]); lg(s, f, "added") }
  list(rows = o, log = if (length(log)) rbindlist(log) else data.table(serial = character(), from = character(), action = character(), field = character(), was = character(), now = character()))
}
# The log as one line for the screen: "3 added, 1 closed, 2 conflicts".
dep_merge_summary <- function(log) { if (!nrow(log)) return("nothing to merge")
  k <- log[, .(n = uniqueN(paste(serial, from, field))), by = action]
  paste(paste(k$n, ifelse(k$action == "conflict" & k$n > 1, "conflicts", k$action)), collapse = ", ") }

# ---- the project's sheet (config/deployment_sheet.csv) ------------------------------------------------------------------------
# One sheet for every app, in the eight columns above (headings by field name or label; other columns are kept). GT and WLR
# keep their logger list in it too: columns of their own (a name, a year, a hang length, sensor depths ...), each optional
# (core metasheet.R meta_sheet()); CS turns its rows into stations, where a logger stations.csv names keeps that entry.
# dep_sheet() reads and checks it (problems reported once per run); NULL when there is none or it has no serial in it. keep:
# the app's own columns, never taken for one of the eight by their heading ("name" is not the station). dep_day(): a typed
# From or To as the day it stands for.
DEPLOY_SHEET <- "deployment_sheet.csv"
IMPORT_SPECS <- list("deployment_sheet.csv" = DEPLOY_FIELDS)        # the editor's Import matches columns by these headings and aliases
dep_sheet_path <- function(project_dir) file.path(path.expand(project_dir), "config", DEPLOY_SHEET)
# The sheet's headings as field names: a heading that is a field's label or alias is that field, unless it is one of keep or
# the field has a column already.
dep_names <- function(cols, keep = character()) {
  nm <- meta_guess(cols, meta_defaults(DEPLOY_FIELDS)); nm[cols %in% c(keep, DEPLOY_COLS) | !nzchar(nm)] <- cols[cols %in% c(keep, DEPLOY_COLS) | !nzchar(nm)]
  nm[duplicated(nm) | (nm != cols & nm %in% cols)] <- cols[duplicated(nm) | (nm != cols & nm %in% cols)]; nm }
# The sheet as written (text), its headings as field names; NULL when there is none or it cannot be read.
dep_sheet_raw <- function(project_dir, keep = character(), report = FALSE) {
  p <- dep_sheet_path(project_dir); if (!file.exists(p)) return(NULL)
  d <- tryCatch(read_cfg(p), error = function(e) { if (report) problem(DEPLOY_SHEET, conditionMessage(e)); NULL })
  if (is.null(d)) return(NULL); setnames(d, dep_names(names(d), keep)); d[]
}
dep_sheet <- function(project_dir, report = TRUE, keep = character()) {
  d <- dep_sheet_raw(project_dir, keep, report); if (is.null(d) || !nrow(d)) return(NULL)
  cl <- dep_clean(d); cl$rows[, .row := .I]
  if (report) for (i in seq_len(min(nrow(cl$problems), 50))) { x <- cl$problems[i]
    problem(paste(DEPLOY_SHEET, "row", x$row + 1L), paste0(DEPLOY_FIELDS[field == x$field, label], " '", x$value, "': ", x$problem)) }
  r <- cl$rows[nzchar(.serial)]; if (!nrow(r)) NULL else r[]
}
dep_day <- function(t) ifelse(is.na(t), "", format(t, "%Y-%m-%d"))
dep_where <- function(r) ifelse(nzchar(r$station), r$station, r$site)       # the station, else the site
DEPLOY_SHEET_INTRO <- paste("One row per deployment: a logger, by its serial, at a station for a period. Only the serial is needed; From and To left",
  "blank cover its whole record, and a year or a month alone is fine. A logger moved or reused gets a second row; a logger swapped",
  "for another leaves the station on both serials. Import reads a sheet you already have (its columns are matched by their headings).",
  "The same sheet works in every Ovid app; a logger this app's own list already names keeps that entry.")

# ---- the sheet against what the last run read ----------------------------------------------------------------------------
# The files an app's last run listed, as dep_propose() wants them. scan: where the app keeps that list and its column names,
# list(file = "output/audit/files.csv", serial =, name =, file_col =); only files whose status is "ok" are used.
dep_scan_files <- function(project_dir, scan) {
  f <- file.path(path.expand(project_dir), scan$file)
  d <- if (file.exists(f)) tryCatch(fread(f, colClasses = "character", na.strings = c("", "NA")), error = function(e) data.table()) else data.table()
  if (!nrow(d) || !all(c(scan$serial, "first_ts", "last_ts") %in% names(d))) return(data.table(serial = character(), name = character(), first = as.POSIXct(character()), last = as.POSIXct(character()), file = character()))
  if ("status" %in% names(d)) d <- d[status == "ok"]
  g <- function(k) if (!is.null(k) && k %in% names(d)) { x <- d[[k]]; x[is.na(x)] <- ""; x } else rep("", nrow(d))
  data.table(serial = g(scan$serial), name = g(scan$name), first = parse_dt(g("first_ts")), last = parse_dt(g("last_ts")), file = g(scan$file_col))
}
# One table for the screen: each sheet row, how it matched, and the record the files hold for it; then the deployments found that
# no row claims. counts: the matches added up.
dep_check <- function(project_dir, scan) {
  sh <- dep_sheet(project_dir, report = FALSE); pr <- dep_propose(dep_scan_files(project_dir, scan))
  m <- if (is.null(sh)) NULL else dep_match(sh, pr$deployments)
  un <- if (is.null(m)) pr$deployments else dep_unclaimed(m, pr$deployments)
  list(rows = m, unclaimed = un, unassigned = pr$unassigned, counts = if (is.null(m)) integer() else table(m$match))
}
# Accepting a suggested match ("name", "only", "year", "month"): the record's first and last day the files hold written into the
# sheet row's From and To, so the row matches by dates from then on. rows: from dep_check()$rows (serial, site, station, from,
# to as the sheet has them; first_record, last_record). Returns the sheet as written (NULL when nothing changed).
DEP_SUGGESTED <- c("name", "only", "year", "month")
dep_accept_dates <- function(project_dir, rows, who = "app") {
  p <- dep_sheet_path(project_dir); r <- as.data.table(rows)[match %in% DEP_SUGGESTED & !is.na(first_record) & !is.na(last_record)]
  if (!file.exists(p) || !nrow(r)) return(NULL)
  d <- read_cfg(p); nm <- dep_names(names(d))
  col <- function(f) { k <- names(d)[nm == f]; if (length(k)) k[1] else if (f %in% names(d)) f else NA_character_ }
  cs <- vapply(c("serial", "site", "station", "from", "to"), col, ""); if (anyNA(cs[c("serial", "from", "to")])) return(NULL)
  g <- function(f) if (is.na(cs[[f]])) rep("", nrow(d)) else trim(as.character(d[[cs[[f]]]]))
  key <- paste(dep_serial(g("serial")), g("site"), g("station"), g("from"), g("to"), sep = "\r")
  i <- match(paste(r$.serial %||% dep_serial(r$serial), r$site, r$station, r$from, r$to, sep = "\r"), key); ok <- !is.na(i); if (!any(ok)) return(NULL)
  for (f in c("from", "to")) { v <- g(f); v[i[ok]] <- dep_day(if (f == "from") r$first_record[ok] else r$last_record[ok]); set(d, j = cs[[f]], value = v) }
  backup_cfg(p, d, who, "matches accepted"); fwrite_safe(d, p); invisible(d)
}
# Rows for the sheet from deployments found and not claimed: serial, the name saved in the files as the station (to check), the
# record's first and last day as From and To.
dep_rows_found <- function(un) data.table(serial = un$serial, site = "", station = sub(";.*$", "", un$names_in_files), latitude = "", longitude = "",
                                          from = dep_day(un$first_record), to = dep_day(un$last_record), notes = "from the files: check the station")

# ---- a logger split where it looks moved (core grouping.R, Needs you) --------------------------------------------------------
# The split is a row of the sheet: the serial's row that holds the time `at` ends the day before and a new row starts on that
# day with the same station, site and other cells (a move: change the station if it went elsewhere); a serial with no such
# row gets two, before and after, at place (list(site =, station =): where the app has the logger now; blank when not given).
# d: the sheet as written (headings as field names). Returns d with the rows changed, as text.
DEP_SPLIT_NOTE <- "split here: looks moved; check the station"
dep_split_rows <- function(d, serial, at, note = DEP_SPLIT_NOTE, place = list()) {
  d <- ensure_cols(copy(as.data.table(d %||% data.table())), DEPLOY_COLS); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x })
  t <- parse_date_cell(at); if (is.na(t)) return(d); day <- format(t, "%Y-%m-%d", tz = "UTC"); before <- format(t - 86400, "%Y-%m-%d", tz = "UTC")
  sn <- dep_serial(serial); mine <- which(dep_serial(d$serial) == sn)
  if (any(dep_day(dep_date(d$from[mine])$t) == day)) return(d)                       # split there already
  f <- dep_date(d$from[mine])$t; u <- dep_date(d$to[mine], end = TRUE)$t
  h <- mine[(is.na(f) | f < t) & (is.na(u) | u >= t)]
  if (length(h)) { h <- h[1]; new <- d[h]; set(d, h, "to", before)
    set(new, j = "from", value = day); set(new, j = "notes", value = paste(c(if (nzchar(new$notes)) new$notes, note), collapse = "; "))
    return(rbind(d[seq_len(h)], new, d[-seq_len(h)])) }
  blank <- d[rep(NA_integer_, 2)]; for (v in names(blank)) set(blank, j = v, value = "")
  blank[, `:=`(serial = trim(as.character(serial)), from = c("", day), to = c(before, ""), notes = note)]
  for (k in intersect(names(place), c("site", "station"))) if (length(place[[k]]) == 1 && !is.na(place[[k]])) set(blank, j = k, value = as.character(place[[k]]))
  rbind(d, blank)
}
dep_split <- function(project_dir, serial, at, note = DEP_SPLIT_NOTE, who = "app", place = list()) {
  p <- dep_sheet_path(project_dir); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  d <- if (file.exists(p)) dep_sheet_raw(project_dir) else empty_cfg(DEPLOY_COLS); if (is.null(d)) stop(DEPLOY_SHEET, " cannot be read")
  out <- dep_split_rows(d, serial, at, note, place); if (identical(nrow(out), nrow(d))) return(invisible(FALSE))
  backup_cfg(p, out, who, "split"); fwrite_safe(out, p); invisible(TRUE)
}
# The splits the sheet holds: for each serial with more than one row, the From of every row after its first (serial, at).
dep_splits <- function(d) {
  if (is.null(d) || !nrow(d) || !all(c("serial", "from") %in% names(d))) return(data.table(serial = character(), at = character()))
  x <- data.table(serial = trim(as.character(d$serial)), t = dep_date(d$from)$t)[nzchar(serial)]
  x[, s := dep_serial(serial)]; x <- x[s %in% x[, .N, by = s][N > 1, s]]
  x[, first := !is.na(t) & !any(is.na(t)) & t == min(t, na.rm = TRUE), by = s]       # a row with no From is the first
  x[!is.na(t) & !first][order(s, t), .(serial, at = format(t, "%Y-%m-%d", tz = "UTC"))]
}
