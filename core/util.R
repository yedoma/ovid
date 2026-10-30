# Ovid/util.R  --  Small helpers shared by every Ovid Core app: console output, text clean-up, names, time.
# Part of Ovid Core (shared). Sourced by cs_pipeline.R, wlr_pipeline.R, gt_pipeline.R, gs_pipeline.R and rs_pipeline.R (RS) before their own R/ files;
# not meant to be run on its own. A helper whose rules differ between apps (serial numbers, for example) stays in that app.

# Where a record has a hole, a line must not be drawn across it: a new segment starts after any gap longer than tol steps.
seg_id <- function(t, step = NULL, tol = 1.5) { g <- c(0, diff(as.numeric(t)))
  if (is.null(step)) step <- if (length(t) > 2) max(stats::median(g[-1]), 1) else Inf
  cumsum(g > tol * step) }

# Console colours come from the app's palette: each app sets PAL (a named list of hex colours with at least fg).
.ansi <- function(hex) { v <- col2rgb(hex); sprintf("\033[38;2;%d;%d;%dm", v[1], v[2], v[3]) }

# One say() for every app. log: an environment whose $lines collects each message (an app's LOG, for its Log page);
# steps: the number of pipeline steps, so a "[k/N]" message moves the Shiny progress bar. Both default to the app's
# LOG and SAY_STEPS when it sets them; a pipeline run on its own has neither and only prints.
say <- function(..., col = "fg", log = get0("LOG", ifnotfound = NULL),
                steps = get0("SAY_STEPS", ifnotfound = 0L)) {
  tty <- isatty(stdout()) || nzchar(Sys.getenv("RSTUDIO"))
  msg <- paste0(...)
  if (is.environment(log)) log$lines <- c(log$lines, msg)
  stage_mark(msg)
  say_progress(msg, steps)
  if (tty) { pal <- get0("PAL", ifnotfound = list()); hex <- pal[[col]]; if (is.null(hex)) hex <- pal$fg
    if (!is.null(hex)) msg <- paste0(.ansi(hex), msg, "\033[0m") }
  cat(msg, "\n", sep = "")
}

# "[k/N] what" moves the progress bar of the Shiny session to the middle of step k. Outside a session, or with no steps,
# nothing happens; a message sent while no progress bar is open only warns, and that is silenced too.
say_progress <- function(msg, steps) {
  pat <- paste0("^\\[[0-9]+/", steps, "\\] ?")
  if (!steps || !grepl(pat, msg)) return(invisible())
  if (!requireNamespace("shiny", quietly = TRUE) || is.null(shiny::getDefaultReactiveDomain())) return(invisible())
  k <- as.numeric(sub("^\\[([0-9]+)/.*", "\\1", msg))
  try(suppressWarnings(shiny::setProgress((k - 0.5) / steps, detail = sub(pat, "", msg))), silent = TRUE)   # no bar open: a warning, not an error
  invisible()
}

# Stage times for the run log: each "[k/N] what" message starts a stage, which ends when the next one starts or the run
# ends. run_logged() (core manifest.R) clears them when a run starts and lists them at the end of the log.
STAGES <- new.env()
stage_mark <- function(msg) { if (grepl("^\\[[0-9]+/[0-9]+\\]", msg)) STAGES$at <- c(STAGES$at, stats::setNames(as.numeric(Sys.time()), msg)); invisible() }
stage_times <- function(end = Sys.time()) { a <- STAGES$at; if (!length(a)) return(character())
  s <- diff(c(unname(a), as.numeric(end))); tot <- max(sum(s), 1e-9)
  sprintf("  %-*s %7.1f s %4.0f%%", max(nchar(names(a))), names(a), s, 100 * s / tot) }

fmt_ts <- function(x) if (inherits(x, "POSIXct")) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC") else as.character(x)

# Logger files are not reliably UTF-8 (degree signs, Latin-1 exports, corrupted header bytes).
# Anything that is not valid UTF-8 is re-read as Windows-1252 (else Latin-1) so text functions never fail.
fix_enc <- function(x) {
  if (!is.character(x)) return(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) { y <- iconv(x[bad], "CP1252", "UTF-8")                # Windows exports: Latin-1 plus curly quotes, dashes, euro
    y[is.na(y)] <- iconv(x[bad][is.na(y)], "latin1", "UTF-8", sub = "?"); x[bad] <- y }
  utf <- !is.na(x) & !bad & Encoding(x) == "latin1"      # read as Latin-1 but the bytes are UTF-8 (W/m2 from newer loggers): relabel, do not convert twice
  if (any(utf)) { y <- x[utf]; Encoding(y) <- "UTF-8"; x[utf] <- y }
  enc2utf8(x)
}

trim <- function(x) trimws(fix_enc(x))

safe_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

html_esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE) }

num <- function(x) format(x, big.mark = ",", trim = TRUE)

parse_csv_line <- function(l, sep = ",") tryCatch(
  fix_enc(scan(text = fix_enc(l), what = character(), sep = sep, quote = "\"", quiet = TRUE, na.strings = NULL)),
  # safe: a line that cannot be split gives no cells, which the caller treats as no heading
  error = function(e) character())

drop_trailing_blank <- function(x) { while (length(x) && (is.na(x[length(x)]) || !nzchar(x[length(x)]))) x <- x[-length(x)]; x }

# Read a text file whatever its encoding or line endings; drops a UTF-8 byte-order mark and reads NUL bytes as spaces.
read_lines_any <- function(path, n = -1L) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0) return(character())
  raw <- readBin(path, "raw", if (n > 0) min(size, n * 400) else size)
  if (length(raw) >= 3 && all(raw[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  raw[raw == as.raw(0)] <- as.raw(32)
  txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8", sub = "?")
  l <- strsplit(gsub("\r", "", txt, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  if (n > 0) head(l, n) else l
}

# ---- time -------------------------------------------------------------------
# Every time stamp is kept as clock time in the base zone (UTC-7 unless the app says otherwise, no daylight saving),
# stored as a POSIXct in "UTC" so R never applies a daylight-saving shift. A file in another zone is moved onto the base.
BASE_OFFSET <- -7

# A UTC offset in hours as a label: 0 -> "UTC", -7 -> "UTC-7", 5.5 -> "UTC+5:30".
zone_text <- function(h) {
  if (is.null(h) || !length(h) || is.na(h)) return("UTC")
  if (h == 0) return("UTC")
  m <- round(abs(h %% 1) * 60); paste0("UTC", if (h < 0) "-" else "+", abs(trunc(h)), if (m) sprintf(":%02d", m) else "")
}
# A choice list of zones: the base zone first, then UTC (named by zone_text, valued as text hours).
zone_choices <- function(base = BASE_OFFSET) { z <- unique(c(base, 0)); stats::setNames(as.character(z), vapply(z, zone_text, "")) }

to_base <- function(t, offset_hours, base = BASE_OFFSET) {
  if (is.na(offset_hours)) offset_hours <- base
  t - (offset_hours - base) * 3600
}

# "GMT-07:00", "UTC-06:00", "Mountain Daylight Time" -> hours; NA when nothing is stated.
parse_offset <- function(x) {
  x <- trim(if (is.null(x) || !length(x)) "" else x)
  m <- regmatches(x, regexec("(GMT|UTC)\\s*([+-])\\s*(\\d{1,2}):?(\\d{2})?", x))[[1]]
  if (length(m)) return((if (m[3] == "-") -1 else 1) * (as.numeric(m[4]) + as.numeric(if (nzchar(m[5])) m[5] else 0) / 60))
  zones <- c("Mountain Standard Time" = -7, "Mountain Daylight Time" = -6, "Pacific Standard Time" = -8, "Pacific Daylight Time" = -7,
             "Central Standard Time" = -6, "Central Daylight Time" = -5)
  hit <- zones[vapply(names(zones), function(z) grepl(z, x, fixed = TRUE), logical(1))]
  if (length(hit)) unname(hit[1]) else NA_real_
}

# The date-time styles tried, in order. An app whose files use other styles sets its own DT_FORMATS.
DT_FORMATS <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M:%OS", "%m/%d/%y %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p",
                "%d.%m.%Y %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%d/%m/%Y %H:%M:%OS", "%m/%d/%Y %H:%M", "%Y-%m-%d")

# A format is accepted for the whole column only if it reads nearly everything: 98 %, or all but two cells, so a file cut off
# in its last line still reads (avoids day/month mix-ups row by row). strptime() reads the start of a cell and ignores the
# rest, so a format is also refused when it would leave something behind: an AM/PM the format has no %p for, seconds it has
# no %S for, or a two-digit year read as %Y (a year before 1900).
parse_dt <- function(x, formats = DT_FORMATS) {
  x <- trim(as.character(x)); out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  todo <- !is.na(x) & nzchar(x)
  for (f in formats) {
    if (!any(todo)) break
    v <- x[todo]
    if (!grepl("%p", f, fixed = TRUE) && mean(grepl("[AaPp]\\.?[Mm]\\.?$", v)) > 0.5) next
    if (!grepl("%S|%OS|%T", f) && mean(grepl("[0-9]:[0-9]{2}:[0-9]{2}", v)) > 0.5) next
    p <- as.POSIXct(strptime(v, f, tz = "UTC"))
    ok <- !is.na(p); if (!any(ok)) next
    if (mean(as.integer(format(p[ok], "%Y")) < 1900) > 0.02) next
    if (mean(ok) >= 0.98 || sum(!ok) <= 2L) { out[todo] <- p; todo[todo] <- !ok }
  }
  out
}

# Set-up table cells, one cell at a time (a column may mix dates and date-times): "2019-08-08", "2019-08-08 12:00",
# "2019/08/08", "8/8/2019 12:00" (as Excel saves a CSV) or an Excel serial. end = TRUE: a date with no time means the end
# of that day, so an end date is inclusive in every table.
CELL_FORMATS <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
                  "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y")
CELL_DATE_ONLY <- "^(\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}|\\d{1,2}/\\d{1,2}/\\d{4})$"
parse_date_cell <- function(x, end = FALSE) {
  x <- trim(as.character(x)); x[is.na(x)] <- ""
  one <- function(v) { if (!nzchar(v)) return(NA_real_)
    for (f in CELL_FORMATS[grepl("^\\d{4}", v) == startsWith(CELL_FORMATS, "%Y")]) {   # full date-times first, so no time is cut off
      t <- strptime(v, f, tz = "UTC"); if (!is.na(t)) return(as.numeric(as.POSIXct(t))) }
    n <- suppressWarnings(as.numeric(v)); if (!is.na(n) && n > 20000 && n < 80000) return(round(n * 86400) - 25569 * 86400)
    NA_real_ }
  out <- as.POSIXct(vapply(x, one, 0, USE.NAMES = FALSE), origin = "1970-01-01", tz = "UTC")
  if (end) { d <- !is.na(out) & grepl(CELL_DATE_ONLY, x); out[d] <- out[d] + 86399 }
  out
}

# The step between records, in seconds (the most common difference).
step_of <- function(t) { d <- diff(sort(as.numeric(unique(t)))); if (!length(d)) return(NA_real_)
  as.numeric(names(sort(table(d), decreasing = TRUE))[1]) }

fmt_step <- function(s) ifelse(is.na(s), "", ifelse(s < 60, paste(s, "s"), ifelse(s < 3600, paste(round(s / 60, 1), "min"),
                        ifelse(s < 86400, paste(round(s / 3600, 1), "h"), paste(round(s / 86400, 1), "d")))))

water_year <- function(t, start_month = 10) { y <- as.integer(format(t, "%Y")); m <- as.integer(format(t, "%m")); y + (m >= start_month) }

# ---- small shared helpers (each app had its own copy) ---------------------------------------------------------------
out_dir <- function(project_dir, ...) file.path(path.expand(project_dir), "output", ...)
zip_dir <- function(dir, zipfile) {
  files <- list.files(dir, recursive = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, files, root = dir)
  else { old <- setwd(dir); on.exit(setwd(old)); utils::zip(zipfile, files, flags = "-rq") }
  zipfile
}
`%|%` <- function(a, b) if (length(a) && !is.na(a[1]) && nzchar(as.character(a[1]))) a else b      # a value, unless it is missing or blank
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a                     # a value, unless it is NULL or empty
thin <- function(d, max_points = 60000) if (nrow(d) > max_points) d[unique(round(seq(1, .N, length.out = max_points)))] else d   # evenly spaced rows, for plotting
has_value <- function(x) !is.null(x) && length(x) && any(!is.na(x) & nzchar(as.character(x)))   # anything there besides NA and ""
# A two-column table (item, value) from a named list, for a report: entries with no value are left out, several values are
# joined with ", ". NULL when nothing is left.
kv_table <- function(rows) { rows <- Filter(has_value, rows); if (!length(rows)) return(NULL)
  data.table(item = names(rows), value = vapply(rows, function(r) paste(unique(as.character(r[!is.na(r) & nzchar(as.character(r))])), collapse = ", "), "")) }
col_or_null <- function(tab, col) if (nrow(tab) && col %in% names(tab)) tab[[col]] else NULL     # a column, or NULL when the table is empty or lacks it
rx_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)                       # text as a regular expression that matches only it

# ---- the app's own settings (app_settings.json) ----------------------------------------------------------------------
# Kept from one session and one run to the next, as plain JSON that any text editor can read. settings_save() changes
# only the keys it is given and keeps every other one; it writes a new copy beside the file and renames it into place,
# keeping the copy before as .bak. settings_read() falls back to that .bak when the file cannot be read (marked
# from_backup, so the app can say so), then to the app_settings.rds an older version saved (the next save writes the
# .json), and to no settings only when none can be read.
# JSON has no "missing" or "empty text" of its own: a missing value is written as null and read back as NA, an empty
# list is read back as character(0) (an empty list of folders or names), and whole numbers as numbers.
settings_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
}
settings_unjson <- function(x) {
  if (is.null(x)) return(NA)
  if (is.integer(x)) return(as.numeric(x)) # 7 in the file is 7, not 7L
  if (is.list(x)) {
    if (!length(x) && is.null(names(x))) return(character())
    x[] <- lapply(x, settings_unjson)
  }
  x
}
.settings_rd <- function(f) {
  if (!file.exists(f)) return(NULL)
  rd <- function() {
    if (grepl("\\.rds(\\.bak)?$", f)) readRDS(f) else settings_unjson(jsonlite::read_json(f, simplifyVector = TRUE))
  }
  # safe: a damaged settings file reads as none; settings_read() then takes the last good copy
  x <- tryCatch(rd(), error = function(e) NULL)
  if (is.list(x)) x else NULL
}
settings_read <- function(file) {
  x <- .settings_rd(file)
  if (!is.null(x)) return(x)
  b <- .settings_rd(paste0(file, ".bak"))
  if (!is.null(b)) {
    if (file.exists(file)) attr(b, "from_backup") <- TRUE
    return(b)
  }
  if (grepl("\\.json$", file) && !file.exists(file)) { # saved by an older version
    old <- sub("\\.json$", ".rds", file)
    x <- .settings_rd(old) %||% .settings_rd(paste0(old, ".bak"))
    if (!is.null(x)) return(x)
  }
  list()
}
# The environment holding `name`, from `env` up (an app's own settings, wherever the app's code was sourced).
env_with <- function(name, env) { while (!identical(env, emptyenv())) { if (exists(name, envir = env, inherits = FALSE)) return(env); env <- parent.env(env) }; NULL }
settings_save <- function(file, x) {
  x <- x[!is.na(names(x)) & nzchar(names(x))] # x[keys] gives NA names for keys it lacks
  s <- settings_read(file)
  attr(s, "from_backup") <- NULL
  s[names(x)] <- x
  tmp <- paste0(file, ".tmp")
  writeLines(settings_json(s), tmp, useBytes = TRUE)
  if (!is.null(.settings_rd(file))) file.copy(file, paste0(file, ".bak"), overwrite = TRUE)
  if (!file.rename(tmp, file)) {
    file.copy(tmp, file, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(s)
}

# ---- a run in the background (core runner.R) -------------------------------------------------------------------------------
# A run started from an app goes on in its own R process; Cancel there leaves a flag file (option ovid.cancel_file). The run
# looks for it at each message (say) and each item (run_tick), so it stops between steps, never in the middle of writing a
# file, and unwinds past every tryCatch to the runner (a "ovid_cancel" restart, not an error, so no step records it as a
# problem). Outside a background run both do nothing.
run_check_cancel <- function() {
  f <- getOption("ovid.cancel_file")
  if (!is.null(f) && file.exists(f) && !is.null(findRestart("ovid_cancel"))) {
    options(ovid.cancelled = TRUE); invokeRestart("ovid_cancel") }
  invisible(FALSE)
}
# An R value as bytes for hashing: what saveRDS(version = 2, compress = FALSE) writes, except the version of R that wrote
# them (bytes 7-10), which is set to 4.6.1 (00 04 06 01, the version flight ids and review decisions were made with), so
# a new R release (4.6.2 moved every flight id) never changes a hash.
hash_bytes <- function(x) { b <- serialize(x, NULL, version = 2); b[7:10] <- as.raw(c(0x00, 0x04, 0x06, 0x01)); b }
# Item i of n in the current stage (a file, a logger, a station), for the progress bar.
# ---- reading files in parallel ----------------------------------------------------------------------------------------------
# FUN over X in `workers` R processes at once (forked, so on macOS and Linux; one at a time on Windows or with one worker). FUN
# must only read and return: whatever it changes in the app (the audit trail, flags, problems) is lost in a worker, so the
# caller records the results itself. An error in one item is returned as its condition, not raised. Items go in batches, and
# between batches the progress bar moves and Cancel is looked for (a worker never stops part way). OVID_WORKERS or the
# option ovid.workers sets the number; by default the cores less one, at most 8.
run_workers <- function() {
  n <- suppressWarnings(as.integer(getOption("ovid.workers", Sys.getenv("OVID_WORKERS", NA))))
  if (is.na(n)) n <- max(1L, min(8L, (parallel::detectCores(logical = FALSE) %|% 2L) - 1L))
  if (.Platform$OS.type == "windows") 1L else n
}
par_map <- function(X, FUN, workers = run_workers(), what = "") {
  one <- function(x) tryCatch(FUN(x), error = function(e) e)
  if (.Platform$OS.type == "windows") workers <- 1L                                       # no forking on Windows, whatever was asked
  if (workers < 2L || length(X) < 2L * workers) return(lapply(X, one))
  out <- vector("list", length(X)); b <- split(seq_along(X), ceiling(seq_along(X) / (4L * workers)))
  for (k in b) { run_tick(max(k), length(X), what)
    out[k] <- parallel::mclapply(X[k], function(x) { options(ovid.cancel_file = NULL); one(x) }, mc.cores = workers, mc.preschedule = FALSE) }
  out
}

# Printed at most four times a second (the cancel flag is looked for every time).
RUN_TICK <- new.env()
run_tick <- function(i, n, what = "") {
  if (is.null(getOption("ovid.cancel_file"))) return(invisible())
  run_check_cancel(); now <- as.numeric(Sys.time()); if (i < n && now - (RUN_TICK$t %||% 0) < 0.25) return(invisible())
  RUN_TICK$t <- now; cat("@@ ", i, " ", n, " ", gsub("[\r\n]+", " ", what), "\n", sep = ""); flush(stdout()); invisible()
}
# A span of seconds as text: "45 s", "75.5 min", "1.5 h", "3 days"; the sign is kept (a clock running backwards reads
# "-30 s"), and a missing span is blank.
fmt_span <- function(sec) { a <- abs(sec)
  ifelse(is.na(a), "", ifelse(a < 120, paste(round(sec), "s"), ifelse(a < 7200, paste(round(sec / 60, 1), "min"),
         ifelse(a < 172800, paste(round(sec / 3600, 1), "h"), paste(round(sec / 86400, 1), "days"))))) }

# Serial numbers as one spelling, so a file, a workbook and a set-up table name a logger alike: spaces, dots and dashes go,
# ".0" from a spreadsheet goes. SERIAL_KEEP says what a serial is made of in an app: "alnum" (letters and digits, upper case;
# leading zeros dropped from all-digit serials: GeoPrecision E00001, HOBO 0000001) or "digits" (digits only, leading zeros
# dropped: Solinst and HOBO water level loggers, whose workbook serials carry stray prefixes). An app sets its own.
SERIAL_KEEP <- "alnum"
norm_serial <- function(x, keep = SERIAL_KEEP) {
  x <- gsub("\\.0+$", "", trim(as.character(x)))
  if (identical(keep, "digits")) return(sub("^0+", "", gsub("[^0-9]", "", x)))
  x <- gsub("[^A-Z0-9]", "", toupper(x)); ifelse(grepl("^[0-9]+$", x), sub("^0+(?=.)", "", x, perl = TRUE), x)
}

# ---- The project folder when none is set --------------------------------------------------------------------------------
# Only the raw data folder is needed. With no project (output) folder, or one that is "/", the home folder or inside the
# app's own folder (its git repository), the app uses ~/Projects/Ovid/Projects/<app>/project, the folder every app's
# projects go in; it is created on the first write. A folder the user sets always wins otherwise.
ovid_default_project <- function(app = get0("OVID_APP", ifnotfound = "ovid"), name = "project") file.path(ovid_projects_home(app), name)
# Apps that were renamed: former name = current name (ERT became GS on 2026-10-14). A project marked with a former name
# opens in the current app, and ~/Projects/Ovid/Projects/<former> stays the projects folder until a <current> one exists.
OVID_APP_RENAMED <- c(ERT = "GS")
ovid_app_name <- function(app) { app <- as.character(app); i <- app %in% names(OVID_APP_RENAMED); app[i] <- OVID_APP_RENAMED[app[i]]; app }
ovid_projects_home <- function(app, root = "~/Projects/Ovid/Projects") {
  d <- file.path(root, app); old <- file.path(root, names(OVID_APP_RENAMED)[OVID_APP_RENAMED == app])
  old <- old[dir.exists(path.expand(old))]
  if (!dir.exists(path.expand(d)) && length(old)) old[1] else d }
# A path with its links resolved as far as it exists (/var and /private/var are the same folder on a Mac).
norm_any <- function(p) { p <- path.expand(p); tail <- character()
  while (!file.exists(p) && !identical(p, dirname(p))) { tail <- c(basename(p), tail); p <- dirname(p) }
  do.call(file.path, as.list(c(normalizePath(p, winslash = "/", mustWork = FALSE), tail))) }
ovid_repo_root <- function(dir) { d <- norm_any(dir)
  while (nzchar(d) && !identical(d, dirname(d))) { if (file.exists(file.path(d, ".git"))) return(d); d <- dirname(d) }; NULL }
ovid_project_dir <- function(x, app = get0("OVID_APP", ifnotfound = "ovid"), repo = ovid_repo_root(get0("APP_DIR", ifnotfound = get0("PIPELINE_DIR", ifnotfound = getwd())))) {
  x <- if (length(x) >= 1 && !is.na(x[1])) trim(as.character(x[1])) else ""
  if (!nzchar(x)) return(ovid_default_project(app))
  n <- norm_any(x)
  if (n == norm_any("~") || grepl("^([A-Za-z]:)?/$", n)) return(ovid_default_project(app))
  if (!is.null(repo) && startsWith(paste0(n, "/"), paste0(norm_any(repo), "/"))) return(ovid_default_project(app))
  x }
