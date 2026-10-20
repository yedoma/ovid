# Ovid/timezones.R  --  The time-zone table: which zone a logger's clock was set to, for a whole logger, model, folder or year at once.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# config/timezones.csv, one row per rule. Every column that is filled in must match; a blank column matches anything:
#   serial        the logger's serial
#   model         any part of the logger's model name, e.g. U20 or Levelogger
#   file_pattern  a file name, folder or regular expression on the file's path, e.g. 2019/ or ^baro/
#   start, end    the file's first reading falls in this span (dates or date-times; end is inclusive)
#   offset_hours  the clock's offset from UTC, e.g. -7 or -6
# The first matching row wins, so put narrow rules (one serial) above broad ones (a whole year).

TZ_COLS <- c("serial", "model", "file_pattern", "start", "end", "offset_hours", "reason")

tz_str <- function(v) if (is.null(v) || !length(v) || is.na(v[1])) "" else trim(as.character(v[1]))
tz_dt <- function(x) parse_dt(x, formats = unique(c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d", DT_FORMATS)))
tz_end <- function(x) { t <- tz_dt(x); if (!is.na(t) && grepl("^\\s*\\d{4}-\\d{2}-\\d{2}\\s*$", tz_str(x))) t + 86400 - 1 else t }

# Index of the first row that applies to this file, or NA.
tz_match <- function(tz, serial, rel, model = NA, first_ts = NA) {
  if (is.null(tz) || !nrow(tz)) return(NA_integer_)
  ns <- get0("norm_serial", ifnotfound = function(x) toupper(gsub("[^A-Za-z0-9]", "", trim(as.character(x)))))   # the app's own rule, if it has one
  g <- function(col, i) { v <- if (col %in% names(tz)) tz[[col]][i] else ""; if (is.na(v)) "" else trim(as.character(v)) }
  for (i in seq_len(nrow(tz))) {
    if (is.na(suppressWarnings(as.numeric(g("offset_hours", i))))) next
    s <- g("serial", i); md <- g("model", i); p <- g("file_pattern", i); a <- g("start", i); b <- g("end", i)
    if (nzchar(s) && !identical(ns(s), ns(tz_str(serial)))) next
    if (nzchar(md) && !(!is.na(model) && grepl(md, model, ignore.case = TRUE, fixed = FALSE))) next
    # safe for the run: a pattern that is not a valid regular expression matches no file
    if (nzchar(p) && !isTRUE(tryCatch(grepl(p, rel, ignore.case = TRUE), error = function(e) FALSE))) next
    if ((nzchar(a) || nzchar(b)) && is.na(first_ts)) next
    if (nzchar(a) && !isTRUE(first_ts >= tz_dt(a))) next
    if (nzchar(b) && !isTRUE(first_ts <= tz_end(b))) next
    return(i)
  }
  NA_integer_
}

# The rows of the table that could apply to any of these files, whatever their first reading (serial, model and path
# match; start and end are left to tz_match()), in table order: what a file's zone can depend on, for a cache key.
tz_rows_for <- function(tz, serial, rel, model = NA) {
  if (is.null(tz) || !nrow(tz)) return(tz)
  ns <- get0("norm_serial", ifnotfound = function(x) toupper(gsub("[^A-Za-z0-9]", "", trim(as.character(x)))))
  g <- function(col, i) { v <- if (col %in% names(tz)) tz[[col]][i] else ""; if (is.na(v)) "" else trim(as.character(v)) }
  model <- rep_len(if (is.null(model)) NA else model, length(rel)); serial <- rep_len(serial, length(rel))
  keep <- vapply(seq_len(nrow(tz)), function(i) { s <- g("serial", i); md <- g("model", i); p <- g("file_pattern", i)
    any(vapply(seq_along(rel), function(j) (!nzchar(s) || identical(ns(s), ns(tz_str(serial[j])))) &&
      (!nzchar(md) || (!is.na(model[j]) && grepl(md, model[j], ignore.case = TRUE))) &&
      # safe for the run: as above, an invalid pattern matches no file
      (!nzchar(p) || isTRUE(tryCatch(grepl(p, rel[j], ignore.case = TRUE), error = function(e) FALSE))), NA)) }, NA)
  tz[keep]
}

# Which zone a file's clock is in. With stated_wins = FALSE (GT) a table row wins over what the file states;
# with stated_wins = TRUE (WLR) a zone stated in the file (HOBOware header) wins and the table fills in the rest.
# Neither: the base zone. Returns list(offset, why, row).
zone_for <- function(serial, rel, stated, tz, base, model = NA, first_ts = NA, stated_wins = FALSE, unzoned = NA) {
  if (stated_wins && !is.na(stated)) return(list(offset = stated, why = "stated in the file", row = NA_integer_))
  i <- tz_match(tz, serial, rel, model, first_ts)
  if (!is.na(i)) { r <- if ("reason" %in% names(tz) && !is.na(tz$reason[i])) trim(tz$reason[i]) else ""
    return(list(offset = as.numeric(tz$offset_hours[i]), why = paste0("time zone table", if (nzchar(r)) paste0(": ", r) else ""), row = i)) }
  if (!is.na(stated)) return(list(offset = stated, why = "stated in the file", row = NA_integer_))
  if (length(unzoned) == 1 && !is.na(unzoned)) return(list(offset = unzoned, why = "no zone stated; taken as the zone chosen for such files", row = NA_integer_))
  list(offset = base, why = "logger clock, taken as the base zone", row = NA_integer_)
}

# Files that state no time zone are read in the base zone unless a zone has been chosen for them (unzoned_offset_h). When
# none of a run's files states its zone and none has been chosen, the app asks which zone the clocks were in (ask = TRUE
# raises a "ovid_ask_zone" condition the app turns into a question); a script carries on with the base zone and says so.
unzoned_check <- function(n_unzoned, n_total, chosen = NA, ask = FALSE, base = BASE_OFFSET) {
  if (!n_total || n_unzoned < n_total || (length(chosen) == 1 && !is.na(chosen))) return(invisible(FALSE))
  msg <- sprintf("None of the %d files states its time zone. Choose the zone their clocks were in when the app asks, or give it in the time-zone table; until then they are read as the base zone (UTC%+g).", n_total, base)
  if (isTRUE(ask)) stop(structure(class = c("ovid_ask_zone", "error", "condition"), list(message = msg, call = NULL, n = n_total)))
  problem("time zones", msg); say("  ", msg, col = "yellow"); invisible(TRUE)
}

# Set-up check rows (row, item, problem) for the table.
tz_check <- function(tz) {
  out <- list(); add <- function(i, msg) out[[length(out) + 1L]] <<- data.table(row = i, item = paste(c(tz$serial[i], tz$model[i], tz$file_pattern[i])[nzchar(c(tz$serial[i], tz$model[i], tz$file_pattern[i]))], collapse = " "), problem = msg)
  if (!is.null(tz) && nrow(tz)) for (i in seq_len(nrow(tz))) {
    o <- suppressWarnings(as.numeric(tz$offset_hours[i]))
    if (is.na(o)) add(i, "offset_hours must be a number such as -7 or 0; this row is ignored.")
    else if (abs(o) > 14) add(i, "offset_hours is more than 14 hours from UTC.")
    for (v in intersect(c("start", "end"), names(tz))) if (nzchar(tz_str(tz[[v]][i])) && is.na(tz_dt(tz[[v]][i]))) add(i, paste0(v, " '", tz[[v]][i], "' is not a date or date-time; this row never matches."))
    p <- tz_str(tz$file_pattern[i])
    if (nzchar(p) && inherits(tryCatch(grepl(p, "x"), error = function(e) e), "error")) add(i, paste0("file_pattern '", p, "' is not a valid pattern; this row never matches."))
  }
  if (length(out)) rbindlist(out) else data.table(row = integer(), item = character(), problem = character())
}
