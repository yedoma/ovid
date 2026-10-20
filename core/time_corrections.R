# Ovid/time_corrections.R  --  Clock corrections set by hand (config/time_corrections.csv) applied to one logger's records.
# Part of Ovid Core (shared). Sourced by cs_pipeline.R, wlr_pipeline.R and gt_pipeline.R after timestamps.R.
# Each app keeps its own table columns (TIME_COLS); this reads the ones it finds.

# tc: the corrections table; rows: the rows that are for this logger (the app picks them: by serial, or all of them when
# a row names its file instead). A row moves the records between start and end (blank: from the first, to the last) by
# shift_minutes, or with mode "drift" spreads that shift over the span (none at its start, all of it at its end; the USGS
# proration, Wagner et al. 2006, shift_times() in timestamps.R). Optional columns: action "remove" drops the records
# instead, source_file limits the row to files whose name contains it, author is added to the reason.
# audit = FALSE leaves the audit to the caller; dedupe = TRUE sorts the records and drops one moved onto the time of
# another (for a table with one record per time). The number of records moved is attr(result, "shifted").
apply_time_corrections <- function(dt, tc, rows = seq_len(NROW(tc)), audit = TRUE, dedupe = FALSE) {
  moved <- 0L
  if (is.null(tc) || !NROW(tc) || !nrow(dt)) {
    return(setattr(dt, "shifted", moved))
  }
  for (k in rows) {
    r <- time_correction_row(dt, tc, k, audit)
    dt <- r$dt
    moved <- moved + r$moved
  }
  if (dedupe) {
    setorder(dt, TIMESTAMP)
    clash <- duplicated(dt$TIMESTAMP)
    if (any(clash)) {
      dt <- remove_rows(dt, which(clash), "stitch", "clock_fix_overlap", "a clock correction moved a record onto an existing one")
    }
  }
  setattr(dt, "shifted", moved)
}

# Cell v of row k as trimmed text ("" when the table has no such column or the cell is empty).
time_correction_cell <- function(tc, k, v) {
  x <- if (v %in% names(tc)) tc[[v]][k] else ""
  if (is.null(x) || is.na(x)) "" else trim(as.character(x))
}

# One row k of the table applied to dt: list(dt, moved). A row that cannot be read is a problem and changes nothing.
time_correction_row <- function(dt, tc, k, audit) {
  cell <- function(v) time_correction_cell(tc, k, v)
  none <- list(dt = dt, moved = 0L)
  sp <- correction_span(dt$TIMESTAMP, cell("start"), cell("end"))                       # core timestamps.R
  if ((nzchar(cell("start")) && is.na(sp$a)) || (nzchar(cell("end")) && is.na(sp$b))) {
    problem("time_corrections.csv", paste0("row ", k, ": start or end is not a date; row ignored"))
    return(none)
  }
  hit <- sp$hit
  if (nzchar(cell("source_file")) && "source_file" %in% names(dt)) {
    hit <- hit & grepl(cell("source_file"), basename(dt$source_file), fixed = TRUE)
  }
  if (!any(hit)) return(none)
  why <- paste0(cell("reason"), if (nzchar(cell("author"))) paste0(" [", cell("author"), "]") else "")
  if (tolower(cell("action")) == "remove") {
    dt <- remove_rows(dt, hit, "stitch", "clock_fix_removed", paste0("removed by a clock correction: ", why), detail = "summary")
    return(list(dt = dt, moved = 0L))
  }
  mins <- suppressWarnings(as.numeric(cell("shift_minutes")))                            # NA is reported just below
  if (is.na(mins)) {
    problem("time_corrections.csv", paste0("row ", k, ": shift_minutes is not a number; row ignored"))
    return(none)
  }
  if (mins == 0) return(none)
  drift <- identical(tolower(cell("mode")), "drift")
  a <- if (is.na(sp$a)) min(dt$TIMESTAMP, na.rm = TRUE) else sp$a
  b <- if (is.na(sp$b)) max(dt$TIMESTAMP, na.rm = TRUE) else sp$b
  if (audit) time_correction_audit(dt, hit, mins, drift, why)
  set(dt, which(hit), "TIMESTAMP", shift_times(dt$TIMESTAMP[hit], mins, if (drift) "drift" else "shift", a, b))
  list(dt = dt, moved = sum(hit))
}

# One audit row per raw file of the records a correction moves (one row when the records do not name their file).
time_correction_audit <- function(dt, hit, mins, drift, why) {
  by <- if ("source_file" %in% names(dt)) "source_file" else NULL
  sm <- dt[hit, .(n = .N, t0 = fmt_ts(min(TIMESTAMP)), t1 = fmt_ts(max(TIMESTAMP))), by = by]
  what <- if (drift) {
    sprintf("clock drift spread over the span, up to %+g minutes at its end", mins)
  } else {
    sprintf("moved by %+g minutes", mins)
  }
  audit_add("stitch", if (drift) "clock_fix_drift" else "clock_fix_shifted", "change", sm$n,
            logger_id = dt$logger_id[1], stream = if ("table" %in% names(dt)) dt$table[1] else NA,
            timestamp = sm$t0, column = "TIMESTAMP", reason = sprintf("%s records from %s to %s %s: %s", sm$n, sm$t0, sm$t1, what, why),
            source_file = if (is.null(by)) NA else sm$source_file)
}
