# Ovid/audit.R  --  The audit trail: the only ways data can leave the dataset, and how each removal is recorded.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# The rule in every app: nothing is dropped or blanked except through remove_rows() and remove_cells(), and the run
# stops if counts do not reconcile against this log. Raw files are never modified.
#
# Each app describes its records once with audit_schema() (in its own R/ files): which key columns an audit line
# carries, how the index groups them, and how a "summary" line groups many removed rows. Wide records (one row per
# time step, CS and WLR) remove whole records with remove_rows() and single values with remove_cells(). Long records
# (one row per value, GT) remove values with remove_rows(); set long = TRUE so each line keeps the old value.

AUDIT <- new.env()

AUDIT_SCHEMA <- list(
  keys       = c("site", "logger_id", "timestamp", "column", "old_value"),   # columns between level and n, in this order
  index_by   = c("site", "logger_id", "column"),                              # audit_index.csv groups (after stage, rule, level)
  summary_by = c("site", "logger_id", "source_file"),                         # one summary line per group
  long       = FALSE,                 # TRUE: one row per value, with a `value` column (GT)
  detail     = "each",                # default detail for remove_rows(): "each", "summary" or "auto"
  auto_limit = 200L,                  # "auto": a line per row up to this many rows, else summary lines
  level_each = "row", level_summary = "row",
  cells_skip_na = TRUE,               # remove_cells() leaves values that are already NA alone (and unlogged)
  on_remove  = NULL)                  # function(d, stage, rule): called with the rows remove_rows() is about to drop

audit_schema <- function(...) { a <- list(...); for (k in names(a)) AUDIT_SCHEMA[k] <<- a[k]; invisible(AUDIT_SCHEMA) }

audit_reset <- function(dir = NULL) {
  AUDIT$log <- list(); AUDIT$index <- list(); AUDIT$n <- 0; AUDIT$dir <- dir; AUDIT$problems <- list(); AUDIT$by_file <- list()
  AUDIT$stash <- list()
  AUDIT$run_id <- format(Sys.time(), "%Y%m%dT%H%M%S")
}

# Things that went wrong but did not stop the run (a file that could not be read, a bad pattern in a set-up table).
# Shown at the end and in the app.
problem <- function(where, what) {
  AUDIT$problems[[length(AUDIT$problems) + 1L]] <- data.table(where = as.character(where), problem = as.character(what))
  say("  ! ", where, ": ", what, col = "orange")
}

# One problem line for a file whose reader left cells blank because they were not numbers (convert.R num_cells()).
unread_report <- function(meta, file) {
  n <- meta$unreadable %||% 0L
  if (n > 0) problem(file, sprintf("%s value(s) are not numbers and were left blank, e.g. '%s'", n, meta$unreadable_example %||% ""))
}

# The value of expr, or default when it fails. The failure goes on the problem list with where, so a part that could
# not be made is never mistaken for one with nothing in it (CONTRIBUTING rule 13).
try_or <- function(expr, default = NULL, where = "") {
  tryCatch(expr, error = function(e) {
    problem(where, conditionMessage(e))
    default
  })
}

# The problems added after the first n, as a table (for an export or a page that wants only its own).
problems_since <- function(n = 0L) {
  p <- AUDIT$problems
  if (length(p) <= n) return(data.table(where = character(), problem = character()))
  rbindlist(p[(n + 1L):length(p)], fill = TRUE)
}

# One audit line (or one per element when the arguments are vectors). Key columns are passed by name; a name the
# app's schema does not carry is ignored, a key that is not passed is NA.
audit_add <- function(stage, rule, level, n = 1L, ..., reason = NA, source_file = NA) {
  a <- list(...)
  cols <- list(run_id = AUDIT$run_id, stage = stage, rule = rule, level = level)
  for (k in AUDIT_SCHEMA$keys) { v <- a[[k]]; if (is.null(v)) v <- NA
    cols[[k]] <- if (k == "timestamp") fmt_ts(v) else as.character(v) }
  cols$n <- as.integer(n); cols$reason <- as.character(reason); cols$source_file <- as.character(source_file)
  AUDIT$log[[length(AUDIT$log) + 1L]] <- as.data.table(cols)
  invisible(NULL)
}

audit_table <- function() {
  if (length(AUDIT$log)) return(rbindlist(AUDIT$log, use.names = TRUE))
  cols <- c("run_id", "stage", "rule", "level", AUDIT_SCHEMA$keys)
  e <- as.data.table(setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  e[, n := integer()][, reason := character()][, source_file := character()][]
}

# Audit entries are written to disk group by group (output/audit/detail/<key>.csv) so a large archive never has to
# hold millions of entries in memory. Only a compact index stays in memory.
audit_flush <- function(key) {
  b <- audit_table(); AUDIT$log <- list()
  if (!nrow(b)) return(invisible(0L))
  file <- paste0(safe_name(key), ".csv")
  rng <- function(x, f) { x <- x[!is.na(x) & nzchar(x)]; if (length(x)) f(x) else NA_character_ }
  AUDIT$index[[length(AUDIT$index) + 1L]] <- b[, .(n = sum(n, na.rm = TRUE), entries = .N, first = rng(timestamp, min), last = rng(timestamp, max),
                                                   detail_file = file), by = c("stage", "rule", "level", AUDIT_SCHEMA$index_by)]
  AUDIT$n <- AUDIT$n + nrow(b)
  AUDIT$by_file[[length(AUDIT$by_file) + 1L]] <- b[!is.na(source_file), .(n = sum(n, na.rm = TRUE), entries = .N), by = .(source_file, stage, rule, level)]
  if (!is.null(AUDIT$dir)) { f <- file.path(AUDIT$dir, file); fwrite(b, f, append = file.exists(f)) }
  invisible(nrow(b))
}

.col <- function(dt, nm, idx) if (nm %in% names(dt)) dt[[nm]][idx] else rep(NA, length(idx))

# The value of audit key k for rows idx of a record. Most keys are columns of the same name; a few are spelled
# differently in the records (TIMESTAMP, RECORD, a CS logger table that has no stream yet).
.key_of <- function(dt, k, idx) switch(k,
  timestamp = dt$TIMESTAMP[idx],
  record    = .col(dt, "RECORD", idx),
  stream    = if ("stream" %in% names(dt)) dt$stream[idx] else .col(dt, "table", idx),
  .col(dt, k, idx))

.keys_of <- function(dt, idx, skip = character()) { k <- setdiff(AUDIT_SCHEMA$keys, c("column", "old_value", skip))
  setNames(lapply(k, function(x) .key_of(dt, x, idx)), k) }

# Blank individual values (set to NA) and log each one.
remove_cells <- function(dt, col, idx, stage, rule, reason) {
  if (is.logical(idx)) idx <- which(idx)
  if (isTRUE(AUDIT_SCHEMA$cells_skip_na)) idx <- idx[!is.na(dt[[col]][idx])]
  if (!length(idx)) return(invisible(0L))
  do.call(audit_add, c(list(stage, rule, "cell", 1L), .keys_of(dt, idx),
                       list(column = col, old_value = dt[[col]][idx], reason = reason, source_file = .col(dt, "source_file", idx))))
  set(dt, i = idx, j = col, value = NA)
  invisible(length(idx))
}

# Drop whole rows and log them; returns the record without them. detail = "each" logs one line per row (on a long
# record the old value is kept); "summary" one line per group of AUDIT_SCHEMA$summary_by with a count and a time span
# (used where the dropped rows are copies, error codes or long runs, where a line per row would only bury the rest);
# "auto" picks "each" up to auto_limit rows.
remove_rows <- function(dt, idx, stage, rule, reason, detail = AUDIT_SCHEMA$detail) {
  if (is.logical(idx)) idx <- which(idx)
  idx <- unique(idx[!is.na(idx)])
  if (!length(idx)) return(dt)
  d <- dt[idx]; i <- seq_len(nrow(d)); long <- isTRUE(AUDIT_SCHEMA$long)
  if (is.function(AUDIT_SCHEMA$on_remove)) AUDIT_SCHEMA$on_remove(d, stage, rule)
  if (detail == "auto") detail <- if (length(idx) <= AUDIT_SCHEMA$auto_limit) "each" else "summary"
  if (detail == "each") {
    do.call(audit_add, c(list(stage, rule, AUDIT_SCHEMA$level_each, 1L), .keys_of(d, i),
                         list(column = "<record>", old_value = if (long) signif(d$value, 7) else NA, reason = reason,
                              source_file = .col(d, "source_file", i))))
  } else {
    by <- AUDIT_SCHEMA$summary_by
    g <- as.data.table(setNames(lapply(by, function(x) { v <- .key_of(d, x, i); if (all(is.na(v))) rep(NA_character_, length(i)) else v }), by))
    g[, .t := d$TIMESTAMP]; if (long) g[, .v := d$value]
    s <- g[, c(list(n = .N, t0 = suppressWarnings(min(.t, na.rm = TRUE)), t1 = suppressWarnings(max(.t, na.rm = TRUE))),
               if (long) list(lo = suppressWarnings(min(.v, na.rm = TRUE)), hi = suppressWarnings(max(.v, na.rm = TRUE)))), by = by]
    span <- paste0(fmt_ts(s$t0), " to ", fmt_ts(s$t1))
    why <- if (long) paste0(reason, " (", s$n, " values, ", span, ifelse(is.finite(s$lo), sprintf(", values %.4g to %.4g", s$lo, s$hi), ""), ")")
           else paste0(reason, " (", s$n, " records, ", span, ")")
    keys <- setNames(lapply(setdiff(by, "source_file"), function(x) s[[x]]), setdiff(by, "source_file"))
    do.call(audit_add, c(list(stage, rule, AUDIT_SCHEMA$level_summary, s$n), keys,
                         list(timestamp = s$t0, column = "<record>", reason = why, source_file = if ("source_file" %in% by) s$source_file else NA)))
  }
  dt[-idx]
}

# A step read back from the cache brings the audit entries it made when it ran; they are stamped with this run's id.
audit_restamp <- function(log) lapply(log, function(x) { x <- copy(x); set(x, j = "run_id", value = AUDIT$run_id); x })
