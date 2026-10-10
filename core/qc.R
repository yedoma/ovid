# core/qc.R  --  The generic mechanics of quality checks, shared by the apps.
#
# What goes here: tests that depend only on a series of values and times, never on what is measured. What does not: rule
# tables, flag names, default thresholds and data-specific checks (sunlight at night, readings in the air, zero curtain,
# between-site comparison). Those stay in each app and call these functions.
#
# Every function takes plain vectors and returns row positions; the app decides whether a position is flagged or removed
# (through add_flag() or remove_cells()). Positions are in the order of the vectors passed in. `group` splits the vectors
# into series that never mix (one logger channel, one deployment): neighbours and runs stop at a group edge, and the vectors
# must already be in time order within each group.

# The run number of each row position: 1, 1, 1, 2, 2, 3 ... for positions that are consecutive or not. idx must be sorted.
qc_run_id <- function(idx) cumsum(c(1L, diff(idx) != 1L))

# Contiguous runs of row positions: one row per run (start, end, records). Used to write one flag line per run.
qc_runs <- function(idx) {
  idx <- sort(unique(idx[!is.na(idx)]))
  if (!length(idx)) return(data.table(start = integer(), end = integer(), records = integer()))
  data.table(i = idx, run = qc_run_id(idx))[, .(start = min(i), end = max(i), records = .N), by = run][, run := NULL][]
}

# Single-point spikes: a value that differs from both neighbours by more than a threshold, in the same direction (it jumped
# and came straight back). Neighbours are the adjacent records in the same group, whatever the time between them.
#   limit    the least jump that counts
#   k        0 uses `limit` alone; above 0 the threshold is the larger of `limit` and k times the usual size of a step in that
#            group (1.4826 * the median absolute deviation of the steps), so a noisy sensor needs a bigger jump
#   skip_na  TRUE ignores missing values, so a value next to a gap is compared with the nearest reading
#   confirm  0 (default) as above; an odd number of 3 or more also asks that the value be more than the threshold away from
#            the running median of that many readings around it. A good reading between two dropouts (1.45, 4.22, 1.45, 4.22,
#            1.50) looks like a spike to its neighbours but sits on the median, so it is kept.
qc_spike_idx <- function(x, limit, k = 0, group = NULL, skip_na = FALSE, confirm = 0L) {
  n <- length(x); if (n < 3) return(integer())
  d <- data.table(i = seq_len(n), x = as.numeric(x), g = if (is.null(group)) 1L else group)
  if (skip_na) d <- d[!is.na(x)]
  if (nrow(d) < 3) return(integer())
  d[, `:=`(d1 = x - shift(x), d2 = x - shift(x, -1L)), by = g]
  if (k > 0) d[, thr := pmax(limit, k * 1.4826 * stats::mad(d1, na.rm = TRUE)), by = g] else d[, thr := limit]
  if (confirm >= 3) { w <- as.integer(confirm) + (as.integer(confirm) %% 2L == 0L)
    d[, med := { m <- rep(NA_real_, .N); ok <- !is.na(x); if (sum(ok) >= w) m[ok] <- stats::runmed(x[ok], w, endrule = "median"); m }, by = g]
    return(d[!is.na(d1) & !is.na(d2) & sign(d1) == sign(d2) & pmin(abs(d1), abs(d2)) > thr & (is.na(med) | abs(x - med) > thr), i]) }
  d[!is.na(d1) & !is.na(d2) & sign(d1) == sign(d2) & pmin(abs(d1), abs(d2)) > thr, i]
}

# Stuck or flat: runs of the same value that last `hours` or more.
#   min_n    the fewest readings in a run (3 for most; 1 lets two readings a day apart count)
#   group    runs stop at a group edge
#   band     if given, a run whose value is within `band` of zero is exempt (a ground temperature that sits at 0 is the
#            zero curtain, not a stuck sensor); NULL exempts nothing
#   skip_na  TRUE ignores missing values, so a run continues across them; FALSE ends a run at a missing value
# t is the time of each reading (POSIXct or seconds).
qc_flat_idx <- function(x, t, hours, min_n = 3L, group = NULL, band = NULL, skip_na = TRUE) {
  n <- length(x); if (!n) return(integer())
  d <- data.table(i = seq_len(n), x = as.numeric(x), t = as.numeric(t), g = if (is.null(group)) 1L else group)
  if (skip_na) d <- d[!is.na(x)]
  if (!nrow(d)) return(integer())
  d[, run := rleid(g, x)]
  r <- d[!is.na(x), .(t0 = t[1], t1 = t[.N], n = .N, v = x[1]), by = run]
  r <- r[(t1 - t0) / 3600 >= hours & n >= min_n]
  if (!is.null(band)) r <- r[abs(v) > band]
  if (!nrow(r)) return(integer())
  d[run %in% r$run & !is.na(x), i]
}

# ---- WMO quality control codes ---------------------------------------------------------------------------------------
# WMO BUFR code table 0 33 020, "Quality control indication of following value" (codes.wmo.int/bufr4/codeflag/0-33-020).
# These are the codes written to exports. Inside the apps a flag still names the check that failed; the code says what
# that check found out about the value.
WMO_QC <- data.table(code = 0:7,
  name = c("good", "inconsistent", "doubtful", "wrong", "not_checked", "changed", "estimated", "missing"),
  meaning = c("passed every check that was run", "failed a check against another parameter, sensor or station",
              "failed a plausibility check (jump, stuck value, no change); may still be right",
              "failed a limit check or was found to be an error", "no quality check was run on it",
              "corrected after it was read (calibration, drift, clock)", "filled or computed from other data",
              "no value"))

# Which code a check gives. Generic checks are here; each app adds its own with qc_wmo_register(). A check the table does
# not know is treated as doubtful (2), never as good.
QC_WMO_CHECKS <- new.env()
QC_WMO_CHECKS$map <- c(range_min = 3L, range_max = 3L, sentinel = 3L, manual = 3L, manual_records = 3L,
                       spike = 2L, step = 2L, flat_line = 2L, persistence = 2L, manual_flag = 2L)
qc_wmo_register <- function(map) { m <- QC_WMO_CHECKS$map; m[names(map)] <- as.integer(map); QC_WMO_CHECKS$map <- m; invisible(m) }
qc_wmo_class <- function(check) { m <- QC_WMO_CHECKS$map; k <- unname(m[as.character(check)]); k[is.na(k)] <- 2L; k }

# One code for a value that several checks have spoken about. A failure outranks everything else, and among failures wrong
# outranks inconsistent outranks doubtful; then estimated, changed, good, not checked; missing counts only when nothing else
# applies. a and b are vectors of codes of the same length.
WMO_RANK <- c(`3` = 1L, `1` = 2L, `2` = 3L, `6` = 4L, `5` = 5L, `0` = 6L, `4` = 7L, `7` = 8L)
qc_wmo_combine <- function(a, b) { ra <- WMO_RANK[as.character(a)]; rb <- WMO_RANK[as.character(b)]; unname(ifelse(ra <= rb, a, b)) }

# The worst code among several (a mean or a day's readings are given the worst code of the values in them).
qc_wmo_worst <- function(codes) { r <- min(WMO_RANK[as.character(unique(codes))]); as.integer(names(WMO_RANK)[match(r, WMO_RANK)]) }

# Runs of a condition that last `hours` or more: the row positions of every run of TRUE in `cond` (missing counts as FALSE) that
# spans at least that long. t is the time of each row. For tests such as "no wind while it is below freezing and humid".
qc_cond_runs_idx <- function(cond, t, hours) {
  cond[is.na(cond)] <- FALSE; if (!any(cond)) return(integer())
  r <- rle(cond); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1; tt <- as.numeric(t)
  long <- which(r$values & (tt[ends] - tt[starts]) / 3600 >= hours)
  if (!length(long)) return(integer())
  as.integer(unlist(lapply(long, function(j) starts[j]:ends[j])))
}

# A step: a change between two consecutive readings faster than `per_hour` (units of the value per hour). It is a rate, so the
# same limit means the same thing at a 10-minute and at an hourly interval. Readings further apart than `max_gap_hours` are not
# compared. The later reading of the pair is returned. min_hours: readings closer than this are judged on their change against
# per_hour * min_hours, so a one-minute wobble is not read as a fast rate (1 = no rate is taken over less than an hour).
qc_step_idx <- function(x, t, per_hour, max_gap_hours = Inf, group = NULL, min_hours = 0) {
  n <- length(x); if (n < 2) return(integer())
  d <- data.table(i = seq_len(n), x = as.numeric(x), t = as.numeric(t), g = if (is.null(group)) 1L else group)
  d[, `:=`(dx = x - shift(x), dt = (t - shift(t)) / 3600), by = g]
  d[!is.na(dx) & !is.na(dt) & dt > 0 & dt <= max_gap_hours & abs(dx) / pmax(dt, min_hours) > per_hour, i]
}

# ---- Rule ids and the common flags table ------------------------------------------------------------------------------------
# Every row of a rule table carries a rule_id that stays with it, so a flag can name the row that made it even after rows are
# added, edited or reordered. A row without one (or with a copy of another row's) gets the next free number: Q-01, Q-02 ...
# The table is returned with the ids; qc_read_rules() also writes them back to the file the first time.
qc_rule_ids <- function(tab, prefix = "Q") {
  if (is.null(tab)) return(tab)
  tab <- as.data.table(tab); if (!"rule_id" %in% names(tab)) tab[, rule_id := ""]
  id <- trimws(as.character(tab$rule_id)); id[is.na(id)] <- ""; id[duplicated(id) & nzchar(id)] <- ""
  rx <- paste0("^", prefix, "-([0-9]+)$"); used <- suppressWarnings(as.integer(sub(rx, "\\1", id[grepl(rx, id)])))
  nxt <- max(c(0L, used), na.rm = TRUE)
  for (k in which(!nzchar(id))) { nxt <- nxt + 1L; id[k] <- sprintf("%s-%02d", prefix, nxt) }
  set(tab, j = "rule_id", value = id); tab[]
}

# Read a rule table with read_cfg(), give it the columns `cols` and its rule ids, and save the ids if any were new (the old
# version goes to config/history first).
qc_read_rules <- function(path, cols, prefix = "Q") {
  if (!file.exists(path)) return(qc_rule_ids(empty_cfg(unique(c(cols, "rule_id"))), prefix))
  q0 <- ensure_cols(read_cfg(path), cols); q <- qc_rule_ids(q0, prefix)
  if (!"rule_id" %in% names(q0) || !identical(q0$rule_id, q$rule_id)) {
    backup_cfg(path, q, "pipeline"); fwrite(q, path); say("  ", basename(path), ": gave ", sum(if ("rule_id" %in% names(q0)) q0$rule_id != q$rule_id else TRUE), " rule(s) an id", col = "yellow") }
  q
}

# Flags: values kept but marked. Each app adds rows with its own columns for where the value is (station and stream, or
# site, logger and channel) and then start, end, records, check, detail, rule_id; a run starts with FLAGS$list empty.
# flag_mark() also marks the values on the record: their checks, ";"-separated, in column `col`.
FLAGS <- new.env(); FLAGS$list <- list()
flag_add <- function(rows) { if (nrow(rows)) FLAGS$list[[length(FLAGS$list) + 1L]] <- rows; invisible(nrow(rows)) }
flag_mark <- function(dt, col, idx, check) {
  if (!col %in% names(dt)) set(dt, j = col, value = "")
  old <- dt[[col]][idx]; set(dt, i = idx, j = col, value = ifelse(nzchar(old), paste(old, check, sep = ";"), check)); invisible(dt)
}

# flags.csv, the same in every app: the app's own columns (where, what, start, end, records, check, detail) and then the
# common ones: rule_id (the row of the rule table that raised it, or the check's name for a built-in check), qc_code (the WMO
# code the check maps to, see qc_wmo_class()), app and run_id. `empty` is the table to write when nothing was flagged.
FLAG_COMMON <- c("rule_id", "qc_code", "app", "run_id")
qc_flags_table <- function(flags, empty, app, wmo = TRUE) {
  fl <- if (length(flags)) rbindlist(flags, fill = TRUE) else as.data.table(empty)
  if (!"rule_id" %in% names(fl)) fl[, rule_id := character(nrow(fl))]
  fl[is.na(rule_id) | !nzchar(rule_id), rule_id := check]
  fl[, qc_code := if (wmo) qc_wmo_class(check) else rep(NA_integer_, .N)]
  fl[, `:=`(app = rep(app, .N), run_id = rep(if (is.null(AUDIT$run_id)) "" else as.character(AUDIT$run_id), .N))]
  setcolorder(fl, c(setdiff(names(fl), FLAG_COMMON), FLAG_COMMON))[]
}
