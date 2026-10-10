# Ovid/periods.R  --  Corrections over a period: the Workbench's period tool and its list, applied by a run.
# Part of Ovid Core (shared). Sourced by the processors' pipelines (CS, WLR, GT) and their apps.
#
# One set-up table, config/period_corrections.csv, a row per correction: on the values of `parameter` (a column name, or
# empty for every value column) of `target` (a station, site, logger or well name or serial; empty for all) from `from` to
# `to` (a date without a time runs to the end of that day), do `action` with `value`:
#   remove       the values are removed (missing)
#   offset       value added
#   scale        multiplied by value
#   drift        a share of value that grows in time from none at `from` to all of it at `to` (linear drift)
#   interpolate  filled in a straight line in time between the last value before the period and the first after it
#   flag         kept, and flagged (check "period_flag")
# Rows apply in their order, after the app's calibrations and before its quality rules; `on` = off keeps a row without
# using it. The raw files are never changed: a run reads them again and applies the list each time.
# These are the corrections of hydrometric time-series software (AQUARIUS "delete region", offset, drift, "fill gap"; ODM
# Tools; USGS proration of drift, Wagner et al. 2006, Techniques and Methods 1-D3).
#
# The table also holds what used to be separate tables (manual removals, CS calibrations, WLR drift corrections; their old
# rows are moved in once, core config.R config_migrate()). An app may add columns of its own (period_config(extra)): CS
# `output` (offset and scale write into that column, the original kept) and `stream`; WLR and GT the removal's value band
# and what else the old tables named. A target may name several stations, sites or loggers separated by |. A remove row
# (and a GT flag row) is applied by the app's own removal step, where its manual removals were; an app's own actions
# (PERIOD_APP_ACTIONS, WLR's drift choices) by the app.

PERIOD_FILE <- "period_corrections.csv"
PERIOD_COLS <- c("target", "parameter", "from", "to", "action", "value", "on", "note", "author", "added")
PERIOD_ACTIONS <- c(remove = "Remove data", offset = "Offset", scale = "Scale", drift = "Linear drift", interpolate = "Fill by interpolation", flag = "Flag only")
period_config <- function(extra = character()) list(file = PERIOD_FILE, cols = c(PERIOD_COLS, extra), optional = extra, label = "Corrections over a period")
period_actions <- function() c(PERIOD_ACTIONS, get0("PERIOD_APP_ACTIONS", ifnotfound = character()))
period_parts <- function(x) { p <- trim(strsplit(as.character(x %||% ""), "|", fixed = TRUE)[[1]]); p[nzchar(p)] }   # the names of a target cell

# The action of a row as its key: the key itself or its label (as the period tool writes it), any case.
period_action <- function(x) { x <- tolower(trim(as.character(x))); acts <- period_actions(); lab <- tolower(acts)
  k <- ifelse(x %in% names(acts), x, names(acts)[match(x, lab)])
  k[is.na(k) & x %in% c("delete", "delete region", "remove")] <- "remove"; k[is.na(k) & x %in% c("fill", "interp", "fill gaps")] <- "interpolate"
  k[is.na(k) & x %in% c("multiply", "scale x")] <- "scale"; k }
period_on <- function(x) { x <- tolower(trim(as.character(x))); !x %in% c("off", "false", "no", "0", "n") }

# The table as a run uses it: rows switched on, with an action it knows and a period that can be read; why any other
# row was left out (bad), each with its row number in the file.
# skip: actions an app applies itself (its removal step, its own actions): left out here, rows and problems alike. The app's
# own columns (output ...) come along as text.
period_read <- function(path, skip = character()) {
  d <- if (length(path) == 1 && file.exists(path)) tryCatch(read_cfg(path), error = function(e) NULL) else NULL
  empty <- data.table(row = integer(), target = character(), parameter = character(), action = character(), value = numeric(),
                      t0 = as.POSIXct(character(), tz = "UTC"), t1 = as.POSIXct(character(), tz = "UTC"), note = character())
  if (is.null(d) || !nrow(d)) return(list(rows = empty, bad = data.table(row = integer(), problem = character())))
  d <- ensure_cols(as.data.table(d), PERIOD_COLS)
  out <- data.table(row = seq_len(nrow(d)), target = trim(d$target), parameter = trim(d$parameter), action = period_action(d$action),
                    value = suppressWarnings(as.numeric(d$value)), t0 = parse_date_cell(d$from), t1 = parse_date_cell(d$to, end = TRUE),
                    note = trim(d$note), on = period_on(d$on))
  for (v in setdiff(names(d), c(PERIOD_COLS, names(out)))) set(out, j = v, value = trim(d[[v]]))
  if (length(skip)) { k <- !out$action %in% skip; d <- d[k]; out <- out[k] }
  why <- ifelse(!out$on, "", ifelse(is.na(out$action), paste0("action '", d$action, "' is not one of ", paste(names(period_actions()), collapse = ", ")),
         ifelse(is.na(out$t0) & nzchar(trim(d$from)), paste0("from '", d$from, "' is not a date"),
         ifelse(is.na(out$t1) & nzchar(trim(d$to)), paste0("to '", d$to, "' is not a date"),
         ifelse(!is.na(out$t0) & !is.na(out$t1) & out$t1 < out$t0, "to is before from",
         ifelse(out$action %in% c("offset", "scale", "drift") & is.na(out$value), paste0("a ", out$action, " needs a number in value"), ""))))))
  bad <- data.table(row = out$row[nzchar(why)], problem = why[nzchar(why)])
  list(rows = out[on & !nzchar(why), !"on"], bad = bad)
}

# Apply the rows to one record dt (a station's, logger's or site's), in order. keys: the names this record answers to
# (station, site, logger serial...); a row whose target is empty or "*" applies to every record. Only numeric columns
# change. Returns the record, a log (a row per correction that touched values) and the flagged values (parameter, idx).
# by: a column of dt whose value on each record also counts as a name (CS: the logger of each record), so a row for one
# logger changes only that logger's records. A row with an output (CS) and one column writes the offset, scaled or drifted
# values into that column (added when missing) and leaves the column itself as it was.
period_apply <- function(dt, rows, keys = character(), time = "TIMESTAMP", skip = character(), only = NULL, by = NULL) {
  log <- list(); fl <- list()
  if (is.null(rows) || !nrow(rows) || !nrow(dt) || !time %in% names(dt)) return(list(dt = dt, log = NULL, flagged = NULL))
  keys <- tolower(trim(as.character(keys[!is.na(keys)]))); t <- dt[[time]]; tn <- as.numeric(t)
  rk <- if (!is.null(by) && by %in% names(dt)) tolower(trim(as.character(dt[[by]]))) else NULL
  for (i in seq_len(nrow(rows))) { r <- rows[i]
    tg <- tolower(period_parts(r$target)); mine <- rep(TRUE, length(t))
    if (length(tg) && !"*" %in% tg && !any(tg %in% keys)) { if (is.null(rk) || !any(rk %in% tg)) next; mine <- rk %in% tg }
    cols <- if (nzchar(r$parameter) && r$parameter != "*") intersect(trim(strsplit(r$parameter, "|", fixed = TRUE)[[1]]), names(dt)) else
      setdiff(names(dt)[vapply(dt, is.numeric, NA)], c(time, skip))
    if (!is.null(only)) cols <- intersect(cols, only)                                  # this step's columns only (a run in steps)
    cols <- cols[vapply(cols, function(p) is.numeric(dt[[p]]), NA)]; if (!length(cols)) next
    hit <- mine & !is.na(t) & (is.na(r$t0) | t >= r$t0) & (is.na(r$t1) | t <= r$t1); idx <- which(hit); if (!length(idx)) next
    a0 <- if (is.na(r$t0)) min(tn[idx]) else as.numeric(r$t0); a1 <- if (is.na(r$t1)) max(tn[idx]) else as.numeric(r$t1)
    out <- trim(r$output %||% ""); out <- if (length(cols) == 1 && length(out) == 1 && !is.na(out) && nzchar(out) && r$action %in% c("offset", "scale", "drift")) out else ""
    for (p in cols) { x <- dt[[p]]; if (!is.double(x)) { x <- as.numeric(x); set(dt, j = p, value = x) }
      n <- 0L; o <- p
      if (nzchar(out)) { o <- out; if (!o %in% names(dt)) set(dt, j = o, value = NA_real_) else if (!is.double(dt[[o]])) set(dt, j = o, value = as.numeric(dt[[o]])) }
      if (r$action == "remove") { k <- idx[!is.na(x[idx])]; n <- length(k); if (n) set(dt, k, p, NA_real_) }
      else if (r$action == "offset") { k <- idx[!is.na(x[idx])]; n <- length(k); if (n) set(dt, k, o, x[k] + r$value) }
      else if (r$action == "scale") { k <- idx[!is.na(x[idx])]; n <- length(k); if (n) set(dt, k, o, x[k] * r$value) }
      else if (r$action == "drift") { k <- idx[!is.na(x[idx])]; n <- length(k)
        if (n) { f <- if (a1 > a0) (tn[k] - a0) / (a1 - a0) else rep(1, n); set(dt, k, o, x[k] + r$value * pmin(1, pmax(0, f))) } }
      else if (r$action == "interpolate") { b <- which(!is.na(x) & tn < min(tn[idx])); e <- which(!is.na(x) & tn > max(tn[idx]))
        if (length(b) && length(e)) { b <- b[which.max(tn[b])]; e <- e[which.min(tn[e])]
          v <- x[b] + (x[e] - x[b]) * (tn[idx] - tn[b]) / (tn[e] - tn[b]); n <- length(idx); set(dt, idx, p, v) } }
      else if (r$action == "flag") { k <- idx[!is.na(x[idx])]; n <- length(k); if (n) fl[[length(fl) + 1L]] <- data.table(parameter = p, idx = k, row = r$row, note = r$note) }
      if (n) log[[length(log) + 1L]] <- data.table(row = r$row, target = r$target, parameter = if (o != p) paste(p, "to", o) else p, action = r$action, value = r$value,
                                                   from = fmt_ts(t[min(idx)]), to = fmt_ts(t[max(idx)]), values = n, note = r$note) }
  }
  list(dt = dt, log = if (length(log)) rbindlist(log) else NULL, flagged = if (length(fl)) rbindlist(fl) else NULL)
}

# period_read() for rows in hand (a data.table with PERIOD_COLS) instead of a file.
period_read_rows <- function(d) { f <- tempfile(fileext = ".csv"); on.exit(unlink(f)); fwrite(d, f); period_read(f) }

# The table as a pipeline loads it (rows ready to apply); a row that cannot be used is a run problem, not a stop. skip: as
# period_read().
period_load <- function(path, skip = character()) { x <- period_read(path, skip)
  for (i in seq_len(nrow(x$bad))) problem(basename(path), paste0("row ", x$bad$row[i], ": ", x$bad$problem[i], "; the row was not applied"))
  x$rows }
# The rows an app applies itself (actions), as written (text, every column), switched on, with their row number in the file
# (row) and the action as its key. The app reads them as it read its old table (period_view()), problems and all.
period_own_rows <- function(path, actions) {
  d <- if (length(path) == 1 && file.exists(path)) tryCatch(read_cfg(path), error = function(e) NULL) else NULL
  if (is.null(d) || !nrow(d)) return(cbind(empty_cfg(PERIOD_COLS), data.table(row = integer())))
  d <- ensure_cols(as.data.table(d), PERIOD_COLS); d[, row := .I]; d[, action := period_action(action)]
  d[period_on(on) & action %in% actions]
}
# The rows that can touch a record answering to the names `who` (for a cache key: a change elsewhere keeps it).
period_for <- function(rows, who) { if (is.null(rows) || !nrow(rows)) return(rows); w <- tolower(trim(as.character(who)))
  rows[vapply(target, function(x) { p <- tolower(period_parts(x)); !length(p) || "*" %in% p || any(p %in% w) }, NA)] }
# One step of a run: apply the rows to a record, write what each did to the audit trail (stage "period") and flag the
# values of flag rows through the app's flag(dt, parameter, idx, check, detail). where: the app's audit keys for the record.
# on_log(log): what each row did (period_apply()'s log), for an app that lists it apart as well.
period_step <- function(dt, rows, keys, flag = NULL, where = list(), time = "TIMESTAMP", skip = character(), only = NULL, by = NULL, on_log = NULL) {
  o <- period_apply(dt, rows, keys, time, skip, only, by)
  if (is.function(on_log) && !is.null(o$log)) on_log(o$log)
  if (!is.null(o$log)) for (i in seq_len(nrow(o$log))) { l <- o$log[i]
    do.call(audit_add, c(list(stage = "period", rule = paste0("period_", l$action), level = "summary", n = l$values,
      reason = paste0(PERIOD_ACTIONS[[l$action]], if (!is.na(l$value)) paste0(" ", l$value), " from ", l$from, " to ", l$to, " (row ", l$row, " of ", PERIOD_FILE, ")",
                      if (nzchar(l$note)) paste0(": ", l$note))), where, list(column = l$parameter))) }
  if (!is.null(o$flagged) && is.function(flag)) for (g in split(o$flagged, by = c("parameter", "row")))
    flag(o$dt, g$parameter[1], g$idx, "period_flag", paste0("flagged by row ", g$row[1], " of ", PERIOD_FILE, if (nzchar(g$note[1])) paste0(": ", g$note[1])))
  o$dt
}

# ---- an old table as a view of this one ------------------------------------------------------------------------------------
# The pages that wrote manual removals, calibrations or drift rows keep writing them in the old shape; the view turns them
# into rows of this table (to_period(old rows) -> period rows) and shows this table's rows of `actions` in the old shape
# (from_period(period rows) -> old rows, keeping `row`, the row's number in this table). ed: the app's editor of this table
# (core editor.R). Returns what the pages use of an editor: data(), append(rows), mutate(fn) and the rest as no-ops.
period_view <- function(ed, actions, to_period, from_period) {
  own <- function(d) { d <- ensure_cols(as.data.table(d %||% data.frame()), PERIOD_COLS); if (!nrow(d)) return(integer())
    which(period_on(d$on) & period_action(d$action) %in% actions) }
  as_old <- function(d) { d <- ensure_cols(as.data.table(d %||% data.frame()), PERIOD_COLS); i <- own(d)
    x <- d[i]; if (!nrow(x)) x <- d[0]; x[, action := period_action(action)]
    out <- as.data.table(from_period(x)); out[, row := as.character(i)]; out[] }
  rows_of <- function(old, cols) { r <- ensure_cols(as.data.table(to_period(as.data.table(old)))[, lapply(.SD, as.character)], cols)
    r[, added := ifelse(nzchar(added), added, format(Sys.time(), "%Y-%m-%d %H:%M"))]; r[, cols, with = FALSE] }
  list(data = function() { d <- ed$data(); if (is.null(d)) NULL else as.data.frame(as_old(d), stringsAsFactors = FALSE) },
       append = function(rows) { cols <- names(ed$data() %||% data.frame()); if (!length(cols)) cols <- PERIOD_COLS
         ed$append(as.data.frame(rows_of(as.data.table(rows)[, setdiff(names(rows), "row"), with = FALSE], cols), stringsAsFactors = FALSE)) },
       mutate = function(fn) ed$mutate(function(d) { d <- as.data.table(d); cols <- names(d); i <- own(d); was <- as_old(d)
         now <- as.data.table(fn(as.data.frame(was, stringsAsFactors = FALSE))); now <- ensure_cols(now, "row")
         key <- function(x) do.call(paste, c(lapply(x, as.character), sep = "\r"))
         same <- now[nzchar(row) & key(now) %in% key(was)]                                   # rows left as they were keep theirs
         keep <- setdiff(seq_len(nrow(d)), i[!as.character(i) %in% same$row])
         new <- now[!(nzchar(row) & key(now) %in% key(was))][, row := NULL]
         as.data.frame(rbind(d[keep], if (nrow(new)) rows_of(new, cols)), stringsAsFactors = FALSE) }),
       save = function(...) invisible(), dirty = function() FALSE, fresh = function() TRUE, path = ed$path, discard = function() invisible(),
       selected = function() integer(), select = function(rows) invisible())
}

# The note above the corrections list on a Standard page (the list itself is the app's editor for PERIOD_FILE).
period_help <- function() shiny::p(class = "dim", "Corrections over a period, applied in this order after the calibrations and before the quality rules. ",
  shiny::code("target"), ": a station, site or logger (blank: all); ", shiny::code("parameter"), ": a column name, several with | (blank: every value); ",
  shiny::code("from"), " and ", shiny::code("to"), ": dates or date-times (a date alone runs to the end of that day); ", shiny::code("action"), ": ",
  paste(names(PERIOD_ACTIONS), collapse = ", "), "; ", shiny::code("value"), ": the number for offset, scale and drift; ", shiny::code("on"), ": off keeps the row without using it. ",
  "The easiest way to add one is Correct a period in the Workbench: select cells or drag across the chart. The raw files are never changed.")
