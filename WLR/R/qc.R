# R/qc.R  --  Quality checks: limits, spikes, flat lines, out-of-water periods, service visits,
# freezing, and manual removals. Every removal goes through remove_rows()/remove_cells(); flags keep the value.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

# Flags: values kept but marked suspect. One row per contiguous run.
# (FLAGS, flag_add() and flag_mark() are core qc.R.)
add_flag <- function(dt, col, idx, check, detail, rule_id = check) {
  if (is.logical(idx)) idx <- which(idx)
  if (!length(idx)) return(invisible(0L))
  idx <- sort(unique(idx)); r <- qc_runs(idx)
  flag_add(data.table(site = .col(dt, "site", 1L), logger_id = .col(dt, "logger_id", 1L), parameter = col,
    start = fmt_ts(dt$TIMESTAMP[r$start]), end = fmt_ts(dt$TIMESTAMP[r$end]), records = r$records, check = check, detail = detail, rule_id = rule_id))
  flag_mark(dt, paste0("flag_", col), idx, check)
  invisible(length(idx))
}

QC_COLS <- c("applies_to", "parameter", "min", "max", "max_spike", "max_flat_hours", "action", "notes", "rule_id")
default_qc <- function() qc_rule_ids(data.table(
  applies_to = c("water", "water", "water", "baro", "baro", "ec", "ec", "all"),
  parameter  = c("pres_kpa", "depth_m", "temp_c", "pres_kpa", "temp_c", "spc_uscm", "temp_c", "pres_kpa"),
  min        = c("50", "-0.05", "-40", "90", "-55", "0", "-5", ""),
  max        = c("250", "", "50", "110", "45", "5000", "40", ""),
  max_spike  = c("5", "0.5", "", "1.5", "", "500", "", ""),
  max_flat_hours = c("", "", "", "", "", "", "", "48"),
  action     = c("remove", "flag", "remove", "remove", "remove", "remove", "remove", "remove"),
  notes      = c("water pressure outside anything a water level logger here can read", "water below the sensor: logger likely out of water",
                 "", "air pressure outside what is possible at this elevation", "", "", "", "a reading stuck on one value for two days")), "Q")

# parameter: blank = every value, or names separated by |, or "records" to drop whole records (a logger launched before it
# went into the water, a bad stretch). min_value / max_value (optional) limit the removal to values inside that band,
# so a box dragged around a few bad points removes only those points.
MANUAL_COLS <- c("serial", "site", "start", "end", "parameter", "min_value", "max_value", "reason", "author")
OOW_COLS <- c("serial", "site", "start", "end", "cause", "reason", "author")

applies <- function(rule, kind, serial, site) {
  a <- trim(rule); if (!nzchar(a) || a == "all") return(TRUE)
  p <- trim(strsplit(a, "|", fixed = TRUE)[[1]])
  any(p == kind | norm_serial(p) == serial & nzchar(norm_serial(p)) | p == site)
}

apply_limits <- function(dt, rules, kind) {
  for (k in seq_len(nrow(rules))) {
    r <- rules[k]; p <- r$parameter
    if (!p %in% names(dt) || !applies(r$applies_to, kind, dt$logger_id[1], dt$site[1])) next
    x <- dt[[p]]; act <- tolower(r$action); lo <- suppressWarnings(as.numeric(r$min)); hi <- suppressWarnings(as.numeric(r$max))
    rid <- if (nzchar(r$rule_id %||% "")) r$rule_id else NULL
    do <- function(idx, rule, why) if (act == "flag") add_flag(dt, p, idx, rule, why, rid %||% rule) else remove_cells(dt, p, idx, "qc", rule, why)
    if (!is.na(lo)) do(which(x < lo), "range_min", sprintf("%s below %g%s", p, lo, if (nzchar(r$notes)) paste0(": ", r$notes) else ""))
    if (!is.na(hi)) do(which(dt[[p]] > hi), "range_max", sprintf("%s above %g%s", p, hi, if (nzchar(r$notes)) paste0(": ", r$notes) else ""))
    sp <- suppressWarnings(as.numeric(r$max_spike))
    if (!is.na(sp)) { i <- qc_spike_idx(dt[[p]], sp)
      if (length(i)) add_flag(dt, p, i, "spike", sprintf("%s jumped more than %g and came back", p, sp), rid %||% "spike") }
    fh <- suppressWarnings(as.numeric(r$max_flat_hours))
    if (!is.na(fh)) { idx <- qc_flat_idx(dt[[p]], dt$TIMESTAMP, fh)
      if (length(idx)) do(idx, "flat_line", sprintf("%s did not change for %g hours or more", p, fh)) }
  }
  dt
}

# A row's serial or site may hold several, separated by | (rows of the corrections over a period name both, and "*" for all);
# also: other names the record answers to (its deployment).
wlr_row_hits <- function(tab, serial, site, also = character()) {
  one <- function(x, f) vapply(as.character(x), function(v) { p <- trim(strsplit(v, "|", fixed = TRUE)[[1]]); p <- p[nzchar(p) & !is.na(p)]; f(p) }, NA, USE.NAMES = FALSE)
  s0 <- norm_serial(serial); a <- trim(also[nzchar(also) & !is.na(also)])
  one(tab$serial, function(p) "*" %in% p || (length(p) > 0 && any(nzchar(norm_serial(p)) & norm_serial(p) == s0)) || any(p %in% a)) |
    one(tab$site, function(p) "*" %in% p || any(p == trim(site)) || any(p %in% a))
}
span_rows <- function(tab, serial, site, also = character()) {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  hit <- wlr_row_hits(tab, serial, site, also)
  r <- tab[hit]
  if (!nrow(r)) return(NULL)
  r[, `:=`(a = parse_date_cell(start), b = parse_date_cell(end, end = TRUE))]     # set-up cells; an end date covers the whole day
  bad <- (nzchar(r$start) & is.na(r$a)) | (nzchar(r$end) & is.na(r$b))
  if (any(bad)) problem("set-up table", paste0("serial ", r$serial[bad], ": start or end is not a date ('", r$start[bad], "' / '", r$end[bad], "'); that row was skipped"))
  r[!nzchar(start), a := as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")][!nzchar(end), b := as.POSIXct(Inf, origin = "1970-01-01", tz = "UTC")]
  r[!bad]
}

# Values worked out from the depth that a removal may name as well (as a row of the corrections over a period could before).
wlr_more_vals <- function(dt) intersect(c("baro_kpa", "dp_kpa", "dtw_m", "wt_ground_m"), names(dt))
apply_manual <- function(dt, manual, oow, kind) {
  vals <- intersect(c("pres_kpa", "depth_m", "elev_m", "temp_c", "spc_uscm"), names(dt))
  m <- span_rows(manual, dt$logger_id[1], dt$site[1], if ("deployment" %in% names(dt)) dt$deployment[1] else character())
  why <- function(k) paste0(m$reason[k], if (nzchar(m$author[k] %||% "")) paste0(" [", m$author[k], "]") else "", if (!is.null(m$row)) paste0(" (row ", m$row[k], " of ", PERIOD_FILE, ")") else "")
  drop <- integer()
  for (k in seq_len(NROW(m))) { idx <- which(dt$TIMESTAMP >= m$a[k] & dt$TIMESTAMP <= m$b[k])
    if (tolower(trim(m$parameter[k])) %in% c("records", "<records>", "<record>")) { drop <- c(drop, k); next }
    cols <- if (nzchar(m$parameter[k]) && m$parameter[k] != "*") intersect(trim(strsplit(m$parameter[k], "|", fixed = TRUE)[[1]]), c(vals, wlr_more_vals(dt))) else vals
    lo <- suppressWarnings(as.numeric(m$min_value[k] %||% "")); hi <- suppressWarnings(as.numeric(m$max_value[k] %||% ""))
    for (p in cols) { j <- idx
      if (!is.na(lo)) j <- j[!is.na(dt[[p]][j]) & dt[[p]][j] >= lo]; if (!is.na(hi)) j <- j[!is.na(dt[[p]][j]) & dt[[p]][j] <= hi]
      remove_cells(dt, p, j, "qc", "manual", why(k)) } }
  for (k in drop) dt <- remove_rows(dt, which(dt$TIMESTAMP >= m$a[k] & dt$TIMESTAMP <= m$b[k]), "qc", "manual_records", paste("records dropped by hand:", why(k)), detail = "summary")
  o <- span_rows(oow, dt$logger_id[1], dt$site[1])
  for (k in seq_len(NROW(o))) { idx <- which(dt$TIMESTAMP >= o$a[k] & dt$TIMESTAMP <= o$b[k])
    for (p in intersect(c("pres_kpa", "depth_m", "elev_m"), vals))
      remove_cells(dt, p, idx, "qc", "out_of_water_manual", paste0("out of water (", o$cause[k], "): ", o$reason[k])) }
  dt
}

# Flags that need no rule table: service visits, the logger reading below the out-of-water margin, freezing, baro fill-in.
apply_auto_flags <- function(dt, visits, events, cfg) {
  w <- cfg$service_window_h * 3600
  vt <- c(visits, events$TIMESTAMP[events$event %in% c("Coupler Detached", "Coupler Attached", "Host Connected", "Stopped")])
  vt <- unique(vt[!is.na(vt)])
  if (length(vt)) { near <- vapply(as.numeric(dt$TIMESTAMP), function(t) any(abs(t - as.numeric(vt)) <= w), logical(1))
    for (p in intersect(c("depth_m", "pres_kpa", "spc_uscm"), names(dt))) add_flag(dt, p, which(near), "service_visit", sprintf("within %g h of a download or field visit", cfg$service_window_h)) }
  if ("depth_m" %in% names(dt)) {
    add_flag(dt, "depth_m", which(dt$depth_m < cfg$out_of_water_m), "out_of_water", sprintf("less than %g m of water above the sensor", cfg$out_of_water_m))
    if ("temp_c" %in% names(dt)) add_flag(dt, "depth_m", which(dt$temp_c <= 0), "freezing", "logger temperature at or below 0 deg C")
    if ("baro_source" %in% names(dt)) {
      add_flag(dt, "depth_m", which(dt$baro_source == "secondary"), "baro_secondary", "compensated with the secondary barologger")
      add_flag(dt, "depth_m", which(dt$baro_source == "fill"), "baro_fill", "neither chosen barologger had a reading; filled from another barologger shifted onto the primary's level")
      add_flag(dt, "depth_m", which(dt$baro_source == "" & !is.na(dt$pres_kpa)), "no_baro", "no barometric reading within reach") }
  }
  dt
}

# Readings at the start or end of a deployment taken while the logger was still in the air: launched before it went down
# the well, or pulled up before it was stopped. In the air a water logger reads about the barometric pressure, so the water
# above the sensor is near zero; without a barologger, the jump in pressure when it goes into the water gives it away.
# Returns the runs found (one row each); the pipeline flags them, or drops them when auto_trim_air is on.
detect_air_runs <- function(d, cfg) {
  n <- nrow(d); if (n < 6) return(data.table())
  thr <- if (isFALSE(cfg$air_same_as_oow)) (cfg$air_threshold_cm %||% 3) / 100 else cfg$out_of_water_m %||% 0.02   # in the air at the ends: the out-of-water limit, or its own
  in_air <- if ("depth_m" %in% names(d) && any(!is.na(d$depth_m))) !is.na(d$depth_m) & d$depth_m < thr else {
    p <- d$pres_kpa; med <- stats::median(p, na.rm = TRUE)
    !is.na(p) & p < med - 2                            # 2 kPa (about 20 cm of water) below the logger's usual reading
  }
  maxn <- (cfg$air_max_hours %||% 72) * 3600 / max(step_of(d$TIMESTAMP), 1)       # longer runs are a dry well or a bad sensor, not a launch
  run_from <- function(v) { r <- rle(v); if (!r$values[1] || r$lengths[1] > maxn) 0L else r$lengths[1] }
  out <- list()
  a <- run_from(in_air); if (a > 0 && a < n / 2)
    out[[1]] <- data.table(where = "start", from = d$TIMESTAMP[1], to = d$TIMESTAMP[a], records = a, first_in_water = d$TIMESTAMP[a + 1])
  z <- run_from(rev(in_air)); if (z > 0 && z < n / 2)
    out[[2]] <- data.table(where = "end", from = d$TIMESTAMP[n - z + 1], to = d$TIMESTAMP[n], records = z, first_in_water = d$TIMESTAMP[n - z])
  rbindlist(out)
}

# Values typed by hand (Corrections over a period, action set_value): a row's value goes into the missing values of its
# parameter in its period, for the record answering to keys (serial, site, deployment), and only for the step's columns
# (only), so a typed depth is used for the elevations worked out after it. A value that is there is never replaced. Each
# is flagged hand_value and written to the audit trail with the row and its reason.
wlr_set_values <- function(dt, rows, keys, only, flag, where = list()) {
  if (is.null(rows) || !nrow(rows) || !nrow(dt)) return(dt)
  rows <- rows[action == "set_value"]; if (!nrow(rows)) return(dt)
  keys <- tolower(trim(as.character(keys[!is.na(keys)]))); t <- dt$TIMESTAMP
  for (i in seq_len(nrow(rows))) { r <- rows[i]; tg <- tolower(period_parts(r$target))
    if (length(tg) && !"*" %in% tg && !any(tg %in% keys)) next
    if (is.na(r$value)) { problem(PERIOD_FILE, paste0("row ", r$row, ": a typed value needs a number in value; the row was not applied")); next }
    cols <- intersect(intersect(trim(strsplit(r$parameter, "|", fixed = TRUE)[[1]]), names(dt)), only)
    for (p in cols) { if (!is.double(dt[[p]])) set(dt, j = p, value = as.numeric(dt[[p]]))
      k <- which(!is.na(t) & (is.na(r$t0) | t >= r$t0) & (is.na(r$t1) | t <= r$t1) & is.na(dt[[p]])); if (!length(k)) next
      set(dt, k, p, r$value)
      why <- paste0("typed by hand: ", r$value, " (row ", r$row, " of ", PERIOD_FILE, ")", if (nzchar(r$note)) paste0(": ", r$note))
      flag(dt, p, k, "hand_value", why)
      do.call(audit_add, c(list(stage = "period", rule = "period_set_value", level = "summary", n = length(k), reason = why), where, list(column = p))) } }
  dt
}
