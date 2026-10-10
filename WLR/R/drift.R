# R/drift.R  --  Logger drift and offset: what the tape says the logger should have read, and an optional correction.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# OFF BY DEFAULT. Nothing here changes a value unless the user asks, and the logger's own values are never overwritten: a
# correction adds columns beside them (depth_m_corr, elev_m_corr, dtw_m_corr and drift_offset_m).
#
# The offset at a tape check is the tape level minus the logger level (so a positive offset means the logger read too low).
# Between two checks it is prorated linearly with time, which is the usual practice: "a drift correction, usually a linear
# proration with time, is applied" (Freeman et al. 2004, USGS TWRI 8-A3). Before the first check and after the last it is held
# at the nearest check's value. With one check the offset is held constant (or, if asked, grows linearly from zero at the start of
# the deployment). A correction is worth making only when the cause is believed to be drift (Cunningham and Schalk 2011,
# USGS GWPD 16), which is why it is a choice and not automatic.
#
# Three ways to correct, all explicit:
#   drift_correction in the settings: "off" (default) or "apply" (apply the offsets worked out from the tape checks, where
#       the largest is at least drift_min_m). drift_min_m and drift_one_visit are set under Settings, Advanced.
#   rows of the corrections over a period (actions drift_tape, drift_manual, drift_none, drift_keep; an older version's
#       config/drift_corrections.csv is moved in once), read here in that table's old shape: one row per choice, with mode "tape" (apply the tape offsets to this logger or site), "manual"
#       (apply the offset typed in: offset_m at the start, offset_end_m at the end, blank = the same all through) or "none"
#       (never correct this logger or site), or "keep" (a large tape offset was checked and is kept as it is: it no longer goes
#       on the problems list, and nothing is corrected because of it). Rows can be limited to a year and to a period.
#   A manual row always wins over the tape offsets for the period it covers.

DRIFT_COLS <- c("serial", "site", "year", "mode", "offset_m", "offset_end_m", "start", "end", "reason", "author")

# Offset at each time: piecewise linear between the checks, held constant outside them. vt and e are the check times (seconds)
# and offsets (metres); t0 is the start of the record, used only when one_visit = "linear".
drift_offset_series <- function(t, vt, e, one_visit = "constant", t0 = NULL) {
  ok <- !is.na(vt) & !is.na(e); vt <- vt[ok]; e <- e[ok]; tt <- as.numeric(t)
  if (!length(vt)) return(rep(NA_real_, length(tt)))
  if (length(vt) == 1) {
    if (identical(one_visit, "linear") && !is.null(t0) && t0 < vt) return(stats::approx(c(t0, vt), c(0, e), tt, rule = 2)$y)
    return(rep(e, length(tt)))
  }
  stats::approx(vt, e, tt, rule = 2, ties = mean)$y
}

# Rows of the drift table that belong to this deployment (by serial or site, and by year when one is given).
drift_rows <- function(tab, dep) {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  yr <- suppressWarnings(as.integer(tab$year))
  hit <- wlr_row_hits(tab, dep$serial, dep$site, dep$deployment %||% character()) & (is.na(yr) | yr == as.integer(dep$year))   # R/qc.R
  if (!any(hit)) return(NULL)
  tab[hit]
}

# Work out, and if asked apply, the correction for one deployment. d is the deployment's table (changed by reference when a
# correction is applied); chk is its tape-check table (or NULL). Returns one row for audit/drift_corrections.csv.
apply_drift <- function(d, dep, chk, tab, cfg) {
  mode <- tolower(trim(cfg$drift_correction %||% "off")); if (!identical(mode, "apply")) mode <- "off"   # an old "check" (report only) is off
  rows <- drift_rows(tab, dep); rmode <- if (!is.null(rows)) tolower(trim(rows$mode)) else character()
  info <- data.table(deployment = dep$deployment, site = dep$site, serial = dep$serial, year = dep$year, mode = "off", checks = 0L,
                     offset_first_m = NA_real_, offset_last_m = NA_real_, largest_offset_m = NA_real_, applied = FALSE, how = "", rows_corrected = 0L)
  if (!"depth_m" %in% names(d) || !nrow(d)) return(info)
  if ("none" %in% rmode) { info$how <- "turned off for this logger or site"; return(info) }
  t <- as.numeric(d$TIMESTAMP); series <- rep(0, length(t)); manual_used <- FALSE
  # offsets from the tape checks
  want_tape <- mode == "apply" || "tape" %in% rmode
  e <- NULL
  if (want_tape && !is.null(chk) && nrow(chk)) {
    c1 <- chk[!is.na(difference_m) & !is.na(visit_time)]
    if (nrow(c1)) { e <- drift_offset_series(t, as.numeric(c1$visit_time), -c1$difference_m, cfg$drift_one_visit %||% "constant", min(t))
      info[, `:=`(mode = "tape", checks = nrow(c1), offset_first_m = round(-c1$difference_m[which.min(c1$visit_time)], 4), offset_last_m = round(-c1$difference_m[which.max(c1$visit_time)], 4),
                  largest_offset_m = round(max(abs(c1$difference_m)), 4))] } }
  apply_tape <- !is.null(e) && (mode == "apply" || "tape" %in% rmode) && is.finite(info$largest_offset_m) && info$largest_offset_m >= (cfg$drift_min_m %||% 0.01)
  if (!is.null(e) && !apply_tape) info$how <- sprintf("largest offset is below %g m", cfg$drift_min_m %||% 0.01)
  if (apply_tape) { series <- e; info$applied <- TRUE; info$how <- "tape offsets applied, prorated linearly between checks" }
  # offsets typed in
  man <- if (!is.null(rows)) rows[rmode == "manual"] else NULL
  for (k in seq_len(NROW(man))) {
    off <- suppressWarnings(as.numeric(man$offset_m[k])); if (is.na(off)) { problem(dep$deployment, "a manual drift row has no offset_m and was skipped"); next }
    end <- suppressWarnings(as.numeric(man$offset_end_m[k])); if (is.na(end)) end <- off
    a <- if (nzchar(man$start[k])) as.numeric(parse_date_cell(man$start[k])) else min(t); b <- if (nzchar(man$end[k])) as.numeric(parse_date_cell(man$end[k], end = TRUE)) else max(t)
    if (is.na(a) || is.na(b) || b < a) { problem(dep$deployment, "a manual drift row has a start or end that is not a date and was skipped"); next }
    inside <- t >= a & t <= b; series[inside] <- if (b > a) off + (end - off) * (t[inside] - a) / (b - a) else off
    manual_used <- TRUE; info$applied <- TRUE; info$mode <- if (apply_tape) "tape + manual" else "manual"
    info$how <- paste0(if (apply_tape) "tape offsets applied; " else "", "manual offset ", off, if (end != off) paste0(" to ", end), " m over the period typed in")
  }
  if (!info$applied) return(info)
  series[is.na(series)] <- 0
  set(d, j = "drift_offset_m", value = round(series, 4))
  set(d, j = "depth_m_corr", value = d$depth_m + series)
  if ("elev_m" %in% names(d)) set(d, j = "elev_m_corr", value = d$elev_m + series)       # elevation = reference + depth, so it moves with the level
  if ("dtw_m" %in% names(d)) set(d, j = "dtw_m_corr", value = d$dtw_m - series)          # depth to water below the reference moves the other way
  info$rows_corrected <- sum(!is.na(d$depth_m) & series != 0)
  info
}

# Tape against logger, every deployment with a tape check, whether drift is on or off. Drift is usually 1-3 cm a year, so a
# median difference above tape_warn_m (0.05 m by default) more likely means a metadata error: a wrong hang length (T_ST), logger
# offset or reference point, or a tape reading taken when the logger was out of the water. Nothing is changed. A deployment with
# a "keep" row in the drift table (the offset was checked and is kept as it is) is not counted. Returns one row for
# audit/tape_offsets.csv, or NULL; the pipeline lists the count on the problems list and the app shows the rows on Field visits.
tape_offset_check <- function(dep, chk, tab, cfg) {
  if (is.null(chk) || !nrow(chk)) return(NULL)
  c1 <- chk[!is.na(difference_m)]; if (!nrow(c1)) return(NULL)
  lim <- cfg$tape_warn_m %||% 0.05
  rows <- drift_rows(tab, dep); kept <- !is.null(rows) && "keep" %in% tolower(trim(rows$mode))
  med <- stats::median(c1$difference_m); big <- c1$difference_m[which.max(abs(c1$difference_m))]
  lg <- stats::median(c1$h_logger %||% NA_real_, na.rm = TRUE); tp <- stats::median(c1$h_tape %||% NA_real_, na.rm = TRUE)
  status <- if (abs(med) <= lim) "ok" else if (kept) "kept as it is" else "check the metadata"
  hint <- if (status == "ok") "" else if (is.finite(lg) && abs(lg) < 0.03) "the logger read about zero at the visit: out of the water, or the visit time is wrong"
    else if (is.finite(tp) && tp < 0) "the tape puts the water below the sensor: check T_ST and the tape reading"
    else if (is.na(dep$T_ST)) "no hang length (T_ST)"
    else if (nrow(c1) > 1 && all(sign(c1$difference_m) == sign(med))) "a steady offset at every check: hang length, logger offset or reference point"
    else "check the hang length, logger offset, reference point and the tape reading"
  data.table(deployment = dep$deployment, site = dep$site, serial = dep$serial, year = dep$year, checks = nrow(c1), median_diff_m = round(med, 3),
             largest_diff_m = round(big, 3), tape_level_m = round(tp, 3), logger_level_m = round(lg, 3), T_ST = dep$T_ST, T_ST_source = dep$T_ST_source %||% "",
             offset_m = dep$offset_m, reference = dep$ref_code %||% "", status = status, hint = hint)
}

# Which version of the values to hand out: both (the logger's own values and the corrected ones in _corr columns), the original
# only, or the corrected only (the corrected value in the usual column wherever a correction was made). Works on a table that has
# the value columns and, optionally, their _flag and _qc columns.
drift_versions <- function(x, versions = "both") {
  cc <- grep("_corr$", names(x), value = TRUE)
  if (!length(cc) || identical(versions, "both")) return(x)
  x <- copy(x)
  if (identical(versions, "corrected")) for (cv in cc) { v <- sub("_corr$", "", cv)
    if (v %in% names(x)) { k <- !is.na(x[[cv]]); set(x, i = which(k), j = v, value = x[[cv]][k])
      qv <- paste0(v, "_qc"); qc_c <- paste0(cv, "_qc"); if (qv %in% names(x) && qc_c %in% names(x)) set(x, i = which(k & x[[qc_c]] == 5L), j = qv, value = 5L) } }
  drop <- c(cc, intersect(c("drift_offset_m"), names(x)), grep("^(depth_m|elev_m|dtw_m)_corr_(flag|qc)$", names(x), value = TRUE))
  if (identical(versions, "corrected")) drop <- setdiff(drop, "drift_offset_m")
  x[, (intersect(drop, names(x))) := NULL][]
}
