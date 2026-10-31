# R/baro_check.R  --  Checking one barologger against another: constant offset and drift, and (if asked) correcting it.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# The set-up table config/baro_corrections.csv has one row per check:
#   barologger  the station or serial to check (e.g. Solinst_555, or 1039555)
#   reference   the station or serial it is compared with (any other barologger, e.g. HOBO_904 or Solinst_556)
#   method      check         estimate and report only
#               offset        estimate and, if apply = yes, subtract a constant offset
#               offset_drift  estimate and, if apply = yes, subtract the offset and a linear drift
#   from, to    optional window (dates) for the comparison and for the correction
#   apply       yes / no      nothing is changed unless this is yes
# Each check writes output/reports/baro_check_<barologger>_vs_<reference>.html with the data, the method, the results,
# the decision and the R source of the functions below.
#
# Method (all on the barologgers' readings after the Solinst altitude step and the quality rules):
#   1. Both series are averaged to whole hours; only hours where both have a reading are used: d_h = P_target - P_ref (kPa).
#   2. Daily medians of d_h (days with >= 12 paired hours, or half the usual number when a logger records only every few
#      hours): d_i at time t_i (decimal years). Medians resist spikes and
#      the daily step removes most of the hour-to-hour autocorrelation and the daily cycle.
#   3. Offset: median of d_i, with the Hodges-Lehmann estimate (median of all pairwise means) as a cross-check.
#   4. Drift: Theil-Sen slope b = median_{i<j} (d_j - d_i) / (t_j - t_i), in kPa per year; intercept at the middle of the
#      record, so the offset and the drift are estimated independently: correction(t) = a + b (t - t_mid).
#   5. Trend test: Mann-Kendall S on d_i with the Hamed & Rao (1998) variance correction for serial correlation
#      (ranks of the Theil-Sen-detrended series; only autocorrelations significant at 5 % are used).
#   6. 95 % intervals: moving-block bootstrap of the daily series (blocks of 30 days, 1000 resamples, fixed seed);
#      the slope is re-estimated on weekly medians in each resample to keep it fast.
#   7. Decision: an offset is material when |a| >= 0.1 kPa (about 1 cm of water) and its interval excludes 0; a drift is
#      material when the corrected Mann-Kendall p < 0.05, its interval excludes 0 and |b| x span >= 0.1 kPa.

BARO_CORR_COLS <- c("barologger", "reference", "method", "from", "to", "apply", "notes")
BARO_CHECK <- list(min_hours_per_day = 12, block_days = 30, n_boot = 1000, seed = 20261005, material_kpa = 0.1, alpha = 0.05)

# Hourly means of one barometric series (TIMESTAMP, baro_kpa).
baro_hourly <- function(b) { h <- as.numeric(b$TIMESTAMP) %/% 3600 * 3600
  data.table(h = h, v = b$baro_kpa)[!is.na(v), .(v = mean(v)), by = h] }

# Daily medians of the hourly differences target - reference.
baro_daily_diff <- function(target, ref, from = NA, to = NA, min_hours = BARO_CHECK$min_hours_per_day) {
  m <- merge(baro_hourly(target), baro_hourly(ref), by = "h")
  if (!is.na(from)) m <- m[h >= as.numeric(from)]; if (!is.na(to)) m <- m[h < as.numeric(to)]
  m[, d := v.x - v.y]
  dd <- m[, .(d = median(d), n = .N, ref = median(v.y)), by = .(day = h %/% 86400)]
  # a day counts when it has at least min_hours paired hours, or half the usual number when one logger records sparsely (every 6 h)
  need <- min(min_hours, max(1, ceiling(0.5 * stats::median(dd$n))))
  dd <- dd[n >= need][order(day)]; attr(dd, "min_hours") <- need
  dd[, t := 1970 + day / 365.2425][]
}

# Theil-Sen, Hodges-Lehmann and the Mann-Kendall test (Hamed & Rao) are in core/trend.R.

# Moving-block bootstrap of the offset (median) and the drift (Theil-Sen on weekly medians).
block_boot <- function(dd, block = BARO_CHECK$block_days, R = BARO_CHECK$n_boot, seed = BARO_CHECK$seed) {
  n <- nrow(dd); if (n < 2 * block) block <- max(2L, n %/% 4L)
  starts <- seq_len(max(1L, n - block + 1L)); nb <- ceiling(n / block)
  set.seed(seed)
  res <- t(vapply(seq_len(R), function(r) {
    idx <- unlist(lapply(sample(starts, nb, replace = TRUE), function(s) s:(s + block - 1L)))[seq_len(n)]
    x <- dd[idx]; w <- x[, .(d = median(d), t = median(t)), by = .(wk = day %/% 7)]
    c(offset = median(x$d), slope = theil_sen(w$t, w$d)["slope"]) }, c(offset = 0, slope = 0)))
  colnames(res) <- c("offset", "slope"); res
}

# The whole check for one pair. Returns the estimates, the decision and the data used.
baro_check <- function(target, ref, from = NA, to = NA) {
  dd <- baro_daily_diff(target, ref, from, to)
  if (nrow(dd) < 30) return(list(ok = FALSE, why = sprintf("only %d days with both barologgers", nrow(dd)), daily = dd))
  t0 <- stats::median(dd$t); ts <- theil_sen(dd$t, dd$d, t0); mk <- mk_hamed_rao(dd$d, dd$t); bb <- block_boot(dd)
  ci <- function(x) stats::quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  span <- diff(range(dd$t)); off <- stats::median(dd$d); oci <- ci(bb[, "offset"]); sci <- ci(bb[, "slope"])
  est <- data.table(days = nrow(dd), hours = sum(dd$n), first = as.Date(min(dd$day), origin = "1970-01-01"), last = as.Date(max(dd$day), origin = "1970-01-01"),
                    span_years = round(span, 2), offset_kpa = round(off, 3), offset_ci_low = round(oci[1], 3), offset_ci_high = round(oci[2], 3),
                    hodges_lehmann_kpa = round(hodges_lehmann(if (nrow(dd) > 3000) dd$d[round(seq(1, nrow(dd), length.out = 3000))] else dd$d), 3),
                    iqr_kpa = round(stats::IQR(dd$d), 3), intercept_at_mid_kpa = round(ts[["intercept"]], 3), t_mid = round(t0, 3),
                    drift_kpa_per_year = round(ts[["slope"]], 4), drift_ci_low = round(sci[1], 4), drift_ci_high = round(sci[2], 4),
                    drift_over_record_kpa = round(ts[["slope"]] * span, 3), mk_S = mk[["S"]], mk_z = round(mk[["z"]], 2), mk_p = signif(mk[["p"]], 3),
                    hamed_rao_ratio = round(mk[["n_eff_ratio"]], 2))
  est[, offset_material := abs(offset_kpa) >= BARO_CHECK$material_kpa & (offset_ci_low > 0 | offset_ci_high < 0)]
  est[, drift_material := !is.na(mk_p) & mk_p < BARO_CHECK$alpha & (drift_ci_low > 0 | drift_ci_high < 0) & abs(drift_over_record_kpa) >= BARO_CHECK$material_kpa]
  list(ok = TRUE, est = est, daily = dd, boot = bb)
}

# The correction for a barologger's readings at times tt (kPa to subtract).
baro_correction_at <- function(est, method, tt) {
  t <- 1970 + as.numeric(tt) / 86400 / 365.2425
  if (method == "offset") return(rep(est$offset_kpa, length(t)))
  if (method == "offset_drift") return(est$intercept_at_mid_kpa + est$drift_kpa_per_year * (t - est$t_mid))
  rep(0, length(t))
}

# The functions whose source goes into each report.
BARO_CHECK_FUNS <- c("baro_hourly", "baro_daily_diff", "theil_sen", "hodges_lehmann", "mk_hamed_rao", "block_boot", "baro_check", "baro_correction_at")

baro_check_report <- function(target_name, ref_name, res, method, applied, alt_tab, second = NULL, solinst_mode = "undo") {
  e <- res$est; dd <- res$daily
  code <- unlist(lapply(BARO_CHECK_FUNS, function(f) c(paste0(f, " <- ", paste(deparse(get(f)), collapse = "\n")), "")))
  code <- unlist(strsplit(code, "\n", fixed = TRUE)); code <- gsub("^( +)", "\\1", code)
  code <- vapply(code, function(l) { lead <- nchar(l) - nchar(sub("^ +", "", l)); paste0(strrep("\u00a0", lead), sub("^ +", "", l)) }, "", USE.NAMES = FALSE)
  dd[, date := as.Date(day, origin = "1970-01-01")]
  fit <- data.table(date = dd$date, f = e$intercept_at_mid_kpa + e$drift_kpa_per_year * (dd$t - e$t_mid))
  p1 <- ggplot2::ggplot(dd, ggplot2::aes(date, d)) + ggplot2::geom_point(size = 0.6, alpha = 0.5, colour = NORD$frost) +
    ggplot2::geom_hline(yintercept = 0, colour = NORD$gray) + ggplot2::geom_line(data = fit, ggplot2::aes(date, f), colour = NORD$red, linewidth = 1) +
    ggplot2::labs(x = NULL, y = paste(target_name, "minus", ref_name, "(kPa, daily median)"), title = "Difference over time, with the Theil-Sen fit") + theme_nord()
  p2 <- ggplot2::ggplot(dd, ggplot2::aes(d)) + ggplot2::geom_histogram(bins = 60, fill = NORD$frost) +
    ggplot2::geom_vline(xintercept = e$offset_kpa, colour = NORD$red) + ggplot2::labs(x = "Daily median difference (kPa)", y = "Days", title = "Distribution of the daily differences") + theme_nord()
  yr <- dd[, .(days = .N, median_kpa = round(median(d), 3), iqr_kpa = round(stats::IQR(d), 3)), by = .(year = as.integer(format(date, "%Y")))]
  mo <- dd[, .(days = .N, median_kpa = round(median(d), 3)), by = .(month = as.integer(format(date, "%m")))][order(month)]
  decision <- c(
    sprintf("Offset %s: %.3f kPa (95%% interval %.3f to %.3f), about %.1f cm of water. %s", if (e$offset_material) "found" else "not material", e$offset_kpa, e$offset_ci_low, e$offset_ci_high,
            e$offset_kpa / 9.80665 * 100, if (e$offset_material) "It is larger than the 0.1 kPa threshold and its interval excludes zero." else "Below the 0.1 kPa threshold or its interval includes zero."),
    sprintf("Drift %s: %.4f kPa per year (95%% interval %.4f to %.4f), %.3f kPa over the %.1f years; Mann-Kendall z = %.2f, p = %s after the Hamed-Rao correction (variance x %.2f).",
            if (e$drift_material) "found" else "not material", e$drift_kpa_per_year, e$drift_ci_low, e$drift_ci_high, e$drift_over_record_kpa, e$span_years, e$mk_z, format(e$mk_p), e$hamed_rao_ratio),
    sprintf("Method chosen: %s. Applied to the readings: %s.", method, if (applied) "yes" else "no (estimate only)"),
    sprintf("Solinst altitude setting during this run: %s.", if (identical(solinst_mode, "keep")) "kept as stored (readings include altitude/826 m)" else "taken off (altitude/826 m of water subtracted from each Solinst file that carries one)"))
  rpt_doc(paste("Barologger check:", target_name, "against", ref_name), format(Sys.time(), "Run %Y-%m-%d %H:%M"), list(
    rpt_heading("Question"),
    rpt_text(sprintf("Does %s read the same air pressure as %s, or is it offset by a constant, or drifting with time? Two barologgers in the same area should read the same pressure to within their accuracy (Solinst Barologger about 0.05 kPa, HOBO U20 about 0.4 kPa typical), apart from a difference in elevation (about 0.12 kPa per 10 m).", target_name, ref_name)),
    rpt_heading("Result"),
    rpt_cards(c(`offset (kPa)` = e$offset_kpa, `drift (kPa/yr)` = e$drift_kpa_per_year, `days compared` = e$days, `p (drift)` = e$mk_p)),
    rpt_text(decision),
    rpt_table(e, "All estimates"),
    if (!is.null(second)) rpt_table(second, "The same check against a second reference (consistency)") else NULL,
    rpt_heading("Data"),
    rpt_text(sprintf("Hourly means of both barologgers after the Solinst altitude step and the quality rules; %s paired hours on %s days (%s to %s) with at least %d paired hours each.",
                     format(e$hours, big.mark = ","), format(e$days, big.mark = ","), e$first, e$last, attr(dd, "min_hours") %||% BARO_CHECK$min_hours_per_day)),
    if (!is.null(alt_tab) && nrow(alt_tab)) rpt_table(alt_tab, "Altitude programmed into the Solinst files of these barologgers (from each file's header)") else NULL,
    rpt_plot(p1, 11, 5), rpt_plot(p2, 11, 4),
    rpt_table(yr, "By year (a step between years points to a change of instrument settings, a drift to a sensor ageing)"),
    rpt_table(mo, "By month (a seasonal pattern points to a temperature effect, not a constant offset)"),
    rpt_heading("Method"),
    rpt_text("1. Both series are averaged to whole hours and only hours where both have a reading are kept: d = P(barologger) - P(reference), in kPa.",
             sprintf("2. Daily medians of d on days with at least %d paired hours. Medians resist spikes; the daily step removes most of the hour-to-hour autocorrelation and the daily cycle.", BARO_CHECK$min_hours_per_day),
             "3. Offset a = median of the daily differences; the Hodges-Lehmann estimate (median of all pairwise means) is reported as a cross-check.",
             "4. Drift b = Theil-Sen slope, the median of (d_j - d_i) / (t_j - t_i) over all pairs of days, in kPa per year. The fit is a + b (t - t_mid), with t_mid the middle of the record, so the offset and the drift are estimated independently.",
             "5. Trend test: Mann-Kendall S with the Hamed and Rao (1998) variance correction for autocorrelation, computed on the ranks of the detrended series using only lags whose autocorrelation is significant at 5 %.",
             sprintf("6. 95 %% intervals: moving-block bootstrap of the daily series (blocks of %d days, %d resamples, seed %d); in each resample the offset is the median and the drift the Theil-Sen slope of weekly medians.", BARO_CHECK$block_days, BARO_CHECK$n_boot, BARO_CHECK$seed),
             sprintf("7. Decision: the offset is material when |a| >= %.1f kPa (about 1 cm of water) and its interval excludes zero; the drift when p < %.2f, its interval excludes zero and |b| x span >= %.1f kPa.", BARO_CHECK$material_kpa, BARO_CHECK$alpha, BARO_CHECK$material_kpa),
             "8. Correction (only if apply = yes): offset: P - a; offset_drift: P - (a + b (t - t_mid)), within the from/to window if one is given."),
    rpt_heading("R code used (exact source of the functions)"),
    rpt_table(data.table(line = seq_along(code), code = code), NULL, max_rows = 10000),
    rpt_heading("Session"),
    rpt_text(R.version.string, paste("data.table", as.character(utils::packageVersion("data.table"))), paste("WLR", PIPELINE_VERSION))))
}

# Runs every row of the set-up table: resolves the barologger and the reference (a station or a serial), checks, writes
# the report, and corrects the barologger's own serials when apply = yes. Returns the (possibly corrected) series and a table.
run_baro_checks <- function(rows, cleaned, bst, canon, report_dir, alt_log, cfg) {
  empty <- data.table(barologger = character(), reference = character(), method = character(), apply = character(), applied = logical(), report = character(), note = character())
  if (is.null(rows) || !nrow(rows)) return(list(cleaned = cleaned, table = empty))
  rows <- rows[nzchar(trim(barologger)) & nzchar(trim(reference))]
  if (!nrow(rows)) return(list(cleaned = cleaned, table = empty))
  serials_of <- function(name) { name <- trim(name); if (name %in% bst$station) return(bst[station == name & serial %in% names(cleaned), serial])
    k <- canon(norm_serial(name)); if (nzchar(k) && !is.null(cleaned[[k]])) k else character() }
  series_of <- function(name) { ss <- serials_of(name); if (!length(ss)) return(NULL)
    if (length(ss) == 1L) return(cleaned[[ss]][, .(TIMESTAMP, baro_kpa)]); stitch_baro_station(name, ss, cleaned)$series }
  dt_cell <- function(x) { x <- trim(x %||% ""); if (!nzchar(x)) NA else as.POSIXct(x, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M", "%Y-%m-%d")) }
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  results <- list(); out <- list(); done_apply <- character()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i]; m <- tolower(trim(r$method %||% "check")); if (!m %in% c("check", "offset", "offset_drift")) m <- "check"
    tg <- series_of(r$barologger); rf <- series_of(r$reference)
    if (is.null(tg) || is.null(rf)) { why <- sprintf("%s not found among the barologgers", if (is.null(tg)) r$barologger else r$reference)
      problem(paste("baro check", r$barologger), why); out[[i]] <- data.table(barologger = r$barologger, reference = r$reference, method = m, apply = r$apply, applied = FALSE, report = "", note = why); next }
    res <- baro_check(tg, rf, dt_cell(r$from), dt_cell(r$to)); results[[i]] <- res
    if (!res$ok) { problem(paste("baro check", r$barologger), res$why)
      out[[i]] <- data.table(barologger = r$barologger, reference = r$reference, method = m, apply = r$apply, applied = FALSE, report = "", note = res$why); next }
    want <- tolower(trim(r$apply %||% "")) %in% c("yes", "y", "true", "1") && m != "check"
    applied <- FALSE; note <- ""
    if (want && r$barologger %in% done_apply) note <- "not applied: another row already corrects this barologger"
    else if (want) {
      f0 <- dt_cell(r$from); f1 <- dt_cell(r$to)
      for (s in serials_of(r$barologger)) { b <- copy(cleaned[[s]]); k <- (is.na(f0) | b$TIMESTAMP >= f0) & (is.na(f1) | b$TIMESTAMP < f1)
        b[k, baro_kpa := baro_kpa - baro_correction_at(res$est, m, TIMESTAMP)]; cleaned[[s]] <- b }
      applied <- TRUE; done_apply <- c(done_apply, r$barologger)
      problem(paste("baro check", r$barologger), sprintf("%s corrected against %s (%s): offset %.3f kPa, drift %.4f kPa/yr", r$barologger, r$reference, m, res$est$offset_kpa, res$est$drift_kpa_per_year))
    }
    out[[i]] <- cbind(data.table(barologger = r$barologger, reference = r$reference, method = m, apply = r$apply, applied = applied, note = note), res$est)
  }
  tab <- rbindlist(out, fill = TRUE); if (!"report" %in% names(tab)) tab[, report := ""]
  for (i in seq_len(nrow(rows))) { res <- if (i <= length(results)) results[[i]] else NULL; if (is.null(res) || !isTRUE(res$ok)) next; r <- rows[i]   # a row that was not checked has no result
    second <- tab[barologger == r$barologger & reference != r$reference & !is.na(offset_kpa), .(reference, days, offset_kpa, offset_ci_low, offset_ci_high, drift_kpa_per_year, drift_ci_low, drift_ci_high, mk_p)]
    at <- if (!is.null(alt_log) && nrow(alt_log)) alt_log[serial %in% serials_of(r$barologger)] else NULL
    f <- file.path(report_dir, paste0("baro_check_", safe_name(r$barologger), "_vs_", safe_name(r$reference)))
    doc <- baro_check_report(r$barologger, r$reference, res, tab$method[i], isTRUE(tab$applied[i]), at, if (nrow(second)) second else NULL, cfg$solinst_altitude %||% "undo")
    w <- tryCatch(report_write(doc, f, "html", NORD), error = function(e) { problem("baro check report", conditionMessage(e)); character() })
    if (length(w)) set(tab, i, "report", basename(w[1]))
  }
  list(cleaned = cleaned, table = tab)
}
