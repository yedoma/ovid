# R/qc.R  --  QC profiles, manual flags and survey statistics. Measurements are never changed: QC returns flags.
# Part of Ovid Core // ERT. Sourced by ert_pipeline.R; not meant to be run on its own.
#
# A profile is the set of rows in qc_profiles.csv with that name. A row applies to every survey ("all"), to one
# instrument type ("agi", "syscal") or to one survey_id. Each rule returns the meas_ids it flags; flags.csv keeps
# one line per measurement, rule and profile. Manual flags (manual_flags.csv, active = TRUE) are added to every profile.

QC_RULES <- list(
  max_error_pct   = function(d, v) d$meas_id[!is.na(d$error_pct) & d$error_pct > v[1]],
  min_current_ma  = function(d, v) d$meas_id[!is.na(d$current_a) & abs(d$current_a) * 1000 < v[1]],
  min_vp_mv       = function(d, v) d$meas_id[!is.na(d$voltage_v) & abs(d$voltage_v) * 1000 < v[1]],
  min_abs_r_ohm   = function(d, v) d$meas_id[!is.na(d$resistance_ohm) & abs(d$resistance_ohm) < v[1]],
  rhoa_range      = function(d, v) d$meas_id[!is.na(d$rhoa_ohmm) & d$rhoa_ohmm != 0 & (abs(d$rhoa_ohmm) < v[1] | abs(d$rhoa_ohmm) > v[2])],   # absolute values, as EarthImager
  negative_rhoa   = function(d, v) d$meas_id[!is.na(d$rhoa_ohmm) & d$rhoa_ohmm <= 0],
  negative_ip     = function(d, v) d$meas_id[(!is.na(d$chargeability_mv_v) & d$chargeability_mv_v < 0) | (!is.na(d$ip_mrad) & d$ip_mrad < 0)],   # Herring et al. 2023: negative chargeability
  max_contact_ohm = function(d, v) d$meas_id[!is.na(d$contact_ohm) & d$contact_ohm > v[1]],
  duplicates      = function(d, v) d$meas_id[duplicated(d[, .(a, b, m, n)])],          # repeats of a quadrupole after the first
  geometry        = function(d, v) {
    eq <- function(x, y) x == y & x != 0                                                  # 0 = remote electrode
    same <- with(d, eq(a, b) | eq(a, m) | eq(a, n) | eq(b, m) | eq(b, n) | eq(m, n))
    off <- abs(d$geom_factor_m * d$resistance_ohm - d$rhoa_ohmm) > 0.05 * abs(d$rhoa_ohmm) + 1e-6   # rhoa does not follow from k and R
    d$meas_id[is.na(d$a) | is.na(d$m) | same %in% TRUE | !is.finite(d$geom_factor_m) | off %in% TRUE] },
  magnitude_sd    = function(d, v) { ok <- !is.na(d$rhoa_ohmm) & d$rhoa_ohmm > 0; if (sum(ok) < 10) return(integer())   # see below
    x <- if (length(v) >= 2 && isTRUE(v[2] == 1)) log10(d$rhoa_ohmm[ok]) else d$rhoa_ohmm[ok]
    d$meas_id[ok][abs(x - mean(x)) > v[1] * stats::sd(x)] },
  outlier_mad     = function(d, v) { ok <- !is.na(d$rhoa_ohmm) & d$rhoa_ohmm > 0; if (sum(ok) < 10) return(integer())
    l <- log10(d$rhoa_ohmm[ok]); z <- abs(l - median(l)) / (mad(l) + 1e-9); d$meas_id[ok][z > v[1]] },
  reciprocal_pct  = function(d, v) {                                                     # normal and reciprocal (A,B <-> M,N)
    k1 <- paste(d$a, d$b, d$m, d$n); k2 <- paste(d$m, d$n, d$a, d$b); j <- match(k1, k2)
    e <- abs(d$resistance_ohm - d$resistance_ohm[j]) / abs((d$resistance_ohm + d$resistance_ohm[j]) / 2) * 100
    d$meas_id[!is.na(e) & e > v[1]] })

# magnitude_sd   value "k" or "k;1": apparent resistivity more than k standard deviations from the survey mean. Herring et al.
#                (2023) and the CPERS database (Herring et al. 2024) remove values more than 9 SD away. The papers do not say
#                whether the mean and SD are of rho_a or of log rho_a: "9" uses rho_a as written, "9;1" uses log10 rho_a.
#                Unlike outlier_mad (median and MAD of log rho_a), one huge value inflates the SD, so this catches only gross spikes.
#                No value can lie more than (n - 1) / sqrt(n) SD from the mean, so 9 SD can flag nothing below about 83 points.
# Two rules that work on the survey as a whole, after the others (see qc_survey):
#   moving_median  value "window;percent" (default 5;7): along each depth level (one pseudo-depth with the same pair of
#                  dipole lengths, so different arrays are not mixed), a measurement whose log10 apparent resistivity departs
#                  from the moving median of the window around it by more than that percentage of the median. Herring et al.
#                  (2023) give a 5-point moving median of log apparent resistivity per depth level and 7 % as an example to test
#                  per survey; the paper does not say whether the 7 % applies to the log value or to rho_a, and this reads it as
#                  the log value. Check it against your own surveys before relying on it.
#   bad_electrode  value "percent;minimum" (default 25;10): an electrode is bad when more than that share of the measurements
#                  that use it are already flagged (the CPERS processing notebook flags at 25 %; Herring et al. 2023 give 20 %), counting only electrodes used
#                  in at least `minimum` measurements; every measurement using it is then flagged. Repeats (duplicates), bad
#                  geometry and electrodes turned off are left out of the count, since they say nothing about the electrode.
QC_RULES$moving_median <- function(d, v) {
  k <- if (length(v) >= 1 && !is.na(v[1])) as.integer(v[1]) else 5L; if (k %% 2L == 0L) k <- k + 1L
  pct <- if (length(v) >= 2 && !is.na(v[2])) v[2] else 7
  xy <- pseudo_xy(d); ok <- !is.na(d$rhoa_ohmm) & d$rhoa_ohmm > 0 & is.finite(xy$px) & is.finite(xy$pz)
  if (sum(ok) < k) return(integer())
  dl <- function(p, q) { r <- abs(d[[p]] - d[[q]]); r[!is.finite(r)] <- -1; round(r, 2) }          # dipole lengths (a remote electrode is -1)
  w <- data.table(id = d$meas_id[ok], l = log10(d$rhoa_ohmm[ok]), x = xy$px[ok], lev = round(xy$pz[ok], 2), ab = dl("ax", "bx")[ok], mn = dl("mx", "nx")[ok])
  setorder(w, lev, ab, mn, x)                                                   # one level = one pseudo-depth with the same dipoles
  w[, med := if (.N >= k) stats::runmed(l, k, endrule = "median") else NA_real_, by = .(lev, ab, mn)]
  w[!is.na(med) & abs(l - med) > pct / 100 * pmax(abs(med), 1e-6), id]          # the departure is a share of the median log10(rho_a)
}

# Which rule rows apply to a survey. applies_to may be all (or blank), an instrument type (agi, syscal), a survey_id, or
# site:<site>, transect:<transect_id> or months:<list> (for example months:11-4 for November to April, or months:6,7,8), so a
# limit can differ by place and season. When a rule has rows at several levels the most specific wins:
# survey > transect > site > months > instrument type > all.
qc_rows <- function(qc, prof, sid, typ, s1 = NULL) {
  at <- trim(qc$applies_to); at[is.na(at)] <- ""
  site <- as.character(s1$site %||% ""); tr <- as.character(s1$transect_id %||% ""); site[is.na(site)] <- ""; tr[is.na(tr)] <- ""
  dt <- as.character(s1$acquired_date %||% ""); if (is.na(dt) || !nzchar(dt)) dt <- substr(as.character(s1$datetime_start %||% ""), 1, 10)
  mon <- suppressWarnings(as.integer(substr(dt, 6, 7)))
  in_months <- function(a) { if (is.na(mon)) return(FALSE); m <- ids_from(sub("^months?:", "", a)); if (grepl("-", a)) { r <- as.integer(strsplit(sub("^months?:", "", a), "-")[[1]])
      m <- if (r[1] <= r[2]) seq(r[1], r[2]) else c(seq(r[1], 12), seq(1, r[2])) }; mon %in% m }
  rank <- vapply(at, function(a) if (a %in% c("all", "")) 1L else if (a == typ) 2L else if (grepl("^months?:", a)) (if (in_months(a)) 3L else 0L)
                 else if (nzchar(site) && a == paste0("site:", site)) 4L else if (nzchar(tr) && a == paste0("transect:", tr)) 5L else if (a == sid) 6L else 0L, 1L)
  keep <- qc$profile == prof & tolower(qc$enabled) %in% c("true", "1", "yes") & rank > 0
  r <- qc[which(keep)][, .rank := rank[which(keep)]]
  if (!nrow(r)) return(r[, .rank := NULL][])
  r[, .best := max(.rank), by = rule][.rank == .best][, c(".rank", ".best") := NULL][]
}

# Electrodes turned off (config/electrode_status.csv): by survey, or by transect over a period. A measurement that uses one is
# flagged "electrode turned off"; the electrode is marked off in the survey's electrodes.csv.
electrodes_off <- function(status, sid, s1 = NULL) {
  if (is.null(status) || !nrow(status)) return(integer())
  st <- status[tolower(trim(status)) == "off"]; if (!nrow(st)) return(integer())
  tr <- as.character(s1$transect_id %||% ""); tr[is.na(tr)] <- ""
  dt <- as.character(s1$acquired_date %||% ""); if (is.na(dt) || !nzchar(dt)) dt <- substr(as.character(s1$datetime_start %||% ""), 1, 10)
  day <- suppressWarnings(as.Date(dt))
  hit <- trim(st$survey_id) == sid | (nzchar(tr) & trim(st$transect_id) == tr & !nzchar(trim(st$survey_id)) &
         (is.na(day) | ((!nzchar(trim(st$from)) | day >= suppressWarnings(as.Date(st$from))) & (!nzchar(trim(st$to)) | day <= suppressWarnings(as.Date(st$to))))))
  hit[is.na(hit)] <- FALSE
  sort(unique(as.integer(unlist(lapply(st$electrode[hit], ids_from)))))
}

ids_from <- function(x) { x <- trim(unlist(strsplit(x, "[;,]"))); x <- x[nzchar(x)]
  unlist(lapply(x, function(p) if (grepl("-", p)) { r <- as.integer(strsplit(p, "-")[[1]]); seq(r[1], r[2]) } else as.integer(p))) }

qc_survey <- function(d, sid, type, qc, profile, manual, s1 = NULL, status = NULL) {
  out <- list()
  R <- qc_rows(qc, profile, sid, type, s1); late <- R[rule == "bad_electrode"]; R <- R[rule != "bad_electrode"]
  off <- electrodes_off(status, sid, s1)
  if (length(off)) { ids <- d$meas_id[d$a %in% off | d$b %in% off | d$m %in% off | d$n %in% off]
    if (length(ids)) out[[length(out) + 1L]] <- data.table(meas_id = ids, rule = "electrode_off", flag = "electrode turned off", value = paste("electrode", paste(off, collapse = ", "))) }
  for (i in seq_len(nrow(R))) {
    f <- QC_RULES[[R$rule[i]]]; if (is.null(f)) next
    v <- suppressWarnings(as.numeric(strsplit(R$value[i], ";")[[1]]))
    ids <- tryCatch(f(d, v), error = function(e) { problem(sid, paste("QC rule", R$rule[i], "failed:", conditionMessage(e))); integer() })
    if (length(ids)) out[[length(out) + 1L]] <- data.table(meas_id = ids, rule = R$rule[i], flag = R$flag[i], value = R$value[i], rule_id = R$rule_id[i] %||% R$rule[i])
  }
  mf <- manual[survey_id == sid & !tolower(active) %in% c("false", "0", "no")]
  for (i in seq_len(nrow(mf))) {
    ids <- c(ids_from(mf$meas_ids[i]), if (nzchar(mf$electrode[i])) { e <- as.integer(ids_from(mf$electrode[i])); d$meas_id[d$a %in% e | d$b %in% e | d$m %in% e | d$n %in% e] })
    if (length(ids)) out[[length(out) + 1L]] <- data.table(meas_id = intersect(ids, d$meas_id), rule = "manual",
                                                            flag = if (nzchar(mf$flag[i])) mf$flag[i] else "manually excluded", value = mf$reason[i])
  }
  for (i in seq_len(nrow(late))) {                                     # bad electrodes, from what the other rules flagged
    v <- suppressWarnings(as.numeric(strsplit(late$value[i], ";")[[1]])); pct <- if (length(v) && !is.na(v[1])) v[1] else 25
    nmin <- if (length(v) >= 2 && !is.na(v[2])) v[2] else 10                 # an electrode in fewer measurements than this is not judged
    fl <- unique(unlist(lapply(out, function(x) x[!rule %in% c("electrode_off", "duplicates", "geometry"), meas_id])))   # repeats and geometry say nothing about an electrode
    el <- rbindlist(lapply(c("a", "b", "m", "n"), function(cc) data.table(e = d[[cc]], id = d$meas_id)))[!is.na(e) & e != 0 & !e %in% off]
    el <- unique(el)[, bad := id %in% fl]
    sh <- el[, .(share = mean(bad), n = .N), by = e][n >= nmin & share > pct / 100]
    if (nrow(sh)) { ids <- unique(el[e %in% sh$e, id])
      out[[length(out) + 1L]] <- data.table(meas_id = ids, rule = "bad_electrode", flag = if (nzchar(late$flag[i])) late$flag[i] else "bad electrode",
                                            value = paste(sprintf("electrode %d (%.0f%% flagged)", sh$e, 100 * sh$share), collapse = "; "), rule_id = late$rule_id[i] %||% "bad_electrode") }
  }
  f <- rbindlist(out, fill = TRUE)
  if (!nrow(f)) f <- data.table(meas_id = integer(), rule = character(), flag = character(), value = character(), rule_id = character())
  if (!"rule_id" %in% names(f)) f[, rule_id := rule]
  f[is.na(rule_id) | !nzchar(rule_id), rule_id := rule]               # electrode_off and manual flags are named by their rule
  f[, `:=`(survey_id = sid, profile = profile)]; setcolorder(f, c(setdiff(names(f), "rule_id"), "rule_id"))[]
}

STAT_VARS <- c(rhoa_ohmm = "rhoa_ohmm", resistance_ohm = "resistance_ohm", current_ma = "current_a", voltage_mv = "voltage_v",
               error_pct = "error_pct", contact_ohm = "contact_ohm", ip_mrad = "ip_mrad", chargeability_mv_v = "chargeability_mv_v")

# Distribution of each variable, for all measurements and for those that pass. Empty surveys give rows of NA, not errors.
survey_stats <- function(d, flagged, sid, profile) {
  keep <- !d$meas_id %in% flagged
  rbindlist(lapply(names(STAT_VARS), function(v) {
    x0 <- d[[STAT_VARS[v]]]; if (v %in% c("current_ma", "voltage_mv")) x0 <- x0 * 1000
    rbindlist(lapply(c("all", "passed"), function(set) { x <- x0[if (set == "all") TRUE else keep]; x <- x[!is.na(x)]
      q <- if (length(x)) quantile(x, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), names = FALSE) else rep(NA_real_, 7)
      data.table(survey_id = sid, profile = profile, variable = v, set = set, n = length(x), min = q[1], q05 = q[2], q25 = q[3],
                 median = q[4], q75 = q[5], q95 = q[6], max = q[7], mean = if (length(x)) mean(x) else NA_real_,
                 sd = if (length(x) > 1) sd(x) else NA_real_) }))
  }))[n > 0 | variable %in% c("rhoa_ohmm", "resistance_ohm")]
}

# The profiles a run checks, the one the catalogue reports first: cfg$catalogue_profile (Settings) when qc_profiles.csv has
# it, else the first built-in one; then the built-in ones and every other profile in the table (saved on the QC page, cpers).
run_profiles <- function(cfg, table_profiles) {
  p <- trim(as.character(table_profiles)); p <- unique(p[!is.na(p) & nzchar(p) & !p %in% c("none", "live")])
  first <- trim(cfg$catalogue_profile %||% "")
  if (!length(first) || !nzchar(first[1]) || !first[1] %in% p) first <- cfg$qc_profiles[1]
  unique(c(first[1], cfg$qc_profiles, p))
}

# Flags and statistics of every survey under every profile. cache: NULL, or function(sid, s1, compute) returning compute()'s
# result, or the one kept from an earlier run when nothing it depends on changed (the pipeline's, cache/qc).
run_qc <- function(surveys, meas, cfgs, profiles, cache = NULL) {
  ok_p <- profiles[profiles %in% c(cfgs$qc$profile, "none")]
  for (p in setdiff(profiles, ok_p)) problem("QC", paste0("profile '", p, "' is not in qc_profiles.csv"))
  F <- list(); S <- list()
  for (sid in names(meas)) {
    d <- meas[[sid]]; s1 <- surveys[survey_id == sid][1]; type <- s1$type
    one <- function() { f <- list(); st <- list()
      for (p in ok_p) { x <- qc_survey(d, sid, type, cfgs$qc, p, cfgs$manual, s1, cfgs$electrodes); f[[p]] <- x; st[[p]] <- survey_stats(d, unique(x$meas_id), sid, p) }
      list(flags = f, stats = st) }
    r <- if (is.function(cache)) cache(sid, s1, one) else one()
    F <- c(F, r$flags); S <- c(S, r$stats)
    if (length(ok_p) && ok_p[1] == profiles[1]) { f <- r$flags[[1]]
      by_flag <- f[, .(n = uniqueN(meas_id)), by = flag][order(-n)]
      surveys[survey_id == sid, `:=`(qc_profile = ok_p[1], n_flagged = uniqueN(f$meas_id), n_retained = nrow(d) - uniqueN(f$meas_id),
                                     pct_retained = if (nrow(d)) round(100 * (1 - uniqueN(f$meas_id) / nrow(d)), 1) else NA_real_,
                                     qc_flags = if (nrow(by_flag)) paste(paste0(by_flag$flag, ": ", by_flag$n), collapse = "; ") else "")]
    }
  }
  ord <- function(x) if (!nrow(x)) x else x[order(match(profile, ok_p), match(survey_id, names(meas)))]   # by profile, then survey, as before
  list(surveys = surveys, flags = ord(rbindlist(F)), stats = ord(rbindlist(S)))
}

# A survey's electrodes with a status column when any of them was turned off (left out otherwise, so nothing changes for surveys
# with every electrode on).
electrodes_with_status <- function(e, status, sid, s1 = NULL) {
  off <- electrodes_off(status, sid, s1); if (!length(off) || !nrow(e)) return(e)
  e <- copy(e); e[, status := ifelse(electrode %in% off, "off", "on")][]
}
