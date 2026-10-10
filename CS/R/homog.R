# R/homog.R  --  Daily values estimated from neighbouring stations, and breakpoints against them.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.
#
# Both work on the daily means the between-station check uses (the neighbour_params setting: air temperature, humidity,
# radiation; never precipitation). Neither changes a station table: the estimates are a table of their own (level L3,
# WMO code 6 "estimated"), and a breakpoint is reported, never adjusted.

# A missing daily mean, inside a station's record, estimated from the station that tracks it best: a straight-line
# regression on the days both have, used only when it explains at least r2_min of the variance over min_days or more,
# and only for gaps up to max_days. L: one row per station key, date and parameter (key, Date, parameter, value).
neighbour_estimates <- function(L, r2_min = 0.9, min_days = 60, max_days = 31) {
  none <- data.table(station = character(), stream = character(), parameter = character(), Date = as.Date(character()), value = numeric(),
                     donor = character(), r2 = numeric(), shared_days = integer(), gap_days = integer())
  if (is.null(L) || !nrow(L) || uniqueN(L$key) < 2) return(none)
  out <- list()
  for (p in unique(L$parameter)) {
    w <- dcast(L[parameter == p], Date ~ key, value.var = "value", fun.aggregate = mean)
    ks <- setdiff(names(w), "Date"); if (length(ks) < 2) next
    full <- data.table(Date = seq(min(w$Date), max(w$Date), by = "day")); w <- w[full, on = "Date"]
    fits <- list()
    for (k in ks) { y <- w[[k]]; have <- which(!is.na(y)); if (length(have) < min_days) next
      miss <- setdiff(seq(min(have), max(have)), have); if (!length(miss)) next
      run <- cumsum(c(1L, diff(miss) != 1L)); glen <- ave(miss, run, FUN = length); miss <- miss[glen <= max_days]; glen <- glen[glen <= max_days]
      if (!length(miss)) next
      cand <- list()
      for (j in setdiff(ks, k)) { x <- w[[j]]; ok <- !is.na(x) & !is.na(y); if (sum(ok) < min_days) next
        f <- stats::lm.fit(cbind(1, x[ok]), y[ok]); r2 <- 1 - sum(f$residuals^2) / sum((y[ok] - mean(y[ok]))^2)
        if (is.finite(r2) && r2 >= r2_min) cand[[j]] <- list(a = f$coefficients[1], b = f$coefficients[2], r2 = r2, n = sum(ok)) }
      if (!length(cand)) next
      cand <- cand[order(-vapply(cand, `[[`, 0, "r2"))]
      est <- rep(NA_real_, length(miss)); donor <- rep(NA_character_, length(miss)); r2v <- rep(NA_real_, length(miss)); nv <- rep(NA_integer_, length(miss))
      for (j in names(cand)) { c0 <- cand[[j]]; x <- w[[j]][miss]; use <- is.na(est) & !is.na(x)
        est[use] <- c0$a + c0$b * x[use]; donor[use] <- j; r2v[use] <- c0$r2; nv[use] <- c0$n }
      ok <- !is.na(est); if (!any(ok)) next
      ks2 <- strsplit(k, "__", fixed = TRUE)[[1]]
      out[[length(out) + 1]] <- data.table(station = ks2[1], stream = ks2[2] %||% "", parameter = p, Date = w$Date[miss][ok], value = signif(est[ok], 5),
                                           donor = sub("__", " / ", donor[ok], fixed = TRUE), r2 = round(r2v[ok], 3), shared_days = nv[ok], gap_days = as.integer(glen[ok])) }
  }
  if (!length(out)) return(none)
  rbindlist(out)[order(station, stream, parameter, Date)]
}

# Pettitt's (1979) test for one change in the level of a series: the time of the break, and its approximate p-value.
pettitt <- function(x) {
  n <- length(x); if (n < 4) return(list(k = NA_integer_, p = NA_real_))
  r <- rank(x); U <- vapply(seq_len(n - 1), function(t) 2 * sum(r[seq_len(t)]) - t * (n + 1), 0)
  K <- max(abs(U)); list(k = which.max(abs(U)), p = min(1, 2 * exp(-6 * K^2 / (n^3 + n^2))))
}

# A break in a station's monthly mean difference from the median of the other stations (so weather common to all of them
# cancels): Pettitt's test on months with min_days or more days, at least min_months of them. Reported with the size of the
# shift (median after minus median before) when p is below p_max: a sensor swap, a move, a recalibration or a drifting
# sensor. Never adjusted. L as for neighbour_estimates(); needs three stations or more.
breakpoints <- function(L, min_months = 24, min_days = 20, p_max = 0.05) {
  none <- data.table(station = character(), stream = character(), parameter = character(), months = integer(), break_month = character(),
                     shift = numeric(), p = numeric(), result = character())
  if (is.null(L) || !nrow(L) || uniqueN(L$key) < 3) return(none)
  out <- list()
  for (p in unique(L$parameter)) {
    w <- L[parameter == p]; if (uniqueN(w$key) < 3) next
    w[, others := { n <- .N; if (n >= 3) vapply(seq_len(n), function(i) stats::median(value[-i]), 0) else rep(NA_real_, n) }, by = Date]
    m <- w[!is.na(others), .(d = mean(value - others), days = .N), by = .(key, month = format(Date, "%Y-%m"))][days >= min_days][order(key, month)]
    for (k in unique(m$key)) { x <- m[key == k]; if (nrow(x) < min_months) next
      t <- pettitt(x$d); if (is.na(t$p)) next
      ks <- strsplit(k, "__", fixed = TRUE)[[1]]; sh <- stats::median(x$d[-seq_len(t$k)]) - stats::median(x$d[seq_len(t$k)])
      out[[length(out) + 1]] <- data.table(station = ks[1], stream = ks[2] %||% "", parameter = p, months = nrow(x), break_month = x$month[t$k + 1],
                                           shift = signif(sh, 3), p = signif(t$p, 3), result = if (t$p < p_max) "break against the other stations" else "no break at this level") } }
  if (!length(out)) return(none)
  rbindlist(out)[order(p, station)]
}
