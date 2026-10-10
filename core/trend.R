# Ovid/trend.R  --  Trend statistics shared by the apps: Theil-Sen slope, Hodges-Lehmann location and the Mann-Kendall test
# with the Hamed & Rao (1998) correction for serial correlation. Part of Ovid Core (shared). Used by WLR (barologger drift)
# and GT (trends in annual ground temperature and active-layer thickness).

# Theil-Sen slope (median of pairwise slopes) and the intercept at t0.
theil_sen <- function(t, y, t0 = stats::median(t)) {
  n <- length(t); if (n < 3) return(c(slope = NA_real_, intercept = NA_real_))
  i <- rep(seq_len(n - 1L), (n - 1L):1L); j <- unlist(lapply(2:n, function(k) k:n))
  s <- (y[j] - y[i]) / (t[j] - t[i]); b <- stats::median(s[is.finite(s)])
  c(slope = b, intercept = stats::median(y - b * (t - t0)))
}

# Hodges-Lehmann location estimate (median of the Walsh averages).
hodges_lehmann <- function(y) { n <- length(y); i <- rep(seq_len(n), n:1); j <- unlist(lapply(seq_len(n), function(k) k:n)); stats::median((y[i] + y[j]) / 2) }

# Mann-Kendall test with the Hamed & Rao (1998) correction of the variance for autocorrelation.
# min_n: the fewest values tested (WLR: 10). Below 10 values the autocorrelation is not estimated and the plain variance is used.
mk_hamed_rao <- function(y, t = seq_along(y), alpha = 0.05, min_n = 10) {
  n <- length(y); if (n < min_n || n < 3) return(c(S = NA, z = NA, p = NA, n_eff_ratio = NA))
  S <- 0; for (k in 1:(n - 1)) S <- S + sum(sign(y[(k + 1):n] - y[k]))
  tie <- table(y); var0 <- (n * (n - 1) * (2 * n + 5) - sum(tie * (tie - 1) * (2 * tie + 5))) / 18
  ratio <- 1
  if (n >= 10) { b <- theil_sen(t, y)["slope"]; r <- rank(y - b * t)                          # ranks of the detrended series
    rho <- stats::acf(r, lag.max = n - 1, plot = FALSE)$acf[-1]
    sig <- abs(rho) > stats::qnorm(1 - alpha / 2) / sqrt(n)
    k <- seq_along(rho)[sig]
    ratio <- 1 + 2 / (n * (n - 1) * (n - 2)) * sum((n - k) * (n - k - 1) * (n - k - 2) * rho[k]) }
  v <- var0 * max(ratio, 1e-6)
  z <- if (S > 0) (S - 1) / sqrt(v) else if (S < 0) (S + 1) / sqrt(v) else 0
  c(S = S, z = z, p = 2 * stats::pnorm(-abs(z)), n_eff_ratio = ratio)
}

