# Ovid/neighbours.R  --  Comparing one site with the others that measure the same thing.
# Part of Ovid Core (shared by CS, WLR, GT, GS and RS). Sourced before each app's own R/ files.
#
# Works on daily means of one comparable set (one parameter, or one depth): a table of id (the site), date and
# value. Each site is compared with the median of the other sites on the same day. Sites differ by a steady amount
# that drifts with the season (snow, shade, landcover), so the expected difference is a running median over
# `window` days, and only a sudden departure from it stands out. Needs `min_keys` sites or more on a day.

nb_runmed <- function(x, window) {
  n <- length(x); if (!n) return(x)
  k <- min(window, if (n %% 2) n else n - 1L); if (k < 3) return(rep(stats::median(x), n))
  as.numeric(stats::runmed(x, k, endrule = "median"))
}

# Day by day: the site, the others' median, the difference, what was expected and whether the day departs.
neighbour_days <- function(L, floor = 0, k = 6, window = 31, min_keys = 3, lag = 3) {
  L <- as.data.table(L)[!is.na(value), .(id = as.character(id), date = as.Date(date), value)]
  L <- L[, .(value = mean(value)), by = .(id, date)]
  L[, n := .N, by = date]; L <- L[n >= min_keys]
  if (!nrow(L)) return(data.table(id = character(), date = as.Date(character()), value = numeric(), others = numeric(), offset = numeric(),
                                  expected = numeric(), departure = numeric(), bad = logical()))
  L[, others := { v <- value; vapply(seq_along(v), function(i) stats::median(v[-i]), 0) }, by = date]
  setorder(L, id, date); L[, offset := value - others]
  L[, expected := nb_runmed(offset, window), by = id]; L[, departure := offset - expected]
  L[, sc := stats::mad(departure), by = id]
  # while everything warms or cools fast (spring thaw, autumn freeze-up) a few days' difference in timing between sites
  # is normal: the allowance grows by how much the others change over `lag` days either side
  L[, allow := abs(shift(others, lag, type = "lead") - shift(others, lag)), by = id]; L[is.na(allow), allow := 0]
  L[, bad := abs(departure) > pmax(k * sc, floor) + allow]
  L[, .(id, date, value, others, offset, expected, departure, bad)]
}

# Runs of departing days, one row per run.
neighbour_periods <- function(days) {
  b <- days[bad == TRUE]
  if (!nrow(b)) return(data.table(id = character(), start = as.Date(character()), end = as.Date(character()), days = integer(), largest = numeric()))
  setorder(b, id, date); b[, run := cumsum(c(1L, diff(as.integer(date)) != 1L | id[-1] != id[-.N]))]
  b[, .(start = min(date), end = max(date), days = .N, largest = round(departure[which.max(abs(departure))], 2)), by = .(id, run)][, run := NULL][]
}

# One row per site: how closely it follows the others.
#  r_change: correlation of the change over `lag` days with the others' (near 1 = moves with them; low = out of
#  step). Not judged where the others hardly change (sd below min_change), as deep down.
#  verdict: ok, departs (has departing runs), out of step (r_change below r_min), or too short to judge.
neighbour_summary <- function(days, r_min = 0.5, min_days = 30, lag = 7, min_change = 0.2) {
  if (!nrow(days)) return(data.table(id = character(), days_compared = integer(), median_offset = numeric(), r_change = numeric(),
                                     days_departing = integer(), runs = integer(), largest_departure = numeric(), verdict = character()))
  d <- copy(days); setorder(d, id, date)
  w <- d[, .(id, date = date + lag, v0 = value, o0 = others)]                      # the same site `lag` days earlier
  d <- merge(d, w, by = c("id", "date"))[, `:=`(dv = value - v0, do = others - o0)]
  r <- d[, .(r_change = if (.N >= 10 && stats::sd(dv) > 0 && stats::sd(do) > min_change) round(stats::cor(dv, do), 2) else NA_real_), by = id]
  d <- copy(days)
  s <- d[, .(days_compared = .N, median_offset = round(stats::median(offset), 2), days_departing = sum(bad),
             largest_departure = if (any(bad)) round(departure[bad][which.max(abs(departure[bad]))], 2) else NA_real_), by = id]
  runs <- neighbour_periods(days)[, .(runs = .N), by = id]
  s <- merge(merge(s, r, by = "id", all.x = TRUE), runs, by = "id", all.x = TRUE); s[is.na(runs), runs := 0L]
  s[, verdict := fifelse(days_compared < min_days, "too short to judge",
                 fifelse(!is.na(r_change) & r_change < r_min, "out of step",
                 fifelse(days_departing > 0, "departs at times", "ok")))]
  s[order(verdict == "ok", -days_departing)]
}
