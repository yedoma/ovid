# Ovid/timestamps.R  --  Corrupt timestamps among good records, the same in every app.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# A logger whose memory or clock fails can write records stamped at random times (often with odd seconds) between its
# good ones, so they land inside months of good data. Removing a period would take the good records with them, so these
# are found record by record, in the order each file was written:
#   a record is stray when it is off the time grid the records follow AND it breaks the order the file was written in.
# A reading written in order but off the grid (one taken when the logger was connected, a short test at another
# interval, a clock set mid-file) is real and is kept; those are listed in the clock checks, where they can be removed.

# TRUE for the records on the longest run of timestamps that never goes backwards, in the order given (patience sorting)
in_sequence <- function(x) {
  n <- length(x); if (n < 3 || isFALSE(is.unsorted(x))) return(rep(TRUE, n))                # already in order: all of it
  tails <- numeric(0); tidx <- integer(0); prev <- integer(n); na <- anyNA(x)
  # tails is sorted by construction, so findInterval() need not check it each time (that check made the loop quadratic)
  for (i in seq_len(n)) { k <- findInterval(x[i], tails, checkSorted = FALSE, checkNA = na) + 1L; prev[i] <- if (k > 1L) tidx[k - 1L] else 0L; tails[k] <- x[i]; tidx[k] <- i }
  keep <- logical(n); i <- tidx[length(tidx)]; while (i > 0L) { keep[i] <- TRUE; i <- prev[i] }
  keep
}

# t: timestamps (POSIXct or seconds); file: the file each record came from; ord: the order it was written in that file.
# Returns TRUE for the stray records. Needs a logging interval of a minute or more.
stray_records <- function(t, file = rep("", length(t)), ord = seq_along(t)) {
  t <- as.numeric(t); n <- length(t); out <- logical(n)
  ok <- !is.na(t); dd <- diff(sort(unique(t[ok]))); dd <- dd[dd > 0]
  if (sum(ok) < 10 || !length(dd)) return(out)
  step <- as.numeric(names(which.max(table(dd)))); if (step < 60) return(out)
  ph <- t %% step; off <- ok & ph != as.numeric(names(which.max(table(ph[ok]))))
  if (!any(off) || mean(off[ok]) >= 0.5) return(out)
  for (ix in split(which(ok), file[ok])) {
    ix <- ix[order(ord[ix])]
    out[ix] <- off[ix] & !in_sequence(t[ix])
  }
  out
}

STRAY_REASON <- "corrupt timestamp among good records: off the time grid and out of the order the file was written in"

# ---- Clock corrections: the mechanics every app shares ---------------------------------------------------------------------
# Each app keeps its own table of corrections (which logger, file or serial a row is for) and its own audit lines; these two
# do the rest the same way everywhere.
# The records a correction row covers: start and end as typed (blank = open; a date alone as the end takes in its whole day),
# on times t. list(a, b, hit).
correction_span <- function(t, start = "", end = "") {
  a <- parse_date_cell(start %||% ""); b <- parse_date_cell(end %||% "", end = TRUE)
  list(a = a, b = b, hit = !is.na(t) & (is.na(a) | t >= a) & (is.na(b) | t <= b))
}
# New times for records in a span: moved by `minutes` (mode shift), or by a share of it that grows from none at a to all of it
# at b (mode drift: the USGS proration of a clock error found at download).
shift_times <- function(t, minutes, mode = "shift", a = min(t, na.rm = TRUE), b = max(t, na.rm = TRUE)) {
  if (identical(tolower(trim(mode %||% "")), "drift")) t + round(minutes * 60 * pmin(1, pmax(0, as.numeric(difftime(t, a, units = "secs")) / max(1, as.numeric(difftime(b, a, units = "secs"))))))
  else t + minutes * 60
}

# Clock faults in one file, from the order its rows were written in (ord) and their time stamps (t), one flag per row:
#   bad     no time, before `lo` or after `hi` (a clock never set, or reset to 1998 or 2000)
#   stray   a corrupt time stamp among good readings (stray_records(): off the time grid and out of written order); found
#           first, so one cannot make the good readings after it look reset. stray = FALSE leaves this test out
#   reset   the clock jumped back by more than a day behind the latest trusted time (a firmware reset, a flat clock battery)
#   resync  the clock was set back by less than a day: a reading kept but stamped at or after a time a later reading has
# The rows come back in written order with these columns added; what to do with each kind is the app's.
clock_faults <- function(t, ord = seq_along(t), lo = -Inf, hi = Inf, stray = TRUE) {
  rows <- data.table(ord = ord, TIMESTAMP = t)[order(ord)]
  rows[, bad := is.na(TIMESTAMP) | TIMESTAMP < lo | TIMESTAMP > hi]
  rows[, stray := FALSE]; if (isTRUE(stray)) rows[bad == FALSE, stray := stray_records(TIMESTAMP, ord = ord)]
  x <- as.numeric(rows$TIMESTAMP); ok <- !rows$bad & !rows$stray; reset <- rep(FALSE, nrow(rows)); cm <- -Inf
  xo <- x[ok]; reset[ok] <- xo < c(cm, cummax(xo)[-length(xo)]) - 86400      # behind the running maximum of the trusted rows before it
  rows[, reset := reset]
  keep <- ok & !reset; tk <- ifelse(keep, x, Inf)
  later_min <- c(rev(cummin(rev(tk)))[-1], Inf)                                                        # smallest trusted time written after each row
  rows[, resync := keep & x >= later_min]
  rows[]
}
