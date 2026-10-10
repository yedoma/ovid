# R/f2b.R  --  Reading GeoPrecision FG2 logger files (.f2b) directly, without an FG2-Shell export.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# The .f2b is a byte stream; this decodes it the way FG2-Shell does (checked row for row against FG2-Shell V3.93a exports):
#   fe 00 / fe 01 stand for the bytes fe / ff; fe NN (NN >= 2) is a marker (02 block, 03 readout, 04 new parameter set, 05 file start, 06)
#   00-3f   channel number, then a big-endian float32 (below -1e30 = the logger's NoReply)
#   bf      uint32: time of the record just read, in 2 s steps since 1998-01-01 (the logger clock; 1998 = clock not set)
#   fb      uint16: battery, mV + 32768          fa  uint16: logging period / 2 + 32768
#   be      uint32: parameter-set time           f3  uint16: battery used, per mille + 32768
# A record starts when the channel number does not rise; a record without its own time follows the last one by the period.

F2B_EPOCH <- as.POSIXct("1998-01-01", tz = "UTC")

f2b_decode <- function(path) {
  r <- as.integer(readBin(path, "raw", file.info(path)$size))
  n <- length(r); fe <- which(r == 254L & seq_len(n) < n)
  if (length(fe) > 1L) { keep <- rep(TRUE, length(fe)); for (k in 2:length(fe)) if (keep[k - 1L] && fe[k] == fe[k - 1L] + 1L) keep[k] <- FALSE; fe <- fe[keep] }
  nx <- r[fe + 1L]; esc <- fe[nx < 2L]
  r[esc] <- 254L + nx[nx < 2L]
  mk <- logical(n); mk[fe[nx >= 2L]] <- TRUE
  if (length(esc)) { r <- r[-(esc + 1L)]; mk <- mk[-(esc + 1L)] }
  n <- length(r)
  cap <- n %/% 5L + 10L
  ch <- integer(cap); rid <- integer(cap); vpos <- integer(cap); nv <- 0L
  rtime <- rep(NA_real_, cap); rper <- rep(NA_real_, cap); bat <- rep(NA_real_, cap)
  mrec <- integer(); mtype <- integer()
  rec <- 0L; lastch <- 99L; i <- 1L; per <- NA_real_; unknown <- 0L; cut <- 0L
  u16 <- function(j) r[j] * 256 + r[j + 1L]
  u32 <- function(j) ((r[j] * 256 + r[j + 1L]) * 256 + r[j + 2L]) * 256 + r[j + 3L]
  while (i <= n) {
    c <- r[i]
    if (mk[i]) { mtype <- c(mtype, r[i + 1L]); mrec <- c(mrec, rec); lastch <- 99L; i <- i + 2L; next }
    if (c < 64L) {
      if (i + 4L > n) break
      if (any(mk[(i + 1L):(i + 4L)])) { cut <- cut + 1L; i <- i + 1L; next }   # a value cut short by a marker (the logger restarted mid-write)
      if (c <= lastch) { rec <- rec + 1L; rper[rec] <- per }
      nv <- nv + 1L; ch[nv] <- c; rid[nv] <- rec; vpos[nv] <- i + 1L
      lastch <- c; i <- i + 5L
    } else if (c == 0xfb) { if (i + 2L > n) break; if (rec) bat[rec] <- (u16(i + 1L) - 32768) / 1000; i <- i + 3L
    } else if (c == 0xfa) { if (i + 2L > n) break; per <- 2 * (u16(i + 1L) - 32768); i <- i + 3L
    } else if (c == 0xf3) { i <- i + 3L
    } else if (c == 0xbf) { if (i + 4L > n) break; if (rec) rtime[rec] <- 2 * u32(i + 1L); i <- i + 5L
    } else if (c == 0xbe) { i <- i + 5L
    } else { unknown <- unknown + 1L; i <- i + 1L }
  }
  vpos <- vpos[seq_len(nv)]
  val <- if (nv) readBin(as.raw(r[rep(vpos, each = 4L) + 0:3]), "numeric", n = nv, size = 4, endian = "big") else numeric()
  # times: a record without its own time follows the one before by the logging period
  tt <- rtime[seq_len(rec)]; p <- rper[seq_len(rec)]; p[is.na(p)] <- 0
  if (rec) { g <- cumsum(!is.na(tt)); tt <- ave(seq_len(rec), g, FUN = function(k) if (is.na(tt[k[1]])) NA_real_ else tt[k[1]] + c(0, cumsum(p[k][-1]))) }
  list(ch = ch[seq_len(nv)], rec = rid[seq_len(nv)], val = val, time = F2B_EPOCH + tt, period = rper[seq_len(rec)], battery = bat[seq_len(rec)],
       marker = mtype, marker_rec = mrec, unknown = unknown, cut = cut)
}

read_f2b <- function(path, header_only = FALSE) {
  b <- basename(path)
  stem <- regmatches(b, regexpr("[0-9]{14}", b))
  meta <- list(format = "fg2_f2b", type = "geoprecision", serial = norm_serial(sub("_.*$", "", b)), model = "GeoPrecision FG2", title = NA_character_,
               offset_hours = NA_real_, interval_s = NA_real_, rows = NA_integer_,
               download = if (length(stem)) as.POSIXct(strptime(stem, "%Y%m%d%H%M%S", tz = "UTC")) else NA)
  if (header_only) return(list(meta = meta))
  d <- f2b_decode(path)
  nr <- length(d$time); meta$rows <- nr
  if (!nr || !length(d$ch)) return(list(meta = meta, data = empty_data(), events = empty_events()))
  wide <- data.table(TIMESTAMP = d$time, ord = seq_len(nr))
  chs <- sort(unique(d$ch)); chans <- paste0("T", chs + 1L)                  # FG2-Shell numbers the channels #1, #2 ...
  errs <- vector("list", length(chs) + 1L)
  for (k in seq_along(chs)) { s <- d$ch == chs[k]; v <- rep(NA_real_, nr); v[d$rec[s]] <- d$val[s]
    nrp <- !is.na(v) & v < -1e30; v[nrp] <- NA
    set(wide, j = chans[k], value = v); errs[[k]] <- ifelse(nrp, "logger reported (NoReply)", NA_character_) }
  set(wide, j = "HK-BAT", value = d$battery); errs[[length(chs) + 1L]] <- rep(NA_character_, nr)
  long <- melt_channels(wide, c(chans, "HK-BAT"), c(rep("temp", length(chs)), "battery"), errs)
  long <- long[!(variable == "battery" & is.na(value) & is.na(err))]           # battery is written only every few records
  # a channel written at no record (never happens in a good file) and records with no time are left out by the readers' callers
  np <- d$marker == 4L & d$marker_rec < nr
  evt <- if (any(np)) data.table(TIMESTAMP = d$time[d$marker_rec[np] + 1L], event = "NEW PARAMETER SET (.f2b)") else empty_events()
  if (d$cut) evt <- rbind(evt, data.table(TIMESTAMP = d$time[nr], event = paste(d$cut, "value(s) cut short by a logger restart were skipped (.f2b)")))
  per <- d$period[!is.na(d$period)]
  meta$interval_s <- if (length(per)) per[length(per)] else step_of(d$time)
  meta$channels <- length(chs)
  list(meta = meta, data = long, events = evt)
}
