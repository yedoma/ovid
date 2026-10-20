# R/tob3.R  --  Reading Campbell TOB3 binary files (CR1000, CR3000, CR6 card and CPU tables) without converting them first.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.
#
# A TOB3 file is a 6-line ASCII header (as in TOA5, plus a line of data types) followed by fixed-size frames.
# Each frame: 12-byte header (seconds since 1990, sub-seconds, first record number; little-endian), as many
# whole records as fit, and a 4-byte footer (offset, flags E = empty and M = minor frame, validation stamp).
# Frames whose stamp is neither the file's stamp nor its complement are old or never written and are skipped.

TOB3_EPOCH <- as.POSIXct("1990-01-01", tz = "UTC")
TOB3_RES <- c(SecMsec = 1e-3, Sec100Usec = 1e-4, Sec10Usec = 1e-5, SecUsec = 1e-6, Sec = 1, SecNano = 1e-9)

tob3_type_size <- function(ty) {
  ty <- toupper(trim(ty)); n <- suppressWarnings(as.integer(sub("^ASCII\\(([0-9]+)\\)$", "\\1", ty)))
  sz <- c(FP2 = 2L, IEEE4 = 4L, IEEE4B = 4L, IEEE4L = 4L, IEEE8 = 8L, IEEE8B = 8L, IEEE8L = 8L, ULONG = 4L, LONG = 4L,
          UINT4 = 4L, INT4 = 4L, UINT2 = 2L, INT2 = 2L, USHORT = 2L, SHORT = 2L, BOOL = 1L, BOOL2 = 2L, BOOL4 = 4L,
          BOOL8 = 1L, BYTE = 1L, SECNANO = 8L, NSEC = 8L)[ty]
  ifelse(grepl("^ASCII\\(", ty), n, unname(sz))
}

tob3_interval <- function(x) {
  n <- suppressWarnings(as.numeric(sub("^\\s*([0-9.]+).*$", "\\1", x))); u <- toupper(trim(sub("^\\s*[0-9.]+\\s*", "", x)))
  n * unname(c(NSEC = 1e-9, USEC = 1e-6, MSEC = 1e-3, SEC = 1, MIN = 60, HR = 3600, HOUR = 3600, DAY = 86400)[u])
}

# header only: identity, columns and where the frames start
tob3_header <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  b <- readBin(con, "raw", 8192L)
  ends <- which(b[-length(b)] == as.raw(13) & b[-1] == as.raw(10))
  if (length(ends) < 6) stop("TOB3 header is incomplete")
  ln <- vapply(seq_len(6), function(i) { s <- if (i == 1) 1L else ends[i - 1] + 2L; rawToChar(b[s:(ends[i] - 1L)]) }, "")
  p <- lapply(ln, function(l) trim(parse_csv_line(trim(l), ",")))
  types <- p[[6]]; types <- types[nzchar(types)]
  f <- p[[3]][seq_along(types)]
  list(env = p[[1]], table = p[[2]][1], interval = tob3_interval(p[[2]][2]), frame = as.integer(p[[2]][3]),
       stamp = as.integer(p[[2]][5]), res = { r <- unname(TOB3_RES[trim(p[[2]][6])]); if (is.na(r)) 1e-4 else r },
       fields = f, units = c(p[[4]], rep("", length(f)))[seq_along(f)], process = c(p[[5]], rep("", length(f)))[seq_along(f)],
       types = types, sizes = tob3_type_size(types), start = ends[6] + 2L)
}

# one column of raw bytes (a matrix: bytes x records) to values
tob3_decode <- function(r, ty) {
  ty <- toupper(ty); n <- ncol(r); v <- as.vector(r)
  big <- grepl("B$", ty) || ty %in% c("FP2", "UINT4", "INT4", "UINT2", "INT2", "BOOL2", "BOOL4", "NSEC")
  end <- if (big) "big" else "little"
  switch(sub("[BL]$", "", ty),
    FP2 = { u <- readBin(v, "integer", n, size = 2, signed = FALSE, endian = "big")
            m <- bitwAnd(u, 0x1FFFL); e <- bitwAnd(bitwShiftR(u, 13L), 3L); s <- ifelse(bitwAnd(u, 0x8000L) > 0, -1, 1)
            x <- s * m / 10^e; x[e == 0L & m >= 8190L] <- NA_real_; x },
    IEEE4 = { x <- readBin(v, "double", n, size = 4, endian = end); x[!is.finite(x)] <- NA_real_; x },
    IEEE8 = { x <- readBin(v, "double", n, size = 8, endian = end); x[!is.finite(x)] <- NA_real_; x },
    ULONG = , UINT4 = { x <- as.numeric(readBin(v, "integer", n, size = 4, endian = end)); x + ifelse(x < 0, 2^32, 0) },
    LONG = , INT4 = as.numeric(readBin(v, "integer", n, size = 4, endian = end)),
    UINT2 = , USHORT = as.numeric(readBin(v, "integer", n, size = 2, signed = FALSE, endian = end)),
    INT2 = , SHORT = as.numeric(readBin(v, "integer", n, size = 2, endian = end)),
    BOOL = , BOOL8 = , BYTE = as.numeric(r[1, ] != as.raw(0)) * if (ty == "BYTE") 1 else -1,
    BOOL2 = , BOOL4 = as.numeric(colSums(r != as.raw(0)) > 0) * -1,
    NSEC = , SECNANO = { s <- readBin(r[1:4, , drop = FALSE], "integer", n, size = 4, endian = end); ns <- readBin(r[5:8, , drop = FALSE], "integer", n, size = 4, endian = end)
            as.numeric(s) + ns * 1e-9 + as.numeric(TOB3_EPOCH) },
    if (grepl("^ASCII", ty)) apply(r, 2, function(z) { z <- z[seq_len(match(as.raw(0), z, nomatch = length(z) + 1L) - 1L)]; trim(rawToChar(z)) })
    else rep(NA_real_, n))
}

# All valid records of a TOB3 file as a table shaped like read_toa5(): character TIMESTAMP, RECORD, one column per field.
read_tob3 <- function(path, mtime = NA, chunk_frames = 20000L) {
  h <- tob3_header(path); fs <- h$frame; rs <- sum(h$sizes)
  if (is.na(fs) || is.na(rs) || rs < 1 || fs <= 16) stop("TOB3 frame or record size not understood")
  nrec <- (fs - 16L) %/% rs; ok_stamp <- c(h$stamp, bitwXor(h$stamp, 0xFFFFL))
  off <- cumsum(c(0L, h$sizes))[seq_along(h$sizes)]
  total <- (file.size(path) - h$start + 1) %/% fs
  con <- file(path, "rb"); on.exit(close(con)); invisible(readBin(con, "raw", h$start - 1L))
  out <- list(); done <- 0
  u32 <- function(r) { x <- as.numeric(readBin(as.vector(r), "integer", ncol(r), size = 4, endian = "little")); x + ifelse(x < 0, 2^32, 0) }
  # records of frames given as a matrix (bytes x frames) whose data holds k records each
  grab <- function(m, k, t0, r0) {
    if (!ncol(m) || k < 1) return(NULL)
    d <- matrix(m[12L + seq_len(k * rs), , drop = FALSE], nrow = rs)
    vals <- lapply(seq_along(h$sizes), function(j) tob3_decode(d[off[j] + seq_len(h$sizes[j]), , drop = FALSE], h$types[j]))
    names(vals) <- h$fields
    i <- rep(seq_len(k) - 1L, ncol(m))
    c(list(.t = rep(t0, each = k) + i * (if (is.na(h$interval)) 0 else h$interval), RECORD = rep(r0, each = k) + i), vals)
  }
  while (done < total) {
    nf <- min(chunk_frames, total - done)
    b <- readBin(con, "raw", nf * fs); nf <- length(b) %/% fs; if (!nf) break
    m <- matrix(b[seq_len(nf * fs)], nrow = fs); done <- done + nf
    ft <- u32(m[(fs - 3L):fs, , drop = FALSE])
    valid <- (ft %/% 65536) %in% ok_stamp & bitwAnd(as.integer(ft %% 65536), 0x2000L) == 0L
    minor <- valid & bitwAnd(as.integer(ft %% 65536), 0x4000L) > 0
    t0 <- u32(m[1:4, , drop = FALSE]) + u32(m[5:8, , drop = FALSE]) * h$res; r0 <- u32(m[9:12, , drop = FALSE])
    full <- valid & !minor & t0 > 0
    if (any(full)) out[[length(out) + 1L]] <- grab(m[, full, drop = FALSE], nrec, t0[full], r0[full])
    for (w in which(minor)) {                      # a partly filled frame: its footer gives the unused tail, then minor frames back to front
      pos <- fs - as.integer(ft[w] %% 2048)
      while (pos > 16L) {
        f2 <- u32(m[(pos - 3L):pos, w, drop = FALSE]); sz <- as.integer(f2 %% 2048)
        if (sz < 16L || sz > pos || (sz - 16L) %% rs != 0L || !((f2 %/% 65536) %in% ok_stamp)) break
        mm <- m[(pos - sz + 1L):pos, w, drop = FALSE]; k <- (sz - 16L) %/% rs
        if (bitwAnd(as.integer(f2 %% 65536), 0x2000L) == 0L && k > 0)
          out[[length(out) + 1L]] <- grab(mm, k, u32(mm[1:4, , drop = FALSE]) + u32(mm[5:8, , drop = FALSE]) * h$res, u32(mm[9:12, , drop = FALSE]))
        pos <- pos - sz
      }
    }
  }
  if (!length(out)) return(NULL)
  dt <- rbindlist(lapply(out, as.data.table), use.names = TRUE)
  dt <- dt[.t > 0]; setorder(dt, .t, RECORD); dt <- unique(dt, by = c(".t", "RECORD"))
  tt <- as.POSIXct(dt$.t, origin = TOB3_EPOCH, tz = "UTC")
  sub_s <- any(abs(dt$.t - round(dt$.t)) > 1e-6)
  dt[, TIMESTAMP := format(tt, if (sub_s) "%Y-%m-%d %H:%M:%OS3" else "%Y-%m-%d %H:%M:%S", tz = "UTC")][, .t := NULL]
  setcolorder(dt, c("TIMESTAMP", "RECORD"))
  dt[, `:=`(source_file = path, .mtime = mtime)]
  dt[]
}

# first and last record time without reading the data (for tables that are only listed, such as 10 Hz ts_data)
tob3_span <- function(path) {
  h <- tob3_header(path); fs <- h$frame; n <- (file.size(path) - h$start + 1) %/% fs
  if (n < 1) return(c(NA_real_, NA_real_))
  con <- file(path, "rb"); on.exit(close(con)); ok_stamp <- c(h$stamp, bitwXor(h$stamp, 0xFFFFL))
  at <- function(i) { seek(con, h$start - 1 + (i - 1) * fs); r <- readBin(con, "raw", fs)
    v <- function(a) { x <- as.numeric(readBin(r[a], "integer", 1L, size = 4, endian = "little")); x + if (x < 0) 2^32 else 0 }
    if (!((v((fs - 3L):fs) %/% 65536) %in% ok_stamp)) return(NA_real_); v(1:4) + v(5:8) * h$res }
  ix <- unique(round(seq(1, n, length.out = min(n, 64L))))
  t <- vapply(ix, at, 0); t <- t[!is.na(t) & t > 0]
  if (!length(t)) return(c(NA_real_, NA_real_))
  range(t) + as.numeric(TOB3_EPOCH)
}

# read_header() for a TOB3 file: the same identity fields as TOA5, plus the time span from a few frames
tob3_manifest_row <- function(out, path) {
  out$format <- "TOB3"
  # safe: a header that cannot be read is reported as unsupported_binary_TOB on the next line
  h <- tryCatch(tob3_header(path), error = function(e) NULL)
  if (is.null(h) || !length(h$fields) || anyNA(h$sizes)) { out$status <- "unsupported_binary_TOB"; return(out) }
  env <- c(h$env, rep("", 8))[1:8]
  out$station_name <- env[2]; out$model <- env[3]; out$serial <- env[4]; out$os <- env[5]; out$program <- env[6]; out$signature <- env[7]
  out$table <- h$table; out$sep <- ","
  out$columns <- list(c("TIMESTAMP", "RECORD", h$fields)); out$units <- list(c("TS", "RN", h$units)); out$process <- list(c("", "", h$process))
  sp <- tryCatch(tob3_span(path), error = function(e) c(NA_real_, NA_real_))
  out$tob3_first <- sp[1]; out$tob3_last <- sp[2]
  out$status <- "ok"
  out
}
