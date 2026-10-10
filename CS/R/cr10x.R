# R/cr10x.R  --  CR10X / CR23X mixed-array files, labelled from Edlog .FSL files.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

ARRAY_CACHE <- new.env()

# An .FSL lists, for each output array:  "<id> Output_Table ..." then one line
# per field:  "<position> <label> <L|H>".  Returns list(id = labels).
parse_fsl <- function(path) {
  if (grepl("\\.(csi|dld)$", path, ignore.case = TRUE)) return(parse_edlog(path)$arrays)   # Edlog program: labels worked out as Edlog does
  l <- fix_enc(suppressWarnings(readLines(path, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
  out <- list(); cur <- NULL
  for (x in l) {
    if (grepl("^\\s*[0-9]+\\s+Output[_ ]Table", x, ignore.case = TRUE)) {
      cur <- sub("^\\s*([0-9]+).*$", "\\1", x); out[[cur]] <- character(); next }
    m <- regmatches(x, regexec("^\\s*([0-9]+)\\s+(\\S+)(\\s+[LHlh])?\\s*$", x))[[1]]
    if (!is.null(cur) && length(m)) out[[cur]][as.integer(m[2])] <- m[3]
  }
  out
}

# cr10x.csv may name several FSLs for one file ("ST01_240.fsl; ST02_186.fsl").
# Returns a list of entries (array id, labels, FSL name); the labels used for an
# array are the first entry whose id AND field count match the data.
load_fsl_set <- function(spec, cfg, cache = new.env()) {
  out <- list()
  for (nm in trim(strsplit(spec %||% "", ";", fixed = TRUE)[[1]])) {
    fp <- resolve_fsl(nm, cfg); if (!nzchar(fp)) next
    if (is.null(cache[[fp]])) cache[[fp]] <- parse_fsl(fp)
    f <- cache[[fp]]
    for (id in names(f)) out[[length(out) + 1L]] <- list(id = id, labs = f[[id]], name = basename(fp))
  }
  out
}

resolve_fsl <- function(fsl, cfg) {
  if (!nzchar(fsl)) return("")
  for (p in c(path.expand(fsl), file.path(cfg$project_dir, "config", "fsl", fsl))) if (file.exists(p)) return(p)
  say("  FSL not found: ", fsl, " (using position labels)", col = "orange"); ""
}

guess_cr10x_id <- function(path) {
  s <- tools::file_path_sans_ext(fix_enc(basename(path)))
  s <- gsub("_final_storage_[0-9]+|FSArea[0-9]*|\\([0-9]+\\)|[0-9]{4,}.*$|^CR10X", "", s, ignore.case = TRUE)
  s <- toupper(gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", s)))
  paste0("CR10X_", if (nzchar(s)) s else "UNKNOWN")
}

sync_cr10x <- function(cfg, files) {
  f <- file.path(cfg$project_dir, "config", "cr10x.csv")
  cx <- if (file.exists(f)) read_cfg(f) else data.table(file = character(), import = character(),
    logger_id = character(), fsl = character(), start_year = character(), notes = character())
  # rows whose file is no longer at its path (a new drive letter or volume name, a folder copied elsewhere) follow it to the
  # one file now found with the same name and folder, so their logger and FSL settings are kept
  moved <- which(!cx$file %in% files & !file.exists(cx$file))
  if (length(moved)) {
    tail2 <- function(p) { p <- gsub("\\\\", "/", p); paste(utils::tail(strsplit(p, "/", fixed = TRUE)[[1]], 2), collapse = "/") }
    ft <- vapply(files, tail2, ""); n_moved <- 0L
    for (j in moved) { hit <- files[ft == tail2(cx$file[j]) & !files %in% cx$file]; if (length(hit) == 1L) { set(cx, j, "file", hit); n_moved <- n_moved + 1L } }
    if (n_moved) { backup_cfg(f, cx, "pipeline"); fwrite_safe(cx, f); say("  cr10x.csv: ", n_moved, " file(s) found at a new path; their settings were kept", col = "yellow") }
  }
  new <- setdiff(files, cx$file)
  if (length(new)) {
    cx <- rbind(cx, data.table(file = new, import = "yes", logger_id = vapply(new, guess_cr10x_id, ""),
                               fsl = "", start_year = "", notes = ""), fill = TRUE)
    for (v in names(cx)) set(cx, which(is.na(cx[[v]])), v, "")
    backup_cfg(f, cx, "pipeline")
    fwrite_safe(cx, f)
    say("  cr10x.csv: added ", length(new), " mixed-array file(s) - set logger_id and fsl", col = "yellow")
  }
  cx
}

cr10x_enable <- function(man, cfg) {
  arr <- which(man$status == "unsupported_mixed_array")
  if (!length(arr)) return(man)
  cx <- sync_cr10x(cfg, man$path[arr])
  k <- match(man$path[arr], cx$file)
  use <- !tolower(cx$import[k]) %in% c("no", "n", "false", "0")
  i <- arr[use]; k <- k[use]
  man[i, `:=`(status = "ok", table = "ARRAY", model = "CR10X", id_source = "cr10x.csv",
              station_name = tools::file_path_sans_ext(fix_enc(basename(path))),
              logger_id = fifelse(nzchar(cx$logger_id[k]), cx$logger_id[k], vapply(path, guess_cr10x_id, "")))]
  man[arr[!use], status := "cr10x_import_off"]
  man
}

# Lines of a mixed-array file. Also accepts Campbell "printable ASCII"
# (01+0101.  02+2012.  03+0311. ...), which is rewritten as comma-separated.
read_array_lines <- function(path) {
  lines <- fix_enc(suppressWarnings(readLines(path, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
  lines <- sub("[,\t ]+$", "", lines); lines <- lines[nzchar(trim(lines))]
  if (length(lines) && grepl("^\\s*01[+-][0-9.]+\\s+02[+-][0-9.]+", lines[1])) {
    all <- paste(lines, collapse = " ")
    tok <- regmatches(all, gregexpr("[0-9]{2}[+-][0-9.]+", all))[[1]]
    val <- sub("^\\+", "", substring(tok, 3)); rec <- cumsum(substr(tok, 1, 2) == "01")
    first <- !duplicated(rec)
    val[first] <- as.character(suppressWarnings(as.integer(as.numeric(val[first]))))   # array id "0101." -> "101"
    lines <- unname(vapply(split(val, rec), paste, "", collapse = ","))
  }
  lines
}

parse_array_file <- function(path, logger_id, fsl, start_year, mtime) {
  lines <- read_array_lines(path)
  if (!length(lines)) return(list())
  h <- head(lines, 50)
  sep <- if (sum(grepl("\t", h)) > sum(grepl(",", h))) "\t" else ","
  parts <- strsplit(lines, sep, fixed = TRUE)
  id <- trim(vapply(parts, `[`, "", 1L)); nf <- lengths(parts)
  valid <- grepl("^[0-9]{1,3}$", id) & nf >= 3L
  combo <- data.table(i = which(valid), id = id[valid], nf = nf[valid])
  cn <- combo[, .N, by = .(id, nf)][order(id, -N)]
  cn[, modal := seq_len(.N) == 1L, by = id]
  cn <- cn[modal | N >= 10L]
  cn[, table := ifelse(modal, paste0("A", id), paste0("A", id, "_", nf, "f"))]
  bad <- function(i, why, tb = NA) if (length(i))
    audit_add("parse", "malformed_row", "row", 1L, logger_id = logger_id, stream = tb, column = "<record>",
              old_value = substr(lines[i], 1, 80), reason = why, source_file = path)
  bad(which(!valid), "line does not start with an array id")
  rare <- combo[!cn, on = c("id", "nf")]
  bad(rare$i, "field count differs from the rest of this array", paste0("A", rare$id))
  n <- function(x) suppressWarnings(as.numeric(x))
  whole <- function(v) all(is.na(v) | v == round(v))
  share <- function(ok) mean(ok, na.rm = TRUE) >= 0.95
  out <- list()
  for (r in seq_len(nrow(cn))) {
    idx <- combo[id == cn$id[r] & nf == cn$nf[r], i]; k <- cn$nf[r]; tb <- cn$table[r]
    m <- matrix(unlist(parts[idx], use.names = FALSE), ncol = k, byrow = TRUE)
    hit <- Filter(function(e) e$id == cn$id[r] && length(e$labs) == k && !anyNA(e$labs), fsl)
    labelled <- length(hit) > 0; labs <- if (labelled) hit[[1]]$labs else NULL
    yi <- di <- hi <- si <- NA_integer_
    if (labelled) {
      g <- function(rx) { w <- grep(rx, labs, ignore.case = TRUE); w <- w[w > 1L]; if (length(w)) w[1] else NA_integer_ }
      yi <- g("^year"); di <- g("^day"); hi <- g("^(hour|hr|time)"); si <- g("^sec")
    } else {
      c2 <- n(m[, 2]); c3 <- n(m[, 3]); c4 <- if (k >= 4) n(m[, 4]) else NA
      is_day  <- function(v) whole(v) && share(v >= 1 & v <= 366)
      is_hhmm <- function(v) whole(v) && share(v >= 0 & v <= 2400 & v %% 100 < 60)
      if (share(c2 >= 1980 & c2 <= 2100) && is_day(c3)) { yi <- 2L; di <- 3L; if (k >= 4 && is_hhmm(c4)) hi <- 4L }
      else if (is_day(c2)) { di <- 2L; if (is_hhmm(c3)) hi <- 3L }
    }
    if (is.na(di)) { bad(idx, "no day-of-year field found; assign an .FSL in cr10x.csv", tb); next }
    day <- n(m[, di])
    year <- if (!is.na(yi)) n(m[, yi]) else if (!is.na(start_year)) start_year + cumsum(c(0, diff(day) < 0)) else NULL
    if (is.null(year)) { bad(idx, "no year in the data; set start_year in cr10x.csv", tb); next }
    hm <- if (!is.na(hi)) n(m[, hi]) else 0; sc <- if (!is.na(si)) n(m[, si]) else 0
    ok <- !is.na(year) & !is.na(day) & !is.na(hm) & year >= 1900 & year <= 2200 & day >= 1 & day <= 366
    ts <- rep(as.POSIXct(NA, tz = "UTC"), length(day))
    ts[ok] <- ISOdatetime(year[ok], 1, 1, 0, 0, 0, tz = "UTC") + (day[ok] - 1) * 86400 +
      (hm[ok] %/% 100) * 3600 + (hm[ok] %% 100) * 60 + fifelse(is.na(sc[ok] + 0), 0, sc[ok] + 0)
    stamp <- format(ts, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    stamp[is.na(ts)] <- paste0("invalid year/day/time: ", if (!is.na(yi)) m[is.na(ts), yi] else "", "/", m[is.na(ts), di])
    keep <- setdiff(seq_len(k), c(1L, yi, di, hi, si))
    nm <- if (labelled) make.unique(labs[keep], sep = "_dup") else sprintf("F%02d", keep)
    dt <- as.data.table(m[, keep, drop = FALSE]); setnames(dt, nm)
    dt <- cbind(data.table(TIMESTAMP = stamp), dt)
    dt[, `:=`(source_file = paste0(path, "#", tb), .mtime = mtime)]
    setattr(dt, "fsl_name", if (labelled) hit[[1]]$name else "")
    out[[tb]] <- dt
  }
  out
}

# ---- find mixed-array files without running the whole pipeline -------------
cr10x_scan <- function(cfg) {
  cfg$project_dir <- path.expand(cfg$project_dir)
  dir.create(file.path(cfg$project_dir, "config"), recursive = TRUE, showWarnings = FALSE)
  dirs <- path.expand(cfg$raw_dirs); dirs <- dirs[dir.exists(dirs)]
  files <- unlist(lapply(dirs, list.files, pattern = "\\.dat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
  files <- files[!startsWith(basename(files), "._")]
  arr <- files[vapply(files, function(f) identical(read_header(f)$status, "unsupported_mixed_array"), logical(1))]
  sync_cr10x(cfg, arr)
}

# ---- every distinct .FSL (or Edlog .CSI/.DLD program) under a folder (backup copies collapse to one) ----
scan_fsl_dir <- function(dir) {
  f <- list.files(path.expand(dir), pattern = "\\.(fsl|csi|dld)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  f <- f[!startsWith(basename(f), "._")]
  if (!length(f)) return(list())
  md5 <- unname(tools::md5sum(f)); keep <- !duplicated(md5)
  copies <- table(md5)
  lapply(which(keep), function(i) {
    l <- fix_enc(suppressWarnings(readLines(f[i], warn = FALSE, skipNul = TRUE, encoding = "latin1")))
    d <- trim(sub("^\\s*Date:\\s*", "", grep("^\\s*Date:", l, value = TRUE)[1]))
    arr <- parse_fsl(f[i])
    blank <- length(arr) > 0 && all(unlist(lapply(arr, function(a) grepl("^_+", a[-(1:4)]))))
    list(name = basename(f[i]), path = f[i], date = { x <- as.Date(d, format = "%m/%d/%Y"); if (is.na(x)) as.Date(file.mtime(f[i])) else x }, arrays = arr,
         blank = blank, copies = as.integer(copies[[md5[i]]]))
  })
}

# ---- which arrays (id + field count) a mixed-array file holds, and when ----
array_signature <- function(path) {
  l <- read_array_lines(path)
  if (!length(l)) return(NULL)
  h <- head(l, 50); sep <- if (sum(grepl("\t", h)) > sum(grepl(",", h))) "\t" else ","
  id <- sub("[,\t].*$", "", l)
  nf <- nchar(l) - nchar(gsub(sep, "", l, fixed = TRUE)) + 1L
  ok <- grepl("^[0-9]{1,3}$", id) & nf >= 3L
  if (!any(ok)) return(NULL)
  s <- data.table(id = id[ok], nf = nf[ok], i = which(ok))[, .(n = .N, a = i[1], b = i[.N]), by = .(id, nf)]
  dte <- function(i) {
    p <- strsplit(l[i], sep, fixed = TRUE)
    y <- suppressWarnings(as.numeric(vapply(p, `[`, "", 2L))); d <- suppressWarnings(as.numeric(vapply(p, `[`, "", 3L)))
    r <- rep(as.Date(NA), length(i)); g <- !is.na(y) & !is.na(d) & y >= 1980 & y <= 2100 & d >= 1 & d <= 366
    r[g] <- as.Date(sprintf("%d-01-01", as.integer(y[g]))) + floor(d[g]) - 1; r
  }
  s[, `:=`(first = pmin(dte(a), dte(b), na.rm = TRUE), last = pmax(dte(a), dte(b), na.rm = TRUE))]
  s[order(-n)]
}

# ---- match FSLs to files: same array id and field count; when several
#      programs fit, the one dated most recently before the data starts ------
cr10x_automatch <- function(cfg, fsl_dir, set_logger_id = TRUE, progress = function(i, n) NULL) {
  cfg$project_dir <- path.expand(cfg$project_dir)
  fsls <- scan_fsl_dir(fsl_dir)
  if (!length(fsls)) stop("No .fsl files found under ", fsl_dir)
  cx <- cr10x_scan(cfg)
  if (!nrow(cx)) stop("No CR10X mixed-array files found in the raw folders.")
  usable <- Filter(function(f) !f$blank, fsls)
  fdir <- file.path(cfg$project_dir, "config", "fsl"); dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
  res <- vector("list", nrow(cx)); md5 <- unname(tools::md5sum(cx$file)); first_of <- cx$file[match(md5, md5)]
  for (r in seq_len(nrow(cx))) {
    progress(r, nrow(cx))
    sig <- if (file.exists(cx$file[r])) array_signature(cx$file[r]) else NULL
    if (is.null(sig)) { res[[r]] <- data.table(file = cx$file[r], status = "unreadable"); next }
    sig[, main := n >= 10 | seq_len(.N) == 1L, by = id]
    tot <- sum(sig$n); picks <- character(); covered <- 0; alt <- character(); miss <- character()
    for (j in which(sig$main)) {
      cand <- Filter(function(f) { a <- f$arrays[[sig$id[j]]]; !is.null(a) && length(a) == sig$nf[j] }, usable)
      if (!length(cand)) { miss <- c(miss, sprintf("A%s (%d fields, %s rows)", sig$id[j], sig$nf[j], format(sig$n[j], big.mark = ","))); next }
      d <- as.Date(vapply(cand, function(f) as.character(f$date), "")); s0 <- sig$first[j]; e0 <- sig$last[j]
      best <- 1L
      if (length(cand) > 1 && !is.na(s0)) {
        ok <- which(!is.na(d) & d <= e0); bef <- ok[d[ok] <= s0]
        best <- if (length(bef)) bef[which.max(d[bef])] else if (length(ok)) ok[which.min(d[ok])] else which.min(d)
        same <- vapply(cand[-best], function(f) identical(f$arrays[[sig$id[j]]], cand[[best]]$arrays[[sig$id[j]]]), logical(1))
        alt <- c(alt, paste0(vapply(cand[-best], `[[`, "", "name"), " for A", sig$id[j], ifelse(same, " (same labels)", " (different labels)")))
      }
      covered <- covered + sig$n[j]; picks <- c(picks, cand[[best]]$name)
      file.copy(cand[[best]]$path, file.path(fdir, cand[[best]]$name), overwrite = TRUE)
    }
    picks <- unique(picks)
    res[[r]] <- data.table(file = cx$file[r],
      status = if (!length(picks)) "no matching FSL" else if (covered >= 0.98 * tot) "matched" else "partly matched",
      fsl = paste(picks, collapse = "; "), rows = tot, rows_labelled_pct = round(100 * covered / tot, 1),
      first = if (all(is.na(sig$first))) as.Date(NA) else min(sig$first, na.rm = TRUE),
      last = if (all(is.na(sig$last))) as.Date(NA) else max(sig$last, na.rm = TRUE),
      arrays = paste(sprintf("A%s:%df x%s", sig$id, sig$nf, sig$n)[seq_len(min(5, nrow(sig)))], collapse = "; "),
      other_fsl_that_fit = paste(unique(alt), collapse = "; "), arrays_without_fsl = paste(miss, collapse = "; "),
      identical_copy_of = if (first_of[r] != cx$file[r]) basename(first_of[r]) else "")
  }
  res <- rbindlist(res, fill = TRUE)
  k <- match(cx$file, res$file); has <- !is.na(res$fsl[k]) & nzchar(res$fsl[k])
  cx[has, fsl := res$fsl[k][has]]
  if (set_logger_id) {
    one <- sub(";.*$", "", cx$fsl); pre <- toupper(sub("^([A-Za-z]{2,5})[0-9]{2}_[0-9]{3}.*$", "\\1", one))
    byfsl <- has & grepl("^[A-Za-z]{2,5}[0-9]{2}_[0-9]{3}", one)
    ser <- regmatches(basename(cx$file), regexpr("CR10X[_ ]+[0-9]{4,6}", basename(cx$file), ignore.case = TRUE))
    byser <- grepl("CR10X[_ ]+[0-9]{4,6}", basename(cx$file), ignore.case = TRUE)
    cx[byfsl, logger_id := paste0("CR10X_", pre[byfsl])]
    cx[byser, logger_id := paste0("CR10X_", sub("^CR10X[_ ]+", "", ser, ignore.case = TRUE))]
  }
  backup_cfg(file.path(cfg$project_dir, "config", "cr10x.csv"), cx, "automatch")
  fwrite_safe(cx, file.path(cfg$project_dir, "config", "cr10x.csv"))
  res[, file_name := basename(file)]
  setcolorder(res, c("file_name", "status", "fsl"))
  fwrite(res, file.path(cfg$project_dir, "config", "cr10x_fsl_matches.csv"))
  list(matches = res, fsl_found = length(fsls), fsl_blank = sum(vapply(fsls, `[[`, logical(1), "blank")),
       fsl_copies = sum(vapply(fsls, `[[`, integer(1), "copies")))
}

cr10x_expand <- function(man, cfg) {
  unlink(unlist(mget(ls(ARRAY_CACHE), envir = ARRAY_CACHE))); rm(list = ls(ARRAY_CACHE), envir = ARRAY_CACHE)
  w <- which(man$status == "ok" & man$format %in% "mixed-array")
  if (!length(w)) return(man)
  cx <- sync_cr10x(cfg, character())
  fsl_cache <- new.env(); extra <- list()
  for (i in w) {
    c1 <- cx[file == man$path[i]][1]
    tabs <- parse_array_file(man$path[i], man$logger_id[i], load_fsl_set(c1$fsl, cfg, fsl_cache),
                             suppressWarnings(as.numeric(c1$start_year)), man$mtime[i])
    if (!length(tabs)) { man[i, status := "no_usable_arrays"]; next }
    rr <- man[rep(i, length(tabs))]
    rr[, `:=`(table = names(tabs), src = paste0(path, "#", names(tabs)),
              columns = lapply(tabs, function(d) setdiff(names(d), c("source_file", ".mtime"))),
              program = vapply(tabs, function(d) { f <- attr(d, "fsl_name"); if (nzchar(f)) paste0("FSL:", f) else "no FSL (position labels)" }, ""))]
    for (tb in names(tabs)) {                      # parked on disk, not in memory, until stitched
      f <- tempfile("cr10x_", fileext = ".rds"); saveRDS(tabs[[tb]], f, compress = FALSE)
      assign(paste0(man$path[i], "#", tb), f, envir = ARRAY_CACHE)
    }
    extra[[length(extra) + 1L]] <- rr
  }
  rbind(man[!(seq_len(nrow(man)) %in% w & status == "ok")], rbindlist(extra), fill = TRUE)
}

