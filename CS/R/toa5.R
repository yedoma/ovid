# R/toa5.R  --  Finding Campbell files and reading TOA5: headers, logger identity, timestamps.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

resolve_logger_id <- function(serial, station_name) {
  station_name <- fix_enc(station_name)
  serial <- trim(serial)
  if (grepl("^[0-9]{2,}$", serial) && serial != "16777215") return(c(serial, "header serial"))
  nm  <- gsub("CR[0-9]+X?(Series)?", " ", station_name, ignore.case = TRUE)   # drop model tokens
  hit <- regmatches(nm, gregexpr("(?<![0-9])[0-9]{4,6}(?![0-9])", nm, perl = TRUE))[[1]]
  hit <- hit[!grepl("^(19|20)[0-9]{2}$", hit)]                                # not a year
  if (length(hit)) return(c(hit[1], "number in station name"))
  c(paste0("NOSN_", toupper(gsub("[^A-Za-z0-9]+", "_", trim(station_name)))), "station name (no serial)")
}

read_header <- function(path) {
  out <- list(path = path, format = NA_character_, station_name = NA_character_, model = NA_character_,
              serial = NA_character_, os = NA_character_, program = NA_character_, signature = NA_character_,
              table = NA_character_, columns = list(character()), units = list(character()),
              process = list(character()), sep = NA_character_, status = "unrecognised_format")
  if (file.size(path) == 0) { out$status <- "empty_file"; return(out) }
  sig <- readBin(path, "raw", 6L); sig <- rawToChar(sig[sig != as.raw(0)])
  if (grepl("^\"TOB3\"", sig, useBytes = TRUE)) return(tob3_manifest_row(out, path))
  if (grepl("^\"TOB[12]\"", sig, useBytes = TRUE)) { out$format <- substr(sig, 2, 5); out$status <- "unsupported_binary_TOB" ; return(out) }
  th <- toa5_head(path)                                         # core logger_files.R
  if (th$status == "not_toa5") {
    top <- fix_enc(suppressWarnings(readLines(path, n = 12L, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
    top <- top[nzchar(trim(top))]
    if (length(top) && (mean(grepl("^[0-9]{1,3}[,\t][0-9]{1,4}[,\t][0-9]{1,4}([,\t]|$)", top)) >= 0.5 ||
                        grepl("^\\s*01[+-][0-9.]+\\s+02[+-][0-9.]+", top[1]))) {
      out$format <- "mixed-array"; out$status <- "unsupported_mixed_array" }
    return(out)
  }
  if (is.null(th$env)) { out$status <- th$status; return(out) }
  env <- th$env
  out$format <- "TOA5"; out$station_name <- env[2]; out$model <- env[3]; out$serial <- env[4]
  out$os <- env[5]; out$program <- env[6]; out$signature <- env[7]; out$table <- env[8]; out$sep <- th$sep
  if (th$status != "ok") { out$status <- th$status; return(out) }
  out$columns <- list(th$columns); out$units <- list(th$units); out$process <- list(th$process)
  out$status  <- "ok"
  out
}

discover_files <- function(cfg) {
  dirs  <- sub("[/\\\\]+$", "", path.expand(trim(cfg$raw_dirs)))
  miss  <- dirs[!dir.exists(dirs)]
  if (length(miss)) say("  raw folder not found: ", paste(miss, collapse = ", "), col = "orange")
  files <- raw_file_list(dirs, "\\.dat$")$path                    # core logger_files.R (no macOS resource forks)
  if (!length(files)) stop("No .dat files found under: ", paste(dirs, collapse = ", "))
  # headers and checksums of files that have not changed since the last run (same size and time) are taken from the cache
  hc_file <- if (isTRUE(cfg$incremental) && nzchar(cfg$project_dir %||% "")) file.path(cfg$project_dir, "cache", "headers.rds") else ""
  hc <- if (nzchar(hc_file) && file.exists(hc_file)) tryCatch(readRDS(hc_file), error = function(e) list()) else list()
  sig <- paste(files, file.size(files), as.numeric(file.mtime(files)), STEP_HASH$headers, sep = "|")
  hdr <- lapply(seq_along(files), function(i) { h <- hc[[sig[i]]]; if (!is.null(h)) return(h$row)
    f <- files[i]; run_tick(i, length(files), basename(f)); tryCatch(as.data.table(read_header(f)), error = function(e) {
      problem(basename(f), paste("could not be opened:", conditionMessage(e)))
      data.table(path = f, status = "unreadable") }) })
  man <- rbindlist(hdr, fill = TRUE); man[, .sig := sig]
  for (v in c("format", "station_name", "model", "serial", "os", "program", "table", "sep"))
    if (!v %in% names(man)) man[, (v) := NA_character_]
  if (!"columns" %in% names(man)) man[, columns := list(list(character()))]
  man[, `:=`(size = file.size(path), mtime = as.numeric(file.mtime(path)))]
  ids <- t(mapply(resolve_logger_id, fifelse(is.na(man$serial), "", man$serial),
                  fifelse(is.na(man$station_name), "", man$station_name)))
  man[, `:=`(logger_id = ids[, 1], id_source = ids[, 2])]
  man[status != "ok", `:=`(logger_id = NA_character_, id_source = NA_character_)]
  man[status == "ok" & (is.na(table) | table == ""), table := "UNNAMED"]
  man[status == "ok" & table %in% cfg$skip_tables, status := "skipped_table"]
  man[, src := path]
  if (isTRUE(cfg$import_cr10x)) man <- cr10x_enable(man, cfg)
  # byte-identical copies ("file (1).dat") are read once
  man[status == "ok", md5 := raw_md5(path, .sig, hc)]
  if (nzchar(hc_file)) tryCatch({ keep <- which(vapply(hdr, function(h) !identical(h$status[1], "unreadable"), TRUE))      # an unreadable file is tried again next time
    saveRDS_safe(setNames(lapply(keep, function(i) list(row = hdr[[i]], md5 = man$md5[match(sig[i], man$.sig)])), sig[keep]), hc_file) },
                                error = function(e) problem("cache", paste("headers could not be saved:", conditionMessage(e))))
  setorder(man, -mtime, path)
  man[status == "ok" & !is.na(md5), dup_of := path[1], by = md5]                 # no checksum (unreadable): not a copy
  man[status == "ok" & path != dup_of, status := "duplicate_file"]
  if (isTRUE(cfg$import_cr10x)) man <- cr10x_expand(man, cfg)
  man[, .sig := NULL][]
}

# Timestamps are normally "YYYY-MM-DD HH:MM:SS". Files that passed through a
# spreadsheet come back as 3/22/2010 16:00 or similar; those are recognised
# per file (day/month order is worked out from the values) and rewritten.
normalise_timestamps <- function(x, file = "") {
  if (!length(x) || all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", head(x[nzchar(x)], 50)))) return(x)
  ts <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  try_f <- function(idx, fmts) for (f in fmts) {
    todo <- idx & is.na(ts); if (!any(todo)) break
    ts[todo] <<- as.POSIXct(x[todo], tz = "UTC", format = f)
  }
  iso <- grepl("^[0-9]{4}[-/]", x)
  try_f(iso, c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d"))
  sl <- grepl("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}", x)
  if (any(sl)) {
    p <- strsplit(sub("[ T].*$", "", x[sl]), "[/-]")
    a <- suppressWarnings(as.integer(vapply(p, `[`, "", 1L))); b <- suppressWarnings(as.integer(vapply(p, `[`, "", 2L)))
    y4 <- nchar(vapply(p, `[`, "", 3L)[1]) == 4
    dmy <- any(a > 12, na.rm = TRUE) && !any(b > 12, na.rm = TRUE)
    if (!dmy && !any(b > 12, na.rm = TRUE))
      problem(basename(file), "dates like 01/09/2018 could be day/month or month/day; month/day was assumed")
    d <- if (dmy) "%d/%m/" else "%m/%d/"; yy <- if (y4) "%Y" else "%y"
    xs <- x; xs[sl] <- sub("^([0-9]{1,2})-([0-9]{1,2})-", "\\1/\\2/", x[sl])
    for (f in paste0(d, yy, c(" %H:%M:%OS", " %H:%M", ""))) {
      todo <- sl & is.na(ts); if (!any(todo)) break
      ts[todo] <- as.POSIXct(xs[todo], tz = "UTC", format = f)
    }
  }
  ok <- !is.na(ts)
  x[ok] <- format(ts[ok], "%Y-%m-%d %H:%M:%S", tz = "UTC")
  x
}

read_toa5 <- function(path, cols, mtime, sep = ",") {
  if (is.na(sep) || !nzchar(sep)) sep <- ","
  sk <- toa5_skip(path)                                            # core logger_files.R: the header, blank lines left aside
  rd <- function() fread(path, skip = sk, header = FALSE, sep = sep, quote = "\"", fill = TRUE, blank.lines.skip = TRUE,
                         colClasses = "character", na.strings = NULL, encoding = "Latin-1",
                         showProgress = FALSE, strip.white = TRUE)
  n_lines <- length(readLines(path, n = sk + 1L, warn = FALSE, skipNul = TRUE))
  if (n_lines < sk + 1L) return(NULL)                              # header only
  dt <- tryCatch(withCallingHandlers(rd(), warning = function(w) invokeRestart("muffleWarning")),
                 error = function(e) { problem(basename(path), paste("could not be read and was left out:", conditionMessage(e))); NULL })
  if (is.null(dt) || !nrow(dt)) return(NULL)
  # a new program written into the same file starts another TOA5 header: the same columns go on being read, other columns
  # stop the read there (read on, they would land under the first header's names) and the file is reported
  hb <- which(trim(dt[[1]]) == "TOA5")
  if (length(hb)) { nx <- unlist(dt[hb[1] + 1L, -1L], use.names = FALSE); same <- identical(trim(c(dt[[1]][hb[1] + 1L], nx[nzchar(nx %||% "")])), trim(cols[nzchar(cols)]))
    if (same) dt <- dt[-unlist(lapply(hb, function(i) i + 0:3))]
    else { problem(basename(path), sprintf("a new TOA5 header with other columns after record %d (the program changed): the %d records after it were left out; split the file there",
                                           hb[1] - 1L, nrow(dt) - hb[1] - 3L)); dt <- dt[seq_len(hb[1] - 1L)] } }
  # a file that does not end with a line break was cut off (a copy that stopped): its last row can hold a value cut short
  sz <- file.size(path); con <- file(path, "rb"); seek(con, sz - 1); cut <- !identical(readBin(con, "raw", 1L), as.raw(10L)); close(con)
  if (cut && nrow(dt)) { problem(basename(path), "the file ends inside its last row (cut off): that row was left out"); dt <- dt[-nrow(dt)] }
  if (!nrow(dt)) return(NULL)
  cols <- make.unique(ifelse(is.na(cols) | cols == "", "UNNAMED", cols), sep = "_dup")
  nm <- c(cols, paste0("EXTRA_", seq_len(max(0, ncol(dt) - length(cols)))))[seq_len(ncol(dt))]
  setnames(dt, nm)
  if (!"TIMESTAMP" %in% nm) setnames(dt, 1L, "TIMESTAMP")
  for (v in names(dt)) if (is.character(dt[[v]]) && any(Encoding(dt[[v]]) != "unknown"))   # text read as Latin-1: UTF-8 kept as UTF-8 (Dene names), true Latin-1 converted
    set(dt, j = v, value = fix_enc(dt[[v]]))
  for (v in grep("^EXTRA_", names(dt), value = TRUE))            # trailing separators make empty columns
    if (all(is.na(dt[[v]]) | dt[[v]] == "")) set(dt, j = v, value = NULL)
  set(dt, j = "TIMESTAMP", value = normalise_timestamps(fix_enc(dt$TIMESTAMP), path))
  dt[, `:=`(source_file = path, .mtime = mtime)]
  dt
}

