# R/discover.R  --  Finding files, telling what each one is, and grouping each download into a bundle.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# A bundle is one download of one logger: its own file (.hex, .f2b, .hobo) plus any exports of it (Ruskin .txt/.xls,
# FG2-Shell .csv/.log, HOBOware .csv). Members are matched by type, folder, serial and file name (one name starts with
# the other), never by the name's site label. In each bundle the best readable file is used:
#   RBR .hex  >  Ruskin .txt  >  Ruskin .xls;   FG2-Shell .csv  >  .f2b (decoded by GT, R/f2b.R);   HOBOware .csv (the .hobo cannot be read)
# A bundle with nothing readable is listed as "needs export" with what to do.
# A HOBO water-level or conductivity logger (a pressure or conductivity channel in its CSV, or a U20/U24 model in its .hobo)
# is read by WLR, not GT: its whole bundle is listed as "water_logger" and none of it is read or asked to be exported.

SOURCE_PRIORITY <- c(rbr_hex = 1, rbr_txt = 2, rbr_xls = 3, fg2_csv = 1, fg2_f2b = 2, hobo_csv = 1, em50_xls = 1, campbell_toa5 = 1)

EXPORT_ADVICE <- c(rbr = "Open the .hex in Ruskin and export it as text (.txt), or check the file is complete.",
                   geoprecision = "Open the .f2b in FG2-Shell (or GP5W) and export it as .csv into the same folder.",
                   hobo = "Open the .hobo in HOBOware and export it as .csv into the same folder.",
                   em50 = "Open the data in ECH2O Utility and export it as .xls.",
                   campbell = "Mixed-array (CR10X) and binary tables are not read by GT: convert to TOA5 with LoggerNet's CardConvert, or process them in ovid CS.")

WATER_LOGGER_NOTE <- "HOBO water-level or conductivity logger: read by WLR, not GT"

# f() with what it would record (problems, warnings) kept instead of recorded, and its console text dropped: for a worker
# (core util.R par_map), whose records would be lost; the caller records them in the main process.
quiet_call <- function(f) {
  np0 <- length(AUDIT$problems); lg <- get0("LOG", ifnotfound = NULL); nl0 <- if (is.environment(lg)) length(lg$lines) else 0L; w <- list()
  on.exit({ AUDIT$problems <- AUDIT$problems[seq_len(np0)]; if (is.environment(lg)) lg$lines <- lg$lines[seq_len(nl0)] })   # also when par_map runs it here
  utils::capture.output(r <- withCallingHandlers(f(), warning = function(e) { w[[length(w) + 1L]] <<- e; invokeRestart("muffleWarning") }))
  list(r = r, problems = AUDIT$problems[seq_along(AUDIT$problems) > np0], warnings = w)
}

# File names in one canonical Unicode form (NFC), so an accented name written by macOS (NFD) matches its export

discover_files <- function(cfg, rules = NULL) {
  dirs <- path.expand(cfg$raw_dirs); dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) stop("None of the data folders exists: ", paste(cfg$raw_dirs, collapse = ", "))
  m <- raw_file_list(dirs, skip_dirs = c("output", "config", "library", "R-portable"))     # core logger_files.R
  if (!nrow(m)) return(data.table())
  m[, root := NULL][, dir := dirname(path)]; setcolorder(m, c("path", "file", "rel", "dir"))   # the order files.csv has always had
  # what is known about a file that has not changed since the last run (same size and time) comes from the cache
  hc_file <- if (isTRUE(cfg$incremental) && nzchar(cfg$project_dir %||% "")) file.path(path.expand(cfg$project_dir), "cache", "files.rds") else ""
  # safe: a damaged header cache is rebuilt
  hc <- if (nzchar(hc_file) && file.exists(hc_file)) tryCatch(readRDS(hc_file), error = function(e) list()) else list()
  m[, sig := paste(path, size, as.numeric(mtime), STEP_HASH$files, sep = "|")]
  known <- m$sig %in% names(hc)
  m[, md5 := raw_md5(path, sig, hc)]
  m[, format := ifelse(known, vapply(sig, function(x) hc[[x]]$format %||% NA_character_, ""), NA_character_)]
  m[is.na(format), format := vapply(path, sniff_format, "")]
  m[, type := FORMAT_INFO$type[match(format, FORMAT_INFO$format)]]
  m[, readable := FORMAT_INFO$readable[match(format, FORMAT_INFO$format)] %in% TRUE]
  m[, role := FORMAT_INFO$role[match(format, FORMAT_INFO$format)]]
  # identity from each file's header
  id_of <- function(i) {
    if (known[i]) return(hc[[m$sig[i]]]$id)
    if (is.na(m$type[i])) return(NULL)
    tryCatch(if (m$readable[i] || m$format[i] == "hobo_binary") read_logger_file(m$path[i], m$format[i], header_only = TRUE)$meta else name_identity(m$path[i], m$format[i]),
             error = function(e) { x <- name_identity(m$path[i], m$format[i]); x$error <- conditionMessage(e); x }) }
  # the headers not known yet are read in parallel (core util.R par_map; one at a time on Windows): what a reader notes is
  # kept there and noted here in file order, as one at a time; a file its worker did not read is read here
  todo <- which(!known & !is.na(m$type))
  if (run_workers() > 1L && length(todo)) {
    ids <- lapply(seq_len(nrow(m)), function(i) if (i %in% todo) NULL else id_of(i))
    got <- par_map(todo, function(i) quiet_call(function() id_of(i)), what = "reading file details")
    for (k in seq_along(todo)) { x <- got[[k]]
      ids[todo[k]] <- list(if (!is.list(x) || inherits(x, "error") || !identical(names(x), c("r", "problems", "warnings"))) id_of(todo[k])
                           else { for (p in x$problems) problem(p$where, p$problem); for (w in x$warnings) warning(w); x$r }) }
  } else ids <- lapply(seq_len(nrow(m)), id_of)
  gv <- function(k) vapply(ids, function(x) if (is.null(x) || is.null(x[[k]]) || !length(x[[k]])) NA_character_ else as.character(x[[k]])[1], "")
  m[, `:=`(serial = gv("serial"), model = gv("model"), title = gv("title"), stated_offset = suppressWarnings(as.numeric(gv("offset_hours"))),
           download = gv("download"), read_error = gv("error"), water = gv("water") %in% "TRUE")]
  if (nzchar(hc_file)) tryCatch({ dir.create(dirname(hc_file), recursive = TRUE, showWarnings = FALSE); ok <- vapply(ids, function(x) is.null(x$error), TRUE)   # a file that failed is tried again next time
    saveRDS_safe(setNames(lapply(which(ok), function(i) list(md5 = m$md5[i], format = m$format[i], id = ids[[i]])), m$sig[ok]), hc_file) },
    error = function(e) problem("cache", paste("file details could not be saved:", conditionMessage(e))))
  m[, sig := NULL]
  m[serial %in% c("", "NA"), serial := NA_character_]
  m[, download := as.POSIXct(ifelse(is.na(download) | download == "NA", NA_character_, download), tz = "UTC")]
  # an EM50 file without its serial in the name takes the serial of other files with the same logger name inside
  em <- m[type == "em50" & grepl("^EM[0-9]", file, ignore.case = TRUE) & !is.na(title), .(s = serial[1]), by = .(t = toupper(title))]
  m[type == "em50" & !grepl("^EM[0-9]", file, ignore.case = TRUE), serial := { s <- em$s[match(toupper(title), em$t)]; ifelse(is.na(s), serial, s) }]
  m[, status := fifelse(format %in% c("unrecognised_format", "unreadable", "empty_file"), format, "ok")]
  # byte-identical copies are read once
  setorder(m, md5, -mtime, path)
  m[, dup_of := copy_of(md5, path, status == "ok")]
  m[!is.na(dup_of), status := "duplicate_file"]
  setorder(m, rel)
  # file rules from the Raw files page
  if (!is.null(rules) && nrow(rules)) for (i in seq_len(nrow(rules))) {
    pat <- rules$pattern[i]; if (!nzchar(pat)) next
    hit <- tryCatch(grepl(pat, m$rel, ignore.case = TRUE) | m$file == pat, error = function(e) m$file == pat)
    a <- tolower(rules$action[i])
    if (a == "ignore") m[hit, status := "excluded_by_user"]
    else if (a == "serial" && nzchar(rules$serial[i])) m[hit, serial := norm_serial(rules$serial[i])]
    else if (a == "use") m[hit & status == "duplicate_file", status := "ok"]
  }
  m
}

# Group files into bundles and pick the file each bundle is read from.
make_bundles <- function(m) {
  m[, bundle := NA_character_]
  cand <- m[status == "ok" & !is.na(type)]
  cand[, stem := tolower(nfc(tools::file_path_sans_ext(file)))]
  cand <- cand[order(nchar(stem))]
  b <- list()
  for (i in seq_len(nrow(cand))) {
    r <- cand[i]; hit <- NA_integer_
    if (!r$type %in% c("em50", "campbell")) for (k in seq_along(b)) { x <- b[[k]]
      if (x$type == r$type && x$dir == r$dir && (is.na(x$serial) || is.na(r$serial) || x$serial == r$serial) &&
          (startsWith(r$stem, x$stem) || startsWith(x$stem, r$stem))) { hit <- k; break } }
    if (is.na(hit)) { b[[length(b) + 1L]] <- list(id = sprintf("B%03d", length(b) + 1L), type = r$type, dir = r$dir, serial = r$serial, stem = r$stem); hit <- length(b) }
    else if (is.na(b[[hit]]$serial)) b[[hit]]$serial <- r$serial
    m[path == r$path, bundle := b[[hit]]$id]
  }
  if (!length(b)) return(list(files = m[, use := FALSE], bundles = data.table()))                 # no file of a logger type (all empty or unknown)
  bt <- rbindlist(lapply(b, function(x) data.table(bundle = x$id, type = x$type, serial = x$serial, dir = x$dir)))
  m[!is.na(bundle) & is.na(serial), serial := bt$serial[match(bundle, bt$bundle)]]
  # the file each bundle is read from
  m[, prio := SOURCE_PRIORITY[format]]
  m[, source := FALSE]
  m[!is.na(bundle) & readable & !is.na(prio), source := { o <- order(prio, -as.numeric(size)); s <- rep(FALSE, .N); s[o[1]] <- TRUE; s }, by = bundle]
  bs <- m[!is.na(bundle), .(type = type[1], serial = serial[1], folder = basename(dir[1]), members = paste(file, collapse = " | "), n_files = .N,
                            source_file = if (any(source)) file[source][1] else NA_character_, source_format = if (any(source)) format[source][1] else NA_character_,
                            raw_file = if (any(role == "raw")) file[role == "raw"][1] else NA_character_,
                            exports = paste(file[role == "export" & !source], collapse = " | "),
                            unsupported = any(format %in% c("campbell_array", "campbell_binary"))), by = bundle]
  wb <- m[!is.na(bundle), .(w = any(water %in% TRUE)), by = bundle][w == TRUE, bundle]
  bs[, status := fifelse(bundle %in% wb, "water_logger", fifelse(!is.na(source_file), "ok", fifelse(unsupported, "unsupported", "needs_export")))]
  bs[, advice := fifelse(status == "ok", "", fifelse(status == "water_logger", WATER_LOGGER_NOTE, EXPORT_ADVICE[type]))]
  m[!is.na(bundle), use := source & !bundle %in% wb]
  m[is.na(use), use := FALSE]
  m[status == "ok" & (bundle %in% wb | (is.na(bundle) & water %in% TRUE)), status := "water_logger"]
  m[status == "ok" & !is.na(bundle) & !use, status := fifelse(role == "companion", "companion", fifelse(readable, fifelse(role == "raw", "raw_not_used", "export_not_used"), "proprietary_not_readable"))]
  m[status == "ok" & is.na(bundle), status := "no_logger_type"]
  m[, prio := NULL]
  list(files = m, bundles = bs[, !"unsupported"])
}
