# R/io.R  --  Reading and writing station tables after a run.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# ---- helpers for analysis after a run ---------------------------------------
has_arrow <- function() requireNamespace("nanoparquet", quietly = TRUE)   # small Parquet reader/writer, no system libraries

write_station <- function(dt, base, format = "csv") {
  format <- tolower(format %||% "csv")
  if (format %in% c("parquet", "both") && !has_arrow()) {
    if (is.null(AUDIT$arrow_warned)) { AUDIT$arrow_warned <- TRUE
      problem("station files", "Parquet was asked for but the 'nanoparquet' package is not installed; CSV was written instead. Run install.packages(\"nanoparquet\").") }
    format <- "csv"
  }
  if (format %in% c("parquet", "both")) nanoparquet::write_parquet(as.data.frame(dt), paste0(base, ".parquet"))
  if (format %in% c("csv", "both")) fwrite(dt, paste0(base, ".csv"), dateTimeAs = "write.csv")
  invisible(format)
}

# Path of a station file: the Parquet copy when there is one, else the CSV.
station_file <- function(station, stream = NULL, project_dir = CFG$project_dir) {
  d <- file.path(path.expand(project_dir), "output", "stations")
  f <- list.files(d, full.names = TRUE, pattern = paste0("^", safe_name(station), "__", if (is.null(stream)) ".*" else safe_name(stream), "\\.(csv|parquet)$"))
  if (!length(f)) stop("No station file for '", station, "'. See output/stations/.")
  keys <- unique(tools::file_path_sans_ext(basename(f)))
  if (length(keys) > 1) stop("'", station, "' has several streams, pick one: ", paste(sub("^.*__", "", keys), collapse = ", "))
  pq <- f[grepl("\\.parquet$", f)]
  if (length(pq) && has_arrow()) pq[1] else { cs <- f[grepl("\\.csv$", f)]
    if (!length(cs)) stop("This station is stored as Parquet; run install.packages(\"nanoparquet\") to read it.") else cs[1] }
}

# Clean record of one station. columns = NULL reads everything; naming columns
# reads only those (plus TIMESTAMP), which is much faster from Parquet.
load_station <- function(station, stream = NULL, project_dir = CFG$project_dir, columns = NULL) {
  f <- station_file(station, stream, project_dir)
  if (!is.null(columns)) columns <- unique(c("TIMESTAMP", columns))
  if (grepl("\\.parquet$", f)) {
    dt <- if (is.null(columns)) nanoparquet::read_parquet(f)
          else nanoparquet::read_parquet(f, col_select = intersect(columns, nanoparquet::read_parquet_schema(f)$name))
    dt <- as.data.table(as.data.frame(dt))
  } else {
    dt <- if (is.null(columns)) fread(f, colClasses = list(character = c("station", "stream", "logger_id", "source_file")))
          else { have <- names(fread(f, nrows = 0)); fread(f, select = intersect(columns, have)) }
  }
  if (!inherits(dt$TIMESTAMP, "POSIXct")) dt[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]
  attr(dt$TIMESTAMP, "tzone") <- "UTC"
  dt[]
}

# The logger tables that make up a station.
station_streams <- function(station, project_dir = CFG$project_dir) {
  f <- file.path(path.expand(project_dir), "output", "audit", "station_summary.csv")
  if (file.exists(f)) { s <- fread(f, colClasses = list(character = c("station", "stream"))); st <- station; return(s[station == st, stream]) }
  f <- list.files(file.path(path.expand(project_dir), "output", "stations"), pattern = paste0("^", safe_name(station), "__.*\\.(csv|parquet)$"))
  unique(sub("^.*?__", "", tools::file_path_sans_ext(f), perl = TRUE))
}

# ONE continuous record per parameter for a station, whatever logger tables it came from. Every table of the station
# is read; where the same parameter is recorded by more than one table on the same day, the table with the shorter
# interval is used for that day. Each row keeps its source_file and the table (stream) it came from.
load_station_all <- function(station, project_dir = CFG$project_dir, columns = NULL) {
  sm <- station_streams(station, project_dir); if (!length(sm)) stop("No station file for '", station, "'.")
  parts <- lapply(sm, function(x) try_or(load_station(station, x, project_dir, columns = if (is.null(columns)) NULL else unique(c(columns, "source_file", "logger_id"))),
                                       NULL, paste0(station, ", table ", x, " left out")))
  ok <- !vapply(parts, is.null, logical(1)); parts <- parts[ok]; sm <- sm[ok]; if (!length(parts)) stop("No station file for '", station, "' could be read.")
  step <- vapply(parts, function(d) if (nrow(d) > 1) stats::median(diff(as.numeric(d$TIMESTAMP))) else Inf, 0)
  for (i in seq_along(parts)) parts[[i]][, `:=`(station = station, stream = sm[i])]
  info <- data.table(stream = sm, step_seconds = step, records = vapply(parts, nrow, 0L))
  if (length(parts) == 1) { d <- parts[[1]]; setattr(d, "tables", info); return(d[]) }
  # a column that is numeric in one table and text in another stays numeric
  allc <- unique(unlist(lapply(parts, names)))
  for (v in setdiff(allc, META_COLS)) { cl <- vapply(parts, function(d) if (v %in% names(d)) class(d[[v]])[1] else NA_character_, "")
    if (any(cl %in% c("numeric", "integer"), na.rm = TRUE) && any(cl == "character", na.rm = TRUE))
      for (i in which(cl == "character")) set(parts[[i]], j = v, value = suppressWarnings(as.numeric(parts[[i]][[v]]))) }
  L <- rbindlist(parts, use.names = TRUE, fill = TRUE); day <- as.integer(as.Date(L$TIMESTAMP)); ord <- sm[order(step)]
  for (v in setdiff(names(L), META_COLS)) { has <- !is.na(L[[v]]); st <- unique(L$stream[has]); if (length(st) < 2) next
    taken <- integer()
    for (x in ord[ord %in% st]) { idx <- which(has & L$stream == x)
      if (length(taken)) { drop <- idx[day[idx] %in% taken]; if (length(drop)) { set(L, drop, v, NA); idx <- setdiff(idx, drop) } }
      taken <- c(taken, unique(day[idx])) } }
  L[, .o := match(stream, ord)]; setorder(L, TIMESTAMP, .o); L[, .o := NULL]
  setattr(L, "tables", info); L[]
}

# Which raw files a selection came from: one row per file with the first and last record used.
files_used <- function(dt, params = NULL) {
  if (!"source_file" %in% names(dt) || !nrow(dt)) return(data.table())
  keep <- if (is.null(params) || !length(params)) rep(TRUE, nrow(dt)) else rowSums(!is.na(dt[, intersect(params, names(dt)), with = FALSE])) > 0
  d <- dt[keep]; if (!nrow(d)) return(data.table())
  d[, .(Logger = paste(unique(logger_id), collapse = ", "), Table = paste(unique(stream), collapse = ", "), `First record used` = fmt_ts(min(TIMESTAMP)), `Last record used` = fmt_ts(max(TIMESTAMP)),
        `Records used` = .N, Folder = dirname(source_file[1])), by = .(File = basename(source_file))][order(`First record used`)]
}

# Audit detail. With no arguments, every entry (can be large); or name the
# detail files to read, as listed in the detail_file column of audit_index.csv.
load_audit <- function(project_dir = CFG$project_dir, detail_files = NULL) {
  d <- file.path(path.expand(project_dir), "output", "audit", "detail")
  f <- if (is.null(detail_files)) list.files(d, full.names = TRUE) else file.path(d, detail_files)
  rbindlist(lapply(f[file.exists(f)], fread, colClasses = "character", na.strings = ""), fill = TRUE)
}

