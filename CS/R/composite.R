# R/composite.R  --  Combined records: one parameter stitched from several stations (config/composites.csv).
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.
#
# One row per source of a combined record:
#   composite  the record's name, e.g. air_temp_old_camp
#   parameter  the parameter to take (the common name, as on Plot and explore)
#   station, stream   where to take it from (stream blank = every table of the station, joined as load_station_all does)
#   start, end the stretch this source may supply (blank = any time; an end date alone covers the whole day)
#   priority   1 is preferred; where two sources both have a value at a time, the lower number wins
#   offset     added to this source's values (blank = 0), to line a source up with the others where a sensor reads high or low
#   step       blank = every record, or hour / day for means (totals such as rain are summed); read from the first row
#   notes      free text
# Every value in the output keeps the station and stream it came from, so the record says where each stretch came from.

COMPOSITE_COLS <- c("composite", "parameter", "station", "stream", "start", "end", "priority", "offset", "step", "notes")
COMPOSITE_STEPS <- c("", "hour", "day")

composite_source <- function(r, project_dir) {
  v <- r$parameter
  d <- tryCatch(if (nzchar(r$stream)) load_station(r$station, r$stream, project_dir, columns = v) else load_station_all(r$station, project_dir, columns = v),
                error = function(e) { problem(paste("combined record", r$composite), paste0(r$station, if (nzchar(r$stream)) paste0(" / ", r$stream), ": ", conditionMessage(e))); NULL })
  if (is.null(d) || !nrow(d)) return(NULL)
  if (!v %in% names(d)) { problem(paste("combined record", r$composite), paste0(r$station, if (nzchar(r$stream)) paste0(" / ", r$stream), " has no parameter '", v, "'")); return(NULL) }
  s <- parse_time(r$start); e <- parse_time(r$end)
  if (!is.na(e) && nchar(r$end) <= 10) e <- e + 86399
  d <- d[!is.na(get(v)) & (is.na(s) | TIMESTAMP >= s) & (is.na(e) | TIMESTAMP <= e)]
  if (!nrow(d)) return(NULL)
  off <- suppressWarnings(as.numeric(r$offset)); if (!length(off) || is.na(off)) off <- 0
  data.table(TIMESTAMP = d$TIMESTAMP, value = as.numeric(d[[v]]) + off, station = r$station,
             stream = if ("stream" %in% names(d)) as.character(d$stream) else r$stream, priority = r$priority)
}

# Means over hours or days, per source, before the sources are combined (so a 30-minute source and an hourly one meet on one grid).
composite_step <- function(d, step, parameter) {
  if (!nzchar(step) || !nrow(d)) return(d)
  w <- if (step == "hour") 3600 else 86400
  total <- grepl(TOTAL_RX, parameter, ignore.case = TRUE)
  d[, TIMESTAMP := as.POSIXct(floor(as.numeric(TIMESTAMP) / w) * w, origin = "1970-01-01", tz = "UTC")]
  d[, .(value = if (total) sum(value) else mean(value), n = .N), by = .(TIMESTAMP, station, stream, priority)][, n := NULL][]
}

# Build one combined record from its rows of composites.csv. Returns the record (TIMESTAMP, value, station, stream)
# and a summary of what each source supplied.
build_composite <- function(rows, project_dir = CFG$project_dir) {
  rows <- ensure_cols(as.data.table(rows), COMPOSITE_COLS)
  rows[, priority := suppressWarnings(as.numeric(priority))][is.na(priority), priority := seq_len(.N)[is.na(priority)] + 1000]
  step <- tolower(trim(rows$step[1])); if (!step %in% COMPOSITE_STEPS) step <- ""
  par <- rows$parameter[nzchar(rows$parameter)][1]; if (is.na(par)) stop("No parameter given.")
  rows[!nzchar(parameter), parameter := par]
  src <- rbindlist(lapply(seq_len(nrow(rows)), function(i) composite_step(composite_source(rows[i], project_dir), step, rows$parameter[i])), fill = TRUE)
  empty <- data.table(TIMESTAMP = as.POSIXct(character(), tz = "UTC"), value = numeric(), station = character(), stream = character())
  if (!nrow(src)) return(list(record = empty, sources = data.table(), step = step, parameter = par))
  setorder(src, TIMESTAMP, priority)
  rec <- src[, .SD[1], by = TIMESTAMP]
  used <- rec[, .(records = .N, first = min(TIMESTAMP), last = max(TIMESTAMP)), by = .(station, stream, priority)]
  offered <- src[, .(offered = .N), by = .(station, stream, priority)]
  # where a source overlaps the one preferred to it: the median difference (this source minus the record), a guide for its offset
  ov <- merge(src, rec[, .(TIMESTAMP, chosen = value, chosen_priority = priority)], by = "TIMESTAMP")[priority != chosen_priority]
  if (nrow(ov)) offered <- merge(offered, ov[, .(overlap_records = .N, median_difference = signif(stats::median(value - chosen), 3)), by = .(station, stream, priority)],
                                 by = c("station", "stream", "priority"), all.x = TRUE)
  sm <- merge(offered, used, by = c("station", "stream", "priority"), all.x = TRUE)
  sm[is.na(records), records := 0L][, not_used := offered - records]
  setorder(sm, priority, first)
  list(record = rec[, .(TIMESTAMP, value, station, stream)], sources = sm, step = step, parameter = par)
}

# Every combined record in composites.csv, written to output/composites/<name>.csv (TIMESTAMP, <parameter>, station, stream)
# with a summary in output/audit/composites.csv.
write_composites <- function(comp, project_dir = CFG$project_dir) {
  out <- file.path(path.expand(project_dir), "output", "composites"); adir <- file.path(path.expand(project_dir), "output", "audit")
  comp <- if (is.null(comp)) data.table() else ensure_cols(as.data.table(comp), COMPOSITE_COLS)[nzchar(composite) & nzchar(station)]
  old <- list.files(out, "\\.csv$", full.names = TRUE)
  if (!nrow(comp)) { unlink(old); fwrite(data.table(composite = character()), file.path(adir, "composites.csv")); return(invisible(0L)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  made <- character(); summ <- list()
  for (nm in unique(comp$composite)) {
    b <- tryCatch(build_composite(comp[composite == nm], project_dir), error = function(e) { problem(paste("combined record", nm), conditionMessage(e)); NULL })
    if (is.null(b)) next
    f <- file.path(out, paste0(safe_name(nm), ".csv")); rec <- copy(b$record); setnames(rec, "value", b$parameter)
    fwrite(rec, f, dateTimeAs = "write.csv"); made <- c(made, f)
    if (nrow(b$sources)) summ[[nm]] <- cbind(data.table(composite = nm, parameter = b$parameter, step = b$step), b$sources)
  }
  unlink(setdiff(old, made))
  fwrite(rbindlist(summ, fill = TRUE), file.path(adir, "composites.csv"), dateTimeAs = "write.csv")
  invisible(length(made))
}
