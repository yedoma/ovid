# R/history.R  --  Program-change history and station/logger lineage.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# One row per stretch of a logger table with the same program and the same columns.
program_history <- function(man, reg = NULL) {
  m <- man[status == "ok" & !is.na(first_ts)]; if (!nrow(m)) return(data.table())
  m <- m[order(logger_id, table, first_ts)]
  cols <- lapply(m$columns, function(x) setdiff(x, c("TIMESTAMP", "RECORD", "")))
  m[, `:=`(prog = fifelse(is.na(program), "", sub("^CPU:", "", program)), sig = vapply(cols, function(x) paste(sort(x), collapse = "\r"), ""), .i = seq_len(.N))]
  m[, seg := cumsum(c(TRUE, prog[-1] != prog[-.N] | sig[-1] != sig[-.N])), by = .(logger_id, table)]
  out <- m[, .(from = min(first_ts), to = max(last_ts, na.rm = TRUE), program = prog[1], os = paste(unique(os[!is.na(os) & nzchar(os)]), collapse = " | "),
               model = paste(unique(model[!is.na(model)]), collapse = " | "), files = .N, i = .i[1], first_file = basename(path[1])), by = .(logger_id, table, seg)]
  out[, n_columns := lengths(cols[i])]
  prev <- function(k) if (out$seg[k] == 1) NULL else cols[[out$i[k - 1]]]
  out[, `:=`(columns_added = vapply(seq_len(.N), function(k) { p <- prev(k); if (is.null(p)) "" else paste(setdiff(cols[[i[k]]], p), collapse = ", ") }, ""),
             columns_removed = vapply(seq_len(.N), function(k) { p <- prev(k); if (is.null(p)) "" else paste(setdiff(p, cols[[i[k]]]), collapse = ", ") }, ""))]
  out[, change := { pp <- shift(program); fifelse(seg == 1, "first seen", fifelse(program != pp & (nzchar(columns_added) | nzchar(columns_removed)), "program and columns changed",
                    fifelse(program != pp, "program changed", "columns changed"))) }, by = .(logger_id, table)]
  if (!is.null(reg) && nrow(reg)) out <- merge(out, unique(reg[, .(logger_id, table, station)], by = c("logger_id", "table")), by = c("logger_id", "table"), all.x = TRUE, sort = FALSE)
  else out[, station := NA_character_]
  out[order(logger_id, table, from), .(station, logger_id, table, model, from, to, change, program, os, n_columns, columns_added, columns_removed, files, first_file)]
}

# One row per logger at a station, in time order, with a plain note: first logger, replaced X, ran alongside X, came from station Y.
station_lineage <- function(lin, reg = NULL) {
  if (!nrow(lin)) return(data.table())
  L <- lin[station != "", .(first = min(first), last = max(last), records = sum(records), tables = paste(sort(unique(stream)), collapse = ", ")), by = .(station, logger_id)]
  if (!is.null(reg) && nrow(reg)) { r <- reg[, .(model = paste(unique(model[!is.na(model) & nzchar(model)]), collapse = " | "), serial = paste(unique(serial[!is.na(serial) & nzchar(serial)]), collapse = " | ")), by = logger_id]
    L <- merge(L, r, by = "logger_id", all.x = TRUE, sort = FALSE) } else L[, `:=`(model = "", serial = "")]
  setorder(L, station, first); nm <- function(m, id) trimws(paste(ifelse(is.na(m), "", m), id))
  L[, note := { n <- .N; out <- character(n)
    for (k in seq_len(n)) out[k] <- if (k == 1) "first logger at this station" else {
      over <- as.numeric(difftime(pmin(last[k], max(last[seq_len(k - 1)])), first[k], units = "days")); j <- which.max(last[seq_len(k - 1)])
      if (over > 30) paste0("ran alongside ", nm(model[j], logger_id[j])) else paste0("replaced ", nm(model[j], logger_id[j])) }
    out }, by = station]
  mv <- L[order(logger_id, first)][, .(station, from_station = shift(station), first), by = logger_id][!is.na(from_station) & from_station != station]
  if (nrow(mv)) L[mv, on = .(logger_id, station), note := paste0(note, "; this logger was earlier at ", i.from_station)]
  L[, .(station, logger_id, model, serial, first, last, days = round(as.numeric(difftime(last, first, units = "days")), 1), records, tables, note)]
}

