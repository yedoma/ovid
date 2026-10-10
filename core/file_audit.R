# Ovid/file_audit.R  --  Every raw file accounted for: what was read from it, what reached the output, and why the rest did not.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# The audit trail (audit.R) records each removal with the file it came from. This joins that with what each app read
# from each file and what each file contributed to the output, one row per file:
#
#   read  =  in_output  +  dropped        (reconciles is TRUE when that holds)
#
# "read" and "in_output" are counted in the app's own unit (records for CS and WLR, single values for GT). "dropped"
# counts row-level removals (whole records or values) by rule; values blanked inside a kept record are cell-level and
# shown apart as values_removed, since the record itself still reaches the output.

# Everything audit_flush() has gathered by source file, as one table.
audit_by_file <- function() {
  if (length(AUDIT$by_file)) rbindlist(AUDIT$by_file, fill = TRUE)
  else data.table(source_file = character(), stage = character(), rule = character(), level = character(), n = integer(), entries = integer())
}

# files:    one row per raw file (or per part of a file, e.g. one array of a CR10X file), with columns
#           file (matches source_file in the audit and in `kept`), used (TRUE when the file was read) and read (count);
#           any other columns (md5, size, status, ...) are carried through.
# kept:     source_file, in_output and optionally used_in (where its readings went); several rows per file are summed.
# by_file:  audit_by_file(), or the same columns.
# skip_stages: audit stages whose rows were never counted in `read` (e.g. lines of a file that could not be parsed).
# unit:     word used in what_happened ("records", "readings", "values").
# row_levels: audit levels that remove a whole record or value (GT's long records log "value" and "values").
file_reconcile <- function(files, kept, by_file = audit_by_file(), skip_stages = character(), unit = "records",
                           row_levels = unique(c("row", AUDIT_SCHEMA$level_each, AUDIT_SCHEMA$level_summary))) {
  fa <- copy(as.data.table(files))
  if (!"used" %in% names(fa)) fa[, used := !is.na(read)]
  if (!"status" %in% names(fa)) fa[, status := "not read"]
  used <- if (nrow(kept)) kept[, .(in_output = as.integer(sum(in_output)),
                                   used_in = if ("used_in" %in% names(kept)) paste(unique(used_in[!is.na(used_in) & nzchar(used_in)]), collapse = " | ") else NA_character_), by = source_file]
          else data.table(source_file = character(), in_output = integer(), used_in = character())
  bf <- if (nrow(by_file)) by_file[!is.na(source_file)] else by_file
  if (length(skip_stages) && "stage" %in% names(bf)) bf <- bf[!stage %in% skip_stages]
  rows <- if (nrow(bf) && any(bf$level %in% row_levels)) bf[level %in% row_levels, .(n = sum(n, na.rm = TRUE)), by = .(source_file, rule)][n > 0][order(source_file, -n)][
    , .(dropped = as.integer(sum(n)), dropped_why = paste(sprintf("%s %s", num(n), rule), collapse = "; ")), by = source_file]
          else data.table(source_file = character(), dropped = integer(), dropped_why = character())
  cells <- if (nrow(bf) && any(bf$level == "cell")) bf[level == "cell", .(values_removed = as.integer(sum(n, na.rm = TRUE))), by = source_file]
           else data.table(source_file = character(), values_removed = integer())
  for (x in list(used, rows, cells)) fa <- merge(fa, x, by.x = "file", by.y = "source_file", all.x = TRUE, sort = FALSE)
  for (v in c("in_output", "dropped", "values_removed")) set(fa, which(is.na(fa[[v]]) & fa$used %in% TRUE), v, 0L)
  fa[is.na(dropped_why) & used %in% TRUE, dropped_why := ""]
  fa[, reconciles := ifelse(used %in% TRUE, read == in_output + dropped, NA)]
  fa[, what_happened := fifelse(!used %in% TRUE, as.character(status),
        fifelse(read == 0L, paste("no", unit),
        fifelse(in_output == read, sprintf("all %s %s in the output", num(read), unit),
                sprintf("%s of %s %s in the output", num(in_output), num(read), unit))))]
  fa[]
}

# Adds the per-file rows up to a group (a download bundle, a logger), keeping read = in_output + dropped.
file_reconcile_by <- function(fa, by) {
  fa[used %in% TRUE, .(files = .N, read = sum(read), in_output = sum(in_output), dropped = sum(dropped), values_removed = sum(values_removed),
                       reconciles = all(reconciles)), by = by][]
}

# Raises one problem when any file does not add up, and returns how many.
file_reconcile_check <- function(fa, unit = "records") {
  n <- sum(fa$reconciles == FALSE, na.rm = TRUE)
  if (n) problem("file audit", paste(n, "file(s) whose", unit, "do not add up (read is not in output + dropped); see the Audit trail, Raw files"))
  # a file that was read but adds nothing to the output, unless all it had is also in another download (a later, longer one)
  if (all(c("used", "in_output", "read") %in% names(fa))) {
    only_copies <- vapply(strsplit(ifelse(is.na(fa$dropped_why), "", fa$dropped_why), "; ", fixed = TRUE), function(r) length(r) && all(grepl("duplicate|overlap|same reading|copy", r)), NA)
    z <- fa[used %in% TRUE & in_output %in% 0L & !(read > 0 & only_copies)]
    if (nrow(z)) problem("file audit", paste0(nrow(z), " file(s) were read but add nothing to the output (no ", unit, ", or every one left out): ",
                                              paste(utils::head(basename(z$file), 5), collapse = ", "), if (nrow(z) > 5) ", ..." else "", ". See the Audit trail, Raw files"))
  }
  invisible(n)
}
