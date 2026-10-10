# Ovid/metasheet.R  --  The metadata sheet: an optional table of what people know about what was measured (loggers,
# surveys, flights), with the columns each project chooses (config/metadata_fields.csv). GT and WLR keep it in the
# deployment sheet (config/deployment_sheet.csv, core deployments.R): its eight columns and the app's own, each optional;
# an older version's logger list (config/metadata.csv) is moved into it once (meta_fold()).
# Part of Ovid Core (shared). Sourced by each app's pipeline after config.R; the screens are in metasheet_ui.R.
#
# Nothing in it is required, and an app works without it (and without any metadata workbook). Each app lists the fields its
# pipeline can use as META_FIELDS (field, label, type, use, help, aliases ...); a project can relabel them, hide them, change
# the choices of a list, and add columns of its own, which are kept, shown and exported but not used. The key field (a
# serial, or a pattern of file paths) ties a row to what was read; a row without one is left aside.
#
#   field    the column's name in metadata.csv          label   the heading people see
#   type     text, number, integer, date, datetime, choice (one of `choices`, separated by |) or list (values separated by , or ;)
#   unit     shown beside the heading; a number typed with this unit after it ("45 cm") is read without it
#   use      what the app does with the field, in its words ("" for a column people added: kept, not used)
#   help     a longer note        aliases   other headings a spreadsheet may give it, separated by |, for imports
#   show     FALSE hides the column (its values are kept)
#   identifies  TRUE for the fields that identify a row; the first of them ties it to what was read (a row with it blank is
#            left aside), and together they say which row an import updates (WLR: serial and year)

META_FIELD_COLS <- c("field", "label", "type", "choices", "unit", "use", "help", "aliases", "show", "identifies")
META_TYPES <- c("text", "number", "integer", "date", "datetime", "choice", "list")

meta_keys <- function(spec) spec[meta_true(identifies), field]
meta_norm <- function(x) gsub("[^a-z0-9]", "", tolower(trim(as.character(x))))
meta_true <- function(x) !tolower(trim(as.character(x))) %in% c("false", "0", "no", "n")

# An app's fields as a full spec (every column of META_FIELD_COLS filled in).
meta_defaults <- function(fields) {
  f <- ensure_cols(copy(as.data.table(fields))[, lapply(.SD, as.character)], META_FIELD_COLS)
  f[!nzchar(label), label := field]; f[!nzchar(type), type := "text"]; f[!nzchar(show), show := "TRUE"]; f[!nzchar(identifies), identifies := "FALSE"]
  f[, META_FIELD_COLS, with = FALSE]
}

# The project's spec: the app's fields with what people changed (label, help, choices, unit, aliases, whether shown, and the
# type of a column they added), the app's new fields added at the end, and the columns people added kept. A field the app
# uses keeps the app's type and use, so the pipeline can always read it.
meta_spec <- function(app_fields, project_dir) {
  def <- meta_defaults(app_fields); p <- cfg_path_meta(project_dir, "fields")
  saved <- if (file.exists(p)) tryCatch(ensure_cols(read_cfg(p), META_FIELD_COLS)[, META_FIELD_COLS, with = FALSE], error = function(e) NULL) else NULL
  if (is.null(saved) || !nrow(saved)) return(def)
  saved <- saved[nzchar(field) & !duplicated(field) & (field %in% def$field | !nzchar(use))]     # a field the app no longer has (folded into another) goes
  out <- copy(saved)
  for (i in which(out$field %in% def$field)) { d <- def[field == out$field[i]]
    for (k in c("use", "identifies")) set(out, i, k, d[[k]])
    if (nzchar(d$use)) set(out, i, "type", d$type)
    for (k in c("label", "help", "aliases", "choices", "unit")) if (!nzchar(out[[k]][i])) set(out, i, k, d[[k]]) }
  out[!field %in% def$field, `:=`(use = "", identifies = "FALSE")]
  out[!type %in% META_TYPES, type := "text"]
  rbind(out, def[!field %in% out$field])
}
cfg_path_meta <- function(project_dir, what = c("sheet", "fields", "list"))
  file.path(path.expand(project_dir), "config", switch(match.arg(what), fields = "metadata_fields.csv", list = "metadata.csv", "deployment_sheet.csv"))
meta_write_spec <- function(spec, project_dir, who = "app") { p <- cfg_path_meta(project_dir, "fields")
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); backup_cfg(p, spec, who); fwrite_safe(spec[, META_FIELD_COLS, with = FALSE], p) }

# The sheet as written (every value text), with a column for every field of the spec. NULL-safe: no file, no rows.
meta_read <- function(project_dir, spec) {
  p <- cfg_path_meta(project_dir, "sheet")
  d <- if (file.exists(p)) tryCatch(read_cfg(p), error = function(e) { problem(basename(p), conditionMessage(e)); data.table() }) else data.table()
  if (ncol(d)) setnames(d, dep_names(names(d), spec$field))
  ensure_cols(d, spec$field)
}

# One value per cell read by its field's type. Returns list(values = typed table, problems = row, field, value, problem).
meta_values <- function(d, spec) {
  d <- copy(as.data.table(d)); probs <- list()
  bad <- function(f, i, why) probs[[length(probs) + 1L]] <<- data.table(row = i, field = f, value = d[[f]][i], problem = why)
  for (j in seq_len(nrow(spec))) { f <- spec$field[j]; if (!f %in% names(d)) next
    x <- trim(as.character(d[[f]])); x[is.na(x)] <- ""; has <- nzchar(x); ty <- spec$type[j]
    if (ty %in% c("number", "integer")) {
      u <- trim(spec$unit[j]); y <- if (nzchar(u)) trim(sub(paste0("\\s*", gsub("([.+*?^$()|\\[\\]\\\\])", "\\\\\\1", u), "$"), "", x, ignore.case = TRUE)) else x
      v <- suppressWarnings(as.numeric(gsub(",", ".", y, fixed = TRUE)))
      for (i in which(has & is.na(v))) bad(f, i, paste("not a number", if (nzchar(u)) paste0("(", u, ")") else ""))
      if (ty == "integer") { k <- which(has & !is.na(v) & v != round(v)); for (i in k) bad(f, i, "not a whole number"); v <- as.integer(round(v)) }
      set(d, j = f, value = v)
    } else if (ty %in% c("date", "datetime")) {
      v <- parse_date_cell(x)
      for (i in which(has & is.na(v))) bad(f, i, "not a date (2024-08-31, or 2024-08-31 14:00)")
      set(d, j = f, value = v)
    } else if (ty == "choice") {
      ch <- trim(strsplit(spec$choices[j], "|", fixed = TRUE)[[1]]); ch <- ch[nzchar(ch)]
      if (length(ch)) { m <- match(meta_norm(x), meta_norm(ch)); for (i in which(has & is.na(m))) bad(f, i, paste("not one of", paste(ch, collapse = ", ")))
        set(d, j = f, value = ifelse(is.na(m), x, ch[m])) } else set(d, j = f, value = x)
    } else if (ty == "list") {
      set(d, j = f, value = lapply(strsplit(x, "[,;]"), function(v) { v <- trim(v); v[nzchar(v)] }))
    } else set(d, j = f, value = x)
  }
  list(values = d, problems = rbindlist(probs, fill = TRUE) %||% data.table())
}

# Two cells that say the same: equal ignoring case, or the same date, or the same number ("2024-05-01" and "2024-05-01 00:00",
# "60.10" and "60.1").
meta_same <- function(a, b) { if (identical(tolower(trim(a)), tolower(trim(b)))) return(TRUE)
  na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b)); if (!is.na(na) && !is.na(nb)) return(isTRUE(all.equal(na, nb)))
  da <- suppressWarnings(as.Date(substr(a, 1, 10), optional = TRUE)); db <- suppressWarnings(as.Date(substr(b, 1, 10), optional = TRUE))
  !is.na(da) && !is.na(db) && da == db && nchar(trim(a)) <= 16 && nchar(trim(b)) <= 16 }

# ---- the logger list in the deployment sheet (GT, WLR) -----------------------------------------------------------------------
# The sheet's columns for an app: the eight of the deployment sheet (From, To and the position as text: the sheet reads a year,
# a month or degrees and minutes itself), then the app's fields it does not have. map: the app's fields that are one of the
# eight under another name (GT: start = from, end = to).
meta_sheet_fields <- function(app_fields, map = character()) {
  dep <- meta_defaults(DEPLOY_FIELDS); dep[field %in% c("from", "to", "latitude", "longitude"), type := "text"]
  rbind(dep, meta_defaults(app_fields)[!field %in% c(DEPLOY_COLS, names(map))])
}
# The sheet as an app reads it: a row of the app's list (app_fields) for each row of the sheet with a serial. deploy(row): the
# app's way of turning a sheet row (core deployments.R dep_sheet(): station, site, From, To ...) into rows of its list; the
# row's own columns (a name, a year, a hang length, depths) are then put over them, as typed: where From and To (say) give a
# value too and it differs, the typed one is used and the difference listed (attribute "conflicts": item, field, list (the
# typed value), sheet). Typed values with problems are reported (once per run) as notes in the audit, never as errors, and left
# blank. NULL when there is no sheet or no row with a serial.
meta_sheet <- function(project_dir, app_fields, report = TRUE, deploy = NULL, map = character()) {
  spec <- meta_spec(app_fields, project_dir); k0 <- meta_keys(spec)[1]
  own <- setdiff(spec$field, c(DEPLOY_COLS, names(map)))
  dr <- dep_sheet(project_dir, report, keep = own); if (is.null(dr) || is.na(k0)) return(NULL)
  txt <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trim(x) }
  conflicts <- NULL; rows <- vector("list", nrow(dr))
  for (i in seq_len(nrow(dr))) { x <- dr[i]
    b <- if (is.function(deploy)) as.data.table(deploy(x)) else data.table(serial = x$serial)
    if (!nrow(b)) next
    b <- ensure_cols(b[, lapply(.SD, txt)], spec$field)
    for (v in intersect(own, names(x))) { a <- txt(x[[v]]); if (!nzchar(a)) next
      w <- b[[v]][nzchar(b[[v]])]; if (length(w) && !all(vapply(w, meta_same, NA, b = a))) conflicts <- rbind(conflicts, data.table(item = x$serial, field = v, list = a, sheet = w[1]))
      set(b, j = v, value = a) }
    b[, .row := x$.row]; rows[[i]] <- unique(b) }
  d <- rbindlist(rows, fill = TRUE); if (!nrow(d)) return(NULL)
  if (report && !is.null(conflicts)) for (i in seq_len(min(nrow(conflicts), 50))) { z <- conflicts[i]
    problem("deployment sheet", paste0(z$item, ": From and To give ", z$field, " '", z$sheet, "', the ", z$field, " column '", z$list, "', which is used")) }
  d <- d[nzchar(trim(d[[k0]]))]; if (!nrow(d)) return(NULL)
  at <- d$.row; d[, .row := NULL]
  v <- meta_values(d, spec)
  if (report && nrow(v$problems)) for (i in seq_len(min(nrow(v$problems), 50))) { x <- v$problems[i]
    problem(paste("deployment_sheet.csv row", at[x$row] + 1L), paste0(spec[field == x$field, label], " '", x$value, "': ", x$problem, "; left blank")) }
  out <- v$values
  for (i in seq_len(nrow(v$problems))) { x <- v$problems[i]; if (!is.list(out[[x$field]])) set(out, x$row, x$field, if (is.character(out[[x$field]])) "" else NA) }
  setattr(out, "spec", spec); setattr(out, "conflicts", conflicts); setattr(out, "sheet_row", at); out[]
}
# The problems of the typed values, by the sheet's row (row, field, value, problem), for the set-up check.
meta_sheet_problems <- function(project_dir, app_fields, deploy = NULL, map = character()) {
  ms <- meta_sheet(project_dir, app_fields, report = FALSE, deploy = deploy, map = map)
  if (is.null(ms)) return(data.table(row = integer(), field = character(), value = character(), problem = character()))
  spec <- attr(ms, "spec"); dr <- dep_sheet(project_dir, FALSE, keep = setdiff(spec$field, c(DEPLOY_COLS, names(map))))
  own <- intersect(setdiff(spec$field, c(DEPLOY_COLS, names(map))), names(dr)); if (!length(own)) return(data.table(row = integer(), field = character(), value = character(), problem = character()))
  p <- meta_values(dr[, own, with = FALSE], spec[field %in% own])$problems
  if (nrow(p)) p[, row := dr$.row[row]]; p
}

# An older version's logger list (config/metadata.csv) into the sheet, once (core config.R CONFIG_FOLDS). A logger with one row
# in each, where the sheet's row stood for one row of the list, becomes one row: the list's cells added, and where both gave a
# value and they differ, the list's (typed in the app, and used until now) kept and the difference listed. A logger the list had
# in other ways (several rows) keeps the list's rows, which were the ones used: its sheet rows are replaced and listed (the sheet
# before is in config/history). The rest is added. Every column of the list is kept.
meta_fold <- function(app_fields, deploy = NULL, map = character()) function(old, cur) {
  spec <- meta_defaults(app_fields); k0 <- meta_keys(spec)[1]; own <- setdiff(spec$field, c(DEPLOY_COLS, names(map)))
  L <- copy(as.data.table(old)); hit <- intersect(names(map), names(L)); if (length(hit)) setnames(L, hit, map[hit])
  S <- if (is.null(cur) || !ncol(cur)) empty_cfg(DEPLOY_COLS) else { x <- copy(as.data.table(cur)); setnames(x, dep_names(names(x), own)); x }
  cols <- unique(c(DEPLOY_COLS, names(S), names(L))); L <- ensure_cols(L, cols)[, cols, with = FALSE]; S <- ensure_cols(S, cols)[, cols, with = FALSE]
  kL <- meta_norm(L[[k0]]); kS <- meta_norm(S$serial); add <- rep(TRUE, nrow(L)); drop <- integer(); log <- list()
  say <- function(sn, what) log[[length(log) + 1L]] <<- data.table(where = "logger list and deployment sheet", problem = paste0(sn, ": ", what))
  base <- function(r) { cl <- dep_clean(r)$rows; b <- if (is.function(deploy)) as.data.table(deploy(cl)) else data.table(serial = cl$serial)
    for (v in names(map)) if (v %in% names(b)) setnames(b, v, map[[v]]); b }
  for (sn in unique(kL[nzchar(kL)])) { iL <- which(kL == sn); iS <- which(kS == sn); if (!length(iS)) next
    b <- if (length(iL) == 1 && length(iS) == 1) base(S[iS]) else NULL
    if (is.null(b) || nrow(b) != 1L) { drop <- c(drop, iS)
      say(L[[k0]][iL[1]], paste0("the logger list's row", if (length(iL) > 1) "s were" else " was", " used, not the deployment sheet's (", paste(S$station[iS], S$from[iS], S$to[iS], collapse = "; "), "): the sheet now holds the list's")); next }
    add[iL] <- FALSE; l <- L[iL]
    for (v in setdiff(names(L), "serial")) { a <- trim(l[[v]]); if (!nzchar(a)) next
      was <- if (v == "site") dep_where(S[iS]) else if (v %in% names(b)) trim(as.character(b[[v]])) else trim(S[[v]][iS]); was[is.na(was)] <- ""
      if (nzchar(was) && meta_same(a, was)) next
      if (nzchar(was)) say(l[[k0]], paste0("the deployment sheet gave ", v, " '", was, "', the logger list '", a, "'; the logger list's is kept"))
      set(S, iS, if (v == "site" && nzchar(S$station[iS])) "station" else v, a) } }
  out <- rbind(if (length(drop)) S[-drop] else S, L[add])
  list(rows = out, log = if (length(log)) rbindlist(log) else NULL)
}

# ---- import ------------------------------------------------------------------------------------------------------------
# Reading a spreadsheet, matching its columns and checking the rows are in import.R (sourced after this file); here the rows are
# made and merged.
# Rows for metadata.csv from an import: map is the field for each column of x ("" leaves it out; "+label" adds a column).
# Returns list(rows, spec) with any new columns added to the spec.
meta_from_import <- function(x, map, spec) {
  spec <- copy(spec); out <- data.table(.n = seq_len(nrow(x)))
  mm <- as.character(map); mm <- mm[!is.na(mm) & nzchar(mm) & !startsWith(mm, "+")]
  if (anyDuplicated(mm)) stop("Two columns are matched to the same field (", paste(unique(mm[duplicated(mm)]), collapse = ", "), "); each field can come from one column only.", call. = FALSE)
  for (i in seq_along(map)) { m <- map[i]; if (is.na(m) || !nzchar(m)) next
    if (startsWith(m, "+")) { lab <- trim(substring(m, 2)); if (!nzchar(lab)) lab <- names(x)[i]
      f <- safe_name(tolower(gsub("[^A-Za-z0-9]+", "_", lab))); f <- sub("^_+|_+$", "", f); if (!nzchar(f)) f <- paste0("col", i)
      while (f %in% spec$field) f <- paste0(f, "_2")
      spec <- rbind(spec, meta_defaults(data.table(field = f, label = lab, use = "")))
      m <- f }
    set(out, j = m, value = x[[i]]) }
  out[, .n := NULL]
  list(rows = ensure_cols(out, spec$field)[, spec$field, with = FALSE], spec = spec)
}
# The sheet with new rows: "add" appends them, "update" replaces the rows with the same key (and appends the rest), "replace"
# keeps only the new rows. Cells left blank in a new row do not blank a filled cell of the row it updates; a new row with a year
# (say) updates the row for the same serial that has no year yet.
meta_merge <- function(old, new, spec, mode = c("add", "update", "replace")) {
  mode <- match.arg(mode); keys <- meta_keys(spec)
  cols <- unique(c(names(old), spec$field)); old <- ensure_cols(copy(as.data.table(old)), cols); new <- ensure_cols(copy(as.data.table(new)), cols)
  if (mode == "replace") return(new[, cols, with = FALSE])
  if (mode == "add" || !length(keys)) return(rbind(old[, cols, with = FALSE], new[, cols, with = FALSE]))
  id <- function(d) do.call(paste, c(lapply(keys, function(k) meta_norm(d[[k]])), sep = "\r"))
  for (i in seq_len(nrow(new))) { k <- id(new[i]); hit <- which(nzchar(meta_norm(new[[keys[1]]][i])) & id(old) == k)
    if (!length(hit) && length(keys) > 1) {                 # else the row for the same first key whose other keys are still blank
      hit <- which(meta_norm(old[[keys[1]]]) == meta_norm(new[[keys[1]]][i]) & Reduce(`&`, lapply(keys[-1], function(k2) !nzchar(trim(old[[k2]])))))[1]
      if (is.na(hit)) hit <- integer() }
    if (!length(hit)) { old <- rbind(old, new[i, cols, with = FALSE]); next }
    for (v in cols) if (nzchar(new[[v]][i])) set(old, hit, v, new[[v]][i]) }
  old[, cols, with = FALSE]
}
