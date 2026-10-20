# Ovid/config.R  --  Set-up tables: reading, writing with a backup of every change, and import / export of a whole
# configuration. Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Everything the user decides lives in <project>/config/*.csv. Each app lists its tables in CONFIG_FILES:
#   name = list(file = "x.csv", cols = <character vector, or a function returning one>, label = "...", default = <function>)
# The tables themselves, their columns and defaults stay in the app.

# A table that cannot be a CSV (NUL bytes: a save cut short, or another file saved over it) or that fread cannot read stops
# with a message that names it and the last good copy in config/history (condition class "ovid_bad_table").
read_cfg <- function(path) {
  bad <- function(why) { h <- cfg_last_backup(path)
    msg <- paste0("The set-up table ", basename(path), " is damaged (", why, ")",
                  if (length(h)) paste0(". The last good copy is config/history/", h, ": restore it from the table's History, or copy it over ", basename(path)) else "", ".")
    stop(structure(class = c("ovid_bad_table", "error", "condition"), list(message = msg, call = NULL))) }
  sz <- file.size(path)
  if (!is.na(sz) && sz > 0 && any(readBin(path, "raw", min(sz, 65536)) == as.raw(0))) bad("it holds binary data, not text")
  dt <- tryCatch(fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8"), error = function(e) bad(conditionMessage(e)))
  for (v in names(dt)) set(dt, j = v, value = trim(fifelse(is.na(dt[[v]]), "", dt[[v]])))
  dt
}
# The newest copy of a table in config/history (file name), or character() when there is none.
cfg_last_backup <- function(path) {
  b <- list.files(file.path(dirname(path), "history"), pattern = paste0("^", rx_escape(tools::file_path_sans_ext(basename(path))), "__[0-9_]+\\.csv$"))
  if (length(b)) sort(b, decreasing = TRUE)[1] else character()
}
# A CSV written beside the target and renamed into place, so a save cut short never leaves half a file.
fwrite_safe <- function(dt, path, ...) {
  tmp <- paste0(path, ".tmp"); fwrite(dt, tmp, ...)
  if (!file.rename(tmp, path)) { ok <- file.copy(tmp, path, overwrite = TRUE); unlink(tmp); if (!ok) stop("could not write ", path, " (is it open in another program?)") }
  invisible(path)
}

ensure_cols <- function(dt, cols) {
  for (v in setdiff(cols, names(dt))) dt[, (v) := ""]
  for (v in names(dt)) set(dt, which(is.na(dt[[v]])), v, "")
  dt
}

empty_cfg <- function(cols) as.data.table(setNames(replicate(length(cols), character(), simplify = FALSE), cols))

cfg_cols <- function(what) { c <- CONFIG_FILES[[what]]$cols; if (is.function(c)) c() else c }

cfg_path <- function(project_dir, what) file.path(path.expand(project_dir), "config", CONFIG_FILES[[what]]$file)

# Reads every set-up table in CONFIG_FILES, creating any that are missing (with defaults where there are some).
load_configs <- function(cfg) {
  dir.create(file.path(path.expand(cfg$project_dir), "config"), recursive = TRUE, showWarnings = FALSE)
  config_migrate(cfg$project_dir, "pipeline")                       # tables folded into others, moved in once (below)
  out <- list()
  for (w in names(CONFIG_FILES)) {
    f <- cfg_path(cfg$project_dir, w); cols <- cfg_cols(w)
    if (!file.exists(f)) fwrite(if (!is.null(CONFIG_FILES[[w]]$default)) CONFIG_FILES[[w]]$default() else empty_cfg(cols), f)
    d <- tryCatch(read_cfg(f), error = function(e) { problem(basename(f), conditionMessage(e)); empty_cfg(cols) })
    out[[w]] <- ensure_cols(d, cols)
    if (!is.null(CONFIG_FILES[[w]]$rule_ids)) out[[w]] <- qc_read_rules(f, cols, CONFIG_FILES[[w]]$rule_ids)   # a rule table: every row keeps a rule_id (core/qc.R)
  }
  out
}

write_cfg <- function(dt, project_dir, what, who = "pipeline") {
  p <- cfg_path(project_dir, what)
  backup_cfg(p, dt, who)
  fwrite_safe(dt, p)
}

# Every change to a set-up table keeps the previous version in config/history and a line in change_log.csv.
# `new` is the table about to be written (then nothing is kept if it is unchanged, and the rows added and removed are
# counted), or just its number of rows. Returns the name of the backup, or NULL when there was nothing to keep.
HIST_COLS <- c("time", "user", "table", "rows_before", "rows_after", "rows_added_or_changed", "rows_removed_or_changed", "backup")
backup_cfg <- function(path, new = NULL, who = "", what = "") {
  if (!file.exists(path)) return(invisible(NULL))
  # safe: a damaged old table is still kept, copied as it is below
  old <- tryCatch(as.data.frame(read_cfg(path), stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(old) && !nrow(old)) return(invisible(NULL))          # an empty starter table: nothing worth keeping
  key <- function(d) if (!nrow(d)) character() else do.call(paste, c(lapply(d, as.character), sep = "\r"))
  ko <- if (!is.null(old)) key(old); kn <- if (is.data.frame(new)) key(as.data.frame(new, stringsAsFactors = FALSE))
  if (!is.null(ko) && !is.null(kn) && identical(ko, kn) && identical(names(old), names(new))) return(invisible(NULL))
  h <- file.path(dirname(path), "history"); dir.create(h, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S"); base <- tools::file_path_sans_ext(basename(path))
  b <- paste0(base, "__", stamp, ".csv"); n <- 1
  while (file.exists(file.path(h, b))) { n <- n + 1; b <- paste0(base, "__", stamp, "_", n, ".csv") }
  file.copy(path, file.path(h, b))
  both <- !is.null(ko) && !is.null(kn)
  row <- data.table(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), user = who,
                    table = paste0(basename(path), if (nzchar(what)) paste0(" (", what, ")") else ""),
                    rows_before = if (is.null(ko)) NA_integer_ else length(ko),
                    rows_after = if (!is.null(kn)) length(kn) else if (is.numeric(new)) as.integer(new[1]) else NA_integer_,
                    rows_added_or_changed = if (both) sum(!kn %in% ko) else NA_integer_,
                    rows_removed_or_changed = if (both) sum(!ko %in% kn) else NA_integer_, backup = b)
  lf <- file.path(h, "change_log.csv")
  # safe: an unreadable header is taken as an older log, which is rewritten below (and reported there)
  hdr <- if (file.exists(lf)) tryCatch(names(fread(lf, nrows = 0)), error = function(e) character()) else character()
  if (!file.exists(lf) || identical(hdr, HIST_COLS)) fwrite(row, lf, append = file.exists(lf))
  else {                                                   # an older, shorter log: rewrite it with every column
    prev <- try_or(fread(lf, colClasses = "character", na.strings = NULL), data.table(), "config/history/change_log.csv")
    fwrite(rbind(prev, row[, lapply(.SD, as.character)], fill = TRUE)[, c(HIST_COLS, setdiff(names(prev), HIST_COLS)), with = FALSE], lf)
  }
  invisible(b)
}

# ---- import / export of a whole configuration ------------------------------------------------------------------
# Export: every set-up table plus the settings, in one zip with a manifest. Import: a zip like that, a single CSV
# (recognised by its columns) or an .xlsx with one sheet per table; nothing is replaced until the preview is accepted.
# An app's CONFIG_EXTRA (file names, or a named vector: file = what it is) lists files of config/ that are no set-up table
# but go with the configuration as they are (the review decisions, the deployment sheet's own columns ...).
config_extra <- function() { x <- get0("CONFIG_EXTRA", ifnotfound = character()); if (is.null(names(x))) stats::setNames(x, x) else x }
export_config <- function(project_dir, settings, zipfile) {
  config_migrate(project_dir)
  tmp <- tempfile("ovidcfg_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- character()
  for (w in names(CONFIG_FILES)) { p <- cfg_path(project_dir, w); if (file.exists(p)) { file.copy(p, file.path(tmp, basename(p))); files <- c(files, basename(p)) } }
  for (f in setdiff(names(config_extra()), files)) { p <- file.path(path.expand(project_dir), "config", f); if (file.exists(p)) { file.copy(p, file.path(tmp, f)); files <- c(files, f) } }
  s <- settings[!vapply(settings, is.null, logical(1))]
  fwrite(data.table(setting = names(s), value = vapply(s, function(v) paste(v, collapse = ";"), "")), file.path(tmp, "settings.csv")); files <- c(files, "settings.csv")
  fwrite(data.table(file = files, exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), app = APP_NAME, version = APP_VERSION, core = CORE_VERSION),
         file.path(tmp, "manifest.csv"))
  zip::zip(zipfile, c(files, "manifest.csv"), root = tmp)
  invisible(zipfile)
}

# What a file to import holds: a named list of tables (by CONFIG_FILES name, plus "settings"), the app's CONFIG_EXTRA files
# by file name in `extra`, and a table of an older version that was folded into another (config_folds() below) already
# folded into it. Files the app reads its own way (CONFIG_IMPORT_FILES) go to its config_import_more(out, files), which
# returns `out` with what it made of them (files: the tables read, by file name).
read_config_import <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name)); out <- list(); old <- list(); more <- list(); ex <- config_extra()
  folds <- config_folds(); from <- vapply(folds, `[[`, "", "from"); own <- get0("CONFIG_IMPORT_FILES", ifnotfound = character())
  which_table <- function(fname, cols) {
    hit <- names(CONFIG_FILES)[vapply(CONFIG_FILES, function(x) x$file == basename(fname), logical(1))]
    if (length(hit)) return(hit[1])
    sc <- vapply(names(CONFIG_FILES), function(w) { cc <- cfg_cols(w); length(intersect(cols, cc)) / length(cc) }, 0)
    if (max(sc) >= 0.6) names(CONFIG_FILES)[which.max(sc)] else NA_character_ }
  shape <- function(d, w) { cc <- cfg_cols(w); if (!is.null(CONFIG_FILES[[w]]$optional)) cc <- union(cc, names(d))   # a table with columns of its own keeps every one
    ensure_cols(d, cc)[, cc, with = FALSE] }
  # one CSV, by its name: an extra file, an older table, one the app reads itself, or else a set-up table
  take <- function(f, d, strict = FALSE) { b <- basename(f)
    if (b %in% names(ex)) { out$extra[[b]] <<- d; return(TRUE) }
    if (b %in% from) { old[[b]] <<- d; return(TRUE) }
    if (b %in% own) { more[[b]] <<- d; return(TRUE) }
    w <- which_table(f, names(d)); if (is.na(w)) { if (strict) stop("The columns of ", name, " do not match any set-up table."); return(FALSE) }
    out[[w]] <<- shape(d, w); TRUE }
  if (ext == "zip") {
    tmp <- tempfile("ovidimp_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    # safe: a file that is not a zip stops on the next line with a message
    ok <- tryCatch({ zip::unzip(path, exdir = tmp); TRUE }, error = function(e) FALSE)
    if (!ok) stop(name, " is not a readable zip file. Choose a configuration exported from Ovid (a .zip), or a single set-up table (.csv).", call. = FALSE)
    for (f in list.files(tmp, "\\.csv$", recursive = TRUE, full.names = TRUE)) {
      if (basename(f) == "manifest.csv") next
      d <- read_cfg(f); if (basename(f) == "settings.csv") { out$settings <- d; next }
      take(f, d) }
  } else if (ext %in% c("xlsx", "xls")) {
    for (s in readxl::excel_sheets(path)) { d <- as.data.table(readxl::read_excel(path, sheet = s, col_types = "text"))
      for (v in names(d)) set(d, j = v, value = trim(fifelse(is.na(d[[v]]), "", d[[v]])))
      w <- which_table(paste0(s, ".csv"), names(d)); if (is.na(w)) w <- which_table(s, names(d))
      if (!is.na(w)) out[[w]] <- shape(d, w) }
  } else if (ext == "csv") take(name, read_cfg(path), strict = TRUE)
  else stop("Import a .zip exported from this app, a set-up .csv, or an .xlsx with one sheet per table.")
  if (length(more) && is.function(get0("config_import_more", mode = "function"))) out <- config_import_more(out, more)
  for (f in folds) { if (is.null(old[[f$from]])) next                      # an older table: its rows into the table it is part of now
    w <- names(CONFIG_FILES)[vapply(CONFIG_FILES, function(x) x$file == f$into, NA)]; if (!length(w)) next
    r <- f$fold(old[[f$from]], if (is.null(out[[w[1]]])) empty_cfg(cfg_cols(w[1])) else out[[w[1]]])
    out[[w[1]]] <- ensure_cols(as.data.table(r$rows), cfg_cols(w[1])) }
  out
}

# Rows added, removed and unchanged between the current table and an incoming one.
config_diff <- function(cur, new) {
  key <- function(d) do.call(paste, c(as.list(d), sep = "\u241f"))
  kc <- key(cur); kn <- key(new)
  data.table(added = sum(!kn %in% kc), removed = sum(!kc %in% kn), unchanged = sum(kn %in% kc), rows_now = nrow(cur), rows_after = nrow(new))
}

# The table as it would be after an import, for the preview and for applying it.
config_after <- function(w, new, project_dir, mode = "replace") {
  p <- cfg_path(project_dir, w); cc <- cfg_cols(w); cur <- if (file.exists(p)) read_cfg(p) else empty_cfg(cc)
  if (!is.null(CONFIG_FILES[[w]]$optional)) cc <- union(cc, union(names(cur), names(new)))   # a table with columns of its own keeps every one
  cur <- ensure_cols(cur, cc)[, cc, with = FALSE]; new <- ensure_cols(as.data.table(new), cc)[, cc, with = FALSE]
  after <- if (identical(mode, "merge")) unique(rbind(cur, new)) else new
  list(cur = cur, after = after)
}

apply_config_import <- function(imp, project_dir, mode = "replace", who = "", what = "import") {
  done <- character(); config_migrate(project_dir, who)
  for (w in intersect(names(imp), names(CONFIG_FILES))) {
    p <- cfg_path(project_dir, w); x <- config_after(w, imp[[w]], project_dir, mode)
    if (file.exists(p) && identical(config_diff(x$cur, x$after)[, added + removed], 0L)) next      # nothing would change
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); backup_cfg(p, x$after, who, what)
    fwrite_safe(x$after, p); done <- c(done, w)
  }
  for (f in intersect(names(imp$extra), names(config_extra()))) { x <- config_extra_after(f, imp$extra[[f]], project_dir, mode)
    if (file.exists(x$path) && identical(config_diff(x$cur, x$after)[, added + removed], 0L)) next
    dir.create(dirname(x$path), recursive = TRUE, showWarnings = FALSE); backup_cfg(x$path, x$after, who, what); fwrite_safe(x$after, x$path); done <- c(done, f) }
  invisible(done)
}
# A CONFIG_EXTRA file of an import: the current one and the one after (replaced, or with mode "merge" the rows added).
config_extra_after <- function(f, new, project_dir, mode = "replace") {
  p <- file.path(path.expand(project_dir), "config", f); cur <- if (file.exists(p)) tryCatch(read_cfg(p), error = function(e) empty_cfg(names(new))) else empty_cfg(names(new))
  cols <- union(names(cur), names(new)); al <- function(d) ensure_cols(copy(as.data.table(d)), cols)[, cols, with = FALSE]
  cur <- al(cur); new <- al(new)
  list(path = p, cur = cur, after = if (identical(mode, "merge")) unique(rbind(cur, new)) else new)
}
# The preview of an import: one row per table (label, rows added, removed, unchanged, now, after) and per extra file.
config_import_preview <- function(imp, project_dir, mode = "replace") {
  x <- rbindlist(lapply(intersect(names(imp), names(CONFIG_FILES)), function(w) { a <- config_after(w, imp[[w]], project_dir, "replace")
    cbind(data.table(table = CONFIG_FILES[[w]]$label %||% CONFIG_FILES[[w]]$file), config_diff(a$cur, a$after)) }), fill = TRUE)
  ex <- config_extra()
  for (f in intersect(names(imp$extra), names(ex))) { a <- config_extra_after(f, imp$extra[[f]], project_dir, mode)
    x <- rbind(x, cbind(data.table(table = ex[[f]]), config_diff(a$cur, a$after)), fill = TRUE) }
  x
}

# ---- tables folded into others ----------------------------------------------------------------------------------------
# A table of an older version that is now part of another (manual removals in the corrections over a period, the logger list
# in the deployment sheet ...): its rows are moved in once, the first time the project is read, and the old file is left where
# it was, untouched; config/migrated.json remembers it was done, so its rows are never applied twice. Each app lists its own
# in CONFIG_FOLDS, list(from = "old.csv", into = "new.csv", fold = function(old, cur[, project_dir]) list(rows = <the new table>, log =
# <what to say, or NULL: where, problem>)); the core adds the logger splits (core grouping.R) to every app's.
CFG_MIGRATED <- "migrated.json"
cfg_migrated <- function(project_dir) { p <- file.path(path.expand(project_dir), "config", CFG_MIGRATED)
  if (file.exists(p)) try_or(as.list(jsonlite::fromJSON(p)), list(), CFG_MIGRATED) else list() }
config_folds <- function() { f <- get0("CONFIG_FOLDS", ifnotfound = list()); if (exists("group_split_fold", mode = "function") && exists("dep_split_rows", mode = "function")) f <- c(f, list(group_split_fold())); f }
config_migrate <- function(project_dir, who = "ovid") {
  if (length(project_dir) != 1 || is.na(project_dir) || !nzchar(trim(project_dir)) || isTRUE(get0("VIEWER", inherits = TRUE))) return(invisible(character()))
  cdir <- file.path(path.expand(project_dir), "config"); folds <- config_folds(); done <- character()
  if (!dir.exists(cdir) || !length(folds)) return(invisible(done))
  m <- cfg_migrated(project_dir)
  for (f in folds) { po <- file.path(cdir, f$from); if (!file.exists(po) || !is.null(m[[f$from]])) next
    old <- tryCatch(read_cfg(po), error = function(e) NULL); if (is.null(old)) next                 # damaged: left until it can be read
    # safe: an unreadable target table is skipped on the next line and tried again at the next start
    pn <- file.path(cdir, f$into); cur <- if (file.exists(pn)) tryCatch(read_cfg(pn), error = function(e) NULL) else NULL
    if (file.exists(pn) && is.null(cur)) next                                                       # the table it goes into cannot be read
    r <- tryCatch(if ("project_dir" %in% names(formals(f$fold))) f$fold(old, cur, project_dir = project_dir) else f$fold(old, cur), error = function(e) { if (exists("problem", mode = "function")) problem(f$from, paste("its rows could not be moved into", f$into, ":", conditionMessage(e))); NULL })
    if (is.null(r)) next
    ok <- try_or({ if (nrow(old) || !file.exists(pn)) { if (!is.null(cur)) backup_cfg(pn, as.data.frame(r$rows), who, paste("rows of", f$from)); fwrite_safe(r$rows, pn) }
      m[[f$from]] <- list(into = f$into, rows = nrow(old), at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE), file.path(cdir, CFG_MIGRATED)); TRUE }, FALSE, f$from)
    if (!ok) next
    done <- c(done, f$from)
    lg <- r$log; if (!is.null(lg) && NROW(lg) && exists("problem", mode = "function")) for (i in seq_len(min(NROW(lg), 50))) problem(lg$where[i], lg$problem[i])
    if (exists("live_touch", mode = "function")) live_touch() }
  invisible(done)
}

# ---- review decisions (config/review_decisions.csv) ---------------------------------------------------------------------
# One row per flag a person decided on, in the Workbench's review queue or on Check the data: id (wb_queue_id of the flag's
# where, parameter, start, end and check), decision, note, author, at. "not a problem" (or RS's "accepted as is") accepts
# the flag: the next run keeps it in flags.csv marked accepted and leaves it out of the values' flag columns.
REVIEW_FILE <- "review_decisions.csv"
REVIEW_COLS <- c("id", "decision", "note", "author", "at")
REVIEW_ACCEPT <- c("not a problem", "accepted as is")
review_path <- function(project_dir) file.path(path.expand(project_dir), "config", REVIEW_FILE)
# A time as "YYYY-MM-DD HH:MM:SS" whether it came as a time or as text (read back from flags.csv, a midnight may have lost
# its 00:00:00); text that is not a time is kept as it is.
review_ts <- function(x) {
  if (inherits(x, "POSIXct")) return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  x <- sub("Z$", "", sub("T", " ", trim(as.character(x)))); out <- x; todo <- grepl("^[0-9]{4}-", x) & !is.na(x)
  for (f in c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")) { k <- which(todo); if (!length(k)) break
    t <- as.POSIXct(x[k], format = f, tz = "UTC"); ok <- !is.na(t); out[k[ok]] <- format(t[ok], "%Y-%m-%d %H:%M:%S"); todo[k[ok]] <- FALSE }
  out
}
review_hash <- function(x) vapply(x, function(s) substr(if (requireNamespace("digest", quietly = TRUE))   # the same as core cache.R hash_of(s), faster
  digest::digest(serialize(s, NULL, version = 2), "md5", serialize = FALSE) else hash_of(s), 1, 16), "", USE.NAMES = FALSE)
wb_queue_id <- function(where, parameter, start, end, check) review_hash(paste(where, parameter, review_ts(start), review_ts(end), check, sep = "|"))
review_read <- function(path) { if (length(path) != 1 || !file.exists(path)) return(empty_cfg(REVIEW_COLS))
  tryCatch(ensure_cols(read_cfg(path), REVIEW_COLS), error = function(e) empty_cfg(REVIEW_COLS)) }
# Drop the decisions on ids, then add rows (a table with REVIEW_COLS) if any.
review_write <- function(path, ids, rows = NULL) {
  d <- review_read(path); d <- d[!id %in% ids]; if (!is.null(rows) && NROW(rows)) d <- rbind(d, as.data.table(rows), fill = TRUE)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE); fwrite_safe(d[, REVIEW_COLS, with = FALSE], path)
  if (exists("live_touch", mode = "function")) live_touch()
  invisible(d) }
# The row of d (decisions) for each flag, or NA: by its id, else by the id an earlier version gave it (a time at midnight
# written as the date alone).
review_match <- function(d, where, parameter, start, end, check) {
  if (is.null(d) || !NROW(d) || !length(where)) return(rep(NA_integer_, length(where)))
  k <- match(wb_queue_id(where, parameter, start, end, check), d$id); m <- which(is.na(k))
  if (length(m)) { short <- function(x) sub(" 00:00:00$", "", review_ts(x)[m])
    k[m] <- match(review_hash(paste(where[m], parameter[m], short(start), short(end), check[m], sep = "|")), d$id) }
  k }
review_decision <- function(d, where, parameter, start, end, check) { k <- review_match(d, where, parameter, start, end, check)
  if (all(is.na(k))) rep(NA_character_, length(k)) else d$decision[k] }
# A run, where its flags are final: flags gets a column `accepted` (TRUE where a person said the flag is not a problem).
# where, parameter: the columns of flags that name the place and the value (station and parameter, or site and channel).
review_accept <- function(flags, decisions, where, parameter = "parameter") {
  fl <- as.data.table(flags)
  dec <- if (nrow(fl)) review_decision(decisions, fl[[where]], fl[[parameter]], fl$start, fl$end, fl$check) else character()
  fl[, accepted := !is.na(dec) & dec %in% REVIEW_ACCEPT]
  com <- intersect(get0("FLAG_COMMON", ifnotfound = c("rule_id", "qc_code", "app", "run_id")), names(fl))   # flags.csv still ends with rule_id, qc_code, app, run_id (core qc.R)
  setcolorder(fl, c(setdiff(names(fl), com), com))[] }
flag_accepted <- function(flags) if ("accepted" %in% names(flags)) toupper(as.character(flags$accepted)) %in% "TRUE" else rep(FALSE, NROW(flags))
# The flags still open: neither accepted by the run that wrote them nor decided since (decisions: the review table).
review_open <- function(flags, decisions, where, parameter = "parameter") {
  if (is.null(flags) || !NROW(flags)) return(flags); fl <- as.data.table(flags)
  dec <- review_decision(decisions, fl[[where]], fl[[parameter]], fl$start, fl$end, fl$check)
  fl[!flag_accepted(fl) & !(dec %in% REVIEW_ACCEPT)] }
# The checks of accepted flags taken out of a record's flag column (checks separated by ;). acc: the accepted flags of this
# record (parameter, start, end, check); col(p): the flag column of parameter p; within(p): the rows it can apply to.
review_unmark <- function(d, acc, col = function(p) paste0("flag_", p), within = function(p) rep(TRUE, nrow(d)), time = "TIMESTAMP") {
  if (is.null(acc) || !NROW(acc) || !NROW(d)) return(d)
  t0 <- as.POSIXct(review_ts(acc$start), tz = "UTC"); t1 <- as.POSIXct(review_ts(acc$end), tz = "UTC")
  for (i in seq_len(NROW(acc))) { k <- col(acc$parameter[i]); if (!k %in% names(d) || !is.character(d[[k]]) || is.na(t0[i]) || is.na(t1[i])) next
    j <- which(within(acc$parameter[i]) & d[[time]] >= t0[i] & d[[time]] <= t1[i] & !is.na(d[[k]]) & nzchar(d[[k]])); if (!length(j)) next
    set(d, j, k, vapply(strsplit(d[[k]][j], ";", fixed = TRUE), function(x) paste(x[x != acc$check[i]], collapse = ";"), "")) }
  d }
