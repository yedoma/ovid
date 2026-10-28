# Ovid/cache.R  --  Reusing work between runs: a step whose inputs have not changed is read back instead of redone.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Each cached result is one .rds file in <project>/cache/<kind>/, named <name>__<key>.rds. The key is a hash of
# everything the result depends on: its input files (path, size, modification time or md5), the set-up rows that
# apply to it, the settings it uses, and the app's code (code_hash), so editing a reader or a rule redoes the work.
# A run lists the cache files it used; cache_prune() then removes the rest, so the cache never outgrows the data.

# Short hash of any R value.
hash_of <- function(x, n = 16L) {
  f <- tempfile(); on.exit(unlink(f))
  saveRDS(x, f, version = 2, compress = FALSE)
  substr(unname(tools::md5sum(f)), 1, n)
}

# Hash of the code that produces the output: every .R file in the given folders (and any extra files), except the core
# files that only build screens (CORE_UI_FILES: no pipeline sources them), so a change to an interface never makes an
# app redo its work.
CORE_UI_FILES <- c("shell.R", "runner.R", "workbench.R", "editor.R", "metasheet_ui.R", "files.R", "launcher.R", "projects.R", "rename.R", "tsview.R", "flow.R", "sheet.R", "reference.R", "convert_page.R", "grouping.R", "table.R", "simple.R")
code_hash <- function(dirs, extra = character()) {
  f <- c(unlist(lapply(dirs[dir.exists(dirs)], list.files, pattern = "\\.R$", full.names = TRUE)), extra[file.exists(extra)])
  if (exists("CORE_DIR")) f <- f[!normalizePath(f, winslash = "/", mustWork = FALSE) %in% normalizePath(file.path(CORE_DIR, CORE_UI_FILES), winslash = "/", mustWork = FALSE)]
  f <- f[order(basename(f))]
  hash_of(list(basename(f), unname(tools::md5sum(f))), 12L)
}

# The code one step depends on, as a hash: its entry functions and every function and constant they use, followed through
# the code (codetools::findGlobals), wherever it is defined (the app's R/ files or core). Package functions are not
# followed. Each is hashed as parsed code, so comments and layout do not count: editing a report or a plot redoes nothing
# that does not draw it, and a core change redoes only the steps that call what changed.
#   entries  names of functions (or functions themselves) the step calls
#   shallow  names of functions taken as their own text only, not followed: the pipeline function whose inline code makes
#            the step, so an edit there still counts without pulling in everything it calls
#   env      where the names are looked up (the pipeline's environment)
#   skip     names never followed: settings and run state, which keys hold separately, and say() (console messages,
#            which apps and tests replace with their own)
#   only     the names the code files define (code_names()); anything else in the environment (a test's or an app's own
#            objects that happen to share a name with a data column) is never followed, so the hash is the same wherever
#            the pipeline is sourced
CODE_SKIP <- c("say", "cache_retry", "run_project", "run_tick", "run_check_cancel", "RUN_TICK", "CFG", "SETTINGS", "AUDIT", "FLAGS", "READ_CACHE", "STATE", "APP_DIR", "PIPELINE_DIR", "CORE_DIR", "CODE_HASH", "STEP_HASH")
fn_text <- function(f) paste(deparse(utils::removeSource(f), control = c("keepInteger", "keepNA", "niceNames")), collapse = "\n")
code_fingerprint <- function(entries = character(), shallow = character(), env = parent.frame(), skip = CODE_SKIP, n = 12L,
                             only = if (exists("CODE_NAMES", envir = env)) get("CODE_NAMES", envir = env) else NULL) {
  parts <- list(); seen <- skip; q <- character()
  own <- function(f) { e <- environment(f); is.null(e) || !(isNamespace(e) || identical(e, baseenv()) || identical(e, emptyenv())) }
  as_text <- function(x) if (is.function(x)) fn_text(x) else if (is.list(x)) rapply(x, fn_text, classes = "function", how = "replace") else x
  calls_in <- function(x) {                                      # names used by a function, or by the functions inside a list
    fs <- if (is.function(x)) list(x) else if (is.list(x)) Filter(is.function, unlist(x, recursive = TRUE)) else list()
    unique(unlist(lapply(fs, function(f) if (is.primitive(f)) character() else codetools::findGlobals(f, merge = TRUE)))) }
  for (nm in shallow) if (exists(nm, envir = env)) { parts[[paste0("shallow:", nm)]] <- fn_text(get(nm, envir = env)); seen <- c(seen, nm) }
  for (e in entries) if (is.function(e)) { parts[[paste0("fn:", length(parts))]] <- fn_text(e); q <- c(q, calls_in(e)) } else q <- c(q, e)
  while (length(q)) {
    nm <- q[1]; q <- q[-1]; if (nm %in% seen) next; seen <- c(seen, nm)
    if ((!is.null(only) && !nm %in% only) || !exists(nm, envir = env)) next
    x <- get(nm, envir = env)
    if (is.function(x)) { if (is.primitive(x) || !own(x)) next }
    else if (is.environment(x) || inherits(x, c("R6", "reactive", "shiny.tag"))) next
    parts[[nm]] <- as_text(x); q <- c(q, calls_in(x))
  }
  hash_of(parts[order(names(parts))], n)
}

# The names the code files assign at their top level (functions and constants), for code_fingerprint(only =). The core files
# that only build screens (CORE_UI_FILES) are left out, as in code_hash().
code_names <- function(dirs, extra = character()) {
  f <- c(unlist(lapply(dirs[dir.exists(dirs)], list.files, pattern = "\\.R$", full.names = TRUE)), extra[file.exists(extra)])
  if (exists("CORE_DIR")) f <- f[!normalizePath(f, winslash = "/", mustWork = FALSE) %in% normalizePath(file.path(CORE_DIR, CORE_UI_FILES), winslash = "/", mustWork = FALSE)]   # screens only: no pipeline sources them
  unique(unlist(lapply(f, function(p) { x <- tryCatch(parse(p, keep.source = FALSE), error = function(e) expression())
    unlist(lapply(x, function(e) if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=", "<<-") && is.symbol(e[[2]])) as.character(e[[2]]))) })))
}

# App functions that replace a core function of the same name. An app's copy wins for every caller, core's own included,
# so a fix made in core would silently not reach that app. Each app checks at start-up (and in its tests) and says which;
# `allow` names any replacement meant on purpose (none now: say() is core's only, with the app's log and steps).
core_shadows <- function(app_files, allow = character()) {
  fdefs <- function(f) unique(unlist(lapply(f[file.exists(f)], function(p) { x <- tryCatch(parse(p, keep.source = FALSE), error = function(e) expression())
    unlist(lapply(x, function(e) if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.symbol(e[[2]]) &&
                                     is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) as.character(e[[2]]))) })))
  core <- fdefs(list.files(CORE_DIR, pattern = "\\.R$", full.names = TRUE))
  setdiff(sort(intersect(fdefs(app_files), core)), allow)
}

# Every R file of an app: its R/ folder, its pipeline, app.R, its page files and its Workbench.
app_code_files <- function(dir) c(list.files(file.path(dir, "R"), pattern = "\\.R$", full.names = TRUE), list.files(dir, pattern = "_pipeline\\.R$", full.names = TRUE),
                                  file.path(dir, "app.R"), list.files(file.path(dir, "pages"), pattern = "\\.R$", full.names = TRUE),
                                  list.files(file.path(dir, "workbench"), pattern = "\\.R$", full.names = TRUE))
# safe: a note for developers only; the app runs the same without it
warn_core_shadows <- function(dir) { s <- tryCatch(core_shadows(app_code_files(dir)), error = function(e) character())
  if (length(s)) say("  note for developers: ", paste(s, collapse = ", "), " in this app replace", if (length(s) == 1) "s" else "", " the core function", if (length(s) > 1) "s", " of the same name; rename the app's copy", col = "yellow")
  invisible(s) }

# The rows of a set-up table (or of a part of one) as a key: only the values count, not how the table was made (an empty
# table and an empty subset of a longer one carry different attributes), and no rows at all is NULL.
key_rows <- function(x) if (is.null(x) || !NROW(x)) NULL else as.list(x)

# Settings as a key: by name (their order does not matter) and as text, so a run from the app and one from the command line
# key alike (2000 and 2000L are one setting; so are NA and a numeric input left blank). drop: settings left out.
key_cfg <- function(cfg, drop = character()) { x <- cfg[setdiff(names(cfg), drop)]; x <- x[order(names(x))]
  lapply(x, function(v) if (is.null(v) || !length(v)) "" else { v <- as.character(unlist(v)); v[is.na(v)] <- "NA"; v }) }

cache_file <- function(dir, name, key) file.path(dir, paste0(safe_name(name), "__", key, ".rds"))

# The cached value, or NULL when there is none or it cannot be read (a half-written file from a stopped run).
cache_read <- function(dir, name, key) {
  f <- cache_file(dir, name, key)
  if (!file.exists(f)) return(NULL)
  # safe: a damaged cache file is read again from the raw files
  tryCatch(readRDS(f), error = function(e) NULL)
}

cache_write <- function(dir, name, key, value) {
  f <- cache_file(dir, name, key); tmp <- paste0(f, ".part")
  ok <- tryCatch({ save_rds_fast(value, tmp); file.rename(tmp, f) }, error = function(e) { problem("cache", paste("could not be written:", conditionMessage(e))); FALSE })
  if (!isTRUE(ok)) unlink(tmp)
  invisible(f)
}

# Removes every cached result of this kind that the run did not use.
cache_prune <- function(dir, used) {
  have <- list.files(dir, full.names = TRUE)
  unlink(have[!normalizePath(have, winslash = "/", mustWork = FALSE) %in% normalizePath(used, winslash = "/", mustWork = FALSE)])
}

# Drops every cached result for the given names (all keys), so the next run redoes them: "rescan this file".
cache_forget <- function(dir, names) {
  if (!dir.exists(dir) || !length(names)) return(invisible(0L))
  f <- list.files(dir, full.names = TRUE)
  hit <- f[sub("__[^_]*\\.rds$", "", basename(f)) %in% safe_name(names)]
  unlink(hit); invisible(length(hit))
}

# ---- cached data files written and read safely --------------------------------------------------------------------------------
# saveRDS_safe() writes beside the file and renames it into place, so a power cut never leaves half a file under the real
# name. cache_load() reads a cached data file; one that cannot be read stops the run with the condition "ovid_cache_bad",
# and cache_retry() (around each pipeline's steps) then clears the project's cache and runs once more from the raw files.
# Cached files are written with fast gzip (level 1): as quick to write and read as no compression, and 10 to 30 times
# smaller (long tables of repeated names). Any compress = argument is ignored.
save_rds_fast <- function(x, f) { con <- gzfile(f, "wb", compression = 1L); on.exit(close(con)); saveRDS(x, con) }
saveRDS_safe <- function(x, f, ...) {
  tmp <- paste0(f, ".part"); save_rds_fast(x, tmp)
  if (!file.rename(tmp, f)) { file.copy(tmp, f, overwrite = TRUE); unlink(tmp) }
  invisible(f)
}
cache_load <- function(f) tryCatch(readRDS(f), error = function(e) stop(structure(class = c("ovid_cache_bad", "error", "condition"),
  list(call = NULL, message = paste0("a cached file could not be read (", basename(f), ": ", conditionMessage(e), ")")))))
cache_retry <- function(cfg, fun) tryCatch(fun(), ovid_cache_bad = function(e) {
  say("  ", conditionMessage(e), "; the cache is cleared and the run starts again from the raw files", col = "orange")
  unlink(file.path(path.expand(cfg$project_dir), "cache"), recursive = TRUE); fun() })
