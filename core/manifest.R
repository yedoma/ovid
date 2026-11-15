# Ovid/manifest.R  --  Run manifest: what ran, with which code and which set-up; and one line per run in the history.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Each app sets APP_NAME and APP_VERSION (bump the version when a change alters the output). CORE_VERSION is the
# version of these shared files; bump it when a change here alters any app's output.

CORE_VERSION <- "1.16.0"

file_md5 <- function(f) if (file.exists(f)) unname(tools::md5sum(f)) else ""

# Writes output/audit/run_manifest.csv (key, value) and appends a line to output/audit/run_history.csv.
#   counts   named list of numbers for this run (files read, loggers, ...), kept in both files
#   settings named list of the settings that shape the output, kept in the manifest
#   packages packages whose versions are recorded
#   utc      TRUE: start and finish times in UTC; FALSE: in the computer's own clock
write_run_manifest <- function(cfg, t0, counts, settings = list(), packages = c("data.table", "ggplot2"), utc = TRUE) {
  adir <- file.path(cfg$project_dir, "output", "audit"); cdir <- file.path(cfg$project_dir, "config")
  dir.create(adir, recursive = TRUE, showWarnings = FALSE)
  cf <- sort(list.files(cdir, pattern = "\\.csv$")); h <- vapply(file.path(cdir, cf), file_md5, "")
  pkg <- function(p) if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "not installed"
  now <- Sys.time(); secs <- round(as.numeric(difftime(now, t0, units = "secs")))
  when <- function(t) if (utc) fmt_ts(as.POSIXct(format(t, tz = "UTC"), tz = "UTC")) else format(t, "%Y-%m-%d %H:%M:%S")
  chash <- substr(file_md5({ tf <- tempfile(); writeLines(paste(cf, h), tf); tf }), 1, 12)
  flat <- function(l) vapply(l, function(v) if (is.null(v) || !length(v)) "" else paste(v, collapse = " ; "), "")
  kv <- c(run_id = AUDIT$run_id, app = APP_NAME, pipeline_version = APP_VERSION, core_version = CORE_VERSION,
          started = when(t0), finished = when(now), seconds = secs, user = Sys.info()[["user"]], machine = Sys.info()[["nodename"]],
          R = paste(R.version$major, R.version$minor, sep = "."), platform = R.version$platform,
          setNames(vapply(packages, pkg, ""), packages), flat(settings), config_hash = chash,
          unlist(counts), setNames(h, paste0("config/", cf)))
  fwrite(setnames(data.table(names(kv), as.character(kv)), c("key", "value")), file.path(adir, "run_manifest.csv"))
  hist <- as.data.table(as.list(c(kv[c("run_id", "pipeline_version", "core_version", "started", "seconds", "user", "config_hash")], unlist(counts))))
  hf <- file.path(adir, "run_history.csv")
  old <- if (file.exists(hf)) try_or(fread(file = hf, colClasses = "character"), where = "output/audit/run_history.csv") else NULL
  fwrite(rbind(old, hist[, lapply(.SD, as.character)], fill = TRUE), hf)
  invisible(kv)
}

# ---- Run log ---------------------------------------------------------------------------------------------------------
# Every run leaves output/logs/<start>_<app>.log, kept after the app closes: what the run printed, each warning and
# message, and for an error its message and the calls that led to it; a header with the versions and the folders (home
# shown as ~) and a footer with the time each stage took, the total and whether it finished. The newest RUN_LOG_KEEP are kept.
RUN_LOG_KEEP <- 50L
tilde <- function(x) { x <- as.character(x); h <- path.expand("~"); ifelse(startsWith(x, h), paste0("~", substring(x, nchar(h) + 1)), x) }
run_logged <- function(cfg, fun) {
  pd <- path.expand(as.character(cfg$project_dir %||% (if (exists("CFG")) CFG$project_dir else "") %||% ""))
  dir <- file.path(pd, "output", "logs")
  if (!nzchar(pd) || !(dir.exists(dir) || dir.create(dir, recursive = TRUE, showWarnings = FALSE))) return(fun())
  t0 <- Sys.time(); app <- if (exists("APP_NAME")) APP_NAME else "app"
  f <- file.path(dir, paste0(format(t0, "%Y%m%dT%H%M%S"), "_", safe_name(sub("^.*// *", "", app)), ".log"))
  k <- 1L; while (file.exists(f)) { k <- k + 1L; f <- sub("(_[0-9]+)?\\.log$", paste0("_", k, ".log"), f) }    # two runs in one second
  # safe: with no log file the run goes on without its log
  con <- tryCatch(file(f, "wt"), error = function(e) NULL); if (is.null(con)) return(fun())
  w <- function(...) { cat(paste0(...), "\n", sep = "", file = con); flush(con) }
  w("Ovid ", sub("^.*// *", "", app), " ", if (exists("APP_VERSION")) APP_VERSION else "", ", core ", if (exists("CORE_VERSION")) CORE_VERSION else "", ", ", R.version.string, " (", R.version$platform, ")")
  w("Run started ", format(t0, "%Y-%m-%d %H:%M:%S %Z"), " by ", Sys.info()[["user"]], " on ", Sys.info()[["nodename"]])
  w("Project: ", tilde(pd)); for (k in intersect(c("raw_dirs", "metadata_dir", "dgps_dir"), names(cfg))) w(k, ": ", paste(tilde(path.expand(as.character(unlist(cfg[[k]])))), collapse = " ; "))
  for (n in cfg$run_notes) w("Note: ", n)
  # safe: corrections_log() puts its own failure on the problem list
  if (exists("corrections_log", mode = "function")) for (x in tryCatch(corrections_log(cfg), error = function(e) character())) w(x)
  if (is.character(cfg$run_kept) && !identical(cfg$run_kept, "kept")) w("Output updated in place (no copy kept to put back if the run stops): ", cfg$run_kept)
  if (length(cfg$run_anyway)) { w("Run anyway: started although the set-up check found ", length(cfg$run_anyway), " thing(s) that would be ignored or misapplied:")
    for (x in utils::head(cfg$run_anyway, 30)) w("  ", x) }
  w(strrep("-", 80))
  n0 <- sink.number(); sink(con, split = TRUE); status <- "failed"; if (exists("STAGES")) STAGES$at <- NULL
  on.exit({ while (sink.number() > n0) sink(); if (isTRUE(getOption("ovid.cancelled"))) status <- "cancelled by the user"
    w(strrep("-", 80)); st <- if (exists("stage_times", mode = "function")) stage_times() else character()
    if (length(st)) { w("Time per stage:"); for (x in st) w(x) }
    w("Run ", status, " after ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s, ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    close(con)
    l <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)                        # colours of the console left out
    if (!is.null(l)) writeLines(gsub("\033\\[[0-9;]*m", "", l[!startsWith(l, "@@ ")]), f)            # nor the progress ticks of a background run
    old <- sort(list.files(dir, pattern = "\\.log$", full.names = TRUE), decreasing = TRUE); unlink(old[-seq_len(min(length(old), RUN_LOG_KEEP))]) }, add = TRUE)
  res <- withCallingHandlers(fun(),
    warning = function(c) w("WARNING: ", conditionMessage(c), if (!is.null(conditionCall(c))) paste0("  [in ", substr(deparse1(conditionCall(c)), 1, 120), "]") else ""),
    message = function(c) w("MESSAGE: ", sub("\n$", "", conditionMessage(c))),
    error = function(c) { w("ERROR [", error_ref(conditionMessage(c)), "]: ", conditionMessage(c)); w("  calls (innermost last):")
      cs <- sys.calls(); n <- length(cs); top <- max(c(0L, which(vapply(cs, function(x) identical(x, quote(fun())), NA))))   # the run's own calls, not the logging around them
      cs <- cs[seq_len(n)[seq_len(n) > top & seq_len(n) <= n - 2L]]                                                  # nor the two frames of this handler
      for (x in utils::tail(cs, 20)) w("    ", substr(paste(deparse(x, nlines = 1L), collapse = " "), 1, 160)) })
  status <- "finished"
  res
}

# Errors in the app outside a run (a page that fails to draw, a button whose handler stops) go to ~/.ovid/logs/<app>_app.log,
# which outlives the session and goes into the troubleshooting zip. Set once by each app.R.
log_app_errors <- function(app = if (exists("APP_NAME")) APP_NAME else "app") {
  f <- file.path(path.expand("~"), ".ovid", "logs", paste0(safe_name(sub("^.*// *", "", app)), "_app.log"))
  options(shiny.error = function() tryCatch({ dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    # safe: an error while recording an error; nothing more can be done
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", sub("\n$", "", geterrmessage()), "\n", sep = "", file = f, append = TRUE) }, error = function(e) NULL))
  invisible(f)
}

# A short reference for an error, the same for the same message: E- and six characters. Quoted from a log or the
# troubleshooting zip, it finds the same error in any other log.
error_ref <- function(msg) paste0("E-", substr(hash_of(sub("\\s+$", "", as.character(msg)), 16L), 1, 6))

# ---- Troubleshooting zip ----------------------------------------------------------------------------------------------
# One zip to send when something goes wrong: versions, the newest run logs and the app's own error log, the problems and
# the run records, the set-up check, the set-up tables and when they changed. No raw data, nothing over 5 MB, no column that
# looks like a password or token, and every path with the home folder shown as ~. Returns its reference code.
#   check  a function returning the app's set-up check as a table (or NULL)
# What the troubleshooting zip would hold now, for a look before it is saved: each file with its size, and what was left out
# (and why). It is made the same way as the zip itself, in a temporary folder.
troubleshooting_inventory <- function(project_dir, check = NULL) {
  z <- tempfile(fileext = ".zip"); on.exit(unlink(z)); ref <- troubleshooting_zip(project_dir, z, check)
  l <- utils::unzip(z, list = TRUE); ux <- tempfile(); on.exit(unlink(ux, recursive = TRUE), add = TRUE); utils::unzip(z, files = file.path(ref, "README.txt"), exdir = ux)
  rd <- readLines(file.path(ux, ref, "README.txt"), warn = FALSE); i <- which(rd == "Left out:")
  list(files = data.table(file = sub(paste0("^", ref, "/"), "", l$Name), bytes = l$Length)[nzchar(file)], left_out = if (length(i)) trimws(rd[(i + 1):length(rd)]) else character())
}
troubleshooting_zip <- function(project_dir, zipfile, check = NULL, n_logs = 10L, max_mb = 5) {
  pd <- path.expand(project_dir %||% ""); app <- if (exists("APP_NAME")) sub("^.*// *", "", APP_NAME) else "app"
  ref <- paste0("OVID-", toupper(safe_name(app)), "-", format(Sys.time(), "%Y%m%d-%H%M"), "-", substr(hash_of(list(pd, Sys.time(), Sys.getpid()), 16L), 1, 4))
  tmp <- file.path(tempfile("ovid_ts_"), ref); dir.create(tmp, recursive = TRUE); left <- character()
  home <- path.expand("~"); clean <- function(x) gsub(home, "~", x, fixed = TRUE)
  rt <- tolower(path_norm(pd)); roots <- tryCatch(protected_roots(c(project_settings_read(pd), list(project_dir = pd, raw_dirs = get0("CFG")$raw_dirs))), error = function(e) character())
  put <- function(src, dest) { if (!file.exists(src)) return(invisible(FALSE))
    real <- tolower(normalizePath(src, winslash = "/", mustWork = FALSE))        # a file reached through a link is not copied unless it is really the project's
    inr <- any(vapply(roots, function(r) { rr <- tolower(path_norm(r)); real == rr || startsWith(real, paste0(rr, "/")) }, NA))
    if (!startsWith(src, path.expand(file.path(home, ".ovid"))) && (!startsWith(real, paste0(rt, "/")) || inr)) {
      left <<- c(left, paste0(clean(src), " (it leads outside the project folder: ", clean(normalizePath(src, winslash = "/", mustWork = FALSE)), ")")); return(invisible(FALSE)) }
    if (file.info(src)$size > max_mb * 1e6) { left <<- c(left, paste0(clean(src), " (over ", max_mb, " MB)")); return(invisible(FALSE)) }
    dir.create(dirname(file.path(tmp, dest)), recursive = TRUE, showWarnings = FALSE)
    # safe: a file that is not text is copied as it is
    l <- tryCatch(readLines(src, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(l)) file.copy(src, file.path(tmp, dest)) else writeLines(clean(l), file.path(tmp, dest)); invisible(TRUE) }
  # versions
  pk <- c("shiny", "bslib", "data.table", "ggplot2", "jsonlite", "zip", "DT", "plotly", "codetools")
  writeLines(c(paste("Reference:", ref), paste("App:", app, if (exists("APP_VERSION")) APP_VERSION else ""), paste("Core:", if (exists("CORE_VERSION")) CORE_VERSION else ""),
               paste("R:", R.version.string), paste("Platform:", R.version$platform), paste("OS:", utils::osVersion %||% Sys.info()[["sysname"]]),
               paste("Made:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), paste("Project:", clean(pd)), "", "Packages:",
               vapply(pk, function(p) paste0("  ", p, " ", if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "not installed"), "")),
             file.path(tmp, "versions.txt"))
  # logs
  lg <- sort(list.files(file.path(pd, "output", "logs"), pattern = "\\.log$", full.names = TRUE), decreasing = TRUE)
  for (f in utils::head(lg, n_logs)) put(f, file.path("logs", basename(f)))
  al <- file.path(home, ".ovid", "logs", paste0(safe_name(app), "_app.log")); put(al, file.path("logs", basename(al)))
  # run records and problems
  for (f in c("problems.csv", "run_manifest.csv", "run_history.csv", "reconciliation.csv", "setup_check.csv")) put(file.path(pd, "output", "audit", f), file.path("audit", f))
  if (is.function(check)) { sc <- tryCatch(check(), error = function(e) data.table(level = "error", problem = paste("the set-up check stopped:", conditionMessage(e))))
    if (!is.null(sc)) { sc <- as.data.table(sc); for (k in names(sc)) if (is.character(sc[[k]])) set(sc, j = k, value = clean(sc[[k]])); fwrite(sc, file.path(tmp, "setup_check_now.csv")) } }
  # set-up tables (secrets left out) and when they changed
  cf <- list.files(file.path(pd, "config"), pattern = "\\.csv$", full.names = TRUE)
  ok_cf <- vapply(cf, function(f) { r <- tolower(normalizePath(f, winslash = "/", mustWork = FALSE)); startsWith(r, paste0(rt, "/")) &&
    !any(vapply(roots, function(x) { rr <- tolower(path_norm(x)); r == rr || startsWith(r, paste0(rr, "/")) }, NA)) }, NA)
  if (any(!ok_cf)) left <- c(left, paste0(clean(cf[!ok_cf]), " (it leads outside the project folder)")); cf <- cf[ok_cf]
  for (f in cf) { d <- try_or(fread(file = f, colClasses = "character", na.strings = NULL), where = basename(f)); if (is.null(d)) next
    drop <- grep("password|passwd|token|secret|api_?key", names(d), ignore.case = TRUE, value = TRUE); if (length(drop)) d[, (drop) := NULL]
    for (k in names(d)) set(d, j = k, value = clean(d[[k]]))
    dir.create(file.path(tmp, "config"), showWarnings = FALSE); fwrite(d, file.path(tmp, "config", basename(f))) }
  hf <- list.files(file.path(pd, "config", "history"), recursive = TRUE, full.names = TRUE)
  if (length(hf)) fwrite(data.table(file = clean(sub(paste0("^", file.path(pd, "config", "history"), "/?"), "", hf)), saved = format(file.info(hf)$mtime, "%Y-%m-%d %H:%M:%S"),
                                    bytes = file.info(hf)$size)[order(-xtfrm(saved))], file.path(tmp, "config_changes.csv"))
  errs <- unique(unlist(lapply(list.files(file.path(tmp, "logs"), full.names = TRUE), function(f) regmatches(l <- readLines(f, warn = FALSE), regexpr("^ERROR \\[E-[0-9a-f]{6}\\]: .*", l)))))
  writeLines(c(paste("Ovid troubleshooting zip", ref), "",
               "Send this zip with a line on what you did and what went wrong. It holds no raw data.", "",
               "versions.txt        app, core, R and package versions", "logs/               the newest run logs and the app's own error log",
               "audit/              problems, the run manifest and history, reconciliation, the last set-up check", "setup_check_now.csv the set-up check as the zip was made",
               "config/             the set-up tables (columns that look like passwords or tokens left out)", "config_changes.csv  when each set-up table was saved",
               "", "Home folders are shown as ~.", if (length(errs)) c("", "Errors in these logs:", paste0("  ", errs)),
               if (length(left)) c("", "Left out:", paste0("  ", left))), file.path(tmp, "README.txt"))
  zipfile <- normalizePath(zipfile, mustWork = FALSE); unlink(zipfile)
  old <- setwd(dirname(tmp)); on.exit(setwd(old), add = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, ref) else utils::zip(zipfile, ref, flags = "-rq")
  invisible(ref)
}

# ---- the parts of a run every processor has ---------------------------------------------------------------------------------
# The project's folders (relative to it), created when missing; a folder that cannot be created stops the run with the reason.
project_dirs <- function(project_dir, dirs) {
  for (d in dirs) if (!dir.exists(file.path(project_dir, d)) && !dir.create(file.path(project_dir, d), recursive = TRUE, showWarnings = FALSE))
    stop("Cannot create ", file.path(project_dir, d), ". Check that the project folder is writable.")
  invisible(project_dir)
}
# Output files a run rebuilds or reuses one by one (station or site files, their plots and summaries) stay until the run has
# done so: outputs_prev() reads the list the last run left (paths under `out`; none when incremental is off), outputs_prune()
# removes now whatever in `dirs` is not on it, and outputs_done() removes what the last run wrote and this one did not, and
# keeps this run's list for the next.
outputs_prev <- function(file, incremental = TRUE) if (isTRUE(incremental) && file.exists(file)) tryCatch(readRDS(file), error = function(e) character()) else character()
outputs_prune <- function(out, dirs, keep) for (d in dirs) { f <- list.files(file.path(out, d))
  remove_within(file.path(out, d, f[!file.path(d, f) %in% keep]), getOption("ovid.project_root") %||% dirname(out)) }
outputs_done <- function(out, file, prev, now) { run_checkpoint("outputs_done"); now <- unlist(now, use.names = FALSE)
  remove_within(file.path(out, setdiff(prev, now)), getOption("ovid.project_root") %||% dirname(out)); saveRDS_safe(now, file); invisible(now) }

# ---- before a run: the folders, and one run at a time ----------------------------------------------------------------------
# Every app's run_pipeline() goes through run_project(): the project folder and the raw folders are checked, the project is
# locked for this run, and the run is logged (run_logged) and retried once from the raw files if a cached file is damaged
# (cache_retry). A check that fails stops the run before anything is written; no setting is changed. The checks:
#   - the project folder is set, and is not a raw folder, inside one, or around one (each would read or clear the other);
#   - every raw folder is there (a drive that is not plugged in would otherwise drop its stations' outputs);
#   - the project belongs to this app (.ovid_project names it: two apps in one folder overwrite each other's audit files
#     and clear each other's outputs), and a folder that holds other things is not taken over (Desktop, home);
#   - no other run is going on it (.ovid_lock, removed when the run ends; one left by a process that has gone is taken over).
PROJECT_MARK <- ".ovid_project"; PROJECT_LOCK <- ".ovid_lock"
OUTPUT_KEPT <- ".output_before_run"; OUTPUT_FAILED <- ".output_failed"                # see "the last finished output" below
PROJECT_KNOWN <- c("config", "output", "cache", "viewer_app", "reports", PROJECT_MARK, PROJECT_LOCK, OUTPUT_KEPT, OUTPUT_FAILED, ".ovid_run", ".ovid_last_run", ".output_before_run.part", ".DS_Store", "Thumbs.db", "desktop.ini")
project_stop <- function(...) stop(structure(class = c("ovid_project_check", "error", "condition"), list(message = paste0(...), call = NULL)))
path_norm <- function(p) {                                           # the real path, also of a folder not made yet (its nearest existing parent resolved)
  p <- sub("[/\\\\]+$", "", path.expand(p)); rest <- character()
  while (!dir.exists(p) && dirname(p) != p) { rest <- c(basename(p), rest); p <- dirname(p) }
  sub("/+$", "", do.call(file.path, as.list(c(normalizePath(p, winslash = "/", mustWork = FALSE), rest))))
}
path_within <- function(a, b) { a <- tolower(path_norm(a)); b <- tolower(path_norm(b)); a == b || startsWith(a, paste0(b, "/")) }   # a is b or under it
path_overlap <- function(a, b) path_within(a, b) || path_within(b, a)
shown <- function(p) { r <- path_norm(p); if (tolower(r) == tolower(sub("[/\\\\]+$", "", path.expand(p)))) tilde(r) else paste0(tilde(p), " (resolves to ", tilde(r), ")") }

# ---- the raw-data boundary -----------------------------------------------------------------------------------------------------
# Raw folders are only read. Nothing a run writes or removes may resolve into a raw folder or around one: the project folder
# (project_check) and every link inside it (raw_boundary_check: output/, output/stations, cache/, config/, the kept copies, an
# app's own folders) are followed through link chains and Windows junctions, and one that points into or around a raw folder,
# or cannot be resolved, stops the run before anything is written. remove_within() checks each removal again where it happens.
RAW_LINKS_MAX <- 2000L; RAW_LINK_DEPTH <- 8L
path_links <- function(dir) {                                        # links and junctions under dir (not followed): link -> resolved target ("" if unresolved)
  if (!dir.exists(dir)) return(character())
  if (.Platform$OS.type == "windows") {
    d <- list.dirs(dir, recursive = TRUE, full.names = TRUE)[-1]; f <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    own <- file.path(normalizePath(dirname(d), winslash = "/", mustWork = FALSE), basename(d))
    lk <- c(d[tolower(normalizePath(d, winslash = "/", mustWork = FALSE)) != tolower(own)], f[nzchar(Sys.readlink(f))])
  } else {
    lk <- suppressWarnings(system2("find", c("-P", shQuote(path.expand(dir)), "-type", "l"), stdout = TRUE, stderr = FALSE))
    if (!is.null(attr(lk, "status"))) project_stop("The links in ", tilde(path_norm(dir)), " could not be listed, so the run cannot check that it stays out of the raw folders. Nothing was changed.")
  }
  if (length(lk) > RAW_LINKS_MAX) project_stop("The folder ", tilde(path_norm(dir)), " holds more than ", RAW_LINKS_MAX, " links; the run does not check that many. Nothing was changed.")
  setNames(ifelse(file.exists(lk), normalizePath(lk, winslash = "/", mustWork = FALSE), ""), lk)
}
raw_boundary_check <- function(project_dir, raw) {
  pd <- path_norm(project_dir); raw <- raw[nzchar(raw)]; todo <- pd; seen <- tolower(pd); depth <- 0L; scanned <- pd   # the real folder: a linked project root is scanned too
  where <- function(l) { rel <- substring(l, nchar(pd) + 2L); if (startsWith(l, paste0(pd, "/")) && nzchar(rel)) rel else tilde(l) }
  while (length(todo)) {
    if (depth >= RAW_LINK_DEPTH) project_stop("In the project folder ", tilde(pd), ", links lead to other linked folders more than ", RAW_LINK_DEPTH, " deep (",
                                              tilde(todo[1]), "), so the run cannot establish that it stays out of the raw folders. Nothing was changed. ",
                                              "Replace those links with folders of their own and run again.")
    links <- unlist(lapply(todo, path_links)); todo <- character(); depth <- depth + 1L
    for (l in names(links)) { tg <- links[[l]]
      if (!nzchar(tg)) project_stop("In the project folder ", tilde(path_norm(pd)), ", ", where(l), " is a link whose target cannot be found (",
                                    tilde(Sys.readlink(l) %|% "unknown"), "). The run cannot tell whether it writes into a raw folder, so nothing was changed. ",
                                    "Remove the link or reconnect its drive, then run again.")
      for (r in raw) if (path_overlap(tg, r)) project_stop("In the project folder ", tilde(path_norm(pd)), ", ", where(l), " is a link to ", tilde(tg), ", which ",
                                                           if (path_within(tg, r)) "is inside" else "holds", " the raw folder ", tilde(r), " (resolves to ", tilde(path_norm(r)), "). ",
                                                           "Raw folders are only read, so nothing was changed. Replace the link with a folder of its own.")
      if (!dir.exists(tg) && isTRUE(fs::file_info(tg)$hard_links > 1))         # a linked file that is itself a hard link (perhaps of a raw file)
        project_stop("In the project folder ", tilde(pd), ", ", where(l), " is a link to ", tilde(tg), ", which shares its data with another file ",
                     "(a hard link, perhaps to a raw file), so writing it would change that file too. Nothing was changed. Remove the link and run again.")
      if (dir.exists(tg) && !tolower(tg) %in% seen) { seen <- c(seen, tolower(tg)); todo <- c(todo, tg); scanned <- c(scanned, tg) } }
  }
  hard_links_check(scanned, pd)
  invisible(TRUE)
}
# A file the run may write in place that shares its bytes with another file (a hard link, possibly to a raw file) would be
# changed in both: the run stops before opening it, naming it. Files written by the run are new files (written beside and
# renamed into place, or created anew), so only files already there are checked.
hard_links_check <- function(dirs, pd) {
  for (d in dirs[dir.exists(dirs)]) {
    # safe: an unreadable folder is reported by the run itself; this looks only for shared files
    fi <- tryCatch(fs::dir_info(d, recurse = TRUE, type = "file", all = TRUE), error = function(e) NULL)
    if (is.null(fi)) project_stop("The files in ", tilde(d), " could not be listed, so the run cannot check that it writes no raw file. Nothing was changed.")
    hl <- fi$path[!is.na(fi$hard_links) & fi$hard_links > 1]
    if (length(hl)) project_stop("In the project folder ", tilde(pd), ", ", paste(tilde(utils::head(hl, 3)), collapse = ", "), if (length(hl) > 3) ", ..." else "",
                                 " shares its data with another file (a hard link, perhaps to a raw file), so writing it would change that file too. ",
                                 "Nothing was changed. Replace it with a copy of its own (or delete it) and run again.")
  }
}
# Removes paths only where each resolves (its folder's links followed, the last part not) inside root and outside the raw
# folders of the run going (option ovid.raw_dirs); anything else stops with where it resolves and is left as it is.
remove_within <- function(paths, root, recursive = FALSE, raw = getOption("ovid.raw_dirs")) {
  paths <- sub("[/\\\\]+$", "", as.character(paths)); paths <- paths[nzchar(paths)]; if (!length(paths)) return(invisible(0L))
  dirs <- unique(dirname(paths)); res <- setNames(vapply(dirs, path_norm, ""), dirs)
  full <- file.path(res[dirname(paths)], basename(paths)); rt <- tolower(path_norm(root))
  bad <- !(tolower(full) == rt | startsWith(tolower(full), paste0(rt, "/")))
  lf <- tolower(full)
  for (r in raw[nzchar(raw %||% "")]) { rr <- tolower(path_norm(r)); bad <- bad | lf == rr | startsWith(lf, paste0(rr, "/")) | startsWith(rr, paste0(lf, "/")) }
  if (any(bad)) stop(structure(class = c("ovid_project_check", "error", "condition"), list(call = NULL, message = paste0(
    "Not removed: ", tilde(paths[bad][1]), " resolves to ", tilde(full[bad][1]), ", which is outside the project folder ", tilde(path_norm(root)),
    " or in a raw folder. Nothing was removed."))))
  if (.Platform$OS.type == "windows") for (p in paths[dir.exists(paths)]) {      # folder links first: unlink() cannot remove a symbolic one
    if (dir_link(p)) link_unlink(p) else if (recursive) for (l in names(path_links(p))) link_unlink(l) }
  unlink(paths, recursive = recursive); invisible(length(paths))
}
# Every folder a run must never write: the raw folders, and any an app adds as protect_dirs (RS: its saved sources and the
# folders a scan is given), whether plugged in or not.
# An app with folders of its own to protect defines app_protect_dirs(cfg) (RS: its saved sources).
protected_roots <- function(cfg) { extra <- if (is.function(f <- get0("app_protect_dirs", mode = "function"))) f(cfg) else character()
  r <- as.character(unlist(c(cfg$raw_dirs, cfg$protect_dirs, extra))); unique(r[!is.na(r) & nzchar(trim(r))]) }   # kept exactly: a name may end in a space
project_check <- function(cfg, app = sub("^.*// *", "", if (exists("APP_NAME")) APP_NAME else "app"), need_raw = TRUE) {
  pd <- trim(as.character(cfg$project_dir %||% "")[1] %|% "")
  if (!nzchar(pd)) pd <- ovid_project_dir(pd)                                    # none set: the default folder (core util.R)
  raw <- as.character(unlist(cfg$raw_dirs)); raw <- trim(raw[!is.na(raw) & nzchar(trim(raw))])
  if (need_raw && "raw_dirs" %in% names(cfg) && !length(raw)) project_stop("No raw data folder is set: it is the one thing every run needs. Choose the folder that holds the logger files on 1 Project ",
                                              "(Choose folder\u2026) and run again. Nothing was changed.")
  for (r in protected_roots(cfg)) {
    if (path_within(pd, r)) project_stop("The project folder ", shown(pd), " is inside the raw folder ", shown(r),
                                         ". Raw folders are only read; choose a project folder outside them.")
    if (path_within(r, pd)) project_stop("The raw folder ", shown(r), " is inside the project folder ", shown(pd),
                                         ". The app would read its own output as raw data; keep raw data outside the project folder.")
  }
  raw_boundary_check(pd, protected_roots(cfg))
  gone <- raw[!dir.exists(path.expand(raw))]
  if (need_raw && length(gone)) project_stop("Raw folder not found: ", paste(tilde(gone), collapse = ", "), ". Is the drive plugged in? Nothing was changed; ",
                                 "plug it in and run again, or remove the folder from the data folders if it is no longer used.")
  p <- path.expand(pd); mk <- file.path(p, PROJECT_MARK)
  if (file.exists(mk)) { owner <- ovid_app_name(sub("^app: *", "", grep("^app:", readLines(mk, warn = FALSE), value = TRUE)[1] %|% ""))
    if (nzchar(owner) && !identical(owner, app)) project_stop("The folder ", tilde(path_norm(pd)), " is the project of Ovid ", owner, ", not ", app,
                                                              ". Two apps in one project folder overwrite each other's files: choose a folder of its own.") }
  else if (dir.exists(p)) {
    other <- setdiff(list.files(p, all.files = TRUE, no.. = TRUE), PROJECT_KNOWN); other <- other[!startsWith(other, PROJECT_LOCK) & !startsWith(other, OUTPUT_REVIEW) & !startsWith(other, RUN_JOURNAL) & !startsWith(other, ".ovid_new_")]
    ovid <- any(dir.exists(file.path(p, c("config", "output"))))
    if (length(other) && !ovid) project_stop("The project folder ", tilde(path_norm(pd)), " already holds other files (", paste(utils::head(other, 3), collapse = ", "),
                                               if (length(other) > 3) ", ..." else "", "). Choose an empty folder, or one made by Ovid.")
  }
  # every folder a run writes to must be a folder and writable (a read-only copy, a locked drive, a file where a folder goes)
  if (dir.exists(p)) for (d in c(".", "output", "cache", "config")) { f <- if (d == ".") p else file.path(p, d)
    if (d != "." && file.exists(f) && !dir.exists(f)) project_stop("In the project folder ", tilde(path_norm(pd)), ", ", d, " is a file, not a folder. ",
                                                                   "Move or rename that file, then run again.")
    if (dir.exists(f) && file.access(f, 2) != 0) project_stop(if (d == ".") paste0("The project folder ", tilde(path_norm(pd))) else paste0("The folder ", tilde(path_norm(f))),
      " cannot be written to (read-only, or on a locked or full drive). Nothing was changed; make it writable or choose another project folder.") }
  else if (file.exists(p)) project_stop("The project folder ", tilde(path_norm(pd)), " is a file, not a folder. Choose a folder.")
  invisible(TRUE)
}
# ---- one run at a time: the project lock -------------------------------------------------------------------------------------
# The lock is the folder .ovid_lock holding run.txt (pid, host, start time, a token). It is written in full under another name
# and renamed into place, so it never exists without its owner, and a rename onto a lock already there fails: one process
# wins. A lock is taken over only when its owner is known to have gone: a process on this computer that has ended (its start
# time is compared where the system tells it, so a reused pid does not count). A lock from another computer (a shared drive)
# or one that cannot be read is never taken over: the run stops and says how to clear it. A live run keeps its lock however
# long it runs. A takeover is done by one process at a time (.ovid_lock_takeover) and only of the very lock judged gone; the
# lock is released only by the run that holds it (its token), so a former owner's late clean-up never removes a new lock.
# (An empty .ovid_lock folder, which only an older version could leave, is replaced by the rename.)
LOCK_TAKEOVER <- ".ovid_lock_takeover"; LOCK_TAKEOVER_STALE <- 60
pid_alive <- function(pid) if (.Platform$OS.type == "windows") { if (requireNamespace("ps", quietly = TRUE)) pid %in% ps::ps_pids() else TRUE } else isTRUE(tools::pskill(pid, 0L))
pid_started <- function(pid = Sys.getpid()) {                       # a process's start time (seconds), "" when the system cannot say
  if (!requireNamespace("ps", quietly = TRUE)) return("")
  # safe: with no start time a lock is judged by its pid alone
  tryCatch(format(as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(pid)))), nsmall = 2), error = function(e) "")
}
lock_read <- function(dir) {                                         # list(info lines, field getter); info is character() when unreadable
  # safe: an unreadable lock is treated as held by someone unknown
  info <- tryCatch(readLines(file.path(dir, "run.txt"), warn = FALSE), error = function(e) character())
  list(info = info, get = function(k) sub(paste0("^", k, ": *"), "", grep(paste0("^", k, ":"), info, value = TRUE)[1] %|% ""))
}
# "none", "live" (its run is going), "gone" (its process on this computer has ended) or "unknown" (another computer, or unreadable)
lock_state <- function(dir) {
  if (!dir.exists(dir)) return(list(state = "none"))
  l <- lock_read(dir); g <- l$get; pid <- suppressWarnings(as.integer(g("pid")))
  st <- if (!length(l$info) || is.na(pid) || !nzchar(g("host"))) "unknown" else if (!identical(g("host"), Sys.info()[["nodename"]])) "unknown"
        else if (!pid_alive(pid)) "gone" else if (nzchar(g("pid_start")) && nzchar(now <- pid_started(pid)) && abs(as.numeric(now) - as.numeric(g("pid_start"))) > 1) "gone"
        else "live"
  list(state = st, started = g("started"), host = g("host"), id = paste(l$info, collapse = "\n"))
}
# The run holding the project's lock (started, host), or NULL when there is none or its owner on this computer has gone.
lock_holder <- function(project_dir) {
  s <- lock_state(file.path(path.expand(project_dir), PROJECT_LOCK))
  if (s$state %in% c("live", "unknown")) list(started = s$started %|% "unknown", host = s$host %|% "unknown", state = s$state) else NULL
}
lock_publish <- function(p, to, info) {                              # TRUE when `to` was created holding info (atomic rename)
  tmp <- file.path(p, paste0(PROJECT_LOCK, "_new_", info[["token"]]))
  if (!dir.create(tmp, showWarnings = FALSE)) return(FALSE)
  ok <- tryCatch({ writeLines(paste0(names(info), ": ", info), file.path(tmp, "run.txt")); TRUE }, error = function(e) FALSE) &&
        file_move(tmp, to)
  if (!ok) unlink(tmp, recursive = TRUE)
  ok && identical(lock_read(to)$get("token"), info[["token"]])
}
lock_retire <- function(p, dir, token) {                             # moves a lock aside under a unique name, then removes it
  gone <- file.path(p, paste0(PROJECT_LOCK, "_gone_", token))
  ok <- file_move(dir, gone); if (ok) unlink(gone, recursive = TRUE); ok
}
lock_stop <- function(p, s) project_stop(
  if (s$state == "live") paste0("Another run is going on this project (started ", s$started, " on ", s$host, "). Wait for it to end.")
  else paste0("The project is locked by a run ", if (nzchar(s$host %||% "")) paste0("on ", s$host, " (started ", s$started %|% "unknown", ")") else "that left no details",
              ", and this computer cannot tell whether it is still going. If you are sure no run is going on it, delete the folder ",
              tilde(file.path(path_norm(p), PROJECT_LOCK)), " and run again."))
project_lock <- function(cfg) {
  p <- path.expand(cfg$project_dir); dir.create(p, recursive = TRUE, showWarnings = FALSE)
  mk <- file.path(p, PROJECT_MARK); app <- sub("^.*// *", "", if (exists("APP_NAME")) APP_NAME else "app")
  # safe: the marker file is a convenience; the project works without it
  if (!file.exists(mk) && !nzchar(Sys.readlink(mk) %|% "")) try(write_new(mk, c(paste0("app: ", app), "This folder is an Ovid project. Raw data is never kept here.")), silent = TRUE)
  lk <- file.path(p, PROJECT_LOCK); tok <- paste0(Sys.getpid(), "_", format(as.numeric(Sys.time()) * 1e6, scientific = FALSE), "_", sample.int(1e9, 1))
  me <- c(pid = Sys.getpid(), host = Sys.info()[["nodename"]], started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), pid_start = pid_started(), token = tok)
  for (attempt in 1:3) {
    if (lock_publish(p, lk, me)) return(invisible(structure(lk, token = tok)))
    s <- lock_state(lk)
    if (s$state == "none") next                                       # released between the two looks: try again
    if (s$state != "gone") lock_stop(p, s)
    tk <- file.path(p, LOCK_TAKEOVER)                                 # one takeover at a time, and only of the lock judged gone
    if (dir.exists(tk) && as.numeric(difftime(Sys.time(), file.mtime(tk), units = "secs")) > LOCK_TAKEOVER_STALE) lock_retire(p, tk, paste0("t", tok))
    if (!dir.create(tk, showWarnings = FALSE)) { Sys.sleep(0.2); next }
    taken <- tryCatch({ s2 <- lock_state(lk); if (s2$state == "gone" && identical(s2$id, s$id)) lock_retire(p, lk, tok) else FALSE },
                      finally = unlink(tk, recursive = TRUE))
    if (!taken && lock_state(lk)$state %in% c("live", "unknown")) lock_stop(p, lock_state(lk))
  }
  s <- lock_state(lk); if (s$state %in% c("live", "unknown")) lock_stop(p, s)
  project_stop("The project lock could not be taken (", tilde(lk), "). Nothing was changed; run again.")
}
# Releases the lock only when it is still this run's (token); TRUE when released.
project_unlock <- function(lk) {
  tok <- attr(lk, "token"); if (is.null(tok) || !dir.exists(lk)) return(invisible(FALSE))
  if (!identical(lock_read(lk)$get("token"), tok)) return(invisible(FALSE))
  invisible(lock_retire(dirname(lk), lk, tok))
}
lock_owned <- function(lk) !is.null(attr(lk, "token")) && identical(lock_read(lk)$get("token"), attr(lk, "token"))
# Output and set-up files another program holds open (Excel on Windows locks a CSV it shows) or that may not be written:
# the run would stop part way, so it does not start. Returns them under the project folder.
files_locked <- function(project_dir) {
  p <- path.expand(project_dir); f <- list.files(file.path(p, c("output", "config")), pattern = "\\.(csv|xlsx|xls|rds|html|pdf|nc|json)$", recursive = TRUE, full.names = TRUE)
  f <- f[!grepl("/history/", f, fixed = TRUE)]
  # safe: this tests whether each file can be written; one that cannot be opened is the answer
  bad <- f[!vapply(f, function(x) { con <- tryCatch(suppressWarnings(file(x, "ab")), error = function(e) NULL); if (is.null(con)) FALSE else { close(con); TRUE } }, NA)]
  sub(paste0("^", rx_escape(normalizePath(p, winslash = "/", mustWork = FALSE)), "/?"), "", normalizePath(bad, winslash = "/", mustWork = FALSE))
}
# output/.run_incomplete stays when a run stops or is cancelled part way (a run updates its outputs in place, so some may then
# be from before it); the app says so until a run finishes. run_incomplete() reads it.
RUN_INCOMPLETE <- ".run_incomplete"
run_incomplete <- function(project_dir) { f <- file.path(path.expand(project_dir %||% ""), "output", RUN_INCOMPLETE)
  if (nzchar(project_dir %||% "") && file.exists(f)) paste(readLines(f, warn = FALSE), collapse = " ") else "" }
# ---- folders a cloud service syncs -----------------------------------------------------------------------------------------
# A project folder that iCloud, OneDrive, Dropbox or Google Drive syncs while a run writes to it can end up with duplicates
# ("summary 2.csv"), files held open, or outputs kept online only; raw files kept online only are downloaded as the run reads
# them (slow, and failing without a connection). Not stopped: said at the start of the run and in its log.
cloud_service <- function(path) {                                   # the service syncing `path`, or "" (by the folders each one uses)
  p <- path_norm(path)
  m <- regmatches(p, regexpr("(?i)/(Library/Mobile Documents|Library/CloudStorage/[^/]+|iCloud ?Drive|OneDrive[^/]*|Dropbox[^/]*|Google ?Drive[^/]*|My Drive|Box( Sync)?)(/|$)", p, perl = TRUE))
  if (!length(m)) return("")
  m <- sub("^/(Library/CloudStorage/)?", "", sub("/$", "", m))
  if (grepl("^Library/Mobile Documents$|^iCloud", m, ignore.case = TRUE)) "iCloud Drive" else if (grepl("^OneDrive", m, ignore.case = TRUE)) "OneDrive"
  else if (grepl("^Dropbox", m, ignore.case = TRUE)) "Dropbox" else if (grepl("^(Google|My Drive)", m, ignore.case = TRUE)) "Google Drive" else if (grepl("^Box", m)) "Box" else sub("-.*$", "", m)
}
online_only <- function(dir) {                                      # files under `dir` kept online only (a Mac's file providers mark them "dataless")
  if (Sys.info()[["sysname"]] != "Darwin" || !dir.exists(dir)) return(0L)
  # safe: a count for a note only
  length(tryCatch(suppressWarnings(system2("find", c(shQuote(path.expand(dir)), "-type", "f", "-flags", "+dataless"), stdout = TRUE, stderr = FALSE)), error = function(e) character()))
}
cloud_notes <- function(cfg) {
  out <- character(); pd <- as.character(cfg$project_dir %||% "")[1]
  sv <- if (nzchar(pd %|% "")) cloud_service(pd) else ""
  if (nzchar(sv)) out <- c(out, paste0("The project folder is in ", sv, ", which syncs it while the run writes: files can be duplicated (\"summary 2.csv\"), held open or kept online only. ",
                                       "A folder outside ", sv, " is safer; copy the results there when the run ends."))
  raw <- as.character(unlist(cfg$raw_dirs)); raw <- raw[!is.na(raw) & nzchar(trim(raw))]
  for (r in raw) { sv <- cloud_service(r); if (!nzchar(sv)) next; n <- online_only(r)
    out <- c(out, paste0("The raw folder ", tilde(path_norm(r)), " is in ", sv, if (n) paste0("; ", n, " of its files are kept online only and are downloaded as the run reads them (slow, and failing without a connection). ",
                                                                             "Keep the folder downloaded (", if (sv == "iCloud Drive") "Keep Downloaded" else "Available offline", " on the folder) before a run.")
                         else ". Keep it downloaded, so the run does not wait for files to come from the cloud.")) }
  out
}

RUN_RESTORED <- ".run_restored"
run_restored <- function(project_dir) { f <- file.path(path.expand(project_dir %||% ""), "output", RUN_RESTORED)
  if (nzchar(project_dir %||% "") && file.exists(f)) paste(readLines(f, warn = FALSE), collapse = " ") else "" }

# ---- the last finished output, kept while a run goes -----------------------------------------------------------------------
# A run updates output/ in place, so before it starts output/ is copied (on a Mac's own disk, APFS, as a clone: instant and
# taking no space until files change). Each run has an id, and the journal <project>/.ovid_run (written whole and renamed into
# place) says how far it got:
#   backup     the copy is being made in .output_before_run.part; output/ is not touched yet;
#   verified   the copy holds every file of output/ at its size and is now .output_before_run (its .ovid_kept names the run,
#              file count and bytes); only now does the run change output/;
#   committed  the run finished; its output stands and the copy is only waiting to be removed.
# A run that fails, is cancelled or stops to ask a question puts the copy back, so the output is again what the last finished
# run left, with the stopped run's log added; the cached results it made are dropped (they would let the next run skip outputs
# that were put back) and output/.run_restored says what happened until a run finishes. A run whose process was killed is
# recovered under the lock before the next run or when the app starts (output_recover): a copy is put back only when it is
# verified, belongs to that run and the run did not commit; a partial copy is never used; a committed run's copy is removed.
# When putting back fails, both copies stay and the next try goes on from there. A copy left by an older version (no journal)
# is not put back: it is kept beside the output as .output_to_review_<time> and the note says so. Set-up tables a stopped run
# changed (pipeline additions such as CS's precip_gauges.csv; each also kept in config/history) are not put back: the note
# names them. When output/ holds a finished run's results and no copy can be made (too large, not enough space, a failed copy)
# the run does not start, so that output is never overwritten with no way back.
RUN_PHASES <- c("backup", "verified", "restoring", "committed")
RUN_JOURNAL <- ".ovid_run"; OUTPUT_STAGING <- ".output_before_run.part"; OUTPUT_REVIEW <- ".output_to_review_"
OUTPUT_COPY_MAX <- 2e9; DISK_MIN <- 500e6
fmt_bytes <- function(b) ifelse(is.na(b), "?", ifelse(b >= 1e9, sprintf("%.1f GB", b / 1e9), sprintf("%.0f MB", b / 1e6)))
dir_bytes <- function(d) { f <- list.files(d, recursive = TRUE, full.names = TRUE, all.files = TRUE); if (length(f)) sum(file.info(f)$size, na.rm = TRUE) else 0 }
disk_free <- function(path) {                                       # bytes free on the drive of `path` (or of its nearest existing parent); NA when unknown
  if (identical(R.version$os, "emscripten")) return(NA_real_)          # R in the browser (webR): no disk to ask about
  p <- path_norm(path); while (!dir.exists(p) && dirname(p) != p) p <- dirname(p)
  # safe: free space is for a note only; unknown when the system cannot say
  if (requireNamespace("ps", quietly = TRUE)) { v <- tryCatch(as.numeric(ps::ps_disk_usage(p)$available[1]), error = function(e) NA_real_); if (isTRUE(v >= 0)) return(v) }
  if (.Platform$OS.type == "windows") return(NA_real_)
  # safe: free space is for a note only; unknown when the system cannot say
  x <- tryCatch(suppressWarnings(system2("df", c("-Pk", shQuote(p)), stdout = TRUE, stderr = FALSE)), error = function(e) character())
  v <- if (length(x) >= 2) suppressWarnings(as.numeric(strsplit(trimws(x[length(x)]), "\\s+")[[1]][4])) else NA_real_
  v * 1024
}
disk_clones <- function(path) {                                     # TRUE on a Mac's APFS disk, where a copy is a clone
  if (Sys.info()[["sysname"]] != "Darwin") return(FALSE)
  # safe: the disk type is for a note only; unknown when the system cannot say
  x <- tryCatch(suppressWarnings(system2("df", c("-P", shQuote(path_norm(path))), stdout = TRUE, stderr = FALSE)), error = function(e) character())
  if (length(x) < 2) return(FALSE); mp <- sub("^.*%\\s+", "", x[length(x)])
  # safe: the disk type is for a note only; unknown when the system cannot say
  m <- tryCatch(suppressWarnings(system2("mount", stdout = TRUE, stderr = FALSE)), error = function(e) character())
  any(startsWith(m, "/") & grepl(paste0(" on ", mp, " (apfs"), m, fixed = TRUE))
}
disk_check <- function(project_dir) {
  free <- disk_free(project_dir)
  if (isTRUE(free < DISK_MIN)) project_stop("Only ", fmt_bytes(free), " is free on the drive of the project folder ", tilde(path_norm(project_dir)),
                                            "; a run needs at least ", fmt_bytes(DISK_MIN), ". Nothing was changed: free some space and run again.")
  free
}
# A test may stop the process at a named point (OVID_TEST_KILL_AT, set by the test's child process itself), to check what the
# next start makes of it; OVID_TEST_KILL_MARK names a file written first, so the test knows the point was reached.
run_checkpoint <- function(name) if (identical(Sys.getenv("OVID_TEST_KILL_AT"), name)) {
  mk <- Sys.getenv("OVID_TEST_KILL_MARK"); if (nzchar(mk)) writeLines(name, mk)
  tools::pskill(Sys.getpid(), if (is.na(tools::SIGKILL)) tools::SIGTERM else tools::SIGKILL) }   # Windows has no SIGKILL (NA, which kills nothing); its SIGTERM is TerminateProcess
journal_read <- function(pd) { f <- file.path(pd, RUN_JOURNAL); if (!file.exists(f)) return(NULL)
  # safe: a journal that cannot be read is treated as missing (the copy, if any, is then kept for review)
  x <- tryCatch(readLines(f, warn = FALSE), error = function(e) character()); if (!length(x)) return(NULL)
  k <- sub(":.*$", "", x)                                                         # every line "key: value", each key once; else marked invalid
  if (!all(grepl("^[a-z0-9_]+: ?", x)) || anyDuplicated(k)) return(list(invalid = TRUE))
  as.list(setNames(sub("^[^:]*: *", "", x), k)) }
journal_write <- function(pd, run, phase, ...) {
  f <- file.path(pd, RUN_JOURNAL); x <- c(run = run, phase = phase, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ...)
  if (!write_new(f, paste0(names(x), ": ", x))) project_stop("The run journal ", tilde(f), " could not be written. Nothing was changed; check that the project folder is writable.")
  invisible(x) }
# A small file written without opening any path that is already there: the lines go into a new file inside a folder this call
# makes (mkdir fails on anything existing, a link included), which is then renamed onto `path` (a link there is replaced, never
# written through). TRUE when done.
write_new <- function(path, lines) {
  tmp <- file.path(dirname(path), paste0(".ovid_new_", Sys.getpid(), "_", sample.int(1e9, 1)))
  if (!dir.create(tmp, showWarnings = FALSE)) return(FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  ok <- tryCatch({ writeLines(lines, file.path(tmp, "f")); TRUE }, error = function(e) FALSE)
  ok && file_move(file.path(tmp, "f"), path)
}
journal_clear <- function(pd) unlink(file.path(pd, c(RUN_JOURNAL, paste0(RUN_JOURNAL, ".part"))))
KEPT_FILES <- c(".ovid_kept", "._.ovid_kept", ".ovid_kept_files", "._.ovid_kept_files")
OUT_NOTES <- c(".run_restored", ".run_incomplete")                                # status notes in output/: never part of a copy's record
out_inventory <- function(d, hash = FALSE) { f <- sort(list.files(d, recursive = TRUE, all.files = TRUE)); f <- f[!grepl("^\\._", basename(f)) & !basename(f) %in% KEPT_FILES & !f %in% OUT_NOTES]
  list(n = length(f), bytes = if (length(f)) sum(file.size(file.path(d, f)), na.rm = TRUE) else 0, files = f, sizes = file.size(file.path(d, f)),
       hashes = if (hash) vapply(file.path(d, f), function(x) digest::digest(file = x, algo = "xxhash64"), "", USE.NAMES = FALSE)) }   # xxHash: fast enough for every run
cfg_inventory <- function(pd) { f <- list.files(file.path(pd, "config"), pattern = "\\.csv$", full.names = TRUE)   # set-up tables (not history)
  setNames(unname(tools::md5sum(f)), basename(f)) }
kept_info <- function(kept) { x <- tryCatch(readLines(file.path(kept, ".ovid_kept"), warn = FALSE), error = function(e) character())
  as.list(setNames(sub("^[^:]*: *", "", x), sub(":.*$", "", x))) }
# The copy is the one recorded for this run, file by file: names, sizes and content hashes (.ovid_kept_files), checked when it
# is made and again before it is put back, so a same-size change is found too.
# extra: files a restore adds itself (the stopped run's logs, the note), left out of the comparison when not in the record.
kept_verified <- function(kept, run, extra = character()) { k <- kept_info(kept); if (!identical(k$run, run)) return(FALSE)
  rec <- tryCatch(fread(file = file.path(kept, ".ovid_kept_files"), colClasses = "character", sep = "\t", na.strings = NULL), error = function(e) NULL)
  if (is.null(rec) || !all(c("file", "size", "hash") %in% names(rec))) return(FALSE)
  rec <- rec[!file %in% OUT_NOTES]                                                # a record made before the notes were left out of it (status, not data)
  inv <- out_inventory(kept, hash = TRUE); own <- !(inv$files %in% setdiff(extra, rec$file))
  inv <- list(files = inv$files[own], sizes = inv$sizes[own], hashes = inv$hashes[own])
  identical(rec$file, inv$files) && identical(as.numeric(rec$size), as.numeric(inv$sizes)) && identical(rec$hash, inv$hashes) }
# TRUE when a verified copy is kept (journal: verified); a character string when not (why). run: the run's id.
output_keep <- function(project_dir, free = disk_free(project_dir), run = run_new_id()) {
  pd <- path.expand(project_dir); out <- file.path(pd, "output"); kept <- file.path(pd, OUTPUT_KEPT); stage <- file.path(pd, OUTPUT_STAGING)
  remove_within(stage, pd, recursive = TRUE)
  if (dir.exists(kept)) return("a copy from an earlier run is still there")
  if (!dir.exists(out) || !length(list.files(out, recursive = TRUE, all.files = TRUE))) return("no output yet")
  clone <- disk_clones(pd); size <- dir_bytes(out)
  lk <- path_links(out)                                                           # a linked file's bytes are not the copy's: a restore could not undo a change to it
  if (length(lk)) return(structure(paste0("output/ holds links to files elsewhere (", paste(utils::head(substring(names(lk), nchar(out) + 2L), 3), collapse = ", "),
                                          "), whose contents a copy cannot keep"), action = "Replace those links with files of their own, then run again."))
  if (!clone && size > OUTPUT_COPY_MAX) return(structure(paste0("the output is ", fmt_bytes(size), " and this drive cannot clone files, so each run would copy it in full; ",
                                                                "the limit for a full copy is ", fmt_bytes(OUTPUT_COPY_MAX), ", whatever the free space"),
    action = "Move the project folder to a drive that clones files (a Mac's own APFS disk), where the copy takes no space, or make the output smaller (fewer outputs or a project split by years)."))
  if (!clone && isTRUE(free < 2 * size + DISK_MIN)) return(structure(paste0("a full copy of the output (", fmt_bytes(size), ") needs ", fmt_bytes(2 * size + DISK_MIN),
                                                                            " free on the drive, and ", fmt_bytes(free), " is free"),
    action = "Free space on the drive of the project folder, or move the project to a drive with room."))
  journal_write(pd, run, "backup", config = paste(names(c0 <- cfg_inventory(pd)), c0, sep = "=", collapse = ";"))
  ok <- tryCatch(if (.Platform$OS.type == "windows") { dir.create(stage)
          all(file.copy(list.files(out, full.names = TRUE, all.files = TRUE, no.. = TRUE), stage, recursive = TRUE, copy.date = TRUE)) }
        else { flags <- if (clone) "-cpR" else if (Sys.info()[["sysname"]] == "Linux") c("-pR", "--reflink=auto") else "-pR"
          identical(suppressWarnings(system2("cp", c(flags, shQuote(out), shQuote(stage)), stdout = FALSE, stderr = FALSE)), 0L) },
        error = function(e) FALSE)
  run_checkpoint("backup_copy")
  a <- out_inventory(out, hash = TRUE); b <- if (isTRUE(ok)) out_inventory(stage, hash = TRUE) else NULL
  same <- isTRUE(ok) && identical(a$files, b$files) && identical(a$sizes, b$sizes) && identical(a$hashes, b$hashes) && !length(path_links(stage))
  if (same) same <- tryCatch({ fwrite(data.table(file = b$files, size = b$sizes, hash = b$hashes), file.path(stage, ".ovid_kept_files"), sep = "\t")
    writeLines(c(paste0("run: ", run), paste0("time: ", as.numeric(Sys.time())), paste0("files: ", b$n), paste0("bytes: ", b$bytes)), file.path(stage, ".ovid_kept"))
    kept_verified(stage, run) && file_move(stage, kept) }, error = function(e) FALSE)
  if (!same) { remove_within(stage, pd, recursive = TRUE); journal_clear(pd)
    return(structure("the output could not be copied, or the copy did not match it file by file",
                     action = "Check that the drive is not full, locked or read-only, and that no other program is changing the output, then run again.")) }
  journal_write(pd, run, "verified", config = journal_read(pd)$config %||% "")
  TRUE
}
run_new_id <- function() paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "_", Sys.getpid(), "_", sample.int(1e6, 1))
# Puts the verified copy back (the caller has checked it belongs to the interrupted run). Resumable: the journal says
# "restoring" before anything moves, and each step is done only if its state shows it is still to do, so a process killed after
# either rename finishes the work on the next try:
#   1. output/ -> .output_failed (skipped when already moved, or when there is no output/);
#   2. .output_before_run -> output/ (the copy keeps its .ovid_kept until the end, so a published copy is recognised);
#   3. the stopped run's logs copied over, .output_failed removed, the cache it made dropped, the note written.
# Returns TRUE, or a string saying what stops it (a file held open, or a state that is not this restore's: both kept).
# keep: cache sub-folders an app marks as holding only what was read from its inputs (CACHE_INPUT_ONLY, e.g. RS "sources").
output_restore <- function(project_dir, why = "stopped", keep = get0("CACHE_INPUT_ONLY", ifnotfound = character())) {
  pd <- path.expand(project_dir); out <- file.path(pd, "output"); kept <- file.path(pd, OUTPUT_KEPT); failed <- file.path(pd, OUTPUT_FAILED)
  j <- journal_read(pd); run <- j$run %||% ""
  if (!identical(j$phase, "restoring")) journal_write(pd, run, "restoring", config = j$config %||% "", why = why) else why <- j$why %||% why
  mv <- function(a, b) file_move(a, b)
  if (dir.exists(kept) && dir.exists(out) && !dir.exists(failed) && !mv(out, failed))
    return(paste0("output/ could not be moved aside (a file in it is held open by another program?); close it and run again"))
  run_checkpoint("restore_1")
  if (dir.exists(kept) && !dir.exists(out) && !mv(kept, out)) {
    if (dir.exists(failed)) mv(failed, out)
    return(paste0("the copy ", OUTPUT_KEPT, " could not be moved into output/ (a file held open?); close it and run again")) }
  run_checkpoint("restore_2")
  # The copy is in output/: its recorded bytes are checked on every attempt before anything is removed. What the clean-up adds
  # itself is known from the journal, written before it starts (the stopped run's log names and md5s), so an earlier attempt's
  # additions (a log, its .part file) never count against the copy, even after .output_failed is gone; the notes
  # (.run_restored, .run_incomplete) are status, never part of the record.
  if (dir.exists(kept) || !identical(kept_info(out)$run, run)) return("ambiguous")
  if (is.null(j$cleanup)) {                                                       # first time here: what the clean-up will add, recorded before it is added
    lg <- list.files(file.path(failed, "logs"), pattern = "\\.log$", full.names = TRUE)
    rd <- kept_record(out); hl <- vapply(lg, function(x) digest::digest(file = x, algo = "xxhash64"), "", USE.NAMES = FALSE)
    same <- file.path("logs", basename(lg)) %in% rd$file & hl == rd$hash[match(file.path("logs", basename(lg)), rd$file)]
    lg <- lg[!same]                                                               # logs the last finished run left, unchanged: already in the copy
    taken <- c(rd$file, file.path("logs", list.files(file.path(out, "logs"), all.files = TRUE, no.. = TRUE)))
    to <- log_targets(basename(lg), taken)                                        # every final and .part name reserved: nothing there is replaced
    j <- as.list(journal_write(pd, run, "restoring", config = j$config %||% "", why = why, cleanup = "yes",
                               logs = paste(basename(lg), collapse = "|"), logs_to = paste(to, collapse = "|"), logs_md5 = paste(unname(tools::md5sum(lg)), collapse = "|"))) }
  lr <- log_record(j)
  if (is.null(lr)) return("invalid_logs")                                         # names or checksums not as this restore writes them: nothing is used
  added <- if (length(lr$to)) c(file.path("logs", lr$to), file.path("logs", paste0(lr$to, ".part"))) else character()
  if (!kept_verified(out, run, extra = added)) return("mismatch")
  since <- suppressWarnings(as.numeric(kept_info(out)$time %||% NA)); if (is.na(since)) since <- as.numeric(file.mtime(out))
  if (length(lr$to)) { dir.create(file.path(out, "logs"), showWarnings = FALSE)
    to <- in_dir(file.path(out, "logs"), lr$to); from <- in_dir(file.path(failed, "logs"), lr$from)
    m <- unname(tools::md5sum(to)); need <- is.na(m) | m != lr$md5
    if (any(need) && all(file.exists(from[need]))) log_copy(from[need], to[need])
    if (!identical(unname(tools::md5sum(to)), lr$md5))                            # the whole log must be published before its original goes
      return(paste0("cleanup: the output is put back, but the stopped run's log could not be copied into output/logs",
                    if (dir.exists(failed)) paste0("; it is kept in ", OUTPUT_FAILED, " (", paste(lr$from, collapse = ", "), ") and copying is tried again before the next run")
                    else "; its original is no longer there, so it could not be copied again"))
    unlink(in_dir(file.path(out, "logs"), paste0(lr$to, ".part"))) }              # only this restore's own staging files
  if (dir.exists(failed)) {                                                       # retired only now, the stopped run's log safely published
    rm_err <- tryCatch({ remove_within(failed, pd, recursive = TRUE); "" }, error = function(e) conditionMessage(e))
    if (dir.exists(failed)) return(paste0("cleanup: the output is put back and the stopped run's log is in output/logs, but ", tilde(failed),
                                          " (the stopped run's own output) could not be removed", if (nzchar(rm_err)) paste0(" (", rm_err, ")"),
                                          "; it is kept and removing it is tried again before the next run")) }
  run_checkpoint("restore_3")
  cf <- list.files(file.path(pd, "cache"), recursive = TRUE, full.names = FALSE, all.files = TRUE)
  cf <- file.path(pd, "cache", cf[!sub("/.*", "", cf) %in% keep])
  remove_within(cf[as.numeric(file.mtime(cf)) >= since - 2], pd)
  writeLines(paste0("The run of ", format(as.POSIXct(since, origin = "1970-01-01"), "%Y-%m-%d %H:%M"), " ", why,
                    "; the output was put back as the run before it left it.", cfg_changed_note(pd)), file.path(out, RUN_RESTORED))
  unlink(file.path(out, KEPT_FILES))
  TRUE
}
# The stopped run's logs as the journal records them, or NULL when the record is not one this restore writes: plain unique
# file names ending .log (no folder part, no "..", no separators), as many names, targets and MD5 digests.
log_record <- function(j) {
  sp <- function(x) if (is.null(x) || !nzchar(x)) character() else strsplit(x, "|", fixed = TRUE)[[1]]
  from <- sp(j$logs); to <- sp(j$logs_to %||% j$logs); md5 <- sp(j$logs_md5)
  plain <- function(x) all(grepl("^[A-Za-z0-9_][A-Za-z0-9_. -]*\\.log$", x)) && !any(grepl("\\.\\.", x)) && !anyDuplicated(x)
  if (length(from) != length(to) || length(from) != length(md5) || !plain(from) || !plain(to) || !all(grepl("^[0-9a-f]{32}$", md5))) return(NULL)
  list(from = from, to = to, md5 = md5)
}
# where each stopped-run log goes in output/logs: its own name, or name_stopped_run[_2...].log when the copy already holds that name
log_targets <- function(names, taken) { taken <- tolower(taken); out <- character()      # letter case ignored (macOS and Windows disks)
  for (n in names) { cand <- n; k <- 1L
    while (any(tolower(file.path("logs", c(cand, paste0(cand, ".part")))) %in% taken)) { cand <- sub("\\.log$", paste0("_stopped_run", if (k > 1) paste0("_", k), ".log"), n); k <- k + 1L }
    out <- c(out, cand); taken <- c(taken, tolower(file.path("logs", c(cand, paste0(cand, ".part"))))) }
  out }
# paths under dir only: each name must resolve to a file directly in dir, else the restore stops (checked where files are used)
in_dir <- function(dir, names) { p <- file.path(dir, names); d <- tolower(path_norm(dir))
  if (any(tolower(vapply(dirname(p), path_norm, "")) != d)) stop(structure(class = c("ovid_project_check", "error", "condition"),
    list(call = NULL, message = paste0("A log name in the run journal leads outside ", tilde(dir), "; nothing was copied or removed."))))
  p }
kept_record <- function(kept) tryCatch(fread(file = file.path(kept, ".ovid_kept_files"), colClasses = "character", sep = "\t", na.strings = NULL), error = function(e) data.table(file = character()))
log_copy <- function(from, to) for (k in seq_along(from)) { tmp <- paste0(to[k], ".part"); if (isTRUE(file.copy(from[k], tmp, overwrite = TRUE, copy.date = TRUE))) file_move(tmp, to[k]) else unlink(tmp) }
cfg_changed_note <- function(pd) {                                   # set-up tables the stopped run changed (kept, not put back)
  j <- journal_read(pd); if (is.null(j$config)) return("")
  b <- strsplit(strsplit(j$config, ";", fixed = TRUE)[[1]], "=", fixed = TRUE); b <- setNames(vapply(b, `[`, "", 2), vapply(b, `[`, "", 1))
  now <- cfg_inventory(pd); ch <- union(names(now)[is.na(b[names(now)]) | b[names(now)] != now], setdiff(names(b), names(now)))
  if (!length(ch)) "" else paste0(" The stopped run changed these set-up tables, which were kept as they are (earlier versions in config/history): ", paste(ch, collapse = ", "), ".")
}
note_restored <- function(pd, text) { if (dir.exists(file.path(pd, "output"))) writeLines(text, file.path(pd, "output", RUN_RESTORED)); text }   # never makes output/ (a restore may be half done)
# Under the lock, before a run or when the app starts: what an interrupted run left, made safe. Returns a note for the user, or "".
run_recover <- function(pd, why = "did not finish (the app or computer stopped)") {
  kept <- file.path(pd, OUTPUT_KEPT); j <- journal_read(pd)
  if (dir.exists(file.path(pd, OUTPUT_STAGING))) remove_within(file.path(pd, OUTPUT_STAGING), pd, recursive = TRUE)   # a copy never finished: never used
  review <- function(what) { to <- file.path(pd, paste0(OUTPUT_REVIEW, format(Sys.time(), "%Y%m%dT%H%M%S")))
    if (!file_move(kept, to)) return(note_restored(pd, paste0("A copy of the output from before an earlier run (", OUTPUT_KEPT, ") could not be moved aside; it was left as it is.")))
    journal_clear(pd)
    note_restored(pd, paste0(what, " Both are kept: output/ as it is, and the copy in ", basename(to), ". Compare them, keep the right one as output/, and delete the other.")) }
  if (is.null(j)) return(if (dir.exists(kept)) review("A copy of the output from before an earlier run was found, but not whether that run finished.") else "")
  if (isTRUE(j$invalid) || !(j$phase %||% "") %in% RUN_PHASES || !grepl("^[0-9]{8}T[0-9]{6}_[0-9]+_[0-9]+$", j$run %||% "")) {   # an incomplete, repeated or unknown journal: nothing is moved
    if (dir.exists(kept)) return(review(paste0("The run journal (", RUN_JOURNAL, ") is incomplete or names a state this version does not know, so it is not safe to put anything back.")))
    journal_clear(pd)
    return(note_restored(pd, paste0("The run journal (", RUN_JOURNAL, ") was incomplete or named an unknown state; nothing was moved. Check that output/ is as you expect."))) }
  if (j$phase %in% c("committed", "backup") && dir.exists(kept) && !identical(kept_info(kept)$run, j$run))   # a copy this run did not make: never removed
    return(review(paste0("The copy of the output (", OUTPUT_KEPT, ") belongs to another run than the one the journal names, so it was not removed.")))
  if (identical(j$phase, "committed")) { if (dir.exists(kept)) remove_within(kept, pd, recursive = TRUE); journal_clear(pd); return("") }
  if (identical(j$phase, "backup")) { if (dir.exists(kept)) remove_within(kept, pd, recursive = TRUE); journal_clear(pd)      # output/ was not touched yet
    return("") }
  failed <- file.path(pd, OUTPUT_FAILED); out <- file.path(pd, "output")
  aside <- function(d) { to <- file.path(pd, paste0(OUTPUT_REVIEW, sub("^\\.", "", basename(d)), "_", format(Sys.time(), "%Y%m%dT%H%M%S")))
    if (file_move(d, to)) basename(to) else NA_character_ }
  if (!identical(j$phase, "restoring")) {
    if (!dir.exists(kept)) { journal_clear(pd); return("") }                       # a first run (nothing to put back): output/.run_incomplete says so
    if (!kept_verified(kept, j$run)) return(review("The copy of the output kept before the last run does not match its record."))
    if (dir.exists(failed)) { to <- aside(failed)                                  # not this restore's: kept for review, never removed
      if (is.na(to)) return(note_restored(pd, paste0("A folder ", OUTPUT_FAILED, " from an earlier attempt is in the way and could not be moved aside; ",
                                                     "the output was not put back. Move or delete it yourself, then run again.")))
      say("  ", OUTPUT_FAILED, " from an earlier attempt was kept as ", to, " for review", col = "orange") }
  } else if (dir.exists(kept) && !kept_verified(kept, j$run)) return(review("The copy of the output kept before the last run does not match its record."))
  r <- output_restore(pd, why)
  if (isTRUE(r)) { journal_clear(pd); return(run_restored(pd)) }
  if (identical(r, "mismatch")) {                                                  # the published copy is not the one recorded: keep both, report
    to <- if (dir.exists(failed)) aside(failed) else NA_character_; journal_clear(pd)
    return(note_restored(pd, paste0("Putting back the output after the last run stopped was interrupted, and the copy now in output/ does not match its record ",
                                    "(a file changed since it was made). It was not accepted as put back. Nothing was removed: output/ holds that copy",
                                    if (!is.na(to)) paste0(", and the stopped run's output is in ", to), ". Compare them, keep the right one as output/, and delete the other."))) }
  if (identical(r, "invalid_logs")) { journal_clear(pd)
    return(note_restored(pd, paste0("Putting back the output was interrupted, and its journal's record of the stopped run's logs is not valid (a name that is not a plain ",
                                    "file name, or names and checksums that do not match). Nothing was copied or removed: output/",
                                    if (dir.exists(failed)) paste0(" and ", OUTPUT_FAILED), " are kept. Check output/ and copy any log you need by hand."))) }
  if (startsWith(r, "cleanup: ")) return(note_restored(pd, paste0("The last run ", why, "; ", sub("^cleanup: ", "", r), ".")))
  if (identical(r, "ambiguous")) {                                                 # the folders are not where this restore left them: keep everything
    kept_now <- c(if (dir.exists(out)) "output/", if (dir.exists(kept)) OUTPUT_KEPT, if (dir.exists(failed)) OUTPUT_FAILED)
    journal_clear(pd)
    return(note_restored(pd, paste0("Putting back the output after the last run stopped was interrupted, and the folders are not as it left them. Nothing was removed: ",
                                    paste(kept_now, collapse = ", "), " are kept. Compare them, keep the right one as output/, and delete the others."))) }
  note_restored(pd, paste0("The last run ", why, ", and its output could not be put back yet: ", r, ". Both copies are kept (",
                           paste(c(if (dir.exists(out)) "output/", if (dir.exists(kept)) OUTPUT_KEPT, if (dir.exists(failed)) OUTPUT_FAILED), collapse = ", "),
                           "); it is tried again before the next run."))
}
# A run whose process was killed (and no run going now): made safe as above. Returns what output/.run_restored says, or "".
# cfg: the app's settings (raw folders and any app_protect_dirs()); the project and its links are checked against them under the
# lock before anything is changed, and a failed check is returned as the note, with nothing changed.
output_recover <- function(project_dir, cfg = list()) {
  pd <- path.expand(project_dir %||% ""); if (!nzchar(pd) || !any(file.exists(file.path(pd, c(OUTPUT_KEPT, OUTPUT_STAGING, RUN_JOURNAL, OUTPUT_FAILED))))) return("")
  cfg$project_dir <- pd; roots <- protected_roots(cfg)
  check <- function() tryCatch({ for (r in roots) if (path_overlap(pd, r)) project_stop("The project folder ", shown(pd), " overlaps the raw folder ", shown(r), ".")
    raw_boundary_check(pd, roots); "" }, ovid_project_check = function(e) conditionMessage(e))
  refused <- function(why) paste0("An earlier run did not finish, and its output was not put back: ", why)
  bad <- check(); if (nzchar(bad)) return(refused(bad))                            # before the lock: nothing is written until the folder is known safe
  lk <- tryCatch(project_lock(list(project_dir = pd)), ovid_project_check = function(e) NULL); if (is.null(lk)) return("")   # a run is going
  on.exit(project_unlock(lk))
  bad <- check(); if (nzchar(bad)) return(refused(bad))                            # again under the lock: nothing changed in between
  old <- options(ovid.raw_dirs = roots, ovid.project_root = pd); on.exit(options(old), add = TRUE)
  run_recover(pd)
}

# A change to a project outside a run (an app action: importing programs, removing a source): the same folder and link checks
# and the same lock as a run, with no output copy. Removals inside fun() are checked by remove_within().
project_mutate <- function(cfg, fun, need_raw = FALSE) {
  project_check(cfg, need_raw = need_raw)
  lk <- project_lock(cfg); on.exit(project_unlock(lk), add = TRUE)
  old <- options(ovid.raw_dirs = protected_roots(cfg), ovid.project_root = path.expand(cfg$project_dir)); on.exit(options(old), add = TRUE)
  raw_boundary_check(cfg$project_dir, protected_roots(cfg))                     # again under the lock
  fun()
}

# A page's write to a project (a set-up table saved in an editor, the project's settings): the project's own settings give
# its raw folders (with the app's CFG and app_protect_dirs()), then project_mutate()'s checks and lock. Errors carry the
# reason; nothing is written when the checks fail.
project_guarded <- function(project_dir, fun, raw = character()) {             # raw: more folders to protect (settings being saved)
  pd <- path.expand(project_dir); cfg <- tryCatch(project_settings_read(pd), error = function(e) list())
  cfg$raw_dirs <- unique(c(as.character(unlist(cfg$raw_dirs)), as.character(unlist(get0("CFG")$raw_dirs)), as.character(unlist(raw)))); cfg$project_dir <- pd
  project_mutate(cfg, fun)
}

# The last run's result, kept with the project (.ovid_last_run) so it can be shown after a reload or a project switch:
# status (finished, failed, cancelled, pending recovery), what happened to the output, and where the run's log is.
RUN_RESULT <- ".ovid_last_run"
run_result_write <- function(pd, status, note) {
  lg <- sort(list.files(file.path(pd, "output", "logs"), "\\.log$", full.names = TRUE), decreasing = TRUE)[1]
  try(write_new(file.path(pd, RUN_RESULT), c(paste0("status: ", status), paste0("time: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
                                             paste0("log: ", if (is.na(lg)) "" else lg), paste0("note: ", gsub("[\r\n]+", " ", note)))), silent = TRUE)   # safe: the record is extra
}
run_result <- function(project_dir) {                                  # list(status, time, log, note); a pending recovery wins over the record
  pd <- path.expand(project_dir %||% ""); f <- file.path(pd, RUN_RESULT)
  x <- if (file.exists(f)) tryCatch(readLines(f, warn = FALSE), error = function(e) character()) else character()
  r <- as.list(setNames(sub("^[^:]*: *", "", x), sub(":.*$", "", x)))
  if (nzchar(pd) && any(file.exists(file.path(pd, c(RUN_JOURNAL, OUTPUT_KEPT, OUTPUT_FAILED))))) {
    r$status <- "pending recovery"; r$note <- paste0("A run did not finish and its recovery is not done (", paste(basename(file.path(pd, c(RUN_JOURNAL, OUTPUT_KEPT, OUTPUT_FAILED)))[file.exists(file.path(pd, c(RUN_JOURNAL, OUTPUT_KEPT, OUTPUT_FAILED)))], collapse = ", "),
                                                     "): reopening the project or the next run finishes it.") }
  r
}
run_project <- function(cfg, fun, need_raw = TRUE) {                # need_raw = FALSE: an app that keeps the last scan of a drive that is away (RS)
  options(ovid.cancelled = NULL)
  project_check(cfg, need_raw = need_raw)
  lk <- project_lock(cfg); on.exit(project_unlock(lk), add = TRUE)
  old_opt <- options(ovid.raw_dirs = protected_roots(cfg), ovid.project_root = path.expand(cfg$project_dir))
  on.exit(options(old_opt), add = TRUE)
  held <- files_locked(cfg$project_dir)
  if (length(held)) project_stop("These files are open in another program or may not be written: ", paste(utils::head(held, 5), collapse = ", "),
                                 if (length(held) > 5) ", ..." else "", ". Close them (Excel locks a CSV it has open) and run again.")
  pd <- path.expand(cfg$project_dir)
  rec <- run_recover(pd); if (nzchar(rec)) say("  ", rec, col = "orange")
  if (dir.exists(file.path(pd, OUTPUT_KEPT)) || dir.exists(file.path(pd, OUTPUT_FAILED)))
    project_stop(if (nzchar(rec)) paste0(rec, " This run did not start. ") else paste0("The output of an earlier run that did not finish could not be put back (", tilde(file.path(pd, OUTPUT_KEPT)), "). "),
                 "Nothing was changed; close any program holding a file in the project folder and run again.")
  run <- run_new_id(); free <- disk_check(pd); kept <- output_keep(pd, free, run); done <- FALSE
  if (!isTRUE(kept) && !identical(kept, "no output yet"))
    project_stop("No copy of the last finished output could be made before this run (", kept, "), so the run did not start: it would change that output ",
                 "with no way back. Nothing was changed. ", attr(kept, "action") %||% "Check the project folder's drive and run again.")
  if (!isTRUE(kept)) journal_write(pd, run, "verified", config = paste(names(c0 <- cfg_inventory(pd)), c0, sep = "=", collapse = ";"))   # a first run: nothing to keep
  first <- !isTRUE(kept); err <- ""
  on.exit({ if (!lock_owned(lk)) NULL                                              # the lock was lost: another process owns the project now
    else if (!done) { why <- if (isTRUE(getOption("ovid.cancelled"))) "was cancelled" else "stopped before it finished"
      r <- run_recover(pd, why)
      run_result_write(pd, if (isTRUE(getOption("ovid.cancelled"))) "cancelled" else "failed", paste0(if (nzchar(err)) paste0(err, ". ") else "",
        if (first) "This was the first run: there was no earlier output to put back, so the output is incomplete until a run finishes." else r)) }
    else { journal_write(pd, run, "committed"); run_checkpoint("committed")
      if (dir.exists(file.path(pd, OUTPUT_KEPT))) remove_within(file.path(pd, OUTPUT_KEPT), pd, recursive = TRUE); journal_clear(pd)
      run_result_write(pd, "finished", "") } }, add = TRUE, after = FALSE)
  mk <- file.path(pd, "output", RUN_INCOMPLETE); dir.create(dirname(mk), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste("started", format(Sys.time(), "%Y-%m-%d %H:%M")), mk); unlink(file.path(pd, "output", RUN_RESTORED))
  # safe: cloud-folder notes are advice only
  notes <- tryCatch(cloud_notes(cfg), error = function(e) character())
  res <- withCallingHandlers(run_logged(c(cfg, list(run_kept = "kept", run_id = run, run_notes = notes)),
                                        function() { for (n in notes) say("  Note: ", n, col = "orange"); cache_retry(cfg, fun) }),
                             error = function(e) err <<- conditionMessage(e))
  run_checkpoint("run_written")                                                   # tests: the run has written all its output and not committed
  done <- TRUE; unlink(mk); res
}

# Raw files the last run used that are no longer there (deleted, moved or renamed): one problem naming them, before the outputs
# that held their records are replaced. prev: the last run's file list (paths), now: this run's.
files_prev <- function(file, col) { d <- tryCatch(fread(file = file, colClasses = "character", na.strings = NULL), error = function(e) data.table())
  if (!nrow(d) || !col %in% names(d)) return(character()); if ("status" %in% names(d)) d <- d[status == "ok"]; d[[col]] }
files_gone_check <- function(prev, now) {
  g <- setdiff(prev[!is.na(prev) & nzchar(prev)], now)
  if (length(g)) { problem("raw files", paste0(length(g), " raw file(s) the last run used are gone (deleted, moved or renamed), and their records are no longer in the output: ",
                                               paste(utils::head(basename(g), 5), collapse = ", "), if (length(g) > 5) ", ..." else ""))
    say("  ", length(g), " raw file(s) from the last run are gone; their records are no longer in the output (see Problems)", col = "orange") }
  invisible(g)
}
