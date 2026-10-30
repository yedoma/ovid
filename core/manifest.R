# Ovid/manifest.R  --  Run manifest: what ran, with which code and which set-up; and one line per run in the history.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Each app sets APP_NAME and APP_VERSION (bump the version when a change alters the output). CORE_VERSION is the
# version of these shared files; bump it when a change here alters any app's output.

CORE_VERSION <- "1.15.0"

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
troubleshooting_zip <- function(project_dir, zipfile, check = NULL, n_logs = 10L, max_mb = 5) {
  pd <- path.expand(project_dir %||% ""); app <- if (exists("APP_NAME")) sub("^.*// *", "", APP_NAME) else "app"
  ref <- paste0("OVID-", toupper(safe_name(app)), "-", format(Sys.time(), "%Y%m%d-%H%M"), "-", substr(hash_of(list(pd, Sys.time(), Sys.getpid()), 16L), 1, 4))
  tmp <- file.path(tempfile("ovid_ts_"), ref); dir.create(tmp, recursive = TRUE); left <- character()
  home <- path.expand("~"); clean <- function(x) gsub(home, "~", x, fixed = TRUE)
  put <- function(src, dest) { if (!file.exists(src)) return(invisible(FALSE))
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
outputs_prune <- function(out, dirs, keep) for (d in dirs) { f <- list.files(file.path(out, d)); unlink(file.path(out, d, f[!file.path(d, f) %in% keep])) }
outputs_done <- function(out, file, prev, now) { now <- unlist(now, use.names = FALSE); unlink(file.path(out, setdiff(prev, now))); saveRDS_safe(now, file); invisible(now) }

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
PROJECT_KNOWN <- c("config", "output", "cache", "viewer_app", "reports", PROJECT_MARK, PROJECT_LOCK, OUTPUT_KEPT, OUTPUT_FAILED, ".DS_Store", "Thumbs.db", "desktop.ini")
project_stop <- function(...) stop(structure(class = c("ovid_project_check", "error", "condition"), list(message = paste0(...), call = NULL)))
path_norm <- function(p) {                                           # the real path, also of a folder not made yet (its nearest existing parent resolved)
  p <- sub("[/\\\\]+$", "", path.expand(p)); rest <- character()
  while (!dir.exists(p) && dirname(p) != p) { rest <- c(basename(p), rest); p <- dirname(p) }
  sub("/+$", "", do.call(file.path, as.list(c(normalizePath(p, winslash = "/", mustWork = FALSE), rest))))
}
path_within <- function(a, b) { a <- tolower(path_norm(a)); b <- tolower(path_norm(b)); a == b || startsWith(a, paste0(b, "/")) }   # a is b or under it
project_check <- function(cfg, app = sub("^.*// *", "", if (exists("APP_NAME")) APP_NAME else "app"), need_raw = TRUE) {
  pd <- trim(as.character(cfg$project_dir %||% "")[1] %|% "")
  if (!nzchar(pd)) pd <- ovid_project_dir(pd)                                    # none set: the default folder (core util.R)
  raw <- as.character(unlist(cfg$raw_dirs)); raw <- trim(raw[!is.na(raw) & nzchar(trim(raw))])
  if (need_raw && "raw_dirs" %in% names(cfg) && !length(raw)) project_stop("No raw data folder is set: it is the one thing every run needs. Choose the folder that holds the logger files on 1 Project ",
                                              "(Choose folder\u2026) and run again. Nothing was changed.")
  for (r in raw) {
    if (path_within(pd, r)) project_stop("The project folder ", tilde(path_norm(pd)), " is inside the raw folder ", tilde(path_norm(r)),
                                         ". Raw folders are only read; choose a project folder outside them.")
    if (path_within(r, pd)) project_stop("The raw folder ", tilde(path_norm(r)), " is inside the project folder ", tilde(path_norm(pd)),
                                         ". The app would read its own output as raw data; keep raw data outside the project folder.")
  }
  gone <- raw[!dir.exists(path.expand(raw))]
  if (need_raw && length(gone)) project_stop("Raw folder not found: ", paste(tilde(gone), collapse = ", "), ". Is the drive plugged in? Nothing was changed; ",
                                 "plug it in and run again, or remove the folder from the data folders if it is no longer used.")
  p <- path.expand(pd); mk <- file.path(p, PROJECT_MARK)
  if (file.exists(mk)) { owner <- ovid_app_name(sub("^app: *", "", grep("^app:", readLines(mk, warn = FALSE), value = TRUE)[1] %|% ""))
    if (nzchar(owner) && !identical(owner, app)) project_stop("The folder ", tilde(path_norm(pd)), " is the project of Ovid ", owner, ", not ", app,
                                                              ". Two apps in one project folder overwrite each other's files: choose a folder of its own.") }
  else if (dir.exists(p)) {
    other <- setdiff(list.files(p, all.files = TRUE, no.. = TRUE), PROJECT_KNOWN)
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
pid_alive <- function(pid) if (.Platform$OS.type == "windows") { if (requireNamespace("ps", quietly = TRUE)) pid %in% ps::ps_pids() else TRUE } else isTRUE(tools::pskill(pid, 0L))
# The run holding the project's lock (started, host), or NULL when there is none or the one that left it has gone (its process
# ended on this computer, or the lock is over a day old).
lock_holder <- function(project_dir) {
  lk <- file.path(path.expand(project_dir), PROJECT_LOCK); if (!dir.exists(lk)) return(NULL)
  # safe: the lock's details are only for the message
  info <- tryCatch(readLines(file.path(lk, "run.txt"), warn = FALSE), error = function(e) character())
  g <- function(k) sub(paste0("^", k, ": *"), "", grep(paste0("^", k, ":"), info, value = TRUE)[1] %|% "")
  pid <- suppressWarnings(as.integer(g("pid"))); same_host <- identical(g("host"), Sys.info()[["nodename"]])
  alive <- !is.na(pid) && (!same_host || pid_alive(pid))
  age <- as.numeric(difftime(Sys.time(), file.mtime(lk), units = "hours"))
  if (alive && !is.na(age) && age < 24) list(started = g("started"), host = g("host")) else NULL
}
project_lock <- function(cfg) {
  p <- path.expand(cfg$project_dir); dir.create(p, recursive = TRUE, showWarnings = FALSE)
  mk <- file.path(p, PROJECT_MARK); app <- sub("^.*// *", "", if (exists("APP_NAME")) APP_NAME else "app")
  # safe: the marker file is a convenience; the project works without it
  if (!file.exists(mk)) try(writeLines(c(paste0("app: ", app), "This folder is an Ovid project. Raw data is never kept here."), mk), silent = TRUE)
  lk <- file.path(p, PROJECT_LOCK); me <- c(paste0("pid: ", Sys.getpid()), paste0("host: ", Sys.info()[["nodename"]]), paste0("started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  if (!dir.create(lk, showWarnings = FALSE)) {
    h <- lock_holder(p)
    if (!is.null(h)) project_stop("Another run is going on this project (started ", h$started, " on ", h$host, "). Wait for it to end.")
    unlink(lk, recursive = TRUE); dir.create(lk, showWarnings = FALSE)                            # left by a run that has gone
  }
  writeLines(me, file.path(lk, "run.txt"))
  invisible(lk)
}
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
# A run updates output/ in place. Before it starts, output/ is copied to <project>/.output_before_run (on a Mac's own disk, APFS,
# as a clone: instant and taking no space until files change). A run that finishes removes the copy; one that fails, is
# cancelled or stops to ask a question puts it back, so the output is again what the last finished run left, with the stopped
# run's log added. The cached results the stopped run made are dropped (they would let the next run skip outputs that were put
# back), and output/.run_restored says what happened until a run finishes. A run whose process was killed leaves the copy:
# it is put back when the app next starts or before the next run. No copy is kept (the output is updated in place, as before,
# with output/.run_incomplete) when it would not fit on the drive, or is over OUTPUT_COPY_MAX and the disk cannot clone.
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
output_keep <- function(project_dir, free = disk_free(project_dir)) {    # TRUE when a copy is kept; a character string when not (why)
  pd <- path.expand(project_dir); out <- file.path(pd, "output"); kept <- file.path(pd, OUTPUT_KEPT)
  unlink(kept, recursive = TRUE)
  if (!dir.exists(out) || !length(list.files(out, recursive = TRUE, all.files = TRUE))) return("no output yet")
  clone <- disk_clones(pd); size <- dir_bytes(out)
  if (!clone && size > OUTPUT_COPY_MAX) return(paste0("the output (", fmt_bytes(size), ") is too large to copy before each run"))
  if (!clone && isTRUE(free < 2 * size + DISK_MIN)) return(paste0("a copy of the output (", fmt_bytes(size), ") would not fit on the drive (", fmt_bytes(free), " free)"))
  ok <- if (.Platform$OS.type == "windows") { dir.create(kept)
          all(file.copy(list.files(out, full.names = TRUE, all.files = TRUE, no.. = TRUE), kept, recursive = TRUE, copy.date = TRUE)) }
        else { flags <- if (clone) "-cpR" else if (Sys.info()[["sysname"]] == "Linux") c("-pR", "--reflink=auto") else "-pR"
          identical(suppressWarnings(system2("cp", c(flags, shQuote(out), shQuote(kept)), stdout = FALSE, stderr = FALSE)), 0L) }
  n_files <- function(d) { f <- list.files(d, recursive = TRUE, all.files = TRUE); sum(!grepl("^\\._", basename(f))) }   # ._ files: a Mac's companions on exFAT and network drives
  if (!isTRUE(ok) || n_files(kept) != n_files(out)) {
    unlink(kept, recursive = TRUE); return("the output could not be copied") }
  writeLines(as.character(as.numeric(Sys.time())), file.path(kept, ".ovid_kept"))
  TRUE
}
# keep: cache sub-folders an app marks as holding only what was read from its inputs (CACHE_INPUT_ONLY, e.g. RS "sources":
# image headers and log reads); they stay valid whatever the output, so a stopped run keeps them. Default none.
output_restore <- function(project_dir, why = "stopped", keep = get0("CACHE_INPUT_ONLY", ifnotfound = character())) {
  pd <- path.expand(project_dir); out <- file.path(pd, "output"); kept <- file.path(pd, OUTPUT_KEPT); failed <- file.path(pd, OUTPUT_FAILED)
  if (!dir.exists(kept)) return(FALSE)
  # safe: with no stamp the folder's own time is used
  since <- suppressWarnings(as.numeric(tryCatch(readLines(file.path(kept, ".ovid_kept"), warn = FALSE)[1], error = function(e) NA)))
  if (is.na(since)) since <- as.numeric(file.mtime(kept))
  unlink(failed, recursive = TRUE)
  if (dir.exists(out) && !file.rename(out, failed)) return(FALSE)                  # a file held open (Windows): left for the next try
  if (!file.rename(kept, out)) { file.rename(failed, out); return(FALSE) }
  unlink(file.path(out, c(".ovid_kept", "._.ovid_kept")))
  lg <- list.files(file.path(failed, "logs"), pattern = "\\.log$", full.names = TRUE)        # the stopped run's log is kept
  lg <- lg[!basename(lg) %in% list.files(file.path(out, "logs"))]
  if (length(lg)) { dir.create(file.path(out, "logs"), showWarnings = FALSE); file.copy(lg, file.path(out, "logs"), copy.date = TRUE) }
  unlink(failed, recursive = TRUE)
  cf <- list.files(file.path(pd, "cache"), recursive = TRUE, full.names = FALSE, all.files = TRUE)
  cf <- file.path(pd, "cache", cf[!sub("/.*", "", cf) %in% keep])
  unlink(cf[as.numeric(file.mtime(cf)) >= since - 2])
  writeLines(paste0("The run of ", format(as.POSIXct(since, origin = "1970-01-01"), "%Y-%m-%d %H:%M"), " ", why,
                    "; the output was put back as the run before it left it."), file.path(out, RUN_RESTORED))
  TRUE
}
# A copy left by a run whose process was killed (and no run going now): put back. Returns what output/.run_restored says, or "".
output_recover <- function(project_dir) {
  pd <- path.expand(project_dir %||% ""); if (!nzchar(pd) || !dir.exists(file.path(pd, OUTPUT_KEPT)) || !is.null(lock_holder(pd))) return("")
  if (output_restore(pd, "did not finish (the app or computer stopped)")) run_restored(pd) else ""
}

run_project <- function(cfg, fun, need_raw = TRUE) {                # need_raw = FALSE: an app that keeps the last scan of a drive that is away (RS)
  options(ovid.cancelled = NULL)
  project_check(cfg, need_raw = need_raw)
  lk <- project_lock(cfg); on.exit(unlink(lk, recursive = TRUE), add = TRUE)
  held <- files_locked(cfg$project_dir)
  if (length(held)) project_stop("These files are open in another program or may not be written: ", paste(utils::head(held, 5), collapse = ", "),
                                 if (length(held) > 5) ", ..." else "", ". Close them (Excel locks a CSV it has open) and run again.")
  pd <- path.expand(cfg$project_dir)
  if (dir.exists(file.path(pd, OUTPUT_KEPT))) { output_restore(pd, "did not finish (the app or computer stopped)")      # left by a run that was killed
    say("  The last run did not finish; its output was put back as the run before it left it", col = "orange") }
  free <- disk_check(pd); kept <- output_keep(pd, free); done <- FALSE
  if (isTRUE(kept)) on.exit(if (!done) output_restore(pd, if (isTRUE(getOption("ovid.cancelled"))) "was cancelled" else "stopped before it finished")
                            else unlink(file.path(pd, OUTPUT_KEPT), recursive = TRUE), add = TRUE, after = FALSE)
  mk <- file.path(pd, "output", RUN_INCOMPLETE); dir.create(dirname(mk), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste("started", format(Sys.time(), "%Y-%m-%d %H:%M")), mk); unlink(file.path(pd, "output", RUN_RESTORED))
  # safe: cloud-folder notes are advice only
  notes <- tryCatch(cloud_notes(cfg), error = function(e) character())
  res <- run_logged(c(cfg, list(run_kept = if (isTRUE(kept) || identical(kept, "no output yet")) "kept" else kept, run_notes = notes)),
                    function() { for (n in notes) say("  Note: ", n, col = "orange"); cache_retry(cfg, fun) })
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
