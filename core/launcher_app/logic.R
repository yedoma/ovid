# core/launcher_app/logic.R  --  What the ovid launcher knows about the five apps and the processes it starts. No Shiny here,
# so the tests can drive it (core/tests/run_tests.R). Sourced by core/launcher_app/app.R.
#
# Each app runs in its own R process on its own port; the launcher keeps one record per app:
#   state "stopped" (never started, or stopped), "starting" (process up, page not answering yet), "running" (the page answers),
#   "failed" (the process ended before the page answered, or while running).
# Starting an app that is starting or running starts nothing: the launcher opens its page again instead.

LAUNCHER_APPS <- data.frame(stringsAsFactors = FALSE,
  key  = c("CS", "WLR", "GT", "GS", "RS"),
  name = c("Climate stations", "Water levels", "Ground temperature", "Geophysics", "Remote sensing"),
  what = c("Station loggers to clean daily records, with every value checked and every change explained.",
           "Pressure loggers to water above the sensor and water elevation, compensated for air pressure.",
           "Thermistor strings to daily temperatures by depth, MAGT and the active layer.",
           "ERT and GPR surveys in one catalogue, with quality control, pseudosections and radargrams.",
           "Drone flights and imagery catalogued from your drives, with positioning and footprints."))

# A launcher state: an environment with one record per app (proc, port, url, log, state, error).
lr_new <- function(root, apps = LAUNCHER_APPS) {
  st <- new.env(); st$root <- normalizePath(root, mustWork = FALSE); st$apps <- apps
  st$rec <- stats::setNames(lapply(apps$key, function(k) list(state = "stopped", proc = NULL, port = NA_integer_, url = "", log = "", error = "")), apps$key)
  st
}

# The command that starts one app: Rscript running shiny::runApp on 127.0.0.1 and the given port, from the app's folder,
# with the core found through OVID_CORE and <APP>_LOCAL=1 (the app shows its Browse... buttons, as from its own launcher).
lr_command <- function(st, key, port, windows = .Platform$OS.type == "windows") {
  dir <- file.path(st$root, key)
  list(cmd = file.path(R.home("bin"), if (windows) "Rscript.exe" else "Rscript"),
       args = c("-e", sprintf("shiny::runApp(%s, port = %dL, host = '127.0.0.1', launch.browser = FALSE, quiet = TRUE)", deparse(dir), as.integer(port))),
       wd = dir, env = c(OVID_CORE = file.path(st$root, "core"), stats::setNames("1", paste0(key, "_LOCAL"))))
}

# Start an app, or say it is already up. spawn(cmd, args, wd, env, log) returns a process-like object with $is_alive() and
# $kill() (processx by default; the tests pass a stand-in). Returns "started" or "already".
lr_start <- function(st, key, port, spawn = lr_spawn) {
  r <- st$rec[[key]]
  if (r$state %in% c("starting", "running") && !is.null(r$proc) && isTRUE(r$proc$is_alive())) return("already")
  log <- file.path(tempdir(), paste0("ovid_launcher_", tolower(key), ".log"))
  cmd <- lr_command(st, key, port)
  p <- spawn(cmd$cmd, cmd$args, cmd$wd, cmd$env, log)
  st$rec[[key]] <- list(state = "starting", proc = p, port = as.integer(port), url = sprintf("http://127.0.0.1:%d/", as.integer(port)), log = log, error = "")
  "started"
}

lr_spawn <- function(cmd, args, wd, env, log)
  processx::process$new(cmd, args, wd = wd, env = c("current", env), stdout = log, stderr = "2>&1", cleanup = FALSE)

# Move each record on: a starting app whose page answers is running; a process that has ended is failed (with the reason
# from its log) unless it was stopped on purpose. answers(url) says whether a page answers (lr_answers by default).
lr_poll <- function(st, answers = lr_answers) {
  for (k in names(st$rec)) { r <- st$rec[[k]]
    if (!r$state %in% c("starting", "running")) next
    if (is.null(r$proc) || !isTRUE(r$proc$is_alive())) { r$state <- "failed"; r$error <- lr_reason(r$log) }
    else if (r$state == "starting" && isTRUE(answers(r$url))) r$state <- "running"
    st$rec[[k]] <- r }
  invisible(st)
}

lr_stop <- function(st, key) {
  r <- st$rec[[key]]
  # the whole process tree: on Windows Rscript.exe starts Rterm.exe, which would outlive a kill of Rscript alone
  if (!is.null(r$proc) && isTRUE(r$proc$is_alive())) try(if (is.function(r$proc$kill_tree)) r$proc$kill_tree() else r$proc$kill(), silent = TRUE)   # safe: a process already ended
  r$state <- "stopped"; r$proc <- NULL; r$error <- ""; st$rec[[key]] <- r
  invisible(st)
}

lr_running <- function(st) names(Filter(function(r) r$state %in% c("starting", "running"), st$rec))

# Does the page answer? One short read of its first line.
lr_answers <- function(url) {
  con <- url(url); on.exit(try(close(con), silent = TRUE))                 # safe: a closed connection
  ok <- tryCatch(suppressWarnings(length(readLines(con, n = 1L, warn = FALSE)) > 0), error = function(e) FALSE)
  isTRUE(ok)
}

# One plain sentence from a failed app's log: the last "Error" line, made readable; else a general sentence.
lr_reason <- function(log) {
  l <- if (nzchar(log) && file.exists(log)) readLines(log, warn = FALSE) else character()
  e <- grep("^Error|Error in|there is no package|could not find|cannot open", l, value = TRUE)
  if (!length(e)) return("It stopped before its page opened.")
  e <- trimws(sub("^Error( in [^:]*)?:\\s*", "", utils::tail(e, 1)))
  pk <- regmatches(e, regexpr("(?<=there is no package called .)[A-Za-z0-9._]+", e, perl = TRUE))
  if (length(pk)) return(sprintf("It needs the package %s, which is not installed.", pk))
  paste0(toupper(substring(e, 1, 1)), substring(e, 2), if (grepl("[.!?]$", e)) "" else ".")
}

# A free port in the launcher's range.
lr_port <- function() tryCatch(httpuv::randomPort(min = 8100L, max = 8999L), error = function(e) sample(8100:8999, 1L))

# The packages the apps need that this R does not have (from build/renv.lock); the first start installs them.
lr_missing <- function(root) {
  lock <- file.path(root, "build", "renv.lock"); if (!file.exists(lock)) return(character())
  p <- names(jsonlite::read_json(lock)$Packages)
  p[!vapply(p, function(x) nzchar(system.file(package = x)), logical(1))]
}
