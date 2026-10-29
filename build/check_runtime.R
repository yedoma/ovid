# build/check_runtime.R  --  The smoke test of a runtime: from a clean copy of Ovid_Apps that holds the runtime, start the five
# apps the way the launcher does (core/launcher_app/logic.R), check that each page answers HTTP 200, then stop them all.
# Each app gets an empty project and raw folder of its own (so nothing outside the copy is read or written) and no run at start.
#
#   <runtime>/R/bin/Rscript build/check_runtime.R <Ovid_Apps copy>      (RHOME and an empty R_LIBS_USER / R_LIBS_SITE set)
# Exits 1 when an app does not answer within 180 s.
args <- commandArgs(TRUE); root <- normalizePath(if (length(args)) args[1] else ".", mustWork = TRUE)
source(file.path(root, "core", "launcher_app", "logic.R"))
cat("R", as.character(getRversion()), R.version$arch, "at", R.home(), "\nlibraries:", paste(.libPaths(), collapse = ", "), "\n")
tmp <- file.path(tempdir(), "ovid_check"); dir.create(tmp, showWarnings = FALSE)
env_for <- function(k) { d <- file.path(tmp, k); dir.create(file.path(d, "raw"), recursive = TRUE, showWarnings = FALSE); dir.create(file.path(d, "project"), showWarnings = FALSE)
  c(stats::setNames(file.path(d, "project"), paste0(k, "_PROJECT_DIR")), stats::setNames(file.path(d, "raw"), paste0(k, "_RAW_DIRS")), stats::setNames("1", paste0(k, "_NO_RUN"))) }
status_of <- function(url) tryCatch({ h <- curl::curl_fetch_memory(url); h$status_code }, error = function(e) NA_integer_)   # NA: no answer, reported as a failure
st <- lr_new(root)
for (k in st$apps$key) { e <- env_for(k)
  lr_start(st, k, lr_port(), spawn = function(cmd, args, wd, env, log) lr_spawn(cmd, args, wd, c(env, e), log)) }
# time zones as the apps use them: from the runtime's own zoneinfo, with no warning
withCallingHandlers({ Sys.timezone(); stopifnot(format(as.POSIXct("2020-07-01 12:00", tz = "America/Edmonton"), "%Z") == "MDT") },
  warning = function(w) { cat("time zones:", conditionMessage(w), "\n"); quit(status = 1) })
cat("time zones ok:", R.home("share"), "\n")
t0 <- Sys.time()
repeat { lr_poll(st); s <- vapply(st$rec, `[[`, "", "state")
  if (all(s %in% c("running", "failed")) || difftime(Sys.time(), t0, units = "secs") > 180) break; Sys.sleep(2) }
ok <- TRUE
for (k in names(st$rec)) { r <- st$rec[[k]]; code <- if (r$state == "running") status_of(r$url) else NA_integer_
  good <- identical(code, 200L); ok <- ok && good
  cat(sprintf("%-4s %-8s HTTP %s %s\n", k, r$state, code, r$error))
  if (!good && file.exists(r$log)) cat(utils::tail(readLines(r$log, warn = FALSE), 15), sep = "\n") }
for (k in names(st$rec)) lr_stop(st, k)
cat(sprintf("%s in %.0f s\n", if (ok) "all five apps answered" else "NOT all apps answered", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
quit(status = if (ok) 0 else 1)
