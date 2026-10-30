# Ovid/runner.R  --  Runs an app's pipeline in a background R process, so the app stays usable while it works.
# Part of Ovid Core (shared). Sourced by each app.R (not by the pipelines); not meant to be run on its own.
#
# The run is a separate R process (callr) that sources the app's pipeline with <APP>_NO_RUN=1 and calls run_pipeline(cfg).
# What it prints goes to a file the app reads every half second: the "[k/n] ..." lines are its stages and the "@@ i n what"
# lines (core util.R run_tick) the items within a stage, shown as one progress bar in the corner of every window of the app.
# Cancel leaves a flag file; the run stops at its next message or item, between steps and never in the middle of writing a
# file, and its log (output/logs) ends "cancelled by the user". Files already written stay; the next run redoes what the
# cancelled run did not finish. A run that has not stopped RUN_KILL_AFTER seconds after Cancel is ended.
#
# One run at a time per app process, whichever window started it: every window shows its progress and its Run buttons are
# turned off until it ends. The window that started it gets the result.
#
#   run_server(input, output, session, busy_ids)   once in each app's server: the progress box and the busy buttons
#   run_start(session, cfg, on_done, ...)          starts a run (args: more arguments for run_pipeline); on_done(r) is called
#                                                    in that window when it ends, with
#       r$status   "finished", "failed", "cancelled" or "asked" (the run raised one of the conditions in `ask`)
#       r$value    what run_pipeline() returned ("finished")    r$message  the error or the condition's message
#       r$condition  the condition ("asked")                      r$lines  what the run printed    r$seconds
#   run_busy()                                       TRUE while a run is going

RUN_KILL_AFTER <- 30
RUNNER <- new.env()                                  # job: the run going on now, or NULL; tick: changes when a run starts or ends

# The background process: everything it needs is passed in, nothing is taken from the app.
.run_child <- function(app_dir, pipeline, no_run, cfg, cancel_file, ask, args = list()) {
  do.call(Sys.setenv, stats::setNames(list("1"), no_run))
  options(ovid.cancel_file = cancel_file, warn = 1)
  setwd(app_dir); suppressPackageStartupMessages(source(pipeline, chdir = TRUE))
  assign("say", function(..., col = "fg") { run_check_cancel(); msg <- paste0(...); stage_mark(msg); cat(msg, "\n", sep = ""); flush(stdout()) }, envir = globalenv())
  withRestarts(
    tryCatch(list(status = "finished", value = do.call(run_pipeline, c(list(cfg), args))),
      error = function(e) if (length(intersect(class(e), ask))) list(status = "asked", message = conditionMessage(e), condition = e)
                          else list(status = "failed", message = conditionMessage(e))),
    ovid_cancel = function() list(status = "cancelled", message = "cancelled by the user"))
}

run_busy <- function() !is.null(RUNNER$job)
# OVID_RUN_SYNC=1 (the apps' tests, or where callr is missing): the run happens in the app's own process, as before, and
# on_done is called before run_start returns.
run_sync <- function() identical(Sys.getenv("OVID_RUN_SYNC"), "1") || !requireNamespace("callr", quietly = TRUE)
run_app_short <- function() sub("^.*// *", "", APP_NAME)                              # "Ovid Core // CS" -> "CS"

# Starts a run. app_dir and pipeline default to the app's own (APP_DIR, <app>_pipeline.R); no_run to <APP>_NO_RUN.
run_start <- function(session, cfg, on_done, label = "Running pipeline", ask = "ovid_ask_zone", args = list(),
                      app_dir = APP_DIR, pipeline = file.path(app_dir, paste0(tolower(run_app_short()), "_pipeline.R")),
                      no_run = paste0(toupper(run_app_short()), "_NO_RUN")) {
  if (run_sync()) {
    t0 <- Sys.time(); r <- NULL
    lines <- utils::capture.output(r <- tryCatch(list(status = "finished", value = do.call(run_pipeline, c(list(cfg), args))),
      error = function(e) if (length(intersect(class(e), ask))) list(status = "asked", message = conditionMessage(e), condition = e)
                          else list(status = "failed", message = conditionMessage(e))))
    r$lines <- gsub("\033\\[[0-9;]*m", "", lines); r$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    on_done(r); return(invisible(TRUE)) }
  if (run_busy()) { showNotification("A run is already going. Wait for it, or cancel it in the box at the bottom right.", type = "warning"); return(invisible(FALSE)) }
  d <- tempfile("ovid_run_"); dir.create(d)
  out <- file.path(d, "console.txt"); cancel <- file.path(d, "cancel")
  p <- tryCatch(callr::r_bg(.run_child, args = list(app_dir = normalizePath(app_dir), pipeline = normalizePath(pipeline), no_run = no_run, cfg = cfg,
                                                    cancel_file = cancel, ask = ask, args = args),
                            stdout = out, stderr = "2>&1", supervise = TRUE, package = FALSE),
                error = function(e) e)
  if (inherits(p, "error")) { unlink(d, recursive = TRUE); showNotification(paste("The run could not start:", conditionMessage(p)), type = "error", duration = NULL); return(invisible(FALSE)) }
  RUNNER$job <- list(proc = p, dir = d, out = out, cancel = cancel, label = label, t0 = Sys.time(), on_done = on_done,
                     session = session, cancelled_at = NULL, project_dir = cfg$project_dir)
  RUNNER$tick <- (RUNNER$tick %||% 0) + 1
  invisible(TRUE)
}

run_cancel <- function() { j <- RUNNER$job; if (is.null(j) || !is.null(j$cancelled_at)) return(invisible())
  file.create(j$cancel); j$cancelled_at <- Sys.time(); RUNNER$job <- j; invisible() }

# What the run has printed so far, without the console colours.
run_lines <- function(j) if (file.exists(j$out)) gsub("\033\\[[0-9;]*m", "", readLines(j$out, warn = FALSE)) else character()

# Where the run is: the stage (k of n and its name) and, within it, the latest item.
run_where <- function(lines) {
  st <- grep("^\\s*\\[[0-9]+/[0-9]+\\]", lines)
  if (!length(st)) return(list(k = 0L, n = 1L, stage = "Starting", frac = 0, item = "", last = utils::tail(lines[!startsWith(lines, "@@ ")], 1)))
  s <- lines[max(st)]; kn <- as.integer(regmatches(s, regexec("\\[([0-9]+)/([0-9]+)\\]", s))[[1]][2:3])
  ticks <- grep("^@@ [0-9]+ [0-9]+", lines[seq.int(max(st), length(lines))], value = TRUE)
  frac <- 0; item <- ""
  if (length(ticks)) { t <- utils::tail(ticks, 1); v <- strsplit(sub("^@@ ", "", t), " ", fixed = TRUE)[[1]]
    i <- as.numeric(v[1]); n <- as.numeric(v[2]); if (is.finite(i) && is.finite(n) && n > 0) frac <- min(1, (i - 1) / n)
    item <- paste0(v[1], " of ", v[2], if (length(v) > 2) paste0(": ", paste(v[-(1:2)], collapse = " "))) }
  list(k = kn[1], n = kn[2], stage = trimws(sub("^\\s*\\[[0-9]+/[0-9]+\\]\\s*", "", s)), frac = frac, item = item,
       last = utils::tail(lines[!startsWith(lines, "@@ ") & !grepl("^\\s*\\[[0-9]+/[0-9]+\\]", lines)], 1))
}

# The run has ended (or its process is gone): the result, the job cleared, on_done called in the window that started it.
run_finish <- function() {
  j <- RUNNER$job; if (is.null(j)) return(invisible())
  # safe: a run that ended without a result is reported as failed from its log below
  lines <- run_lines(j); r <- tryCatch(j$proc$get_result(), error = function(e) NULL)
  if (is.null(r)) r <- if (!is.null(j$cancelled_at)) list(status = "cancelled", message = "cancelled by the user")
                       else list(status = "failed", message = paste("the run's R process ended unexpectedly", utils::tail(lines, 1)))
  r$lines <- lines[!startsWith(lines, "@@ ")]; r$seconds <- as.numeric(difftime(Sys.time(), j$t0, units = "secs"))
  RUNNER$job <- NULL; RUNNER$tick <- RUNNER$tick + 1; unlink(j$dir, recursive = TRUE)
  if (!identical(r$status, "finished")) {                                                         # core manifest.R: the output kept before the run
    rec <- tryCatch(output_recover(j$project_dir), error = function(e) "")                        # a process that was killed did not put it back itself
    # safe: the note about restored output is extra
    r$restored <- if (nzchar(rec)) rec else tryCatch(run_restored(j$project_dir), error = function(e) "")
    if (nzchar(r$restored)) shiny::withReactiveDomain(j$session, showNotification(r$restored, type = "message", duration = 15)) }
  if (is.function(j$on_done)) shiny::withReactiveDomain(j$session, j$on_done(r))
  invisible(r)
}

# Once in each app's server. busy_ids: the buttons that start a run (or change what a run reads); they are turned off in
# every window while a run is going, with a note saying why.
run_server <- function(input, output, session, busy_ids = "run") {
  seen <- reactiveVal(-1)
  observe({ invalidateLater(500)
    j <- RUNNER$job
    if (!is.null(j)) {
      if (j$session$isClosed()) { j$session <- session; RUNNER$job <- j }                # the window that started it was closed: this one finishes it
      if (identical(j$session, session)) {
        if (!j$proc$is_alive()) run_finish()
        # safe: the run is already ending; a process that is gone cannot be stopped
        else if (!is.null(j$cancelled_at) && difftime(Sys.time(), j$cancelled_at, units = "secs") > RUN_KILL_AFTER) try(j$proc$kill_tree(), silent = TRUE) }
    }
    t <- RUNNER$tick %||% 0
    if (!identical(isolate(seen()), t)) { seen(t)
      session$sendCustomMessage("ovid-busy", list(on = run_busy(), ids = as.list(busy_ids),
                                                  why = "A run is going: wait for it, or cancel it in the box at the bottom right.")) }
  })
  output$ovid_runner <- renderUI({ invalidateLater(700); j <- RUNNER$job; if (is.null(j)) return(NULL)
    w <- run_where(run_lines(j)); secs <- round(as.numeric(difftime(Sys.time(), j$t0, units = "secs")))
    pct <- if (w$k > 0) round(100 * ((w$k - 1) + w$frac) / w$n) else 1
    stopping <- !is.null(j$cancelled_at)
    div(class = "d-run", role = "status",
      div(class = "d-run-head", strong(j$label), span(class = "d-run-time", sprintf("%d:%02d", secs %/% 60, secs %% 60))),
      div(class = "d-run-bar", div(class = "d-run-fill", style = sprintf("width:%d%%", max(1L, as.integer(pct))))),
      div(class = "d-run-stage", if (w$k > 0) sprintf("Step %d of %d: %s", w$k, w$n, w$stage) else w$stage),
      if (nzchar(w$item)) div(class = "d-run-item", w$item) else if (length(w$last) && nzchar(trimws(w$last))) div(class = "d-run-item", trimws(w$last)),
      div(class = "d-run-foot",
        if (stopping) span(class = "d-run-note", "Stopping at the next safe point...")
        else span(class = "d-run-note", "The app stays usable while it runs."),
        actionButton("ovid_run_cancel", if (stopping) "Stopping" else "Cancel", class = "btn-sm", disabled = if (stopping) NA else NULL)))
  })
  observeEvent(input$ovid_run_cancel, run_cancel())
  pd0 <- get0("CFG")$project_dir; rec <- if (is.null(RUNNER$job)) tryCatch(output_recover(pd0), error = function(e) "") else ""   # core manifest.R
  if (nzchar(rec)) showNotification(rec, type = "warning", duration = NULL)
  # safe: a note about an unfinished run is extra
  inc <- tryCatch(run_incomplete(pd0), error = function(e) "")
  if (nzchar(inc)) showNotification(paste0("The last run on this project did not finish (", inc, "). Some outputs may be from before it: run again to bring them all up to date."),
                                    type = "warning", duration = NULL)
  if (isTRUE(attr(get0("SETTINGS", ifnotfound = list()), "from_backup")))                        # core util.R settings_read()
    showNotification("The app's settings file could not be read; the last good copy (app_settings.json.bak) was used. Check the folders on Settings.", type = "warning", duration = NULL)
  invisible()
}

# The progress box and what turns the Run buttons off, in both interfaces (ovid_page and wb_page add it when this file is
# sourced). Colours from the page: the Standard shell's tokens, else the Workbench's.
run_ui <- function() tagList(tags$style(HTML(RUN_CSS)), tags$script(HTML(RUN_JS)), uiOutput("ovid_runner"))
RUN_CSS <- paste(readLines(file.path(CORE_DIR, "www", "runner.css"), encoding = "UTF-8"), collapse = "\n")
RUN_JS <- paste(readLines(file.path(CORE_DIR, "www", "runner.js"), encoding = "UTF-8"), collapse = "\n")
