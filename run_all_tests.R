# run_all_tests.R  --  Runs every ovid test suite (core and all five apps), each in its own fresh R, several at
# once, and prints one summary table. Full output of each suite goes to a log folder (printed at the end), never into
# the repo.
#   Rscript run_all_tests.R               every suite
#   Rscript run_all_tests.R CS WLR        core plus only these apps
#   Rscript run_all_tests.R --changed     core plus only the apps with changes not yet on GitHub
#                                         (every app if core changed)
#   Rscript run_all_tests.R --one         one suite at a time (slower; for a clean timing of each suite)
# Exit status is 1 if any suite fails, so a GitHub check can use it as is.

args <- commandArgs(trailingOnly = TRUE)
flags <- args[startsWith(args, "--")]
repo <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
})
all_apps <- c("CS", "WLR", "GT", "ERT", "RS")
apps <- all_apps
named <- toupper(setdiff(args, flags))
if (length(named)) apps <- intersect(apps, named)

# --changed: files changed in the working tree or in commits not yet pushed. A change to core/ or to this script tests
# every app, because every app sources core.
if ("--changed" %in% flags) {
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = FALSE))
  changed <- c(git("diff", "--name-only", shQuote("@{upstream}...HEAD")), substring(git("status", "--porcelain"), 4))
  top <- unique(sub("/.*", "", changed[nzchar(changed)]))
  if (!any(c("core", "run_all_tests.R") %in% top)) apps <- intersect(apps, top)
  cat("Changed:", if (length(top)) paste(top, collapse = ", ") else "nothing", "\n")
}

rscript <- file.path(R.home("bin"), "Rscript")
# In the system temp folder, not R's own tempdir(), which R deletes on exit and would take the logs with it.
log_dir <- file.path(dirname(tempdir()), paste0("ovid_tests_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(log_dir, recursive = TRUE)

# One row per suite: where it runs from, which script, and its arguments. Each app's tests start in the app folder.
# RS's app test runs RS's tests/run_tests.R first, so RS has that one suite only. The raw-folder-only check runs once
# per app so those runs can go side by side. Rows are in rough order of run time, longest first, so the slow ones start
# first.
suite <- function(name, dir, script, arg = character()) list(name = name, dir = dir, script = script, arg = arg)
suites <- list()
for (a in intersect(c("GT", "CS", "WLR", "ERT"), apps)) {
  suites[[length(suites) + 1]] <- suite(a, file.path(repo, a), "tests/run_tests.R")
}
if ("RS" %in% apps) suites[[length(suites) + 1]] <- suite("RS (with app)", file.path(repo, "RS"), "tests/test_app.R")
if ("RS" %in% apps) suites[[length(suites) + 1]] <- suite("RS drone photos", file.path(repo, "RS"), "tests/test_drone_sources.R")
for (a in apps) {
  suites[[length(suites) + 1]] <- suite(paste("raw folder only", a), repo, "core/tests/test_raw_only.R", a)
}
if ("ERT" %in% apps) suites[[length(suites) + 1]] <- suite("ERT gpr", file.path(repo, "ERT"), "tests/test_gpr.R")
suites[[length(suites) + 1]] <- suite("core", repo, "core/tests/run_tests.R")
suites[[length(suites) + 1]] <- suite("core import", repo, "core/tests/test_import.R")
suites[[length(suites) + 1]] <- suite("core areas", repo, "core/tests/test_areas.R")
suites[[length(suites) + 1]] <- suite("core panels", repo, "core/tests/test_wb_panels.R")

# How many suites at once: the cores less one (forked, so macOS and Linux; Windows runs one at a time). Each suite's
# own file reading (core util.R par_map) then gets a share of the cores, so the suites do not crowd each other out.
cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
jobs <- if ("--one" %in% flags || .Platform$OS.type == "windows") 1L else min(cores, length(suites))
child_env <- c(paste0("OVID_WORKERS=", max(1L, cores %/% jobs)))

# The "N passed, M failed" line most suites print last; NA where a suite only prints its own wording.
counts <- function(lines) {
  hit <- grep("[0-9]+ passed", lines, value = TRUE)
  if (!length(hit)) return(c(NA, NA))
  last <- hit[length(hit)]
  pass <- as.integer(sub(".*?([0-9]+) passed.*", "\\1", last, perl = TRUE))
  fail <- if (grepl("[0-9]+ failed", last)) as.integer(sub(".*?([0-9]+) failed.*", "\\1", last, perl = TRUE)) else 0L
  c(pass, fail)
}

run_one <- function(s) {
  log_file <- file.path(log_dir, paste0(gsub("[^A-Za-z0-9]+", "_", s$name), ".log"))
  started <- Sys.time()
  # Some suites expect to start in their app folder. Each suite runs in its own forked process, so changing the
  # folder here never affects the suites running beside it.
  setwd(s$dir)
  status <- system2(rscript, c(shQuote(file.path(s$dir, s$script)), s$arg),
                    stdout = log_file, stderr = log_file, env = child_env)
  secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")))
  n <- counts(readLines(log_file, warn = FALSE))
  ok <- identical(as.integer(status), 0L)
  # A suite that passed without checking anything (no data to check) shows as skipped, never as ok.
  result <- if (!ok) "FAILED" else if (identical(n[1], 0L) && identical(n[2], 0L)) "skipped" else "ok"
  cat(sprintf("  %-24s %s (%ds)\n", s$name, result, secs))
  data.frame(suite = s$name, result = result, passed = n[1], failed = n[2],
             seconds = secs, log = basename(log_file))
}

cat(sprintf("Running %d suites, %d at a time\n", length(suites), jobs))
t0 <- Sys.time()
results <- if (jobs > 1) {
  parallel::mclapply(suites, run_one, mc.cores = jobs, mc.preschedule = FALSE)
} else {
  lapply(suites, run_one)
}
res <- do.call(rbind, results)
total <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))

cat("\n")
print(res, row.names = FALSE)
cat(sprintf("\nTotal %ds. Logs: %s\n", total, log_dir))
bad <- res$suite[res$result == "FAILED"]
if (length(bad)) {
  cat("Failed:", paste(bad, collapse = ", "), "\n")
  for (l in res$log[res$result == "FAILED"]) {
    lines <- readLines(file.path(log_dir, l), warn = FALSE)
    hit <- grep("FAIL|Error", lines)
    cat(sprintf("\n-- %s --\n", l), lines[unique(pmin(length(lines), sort(c(hit, hit + 1, hit + 2))))], sep = "\n")
  }
  quit(status = 1)
}
cat(if (any(res$result == "skipped")) sprintf("All suites passed; skipped (nothing to check): %s.\n", paste(res$suite[res$result == "skipped"], collapse = ", ")) else "All suites passed.\n")
