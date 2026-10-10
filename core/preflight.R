# Ovid/preflight.R  --  The set-up check's shared parts: a collector for findings and the checks every app makes.
# Part of Ovid Core (shared). Sourced before each app's own R/ files; not meant to be run on its own.
#
# Each app's own setup check lists what in its set-up would be ignored, misapplied or is missing; this file gives it
# the collector, small tests for cells, and the folder checks. Nothing is changed by a check.

# A collector: add(...) records a finding with the given columns; result() returns them as one table.
#   chk <- check_collector(c("level", "area", "item", "problem", "fix")); chk$add("error", "Folders", d, "not found", "Fix the path.")
check_collector <- function(cols) {
  out <- list()
  list(add = function(...) { v <- list(...); v <- c(v, rep(list(""), max(0, length(cols) - length(v))))[seq_along(cols)]
                             out[[length(out) + 1L]] <<- as.data.table(setNames(lapply(v, as.character), cols)) },
       result = function() if (length(out)) rbindlist(out) else as.data.table(setNames(replicate(length(cols), character(), simplify = FALSE), cols)))
}

# Cell tests: TRUE where a non-blank cell is not what it should be.
bad_regex  <- function(p) nzchar(p) & vapply(p, function(x) inherits(suppressWarnings(try(grepl(x, "x", perl = TRUE), silent = TRUE)), "try-error"), logical(1))
bad_number <- function(x) nzchar(x) & is.na(suppressWarnings(as.numeric(x)))
bad_date   <- function(x, parse = parse_date_cell) nzchar(x) & is.na(parse(x))

# Raw folders and the project folder. add is a collector's add (or any function taking level, then what, item, problem, fix).
check_folders <- function(add, raw_dirs, project_dir, what = "Folders") {
  rdirs <- path.expand(trim(raw_dirs)); rdirs <- rdirs[nzchar(rdirs)]
  if (!length(rdirs)) add("error", what, "", "No raw data folder is set.", "Add one under Data source on the Home page.")
  for (d in rdirs[!dir.exists(rdirs)]) add(if (any(dir.exists(rdirs))) "warning" else "error", what, d, "This raw data folder does not exist.", "Check the path on the Home page.")
  pd <- path.expand(trim(if (is.null(project_dir)) "" else project_dir))
  if (!nzchar(pd)) add("error", what, "", "No project folder is set.", "Set one on the Home page.")
  else if (dir.exists(pd) && file.access(pd, 2) != 0) add("error", what, pd, "The project folder is not writable.", "Choose a folder you can write to.")
  invisible(NULL)
}

# A one-word summary of a check's findings: "error", "warning" or "ok".
check_status <- function(p) if (is.null(p) || !nrow(p)) "ok" else if (any(p$level == "error")) "error" else if (any(p$level == "warning")) "warning" else "ok"
