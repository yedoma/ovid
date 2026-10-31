#!/usr/bin/env Rscript
# =============================================================================
#  gs_pipeline.R  --  ERT catalogue / metadata / QC pipeline   (Ovid Core // GS)
# =============================================================================
#  What it does (milestone 1)
#    1. Reads the set-up tables (projects, sites, transects, surveys, QC profiles, manual flags, file rules).
#    2. Finds every file under the raw folders and tells what each one is; every file is listed, readable or not.
#    3. Reads each instrument data file once (AGI SuperSting .stg so far) into the common Ovid GS model, keeping
#       the instrument's own fields beside it; joins contact resistance (.crs) and the command file (.cmd).
#    4. Links copies, merged files and EarthImager sub/trimmed sets to the original surveys.
#    5. Writes the catalogue (output/catalogue) and one folder per survey (output/surveys/<survey_id>).
#  Raw files are never modified. Nothing is removed: QC (next milestone) only adds flags.
#
#  Use
#    Rscript gs_pipeline.R      folders from CFG below, or GS_RAW_DIRS (separated by ;) and GS_PROJECT_DIR
#  Design: docs/DESIGN.md
# =============================================================================

for (.l in if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8", "UTF-8"))   # UTF-8 text ( deg C, uS/cm);
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break                               # macOS has no C.UTF-8

GS_VERSION <- "0.5.0"

OVID_APP <- "GS"                                          # its projects go in ~/Projects/Ovid/Projects/GS/ (core util.R)
CFG <- list(
  raw_dirs    = c("~/Projects/Ovid/Projects/GS/raw"),  # the raw archive, any layout; read only
  project_dir = "",       # config/ and output/ live here (never inside the raw archive)
  qc_profiles = c("frozen_ground", "standard", "strict", "exploratory"),   # the built-in profiles; every profile in qc_profiles.csv is checked
  catalogue_profile = "",                      # the profile that fills the catalogue (Settings); none or unknown: the first built-in
  previews    = TRUE,                          # a pseudosection PNG per survey (not for EarthImager sets) in output/catalogue/previews
  base_offset_h = NA,                          # the zone of clocks no row of timezones.csv matches (Settings > Time zone); NA: unknown, left blank
  incremental = TRUE)
env_gs <- function(k) { v <- Sys.getenv(paste0("GS_", k)); if (nzchar(v)) v else Sys.getenv(paste0("ERT_", k)) }   # ERT_*: the names before ERT became GS
if (nzchar(env_gs("RAW_DIRS")))    CFG$raw_dirs    <- strsplit(env_gs("RAW_DIRS"), ";")[[1]]
if (nzchar(env_gs("PROJECT_DIR"))) CFG$project_dir <- env_gs("PROJECT_DIR")
RUN_ON_SOURCE <- !identical(Sys.getenv("GS_NO_RUN"), "1")

for (p in c("data.table", "ggplot2", "jsonlite", "zip"))
  if (!requireNamespace(p, quietly = TRUE)) stop("The R package ", p, " is missing. Start the app with its launcher (the .bat or .command file), ",
                                              "which installs it as in renv.lock, or run renv::restore() in Ovid_Apps.", call. = FALSE)
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

PIPELINE_DIR <- local({ f <- NULL
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { f <- o; break } }
  if (is.null(f)) { a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) f <- sub("^--file=", "", a[1]) }
  if (is.null(f)) getwd() else dirname(normalizePath(f)) })
APP_NAME <- "ovid // GS"
APP_VERSION <- GS_VERSION

CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(PIPELINE_DIR, "core"), file.path(dirname(PIPELINE_DIR), "core"))
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "util.R"))]
  if (!length(hit)) stop("The shared ovid core folder was not found. Looked in: ", paste(d, collapse = ", "), ". Set OVID_CORE to its path.")
  normalizePath(hit[1]) })
for (.f in c("util", "convert", "audit", "qc", "file_audit", "cache", "timezones", "config", "table_rules", "manifest", "preflight", "theme"))
  source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
CFG$project_dir <- ovid_project_dir(CFG$project_dir)               # none set: ~/Projects/Ovid/Projects/<app>/project (core util.R)
for (.f in c("config", "importers", "catalogue", "qc", "plots", "inversion", "export", "preflight"))
  source(file.path(PIPELINE_DIR, "R", paste0(.f, ".R")), local = TRUE)
CODE_HASH <- code_hash(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "gs_pipeline.R"))   # every file's code (the run manifest)
# What each cached step depends on (core code_fingerprint()): only the code it runs. STEP_HASH is set at the end of this file.
CODE_NAMES <- code_names(c(file.path(PIPELINE_DIR, "R"), CORE_DIR), file.path(PIPELINE_DIR, "gs_pipeline.R"))
step_hashes <- function(env = parent.frame()) list(
  read     = code_fingerprint("read_format", env = env),
  qc       = code_fingerprint(c("run_qc", "qc_survey", "survey_stats"), shallow = "run_pipeline_steps", env = env),
  previews = code_fingerprint(c("plot_pseudosection", "preview_file"), env = env))
CACHE_VERSION <- 1L                                    # the layout of cache/qc and cache/previews

# =============================================================================
#  RUN
# =============================================================================
run_pipeline_steps <- function(cfg = CFG) {
  t0 <- Sys.time(); cfg <- modifyList(CFG, cfg)   # settings not given keep their defaults
  od <- file.path(path.expand(cfg$project_dir), "output"); catd <- file.path(od, "catalogue"); svd <- file.path(od, "surveys")
  for (d in c(catd, svd, file.path(od, "audit"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  raw <- normalizePath(path.expand(cfg$raw_dirs), winslash = "/", mustWork = FALSE)
  if (any(startsWith(paste0(normalizePath(path.expand(cfg$project_dir), winslash = "/", mustWork = FALSE), "/"), paste0(raw, "/"))))
    stop("The project folder must not be inside the raw archive.")
  audit_reset(file.path(od, "audit"))

  say("[1/6] Reading the set-up", col = "accent")
  sc <- setup_check(cfg); fwrite(sc, file.path(od, "audit", "setup_check.csv"))
  if (any(sc$level == "error")) problem("set-up check", paste(sum(sc$level == "error"), "problem(s) in the set-up; see output/audit/setup_check.csv"))
  cfgs <- load_configs(cfg)
  if (!nrow(cfgs$qc) || !any(nzchar(trim(cfgs$qc$profile)))) {                       # an emptied qc_profiles.csv: the built-in profiles, and say so
    problem("QC profiles", "qc_profiles.csv has no rules; the built-in profiles were used. Restore the table on the QC page or from config/history.")
    cfgs$qc <- default_qc() }
  if (!"cpers" %in% cfgs$qc$profile) {                                               # a project made before the cpers profile: add it once
    q <- qc_rule_ids(rbind(cfgs$qc, default_qc()[profile == "cpers"][, rule_id := ""], fill = TRUE), "Q")[, names(cfgs$qc), with = FALSE]
    write_cfg(q, cfg$project_dir, "qc"); cfgs$qc <- q; say("  qc_profiles.csv: added the cpers profile (Herring et al. 2023)", col = "yellow") }

  say("[2/6] Finding files", col = "accent")
  files <- discover_files(cfg, cfgs$file_rules)
  files_gone_check(files_prev(file.path(catd, "files.csv"), "rel"), files$rel)               # core manifest.R
  if (!nrow(files)) stop("No files found in ", paste(cfg$raw_dirs, collapse = ", "))
  say("  ", nrow(files), " files: ", paste(files[, .N, by = role][order(-N), paste(N, role)], collapse = ", "))

  say("[3/6] Reading survey files", col = "accent")
  b <- build_surveys(files, cfg)
  a <- attach_companions(b$surveys, files, b$meas)
  # measurements are written as read; elevations people added are applied when they are viewed or exported, and to the summary here
  geo <- a$meas; for (sid in intersect(names(geo), cfgs$elevations$survey_id)) geo[[sid]] <- apply_elevations(geo[[sid]], cfgs$elevations[survey_id == sid])
  surveys <- geometry_summary(a$surveys, geo)
  surveys <- survey_zones(surveys, cfgs$timezones, cfg$base_offset_h %||% NA_real_)   # which zone each instrument clock was in (Ovid/timezones.R)
  say("  ", nrow(surveys), " surveys; ", paste(surveys[, .N, by = status][, paste(N, status)], collapse = ", "))

  say("[4/6] Linking copies and derived sets", col = "accent")
  surveys <- link_surveys(surveys, a$meas)
  say("  ", paste(surveys[, .N, by = kind][, paste(N, kind)], collapse = ", "))

  profs <- run_profiles(cfg, cfgs$qc$profile)                    # the catalogue's profile first, then every other one (R/qc.R)
  say("[5/6] Quality checks (", paste(profs, collapse = ", "), ")", col = "accent")
  # what people entered about each survey (site, transect, date) decides which site:, transect: and months: rules and which
  # transect-wide electrode rows apply to it, as on the QC page
  for (k in c("site", "transect_id", "acquired_date")) { v <- cfgs$surveys[[k]][match(surveys$survey_id, cfgs$surveys$survey_id)]; v[is.na(v)] <- ""; set(surveys, j = k, value = v) }
  # A survey whose measurements, details and the rows of qc_profiles.csv, manual_flags.csv and electrode_status.csv that can
  # apply to it are the same as in the last run is read back from the cache (cache/qc) instead of being checked again.
  inc <- isTRUE(cfg$incremental); cdir <- file.path(path.expand(cfg$project_dir), "cache"); c_used <- character(); n_q <- 0L; n_pv <- 0L
  if (inc) for (d in c("qc", "previews")) dir.create(file.path(cdir, d), recursive = TRUE, showWarnings = FALSE)
  mh <- lapply(a$meas, hash_of)
  qc_rows_for <- function(s1) { at <- trim(cfgs$qc$applies_to); at[is.na(at)] <- ""
    cfgs$qc[profile %in% c(profs, "none") & (at %in% c("all", "", s1$type, s1$survey_id) | grepl("^months?:", at) |
            (nzchar(s1$site) & at == paste0("site:", s1$site)) | (nzchar(s1$transect_id) & at == paste0("transect:", s1$transect_id)))] }
  key_of <- function(sid, s1) hash_of(list(CACHE_VERSION, STEP_HASH$qc, mh[[sid]], s1[, intersect(c("type", "site", "transect_id", "acquired_date", "datetime_start"), names(s1)), with = FALSE],
    profs, key_rows(qc_rows_for(s1)), key_rows(cfgs$manual[survey_id == sid]),
    key_rows(cfgs$electrodes[trim(survey_id) == sid | (nzchar(s1$transect_id) & trim(transect_id) == s1$transect_id & !nzchar(trim(survey_id)))])))
  qc_cache <- function(sid, s1, compute) { run_tick(match(sid, names(a$meas)), length(a$meas), sid); k <- key_of(sid, s1); x <- if (inc) cache_read(file.path(cdir, "qc"), sid, k) else NULL
    if (!is.null(x)) { n_q <<- n_q + 1L; for (pp in x$problems) problem(pp$where, pp$problem) }
    else { p0 <- length(AUDIT$problems); x <- compute(); x$problems <- AUDIT$problems[seq_along(AUDIT$problems) > p0]; if (inc) cache_write(file.path(cdir, "qc"), sid, k, x) }
    if (inc) c_used <<- c(c_used, cache_file(file.path(cdir, "qc"), sid, k))
    x }
  q <- run_qc(surveys, a$meas, cfgs, profs, cache = qc_cache); surveys <- q$surveys
  if (n_q) say("  ", n_q, " of ", length(a$meas), " surveys unchanged, reused")
  o <- surveys[kind != "derived" & n_read > 0]
  say("  ", profs[1], ": median ", median(o$pct_retained, na.rm = TRUE), "% of measurements pass in the ", nrow(o), " original surveys")

  say("[6/6] Writing the catalogue", col = "accent")
  # new surveys get a row in config/surveys.csv for people to fill in; existing rows are never changed by the pipeline
  new <- setdiff(surveys$survey_id, cfgs$surveys$survey_id)
  if (length(new)) { cfgs$surveys <- rbind(cfgs$surveys, ensure_cols(data.table(survey_id = new, quality = "Unknown"), SURVEY_COLS)[, ..SURVEY_COLS])
    write_cfg(cfgs$surveys, cfg$project_dir, "surveys") }
  files[, survey_id := ifelse(role %in% c("data", "derived"), survey_id_of(md5), NA_character_)]
  surveys[, project_guess := vapply(primary_file, function(r) { p <- strsplit(r, "/", fixed = TRUE)[[1]]; if (length(p) > 1) p[1] else "" }, "")]
  user <- cfgs$surveys[, c("survey_id", setdiff(SURVEY_COLS, "survey_id")), with = FALSE]
  cat_s <- merge(surveys[, !c("folder", "stem", "site", "transect_id", "acquired_date")], user, by = "survey_id", all.x = TRUE, sort = FALSE)   # people's values come from user
  cat_s[, project := fifelse(is.na(project) | project == "", project_guess, project)]
  cat_s[!is.na(acquired_date) & nzchar(acquired_date), year := as.integer(substr(acquired_date, 1, 4))]   # a date people entered wins
  setorder(cat_s, year, project, primary_file, na.last = TRUE)
  fwrite(cat_s, file.path(catd, "surveys.csv"))
  fwrite(files[, .(survey_id, rel, file, format, type, role, readable, status, size, mtime = fmt_ts(mtime), md5)], file.path(catd, "files.csv"))
  for (w in c("projects", "sites", "transects")) fwrite(cfgs[[w]], file.path(catd, paste0(w, ".csv")))
  for (sid in names(a$meas)) {
    d <- file.path(svd, sid); dir.create(d, showWarnings = FALSE)
    fwrite(a$meas[[sid]][, datetime := fmt_ts(datetime)], file.path(d, "measurements.csv"))
    fwrite(b$extra[[sid]], file.path(d, "extra.csv"))
    fwrite(electrodes_with_status(electrodes_of(a$meas[[sid]]), cfgs$electrodes, sid, surveys[survey_id == sid][1]), file.path(d, "electrodes.csv"))
    if (!is.null(a$contact[[sid]])) fwrite(a$contact[[sid]][, datetime := fmt_ts(datetime)], file.path(d, "contact.csv"))
    if (!is.null(a$cmd_geometry[[sid]])) fwrite(a$cmd_geometry[[sid]], file.path(d, "cmd_geometry.csv"))
  }
  fwrite(q$stats, file.path(catd, "survey_stats.csv"))
  fa <- ert_file_audit(files, surveys, a$meas); fwrite(fa, file.path(od, "audit", "file_audit.csv")); file_reconcile_check(fa, "measurements")
  rid <- if (is.null(AUDIT$run_id)) "" else AUDIT$run_id                 # flags.csv ends with rule_id, app and run_id, as in the other apps (no WMO code for ERT)
  for (sid in names(a$meas)) fwrite(q$flags[survey_id == sid][, `:=`(app = "GS", run_id = rid)], file.path(svd, sid, "flags.csv"))
  # a preview whose measurements, flags and title are unchanged is copied from cache/previews instead of drawn again
  if (isTRUE(cfg$previews)) { pv <- file.path(catd, "previews"); dir.create(pv, showWarnings = FALSE); used <- character()
    for (sid in o$survey_id) { s1 <- as.list(surveys[survey_id == sid]); f <- preview_file(s1, used); fl <- q$flags[survey_id == sid & profile == profs[1]]
      k <- hash_of(list(CACHE_VERSION, STEP_HASH$previews, mh[[sid]], key_rows(fl), s1[intersect(c("display_name", "primary_file", "acquired_date", "datetime_start", "survey_name", "model", "array", "n_electrodes", "spacing_m"), names(s1))], profs[1]))
      pc <- sub("\\.rds$", ".png", cache_file(file.path(cdir, "previews"), sid, k))
      if (inc && file.exists(pc)) { file.copy(pc, file.path(pv, f), overwrite = TRUE); n_pv <- n_pv + 1L }
      else { g <- plot_pseudosection(a$meas[[sid]], fl, s1, profs[1]); if (is.null(g)) next
             ggsave(file.path(pv, f), g, width = 9, height = 8.5, dpi = 100); if (inc) file.copy(file.path(pv, f), pc, overwrite = TRUE) }
      if (inc) c_used <- c(c_used, pc)
      used <- c(used, f); surveys[survey_id == sid, preview := f] }
    unlink(file.path(pv, setdiff(list.files(pv), used))) }
  if (inc) for (d in c("qc", "previews")) cache_prune(file.path(cdir, d), c_used)
  if ("preview" %in% names(surveys)) { cat_s[surveys, preview := i.preview, on = "survey_id"]; fwrite(cat_s, file.path(catd, "surveys.csv")) }
  probs <- rbindlist(AUDIT$problems); pf <- file.path(od, "audit", "problems.csv")
  if (nrow(probs)) fwrite(probs, pf) else unlink(pf)          # a clean run leaves no problems from an earlier one
  write_run_manifest(cfg, t0, list(files = nrow(files), surveys = nrow(surveys), measurements = sum(vapply(a$meas, nrow, 0L)),
                                   surveys_checked = length(a$meas), surveys_reused = n_q, previews_reused = n_pv),
                     settings = list(raw_dirs = paste(cfg$raw_dirs, collapse = ";"), catalogue_profile = profs[1]), packages = c("data.table", "jsonlite"))
  say("Done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s. Catalogue: ", catd, col = "green")
  invisible(list(files = files, surveys = cat_s, meas = a$meas, extra = b$extra, flags = q$flags, stats = q$stats))
}

run_pipeline <- function(cfg = CFG, ...) { cfg$project_dir <- ovid_project_dir(cfg$project_dir); run_project(cfg, function() run_pipeline_steps(cfg, ...)) }   # folders checked, project locked, run logged (core manifest.R)   # every run leaves output/logs/<start>.log (core manifest.R)
STEP_HASH <- step_hashes(environment())          # after every function is defined, run_pipeline() included
warn_core_shadows(PIPELINE_DIR)                  # an app function that replaces a core one (core/cache.R)
if (RUN_ON_SOURCE && sys.nframe() == 0L) run_pipeline()
