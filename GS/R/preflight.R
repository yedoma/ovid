# R/preflight.R  --  The set-up check: what in GS's set-up would be ignored, misapplied or is missing. Nothing is changed.
# Part of Ovid Core // GS. Sourced by gs_pipeline.R after the shared core (Ovid/preflight.R); not meant to be run on its own.

setup_check <- function(cfg) {
  chk <- check_collector(c("level", "area", "item", "problem", "fix")); add <- chk$add
  check_folders(add, cfg$raw_dirs, cfg$project_dir, "Folders")
  cfgs <- tryCatch(load_configs(cfg), error = function(e) { add("error", "Set-up tables", "", conditionMessage(e), "Fix or remove the file named."); NULL })
  if (is.null(cfgs)) return(chk$result())
  fr <- cfgs$file_rules
  for (i in which(bad_regex(fr$pattern))) add("error", "File rules", fr$pattern[i], "Not a valid pattern; this rule never matches.", "Fix the pattern in file_rules.csv.")
  sv <- cfgs$surveys
  for (d in unique(sv$survey_id[duplicated(sv$survey_id) & nzchar(sv$survey_id)])) add("error", "Surveys", d, "This survey_id is listed twice in surveys.csv; only the first row is used.", "Delete the extra row.")
  for (i in which(nzchar(sv$acquired_date) & is.na(as.Date(sv$acquired_date, optional = TRUE)))) add("warning", "Surveys", sv$survey_id[i], paste0("acquired_date '", sv$acquired_date[i], "' is not a date (YYYY-MM-DD)."), "Fix it on the survey's page.")
  tr <- sv$transect_id[nzchar(sv$transect_id)]
  for (t in setdiff(unique(tr), cfgs$transects$transect_id)) add("warning", "Surveys", t, "A survey names this transect, but it is not in transects.csv.", "Add the transect on the Map page (Transect details).")
  for (p in setdiff(unique(sv$project[nzchar(sv$project)]), cfgs$projects$project)) add("info", "Surveys", p, "A survey names this project, but it is not in projects.csv.", "Add the project, or leave it as is.")
  st <- cfgs$sites
  for (v in intersect(c("latitude", "longitude"), names(st))) for (i in which(bad_number(st[[v]]))) add("error", "Sites", st$site[i], paste0(v, " '", st[[v]][i], "' is not a number."), "Use decimal degrees.")
  el <- cfgs$elevations
  for (v in intersect(c("electrode", "distance_m", "elevation_m"), names(el))) for (i in which(bad_number(el[[v]]))) add("error", "Elevations", el$survey_id[i], paste0(v, " '", el[[v]][i], "' is not a number; the row is ignored."), "Fix the value.")
  for (p in setdiff(cfg$qc_profiles, c(cfgs$qc$profile, "none"))) add("error", "QC profiles", p, "This profile is selected but is not in qc_profiles.csv.", "Add it, or pick another profile.")
  cp <- trim(cfg$catalogue_profile %||% "")
  if (length(cp) == 1 && nzchar(cp) && !cp %in% cfgs$qc$profile) add("warning", "QC profiles", cp, paste0("The catalogue's profile (Settings) is not in qc_profiles.csv; ", cfg$qc_profiles[1], " is used."), "Pick another profile on Settings.")
  tc <- tz_check(cfgs$timezones)
  for (i in seq_len(nrow(tc))) add("warning", "Time zones", paste("row", tc$row[i], tc$item[i]), tc$problem[i], "Fix the row in timezones.csv.")
  with_declared(chk$result(), cfg$project_dir)                     # with every table's declared rules (core table_rules.R)
}
