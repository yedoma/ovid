# R/preflight.R  --  The set-up check: what in RS's set-up would be ignored, misapplied or is missing. Nothing is changed.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R after the shared core (Ovid/preflight.R); not meant to be run on its own.

setup_check <- function(cfg) {
  chk <- check_collector(c("level", "area", "item", "problem", "fix")); add <- chk$add
  cfgs <- tryCatch(load_configs(cfg), error = function(e) { add("error", "Set-up tables", "", conditionMessage(e), "Fix or remove the file named."); NULL })
  s <- if (is.null(cfgs)) data.table(source_id = character(), path = character()) else cfgs$sources
  # sources stand in for raw folders: one that is not plugged in keeps its last scan, so that is only a note
  if (!nrow(s)) check_folders(add, cfg$raw_dirs, cfg$project_dir, "Folders")
  else check_folders(function(level, ...) add(if (level == "error" && grepl("raw data folder", list(...)[[3]])) "info" else level, ...),
                     s$path[dir.exists(path.expand(s$path))] %||% "", cfg$project_dir, "Folders")
  for (i in which(!dir.exists(path.expand(s$path)))) add("info", "Sources", s$source_id[i], paste0("Not plugged in (", s$path[i], "); its last scan is used."), "Connect the drive to rescan it.")
  for (d in unique(s$source_id[duplicated(s$source_id)])) add("error", "Sources", d, "This source_id is used twice in sources.csv.", "Give each source its own short name.")
  if (is.null(cfgs)) return(chk$result())
  fr <- cfgs$file_rules
  for (i in which(bad_regex(fr$pattern))) add("error", "File rules", fr$pattern[i], "Not a valid pattern; this rule never matches.", "Fix the pattern in file_rules.csv.")
  ok_act <- c("ignore", FORMAT_INFO$role)
  for (i in which(nzchar(fr$action) & !fr$action %in% ok_act)) add("warning", "File rules", fr$pattern[i], paste0("Action '", fr$action[i], "' is not ignore or a known role; the rule does nothing."), "Use ignore or one of the roles listed in the README.")
  st <- cfgs$sites
  for (v in c("latitude", "longitude")) for (i in which(bad_number(st[[v]]))) add("error", "Sites", st$site[i], paste0(v, " '", st[[v]][i], "' is not a number."), "Use decimal degrees.")
  fl <- cfgs$flights
  for (d in unique(fl$flight_id[duplicated(fl$flight_id) & nzchar(fl$flight_id)])) add("error", "Flights", d, "This flight_id is listed twice in flights.csv; only the first row is used.", "Delete the extra row.")
  for (i in which(nzchar(fl$positioning_status) & !fl$positioning_status %in% POSITIONING)) add("warning", "Flights", fl$flight_id[i], paste0("Positioning '", fl$positioning_status[i], "' is not one of the known states."), "Pick a state on the Flight page.")
  lk <- cfgs$links
  for (i in which(nzchar(lk$state) & !lk$state %in% c("confirmed", "rejected"))) add("warning", "Links", lk$file[i], paste0("State '", lk$state[i], "' is not confirmed or rejected; the row is ignored."), "Confirm or reject it on the Flight page.")
  yn <- c("", "yes", "y", "true", "no", "n", "false")
  gb <- if (is.null(cfgs$gnss_bases)) empty_cfg(GNSS_BASE_COLS) else ensure_cols(copy(cfgs$gnss_bases), GNSS_BASE_COLS)
  for (i in which(nzchar(trim(gb$method)) & !tolower(trim(gb$method)) %in% tolower(BASE_METHODS)))
    add("warning", "GNSS bases", gb$file[i], paste0("Method '", gb$method[i], "' is not one of the known ones; the base is reported as not recorded."), paste0("Use one of: ", paste(BASE_METHODS, collapse = ", "), "."))
  for (i in which(!tolower(trim(gb$post_processed)) %in% yn))
    add("warning", "GNSS bases", gb$file[i], paste0("post_processed '", gb$post_processed[i], "' is not yes or no; it is read as no."), "Write yes or no in gnss_bases.csv.")
  for (i in which(!nzchar(trim(gb$file)) & (nzchar(trim(gb$method)) | nzchar(trim(gb$base)))))
    add("warning", "GNSS bases", gb$base[i], "A row with no file name or pattern applies to no file.", "Give the file name, a pattern (* ?), or * for all.")
  dc <- if (is.null(cfgs$drone_cameras)) empty_cfg(DRONE_CAMERA_COLS) else ensure_cols(copy(cfgs$drone_cameras), DRONE_CAMERA_COLS)
  for (i in which(!tolower(trim(dc$drone)) %in% yn))
    add("warning", "Drone cameras", paste(dc$make[i], dc$model[i]), paste0("drone '", dc$drone[i], "' is not yes or no; the camera is counted as a drone."), "Write yes or no in drone_cameras.csv.")
  for (i in which(!nzchar(trim(dc$make)) & nzchar(trim(dc$model))))
    add("warning", "Drone cameras", dc$model[i], "A row with a model but no make matches no photo.", "Fill in the make as the EXIF gives it.")
  tc <- tz_check(cfgs$timezones)
  for (i in seq_len(nrow(tc))) add("warning", "Time zones", paste("row", tc$row[i], tc$item[i]), tc$problem[i], "Fix the row in timezones.csv.")
  with_declared(chk$result(), cfg$project_dir)                     # with every table's declared rules (core table_rules.R)
}
