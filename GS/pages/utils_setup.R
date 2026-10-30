# GS: the set-up tables, their editors and what a rebuild redid. Sourced by app.R.
# ---- set-up tables -------------------------------------------------------------------------------------------------------
# Every table in config/ (CONFIG_FILES), each with the editor shared by all the apps (core/editor.R) on the Set-up tables
# page. Both interfaces build their editors from this list, and the Workbench writes its corrections through them. A saved
# change applies on the next rebuild.
SETUP_TABLES <- data.table(id = c("surveys", "qc", "manual", "electrodes", "transects", "sites", "projects", "elevations", "file_rules", "timezones"))
SETUP_TABLES[, `:=`(file = vapply(id, function(k) CONFIG_FILES[[k]]$file, ""), label = vapply(id, function(k) CONFIG_FILES[[k]]$label, ""),
  help = c("What people entered about each survey: its name, date, project, site, transect, conditions, quality and notes. The Survey page and the Archive write these; site, transect and date decide which site:, transect: and months: rules apply.",
           "One row per rule and limit of each profile. applies_to: all, agi, syscal, a survey_id, site:<site>, transect:<transect_id> or months:<list>; the most specific row of a rule wins. The QC page saves its sliders here as a profile.",
           "Measurements flagged by hand: survey_id, meas_ids (for example 5-20,31) or an electrode, the flag and the reason; active FALSE keeps a row without applying it. The Workbench writes these.",
           "Electrodes turned off, by survey_id, or by transect_id over a period (from, to); status off. Every measurement that uses one is flagged. The QC page writes these, and has no tab here.",
           "Transect lines: their start and end points and layout. The Map page writes these.",
           "Sites: where they are, and what is there.",
           "Projects: their code, year, team and purpose.",
           "Electrode elevations, by electrode number or by distance along the line. Survey, Geometry writes these.",
           "File rules: a pattern of file paths, and the action: ignore, or the role of the files (data, companion, derived, notes, result).",
           "Instrument clocks: the offset from UTC of a file's times, by instrument, serial and period (core/timezones.R)."))]
# electrode_status.csv has no tab: electrodes are turned off on the QC page; its editor stays for the Workbench's Unflag.
SETUP_TABLES[, tab := id != "electrodes"]
SETUP_TABLES[, wb_only := id %in% c("qc", "manual", "electrodes", "elevations")]   # these change what the data say or how they are read: edited by hand only in the Workbench (M 28 Oct)
setup_editors <- function(path_of, tick, mark) setNames(lapply(SETUP_TABLES$id, function(k)
  ovid_edit_server(k, reactive(path_of(k)), tick, on_change = mark, wb_only = SETUP_TABLES[id == k]$wb_only)), SETUP_TABLES$id)
# ---- electrodes turned off (the QC page) -------------------------------------------------------------------------------------
# An electrode_status.csv row (ELECTRODE_COLS): electrodes turned off in survey sid, or on transect tr from one date to
# another (blank: open-ended), by scope.
electrode_status_row <- function(sid, tr, electrodes, from, to, reason, author, scope = "survey") { t <- identical(scope, "transect")
  data.table(survey_id = if (t) "" else sid, transect_id = if (t) tr else "", electrode = gsub("\\s+", "", electrodes), status = "off",
             from = if (t) trim(from) else "", to = if (t) trim(to) else "", reason = trim(reason), author = trim(author)) }
# The rows of electrode_status.csv that turn electrodes off in survey sid (s1: its transect and date): its own rows, and those
# of its transect over a period that includes its date (as electrodes_off() reads them), with what each applies to.
electrode_rows_for <- function(status, sid, s1 = NULL) {
  e <- ensure_cols(as.data.table(status), ELECTRODE_COLS)
  if (!nrow(e)) return(cbind(data.table(applies_to = character()), e[0]))
  hit <- vapply(seq_len(nrow(e)), function(i) trim(e$survey_id[i]) == sid || (!nzchar(trim(e$survey_id[i])) && length(electrodes_off(e[i], sid, s1)) > 0), NA)
  x <- e[hit]
  cbind(data.table(applies_to = ifelse(nzchar(trim(x$survey_id)), "this survey",
    sprintf("transect %s, %s to %s", x$transect_id, ifelse(nzchar(trim(x$from)), x$from, "the start"), ifelse(nzchar(trim(x$to)), x$to, "now")))), x)
}

# Set-up saved after the last rebuild (the newest file in config/, and the last rebuild's manifest), as seconds.
# What the last rebuild redid, from its manifest: surveys whose measurements and set-up had not changed are reused.
run_redone <- function(pd) {
  f <- file.path(path.expand(pd), "output", "audit", "run_manifest.csv"); if (!file.exists(f)) return("")
  m <- tryCatch(fread(file = f, colClasses = "character"), error = function(e) setnames(data.table(character(), character()), c("key", "value")))
  v <- function(k) suppressWarnings(as.integer(m[key == k, value][1]))
  n <- v("surveys_checked"); r <- v("surveys_reused"); if (anyNA(c(n, r))) return("")
  wb_redone(n - r, n, "surveys")
}
# The headings and types the Import on transects.csv matches (core import.R): coordinates come in from the person's own CSV or
# Excel file with the columns matched, never guessed into place.
TRANSECT_FIELDS <- data.table(
  field   = c("transect_id", "name", "site", "start_lat", "start_lon", "end_lat", "end_lon", "length_m", "spacing_m", "n_electrodes", "notes"),
  label   = c("Transect", "Name", "Site", "Start latitude", "Start longitude", "End latitude", "End longitude", "Length (m)", "Electrode spacing (m)", "Electrodes", "Notes"),
  type    = c("text", "text", "text", "number", "number", "number", "number", "number", "number", "number", "text"),
  unit    = c("", "", "", "", "", "", "", "m", "m", "", ""),
  use     = c("ties the row to its surveys", "kept with the row", "the area the line is in", "decimal degrees, north positive", "decimal degrees, east positive (west is negative)",
              "decimal degrees, north positive", "decimal degrees, east positive (west is negative)", "along the line", "between electrodes", "on the line", "kept with the row"),
  aliases = c("transect|line|line id|line name|profile|transect name", "label|title", "site name|area|study area|field site",
              "start lat|lat start|latitude start|lat 1|lat1|y1|start y|first latitude", "start lon|start long|lon start|longitude start|lon 1|lon1|x1|start x|first longitude",
              "end lat|lat end|latitude end|lat 2|lat2|y2|end y|last latitude", "end lon|end long|lon end|longitude end|lon 2|lon2|x2|end x|last longitude",
              "length|line length|length m", "spacing|electrode spacing|a spacing|spacing m", "electrodes|number of electrodes|n electrodes|nel", "note|remarks|comments|comment|description"),
  show    = "TRUE",
  identifies = c("TRUE", rep("FALSE", 10)))
IMPORT_SPECS <- c(get0("IMPORT_SPECS", ifnotfound = list()), list("transects.csv" = TRANSECT_FIELDS))
