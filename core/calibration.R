# Ovid/calibration.R  --  Calibration options: values fixed in code that a project may change, each with its source.
# Part of Ovid Core (shared). Sourced by every app's pipeline after config.R; not meant to be run on its own.
#
# One set-up table, config/calibration_options.csv (_hub/Derived_values_references.md, sections 11 and 12):
#   app, value, applies_to, choice, number, source, notes
#   applies_to  a station, site, survey or file name, or * for all; a row for the name wins over *.
#   choice      default       the number and source in the app's catalogue (the literature value);
#               scotty_creek  the published Scotty Creek value in the catalogue (or the row's number, if given);
#               custom        the user's own number; `source` saying where it comes from is encouraged, not required
#                             (without one the value is used and recorded as "no source given").
# scotty_creek is offered only for values with a published Scotty Creek calibration (scotty_number in the catalogue).
# Each app declares its values in a catalogue, a data.table with the columns of CALIB_CATALOGUE_COLS, and adds
# calibration_config(<catalogue>) to its CONFIG_FILES. Outputs record what they used (calib_label()).

CALIBRATION_COLS <- c("app", "value", "applies_to", "choice", "number", "source", "notes")
CALIB_CATALOGUE_COLS <- c("app", "value", "label", "unit", "default_number", "default_source", "scotty_number", "scotty_source", "allowed", "notes")
CALIB_CHOICES <- c("default", "scotty_creek", "custom")

# The starter table: one default row per value of the catalogue.
calibration_default <- function(cat) {
  data.table(app = cat$app, value = cat$value, applies_to = "*", choice = "default", number = cat$default_number,
             source = cat$default_source, notes = paste0(cat$label, if (any(nzchar(cat$unit))) ifelse(nzchar(cat$unit), paste0(" (", cat$unit, ")"), "") else "",
                                                         ifelse(nzchar(cat$scotty_number), paste0(". Scotty Creek option: ", cat$scotty_number, " (", cat$scotty_source, ")"), ""),
                                                         ifelse(nzchar(cat$allowed), paste0(". Allowed: ", cat$allowed), "")))
}

# The CONFIG_FILES entry for an app's catalogue.
calibration_config <- function(cat) list(file = "calibration_options.csv", cols = CALIBRATION_COLS, label = "Calibration options",
                                         default = function() calibration_default(cat),
                                         rules = list(required = c("value", "choice"), choices = list(choice = CALIB_CHOICES, value = cat$value)))

# The value for one name: list(value = text as entered, number = numeric or NA, source, choice, note). Rows for the name come
# before rows for *, and a scotty_creek or custom row before a default one; the first usable row is used. A custom row without
# a number, a number outside `allowed`, or scotty_creek where none is published is skipped with a note. A custom row without a
# source is used; its source reads "no source given".
calib_get <- function(cal, cat, value, name = "") {
  v <- value; def <- cat[cat$value == v][1]
  if (!nrow(def) || is.na(def$value)) stop("calibration value '", v, "' is not in the catalogue")
  mk <- function(x, src, ch, note = "") list(value = x, number = suppressWarnings(as.numeric(x)), source = src, choice = ch, note = note)
  out <- mk(def$default_number, def$default_source, "default")
  if (is.null(cal) || !nrow(cal)) return(out)
  r <- cal[value == v & app %in% c(def$app, "") & tolower(choice) %in% CALIB_CHOICES & applies_to %in% c(name, "*", "")]
  r <- r[order(!applies_to %in% name, tolower(choice) == "default")]
  notes <- character()
  for (i in seq_len(nrow(r))) { ch <- tolower(r$choice[i]); n <- r$number[i]; src <- r$source[i]
    if (ch == "default") break
    if (ch == "scotty_creek") {
      if (!nzchar(def$scotty_number) && !nzchar(n)) { notes <- c(notes, paste0(v, ": no Scotty Creek value is published")); next }
      return(mk(if (nzchar(n)) n else def$scotty_number, if (nzchar(src)) src else def$scotty_source, "scotty_creek", paste(notes, collapse = "; "))) }
    if (!nzchar(n)) { notes <- c(notes, paste0(v, ": a custom row has no number")); next }
    ok <- if (nzchar(def$allowed)) tolower(n) %in% tolower(trimws(strsplit(def$allowed, "[;,]")[[1]])) else !is.na(suppressWarnings(as.numeric(n)))
    if (!ok) { notes <- c(notes, paste0(v, ": '", n, "' is not ", if (nzchar(def$allowed)) paste("one of", def$allowed) else "a number")); next }
    return(mk(n, if (nzchar(src)) src else "no source given", "custom", paste(notes, collapse = "; "))) }
  if (length(notes)) out$note <- paste0(paste(notes, collapse = "; "), "; the default is used")
  out
}

# For several names at once: a named vector of the values (as numbers when every one is a number), with the notes raised
# once each through problem() when it exists.
calib_by <- function(cal, cat, value, names) {
  names <- unique(as.character(names)); g <- lapply(names, function(n) calib_get(cal, cat, value, n))
  notes <- unique(unlist(lapply(g, `[[`, "note"))); notes <- notes[nzchar(notes)]
  if (length(notes) && exists("problem", mode = "function")) for (n in notes) problem("calibration_options.csv", n)
  x <- vapply(g, `[[`, "", "value"); num <- suppressWarnings(as.numeric(x))
  setNames(if (all(!is.na(num))) num else x, names)
}

# "Priestley-Taylor alpha = 0.69 (scotty_creek: Connon et al. 2015)", for outputs and reports.
calib_label <- function(cal, cat, value, name = "") {
  g <- calib_get(cal, cat, value, name); def <- cat[cat$value == value][1]
  sprintf("%s = %s (%s: %s)", def$label, g$value, g$choice, g$source)
}
