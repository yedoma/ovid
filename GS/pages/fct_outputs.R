# GS: reading the catalogue and a survey's measurements, and their display names. Sourced by app.R.
load_catalogue <- function() {
  f <- file.path(CAT, "surveys.csv")   # no catalogue built yet: an empty table with the columns the pages use
  s <- if (file.exists(f)) fread(file = f, colClasses = list(character = c("datetime_start", "datetime_end"))) else
    data.table(survey_id = character(), kind = character(), type = character(), format = character(), array = character(), primary_file = character(),
               datetime_start = character(), datetime_end = character(), year = integer(), n_read = integer(), pct_retained = numeric())
  s[datetime_start == "", datetime_start := NA_character_]
  # what people entered (config/surveys.csv) is read fresh, so changes made outside the app show without a rebuild
  cf <- tryCatch(load_configs(CFG)$surveys, error = function(e) NULL)
  if (!is.null(cf) && nrow(cf)) for (k in setdiff(SURVEY_COLS, c("survey_id", "project"))) {
    if (!k %in% names(s)) s[, (k) := ""]; s[, (k) := as.character(get(k))]
    i <- match(s$survey_id, cf$survey_id); hit <- !is.na(i); set(s, which(hit), k, cf[[k]][i[hit]]) }
  if (!is.null(cf) && nrow(cf)) { i <- match(s$survey_id, cf$survey_id); hit <- !is.na(i) & nzchar(cf$project[i] %|na|% "")   # a project people set wins over the folder guess
    set(s, which(hit), "project", cf$project[i[hit]]) }
  for (v in c("display_name", "acquired_date")) { if (!v %in% names(s)) s[, (v) := ""]; s[, (v) := as.character(get(v))]; s[is.na(get(v)), (v) := ""] }
  s[, `:=`(name = fifelse(nzchar(display_name), display_name, sub("\\.(stg|csv|txt|dat)$", "", basename(primary_file), ignore.case = TRUE)),
           date = fifelse(nzchar(acquired_date), acquired_date, fifelse(is.na(datetime_start), "undated", substr(datetime_start, 1, 10))),
           folder = sub("/.*", "", primary_file), instrument = fifelse(type == "agi", "AGI SuperSting", fifelse(type == "syscal", "IRIS Instruments", "other")))]
  for (v in c("instrument_model", "quality", "project", "site", "transect_id", "operator", "conditions", "ground_conditions", "notes", "quality_reasons", "quality_notes",
              "other_paths", "companions", "qc_flags", "message", "serial", "firmware", "software", "processing", "model", "manufacturer")) {
    if (!v %in% names(s)) s[, (v) := ""]; s[, (v) := as.character(get(v))]; s[is.na(get(v)), (v) := ""] }
  s[quality == "", quality := "Unknown"]
  s[, file_model := model]
  if ("instrument_model" %in% names(s)) s[nzchar(instrument_model %|na|% ""), model := instrument_model]   # an instrument people corrected wins
  s[, `:=`(kind_lbl = NICE_KIND[kind] %|na|% kind, format_lbl = NICE_FORMAT[format] %|na|% format, array_lbl = nice_array(array),
           model_lbl = sub("^AGI AGI ", "AGI ", model))]
  s[order(kind != "original", is.na(datetime_start), datetime_start, name)]
}
ELEV <- reactiveVal(NULL)       # set in the server; config/elevations.csv
# Readable labels for codes kept in the tables
`%|na|%` <- function(a, b) ifelse(is.na(a), b, a)
NICE_KIND <- c(original = "Original", copy = "Copy", subset = "Subset", merged = "Merged", derived = "Processed set")
NICE_FORMAT <- c(agi_stg = "AGI SuperSting (.stg)", agi_stg_earthimager = "EarthImager set (.stg)", syscal_prosys_txt = "IRIS Prosys export",
                 res2dinv_dat = "Res2DInv input (.dat)", syscal_bin = "IRIS binary (.bin)")
ARRAY_NAMES <- c(`dipole-dipole` = "Dipole-dipole", `pole-dipole` = "Pole-dipole", `pole-pole` = "Pole-pole", wenner = "Wenner", schlumberger = "Schlumberger", mixed = "Mixed")
nice_array <- function(x) { x <- ifelse(is.na(x), "", x); m <- sub("^mixed \\(mostly (.*)\\)$", "\\1", x)
  ifelse(grepl("^mixed \\(mostly", x), paste("Mixed, mostly", ifelse(m %in% c("wenner", "schlumberger"), ARRAY_NAMES[m], tolower(ARRAY_NAMES[m] %|na|% m))), ARRAY_NAMES[x] %|na|% x) }
read_meas <- function(sid) { d <- fread(file = file.path(PROJECT, "output", "surveys", sid, "measurements.csv")); e <- isolate(ELEV())
  if (!is.null(e) && sid %in% e$survey_id) apply_elevations(d, e[survey_id == sid]) else d }
read_part <- function(sid, f) { p <- file.path(PROJECT, "output", "surveys", sid, f); if (file.exists(p)) fread(file = p) else NULL }
