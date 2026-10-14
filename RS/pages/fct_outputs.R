# RS: reading the catalogue (flights, images, links) and the small display helpers. Sourced by app.R.
# ---- the catalogue -------------------------------------------------------------------------------------------------------
rd <- function(f, ...) { p <- file.path(CAT(), f); if (file.exists(p)) fread(p, ...) else data.table() }
load_cat <- function(points_per_flight = CFG$points_per_flight) {
  fl <- rd("flights.csv", colClasses = list(character = c("date", "start", "display_name", "project", "site", "notes", "status_note", "copies", "qa_flags", "mission", "mission_set", "mission_status", "mission_evidence")))
  if (!nrow(fl)) return(NULL)
  for (v in c("display_name", "project", "site", "aircraft", "sensor", "notes", "status_note", "copies", "qa_flags", "date", "exclude_from_archive", "time_source"))
    { if (!v %in% names(fl)) fl[, (v) := ""]; fl[, (v) := as.character(get(v))]; fl[is.na(get(v)), (v) := ""] }
  if (!"kind" %in% names(fl)) fl[, kind := "flight"]; fl[is.na(kind) | !nzchar(kind), kind := "flight"]          # catalogues made before photo sets
  if (!"kind_found" %in% names(fl)) fl[, kind_found := kind]
  fl[, `:=`(name = fifelse(nzchar(display_name), display_name, trimws(paste(site %|na|% "unnamed site", fifelse(is.na(flight_no), "", paste0("#", flight_no))))),
            date = fifelse(nzchar(date), date, "undated"), year = fifelse(is.na(year), "undated", as.character(year)))]
  im <- rd("images.csv", select = c("flight_id", "primary", "lat", "lon"))
  pts <- if (nrow(im)) thin_points(im[, primary := as.logical(primary)], points_per_flight) else data.table(flight_id = character(), lat = numeric(), lon = numeric())
  list(flights = fl, links = rd("links.csv"), geometry = rd("geometry.csv", colClasses = list(character = c("flight_id", "ref", "line"))),
       points = pts, sources = rd("sources.csv"), gis_layers = rd("gis_layers.csv"), gis_features = rd("gis_features.csv", colClasses = list(character = c("feature", "line"))),
       datasets = rd("datasets.csv", colClasses = list(character = c("acquired", "footprint", "crs", "detail", "product", "platform"))), unassigned = rd("unassigned.csv"), logs = rd("logs.csv"), made = file.mtime(file.path(CAT(), "flights.csv")))
}
badge <- function(s) span(class = "badge-st", style = sprintf("border-color:%s;color:%s", STATUS_COL[s] %|na|% PAL$gray, STATUS_COL[s] %|na|% PAL$gray), s)
fact <- function(k, v) if (length(v) && !is.na(v) && nzchar(as.character(v))) div(class = "fact", span(class = "k", k), span(class = "v", as.character(v)))
# The problems of the last run (output/audit/problems.csv: where, problem); a clean run leaves none.
run_problems_read <- function(project_dir) {
  p <- file.path(path.expand(project_dir), "output", "audit", "problems.csv")
  if (file.exists(p)) tryCatch(fread(p, colClasses = "character"), error = function(e) data.table(where = character(), problem = character()))
  else data.table(where = character(), problem = character())
}
