# R/metadata.R  --  The metadata folder: the site registry workbook, NTGS-style metadata workbooks and field notes (.docx).
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# Nothing here is written back. The registry (a sheet with Name, Type and SN columns, for example a site record workbook)
# seeds sites.csv and sensors.csv on the first run; every other workbook is indexed field by field so the Site details
# page can show everything known about a site; .docx field notes are kept as paragraphs and searched for serials and site names.

REG_TYPE <- c(rbr = "rbr", shallowhobo = "hobo", hobo = "hobo", cryogeek = "geoprecision", geoprecision = "geoprecision", em50 = "em50",
              shallowcampbell = "campbell", campbell = "campbell")
REG_LC <- c(plateau = "Plateau", bog = "Bog", fen = "Fen", linear_dist = "Other")

xlsx_files <- function(dir) { if (!length(dir) || is.na(dir) || !dir.exists(path.expand(dir))) return(character())
  f <- list.files(path.expand(dir), "\\.xlsx?$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE); f[!grepl("^~\\$", basename(f))] }

# A sheet as text, with the header row found among the first rows (the first row holding one of the wanted names).
read_sheet_header <- function(path, sheet, want) {
  # safe: looking for a registry; a sheet that cannot be read is not one
  d <- tryCatch(as.data.table(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 2) return(NULL)
  for (r in seq_len(min(6, nrow(d)))) { h <- trim(unlist(d[r])); if (any(tolower(h) %in% tolower(want), na.rm = TRUE)) {
    h[is.na(h) | !nzchar(h)] <- paste0("col", which(is.na(h) | !nzchar(h)))
    b <- d[(r + 1):nrow(d)]; setnames(b, make.unique(h)); for (v in names(b)) set(b, j = v, value = trim(fifelse(is.na(b[[v]]), "", b[[v]])))
    return(b[rowSums(b != "") > 0]) } }
  NULL
}

find_registry <- function(dir) {
  # safe: looking for a registry; a workbook that cannot be read has none
  for (f in xlsx_files(dir)) for (s in tryCatch(readxl::excel_sheets(f), error = function(e) character())) {
    b <- read_sheet_header(f, s, c("Name", "SN"))
    if (!is.null(b) && all(c("name", "sn") %in% tolower(names(b))) && any(tolower(names(b)) == "type")) {
      setnames(b, tolower(names(b))); return(list(file = f, sheet = s, data = b[nzchar(name)])) } }
  NULL
}

# Every workbook in the folder, one row per site, sheet and field, for the Site details page.
index_metadata <- function(dir) {
  out <- list()
  # safe: looking for a registry; a workbook that cannot be read has none
  for (f in xlsx_files(dir)) for (s in tryCatch(readxl::excel_sheets(f), error = function(e) character())) {
    b <- read_sheet_header(f, s, c("Site ID", "Name", "Site"))
    if (is.null(b)) next
    kc <- names(b)[tolower(names(b)) %in% c("site id", "name", "site")][1]
    key <- sub(":.*$", "", b[[kc]])
    long <- rbindlist(lapply(setdiff(names(b), kc), function(v) data.table(site_key = key, field = v, value = b[[v]])))
    long <- long[nzchar(site_key) & nzchar(value) & value != "-999" & !grepl("^col[0-9]+$", field)]
    if (nrow(long)) out[[length(out) + 1L]] <- long[, `:=`(file = basename(f), sheet = s)]
  }
  if (!length(out)) return(data.table(site_key = character(), field = character(), value = character(), file = character(), sheet = character()))
  x <- rbindlist(out)
  dt <- grepl("date|start|end", x$field, ignore.case = TRUE) & grepl("^[0-9]{5}$", x$value)      # Excel date serials
  x[dt, value := format(as.Date(as.numeric(value), origin = "1899-12-30"))]
  x
}

# Field notes: every paragraph of every .docx (and .txt / .md) in the folder.
read_field_notes <- function(dir) {
  if (!length(dir) || is.na(dir) || !dir.exists(path.expand(dir))) return(data.table(file = character(), para = integer(), text = character()))
  f <- list.files(path.expand(dir), "\\.(docx|txt|md)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE); f <- f[!grepl("^~\\$", basename(f))]
  rbindlist(lapply(f, function(p) {
    txt <- tryCatch({
      if (grepl("docx$", p, ignore.case = TRUE)) {
        tmp <- file.path(tempdir(), paste0("docx_", as.integer(runif(1) * 1e9))); zip::unzip(p, files = "word/document.xml", exdir = tmp)
        x <- paste(readLines(file.path(tmp, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
        x <- gsub("</w:p>", "\n", x); x <- gsub("<w:tab/>", "\t", x); x <- gsub("<[^>]+>", "", x)
        x <- gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", x)))
        strsplit(x, "\n", fixed = TRUE)[[1]]
      # safe: a field-note file that cannot be read gives no notes
      } else read_lines_any(p) }, error = function(e) character())
    txt <- trim(txt); k <- which(nzchar(txt))
    if (!length(k)) NULL else data.table(file = basename(p), para = k, text = txt[k]) }), fill = TRUE)
}

# Paragraphs of the field notes that mention a serial or a site name, with the year heading they fall under.
notes_for <- function(notes, terms) {
  terms <- unique(trim(terms[!is.na(terms) & nchar(trim(terms)) >= 4]))
  if (!nrow(notes) || !length(terms)) return(notes[0])
  yr <- notes[, .(year = ifelse(grepl("^(19|20)[0-9]{2}$", text), text, NA_character_)), by = .(file, para)]
  notes <- merge(notes, yr, by = c("file", "para")); setorder(notes, file, para)
  notes[, year := zoo_fill(year), by = file]
  pat <- paste0("(", paste(gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", terms), collapse = "|"), ")")
  notes[grepl(pat, text, ignore.case = TRUE)]
}
zoo_fill <- function(x) { v <- NA_character_; for (i in seq_along(x)) if (!is.na(x[i])) v <- x[i] else x[i] <- v; x }

# Sensor names per logger type, in channel order (used to place a registry's depth list on the channels).
sensor_names <- function(type, n) switch(type, em50 = paste0("P", seq_len(n)), hobo = paste0("T", seq_len(n)), paste0("T", seq_len(n)))

# ---- the logger list (metadata sheet, core/metasheet.R): what people know about each logger, with or without a registry ----
# Every column but the serial is optional. A logger listed with a site (or only a name) is placed there; a depth list gives
# its sensors' depths, top one first. Each project can relabel, hide or add columns (Sensors and depths > Deployment sheet).
META_FIELDS <- data.table(
  field   = c("serial", "name", "site", "depths", "model", "start", "end", "notes"),
  label   = c("Serial", "Name", "Site", "Sensor depths", "Logger model", "From", "Until", "Notes"),
  type    = c("text", "text", "text", "list", "text", "date", "date", "text"),
  unit    = c("", "", "", "m", "", "", "", ""),
  use     = c("ties the row to the logger's files", "a name for the logger; its site until one is given", "the site its sensors are at",
              "the depth of each sensor, the top one first (0.1, 0.2, 0.5); blank: the model's template, else the default steps",
              "shown with the logger", "it was at the site from this day (blank: from its first record)", "until this day (blank: to its last record)", "shown with the logger"),
  aliases = c("sn|serial number|s/n|logger serial|logger sn", "logger name|alias|label", "location|site name|site id|station|borehole|hole",
              "depth|depths|depths m|all depths|all_depths_sep_by_comma|sensor depth", "logger|model|sensortype|sensor type|logger model",
              "record start|record_start|installed|install date|start date|deployed", "record end|record_end|removed|end date|retrieved", "remarks|comments|comment"),
  show    = "TRUE",
  identifies = c("TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "FALSE", "FALSE"))

# Rows of the shared deployment sheet (core deployments.R) as rows of this logger list: the station (else the site) is the
# logger's site from its From to its To.
gt_deploy_rows <- function(r) data.table(serial = r$serial, site = dep_where(r), start = dep_day(r$.from), end = dep_day(r$.to), notes = r$notes)

# Serials GT already places without the deployment sheet's From and To: a row of the sheet, or a sensors.csv row with a site (set by
# hand or from the registry; not the site named after the logger when nothing was known). The Workbench's Needs you leaves
# them out of "found in the files, in no row of the deployment sheet".
gt_placed_serials <- function(project_dir) {
  se <- try_or(read_cfg(cfg_path(project_dir, "sensors")), data.table(), "sensors.csv")
  a <- if (nrow(se) && all(c("serial", "site", "logger_type") %in% names(se))) se[nzchar(site) & site != auto_site(logger_type, norm_serial(serial)), serial] else character()
  ms <- try_or(meta_sheet(project_dir, META_FIELDS, report = FALSE, map = GT_MAP), where = "deployment sheet")
  b <- if (!is.null(ms) && "serial" %in% names(ms)) as.character(ms$serial) else character()
  x <- norm_serial(c(a, b)); unique(x[!is.na(x) & nzchar(x)])
}
# Where a logger the deployment sheet does not list is now (Split here on Needs you, core grouping.R): its site in sensors.csv.
dep_split_place <- function(project_dir, serial) {
  se <- try_or(read_cfg(cfg_path(project_dir, "sensors")), data.table(), "sensors.csv")
  s <- if (nrow(se) && all(c("serial", "site") %in% names(se))) se$site[norm_serial(se$serial) == norm_serial(serial) & nzchar(se$site)] else character()
  if (length(s)) list(site = s[1]) else list()
}
gt_needs_check <- function(ck, project_dir) {
  if (is.null(ck) || is.null(ck$unclaimed) || !nrow(ck$unclaimed)) return(ck)
  ck$unclaimed <- ck$unclaimed[!norm_serial(serial) %in% gt_placed_serials(project_dir)]; ck
}

# The registry as rows of sites.csv and sensors.csv.
seed_from_registry <- function(reg, ntgs = NULL) {
  r <- reg$data; g <- function(v) if (v %in% names(r)) r[[v]] else rep("", nrow(r))
  type <- unname(REG_TYPE[gsub("[^a-z0-9]", "", tolower(g("type")))]); type[is.na(type)] <- ""
  ex <- function(x) { n <- suppressWarnings(as.numeric(x)); ifelse(!is.na(n) & n > 20000 & n < 80000, format(as.Date(n, origin = "1899-12-30")), x) }
  grp <- trim(sub("\\s*-.*$", "", g("lc_notes"))); grp[!nzchar(grp)] <- g("name")[!nzchar(grp)]
  sites <- data.table(site = g("name"), site_group = grp, landcover = unname(REG_LC[tolower(g("lc"))]) %||% "", instrument = ifelse(nzchar(type), TYPE_LABEL[type], ""),
                      status = g("status"), utm_zone = g("zone"), easting = g("easting"), northing = g("northing"), latitude = "", longitude = "",
                      install_date = ex(g("record_start")), drilling_method = "", borehole_depth_m = "", sensor_type = paste(g("sensortype"), g("logger")),
                      sensor_accuracy = "", calibration = "", description = paste(g("lc_notes"), g("notes")), source = paste(basename(reg$file), reg$sheet, sep = " / "))
  sites[is.na(landcover), landcover := ""]; sites[, description := trim(description)]
  sites[, `:=`(sensor_type = trim(sensor_type))]
  if (!is.null(ntgs) && nrow(ntgs)) {                                   # NTGS template: installation details by site id
    for (i in seq_len(nrow(sites))) { k <- sites$site[i]; x <- ntgs[site_key == k]
      if (!nrow(x)) next
      val <- function(f) { v <- x[tolower(field) == tolower(f), value]; if (length(v)) v[1] else "" }
      set(sites, i, "drilling_method", val("Drilling Method")); set(sites, i, "borehole_depth_m", val("Borehole Depth (m)"))
      set(sites, i, "sensor_accuracy", val("Sensor Accuracy (\u02daC)")); set(sites, i, "calibration", val("Calibration Values"))
      if (!nzchar(sites$latitude[i])) { set(sites, i, "latitude", val("Latitude")); set(sites, i, "longitude", val("Longitude")) } }
  }
  sensors <- list()
  for (i in seq_len(nrow(r))) {
    sn <- norm_serial(g("sn")[i]); if (!nzchar(sn) || !nzchar(type[i])) next
    dl <- trim(strsplit(g("all_depths_sep_by_comma")[i], ",")[[1]]); dl <- dl[depth_ok(dl)]             # kept as text: a leading + stays
    base <- list(logger_type = type[i], make = MAKES[[type[i]]], model = g("sensortype")[i], serial = sn, site = g("name")[i],
                 start = ex(g("record_start")[i]), end = ex(g("record_end")[i]))
    if (length(dl)) sensors[[length(sensors) + 1L]] <- as.data.table(c(base, list(sensor = sensor_names(type[i], length(dl)), depth_m = as.character(dl),
                                                                       depth_source = "registry", notes = "depth list from the registry")))
    else sensors[[length(sensors) + 1L]] <- as.data.table(c(base, list(sensor = "", depth_m = "", depth_source = "",
                                                                     notes = paste0("site from the registry", if (nzchar(g("n_sensors")[i])) paste0(" (registry: ", g("n_sensors")[i], " sensors", if (nzchar(g("min_depth_m")[i])) paste0(", ", signif(as.numeric(g("min_depth_m")[i]), 4), " to ", signif(as.numeric(g("max_depth_m")[i]), 4), " m") else "", ", no depth list)") else ""))))
  }
  sensors <- if (length(sensors)) rbindlist(sensors, fill = TRUE) else empty_cfg(SENSOR_COLS)
  list(sites = ensure_cols(sites, SITE_COLS)[, SITE_COLS, with = FALSE], sensors = ensure_cols(sensors, SENSOR_COLS)[, SENSOR_COLS, with = FALSE])
}

default_templates <- function() data.table(make = "Onset", model = "U12-008", depths = "0.05, 0.20, 0.40, 1.00",
                                            notes = "HOBO TMC-HD shallow pits (2017 NTGS ground temperature report). Delete this row to use the default steps instead.")
