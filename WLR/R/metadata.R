# R/metadata.R  --  Reading the WL_Metadata workbook (one sheet per year), as it is.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# A year's sheet describes the period from the previous September download sweep to that year's sweep: the 2025 sheet
# covers the files downloaded in 2025. Each row is a download visit with its manual measurements, in cm:
#   T_water   T-coupler to water table      T_TOC   T-coupler to top of casing    TOC_water  casing to water table
#   T_veg     T-coupler to vegetation       TOC_veg casing to vegetation          T_ST       T-coupler to sensor tip
# Extra visits within a year are extra rows under the same site.

META_NUM <- c("T_water", "T_TOC", "TOC_water", "T_veg", "TOC_veg", "T_ST")

find_metadata_file <- function(dir) {
  if (!nzchar(dir %||% "") || !dir.exists(path.expand(dir))) return(NA_character_)
  f <- list.files(path.expand(dir), pattern = "\\.xlsx?$", full.names = TRUE, ignore.case = TRUE)
  f <- f[!startsWith(basename(f), "~$")]
  if (!length(f)) NA_character_ else f[order(-as.numeric(file.info(f)$mtime))][1]
}

excel_date <- function(x) {
  x <- trim(as.character(x)); out <- as.Date(rep(NA_real_), origin = "1970-01-01")
  n <- suppressWarnings(as.numeric(x)); k <- !is.na(n) & n > 20000 & n < 80000
  out[k] <- as.Date(n[k], origin = "1899-12-30")
  rest <- !k & !is.na(x) & nzchar(x)
  for (f in c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d-%b-%Y", "%B %d, %Y")) {
    if (!any(rest)) break
    p <- as.Date(substr(x[rest], 1, 20), f); out[rest][!is.na(p)] <- p[!is.na(p)]; rest[rest] <- is.na(p)
  }
  out
}

excel_time <- function(x) {           # seconds after midnight
  x <- trim(as.character(x)); n <- suppressWarnings(as.numeric(x)); out <- rep(NA_real_, length(x))
  k <- !is.na(n) & n >= 0 & n < 1; out[k] <- round(n[k] * 86400)
  k2 <- !k & !is.na(n) & n >= 1; out[k2] <- round((n[k2] %% 1) * 86400)           # date-time serial
  m <- regmatches(x, regexec("(\\d{1,2}):(\\d{2})(?::(\\d{2}))?\\s*([AaPp][Mm])?", x))
  for (i in which(is.na(out) & lengths(m) > 0)) {
    v <- m[[i]]; h <- as.numeric(v[2]); ap <- tolower(v[5])
    if (ap == "pm" && h < 12) h <- h + 12
    if (ap == "am" && h == 12) h <- 0
    out[i] <- h * 3600 + as.numeric(v[3]) * 60 + if (nzchar(v[4])) as.numeric(v[4]) else 0
  }
  out
}

clean_type <- function(x) { x <- tolower(trim(x))
  ifelse(grepl("^baro", x), "Baro", ifelse(grepl("^pi?[ez]+o|^pizeo", x), "Piezo", ifelse(grepl("^wlr", x), "WLR", ifelse(grepl("^flume", x), "Flume", trim(x))))) }

read_metadata <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  sheets <- readxl::excel_sheets(path)
  yrs <- suppressWarnings(as.integer(trim(sheets)))
  out <- list()
  for (k in which(!is.na(yrs))) {
    d <- suppressMessages(readxl::read_excel(path, sheet = sheets[k], col_types = "text", .name_repair = "minimal"))
    names(d) <- trim(names(d)); d <- d[, nzchar(names(d)) & !duplicated(names(d)), drop = FALSE]
    d <- as.data.table(d)
    if (!"LocationName" %in% names(d) || !nrow(d)) next
    d <- d[!is.na(LocationName) & nzchar(trim(LocationName))]
    for (v in c("Easting", "Northing", "LC", "Type", "SN", "Logger", "Install_Date", "Downloaded", "DGPS", "Date", "Time", META_NUM, "Remarks", "Alias", "LocationNotes"))
      if (!v %in% names(d)) d[, (v) := NA_character_]
    r <- data.table(year = yrs[k], sheet = sheets[k], row = seq_len(nrow(d)) + 1L, site = trim(d$LocationName), alias = trim(d$Alias),
                    easting = suppressWarnings(as.numeric(d$Easting)), northing = suppressWarnings(as.numeric(d$Northing)),
                    land_cover = trim(d$LC), type = clean_type(d$Type), serial_raw = trim(d$SN), serial = norm_serial(d$SN),
                    logger = trim(d$Logger), downloaded_by = trim(d$Downloaded), dgps = toupper(trim(d$DGPS)) %in% c("Y", "YES"),
                    visit_date = excel_date(d$Date), visit_sec = excel_time(d$Time), install_date = excel_date(d$Install_Date),
                    remarks = trim(d$Remarks), notes = trim(d$LocationNotes))
    for (v in META_NUM) {
      raw <- trim(d[[v]]); val <- suppressWarnings(as.numeric(raw))
      set(r, j = paste0(v, "_raw"), value = raw); set(r, j = v, value = val / 100)          # cm -> m
    }
    out[[length(out) + 1L]] <- r
  }
  if (!length(out)) return(NULL)
  md <- rbindlist(out, fill = TRUE)
  md[is.na(site) | site == "NA", site := NA]
  md[, site := sub("\\s+$", "", site)]
  md[, visit_time := as.POSIXct(visit_date) + ifelse(is.na(visit_sec), 12 * 3600, visit_sec)]
  md[, visit_time_known := !is.na(visit_sec)]
  # T_ST measured from the casing instead of the T-coupler: only ever stated in Remarks.
  rx <- "no t\\b|no \"?t\"?[ -]|toc[ -]*(to|-)[ -]*st|toc-st|hanging from (the )?ca(p|sing)|t is below|no t-coupler|no tee"
  md[, tst_from_toc := grepl(rx, tolower(paste(remarks, notes)), perl = TRUE) | (is.na(T_TOC) & !is.na(TOC_water) & is.na(T_water) & !is.na(T_ST))]
  md[, tst_problem := ifelse(!is.na(T_ST_raw) & nzchar(T_ST_raw) & is.na(T_ST), "not a number", ifelse(type %in% c("WLR", "Piezo") & !is.na(serial) & nzchar(serial) & is.na(T_ST), "missing", ""))]
  md[]
}

# ---- the metadata sheet (core/metasheet.R): what people know about each logger, with or without a workbook --------------
# A row per logger and season. Every column but the serial is optional: a logger listed with only a name is processed under
# that name; one with a site and a year is deployed there that year (between the visits given, else from its first record to
# its last); Baro makes it a barologger. Each project can relabel, hide or add columns (Loggers and metadata > Deployment sheet).
META_FIELDS <- data.table(
  field   = c("serial", "year", "name", "site", "type", "model", "installed", "visit", "T_ST", "T_water", "T_TOC", "TOC_water", "easting", "northing", "land_cover", "notes"),
  label   = c("Serial", "Year", "Name", "Site", "Type", "Logger model", "Installed", "Download visit", "Hang length", "T-coupler to water", "T-coupler to casing top",
              "Casing top to water", "Easting", "Northing", "Land cover", "Notes"),
  type    = c("text", "integer", "text", "text", "choice", "text", "date", "datetime", "number", "number", "number", "number", "number", "number", "text", "text"),
  choices = c("", "", "", "", "WLR|Piezo|Baro|Flume", rep("", 11)),
  unit    = c(rep("", 8), "cm", "cm", "cm", "cm", "", "", "", ""),
  use     = c("ties the row to the logger's files", "the season: the year its files were downloaded, as the workbook's year sheets; blank: every year it has files",
              "a name for the logger; its site until one is given", "where it was deployed", "Baro makes it a barologger; Piezo and Flume are water loggers", "shown with the logger",
              "the deployment starts here when there is no earlier visit", "the deployment ends at this visit (a date alone is taken as noon)",
              "T-coupler to sensor tip: turns pressure into the depth of water below the T-coupler", "checked against the logger at the visit", "used with the casing measurements",
              "used with the casing measurements", "the site's position (UTM)", "the site's position (UTM)", "shown with the site", "shown with the deployment"),
  aliases = c("sn|serial number|s/n|logger serial|logger sn", "season|sheet", "logger name|alias|label|nickname", "location|locationname|location name|station|well|site name",
              "kind|role|logger type", "logger|model|make", "install date|install_date|installed date|deployed", "visit date|date|download|downloaded|download date|retrieved",
              "t_st|tst|hang|cable length|t coupler to sensor", "t_water", "t_toc", "toc_water", "utm easting|x", "utm northing|y", "lc|landcover|land cover", "remarks|comments|locationnotes|comment"),
  show    = c(rep("TRUE", 9), "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE"),
  identifies = c("TRUE", "TRUE", rep("FALSE", 14)))

# The sheet's rows in the workbook's form (read_metadata()), so the rest of the pipeline reads both alike. canon: the
# pipeline's serial matching; years_of(serial): the years the logger has files, for rows without a year. Rows with neither
# a site nor a name (and not Baro) say nothing about where the logger was and are left out here.
sheet_metadata <- function(ms, canon = identity, years_of = function(s) integer()) {
  if (is.null(ms) || !nrow(ms)) return(NULL)
  g <- function(r, k) { v <- r[[k]]; if (is.null(v) || !length(v) || is.na(v[1])) NA else v[1] }
  txt <- function(x) { x <- if (is.na(x)) "" else trim(as.character(x)); x }
  out <- list()
  for (i in seq_len(nrow(ms))) { r <- ms[i]; s <- canon(norm_serial(r$serial)); if (!nzchar(s)) next
    ty <- txt(g(r, "type")); site <- txt(g(r, "site")); if (!nzchar(site)) site <- txt(g(r, "name")); if (!nzchar(site) && ty == "Baro") site <- s
    if (!nzchar(site)) next
    y <- g(r, "year"); yrs <- if (!is.na(y)) as.integer(y) else { v <- g(r, "visit"); yy <- years_of(s); if (length(yy)) yy else if (!is.na(v)) as.integer(format(v, "%Y")) else integer() }
    if (!length(yrs)) next                                    # no year, no visit and no files: nothing to place
    v <- g(r, "visit"); known <- !is.na(v) && format(v, "%H:%M:%S", tz = "UTC") != "00:00:00"
    cm <- function(k) { x <- g(r, k); if (is.na(x)) NA_real_ else as.numeric(x) / 100 }; craw <- function(k) { x <- g(r, k); if (is.na(x)) "" else format(x) }
    out[[length(out) + 1L]] <- data.table(year = yrs, sheet = "metadata.csv", row = i + 1L, site = site, alias = txt(g(r, "name")),
      easting = as.numeric(g(r, "easting")), northing = as.numeric(g(r, "northing")), land_cover = txt(g(r, "land_cover")), type = if (nzchar(ty)) ty else "WLR",
      serial_raw = txt(r$serial), serial = s, logger = txt(g(r, "model")), downloaded_by = "", dgps = FALSE,
      visit_date = if (is.na(v)) as.Date(NA) else as.Date(v), visit_sec = if (known) as.numeric(v) %% 86400 else NA_real_,
      install_date = { x <- g(r, "installed"); if (is.na(x)) as.Date(NA) else as.Date(x) }, remarks = txt(g(r, "notes")), notes = "",
      T_water_raw = craw("T_water"), T_water = cm("T_water"), T_TOC_raw = craw("T_TOC"), T_TOC = cm("T_TOC"), TOC_water_raw = craw("TOC_water"), TOC_water = cm("TOC_water"),
      T_veg_raw = "", T_veg = NA_real_, TOC_veg_raw = "", TOC_veg = NA_real_, T_ST_raw = craw("T_ST"), T_ST = cm("T_ST"))
  }
  if (!length(out)) return(NULL)
  md <- rbindlist(out, fill = TRUE)
  md[, visit_time := as.POSIXct(visit_date) + ifelse(is.na(visit_sec), 12 * 3600, visit_sec)][, visit_time_known := !is.na(visit_sec)]
  md[, `:=`(tst_from_toc = FALSE, tst_problem = ifelse(type %in% c("WLR", "Piezo") & is.na(T_ST) & nzchar(T_ST_raw), "not a number", ""))]
  md[]
}
# The workbook's rows with the sheet's: where the sheet has a row for a serial and year the workbook lists, its filled cells
# replace the workbook's (site, type, name, model, position, land cover, hang length, notes) and its visit is added; other
# sheet rows are added as they are. Either may be missing.
merge_metadata <- function(md, sm) {
  if (is.null(sm) || !nrow(sm)) return(if (is.null(md)) data.table() else md)
  if (is.null(md) || !nrow(md)) return(sm)
  md <- copy(md); add <- list()
  for (i in seq_len(nrow(sm))) { r <- sm[i]; hit <- which(md$serial == r$serial & md$year %in% r$year & md$sheet != "metadata.csv")
    if (!length(hit)) { add[[length(add) + 1L]] <- r; next }
    for (k in c("site", "alias", "logger", "land_cover", "remarks", "T_ST_raw")) if (nzchar(r[[k]])) set(md, hit, k, r[[k]])
    if (r$type != "WLR" || !nzchar(md$type[hit[1]])) set(md, hit, "type", r$type)
    for (k in c("easting", "northing", "T_ST")) if (!is.na(r[[k]])) set(md, hit, k, r[[k]])
    if (!is.na(r$visit_date)) add[[length(add) + 1L]] <- r }
  rbind(md, rbindlist(add, fill = TRUE), fill = TRUE)
}

# Deployments: one per (serial, site, year). It ends at that year's last visit and starts at the same serial's last visit
# in the year before (or, for older seasonal records, the Install_Date). Overrides from the app replace either end.
build_deployments <- function(md, overrides = NULL) {
  if (is.null(md) || !nrow(md)) {           # no workbook: an empty table with every column, so hand-set deployments still apply
    md <- data.table(); t0 <- as.POSIXct(character(), tz = "UTC")
    dep <- data.table(serial = character(), site = character(), year = integer(), type = character(), logger = character(), end = t0,
                      end_known = logical(), install = t0, T_ST = numeric(), T_ST_raw = character(), tst_from_toc = logical(),
                      tst_problem = character(), easting = numeric(), northing = numeric(), land_cover = character(), alias = character(), visits = integer())
  } else {
  v <- md[!is.na(serial) & nzchar(serial)]
  dep <- v[, .(type = type[1], logger = logger[1], end = suppressWarnings(max(visit_time, na.rm = TRUE)),
               end_known = any(visit_time_known), install = suppressWarnings(min(install_date, na.rm = TRUE)),
               T_ST = T_ST[!is.na(T_ST)][1], T_ST_raw = T_ST_raw[1], tst_from_toc = any(tst_from_toc), tst_problem = tst_problem[1], T_veg = if ("T_veg" %in% names(v)) T_veg[!is.na(T_veg)][1] else NA_real_,
               easting = easting[1], northing = northing[1], land_cover = land_cover[1], alias = alias[1], visits = .N),
           by = .(serial, site, year)]
  }
  dep[!is.finite(end), end := NA]
  setorder(dep, serial, year)
  dep[, end_source := ifelse(is.na(end), "last record", "visit")]
  # a year with no visit date that is followed by another year: its end is estimated as mid-September (the download sweep),
  # so that it cannot swallow the following years; set the real date on the Deployment history page
  dep[, later := shift(year, -1L), by = serial]
  dep[is.na(end) & !is.na(later), `:=`(end = as.POSIXct(paste0(year, "-09-15 12:00:00"), tz = "UTC"), end_source = "estimated (no visit date)")]
  dep[, later := NULL]
  dep[, start := shift(end), by = serial]
  dep[, start_source := ifelse(is.na(start), "first record", ifelse(shift(end_source) == "visit", "previous visit", "estimated (no visit date)")), by = serial]
  dep[is.na(start) & is.finite(install), `:=`(start = as.POSIXct(install), start_source = "install date")]
  dep[, install := NULL]
  dep[, `:=`(T_ST_source = ifelse(is.na(T_ST), "", "workbook"), deployment = paste(serial, safe_name(site), year, sep = "__"), added_by_hand = FALSE)]
  dep[, measured_to := ""]
  if (!is.null(overrides) && nrow(overrides)) for (k in seq_len(nrow(overrides))) {
    o <- overrides[k]; by_site <- !nzchar(o$serial %||% "")                  # a row with no serial applies to every logger at its site
    if (by_site && !nzchar(o$site %||% "")) next
    hit <- (if (by_site) dep$site == o$site else dep$serial == norm_serial(o$serial)) & (o$year == "" | dep$year == as.integer(o$year))
    if (!by_site && nzchar(o$site %||% "")) hit <- hit & (dep$site == o$site | !any(hit & dep$site == o$site))
    if (!by_site && !any(hit) && nzchar(o$site %||% "") && nzchar(o$year)) {          # a logger the workbook does not list that year
      dep <- rbind(dep, data.table(serial = norm_serial(o$serial), site = o$site, year = as.integer(o$year), type = "WLR", visits = 0L,
                                   tst_from_toc = FALSE, start_source = "first record", end_source = "last record", T_ST_source = "",
                                   deployment = paste(norm_serial(o$serial), safe_name(o$site), o$year, sep = "__"), added_by_hand = TRUE), fill = TRUE)
      hit <- seq_len(nrow(dep)) == nrow(dep)
    } else if (!by_site && any(hit) && nzchar(o$site %||% "")) dep[hit, site := o$site]
    if (!any(hit)) next
    if (nzchar(o$deployed %||% "")) dep[hit, `:=`(start = parse_date_cell(o$deployed), start_source = "set by hand")]
    if (nzchar(o$removed %||% "")) dep[hit, `:=`(end = parse_date_cell(o$removed, end = TRUE), end_source = "set by hand")]
    if (nzchar(o$T_ST_m %||% "")) dep[hit, `:=`(T_ST = as.numeric(o$T_ST_m), T_ST_source = "set by hand")]
    if (nzchar(o$T_ST_from_year %||% "") && nrow(md)) { y <- as.integer(o$T_ST_from_year)
      src <- md[serial == norm_serial(o$serial) & year == y & !is.na(T_ST)]
      if (!nrow(src)) src <- md[site %in% dep$site[hit] & year == y & !is.na(T_ST)]
      if (nrow(src)) dep[hit, `:=`(T_ST = src$T_ST[1], T_ST_source = paste("from", y, "sheet"))] }
    if (nzchar(o$reference %||% "")) dep[hit, tst_from_toc := toupper(o$reference) == "TOC"]
    if (tolower(o$measured_to %||% "") %in% c("tip", "sensor")) dep[hit, measured_to := tolower(o$measured_to)]
  }
  dep[]
}
# measured_to: what T_ST was measured to, "tip" (the end of the logger) or "sensor"; blank = the hang_measured_to setting.
# A row with a site and no serial sets it (or any other column) for every logger at that site.
# Rows of the shared deployment sheet (core deployments.R) as rows of this logger list: one per season (the year of its
# download: season_year() of its From, the year of its To) from its From to its To, at the station (else the site); its From is the installation, its To the
# last visit. A row with no dates applies to every season the logger has files, as a row with no year here does.
wlr_deploy_rows <- function(r) rbindlist(lapply(seq_len(nrow(r)), function(i) { x <- r[i]
  if (is.na(x$.from) && is.na(x$.to)) return(data.table(serial = x$serial, year = "", site = dep_where(x), installed = "", visit = "", notes = x$notes))
  # installed: its first download is the season after; removed: that visit's download is its last season (the visit's year)
  b <- if (!is.na(x$.to)) as.integer(format(x$.to, "%Y")) else season_year(Sys.time())
  a <- if (!is.na(x$.from)) season_year(x$.from) else b; yrs <- seq(a, max(a, b)); n <- length(yrs)
  data.table(serial = x$serial, year = as.character(yrs), site = dep_where(x), installed = c(dep_day(x$.from), rep("", n - 1L)),
             visit = c(rep("", n - 1L), dep_day(x$.to)), notes = x$notes) }))

DEPLOY_OVERRIDE_COLS <- c("serial", "year", "site", "deployed", "removed", "T_ST_m", "T_ST_from_year", "reference", "measured_to", "reason", "author")
