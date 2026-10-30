# R/export.R  --  Flag columns, station report (HTML and/or PDF) and the export zip.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

# ---- export for analysis and archiving ---------------------------------------
FLAG_CODES <- c(S = "suspect: a check was failed but the value was kept", R = "removed by a quality rule (the value is blank)", M = "missing in the source")

# What each CS check tells us about a value, for the WMO codes in exports (core/qc.R holds the generic ones).
qc_wmo_register(c(sw_above_physical_limit = 3L, lw_outside_physical_limit = 3L, rh_above_103 = 3L, possible_icing = 2L, tips_below_freezing = 2L,
                  outgoing_exceeds_incoming = 1L, sunlight_at_night = 1L, sensors_disagree = 1L, mean_outside_min_max = 1L,
                  differs_from_other_stations = 2L))

read_flags <- function(project_dir) { f <- file.path(path.expand(project_dir), "output", "audit", "flags.csv")
  if (file.exists(f)) fread(file = f, colClasses = "character", na.strings = NULL) else data.table() }

# The station table with a <parameter>_flag column beside every parameter that has a suspect or removed value; or, with
# scheme = "wmo", a <parameter>_qc column beside every parameter holding the WMO code (WMO BUFR table 0 33 020; see WMO_QC in core/qc.R).
# estimate = TRUE fills short gaps by straight lines between the readings around them, where that is safe: air temperature and
# humidity up to 2 hours, pressure up to 3 hours (as WLR does), never precipitation or anything else; those values get the WMO
# code 6 (estimated) and the table's level becomes L3.
ESTIMATE_RX <- c("^(air_temp|rh)(_[0-9]+)?$" = 2, "^(baro|press|bp|atm_?press|pressure)" = 3)
fill_short <- function(t, x, max_h) {
  i <- which(!is.na(x)); if (length(i) < 2) return(integer())
  g <- which(diff(i) > 1); if (!length(g)) return(integer())
  a <- i[g]; b <- i[g + 1]; ok <- (as.numeric(t[b]) - as.numeric(t[a])) / 3600 <= max_h + 1e-9
  unlist(lapply(which(ok), function(k) seq(a[k] + 1L, b[k] - 1L)))
}
flagged_station <- function(station, stream, project_dir, from = NULL, to = NULL, scheme = "wmo", estimate = FALSE, hide_accepted = TRUE) {
  dt <- load_station(station, stream, project_dir); project_dir <- path.expand(project_dir)
  fl <- read_flags(project_dir); if (nrow(fl)) fl <- fl[station == dt$station[1] & stream == dt$stream[1] & !(hide_accepted & flag_accepted(fl))]   # an accepted flag marks no value unless hide_accepted is off (core config.R)
  rf <- file.path(project_dir, "output", "audit", "detail", paste0(safe_name(paste0("station_", station, "__", stream)), ".csv"))
  qf <- file.path(project_dir, "output", "qc", paste0(safe_name(paste(station, stream, sep = "__")), "_removed.csv"))
  rem <- if (file.exists(qf)) setnames(fread(file = qf, colClasses = "character", na.strings = ""), "parameter", "column")
         else if (file.exists(rf)) fread(file = rf, colClasses = "character", na.strings = "")[stage == "qc"] else data.table()
  vals <- setdiff(names(dt), c(META_COLS)); out <- dt[, c("station", "stream", "TIMESTAMP", intersect("RECORD", names(dt)), "logger_id"), with = FALSE]
  wmo <- identical(scheme, "wmo")
  if (wmo) { rd <- function(f) if (file.exists(f)) fread(file = f, colClasses = "character", na.strings = "") else data.table()
    rules <- rd(file.path(project_dir, "config", "qc_rules.csv")); cal <- rd(file.path(project_dir, "output", "audit", "calibrations_applied.csv"))
    if (nrow(cal)) cal <- cal[station == dt$station[1] & stream == dt$stream[1]]
    has <- function(x) !is.na(x) & nzchar(x)
    tested <- if (nrow(rules)) rules[has(min) | has(max) | has(max_spike) | has(max_flat_hours)]$pattern else character()
    tsn <- fmt_ts(dt$TIMESTAMP) }
  for (v in vals) { set(out, j = v, value = dt[[v]])
    if (wmo) {                                                       # one WMO code per value
      checked <- length(tested) && any(vapply(tested, function(p) grepl(p, v, ignore.case = TRUE, perl = TRUE), logical(1)))
      code <- rep(if (checked) 0L else 4L, nrow(dt)); code[is.na(dt[[v]])] <- 7L
      if (nrow(fl)) for (i in which(fl$parameter == v)) { hit <- dt$TIMESTAMP >= parse_time(fl$start[i]) & dt$TIMESTAMP <= parse_time(fl$end[i])
        if (any(hit)) code[hit] <- qc_wmo_combine(code[hit], qc_wmo_class(fl$check[i])) }
      if (nrow(rem)) { r <- rem[column == v]; if (nrow(r)) { k <- match(tsn, r$timestamp); hit <- which(!is.na(k))
        if (length(hit)) code[hit] <- qc_wmo_combine(code[hit], qc_wmo_class(if ("rule" %in% names(r)) r$rule[k[hit]] else "range_min")) } }
      if (nrow(cal)) for (i in which(cal$parameter == v)) { a <- if (has(cal$start[i])) parse_time(cal$start[i]) else -Inf; b <- if (has(cal$end[i])) parse_time(cal$end[i]) else Inf
        hit <- !is.na(dt[[v]]) & dt$TIMESTAMP >= a & dt$TIMESTAMP <= b; if (any(hit)) code[hit] <- qc_wmo_combine(code[hit], 5L) }
      if (estimate && is.numeric(dt[[v]])) { mh <- ESTIMATE_RX[vapply(names(ESTIMATE_RX), function(r) grepl(r, v, ignore.case = TRUE, perl = TRUE), logical(1))]
        if (length(mh)) { k <- fill_short(dt$TIMESTAMP, dt[[v]], mh[[1]]); k <- k[code[k] == 7L]          # missing, not removed by a rule
          if (length(k)) { x <- dt[[v]]; ok <- which(!is.na(x)); x[k] <- stats::approx(as.numeric(dt$TIMESTAMP[ok]), x[ok], as.numeric(dt$TIMESTAMP[k]))$y
            set(out, j = v, value = x); code[k] <- 6L } } }
      set(out, j = paste0(v, "_qc"), value = code); next }
    f <- rep("", nrow(dt)); any_flag <- FALSE
    if (nrow(fl)) for (i in which(fl$parameter == v)) { hit <- dt$TIMESTAMP >= parse_time(fl$start[i]) & dt$TIMESTAMP <= parse_time(fl$end[i]); if (any(hit)) { f[hit] <- "S"; any_flag <- TRUE } }
    if (nrow(rem)) { r <- rem[column == v]; if (nrow(r)) { hit <- fmt_ts(dt$TIMESTAMP) %in% r$timestamp; if (any(hit)) { f[hit] <- "R"; any_flag <- TRUE } } }
    if (any_flag) { f[f == "" & is.na(dt[[v]])] <- "M"; set(out, j = paste0(v, "_flag"), value = f) } }
  out[, source_file := basename(dt$source_file)]
  if (!is.null(from)) out <- out[TIMESTAMP >= as.POSIXct(format(from), tz = "UTC")]
  if (!is.null(to)) out <- out[TIMESTAMP < as.POSIXct(format(as.Date(to) + 1), tz = "UTC")]
  out[]
}

# One station table (or a whole station): what it is, every parameter at a glance (grouped, as daily means), statistics for each parameter, and what was removed or flagged.
# The parts of a station report that can be left out (Export page).
CS_REPORT_SECTIONS <- c("Logger and program" = "info", "Program history" = "program", "Data availability" = "availability", "Parameters at a glance" = "overview",
                        "Statistics" = "stats", "Sensors and measurements" = "sensors", "Quality control" = "qc")
# Axes shared by the station reports written together (core report_options.R): each parameter from the 1st to the 99th
# percentile of every station's records (output/audit/parameter_stats.csv). keys: station and stream.
cs_report_limits <- function(project_dir, keys, mode = "same") {
  f <- file.path(path.expand(project_dir), "output", "audit", "parameter_stats.csv"); if (identical(mode, "free") || !file.exists(f)) return(NULL)
  ps <- fread(file = f, colClasses = list(character = c("station", "stream", "parameter"))); k <- as.data.table(keys)
  ps <- ps[paste(station, stream) %in% paste(k$station, k$stream) | (station %in% k$station[!nzchar(k$stream %||% "")])]
  if (!nrow(ps)) return(NULL)
  axis_limits(ps[, .(unit = rep(station, 2), key = rep(parameter, 2), value = c(p01, p99))], mode)
}
station_report_doc <- function(station, stream, project_dir, groups = default_groups(), opts = NULL, lims = NULL) {
  ro <- report_opts(opts); want <- function(id) report_wants(ro, id)
  project_dir <- path.expand(project_dir); a <- file.path(project_dir, "output", "audit")
  whole <- is.null(stream) || !nzchar(stream)
  dt <- if (whole) load_station_all(station, project_dir) else load_station(station, stream, project_dir); rd <- function(f, ...) if (file.exists(file.path(a, f))) fread(file = file.path(a, f), ...) else data.table()
  in_scope <- function(d) d[station == dt$station[1] & (whole | stream == dt$stream[1])]
  meta <- rd("metadata.csv", colClasses = "character"); if (nrow(meta)) meta <- in_scope(meta)
  idx <- rd("audit_index.csv", colClasses = list(character = c("station", "stream"))); if (nrow(idx)) idx <- in_scope(idx)[level == "cell"]
  fl <- read_flags(project_dir); if (nrow(fl)) fl <- in_scope(fl)
  title <- if (whole) dt$station[1] else paste0(dt$station[1], "  /  ", dt$stream[1])
  vals <- setdiff(names(dt), META_COLS); nv <- vals[vapply(vals, function(v) is.numeric(dt[[v]]) && any(!is.na(dt[[v]])), logical(1))]
  ov <- NULL; st_tab <- NULL; cov <- NULL
  if (length(nv)) { dd <- dt[, lapply(.SD, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)), by = .(TIMESTAMP = as.POSIXct(format(as.Date(TIMESTAMP)), tz = "UTC")), .SDcols = nv]
    dd[, `:=`(station = dt$station[1], stream = dt$stream[1])]; g <- param_group(nv, groups)
    cov <- try_or(rpt_availability(dt, nv, stats::setNames(g, nv), "TIMESTAMP", GB), NULL, paste0(title, ": report availability chart left out"))
    st_tab <- try_or(rpt_stats(dt, nv, stats::setNames(g, nv)), NULL, paste0(title, ": report statistics table left out"))
    ov <- try_or(rpt_overview(melt(dd[, c("TIMESTAMP", nv), with = FALSE], id.vars = "TIMESTAMP", variable.name = "parameter", value.name = "value")[, `:=`(group = g[match(as.character(parameter), nv)], time = TIMESTAMP)],
                                GB, paste0("Parameter overview  -  ", gsub("  ", " ", title)), ylim = if (!is.null(lims)) lims[unit == dt$station[1], .(parameter = key, lo, hi)]), NULL, paste0(title, ": report overview charts left out"))       # every parameter as a small chart, grouped, paged
  }
  # what the scan found out about the logger and its program (all of it optional: a table is left out when there is nothing to show)
  key <- if (nrow(meta)) paste(meta$logger_id, meta$table) else character()
  ph <- rd("program_history.csv", colClasses = "character"); if (nrow(ph) && length(key)) ph <- ph[paste(logger_id, table) %in% key] else ph <- data.table()
  cd <- rd("column_dictionary.csv", colClasses = "character", na.strings = NULL); if (nrow(cd) && length(key)) cd <- cd[paste(logger_id, table) %in% key] else cd <- data.table()
  nz <- has_value                                                                            # core util.R
  info_t <- if (nrow(meta) && nrow(meta) <= 4) { f <- function(col, lab) if (col %in% names(meta) && nz(meta[[col]])) stats::setNames(list(as.character(meta[[col]])), lab)
    rows <- c(f("logger_id", "Logger"), f("model", "Model"), f("serial", "Serial number"), f("table", "Table"), f("program", "Program"), f("os", "Operating system"), f("interval", "Recording interval"),
              f("records", "Records"), f("start", "First record"), f("end", "Last record"), f("span_days", "Days covered"), f("complete_pct", "Complete (%)"), f("gaps", "Gaps"), f("largest_gap_hours", "Largest gap (hours)"), f("files", "Files read"), f("format", "File format"))
    out <- data.table(item = names(rows)); for (i in seq_len(nrow(meta))) out[[paste0("v", i)]] <- vapply(names(rows), function(n) { v <- rows[[n]][i]; if (n %in% c("Records", "Files read") && !is.na(suppressWarnings(as.numeric(v)))) format(as.numeric(v), big.mark = ",", scientific = FALSE) else v }, ""); setnames(out, c("item", if (nrow(meta) == 1) "value" else paste0("logger ", seq_len(nrow(meta))))); out
  } else if (nrow(meta)) meta[, intersect(c("logger_id", "model", "table", "program", "interval", "records", "start", "end", "complete_pct", "gaps"), names(meta)), with = FALSE] else NULL
  ph_t <- if (nrow(ph)) { x <- ph[, intersect(c("from", "to", "logger_id", "table", "change", "program", "os", "columns_added", "columns_removed"), names(ph)), with = FALSE][order(logger_id, from)]; x[, vapply(x, nz, NA), with = FALSE] } else NULL
  sens_t <- if (nrow(cd)) { keep <- c("raw_column", "parameter", "group", "unit", "process", if (nz(cd$measured_by)) "measured_by", if (nz(cd$program_interval)) "program_interval"); unique(cd[, intersect(keep, names(cd)), with = FALSE]) } else NULL
  rem_t <- if (nrow(idx)) idx[, .(rule, values = entries)][, .(values = sum(values)), by = rule][order(-values)] else NULL
  flg_t <- if (nrow(fl)) fl[, .(values = sum(as.numeric(records))), by = check][order(-values)] else NULL
  fr <- format(min(dt$TIMESTAMP), "%Y-%m-%d"); to <- format(max(dt$TIMESTAMP), "%Y-%m-%d")
  rpt_doc(report_title(ro, title, name = dt$station[1], from = fr, to = to, app = "CS"),
          report_subtitle(ro, paste0(fr, " to ", to, "  |  logger time as recorded"), name = dt$station[1], from = fr, to = to, app = "CS"), list(
    rpt_cards(c(records = nrow(dt), loggers = uniqueN(dt$logger_id), `parameters with values` = length(nv),
                `values removed` = if (nrow(idx)) sum(idx$entries, na.rm = TRUE) else 0, `values flagged` = if (nrow(fl)) sum(as.numeric(fl$records), na.rm = TRUE) else 0)),
    if (want("info") && !is.null(info_t)) rpt_heading("Logger and program"),
    if (want("info") && !is.null(info_t)) rpt_table(info_t, if (nrow(meta) > 1) "From the files the scan read; one column per logger." else "From the headers of the files the scan read."),
    if (want("program") && !is.null(ph_t)) rpt_heading("Program history"),
    if (want("program") && !is.null(ph_t)) rpt_table(ph_t, "Each program the logger ran in this table, and the columns it added or removed."),
    if (want("availability") && !is.null(cov)) rpt_heading("Data availability"),
    if (want("availability")) cov,
    if (want("overview") && !is.null(ov)) rpt_heading("Parameters at a glance"),
    if (want("overview") && !is.null(ov)) rpt_text("Every parameter as daily means (the pale band is the daily minimum to maximum), grouped by what it measures."),
    if (want("overview")) ov,
    if (want("stats") && !is.null(st_tab)) rpt_heading("Statistics by parameter"),
    if (want("stats")) st_tab,
    if (want("sensors") && !is.null(sens_t)) rpt_heading("Sensors and measurements"),
    if (want("sensors") && !is.null(sens_t)) rpt_table(sens_t, "Each raw column, what it measures and how it is processed; where logger programs were supplied, the instruction that measured it and the program's output interval."),
    if (want("qc")) applied_report(project_dir, "Counts are for the whole run, all stations."),
    if (want("qc")) rpt_heading("Quality control"),
    if (want("qc")) rpt_quality_bars(rem_t, flg_t, GB),
    if (want("qc")) rpt_heading("Removed by quality rules"),
    if (want("qc")) rpt_table(rem_t, "Blank in the data and listed in the audit trail."),
    if (want("qc")) rpt_heading("Flagged as suspect"),
    if (want("qc")) rpt_table(flg_t, "The values are kept in the data.")))
}

# Writes the report for one station table in the chosen formats; `file` may carry an extension, which is replaced. Returns the files written.
station_report <- function(station, stream, project_dir, file, groups = default_groups(), formats = "pdf", opts = NULL, lims = NULL)
  report_write(station_report_doc(station, stream, project_dir, groups, opts, lims), sub("\\.(pdf|html)$", "", file), formats, GB)

# One zip a colleague or a repository can use without the app.
# Each station's UTC offset from Station details (config/station_info.csv); NA where none is given.
station_offsets <- function(project_dir, stations) {
  f <- file.path(path.expand(project_dir), "config", "station_info.csv")
  si <- if (file.exists(f)) fread(file = f, colClasses = "character", na.strings = "") else data.table(station = character(), utc_offset_hours = character())
  o <- suppressWarnings(as.numeric(si$utc_offset_hours[match(stations, si$station)])); setNames(o, stations)
}

# out_offset: NA keeps each station's times as stored (its own zone); a number (for example -6) moves every station's records to
# that UTC offset, using the station's utc_offset_hours. Stations with none are moved from unzoned_offset if one is given, else
# left as recorded and named in the readme. When none of the stations has an offset and none is given, a "ovid_ask_zone"
# condition is raised so the app can ask which zone their clocks were in.
# ---- NetCDF -----------------------------------------------------------------------------------------------------------------
# CF standard names of the parameters CS gives a name to (only names in the CF table; anything else gets a long name only), and
# the unit each is written in.
CF_CS <- list(air_temp = c("air_temperature", "degree_Celsius"), rh = c("relative_humidity", "percent"), wind_speed = c("wind_speed", "m s-1"),
  wind_speed_max = c("wind_speed_of_gust", "m s-1"), wind_dir = c("wind_from_direction", "degree"), sw_in = c("surface_downwelling_shortwave_flux_in_air", "W m-2"),
  sw_out = c("surface_upwelling_shortwave_flux_in_air", "W m-2"), lw_in = c("surface_downwelling_longwave_flux_in_air", "W m-2"),
  lw_out = c("surface_upwelling_longwave_flux_in_air", "W m-2"), net_rad = c("surface_net_downward_radiative_flux", "W m-2"), albedo = c("surface_albedo", "1"),
  snow_depth = c("surface_snow_thickness", "m"), rain = c("precipitation_amount", "kg m-2"), precip = c("precipitation_amount", "kg m-2"))
cf_cs <- function(v) { k <- if (v %in% names(CF_CS)) v else if (grepl("^soil_temp", v)) "soil_temp" else ""
  if (k == "soil_temp") return(c("soil_temperature", "degree_Celsius")); if (nzchar(k)) CF_CS[[k]] else c("", "") }

# One station table as a CF-1.10 / ACDD-1.3 NetCDF file (featureType timeSeries): every numeric parameter with its WMO code
# (<p>_qc, CF flags), in true UTC from the station's utc_offset_hours (Station details; taken as the base zone, Settings >
# Time zone, when it has none, as the run does, and the file says so). NULL when the table has no records in the period.
station_netcdf <- function(station, stream, project_dir, path, from = NULL, to = NULL, estimate = FALSE, hide_accepted = TRUE) {
  d <- flagged_station(station, stream, project_dir, from, to, "wmo", estimate, hide_accepted); if (is.null(d) || !nrow(d)) return(NULL)
  so <- station_offsets(project_dir, station)[[station]]; off <- if (length(so) && !is.na(so)) so else BASE_OFFSET
  pinfo <- try_or(default_parameter_info(), NULL, paste0(station, ": parameter names and units for NetCDF"))
  vals <- grep("_qc$", names(d), value = TRUE); vals <- sub("_qc$", "", vals); vals <- vals[vapply(vals, function(v) is.numeric(d[[v]]), logical(1))]
  t <- nc_time(d$TIMESTAMP, off); vars <- list(list(name = "time", dims = "time", data = t, atts = NC_TIME_ATTS)); codes <- integer()
  for (v in vals) { cf <- cf_cs(v); q <- as.integer(d[[paste0(v, "_qc")]]); codes <- c(codes, q)
    r <- if (!is.null(pinfo)) param_info_row(v, pinfo) else NA_integer_
    unit <- if (nzchar(cf[2])) cf[2] else if (!is.na(r)) gsub("deg C", "degree_Celsius", gsub("W/m2", "W m-2", gsub("m/s", "m s-1", pinfo$unit[r]))) else "1"
    x <- d[[v]]
    vars[[length(vars) + 1]] <- list(name = v, dims = "time", data = x, atts = c(if (nzchar(cf[1])) list(standard_name = cf[1]),
      list(long_name = if (!is.na(r)) pinfo$topic[r] else v, units = unit, `_FillValue` = -9999, coordinates = "time lat lon", ancillary_variables = paste0(v, "_qc"))))
    vars[[length(vars) + 1]] <- list(name = paste0(v, "_qc"), dims = "time", data = q, type = "byte", atts = cf_qc_atts(v, cf[1])) }
  sif <- file.path(path.expand(project_dir), "config", "station_info.csv")
  si <- if (file.exists(sif)) try_or(fread(file = sif, colClasses = "character"), data.table(), "config/station_info.csv: NetCDF positions left blank") else data.table()
  s0 <- station; si <- if (nrow(si) && "station" %in% names(si)) si[station == s0] else data.table()
  lat <- suppressWarnings(as.numeric(si$latitude[1] %||% NA)); lon <- suppressWarnings(as.numeric(si$longitude[1] %||% NA))
  vars <- c(vars, nc_station_vars(paste(station, stream, sep = " / "), lat, lon))
  nc_write(path, c(time = length(t), name_strlen = 64L), vars,
    acdd_atts(paste("Climate station", station, "-", stream), paste0("Records of the Campbell Scientific station ", station, " (table ", stream, "), quality controlled, ",
                "with a WMO quality control code for every value. Values removed by a quality rule are absent (fill value).",
                if (is.na(so %||% NA)) sprintf(" The station has no UTC offset in Station details: its clock was taken as the base zone, UTC%+g.", off) else ""),
              paste0("ovid-cs-", safe_name(paste(station, stream, sep = "-"))), t, lat, lon, "timeSeries", proc_level(codes),
              keywords = c("meteorology", "climate station", "permafrost", "peatland"), source = "Campbell Scientific data logger",
              extra = list(utc_offset_of_logger_clock = off), project_dir = project_dir))
  path
}

build_export <- function(project_dir, keys, zipfile, from = NULL, to = NULL, flags = TRUE, flag_scheme = "wmo", out_offset = NA, unzoned_offset = NA, summaries = TRUE, report = TRUE, formats = c("html", "pdf"), progress = function(i, n, what) NULL,
                         estimate = FALSE, netcdf = FALSE, report_opts = NULL, hide_accepted = TRUE) {
  project_dir <- path.expand(project_dir); out <- file.path(project_dir, "output"); a <- file.path(out, "audit")
  ro <- report_opts(report_opts); lims <- if (report) try_or(cs_report_limits(project_dir, keys, ro$axis), NULL, "report axis limits: each chart uses its own range")
  n_prob <- length(AUDIT$problems)                 # problems from here on go into the export's problems.csv
  root <- file.path(tempfile("export_"), "campbell_export"); dir.create(file.path(root, "data"), recursive = TRUE)
  if (summaries) dir.create(file.path(root, "summaries")); report <- report && length(formats) > 0; if (report) dir.create(file.path(root, "reports"))
  dict <- if (file.exists(file.path(a, "column_dictionary.csv"))) fread(file = file.path(a, "column_dictionary.csv"), colClasses = "character", na.strings = NULL) else data.table()
  ps <- if (file.exists(file.path(a, "parameter_stats.csv"))) fread(file = file.path(a, "parameter_stats.csv"), colClasses = list(character = c("station", "stream", "parameter"))) else data.table()
  done <- list(); so <- station_offsets(project_dir, unique(keys$station)); left_alone <- character()
  if (!is.na(out_offset) && all(is.na(so)) && is.na(unzoned_offset))
    stop(structure(class = c("ovid_ask_zone", "error", "condition"), list(message = "None of these stations has a UTC offset in Station details. Which zone were their loggers set to?", call = NULL, n = length(so))))
  for (i in seq_len(nrow(keys))) { st <- keys$station[i]; sm <- keys$stream[i]; kk <- safe_name(paste(st, sm, sep = "__")); progress(i, nrow(keys), paste(st, sm))
    if (netcdf) { dir.create(file.path(root, "netcdf"), showWarnings = FALSE); station_netcdf(st, sm, project_dir, file.path(root, "netcdf", paste0(kk, ".nc")), from, to, estimate, hide_accepted) }
    d <- if (flags) flagged_station(st, sm, project_dir, from, to, flag_scheme, estimate, hide_accepted) else { x <- load_station(st, sm, project_dir)
      if (!is.null(from)) x <- x[TIMESTAMP >= as.POSIXct(format(from), tz = "UTC")]; if (!is.null(to)) x <- x[TIMESTAMP < as.POSIXct(format(as.Date(to) + 1), tz = "UTC")]; x }
    if (!is.na(out_offset)) { o <- so[[st]]; if (is.na(o)) o <- unzoned_offset
      if (is.na(o)) left_alone <- c(left_alone, st) else { d[, TIMESTAMP := TIMESTAMP + (out_offset - o) * 3600]; d[, time_zone := sprintf("UTC%+g", out_offset)]
        setcolorder(d, intersect(c("station", "stream", "TIMESTAMP", "time_zone"), names(d))) } }
    fwrite(d, file.path(root, "data", paste0(kk, ".csv")), dateTimeAs = "write.csv")
    if (summaries) { f <- list.files(file.path(out, c("derived", "precip")), pattern = paste0("^", kk, "_"), full.names = TRUE); file.copy(f, file.path(root, "summaries")) }
    qf <- file.path(out, "qc", paste0(kk, "_removed.csv")); if (flags && file.exists(qf)) { dir.create(file.path(root, "qc"), showWarnings = FALSE); file.copy(qf, file.path(root, "qc")) }
    if (report) try_or(station_report(st, sm, project_dir, file.path(root, "reports", report_file(ro, kk, name = st, app = "CS")), formats = formats, opts = ro, lims = lims), NULL, paste0(kk, ": report left out"))
    done[[i]] <- data.table(station = st, stream = sm, file = paste0("data/", kk, ".csv"), records = nrow(d), first = fmt_ts(min(d$TIMESTAMP)), last = fmt_ts(max(d$TIMESTAMP)),
                            flag_columns = sum(grepl("_flag$|_qc$", names(d))))
  }
  done <- rbindlist(done); fwrite(done, file.path(root, "contents.csv"))
  probs <- problems_since(n_prob); if (nrow(probs)) fwrite(probs, file.path(root, "problems.csv"))
  if (nrow(ps)) { pp <- ps[paste(station, stream) %in% paste(keys$station, keys$stream)]
    if (nrow(dict)) { u <- dict[nzchar(unit), .(unit = unit[1], raw_columns = paste(unique(raw_column), collapse = " | ")), by = parameter]; pp <- merge(pp, u, by = "parameter", all.x = TRUE) }
    fwrite(pp[order(station, stream, parameter)], file.path(root, "parameters.csv")) }
  fl <- read_flags(project_dir); if (nrow(fl)) fwrite(fl[paste(station, stream) %in% paste(keys$station, keys$stream)], file.path(root, "flags.csv"))
  for (f in c("station_info.csv", "field_log.csv")) if (file.exists(file.path(project_dir, "config", f))) file.copy(file.path(project_dir, "config", f), root)
  cmp <- list.files(file.path(out, "composites"), "\\.csv$", full.names = TRUE)      # combined records of the last run (R/composite.R)
  if (length(cmp)) { dir.create(file.path(root, "composites")); file.copy(cmp, file.path(root, "composites")) }
  for (f in c(APPLIED_FILE, "audit_summary.csv", "metadata.csv", "run_manifest.csv", "qc_summary.csv", "sensor_health.csv", "lineage.csv", "program_history.csv")) if (file.exists(file.path(a, f))) file.copy(file.path(a, f), root)
  writeLines(c("Ovid Core // CS - logger data export", paste("Made", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    if (is.na(out_offset)) "data/            one CSV per station table. Timestamps are each station's own time, as stored (no common zone was applied)."
    else c(sprintf("data/            one CSV per station table. Timestamps were moved to UTC%+g from each station's utc_offset_hours (Station details); the time_zone column says so.", out_offset),
           if (!is.na(unzoned_offset)) sprintf("                 Stations without an offset were taken as UTC%+g.", unzoned_offset),
           if (length(left_alone)) paste0("                 Left as recorded (no offset given): ", paste(unique(left_alone), collapse = ", "), "."),
           "                 Daily, monthly and water-year summaries stay in each station's own days."),
    "                 Blank cells are missing values.",
    if (flags && flag_scheme == "wmo") c("                 A column ending in _qc sits beside every parameter, with the WMO quality control code (BUFR table 0 33 020):",
                 paste0("                     ", WMO_QC$code, " = ", WMO_QC$name, ": ", WMO_QC$meaning),
                 "                     When several checks apply the worst one is given (wrong, then inconsistent, doubtful, estimated, changed, good).",
                 "                     flags.csv names the check behind each code, and qc/ holds the values a rule removed (blank in data/)."),
    if (flags && flag_scheme != "wmo") c("                 A column ending in _flag sits beside each parameter that has flagged values:", paste0("                     ", names(FLAG_CODES), " = ", FLAG_CODES),
                 "                     blank = no flag. A parameter with no _flag column had nothing flagged or removed."),
    if (estimate) c("                 Short gaps were filled by straight lines (air temperature and humidity up to 2 hours, pressure up to 3 hours; never precipitation):",
                    "                 those values have the code 6, estimated (level L3)."),
    if (netcdf) "netcdf/          each station table as CF-1.10 / ACDD-1.3 NetCDF: times in UTC, CF standard names, every value with its WMO code as CF flags.",
    if (summaries) c("summaries/       daily, monthly and water-year values (given only when enough records are present),", "                 and the precipitation tables for weighing gauges.",
                     "                 Daily evapotranspiration (et0_fao56_mm, FAO-56 Penman-Monteith; et_pt_mm, Priestley-Taylor) where the station has the inputs and a latitude;",
                     "                 vwc_..._liquid_only marks days the soil froze (a dielectric probe then sees the liquid water only)."),
    if (report) paste0("reports/         one report per station table (", paste(toupper(formats), collapse = " and "), ")."),
    if (length(cmp)) "composites/      every combined record (Combined records): one parameter from several stations, as the last run built it; each value names its station.",
    if (nrow(probs)) "problems.csv     parts of this export that could not be made (a report, a chart), and why.",
    "contents.csv     what is in data/.", "parameters.csv   every parameter: unit, the raw logger columns it came from, range and missing share.",
    "flags.csv        every suspect period: which check, and why.", APPLIED_README,
    if (flags) "qc/              every value a quality rule removed, with the value it had, so a removal can be reviewed or reversed.",
    "run_manifest.csv the run that produced this: version, date, settings and a fingerprint of every set-up table.", "metadata.csv     logger model, program, interval and completeness for each table.",
    "audit_summary.csv  how many values each rule removed. The full audit trail stays with the project.",
    "station_info.csv, field_log.csv  coordinates and site visits, where they have been entered.", "", proc_levels_text()), file.path(root, "README.txt"))
  zipfile <- normalizePath(zipfile, mustWork = FALSE); unlink(zipfile); old <- setwd(dirname(root)); on.exit(setwd(old), add = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, "campbell_export") else utils::zip(zipfile, "campbell_export", flags = "-rq")
  if (!file.exists(zipfile)) stop("the zip could not be written; install.packages(\"zip\") fixes this on machines without a zip program")
  invisible(done)
}

