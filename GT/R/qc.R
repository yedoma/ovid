# R/qc.R  --  Sensors and depths, then quality rules: removals, suspect flags, sensor health.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.
#
# Suspect values are flagged, not removed: the value stays and the period is listed in output/audit/flags.csv (and in a
# flag column in the exports). A rule removes values only when its action says so.

# Flags: values kept but marked. One row per contiguous run in flags.csv, and a flag column on the record.
# (FLAGS, flag_add() and flag_mark() are core qc.R.)
add_flag <- function(dt, idx, check, detail, rule_id = check) {
  if (is.logical(idx)) idx <- which(idx)
  idx <- sort(unique(idx[!is.na(idx)]))
  if (!length(idx)) return(invisible(0L))
  flag_mark(dt, "flag", idx, check)
  d <- dt[idx, .(site = .col(dt, "site", idx), logger_id, channel, variable = .col(dt, "variable", idx), TIMESTAMP, i = idx)]
  d[, run := qc_run_id(i)]
  r <- d[, .(start = fmt_ts(min(TIMESTAMP)), end = fmt_ts(max(TIMESTAMP)), records = .N), by = .(site, logger_id, channel, variable, run)]
  flag_add(r[, .(site, logger_id, channel, variable, start, end, records, check = check, detail = detail, rule_id = rule_id)])
  invisible(length(idx))
}

# Channel -> sensor: an EM50 port carries several variables (P1_temp, P1_vwc) at one depth.
sensor_of <- function(channel, type) if (identical(type, "em50")) sub("_(temp|vwc|ec)$", "", channel) else channel

# Position of each sensor on its logger, for default depths: the number in its name (T109_C_3_Avg -> 3, T2 -> 2, port P4 -> 4;
# a name with no number counts as 1), so a dead sensor that wrote nothing does not shift the ones below it.
sensor_rank <- function(s) { x <- sub("^T10[79]_", "", s); m <- regexpr("[0-9]+", x)
  n <- ifelse(m > 0, suppressWarnings(as.integer(substring(x, m, m + attr(m, "match.length") - 1L))), NA_integer_)
  if (all(is.na(n))) return(rank(s, ties.method = "first"))
  n[is.na(n)] <- 1L; n }

auto_site <- function(type, serial) paste0(TYPE_LABEL[type], "_", serial)

# Makes sure every sensor seen in the data has its own row in sensors.csv, with a site and a depth.
#   - a row the user (or the registry) gave a depth keeps it
#   - a serial linked to a site without depths (sensor blank) is expanded into one row per sensor, for each period
#   - depths come from a template for the make and model, else the default steps (first depth + step x (n - 1))
#   - rows whose depth came from the template or the default are recomputed each run, so changing the steps applies
# The logger list (metadata sheet, core/metasheet.R) as sensor rows. A logger it lists, and that has no row set by hand or
# from the registry, gets one row per list row (its site, else its name, and the dates), noted SHEET_NOTE; complete_sensors()
# then gives each of its sensors a row there, with the list's depths when it has them. Rows from the list are made again
# from it on each run, so the list is where they are changed. Returns the rows, and the depth lists by serial.
SHEET_NOTE <- "from the logger list"
sheet_sensors <- function(sens, ms) {
  sens <- copy(ensure_cols(sens, SENSOR_COLS)); depths <- list()
  if (is.null(ms) || !nrow(ms)) return(list(sens = sens, depths = depths))
  txt <- function(x) { if (is.null(x) || !length(x) || is.na(x[1])) "" else trim(as.character(x[1])) }
  day <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "" else format(as.Date(x[1]))
  sn_all <- norm_serial(sens$serial)
  own <- function(sn) { k <- which(sn_all == sn); k[!(sens$site[k] == auto_site(sens$logger_type[k], sn) | sens$notes[k] == SHEET_NOTE)] }
  add <- list(); drop <- integer()
  for (sn in unique(norm_serial(ms$serial))) { if (!nzchar(sn)) next
    if (length(k <- own(sn))) {                                                                  # rows set by hand or from the registry win, and say so
      want <- unique(trim(c(ms$site[norm_serial(ms$serial) == sn], ms$name[norm_serial(ms$serial) == sn & !nzchar(trim(ms$site %||% ""))]))); want <- want[!is.na(want) & nzchar(want)]
      if (length(want) && !all(tolower(want) %in% tolower(sens$site[k])) && exists("problem", mode = "function"))
        problem("Sensors and depths and the logger list", paste0(sn, ": the logger list gives the site '", paste(want, collapse = "', '"), "', Sensors and depths '",
                                                                   paste(unique(sens$site[k]), collapse = "', '"), "', which is used"))
      next }
    r <- ms[norm_serial(serial) == sn]; drop <- c(drop, which(sn_all == sn))
    for (i in seq_len(nrow(r))) { site <- txt(r$site[i]); if (!nzchar(site)) site <- txt(r$name[i])
      dl <- if ("depths" %in% names(r)) trim(as.character(unlist(r$depths[i]))) else character(); dl <- dl[depth_ok(dl)]; if (length(dl)) depths[[sn]] <- dl
      add[[length(add) + 1L]] <- data.table(logger_type = "", make = "", model = txt(r$model[i]), serial = sn, sensor = "", depth_m = "", depth_source = "",
                                            site = site, start = day(r$start[i]), end = day(r$end[i]), notes = SHEET_NOTE) } }
  if (length(drop)) sens <- sens[-drop]
  list(sens = rbind(sens, rbindlist(add), fill = TRUE), depths = depths)
}

complete_sensors <- function(sens, seen, templates, cfg, depths = list()) {
  sens <- copy(sens); sens[, serial := norm_serial(serial)]
  first <- as.numeric(cfg$default_first_depth); step <- as.numeric(cfg$default_step)
  dflt <- function(r) if (isFALSE(cfg$assign_default_depths)) "" else sgn * (first + step * (r - 1))   # off: left blank, and its values out until a depth is typed
  sgn <- depth_sign(cfg)                          # templates and default steps are below the surface; written in the project's convention
  tmpl_for <- function(make, model) { if (!nrow(templates)) return(NULL)
    for (i in seq_len(nrow(templates))) if ((!nzchar(templates$make[i]) || grepl(templates$make[i], make, ignore.case = TRUE)) &&
                                           nzchar(templates$model[i]) && grepl(tolower(templates$model[i]), tolower(model), fixed = TRUE)) {
      z <- trim(strsplit(templates$depths[i], ",")[[1]]); return(z[depth_ok(z)]) }
    NULL }
  # a listed or template depth as written into sensors.csv: "+x" as typed; a template value (below the surface) in the project's convention
  as_typed <- function(s, listed) if (startsWith(s, "+")) s else if (listed) as.numeric(s) else sgn * as.numeric(s)
  fmt <- function(v, digits = 3) if (is.character(v)) v else format(if (is.na(digits)) v else round(v, digits))
  out <- list(); added <- 0L
  for (lg in split(seen, by = c("logger_type", "serial"))) {
    ty <- lg$logger_type[1]; sn <- lg$serial[1]; sensors <- unique(lg$sensor); rk <- sensor_rank(sensors)
    mine <- sens[logger_type == ty & serial == sn]
    if (!nrow(mine)) mine <- sens[serial == sn & !nzchar(logger_type)]
    links <- mine[!nzchar(sensor)]
    if (!nrow(links) && nrow(mine)) links <- unique(mine[, .(logger_type, make, model, serial, sensor = "", depth_m = "", depth_source = "", site, start, end, notes)], by = c("site", "start", "end"))
    if (!nrow(links)) links <- data.table(logger_type = ty, make = MAKES[[ty]], model = lg$model[1], serial = sn, sensor = "", depth_m = "", depth_source = "",
                                          site = auto_site(ty, sn), start = "", end = "", notes = "not in the registry or the logger list: site named after the logger")
    listed <- !is.null(depths[[sn]]); tz <- if (listed) depths[[sn]] else tmpl_for(MAKES[[ty]], paste(lg$model[1], links$model[1]))
    for (j in seq_len(nrow(links))) for (k in seq_along(sensors)) {
      have <- mine[sensor == sensors[k] & nzchar(sensor) & site == links$site[j]]        # this sensor already has a row for this site
      if (nrow(have)) next
      z <- if (!is.null(tz) && rk[k] <= length(tz)) as_typed(tz[rk[k]], listed) else dflt(rk[k])
      out[[length(out) + 1L]] <- data.table(logger_type = ty, make = MAKES[[ty]], model = links$model[j] %||% lg$model[1], serial = sn, sensor = sensors[k],
                                            depth_m = fmt(z), depth_source = if (!is.null(tz) && rk[k] <= length(tz)) (if (listed) "logger list" else "template") else "default",
                                            site = if (nzchar(links$site[j])) links$site[j] else auto_site(ty, sn), start = links$start[j], end = links$end[j],
                                            notes = links$notes[j])
      added <- added + 1L
    }
  }
  if (length(out)) sens <- rbind(sens, rbindlist(out), fill = TRUE)
  # the link rows have been expanded where sensors were seen; keep the others (loggers with no data yet)
  seen_keys <- unique(paste(seen$logger_type, seen$serial))
  sens <- sens[!(!nzchar(sensor) & paste(logger_type, serial) %in% seen_keys)]
  # recompute template / default depths
  for (lg in split(seen, by = c("logger_type", "serial"))) {
    ty <- lg$logger_type[1]; sn <- lg$serial[1]; sensors <- unique(lg$sensor); rk <- setNames(sensor_rank(sensors), sensors)
    listed <- !is.null(depths[[sn]]); tz <- if (listed) depths[[sn]] else tmpl_for(MAKES[[ty]], paste(lg$model[1], sens[logger_type == ty & serial == sn, model][1]))
    k <- which(sens$logger_type == ty & sens$serial == sn & sens$depth_source %in% c("default", "template", "logger list") & sens$sensor %in% sensors)
    for (i in k) { r <- rk[[sens$sensor[i]]]
      if (!is.null(tz) && r <= length(tz)) set(sens, i, c("depth_m", "depth_source"), list(fmt(as_typed(tz[r], listed), NA), if (listed) "logger list" else "template"))
      else { z <- dflt(r); set(sens, i, c("depth_m", "depth_source"), list(if (is.numeric(z)) format(round(z, 3)) else z, "default")) } }
  }
  sens[is.na(sens)] <- ""
  setorder(sens, logger_type, serial, start, sensor)
  list(sensors = ensure_cols(sens, SENSOR_COLS)[, SENSOR_COLS, with = FALSE], added = added)
}

# Site and depth for every value, from the sensor rows of its serial. A value outside every deployment window of
# its sensor is removed ("outside_deployment"), so a logger reading in the truck before installation never reaches the site.
assign_sensors <- function(d, sens, type, serial, sgn = 1) {
  ty <- type; sn <- serial
  rows <- sens[logger_type == ty & norm_serial(serial) == sn & nzchar(sensor)]
  d[, `:=`(site = NA_character_, depth_m = NA_real_, depth_source = NA_character_, dep_start = as.POSIXct(NA, tz = "UTC"))]
  for (i in seq_len(nrow(rows))) {
    a <- parse_date_cell(rows$start[i]); b <- parse_date_cell(rows$end[i])
    if (!is.na(b) && !grepl(":", rows$end[i])) b <- b + 86399                      # an end date covers the whole day
    k <- which(d$sensor == rows$sensor[i] & is.na(d$site) & (is.na(a) | d$TIMESTAMP >= a) & (is.na(b) | d$TIMESTAMP <= b))
    if (length(k)) set(d, k, c("site", "depth_m", "depth_source", "dep_start"),
                       list(rows$site[i], typed_depth(rows$depth_m[i], sgn), rows$depth_source[i], a))
  }
  configured <- d$sensor %in% rows$sensor
  d <- remove_rows(d, which(configured & !is.na(d$site) & d$depth_source %in% "default" & is.na(d$depth_m)), "site", "no_depth",
                   "the sensor has no depth and default depths are switched off (Automatic corrections): type one in the sensors and depths table", detail = "summary")
  configured <- d$sensor %in% rows$sensor                                          # again: the removal above shortened d
  d <- remove_rows(d, which(configured & is.na(d$site)), "site", "outside_deployment",
                   "outside the start and end dates of this sensor's deployment (sensors and depths table)", detail = "summary")
  d
}

# ---- quality rules ----------------------------------------------------------------------------------------------
rule_applies <- function(applies_to, type, serial, site) {
  a <- trim(strsplit(applies_to %||% "", "|", fixed = TRUE)[[1]])
  if (!length(a) || all(!nzchar(a)) || any(tolower(a) == "all")) return(TRUE)
  any(tolower(a) %in% c(tolower(type), tolower(TYPE_LABEL[type]))) || any(norm_serial(a) == serial) || any(a == site)
}

# One logger's values (already given sites and depths). Rules from qc_rules.csv, then built-in checks, then manual removals.
apply_qc <- function(d, rules, manual, type, serial, cfg) {
  if (!nrow(d)) return(d)
  setorder(d, channel, TIMESTAMP)
  if (!"flag" %in% names(d)) d[, flag := ""]
  site1 <- d$site[!is.na(d$site)][1] %||% ""
  for (i in seq_len(nrow(rules))) {
    r <- rules[i]; if (!rule_applies(r$applies_to, type, serial, site1)) next
    v <- r$variable; act <- if (tolower(r$action) == "flag") "flag" else "remove"
    num <- function(x) suppressWarnings(as.numeric(x))
    mn <- num(r$min); mx <- num(r$max); sp <- num(r$max_spike); fh <- num(r$max_flat_hours); band <- num(r$flat_exempt_band); if (is.na(band)) band <- 0
    sel <- d$variable == v
    do <- function(idx, rule, why) { if (!length(idx)) return(invisible())
      if (act == "remove") d <<- remove_rows(d, idx, "qc", rule, why) else add_flag(d, idx, rule, why, if (nzchar(r$rule_id %||% "")) r$rule_id else rule) }
    if (!is.na(mn)) do(which(d$variable == v & d$value < mn), "range_min", paste("below", mn))
    if (!is.na(mx)) do(which(d$variable == v & d$value > mx), "range_max", paste("above", mx))
    sel <- d$variable == v
    if (!is.na(sp)) {
      k <- qc_spike_idx(d$value, sp, cfg$spike_k, d$channel); k <- k[sel[k]]
      do(k, "spike", paste0("a single reading that jumps by at least ", sp, " (and far more than this sensor usually moves) and comes straight back"))
    }
    if (!is.na(fh)) {
      sel <- d$variable == v
      k <- qc_flat_idx(d$value, d$TIMESTAMP, fh, min_n = 1L, group = d$channel, band = band, skip_na = FALSE); k <- k[sel[k]]
      do(k, "flat_line", paste0("unchanged for ", fh, " hours or more (a stuck sensor); values within ", band, " of zero are exempt"))
    }
  }
  # built-in checks (flags only)
  tt <- d$variable == "temp"
  if (any(tt) && isTRUE(cfg$check_air_like)) {                         # a buried sensor does not swing like air: daily range at depth
    day <- d[tt, .(rng = max(value) - min(value), n = .N), by = .(channel, depth_m, date = as.Date(TIMESTAMP))]
    z0 <- cfg$air_like_min_depth                                         # m below the ground (fixed in CFG)
    bad <- day[!is.na(depth_m) & depth_m >= z0 & rng > cfg$air_like_daily_range & n >= 4]
    if (nrow(bad)) { k <- which(tt & d$channel %in% bad$channel); k <- k[paste(d$channel[k], as.Date(d$TIMESTAMP[k])) %in% paste(bad$channel, bad$date)]   # channel and day, pasted only where the channel has a bad day
      add_flag(d, k, "air_like", paste0("daily range above ", cfg$air_like_daily_range, " \u00b0C at ", depth_lab(z0), " or deeper: the sensor may be exposed or above the ground")) }
  }
  if (any(tt) && isTRUE(cfg$check_pre_install)) {                      # all sensors read the same temperature: the string is not in the ground yet
    w <- d[tt, .(spread = max(value) - min(value), n = .N), by = TIMESTAMP][n >= 3]
    if (nrow(w)) { setorder(w, TIMESTAMP); w[, same := spread < cfg$pre_install_spread]
      w <- w[TIMESTAMP <= min(TIMESTAMP) + cfg$pre_install_days * 86400]
      lead <- w[seq_len(if (all(w$same)) nrow(w) else which(!w$same)[1] - 1L)]
      if (nrow(lead) && as.numeric(max(lead$TIMESTAMP) - min(lead$TIMESTAMP), units = "hours") >= 6)
        add_flag(d, which(tt & d$TIMESTAMP <= max(lead$TIMESTAMP)), "pre_install", "at the start of the record every sensor reads the same temperature: the string was probably not in the ground yet (trim it on Plot and explore)") }
  }
  # manual removals and flags: rows of the corrections over a period (R/config.R gt_period_to_manual()), whose site is the
  # target: sites, the logger's serial or sensors, several with | (blank or *: every logger); a site limits the row to it
  if (nrow(manual)) for (i in seq_len(nrow(manual))) {
    m <- manual[i]
    if (nzchar(m$serial) && norm_serial(m$serial) != serial) next
    k <- gt_manual_where(m$site, d, serial); if (is.null(k)) next
    a <- parse_date_cell(m$start); b <- parse_date_cell(m$end, end = TRUE)          # an end date covers the whole day
    k <- k & (is.na(a) | d$TIMESTAMP >= a) & (is.na(b) | d$TIMESTAMP <= b)
    if (nzchar(m$sensor)) k <- k & d$sensor %in% trim(strsplit(m$sensor, "|", fixed = TRUE)[[1]])
    if (nzchar(m$depth_m)) { z <- typed_depth(strsplit(m$depth_m, "|", fixed = TRUE)[[1]], depth_sign(cfg)); k <- k & !is.na(d$depth_m) & round(d$depth_m, 3) %in% round(z, 3) }
    if (nzchar(m$variable) && m$variable != "*") k <- k & gt_param_hits(m$variable, d)
    lo <- suppressWarnings(as.numeric(m$min_value)); hi <- suppressWarnings(as.numeric(m$max_value))
    if (!is.na(lo)) k <- k & d$value >= lo
    if (!is.na(hi)) k <- k & d$value <= hi
    why <- paste0(if (nzchar(m$reason)) m$reason else "removed by hand", if (nzchar(m$author)) paste0(" (", m$author, ")") else "",
                  if (!is.null(m$row)) paste0(" (row ", m$row, " of ", PERIOD_FILE, ")") else "")      # the row, for corrections applied (core applied.R)
    if (tolower(m$action) == "flag") add_flag(d, which(k), "manual_flag", why) else d <- remove_rows(d, which(k), "qc", "manual", why)
  }
  d
}

# The values of d a row's target takes (NULL: none): every one for a blank target, "*" or the logger's serial; else those at a
# site or a sensor it names.
gt_manual_where <- function(target, d, serial) {
  w <- period_parts(target); if (!length(w) || "*" %in% w || serial %in% norm_serial(w)) return(rep(TRUE, nrow(d)))
  k <- d$site %in% w | ("sensor" %in% names(d) & d$sensor %in% w); if (any(k, na.rm = TRUE)) k else NULL
}
# The values of d a row's parameter names: the variable (temp, vwc, ec) or, as the Workbench shows a series, its depth ("0.50 m",
# "vwc 0.10 m"), sensor or channel; several with |.
gt_param_hits <- function(parameter, d) {
  w <- trim(strsplit(parameter, "|", fixed = TRUE)[[1]]); lab <- ifelse(d$variable == "temp", depth_lab(d$depth_m), paste(d$variable, depth_lab(d$depth_m)))
  d$variable %in% w | lab %in% w | (if ("sensor" %in% names(d)) d$sensor %in% w else FALSE) | (if ("channel" %in% names(d)) d$channel %in% w else FALSE)
}

# One row per logger sensor: what was read, what failed, what is left.
sensor_health <- function(read_counts, d, removed, gaps) {
  left <- d[, .(values_kept = .N, flagged = sum(nzchar(flag)), first_valid = min(TIMESTAMP), last_valid = max(TIMESTAMP)), by = .(logger_id, channel)]
  h <- merge(read_counts, left, by = c("logger_id", "channel"), all.x = TRUE)
  h <- merge(h, removed, by = c("logger_id", "channel"), all.x = TRUE)
  g <- if (nrow(gaps)) gaps[, .(gaps = .N, longest_gap_h = max(length_h)), by = .(logger_id, channel)] else data.table(logger_id = character(), channel = character(), gaps = integer(), longest_gap_h = numeric())
  h <- merge(h, g, by = c("logger_id", "channel"), all.x = TRUE)
  for (v in c("values_kept", "flagged", "errors", "removed_qc", "gaps")) if (v %in% names(h)) set(h, which(is.na(h[[v]])), v, 0L)
  h[, pct_valid := round(100 * values_kept / pmax(values_read, 1), 1)]
  h[, health := fifelse(values_kept == 0, "failed: nothing usable",
                fifelse(errors > 0.5 * values_read, "failing: mostly errors",
                fifelse(errors > 0.02 * values_read | removed_qc > 0.02 * values_read, "intermittent",
                fifelse(flagged > 0.1 * pmax(values_kept, 1), "suspect", "ok"))))]
  h
}

# ---- between sites ------------------------------------------------------------------------------------------------
# Each depth that three sites or more share is compared across sites, day by day (Ovid/neighbours.R). A day on
# which one site departs suddenly from the others (beyond its usual, slowly changing offset) is flagged, not removed:
# a sensor fault, a clock or depth mix-up, or a real local event such as flooding. Complete days only.
BETWEEN_FLOOR <- function(depth) fifelse(depth <= 0.1, 5, fifelse(depth <= 0.5, 3, 1.5))     #  deg C a day must depart by, at least

between_site_days <- function(dd, depth, cfg = CFG) {
  x <- dd[depth_m == depth & complete == TRUE, .(id = site, date, value = temp)]
  if (uniqueN(x$id) < 3) return(NULL)
  neighbour_days(x, floor = BETWEEN_FLOOR(depth), k = cfg$between_site_k %||% 6, window = cfg$between_site_window %||% 31)
}

between_site_checks <- function(dd, all, cfg = CFG) {
  none <- data.table(site = character(), depth_m = numeric(), sites_compared = integer(), days_compared = integer(), median_offset = numeric(),
                     r_change = numeric(), days_departing = integer(), runs = integer(), largest_departure = numeric(), verdict = character())
  if (!nrow(dd)) return(none)
  shared <- dd[complete == TRUE, .(n = uniqueN(site)), by = depth_m][n >= 3, depth_m]
  # each sensor's sorted times, kept once: counting the records in a departing period is then a binary search, not a scan of every value
  sens <- all[variable == "temp", .(t0 = min(TIMESTAMP), t1 = max(TIMESTAMP), ts = list(sort(as.numeric(TIMESTAMP)))), by = .(site, depth_m, logger_id, channel)]
  out <- list(); pp <- list()
  for (z in sort(shared)) { days <- between_site_days(dd, z, cfg); if (is.null(days) || !nrow(days)) next
    s <- neighbour_summary(days, cfg$between_site_r_min %||% 0.5)
    out[[length(out) + 1L]] <- s[, .(site = id, depth_m = z, sites_compared = uniqueN(days$id), days_compared, median_offset, r_change, days_departing, runs, largest_departure, verdict)]
    per <- neighbour_periods(days)
    for (i in seq_len(nrow(per))) { a <- as.POSIXct(format(per$start[i]), tz = "UTC"); b <- as.POSIXct(format(per$end[i]), tz = "UTC") + 86399
      ss <- sens[site == per$id[i] & depth_m == z & t1 >= a & t0 <= b]
      for (j in seq_len(nrow(ss))) {
        x <- ss$ts[[j]]; n <- findInterval(as.numeric(b), x) - findInterval(as.numeric(a), x, left.open = TRUE)
        pp[[length(pp) + 1L]] <- data.table(site = per$id[i], logger_id = ss$logger_id[j], channel = ss$channel[j], start = a, end = b)
        FLAGS$list[[length(FLAGS$list) + 1L]] <- data.table(site = per$id[i], logger_id = ss$logger_id[j], channel = ss$channel[j], variable = "temp",
          start = fmt_ts(a), end = fmt_ts(b), records = n, check = "differs_from_other_sites",
          detail = sprintf("daily mean at %s m departs from the other sites by %+.2f \u00b0C more than usual (%d day%s)", z, per$largest[i], per$days[i], if (per$days[i] == 1) "" else "s")) } }
  }
  res <- if (length(out)) rbindlist(out)[order(verdict == "ok", -days_departing, site, depth_m)] else none
  setattr(res, "periods", if (length(pp)) rbindlist(pp) else NULL)            # the flagged stretches, for between_site_mark()
  res
}

# The between-site flags into one site's records (x: long, a row per value), so its files, the export and the WMO codes carry
# them; periods from between_site_checks(). between_site_key() tells whether a cached site's flags are the same as last time.
between_site_mark <- function(x, periods) {
  if (is.null(periods) || !nrow(periods) || is.null(x) || !nrow(x)) return(x)
  p <- periods[site %in% unique(x$site)]; if (!nrow(p)) return(x)
  if (!"flag" %in% names(x)) x[, flag := ""]
  # each sensor's temperature rows are found once, then each period's among them (a scan of every row per period was slow)
  key <- function(a, b, c) paste(a, b, c, sep = "\u001f")
  r <- which(x$variable == "temp" & !is.na(x$site) & !is.na(x$logger_id) & !is.na(x$channel)); g <- split(r, key(x$site[r], x$logger_id[r], x$channel[r]))
  for (i in seq_len(nrow(p))) { if (is.na(p$site[i]) || is.na(p$logger_id[i]) || is.na(p$channel[i])) next
    r <- g[[key(p$site[i], p$logger_id[i], p$channel[i])]]; k <- r[which(x$TIMESTAMP[r] >= p$start[i] & x$TIMESTAMP[r] <= p$end[i])]
    if (length(k)) flag_mark(x, "flag", k, "differs_from_other_sites") }
  x
}
between_site_key <- function(periods, s) {
  if (is.null(periods) || is.na(s)) return("")
  p <- periods[site == s]; if (!nrow(p)) "" else hash_of(p[order(logger_id, channel, start)])
}

# Corrections over a period (core periods.R) on a logger's long records d (a row per value: site, sensor, channel,
# variable, depth_m, value). Each series is one column as the Workbench names it (its depth, or its variable and depth),
# so a row's parameter is what the Workbench shows ("0.50 m", "vwc 0.10 m"), or the sensor or channel name; its target is
# the logger's serial, the site or the sensor. Applied after the quality rules' manual removals, before gaps are found. Rows that
# remove or flag are applied with the quality rules instead (apply_qc()), as the manual removals they took over were.
gt_period <- function(d, rows, serial) {
  if (is.null(rows) || !nrow(rows) || !nrow(d)) return(d)
  lab <- ifelse(d$variable == "temp", depth_lab(d$depth_m), paste(d$variable, depth_lab(d$depth_m)))
  grp <- paste(d$site, d$sensor %||% "", d$channel, lab, sep = "\r"); gone <- integer()
  for (g in unique(grp)) { i <- which(grp == g); s0 <- d$site[i[1]]; sen <- if ("sensor" %in% names(d)) d$sensor[i[1]] else ""; ch <- d$channel[i[1]]; lb <- lab[i[1]]
    pr <- period_for(rows, c(serial, s0, sen)); if (!nrow(pr)) next
    nm <- unique(c(lb, sen, ch)); nm <- nm[!is.na(nm) & nzchar(nm)]
    pr <- pr[!nzchar(parameter) | parameter == "*" | vapply(strsplit(parameter, "|", fixed = TRUE), function(p) any(trim(p) %in% nm), NA)]
    if (!nrow(pr)) next
    pr <- copy(pr)[, parameter := ""]                                                   # matched here: the one series below
    x <- data.table(TIMESTAMP = d$TIMESTAMP[i], v = as.numeric(d$value[i]))
    x <- period_step(x, pr, c(serial, s0, sen), flag = function(dt, p, idx, check, detail) add_flag(d, i[idx], check, detail),
                     where = list(site = s0, logger_id = d$logger_id[i[1]], channel = ch, variable = d$variable[i[1]]))
    old <- as.numeric(d$value[i]); out <- is.na(x$v) & !is.na(old); gone <- c(gone, i[out])
    if (any(!out)) set(d, i[!out], "value", x$v[!out]) }
  if (length(gone)) d <- remove_rows(d, sort(unique(gone)), "period", "period_remove", paste("removed by a correction over a period (", PERIOD_FILE, ")"), detail = "summary")
  d
}
