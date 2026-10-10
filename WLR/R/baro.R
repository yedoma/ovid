# R/baro.R  --  Barometric groups, primary and secondary barologgers per year, suggestions, and compensation.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.
#
# For each processing year, loggers are put in baro groups (by default, the barologger nearest each site). Each group
# has a primary and a secondary barologger chosen by the user from the barologgers found that year. The app suggests
# both. The secondary fills any gap in the primary, and every filled reading is flagged.

BARO_GROUP_COLS  <- c("year", "group", "primary", "secondary", "notes")
BARO_MEMBER_COLS <- c("year", "group", "site", "serial")

# Linear interpolation of a barometric series onto other times, leaving holes where the series has a gap.
baro_at <- function(bt, bv, t, max_gap_s) {
  ok <- !is.na(bv) & !is.na(bt); bt <- as.numeric(bt[ok]); bv <- bv[ok]
  if (length(bt) < 2) return(rep(NA_real_, length(t)))
  t <- as.numeric(t); y <- stats::approx(bt, bv, xout = t, rule = 1, ties = mean)$y
  i <- findInterval(t, bt); lo <- bt[pmax(i, 1)]; hi <- bt[pmin(i + 1, length(bt))]
  far <- (hi - lo) > max_gap_s & !(t %in% bt)
  y[far | i == 0 | i >= length(bt) & t > max(bt)] <- NA
  y
}

# The longest gap a barometric series is interpolated across (s): baro_max_gap_h or 1.5 logging intervals, whichever is longer;
# 0 with baro_interp off, so only water readings at the time of a barometric reading are compensated.
baro_gap_s <- function(bt, cfg) if (isFALSE(cfg$baro_interp)) 0 else max(cfg$baro_max_gap_h * 3600, 1.5 * step_of(bt), na.rm = TRUE)

# Coverage of a time window by a barologger record (share of the window within reach of a baro reading).
baro_coverage <- function(bt, a, b, step_s) {
  if (!length(bt) || is.na(a) || is.na(b) || b <= a) return(0)
  bt <- sort(as.numeric(bt)); bt <- bt[bt >= as.numeric(a) - step_s & bt <= as.numeric(b) + step_s]
  if (length(bt) < 2) return(0)
  g <- diff(bt); covered <- sum(pmin(g, 1.5 * step_s)[g <= 1.5 * step_s])
  min(1, covered / (as.numeric(b) - as.numeric(a)))
}

# Suggested groups and barologgers for one year.
#   deps : deployments of water loggers that year (serial, site, start, end, easting, northing)
#   baros: list serial -> data.table(TIMESTAMP, baro_kpa); bmeta: data.table(serial, site, easting, northing)
#   agreement: baro_agreement(), or a copy of it that remembers its answers (the pipeline's, kept between runs)
suggest_baro <- function(year, deps, baros, bmeta, agreement = baro_agreement) {
  if (!length(baros) || !nrow(deps)) return(list(groups = data.table(), members = data.table(), scores = data.table()))
  bs <- names(baros)
  bm <- merge(data.table(serial = bs), bmeta, by = "serial", all.x = TRUE)
  bm[is.na(site) | !nzchar(site), site := paste("Baro", serial)]
  dist_km <- function(e, n, e2, n2) ifelse(is.na(e) | is.na(e2), NA_real_, sqrt((e - e2)^2 + (n - n2)^2) / 1000)
  # each site joins the group of its nearest barologger (by workbook coordinates); sites without coordinates join the largest group
  near <- vapply(seq_len(nrow(deps)), function(i) { d <- dist_km(deps$easting[i], deps$northing[i], bm$easting, bm$northing)
    if (all(is.na(d))) NA_character_ else bm$site[which.min(d)] }, "")
  members <- data.table(year = year, group = near, site = deps$site, serial = deps$serial)
  if (any(is.na(members$group))) members[is.na(group), group := names(sort(table(members$group), decreasing = TRUE))[1] %||% bm$site[1]]
  scores <- rbindlist(lapply(unique(members$group), function(gname) {
    m <- deps[members$group == gname]; a <- suppressWarnings(min(m$start, na.rm = TRUE)); b <- suppressWarnings(max(m$end, na.rm = TRUE))
    if (!is.finite(a)) a <- suppressWarnings(min(unlist(lapply(baros, function(x) min(x$TIMESTAMP)))))
    if (!is.finite(b)) b <- suppressWarnings(max(unlist(lapply(baros, function(x) max(x$TIMESTAMP)))))
    a <- as.POSIXct(a, origin = "1970-01-01", tz = "UTC"); b <- as.POSIXct(b, origin = "1970-01-01", tz = "UTC")
    ce <- mean(m$easting, na.rm = TRUE); cn <- mean(m$northing, na.rm = TRUE)
    rbindlist(lapply(seq_len(nrow(bm)), function(j) { x <- baros[[bm$serial[j]]]; st <- step_of(x$TIMESTAMP)
      data.table(group = gname, serial = bm$serial[j], baro_site = bm$site[j], coverage = round(baro_coverage(x$TIMESTAMP, a, b, st), 3),
                 distance_km = round(dist_km(ce, cn, bm$easting[j], bm$northing[j]), 2), interval_s = st) }))
  }))
  # agreement with the other barologgers: median absolute departure from the median of all of them (kPa)
  agree <- agreement(baros)
  scores <- merge(scores, agree[, .(serial, departure_kpa)], by = "serial", all.x = TRUE)
  scores[, score := coverage * 3 - ifelse(is.na(distance_km), 0.5, pmin(distance_km, 20) / 10) - pmin(interval_s, 21600) / 21600 * 0.5 -
           ifelse(is.na(departure_kpa), 0, pmin(departure_kpa, 2))]
  setorder(scores, group, -score)
  groups <- scores[, .(primary = serial[1], secondary = if (.N > 1) serial[2] else "",
                       notes = sprintf("suggested: %s covers %.0f%%%s", baro_site[1], 100 * coverage[1], if (.N > 1) sprintf("; then %s (%.0f%%)", baro_site[2], 100 * coverage[2]) else "")), by = group]
  groups[, year := year]
  list(groups = groups[, BARO_GROUP_COLS, with = FALSE], members = members[, BARO_MEMBER_COLS, with = FALSE], scores = scores)
}

baro_agreement <- function(baros, max_gap_s = 6 * 3600) {
  if (length(baros) < 2) return(data.table(serial = names(baros), departure_kpa = NA_real_))
  t <- sort(unique(unlist(lapply(baros, function(x) as.numeric(x$TIMESTAMP[as.numeric(x$TIMESTAMP) %% 3600 == 0])))))
  M <- sapply(baros, function(x) baro_at(x$TIMESTAMP, x$baro_kpa, t, max_gap_s))
  if (is.null(dim(M))) M <- matrix(M, nrow = 1)
  med <- apply(M, 1, stats::median, na.rm = TRUE)
  data.table(serial = names(baros), departure_kpa = round(apply(abs(M - med), 2, stats::median, na.rm = TRUE), 3))
}

# Pairs of barologgers compared over time (daily medians of the difference) for the Barometric page.
baro_compare <- function(baros) {
  if (length(baros) < 2) return(data.table())
  pr <- utils::combn(names(baros), 2, simplify = FALSE)
  rbindlist(lapply(pr, function(p) { a <- baros[[p[1]]]; b <- baros[[p[2]]]
    v <- baro_at(b$TIMESTAMP, b$baro_kpa, a$TIMESTAMP, 6 * 3600); d <- a$baro_kpa - v
    x <- data.table(day = as.Date(a$TIMESTAMP), d = d)[!is.na(d), .(diff_kpa = stats::median(d)), by = day]
    if (nrow(x)) x[, pair := paste(p, collapse = " - ")] else NULL }))
}

# Water pressure minus barometric pressure. Returns baro_kpa, baro_source (primary / secondary serial) and flags.
compensate <- function(dt, primary, secondary, cfg) {
  gap <- function(x) baro_gap_s(x$TIMESTAMP, cfg)
  b1 <- if (!is.null(primary)) baro_at(primary$TIMESTAMP, primary$baro_kpa, dt$TIMESTAMP, gap(primary)) else rep(NA_real_, nrow(dt))
  b2 <- if (!is.null(secondary)) baro_at(secondary$TIMESTAMP, secondary$baro_kpa, dt$TIMESTAMP, gap(secondary)) else rep(NA_real_, nrow(dt))
  use2 <- is.na(b1) & !is.na(b2)
  dt[, baro_kpa := ifelse(use2, b2, b1)][, baro_source := ifelse(!is.na(b1), "primary", ifelse(use2, "secondary", ""))]
  invisible(dt)
}

# ---- baro stations: one series per barologger location across years and instruments --------------------------------
# A barologger is often replaced, or moved between years. A station is a place with a barometric record; the serials that
# were there are stitched into one series (baro_stations.csv says which serial belongs to which station; by default the
# workbook site of the serial). Where two serials overlap, the one installed later wins; each change of instrument is a
# seam, reported with the difference between the two over their overlap.
BARO_STATION_COLS <- c("serial", "station", "notes")

# Loggers redeployed between roles: typed Baro in some workbook years and WLR (or Piezo) in others, and not in a baro folder.
redeployed_serials <- function(md, baro_serials, folder_baro = character()) {
  if (is.null(md) || !nrow(md)) return(character())
  setdiff(intersect(baro_serials, md[type != "Baro" & !is.na(serial) & nzchar(serial), serial]), folder_baro)
}
# Workbook rows of those loggers that name a different make from the logger's Baro rows (a Solinst WLR row for a HOBO
# barologger): two loggers sharing a short serial, not a redeployment. These rows are set aside for that serial.
role_conflicts <- function(md, serials) {
  if (!length(serials)) return(integer())
  make <- function(x) { x <- tolower(sub("\\s.*$", "", trim(as.character(x)))); x[is.na(x) | x %in% c("", "na")] <- NA; x }
  mk <- make(md$logger); out <- integer()
  for (s in serials) { a <- unique(na.omit(mk[md$serial %in% s & md$type == "Baro"]))
    if (length(a)) out <- c(out, which(md$serial %in% s & md$type != "Baro" & !is.na(mk) & !mk %in% a)) }
  out
}
# The time windows in which each of those loggers was a water logger: its WLR (or Piezo) deployments. Outside them,
# from its first Baro deployment on, it is a barologger (a barologger has no removal date: it reads until its next water deployment).
water_windows_of <- function(md, overrides, serials) {
  if (!length(serials)) return(data.table(serial = character(), start = as.POSIXct(character(), tz = "UTC"), end = as.POSIXct(character(), tz = "UTC")))
  build_deployments(md, overrides)[serial %in% serials & type != "Baro", .(serial, start, end)]
}
in_windows <- function(w, s, t) {
  w <- w[serial == s]; k <- rep(FALSE, length(t))
  for (i in seq_len(nrow(w))) k <- k | ((is.na(w$start[i]) | t >= w$start[i]) & (is.na(w$end[i]) | t < w$end[i]))
  k
}

default_baro_stations <- function(serials, md) {
  st <- vapply(serials, function(s) { r <- if (!is.null(md) && nrow(md)) md[serial == s & nzchar(site %||% "")][order(-year)] else data.table()
    if (nrow(r)) r$site[1] else paste("Baro", s) }, "")
  data.table(serial = serials, station = unname(st), notes = "from the metadata workbook")
}

stitch_baro_station <- function(station, serials, cleaned) {
  parts <- lapply(serials, function(s) { b <- cleaned[[s]]; if (is.null(b) || !nrow(b)) NULL else b[, .(TIMESTAMP, baro_kpa, baro_serial = s)] })
  parts <- parts[!vapply(parts, is.null, TRUE)]
  if (!length(parts)) return(list(series = NULL, seams = data.table()))
  first <- vapply(parts, function(p) as.numeric(min(p$TIMESTAMP)), 0); parts <- parts[order(first)]
  rank <- setNames(seq_along(parts), vapply(parts, function(p) p$baro_serial[1], ""))
  x <- rbindlist(parts)[, .r := rank[baro_serial]]
  setorder(x, TIMESTAMP, -.r); x <- x[!duplicated(TIMESTAMP)][, .r := NULL]
  seams <- if (length(parts) > 1) rbindlist(lapply(2:length(parts), function(i) { a <- parts[[i - 1]]; b <- parts[[i]]
    v <- baro_at(a$TIMESTAMP, a$baro_kpa, b$TIMESTAMP, 6 * 3600); ok <- !is.na(v)
    data.table(station = station, from_serial = a$baro_serial[1], to_serial = b$baro_serial[1], change_at = min(b$TIMESTAMP),
               overlap_days = round(sum(ok) * step_of(b$TIMESTAMP) / 86400, 1), difference_kpa = if (any(ok)) round(stats::median(b$baro_kpa[ok] - v[ok]), 3) else NA_real_,
               gap_hours = round(max(0, as.numeric(min(b$TIMESTAMP)) - as.numeric(max(a$TIMESTAMP))) / 3600, 1)) })) else data.table()
  list(series = x, seams = seams)
}

# ---- other barometric records as fill-ins (config/external_baro.csv) --------------------------------------------------------
# Station pressure from another instrument, a Campbell climate station's export for instance, used after the barologgers when
# neither chosen one has a reading (with baro_fill on). Any CSV with a time and a pressure column: units kPa, hPa, mbar, Pa,
# mmHg or inHg; utc_offset_h the zone of its times (blank: the base zone). Where both elevations are given the pressure is
# brought to reference_elevation_m with the hypsometric equation (air temperature from temp_column, else 5 C); like every
# fill-in it is then shifted onto the primary barologger's level (median difference where both have readings).
EXT_BARO_COLS <- c("name", "file", "time_column", "pressure_column", "units", "utc_offset_h", "station_elevation_m", "reference_elevation_m", "temp_column", "notes")
EXT_BARO_UNITS <- c(kpa = 1, hpa = 0.1, mbar = 0.1, mb = 0.1, pa = 0.001, mmhg = 0.133322, inhg = 3.38639)
read_external_baro <- function(tab) {
  out <- list(); if (is.null(tab) || !nrow(tab)) return(out)
  for (i in seq_len(nrow(tab))) { r <- tab[i]; nm <- trim(r$name); f <- path.expand(trim(r$file)); if (!nzchar(nm) || !nzchar(f)) next
    where <- paste("external_baro.csv row", i)
    if (!file.exists(f)) { problem(where, paste("file not found:", r$file)); next }
    d <- tryCatch(fread(f, showProgress = FALSE), error = function(e) { problem(where, conditionMessage(e)); NULL }); if (is.null(d) || !nrow(d)) next
    tc <- if (nzchar(trim(r$time_column))) trim(r$time_column) else names(d)[grepl("^(timestamp|datetime|date ?time|time)$", names(d), ignore.case = TRUE)][1]
    pc <- trim(r$pressure_column); u <- EXT_BARO_UNITS[tolower(gsub("[^A-Za-z]", "", r$units %||% "kpa"))]; if (is.na(u)) u <- 1
    if (is.na(tc) || !tc %in% names(d) || !pc %in% names(d)) { problem(where, paste0("columns '", tc, "' / '", pc, "' not found in ", basename(f))); next }
    t <- parse_date_cell(as.character(d[[tc]])); p <- suppressWarnings(as.numeric(d[[pc]])) * u
    off <- suppressWarnings(as.numeric(r$utc_offset_h)); if (!is.na(off)) t <- t - (off - BASE_OFFSET) * 3600
    zs <- suppressWarnings(as.numeric(r$station_elevation_m)); zr <- suppressWarnings(as.numeric(r$reference_elevation_m))
    if (!is.na(zs) && !is.na(zr)) { tk <- 273.15 + if (nzchar(trim(r$temp_column)) && trim(r$temp_column) %in% names(d)) suppressWarnings(as.numeric(d[[trim(r$temp_column)]])) else 5
      tk[!is.finite(tk)] <- 278.15; p <- p * exp(-9.80665 * (zr - zs) / (287.05 * tk)) }
    ok <- !is.na(t) & is.finite(p) & p > 50 & p < 110                                     # kPa; anything else is not station pressure
    if (!any(ok)) { problem(where, "no readable pressure (check the units)"); next }
    x <- data.table(TIMESTAMP = t[ok], baro_kpa = round(p[ok], 4))[order(TIMESTAMP)]; x <- unique(x, by = "TIMESTAMP")
    out[[paste("ext:", nm)]] <- x }
  out
}

# The processing year a time belongs to: a year's sheet runs from the previous mid-September download sweep to its own.
season_year <- function(t) { y <- as.integer(format(t, "%Y")); y + (t > as.POSIXct(paste0(y, "-09-15 12:00:00"), tz = "UTC")) }

# The water (hydrological) year a time belongs to, for summaries, hydroperiod, recharge and the season plots: from `start`
# ("MM-DD") to the day before it, named for the year it ends in. Default 1 October to 30 September (USGS; AMS Glossary of
# Meteorology; Haynes et al. 2018; Connon et al. 2021). A project
# may set another start (setting water_year_start). The deployment (processing) year above stays on the download sweep.
WATER_YEAR_START <- "10-01"
water_year_start_valid <- function(start) { start <- trimws(as.character(start %||% ""))[1]
  !is.na(start) && grepl("^[0-9]{1,2}-[0-9]{1,2}$", start) && !is.na(tryCatch(as.Date(paste0("2001-", start)), error = function(e) NA)) }
water_year_start_ok <- function(start) if (water_year_start_valid(start)) format(as.Date(paste0("2001-", trimws(start[1]))), "%m-%d") else WATER_YEAR_START
water_year_of <- function(t, start = WATER_YEAR_START) { start <- water_year_start_ok(start)
  if (inherits(t, "Date")) t <- as.POSIXct(paste(t, "12:00:00"), tz = "UTC")
  y <- as.integer(format(t, "%Y")); y + (t >= as.POSIXct(paste0(y, "-", start, " 00:00:00"), tz = "UTC")) - (start == "01-01") }
water_year_first_day <- function(year, start = WATER_YEAR_START) { start <- water_year_start_ok(start)
  as.POSIXct(paste0(year - (start != "01-01"), "-", start), tz = "UTC") }
water_year_label <- function(start = WATER_YEAR_START) { d <- as.Date(paste0("2001-", water_year_start_ok(start))); paste(as.integer(format(d, "%d")), format(d, "%B")) }

# Fill a water record's barometric pressure from an ordered chain of sources (primary, secondary, then fill-ins).
# Fill-ins are shifted onto the level of the first source in the chain (median difference over their overlap), so a
# barologger at another elevation does not put a step into the water level.
compensate_chain <- function(t, chain, cfg, ref = NULL) {
  out <- rep(NA_real_, length(t)); src <- rep("", length(t)); nm <- rep("", length(t)); off <- rep(0, length(t)); ser <- rep("", length(t)); nb <- 0
  for (c in chain) {
    if (is.null(c$series) || !nrow(c$series)) next
    todo <- which(is.na(out)); if (!length(todo)) break
    gap <- baro_gap_s(c$series$TIMESTAMP, cfg)
    v <- baro_at(c$series$TIMESTAMP, c$series$baro_kpa, t[todo], gap)
    shift <- 0
    if (identical(c$role, "fill") && !is.null(ref) && nrow(ref)) {
      r <- baro_at(ref$TIMESTAMP, ref$baro_kpa, c$series$TIMESTAMP, 3 * 3600); ok <- !is.na(r)
      shift <- if (sum(ok) > 24) stats::median(r[ok] - c$series$baro_kpa[ok]) else 0
    }
    k <- !is.na(v); bt <- as.numeric(c$series$TIMESTAMP[!is.na(c$series$baro_kpa)]); tk <- as.numeric(t[todo[k]]); i <- findInterval(tk, bt)   # bridged: across a gap of more than a step
    nb <- nb + sum(!tk %in% bt & i > 0 & bt[pmin(i + 1, length(bt))] - bt[pmax(i, 1)] > 1.5 * step_of(c$series$TIMESTAMP), na.rm = TRUE)
    out[todo[k]] <- v[k] + shift; src[todo[k]] <- c$role; nm[todo[k]] <- c$name; off[todo[k]] <- round(shift, 3)
    if ("baro_serial" %in% names(c$series) && any(k)) ser[todo[k]] <- c$series$baro_serial[pmax(1L, findInterval(as.numeric(t[todo[k]]), as.numeric(c$series$TIMESTAMP)))]
  }
  structure(list(baro_kpa = out, baro_source = src, baro_name = nm, baro_shift = off, baro_serial = ser), bridged = nb)   # bridged: readings interpolated across a gap
}
