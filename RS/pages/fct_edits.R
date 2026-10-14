# RS/pages/fct_edits.R  --  Editing flights from the Flights list, as plain functions (no Shiny; tests call them): edits
# staged in the list and saved to flights.csv at once, joined flights shown as one row, site names found in folder names
# matched to known sites, the logs an eBee flight may have, and the other geodata a scan found, by folder.

# ---- edits staged in the Flights list ------------------------------------------------------------------------------------
# staged: one row per change (flight_id, col: a flights.csv column, value, step); a step is one action on the list, so Undo
# takes back the last action whatever it changed, and the latest step wins for a cell.
edit_empty <- function() data.table(flight_id = character(), col = character(), value = character(), step = integer())
edit_stage <- function(staged, ids, col, value, step = if (nrow(staged)) max(staged$step) + 1L else 1L) {
  ids <- unique(ids[!is.na(ids) & nzchar(ids)]); if (!length(ids)) return(staged)
  rbind(staged, data.table(flight_id = ids, col = col, value = trim(value %||% ""), step = step))
}
edit_undo <- function(staged) if (nrow(staged)) staged[step != max(step)] else staged
edit_final <- function(staged) if (!nrow(staged)) staged else unique(staged[order(-step)], by = c("flight_id", "col"))
edit_count <- function(staged) nrow(edit_final(staged))
# flights.csv with the staged values written in; a flight with no row yet gets one.
edit_apply <- function(flights, staged) {
  cf <- ensure_cols(as.data.table(flights), FLIGHT_COLS); f <- edit_final(staged)
  for (i in seq_len(nrow(f))) { j <- which(cf$flight_id == f$flight_id[i])
    if (!length(j)) { cf <- rbind(cf, ensure_cols(data.table(flight_id = f$flight_id[i]), FLIGHT_COLS)[, FLIGHT_COLS, with = FALSE], fill = TRUE); j <- nrow(cf) }
    set(cf, j, f$col[i], f$value[i]) }
  cf[]
}
# The catalogue's flights with the staged values in, as the list shows them: a blank site or positioning status shows what
# the scan found, a blank name keeps the name shown. staged_cells: "flight_id|column" of each staged cell.
edit_view <- function(fl, staged) {
  f <- edit_final(staged); if (!nrow(f) || !nrow(fl)) return(fl); fl <- copy(fl)
  for (i in seq_len(nrow(f))) { r <- which(fl$flight_id == f$flight_id[i]); if (!length(r)) next
    v <- f$value[i]; k <- f$col[i]
    if (k == "display_name") { if (nzchar(v)) set(fl, r, "name", v); next }
    if (!nzchar(v)) v <- switch(k, site = fl$site_found[r] %||% "", positioning_status = fl$positioning_found[r] %||% "", kind = fl$kind_found[r] %||% "", v)
    if (k %in% names(fl)) set(fl, r, k, v) }
  fl
}
edit_cells <- function(staged) { f <- edit_final(staged); paste(f$flight_id, f$col, sep = "|") }
# The values offered in a column's dropdown: the known sites (sites.csv) first, then the values flights already have.
edit_choices <- function(fl, col, sites = NULL) {
  have <- function(x) { x <- trim(as.character(x)); sort(unique(x[!is.na(x) & nzchar(x)])) }
  if (col == "site") unique(c(have(sites$site), have(fl$site))) else have(fl[[col]])
}

# ---- flights joined by a person -------------------------------------------------------------------------------------------
# A flight joined from sorties (flights.csv mission_set names it) is one row in the Flights list and on the map: the first
# sortie's row with the images and size of all of them and their whole extent. The sorties stay flights of their own in the
# catalogue and its exports. parts: sorties in the row; part_ids: their flight ids.
fold_joined <- function(fl) {
  fl <- copy(fl); fl[, `:=`(parts = rep(1L, .N), part_ids = as.character(flight_id))]
  if (!nrow(fl)) return(fl)
  if (!"mission_set" %in% names(fl)) return(fl)
  ms <- trim(as.character(fl$mission_set)); ms[is.na(ms) | tolower(ms) == "own"] <- ""
  fl[, set_ := ms]; fl[nzchar(set_), n_ := .N, by = set_]
  j <- nzchar(ms) & fl$n_ %in% 2:100000
  if (!any(j)) return(fl[, c("set_", "n_") := NULL][])
  num <- function(x) suppressWarnings(as.numeric(x))
  g <- fl[j][order(set_, start)]
  s <- g[, .(n = sum(num(n_images), na.rm = TRUE), gb = round(sum(num(size_gb), na.rm = TRUE), 2), a1 = rng(num(lat_min), min), a2 = rng(num(lat_max), max),
             o1 = rng(num(lon_min), min), o2 = rng(num(lon_max), max), parts = .N, part_ids = paste(flight_id, collapse = " ")), by = set_]
  one <- g[, .SD[1], by = set_][s, on = "set_", `:=`(n_images = i.n, size_gb = i.gb, lat_min = i.a1, lat_max = i.a2, lon_min = i.o1, lon_max = i.o2, parts = i.parts, part_ids = i.part_ids)]
  dn <- if ("display_name" %in% names(one)) trim(as.character(one$display_name)) else rep("", nrow(one)); dn[is.na(dn)] <- ""
  one[!nzchar(dn), name := set_]
  rbind(fl[!j], one, fill = TRUE)[, c("set_", "n_") := NULL][]
}

# ---- site names found in folder names ------------------------------------------------------------------------------------
# Every site name the scan read from folder names (site_folder) that is not a known site (sites.csv), with its flights and a
# suggested known site, and why: the same name in another case; a name it contains or that contains it (a known site first,
# then the name most flights have); for a name several flights use, itself; or the known site whose sites.csv coordinates its flights cover. Flights with no name
# are one row, found "". names: config/site_names.csv, what was already decided.
SITE_KEEP_MIN <- 3         # a name this many flights use, with no other match, is suggested as a site of its own
site_suggest <- function(fl, sites = NULL, names = NULL) {
  out <- data.table(found = character(), flights = integer(), suggested = character(), why = character(), mapped = character())
  if (!nrow(fl)) return(out)
  fn <- if ("site_folder" %in% names(fl)) as.character(fl$site_folder) else as.character(fl$site); fn[is.na(fn)] <- ""
  known <- trim(as.character(sites$site)); known <- known[!is.na(known) & nzchar(known)]
  cnt <- data.table(found = fn)[, .(flights = .N), by = found][order(-flights, found)]
  cand <- unique(c(known, cnt[nzchar(found)]$found))
  notsite <- if (!is.null(names) && nrow(names)) tolower(trim(as.character(names$found[!nzchar(trim(as.character(names$site)) %|na|% "")]))) else character()
  good <- unique(c(known, cnt[nzchar(found) & flights >= SITE_KEEP_MIN & !tolower(found) %in% notsite]$found))   # targets for a match by name or area
  box <- function(nm) { i <- fn == nm; num <- function(x) suppressWarnings(as.numeric(x[i]))
    c(rng(num(fl$lat_min), min), rng(num(fl$lat_max), max), rng(num(fl$lon_min), min), rng(num(fl$lon_max), max)) }
  pts <- if (!is.null(sites) && nrow(sites) && all(c("latitude", "longitude") %in% names(sites))) data.table(site = trim(as.character(sites$site)), lat = suppressWarnings(as.numeric(sites$latitude)), lon = suppressWarnings(as.numeric(sites$longitude)))[is.finite(lat) & is.finite(lon)] else data.table()
  rank <- function(x) x[order(!x %in% known, -cnt$flights[match(x, cnt$found)] %|na|% 0)]
  for (i in seq_len(nrow(cnt))) { nm <- cnt$found[i]; if (nm %in% known) next
    sug <- ""; why <- ""; others <- setdiff(cand, nm); low <- tolower(nm)
    m <- others[tolower(others) == low & (others %in% known | cnt$flights[match(others, cnt$found)] %|na|% 0 > cnt$flights[i])]
    if (length(m)) { sug <- rank(m)[1]; why <- "same name, other case" }
    others <- setdiff(good, nm)
    if (!nzchar(sug) && nchar(nm) >= 3) { m <- others[nchar(others) >= 3 & vapply(others, function(o) grepl(tolower(o), low, fixed = TRUE) || grepl(low, tolower(o), fixed = TRUE), NA)]
      m <- m[m %in% known | cnt$flights[match(m, cnt$found)] %|na|% 0 >= cnt$flights[i]]
      if (length(m)) { sug <- rank(m)[1]; why <- paste("name contains", sug) } }
    if (!nzchar(sug) && nzchar(nm) && cnt$flights[i] >= SITE_KEEP_MIN) { sug <- nm; why <- paste(cnt$flights[i], "flights use this name: keep it as a site") }
    if (!nzchar(sug) && nrow(pts)) { b <- box(nm)                     # a known site with coordinates (sites.csv) inside these flights' area
      inb <- is.finite(b[1]) & pts$lat >= b[1] & pts$lat <= b[2] & pts$lon >= b[3] & pts$lon <= b[4]
      if (any(inb)) { d <- (pts$lat - mean(b[1:2]))^2 + (pts$lon - mean(b[3:4]))^2; d[!inb] <- Inf; sug <- pts$site[which.min(d)]; why <- paste("the flights cover", sug, "(sites.csv coordinates)") } }
    out <- rbind(out, data.table(found = nm, flights = cnt$flights[i], suggested = sug, why = if (nzchar(why)) why else "no match", mapped = "")) }
  if (!is.null(names) && nrow(names)) { k <- match(tolower(out$found), tolower(trim(as.character(names$found))))
    out[!is.na(k), mapped := fifelse(nzchar(trim(as.character(names$site[k[!is.na(k)]]))), trim(as.character(names$site[k[!is.na(k)]])), "(not a site)")] }
  out[]
}
# site_names.csv with a row per mapping (found -> site; site "" = not a site), replacing rows for the same names.
site_names_set <- function(names, found, site) {
  d <- ensure_cols(as.data.table(names %||% data.table()), SITE_NAME_COLS)[, SITE_NAME_COLS, with = FALSE]
  f0 <- found; d <- d[!tolower(trim(found)) %in% tolower(trim(f0))]
  rbind(d, data.table(found = f0, site = site, note = ""))[]
}
# sites.csv with any of these sites it lacks added (a name kept as a site becomes a known site).
sites_add <- function(sites, add) {
  d <- ensure_cols(as.data.table(sites %||% data.table()), SITE_COLS)[, SITE_COLS, with = FALSE]
  add <- unique(trim(add[!is.na(add) & nzchar(trim(add))])); add <- add[!tolower(add) %in% tolower(trim(d$site))]
  if (length(add)) d <- rbind(d, ensure_cols(data.table(site = add), SITE_COLS)[, SITE_COLS, with = FALSE])
  d[]
}

# ---- an eBee flight's logs -----------------------------------------------------------------------------------------------
# Logs no flight has (catalogue/logs.csv) that may be this flight's: the same aircraft serial (when both are known) and a
# time within a day of the flight's, or any time when the flight has none; the closest photo-tag count to the flight's
# images first. f: one flight row; logs: catalogue/logs.csv.
log_candidates <- function(f, logs, n = 8L) {
  if (is.null(logs) || !nrow(logs) || is.null(f) || !nrow(f)) return(data.table())
  d <- logs[is.na(flight_id) | !nzchar(as.character(flight_id))]
  ser <- as.character(f$aircraft_serial %||% NA)[1]
  if (!is.na(ser) && nzchar(ser) && "serial" %in% names(d)) d <- d[is.na(serial) | serial == ser]
  t0 <- suppressWarnings(as.POSIXct(as.character(f$start_utc %||% NA)[1], tz = "UTC")); ts <- suppressWarnings(as.POSIXct(as.character(d$start_utc), tz = "UTC"))
  if (!is.na(t0)) d <- d[!is.na(ts) & abs(as.numeric(difftime(ts, t0, units = "hours"))) <= 24]
  if (!nrow(d)) return(d)
  ni <- suppressWarnings(as.numeric(f$n_captures %||% f$n_images))[1]
  d[, gap := abs(suppressWarnings(as.numeric(photos)) - ni)][order(gap, start_utc, na.last = TRUE)][seq_len(min(.N, n))][, gap := NULL][]
}

# ---- other geodata a scan found ------------------------------------------------------------------------------------------
# The satellite, aerial, LiDAR and raster files (datasets.csv) and GIS layers (gis_layers.csv), one row per folder and kind:
# files, where they are (no position, inside or near the flights, or far from them), and whether removal is suggested (no
# position, or more than pad_deg from every flight). Removing writes an ignore rule for the folder (geodata_rule).
geodata_folders <- function(ds, gl, fl, pad_deg = 0.5) {
  pick <- function(d, kind) if (is.null(d) || !nrow(d)) NULL else d[, .(source_id, folder = as.character(folder), kind = if (is.null(kind)) as.character(d$kind) else kind,
                                                                     lat_min = suppressWarnings(as.numeric(lat_min)), lat_max = suppressWarnings(as.numeric(lat_max)),
                                                                     lon_min = suppressWarnings(as.numeric(lon_min)), lon_max = suppressWarnings(as.numeric(lon_max)))]
  d <- rbindlist(list(pick(ds, NULL), pick(gl, "GIS layer")), fill = TRUE)
  if (is.null(d) || !nrow(d)) return(data.table(source_id = character(), folder = character(), kind = character(), files = integer(), where = character(), suggest = character()))
  d[is.na(folder), folder := ""]
  g <- d[, .(files = .N, a1 = rng(lat_min, min), a2 = rng(lat_max, max), o1 = rng(lon_min, min), o2 = rng(lon_max, max)), by = .(source_id, folder, kind)]
  num <- function(x) suppressWarnings(as.numeric(x))
  fb <- if (nrow(fl)) fl[is.finite(num(lat_min)), .(a1 = num(lat_min) - pad_deg, a2 = num(lat_max) + pad_deg, o1 = num(lon_min) - pad_deg, o2 = num(lon_max) + pad_deg)] else data.table()
  near <- vapply(seq_len(nrow(g)), function(i) is.finite(g$a1[i]) && nrow(fb) > 0 && any(g$a1[i] <= fb$a2 & fb$a1 <= g$a2[i] & g$o1[i] <= fb$o2 & fb$o1 <= g$o2[i]), NA)
  g[, where := fifelse(!is.finite(a1), "no position", fifelse(near, "near the flights", "far from the flights"))]
  g[, suggest := fifelse(where == "near the flights", "keep", "remove")]
  g[order(suggest != "remove", -files), .(source_id, folder, kind, files, where, suggest)]
}
GEODATA_REASON <- "not remote sensing (removed from Other geodata)"
# file_rules.csv rows that ignore everything in these folders (paths within their source).
geodata_rule <- function(folders) {
  folders <- unique(folders[!is.na(folders)])
  data.table(pattern = ifelse(nzchar(folders), paste0("^", rx_escape(folders), "/[^/]+$"), "^[^/]+$"), action = "ignore", reason = GEODATA_REASON)
}
# The review id of a folder of other geodata (config/review_decisions.csv), so a folder kept is not asked about again.
geodata_id <- function(source_id, folder, kind) review_hash(paste("geodata", source_id, folder, kind, sep = "|"))
