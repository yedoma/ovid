# R/emotion_catalogue.R  --  senseFly flight logs (.bbx, .bb3) and eMotion missions (.mis3) in the catalogue. Part of Ovid
# Core // RS. Sourced by rs_pipeline.R after read_emotion.R; not meant to be run on its own.
#
# Each log and mission is read once and kept in the source's cache (a .bb3 takes 15-20 s), keyed by its name, size and
# date, the .bb3 key in the project and the reader code, so a copy already read for another source is reused, not read
# again. Copies of one log in several folders or sources count once: a flight takes one of them, the others are listed
# in duplicates.csv (emotion_duplicates). A flight linked to a log gains the log's time span, a thinned
# track, its photo tags and, through the log's name, the mission block and waypoints eMotion planned for it. Without a .bb3
# key, .bb3 logs are listed but not read, and the flight says so in its QA flags.

EMO_TRACK_POINTS <- 300L                                       # track points kept per log for the map
# A camera time further from its decoded flight log than any time zone can explain is a clock that was never set (the Canon
# S110 NIR, a compact camera still at its factory date). Civil time zones run from UTC-12 to UTC+14 (IANA time zone
# database), so a local camera clock is at most 14 h off the log's GNSS time.
CLOCK_LOG_TOL_H <- 14

emotion_thin <- function(d, n = EMO_TRACK_POINTS) if (nrow(d) > n) d[unique(round(seq(1, nrow(d), length.out = n)))] else d

# One log: a one-row summary, its thinned track and its photo tags.
emotion_log_summary <- function(path, ring = bb3_ring_path()) {
  f <- basename(path); kind <- tolower(sub(".*\\.", "", f))
  row <- data.table(log_file = f, log_kind = kind, log_decoded = FALSE, log_note = "", log_start_utc = as.POSIXct(NA), log_end_utc = as.POSIXct(NA),
                    log_photos = 0L, log_track_points = 0L, log_lat_min = NA_real_, log_lat_max = NA_real_, log_lon_min = NA_real_, log_lon_max = NA_real_,
                    log_decrypted_pct = NA_real_)
  if (grepl("_em\\.bb3$", f, ignore.case = TRUE)) { row[, `:=`(log_kind = "link", log_note = "eMotion radio-link log")]
    return(list(log = row, track = data.table(), photos = data.table())) }
  if (kind == "bbx") {
    x <- read_bbx(path)
    tr <- if (nrow(x$track)) x$track[, .(utc, lat, lon, alt_msl = alt, alt_wgs84 = altWGS84)] else data.table()
    ph <- if (nrow(x$photos)) x$photos[, .(photo = as.integer(id), utc, lat = latitude, lon = longitude, alt_msl = altitudeMsl, alt_wgs84 = altitudeWgs84,
                                           acc_xy = accXY, acc_z = accZ, pitch, yaw, roll)] else data.table()
    row[, `:=`(log_decoded = TRUE, log_decrypted_pct = 100)]
  } else if (kind == "bb3") {
    x <- read_bb3(path, ring)
    if (!nrow(x$records) || is.na(x$meta$decrypted_pct)) { row[, log_note := x$meta$note %||% "no drone records"]
      return(list(log = row, track = data.table(), photos = data.table())) }
    tr <- if (nrow(x$track)) x$track[, .(utc, lat, lon, alt_msl, alt_wgs84)] else data.table()
    ph <- if (nrow(x$photos)) x$photos[, .(photo, utc, lat, lon, alt_msl, alt_wgs84, acc_xy, acc_z, pitch, yaw, roll)] else data.table()
    row[, `:=`(log_decoded = TRUE, log_decrypted_pct = x$meta$decrypted_pct)]
  } else { row[, log_note := "format not read"]; return(list(log = row, track = data.table(), photos = data.table())) }
  if (nrow(tr)) tr <- unique(tr[!is.na(utc) & is.finite(lat) & is.finite(lon) & (lat != 0 | lon != 0)], by = "utc")   # a record written twice
  if (nrow(ph)) ph <- unique(ph, by = c("photo", "utc"))
  if (nrow(tr)) row[, `:=`(log_start_utc = min(tr$utc), log_end_utc = max(tr$utc), log_track_points = nrow(tr),
                           log_lat_min = min(tr$lat), log_lat_max = max(tr$lat), log_lon_min = min(tr$lon), log_lon_max = max(tr$lon))]
  else if (nrow(ph)) row[, `:=`(log_start_utc = min(ph$utc, na.rm = TRUE), log_end_utc = max(ph$utc, na.rm = TRUE))]
  row[, log_photos := nrow(ph)]
  list(log = row, track = emotion_thin(tr), photos = ph)
}

# One mission: its summary, planned waypoints, mapping areas and the photo geotags it keeps, each under the log's name.
emotion_plan_summary <- function(path) {
  p <- read_emotion_plan(path)
  list(plan = p$meta[, plan_file := basename(path)], waypoints = p$waypoints, areas = p$areas, geotags = p$geotags)
}

# Every log and mission in a source, through the cache. Returns list(logs, tracks, photos, plans, waypoints, areas, geotags),
# each with the file's rel path.
read_source_emotion <- function(files, sdir, ring = bb3_ring_path()) {
  cdir <- file.path(sdir, "emotion"); dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  rk <- if (length(ring) && !is.na(ring) && file.exists(ring)) unname(tools::md5sum(ring)) else "no key"
  sh <- if (exists("STEP_HASH") && !is.null(STEP_HASH$emotion)) STEP_HASH$emotion else ""
  one <- function(f, i, what, fun) {
    rkk <- if (what == "log" && grepl("\\.bb3$", f$file[i], ignore.case = TRUE)) rk
    key <- hash_of(list(f$file[i], f$size[i], as.numeric(f$mtime[i]), rkk, sh))       # no folder: a copy in another source shares it
    name <- paste0(what, "_", hash_of(list(f$file[i], f$size[i]), 12L)); used <<- c(used, cache_file(cdir, name, key))
    r <- cache_read(cdir, name, key)
    if (is.null(r))                                            # the same file read for another source, or this one under the earlier key
      for (cf in c(Sys.glob(file.path(dirname(sdir), "*", "emotion", basename(cache_file(cdir, name, key)))),
                   cache_file(cdir, paste0(what, "_", hash_of(f$rel[i], 12L)), hash_of(list(f$rel[i], f$size[i], as.numeric(f$mtime[i]), rkk, sh)))))
        if (file.exists(cf) && !is.null(r <- tryCatch(readRDS(cf), error = function(e) NULL))) { cache_write(cdir, name, key, r); break }
    if (is.null(r)) { r <- tryCatch(fun(f$path[i]), error = function(e) { problem(f$rel[i], paste("not read:", conditionMessage(e))); NULL })
                      if (!is.null(r)) cache_write(cdir, name, key, r) }
    if (!is.null(r)) lapply(r, function(d) if (is.data.table(d) && nrow(d)) d[, rel := f$rel[i]] else d)
  }
  used <- character()
  lg <- files[format == "sensefly_log" & status == "" & size > 0]
  ms <- files[format == "emotion_plan" & status == "" & grepl("\\.mis3$", file, ignore.case = TRUE)]
  L <- lapply(seq_len(nrow(lg)), function(i) { if (exists("run_tick")) run_tick(i, nrow(lg), lg$file[i]); one(lg, i, "log", function(p) emotion_log_summary(p, ring)) })
  M <- lapply(seq_len(nrow(ms)), function(i) one(ms, i, "plan", emotion_plan_summary))
  cache_prune(cdir, used)
  st <- function(x, k) rbindlist(lapply(x, function(r) r[[k]]), fill = TRUE)
  out <- list(logs = st(L, "log"), tracks = st(L, "track"), photos = st(L, "photos"),
              plans = st(M, "plan"), waypoints = st(M, "waypoints"), areas = st(M, "areas"), geotags = st(M, "geotags"))
  if (nrow(lg) && exists("say") && nrow(n <- out$logs)) {
    wait <- sum(!n$log_decoded & grepl("key", n$log_note)); link <- sum(n$log_kind == "link" | n$log_note == "no drone records")
    say("    ", nrow(lg), " flight logs: ", sum(n$log_decoded), " read",
        if (link) paste0(", ", link, " eMotion link files skipped") else "", if (wait) paste0(", ", wait, " .bb3 waiting for the key (scan their eMotion mission with them)") else "",
        if (nrow(ms)) paste0("; ", nrow(ms), " eMotion mission(s)") else "") }
  out
}

# The project's .bb3 key, built during a scan when there is none yet and the source holds eMotion missions (.mis3) with
# .bb3 logs (build_bb3_ring, R/read_emotion.R). It is kept in the project's config folder (ring), never in the code or a
# source. Returns TRUE when the key is there.
emotion_key_auto <- function(files, ring, build = build_bb3_ring) {
  if (!length(ring) || is.na(ring)) return(FALSE)
  if (file.exists(ring)) return(TRUE)
  ms <- files[format == "emotion_plan" & status == "" & grepl("\\.mis3$", file, ignore.case = TRUE)]
  lg <- files[format == "sensefly_log" & status == "" & size > 0 & grepl("\\.bb3$", file, ignore.case = TRUE) & !grepl("_em\\.bb3$", file, ignore.case = TRUE)]
  if (!nrow(ms) || !nrow(lg)) return(FALSE)
  say("    building the .bb3 key from ", nrow(ms), " eMotion mission(s) and ", nrow(lg), " .bb3 log(s)")
  k <- tryCatch(build(ms$path, lg$path, out = ring), error = function(e) { problem("bb3 key", paste("not built:", conditionMessage(e))); NULL })
  if (isTRUE(k$closed)) { say("    .bb3 key built and kept in the project (config/", basename(ring), ")"); return(TRUE) }
  if (!is.null(k)) say("    no .bb3 key yet: the missions' photo geotags matched too few .bb3 photo records (", k$blocks, " key blocks found)", col = "yellow")
  FALSE
}

# The emotion part of every source's scan, stacked with source_id (load_sources).
# A log's identity is its name and size (log_id): the same log in several folders or sources is one log. A mission found in
# several sources is kept once (its first source), and records written twice are dropped.
emotion_stack <- function(sc) {
  ks <- c("logs", "tracks", "photos", "plans", "waypoints", "areas", "geotags")
  e <- setNames(lapply(ks, function(k) rbindlist(lapply(names(sc), function(sid) { d <- sc[[sid]]$emotion[[k]]
    if (!is.null(d) && nrow(d)) copy(d)[, source_id := sid] }), fill = TRUE)), ks)
  if (nrow(e$logs)) {
    sz <- rbindlist(lapply(names(sc), function(sid) { f <- sc[[sid]]$files; if (!is.null(f) && nrow(f)) f[, .(source_id = sid, rel, log_size = size)] }), fill = TRUE)
    if (nrow(sz)) e$logs[sz, log_size := i.log_size, on = .(source_id, rel)] else e$logs[, log_size := NA_real_]
    e$logs[, log_id := paste(log_file, log_size)]
  }
  if (nrow(e$tracks)) e$tracks <- unique(e$tracks, by = c("source_id", "rel", "utc"))
  if (nrow(e$photos)) e$photos <- unique(e$photos, by = c("source_id", "rel", "photo", "utc"))
  if (nrow(e$plans)) {
    keep <- unique(e$plans[, .(name, plan_file, source_id, rel)], by = c("name", "plan_file"))[, .(source_id, rel)]
    for (k in c("plans", "waypoints", "areas", "geotags")) if (nrow(e[[k]])) e[[k]] <- e[[k]][keep, on = .(source_id, rel), nomatch = NULL]
    if (nrow(e$geotags)) e$geotags <- unique(e$geotags, by = c("log_file", "photo", "utc"))
  }
  e
}

# Flight columns from the linked logs and the missions: log time span, track and photo counts and extent, the mission block
# and its planned waypoints. A flight with no camera time takes the log's; one with no image positions takes the track's
# extent. emo_qa says what is missing or does not agree.
emotion_flight_fields <- function(fl, ok, emo) {
  cols <- c("log_file", "log_ref", "log_id", "log_copies", "log_versions", "log_kind", "log_decoded", "log_note", "log_start_utc", "log_end_utc", "log_photos", "log_track_points",
            "log_lat_min", "log_lat_max", "log_lon_min", "log_lon_max", "log_decrypted_pct", "plan_name", "plan_block", "plan_waypoints", "plan_photos")
  fl <- copy(fl); fl[, emo_qa := ""]
  lg <- emo$logs
  if (!is.null(lg) && nrow(lg)) {
    m <- merge(ok[format == "sensefly_log", .(flight_id, source_id, rel)], lg[log_kind != "link"], by = c("source_id", "rel"))
    if (nrow(m)) {
      best <- m[order(flight_id, -log_decoded, -log_track_points, -log_photos, -log_size)][, .SD[1], by = flight_id]
      best[, log_ref := paste0(source_id, ":", rel)]
      # copies of the chosen log, and other versions of the same flight's log (same name, other size: a cut or partial copy)
      if ("log_id" %in% names(lg)) {
        best[lg[, .(log_copies = uniqueN(paste(source_id, rel))), by = log_id], log_copies := i.log_copies, on = "log_id"]
        named <- lg[log_kind != "link" & !is.na(ebee_flight_no(log_file)$no)]      # versions only for logs named by drone and flight number
        best[named[, .(log_versions = uniqueN(log_id)), by = log_file], log_versions := i.log_versions, on = "log_file"] }
      waiting <- m[log_kind == "bb3" & !log_decoded & grepl("key", log_note), unique(flight_id)]
      tr <- emo$tracks
      if (!is.null(tr) && nrow(tr)) best[tr[, .(c_lat = stats::median(lat), c_lon = stats::median(lon)), by = .(source_id, rel)], on = .(source_id, rel), `:=`(c_lat = i.c_lat, c_lon = i.c_lon)]
      fl <- merge(fl, best[, intersect(c("flight_id", cols, "c_lat", "c_lon"), names(best)), with = FALSE], by = "flight_id", all.x = TRUE)
      fl[log_versions > 1, emo_qa := paste0("flight log found in ", log_versions, " versions; the most complete is used")]
      fl[flight_id %in% waiting & !(log_decoded %in% TRUE), emo_qa := "flight log not decoded: no .bb3 key in this project yet (scan its eMotion mission with the .bb3 logs)"]
    }
  }
  for (k in setdiff(cols, names(fl))) fl[, (k) := if (k %in% c("log_start_utc", "log_end_utc")) as.POSIXct(NA) else if (k == "log_decoded") NA
                                                   else if (k %in% c("log_file", "log_ref", "log_id", "log_kind", "log_note", "plan_name", "plan_block")) NA_character_ else NA_real_]
  # the mission block a flight flew: the mapping area that holds the middle of its track (a mission names only the last
  # log flown on each block)
  wp <- emo$waypoints; gt <- emo$geotags; ar <- emo$areas
  if (!is.null(ar) && nrow(ar) && "c_lat" %in% names(fl)) {
    inside <- function(y, x, py, px) { j <- c(length(px), seq_len(length(px) - 1L))
      sum(((py > y) != (py[j] > y)) & (x < (px[j] - px) * (y - py) / (py[j] - py) + px)) %% 2 == 1 }
    polys <- split(ar, by = c("source_id", "rel", "block"))
    for (i in which(is.finite(fl$c_lat))) for (pg in polys) if (inside(fl$c_lat[i], fl$c_lon[i], pg$lat, pg$lon)) {
      pn <- if (!is.null(emo$plans) && nrow(emo$plans)) emo$plans[source_id == pg$source_id[1] & rel == pg$rel[1], name][1] else NA_character_
      set(fl, i, c("plan_block", "plan_name"), list(pg$block[1], pn))
      if (!is.null(wp) && nrow(wp)) set(fl, i, "plan_waypoints", wp[source_id == pg$source_id[1] & rel == pg$rel[1] & block == pg$block[1] & part == "block", .N])
      break }
  }
  if ("c_lat" %in% names(fl)) fl[, c("c_lat", "c_lon") := NULL]
  if (!is.null(gt) && nrow(gt)) fl[gt[, .(n = .N), by = log_file], on = "log_file", plan_photos := i.n]
  fl[, time_source := if ("time_source" %in% names(fl)) time_source else fifelse(is.na(start_utc), "", "camera")]
  # the decoded log's GNSS time wins over eMotion's KML export and over a camera clock that was never set
  off_h <- abs(as.numeric(difftime(fl$start_utc, fl$log_start_utc, units = "hours")))
  unset <- which(fl$time_source == "camera" & is.finite(off_h) & off_h > CLOCK_LOG_TOL_H)
  if (length(unset)) fl[unset, `:=`(clock_ok = FALSE, emo_qa = paste0(fifelse(nzchar(emo_qa), paste0(emo_qa, "; "), ""), "camera clock ",
                                    fifelse(off_h[unset] >= 48, paste(round(off_h[unset] / 24), "days"), paste(round(off_h[unset]), "h")), " off the flight log"))]
  fl[!is.na(log_start_utc) & (is.na(start_utc) | time_source == "flown track (KML)" | seq_len(.N) %in% unset),
     `:=`(start_utc = log_start_utc, end_utc = log_end_utc, time_source = "flight log")]
  fl[!is.finite(lat_min) & is.finite(log_lat_min), `:=`(lat_min = log_lat_min, lat_max = log_lat_max, lon_min = log_lon_min, lon_max = log_lon_max)]
  # photo tags against the flight's captures (a multispectral capture is several images); small differences are test shots
  nc <- if ("n_captures" %in% names(fl)) fl$n_captures else if ("n_images" %in% names(fl)) fl$n_images else NULL
  if (!is.null(nc)) { fl[, nc_ := nc]
    fl[log_photos > 0 & abs(log_photos - nc_) > pmax(2, 0.05 * log_photos),
       emo_qa := paste0(fifelse(nzchar(emo_qa), paste0(emo_qa, "; "), ""), "flight log has ", log_photos, " photo tags, ", nc_, " captures found")]
    fl[, nc_ := NULL] }
  fl[]
}

# Map lines: each linked log's flown track (observed), and the mission's mapping areas and planned route (planned) for the
# flights flown on that block.
emotion_geometry <- function(fl, links, emo) {
  ln <- function(lat, lon) paste(sprintf("%.6f %.6f", lat, lon), collapse = ";")
  g <- list()
  tr <- emo$tracks
  if (!is.null(tr) && nrow(tr) && "log_ref" %in% names(fl)) {             # the flight's chosen log only: copies draw no second line
    ok <- fl[!is.na(log_ref), .(flight_id, ref = log_ref)]
    t <- tr[paste0(source_id, ":", rel) %in% ok$ref, .(line = ln(lat, lon)), by = .(ref = paste0(source_id, ":", rel))]
    g <- c(g, list(merge(t, ok, by = "ref")[, .(flight_id, kind = "observed", what = "flown track (flight log)", ref, line)]))
  }
  ar <- emo$areas; wp <- emo$waypoints
  if (!is.null(wp) && nrow(wp) && "plan_block" %in% names(fl)) {
    f <- fl[!is.na(plan_block), .(flight_id, block = plan_block)]
    if (nrow(f) && !is.null(ar) && nrow(ar)) {
      a <- ar[, .(line = ln(c(lat, lat[1]), c(lon, lon[1]))), by = .(block, ref = paste0(source_id, ":", rel))]
      g <- c(g, list(merge(f, a, by = "block", allow.cartesian = TRUE)[, .(flight_id, kind = "planned", what = paste("mission area:", block), ref, line)])) }
    r <- wp[part == "block", .(line = ln(lat, lon)), by = .(block, ref = paste0(source_id, ":", rel))]
    if (nrow(f)) g <- c(g, list(merge(f, r, by = "block", allow.cartesian = TRUE)[, .(flight_id, kind = "planned", what = paste("mission route (eMotion):", block), ref, line)]))
  }
  rbindlist(g, fill = TRUE)
}

# duplicates.csv rows for the logs (same columns as the photos): every copy of a log other than the one its flight uses (or,
# for a log no flight uses, its first copy), and other versions of a flight's log.
emotion_duplicates <- function(fl, emo) {
  out <- data.table(kind = character(), key = character(), kept_path = character(), dup_path = character(), why = character())
  lg <- emo$logs
  if (is.null(lg) || !nrow(lg) || !"log_id" %in% names(lg)) return(out)
  lg <- lg[log_kind != "link" & !is.na(ebee_flight_no(log_file)$no)][, ref := paste0(source_id, ":", rel)]   # logs named by drone and flight
  used <- if ("log_ref" %in% names(fl)) fl[!is.na(log_ref), unique(log_ref)] else character()
  lg[, kept := { u <- ref[ref %in% used]; if (length(u)) u[1] else sort(ref)[1] }, by = log_id]
  same <- lg[ref != kept, .(kind = "flight log", key = log_id, kept_path = kept, dup_path = ref, why = "same name and size")]
  v <- lg[ref == kept][, if (uniqueN(log_id) > 1L) { k <- ref[ref %in% used][1]; if (is.na(k)) k <- ref[which.max(log_size)]
         .(kind = "flight log", key = log_file, kept_path = k, dup_path = setdiff(ref, k), why = "same flight, other version") }, by = log_file]
  rbind(out, same, if (nrow(v)) v[, .(kind, key, kept_path, dup_path, why)], fill = TRUE)
}

# Every flight log with its time, photo tags and the flight it is linked to (catalogue/logs.csv), so the Flight page can offer
# a flight the logs that no flight has yet.
LOG_TABLE_COLS <- c("source_id", "rel", "log_file", "log_kind", "log_decoded", "start_utc", "end_utc", "photos", "serial", "flight_no", "flight_id")
log_table <- function(lg, links) {
  if (is.null(lg) || !nrow(lg)) return(ensure_cols(data.table(), LOG_TABLE_COLS)[0])
  d <- lg[log_kind != "link", .(source_id, rel, log_file, log_kind, log_decoded, start_utc = fmt_ts(log_start_utc), end_utc = fmt_ts(log_end_utc), photos = log_photos)]
  e <- ebee_flight_no(d$log_file); d[, `:=`(serial = e$serial, flight_no = e$no)]
  ok <- if (!is.null(links) && nrow(links)) links[state %in% c("auto", "probable", "confirmed") & format == "sensefly_log", .(flight_id = flight_id[1]), by = .(source_id, rel)]
  d <- if (!is.null(ok) && nrow(ok)) merge(d, ok, by = c("source_id", "rel"), all.x = TRUE) else d[, flight_id := NA_character_]
  d[, LOG_TABLE_COLS, with = FALSE][order(serial, flight_no, rel)]
}
