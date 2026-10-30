# RS/pages/fct_needs.R  --  The Workbench's Needs you queue as plain functions (no Shiny; tests call them): every flight's
# open problems from the catalogue and its checks (pages/fct_checks.R), worst first, each saying what is wrong, why it
# matters and what to do. Decisions are kept on the core review queue's file (config/review_decisions.csv; core/workbench.R).

NEEDS_SEVERITY <- c(bad = 3L, look = 2L, note = 1L)
# per kind: why it matters, what to do, and the page (go) and tab on it (tab) that fix it
NEEDS_KINDS <- data.table(
  kind = c("images", "clock", "positions", "accuracy", "log", "from_log", "site", "log_key", "qa"),
  why  = c("A flight with no images read cannot be catalogued or processed.",
           "Without a checked clock, images cannot be matched to GNSS logs or base data, and times may be off by the UTC offset.",
           "Images without a position cannot be placed on the map or processed without ground control.",
           "Positions not corrected by RTK or PPK (standalone GNSS, or not known) can be metres off: fine for an archive, not for mapping or for comparing years.",
           "A flight log or mission file shows how the flight was flown and can place images the tags do not.",
           "The positions come from the flight log, not from the images; a processing tool reading only the images will not see them.",
           "A flight with no site cannot be found by site or compared with other years of the same place.",
           "An encrypted .bb3 log is read only with the project's .bb3 key; until then the flight has no log time, track or photo tags.",
           "Flagged when the catalogue was built."),
  todo = c("Check the source folder and rescan it.",
           "Check the camera time against the log, or give the camera's UTC offset in the time zones table (Set-up tables).",
           "Link the geotag log or MRK file, or accept the flight as archive only.", "Process it with a base (PPK), or accept it as archive only.",
           "Link the log or mission file if there is one.", "Export the log positions with the images when you process them.",
           "Set its site on the Flight page (or Set site on the Timeline).",
           "Scan a folder that holds the eMotion mission (.mis3) of these flights with their .bb3 logs: the key is built from them during the scan and kept in the project's config folder.",
           "Look at the flight and decide."),
  go   = c("Sources", "Setup", "Flight", "wb-positioning", "Flight", "Flight", "Flight", "Sources", "Flight"),
  tab  = c("", "timezones", "", "", "", "", "", "", ""))
NEEDS_NO_KEY <- "flight log not decoded"                      # the QA note a .bb3 log without the project's key leaves (R/emotion_catalogue.R)

# One row per open item: id (stable, the review queue's), flight_id, flight, date, kind, severity (bad, look, note), what,
# why, todo, go, tab, images. fl: flights; ck: flight_checks(fl).
rs_needs <- function(fl, ck) {
  empty <- data.table(id = character(), flight_id = character(), flight = character(), date = character(), kind = character(), severity = character(),
                      what = character(), why = character(), todo = character(), go = character(), tab = character(), images = numeric())
  if (!nrow(fl)) return(empty)
  k <- ck[match(fl$flight_id, ck$flight_id)]; n <- suppressWarnings(as.numeric(fl$n_images)); n[is.na(n)] <- 0
  out <- list(); add <- function(i, kind, sev, what) if (length(i)) out[[length(out) + 1L]] <<- data.table(row = i, kind = kind, severity = sev, what = what)
  for (c0 in c("images", "clock", "positions", "accuracy", "log")) { v <- k[[paste0("chk_", c0)]]; i <- which(v %in% c("look", "bad"))
    add(i, c0, v[i], switch(c0, images = "No images read", clock = "Clock not checked",
                            positions = sprintf("%s of %s images placed", num_or(k$n_placed[i]), num_or(n[i])),
                            accuracy = paste("Positions:", fl$positioning_status[i]), log = "No flight log or mission file")) }
  i <- which(k$placed_from == "flight log"); add(i, "from_log", rep("note", length(i)), sprintf("%s of %s positions from the flight log", num_or(k$n_placed[i]), num_or(n[i])))
  s <- as.character(fl$site); i <- which(is.na(s) | !nzchar(trimws(s))); add(i, "site", rep("look", length(i)), rep("No site", length(i)))
  q <- as.character(fl$qa_flags); q[is.na(q)] <- ""
  for (r in which(nzchar(q))) { parts <- trimws(strsplit(q[r], ";", fixed = TRUE)[[1]]); parts <- parts[nzchar(parts)]
    nk <- startsWith(parts, NEEDS_NO_KEY)
    if (any(nk)) add(r, "log_key", "look", "Flight log not decoded: this project has no .bb3 key yet")
    parts <- parts[!nk]; add(rep(r, length(parts)), "qa", rep("look", length(parts)), paste("QA:", parts)) }
  if (!length(out)) return(empty)
  x <- rbindlist(out); f <- fl[x$row]
  x[, `:=`(flight_id = f$flight_id, flight = as.character(f$name), date = as.character(f$date), images = n[row])]
  x <- merge(x, NEEDS_KINDS, by = "kind", sort = FALSE)
  x[, id := vapply(paste(flight_id, kind, what, sep = "|"), function(z) substr(hash_of(z), 1, 16), "", USE.NAMES = FALSE)]   # core cache.R
  x[order(-NEEDS_SEVERITY[severity], -images, date), names(empty), with = FALSE]
}

# The checks with the ones accepted as is in Needs you counted as ok (and said so in chk_why), so the summary cards and the
# Map's Checks column agree with the queue. ck: flight_checks(fl); decisions: the review queue's file as read (core
# review_read(): id, decision, note, author, at).
checks_decided <- function(fl, ck, decisions) {
  if (is.null(decisions) || !nrow(decisions) || !nrow(ck)) return(ck)
  x <- rs_needs(fl, ck); ok <- decisions$id[!is.na(decisions$decision) & nzchar(decisions$decision)]
  x <- x[kind %in% tolower(CHECKS) & id %in% ok]; if (!nrow(x)) return(ck)
  ck <- copy(ck)
  for (i in seq_len(nrow(x))) set(ck, match(x$flight_id[i], ck$flight_id), paste0("chk_", x$kind[i]), "ok")
  ck[x[, .(w = paste0(" Accepted as is: ", paste(kind, collapse = ", "), ".")), by = flight_id], on = "flight_id", chk_why := paste0(chk_why, i.w)][]
}

# Flights missing each piece of metadata (the Missing metadata chart on Needs you): what, n. fl: flights.
rs_missing_counts <- function(fl) {
  data.table(what = c("no site", "no project", "no georeferenced images", "camera clock unusable", "eBee flight without a log", "no positioning evidence"),
             n = c(sum(!nzchar(fl$site)), sum(!nzchar(fl$project)), sum(fl$n_georef == 0, na.rm = TRUE), sum(!fl$clock_ok, na.rm = TRUE),
                   sum(fl$family == "ebee" & !fl$has_log, na.rm = TRUE), sum(fl$positioning_status == "unknown")))
}
