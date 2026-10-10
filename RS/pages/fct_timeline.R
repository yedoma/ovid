# RS/pages/fct_timeline.R  --  The timeline of the archive as plain functions (no Shiny; tests call them): flights by site
# and year, the summary under it, the HTML of the full timeline and of the strip under the map, and setting a flight's site
# in the flights table. Sourced by app.R. A site is only ever what a person typed or the folder named; nothing here guesses one.

NO_SITE <- "no site"

# One row per flight with a year: flight_id, site ("no site" when blank), year (integer), date, sensor, n_images, name.
timeline_flights <- function(fl) {
  if (!nrow(fl)) return(data.table(flight_id = character(), site = character(), year = integer(), date = character(), sensor = character(), n_images = numeric(), name = character()))
  s <- trimws(as.character(fl$site)); s[is.na(s) | !nzchar(s)] <- NO_SITE
  y <- suppressWarnings(as.integer(fl$year)); k <- is.na(y); y[k] <- suppressWarnings(as.integer(substr(fl$date[k], 1, 4)))
  se <- trimws(as.character(fl$sensor)); se[is.na(se) | !nzchar(se)] <- "unknown sensor"
  d <- data.table(flight_id = fl$flight_id, site = s, year = y, date = as.character(fl$date), sensor = se,
                  n_images = suppressWarnings(as.numeric(fl$n_images)), name = as.character(fl$name))
  d[!is.na(year)][order(site == NO_SITE, site, date)]
}

# Every year from the first flight to the last: years with no flight stay, since the gaps are the point.
timeline_years <- function(d) if (!nrow(d)) integer() else seq(min(d$year), max(d$year))

# The cards: sites, flights, sites flown in two or more years, the longest time between two flight years at one site, flights with no site.
timeline_summary <- function(d) {
  s <- d[site != NO_SITE]; ys <- s[, .(y = sort(unique(year))), by = site]
  g <- ys[, if (.N > 1) { i <- which.max(diff(y)); .(gap = diff(y)[i], from = y[i], to = y[i + 1]) }, by = site]
  g <- if (nrow(g)) g[order(-gap, site)][1] else data.table(site = "", gap = NA_integer_, from = NA_integer_, to = NA_integer_)
  list(sites = uniqueN(s$site), flights = nrow(d), revisited = sum(ys[, .N, by = site]$N > 1),
       gap = g$gap, gap_site = g$site, gap_from = g$from, gap_to = g$to, no_site = sum(d$site == NO_SITE))
}

# One colour per sensor, most flown first.
sensor_colours <- function(sensors, pal = PAL) {
  cols <- unname(c(pal$blue, pal$accent, pal$green, pal$red, pal$purple, pal$teal, pal$gold, pal$fg4))
  u <- names(sort(table(sensors), decreasing = TRUE)); setNames(rep_len(cols, length(u)), u)
}

tl_square <- function(r, col, input_id, size, picked = FALSE)
  sprintf("<span class='tl-sq%s' data-id='%s' title='%s' style='background:%s;width:%dpx;height:%dpx' onclick=\"Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\"></span>",
          if (picked) " on" else "", htmltools::htmlEscape(r$flight_id, TRUE),
          htmltools::htmlEscape(sprintf("%s\n%s\n%s\n%s images", r$name, r$date, r$sensor, ifelse(is.na(r$n_images), "?", num(r$n_images))), TRUE), col, size, size, input_id)

# The full timeline: rows are sites ("no site" last), columns the years, a square per flight (at most `cap` per cell, then "+n").
# row_id and flight_id: the Shiny inputs a click on a site name or a square sets.
timeline_html <- function(d, row_id, flight_id, row = NULL, flight = NULL, cap = 40L, pal = PAL) {
  if (!nrow(d)) return("<p class='muted'>No flights with a date yet.</p>")
  yrs <- timeline_years(d); col <- sensor_colours(d$sensor, pal); sites <- unique(d$site)
  head <- paste0("<tr><th></th>", paste0("<th>", yrs, "</th>", collapse = ""), "</tr>")
  body <- vapply(sites, function(s) { x <- d[site == s]
    cells <- vapply(yrs, function(y) { z <- x[year == y]; if (!nrow(z)) return("<td class='tl-empty'></td>")
      n <- min(nrow(z), cap); sq <- vapply(seq_len(n), function(i) tl_square(z[i], col[[z$sensor[i]]], flight_id, 12, identical(z$flight_id[i], flight)), "")
      paste0("<td>", paste(sq, collapse = ""), if (nrow(z) > cap) sprintf("<span class='muted'> +%d</span>", nrow(z) - cap) else "", "</td>") }, "")
    sprintf("<tr class='%s'><th class='tl-site' data-id='%s' onclick=\"Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">%s <span class='muted'>%d</span></th>%s</tr>",
            if (identical(s, row)) "tl-row on" else "tl-row", htmltools::htmlEscape(s, TRUE), row_id, htmltools::htmlEscape(s), nrow(x), paste(cells, collapse = "")) }, "")
  key <- paste0("<div class='tl-key'>", paste(sprintf("<span><span class='tl-sq' style='background:%s;width:10px;height:10px'></span> %s</span>", col, htmltools::htmlEscape(names(col))), collapse = ""), "</div>")
  paste0(key, "<div class='tl-wrap'><table class='tl'>", head, paste(body, collapse = ""), "</table></div>")
}

# The strip under the map: a column per year with a square per flight (at most `cap`); clicking a year sets year_id.
timeline_strip_html <- function(d, year_id, picked = character(), cap = 60L, pal = PAL) {
  if (!nrow(d)) return("")
  col <- sensor_colours(d$sensor, pal)
  cols <- vapply(timeline_years(d), function(y) { z <- d[year == y]; n <- min(nrow(z), cap)
    sq <- if (n) paste(vapply(seq_len(n), function(i) sprintf("<span class='tl-sq' style='background:%s;width:7px;height:7px'></span>", col[[z$sensor[i]]]), ""), collapse = "") else ""
    sprintf("<div class='tl-yr%s' data-y='%d' title='%d flights in %d' onclick=\"Shiny.setInputValue('%s', this.dataset.y, {priority: 'event'})\"><div class='tl-sqs'>%s%s</div><div class='tl-lab'>%d</div></div>",
            if (as.character(y) %in% picked) " on" else "", y, nrow(z), y, year_id, sq, if (nrow(z) > cap) sprintf("<span class='muted'>+%d</span>", nrow(z) - cap) else "", y) }, "")
  paste0("<div class='tl-strip'>", paste(cols, collapse = ""), "</div>")
}

# The flights table (config/flights.csv) with `site` set for the flights `ids` (rows added for flights not in it yet).
flights_set_site <- function(cf, ids, site, cols = FLIGHT_COLS) {
  s_new <- trimws(site %||% ""); if (!nzchar(s_new)) stop("Type a site name.")
  cf <- ensure_cols(copy(cf), cols); new <- setdiff(ids, cf$flight_id)
  if (length(new)) cf <- rbind(cf, ensure_cols(data.table(flight_id = new), cols)[, ..cols], fill = TRUE)
  cf[flight_id %in% ids, site := s_new]; cf
}
