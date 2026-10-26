# RS/pages/fct_footprints.R  --  The Map page's footprints: each flight's ground (catalogue/footprints.csv, made by
# R/footprints.R), the Optical / LiDAR / Both pick, one footprint per year with its coverage card, and the Report (PDF)
# of the view. Plain functions, sourced by app.R; the server parts are in app.R.

FP_KIND_LABEL <- c(optical = "Optical", lidar = "LiDAR")
fp_kind_cols <- function() c(optical = PAL$accent, lidar = PAL$blue)
FP_COLS <- c("fp_kind", "fp_area_m2", "fp_height_m", "fp_height_from", "fp_width_m", "fp_gsd_cm", "fp_images", "fp_cell_m", "fp_note")

# A flight's kind: its footprint's, else LiDAR when it has LiDAR raw files (a catalogue made before footprints).
flight_kind <- function(f) {
  k <- if ("fp_kind" %in% names(f)) as.character(f$fp_kind) else rep(NA_character_, nrow(f))
  lr <- if ("has_lidar_raw" %in% names(f)) f$has_lidar_raw %in% TRUE else rep(FALSE, nrow(f))
  fifelse(k %in% names(FP_KIND_LABEL), k, fifelse(lr, "lidar", "optical"))
}
fp_kinds <- function(pick) if (identical(pick, "optical")) "optical" else if (identical(pick, "lidar")) "lidar" else c("optical", "lidar")

# The map's footprints: row id -> parts -> rings (the outer edge first). A joined flight (pages/fct_edits.R fold_joined)
# draws the footprints of all its sorties.
fp_rings_json <- function(fp, f) {
  none <- structure(list(), names = character())
  if (is.null(fp) || !nrow(fp) || !nrow(f)) return(none)
  ids <- if ("part_ids" %in% names(f)) strsplit(as.character(f$part_ids), " ", fixed = TRUE) else as.list(f$flight_id)
  m <- data.table(row = rep(f$flight_id, lengths(ids)), flight_id = unlist(ids))
  x <- merge(fp, m, by = "flight_id")
  if (!nrow(x)) return(none)
  setorder(x, row, flight_id, part, ring)
  lapply(split(x, by = "row", keep.by = FALSE), function(d) unname(lapply(split(d$line, paste(d$flight_id, d$part), drop = TRUE), as.list)))
}

# One footprint per year and kind for the flights in view (photo sets left out): list of year, kind, ids (the flights of
# that year and kind), with (those with a footprint) and u (footprint_union). memo: an environment, so the same set is
# joined once.
year_footprints <- function(f, fp, kinds, memo) {
  if (!nrow(f)) return(list())
  f <- f[kind != "photo set" & !year %in% c("undated", "")]
  out <- list()
  for (y in sort(unique(f$year))) for (k in kinds) {
    ids <- f[year == y & fpk == k, flight_id]; if (!length(ids)) next
    with <- intersect(ids, fp$flight_id[fp$kind == k])
    key <- paste(k, paste(sort(with), collapse = ","))
    if (is.null(memo[[key]])) memo[[key]] <- footprint_union(fp[flight_id %in% with & kind == k])
    out[[length(out) + 1L]] <- list(year = y, kind = k, ids = ids, with = with, u = memo[[key]])
  }
  out
}

# For the map: each year outline as parts of ring lines, with its tooltip numbers and its kind's colour.
year_layers <- function(yf) lapply(yf, function(x) { r <- x$u$rings; more <- sum(x$u$by_n[n > 1, m2])
  list(year = x$year, label = FP_KIND_LABEL[[x$kind]], col = fp_kind_cols()[[x$kind]], n = length(x$with), n_parts = x$u$parts,
       area_km2 = round(x$u$area_m2 / 1e6, 3), more_km2 = round(more / 1e6, 3),
       parts = if (nrow(r)) unname(lapply(split(r$line, r$part), as.list)) else list()) })

# ---- the numbers of a year's footprint -------------------------------------------------------------------------------------
# section, label, value: one list for the coverage card and the report. f: the flights of the year and kind (all, with
# or without a footprint); u: footprint_union of those with one (with: their ids).
km2 <- function(m2) sprintf("%.3f km\u00b2", m2 / 1e6)
year_facts <- function(f, u, kind, with = f$flight_id) {
  v <- function(k) if (k %in% names(f)) suppressWarnings(as.numeric(f[[k]])) else rep(NA_real_, nrow(f))
  mr <- function(x, unit = "", d = 1) { x <- x[is.finite(x)]; if (!length(x)) return(NA_character_)
    r <- function(z) formatC(z, format = "f", digits = d, big.mark = ",")
    if (length(x) == 1L || min(x) == max(x)) paste0(r(stats::median(x)), unit) else sprintf("%s%s (%s to %s)", r(stats::median(x)), unit, r(min(x)), r(max(x))) }
  cnt <- function(x) { x <- x[!is.na(x) & nzchar(x)]; if (!length(x)) return(NA_character_); t <- sort(table(x), decreasing = TRUE); paste(sprintf("%s %d", names(t), as.integer(t)), collapse = "; ") }
  more <- sum(u$by_n[n > 1, m2]); lidar <- kind == "lidar"
  gsd <- fifelse(is.finite(v("fp_gsd_cm")), v("fp_gsd_cm"), v("gsd_cm_median"))
  sun <- c(v("sun_elev_min_deg"), v("sun_elev_max_deg")); sun <- sun[is.finite(sun)]
  dates <- sort(unique(f$date[nzchar(f$date) & f$date != "undated"]))
  mins <- sum(v("duration_min"), na.rm = TRUE)
  rbindlist(list(
    list("Coverage", "Flights joined", if (length(with) == nrow(f)) as.character(nrow(f)) else sprintf("%d of %d (the others have no footprint)", length(with), nrow(f))),
    list("Coverage", "Area covered", km2(u$area_m2)),
    list("Coverage", "Parts \u00b7 holes", sprintf("%d \u00b7 %d", u$parts, u$holes)),
    list("Coverage", "Sum of flight footprints", km2(u$sum_m2)),
    list("Coverage", "Flown more than once", if (u$area_m2 > 0) sprintf("%s (%.0f %%)", km2(more), 100 * more / sum(u$by_n$m2)) else NA_character_),
    list("Acquisition", if (lidar) "Colour camera GSD, median of flights (range)" else "Pixel size (GSD), median of flights (range)", mr(gsd, " cm", 2)),
    if (lidar) list("Acquisition", "LiDAR swath width", mr(v("fp_width_m"), " m")),
    list("Acquisition", "Height above take-off ground", mr(v("fp_height_m"), " m")),
    list("Acquisition", "Height from", cnt(f$fp_height_from %||% character())),
    list("Acquisition", "Forward overlap, median of flights", mr(v("fwd_overlap_pct_median"), " %", 0)),
    if (!lidar) list("Acquisition", "Side overlap, median of flights", mr(v("side_overlap_pct_median"), " %", 0)),
    list("Acquisition", "Ground speed", mr(v("speed_ms_median"), " m/s")),
    list("Acquisition", "Sun elevation", if (length(sun)) sprintf("%.1f\u00b0 to %.1f\u00b0", min(sun), max(sun)) else NA_character_),
    list("Acquisition", "RTK fixed share, median of flights", mr(v("rtk_fixed_pct"), " %", 0)),
    list("Flights", "Dates", if (length(dates)) paste(unique(range(dates)), collapse = " to ") else NA_character_),
    list("Flights", "Days flown", as.character(length(dates))),
    list("Flights", "Flight time", if (mins > 0) sprintf("%d h %02d min", as.integer(mins %/% 60), as.integer(round(mins %% 60))) else NA_character_),
    list("Flights", "Images \u00b7 size", sprintf("%s \u00b7 %s GB", format(sum(v("n_images"), na.rm = TRUE), big.mark = ","), format(round(sum(v("size_gb"), na.rm = TRUE), 1), nsmall = 1))),
    list("Flights", "Aircraft and sensors", cnt(trimws(paste(f$aircraft, f$sensor, sep = " \u00b7 ")))),
    list("Flights", "Positioning", cnt(f$positioning_status)),
    list("Flights", "Sites", { s <- sort(unique(f$site[nzchar(f$site)])); if (length(s)) paste(s, collapse = ", ") else "none set" })
  ))[, setNames(.SD, c("section", "label", "value"))][!is.na(value)]
}
# Area under 1, 2, 3 to 5, 6 to 10 and 11 or more flights (m2).
year_by_n <- function(u) { b <- u$by_n; s <- function(a, z) sum(b[n >= a & n <= z, m2])
  data.table(under = c("1 flight", "2 flights", "3 to 5", "6 to 10", "11 or more"), m2 = c(s(1, 1), s(2, 2), s(3, 5), s(6, 10), s(11, Inf))) }

# The coverage card beside the map (one block per year and kind).
year_card <- function(yf, f) {
  if (!length(yf)) return(p(class = "muted", "No flight in view has a footprint. Rebuild the catalogue (Sources) to work them out."))
  tagList(lapply(yf, function(x) { g <- f[flight_id %in% x$ids]; fa <- year_facts(g, x$u, x$kind, x$with); b <- year_by_n(x$u); mx <- max(b$m2, 1)
    tagList(h3(class = "mt-2", sprintf("%s \u00b7 %s", x$year, FP_KIND_LABEL[[x$kind]]), span(class = "rs-fp-key", style = sprintf("border-color:%s", fp_kind_cols()[[x$kind]]))),
      lapply(split(fa, factor(fa$section, unique(fa$section))), function(s) tagList(div(class = "rs-fp-sec", s$section[1]), lapply(seq_len(nrow(s)), function(i) fact(s$label[i], s$value[i])),
        if (s$section[1] == "Coverage") div(class = "rs-fp-bars", div(class = "muted", "Area by number of flights over it (km\u00b2)"),
          lapply(seq_len(nrow(b)), function(i) div(class = "rs-fp-bar", span(b$under[i]), div(class = "tr", div(style = sprintf("width:%.1f%%;background:%s", 100 * b$m2[i] / mx, fp_kind_cols()[[x$kind]]))),
                                                   span(sprintf("%.3f", b$m2[i] / 1e6))))))))
  }), p(class = "muted mt-2", sprintf("Footprints from each photo (sensor size x height / focal length) or LiDAR strip (2 x height x tan(FOV / 2)); joined on a %s m grid. Photo sets are left out. Method and sources: Reference.", format(FOOTPRINT_CELL_M))))
}

# ---- the report ----------------------------------------------------------------------------------------------------------
# One HTML page (A4, printed to PDF by Chrome through chromote): the view's filters, the map as drawn (PNG from the browser
# with its legend, scale bar, north arrow and corners), each year and kind's numbers, the flights, the method and sources.
FP_REFERENCES <- c(
  "Baltsavias, E. P. (1999). Airborne laser scanning: basic relations and formulas. ISPRS Journal of Photogrammetry and Remote Sensing 54, 199-214.",
  "Bowring, B. R. (1976). Transformation from spatial to geographical coordinates. Survey Review 23(181), 323-327.",
  "DJI (2024). Zenmuse L2 User Manual v1.2 (field of view 70 deg horizontal).",
  "Douglas, D. H., Peucker, T. K. (1973). Algorithms for the reduction of the number of points required to represent a digitized line or its caricature. Cartographica 10(2), 112-122.",
  "McGlone, J. C. (ed.) (2013). Manual of Photogrammetry, 6th ed. ASPRS.",
  "Wolf, P. R., Dewitt, B. A., Wilkinson, B. E. (2014). Elements of Photogrammetry with Applications in GIS, 4th ed. McGraw-Hill.")
FP_METHOD <- c(
  "Photo footprint: across the track = sensor width x height / focal length, along = that x image height / image width, turned to the flight direction (pinhole camera; McGlone 2013; Wolf et al. 2014). GSD = sensor width x height / (focal length x image width).",
  "LiDAR strip: swath width = 2 x height x tan(field of view / 2) along the flown lines (Baltsavias 1999); Zenmuse L2 70 deg (DJI 2024).",
  "Height above the take-off ground: DJI's height above take-off; eBee: the photo's GNSS height in the flight log less the log's take-off height (both RTK), or less the GNSS base's height (RINEX header less antenna height; Bowring 1976). Over sloping ground the footprint is approximate.",
  "Footprints are marked on a grid in metres around the centre of what is joined, the covered cells traced once into outlines (holes kept), simplified by one cell (Douglas and Peucker 1973); parts and holes under 500 m\u00b2 are dropped. Areas are the outlines' areas.")

report_html <- function(view, fig, caption, blocks, flights, made) {
  e <- htmltools::htmlEscape
  tbl <- function(d) paste0("<table>", paste0("<tr><td>", e(d$label), "</td><td class=\"n\">", e(d$value), "</td></tr>", collapse = ""), "</table>")
  bl <- vapply(blocks, function(x) { fa <- x$facts; b <- year_by_n(x$u)
    paste0("<h2>", e(sprintf("%s \u00b7 %s", x$year, FP_KIND_LABEL[[x$kind]])), "</h2><div class=\"two\"><div><h3>Coverage</h3>", tbl(fa[section == "Coverage"]),
           tbl(data.table(label = paste("under", b$under), value = sprintf("%.3f km\u00b2", b$m2 / 1e6))),
           "</div><div><h3>Acquisition</h3>", tbl(fa[section == "Acquisition"]), "<h3>Flights</h3>", tbl(fa[section == "Flights"]), "</div></div>") }, "")
  ft <- if (nrow(flights)) paste0("<h2>Flights (", nrow(flights), ")</h2><table class=\"fl\"><tr>", paste0("<th>", e(names(flights)), "</th>", collapse = ""), "</tr>",
                                  paste0("<tr>", apply(flights, 1, function(r) paste0("<td>", e(ifelse(is.na(r), "", r)), "</td>", collapse = "")), "</tr>", collapse = ""), "</table>") else ""
  paste0('<!doctype html><html><head><meta charset="utf-8"><title>ovid RS coverage report</title><style>
@page { size: A4; margin: 16mm 14mm 18mm 14mm }
body { font: 9.5pt/1.4 "IBM Plex Sans", "Helvetica Neue", Arial, sans-serif; color: #1d1f23; margin: 0 }
h1 { font-size: 15pt; margin: 0 } h2 { font-size: 11pt; margin: 14pt 0 4pt; border-bottom: 1px solid #bbb; padding-bottom: 2pt; break-after: avoid }
h3 { font-size: 9.5pt; margin: 6pt 0 2pt } .sub { color: #555; font: 8pt "IBM Plex Mono", Menlo, monospace; margin: 2pt 0 8pt }
img { width: 100%; border: 1px solid #888 } .cap { color: #333; margin: 3pt 0 0 } .two { display: grid; grid-template-columns: 1fr 1fr; gap: 0 14pt }
table { border-collapse: collapse; width: 100%; font-variant-numeric: tabular-nums } td, th { border-bottom: 1px solid #e2e2e2; padding: 1.5pt 4pt; text-align: left; vertical-align: top }
td.n { text-align: right } th { background: #f1f1f1; font-weight: 600 } table.fl { font-size: 7.5pt } table.fl tr { break-inside: avoid } table.fl td:first-child { white-space: nowrap } table:not(.fl) { break-inside: avoid } h3 { break-after: avoid } ol { padding-left: 14pt; margin: 0 } li { margin-bottom: 2pt }
</style></head><body>',
    "<h1>", e(view$title), "</h1><div class=\"sub\">", e(made), "</div>",
    "<div><b>View:</b> ", e(view$filters), "</div>",
    "<h2>Figure 1</h2><img src=\"", fig, "\" alt=\"The map as shown\"><p class=\"cap\">Figure 1. ", e(caption), "</p>",
    paste(bl, collapse = ""), ft,
    "<h2>Method</h2><ol>", paste0("<li>", e(FP_METHOD), "</li>", collapse = ""), "</ol>",
    "<h2>References</h2><ol>", paste0("<li>", e(FP_REFERENCES), "</li>", collapse = ""), "</ol></body></html>")
}

# The HTML to PDF through Chrome (chromote, as the guides are built); NULL when Chrome cannot be started (the HTML is kept).
report_pdf <- function(html_file, footer) {
  if (!requireNamespace("chromote", quietly = TRUE)) return(NULL)
  tryCatch({
    b <- chromote::ChromoteSession$new(); on.exit(b$close(), add = TRUE)
    loaded <- b$Page$loadEventFired(wait_ = FALSE); b$Page$navigate(paste0("file://", normalizePath(html_file))); b$wait_for(loaded)
    p <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE, displayHeaderFooter = TRUE, headerTemplate = "<span></span>",
                           footerTemplate = sprintf('<div style="font-size:7pt;width:100%%;padding:0 14mm;color:#666;display:flex;justify-content:space-between"><span>%s</span><span>page <span class="pageNumber"></span> of <span class="totalPages"></span></span></div>', htmltools::htmlEscape(footer)),
                           timeout_ = 120)
    out <- sub("\\.html$", ".pdf", html_file); writeBin(jsonlite::base64_dec(p$data), out); out
  }, error = function(e) { problem("coverage report", paste("PDF not made:", conditionMessage(e))); NULL })
}
