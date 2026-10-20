# R/maps.R  --  The Map page: transects as lines from their start to their end point, sites as points, on OpenStreetMap
# tiles (plotly, no access token). Sourced by app.R only. Coordinates are WGS84 latitude / longitude in decimal degrees.

num_or_na <- function(x) suppressWarnings(as.numeric(x))

# Length (m) and bearing (degrees from north) between two points on the sphere.
geo_line <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180; R <- 6371000
  dlat <- (lat2 - lat1) * r; dlon <- (lon2 - lon1) * r
  a <- sin(dlat / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin(dlon / 2)^2
  brg <- (atan2(sin(dlon) * cos(lat2 * r), cos(lat1 * r) * sin(lat2 * r) - sin(lat1 * r) * cos(lat2 * r) * cos(dlon)) / r + 360) %% 360
  list(length_m = 2 * R * asin(sqrt(a)), bearing = brg)
}

# Accepts "45.42, -75.70", "45.42 -75.70" or "45°25'N 75°42'W" style text; returns c(lat, lon) or NULL.
parse_latlon <- function(txt) {
  if (is.null(txt) || !nzchar(trim(txt))) return(NULL)
  t <- toupper(trim(txt))
  dms <- regmatches(t, gregexpr("([0-9.]+)\\s*[°D ]\\s*([0-9.]+)?\\s*['M ]?\\s*([0-9.]+)?\\s*[\"S]?\\s*([NSEW])", t))[[1]]
  if (length(dms) == 2) {
    v <- vapply(dms, function(d) { p <- as.numeric(regmatches(d, gregexpr("[0-9.]+", d))[[1]]); h <- sub(".*([NSEW])$", "\\1", d)
      x <- p[1] + (if (length(p) > 1) p[2] / 60 else 0) + (if (length(p) > 2) p[3] / 3600 else 0); if (h %in% c("S", "W")) -x else x }, 0)
    return(unname(v))
  }
  v <- num_or_na(strsplit(t, "[,; \t]+")[[1]]); v <- v[!is.na(v)]
  if (length(v) == 2 && abs(v[1]) <= 90 && abs(v[2]) <= 180) v else NULL
}

# Basemaps: satellite imagery (Esri World Imagery, attribution shown on the map) or OpenStreetMap streets.
BASEMAPS <- list(
  satellite = list(style = "white-bg", layers = list(list(below = "traces", sourcetype = "raster", sourceattribution = "Imagery: Esri, Maxar, Earthstar Geographics",
                   source = list("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")))),
  streets = list(style = "open-street-map", layers = list()))
# Survey-year line colours from the scheme in use (PAL, R/config.R); kanagawa's springBlue, which has no palette key, is blue mixed with teal.
YEAR_COLS <- c(PAL$gold, PAL$blue, PAL$red, PAL$green, PAL$accent, mix_col(PAL$blue, PAL$teal, 0.5), PAL$orange, PAL$fg, PAL$purple, PAL$teal)
MAP_LABEL_COL <- "#ffffff"          # text on the map (site names, GPR markers) stays white on satellite imagery (M, 2026-10-20); PAL$fg would follow the scheme
MAP_LEGEND_BG <- col_rgba(PAL$bg1, 0.85)   # legend ground: bg1 at 85 % (core/theme.R)

# Where the map opens: on the selected transect, else on all transects, else on the project's sites, else the whole world
# (no region is assumed). Returns list(center = list(lat, lon), zoom) for the mapbox layout.
map_view <- function(lay, sites = NULL, selected = "") {
  pick <- nzchar(selected) && selected %in% lay$transect_id
  pts <- if (pick) lay[transect_id == selected] else lay
  if (nrow(pts)) {
    la <- c(pts$start_lat, pts$end_lat)
    lo <- c(pts$start_lon, pts$end_lon)
    span <- max(diff(range(la, na.rm = TRUE)), diff(range(lo, na.rm = TRUE)) / 2, 1e-4)
    zoom <- if (pick) 16 else max(3, min(16, floor(log2(360 / span)) - 2))
    return(list(center = list(lat = mean(la, na.rm = TRUE), lon = mean(lo, na.rm = TRUE)), zoom = zoom))
  }
  sl <- matrix(numeric(), 0, 2)
  if (!is.null(sites) && nrow(sites)) sl <- cbind(num_or_na(sites$latitude), num_or_na(sites$longitude))
  sl <- sl[stats::complete.cases(sl), , drop = FALSE]
  if (nrow(sl)) return(list(center = list(lat = mean(sl[, 1]), lon = mean(sl[, 2])), zoom = 6))
  list(center = list(lat = 0, lon = 0), zoom = 0)
}

# One line per transect and survey year (a transect surveyed in 2018 and 2025 has two layers), so years can be switched
# off from the legend. Transects without surveys form their own layer.
#   years, projects: what to show ("none" = transects with no survey / no project)
transect_map <- function(tr, surveys, sites = NULL, selected = "", years = NULL, projects = NULL, basemap = "satellite", legend = TRUE) {
  tr <- copy(tr); for (v in c("start_lat", "start_lon", "end_lat", "end_lon")) set(tr, j = v, value = num_or_na(tr[[v]]))
  ok <- tr[!is.na(start_lat) & !is.na(start_lon)][, label := ifelse(nzchar(name), name, transect_id)]
  sv <- surveys[nzchar(transect_id) & kind %in% c("original", "merged"), .(transect_id, year = ifelse(is.na(year), "undated", as.character(year)), project, name)]
  lay <- merge(ok, sv[, .(n = .N, names = paste(name, collapse = ", "), project = paste(unique(project[nzchar(project)]), collapse = ", ")), by = .(transect_id, year)],
               by = "transect_id", all.x = TRUE)
  lay[, `:=`(year = as.character(year), project = as.character(project), names = as.character(names))]
  lay[is.na(year), `:=`(year = "no surveys", n = 0L, names = "", project = "")]; lay[is.na(project), project := ""]
  if (!is.null(years)) lay <- lay[year %in% years]
  if (!is.null(projects)) lay <- lay[vapply(strsplit(project, ", "), function(p) any(c(if (!length(p) || all(p == "")) "none" else p) %in% projects), TRUE)]
  yrs <- sort(unique(lay$year)); cols <- distinct_cols(setNames(rep(YEAR_COLS, length.out = length(yrs)), yrs), PAL); cols["no surveys"] <- PAL$fg4
  g <- plotly::plot_ly()
  for (i in seq_len(nrow(lay))) { t <- lay[i]; sel <- t$transect_id == selected
    lat <- c(t$start_lat, if (!is.na(t$end_lat)) t$end_lat); lon <- c(t$start_lon, if (!is.na(t$end_lon)) t$end_lon)
    hv <- sprintf("<b>%s</b><br>%s%s<br>%s", t$label, ifelse(nzchar(t$site), paste0(t$site, " \u00b7 "), ""), t$year,
                  if (t$n) sprintf("%d survey%s: %s%s", t$n, ifelse(t$n == 1, "", "s"), t$names, ifelse(nzchar(t$project), paste0("<br>", t$project), "")) else "no surveys linked")
    g <- plotly::add_trace(g, type = "scattermapbox", mode = if (length(lat) > 1) "lines+markers" else "markers", lat = lat, lon = lon,
      line = list(width = if (sel) 7 else 4, color = cols[[t$year]]), marker = list(size = c(11, 9)[seq_along(lat)], color = c(PAL$green, PAL$red)[seq_along(lat)]),
      text = c(paste(hv, "<br>start"), paste(hv, "<br>end"))[seq_along(lat)], hoverinfo = "text",
      name = t$year, legendgroup = t$year, showlegend = !t$year %in% lay$year[seq_len(i - 1)]) }
  if (!is.null(sites) && nrow(sites)) { st <- copy(sites)[, `:=`(la = num_or_na(latitude), lo = num_or_na(longitude))][!is.na(la) & !is.na(lo)]
    if (nrow(st)) g <- plotly::add_trace(g, type = "scattermapbox", mode = "markers+text", lat = st$la, lon = st$lo, text = st$site, textposition = "top right",
      marker = list(size = 10, color = PAL$accent), hoverinfo = "text", name = "sites", legendgroup = "sites", textfont = list(color = MAP_LABEL_COL)) }
  v <- map_view(lay, sites, selected)
  ctr <- v$center
  zoom <- v$zoom
  if (!nrow(lay)) g <- plotly::add_trace(g, type = "scattermapbox", mode = "markers", lat = ctr$lat, lon = ctr$lon, hoverinfo = "none",   # keeps the map when nothing is located yet
                                         marker = list(size = 1, opacity = 0), showlegend = FALSE)
  b <- BASEMAPS[[basemap]] %||% BASEMAPS$satellite
  plotly::layout(g, mapbox = list(style = b$style, layers = b$layers, center = ctr, zoom = zoom), margin = list(l = 0, r = 0, t = 0, b = 0),
                 paper_bgcolor = PAL$bg1, font = list(color = PAL$fg), showlegend = isTRUE(legend), legend = list(title = list(text = "Survey year"), bgcolor = MAP_LEGEND_BG, x = 0.01, y = 0.99)) |>
    plotly::config(displaylogo = FALSE)
}

# "Export view (PNG)" on the Map page: the map as it is on screen (centre, zoom, years shown), redrawn at 250 ppi, i.e. 250/96
# times the screen size, with lines, markers and text scaled to match and imagery fetched at the matching finer zoom.
# The PNG is tagged 250 ppi (pHYs chunk) and named ovid_gs_<what>_<date>.png.
MAP_PNG_JS <- r"--(
window.ovidPlotPng = async function (id, withLegend, what) {
  const gd = document.getElementById(id); if (!gd || !gd.data) return;
  const K = 250 / 96, W = Math.round(gd.clientWidth * K), H = Math.round(gd.clientHeight * K), sc = v => v == null ? v : Array.isArray(v) ? v.map(x => x * K) : v * K;
  const data = JSON.parse(JSON.stringify(gd.data)), lay = JSON.parse(JSON.stringify(gd.layout)), fl = gd._fullLayout;
  data.forEach(t => { if (t.line) t.line.width = sc(t.line.width ?? 2); if (t.marker) t.marker.size = sc(t.marker.size ?? 6);
    t.textfont = Object.assign({}, t.textfont, { size: sc((t.textfont && t.textfont.size) || 12) }); });
  Object.assign(lay, { width: W, height: H, showlegend: withLegend, autosize: false, font: Object.assign({}, lay.font, { size: sc((lay.font && lay.font.size) || 12) }) });
  lay.legend = Object.assign({}, lay.legend, { font: { size: sc(12), color: (lay.font || {}).color } });
  if (lay.legend.title) lay.legend.title = Object.assign({}, lay.legend.title, { font: { size: sc(13) } });
  if (fl.mapbox) { const m = fl.mapbox._subplot && fl.mapbox._subplot.map, c = m ? m.getCenter() : fl.mapbox.center;
    lay.mapbox = Object.assign({}, lay.mapbox, { center: { lat: c.lat, lon: c.lon ?? c.lng }, zoom: (m ? m.getZoom() : fl.mapbox.zoom) + Math.log2(K) }); }
  const url = await Plotly.toImage({ data, layout: lay }, { format: 'png', width: W, height: H });
  const png = Uint8Array.from(atob(url.split(',')[1]), c => c.charCodeAt(0));
  const T = new Uint32Array(256).map((_, n) => { let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; return c >>> 0; });
  const crc = b => { let c = 0xffffffff; for (const x of b) c = T[(c ^ x) & 255] ^ (c >>> 8); return (c ^ 0xffffffff) >>> 0; };
  const ch = new Uint8Array(21), v = new DataView(ch.buffer), ppm = Math.round(250 / 0.0254);
  v.setUint32(0, 9); ch.set([112, 72, 89, 115], 4); v.setUint32(8, ppm); v.setUint32(12, ppm); ch[16] = 1; v.setUint32(17, crc(ch.subarray(4, 17)));
  const out = new Uint8Array(png.length + 21); out.set(png.subarray(0, 33)); out.set(ch, 33); out.set(png.subarray(33), 54);
  const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([out], { type: 'image/png' }));
  a.download = `ovid_gs_${what}_${new Date().toISOString().slice(0, 10)}.png`; document.body.appendChild(a); a.click();
  setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
};
)--"
