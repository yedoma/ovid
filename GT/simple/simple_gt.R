# GT/simple/simple_gt.R  --  The Simple view of GT (M 28 Oct): a field check of the ground temperatures, in four steps.
# Import: the folders, Read the data and a checklist (downloads, clocks, sites, names, depths, places). View: one tile per
# site (its depths, last record, MAGT at the deepest sensor, ALT) and one plot, over time or as a profile by depth.
# Edit: names, places, land cover, sensor depths and which site each string is at (core simple.R), with the registry's names
# and a model's template depths proposed for review. Export: the field check report, the daily temperatures and the plots.
# The Standard server runs underneath (server(embedded = TRUE)), so a run, the outputs and Needs you are the same; Edit writes
# only sites.csv, sensors.csv and, for a rename, the tables RENAMES lists (R/config.R). Quality rules, clocks, corrections
# over a period and ground-surface changes are not edited here, and data values never. Sourced by app.R.

SP_GT_STOPPED_DAYS <- 30                      # a site whose last record is this much older than the newest has stopped (as WLR)
SP_GT_ASSUMED <- c("default", "template")     # depth sources that are a guess, not a measurement (R/qc.R complete_sensors)
SP_GT_TILES <- 8                              # tiles shown before "Show all"
SP_GT_HAND <- "set by hand"

sp_gt_chr <- function(d, cols) { d <- as.data.table(d); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x }); ensure_cols(d, cols) }
sp_gt_blank <- function(d, ...) { r <- as.data.table(as.list(stats::setNames(rep("", ncol(d)), names(d)))); x <- list(...)
  for (k in names(x)) set(r, j = k, value = as.character(x[[k]])); r }
sp_gt_cfg <- function(pd, what, cols) { p <- cfg_path(pd, what); sp_gt_chr(if (file.exists(p)) read_cfg(p) else data.table(), cols) }
sp_gt_num <- function(x) suppressWarnings(as.numeric(x))
# The sensors of a site in their order on the string (T1, T2 ... T10), for the depths typed as one list.
sp_gt_order <- function(sens) sens[order(serial, as.integer(sub("\\D*(\\d+).*", "\\1", paste0(sensor, "0"))), sensor)]
# A site still named after its logger (R/qc.R auto_site), from its rows of sensors.csv.
sp_gt_auto <- function(sens) { if (!nrow(sens)) return(character()); x <- sens[nzchar(site) & site == auto_site(logger_type, serial)]; unique(x$site) }

# The registry's name for each serial: REGISTRY_NAMES (R/config.R) names its file and columns, the folder is the project's
# registry_dir setting. The latest deployment of a serial wins. Returns serial, name (empty table when there is none).
sp_gt_registry <- function(dir) {
  none <- data.table(serial = character(), name = character()); if (!length(dir) || !nzchar(dir %||% "")) return(none)
  f <- file.path(path.expand(dir), REGISTRY_NAMES$file); if (!file.exists(f)) return(none)
  # safe: an unreadable registry offers no names
  r <- tryCatch(fread(file = f, colClasses = "character", na.strings = NULL), error = function(e) NULL); k <- REGISTRY_NAMES
  if (is.null(r) || !all(c(k$serial, k$name) %in% names(r))) return(none)
  r <- data.table(serial = norm_serial(r[[k$serial]]), name = trim(r[[k$name]]), at = if (k$order %in% names(r)) r[[k$order]] else "")[nzchar(serial) & nzchar(name)]
  r[order(serial, at)][, .(name = name[.N]), by = serial] }

# The renames the registry offers: for each site still named after its logger, the registry's name of that logger (site, name).
sp_gt_reg_names <- function(sens, reg) { auto <- sp_gt_auto(sens)
  if (!nrow(reg) || !length(auto)) return(data.table(site = character(), name = character()))
  x <- unique(sens[site %in% auto, .(site, serial = norm_serial(serial))])[reg, on = "serial", nomatch = 0][, .(site, name)]
  unique(x[nzchar(name) & name != site], by = "site") }

# ---- sensor depths (the Edit step's editor) ------------------------------------------------------------------------------------
# A site's temperature sensors in their order on the string (the rows still open, else all).
sp_gt_site_sensors <- function(sens, k) { se <- sens[site == k & nzchar(sensor)]; s1 <- se[!nzchar(end)]; sp_gt_order(if (nrow(s1)) s1 else se) }
sp_gt_even <- function(first, step, n) round(first + step * (seq_len(n) - 1L), 4)
sp_gt_sgn <- function() depth_sign(list(depth_positive = SETTINGS$depth_positive %||% CFG$depth_positive))   # the project's convention (R/util.R)
sp_gt_txt <- function(z) format(z, trim = TRUE, drop0trailing = TRUE)
# One typed depth as GT keeps it in sensors.csv (text, so a leading '+' stays; typed_depth() reads it, R/util.R): "+.1" and
# "+0,1" become "+0.10", "0,25" becomes "0.25" (at least two decimals, up to four as typed); anything else is left as typed,
# for its row to say why.
sp_gt_norm <- function(x) vapply(trim(as.character(x)), function(v) { if (is.na(v) || !nzchar(v)) return("")
  pl <- startsWith(v, "+"); b <- gsub(",", ".", sub("^\\+", "", v), fixed = TRUE); n <- suppressWarnings(as.numeric(b))
  if (is.na(n) || !grepl("^-?[0-9]*\\.?[0-9]+$", b)) return(v)
  d <- nchar(sub("^[^.]*\\.?", "", b)); paste0(if (pl) "+", formatC(n, format = "f", digits = max(2L, min(4L, d)))) }, "", USE.NAMES = FALSE)
# Depths are typed as text, as in Sensors and depths: metres below the surface, and a leading '+' for a sensor that far above it
# ("+0.3"), read by typed_depth() (R/util.R) whatever the project's convention. A pasted list (commas, semicolons, spaces or
# lines) as n depths; error says why it cannot be used.
sp_gt_parse_list <- function(txt, n) { v <- trim(strsplit(as.character(txt %||% ""), "[,;[:space:]]+")[[1]]); v <- v[nzchar(v)]
  if (!length(v)) return(list(z = character(), error = "The list is empty."))
  v <- sp_gt_norm(v); if (any(!depth_ok(v))) return(list(z = character(), error = paste0("Not a depth: ", paste(v[!depth_ok(v)], collapse = ", "), ".")))
  if (length(v) != n) return(list(z = character(), error = sprintf("The list has %d depths; this site has %d sensors.", length(v), n)))
  list(z = v, error = "") }
# What is wrong with each typed depth, in string order ("" when fine): blank or not a number, a minus where the project writes
# depths below the surface positive (+ marks a sensor above it), or not deeper than the sensor above it on the string.
# The string runs from its top down: sensors above the ground first (the highest at the top), then those in the ground, deeper
# each time; in GT's convention (negative above the ground) that is each depth greater than the one before.
sp_gt_depth_problems <- function(x, sgn = 1) { x <- sp_gt_norm(x); z <- typed_depth(x, sgn)
  vapply(seq_along(x), function(i)
    if (!nzchar(x[i])) "blank" else if (is.na(z[i])) "not a depth: a number in m, with + for above the ground" else if (sgn == 1 && startsWith(x[i], "-")) "use + for a sensor above the ground"
    else if (i > 1 && !is.na(z[i - 1]) && z[i] <= z[i - 1]) (if (z[i] < 0) "must be lower than the sensor above it on the string" else "must be deeper than the sensor above it") else "", "") }
# Changes setting the depths of each site (one op per site), as typed.
sp_gt_depth_ops <- function(sites, x) data.table(type = "field", group = sites, item = "depths", to = paste(sp_gt_norm(x), collapse = ", "))
# The other sites whose strings have the same model and sensor count as site k.
sp_gt_same_model <- function(sens, k) { x <- sp_gt_site_sensors(sens, k); if (!nrow(x)) return(character()); m <- unique(x$model[nzchar(x$model)]); if (!length(m)) return(character())
  o <- sens[nzchar(sensor) & nzchar(site) & site != k & model %in% m]; if (!nrow(o)) return(character())
  n <- vapply(unique(o$site), function(s) nrow(sp_gt_site_sensors(sens, s)), 0L); sort(names(n)[n == nrow(x)]) }
sp_gt_template_for <- function(tp, md) { if (!nrow(tp) || !nzchar(md %||% "")) return(tp[0]); tp[nzchar(model) & vapply(model, function(m) grepl(tolower(m), tolower(md), fixed = TRUE), NA)] }
# A tile's "Set depths" link: outside the tile's button (core simple_tile), it opens the Edit step's editor on that site.
sp_gt_depth_link <- function(tile, k) { tile$children <- c(tile$children, list(tags$a(class = "sp-tile-link", href = "#sp-depths",
  onclick = sprintf("Shiny.setInputValue('sp_set_depths', %s, {priority: 'event'}); return false;", jsonlite::toJSON(k, auto_unbox = TRUE)), "Set depths"))); tile }
# The live profile beside the boxes: the depth axis with a dot per sensor (red where its depth is wrong) and the site's mean
# temperature at each depth from the daily data (mt: depth_m, t) as a line, an inline SVG (no network, no plot device).
sp_gt_profile_svg <- function(z, prob, z0, mt) {
  z <- as.numeric(z); ok <- is.finite(z); all <- c(z[ok], as.numeric(z0), mt$depth_m); all <- all[is.finite(all)]; if (!length(all)) all <- c(0, 1)
  y1 <- max(1, ceiling(max(all) * 10) / 10); y0 <- min(0, floor(min(all) * 10) / 10)                # above the ground: negative, drawn above 0
  H <- 240; W <- 200; top <- 14; Y <- function(v) top + (H - top - 10) * (v - y0) / (y1 - y0)
  tr <- if (nrow(mt) && any(is.finite(mt$t))) range(c(0, mt$t), na.rm = TRUE) else c(-1, 1); if (diff(tr) == 0) tr <- tr + c(-1, 1); X <- function(v) 50 + (W - 60) * (v - tr[1]) / diff(tr)
  tk <- pretty(c(y0, y1), 5); tk <- tk[tk >= y0 & tk <= y1]
  ax <- paste0(sprintf('<line x1="44" x2="44" y1="%.1f" y2="%.1f" class="ax"/>', Y(y0), Y(y1)),
    paste0(sprintf('<text x="40" y="%.1f" text-anchor="end">%s</text><line x1="44" x2="%d" y1="%.1f" y2="%.1f" class="gr"/>', Y(tk) + 4,
                   ifelse(tk < 0, paste0("+", format(-tk)), format(tk)), W, Y(tk), Y(tk)), collapse = ""),
    if (y0 < 0) sprintf('<line x1="30" x2="%d" y1="%.1f" y2="%.1f" class="ground"/><text x="%d" y="%.1f" text-anchor="end">ground</text>', W, Y(0), Y(0), W, Y(0) - 3) else "",
    sprintf('<text x="%d" y="10" text-anchor="end">m</text>', 40))
  m0 <- mt[is.finite(t)][order(depth_m)]
  ln <- if (nrow(m0) > 1) sprintf('<polyline class="mt" fill="none" points="%s"/><line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" class="zero"/><text x="%.1f" y="%d" text-anchor="middle">0 °C</text>',
    paste(sprintf("%.1f,%.1f", X(m0$t), Y(m0$depth_m)), collapse = " "), X(0), X(0), Y(y0), Y(y1), X(0), H) else ""
  up <- ok & z < 0                                                                              # above the ground: a square, not a dot
  yy <- ifelse(ok, Y(z), Y(y0)); cls <- ifelse(nzchar(prob), "bad", "dot")
  lab <- paste0(seq_along(z), ": ", ifelse(ok, ifelse(up, paste0("+", format(-z), " m above ground"), paste0(format(z), " m")), "blank"), ifelse(nzchar(prob), paste0(" (", prob, ")"), ""))
  dots <- paste0(ifelse(up, sprintf('<rect x="39" y="%.1f" width="10" height="10" class="%s up"><title>%s</title></rect>', yy - 5, cls, lab),
                            sprintf('<circle cx="44" cy="%.1f" r="5" class="%s"><title>%s</title></circle>', yy, cls, lab)), collapse = "")
  HTML(sprintf('<svg class="sp-prof" viewBox="0 0 %d %d" role="img" aria-label="The sensors at their depths, and the mean temperature by depth">%s%s%s</svg><p class="sp-dim sp-small">Dots: the sensors as typed, squares above the ground (red: a depth to fix). Line: the mean temperature at each depth so far.</p>',
               W, H + 6, ax, ln, dots)) }

# One row per site: its sensors and depths (assumed or set), first and last record, days with a complete daily mean, MAGT at
# the deepest sensor and the ALT of the last complete year, exposed-sensor flags, and how far its last record is behind the
# newest of the project. daily: the site's daily means (depth_m, date, temp, complete).
sp_gt_facts <- function(o, pd, reg = NULL) {
  s <- copy(o$ss); if (is.null(s) || !nrow(s)) return(NULL)
  sens <- sp_gt_cfg(pd, "sensors", SENSOR_COLS); auto <- sp_gt_auto(sens)
  tm <- function(x) if (inherits(x, "POSIXct")) x else as.POSIXct(sub("Z$", "", sub("T", " ", as.character(x))), tz = "UTC")
  s[, `:=`(first_t = tm(first), last_t = tm(last))]
  newest <- max(s$last_t, na.rm = TRUE); dd <- o$daily; an <- o$annual; al <- o$alt; fl <- o$flags
  f <- rbindlist(lapply(seq_len(nrow(s)), function(i) { r <- s[i]; k <- r$site
    z <- sort(unique(sp_gt_num(strsplit(as.character(r$depth_list), ",\\s*")[[1]]))); z <- z[is.finite(z)]
    se <- sens[site == k]; assumed <- nrow(se) && any(se$depth_source %in% SP_GT_ASSUMED) || isTRUE(sp_gt_num(r$assumed_depths) > 0)
    d <- if (nrow(dd)) dd[site == k] else data.table()
    zd <- if (length(z)) max(z) else NA_real_
    a <- if (nrow(an) && is.finite(zd)) an[site == k & abs(depth_m - zd) < 1e-6 & isTRUE_v(complete) & year_type %in% c("calendar", "")][order(year)] else data.table()
    l <- if (nrow(al)) al[site == k][order(year)] else data.table(); lc <- if (nrow(l)) l[!grepl("incomplete", result)] else l
    ex <- if (nrow(fl)) fl[site == k & check == "air_like"] else data.table()
    sh <- if (nrow(d) && length(z)) d[abs(depth_m - min(z)) < 1e-6] else data.table()
    span <- if (is.finite(r$first_t) && is.finite(r$last_t)) as.integer(as.Date(r$last_t) - as.Date(r$first_t)) + 1L else NA_integer_
    sn <- unique(se$serial); rg <- if (!is.null(reg) && nrow(reg) && k %in% auto) reg[serial %in% norm_serial(sn)]$name else character()
    data.table(site = k, first = r$first_t, last = r$last_t, n = length(z), dmin = if (length(z)) min(z) else NA_real_, dmax = zd, assumed = assumed,
               named = !k %in% auto, suggest = if (length(rg)) rg[1] else "", loggers = paste(sn, collapse = ", "),
               days = if (nrow(sh)) sum(isTRUE_v(sh$complete)) else NA_integer_, span = span,
               magt = if (nrow(a)) a$mean[nrow(a)] else NA_real_, magt_year = if (nrow(a)) a$year[nrow(a)] else NA_integer_,
               alt = if (nrow(lc)) lc$alt_m[nrow(lc)] else NA_real_, alt_year = if (nrow(lc)) lc$year[nrow(lc)] else NA_integer_,
               alt_note = if (nrow(lc)) as.character(lc$result[nrow(lc)]) else "", exposed = nrow(ex), exposed_at = if (nrow(ex)) paste(unique(ex$channel), collapse = ", ") else "",
               behind = if (is.finite(r$last_t)) as.numeric(difftime(newest, r$last_t, units = "days")) else NA_real_) }), fill = TRUE)
  f[, state := "ok"]                                                             # each test NA-safe: a value not known is not a finding
  f[exposed > 0 | !named | assumed, state := "warn"]
  f[!is.na(behind) & behind > SP_GT_STOPPED_DAYS, state := "warn"]
  f[!is.finite(last), state := "off"]
  f[, note := fifelse(!is.na(behind) & behind > SP_GT_STOPPED_DAYS, paste0("No data after ", format(last, "%Y-%m-%d"), "."),
              fifelse(exposed > 0, "A sensor reads like air (exposed?), flagged and kept.",
              fifelse(!named, "Not named: still called after its logger.", fifelse(assumed, "Depths are assumed (default steps or a template).", ""))))]
  list(f = f[order(match(state, c("error", "warn", "ok", "off")), site)], daily = split(dd, by = "site", keep.by = FALSE), newest = newest)
}
isTRUE_v <- function(x) !is.na(x) & as.logical(x)

# Two lines of a site's daily means (shallowest and deepest sensor) as one sparkline, on one scale.
sp_gt_spark <- function(d, z1, z2, level = "ok", n = 120) {
  if (is.null(d) || !nrow(d) || !is.finite(z1)) return(NULL)
  one <- function(z) { x <- d[abs(depth_m - z) < 1e-6][order(date)]; utils::tail(x, 365) }
  a <- one(z1); b <- if (is.finite(z2) && z2 != z1) one(z2) else NULL; all <- c(a$temp, b$temp); if (sum(is.finite(all)) < 2) return(NULL)
  r <- range(all, na.rm = TRUE); if (diff(r) == 0) r <- r + c(-1, 1); t0 <- min(c(a$date, b$date)); t1 <- max(c(a$date, b$date)); w <- max(1, as.numeric(t1 - t0))
  line <- function(x, cls) { if (is.null(x) || nrow(x) < 2) return(""); if (nrow(x) > n) { k <- round(seq(1, nrow(x), length.out = n)); x <- x[k] }   # outside [ ]: the daily table has a column n
    k <- is.finite(x$temp); sprintf('<polyline fill="none" class="%s" points="%s"/>', cls, paste(sprintf("%.1f,%.1f", 300 * as.numeric(x$date[k] - t0) / w, 46 - 42 * (x$temp[k] - r[1]) / diff(r)), collapse = " ")) }
  HTML(sprintf('<svg class="sp-spark" viewBox="0 0 300 50" preserveAspectRatio="none" aria-hidden="true">%s%s</svg>', line(a, paste0("sp-spark-", level)), line(b, "sp-spark-off"))) }

# ---- interface ---------------------------------------------------------------------------------------------------------------
sp_gt_import_ui <- function() tagList(
  p(class = "sp-lead", "Point ovid at the folders that hold the logger downloads (.f2b, .hex, .hobo, .csv …), then press Read the data. The raw files are never changed."),
  div(class = "sp-cols",
    div(class = "sp-card", div(class = "sp-h", "Folders"),
      textAreaInput("raw_dirs", "Logger files, one folder per line (any layout)", paste(SETTINGS$raw_dirs, collapse = "\n"), rows = 3, width = "100%"),
      ovid_browse_button("raw_dirs"), uiOutput("raw_check"),
      textInput("project_dir", "Project folder (the results are kept here)", SETTINGS$project_dir, width = "100%"), ovid_browse_button("project_dir"),
      tags$details(tags$summary("More options"),
        textInput("metadata_dir", "Metadata folder (registry workbook, field notes), optional", SETTINGS$metadata_dir, width = "100%"), ovid_browse_button("metadata_dir"),
        textInput("registry_dir", "Registry folder, optional (station names by serial, offered in Edit)", SETTINGS$registry_dir %||% "", width = "100%"), ovid_browse_button("registry_dir")),
      div(class = "sp-row", if (VIEWER) span(class = "sp-dim", "A viewer copy: the data are already read.") else actionButton("run", "Read the data", class = "sp-btn"))),
    div(class = "sp-card", div(class = "sp-h", "What was read"), uiOutput("sp_read"), uiOutput("sp_checks"))),
  div(class = "sp-card", div(class = "sp-h", "Files"), uiOutput("sp_files")))

sp_gt_view_ui <- function() tagList(
  uiOutput("sp_hero"),
  div(class = "sp-row sp-filters", style = "margin-top:0",
    radioButtons("sp_filter", NULL, c("All" = "all", "Need a look" = "look", "Not named" = "unnamed", "Depths assumed" = "assumed"), inline = TRUE),
    div(class = "sp-fill"), textInput("sp_q", NULL, placeholder = "Find a site or serial", width = "220px")),
  uiOutput("sp_tiles", class = "sp-tiles"), uiOutput("sp_more"),
  div(class = "sp-card",
    div(class = "sp-row", style = "margin-top:0;justify-content:space-between",
      tags$b(textOutput("sp_plot_title", inline = TRUE)),
      radioButtons("sp_kind", NULL, c("Over time" = "time", "Profile by depth" = "profile"), inline = TRUE)),
    conditionalPanel("input.sp_kind != 'profile'",
      radioButtons("sp_period", NULL, c("Last 30 days" = "30", "This year" = "year", "All" = "all"), selected = "year", inline = TRUE),
      ts_view("sp_plot", 300)),
    conditionalPanel("input.sp_kind == 'profile'", plotOutput("sp_profile", height = "420px")),
    p(class = "sp-dim sp-small", textOutput("sp_plot_note", inline = TRUE))),
  uiOutput("sp_next"))

sp_gt_edit_ui <- function() tagList(uiOutput("sp_propose"), uiOutput("sp_depths"), simple_edit_ui("spe", "site", "strings"))

sp_gt_export_ui <- function() div(class = "sp-cols",
  div(class = "sp-card", div(class = "sp-h", "Field check report"),
    p("One page: the period, sites and strings; per site its sensors and depths, days, how complete the record is, MAGT at the deepest sensor, the active layer, thaw and freeze, and flags; what needs a look; the methods with their sources."),
    textInput("sp_rpt_who", "Your initials (on the report)", width = "220px"),
    div(class = "sp-row", downloadButton("sp_rpt", "Download the report (HTML)", class = "sp-btn", icon = NULL))),
  div(class = "sp-card", div(class = "sp-h", "Daily temperatures and plots"),
    p("One CSV per site with a value per day for each depth and the flags of that day, with a README of the methods, their sources and the run's record. The plots: each site over time and as a profile."),
    div(class = "sp-row", downloadButton("sp_csv", "Daily temperatures (ZIP)", class = "sp-btn2", icon = NULL), downloadButton("sp_png", "Plots (ZIP)", class = "sp-btn2", icon = NULL)),
    p(class = "sp-dim sp-small", "Every reading, soil moisture, NetCDF or the full report: ", tags$a(href = simple_std_href("Export"), "Standard, Report"), ".")))

# The Import checklist and the file table, in the Simple view's colours (core simple.css).
SP_GT_CSS <- paste(".sp-checks{margin-top:10px;border-top:1px solid var(--sp-line)}",
  ".sp-check{display:flex;flex-wrap:wrap;align-items:baseline;gap:4px 10px;padding:7px 0;border-bottom:1px solid var(--sp-line)}",
  ".sp-check>.sp-dot{flex:none;width:9px;height:9px;border-radius:50%;background:var(--sp-ok);align-self:center}",
  ".sp-check.sp-warn>.sp-dot{background:var(--sp-warn)} .sp-check.sp-error>.sp-dot{background:var(--sp-err)} .sp-check>.sp-fill{flex:1}",
  ".sp-table{width:100%;border-collapse:collapse;font-size:13px;font-variant-numeric:tabular-nums} .sp-table th{text-align:left;font-weight:600;color:var(--sp-mut);padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line2)}",
  ".sp-table td{padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line)}",
  ".sp-filters .form-group{margin:0} .sp-filters .shiny-options-group{display:flex;flex-wrap:wrap;gap:6px}",
  ".sp-filters .radio-inline{margin:0;padding:4px 12px;border:1px solid var(--sp-line2);border-radius:999px} .sp-filters .radio-inline input{display:none}",
  ".sp-filters .radio-inline:has(input:checked){border-color:var(--sp-acc);box-shadow:inset 0 0 0 1px var(--sp-acc)}",
  ".sp-depth-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(120px,1fr));gap:4px 12px;margin:6px 0} .sp-depth-cell .form-group{margin-bottom:0}",
  ".sp-depths .form-group{margin-bottom:0} .sp-depth-cols{display:grid;grid-template-columns:minmax(0,1fr) 220px;gap:16px;align-items:start;margin:8px 0}",
  ".sp-depth-row{display:flex;align-items:center;gap:8px;padding:2px 0} .sp-depth-n{min-width:64px;font-weight:600}",
  ".sp-depth-row.sp-assumed input{border-style:dashed;color:var(--sp-fg2)} .sp-depth-fill{flex-wrap:wrap} .sp-paste summary,.sp-many summary{cursor:pointer;color:var(--sp-acc)}",
  ".sp-prof{width:100%;height:auto} .sp-prof text{font-size:10px;fill:var(--sp-fg2)} .sp-prof .ax{stroke:var(--sp-line2)} .sp-prof .gr{stroke:var(--sp-line);stroke-dasharray:2 3}",
  ".sp-prof .zero{stroke:var(--sp-line2);stroke-dasharray:3 3} .sp-prof .ground{stroke:var(--sp-warn);stroke-width:1.2} .sp-prof .mt{stroke:var(--sp-info);stroke-width:1.6} .sp-prof .dot{fill:var(--sp-ok)} .sp-prof .bad{fill:var(--sp-err)}",
  "@media (max-width:760px){.sp-depth-cols{grid-template-columns:1fr}}")
# Enter in a depth box moves to the next one (Tab already goes down the rows)
SP_GT_JS <- "document.addEventListener('keydown',function(e){if(e.key!=='Enter')return;var t=e.target;if(!t.closest||!t.closest('.sp-depth-rows'))return;e.preventDefault();var a=Array.from(document.querySelectorAll('.sp-depth-rows input'));var i=a.indexOf(t);if(i>=0&&i<a.length-1)a[i+1].focus();});"

simple_ui <- function(req) {
  pages <- Filter(function(x) inherits(x, "ovid_tool"), gt_flow_pages(sources = FALSE))            # the Standard pages, hidden: their
  hidden <- lapply(pages, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))   # inputs exist for its server
  simple_page("GT", MI, import = sp_gt_import_ui(), view = sp_gt_view_ui(), edit = sp_gt_edit_ui(), export = sp_gt_export_ui(),
              project = if (!VIEWER) projects_switch_ui("proj"), hidden = hidden, head = tagList(std_head(), tags$style(HTML(SP_GT_CSS)), tags$script(HTML(SP_GT_JS))))
}

# ---- server ------------------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns (E, outs, pd, rv, mark, review).
simple_server <- function(input, output, session, std) {
  outs <- std$outs; pd <- std$pd
  steps <- simple_steps_server(session, function() c(import = !is.null(outs())))
  if (!is.null(isolate(outs()))) steps$go("view")
  ran <- reactiveVal(FALSE)
  observeEvent(input$run, ran(TRUE), priority = 10)
  observeEvent(outs(), if (isTRUE(ran()) && !is.null(outs())) { ran(FALSE); steps$go("view") })
  go_step <- function(k) sprintf("document.querySelector('.sp-step[data-step=\"%s\"]').click()", k)
  to_std <- function(page, label, site = NULL) tags$a(href = simple_std_href(page, list(site = site)), label)   # a page of Standard (core simple.R)
  observeEvent(input$registry_dir, { v <- trim(input$registry_dir %||% ""); if (!identical(v, SETTINGS$registry_dir %||% "")) {
    try(settings_save(SETTINGS_FILE, list(registry_dir = v)), silent = TRUE); SETTINGS$registry_dir <<- v } }, ignoreInit = TRUE)
  reg <- reactive({ input$registry_dir; sp_gt_registry(SETTINGS$registry_dir %||% "") })

  last_run <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$runs)) NULL else o$runs[.N] })
  sens <- reactive({ std$rv$tick; sp_gt_cfg(pd(), "sensors", SENSOR_COLS) })
  output$sp_read <- renderUI({ o <- outs(); r <- last_run()
    if (is.null(o)) return(p(class = "sp-dim", "Nothing read yet."))
    tags$dl(class = "sp-kv",
      tags$dt("Files"), tags$dd(sprintf("%s found, %s downloads read", num(nrow(o$files)), num(if (nrow(o$bundles)) sum(o$bundles$status == "ok") else 0))),
      tags$dt("Strings"), tags$dd(num(uniqueN(o$loggers$logger_id %||% o$loggers$serial))),
      tags$dt("Sensors"), tags$dd(num(nrow(sens()))),
      tags$dt("Sites"), tags$dd(num(nrow(o$ss))),
      tags$dt("Clocks"), tags$dd(paste("set to", zone_text(BASE_OFFSET))),
      tags$dt("Period"), tags$dd(if (nrow(o$ss)) paste(substr(min(as.character(o$ss$first)), 1, 10), "to", substr(max(as.character(o$ss$last)), 1, 10)) else "—"),
      tags$dt("Read"), tags$dd(if (!is.null(r)) as.character(r$started) else format(o$when, "%Y-%m-%d %H:%M")))
  })
  # the checklist: what a ground temperature record needs, each with its state and the page in Standard that fixes it
  output$sp_checks <- renderUI({ o <- outs(); if (is.null(o)) return(NULL); se <- sens()
    b <- o$bundles; nb <- nrow(b); ok <- if (nb) sum(b$status == "ok") else 0L; cp <- if (nrow(o$files) && "role" %in% names(o$files)) sum(o$files$role %in% c("copy", "duplicate")) else 0L
    ck <- o$clock; nck <- nrow(ck); strings_ck <- if (nck && "logger_id" %in% names(ck)) uniqueN(ck$logger_id) else 0L
    nosite <- se[!nzchar(site)]; auto <- sp_gt_auto(se); st <- sp_gt_cfg(pd(), "sites", SITE_COLS)
    asm <- sum(se$depth_source %in% SP_GT_ASSUMED); placed <- st[nzchar(latitude) & nzchar(longitude) | nzchar(easting) & nzchar(northing)]
    row <- function(lv, what, detail, fix = NULL) div(class = paste("sp-check", paste0("sp-", lv)), span(class = "sp-dot"), tags$b(what), span(class = "sp-dim", detail), div(class = "sp-fill"), fix)
    div(class = "sp-checks",
      row(if (ok == nb) "ok" else "warn", "Downloads read", sprintf("%d of %d (the best file of each; %d copies skipped)", ok, nb, cp), if (ok < nb) to_std("Files", "Bring in files")),
      row("ok", "Clocks fixed", if (nck) sprintf("%d fixes on %d strings (resets, impossible times, stray records), set to %s", nck, strings_ck, zone_text(BASE_OFFSET)) else paste("none needed; set to", zone_text(BASE_OFFSET))),
      row(if (!nrow(nosite)) "ok" else "warn", "Every string has a site", if (!nrow(nosite)) sprintf("%d strings", uniqueN(se$serial)) else paste(uniqueN(nosite$serial), "without one: give it in Edit")),
      row(if (!length(auto)) "ok" else "warn", "Site names", if (!length(auto)) sprintf("%d of %d named", nrow(o$ss), nrow(o$ss)) else sprintf("%d of %d still named after their logger: name them in Edit", length(auto), nrow(o$ss))),
      row(if (!asm) "ok" else "warn", "Sensor depths", sprintf("%d / %d set%s", nrow(se) - asm, nrow(se), if (asm) "; the rest assumed: type them in Edit" else "")),
      row(if (nrow(placed) >= nrow(o$ss)) "ok" else "warn", "Places", sprintf("%d of %d sites placed", nrow(placed), nrow(o$ss)))) })
  output$sp_files <- renderUI({ o <- outs(); if (is.null(o) || !nrow(o$files)) return(p(class = "sp-dim", "No files read yet."))
    fl <- copy(o$files); se <- sens(); fl[, nsens := vapply(as.character(serial), function(s) sum(se$serial == s), 0L)]
    fl[, first := !duplicated(serial)]; one <- fl[first == TRUE]; rep <- fl[, .N, by = serial]
    tb <- function(x) tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("File"), tags$th("Model"), tags$th("Serial"), tags$th("Sensors"), tags$th("Downloads"))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(class = "sp-mono", basename(as.character(x$rel[i]))), tags$td(as.character(x$model[i])), tags$td(class = "sp-mono", x$serial[i]),
        tags$td(x$nsens[i]), tags$td(rep$N[match(x$serial[i], rep$serial)])))))
    n <- 12L; tagList(p(class = "sp-dim sp-small", "One row per string, its first download; repeat downloads of a string are folded into its count."), tb(one[seq_len(min(nrow(one), n))]),
      if (nrow(one) > n) tags$details(tags$summary(sprintf("All %d strings", nrow(one))), tb(one[seq(n + 1L, nrow(one))]))) })

  # ---- View
  facts <- reactive({ o <- outs(); req(o); std$rv$tick; sp_gt_facts(o, pd(), reg()) })
  pick <- reactiveVal(NULL); all_tiles <- reactiveVal(FALSE)
  observeEvent(input$sp_pick, pick(input$sp_pick))
  observeEvent(input$sp_all, all_tiles(!all_tiles()))
  cur <- reactive({ x <- facts(); req(x); k <- pick(); if (is.null(k) || !k %in% x$f$site) x$f[state != "off"]$site[1] %||% x$f$site[1] else k })
  shown <- reactive({ x <- facts(); req(x); f <- x$f; w <- input$sp_filter %||% "all"; q <- tolower(trim(input$sp_q %||% ""))
    f <- switch(w, look = f[state %in% c("warn", "error")], unnamed = f[named == FALSE], assumed = f[assumed == TRUE], f)
    if (nzchar(q)) f <- f[grepl(q, tolower(site), fixed = TRUE) | grepl(q, tolower(loggers), fixed = TRUE) | grepl(q, tolower(suggest), fixed = TRUE)]
    f })
  output$sp_hero <- renderUI({ o <- outs()
    if (is.null(o)) return(simple_hero("off", "Nothing read yet", "Choose the folders and press Read the data.", tags$button(type = "button", class = "btn sp-btn", onclick = go_step("import"), "Import")))
    x <- facts(); if (is.null(x)) return(simple_hero("warn", "No sites yet", "The downloads read have no temperature sensors."))
    f <- x$f; bad <- f[state %in% c("warn", "error")]; stop <- f[!is.na(behind) & behind > SP_GT_STOPPED_DAYS]
    lv <- if (nrow(bad)) "warn" else "ok"
    head <- paste0(nrow(f), " sites · ", nrow(f) - nrow(stop), " reporting")
    sub <- paste0("Newest record ", format(x$newest, "%Y-%m-%d %H:%M"), ". ", if (nrow(bad)) paste0(bad$site[1], ": ", bad$note[1], if (nrow(bad) > 1) paste0(" ", nrow(bad) - 1, " more need a look.")) else "Nothing to look at.")
    simple_hero(lv, head, sub, if (!VIEWER) tags$button(type = "button", class = "btn sp-btn2", onclick = "document.getElementById('run').click()", "Read again")) })
  output$sp_tiles <- renderUI({ x <- facts(); f <- shown(); k <- cur(); if (!nrow(f)) return(p(class = "sp-dim", "No site matches."))
    if (!all_tiles()) f <- utils::head(f, SP_GT_TILES)
    lapply(seq_len(nrow(f)), function(i) { r <- f[i]; d <- x$daily[[r$site]]
      dep <- if (r$n) sprintf("%d · %s–%s m%s", r$n, format(r$dmin), format(r$dmax), if (r$assumed) " *" else "") else "—"
      simple_tile(r$site, r$state, state = if (!r$named) "Not named" else NULL, spark = sp_gt_spark(d, r$dmin, r$dmax, r$state),
        facts = stats::setNames(list(dep, if (is.finite(r$last)) format(r$last, "%Y-%m-%d") else "—",
                                     if (is.na(r$magt)) "—" else sprintf("%.2f °C (%d)", r$magt, r$magt_year),
                                     if (!is.na(r$alt)) sprintf("%.2f m (%d)", r$alt, r$alt_year) else if (nzchar(r$alt_note)) "no pf" else "—"),
                                c("Sensors", "Last record", "MAGT, deepest", "ALT, last year")),
        note = paste0(r$note, if (nzchar(r$suggest)) paste0(" Registry: ", r$suggest, ".") else ""), on = identical(r$site, k),
        link = if (!is.na(r$behind) && r$behind > SP_GT_STOPPED_DAYS) list(label = "Open in Standard: Needs you", page = "Needs", site = r$site)
               else if (r$exposed > 0) list(label = "Open in Standard: Sensor health", page = "Health", site = r$site)) |> sp_gt_depth_link(r$site) }) })
  output$sp_more <- renderUI({ f <- shown(); if (nrow(f) <= SP_GT_TILES) return(NULL)
    div(class = "sp-row", style = "margin-top:0", actionButton("sp_all", if (all_tiles()) "Show fewer" else sprintf("Show all %d", nrow(f)), class = "sp-btn2")) })
  sel_daily <- reactive({ x <- facts(); x$daily[[cur()]] })
  output$sp_plot_title <- renderText(cur())
  observe({ req(identical(input$simple_step, "view"), !identical(input$sp_kind, "profile")); d <- sel_daily(); req(d, nrow(d) > 0)
    d <- d[order(date)]; n <- input$sp_period %||% "year"
    if (n == "30") d <- d[date > max(date) - 30] else if (n == "year") d <- d[date > max(date) - 365]
    z <- sort(unique(d$depth_m)); tt <- sort(unique(d$date)); cols <- depth_ramp(length(z))
    s <- lapply(seq_along(z), function(i) { x <- d[abs(depth_m - z[i]) < 1e-6]; v <- rep(NA_real_, length(tt)); v[match(x$date, tt)] <- x$temp
      list(name = depth_lab(z[i]), key = format(z[i]), unit = "°C", v = v, digits = 2, col = cols[i], panel = "all", plab = "Daily mean, every depth (°C)") })
    t <- as.POSIXct(tt) + 43200
    session$onFlushed(function() ts_view_send(session, "sp_plot", t, s, zone = "UTC", height = 300), once = TRUE) })
  output$sp_profile <- renderPlot({ req(identical(input$sp_kind, "profile")); o <- outs(); k <- cur(); a <- o$annual
    req(nrow(a) > 0); a <- a[site == k & isTRUE_v(complete) & year_type %in% c("calendar", "")]; validate(need(nrow(a) > 0, "No complete year at this site yet: the profile needs one."))
    y <- max(a$year); a <- a[year == y][order(depth_m)]; l <- o$alt[site == k & year == y]; alt <- if (nrow(l)) l$alt_m[1] else NA_real_
    g <- ggplot2::ggplot(a, ggplot2::aes(y = depth_m)) + ggplot2::geom_vline(xintercept = 0, colour = MI$gray, linewidth = 0.3) +
      ggplot2::geom_path(ggplot2::aes(x = min), colour = MI$blue, linewidth = 0.8) + ggplot2::geom_path(ggplot2::aes(x = max), colour = MI$red, linewidth = 0.8) +
      ggplot2::geom_path(ggplot2::aes(x = mean), colour = MI$fg, linewidth = 0.8) + ggplot2::geom_point(ggplot2::aes(x = mean), colour = MI$fg, size = 1.6) +
      ggplot2::scale_y_reverse() + ggplot2::labs(x = "Temperature (°C)", y = "Depth (m)", title = paste(k, y), subtitle = "Minimum, mean and maximum of the daily means") + theme_ovid(MI)
    ac <- .or(MI$gold, MI$yellow, MI$accent)
    if (is.finite(alt)) g <- g + ggplot2::geom_hline(yintercept = alt, linetype = "dashed", colour = ac) + ggplot2::annotate("text", x = -Inf, y = alt, label = sprintf(" ALT %.2f m", alt), hjust = 0, vjust = -0.4, colour = ac, size = 3.4)
    g }, bg = MI$bg)
  output$sp_plot_note <- renderText({ x <- facts(); r <- x$f[site == cur()]
    paste0(if (identical(input$sp_kind, "profile")) "Envelope of the last complete year (minimum, mean, maximum of the daily means) by depth; dashed: the active-layer thickness, the deepest thaw where permafrost is present (Nelson & Hinkel 2003; Brown et al. 2000). "
           else "Daily means at each depth, shallow to deep; the zero curtain is kept as recorded. ",
           "Permafrost: at or below 0 °C for two years or more (van Everdingen 1998). A day counts when at least ", round(100 * (CFG$min_complete %||% 0.8)), " % of its readings are there (WMO-No. 1203).",
           if (nrow(r) && r$assumed) " Depths marked * are assumed, so MAGT and ALT may move once they are set." else "") })
  output$sp_next <- renderUI({ req(outs()); n <- tryCatch(nrow(std$review$items()), error = function(e) 0L)
    div(class = "sp-row",
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("edit"), "✎ Name sites, set depths and places"),
      tags$button(type = "button", class = "btn sp-btn2", onclick = go_step("export"), "⤓ Export or report"),
      div(class = "sp-fill"),
      if (n) span(class = "sp-dim", paste(n, if (n == 1) "thing waits" else "things wait", "for a decision in Needs you:")),
      to_std(if (n) "Needs" else "Home", if (n) "Open in Standard" else "Assess in Standard ›")) })

  # ---- Edit
  sites_tab <- reactive({ std$rv$tick; sp_gt_cfg(pd(), "sites", SITE_COLS) })
  rows <- reactive({ se <- sens(); req(nzchar(pd())); if (!nrow(se)) return(NULL); st <- sites_tab()
    r <- se[nzchar(serial), .(first = { v <- start[nzchar(start)]; if (length(v)) min(v) else "" }, last = { v <- end[nzchar(end)]; if (length(v) == .N) max(v) else "" }), by = .(item = serial, group = site)]
    r[, `:=`(label = item, kind = "")]
    r[, `:=`(lat = sp_gt_num(st$latitude[match(group, st$site)]), lon = sp_gt_num(st$longitude[match(group, st$site)]))]
    r[, .(item, label, kind, group, first, last, lat, lon)] })
  depths_of <- function(k) { se <- sens()[site == k & nzchar(sensor)]; s1 <- se[!nzchar(end)]; se <- sp_gt_order(if (nrow(s1)) s1 else se); if (!nrow(se)) return("")
    paste0(paste(se$depth_m, collapse = ", "), if (any(se$depth_source %in% SP_GT_ASSUMED)) " (assumed)" else "") }
  touched <- c("sites.csv", "sensors.csv", vapply(c(RENAMES$site$cols, RENAMES$site$lists), `[`, "", 1))
  busy <- reactive({ invalidateLater(3000); k <- ls(OVID_PENDING); if (!length(k)) return(character())
    k <- k[dirname(k) == live_path(file.path(pd(), "config"))]; intersect(unique(basename(k)), touched) })
  ed <- simple_edit_server("spe", rows, apply = function(ops, who) { r <- sp_gt_apply(pd(), ops, who); std$rv$tick <- isolate(std$rv$tick) + 1; std$mark(); r },
    busy = busy, noun = "site", items = "strings",
    fields = list(list(id = "landcover", label = "Land cover", type = "text", get = function(k) { st <- sites_tab(); v <- st$landcover[st$site == k]; if (length(v)) v[1] else "" }),
                  list(id = "depths", label = "Sensor depths, m (changes MAGT and ALT)", type = "readonly", get = depths_of)))
  # Sensor depths: one box per sensor of the site picked (here, on its card, from a tile's "Set depths" or the "n assumed" chip),
  # a live profile beside them, quick fills that only fill the boxes, and actions that add to the list of changes (propose()):
  # this site, every site of the same model and sensor count, or the depths saved as the model's template.
  dsite <- reactiveVal(NULL)
  observeEvent(input[["spe-pick"]], dsite(input[["spe-pick"]]))
  observeEvent(input$sp_d_site, if (nzchar(input$sp_d_site %||% "")) dsite(input$sp_d_site), ignoreInit = TRUE)
  observeEvent(input$sp_set_depths, { dsite(input$sp_set_depths); steps$go("edit") })
  asm_sites <- reactive({ se <- sens(); sort(unique(se[depth_source %in% SP_GT_ASSUMED & nzchar(sensor)]$site)) })
  observeEvent(input$sp_d_next, { a <- asm_sites(); if (!length(a)) return(); k <- dsite(); i <- match(k, a); dsite(a[if (is.na(i)) 1 else i %% length(a) + 1]) })
  dsens <- reactive({ k <- dsite(); req(k); sp_gt_site_sensors(sens(), k) })
  dpend <- reactive({ o <- ed$ops(); k <- dsite(); x <- o[type == "field" & item == "depths" & group == k]; if (nrow(x)) x$to[nrow(x)] else NULL })
  dmodel <- reactive({ x <- dsens(); m <- unique(x$model[nzchar(x$model)]); if (length(m)) m[1] else "" })
  dhit <- reactive({ sp_gt_template_for(tmpl(), dmodel()) })
  dvals <- reactive({ x <- dsens(); sp_gt_norm(vapply(seq_len(nrow(x)), function(i) as.character(input[[paste0("sp_d_", i)]] %||% ""), "")) })   # as typed, tidied
  dprob <- reactive(sp_gt_depth_problems(dvals(), sp_gt_sgn()))
  output$sp_depths <- renderUI({ se <- sens(); st <- sort(unique(se[nzchar(site) & nzchar(sensor)]$site)); if (!length(st)) return(NULL)
    asm <- asm_sites(); k <- dsite(); if (is.null(k) || !k %in% st) { k <- if (length(asm)) asm[1] else st[1]; isolate(dsite(k)) }
    x <- isolate(dsens()); pv <- isolate(dpend()); z <- if (!is.null(pv)) trim(strsplit(pv, ",")[[1]]) else x$depth_m
    md <- isolate(dmodel()); hit <- isolate(dhit()); many <- sp_gt_same_model(se, k)
    cp <- setdiff(unique(se[nzchar(sensor), .N, by = site][N == nrow(x)]$site), k)
    div(class = "sp-card sp-depths", id = "sp-depths",
      div(class = "sp-row", style = "margin-top:0;align-items:flex-end",
        div(class = "sp-h", style = "margin:0", "Sensor depths"),
        selectizeInput("sp_d_site", NULL, st, selected = k, width = "320px", options = list(placeholder = "Find a site")),
        if (length(asm)) actionButton("sp_d_next", sprintf("%d assumed · next ›", length(asm)), class = "sp-btn2"),
        div(class = "sp-fill"), span(class = "sp-dim", sprintf("%s · %d sensors", if (nzchar(md)) md else "model not known", nrow(x)))),
      div(class = "sp-depth-cols",
        div(class = "sp-depth-rows", lapply(seq_len(nrow(x)), function(i) { as <- x$depth_source[i] %in% SP_GT_ASSUMED && is.null(pv)
          div(class = paste("sp-depth-row", if (as) "sp-assumed"), span(class = "sp-depth-n", x$sensor[i], if (uniqueN(x$serial) > 1) span(class = "sp-dim", x$serial[i])),
            tagAppendAttributes(textInput(paste0("sp_d_", i), NULL, sp_gt_norm(z[i]), width = "110px", placeholder = "0.25 or +0.10"), .cssSelector = "input", inputmode = "decimal"), span(class = "sp-dim", "m"),
            span(class = "sp-dim sp-small", if (as) "assumed" else if (!is.null(pv)) "in the changes" else x$depth_source[i]), uiOutput(paste0("sp_d_m_", i), inline = TRUE)) }),
          p(class = "sp-dim sp-small", if (identical(SETTINGS$depth_positive, "up")) "In string order, as this project writes depths: metres above the surface positive, below it negative; a leading + is always above the ground, e.g. +0.10."
            else "Depth below the surface in m. Add + for a sensor above the ground, e.g. +0.10.")),
        div(class = "sp-depth-prev", uiOutput("sp_d_prev"))),
      div(class = "sp-row sp-depth-fill",
        span(class = "sp-dim", "Fill:"), numericInput("sp_d_first", NULL, 0.05, step = 0.01, width = "86px"), span(class = "sp-dim", "every"),
        numericInput("sp_d_step", NULL, 0.1, step = 0.01, width = "86px"), actionButton("sp_d_even", "Even steps", class = "sp-btn2"),
        if (length(cp)) tagList(selectInput("sp_d_copy", NULL, c(stats::setNames("", "Copy from site…"), stats::setNames(cp, cp)), width = "200px")),
        if (nrow(hit)) actionButton("sp_d_tmpl", paste("Use", hit$model[1], "template"), class = "sp-btn2"),
        tags$details(class = "sp-paste", tags$summary("Paste a list"), textAreaInput("sp_d_paste", NULL, rows = 2, width = "260px", placeholder = "0.05, 0.25, 0.5 … (commas, spaces or lines)"),
                     actionButton("sp_d_paste_go", "Fill from the list", class = "sp-btn2"))),
      uiOutput("sp_d_msg"),
      div(class = "sp-row",
        actionButton("sp_d_add", "Add to changes", class = "sp-btn"),
        if (length(many)) tags$details(class = "sp-many", tags$summary(sprintf("Apply to all sites with the same model and sensor count (%d)", length(many))),
          checkboxGroupInput("sp_d_many", NULL, many, selected = many, inline = TRUE), actionButton("sp_d_many_go", "Add these sites to the changes", class = "sp-btn2")),
        if (nzchar(md)) actionButton("sp_d_save_tmpl", paste("Save as template for", md), class = "sp-btn2")),
      p(class = "sp-dim sp-small", "Depths change MAGT and ALT. Each action adds to the list of changes below: Save keeps them (Undo takes them back), and the next Read the data uses them.")) })
  for (i in seq_len(40)) local({ j <- i; output[[paste0("sp_d_m_", j)]] <- renderUI({ p0 <- dprob(); if (j > length(p0) || !nzchar(p0[j])) NULL else span(class = "sp-warn-t", p0[j]) }) })
  output$sp_d_prev <- renderUI({ x <- dsens(); z <- dvals(); pr <- dprob(); k <- dsite(); d <- facts()$daily[[k]]
    mt <- if (!is.null(d) && nrow(d)) d[, .(t = mean(temp, na.rm = TRUE)), by = depth_m] else data.table(depth_m = numeric(), t = numeric())
    sp_gt_profile_svg(typed_depth(z, sp_gt_sgn()), pr, typed_depth(x$depth_m, sp_gt_sgn()), mt) })
  setz <- function(z) { x <- dsens(); z <- sp_gt_norm(if (is.numeric(z)) sp_gt_txt(z) else as.character(z)); for (i in seq_len(min(length(z), nrow(x)))) updateTextInput(session, paste0("sp_d_", i), value = z[i]) }
  dmsg <- reactiveVal(NULL); output$sp_d_msg <- renderUI({ m <- dmsg(); if (is.null(m)) NULL else p(class = "sp-warn-t", m) })
  observeEvent(input$sp_d_even, { a <- input$sp_d_first; b <- input$sp_d_step; req(is.finite(a), is.finite(b)); setz(sp_gt_even(a, b, nrow(dsens()))); dmsg(NULL) })
  observeEvent(input$sp_d_copy, { k <- input$sp_d_copy; if (!nzchar(k %||% "")) return(); setz(sp_gt_site_sensors(sens(), k)$depth_m); dmsg(paste("Filled from", k, "; check them, then Add to changes.")) }, ignoreInit = TRUE)
  observeEvent(input$sp_d_tmpl, { h <- dhit(); req(nrow(h) > 0); r <- sp_gt_parse_list(h$depths[1], nrow(dsens())); if (nzchar(r$error)) dmsg(paste("Template:", r$error)) else { setz(r$z); dmsg(NULL) } })
  observeEvent(input$sp_d_paste_go, { r <- sp_gt_parse_list(input$sp_d_paste, nrow(dsens())); if (nzchar(r$error)) dmsg(r$error) else { setz(r$z); dmsg(NULL) } })
  dok <- function() { p0 <- dprob(); if (any(nzchar(p0))) { dmsg(sprintf("Not added: %s.", paste(unique(p0[nzchar(p0)]), collapse = "; "))); FALSE } else TRUE }
  observeEvent(input$sp_d_add, { if (!dok()) return(); ed$propose(sp_gt_depth_ops(dsite(), dvals())); dmsg(NULL)
    showNotification(paste("Depths of", dsite(), "added to the changes below; Save to keep them."), type = "message") })
  observeEvent(input$sp_d_many_go, { if (!dok()) return(); k <- unique(c(dsite(), input$sp_d_many)); ed$propose(sp_gt_depth_ops(k, dvals())); dmsg(NULL)
    showNotification(sprintf("Depths added for %d sites; Save to keep them.", length(k)), type = "message") })
  observeEvent(input$sp_d_save_tmpl, { if (!dok()) return(); ed$propose(data.table(type = "field", group = dmodel(), item = "template", to = paste(dvals(), collapse = ", ")))
    showNotification(paste("The template for", dmodel(), "is in the changes below; Save to keep it."), type = "message") })

  # proposals for review: the registry's names for sites still named after their logger, and a model's template depths
  tmpl <- reactive({ std$rv$tick; sp_gt_cfg(pd(), "templates", TEMPLATE_COLS) })
  proposals <- reactive({ se <- sens(); auto <- sp_gt_auto(se); rg <- reg()
    names <- sp_gt_reg_names(se, rg)
    tp <- tmpl(); tm <- if (nrow(tp)) rbindlist(lapply(seq_len(nrow(tp)), function(i) { m <- tp$model[i]; if (!nzchar(m)) return(NULL)
      k <- unique(se[grepl(tolower(m), tolower(model), fixed = TRUE) & depth_source %in% SP_GT_ASSUMED]$site); if (!length(k)) return(NULL)
      data.table(model = m, depths = tp$depths[i], site = k) })) else data.table()
    list(names = names, tm = tm) })
  output$sp_propose <- renderUI({ p0 <- proposals(); n <- nrow(p0$names); tm <- p0$tm
    if (!n && !nzchar(SETTINGS$registry_dir %||% "")) return(NULL)
    div(class = "sp-card", div(class = "sp-h", "Suggestions to review"),
      div(class = "sp-row", style = "margin-top:0",
        if (n) actionButton("sp_use_reg", sprintf("Use registry names (%d)", n), class = "sp-btn2") else span(class = "sp-dim", if (nzchar(SETTINGS$registry_dir %||% "")) "The registry names no site still called after its logger." else ""),
      ),
      p(class = "sp-dim sp-small", "They are added to the changes below to look over: drop any that are wrong, then Save. Only sites still named after their logger get a registry name.")) })
  observeEvent(input$sp_use_reg, { p0 <- proposals()$names; if (!nrow(p0)) return()
    ed$propose(data.table(type = "rename", group = p0$site, to = p0$name)) })

  # ---- Export
  table_of <- function() { x <- facts(); o <- outs(); f <- x$f[state != "off"]
    ft <- o$ft; tf <- function(k, z) { if (!nrow(ft) || !is.finite(z)) return("—"); r <- ft[site == k & abs(depth_m - z) < 1e-6][order(water_year)]; if (!nrow(r)) return("—")
      th <- as.character(r$first_day_above_0_after_freezing); fr <- as.character(r$first_day_at_or_below_0); i <- max(which(nzchar(th) & !is.na(th)), 0L); if (!i) return("—")
      paste0(substr(th[i], 6, 10), " → ", if (i < length(fr) && !is.na(fr[i + 1]) && nzchar(fr[i + 1])) substr(fr[i + 1], 6, 10) else "…") }
    fc <- if (nrow(o$flags)) o$flags[, .N, by = site] else data.table(site = character(), N = integer())
    f[, .(Site = site, `Sensors m` = fifelse(n > 0, paste0(format(dmin), "–", format(dmax), fifelse(assumed, " *", "")), "—"), Days = span, `Complete %` = round(100 * days / span),
          `MAGT deepest °C` = round(magt, 2), `ALT last year m` = fifelse(is.na(alt), fifelse(nzchar(alt_note), "no pf", "—"), sprintf("%.2f", alt)),
          `Thaw → freeze` = mapply(tf, site, dmin), Flags = fcoalesce(fc$N[match(site, fc$site)], 0L))] }
  doc <- function() { x <- facts(); o <- outs(); f <- x$f[state != "off"]; r <- last_run(); nm <- basename(pd()); if (identical(nm, "project")) nm <- basename(dirname(pd()))
    need <- round(100 * (CFG$min_complete %||% 0.8)); fl <- if (nrow(o$flags)) o$flags[site %in% f$site, .N, by = check][order(-N)] else data.table()
    bad <- f[state %in% c("warn", "error")]; who <- trim(input$sp_rpt_who %||% "")
    deep <- rbindlist(lapply(f$site, function(k) { d <- x$daily[[k]]; if (is.null(d) || !nrow(d)) return(NULL); z <- max(d$depth_m); d[abs(depth_m - z) < 1e-6, .(site = k, date, temp)] }))
    plot <- if (nrow(deep)) ggplot2::ggplot(deep[!is.na(temp)], ggplot2::aes(date, temp, colour = site)) + ggplot2::geom_line(linewidth = 0.3) + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, colour = MI$gray) +
      ggplot2::scale_colour_viridis_d(end = 0.85, guide = "none") + ggplot2::labs(x = NULL, y = "Temperature (°C)") + ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) else NULL
    simple_report_doc(paste("Field check:", nm),
      meta = c(paste(format(min(f$first, na.rm = TRUE), "%Y-%m-%d"), "to", format(max(f$last, na.rm = TRUE), "%Y-%m-%d")), paste(nrow(f), "sites"), paste(uniqueN(sens()$serial), "strings"),
               if (!is.null(r)) paste("run", substr(as.character(r$started), 1, 10)), if (nzchar(who)) paste("checked by", who)),
      summary = paste0(sum(is.na(f$behind) | f$behind <= SP_GT_STOPPED_DAYS), " of ", nrow(f), " sites were recording up to the newest record (", format(x$newest, "%Y-%m-%d"), "). ",
                       if (any(f$assumed)) paste0(sum(f$assumed), " sites have assumed depths (marked *): their MAGT and ALT may move once the depths are set. ") else "",
                       if (nrow(bad)) paste0(nrow(bad), " sites need a look: ", paste(utils::head(bad$site, 6), collapse = ", "), if (nrow(bad) > 6) " …" else "", ". ") else "",
                       if (nrow(fl)) paste0("Flags (values kept): ", paste0(fl$N, " ", gsub("_", " ", fl$check), collapse = ", "), ".") else ""),
      figures = c(Sites = as.character(nrow(f)), Sensors = as.character(nrow(sens())), `Depths set` = sprintf("%d / %d", sum(!sens()$depth_source %in% SP_GT_ASSUMED), nrow(sens())),
                  `With ALT` = as.character(sum(!is.na(f$alt)))),
      notes = c(Sites = "with ground temperatures", Sensors = "thermistors", `Depths set` = "the rest assumed", `With ALT` = "permafrost within the sensors"),
      plot = plot, plot_caption = "Daily mean temperature at each site's deepest sensor, quality checked as the project sets.",
      table = table_of(), table_caption = "Sites",
      methods = c(paste0("Readings as logged, clocks set to ", zone_text(BASE_OFFSET), ", copies and clock resets removed, quality rules as the project sets them; every removal in output/audit (ovid GT pipeline ", APP_VERSION, ")."),
                  "MAGT: the mean of the daily means of a complete calendar year at the deepest sensor. Permafrost: ground at or below 0 °C for at least two consecutive years: van Everdingen, R. O. (ed.) (1998, rev. 2005) Multi-language Glossary of Permafrost and Related Ground-Ice Terms, IPA.",
                  "ALT: the deepest daily thaw depth of the year (0 °C interpolated between sensors), where permafrost is present: Nelson, F. E., Hinkel, K. M. (2003) Methods for measuring active-layer thickness; Brown, J., Hinkel, K. M., Nelson, F. E. (2000) The CALM program, Polar Geogr. 24.",
                  "Thaw and freeze: the first day above 0 °C after freezing and the first day at or below 0 °C, at the shallowest sensor; the zero curtain (days within ±0.2 °C of 0 °C): Outcalt, S. I., Nelson, F. E., Hinkel, K. M. (1990) Water Resour. Res. 26.",
                  paste0("Complete %: days with a daily mean (at least ", need, " % of the day's readings) over the days from first to last record: WMO (2017) WMO-No. 1203; WMO (2018) WMO-No. 100."),
                  "Depths marked * are assumed (default steps or a model's template), not measured."),
      record = c(run = r$run_id %||% "", started = as.character(r$started %||% ""), files = as.character(r$files %||% ""),
                 pipeline = r$pipeline_version %||% APP_VERSION, core = r$core_version %||% CORE_VERSION, `set-up` = r$config_hash %||% "",
                 audit = "output/audit: every removed or changed value"), pal = MI) }
  output$sp_rpt <- downloadHandler(filename = function() "field_check.html",
    content = function(file) { req(outs()); withProgress(message = "Making the report", value = 0.5, { b <- tempfile("field_check"); out <- simple_report_write(doc(), b, "html", MI); file.copy(out[1], file, overwrite = TRUE) }) })
  readme <- function(r) c(paste("Daily ground temperatures from ovid GT, Simple view,", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    "One file per site. date is the day; each t_<depth> column the day's mean temperature (°C) at that depth (m below the ground), quality checked",
    "as the project sets; flags the checks flagged that day (values kept). Depths assumed (default steps or a template) are listed below the files.",
    paste0("A day has a value when at least ", round(100 * (CFG$min_complete %||% 0.8)), " % of its readings are there: WMO (2017) WMO-No. 1203; WMO (2018) WMO-No. 100."),
    "Permafrost, MAGT: van Everdingen (1998) IPA glossary. Active layer: Nelson and Hinkel (2003); Brown et al. (2000).", "",
    paste("Run:", r$run_id %||% ""), paste("Pipeline:", r$pipeline_version %||% APP_VERSION, " core:", r$core_version %||% CORE_VERSION),
    paste("Set-up:", r$config_hash %||% ""), "Every removed or changed value is in the project's output/audit folder.")
  output$sp_csv <- downloadHandler(filename = function() "daily_ground_temperatures.zip", contentType = "application/zip",
    content = function(file) { x <- facts(); o <- outs(); dir <- tempfile("daily"); dir.create(dir)
      fs <- unlist(lapply(names(x$daily), function(k) { d <- x$daily[[k]]; if (is.null(d) || !nrow(d)) return(NULL)
        w <- dcast(d, date ~ paste0("t_", format(depth_m)), value.var = "temp"); fl <- if (nrow(o$flags)) o$flags[site == k] else data.table()
        fd <- if (nrow(fl)) { a <- as.integer(as.Date(substr(fl$start, 1, 10))); b <- pmax(a, as.integer(as.Date(substr(fl$end, 1, 10))), na.rm = TRUE)
            data.table(day = unlist(Map(seq.int, a, b)), check = rep(fl$check, b - a + 1L))[, .(flags = paste(sort(unique(check)), collapse = ";")), by = day] } else data.table(day = integer(), flags = character())
        w[, flags := fcoalesce(fd$flags[match(as.integer(date), fd$day)], "")]
        f <- file.path(dir, paste0("daily_", safe_name(k), ".csv")); fwrite(w, f); basename(f) }))
      a <- x$f[assumed == TRUE]$site; writeLines(c(readme(last_run()), "", if (length(a)) paste("Sites with assumed depths:", paste(a, collapse = ", "))), file.path(dir, "README.txt"))
      zip::zip(file, c(fs, "README.txt"), root = dir) })
  output$sp_png <- downloadHandler(filename = function() "ground_temperature_plots.zip", contentType = "application/zip",
    content = function(file) { x <- facts(); o <- outs(); dir <- tempfile("plots"); dir.create(dir)
      fs <- unlist(lapply(names(x$daily), function(k) { d <- x$daily[[k]]; if (is.null(d) || !nrow(d)) return(NULL); z <- sort(unique(d$depth_m))
        g1 <- ggplot2::ggplot(d, ggplot2::aes(date, temp, colour = factor(depth_m, levels = z))) + ggplot2::geom_line(linewidth = 0.3) + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
          ggplot2::scale_colour_manual(values = depth_ramp(length(z)), name = "Depth (m)") + ggplot2::labs(x = NULL, y = "Temperature (°C)", title = k, subtitle = "Daily means, quality checked as the project sets") + ggplot2::theme_minimal(base_size = 10)
        f1 <- file.path(dir, paste0(safe_name(k), "_over_time.png")); ggplot2::ggsave(f1, g1, width = 18, height = 9, units = "cm", dpi = 200, bg = "white")
        a <- o$annual[site == k & isTRUE_v(complete) & year_type %in% c("calendar", "")]; f2 <- NULL
        if (nrow(a)) { y <- max(a$year); a <- a[year == y][order(depth_m)]
          g2 <- ggplot2::ggplot(a, ggplot2::aes(y = depth_m)) + ggplot2::geom_vline(xintercept = 0, linewidth = 0.3) + ggplot2::geom_path(ggplot2::aes(x = min), colour = MI$blue) +
            ggplot2::geom_path(ggplot2::aes(x = max), colour = MI$red) + ggplot2::geom_path(ggplot2::aes(x = mean)) + ggplot2::scale_y_reverse() +
            ggplot2::labs(x = "Temperature (°C)", y = "Depth (m)", title = paste(k, y), subtitle = "Minimum, mean and maximum of the daily means") + ggplot2::theme_minimal(base_size = 10)
          f2 <- file.path(dir, paste0(safe_name(k), "_profile.png")); ggplot2::ggsave(f2, g2, width = 10, height = 12, units = "cm", dpi = 200, bg = "white") }
        basename(c(f1, f2)) }))
      zip::zip(file, fs, root = dir) })
}

# Saves the Edit step's changes, in order, each with the version before in config/history and the change log (backup_cfg):
#   rename  through rename.R (every table RENAMES lists: sites.csv, sensors.csv ...)
#   move    the string's rows at its site in sensors.csv end the day before and start again at the new site that day (or the
#           whole record moves)
#   place   latitude and longitude in sites.csv; field landcover: its land cover there
#   field depths: the site's sensors in their order on the string get the depths typed (as many as there are sensors),
#           noted "set by hand" so a run keeps them
sp_gt_apply <- function(pd, ops, who) {
  before <- list(); keep <- function(f) { fp <- file.path(pd, "config", f); if (is.null(before[[f]])) before[[f]] <<- if (file.exists(fp)) copy(read_cfg(fp)) else NA }
  wr <- function(what, d, why) { f <- CONFIG_FILES[[what]]$file; fp <- cfg_path(pd, what); keep(f); backup_cfg(fp, d, who, why); fwrite_safe(d, fp) }
  site_row <- function(st, k) { i <- which(st$site == k); if (length(i)) return(list(st = st, i = i[1])); st <- rbind(st, sp_gt_blank(st, site = k)); list(st = st, i = nrow(st)) }
  for (j in seq_len(nrow(ops))) { q <- ops[j]
    if (q$type == "rename") {
      plan <- rename_plan(pd, "site", q$group, q$to); for (f in unique(plan$file)) keep(f)
      if (nrow(plan)) rename_apply(pd, plan, who, "rename (Simple view)")
    } else if (q$type == "move") {
      se <- sp_gt_cfg(pd, "sensors", SENSOR_COLS); i <- which(se$serial == q$item & se$site == q$group); if (!length(i)) i <- which(se$serial == q$item & !nzchar(se$site))
      if (!length(i)) stop("no sensor rows for ", q$item, " at ", q$group)
      if (!nzchar(q$at)) se[i, site := q$to]
      else { at <- as.Date(substr(q$at, 1, 10)); i <- i[!nzchar(se$end[i]) | se$end[i] >= format(at)]
        nw <- copy(se[i]); nw[, `:=`(site = q$to, start = format(at), end = "", notes = "moved here in the Simple view")]
        se[i, end := format(at - 1)]; se <- rbind(se, nw) }
      wr("sensors", se, "move (Simple view)")
    } else if (q$type == "place") {
      x <- site_row(sp_gt_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, `:=`(latitude = format(q$lat, digits = 9), longitude = format(q$lon, digits = 9))]
      wr("sites", x$st, "place (Simple view)")
    } else if (q$type == "field" && identical(q$item, "landcover")) {
      x <- site_row(sp_gt_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, landcover := q$to]; wr("sites", x$st, "land cover (Simple view)")
    } else if (q$type == "field" && identical(q$item, "template")) {
      tp <- sp_gt_cfg(pd, "templates", TEMPLATE_COLS); i <- which(tolower(tp$model) == tolower(q$group))
      if (length(i)) tp[i[1], depths := q$to] else tp <- rbind(tp, sp_gt_blank(tp, model = q$group, depths = q$to, notes = "saved in the Simple view"))
      wr("templates", tp, "template (Simple view)")
    } else if (q$type == "field" && identical(q$item, "depths")) {
      z <- sp_gt_norm(trim(strsplit(sub("\\s*\\(assumed\\)\\s*$", "", q$to), "[,;]")[[1]])); z <- z[nzchar(z)]
      if (!length(z) || any(!depth_ok(z))) stop("the depths of ", q$group, " are numbers in metres separated by commas (+ for above the ground)")
      se <- sp_gt_cfg(pd, "sensors", SENSOR_COLS); se[, .r := .I]; o <- sp_gt_order(se[site == q$group & nzchar(sensor) & !nzchar(end)]); if (!nrow(o)) o <- sp_gt_order(se[site == q$group & nzchar(sensor)])
      if (length(z) != nrow(o)) stop(q$group, " has ", nrow(o), " sensors; ", length(z), " depths were typed")
      se[o$.r, `:=`(depth_m = z, depth_source = SP_GT_HAND)]; se[, .r := NULL]
      wr("sensors", se, "depths (Simple view)") } }
  n <- nrow(ops)
  list(ok = TRUE, message = sprintf("Saved %d change%s. Read the data again (Import) to see %s in the results.", n, if (n > 1) "s" else "", if (n > 1) "them" else "it"),
       undo = function() for (f in names(before)) { fp <- file.path(pd, "config", f); b <- before[[f]]
         if (identical(b, NA)) { if (file.exists(fp)) { e <- read_cfg(fp)[0]; backup_cfg(fp, e, who, "undo (Simple view)"); fwrite_safe(e, fp) } }
         else { backup_cfg(fp, b, who, "undo (Simple view)"); fwrite_safe(b, fp) } })
}
