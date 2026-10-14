# GS/pages/fct_gpr_flow.R  --  What the person decides about a GPR line, saved in the project and never beside the raw files,
# as plain functions (no Shiny; tests call them): the processing flow of each line with its versions
# (config/gpr_flows/<line>.json), markers added by hand (config/gpr_markers.csv; the .mrk stays as it is) and picks
# (config/gpr_picks.csv). Depth of a pick is two-way time x velocity / 2 (gpr_depth() in R/gpr.R; REFLEXW manual,
# Davis and Annan 1989). Sourced by app.R.

GPR_FLOW_KEYS <- c("order", "dc", "dewow", "t0", "t0_ns", "bp", "bp1", "bp2", "bp3", "bp4", "gain", "ga", "gb", "agc_ns", "bg", "bgn")   # order: the steps in the order run (GPR_STEPS)
GPR_MARKER_COLS <- c("line", "trace", "note", "added")
GPR_PICK_COLS <- c("line", "trace", "distance_m", "twt_ns", "depth_m", "layer", "velocity_m_ns", "flow_version", "picked")

gpr_cfg_dir <- function(project) file.path(path.expand(project), "config")
gpr_flow_path <- function(project, line) file.path(gpr_cfg_dir(project), "gpr_flows", paste0(safe_name(line), ".json"))

# Every saved version of a line's flow (a list, oldest first); empty when none is saved.
gpr_flow_history <- function(project, line) { p <- gpr_flow_path(project, line); if (!file.exists(p)) return(list())
  x <- tryCatch(jsonlite::read_json(p, simplifyVector = FALSE), error = function(e) NULL); x$versions %||% list() }

# Save the flow as the line's next version: steps (a list with GPR_FLOW_KEYS), the velocities and thaw depth used for depth,
# and the preset it started from. Returns the version number.
gpr_flow_save <- function(project, line, steps, velocity, v_top = velocity, thaw_m = 0, preset = "", time = Sys.time()) {
  h <- gpr_flow_history(project, line); v <- length(h) + 1L
  h[[v]] <- list(version = v, saved = format(time, "%Y-%m-%d %H:%M"), preset = preset, steps = steps[intersect(GPR_FLOW_KEYS, names(steps))],
                 velocity_m_ns = velocity, v_top_m_ns = v_top, thaw_m = thaw_m)
  p <- gpr_flow_path(project, line); dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(list(line = line, versions = h), p, auto_unbox = TRUE, pretty = TRUE, digits = NA); v
}

# The saved flow of a line (the latest, or the version asked for), or NULL.
gpr_flow_load <- function(project, line, version = NULL) { h <- gpr_flow_history(project, line); if (!length(h)) return(NULL)
  if (is.null(version)) h[[length(h)]] else { i <- which(vapply(h, function(x) identical(as.integer(x$version), as.integer(version)), NA)); if (length(i)) h[[i[1]]] else NULL } }

# The flow of a line with none saved: the preset given (else the first) with every other setting and the velocities at their
# defaults, so a line never keeps the settings of the line shown before it.
GPR_STEP_DEFAULTS <- list(t0_ns = 0, bp1 = 0.25, bp2 = 1, bp3 = 2, bp4 = 4, ga = 0, gb = 0, agc_ns = 25, bgn = 50L)
gpr_flow_default <- function(preset = "") {
  p <- GPR_PRESETS[[if (length(preset) == 1 && preset %in% names(GPR_PRESETS)) preset else 1L]]
  list(steps = c(list(order = names(GPR_STEPS)), p[c("dc", "dewow", "t0", "bp", "gain", "bg")], GPR_STEP_DEFAULTS),
       velocity_m_ns = 0.141, v_top_m_ns = 0.0775, thaw_m = 0) }

# TRUE when the processing now in use is the saved flow f (same steps, same order, same values).
gpr_flow_same <- function(f, steps) { if (is.null(f)) return(FALSE); a <- f$steps; a$order <- gpr_order(a$order); b <- steps; b$order <- gpr_order(b$order)
  identical(as.character(unlist(a[GPR_FLOW_KEYS])), as.character(unlist(b[GPR_FLOW_KEYS]))) }
# The order of the steps: the one given (names of GPR_STEPS), with any step it leaves out at its usual place at the end.
gpr_order <- function(o) { o <- as.character(unlist(o)); c(intersect(o, names(GPR_STEPS)), setdiff(names(GPR_STEPS), o)) }
# Move a step one place up (-1) or down (+1).
gpr_order_move <- function(o, step, by) { o <- gpr_order(o); i <- match(step, o); j <- i + by; if (is.na(i) || j < 1L || j > length(o)) return(o); o[c(i, j)] <- o[c(j, i)]; o }
# One line saying what a step does with these settings ("" when it is off). mhz: the antenna frequency (NA when unknown).
gpr_step_summary <- function(step, p, mhz = NA) switch(step,
  dc = if (isTRUE(p$dc)) "last 25 % of each trace" else "off (mean of each trace removed)",
  dewow = if (isTRUE(p$dewow > 0)) sprintf("%g period(s)%s", p$dewow, if (is.na(mhz)) "" else sprintf(" (%.1f ns)", p$dewow * 1000 / mhz)) else "off",
  t0 = switch(p$t0 %||% "none", auto = "first zero crossing", manual = sprintf("set to %g ns", p$t0_ns %||% 0), "off"),
  bp = if (isTRUE(p$bp)) { f <- c(p$bp1, p$bp2, p$bp3, p$bp4); if (is.na(mhz)) paste(f, collapse = " / ") else paste(paste(round(f * mhz), collapse = " / "), "MHz") } else "off",
  gain = switch(p$gain %||% "none", decay = "energy decay", fn = sprintf("(1 + %g t) exp(%g t)", p$ga %||% 0, p$gb %||% 0), agc = sprintf("AGC, %g ns", p$agc_ns %||% 25), "off"),
  bg = switch(p$bg %||% "none", mean = "mean trace of the line", moving = sprintf("moving mean of %d traces", as.integer(p$bgn %||% 50L)), "off"), "")

# A line read from its files (header, traces, GPS, markers); r: one row of gpr_scan().
gpr_read_line <- function(r) list(r = r, h = read_rad(r$rad), m = read_gpr_data(r$data, r$samples, if (grepl("\\.rd7$", r$data, ignore.case = TRUE)) 4L else 2L),
  cor = if (!is.na(r$cor)) read_cor(r$cor) else NULL, mrk = if (!is.na(r$mrk)) read_mrk(r$mrk) else NULL)

# The radargram's distance across, for a line read by gpr_read_line(): mode "distance" (from its GPS, else the header's
# distance interval), "markers" (the field markers, spacing m apart; gpr_marker_distance()) or "trace". full: the distance of
# every trace, NULL when there are too few to interpolate (then by trace).
gpr_axis <- function(x, mode = "distance", spacing = NA) {
  n <- ncol(x$m)
  d <- if (identical(mode, "distance")) { d <- gpr_distance(x$cor, n); if (is.null(d) && isTRUE(x$h$distance_interval_m > 0)) d <- (seq_len(n) - 1) * x$h$distance_interval_m; d }
       else if (identical(mode, "markers")) gpr_marker_distance(x$mrk, spacing, n)
  if (sum(is.finite(d)) < 2L) d <- NULL
  list(full = d, label = if (is.null(d)) "Trace" else if (identical(mode, "markers")) "Distance from markers (m)" else "Distance (m)")
}

# Save one flow as the next version of every line given (Apply to all lines); returns the number saved.
gpr_flow_apply_all <- function(project, lines, steps, velocity, v_top = velocity, thaw_m = 0, preset = "", time = Sys.time()) {
  for (k in lines) gpr_flow_save(project, k, steps, velocity, v_top, thaw_m, preset, time); length(lines) }

# The line written into every exported image and file: the line, the flow version and what was applied.
gpr_flow_caption <- function(line, flow, applied) paste0(line, " · ", if (is.null(flow)) "flow not saved" else sprintf("flow v%d (%s)", as.integer(flow$version), flow$saved),
  " · ", if (length(applied)) paste(applied, collapse = "; ") else "no processing")

# Markers added by hand for a line (trace, note), from config/gpr_markers.csv.
gpr_markers_read <- function(project, line = NULL) { p <- file.path(gpr_cfg_dir(project), "gpr_markers.csv")
  x <- if (file.exists(p)) ensure_cols(fread(p, colClasses = "character"), GPR_MARKER_COLS) else empty_cfg(GPR_MARKER_COLS)
  if (!is.null(line)) { ln <- line; x <- x[x$line == ln] }; x[, trace := suppressWarnings(as.integer(trace))][] }
gpr_marker_add <- function(project, line, trace, note = "", time = Sys.time()) {
  p <- file.path(gpr_cfg_dir(project), "gpr_markers.csv"); x <- gpr_markers_read(project)[, trace := as.character(trace)]
  x <- rbind(x, data.table(line = line, trace = as.character(as.integer(trace)), note = trimws(note), added = format(time, "%Y-%m-%d %H:%M")))
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE); fwrite_safe(x, p); invisible(x) }
gpr_marker_drop_last <- function(project, line) { p <- file.path(gpr_cfg_dir(project), "gpr_markers.csv"); x <- gpr_markers_read(project); i <- which(x$line == line)
  if (length(i)) { x <- x[-max(i)][, trace := as.character(trace)]; fwrite_safe(x, p) }; invisible(x) }

# The two-way time (ns, from time zero) of a depth: the inverse of gpr_depth().
gpr_twt <- function(depth_m, v, thaw_m = 0, v_top = v) {
  if (!isTRUE(thaw_m > 0)) return(2 * depth_m / v)
  ifelse(depth_m <= thaw_m, 2 * depth_m / v_top, 2 * thaw_m / v_top + 2 * (depth_m - thaw_m) / v)
}
# A pick from a click on the radargram: the nearest trace to x (dist: the distance of every trace, or NULL when x is the trace),
# the two-way time from y (a depth or a time), and its depth at the velocities given.
gpr_pick_row <- function(line, x, y, vaxis, dist, velocity, thaw_m = 0, v_top = velocity, layer = "", flow_version = NA, time = Sys.time()) {
  tr <- if (is.null(dist)) as.integer(round(x)) else which.min(abs(dist - x))
  twt <- if (identical(vaxis, "depth")) gpr_twt(y, velocity, thaw_m, v_top) else y
  data.table(line = line, trace = tr, distance_m = if (is.null(dist)) NA_real_ else round(dist[tr], 2), twt_ns = round(twt, 2),
             depth_m = round(gpr_depth(twt, velocity, thaw_m, v_top), 3), layer = trimws(layer), velocity_m_ns = velocity,
             flow_version = if (is.null(flow_version) || is.na(flow_version)) "" else as.character(flow_version), picked = format(time, "%Y-%m-%d %H:%M:%S"))
}
gpr_picks_path <- function(project) file.path(gpr_cfg_dir(project), "gpr_picks.csv")
gpr_picks_read <- function(project, line = NULL) { p <- gpr_picks_path(project)
  x <- if (file.exists(p)) ensure_cols(fread(p, colClasses = "character"), GPR_PICK_COLS) else empty_cfg(GPR_PICK_COLS)
  if (!is.null(line)) { ln <- line; x <- x[x$line == ln] }; x[] }
gpr_pick_add <- function(project, row) { p <- gpr_picks_path(project); x <- gpr_picks_read(project)
  x <- rbind(x, row[, lapply(.SD, as.character)], fill = TRUE); dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE); fwrite_safe(x, p); invisible(x) }
gpr_pick_drop_last <- function(project, line) { p <- gpr_picks_path(project); x <- gpr_picks_read(project); i <- which(x$line == line)
  if (length(i)) { x <- x[-max(i)]; fwrite_safe(x, p) }; invisible(x) }

# The year of a line, from its first GPS fix ("" without GPS).
gpr_line_year <- function(cor) if (is.null(cor) || !nrow(cor) || all(is.na(cor$datetime))) "" else format(min(cor$datetime, na.rm = TRUE), "%Y")
