# ERT/pages/fct_stack.R  --  The figures behind the Home and Stack pages, as plain functions (no Shiny; tests call them).
# Sourced by app.R. Numbers are shown as computed: no lights and no judgement.

stack_blank <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# The project a survey belongs to: the project people set, else its folder (folders stand in for projects until named).
survey_project <- function(s) { p <- stack_blank(s$project); ifelse(nzchar(p), p, stack_blank(s$folder)) }

# One row per original (or merged) survey: project, date, name, measurements, percent kept under the survey's QC profile,
# and the median contact resistance (ohm) and median repeat error (%) from survey_stats.csv for that profile. placed: the
# survey's transect has a start point in transects.csv.
survey_figures <- function(s, stats = NULL, transects = NULL) {
  s <- s[kind %in% c("original", "merged")]
  if (!nrow(s)) return(data.table(survey_id = character(), project = character(), date = character(), name = character(), n_meas = integer(),
                                  kept_pct = numeric(), contact_ohm = numeric(), error_pct = numeric(), qc_profile = character(), placed = logical(),
                                  transect_id = character(), layout = character()))
  prof <- if ("qc_profile" %in% names(s)) stack_blank(s$qc_profile) else rep("", nrow(s))
  med <- function(v) { if (is.null(stats) || !nrow(stats)) return(rep(NA_real_, nrow(s)))
    x <- stats[variable == v & set == "all", .(survey_id, profile, median)]
    x[data.table(survey_id = s$survey_id, profile = prof), on = .(survey_id, profile), mult = "first"]$median }
  nn <- function(v) if (v %in% names(s)) suppressWarnings(as.numeric(s[[v]])) else rep(NA_real_, nrow(s))
  tr <- stack_blank(s$transect_id)
  placed <- if (is.null(transects) || !nrow(transects)) rep(FALSE, nrow(s)) else {
    t <- transects[nzchar(stack_blank(start_lat)) & nzchar(stack_blank(start_lon))]; nzchar(tr) & tr %in% t$transect_id }
  data.table(survey_id = s$survey_id, project = survey_project(s), date = stack_blank(s$date), name = stack_blank(s$name),
             n_meas = as.integer(nn("n_read")), kept_pct = nn("pct_retained"),
             contact_ohm = med("contact_ohm"), error_pct = med("error_pct"), qc_profile = prof, placed = placed, transect_id = tr,
             layout = electrode_layout(s))[order(project, date, name)]
}

# The electrode layout of each survey, as one string: number of electrodes, spacing and array. Two surveys with the same
# string were laid out the same way.
electrode_layout <- function(s) {
  ne <- if ("n_electrodes" %in% names(s)) suppressWarnings(as.integer(s$n_electrodes)) else NA_integer_
  sp <- if ("spacing_m" %in% names(s)) suppressWarnings(as.numeric(s$spacing_m)) else NA_real_
  ar <- if ("array" %in% names(s)) stack_blank(s$array) else ""
  ifelse(is.na(ne) | is.na(sp), "", paste0(ne, " electrodes at ", format(sp, trim = TRUE, drop0trailing = TRUE), " m", ifelse(nzchar(ar), paste0(", ", ar), "")))
}

STACK_MIN_KEPT <- 50   # Compare's default "Both kept at least (%)", in Standard and the Workbench
stack_min_kept <- function(x) if (is.numeric(x) && length(x) == 1 && !is.na(x)) x else STACK_MIN_KEPT   # the setting, or the default

# Whether two surveys can be compared, and why not: the same electrode layout and both at or above min_kept % kept. A change
# image of badly contacted surveys shows the contacts, not the ground. r1, r2: rows of survey_figures().
stack_comparable <- function(r1, r2, min_kept = STACK_MIN_KEPT) {
  why <- character()
  if (!nzchar(r1$layout) || !identical(r1$layout, r2$layout))
    why <- c(why, sprintf("different electrode layouts (%s: %s; %s: %s)", r1$name, if (nzchar(r1$layout)) r1$layout else "unknown", r2$name, if (nzchar(r2$layout)) r2$layout else "unknown"))
  for (r in list(r1, r2)) if (is.na(r$kept_pct) || r$kept_pct < min_kept)
    why <- c(why, sprintf("%s: %s kept%s", r$name, if (is.na(r$kept_pct)) "no figure for" else paste0(format(round(r$kept_pct, 1), nsmall = 1), " %"),
                          if (!is.na(r$contact_ohm)) sprintf("; contact %s kohm", format(round(r$contact_ohm / 1000, 1), nsmall = 1)) else ""))
  list(ok = !length(why), why = why)
}

# Compare with...: the choices for the one Compare control (Stack, and the Workbench's Panels): the other surveys of f
# (survey_figures() rows), those with survey sid's electrode layout first, then by date; "" is off.
stack_compare_choices <- function(f, sid) {
  off <- c(`Compare with… (off)` = ""); me <- f[survey_id == sid]; if (!length(sid) || !nrow(me)) return(off)
  o <- f[survey_id != sid][order(layout != me$layout[1], -as.numeric(date != ""), date)]
  c(off, stats::setNames(o$survey_id, paste(o$date, o$name, ifelse(o$layout == me$layout[1], "", "\u00b7 other layout"))))
}

# Percent change of apparent resistivity from survey 1 to survey 2, matched by electrode indices (a, b, m, n), never by plotted
# position. Repeats of a quadrupole are averaged first; only positive values are used. Positions are survey 1's.
stack_change <- function(d1, d2) {
  k <- c("a", "b", "m", "n")
  one <- function(d) { x <- cbind(d[, c(k, "rhoa_ohmm"), with = FALSE], pseudo_xy(d))[is.finite(rhoa_ohmm) & rhoa_ohmm > 0]
    x[, .(rhoa = mean(rhoa_ohmm), px = px[1], pz = pz[1]), by = k] }
  x1 <- one(d1); x2 <- one(d2)
  m <- merge(x1, x2[, c(k, "rhoa"), with = FALSE], by = k, suffixes = c("_1", "_2"))
  m[, change_pct := 100 * (rhoa_2 - rhoa_1) / rhoa_1][]
  list(data = m, n_matched = nrow(m), n_1 = nrow(x1), n_2 = nrow(x2))
}

# The project's surveys side by side on one colour scale (lim, ohm-m, log), raw values, sorted by date. d: one data.table of
# measurements with survey_id; f: their survey_figures() rows. Each panel is titled with its figures.
plot_stack <- function(d, f, lim = RHOA_RANGE, pal = PAL, ncol = 3) {
  if (!nrow(d)) return(NULL)
  p <- d[is.finite(rhoa_ohmm) & rhoa_ohmm > 0]
  lab <- f[, .(survey_id, label = sprintf("%s  ·  %s\n%s kept  ·  contact %s  ·  repeat error %s", name, date,
             ifelse(is.na(kept_pct), "–", paste0(format(round(kept_pct, 1), nsmall = 1), " %")),
             ifelse(is.na(contact_ohm), "–", paste0(format(round(contact_ohm / 1000, 1), nsmall = 1), " kohm")),
             ifelse(is.na(error_pct), "–", paste0(format(round(error_pct, 2), nsmall = 2), " %"))))]
  p <- merge(p, lab, by = "survey_id"); p[, label := factor(label, unique(lab$label))]
  lo <- log10(max(lim[1], 0.01)); hi <- log10(max(lim[2], lim[1] * 1.01, 0.02))
  ggplot(p, aes(px, pz, colour = pmin(pmax(log10(rhoa_ohmm), lo), hi))) + geom_point(size = 1.4) +
    facet_wrap(~label, ncol = ncol) +
    scale_colour_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), limits = c(lo, hi),
                           name = "Apparent resistivity (ohm-m, log scale)", labels = function(b) format(signif(10^b, 3), big.mark = ",", trim = TRUE)) +
    guides(colour = guide_colourbar(barwidth = 16, title.position = "top")) +
    labs(x = "Distance along line (m)", y = "Pseudo-depth (m)", caption = "All measurements as read (nothing hidden or smoothed). Values outside the scale take its end colour. Pseudo-depth = 0.19 x array length.") +
    theme_ovid(pal) + theme(legend.position = "bottom", strip.text = element_text(hjust = 0), plot.caption = element_text(colour = pal$fg4, size = 8, hjust = 0))
}

# The change image: percent change at survey 1's positions, blue (lower) - white - red (higher), +/- lim %.
plot_change <- function(ch, n1, n2, lim = 25, pal = PAL) {
  ggplot(ch$data, aes(px, pz, colour = pmin(pmax(change_pct, -lim), lim))) + geom_point(size = 2) +
    scale_colour_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0, limits = c(-lim, lim), name = "Change in apparent resistivity (%)") +
    guides(colour = guide_colourbar(barwidth = 16, title.position = "top")) +
    labs(x = "Distance along line (m)", y = "Pseudo-depth (m)", title = sprintf("%s to %s", n1, n2),
         caption = sprintf("%s quadrupoles in both surveys (of %s and %s), matched by electrode numbers. 100 x (second - first) / first.", ch$n_matched, ch$n_1, ch$n_2)) +
    theme_ovid(pal) + theme(legend.position = "bottom", plot.caption = element_text(colour = pal$fg4, size = 8, hjust = 0))
}

# Surveys given a project or a transect (config/surveys.csv; a new project or transect is added to its table). Returns the
# catalogue with the change, for cat_s(). transects: the transects table, or NULL when field is project.
assign_surveys <- function(sids, field, value, cat, transects = NULL) {
  cf <- load_configs(CFG)$surveys
  for (sid in setdiff(sids, cf$survey_id)) cf <- rbind(cf, ensure_cols(data.table(survey_id = sid), SURVEY_COLS)[, ..SURVEY_COLS])
  set(cf, which(cf$survey_id %in% sids), field, value); write_cfg(cf, CFG$project_dir, "surveys", who = "app")
  if (field == "project") { pj <- load_configs(CFG)$projects
    if (!value %in% pj$project) write_cfg(rbind(pj, ensure_cols(data.table(project = value), PROJECT_COLS)[, ..PROJECT_COLS]), CFG$project_dir, "projects", who = "app") }
  if (field == "transect_id" && !is.null(transects) && !value %in% transects$transect_id)
    write_cfg(rbind(transects, ensure_cols(data.table(transect_id = value), TRANSECT_COLS)[, ..TRANSECT_COLS]), CFG$project_dir, "transects", who = "app")
  x <- copy(cat); set(x, which(x$survey_id %in% sids), field, value); x
}
