# R/reference.R  --  The Reference page: what each QC rule and profile does, what each statistic means, and how the
# common electrode arrays compare. Sourced by app.R only.
# Sources: Loke, M.H. (1999) Electrical imaging surveys for environmental and engineering studies (ERT/ref_manuals/Lokenote.pdf),
# the AGI EarthImager 2D manual (2014) and the SuperSting manual. Text is summarised in our own words; section numbers are given.

RULE_HELP <- data.table(
  rule = c("max_error_pct", "min_current_ma", "min_vp_mv", "min_abs_r_ohm", "rhoa_range", "negative_rhoa", "max_contact_ohm",
           "duplicates", "geometry", "outlier_mad", "reciprocal_pct"),
  name = c("Repeat error", "Injected current", "Measured voltage", "Resistance |V/I|", "Apparent resistivity range", "Negative apparent resistivity",
           "Contact resistance", "Repeated quadrupoles", "Geometry", "Outliers", "Reciprocal error"),
  what = c("The spread of the stacked repeat measurements, in percent (SuperSting: tenths of a percent in the file; Syscal: Dev.).",
           "The current driven into the ground through A and B.",
           "The potential difference measured between M and N.",
           "The measured resistance V/I, the quantity the instrument actually records.",
           "Apparent resistivity = geometric factor x V/I.",
           "A measurement whose apparent resistivity is zero or below.",
           "The resistance between the A and B electrodes and the ground, from the SuperSting .crs file.",
           "The same A, B, M, N combination measured more than once.",
           "Electrodes that coincide, a missing electrode, or an apparent resistivity that does not follow from the geometric factor and V/I.",
           "A value far from the rest of the survey: robust z-score of log apparent resistivity (distance from the median in median absolute deviations).",
           "The difference between a measurement and its reciprocal (current and potential electrodes swapped)."),
  why = c("Usually under 1 %; above 3 % often means a poorly planted electrode (EarthImager 3.5). EarthImager suggests removing data at 5-10 % (4.1.3).",
          "Below about 20 mA the data quality becomes shaky (EarthImager 3.5). Frozen ground and dry peat limit the current the instrument can drive.",
          "Below about 1 mV the signal is too small to measure accurately (EarthImager 4.1.1). Signal falls fast with large dipole-dipole n (Loke 2.5.2).",
          "Very small V/I is dominated by noise; 2 to 5 x 10^-4 ohm is recommended, never above 10^-3 (EarthImager 4.1.2).",
          "Values outside 0.1-1 to 10,000-100,000 ohm-m are rarely real for surface surveys (EarthImager 4.1.4-4.1.5). Ice-rich permafrost can be very resistive.",
          "Common in some layouts and hard to fit in an inversion; removed for surface surveys (EarthImager 4.1.7).",
          "Over 10,000 ohm is a clear sign of a poorly installed electrode; below 5,000 acceptable, below 2,000 ideal (EarthImager 3.5.9). The .crs value covers the pair, so it reads higher than one electrode.",
          "Repeats are averaged before inversion (EarthImager 4.1.3); flagging the later ones keeps one of each.",
          "A wrong geometry gives a wrong apparent resistivity and cannot be inverted.",
          "Isolated spikes usually come from a bad reading rather than the ground.",
          "Theory says both should be equal; the difference is a direct estimate of the noise (EarthImager 4.1.6)."))
RULE_HELP <- rbind(RULE_HELP, data.table(
  rule = c("moving_median", "bad_electrode", "electrode_off", "negative_ip", "magnitude_sd"),
  name = c("Moving median", "Bad electrode", "Electrode turned off", "Negative chargeability", "Extreme value (SD)"),
  what = c("Along each depth level, the departure of log10 apparent resistivity from the median of the 5 measurements around it, as a percentage of that median.",
           "The share of the measurements using an electrode that other rules flagged; above the limit, every measurement using it is flagged.",
           "An electrode listed as off in config/electrode_status.csv, for a survey or for a transect over a period.",
           "Chargeability (or phase) below zero, in surveys with induced polarisation.",
           "Apparent resistivity more than k standard deviations from the survey mean (value \"9\"; \"9;1\" uses log10 rho_a)."),
  why = c("Herring et al. (2023) use a 5-point moving median per depth level and 7 % as an example to test per survey; a smooth section should vary little between neighbours.",
          "A single bad electrode spoils every quadrupole it is in; the CPERS notebook flags an electrode above 25 %. Repeats and geometry are not counted.",
          "Measurements that use an electrode which was disconnected are not valid; the electrode is marked off in electrodes.csv.",
          "A negative chargeability usually means electromagnetic coupling or a bad reading rather than the ground (Herring et al. 2023 list it among the IP checks).",
          "The CPERS magnitude filter: Herring et al. (2023) remove values more than 9 SD from the mean. The paper does not say whether rho_a or log rho_a is used; check against your own surveys.")))

PROFILE_HELP <- c(
  none = "No filtering: every measurement is shown and counted.",
  exploratory = "A light touch for a first look or for poor legacy data: only clearly impossible values are flagged (error over 10 %, |V/I| under 10^-4 ohm, apparent resistivity outside 0.1-100,000 ohm-m, negative values, bad geometry). No current or contact limits.",
  frozen_ground = "The default for this archive. The manual limits of the standard profile, without the 20 mA current rule and with contact resistance allowed up to 50,000 ohm, because winter and permafrost surveys routinely read 20-35 kohm A-B contact and drive less current. Everything else (5 % error, 1 mV, |V/I|, 1-100,000 ohm-m, negatives, repeats, geometry, outliers) is as in standard.",
  standard = "The limits recommended in the EarthImager and SuperSting manuals, applied as written: 5 % error, 20 mA, 1 mV, 2 x 10^-4 ohm, 1-100,000 ohm-m, 10,000 ohm contact, negatives, repeats, geometry, outliers and 5 % reciprocal error. Suited to unfrozen ground with good electrode contact.",
  strict = "Tighter than standard for data going into an inversion: 3 % error, 5 x 10^-4 ohm, 5,000 ohm contact, outlier limit 4 and 3 % reciprocal error.",
  cpers = "The published filter sequence of the Canadian Permafrost Electrical Resistivity Survey database (Herring et al. 2023, 2024; after Rosset et al. 2013): negatives, stacking error over 10 %, values more than 9 SD from the mean, a 5-point moving median at 7 %, and electrodes with more than 20 % of their data removed. No current, voltage or contact limits. Use it to compare a survey with the CPERS archive. On average about 9 % of points are removed this way (Rosset et al. 2013).",
  supersting_r8 = "The manual limits for AGI SuperSting data only.",
  iris_syscal = "Limits for IRIS Instruments data only: 5 % Dev., 1 mV, negatives and geometry.")

STAT_HELP <- data.table(
  stat = c("Measurements", "Kept % / Kept / Flagged", "Records (stated / read)", "Apparent resistivity", "Resistance V/I", "Current", "Voltage",
           "Repeat error", "Contact resistance", "Negative values", "Electrodes", "Spacing", "Line length", "Unused positions", "Elevation range",
           "Array", "Geometric factor", "Pseudo-depth", "Quantiles, median, mean, SD"),
  meaning = c("Quadrupoles (A, B, M, N combinations) read from the file. Nothing is ever removed.",
              "Share and number of measurements that pass the chosen QC profile, and those flagged by at least one rule. Flagged values are kept and can be shown.",
              "The record count the instrument wrote in the header against the number read. Fewer read means the file is incomplete; more means records were added later.",
              "The resistivity a uniform ground would need to give the measured V/I with this electrode layout (Loke 1.2). It mixes all the ground the current passes through, so it is not the true resistivity at a depth. Drawn on 1-10,000 ohm-m in every view so surveys compare.",
              "What the instrument measures: voltage between M and N divided by the current between A and B (Loke 1.2).",
              "Current injected between A and B, in mA. Higher current gives a stronger, cleaner signal.",
              "Potential measured between M and N, in mV. Signal strength falls as the potential electrodes move away from the current electrodes.",
              "The scatter of the stacked repeats of each reading; a direct measure of noise.",
              "A-B pair resistance through the ground at each current injection, a guide to how well the electrodes are planted.",
              "Measurements with apparent resistivity of zero or below; usually poor contact or noise in surface surveys.",
              "Number of distinct electrode positions used.",
              "The usual distance between neighbouring electrodes (the most common step).",
              "Distance along the line from the first to the last electrode used.",
              "Positions on the regular spacing that no measurement used (skipped or disconnected electrodes).",
              "Highest minus lowest electrode elevation, from the file or from elevations added on the Geometry tab.",
              "Wenner, Schlumberger, dipole-dipole, pole-dipole, pole-pole or mixed, worked out from the electrode numbers of each measurement.",
              "k = 2 pi / (1/AM - 1/BM - 1/AN + 1/BN): converts V/I to apparent resistivity for the electrode layout (Loke 1.2, Figure 2).",
              "Where a measurement is plotted: below the centre of its four electrodes at 0.19 x the array length, close to the median depth of investigation for Wenner, Wenner-Schlumberger and dipole-dipole (Edwards 1977; Loke 2.3 and Table 2). The pseudosection is a plotting convention, not an image of the ground (Loke 2.3).",
              "Distribution summaries in survey_stats.csv, for all measurements and for those passing QC: q05/q95 = 5th/95th percentiles, q25/q75 = quartiles, SD = standard deviation. Medians are used in the catalogue because resistivity spans orders of magnitude (Loke 1.3)."))

ARRAY_HELP <- list(
  list(name = "Wenner", layout = "A, M, N, B equally spaced (a, a, a). Total length L = 3a.", depth = "Median depth 0.52 a, or 0.17 L.",
       pros = c("Strongest signal of the common arrays (smallest geometric factor, 2 pi a), so it copes best with noisy sites.",
                "Good at resolving vertical changes, i.e. horizontal layers such as an active layer over permafrost."),
       cons = c("Poor at detecting narrow vertical structures (horizontal changes).", "Horizontal coverage shrinks quickly with depth: each deeper level loses three data points."),
       use = "Noisy areas, layered ground, limited survey time.", src = "Loke 2.5.1, Table 2"),
  list(name = "Dipole-dipole", layout = "Current pair B-A, then potential pair M-N, both with spacing a, separated by n x a.", depth = "Median depth 0.42 a (n = 1) to 1.73 a (n = 6); 0.14-0.22 L.",
       pros = c("Very sensitive to horizontal changes: good for vertical structures such as dykes, cavities, ice wedges or thaw channels.",
                "Better horizontal coverage than Wenner; low EM coupling, so it is widely used for IP."),
       cons = c("Signal falls with the cube of n: about 200 times weaker at n = 6 than at n = 1.", "Shallower than Wenner for the same length; needs a sensitive meter and good contact.",
                "The plotted point lies where the array is least sensitive, so the pseudosection is especially distorted."),
       use = "Good horizontal resolution where the instrument is sensitive and contact is good. Increase a rather than n past about 6.", src = "Loke 2.5.2, Table 2"),
  list(name = "Wenner-Schlumberger", layout = "A ... M N ... B with the M-N pair (a) in the middle and n x a to each current electrode. Wenner is the case n = 1.", depth = "Median depth about 10 % deeper than Wenner for the same outer spacing; 0.17-0.19 L.",
       pros = c("Moderately sensitive to both horizontal and vertical structures: a compromise between Wenner and dipole-dipole.",
                "Slightly better coverage than Wenner (two points lost per deeper level instead of three); stronger signal than dipole-dipole."),
       cons = c("Weaker signal than Wenner.", "Narrower coverage than dipole-dipole."),
       use = "When unsure, or when both vertical and horizontal resolution matter, ideally with overlapping data levels.", src = "Loke 2.5.3, 2.5.6"),
  list(name = "Pole-dipole", layout = "One current electrode A on the line with B far away (remote), and a potential pair M-N at n x a.", depth = "Median depth 0.52 a (n = 1) to 2.48 a (n = 6).",
       pros = c("Good horizontal coverage, useful with few electrodes.", "Stronger signal than dipole-dipole and much less telluric noise than pole-pole; attractive for IP."),
       cons = c("Asymmetric: anomalies look lopsided unless forward and reverse readings are combined.", "Needs a remote electrode more than 5 x the largest A-M distance away for under 5 % error.",
                "Signal falls with the square of n; avoid n above 8-10."),
       use = "Systems with a limited number of electrodes, measured forward and reverse.", src = "Loke 2.5.5"),
  list(name = "Pole-pole", layout = "One current electrode A and one potential electrode M on the line; B and N remote.", depth = "Median depth 0.87 a, the deepest of the common arrays.",
       pros = c("Widest horizontal coverage and deepest investigation.", "Simple; popular for small spacings, e.g. archaeology and some 3-D surveys."),
       cons = c("Poorest resolution.", "Remote electrodes must be more than 20 x the largest A-M distance away for under 5 % error, which is hard in the field.",
                "The long M-N distance picks up a lot of telluric noise."),
       use = "Small electrode spacings (under about 10 m) where coverage matters more than resolution.", src = "Loke 2.5.4"))

# ---- diagrams (inline SVG, drawn in the app's colours) ---------------------------------------------------------------------
# An array: electrodes at positions x (in spacings) with labels; "inf" marks a remote electrode. Current pair red, potential pair blue.
array_svg <- function(el, spans) {
  W <- 300; x0 <- 26; dx <- 42; y <- 52
  X <- function(v) x0 + v * dx
  col <- function(l) if (l %in% c("A", "B")) PAL$red else PAL$blue
  parts <- c(sprintf("<line x1='8' y1='%d' x2='%d' y2='%d' stroke='%s' stroke-width='2'/>", y, W - 8, y, PAL$fg4),
    unlist(lapply(seq_len(nrow(el)), function(i) { e <- el[i]; if (is.na(e$x)) {
        sprintf("<text x='%d' y='%d' fill='%s' font-size='12' text-anchor='end'>%s at infinity &#8594;</text>", W - 8, y + (if (e$l %in% c("A", "B")) -32 else 36), col(e$l), e$l) } else
        c(sprintf("<line x1='%f' y1='%d' x2='%f' y2='%d' stroke='%s' stroke-width='3'/>", X(e$x), y - 14, X(e$x), y + 6, col(e$l)),
          sprintf("<text x='%f' y='%d' fill='%s' font-size='13' font-weight='600' text-anchor='middle'>%s</text>", X(e$x), y - 20, col(e$l), e$l)) })),
    unlist(lapply(spans, function(s) c(sprintf("<line x1='%f' y1='%d' x2='%f' y2='%d' stroke='%s' stroke-width='1' marker-start='url(#ar)' marker-end='url(#ar)'/>", X(s$from) + 3, y + 20, X(s$to) - 3, y + 20, PAL$fg4),
                                      sprintf("<text x='%f' y='%d' fill='%s' font-size='12' text-anchor='middle'>%s</text>", (X(s$from) + X(s$to)) / 2, y + 34, PAL$fg4, s$lab)))))
  HTML(sprintf("<svg viewBox='0 0 %d 96' width='100%%' style='max-width:320px'><defs><marker id='ar' markerWidth='6' markerHeight='6' refX='3' refY='3' orient='auto-start-reverse'><path d='M0,0 L6,3 L0,6 z' fill='%s'/></marker></defs>%s</svg>",
               W, PAL$fg4, paste(parts, collapse = "")))
}
sp <- function(from, to, lab) list(from = from, to = to, lab = lab)
ARRAY_SVG <- list(
  Wenner = array_svg(data.table(l = c("A", "M", "N", "B"), x = c(0, 2, 4, 6) * 0.95), list(sp(0, 1.9, "a"), sp(1.9, 3.8, "a"), sp(3.8, 5.7, "a"))),
  `Dipole-dipole` = array_svg(data.table(l = c("B", "A", "M", "N"), x = c(0, 1.3, 3.9, 5.2) * 1.1), list(sp(0, 1.43, "a"), sp(1.43, 4.29, "n a"), sp(4.29, 5.72, "a"))),
  `Wenner-Schlumberger` = array_svg(data.table(l = c("A", "M", "N", "B"), x = c(0, 2.3, 3.4, 5.7)), list(sp(0, 2.3, "n a"), sp(2.3, 3.4, "a"), sp(3.4, 5.7, "n a"))),
  `Pole-dipole` = array_svg(data.table(l = c("A", "M", "N", "B"), x = c(0, 2.6, 3.9, NA)), list(sp(0, 2.6, "n a"), sp(2.6, 3.9, "a"))),
  `Pole-pole` = array_svg(data.table(l = c("A", "M", "B", "N"), x = c(0, 1.8, NA, NA)), list(sp(0, 1.8, "a"))))

# How Ovid works: a row of steps with arrows.
flow_svg <- function(steps, notes) {
  n <- length(steps); w <- 150; gap <- 26; W <- n * w + (n - 1) * gap
  box <- function(i) { x <- (i - 1) * (w + gap)
    c(sprintf("<rect x='%d' y='6' width='%d' height='62' rx='8' fill='%s' stroke='%s'/>", x, w, PAL$bg1, if (i %in% c(1, n)) PAL$accent else PAL$bg2),
      sprintf("<text x='%d' y='32' fill='%s' font-size='13' font-weight='600' text-anchor='middle'>%s</text>", x + w / 2, PAL$fg, steps[i]),
      sprintf("<text x='%d' y='52' fill='%s' font-size='11' text-anchor='middle'>%s</text>", x + w / 2, PAL$fg4, notes[i]),
      if (i < n) sprintf("<path d='M%d,37 L%d,37' stroke='%s' stroke-width='2' marker-end='url(#fa)'/>", x + w + 3, x + w + gap - 3, PAL$accent)) }
  HTML(sprintf("<svg viewBox='0 0 %d 74' width='100%%' style='max-width:%dpx'><defs><marker id='fa' markerWidth='8' markerHeight='8' refX='6' refY='4' orient='auto'><path d='M0,0 L8,4 L0,8 z' fill='%s'/></marker></defs>%s</svg>",
               W, W, PAL$accent, paste(unlist(lapply(seq_len(n), box)), collapse = "")))
}

# Where a measurement is plotted: midpoint of its electrodes, at 0.19 x the array length below the surface.
pseudo_svg <- function() HTML(sprintf(paste0("<svg viewBox='0 0 640 175' width='100%%' style='max-width:700px'>",
  "<line x1='10' y1='30' x2='410' y2='30' stroke='%1$s' stroke-width='2'/>",
  "<g font-size='13' font-weight='600' text-anchor='middle'><text x='60' y='20' fill='%2$s'>A</text><text x='160' y='20' fill='%3$s'>M</text><text x='260' y='20' fill='%3$s'>N</text><text x='360' y='20' fill='%2$s'>B</text></g>",
  "<g stroke-width='3'><line x1='60' y1='24' x2='60' y2='38' stroke='%2$s'/><line x1='160' y1='24' x2='160' y2='38' stroke='%3$s'/><line x1='260' y1='24' x2='260' y2='38' stroke='%3$s'/><line x1='360' y1='24' x2='360' y2='38' stroke='%2$s'/></g>",
  "<path d='M60,30 Q210,190 360,30' fill='none' stroke='%2$s' stroke-dasharray='4 4' opacity='.6'/>",
  "<line x1='210' y1='30' x2='210' y2='87' stroke='%1$s' stroke-dasharray='3 3'/><circle cx='210' cy='87' r='7' fill='%4$s'/>",
  "<text x='222' y='64' fill='%1$s' font-size='12'>0.19 &#215; L</text><text x='222' y='92' fill='%5$s' font-size='12'>plotted here: apparent resistivity = k &#215; V/I</text>",
  "<line x1='60' y1='120' x2='360' y2='120' stroke='%1$s' marker-start='url(#pa)' marker-end='url(#pa)'/><text x='210' y='138' fill='%1$s' font-size='12' text-anchor='middle'>array length L</text>",
  "<text x='10' y='165' fill='%1$s' font-size='11'>dashed: current flows from A to B through the ground; M and N measure the voltage it creates</text>",
  "<defs><marker id='pa' markerWidth='6' markerHeight='6' refX='3' refY='3' orient='auto-start-reverse'><path d='M0,0 L6,3 L0,6 z' fill='%1$s'/></marker></defs></svg>"),
  PAL$fg4, PAL$red, PAL$blue, PAL$gold, PAL$fg))

reference_ui <- function(qc) {
  prof <- intersect(c("exploratory", "frozen_ground", "standard", "strict", "cpers"), unique(qc$profile))
  cmp <- dcast(qc[profile %in% prof & rule != "none" & applies_to %in% c("all", "agi")], rule ~ profile, value.var = "value", fun.aggregate = function(x) if (length(x)) ifelse(x[1] == "", "on", x[1]) else "-", fill = "-")
  cmp <- merge(cmp, RULE_HELP[, .(rule, name)], by = "rule", all.x = TRUE)[order(match(rule, RULE_HELP$rule))]
  card_list <- function(items) tags$ul(class = "mb-1", lapply(items, tags$li))
  navset_card_pill(id = "ref_tab",
    nav_panel("How it works",
      h5("From raw files to a catalogue"),
      flow_svg(c("Raw files", "Identify", "Read", "Catalogue", "QC flags", "View & export"),
               c("never changed", "instrument & format", "common model", "metadata, stats", "nothing deleted", "plots, zips")),
      p(class = "muted mt-2", "Every file in the archive is listed. Instrument files are read into one model shared by AGI and IRIS data; the instrument's own fields are kept beside it. QC only adds flags, so any profile can be applied, compared or undone."),
      h5(class = "mt-4", "From QC limits to what you see"),
      flow_svg(c("Profile", "Rules", "Flags", "Views & stats"), c("set of limits", "one per limit", "per measurement", "passed / flagged")),
      h5(class = "mt-4", "How a measurement becomes a point on the pseudosection"),
      pseudo_svg(),
      p(class = "muted", "The instrument drives a current between A and B and measures the voltage between M and N. V/I times the geometric factor k of the layout gives the apparent resistivity, which is plotted below the centre of the four electrodes at the median depth of investigation (Loke 1.2, 2.3; Edwards 1977). Wider layouts see deeper.")),
    nav_panel("QC profiles",
      p(class = "muted", "A profile is a set of limits. Measurements that break a limit are flagged, never deleted, and every profile is computed so you can compare. Move the sliders on the QC page to try your own and save them as a new profile."),
      div(class = "facts", lapply(names(PROFILE_HELP)[names(PROFILE_HELP) %in% unique(qc$profile)], function(k)
        div(class = "fact-group", div(class = "fact-title", gsub("_", " ", k)), p(class = "mb-0", style = "font-size:.9rem", PROFILE_HELP[[k]])))),
      h5(class = "mt-4", "The main profiles side by side"),
      tags$table(class = "table table-sm", style = "font-size:.9rem",
        tags$thead(tags$tr(tags$th("Rule"), lapply(prof, function(x) tags$th(gsub("_", " ", x))))),
        tags$tbody(lapply(seq_len(nrow(cmp)), function(i) tags$tr(tags$td(cmp$name[i] %||% cmp$rule[i]), lapply(prof, function(x) tags$td(cmp[[x]][i])))))),
      p(class = "muted", "Units: error %, current mA, voltage mV, |V/I| ohm, resistivity ohm-m (min;max), contact ohm. '-' = not used.")),
    nav_panel("QC rules",
      div(class = "facts", lapply(seq_len(nrow(RULE_HELP)), function(i) div(class = "fact-group",
        div(class = "fact-title", RULE_HELP$name[i]), p(style = "font-size:.9rem", RULE_HELP$what[i]), p(class = "muted mb-0", RULE_HELP$why[i]))))),
    nav_panel("Statistics",
      div(class = "facts", lapply(seq_len(nrow(STAT_HELP)), function(i) div(class = "fact-group",
        div(class = "fact-title", STAT_HELP$stat[i]), p(class = "mb-0", style = "font-size:.9rem", STAT_HELP$meaning[i]))))),
    nav_panel("Arrays",
      p(class = "muted", span(style = paste0("color:", PAL$red), "A, B: current electrodes. "), span(style = paste0("color:", PAL$blue), "M, N: potential electrodes. "), "The best array depends on the structure to be mapped, the sensitivity of the meter and the background noise (Loke 2.5). Depths are median depths of investigation for a uniform ground (Edwards 1977, Loke Table 2); multiply by the largest a or L used."),
      div(class = "facts", lapply(ARRAY_HELP, function(a) div(class = "fact-group",
        div(class = "fact-title", a$name), ARRAY_SVG[[a$name]], p(style = "font-size:.88rem", a$layout, br(), span(class = "muted", a$depth)),
        strong("Strengths"), card_list(a$pros), strong("Weaknesses"), card_list(a$cons),
        p(class = "mb-0 mt-1", style = "font-size:.88rem", strong("Use it for: "), a$use), p(class = "muted mb-0", a$src)))),
      p(class = "muted mt-3", "Loke's summary (2.5.7): Wenner for noisy sites and vertical resolution; dipole-dipole for horizontal resolution with a sensitive meter and good contact; Wenner-Schlumberger with overlapping levels when unsure; pole-dipole forward and reverse with few electrodes; pole-pole for small spacings.")),
    nav_panel("Sources",
      tags$ul(tags$li("Loke, M.H. (1999). Electrical imaging surveys for environmental and engineering studies: a practical guide to 2-D and 3-D surveys. (ref_manuals/Lokenote.pdf)"),
              tags$li("Advanced Geosciences Inc. (2014). Instruction manual for EarthImager 2D. (ref_manuals/EarthImager2DManual.pdf)"),
              tags$li("Advanced Geosciences Inc. SuperSting instruction manual. (ref_manuals/SuperSting Manual.pdf)"),
              tags$li("Edwards, L.S. (1977). A modified pseudosection for resistivity and IP. Geophysics 42, 1020-1036. (median depth of investigation, as cited by Loke)"),
              tags$li("Herring, T., Lewkowicz, A.G., Hauck, C., Hilbich, C., Mollaret, C., et al. (2023). Best practices for using electrical resistivity tomography to investigate permafrost. Permafrost and Periglacial Processes 34. doi:10.1002/ppp.2207 (moving median, magnitude, stacking and bad-electrode filters)"),
              tags$li("Herring, T., Lewkowicz, A.G., et al. (2024). The Canadian Permafrost Electrical Resistivity Survey (CPERS) database. Arctic Science 10. doi:10.1139/as-2023-0058 (the cpers profile)"),
              tags$li("Rosset, E., Hilbich, C., Schneider, S., Hauck, C. (2013). Automatic filtering of ERT monitoring data in mountain permafrost. Near Surface Geophysics 11. doi:10.3997/1873-0604.2013003"))))
}
