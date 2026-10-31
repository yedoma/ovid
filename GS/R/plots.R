# R/plots.R  --  Preview plots for the catalogue: apparent-resistivity pseudosections (all, passed, flagged).
# Part of Ovid Core // GS. Sourced by gs_pipeline.R; not meant to be run on its own.
#
# Position: midpoint of the four electrodes along the line. Pseudo-depth: 0.19 x the distance between the outermost
# electrodes (a median depth of investigation in the sense of Edwards 1977, close for Wenner, Schlumberger and dipole-dipole).

RHOA_RANGE <- c(1, 10000)   # the default colour scale of apparent resistivity (ohm-m, log) in every view, so surveys compare

pseudo_xy <- function(d) {
  x <- with(d, cbind(ax, bx, mx, nx)); y <- with(d, cbind(ay, by, my, ny))
  s <- if (all(y == 0, na.rm = TRUE)) x else sqrt((x - x[1, 1])^2 + (y - y[1, 1])^2)   # along the line, also for x/y lines
  s[!is.finite(s)] <- NA                                                                   # remote electrodes
  data.table(px = rowMeans(s, na.rm = TRUE), pz = -0.19 * (apply(s, 1, max, na.rm = TRUE) - apply(s, 1, min, na.rm = TRUE)))
}

# A readable name for a survey: the name people gave it, else its file name (the name typed into the instrument goes in the
# subtitle when it differs); and the date people entered, else the first measurement's.
survey_label <- function(s1) {
  nm <- s1$display_name %||% ""; if (is.na(nm) || !nzchar(nm)) nm <- sub("\\.(stg|csv|txt|dat)$", "", basename(s1$primary_file), ignore.case = TRUE)
  d <- s1$acquired_date %||% ""; if (is.na(d) || !nzchar(d)) d <- substr(s1$datetime_start %||% "", 1, 10)   # a date people entered wins
  has <- !is.na(d) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)
  list(name = trim(nm), date = if (has) format(as.Date(d), "%d %b %Y") else "date unknown", file_date = if (has) d else "undated")
}

preview_file <- function(s1, used = character()) {
  l <- survey_label(s1); f <- paste0(l$file_date, "_", gsub("[^A-Za-z0-9._-]+", "_", l$name), "_pseudosection.png")
  if (f %in% used) f <- paste0(l$file_date, "_", gsub("[^A-Za-z0-9._-]+", "_", l$name), "_", sub("^ERT-", "", s1$survey_id), "_pseudosection.png")
  f
}

plot_pseudosection <- function(d, flags, s1, profile = "", pal = PAL) {
  if (!nrow(d)) return(NULL)
  p <- cbind(d[, .(meas_id, rhoa_ohmm)], pseudo_xy(d))
  fl <- flags[, .(flag = paste(unique(flag), collapse = ", ")), by = meas_id]
  p <- merge(p, fl, by = "meas_id", all.x = TRUE); p[, flagged := !is.na(flag)]
  p[, ok := !is.na(rhoa_ohmm) & rhoa_ohmm > 0]
  labs3 <- c(sprintf("All measurements (%s)", num(nrow(p))), sprintf("Passed QC (%s)", num(sum(!p$flagged))), sprintf("Flagged (%s)", num(sum(p$flagged))))
  both <- rbind(copy(p)[, panel := labs3[1]], copy(p)[!(flagged), panel := labs3[2]][!(flagged)], copy(p)[(flagged)][, panel := labs3[3]])
  both[, panel := factor(panel, labs3)]
  lim <- c(0, 4)                                                            # 1 to 10,000 ohm-m for every survey, so previews compare
  col <- function(x) pmin(pmax(log10(x), lim[1]), lim[2])
  l <- survey_label(s1)
  fld <- trim(s1$survey_name %||% ""); fld <- if (!is.na(fld) && nzchar(fld) && fld != l$name) paste0("recorded as ", fld)
  bits <- c(s1$model, fld, if (!is.na(s1$array %||% NA)) s1$array,
            if (!is.na(s1$n_electrodes %||% NA)) sprintf("%d electrodes at %g m", s1$n_electrodes, s1$spacing_m), dirname(sub("!/.*", "", s1$primary_file)))
  fc <- flags[, .(n = uniqueN(meas_id)), by = flag][order(-n)]
  ggplot(mapping = aes(px, pz)) +
    geom_point(data = both[ok & (!flagged | panel != labs3[1])], aes(colour = col(rhoa_ohmm)), size = 1.7) +
    geom_point(data = both[ok & flagged & panel == labs3[1]], colour = pal$gray, size = 1.2, alpha = 0.7) +   # flagged: grey in the overview
    geom_point(data = both[ok == FALSE], shape = 1, colour = pal$red, size = 1.8) +
    facet_wrap(~panel, ncol = 1, drop = FALSE) +
    scale_colour_gradientn(colours = grDevices::hcl.colors(5, "viridis"), name = "Apparent resistivity (\u03a9\u00b7m, log)",
                           limits = lim, breaks = function(x) seq(ceiling(x[1]), floor(x[2])),
                           labels = function(b) format(10^b, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    guides(colour = guide_colourbar(barwidth = 16, title.position = "top")) +
    labs(x = "Distance along line (m)", y = "Pseudo-depth (m)", title = paste0(l$name, "  \u00b7  ", l$date),
         subtitle = paste(bits, collapse = "  \u00b7  "),
         caption = paste0("QC profile: ", profile, if (nrow(fc)) paste0(" \u00b7 flags: ", paste(paste(fc$flag, fc$n), collapse = ", ")) else " \u00b7 nothing flagged",
                          "\nGrey: flagged (overview). Red circles: apparent resistivity 0 or below. Pseudo-depth = 0.19 x array length.  ovid id ", s1$survey_id)) +
    theme_ovid(pal) + theme(plot.caption = element_text(colour = pal$fg4, size = 11, hjust = 0))
}
