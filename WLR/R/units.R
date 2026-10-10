# R/units.R  --  Unit conversion to kPa, water density, kPa to metres of water, specific conductance.
# Part of Ovid Core // WLR. Sourced by wlr_pipeline.R; not meant to be run on its own.

G_STD <- 9.80665                                     # m/s2

# kPa per unit. "m" (and cm, ft) are pressure written as an equivalent column of water, the way Solinst
# reports LEVEL; its factor comes from settings (solinst_kpa_per_m, 9.80665 by default) so it can be adjusted.
kpa_factor <- function(unit, cfg) {
  u <- gsub("\\s+", "", tolower(trim(unit))); kpm <- cfg$solinst_kpa_per_m %||% 9.80665
  water <- c(m = kpm, cm = kpm / 100, mm = kpm / 1000, ft = kpm * 0.3048, `in` = kpm * 0.0254)   # a column of water
  if (u %in% names(water)) return(unname(water[u]))
  p <- unit_name(unit)                                                   # any other pressure unit: core convert.R
  if (is.na(p) || UNITS$kind[UNITS$unit == p] != "pressure") NA_real_ else convert_unit(1, p, "kPa")
}

# A Solinst Barologger writes air pressure as LEVEL less 9.5 m of water (so it reads about 0.3 to 1 m), where a
# Levelogger writes the whole pressure (about 10 m in air). Returns what to add, in the file's unit: 9.5 m when a
# barologger's readings in a length unit are far below a whole atmosphere, else 0.
solinst_baro_offset <- function(x, unit, cfg) {
  f <- kpa_factor(unit, cfg); kpm <- cfg$solinst_kpa_per_m %||% 9.80665
  if (is.na(f) || !tolower(trim(unit)) %in% c("m", "cm", "mm", "ft", "in")) return(0)
  med <- stats::median(x, na.rm = TRUE) * f
  if (is.na(med) || med > 40) return(0)
  9.5 * kpm / f
}

to_kpa <- function(x, unit, cfg) { f <- kpa_factor(unit, cfg); if (is.na(f)) stop("unknown pressure unit: ", unit); x * f }

# Density of fresh water (kg/m3) from temperature (deg C): Tanaka et al. (2001), valid 0 to 40 deg C;
# below 0 deg C (a logger in ice or supercooled water) the 0 deg C value is used.
water_density <- function(t_c) {
  t <- pmin(pmax(t_c, 0), 40)
  999.974950 * (1 - (t + -3.983035)^2 * (t + 301.797) / (522528.9 * (t + 69.34881)))
}

# Water above the sensor (m) from water pressure minus barometric pressure (kPa).
# density: "fixed" uses cfg$fixed_density (1000 kg/m3 by default); "temperature" uses the logger's own water temperature.
kpa_to_m <- function(dp_kpa, temp_c = NULL, cfg) {
  rho <- if (identical(cfg$density_mode, "temperature") && !is.null(temp_c)) {
    r <- water_density(temp_c); r[is.na(r)] <- cfg$fixed_density %||% 1000; r
  } else cfg$fixed_density %||% 1000
  dp_kpa * 1000 / (rho * (cfg$gravity %||% G_STD))
}

# Specific conductance at 25 deg C from actual conductivity, linear compensation (coefficient 0.0191 per deg C by default).
spc25 <- function(ec, temp_c, alpha = 0.0191) ec / (1 + alpha * (temp_c - 25))
