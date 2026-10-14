# R/config.R  --  Set-up tables: the palette, the metadata people edit, quality classes and QC profiles.
# Part of Ovid Core // GS. Sourced by gs_pipeline.R after the shared core; not meant to be run on its own.
#
# Everything people decide lives in <project>/config/*.csv. Values read from the raw files go to the catalogue;
# a non-empty cell here overrides them, and the catalogue keeps both.


# kanagawa.nvim (vimcolorschemes, default "wave" variant; see core/theme.R): sumi-ink ground, fuji-white text and the
# woodblock blues, greens and carp yellow for data; the accent is its oni violet.
# Settings > Colours offers these schemes (M, 2026-10-08; OVID_ALT_SCHEMES in core theme.R); the first is GS's own,
# and in the others the accent is the scheme's purple. APP_SCHEME is the one picked (app_settings.json), read at the start.
APP_SCHEMES <- c("Kanagawa", "Nightfox", "Dracula", "Falcon", "Tomorrow Night", "Benlight")
APP_SCHEME <- saved_scheme(APP_SCHEMES)
app_palette <- function(scheme = APP_SCHEMES[1]) {
  p <- local({ S <- OVID_SCHEMES$kanagawa; n <- S$named
    list(bg = S$bg, bg1 = n[["sumiInk4"]], bg2 = n[["sumiInk5"]], fg = S$fg, fg4 = mix_col(S$fg, n[["fujiGray"]], 0.35), gray = n[["fujiGray"]],
         accent = n[["oniViolet"]], blue = n[["crystalBlue"]], teal = n[["waveAqua2"]], green = n[["springGreen"]], gold = n[["carpYellow"]],
         orange = n[["surimiOrange"]], red = n[["waveRed"]], purple = n[["sakuraPink"]]) })
  p <- scheme_pick(p, scheme, "purple", APP_SCHEMES)
  p <- legible(p, c("fg4", "accent", "red", "green", "gold", "blue"), 4.5, on = c(p$bg, p$bg1)); p <- legible(p, "gray", 3)
  p }
PAL <- app_palette(APP_SCHEME)

PROJECT_COLS  <- c("project", "code", "year", "team", "organization", "purpose", "study_area", "notes")
SITE_COLS     <- c("site", "project", "latitude", "longitude", "region", "environment", "access", "infrastructure", "photos", "notes")
TRANSECT_COLS <- c("transect_id", "site", "name", "start_lat", "start_lon", "end_lat", "end_lon", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "crs", "orientation_deg",
                   "length_m", "spacing_m", "n_electrodes", "array", "method", "terrain", "ground", "notes")
SURVEY_COLS   <- c("survey_id", "display_name", "acquired_date", "instrument_model", "project", "site", "transect_id", "operator", "conditions", "ground_conditions",
                   "notes", "quality", "quality_reasons", "quality_notes", "exclude_from_archive",
                   # reporting (Herring et al. 2023, Table 1; CPERS): how the electrodes were set, and how the data were inverted
                   "electrode_contact", "inversion_software", "inversion_parameters", "inversion_rms_pct")
QC_COLS       <- c("profile", "applies_to", "rule", "value", "flag", "enabled", "notes", "rule_id")
MANUAL_COLS   <- c("survey_id", "meas_ids", "electrode", "flag", "active", "reason", "author", "date")
ELECTRODE_COLS <- c("survey_id", "transect_id", "electrode", "status", "from", "to", "reason", "author")   # status off: the electrode was turned off
FILE_RULE_COLS <- c("pattern", "action", "survey_id", "reason")
ELEV_COLS     <- c("survey_id", "electrode", "distance_m", "elevation_m", "source", "author")   # by electrode number, or by distance along the line

QUALITY_CLASSES <- c("Good", "Usable", "Usable with caution", "Poor", "Unusable", "Incomplete", "Unknown")
QUALITY_REASONS <- c("poor electrode contact", "high contact resistance", "insufficient injected current", "high measurement error",
                     "cable problem", "instrument problem", "geometry problem", "incomplete acquisition", "missing coordinates",
                     "incorrect electrode positions", "environmental interference", "aborted survey", "unknown issue", "other")
FLAG_REASONS <- c("high error", "low current", "extreme apparent resistivity", "poor contact", "suspected electrode issue",
                  "bad geometry", "duplicate", "manually excluded", "uncertain")

# Limits from the manuals in ERT/ref_manuals: EarthImager 2D manual (2014) 3.5 and 4.1, SuperSting manual 5.1.6 and 7.3-7.4.
#   repeat error: usually < 1 %, > 3 % suggests poor contact; remove at 5-10 % (EI 3.5, 4.1.3). SuperSting stacks to 2 % (SS 5.1.6.1.2).
#   current: below 20 mA data quality becomes shaky (EI 3.5).      voltage: below 1 mV too small to measure accurately (EI 4.1.1).
#   |V/I|: 1e-4 ohm is safe, 2e-4 to 5e-4 recommended, never above 1e-3 (EI 4.1.2).
#   apparent resistivity: min 0.1-1 ohm-m, max 1e4-1e5 ohm-m (EI 4.1.4-4.1.5); negative values removed for surface surveys (EI 4.1.7).
#   contact: over 10,000 ohm a clear sign of a poorly installed electrode; acceptable below 5,000, ideally below 2,000 (EI 3.5.9).
#   The .crs value is the resistance of the A-B pair through the ground (SS 7.4), so it reads higher than one electrode alone.
default_qc <- function() qc_rule_ids(fread(sep = ",", colClasses = "character", na.strings = NULL, text = "
profile,applies_to,rule,value,flag,enabled,notes
none,all,none,,,TRUE,no filtering
exploratory,all,max_error_pct,10,high error,TRUE,EI 4.1.3 upper end
exploratory,all,min_abs_r_ohm,0.0001,low voltage,TRUE,EI 4.1.2 safe setting
exploratory,all,rhoa_range,0.1;100000,extreme apparent resistivity,TRUE,EI 4.1.4-4.1.5
exploratory,all,negative_rhoa,,negative apparent resistivity,TRUE,EI 4.1.7
exploratory,all,geometry,,bad geometry,TRUE,
standard,all,max_error_pct,5,high error,TRUE,EI 4.1.3
standard,all,min_current_ma,20,low current,TRUE,EI 3.5: below 20 mA quality becomes shaky
standard,all,min_vp_mv,1,low voltage,TRUE,EI 4.1.1
standard,all,min_abs_r_ohm,0.0002,low voltage,TRUE,EI 4.1.2 recommended 2e-4 to 5e-4
standard,all,rhoa_range,1;100000,extreme apparent resistivity,TRUE,EI 4.1.4-4.1.5
standard,all,negative_rhoa,,negative apparent resistivity,TRUE,EI 4.1.7
standard,all,duplicates,,duplicate,TRUE,EI 4.1.3: duplicates are averaged before inversion
standard,all,geometry,,bad geometry,TRUE,
standard,all,outlier_mad,6,uncertain,TRUE,robust z of log rhoa against the survey
standard,all,reciprocal_pct,5,high error,TRUE,
standard,agi,max_contact_ohm,10000,poor contact,TRUE,EI 3.5.9: over 10000 ohm a clear sign of a poorly installed electrode
strict,all,max_error_pct,3,high error,TRUE,EI 3.5: above 3 % likely poor contact
strict,all,min_current_ma,20,low current,TRUE,EI 3.5
strict,all,min_vp_mv,1,low voltage,TRUE,EI 4.1.1
strict,all,min_abs_r_ohm,0.0005,low voltage,TRUE,EI 4.1.2
strict,all,rhoa_range,1;100000,extreme apparent resistivity,TRUE,EI 4.1.4-4.1.5
strict,all,negative_rhoa,,negative apparent resistivity,TRUE,EI 4.1.7
strict,all,duplicates,,duplicate,TRUE,
strict,all,geometry,,bad geometry,TRUE,
strict,all,outlier_mad,4,uncertain,TRUE,
strict,all,reciprocal_pct,3,high error,TRUE,
strict,agi,max_contact_ohm,5000,poor contact,TRUE,EI 3.5.9: acceptable below 5000 ohm
supersting_r8,agi,max_error_pct,5,high error,TRUE,EI 4.1.3; SuperSting error is in tenths of a percent (SS 7.3)
supersting_r8,agi,min_current_ma,20,low current,TRUE,EI 3.5
supersting_r8,agi,min_vp_mv,1,low voltage,TRUE,EI 4.1.1
supersting_r8,agi,min_abs_r_ohm,0.0002,low voltage,TRUE,EI 4.1.2
supersting_r8,agi,max_contact_ohm,10000,poor contact,TRUE,EI 3.5.9
supersting_r8,agi,negative_rhoa,,negative apparent resistivity,TRUE,
supersting_r8,agi,geometry,,bad geometry,TRUE,
frozen_ground,all,max_error_pct,5,high error,TRUE,standard with a contact limit for frozen ground and peat
frozen_ground,all,min_vp_mv,1,low voltage,TRUE,
frozen_ground,all,min_abs_r_ohm,0.0002,low voltage,TRUE,
frozen_ground,all,rhoa_range,1;100000,extreme apparent resistivity,TRUE,
frozen_ground,all,negative_rhoa,,negative apparent resistivity,TRUE,
frozen_ground,all,duplicates,,duplicate,TRUE,
frozen_ground,all,geometry,,bad geometry,TRUE,
frozen_ground,all,outlier_mad,6,uncertain,TRUE,
frozen_ground,agi,max_contact_ohm,50000,poor contact,TRUE,A-B pair resistance; winter permafrost surveys have medians of 20-35 kohm
iris_syscal,syscal,max_error_pct,5,high error,TRUE,IRIS Dev. (standard deviation of the stack) in percent
iris_syscal,syscal,min_vp_mv,1,low voltage,TRUE,
iris_syscal,syscal,negative_rhoa,,negative apparent resistivity,TRUE,
iris_syscal,syscal,geometry,,bad geometry,TRUE,
standard,all,moving_median,5;7,uncertain,TRUE,Herring et al. 2023: 5-point moving median per depth level and 7 % (an example to test per survey)
standard,all,bad_electrode,25,bad electrode,TRUE,CPERS: an electrode with more than 25 % of its measurements flagged
strict,all,moving_median,5;5,uncertain,TRUE,
strict,all,bad_electrode,20,bad electrode,TRUE,
frozen_ground,all,moving_median,5;7,uncertain,TRUE,Herring et al. 2023
frozen_ground,all,bad_electrode,25,bad electrode,TRUE,CPERS
cpers,all,negative_rhoa,,negative apparent resistivity,TRUE,Herring et al. 2023 technical filter: negative current / voltage / rho_a
cpers,all,max_error_pct,10,high error,TRUE,Herring et al. 2023: stacking variance above 10 %
cpers,all,duplicates,,duplicate,TRUE,
cpers,all,geometry,,bad geometry,TRUE,
cpers,all,magnitude_sd,9,extreme apparent resistivity,TRUE,Herring et al. 2023: more than 9 SD from the mean
cpers,all,moving_median,5;7,uncertain,TRUE,Herring et al. 2023: 5-point moving median per depth level and 7 %
cpers,all,bad_electrode,20;10,bad electrode,TRUE,Herring et al. 2023: electrode with more than 20 % of its data removed
"), "Q")

CONFIG_FILES <- list(
  projects   = list(file = "projects.csv",     cols = PROJECT_COLS,   label = "Projects"),
  sites      = list(file = "sites.csv",        cols = SITE_COLS,      label = "Sites"),
  transects  = list(file = "transects.csv",    cols = TRANSECT_COLS,  label = "Transects"),
  surveys    = list(file = "surveys.csv",      cols = SURVEY_COLS,    label = "Surveys"),
  qc         = list(file = "qc_profiles.csv",  cols = QC_COLS,        label = "QC profiles", default = default_qc, rule_ids = "Q"),
  manual     = list(file = "manual_flags.csv", cols = MANUAL_COLS,    label = "Manual flags"),
  electrodes = list(file = "electrode_status.csv", cols = ELECTRODE_COLS, label = "Electrodes turned off"),
  file_rules = list(file = "file_rules.csv",   cols = FILE_RULE_COLS, label = "File rules"),
  elevations = list(file = "elevations.csv",   cols = ELEV_COLS,      label = "Electrode elevations"),
  timezones  = list(file = "timezones.csv",    cols = TZ_COLS,        label = "Time zones"))   # Ovid/timezones.R: instrument clocks
