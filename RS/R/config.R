# R/config.R  --  Set-up tables: the palette, the sources that make up the archive, and the metadata people edit.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R after the shared core; not meant to be run on its own.
#
# Everything people decide lives in <project>/config/*.csv. Values read from the raw files go to the catalogue;
# a non-empty cell here overrides them, and the catalogue keeps both (the found value in a *_found column).


# accent.vim (vimcolorschemes, dark; see core/theme.R): a plain slate ground and grey text with one accent colour; its
# default accent, yellow, is RS's. Its other six colours are used for data.
SCHEME <- OVID_SCHEMES$accent
# Settings > Colours offers these schemes (M, 2026-10-08; OVID_ALT_SCHEMES in core theme.R); the first is RS's own,
# and in the others the accent is the scheme's yellow. APP_SCHEME is the one picked (app_settings.json), read at the start.
APP_SCHEMES <- c("Accent", "Miasma", "Afterglow", "Tomorrow Night", "Benlight")
APP_SCHEME <- saved_scheme(APP_SCHEMES)
app_palette <- function(scheme = APP_SCHEMES[1]) {
  p <- local({ n <- SCHEME$named; bg <- SCHEME$bg
    list(bg = bg, bg1 = n[["bg_b1"]], bg2 = n[["bg_b2"]], bg3 = n[["fg_d2"]], fg = SCHEME$fg, fg4 = n[["fg_d1"]], gray = n[["fg_d2"]],
         bone = SCHEME$ansi[16], teal = n[["cyan"]], green = n[["green"]], blue = n[["blue"]], red = n[["red"]], gold = n[["orange"]],
         purple = n[["magenta"]], accent = n[["yellow"]]) })
  p <- scheme_pick(p, scheme, "yellow", APP_SCHEMES)
  p <- legible(p, c("fg4", "accent", "teal", "red", "gold", "green", "blue"), 4.5, on = c(p$bg, p$bg1)); p <- legible(p, "gray", 3)
  p }
PAL <- app_palette(APP_SCHEME)

# The archive is built from sources scanned one after another (a drive, a folder, a backup): each is one row here.
# source_id is a short name used in cache and catalogue paths; path is where the source is mounted on this computer.
SOURCE_COLS    <- c("source_id", "path", "label", "notes")
FLIGHT_COLS    <- c("flight_id", "display_name", "project", "site", "positioning_status", "status_note", "notes", "exclude_from_archive", "mission_set",
                    "panel_id", "panel_capture",      # multispectral: the calibration panel (serial or ID) and when it was imaged
                    "kind")                           # flight or photo set (blank: as found, by photoset_max_images)
SITE_COLS      <- c("site", "project", "latitude", "longitude", "region", "notes")
# Site names found in folder names (found) and the known site each means (site); a blank site says the name is not a site
# (a band folder such as msp, a person's name). The scan applies them (site_apply_names); the Timeline's Site names card writes them.
SITE_NAME_COLS <- c("found", "site", "note")
FILE_RULE_COLS <- c("pattern", "action", "reason")     # action: ignore (the file stays listed) or a role from FORMATS
LINK_COLS      <- c("flight_id", "file", "state", "reason", "author", "date")   # state: confirmed or rejected; file = source_id:rel
PRODUCT_COLS   <- c("product_id", "kind", "file", "flights", "software", "version", "parameters", "gcps", "checkpoints", "rmse_h_m", "rmse_v_m",
                    "epsg", "frame", "epoch", "height_type", "geoid_model", "level", "made", "author", "notes")   # R/stac.R: products made from the flights

# Positioning states (design doc: eBee RTK/PPK status). DJI flights use the same states, read from each image's RTK flag.
POSITIONING <- c("RTK corrected", "partly RTK corrected", "PPK corrected", "standalone GNSS / uncorrected",
                 "correction data available, not confirmed", "unknown")
# One colour per state, all from accent.vim and legible on its ground: RTK green, partly RTK cyan, PPK blue,
# uncorrected red, "available, not confirmed" orange, unknown grey. Line style, not colour, tells planned from observed.
STATUS_COL <- distinct_cols(setNames(c(PAL$green, PAL$teal, PAL$blue, PAL$red, PAL$gold, legible_col(PAL$gray, PAL$bg, 3)), POSITIONING), PAL)   # told apart with colour blindness too (core/theme.R)
CORRECTED_M <- 0.1       # a geotag accuracy (m) at or under this counts as corrected (RTK/PPK gives about 0.02; standalone 2-5)

# GNSS bases (base-rover surveys, such as Emlid Reach): how each survey's base was positioned, one row per GNSS file name
# or pattern (* for all). method: single averaging, float averaging, fix averaging, static, known point, network
# (NTRIP / CORS) or unknown; single and float averaging are flagged unless post-processed; post_processed: yes or no; service: what corrected it (NRCan CSRS-PPP, a CORS ...). R/gnss.R gnss_base_check.
GNSS_BASE_COLS <- c("file", "base", "method", "post_processed", "service", "notes")
# Drone cameras: a photo counts as a drone photo when it carries a drone XMP tag (DJI drone-dji, senseFly, Camera:) or its
# EXIF make and model match a row here: make in any case, model as a prefix (blank: any model of that make). A row with drone
# "no" wins over a drone tag: DJI's handheld cameras (Osmo, Pocket) write the same drone-dji tags as its drones. Other photos
# (phones, compact cameras, trail cameras, screenshots) are left out of the flights and listed in catalogue/other_photos.csv.
DRONE_CAMERA_COLS <- c("make", "model", "drone", "note")
drone_cameras_default <- function() rbind(
  data.table(make = "DJI", model = names(DJI_CAMERAS), drone = "yes", note = unname(DJI_CAMERAS)),        # readers.R: older DJI cameras with no drone tag
  data.table(make = c("DJI", "DJI", "DJI", "senseFly", "Parrot", "MicaSense", "Canon"), model = c("ZenmuseP1", "L2", "M3M", "", "Sequoia", "", "Canon PowerShot S110"), drone = "yes",
             note = c("Zenmuse P1", "Zenmuse L2 RGB camera", "Mavic 3 Multispectral", "S.O.D.A., thermoMAP, Aeria and other eBee cameras", "Parrot Sequoia multispectral",
                      "RedEdge, Altum", "eBee RGB and NIR camera (a compact camera: handheld S110 photos match too)")),
  data.table(make = "DJI", model = c("DJI Pocket", "OT110", "OSMO", "AC00"), drone = "no",
             note = c("Pocket 2 (handheld)", "Osmo Pocket (handheld)", "Osmo cameras (handheld)", "Osmo Action (action camera)")))
BASE_METHODS <- c("single averaging", "float averaging", "fix averaging", "static", "known point", "network (NTRIP / CORS)", "unknown")
BASE_LIMITS <- c("single averaging" = "base_single_avg_error_m", "float averaging" = "base_float_avg_error_m")   # methods only metres or so accurate

# Calibration options (core/calibration.R, config/calibration_options.csv shared with the other apps): every RS value a
# project can change, with the code's default and, only where one is published, the Scotty Creek value, each with its
# source. A blank default means no limit (the value is reported, nothing is flagged until a number is set). Flight limits
# apply to the whole project (applies_to *); GNSS limits to a base or file name, or *.
# Sources: _hub/Derived_values_references.md, section 14.
RS_CALIBRATION <- rbindlist(list(
  list("base_single_avg_error_m", "Base set by single averaging: absolute error", "m", "2.5", "Emlid docs (docs.emlid.com, placing the base): a base averaged in single mode is about 2.5 m off; averaging longer does not improve it", "", ""),
  list("base_float_avg_error_m", "Base set by float averaging: absolute error", "m", "1.0", "Emlid docs (docs.emlid.com, placing the base): a base averaged with an RTK float solution is about 1.0 m off", "", ""),
  list("max_correction_age_s", "Oldest corrections", "s", "10", "Emlid firmware 31 onward drops a fix after 10 s without corrections (Emlid staff, community forum)", "", ""),
  list("max_baseline_m", "Longest baseline", "m", "60000", "Emlid Reach RTK working range up to 60 km (PPK up to 100 km); the + 1 ppm term of the RTK spec grows with it", "", ""),
  list("min_fixed_pct", "Fewest fixed positions", "%", "", "no published minimum; set your own", "", ""),
  list("checkpoint_survey_rmse_h_cm", "Checkpoint survey RMSE, horizontal", "cm", "2", "ovid default, about an RTK receiver over a short baseline (Emlid RTK 7 mm + 1 ppm H); use the receiver's own RMS where it is exported",
       "2", "Scotty Creek differential GPS, +/- 0.02 m (Leica SR530 / Viva GS10: Connon et al. 2018; Disher et al. 2021)"),
  list("checkpoint_survey_rmse_v_cm", "Checkpoint survey RMSE, vertical", "cm", "3", "ovid default, about an RTK receiver over a short baseline (Emlid RTK 14 mm + 1 ppm V); use the receiver's own RMS where it is exported",
       "2", "Scotty Creek differential GPS, +/- 0.02 m (Leica SR530 / Viva GS10: Connon et al. 2018; Disher et al. 2021)"),
  list("geotag_corrected_m", "Geotag accuracy that counts as corrected", "m", "0.1", "RTK/PPK geotags are about 0.02 m (DJI Phantom 4 RTK, M350 RTK: 1 cm + 1 ppm H), standalone GNSS metres; 0.1 m separates them", "", ""),
  list("blur_limit_px", "Motion blur limit", "px", "1", "convention: no well-cited paper gives a fixed pixel limit (Sieberth et al. 2014, 2016; gap 15)", "", ""),
  list("fwd_overlap_target_pct", "Forward overlap target", "%", "", "no fixed target; judge against the platform's spec conditions (Zenmuse P1 75 %); accuracy is fairly insensitive once the network is strong (James et al. 2020)", "", ""),
  list("side_overlap_target_pct", "Side overlap target", "%", "", "no fixed target; judge against the platform's spec conditions (Zenmuse P1 55 %, Zenmuse L2 20 %)", "", ""),
  list("min_sun_elev_deg", "Lowest sun elevation", "deg", "", "no well-cited fixed minimum; shadow lowers NIR in boreal peatland (Chasmer et al. 2011)", "", ""),
  list("check_placed_ok_pct", "Positions check: placed for ok", "%", "95", "ovid default (interface design, 9 Oct 2026); no published threshold; set your own", "", ""),
  list("check_placed_look_pct", "Positions check: placed for look (below: bad)", "%", "50", "ovid default (interface design, 9 Oct 2026); no published threshold; set your own", "", ""),
  list("photoset_max_images", "Photo set: a run of fewer images than this", "images", "20", "ovid default; no published threshold; set your own. A photo set is kept apart from mapping flights and is not checked as one", "", ""),
  list("check_standalone_max_m", "Accuracy check: worst standalone accuracy for look (above: bad)", "m", "5", "standalone GNSS geotags are good to 2-5 m (see geotag_corrected_m); worse suggests a bad fix", "", ""),
  list("spatial_max_km", "Other geodata: farthest from a flight to catalogue", "km", "5", "ovid default (30 Oct 2026); no published threshold; set your own. Folders beside flight photos and folders kept on the Other geodata tab are catalogued at any distance", "", ""),
  list("spatial_max_extent_km", "Other geodata: largest extent to catalogue (above: a basemap)", "km", "200", "ovid default (30 Oct 2026); no published threshold; a national or provincial layer spans hundreds of km", "", ""),
  list("geodata_gis_layers", "Catalogue GIS layers (1 yes, 0 no)", "", "1", "ovid default: on", "", ""),
  list("geodata_imagery", "Catalogue satellite, aerial, LiDAR and raster files (1 yes, 0 no)", "", "1", "ovid default: on", "", "")))
setnames(RS_CALIBRATION, c("value", "label", "unit", "default_number", "default_source", "scotty_number", "scotty_source"))
RS_CALIBRATION[, `:=`(app = "RS", allowed = "", notes = "")]; setcolorder(RS_CALIBRATION, CALIB_CATALOGUE_COLS)

# Footprint colours on the map, the report and the viewer (config/footprint_colours.csv): one row per drone family, matched
# on the aircraft, camera and make (match, a pattern; a blank match takes every flight the rows above did not), with a colour
# for its optical footprints, one for its other sensor (LiDAR, NIR), and one for thermal (blank: the other sensor's). A colour
# is a palette name (red, gold, blue, purple, green, teal ...), two joined by + (their mix) or a #hex. Each year in view gets
# its own shade of it, the hue turned by steps of year_step_deg and lighter or darker in turn, at least 15 dE from every other
# year of that drone and sensor (R/footprints.R fp_year_shades).
FP_COLOUR_COLS <- c("family", "match", "optical_col", "other_col", "thermal_col", "year_step_deg", "note")
fp_colours_default <- function() data.table(family = c("DJI", "eBee", "Other"),
  match = c("DJI|Zenmuse|Phantom|Matrice|Mavic|Inspire|^FC[0-9]", "senseFly|eBee|S[.]O[.]D[.]A|thermoMAP|Duet|Aeria|Sequoia|S110", ""),
  optical_col = c("red", "blue", "green"), other_col = c("gold", "purple", "teal"), thermal_col = c("", "purple+red", ""),
  year_step_deg = "16", note = c("DJI drones and Zenmuse cameras", "senseFly eBee and its cameras", "every other drone"))

CONFIG_FILES <- list(
  sources    = list(file = "sources.csv",    cols = SOURCE_COLS,    label = "Archive sources"),
  flights    = list(file = "flights.csv",    cols = FLIGHT_COLS,    label = "Flights"),
  sites      = list(file = "sites.csv",      cols = SITE_COLS,      label = "Sites"),
  site_names = list(file = "site_names.csv", cols = SITE_NAME_COLS, label = "Site names found"),
  links      = list(file = "links.csv",      cols = LINK_COLS,      label = "Confirmed and rejected links"),
  file_rules = list(file = "file_rules.csv", cols = FILE_RULE_COLS, label = "File rules"),
  timezones  = list(file = "timezones.csv",  cols = TZ_COLS,        label = "Time zones"),
  products   = list(file = "products.csv",   cols = PRODUCT_COLS,   label = "Products"),
  gnss_bases = list(file = "gnss_bases.csv", cols = GNSS_BASE_COLS, label = "GNSS bases"),
  drone_cameras = list(file = "drone_cameras.csv", cols = DRONE_CAMERA_COLS, label = "Drone cameras", default = function() drone_cameras_default()),
  footprint_colours = list(file = "footprint_colours.csv", cols = FP_COLOUR_COLS, label = "Footprint colours", default = function() fp_colours_default()),
  calibration = calibration_config(RS_CALIBRATION))   # Ovid/timezones.R: camera clocks

# Site names found in folder names, mapped through config/site_names.csv (found -> site; any case): a blank site there means
# the name is not a site. Names with no row are kept as found.
# The Simple view: folder names that are a camera's, a card's or a backup's, never a site (Edit offers no site for them, View
# marks their groups "camera folder"). A regular expression, any case.
SIMPLE_GENERIC_FOLDERS <- "^(dcim|\\d{3}media|\\d{3}_?[a-z]*|camera|cameras|photos?|pictures?|images?|img|backup|backups|raw|data|sd ?card|card ?\\d*|new folder|untitled|misc|other|unsorted)$"
# Renaming a site (core rename.R): the sites table, and the tables that name a site.
RENAMES <- list(site = list(label = "Site", main = c("sites.csv", "site"), cols = list(c("flights.csv", "site"), c("site_names.csv", "site")), lists = list()))
site_apply_names <- function(found, names) {
  if (is.null(names) || !nrow(names)) return(found)
  k <- match(tolower(trimws(found)), tolower(trimws(as.character(names$found))))
  to <- trimws(as.character(names$site))[k]; to[is.na(to)] <- ""
  ifelse(is.na(k), found, to)
}
