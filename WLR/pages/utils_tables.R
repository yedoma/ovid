# WLR: the theme, the table status styles and what every audit rule means. Sourced by app.R.
# =============================================================================
#  Look: Nord dark (Polar Night grounds, Snow Storm text, Frost accents, Aurora for status)
# =============================================================================
# Only WLR's own rules here: the shared look (type, controls, tables, tabs, panels) comes from Ovid/shell.R, which
# takes its colours from NORD below.
WLR_TOKENS <- list(acc = NORD$frost, acc2 = NORD$aqua, deep = NORD$deep, onacc = NORD$bg_hard, bgh = NORD$bg_hard)
WLR_PAL <- c(NORD, WLR_TOKENS)
nord_css <- paste(readLines(file.path(APP_DIR, "www", "nord.css"), encoding = "UTF-8"), collapse = "\n")

status_style <- function(dt, col) ovid_dt_levels(dt, col, c(ok = "ok", paired_hobo = "off", companion_csv = "off", duplicate_file = "off",
  excluded_by_user = "off", temperature_logger = "info", hobo_without_csv = "error", unreadable = "error", no_serial = "error", empty_file = "off", no_data_rows = "off"), WLR_PAL)

RULE_INFO <- fread(sep = "|", quote = "", colClasses = "character", text = '
rule|title|lost|meaning|action|page
bad_timestamp|Records with impossible dates|yes|Stamped before the earliest plausible year or in the future (a reset clock), so they were left out.|Check the logger clock.|
stray_record|Records with corrupt time stamps|yes|A failing memory or clock stamped these at random times among good records (off the time grid and out of written order), so they were left out.|Nothing to do.|
duplicate_record|Identical copies of a record|no|The same reading was found in more than one download (a Solinst logger that is not cleared carries every earlier season; HOBO files are often exported twice). One copy is kept.|Nothing to do.|
overlap_conflict|Readings that disagreed between files|kept|Two files of one logger gave different values at the same time. The newest download is kept.|A handful is normal. Many on one logger usually means two loggers share a serial, or a clock or time zone was wrong: see Raw files and Clock checks.|Raw files
clock_fix_shifted|Readings moved in time|kept|A clock correction you set moved these readings. Values are unchanged.|Review on Clock checks.|Clock checks
clock_fix_overlap|Readings moved onto existing ones|yes|A clock correction moved readings onto times that already had one.|Check the correction on Clock checks.|Clock checks
outside_deployment|Readings outside the deployment|yes|Before the logger went into the water or after it came out, from the visit dates in the workbook (or the dates set by hand).|Set the deployed and removed dates on the Deployment history page if these look wrong.|Deployment history
range_min|Values below a limit|yes|Below the minimum set on the Quality rules page.|If real values are removed, lower the limit or set the rule to flag.|Quality rules
range_max|Values above a limit|yes|Above the maximum set on the Quality rules page.|If real values are removed, raise the limit or set the rule to flag.|Quality rules
flat_line|Stuck readings|yes|The value did not change for longer than the limit.|Adjust max_flat_hours on the Quality rules page.|Quality rules
manual|Values removed by hand|yes|Someone dragged across a plot, or added a removal under Corrections over a period, with the reason shown.|Review under Quality rules.|Quality rules
out_of_water_manual|Out of the water (set by hand)|yes|Pressure, depth and elevation were removed for a period marked as out of the water (retrieval, download, or a data error).|Review under Quality rules, Out of water.|Quality rules
duplicate_file|Byte-for-byte duplicate files|file|An exact copy of another file. Read once.|Nothing to do.|
empty_file|Empty files|file|The file has no content.|Nothing to do.|
unrecognised_format|Not a logger file|file|Not a HOBOware CSV, Solinst .xle or Solinst CSV.|Nothing to do.|
unreadable|Files that could not be read|file|The file could not be opened or parsed.|Open it on the Raw files page.|Raw files
no_data_rows|Files with no readings|file|A header but no records (an empty logger).|Nothing to do.|
no_serial|Files without a serial number|file|No serial could be found in the file.|Give it one with a rule on the Raw files page.|Raw files
hobo_without_csv|HOBO files with no CSV export|file|U20 and U20L .hobo files are read directly; for other models the app reads the HOBOware CSV export, and none was found for this file.|Export a CSV from HOBOware into the same folder.|Raw files
temperature_logger|HOBO temperature loggers|file|A HOBO with temperature but no pressure or conductivity channel (a U12 thermistor logger, for example) is a ground-temperature logger: Ovid GT reads it, not WLR.|Nothing to do.|Raw files
excluded_by_user|Files left out by a rule|file|Left out on the Raw files page.|Undo there.|Raw files
clock_fix_drift|Readings spread back over a clock drift|kept|A clock correction you set (mode drift) moved these readings by an amount growing from none to the full drift. Values are unchanged.|Review on Clock checks.|Clock checks
launched_in_air|Readings taken in the air|yes|The logger was still in the air at the start or end of the deployment (pressure close to the barometric pressure), so these readings were removed. With the switch off they are flagged instead.|Turn off Trim readings in the air in Settings (Checks) to keep and flag them.|Settings
drift_correction|Water levels corrected for drift|kept|Corrected to the tape checks between visits (Settings, Drift between tape checks, or a row under Drift and offset corrections). The values as the logger read them are kept beside the corrected ones.|Review on Field visits, Drift and offset corrections.|Field visits
period_remove|Values removed over a period|yes|A row of Corrections over a period removed the values of its period.|Review under Quality rules, Corrections over a period.|Quality rules
period_offset|Values offset over a period|kept|A row of Corrections over a period added its value to the values of its period.|Review under Quality rules, Corrections over a period.|Quality rules
period_scale|Values scaled over a period|kept|A row of Corrections over a period multiplied the values of its period by its value.|Review under Quality rules, Corrections over a period.|Quality rules
period_drift|Values drift-corrected over a period|kept|A row of Corrections over a period added a share of its value growing from none at the start of the period to all of it at the end (linear drift).|Review under Quality rules, Corrections over a period.|Quality rules
period_interpolate|Values filled in over a period|kept|A row of Corrections over a period filled the period with a straight line between the last value before it and the first after it.|Review under Quality rules, Corrections over a period.|Quality rules
period_flag|Values flagged over a period|kept|A row of Corrections over a period flagged the values of its period; none was changed.|Review under Quality rules, Corrections over a period.|Quality rules
')
rule_info <- function(r) { i <- match(r, RULE_INFO$rule); out <- RULE_INFO[i]; out[is.na(i), `:=`(rule = r[is.na(i)], title = r[is.na(i)], lost = "kept", meaning = "", action = "", page = "")]; out }
