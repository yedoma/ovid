# CS: the theme, the table status styles and what every audit rule means. Sourced by app.R.
# =============================================================================
#  Theme  --  olive-crt, dark (colours from GB in R/plots.R)
# =============================================================================
gb_css <- paste(readLines(file.path(APP_DIR, "www", "cs.css"), warn = FALSE), collapse = "\n")   # CS/www/cs.css


status_style <- function(dt, col) ovid_dt_levels(dt, col, c(ok = "ok", matched = "ok", written = "ok", `partly matched` = "warn", `no matching FSL` = "error", unreadable = "error",
  duplicate_file = "off", skipped_table = "off", `columns differ slightly` = "warn", `no program` = "error", `system table` = "off", `see CR10X files` = "off"), GB, default = "warn")
# What every audit rule means, in plain words. lost: no = nothing lost, yes = a value or record was removed,
# kept = changed or flagged but still there, file = a whole file was not used.
RULE_INFO <- fread(sep = "|", quote = "", colClasses = "character", text = '
rule|title|lost|meaning|action|page
duplicate_record|Identical copies of a record|no|The same record, value for value, was found in more than one file (downloads usually overlap). One copy is kept.|Nothing to do.|
overlap_conflict_merged|Records sharing a timestamp, merged|no|Two files had a record for the same time. They were merged into one: blanks in the newer file are filled from the older one.|Nothing to do unless the next line shows many conflicts.|
overlap_conflict|Values that disagreed between files|kept|Two files gave different values for the same logger, time and column. The newest file wins; the other value is stored here.|A handful is normal. Thousands on one logger usually means two different loggers share a serial number or name, or a clock was wrong: check Raw files and Clock checks.|Raw files
station_overlap_merged|Records merged between loggers at one station|no|Two loggers assigned to the same station both had a record at the same time; they were merged.|Check the station dates on the Stations page if the loggers should not overlap.|Stations
station_overlap|Values that disagreed between loggers at one station|kept|Two loggers assigned to one station gave different values at the same time. The newest file wins.|Give each logger its own date range on the Stations page.|Stations
bad_timestamp|Records with an impossible date|yes|The timestamp could not be read, or is before the earliest plausible year or in the future. A logger that loses power often restarts at a default date.|See Clock checks: the records can be moved to the right time instead of being dropped.|Clock checks
clock_fix_removed|Records removed by a rule you set|yes|A correction on the Clock checks or Raw files page removed these records.|Review or undo it under Clock checks, Corrections already set.|Clock checks
clock_fix_shifted|Records moved in time by a correction|kept|A clock correction you set shifted these records. Values are unchanged.|Review it under Clock checks.|Clock checks
malformed_row|Lines that are not a record|yes|A line in the file had the wrong number of fields or could not be parsed (a cut-off download, or text in the middle of the data).|Open the file on the Raw files page to look at the raw text.|Raw files
non_numeric|Text in a numeric column|yes|A value in a column that is otherwise numeric could not be read as a number, so it was blanked. NAN written by the logger is counted separately as missing, not here.|Usually a corrupted line. Nothing to do.|
alias_collision|Two raw columns mapped to one parameter|kept|Two columns of one logger were given the same common name and disagreed. The first is kept.|Give them different names on the Column names page.|Column names
sensor_change|Sensor changes joined into one series|change|A sensor was replaced and the program wrote it under a new column; the old and new columns were joined, the newer sensor winning where both had a value.|Check the hand-over date is right.|Column names
alias_kept_apart|Columns kept apart|change|Two columns mapping to one parameter were recorded at the same time (a sample beside its average, or two sensors), so they were not merged.|Give the second one its own name if needed.|Column names
sentinel|Logger overflow codes|yes|Values of 7999, -7999, 6999 or -6999, or infinity: the codes a Campbell logger writes when a reading is out of range for the input. Blanked.|Nothing to do; a long run of them points to a failed sensor or wiring.|
range_min|Values below a limit|yes|The value was below the minimum set for that parameter on the Quality rules page.|If real values are being removed, lower the limit or set the rule to flag instead of remove.|Quality rules
range_max|Values above a limit|yes|The value was above the maximum set for that parameter on the Quality rules page.|If real values are being removed, raise the limit or set the rule to flag instead of remove.|Quality rules
spike|Single-point spikes|yes|One record jumped by more than the allowed amount and came straight back.|Adjust max_spike on the Quality rules page.|Quality rules
flat_line|Stuck-sensor values removed|yes|The value did not change for longer than the limit and the rule is set to remove.|Adjust max_flat_hours on the Quality rules page.|Quality rules
manual|Values removed by hand|yes|Someone dragged across a plot, or added a row under Manual removals, with the reason shown.|Review under Quality rules, Manual removals.|Quality rules
ignored_source|Records from a source set to ignore|yes|The station name of this logger or file is set to ignore, so its records were left out.|Undo on the Stations page.|Stations
duplicate_file|Byte-for-byte duplicate files|file|An exact copy of another file. Read once.|Nothing to do.|
skipped_table|Tables on the leave-out list|file|Diagnostic tables such as Public or Status, listed under Folders on the Project step.|Remove the table from that list to include it.|Project
unsupported_binary_TOB|Binary TOB files|file|Campbell TOB1 or TOB2 binary (TOB3 is read directly).|Convert to TOA5 with LoggerNet or CardConvert.|
unsupported_mixed_array|CR10X files not imported|file|Header-less mixed-array files, with CR10X import switched off.|Tick Include CR10X files on the Project step.|Project
cr10x_import_off|CR10X files marked not to import|file|Marked import = no on the CR10X files page.|Change it there.|CR10X files
no_usable_arrays|CR10X files with no usable array|file|No array in the file could be given a timestamp.|Check the file on the Raw files page.|Raw files
unrecognised_format|Not a Campbell data file|file|The file ends in .dat but is not TOA5 or mixed-array.|Nothing to do.|
empty_file|Empty files|file|The file has no content.|Nothing to do.|
truncated_header|Files with an incomplete header|file|The header lines are cut off.|Download the file again.|
unreadable|Files that could not be opened|file|The file could not be opened (permissions, or a broken link).|Check the file on disk.|
no_data_rows|Files with a header but no records|file|The download contained no data.|Nothing to do.|
excluded_by_user|Files left out by a rule|file|Left out on the Raw files page.|Undo there.|Raw files
stray_record|Records with corrupt timestamps|row|A failing memory or clock stamped these at random times among good records (off the time grid and out of written order).|See Clock checks.|Clock checks
set_aside|Files of loggers set aside|file|The logger was set aside on the Stations page, so its files were not read.|Bring it back there.|Stations
group_failed|Logger tables that could not be stitched|file|An error stopped this logger table; the rest of the run carried on.|See Loggers and files, Problems.|Metadata
station_failed|Stations that could not be written|file|An error stopped this station; the rest of the run carried on.|See Loggers and files, Problems.|Metadata
')
