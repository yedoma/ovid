# Ovid WLR changelog

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Added (corrections applied, 2026-10-10)**
- Corrections applied: a chip and pop-up in the top bar list every correction in the last run with counts (core applied.R); output/audit/corrections_applied.csv, also in exports and reports. **Output changes:** new file; summary.csv gains baro_bridged and values; period removals name their row in the audit reason.

**Changed (Plot and explore on the standard plot, 2026-10-09; plot and table standards A11)**
- One site on Plot and explore uses the standard plot (as CS): the same drag, click, zoom, keys, Save image and Save data.
  Drag selects a stretch and dragging up a panel's value axis narrows it to a band (the old box). The edits are on the bar
  above the selection: remove the points in the band, remove every value, out of the water (with why), logger
  went in or came out here; each needs a reason, goes to the same set-up table as before and has Undo. Tape checks and manual
  readings are diamonds, out-of-water and hand-removed periods are shaded grey, removed values grey crosses, flagged values
  gold rings, field visits dashed lines. Export plot (PNG) gives way to Save image.
- Output changes: none.
**Changed (one review list, project menu, time zone, 2026-10-09)**
- Needs you shows the one review list; Home's More section and "Worth a look" fold into Needs you and the set-up check; the Check the data and "Saved, and next time" steps fold into Needs you and Home. **Output changes:** none.
- Save settings is gone (settings save as they change). The Workbench Review queue tab is gone.
- Time zone: one core control in Settings; the unzoned-file offset field and the Raw files set-zone buttons are gone (the app still asks, and the time-zone table still applies). **Output changes:** none.
- Settings lists the projects; the project menu is in the top bar.

**Removed and folded in (approved mockup, 2026-10-09)**
- Loggers and metadata: the Logger list tab is gone; its columns (name, type, hang length, depths) are optional columns of the Deployment sheet. An older metadata.csv is moved in once and left in place. **Output changes:** none.
- Quality rules: Manual removals, and Field visits' drift and offset table, are rows of Corrections over a period (remove; drift_tape, drift_manual, drift_none, drift_keep). Older files are moved in once and applied once. **Output changes:** a removal of depth_m no longer blanks dtw and elevation worked out from it; existing period remove rows now run in the manual-removal step; * in an old row means all.
- Split here on Needs you writes two rows of the Deployment sheet instead of logger_splits.csv. **Output changes:** none.
- Settings: the tip-to-sensor offset switch is gone; the offset is always added where the hang length was measured to the tip (set it to 0, or measured_to to sensor, to leave it out). **Output changes:** only for a project that had switched it off.
- Settings: drift between tape checks is Off or Apply (the report-only choice is gone; an old one reads as Off). The tape limit (5 cm), the smallest offset applied (1 cm) and the one-check rule (held constant) are constants; which version a file carries is chosen at export (Drift corrected values), and the run always writes both. **Output changes:** none at the defaults; the export zip follows the export's Drift corrected values again.
- Settings: one level limit, Out of the water below, serves the out-of-water flag and the in-the-air check at a deployment's ends (the separate In the air below goes). **Output changes:** at the defaults the in-the-air check now uses 0.02 m, not 0.03 m.
- Settings: the physical constants and expert thresholds (Solinst factor, density, gravity, conductivity coefficient, baro gap, tape window, the uncertainty sigmas, barometric efficiency, casing limit) are under a folded Advanced section, same defaults. **Output changes:** none.
- Plot and explore: Drop these records is gone; Remove every value in this period does the same to the output. **Output changes:** none.
- Datum and dGPS loses the Logger against tape tab, and Field visits the second table (Measured against the logger); the tape comparison is on Field visits and manual measurements on Plot and explore. **Output changes:** none.
- Barometric: the All years table and Use the suggestions are gone; a year with no saved groups opens with the suggestion in the pickers and Save choices puts it in place. **Output changes:** none.
- Reports and export: one export (sites, values, time step, layout) with a Format choice: ZIP package, one CSV, one file per site, or a report. Approvals are set there (no longer under Hydrology and uncertainty). **Output changes:** none.
- The Conversion check and Conductivity pages are gone: conductivity and every conversion step (raw pressure, water pressure, baro, water minus baro) are series on Plot and explore, and the formulas are in Reference. **Output changes:** none.
- Loggers and metadata loses the Problems and Runs tabs (problems are on the Set-up check and Needs you, runs in the audit trail). **Output changes:** none.

**Fixed (review items, 2026-10-09)**
- Run saves unsaved edits in every set-up table first, the logger list and the tables added since (manual levels, other baro records, approvals and the rest) included; it asks about set-up problems with the shared question (core setup_gate). **Output changes:** none.
- Summaries help follows the water year start in Settings; the Barometric page says a processing year and its baro groups run from one 15 September download sweep to the next. **Output changes:** none.
- The tape limit on Needs you and the site dashboard is the one the run's tape check uses (tape_warn_m, 5 cm), not a fixed 5 cm and 2 cm. "See the tape check" opens Field visits, where the tape table and Keep are. **Output changes:** none.
- Field visits lists the sites of the deployments too, so a project with only a logger list (no workbook) has sites and a table. **Output changes:** none.
- The export zip takes its drift-corrected values from the export's own choice (Drift corrected values on Reports and export), like the other export forms. **Output changes:** none at the default (both versions).
- Recent tabs in the Workbench show the page titles of the set-up flow, not their ids. **Output changes:** none.
- The configuration zip carries the logger list, the Check the data decisions and the Automatic corrections switches; "Also apply its settings" applies every setting in it, not only the folders. **Output changes:** none.
- Stale text: the Get started steps, where the run log is, where each automatic correction is changed, and the Automatic corrections step now shows how many switches are on. **Output changes:** none.
- The problems link in the Workbench status bar opens the Set-up check, as the Standard one does. The audit explains readings in the air, drift corrections and corrections over a period. **Output changes:** none.
- Raw files are counted the same way on Home, the site dashboard and the Workbench (.csv, .txt, .xle, .hobo, without ._ copies). **Output changes:** none.

**Fixed (review decisions and set-up notice, 2026-10-09)**
- Check the data: Keep is stored with the review queue's decisions (config/review_decisions.csv) and the item leaves both lists; Flag works (kept there as flagged).
- A run reads review_decisions.csv: a flag decided not a problem stays in flags.csv marked `accepted` and leaves the corrected files' and exports' flag columns. flags.csv is written before the corrected files.

**Fixed (damaged run record, 2026-10-09)**
- A damaged output/audit/run_manifest.csv no longer stops the Set-up page (and the Raw files page in GT and WLR) with an error; it is read as empty. **Output changes:** none.

**Removed (unused code, 2026-10-09)**
- trim_deployment(), in_spans() (qc.R) and match_serials() (metadata.R): never called. WLR does not cut records at the deployment
  dates; the ends of a deployment in air are found by the out-of-water check. **Output changes:** none.

## 0.12.0 (2026-10-10), needs core 1.15.0

**Changed (sample tests, 2026-10-09)**
- Sample tests: a new or changed sample fails until accepted (tests/samples/accepted.csv); hand-checked values in reference.csv. **Output changes:** none.

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). **Output changes:** none.

**Changed (UTM zone and time zone, 2026-10-09)**
- Settings has a UTM zone, used for the dGPS survey points and the NetCDF site positions; before, both assumed zone 10.
- Labels and the zone question use the time zone from Settings. Unit conversions go through core convert.R.

**Output changes:** none with zone 10 (the default).

**Changed (errors reported, 2026-10-09)**
- An audit file that cannot be read during an export, or a site position that cannot be converted for NetCDF, is listed as a problem instead of being left blank with no notice.

**Output changes:** none.

**Changed (packages, 2026-10-08)**
- The app and its pipeline no longer install packages when they start: a missing one is named, with how to get it (the launcher installs the versions in renv.lock). **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R from 343 to 221 lines: its helpers moved to pages/fct_outputs.R (reading the outputs), pages/utils_tables.R (theme and table style) and pages/utils_setup.R (set-up table editors), sourced in the same place. Code unchanged line for line. **Output changes:** none.
- The Nord CSS (app.R) is www/nord.css; the text is the same character for character. **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R split from 1409 to 357 lines: each server section is its own file in pages/ (sourced inside the server, same order),
  and the interface is pages/ui.R. Code unchanged line for line. **Output changes:** none.

**Added (2026-10-08)**
- Site page: Export of the site's deployments table as CSV (the rest of the approved site dashboard was already in place). **Output changes:** none.

**Changed (2026-10-08)**
- Before import: "Import metadata (CSV)" opens Loggers and metadata, where a CSV or Excel file is imported and its columns matched. **Output changes:** none.

**Changed (2026-10-08)**
- Errors under their field in the Workbench dialogs; correction buttons say why they are greyed; a Columns picker on every wide table. Moves found by a jump in water level are suggested in Needs you. **Output changes:** none.

**Added (2026-10-08)**
- Group the logger list by serial number, deployment, download or raw file (Loggers and metadata and the Workbench tree); moves found in the records in Needs you;
  Rename and Duplicate in the projects menu. **Output changes:** none.

**Added (2026-10-08)**
- Barometric > Offset and drift: the sites each barologger check changes, listed before a run applies it. **Output changes:** none.

**Changed (2026-10-08)**
- Only the raw folder is needed: with no project folder the app uses ~/Desktop/Ovid_Projects/WLR/Untitled; no metadata is needed
  (loggers go by serial). Test: `tests/test_no_metadata.R`. **Output changes:** none.

**Added (UI design, 2026-10-08)**
- Before import: a drop area, a checklist of what compensation needs with each item's state, and blank template tables. Reference
  page; Workbench pipeline tabs; switching views keeps the site. **Output changes:** none.

**Added (set-up Workbench design, 2026-10-08; core workbench 0.9.0)**
- Workbench: every set-up table is a spreadsheet with a formula bar; **Correct a period** in the cell bar; **Needs you** at the top
  right; the **Review queue** tab; a "WLR processing" section in the tree that opens the app's own steps.
- Corrections over a period apply to the logger's pressure and temperature before barometric compensation, to the depth before the elevations are worked out, and to barologgers before they are used (Quality rules > Corrections over a period).
  **Output changes:** none until a project has rows in period_corrections.csv.

**Changed (set-up flow, shared core shell 1.2.0, flow 1.0.0, tsview 1.0.0, workbench 0.8.0)**
- The numbered set-up flow, the project switcher, the information panel, Check the data and the Standard view plots (see core
  CHANGELOG); table headings shown as words with units (`ovid_colnames()`).
- Workbench: units under the grid headings, quality-rule range limits on the chart, chart selection statistics in the inspector.

**Fixed (run-through on the Mac, 2026-10-08)**
- The app no longer greys out after an error in a handler, notices a deleted output folder, and shows damaged set-up tables with
  their columns restored (core). Pending removals are drawn only on their own parameter in the Workbench chart.
- Site dashboard: a clash of two variables named k (the plot cache key and a pressure ratio) broke the plot when a removed level
  had to be shown as a ghost.
- Saving a precipitation file no longer makes the next run redo every logger (it is a display setting and left the cache key).
- Pages no longer break when no deployment was processed (every logger waiting in Needs you): empty output tables keep their columns.
- Check the data: a flag without a time is left out instead of breaking the page.
- New Configuration page (as CS and GT): export every set-up table and the settings as one zip, import a zip, CSV or workbook.
- A U20 or U20L .hobo with no HOBOware CSV is read directly (core hobo.R) instead of being set aside.
  **Output changes:** loggers that had only a .hobo download now have records; nothing else changes.


**Added (projects, renames, report layout)**
- Projects list on Home (core `projects.R`).
- Rename a site (or barometric station) everywhere (Loggers and metadata > Rename; core `rename.R`).
- Report layout on the Export page (core `report_options.R`): titles, file names, a line under the title, who prepared it, sections and axes; water elevation shares the span between sites, each at its own level.
- The deployment sheet fills what the logger list leaves blank; differences are listed in the set-up check.

**Output changes:** none.

**Added (automatic corrections list)**
- Settings: an Automatic corrections list (core `corrections.R`) of everything a run changes in the data on its own, with
  what each does, whether it is on, and where it is switched; it adds a switch for leaving out reset-clock records (on by default), and switches for leaving out stray records, which had no control in the app. Each run log lists them.

**Output changes:** none.

**Added**
- Needs assessment, second batch (medium items), on a new page, Hydrology and uncertainty (`R/hydro.R`); nothing here changes
  a reading:
  - Uncertainty budget (`audit/uncertainty.csv`): the standard uncertainty of each deployment's depth, depth to water and
    elevation from the logger's stated accuracy (new `logger_accuracy.csv`, seeded with typical values for HOBO U20/U20L and
    Solinst models), the barometric record (a fill-in counts for more), the water density, the spread of the tape checks and
    the reference survey, added in quadrature. Monthly and water-year summaries add `sigma_m`.
  - Barometric efficiency (`audit/barometric_efficiency.csv`): hourly changes of depth regressed on changes of air pressure
    at lags up to a day (regression deconvolution); a well that answers the air pressure, at once or with a delay, is listed.
  - Casing movement (`audit/casing_movement.csv`): the casing's height above the ground (TOC_veg) between visits; a change
    over 2 cm is reported as frost jacking or ground movement (a problem line too).
  - Reference points that follow their surveys in time: Settings > "Follow the surveys in time" interpolates a deployment's
    reference elevation between surveys, so the elevations of subsiding ground sink with it instead of stepping.
  - Water table against the ground (`wt_ground_m`, a new value in plots, summaries and exports): from the T-coupler to
    vegetation height of the visit (T_veg), else from a surveyed ground point (GS).
  - Hydroperiod (`summaries/hydroperiod.csv`), hydraulic gradients between pairs of wells with their uncertainty (new
    `well_pairs.csv`; `summaries/gradients.csv`) and recharge by the water-table fluctuation method (new
    `specific_yield.csv`; `summaries/recharge.csv`).
  - Approvals (new `approvals.csv`): a site's record is approved up to a date, provisional after it; every export carries
    an `approval` column.
  - NetCDF (Reports and export > ZIP: NetCDF too): each site as CF-1.10 / ACDD-1.3 NetCDF with every value's WMO code,
    uncertainty and approval, times in UTC (core `standards.R`). The export readme lists the processing levels.

**Fixed**
- On a project with no outputs yet, the Barometric page's chart stopped with an error, and the Workbench tried to load a record before one was chosen (found by the click-through tests).
- Datum and dGPS > Logger offsets no longer stops its model list on a project with no logger offsets yet.
- Clock drift: a row of `time_corrections.csv` with `mode` drift spreads a clock error found at download over its span (no
  shift at the start, `shift_minutes` at the end), the USGS proration GT uses for RBR loggers; also in the Workbench's Clock dialog.
- Heights and frames: survey points keep their ellipsoidal height, elevation and coordinate system (Emlid's CS name). A point
  with only an ellipsoidal height becomes an elevation with the geoid separation of the same survey's other points
  (H = h - N); ellipsoidal heights are never mixed with elevations at a site; a site surveyed in different frames
  (NAD83(CSRS), WGS 84, ITRF) is listed as a problem. Elevations typed by hand take optional `height_type`, `frame`, `epoch`,
  `geoid_model` and `geoid_N_m`, shown with the elevation used.
- Other barometric records as the last fill-ins (`external_baro.csv`, Barometric page): station pressure from any CSV, a
  Campbell climate station's export for instance, in kPa, hPa, mbar, Pa, mmHg or inHg and its own time zone, brought to the
  barologgers' elevation with the hypsometric equation when both elevations are given, then shifted onto the primary's level
  and flagged like every fill-in.
- Logger list (Loggers and metadata > Logger list): optional metadata for when there is no WL_Metadata workbook, or to add
  to it. Only the serial is needed; a name, site, year, type (Baro makes a barologger), logger model, install and visit dates,
  hang length (cm), position, land cover and notes are used when given, and a row without a year applies to every year the
  logger has files. Start from the loggers found, paste rows from Excel or import a CSV or Excel file (columns matched by
  heading), and rename, hide or add columns with Columns. Where the workbook lists the same logger and year, the list's
  filled cells win and its visit is added. A change to a logger's row redoes that logger only.
- Paste and Import in every set-up table.
- Workbench interface (preview), `workbench/` on core `workbench.R`: an additional, more detailed view of the same project with
  every process of the Standard interface, in WLR's Nord colours.
  - Every Standard page runs inside the Workbench on the Standard server (`server(embedded = TRUE)`): home, plot and explore,
    barometric, conversion check, conductivity, deployment history, datum and dGPS, field visits, audit trail, reports and
    export, set-up check, raw files, loggers and metadata, quality rules, clock checks, settings. They are listed under Pages in
    the tree and open as tabs.
  - Data: the tree lists every deployment by site (year and serial) and every barologger. The selected parameters of one record
    are charted over a spreadsheet grid; the inspector shows the selected value, its state (passed, flagged, removed, saved for
    the next run) and its lineage: the file and raw reading, the serial it was stitched into, the barologger it was compensated
    with (primary, secondary or a fill-in, and its shift), the conversion, and the rule or removal that took it. The deployment's
    hang length, offset, reference elevation and barologgers sit beside it.
  - Selecting works as in CS: cells, records (a time), parameters (a heading), drag, shift and cmd/ctrl, keys; the chart
    highlights the selection, zooms (drag, wheel, buttons) and selects (shift-drag, click).
  - Corrections, with the same rows Plot and explore writes: **Remove** (`manual_removals.csv`, one row per logger and run of
    records; a depth, elevation, depth to water or water minus baro is removed at its source, the pressure; R or Delete),
    **Restore** (deletes the manual removals and out-of-water periods that cover the values), **Out of water**
    (`out_of_water.csv`), **Drop records** (whole records), **Clock** (`time_corrections.csv`), **In / out dates** (a
    `deployments.csv` row: the logger went in after the selected records, or came out at the first of them), **Measurement**
    (`manual_measurements.csv`), and **Open the quality rules** of the parameter. The grid and chart mark saved corrections
    that the next run applies.
  - Set-up tables in the tree open the Standard page and tab that edits them. Import data holds the folders with a scan of new
    and changed files and the set-up check; Run is the Standard run.
- Choosing the interface: **Workbench** in the Standard title bar (it opens the deployment shown on Plot and explore),
  `?ui=workbench` / `?ui=standard` in the address, else `OVID_UI`, else the `ui` setting. Standard is the default.

**Changed**
- Deployment sheet (core): Loggers and metadata > Deployment sheet, with "Against the data". Its rows become one logger-list row
  per season (installed: the season after; removed: that visit's season). Declared rules for clock corrections (mode shift or
  drift) and manual removals. "Run anyway" is recorded in the run log.
- Fixed: two or more rows in datum_manual.csv (or an emptied one) stopped every run; a baro_corrections.csv row naming an
  unknown barologger stopped the run (now a problem). Solinst CSV with blank lines or a repeated header block, and HOBOware CSV
  with blank lines, now read. Guardrails: see core.
- Files not read before are read in parallel (core `par_map()`): the first run on the example data took 204 s instead of
  268 s. HOBOware headers, file listing, copies, gaps, flags and report helpers use core code (output unchanged); macOS ._
  files are no longer listed.
- Run happens in the background (core `runner.R`): the app stays usable, a box at the bottom right shows the step, the logger
  or deployment being worked on, and Cancel. Rescan and the time-zone question use it too.
- Every run leaves a log in `output/logs/`, and the lifebuoy in the top bar saves a troubleshooting zip (core). Tests now also read real sample files and click through every page in a browser.
- Each cached step is keyed on the code it runs (core `code_fingerprint()`, `STEP_HASH`), not on every file: an edit elsewhere no longer redoes it. The first run after this redoes everything once. `read_lines_any()` is renamed `wlr_read_lines()` (it no longer replaces core's).
- No metadata workbook is normal: it is no longer an error in the set-up check (a note), or a problem for each logger in the
  run; the metadata folder is marked optional. A missing dGPS folder is a note too.
- A run redoes only what an edit can change. Each logger's stitched record is kept (cache/stitch) and read back while its
  files, clock corrections and settings are unchanged. Each water logger's deployments are kept (cache/logger) and read back
  while its record, deployments, workbook rows, the quality rules, manual removals, out-of-water periods, drift rows and manual
  measurements that apply to it, the settings and the barometric records it was compensated with are unchanged: a removal on
  one logger redoes that logger only; a removal on a barologger redoes the water loggers it compensates. The corrected data of
  a site whose loggers were all reused is copied (cache/corrected). The agreement between barologgers is worked out once for
  each set of them and kept while their records are unchanged (it was worked out again for every year: half of a run on the
  demo project). Run with nothing changed on the demo project: 20 s before, 6 s now.
- The message at the end of a run says what was redone, for example "Redone: 1 of 4 water loggers; the rest was unchanged
  and reused."; `run_manifest.csv` counts `loggers_stitched_reused`, `water_loggers`, `water_loggers_reused` and
  `sites_corrected_reused`. Rescan on Raw files also forgets the selected files' loggers as stitched and compensated.
- The set-up tables use the editor shared by every app (core `editor.R`): a table open in both interfaces, or in two windows,
  takes up a version saved in the other within two seconds when it has no unsaved edits, and asks before it overwrites one
  when it has. Corrections added from a page start from the saved version.
- The Standard pages are built by `std_pages()` (with `std_head()`, `std_css()` and `std_sources()`), and both interfaces
  build their editors from one list (`SETUP_TABLES`); the Standard interface looks and works as before. Started from
  `Ovid_WLR.bat`, the app stops when the last tab of either interface closes.

**Fixed**
- The set-up check read the dates of manual removals, out-of-water periods and clock corrections as a column in the file date
  styles, so a column mixing dates and date-times was reported as unreadable although the run reads it; it now reads each
  cell as the run does.
- A project whose metadata folder held no workbook disconnected the app after a run: the Field visits list read an empty
  metadata table (`req(nrow(...))` passes on 0). The same guard on Deployment history and Reports and export.

**Output changes**
- Only where the new tables are used, or where a site had both ellipsoidal heights and elevations (the ellipsoidal ones are now left out).
- None: every output file of the demo project is the same, row for row, with and without the caches, and after each kind of
  edit the incremental run gives what a run from scratch gives.



## 0.11.0 (2026-10-06), needs core 1.14.0

**Added**
- Tip-to-sensor offset. `logger_offsets.csv` is seeded from the makers' specifications: HOBO U20 and U20L 0.0125 m (Onset:
  distance from sensing element to tip 1.25 cm). Solinst rows (Levelogger 5, Edge, Junior, LTC, Gold) are listed with no
  distance, because Solinst marks the sensor with a machined line rather than giving one in text; measure it once and type it in.
  `model` is now a pattern, first row first, and a `source` column says where each value came from. A project still holding
  the old all-zero table gets the seeded one on the next run (backed up in config/history).
- What the hang length is measured to: Settings (*the tip of the logger*, the default, or *the pressure sensor*), and a
  `measured_to` column (tip or sensor) on Deployment history. A row there with a site and no serial applies to every logger
  at that site. The offset is added only where it is measured to the tip.
- A form on Datum and dGPS > Logger offsets: pick a model (or one serial), then a known offset from the drop-down or *Manual
  measurement* with the distance typed in cm. It writes the row (replacing the one for that model or serial) with its source.
- A switch in Settings to leave the offset out altogether.
- The applied offset, `measured_to` and where the offset came from are shown on Deployment history, Datum and dGPS, the
  conversion check, `summary.csv`, `deployments.csv` and the run and site reports.

**Output changes**
- HOBO U20 and U20L deployments measured to the tip gain 1.25 cm: water elevation is 0.0125 m higher, depth to water
  0.0125 m smaller, and the tape-check difference moves by the same amount. Depth above the sensor is unchanged.
  The old model rows did not match Solinst files (their model reads `LT_EDGE`, `LT_Gold`), so Solinst output is unchanged
  until a distance is entered.

## 0.10.0 (2026-10-06), needs core 1.14.0

**Added**
- Tape against logger: every deployment with a tape check is compared, drift on or off. A median difference above 0.05 m
  (*Settings*, `tape_warn_m`) is more than drift usually explains, so it is listed as a likely metadata error (hang length, logger
  offset, reference point or visit time) with a hint. One line goes on the problems list; the deployments are on Field visits,
  where they can be adjusted (Deployment history, Datum and dGPS) or kept as they are (*Keep selected*, which adds a `keep` row
  to `drift_corrections.csv`). `audit/tape_offsets.csv` lists them. Nothing is changed.
- Every quality rule has a `rule_id`; `flags.csv` ends with `rule_id`, `qc_code`, `app`, `run_id`.

**Output changes**
- New file `audit/tape_offsets.csv`, one problem line when deployments differ, and four columns at the end of `flags.csv`.
  Values are unchanged.

## 0.9.0 (2026-10-06), needs core 1.13.0

**Added**
- Base time zone can be set to UTC-6 to standardise every record; a new *Zone of files that state none* setting (blank = the base zone, as before); if no file states its zone the app asks which zone before running. Defaults unchanged: output identical on the real data.
- Drift and offset correction, off by default: report the tape offsets only, or apply them (prorated linearly between tape checks,
  only where the largest is at least 0.01 m), or apply an offset typed in `drift_corrections.csv`. The logger's values are kept;
  corrected values go in `depth_m_corr`, `elev_m_corr`, `dtw_m_corr` with `drift_offset_m`, and every export can give both, the
  original only or the corrected only. `audit/drift_corrections.csv` lists what was done.
- Bulk export: one corrected-data file per site, all sites in one file and a readme.
- WMO quality codes (`_qc` columns) in `corrected/` and exports, the default; the letters remain a choice.

**Changed**
- Spike, flat-line and flag runs use the shared core functions.

**Output changes**
- `corrected/` and the export zip gain `_qc` columns. With drift off (the default) nothing else changes; this was checked on the
  2023 to 2025 data.

## 0.8.0 and 0.7.x (2026-10-05)
- Changes between 0.6.1 and 0.8.0 are not in the git history yet; see the WLR guide for the current behaviour.

## 0.6.1 (2026-10-05)
- Reads Latin-1 `.xle` files and runs without a workbook; rerun cache with Rescan.
