# Ovid CS changelog

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Docs fixes (2026-10-29)**
- Guides checked against the app: reference guide: three views (Simple first); clock corrections and manual removals in the Workbench; how-to task 9 removes a stretch through Open in Workbench. **Output changes:** none.

**Fixed (side panel, 2026-10-29)**
- With no run problems and nothing to count, the Needs you notes were a TRUE matrix instead of an empty table: a blank
  Needs you row, and the side panel ("This project") failed with "argument is not interpretable as logical". **Output changes:** none.

**Changed (Standard view: one bar, pages on the left, 2026-10-28)**
- The Standard view uses the core tabs layout as approved (option 2): stages on the top bar with the Needs you count on
  Decide, the pages of the stage on the left with their status lines, Go to (Cmd/Ctrl+K) for pages, stations and settings.
  The station strip shows at the top of Plot and explore and Overview instead of above every page. The Plots size moved
  to Settings (Plot height); Run pipeline is labelled Run.
- **Output changes**: none.

**Changed (folders under ~/Projects/Ovid, 2026-10-21)**
- Projects go in ~/Projects/Ovid/Projects/CS when none is set (was ~/Desktop/Ovid_Projects/CS).
- Default raw folder is ~/Projects/Ovid/Projects/CS/raw (was ~/Desktop/dat/RAW and ~/Desktop/Dat_Files, both retired).
- Set-up: projects and raw data kept in the old Desktop folders are not found by default any more; open them from their new place.
- **Output changes**: none.

**Changed (finding the core, 2026-10-21)**
- The launcher and pipeline look for the shared core in OVID_CORE, core/ inside the app and ../core beside it only; the old
  ~/Desktop/Ovid fallback (and its Windows Desktop\\Ovid twin) is gone. The launcher's not-found message says to keep the app beside
  core/ or run Make_Portable.bat.
- Set-up: a core kept only at ~/Desktop/Ovid is no longer found; put core/ beside the app or set OVID_CORE.
- **Output changes**: none.

**Fixed (empty derived summary, 2026-10-21)**
- audit/derived_summary.csv is written with its header when no station yields derived products, instead of leaving the last run's rows in place.
- **Output changes**: none.

**Changed (precipitation colours, 2026-10-20)**
- The "missing" fill on the precipitation flag plot is the palette's bg2 with no hard-coded fall-back (bg2 is always set).
- **Output changes:** none.

**Changed (failures reported, not hidden, 2026-10-20)**
- 4 failures now reported (a station plot, a set-up table and timezones.csv read for the Clock page, a program header); 61 defaults commented as safe. Run and Run anyway show a notice when the set-up check cannot run (it used to go on silently).
- **Output changes:** none for readable projects; a part that cannot be read or made is now on the problem list.

**Changed (core say and shared helpers, station map, 2026-10-20)**
- app.R uses core say() (it sets LOG and SAY_STEPS = 6) instead of its own copy.
- Clock corrections, the set-up editor, rule_info(), current_user() and track_session() are core's (core/time_corrections.R, shell.R, editor.R, table.R). A correction row whose start or end is not a date is now listed as a problem and skipped (it used to apply from the first or to the last record); a shift of 0 minutes is skipped without a problem.
- The Station map opens on the whole world until the stations are drawn and fitted (it used to open on a fixed region).
- **Output changes:** only for a project with a clock correction whose start or end is not a date (now skipped and listed as a problem).

**Changed (albedo daylight, 2026-10-20)**
- Albedo is computed only while the sun is within 70° of the zenith (new calibration option albedo_max_zenith; PROMICE/GC-Net, Fausto et al. 2026; the limit used to compare albedo, Stroeve et al. 2006, Liu et al. 2009, Schaaf et al. 2011), at the middle of each record, for stations with latitude and longitude in Station details. Stations without them keep the 50 W/m2 test, and the set-up check says so. The sun's height now uses Spencer's (1971) declination and equation of time (also in the night-sunlight and BSRN limit checks; the old formula could be off by up to 4° in hour angle). **Output changes:** stations with coordinates: albedo_calc in records and in daily, monthly and water-year tables (low-sun records left out, dim high-sun records kept); sunlight_at_night and sw_above_physical_limit flags can move by a record near sunrise and sunset.

**Fixed (CI again, 2026-10-20)**
- Daily, monthly, wind and water-year values are rounded to 6 significant digits after 12 (signif_stable, R/derived.R): a mean on a rounding boundary (wind_speed_max 1.293875 on 2024-07-04 in the golden project) differed in its last bits between x86_64 and arm64, so its sixth digit flipped (Mac 1.29387, GitHub 1.29388). run_all_tests.R now also prints the golden test's differing files in the failed-log excerpt. **Output changes:** a value lying on a 6-digit boundary can move by one in its last digit (one cell in the golden project).

**Fixed (CI, 2026-10-19)**
- parameter_stats.csv percentiles (p01, median, p99) are kept to 10 significant digits: the interpolated values differed in their last bits between x86_64 (the GitHub runner) and arm64 (the Mac), which failed the golden test on CI. tests/test_golden.R also compares numbers to 1e-9 of their size. **Output changes:** percentiles that carried floating-point noise (0.308399999999999) read 0.3084.

**Changed (daily albedo, 2026-10-19)**
- The day's albedo is the sum of SW out over the sum of SW in over its daylight records (SW in above 50 W/m2) also where the logger writes its own albedo column; that column stays in the station file but is no longer averaged into the daily, monthly and water-year tables (its mean holds night and low-sun ratios: -0.131 for a day whose ratio of sums is 0.125). Sources in the hub references file; the 50 W/m2 daylight test is an Ovid default with no published source found. **Output changes:** stations whose logger writes an albedo column: the daily tables have albedo_calc in place of the logger albedo's mean.

**Fixed (evapotranspiration, 2026-10-19)**
- The daily FAO-56 reference evapotranspiration (et0_fao56_mm) and Priestley-Taylor (et_pt_mm) were computed with the humidity, wind and shortwave left out of the call (a comment had taken that line in since 1.0.0), so every other argument landed in the wrong place. Both now get the day's values; FAO-56 Example 18 (Brussels) is reproduced (Rn 13.28 MJ/m2/day, ET0 3.88 mm/day). **Output changes:** every et0_fao56_mm and et_pt_mm value, in the daily, monthly and water-year tables.

**Added (tests, 2026-10-19)**
- Reader samples for every format CS reads: a TOB3 table (every column of its 145 records equal to LoggerNet's TOA5 copy of it within its 7 printed digits), a CR10X mixed array with its .FSL, and a CR200X table with no serial. tests/test_bad_files.R (damaged files, a program change partway, a table re-initialised, NAN/INF/-7999, two intervals), tests/test_golden.R with tests/golden (a Pluvio2 gauge with the single-Alter undercatch correction and a met station: a whole run against accepted outputs), tests/test_formulas.R (FAO-56 Example 18; the rest hand-computed); all run by tests/run_tests.R. **Output changes:** none.

**Fixed (damaged files, 2026-10-19)**
- A TOA5 file that ends inside a row (no final line break) has that row left out and the file reported, not a value cut short read as whole. A new program written into the same file (a second TOA5 header with other columns) is read up to it and reported, not read on under the first header's names; a repeated header with the same columns is skipped. An emptied cr10x.csv no longer stops a run. **Output changes:** only for such files.

**Changed (toggles, 2026-10-14)**
- Settings (CR10X files, reuse unchanged loggers), Same scale, Station names and the "Only ..." filters are toggles; export and dialog choices stay checkboxes. **Output changes:** none.

**Added (corrections applied, 2026-10-10)**
- Corrections applied: a chip and pop-up in the top bar list every correction in the last run with counts (core applied.R); output/audit/corrections_applied.csv, also in exports and reports. **Output changes:** new file; period removals name their row in the audit reason.

**Changed (one review list, project menu, time zone, 2026-10-09)**
- Needs you shows the one review list; Home's More section, tiles and "Worth a look" fold into Needs you and the set-up check; the Check the data and "Saved, and next time" steps fold into Needs you and Home (Next time, Recent changes). **Output changes:** none.
- The Workbench Review queue tab is gone; its items are in the Workbench's Needs you.
- Settings lists the projects; the project menu is in the top bar. The time zone is set in the one core control and saved with the project.

**Changed (fewer controls, 2026-10-09)**
- Stations: one Station picker and Apply; the Deployment sheet is shown below the table (a logger that moved gets a row per period). Built-in station names (none shipped), Clear name, Set aside / Bring back (name a logger `ignore`), the per-header-name box, Suggest names and the moved-logger form are gone; the station table stays under the sheet. **Output changes:** none.
- Precipitation: the bucket-only window and the final series are set in the gauge table's columns and applied with Reprocess precipitation only; the two Apply and reprocess panels are gone. **Output changes:** none.
- Plot and explore loses Export station report (PDF); Export's one Report formats choice serves the zip and Export report (the first station picked). **Output changes:** none.
- What the scan will show (file types found, readable, matched, new and changed size) is shown with the folders on Project and Import data, in place of the Home card, the folder count and the Workbench scan tiles. **Output changes:** none.
- Metadata loses Source files (Raw files lists them), and Plot and explore and Precipitation lose Data sample; Precipitation's Export full table (CSV) sits by Export plot. **Output changes:** none.
- One programs folder and one upload (.FSL too) on Logger programs feed the CR10X matching; the CR10X FSL folder and upload are gone (an old FSL folder setting becomes the programs folder). **Output changes:** none.
- CR10X files: Write .dat files with headers is gone; CR10X files are used through the pipeline. **Output changes:** output/cr10x_with_headers is no longer written.
- Statistics is no longer a page: Break down by and Statistic are on Plot and explore > Summaries, for the station and period of the first tab. **Output changes:** none.
- Initials are asked once per session and go on every field-log entry; the Your initials fields start with them. **Output changes:** none.
- Plot and explore: group picking, statistic, draw style, one panel per group, file-start marks and the battery threshold are gone (mean, automatic style, 11.5 V). **Output changes:** none.
- Quality rules: Manual removals and Calibrations are rows of Corrections over a period (actions remove, scale, offset; new columns stream and output). An older project's manual_removals.csv and calibrations.csv are moved in once on first read and left in place. **Output changes:** none for those rows; rows already in Corrections over a period that remove values now do so with the quality rules (they are counted as removed and listed in output/qc), and a row naming a logger changes only that logger's records. Station tables are recomputed once.
- Split here on Needs you writes two rows of the Deployment sheet (before and after the gap) instead of the hidden logger_splits.csv; older splits are moved in once. **Output changes:** none.

**Fixed (review decisions and set-up notice, 2026-10-09)**
- Check the data: Keep is stored with the review queue's decisions (config/review_decisions.csv) and the item leaves both lists; Flag works (kept there as flagged).
- A run reads review_decisions.csv: a flag decided not a problem stays in flags.csv marked `accepted` and leaves the exported flag and _qc columns.

**Fixed (set-up review, 2026-10-09)**
- Run pipeline saves every set-up table first, Calibration options and Corrections over a period too, and the Set-up check follows them. **Output changes:** none.
- The question before a run with set-up errors is the shared one (core setup_gate). **Output changes:** none.
- Stations page: the table and Apply / Set aside / Bring back work again (a name clash with the set-up flow). **Output changes:** none.
- Settings > Time zone now applies to runs: a station with no utc_offset_hours is taken to be in it (its own offset still wins), and NetCDF uses it in place of UTC. **Output changes:** yes, for stations with no utc_offset_hours: time-zone rows now move their files into the base zone, and their NetCDF times are taken from the base zone.
- Deployment sheet: only a row matched by its dates is applied on its own; a row with no dates, or only a year or month, is a suggestion on Needs you. **Output changes:** yes, a logger named only by an undated sheet row is no longer given that station until the row has dates.
- Time zones (timezones.csv) is a set-up table with its own editor (Clock checks, Time zones), and the Set-up check checks it. **Output changes:** none.
- Combined records: saving or deleting one marks the run out of date, and the export zip carries output/composites. The flow and Workbench call the step Combined records. **Output changes:** export zips gain composites/ when there are combined records.
- Clock corrections made from a selection (Raw files "Remove these records", Workbench Clock) are prefilled in the raw file's own clock times. **Output changes:** none.
- Home: "Bring your own metadata" opens Stations, where the Deployment sheet is imported; the HOBO weather claim is gone (CS reads Campbell .dat files). **Output changes:** none.
- Messages that sent users to Home or Data source for the folders now name the Project step; Automatic corrections and station map wording corrected. **Output changes:** none.
- Configuration export includes prog_dir and the Automatic corrections switches. **Output changes:** none.
- Removed a handler for a station jump box that no longer exists. **Output changes:** none.

**Fixed (damaged run record, 2026-10-09)**
- A damaged output/audit/run_manifest.csv no longer stops the Set-up page with an error; it is read as empty. **Output changes:** none.

## 1.11.0 (2026-10-10), needs core 1.15.0

**Changed (no site data in the code, 2026-10-09)**
- Built-in site tables removed from R/config.R: per-logger thermistor depths, CR10X position layouts, per-logger aliases, gauge
  calibrations, the pit QC row and the built-in station names and positions. They belong in a project's own config (aliases.csv,
  calibrations.csv, qc_rules.csv, station_info.csv). The general rules stay (sensor N at N x 10 cm, CR10X reading by position,
  NOSN_/CR10X_ ids). Examples, map start view and tests use neutral names and coordinates. **Output changes:** none for a project
  whose config already holds those rows; a new project no longer gets them.

**Changed (sample tests, 2026-10-09)**
- Sample tests: a new or changed sample fails until accepted (tests/samples/accepted.csv); hand-checked values in reference.csv. **Output changes:** none.

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). **Output changes:** none.

**Changed (time zone, 2026-10-09)**
- Settings has a Time zone. The export choices are the stations' own zones, the project's time zone or UTC, in place of a fixed UTC-6/UTC-7.

**Output changes:** none unless a different zone is chosen.

**Changed (errors reported, 2026-10-09)**
- A station table that cannot be read, clock checks that fail, and a report or chart that cannot be made are now listed as problems. Before, they were left out with no notice.
- An export lists anything it could not make in problems.csv.

**Output changes:** exports may gain problems.csv; the data are unchanged.

**Changed (packages, 2026-10-08)**
- The app and its pipeline no longer install packages when they start: a missing one is named, with how to get it (the launcher installs the versions in renv.lock). **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- The Standard home's CSS (home.R) is www/home.css; the text is the same character for character. **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R from 417 to 186 lines: its helpers moved to pages/fct_outputs.R (outputs and the viewer bundle), pages/utils_tables.R (theme, table style, audit rules) and pages/utils_setup.R (set-up editors, runs, settings, history, sessions), sourced in the same place. Code unchanged line for line. **Output changes:** none.

**Changed (2026-10-08)**
- Pipeline tabs are tighter (smaller padding and type), so they take two rows at 1440 px instead of three. Audit against the approved layout: tabs, station strip, info cards, plot with Smoothing, station panel and daily table were already in place. **Output changes:** none.

**Changed (2026-10-08)**
- Standard before import: the middle choice is "Bring your own metadata" (opens Loggers and metadata, where a CSV or Excel file is imported and its columns matched). Removed: Configuration > "Pull from the station metadata" (a folder of set-up CSVs) and its Metadata folder setting; bring the rows in with Import on each table instead. **Output changes:** none.

**Added (2026-10-08)**
- Group the logger list by serial number, deployment (stations.csv rows), download or raw file, on Stations and in the Workbench
  tree (`pages/grouping.R`, core grouping.R); moves found in the records in Needs you; projects Rename and Duplicate; errors under
  their field in the Workbench dialogs; greyed correction buttons say why; a Columns picker on wide tables. **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R split from 2,818 to about 415 lines: each page is its own file in pages/ (sourced inside the server, same order),
  the interface is pages/ui.R, and the CSS is www/cs.css. Code unchanged line for line. The viewer bundle now also copies
  home.R (it was missing) and pages/. **Output changes:** none.

**Changed (2026-10-08)**
- Only the raw folder is needed: with no project folder the app uses ~/Desktop/Ovid_Projects/CS/Untitled; no metadata is needed
  (loggers go by serial). Test: `tests/test_no_metadata.R`. **Output changes:** none.

**Added (UI design, 2026-10-08)**
- Standard home (`home.R`): before import, three ways in and what a scan of the folders will show; with data, info cards, air
  temperature with the mean of the other stations, parameter chips, small humidity and net-radiation plots, the station panel and a
  daily summary table (Find, Columns, Export). The "+N" station chip is a searchable list. Reference page; Workbench pipeline tabs.
  **Output changes:** none.

**Added (set-up Workbench design, 2026-10-08; core workbench 0.9.0)**
- Workbench: every set-up table is a spreadsheet with a formula bar; **Correct a period** in the cell bar; **Needs you** at the top
  right; the **Review queue** tab; a "CS processing" section in the tree that opens the app's own steps.
- Corrections over a period are applied after the calibrations and before the quality rules (Quality rules > Corrections over a period).
  **Output changes:** none until a project has rows in period_corrections.csv.

**Changed (set-up flow, shared core shell 1.2.0, flow 1.0.0, tsview 1.0.0, workbench 0.8.0)**
- The numbered set-up flow, the project switcher, the information panel, Check the data and the Standard view plots (see core
  CHANGELOG); table headings shown as words with units (`ovid_colnames()`).
- Workbench: units under the grid headings, quality-rule range limits on the chart, chart selection statistics in the inspector.

**Fixed (run-through on the Mac, 2026-10-08)**
- The app no longer greys out after an error in a handler, notices a deleted output folder, and shows damaged set-up tables with
  their columns restored (core). Pending removals are drawn only on their own parameter in the Workbench chart.
- Raw files page: no error when the run manifest has no path column (output deleted or damaged); Statistics and Plot and explore:
  no error when the output has no column dictionary; the Workbench says "run the pipeline" when a station file is gone.
- The note after importing folder settings says they are saved as they change.
  **Output changes:** none.


**Added (projects, renames, report layout)**
- Projects list on Home (core `projects.R`): new project, open, delete outputs or the project to the Trash; each keeps its own folders and settings.
- Rename a station everywhere (Configuration; core `rename.R`): Stations, Station details, manual removals, file rules, gauges, field log, composites, the deployment sheet, and the stations named in quality rules and calibrations.
- Report layout on the Export page (core `report_options.R`): titles, file names, a line under the title, who prepared it, sections, and each parameter on the same scale at every station (from its 1st to 99th percentile).
- A station typed in stations.csv that differs from the deployment sheet's is listed (the typed one is used).

**Output changes:** none.

**Added (automatic corrections list)**
- Settings: an Automatic corrections list (core `corrections.R`) of everything a run changes in the data on its own, with
  what each does, whether it is on, and where it is switched; it adds switches for leaving out reset-clock records, removing logger error codes (both on by default) and which file wins where two disagree (newest by default; now also where loggers are combined into a station), and switches for leave out stray records, cross-sensor checks and neighbour checks, which had no control in the app. Each run log lists them.

**Output changes:** none.

**Added**
- Needs assessment, second batch (medium items):
  - Daily evapotranspiration in the summaries: FAO-56 Penman-Monteith reference ET (`et0_fao56_mm`; Allen et al. 1998, checked
    against their Brussels example) and Priestley-Taylor (`et_pt_mm`, for wetlands), from the day's air temperature, humidity,
    wind and shortwave or net radiation; needs the station's latitude (Station details; new `wind_height_m`, blank = 2 m).
    Summed in the monthly and water-year tables.
  - Soil water in frozen ground: a daily `vwc_..._liquid_only` marks days the soil at that depth froze (a dielectric probe then
    sees the liquid water only).
  - Estimated daily means (`output/derived/daily_estimated.csv`; level L3): a missing daily mean of the between-station
    parameters (never precipitation), estimated from the station that tracks it best (r2 0.9 or more over 60 days, gaps up to
    31 days). Nothing in a station table is changed.
  - Breakpoints (`output/audit/breakpoints.csv`): Pettitt's test on each station's monthly difference from the other stations
    finds a shift (sensor swap, move, recalibration); reported, never adjusted. Explore > Against the neighbours shows both.
  - Export: short gaps filled by straight lines on request (air temperature and humidity up to 2 hours, pressure up to 3;
    WMO code 6), and NetCDF (CF-1.10 / ACDD-1.3, times in UTC, CF standard names, every value's WMO code as CF flags; core
    `standards.R`). The readme lists the processing levels.

- Paste and Import in every set-up table: rows copied from Excel, or a CSV or Excel file with its columns matched to the
  table's by heading, added to the table or used instead of it.
- Workbench interface (preview), `workbench/` on core `workbench.R`: an additional, more detailed view of the same project with
  every process of the Standard interface.
  - Every Standard page runs inside the Workbench on the Standard server (`server(embedded = TRUE)`): overview, plots, statistics,
    sensor health, precipitation, combined records, map, raw files, clock checks, quality rules, audit trail, reports and export,
    set-up check, configuration import and export, stations, column names, logger programs, CR10X files, sites and field log,
    history, loggers and files, logger history, parameter guide. They are listed under Pages in the tree and open as tabs.
  - Data: the station tree, the selected parameters charted over a spreadsheet grid, and an inspector with the selected cell's
    value, state, lineage (source file, table, the rule that removed or flagged it) and the quality rule that covers it.
  - Selecting: click a cell, drag across cells, shift-click to extend, cmd/ctrl-click to add; click a time to select the whole
    record, a parameter heading for the whole column, the corner for everything; arrows and shift-arrows, Esc, cmd/ctrl-A. The
    chart highlights the selected values (a band over each run of records and a mark on each value); the inspector sums the
    selection up per parameter (n, mean, min, max, removed, flagged), and can zoom the chart to it.
  - The chart zooms: drag across it to zoom to that stretch, wheel or + and - to zoom in and out at the pointer, alt-drag,
    sideways wheel or the arrows to move along, double-click or Reset for the whole period. Shift-drag (or Drag: select) selects
    the stretch of one parameter in the grid, and a click the nearest record; a tooltip gives the values under the pointer. A long
    period is drawn from the low and high of each stretch of time, and zooming in loads every record of the window. Use as period
    makes the stretch in view the period, so the grid lists it.
  - Corrections on the selection or a brushed period: **Remove** (rows in `manual_removals.csv`, one per run of records, for that
    station table; R or Delete), **Restore** (deletes the manual removals that cover the values), **Calibrate** (`calibrations.csv`),
    **Clock** (`time_corrections.csv`, for the logger tables behind the records), **Note** (`field_log.csv`), and for a value a rule
    removed or flagged, **Open rule** and **Rule for <station>** (a copy of the rule in `qc_rules.csv` for that station only). Each
    dialog shows the rows it writes. The grid and chart mark saved corrections that the next run applies.
  - Set-up tables in the tree open the Standard page and tab that edits them. Import data holds the Standard folder and run
    settings with a scan of new and changed files and the set-up check; Run is the Standard run (save, check, run).
  - Export view writes the rows and parameters shown as CSV. A viewer copy opens the Workbench read-only.
- Choosing the interface: **Workbench** in the Standard title bar, `?ui=workbench` / `?ui=standard` in the address, else
  `OVID_UI`, else the `ui` setting. Standard is the default. Both run in one app process on the same project.

**Changed**
- Deployment sheet (core): Stations page; a logger stations.csv leaves without a station takes it from the sheet, one row per
  period. Set-up check and editor read each table's rules (core); a manual removal or clock correction naming an unknown station
  or logger is pointed out. "Run anyway" is recorded in the run log.
- Guardrails (core `run_project()`): see core. "Newest" download where two files disagree is now the one whose own record
  runs latest, not the newest file date (which copying to another drive changes). CR10X rows in cr10x.csv follow their file
  to a new drive letter or folder and keep their logger and FSL settings. TOA5 files with blank lines read. Tests: guardrails.
- Neighbour checks use core `neighbours.R`: a seasonal difference between stations is no longer flagged, and the flag detail
  gives the largest departure. TOA5 headers, file listing, checksums, gaps and flags use core code (output unchanged).
- Run happens in the background (core `runner.R`): the app stays usable, a box at the bottom right shows the step, the logger
  table or station being worked on, and Cancel. Rescan uses it too. The first run after updating redoes the reading steps once.
- Every run leaves a log in `output/logs/`, and the lifebuoy in the top bar saves a troubleshooting zip (core). Tests now also read real sample files and click through every page in a browser.
- Each cached step is keyed on the code it runs (core `code_fingerprint()`, `STEP_HASH`), not on every file: an edit elsewhere no longer redoes it. The first run after this redoes everything once. `load_configs()` is renamed `cs_load_configs()`.
- Spans in clock-check and stitch audit lines are written by core `fmt_span()`: 60 to 120 min now reads in minutes
  ("90 min", was "1.5 h"), to 0.1 min. Clock corrections use core `correction_span()` / `shift_times()`; output unchanged.
- A run redoes only what an edit can change. A station table is rebuilt when its records change, or when a quality rule,
  calibration or manual removal that applies to it and names one of its parameters changes; a rule for snow depth no longer
  rebuilds the stations without snow depth. An alias redoes only the loggers whose columns it renames. Editing parameter
  groups or the parameter guide rebuilds no station (they are used after the stations). Changes to the interfaces (core
  `shell.R`, `workbench.R`, `files.R`, `launcher.R`) no longer redo anything. A full rebuild happens on the first import, when
  the pipeline code or a run setting changes, or with incremental runs switched off.
- The message at the end of a run says what was redone, for example "Redone: 1 of 26 station tables; the rest was unchanged
  and reused."
- The Standard pages are built by `std_pages()` (with `std_head()`, `std_css()` and `std_sources()`), so both interfaces show the
  same pages; the Standard interface looks and works as before.
- Both interfaces build their table editors from one list (`SETUP_TABLES` in app.R) and run through one function (`app_run`).
  The editor and the Workbench's data view are now core's (`editor.R`, workbench 0.5.0), shared with WLR and GT. A
  table open in both interfaces, or in two windows, takes up a version saved in the other within two seconds when it has no
  unsaved edits; with unsaved edits, Save asks before it overwrites the other version. Corrections added from a page (a removal
  dragged on a plot, a clock correction, a field log entry) start from the saved version, so none is lost.
- Started from `Ovid_CS.bat`, the app stops when the last tab of either interface closes (it counted Standard tabs only).
- Screens and plots use olive-crt's full Neovim palette (lua/olive_crt/palette.lua) instead of its terminal colours: pages on
  its editor ground #171a15, the title bar and rail on its status-line ground, cards on its side-panel ground, dividers in its
  selection colour, pane edges in its border colour, olive accent, teal for ok, its own rust for orange. Dark only.

**Fixed**
- A project with no precipitation gauges wrote `precip_summary.csv` without its `stream` column, and reading it warned.
- After a correction the Workbench grid kept its selection, and opening a set-up table from the inspector (Open rule, Rule for
  a station) did not reach the page: the Workbench script's message handlers were refused by Shiny (core workbench 0.5.0).

**Output changes**
- Summaries gain `et0_fao56_mm` and `et_pt_mm` where a station has a latitude and the inputs, and `vwc_..._liquid_only`
  where a soil temperature sits at the same depth.
- Plot and report colours only. Values are unchanged; a run that reuses tables writes the same files as a full run (tested).
  `precip_summary.csv` of a project without gauges gains its `stream` header. The new cache keys and code make the next run
  redo all loggers once; after that only what changes is redone.

## 1.10.0 (2026-10-06), needs core 1.14.0

**Added**
- Default spike and step limits for new projects (air temperature, humidity, battery, longwave, snow depth, ground and soil
  temperature, water content), set wide so they catch sensor glitches and not weather (Zahumenský 2004; Estévez et al. 2011).
- Spike and step tests run only on tables recorded every 3 h or more often (`spike_step_max_interval_h`); a spike must also stand
  away from the median of the 5 readings around it, and a step is never taken as a rate over less than an hour (1-minute tables).
- *Fill in suggested spike and step limits* on Quality rules: fills only blank cells of the standard rows of an existing project.
- Every quality rule has a `rule_id`; `flags.csv` names the rule behind each flag and ends with `rule_id`, `qc_code`, `app`, `run_id`.

**Output changes**
- New projects remove single-point spikes and flag fast steps by default. Existing projects keep their rules until the button is
  pressed or limits are typed. `flags.csv` gains four columns.

## 1.9.0 (2026-10-06), needs core 1.13.0

**Added**
- Export option *Times in*: as stored (default), UTC-6 or UTC-7, moving each station from its utc_offset_hours and adding a time_zone column; stations without an offset are left as recorded unless a zone is chosen, and the page asks when none has one.
- Flag-only checks: incoming sunlight above the BSRN physically possible limit, incoming longwave outside 40 to 700 W/m², relative
  humidity above 103 %, a possibly frozen anemometer (no wind for 3 h below 0 °C and above 90 % humidity), rain recorded below freezing.
- A rate-based step test (`max_step_per_hour` in the quality rules, empty by default).
- `dew_point_calc` (Magnus formula, Alduchov and Eskridge 1996).
- Daily Yamartino spread of wind direction and the resultant wind speed and direction.
- Weighing gauges: the 10 m coefficient set of Kochendorfer et al. (2017) when `wind_height_m` is 5 or more, and a low and high
  corrected value from the paper's 0.2 uncertainty.
- WMO quality codes in exports (`_qc` columns), now the default; the S, R, M letters remain a choice on the Export page.

**Changed**
- Spike, flat-line and flag runs use the shared core functions.

**Fixed**
- Wind direction is averaged on the circle (daily, monthly, water year); the arithmetic mean was wrong near north.

**Output changes**
- `derived/`: wind direction means change where winds straddle north; new columns `dew_point_calc`, `wind_dir_sd_calc`,
  `wind_speed_resultant_calc`, `wind_dir_resultant_calc`. `flags.csv` gains the new checks. Precipitation tables of gauges with
  undercatch correction gain `precip_corrected_low_mm` and `precip_corrected_high_mm`. Exports have `_qc` columns by default.
  Removals are unchanged.

## 1.8.0 (2026-10-05)
- Pull from a metadata exchange folder (removed 2026-10-08), station map, combined records.

## 1.7.1 (2026-10-05)
- TOB3 binary files read directly; duplicate check fixed; CRBasic lines that do not start with a name no longer break the parser.
