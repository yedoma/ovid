# ovid GT changelog

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Changed (one way of writing things across GT, 2026-10-30; Plot_table_standards A4, A8)**
- Every chart axis names its quantity: "Temperature (°C)" (was "°C" on the MAGT, depth and Simple charts), "Battery (V)",
  "Active-layer thickness (m)".
- Compare's legend sits at the top, as on every other GT chart.
- Site notebook: the site's name at the page-title size (18 px; was 22 px, larger than the page title).
- Dates and times as 2024-04-04 and 2024-04-04 12:09 in the run status and the Simple view; Simple downloads "(ZIP)".
- **Output changes:** none (screen and report figures only).

**Fixed (found by the click-through test, 2026-10-30)**
- The heatmap (Sensors, Site notebook, Explore) failed with the test project: its colour limits came out missing (no finite temperature in the period, or a fixed scale with empty boxes); it now falls back to the automatic scale, or says there is not enough data. Simple, View: "Not named" broke the tiles (f[!named] is a not-join in data.table).
- A missing file whose path holds a space was run as a shell command by fread(): every file read names it as file=.
- **Output changes:** none.

**Changed (speed, 2026-10-30)**
- Building site records: the between-site flags find each sensor's temperature rows once, not by scanning every row for every flagged period (between_site_mark, R/qc.R); a test compares it with the old code. **Output changes:** none.
- Stitching and site records: a reading's place among those of its channel and time, and the first value there, come from one pass over rows already sorted, not from grouping them again (run_starts, run_rank in R/stitch.R); a site's values and flags are cast to its wide file at once, not cast apart and merged (site_wide in R/export.R). Tests compare both with the old code. **Output changes:** none.
- Building site records: the daily means and the between-site check are kept in the cache with the keys of every site's records and the settings; a run whose sites are all unchanged reads them back, a change at any site works them out again. A test checks it. **Output changes:** none.
- Finding files reads the headers of files not seen before in parallel (core par_map; one at a time on Windows), noting what they find in file order as before; the air-like check matches channel and day only on the rows of channels that have a bad day. A test compares the second with the old code; the 1-and-2-workers run test covers the first. **Output changes:** none.
- Reading and stitching loggers reads the raw files of loggers that are not reused in parallel (core par_map, as in WLR), a few loggers at a time; problems are recorded in the same order as before. On Windows it reads one file at a time as before. **Output changes:** none.

**Changed (names, 2026-10-29)**
- excel_time is now excel_datetime (it returns a date-time; WLR's excel_time returns seconds after midnight). **Output changes:** none.

**Changed (Run, 2026-10-29)**
- The top bar's Run pipeline button reads Run, as in CS; guides and the Standard figure updated. Reference guide: degree days and n-factors (12b), as in the hub. **Output changes:** none.

**Docs (citations, 2026-10-29)**
- Reference guide: ALT's permafrost test is two consecutive frozen years (van Everdingen 1998), as the code and the hub say. **Output changes:** none.

**Changed (hints, 2026-10-29)**
- Plot and explore and Quality rules say that stretches are removed in the Workbench (they said to drag across a plot in Standard). **Output changes:** none.

**Fixed (metadata folder, 2026-10-29)**
- No metadata folder unless one is set (it defaulted to the projects folder's GT/project/metadata, so a test or new project read another project's registry). The click-through test reads only its test project. **Output changes:** none.

**Docs fixes (2026-10-29)**
- Guides checked against the app: reference guide: three views (Simple first); a pre_install stretch is removed in the Workbench. **Output changes:** none.

**Docs (2026-10-29)**
- New quick start and how-to guide (docs/GT_quick_start.*, docs/GT_how_to.*: md, html and pdf), built by docs/build_guides.R in
  the reference guide's colours, with figures from the test project. **Output changes:** none.

**Fixed (side panel, 2026-10-29)**
- With no run problems and nothing to count, the Needs you notes were a TRUE matrix instead of an empty table: a blank
  Needs you row, and the side panel ("This project") failed with "argument is not interpretable as logical". **Output changes:** none.

**Added (the Simple view, 2026-10-28)**
- simple/simple_gt.R: the Simple view (?ui=simple; a new install opens in it, an install with settings in app_settings.rds keeps
  Standard). Import: the folders, Read the data and a checklist (downloads read, clocks fixed, every string at a site, site names,
  sensor depths set or assumed, places) with the strings' files. View: filters (all, need a look, not named, depths assumed) and a
  search, a tile per site (depths, * when assumed; last record; MAGT at the deepest sensor; ALT of the last complete year; the
  registry's name for a site still named after its logger; amber when stopped or a sensor reads like air, with a link to Standard),
  and a plot over time (daily means by depth) or as a profile (last complete year, with the ALT). Edit: names, places, land cover,
  sensor depths (set by hand) and which site each string is at, in sites.csv and sensors.csv only (a rename through rename.R);
  "Use registry names" (registry_dir setting; REGISTRY_NAMES in R/config.R) and "Use <model> template" add their changes to the
  list to review. Sensor depths have their own editor in Edit (opened from a tile's Set depths, the site's card, or "n assumed ·
  next"): one box per sensor in string order (assumed ones dashed; a leading + for a sensor above the ground, e.g. +0.10, kept as
  Sensors and depths keeps it; comma decimals read), a live profile with the mean temperature by depth (a depth to fix
  in red), Even steps, Copy from site, Use <model> template and Paste a list (which only fill the boxes), then Add to changes, Apply
  to all sites of the same model and sensor count, or Save as template for <model> (sensor_templates.csv), all through the list of
  changes with Save and Undo. Export: the field check report (HTML), daily temperatures and plots (zip).
- Quality rules, clock corrections, corrections over a period and ground-surface changes are edited by hand only in the Workbench
  (wb_only); removals drawn on Plot and explore are offered in the Workbench only. ?page= opens a Standard page from a link;
  GT_REGISTRY_DIR sets the registry folder. Tests added.
- **Output changes**: none.

**Changed (folder layout, 2026-10-21)**
- Default raw folder is ~/Projects/Ovid/Projects/GT/raw (logger files only); the metadata folder is in the project (project/metadata).
- **Output changes**: none.

**Changed (folders under ~/Projects/Ovid, 2026-10-21)**
- The default raw and metadata folders are ~/Projects/Ovid/Projects/GT/raw/data and raw/metadata (were ~/Desktop/GT/...); projects go in
  ~/Projects/Ovid/Projects/GT.
- Set-up: projects and raw data kept in the old Desktop folders are not found by default any more; open them from their new place.
- **Output changes**: none.

**Changed (finding the core, 2026-10-21)**
- The launcher and pipeline look for the shared core in OVID_CORE, core/ inside the app and ../core beside it only; the old
  ~/Desktop/Ovid fallback (and its Windows Desktop\\Ovid twin) is gone. The launcher's not-found message says to keep the app beside
  core/ or run Make_Portable.bat.
- Set-up: a core kept only at ~/Desktop/Ovid is no longer found; put core/ beside the app or set OVID_CORE.
- **Output changes**: none.

**Fixed (sensor assignment, HOBO battery column, 2026-10-21)**
- assign_sensors(): the sensor mask was taken before the no-depth removal shortened the table, so with default depths off the outside-deployment removal could drop the wrong rows.
- A HOBOware CSV column with a unit ("Batt, V") is a measurement, not an event: it no longer adds one event per record to audit/events.csv (as WLR).
- **Output changes**: events.csv loses the "Batt" rows for U23 exports with a battery column; values unchanged.

**Fixed (scan table colours, 2026-10-20)**
- The before-import scan table's ok / no words take the scheme's green and yellow (gold when it has none) instead of fixed fall-back colours (GT set no --yel variable).
- **Output changes:** none (screen colour only).

**Changed (failures reported, not hidden, 2026-10-20)**
- 13 failures now reported (the calibration table, sensors.csv, the deployment sheet, each part of a site report, the report axis limits); 31 defaults commented as safe. Run and Run anyway show a notice when the set-up check cannot run.
- **Output changes:** none for readable projects; a part that cannot be read or made is now on the problem list.

**Changed (core say and shared helpers, 2026-10-20)**
- app.R uses core say() (it sets LOG and SAY_STEPS = 8) instead of its own copy.
- Clock corrections (core/time_corrections.R), the HOBO CSV rows and events (core/logger_files.R), the set-up editor, rule_info(), current_user() and track_session() are core's. A correction row with an unreadable date or minutes is now listed as a problem. A HOBO CSV with a second heading of other columns (two exports joined) is read up to that heading and the rows after it are listed as a problem, as WLR does (M, 2026-10-20; it used to fail with a fread error and the whole file was left out); a repeated identical heading line is no longer read as a record.
- **Output changes:** only for a project with an unreadable clock correction row or a HOBO CSV with two exports joined or a repeated heading.

**Added (tests, 2026-10-19)**
- Reader samples for every format GT reads: a second .f2b (the download behind the fg2_csv sample, decoded row for row the same as its FG2-Shell V3.93a export: 184,584 values, times, NoReply and battery), an EM50 workbook, a Campbell TOA5 table, a .hobo header and a Ruskin Excel export (its first 60 rows as .xlsx), with reference values from the raw files and vendor exports. tests/test_bad_files.R (damaged files), tests/test_golden.R with tests/golden (a whole run against accepted outputs), tests/test_formulas.R (derived values against worked cases); all run by tests/run_tests.R. **Output changes:** none.

**Fixed (damaged files, 2026-10-19)**
- A text file that ends inside a row (no final line break) has that row's values marked "last row cut short", not read as whole values (a cut 23.52 read as 23.5). **Output changes:** only for such files: their last row is removed, with the reason.
- FG2-Shell exports read each row by the header above it: a parameter set that changed the channels shifted the earlier rows' values into the wrong channels. **Output changes:** only for exports whose headers name different channels.
- A file with its header only (FG2-Shell, HOBOware, TOA5, RBR .hex) is reported as holding no readings, not as an unreadable file with an internal error. A raw folder whose files are all empty or header-only stops with a message naming each file, not an internal error. **Output changes:** none.

**Changed (toggles, 2026-10-14)**
- Plot layers (0 °C isotherm, sensor depths), One axis for every depth and Complete years only are toggles; export choices stay checkboxes. **Output changes:** none.

**Added (corrections applied, 2026-10-10)**
- Corrections applied: a chip and pop-up in the top bar list every correction in the last run with counts (core applied.R); output/audit/corrections_applied.csv, also in exports and reports. **Output changes:** new file; period removals name their row in the audit reason.

**Changed (Over time on the standard plot, 2026-10-09; plot and table standards A11)**
- Full profile, Individual depths and the QC view are one tab, Over time, on the standard plot (as CS), shown two ways:
  every depth together, or one panel per depth with flagged readings as gold rings and removed ones as grey crosses; the
  flag and removal tables sit under it.
  Drag selects a stretch and dragging up a panel's value axis narrows it to a band (the old box). The edits are on the bar
  above the selection: remove or flag the points in the band, remove a depth (or every depth) in the period, record starts or
  ends here; each needs a reason, goes to Corrections over a period and has Undo. A depth is written in the project's convention. Logger changes are dashed lines.
  Export plot (PNG) gives way to Save image; the axis options of the three tabs give way to the plot's own scale.
- Output changes: none.
**Changed (one review list, project menu, time zone, 2026-10-09)**
- Needs you shows the one review list; Home's More section and "Worth a look" fold into Needs you and the set-up check; the Check the data and "Saved, and next time" steps fold into Needs you and Home. **Output changes:** none.
- Save settings and Save folders are gone (both save as they change). The Workbench Review queue tab is gone.
- Time zone: one core control in Settings; the unzoned-file offset field is gone (the app still asks). **Output changes:** none.
- Settings lists the projects; the project menu is in the top bar.

**Changed (fewer controls, 2026-10-09)**
- Sensors and depths: the Logger list tab is gone; its columns are optional columns of the Deployment sheet (From and To there). An older metadata.csv is moved in once and left in place. **Output changes:** none.
- Quality rules: Manual removals are rows of Corrections over a period (remove, flag; columns serial, sensor, depth_m, min_value, max_value). An older manual_removals.csv is moved in once. **Output changes:** existing period remove and flag rows now run with the quality rules and are audited as manual / manual_flag.
- Split here on Needs you writes two rows of the Deployment sheet (a logger it does not list yet gets its site from sensors.csv) instead of logger_splits.csv. **Output changes:** none.
- Sensors and depths: the "Set the depths of one logger" form is gone; depths are typed in the Sensors table or the logger list's depths column.
- Settings: the shallow/deep profile cut is gone; it is fixed at 3 m and the profile column of Sites still decides per site.
- Quality rules: the Calibration options tab is gone; the permafrost test (consecutive frozen years) is a setting under Summaries, default 2. A project with a calibration_options.csv keeps its value for all sites (rows for one site are no longer read); the old file is left in place and an old configuration zip imports it as the setting.
- Every on/off switch (RBR clock drift, estimated days in annual statistics, air-like sensors, record start before installation, RBR export check) is on the Automatic corrections step; Settings keeps only numbers. Saved switches are kept.
- Rarely changed limits are fixed in code with their old defaults: reset-clock year, zero-curtain offset listing, ground-surface depth, gap filling, gap and spike factors, air-like and pre-installation limits. These depths are now always metres below the ground. **Output changes:** only for a project whose saved values differed from the defaults, or with depths positive up.
- Heatmap and Site results: one "Smooth over (time steps; 0 = off)" field replaces the Smoothing On/Off switch.
- Reports and export: one report picker (run report, a site, or every report as a ZIP; HTML or PDF). The export ZIP no longer holds reports, the Summaries ZIP button and soil tab and the Site results CSV button are gone (soil has its own page and is in the export ZIP).
- Set-up flow: "Sites: rename, what shows" and "Sites and landcover" are one step.

**Fixed (runs, depths, settings and the configuration, 2026-10-09)**
- Run pipeline saves the unsaved edits of every set-up table and the logger list, not only nine of them.
- The set-up question before a run is the shared one (as in CS, RS and ERT).
- Depths typed in manual removals and Compare sites follow the project's depth convention; rows written from a plot are written in it.
- The export writes depths in the project's convention and the readme and metadata/sensors_and_depths.csv say which. **Output changes:** only for projects with depths positive up.
- Between-site flags reach the site files' flag column, the export and its WMO codes, not only flags.csv. **Output changes:** sites that depart from the others now carry the flag in their files.
- Changing a summary-only setting, or Run anyway, no longer redoes every cached logger and site.
- Workbench Needs you no longer lists loggers the logger list or the sensors table already places as "in no row of the deployment sheet".
- Summaries' ZIP includes the thermal and estimated-days tables; the water-year labels follow the setting; Explore uses the water year and zero band of the last run, and the seasons plot's month axis follows the water year's start.
- Configuration export and import hold the logger list and its columns, the review decisions and the Automatic corrections switches; imported settings show on the pages at once and are no longer overwritten by the next autosave.
- Typing another project folder loads that project's saved settings, as opening it from the Projects list does.
- Texts: Automatic corrections and Quality rules named as the pages are; the folders on 1 Project; the run log under More; no Scotty Creek choice offered in Calibration options.

**Fixed (review decisions and set-up notice, 2026-10-09)**
- Check the data: Remove and Flag name the sensor (an EM50 port, P1), not the channel (P1_temp), so they now match the values; Keep is stored with the review queue's decisions and the item leaves both lists.
- A run reads review_decisions.csv: a flag decided not a problem stays in flags.csv marked `accepted` and leaves the site files' flag column and the exports.
- Not a problem on a Workbench selection writes through the core review helper.

**Fixed (damaged run record, 2026-10-09)**
- A damaged output/audit/run_manifest.csv no longer stops the Set-up page (and the Raw files page in GT and WLR) with an error; it is read as empty. **Output changes:** none.

## 1.8.0 (2026-10-10), needs core 1.15.0

**Changed (sample tests, 2026-10-09)**
- Sample tests: a new or changed sample fails until accepted (tests/samples/accepted.csv); hand-checked values in reference.csv. **Output changes:** none.

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). **Output changes:** none.

**Changed (time zone, 2026-10-09)**
- Labels, the zone question at a run and the export notes use the time zone from Settings, not a fixed UTC-7. Fahrenheit readings convert through core convert.R.

**Output changes:** none.

**Changed (errors reported, 2026-10-09)**
- Set-up tables and summaries that cannot be read during an export or report are listed as problems, and an export lists them in problems.csv. The pre-run check warns when the site registry or the deployment sheet cannot be read.

**Output changes:** exports may gain problems.csv; the data are unchanged.

**Changed (packages, 2026-10-08)**
- The app and its pipeline no longer install packages when they start: a missing one is named, with how to get it (the launcher installs the versions in renv.lock). **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R from 276 to 154 lines: its helpers moved to pages/fct_outputs.R (reading the outputs), pages/utils_tables.R (theme and table style) and pages/utils_setup.R (set-up table editors), sourced in the same place. Code unchanged line for line. **Output changes:** none.
- app.R split from 1296 to 276 lines: each server section is its own file in pages/ (sourced inside the server, same order),
  and the interface is pages/ui.R. Code unchanged line for line. **Output changes:** none.

**Added (2026-10-08, approved mockup)**
- Settings, Time and depths: **Depths in the set-up tables**, below the ground positive (default) or above the ground positive (the `depth_positive` setting, saved per project), with a note that + always means above the ground. The Logger list shows a line on how to write depths under the chosen setting. **Output changes:** none unless the setting is changed.

**Removed (2026-10-08)**
- TTOP: the measured and modelled temperature at the top of permafrost, the thermal offset (TTOP − MAGST), the conductivity ratio (Settings and calibration_options.csv) that only the modelled TTOP used. n-factors, degree days and the surface offset stay. **Output changes:** `summaries/n_factors.csv` loses the columns ttop_measured, ttop_model, thermal_offset and rk; nothing else changes.

**Added (2026-10-09, approved layout, continued)**
- Plot and explore > Heatmap: a Smoothing On/Off switch, On by default (a moving average of the number of time steps set in Plot options, 7 by default). It changes the picture and its PNG only; the gridded-values CSV is always unsmoothed. Off draws what the Workbench draws. Workbench: a depth-by-time strip above the chart (daily mean of each sensor as its own band, no interpolation, no smoothing) for the period shown. **Output changes:** none.

**Added (2026-10-08, approved layout, continued)**
- Standard: before-import screen on the Project page (until there is a run): Choose a data folder, Bring your own metadata (opens Loggers, sensors and depths, where a CSV or Excel file is imported and its columns matched), Open a project, and a table of what the data folders hold by kind of file (found, read or needs export, notes; looked at by name only). Cards and panels share one lift (8 px corners, shadow) in the notebook layout.
- Workbench: the selection's panel is **Decide**: a reason, Remove (or Restore), Flag only, Correct a period, **Not a problem**, Trim, Clock. Not a problem marks the flagged stretches of this site's sensors that overlap the selection as done, with the reason (review_decisions.csv); nothing flagged there, nothing to mark. A reason typed in the panel fills the dialogs. **Output changes:** none.

**Added (2026-10-08, approved layout)**
- Standard: **Site results** page (Report group): site chips with a searchable `+N sites`, the site's name and one line (sensors and depths, period, interval), four facts for the site (mean ground temperature at a chosen depth, max thaw depth, days thawed at the shallowest sensor, completeness), ground temperature by depth with a Smoothing On/Off switch (On by default, display only), thaw depth over time, and an annual summary by depth with a year picker and CSV export. Reads the last run's summaries; no calculation changed. **Output changes:** none.

**Changed (2026-10-08, approved layout)**
- Standard: the contents list and the Standard | Workbench switch are on the right; the project facts and data gaps are a card at the top of every page instead of a side panel (core `shell.R`, notebook layout only). **Output changes:** none.

**Changed (2026-10-08)**
- Errors under their field in the Workbench dialogs; correction buttons say why they are greyed; a Columns picker on every wide table. **Output changes:** none.

**Added (2026-10-08)**
- Group the logger list by serial number, deployment, download or raw file (Sensors and depths and the Workbench tree); moves found in the records in Needs you;
  Rename and Duplicate in the projects menu. **Output changes:** none.

**Changed (2026-10-08)**
- Only the raw folder is needed: with no project folder the app uses ~/Desktop/Ovid_Projects/GT/Untitled; no metadata is needed
  (loggers go by serial). Test: `tests/test_no_metadata.R`. **Output changes:** none.

**Added (UI design, 2026-10-08)**
- Reference page; Workbench pipeline tabs; switching views keeps the site and period. **Output changes:** none.

**Added (set-up Workbench design, 2026-10-08; core workbench 0.9.0)**
- Workbench: every set-up table is a spreadsheet with a formula bar; **Correct a period** in the cell bar; **Needs you** at the top
  right; the **Review queue** tab; a "GT processing" section in the tree that opens the app's own steps.
- Corrections over a period apply to each depth series (its parameter is the depth as the Workbench shows it, "0.50 m", or the sensor or channel) after the quality rules and manual removals; a typed sensor offset is an offset row (Quality rules > Corrections over a period).
  **Output changes:** none until a project has rows in period_corrections.csv.

**Changed (set-up flow, shared core shell 1.2.0, flow 1.0.0, tsview 1.0.0, workbench 0.8.0)**
- The numbered set-up flow, the project switcher, the information panel, Check the data and the Standard view plots (see core
  CHANGELOG); table headings shown as words with units (`ovid_colnames()`).
- Workbench: units under the grid headings, quality-rule range limits on the chart, chart selection statistics in the inspector.

**Fixed (run-through on the Mac, 2026-10-08)**
- The app no longer greys out after an error in a handler, notices a deleted output folder, and shows damaged set-up tables with
  their columns restored (core). Pending removals are drawn only on their own parameter in the Workbench chart.
- A damaged sites.csv no longer stops the run (new sites are added with the table's columns).
- Workbench: the tree's long parameter names wrap instead of overlapping.
  **Output changes:** none.


**Added (sensors above the ground)**
- `depth_positive` setting, kept per project: "down" (default, as before: metres below the surface, negative above) or "up" (above the surface positive, below negative), for chains in a casing or conduit that stands above the ground. Template and default depths are written in the project's convention. A depth typed with a leading + ("+0.5") is always that far above the ground, with either setting; typed depths are kept as text (logger list, sensors.csv, templates, registry depth list, Sensors page) so the + is not lost. No control on the Settings page yet.
- Sensors above the ground stay in the data, plots and exports (labelled "m above ground"; report band "Above ground"), and are left out of ground-only results: active layer, zero annual amplitude, zero-curtain offsets and MAGT trends. Annual statistics get a `position` column (ground / above ground).
- Output changes: with the default setting, only for sites with sensors above the ground (they no longer enter the results above), plus the new `position` column.

**Fixed (empty and unreadable files)**
- An empty raw file, a file that is not a logger file (e.g. firmware saved as .hex) and a file with no data rows are now listed in the run's problems as skipped; a file with no data rows is marked unreadable and not read further. Output changes: none to values or sites; problems.csv lists these files.

**Added (projects, renames, report layout)**
- Projects list on Home (core `projects.R`).
- Rename a site (or site group) everywhere (Configuration; core `rename.R`).
- Shallow and deep profiles: a site whose deepest sensor is no deeper than the cut-off (Settings, default 3 m) is a shallow profile; the new `profile` column of Sites sets it by hand. Reports written together share the temperature axis per depth band, the heatmap colours, the depth axis and the trumpet curves' axes among sites of one kind.
- Report layout on the Export page (core `report_options.R`): titles, file names, a line under the title, who prepared it, sections and axes.
- A site in the logger list that differs from one set by hand in Sensors and depths is listed (the hand one is used); the deployment sheet fills what the logger list leaves blank.

**Output changes:** none.

**Added (automatic corrections list)**
- Settings: an Automatic corrections list (core `corrections.R`) of everything a run changes in the data on its own, with
  what each does, whether it is on, and where it is switched; it adds a switch for leaving out reset-clock readings (on by default), and switches for the between-site check and leaving out stray readings, which had no control in the app. Each run log lists them.

**Output changes:** none.

**Added**
- Needs assessment, second batch (medium items):
  - NetCDF export (Export > NetCDF too): one CF-1.10 / ACDD-1.3 file per site, `featureType` timeSeriesProfile (temperature on
    time x depth, depth positive down, times in UTC), every value's WMO code in `soil_temperature_qc` with CF `flag_values` /
    `flag_meanings`, and the processing level. The form GTN-P and the PermafrostNet data recommendations ask for. Written by
    core `standards.R` (no new package).
  - Estimated daily means (`daily_estimated.csv`, Summaries G; level L3): gaps up to 5 days at 0.5 m or deeper are filled
    linearly in time; longer or shallower ones from the sensors above and below when a regression on the days all three have
    explains r2 0.95 or more. Measured days are never replaced. Annual statistics use the estimates only if Settings allows
    it (off by default); `annual_stats.csv` has a new `days_estimated` column.
  - Apparent thermal diffusivity and the gradient (`thermal.csv`, Summaries G): per complete calendar year and pair of
    sensors, the annual wave's amplitude and lag, diffusivity from amplitude and from phase (they agree where heat moves by
    conduction alone; a note marks a 2x disagreement), and the gradient of the annual mean (`deepest` marks the deepest pair).
  - Ground-surface changes (`surface_changes.csv`, Quality rules > Ground-surface changes): subsidence or heave since
    installation, interpolated between surveys. `active_layer.csv` adds `surface_change_m` and `alt_from_surface_m` (the
    active layer from the surface of the day); sensor depths stay as installed.
  - Soil water content in frozen ground: each vwc reading is marked `liquid_only` when the nearest ground temperature (within
    0.15 m) had a daily mean at or below 0 C, since a dielectric probe sees only the unfrozen water; `soil_annual.csv` splits
    its statistics by `water` (total, or liquid only).
  - The export readme lists the processing levels L0 to L4.
- Permafrost products (Summaries, tabs D to F), from the needs assessment's quick wins:
  - Freezing and thawing degree days per depth and water year (`degree_days.csv`), and per site the n-factors (surface over
    air, Klene et al. 2001), the surface offset (MAGST - MAAT), TTOP measured at the active-layer depth and modelled after
    Smith and Riseborough (2002) with a conductivity ratio (Settings, default 0.7), and the thermal offset (`n_factors.csv`).
    Air temperature comes from a sensor above the ground at the site, or from any CSV named in the new
    `air_temperature.csv` (a Campbell station's export, say); without it only the ground values are given.
  - The state of each depth per complete year (permafrost two years in a row, seasonally frozen, perennially unfrozen;
    `ground_state.csv`), and taliks (`taliks.csv`): a layer unfrozen all year between the seasonal frost and permafrost,
    with its top, bottom and thickness. The whole profile is scanned, not only down to the first frozen sensor.
  - Sensor offsets in the zero curtain (`zero_curtain_offsets.csv`): a sensor whose median there is more than 0.1 C from
    0 C is listed (a field check; never applied).
  - Trends (`trends.csv`): Theil-Sen slope per decade and the Mann-Kendall test on MAGT per depth and on the active-layer
    thickness, at least four complete years, with the Hamed and Rao correction from ten years (core `trend.R`).
- Logger list (Sensors and depths > Logger list): optional metadata for when there is no registry workbook. Only the serial is
  needed; a name or site places the logger, a depth list (top sensor first, metres) gives its sensors' depths, and from and
  until dates bound it. Rows set by hand or seeded from the registry win; rows from the list are made again from it on each
  run. Start from the loggers found, paste rows, or import a CSV or Excel file; rename, hide or add columns with Columns.
- Paste and Import in every set-up table.
- Workbench interface (preview), `workbench/` on core `workbench.R`: an additional, more detailed view of the same project with
  every process of the Standard interface, in GT's soviet-dark colours.
  - Every Standard page runs inside the Workbench on the Standard server (`server(embedded = TRUE)`): home, plot and explore,
    compare sites, summaries, soil, site details, raw files, sensor health, clock checks, audit trail, sites, sensors and depths,
    quality rules, configuration, set-up check, settings, reports and export. They are listed under Pages and open as tabs.
  - Data: the tree lists every site by instrument (with its soil record when it has one). A site is a spreadsheet with a column
    per depth (soil: per variable and depth) and a row per time, the selected depths charted above it. A time at which every
    value was removed (a trimmed start, a period removed at every depth) keeps its row, so its values can be seen and restored.
    The inspector shows the selected value, its state (passed, flagged, removed, saved for the next run) and its lineage: the
    logger and sensor, the depth and where it came from (set, registry, template or assumed), the site join, and the rule or
    removal that took it, with the site's facts beside it.
  - Selecting works as in CS and WLR: cells, records (a time), depths (a heading), drag, shift and cmd/ctrl, keys; the chart
    highlights the selection, zooms and selects.
  - Corrections, with the rows Plot and explore writes: **Remove** and **Flag** (`manual_removals.csv`: the site, the depths and
    the variable, one row per run of records; R or Delete), **Restore** (deletes the rows that cover the values), **Trim** (the
    record starts after the selected records, or ends at them), **Clock** (`time_corrections.csv`, for the logger), and links that
    open the logger's rows on Sensors and depths and the quality rules. The grid and chart mark saved corrections the next run applies.
  - Set-up tables in the tree open the Standard page and tab that edits them. Import data holds the folders with a scan and the
    set-up check; Run is the Standard run.
- Choosing the interface: **Workbench** in the Standard title bar (it opens the site shown on Plot and explore), `?ui=workbench`
  / `?ui=standard` in the address, else `OVID_UI`, else the `ui` setting. Standard is the default.

**Changed**
- Deployment sheet (core): Sensors and depths > Deployment sheet, with "Against the data". Its loggers join the logger list.
  Fixed: the set-up check stopped on an undefined name (since core 1.11.0), so it showed nothing and did not hold back Run.
  Declared rules for clock corrections and manual removals (known serials and sites). "Run anyway" is recorded in the run log.
- Guardrails (core `run_project()`): see core. A .f2b (or any file) that may not be read is listed as unreadable instead of
  stopping the run. "Newest" download is by the time the file states, else its own last record (not the file date).
- HOBOware CSV, .hobo and TOA5 headers, file listing, checksums, copies, gaps, clock checks, flags and report helpers use core
  code (output unchanged). A tab-separated HOBOware CSV or TOA5 table, and a TOA5 header with no station name, now read.
- Run happens in the background (core `runner.R`): the app stays usable, a box at the bottom right shows the step, the logger
  or site being worked on, and Cancel. Rescan and the time-zone question use it too.
- Every run leaves a log in `output/logs/`, and the lifebuoy in the top bar saves a troubleshooting zip (core). Tests now also read real sample files and click through every page in a browser.
- Each cached step is keyed on the code it runs (core `code_fingerprint()`, `STEP_HASH`), not on every file: an edit elsewhere no longer redoes it. The first run after this redoes everything once.
- `fmt_span()`, `norm_serial()` and the clock-correction mechanics are core's (the same rules as before); output unchanged.
- No registry or metadata folder is normal: the set-up check notes it instead of warning, and the metadata folder is marked
  optional.
- A run redoes only what an edit can change. A logger's quality-rule step is redone when its record, its sensor rows, or the
  quality rules and manual removals that can apply to it change (its type, serial or sites); before, any change to either table
  redid every logger. A logger is read and stitched again when its files, its clock corrections, the time-zone rows that can
  match its files, or (for Campbell tables) the column patterns change; before, any time-zone or Campbell row did. On the demo
  project a removal on one site redoes 1 of 10 loggers: 16 s instead of 90 s.
- The message at the end of a run says what was redone, for example "Redone: 1 of 10 loggers; the rest was unchanged and
  reused."; `run_manifest.csv` counts `loggers_checked` and `sites_total` beside `loggers_reused` and `sites_reused`. Settings are
  keyed by value, so a run from the app reuses what a run from the command line made and the other way round.
- The set-up tables use the editor shared by every app (core `editor.R`): a table open in both interfaces, or in two windows,
  takes up a version saved in the other when it has no unsaved edits, and asks before it overwrites one when it has.
- The Standard pages are built by `std_pages()` (with `std_head()`, `std_css()` and `std_sources()`), and both interfaces build
  their editors from one list (`SETUP_TABLES`); the Standard interface looks and works as before. Started from `Ovid_GT.bat`,
  the app stops when the last tab of either interface closes.

**Fixed**
- The Workbench tried to load a record before one was chosen, writing an error to the console (found by the click-through tests).
- Compare sites had two inputs named `cp_mode` (Show, and the temperature axis), so the axis setting did not work there; the axis
  has its own id now.

**Output changes**
- Six new files in `output/summaries/`; existing summaries are unchanged.
- None: the demo project's site files, soil files and summaries are the same with and without the caches, and after each kind
  of edit the incremental run gives what a run from scratch gives.



## 1.7.0 (2026-10-06), needs core 1.14.0

**Added**
- Every quality rule has a `rule_id`; `flags.csv` ends with `rule_id`, `qc_code`, `app`, `run_id`.

**Fixed**
- A GeoPrecision logger with no FG2-Shell export is read from its `.f2b`; its 1998 clock-reset
  records, the NoReply hours after the restart and the gap from December 2022 to June 2023 are in the audit.

**Output changes**
- Four columns at the end of `flags.csv`; `qc_rules.csv` gains `rule_id`. Values are unchanged.

## 1.6.0 (2026-10-06), needs core 1.13.0

**Added**
- Base time zone can be set to UTC-6 to standardise every record; a new *Zone of files that state none* setting (blank = the base zone, as before); if no file states its zone the app asks which zone before running. Defaults unchanged: output identical on the real data.
- WMO quality codes in exports: a `qc` column (long) or `T_<depth>m_qc` (wide), a mean taking the worst code of its values; the
  default, with "Checks only" as a choice.

**Changed**
- Spike, flat-line and flag runs use the shared core functions.

**Output changes**
- Exports gain the `qc` columns. Pipeline outputs are unchanged (checked on the real data).

## 1.5.0 (2026-10-05)
- Changes since 1.4.2 are not in the git history yet; see the GT guide.

## 1.4.2 (2026-10-05)
- Faster reruns.
