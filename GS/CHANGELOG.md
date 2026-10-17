# Ovid GS changelog

GS was called ERT until 2026-10-14; the entries below that date use the old name.

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Changed (combined ERT/GPR icon, 2026-10-17)**

- New icon: a gold radar trace above a purple ERT sample section. The app headers, browser-tab icon, platform icons and documentation banners use this artwork. **Output changes:** none.

**Changed (toggles, 2026-10-14)**
- QC rules, GPR processing steps, Before / after, Topography, Markers, Legend and the list filters are toggles. **Output changes:** none.

**Renamed (ERT is now GS, 2026-10-14)**

- The app is GS (geophysical surveys), as it now covers GPR as well as resistivity: folder `GS/`, `gs_pipeline.R`,
  `Open GS (Mac).command`, `Ovid_GS.bat`, `GS_reference_guide`, downloads named `ovid_gs_<what>_<date>`, environment
  variables `GS_RAW_DIRS` and `GS_PROJECT_DIR` (the `ERT_` names still work). The ERT | GPR switch keeps its names.
- Projects made as ERT open unchanged; `~/Desktop/Ovid_Projects/ERT` stays the projects folder until a `GS` one exists.
- **Output changes:** new flag lines say `app = GS`; values are unchanged.

**Changed (one review list, project menu, time zone, 2026-10-09)**
- The project menu is in the top bar; Settings lists the projects and has the core time-zone control, used for surveys whose files carry no zone. **Output changes:** none when it is left blank.
- Map and transects saves only the folders it changes, not the whole settings.

**Changed (removals and fold-ins, 2026-10-09)**
- Home shows the counts and Scan; the "What the scan found" table and its Open buttons go (the surveys are listed on Archive). **Output changes:** none.
- A survey's transect is set only in Archive (Add to transect) and on the Map; Stack and the survey form show it read only. **Output changes:** none.
- One Compare control on Stack and the Workbench's Panels: Compare with... (off) and Both kept at least (%), kept in step; Stack compares from the survey selected in its table, else the first. **Output changes:** none.
- GPR processing is one screen in both interfaces, the Workbench's flow (ordered steps with switches, versions, Undo, Apply to all, Before / after, markers table and CSVs) with Standard's display settings (across, topography, markers, colour limit and palette, deepest depth); the Workbench's GPR flow tab goes, its A-scan shows in the inspector on GPR processing. **Output changes:** none.
- Each GPR processing step shows its switch and a one-line summary, with its settings folded under it. **Output changes:** none.
- The GPR Lines page folds into the GPR Map (folders, Search again, All lines found). **Output changes:** none.
- Map: one Export view (PNG) button that follows the Legend checkbox; the transect table drops its All / By year / By project choice. **Output changes:** none.
- Survey > Geometry: one file input for an elevation table or a GNSS file, told apart by its columns. **Output changes:** none.
- Survey: Display options keep colours and range (flagged points by reason and point size 7, as before); the inversion fields are hidden (values already saved are kept). **Output changes:** none.
- One survey export, on Survey, with a choice of QC profile (or the QC page's limits); the QC page's export goes. **Output changes:** none.
- Electrodes are turned off on the QC page only, now also for a whole transect over a period; the Workbench's Electrodes off and Drop electrode and the electrode_status.csv tab go. **Output changes:** none.
- The Workbench remembers the panels ticked on its own (app_settings.json); Save this view goes and config/wb_stack_view.csv is no longer written or read. Settings drops the Folders card. **Output changes:** none.

**Fixed (review decisions and set-up notice, 2026-10-09)**
- "Set-up changed since the last rebuild" no longer follows GPR picks, markers, flows or the saved Workbench view, which a rebuild does not read.

**Fixed (damaged run record, 2026-10-09)**
- A damaged output/audit/run_manifest.csv no longer stops the Set-up page with an error; it is read as empty. **Output changes:** none.

**Fixed (review items, 2026-10-09)**
- No lights: Kept %, Quality, Flagged, the problems count, Safe to scan and the reporting checklist are plain numbers and text; the survey tree no longer warns under 75 %; the contact guide lines are one neutral colour, named by their values. **Output changes:** none.
- Every profile in qc_profiles.csv is checked at a scan, those saved on the QC page and cpers too; Settings has "QC profile of the catalogue" (default: the first built-in). **Output changes:** flags.csv and survey_stats.csv gain rows for those profiles.
- The Survey export uses the catalogue's profile and is named by it; the QC export is named by the profile its limits started from. **Output changes:** QC export file name.
- The set-up check says transects are added on the Map page. **Output changes:** none.
- Delete the last marker, beside Delete the last pick (GPR processing and the Workbench's GPR flow). **Output changes:** none.
- A GPR line with no saved flow starts from the defaults, not the line before; at start the preset no longer overwrites a line's saved flow. **Output changes:** none.
- One name for the share kept: "Kept %" (Archive, Survey, QC, Home, Workbench, the exported index.html); the Workbench's Stack tab is "Panels" and Standard's Home is "Home" in its page tree; "Scan" everywhere for reading the archive. **Output changes:** catalogue index.html column heading.
- One default colour scale for apparent resistivity, 1 to 10,000 ohm-m, on Stack, Survey and the Workbench (each still settable); the Workbench's Compare with uses Stack's "Both kept at least (%)". **Output changes:** none.
- "Open" in Archive's group-by opens the survey in this interface instead of reloading into the Workbench. **Output changes:** none.
- The context bar covers Stack and GPR processing; the app.R header lists every page; an unused Reference output removed. **Output changes:** none.

## 0.5.0 (2026-10-10), needs core 1.15.0

**Changed (ERT | GPR switch, 2026-10-09)**
- ERT and GPR are two sides of the app, with a switch beside "ovid // ert" in Standard and the Workbench (core areas): the count of
  surveys and of viewable GPR lines on each side, Alt-1 / Alt-2, `?area=gpr`. The GPR page is split into three pages, GPR (Map),
  GPR lines (Lines) and GPR processing (Processing). With no line yet the GPR side says so, with Add folders to scan and Go to ERT.
  Reference, Set-up tables and Convert show on both sides; the Workbench's GPR flow tab is on the GPR side. **Output changes:** none.

**Changed (sample tests, 2026-10-09)**
- Sample tests: a new or changed sample fails until accepted (tests/samples/accepted.csv); hand-checked values in reference.csv. **Output changes:** none.

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). **Output changes:** none.

**Changed (packages, 2026-10-08)**
- The app and its pipeline no longer install packages when they start: a missing one is named, with how to get it (the launcher installs the versions in renv.lock). **Output changes:** none.

**Fixed (2026-10-08)**
- No "Couldn't coerce the value argument to a date string" warning at start (the Survey page's empty date box). **Output changes:** none.
- An empty survey file no longer gives a missing header line when its format is judged (`R/importers.R`, found by the new style check). **Output changes:** none for files with content.

**Fixed (2026-10-08)**
- Archive (module pilot): double-clicking a survey, or Open, goes to the Survey page again; the Year filter keeps its choice when the catalogue changes (after Add to project or Add to transect), like the Project filter. **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R from 207 to 102 lines: its helpers moved to pages/fct_outputs.R (reading the outputs), pages/utils_tables.R (theme and table style) and pages/utils_setup.R (set-up table editors), sourced in the same place. Code unchanged line for line. **Output changes:** none.
- app.R split from 799 to 206 lines: each server section is its own file in pages/ (sourced inside the server, same order),
  and the interface is pages/ui.R. Code unchanged line for line. tests/run_tests.R links pages/ into its copy of the app. **Output changes:** none.

**Changed (2026-10-08)**
- Errors under their field in the Workbench dialogs; correction buttons say why they are greyed; a Columns picker on every wide table. **Output changes:** none.

**Added (2026-10-08)**
- Group the logger list by serial number, deployment, download or raw file (Archive (by instrument) and the Workbench tree); moves found in the records in Needs you;
  Rename and Duplicate in the projects menu. **Output changes:** none.

**Changed (2026-10-08)**
- Only the raw folder is needed: with no project folder the app uses ~/Desktop/Ovid_Projects/ERT/Untitled; no metadata is needed
  (loggers go by serial). Test: `tests/test_no_metadata.R`. **Output changes:** none.

**Added (2026-10-08)**
- Reference page from the guide (core reference.R). **Output changes:** none.

**Added (core workbench 0.9.0, 2026-10-08)**
- Workbench: every set-up table is a spreadsheet with a formula bar (edit in place, ranges with totals, copy and paste, sort, Find,
  Undo, Redo, Apply). **Output changes:** none.

**Added (GPR viewer, 2026-10-08)**
- GPR page (`R/gpr.R`): view MALA ground-penetrating radar lines. Search folders are set on the page (default: the raw archive folders).
  A line is its `.rad` header, the `.rd3` / `.rd7` traces and the `.cor` GPS file, which are put together wherever they sit: by name,
  then by the size the header implies (traces = size / (samples x bytes), up to 10 more than LAST TRACE, as MALA files are), then
  the same folder. A line that cannot be matched says why (no data file, an unfinished `.partial` copy, a size that does not fit,
  several candidates) and is not guessed.
- Radargram (distance or trace across, two-way time or depth down) with dewow, linear or automatic gain and a colour limit; the GPS
  track on the satellite or street map with the picked line on top. Long lines are shown with neighbouring traces averaged.
- GPS fixes with a dropped digit in a coordinate (latitude 1.1 for 61.1) are taken out; a line whose fixes never move is drawn by trace.
- Viewing only: GPR lines are not surveys, so they are not in the catalogue, QC or exports, and the raw files are never changed.
  Tests: `Rscript tests/test_gpr.R`.
- GPR topography, markers and recovered copies (2026-10-08):
  - Topography: the radargram can be drawn on elevation (m a.s.l.) from the height in the `.cor` file, vertical axis in depth, with the thaw
    line following the surface. The GPS heights are smoothed first (running median, 11 fixes by default); a handheld GPS height is good to a few
    metres, much worse than the position.
  - Markers: the `.mrk` file (a header line, then trace number and two more numbers per marker) is matched to its line, drawn as numbered dotted
    lines on the radargram and as points on the map, and gives a distance axis from the spacing you enter (the first marker at 0; REFLEXW "mark
    interpol"). The two extra numbers (30 and 1 in these files) are kept but not interpreted: the manuals read do not give them.
  - Hidden `.partial` copies: the Logger Consolidator copies to `.NAME.partial`, re-hashes, then renames; the source's macOS immutable flag (uchg),
    copied after the hash check, made the rename fail (`[Errno 1] Operation not permitted`) and left a whole, verified copy. All 149 of them
    (148 `.rd3`, 1 `.cor`) match their sources byte for byte. The viewer reads them in place and says so; a file with fewer traces than its
    header says is shown as truncated (`DAT_0097`: 242 of 2119 traces, as in its source). Viewable lines: 31 -> 168 of 286.
  - `Logger_tools/recover_partials.py` verifies each `.partial` against its source and, with `--apply`, clears the flag and renames it (dry run by default).
**Output changes:** none.

- Processing from the manuals (REFLEXW 10.3 and its 2D guide, MALA GroundVision 1.4), in their order: DC, dewow (one period of the antenna
  frequency), time zero (first zero crossing), Ormsby band-pass, gain (energy decay, gain function or AGC), background removal (mean or
  moving; off by default because it erases a flat frost table). Presets for the flows of Gusmeroli et al. (2015) and Wollschlager et al.
  (2010); the page prints the steps applied and lists the references (`docs/GPR_references.md`). Velocities are the values published for
  frozen ground, thawed sand / gravel and peat / silt active layers, with their sources; the earlier unreferenced list is replaced.
  The time axis now uses the sampling interval (time window / samples), as the header defines it.
- Permafrost settings: the vertical axis is depth by default, with typical velocities to pick from (ice 0.168 m/ns, frozen mineral soil
  0.13, frozen peat 0.10, thawed wet soil 0.07, saturated peat 0.045; replace them with one measured on site). A thaw depth gives the
  thawed layer above it its own velocity, with the frozen ground below, and draws the permafrost table as a dashed line. A deepest
  depth cuts the view. Gain defaults to automatic (AGC).
**Output changes:** none.


**Fixed (core, 2026-10-08)**
- With the shared core: an error in a handler shows a notice instead of greying out the app; damaged set-up tables are shown with
  their columns restored; plain messages for a read-only project folder; cache files about ten times smaller.
  **Output changes:** none.


**Added (projects, renames, report layout)**
- Projects list in Settings (core `projects.R`): new catalogue project, open, delete outputs or the project to the Trash.

**Output changes:** none.

**Added**
- Needs assessment, second batch (medium items), in `R/inversion.R`:
  - Error model: with 40 or more reciprocal pairs the error is fitted against the transfer resistance in 20 equal-count bins,
    as a line or a power law (the better fit), as ResIPy does; otherwise 4 % relative (CPERS), or the stacking error when
    larger. Shown under Survey > Error vs current.
  - Inversion files in every survey export (`inversion/`): pyGIMLi / BERT unified format (`.ohm`, with the relative error),
    R2 / ResIPy `protocol.dat` with `electrodes.csv`, and Res2DInv general array with the electrode heights as a topography
    list; every flagged measurement left out, and `manifest.json` says which and why.
  - Electrode heights from a GNSS file (Survey > Geometry): an Emlid, Leica or DJI export, or the RS app's GNSS points; the
    electrode is the last number in each point's name. Added to `elevations.csv` like any other heights.
- Not done from that batch: temperature correction of resistivity (it needs ground temperatures at depth from GT and applies
  to inverted models of unfrozen ground, not to apparent resistivity), and a reader for Syscal `.bin` files (export them as
  text with Prosys, which is read).
- Reporting (Herring et al. 2023, Table 1; CPERS): each survey keeps the electrode contact, inversion software, inversion
  parameters and misfit (Survey > Details > Processing and reporting, `surveys.csv`), and a checklist there shows which of the
  items a report should state the catalogue has.
- QC rule `negative_ip` (QC page: Flag negative chargeability): chargeability or phase below zero, flag-only; in no profile
  by default. A check of the IP decay curve is not added yet: the AGI IP windows are kept (`ip_1`, `ip_2` ...) but not yet
  mapped to their gates, and there are no IP surveys in the sample archive to check it on.
- Paste and Import in every set-up table: rows copied from Excel, or a CSV or Excel file with its columns matched to the
  table's by heading, added to the table or used instead of it.
- Workbench interface (preview), `workbench/` on core `workbench.R`: an additional, more detailed view of the same project with
  every process of the Standard interface, in ERT's kanagawa colours.
  - Every Standard page runs inside the Workbench on the Standard server (`server(embedded = TRUE)`): archive, map, survey, QC,
    set-up tables, reference and settings. They are listed under Pages and open as tabs.
  - Data: the tree lists every survey with measurements by year. A survey is a spreadsheet with a row per measurement and a
    column per value (electrodes A, B, M and N, apparent resistivity, V/I, current, voltage, error, contact resistance, position,
    time and flags), its pseudosection drawn above it as points coloured by the value chosen (on the Survey page's colours).
    A flag shows on the value its rule tested (an electrode flag on the electrode, a manual flag on the whole measurement).
    The flags are those the set-up as saved gives, compared with the last rebuild: a saved change shows before the rebuild
    (saved) and a flag the set-up no longer gives shows as coming back. The inspector shows the measurement's file, quadrupole,
    array and geometric factor, its position, and each flag with its rule, row of qc_profiles.csv and limit.
  - Selecting works as in the other Workbenches: cells, measurements (a number), columns (a heading), drag, shift and cmd/ctrl,
    keys; a click on the pseudosection selects the nearest measurement and a shift-drag every one in a box.
  - Corrections, through the set-up tables: **Flag** (`manual_flags.csv`: the measurements as runs, 5-20,31; R or Delete),
    **Unflag** (takes them out of the survey's manual flags, and turns back on the electrodes turned off for this survey that
    they use), **Electrodes off** (`electrode_status.csv`, for the survey or for its transect over a period, starting from the
    electrodes in the selected cells), and links to the QC page, the survey's manual flags and its details.
  - Import data holds the folders with a scan and the set-up check; Rebuild is the Standard rebuild.
- Set-up tables page: every table in config/ (surveys, QC profiles, manual flags, electrodes turned off, transects, sites,
  projects, elevations, file rules, time zones) with the editor shared by all the apps (core `editor.R`). Manual flags, file
  rules, sites, projects and time zones had no editor in the app before.
- Choosing the interface: **Workbench** in the Standard title bar (it opens the survey shown on the Survey page), `?ui=workbench`
  / `?ui=standard` in the address, else `OVID_UI`, else the `ui` setting. Standard is the default.

**Changed**
- Rebuild asks first when the set-up check finds something that would be ignored or misapplied (Review, Rebuild anyway, Cancel);
  a rebuild anyway is recorded in its log. Tables are checked as they are edited (core rules).
- Fixed: an emptied qc_profiles.csv stopped the rebuild (the built-in profiles are used, and said so); a catalogue zip named
  just ".zip" could remove every imported catalogue. Guardrails: see core.
- Rebuild happens in the background (core `runner.R`): the app stays usable, a box at the bottom right shows the step, the
  survey file or survey being worked on, and Cancel.
- Every run leaves a log in `output/logs/`, and the lifebuoy in the top bar saves a troubleshooting zip (core). Tests now also read real sample files and click through every page in a browser.
- Each cached step is keyed on the code it runs (core `code_fingerprint()`, `STEP_HASH`), not on every file: an edit elsewhere no longer redoes it. The first run after this redoes everything once.
- Survey duration uses core `fmt_span()`: under 2 min in seconds, then minutes to 0.1, then hours and days.
- A rebuild redoes only what changed. A survey's quality checks are redone when its measurements, its details (type, site,
  transect, date) or the rows of `qc_profiles.csv`, `manual_flags.csv` and `electrode_status.csv` that can apply to it change;
  the others are read back from `<project>/cache/qc`. A pseudosection preview is drawn again only when its measurements, flags
  or title change (`cache/previews`); before, every preview was drawn on every rebuild. On the demo archive (31 surveys) a
  rebuild with nothing changed takes 0.8 s instead of 6.5 s, and a manual flag, an electrode turned off or a site rule redoes
  one survey (about 1.2 s).
- `run_manifest.csv` counts `surveys_checked`, `surveys_reused` and `previews_reused`, and the message after a rebuild says what
  was redone, for example "Redone: 1 of 31 surveys; the rest was unchanged and reused."
- The Standard pages are built by `std_pages()` (with `std_sources()` for the folders), and both interfaces build their editors
  from one list (`SETUP_TABLES`); the Standard interface looks and works as before.

**Fixed**
- With no survey files yet, the app stopped at start-up instead of opening with an empty catalogue (found by the click-through tests).
- A rebuild applies the `site:` and `transect:` rows of `qc_profiles.csv` and the transect-wide rows of `electrode_status.csv`
  with the site, transect and date entered for each survey, as the QC page does. Before, a rebuild did not see the entered
  values: those rows applied only on the QC page, and `months:` rows went by the instrument's start time even when a date had
  been entered.
- After the project folder was changed on Settings, *back to my catalogue* (from an imported catalogue) went to the folder the
  app had started with.

**Output changes**
- Flags change only in a project with `site:` or `transect:` rows, transect-wide electrode rows, or `months:` rows for a survey
  whose entered date is in another month than its start time: a rebuild now flags what the QC page flags. None on the demo
  archive. A rebuild that reuses the caches writes the same catalogue, flags and previews as one from scratch.


## 0.4.0 (2026-10-06), needs core 1.14.0

**Added**
- Every row of `qc_profiles.csv` has a `rule_id`; each survey's `flags.csv` names the row behind each flag and ends with
  `rule_id`, `app` and `run_id` (ERT has no WMO codes).

**Output changes**
- Three columns at the end of each survey's `flags.csv`. Flags are unchanged.

## 0.3.0 (2026-10-06), needs core 1.13.0

**Added**
- QC rules `moving_median` (per depth level, 5 points, 7 % of log10 apparent resistivity; Herring et al. 2023) and `bad_electrode`
  (more than 25 % of an electrode's measurements flagged; CPERS), both flag-only and in the defaults of new projects only.
- Electrodes turned off (`electrode_status.csv`, by survey or by transect over a period), with a control on the QC page; their
  measurements are flagged and `electrodes.csv` marks them off.
- `applies_to` can be `site:`, `transect:` or `months:`; the most specific row of a rule wins.

**Output changes**
- None for an existing project until the new rules or electrode rows are added (checked on the archive). With both rules on in
  the frozen-ground profile, the median survey keeps 90 % of its measurements instead of 99.5 %.

## 0.2.0 (2026-10-04)
- Catalogue, metadata, QC profiles and viewing for SuperSting and IRIS surveys; stays a viewer (no inversion).
