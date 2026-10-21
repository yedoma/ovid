# Ovid Core changelog

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Changed (finding the core, 2026-10-21)**
- launcher.R comment and README.md: the core is found through OVID_CORE, core/ in the app or ../core only; ~/Desktop/Ovid is no longer looked in.
- tests: a check that no app's launcher or pipeline looks for the core on the Desktop.
- **Output changes**: none.

**Fixed (warning leaks, 2026-10-21)**
- timezones.R: checking a `file_pattern` no longer prints TRE's compilation warning beside the finding.
- util.R `say_progress()`: a step message with no progress bar open no longer warns ("setProgress was called outside of withProgress", seen in background runs).
- README.md: current versions brought up to date (core 1.15.0, shell 1.2.0, CS 1.11.0, WLR 0.12.0, GT 1.8.0, GS 0.5.0, RS 0.6.0).

**Changed (colours from the palette, 2026-10-20)**
- theme.R: OVID_SCHEMES$Nord holds nord0 to nord15, WLR's status-line ground and grey (`named`); `nord_palette()` gives WLR's palette from it (WLR no longer keeps its own copy). New `col_rgba(col, a)` for plotly fills and legend grounds.
- tests: apps draw no quoted hex or rgba() colour outside their palette, except the named scientific scales (Spectral, Viridis, RdBu, Grey, Blue-white-red) and two fixed whites (map labels on satellite imagery, the radargram cursor).
- **Output changes:** none.

**Changed (failures reported, not hidden; CONTRIBUTING rule 13, 2026-10-20)**
- Every tryCatch() or try() that gave a default (NULL, FALSE, an empty table) was looked at: 16 now use try_or(), so the failure goes on the problem list with the table or part it concerns (period corrections, the logger-list fields, the dataset description, the scan list of the deployment sheet, a correction's on/off test and the corrections list, the change log, the migration record, the run history, config tables in the exported configuration, set-up table checks, a report figure); the other 121 have a comment saying why the default is safe (a probe, a cache, a convenience, or a value only shown).
- **Output changes:** none for readable projects; a set-up table that cannot be read is now on the problem list.

**Changed (one say(), maps open on the world, shared helpers moved to core, 2026-10-20)**
- util.R: say() is the only one. It takes `log` (an environment whose `$lines` collects each message; default the app's LOG) and `steps` (default the app's SAY_STEPS: a "[k/N]" message moves the Shiny progress bar, say_progress()). CS, WLR and GT no longer define their own; core_shadows() now allows no replacement, so an app copy is reported again. metasheet.R meta_fold's local `say` is renamed note_problem. On a terminal (or RStudio) the CS, WLR and GT console lines now take the palette colours, as GS and RS already did; the Log page text is unchanged.
- convert_page.R: the latitude, longitude and zone examples are a neutral place (45.4215, -75.6972, zone 18N), not a field site.
- time_corrections.R (new): apply_time_corrections(), the one copy of CS, WLR and GT's (each had drifted). Rows are picked by the app (`rows`), and every row may shift, spread a drift (mode drift), or remove (action remove), limited to a file (source_file); `audit` and `dedupe` are the app's choices. Drift resolved by taking the fuller behaviour: a start or end that is not a date, and minutes that are not a number, are now a problem for every app (CS, GT used to apply or skip them silently); a shift of 0 minutes is skipped without a problem; a shift is audited per raw file as a change of TIMESTAMP (WLR counted it as records dropped). The number moved is attr(, "shifted").
- logger_files.R: ends_cut() and empty_events() (were identical in WLR and GT); hobo_csv_rows() (the records of a HOBOware CSV: blank lines and repeated headings out, a second heading with other columns ends them and is returned as header_change; `stop_at_change`) and hobo_csv_events() (event columns as events), so WLR and GT read a HOBO CSV's rows and events one way.
- shell.R: current_user() (CS's, which also asks the module's session) and track_session(session, app); editor.R: editServer(); table.R: rule_info(); workbench.R: wb_num() and wb_cells() (GS's, which marks a restored value; RS has no restored state, so the same cells). The app copies are gone.
- **Output changes:** none.

**Fixed (.hobo decoder: events mid-file, battery channel, time zone, 2026-10-20)**

- hobo.R reads the data as the bit stream it is: 24-bit records (14-bit pressure count, 10-bit thermistor count, then 8 battery bits when that channel was logged, tag 0x0b) and events of 14 one bits, a 4-bit type (0 coupler detached, 1 attached, 3 stopped, 6 host connected), a 4-bit length n and 4n bits of seconds. Events are skipped exactly, a stopped event or erased memory ends the records, and the earlier rules (launch record packed apart, end mark, guessed resume points) are gone: they were the same records read at a 2-bit offset after an event. The interval is the last of tag 0x08's values; the clock's UTC offset comes from the launch time in UTC (tag 0x22 bytes 5-8) as attribute utc_offset_h.
- Checked against HOBOware: all 826 U20 files in the consolidation archive that have a CSV export: 808 match in every record, value and time; 15 differ only below about -17 C, by 0.01 to 0.035 C (the thermistor curve, not the events); 1 has corrupt memory where HOBOware shows 749 F (decoding stops there, flagged); 2 exports hold no records. Of the 30 files compared before, 30 now read every record (9 before). Test in tests/run_tests.R with a real file holding seven events mid-file (tests/data, serial neutralized), and a bit-stream test in place of the old nibble one.

**Output changes:** decoded U20 .hobo files (WLR): records after any event chain, and every record of loggers that also logged battery voltage, are now right and complete (they stopped early or were shifted before); a file holding an earlier interval in its header gets the right times.

**Fixed (.hobo decoder; reader-sample time check, 2026-10-19)**

- hobo.R: the U20 launch record is packed apart (pressure high 12 bits, then 2 low bits and a 10-bit temperature) and the last record carries an end mark in bit 0; both are now decoded. WLR's U20 sample matches its HOBOware export at both ends (98.92 and 111.66 kPa, all 1,564 records); of 30 real U20 pairs, 9 decode fully right (0 before), and mid-file event chains still stop or shift some files (open). Test in tests/run_tests.R.
- testkit.R: check_reader_samples compares time columns to the millisecond (sample_times_same); all.equal alone let about 26 s through, so an 18 s shift passed unseen. Test in tests/run_tests.R.

**Output changes:** decoded U20 .hobo files (WLR): the first and last record's pressure and temperature are now right (they were wrong in every file). Reader-sample checks only: no output change.

**Changed (GS icon, 2026-10-17)**

- Updated only the GS entry in `OVID_ICONS` to the new ERT/GPR trace-and-section artwork, shared by Standard and Workbench. Other app icons are unchanged. **Output changes:** none.

**Added (toggles and not deployed, 2026-10-14)**

- shell.R `ovid_switch()`: a toggle switch (a checkbox input with role="switch", value TRUE/FALSE as before), its state
  written On or Off (parts.css). Corrections list switches, Explanations, the Standard plot's Smoothing, Ghost and
  Precipitation and Needs you's "Only what needs a look" are toggles (Plot_table_standards A12).
- deployments.R `absent_spans()`: the time between a place's deployments; span kind "absent" is hatched grey and labelled
  "not deployed" in tsview.js and workbench_chart.js (base message `spans`), with a tooltip giving dates, length and loggers.
- **Output changes:** none.

**Changed (shared items for every app, 2026-10-14)**

- Plots (tsview.js, workbench_chart.js, workbench_xy.js): a drag left to right zooms to the dragged stretch, right to left
  zooms out (twice as wide); Shift + drag selects, Shift + click adds. Hints updated.
- Needs you (workbench.R, flow.R): grouped by place (site or serial; sheet and project findings apart), the same check on
  nearby days merged, every line labelled decide / fix in ovid / raw data / add a file; the pop-up can group by what to do.
- Yes/no columns (table_rules.R, editor.R, sheet.R, www/yesno.js): enabled, show, identifies, include, active, hidden and an
  app's `rules$yesno` show Yes/No with a pick in both views and are written TRUE/FALSE; other text is an error.
- Set-up sheets (sheet.js, sheet.R, editor.R): only the rows near the view are drawn (tables over 150 rows); rows are
  converted to and from the browser a column at a time (about 20x faster).
- Processing levels (standards.R): L0 raw, L1A filtered, L1B flagged, L2 QA/QC, L3 refined, L4 derived, from a file's WMO
  codes, review and approval. Dataset card (Settings > Dataset, config/dataset.json) written into NetCDF exports as ACDD
  attributes; `nc_import()` reads CF time series NetCDF (ncdf4, added to the launcher and renv.lock).
- Fixed: clearing the project folder field no longer closes the open project or saves a blank folder (settings_autosave
  `required`; CS, WLR, GT fall back on the open project); the field says to type or browse a folder.
- Fixed: one Standard | Workbench switch in GS and RS (no second Workbench button); the Workbench overview strip waits for
  room instead of failing with "figure margins too large".
- **Output changes:** NetCDF processing_level now uses the new codes (L1A, L1B ...) and carries the dataset card's
  attributes; yes/no cells are written TRUE/FALSE the next time a table is saved. Values are unchanged.

**Changed (ERT is now GS, 2026-10-14)**

- `OVID_APP_RENAMED` (util.R): a project marked with a former app name (ERT) opens in the current app (GS), and
  `ovid_projects_home()` keeps `~/Desktop/Ovid_Projects/ERT` as the GS projects folder until a `GS` one exists. The GS icon,
  test lists and comments use the new name. **Output changes:** none.

**Added (corrections applied, 2026-10-10)**
- applied.R: Corrections applied. Each run writes output/audit/corrections_applied.csv, listing every correction in effect with the values it removed, changed, filled or flagged, plus totals; a chip and pop-up in the top bar (applied_ui/applied_server), copied into exports and added to reports. **Output changes:** one new file in output/audit; period-row removals name their row in the audit reason.

**Added (edits from the standard plot, 2026-10-09; plot and table standards A11)**
- tsview.R 1.5.0: a page can put its own edits on the bar above the selection (actions), with a reason, initials and a choice;
  a click sets input$<id>_act. Dragging up a panel's value axis narrows the selection to a band of values. ts_view_done says
  what was written, with Undo while the set-up table can take it back (ts_view_undo). Series can share a panel in their own
  colours, mark removed (grey cross) and flagged (gold ring) readings, and label each mark; spans shades periods in grey.
- Output changes: none. Only what the plot shows and offers changed.
**Changed (one review list, project menu, time zone, 2026-10-09)**
- One metadata sheet: an app's logger list (metadata.csv) folds into the deployment sheet (meta_sheet, meta_fold, meta_sheet_fields); its columns are optional columns of the sheet. Typed values win over From and To, and the differences are listed. **Output changes:** none.
- One "fix a stretch" table: period_corrections.csv takes the apps' manual removals, calibrations and drift rows (period_view, period_own_rows; a target lists names with |, * is all; app columns such as output). config_migrate() moves an older table in once (config/migrated.json) and leaves the old file in place.
- export_config/read_config_import take an app's CONFIG_EXTRA files as they are, and read an older zip's tables into the ones they fold into.
- Split here writes two rows of the deployment sheet (dep_split) instead of config/logger_splits.csv; older splits are moved in once. The unused group_suggestions_ui is gone.
- Needs you is one review list (flow.R review_list_ui/server), shown on the Needs you page and in the Workbench's Needs you; Check the data, the Workbench Review queue tab and flow_check.js are gone. Keep records "kept"; Not a problem asks for a reason. **Output changes:** none.
- config_update and the `by` argument of the set-up table import are gone; an import replaces or merges (config_after). **Output changes:** none.
- Projects: the project menu sits in the top bar of every app and of the Workbench; Settings shows a read-only Projects list. Opening a project loads all its settings (project_apply_settings) and maps an old fsl_dir to prog_dir when prog_dir is empty.
- One time-zone control (time_zone_ui) in every app; the unzoned-file message points to the app's question or the time-zone table.
- group_by_ui/group_by_server take a label and an empty-list text.

**Fixed (review decisions, rename, set-up notice, what shows, start view, 2026-10-09)**
- Check the data and the Workbench review queue keep one set of decisions in config/review_decisions.csv: Keep is stored as "not a problem" (Flag as "flagged"), and a decided item leaves both lists.
- The runs read review_decisions.csv (review_accept): a flag decided not a problem stays in flags.csv with `accepted` TRUE and is left out of the values' flag columns (review_unmark). The helpers moved from workbench.R to config.R.
- Review ids are made from times written one way, so a midnight start read back as a date alone matches; decisions under the older ids are still found.
- Renaming a site or station also changes period_corrections.csv `target`, calibration_options.csv `applies_to` and display.csv `name`, and carries review decisions to the new name.
- "Set-up changed since the last rebuild" can leave out files a rebuild does not read (cfg_mtime_ignore()).
- What shows says what it does (the strip, Home and Check the data); the unused `exclude` column of display.csv is no longer read or written.
- Settings has "Opens in": Standard view or Workbench, saved as the `ui` setting and used from the next start.
- README: no read_metadata_folder; RS uses the runner.
- The run log points to the Automatic corrections step of the set-up, not the Settings page.
- Deployment sheet, Against the data: "Accept the suggested matches" (all, or the rows selected) writes a name, year, month or only-deployment match's first and last record day into From and To (dep_accept_dates).
  **Output changes:** flags.csv gains an `accepted` column; values whose flags were all decided not a problem lose those flags in exports.

**Removed (formulas in set-up tables, 2026-10-09; Streamline ovid workflow R5)**
- The Workbench sheet (sheet.R, www/sheet.js) no longer works out formulas, and config/formulas/<table>.json is no longer read,
  written or copied by Duplicate. Formulas were kept by row and column, so after Delete, Duplicate or an import they landed in
  the wrong cells. Cells hold values only; a value typed with a leading = stays as text. The bar above the sheet shows the
  cell's value (no fx). Existing config/formulas folders are left on disk and ignored.
- Output changes: none. The tables always kept the worked-out values, and those are what the pipeline reads.

**Added (one core table, 2026-10-09; _hub/Plot_table_standards.md A9)**
- ovid_dt() (table.R, www/table.js, www/table.css) is the one table wrapper; CS gb_dt, WLR nord_dt and GT mi_dt are gone and
  ERT and RS pages use it instead of calling datatable(). A header click no longer sorts: each column has a button for Low to
  high, High to low or Original order, and a filter (a value range, only missing, ticked values, or text). Active filters show
  as chips. Column widths drag from the header edge (double-click fits); Columns shows and hides columns; widths and hidden
  columns are kept per table in the browser, with Reset view. Click a value, drag a block or click a row number for the row
  (Shift extends, Ctrl/Cmd adds); the footer gives the count, mean, min, max and sum, with Copy (Ctrl/Cmd+C) and Esc to clear.
  ovid_dt(limits = list(column = c(min =, max =))) tints values outside a quality limit. Tables whose rows pick a record
  (selection = "single" or "multiple") keep that click. Times show without seconds when every record is on a whole minute;
  missing values show as a dash. The old Columns picker (shell_field.js) skips these tables. Column order cannot be changed in
  the apps' tables: the DataTables extension for it would mix up status colours and server-side paging.
- Output changes: none (display only; exported files are unchanged).

**Added (more on every plot, 2026-10-09; standards A10, A11)**
- Standard plot (tsview 1.4.0): event marks (ts_view_send(events =): dashed line and flag; CS sends new source files and field-log
  entries, WLR deployment starts and field visits); one cursor for the page; the tooltip adds the zone, the record's status and,
  when sent (sources =), the logger and file; keys: arrows move the selection by a record, Shift + arrow extends it, + and -
  zoom, Enter looks closer; Save image at 85 mm or 180 mm (300 dpi) or a 16:9 slide, in a light print scheme with a title and
  a caption; Save data saves the records behind the plot as CSV with a QC column.
- Workbench: + and - zoom on both charts, arrows move the selection on the time chart. After an action is written (Remove,
  Calibrate, Clock, Note ...), a bar says what was done with Undo (or Ctrl/Cmd+Z), which puts the set-up table back as it was
  (editor.R keeps the version before; workbench.R wb_undo_server(), wb_undo_offer()).
- Output changes: none until a run; Undo only changes the set-up table the action wrote.

**Changed (one way to select on every plot, 2026-10-09; _hub/Plot_table_standards.md A11)**
- The Standard time plot (tsview 1.3.0), the Workbench chart and the Workbench point chart now select the same way: drag on
  the plot selects (a stretch, or a box on the point chart), a click the nearest record, Shift adds, Esc clears. A bar above
  the selection gives its size and Zoom to this; the Standard plot adds Look closer and Open in Workbench, the Workbench
  charts the page's own action buttons. To move along, drag the time axis or the strip under the plot, or hold Space and
  drag; the wheel zooms; double-click shows the whole record; Back (Backspace) returns to the previous view. The Workbench
  "Drag: scroll / Drag: select" switches and shift-drag zoom are gone; right-drag still selects. Help texts in the apps say
  the same. ERT's plotly sections zoom with the wheel.
- Output changes: none (display only).

**Changed (styles out of R code, 2026-10-09)**
- The styles that were written inside R strings (the app sides switch and colour schemes in shell.R, column matching on Import in
  metasheet_ui.R, corrections.R) are in www/parts.css, loaded on every Standard and Workbench page (CONTRIBUTING rule 5). The
  tables' and plots' styles are left to the table and plot work. **Output changes:** none; the pages look the same.

**Fixed (values that are not numbers, 2026-10-09)**
- A cell in a raw file that is not a number (a typo, "12,5" where a point is expected, "?") used to become blank without a word. It
  still becomes blank, but each file that had any is now named on the problem list with the count and an example. Blank cells and
  missing-value words (NA, NAN, INF, NULL) stay quiet. convert.R num_cells(), unread_mark(); audit.R unread_report(); used by the
  GT and WLR logger readers and the ERT Syscal/Prosys reader. CS already removed such cells with an audit line ("non_numeric").
- The logger list grouping no longer hides every warning: a logger with no dates gets no first and last (NA) instead of -Inf/Inf.
  grouping.R group_first(), group_last(). The hidden warnings also hid a bug: the empty list (no loggers) had no key column, because
  data.table(key = ...) sets the sort key instead of making a column; it now has one. **Output changes:** none to the data; the problem
  list can have new lines.

**Removed (unused code, 2026-10-09)**
- Functions nothing called: ovid_cards(), ovid_card() and their .d-card styles (shell.R, shell.css), shift_lab() (theme.R),
  flow_gaps() (flow.R), file_sig() (logger_files.R; the apps build the signature themselves) and meta_import_clean() (import.R).
  **Output changes:** none.

## 1.15.0 (2026-10-10)

**Added (Workbench side panels, 2026-10-09)**
- The Tree and the Inspector fold to a 28 px strip with their name and an open button (the button in each panel's header, Alt+[ and Alt+]),
  resize by a 5 px handle on their inner edge (180 to 480 px), and the Inspector floats over the main area (its ⧉ button; drag it by its
  header; ⧉ again docks it). Pages that hid the Inspector (all but Data and Import) show its strip; opening it there is kept apart.
  Open, closed, floating, position and widths are kept in this browser (localStorage, the grid's key prefix); plots refit after each change.
  workbench.R wb_panel_head(), wb_strip(), wb_grip(); workbench.js wbPanels; tests/test_wb_panels.R. **Output changes:** none.

**Added (sides of one app, 2026-10-09)**
- ovid_page() and wb_page() take `areas =`, a named list of the tool values on each side (ERT | GPR). A switch beside the logo shows the count
  on each side (ovid_areas_server()); only the side's own pages show in the rail, tabs and Workbench tabs and tree, and a page in no side
  (Reference) shows on both. Each side keeps its last page; Alt-1 / Alt-2 and `?area=<name>` switch; the side last used is saved in the project's
  config/project_settings.json, and a new project opens on the side with data. A side with none is dimmed, not disabled. www/areas.js;
  tests/test_areas.R. Without `areas` a page is as before. **Output changes:** none.

**Changed (no site data in the code, 2026-10-09)**
- Examples, placeholders, the metadata template's serials and the tests use neutral names, serials and coordinates; UTM test
  values recomputed and cross-checked. test_raw_only.R reads OVID_TEST_RAW_<APP>, else ~/Desktop/Ovid_Projects/demo/<APP>/raw.
  **Output changes:** none.

**Changed (sample tests, 2026-10-09)**
- Sample tests: a new or changed sample fails until accepted (tests/samples/accepted.csv); hand-checked values in reference.csv. testkit.R: accept_sample() writes the row after a person checks the expected .rds. **Output changes:** none.

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). convert_page.R (UI only) holds the page; it calls only convert.R. **Output changes:** none.

**Added (conversions and time zone, 2026-10-09)**
- convert.R: every unit and coordinate conversion in one place (pressure, length, speed, temperature; decimal degrees and degrees-minutes-seconds; UTM both ways; earth-centred XYZ). Sources are in the file and the hub references. The apps' own copies are gone.
- `zone_text()`, `zone_choices()`, and `time_zone_ui()`/`time_zone_server()` (shell.R): the time zone is a setting in every app.

**Output changes:** none expected. The UTM series is the one WLR used, now checked against PROJ.

**Changed (errors reported, 2026-10-09)**
- `try_or()` and `problems_since()` in audit.R: a step that fails gives its fallback value and goes on the problem list with where and why, instead of passing for "no data".
- The example deployment sheet (templates/metadata_example.csv) uses neutral site and station names and coordinates.

**Output changes:** none.

**Fixed (2026-10-09)**
- Projects: the open project is in the list (and Rename and Duplicate work on it) also when the app was started without a
  saved project folder, as ERT and RS are on first use with raw data only or the automatic folder (`projects_with_open()`).
  **Output changes:** none.

**Changed (packages, 2026-10-08)**
- Package versions are pinned in `renv.lock` (repo root, 91 packages, R 4.6.1). The launcher installs what is missing at those versions into the app's library/ before the app starts (falling back to CRAN's current versions, with a note, if a pinned one cannot be installed, e.g. an older version on Windows without Rtools); a portable copy takes renv.lock with it. **Output changes:** none.

**Added (tests, 2026-10-08)**
- Code standards in CONTRIBUTING.md; style notes for changed files with `Rscript lint_changed.R` (lintr, `.lintr`), never failing. **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- The shell's CSS and JavaScript (shell.R: page style, set-up flow style, shell, context menu, field errors) are files in `core/www/` too; the text is the same character for character. **Output changes:** none.

**Changed (tests, 2026-10-08)**
- Browser page checks (`core/uitest.R`): the first page may take 90 s to load (was 30 s), so a busy computer running several suites at once no longer fails GT's check. **Output changes:** none.
- Browser page checks: a missed "page loaded" signal no longer fails them; the check waits for the page to connect to the app instead, and Chrome gets 60 s to start (`chromote.timeout`). **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- The CSS and JavaScript held in R strings (workbench.R, sheet.R, tsview.R, files.R, flow.R, grouping.R, reference.R, runner.R) are files in `core/www/`, read at start; the text is the same character for character. CS's viewer bundle also copies `core/www/`. **Output changes:** none.

**Changed (2026-10-08)**
- Import into a set-up table that declares no fields (`import_spec_guess()`): the type of each column is judged from its name and the table's rules, so day-first dates, decimal commas and choices are put in order like the deployment sheet's. Notebook layout: panels and cards lift alike; tab bar (CS) tighter. **Output changes:** none.

**Changed (2026-10-08, import hardened; new `core/import.R`, tests in `core/tests/test_import.R`)**
- Import and Paste (every set-up table, the metadata sheet, RS checkpoints) read any reasonable spreadsheet: CSV, text, tab, semicolon or
  bar separated; UTF-8, UTF-8 with a mark, UTF-16 and Windows-1252 text; `.xlsx` and `.xls` (several sheets: the one whose headings fit
  is read, any can be chosen); the kind of file is judged by its content, not its name; titles and blank lines above the headings are
  skipped (the row can be set by hand); empty and duplicate headings are made distinct; non-breaking spaces, zero-width characters,
  N/A, `-`, `#N/A` and other Excel errors are cleaned (each said in a note); web pages saved as .xls, PDFs, pictures, empty and
  damaged files get a plain message on the screen, never an R error. Files up to 100 MB are accepted (the old 5 MB upload limit
  stopped bigger ones before they were read) but a table over 200,000 rows is refused as the wrong file.
- Matching columns: name, label or alias as before, now also with a unit or note in brackets left out ("Latitude (dd)"), an alias's words
  inside a longer heading ("Logger serial number (HOBO)"), "Date in / Date installed / Time out", and a spelling slip, each marked
  "check" on the screen; a field goes to one column only; what was matched last time for the same headings is offered first
  (`config/import_memory.csv`). Two columns on one field is an error that names both, not a silent overwrite.
- Values: dates are put into one form before they go in: Excel serial numbers (45531), day/month/year or month/day/year with `/`, `.` or `-`,
  two-digit years, months in words, 12 and 24 hour times. A date such as 03/04/2024 that nothing in the file settles is an error that
  asks which order (before, it was read month first, and a 31/08/2024 was lost). Decimal commas, thousands separators and the minus
  sign are read; a serial Excel has turned into 2.01E+07 is an error; `123.0` becomes `123`; a choice field takes the choice whatever
  the case, and values that are not one of them are listed.
- Before anything is written the screen says what will happen: errors (stop), warnings (rows with no key, repeated keys, keys already in
  the table, values that are not dates, numbers or choices, with examples and what the app will do with them) and notes; a preview of
  the first rows as they will be written; the button is off while there is an error. Updating needs the key column. Replacing says the
  current rows go (Undo brings them back). **Output changes:** none to any result; metadata imported with day-first dates is now read correctly.

**Added (2026-10-08)**
- Sheet: a column with set choices in the app's table rules (e.g. GT sites `profile`, WLR `mode`, calibration `choice`) is
  edited from a dropdown; a typed or pasted value that is not one of them is outlined in red with what to pick, and Apply waits
  until it is fixed (`sheet_choices()`). Free-text columns are unchanged. **Output changes:** none.
- Workbench data grid: Columns (show or hide each column, decimals per number column), Wrap, and count, sum, mean, min and
  max of the selected cells next to the table tools; kept per app. Sheet: Wrap, and the filter row stays under the heading
  whatever its height. **Output changes:** none (display only).
- Workbench tables (the Standard tables are unchanged). A bar between the chart and the data grid: drag to share the room,
  double-click for the usual size; the height is kept per app. Data grid: sort from each heading (again: the other way, then
  by time), a filter under each heading (`>5`, `<=0`, `1..3`, `=2`, `!=0` or text), drag a heading's edge to resize or
  double-click it to fit, drag the grip to move a column; widths and order are kept per app. Sheet: the first column stays in
  view, a filter under each heading, shift-click adds a sort, Fill down (Cmd/Ctrl+D, undoable), Replace all next to Find,
  decimals per number column (Columns), Rows: compact, normal or tall, Export (the table as it shows, as CSV), resize and fit
  columns, kept per table. **Output changes:** none (display only; Export saves a copy).
- Errors under the field they are about (`ovid_field_error()`, shell.R) instead of message boxes, in the shared dialogs and the apps'
  Workbench dialogs; the field is outlined until it is changed.
- Disabled buttons say why on hover: the Workbench's corrections wait for a selection; the sheet's Apply, Undo, Redo, Delete rows.
- Every table with five or more columns has a Columns picker (show or hide; nothing in the table or its file changes).
- "Looks like it was moved. Split here?" also when the level jumps where a new file starts (`group_level_jumps()`, WLR: water
  above the sensor, 5 cm or more). **Output changes:** none.

**Added (projects and grouping, 2026-10-08)**
- `grouping.R`: "Group loggers by" Serial number (default), Deployment, Download or Raw file, for display and selection only (no change
  to processing or outputs); kept per project (config/view.json). Long gaps in a logger's record are Needs you suggestions ("Looks
  like it was moved. Split here?"), kept in config/logger_splits.csv and used by Deployment where there are no deployment rows.
- `projects.R`: Rename (the folder too under ~/Desktop/Ovid_Projects, else the name shown) and Duplicate (settings, raw folders and
  set-up tables, not outputs), with the name checked as it is typed; the automatic folder is marked. **Output changes:** none.

**Added (small UI fixes, 2026-10-08)**
- Workbench data grid: Find (marks matching cells), Go to date (moves the period) and Copy (selected cells as tab-separated text,
  also Cmd/Ctrl+C). Sheets: a Columns picker. Review queue: "Not a problem" asks why (kept with the decision, inline error when blank).
- Empty Workbench: Go to Import and Open a raw file. Downloads show "Preparing..." then "Saved to Downloads". Notebook layout (GT)
  before the first run: later steps dimmed, marked after import, still open. Chips and badges with 6 px corners.
  **Output changes:** none.

**Changed (only the raw folder is needed, 2026-10-08)**
- With no project folder set (or "/", the home folder, or a folder inside the apps' git folder), every app uses
  `~/Desktop/Ovid_Projects/<app>/Untitled` (`ovid_project_dir()`, util.R), made on the first write; a folder the user sets wins.
- No raw folder set: the run stops with what to do (choose it on 1 Project). The logger list is not written when there is no project.
- Tests: `core/tests/test_raw_only.R` runs every app's pipeline with only a raw folder (no metadata, no project folder).
  **Output changes:** none for a project with its folder set.

**Added (UI design gaps, 2026-10-08; tsview 1.1.0)**
- `reference.R`: the Reference page in both views of every app: the app's guide as a searchable index (Calculated values, Data
  handling, QA/QC, Sensors, Glossary), each topic with where it happens in the pipeline, reasonable values, where it is set, Show on
  my data and the PDF.
- Workbench tabs in pipeline order (`wb_tabs_server(pipeline =)`); switching Standard and Workbench keeps the station or site,
  parameters, period and selected stretch (`ovid_ctx()`), and unsaved table edits carry across (editor.R holds them per file).
- `tsview.R` 1.1.0: a series can carry a reference line drawn in its panel (`ref`), such as the neighbour mean.
  **Output changes:** none.

**Added (set-up Workbench design; workbench 0.9.0, 2026-10-08)**
- `sheet.R`: in the Workbench every set-up table is a spreadsheet: a formula bar (name box, fx, Find, Go to date), edit in place,
  arrows and Tab, Shift ranges with count, sum, mean, min and max in the status bar, copy and paste (tab-separated), Delete, sort by
  a heading, Undo, Redo and Apply (changed cells marked until then; the editor's save keeps the table before it in config/history).
  Formulas (=B2*1.05, =SUM(C1:C4), =ROUND(MEAN(D1:D3),2)): numbers, cells, + - * / ^, brackets, SUM, MEAN, MIN, MAX, COUNT, ROUND
  and ABS, read by a small parser (no eval); the value is saved in the table and the formula beside it (config/formulas/).
- `periods.R`: corrections over a period (`config/period_corrections.csv`): remove, offset, scale, linear drift, fill by
  interpolation or flag, on a target (station, site, logger) and parameters, between two dates; applied by a run in the list's order,
  each logged in the audit trail (stage `period`) and flagged values listed with check `period_flag`; the raw files are never
  changed. The Workbench's **Correct a period** button opens it with the selected cells or chart stretch, or typed dates.
- `workbench.R` 0.9.0: **Needs you** at the top right (matches that are only a guess, loggers in no row, files with no serial, the
  last run's problems, each opening where it is settled); the **Review queue** tab (every flag, worst first, chips per check; Open,
  Remove, Correct or Not a problem, kept in `config/review_decisions.csv`); `wb_process_tree()` (each app's own processing steps in
  the tree); corrections over a period in the clicked value's lineage.
  **Output changes:** none until a project has rows in period_corrections.csv; then the values in those periods change as each
  row says, and the run redoes only the records they touch.

**Added (set-up flow; shell 1.2.0, flow 1.0.0, tsview 1.0.0, workbench 0.8.0)**
- `shell.R` 1.2.0: the numbered set-up flow (1 Bring in, 2 Organize, 3 Correct, 4 Report, 5 Save) with each app's own layout
  (`layout`: rail, icons for WLR, notebook for GT, tabs for CS), step status lines (`ovid_steps()`), the project switcher in the top
  bar (`projects_switch_ui()`), the information panel (`aside`), the Standard | Workbench switch and the Explanations switch.
- `flow.R` 1.0.0: unsaved-changes chip (Apply, Discard), which sites or stations show (`config/display.csv`), the information panel,
  Check the data (one plot per site with its automatic checks, decided in place) and `ovid_colnames()` (snake_case headings shown
  as words with their unit: depth_m -> "Depth (m)").
- `tsview.R` 1.0.0: the Standard view's time-series plot (scroll, wheel zoom, view bar, drag-select statistics, Look closer).
- `workbench.R` 0.8.0: right-drag selects (several stretches add), Look closer opens the Workbench at a time; units under the grid
  headings (`wb_grid(units =)`); the quality-rule range limits drawn as dashed lines on the chart and each panel labelled with its
  unit (`wb_chart_data(units =, limits =)`, `wb_match()`, `wb_limits()`); long parameter names wrap in the tree.
- `report_options.R`: neutral placeholders for the title and file name ({name}, {from} to {to}).
- `hobo.R`: reads the records of a U20 or U20L .hobo file directly (header records, launch time, start delay, interval and the
  logger's calibration; escapes and events), for a download that has no HOBOware CSV export. Checked against the CSV export of 200
  downloads: 99.4% of values identical. Other models still need the CSV.
  **Output changes:** none for files that have a CSV export.

**Fixed (run-through on the Mac, 2026-10-08)**
- `shell.R`: an error inside any handler no longer ends the session (the page used to grey out, the old "crash" after deleting the
  output folder or garbling a set-up table). `observe()`/`observeEvent()` run each handler in `ovid_guard()`: a notice with a
  short reference, the app's error log and the console, and the app carries on.
- `shell.R` (`ovid_output_watch()`): pages notice within seconds when the output folder is deleted or replaced outside the app.
- `editor.R` (`edit_read()`): a set-up table that cannot be read, or lacks columns the app expects, is shown with the columns added
  and a notice instead of breaking pages; the file changes only when saved.
- `manifest.R` (`project_check()`): a read-only project folder, output, cache or config folder, or a file where one of these folders
  should be, stops the run with a plain message (was "cannot open the connection").
- `cache.R`: cached files are written with fast gzip (level 1): as quick as before and 10 to 30 times smaller (GT demo cache 809 MB to
  28 MB). Older uncompressed caches are still read.
- `config.R`: a file that is not a readable zip is refused with a plain message on Import configuration.
- `workbench.R`: a removal saved for one parameter was drawn as pending on every panel of the chart (the grid and the saved rows were
  right); `flow.R`: a check with no end time no longer breaks Check the data.
- R sources hold ASCII only (non-ASCII in strings as \uXXXX escapes).
  **Output changes:** none.


**Fixed (edits show on every page)**
- `editor.R`: one live view of the set-up per window (`live_stamp()`, `live_table()`, `live_config()`, `live_settings()`,
  `live_touch()`). Every page reads set-up tables, the deployment sheet and the app's settings through it, so an edit made
  anywhere (another page, a rename, a switch, an import, another window, a run, a hand edit) shows on every page within a
  second, and at once after a save in the same window. An editor that takes up a newer saved table now calls `on_change` (the
  app's pages refresh as after a save here). A reload (rename, run, project switch) no longer drops unsaved edits in other tables.
- Rename names, the deployment-sheet check, the logger-list columns (shown/hidden, labels), the corrections list, the Projects
  list and the typed-field suggestions follow the live view. Apps: pages that kept their own copy now read it too (CS combined
  records, clock-zone labels, deployment sheet in the set-up check; GT site details, Home landcover count, water-year start and
  zero band from Settings; ERT survey and transect forms, elevations on QC and the Workbench; RS flight form, flight edits on
  every page, sources; set-up check in all five).
  **Output changes:** none.

**Fixed (settings saved when changed)**
- `editor.R` (`settings_autosave()`): a setting is saved a moment after it is changed, not only at the next run. CS, WLR and GT
  save their Settings page this way (a changed project folder had stayed unsaved until a run); ERT its two folders (the
  catalogue switches at the next rebuild). RS has no settings fields (its Projects list saves at once).
  **Output changes:** none.

**Fixed (browser autofill)**
- `shell.R`: no browser autofill in any typed field of any app (names, notes, editor cells, table filters, fields drawn
  later). Safari had offered names from the Mac's Contacts in a site-name box. Fields get autocomplete off, and a name
  holding 'search' that Safari's Contacts AutoFill skips; checkboxes and radio buttons are untouched.
- `shell.R` (`suggest_server()`, called from `troubleshoot_server()`, so every app has it): typed fields offer only the app's
  own data as you type: the last 15 values typed in the same field (kept in app_settings.rds as `typed`), and in any field
  whose label names a site, station or serial, the names from the set-up tables (`RENAMES`) and the deployment sheet.
  `rename.R`: New name offers names of that kind the deployment sheet holds and the set-up tables do not yet.
  **Output changes:** none.

**Added (batch E: projects, metadata, renames, reports)**
- `projects.R`: a Projects list in every app (next to the folders; on Sources in RS). A new project (empty, or with the
  set-up tables of another), opening one, deleting its outputs and cache or the whole folder, and removing it from the list.
  Each project keeps its own folders and settings (`config/project_settings.rds`): the one left keeps them and the one
  opened gets its own back, so new data worked on apart never mixes with the main project. Deleting goes to the Trash
  (Recycle Bin on Windows) after a preview; nothing is deleted while a run holds the project, or a folder holding things
  Ovid did not make.
- `metasheet.R`: the deployment sheet fills what an app's logger list leaves blank for the same logger; where both give a
  value and they differ, the list's is used and the difference is listed (run problems and the GT and WLR set-up checks).
  Dates and numbers written two ways count as the same (`meta_same()`).
- `rename.R`: rename a site or station once and every set-up table that names it follows (cells, and lists such as
  applies_to); a preview of every cell, the tables before kept in config/history, and Undo. Each app declares `RENAMES`.
- `report_options.R`: a Report layout panel on the Export page of CS, GT and WLR: the title and file name (with {name},
  {from}, {to}), a line under the title, who prepared it, which sections, and whether a parameter is drawn on the same scale
  at every site or station, the same span, or its own (`axis_limits()`). Kept with the app's settings.
- `report.R`: `rpt_overview(ylim = )`, the limits shared with other reports (nothing else changed; without it, as before).
- `util.R`: `env_with()`.

**Output changes:** none (reports change only when the new options are used).

**Added (automatic corrections list, batch D)**
- `corrections.R`: everything a run changes in the data on its own, listed in one place. Each processor declares its
  `CORRECTIONS` (what it does; removes, changes, fills or flags; the setting or set-up table behind it; where it is switched).
  The Settings page shows the list with each one's state (on, off, always, or by a set-up table and its row count), and
  gives a switch to settings that had none in the app; a switch is saved with the app's settings only when changed
  (`corrections_cfg()` brings it into the run). Every run log lists them by state (`corrections_log()`).
  An entry may give a `default` (on unless said) and `choices` (a menu instead of on and off).

**Output changes:** none.

**Changed (guardrails, batch D)**
- A failed run no longer leaves a mix of old and new outputs (`manifest.R`). Before each run `output/` is copied to
  `<project>/.output_before_run` (a clone on a Mac's own APFS disk: instant, no space until files change). A run that fails,
  is cancelled or stops to ask a question puts it back, adds its own log, drops the cached results it made (they would let
  the next run skip outputs that were put back) and leaves `output/.run_restored`, which the app shows. A copy left by a run
  whose process was killed is put back when the app starts or before the next run (`output_recover()`). Tested on an exFAT
  drive (T7): WLR's 1.3 GB output copies in 6 s, and the output put back is identical after a run fails part way. No copy is kept, and
  the run log says why, when it would not fit on the drive or the output is over 2 GB on a disk that cannot clone.
- Free space: a run does not start with less than 500 MB free on the project folder's drive (`disk_check()`).
- Cloud-synced folders: a project or raw folder in iCloud Drive, OneDrive, Dropbox, Google Drive or Box is noted at the
  start of the run and in its log, with how many raw files are kept online only on a Mac (`cloud_notes()`). Not stopped.
- `lock_holder()` split from `project_lock()`.

**Output changes:** none.

**Changed (guardrails, batches A and B)**
- Settings kept between runs: `settings_save()` changes only the keys it is given (a Run in CS, GT or WLR replaced the whole
  file and dropped every other setting), writes beside the file and renames it, and keeps the copy before as `.bak`;
  `settings_read()` falls back to it when the file is damaged and the app says so (CS, GT and WLR did not open; ERT and RS
  started with no project folder). No check below changes a setting.
- `run_project()` before every run of every app (`manifest.R`): refuses an unset project folder, a project folder inside a
  raw folder or around one, a raw folder that is not there (a drive not plugged in, which dropped its stations' outputs; not
  in RS, which keeps a drive's last scan), another app's project (`.ovid_project` names the app: two apps in one folder
  overwrote each other's audit files) and a folder holding other files (Desktop, home). One run at a time per project
  (`.ovid_lock`; one left by a process that has gone is taken over). Output or set-up files open in another program (Excel
  on Windows) stop the run before it starts, named. `output/.run_incomplete` stays when a run stops part way and the app
  says so until a run finishes. Each refusal says why; nothing is written.
- Damaged files: a set-up table that is not text stops with its name and the last good copy in config/history
  (`read_cfg()`, class `ovid_bad_table`); set-up tables are written beside the file and renamed (`fwrite_safe()`: the
  editor, imports and the pipelines' own additions); cached data files likewise (`saveRDS_safe()`), and one that cannot be
  read (`cache_load()`) clears the cache and runs once more from the raw files (`cache_retry()`).
- Dates: `parse_dt()` no longer takes a format that reads only the start of each cell (a short HOBOware file cut off in its
  last line was read as year 0023, with PM as AM, and every record dropped): an AM/PM or seconds the format lacks, or a
  two-digit year read as four, rule it out; all but two cells is enough.
- Raw files: those the last run used and that are gone are named (`files_gone_check()`); a file read but adding nothing to
  the output is a problem (unless all it had is in another download); an unreadable file is no longer listed as a copy of
  another (`copy_of()`); file names are one Unicode spelling (`nfc()`, from GT); paths over 250 characters are named on
  Windows; TOA5 headers and data skip blank lines (`toa5_skip()`).
- `testkit.R`: `damaged_file_runs()` and `damaged_table_runs()`, in each processor's tests (OVID_FUZZ=1 for every kind of
  damage). Output on the example data: CS and GT identical; WLR identical but one empty file's note ("no records").

**Added (batch C: deployment sheet and set-up table rules)**
- `deployments.R` (from the metadata design; now tested and sourced by CS, GT and WLR): one deployment sheet for every app,
  `config/deployment_sheet.csv`, the eight columns of `templates/metadata_template.csv` (only the serial needed). Its rows
  feed each app's logger list (GT and WLR through `meta_sheet(deploy =)`, CS through its stations), adding loggers the app's
  own list does not name and never overriding it. The editor's Import matches a sheet's columns by the template's headings and
  aliases (`IMPORT_SPECS`). Under the sheet, "Against the data" (`deploy_check_ui()` in `metasheet_ui.R`) shows how each row
  matches what the last run read (dates, year, name, only one, to choose, no data) and adds the deployments no row claims
  (merged: blanks filled, conflicts listed, nothing deleted). Fixed in the draft: every serial was reported as overlapping
  (a column compared with itself); the west-longitude check assumed one hemisphere and now flags a sign unlike the sheet's.
- `table_rules.R`: what each set-up table's cells must be, read by the editor and the set-up check alike. Every table gets the
  rules its column names imply (dates, numbers, latitude and longitude ranges, periods that run forwards, repeated rows); an
  app adds required columns, choices, keys and known values (`known_from()`: the stations, sites or serials a project has) in
  its CONFIG_FILES entry. `with_declared()` adds the findings to each app's set-up check without repeating its own.
- Editor: Undo (the last 30 changes not yet saved) and, under the table, what its rules find as it is edited.
- The set-up check is asked before ERT's Rebuild and RS's scans too ("Rebuild anyway", "Scan anyway"); a run started anyway
  records the findings in its log (`cfg$run_anyway`, `run_logged()`), in every app. `setup_gate()` in `shell.R`.
- `core/tests/run_tests.R`: tests of the core on its own (deployment sheet, rules, editor). **Output changes:** none.

**Added (metadata template)**
- `templates/metadata_template.csv` (blank) and `templates/metadata_example.csv`: the starting metadata sheet, eight
  columns with only Serial number required (Site, Station, Latitude, Longitude, From, To, Notes). The example shows a logger
  that never moved, one redeployed (same serial, two rows), a swap (new serial, same station), a year-only date and a
  serial-only row. Not read by any app yet. **Output changes:** none.

**Changed (Batch 2: shared plumbing, second part)**
- `logger_files.R` (new; sourced by CS, GT and WLR): the raw-folder listing, checksums reused from the last run, byte-identical
  copies, gaps, and the headers of HOBOware CSV, .hobo and TOA5 files, from the copies in CS, GT and WLR. Each app's reader
  still decides what the columns mean. Small changes where the copies differed, each now done the best way any app did:
  GT reads a tab-separated HOBOware CSV and a TOA5 table with no station name or saved tab-separated (as CS did); WLR leaves
  out macOS ._ files and Office lock files when listing (as GT did) and drops entries of rescanned files the same way.
- `timestamps.R` `clock_faults()`: GT's clock checks on written order (impossible time, stray record, reset, resync). CS
  keeps its own check, which reports and never removes.
- `qc.R`: `FLAGS`, `flag_add()` and `flag_mark()`; CS, GT and WLR keep their own columns for where a flag is.
- CS compares stations with core `neighbours.R` (as GT compares sites): the usual difference between two stations is a
  31-day running median, so a seasonal difference is no longer flagged, and fast spring and autumn changes are allowed for.
  The flag detail now gives the largest departure of each run. New CS test.
- `util.R`: `has_value()`, `kv_table()`, `col_or_null()` (GT and WLR reports had identical copies); `par_map()` and
  `run_workers()`: reading in parallel (forked processes on macOS and Linux, one at a time on Windows), in batches so the
  progress bar moves and Cancel works between them. WLR reads the files it has not read before this way: its first run on
  the example data took 204 s instead of 268 s (10 cores). OVID_WORKERS sets the number of processes.
- `audit.R` `audit_restamp()`; `manifest.R` `project_dirs()` (a folder that cannot be created stops the run with the reason,
  now in GT and WLR too), `outputs_prev()`, `outputs_prune()`, `outputs_done()` (station and site files kept until a run has
  rebuilt or reused them): the parts of a run every processor had its own copy of. The stages themselves stay in each app.
- `runner.R`: `args` passes more arguments to `run_pipeline()`; RS scans now use the shared runner (box at the bottom
  right, Cancel between folders).
- Output on the CS, GT and WLR example data is identical before and after, on a first run and on a run that reuses the cache.

**Changed (Batch 2: runs in the background)**
- `runner.R`: Run (Rebuild in ERT) starts the pipeline in its own R process (callr, already a dependency), so the app stays
  usable while it works. A box at the bottom right of both interfaces shows the step ("Step 3 of 6: stitching by logger and
  table"), the item within it ("12 of 40: 18123 Biomet"), the time taken and Cancel; the Run buttons are turned off in every
  window until the run ends, with a note saying why. One run at a time per app; the window that started it gets the result.
- Cancel leaves a flag file the run looks for at each message (`say`) and each item (`run_tick()`, new in `util.R`). It then
  unwinds to the runner through a restart, past every `tryCatch`, so no step records the cancel as a problem and nothing
  is stopped in the middle of writing a file. The run log ends "cancelled by the user". Files already written stay and the
  next run redoes what the cancelled run did not finish (CS example data: cancelled half-way, then run again, gives the
  same output as one run). A run that has not stopped 30 s after Cancel is ended.
- `run_tick(i, n, what)` in the main loops of CS (file headers, logger tables, stations), GT (loggers, quality rules, sites),
  WLR (loggers, deployments) and ERT (survey files, quality checks): at most four lines a second, left out of the run log;
  nothing outside a background run. `run_tick`, `run_check_cancel` and `RUN_TICK` are not part of any step's code
  fingerprint; the loops that call them changed, so the first run after this redoes those steps once.
- `OVID_RUN_SYNC=1` runs in the app's own process as before (the apps' tests set it; also used when callr is missing).
- A WLR or GT run that needs the time zone of files that state none still asks; the question comes when the run stops.
- RS keeps its own background scans for now.

**Changed (Batch 2: shared plumbing, first part)**
- Helpers each app had its own copy of are in core: `out_dir()`, `zip_dir()`, `step()`, `page_head()`, `empty_msg()`,
  `editUI()`, `cfg_mtime()`, `run_mtime()`, `%|%`, `%||%` (NULL or empty), `thin()`, `rx_escape()` (26 app copies gone).
- `norm_serial()`: one spelling of a serial number, with `SERIAL_KEEP` set by each app ("alnum" by default; WLR "digits").
- `correction_span()` and `shift_times()` (`timestamps.R`): the start/end span of a clock correction and the shift or
  prorated drift, used by the clock corrections of CS, GT and WLR. Each app keeps its own table and audit lines.
- `fmt_span()`: one way to write a span of seconds ("45 s", "75.5 min", "1.5 h", "3 days"; sign kept, blank when missing),
  GT's version, replacing the CS, GT and ERT copies. CS audit and problem lines for spans of 60 to 120 min now read in
  minutes ("90 min", was "1.5 h"); ERT shows short survey durations in seconds and no longer rounds them to whole minutes.

**Changed (Batch 1: safety net)**
- Cache keys per step: `code_fingerprint()` hashes only the code a cached step runs (its entry functions and everything they
  use, followed through the app's and core's files; comments and layout do not count; `say()` and settings are left out).
  Each app keeps a `STEP_HASH` per cached step instead of one `CODE_HASH` over every file, so an edit to a report, a plot,
  a page or a core file a step does not use redoes nothing. On the GT example data a changed core report function now reuses
  every logger and site (21 s instead of 123 s); CS no longer re-reads every logger after a core edit. `code_names()` limits
  the search to names the code files define, so the hashes are the same wherever the pipeline is sourced. `CODE_HASH` stays
  for the run manifest. The first run after this redoes everything once (new keys).
- `core_shadows()` / `warn_core_shadows()`: an app function with the name of a core function (which would replace it for
  every caller) is reported at start-up and fails the app's tests; `say()` is the one allowed. CS `load_configs()` is now
  `cs_load_configs()`, WLR `read_lines_any()` is now `wlr_read_lines()`, and CS's copy of `num()` is gone (identical to core's).
- Output on the CS and GT example data is identical before and after (every output file compared, run ids and times aside).
- Run log: every run leaves `output/logs/<start>_<app>.log` in the project (kept after the app closes, newest 50): versions
  and folders (home as `~`), what the run printed, each warning and message, and for an error its reference (`E-` and six
  characters, the same for the same message) and the calls that led to it, then the time taken and whether it finished.
  Each app's `run_pipeline()` is now `run_logged()` around `run_pipeline_steps()`. Errors outside a run (a page or a
  button whose code stops) go to `~/.ovid/logs/<app>_app.log` (`log_app_errors()`).
- Troubleshooting zip: the lifebuoy in every app's top bar (`troubleshooting_zip()`, `troubleshoot_server()`) saves one zip
  with a reference code: versions, the newest run logs and the app's error log, problems and the run records, the set-up
  check as it stands, the set-up tables (columns that look like passwords or tokens left out) and when each was saved. No
  raw data, nothing over 5 MB, home folders shown as `~`; the readme lists the errors in its logs by reference.
- `build/renv.lock` and `build/make_lockfile.R`: R 4.6.1 and the exact versions of the 81 packages the five apps use (from
  each launcher's list, the core's and the optional ones), for the bundled-R build and `renv::restore()`.
- `testkit.R`: reader tests on real sample files. Each processor keeps one small real file per format in `tests/samples/`
  (picked by `tests/make_samples.R <folders>` with the app's own sniffer: CS TOA5 and TOB3, WLR HOBO CSV and binary,
  Solinst CSV and XLE, GT HOBO, GeoPrecision CSV and F2B, RBR HEX and TXT, EM50 XLS; ERT AGI STG and EarthImager STG) and
  what its reader made of each; a test reads each twice (identical) and against that (`OVID_UPDATE_SAMPLES=1` saves a
  deliberate change).
- `uitest.R`: click-through tests in a real browser (chromote): the app in its own R process, every page of the rail and
  the Workbench clicked, each failing on an output error, a JavaScript error or an error the server writes.

**Changed**
- `code_hash()` leaves out the core files that only build screens (`CORE_UI_FILES`: `shell.R`, `workbench.R`, `editor.R`,
  `files.R`, `launcher.R`; no pipeline sources them), so a change to an interface never makes an app redo its work. The
  fingerprint changes once with this: each app redoes all loggers once on its next run.
- `workbench.R` is workbench 0.5.0: the data view's parts are shared by every app's Workbench (they were CS's):
  `wb_data_ui()` (chips, layout, period, overview strip, chart and toolbar, cell bar with the app's corrections, grid),
  `wb_chart_ui()`, `wb_chart_server()` (the chart's data as messages, full resolution for a zoomed-in window, Use as period),
  `wb_chart_data()`, `wb_grid_cells()` (optional decimals per column), `wb_sel_cells()`, `wb_sel_table()`, `wb_state_chip()`,
  `wb_flag_runs()` (periods from a per-row flag column), `wb_overview_plot()`, `wb_keys_js()`, `wb_preview()` /
  `wb_html_table()`, `wb_digits()`, `wb_fmt()`; and the frame: `wb_tools_tree()` (the Standard pages as a tree),
  `wb_tabs_server()` (Import, the data view and the pages opened last), `wb_setup_tree()` and `wb_open_attr()` (the set-up
  tables, each opening the page and tab that edits it), `wb_watch_outputs()` (outputs written by another window are read
  again), `wb_redone()` (the run message). `wb_page(theme =)` takes a bslib theme, for the Bootstrap 5 apps.
- Workbench 0.6.0: a point chart for records that are not a time series (`WB_XY_JS`, `wb_xy_ui()`, `wb_xy_message()`; ERT's
  pseudosection): points at x and y coloured by a value with a colour bar, flagged and saved states drawn as rings, box zoom,
  wheel zoom, pan, and a click or a box that selects the points' rows in the grid. `wb_data_ui(xy = TRUE)` uses it and drops
  the period. The grid selects whole rows by key from a chart (`wbGridSelectRows`). The data view's tool row keeps its controls
  on one line in Bootstrap 5 apps too. `wb_xy_message(equal = TRUE)` draws x and y to the same scale (positions in metres, RS).

**Added**
- `standards.R`: one processing-level scale for the suite (`PROC_LEVELS`: L0 raw, L1 quality controlled, L2 corrected, L3
  gap-filled, L4 derived, mapped to the drone levels and provisional / approved; `proc_level()` from a variable's WMO codes,
  `proc_level_label()`, `proc_levels_text()` for readmes); the WMO codes in CF terms (`cf_qc_atts()`: `flag_values`,
  `flag_meanings`, `status_flag`); and a NetCDF writer in base R (`nc_write()`, classic format with 64-bit offsets, read back
  by `nc_read()`; checked against SciPy's reader) with CF time (`nc_time()`, `NC_TIME_ATTS`), ACDD-1.3 global attributes
  (`acdd_atts()`) and the station variables of a single-station file (`nc_station_vars()`). No new package.
- `trend.R`: Theil-Sen, Hodges-Lehmann and the Mann-Kendall test with the Hamed and Rao correction (WLR's, now shared; `min_n` lets a short annual series be tested without the correction). WLR's barologger checks give the same results.
- `metasheet.R` and `metasheet_ui.R`: the metadata sheet, an optional table of what people know about what was measured
  (`config/metadata.csv`), for when there is no metadata file. Each app gives the fields its pipeline can use (`META_FIELDS`:
  label, type, unit, choices, use, aliases, which fields identify a row); each project can relabel, hide or retype them, change
  a list's choices and add columns of its own (`config/metadata_fields.csv`), which are kept, shown and exported. Every field
  but the key is optional. The screen fills it from what the last run found, from rows pasted from a spreadsheet, or from a CSV
  or Excel file with its columns matched by heading (`meta_guess`), and adds, updates by key or replaces rows (`meta_merge`).
  Values are read by type (`meta_values`: a number typed with its unit, a date, a list); a value that cannot be read is left
  blank and noted, never an error.
- Paste and Import in every set-up table editor (`editor.R`, with `metasheet_ui.R`): rows copied from Excel, or a CSV or Excel
  file, with its columns matched to the table's.
- `editor.R`: the set-up table editor every app uses (`ovid_edit_ui()`, `ovid_edit_server()`; it was CS's). The same table
  can be open in both interfaces and in several windows: a copy with no unsaved edits takes up a newer saved version within two
  seconds, and one with unsaved edits asks before it overwrites it (`file_stamp`). `append`, `mutate` and `replace` start
  from the saved version and save at once; every save keeps the version before it in config/history; viewer mode writes nothing.
  Each app passes its own table renderer (`dt =`).
- `key_rows()` and `key_cfg()` (`cache.R`): the rows of a set-up table, and the settings, as a cache key by value, so an empty
  table and an empty part of a longer one key alike, and a run from the app reuses what a run from the command line made.
- `tz_rows_for()` (`timezones.R`): the rows of the time-zone table that can apply to a logger's files whatever their first
  reading, so a time-zone row redoes only the loggers it can match.

**Fixed**
- The Workbench's script registered its clear-selection message with a handler Shiny refuses (no argument), which also stopped
  the next one, open-a-table, from being registered: a correction left the grid selected, and opening a set-up table from the
  inspector did nothing.

**Added**
- `workbench.R` (workbench 0.4.0): the Workbench interface layout, opt-in beside the Standard shell. `wb_page()` (title bar with
  the app icon, project, the Standard | Workbench switch and Import / Run / Export; tree, main and inspector panes; status bar),
  `wb_wanted()` (picks the interface from `?ui=`, then `OVID_UI`, then the app's setting; Standard by default), and the pieces
  `wb_row` (with a dot for unsaved changes), `wb_tab`, `wb_sec`, `wb_fact`, `wb_kv`, `wb_step`, `wb_state`. `wb_grid()` is a
  spreadsheet with its selection script: cells by click, drag, shift-click and cmd/ctrl-click, records by their time, parameters
  by their heading, keys; the selection comes back as runs of rows per column. `WB_CHART_JS` is a chart drawn in the browser
  (a panel per parameter on one time axis): drag to zoom, wheel to zoom in and out at the pointer, alt-drag or sideways wheel to
  pan, double-click to reset, shift-drag or a click to select in the grid, with the grid's selection highlighted on it. Its data
  come as messages; a long period comes thinned to the low and high of each stretch and a zoomed-in window at full resolution. An app's Standard pages can sit in the Workbench's
  main pane in the hidden tab set `nav`, so `ovid_go()` and `data-ovid-go` links work there; `data-wb-open` opens a page and
  then a tab or folded section on it. The inspector hides on those pages. Styles for pages, cards, forms, tiles, dialogs,
  notifications, the grid and corrections not yet run.
  Colours come from the app's palette, so each app keeps its theme. Dark only. Used by CS, WLR and GT. Not part of the code
  fingerprint (see `code_hash()` above).
- `OVID_SCHEMES$`olive-crt`$roles`: olive-crt's Neovim palette with its own role names (bg, bg_alt, bg_statusline,
  bg_visual, border, the error/warn/info/hint grounds and the rest), used by CS. Dark only.
- `olive_crt_palette()`: those roles in the token names the shell and the Ovid design system read (green = ok = teal, aqua =
  sand, accent = olive). CS builds its palette from it; WLR, GT, ERT and RS keep their own schemes.

**Output changes**
- None. `metasheet.R` is new pipeline code, so each app's code fingerprint changes and its next run redoes everything once.



## 1.14.0 (2026-10-06)

Affects CS, WLR, GT and ERT. Any core change makes every app redo all loggers on its next run.

**Added**
- `qc_rule_ids()` and `qc_read_rules()`: every row of a rule table keeps a `rule_id` (Q-01, Q-02 ...), given once and saved to
  the file, so a flag can name the row that raised it after rows are added, edited or reordered. A table in `CONFIG_FILES` opts in
  with `rule_ids = "Q"`, and `load_configs()` then reads it this way.
- `qc_spike_idx(confirm = 5)`: a candidate must also be more than the threshold from the running median of that many readings,
  so a good reading between two dropouts is kept. `qc_step_idx(min_hours = 1)`: no rate is taken over less than that many hours.
  Both default to the old behaviour (WLR and GT unchanged).
- `qc_flags_table()`: `flags.csv` in every app keeps its own columns first and ends with the same four: `rule_id`, `qc_code` (the
  WMO code the check maps to), `app` and `run_id`.

**Output changes**
- `flags.csv` gains four columns at the end; rule tables gain a `rule_id` column. Values and removals are unchanged.

## 1.13.0 (2026-10-06)

Affects CS, WLR and GT (and ERT for the PDF fix). Any core change makes every app redo all loggers on its next run.

**Added**
- `zone_for(..., unzoned = )`: a zone for files that state none (NA = the base zone, as before), and `unzoned_check()`, which asks the app when no file in a run states its zone.
- `qc.R`: the shared mechanics of quality checks. `qc_runs`, `qc_run_id`, `qc_spike_idx` (optional threshold that follows a
  sensor's usual step), `qc_flat_idx` (minimum run, zero-exempt band, how gaps count), `qc_step_idx` (a change faster than a rate
  per hour), `qc_cond_runs_idx` (a condition lasting a given time).
- WMO quality control codes (BUFR code table 0 33 020): `WMO_QC`, `qc_wmo_register`, `qc_wmo_class`, `qc_wmo_combine`,
  `qc_wmo_worst`. Apps register what each of their checks means.

**Changed**
- CS, WLR and GT now call the shared spike, flat-line and run functions instead of their own copies.

**Fixed**
- `report.R`: PDF reports chose cairo whenever R reported it, which failed on a Mac without XQuartz and stopped the WLR and GT
  test suites. Cairo is now used only when it actually loads; otherwise the plain PDF device is used.

**Output changes**
- None from the refactor: CS, WLR and GT gave identical output on real data before and after.

## 1.12.0 (2026-10-05)
- `read_metadata_folder` and `apply_config_import(mode = "update", by = )` for pulling a metadata folder; `config_after` for a preview. (`read_metadata_folder` removed 2026-10-08.)

## 1.11.1 (2026-10-05)
- An end date with no time in a set-up table covers the whole day.

## 1.11.0 (2026-10-05)
- `timestamps.R` (corrupt timestamps among good records), `distinct_cols` (colour-blind separation). First version on GitHub with
  all five apps (shell 1.1.0 adds each app's icon).
