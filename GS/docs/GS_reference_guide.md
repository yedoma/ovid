# ovid GS Reference Guide

Geophysical surveys (electrical resistivity tomography and ground-penetrating radar): what the app does to every file and measurement, why, and how to check it.
For GS 0.5.0 (ovid Core 1.15.0). ovid GS is a catalogue, metadata, QC and viewing tool; it does not invert.
GS was called ERT before October 2026; projects made as ERT open unchanged. The ERT | GPR switch in the header moves between resistivity surveys (sections 1 to 8) and GPR lines (section 9).
Settings are quoted with their defaults; change them in the app or in `CFG` at the top of `gs_pipeline.R`.

**Contents**

1. [Quick start](#quick-start)
3. [A typical session](#a-typical-session)
4. [What happens to the data, in order](#what-happens-to-the-data-in-order)
5. [1. Finding files](#1-finding-files)
6. [2. Reading instrument files](#2-reading-instrument-files)
7. [3. Surveys, copies and processed sets](#3-surveys-copies-and-processed-sets)
8. [4. Geometry, arrays and pseudo-depth](#4-geometry-arrays-and-pseudo-depth)
9. [5. Quality control](#5-quality-control)
10. [6. Statistics](#6-statistics)
11. [7. Metadata, projects and transects](#7-metadata-projects-and-transects)
12. [8. Exports, sharing and opening a catalogue](#8-exports-sharing-and-opening-a-catalogue)
13. [9. GPR lines](#9-gpr-lines)
13. [Set-up check, time zones and file audit](#set-up-check-time-zones-and-file-audit)
14. [Reading the outputs](#reading-the-outputs)
15. [Electrode arrays](#electrode-arrays)
16. [Plots and set-up tables](#plots-and-set-up-tables)
16. [Troubleshooting](#troubleshooting)
17. [Glossary](#glossary)
18. [References](#references)

## Quick start

- **Mac:** double-click `Open GS (Mac).command` in the Ovid_Apps folder. **Windows:** double-click `GS/Ovid_GS.bat`
  (`Make_Portable.bat` makes a copy that runs on computers without R). From R: `shiny::runApp("GS")`.
- On the **Settings** page give the raw archive (one or more folders, read only) and a project folder (catalogue, metadata
  and settings are kept there; never inside the raw archive), then **Save and rebuild the catalogue**.
- Without the app: `Rscript gs_pipeline.R` (folders from `CFG`, or `GS_RAW_DIRS` separated by `;` and `GS_PROJECT_DIR`).
- Tests: `Rscript tests/run_tests.R` builds a small synthetic archive and checks the results.

Raw files are never changed. Quality control only adds flags; nothing is deleted. Failed, partial and unreadable surveys
stay in the catalogue.

## A typical session

**1. Archive.** Every survey with its date, project, transect, instrument, array, number of measurements, share passing QC
and quality. Filter by year, project, transect, folder, instrument or quality, or search. Select several rows and add them to
a project or a transect in one step; double-click a row to open it. **Export catalogue** writes a zip others can browse.

**2. Survey: pseudosection.** Apparent resistivity on a log scale from 1 to 10,000 ohm-m (Display options: log or linear,
any range). Flagged measurements are drawn on top as crosses, one colour per reason, with their counts in the legend.
Hover a point for its values. On the right, the details people enter.

**3. Survey: about.** Everything read from the files, grouped by instrument, acquisition, geometry, signal and record.
Other tabs: distributions, contact resistance by electrode, error against current, geometry (with elevations), and a
filterable table of every measurement.

**4. QC.** Start from a profile and move any limit; the survey is re-flagged at once. Each limit has an (i) note saying what
it measures and where the value comes from. **Save profile** (in the Workbench) keeps the settings; **Export survey (ZIP)** on Survey writes them
with the data.

**5. Map.** Transects on satellite imagery (or streets), one layer per survey year; click a year in the legend to hide it, or
filter by year and project above the map. Enter each transect's start and end points and its surveys on the right; the table
below lists all transects, by year or by project. The **Legend** switch shows or hides the legend, and **Export view
(PNG)**, with or without it, saves the map as it is on screen at 250 ppi (`ovid_gs_map_<date>.png`). *The three lines in this screenshot are illustrative examples, not surveyed
positions.*

**6. Reference.** How the app works, the QC profiles and rules, every statistic, the electrode arrays with diagrams, and the
sources.

## What happens to the data, in order

1. **Find** every file under the raw folders (zip files are opened, not changed) and tell what each one is.
2. **Read** each instrument file once into the common ovid GS model; keep the instrument's own fields beside it.
3. **Join** companions: contact resistance (`.crs`) to each measurement, the command file (`.cmd`) to the survey.
4. **Link** copies, merged files and processed sets (EarthImager, Res2DInv) to the surveys they came from.
5. **Summarise** geometry: electrodes, spacing, line length, gaps, array, elevation range.
6. **Flag** measurements with each QC profile and compute the statistics.
7. **Write** the catalogue, one folder per survey, and a pseudosection preview for every original survey.

Measurements are written as read. People's metadata, elevations, manual flags and QC profiles live in `config/` and are
applied on top.

## 1. Finding files

Every file is listed in `catalogue/files.csv` with its format, role and status, readable or not. Content is checked first,
the extension second.

| Role | Formats |
|---|---|
| data | AGI SuperSting `.stg`; IRIS Prosys exports (`.csv`/`.txt`); IRIS binary `.bin` (listed, no reader yet) |
| processed | EarthImager sub-sets and trimmed sets (`.stg`); Res2DInv input (`.dat`) |
| companion | `.crs` contact resistance, `.cmd` command file, `.trn`, GPS, `.xyz`, SuperSting memory dumps (`*.stg.bin`, `CmdDir.bin` ...) |
| result | EarthImager inversion output (`.out`, `.ini`), inverted models (x, z, resistivity `.dat`), zip archives, Matlab files |
| notes | photos, PDFs, documents |

A zip is opened once into `<project>/cache/zip/` and its members are listed as `<zip>!/<member>`, so a SuperSting download
kept as a zip is read like a loose one. A file that is the same as a loose file (same bytes) is one survey with two paths;
the loose file is the primary copy.

## 2. Reading instrument files

**AGI SuperSting `.stg`.** Three header lines (instrument, serial, firmware, survey date, record count), then one line per
measurement: V/I (ohm), repeat error in tenths of a percent, current in mA, apparent resistivity, the survey name typed on the
instrument, and the x, y, z of A, B, M and N. Settings follow as `Cmd=`, `HV=`, `Cyk=`, `MTime=`, `Gain=`, `Ch=`, and on IP
firmware further `IP:` fields. ovid converts error to percent, current to A, and computes voltage = V/I x I.

**AGI `.crs`.** One line per current injection: the A-B pair resistance (ohm) and current (A). Each measurement takes the
reading for its A and B taken at or just before it.

**IRIS Prosys exports.** Three header styles are read: old (`Spa.1..4`, `Rho`, `Dev.`, `M`, `Sp`, `Vp`, `In`, `Stack`),
Prosys II (`xA(m)`, `VMN (mV)`, `IAB (mA)`) and Prosys III (`xA (m)`, `Dev. Rho (%)`, `Dev. M`, y and z columns). V/I = Vp / In.
The exports name no model or serial, so the instrument is recorded as *IRIS Instruments (model unknown)*; correct it on the Survey page if you know the model. Every column is kept.

**Res2DInv `.dat`** (general array). Read as a processed set; its first line often names the original instrument file
(for example `FG72_3917.bin`), which is kept as provenance.

**Remote electrodes.** Coordinates of 9,999 or more (either maker) mark an electrode at infinity; it is electrode 0, and the
geometric factor treats its distance as infinite.

## 3. Surveys, copies and processed sets

A survey is one instrument data file. Its id is `ERT-` and the first ten characters of the file's MD5 checksum, so renaming or
moving a file keeps the id. Measurements are matched by time stamp and V/I to earlier surveys:

| Type | Meaning |
|---|---|
| Original | Not found in any earlier survey |
| Copy | Same measurements as one survey, different bytes |
| Subset | Part of one survey (or a survey with records added) |
| Merged | Measurements from two or more surveys |
| Processed set | EarthImager or Res2DInv file made from a survey |

Records read are compared with the count in the header: fewer means **incomplete**; more is noted (records added later).

## 4. Geometry, arrays and pseudo-depth

- **Electrodes** are numbered from their distance along the line when positions sit on a regular spacing, so unused positions
  keep their numbers (and are counted as *unused positions*).
- **Geometric factor** k = 2 pi / (1/AM - 1/BM - 1/AN + 1/BN), from the 3-D distances; apparent resistivity = k x V/I
  (Telford et al. 1990; Loke 2004; Binley and Kemna 2005).
- **Array** of each measurement from its electrode numbers: Wenner, Schlumberger, dipole-dipole, pole-dipole, pole-pole or
  mixed. A survey is named after the array of at least 80 % of its measurements, else *mixed, mostly ...*.
- **Pseudo-depth** = 0.19 x the distance between the outermost electrodes, below the centre of the four electrodes: close to the
  median depth of investigation (Edwards 1977; Loke 1999, Table 2). It is a plotting convention, not an image of the ground.
- **Elevations** can be added per survey on the Geometry tab (CSV with electrode and elevation, or distance and elevation,
  interpolated). They change only the z of the electrodes; the instrument's apparent resistivity is kept as read.

## 5. Quality control

A profile is a set of limits. A measurement that breaks a limit is flagged with a reason; every profile is computed and none
deletes anything. The catalogue and previews use the first profile in `CFG$qc_profiles` (default **frozen_ground**).

| Rule | Exploratory | Frozen ground | Standard | Strict | Source |
|---|---|---|---|---|---|
| Repeat error, max (%) | 10 | 5 | 5 | 3 | EarthImager 3.5, 4.1.3 |
| Current, min (mA) | - | - | 20 | 20 | EarthImager 3.5 |
| Voltage, min (mV) | - | 1 | 1 | 1 | EarthImager 4.1.1 |
| abs(V/I), min (ohm) | 0.0001 | 0.0002 | 0.0002 | 0.0005 | EarthImager 4.1.2 |
| Apparent resistivity (ohm-m) | 0.1-100,000 | 1-100,000 | 1-100,000 | 1-100,000 | EarthImager 4.1.4-4.1.5 |
| Negative apparent resistivity | flag | flag | flag | flag | EarthImager 4.1.7 |
| Contact resistance, max (ohm, A-B pair) | - | 50,000 | 10,000 | 5,000 | EarthImager 3.5.9 |
| Repeated quadrupoles, geometry | geometry | both | both | both | |
| Outlier (robust z of log rhoa) | - | 6 | 6 | 4 | |
| Reciprocal error, max (%) | - | - | 5 | 3 | EarthImager 4.1.6 |

**Frozen ground** keeps the manual limits but drops the 20 mA current rule and allows 50,000 ohm contact, because winter and
permafrost surveys routinely read 20-35 kohm A-B contact and drive less current. **Exploratory** flags only
clearly impossible values. Profiles `supersting_r8` and `iris_syscal` apply to AGI or IRIS data only. Try any profile on the QC
page and save it in the Workbench; profiles live in `config/qc_profiles.csv`. Manual flags (by measurement or electrode) go in
`config/manual_flags.csv` and can be switched off with `active = FALSE`. The `moving_median` and `bad_electrode`
rules are in the defaults of new projects; in any project they can be switched on or off and tuned on the QC page, then saved as
a profile in the Workbench. Every row of `qc_profiles.csv` has a `rule_id` (Q-01, Q-02 ...), and each survey's `flags.csv` (measurement, rule, flag,
value) ends with `rule_id` (the row behind the flag, or the rule's name for electrodes turned off and manual flags), `app` and
`run_id`, as in the other ovid apps (GS has no WMO code).

The **survey quality** (Good, Usable, Usable with caution, Poor, Unusable, Incomplete, Unknown) and its reasons are set by people
on the Survey page and are separate from the measurement flags.

### Added in October 2026: moving median, bad electrodes, electrodes turned off, limits by site and season

All of these flag and never remove, like the rest of GS's quality control.

- **`magnitude_sd`** (value `k` or `k;1`; the `cpers` profile sets it to 9). A measurement is flagged when its apparent resistivity is more than `k` standard deviations from the survey mean (`k;1` works on log10 apparent resistivity). Herring et al. (2023) remove values more than 9 SD away. One huge value inflates the SD, so this catches only gross spikes, and no value can lie more than (n − 1)/√n SD from the mean: 9 SD can flag nothing in a survey of fewer than about 83 measurements (it is not applied below 10 positive values). Use `outlier_mad` for smaller surveys.
- **`moving_median`** (value `window;percent`, default `5;7`). Measurements are grouped into depth levels (one pseudo-depth with the same pair of dipole lengths, so different arrays are not mixed) and ordered along the line. A measurement is flagged when its log10 apparent resistivity departs from the moving median of the window around it by more than that percentage of the median. Herring et al. (2023) describe a 5-point moving median of log apparent resistivity per depth level with a 7 % departure, as an example to test per survey; the paper does not say whether the 7 % applies to the log value or to the resistivity, and ERT reads it as the log value. The CPERS processing notebook uses the same filter.
- **`bad_electrode`** (value `percent;minimum`, default `25;10`). After the other rules, an electrode used in at least `minimum` measurements is bad when more than `percent` of them are flagged, as in the CPERS processing notebook; every measurement that uses it is then flagged and the electrode is named in the flag. Repeats, bad geometry and electrodes turned off are not counted.
- **Electrodes turned off** (`config/electrode_status.csv`, *Electrodes turned off*): one row per electrode or range (`12`, `5-8`, `3;9`) with `status` = off, either for one `survey_id`, or for a `transect_id` between `from` and `to`. Every measurement using it is flagged "electrode turned off", and the survey's `electrodes.csv` gains a `status` column marking it off.
- **Limits by site and season.** In `qc_profiles.csv`, `applies_to` can also be `site:<site>`, `transect:<transect_id>` or `months:<list>` (for example `months:11-4` for November to April). When a rule has rows at several levels the most specific wins: survey, then transect, then site, then months, then instrument, then all. This is how a contact-resistance limit can be set per substrate and season: Herring et al. (2023) give below 5 kΩ for unfrozen fine-grained or wet deposits and 50 to 200 kΩ for coarse or frozen ground.

**Not switched on in an existing project.** New projects get `moving_median` and `bad_electrode` rows in the `standard`, `strict` and `frozen_ground` profiles; an existing `qc_profiles.csv` is left as it is. Run on a test archive (413 surveys, 297,725 measurements) with both added to `frozen_ground`, the existing rules flagged 15.7 % of measurements, the moving median added 6.6 % and the bad-electrode rule 11.8 %, and the median survey kept 90 % instead of 99.5 %. Look at the flagged measurements on the QC page before turning them on.

## 6. Statistics

`catalogue/survey_stats.csv` holds, for each survey and profile, the distribution of apparent resistivity, V/I, current, voltage,
repeat error, contact resistance and IP: n, minimum, 5th, 25th, 50th, 75th and 95th percentiles, maximum, mean and standard
deviation, for all measurements and for those passing QC. Statistics work on empty and partial surveys. The Reference page in the
app explains every statistic shown.

## 7. Metadata, projects and transects

People's metadata is kept in `config/` as CSV, independent of the raw files; only values that differ from the files are stored.

- **Survey:** name, date acquired, instrument (if the files are wrong), operator, project, site, transect, weather and ground conditions, quality and reasons, notes.
- **Transect:** id, name, site, start and end point (latitude, longitude; length and bearing are computed), notes. A transect is
  the physical line; its surveys from different years are linked to it.
- **Project** and **site** tables (`projects.csv`, `sites.csv`). Projects are first guessed from the top folder; a project set by
  people wins.

## 8. Exports, sharing and opening a catalogue

- **Export survey** (Survey or QC page): `measurements_all.csv` (every measurement with `qc_pass` and `qc_flags`),
  `measurements_passed.csv`, `instrument_fields.csv`, `electrodes.csv` (with elevations), `contact.csv`, `metadata.json`,
  `pseudosection.png` and `provenance.json` (source files, format, instrument, ovid version, QC profile and limits, manual flags,
  elevations, export time, survey and transect ids).
- **Export catalogue** (Archive page): a zip with `index.html` (opens in any browser: a searchable, sortable list with
  pseudosection previews), the catalogue tables, metadata and QC profiles; measurements optionally.
- **Open a catalogue** (Archive page): loads such a zip into `GS_imports/<name>` beside your project. A banner shows that you are
  viewing an imported catalogue, with a link back to your own. Without measurements, the Survey page shows the preview and details.

## 9. GPR lines

MALA lines are read from their own files: `.rd3` (16-bit) or `.rd7` (32-bit) traces, the `.rad` header, `.cor` positions and heights, and `.mrk` markers. Raw files are never written; flows, picks and markers are kept in the project's `config/` folder.

### GPR Map

Every line with GPS is a track on the map, with filters (antenna, year, with markers only) and the line table. Under the map, the selected line's radargram is shown read-only with a shared cursor: moving along the radargram moves a dot on the map, and moving along the track moves a line on the radargram. Lines without GPS are listed, not placed.

### GPR processing

One screen in both views. The flow is a list of steps run in order, each with a switch, a one-line summary and its settings folded under it; Up and Down reorder them. Presets, Undo, Before / after and Apply to all lines are on the flow. The flow is saved with the line as versions (`config/gpr_flows/<line>.json`); a line with no saved flow starts from the defaults.

| Step | Default | Source |
|---|---|---|
| Order | DC / dewow, time zero, band-pass, gain, background removal | REFLEXW manual; GroundVision manual |
| Dewow | window of 1 period of the antenna frequency (0 to 5) | REFLEXW manual |
| Time zero | first zero crossing before the first strong extremum of the mean trace | REFLEXW guide |
| Band-pass | Ormsby, corners 0.25 / 1 / 2 / 4 × antenna frequency | Gusmeroli et al. 2015 |
| Gain | energy decay; or (1 + a·t)·exp(b·t); or AGC (25 ns window, an ovid starting value) | REFLEXW manual; GroundVision manual |
| Background removal | mean trace, or moving mean of 50 traces (an ovid starting value); off by default | GroundVision manual |

### Depth, picks and markers

| Value | Formula | Source |
|---|---|---|
| `velocity_m_ns` | v = 0.3 / √εr (m/ns); frozen ground 0.141, thawed wet sand or gravel 0.0775, peat 0.039, silt 0.054, whole active layer 0.045 | Wollschläger et al. 2010; Gusmeroli et al. 2015 |
| `depth_m` | depth = two-way time × v / 2 | Davis and Annan 1989; REFLEXW manual |

The velocities are site values from the papers; measure one on site (hyperbola fit, CMP, or a probe depth to the frost table) where possible. A click on the radargram adds a pick (`config/gpr_picks.csv`: line, trace, distance, two-way time, depth, layer, velocity and the flow version used) or a marker (`config/gpr_markers.csv`; the `.mrk` file stays as it is). **Export picks** writes the picks as a CSV. Every setting and its source is listed in `GS/docs/GPR_references.md`.

## Set-up check, time zones and file audit

These three checks are shared with the other ovid apps and come from the core. Each run writes them to `output/audit/`; the Settings page shows them in the **Set-up check and file audit** card, with a button to run the check again and one to export the file audit.

### Set-up check (setup_check.csv)

Before any file is read, the set-up is checked: raw and project folders exist, every file rule is a valid pattern, survey ids are not repeated, acquired dates parse, transects and projects point at things that exist, site latitude and longitude are in range, electrode elevation tables have `electrode`, `distance_m` and `elevation_m`, and QC profiles name known checks. Each row has a level (`error`, `warn` or `info`). Any error is also written to `problems.csv`, but the run still goes on.

### Time zones (config/timezones.csv)

Instrument clocks are often left on local time. The shared table has one row per rule: `serial`, `model`, `file_pattern`, `start`, `end`, `offset_hours` and `reason`. Blank columns match anything, and the first matching row wins. Each survey gets `utc_offset_h` and `zone_source` (the rule that matched, or `none`).

> **Worked example.** A row with model `Terrameter`, start `2024-04-01`, end `2024-10-31`, offset `-6` and reason "summer field clock". A survey acquired at 2024-07-12 14:30 on that instrument gets `utc_offset_h = -6`, so in UTC it is 2024-07-12 20:30. A survey from November matches no row and keeps `zone_source = none`.

### File audit (file_audit.csv)

One row per raw file found: its path, size, which survey it became, how many measurements it gave, and `what_happened` (read, skipped by a rule, duplicate copy, unreadable and why). The run then checks that every file found is accounted for; a gap is written to `problems.csv`.

## Reading the outputs

```
<project>/config/                 what people decide (CSV; every change backed up in config/history/)
<project>/output/catalogue/       surveys.csv, files.csv, survey_stats.csv, projects/sites/transects.csv, previews/
<project>/output/surveys/<id>/    measurements.csv, extra.csv, electrodes.csv, contact.csv, cmd_geometry.csv, flags.csv
<project>/output/audit/           run_manifest.csv, run_history.csv, problems.csv, setup_check.csv, file_audit.csv
<project>/cache/                  read files and opened zips (safe to delete)
```

## Electrode arrays

Summarised from Loke (1999), section 2.5. Diagrams of each layout are on the app's Reference page.

| Array | Strengths | Weaknesses | Use for |
|---|---|---|---|
| Wenner | Strongest signal; good for horizontal layers | Poor for narrow vertical features; coverage shrinks with depth | Noisy sites, layered ground |
| Dipole-dipole | Good for vertical features; good coverage; low EM coupling (IP) | Signal falls with n cubed; shallower; needs good contact | Horizontal resolution with a sensitive meter |
| Wenner-Schlumberger | Balanced vertical and horizontal sensitivity | Weaker signal than Wenner | When unsure; overlapping levels |
| Pole-dipole | Good coverage; stronger than dipole-dipole | Asymmetric; needs a remote electrode | Few electrodes, forward and reverse |
| Pole-pole | Widest coverage, deepest | Poorest resolution; telluric noise; two remote electrodes | Small spacings |

## When something goes wrong


- If part of a page fails, a notice says so with a short reference (E- and six characters) and the app carries on. The life-ring button collects versions, logs and the set-up into a troubleshooting zip (no raw data) to send with the reference.
- If the output folder is deleted or moved while the app is open, the pages notice within seconds and show what is left; run again to rebuild it.
- A project folder that cannot be written to (read-only, a locked or full drive), or a file where the output, cache or config folder should be, stops the run before anything is changed, with what to do.
- A set-up table that is damaged or was saved by another program with columns missing is shown with those columns added empty and a notice; nothing changes in the file until it is saved.


## Plots and set-up tables

### Plot gestures

The same on the Workbench point chart (pseudosection points); ERT sections and radargrams keep their own toolbar.

- **Drag left to right** zooms in on the dragged stretch (a box on depth, scatter and section plots). **Drag right to left** zooms out, twice as wide each time, up to the whole record. A drag shorter than 6 px is a click.
- **Shift + drag** (or right-drag) selects: a time stretch across all panels, or a box on depth, scatter and section plots. Click selects one record; Shift + click adds one (or drops it, if already selected); Esc clears.
- The wheel or a pinch zooms at the pointer; Shift + wheel or a sideways swipe scrolls. Drag the time axis or the whole-record strip, or hold Space and drag, to move along. Double-click shows the whole record; Backspace returns to the previous view.
- Maps keep map habits: drag moves, click picks one item, Shift + drag draws a selection box.

### Yes/No picks in set-up tables

Columns that hold yes or no (`enabled`, `show`, `identifies`, `include`, `active`, `hidden`) show **Yes** or **No** with a pick, in the Standard sheet and the Workbench grid alike. Typed or imported values are read in any case: yes, y, true, t, 1, on and no, n, false, f, 0, off. They are written `TRUE` or `FALSE`. Any other text is marked as an error, and the table cannot be applied until it is fixed.

## Troubleshooting

| What you see | Why, and what to do |
|---|---|
| A survey is *Incomplete* | Fewer records than the header states: the download was cut. The survey stays in the catalogue. |
| Many *poor contact* flags | Frozen or dry ground. Compare profiles on the QC page; frozen_ground allows 50 kohm A-B. |
| About half of a 3-D IRIS export flagged *duplicate* and *bad geometry* | The export lost the cross-line offsets; the positions repeat along the line. |
| A `.bin` file has *No reader yet* | Export it from Prosys as text, or wait for the IRIS binary reader. |
| The map is blank | Imagery needs an internet connection; transects need start and end points. |
| A change made outside the app does not show | Restart the app; config is read at start. Rebuild the catalogue after adding raw files. |

## Glossary

**Apparent resistivity** the resistivity of a uniform ground that would give the measured V/I with this layout (ohm-m).
**A, B / M, N** current electrodes / potential electrodes. **Geometric factor (k)** converts V/I to apparent resistivity.
**Pseudosection** measurements plotted under their electrodes at a depth set by the array length. **Repeat error** scatter of
the stacked readings. **Reciprocal** the same measurement with current and potential electrodes swapped. **Transect** the
physical line; **survey** one acquisition on it. **Flag** a QC mark on a measurement; nothing is removed.

## References

- Loke, M.H. (1999). *Electrical imaging surveys for environmental and engineering studies.* (ref_manuals/Lokenote.pdf)
- Advanced Geosciences Inc. (2014). *Instruction manual for EarthImager 2D.* (ref_manuals/EarthImager2DManual.pdf)
- Advanced Geosciences Inc. *SuperSting instruction manual.* (ref_manuals/SuperSting Manual.pdf)
- Telford, W. M., Geldart, L. P., Sheriff, R. E. (1990). *Applied Geophysics*, 2nd ed., Ch. 8. Cambridge University Press.
- Loke, M. H. (2004). *Tutorial: 2-D and 3-D electrical imaging surveys*.
- Binley, A., Kemna, A. (2005). DC resistivity and induced polarization methods. In *Hydrogeophysics*, Springer.
- Edwards, L.S. (1977). A modified pseudosection for resistivity and IP. *Geophysics* 42, 1020-1036.
- ResIPy (gitlab.com/hkex/resipy), `parsers.py`: Prosys header styles, remote electrodes, Syscal binary layout.
- Herring, T., et al. (2023). Best practices for using electrical resistivity tomography to investigate permafrost. *Permafrost and Periglacial Processes* 34(4), 494–512. [doi:10.1002/ppp.2207](https://doi.org/10.1002/ppp.2207)
- Canadian Permafrost Electrical Resistivity Survey (CPERS), NSERC PermafrostNet. Data processing notebook. [data.permafrostnet.ca/cpers/data_processing.html](https://data.permafrostnet.ca/cpers/data_processing.html)
- Gusmeroli A., Liu L., Schaefer K.M., Zhang T., Schaefer T., Grosse G. (2015). Active layer stratigraphy and organic layer thickness at a thermokarst site in Arctic Alaska identified using ground penetrating radar. *Arctic, Antarctic, and Alpine Research* 47(2). doi:10.1657/AAAR00C-13-301
- Wollschläger U., Gerhards H., Yu Q., Roth K. (2010). Multi-channel ground-penetrating radar to explore spatial variations in thaw depth and moisture content in the active layer of a permafrost site. *The Cryosphere* 4, 269–283. doi:10.5194/tc-4-269-2010
- Davis J.L., Annan A.P. (1989). Ground-penetrating radar for high-resolution mapping of soil and rock stratigraphy. *Geophysical Prospecting* 37(5), 531–551.
- Sandmeier K.J. *REFLEXW manual*, version 10.3, and *Introduction to the processing of GPR data within REFLEXW*. sandmeier-geo.de
- MALA GeoScience (2009). *GroundVision Software Operating Manual*, v. 1.4.
