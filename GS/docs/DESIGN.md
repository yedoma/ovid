# Ovid Core // GS: design

> **Status (2026-10-05):** the original design, kept for the record. The Shiny app is built (ERT 0.2.0, core 1.11.1) with the
> kanagawa.nvim theme; for how it works now see the [ERT reference guide](GS_reference_guide.md) and the [ERT README](../README.md).

A catalogue, metadata, QA/QC and viewing app for electrical resistivity tomography surveys. It does not invert.
Built like GT and WLR: `gs_pipeline.R` does the work, `R/` holds ERT's own code, the shared `core/` is sourced first,
and a Shiny `app.R` (later) calls the pipeline and edits the set-up tables.

## Rules

1. Raw files are read, never written, moved or renamed. Outputs go to the project folder only.
2. Nothing is deleted. QC and manual decisions are **flags**; views and statistics choose what to hide. Every flag can be undone.
3. Every file found is catalogued, including failed, partial and unreadable surveys. "Unreadable" is a catalogue status, not an omission.
4. The rest of Ovid sees one data model; instrument-specific fields are kept beside it, not thrown away.

## Folders

```
raw archive (any layout, read only)     e.g. ~/Desktop/ERT
<project>/config/        what people decide (CSV, edited in the app or by hand; every change backed up by core/config.R)
<project>/output/catalogue/   the lightweight archive: files, bundles, surveys, transects, sites, projects, stats, QC summary, previews
<project>/output/surveys/<survey_id>/   measurements.csv, extra.csv, electrodes.csv, contact.csv, flags.csv
<project>/output/exports/      cleaned exports with provenance
```

`output/catalogue/` plus the preview images is what gets shared with people who do not have the raw archive.
Plain CSV keeps it open and durable; Parquet can be added later as an extra export.

## Hierarchy and identity

Year > Project > Site > Transect > Survey.

* **Survey** = one acquisition event (one instrument data file). `survey_id = "ERT-" + first 10 hex of the MD5 of the data file`.
  Independent of the filename, stable when files are renamed or moved; byte-identical copies are one survey with several paths.
* **Derived data set**: a file made from a survey by software (AGI EarthImager "sub data set", "trimmed", merged files).
  Catalogued with `kind = derived` and linked to its parent survey(s) when their records match (same time stamps and electrodes).
* **Transect** = the physical line, persistent across years. `transect_id` is assigned in `transects.csv`
  (suggested as `<site>-<name>`); surveys are attached to it in `surveys.csv`. Year comes from the acquisition date.
* Project and site are suggested from the folder path and the file's title on the first run, then belong to the user.

## Files and bundles

Each file is sniffed (content first, extension second) and given a `format` and a `role`:

| role | formats |
|---|---|
| data | AGI `.stg` (instrument), Syscal Prosys ASCII export (`.txt`/`.csv`), Syscal binary `.bin` (later; listed as "needs export" until read) |
| derived | EarthImager `.stg` sub/trimmed sets, Res2DInv `.dat` |
| companion | AGI `.crs` (contact resistance), `.cmd` (command file, geometry), `.trn` (terrain), GPS (`.gpx`, `.csv` with coordinates), SuperSting memory dumps (`*.stg.bin`, `CmdDir.bin` ...) |
| result | EarthImager inversion output (`.out`, `.ini`), images, PDFs |
| notes | `.txt`, `.docx`, photos (`.jpg`, `.png`) |

Zip files are opened, never changed: members are extracted once into `<project>/cache/zip/` and catalogued as
`<zip>!/<member>`, so SuperSting downloads kept as zips (common: `.stg`, `.crs`, `.cmd`, memory dumps) are read like loose files.

A **bundle** is a data file plus its companions: same folder and same stem (`T22.stg` + `T22.crs` + `T22.cmd`), and folder-level
notes and photos. Unrecognised files are listed with `format = unrecognised`.

## Data model

`measurements.csv`, one row per quadrupole (common fields, NA where the instrument does not give them):

```
survey_id, meas_id, source_file, rec_no, datetime (UTC-7, like the other apps; the stated zone kept in meta),
a, b, m, n                       electrode numbers (1..n along the line)
ax, ay, az, bx, ..., nz          coordinates (m, as recorded)
voltage_v, current_a, resistance_ohm, rhoa_ohmm, geom_factor_m,
error_pct, ip_mrad, chargeability_mv_v, stack, contact_ohm (current dipole, as reported), array
```

`extra.csv`, keyed by `survey_id, meas_id`, the instrument's own fields with its own names
(AGI: `cmd_id, hv_v, cycles, meas_time_s, gain, channel, error_permille, ip_windows...`;
Syscal: `el_array, spa1..4, dev, sp_mv, vp_mv, in_ma, mdly, tm, m1..m20, stack, quality...`).

`electrodes.csv`: `survey_id, electrode, x, y, z, source` (from the `.cmd` geometry, a `.trn`/GPS file, or the measurement coordinates).
`contact.csv`: contact resistance readings as recorded (AGI `.crs`).

Survey metadata (catalogue `surveys.csv`): ids, kind, project, site, transect, datetime start/end, manufacturer, model, serial,
firmware, software, source format, files, n_records stated vs read, electrode count and spacing, length, array type,
coordinates available, quality class and reasons, notes.

## Importer interface

Each importer is one entry in `FORMATS` (R/importers.R):

```r
list(format = "agi_stg", type = "agi", role = "data",
     sniff = function(path, head) TRUE/FALSE,          # from the first lines
     read  = function(path) list(meta = list(...), meas = data.table, extra = data.table, electrodes = data.table))
```

Companion readers (`.crs`, `.cmd`, `.trn`) return tables that the bundle step joins to the survey. A reader that fails leaves
the survey in the catalogue with `status = read_error` and the message.

## Notes from ResIPy (gitlab.com/hkex/resipy, src/resipy/parsers.py)

* Prosys exports come in three header styles: old (`Spa.1..4`, `Vp`, `In`, `Dev.`), Prosys II (`xA(m)`, `VMN (mV)`, `IAB (mA)`) and
  Prosys III (`xA (m)`, `Dev. Rho (%)`, `Dev. M`, `y`/`z` columns). All three are read.
* Remote (infinite) electrodes are written as coordinates of +-9999 or more by both makers; Ovid gives them electrode 0.
* AGI IP firmware adds `IP:` fields after the coordinates (ResIPy: total time in field 24, six Vs/Vp windows in fields 25-30).
  They are kept as `ip_1...`; turning them into chargeability waits for a sample file.
* Syscal `.bin` follows a documented IRIS layout (version, `TypeOfSyscal` byte, 1024-byte comment, then per-measurement records;
  ResIPy's `syscalBinRawParser`). The type byte would give the exact Syscal model. To be ported when a sample `.bin` is available.

## Metadata tables (config/, edited by people)

| file | columns (short) |
|---|---|
| `projects.csv` | project, code, year, team, organization, purpose, study_area, notes |
| `sites.csv` | site, project, latitude, longitude, region, environment, access, infrastructure, photos, notes |
| `transects.csv` | transect_id, site, name, start_x/y/z, end_x/y/z, crs, orientation_deg, length_m, spacing_m, n_electrodes, array, method, terrain, ground, notes |
| `surveys.csv` | survey_id, project, site, transect_id, operator, conditions, notes, quality, quality_reasons, quality_notes, exclude_from_archive (never deletes) |
| `qc_profiles.csv` | profile, applies_to, rule, value, flag, enabled, notes |
| `manual_flags.csv` | survey_id, meas_ids (e.g. `12-40;55`), electrode, flag, active, reason, author, date |
| `file_rules.csv` | pattern, action (ignore / data / companion / derived), survey_id, reason |

Values extracted from files fill the catalogue; a non-empty cell in these tables overrides them. The catalogue keeps both.

Quality classes: Good, Usable, Usable with caution, Poor, Unusable, Incomplete, Unknown. Reasons from a fixed list (poor contact,
high contact resistance, low current, high error, cable, instrument, geometry, incomplete acquisition, missing coordinates,
wrong electrode positions, interference, aborted, unknown, other) plus free notes. Survey quality and measurement flags are separate.

## QC profiles

Long table, one row per rule, so profiles are easy to copy and edit:

```
profile,applies_to,rule,value,flag,enabled
standard,all,max_error_pct,5,high error,TRUE
standard,all,min_current_ma,1,low current,TRUE
standard,all,rhoa_range,1;100000,extreme apparent resistivity,TRUE
standard,all,negative_rhoa,,negative apparent resistivity,TRUE
standard,agi,max_contact_ohm,50000,poor contact,TRUE
standard,syscal,min_vp_mv,1,low voltage,TRUE
standard,all,duplicates,,duplicate,TRUE
standard,all,geometry,,bad geometry,TRUE
standard,all,outlier_mad,6,outlier,TRUE
standard,all,reciprocal_pct,5,reciprocal error,TRUE
```

Shipped profiles: `none`, `exploratory`, `standard`, `strict`, `supersting_r8`, `frozen_ground`, `iris_syscal`; users add their own rows.
The limits come from the manuals in `ERT/ref_manuals` (EarthImager 2D 3.5 and 4.1, SuperSting 5.1.6 and 7.3-7.4); each row's notes cite the section.
`frozen_ground` drops the 20 mA current rule and allows 50 kohm A-B contact, for winter and permafrost surveys.
`applies_to` is `all`, an instrument type (`agi`, `syscal`) or a survey_id. QC writes `flags.csv` (survey_id, meas_id, flag, rule, value,
profile); measurements are never edited. Views and statistics take "show: all / passed / flagged" from the chosen profile.

## Statistics (catalogue `survey_stats.csv`, one row per survey and profile)

Counts (measured, retained, flagged by rule, excluded), quantiles and mean/SD of rhoa, resistance, current, voltage, error,
contact resistance and IP; expected (from the `.cmd`) vs acquired measurements; electrode count, spacing, length, elevation range,
coordinate extent, geometry checks. Every statistic tolerates empty or partial surveys.

## Shiny views (later milestones)

Archive browser (filter by year, project, site, transect, instrument, format, quality, operator, location, notes, filename,
coordinates, companions), survey overview, measurement table, pseudosection (all / passed / flagged, hover details), geometry,
map, QC plots, metadata editors, QC profile editor, export.

## Milestones

1. **Data model, catalogue, AGI `.stg` (+ `.crs`, `.cmd`) importer**, tested on the local archive.
2. Syscal importers (Prosys ASCII exports first; binary `.bin` if the format can be read reliably), Res2DInv `.dat`.
3. Statistics and QC profiles, manual flags.
4. Shiny: archive browser, survey overview, table, pseudosection, geometry, QC plots, metadata editing.
5. Map, exports with provenance, archive package for sharing, time-lapse comparison of a transect.
