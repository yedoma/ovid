# ovid changelog: all apps

The whole bundle at a glance, newest first. Each app and the core have their own `CHANGELOG.md` with the detail.

## Unreleased

**Fixed (top bar, 2026-10-29).** The top bar of every app fits its window down to 1024 px: the corrections chip is cut short, then shown as numbers only, before anything else; the view switch and Settings stay whole.

**Changed (folders under ~/Projects/Ovid, 2026-10-21).** Projects, raw archives and demos moved from the Desktop into ~/Projects/Ovid/Projects/<app> (CS, GS, GT, RS, WLR, demo, test_samples). The default projects folder, the GS and GT raw defaults and RS's project folder follow; the ~/Desktop/UAV_project fallback is gone. **Output changes:** none.

**Changed (finding the core, 2026-10-21).** All five apps find the shared core through OVID_CORE, core/ inside the app or ../core beside it; the old ~/Desktop/Ovid fallback is gone (Ovid now lives in ~/Projects/Ovid), with a core test to keep it out; a core kept only at ~/Desktop/Ovid must move beside the apps or be named in OVID_CORE. The dead "Ovid ERT" Finder alias at the repo root is removed. **Output changes:** none.

**Changed (2026-10-20).** Colours follow the colour scheme: GS map, GPR and pseudosection colours and GT's scan table take the palette of the scheme picked in Settings > Colours; WLR's Nord colours come from core. GS's Greys pseudosection scale runs from the scheme's panel ground to its text; the scientific scales (Spectral, Viridis, GPR Grey and Blue-white-red) stay fixed and map labels on satellite imagery stay white. A core test fails on any other fixed colour. **Output changes:** none in each app's own scheme (GS red and violet as lifted for contrast); other schemes draw in their own colours.

**Changed (2026-10-20).** Read failures that used to be silent are reported: core and every app now put a failed read of a set-up table or a missing report part on the problem list (try_or), the Run buttons say when the set-up check could not run, and every remaining silent default has a comment saying why it is safe. **Output changes:** none for readable projects.

**Changed (2026-10-20).** One say() in core for every app (CS, WLR and GT dropped their copies). Maps with nothing to show open on the whole world instead of a fixed field region (CS station map, GS transect map, RS flight and GNSS maps), and example coordinates in the apps are a neutral place. Helpers the apps had copied are in core once: clock corrections (CS, WLR, GT), the HOBO CSV rows and events (WLR, GT), and set-up and Workbench helpers (CS, WLR, GT, GS, RS). **Output changes:** none for the test projects; a clock correction with an unreadable date or minutes is now listed as a problem in CS and GT, WLR no longer counts shifted readings as dropped, and GT reads a joined HOBO CSV up to its second heading, as WLR does, listing the rest as a problem.

**Changed (2026-10-17).** GS has a new icon (a GPR trace above an ERT section) in both interfaces, browser tabs, platform icons and documentation banners. The other four app icons are unchanged. **Output changes:** none.

**Renamed.** ERT is now GS (geophysical surveys), since it covers GPR as well as resistivity. Projects made as ERT open
unchanged; ERT stays the name of the resistivity side of the app.

## 2026-10-10: first release, v1.0.0 (core 1.15.0, CS 1.11.0, WLR 0.12.0, GT 1.8.0, ERT 0.5.0, RS 0.6.0)

**Renamed.** The bundle is now Ovid (it was Dorsal). Nothing works differently; every name changed with it: the folders `Ovid_Apps` and `~/Desktop/Ovid_Projects`, the launchers (`Ovid_<APP>.bat`), the `OVID_CORE` environment variable, the project marker `.ovid_project` and the log folder `~/.ovid`.

**Across the system.** Only the raw data folder is needed; without a project folder each app uses
`~/Desktop/Ovid_Projects/<APP>/Untitled`. A shared set-up flow and set-up Workbench (spreadsheet tables, corrections over a
period, Needs you, a review queue) with spreadsheet-style tables, foldable and floating side panels, and per-app colour schemes.
Projects can be renamed and duplicated. A Convert page (units, decimal degrees and DMS, UTM) in every app's tool rail, using one
cited set of conversions in the core. The time zone is a setting in every app (default UTC-7), and the UTM zone a WLR setting
(default 10). Errors that used to be swallowed are now reported, and exports may gain `problems.csv`. No site-identifying data is
left in the code, tests, docs or history; reader sample tests fail on a new or changed sample until a person accepts it. Packages
are pinned in `renv.lock`, and every suite runs on each push.

**By app.** CS: Standard home, set-up flow and corrections over a period. WLR: water year from 1 October (a setting), U20L
accuracy by range, before-import checklist and barologger checks. GT: `depth_positive` setting for sensors above the ground; TTOP
and the conductivity ratio removed; empty and unreadable files skipped. ERT: opens on Home, with a project Stack tab and an
ERT | GPR switch; GPR is split into Map, Lines and Processing (flows, picks and markers saved in the project). RS: Map and
Timeline pages, flight summary cards and checks, drone photos only, and senseFly eMotion logs and missions decoded without eMotion.

**What users must do.** Check the time zone (and in WLR the UTM zone) under Settings if the defaults do not fit. In GT, set
*depth positive* if depths are written upward.

**Output changes.** GT `n_factors.csv` loses the four TTOP columns. WLR summaries, hydroperiod, recharge and season plots use
the 1 October water year. RS: non-drone photos leave the catalogue, and flights gain positions from logs. Elsewhere values are
unchanged with the default settings.

## 2026-10-06: core 1.14.0, CS 1.10.0, WLR 0.10.0, GT 1.7.0, ERT 0.4.0 (RS 0.5.0 unchanged)

**Across the system.** Rule tables give every row a lasting `rule_id`, and `flags.csv` in CS, WLR, GT and ERT ends with the same
columns (`rule_id`, `qc_code`, `app`, `run_id`; ERT has no `qc_code`).

**By app.** CS: default spike and step limits for new projects, and a button that fills them into an existing project. WLR: tape
against logger, with deployments that differ by more than 5 cm listed as likely metadata errors to adjust or keep. GT: a GeoPrecision logger read
from its `.f2b`. ERT: rule ids (the moving-median and bad-electrode rules are switched on or off
and tuned on the QC page and saved as a profile).

**What users must do.** Nothing. To use the CS spike and step limits in an existing project, press *Fill in suggested spike and
step limits* on Quality rules. In WLR, review Field visits > Tape against logger.

**Output changes.** Four new columns at the end of `flags.csv`, a `rule_id` column in rule tables, WLR `audit/tape_offsets.csv`.
Values are unchanged in existing projects.

## 2026-10-06: core 1.13.0, CS 1.9.0, WLR 0.9.0, GT 1.6.0, ERT 0.3.0, RS 0.5.0

**Across the system.** A shared quality-check module in the core, used by CS, WLR and GT; WMO quality control codes (BUFR table
0 33 020) in the exports of CS, WLR and GT by default; a fix that lets PDF reports and every test suite run on a Mac without
XQuartz. References for every method added are cited in each app's reference guide.

**By app.** CS: radiation, humidity, icing and rain checks, circular wind averaging, dew point, a step test, gauge uncertainty.
WLR: drift and offset correction (off by default) and a bulk export. GT: WMO codes in exports. ERT: moving-median and bad-electrode
rules (new projects only), electrodes turned off, limits by site and season. RS: flight quality from image headers and GNSS
position quality.

**Time zones.** Defaults are unchanged (UTC-7). WLR and GT can store every record in UTC-6 (Settings, base time zone) and have a setting for files that state no zone; CS can export in UTC-6. When no file states its zone the app asks.

**What users must do.** Nothing to keep working. The next run of each app redoes all loggers (the core changed). To use the new
ERT rules in an existing project, add them to `qc_profiles.csv`; to correct WLR drift, turn it on in Settings or add rows to
`drift_corrections.csv`.

**Output changes.** CS wind direction means change where winds straddle north, and CS, WLR and GT exports gain WMO `_qc`
columns. Everything else is additive.

## 2026-10-05: core 1.12.0, shell 1.1.0, CS 1.8.0, WLR 0.8.0, GT 1.5.0, ERT 0.2.0, RS 0.4.3
- Pull from the station metadata (CS), app icons, TOB3 reading, rerun caches; see each app's changelog.

## 2026-10-05: core 1.11.0
- CS, WLR, GT, ERT and RS on the shared ovid core.
