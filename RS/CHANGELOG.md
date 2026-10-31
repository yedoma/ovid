# Ovid RS changelog

Newest first. Each entry gives the date, what was added, changed and fixed, and **Output changes**: whether results a user already
has would come out differently, and why. Entries before October 2026 are taken from the git history and are shorter.

## Unreleased

**Faster (2026-10-30; outputs byte-identical, checked on a copy of a 297-flight catalogue)**
- The photo cache keeps the raw tags read from each file and works the fields out on every scan: a change to how fields are derived (camera tables, clocks) no longer re-reads the drive; only a change to reading the files does. A cache of an earlier version is kept as it is (no forced re-read).
- Each flight's footprint is cached (keyed on its photos, heights, LiDAR angle and the footprint code): a rebuild traces only flights whose inputs changed.
- The Simple checks count the listed files from one column (0.06 s, was 1.8 s reading every line).
- The map keeps the footprints and image positions it has been sent; each change of the view sends only new ones (a new catalogue starts afresh).
- With core's faster file audit: rebuild 50 -> 25-29 s; app start to a usable Simple page 6.8 -> 3.5 s; a warm rescan of one year 9 -> 3-4 s.
- **Output changes:** none.

**Fixed (accuracy audit against the raw files, 2026-10-30)**
- A camera clock with no zone (DJI's consumer drones write the phone's local time) was read as UTC: times, dates and sun were 6-7 h off (P4 Pro snowmelt flights showed 05:00-07:00 with the sun at 3°). It is now taken as the project's zone, said in the clock note, and the clock check is "look" until a time-zone row confirms it. Flight ids unchanged.
- Sun elevation by NOAA's solar position equations (was Cooper's declination without the equation of time, up to 4° off).
- Air 2S and Mavic 2 Pro (Hasselblad L1D-20c): aircraft named, standalone GNSS (were "unknown"), Mavic 2 Pro GSD from its 13.2 mm sensor.
- eBee flights: motion blur and overlap from the log's height (were "not known"). Speed of cameras taking several frames a second (thermoMAP 2.2 -> 12 m/s). Hovering steps under 0.5 m are not overlap pairs.
- Coverage counts mapping flights only, with the eBee GSD from the log; an empty coverage table keeps its columns.
- **Output changes:** times, dates and sun of local-clock cameras; sun everywhere (to 0.01°); new flights.csv column clock_zone; blur, overlap and speed where they were missing or wrong; coverage.csv without photo sets; two drone cameras added to the default table.

**Fixed (use-case click-through, 2026-10-30)**
- A resumed scan failed ("Length of 'no' is 0") when flights.csv held a flight the open catalogue had not got yet.
- Confirm or Reject link kept showing "auto" (data.table read the argument state as the column); the reason box clears after each decision.
- Other geodata: Remove on a source's top folder wrote the rule ^[^/]+$, ignoring every top-level file of every source; Remove now ignores exactly the set-aside files and their side files. Keep on one row then Remove on another removed both (the first pick stayed selected).
- Site guess: a camera's numbered folder (DCIM/00039, 100MEDIA) or a band folder (msp) gives its parent's name, a month folder (2020-08_August) none, unless Site names gives that name a site.
- Flight page: GSD from the flight log's height when the photos carry none (it said "not known" while the Map showed 2.86 cm); sun elevation in degrees.
- Footprint colours are a Set-up table.
- Wording: one count of flights and photo sets on Import, View and each source; Simple's times in the project's zone, named; "6, all plugged in"; the save message mentions folder names only when they were saved; zero-flight source removal; report and coverage card name the drone in each block ("2019 · eBee · Optical"), counts in brackets, "Covered by 1 flight"; Needs you filters "Accuracy, QA, Flight log, Site".
- **Output changes:** catalogue sites for numbered, band and month folders change on the next rebuild; nothing else.

**Changed (text and formatting review, 2026-10-30)**
- Wording: "ovid" in lower case (calibration notes, the viewer's title and "made with" line, STAC descriptions, "worked out by ovid"); plurals written out (1 flight, 1 source, 1 thing to look at; the run log's flight(s), source(s), problem(s) ...); help texts name tables in plain words (the flights table, Set-up tables) instead of config file names; the Needs you explanation of accuracy covers positions not known as well as standalone GNSS.
- Tables: plain headings instead of column names (coverage on the Timeline; files not matched, datasets and products on QA; the last run's problems; Needs you in sentence case); empty tables say so (GIS layers, satellite and raster files, GNSS quality, QA tables).
- Formatting: no space before the colon in a flight's checks; placeholders that fit their boxes; the folder box no longer suggests the old /Volumes/Drive.
- Second pass (computed styles at 1280/1440/1920 on every page): Simple tables at 13.5 px with legible headings and counts right-aligned (Images, GSD, Flights); RS notes, legends, map notes and Timeline text on the type scale (12.5 / 11 px); Sources counts at 26 px; STAC description and the core-not-found message say "ovid". Test: the .bb3 key test no longer picks up a key from the user's own RS projects.
- **Output changes:** none to the catalogue; the calibration notes written into a new project say "ovid default".

**Fixed (eBee footprints, 2026-10-30)**
- eBee flights had no footprints (none in the map's one footprint per year either) in a project without the .bb3 key: their logs could not be decoded, so their height above the ground was unknown. The key build during a scan stopped at the first eMotion mission holding an empty geotag ("is.raw(input) is not TRUE"); it now skips such geotags and flights. A project with no key now takes it from another RS project that has one (only read there; the key is one keystream for every log) and keeps a copy in its config. A year with no footprint is left out of the per-year outlines.
- **Output changes:** yes, after a rescan: eBee flights gain their decoded logs, heights and footprints.

**Added (footprint colours by drone, sensor and year, 2026-10-30)**
- The Map, its one-footprint-per-year layers (now one per year, drone family and sensor), the legend, the coverage report, the Simple view's map and the offline viewer colour footprints by drone, sensor and year: each drone family its own pair of muted hues (DJI red and orange, eBee blue and purple with a magenta mix for thermal, others green and teal), optical and its other sensor (LiDAR, NIR, thermal) apart, and each year in view its own shade within the drone's hue band (the hue turned by steps of year_step_deg and lighter or darker in turn), at least 15 dE (CIELAB) from every other year of that drone and sensor and from the other sensor of the same year, all legible on the map. Set in config/footprint_colours.csv (Set-up tables): family, match, colours as palette names, + mixes or #hex. Style, Colour flights by: "Drone, sensor and year" is the default; Positioning and the others stay.
- **Output changes:** none to the catalogue; the map, report and viewer colours change.

**Fixed (thermal flights, 2026-10-30)**
- senseFly thermoMAP frames (THM_*.tif) were read as rasters, so thermal eBee flights never became flights (T7 2022: two thermal flights, 11,667 frames, listed as rasters with no position). They carry senseFly EXIF, GPS and drone XMP, and are now camera images like the Sequoia band TIFFs. Satellite, aerial and LiDAR rasters' headers are also read several at once (core par_map), as photo headers are.
- Raster, LAS and metadata headers are kept in the source's cache (datasets.rds, by path, size and time), as photo headers are: a rescan reads only new or changed files (a 3-source test: 19.5 s -> 2.6 s).
- thermoMAP frames give no pixel count or sensor size: 640 x 512 px and a sensor width from senseFly's 14 cm/px at 75 m and the frames' focal length (10.75 mm with 9 mm) are filled in when the catalogue is built (CAMERA_SPECS; _hub/Derived_values_references.md 14.x). Footprints follow where a flight's height above the ground is known (a linked log or base).
- DJI Air 2S photos (camera FC3411) give no sensor size: 13.2 mm (1-inch, DJI's specs) is filled in when the catalogue is built, so their flights get footprints and a ground sample distance (_hub/Derived_values_references.md 14.x).
- **Output changes:** yes: thermal frames leave datasets.csv and make flights.

**Fixed (found by the stress and click-through test, 2026-10-30)**
- The Simple report said "1 have corrected positions"; and a source with no flights showed "NA flights".
- Keep and Remove on the Map's Other geodata tab saved nothing: the staged choice read the rows' own (blank) decision column.
- Timeline, Keep as a site (and the other buttons over a table) with no row picked failed: x[NULL] dropped the table's columns.
- A page reloaded during a scan showed "Nothing scanned yet" after it: the scan's end now makes every open page read the catalogue again.
- A folder name with accents gave a source id of underscores ("Été site A" -> _t_site_A); now Ete_site_A.
- Export file audit on an empty project gave a failed download; it now downloads a one-line note and says so.
- (core) A new project could be made while a scan was running (the scan's outputs then went to the project left); a cancelled run that had to be ended was reported as "the app or computer stopped".
- **Output changes:** source ids of new folders with accented names.

**Changed (faster scans, 2026-10-30)**
- A first scan reads photo headers several at once (core par_map; one at a time on Windows): reading a header waits on the drive far longer than parsing it (26 ms a photo on a USB drive, 7.5 ms with eight at once). eBee .bb3 logs are decoded several at once (at most four), copies of one log once, and not opened at all while the project has no .bb3 key (nothing could be read from them). A cold scan of a 5,941-file year folder took 66 s instead of 184 s; the catalogue is identical, file for file (cold and warm, and with the key).
- **Output changes:** none (a damaged .bb3 in a project with no key is no longer listed in problems.csv, as it is not opened).

**Added (other geodata set aside, 2026-10-30)**
- Satellite, aerial, LiDAR and raster files and GIS layers are catalogued when they sit beside flight photos (the same folder, or one inside or around it) or lie within spatial_max_km (5 km, an ovid default) of a flight; the rest is set aside in catalogue/set_aside.csv with the reason (no position read, a basemap over spatial_max_extent_km = 200 km across, N km from the nearest flight), never dropped or changed. The Map's Other geodata tab lists the folders set aside; Keep brings one back on the next rebuild. Sources (Standard) and Import (Simple) switch GIS layers and imagery on or off (calibration options geodata_gis_layers, geodata_imagery). Decided when the catalogue is built: no rescan needed. Distances by the equirectangular approximation (Snyder 1987; _hub/Derived_values_references.md 14.x).
- **Output changes:** yes: geodata far from every flight leaves datasets.csv, gis_layers.csv and gis_features.csv for set_aside.csv. The golden test's expected outputs gain set_aside.csv and four calibration rows: run `Rscript tests/test_golden.R accept "<your name>"` after checking them.

**Added (starting a catalogue, 2026-10-30)**
- Simple, Import: a new install first asks to start a new catalogue (a name; the folder is filled in under ~/Projects/Ovid/Projects/RS, Browse to change it) or open one you have; New project in the menu shows the same. Folders are added to a list (Add to list; each with its short name, Remove, and a note when one is inside another) and scanned together with "Scan N folders"; the list is kept in sources.csv, so it survives a reload. The scan's progress (which folder of how many, time, Cancel) shows on Import itself; after it, "View the flights". View, Edit and Export wait until there is a catalogue. Standard, Sources: Add to list and "Scan the new folders" too.
- **Output changes:** none.

**Fixed (starting fresh and every button, 2026-10-30)**
- A new project scans its first folder straight away: the placeholder raw folder /Volumes/Drive is gone, so no "set-up needs attention" question, no "overlaps a source ... inside NA" (an empty list of sources counted every path as inside one) and no fake source written by "Scan anyway"; a folder that is not there is never added as a source. Rescan and Rebuild with no source say "add a drive or folder first". Browse works in Simple. Opening or making another project shows its own catalogue at once (the old one stayed until a reload). The sticky "No catalogue yet" message is gone; "Review the set-up" opens Sources (Standard) or Import (Simple). On an empty project, Report (PDF), Footprints (GeoJSON), file audit, the Timeline's site-name buttons and the Flight page's Save, Confirm and Split say what is missing instead of failing or doing nothing; a scan that ends after the page was reloaded no longer errors. Simple says "1 flight", "1 sensor".
- **Output changes:** none; a project that relied on the placeholder had no data in it.

**Fixed (flight ids and the R version, 2026-10-30)**
- Flight ids and review ids stay the same under R 4.6.2, which had moved them all (core hash_bytes).
- **Output changes:** none; ids are the ones R 4.6.1 gave.

**Fixed (Workbench edit, 2026-10-29)**
- The Workbench's Edit flight saves a positioning change again: the save no longer judged "in the Workbench" from the page address (it dropped the change unless the address said ?ui=workbench). **Output changes:** none.

**Changed (names, 2026-10-29)**
- read_mrk is now read_dji_mrk (DJI geotag files; GS reads MALA GPR markers). **Output changes:** none.

**Docs (citations, 2026-10-29)**
- Reference guide and code comment: sun elevation is within about 4° (Cooper 1969 declination, no equation of time), not about a degree, as the hub's gap 6 says. **Output changes:** none.

**Docs fixes (2026-10-29)**
- Guides checked against the app: reference guide: links confirmed in the Workbench; Export viewer (ZIP) is one button. **Output changes:** none.

**Docs (2026-10-29)**
- New quick start and how-to guide (docs/RS_quick_start.*, docs/RS_how_to.*: md, html and pdf), built by docs/build_guides.R in
  the reference guide's colours, with figures from the test archive (map imagery hidden). **Output changes:** none.

**Fixed (Simple view, 2026-10-29)**
- Import's "Corrected positions" counted corrected photo sets over the flights only ("1 of 0 flights"); it now counts flights
  over flights (sp_rs_corrected). **Output changes:** none.

**Added (the Simple view, 2026-10-28)**
- simple/simple_rs.R: the Simple view (?ui=simple). Import: the sources with a plugged-in dot and their last scan, Scan plugged-in
  drives and add a drive or folder (the Sources page's own scan), and a checklist (files listed, image details read, flights grouped
  with copies counted once, logs and geotags linked, flights with a site, corrected positions). View: filters (by folder guess, by
  year, by site, need a look) and a search, a tile per group (flights, sensors, dates, % corrected; "camera folder" for a folder
  name SIMPLE_GENERIC_FOLDERS in R/config.R lists; a link to QA), the group's footprints on a map with a year switch, its flights
  with the five checks (✓ ! ✕), and one flight's facts (sensor, positions, GSD, sun elevation, copies) with each check's reason.
  Edit: folder names to sites (site_names.csv, with the suggestion of each, camera folders left out), sites (name, place, region;
  a rename through rename.R, RENAMES$site new in R/config.R), which site each flight is at, and a flight's name, project, notes
  and archive status, all through the list of changes; writes sites.csv, flights.csv and site_names.csv only. Export: the
  catalogue report (HTML), the catalogue tables (zip) and the footprints (GeoJSON); the offline viewer stays on Sources.
- Links, file rules, time zones, GNSS bases and calibration options are edited by hand only in the Workbench (wb_only); a flight's
  positioning is saved only from the Workbench (pages/flight.R). ?page= opens a Standard page from a link. app.R sources core
  report.R and rename.R. Tests added.
- **Output changes**: none.

**Changed (Map page type and icons, 2026-10-27)**
- One Inter type scale on the Map page (11, 12.5, 13.5, 14 and 26 px); the map's controls and legend use Inter, not Leaflet's Helvetica.
- Footprint switch: three equal parts, as tall as the inputs, the chosen one in the accent colour.
- Coverage card: each value on one line with its range under it; "Height from" as short counts (DJI, log, log with base), the
  full wording in the row's hover tip and in the report.
- Map legend: year outlines drawn dotted as on the map; the planned / observed / derived key under a rule with line samples
  (also in the PNG and report legend).
- Style, Legend and PNG are one stack of same-size buttons with line icons; fullscreen and layers are the same size.
- Report (PDF) button: a download-file icon. Number tiles: text starts at the top, labels grey.
- **Output changes**: none (the report's numbers and wording are unchanged; its map legend is drawn in Inter).

**Changed (scan cache kept when a run stops, 2026-10-27)**
- cache/sources (image headers, logs and records read from the sources) is kept when a run fails or is stopped (core
  output_restore, CACHE_INPUT_ONLY in rs_pipeline.R), so the next scan reads only what changed instead of every image.
- **Output changes**: none.

**Fixed (scan cache after a stopped run, 2026-10-26)**
- A rescan that reads no new image no longer rewrites cache/sources/<id>/exif.rds, so a run that stops later (core output_restore
  drops the cache files a stopped run wrote) no longer makes the next scan read every image again. Test added.
- **Output changes**: none.

**Fixed (.bb3 logs with records on two clock offsets, 2026-10-26)**
- A .bb3 log whose leftover records each fit two known clock offsets stopped with "object 'j' not found" (three 2017 eBee logs);
  those records are now left unplaced and the rest of the log is read (R/read_emotion.R bb3_locate). Test added.
- **Output changes**: such logs are now read instead of listed as not read.

**Fixed (geotag lists in large archives, 2026-10-26)**
- A geotag, KML or Pix4D list is linked to the flight(s) with the most of its image names, not to every flight sharing a name
  such as DJI_0001.JPG; a build over many sources stopped on this join. Ties keep each flight at 0.6, "as do N other flights".
- **Output changes**: list links in archives with repeated image names now go to the best-matching flight(s) only.

**Added (flight footprints, one footprint per year, coverage report, 2026-10-26)**
- Pipeline: each flight gets a footprint (photo frames from sensor, focal length and height above take-off ground; LiDAR swath
  2·h·tan(FOV/2)) in catalogue/footprints.csv and fp_* columns in flights.csv (R/footprints.R). eBee height comes from the RTK log
  (photo less take-off GNSS height), else the GNSS base ground; read_rinex now returns the ellipsoidal height and antenna height.
- Map: flights are drawn by their footprint (dotted) instead of a box. Filters: Footprint (Optical, LiDAR, Both) and
  One footprint per year, which joins the footprints in view into one outline per year and kind, with a coverage card
  (area, parts, holes, area flown more than once, GSD, height, overlap, speed, sun, RTK share).
- Report (PDF): the current view with legend, scale bar, north arrow and corner coordinates, coverage numbers, flights, method
  and references (chromote; HTML when Chrome is missing). PNG export shares the drawing code (www/map_tools.js).
- **Output changes**: flights.csv gains the fp_* columns and catalogue/footprints.csv is new; the RINEX sample output gains
  rinex_h and rinex_ant_h. Golden and sample outputs need accepting.
- Fixed: eBee take-off height skips the track's heights before the GNSS fix (two 2026 flights read 388 m instead of about 115 m); footprints count each image once, not its copies.

**Changed (folders under ~/Projects/Ovid, 2026-10-21)**
- The ~/Desktop/UAV_project fallback is gone: the project folder is the one set in app_settings.json (on this Mac
  ~/Projects/Ovid/Projects/RS/UAV_project), else ~/Projects/Ovid/Projects/RS/Untitled.
- app.R: RS_PROJECT_DIR wins over the saved project folder, as in CS (the app test sets it).
- Set-up: projects and raw data kept in the old Desktop folders are not found by default any more; open them from their new place.
- **Output changes**: none.

**Changed (finding the core, 2026-10-21)**
- The launcher and pipeline look for the shared core in OVID_CORE, core/ inside the app and ../core beside it only; the old
  ~/Desktop/Ovid fallback (and its Windows Desktop\\Ovid twin) is gone. The launcher's not-found message says to keep the app beside
  core/ or run Make_Portable.bat.
- Set-up: a core kept only at ~/Desktop/Ovid is no longer found; put core/ beside the app or set OVID_CORE.
- **Output changes**: none.

**Fixed (duplicates table, Sources layout, 2026-10-21)**
- An empty catalogue/duplicates.csv kept its key, kept_path and dup_path columns (data.table() had taken key = as its key argument); golden updated.
- Sources page: three column widths for its three cards (bslib warning gone).
- **Output changes**: the header of an empty duplicates.csv.

**Changed (failures reported, not hidden, 2026-10-20)**
- 1 failure now reported (a source's scan.rds); 27 defaults commented as safe. The Sources page shows a notice when the set-up check cannot run.
- **Output changes:** none for readable projects; a part that cannot be read or made is now on the problem list.

**Changed (Workbench grid helpers to core, 2026-10-20)**
- wb_num() and wb_cells() are core's (core/workbench.R). **Output changes:** none.

**Changed (maps before data, 2026-10-20)**
- The flight map and the GNSS map open on the whole world until data are drawn and fitted (they used to open on a fixed region).
- **Output changes:** none.

**Added and fixed (tests: samples, damaged files, golden project, derived values, 2026-10-19)**
- tests/samples: a trimmed, neutralized real file for every format RS reads but eMotion .mis3 (DJI JPG with XMP, SODA JPG, MRK, RPT, eMotion geoinfo, DJI WPML KMZ, RINEX observation header, GeoTIFF), with reference values from the raw files in reference.csv (tests/samples.R).
- tests/test_bad_files.R: every sample emptied, cut in half and cut to 1,000 bytes beside a good flight; photos with no GPS, an MRK that matches no photo, a camera clock in local time, the same photos stored twice.
- tests/test_golden.R and tests/golden: a small DJI RTK flight (JPG heads, MRK, RPT) and a SODA flight; outputs compared with tests/golden/expected, failing until accepted (`Rscript tests/test_golden.R accept "<name>"`).
- tests/test_formulas.R: GSD against DJI's Phantom 4 RTK figure, blur, forward overlap, sun elevation at the solstice noon, checkpoint RMSE and vertical bias (ASPRS Edition 2), fixed share, base check, UTM. run_tests.R runs all three.
- Fixed: an empty file (0 bytes) was catalogued as a dataset or record; it is now listed with the status "empty file" and not catalogued.
- Fixed: DJI's XMP UTCAtExposure is GPS time, not UTC; photo and flight times were 18 s late. The GPS - UTC leap seconds (GPS_UTC_LEAP, R/readers.R; 18 s since 2017) are now taken off, as they already were for MRK times. Flight ids still count from the GPS time, so they and every edit saved against them stay the same.
- Fixed: RINEX observation times (obs_start_utc, obs_end_utc) are GPS time when the header says GPS or states no system (DJI base files), unless the file is GLONASS only; the leap seconds are now taken off.
- tests/test_golden.R compares numbers to 1e-9 of their size (the last bits of a mean or percentile differ between x86_64 and arm64).

**Output changes:** empty files move from the catalogue to the file audit as "empty file". DJI photo times (images.csv time_utc) and the flight times taken from them (flights.csv start_utc, end_utc and what depends on them, such as the sun elevation) are 18 s earlier (17 s for 2015-2016, 16 s for 2012-2015). RINEX files' obs_start_utc and obs_end_utc (logs.csv and the GNSS base checks) are 18 s earlier.

**Changed (toggles, 2026-10-14)**
- The Map and Imagery filters and the Workbench's show-done switch are toggles; Leave out on the flight form stays a checkbox (saved with the form). **Output changes:** none.

**Added and changed (RS backlog: list editing, joins, sites, eBee logs and times, photo sets, other geodata, 2026-10-14)**
- Flights list under the map: select rows, then set site, project, name or positioning (dropdowns of known values, new ones typed), Join into one flight, Move to photo sets or Leave out. Changes wait in one Save bar (Undo, Discard, Save or Cmd/Ctrl+S) and are written to flights.csv at once; leaving with unsaved changes asks first (pages/list_edits.R, pages/fct_edits.R, www/rs_edits.js).
- A flight joined by hand (flights.csv mission_set) is one row in the list and on the map; its sorties stay separate flights in the catalogue and exports.
- Photo sets: a run of fewer than photoset_max_images (calibration table, default 20, no published source) is a photo set, on its own tab, kept out of missions and checked for its images only; flights.csv kind wins.
- Other geodata tab: satellite, aerial, LiDAR, raster and GIS files by folder, with Keep or Remove (an ignore rule in file_rules.csv; kept folders in review_decisions.csv). No position or far from the flights is suggested for removal.
- Site names found card on the Timeline: names read from folder names, with a suggested known site (same name in another case, a name within it, a name several flights use, or a sites.csv site whose coordinates the flights cover). Saved to the new config/site_names.csv (blank site = not a site) and applied by the next rebuild; names kept as sites go into sites.csv.
- Flight page lists the logs no flight has yet from the same aircraft near the flight's time, closest photo-tag count first, with Link this log; the catalogue writes catalogue/logs.csv. The detail panel shows the flight log and KML start times.
- Times: a decoded flight log's GNSS time now wins over eMotion's KML; a camera clock more than 14 h from the log or the KML (the widest UTC offset) is treated as never set and the GNSS time is used.

**Output changes:** in the current archive, 5 Canon S110 NIR flights (351-356) take the log's start time (about a minute earlier than the KML's); the two Sony WX220 flights at the factory date 2013-12-31 move to 2015-08-28 and 2015-08-29; 17 flights become photo sets and leave mission grouping (uncertain groupings fall from 13 to 6). flights.csv gains a kind column.

**Changed (one review list, project menu, time zone, 2026-10-09)**
- The project menu is in the top bar and in the Workbench; Settings lists the projects (moved from Sources); opening a project loads all its settings.
- The Workbench grouping reads "Group flights by" and "No flights yet".

**Changed (screens made simpler, 2026-10-09)**
- Checkpoint accuracy shows once, on the Workbench's Positioning tab; the GNSS page no longer repeats it.
- QA drops its four count boxes and the flagged-flights table (Needs you and the Map's summary cards show them); Sources drops its Group loggers by list (the Workbench keeps one).
- Each QA summary moved to the page that acts on it: coverage by site, year and sensor to the Timeline, positioning status by year to Positioning, missing metadata to Needs you.
- The Flight page's free-text Mission box is gone; missions are set with Confirm this grouping and Split this sortie off.
- The shareable viewer has one Export viewer (ZIP) button that saves the title, writes the viewer and downloads it; the title is saved with the project.
- The Flight page and the Workbench's Edit flight use one flight edit form.
- The Map filter "Only flights with QA flags" is now "Only flights with checks to look at", as counted in the summary and Needs you.
- The Workbench tabs no longer include Reference (it stays under Pages and the ? button), and Import data no longer repeats the header's Rebuild button.

**Fixed (GNSS bases, 2026-10-09)**
- A base whose method is "network (NTRIP / CORS)" is now matched whatever its case, so it is ok instead of always "needs you".

**Fixed (review decisions and set-up notice, 2026-10-09)**
- "Set-up changed since the last build" no longer follows review_decisions.csv (Accept as is), which a build does not read.
- Needs you reads and writes its decisions through the core review helpers.

**Fixed (Workbench, checks, eMotion logs and saves, 2026-10-09)**
- Workbench: Products and STAC export are tabs of their own, showing only the products table, and the STAC catalogue with the viewer; they no longer open the whole QA page.
- Clock check: a time from a decoded flight log counts as a checked clock (ok), not as unusable.
- Placed counts the photo tags of decoded .bbx/.bb3 logs; placed_from has one meaning in flights.csv and in the app ("photo geotags", "flight log" or "none"; `placed_counts()`). **Output changes:** placed_from no longer says "flight log" for a flight whose log places nothing, and such a flight gets its track from its photo geotags.
- A scan builds the .bb3 key by itself when a source holds eMotion missions (.mis3) with their .bb3 logs and the project has no key yet (kept in the project's config folder). Needs you says when a log waits for the key, and what to scan.
- problems.csv is removed after a clean run, so the Workbench no longer shows old problems; the Sources page lists the last run's problems under the set-up check.
- Rebuild and Create viewer run in the background with Cancel, after the set-up check, like scans.
- Checks accepted as is in Needs you count as ok in "Checks to look at" and the Map's Checks column.
- Needs you Fix: the clock item opens the Time zones set-up table and the accuracy item the Positioning tab; Fix opens the flight in the Workbench too.
- flights.csv is written only through its Set-up tables editor (Flight page, Set site, missions, Workbench Edit flight), so unsaved editor edits are not overwritten. Edit flight has the Flight page's Mission and calibration panel fields, and neither form requires a reason. Set site no longer briefly reverts a hand-set positioning status.
- Group by on the Sources page opens a flight in place in the Workbench instead of reloading the page.
- Settings is reachable from the Workbench (Pages).
- The viewer title is saved in the app's settings and titles the STAC catalogue of every rebuild.
- Copies of eMotion flight logs are counted in "duplicates counted once" and in the run message.
- The set-up check looks at gnss_bases.csv (method, post_processed) and drone_cameras.csv (drone yes or no).
- The Workbench's inspector and its first tab agree (Needs you); the Workbench export is named like every other download (ovid_rs_<flight>_<date>.csv).
- README lists the Timeline page and the Workbench tabs; comments name calibration_options.csv.

## 0.6.0 (2026-10-10), needs core 1.15.0

**Changed (drone photos, every photo read, sources, 2026-10-09)**
- Only drone photos make flights: a drone tag (DJI drone-dji, senseFly, Camera: XMP) or a camera in the new Set-up table `config/drone_cameras.csv` (seeded with the cameras RS knew; a row with drone "no", seeded with DJI's handheld Osmo and Pocket cameras, wins over a drone tag). Other photos are listed in `catalogue/other_photos.csv` with why; raw photos are tested the same way when their EXIF can be read.
- Every photo is read, not 30 per folder; the result is cached per file (`cache/sources/<source>/exif.rds`, by path, size and time), so a rescan reads only new or changed photos.
- A flight with no log takes its times, extent and a track from its photos' geotags (`placed_from` "photo geotags" in flights.csv).
- Copies of a photo are counted once and listed in `catalogue/duplicates.csv` (kind, key, kept_path, dup_path, why; shared with the eMotion logs). A source inside another counts each file once, under the innermost source; the Sources page says so before scanning.
- Satellite scenes stay rasters, never photos or flights; a year in a folder name is no longer a flight number.
- Sources page: each source with its status, drone photos and flights, and Remove (with a confirm; `remove_source()`, files never touched); a Last scan summary with "Why left out" and "Duplicates".
- Fixed: the Workbench no longer disconnects on a flight with blank times, nor errs with no colour variable; the map's photo points no longer fail when no photo has a position.
- **Output changes:** yes. Non-drone photos and the flights they made leave the catalogue; flights gain positions, extents and times from every photo instead of a sample; the first scan after this update reads every photo again.

**Added (senseFly logs and eMotion missions, 2026-10-09)**
- RS reads eBee logs (.bbx, and the encrypted .bb3 once the project has its key) and eMotion missions (.mis3, .syn3, .rtk3) without eMotion (R/read_emotion.R, R/emotion_catalogue.R). A flight linked to a log gains the log's time span, a flown track, its photo tags and the mission block and waypoints it flew. The .bb3 key is built once per project from the user's own .mis3 and .bb3 files (build_bb3_ring) and kept in the project's config folder, never in the code. **Output changes:** flights.csv gains log_* and plan_* columns and two QA notes; geometry.csv gains flown tracks, mission areas and routes; a flight with no camera time or image positions takes them from its log.
- read_bb3() also gives each track point its height, roll, pitch, heading, course, airspeed and ground speed, and returns battery (voltage, current, mAh used and remaining) and wind tables. **Output changes:** none in the catalogue; more columns from read_bb3().

**Added (Convert page, 2026-10-09)**
- Convert page in the tool rail: units, decimal degrees and DMS, UTM (shared core conversions). **Output changes:** none.

**Changed (time zone, 2026-10-09)**
- Settings has a Time zone. Flight and workbench labels show it in place of a fixed UTC-7. UTM and XYZ conversions go through core convert.R.

**Output changes:** none unless a different zone is chosen.

**Changed (errors reported, 2026-10-09)**
- If the set-up tables cannot be read for the GNSS base check, this is listed as a problem.

**Output changes:** none.

**Added (map and timeline, 2026-10-09)**
- Map page: summary cards (flights, images, placed, RTK/PPK corrected, checks to look at) and a Checks column with five checks per flight; placed counts the geotag log apart from RTK/PPK; check limits in the shared calibration table (pages/mod_flight_summary.R, pages/fct_checks.R).
- Timeline view and a strip under the map: a row per site, a square per flight, empty years kept; Set site writes only the chosen flights to flights.csv (pages/mod_timeline.R, pages/fct_timeline.R).
- Checkpoint page moved onto the shared import: columns matched by heading, missing columns and non-numbers reported before anything is computed (pages/mod_checkpoint.R, pages/fct_checkpoint.R). **Output changes:** none; checkpoint accuracy unchanged.

**Changed (packages, 2026-10-08)**
- The app and its pipeline no longer install packages when they start: a missing one is named, with how to get it (the launcher installs the versions in renv.lock). **Output changes:** none.

**Changed (code layout, 2026-10-08)**
- app.R from 227 to 157 lines: its helpers moved to pages/fct_outputs.R (reading the outputs), pages/utils_tables.R (theme and table style) and pages/utils_setup.R (set-up table editors), sourced in the same place. Code unchanged line for line. **Output changes:** none.
- app.R split from 569 to 226 lines: each server section is its own file in pages/ (sourced inside the server, same order),
  and the interface is pages/ui.R. Code unchanged line for line. **Output changes:** none.

**Changed (2026-10-08)**
- Errors under their field in the Workbench dialogs; correction buttons say why they are greyed; a Columns picker on every wide table. **Output changes:** none.

**Added (2026-10-08)**
- Group the logger list by serial number, deployment, download or raw file (Sources (by aircraft) and the Workbench tree); moves found in the records in Needs you;
  Rename and Duplicate in the projects menu. **Output changes:** none.

**Changed (2026-10-08)**
- Only the raw folder is needed: with no project folder the app uses ~/Desktop/Ovid_Projects/RS/Untitled; no metadata is needed
  (loggers go by serial). Test: `tests/test_no_metadata.R`. **Output changes:** none.

**Added (2026-10-08)**
- Reference page from the guide (core reference.R). **Output changes:** none.

**Added (core workbench 0.9.0, 2026-10-08)**
- Workbench: every set-up table is a spreadsheet with a formula bar (edit in place, ranges with totals, copy and paste, sort, Find,
  Undo, Redo, Apply). **Output changes:** none.

**Fixed (core, 2026-10-08)**
- With the shared core: an error in a handler shows a notice instead of greying out the app; damaged set-up tables are shown with
  their columns restored; plain messages for a read-only project folder; cache files about ten times smaller.
  **Output changes:** none.


**Added (projects, renames, report layout)**
- Projects list on Sources (core `projects.R`).

**Output changes:** none.

**Added**
- Needs assessment, second batch (medium items), in `R/stac.R`:
  - Frames and datums: every dataset in `datasets.csv` gains `frame` (WGS 84, NAD83, NAD83(CSRS), from the EPSG code or the
    coordinate system's name) and `crs_status`; a file that states no coordinate system is "datum unknown" (QA page).
  - Products (new set-up table `products.csv`): orthomosaics, surface models, point clouds and maps made from the flights,
    with their flights, software and settings, control and check points, accuracy, coordinate system (frame, epoch, height
    type, geoid) and processing level (L1 calibrated, L2 orthomosaic / DSM / DTM / point cloud, L4 index, classification or
    change map; after the FAIR framework for UAV data). `catalogue/products.csv` checks the flights and the file.
  - Coverage (`catalogue/coverage.csv`, QA page): flights by site, year and sensor (RGB, multispectral, LiDAR) with the finest
    ground sample distance and the share positioned with RTK or PPK, and the satellite, aerial and LiDAR list by year.
  - STAC catalogue (`output/stac/`, a download on the QA page): STAC 1.0 with a collection per project of flights, one for
    the imagery and LiDAR list and one for products, an item for each with its box, time, platform, ground sample distance,
    sun elevation, positioning and level (projection, electro-optical and view extensions). Checked with pystac against the
    published schemas.
- Checkpoint accuracy (GNSS page): a product checked against surveyed checkpoints from a CSV or Excel file (columns matched by
  heading), reported as ASPRS Edition 2 (2023) asks: RMSE_H, RMSE_V and RMSE_3D only, the checkpoints' own survey error added,
  a statement, a note with fewer than 30 points, and a check that the checkpoints are twice as accurate as the product.
- LiDAR: each LAS tile's point format, points per m2 of its box, and the ASPRS classes in its first 20,000 points (not for
  LAZ, which is compressed), on the Imagery page and in `datasets.csv`.
- Multispectral flights (MicaSense, Sequoia, P4 Multispectral, Mavic 3M, or images with a band name): `multispectral` and
  `irradiance_sensor` (a DLS or sunshine sensor wrote its readings) in `flights.csv`; the calibration panel and when it was
  imaged can be entered on the Flight page (`panel_id`, `panel_capture`), and a multispectral flight without one gets a QA flag
  (Assmann et al. 2019).
- Paste and Import in every set-up table: rows copied from Excel, or a CSV or Excel file with its columns matched to the
  table's by heading, added to the table or used instead of it.
- Workbench interface (preview), `workbench/` on core `workbench.R`: an additional, more detailed view of the same archive with
  every process of the Standard interface, in RS's accent colours.
  - Every Standard page runs inside the Workbench on the Standard server (`server(embedded = TRUE)`): map, flight, imagery, GNSS,
    QA, sources (scans included) and set-up tables. They are listed under Pages and open as tabs; the flight in view is the one
    the Map and Flight pages show.
  - Data: the tree lists every flight by year. A flight is a spreadsheet with a row per image (time in UTC-7, file, position,
    altitude, height, RTK solution, geotag accuracy, GSD, speed, blur, overlap, sun elevation, exposure, size), its images drawn
    above it at their positions in metres east and north of the flight's middle, to scale, coloured by minutes into the flight,
    altitude, height, accuracy, speed or sun elevation. Motion blur over a pixel and an RTK solution that is not fixed are marked.
    The inspector shows an image's file, camera, time, position and the flight's positioning evidence.
  - Selecting works as in the other Workbenches; a click on the positions selects the nearest image and a shift-drag every one
    in a box.
  - Corrections, through the set-up tables: **Leave out** (`file_rules.csv`, an ignore rule naming the selected files; R or
    Delete; it warns when the flight's first image, which names the flight, is among them) and **Edit flight** (`flights.csv`,
    as the Flight page), and links to the flight's links, the map and the file rules.
  - Import data shows the sources and the set-up check; Rebuild is the Standard rebuild.
- Set-up tables page: every table in config/ (flights, links, sites, file rules, time zones, sources) with the editor shared by
  all the apps (core `editor.R`). File rules, sites and time zones had no editor in the app before.
- Choosing the interface: **Workbench** in the Standard title bar, `?ui=workbench` / `?ui=standard` in the address, else
  `OVID_UI`, else the `ui` setting. Standard is the default.

**Changed**
- A scan asks first when the set-up check finds something that would be ignored or misapplied (Review, Scan anyway, Cancel);
  a scan anyway is recorded in its log. Tables are checked as they are edited (core rules).
- Guardrails (core `run_project()`): see core; a drive that is away is still allowed (its last scan is kept).
- Scans run on the shared runner (core `runner.R`): the progress box at the bottom right and Cancel, which stops at the next
  folder; Rebuild and Scan are turned off while a scan goes.
- Every run leaves a log in `output/logs/`, and the lifebuoy in the top bar saves a troubleshooting zip (core). Tests now also click through every page in a browser.
- Each cached step is keyed on the code it runs (core `code_fingerprint()`, `STEP_HASH`), not on every file: an edit elsewhere no longer redoes it. The first run after this redoes everything once. A rescan reads image headers again only when the image readers change.
- An ignore rule in `file_rules.csv` applies on the next rebuild, from every source's last scan: its files are marked ignored and
  their images and records left out, without reading the source again. Before, a rule applied only when its source was scanned
  again (it still does for a role rule, or for a rule taken out).
- A rebuild works out flight numbers once per folder instead of once per image: 0.65 s instead of 1.2 s for a catalogue of
  10,002 images, with the same output. Scanning was already incremental (a folder's headers are read again only when a file in it
  changed), and a rebuild reads no source.
- The rebuild message says no source was read. The Standard pages are built by `std_pages()`, and both interfaces build their
  editors from one list (`SETUP_TABLES`); the Standard interface looks and works as before.

**Fixed**
- `tests/test_app.R` failed at "saving a flight" since the Mission field was added (the test did not set it); Save now also
  tolerates a field that is not there.
- The STAC catalogue was not written ("! STAC catalogue: character string is not in a standard unambiguous format") when any
  project had no satellite, aerial or LiDAR files. The empty dataset list was one blank row (`ensure_cols(data.table(), ...)`
  makes a row when it adds a column to a table with none), and `as.POSIXct("")` stops rather than giving NA. The empty
  datasets, products and GNSS tables are now empty (`[0]`, as ERT does), and a blank, missing or unreadable time leaves its
  item out. ISO times with T and Z, fractional seconds, slashes, and a year or year-month alone are now read too; a year
  alone (inferred from a folder or file name) gets the whole year as `start_datetime` and `end_datetime`.
- A DJI flight folder scanned as a source of its own (Scan folder on the folder itself) had its RPT, LiDAR and GNSS files
  left unlinked: files at a source's top level were never matched to a flight there. Without the RPT the DJI mission id was
  missing, so the sorties of one survey were not grouped into a mission. Found on real flights (T9 sample: one LiDAR survey of
  4 sorties, and two of 2 each), which now group as they do when the drive is scanned whole.
- A project with no GIS files wrote empty `gis_layers.csv` and `gis_features.csv` (no header) and a warning to every run
  log; those files are now left out.

**Output changes**
- New columns in `flights.csv`, `images.csv` and `datasets.csv`; the next scan of each source reads its image headers again (the reader changed).
- None unless `file_rules.csv` has ignore rules that were added after their source was last scanned: those files are now left
  out by a rebuild as a rescan would.
- `output/stac/` is now written for projects with no satellite, aerial or LiDAR files (before, the export stopped and left
  nothing), and gains items for datasets dated by a year or month alone. In such projects `datasets.csv` loses its blank row,
  and so does `products.csv` when no products are recorded, and the stitched GNSS download when there are no points.
- Sources that are a single DJI flight folder: its RPT, LiDAR and GNSS files are now linked, and `mission_id`, `has_rpt`,
  `fly_time_s` and the mission columns change for those flights on the next rebuild (no rescan needed).



## 0.5.0 (2026-10-06), needs core 1.13.0

**Added**
- Flight quality from the image headers: ground sample distance, speed, motion blur, forward and side overlap and sun elevation,
  per image and per flight (Flight page, `flights.csv`, `images.csv`). Big folders read from a sample of headers get forward overlap
  from speed x time between exposures and no side overlap.
- GNSS quality fields: satellites, PDOP, HDOP, VDOP, samples, age of corrections, baseline, correction type; a quality table per
  file on the GNSS page.
- Sensor widths for cameras whose EXIF has none (including the Zenmuse L2).

**Fixed**
- Leica GSI files read from the command line no longer fail on a helper defined only in the app.

**Output changes**
- `flights.csv` and `images.csv` gain the new columns; stitched GNSS points gain the quality columns.

- The test "LAS tile: bounds, points, date from the name" failed on computers not set to UTC: it compared a date read back as a
  UTC date-time with text read in the local time zone. It now compares the day. The app itself was right.

## 0.4.3 (2026-10-05)
- A scan with no images no longer gains a phantom image row.

## 0.4.2 (2026-10-05)
- Sources with no images; map tiles counted apart; Google Earth tile KMLs skipped.
