# Ovid Core // RS: design

> **Status (2026-10-05):** the original design, kept for the record (it predates the UAV to RS rename). The Shiny app is built
> (RS 0.4.3, core 1.11.1) with the accent.vim theme, not Khold; for how it works now see the [RS reference guide](RS_reference_guide.md)
> and the [RS README](../README.md).

The full design is the separate document "Ovid UAV: App Design",
approved 2026-10-04. Built like ERT: `rs_pipeline.R` does the work, `R/` holds RS's own code, the shared `core/` is sourced first,
and a Shiny `app.R` (milestone 2) shows the catalogue and edits the set-up tables.

Changes from the design doc to fit Ovid Core:
- The catalogue is plain CSV in `<project>/output/catalogue/` (not SQLite), and people's corrections are `<project>/config/*.csv`
  edited through Ovid Core's `write_cfg` (with backups), as in the other apps.
- Times are kept in Ovid's base zone (UTC-7) beside the UTC times from the images.

Milestones: 1. DJI inventory, flights, links, positioning (done). 1b. senseFly, sources scanned one after another, shareable viewer (done). 1c. Phantom 3/4/4 RTK, eBee Plus, Sequoia, KML/KMZ, RINEX, BBX, planned/observed/derived geometry (done). 2. Shiny app with flight table and map (planned, observed, derived), Khold theme (done).
3. senseFly readers and RTK/PPK rules. 4. GNSS, RINEX and GIS readers. 5. Provenance review. 6. QA views and exports.

Scope grew on 2026-10-04 to Ovid RS (satellite, aerial, LiDAR, GNSS surveys, GIS layers; Black Metal (Khold) theme). Order agreed:
UAV coverage (1c), then the Shiny app in Khold, then the other datasets. Processing of any kind stays out of scope.
