# GT: load_outputs(), the output tables the pages read. Sourced by app.R.
load_outputs <- function(pd) {
  a <- file.path(pd, "output", "audit"); s <- file.path(pd, "output", "summaries")
  if (!file.exists(file.path(a, "summary.csv"))) return(NULL)
  rd <- function(dir, f) { p <- file.path(dir, f)
    # safe: an output table that cannot be read shows as empty until the next run writes it
    d <- if (file.exists(p) && file.info(p)$size > 0) tryCatch(fread(file = p, na.strings = c("", "NA"), colClasses = list(character = intersect(c("serial", "channel", "sensor", "site", "logger_id"), names(fread(file = p, nrows = 0))))), error = function(e) data.table()) else data.table()
    d }
  # safe: as above
  dd <- tryCatch(readRDS(file.path(s, "daily.rds")), error = function(e) data.table())
  list(files = rd(a, "files.csv"), fa = rd(a, "file_audit.csv"), bundles = rd(a, "bundles.csv"), loggers = rd(a, "loggers.csv"), ss = rd(a, "sites_summary.csv"), health = rd(a, "sensor_health.csv"),
       gaps = rd(a, "gaps.csv"), flags = rd(a, "flags.csv"), between = rd(a, "between_sites.csv"), index = rd(a, "audit_index.csv"), problems = rd(a, "problems.csv"), recon = rd(a, "reconciliation.csv"),
       clock = rd(a, "clock.csv"), events = rd(a, "events.csv"), xchk = rd(a, "export_checks.csv"), battery = rd(a, "battery.csv"), mix = rd(a, "metadata_index.csv"),
       notes = rd(a, "field_notes.csv"), registry = rd(a, "registry.csv"), manifest = rd(a, "run_manifest.csv"), runs = rd(a, "run_history.csv"),
       annual = rd(s, "annual_stats.csv"), alt = rd(s, "active_layer.csv"), ft = rd(s, "freeze_thaw.csv"), zc = rd(s, "zero_curtain.csv"), td = rd(s, "thaw_depth_daily.csv"),
       zaa = rd(s, "zero_annual_amplitude.csv"), soil = rd(s, "soil_annual.csv"), dg = rd(s, "degree_days.csv"), nf = rd(s, "n_factors.csv"), gstate = rd(s, "ground_state.csv"),
       taliks = rd(s, "taliks.csv"), zoff = rd(s, "zero_curtain_offsets.csv"), trends = rd(s, "trends.csv"), thermal = rd(s, "thermal.csv"), est = rd(s, "daily_estimated.csv"), daily = dd, when = file.mtime(file.path(a, "summary.csv")))
}
