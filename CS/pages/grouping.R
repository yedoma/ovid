# CS/pages/grouping.R  --  the logger list grouped by serial number, deployment, download or raw file (core grouping.R).
# Part of the CS server (app.R): sourced inside server() with local = TRUE after setup_flow.R (it uses outs, pd, E, strip_cur, go).
# Display and selection only: a row opens its station on the strip and in Plot and explore over the row's period.

  # safe: an output of the last run, only shown
  cs_gb_files <- reactive({ outs(); f <- tryCatch(fread(file = file.path(pd(), "output", "audit", "file_manifest.csv"), colClasses = "character"), error = function(e) NULL)
    if (is.null(f) || !nrow(f) || !"logger_id" %in% names(f)) NULL else f[nzchar(logger_id), .(serial = logger_id, file = path, first = first_ts, last = last_ts)] })
  cs_gb_deps <- reactive({ s <- tryCatch(as.data.table(E$stations$data()), error = function(e) data.table())   # stations.csv: a logger at a station, from start to end
    if (!nrow(s) || !all(c("logger_id", "station") %in% names(s))) NULL else unique(s[nzchar(station), .(serial = as.character(logger_id), from = as.character(start), to = as.character(end), name = station)]) })
  gb <- group_by_server("gb", cs_gb_files, pd, cs_gb_deps, open = function(r) { s <- cs_gb_deps(); s <- if (!is.null(s)) s[serial == r$serial] else NULL
    st <- if (!is.null(s) && nrow(s)) s$name[1] else r$serial
    strip_cur(st); updateSelectInput(session, "ex_station", selected = st)
    if (!is.na(r$first) && !is.na(r$last)) updateDateRangeInput(session, "ex_dates", start = as.Date(r$first), end = as.Date(r$last)); go("Explore") })
