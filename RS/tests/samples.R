# tests/samples.R  --  How the reader tests read RS's sample files (core testkit.R). Sourced by run_tests.R.
# Most RS readers give one summary per file (a list of facts); the sample test reads it as a one-row table so reference.csv can
# check it. A mission plan gives its plan facts first, then its waypoints, areas and geotags.
SAMPLE_KIND <- function(p) format_of(basename(p))
SAMPLE_READ <- function(p, k) { one <- function(x) as.data.table(lapply(x, function(v) if (length(v) == 1L) v else paste(v, collapse = ";")))
  switch(k, image_jpg = read_image_meta(p), dji_mrk = one(read_mrk(p)), dji_rpt = one(read_rpt(p)), emotion_geoinfo = one(read_geoinfo(p)),
         emotion_plan = { x <- read_emotion_plan(p); list(meta = x$meta, waypoints = x$waypoints, areas = x$areas, geotags = x$geotags) },
         kml = one(read_kml(p)), rinex = one(read_rinex(p)), raster = one(read_geotiff(p)), stop("no sample reader for ", k)) }
SAMPLE_TRIM <- character(); SAMPLE_NO_ROWS <- character()
