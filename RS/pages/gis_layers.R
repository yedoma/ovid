# RS/pages/gis_layers.R  --  GIS layers on the map.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- GIS layers on the map: one copy of each feature (backups repeat layers), coloured by category ----
  GIS_COL <- c(`GCPs and checkpoints` = PAL$gold, `Plots, transects and grids` = PAL$teal, `Boundaries and extents` = PAL$bone,
               `Field sites and trails` = PAL$green, `Other GIS layers` = PAL$gray)
  observe({ x <- C(); g <- if (is.null(x) || is.null(x$gis_features) || !nrow(x$gis_features)) data.table() else x$gis_features
    if (nrow(g)) g <- g[!duplicated(paste(category, type, line))]
    if (over("gis", nrow(g))) g <- g[0]
    session$sendCustomMessage("rsgis", list(items = if (nrow(g)) g[, .(category, type, line, col = unname(GIS_COL[category] %|na|% PAL$gray),
      label = paste0(category, " \u00b7 ", fifelse(is.na(feature) | !nzchar(feature), "", paste0(feature, " \u00b7 ")), source_id, ":", rel))] else list())) })
