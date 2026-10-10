# RS/pages/imagery_datasets.R  --  Satellite, aerial and LiDAR datasets.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- satellite, aerial and LiDAR datasets: a table with filters, footprints on the map ----
  dsets <- reactive({ x <- C(); if (is.null(x) || is.null(x$datasets) || !nrow(x$datasets)) data.table() else x$datasets })
  observe({ d <- dsets(); if (!nrow(d)) return()
    for (k in list(c("d_kind", "kind"), c("d_platform", "platform"), c("d_year", "year")))
      updateSelectInput(session, k[1], choices = sort(unique(na.omit(as.character(d[[k[2]]])))), selected = isolate(input[[k[1]]])) })
  dshown <- reactive({ d <- dsets(); if (!nrow(d)) return(d)
    if (!isTRUE(input$d_sidecars)) d <- d[!(duplicated(paste(folder, footprint)) & !is.na(footprint))]    # one row per footprint unless asked
    if (length(input$d_kind)) d <- d[kind %in% input$d_kind]; if (length(input$d_platform)) d <- d[platform %in% input$d_platform]
    if (length(input$d_year)) d <- d[as.character(year) %in% input$d_year]; d })
  output$d_n <- renderText(sprintf("%s of %s satellite, aerial, LiDAR and raster files", num(nrow(dshown())), num(nrow(dsets()))))
  output$d_table <- renderDT({ d <- dshown(); if (!nrow(d)) return(ovid_dt(data.table()))
    ovid_dt(d[, .(Kind = kind, Platform = platform, Product = product, Acquired = acquired, `Date from` = date_source, `Footprint from` = geometry_from,
                    `GSD m` = round(gsd_m, 2), `Cloud %` = cloud_pct, Points = n_points, `Points per m2` = if ("point_density" %in% names(d)) point_density else NA, Classes = if ("classes" %in% names(d)) classes else NA, CRS = fifelse(is.na(epsg), crs, paste0("EPSG:", epsg)), File = paste0(source_id, ":", rel))], selection = "none", page = 15) })
  DS_COL <- c(satellite = PAL$blue, aerial = PAL$bone, lidar = PAL$green, raster = PAL$gray)
  send_ds <- function(d, fit = FALSE) session$sendCustomMessage("rsdata", list(fit = fit, items = if (nrow(d) && "footprint" %in% names(d) && nrow(d <- d[!is.na(footprint)])) d[, .(kind, line = footprint,
    source = grepl("vendor", geometry_from), col = unname(DS_COL[kind] %|na|% PAL$gray),
    label = paste0(platform %|na|% kind, " \u00b7 ", product %|na|% "", " \u00b7 ", acquired %|na|% "date unknown", " \u00b7 ", file))] else list()))
  observe({ d <- dshown(); n <- if (nrow(d) && "footprint" %in% names(d)) sum(!is.na(d$footprint)) else 0; send_ds(if (over("footprints", n)) d[0] else d) })
  observeEvent(input$d_show, { send_ds(dshown(), fit = TRUE); nav_select("nav", "Map") })

  output$g_layers <- renderDT({ x <- C(); g <- if (is.null(x) || is.null(x$gis_layers) || !nrow(x$gis_layers)) data.table() else x$gis_layers
    if (!nrow(g)) return(ovid_dt(data.table()))
    ovid_dt(g[, .(Kind = category, Type = geom_type, Features = n_features, CRS = crs, Note = detail, File = paste0(source_id, ":", rel))], selection = "none", page = 10) })
