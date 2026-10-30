# RS/pages/qa.R  --  QA.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- QA ----
  # positioning status by year and missing metadata are drawn on the Workbench's Positioning and Needs you (workbench/wb_views.R)
  cat_tab <- function(f) { C(); p <- file.path(CAT(), f); if (file.exists(p)) fread(file = p) else data.table() }
  # the catalogue's column names as headings people read (the CSV files keep theirs)
  COL_LABEL <- c(scope = "Scope", site = "Site", year = "Year", kind = "Kind", count = "Flights", images = "Images", finest_gsd_cm = "Finest GSD (cm)",
                 corrected_pct = "RTK or PPK (%)", first = "First", last = "Last", product_id = "Product", file = "File", flights = "Flights", software = "Software",
                 version = "Version", parameters = "Parameters", gcps = "GCPs", checkpoints = "Checkpoints", rmse_h_m = "RMSE horizontal (m)", rmse_v_m = "RMSE vertical (m)",
                 epsg = "EPSG", frame = "Frame", epoch = "Epoch", height_type = "Height", geoid_model = "Geoid model", level = "Level", made = "Made", author = "Author",
                 notes = "Notes", level_name = "Level name", flights_found = "Flights found", file_found = "File found", crs_status = "Datum", rel = "File",
                 platform = "Platform", acquired = "Acquired", crs = "Coordinate system", source_id = "Source", role = "Role")
  labelled <- function(x) { x <- copy(x); k <- intersect(names(x), names(COL_LABEL)); if (length(k)) setnames(x, k, unname(COL_LABEL[k])); x }
  small_dt <- function(x) ovid_dt(labelled(x), selection = "none", page = 10, opts = list(dom = "ftip", language = list(emptyTable = "None yet.")))
  output$coverage <- renderDT(small_dt(cat_tab("coverage.csv")))         # shown on the Timeline page
  output$datum_tbl <- renderDT({ d <- cat_tab("datasets.csv"); if (nrow(d) && "crs_status" %in% names(d)) d <- d[, intersect(c("rel", "platform", "acquired", "epsg", "crs", "frame", "crs_status"), names(d)), with = FALSE][order(crs_status != "datum unknown")]; small_dt(d) })
  # products and the STAC catalogue: on this page and on the Workbench's Products and STAC export tabs (wb_ ids)
  output$products_tbl <- renderDT(small_dt(cat_tab("products.csv")))
  output$wb_products_tbl <- renderDT(small_dt(cat_tab("products.csv")))
  stac_zip <- function(file) { d <- file.path(path.expand(CFG$project_dir), "output", "stac"); validate(need(dir.exists(d), "No STAC catalogue yet: rebuild the catalogue first."))
    zip_dir(d, file) }
  output$stac_dl <- ovid_download("RS", "stac", "zip", stac_zip)
  output$wb_stac_dl <- ovid_download("RS", "stac", "zip", stac_zip)
  output$unassigned <- renderDT({ u <- if (is.null(C())) data.table() else C()$unassigned
    ovid_dt(labelled(u), selection = "none", page = 10, opts = list(dom = "ftip")) })
