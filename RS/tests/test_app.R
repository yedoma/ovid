# tests/test_app.R  --  Runs the app's server against the catalogue tests/run_tests.R builds, without a browser.
#   Rscript tests/run_tests.R && Rscript tests/test_app.R      (exit status 1 if anything fails)
suppressPackageStartupMessages(library(shiny))
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
td <- file.path(tempdir(), "app"); proj <- file.path(td, "rs_test", "project")           # the synthetic archive and catalogue
Sys.setenv(RS_TEST_DIR = td)
if (system2(file.path(R.home("bin"), "Rscript"), file.path(here, "tests", "run_tests.R"), stdout = FALSE) != 0) stop("tests/run_tests.R failed")
setwd(here); Sys.setenv(RS_PROJECT_DIR = proj)
ae <- new.env(); app <- source(file.path(here, "app.R"), local = ae)$value
ae$SETTINGS_FILE <- file.path(td, "app_settings.json")                                  # the viewer title saved here, not in the app folder
ended <- function(session) { j <- ae$RUNNER$job; if (!is.null(j)) j$proc$wait(120000); session$elapse(1000); !ae$run_busy() }   # core runner.R
fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) fails <<- fails + 1L; cat(if (isTRUE(ok)) "ok   " else "FAIL ", what, "\n", sep = "") }

tg <- htmltools::renderTags(ae$ui)
check(grepl('id="rsmap"', tg$html) && grepl("leaflet.min.js", paste(tg$head, tg$html)) && grepl("rs_map.js", paste(tg$head, tg$html)), "page builds, with the map and Leaflet")
testServer(ae$server, {
  # folder browser (Ovid/files.R; first, so an error further down cannot hide it): open, step into a folder, use it
  src <- dirname(proj); session$setInputs(s_path = src, pick_s_path = 1); check(identical(output$d_fb_sel, paste("Selected:", normalizePath(src))), "browser opens at the typed folder")
  sub <- list.dirs(src, recursive = FALSE)[1]; session$setInputs(d_fb_go = sub); session$setInputs(d_fb_ok = 1)
  check(identical(output$d_fb_sel, paste("Selected:", normalizePath(sub))), "browser steps into a folder")
  f <- fl(); check(nrow(f) == 11, "server loads every flight")
  session$setInputs(f_text = "", f_checks = TRUE); k <- fchk()
  check(nrow(shown()) == sum(ae$checks_open(k)) && all(shown()$flight_id %in% k$flight_id[ae$checks_open(k)]), "the Map's filter keeps the flights with checks to look at")
  session$setInputs(f_checks = FALSE, f_status = "RTK corrected")
  check(all(shown()$positioning_status == "RTK corrected") && nrow(shown()) > 0, "filter by positioning status")
  session$setInputs(f_status = character(), f_text = "lakea")
  check(nrow(shown()) == 1, "search finds a flight")
  id <- shown()$flight_id; session$setInputs(map_pick = id)
  check(identical(sel(), id) && grepl("LakeA|Lake A", as.character(output$detail$html)), "picking a flight on the map shows it beside the map")
  check(grepl("Checks", as.character(output$detail$html)), "the flight beside the map shows its checks")
  session$setInputs(`timeline-pick` = id)
  check(grepl("tl-sq on", output$`timeline-grid`$html) && grepl("Set site", as.character(output$`timeline-flight`$html)) && nzchar(output$`timeline-v_sites`),
        "the Timeline marks the picked flight and offers Set site")
  session$setInputs(e_name = "Lake A RGB", e_project = "Fire", e_site = "Lake A", e_status = "", e_status_note = "", e_notes = "test note", e_exclude = FALSE, e_save = 1)
  cf <- ae$read_cfg(file.path(proj, "config", "flights.csv"))
  check(cf[flight_id == id]$display_name == "Lake A RGB" && fl()[flight_id == id]$name == "Lake A RGB", "saving a flight writes config/flights.csv and shows at once")
  session$setInputs(e_status = "PPK corrected", e_save = 2); session$setInputs(`timeline-site` = "Lake B", `timeline-save` = 1)
  cf <- ae$read_cfg(file.path(proj, "config", "flights.csv"))
  check(cf[flight_id == id]$site == "Lake B" && cf[flight_id == id]$positioning_status == "PPK corrected" && fl()[flight_id == id]$positioning_status == "PPK corrected" &&
        fl()[flight_id == id]$site == "Lake B", "Set site writes through the flights.csv editor and keeps a hand-set status, with no reason asked")
  session$setInputs(e_status = "", e_save = 3)
  l <- lk_cur(); session$setInputs(links_rows_selected = 1, l_reason = "checked by hand", l_confirm = 1)
  lc <- ae$read_cfg(file.path(proj, "config", "links.csv"))
  check(any(lc$file == l$file[1] & lc$state == "confirmed" & lc$reason == "checked by hand"), "confirming a link writes config/links.csv")
  check(!is.null(output$coverage) && !is.null(output$unassigned), "the Timeline's coverage and the QA tables render")
  session$setInputs(x_title = "Test archive", x_make = 1)
  check(ended(session) && file.exists(viewer_zip()) && identical(ae$CFG$viewer_title, "Test archive") && identical(ae$settings_read(ae$SETTINGS_FILE)$viewer_title, "Test archive") &&
        identical(ae$project_settings_read(proj)$viewer_title, "Test archive"),
        "Export viewer writes it in the background, and its title is saved with the project for the STAC catalogue")
  # satellite, aerial and LiDAR files: listed and filtered on the Imagery page
  check(nrow(dsets()) == 4 && grepl("^4 of 4", output$d_n), "Imagery page lists the satellite, aerial and LiDAR files")
  session$setInputs(d_kind = "lidar"); check(nrow(dshown()) == 1 && dshown()$platform == "Airborne LiDAR", "Imagery filter by kind")
  session$setInputs(d_kind = character())
  check(nrow(C()$gis_layers) == 4 && nrow(C()$gis_features) == 5, "GIS layers and their features load for the map")
  # GNSS: Emlid, Leica, DJI CSV and MRK stitched into one table and exported
  gf <- list.files(file.path(here, "tests", "gnss"), full.names = TRUE)
  session$setInputs(g_files = data.frame(name = basename(gf), size = file.size(gf), type = "text/csv", datapath = gf))
  d <- gd(); check(nrow(d) == 6 && setequal(unique(d$device), c("Emlid Reach", "Leica", "DJI RTK")) && sum(nzchar(d$note)) == 2, "GNSS files from Emlid, Leica and DJI stitch into one table")
  session$setInputs(g_files = data.frame(name = basename(gf[1]), size = 1, type = "text/csv", datapath = gf[1]))
  check(nrow(gd()) == 6, "adding the same file again adds no repeated points")
  out <- output$g_dl; check(file.exists(out) && nrow(data.table::fread(out)) == 6, "stitched CSV exports")
  # a scan runs in the background, can be stopped, and the next scan then works
  sd <- file.path(td, "extra"); dir.create(sd, showWarnings = FALSE); file.copy(list.files(src, recursive = TRUE, full.names = TRUE, pattern = "\\.(JPG|jpg)$")[1:2], sd)
  session$setInputs(s_path = sd, s_id = "", s_scan = 1); check(ae$run_busy(), "scan starts in the background")
  session$setInputs(ovid_run_cancel = 1); check(ended(session), "a cancelled scan ends")
  session$setInputs(s_scan = 2); check(ended(session) && any(grepl("extra", ae$load_configs(ae$CFG)$sources$source_id)), "a new scan after a stop finishes")
  check(file.exists(viewer_zip()) && grepl("Test archive", paste(readLines(file.path(sub("\\.zip$", "", viewer_zip()), "index.html"), warn = FALSE), collapse = "")), "viewer written from the app")
})
# a first start with nothing scanned: every page draws (no catalogue, no errors)
# the Workbench (preview), on the Standard server, against the same catalogue
ae$CFG$project_dir <- proj
tw <- htmltools::renderTags(ae$wb_ui(list(QUERY_STRING = "ui=workbench"))); hw <- as.character(tw$html); hs <- as.character(tg$html)
ids <- function(h) regmatches(h, gregexpr(' id="[^"]+"', h))[[1]]
toolw <- vapply(Filter(function(x) inherits(x, "ovid_tool"), ae$std_pages()), `[[`, "", "value")
check(!anyDuplicated(ids(hs)) && !anyDuplicated(ids(hw)) && all(vapply(c(toolw, "wb-data", "wb-import"), function(v) grepl(paste0('data-value="', v, '"'), hw, fixed = TRUE), NA)) &&
      !grepl("Create viewer", hw, fixed = TRUE) && !grepl('id="e_mission"', hs, fixed = TRUE) && !grepl('id="ckp-file"', hs, fixed = TRUE) && !"Reference" %in% vapply(ae$WB_PIPELINE, `[[`, "", "go") &&
      lengths(regmatches(hw, gregexpr("&#9656; Rebuild", hw, fixed = TRUE))) == 1L &&
      all(vapply(c("s_build", "wb_export", "wb_xy", "wb_act_out", "wb_act_edit", "rsmap", "wb_products_tbl", "wb_stac_dl", "wb_x_make", "base_offset_h", paste0(ae$SETUP_TABLES$id, "-tbl")), function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)) &&
      all(vapply(paste0(ae$SETUP_TABLES$id, "-tbl"), function(i) grepl(paste0('id="', i, '"'), hs, fixed = TRUE), NA)) && grepl("ui=workbench", hs, fixed = TRUE) && grepl("leaflet.min.js", paste(tw$head, hw), fixed = TRUE),
      "the Workbench holds every Standard page (the map too), its data view, Import, Rebuild and Export, and no input twice; the Standard has Set-up tables and a way to the Workbench")
both <- function(input, output, session) { std <- ae$server(input, output, session, embedded = TRUE); wb <- ae$wb_server(input, output, session, std) }
testServer(both, {
  fid <- wb$tables()$key[1]; session$setInputs(wb_table = fid); d <- wb$dat(); session$setInputs(wb_params = ae$wb_params(d))
  g <- wb$grid()
  check(!session$isClosed() && identical(wb$E, std$E) && identical(std$sel(), fid) && g$n == nrow(d) && g$n > 1 && identical(g$w$key, seq_len(g$n)),
        "the Workbench runs on the Standard server: a flight's images in the grid, the flight shown on the Standard pages too")
  rr <- ae$wb_ignore_rows(d$rel[2:3], "test"); check(nrow(rr) == 1 && all(grepl(rr$pattern, d$rel[2:3], perl = TRUE)) && !any(grepl(rr$pattern, d$rel[-(2:3)], perl = TRUE)),
        "an ignore rule takes the selected images and nothing else in the folder")
  session$setInputs(wb_grid_sel = list(sig = g$sig, c = list(1), r1 = list(2), r2 = list(3), ar = 2, ac = 1))
  session$setInputs(wb_act_out = 1); session$setInputs(wb_out_reason = "take-off"); session$setInputs(wb_out_do = 1)
  fr <- ae$read_cfg(file.path(proj, "config", "file_rules.csv"))
  check(any(fr$reason == "take-off" & fr$action == "ignore") && all(wb$grid()$states[[1]]$state[2:3] == "pend"), "Leave out writes an ignore rule, shown as saved and not yet applied")
  session$setInputs(s_build = 1); ended(session)
  check(wb$dat()[, .N] == g$n - 2L && !any(d$rel[2:3] %in% wb$dat()$rel), "Rebuild in the Workbench applies it from the last scans")
  ms0 <- ae$read_cfg(file.path(proj, "config", "flights.csv"))[flight_id == fid]$mission_set
  session$setInputs(wb_act_edit = 1); session$setInputs(wb_ed_name = "Renamed in the Workbench", wb_ed_project = "", wb_ed_site = "", wb_ed_status = "PPK corrected", wb_ed_status_note = "",
                                                        wb_ed_notes = "", wb_ed_panel = "P-1", wb_ed_panel_capture = "before", wb_ed_exclude = FALSE)
  session$setInputs(wb_ed_save = 1); cf <- ae$read_cfg(file.path(proj, "config", "flights.csv"))
  check(cf[flight_id == fid]$display_name == "Renamed in the Workbench" && std$C()$flights[flight_id == fid]$name == "Renamed in the Workbench" &&
        identical(cf[flight_id == fid]$mission_set, ms0) && cf[flight_id == fid]$panel_id == "P-1" && cf[flight_id == fid]$positioning_status == "PPK corrected",
        "Edit flight writes flights.csv through the Flight page's form, no reason required, a mission left as set, and shows at once")
  fe <- htmltools::renderTags(ae$rs_flight_form("e_"))$html; fw <- htmltools::renderTags(ae$rs_flight_form("wb_ed_"))$html
  check(identical(gsub('"wb_ed_', '"e_', fw), fe) && !grepl("Mission", fe, fixed = TRUE) && grepl("Reason for changing the status", fe, fixed = TRUE),
        "one flight edit form for the Flight page and the Workbench, without a Mission box")
  check(!is.null(output$wbp_status) && !is.null(output$wbn_missing), "Positioning status by year and Missing metadata draw in the Workbench")
})
ae$CFG$project_dir <- file.path(td, "empty_project")
testServer(ae$server, {
  ok <- tryCatch({ for (o in c("flights", "n_shown", "made", "coverage", "unassigned", "src_list", "src_scan", "s_other", "s_dup", "d_n", "d_table", "g_layers")) output[[o]]; TRUE },
                 error = function(e) { cat(conditionMessage(e), "\n"); FALSE })
  check(ok && is.null(C()) && nrow(fl()) == 0, "first start with no catalogue: every page draws")
})
if (fails) { cat(fails, "test(s) failed\n"); quit(status = 1) } else cat("all app tests passed\n")
