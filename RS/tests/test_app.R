# tests/test_app.R  --  Runs the app's server against the catalogue tests/run_tests.R builds, without a browser.
#   Rscript tests/run_tests.R && Rscript tests/test_app.R      (exit status 1 if anything fails)
suppressPackageStartupMessages(library(shiny))
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
td <- file.path(tempdir(), "app"); proj <- file.path(td, "rs_test", "project")           # the synthetic archive and catalogue
Sys.setenv(RS_TEST_DIR = td)
rt_log <- tempfile("rs_run_tests_", fileext = ".log")                                   # to a file, not a pipe: the run starts app processes of its own
if (system2(file.path(R.home("bin"), "Rscript"), file.path(here, "tests", "run_tests.R"), stdout = rt_log) != 0) {   # stderr as before, to this log
  rt <- readLines(rt_log, warn = FALSE); cat(utils::tail(rt, 10), grep("ignore rule:|not accepted|accepted md5|FAIL|Error", rt, value = TRUE), sep = "\n"); stop("tests/run_tests.R failed") }   # its FAIL lines, so the log says which
setwd(here); Sys.setenv(RS_PROJECT_DIR = proj)
ae <- new.env(); app <- source(file.path(here, "app.R"), local = ae)$value
ae$SETTINGS_FILE <- file.path(td, "app_settings.json")                                  # the viewer title saved here, not in the app folder
ended <- function(session) { j <- ae$RUNNER$job; if (!is.null(j)) j$proc$wait(120000); session$elapse(1000); !ae$run_busy() }   # core runner.R
slash <- function(x) gsub("\\", "/", x, fixed = TRUE)                                              # a path the same whichever separators (Windows)
fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) fails <<- fails + 1L; cat(if (isTRUE(ok)) "ok   " else "FAIL ", what, "\n", sep = "") }

tg <- htmltools::renderTags(ae$ui)
check(grepl('id="rsmap"', tg$html) && grepl("leaflet.min.js", paste(tg$head, tg$html)) && grepl("rs_map.js", paste(tg$head, tg$html)), "page builds, with the map and Leaflet")
testServer(ae$server, {
  # folder browser (Ovid/files.R; first, so an error further down cannot hide it): open, step into a folder, use it
  src <- dirname(proj); session$setInputs(s_path = src, pick_s_path = 1); check(identical(slash(output$d_fb_sel), slash(paste("Selected:", normalizePath(src)))), "browser opens at the typed folder")
  sub <- list.dirs(src, recursive = FALSE)[1]; session$setInputs(d_fb_go = sub); session$setInputs(d_fb_ok = 1)
  check(identical(slash(output$d_fb_sel), slash(paste("Selected:", normalizePath(sub)))), "browser steps into a folder")
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
# ---- Add protects the folder it is adding; Missions link edits are checked first
local({ tree <- function(d) { f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)); c(f, unname(tools::md5sum(f[!dir.exists(f)]))) }
  notes <- character(); ae$showNotification <- function(ui, ...) { notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible() }
  on.exit(rm("showNotification", envir = ae), add = TRUE)
  U <- file.path(td, "add_unreg"); dir.create(U); writeLines("sentinel", file.path(U, "keep.txt")); u0 <- tree(U)
  pa <- file.path(td, "add_linked"); dir.create(pa); file.symlink(U, file.path(pa, "config")); ae$CFG$project_dir <- pa
  testServer(ae$server, { session$setInputs(s_path = U, s_id = "U"); session$setInputs(s_add = 1) })
  check(identical(tree(U), u0) && any(grepl("Not changed", notes)), "Add with config/ linked into the folder being added: refused, the folder's files and hashes unchanged")
  pc <- file.path(td, "miss_copy"); dir.create(pc); file.copy(list.files(proj, full.names = TRUE), pc, recursive = TRUE)
  S <- file.path(td, "miss_arch"); file.rename(file.path(pc, "config"), S); file.symlink(S, file.path(pc, "config"))
  sc <- fread(file = file.path(S, "sources.csv"), colClasses = "character"); sc <- rbind(sc, data.table(source_id = "self", path = normalizePath(S, winslash = "/")), fill = TRUE)
  fwrite(sc, file.path(S, "sources.csv")); s0 <- tree(S); ae$CFG$project_dir <- pc; notes <- character()
  had <- FALSE
  testServer(ae$server, { id <- C()$links$flight_id[1]; session$setInputs(map_pick = id, `timeline-pick` = id); session$flushReact()
    had <<- nrow(lk_cur()) > 0; session$setInputs(links_rows_selected = 1, l_reason = "x", l_confirm = 1) })
  check(had && identical(tree(S), s0) && any(grepl("Not changed", notes)), "Missions Confirm with config/ linked into a saved source: refused, links.csv there keeps its bytes")
})
# ---- the Set-up table editor and the GNSS download write nothing they should not
local({ tree <- function(d) { f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)); c(f, unname(tools::md5sum(f[!dir.exists(f)]))) }
  notes <- character(); ae$showNotification <- function(ui, ...) { notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible() }
  on.exit(rm("showNotification", envir = ae), add = TRUE)
  pe <- file.path(td, "edit_copy"); dir.create(pe); file.copy(list.files(proj, full.names = TRUE), pe, recursive = TRUE)
  Z <- file.path(td, "edit_arch"); file.rename(file.path(pe, "config"), Z); file.symlink(Z, file.path(pe, "config"))
  sc <- fread(file = file.path(Z, "sources.csv"), colClasses = "character"); fwrite(rbind(sc, data.table(source_id = "self", path = normalizePath(Z, winslash = "/")), fill = TRUE), file.path(Z, "sources.csv"))
  z0 <- tree(Z); ae$CFG$project_dir <- pe; f <- file.path(pe, "config", "sites.csv"); if (!file.exists(f)) fwrite(data.table(site = "A"), f); z0 <- tree(Z)
  dirty_after <- NA
  testServer(ae$editServer, args = list(id = "t", path = reactive(f), reload = reactive(0)), {
    session$flushReact(); session$setInputs(tbl_cell_edit = list(row = 1, col = 0, value = "edited")); save(); dirty_after <<- dirty() })
  check(identical(tree(Z), z0) && isTRUE(dirty_after) && any(grepl("Not saved", notes)) && !any(grepl("^Saved", notes)),
        "Set-up table editor with config/ linked into a saved source: refused, the archive keeps its bytes, the edit stays unsaved, no success shown")
  pg <- file.path(td, "gnss_proj"); dir.create(pg); file.copy(list.files(proj, full.names = TRUE), pg, recursive = TRUE); ae$CFG$project_dir <- pg
  g0 <- tree(file.path(pg, "output")); dl <- NULL
  testServer(ae$server, { dl <<- output$g_dl })
  check(!is.null(dl) && identical(tree(file.path(pg, "output")), g0), "the GNSS download writes only the file downloaded: nothing in the project")
})
# ---- a folder named with a trailing space next to one without: Add and Scan take the one typed
if (.Platform$OS.type == "windows") cat("skip  folders named with a trailing space next to one without: Windows drops the space, so the two cannot both exist\n") else
local({ Sys.setenv(OVID_RUN_SYNC = "1"); on.exit(Sys.setenv(OVID_RUN_SYNC = ""))
  A <- file.path(td, "rs_test", "T7"); base <- file.path(td, "spaced"); dir.create(base)
  for (n in c("archive", "archive ")) { d <- file.path(base, n); dir.create(d); file.copy(list.files(A, full.names = TRUE), d, recursive = TRUE)
    writeLines(n, file.path(d, if (endsWith(n, " ")) "with_space.txt" else "without_space.txt")) }
  spaced <- file.path(base, "archive "); exact <- normalizePath(spaced, winslash = "/")
  pa <- file.path(td, "spaced_add"); ae$CFG$project_dir <- pa
  testServer(ae$server, { session$setInputs(s_path = spaced, s_id = "SP"); session$setInputs(s_add = 1) })
  sv <- ae$read_configs(ae$CFG)$sources$path
  check(identical(sv, exact) && endsWith(sv, " "), "Add with a folder whose name ends in a space saves that folder exactly, not its unspaced sibling")
  ps <- file.path(td, "spaced_scan"); ae$CFG$project_dir <- ps
  testServer(ae$server, { session$setInputs(s_path = spaced, s_id = "SP"); session$setInputs(s_scan = 1); ended(session) })
  sc <- readRDS(file.path(ps, "cache", "sources", "SP", "scan.rds")); sv <- ae$read_configs(ae$CFG)$sources$path
  check(identical(sc$root, exact) && "with_space.txt" %in% basename(sc$files$rel) && !"without_space.txt" %in% basename(sc$files$rel) && identical(sv, exact),
        "Scan with that folder reads it (its own file, not the sibling's) and the saved source reloads with the trailing space")
  sf <- file.path(ps, "config", "sources.csv"); b0 <- readLines(sf, warn = FALSE)
  testServer(ae$editServer, args = list(id = "t", path = reactive(sf), reload = reactive(0)), {
    session$flushReact(); j <- which(names(data()) == "label") - 1L; session$setInputs(tbl_cell_edit = list(row = 1, col = j, value = "Spaced archive")); save(quiet = TRUE) })
  sv <- ae$read_configs(ae$CFG)$sources; plugged <- NA; status <- ""
  testServer(ae$server, { session$flushReact(); plugged <<- ae$sp_rs_plugged(C()$sources[source_id == "SP"]); session$setInputs(s_rescan = 1); ended(session) })
  sc <- readRDS(file.path(ps, "cache", "sources", "SP", "scan.rds"))
  check(identical(sv$path, exact) && sv$label == "Spaced archive" && grepl(paste0(basename(exact), ","), paste(readLines(sf, warn = FALSE), collapse = "\n"), fixed = TRUE) &&
          isTRUE(plugged) && identical(sc$root, exact) && "with_space.txt" %in% basename(sc$files$rel),
        "the label changed in the Set-up editor keeps the path byte for byte; Simple shows the source plugged in; a rescan reads the spaced folder, not its sibling")
  pn <- file.path(td, "plain_add"); ae$CFG$project_dir <- pn
  testServer(ae$server, { session$setInputs(s_path = paste0(file.path(base, "archive"), "  "), s_id = "P"); session$setInputs(s_add = 1) })
  check(identical(ae$read_configs(ae$CFG)$sources$path, normalizePath(file.path(base, "archive"), winslash = "/")),
        "an ordinary path pasted with stray spaces (no folder of that exact name) is still found")
})
# ---- the Sources page writes nothing when shown; a refused scan's callback removes nothing; a scan clears only its own staging
local({ Sys.setenv(OVID_RUN_SYNC = "1"); on.exit(Sys.setenv(OVID_RUN_SYNC = ""))
  A <- file.path(td, "rs_test", "T7"); tree <- function(d) { f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)); c(f, unname(tools::md5sum(f[!dir.exists(f)]))) }
  notes <- character(); ae$showNotification <- function(ui, ...) { notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible() }
  on.exit(rm("showNotification", envir = ae), add = TRUE)
  arch <- file.path(td, "guard_page_arch"); dir.create(arch); writeLines("sentinel", file.path(arch, "keep.txt")); t0 <- tree(arch)
  pg <- file.path(td, "guard_page_proj"); dir.create(pg); file.symlink(arch, file.path(pg, "config")); ae$CFG$project_dir <- pg
  testServer(ae$server, { session$flushReact(); for (o in c("src_list", "src_scan")) try(output[[o]], silent = TRUE); session$flushReact() })
  check(identical(tree(arch), t0), "opening Sources with config/ linked into an archive writes nothing there (file list and hashes the same)")
  S <- file.path(td, "guard_cb_src"); dir.create(S); file.copy(list.files(A, full.names = TRUE), S, recursive = TRUE)
  pq <- file.path(td, "guard_cb_proj"); ae$CFG$project_dir <- pq; invisible(capture.output(ae$run_pipeline(ae$CFG, scan = S, source_id = "field")))
  dir.create(file.path(S, "sources", "field"), recursive = TRUE); writeLines("partial", file.path(S, "sources", "field", "scan.rds.part")); p0 <- tools::md5sum(file.path(S, "sources", "field", "scan.rds.part"))
  file.rename(file.path(pq, "cache"), file.path(pq, "cache_real")); file.symlink(S, file.path(pq, "cache")); notes <- character()
  testServer(ae$server, { session$setInputs(s_rescan = 1); ended(session) })
  check(identical(tools::md5sum(file.path(S, "sources", "field", "scan.rds.part")), p0) && any(grepl("failed", notes) & grepl("raw folder", notes)),
        "a scan refused (cache/ linked into a saved source): the completion leaves scan.rds.part in the archive byte for byte, and the refusal is shown")
  ae$link_unlink(file.path(pq, "cache")); file.rename(file.path(pq, "cache_real"), file.path(pq, "cache")); unlink(file.path(S, "sources"), recursive = TRUE)
  sd <- file.path(pq, "cache", "sources", "field"); good <- tools::md5sum(file.path(sd, "scan.rds")); writeLines("stale", file.path(sd, "scan.rds.part"))
  testServer(ae$server, { scan_done("Scanning")(list(status = "cancelled", message = "")) })
  check(file.exists(file.path(sd, "scan.rds.part")) && identical(tools::md5sum(file.path(sd, "scan.rds")), good),
        "a cancelled scan's completion sweeps nothing: the last good scan and the staging file are as they were")
  notes <- character(); testServer(ae$server, { session$setInputs(s_rescan = 1); ended(session) })
  check(!file.exists(file.path(sd, "scan.rds.part")) && file.exists(file.path(sd, "scan.rds")) && any(grepl("done", notes)),
        "the next scan, checked and locked, clears its stale staging file and keeps a good scan")
})
if (fails) { cat(fails, "test(s) failed\n"); quit(status = 1) } else cat("all app tests passed\n")
