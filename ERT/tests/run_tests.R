# tests/run_tests.R  --  Builds a small synthetic archive, runs the ERT pipeline on it and checks the results.
# Rscript tests/run_tests.R   (exit status 1 if anything fails)
Sys.setenv(ERT_NO_RUN = "1", OVID_RUN_SYNC = "1")   # app runs happen in this process (core runner.R), so a test sees their result at once
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
source(file.path(here, "ert_pipeline.R"), chdir = TRUE)
tmp <- file.path(tempdir(), "ert_test"); unlink(tmp, TRUE); raw <- file.path(tmp, "raw", "Proj 2024"); dir.create(raw, recursive = TRUE)
fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }

# a 4-row Wenner line (1 m spacing) with settings, a copy of it, and an EarthImager subset of two rows
row <- function(i, a, b, m, n, R, t) sprintf("%4d,USER   ,20240831,%s, %.5E,   2, 50, %.5E,LINE1    , %.5E, 0, 0, %.5E, 0, 0, %.5E, 0, 0, %.5E, 0, 0,Cmd=%d,HV=400,Cyk=2,MTime=1.2,Gain=10,Ch=1",
                                         i, t, R, 2 * pi * (m - a) * R, a, b, m, n, i)
hdr <- c("Advanced Geosciences, Inc. SuperSting R8-IP Resistivity meter. S/N: SS0000001 Type: 3D", "Firmware version: 01.33.73E Survey period: 20240831 Records: 5", "Unit: meter")
L <- c(hdr, row(1, 0, 3, 1, 2, 10, "14:00:00"), row(2, 1, 4, 2, 3, 11, "14:00:10"), row(3, 2, 5, 3, 4, 12, "14:00:20"), row(4, 0, 6, 2, 4, 5, "14:00:30"))
writeLines(L, file.path(raw, "LINE1.stg")); file.copy(file.path(raw, "LINE1.stg"), file.path(raw, "LINE1_copy.stg"))
writeLines(c("Advanced Geosciences Inc. (AGI) Sting/SuperSting measured data (*.stg)   Type: XYZ",
             "A sub data set extracted with AGI EarthImager 2D. Version: 2.4.4 (Build 649). Records: 2", "Unit: meter",
             sub(",Cmd=.*", "", L[4:5])), file.path(raw, "LINE1_01.stg"))
writeLines(c("Advanced Geosciences, Inc. SuperSting R8-IP Resistivity meter. S/N: SS0000001 Type: 3D", "x", "x", "Contact resistance readings (approximate)",
             "RecNo, Vcode, Curr, Res, Date, Time of reading, AddrA, AddrB, Ax, Ay, Az, Bx, By, Bz",
             "   1, 250, 1.0E-02, 1.50000E+03,20240831,13:59:59,   1,   4, 0, 0, 0, 3, 0, 0"), file.path(raw, "LINE1.crs"))
writeLines(c(",El-array,Spa.1,Spa.2,Spa.3,Spa.4,Rho ,Dev., M  ,Sp  ,Vp  ,In  ,Mdly,Stack",
             ",Mixed / non conventional,0.00,2.00,4.00,6.00,1294.20,0.0,0.00,39.7,-604.653,17.613,120,6",
             ",Mixed / non conventional,2.00,4.00,6.00,8.00,1300.00,0.3,1.50,10.0,-600.000,18.000,120,4"), file.path(raw, "dipdip_A.csv"))
dir.create(file.path(raw, "p3"))
writeLines(c("xA (m),xB (m),xM (m),xN (m),Dev. Rho (%),M (mV/V),SP (mV),VMN (mV),IAB (mA),Rho (Ohm.m)",
             "0,99999,2,4,0.5,1.2,3,100,50,62.83", "2,99999,4,6,0.4,1.1,3,90,50,56.55"), file.path(raw, "p3", "pd.csv"))
writeLines("notes", file.path(raw, "field notes.txt")); writeBin(as.raw(0:255), file.path(raw, "old.bin"))
md5_before <- tools::md5sum(list.files(raw, full.names = TRUE))

r <- run_pipeline(list(raw_dirs = dirname(raw), project_dir = file.path(tmp, "project"), incremental = TRUE))
s <- r$surveys
check(identical(unname(md5_before), unname(tools::md5sum(list.files(raw, full.names = TRUE)))), "raw files unchanged")
check(nrow(r$files) == 8, "every file listed")
check(nrow(s) == 5 && TRUE, "surveys: one with a copy, one derived, two Syscal exports, one unreadable .bin")
o <- s[kind == "original" & format == "agi_stg"]
check(nrow(o) == 1 && o$n_read == 4 && grepl("LINE1_copy", o$other_paths), "byte-identical copy is one survey")
check(o$status == "incomplete", "fewer records than the header states: incomplete")
check(s[format == "syscal_bin"]$status == "needs_reader", "unreadable file stays in the catalogue")
check(o$model == "AGI SuperSting R8-IP" && o$serial == "SS0000001", "instrument and serial from the header")
check(s[kind == "derived"]$parent_ids == o$survey_id, "EarthImager subset linked to its parent")
m <- r$meas[[o$survey_id]]
check(all(m$array[1:3] == "wenner") && identical(m$a[1:3], 1:3), "electrode numbers and array type")
check(all(abs(m$geom_factor_m * m$resistance_ohm - m$rhoa_ohmm) < 1e-3 * m$rhoa_ohmm), "geometric factor reproduces rhoa")
check(m$contact_ohm[1] == 1500 && is.na(m$contact_ohm[4]), "contact resistance joined by time and electrodes")
check(abs(m$error_pct[1] - 0.2) < 1e-9 && abs(m$current_a[1] - 0.05) < 1e-9, "units: per mille to percent, mA to A")
check("cmd_id" %in% names(r$extra[[o$survey_id]]), "instrument fields kept")
y <- s[format == "syscal_prosys_txt" & grepl("dipdip", primary_file)]; ym <- r$meas[[y$survey_id]]
check(y$model == "IRIS Instruments (model unknown)" && y$n_read == 2, "Syscal export read, model unknown")
check(abs(ym$resistance_ohm[1] - (-604.653 / 17.613)) < 1e-9 && ym$stack[2] == 4 && ym$error_pct[2] == 0.3 && identical(ym$a, c(1L, 2L)), "Syscal fields: R = Vp/In, stack, Dev., electrodes")
check(all(c("Sp", "Mdly") %in% names(r$extra[[y$survey_id]])), "Syscal own columns kept")
pd <- r$meas[[s[grepl("pd.csv", primary_file)]$survey_id]]
check(all(pd$b == 0) && all(pd$array == "pole-dipole") && all(is.finite(pd$geom_factor_m)) && pd$error_pct[1] == 0.5 && pd$resistance_ohm[1] == 2,
      "Prosys III headers and a remote electrode (99999)")
el <- electrodes_of(apply_elevations(m, data.table(survey_id = o$survey_id, electrode = "", distance_m = c("0", "6"), elevation_m = c("100", "106"), source = "t", author = "")))
check(isTRUE(all.equal(el$z, 100 + el$x)), "elevations by distance interpolated to every electrode")
check(file.exists(file.path(tmp, "project", "config", "surveys.csv")), "surveys.csv seeded for metadata")
# moving median, bad electrode, electrodes turned off, rules by site and season
mk_line <- function(n = 24) { q <- data.table(a = 1:(n - 3), m = 2:(n - 2), n = 3:(n - 1), b = 4:n)          # Wenner-like, one depth level
  q[, `:=`(meas_id = .I, ax = a, bx = b, mx = m, nx = n, ay = 0, by = 0, my = 0, ny = 0, az = 0, bz = 0, mz = 0, nz = 0, rhoa_ohmm = 100 * (1 + 0.01 * sin(.I)), contact_ohm = 1000)]; q }
q <- mk_line(); q[10, rhoa_ohmm := 150]
check(identical(QC_RULES$moving_median(q, c(5, 7)), 10L), "moving median flags only the measurement 50 % off its neighbours")
check(length(QC_RULES$moving_median(q, c(5, 60))) == 0L, "and nothing with a 60 % limit")
qm <- mk_line(200); qm[5, rhoa_ohmm := 1e7]
check(identical(QC_RULES$magnitude_sd(qm, 9), 5L) && !length(QC_RULES$magnitude_sd(mk_line(200), 9)), "the CPERS magnitude filter flags a value more than 9 SD from the mean, and nothing in a smooth line")
cp <- default_qc()[profile == "cpers"]
check(setequal(cp$rule, c("negative_rhoa", "max_error_pct", "duplicates", "geometry", "magnitude_sd", "moving_median", "bad_electrode")) && cp[rule == "bad_electrode", value] == "20;10" &&
      cp[rule == "magnitude_sd", value] == "9", "the cpers profile holds the Herring et al. (2023) filter sequence")
pr <- data.table(profile = "p", applies_to = "all", rule = c("rhoa_range", "bad_electrode"), value = c("1;120", "40;3"), flag = c("extreme", "bad electrode"), enabled = "TRUE", notes = "")
q2 <- mk_line(); q2[a == 7 | b == 7 | m == 7 | n == 7, rhoa_ohmm := 500]              # electrode 7 spoils every measurement it is in
f <- qc_survey(q2, "S1", "agi", pr, "p", data.table(survey_id = character(), meas_ids = character(), electrode = character(), flag = character(), active = character(), reason = character()))
check(any(f$rule == "bad_electrode") && grepl("electrode 7 ", f[rule == "bad_electrode", value][1]), "an electrode with most of its measurements flagged is named bad")
fr <- qc_survey(q2, "S1", "agi", qc_rule_ids(pr, "Q"), "p", data.table(survey_id = character(), meas_ids = character(), electrode = character(), flag = character(), active = character(), reason = character()))
check(all(fr[rule == "rhoa_range", rule_id] == "Q-01") && all(fr[rule == "bad_electrode", rule_id] == "Q-02") && tail(names(fr), 1) == "rule_id" && all(grepl("^Q-[0-9]+$", default_qc()$rule_id)),
      "each flag names the row of qc_profiles.csv that raised it (rule_id)")
st <- data.table(survey_id = "S1", transect_id = "", electrode = "12", status = "off", from = "", to = "", reason = "cable cut", author = "t")
f2 <- qc_survey(mk_line(), "S1", "agi", pr[rule == "rhoa_range"], "p", data.table(survey_id = character(), meas_ids = character(), electrode = character(), flag = character(), active = character(), reason = character()), NULL, st)
check(all(mk_line()[a == 12 | b == 12 | m == 12 | n == 12, meas_id] %in% f2[rule == "electrode_off", meas_id]) && all(f2[rule == "electrode_off", flag] == "electrode turned off"), "measurements with an electrode turned off are flagged")
check({ e <- electrodes_with_status(electrodes_of(mk_line()), st, "S1"); e[electrode == 12, status] == "off" && e[electrode == 11, status] == "on" && !"status" %in% names(electrodes_with_status(electrodes_of(mk_line()), st, "S9")) }, "the electrodes table marks the electrode off, and only for that survey")
st2 <- data.table(survey_id = "", transect_id = "T1", electrode = "5-6", status = "off", from = "2020-01-01", to = "2020-12-31", reason = "", author = "")
check(identical(electrodes_off(st2, "S2", list(transect_id = "T1", acquired_date = "2020-06-01")), 5:6) && !length(electrodes_off(st2, "S3", list(transect_id = "T1", acquired_date = "2021-06-01"))), "an electrode turned off on a transect for a period")
pc <- data.table(profile = "p", applies_to = c("agi", "site:Upland", "months:11-4"), rule = c("max_contact_ohm", "max_contact_ohm", "max_contact_ohm"), value = c("10000", "200000", "50000"), flag = "poor contact", enabled = "TRUE", notes = "")
check(qc_rows(pc, "p", "S1", "agi", list(site = "Upland", acquired_date = "2020-01-15"))$value == "200000" && qc_rows(pc, "p", "S1", "agi", list(site = "Bog", acquired_date = "2020-01-15"))$value == "50000" &&
      qc_rows(pc, "p", "S1", "agi", list(site = "Bog", acquired_date = "2020-07-15"))$value == "10000", "a limit by site wins over one by season, which wins over one by instrument")
qi <- data.table(meas_id = 1:4, chargeability_mv_v = c(2, -0.5, NA, 1), ip_mrad = c(NA, NA, -3, 4))
check(identical(QC_RULES$negative_ip(qi, numeric()), 2:3) && all(c("electrode_contact", "inversion_software", "inversion_parameters", "inversion_rms_pct") %in% SURVEY_COLS),
      "negative chargeability or phase is flagged; surveys keep the reporting fields (Herring et al. 2023)")
# a rebuild redoes only the surveys an edit can change, and gives what a rebuild from scratch gives
p0 <- file.path(tmp, "project"); cfg0 <- list(raw_dirs = dirname(raw), project_dir = p0, incremental = TRUE)
mf <- function(p) { m <- fread(file.path(p, "output", "audit", "run_manifest.csv"), colClasses = "character"); function(k) m[key == k, value] }
outs <- function(p) { f <- list.files(file.path(p, "output", c("catalogue", "surveys")), recursive = TRUE, full.names = TRUE, pattern = "\\.(csv|png)$")
  stats::setNames(lapply(f, function(x) if (grepl("png$", x)) unname(tools::md5sum(x)) else { d <- fread(x, colClasses = "character", na.strings = NULL); if ("run_id" %in% names(d)) d[, run_id := NULL]; d }), sub(".*/output/", "", f)) }
o1 <- outs(p0); invisible(capture.output(run_pipeline(cfg0))); v <- mf(p0)
check(v("surveys_reused") == v("surveys_checked") && as.integer(v("previews_reused")) == length(list.files(file.path(p0, "output", "catalogue", "previews"))) && identical(outs(p0), o1), "a rebuild with nothing changed reuses every survey's checks and preview, and writes the same catalogue")
fwrite(data.table(survey_id = o$survey_id, meas_ids = "2", electrode = "", flag = "manually excluded", active = "TRUE", reason = "t", author = "t", date = ""), cfg_path(p0, "manual"))
invisible(capture.output(run_pipeline(cfg0))); v <- mf(p0)
p1 <- file.path(tmp, "full"); unlink(p1, TRUE); dir.create(p1); file.copy(file.path(p0, "config"), p1, recursive = TRUE)
invisible(capture.output(run_pipeline(modifyList(cfg0, list(project_dir = p1, incremental = FALSE)))))
check(as.integer(v("surveys_reused")) == as.integer(v("surveys_checked")) - 1L && identical(outs(p0), outs(p1)), "a manual flag redoes that survey only, as a rebuild from scratch would")
sv <- read_cfg(cfg_path(p0, "surveys")); sv[survey_id == o$survey_id, site := "Upland"]; fwrite(sv, cfg_path(p0, "surveys"))
qp <- read_cfg(cfg_path(p0, "qc")); fwrite(rbind(qp, data.table(profile = CFG$qc_profiles[1], applies_to = "site:Upland", rule = "rhoa_range", value = "1;5", flag = "extreme apparent resistivity",
                                                             enabled = "TRUE", notes = "site rule", rule_id = ""), fill = TRUE), cfg_path(p0, "qc"))
invisible(capture.output(run_pipeline(cfg0))); fs <- fread(file.path(p0, "output", "surveys", o$survey_id, "flags.csv"), colClasses = "character")
check(nrow(fs[profile == CFG$qc_profiles[1] & rule == "rhoa_range" & value == "1;5"]) == 4L, "a rule for a site, set on the survey, applies in the catalogue's checks as on the QC page")
check(identical(run_profiles(list(qc_profiles = c("a", "b")), c("b", "custom", "none", "live", "")), c("a", "b", "custom")) &&
      identical(run_profiles(list(qc_profiles = c("a", "b"), catalogue_profile = "custom"), c("a", "b", "custom"))[1], "custom") &&
      identical(run_profiles(list(qc_profiles = c("a", "b"), catalogue_profile = "gone"), c("a", "b"))[1], "a"),
      "a run checks every profile in the table; the one picked on Settings comes first, else the first built-in")
local({ p3 <- file.path(tmp, "custom"); unlink(p3, TRUE); dir.create(p3); file.copy(file.path(p0, "config"), p3, recursive = TRUE)
  qp <- read_cfg(cfg_path(p3, "qc")); fwrite(rbind(qp, qp[profile == "strict"][, `:=`(profile = "custom", rule_id = "")]), cfg_path(p3, "qc"))
  invisible(capture.output(run_pipeline(modifyList(cfg0, list(project_dir = p3, catalogue_profile = "custom")))))
  fs <- fread(file.path(p3, "output", "surveys", o$survey_id, "flags.csv"), colClasses = "character"); cs <- fread(file.path(p3, "output", "catalogue", "surveys.csv"))
  check(all(c("custom", "cpers", CFG$qc_profiles) %in% fs$profile) && all(cs[n_read > 0]$qc_profile == "custom"),
        "a profile saved on the QC page and cpers are checked at a scan, and the catalogue reports the profile picked on Settings") })
local({ p2 <- file.path(tmp, "noqc"); unlink(p2, TRUE); dir.create(p2); file.copy(file.path(p0, "config"), p2, recursive = TRUE)
  fwrite(read_cfg(cfg_path(p2, "qc"))[0], cfg_path(p2, "qc"))
  r <- tryCatch({ invisible(capture.output(run_pipeline(modifyList(cfg0, list(project_dir = p2))))); "ok" }, error = function(e) conditionMessage(e))
  check(r == "ok" && any(grepl("no rules", fread(file.path(p2, "output", "audit", "problems.csv"))$problem)), "an emptied QC profiles table falls back to the built-in profiles, and says so") })

# ---- the Workbench (preview), on the Standard server: in a copy of the app folder with its own settings, so the real ones are not read
say("Workbench interface (preview)")
appd <- file.path(tmp, "app"); dir.create(appd, showWarnings = FALSE)
for (f in c("app.R", "ert_pipeline.R", "R", "pages", "workbench", "docs")) if (file.exists(file.path(here, f))) file.symlink(file.path(here, f), file.path(appd, f))
saveRDS(list(raw_dirs = dirname(raw), project_dir = p0), file.path(appd, "app_settings.rds"))
Sys.setenv(OVID_CORE = CORE_DIR)
app <- new.env(); assign("shinyApp", function(...) invisible(NULL), envir = app)
local({ old <- setwd(appd); on.exit(setwd(old)); suppressWarnings(suppressMessages(sys.source("app.R", envir = app, keep.source = FALSE))) })
check(all(app$SETUP_TABLES$id %in% names(CONFIG_FILES)) && !anyDuplicated(app$SETUP_TABLES$id) && setequal(names(app$WB_OPEN), app$SETUP_TABLES$id) &&
      any(grepl("setup_editors(", deparse(app$server), fixed = TRUE)), "both interfaces build their table editors from one list, and the Workbench opens each on Set-up tables")
hs <- as.character(htmltools::renderTags(app$ui)$html); hw <- as.character(suppressWarnings(htmltools::renderTags(app$wb_ui(list(QUERY_STRING = "ui=workbench")))$html))
ids <- function(h) regmatches(h, gregexpr(' id="[^"]+"', h))[[1]]
toolw <- vapply(Filter(function(x) inherits(x, "ovid_tool"), app$std_pages(FALSE)), `[[`, "", "value")
check(!anyDuplicated(ids(hs)) && !anyDuplicated(ids(hw)) && all(vapply(c(toolw, "wb-data", "wb-import"), function(v) grepl(paste0('data-value="', v, '"'), hw, fixed = TRUE), NA)) &&
      all(vapply(c("rebuild", "wb_export", "wb_xy", "wb_act_flag", "wb_act_unflag", "set_raw", "set_project", paste0(app$SETUP_TABLES[(tab)]$id, "-tbl")), function(i) grepl(paste0('id="', i, '"'), hw, fixed = TRUE), NA)) &&
      all(vapply(paste0(app$SETUP_TABLES[(tab)]$id, "-tbl"), function(i) grepl(paste0('id="', i, '"'), hs, fixed = TRUE), NA)) && grepl('href="?ui=standard"', hw, fixed = TRUE) && grepl("ui=workbench", hs, fixed = TRUE),
      "the Workbench holds every Standard page, its data view, Import, Rebuild and Export, and no input twice; the Standard has Set-up tables and a way to the Workbench")
gone <- c("wb_act_el", "electrodes-tbl", "wbs_save", "t_group", "elev_gnss", "v_flag", "v_reset", "v_size", "m_inv_sw", "m_inv_par", "m_inv_rms", "dl_qc",
          "stack-tr", "stack-tr_go", "stack-cmp_on", "stack-cmp_1", "stack-cmp_2", "home-found", "home-open_survey", "home-open_stack", "gpr_line", "gpr_preset", "gpr_dc", "gprwb-line")
check(!any(vapply(gone, function(i) grepl(paste0(' id="', i, '"'), hs, fixed = TRUE) || grepl(paste0(' id="', i, '"'), hw, fixed = TRUE), NA)) &&
      all(vapply(c("gprflow-line", "gprflow-undo", "gprflow-all", "gprflow-before", "gprflow-x", "gprflow-topo", "gprflow-clip", "gprflow-maxd", "gprflow-marks_csv", "stack-cmp", "stack-cmp_kept",
                   "dl_profile", "gpr_dirs", "gpr_table", "eo_scope"), function(i) grepl(paste0(' id="', i, '"'), hs, fixed = TRUE), NA)) &&
      grepl(' id="gprflow-ascan"', hs, fixed = TRUE) && !grepl(' id="gprflow-ascan"', hw, fixed = TRUE) && grepl(' id="gprflow-line"', hw, fixed = TRUE) && grepl(' id="wbs_compare"', hw, fixed = TRUE) && grepl(' id="wbs_kept"', hw, fixed = TRUE) &&
      !grepl('<select id="m_transect"', hs, fixed = TRUE) && sum(gregexpr("Export view (PNG)", hs, fixed = TRUE)[[1]] > 0) == 1L && !grepl("Export view with legend", hs, fixed = TRUE) && !grepl('card-header">Folders<', hs, fixed = TRUE),
      "the removed and folded controls are gone from both interfaces; GPR processing is one screen in both (its A-scan in the Workbench's inspector), Compare one control")
tools <- vapply(Filter(function(x) inherits(x, "ovid_tool"), app$std_pages()), `[[`, "", "value")
check(all(unlist(app$ERT_AREAS) %in% tools) && !length(intersect(app$ERT_AREAS$ERT, app$ERT_AREAS$GPR)) && identical(app$ERT_AREAS$GPR, c("GPR", "GPR processing")) &&
      !"Reference" %in% unlist(app$ERT_AREAS) && grepl('data-area="GPR" title="GPR (Alt-2)"', hs, fixed = TRUE) && grepl('class="wb-switch d-areas"', hw, fixed = TRUE) &&
      identical(app$WB_AREAS$GPR, c("GPR", "GPR processing")) && app$WB_AREAS$ERT[1] == "wb-stack" && !"GPR processing" %in% app$WB_AREAS$ERT,
      "the ERT | GPR switch: every side's page exists and is on one side only; Reference on both; GPR processing on the GPR side of both interfaces")
check(!app$SETUP_TABLES[id == "electrodes"]$tab && all(app$SETUP_TABLES[id != "electrodes"]$tab) && identical(app$WB_OPEN$electrodes$go, "QC") &&
      identical(app$wb_panels_read(list("flags", "x")), "flags") && identical(app$wb_panels_read(NULL), c("pseudo", "contact", "flags")),
      "electrode_status.csv has no Set-up tables tab (the QC page writes it); the Workbench's panels come back from the settings, never from config/wb_stack_view.csv")
check(identical(app$wb_ids_text(c(31, 5:20, 33, 34, 5)), "5-20,31,33-34"), "measurement numbers are written as runs")
es <- rbind(app$electrode_status_row("S", "T1", "3, 4", "", "", "x", "me"), app$electrode_status_row("S", "T1", "7", "2024-08-01", "2024-08-31", "y", "me", "transect"),
            app$electrode_status_row("S", "T1", "9", "2025-01-01", "", "z", "me", "transect"), app$electrode_status_row("R", "", "2", "", "", "w", "me"))
eh <- app$electrode_rows_for(es, "S", list(transect_id = "T1", acquired_date = "2024-08-15"))
check(identical(es$electrode[1:2], c("3,4", "7")) && es$survey_id[2] == "" && es$transect_id[2] == "T1" && es$from[1] == "" && nrow(eh) == 2L &&
      identical(eh$applies_to, c("this survey", "transect T1, 2024-08-01 to 2024-08-31")) && identical(electrodes_off(es, "S", list(transect_id = "T1", acquired_date = "2024-08-15")), c(3L, 4L, 7L)),
      "the QC page turns electrodes off in a survey, or on its transect over a period, and lists the rows that apply to the survey")
dd <- data.table(meas_id = 1:6, a = 1:6, b = 4:9, m = 2:7, n = 3:8)
mm <- data.table(survey_id = c("S", "S", "S", "T"), meas_ids = c("1-4", "5", "", "1"), electrode = c("", "", "9", ""), flag = "x", active = c("TRUE", "TRUE", "TRUE", "TRUE"), reason = "", author = "", date = "")
u <- app$wb_unflag(mm, "S", c(2, 5), dd)
check(identical(u$table$meas_ids, c("1,3-4", "", "1")) && identical(u$table$electrode, c("", "9", "")) && nrow(u$before) == 2, "Unflag takes the numbers out of a survey's rows, and deletes a row left empty")
u2 <- app$wb_unflag(mm, "S", 6, dd)
check(nrow(u2$table) == 3 && !"9" %in% u2$table$electrode, "and deletes a row by electrode when a selected measurement uses that electrode")
if (!requireNamespace("xml2", quietly = TRUE)) say("skip  the Workbench server checks need the xml2 package") else {
  ui_defaults <- function(html) {         # the value a browser sends for each input when the page opens
    x <- xml2::read_html(paste0("<html><body>", html, "</body></html>")); a <- xml2::xml_attr; f <- xml2::xml_find_all; out <- list()
    for (n in f(x, "//input[@id][not(@type) or @type='text' or @type='number']")) { v <- a(n, "value"); if (is.na(v)) v <- ""; if (identical(a(n, "type"), "number")) v <- suppressWarnings(as.numeric(v)); out[[a(n, "id")]] <- v }
    for (n in f(x, "//textarea[@id]")) out[[a(n, "id")]] <- xml2::xml_text(n)
    for (n in f(x, "//input[@type='checkbox'][@id]")) out[[a(n, "id")]] <- !is.na(a(n, "checked"))
    for (n in f(x, "//div[contains(@class,'shiny-input-radiogroup')][@id]")) { v <- a(f(n, ".//input[@checked]"), "value"); if (length(v)) out[[a(n, "id")]] <- v[1] }
    for (n in f(x, "//select[@id]")) { o <- f(n, ".//option[@selected]"); if (!length(o)) o <- f(n, ".//option"); v <- a(o, "value"); out[[a(n, "id")]] <- if (length(v)) v[1] else "" }
    out[!grepl("^(f_|m_date)", names(out))] }
  dvw <- ui_defaults(hw); sid <- o$survey_id; rd <- function(w) read_cfg(cfg_path(p0, w))
  selg <- function(g, c, r1, r2 = r1) list(sig = g$sig, c = as.list(c), r1 = as.list(r1), r2 = as.list(r2), ar = r1[1], ac = c[1])
  both <- function(input, output, session) { std <- app$server(input, output, session, embedded = TRUE); wb <- app$wb_server(input, output, session, std) }
  testServer(both, {
    suppressWarnings({ do.call(session$setInputs, dvw); session$flushReact() })
    session$setInputs(wb_table = sid); d <- app$wb_load(sid); session$setInputs(wb_params = app$wb_params(d))
    g <- wb$grid(); st <- function(p, r) wb$grid()$states[[p]]$state[r]
    check(!session$isClosed() && identical(wb$E, std$E) && g$n == 4 && all(g$states[["rho_a (ohm-m)"]]$state == "flag") && st("A", 2) == "flag" && st("A", 1) == "",
          "the Workbench runs on the Standard server: a survey's measurements in the grid, the site rule's flags on rho_a and the manual flag on the whole measurement")
    session$setInputs(wb_grid_sel = selg(g, 1, 1))
    ins <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(output$wb_inspector$html)))
    check(grepl("Read from LINE1.stg", ins, fixed = TRUE) && grepl("A1 B4 M2 N3", ins, fixed = TRUE) && grepl("Flagged: extreme apparent resistivity", ins, fixed = TRUE),
          "a selected measurement shows its file, its quadrupole and what flagged it")
    session$setInputs(wb_grid_sel = selg(g, 1, 3)); session$setInputs(wb_act_flag = 1); session$setInputs(wb_fl_reason = "spike", wb_fl_author = "WB", wb_fl_do = 1)
    m <- rd("manual")[reason == "spike"]
    check(identical(names(rd("manual")), MANUAL_COLS) && nrow(m) == 1 && m$meas_ids == "3" && m$survey_id == sid && st("A", 3) == "pend" && app$cfg_mtime(p0) > app$run_mtime(p0),
          "Flag writes a manual_flags.csv row for the measurement, shown as saved and not yet applied")
    invisible(capture.output(session$setInputs(rebuild = 1)))
    check(st("A", 3) == "flag" && identical(app$run_redone(p0), paste0(" Redone: 1 of ", mf(p0)("surveys_checked"), " surveys; the rest was unchanged and reused.")),
          "Rebuild in the Workbench is the Standard rebuild, and it checks that survey only")
    session$setInputs(wb_grid_sel = selg(wb$grid(), 1, 2)); session$setInputs(wb_act_unflag = 1); session$setInputs(wb_un_do = 1)
    check(!any(rd("manual")$meas_ids == "2") && st("A", 2) == "back", "Unflag deletes the manual flag, and the measurement is marked to come back")
    session$setInputs(qc_survey = sid, eo_scope = "survey", eo_list = "1", eo_reason = "cable cut"); session$setInputs(eo_add = 1)
    e <- rd("electrodes")[reason == "cable cut"]
    check(nrow(e) == 1 && e$survey_id == sid && e$electrode == "1" && e$status == "off" && st("A", 1) == "pend" && st("B", 1) == "",
          "Mark off on the QC page writes an electrode_status.csv row, and the Workbench shows the measurements using it as flagged after the next scan")
  })
}
# error model, inversion files, electrode heights from a GNSS file
set.seed(3); nq <- 200; qa <- seq_len(nq) * 5L; qd <- data.table(a = qa, b = qa + 3L, m = qa + 1L, n = qa + 2L)
qd <- rbind(qd, qd[, .(a = m, b = n, m = a, n = b)])                                   # every quadrupole again as its reciprocal
Rt <- 10^runif(nq, -1, 2); qd[, resistance_ohm := c(Rt, Rt) * (1 + c(rep(0, nq), rnorm(nq, 0, 1) * (0.002 + 0.01 / Rt)))]
emd <- error_model(qd)
check(emd$pairs == nq && emd$model %in% c("linear", "power") && emd$r2 > 0.5 && all(emd$err > 0) && emd$err[which.min(Rt)] > emd$err[which.max(Rt)],
      "error model: fitted to reciprocal pairs, the relative error falls as the resistance grows")
check(identical(error_model(m)$model, "fixed") && all(error_model(m)$err == 0.04), "error model: without reciprocals, 4 % relative")
idir <- file.path(tmp, "inv"); fl0 <- data.table(meas_id = m$meas_id[4], flag = "high error", rule = "max_error_pct")
inversion_files(list(survey_id = o$survey_id), apply_elevations(m, data.table(survey_id = o$survey_id, electrode = "", distance_m = c("0", "6"), elevation_m = c("100", "106"), source = "t", author = "")), fl0, idir)
ohm <- readLines(file.path(idir, paste0(safe_name(o$survey_id), ".ohm"))); r2d <- read_res2dinv(file.path(idir, paste0(safe_name(o$survey_id), "_res2dinv.dat")))
mf <- jsonlite::fromJSON(file.path(idir, "manifest.json")); pr <- readLines(file.path(idir, "protocol.dat"))
check(as.integer(sub("#.*", "", ohm[1])) == nrow(electrodes_of(m)) && any(grepl("^3# number of data", ohm)) && pr[1] == "3" && mf$left_out == 1 &&
      nrow(r2d$meas) == 3 && all(abs(r2d$meas$rhoa_ohmm - m$rhoa_ohmm[1:3]) < 1e-3 * m$rhoa_ohmm[1:3]) && any(grepl("^Topography", readLines(file.path(idir, paste0(safe_name(o$survey_id), "_res2dinv.dat"))))),
      "inversion files: pyGIMLi, R2 and Res2DInv (read back), flagged data left out and listed, topography from the heights")
gf <- file.path(tmp, "gnss.csv"); fwrite(data.table(Name = c("T1-E1", "T1-E2", "T1-E3", "base"), Easting = 1:4, Northing = 1:4, Elevation = c(101.2, 101.4, 101.1, 99)), gf)
ge <- gnss_elevations(gf, "S1")
check(identical(ge$electrode, c("1", "2", "3")) && identical(ge$elevation_m, c("101.2", "101.4", "101.1")) && all(names(ge) == ELEV_COLS), "electrode heights from a GNSS file, by the number in each point's name")
et <- file.path(tmp, "elev.csv"); fwrite(data.table(Electrode = 1:2, Elevation = c(100, 101)), et)
g1 <- elevation_file_rows(gf, "S1", "field.csv"); t1 <- elevation_file_rows(et, "S1", "levels.csv")
check(g1$kind == "gnss" && identical(g1$rows$elevation_m, ge$elevation_m) && grepl("GNSS file field.csv", g1$rows$source[1], fixed = TRUE) &&
      t1$kind == "table" && identical(t1$rows$electrode, c("1", "2")) && t1$rows$source[1] == "levels.csv" && all(names(t1$rows) == ELEV_COLS),
      "Survey > Geometry's one file input tells an elevation table from a GNSS file by its columns")
check(grepl("^Run finished", utils::tail(readLines(sort(list.files(file.path(file.path(tmp, "project"), "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]), 1)), "every run leaves a log in output/logs that says it finished")
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE); source(file.path(here, "tests", "samples.R"), local = TRUE)
  r <- check_reader_samples(file.path(here, "tests", "samples"), SAMPLE_READ); new <- r[is.na(matches_expected), file]
  if (length(new)) cat("  (expected results saved for", length(new), "new sample(s): check them in", file.path("tests", "samples", "expected"), ")\n")
  for (f in r[!(accepted), file]) cat("  ", f, ": not accepted. Check tests/samples/expected/", f, ".rds, then accept_sample(\"tests/samples\", \"", f, "\", \"<your name>\")\n", sep = "")
  for (f in r[reference_ok %in% FALSE, file]) cat("  ", f, ": a value in tests/samples/reference.csv differs from what the reader makes\n", sep = "")
  check(nrow(r) > 0 && all(r$rerun_identical) && !any(r$matches_expected %in% FALSE) && all(r$rows > 0) && all(r$accepted) && !any(r$reference_ok %in% FALSE),
        "every sample survey file reads, the same twice and the same as its expected result, accepted, and equal to its hand-checked reference values") })
check(!length(core_shadows(app_code_files(PIPELINE_DIR))), "no app function replaces a core function of the same name")
check(identical(STEP_HASH, step_hashes(globalenv())) && all(nchar(unlist(STEP_HASH)) == 12), "each cached step has its own code fingerprint, the same when computed again")
local({                                                               # damaged input (core testkit.R): runs that must not stop
  source(file.path(CORE_DIR, "testkit.R"), local = TRUE); sd <- file.path(here, "tests", "samples")
  if (!length(list.files(sd, "__"))) { cat("  (no sample files in tests/samples: damaged-input runs skipped)\n"); return(invisible()) }
  rn <- function(raw, p) invisible(capture.output(run_pipeline(modifyList(CFG, list(raw_dirs = raw, project_dir = p)))))
  d <- damaged_file_runs(sd, rn); bad <- d[result != "ok"]; if (nrow(bad)) print(bad)
  check(!nrow(bad), paste0("a damaged raw file (", paste(unique(d$damage), collapse = ", "), ") never stops a run: ", nrow(d), " runs"))
  raw0 <- tempfile("raw0"); dir.create(raw0); for (f in list.files(sd, "__", full.names = TRUE)) if (!dir.exists(f)) file.copy(f, file.path(raw0, sub("^.*?__", "", basename(f))))
  p0 <- tempfile("p0"); rn(raw0, p0); t <- damaged_table_runs(p0, function() rn(raw0, p0)); bad <- t[result != "ok" & !startsWith(result, "The set-up table")]; if (nrow(bad)) print(bad)
  check(!nrow(bad), paste0("a damaged or emptied set-up table never stops a run without saying which table: ", nrow(t), " runs")) })
local({                                                               # the Archive page's filters (pages/mod_archive.R), without the app
  e <- new.env(); for (x in parse(file.path(here, "pages", "mod_archive.R"))) if (is.call(x) && identical(as.character(x[[2]]), "archive_filter")) eval(x, e)
  s <- data.table(kind = c("original", "merged", "copy", "original"), year = c(2019L, 2021L, 2021L, NA), folder = c("a", "b", "b", "a"),
                  project = c("P1", "", "P1", "P2"), transect_id = c("T1", "", "", "T1"), instrument = "SAS4000", quality = c("good", "poor", "good", "good"),
                  name = c("Line 1", "Line 2", "Line 2 copy", "Pond"), primary_file = "", notes = c("", "", "", "frozen"), site = "")
  f <- function(...) e$archive_filter(s, list(...))$name
  check(identical(f(), c("Line 1", "Line 2", "Pond")) && identical(f(f_derived = TRUE), s$name),
        "the Archive shows original and merged surveys, and copies only when asked")
  check(identical(f(f_year = "2021"), "Line 2") && identical(f(f_project = "(none)"), "Line 2") && identical(f(f_transect = "T1", f_quality = "good"), c("Line 1", "Pond")) &&
        identical(f(f_text = "FROZEN"), "Pond") && identical(f(f_folder = "a", f_text = ""), c("Line 1", "Pond")),
        "the Archive filters by year, project or transect (none for blank), quality, folder and text")
})
local({                                                               # long tables (core/longtable.R): 100,000 rows, a screen at a time
  source(file.path(CORE_DIR, "longtable.R"), local = TRUE)
  big <- data.table(id = seq_len(1e5), x = round(stats::runif(1e5), 3), note = sample(c("a", "b", ""), 1e5, TRUE))
  t0 <- Sys.time(); w <- ovid_long_table(big, selection = "single"); dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  o <- w$x$options
  check(dt < 10 && isTRUE(o$deferRender) && identical(o$scroller$rowHeight, 28L) && nzchar(o$scrollY) && !grepl("p", o$dom) &&
        any(vapply(w$dependencies, function(d) grepl("Scroller", d$name, ignore.case = TRUE), NA)),
        "a long table of 100,000 rows: rows of 28 px drawn a screen at a time, no pages, a fixed heading")
})
local({                                                               # the Workbench's Stack and Edit data figures (pages/fct_workbench.R)
  source(file.path(here, "pages", "fct_workbench.R"), local = TRUE); source(file.path(here, "R", "views.R"), local = TRUE)
  q <- rbindlist(lapply(1:2, function(s) rbindlist(lapply(1:(8 - 3 * s), function(i) data.table(a = i, m = i + s, n = i + 2 * s, b = i + 3 * s)))))   # Wenner, 8 electrodes 1 m apart
  d <- q[, .(meas_id = .I, a, b, m, n, ax = a - 1, bx = b - 1, mx = m - 1, nx = n - 1, ay = 0, by = 0, my = 0, ny = 0, rhoa_ohmm = 100 + .I, contact_ohm = c(1500, 6000)[1 + (.I %% 2)])]
  ed <- electrode_dist(d); lv <- data_levels(d)
  check(isTRUE(all.equal(ed$electrode, 1:8)) && isTRUE(all.equal(ed$dist, 0:7)) && identical(sort(unique(lv$level_no)), 1:2) && min(lv[level_no == 1]$pz) > max(lv[level_no == 2]$pz),
        "ERT workbench: electrodes at their distance along the line; one data level per pseudo-depth and dipole lengths, shallowest first")
  ct <- data.table(addr_a = c(1, 1, 2), addr_b = c(4, 4, 5), contact_ohm = c(1000, 3000, 12000))
  ec <- electrode_contact(ct, ed); ef <- electrode_flags(d, c(1L, 2L), ed)
  check(ec[electrode == 1]$median_ohm == 2000 && ec[electrode == 5]$median_ohm == 12000 && ec[electrode == 4]$n == 2 && is.null(electrode_contact(NULL, ed)) &&
        ef[electrode == 1]$used == 2 && ef[electrode == 2]$flagged == 2 && ef[electrode == 2]$used == 3,
        "ERT workbench: contact per electrode is the median of the A-B readings that use it; flags per electrode count the measurements using it")
  fl <- data.table(meas_id = c(1L, 1L, 2L), rule = c("max_error_pct", "manual", "max_error_pct"), flag = "x", rule_id = c("Q1", "M", "Q1"))
  rc <- rule_counts(fl, nrow(d))
  check(identical(contact_cutoff(d, 5000), d$meas_id[d$contact_ohm > 5000]) && rc[rule == "max_error_pct"]$n == 2 && sum(rc$n) == 3,
        "ERT workbench: a contact cutoff picks the readings above it; rules counted overlapping, as EarthImager does")
  f <- wb_stack_figure(d, 1L, ec, ef, NULL, c("pseudo", "contact", "flags"))
  check(inherits(f, "plotly") && grepl("<svg", quad_svg(d[1], ed)) && is.null(wb_stack_figure(d, 1L, ec, ef, NULL, character())),
        "ERT workbench: the Stack panels draw on one axis, and a measurement's four electrodes draw on the line")
})
local({                                                               # the Home and Stack figures (pages/fct_stack.R), without the app
  source(file.path(here, "pages", "fct_stack.R"), local = TRUE)
  s <- copy(r$surveys)[, `:=`(name = survey_id, date = substr(as.character(datetime_start), 1, 10), folder = "Proj 2024", project = "", transect_id = "")]
  f <- survey_figures(s, r$stats)
  o <- s[kind %in% c("original", "merged")]
  check(nrow(f) == nrow(o) && all(f$project == "Proj 2024") && isTRUE(all.equal(f$kept_pct, as.numeric(o$pct_retained[match(f$survey_id, o$survey_id)]))),
        "Home and Stack: one row per original survey, in its folder's project until one is set, with the kept % of the catalogue")
  st <- r$stats[variable == "error_pct" & set == "all"]
  k <- f[!is.na(error_pct)][1]
  check(nrow(k) == 0 || isTRUE(all.equal(k$error_pct, st[survey_id == k$survey_id & profile == k$qc_profile]$median[1])),
        "the repeat error shown is the median of the survey's own QC profile")
  tr <- data.table(transect_id = "T1", start_lat = "60.1", start_lon = "-120.1")
  s2 <- copy(s)[1, `:=`(project = "P1", transect_id = "T1")]
  f2 <- survey_figures(s2, r$stats, tr)
  check(f2[survey_id == s2$survey_id[1]]$project == "P1" && f2[survey_id == s2$survey_id[1]]$placed && !any(f2[survey_id != s2$survey_id[1]]$placed),
        "a project people set wins over the folder; a survey is placed only when its transect has a start point")
  a <- data.table(name = "A", layout = "48 electrodes at 1 m, wenner", kept_pct = 99, contact_ohm = 1700)
  b <- copy(a)[, name := "B"]; c2 <- copy(a)[, `:=`(name = "C", kept_pct = 2.8, contact_ohm = 19500)]; d2 <- copy(a)[, `:=`(name = "D", layout = "24 electrodes at 2 m")]
  check(stack_comparable(a, b)$ok && !stack_comparable(a, c2)$ok && grepl("C: 2.8 % kept; contact 19.5 kohm", stack_comparable(a, c2)$why) &&
        !stack_comparable(a, d2)$ok && grepl("different electrode layouts", stack_comparable(a, d2)$why[1]) && stack_comparable(a, c2, min_kept = 2)$ok,
        "Compare: only the same electrode layout with both above the kept limit (editable); otherwise it says why")
  m <- data.table(a = 1:4, b = 2:5, m = 3:6, n = 4:7, rhoa_ohmm = c(100, 200, 50, 80), ax = 0:3, bx = 1:4, mx = 2:5, nx = 3:6, ay = 0, by = 0, my = 0, ny = 0)
  m2 <- copy(m)[4:1][, rhoa_ohmm := rhoa_ohmm * 1.1][, `:=`(ax = ax + 50, bx = bx + 50, mx = mx + 50, nx = nx + 50)]   # other order and other positions
  m2 <- rbind(m2, data.table(a = 9, b = 10, m = 11, n = 12, rhoa_ohmm = 10, ax = 0, bx = 1, mx = 2, nx = 3, ay = 0, by = 0, my = 0, ny = 0))
  ch <- stack_change(m, m2)
  check(ch$n_matched == 4 && ch$n_2 == 5 && isTRUE(all.equal(ch$data$change_pct, rep(10, 4))) && isTRUE(all.equal(sort(ch$data$px), sort(pseudo_xy(m)$px))),
        "the change image matches quadrupoles by electrode numbers, not by position, at the first survey's positions")
  d <- rbindlist(lapply(o$survey_id[1], function(x) { mm <- r$meas[[x]]; cbind(survey_id = x, mm[, .(rhoa_ohmm)], pseudo_xy(mm)) }))
  check(stack_min_kept(NA) == STACK_MIN_KEPT && stack_min_kept(80) == 80 && identical(formals(stack_comparable)$min_kept, quote(STACK_MIN_KEPT)),
        "Compare's minimum kept % is one setting with one default (Standard and the Workbench)")
  cf <- data.table(survey_id = c("a", "b", "c"), date = c("2024-01-01", "2023-01-01", "2022-01-01"), name = c("A", "B", "C"), layout = c("L1", "L2", "L1"))
  cc <- stack_compare_choices(cf, "a")
  check(identical(unname(cc), c("", "c", "b")) && grepl("other layout", names(cc)[3]) && identical(unname(stack_compare_choices(cf, "zz")), ""),
        "Compare with... lists the other surveys, the same electrode layout first, with off first (Stack and the Workbench)")
  check(inherits(plot_stack(d, f[survey_id %in% o$survey_id[1]]), "ggplot") && inherits(plot_change(ch, "A", "B"), "ggplot"), "the stack and the change image draw")
})
local({                                                               # a GPR line's saved flow, markers and picks (pages/fct_gpr_flow.R)
  source(file.path(here, "R", "gpr.R"), local = TRUE); source(file.path(here, "pages", "fct_gpr_flow.R"), local = TRUE)
  source(file.path(here, "R", "views.R"), local = TRUE); source(file.path(here, "R", "maps.R"), local = TRUE)
  vr <- plotly::plotly_build(view_radargram(matrix(rnorm(400), 20), 1, vaxis = "depth", thaw_m = 0.3, markers = data.table(marker = c("1", "fence"), x = c(3, 9))))$x$layout
  check(length(vr$shapes) == 3L && length(vr$annotations) == 3L, "the thaw-depth line and the markers are both drawn")
  gm <- plotly::plotly_build(gpr_map(list(A = data.table(trace = 1:3, datetime = Sys.time(), lat = 45.5 + (1:3) * 1e-4, lon = -72.2, elev_m = 0, hdop = 1)), "A"))$x$data[[1]]
  check(identical(gm$customdata[1], "A|1"), "map tracks carry line|trace, for a click and the shared cursor")
  pj <- file.path(tempdir(), "gpr_flow_test"); unlink(pj, TRUE); dir.create(pj)
  st <- c(GPR_PRESETS[[1]], list(t0_ns = 0, bp1 = 0.25, bp2 = 1, bp3 = 2, bp4 = 4, ga = 0, gb = 0, agc_ns = 25, bgn = 50L))
  v1 <- gpr_flow_save(pj, "DAT 0001", st, 0.141, preset = names(GPR_PRESETS)[1]); st2 <- utils::modifyList(st, list(dewow = 2))
  v2 <- gpr_flow_save(pj, "DAT 0001", st2, 0.12, 0.0775, 0.5)
  f <- gpr_flow_load(pj, "DAT 0001"); f1 <- gpr_flow_load(pj, "DAT 0001", 1)
  check(v1 == 1L && v2 == 2L && f$version == 2L && f$velocity_m_ns == 0.12 && f$thaw_m == 0.5 && f1$steps$dewow == st$dewow && is.null(gpr_flow_load(pj, "other")) &&
        file.exists(file.path(pj, "config", "gpr_flows", "DAT_0001.json")), "a line's flow is saved in the project as versions and loads back, latest or by version")
  check(gpr_flow_same(f, st2) && !gpr_flow_same(f, st) && !gpr_flow_same(NULL, st2), "the flow in use is known to be the saved one only while nothing has changed")
  check(grepl("DAT 0001 \u00b7 flow v2", gpr_flow_caption("DAT 0001", f, c("dewow"))) && grepl("flow not saved", gpr_flow_caption("x", NULL, character())), "the caption names the line and the flow version")
  check(isTRUE(all.equal(gpr_depth(gpr_twt(c(0.3, 1.2), 0.141, 0.5, 0.0775), 0.141, 0.5, 0.0775), c(0.3, 1.2))) && isTRUE(all.equal(gpr_twt(1, 0.1), 20)),
        "two-way time from depth is the inverse of depth from two-way time, with and without a thawed layer")
  r <- gpr_pick_row("L1", 10.4, 1, "depth", c(0, 5, 10, 15), 0.1, layer = " frost table ", flow_version = 2)
  r2 <- gpr_pick_row("L1", 2.6, 40, "time", NULL, 0.1)
  check(r$trace == 3L && r$distance_m == 10 && r$twt_ns == 20 && r$depth_m == 1 && r$layer == "frost table" && r$flow_version == "2" &&
        r2$trace == 3L && is.na(r2$distance_m) && r2$depth_m == 2, "a click becomes a pick: nearest trace, two-way time and depth at the velocity")
  gpr_pick_add(pj, r); gpr_pick_add(pj, r2); gpr_pick_add(pj, gpr_pick_row("L2", 1, 10, "time", NULL, 0.1)); gpr_pick_drop_last(pj, "L1")
  pk <- gpr_picks_read(pj); check(nrow(pk) == 2L && identical(pk$line, c("L1", "L2")) && nrow(gpr_picks_read(pj, "L1")) == 1L && identical(names(pk), GPR_PICK_COLS),
        "picks are kept in config/gpr_picks.csv; Delete the last pick takes the line's last one only")
  gpr_marker_add(pj, "L1", 12, " fence "); gpr_marker_add(pj, "L2", 3); mk <- gpr_markers_read(pj, "L1")
  check(nrow(mk) == 1L && mk$trace == 12L && mk$note == "fence" && nrow(gpr_markers_read(pj)) == 2L, "markers added by hand are kept in config/gpr_markers.csv")
  gpr_marker_add(pj, "L1", 20); gpr_marker_drop_last(pj, "L1"); mk <- gpr_markers_read(pj)
  check(nrow(mk) == 2L && identical(mk[line == "L1"]$trace, 12L), "Delete the last marker takes the line's last marker added by hand only")
  d0 <- gpr_flow_default(names(GPR_PRESETS)[2])
  check(isTRUE(d0$steps$bp) && identical(d0$steps$order, names(GPR_STEPS)) && d0$thaw_m == 0 && d0$velocity_m_ns == 0.141 && d0$steps$bp1 == 0.25 &&
        identical(gpr_flow_default("")$steps$bg, GPR_PRESETS[[1]]$bg), "a line with no saved flow gets the preset's steps and every other setting at its default")
  # the steps in any order: the usual order gives exactly what the fixed order of 9 Oct gave (copied here as it was)
  gpr_process_9oct <- function(m, h, p) {
    s <- nrow(m); dt <- h$timewindow_ns / s; mhz <- gpr_antenna_mhz(h$antennas); log <- character()
    if (isTRUE(p$dc)) { m <- gpr_dc(m); log <- c(log, "DC removed (mean of the last quarter of each trace)") } else m <- sweep(m, 2, colMeans(m))
    if (isTRUE(p$dewow > 0)) { m <- gpr_dewow(m, p$dewow, dt, mhz); log <- c(log, sprintf("dewow, window %g period(s) = %.1f ns", p$dewow, p$dewow * 1000 / (mhz %||% NA))) }
    i0 <- 1L
    if (identical(p$t0, "auto")) { i0 <- gpr_time_zero(m); log <- c(log, sprintf("time zero at the first zero crossing, %.1f ns", (i0 - 1) * dt)) }
    if (identical(p$t0, "manual")) { i0 <- max(1L, round(p$t0_ns / dt) + 1L); log <- c(log, sprintf("time zero set to %.1f ns", p$t0_ns)) }
    if (i0 > 1L) m <- m[i0:s, , drop = FALSE]
    if (isTRUE(p$bp) && !is.na(mhz)) { cr <- mhz * c(p$bp1, p$bp2, p$bp3, p$bp4); m <- gpr_bandpass(m, dt, cr)
      log <- c(log, sprintf("Ormsby band-pass %s MHz", paste(round(pmin(cr, 0.98 * 500 / dt)), collapse = "-"))) }
    g <- p$gain %||% "none"
    if (g == "decay") { m <- gpr_energy_decay(m); log <- c(log, "energy decay gain") }
    if (g == "fn") { m <- gpr_gain_fn(m, dt, mhz, p$ga %||% 0, p$gb %||% 0); log <- c(log, sprintf("gain function a' = %g, b' = %g dB/m", p$ga %||% 0, p$gb %||% 0)) }
    if (g == "agc") { m <- gpr_agc(m, max(3L, round((p$agc_ns %||% 25) / dt))); log <- c(log, sprintf("AGC, window %g ns", p$agc_ns %||% 25)) }
    bg <- p$bg %||% "none"
    if (bg != "none") { m <- gpr_background(m, bg, p$bgn %||% 50L); log <- c(log, if (bg == "mean") "background removal (mean trace)" else sprintf("background removal (moving mean, %d traces)", p$bgn %||% 50L)) }
    list(m = m, t0_ns = (i0 - 1) * dt, dt_ns = dt, log = log)
  }
  set.seed(4); mm <- matrix(rnorm(64 * 40), 64) + outer(1:64, 1:40, function(i, j) 5 * sin(i / 4)); hh <- list(timewindow_ns = 64, antennas = "100 MHz")
  pp <- list(dc = TRUE, dewow = 1, t0 = "auto", bp = TRUE, bp1 = 0.25, bp2 = 1, bp3 = 2, bp4 = 4, gain = "agc", agc_ns = 10, bg = "moving", bgn = 10L)
  same <- all(vapply(c(list(pp), lapply(GPR_PRESETS, function(x) c(x, pp[c("bp1", "bp2", "bp3", "bp4", "agc_ns", "bgn")]))), function(q) {
    a <- gpr_process(mm, hh, q); b <- gpr_process_9oct(mm, hh, q); identical(a$m, b$m) && identical(a$log, b$log) && identical(a$t0_ns, b$t0_ns) }, NA))
  ro <- gpr_process(mm, hh, c(pp, list(order = c("bg", "dc", "dewow", "t0", "bp", "gain"))))
  check(same && grepl("^background", ro$log[1]) && !identical(ro$m, gpr_process(mm, hh, pp)$m), "steps run in the order given; the usual order gives the same result as before")
  check(identical(gpr_order(c("gain", "x")), c("gain", "dc", "dewow", "t0", "bp", "bg")) && identical(gpr_order_move(NULL, "dewow", -1L)[1:2], c("dewow", "dc")) &&
        identical(gpr_order_move(NULL, "dc", -1L), names(GPR_STEPS)), "a step moves one place up or down, never off the ends")
  check(gpr_step_summary("dewow", pp, 100) == "1 period(s) (10.0 ns)" && gpr_step_summary("bp", pp, 100) == "25 / 100 / 200 / 400 MHz" && gpr_step_summary("bg", list(), NA) == "off",
        "each step says what it does in one line")
  check(gpr_flow_same(list(steps = pp), c(pp, list(order = names(GPR_STEPS)))) && !gpr_flow_same(list(steps = c(pp, list(order = c("bg")))), pp), "a flow is the same only in the same order")
  check(gpr_flow_apply_all(pj, c("L1", "L2"), pp, 0.1) == 2L && length(gpr_flow_history(pj, "L1")) == 1L && identical(unlist(gpr_flow_load(pj, "L2")$steps$order), NULL),
        "Apply to all lines saves the flow as a new version of each line")
  ax <- list(m = matrix(0, 2, 5), h = list(distance_interval_m = 0.5), cor = NULL, mrk = data.table(marker = 1:2, trace = c(2L, 4L)))
  check(identical(gpr_axis(ax)$full, c(0, 0.5, 1, 1.5, 2)) && identical(gpr_axis(ax, "markers", 10)$full, c(-5, 0, 5, 10, 15)) && gpr_axis(ax, "markers", 10)$label == "Distance from markers (m)" &&
        is.null(gpr_axis(ax, "trace")$full) && gpr_axis(ax, "markers", NA)$label == "Trace", "GPR processing's distance across: GPS or header interval, markers, or trace")
  check(gpr_line_year(NULL) == "" && gpr_line_year(data.table(datetime = as.POSIXct(c("2016-09-01", "2015-08-01"), tz = "UTC"))) == "2015", "the year of a line is that of its first GPS fix")
})
if (!identical(Sys.getenv("OVID_SKIP_UI"), "1") && requireNamespace("chromote", quietly = TRUE) && !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) local({
  source(file.path(CORE_DIR, "uitest.R"), local = TRUE)
  ui <- tryCatch(ui_click_through(normalizePath(here), c(ERT_PROJECT_DIR = file.path(tmp, "project"), ERT_RAW_DIRS = dirname(raw), ERT_NO_RUN = "1")), error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check(nrow(ui) > 3 && !nrow(bad), "every page opens in a browser with no output, JavaScript or server error (the test project)") })
if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green")
