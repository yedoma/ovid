# core/tests/run_tests.R  --  Tests of the shared core on its own (no app): the deployment sheet (deployments.R), set-up table rules, the editor, and
# the output kept while a run goes (manifest.R).
#   Rscript core/tests/run_tests.R      (exit status 1 if anything fails)
suppressMessages(library(data.table))
CORE_DIR <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else "core" })
for (.f in c("util", "convert", "audit", "config", "calibration", "table_rules", "metasheet", "import", "deployments")) source(file.path(CORE_DIR, paste0(.f, ".R")))
SERIAL_KEEP <- "alnum"; RES <- new.env(); RES$ok <- 0L; RES$fail <- character()
check <- function(what, cond) { if (isTRUE(cond)) { RES$ok <- RES$ok + 1L; cat("  ok   ", what, "\n") } else { RES$fail <- c(RES$fail, what); cat("  FAIL ", what, "\n") } }
t <- function(x) as.POSIXct(x, tz = "UTC")

cat("deployment sheet: cleaning\n")
ex <- fread(file.path(CORE_DIR, "templates", "metadata_example.csv"), colClasses = "character", na.strings = NULL)
map <- meta_guess(names(ex), meta_defaults(DEPLOY_FIELDS)); x <- copy(ex); setnames(x, map)
cl <- dep_clean(x); r <- cl$rows
check("the template's headings are recognised as the eight fields", identical(map, DEPLOY_COLS))
check("serials are cleaned (leading zeros of all-digit serials go, letters stay)", identical(r$.serial, c("10000001", "1000002", "1000002", "1000003", "1000004", "E00001", "123")))
check("a year alone stands for the whole year; a To is inclusive", r$.from_prec[4] == "year" && format(r$.from[4]) == "2019-01-01" && format(r$.to[2], "%H:%M:%S") == "23:59:59")
check("the example has no problems", !nrow(cl$problems))
bad <- data.table(serial = c("SN: 123", "123", "", "77"), from = c("2024-05-01", "2024-06-01", "", "Sept 2024"), to = c("2024-07-01", "", "", "2024-01-01"),
                  latitude = c("60 06 11 N", "", "", "60.1"), longitude = c("120 06 30 W", "120.1", "", "-120.1"))
cb <- dep_clean(bad)
check("SN: prefixes, DMS coordinates and month names are read", cb$rows$.serial[1] == "123" && abs(cb$rows$.lat[1] - 60.10306) < 1e-4 && cb$rows$.lon[1] < 0 && cb$rows$.from_prec[4] == "month")
check("problems: overlapping rows of one serial, To before From, a missing serial, a west longitude without its minus",
      all(c("overlaps", "before From", "no serial", "minus sign") %in% unlist(lapply(c("overlaps", "before From", "no serial", "minus sign"), function(k) if (any(grepl(k, cb$problems$problem))) k))))

cat("deployment sheet: what the files say, and matching\n")
files <- data.table(serial = c("1000002", "1000002", "1000002", "1000003", "1000004", "10000001", ""), name = c("ST01", "ST01", "ST02", "Upland", "Upland", "Outlet", "x"),
                    first = t(c("2022-06-01", "2023-06-01", "2023-09-25", "2019-06-01", "2021-09-12", "2020-01-01", "2020-01-01")),
                    last = t(c("2023-06-01", "2023-09-20", "2024-09-01", "2021-09-01", "2024-09-01", "2024-01-01", "2020-02-01")), file = paste0("f", 1:7))
pr <- dep_propose(files); d <- pr$deployments
check("files become deployments: a serial saved under a new name later is a second deployment; a file with no serial is set apart",
      nrow(d) == 5 && sum(d$serial == "1000002") == 2 && nrow(pr$unassigned) == 1)
m <- dep_match(r, d)
check("rows match by dates, by year, or by the station name saved in the files; a serial with no files is 'no data'",
      identical(m$match, c("name", "dates", "dates", "year", "dates", "no data", "no data")) && m$files[2] == 2 && m$dep_ids[3] == "1000002#2")
check("a serial with one deployment and no dates or name to go by is matched as its only one",
      dep_match(dep_clean(data.table(serial = "1000003"))$rows, d)$match == "only")
check("every deployment the files show is claimed by a row", !nrow(dep_unclaimed(m, d)))

cat("deployment sheet: merging\n")
new <- data.table(serial = c("1000002", "10000001", "555"), station = c("Transect 3", "Outlet", "New"), from = c("2024-09-30", "", ""), notes = c("", "checked", ""), latitude = c("", "60.2", ""))
mg <- dep_merge(x, new); o <- mg$rows; lg <- mg$log
check("a later row at another station closes the open one the day before, and is added", any(lg$action == "closed" & lg$now == "2024-09-29") && sum(dep_serial(o$serial) == "1000002") == 3)
check("blank cells are filled, a differing filled cell is a conflict kept as it was, a new serial is added, nothing is deleted",
      any(lg$action == "conflict" & lg$field == "latitude") && nrow(o) == nrow(ex) + 2 && any(lg$action == "added" & lg$serial == "555"))
check("the summary line counts each action", grepl("added", dep_merge_summary(lg)) && grepl("closed", dep_merge_summary(lg)))

cat("deployment sheet: against the files an app listed\n")
local({ pd <- tempfile("dep"); dir.create(file.path(pd, "config"), recursive = TRUE); dir.create(file.path(pd, "output", "audit"), recursive = TRUE)
  fwrite(x, file.path(pd, "config", DEPLOY_SHEET))
  fwrite(files[, .(serial, title = name, first_ts = format(first), last_ts = format(last), rel = file, status = "ok")][nzchar(serial)][, serial := fifelse(serial == "10000001", "999", serial)],
         file.path(pd, "output", "audit", "files.csv"))
  ck <- dep_check(pd, list(file = "output/audit/files.csv", serial = "serial", name = "title", file_col = "rel"))
  check("the check lists each row's match and the deployments found in no row", nrow(ck$rows) == 7 && ck$rows[1, match] == "no data" && nrow(ck$unclaimed) == 1 && ck$unclaimed$serial == "999")
  add <- dep_rows_found(ck$unclaimed); mg <- dep_merge(read_cfg(file.path(pd, "config", DEPLOY_SHEET)), add)
  check("adding what was found names the station from the files and keeps every row", nrow(mg$rows) == 8 && mg$rows[serial == "999", station] == "Outlet") })

cat("deployment sheet: accepting a suggested match\n")
local({ pd <- tempfile("depacc"); dir.create(file.path(pd, "config"), recursive = TRUE)
  fwrite(data.table(serial = c("1000003", "1000002"), station = c("Upland", "ST01"), from = c("2019", "2022-06-01"), to = c("", "2023-06-01")), file.path(pd, "config", DEPLOY_SHEET))
  r <- dep_match(dep_clean(read_cfg(file.path(pd, "config", DEPLOY_SHEET)))$rows, d)
  dep_accept_dates(pd, r); x <- read_cfg(file.path(pd, "config", DEPLOY_SHEET))
  check("Accept writes a suggested match's first and last record day into From and To, and leaves a match by dates as it is",
        r$match[1] == "year" && x$from[1] == "2019-06-01" && x$to[1] == "2021-09-01" && x$from[2] == "2022-06-01" && x$to[2] == "2023-06-01") })

cat("set-up tables: declared rules\n")
local({ d <- data.table(serial = c("1", "1", "2", "", "3"), start = c("2024-01-01", "2024-05-01", "bad", "", "2024-01-01"), end = c("2023-01-01", "", "", "", ""),
                        latitude = c("61", "95", "x", "", "61"), mode = c("shift", "zap", "", "", "shift"), notes = "")
  d <- rbind(d, d[5])
  pd <- tempfile("rules"); dir.create(file.path(pd, "config"), recursive = TRUE); fwrite(data.table(serial = c("1", "2")), file.path(pd, "config", "known.csv"))
  x <- table_check(d, table_rules(names(d), list(required = "serial", choices = list(mode = c("shift", "drift")), unique = c("serial", "start"),
                                                 known = list(serial = known_from("config/known.csv", "serial")))), pd)
  has <- function(r, col, txt) any(x$row == r & x$column == col & grepl(txt, x$problem))
  check("dates, numbers and ranges, periods that run backwards, choices, keys used twice, unknown values and repeated rows are found; a blank row is not",
        has(1, "end", "before start") && has(2, "latitude", "outside") && has(3, "start", "not a date") && has(3, "latitude", "not a number") && has(2, "mode", "not one of") &&
        has(6, "serial", "same serial and start") && has(5, "serial", "not a serial this project has") && has(6, "serial", "same as row 5") && !any(x$row == 4))
  res <- data.table(level = "error", area = "Clock corrections", item = "row 1", problem = "own check")
  CONFIG_FILES <<- list(time = list(file = "time_corrections.csv", label = "Clock corrections"))
  fwrite(d, file.path(pd, "config", "time_corrections.csv")); y <- with_declared(res, pd)
  check("the set-up check adds the declared findings, leaving out a row the app's own check already names", nrow(y) > 1 && sum(grepl("row 1", y$item)) == 1)
  rm(CONFIG_FILES, envir = globalenv()) })

cat("set-up tables: the editor\n")
if (requireNamespace("shiny", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "editor.R"))
  f <- tempfile(fileext = ".csv"); fwrite(data.table(station = c("A", "B"), start = c("2024-01-01", "2024-02-01"), end = c("", "")), f)
  testServer(ovid_edit_server, args = list(path = reactive(f), reload = reactive(0)), {
    session$flushReact(); n0 <- nrow(data())
    session$setInputs(add = 1); n1 <- nrow(data()); session$setInputs(undo = 1); n2 <- nrow(data())
    check("Undo takes back the last change not yet saved", n1 == n0 + 1 && n2 == n0 && !dirty())
    d <- data(); d$end[1] <- "2023-01-01"; data(d); session$flushReact()
    check("the editor lists what the table's rules find as it is edited", grepl("end is before start", paste(as.character(output$checks), collapse = ""))) }) })

cat("deployment sheet and an app's logger list (one sheet)\n")
local({ pd <- tempfile("ms"); dir.create(file.path(pd, "config"), recursive = TRUE)
  fields <- data.table(field = c("serial", "site", "start", "notes", "depth"), type = c("text", "text", "date", "text", "text"), identifies = c("TRUE", "", "", "", ""))
  old <- data.table(serial = c("111", "222", "444"), site = c("Own site", "", "Far"), start = c("", "2021-06-01", ""), notes = "", depth = c("", "", "2 m"))
  fwrite(old, file.path(pd, "config", "metadata.csv"))
  fwrite(data.table(serial = c("111", "222", "333"), site = c("Plot", "", "New"), station = "", from = c("2020-05-01", "2021-06-01", ""), to = "", notes = c("", "checked", "")),
         file.path(pd, "config", "deployment_sheet.csv"))
  dep <- function(r) data.table(serial = r$serial, site = dep_where(r), start = dep_day(r$.from), notes = r$notes)
  CONFIG_FOLDS <<- list(list(from = "metadata.csv", into = "deployment_sheet.csv", fold = meta_fold(fields, dep))); on.exit(rm(CONFIG_FOLDS, envir = globalenv()))
  AUDIT$problems <- list(); done <- config_migrate(pd, "test"); sh <- read_cfg(file.path(pd, "config", "deployment_sheet.csv"))
  check("the logger list moves into the deployment sheet once: one row per logger, its own columns kept, the old file left in place",
        identical(done, "metadata.csv") && nrow(sh) == 4 && sh[serial == "111", site] == "Own site" && sh[serial == "444", depth] == "2 m" && file.exists(file.path(pd, "config", "metadata.csv")))
  check("where both gave a value and they differ, the logger list's (typed) is kept and the difference listed",
        any(grepl("111: the deployment sheet gave site 'Plot'", vapply(AUDIT$problems, function(x) x$problem, ""))))
  check("a second read moves nothing again (config/migrated.json)", !length(config_migrate(pd, "test")) && nrow(read_cfg(file.path(pd, "config", "deployment_sheet.csv"))) == 4)
  ms <- meta_sheet(pd, fields, deploy = dep)
  check("the sheet as the app reads it: From gives the start, the optional columns come along, only the serial is needed",
        format(ms[serial == "111", start]) == "2020-05-01" && ms[serial == "222", notes] == "checked" && ms[serial == "444", depth] == "2 m" && "333" %in% ms$serial)
  sh[serial == "111", start := "2020-06-01"]; fwrite(sh, file.path(pd, "config", "deployment_sheet.csv"))
  ms <- meta_sheet(pd, fields, deploy = dep, report = FALSE); cf <- attr(ms, "conflicts")
  check("a typed value wins over what From and To give, and the difference is listed; the same date written two ways is no difference",
        format(ms[serial == "111", start]) == "2020-06-01" && nrow(cf) == 1 && cf$field == "start" && cf$sheet == "2020-05-01") })

cat("the deployment sheet: Split here (deployments.R)\n")
local({ d <- data.table(serial = "A1", site = "Bog", station = "", from = "2020-01-01", to = "", latitude = "", longitude = "", notes = "")
  x <- dep_split_rows(d, "A1", "2020-06-01")
  check("a split inside a row ends it the day before and starts a copy of it on the day", nrow(x) == 2 && identical(x$to, c("2020-05-31", "")) && identical(x$from, c("2020-01-01", "2020-06-01")) &&
        all(x$site == "Bog") && grepl("split here", x$notes[2]))
  check("the same split again changes nothing; the sheet's splits are read back", nrow(dep_split_rows(x, "A1", "2020-06-01")) == 2 && identical(dep_splits(x)$at, "2020-06-01"))
  y <- dep_split_rows(d[0], "B2", "2021-03-01", place = list(site = "Fen"))
  check("a logger the sheet does not list gets two rows, before and after the gap, at the app's place for it", nrow(y) == 2 && identical(y$to, c("2021-02-28", "")) && all(y$site == "Fen"))
  source(file.path(CORE_DIR, "grouping.R"), local = TRUE)
  pd <- tempfile("sp"); dir.create(file.path(pd, "config"), recursive = TRUE); fwrite(d, file.path(pd, "config", "deployment_sheet.csv"))
  fwrite(data.table(serial = "A1", at = "2020-06-01", note = ""), file.path(pd, "config", GROUP_SPLIT_FILE))
  r <- group_split_fold()$fold(read_cfg(file.path(pd, "config", GROUP_SPLIT_FILE)), read_cfg(file.path(pd, "config", "deployment_sheet.csv")), pd)
  check("an older version's logger_splits.csv becomes rows of the sheet", nrow(r$rows) == 2 && identical(dep_splits(r$rows)$at, "2020-06-01")) })

cat("runs: the last finished output kept while a run goes\n")
local({ for (.f in c("cache", "manifest")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  APP_NAME <- "Ovid Core // TEST"; say <- function(...) invisible()
  pd <- file.path(tempfile("proj"), "p"); dir.create(file.path(pd, "output", "stations"), recursive = TRUE); dir.create(file.path(pd, "cache"))
  writeLines("old", file.path(pd, "output", "stations", "A.csv")); writeLines("old cache", file.path(pd, "cache", "old.rds"))
  Sys.setFileTime(file.path(pd, "cache", "old.rds"), Sys.time() - 3600)
  cfg <- list(project_dir = pd, raw_dirs = { r <- tempfile("raw"); dir.create(r); r })
  r <- tryCatch(run_project(cfg, function() { writeLines("half", file.path(pd, "output", "stations", "A.csv")); writeLines("new", file.path(pd, "output", "stations", "B.csv"))
    writeLines("new cache", file.path(pd, "cache", "new.rds")); stop("broke part way") }), error = function(e) conditionMessage(e))
  o <- file.path(pd, "output")
  check("a run that fails puts the output back as the last finished run left it",
        identical(r, "broke part way") && readLines(file.path(o, "stations", "A.csv")) == "old" && !file.exists(file.path(o, "stations", "B.csv")) && !dir.exists(file.path(pd, OUTPUT_KEPT)))
  check("... keeps the stopped run's log and says what happened", length(list.files(file.path(o, "logs"), "\\.log$")) == 1 && grepl("put back", run_restored(pd)) && !nzchar(run_incomplete(pd)))
  check("... and drops the cached results it made, keeping the older ones", !file.exists(file.path(pd, "cache", "new.rds")) && file.exists(file.path(pd, "cache", "old.rds")))
  CACHE_INPUT_ONLY <- "sources"; dir.create(file.path(pd, "cache", "sources", "S"), recursive = TRUE)        # an app's input-only cache (RS scans)
  tryCatch(run_project(cfg, function() { writeLines("scan", file.path(pd, "cache", "sources", "S", "exif.rds")); writeLines("step", file.path(pd, "cache", "step.rds")); stop("broke") }), error = function(e) NULL)
  check("... but keeps the cache an app marks as read from its inputs only (RS cache/sources)", file.exists(file.path(pd, "cache", "sources", "S", "exif.rds")) && !file.exists(file.path(pd, "cache", "step.rds")))
  rm(CACHE_INPUT_ONLY)
  run_project(cfg, function() writeLines("new", file.path(pd, "output", "stations", "A.csv")))
  check("a run that finishes keeps its output and removes the copy", readLines(file.path(o, "stations", "A.csv")) == "new" && !dir.exists(file.path(pd, OUTPUT_KEPT)) && !nzchar(run_restored(pd)))
  output_keep(pd); writeLines("half", file.path(o, "stations", "A.csv"))                      # a run whose process was killed
  check("a copy left by a killed run is put back when the app starts", nzchar(output_recover(pd)) && readLines(file.path(o, "stations", "A.csv")) == "new")
  OUTPUT_COPY_MAX <- 0; disk_clones <- function(path) FALSE
  check("no copy is kept of an output too large to copy; the run log says so", grepl("too large", output_keep(pd)))
  DISK_MIN <- Inf
  check("a run does not start on a full drive", inherits(tryCatch(run_project(cfg, function() NULL), error = function(e) e), "ovid_project_check"))
  check("the free space of a drive is read", isTRUE(disk_free(pd) > 0))
  check("folders a cloud service syncs are recognised, others are not",
        identical(vapply(c("~/Library/Mobile Documents/com~apple~CloudDocs/raw", "~/Library/CloudStorage/OneDrive-Org/p", "C:/Users/x/Dropbox (Personal)/raw", "/data/raw"), cloud_service, "", USE.NAMES = FALSE),
                  c("iCloud Drive", "OneDrive", "Dropbox", ""))) })

cat("automatic corrections: the list\n")
local({ source(file.path(CORE_DIR, "corrections.R"), local = TRUE)
  pd <- tempfile("corr"); dir.create(file.path(pd, "config"), recursive = TRUE); fwrite(data.table(pattern = c("a", "b")), file.path(pd, "config", "qc_rules.csv"))
  L <- list(list(id = "stray", does = "removes", setting = "drop_stray", panel = TRUE, what = "x"), list(id = "codes", does = "removes", setting = "", what = "x"),
            list(id = "alt", does = "changes", setting = "alt", on = function(cfg) identical(cfg$alt, "undo"), what = "x"),
            list(id = "qc", does = "removes", table = "qc_rules.csv", what = "x"), list(id = "man", does = "removes", table = "manual_removals.csv", what = "x"))
  cfg <- list(project_dir = pd, drop_stray = TRUE, alt = "keep"); t <- corrections_table(cfg, L)
  check("each correction's state: on, always, off by a setting that is not TRUE/FALSE, and by its table with its rows",
        identical(t$state, c("on", "always", "off", "by table, 2 rows", "by table, none")))
  check("a switch on the list reaches the run's settings; a setting not saved keeps its default",
        isFALSE(corrections_cfg(cfg, list(drop_stray = FALSE), L)$drop_stray) && isTRUE(corrections_cfg(cfg, list(), L)$drop_stray))
  L2 <- list(list(id = "clock", does = "removes", setting = "drop_clock", panel = TRUE, what = "x"),
             list(id = "pick", does = "changes", setting = "prefer", panel = TRUE, choices = c("Newest" = "newest", "Oldest" = "oldest"), what = "x"))
  check("a switch the settings do not have is on; a setting with choices shows the one chosen and reaches the run as it is",
        identical(corrections_table(list(), L2)$state, c("on", "Newest")) && corrections_cfg(list(), list(prefer = "oldest"), L2)$prefer == "oldest" &&
        identical(corrections_table(list(prefer = "oldest"), L2)$state[2], "Oldest"))
  l <- corrections_log(cfg, L)
  check("the run log lists them by state", any(grepl("on: stray", l)) && any(grepl("always: codes", l)) && any(grepl("off: alt", l)) && any(grepl("qc \\(2 rows\\)", l))) })

cat("projects: new, settings kept per project, delete to the Trash\n")
local({ for (.f in c("cache", "manifest", "projects")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  APP_NAME <- "Ovid Core // TEST"; options(ovid.trash_dir = file.path(tempdir(), "trash")); on.exit(options(ovid.trash_dir = NULL))
  root <- tempfile("projs"); main <- file.path(root, "main"); dir.create(file.path(main, "config"), recursive = TRUE); dir.create(file.path(main, "output", "audit"), recursive = TRUE)
  writeLines("app: TEST", file.path(main, PROJECT_MARK)); fwrite(data.table(station = "A"), file.path(main, "config", "stations.csv")); writeLines("x", file.path(main, "output", "audit", "a.csv"))
  q <- project_new(file.path(root, "new"), "Field season", copy_from = main, settings = list(raw_dirs = "/data/raw", min_year = 2001, project_dir = main))
  s <- project_settings_read(q)
  check("a new project has its name, the set-up tables it was given, no raw folders and the other settings",
        project_name(q) == "Field season" && file.exists(file.path(q, "config", "stations.csv")) && !length(s$raw_dirs) && s$min_year == 2001 && path_norm(s$project_dir) == path_norm(q))
  check("a folder holding other things, or another app's project, is not made a project",
        { dir.create(o <- file.path(root, "other")); writeLines("x", file.path(o, "notes.txt")); inherits(tryCatch(project_new(o), error = function(e) e), "ovid_project_check") } &&
        { writeLines("app: GT", file.path(root, "other", PROJECT_MARK)); unlink(file.path(o, "notes.txt")); inherits(tryCatch(project_new(o), error = function(e) e), "ovid_project_check") })
  check("the projects used, the open one first, each once", identical(projects_known(list(project_dir = main, projects = c(q, main, q))), c(main, q)))
  pl <- project_delete_plan(main, "outputs")
  check("deleting outputs takes output and cache only", identical(basename(pl$items), "output") && pl$why == "")
  writeLines("mine", file.path(main, "thesis.docx"))
  check("a project folder holding things Ovid did not make is not deleted whole", nzchar(project_delete_plan(main, "project")$why))
  project_delete(main, "outputs")
  check("outputs go to the Trash; the set-up stays", !dir.exists(file.path(main, "output")) && file.exists(file.path(main, "config", "stations.csv")) &&
        length(list.files(getOption("ovid.trash_dir"))) == 1)
  project_delete(q, "project"); check("a whole project goes to the Trash", !dir.exists(q))
  dir.create(file.path(main, ".ovid_lock")); writeLines(c(paste0("pid: ", Sys.getpid()), paste0("host: ", Sys.info()[["nodename"]])), file.path(main, ".ovid_lock", "run.txt"))
  check("nothing is deleted while a run holds the project", grepl("run is going", project_delete_plan(main, "outputs")$why)) })

cat("renaming a site in every set-up table\n")
local({ source(file.path(CORE_DIR, "rename.R"), local = TRUE)
  pd <- tempfile("ren"); dir.create(file.path(pd, "config"), recursive = TRUE); cf <- function(f) file.path(pd, "config", f)
  fwrite(data.table(site = c("Plot 1", "Plot 2"), notes = ""), cf("sites.csv"))
  fwrite(data.table(serial = c("1", "2", "3"), site = c("plot 1 ", "Plot 2", "Plot 1")), cf("sensors.csv"))
  fwrite(data.table(pattern = c("a", "b", "c"), applies_to = c("Plot 1|Plot 2", "site:Plot 1", "Plot 10")), cf("qc_rules.csv"))
  spec <- list(site = list(label = "Site", main = c("sites.csv", "site"), cols = list(c("sensors.csv", "site"), c("missing.csv", "site")), lists = list(c("qc_rules.csv", "applies_to"))))
  pl <- rename_plan(pd, "site", "Plot 1", "North plot", spec)
  check("the preview finds the name in each table (ignoring case and spaces at the ends) and in lists of names, and not in a longer name",
        nrow(pl) == 5 && identical(sort(unique(pl$file)), c("qc_rules.csv", "sensors.csv", "sites.csv")) && !"Plot 10" %in% pl$before &&
        identical(pl[file == "qc_rules.csv", after], c("North plot|Plot 2", "site:North plot")))
  b <- rename_apply(pd, pl, "test")
  check("renaming changes every cell, keeps the tables before in config/history, and Undo puts them back",
        identical(fread(cf("sensors.csv"))$site, c("North plot", "Plot 2", "North plot")) && length(list.files(file.path(pd, "config", "history"), "__.*\\.csv$")) == 3 &&
        { rename_undo(pd, b); identical(trimws(fread(cf("sensors.csv"), colClasses = "character")$site), c("plot 1", "Plot 2", "Plot 1")) })
  nm <- rename_names(pd, "site", spec); check("the names to pick from, each once", identical(nm, c("Plot 1", "Plot 2"))); if (!identical(nm, c("Plot 1", "Plot 2"))) print(nm) })

cat("review decisions: one file for Needs you, read by the runs (config.R)\n")
local({ source(file.path(CORE_DIR, "cache.R"), local = TRUE); source(file.path(CORE_DIR, "rename.R"), local = TRUE)
  pd <- tempfile("rev"); dir.create(file.path(pd, "config"), recursive = TRUE); dir.create(file.path(pd, "output", "audit"), recursive = TRUE)
  fl <- data.table(site = c("Bog", "Bog", "Fen"), parameter = c("depth_m", "temp_c", "depth_m"), start = c("2024-01-01 00:00:00", "2024-02-01 06:00:00", "2024-01-01 00:00:00"),
                   end = c("2024-01-01 06:00:00", "2024-02-01 07:00:00", "2024-01-02 00:00:00"), records = 3L, check = c("spike", "freezing", "spike"), rule_id = "x", qc_code = 2L, app = "T", run_id = "1")
  check("a review id is the same for a time read back as a time, as text, or as a midnight that lost its 00:00:00",
        identical(wb_queue_id("Bog", "depth_m", as.POSIXct("2024-01-01", tz = "UTC"), "2024-01-01 06:00", "spike"), wb_queue_id("Bog", "depth_m", "2024-01-01", "2024-01-01 06:00:00", "spike")) &&
        identical(wb_queue_id("Bog", "depth_m", "2024-01-01 00:00:00", "2024-01-01 06:00:00", "spike"), substr(hash_of("Bog|depth_m|2024-01-01 00:00:00|2024-01-01 06:00:00|spike"), 1, 16)))
  rp <- review_path(pd); old <- substr(hash_of("Fen|depth_m|2024-01-01|2024-01-02|spike"), 1, 16)      # an id an earlier version wrote
  review_write(rp, character(), data.table(id = c(wb_queue_id("Bog", "depth_m", fl$start[1], fl$end[1], "spike"), old), decision = c("not a problem", "not a problem"), note = "", author = "", at = ""))
  a <- review_accept(fl, review_read(rp), "site")
  check("a run marks the flags decided not a problem as accepted (an older id too), and flags.csv still ends with rule_id, qc_code, app, run_id",
        identical(a$accepted, c(TRUE, FALSE, TRUE)) && identical(tail(names(a), 4), c("rule_id", "qc_code", "app", "run_id")) && nrow(review_open(fl, review_read(rp), "site")) == 1)
  d <- data.table(TIMESTAMP = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:8, flag_depth_m = c("spike", "spike;freezing", rep("", 7)))
  u <- review_unmark(copy(d), a[accepted & site == "Bog"])
  check("an accepted flag leaves the values' flag column, other checks stay", identical(u$flag_depth_m[1:2], c("", "freezing")))
  fwrite(a, file.path(pd, "output", "audit", "flags.csv")); fwrite(data.table(site = "Bog"), file.path(pd, "config", "sites.csv"))
  fwrite(data.table(target = c("Bog", "Fen"), parameter = "", from = "", to = "", action = "remove"), file.path(pd, "config", "period_corrections.csv"))
  fwrite(data.table(name = "Bog", shown = "FALSE"), file.path(pd, "config", "display.csv"))
  spec <- list(site = list(label = "Site", main = c("sites.csv", "site"), cols = list(), lists = list()))
  pl <- rename_plan(pd, "site", "Bog", "Bog North", spec); rename_apply(pd, pl, "test")
  check("a rename changes the period corrections' target, what shows, and carries the review decision to the new name",
        identical(read_cfg(file.path(pd, "config", "period_corrections.csv"))$target, c("Bog North", "Fen")) && read_cfg(file.path(pd, "config", "display.csv"))$name == "Bog North" &&
        wb_queue_id("Bog North", "depth_m", fl$start[1], fl$end[1], "spike") %in% review_read(rp)$id) })

cat("Needs you: the one review list (flow.R)\n")
if (requireNamespace("shiny", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "cache.R"), local = TRUE)
  for (.f in c("periods", "shell", "flow")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  fl <- data.table(site = c("Bog", "Bog", "Bog", "Fen"), parameter = "depth_m", start = c("2024-01-01 00:00:00", "2024-01-01 03:00:00", "2024-03-01 00:00:00", "2024-01-01 00:00:00"),
                   end = c("2024-01-01 01:00:00", "2024-01-01 04:00:00", "2024-03-01 01:00:00", "2024-01-01 01:00:00"), records = c(2, 3, 1, 4), check = "spike", detail = "a jump",
                   logger_id = c("A1", "A1", "A1", "B2"))
  it <- review_flag_items(fl, "site", keep = "logger_id")
  check("flags of one place and check a few hours apart are one item, with every flag's review id", nrow(it) == 3 && it[where == "Bog" & records == 5, start] == "2024-01-01 00:00:00" &&
        identical(strsplit(it[records == 5, qids], " ")[[1]], wb_queue_id(c("Bog", "Bog"), "depth_m", fl$start[1:2], fl$end[1:2], "spike")) && it[where == "Fen", logger_id] == "B2")
  d <- data.table(id = strsplit(it[records == 5, qids], " ")[[1]][1], decision = "not a problem")
  check("an item is decided only when every one of its flags is", all(is.na(review_item_decision(it, d))) &&
        identical(review_item_decision(it, rbind(d, data.table(id = strsplit(it[records == 5, qids], " ")[[1]][2], decision = "not a problem")))[it$records == 5], "not a problem"))
  check("Remove and Correct write a row of the corrections over a period over the item's stretch", identical(names(review_period_row(it[1], "offset", 0.1)), PERIOD_COLS) &&
        review_period_row(it[where == "Bog" & records == 5], "remove")$to == "2024-01-01 04:00:00")
  pd <- tempfile("rl"); dir.create(file.path(pd, "config"), recursive = TRUE); rp <- review_path(pd)
  its <- rbind(it, data.table(kind = "moved", where = "A1", what = "Looks like it was moved", serial = "A1", at = "2024-02-01 00:00"),
                 data.table(kind = "note", where = "3", what = "files could not be read", go = "Files"), fill = TRUE)
  testServer(review_list_server, args = list(items = reactive(NULL), file = reactive(rp)), {
    session$setInputs(only = TRUE); h <- tryCatch(paste(as.character(output$list$html), collapse = ""), error = function(e) conditionMessage(e))
    check("an empty list says nothing waits for you, not an error", grepl("Nothing waits for you", h, fixed = TRUE)) })
  split_to <- NULL
  testServer(review_list_server, args = list(items = reactive(its), file = reactive(rp), split = function(s, a) split_to <<- c(s, a)), {
    session$setInputs(only = TRUE); id1 <- rows()[kind == "flag" & records == 5, id]
    check("the list holds the flags, the moves and the run's findings", nrow(open_items()) == 5)
    h <- paste(as.character(output$list$html), collapse = "")
    check("... each with its own actions: Keep, Remove, Correct and Not a problem; Split here; the page that settles a finding",
          all(vapply(c("Keep", "Remove\u2026", "Not a problem", "Split here", "data-ovid-go=\"Files\""), grepl, NA, h, fixed = TRUE)) && !grepl("Correct\u2026", h, fixed = TRUE))
    session$setInputs(act = list(id = id1, a = "keep"))
    check("Keep writes every flag of the item to review_decisions.csv and the item leaves the open list",
          all(strsplit(its[records == 5, qids], " ")[[1]] %in% review_read(rp)[decision == REVIEW_KEEP, id]) && nrow(open_items()) == 4)
    session$setInputs(only = FALSE)
    check("unticking Only what needs a look shows a decided item, with Undo", grepl("Undo", paste(as.character(output$list$html), collapse = ""), fixed = TRUE))
    session$setInputs(only = TRUE, act = list(id = id1, a = "undo"))
    check("Undo takes the decision back", nrow(review_read(rp)) == 0 && nrow(open_items()) == 5)
    session$setInputs(act = list(id = rows()[kind == "moved", id], a = "split"))
    check("Split here splits the logger at the suggested time", identical(split_to, c("A1", "2024-02-01 00:00")))
    session$setInputs(act = list(id = id1, a = "ok")); session$setInputs(reason = "", author = "AB"); session$setInputs(ok_go = 1)
    check("Not a problem needs a reason", nrow(review_read(rp)) == 0)
    session$setInputs(reason = "a frog on the sensor"); session$setInputs(ok_go = 2)
    check("... and with one the flags are accepted, the reason kept", all(review_read(rp)$decision == "not a problem") && all(grepl("frog", review_read(rp)$note))) }) })

cat("report options: titles, file names, sections and shared axes\n")
local({ source(file.path(CORE_DIR, "report_options.R"), local = TRUE)
  o <- report_opts(list(title = "{name} ground temperature, {from} to {to}", file = "{name}_report", author = "Field team", sections = c("qc"), axis = "bogus"))
  check("titles and file names take the name and dates; blank means the app's own; an unknown axis choice is the same scale",
        report_title(o, "x", name = "Plot 1", from = "2020-01-01", to = "2021-01-01") == "Plot 1 ground temperature, 2020-01-01 to 2021-01-01" &&
        report_title(report_opts(), "Default", name = "P") == "Default" && report_file(o, "x", name = "Plot 1") == safe_name("Plot 1_report") && o$axis == "same" &&
        grepl("Prepared by Field team", report_subtitle(o, "base")))
  check("sections: only those chosen; none chosen means none; no choice at all means every one",
        report_wants(o, "qc") && !report_wants(o, "info") && !report_wants(report_opts(list(sections = character())), "qc") && report_wants(report_opts(), "anything"))
  x <- setnames(data.table(unit = c("A", "A", "B", "B"), k = "t", value = c(0, 10, 5, 7)), "k", "key")
  s1 <- axis_limits(x, "same", pad = 0); s2 <- axis_limits(x, "span", pad = 0)
  check("the same scale spans every site; the same span keeps each site's middle with the widest range; free gives none",
        all(s1$lo == 0 & s1$hi == 10) && isTRUE(all.equal(s2[unit == "B", c(lo, hi)], c(1, 11))) && is.null(axis_limits(x, "free"))) })

cat("calibration options\n")
local({
  ct <- data.table(app = "X", value = c("a", "m"), label = c("A", "M"), unit = "", default_number = c("1.26", "auto"), default_source = c("Lit 1972", "Manual"),
                   scotty_number = c("0.69", ""), scotty_source = c("Scotty 2015", ""), allowed = c("", "auto;none"), notes = "")
  d0 <- calibration_default(ct)
  check("the starter table has one default row per value", identical(d0$value, c("a", "m")) && all(d0$choice == "default") && all(d0$applies_to == "*"))
  check("with no rows, or only default rows, the catalogue default is used", calib_get(NULL, ct, "a")$number == 1.26 && calib_get(d0, ct, "a", "S1")$choice == "default")
  cal <- rbind(d0, data.table(app = "X", value = c("a", "a", "a", "m", "m"), applies_to = c("*", "S1", "S2", "*", "S3"), choice = c("scotty_creek", "custom", "custom", "scotty_creek", "custom"),
                              number = c("", "0.8", "0.9", "", "off"), source = c("", "my plot", "", "", "x"), notes = ""))
  check("a scotty_creek row takes the published value and source", identical(calib_get(cal, ct, "a", "S9")[c("number", "choice", "source")], list(number = 0.69, choice = "scotty_creek", source = "Scotty 2015")))
  check("a site's custom row wins over *", calib_get(cal, ct, "a", "S1")$number == 0.8)
  g2 <- calib_get(cal, ct, "a", "S2")
  check("a custom row without a source is still used (a source is encouraged, not required)", g2$choice == "custom" && g2$number == 0.9 && g2$source == "no source given" && !nzchar(g2$note))
  g3 <- calib_get(cal, ct, "m", "S3")
  check("a value outside the allowed list, or scotty_creek where none is published, falls back to the default", g3$value == "auto" && g3$choice == "default" && grepl("not one of", g3$note))
  check("calib_by gives one value per name", identical(calib_by(cal, ct, "a", c("S1", "S9")), c(S1 = 0.8, S9 = 0.69)))
})

cat("\n.hobo records read directly (hobo.R)\n")
local({ source(file.path(CORE_DIR, "hobo.R"), local = TRUE)
  tlv <- function(tag, v) c(as.raw(0x88), as.raw(tag), as.raw(length(v)), v)
  f32 <- function(x) writeBin(x, raw(), size = 4, endian = "big")
  cal <- c(0.826, 0.00346, 0.001584, 2.8e-5, 2.65e-9, 3.87e-7)
  hdr <- c(charToRaw("HOBO"), tlv(0x05, charToRaw("HOBO U20-001-04 Water Level")), tlv(0x06, charToRaw("10279609")),
           tlv(0x07, as.raw(c(20, 21, 8, 27, 10, 19, 53, 0))), tlv(0x08, as.raw(c(0, 0, 7, 8))),
           tlv(0x22, as.raw(c(1, 0, 0, 16, 40, 187, 219, 57, 0, 178, 39))), tlv(0x31, f32(cal)))
  bits <- function(v, n) rev(as.integer(intToBits(v))[1:n])
  rec <- c(bits(9305, 14), bits(742, 10))                            # 2.41 C, 15.81 psi
  ev <- function(type, secs, n) c(rep(1L, 14), bits(type, 4), bits(n, 4), if (n) bits(secs, 4 * n))
  st <- c(rep(1L, 16), rep(rec, 6), ev(1, 5, 1), ev(6, 0, 0), ev(0, 34, 2), rep(rec, 6), ev(3, 63, 2), rec, rep(1L, 40))   # 6 records, a download, 6 more, stopped, old memory
  st <- c(st, rep(1L, (-length(st)) %% 8))
  f <- tempfile(fileext = ".hobo"); writeBin(c(hdr, packBits(as.integer(matrix(st, 8)[8:1, ]), "raw")), f)
  d <- hobo_decode(f)
  check("a U20 .hobo is read as a bit stream: launch + start delay + i x interval, events (14 one bits, type, length, seconds) skipped and take no slot, stopped ends it",
        nrow(d) == 12 && identical(format(d$TIMESTAMP[1], "%Y-%m-%d %H:%M"), "2021-08-27 23:00") && identical(format(d$TIMESTAMP[12], "%Y-%m-%d %H:%M"), "2021-08-28 04:30") && !attr(d, "stopped"))
  check("its temperature and pressure follow the logger's calibration (Steinhart-Hart; psi to kPa)",
        abs(d$temp_c[2] - 2.4099) < 0.001 && abs(d$pres_kpa[2] / HOBO_PSI_KPA - 15.81) < 0.05 && attr(d, "serial") == "10279609")
  writeBin(charToRaw("not a hobo file"), f); check("a file that is not a .hobo, or another model, gives NULL (its CSV export is needed)", is.null(hobo_decode(f)))
  # a real U20 file with events mid-file (a download: coupler attached and detached, host connected), then stopped, against its
  # HOBOware export (serial neutralized): events skipped bit for bit, every record and value as HOBOware gives it, old memory left out
  ef <- file.path(CORE_DIR, "tests", "data", "hobo_events__10000007.hobo"); e <- hobo_decode(ef)
  x <- fread(file.path(CORE_DIR, "tests", "data", "hobo_events__10000007.csv"), skip = 1, header = TRUE, select = 1:4, col.names = c("n", "time", "psi", "degF"))[!is.na(psi)]
  check("a .hobo with seven events mid-file and a stop: all 1,498 records, every pressure and temperature as HOBOware exports them, on its 30-minute grid",
        nrow(e) == nrow(x) && nrow(x) == 1498L && max(abs(e$pres_kpa / HOBO_PSI_KPA - x$psi)) < 2e-4 && max(abs(e$temp_c * 1.8 + 32 - x$degF)) < 2e-3 &&
        all(diff(as.numeric(e$TIMESTAMP)) == 1800) && !attr(e, "stopped"))
  # a real U20 file against its HOBOware export (WLR's sample): the launch record (packed apart) and the last record (end mark in bit 0)
  hs <- file.path(dirname(CORE_DIR), "WLR", "tests", "samples", "hobo_decoded__10000006.hobo")
  if (file.exists(hs)) { r <- hobo_decode(hs)
    check("a real U20 .hobo: first and last pressure as HOBOware exports them (14.3473 psi = 98.92 kPa, 16.1953 psi = 111.66 kPa), all 1,564 records",
          nrow(r) == 1564L && abs(r$pres_kpa[1] - 14.3473 * HOBO_PSI_KPA) < 0.01 && abs(r$pres_kpa[nrow(r)] - 16.1953 * HOBO_PSI_KPA) < 0.01) }
})


cat("reader samples: times compared to the millisecond (testkit.R)\n")
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE)
  x <- data.table(t = as.POSIXct("2024-09-21 18:15:29", tz = "UTC"), v = 1); y <- copy(x)[, t := t + 18]
  check("an 18 s shift in a time column is a different result (all.equal alone let it pass)", isTRUE(all.equal(x, y, check.attributes = FALSE)) && !sample_times_same(x, y))
  check("the same times, nested in a list, and a 0.4 ms difference, are the same", sample_times_same(list(meta = list(a = 1), d = x), list(meta = list(a = 1), d = copy(x))) &&
        sample_times_same(x, copy(x)[, t := t + 4e-4]))
})

cat("corrections over a period (periods.R)\n")
local({ source(file.path(CORE_DIR, "periods.R"), local = TRUE)
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  fwrite(data.table(target = c("A", "", "A", "A", "A", "B", "A", "A", "A"), parameter = c("x", "x", "x", "x", "y", "x", "x", "x", "x"),
    from = c("2020-01-01 02:00", "2020-01-01 05:00", "2020-01-01", "2020-01-01 08:00", "2020-01-01", "2020-01-01", "2020-01-01", "nonsense", "2020-01-01"),
    to = c("2020-01-01 03:00", "2020-01-01 06:00", "", "2020-01-01 09:00", "2020-01-01", "2020-01-01", "2020-01-01", "", "2020-01-01"),
    action = c("Remove data", "interpolate", "Offset", "Linear drift", "flag", "scale", "offset", "remove", "explode"),
    value = c("", "", "1", "2", "", "10", "5", "", ""), on = c("on", "on", "on", "on", "on", "on", "off", "on", "on"), note = "", author = "", added = ""), f)
  r <- period_read(f)
  check("rows switched off are kept out; a bad date or an unknown action is a problem, not a stop", nrow(r$rows) == 6 && identical(r$bad$row, c(8L, 9L)))
  dt <- data.table(TIMESTAMP = t("2020-01-01") + 3600 * (0:10), x = as.numeric(0:10), y = 1)
  o <- period_apply(dt, r$rows, keys = "A")
  check("remove blanks the period; offset adds; the target limits a row to its record (B's scale is not applied)",
        all(is.na(o$dt$x[3:4])) && o$dt$x[1] == 1 && o$dt$x[11] == 11)
  check("fill by interpolation is a straight line in time between the values either side (applied in the list's order)", isTRUE(all.equal(o$dt$x[6:7], c(6, 7))))
  check("linear drift grows from none at from to the whole value at to", isTRUE(all.equal(o$dt$x[9:10], c(9, 12))))
  check("flag keeps the values and returns them for the app to flag; the log counts each correction", !is.null(o$flagged) && nrow(o$flagged) == 11 && all(o$dt$y == 1) && nrow(o$log) == 5)
  o2 <- period_apply(dt, r$rows, keys = "A", only = "y")
  check("a run in steps applies a row only to the step's columns", isTRUE(all.equal(o2$dt$x, dt$x)))
  check("period_for keeps the rows that can touch a record (blank target, or one of its names)", nrow(period_for(r$rows, c("a"))) == 5)
  g <- tempfile(fileext = ".csv"); on.exit(unlink(g), add = TRUE)
  fwrite(data.table(target = c("A|B", "*", "L2", "A"), parameter = "x", from = "", to = "", action = c("offset", "scale", "offset", "remove"), value = c("1", "2", "100", ""),
                    on = "on", note = "", author = "", added = "", output = c("", "", "", ""), stream = ""), g)
  r2 <- period_read(g, skip = "remove"); o3 <- period_apply(data.table(TIMESTAMP = t("2020-01-01") + 0:1, x = c(1, 1), lg = c("L1", "L2")), r2$rows, keys = "B", by = "lg")
  check("a target lists names with |, * is every record; by: a row for one logger changes only its records; skipped actions stay out",
        nrow(r2$rows) == 3 && "stream" %in% names(r2$rows) && identical(o3$dt$x, c(4, 104)))
  r3 <- period_read_rows(data.table(target = "", parameter = "x", from = "", to = "", action = "scale", value = "2", on = "on", note = "", author = "", added = "", output = "x2"))
  o4 <- period_apply(data.table(TIMESTAMP = t("2020-01-01"), x = 3), r3$rows)
  check("an output column takes the corrected values and the column itself is left as it was", o4$dt$x == 3 && o4$dt$x2 == 6)
  own <- period_own_rows(g, "remove")
  check("the rows an app applies itself come as written, with their row in the file", nrow(own) == 1 && own$row == 4L && own$target == "A")
})

cat("the Workbench sheet holds values only (sheet.R)\n")
local({ sj <- paste(readLines(file.path(CORE_DIR, "www", "sheet.js"), warn = FALSE), collapse = "\n"); sr <- paste(readLines(file.path(CORE_DIR, "sheet.R"), warn = FALSE), collapse = "\n")
  check("no formula parser and nothing kept in config/formulas: a value typed with = stays text",
        !grepl("evalF|isF\\(|formulas:", sj) && !grepl("sheet_formula", sr) && !grepl("formulas", paste(readLines(file.path(CORE_DIR, "projects.R"), warn = FALSE), collapse = "\n")))
})

cat("the project folder when none is set (util.R)\n")
local({ d <- ovid_default_project("CS"); repo <- tempfile("repo_"); dir.create(file.path(repo, ".git"), recursive = TRUE)
  check("no project folder: ~/Projects/Ovid/Projects/<app>/project", identical(ovid_project_dir("", "CS", repo), d) && identical(ovid_project_dir(NA, "CS", repo), d))
  check("\"/\" and the home folder are never used as the project folder", identical(ovid_project_dir("/", "CS", repo), d) && identical(ovid_project_dir("~", "CS", repo), d))
  check("a folder inside the app's own git repository is not used", identical(ovid_project_dir(file.path(repo, "sub", "proj"), "CS", repo), d))
  check("a folder the user sets wins", identical(ovid_project_dir("~/elsewhere/my project", "CS", repo), "~/elsewhere/my project"))
  check("the repository is found from any folder inside it", identical(ovid_repo_root(file.path(repo, "a", "b")), norm_any(repo)))
  unlink(repo, recursive = TRUE) })
local({ r <- tempfile("projects_"); dir.create(file.path(r, "ERT"), recursive = TRUE)
  check("a renamed app: a former name reads as the current one (ERT is GS); others unchanged", identical(ovid_app_name(c("ERT", "GS", "CS")), c("GS", "GS", "CS")))
  check("a renamed app keeps its former projects folder until one with the new name exists",
        identical(ovid_projects_home("GS", r), file.path(r, "ERT")) && identical(ovid_projects_home("CS", r), file.path(r, "CS")) &&
        { dir.create(file.path(r, "GS")); identical(ovid_projects_home("GS", r), file.path(r, "GS")) })
  unlink(r, recursive = TRUE) })


cat("numbers written as text (convert.R num_cells)\n")
local({ unread_take()
  v <- num_cells(c("1.5", " 2 ", "", NA, "NAN", "-INF", "null", "12,5", "abc"))
  u <- unread_take()
  check("numbers are read; blanks and missing-value words quietly give NA (INF stays infinite, as before)", identical(v[1:2], c(1.5, 2)) && all(is.na(v[c(3:5, 7:9)])) && identical(v[6], -Inf))
  check("cells that are not numbers are counted, with the first as an example", u$n == 2L && identical(u$example, "12,5"))
  r <- list(meta = list(serial = "X")); num_cells(c("1", "?")); r <- unread_mark(r)
  check("the count goes on the reader's result and is reset", identical(r$meta$unreadable, 1L) && identical(r$meta$unreadable_example, "?") && unread_take()$n == 0L)
  r2 <- unread_mark(list(meta = list(serial = "Y")))
  check("a file with no such cells has no count", is.null(r2$meta$unreadable))
  n0 <- length(AUDIT$problems); unread_report(r$meta, "a.csv")
  check("the file is named on the problem list", length(AUDIT$problems) == n0 + 1L && grepl("1 value(s) are not numbers", AUDIT$problems[[n0 + 1L]]$problem, fixed = TRUE)) })

cat("the logger list grouped (grouping.R)\n")
local({ source(file.path(CORE_DIR, "grouping.R"), local = TRUE)
  f <- data.table(serial = c("A", "A", "A", "A", "B"), file = c("a1", "a2", "a3", "a4", "b1"), first = c("2020-01-01", "2020-03-01", "2020-06-01", "2020-06-01 12:00", "2020-01-01"),
                  last = c("2020-02-28", "2020-04-15", "2020-08-01", "2020-08-01", "2020-12-31"))
  check("by serial number: one row per logger, all its files", identical(group_index(f, "serial")$n, c(4L, 1L)))
  check("by download: the files of a serial that end the same day are one download (split downloads joined)", nrow(group_index(f, "download")) == 4 && max(group_index(f, "download")$n) == 2)
  check("by raw file: a row per file", nrow(group_index(f, "file")) == 5)
  dep <- data.table(serial = "A", from = c("2020-01-01", "2020-05-20"), to = c("2020-04-30", ""), name = c("Bog", "Fen"))
  g <- group_index(f, "deployment", dep)
  check("by deployment: a redeployed serial is cut at its deployment rows, each stay named by its site", nrow(g[serial == "A"]) == 2 && all(grepl("Bog|Fen", g[serial == "A"]$label)))
  sg <- group_split_suggestions(f)
  check("a long gap in a serial's record is a suggested split (Looks like it was moved)", nrow(sg) == 1 && sg$serial == "A" && grepl("^2020-06-01", sg$at) && grepl("Split here", sg$what))
  check("an accepted split cuts the serial when it has no deployment rows, and is no longer suggested",
        nrow(group_index(f, "deployment", splits = data.table(serial = "A", at = "2020-06-01"))[serial == "A"]) == 2 && !nrow(group_split_suggestions(f, data.table(serial = "A", at = "2020-06-01"))))
  nodates <- data.table(serial = "C", file = "c1", first = "", last = "")
  w <- NULL; gi <- withCallingHandlers(group_index(nodates, "serial"), warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  check("a logger with no dates is listed with no first and last, without a warning", nrow(gi) == 1 && is.na(gi$first) && is.na(gi$last) && is.null(w))
  check("a list with no loggers still has the key column", identical(names(group_index(nodates[0], "serial"))[1], "key"))
  pd <- tempfile("gbproj"); dir.create(file.path(pd, "config"), recursive = TRUE); group_by_write(pd, "download")
  check("the choice is kept per project (config/view.json); serial number when none is kept", identical(group_by_read(pd), "download") && identical(group_by_read(tempfile()), "serial"))
})

cat("projects: rename and duplicate (projects.R)\n")
local({ for (.f in c("cache", "manifest", "projects")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE); APP_NAME <- "Ovid Core // TEST"
  root <- tempfile("dp"); dir.create(root); a <- file.path(root, "Alpha"); project_new(a, "Alpha"); b <- file.path(root, "Beta"); project_new(b, "Beta")
  writeLines("x,y\n1,2", file.path(a, "config", "qc_rules.csv")); dir.create(file.path(a, "output")); writeLines("out", file.path(a, "output", "o.csv"))
  check("a project name: not empty, no / \\ or :, not one already used", nzchar(project_name_problem("", c(a, b))) && nzchar(project_name_problem("a/b")) && nzchar(project_name_problem("a:b")) &&
        nzchar(project_name_problem("Beta", c(a, b), a)) && !nzchar(project_name_problem("Gamma", c(a, b), a)))
  r <- project_rename(a, "Alpha 2", c(a, b))
  check("outside the projects folder a rename changes the name shown, not the folder", identical(path_norm(r), path_norm(a)) && identical(project_name(a), "Alpha 2"))
  e <- tryCatch(project_rename(a, "Beta", c(a, b)), error = function(e) conditionMessage(e)); check("a rename to a name in use is refused, saying why", grepl("already", e))
  d <- project_duplicate(a, "Copy", c(a, b), dest = file.path(root, "Copy"))
  check("a duplicate has the set-up tables, not the outputs", file.exists(file.path(d, "config", "qc_rules.csv")) && !dir.exists(file.path(d, "output")) && identical(project_name(d), "Copy"))
  h0 <- Sys.getenv("HOME"); h <- tempfile("home"); dir.create(h); Sys.setenv(HOME = h); on.exit(Sys.setenv(HOME = h0), add = TRUE)
  p <- file.path(projects_root(), "Old"); dir.create(dirname(p), recursive = TRUE); project_new(p, "Old"); r <- project_rename(p, "New name", p)
  check("the open project is listed when the saved settings have no folder yet (first start, automatic folder); a saved one is kept",
        identical(projects_known(projects_with_open(list(), list(project_dir = "/x/Untitled"))), "/x/Untitled") &&
        identical(projects_with_open(list(project_dir = "/a"), list(project_dir = "/b"))$project_dir, "/a") && !length(projects_known(projects_with_open(list(), list()))))
  check("in the projects folder a rename renames the folder too", dir.exists(r) && basename(r) == "New name" && !dir.exists(p) && identical(project_name(r), "New name"))
  Sys.setenv(HOME = h0); unlink(c(root, h), recursive = TRUE) })

cat("projects: settings of an older version, and opening a project's settings (projects.R)\n")
local({ for (.f in c("cache", "manifest", "projects")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  check("an old FSL folder (fsl_dir) becomes the logger programs folder when that is empty, and never overwrites it",
        identical(project_settings_upgrade(list(fsl_dir = "~/fsl", prog_dir = ""))$prog_dir, "~/fsl") && identical(project_settings_upgrade(list(fsl_dir = "~/fsl", prog_dir = "~/p"))$prog_dir, "~/p"))
  pd <- tempfile("old"); dir.create(file.path(pd, "config"), recursive = TRUE); writeLines('{"fsl_dir": "~/fsl"}', file.path(pd, "config", "project_settings.json"))
  check("... also when the project's settings are read", identical(project_settings_read(pd)$prog_dir, "~/fsl"))
  cfg <- project_apply_settings(list(project_dir = "/a", viewer_title = "Old", base_offset_h = -7, raw_dirs = "x"),
                                list(project_dir = "/b", viewer_title = "", base_offset_h = -6, flight_gap_min = 5, projects = list("/a"), ui = "workbench"), "/b")
  check("opening a project takes all its settings (a blank one keeps the app's), not only its folder",
        cfg$project_dir == "/b" && cfg$base_offset_h == -6 && cfg$flight_gap_min == 5 && cfg$viewer_title == "Old" && is.null(cfg$projects) && is.null(cfg$ui)) })

cat("configuration import: replace or add rows (config.R)\n")
local({ CONFIG_FILES <<- list(t = list(file = "t.csv", cols = c("k", "v"))); pd <- tempfile("cf"); dir.create(file.path(pd, "config"), recursive = TRUE)
  fwrite(data.table(k = c("a", "b"), v = c("1", "2")), file.path(pd, "config", "t.csv")); new <- data.table(k = "a", v = "9")
  check("an import replaces a table, or adds its rows (merge); there is no third mode", nrow(config_after("t", new, pd)$after) == 1 && nrow(config_after("t", new, pd, "merge")$after) == 3 &&
        !"by" %in% names(formals(apply_config_import)) && !exists("config_update"))
  rm(CONFIG_FILES, envir = globalenv()) })

cat("settings as plain JSON (util.R, projects.R)\n")
local({
  for (.f in c("cache", "manifest", "projects")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  root <- tempfile("st")
  dir.create(root)
  x <- list(
    raw_dirs = c("/a", "/b"), skip_tables = character(), project_dir = "/p", base_offset_h = -7,
    unzoned_offset_h = NA_real_, incremental = TRUE, typed = list(site = c("North", "South")),
    report_opts = list(title = "", sections = character())
  )
  f <- file.path(root, "app_settings.json")
  settings_save(f, x)
  y <- settings_read(f)
  check("settings are saved as JSON that reads back the same (folders, empty lists, numbers, missing, true/false)",
        identical(y$raw_dirs, x$raw_dirs) && identical(y$skip_tables, character()) && identical(y$base_offset_h, -7) &&
          is.na(y$unzoned_offset_h) && isTRUE(y$incremental) && identical(y$typed$site, c("North", "South")) &&
          identical(y$report_opts$sections, character()) &&
          isTRUE(jsonlite::validate(paste(readLines(f), collapse = "\n"))))
  g <- file.path(root, "old", "app_settings.json")
  dir.create(dirname(g))
  saveRDS(list(project_dir = "/old", colour_scheme = "Nord"), sub("json$", "rds", g))
  z1 <- settings_read(g)
  settings_save(g, list(colour_scheme = "Ovid"))
  z2 <- jsonlite::read_json(g)
  check("settings an older version saved (.rds) are read, and the next save writes them all to the .json",
        identical(z1$project_dir, "/old") && identical(z2$project_dir, "/old") && identical(z2$colour_scheme, "Ovid"))
  pd <- file.path(root, "proj")
  dir.create(pd)
  project_settings_save(pd, list(raw_dirs = "/r", depth_positive = "down", ui = "workbench"))
  check("a project's own settings are kept as JSON in config/, without the app-wide ones",
        file.exists(file.path(pd, "config", "project_settings.json")) &&
          identical(project_settings_read(pd)$depth_positive, "down") && is.null(project_settings_read(pd)$ui))
  unlink(root, recursive = TRUE)
})

# the Workbench tables: filters, sorting and fill down in the sheet; sort, filter, resize and reorder in the data grid
if (requireNamespace("V8", quietly = TRUE)) local({
  js_of <- function(f, nm) { for (x in parse(file.path(CORE_DIR, f))) if (is.call(x) && identical(as.character(x[[2]]), nm)) return(eval(x[[3]])); "" }
  sj <- js_of("sheet.R", "SHEET_JS"); fn <- function(n) regmatches(sj, regexpr(paste0("function ", n, "\\(.*?\n  function"), sj))
  cx <- V8::v8(); cx$eval(sub("\n  function$", "", fn("isNum"))); cx$eval(sub("\n  function$", "", fn("ftest"))); cx$eval(sub("\n  function$", "", fn("fmtv")))
  f <- function(q, v) cx$call("ftest", q, v)
  check("a sheet filter reads >, <=, a..b, =, != and plain text", f(">5", "6") && !f(">5", "5") && f("<=0", "-1") && f("1..3", "2") && !f("1..3", "4") && f("=2", "2.0") && f("!=0", "1") && f("bar", "Baro") && !f(">5", "abc"))
  check("a number format shows the decimals asked for and leaves text alone", cx$call("fmtv", list(fmt = list(`0` = "2")), 0, "3.14159") == "3.14" && cx$call("fmtv", list(fmt = list(`0` = "2")), 0, "all") == "all")
  check("the sheet has fill down (Cmd/Ctrl+D, undoable), Replace all, Export, row height and saved widths",
        all(vapply(c("function fillDown", "function replaceAll", "function exportCsv", "'rh'", "ovid.sheet.", "k.toLowerCase() === 'd'", "ds-frow", "ds-rsz"), grepl, NA, sj, fixed = TRUE)))
  cx$eval(sub("\n  function$", "", fn("badCh")))
  check("a sheet column with set choices takes them in any case and marks anything else; blanks and free text are left alone",
        !cx$call("badCh", list(ch = list("shallow", "deep")), "Deep") && cx$call("badCh", list(ch = list("shallow", "deep")), "medium") &&
        !cx$call("badCh", list(ch = list("shallow", "deep")), "") && !cx$call("badCh", list(l = "notes"), "anything") &&
        grepl("nBad(T) > 0) return", sj, fixed = TRUE))
  if (requireNamespace("shiny", quietly = TRUE)) local({ e <- new.env(); for (x in parse(file.path(CORE_DIR, "sheet.R"))) if (is.call(x) && as.character(x[[2]]) %in% c("sheet_choices")) eval(x, e)
    CONFIG_FILES <- list(s = list(file = "sites.csv", rules = list(choices = list(profile = c("shallow", "deep", NA), missing = "x")))); environment(e$sheet_choices) <- environment()
    check("the sheet takes a column's choices from the app's table rules, only for columns the table has", identical(e$sheet_choices("sites.csv", c("site", "profile")), list(profile = c("shallow", "deep"))) &&
          identical(e$sheet_choices("other.csv", "profile"), list())) })
  wj <- js_of("workbench.R", "WB_JS")
  check("the data grid sorts, filters, resizes, autofits and reorders, and the chart/table bar is kept per app",
        all(vapply(c("'.wb-sort'", "'.wb-gfilt'", "'.wb-rsz'", "'dragstart', '.wb-grip'", "'.wb-split'", "ovid.grid.", "chart_h"), grepl, NA, wj, fixed = TRUE)))
  if (requireNamespace("shiny", quietly = TRUE)) { suppressMessages(library(shiny)); e <- new.env(); for (x in parse(file.path(CORE_DIR, "workbench.R"))) if (is.call(x) && identical(x[[1]], as.name("<-")) && as.character(x[[2]]) %in% c("wb_grid", "wb_unit_html")) eval(x, e)
    g <- tryCatch(as.character(e$wb_grid(c("a", "b"), c("x", "y"), list(c("1", "2"), c("3", "4")), list(c("", ""), c("", "")), "s")), error = function(err) conditionMessage(err))
  check("every Workbench script parses (grid, chart, sheet)", all(vapply(c("WB_JS", "WB_CHART_JS", "WB_XY_JS"), function(n) { v <- js_of("workbench.R", n); nzchar(v) && isTRUE(tryCatch({ V8::v8()$eval(paste0("(function(){ return function(){", v, "}; })")); TRUE }, error = function(e) FALSE)) }, NA)) &&
        isTRUE(tryCatch({ V8::v8()$eval(paste0("(function(){ return function(){", sj, "}; })")); TRUE }, error = function(e) FALSE)))
  check("the grid shows or hides columns, sets decimals, wraps, and sums the selection; the sheet wraps and keeps its filter row under the heading",
        all(vapply(c("'hidden'", "'fmt'", "function fmtCell", "'wrap'", "function stats", "'count ' + n"), grepl, NA, wj, fixed = TRUE)) &&
        all(vapply(c("'wrap'", "ds-wrap", "x.style.top = hh.offsetHeight"), grepl, NA, sj, fixed = TRUE)))
    check("each grid heading has a sort, a filter, a resize edge and a grip", lengths(regmatches(g, gregexpr("wb-gfilt", g))) == 2 && lengths(regmatches(g, gregexpr("wb-sort", g))) == 2 && lengths(regmatches(g, gregexpr("wb-rsz", g))) == 2 && lengths(regmatches(g, gregexpr("wb-grip", g))) == 2 && grepl('class="gf" data-c="2"', g, fixed = TRUE)) } })

cat("conversions (convert.R)\n")
local({
  # reference eastings/northings (WGS84 UTM), one per hemisphere and side of the central meridian: from PROJ, the first from an
  # independent Krüger-series computation (Karney 2011)
  ref <- data.table(lat = c(60.1031, 45.0, -33.8688), lon = c(-120.1082, -75.5, 151.2093), zone = c(10, 18, 56),
                    e = c(660761.422, 460592.351, 334368.634), n = c(6666411.462, 4983071.988, 6250948.345))
  u <- base::t(vapply(seq_len(nrow(ref)), function(i) ll_to_utm(ref$lon[i], ref$lat[i], ref$zone[i])[1, ], c(easting = 0, northing = 0)))
  check("lat/lon to UTM matches PROJ within 5 cm (Snyder 1987 series)", all(abs(u[, "easting"] - ref$e) < 0.05) && all(abs(u[, "northing"] - ref$n) < 0.05))
  check("lat/lon to UTM picks the standard zone when none is given", abs(ll_to_utm(-120.1082, 60.1031)[1, "easting"] - 660761.422) < 0.05)
  b <- base::t(vapply(seq_len(nrow(ref)), function(i) utm_to_ll(ref$e[i], ref$n[i], paste0(ref$zone[i], if (ref$lat[i] < 0) "S" else "N"))[1, ], c(lat = 0, lon = 0)))
  check("UTM back to lat/lon lands within 1e-6 degrees (about 0.1 m)", all(abs(b[, "lat"] - ref$lat) < 1e-6) && all(abs(b[, "lon"] - ref$lon) < 1e-6))
  four <- utm_to_ll(c(660000, 661000, 661000, 660000), c(6666000, 6666000, 6667000, 6667000), 10)
  check("UTM to lat/lon converts every point of a vector with one zone", nrow(four) == 4 && four[3, "lat"] > four[1, "lat"] && four[2, "lon"] > four[1, "lon"])
  check("UTM to lat/lon with no zone is an error, not a guess", inherits(tryCatch(utm_to_ll(660761, 6666411, NA), error = function(e) e), "error"))
  check("degrees-minutes-seconds and decimal degrees are read", isTRUE(all.equal(dms_to_dd(c("60 06 11 N", "120\u00b0 06.5' W", "-120.1082", "60,5", "x")),
        c(60 + 6 / 60 + 11 / 3600, -(120 + 6.5 / 60), -120.1082, 60.5, NA))))
  check("decimal degrees are written as degrees-minutes-seconds and read back", abs(dms_to_dd(dd_to_dms(-120.1082, "lon")) + 120.1082) < 1e-4 &&
        grepl("W$", dd_to_dms(-120.1082, "lon")))
  check("bad coordinates give NA: minutes or seconds of 60 or more, beyond 180 degrees; a leading S reads as south",
        isTRUE(all.equal(dms_to_dd(c("60 61 0", "60 6 75", "200", "S 33 52")), c(NA, NA, NA, -(33 + 52 / 60)))))
  check("seconds that round to 60 carry into the minute", identical(dd_to_dms(59.99999999), "60° 0' 0.0\" N"))
  check("a point outside UTM's range (above 84 N) gives NA, not a number", all(is.na(ll_to_utm(0, 85))))
  check("a UTM zone outside 1 to 60 is an error", inherits(tryCatch(utm_to_ll(500000, 1, 99), error = function(e) e), "error"))
  ref_s <- utm_to_ll(334368.634, 6250948.345, "56S")
  check("a southern zone written as 56S gives the southern point", abs(ref_s[1, "lat"] + 33.8688) < 1e-6 && abs(ref_s[1, "lon"] - 151.2093) < 1e-6)
  check("units: psi, inHg, mbar to kPa; feet to metres; degF and K to degC (NIST SP 811)",
        isTRUE(all.equal(c(convert_unit(1, "psi", "kPa"), convert_unit(1, "inches Hg", "kPa"), convert_unit(1013.25, "mbar", "kPa"),
                           convert_unit(1, "ft", "m"), convert_unit(212, "deg F", "C"), convert_unit(273.15, "K", "degC")),
                         c(6.894757, 3.386389, 101.325, 0.3048, 100, 0))))
  check("converting between two kinds of unit is an error", inherits(tryCatch(convert_unit(1, "kPa", "m"), error = function(e) e), "error"))
  check("time zone labels: UTC, UTC-7, UTC+5:30", identical(c(zone_text(0), zone_text(-7), zone_text(5.5)), c("UTC", "UTC-7", "UTC+5:30")))
})

cat("errors are reported, not hidden (audit.R)\n")
local({ audit_reset(); a <- try_or(stop("no such file"), "fallback", "config/x.csv"); b <- try_or(1 + 1, NULL, "sum")
  p <- problems_since(0)
  check("try_or gives the default on an error and puts it on the problem list with where and why",
        identical(a, "fallback") && nrow(p) == 1 && p$where == "config/x.csv" && p$problem == "no such file")
  check("try_or gives the value when nothing fails, and adds no problem", identical(b, 2) && length(AUDIT$problems) == 1)
  check("problems_since gives only the problems after the first n", nrow(problems_since(1)) == 0 && nrow(problems_since(0)) == 1) })

cat("reader sample tests: accepted and reference values (testkit.R)\n")
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE)
  d <- tempfile("samples"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  writeLines(c("time,v", "2024-01-02 03:04:05,1.5", "2024-01-02 04:04:05,2.5"), file.path(d, "fake__a.csv"))
  rd <- function(p, k) { x <- fread(p); x[, time := as.POSIXct(time, tz = "UTC")]; list(meta = list(kind = k), data = x) }
  r <- check_reader_samples(d, rd, update = FALSE)
  check("a new sample has its expected result saved but is not accepted", is.na(r$matches_expected) && !r$accepted && is.na(r$reference_ok) &&
        file.exists(file.path(d, "expected", "fake__a.csv.rds")))
  accept_sample(d, "fake__a.csv", "tester", "looked at it"); r <- check_reader_samples(d, rd, update = FALSE)
  a <- fread(file.path(d, "accepted.csv"), colClasses = "character")
  check("after accept_sample the sample is accepted, with who, today and the md5", isTRUE(r$matches_expected) && r$accepted && nrow(a) == 1 &&
        a$checked_by == "tester" && a$checked_on == format(Sys.Date()) && identical(names(a), c("file", "expected_md5", "checked_by", "checked_on", "note")))
  r <- check_reader_samples(d, rd, update = TRUE)
  check("saving an unchanged result again keeps it accepted", isTRUE(r$matches_expected) && r$accepted)
  saveRDS(list(changed = TRUE), file.path(d, "expected", "fake__a.csv.rds"), version = 2); r <- check_reader_samples(d, rd, update = FALSE)
  check("an expected result changed after it was accepted is no longer accepted", !r$accepted && isFALSE(r$matches_expected))
  rd2 <- function(p, k) { x <- rd(p, k); x$data$v <- x$data$v * 2; x }; r <- check_reader_samples(d, rd2, update = TRUE)
  check("a result saved again after a reader change is not accepted until checked", is.na(r$matches_expected) && !r$accepted)
  r <- check_reader_samples(d, rd, update = TRUE)
  ref <- function(...) fwrite(data.table(file = "fake__a.csv", ...), file.path(d, "reference.csv"))
  ref(what = c("rows", "first_time", "last_time", "col:v@2"), value = c("2", "2024-01-02 03:04:05", "2024-01-02 04:04:05", "2.5000000001"), source = "raw file")
  check("reference values that match give reference_ok TRUE (times as text, numbers within 1e-6)", isTRUE(check_reader_samples(d, rd, update = FALSE)$reference_ok))
  ref(what = c("rows", "col:v@2"), value = c("2", "2.6"), source = "raw file")
  check("a wrong reference value gives reference_ok FALSE", isFALSE(check_reader_samples(d, rd, update = FALSE)$reference_ok))
  ref(what = "col:nope@1", value = "1", source = "raw file")
  check("a reference to a column the reader does not make gives reference_ok FALSE", isFALSE(check_reader_samples(d, rd, update = FALSE)$reference_ok)) })

cat("one click, one download (shell.R)\n")
if (requireNamespace("shiny", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "shell.R"), local = TRUE)
  sent <- character(); made <- 0L
  srv <- function(input, output, session) {
    session$sendCustomMessage <- function(type, message) sent <<- c(sent, type)
    output$dl <- downloadHandler(function() "x.csv", function(file) { made <<- made + 1L; writeLines("a", file) })
    observeEvent(input$go, once_done("go")) }
  testServer(srv, { f <- output$dl; session$setInputs(go = 1)
    check("the download is made once per request, and the window is told when it is made", file.exists(f) && made == 1L && "ovid-dl-done" %in% sent)
    check("a d-once button is turned back on when its action ends", "ovid-once-done" %in% sent) }) })

cat("the Convert page (convert_page.R)\n")
if (requireNamespace("shiny", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "convert_page.R"), local = TRUE)
  txt <- function(o) gsub("&#39;", "'", gsub("&quot;", "\"", as.character(o$html)))
  testServer(convert_server, {
    session$setInputs(cv_value = 1, cv_from = "psi", cv_to = "kPa")
    check("1 psi to kPa is 6.894757", grepl("1 psi = 6.894757 kPa", txt(output$cv_unit_out), fixed = TRUE))
    session$setInputs(cv_to = "m")
    check("two kinds of unit give the message", grepl("Pick two units of the same kind", txt(output$cv_unit_out)))
    session$setInputs(cv_mode = "ll2utm", cv_lat = "60.1031", cv_lon = "-120.1082")
    o <- txt(output$cv_coord_out)
    check("lat/long to UTM: 60.1031, -120.1082 is zone 10N E 660761.42 N 6666411.46, with both forms",
          grepl("Zone 10N", o) && grepl("E 660761.42", o) && grepl("N 6666411.46", o) && grepl("Decimal", o) && grepl("DMS", o))
    session$setInputs(cv_mode = "utm2ll", cv_zone = "10N", cv_e = 660761.42, cv_n = 6666411.46)
    check("UTM to lat/long comes back to 60.103100, -120.108200", grepl("60.103100, -120.108200", txt(output$cv_coord_out), fixed = TRUE))
    session$setInputs(cv_mode = "dms", cv_lat = "60 6 11 N", cv_lon = "120 6 29.5 W")
    o <- txt(output$cv_coord_out)
    check("DMS typed in shows decimal and DMS", grepl("60.103056, -120.108194", o, fixed = TRUE) && grepl("60° 6' 11.0\" N", o, fixed = TRUE))
    session$setInputs(cv_lat = "95")
    check("latitude 95 gives the range message", grepl("Latitude must be within", txt(output$cv_coord_out)))
    session$setInputs(cv_mode = "utm2ll", cv_zone = "99")
    check("zone 99 gives the zone message", grepl("Type a zone \\(1 to 60\\), easting and northing", txt(output$cv_coord_out)))
  }) })
cat("\nthe shared time-series plot (tsview.R)\n")
if (requireNamespace("shiny", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "tsview.R"), local = TRUE)
  sent <- list(); s <- list(sendCustomMessage = function(type, msg) sent[[length(sent) + 1]] <<- msg)
  t <- as.POSIXct("2022-01-01", tz = "UTC") + 1800 * 0:9
  ts_view_send(s, "p", t, list(list(name = "a", v = 1:10), list(name = "b", v = 11:20)), same_scale = TRUE)
  ts_view_send(s, "p", t, list(list(name = "a", v = 1:10)), focus = t[5])
  check("same_scale reaches the browser, and is off unless asked for", isTRUE(sent[[1]]$same) && identical(sent[[2]]$same, FALSE))
  check("a focus time is sent as seconds", identical(sent[[2]]$focus, as.numeric(t[5])))
  j <- paste(readLines(file.path(CORE_DIR, "www", "tsview.js"), warn = FALSE), collapse = "\n")
  check("the plot reports the selected stretch as <id>_sel and opens the same data at a new focus",
        grepl("Shiny.setInputValue(this.id + '_sel'", j, fixed = TRUE) && grepl("else if (m.focus != null)", j, fixed = TRUE) && grepl("if (d.same)", j, fixed = TRUE))
  check("drag on the plot selects, the time axis and Space move, and Back returns to the last view (plot standards A11)",
        grepl("!SPACE && !this.onAxis(this.pos(e))", j, fixed = TRUE) && grepl("P.near = function", j, fixed = TRUE) &&
        grepl("data-tsv-go=\"selzoom\"", j, fixed = TRUE) && grepl("P.goBack = function", j, fixed = TRUE))
  ts_view_send(s, "p", t, list(list(name = "a", v = 1:10)), events = data.frame(t = t[3], label = "Field visit", note = "tape 0.51 m"),
               sources = data.frame(from = t[c(6, 1)], label = c("Logger B", "Logger A")), title = "Level", site = "Site", file = "wlr_Site_level")
  m <- sent[[length(sent)]]
  check("events, sources and the save names reach the browser, sources in time order",
        identical(m$events[[1]]$label, "Field visit") && identical(m$events[[1]]$t, as.numeric(t[3])) &&
        identical(vapply(m$sources, `[[`, "", "label"), c("Logger A", "Logger B")) && identical(m$file, "wlr_Site_level"))
  check("the plot draws events, shares one cursor, saves images at 85 mm, 180 mm and slide size and saves its data, and has keys (standards A10, A11)",
        grepl("P.saveImage = function", j, fixed = TRUE) && grepl("'85': {w: 340", j, fixed = TRUE) && grepl("P.saveData = function", j, fixed = TRUE) &&
        grepl("function cursor(src, t)", j, fixed = TRUE) && grepl("P.key = function", j, fixed = TRUE) && grepl("d.events || []", j, fixed = TRUE))
  # edits from the plot (WLR and GT Explore): panels shared by several series, dots, shaded spans, page actions, Done with Undo
  ts_view_send(s, "p", t, list(list(name = "0.05 m", key = "0.05", v = 1:10, panel = "all", col = "#d08770", plab = "Temperature, every depth",
                                    dots = list(list(t = t[c(4, 2)], v = c(9, 8), kind = "removed"), list(t = numeric(), v = numeric(), kind = "flagged"))),
                               list(name = "0.5 m", key = "0.5", v = 11:20, panel = "all")),
               spans = data.frame(t0 = t[2], t1 = t[3], label = "out of the water"),
               actions = list(list(id = "box", label = "Remove the points in the band", band = TRUE)), ask = list(choices = c("retrieval", "other")))
  m <- sent[[length(sent)]]
  check("series name their panel and colour; dots go in time order and empty ones are dropped; spans, actions and the form reach the browser",
        identical(m$series[[1]]$panel, "all") && identical(m$series[[1]]$col, "#d08770") && length(m$series[[1]]$dots) == 1L &&
        identical(as.numeric(m$series[[1]]$dots[[1]]$t), as.numeric(t[c(2, 4)])) && identical(as.numeric(m$series[[1]]$dots[[1]]$v), c(8, 9)) &&
        identical(m$spans[[1]]$label, "out of the water") && identical(m$actions[[1]]$id, "box") && identical(m$ask$choices, c("retrieval", "other")))
  ss <- list(sendCustomMessage = function(type, msg) sent[[length(sent) + 1]] <<- c(type = type, msg), userData = new.env())
  ts_view_done(ss, "p", "Removal added."); d1 <- sent[[length(sent)]]
  ss$userData$ovid_undo <- list(at = Sys.time(), file = "manual_removals.csv", fn = function() TRUE); ts_view_done(ss, "p", "Removal added."); d2 <- sent[[length(sent)]]
  check("Done offers Undo only while the set-up table kept the version before", identical(d1$type, "tsv-done") && identical(d1$undo, FALSE) && isTRUE(d2$undo))
  check("the plot narrows a selection to a value band on a panel's value axis and sends page actions as <id>_act, with Undo as <id>_undo",
        grepl("P.bandDrag = function", j, fixed = TRUE) && grepl("Shiny.setInputValue(this.id + '_act'", j, fixed = TRUE) && grepl("el.id + '_undo'", j, fixed = TRUE) &&
        grepl("m.pn = pn", j, fixed = TRUE) && grepl("d.spans.forEach", j, fixed = TRUE)) })

if (requireNamespace("DT", quietly = TRUE)) local({ suppressMessages(library(shiny)); source(file.path(CORE_DIR, "table.R"), local = TRUE)   # the core table (plot and table standards A9)
  fmt_ts <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"); ovid_colnames <- function(n) n
  d <- data.frame(time = as.POSIXct("2024-07-01", tz = "UTC") + 1800 * 0:2, level = c(0.5, NA, 0.7), status = c("ok", "flagged", "ok"))
  w <- ovid_dt(d, limits = list(level = c(max = 0.65), nothere = c(min = 0)))
  o <- w$x$options$ovid
  check("ovid_dt tells the browser each column's kind, the values to tick and the quality limits",
        identical(unlist(o$types), c("time", "num", "chr")) && identical(unlist(o$levels$status), c("flagged", "ok")) &&
        identical(names(o$limits), "level") && identical(o$limits$level$max, 0.65))
  check("half-hourly times are shown without seconds", identical(w$x$data$time[1], "2024-07-01 00:00"))
  check("values, blocks and rows are selected the ovid way unless the page picks rows",
        isTRUE(o$cells) && identical(w$x$selection$mode, "none") && grepl("d-cells", w$x$container) &&
        isFALSE(ovid_dt(d, selection = "single")$x$options$ovid$cells))
  j <- paste(readLines(file.path(CORE_DIR, "www", "table.js"), warn = FALSE), collapse = "\n")
  check("a header click never sorts: the column button sorts and filters, and widths and hidden columns are kept",
        grepl("th.off('click.DT keypress.DT selectstart.DT')", j, fixed = TRUE) && grepl("data-o=\"desc\"", j, fixed = TRUE) &&
        grepl("'ovid-tbl:'", j, fixed = TRUE) && grepl("T.view.hid", j, fixed = TRUE)) })

cat("corrections applied (applied.R)\n")
local({ for (.f in c("periods", "corrections", "applied")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  pd <- tempfile("applied_"); on.exit(unlink(pd, recursive = TRUE)); ad <- file.path(pd, "output", "audit"); dir.create(ad, recursive = TRUE); dir.create(file.path(pd, "config"))
  fwrite(data.table(target = c("A", "B", "C"), parameter = c("temp", "temp", ""), from = c("2024-06-02", "2023-09-21", "2024-04-10"), to = c("2024-06-05", "2024-09-21", "2024-05-01"),
                    action = c("remove", "offset", "flag"), value = c("", "0.12", ""), on = c("", "", "off"), note = c("out of hole", "", ""), author = "", added = ""), file.path(pd, "config", PERIOD_FILE))
  dir.create(file.path(ad, "detail")); audit_reset(file.path(ad, "detail")); AUDIT$run_id <- "20261010T031200"
  audit_add("period", "period_offset", "values", 40L, reason = paste0("Offset 0.12 from a to b (row 2 of ", PERIOD_FILE, ")"))
  audit_add("qc", "manual", "values", 4L, reason = paste0("out of hole (row 1 of ", PERIOD_FILE, ")"))   # a removal applied with the quality rules
  audit_add("qc", "range_max", "values", 7L, reason = "above the limit"); audit_add("stitch", "stray_record", "row", 3L, reason = "out of order")
  audit_flush("t"); fwrite(rbindlist(AUDIT$index), file.path(ad, "audit_index.csv"))
  fwrite(data.table(check = c("spike", "spike"), records = c(2, 3), detail = c("", "")), file.path(ad, "flags.csv"))
  corr <- list(list(id = "stray_records", rules = "stray_record", does = "removes", setting = "drop_stray_records", what = "Records out of time order are removed."),
               list(id = "gap_fill", does = "fills", setting = "fill_gaps", default = FALSE, what = "Short gaps are filled."),
               list(id = "zone", does = "changes", setting = "", what = "Time stamps are moved to the base zone."),
               list(id = "depths", does = "fills", unit = "sensors", setting = "", what = "Sensors with no depth are given one."))
  a <- applied_write(list(project_dir = pd), corr, counts = list(zone = 12, depths = 2))
  g <- function(k) a[group == k]
  check("a correction that is on is listed with the values its audit rules removed; one that is off is not",
        nrow(g("On by default")) == 3 && g("On by default")$n[1] == 3 && g("On by default")$item[1] == "Records out of time order are removed")
  check("a correction with no audit rule takes the count the pipeline gives it", g("On by default")$n[2] == 12)
  check("a count in another unit says so and stays out of the totals", g("On by default")$n[3] == 2 && g("On by default")$does[3] == "sensors filled")
  check("period rows that are on are listed with what they did, by row, whatever rule wrote the lines; a row the run wrote nothing for counts 0",
        nrow(g("Corrections over a period")) == 2 && g("Corrections over a period")$item[2] == "B temp: Offset 0.12, 2023-09-21 to 2024-09-21" &&
        g("Corrections over a period")$n[2] == 40 && g("Corrections over a period")$n[1] == 4 && g("Corrections over a period")$detail[1] == "row 1: out of hole")
  check("quality rules: removals from the audit index, flags from flags.csv",
        identical(g("Quality rules and checks")$item, c("Range max", "Spike")) && identical(g("Quality rules and checks")$n, c(7, 5)))
  check("totals: corrections listed, values removed (offsets are not removals), changed and flagged",
        identical(sapply(c("corrections", "removed", "changed", "flagged"), function(k) applied_total(a, k), USE.NAMES = FALSE), c(7, 14, 52, 5)))
  check("the list is written to the audit folder and copied into an export", file.exists(file.path(ad, APPLIED_FILE)) &&
        isTRUE(applied_export(pd, pd)) && nrow(applied_read(pd)) == nrow(a))
  fwrite(data.table(target = c("A", "B", "D"), parameter = "temp", from = "", to = "", action = "remove", value = "", on = c("off", "off", ""), note = "", author = "", added = ""), file.path(pd, "config", PERIOD_FILE))
  check("a period row the run wrote nothing for counts 0", applied_write(list(project_dir = pd), corr)[group == "Corrections over a period", n] == 0) })

local({   # yes/no columns, the dataset card and NetCDF import (14 Oct)
  check("yes/no spellings are read in any case; other text is not Yes or No",
        identical(yesno_value(c("YES", "n", "True", "0", "on", "maybe", "")), c(TRUE, FALSE, TRUE, FALSE, TRUE, NA, NA)))
  d <- data.frame(enabled = c("yes", "FALSE", "", "x"), notes = "a", stringsAsFactors = FALSE)
  check("yes/no columns show as Yes/No and are written TRUE/FALSE; blanks and other text stay",
        identical(yesno_as(d, "any.csv")$enabled, c("Yes", "No", "", "x")) && identical(yesno_as(yesno_as(d, "any.csv"), "any.csv", shown = FALSE)$enabled, c("TRUE", "FALSE", "", "x")))
  check("a yes/no cell with other text is an error", any(table_check(d, table_rules(names(d)))$problem == "'x' is not Yes or No"))
  for (.f in c("qc", "standards")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  check("processing levels follow the stage of correction (L0 raw to L4 derived)",
        identical(c(proc_level(4L), proc_level(c(0L, 7L)), proc_level(c(0L, 2L)), proc_level(c(0L, 2L), reviewed = TRUE), proc_level(c(0L, 5L)),
                    proc_level(c(0L, 6L)), proc_level(0L, approved = TRUE), proc_level(0L, derived = TRUE)), c("L0", "L1A", "L1B", "L2", "L2", "L3", "L3", "L4")) &&
        proc_level_label("L1B") == "L1B flagged (provisional)" && proc_level_label("L0") == "L0 raw")
  pd <- tempfile("ovid_ds_"); dir.create(pd)
  dataset_save(pd, list(title = "Peat plateau study", creator = "M", license = "CC-BY-4.0", institution = ""))
  a <- acdd_atts("t", "What the file holds.", "id", project_dir = pd)
  check("the dataset card goes into the NetCDF attributes; blank fields are left out",
        identical(a$project, "Peat plateau study") && identical(a$creator_name, "M") && identical(a$license, "CC-BY-4.0") && is.null(a$institution) && identical(a$summary, "What the file holds."))
  if (requireNamespace("ncdf4", quietly = TRUE)) {
    f <- tempfile(fileext = ".nc"); t <- as.numeric(as.POSIXct("2026-06-01", tz = "UTC")) + c(0, 1800, 3600)
    nc_write(f, c(time = 3L, name_strlen = 64L), c(list(
        list(name = "time", dims = "time", data = t, atts = list(standard_name = "time", units = "seconds since 1970-01-01 00:00:00", axis = "T")),
        list(name = "air_temp", dims = "time", data = c(1.5, NA, 2.5), atts = list(units = "degC", standard_name = "air_temperature")),
        list(name = "air_temp_qc", dims = "time", data = nc_byte(c(1, 9, 3)), atts = cf_qc_atts("air_temp"))),
      nc_station_vars("A / Table1", 60.1, -121.3)), acdd_atts("t", "s", "id", t, 60.1, -121.3, project_dir = pd))
    x <- nc_import(f)
    check("an exported NetCDF file reads back: station, UTC times, values, gaps, QC codes, units and the file's attributes",
          nrow(x) == 3 && x$station[1] == "A / Table1" && format(x$time[2], "%H:%M", tz = "UTC") == "00:30" && is.na(x$value[2]) && x$value[3] == 2.5 &&
          identical(x$qc[c(1, 3)], c(1L, 3L)) && x$units[1] == "degC" && identical(attr(x, "atts")$license, "CC-BY-4.0"))
  } })

local({   # not deployed: the time between a place's deployments (deployments.R absent_spans)
  t <- as.POSIXct(c("2024-06-01", "2024-09-14", "2024-10-02", "2025-09-10", "2025-08-01"), tz = "UTC")
  g <- absent_spans(t[c(3, 1)], t[c(4, 2)], c("B", "A"))
  check("the gap between two deployments is one absent span, in time order, named by both loggers",
        nrow(g) == 1 && g$t0 == as.numeric(t[2]) && g$t1 == as.numeric(t[3]) && g$kind == "absent" && grepl("(18 days), between A and B", g$label, fixed = TRUE))
  check("overlapping or single deployments leave no span", !nrow(absent_spans(t[c(1, 5)], t[c(4, 4)])) && !nrow(absent_spans(t[1], t[2]))) })

cat("say(): one, in core\n")
local({ source(file.path(CORE_DIR, "cache.R"), local = TRUE)
  repo <- dirname(CORE_DIR)
  own <- unlist(lapply(c("CS", "WLR", "GT", "GS", "RS"), function(a) if ("say" %in% core_shadows(app_code_files(file.path(repo, a)))) a))
  check("no app defines its own say() (each uses core's, with its LOG and SAY_STEPS)", !length(own))
  lg <- new.env(); lg$lines <- character()
  out <- capture.output(say("[2/6] ", "reading", log = lg, steps = 6), say("plain", log = NULL, steps = 0))
  check("say() prints, adds the message to the log it is given, and with steps but no Shiny session moves nothing",
        identical(lg$lines, "[2/6] reading") && length(out) == 2 && grepl("plain", out[2], fixed = TRUE))
  had <- exists("LOG", envir = globalenv(), inherits = FALSE)
  out <- capture.output(say("no log"))
  assign("LOG", lg, envir = globalenv())
  capture.output(say("app log"))
  if (!had) rm("LOG", envir = globalenv())
  check("by default say() writes to the app's LOG when there is one, and only prints when there is none",
        !had && identical(out, "no log") && identical(lg$lines, c("[2/6] reading", "app log"))) })

cat("clock corrections (time_corrections.R), shared by CS, WLR and GT\n")
local({ for (.f in c("timestamps", "time_corrections")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  say <- function(...) invisible(); audit_reset()
  t0 <- as.POSIXct("2025-06-01 00:00", tz = "UTC")
  one <- function() data.table(logger_id = "123", TIMESTAMP = t0 + 0:10 * 3600, source_file = rep(c("a.csv", "b.csv"), c(5, 6)))
  wlr <- data.table(serial = "123", start = "2025-06-01 02:00", end = "2025-06-01 04:00", shift_minutes = "30", reason = "fast", author = "t", mode = "")
  gt <- wlr[, .(serial, start, end, shift_minutes, reason)]
  cs <- data.table(logger_id = "123", table = "", source_file = "", start = wlr$start, end = wlr$end, action = "shift", shift_minutes = "30", reason = "fast", author = "t")
  w <- apply_time_corrections(one(), wlr, 1L, dedupe = TRUE); g <- apply_time_corrections(one(), gt, 1L, audit = FALSE); c1 <- apply_time_corrections(one(), cs)
  check("the same correction moves the same records the same way, whatever the app's table columns",
        identical(sort(w$TIMESTAMP), sort(g$TIMESTAMP)) && identical(g$TIMESTAMP, c1$TIMESTAMP) && attr(g, "shifted") == 3L && g$TIMESTAMP[3] == t0 + 2.5 * 3600)
  dr <- apply_time_corrections(one(), wlr[, `:=`(start = "2025-06-01 00:00", end = "2025-06-01 10:00", shift_minutes = "-10", mode = "drift")], 1L)
  check("a drift is spread over the span: none at its start, all of it at its end",
        dr$TIMESTAMP[1] == t0 && dr$TIMESTAMP[11] == t0 + 10 * 3600 - 600 && dr$TIMESTAMP[6] == t0 + 5 * 3600 - 300)
  cl <- apply_time_corrections(one(), data.table(serial = "123", start = "2025-06-01 02:00", end = "2025-06-01 02:00", shift_minutes = "60", reason = ""), 1L, dedupe = TRUE)
  check("with dedupe a record moved onto another's time is dropped and the rest are in time order", nrow(cl) == 10 && !is.unsorted(cl$TIMESTAMP))
  long <- one()[rep(1:11, each = 2)]
  check("without dedupe two channels at one time both stay (GT's long table)", nrow(apply_time_corrections(long, gt, 1L, audit = FALSE)) == 22)
  rm1 <- apply_time_corrections(one(), cs[, `:=`(action = "remove", source_file = "a.csv")])
  check("action remove drops the records, and a source_file limits the row to that file", nrow(rm1) == 8 && !any(rm1$TIMESTAMP %in% (t0 + 2:4 * 3600)))
  np <- length(AUDIT$problems)
  bad <- apply_time_corrections(one(), cs[, `:=`(action = "shift", source_file = "", shift_minutes = "x")])
  bad2 <- apply_time_corrections(one(), cs[, `:=`(shift_minutes = "30", start = "June-ish")])
  check("a row whose minutes or dates are not readable is listed as a problem and moves nothing",
        length(AUDIT$problems) == np + 2 && identical(bad$TIMESTAMP, one()$TIMESTAMP) && identical(bad2$TIMESTAMP, one()$TIMESTAMP))
  a <- audit_table()
  check("a shift is audited per raw file, as a change of TIMESTAMP (not as records dropped)",
        nrow(a[rule == "clock_fix_shifted" & level == "change" & column == "TIMESTAMP"]) >= 2 && nrow(a[rule == "clock_fix_drift"]) == 2) })

cat("HOBOware CSV records and events (logger_files.R), shared by WLR and GT\n")
local({ source(file.path(CORE_DIR, "logger_files.R"), local = TRUE)
  repo <- dirname(CORE_DIR)
  one <- function(f) { a <- read_lines_any(f); a <- a[nzchar(trim(a))]                      # GT reads lines this way
    b <- strsplit(gsub("\r", "", rawToChar(readBin(f, "raw", file.size(f))), fixed = TRUE), "\n", fixed = TRUE)[[1]]   # WLR keeps the bytes as they are
    ra <- hobo_csv_rows(a, hobo_csv_head(a)); rb <- hobo_csv_rows(b, hobo_csv_head(b)); h <- hobo_csv_head(a)
    da <- fread(text = ra$lines, header = FALSE, sep = h$sep, colClasses = "character", fill = TRUE, na.strings = NULL)
    db <- fread(text = rb$lines, header = FALSE, sep = h$sep, colClasses = "character", fill = TRUE, na.strings = c("", "NA"))
    ev <- which(seq_along(h$header) > h$dcol & !grepl("^(Temp|Abs Pres)", h$header)); ts <- parse_dt(da[[h$dcol]])
    list(same_rows = identical(fix_enc(ra$lines), fix_enc(rb$lines)) && is.null(ra$header_change), n = length(ra$lines),
         ea = hobo_csv_events(da, ev, ts, h$header), eb = hobo_csv_events(db, ev, ts, h$header), cut = ends_cut(f)) }
  w <- one(file.path(repo, "WLR", "tests", "samples", "hobo_csv__10000005_SiteA_2024.csv"))
  g <- one(file.path(repo, "GT", "tests", "samples", "hobo_csv__1000004_SiteA20240921.csv"))
  check("WLR's and GT's HOBO samples give the same record lines whichever way the app reads the file",
        w$same_rows && g$same_rows && w$n > 0 && g$n > 0)
  check("... and the same events, named without the logger's serial", identical(w$ea, w$eb) && nrow(w$ea) > 0 && !any(grepl("S/N", w$ea$event)) &&
          identical(g$ea, g$eb))
  check("neither sample ends inside a row", !w$cut && !g$cut)
  j <- c("\"Plot Title: X\"", "\"#\",\"Date Time, GMT-07:00\",\"Temp, \u00b0C\"", "1,09/27/23 03:00:00 PM,1.5", "", "\"#\",\"Date Time, GMT-07:00\",\"Temp, \u00b0C\"",
         "2,09/27/23 03:30:00 PM,1.6", "\"#\",\"Date Time, GMT-07:00\",\"RH, %\"", "3,09/27/23 04:00:00 PM,80", "4,09/27/23 04:30:00 PM,81")
  r <- hobo_csv_rows(j, hobo_csv_head(j))
  check("a blank line and a repeated heading are left out; a heading with other columns ends the records and is reported",
        length(r$lines) == 2 && identical(r$header_change, list(line = 7L, rows = 2L)))
  check("... or, when the app asks, the rows after it are kept",
        length(hobo_csv_rows(j, hobo_csv_head(j), stop_at_change = FALSE)$lines) == 4)
  cf <- tempfile(fileext = ".csv"); writeBin(charToRaw("a,b\n1,2"), cf)
  check("a file that ends inside a row is found", ends_cut(cf) && !nrow(hobo_csv_events(data.table(), integer(), NULL, character()))) })

cat("a set-up table that cannot be read is reported, not taken as empty (CONTRIBUTING rule 13)\n")
local({ for (.f in c("periods", "corrections")) source(file.path(CORE_DIR, paste0(.f, ".R")), local = TRUE)
  say <- function(...) invisible()
  bad <- file.path(tempfile("cfg"), "period_corrections.csv"); dir.create(bad, recursive = TRUE)   # a folder where the table should be: read_cfg() fails
  n0 <- length(AUDIT$problems)
  r <- period_read(bad)
  pr <- rbindlist(AUDIT$problems[-seq_len(n0)])
  check("period corrections that cannot be read: none applied, and the table is named on the problem list",
        !nrow(r$rows) && nrow(pr) >= 1 && any(pr$where == "period_corrections.csv"))
  n1 <- length(AUDIT$problems)
  lg <- corrections_log(list(), list(list(id = "x", what = "x", on = function(cfg) stop("no such setting"))))
  check("a correction whose on/off test fails is listed as off and the failure is on the problem list",
        length(AUDIT$problems) > n1) })


# Item 4 colours: every colour an app draws comes from its palette (app_palette(), core/theme.R), except the named
# scientific scales and fixed values listed here with their reason. A quoted hex colour or rgba() anywhere else fails.
local({ source(file.path(CORE_DIR, "theme.R"), local = TRUE)
  n <- nord_palette()
  check("nord_palette() gives WLR's Nord roles from OVID_SCHEMES$Nord", n$bg == "#2E3440" && n$fg == "#ECEFF4" && n$deep == "#5E81AC" &&
        n$orange == "#D08770" && n$bg_hard == "#242933" && n$gray == "#7B88A1" && identical(n$frost, n$accent))
  check("col_rgba() writes a colour at an opacity", col_rgba("#1F1F28", 0.85) == "rgba(31,31,40,0.85)")
  allowed <- c(
    "GS/R/plots.R" = "c(\"#2c7bb6\", \"#abd9e9\"", "GS/R/views.R" = "Spectral = c(", "GS/pages/fct_stack.R" = "c(\"#2c7bb6\", \"#abd9e9\"",   # Spectral, Viridis
    "GS/pages/fct_stack.R" = "#2166ac", "GS/pages/fct_workbench.R" = "#2166ac",                        # RdBu change in resistivity
    "GS/pages/fct_workbench.R" = "ELECTRODE_COL", "GS/R/gpr.R" = "GPR_SCALES",                         # Spectral ends; Grey, Blue-white-red
    "GS/R/maps.R" = "MAP_LABEL_COL <-",                                                                # white on satellite imagery (M)
    "GS/pages/mod_gpr_map.R" = "line = list(color = \"#ffffff\"")                                     # cursor on the radargram image
  apps <- file.path(dirname(CORE_DIR), c("CS", "WLR", "GT", "GS", "RS"))
  files <- unlist(lapply(apps, list.files, pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
  files <- files[!grepl("/tests/", files)]
  bad <- character()
  for (f in files) { rel <- sub(paste0("^", dirname(CORE_DIR), "/"), "", f); ln <- readLines(f, warn = FALSE)
    hit <- grep("\"#[0-9A-Fa-f]{6}\"|\"rgba\\([0-9]", ln)
    ok <- vapply(hit, function(i) any(vapply(allowed[names(allowed) == rel], function(a) grepl(a, ln[i], fixed = TRUE), TRUE)) ||
                                    grepl("\"rgba(0,0,0,0)\"", ln[i], fixed = TRUE), TRUE)
    bad <- c(bad, sprintf("%s:%d", rel, hit[!ok])) }
  if (length(bad)) cat("    colours outside the palette:", paste(bad, collapse = ", "), "\n")
  check("apps draw no hex or rgba colour outside their palette, except the named scientific scales", !length(bad)) })

cat("finding the core\n")
local({
  root <- dirname(CORE_DIR)
  apps <- c("CS", "WLR", "GT", "GS", "RS")
  files <- c(file.path(root, apps, "launcher", "run_app.R"),
             file.path(root, apps, paste0(tolower(apps), "_pipeline.R")))
  core_line <- function(f) paste(grep("CORE_DIR <- local", readLines(f, warn = FALSE), value = TRUE), collapse = "")
  look <- vapply(files, core_line, "")
  check("every launcher and pipeline looks for the core in OVID_CORE, core/ and ../core only, never on the Desktop",
        all(nzchar(look)) && !any(grepl("Desktop", look)) && all(grepl("OVID_CORE", look)))
  all_lines <- unlist(lapply(files, readLines, warn = FALSE))
  check("the not-found message says to keep the app beside core/ rather than naming a Desktop folder",
        !any(grepl("~/Desktop/Ovid([^_]|$)", all_lines)))
})

cat("default folders\n")
local({
  root <- dirname(CORE_DIR)
  defaults <- function(app) { f <- file.path(root, app, paste0(tolower(app), "_pipeline.R"))
    grep("(raw_dirs|metadata_dir|project_dir)\\s*(=|<-)", readLines(f, warn = FALSE), value = TRUE) }
  look <- unlist(lapply(c("CS", "WLR", "GT", "GS", "RS"), defaults)); dirs <- unlist(regmatches(look, gregexpr('"~/[^"]*"', look)))
  check("every app's default folders are under ~/Projects/Ovid/Projects", length(dirs) >= 6 && all(startsWith(dirs, '"~/Projects/Ovid/Projects/')))
  check("projects go in ~/Projects/Ovid/Projects/<app> when none is set", identical(ovid_projects_home("RS"), "~/Projects/Ovid/Projects/RS"))
})

cat("the Precipitation switch (tsview.js)\n")
local({ js <- paste(readLines(file.path(CORE_DIR, "www", "tsview.js"), warn = FALSE), collapse = "\n")
  check("the Precipitation switch is hidden when no plot has precipitation to draw (it showed On and drew nothing)",
        grepl("data-tsv-opt=\"bars\"", js, fixed = TRUE) && grepl("l.hidden = !anyBars", js, fixed = TRUE)) })

cat(sprintf("\n%d passed, %d failed\n", RES$ok, length(RES$fail))); if (length(RES$fail)) quit(status = 1)
