# tests/test_drone_sources.R  --  Drone photos only (config/drone_cameras.csv), every photo read and cached per file, flights
# placed from photo geotags when there is no log, and removing a source.   Rscript tests/test_drone_sources.R   (exit 1 on a failure)
Sys.setenv(RS_NO_RUN = "1")
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
source(file.path(here, "rs_pipeline.R"), chdir = TRUE)
local({ ex <- parse(file.path(here, "tests", "run_tests.R"))            # the JPG writers of the main suite (EXIF and XMP as cameras write them)
  for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% c("le16", "le32", "exif_app1", "jpg", "dji_xmp")) eval(e, globalenv()) })
tmp <- file.path(tempdir(), "rs_drone_test"); unlink(tmp, TRUE); dir.create(tmp, recursive = TRUE)
fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }
n_read <- 0L; read0 <- read_image_raw; read_image_raw <- function(paths) { n_read <<- n_read + length(paths); read0(paths) }   # counts the photos opened (the photo cache reads raw tags, R/catalogue.R)

# a made-up place (45.5 N, 72.2 W); one DJI flight with no log, a phone photo and a Google phone photo (GCamera: is not a drone tag)
S <- file.path(tmp, "Drive"); f1 <- file.path(S, "Flight A"); dir.create(f1, recursive = TRUE); dir.create(file.path(S, "phone"))
for (i in 1:4) jpg(file.path(f1, sprintf("DJI_%04d.JPG", i)), exif_app1("DJI", "FC6310R", "CAM1", sprintf("2020:07:15 10:00:%02d", 2 * i), 45.5 + i / 1e4, -72.2, 100),
                   dji_xmp(sprintf("2020-07-15T14:00:%02d", 2 * i), 50, "CAM1"), pad = 36000L)
jpg(file.path(S, "phone", "IMG_0001.JPG"), exif_app1("Apple", "iPhone 13", "", "2020:07:15 09:00:00", 45.6, -72.3, 90))
jpg(file.path(S, "phone", "PXL_0001.JPG"), exif_app1("Google", "Pixel 7", "", "2020:07:15 09:01:00", 45.6, -72.3, 90), '<x:xmpmeta><rdf:Description GCamera:MicroVideo="1"/></x:xmpmeta>')
T2 <- file.path(tmp, "Second"); f2 <- file.path(T2, "Flight B"); dir.create(f2, recursive = TRUE)
for (i in 1:3) jpg(file.path(f2, sprintf("DJI_%04d.JPG", i)), exif_app1("DJI", "FC6310R", "CAM2", sprintf("2020:07:16 10:00:%02d", 2 * i), 45.4 + i / 1e4, -72.1, 100),
                   dji_xmp(sprintf("2020-07-16T14:00:%02d", 2 * i), 50, "CAM2"), pad = 36000L)
files0 <- tools::md5sum(list.files(tmp, recursive = TRUE, full.names = TRUE))
proj <- file.path(tmp, "project"); catd <- file.path(proj, "output", "catalogue"); cat_rd <- function(f) fread(file.path(catd, f))
q <- function(...) invisible(capture.output(run_pipeline(list(project_dir = proj), ...)))

q(scan = S, source_id = "Drive"); n1 <- n_read
im <- cat_rd("images.csv"); op <- cat_rd("other_photos.csv"); fl <- cat_rd("flights.csv"); geo <- cat_rd("geometry.csv")
check(nrow(im) == 4 && all(grepl("^Flight A/", im$rel)), "DJI photos kept as drone photos")
check(nrow(op) == 2 && setequal(op$make, c("Apple", "Google")) && all(nzchar(op$why)) && all(file.exists(op$path)), "a phone photo (Make Apple) and a GCamera: photo left out, listed in other_photos.csv with why")
check(file.exists(file.path(proj, "config", "drone_cameras.csv")) && nrow(fread(file.path(proj, "config", "drone_cameras.csv"))) > 5, "drone_cameras.csv seeded in a new project")
cams <- data.table(make = c("SENSEFLY", "Canon"), model = c("", "Canon PowerShot S110"))
check(identical(drone_why(c("senseFly", "Canon", "Canon", NA), c("thermoMAP", "Canon PowerShot S110", "Canon PowerShot S120", NA), FALSE, cams)[1:3] == "", c(TRUE, TRUE, FALSE)) &&
      drone_why(NA, NA, TRUE, cams) == "" && nzchar(drone_why(NA, NA, FALSE, cams)), "a blank-model make row matches any model; a model is matched as a prefix; a drone tag alone counts")
dc <- drone_cameras_default(); w <- drone_why(c("DJI", "DJI", "DJI", "RECONYX", "Apple"), c("DJI Pocket", "FC7303", "FC6310S", "UltraFire", "iPhone 8"), c(TRUE, TRUE, FALSE, FALSE, FALSE), dc)
check(identical(w == "", c(FALSE, TRUE, TRUE, FALSE, FALSE)) && grepl("not a drone", w[1]), "a DJI handheld with drone tags, a trail camera and a phone are left out; DJI drones are kept")
check(n1 == 6, "every photo read on the first scan (no sample)")
check(nrow(fl) == 1 && fl$placed_from == "photo geotags" && abs(fl$lat_min - 45.5001) < 1e-6 && abs(fl$lat_max - 45.5004) < 1e-6 &&
      any(geo$what == "track from photo geotags" & geo$flight_id == fl$flight_id), "a flight with no log takes its extent and track from the photo geotags")

n_read <- 0L; q(scan = S); check(n_read == 0L, "a rescan reads no unchanged photo")
Sys.sleep(1.1); jpg(file.path(f1, "DJI_0005.JPG"), exif_app1("DJI", "FC6310R", "CAM1", "2020:07:15 10:00:10", 45.5005, -72.2, 100), dji_xmp("2020-07-15T14:00:10", 50, "CAM1"), pad = 36000L)
n_read <- 0L; q(scan = S); check(n_read == 1L && nrow(cat_rd("images.csv")) == 5, "a rescan reads only the new photo")
unlink(file.path(f1, "DJI_0005.JPG")); q(scan = S)

q(scan = T2, source_id = "Second"); check(nrow(cat_rd("flights.csv")) == 2, "a second source adds its flight")
r <- remove_source(proj, "Second")
check(!"Second" %in% fread(file.path(proj, "config", "sources.csv"))$source_id && r$flights == 1 && !"Second" %in% cat_rd("images.csv")$source_id &&
      !"Second" %in% cat_rd("flights.csv")$source_id && !"Second" %in% cat_rd("files.csv")$source_id && nrow(cat_rd("flights.csv")) == 1,
      "remove_source drops the source from config/sources.csv and its rows from the catalogue")
check(identical(tools::md5sum(names(files0)), files0), "remove_source and the scans leave every source file as it was")

# copies, a made-up satellite scene and a year-named folder in the same drive
bk <- file.path(S, "Backup", "Flight A copy"); dir.create(bk, recursive = TRUE); file.copy(list.files(f1, full.names = TRUE), bk)
sc <- file.path(S, "Scene"); dir.create(sc); writeLines("satId = \"XX01\";", file.path(sc, "20AUG01-M2AS_P001.IMD"))
writeBin(c(charToRaw("II"), le16(42), le32(8), as.raw(rep(0, 64))), file.path(sc, "20AUG01-M2AS_P001.TIF"))
writeBin(c(charToRaw("II"), le16(42), le32(8), as.raw(rep(0, 64))), file.path(sc, "IMG_SPOT6_P_001.TIF")); jpg(file.path(sc, "browse.JPG"), raw())
q(scan = S); im <- cat_rd("images.csv"); dp <- cat_rd("duplicates.csv"); fl <- cat_rd("flights.csv"); fs <- cat_rd("files.csv")
check(nrow(dp) == 4 && all(dp$kind == "photo") && setequal(c(dirname(dp$kept_path[1]), dirname(dp$dup_path[1])), c("Drive:Flight A", "Drive:Backup/Flight A copy")) && uniqueN(dirname(dp$kept_path)) == 1 &&
      identical(names(dp), c("kind", "key", "kept_path", "dup_path", "why")), "copies of photos are counted once and listed in duplicates.csv (kind, key, kept_path, dup_path, why)")
check(nrow(fl) == 1 && fl$n_copies == 1 && grepl("(4 images)", fl$copies, fixed = TRUE), "the copied folder is still one flight, with its copy listed")
check(!any(grepl("^Scene/", c(im$rel, cat_rd("other_photos.csv")$rel))) && all(fs[grepl("^Scene/.*TIF$", rel), role] == "raster") && nrow(fl) == 1,
      "a satellite scene stays a raster: never a photo or a flight")
check(all(is.na(folder_flight_no(c("Flights 2010", "2023/Drone_2023", "Binders 2003/scans", "Archive/Scenes_2021", "2010", "Photos-1999")))) &&
      folder_flight_no("x/EBEE_0303") == 303L && folder_flight_no("2021/NIR_BogA_354") == 354L, "a year in a folder name is never a flight number; a flight number still is")
sat2 <- file.path(tmp, "Imagery 2010", "Scene_2021"); dir.create(sat2, recursive = TRUE); writeBin(c(charToRaw("II"), le16(42), le32(8), as.raw(rep(0, 64))), file.path(sat2, "IMG_PHR1A_P_001.TIF"))
ls2 <- list_source(dirname(sat2)); check(all(ls2$role == "raster") && !any(ls2$role == "image"), "a satellite scene in a year folder is a raster, never a photo")

# a source inside another (and the same real folder added twice): each file once, under the innermost source
q(scan = f1, source_id = "FlightA"); im <- cat_rd("images.csv"); fs <- cat_rd("files.csv"); s0 <- fread(file.path(proj, "config", "sources.csv"))
check(nrow(im[source_id == "FlightA"]) == 4 && !nrow(im[source_id == "Drive" & startsWith(rel, "Flight A/")]) && !anyDuplicated(fs[, file.path(fifelse(source_id == "FlightA", "Flight A", ""), rel)]),
      "a source inside another: its files are counted once, under the inner source")
q(scan = file.path(S, ".", "Flight A"), source_id = "again"); check(nrow(fread(file.path(proj, "config", "sources.csv"))) == nrow(s0), "the same real folder is not added twice")

if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green")
