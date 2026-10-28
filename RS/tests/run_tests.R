# tests/run_tests.R  --  Builds two small synthetic archive sources (DJI and senseFly eBee), scans them one after the other,
# and checks the catalogue and the viewer.   Rscript tests/run_tests.R   (exit status 1 if anything fails)
Sys.setenv(RS_NO_RUN = "1")
here <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else getwd() })
source(file.path(here, "rs_pipeline.R"), chdir = TRUE)
tmp <- file.path(Sys.getenv("RS_TEST_DIR", tempdir()), "rs_test"); unlink(tmp, TRUE); dir.create(tmp, recursive = TRUE)
fails <- 0L; check <- function(ok, what) { if (!isTRUE(ok)) { fails <<- fails + 1L; say("FAIL ", what, col = "red") } else say("ok   ", what, col = "green") }

# ---- a JPG as cameras write it, as far as Ovid reads it: EXIF (make, model, serial, time, GPS) and an optional XMP packet
le16 <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = "little")
le32 <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
exif_app1 <- function(make, model, serial, time, lat = NA, lon = NA, alt = NA, tiff_only = FALSE) {
  ascii <- function(s) c(charToRaw(s), as.raw(0))
  rat <- function(v) unlist(lapply(v, function(x) c(le32(round(x * 1e4)), le32(1e4))))
  E <- function(tag, type, count, bytes) list(tag = tag, type = type, count = count, bytes = bytes)
  ifd <- function(entries, base) {                 # the entries, then the values too long to sit in an entry
    n <- length(entries); data_off <- base + 2 + 12 * n + 4; head <- le16(n); data <- raw()
    for (e in entries) { v <- e$bytes
      if (length(v) <= 4) head <- c(head, le16(e$tag), le16(e$type), le32(e$count), v, as.raw(rep(0, 4 - length(v))))
      else { head <- c(head, le16(e$tag), le16(e$type), le32(e$count), le32(data_off + length(data))); data <- c(data, v) } }
    c(head, le32(0), data) }
  gps <- if (is.na(lat)) list() else {
    dms <- function(x) { x <- abs(x); c(floor(x), floor(x %% 1 * 60), (x * 3600) %% 60) }
    list(E(1, 2, 2, ascii(if (lat < 0) "S" else "N")), E(2, 5, 3, rat(dms(lat))), E(3, 2, 2, ascii(if (lon < 0) "W" else "E")), E(4, 5, 3, rat(dms(lon))), E(6, 5, 1, rat(alt))) }
  ifd0 <- function(exif_off, gps_off) c(list(E(271, 2, nchar(make) + 1, ascii(make)), E(272, 2, nchar(model) + 1, ascii(model)), E(34665, 4, 1, le32(exif_off))),
                                        if (length(gps)) list(E(34853, 4, 1, le32(gps_off))))
  exif_off <- 8 + length(ifd(ifd0(0, 0), 8))
  ex <- ifd(c(list(E(36867, 2, 20, ascii(time))), if (nzchar(serial)) list(E(42033, 2, nchar(serial) + 1, ascii(serial)))), exif_off)
  gps_off <- exif_off + length(ex)
  tiff <- c(charToRaw("II"), le16(42), le32(8), ifd(ifd0(exif_off, gps_off), 8), ex, if (length(gps)) ifd(gps, gps_off))
  if (tiff_only) return(tiff)
  body <- c(charToRaw("Exif"), as.raw(c(0, 0)), tiff)
  c(as.raw(c(0xFF, 0xE1)), writeBin(as.integer(length(body) + 2), raw(), size = 2, endian = "big"), body)
}
jpg <- function(path, exif, xmp = "", pad = 2000L) writeBin(c(as.raw(c(0xFF, 0xD8)), exif, as.raw(rep(0, pad)), charToRaw(xmp), as.raw(c(0xFF, 0xD9))), path)
dji_xmp <- function(utc, flag, cam) sprintf(paste0('<x:xmpmeta xmlns:x="adobe:ns:meta/"><rdf:Description drone-dji:RelativeAltitude="+60.1" drone-dji:RtkFlag="%d" ',
  'drone-dji:RtkStdLon="0.008" drone-dji:RtkStdLat="0.010" drone-dji:UTCAtExposure="%s.458837" drone-dji:CameraSerialNumber="%s" ',
  'drone-dji:DroneModel="Matrice 350 RTK" drone-dji:DroneSerialNumber="DRONE1"/></x:xmpmeta>'), flag, utc, cam)
sf_xmp <- function(acc) sprintf('<x:xmpmeta xmlns:x="adobe:ns:meta/"><rdf:Description sensefly:CamId="30" Camera:GPSXYAccuracy="%.6f" Camera:GPSZAccuracy="%.6f"/></x:xmpmeta>', acc, acc)

# ---- source A: a T7-like drive ----------------------------------------------------------------------------------------
A <- file.path(tmp, "T7")
f1 <- file.path(A, "2024-Drone Flights", "DJI_202409211138_009_LakeA-RGB"); dir.create(f1, recursive = TRUE)
for (i in 1:3) jpg(file.path(f1, sprintf("DJI_2024092111434%d_000%d_D.JPG", i, i + 2)),
                   exif_app1("DJI", "L2", "CAM1", sprintf("2024:09:21 11:43:4%d", i), 60.098 + i / 1e4, -120.1, 331),
                   dji_xmp(sprintf("2024-09-21T17:43:4%d", i), 50, "CAM1"), pad = 36000L)
writeLines(c("3\t582237.1\t[2332]\t  -348,N\t  -404,E\t   216,V\t60.09803327,Lat\t-120.10022204,Lon\t331.068,Ellh\t0.011, 0.008, 0.021\t50,Q",
             "4\t582240.8\t[2332]\t  -352,N\t  -402,E\t   274,V\t60.09803125,Lat\t-120.10043396,Lon\t331.078,Ellh\t0.010, 0.008, 0.021\t50,Q",
             "5\t582242.4\t[2332]\t  -350,N\t  -394,E\t   287,V\t60.09802945,Lat\t-120.10062467,Lon\t331.093,Ellh\t0.010, 0.008, 0.021\t50,Q"),
           file.path(f1, "DJI_202409211138_009_LakeA-RGB_Timestamp.MRK"))
writeLines('{"SURVEYING_REPORT_ROOT": {"BASIC_INFO_UNIT": {"MISSION_ID": 20030, "MISSION_START_TIME": 1726940571, "FLY_TIME": 1795},
  "VISIBLE_CAM_INFO_UNIT": {"TOTAL_CAP_NUM": 4, "RTK_FIXED_NUM": 4, "RTK_FLOAT_NUM": 0, "RTK_SINGLE_NUM": 0}}}', file.path(f1, "DJI_20240921114251_0001_D.RPT"))
writeBin(as.raw(1:50), file.path(f1, "DJI_20240921114337_0002_D.RTK"))
writeBin(as.raw(1:50), file.path(f1, "DJI_20240921090000_0009_D.RTB"))       # two and a half hours before the flight
# a P1 flight with single-point positions and onboard GNSS observations, then a second flight 40 minutes later
f2 <- file.path(A, "2026-Drone Flights", "Optical-DJI", "DJI_202609031009_001_SiteB-RGB"); dir.create(f2, recursive = TRUE)
for (i in 1:2) jpg(file.path(f2, sprintf("DJI_2026090310171%d_000%d.JPG", i, i)), exif_app1("DJI", "ZenmuseP1", "CAM2", sprintf("2026:09:03 10:17:1%d", i), 60.10, -120.1, 350),
                   dji_xmp(sprintf("2026-09-03T16:17:1%d", i), 16, "CAM2"), pad = 36000L)
writeBin(as.raw(1:50), file.path(f2, "DJI_20260903101700_0001_D.RTK"))
jpg(file.path(f2, "DJI_20260903105800_0003.JPG"), exif_app1("DJI", "ZenmuseP1", "CAM2", "2026:09:03 10:58:00", 60.11, -120.1, 350), dji_xmp("2026-09-03T16:58:00", 50, "CAM2"), pad = 36000L)
# eBee S.O.D.A. flight 359 corrected in flight (2 cm in the camera's own geotags), its log in Drone Logs/
e1 <- file.path(A, "2026-Drone Flights", "Optical-EBEE", "2026_EBEE_SODA_REDO"); dir.create(file.path(e1, "EP-01-00001_0359"), recursive = TRUE)
for (i in 1:3) jpg(file.path(e1, "EP-01-00001_0359", sprintf("EP-01-00001_0359_000%d.JPG", i)),
                   exif_app1("senseFly", "S.O.D.A.", "SODA1", sprintf("2026:09:07 22:38:1%d", i), 60.11 + i / 1e4, -120.11, 386), sf_xmp(0.017))
dir.create(file.path(e1, "Drone Logs")); writeBin(as.raw(1:9), file.path(e1, "Drone Logs", "EP-01-00001_0359.bb3"))
# eBee flight 53 (2018): metre-level geotags in the camera images, centimetre ones in eMotion's geoinfo (post-processed)
e2 <- file.path(A, "2018-Drone Flights", "2018-08-28", "2018-08-28_North Transect"); dir.create(file.path(e2, "img"), recursive = TRUE)
for (i in 1:2) jpg(file.path(e2, "img", sprintf("EP-01-00001_0053_000%d.JPG", i)),
                   exif_app1("senseFly", "S.O.D.A.", "SODA0", sprintf("2018:08:29 00:30:3%d", i), 60.09, -120.11, 366), sf_xmp(2.39))
writeLines(c("#Rev: sensefly/geoinfo/3", "fileName\tlatitude\tlongitude\taltitude_wgs84\tAccuracy XY\tAccuracy Z",
             "EP-01-00001_0053_0001.JPG\t60.0998\t-120.1105\t366.0\t0.021\t0.030\t", "EP-01-00001_0053_0002.JPG\t60.0994\t-120.1106\t365.5\t0.022\t0.031\t"),
           file.path(e2, "2018_08_28_north_transect_geoinfo.txt"))
writeBin(as.raw(1:9), file.path(e2, "EP-01-00001_0053.bb3")); writeBin(as.raw(1:9), file.path(e2, "EP-01-00001_0053_em.bb3"))
# Canon S110 NIR flight 354 on the eBee: no GPS, camera clock at 1980; the flight number is in the folder name
e3 <- file.path(A, "2026-Drone Flights", "NIR-EBEE", "2026_EBEE_NIR_REDO", "NIR_BogA_354", "DCIM", "103___01"); dir.create(e3, recursive = TRUE)
for (i in 1:2) jpg(file.path(e3, sprintf("IMG_526%d.JPG", i)), exif_app1("Canon", "Canon PowerShot S110", "", sprintf("1980:01:01 00:02:0%d", i)))
writeBin(as.raw(1:9), file.path(dirname(dirname(e3)), "EP-01-00001_0354.bb3"))
dir.create(file.path(A, "$RECYCLE.BIN")); writeLines("x", file.path(A, "$RECYCLE.BIN", "junk.txt"))

# ---- source B: a backup holding a copy of the LakeA flight and one flight not on A ---------------------------------
B <- file.path(tmp, "Backup")
b1 <- file.path(B, "DCIM", basename(f1)); dir.create(b1, recursive = TRUE)
invisible(file.copy(list.files(f1, "000[34]_D.JPG", full.names = TRUE), b1))
b2 <- file.path(B, "DJI_202509081445_005_FirstLake-RGB"); dir.create(b2)
jpg(file.path(b2, "DJI_20250908144801_0003_D.JPG"), exif_app1("DJI", "L2", "CAM1", "2025:09:08 14:48:01", 60.12, -120.09, 340), dji_xmp("2025-09-08T20:48:01", 50, "CAM1"), pad = 36000L)

all_raw <- function() list.files(c(A, B), recursive = TRUE, full.names = TRUE, all.files = TRUE)
md5_before <- tools::md5sum(all_raw())
proj <- file.path(tmp, "project")

# ---- scan A, then B --------------------------------------------------------------------------------------------------
r <- run_pipeline(list(project_dir = proj), scan = A, source_id = "T7")
f <- r$flights
check(nrow(r$files) == 23, "every file of the first source listed; recycle bin skipped")
check(nrow(f) == 6, "six flights in the first source")
g <- f[site == "LakeA"]
check(nrow(g) == 1 && g$positioning_status == "RTK corrected" && g$rtk_fixed_pct == 100 && g$sensor == "DJI L2" && g$aircraft == "Matrice 350 RTK", "DJI flight: sensor, aircraft, RTK corrected")
check(g$date == "2024-09-21" && g$start == "2024-09-21 10:43:23", "start in Ovid's base zone (UTC-7), the XMP's GPS time less 18 leap seconds")
check(g$has_mrk && g$has_rpt && g$mission_id == 20030 && g$mrk_shots == 3 && grepl("RPT counts 4 captures, 3 images", g$qa_flags), "MRK and RPT linked, read and compared")
l <- r$links
check(l[grepl("0002_D.RTK$", rel)]$state == "auto" && l[grepl("0009_D.RTB$", rel)]$state == "suggested", "link confidence follows the file time")
o <- f[site == "SiteB"][order(start)]
check(nrow(o) == 2 && o$positioning_status[1] == "correction data available, not confirmed" && o$positioning_status[2] == "RTK corrected" && o$sensor[1] == "DJI ZenmuseP1",
      "a 40-minute gap splits a folder; single point + GNSS observations = available, not confirmed")
s359 <- f[flight_no == 359]
check(nrow(s359) == 1 && s359$family == "ebee" && s359$sensor == "senseFly S.O.D.A." && s359$positioning_status == "RTK corrected" && s359$has_log && s359$aircraft_serial == "EP-01-00001",
      "eBee S.O.D.A.: centimetre geotags = RTK corrected; log in another folder linked by serial and flight number")
s53 <- f[flight_no == 53]
check(nrow(s53) == 1 && s53$positioning_status == "PPK corrected" && s53$has_geotags && sum(l$flight_id == s53$flight_id & grepl("bb3$", l$rel)) == 2,
      "eBee metre geotags in the images, centimetre in eMotion's geoinfo = PPK corrected; both logs linked")
check(s53$site == "North Transect" && f[flight_no == 354]$site == "BogA", "site from eBee folder names")
s354 <- f[flight_no == 354]
check(nrow(s354) == 1 && s354$sensor == "Canon PowerShot S110" && is.na(s354$date) && grepl("camera clock unusable", s354$qa_flags) && s354$has_log && s354$positioning_status == "unknown",
      "Canon S110 without GPS or clock: one flight by its folder's flight number, log linked, status unknown")

r <- run_pipeline(list(project_dir = proj), scan = B, source_id = "Backup")
f <- r$flights
check(nrow(f) == 7, "second source adds one new flight; the duplicate is not counted twice")
g <- f[site == "LakeA"]
check(nrow(g) == 1 && g$source_id == "T7" && grepl("Backup:DCIM", g$copies), "the same flight in the backup is listed as a copy")
check(nrow(read_cfg(file.path(proj, "config", "sources.csv"))) == 2, "both sources recorded in config/sources.csv")

# an unplugged source stays in the catalogue
file.rename(B, file.path(tmp, "B_moved"))
xf <- file.path(source_dir(list(project_dir = proj), "T7"), "exif.rds"); Sys.setFileTime(xf, Sys.time() - 3600); xt <- file.mtime(xf)
r <- run_pipeline(list(project_dir = proj), scan = A)
check(identical(file.mtime(xf), xt), "a rescan that reads no new image leaves its image cache file as it is (a stopped run drops the cache files it wrote)")
check(nrow(r$flights) == 7 && any(grepl("Backup", r$flights$source_id)), "a source that is unplugged keeps its flights")
file.rename(file.path(tmp, "B_moved"), B)

# people's corrections win and survive a rescan; a rejected link is not made again
fc <- read_cfg(file.path(proj, "config", "flights.csv")); fc[flight_id == g$flight_id, `:=`(site = "Lake A", positioning_status = "PPK corrected")]
fwrite(fc, file.path(proj, "config", "flights.csv"))
fwrite(data.table(flight_id = g$flight_id, file = l[grepl("0002_D.RTK$", rel)]$file, state = "rejected", reason = "test", author = "M", date = ""),
       file.path(proj, "config", "links.csv"))
r2 <- run_pipeline(list(project_dir = proj), scan = character(), export = TRUE); g2 <- r2$flights[flight_id == g$flight_id]
check(g2$site == "Lake A" && g2$site_found == "LakeA" && g2$positioning_status == "PPK corrected" && g2$positioning_found == "RTK corrected",
      "user values override, found values kept beside them")
check(!any(grepl("0002_D.RTK$", r2$links$rel)), "rejected link not made again")
lh <- list_hits(data.table(source_id = "S", rel = rep(c("g1.txt", "g2.txt"), c(3, 1)), name = c("dji_0001.jpg", "dji_0002.jpg", "dji_0003.jpg", "dji_0001.jpg")),
                rbind(data.table(file = sprintf("DJI_%04d.JPG", 1:3), flight_id = "A"), data.table(file = sprintf("DJI_%04d.JPG", 1:2), flight_id = "B"),
                      data.table(file = "DJI_0001.JPG", flight_id = "C")))
check(identical(lh[rel == "g1.txt", flight_id], "A") && lh[rel == "g1.txt", n] == 3L && lh[rel == "g1.txt", k] == 1L && setequal(lh[rel == "g2.txt", flight_id], c("A", "B", "C")) &&
      all(lh[rel == "g2.txt", k] == 3L), "a geotag list goes to the flight with most of its image names; a name shared by many flights is a tie, not a join of every pair")

# the viewer
check(!is.null(r2$viewer) && file.exists(r2$viewer), "viewer zip written")
vd <- sub("\\.zip$", "", r2$viewer); h <- paste(readLines(file.path(vd, "index.html"), warn = FALSE), collapse = "\n")
js <- jsonlite::fromJSON(regmatches(h, regexpr('(?s)(?<=type="application/json">).*?(?=</script>)', h, perl = TRUE)))
check(nrow(js$flights) == 7 && nrow(js$points) > 0 && all(c("flights.csv", "links.csv", "sources.csv") %in% list.files(vd)), "viewer holds every flight, thinned image points and the tables")
check(sum(file.size(list.files(vd, full.names = TRUE))) < 2e6, "viewer is lightweight")

# ---- source C: legacy aircraft and other records -------------------------------------------------------------------------
C <- file.path(tmp, "Legacy")
p4 <- file.path(C, "2017", "Phantom4_Bog"); dir.create(p4, recursive = TRUE)
for (i in 1:2) jpg(file.path(p4, sprintf("DJI_000%d.JPG", i)), exif_app1("DJI", "FC330", "P4CAM", sprintf("2017:07:01 12:00:0%d", i), 60.13, -120.08, 300),
                   '<x:xmpmeta xmlns:x="adobe:ns:meta/"><rdf:Description drone-dji:RelativeAltitude="+80.0"/></x:xmpmeta>')
pr <- file.path(C, "2021", "P4RTK_Fen"); dir.create(pr, recursive = TRUE)
for (i in 1:2) jpg(file.path(pr, sprintf("DJI_002%d.JPG", i)), exif_app1("DJI", "FC6310R", "P4RCAM", sprintf("2021:06:10 11:00:0%d", i), 60.14, -120.07, 300),
                   sub('drone-dji:DroneModel="Matrice 350 RTK" ', '', dji_xmp(sprintf("2021-06-10T18:00:0%d", i), 50, "P4RCAM")), pad = 36000L)
# a base station logging from before to after the P4 RTK flight, at about 60.1 N, 120.1 W
writeLines(c(sprintf("%-60s%s", "     3.02           OBSERVATION DATA    M: Mixed", "RINEX VERSION / TYPE"),
             sprintf("%-60s%s", "BASE1", "MARKER NAME"), sprintf("%-60s%s", "123                 EMLID REACH RS2     2.24", "REC # / TYPE / VERS"),
             sprintf("%-60s%s", "  -1579885.0000 -2735203.0000  5527711.0000", "APPROX POSITION XYZ"),
             sprintf("%-60s%s", "", "END OF HEADER"), "> 2021  6 10 17 30  0.0000000  0  1", "G01  21251980.711", "> 2021  6 10 19 00  0.0000000  0  1", "G01  21251980.711"),
           file.path(C, "2021", "base_1610.21o"))
# Sequoia bands in eMotion's img/msp folder; the flight's logs two folders up; an eMotion KML track listing the images
sq <- file.path(C, "2018-08-30_SiteC"); dir.create(file.path(sq, "img", "msp"), recursive = TRUE)
for (k in 0:1) for (band in c("GRE", "NIR", "RED", "REG"))
  writeBin(c(exif_app1("Parrot", "Sequoia", "SQ1", sprintf("2018:08:30 22:23:3%d", k), 60.10 + k / 1e4, -120.12, 380, tiff_only = TRUE), as.raw(rep(0, 500)),
             charToRaw(sprintf('<x:xmpmeta xmlns:x="adobe:ns:meta/"><rdf:Description Camera:RigName="Sequoia" Camera:CaptureUUID="CAP%d" Camera:FlightUUID="FLT66" Camera:GPSXYAccuracy="3.97"/></x:xmpmeta>', k))),
           file.path(sq, "img", "msp", sprintf("IMG_180830_22233%d_000%d_%s.TIF", k, k, band)))
dir.create(file.path(sq, "img", "rgb")); file.copy(file.path(sq, "img", "msp", "IMG_180830_222330_0000_GRE.TIF"), file.path(sq, "img", "rgb", "IMG_180830_222330_0000_RGB.TIF"))
writeLines(c("<pix4dmapper><inputs><images>", '<image path="C:\\Users\\x\\img\\msp\\IMG_180830_222330_0000_GRE.TIF"/>', '<image path="C:/x/img/msp/IMG_180830_222331_0001_NIR.TIF"/>', '<time>2018:08:30 22:23:30</time>', '<gps alt="380" lat="60.10" lng="-120.12"/>', "</images></inputs></pix4dmapper>"), file.path(sq, "2018_08_30_sitec.p4d"))   # Pix4D project beside img/: linked by its image names
check(identical(read_p4d(file.path(sq, "2018_08_30_sitec.p4d"))$p4d_files, "IMG_180830_222330_0000_GRE.TIF;IMG_180830_222331_0001_NIR.TIF") &&
      identical(read_p4d(file.path(sq, "2018_08_30_sitec.p4d"))[c("p4d_lat_min", "p4d_start_utc")], list(p4d_lat_min = 60.1, p4d_start_utc = "2018-08-30 22:23:30")),
      "Pix4D project: image names, positions and times read (Windows and / paths)")
writeBin(as.raw(1:9), file.path(sq, "EP-01-00001_0066.bb3")); writeBin(as.raw(1:9), file.path(sq, "EP-01-00001_0066_em.bb3"))
writeLines(c('<kml xmlns:gx="http://www.google.com/kml/ext/2.2"><Document><Placemark><gx:Track><when>2018-08-30T22:20:00Z</when><when>2018-08-30T22:30:00Z</when>',
             '<gx:coord>-120.12 60.10 380</gx:coord><gx:coord>-120.121 60.101 380</gx:coord></gx:Track></Placemark>',
             '<Placemark><name>IMG_180830_222330_0000_GRE.tif</name><Point><coordinates>-120.12,60.10,380</coordinates></Point></Placemark></Document></kml>'),
           file.path(sq, "2018_08_30_sitec.kml"))
writeLines('<kml><Placemark><LineString><coordinates>-120.10,60.10,0 -120.11,60.11,0</coordinates></LineString></Placemark></kml>', file.path(C, "plan_bog.kml"))
dir.create(file.path(C, "overlay", "14"), recursive = TRUE)
for (t in c("11746", "11747")) writeLines(c('<kml><Document><Folder><Region><LatLonAltBox><north>60.10</north><south>60.09</south><east>-120.08</east><west>-120.11</west></LatLonAltBox>',
  '<Lod><minLodPixels>128</minLodPixels></Lod></Region><GroundOverlay><Icon><href>11746.png</href></Icon></GroundOverlay></Folder></Document></kml>'), file.path(C, "overlay", "14", paste0(t, ".kml")))
nir <- file.path(C, "2026-09-08_EBEE_NIR_351"); dir.create(file.path(nir, "img"), recursive = TRUE)
for (i in 1:2) jpg(file.path(nir, "img", sprintf("IMG_488%d.JPG", i)), exif_app1("Canon", "Canon PowerShot S110", "", sprintf("1980:01:01 00:03:0%d", i)))
writeLines(c('<kml><Document><Placemark><gx:Track><when>2026-09-07T17:26:17Z</when><when>2026-09-07T17:52:32Z</when>',
             '<gx:coord>-120.10 60.0995 400</gx:coord><gx:coord>-120.101 60.0996 400</gx:coord></gx:Track></Placemark>',
             '<Placemark><name>IMG_4881.JPG</name></Placemark><Placemark><name>IMG_4882.JPG</name></Placemark></Document></kml>'), file.path(nir, "2026_09_08_ebee_nir_351.kml"))
dir.create(file.path(C, "BBX")); writeBin(as.raw(1:9), file.path(C, "BBX", "TM-01-00002_00002.bbx"))
dir.create(file.path(sq, "3_dsm_ortho", "2_mosaic"), recursive = TRUE); writeBin(as.raw(1:9), file.path(sq, "3_dsm_ortho", "2_mosaic", "sitec_ortho.tif"))
jpg(file.path(sq, "3_dsm_ortho", "preview.jpg"), exif_app1("Parrot", "Sequoia", "SQ1", "2018:08:30 23:59:00", 60.1, -120.1, 1))

r3 <- run_pipeline(list(project_dir = proj), scan = C, source_id = "Legacy", export = TRUE); f3 <- r3$flights
check(nrow(f3) == 11, "legacy source adds four flights; processed outputs make none")
cn <- f3[flight_no == 351]
check(nrow(cn) == 1 && cn$time_source == "flown track (KML)" && cn$date == "2026-09-07" && grepl("time from flown track", cn$qa_flags), "Canon S110 with no clock takes its time from the linked eMotion track")
p4f <- f3[aircraft == "Phantom 4"]
check(nrow(p4f) == 1 && p4f$positioning_status == "standalone GNSS / uncorrected" && grepl("has no RTK", p4f$positioning_evidence), "Phantom 4: aircraft from the camera model, standalone")
prf <- f3[aircraft == "Phantom 4 RTK"]
check(nrow(prf) == 1 && prf$positioning_status == "RTK corrected" && prf$has_gnss_obs, "Phantom 4 RTK: RTK corrected; base station RINEX linked by time")
check(r3$links[grepl("base_1610", rel)]$state == "probable", "RINEX spanning a flight is a probable link, never proof")
sqf <- f3[sensor == "Parrot Sequoia"]
check(nrow(sqf) == 1 && sqf$n_images == 9 && sqf$n_captures == 2 && any(r3$links$flight_id == sqf$flight_id & grepl("sitec.p4d$", r3$links$rel)) && sqf$flight_no == 66 && sqf$has_log && sqf$aircraft == "senseFly eBee Plus" &&
      sqf$positioning_status == "standalone GNSS / uncorrected", "Sequoia: msp and rgb one flight, Pix4D project above img/ linked; four bands per capture, flight number from logs above, eBee Plus, standalone")
g3 <- r3$geometry
check({ ls_c <- list_source(C); all(ls_c[grepl("^overlay/", rel), status] == "map tile") && ls_c[file == "plan_bog.kml", status] == "" },
      "Google Earth tile KMLs are listed as map tiles, a flight plan KML is still read")
check(any(g3$kind == "observed" & g3$flight_id %in% sqf$flight_id & grepl("track", g3$what)) && any(g3$kind == "planned" & !is.na(g3$flight_id) & g3$ref == "Legacy:plan_bog.kml") &&
      any(g3$what == "GNSS receiver BASE1") && any(g3$kind == "derived"), "geometry: flown track linked, plan matched to its flight by area, receiver placed, extents derived")
check(all(r3$files[grepl("3_dsm_ortho", rel)]$status == "out of scope"), "processed products marked out of scope")
check(r3$files[grepl("bbx$", rel)]$role == "flight_log", "BBX log listed")

# satellite, aerial and LiDAR: a WorldView .IMD, a GeoTIFF in UTM 10N with an IMD of the same scene, a LAS tile, an aerial TIFF with a world file
# a separate folder of RTK and geotag files, buried in subfolders and renamed: matched to the DJI and eBee flights anyway
sup <- file.path(dirname(proj), "Support"); dir.create(file.path(sup, "rtk", "deep", "er"), recursive = TRUE)
file.copy(file.path(f1, "DJI_202409211138_009_LakeA-RGB_Timestamp.MRK"), file.path(sup, "rtk", "deep", "er", "lakea_rtk.MRK"))
file.copy(file.path(e2, "2018_08_28_north_transect_geoinfo.txt"), file.path(sup, "rtk", "copy_geoinfo.txt"))
r5 <- run_pipeline(list(project_dir = proj), scan = sup, source_id = "Support"); l5 <- r5$links
check(any(l5$rel == "rtk/deep/er/lakea_rtk.MRK" & l5$state == "auto" & l5$flight_id %in% r5$flights[grepl("Lake ?A", site)]$flight_id) &&
      any(l5$rel == "rtk/copy_geoinfo.txt" & l5$state %in% c("auto", "probable") & l5$flight_id == r5$flights[flight_no == 53]$flight_id),
      "RTK and geotag files in a separate folder's subfolders matched to their DJI and eBee flights")
rs <- file.path(dirname(proj), "RS_archive"); dir.create(file.path(rs, "WorldView 2017"), recursive = TRUE); dir.create(file.path(rs, "Aerial photographs")); dir.create(file.path(rs, "LiDAR 2018"))
writeLines(c('satId = "WV03";', 'bandId = "Multi";', 'productLevel = "LV2A";', 'cloudCover = 0.05;', 'firstLineTime = 2017-09-12T19:35:43.1Z;', 'meanCollectedGSD = 1.4;',
             'BEGIN_GROUP = BAND_C', '\tULLon = -120.0834;', '\tULLat = 60.1978;', '\tURLon = -120.0459;', '\tURLat = 60.1974;', '\tLRLon = -120.0487;', '\tLRLat = 60.1475;',
             '\tLLLon = -120.0862;', '\tLLLat = 60.1480;', 'END_GROUP = BAND_C'), file.path(rs, "WorldView 2017", "17SEP12193544-M2AS-0104_P002.IMD"))
geotiff <- function(path, x0, y0, px, w, h, epsg) {   # header, IFD at the end like ArcGIS writes it
  ent <- function(tag, typ, cnt, val) c(le16(tag), le16(typ), le32(cnt), val)
  body <- as.raw(rep(0, 64)); dbl <- function(x) writeBin(as.double(x), raw(), size = 8, endian = "little")
  sc <- dbl(c(px, px, 0)); tp <- dbl(c(0, 0, 0, x0, y0, 0)); gk <- unlist(lapply(c(1, 1, 0, 2, 1024, 0, 1, 1, 3072, 0, 1, epsg), le16))
  o_sc <- 8 + length(body); o_tp <- o_sc + 24; o_gk <- o_tp + 48; o_ifd <- o_gk + length(gk)
  ifd <- c(le16(5), ent(256, 3, 1, c(le16(w), le16(0))), ent(257, 3, 1, c(le16(h), le16(0))), ent(33550, 12, 3, le32(o_sc)), ent(33922, 12, 6, le32(o_tp)),
           ent(34735, 3, length(gk) / 2, le32(o_gk)), le32(0))
  writeBin(c(charToRaw("II"), le16(42), le32(o_ifd), body, sc, tp, gk, ifd), path) }
geotiff(file.path(rs, "WorldView 2017", "o17SEP12193544-M2AS-0104_P002.tif"), 661762, 6677016, 2, 1020, 2780, 32610)
geotiff(file.path(rs, "Aerial photographs", "A24750_032_ortho_sub.tif"), 666205, 6674794, 0.32, 13970, 15061, 26910)
las <- c(charToRaw("LASF"), as.raw(rep(0, 20)), as.raw(c(1, 2)), as.raw(rep(0, 64)), le16(200), le16(2018), le16(227), le32(227 + 54 + 16), le32(1), as.raw(0), le16(20), le32(1000),
         as.raw(rep(0, 20)), writeBin(c(0.01, 0.01, 0.01, 0, 0, 0, 663069, 662069, 6674715, 6673715, 300, 200), raw(), size = 8, endian = "little"),
         le16(0), charToRaw("LASF_Projection"), as.raw(0), le16(34735), le16(16), as.raw(rep(0, 32)), unlist(lapply(c(1, 1, 0, 1, 3072, 0, 1, 3157), le16)))   # GeoKeys: NAD83(CSRS) UTM 10N
pt <- function(cl) c(as.raw(rep(0, 15)), as.raw(cl), as.raw(rep(0, 4)))          # point format 0, 20 bytes: classification at byte 15
writeBin(c(las, pt(2), pt(2), pt(5)), file.path(rs, "LiDAR 2018", "C2-20180725f_Site-L001-r.las"))
# GIS: a polygon shapefile of plots in UTM 10N (.prj), a point shapefile of GCPs beside it with no .prj, a GeoJSON boundary, a GPX track
be32 <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "big"); d8 <- function(x) writeBin(as.double(x), raw(), size = 8, endian = "little")
shp <- function(path, type, recs) { body <- unlist(lapply(seq_along(recs), function(i) { r <- recs[[i]]; c(be32(i), be32(length(r) / 2), r) }))
  xy <- c(661166, 6665696, 661366, 6665896)
  writeBin(c(be32(9994), as.raw(rep(0, 20)), be32((100 + length(body)) / 2), le32(1000), le32(type), d8(c(xy, 0, 0, 0, 0)), body), path) }
dir.create(file.path(rs, "Field GIS")); pr <- 'PROJCS["NAD_1983_CSRS_UTM_Zone_10N",GEOGCS["GCS_North_American_1983_CSRS"]]'
ring <- cbind(c(661166, 661366, 661366, 661166, 661166), c(6665696, 6665696, 6665896, 6665896, 6665696))
shp(file.path(rs, "Field GIS", "veg_plots.shp"), 5, list(c(le32(5), d8(c(661166, 6665696, 661366, 6665896)), le32(1), le32(5), le32(0), d8(t(ring)))))
writeLines(pr, file.path(rs, "Field GIS", "veg_plots.prj"))
shp(file.path(rs, "Field GIS", "GCP_2024.shp"), 1, list(c(le32(1), d8(c(661266, 6665796))), c(le32(1), d8(c(661316, 6665846)))))
writeLines('{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"name":"Study area"},"geometry":{"type":"Polygon","coordinates":[[[-120.11,60.09],[-120.09,60.09],[-120.09,60.11],[-120.11,60.09]]]}}]}', file.path(rs, "Field GIS", "site_boundary.geojson"))
writeLines('<gpx><trk><trkseg><trkpt lat="60.10" lon="-120.10"/><trkpt lat="60.101" lon="-120.101"/></trkseg></trk></gpx>', file.path(rs, "Field GIS", "walk.gpx"))
r4 <- run_pipeline(list(project_dir = proj), scan = rs, source_id = "RS", export = TRUE)
gl <- fread(file.path(proj, "output", "catalogue", "gis_layers.csv")); gf <- fread(file.path(proj, "output", "catalogue", "gis_features.csv"))
check(nrow(gl) == 4 && setequal(gl$category, c("Plots, transects and grids", "GCPs and checkpoints", "Boundaries and extents", "Other GIS layers")), "GIS layers listed and sorted by kind from their names")
pl <- gf[grepl("veg_plots", rel)]; ll <- as.numeric(strsplit(strsplit(pl$line, ";")[[1]][1], " ")[[1]])
check(nrow(pl) == 1 && pl$type == "polygon" && abs(ll[1] - 60.09653) < 1e-4 && abs(ll[2] - -120.1015) < 1e-4, "shapefile polygon in UTM 10N placed in latitude/longitude")
check(nrow(gf[grepl("GCP_2024", rel)]) == 2 && grepl("inferred", gl[grepl("GCP_2024", rel)]$crs), "GCP points without a .prj use the folder's .prj, marked inferred")
check(nrow(gf[grepl("geojson|gpx", rel)]) == 2, "GeoJSON boundary and GPX track read")
vh <- paste(readLines(file.path(proj, "output", "viewer", "Ovid_RS_viewer", "index.html"), warn = FALSE), collapse = "")
check(grepl("WorldView-3", vh) && grepl("GCPs and checkpoints", vh) && file.exists(file.path(proj, "output", "viewer", "Ovid_RS_viewer", "gis_features.csv")), "viewer carries satellite footprints and GIS layers")
ds <- fread(file.path(proj, "output", "catalogue", "datasets.csv"))
wv <- ds[grepl("^o17SEP", file)]; ae <- ds[grepl("^A24750", file)]; li <- ds[grepl("las$", file)]; imd <- ds[grepl("IMD$", file)]
check(nrow(ds) == 4 && imd$platform == "WorldView-3" && imd$cloud_pct == 5 && abs(imd$lat_max - 60.1978) < 1e-6, "WorldView .IMD: satellite, cloud, scene corners")
check(wv$platform == "WorldView-3" && grepl("2017-09-12", wv$acquired) && abs(wv$lat_max - 60.1978) < 0.001 && abs(wv$lon_min - -120.086) < 0.002, "GeoTIFF footprint from its tags (IFD at the end) matches the scene corners")
check(ae$kind == "aerial" && is.na(ae$acquired) && ae$gsd_m == 0.32, "aerial photo: kind from the folder, no date invented")
check(li$kind == "lidar" && substr(as.character(li$acquired), 1, 10) == "2018-07-25" && grepl("inferred", li$date_source) && li$n_points == 1000 && is.finite(li$lat_min), "LAS tile: bounds, points, date from the name (inferred)")
check(isTRUE(all.equal(li$point_density, 0.001)) && identical(li$classes, "2,5 (first 3 points)"), "LAS tile: points per m2 of its box, and the classes in the points read")
check(nrow(r4$flights) == nrow(f3), "satellite, aerial and LiDAR files make no flights")

# big folders: only a sample of image headers read, the rest filled from neighbours without positions of their own
g <- data.table(rel = sprintf("a/%03d.JPG", 1:200), folder = "a", file = sprintf("%03d.JPG", 1:200), size = 1); i <- sample_idx(200)
rs <- g[i, .(rel, folder, file, size)][, `:=`(sensor = "P1", lat = 60 + i / 1e4, time_utc = as.POSIXct("2024-06-01", tz = "UTC") + i * 2)]
fs <- fill_sampled(rs, g, i)
check(length(i) == IMAGE_SAMPLE && nrow(fs) == 200 && all(fs$sensor == "P1") && sum(!is.na(fs$lat)) == length(i) &&
      abs(as.numeric(fs$time_utc[100]) - as.numeric(as.POSIXct("2024-06-01", tz = "UTC")) - 200) < 1e-6 && sum(fs$meta_from == "read") == length(i),
      "big folder: sampled headers, camera and interpolated time for the rest, no invented positions")

check(identical(unname(md5_before), unname(tools::md5sum(all_raw()))) && length(all_raw()) == length(md5_before), "sources unchanged, nothing written in them")
# flight quality from the image headers: two parallel lines, 20 m between images (2 s), lines 45 m apart, 100 m up,
# a 1-inch sensor 13.2 mm wide at 8.8 mm focal length, 5472 x 3648 pixels, 1/1000 s
t0f <- as.POSIXct("2024-07-01 18:00:00", tz = "UTC"); lat0 <- 60.1; mlat <- 1 / 111194.9; mlon <- 1 / (111194.9 * cos(lat0 * pi / 180))
fq <- rbind(data.table(k = 0:9, line = 1L), data.table(k = 0:9, line = 2L))
fq[, `:=`(flight_id = "F1", source_id = "s", folder = "f", primary = TRUE, t = t0f + 2 * (k + 10 * (line - 1)) + 60 * (line - 1),
          lat = lat0 + fifelse(line == 1L, k * 20, (9 - k) * 20) * mlat, lon = -120 + (line - 1) * 45 * mlon,
          alt_rel = 100, focal_mm = 8.8, px_x = 5472, px_y = 3648, sensor_w_mm = 13.2, exposure_s = 0.001)]
fm <- flight_metrics(fq)
check(abs(fq$gsd_cm[1] - 2.741) < 0.01 && abs(fm$gsd_cm_median - 2.74) < 0.01, "ground sample distance from sensor width, height and focal length (2.74 cm)")
check(abs(fm$fwd_overlap_pct_median - 80) < 0.5 && abs(fm$side_overlap_pct_median - 70) < 0.5 && fm$fwd_overlap_source == "image pairs", "forward overlap 80 % and side overlap 70 % from image spacing and footprint")
fq3 <- copy(fq)[, `:=`(speed_x = 6, speed_y = 8)][!(k %in% c(0, 5)), `:=`(lat = NA_real_, lon = NA_real_)]   # a sampled big folder: few positions, every time, DJI speeds
fm3 <- flight_metrics(fq3)
check(fm3$fwd_overlap_source == "speed x interval" && abs(fm3$fwd_overlap_pct_median - 80) < 1 && is.na(fm3$side_overlap_pct_median), "with sampled headers, forward overlap from speed x interval and no side overlap")
check(abs(fm$speed_ms_median - 10) < 0.05 && abs(fm$blur_px_median - 0.36) < 0.02 && fm$blur_over_limit_pct == 0 && fm$blur_limit_px == 1 && is.finite(fm$sun_elev_max_deg), "speed from positions, motion blur in pixels, sun elevation")
fq2 <- copy(fq)[, `:=`(speed_x = 3, speed_y = 4)]; check(abs(flight_metrics(fq2)$speed_ms_median - 5) < 1e-6, "DJI's own flight speed wins over positions")
check(unname(SENSOR_WIDTH_MM["FC6310"]) == 13.2 && unname(SENSOR_WIDTH_MM["FC330"]) == 6.17, "sensor widths for cameras whose EXIF has none")
# GNSS quality fields
gq <- tempfile(fileext = ".csv"); writeLines(c("Name,Longitude,Latitude,Ellipsoidal height,Lateral RMS,Elevation RMS,Solution status,Samples,PDOP,GPS Satellites,GLONASS Satellites,Galileo Satellites,Baseline,Correction type",
  "P1,-120.1,60.1,250.1,0.012,0.018,FIX,30,1.2,8,6,5,1520.4,NTRIP", "P2,-120.11,60.11,251.0,0.40,0.80,FLOAT,30,2.6,5,3,2,1530.0,NTRIP"), gq)
gd <- read_gnss_file(gq)
check(identical(gd$n_sat, c(19, 10)) && identical(gd$pdop, c(1.2, 2.6)) && identical(gd$samples, c(30, 30)) && identical(gd$baseline_m, c(1520.4, 1530)) && all(gd$correction == "NTRIP"),
      "Emlid Flow: satellites summed over constellations, PDOP, samples, baseline and correction type")
llh <- tempfile(fileext = ".LLH"); writeLines(c("2024/07/01 18:00:00.000   60.100000000 -120.100000000   250.1000   1  14   0.0100   0.0100   0.0200   0.0000   0.0000   0.0000   1.40    5.2",
  "2024/07/01 18:00:01.000   60.100000010 -120.100000010   250.1000   2  11   0.0500   0.0500   0.0900   0.0000   0.0000   0.0000   2.60    1.1"), llh)
gl <- read_gnss_file(llh)
check(identical(gl$n_sat, c(14, 11)) && identical(gl$age_s, c(1.4, 2.6)) && identical(gl$solution, c("FIX", "FLOAT")), "Reach .LLH: satellites and age of the corrections")
nm <- tempfile(fileext = ".nmea"); writeLines(c("$GNGGA,180000.00,6006.0000,N,12006.0000,W,4,15,0.8,250.0,M,-18.0,M,1.0,0000*00"), nm)
gn <- read_gnss_file(nm)
check(gn$n_sat == 15 && gn$hdop == 0.8 && gn$age_s == 1 && gn$solution == "FIX", "NMEA GGA: satellites, HDOP and age")
qq <- gnss_quality(rbind(gd, gl, fill = TRUE))
check(nrow(qq) == 2 && qq[grepl("csv$", source_file), fixed_pct] == 50 && qq[grepl("csv$", source_file), min_satellites] == 10 && qq[grepl("LLH$", source_file), max_age_s] == 2.6, "quality by file: fixed share, fewest satellites, oldest corrections")

# GNSS base check: single averaging flagged unless post-processed; nothing recorded -> needs you; custom limit needs a source
gb <- tempfile(fileext = ".csv"); writeLines(c("Name,Code,Longitude,Latitude,Ellipsoidal height,Solution status", "BASE1,base,-120.1,60.1,250,SINGLE", "P1,,-120.11,60.11,251,FIX"), gb)
gbd <- rbind(read_gnss_file(gb, "day1.csv"), read_gnss_file(gb, "day2.csv"), read_gnss_file(gb, "day3.csv"), read_gnss_file(gb, "day4.csv"))
bases <- data.table(file = c("day1.csv", "day2*", "day4.csv"), base = c("B1", "B1", "B3"), method = c("single averaging", "single averaging", "float averaging"), post_processed = c("", "yes", ""), service = c("", "NRCan CSRS-PPP", ""), notes = "")
RS_CAL0 <- calibration_default(RS_CALIBRATION)
bc <- gnss_base_check(gbd, bases, RS_CAL0)
check(bc[source_file == "day1.csv", base_check] == "flag" && grepl("2.5 m", bc[source_file == "day1.csv", base_note]) &&
      bc[source_file == "day2.csv", base_check] == "ok" && grepl("CSRS-PPP", bc[source_file == "day2.csv", base_note]) &&
      bc[source_file == "day3.csv", base_check] == "needs you" && grepl("BASE1 has a single solution", bc[source_file == "day3.csv", base_note]) &&
      bc[source_file == "day4.csv", base_check] == "flag" && grepl("1 m", bc[source_file == "day4.csv", base_note]),
      "GNSS base: single (2.5 m) and float (1 m) averaging flagged, post-processed ok, unrecorded base needs you (suggestion only)")
bn <- gnss_base_check(gbd[source_file %in% c("day1.csv", "day2.csv")], data.table(file = c("day1.csv", "day2.csv"), base = "N1", method = c("network (NTRIP / CORS)", "Network (ntrip / cors)"),
                                                                              post_processed = "", service = "", notes = ""), RS_CAL0)
check(all(bn$base_check == "ok") && all(bn$base_method == "network (NTRIP / CORS)"), "GNSS base: a method is matched whatever its case, so a network base is ok, not needs you")
cal <- rbind(RS_CAL0, data.table(app = "RS", value = "base_single_avg_error_m", applies_to = "B1", choice = "custom", number = "4", source = "own test", notes = ""),
             data.table(app = "RS", value = "base_single_avg_error_m", applies_to = "B2", choice = "custom", number = "9", source = "", notes = ""))
check(rs_calibration(cal, "base_single_avg_error_m", "B1")$number == 4 && rs_calibration(cal, "base_single_avg_error_m", "B2")$number == 9 && rs_calibration(cal, "base_single_avg_error_m", "B2")$source == "no source given" &&
      grepl("Scotty Creek option", RS_CAL0[value == "checkpoint_survey_rmse_v_cm", notes]) && !grepl("Scotty", RS_CAL0[value == "blur_limit_px", notes]) &&
      rs_calibration(NULL, "base_single_avg_error_m")$choice == "default" && grepl("4 m", gnss_base_check(gbd, bases, cal)[source_file == "day1.csv", base_note]),
      "calibration: a custom value wins for its base, a missing source is only a note; Scotty Creek offered only where it has a value")
check(flight_metrics(fq, blur_limit_px = 0.3)$blur_over_limit_pct > 0, "blur share follows the calibrated limit")
# calibrated GNSS limits: age, baseline and fixed share; Scotty Creek checkpoint survey error offered as an option
lq <- data.table(source_file = c("a.csv", "b.csv"), max_age_s = c(4, 12), max_baseline_m = c(1500, 72000), fixed_pct = c(100, 60))
lc <- gnss_limit_check(lq, rbind(RS_CAL0, data.table(app = "RS", value = "min_fixed_pct", applies_to = "*", choice = "custom", number = "90", source = "own rule", notes = ""), fill = TRUE))
check(lc[source_file == "a.csv", limits] == "ok" && grepl("12 s old", lc[source_file == "b.csv", limits]) && grepl("72 km", lc[source_file == "b.csv", limits]) && grepl("60 % fixed", lc[source_file == "b.csv", limits]),
      "GNSS limits: corrections age 10 s and baseline 60 km by default, a custom minimum fixed share")
check(cal_num(data.table(app = "RS", value = "checkpoint_survey_rmse_v_cm", applies_to = "*", choice = "scotty_creek", number = "", source = "", notes = ""), "checkpoint_survey_rmse_v_cm") == 2 &&
      cal_num(NULL, "checkpoint_survey_rmse_v_cm") == 3 && is.na(cal_num(NULL, "min_sun_elev_deg")), "calibration: Scotty Creek value from the code, blank default means no limit")
# vertical bias: a constant +5 cm offset is flagged, scatter around zero is not
cb <- data.table(id = 1:10, x_survey = 0, y_survey = 0, z_survey = 0, x_product = 0, y_product = 0, z_product = 0.05 + rep(c(-0.005, 0.005), 5))
cn <- copy(cb)[, z_product := rep(c(-0.05, 0.05), 5)]
rb <- checkpoint_accuracy(cb); rn <- checkpoint_accuracy(cn)
check(isTRUE(rb$summary$vertical_bias) && any(grepl("Vertical bias", rb$notes)) && isFALSE(rn$summary$vertical_bias) && !any(grepl("Vertical bias", rn$notes)),
      "checkpoints: a mean vertical error beyond twice its standard error is reported as a bias")

# ---- a rebuild applies a new ignore rule from the last scans, without reading the source again ------------------------
check(identical(ebee_flight_no(c("EP-01-00001_0359_0005.JPG", "x.JPG", "TM-01-00002_00002.bbx"))$no, c(359L, NA, 2L)) &&
      identical(folder_flight_no(c("a/EBEE_0303", "a/EBEE_0303", "DJI_2024_009", "b/NIR_BogA_354", "")), c(303L, 303L, NA, 354L, NA)), "flight numbers from names and folders")
im0 <- fread(file.path(proj, "output", "catalogue", "images.csv")); fl0 <- fread(file.path(proj, "output", "catalogue", "flights.csv"))
one <- im0[!is.na(flight_id) & primary %in% TRUE & source_id == "T7" & !is.na(time_utc)][order(flight_id, time_utc)][, .SD[.N], by = flight_id][1]; fid <- one$flight_id;   # not the first image: that names the flight
 n0 <- fl0[flight_id == fid, n_images]
fwrite(data.table(pattern = paste0("^", gsub("([.|()\\\\^{}+$*?\\[\\]])", "\\\\\\1", one$rel, perl = TRUE), "$"), action = "ignore", reason = "test"), file.path(proj, "config", "file_rules.csv"))
r6 <- run_pipeline(list(project_dir = proj), scan = character())
fa6 <- fread(file.path(proj, "output", "audit", "file_audit.csv"))
check(!one$rel %in% r6$images[source_id == one$source_id, rel] && r6$flights[flight_id == fid, n_images] == n0 - 1L &&
      r6$files[source_id == one$source_id & rel == one$rel, status] == "ignored",
      "an ignore rule added after a scan applies on the next rebuild, without reading the source again")
fwrite(data.table(pattern = character(), action = character(), reason = character()), file.path(proj, "config", "file_rules.csv"))
r7 <- run_pipeline(list(project_dir = proj), scan = A)
check(one$rel %in% r7$images[source_id == one$source_id, rel] && r7$flights[flight_id == fid, n_images] == n0, "with the rule taken out, a rescan brings the image back")

# ---- checkpoint accuracy (ASPRS Edition 2, 2023) ----------------------------------------------------------------------
cp <- data.table(id = paste0("CP", 1:4), x_survey = c(0, 10, 20, 30), y_survey = 0, z_survey = 100, x_product = c(0.03, 9.97, 20.03, 29.97), y_product = c(0.04, -0.04, 0.04, -0.04), z_product = c(100.05, 99.95, 100.05, 99.95))
ca <- checkpoint_accuracy(cp, 0, 0)
check(ca$summary$rmse_h_cm == 5 && ca$summary$rmse_v_cm == 5 && isTRUE(all.equal(ca$summary$rmse_3d_cm, 7.1)) && any(grepl("at least 30", ca$notes)) && grepl("tested against", ca$statement),
      "checkpoint accuracy: RMSE_H, RMSE_V and RMSE_3D in cm, reported for information with fewer than 30 points")
ca2 <- checkpoint_accuracy(cp, 0.03, 0.04)
check(ca2$summary$rmse_h_cm == round(100 * sqrt(0.05^2 + 0.03^2), 1) && any(grepl("not twice as accurate", ca2$notes)), "the checkpoints' own survey error is added, and checked against the product's")

# frames and datums, products, coverage, STAC
check(identical(frame_of_crs(c(4326, 26910, 3157, NA, NA), c("", "", "", "NAD83(CSRS) / UTM zone 10N", "")), c("WGS 84", "NAD83 (version not stated)", "NAD83(CSRS)", "NAD83(CSRS)", "")) &&
      crs_status(NA, "") == "datum unknown" && grepl("epoch not stated", crs_status(32610, "")), "frames from EPSG codes and names; nothing stated is datum unknown")
check("crs_status" %in% names(ds) && all(nzchar(ds$crs_status)), "every dataset carries its frame and whether its datum is known")
fid <- f3$flight_id[1]
prd <- products_table(data.table(product_id = "ortho-2024", kind = "Orthomosaic", file = "x/ortho.tif", flights = paste(fid, "UAV-nothere"), software = "Pix4D", version = "4.8",
                                 epsg = "3157", frame = "", epoch = "", level = "", made = "2024-08-01"), ds, r4$flights)
check(prd$level == "L2" && prd$flights_found == "1 of 2" && !prd$file_found && grepl("NAD83\\(CSRS\\)", prd$crs_status), "products: level from the kind, flights checked, frame from the EPSG code")
cv <- fread(file.path(proj, "output", "catalogue", "coverage.csv"))
check(nrow(cv[scope == "flights"]) > 0 && sum(cv[scope == "flights", count]) == nrow(r4$flights) && nrow(cv[scope == "datasets"]) > 0, "coverage counts every flight by site, year and sensor, and the datasets by year")
sx <- stac_export(file.path(tmp, "stac"), r4$flights, ds, prd); cat0 <- jsonlite::fromJSON(file.path(tmp, "stac", "catalog.json"), simplifyVector = FALSE)
kids <- vapply(Filter(function(l) l$rel == "child", cat0$links), `[[`, "", "href")
it1 <- list.files(file.path(tmp, "stac"), "\\.json$", recursive = TRUE); it1 <- it1[!basename(it1) %in% c("catalog.json", "collection.json")]
j1 <- jsonlite::fromJSON(file.path(tmp, "stac", it1[1]), simplifyVector = FALSE)
check(cat0$type == "Catalog" && length(kids) == sx$collections && all(file.exists(file.path(tmp, "stac", sub("^\\./", "", kids)))) && "products" %in% basename(dirname(kids)) &&
      length(it1) == sx$items && j1$type == "Feature" && j1$stac_version == "1.0.0" && length(j1$bbox) == 4 && !is.null(j1$properties$datetime) && !is.null(j1$collection),
      "STAC catalogue: collections and items linked, each item with a box, a time and its collection")
check(is.null(stac_time("")) && is.null(stac_time(NA)) && is.null(stac_time(NULL)) && is.null(stac_time("sometime")) && stac_time("2018-08-29 00:30:31") == "2018-08-29T00:30:31Z" &&
      stac_time("2018-08-29T00:30:31.25Z") == "2018-08-29T00:30:31Z" && stac_time("2019") == "2019-01-01T00:00:00Z" && stac_time("2019-06") == "2019-06-01T00:00:00Z" &&
      stac_time(as.POSIXct("2020-01-02 03:04:05", tz = "UTC")) == "2020-01-02T03:04:05Z", "STAC times: blank or unreadable is none; ISO, fractional seconds, a year or a month alone are read")
ds2 <- data.table(rel = c("a/blank.tif", "b/2019/year.tif", "c/day.tif"), source_id = "S", kind = "aerial", acquired = c("", "2019", "2021-05-04"), epsg = "4326", crs = "",
                  lon_min = -120, lat_min = 50, lon_max = -119.9, lat_max = 50.1)
sx2 <- tryCatch(stac_export(file.path(tmp, "stac2"), NULL, ds2), error = function(e) conditionMessage(e))
yj <- if (is.list(sx2)) jsonlite::fromJSON(file.path(tmp, "stac2", "imagery-and-lidar", "S-b-2019-year.tif", "S-b-2019-year.tif.json"), simplifyVector = FALSE)
check(is.list(sx2) && sx2$items == 2L && yj$properties$start_datetime == "2019-01-01T00:00:00Z" && yj$properties$end_datetime == "2019-12-31T23:59:59Z",
      "STAC catalogue is written when a dataset has no date (that one is left out) and a year alone becomes the whole year")
check(nrow(products_table(NULL)) == 0L && "crs_status" %in% names(products_table(NULL)) && nrow(stitch_gnss(character())) == 0L &&
      all(nzchar(fread(file.path(proj, "output", "catalogue", "datasets.csv"), colClasses = "character")$rel)),
      "no datasets, products or GNSS points gives an empty table, not one blank row")
lk <- link_files(data.table(flight_id = "UAV-a", start_utc = as.POSIXct("2025-09-26 16:02:05", tz = "UTC"), end_utc = as.POSIXct("2025-09-26 16:21:30", tz = "UTC"),
                            utc_offset_h = -6, flight_no = NA_integer_, aircraft_serial = NA_character_),
                 data.table(flight_id = "UAV-a", source_id = "S", folder = ""),
                 data.table(source_id = "S", rel = "DJI_20250926100009_0001_D.RPT", file = "DJI_20250926100009_0001_D.RPT", folder = "", format = "dji_rpt", role = "flight_record", status = ""))
check(nrow(lk) == 1L && lk$state == "auto", "a flight folder scanned on its own: its RPT, at the source's top level, is linked to the flight")
check(grepl("^Run finished", utils::tail(readLines(sort(list.files(file.path(proj, "output", "logs"), "[.]log$", full.names = TRUE), decreasing = TRUE)[1]), 1)), "every run leaves a log in output/logs that says it finished")
check(!length(core_shadows(app_code_files(PIPELINE_DIR))), "no app function replaces a core function of the same name")
check(identical(STEP_HASH, step_hashes(globalenv())) && all(nchar(unlist(STEP_HASH)) == 12), "each cached step has its own code fingerprint, the same when computed again")
local({                                                               # placed, corrected and the five checks (pages/fct_checks.R), without the app
  source(file.path(here, "pages", "fct_checks.R"), local = TRUE)
  f <- data.table(flight_id = c("log", "rtk", "none", "sa_bad"), n_images = c(902, 612, 211, 100), n_georef = c(30, 612, 0, 100), geo_images = c(724, NA, NA, NA),
                  positioning_status = c("standalone GNSS / uncorrected", "RTK corrected", "unknown", "standalone GNSS / uncorrected"),
                  rtk_fixed_pct = c(NA, 98, NA, NA), acc_capture_m = c(2.5, NA, NA, 12), clock_ok = c(TRUE, TRUE, FALSE, TRUE), time_source = c("camera", "camera", "", "camera"),
                  has_log = c(TRUE, FALSE, FALSE, FALSE), has_mrk = c(FALSE, TRUE, FALSE, FALSE))
  k <- flight_checks(f)
  check(identical(k$n_placed, c(724, 612, 0, 100)) && identical(k$placed_from, c("flight log", "photo geotags", "none", "photo geotags")),
        "RS: images placed from a log when it places more than the image tags, and where they came from")
  check(identical(k$n_corrected, c(0, 612, 0, 0)), "RS: a log places images without making them corrected; only RTK or PPK counts as corrected")
  check(identical(k$chk_positions, c("look", "ok", "bad", "ok")) && identical(k$chk_accuracy, c("look", "ok", "bad", "bad")) &&
        identical(k$chk_clock, c("ok", "ok", "bad", "ok")) && identical(k$chk_log, c("ok", "ok", "look", "look")) && all(k$chk_images == "ok"),
        "RS: the five checks at the default limits (positions 95 / 50 %, standalone worse than 5 m is bad)")
  cal <- data.table(value = "check_placed_ok_pct", app = "RS", choice = "custom", applies_to = "*", number = "75", source = "test", notes = "")
  k2 <- tryCatch(flight_checks(f, cal), error = function(e) NULL)
  check(!is.null(k2) && k2$chk_positions[1] == "ok", "RS: the check limits come from the project's calibration table")
  sm <- checks_summary(f, k)
  check(sm$images == 1825 && abs(sm$placed_pct - 100 * 1436 / 1825) < 1e-9 && abs(sm$corrected_pct - 100 * 612 / 1825) < 1e-9 && sm$to_look == 3 && sm$n_log == 724,
        "RS: the summary keeps placed and RTK/PPK corrected apart")
  check(all(grepl("\u2713", checks_html(k[2]))) && grepl("\u2715", checks_html(k[3])), "RS: each check shows a symbol as well as a colour")
  check(sum(checks_open(k)) == sm$to_look && identical(checks_open(k[0]), logical()), "RS: the Map's \"checks to look at\" filter takes the flights the summary counts")
  g <- data.table(flight_id = c("bb3", "kml", "cam"), n_images = c(400, 50, 50), n_captures = c(100, 50, 50), n_georef = c(0, 0, 50), log_photos = c(100, NA, 0),
                  clock_ok = FALSE, time_source = c("flight log", "flown track (KML)", ""), positioning_status = "unknown")
  kg <- flight_checks(g); pc <- placed_counts(g)
  check(identical(kg$n_placed, c(400, 0, 50)) && identical(kg$placed_from, c("flight log", "none", "photo geotags")) && identical(pc$placed_from, kg$placed_from),
        "RS: the photo tags of a decoded .bb3 log place every image of their captures; one placed_from for the catalogue and the checks")
  check(identical(kg$chk_clock, c("ok", "look", "bad")) && grepl("flight log", kg$chk_why[1]), "RS: a time from a decoded flight log is a checked clock; a flown track's is worth a look")
  kr <- flight_checks(r3$flights)
  check(nrow(kr) == nrow(r3$flights) && all(kr$n_placed <= suppressWarnings(as.numeric(r3$flights$n_images))), "RS: checks for every flight in the test catalogue")
})
local({                                                               # the Needs you queue (pages/fct_needs.R), without the app
  source(file.path(here, "pages", "fct_checks.R"), local = TRUE); source(file.path(here, "pages", "fct_needs.R"), local = TRUE)
  f <- data.table(flight_id = c("ok", "sa", "nosite"), name = c("A", "B", "C"), date = c("2024-07-01", "2024-07-02", "2024-07-03"), site = c("Bog", "Bog", ""),
                  n_images = c(100, 300, 50), n_georef = c(100, 300, 50), geo_images = NA, positioning_status = c("RTK corrected", "standalone GNSS / uncorrected", "RTK corrected"),
                  acc_capture_m = c(NA, 12, NA), clock_ok = TRUE, time_source = "camera", has_log = TRUE, qa_flags = c("", "", "sun down to 5° (minimum 10°)"))
  x <- rs_needs(f, flight_checks(f)); x2 <- rs_needs(f, flight_checks(f))
  check(!any(x$flight_id == "ok") && x$severity[1] == "bad" && x$flight_id[1] == "sa" && any(x$kind == "site" & x$flight_id == "nosite") && any(x$kind == "qa" & grepl("sun down", x$what)) &&
        identical(x$id, x2$id) && !anyDuplicated(x$id) && all(nzchar(x$why) & nzchar(x$todo)),
        "RS Needs you: each open problem once, worst first, with why it matters and what to do, under an id that stays the same")
  check(nrow(rs_needs(f[0], flight_checks(f[0]))) == 0, "RS Needs you: no flights, nothing waits")
  mc <- rs_missing_counts(f)
  check(mc[what == "no site", n] == 1 && mc[what == "no project", n] == 0 && mc[what == "no georeferenced images", n] == 0 && nrow(rs_missing_counts(f[0])) == 6,
        "RS Needs you: missing metadata counted per kind, a column the catalogue lacks counting none")
  k <- flight_checks(f); acc <- x[flight_id == "sa" & kind == "accuracy"]
  kd <- checks_decided(f, k, data.table(id = acc$id, decision = "accepted as is", note = "archive only", author = "MD", at = ""))
  check(kd[flight_id == "sa"]$chk_accuracy == "ok" && k[flight_id == "sa"]$chk_accuracy == "bad" && grepl("Accepted as is: accuracy", kd[flight_id == "sa"]$chk_why) &&
        checks_summary(f, kd)$to_look == checks_summary(f, k)$to_look - 1L && identical(checks_decided(f, k, NULL), k),
        "RS Needs you: a check accepted as is counts as ok in the summary and the Map's Checks column")
  h <- copy(f)[, `:=`(clock_ok = c(FALSE, TRUE, TRUE), qa_flags = c("flight log not decoded: no .bb3 key in this project yet (scan its eMotion mission with the .bb3 logs)", "", ""))]
  y <- rs_needs(h, flight_checks(h))
  check(any(y$kind == "log_key" & y$go == "Sources" & grepl(".mis3", y$todo, fixed = TRUE)) && !any(y$kind == "qa" & grepl("not decoded", y$what)) &&
        y[kind == "clock"]$go == "Setup" && y[kind == "clock"]$tab == "timezones" && all(y[kind == "accuracy"]$go == "wb-positioning"),
        "RS Needs you: a log waiting for the .bb3 key says what to scan; Fix opens the Time zones table for the clock and Positioning for accuracy")
})
local({                                                               # the timeline (pages/fct_timeline.R), without the app
  source(file.path(here, "pages", "fct_timeline.R"), local = TRUE)
  f <- data.table(flight_id = paste0("f", 1:7), site = c("Bog", "Bog", "Bog", "Fen", "", NA, "Fen"), year = c("2018", "2018", "2026", "2020", "2021", "", "2020"),
                  date = c("2018-07-01", "2018-07-02", "2026-06-30", "2020-08-01", "2021-05-01", "2022-05-01", "2020-08-02"),
                  sensor = c("P1", "P1", "L2", "M3M", "", "P1", "M3M"), n_images = c(100, 120, 900, 50, 10, 20, 60), name = paste("flight", 1:7))
  d <- timeline_flights(f); s <- timeline_summary(d)
  check(nrow(d) == 7 && d$year[d$flight_id == "f6"] == 2022 && sum(d$site == NO_SITE) == 2 && identical(timeline_years(d), 2018:2026),
        "RS timeline: a row per site, flights with no site in \"no site\", every year from the first flight to the last")
  check(s$sites == 2 && s$flights == 7 && s$revisited == 1 && s$gap == 8 && s$gap_site == "Bog" && s$gap_from == 2018 && s$gap_to == 2026 && s$no_site == 2,
        "RS timeline: sites, revisited sites, the longest gap (Bog, 2018 to 2026) and flights needing a site")
  h <- timeline_html(d, "row", "pick"); st <- timeline_strip_html(d, "yr", "2020")
  check(lengths(regmatches(h, gregexpr("class='tl-sq' data-id", h))) == 7 && grepl("tl-empty", h) && lengths(regmatches(st, gregexpr("class='tl-yr", st))) == 9 && grepl("tl-yr on' data-y='2020'", st),
        "RS timeline: a square per flight, empty years kept, the picked year marked in the strip")
  cf <- data.table(flight_id = "f4", display_name = "", project = "", site = "", positioning_status = "")
  cf2 <- flights_set_site(cf, c("f4", "f5"), " Fen ", cols = names(cf))
  check(identical(cf2[order(flight_id)]$site, c("Fen", "Fen")) && cf$site == "" && inherits(tryCatch(flights_set_site(cf, "f4", " "), error = identity), "error"),
        "RS timeline: Set site writes only the chosen flights (adding rows), and refuses a blank site")
})
local({                                                               # a checkpoint file through the core import (pages/fct_checkpoint.R)
  for (f in c("metasheet.R", "import.R")) source(file.path(CORE_DIR, f), local = TRUE)
  source(file.path(here, "pages", "fct_checkpoint.R"), local = TRUE)
  x <- data.table(Point = paste0("CP", 1:4), Easting = c("0", "10", "20", "30"), Northing = "0", Elevation = "100",
                  `x product` = c("0,03", "9,97", "20,03", "29,97"), `y product` = c("0.04", "-0.04", "0.04", "-0.04"), `z product` = c("100.05", "99.95", "100.05", "99.95"))
  r <- checkpoint_read(x)
  ca <- if (r$ok) checkpoint_accuracy(r$rows, 0, 0)
  check(r$ok && identical(unname(r$map), c("id", "x_survey", "y_survey", "z_survey", "x_product", "y_product", "z_product")) && any(grepl("decimal comma", r$issues$message)) &&
        ca$summary$rmse_h_cm == 5 && ca$summary$rmse_v_cm == 5, "RS checkpoints: columns matched by heading, a decimal comma read and said, the same accuracy as before")
  r2 <- checkpoint_read(x[, .(Point, Easting, Northing, `x product`, `y product`)])
  r3b <- checkpoint_read(data.table(Point = "a", Foo = "1"))
  x4 <- copy(x)[2, Elevation := "n/a"]; r4 <- checkpoint_read(x4)
  check(r2$ok && any(r2$issues$severity == "warning" & grepl("Only horizontal", r2$issues$message)) && !r3b$ok && r3b$issues$severity[1] == "error" &&
        r4$ok && any(r4$issues$severity == "warning" & grepl("not a number", r4$issues$message)),
        "RS checkpoints: missing columns warn or stop before anything is worked out, and a value that is not a number is reported")
})
local({                                                               # .bb3 records that fit two known clock offsets (2017 eBee logs)
  set.seed(7); rg <- bb3_ring(as.raw(sample(0:255, 64L * 16L, TRUE))); NB <- rg$NB; ty <- 0x1234L
  rec <- function(j, d, h) c(as.raw(0x83), writeBin(16L * ((j - 1L - d) %% NB), raw(), 4L, endian = "little"), as.raw(c(0, 0)),
                             as.raw(c(bitwXor(h %/% 256L, rg$k0[j]), bitwXor(h %% 256L, rg$k1[j]))), as.raw(rep(0, 10)))
  hk <- function(c0, c1, j) bitwXor(c0, rg$k0[j]) * 256L + bitwXor(c1, rg$k1[j])
  one_fit <- which(rg$cnt[rg$k0 * 256L + rg$k1 + 1L] == 1L)          # blocks whose key is theirs alone
  gd <- one_fit[1:24]; lt <- one_fit[25:27]
  # each left-over record reads as the type at offset 0 and as another type (tyB) at offset 5, so no single fit settles it
  tyB <- vapply(lt, function(j) hk(bitwXor(ty %/% 256L, rg$k0[j]), bitwXor(ty %% 256L, rg$k1[j]), (j + 4L) %% NB + 1L), 1L)
  rs <- c(lapply(seq_along(gd), function(i) rec(gd[i], if (i <= 12L) 0L else 5L, ty)), lapply(lt, function(j) rec(j, 0L, ty)))
  u <- do.call(c, rs); n <- vapply(rs, length, 1L); a <- cumsum(c(1L, n))[seq_along(n)]
  loc <- tryCatch(bb3_locate(list(u = u, a = a, n = n, flags = rep(0x83L, length(n))), rg, data.table(L = 8L, type = c(ty, tyB))), error = function(e) e)
  check(!inherits(loc, "error") && sum(!is.na(loc$rec$type)) == length(gd) && sum(is.na(loc$rec$type)) == length(lt),
        ".bb3: a record that fits two known clock offsets is left unplaced and the rest are read (2017 logs stopped on 'object j not found')")
})
local({                                                               # the .bb3 key built during a scan (R/emotion_catalogue.R), with a stand-in builder
  ring <- file.path(tempfile("ring"), "config", "emotion_bb3.ring"); got <- NULL
  build <- function(m, b, out) { got <<- b; dir.create(dirname(out), recursive = TRUE); writeBin(as.raw(1:4), out); list(closed = TRUE, blocks = 1L) }
  fs <- data.table(file = c("plan.mis3", "EP-01_0359.bb3", "EP-01_0359_em.bb3"), format = c("emotion_plan", "sensefly_log", "sensefly_log"), status = "", size = 10,
                   path = c("/x/plan.mis3", "/x/EP-01_0359.bb3", "/x/EP-01_0359_em.bb3"))
  n0 <- emotion_key_auto(fs[1:2][, format := c("kml", "sensefly_log")], ring, build)
  ok <- emotion_key_auto(fs, ring, build); again <- { got <- NULL; emotion_key_auto(fs, ring, build) }
  check(!n0 && ok && file.exists(ring) && again && is.null(got) && identical(bb3_ring_path(dirname(dirname(ring))), ring),
        "a scan builds the .bb3 key from a source's .mis3 and .bb3 logs when the project has none, into its config folder, and only once")
})
local({                                                               # the set-up check of gnss_bases.csv and drone_cameras.csv (R/preflight.R)
  pd <- tempfile("rs_setup"); cf <- modifyList(CFG, list(project_dir = pd, raw_dirs = character())); invisible(load_configs(cf))
  write_cfg(data.table(file = "*", base = "B1", method = "by eye", post_processed = "maybe", service = "", notes = ""), pd, "gnss_bases")
  write_cfg(data.table(make = "Acme", model = "X1", drone = "perhaps", note = ""), pd, "drone_cameras")
  sc <- setup_check(cf)
  check(sum(sc$area == "GNSS bases") == 2 && any(sc$area == "Drone cameras" & grepl("perhaps", sc$problem)), "the set-up check reads the GNSS bases' method and post_processed, and drone yes or no")
})
local({                                                               # an eBee flight's time: the decoded log wins over KML and an unset camera clock
  t <- function(x) as.POSIXct(x, tz = "UTC")
  fl <- data.table(flight_id = c("a", "b", "c", "d"), n_images = 10, clock_ok = c(TRUE, TRUE, FALSE, TRUE), lat_min = 1, lat_max = 1, lon_min = 1, lon_max = 1,
                   start_utc = t(c("2013-12-31 17:00:00", "2026-09-07 04:30:00", "2026-09-07 10:26:00", NA)), end_utc = t(c("2013-12-31 17:20:00", "2026-09-07 04:50:00", "2026-09-07 10:41:00", NA)),
                   time_source = c("camera", "camera", "flown track (KML)", ""))
  lg <- data.table(source_id = "S", rel = paste0(c("a", "b", "c", "d"), ".bb3"), log_file = paste0(c("a", "b", "c", "d"), ".bb3"), log_kind = "bb3", log_decoded = TRUE, log_note = "",
                   log_start_utc = t(c("2015-08-29 18:00:00", "2026-09-07 11:24:00", "2026-09-07 11:24:00", "2026-09-07 12:00:00")), log_end_utc = t(c("2015-08-29 18:20:00", "2026-09-07 11:44:00", "2026-09-07 11:52:00", "2026-09-07 12:20:00")),
                   log_photos = 10L, log_track_points = 5L, log_size = 1, log_id = c("a", "b", "c", "d"))
  ok <- data.table(flight_id = c("a", "b", "c", "d"), source_id = "S", rel = lg$rel, format = "sensefly_log")
  r <- emotion_flight_fields(fl, ok, list(logs = lg))
  check(r[flight_id == "a"]$time_source == "flight log" && !r[flight_id == "a"]$clock_ok && grepl("camera clock 606 days off the flight log", r[flight_id == "a"]$emo_qa) &&
        r[flight_id == "a"]$start_utc == lg$log_start_utc[1], "a camera clock never set (factory date) gives way to the flight log's time")
  check(r[flight_id == "b"]$time_source == "camera" && r[flight_id == "b"]$clock_ok, "a camera clock in local time (7 h off) is kept")
  check(r[flight_id == "c"]$time_source == "flight log" && r[flight_id == "c"]$start_utc == lg$log_start_utc[3], "the flight log's time wins over the KML's")
  check(r[flight_id == "d"]$time_source == "flight log", "a flight with no time takes the log's")
})
local({                                                               # editing from the Flights list (pages/fct_edits.R), without the app
  source(file.path(here, "pages", "fct_checks.R"), local = TRUE); source(file.path(here, "pages", "fct_edits.R"), local = TRUE)
  fl <- data.table(flight_id = c("f1", "f2", "f3", "f4"), name = c("a", "b", "c", "d"), site = c("PBOG", "Disher PBog", "msp", ""), site_folder = c("PBOG", "Disher PBog", "msp", ""),
                   site_found = c("PBOG", "Disher PBog", "msp", ""), positioning_found = "unknown", positioning_status = "unknown", project = c("P1", "", "P2", ""),
                   n_images = c(100, 7, 50, 3), size_gb = 1, start = c("2020-08-24 10:12", "2020-08-24 10:31", "2020-09-01 09:00", "2020-09-02 09:00"),
                   lat_min = c(61, 61, 61.30, 50), lat_max = c(61.1, 61.1, 61.35, 50.1), lon_min = c(-121, -121, -121.2, -100), lon_max = c(-120.9, -120.9, -121.1, -99.9),
                   mission_set = c("Summer 24 Aug", "Summer 24 Aug", "", ""), display_name = "")
  st <- edit_stage(edit_empty(), c("f1", "f2"), "site", "Pbog"); st <- edit_stage(st, "f3", "project", "P9"); st <- edit_stage(st, "f1", "site", "GooseLake")
  check(edit_count(st) == 3 && edit_final(st)[flight_id == "f1" & col == "site"]$value == "GooseLake" && edit_count(edit_undo(st)) == 3 &&
        edit_final(edit_undo(st))[flight_id == "f1"]$value == "Pbog", "staged edits: the latest step wins a cell, Undo takes back the last step")
  cf <- edit_apply(data.table(flight_id = "f1", site = "old"), st)
  check(nrow(cf) == 3 && cf[flight_id == "f1"]$site == "GooseLake" && cf[flight_id == "f3"]$project == "P9" && all(FLIGHT_COLS %in% names(cf)), "staged edits saved to flights.csv in one write, rows added as needed")
  v <- edit_view(fl, edit_stage(st, "f2", "site", ""))
  check(v[flight_id == "f1"]$site == "GooseLake" && v[flight_id == "f2"]$site == "Disher PBog" && identical(edit_choices(fl, "site", data.table(site = "Zeta")), c("Zeta", "Disher PBog", "msp", "PBOG")),
        "the list shows staged values (blank = as found); the site dropdown offers known sites first")
  j <- fold_joined(fl)
  check(nrow(j) == 3 && j[flight_id == "f1"]$n_images == 107 && j[flight_id == "f1"]$parts == 2 && j[flight_id == "f1"]$name == "Summer 24 Aug", "a joined flight is one row: images summed, named by the join")
  sg <- site_suggest(fl, data.table(site = c("Pbog", "Lake"), latitude = c(NA, 61.32), longitude = c(NA, -121.15)), data.table(found = "msp", site = ""))
  check(sg[found == "PBOG"]$suggested == "Pbog" && sg[found == "Disher PBog"]$suggested == "Pbog" && sg[found == "msp"]$mapped == "(not a site)" && sg[found == "msp"]$suggested == "Lake" && sg[found == ""]$flights == 1,
        "site names found: the same name in another case or within the name is suggested; a name marked not a site says so")
  check(identical(site_apply_names(c("PBOG", "msp", "Lake"), data.table(found = c("pbog", "MSP"), site = c("Pbog", ""))), c("Pbog", "", "Lake")), "site names found are mapped on the next scan (blank = not a site)")
  check(nrow(sites_add(data.table(site = "Pbog"), c("pbog", "GooseLake"))) == 2 && nrow(site_names_set(data.table(found = "PBOG", site = "x", note = ""), "pbog", "Pbog")) == 1, "known sites and site names are added once")
  lg <- data.table(source_id = "S", rel = c("a.bb3", "b.bb3", "c.bb3", "d.bb3"), serial = c("EP-1", "EP-1", "EP-2", "EP-1"), photos = c(110, 0, 111, 230),
                   start_utc = c("2026-09-04 17:00:00", "2026-09-04 17:12:00", "2026-09-04 17:00:00", "2026-09-07 11:24:00"), flight_id = c(NA, NA, NA, "f9"))
  lc <- log_candidates(data.table(aircraft_serial = "EP-1", start_utc = NA, n_images = 111), lg)
  check(identical(lc$rel, c("a.bb3", "b.bb3")), "logs offered to an undated flight: no flight yet, same aircraft, closest photo count first")
  ck <- flight_checks(data.table(flight_id = "p", n_images = 4, kind = "photo set", clock_ok = FALSE, positioning_status = "unknown"))
  check(ck$chk_positions == "ok" && ck$chk_log == "ok" && grepl("Photo set", ck$chk_why), "a photo set is checked for its images only")
  gd <- geodata_folders(data.table(source_id = "S", folder = c("CT", "CT", "Sat"), kind = "raster", lat_min = c(NA, NA, 61), lat_max = c(NA, NA, 61.1), lon_min = c(NA, NA, -121), lon_max = c(NA, NA, -120.9)),
                        data.table(source_id = "S", folder = "far", lat_min = 10, lat_max = 11, lon_min = 10, lon_max = 11), fl)
  check(gd[folder == "CT"]$files == 2 && gd[folder == "CT"]$suggest == "remove" && gd[folder == "Sat"]$suggest == "keep" && gd[folder == "far"]$where == "far from the flights" &&
        grepl("CT/", geodata_rule("CT")$pattern), "other geodata by folder: no position or far from the flights suggested for removal, as an ignore rule")
})
local({ source(file.path(CORE_DIR, "testkit.R"), local = TRUE); source(file.path(here, "tests", "samples.R"), local = TRUE)   # the reader samples (core testkit.R)
  r <- check_reader_samples(file.path(here, "tests", "samples"), SAMPLE_READ); new <- r[is.na(matches_expected), file]
  if (length(new)) cat("  (expected results saved for", length(new), "new sample(s): check them in", file.path("tests", "samples", "expected"), ")\n")
  for (f in r[!(accepted), file]) cat("  ", f, ": not accepted. Check tests/samples/expected/", f, ".rds, then accept_sample(\"tests/samples\", \"", f, "\", \"<your name>\")\n", sep = "")
  for (f in r[reference_ok %in% FALSE, file]) cat("  ", f, ": a value in tests/samples/reference.csv differs from what the reader makes\n", sep = "")
  check(nrow(r) > 0 && all(r$rerun_identical) && !any(r$matches_expected %in% FALSE) && all(r$rows > 0) && all(r$accepted) && !any(r$reference_ok %in% FALSE),
        "every sample file reads, the same twice and the same as its expected result, accepted, and equal to its hand-checked reference values") })
for (t in c("test_bad_files.R", "test_golden.R", "test_formulas.R")) source(file.path(here, "tests", t), local = new.env())   # damaged files, the golden project, derived values
say("the Simple view")
local({
  Sys.setenv(RS_PROJECT_DIR = proj, RS_NO_RUN = "1"); on.exit(Sys.unsetenv(c("RS_PROJECT_DIR", "RS_NO_RUN")))
  app <- new.env(); assign("shinyApp", function(...) invisible(NULL), envir = app)
  local({ old <- setwd(here); on.exit(setwd(old)); suppressWarnings(suppressMessages(sys.source("app.R", envir = app, keep.source = FALSE))) })
  h <- as.character(htmltools::renderTags(app$simple_ui(NULL))$html); ids <- regmatches(h, gregexpr(' id="[^"]+"', h))[[1]]
  check(grepl('data-step="export"', h, fixed = TRUE) && grepl('id="sp_path"', h, fixed = TRUE) && grepl('id="s_rescan"', h, fixed = TRUE) && !anyDuplicated(ids),
        "the Simple view builds: four steps, the sources and Scan, and no input twice")
  check(setequal(app$SETUP_TABLES[wb_only == TRUE]$id, c("links", "file_rules", "timezones", "gnss_bases", "calibration")),
        "links, file rules, clocks, GNSS bases and calibration options are edited by hand only in the Workbench")
  k <- app$rename_spec(app$RENAMES, "site"); allowed <- unique(c(k$main[1], vapply(c(k$cols, k$lists), `[`, "", 1)))
  check(all(c("sites.csv", "flights.csv", "site_names.csv") %in% allowed), "renaming a site reaches sites.csv, flights.csv and site_names.csv (rename.R)")
  check(identical(app$sp_rs_generic(c("100MEDIA", "DCIM", "Camera", "Photos", "Backup", "Scotty", "GooseLake")), c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)),
        "a camera's, a card's or a backup's folder is not offered as a site (the list is in R/config.R)")
  ps <- file.path(tmp, "simple_p"); dir.create(file.path(ps, "config"), recursive = TRUE); invisible(file.copy(list.files(file.path(proj, "config"), full.names = TRUE), file.path(ps, "config"), recursive = TRUE))
  cf <- file.path(ps, "config"); md5 <- function() { f <- list.files(cf, full.names = TRUE, recursive = FALSE); f <- f[!dir.exists(f)]; stats::setNames(tools::md5sum(f), basename(f)) }
  fl0 <- app$sp_rs_cfg(ps, "flights", app$FLIGHT_COLS); st0 <- app$sp_rs_cfg(ps, "sites", app$SITE_COLS); nm0 <- app$sp_rs_cfg(ps, "site_names", app$SITE_NAME_COLS)
  id <- if (nrow(fl0)) fl0$flight_id[1] else "TEST-1"; pos0 <- if (nrow(fl0)) fl0$positioning_status[1] else ""; key <- app$sp_rs_key("x", id)
  ops <- rbind(app$SIMPLE_OPS, data.table(type = c("field", "move", "place", "field", "field", "field", "field", "field", "field"),
    group = c("Scotty2", "", "Pit", "Pit", key, key, key, key, key), to = c("Scotty", "Pit", "", "boreal", "Line 7", "P1", "a note", "yes", "RTK corrected"),
    item = c(app$SP_RS_FOLDER_ITEM, id, "", "region", "display_name", "project", "notes", "exclude_from_archive", "positioning_status"), at = "",
    lat = c(NA, NA, 61.3, NA, NA, NA, NA, NA, NA), lon = c(NA, NA, -121.3, NA, NA, NA, NA, NA, NA)), fill = TRUE)
  b0 <- md5(); r <- app$sp_rs_apply(ps, ops, "tt"); b1 <- md5(); ch <- names(b1)[is.na(b0[names(b1)]) | b0[names(b1)] != b1]
  check(isTRUE(r$ok) && length(ch) && all(ch %in% c("sites.csv", "flights.csv", "site_names.csv")), "Simple's Edit writes only sites.csv, flights.csv and site_names.csv, never a link, a rule or a clock")
  fl1 <- app$sp_rs_cfg(ps, "flights", app$FLIGHT_COLS); st1 <- app$sp_rs_cfg(ps, "sites", app$SITE_COLS); nm1 <- app$sp_rs_cfg(ps, "site_names", app$SITE_NAME_COLS)
  check(any(nm1$found == "Scotty2" & nm1$site == "Scotty") && identical(app$site_apply_names(c("Scotty2", "Other"), nm1), c("Scotty", "Other")) && any(st1$site == "Scotty"),
        "a folder name given a site lands in site_names.csv, and the next build names its flights with it")
  r1 <- fl1[flight_id == id]
  check(r1$site == "Pit" && any(st1$site == "Pit" & st1$latitude == "61.3" & st1$region == "boreal"), "a flight moved onto a new site, the site placed: flights.csv and sites.csv follow")
  check(r1$display_name == "Line 7" && r1$project == "P1" && r1$notes == "a note" && r1$exclude_from_archive == "yes", "a flight's name, project, notes and archive status land in flights.csv")
  check(identical(r1$positioning_status, pos0), "positioning is not written from the Simple view (it is set in the Workbench)")
  check(any(grepl("Simple view", readLines(file.path(cf, "history", "change_log.csv"), warn = FALSE), fixed = TRUE)), "each save keeps the version before, with who changed it")
  r$undo()
  check(identical(app$sp_rs_cfg(ps, "flights", app$FLIGHT_COLS), fl0) && identical(app$sp_rs_cfg(ps, "sites", app$SITE_COLS), st0) && nrow(app$sp_rs_cfg(ps, "site_names", app$SITE_NAME_COLS)) == nrow(nm0),
        "Undo puts flights.csv, sites.csv and site_names.csv back as they were")
})
if (!identical(Sys.getenv("OVID_SKIP_UI"), "1") && requireNamespace("chromote", quietly = TRUE) && !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) local({
  source(file.path(CORE_DIR, "uitest.R"), local = TRUE)
  ui <- tryCatch(ui_click_through(normalizePath(here), c(RS_PROJECT_DIR = proj, RS_NO_RUN = "1")), error = function(e) data.table(page = "(start)", shown = FALSE, errors = conditionMessage(e)))
  bad <- ui[!shown | nzchar(errors)]; if (nrow(bad)) print(bad[, .(page, shown, errors = substr(errors, 1, 300))])
  check(nrow(ui) > 3 && !nrow(bad), "every page opens in a browser with no output, JavaScript or server error (the test project)") })
if (fails) { say(fails, " test(s) failed", col = "red"); quit(status = 1) } else say("all tests passed", col = "green")
