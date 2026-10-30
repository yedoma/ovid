# RS: the set-up tables and their editors. Sourced by app.R.
# ---- set-up tables -------------------------------------------------------------------------------------------------------
# Every table in config/ (CONFIG_FILES), each with the editor shared by all the apps (core/editor.R) on the Set-up tables
# page. Both interfaces build their editors from this list, and the Workbench writes its corrections through them. A saved
# change applies on the next rebuild (a rebuild reads no source: it uses each source's last scan).
SETUP_TABLES <- data.table(id = c("flights", "links", "sites", "site_names", "file_rules", "timezones", "sources", "products", "gnss_bases", "calibration", "drone_cameras", "footprint_colours"))
SETUP_TABLES[, `:=`(file = vapply(id, function(k) CONFIG_FILES[[k]]$file, ""), label = vapply(id, function(k) CONFIG_FILES[[k]]$label, ""),
  help = c("What people decide about each flight: its name, project, site, positioning status (with the reason for changing it), notes, whether it is left out of the archive, and its mission. The Flight page writes these.",
           "Links between flights and files confirmed or rejected by hand (file is source_id:rel); they win over what the scan finds. The Flight page writes these.",
           "Sites: their project and where they are.",
           "Site names found in folder names and the known site each means; a blank site says the name is not a site (a band folder, a person). The Timeline's Site names card writes these; the next rebuild applies them.",
           "File rules: a pattern of paths (relative to the source) and the action: ignore, or a role. An ignore rule applies on the next rebuild; a role, or a rule taken out, applies when the source is scanned again. The Workbench writes ignore rules.",
           "Camera clocks without a zone: the offset from UTC by camera serial, model, folder or file, and period (core/timezones.R).",
           "The drives and folders the archive is built from. The Sources page adds them when it scans one.",
           paste("Products made from the flights (orthomosaic, DSM, DTM, point cloud, reflectance, index, classification, change map): the file, the flights that went into it (flight ids separated by ;),",
                 "the software, version and settings, the control and check points, the accuracy from the processing report (rmse_h_m, rmse_v_m), and the coordinate system: epsg, frame (NAD83(CSRS), WGS 84), epoch,",
                 "height_type (ellipsoidal or orthometric) and geoid_model. level L0 to L4 (blank: from the kind). They go into the STAC catalogue with their provenance."),
           paste("How each GNSS survey's base was positioned, by file name or pattern (* for all): the base, the method (single averaging, float averaging, fix averaging, static, known point, network (NTRIP / CORS)),",
                 "post_processed (yes or no) and the service (for example NRCan CSRS-PPP). A base set by single or float averaging and not post-processed is flagged on the GNSS page."),
           paste("Calibration options, shared with the other apps (config/calibration_options.csv): choice default (the number and source from the code), scotty_creek (only where a Scotty Creek value exists; see notes),",
                 "or custom (your number; a source is encouraged). applies_to is a base or file name, or * for all; flight limits apply to the whole project."),
           paste("Drone cameras: a photo counts as a drone photo when it carries a drone tag (DJI drone-dji, senseFly or Camera: XMP) or its EXIF make and model match a row here",
                 "(make in any case; model as the start of the EXIF model, blank for any model of that make). drone \"no\" leaves a camera out even with drone tags (DJI handhelds). Other photos (phones, compact cameras, trail cameras, screenshots) are left out",
                 "of the flights and listed in catalogue/other_photos.csv. A change applies on the next rebuild."),
           paste("Footprint colours on the map, in the year layers, the report, Simple's map and the viewer: one row per drone family (match: a pattern found in the aircraft or camera name; the first row that fits wins, blank fits any),",
                 "a palette colour name for optical, other and thermal sensors, and year_step_deg, how far apart each year's shade is. Shown at once.")))]
SETUP_TABLES[, wb_only := id %in% c("links", "file_rules", "timezones", "gnss_bases", "calibration")]   # what flights are made of and how they are read: edited by hand only in the Workbench (M 28 Oct)
setup_editors <- function(path_of, tick, mark) setNames(lapply(SETUP_TABLES$id, function(k)
  ovid_edit_server(k, reactive(path_of(k)), tick, on_change = mark, wb_only = SETUP_TABLES[id == k]$wb_only)), SETUP_TABLES$id)
# Set-up saved after the last catalogue build (the newest file in config/, and the build's manifest), as seconds.
