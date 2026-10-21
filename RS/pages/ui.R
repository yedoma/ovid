# RS/pages/ui.R  --  The RS user interface: the map's scripts, the Standard pages and ui.
# Sourced by app.R at the top level (local = TRUE); the Workbench (workbench/wb_rs.R) reuses these pages.

if (dir.exists(file.path(APP_DIR, "docs"))) addResourcePath("docs", file.path(APP_DIR, "docs"))
# The map's scripts and the viewer's download, for both interfaces.
std_head <- function() tagList(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"), tags$script(src = "map_tools.js"), tags$script(src = "rs_map.js"),
  tags$script(src = "rs_download.js"))
# The Standard pages: the tools and section labels for ovid_page(), in rail order. The Workbench shows the same pages in its
# own frame (workbench/wb_rs.R).
std_pages <- function() list(
  "Archive",
  ovid_tool("Map", icon = "map-location-dot",
    mod_flight_summary_ui("summary"),                                  # RS/pages/mod_flight_summary.R: the summary cards
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(title = "Filter", width = 250,
        textInput("f_text", NULL, placeholder = "Search name, folder, notes"),
        selectInput("f_year", "Year", NULL, multiple = TRUE), selectInput("f_site", "Site", NULL, multiple = TRUE),
        selectInput("f_aircraft", "Aircraft", NULL, multiple = TRUE), selectInput("f_sensor", "Sensor", NULL, multiple = TRUE),
        selectInput("f_status", "Positioning", NULL, multiple = TRUE), selectInput("f_source", "Source", NULL, multiple = TRUE),
        ovid_switch("f_checks", "Only flights with checks to look at", FALSE),
        div(class = "legend-line", HTML("On the map: <b>Style</b> (top left) sets colours, sizes and colour-by; layers top right; legend bottom right."))),
      ovid_controls(ovid_split_toggle("map_split", "Map and flight details", on = TRUE)),
      ovid_split("map_split", width = "34%", on = TRUE,
        card(card_body(padding = 0, ovid_focus(uiOutput("map_note"), div(id = "rsmap")))),
        card(card_header("Flight"), card_body(uiOutput("detail")))),
      mod_timeline_strip_ui("strip"),                                  # RS/pages/mod_timeline.R: flights by year under the map
      rs_list_ui())),                                                  # Flights, Photo sets and Other geodata, with the edit and Save bars (below)
  ovid_tool("Timeline", icon = "timeline",
    mod_timeline_ui("timeline"),                                       # RS/pages/mod_timeline.R: sites by year, Set site
    card(card_header("Site names found in folder names"),               # pages/list_edits.R; saved with the Save bar
      p(class = "muted", "Names read from folder names that are not known sites (sites.csv), with a suggested known site: the same name in another case, a name within it, or the site whose flights overlap these. Saved to site_names.csv; the next rebuild applies them. A name kept as a site is added to sites.csv."),
      DTOutput("sn_table"),
      div(class = "d-flex flex-wrap gap-2 align-items-end mt-2",
        actionButton("sn_accept", "Use the suggestion", class = "btn-sm mb-3"),
        div(style = "width:14em", selectizeInput("sn_to", NULL, NULL, options = list(create = TRUE, placeholder = "Known site"))),
        actionButton("sn_map", "Map to this site", class = "btn-sm mb-3"), actionButton("sn_keep", "Keep as a site", class = "btn-sm mb-3"),
        actionButton("sn_not", "Not a site", class = "btn-sm btn-outline-danger mb-3"))),
    card(card_header("Coverage by site, year and sensor"), p(class = "muted", "Flights by site, year and sensor (RGB, multispectral, LiDAR) with the finest ground sample distance and the share positioned with RTK or PPK, and the satellite, aerial and LiDAR list by year: where a comparison between years is missing."),
         DTOutput("coverage"))),
  ovid_tool("Flight", icon = "plane",
    uiOutput("flight_title"), uiOutput("flight_quality"),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Your edits"), rs_flight_form("e_")),
      card(card_header("Mission: sorties flown as one survey"), uiOutput("mission"),
        layout_columns(col_widths = c(6, 6), actionButton("m_confirm", "Confirm this grouping", class = "btn-outline-primary w-100"),
          actionButton("m_split", "Split this sortie off", class = "btn-outline-danger w-100")),
        p(class = "muted", "Sorties are grouped when one aircraft flies again within an hour from the same plan or folder, or over the same area and site soon after (battery swaps, two flights on one battery).")),
      card(card_header("Linked files and the evidence for each"),
        DTOutput("links"),
        layout_columns(col_widths = c(6, 3, 3), textInput("l_reason", NULL, placeholder = "Reason (optional)"),
          actionButton("l_confirm", "Confirm link", class = "btn-outline-primary w-100"), actionButton("l_reject", "Reject link", class = "btn-outline-danger w-100")),
        p(class = "muted", "Confirmed and rejected links are kept in config/links.csv and win over what the scan finds."),
        h3(class = "mt-3", "Logs no flight has yet"),
        p(class = "muted", "Flight logs not linked to any flight, from the same aircraft and within a day of this flight (any day when it has no time), the closest photo-tag count first."),
        DTOutput("log_cands"), actionButton("lc_link", "Link this log", class = "btn-outline-primary btn-sm mt-2")))),
  ovid_tool("Imagery", icon = "satellite",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(title = "Satellite, aerial and LiDAR", width = 250,
        selectInput("d_kind", "Kind", NULL, multiple = TRUE), selectInput("d_platform", "Platform", NULL, multiple = TRUE),
        selectInput("d_year", "Year", NULL, multiple = TRUE), ovid_switch("d_sidecars", "Show every file of a scene", FALSE),
        actionButton("d_show", "Show these on the map", class = "btn-primary"),
        p(class = "muted mt-2", "Footprints come from vendor metadata (solid) or file headers (dashed, worked out by Ovid). A date from a folder or file name is marked inferred. Nothing is processed.")),
      card(card_header(textOutput("d_n", inline = TRUE)), DTOutput("d_table")),
      card(card_header("GIS layers (shapefile, GeoJSON, GPX)"), DTOutput("g_layers")))),
  "Tools",
  ovid_tool("GNSS", icon = "location-crosshairs",
    layout_columns(col_widths = c(4, 8),
      card(card_header("Survey files to stitch"),
        fileInput("g_files", "GNSS CSV or text files", multiple = TRUE, accept = c(".csv", ".txt", ".mrk", ".MRK", ".llh", ".LLH", ".pos", ".nmea", ".zip", ".gsi", ".GSI", ".xml")),
        textInput("g_zone", "UTM zone for grid-only points (e.g. 33N)", ""),
        p(class = "muted", "Emlid Reach (Emlid Flow / ReachView point CSVs, .LLH and NMEA position logs, Emlid Studio .pos, or the zip Emlid Flow downloads), DJI RTK (point CSVs and Timestamp.MRK), Leica (Viva / Captivate / LGO ASCII with or without a header, GSI-8/16, LandXML) and any CSV with latitude/longitude or easting/northing. Add more files and they join the same table."),
        actionButton("g_clear", "Start over"), hr(),
        ovid_export_button("g_dl", "csv", "stitched points", primary = TRUE),
        uiOutput("g_msgs")),
      card(card_header("Survey points on satellite imagery"), div(id = "gnssmap", style = "height:420px"),
        p(class = "muted", "Points with latitude/longitude only; grid-only rows (easting/northing) are listed but not placed, since no coordinates are converted."))),
    card(card_header("Position quality by file"), p(class = "muted", "Where the files give them: the share of fixed positions, the median horizontal and vertical RMS, PDOP and HDOP, the fewest satellites, the oldest corrections and the longest baseline, with limits from Set-up \u203a Calibration options. The base of each file and how it was positioned come from Set-up \u203a GNSS bases; hover the check for the reason."), DTOutput("g_quality")),
    card(card_header("Stitched points"), DTOutput("g_table"))),
  "Review",
  ovid_tool("QA", icon = "filter",
    card(card_header("Files not matched to a flight"), DTOutput("unassigned")),
    card(card_header("Datasets and their datum"), p(class = "muted", "Whether each satellite, aerial, LiDAR or raster file states its coordinate system and frame. None states its epoch or height type; datum unknown means it states nothing."),
         DTOutput("datum_tbl")),
    card(card_header("Products and STAC"), rs_products_ui("products_tbl"), div(class = "mt-2", rs_stac_ui("stac_dl")))),
  "Setup",
  ovid_tool("Sources", icon = "hard-drive",
    layout_columns(col_widths = c(7, 5, 12),
      card(card_header("Sources in the archive"), uiOutput("src_list"),
        p(class = "muted mt-2", "A source that is not plugged in keeps its last scan. The same flight found in two sources is one flight with copies. Remove takes a source out of the catalogue; its folder and files are never changed."),
        h3("Last scan"), uiOutput("src_scan"),
        tags$details(class = "mt-2", tags$summary("Why left out"), DTOutput("s_other")),
        tags$details(class = "mt-2", tags$summary("Duplicates"), DTOutput("s_dup"))),
      card(card_header("Add or update a source"),
        div(class = "d-flex gap-2 align-items-end",
          div(class = "flex-grow-1", textInput("s_path", "Folder or drive", placeholder = "/Volumes/Drive", width = "100%")),
          div(class = "mb-3", ovid_browse_button("s_path"))),
        textInput("s_id", "Short name (optional)", placeholder = "Drive"),
        actionButton("s_scan", "Scan folder", class = "btn-primary"), actionButton("s_rescan", "Rescan connected drives"),
        actionButton("s_build", "Rebuild catalogue"),
        p(class = "muted mt-2", "Mission plans, KMLs, DJI MRK/RTK files, eMotion geotags and eBee logs kept in a folder of their own: scan that folder as a source too (subfolders included). Its files are matched to DJI and eBee flights by flight number, image names, area and time."),
        p(class = "muted mt-2", "A first scan reads the small sidecar files (MRK, logs, metadata) and every photo's header once; later scans read only new or changed files. Only drone photos make flights (Set-up tables > drone_cameras.csv). The app stays usable while it scans; a stopped scan keeps the source's last good scan."),
        hr(), h3("Shareable viewer"), rs_viewer_ui("x_title", "x_make", "x_dl")),
      card(class = "mt-3", card_header("Set-up check and file audit"),
        div(class = "d-flex gap-2 align-items-center mb-2", uiOutput("chk_sum", inline = TRUE), span(class = "ms-auto"),
            actionButton("chk_run", "Check again", class = "btn-sm"), ovid_export_button("fa_dl", "csv", "file audit", class = "btn-sm")),
        DTOutput("chk_tbl"),
        tags$details(class = "mt-2", open = NA, tags$summary(textOutput("run_problems_n", inline = TRUE)), DTOutput("run_problems")),   # output/audit/problems.csv
        p(class = "muted mt-2", "What in the set-up would be ignored, misapplied or is missing (nothing is changed). The file audit lists every raw file and what became of it; it is written by each catalogue build.")))),
  ovid_tool("Set-up tables", value = "Setup", icon = "table-list",
    p(class = "muted mt-3", "Everything people decide about the archive is kept in tables in config/ in the project folder, and this page edits every one of them; the Flight and Sources pages write some of them too. A saved change applies on the next rebuild, and each save keeps the version before it in config/history."),
    do.call(navset_card_tab, c(list(id = "setup_tab"), lapply(seq_len(nrow(SETUP_TABLES)), function(i)
      nav_panel(SETUP_TABLES$file[i], value = SETUP_TABLES$id[i], p(class = "muted", SETUP_TABLES$help[i]), editUI(SETUP_TABLES$id[i])))))))
# The lists under the map: Flights (edited in place: select rows, set values, Save once), Photo sets and Other geodata.
# Every change waits in the Save bar until Save (or Cmd/Ctrl+S); the server is pages/list_edits.R.
rs_list_ui <- function() tagList(
  tags$style(HTML(sprintf(".rs-staged{outline:1px solid %s;outline-offset:1px;padding:0 2px}.rs-savebar{position:sticky;bottom:0;z-index:5;padding:8px 12px;margin-top:8px;border-top:2px solid %s;background:%s}.rs-editbar .form-group{margin-bottom:1rem}",
                          PAL$accent, PAL$accent, PAL$bg1))),
  tags$script(src = "rs_edits.js"),
  navset_card_tab(id = "list_tab",
    nav_panel(textOutput("n_shown", inline = TRUE), value = "flights",
      div(class = "rs-editbar d-flex flex-wrap gap-2 align-items-end",
        span(class = "muted", textOutput("ed_n", inline = TRUE)),
        div(style = "width:11em", selectizeInput("ed_site", NULL, NULL, options = list(create = TRUE, placeholder = "Site"))),
        div(style = "width:11em", selectizeInput("ed_project", NULL, NULL, options = list(create = TRUE, placeholder = "Project"))),
        div(style = "width:11em", textInput("ed_name", NULL, placeholder = "Name")),
        div(style = "width:13em", selectInput("ed_status", NULL, c("Positioning (no change)" = "", "(as found)" = "(as found)", POSITIONING))),
        actionButton("ed_set", "Set on selected", class = "btn-sm mb-3"),
        actionButton("ed_join", "Join into one flight", class = "btn-sm mb-3"),
        actionButton("ed_photoset", "Move to photo sets", class = "btn-sm mb-3"),
        actionButton("ed_out", "Leave out", class = "btn-sm btn-outline-danger mb-3")),
      DTOutput("flights")),
    nav_panel(textOutput("n_photosets", inline = TRUE), value = "photosets",
      p(class = "muted", textOutput("ps_rule", inline = TRUE)),
      DTOutput("photosets"), actionButton("ps_back", "Back to flights", class = "btn-sm mt-2")),
    nav_panel(textOutput("n_geodata", inline = TRUE), value = "geodata",
      p(class = "muted", "Satellite, aerial, LiDAR, raster and GIS files the scan found, by folder. Remove adds an ignore rule for the folder (file_rules.csv); the files stay where they are and stay in the file audit. Keep marks the folder as looked at."),
      DTOutput("geodata"),
      div(class = "d-flex gap-2 mt-2", actionButton("gd_keep", "Keep", class = "btn-sm"), actionButton("gd_remove", "Remove", class = "btn-sm btn-outline-danger"),
          actionButton("gd_suggested", "Use the suggestion for all shown", class = "btn-sm")))),
  uiOutput("save_bar"))
# Parts shown on two pages: on the Standard pages and on the Workbench's Products and STAC export tabs (workbench/wb_project.R),
# each with its own ids (an id may appear only once in a page); the server fills both (pages/qa.R, pages/set_up_check.R).
rs_products_ui <- function(tbl) tagList(p(class = "muted", "Products recorded in Set-up tables > products.csv, with their level and how they were made."), DTOutput(tbl))
rs_stac_ui <- function(dl) tagList(p(class = "muted", "The STAC catalogue (output/stac/) lists every flight, dataset and product for any STAC browser; each rebuild writes it, titled as the viewer."),
                                  downloadButton(dl, "Download the STAC catalogue (ZIP)"))
# The viewer: one button writes it in the background (make) and then downloads it through a hidden link (dl).
rs_viewer_ui <- function(title, make, dl) tagList(textInput(title, "Title (saved with the project)", CFG$viewer_title %||% "ovid RS archive"),
  actionButton(make, "Export viewer (ZIP)", icon = icon("download")), div(style = "display:none", downloadButton(dl, "Export viewer (ZIP)")),
  p(class = "muted mt-2", "The title is the viewer's and the STAC catalogue's. One folder with index.html and the tables; whoever gets it unzips it and opens index.html. No R or archive needed."))
# The flight edit form, one for the Flight page (prefix "e_") and the Workbench's Edit flight (prefix "wb_ed_"): its fields
# are flights.csv columns, v a flight's saved row (a list; blank or missing shows as found). The server fills and reads it
# with rs_flight_form_update() and rs_flight_form_read(); both save through save_flight() (pages/flight.R).
RS_FLIGHT_FORM <- c(name = "display_name", project = "project", site = "site", status = "positioning_status", status_note = "status_note",
                    notes = "notes", panel = "panel_id", panel_capture = "panel_capture", exclude = "exclude_from_archive")
rs_flight_value <- function(v, k) { x <- v[[k]]; if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1]) }
rs_flight_excluded <- function(v) tolower(rs_flight_value(v, "exclude_from_archive")) %in% c("true", "yes", "1", "x")
rs_flight_form <- function(prefix, v = list()) { id <- function(k) paste0(prefix, k); g <- function(k) rs_flight_value(v, RS_FLIGHT_FORM[[k]])
  tagList(textInput(id("name"), "Display name", g("name"), width = "100%"), textInput(id("project"), "Project", g("project"), width = "100%"),
    textInput(id("site"), "Site", g("site"), width = "100%"),
    selectInput(id("status"), "Positioning status", c("(as found)" = "", POSITIONING), g("status"), width = "100%"),
    textInput(id("status_note"), "Reason for changing the status (optional)", g("status_note"), width = "100%", placeholder = "where it came from; your initials"),
    textAreaInput(id("notes"), "Notes", g("notes"), rows = 3, width = "100%"),
    layout_columns(col_widths = c(6, 6), textInput(id("panel"), "Calibration panel", g("panel"), placeholder = "multispectral: panel ID"),
                   textInput(id("panel_capture"), "Panel imaged", g("panel_capture"), placeholder = "e.g. before and after")),
    checkboxInput(id("exclude"), "Leave out of the archive and the viewer", rs_flight_excluded(v)),
    actionButton(id("save"), "Save", class = "btn-primary"),
    p(class = "muted mt-2", "Saved to config/flights.csv (backed up); blank keeps what the scan found, and the files themselves are never changed. Shown at once; the next rebuild writes it into the catalogue."))
}
rs_flight_form_update <- function(session, prefix, v) { id <- function(k) paste0(prefix, k); g <- function(k) rs_flight_value(v, RS_FLIGHT_FORM[[k]])
  for (k in c("name", "project", "site", "status_note", "panel", "panel_capture")) updateTextInput(session, id(k), value = g(k))
  updateSelectInput(session, id("status"), selected = g("status")); updateTextAreaInput(session, id("notes"), value = g("notes"))
  updateCheckboxInput(session, id("exclude"), value = rs_flight_excluded(v))
}
# The form's values as flights.csv columns (input: the server's input, or any list holding the form's ids).
rs_flight_form_read <- function(input, prefix) {
  v <- lapply(names(RS_FLIGHT_FORM), function(k) input[[paste0(prefix, k)]] %||% ""); names(v) <- unname(RS_FLIGHT_FORM)
  v$exclude_from_archive <- if (isTRUE(input[[paste0(prefix, "exclude")]])) "TRUE" else ""
  v
}
# Settings: reached from the gear in the Standard top bar (rail = FALSE) and from the Pages tree in the Workbench (rail = TRUE).
rs_settings_tool <- function(rail = FALSE) ovid_tool("Settings", value = "Settings", icon = "sliders", rail = rail,
  card(class = "mt-3", style = "max-width:1100px", card_header("Time zone"), time_zone_ui()),
  card(class = "mt-3", style = "max-width:1100px", card_header("Colours"), colours_ui()),
  card(class = "mt-3", style = "max-width:1100px", card_header("Projects"), projects_list_ui("proj")))
ui <- do.call(ovid_page, c(list("RS", PAL, theme = THEME, project = projects_switch_ui("proj"), status = span(class = "muted", textOutput("made", inline = TRUE)),
  help = if (file.exists(file.path(APP_DIR, "docs", "RS_reference_guide.html"))) "docs/RS_reference_guide.html", head = std_head(), settings = "Settings"), std_pages(),
  list(convert_tool(), reference_tool("RS"), rs_settings_tool())))
