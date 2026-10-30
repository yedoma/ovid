# RS/simple/simple_rs.R  --  The Simple view of RS (M 28 Oct): a field check of the drone and remote-sensing archive, in four
# steps. Import: the sources (drives, folders), Scan and a checklist (files listed, image details read, flights grouped, logs
# linked, flights with a site, corrected positions). View: the footprints on a map beside a tile per group (folder guess, year
# or site), the group's flights with their five checks, and one flight's facts. Edit: folder names to sites (site_names.csv),
# sites (name, place, region), which site each flight is at, and a flight's name, project, notes and archive status, through
# the list of changes (core simple.R). Export: the catalogue report, the tables, the footprints and the offline viewer.
# The Standard server runs underneath (server(embedded = TRUE)); Edit writes only sites.csv, flights.csv and site_names.csv
# (a rename through rename.R). Positioning, links, file rules, clocks and GNSS bases are not edited here. Sourced by app.R.

SP_RS_TILES <- 8                                # groups shown before "Show all"
SP_RS_FOLDER_ITEM <- "site for this folder name"   # the list-of-changes entry that maps a folder name to a site (site_names.csv)
# Flights (photo sets left out) whose positions are RTK or PPK corrected: counted over the same rows the check divides by
# (it said "1 of 0 flights" when a photo set was corrected).
sp_rs_corrected <- function(f) sum(f[kind != "photo set"]$positioning_status %in% CORRECTED_STATES)
# "5 flights and 7 photo sets": the one wording of what a catalogue holds, on Import, View and each source (a photo set: a few
# photos, fewer than photoset_max_images, not a mapping flight). Photo sets are named only when there are some.
sp_rs_fl_n <- function(kind) { k <- sum(kind == "photo set"); n <- length(kind) - k
  f <- function(x, w) sp_rs_n(x, w, shown = num(x))
  if (!k) f(n, "flight") else if (!n) f(k, "photo set") else paste(f(n, "flight"), "and", f(k, "photo set")) }
# A time as the rest of RS shows it: in the project's base zone (Settings, Time zone), named. t: POSIXct, or text in UTC
# (a source's last scan, catalogue/sources.csv).
sp_rs_when <- function(t) { t <- if (inherits(t, "POSIXct")) t else as.POSIXct(as.character(t), tz = "UTC"); if (!length(t) || is.na(t)) return("")
  paste0(format(t + BASE_OFFSET * 3600, "%Y-%m-%d %H:%M", tz = "UTC"), " (", zone_text(BASE_OFFSET), ")") }
sp_rs_generic <- function(x) grepl(SIMPLE_GENERIC_FOLDERS, trim(as.character(x)), ignore.case = TRUE, perl = TRUE)   # a camera's or a backup's folder (R/config.R)

sp_rs_n <- function(n, word, words = paste0(word, "s"), shown = n) paste(shown, if (isTRUE(n == 1)) word else words)   # "1 flight", "2 flights"
sp_rs_chr <- function(d, cols) { d <- as.data.table(d); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x }); ensure_cols(d, cols) }
sp_rs_blank <- function(d, ...) { r <- as.data.table(as.list(stats::setNames(rep("", ncol(d)), names(d)))); x <- list(...)
  for (k in names(x)) set(r, j = k, value = as.character(x[[k]])); r }
sp_rs_cfg <- function(pd, what, cols) { p <- cfg_path(pd, what); sp_rs_chr(if (file.exists(p)) read_cfg(p) else data.table(), cols) }
sp_rs_num <- function(x) suppressWarnings(as.numeric(x))
sp_rs_key <- function(name, id) paste0(name, " · ", id)       # a flight in the list of changes; the id is read back by sp_rs_apply
sp_rs_id <- function(key) sub("^.* · ", "", key)
sp_rs_src_path <- function(s) as.character(if ("root" %in% names(s)) s$root else s$path)   # the catalogue's sources (root) or the set-up's (path)
sp_rs_plugged <- function(s) { p <- sp_rs_src_path(s); !is.na(p) & nzchar(p) & dir.exists(path.expand(p)) }

# The flights as Simple shows them: the site each is at (one people set, else the folder name's site when it is a known site),
# the folder guess and whether it is a camera's folder, % of images with corrected positions, GSD or LiDAR, and the checks.
sp_rs_flights <- function(fl, ck, cfg_fl, sites) {
  if (is.null(fl) || !nrow(fl)) return(NULL); f <- copy(fl)
  known <- sites$site[nzchar(sites$site)]; set_site <- cfg_fl$site[match(f$flight_id, cfg_fl$flight_id)]; set_site[is.na(set_site)] <- ""
  sf <- if ("site_folder" %in% names(f)) as.character(f$site_folder) else as.character(f$site); sf[is.na(sf)] <- ""
  f[, `:=`(folder_guess = sf, generic = sp_rs_generic(sf) | !nzchar(sf),
           place = fifelse(nzchar(set_site), set_site, fifelse(site %in% known, site, "")))]
  k <- ck[match(f$flight_id, ck$flight_id)]
  f[, `:=`(corrected_pct = fifelse(sp_rs_num(n_images) > 0, round(100 * k$n_corrected / sp_rs_num(n_images)), NA_real_),
           look = k$chk_images != "ok" | k$chk_clock != "ok" | k$chk_positions != "ok" | k$chk_accuracy != "ok" | k$chk_log != "ok",
           gsd = fifelse(fpk == "lidar", "LiDAR", fifelse(is.finite(sp_rs_num(fp_gsd_cm)), sprintf("%.1f cm", sp_rs_num(fp_gsd_cm)),
                         fifelse(is.finite(sp_rs_num(gsd_cm_median)), sprintf("%.1f cm", sp_rs_num(gsd_cm_median)), "—"))))]
  f[is.na(look), look := FALSE]
  f[]
}
sp_rs_groups <- function(f, by) { if (is.null(f) || !nrow(f)) return(NULL)
  f <- copy(f); f[, g := switch(by, year = year, site = fifelse(nzchar(place), place, "No site yet"), fifelse(nzchar(folder_guess), folder_guess, "(no folder name)"))]
  f[, .(flights = .N, sensors = paste(utils::head(names(sort(table(sensor[nzchar(sensor)]), decreasing = TRUE)), 3), collapse = ", "),
        first = { d <- substr(date[date != "undated"], 1, 10); if (length(d)) min(d) else "" }, last = { d <- substr(date[date != "undated"], 1, 10); if (length(d)) max(d) else "" },
        corrected = round(stats::weighted.mean(corrected_pct, pmax(1, sp_rs_num(n_images)), na.rm = TRUE)), look = sum(look), images = sum(sp_rs_num(n_images), na.rm = TRUE),
        generic = by == "folder" && all(generic)), by = .(group = g)][order(-flights, group)] }

# The footprints of some flights as GeoJSON (one Feature per flight part; the catalogue's rings are "lat lon;lat lon ...").
sp_rs_geojson <- function(fp, f) {
  if (is.null(fp) || !nrow(fp) || is.null(f) || !nrow(f)) return('{"type":"FeatureCollection","features":[]}')   # nothing scanned yet
  x <- fp[flight_id %in% f$flight_id]; if (!nrow(x)) return('{"type":"FeatureCollection","features":[]}')
  feat <- vapply(split(x, by = c("flight_id", "part")), function(d) { r <- d[1]; ff <- f[flight_id == r$flight_id][1]
    rings <- vapply(d$line, function(l) { p <- do.call(rbind, lapply(strsplit(strsplit(l, ";", fixed = TRUE)[[1]], " ", fixed = TRUE), as.numeric)); if (!is.matrix(p)) return("")
      if (!all(p[1, ] == p[nrow(p), ])) p <- rbind(p, p[1, ]); paste0("[", paste(sprintf("[%.7f,%.7f]", p[, 2], p[, 1]), collapse = ","), "]") }, "")
    sprintf('{"type":"Feature","properties":{"flight_id":%s,"name":%s,"date":%s,"kind":%s,"site":%s},"geometry":{"type":"Polygon","coordinates":[%s]}}',
            jsonlite::toJSON(r$flight_id, auto_unbox = TRUE), jsonlite::toJSON(ff$name, auto_unbox = TRUE), jsonlite::toJSON(ff$date, auto_unbox = TRUE),
            jsonlite::toJSON(r$kind, auto_unbox = TRUE), jsonlite::toJSON(ff$place %||% "", auto_unbox = TRUE), paste(rings[nzchar(rings)], collapse = ",")) }, "")
  paste0('{"type":"FeatureCollection","features":[', paste(feat, collapse = ","), "]}") }

# ---- interface ---------------------------------------------------------------------------------------------------------------
sp_rs_import_ui <- function() tagList(
  uiOutput("sp_choose"),                                                     # choose a catalogue (a new install; New project in the menu)
  conditionalPanel("!output.sp_choosing",
    uiOutput("sp_run"), uiOutput("sp_next"),                                 # the scan's progress; after it, the way on to View
    div(class = "sp-cols",
      div(class = "sp-card", uiOutput("sp_sources"),
        div(class = "sp-row sp-addrow", div(class = "sp-fill", textInput("sp_path", NULL, placeholder = "/Volumes/T7/2024-Drone Flights", width = "100%")),
            ovid_browse_button("sp_path"), actionButton("sp_add", "Add to list", class = "sp-btn2")),
        uiOutput("sp_scan_btns"),
        tags$details(class = "sp-small", style = "margin-top:10px", tags$summary("Other geodata"),
          checkboxInput("sp_gd_gis", "Also catalogue GIS layers", TRUE), checkboxInput("sp_gd_img", "Also catalogue satellite, aerial, LiDAR and raster files", TRUE),
          p(class = "sp-dim", "Geodata more than 5 km from every flight, and not beside its photos, is set aside rather than catalogued; Standard, Map, Other geodata lists it."))),
      div(class = "sp-card", div(class = "sp-h", "What was read"), uiOutput("sp_read"), uiOutput("sp_checks")))))

sp_rs_view_ui <- function() tagList(
  uiOutput("sp_hero"),
  div(class = "sp-row sp-filters", style = "margin-top:0",
    radioButtons("sp_by", NULL, c("By folder guess" = "folder", "By year" = "year", "By site" = "site", "Need a look" = "look"), inline = TRUE),
    div(class = "sp-fill"), textInput("sp_q", NULL, placeholder = "Find a flight, folder or sensor", width = "240px")),
  div(class = "sp-rs-cols",
    div(uiOutput("sp_tiles", class = "sp-tiles"), uiOutput("sp_more")),
    div(class = "sp-card", div(class = "sp-row", style = "margin-top:0", tags$b(textOutput("sp_map_title", inline = TRUE)), div(class = "sp-fill"),
                                selectInput("sp_year", NULL, "All years", width = "130px")),
        plotly::plotlyOutput("sp_map", height = "380px"),
        p(class = "sp-dim sp-small", "Photo footprints from the camera's sensor and focal length at the flying height (pinhole, nadir); LiDAR as its swath. Coloured by drone, sensor and year, each year its own shade (footprint colours, Standard: Set-up tables)."))),
  div(class = "sp-card", div(class = "sp-h", textOutput("sp_rows_title", inline = TRUE)), uiOutput("sp_rows")),
  div(class = "sp-card", div(class = "sp-h", textOutput("sp_flight_title", inline = TRUE)), uiOutput("sp_flight")))

sp_rs_edit_ui <- function() tagList(uiOutput("sp_folders"), uiOutput("sp_flight_card"), simple_edit_ui("spe", "site", "flights"))

sp_rs_export_ui <- function() div(class = "sp-cols",
  div(class = "sp-card", div(class = "sp-h", "Catalogue report"),
    p("One page: per site its flights, dates, sensors, images, how many have corrected positions, the ground sample distance, products and the five checks; the flights with no site yet; the methods with their sources."),
    textInput("sp_rpt_who", "Your initials (on the report)", width = "220px"),
    div(class = "sp-row", downloadButton("sp_rpt", "Download the report (HTML)", class = "sp-btn", icon = NULL))),
  div(class = "sp-card", div(class = "sp-h", "Tables, footprints and the viewer"),
    p("The catalogue tables and the set-up; every flight's footprint as GeoJSON (for a GIS); and the offline viewer, a folder to send to people without the archive."),
    div(class = "sp-row", downloadButton("sp_cat", "Catalogue tables (.zip)", class = "sp-btn2", icon = NULL), downloadButton("sp_geo", "Footprints (GeoJSON)", class = "sp-btn2", icon = NULL)),
    p(class = "sp-dim sp-small", "The offline viewer is made on ", tags$a(href = simple_std_href("Sources"), "Standard, Sources"), "; every flight, image and file on ", tags$a(href = simple_std_href("Map"), "Standard, Map"), ".")))

SP_RS_CSS <- paste(".sp-checks{margin-top:10px;border-top:1px solid var(--sp-line)}",
  ".sp-check{display:flex;flex-wrap:wrap;align-items:baseline;gap:4px 10px;padding:7px 0;border-bottom:1px solid var(--sp-line)}",
  ".sp-check>.sp-dot{flex:none;width:9px;height:9px;border-radius:50%;background:var(--sp-ok);align-self:center}",
  ".sp-check.sp-warn>.sp-dot{background:var(--sp-warn)} .sp-check.sp-error>.sp-dot{background:var(--sp-err)} .sp-check>.sp-fill{flex:1}",
  ".sp-table{width:100%;border-collapse:collapse;font-size:13.5px;font-variant-numeric:tabular-nums} .sp-table th{text-align:left;font-weight:600;color:var(--sp-fg2);padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line2)}",
  ".sp-table td{padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line);vertical-align:middle} .sp-table tr.on td{background:color-mix(in srgb,var(--sp-acc) 10%,transparent)}",
  ".sp-table .sp-n{text-align:right} .sp-table .form-group{margin:0} .sp-plug{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:6px;background:var(--sp-mut)} .sp-plug.on{background:var(--sp-ok)}",
  ".sp-filters .form-group{margin:0} .sp-filters .shiny-options-group{display:flex;flex-wrap:wrap;gap:6px}",
  ".sp-filters .radio-inline{margin:0;padding:4px 12px;border:1px solid var(--sp-line2);border-radius:999px} .sp-filters .radio-inline input{display:none}",
  ".sp-filters .radio-inline:has(input:checked){border-color:var(--sp-acc);box-shadow:inset 0 0 0 1px var(--sp-acc)}",
  ".sp-rs-cols{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:16px;align-items:start} .sp-rs-cols .sp-tiles{grid-template-columns:repeat(auto-fill,minmax(210px,1fr))}",
  "@media (max-width:1000px){.sp-rs-cols{grid-template-columns:1fr}} .sp-card .form-group{margin-bottom:6px}",
  ".sp-addrow{align-items:flex-start;margin-top:10px} .sp-addrow .form-group{margin:0} .sp-big{font-size:17px;font-weight:600;margin-bottom:4px}",
  ".sp-name{width:100%;max-width:220px;background:transparent;color:inherit;border:1px solid var(--sp-line2);border-radius:4px;padding:2px 6px;font:inherit}",
  ".sp-next{display:flex;flex-wrap:wrap;gap:12px;align-items:center;justify-content:space-between;margin:0 0 16px;padding:12px 14px;border:1px solid var(--sp-acc);border-radius:6px}",
  ".sp-run{margin-bottom:16px} .sp-run .d-run-bar{margin:12px 0 8px} .sp-step.sp-off{opacity:.45;pointer-events:none}",
  ".sp-plist>div{display:flex;justify-content:space-between;gap:10px;align-items:center;padding:8px 0;border-top:1px solid var(--sp-line)} .sp-plist>div:first-child{border-top:0}",
  "html.sp-root:has(.sp-body .tab-pane.active[data-value=\"import\"]) #ovid_runner{display:none}")   # the progress is shown on Import itself

simple_ui <- function(req) {
  pages <- Filter(function(x) inherits(x, "ovid_tool"), std_pages())                              # the Standard pages, hidden: their
  hidden <- lapply(pages, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))   # inputs exist for its server
  simple_page("RS", PAL, import = sp_rs_import_ui(), view = sp_rs_view_ui(), edit = sp_rs_edit_ui(), export = sp_rs_export_ui(),
              project = projects_switch_ui("proj"), hidden = hidden, theme = THEME, head = tagList(std_head(), tags$style(HTML(SP_RS_CSS)),
                tags$script(HTML("$(function(){ Shiny.addCustomMessageHandler('sp-rs-lock', function(m){ document.querySelectorAll('.sp-step').forEach(function(b){   // View, Edit and Export wait for a catalogue
                  if (b.dataset.step !== 'import') { b.classList.toggle('sp-off', m.on); b.setAttribute('aria-disabled', m.on ? 'true' : 'false'); } });
                  if (m.on) { var a = document.querySelector('.sp-step[data-step=\\'import\\']'); if (a && !a.classList.contains('on')) a.click(); } }); });"))))
}

# ---- server ------------------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns (E, C, rv, fchk, reload, ...).
simple_server <- function(input, output, session, std) {
  pd <- function() CFG$project_dir; C <- std$C
  steps <- simple_steps_server(session, function() c(import = !is.null(C())))
  if (!is.null(isolate(C()))) steps$go("view")
  go_step <- function(k) sprintf("document.querySelector('.sp-step[data-step=\"%s\"]').click()", k)
  to_std <- function(page, label) tags$a(href = simple_std_href(page), label)
  sites_tab <- reactive({ std$rv$tick; live_stamp(cfg_path(pd(), "sites")); sp_rs_cfg(pd(), "sites", SITE_COLS) })
  cfg_fl <- reactive({ std$rv$tick; live_stamp(cfg_path(pd(), "flights")); sp_rs_cfg(pd(), "flights", FLIGHT_COLS) })
  names_tab <- reactive({ std$rv$tick; live_stamp(cfg_path(pd(), "site_names")); sp_rs_cfg(pd(), "site_names", SITE_NAME_COLS) })
  F <- reactive({ x <- C(); if (is.null(x)) return(NULL); sp_rs_flights(x$flights, std$fchk(), cfg_fl(), sites_tab()) })

  # ---- Import
  # Choosing a catalogue: on a new install (no project chosen yet), and from New project in the menu. Nothing opens by default.
  choosing <- reactiveVal(is.null(SETTINGS$project_dir) && !nzchar(Sys.getenv("RS_PROJECT_DIR")))
  output$sp_choosing <- reactive(choosing()); outputOptions(output, "sp_choosing", suspendWhenHidden = FALSE)
  std$proj$on_new(function() choosing(TRUE))
  observe(session$sendCustomMessage("sp-rs-lock", list(on = choosing() || is.null(C()))))
  new_auto <- reactiveVal("")
  output$sp_choose <- renderUI({ if (!choosing()) return(NULL)
    known <- isolate(tryCatch(project_info(projects_known(SETTINGS)), error = function(e) data.table()))
    known <- if (nrow(known)) known[there == TRUE] else known
    js_open <- function(p) sprintf("Shiny.setInputValue('sp_open', %s, {priority: 'event'})", jsonlite::toJSON(p, auto_unbox = TRUE))
    tagList(p(class = "sp-lead", "A catalogue lists the drone flights, imagery and survey files found on your drives and folders. It reads them and never changes them."),
      div(class = "sp-cols",
        div(class = "sp-card", div(class = "sp-h", "Start a new catalogue"),
          textInput("sp_new_name", "Name", placeholder = "e.g. 2026 field season", width = "100%"),
          div(class = "sp-row sp-addrow", div(class = "sp-fill", textInput("sp_new_dir", "Saved in", width = "100%")), ovid_browse_button("sp_new_dir")),
          div(class = "sp-row", actionButton("sp_new_go", "Start the catalogue", class = "sp-btn"),
              if (isolate(!is.null(C())) || nrow(known)) actionLink("sp_choose_back", "Back to the open catalogue"))),
        div(class = "sp-card", div(class = "sp-h", "Or open one you have"),
          if (nrow(known)) div(class = "sp-plist", lapply(seq_len(nrow(known)), function(i) div(span(tags$b(known$name[i]), div(class = "sp-dim sp-small", tilde(known$path[i]),
              if (nzchar(known$last_run[i])) paste(" \u00b7 run", known$last_run[i]) else " \u00b7 never run")),
              tags$button(type = "button", class = "btn sp-btn2", onclick = js_open(known$path[i]), "Open"))))
          else p(class = "sp-dim", "No catalogue yet on this computer."),
          div(class = "sp-row sp-addrow", div(class = "sp-fill", textInput("sp_open_dir", "Open a folder from somewhere else", width = "100%")), ovid_browse_button("sp_open_dir")),
          div(class = "sp-row", actionButton("sp_open_go", "Open", class = "sp-btn2"))))) })
  observeEvent(input$sp_new_name, { d <- trim(input$sp_new_dir %||% ""); if (nzchar(d) && !identical(d, new_auto())) return()   # the folder follows the name until it is changed
    n <- trim(input$sp_new_name); v <- if (nzchar(n) && !grepl("[/\\\\:]", n)) tilde(file.path(projects_root(), n)) else ""
    new_auto(v); updateTextInput(session, "sp_new_dir", value = v) }, ignoreInit = TRUE)
  observeEvent(input$sp_new_go, { n <- trim(input$sp_new_name %||% ""); d <- trim(input$sp_new_dir %||% "")
    if (!nzchar(n)) return(showNotification("Give the catalogue a name.", type = "warning"))
    if (nzchar(why <- project_name_problem(n, projects_known(SETTINGS)))) return(showNotification(why, type = "warning"))
    if (run_busy()) return(showNotification("A scan is going: wait for it or cancel it first.", type = "warning"))
    r <- tryCatch(std$proj$new(if (nzchar(d)) d else file.path(projects_root(), n), n), error = function(e) e)
    if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error", duration = 10))
    choosing(FALSE); steps$go("import") })
  open_project <- function(p) { p <- path.expand(trim(p %||% ""))
    if (!nzchar(p) || !dir.exists(p)) return(showNotification("That folder was not found.", type = "error"))
    ow <- project_owner(p); if (nzchar(ow) && !identical(ow, project_app())) return(showNotification(paste0("That folder is a project of ovid ", ow, "."), type = "error"))
    if (!nzchar(ow) && !dir.exists(file.path(p, "config")) && length(list.files(p, no.. = TRUE))) return(showNotification("That folder is not an ovid RS catalogue.", type = "error"))
    if (run_busy()) return(showNotification("A scan is going: wait for it or cancel it first.", type = "warning"))
    std$proj$open(p); choosing(FALSE); steps$go(if (is.null(std$C())) "import" else "view") }
  observeEvent(input$sp_open, open_project(input$sp_open)); observeEvent(input$sp_open_go, open_project(input$sp_open_dir))
  observeEvent(input$sp_choose_back, choosing(FALSE))

  # the sources: scanned ones and the folders waiting in the list (short name editable, Remove, a note when one is inside another)
  output$sp_sources <- renderUI({ s <- std$src_cfg(); x <- C(); sc <- if (is.null(x)) data.table() else x$sources
    if (!nrow(s)) return(tagList(div(class = "sp-big", "Add the folder or drive that holds your flights"),
      p(class = "sp-dim", "For example a drive's top folder, or one year's folder. Add as many as you like, then scan them together. Read only: nothing on them is changed.")))
    f <- F(); n <- if (!is.null(f)) f[, .(N = sp_rs_fl_n(kind)), by = source_id] else data.table(source_id = character(), N = character())
    np <- normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE); done <- std$src_scanned(s$source_id)
    rm_js <- function(sid) sprintf("Shiny.setInputValue('sp_rm', %s, {priority: 'event'})", jsonlite::toJSON(sid, auto_unbox = TRUE))
    tagList(div(class = "sp-h", "Sources"),
      tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("Source"), tags$th("Status"), tags$th(""))),
        tags$tbody(lapply(seq_len(nrow(s)), function(i) { sid <- s$source_id[i]; on <- dir.exists(path.expand(s$path[i]))
          inside <- s$source_id[-i][if (length(np[-i])) startsWith(np[i], paste0(np[-i], "/")) else logical()]
          when <- if ("last_scanned" %in% names(sc)) as.character(sc$last_scanned[match(sid, sc$source_id)]) else NA_character_
          tags$tr(tags$td(span(class = paste("sp-plug", if (on) "on"), title = if (on) "plugged in" else "not plugged in: its last scan is used"),
                          if (done[i]) tags$b(sid) else tags$input(type = "text", class = "sp-name", value = sid, `aria-label` = "Short name",
                            onchange = sprintf("Shiny.setInputValue('sp_rename', {sid: %s, name: this.value}, {priority: 'event'})", jsonlite::toJSON(sid, auto_unbox = TRUE))),
                          div(class = "sp-dim sp-small", s$path[i], if (!on) " \u00b7 not plugged in", if (length(inside)) paste0(" \u00b7 inside ", inside[1], ": its files count here only"))),
                  tags$td(if (done[i]) paste0(if (length(when) && !is.na(when)) paste0("scanned ", sp_rs_when(when), " \u00b7 ") else "", fcoalesce(n$N[match(sid, n$source_id)], "no flights")) else span(class = "sp-dim", "waiting to be scanned")),
                  tags$td(tags$button(type = "button", class = "btn sp-btn2", onclick = rm_js(sid), "Remove"))) })))) })
  output$sp_scan_btns <- renderUI({ q <- nrow(std$queued()); s <- std$src_cfg(); scanned <- nrow(s) - q
    div(class = "sp-row", if (q) actionButton("sp_scan_new", paste("Scan", sp_rs_n(q, "folder", "folders")), class = "sp-btn"),
      if (scanned) tags$button(type = "button", class = if (q) "btn sp-btn2" else "btn sp-btn", onclick = "document.getElementById('s_rescan').click()", "Rescan plugged-in drives"),
      if (q || !nrow(s)) span(class = "sp-dim sp-small", "Read only. A first scan of a big drive can take a while; you can cancel it at any point.")) })
  # the scan's progress where you are looking (the same run as the box in the corner, its numbers updated in place: core runner.js)
  run_key <- reactiveVal("")
  observe({ invalidateLater(1000); j <- RUNNER$job; run_key(if (is.null(j)) "" else paste(format(j$t0), j$label, !is.null(j$cancelled_at))) })
  output$sp_run <- renderUI({ if (!nzchar(run_key())) return(NULL); j <- isolate(RUNNER$job); if (is.null(j)) return(NULL); st <- run_state(j); stopping <- !is.null(j$cancelled_at)
    div(class = "sp-card sp-run", `data-ovid-run` = "",
      div(class = "sp-row", style = "margin-top:0;justify-content:space-between", tags$b(j$label),
          actionButton("sp_run_cancel", if (stopping) "Stopping" else "Cancel", class = "sp-btn2", disabled = if (stopping) NA)),
      div(class = "d-run-bar", div(class = "d-run-fill", style = sprintf("width:%d%%", st$pct))),
      tags$dl(class = "sp-kv", tags$dt("Now"), tags$dd(span(class = "d-run-stage", st$stage), div(class = "d-run-item sp-dim sp-small", style = if (!nzchar(st$item)) "display:none", st$item)),
              tags$dt("Time"), tags$dd(span(class = "d-run-time", st$time))),
      p(class = "sp-dim sp-small", style = "margin:8px 0 0", if (stopping) "Stopping at the next safe point." else
        "You can leave this page; the scan keeps going. Cancel keeps what was read so far, and the next scan picks up from there.")) })
  observeEvent(input$sp_run_cancel, run_cancel())
  output$sp_next <- renderUI({ x <- C(); if (nzchar(run_key()) || is.null(x)) return(NULL); f <- F()
    div(class = "sp-next", span(tags$b(paste(sp_rs_fl_n(f$kind), "in the catalogue.")), " ",
                                span(class = "sp-dim", "Next: see them on the map, grouped by folder, year or site.")),
        tags$button(type = "button", class = "btn sp-btn", onclick = go_step("view"), "View the flights \u2192")) })
  output$sp_read <- renderUI({ x <- C(); if (is.null(x)) return(p(class = "sp-dim", "Nothing scanned yet.")); f <- F(); s <- x$sources
    off <- if (nrow(s)) sum(!sp_rs_plugged(s)) else 0L
    tags$dl(class = "sp-kv",
      tags$dt("Flights"), tags$dd(sprintf("%s (%s, %s)", num(nrow(f)), sp_rs_n(sum(f$kind != "photo set"), "flight"), sp_rs_n(sum(f$kind == "photo set"), "photo set"))),
      tags$dt("Images"), tags$dd(num(sum(sp_rs_num(f$n_images), na.rm = TRUE))),
      tags$dt("Sources"), tags$dd(if (!off) sprintf("%d, all plugged in", nrow(s)) else sprintf("%d; %d not plugged in (%s from the last scan)", nrow(s), off, if (off == 1) "its flights come" else "their flights come")),
      tags$dt("Catalogue"), tags$dd(sp_rs_when(x$made))) })
  output$sp_checks <- renderUI({ x <- C(); f <- F(); if (is.null(f)) return(NULL); fa <- file.path(CAT(), "files.csv")
    nfiles <- if (file.exists(fa)) max(0L, length(readLines(fa, warn = FALSE)) - 1L) else NA_integer_
    cp <- sum(sp_rs_num(f$n_copies) > 1, na.rm = TRUE); lk <- sum(f$has_log %in% c(TRUE, "TRUE") | f$has_geotags %in% c(TRUE, "TRUE") | f$has_mrk %in% c(TRUE, "TRUE") | f$has_mission %in% c(TRUE, "TRUE"), na.rm = TRUE)
    fl <- f[kind != "photo set"]; nos <- sum(!nzchar(f$place)); cor <- sp_rs_corrected(f)
    row <- function(lv, what, detail, fix = NULL) div(class = paste("sp-check", paste0("sp-", lv)), span(class = "sp-dot"), tags$b(what), span(class = "sp-dim", detail), div(class = "sp-fill"), fix)
    div(class = "sp-checks",
      row("ok", "Files listed", if (is.na(nfiles)) "—" else sprintf("%s files; %s left unassigned", num(nfiles), num(nrow(x$unassigned)))),
      row("ok", "Image details read", sprintf("%s images (EXIF and XMP)", num(sum(sp_rs_num(f$n_images), na.rm = TRUE)))),
      row("ok", "Flights grouped", sprintf("%s; %d found in more than one place, each counted once", sp_rs_n(nrow(f), "flight"), cp)),
      row(if (lk >= nrow(fl)) "ok" else "warn", "Logs and geotags linked", sprintf("%d / %d flights", min(lk, nrow(f)), nrow(f))),
      row(if (!nos) "ok" else "warn", "Flights with a site", if (!nos) sprintf("%d of %d", nrow(f), nrow(f)) else sprintf("%d of %d have none yet: name the folders in Edit", nos, nrow(f))),
      row(if (cor == nrow(fl)) "ok" else "warn", "Corrected positions (RTK or PPK)", sprintf("%d of %d flights; %d standalone, %d unknown", cor, nrow(fl),
          sum(fl$positioning_status == "standalone GNSS / uncorrected"), sum(fl$positioning_status %in% c("unknown", ""))), if (cor < nrow(fl)) to_std("QA", "QA"))) })

  # ---- View
  pick <- reactiveVal(NULL); fpick <- reactiveVal(NULL); all_tiles <- reactiveVal(FALSE)
  observeEvent(input$sp_pick, { pick(input$sp_pick); fpick(NULL) })
  observeEvent(input$sp_flight, fpick(input$sp_flight))
  observeEvent(input$sp_all, all_tiles(!all_tiles()))
  by <- reactive({ b <- input$sp_by %||% "folder"; if (b == "look") "folder" else b })
  shown <- reactive({ f <- F(); req(f); q <- tolower(trim(input$sp_q %||% ""))
    if (identical(input$sp_by, "look")) f <- f[look == TRUE]
    if (nzchar(q)) f <- f[grepl(q, tolower(paste(name, folder_guess, sensor, place, flight_id)), fixed = TRUE)]
    f })
  groups <- reactive(sp_rs_groups(shown(), by()))
  key_of <- function(f, b) switch(b, year = f$year, site = fifelse(nzchar(f$place), f$place, "No site yet"), fifelse(nzchar(f$folder_guess), f$folder_guess, "(no folder name)"))
  cur <- reactive({ g <- groups(); k <- pick(); if (is.null(g) || !nrow(g)) return(NULL); if (is.null(k) || !k %in% g$group) g$group[1] else k })
  rows_g <- reactive({ k <- cur(); f <- shown(); if (is.null(k)) return(NULL); f[key_of(f, by()) == k][order(date)] })
  cur_f <- reactive({ r <- rows_g(); k <- fpick(); if (is.null(r) || !nrow(r)) return(NULL); if (is.null(k) || !k %in% r$flight_id) r$flight_id[1] else k })
  output$sp_hero <- renderUI({ f <- F()
    if (is.null(f)) return(simple_hero("off", "Nothing scanned yet", "Add a drive or folder and scan it.", tags$button(type = "button", class = "btn sp-btn", onclick = go_step("import"), "Import")))
    d <- substr(f$date[f$date != "undated"], 1, 10); nl <- sum(f$look)
    simple_hero(if (nl) "warn" else "ok", paste(sp_rs_fl_n(f$kind), "·", sp_rs_n(uniqueN(f$sensor[nzchar(f$sensor)]), "sensor")),
      paste0(if (length(d)) paste0(min(d), " to ", max(d), ". ") else "", if (nl) sprintf("%s a check to look at (clock, positions, accuracy or log).", if (nl == 1) "1 has" else paste(nl, "have")) else "Every check is fine.")) })
  output$sp_tiles <- renderUI({ g <- groups(); if (is.null(g) || !nrow(g)) return(p(class = "sp-dim", "No flight matches.")); k <- cur()
    if (!all_tiles()) g <- utils::head(g, SP_RS_TILES)
    lapply(seq_len(nrow(g)), function(i) { r <- g[i]
      simple_tile(r$group, if (r$look) "warn" else "ok", state = if (r$generic) "camera folder" else if (r$look) sprintf("%d to look at", r$look) else NULL,
        facts = stats::setNames(list(as.character(r$flights), if (nzchar(r$sensors)) r$sensors else "—", if (nzchar(r$first)) (if (r$first == r$last) r$first else paste(r$first, "to", r$last)) else "undated",
                                     if (is.na(r$corrected)) "—" else sprintf("%d %%", r$corrected)), c("Flights", "Sensors", "Dates", "Corrected")),
        on = identical(r$group, k), link = if (r$look) list(label = "Open in Standard: QA", page = "QA")) }) })
  output$sp_more <- renderUI({ g <- groups(); if (is.null(g) || nrow(g) <= SP_RS_TILES) return(NULL)
    div(class = "sp-row", style = "margin-top:0", actionButton("sp_all", if (all_tiles()) "Show fewer" else sprintf("Show all %d", nrow(g)), class = "sp-btn2")) })
  observe({ r <- rows_g(); y <- if (is.null(r)) character() else sort(unique(r$year), decreasing = TRUE); updateSelectInput(session, "sp_year", choices = c("All years", y), selected = "All years") })
  output$sp_map_title <- renderText({ k <- cur(); if (is.null(k)) "Footprints" else paste("Footprints ·", k) })
  output$sp_map <- plotly::renderPlotly({ r <- rows_g(); req(r, nrow(r) > 0); x <- C(); fp <- x$footprints; y <- input$sp_year %||% "All years"
    if (y != "All years") r <- r[year == y]; z <- fp[flight_id %in% r$flight_id]; validate(need(nrow(z) > 0, "No footprint for these flights (no position or no camera geometry)."))
    sel <- cur_f(); g <- plotly::plot_ly(); lat <- c(); lon <- c()
    fc <- fp_colours(r, tryCatch(load_configs(CFG)$footprint_colours, error = function(e) fp_colours_default())); fcol <- setNames(fc$col, r$flight_id)   # by drone, sensor and year
    fc_of <- function(id) { c0 <- unname(fcol[id]); if (length(c0) != 1 || is.na(c0)) PAL$gray else c0 }
    for (i in seq_len(nrow(z))) { p <- do.call(rbind, lapply(strsplit(strsplit(z$line[i], ";", fixed = TRUE)[[1]], " ", fixed = TRUE), as.numeric)); if (!is.matrix(p)) next
      p <- rbind(p, p[1, ]); lat <- c(lat, p[, 1]); lon <- c(lon, p[, 2]); on <- identical(z$flight_id[i], sel)
      g <- plotly::add_trace(g, type = "scattermapbox", mode = "lines", lat = p[, 1], lon = p[, 2], fill = "toself", hoverinfo = "text", showlegend = FALSE,
        text = r$name[match(z$flight_id[i], r$flight_id)], line = list(width = if (on) 3 else 1.2, color = if (on) PAL$fg else fc_of(z$flight_id[i])),
        fillcolor = col_rgba(fc_of(z$flight_id[i]), 0.12)) }
    plotly::layout(g, mapbox = list(style = "open-street-map", center = list(lat = mean(range(lat)), lon = mean(range(lon))), zoom = max(3, min(15, 13 - log2(max(diff(range(lat)), diff(range(lon)) / 2, 1e-3) * 50)))),
                   margin = list(l = 0, r = 0, t = 0, b = 0), paper_bgcolor = PAL$bg) |> plotly::config(displaylogo = FALSE) })
  output$sp_rows_title <- renderText({ k <- cur(); if (is.null(k)) "" else paste("Flights ·", k) })
  output$sp_rows <- renderUI({ r <- rows_g(); if (is.null(r) || !nrow(r)) return(NULL); k <- cur_f(); ck <- std$fchk(); ch <- checks_html(ck[match(r$flight_id, ck$flight_id)])
    tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("Date"), tags$th("Flight"), tags$th("Sensor"), tags$th(class = "sp-n", "Images"), tags$th("Positioning"), tags$th(class = "sp-n", "GSD"), tags$th("Checks"))),
      tags$tbody(lapply(seq_len(min(nrow(r), 200)), function(i) { f <- r[i]
        tags$tr(class = if (identical(f$flight_id, k)) "on", style = "cursor:pointer", onclick = sprintf("Shiny.setInputValue('sp_flight', %s, {priority: 'event'})", jsonlite::toJSON(f$flight_id, auto_unbox = TRUE)),
          tags$td(f$date), tags$td(tags$b(f$name), if (sp_rs_num(f$n_copies) > 1) span(class = "sp-dim", sprintf(" · %d copies", as.integer(f$n_copies)))), tags$td(f$sensor),
          tags$td(class = "sp-n", num(sp_rs_num(f$n_images))), tags$td(f$positioning_status), tags$td(class = "sp-n", f$gsd), tags$td(HTML(ch[i]))) }))) })
  output$sp_flight_title <- renderText({ k <- cur_f(); f <- F(); if (is.null(k)) "" else { r <- f[flight_id == k]; paste(r$date, "·", r$name) } })
  output$sp_flight <- renderUI({ k <- cur_f(); req(k); f <- F()[flight_id == k]; ck <- std$fchk()[flight_id == k]
    sun <- if (is.finite(sp_rs_num(f$sun_elev_min_deg))) { a <- round(sp_rs_num(f$sun_elev_min_deg)); b <- round(sp_rs_num(f$sun_elev_max_deg)); if (a == b) sprintf("%.0f°", a) else sprintf("%.0f° to %.0f°", a, b) } else "—"
    lv <- c(ok = "✓ ok", look = "! look", bad = "✕ bad")
    div(class = "sp-cols",
      tags$dl(class = "sp-kv", tags$dt("Sensor"), tags$dd(paste(f$aircraft, f$sensor)), tags$dt("Images"), tags$dd(num(sp_rs_num(f$n_images))),
        tags$dt("Positions"), tags$dd(paste0(f$positioning_status, if (nrow(ck)) sprintf(" · %s placed (%s)", num(ck$n_placed), ck$placed_from) else "")),
        tags$dt("GSD"), tags$dd(f$gsd), tags$dt("Sun elevation"), tags$dd(sun), tags$dt("Copies"), tags$dd(as.character(f$n_copies %||% 1)),
        tags$dt("Folder"), tags$dd(class = "sp-dim", f$folder)),
      div(if (nrow(ck)) tags$ul(lapply(seq_along(CHECKS), function(j) { l <- ck[[c("chk_images", "chk_clock", "chk_positions", "chk_accuracy", "chk_log")[j]]]
            tags$li(tags$b(paste0(CHECKS[j], ":")), " ", lv[[l]]) })),   # no space before the colon
          p(class = "sp-dim sp-small", if (nrow(ck)) ck$chk_why),
          p(class = "sp-dim sp-small", "GSD: ground sample distance from the flying height and the camera (McGlone 2013). Sun elevation from the time and place (NOAA solar position equations; Meeus 1991)."))) })

  # ---- Edit
  rows <- reactive({ f <- F(); if (is.null(f)) return(NULL); st <- sites_tab()
    r <- f[kind != "photo set" | nzchar(place), .(item = flight_id, label = paste0(substr(date, 1, 10), " · ", sensor), kind = "flight", group = place, first = "", last = "")]
    extra <- setdiff(st$site[nzchar(st$site)], r$group)
    if (length(extra)) r <- rbind(r, data.table(item = "", label = "", kind = "", group = extra, first = "", last = ""))
    r[, `:=`(lat = sp_rs_num(st$latitude[match(group, st$site)]), lon = sp_rs_num(st$longitude[match(group, st$site)]))]
    r[] })
  touched <- c("sites.csv", "flights.csv", "site_names.csv")
  busy <- reactive({ invalidateLater(3000); k <- ls(OVID_PENDING); if (!length(k)) return(character())
    k <- k[dirname(k) == live_path(file.path(pd(), "config"))]; intersect(unique(basename(k)), touched) })
  ed <- simple_edit_server("spe", rows, apply = function(ops, who) { r <- sp_rs_apply(pd(), ops, who); std$reload(); r },
    busy = busy, noun = "site", items = "flights",
    fields = list(list(id = "region", label = "Region", type = "text", get = function(k) { st <- sites_tab(); v <- st$region[st$site == k]; if (length(v)) v[1] else "" })))
  # folder names to sites: each a change in the list (site_names.csv); a camera's or a backup's folder names nothing
  fold <- reactive({ f <- F(); if (is.null(f)) return(NULL); x <- C(); nm <- names_tab(); st <- sites_tab()
    s <- tryCatch(site_suggest(x$flights, st, nm), error = function(e) data.table(found = character(), suggested = character(), mapped = character()))   # pages/fct_edits.R
    t <- f[nzchar(folder_guess) & !generic, .(flights = .N, example = folder[1]), by = .(found = folder_guess)][order(-flights)]
    t[, `:=`(suggested = s$suggested[match(found, s$found)] %||% "", mapped = nm$site[match(tolower(found), tolower(nm$found))])]
    t[is.na(suggested), suggested := ""][is.na(mapped), mapped := ""]; t[] })
  output$sp_folders <- renderUI({ t <- fold(); if (is.null(t) || !nrow(t)) return(NULL); f <- F(); ng <- f[generic == TRUE & nzchar(folder_guess), uniqueN(folder_guess)]
    n <- min(nrow(t), 25L)
    div(class = "sp-card", div(class = "sp-h", "Folder names → sites"),
      p(class = "sp-dim sp-small", sprintf("The scan guesses a flight's site from its folder name. Give each name the site it means (it then names every flight in that folder, at the next rebuild).%s", if (ng) sprintf(" %s (DCIM, 100MEDIA, Photos ...) %s nothing and %s left out.", sp_rs_n(ng, "camera or backup folder name"), if (ng == 1) "names" else "name", if (ng == 1) "is" else "are") else "")),
      tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("Folder name"), tags$th(class = "sp-n", "Flights"), tags$th("Example"), tags$th("Site"), tags$th(""))),
        tags$tbody(lapply(seq_len(n), function(i) { r <- t[i]
          tags$tr(tags$td(tags$b(r$found)), tags$td(class = "sp-n", r$flights), tags$td(class = "sp-dim sp-small", r$example),
            tags$td(textInput(paste0("sp_fn_", i), NULL, if (nzchar(r$mapped)) r$mapped else "", width = "180px", placeholder = "site")),
            tags$td(if (nzchar(r$suggested) && !identical(r$suggested, r$mapped)) tags$button(type = "button", class = "btn sp-btn2 btn-sm",
                      onclick = sprintf("var e=document.getElementById('sp_fn_%d'); e.value=%s; $(e).trigger('change');", i, jsonlite::toJSON(r$suggested, auto_unbox = TRUE)), sprintf("Use '%s'", r$suggested)))) }))),
      if (nrow(t) > n) p(class = "sp-dim sp-small", sprintf("And %d more folder names with fewer flights (Standard, Timeline, Site names).", nrow(t) - n)),
      div(class = "sp-row", actionButton("sp_fn_add", "Add to changes", class = "sp-btn"))) })
  observeEvent(input$sp_fn_add, { t <- fold(); req(t); n <- min(nrow(t), 25L)
    v <- vapply(seq_len(n), function(i) trim(input[[paste0("sp_fn_", i)]] %||% ""), ""); ch <- which(nzchar(v) & v != t$mapped[seq_len(n)])
    if (!length(ch)) return(showNotification("No folder name was given a site.", type = "message"))
    ed$propose(data.table(type = "field", group = t$found[ch], item = SP_RS_FOLDER_ITEM, to = v[ch])) })
  # one flight's details (positioning is shown only: it is set in the Workbench)
  output$sp_flight_card <- renderUI({ f <- F(); if (is.null(f)) return(NULL); ch <- stats::setNames(f$flight_id, paste0(f$date, " · ", f$name, " · ", f$sensor))
    div(class = "sp-card", div(class = "sp-h", "Flight details"),
      div(class = "sp-row", style = "margin-top:0;align-items:flex-end;flex-wrap:wrap",
        selectizeInput("sp_ed_f", "Flight", ch, selected = isolate(input$sp_ed_f) %||% cur_f(), width = "340px"),
        textInput("sp_ed_name", "Name", width = "180px"), textInput("sp_ed_proj", "Project", width = "150px"), textInput("sp_ed_notes", "Notes", width = "220px"),
        selectInput("sp_ed_ex", "Archive", c("Included" = "", "Left out" = "yes"), width = "130px"), actionButton("sp_ed_add", "Add to changes", class = "sp-btn")),
      p(class = "sp-dim sp-small", textOutput("sp_ed_pos", inline = TRUE))) })
  output$sp_ed_pos <- renderText({ f <- F(); r <- f[flight_id == (input$sp_ed_f %||% "")]; if (!nrow(r)) return("")
    paste0("Positioning: ", r$positioning_status, " (set in the Workbench). Which site a flight is at: drag it onto a site below.") })
  observeEvent(input$sp_ed_f, { f <- F(); r <- f[flight_id == input$sp_ed_f]; if (!nrow(r)) return(); c0 <- cfg_fl()[flight_id == input$sp_ed_f]
    updateTextInput(session, "sp_ed_name", value = r$display_name); updateTextInput(session, "sp_ed_proj", value = r$project); updateTextInput(session, "sp_ed_notes", value = r$notes)
    updateSelectInput(session, "sp_ed_ex", selected = if (nrow(c0) && tolower(c0$exclude_from_archive) %in% c("yes", "true", "1")) "yes" else "") })
  observeEvent(input$sp_ed_add, { f <- F(); r <- f[flight_id == input$sp_ed_f]; req(nrow(r) > 0); c0 <- cfg_fl()[flight_id == input$sp_ed_f]; k <- sp_rs_key(r$name, r$flight_id)
    now <- list(display_name = r$display_name, project = r$project, notes = r$notes, exclude_from_archive = if (nrow(c0) && tolower(c0$exclude_from_archive) %in% c("yes", "true", "1")) "yes" else "")
    new <- list(display_name = trim(input$sp_ed_name %||% ""), project = trim(input$sp_ed_proj %||% ""), notes = trim(input$sp_ed_notes %||% ""), exclude_from_archive = input$sp_ed_ex %||% "")
    ch <- names(new)[!mapply(identical, new, now)]; if (!length(ch)) return(showNotification("Nothing changed for this flight.", type = "message"))
    ed$propose(data.table(type = "field", group = k, item = ch, to = unlist(new[ch]))) })

  # ---- Export
  table_of <- function(f, pr) { if (is.null(f) || !nrow(f)) return(NULL); ck <- std$fchk()[match(f$flight_id, std$fchk()$flight_id)]
    lvl <- function(l) c(ok = sum(l == "ok"), look = sum(l == "look"), bad = sum(l == "bad"))
    f <- cbind(f, ck[, .(chk_images, chk_clock, chk_positions, chk_accuracy, chk_log)])
    t <- f[, { a <- c(chk_images, chk_clock, chk_positions, chk_accuracy, chk_log); g <- sp_rs_num(fp_gsd_cm); g <- g[is.finite(g)]
      .(Flights = .N, Dates = { d <- substr(date[date != "undated"], 1, 10); if (length(d)) paste(min(d), "to", max(d)) else "—" },
        Sensors = paste(utils::head(names(sort(table(sensor[nzchar(sensor)]), decreasing = TRUE)), 3), collapse = ", "), Images = sum(sp_rs_num(n_images), na.rm = TRUE),
        `% corrected` = round(stats::weighted.mean(corrected_pct, pmax(1, sp_rs_num(n_images)), na.rm = TRUE)),
        `GSD (cm)` = if (length(g)) sprintf("%.1f–%.1f", min(g), max(g)) else "—", Products = sum(vapply(flight_id, function(i) sum(grepl(i, pr$flights, fixed = TRUE)), 0L) > 0),
        `Checks ✓ ! ✕` = paste(lvl(a), collapse = " / ")) }, by = .(Site = fifelse(nzchar(place), place, "No site yet"))]
    t[order(Site == "No site yet", Site)] }
  doc <- function() { f <- F(); x <- C(); who <- trim(input$sp_rpt_who %||% ""); s <- x$sources; off <- if (nrow(s)) sum(!sp_rs_plugged(s)) else 0L
    pr <- if (nrow(x$products %||% data.table())) x$products else sp_rs_cfg(pd(), "products", PRODUCT_COLS); d <- substr(f$date[f$date != "undated"], 1, 10)
    nm <- basename(pd()); if (identical(nm, "project")) nm <- basename(dirname(pd())); cor <- sum(f$positioning_status %in% CORRECTED_STATES)
    simple_report_doc(paste("Catalogue:", nm),
      meta = c(if (length(d)) paste(min(d), "to", max(d)), paste(nrow(f), "flights"), paste(uniqueN(f$place[nzchar(f$place)]), "sites"), paste("catalogue", format(x$made, "%Y-%m-%d")), if (nzchar(who)) paste("checked by", who)),
      summary = paste0(nrow(f), " flights and photo sets with ", num(sum(sp_rs_num(f$n_images), na.rm = TRUE)), " images; a flight found on several drives is counted once (", sum(sp_rs_num(f$n_copies) > 1, na.rm = TRUE), " have copies). ",
                       if (cor == 1) "1 has" else paste(cor, "have"), " corrected positions (RTK or PPK: the images' positions corrected against a base); the rest are standalone GNSS or not known. ",
                       if (off) sprintf("%d sources were not plugged in: their flights come from the last scan. ", off) else "",
                       sprintf("%s files are not part of a flight (unassigned). %s no site yet.", num(nrow(x$unassigned)), if (sum(!nzchar(f$place)) == 1) "1 flight has" else paste(sum(!nzchar(f$place)), "flights have"))),
      figures = c(Flights = as.character(nrow(f)), Images = num(sum(sp_rs_num(f$n_images), na.rm = TRUE)), Corrected = sprintf("%d", cor), `To look at` = as.character(sum(f$look))),
      notes = c(Flights = "copies counted once", Images = "read from the files", Corrected = "RTK or PPK", `To look at` = "a check not ok"),
      table = table_of(f, pr), table_caption = "Sites",
      methods = c("Files listed and read as found (EXIF and XMP); the same flight on several sources is one flight with copies; sources are never changed (ovid RS catalogue).",
                  "Ground sample distance and photo footprint from the flying height and the camera's sensor and focal length (pinhole, nadir): McGlone, J. C. (ed.) (2013) Manual of Photogrammetry, 6th ed., ASPRS; Colomina, I., Molina, P. (2014) ISPRS J. Photogramm. Remote Sens. 92, 79-97.",
                  "LiDAR swath 2·h·tan(FOV/2): Baltsavias, E. P. (1999) Airborne laser scanning: basic relations and formulas, ISPRS J. Photogramm. Remote Sens. 54.",
                  "Positional accuracy (checkpoints, products' RMSE): ASPRS (2023) ASPRS Positional Accuracy Standards for Digital Geospatial Data, Edition 2, Version 1.0.",
                  "The five checks (✓ ok, ! look, ✕ bad): images read; clock checked; share of images placed; positions corrected; flight log or geotags found."),
      record = c(run = paste("catalogue", format(x$made, "%Y-%m-%d %H:%M")), catalogue = format(x$made, "%Y-%m-%d %H:%M"), sources = as.character(nrow(s)), pipeline = APP_VERSION, core = CORE_VERSION, audit = "output: every file and what it became"), pal = PAL) }
  output$sp_rpt <- downloadHandler(filename = function() "catalogue_report.html",
    content = function(file) { req(F()); withProgress(message = "Making the report", value = 0.5, { b <- tempfile("catalogue"); out <- simple_report_write(doc(), b, "html", PAL); file.copy(out[1], file, overwrite = TRUE) }) })
  output$sp_cat <- downloadHandler(filename = function() "catalogue_tables.zip", contentType = "application/zip",
    content = function(file) { dir <- tempfile("cat"); dir.create(file.path(dir, "catalogue"), recursive = TRUE); dir.create(file.path(dir, "config"))
      a <- setdiff(list.files(CAT(), "\\.csv$"), c("files.csv", "images.csv")); file.copy(file.path(CAT(), a), file.path(dir, "catalogue", a))   # the file and image lists are large: in output/catalogue
      b <- list.files(file.path(pd(), "config"), "\\.csv$"); file.copy(file.path(pd(), "config", b), file.path(dir, "config", b))
      zip::zip(file, c(file.path("catalogue", a), file.path("config", b)), root = dir) })
  output$sp_geo <- downloadHandler(filename = function() "footprints.geojson", contentType = "application/geo+json",
    content = function(file) writeLines(sp_rs_geojson(C()$footprints, F()), file, useBytes = TRUE))
}

# Saves the Edit step's changes, in order, each with the version before in config/history and the change log (backup_cfg):
#   rename  through rename.R (RENAMES$site: sites.csv, flights.csv, site_names.csv)
#   move    the flight's site in flights.csv (a site it names gets a row in sites.csv); positioning is never written here
#   place   latitude and longitude in sites.csv; field region: its region there
#   field   "site for this folder name": site_names.csv (pages/fct_edits.R site_names_set), the site added to sites.csv;
#           display_name, project, notes or exclude_from_archive of a flight (group: sp_rs_key) in flights.csv
sp_rs_apply <- function(pd, ops, who) {
  before <- list(); keep <- function(f) { fp <- file.path(pd, "config", f); if (is.null(before[[f]])) before[[f]] <<- if (file.exists(fp)) copy(read_cfg(fp)) else NA }
  wr <- function(what, d, why) { f <- CONFIG_FILES[[what]]$file; fp <- cfg_path(pd, what); keep(f); dir.create(dirname(fp), showWarnings = FALSE, recursive = TRUE); backup_cfg(fp, d, who, why); fwrite_safe(d, fp) }
  site_row <- function(st, k) { i <- which(st$site == k); if (length(i)) return(list(st = st, i = i[1])); st <- rbind(st, sp_rs_blank(st, site = k)); list(st = st, i = nrow(st)) }
  ensure_site <- function(k) { if (!nzchar(k)) return(); st <- sp_rs_cfg(pd, "sites", SITE_COLS); if (!tolower(k) %in% tolower(st$site)) wr("sites", rbind(st, sp_rs_blank(st, site = k, notes = "made in the Simple view")), "site (Simple view)") }
  fl_row <- function(cf, id) { i <- which(cf$flight_id == id); if (length(i)) return(list(cf = cf, i = i[1])); cf <- rbind(cf, sp_rs_blank(cf, flight_id = id)); list(cf = cf, i = nrow(cf)) }
  for (j in seq_len(nrow(ops))) { q <- ops[j]
    if (q$type == "rename") {
      plan <- rename_plan(pd, "site", q$group, q$to); for (f in unique(plan$file)) keep(f)
      if (nrow(plan)) rename_apply(pd, plan, who, "rename (Simple view)")
    } else if (q$type == "move") {
      ensure_site(q$to); x <- fl_row(sp_rs_cfg(pd, "flights", FLIGHT_COLS), q$item); x$cf[x$i, site := q$to]; wr("flights", x$cf, "site of a flight (Simple view)")
    } else if (q$type == "place") {
      x <- site_row(sp_rs_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, `:=`(latitude = format(q$lat, digits = 9), longitude = format(q$lon, digits = 9))]; wr("sites", x$st, "place (Simple view)")
    } else if (q$type == "field" && identical(q$item, "region")) {
      x <- site_row(sp_rs_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, region := q$to]; wr("sites", x$st, "region (Simple view)")
    } else if (q$type == "field" && identical(q$item, SP_RS_FOLDER_ITEM)) {
      wr("site_names", sp_rs_chr(site_names_set(sp_rs_cfg(pd, "site_names", SITE_NAME_COLS), q$group, q$to), SITE_NAME_COLS), "folder name to site (Simple view)"); ensure_site(q$to)
    } else if (q$type == "field" && q$item %in% c("display_name", "project", "notes", "exclude_from_archive")) {
      x <- fl_row(sp_rs_cfg(pd, "flights", FLIGHT_COLS), sp_rs_id(q$group)); set(x$cf, x$i, q$item, q$to); wr("flights", x$cf, paste(q$item, "(Simple view)")) } }
  n <- nrow(ops)
  fold_n <- sum(ops$type == "field" & ops$item %in% SP_RS_FOLDER_ITEM)   # only folder names wait for a rebuild; a flight's edits show at once
  list(ok = TRUE, message = paste0(sprintf("Saved %d change%s.", n, if (n > 1) "s" else ""), if (fold_n) " Folder names reach their flights at the next scan (Import)." else ""),
       undo = function() for (f in names(before)) { fp <- file.path(pd, "config", f); b <- before[[f]]
         if (identical(b, NA)) { if (file.exists(fp)) { e <- read_cfg(fp)[0]; backup_cfg(fp, e, who, "undo (Simple view)"); fwrite_safe(e, fp) } }
         else { backup_cfg(fp, b, who, "undo (Simple view)"); fwrite_safe(b, fp) } })
}
