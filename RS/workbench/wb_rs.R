# RS/workbench/wb_rs.R  --  The RS Workbench (preview): one flight's images as a spreadsheet grid, a row per image, with
# their positions drawn above it as points (metres east and north of the flight's middle), the selected images, where they
# came from and how the flight was positioned beside it, and every page of the Standard interface around it.
# Part of Ovid Core // RS. Sourced by app.R after rs_pipeline.R and core/shell.R.
# A Workbench session runs the Standard server too (app.R, server(embedded = TRUE)) and shows its pages in the same hidden
# tab set, so every Standard process (map, flight, imagery, GNSS, QA, sources and scans, the viewer, set-up tables) works
# here through the same code. This file adds the data view; wb_edit.R the corrections; wb_project.R the Import view, the
# trees and the tabs.

source(file.path(CORE_DIR, "workbench.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_edit.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_project.R"), local = TRUE)
source(file.path(APP_DIR, "pages", "fct_needs.R"), local = TRUE)          # the Needs you queue, worst first
source(file.path(APP_DIR, "workbench", "wb_views.R"), local = TRUE)       # the Needs you and Positioning tabs

WB_MAX_ROWS <- 5000L                                   # images the grid shows at once


# The grid's columns: the heading, the image's column, its decimals (NA: four significant digits), and whether it is
# ticked at first.
WB_COLS <- data.table(
  p      = c(paste0("time (", zone_text(BASE_OFFSET), ")"), "file", "lat", "lon", "east (m)", "north (m)", "alt (m)", "height (m)", "RTK", "acc (m)", "GSD (cm)", "speed (m/s)",
             "blur (px)", "overlap (%)", "sun (deg)", "exposure (s)", "size (MB)"),
  col    = c("tbase", "file", "lat", "lon", "east", "north", "alt", "alt_rel", "rtk_flag", "acc_xy", "gsd_cm", "speed_ms",
             "blur_px", "fwd_overlap_pct", "sun_elev_deg", "exposure_s", "size_mb"),
  digits = c(NA, NA, 6, 6, 1, 1, 1, 1, 0, 2, 2, 1, 2, 0, 1, NA, 1),
  first  = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
WB_TEXT <- c("tbase", "file")
# What the points can be coloured by.
WB_COLOUR <- c(`minutes into the flight` = "min", `altitude (m)` = "alt", `height above take-off (m)` = "alt_rel", `geotag accuracy (m)` = "acc_xy",
               `speed (m/s)` = "speed_ms", `sun elevation (deg)` = "sun_elev_deg")

# The flights the Workbench lists, newest year first.
wb_tables <- function(f) {
  if (is.null(f) || !nrow(f)) return(data.table())
  f[, .(key = flight_id, label = paste(date, name), year, n = n_images, qa = qa_flags, sensor, status = positioning_status)][order(year == "undated", -suppressWarnings(as.integer(year)), -xtfrm(label))]
}

# A flight's images as the grid's rows (images.csv; the primary copy when the flight was found more than once), in time
# order, with the time in Ovid's base zone, the size in MB and each position in metres east and north of the flight's
# middle. key is the row's number, which ties the grid and the points together.
wb_load <- function(img, fid) {
  if (is.null(img) || !nrow(img) || is.null(fid)) return(NULL)
  d <- img[flight_id == fid]; if (!nrow(d)) return(NULL)
  if (any(d$primary %in% TRUE)) d <- d[primary %in% TRUE]
  d <- copy(d); t <- if (inherits(d$time_utc, "POSIXct")) d$time_utc else { s <- as.character(d$time_utc); s[!nzchar(s %|na|% "")] <- NA; as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") }   # blank: no time
  d[, `:=`(t_utc = t, tbase = ifelse(is.na(t), "", format(to_base(t, 0), "%Y-%m-%d %H:%M:%S")), file = basename(rel), size_mb = size / 1e6)]
  ok <- is.finite(d$lat) & is.finite(d$lon); la <- if (any(ok)) mean(d$lat[ok]) else 0; lo <- if (any(ok)) mean(d$lon[ok]) else 0
  d[, `:=`(east = (lon - lo) * 111320 * cos(la * pi / 180), north = (lat - la) * 110540)]
  for (k in setdiff(WB_COLS$col, names(d))) d[, (k) := NA_real_]
  setorder(d, t_utc, rel, na.last = TRUE)[, key := seq_len(.N)][, min := as.numeric(difftime(t_utc, min(t_utc, na.rm = TRUE), units = "mins"))][]
}
wb_params <- function(d) WB_COLS[col %in% WB_TEXT | vapply(col, function(k) any(!is.na(d[[k]])), NA), p]

# The images a saved ignore rule of file_rules.csv takes (the rule applies on the next rebuild): TRUE per row of d.
wb_ignored <- function(rules, d) {
  out <- rep(FALSE, nrow(d)); if (is.null(rules) || !NROW(rules)) return(out)
  r <- as.data.table(rules)[trim(action) == "ignore" & nzchar(trim(pattern))]
  for (i in seq_len(nrow(r))) out <- out | tryCatch(grepl(r$pattern[i], d$rel, perl = TRUE), error = function(e) FALSE)
  out
}
# State of every cell (see core/workbench.R): "flag" for motion blur over a pixel (blur column) and an RTK solution that is
# not fixed (RTK column); "pend" on every column of an image a saved ignore rule leaves out at the next rebuild.
wb_states <- function(w, params, ign) {
  n <- nrow(w)
  stats::setNames(lapply(params, function(p) { k <- WB_COLS$col[match(p, WB_COLS$p)]; st <- rep("", n); why <- rep("", n)
    if (k == "blur_px") { b <- which(w$blur_px > 1); st[b] <- "flag"; why[b] <- "motion blur over 1 px" }
    if (k == "rtk_flag") { b <- which(!is.na(w$rtk_flag) & w$rtk_flag != 50); st[b] <- "flag"
      why[b] <- ifelse(w$rtk_flag[b] == 34, "RTK float", ifelse(w$rtk_flag[b] == 16, "RTK single (no correction)", "RTK not fixed")) }
    st[ign] <- "pend"; why[ign] <- "left out by a saved file rule; a rebuild applies it"
    data.table(state = st, why = why, rule = why, old = NA_real_) }), params)
}

# A value of column p as the grid shows it.
wb_num <- function(v, p) { dg <- WB_COLS$digits[match(p, WB_COLS$p)]
  if (!is.numeric(v)) { v <- as.character(v); v[is.na(v)] <- ""; return(v) }
  out <- if (is.na(dg)) formatC(signif(v, 4), digits = 4, format = "fg") else formatC(v, format = "f", digits = as.integer(dg))
  out <- sub("^-", "\u2212", trimws(out)); out[is.na(v)] <- ""; out }
wb_cells <- function(w, params, states) {
  out <- lapply(params, function(p) { k <- WB_COLS$col[match(p, WB_COLS$p)]
    list(txt = htmltools::htmlEscape(wb_num(w[[k]], p)), cls = states[[p]]$state) })
  list(txt = lapply(out, `[[`, "txt"), cls = lapply(out, `[[`, "cls"))
}
# Per selected column: how many cells, their mean, low and high.
wb_rs_sel_table <- function(cc, g) {
  x <- cc[!p %in% WB_COLS[col %in% WB_TEXT, p], { k <- WB_COLS$col[match(p[1], WB_COLS$p)]; v <- suppressWarnings(as.numeric(g$w[[k]][r])); f <- v[is.finite(v)]
    sg <- function(z) if (length(f)) signif(z, 4) else NA_real_
    .(n = .N, values = length(f), mean = sg(mean(f)), min = sg(min(f)), max = sg(max(f))) }, by = p]
  if (!nrow(x)) return(NULL)
  tags$table(class = "wb-seltbl", tags$tr(tags$th("column"), tags$th("n"), tags$th(title = "with a value", "values"), tags$th("mean"), tags$th("min"), tags$th("max")),
    lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(x$p[i]), tags$td(x$n[i]), tags$td(x$values[i]), tags$td(x$mean[i]), tags$td(x$min[i]), tags$td(x$max[i]))))
}

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_rs_actions <- function() tagList(
  actionButton("wb_act_out", HTML("&#10005; Leave out&hellip; <kbd>R</kbd>"), class = "wb-btn sm", title = "Leave the selected images out of the catalogue (file_rules.csv, ignore). R or Delete"),
  actionButton("wb_act_edit", "Edit flight\u2026", class = "wb-btn sm", title = "Name, project, site, positioning status and notes of the flight (flights.csv)"))
wb_rs_tools <- function() div(class = "wb-colour", selectInput("wb_colour", NULL, choices = stats::setNames(WB_COLOUR, paste("colour:", names(WB_COLOUR))), width = "260px"))

wb_ui <- function(req) {
  pages <- c(std_pages(), list(convert_tool(), reference_tool("RS"), rs_settings_tool(rail = TRUE)))   # the Reference page, then Settings (in the Pages tree)
  std_panels <- lapply(Filter(function(x) inherits(x, "ovid_tool"), pages), function(x) tabPanel(x$title, value = x$value, div(class = "wb-std d-tool", x$ui)))
  wb_page("RS", PAL, project = if (!VIEWER) projects_switch_ui("proj") else textOutput("wb_project", inline = TRUE), standard_href = "?ui=standard", theme = THEME,
    head = tagList(tags$style(HTML(paste0(ovid_css(ovid_tokens(PAL)), if (exists("OVID_FILES_CSS")) OVID_FILES_CSS else "",
                                          "\n.wb-colour .form-group{margin:0} .wb-colour .selectize-input{min-height:28px;padding:3px 10px;font-size:12.5px} .wb-std #rsmap{height:60vh}"))),
                   tags$script(HTML(OVID_SHELL_JS)), std_head(), tags$script(HTML(wb_keys_js("wb_act_out")))),
    actions = tagList(
      tags$a(class = "wb-btn outline", href = "#", `data-ovid-go` = "wb-import", title = "Sources, the set-up check and the catalogue build", HTML("&#10515; Import data")),
      tags$button(type = "button", class = "wb-btn primary", onclick = "document.getElementById('s_build').click()",
                  title = "Rebuild the catalogue from the sources' last scans (no source is read)", HTML("&#9656; Rebuild")),
      downloadButton("wb_export", HTML("&#8599; Export view"), class = "wb-btn", icon = NULL),
      if (file.exists(file.path(APP_DIR, "docs", "RS_reference_guide.html"))) tags$a(class = "wb-btn", href = "docs/RS_reference_guide.html", target = "_blank", title = "Reference guide", "?")),
    nav = tagList(
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Flights"), span(class = "n", textOutput("wb_n_fl", inline = TRUE))), uiOutput("wb_tree")),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Columns")), checkboxGroupInput("wb_params", NULL, choices = character())),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Aircraft")), group_by_ui("wbgb", wb = TRUE, label = "Group flights by")),   # core grouping.R
      wb_tools_tree(pages),
      uiOutput("wb_setup_tree")),
    main = tagList(uiOutput("wb_tabbar", class = "wb-tabs"),
      do.call(tabsetPanel, c(list(id = "nav", type = "hidden", selected = "wb-needs",
        tabPanel("Needs you", value = "wb-needs", wb_needs_tab_ui()), tabPanel("Positioning", value = "wb-positioning", wb_positioning_ui()),
        tabPanel("Data", value = "wb-data", wb_data_ui(wb_rs_actions(), tools = wb_rs_tools(), xy = TRUE)), tabPanel("Import data", value = "wb-import", wb_import_ui()),
        tabPanel("Products", value = "wb-products", wb_products_ui()), tabPanel("STAC export", value = "wb-stac", wb_stac_ui())), std_panels))),
    aside = uiOutput("wb_inspector"),
    status = uiOutput("wb_status", style = "display:contents"))
}

# ---- server -----------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns: the set-up table editors (E), the catalogue (C), the flight in view on the
# Standard pages (sel), the build state (rv: tick; stale), the set-up check (chk), reload() and show_edit().
wb_server <- function(input, output, session, std) {
  q <- wb_query(isolate(session$clientData$url_search))
  ctx <- environment()                                          # what wb_edit.R and wb_project.R share
  E <- std$E
  view <- reactive(input$nav %||% "wb-needs")                   # the tab shown first (wb_ui)
  set_view <- function(v) updateTabsetPanel(session, "nav", selected = v)
  open_table <- function(id) session$sendCustomMessage("wb-open", WB_OPEN[[id]])
  output$wb_project <- renderText(basename(path.expand(CFG$project_dir)))

  flights <- reactive({ x <- std$C(); if (is.null(x)) data.table() else x$flights })
  tables <- reactive(wb_tables(flights()))
  images <- reactive({ std$rv$tick; f <- file.path(CAT(), "images.csv"); if (file.exists(f)) fread(f, colClasses = list(character = c("time_utc", "rel", "flight_id"))) else data.table() })
  key <- reactiveVal(NULL)
  observeEvent(tables(), { k <- isolate(key()); s <- tables()
    if (is.null(k) || !k %in% s$key) key(if (is.null(k) && !is.null(q$flight) && q$flight %in% s$key) q$flight else if (nrow(s)) s$key[1] else NULL) })
  observeEvent(input$wb_table, { key(input$wb_table); set_view("wb-data") })
  group_by_server("wbgb", reactive({ x <- flights(); if (!nrow(x)) NULL else x[, .(serial = or_blank(aircraft_serial, paste(or_blank(aircraft, "aircraft"), "(no serial)")), file = flight_id, first = or_blank(start_utc, date), last = or_blank(end_utc, date))] }),
    reactive(CFG$project_dir), empty = "No flights yet", open = function(r) { k <- strsplit(r$files, "; ")[[1]][1]; if (k %in% tables()$key) { key(k); set_view("wb-data") } })   # core grouping.R
  observeEvent(key(), std$sel(key()))                           # the Flight page and the map show it too
  part <- reactive({ k <- key(); req(k); f <- flights()[flight_id == k]; req(nrow(f)); list(fid = k, f = as.list(f[1]), label = paste(f$date[1], f$name[1])) })
  dat <- reactive({ d <- wb_load(images(), key()); validate(need(!is.null(d) && nrow(d) > 0, "No images of this flight in the catalogue.")); d })
  params_all <- reactive(wb_params(dat()))
  observeEvent(params_all(), { pa <- params_all(); keep <- intersect(isolate(input$wb_params), pa)
    want <- if (length(keep)) keep else intersect(WB_COLS[first == TRUE, p], pa)
    updateCheckboxGroupInput(session, "wb_params", choices = stats::setNames(pa, pa), selected = want)
    d <- dat(); cv <- WB_COLOUR[vapply(WB_COLOUR, function(k) any(is.finite(d[[k]])), NA)]
    updateSelectInput(session, "wb_colour", choices = if (length(cv)) stats::setNames(cv, paste("colour:", names(cv))) else character(), selected = if (isTRUE(isolate(input$wb_colour) %in% cv)) isolate(input$wb_colour) else cv[1]) })
  sel_params <- reactive(intersect(WB_COLS$p, intersect(input$wb_params, params_all())))
  ign <- reactive(wb_ignored(E$file_rules$data(), dat()))
  grid <- reactive({ d <- dat(); ps <- sel_params(); validate(need(length(ps) > 0, "Tick a column on the left."))
    n <- nrow(d); w <- head(d, WB_MAX_ROWS); st <- wb_states(w, ps, head(ign(), WB_MAX_ROWS))
    list(w = w, n = n, params = ps, states = st, cells = wb_cells(w, ps, st), sig = paste(key(), paste(ps, collapse = ","), n, sep = "|")) })

  # tree
  output$wb_n_fl <- renderText({ s <- tables(); if (nrow(s)) nrow(s) else "" })
  output$wb_tree <- renderUI({ s <- tables(); validate(need(nrow(s), "No catalogue yet. Scan a source on the Sources page (Import data)."))
    k <- key(); on_data <- identical(view(), "wb-data")
    tagList(lapply(unique(s$year), function(y) { x <- s[year == y]
      tagList(div(class = "wb-subsec", y), lapply(seq_len(nrow(x)), function(i) wb_row(x$label[i], key = x$key[i], input = "wb_table", n = format(x$n[i], big.mark = ","),
        on = identical(x$key[i], k) && on_data, warn = nzchar(x$qa[i] %|% ""), title = paste0(x$label[i], " \u00b7 ", x$sensor[i], " \u00b7 ", x$status[i], if (nzchar(x$qa[i] %|% "")) paste0(" \u00b7 ", x$qa[i]) else "")))) })) })
  output$wb_chips <- renderUI({ p0 <- tryCatch(part(), error = function(e) NULL); if (is.null(p0)) return(NULL)
    tagList(span(class = "wb-chip", p0$f$sensor %|% "sensor unknown"), span(class = "wb-chip", style = paste0("border-color:", STATUS_COL[p0$f$positioning_status] %|% PAL$gray), p0$f$positioning_status)) })

  # grid and selection
  output$wb_grid <- renderUI({ g <- grid()
    wb_grid(g$w$key, g$params, g$cells$txt, g$cells$cls, g$sig, corner = "#", times = g$w$key) })
  output$wb_ginfo <- renderText({ g <- grid(); cc <- cells()
    paste0(if (g$n > nrow(g$w)) sprintf("first %s of %s images", num(nrow(g$w)), num(g$n)) else paste(num(g$n), "images"),
           if (nrow(cc)) paste0(" \u00b7 ", num(uniqueN(cc$r)), " selected") else "",
           " \u00b7 click, drag, shift-click; a number selects the image, a heading the column; cmd/ctrl adds; Esc clears") })
  cells <- reactive(wb_sel_cells(input$wb_grid_sel, tryCatch(grid(), error = function(e) NULL)))
  active <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); s <- input$wb_grid_sel
    a <- cc[r == as.integer(s$ar %||% 0) & c == as.integer(s$ac %||% 0)]; if (nrow(a)) a[1] else cc[1] })
  sel <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); w <- grid()$w; r <- sort(unique(cc$r)); list(rows = r, rel = w$rel[r], source = w$source_id[r]) })
  clear_sel <- function() session$sendCustomMessage("wb-grid-clear", list())
  cell_info <- function(r, p) { g <- grid(); w <- g$w; s <- g$states[[p]]; k <- WB_COLS$col[match(p, WB_COLS$p)]
    list(txt = wb_num(w[[k]][r], p), state = s$state[r], why = s$why[r], file = w$file[r]) }
  state_chip <- function(ci) wb_state_chip(ci$state, ci$why, pend = "left out \u00b7 rebuild to apply")

  # the positions as points (WB_XY_JS), in metres, coloured as chosen
  observe({ d <- dat(); v <- input$wb_colour %||% "min"; if (!v %in% names(d)) v <- "min"
    cv <- d[[v]]; f <- cv[is.finite(cv)]; lim <- if (length(f)) range(f) else c(0, 1)
    lab <- names(WB_COLOUR)[match(v, WB_COLOUR)]
    session$sendCustomMessage("wb-xy", wb_xy_message(d$east, d$north, d$key, cv, cv, ifelse(ign(), "pend", ifelse(!is.na(d$blur_px) & d$blur_px > 1, "flag", "")),
      lab = paste(d$file, d$tbase), key = key(), xlab = "east (m)", ylab = "north (m)", clab = lab, vlab = lab, clim = lim,
      pal = unname(c(PAL$blue, PAL$teal, PAL$green, PAL$accent, PAL$gold)), idlab = "#", equal = TRUE)) })

  # cell bar, inspector, status
  output$wb_cellinfo <- renderUI({ cc <- cells()
    if (!nrow(cc)) return(span(class = "txt", "Select cells, images (a number) or columns (a heading), or click or drag on the positions; then leave images out or edit the flight."))
    a <- active(); ci <- cell_info(a$r, a$p)
    tagList(span(class = "mono", style = paste0("color:", PAL$fg4), paste0(LETTERS[min(a$c + 1, 26)], a$r)), span(class = "mono", paste0("#", a$r, " \u00b7 ", a$p)),
            span(class = "mono", style = "font-size:14px", if (nzchar(ci$txt)) ci$txt else "\u2014"), state_chip(ci),
            span(class = "txt", if (uniqueN(cc$r) > 1) paste(num(uniqueN(cc$r)), "images selected") else if (nzchar(ci$why)) ci$why else ci$file)) })
  btn <- function(lbl, target, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  correct_btns <- function() tagList(
    div(class = "wb-inline", btn("&#10005; Leave out&hellip;", "wb_act_out", "danger"), btn("Edit flight&hellip;", "wb_act_edit")),
    div(class = "wb-inline", actionLink("wb_links_open", "The flight's links"), actionLink("wb_map_open", "Show it on the map"), actionLink("wb_rules_open", "File rules")))
  flight_box <- function() { f <- part()$f
    div(class = "wb-hr", span(class = "wb-eb", "Flight"), div(style = "margin-top:6px",
      wb_kv("aircraft", f$aircraft %|% "-"), wb_kv("sensor", f$sensor %|% "-"), wb_kv("images", num(f$n_images)), wb_kv(paste0("start (", zone_text(BASE_OFFSET), ")"), f$start %|% "-"),
      wb_kv("duration", if (is.finite(f$duration_min %||% NA)) paste(f$duration_min, "min") else "-"), wb_kv("positioning", f$positioning_status %|% "-"),
      if (nzchar(f$project %|% "")) wb_kv("project", f$project), if (nzchar(f$site %|% "")) wb_kv("site", f$site),
      if (isTRUE(f$mission_parts > 1)) wb_kv("mission", paste0("sortie ", f$mission_part, " of ", f$mission_parts)),
      if (nzchar(f$copies %|% "")) wb_kv("copies", f$copies), wb_kv("folder", paste0(f$source_id, ":", f$folder))),
      if (nzchar(f$qa_flags %|% "")) p(class = "wb-p", style = "margin-top:6px;color:var(--wb-warn)", f$qa_flags)) }

  output$wb_inspector <- renderUI({ v <- view()
    if (identical(v, "wb-import")) return(wb_import_aside())
    if (!identical(v, "wb-data")) return(NULL)
    cc <- cells(); p0 <- tryCatch(part(), error = function(e) NULL)
    if (is.null(p0)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet"), wb_empty_acts(NULL)), p(class = "wb-p", "Scan a source on the Sources page.")))
    if (!nrow(cc)) { d <- tryCatch(dat(), error = function(e) NULL)
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No image selected"),
        div(class = "sub", "Click or drag in the grid; a number selects the image, a heading the column. On the positions, click a point or drag a box.")),
        if (!is.null(d)) div(class = "wb-facts", wb_fact("images", num(nrow(d)), p0$label), wb_fact("placed", num(sum(is.finite(d$east))), "with a position read"),
                             wb_fact("blurred", num(sum(d$blur_px > 1, na.rm = TRUE)), "over 1 px"), wb_fact("left out", num(sum(ign())), "saved, not rebuilt")),
        p(class = "wb-p", style = "color:var(--wb-fg2)", p0$f$positioning_evidence %|% ""), flight_box())) }
    if (uniqueN(cc$r) > 1) { g <- grid()
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2(paste(num(uniqueN(cc$r)), "images")),
          div(class = "sub", paste(g$w$tbase[min(cc$r)], "\u2013", substr(g$w$tbase[max(cc$r)], 12, 19)))),
        wb_rs_sel_table(cc, g), div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", correct_btns())), flight_box())) }
    a <- active(); ci <- cell_info(a$r, a$p); w <- grid()$w; r <- a$r; f <- p0$f
    div(class = "wb-insp",
      div(span(class = "wb-eb", "Selected image"), tags$h2(w$file[r]), div(class = "sub", paste(p0$label, "\u00b7", w$tbase[r] %|% "no time"))),
      div(style = "display:flex;align-items:baseline;gap:10px;flex-wrap:wrap", span(style = "font-family:var(--wb-mono);font-size:18px", paste(a$p, if (nzchar(ci$txt)) ci$txt else "\u2014")), state_chip(ci)),
      div(span(class = "wb-eb", "Lineage"), div(style = "margin-top:6px",
        wb_step(paste("Read from", w$source_id[r]), paste0(w$rel[r], " \u00b7 ", wb_num(w$size_mb[r], "size (MB)"), " MB")),
        wb_step(paste("Camera", w$sensor[r] %|% "unknown"), paste0("serial ", w$camera_serial[r] %|% "-")),
        wb_step(if (nzchar(w$tbase[r] %|% "")) paste("Taken", w$tbase[r], zone_text(BASE_OFFSET)) else "No time", paste("flight time from", f$time_source %|% "the camera")),
        if (is.finite(w$lat[r])) wb_step(sprintf("At %.6f, %.6f", w$lat[r], w$lon[r]), paste0(if (is.finite(w$alt[r])) sprintf("altitude %.1f m", w$alt[r]) else "", if (is.finite(w$acc_xy[r])) sprintf(" \u00b7 geotag accuracy %.2f m", w$acc_xy[r]) else ""))
        else wb_step("No position of its own", "big folders are read as a sample; the MRK and other sidecars still place every exposure", "w"),
        wb_step(paste("In flight", p0$label), paste(f$positioning_status %|% "", "\u00b7", f$positioning_evidence %|% ""), if (f$positioning_status %in% POSITIONING[1:3]) "" else "w"),
        if (nzchar(ci$why)) wb_step(if (ci$state == "pend") "Left out" else "Flagged", ci$why, if (ci$state == "pend") "e" else "w"))),
      div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", correct_btns())),
      flight_box())
  })

  wb_import_aside <- function() { m <- tryCatch(fread(file.path(path.expand(CFG$project_dir), "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) NULL)
    v <- function(k) if (!is.null(m) && nrow(m)) m$value[match(k, m$key)] else NA
    steps <- c(`Set-up` = "sources, flights, sites, links and file rules", Scan = "each source listed; image headers read once per folder, again only where a file changed",
               Gather = "every source's last scan, so an unplugged drive stays in the catalogue", Flights = "images grouped into flights, copies found, logs, geotags, MRK, GNSS and plans linked, positioning worked out",
               Catalogue = "flights, images, links, geometry and the file audit; on request the viewer")
    div(class = "wb-insp", div(span(class = "wb-eb", "Pipeline"), div(style = "margin-top:8px",
        lapply(seq_along(steps), function(i) div(class = "wb-pstep", span(class = "n", i), div(div(names(steps)[i]), div(class = "s", steps[[i]])))))),
      div(class = "wb-hr", span(class = "wb-eb", "Last build"), div(style = "margin-top:6px",
        if (is.null(m)) p(class = "wb-p", "Not built yet.") else tagList(wb_kv("when", format(as.POSIXct(run_mtime(CFG$project_dir), origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
          wb_kv("sources", v("sources") %|% "?"), wb_kv("files", v("files") %|% "?"), wb_kv("images", v("images") %|% "?"), wb_kv("flights", v("flights") %|% "?")))),
      p(class = "wb-p", style = "color:var(--wb-fg2)", "Sources are only read: nothing is written inside them.")) }

  output$wb_status <- renderUI({ std$rv$tick; cc <- cells(); g <- tryCatch(grid(), error = function(e) NULL)
    np <- nrow(run_problems_read(CFG$project_dir))                # listed on the Sources page, under the set-up check (pages/set_up_check.R)
    mid <- if (!identical(view(), "wb-data") || is.null(g)) NULL else if (nrow(cc)) span(paste(num(uniqueN(cc$r)), "images selected")) else span(paste(num(g$n), "images"))
    tagList(span(style = paste0("color:", PAL$fg), HTML(if (np) paste0('<a href="#" data-ovid-go="Sources" style="color:inherit"><span style="color:', PAL$gold, '">&#9650;</span> ', np, " problems</a>")
                                                      else paste0('<span style="color:', PAL$green, '">&#10003;</span> No problems'))),
            if (isTRUE(std$stale())) tags$a(class = "wb-stale", href = "#", `data-ovid-go` = "wb-import", HTML("&#9679; Set-up changed since the last build")),
            div(class = "wb-fill mono", style = "display:flex;justify-content:center;gap:16px", mid),
            span(class = "mono", paste0(zone_text(BASE_OFFSET), " \u00b7 RS ", RS_VERSION, " \u00b7 workbench ", OVID_WORKBENCH_VERSION))) })
  output$wb_export <- downloadHandler(function() paste0(safe_name(part()$label), ".csv"),     # ovid_rs_<flight>_<date>.csv (core ovid_downloads, app.R)
    function(file) { g <- grid(); ks <- WB_COLS$col[match(g$params, WB_COLS$p)]; x <- g$w[, c("source_id", "rel", ks), with = FALSE]
      setnames(x, c("source_id", "rel", g$params)); fwrite(x, file) })

  wb_project_server(input, output, session, ctx)
  wb_edit_server(input, output, session, ctx)
  wb_views_server(input, output, session, ctx)
  invisible(ctx)
}
