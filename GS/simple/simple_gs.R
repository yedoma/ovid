# GS/simple/simple_gs.R  --  The Simple view of GS (M 28 Oct): a field check of the geophysics archive, in four steps, on the
# side the ERT | GPR switch shows. Import: the folders, Scan and a checklist (files sorted, ERT read, originals, GPR lines
# complete, surveys with a site and a date). View: ERT, a tile per site (or folder) with its surveys and their pseudosections;
# GPR, the tracks and the radargram of a line (view only). Edit (ERT): sites (name, place, environment), which site each
# survey is at, and each survey's name, date, transect and quality, with dates and sites from folder names proposed for review.
# Export: the catalogue report, the catalogue tables, the survey data and the previews. The Standard server runs underneath
# (server(embedded = TRUE)); Edit writes only sites.csv, transects.csv and surveys.csv (a rename through rename.R). QC
# profiles, flags, electrodes turned off and elevations are not edited here, and data files never. Sourced by app.R.

SP_GS_TILES <- 8                                # groups shown before "Show all"
SP_GS_KINDS <- c("original", "merged")          # the surveys shown; copies, subsets and processed sets are folded into them
SP_GS_STOP <- c("raw", "data", "ert", "survey", "surveys", "new", "old", "copy", "backup", "final", "files")   # not a site's name
sp_gs_look <- function() as.numeric(SETTINGS$simple_look_pct %||% SIMPLE_LOOK_PCT)   # % flagged above which a survey needs a look (R/config.R)

sp_gs_chr <- function(d, cols) { d <- as.data.table(d); for (v in names(d)) set(d, j = v, value = { x <- as.character(d[[v]]); x[is.na(x)] <- ""; x }); ensure_cols(d, cols) }
sp_gs_blank <- function(d, ...) { r <- as.data.table(as.list(stats::setNames(rep("", ncol(d)), names(d)))); x <- list(...)
  for (k in names(x)) set(r, j = k, value = as.character(x[[k]])); r }
sp_gs_cfg <- function(pd, what, cols) { p <- cfg_path(pd, what); sp_gs_chr(if (file.exists(p)) read_cfg(p) else data.table(), cols) }

# ---- folder names --------------------------------------------------------------------------------------------------------------
# A date in a folder name, as precise as the name says: "2018-03-14", "20180314", "14 Mar 2018" (a day), "March 2018", "Mar18",
# "2018_03" (a month), or "Summer18", "2019" (a year; a season is no more than its year). "" when the name holds none.
SP_GS_MONTHS <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, sept = 9, oct = 10, nov = 11, dec = 12,
                  january = 1, february = 2, march = 3, april = 4, june = 6, july = 7, august = 8, september = 9, october = 10, november = 11, december = 12)
sp_gs_folder_date <- function(x) vapply(as.character(x), function(s) { s <- tolower(s %||% ""); if (is.na(s) || !nzchar(s)) return("")
  yr <- function(y) { y <- as.integer(y); if (y < 100) y <- y + 2000L; if (y >= 1980 && y <= 2100) y else NA_integer_ }
  ok_d <- function(y, m, d) { r <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d))); if (!is.na(r)) format(r) else "" }
  if (length(m <- regmatches(s, regexec("(19|20)(\\d{2})[-_. ]?(\\d{2})[-_. ]?(\\d{2})(?!\\d)", s, perl = TRUE))[[1]]) && length(m)) {
    r <- ok_d(as.integer(paste0(m[2], m[3])), as.integer(m[4]), as.integer(m[5])); if (nzchar(r)) return(r) }
  mn <- paste(names(SP_GS_MONTHS)[order(-nchar(names(SP_GS_MONTHS)))], collapse = "|")
  if (length(m <- regmatches(s, regexec(sprintf("(\\d{1,2})[-_. ]?(%s)[a-z]*[-_. ]?((19|20)?\\d{2})(?!\\d)", mn), s, perl = TRUE))[[1]]) && length(m)) {
    y <- yr(m[4]); if (!is.na(y)) { r <- ok_d(y, SP_GS_MONTHS[[m[3]]], as.integer(m[2])); if (nzchar(r)) return(r) } }
  if (length(m <- regmatches(s, regexec(sprintf("(?<![a-z])(%s)[a-z]*[-_. ]?((19|20)?\\d{2})(?!\\d)", mn), s, perl = TRUE))[[1]]) && length(m)) {
    y <- yr(m[3]); if (!is.na(y)) return(sprintf("%04d-%02d", y, SP_GS_MONTHS[[m[2]]])) }
  if (length(m <- regmatches(s, regexec("((19|20)\\d{2})[-_.](0[1-9]|1[0-2])(?!\\d)", s, perl = TRUE))[[1]]) && length(m)) return(sprintf("%s-%s", m[2], m[4]))
  if (length(m <- regmatches(s, regexec("(?<!\\d)((19|20)\\d{2})(?!\\d)", s, perl = TRUE))[[1]]) && length(m)) return(m[2])
  if (length(m <- regmatches(s, regexec("(spring|summer|fall|autumn|winter)[-_. ]?(\\d{2})(?!\\d)", s, perl = TRUE))[[1]]) && length(m)) { y <- yr(m[3]); if (!is.na(y)) return(as.character(y)) }
  "" }, "", USE.NAMES = FALSE)
# A site's name in a folder name: the name without its date, the words SP_GS_STOP lists and stray separators; "" when nothing
# that reads as a name (three letters or more) is left.
sp_gs_folder_site <- function(x) vapply(as.character(x), function(s) { if (is.na(s) || !nzchar(s)) return("")
  mn <- paste(names(SP_GS_MONTHS)[order(-nchar(names(SP_GS_MONTHS)))], collapse = "|")
  v <- gsub(sprintf("(?i)\\b(%s)[a-z]*\\b", mn), " ", s, perl = TRUE); v <- gsub("(?i)(spring|summer|fall|autumn|winter)\\d*", " ", v, perl = TRUE)
  v <- gsub("(?<![A-Za-z])\\d+(?![A-Za-z])", " ", v, perl = TRUE); w <- strsplit(gsub("[-_.()]+", " ", v), "\\s+")[[1]]; w <- w[nzchar(w) & !tolower(w) %in% SP_GS_STOP]
  out <- trim(paste(w, collapse = " ")); if (nchar(gsub("[^A-Za-z]", "", out)) >= 3) out else "" }, "", USE.NAMES = FALSE)

# ---- the catalogue as Simple shows it -------------------------------------------------------------------------------------------
# The surveys shown (originals and merged sets), each with its group (site, else folder), its date (the file's, the one people
# entered, or "(folder)" when only its folder name gives one), % flagged under the catalogue's QC profile, and the derived sets
# folded into it.
sp_gs_surveys <- function(s) {
  if (is.null(s) || !nrow(s)) return(NULL); s <- copy(s); s[, folder := sub("/.*", "", primary_file)]
  if ("parent_ids" %in% names(s)) s[, parent_ids := as.character(parent_ids)]   # a catalogue with no parents reads as logical NA
  par <- if ("parent_ids" %in% names(s)) s[!kind %in% SP_GS_KINDS & nzchar(parent_ids %|na|% ""), .(p = as.character(unlist(strsplit(parent_ids, "[;,]\\s*")))), by = survey_id] else data.table(p = character())
  nder <- if (nrow(par)) par[, .N, by = p] else data.table(p = character(), N = integer())
  x <- s[kind %in% SP_GS_KINDS]
  fd <- sp_gs_folder_date(x$folder); own <- !x$date %in% c("undated", "")
  x[, `:=`(derived = fcoalesce(nder$N[match(survey_id, nder$p)], 0L), folder_date = fd, dated = own,
           date_shown = fifelse(own, date, fifelse(nzchar(fd), paste(fd, "(folder)"), "no date")),
           flagged_pct = fifelse(is.na(suppressWarnings(as.numeric(pct_retained))), NA_real_, round(100 - as.numeric(pct_retained), 1)),
           group = fifelse(nzchar(site), site, folder), grouped_by = fifelse(nzchar(site), "site", "folder"))]
  x[, look := !is.na(flagged_pct) & flagged_pct > sp_gs_look()]
  x[]
}
# One row per group: its surveys, arrays, dates, how many need a look, and a preview to show on its tile.
sp_gs_groups <- function(x) { if (is.null(x) || !nrow(x)) return(NULL)
  x[, .(surveys = .N, arrays = paste(unique(array_lbl[nzchar(array_lbl)]), collapse = ", "), first = { d <- substr(date[dated], 1, 10); if (length(d)) min(d) else "" },
        last = { d <- substr(date[dated], 1, 10); if (length(d)) max(d) else "" }, folder_only = !any(dated) && any(nzchar(folder_date)), fdate = folder_date[nzchar(folder_date)][1] %||% "",
        undated = sum(!dated), look = sum(look), flagged = round(stats::median(flagged_pct, na.rm = TRUE), 1), measurements = sum(as.numeric(n_read), na.rm = TRUE),
        preview = preview[nzchar(preview %|na|% "")][1] %||% "", by = grouped_by[1]), by = group][order(-look, group)] }

# ---- interface ---------------------------------------------------------------------------------------------------------------
sp_gs_import_ui <- function() tagList(
  p(class = "sp-lead", "Point ovid at the archive (ERT downloads, EarthImager sets, photos …), then press Scan. Every file is listed; the raw files are never changed."),
  div(class = "sp-cols",
    div(class = "sp-card", div(class = "sp-h", "Folders"), std_folders(),
      p(class = "sp-dim sp-small", "GPR folders are set on the GPR side of the Standard view (Map)."),
      div(class = "sp-row", actionButton("rebuild", "Scan", class = "sp-btn"))),
    div(class = "sp-card", div(class = "sp-h", "What was read"), uiOutput("sp_read"), uiOutput("sp_checks"))),
  div(class = "sp-card", div(class = "sp-h", "Folders in the archive"), uiOutput("sp_folders")))

sp_gs_view_ui <- function() tagList(
  conditionalPanel("input.sp_area != 'GPR'",
    uiOutput("sp_hero"),
    div(class = "sp-row sp-filters", style = "margin-top:0",
      radioButtons("sp_filter", NULL, c("By folder" = "folder", "By site" = "site", "Need a look" = "look", "No date" = "nodate"), inline = TRUE),
      div(class = "sp-fill"), textInput("sp_q", NULL, placeholder = "Find a survey, site or folder", width = "240px")),
    uiOutput("sp_tiles", class = "sp-tiles"), uiOutput("sp_more"),
    div(class = "sp-card", div(class = "sp-h", textOutput("sp_group_title", inline = TRUE)), uiOutput("sp_rows")),
    div(class = "sp-card",
      div(class = "sp-row", style = "margin-top:0;justify-content:space-between", tags$b(textOutput("sp_plot_title", inline = TRUE)),
          radioButtons("sp_colour", NULL, c("Apparent resistivity" = "rhoa", "Error" = "error"), inline = TRUE)),
      plotOutput("sp_pseudo", height = "340px"), p(class = "sp-dim sp-small", textOutput("sp_plot_note", inline = TRUE)))),
  conditionalPanel("input.sp_area == 'GPR'",
    uiOutput("sp_gpr_hero"),
    div(class = "sp-card", div(class = "sp-row", style = "margin-top:0", selectizeInput("sp_gpr_day", "Day or folder", NULL, width = "360px"), div(class = "sp-fill"),
                                selectizeInput("sp_gpr_line", "Line", NULL, width = "260px")),
      plotly::plotlyOutput("sp_gpr_map", height = "320px")),
    div(class = "sp-card", div(class = "sp-h", textOutput("sp_gpr_title", inline = TRUE)), plotly::plotlyOutput("sp_gpr_rg", height = "340px"),
      p(class = "sp-dim sp-small", "Raw amplitude, as recorded (the mean of each trace taken off); two-way time. GPR lines are viewed here, not changed: processing is in the Standard view."))))

sp_gs_edit_ui <- function() tagList(
  conditionalPanel("input.sp_area == 'GPR'", div(class = "sp-card sp-dim", "GPR lines are viewed only: there is nothing to name or place here. Switch to ERT to edit sites and surveys.")),
  conditionalPanel("input.sp_area != 'GPR'", uiOutput("sp_propose"), uiOutput("sp_survey_card"), simple_edit_ui("spe", "site", "surveys")))

sp_gs_export_ui <- function() div(class = "sp-cols",
  div(class = "sp-card", div(class = "sp-h", "Catalogue report"),
    p("One page: per site its surveys, dates, arrays, electrodes and spacing, measurements, how many are flagged and their quality, and the surveys with no site yet; the methods with their sources."),
    textInput("sp_rpt_who", "Your initials (on the report)", width = "220px"),
    div(class = "sp-row", downloadButton("sp_rpt", "Download the report (HTML)", class = "sp-btn", icon = NULL))),
  div(class = "sp-card", div(class = "sp-h", "Tables, data and previews"),
    p("The catalogue tables and the set-up; each survey's measurements with their flags (CSV) beside its original file; and the pseudosection previews."),
    div(class = "sp-row", downloadButton("sp_cat", "Catalogue tables (.zip)", class = "sp-btn2", icon = NULL), downloadButton("sp_data", "Survey data (.zip)", class = "sp-btn2", icon = NULL),
        downloadButton("sp_prev", "Previews (.zip)", class = "sp-btn2", icon = NULL)),
    p(class = "sp-dim sp-small", "Any survey on its own, Res2DInv files or the full archive: ", tags$a(href = simple_std_href("Archive"), "Standard, Archive"), ".")))

SP_GS_CSS <- paste(".sp-checks{margin-top:10px;border-top:1px solid var(--sp-line)}",
  ".sp-check{display:flex;flex-wrap:wrap;align-items:baseline;gap:4px 10px;padding:7px 0;border-bottom:1px solid var(--sp-line)}",
  ".sp-check>.sp-dot{flex:none;width:9px;height:9px;border-radius:50%;background:var(--sp-ok);align-self:center}",
  ".sp-check.sp-warn>.sp-dot{background:var(--sp-warn)} .sp-check.sp-error>.sp-dot{background:var(--sp-err)} .sp-check>.sp-fill{flex:1}",
  ".sp-table{width:100%;border-collapse:collapse;font-size:13px;font-variant-numeric:tabular-nums} .sp-table th{text-align:left;font-weight:600;color:var(--sp-mut);padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line2)}",
  ".sp-table td{padding:4px 12px 4px 0;border-bottom:1px solid var(--sp-line);vertical-align:middle} .sp-table tr.on td{background:color-mix(in srgb,var(--sp-acc) 10%,transparent)}",
  ".sp-table img{height:34px;border-radius:4px;display:block} .sp-tile-img{display:block;width:100%;height:64px;object-fit:cover;border-radius:6px;margin:8px 0;background:var(--sp-bg)}",
  ".sp-filters .form-group{margin:0} .sp-filters .shiny-options-group{display:flex;flex-wrap:wrap;gap:6px}",
  ".sp-filters .radio-inline{margin:0;padding:4px 12px;border:1px solid var(--sp-line2);border-radius:999px} .sp-filters .radio-inline input{display:none}",
  ".sp-filters .radio-inline:has(input:checked){border-color:var(--sp-acc);box-shadow:inset 0 0 0 1px var(--sp-acc)} .sp-area .on{font-weight:600}",
  ".sp-card .form-group{margin-bottom:6px}",
  ".sp-area{padding:0} .sp-area .form-group{margin:0} .sp-area .shiny-options-group{display:flex} .sp-area .radio-inline{margin:0;padding:4px 12px;cursor:pointer}",
  ".sp-area .radio-inline input{display:none} .sp-area .radio-inline:has(input:checked){background:var(--sp-acc);color:var(--sp-onacc);font-weight:600}")

simple_ui <- function(req) {
  pages <- Filter(function(x) inherits(x, "ovid_tool"), std_pages(FALSE))                        # the Standard pages, hidden: their
  hidden <- lapply(pages, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))   # inputs exist for its server
  area <- tryCatch(project_settings_read(path.expand(CFG$project_dir))$area, error = function(e) NULL); if (!identical(area, "GPR")) area <- "ERT"
  sw <- div(class = "d-views sp-views sp-area", role = "group", `aria-label` = "Side of GS",
    radioButtons("sp_area", NULL, c("ERT" = "ERT", "GPR" = "GPR"), selected = area, inline = TRUE))
  simple_page("GS", PAL, import = sp_gs_import_ui(), view = sp_gs_view_ui(), edit = sp_gs_edit_ui(), export = sp_gs_export_ui(),
              project = projects_switch_ui("proj"), hidden = hidden, theme = THEME, actions = sw, head = tags$style(HTML(SP_GS_CSS)))
}

# ---- server ------------------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns (E, rv, proj, cat_s, stats, gpr_ok, gpr_tracks, mark, ...).
simple_server <- function(input, output, session, std) {
  cat_s <- std$cat_s; pd <- std$proj
  if (dir.exists(file.path(CAT, "previews"))) shiny::addResourcePath("gs-previews", file.path(CAT, "previews"))
  steps <- simple_steps_server(session, function() c(import = !is.null(cat_s()) && nrow(cat_s()) > 0))
  if (isTRUE(nrow(isolate(cat_s())) > 0)) steps$go("view")
  observeEvent(input$sp_area, { p <- path.expand(pd()); if (!dir.exists(p)) return()
    s <- project_settings_read(p); if (!identical(s$area, input$sp_area)) { s$area <- input$sp_area; project_settings_save(p, s) } }, ignoreInit = TRUE)
  go_step <- function(k) sprintf("document.querySelector('.sp-step[data-step=\"%s\"]').click()", k)
  to_std <- function(page, label, extra = list()) tags$a(href = simple_std_href(page, extra), label)
  x_all <- reactive({ std$rv$tick; sp_gs_surveys(cat_s()) })
  last_run <- reactive({ cat_s(); f <- file.path(PROJECT, "output", "audit", "run_history.csv"); if (!file.exists(f)) return(NULL)
    # safe: the run history is shown only
    h <- tryCatch(fread(f), error = function(e) NULL); if (is.null(h) || !nrow(h)) NULL else h[.N] })
  files <- reactive({ cat_s(); f <- file.path(CAT, "files.csv"); if (file.exists(f)) tryCatch(fread(f, colClasses = "character"), error = function(e) data.table()) else data.table() })

  # ---- Import
  output$sp_read <- renderUI({ s <- cat_s(); if (is.null(s) || !nrow(s)) return(p(class = "sp-dim", "Nothing scanned yet."))
    x <- x_all(); g <- std$gpr_ok(); r <- last_run()
    tags$dl(class = "sp-kv",
      tags$dt("Files"), tags$dd(sprintf("%s catalogued", num(nrow(files())))),
      tags$dt("ERT surveys"), tags$dd(sprintf("%d originals; %d derived sets (trims, merges, subsets, copies) linked and folded", nrow(x), sum(!s$kind %in% SP_GS_KINDS))),
      tags$dt("Measurements"), tags$dd(num(sum(as.numeric(x$n_read), na.rm = TRUE))),
      tags$dt("GPR lines"), tags$dd(num(nrow(g))),
      tags$dt("Scanned"), tags$dd(if (!is.null(r)) as.character(r$started) else "—"))
  })
  output$sp_checks <- renderUI({ s <- cat_s(); if (is.null(s) || !nrow(s)) return(NULL); x <- x_all(); f <- files(); g <- tryCatch(std$gpr_ok(), error = function(e) data.table())
    un <- if (nrow(f) && "format" %in% names(f)) sum(f$format == "unrecognised") else 0L; rd <- sum(as.numeric(x$n_read) > 0, na.rm = TRUE)
    gfull <- if (nrow(g)) sum(!is.na(g$data) & !is.na(g$cor) & nzchar(g$cor %|na|% "")) else 0L
    nosite <- sum(!nzchar(x$site)); nodate <- sum(!x$dated); fdate <- sum(!x$dated & nzchar(x$folder_date))
    row <- function(lv, what, detail, fix = NULL) div(class = paste("sp-check", paste0("sp-", lv)), span(class = "sp-dot"), tags$b(what), span(class = "sp-dim", detail), div(class = "sp-fill"), fix)
    div(class = "sp-checks",
      row(if (!un) "ok" else "warn", "Files sorted", sprintf("%s files; %d not recognised", num(nrow(f)), un), if (un) to_std("Archive", "Archive")),
      row(if (rd == nrow(x)) "ok" else "warn", "ERT data read", sprintf("%d / %d surveys, %s measurements", rd, nrow(x), num(sum(as.numeric(x$n_read), na.rm = TRUE)))),
      row("ok", "Originals found", sprintf("%d originals; %d derived sets linked to them", nrow(x), sum(!s$kind %in% SP_GS_KINDS))),
      row(if (!nrow(g) || gfull == nrow(g)) "ok" else "warn", "GPR lines complete", if (nrow(g)) sprintf("%d / %d with header, traces and GPS", gfull, nrow(g)) else "no GPR folders scanned"),
      row(if (!nosite) "ok" else "warn", "Surveys with a site", if (!nosite) sprintf("%d of %d", nrow(x), nrow(x)) else sprintf("%d of %d have none yet: give them one in Edit", nosite, nrow(x))),
      row(if (!nodate) "ok" else "warn", "Surveys with a date", if (!nodate) sprintf("%d of %d", nrow(x), nrow(x)) else sprintf("%d of %d undated; %d folder names suggest one (Edit)", nodate, nrow(x), fdate))) })
  output$sp_folders <- renderUI({ x <- x_all(); if (is.null(x)) return(p(class = "sp-dim", "No surveys yet."))
    fo <- x[, .(surveys = .N, examples = paste(utils::head(name, 3), collapse = ", "), date = folder_date[1]), by = folder][order(-surveys)]
    tb <- function(k) tags$table(class = "sp-table", tags$thead(tags$tr(tags$th("Folder"), tags$th("Surveys"), tags$th("Examples"), tags$th("Date in its name"))),
      tags$tbody(lapply(k, function(i) tags$tr(tags$td(fo$folder[i]), tags$td(fo$surveys[i]), tags$td(class = "sp-dim", fo$examples[i]), tags$td(if (nzchar(fo$date[i])) fo$date[i] else "—")))))
    n <- 12L; tagList(tb(seq_len(min(nrow(fo), n))), if (nrow(fo) > n) tags$details(tags$summary(sprintf("All %d folders", nrow(fo))), tb(seq(n + 1L, nrow(fo))))) })

  # ---- View, ERT
  sel_g <- reactiveVal(NULL); sel_s <- reactiveVal(NULL); all_tiles <- reactiveVal(FALSE)
  observeEvent(input$sp_pick, { sel_g(input$sp_pick); sel_s(NULL) })
  observeEvent(input$sp_survey, sel_s(input$sp_survey))
  observeEvent(input$sp_all, all_tiles(!all_tiles()))
  shown <- reactive({ x <- x_all(); req(x); w <- input$sp_filter %||% "folder"; q <- tolower(trim(input$sp_q %||% ""))
    if (w == "site") x <- x[nzchar(site)] else if (w == "look") x <- x[look == TRUE] else if (w == "nodate") x <- x[!dated]
    if (w == "folder") x[, `:=`(group = folder, grouped_by = "folder")]
    if (nzchar(q)) x <- x[grepl(q, tolower(paste(name, site, folder, survey_id)), fixed = TRUE)]
    x })
  groups <- reactive(sp_gs_groups(shown()))
  cur_g <- reactive({ g <- groups(); k <- sel_g(); if (is.null(g) || !nrow(g)) return(NULL); if (is.null(k) || !k %in% g$group) g$group[1] else k })
  rows_g <- reactive({ k <- cur_g(); x <- shown(); if (is.null(k)) return(NULL); x[group == k][order(date)] })
  cur_s <- reactive({ r <- rows_g(); k <- sel_s(); if (is.null(r) || !nrow(r)) return(NULL); if (is.null(k) || !k %in% r$survey_id) r$survey_id[1] else k })
  prev_src <- function(f) if (nzchar(f %|na|% "") && file.exists(file.path(CAT, "previews", f))) paste0("gs-previews/", utils::URLencode(f)) else NULL
  output$sp_hero <- renderUI({ x <- x_all()
    if (is.null(x)) return(simple_hero("off", "Nothing scanned yet", "Choose the folders and press Scan.", tags$button(type = "button", class = "btn sp-btn", onclick = go_step("import"), "Import")))
    g <- sp_gs_groups(x); d <- substr(x$date[x$dated], 1, 10); nl <- sum(x$look)
    simple_hero(if (nl) "warn" else "ok", sprintf("%d surveys · %d %s", nrow(x), nrow(g), if (any(nzchar(x$site))) "sites and folders" else "folders"),
      paste0(if (length(d)) paste0(min(d), " to ", max(d), ". ") else "", if (nl) sprintf("%d have more than %g %% of their measurements flagged (QC profile %s).", nl, sp_gs_look(), x$qc_profile[1] %||% "") else "Nothing over the flagged-share limit."),
      tags$button(type = "button", class = "btn sp-btn2", onclick = "document.getElementById('rebuild').click()", "Scan again")) })
  output$sp_tiles <- renderUI({ g <- groups(); if (is.null(g) || !nrow(g)) return(p(class = "sp-dim", "No survey matches.")); k <- cur_g()
    if (!all_tiles()) g <- utils::head(g, SP_GS_TILES)
    lapply(seq_len(nrow(g)), function(i) { r <- g[i]; src <- prev_src(r$preview)
      simple_tile(r$group, if (r$look) "warn" else "ok", state = if (r$look) sprintf("%d to look at", r$look) else r$by,
        spark = if (!is.null(src)) tags$img(class = "sp-tile-img", src = src, alt = ""),
        facts = stats::setNames(list(as.character(r$surveys), if (nzchar(r$arrays)) r$arrays else "—",
                                     if (nzchar(r$first)) (if (r$first == r$last) r$first else paste(r$first, "to", r$last)) else if (r$folder_only) paste(r$fdate, "(folder)") else "no date",
                                     if (is.na(r$flagged)) "—" else sprintf("%.0f %%", r$flagged)), c("Surveys", "Arrays", "Date", "Flagged, median")),
        on = identical(r$group, k), link = if (r$look) list(label = "Open in Standard: QC", page = "QC")) }) })
  output$sp_more <- renderUI({ g <- groups(); if (is.null(g) || nrow(g) <= SP_GS_TILES) return(NULL)
    div(class = "sp-row", style = "margin-top:0", actionButton("sp_all", if (all_tiles()) "Show fewer" else sprintf("Show all %d", nrow(g)), class = "sp-btn2")) })
  output$sp_group_title <- renderText({ k <- cur_g(); if (is.null(k)) "" else paste("Surveys ·", k) })
  output$sp_rows <- renderUI({ r <- rows_g(); if (is.null(r) || !nrow(r)) return(NULL); k <- cur_s()
    tagList(tags$table(class = "sp-table", tags$thead(tags$tr(tags$th(""), tags$th("Survey"), tags$th("Array"), tags$th("Electrodes · spacing"), tags$th("Measurements"), tags$th("Flagged"), tags$th("Date"))),
      tags$tbody(lapply(seq_len(nrow(r)), function(i) { s <- r[i]; src <- prev_src(s$preview)
        tags$tr(class = if (identical(s$survey_id, k)) "on", style = "cursor:pointer", onclick = sprintf("Shiny.setInputValue('sp_survey', %s, {priority: 'event'})", jsonlite::toJSON(s$survey_id, auto_unbox = TRUE)),
          tags$td(if (!is.null(src)) tags$img(src = src, alt = "")), tags$td(tags$b(s$name), if (s$derived) span(class = "sp-dim", sprintf(" +%d derived folded", s$derived))),
          tags$td(s$array_lbl), tags$td(if (is.na(s$n_electrodes)) "—" else sprintf("%s · %s m", s$n_electrodes, format(s$spacing_m))), tags$td(num(as.numeric(s$n_read))),
          tags$td(if (is.na(s$flagged_pct)) "—" else span(class = if (s$look) "sp-warn-t", sprintf("%.0f %%", s$flagged_pct))), tags$td(s$date_shown)) }))),
      p(class = "sp-dim sp-small", "Originals only: trimmed, merged and processed sets made from a survey are folded into it (Standard, Archive lists them).")) })
  sel_row <- reactive({ k <- cur_s(); x <- x_all(); if (is.null(k)) NULL else x[survey_id == k] })
  output$sp_plot_title <- renderText({ s <- sel_row(); if (is.null(s) || !nrow(s)) "" else paste0(s$name, " · ", s$date_shown) })
  output$sp_pseudo <- renderPlot({ s <- sel_row(); req(s, nrow(s)); sid <- s$survey_id; validate(need(has_meas(sid), "No measurements were read from this survey."))
    d <- read_meas(sid); fl <- read_part(sid, "flags.csv"); if (is.null(fl)) fl <- data.table(meas_id = integer(), flag = character())
    p <- cbind(d[, intersect(c("meas_id", "rhoa_ohmm", "error_pct"), names(d)), with = FALSE], pseudo_xy(d)); p[, flagged := meas_id %in% fl$meas_id]
    er <- identical(input$sp_colour, "error") && "error_pct" %in% names(p)
    p[, v := if (er) error_pct else ifelse(is.finite(rhoa_ohmm) & rhoa_ohmm > 0, log10(rhoa_ohmm), NA_real_)]
    ggplot2::ggplot(p[is.finite(v)], ggplot2::aes(px, pz)) + ggplot2::geom_point(ggplot2::aes(colour = v), size = 2) +
      ggplot2::geom_point(data = p[is.finite(v) & flagged == TRUE], shape = 1, colour = PAL$fg, size = 2.8, stroke = 0.5) +
      (if (er) ggplot2::scale_colour_viridis_c(name = "Error (%)") else
        ggplot2::scale_colour_gradientn(colours = grDevices::hcl.colors(9, "Spectral", rev = TRUE), name = "ρa (Ω·m)", labels = function(b) format(10^b, big.mark = ",", scientific = FALSE, trim = TRUE))) +
      ggplot2::labs(x = "Distance along line (m)", y = "Pseudo-depth (m)") + theme_ovid(PAL) }, bg = PAL$bg)
  output$sp_plot_note <- renderText({ s <- sel_row(); if (is.null(s) || !nrow(s)) return("")
    paste0("Apparent resistivity ρa = K·R, K from the electrode positions (Telford, Geldart & Sheriff 1990; Loke 2004): measured data, not an inversion. ",
           "Ringed: flagged under the QC profile ", s$qc_profile %||% "", " (kept, not removed). Pseudo-depth = 0.19 × array length.") })

  # ---- View, GPR (view only)
  gl <- reactive({ g <- tryCatch(std$gpr_ok(), error = function(e) data.table()); if (!nrow(g)) return(g); g[, day := basename(folder)][] })
  output$sp_gpr_hero <- renderUI({ g <- gl(); if (!nrow(g)) return(simple_hero("off", "No GPR lines scanned", "GPR folders are set on the GPR side of the Standard view (Map)."))
    yrs <- unique(regmatches(g$folder, regexpr("(19|20)\\d{2}", g$folder))); odd <- sum(grepl("(?i)_undated|_incomplete", g$folder, perl = TRUE))
    simple_hero("ok", sprintf("%d GPR lines · %d years", nrow(g), length(yrs)), sprintf("%d with GPS. %s", sum(!is.na(g$cor) & nzchar(g$cor %|na|% "")),
      if (odd) sprintf("%d in undated or incomplete folders.", odd) else "")) })
  # the days or folders, dated ones first and the newest at the top
  observe({ g <- gl(); if (!nrow(g)) return(); d <- unique(g$day); d <- d[order(!grepl("^(19|20)\\d{2}", d), -rank(d))]; k <- isolate(input$sp_gpr_day); updateSelectizeInput(session, "sp_gpr_day", choices = d, selected = if (isTRUE(k %in% d)) k else d[1]) })
  observe({ g <- gl(); k <- input$sp_gpr_day; if (!nrow(g) || !nzchar(k %||% "")) return(); l <- g[day == k]$line; updateSelectizeInput(session, "sp_gpr_line", choices = l, selected = l[1]) })
  output$sp_gpr_map <- plotly::renderPlotly({ g <- gl(); k <- input$sp_gpr_day; req(nrow(g), nzchar(k %||% "")); trk <- std$gpr_tracks(); l <- intersect(g[day == k]$line, names(trk))
    validate(need(length(l) > 0, "No GPS track for the lines of this day.")); gpr_map(trk[l], input$sp_gpr_line %||% "") })
  output$sp_gpr_title <- renderText(paste("Radargram ·", input$sp_gpr_line %||% ""))
  output$sp_gpr_rg <- plotly::renderPlotly({ g <- gl(); k <- input$sp_gpr_line; req(nrow(g), nzchar(k %||% "")); r <- g[line == k][1]; req(!is.na(r$data))
    L <- tryCatch(gpr_read_line(r), error = function(e) NULL); validate(need(!is.null(L) && length(L$m), "The trace data could not be read."))   # pages/fct_gpr_flow.R
    dd <- gpr_decimate(gpr_dc(L$m)); dt <- (L$h$timewindow_ns %||% r$timewindow_ns) / r$samples; view_radargram(dd$m, dt, x = dd$tr) })

  # ---- Edit (ERT)
  sites_tab <- reactive({ std$rv$tick; std$eo_tick(); sp_gs_cfg(pd(), "sites", SITE_COLS) })
  rows <- reactive({ x <- x_all(); if (is.null(x)) return(NULL); st <- sites_tab()
    r <- x[, .(item = survey_id, label = paste0(name, fifelse(nzchar(date_shown) & date_shown != "no date", paste0(" · ", substr(date_shown, 1, 10)), "")), kind = "survey", group = site,
               first = "", last = "")]
    extra <- setdiff(st$site[nzchar(st$site)], r$group)                                      # a site with no survey yet still gets its card
    if (length(extra)) r <- rbind(r, data.table(item = "", label = "", kind = "", group = extra, first = "", last = ""))
    r[, `:=`(lat = suppressWarnings(as.numeric(st$latitude[match(group, st$site)])), lon = suppressWarnings(as.numeric(st$longitude[match(group, st$site)])))]
    r[] })
  touched <- c("sites.csv", "surveys.csv", "transects.csv")
  busy <- reactive({ invalidateLater(3000); k <- ls(OVID_PENDING); if (!length(k)) return(character())
    k <- k[dirname(k) == live_path(file.path(pd(), "config"))]; intersect(unique(basename(k)), touched) })
  ed <- simple_edit_server("spe", rows, apply = function(ops, who) { r <- sp_gs_apply(pd(), ops, who); std$mark(); std$rv$tick <- isolate(std$rv$tick) + 1; r },
    busy = busy, noun = "site", items = "surveys",
    fields = list(list(id = "environment", label = "Environment", type = "text", get = function(k) { st <- sites_tab(); v <- st$environment[st$site == k]; if (length(v)) v[1] else "" })))
  # one survey's details: its name, date, transect and quality, added to the list of changes
  output$sp_survey_card <- renderUI({ x <- x_all(); if (is.null(x)) return(NULL); tr <- sp_gs_cfg(pd(), "transects", TRANSECT_COLS)
    ch <- stats::setNames(x$survey_id, paste0(x$name, " · ", x$folder))
    div(class = "sp-card", div(class = "sp-h", "Survey details"),
      div(class = "sp-row", style = "margin-top:0;align-items:flex-end;flex-wrap:wrap",
        selectizeInput("sp_ed_s", "Survey", ch, selected = isolate(input$sp_ed_s) %||% cur_s(), width = "320px"),
        textInput("sp_ed_name", "Name", width = "200px"), textInput("sp_ed_date", "Date (YYYY-MM-DD)", width = "160px"),
        selectizeInput("sp_ed_tr", "Transect", c("", tr$transect_id[nzchar(tr$transect_id)]), options = list(create = TRUE), width = "180px"),
        selectInput("sp_ed_q", "Quality", c("Not judged" = "", stats::setNames(QUALITY_CLASSES[QUALITY_CLASSES != "Unknown"], QUALITY_CLASSES[QUALITY_CLASSES != "Unknown"])), width = "200px"),
        actionButton("sp_ed_add", "Add to changes", class = "sp-btn")),
      p(class = "sp-dim sp-small", "Which site a survey is at: drag it onto a site below. These go to surveys.csv when you Save; the data files are never changed.")) })
  observeEvent(input$sp_ed_s, { x <- x_all(); s <- x[survey_id == input$sp_ed_s]; if (!nrow(s)) return()
    updateTextInput(session, "sp_ed_name", value = s$name); updateTextInput(session, "sp_ed_date", value = if (s$dated) substr(s$date, 1, 10) else "")
    updateSelectizeInput(session, "sp_ed_tr", selected = s$transect_id); updateSelectInput(session, "sp_ed_q", selected = if (s$quality %in% c("Unknown", "")) "" else s$quality) })
  observeEvent(input$sp_ed_add, { x <- x_all(); s <- x[survey_id == input$sp_ed_s]; req(nrow(s)); k <- sp_gs_key(s$name, s$survey_id)
    d <- trim(input$sp_ed_date %||% ""); if (nzchar(d) && is.na(suppressWarnings(as.Date(d, optional = TRUE))) && !grepl("^(19|20)\\d{2}(-\\d{2})?$", d))
      return(showNotification("A date as YYYY-MM-DD (or YYYY-MM, or the year).", type = "warning"))
    now <- list(display_name = s$name, acquired_date = if (s$dated) substr(s$date, 1, 10) else "", transect_id = s$transect_id, quality = if (s$quality == "Unknown") "" else s$quality)
    new <- list(display_name = trim(input$sp_ed_name %||% ""), acquired_date = d, transect_id = trim(input$sp_ed_tr %||% ""), quality = input$sp_ed_q %||% "")
    ch <- names(new)[!mapply(identical, new, now)]; if (!length(ch)) return(showNotification("Nothing changed for this survey.", type = "message"))
    ed$propose(data.table(type = "field", group = k, item = ch, to = unlist(new[ch]))) })
  # suggestions for review: dates and sites from folder names
  props <- reactive({ x <- x_all(); if (is.null(x)) return(list(dates = NULL, sites = NULL))
    dd <- x[!dated & nzchar(folder_date)]; ss <- x[!nzchar(site)][, s := sp_gs_folder_site(folder)][nzchar(s)]
    list(dates = dd, sites = ss) })
  output$sp_propose <- renderUI({ p0 <- props(); nd <- NROW(p0$dates); ns <- NROW(p0$sites); if (!nd && !ns) return(NULL)
    div(class = "sp-card", div(class = "sp-h", "Suggestions to review"),
      div(class = "sp-row", style = "margin-top:0", if (nd) actionButton("sp_use_dates", sprintf("Folder dates (%d)", nd), class = "sp-btn2"),
          if (ns) actionButton("sp_use_sites", sprintf("Sites from folders (%d)", ns), class = "sp-btn2")),
      p(class = "sp-dim sp-small", "From the folder names: a date for surveys whose files give none, and a site for surveys with none (the folder's name without its date). They are added to the changes below to look over; drop any that are wrong, then Save.")) })
  observeEvent(input$sp_use_dates, { d <- props()$dates; req(NROW(d)); ed$propose(data.table(type = "field", group = sp_gs_key(d$name, d$survey_id), item = "acquired_date", to = d$folder_date)) })
  observeEvent(input$sp_use_sites, { s <- props()$sites; req(NROW(s)); ed$propose(data.table(type = "move", group = "", to = s$s, item = s$survey_id, at = "")) })

  # ---- Export
  table_of <- function(x) { if (is.null(x) || !nrow(x)) return(NULL)
    t <- x[, .(Surveys = .N, Dates = { d <- substr(date[dated], 1, 10); if (length(d)) (if (min(d) == max(d)) min(d) else paste(min(d), "to", max(d))) else "—" },
               Arrays = paste(unique(array_lbl[nzchar(array_lbl)]), collapse = ", "),
               `Electrodes · spacing` = paste(unique(stats::na.omit(ifelse(is.na(n_electrodes), NA, sprintf("%s · %s m", n_electrodes, format(spacing_m))))), collapse = ", "),
               Measurements = sum(as.numeric(n_read), na.rm = TRUE), `% flagged` = round(stats::median(flagged_pct, na.rm = TRUE)),
               Quality = { q <- table(quality); paste(paste(names(q), q), collapse = ", ") }), by = .(Site = fifelse(nzchar(site), site, "No site yet"))]
    t[order(Site == "No site yet", Site)] }
  doc <- function() { x <- x_all(); s <- cat_s(); r <- last_run(); nm <- basename(pd()); if (identical(nm, "project")) nm <- basename(dirname(pd()))
    who <- trim(input$sp_rpt_who %||% ""); d <- substr(x$date[x$dated], 1, 10); nl <- sum(x$look); ns <- sum(!nzchar(x$site))
    simple_report_doc(paste("Catalogue:", nm),
      meta = c(if (length(d)) paste(min(d), "to", max(d)), paste(nrow(x), "surveys"), paste(uniqueN(x$site[nzchar(x$site)]), "sites"), if (!is.null(r)) paste("scanned", substr(as.character(r$started), 1, 10)), if (nzchar(who)) paste("checked by", who)),
      summary = paste0(nrow(x), " original surveys (", sum(!s$kind %in% SP_GS_KINDS), " derived sets folded into them), ", num(sum(as.numeric(x$n_read), na.rm = TRUE)), " measurements. ",
                       if (nl) sprintf("%d surveys have more than %g %% of their measurements flagged under the QC profile %s; flagged values are kept, not removed. ", nl, sp_gs_look(), x$qc_profile[1] %||% "") else "",
                       if (ns) sprintf("%d surveys have no site yet.", ns) else "Every survey has a site."),
      figures = c(Surveys = as.character(nrow(x)), Sites = as.character(uniqueN(x$site[nzchar(x$site)])), Measurements = num(sum(as.numeric(x$n_read), na.rm = TRUE)),
                  `Flagged, median` = sprintf("%.0f %%", stats::median(x$flagged_pct, na.rm = TRUE))),
      notes = c(Surveys = "originals; derived sets folded", Sites = "with a survey", Measurements = "as read", `Flagged, median` = paste("QC profile", x$qc_profile[1] %||% "")),
      table = table_of(x), table_caption = "Sites",
      methods = c(paste0("Files catalogued and read as recorded; copies, merges, subsets and processed sets linked to their originals; every file listed (ovid GS pipeline ", APP_VERSION, ")."),
                  "Apparent resistivity ρa = K·R, with the geometric factor K = 2π / (1/AM − 1/BM − 1/AN + 1/BN) from the electrode positions: Telford, W. M., Geldart, L. P., Sheriff, R. E. (1990) Applied Geophysics, 2nd ed., Ch. 8; Loke, M. H. (2004) Tutorial: 2-D and 3-D electrical imaging surveys. Measured data, not an inversion.",
                  "QC limits (error, current, voltage, resistance, apparent resistivity, duplicates, geometry): the AGI SuperSting and EarthImager manuals; Herring, T., et al. (2023), the CPERS QA/QC workflow. Flags mark measurements; none is removed.",
                  "Dates marked (folder) come from a folder's name, not from the files."),
      record = c(run = as.character(r$run_id %||% ""), started = as.character(r$started %||% ""), files = as.character(r$files %||% ""), pipeline = as.character(r$pipeline_version %||% APP_VERSION),
                 core = as.character(r$core_version %||% CORE_VERSION), `set-up` = as.character(r$config_hash %||% ""), audit = "output/audit: every file and what was read from it"), pal = PAL) }
  output$sp_rpt <- downloadHandler(filename = function() "catalogue_report.html",
    content = function(file) { req(x_all()); withProgress(message = "Making the report", value = 0.5, { b <- tempfile("catalogue"); out <- simple_report_write(doc(), b, "html", PAL); file.copy(out[1], file, overwrite = TRUE) }) })
  output$sp_cat <- downloadHandler(filename = function() "catalogue_tables.zip", contentType = "application/zip",
    content = function(file) { dir <- tempfile("cat"); dir.create(dir); fs <- c(list.files(CAT, "\\.csv$", full.names = TRUE), list.files(file.path(pd(), "config"), "\\.csv$", full.names = TRUE))
      out <- file.path(dir, ifelse(dirname(fs) == CAT, "catalogue", "config")); for (d in unique(out)) dir.create(d, showWarnings = FALSE); file.copy(fs, file.path(out, basename(fs)))
      zip::zip(file, c(file.path("catalogue", basename(fs[dirname(fs) == CAT])), file.path("config", basename(fs[dirname(fs) != CAT]))), root = dir) })
  output$sp_data <- downloadHandler(filename = function() "survey_data.zip", contentType = "application/zip",
    content = function(file) { x <- x_all(); dir <- tempfile("data"); dir.create(dir); fs <- character()
      withProgress(message = "Collecting the surveys", value = 0, for (i in seq_len(nrow(x))) { s <- x[i]; incProgress(1 / nrow(x)); if (!has_meas(s$survey_id)) next
        k <- safe_name(paste0(s$name, "_", sub("^ERT-", "", s$survey_id))); dir.create(file.path(dir, k)); d <- read_meas(s$survey_id); fl <- read_part(s$survey_id, "flags.csv")
        if (!is.null(fl) && nrow(fl)) d[, flags := fcoalesce(fl[, .(f = paste(unique(flag), collapse = ";")), by = meas_id]$f[match(meas_id, unique(fl$meas_id))], "")] else d[, flags := ""]
        fwrite(d, file.path(dir, k, "measurements.csv")); fs <- c(fs, file.path(k, "measurements.csv"))
        src <- unlist(lapply(CFG$raw_dirs, function(r) file.path(r, s$primary_file))); src <- src[file.exists(src)][1]
        if (!is.na(src)) { file.copy(src, file.path(dir, k, basename(src))); fs <- c(fs, file.path(k, basename(src))) } })
      writeLines(c(paste("Survey data from ovid GS, Simple view,", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
        "One folder per original survey: measurements.csv (the common model: electrodes, V/I, R, apparent resistivity, error, current, ...) with flags",
        "(the QC profile's flags; values kept), beside the original file as recorded. Apparent resistivity: Telford et al. (1990); Loke (2004)."), file.path(dir, "README.txt"))
      zip::zip(file, c(fs, "README.txt"), root = dir) })
  output$sp_prev <- downloadHandler(filename = function() "previews.zip", contentType = "application/zip",
    content = function(file) { d <- file.path(CAT, "previews"); f <- list.files(d, "\\.png$"); validate(need(length(f), "No previews were made.")); zip::zip(file, f, root = d) })
}

# A survey in the list of changes: its name and id ("SL2SCH · ERT-a5faacf3c4"), the id read back by sp_gs_apply.
sp_gs_key <- function(name, sid) paste0(name, " · ", sid)
sp_gs_sid <- function(key) sub("^.* · ", "", key)

# Saves the Edit step's changes, in order, each with the version before in config/history and the change log (backup_cfg):
#   rename  through rename.R (RENAMES$site: sites.csv, surveys.csv, transects.csv)
#   move    the survey's site in surveys.csv (a site it names gets a row in sites.csv)
#   place   latitude and longitude in sites.csv; field environment: its environment there
#   field   display_name, acquired_date, transect_id or quality of a survey (group: sp_gs_key) in surveys.csv
sp_gs_apply <- function(pd, ops, who) {
  before <- list(); keep <- function(f) { fp <- file.path(pd, "config", f); if (is.null(before[[f]])) before[[f]] <<- if (file.exists(fp)) copy(read_cfg(fp)) else NA }
  wr <- function(what, d, why) { f <- CONFIG_FILES[[what]]$file; fp <- cfg_path(pd, what); keep(f); dir.create(dirname(fp), showWarnings = FALSE, recursive = TRUE); backup_cfg(fp, d, who, why); fwrite_safe(d, fp) }
  site_row <- function(st, k) { i <- which(st$site == k); if (length(i)) return(list(st = st, i = i[1])); st <- rbind(st, sp_gs_blank(st, site = k)); list(st = st, i = nrow(st)) }
  ensure_site <- function(k) { if (!nzchar(k)) return(); st <- sp_gs_cfg(pd, "sites", SITE_COLS); if (!k %in% st$site) wr("sites", rbind(st, sp_gs_blank(st, site = k, notes = "made in the Simple view")), "site (Simple view)") }
  sv_row <- function(sv, sid) { i <- which(sv$survey_id == sid); if (length(i)) return(list(sv = sv, i = i[1])); sv <- rbind(sv, sp_gs_blank(sv, survey_id = sid)); list(sv = sv, i = nrow(sv)) }
  for (j in seq_len(nrow(ops))) { q <- ops[j]
    if (q$type == "rename") {
      plan <- rename_plan(pd, "site", q$group, q$to); for (f in unique(plan$file)) keep(f)
      if (nrow(plan)) rename_apply(pd, plan, who, "rename (Simple view)")
    } else if (q$type == "move") {
      ensure_site(q$to); x <- sv_row(sp_gs_cfg(pd, "surveys", SURVEY_COLS), q$item); x$sv[x$i, site := q$to]; wr("surveys", x$sv, "site of a survey (Simple view)")
    } else if (q$type == "place") {
      x <- site_row(sp_gs_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, `:=`(latitude = format(q$lat, digits = 9), longitude = format(q$lon, digits = 9))]; wr("sites", x$st, "place (Simple view)")
    } else if (q$type == "field" && identical(q$item, "environment")) {
      x <- site_row(sp_gs_cfg(pd, "sites", SITE_COLS), q$group); x$st[x$i, environment := q$to]; wr("sites", x$st, "environment (Simple view)")
    } else if (q$type == "field" && q$item %in% c("display_name", "acquired_date", "transect_id", "quality")) {
      x <- sv_row(sp_gs_cfg(pd, "surveys", SURVEY_COLS), sp_gs_sid(q$group)); set(x$sv, x$i, q$item, q$to); wr("surveys", x$sv, paste(q$item, "(Simple view)")) } }
  n <- nrow(ops)
  list(ok = TRUE, message = sprintf("Saved %d change%s. They show at once; Scan again to see them in the previews.", n, if (n > 1) "s" else ""),
       undo = function() for (f in names(before)) { fp <- file.path(pd, "config", f); b <- before[[f]]
         if (identical(b, NA)) { if (file.exists(fp)) { e <- read_cfg(fp)[0]; backup_cfg(fp, e, who, "undo (Simple view)"); fwrite_safe(e, fp) } }
         else { backup_cfg(fp, b, who, "undo (Simple view)"); fwrite_safe(b, fp) } })
}
