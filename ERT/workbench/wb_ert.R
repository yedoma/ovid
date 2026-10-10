# ERT/workbench/wb_ert.R  --  The ERT Workbench (preview): one survey's measurements as a spreadsheet grid, a row per
# measurement, with the survey's pseudosection drawn above it as points, the selected measurements, where they came from and
# what flagged them beside it, and every page of the Standard interface around it.
# Part of Ovid Core // ERT. Sourced by app.R after ert_pipeline.R and core/shell.R.
# A Workbench session runs the Standard server too (app.R, server(embedded = TRUE)) and shows its pages in the same hidden
# tab set, so every Standard process (archive, map, survey, QC, set-up tables, reference, settings, the set-up check,
# rebuilding and the exports) works here through the same code. This file adds the data view; wb_edit.R the corrections made
# from a selection; wb_project.R the Import view, the trees and the tabs.
# The flags shown are those the set-up as saved now gives (qc_survey(), as the next rebuild will), compared with the ones the
# last rebuild wrote, so a saved change shows before it is applied.

source(file.path(CORE_DIR, "workbench.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_edit.R"), local = TRUE)
source(file.path(APP_DIR, "workbench", "wb_project.R"), local = TRUE)
source(file.path(APP_DIR, "pages", "fct_workbench.R"), local = TRUE)      # the figures behind Panels and Edit data
source(file.path(APP_DIR, "workbench", "wb_views.R"), local = TRUE)       # the Panels and Edit data tabs

WB_MAX_ROWS <- 5000L                                   # measurements the grid shows at once


# The grid's columns: the heading, the measurement's column, its decimals (NA: four significant digits), and whether it is
# ticked at first.
WB_COLS <- data.table(
  p      = c("A", "B", "M", "N", "rho_a (ohm-m)", "V/I (ohm)", "I (mA)", "V (mV)", "error (%)", "contact (ohm)", "chargeability (mV/V)",
             "x (m)", "pseudo-depth (m)", "time", "flags"),
  col    = c("a", "b", "m", "n", "rhoa_ohmm", "resistance_ohm", "current_ma", "voltage_mv", "error_pct", "contact_ohm", "chargeability_mv_v",
             "px", "pz", "tod", "flags"),
  digits = c(0, 0, 0, 0, NA, NA, NA, NA, 2, 0, NA, 2, 2, NA, NA),
  first  = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
WB_TEXT <- c("tod", "flags")                           # shown as text

# The columns a flag is about: the value its rule tested, or the electrodes of the quadrupole. A manual flag is about the
# whole measurement; electrode_off and bad_electrode about the electrodes they name.
WB_RULE_COLS <- list(max_error_pct = "error_pct", reciprocal_pct = "resistance_ohm", min_current_ma = "current_ma", min_vp_mv = "voltage_mv",
  min_abs_r_ohm = "resistance_ohm", rhoa_range = "rhoa_ohmm", negative_rhoa = "rhoa_ohmm", outlier_mad = "rhoa_ohmm", moving_median = "rhoa_ohmm",
  max_contact_ohm = "contact_ohm", duplicates = c("a", "b", "m", "n"), geometry = c("a", "b", "m", "n"))

# The surveys the Workbench lists: those whose measurements were read, newest year first, copies and processed sets after
# the originals of their year.
wb_tables <- function(s) {
  if (is.null(s) || !nrow(s)) return(data.table())
  s <- s[n_read > 0][vapply(survey_id, has_meas, NA)]
  if (!nrow(s)) return(data.table())
  s[, .(key = survey_id, label = paste(date, name), year = ifelse(is.na(year), "undated", as.character(year)), kind, kind_lbl,
        n = n_read, pct = pct_retained)][order(year == "undated", -suppressWarnings(as.integer(year)), !kind %in% c("original", "merged"), label)]
}

# A survey's measurements as the grid's rows (read_meas(): elevations applied), with the position of each on the
# pseudosection (px, pz), current and voltage in mA and mV, and the time of day; in measurement order.
wb_load <- function(sid) {
  if (is.null(sid) || !nzchar(sid) || !has_meas(sid)) return(NULL)
  d <- read_meas(sid); if (!nrow(d)) return(NULL)
  d <- cbind(d, pseudo_xy(d))
  d[, `:=`(current_ma = current_a * 1000, voltage_mv = voltage_v * 1000,
           tod = if (inherits(datetime, "POSIXct")) format(datetime, "%H:%M:%S", tz = "UTC") else substr(as.character(datetime), 12, 19))]
  for (k in setdiff(WB_COLS$col, c(names(d), "flags"))) d[, (k) := NA_real_]
  setorder(d, meas_id)[]
}
wb_params <- function(d) WB_COLS[col %in% WB_TEXT | vapply(col, function(k) any(!is.na(d[[k]])), NA), p]

# Flags (qc_survey(), or a survey's flags.csv) as the cells they are about: meas_id, col (a grid column), rule, flag, value
# and rule_id. Every flag is about the flags column too.
wb_flag_cells <- function(f, d) {
  none <- data.table(meas_id = integer(), col = character(), rule = character(), flag = character(), value = character(), rule_id = character())
  if (is.null(f) || !nrow(f)) return(none)
  f <- as.data.table(f); ch <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  f <- data.table(meas_id = as.integer(f$meas_id), rule = ch(f$rule), flag = ch(f$flag), value = ch(f$value),
                  rule_id = if ("rule_id" %in% names(f)) ch(f$rule_id) else ch(f$rule))
  el <- c("a", "b", "m", "n")
  out <- lapply(split(f, by = c("rule", "value")), function(g) { r <- g$rule[1]
    if (r %in% c("electrode_off", "bad_electrode")) {
      e <- if (r == "electrode_off") suppressWarnings(ids_from(sub("^electrode ", "", g$value[1])))
           else suppressWarnings(as.integer(sub("electrode ", "", regmatches(g$value[1], gregexpr("electrode [0-9]+", g$value[1]))[[1]])))
      dd <- d[match(g$meas_id, d$meas_id)]
      x <- rbindlist(lapply(el, function(k) g[!is.na(dd[[k]]) & dd[[k]] %in% e, .(meas_id, col = k, rule, flag, value, rule_id)]))
      if (!nrow(x)) x <- g[, .(col = el), by = .(meas_id, rule, flag, value, rule_id)]
      return(rbind(x, g[, .(meas_id, col = "flags", rule, flag, value, rule_id)]))
    }
    cols <- if (r == "manual") WB_COLS$col else c(WB_RULE_COLS[[r]], "flags")
    g[, .(col = unique(c(cols, "flags"))), by = .(meas_id, rule, flag, value, rule_id)][, .(meas_id, col, rule, flag, value, rule_id)] })
  unique(rbindlist(out, use.names = TRUE))
}

# State of every cell in the grid's rows w (see core/workbench.R): "flag" where a flag of the set-up as saved now is about it
# and the last rebuild wrote one too; "pend" where it is flagged now and the last rebuild did not flag it (a saved change the
# next rebuild applies); "back" where the last rebuild flagged it and the set-up no longer does. now and last are
# wb_flag_cells(). Per column a data.table (state, why, rule, old).
wb_states <- function(w, params, now, last) {
  agg <- function(x) if (!nrow(x)) data.table(meas_id = integer(), col = character(), why = character(), rule = character()) else
    x[, .(why = paste(unique(flag), collapse = ", "), rule = paste(unique(rule_id), collapse = ", ")), by = .(meas_id, col)]
  a <- agg(now); b <- agg(last); id <- w$meas_id
  stats::setNames(lapply(params, function(p) { k <- WB_COLS$col[match(p, WB_COLS$p)]; an <- a[col == k]; bn <- b[col == k]
    i <- match(id, an$meas_id); j <- match(id, bn$meas_id); hn <- !is.na(i); hl <- !is.na(j)
    data.table(state = ifelse(hn & hl, "flag", ifelse(hn, "pend", ifelse(hl, "back", ""))),
               why = ifelse(hn, an$why[i], ifelse(hl, bn$why[j], "")), rule = ifelse(hn, an$rule[i], ifelse(hl, bn$rule[j], "")), old = NA_real_) }), params)
}
# The same for whole measurements (the points of the pseudosection).
wb_row_state <- function(ids, now, last) { hn <- ids %in% now$meas_id; hl <- ids %in% last$meas_id
  ifelse(hn & hl, "flag", ifelse(hn, "pend", ifelse(hl, "back", ""))) }

# A value of column p as the grid shows it.
wb_num <- function(v, p) { dg <- WB_COLS$digits[match(p, WB_COLS$p)]
  if (!is.numeric(v)) { v <- as.character(v); v[is.na(v)] <- ""; return(v) }
  out <- if (is.na(dg)) formatC(signif(v, 4), digits = 4, format = "fg") else formatC(v, format = "f", digits = as.integer(dg))
  out <- sub("^-", "\u2212", trimws(out)); out[is.na(v)] <- ""; out }
# The grid's cells: per column each cell's HTML and class (its state).
wb_cells <- function(w, params, states) {
  out <- lapply(params, function(p) { k <- WB_COLS$col[match(p, WB_COLS$p)]; s <- states[[p]]
    txt <- htmltools::htmlEscape(wb_num(w[[k]], p))
    list(txt = ifelse(s$state == "back", paste0('<span class="x">\u21ba</span>', txt), txt), cls = s$state) })
  list(txt = lapply(out, `[[`, "txt"), cls = lapply(out, `[[`, "cls"))
}
# Per selected column: how many cells, their mean, low and high, and how many are flagged (or will be after a rebuild).
wb_ert_sel_table <- function(cc, g) {
  x <- cc[!p %in% WB_COLS[col %in% WB_TEXT, p], { k <- WB_COLS$col[match(p[1], WB_COLS$p)]; v <- suppressWarnings(as.numeric(g$w[[k]][r])); f <- v[is.finite(v)]
    s <- g$states[[p[1]]]$state[r]; sg <- function(z) if (length(f)) signif(z, 4) else NA_real_
    .(n = .N, mean = sg(mean(f)), min = sg(min(f)), max = sg(max(f)), flagged = sum(s %in% c("flag", "pend"))) }, by = p]
  if (!nrow(x)) return(NULL)
  tags$table(class = "wb-seltbl", tags$tr(tags$th("column"), tags$th("n"), tags$th("mean"), tags$th("min"), tags$th("max"), tags$th(title = "flagged, now or after a scan", "flag")),
    lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(x$p[i]), tags$td(x$n[i]), tags$td(x$mean[i]), tags$td(x$min[i]), tags$td(x$max[i]), tags$td(x$flagged[i]))))
}
# The set-up tables as qc_survey() reads them, from their editors (or from disk before an editor has read its table).
wb_cfg <- function(E) {
  one <- function(k) { d <- E[[k]]$data(); d <- if (is.null(d)) load_configs(CFG)[[k]] else as.data.table(d); ensure_cols(copy(d), cfg_cols(k)) }
  q <- one("qc"); if (!"rule_id" %in% names(q)) q[, rule_id := ""]
  list(qc = q, manual = one("manual"), electrodes = one("electrodes"))
}

# ---- UI ---------------------------------------------------------------------------------------------------------------
# The data view (core/workbench.R) with the pseudosection as points, the colour it is drawn by, and ERT's corrections.
wb_ert_actions <- function() tagList(
  actionButton("wb_act_flag", HTML("&#9873; Flag&hellip; <kbd>R</kbd>"), class = "wb-btn sm", title = "Flag the selected measurements (manual_flags.csv). R or Delete"),
  actionButton("wb_act_unflag", HTML("&#8634; Unflag&hellip;"), class = "wb-btn sm", title = "Take the selected measurements out of their manual flags, and turn back on the electrodes turned off for this survey that they use"))   # electrodes are turned off on the QC page
wb_ert_tools <- function() div(class = "wb-colour", selectInput("wb_colour", NULL, choices = stats::setNames(VAR_INFO$var, paste("colour:", sub(" \\(.*", "", VAR_INFO$label))), width = "250px"))

wb_ui <- function(req) {
  pages <- std_pages(sources = FALSE)
  std_panels <- lapply(Filter(function(x) inherits(x, "ovid_tool"), pages), function(x) tabPanel(x$title, value = x$value, div(class = "wb-std d-tool", x$ui)))
  wb_page("ERT", PAL, project = if (!VIEWER) projects_switch_ui("proj") else textOutput("wb_project", inline = TRUE), standard_href = "?ui=standard", theme = THEME, areas = WB_AREAS,
    head = tagList(tags$style(HTML(paste0(ovid_css(ovid_tokens(PAL)), if (exists("OVID_FILES_CSS")) OVID_FILES_CSS else "",
                                          "\n.wb-colour .form-group{margin:0} .wb-colour .selectize-input{min-height:28px;padding:3px 10px;font-size:12.5px}"))),
                   tags$script(HTML(OVID_SHELL_JS)), tags$script(HTML(wb_keys_js("wb_act_flag")))),
    actions = tagList(
      tags$a(class = "wb-btn outline", href = "#", `data-ovid-go` = "wb-import", title = "Folders, the set-up check and scanning", HTML("&#10515; Import data")),
      actionButton("rebuild", HTML("&#9656; Scan"), class = "wb-btn primary", title = "Save the folders and scan the archive into the catalogue; surveys whose files and set-up are unchanged are reused"),
      downloadButton("wb_export", HTML("&#8599; Export view"), class = "wb-btn", icon = NULL),
      if (file.exists(file.path(APP_DIR, "docs", "ERT_reference_guide.html"))) tags$a(class = "wb-btn", href = "docs/ERT_reference_guide.html", target = "_blank", title = "Reference guide", "?")),
    nav = tagList(
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Surveys"), span(class = "n", textOutput("wb_n_sv", inline = TRUE))), uiOutput("wb_tree")),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Columns")), checkboxGroupInput("wb_params", NULL, choices = character())),
      tags$details(class = "wb-tree", open = NA, tags$summary(span(class = "wb-eb", "Instruments")), group_by_ui("wbgb", wb = TRUE)),   # core grouping.R
      wb_tools_tree(pages, home = "Home"),          # the Standard pages by their Standard names (core workbench.R)
      uiOutput("wb_setup_tree")),
    main = tagList(uiOutput("wb_tabbar", class = "wb-tabs"),
      do.call(tabsetPanel, c(list(id = "nav", type = "hidden", selected = "wb-stack",
        tabPanel("Panels", value = "wb-stack", wb_stack_ui()), tabPanel("Edit data", value = "wb-editdata", wb_editdata_ui()),
        tabPanel("Data", value = "wb-data", wb_data_ui(wb_ert_actions(), tools = wb_ert_tools(), xy = TRUE)), tabPanel("Import data", value = "wb-import", wb_import_ui())), std_panels))),
    aside = uiOutput("wb_inspector"),
    status = uiOutput("wb_status", style = "display:contents"))
}

# ---- server -----------------------------------------------------------------------------------------------------------
# std: what server(embedded = TRUE) returns: the set-up table editors (E), the rebuild state (rv: tick, log), whether the
# set-up changed since the last rebuild (stale), the catalogue (cat_s), the set-up check (chk) and the project (proj).
wb_server <- function(input, output, session, std) {
  q <- wb_query(isolate(session$clientData$url_search))
  ctx <- environment()                                          # what wb_edit.R and wb_project.R share
  E <- std$E
  prof_now <- function() std$cat_profile()          # the profile the catalogue reports (Settings; pages/set_up_check.R)
  view <- reactive(input$nav %||% "wb-data")
  set_view <- function(v) updateTabsetPanel(session, "nav", selected = v)
  open_table <- function(id) session$sendCustomMessage("wb-open", WB_OPEN[[id]])
  output$wb_project <- renderText({ std$proj(); paste0(basename(PROJECT), if (is_imported()) " (imported)" else "") })

  tables <- reactive({ std$proj(); wb_tables(std$cat_s()) })
  key <- reactiveVal(NULL)
  observeEvent(tables(), { k <- isolate(key()); s <- tables()
    if (is.null(k) || !k %in% s$key) key(if (!is.null(q$survey) && q$survey %in% s$key && is.null(k)) q$survey else if (nrow(s)) s[kind %in% c("original", "merged")]$key[1] %|% s$key[1] else NULL) })
  observeEvent(input$wb_table, { key(input$wb_table); if (!view() %in% c("wb-stack", "wb-editdata")) set_view("wb-data") })
  group_by_server("wbgb", reactive({ x <- std$cat_s(); if (is.null(x) || !nrow(x)) NULL else x[, .(serial = or_blank(serial, paste(or_blank(model, "instrument"), "(no serial)")), file = survey_id, first = datetime_start, last = datetime_end)] }),
    reactive({ std$proj(); PROJECT }), open = function(r) { k <- strsplit(r$files, "; ")[[1]][1]; if (k %in% tables()$key) { key(k); set_view("wb-data") } else showNotification("That survey has no measurements to show.", type = "message") })   # core grouping.R
  observeEvent(key(), { updateSelectInput(session, "survey", selected = key()); updateSelectInput(session, "qc_survey", selected = key()) })   # the Survey and QC pages show it too
  part <- reactive({ k <- key(); req(k); s1 <- as.list(std$cat_s()[survey_id == k][1]); list(sid = k, s1 = s1, label = paste(s1$date, s1$name)) })
  dat <- reactive({ std$rv$tick; std$proj(); ELEV(); d <- wb_load(key()); validate(need(!is.null(d) && nrow(d) > 0, "No measurements for this survey.")); d })
  params_all <- reactive(wb_params(dat()))
  first_load <- reactiveVal(TRUE)
  observeEvent(params_all(), { pa <- params_all(); keep <- intersect(isolate(input$wb_params), pa)
    want <- if (isTRUE(first_load()) && !is.null(q$params)) intersect(strsplit(q$params, ",")[[1]], pa) else keep
    if (!length(want)) want <- intersect(WB_COLS[first == TRUE, p], pa)
    updateCheckboxGroupInput(session, "wb_params", choices = stats::setNames(pa, pa), selected = want)
    cv <- VAR_INFO$var[vapply(VAR_INFO$var, function(k) any(is.finite(dat()[[k]])), NA)]
    updateSelectInput(session, "wb_colour", choices = stats::setNames(cv, paste("colour:", sub(" \\(.*", "", VAR_INFO$label[match(cv, VAR_INFO$var)]))),
                      selected = if (isTRUE(isolate(input$wb_colour) %in% cv)) isolate(input$wb_colour) else cv[1])
    first_load(FALSE) })
  sel_params <- reactive(intersect(WB_COLS$p, intersect(input$wb_params, params_all())))

  # flags: as the set-up saved now gives them (what the next rebuild writes), and as the last rebuild wrote them
  cfg_now <- reactive(wb_cfg(E))
  flags_now <- reactive({ d <- dat(); s1 <- part()$s1; cf <- cfg_now(); qc_survey(d, s1$survey_id, s1$type, cf$qc, prof_now(), cf$manual, s1, cf$electrodes) })
  flags_last <- reactive({ std$rv$tick; f <- file.path(PROJECT, "output", "surveys", key(), "flags.csv"); if (!file.exists(f)) return(NULL)
    x <- fread(f, colClasses = "character", na.strings = NULL); if (!nrow(x)) return(NULL); x[profile == prof_now()][, meas_id := as.integer(meas_id)][] })
  now <- reactive(wb_flag_cells(flags_now(), dat()))
  last <- reactive(wb_flag_cells(flags_last(), dat()))
  flag_text <- function(ids, f) { if (is.null(f) || !nrow(f)) return(rep("", length(ids))); x <- f[, .(t = paste(unique(flag), collapse = ", ")), by = meas_id]
    out <- x$t[match(ids, x$meas_id)]; out[is.na(out)] <- ""; out }
  grid <- reactive({ d <- dat(); ps <- sel_params(); validate(need(length(ps) > 0, "Tick a column on the left."))
    n <- nrow(d); w <- copy(head(d, WB_MAX_ROWS)); fn <- flags_now()
    w[, flags := flag_text(meas_id, fn)]; b <- !nzchar(w$flags); if (any(b)) w[b, flags := flag_text(meas_id, flags_last())]   # a flag the set-up no longer gives shows until the rebuild
    st <- wb_states(w, ps, now(), last())
    list(w = w, n = n, params = ps, states = st, cells = wb_cells(w, ps, st), sig = paste(key(), paste(ps, collapse = ","), n, sep = "|")) })

  # tree
  output$wb_n_sv <- renderText({ s <- tables(); if (nrow(s)) nrow(s) else "" })
  output$wb_tree <- renderUI({ s <- tables(); validate(need(nrow(s), "No catalogue yet. Set the folders under Import data and press Scan."))
    k <- key(); on_data <- identical(view(), "wb-data")
    tagList(lapply(unique(s$year), function(y) { x <- s[year == y]
      tagList(div(class = "wb-subsec", y), lapply(seq_len(nrow(x)), function(i) wb_row(x$label[i], key = x$key[i], input = "wb_table", n = format(x$n[i], big.mark = ","),
        on = identical(x$key[i], k) && on_data, level = if (x$kind[i] %in% c("original", "merged")) 0 else 1,
        title = paste0(x$label[i], " \u00b7 ", x$kind_lbl[i], if (!is.na(x$pct[i])) paste0(" \u00b7 ", round(x$pct[i]), "% kept") else "")))) })) })
  output$wb_chips <- renderUI({ p0 <- tryCatch(part(), error = function(e) NULL); if (is.null(p0)) return(NULL)
    tagList(span(class = "wb-chip", paste("profile", prof_now())), if (nzchar(p0$s1$array_lbl %|% "")) span(class = "wb-chip", p0$s1$array_lbl)) })

  # grid and selection
  output$wb_grid <- renderUI({ g <- grid()
    wb_grid(g$w$meas_id, g$params, g$cells$txt, g$cells$cls, g$sig, corner = "#", times = g$w$meas_id) })
  output$wb_ginfo <- renderText({ g <- grid(); cc <- cells()
    paste0(if (g$n > nrow(g$w)) sprintf("first %s of %s measurements", num(nrow(g$w)), num(g$n)) else paste(num(g$n), "measurements"),
           if (nrow(cc)) paste0(" \u00b7 ", num(uniqueN(cc$r)), " selected") else "",
           " \u00b7 click, drag, shift-click; a number selects the measurement, a heading the column; cmd/ctrl adds; Esc clears") })
  cells <- reactive(wb_sel_cells(input$wb_grid_sel, tryCatch(grid(), error = function(e) NULL)))
  active <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); s <- input$wb_grid_sel
    a <- cc[r == as.integer(s$ar %||% 0) & c == as.integer(s$ac %||% 0)]; if (nrow(a)) a[1] else cc[1] })
  # what the corrections act on: the selected measurements, and the electrodes in the selected A, B, M and N cells (else the
  # electrodes every selected measurement uses)
  sel <- reactive({ cc <- cells(); if (!nrow(cc)) return(NULL); w <- grid()$w; ids <- sort(unique(w$meas_id[cc$r]))
    k <- WB_COLS$col[match(cc$p, WB_COLS$p)]; ec <- cc[k %in% c("a", "b", "m", "n")]
    el <- if (nrow(ec)) unique(vapply(seq_len(nrow(ec)), function(i) as.integer(w[[WB_COLS$col[match(ec$p[i], WB_COLS$p)]]][ec$r[i]]), 0L))
          else { x <- lapply(unique(cc$r), function(r) unlist(w[r, .(a, b, m, n)])); Reduce(intersect, x) }
    list(ids = ids, rows = unique(cc$r), electrodes = sort(unique(el[!is.na(el) & el != 0]))) })
  clear_sel_grid <- function() session$sendCustomMessage("wb-grid-clear", list())
  # on Stack and Edit data the corrections act on what was picked there (a click, a box, Pick these, Select matching)
  pick <- reactiveVal(NULL); sel_grid <- sel
  sel <- reactive(if (view() %in% c("wb-stack", "wb-editdata")) { p <- pick(); if (is.null(p)) NULL else list(ids = sort(unique(p$ids)), rows = integer(), electrodes = p$electrodes) } else sel_grid())
  clear_sel <- function() if (isolate(view()) %in% c("wb-stack", "wb-editdata")) pick(NULL) else clear_sel_grid()
  cell_info <- function(r, p) { g <- grid(); w <- g$w; s <- g$states[[p]]; k <- WB_COLS$col[match(p, WB_COLS$p)]
    list(id = w$meas_id[r], txt = wb_num(w[[k]][r], p), state = s$state[r], why = s$why[r], rule = s$rule[r],
         quad = sprintf("A%s B%s M%s N%s", w$a[r], w$b[r], w$m[r], w$n[r])) }
  state_chip <- function(ci) wb_state_chip(ci$state, ci$why, pend = "flag saved \u00b7 scan to apply")

  # the pseudosection as points (WB_XY_JS): coloured as Colour by says, on the Survey page's colours, flagged points ringed
  observe({ d <- dat(); v0 <- input$wb_colour %||% "rhoa_ohmm"; v <- if (v0 %in% names(d)) v0 else "rhoa_ohmm"
    cv <- suppressWarnings(to_log(d[[v]], v)); lim <- if (v == "rhoa_ohmm") RHOA_LIM else suppressWarnings(stats::quantile(cv, c(0.02, 0.98), na.rm = TRUE, names = FALSE))
    if (!all(is.finite(lim))) lim <- c(0, 1); if (diff(lim) == 0) lim <- lim + c(-0.5, 0.5); tk <- log_ticks(lim)
    pal <- PALETTES[[input$v_pal %||% "Spectral"]] %||% PALETTES[[1]]; lab <- VAR_INFO$label[match(v, VAR_INFO$var)]
    session$sendCustomMessage("wb-xy", wb_xy_message(d$px, d$pz, d$meas_id, cv, d[[v]], wb_row_state(d$meas_id, now(), last()),
      lab = sprintf("A%s B%s M%s N%s", d$a, d$b, d$m, d$n), key = key(), xlab = "distance along the line (m)", ylab = "pseudo-depth (m)",
      clab = lab, vlab = sub(" \\(.*", "", lab), clim = lim, cticks = tk$tickvals, clabs = tk$ticktext, pal = pal)) })

  # cell bar, inspector, status
  output$wb_cellinfo <- renderUI({ cc <- cells()
    if (!nrow(cc)) return(span(class = "txt", "Select cells, measurements (a number) or columns (a heading), or click or drag on the pseudosection; then flag them. Electrodes are turned off on the QC page."))
    a <- active(); ci <- cell_info(a$r, a$p)
    tagList(span(class = "mono", style = paste0("color:", PAL$fg4), paste0(LETTERS[min(a$c + 1, 26)], a$r)), span(class = "mono", paste0("#", ci$id, " \u00b7 ", a$p)),
            span(class = "mono", style = "font-size:14px", if (nzchar(ci$txt)) ci$txt else "\u2014"), state_chip(ci),
            span(class = "txt", if (uniqueN(cc$r) > 1) paste(num(uniqueN(cc$r)), "measurements selected") else if (nzchar(ci$why)) ci$why else ci$quad)) })
  btn <- function(lbl, target, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  correct_btns <- function(state = "") tagList(
    div(class = "wb-inline", if (state %in% c("flag", "pend")) btn("&#8634; Unflag&hellip;", "wb_act_unflag") else btn("&#9873; Flag&hellip;", "wb_act_flag", "danger"),
        if (state %in% c("flag", "pend")) btn("&#9873; Flag&hellip;", "wb_act_flag")),
    div(class = "wb-inline", actionLink("wb_qc_open", "Open the QC page for this survey"), actionLink("wb_flags_open", "Open its manual flags"), actionLink("wb_details_open", "Its details")))
  survey_box <- function() { s <- part()$s1; if (is.null(s$survey_id)) return(NULL)
    div(class = "wb-hr", span(class = "wb-eb", "Survey"), div(style = "margin-top:6px",
      wb_kv("instrument", s$model_lbl %|% "-"), wb_kv("array", s$array_lbl %|% "-"),
      wb_kv("electrodes", if (!is.na(s$n_electrodes %||% NA)) sprintf("%d at %g m", as.integer(s$n_electrodes), s$spacing_m) else "-"),
      wb_kv("measurements", num(s$n_read)), wb_kv("kept %", if (is.na(s$pct_retained %||% NA)) "-" else paste0(round(s$pct_retained, 1), "% (last scan)")),
      if (nzchar(s$transect_id %|% "")) wb_kv("transect", s$transect_id), if (nzchar(s$site %|% "")) wb_kv("site", s$site),
      wb_kv("quality", s$quality %|% "Unknown"), wb_kv("survey id", s$survey_id))) }
  rule_name <- function(r) { n <- RULE_HELP$name[match(r, RULE_HELP$rule)]; ifelse(is.na(n), r, n) }
  qc_steps <- function(id, state) { f <- flags_now(); f <- if (!is.null(f)) f[meas_id == id] else data.table()
    l <- flags_last(); l <- if (!is.null(l)) l[meas_id == id] else data.table()
    st <- lapply(seq_len(nrow(f)), function(i) wb_step(paste("Flagged:", f$flag[i]), paste0(rule_name(f$rule[i]), " \u00b7 ", f$rule_id[i] %|% f$rule[i],
      if (nzchar(f$value[i] %|% "")) paste0(" \u00b7 ", if (f$rule[i] == "manual") "reason " else "", f$value[i]) else ""), if (state == "pend") "e" else "w"))
    if (nrow(f) && state == "pend") st <- c(st, list(wb_step("Not flagged in the last scan", "a saved change of the set-up flags it; a scan applies it", "w")))
    if (!nrow(f) && nrow(l)) st <- list(wb_step(paste("Flagged in the last scan:", paste(unique(l$flag), collapse = ", ")), "the set-up as saved no longer flags it; a scan brings it back", "w"))
    if (!length(st)) st <- list(wb_step(paste("No rule flagged it"), paste("profile", prof_now(), "and the manual flags")))
    st }

  output$wb_inspector <- renderUI({ v <- view()
    if (identical(v, "wb-import")) return(wb_import_aside())
    if (v %in% c("wb-stack", "wb-editdata")) return(wb_pick_aside(ctx))
    if (identical(v, "GPR processing")) return(mod_gpr_flow_aside_ui("gprflow"))   # the A-scan and map of GPR processing (pages/mod_gpr_flow.R)
    if (!identical(v, "wb-data")) return(NULL)
    cc <- cells(); p0 <- tryCatch(part(), error = function(e) NULL)
    if (is.null(p0)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet"), wb_empty_acts(NULL)), p(class = "wb-p", "Set the folders under Import data and press Scan.")))
    if (!nrow(cc)) {
      g <- tryCatch(grid(), error = function(e) NULL); d <- tryCatch(dat(), error = function(e) NULL)
      rs <- if (!is.null(d)) wb_row_state(d$meas_id, now(), last()) else character()
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No measurement selected"),
        div(class = "sub", "Click or drag in the grid; a number selects the measurement, a heading the column. On the pseudosection, click a point or drag a box.")),
        if (length(rs)) div(class = "wb-facts", wb_fact("measurements", num(length(rs)), p0$label), wb_fact("kept %", paste0(round(100 * mean(!rs %in% c("flag", "pend")), 1), "%"), "with the set-up as saved"),
                            wb_fact("flagged", num(sum(rs %in% c("flag", "pend"))), "now"), wb_fact("pending", num(sum(rs %in% c("pend", "back"))), "saved, not scanned")),
        survey_box()))
    }
    if (uniqueN(cc$r) > 1) {
      g <- grid()
      return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2(paste(num(uniqueN(cc$r)), "measurements")),
          div(class = "sub", paste0("#", paste(range(g$w$meas_id[cc$r]), collapse = " \u2013 #"), " \u00b7 ", num(nrow(cc)), " cells"))),
        wb_ert_sel_table(cc, g),
        div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px", correct_btns())),
        survey_box()))
    }
    a <- active(); ci <- cell_info(a$r, a$p); w <- grid()$w; r <- a$r; s1 <- p0$s1
    div(class = "wb-insp",
      div(span(class = "wb-eb", "Selected measurement"), tags$h2(paste0("#", ci$id, " \u00b7 ", a$p)), div(class = "sub", paste(p0$label, if (nzchar(w$tod[r] %|% "")) paste("\u00b7", w$tod[r]) else ""))),
      div(style = "display:flex;align-items:baseline;gap:10px;flex-wrap:wrap", span(style = "font-family:var(--wb-mono);font-size:22px", if (nzchar(ci$txt)) ci$txt else "\u2014"), state_chip(ci)),
      div(class = "wb-facts", wb_fact("rho_a", wb_num(w$rhoa_ohmm[r], "rho_a (ohm-m)"), "ohm-m"), wb_fact("V/I", wb_num(w$resistance_ohm[r], "V/I (ohm)"), "ohm"),
          wb_fact("error", wb_num(w$error_pct[r], "error (%)"), "%"), wb_fact("current", wb_num(w$current_ma[r], "I (mA)"), "mA")),
      div(span(class = "wb-eb", "Lineage"), div(style = "margin-top:6px",
        wb_step(paste("Read from", basename(sub("!/.*", "", s1$primary_file %|% ""))), paste0(s1$format_lbl %|% "", " \u00b7 ", s1$primary_file %|% "")),
        wb_step(ci$quad, paste0(nice_array(w$array[r] %||% ""), " \u00b7 geometric factor ", fmtv(w$geom_factor_m[r] %||% NA), " m")),
        wb_step(sprintf("At %s m along the line", fmtv(w$px[r])), sprintf("pseudo-depth %s m (0.19 times the array's length)", fmtv(abs(w$pz[r])))),
        qc_steps(ci$id, ci$state))),
      div(class = "wb-hr", span(class = "wb-eb", "Correct"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px",
        p(class = "wb-p", switch(ci$state %||% "",
          flag = "Flagged in the last scan, and by the set-up as saved. A flag by hand is undone with Unflag; one from a QC rule when the profile is changed on the QC page.",
          pend = "The set-up as saved flags it; the last scan did not. A scan applies it.",
          back = "Flagged in the last scan; the set-up as saved no longer flags it. A scan brings it back.",
          "Passes QC.")),
        correct_btns(ci$state))),
      survey_box())
  })

  wb_import_aside <- function() { m <- tryCatch(fread(file.path(PROJECT, "output", "audit", "run_manifest.csv"), colClasses = "character"), error = function(e) NULL)
    v <- function(k) if (!is.null(m) && nrow(m)) m$value[match(k, m$key)] else NA
    steps <- c(Files = "every file under the archive folders, by what it contains; zip files opened, never changed", Surveys = "originals, copies, subsets, merged files and processed sets, linked",
               Measurements = "AGI .stg (with .crs and .cmd), IRIS Prosys exports and Res2DInv .dat in one measurement model", QC = "the profiles' rules, the manual flags and the electrodes turned off; flags only, nothing removed",
               Catalogue = "surveys.csv, one folder per survey (measurements, flags, electrodes) and pseudosection previews", Audit = "every file and what became of it")
    div(class = "wb-insp", div(span(class = "wb-eb", "Pipeline"), div(style = "margin-top:8px",
        lapply(seq_along(steps), function(i) div(class = "wb-pstep", span(class = "n", i), div(div(names(steps)[i]), div(class = "s", steps[[i]])))))),
      div(class = "wb-hr", span(class = "wb-eb", "Last scan"), div(style = "margin-top:6px",
        if (is.null(m)) p(class = "wb-p", "Not scanned yet.") else tagList(wb_kv("when", format(as.POSIXct(run_mtime(PROJECT), origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
          wb_kv("files", v("files") %|% "?"), wb_kv("surveys", v("surveys") %|% "?"), wb_kv("measurements", v("measurements") %|% "?"),
          if (!is.na(v("surveys_reused"))) wb_kv("surveys reused", paste(v("surveys_reused"), "of", v("surveys_checked") %|% "?"))))),
      if (length(std$rv$log)) div(class = "wb-hr", span(class = "wb-eb", "Log of this scan"), tags$pre(class = "wb-log", paste(tail(std$rv$log, 40), collapse = "\n"))),
      p(class = "wb-p", style = "color:var(--wb-fg2)", "QC only adds flags; measurements and raw files are never changed.")) }

  output$wb_status <- renderUI({ std$rv$tick; cc <- cells(); g <- tryCatch(grid(), error = function(e) NULL)
    pf <- file.path(PROJECT, "output", "audit", "problems.csv"); np <- if (file.exists(pf)) nrow(tryCatch(fread(pf), error = function(e) data.table())) else 0L
    mid <- if (!identical(view(), "wb-data") || is.null(g)) NULL else if (nrow(cc)) span(paste(num(uniqueN(cc$r)), "measurements selected")) else span(paste(num(g$n), "measurements"))
    tagList(span(style = paste0("color:", PAL$fg), if (np) tags$a(href = "#", `data-ovid-go` = "Settings", style = "color:inherit", paste(np, "problems")) else "No problems"),   # a number, no light
            if (isTRUE(std$stale())) tags$a(class = "wb-stale", href = "#", `data-ovid-go` = "wb-import", HTML("&#9679; Set-up changed since the last scan")),
            div(class = "wb-fill mono", style = "display:flex;justify-content:center;gap:16px", mid),
            span(class = "mono", paste0("profile ", prof_now(), " \u00b7 ERT ", ERT_VERSION, " \u00b7 workbench ", OVID_WORKBENCH_VERSION))) })
  output$wb_export <- downloadHandler(function() paste0("ert_", safe_name(part()$label), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    function(file) { g <- grid(); w <- g$w; ks <- WB_COLS$col[match(g$params, WB_COLS$p)]
      x <- w[, c("meas_id", setdiff(ks, "meas_id")), with = FALSE]; setnames(x, c("meas_id", g$params[ks != "meas_id"])); fwrite(x, file) })

  wb_project_server(input, output, session, ctx)
  wb_edit_server(input, output, session, ctx)
  wb_views_server(input, output, session, ctx)
  invisible(ctx)
}
