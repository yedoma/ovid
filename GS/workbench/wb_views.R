# GS/workbench/wb_views.R  --  The Workbench's Panels and Edit data tabs for the survey in view. Panels: panels on one distance
# axis with one cursor (pseudosection, contact per electrode, flagged share per electrode, and the change image when Compare
# with... is on and the two surveys are comparable). Edit data: apparent resistivity along each data level, the contact
# histogram with a movable cutoff, the flags per rule and the QC profiles with live counts. A click or a box picks
# measurements or an electrode; Decide (Flag..., Not a problem...) writes through wb_edit.R as the Data
# tab's corrections do. Sourced by wb_gs.R; the figures are plain functions in pages/fct_workbench.R.

WB_STACK_PANELS <- c(Pseudosection = "pseudo", `Contact per electrode` = "contact", `Flagged per electrode` = "flags", `Change (Compare)` = "change")
# The panels ticked last (app_settings.json, saved as they change); an old config/wb_stack_view.csv is not read.
wb_panels_read <- function(saved) { x <- intersect(as.character(unlist(saved)), WB_STACK_PANELS); if (length(x)) x else c("pseudo", "contact", "flags") }

# ---- UI ---------------------------------------------------------------------------------------------------------------
wb_stack_ui <- function() div(class = "wb-std d-tool",
  div(class = "wb-inline", style = "flex-wrap:wrap;gap:12px;align-items:flex-end",
    checkboxGroupInput("wbs_panels", NULL, WB_STACK_PANELS, selected = c("pseudo", "contact", "flags"), inline = TRUE),
    stack_compare_ui("wbs_compare", "wbs_kept")),                     # the same Compare as Stack (pages/mod_stack.R)
  uiOutput("wbs_note"),
  plotly::plotlyOutput("wbs_plot", height = "640px"),
  p(class = "wb-p", style = "color:var(--wb-fg2)", "One cursor runs through every panel. Click a point for its measurement, or a contact or flag mark for its electrode; drag a box on the pseudosection to pick several (Shift adds); the wheel zooms, double-click shows everything. Raw values; nothing smoothed."))

wb_editdata_ui <- function() div(class = "wb-std d-tool",
  tags$h2(class = "wb-h2", "Profile per data level"),
  selectizeInput("wbe_levels", NULL, choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Every level (pick some to compare)", plugins = list("remove_button"))),
  plotly::plotlyOutput("wbe_profile", height = "360px"),
  p(class = "wb-p", style = "color:var(--wb-fg2)", "Apparent resistivity along each level (one pseudo-depth with the same dipole lengths). Click a point, or box-select a run, then decide in the panel on the right; nothing is removed, only flagged (Res2DInv's exterminate tool, as flags)."),
  tags$h2(class = "wb-h2", "Contact resistance"),
  div(class = "wb-inline", style = "gap:16px;align-items:center;flex-wrap:wrap",
    sliderInput("wbe_cut", "Cutoff (kohm)", min = 0.5, max = 50, value = 5, step = 0.5, width = "320px"),
    uiOutput("wbe_cut_n", inline = TRUE), actionButton("wbe_cut_pick", "Pick these", class = "wb-btn sm")),
  plotly::plotlyOutput("wbe_hist", height = "240px"),
  tags$h2(class = "wb-h2", "Flags per rule"),
  p(class = "wb-p", style = "color:var(--wb-fg2)", "With the set-up as saved. A measurement can be caught by several rules, so the rows add up to more than the total (as EarthImager counts them)."),
  DT::DTOutput("wbe_rules"), div(class = "wb-inline", style = "margin:6px 0 14px", actionButton("wbe_rule_pick", "Select matching", class = "wb-btn sm")),
  tags$h2(class = "wb-h2", "QC profiles"),
  uiOutput("wbe_profiles"),
  p(class = "wb-p", style = "color:var(--wb-fg2)", "Each profile's limits are edited on the QC page (qc_profiles.csv). Shift, reverse and scale of the line are not here yet: they need a scan to apply geometry corrections."))

# ---- server -----------------------------------------------------------------------------------------------------------
# ctx: the environment of wb_server (std, E, key, part, dat, flags_now, cfg_now, prof_now, view, pick).
wb_views_server <- function(input, output, session, ctx) {
  std <- ctx$std; pick <- ctx$pick
  updateCheckboxGroupInput(session, "wbs_panels", selected = wb_panels_read(SETTINGS$wb_panels))
  observeEvent(input$wbs_panels, {                                       # the panel choice is remembered on its own
    SETTINGS$wb_panels <<- as.list(input$wbs_panels %||% character()); settings_save(SETTINGS_FILE, SETTINGS["wb_panels"]) }, ignoreInit = TRUE, ignoreNULL = FALSE)
  flagged <- reactive({ f <- ctx$flags_now(); if (is.null(f) || !nrow(f)) integer() else unique(f$meas_id) })
  ed <- reactive(electrode_dist(ctx$dat()))
  contact <- reactive(electrode_contact(read_part(ctx$key(), "contact.csv"), ed()))
  ef <- reactive(electrode_flags(ctx$dat(), flagged(), ed()))

  # Compare with: surveys with the same electrode layout first; the change only when stack_comparable() says so
  figs <- reactive(survey_figures(std$cat_s(), std$stats(), std$transects()))
  observeEvent(list(figs(), ctx$key()), { ch <- stack_compare_choices(figs(), ctx$key()); if (length(ch) < 2L) return()
    updateSelectInput(session, "wbs_compare", choices = ch) })
  observeEvent(std$cmp_kept(), if (!identical(input$wbs_kept, std$cmp_kept())) updateNumericInput(session, "wbs_kept", value = std$cmp_kept()))   # one setting with Stack's
  observeEvent(input$wbs_kept, if (!identical(input$wbs_kept, std$cmp_kept())) updateNumericInput(session, "stack-cmp_kept", value = input$wbs_kept), ignoreInit = TRUE)
  cmp <- reactive({ s2 <- input$wbs_compare %||% ""; if (!nzchar(s2)) return(NULL); f <- figs()
    r1 <- f[survey_id == ctx$key()]; r2 <- f[survey_id == s2]; if (!nrow(r1) || !nrow(r2)) return(NULL)
    ok <- stack_comparable(r1, r2, stack_min_kept(input$wbs_kept)); list(ok = ok$ok, why = ok$why, ch = if (ok$ok && has_meas(s2)) stack_change(ctx$dat(), read_meas(s2)), n2 = r2$name) })
  output$wbs_note <- renderUI({ c0 <- cmp(); if (is.null(c0)) return(NULL)
    if (!c0$ok) return(p(class = "wb-p wb-warnt", paste0("No change image: ", paste(c0$why, collapse = "; "), ". A change image of surveys like these shows the contacts, not the ground.")))
    p(class = "wb-p", sprintf("Change from this survey to %s: %s quadrupoles both surveys have (matched by A, B, M and N).", c0$n2, num(c0$ch$n_matched %||% nrow(c0$ch$data)))) })
  output$wbs_plot <- plotly::renderPlotly({ d <- ctx$dat(); c0 <- cmp()
    f <- wb_stack_figure(d, flagged(), contact(), ef(), if (!is.null(c0) && c0$ok) c0$ch, input$wbs_panels %||% character())
    validate(need(!is.null(f), "Tick a panel above.")); plotly::event_register(f, "plotly_selected") |> plotly::event_register("plotly_click") })

  # Edit data: levels, contact cutoff, rules, profiles
  lv <- reactive(data_levels(ctx$dat()))
  observeEvent(lv(), { l <- unique(lv()[!is.na(level), .(level_no, level)]); updateSelectizeInput(session, "wbe_levels", choices = stats::setNames(l$level_no, l$level), selected = character()) })
  output$wbe_profile <- plotly::renderPlotly({ x <- lv()[!is.na(level_no) & is.finite(rhoa_ohmm) & rhoa_ohmm > 0]; s <- as.integer(input$wbe_levels %||% character())
    if (length(s)) x <- x[level_no %in% s]; validate(need(nrow(x) > 0, "No apparent resistivity to draw."))
    x[, fl := meas_id %in% flagged()]
    g <- plotly::plot_ly(source = "wbedit")
    for (k in unique(x$level_no)) { z <- x[level_no == k]
      g <- plotly::add_trace(g, data = z, x = ~px, y = ~rhoa_ohmm, customdata = ~meas_id, type = "scatter", mode = "lines+markers", name = z$level[1], legendgroup = z$level[1],
                             text = ~sprintf("#%d · %s ohm-m<br>A%s B%s M%s N%s%s", meas_id, fmtv(rhoa_ohmm), a, b, m, n, ifelse(fl, "<br>flagged", "")), hoverinfo = "text",
                             marker = list(size = 5, symbol = ifelse(z$fl, "x", "circle")), line = list(width = 1)) }
    plotly::layout(g, xaxis = list(title = "distance along the line (m)"), yaxis = list(title = "apparent resistivity (ohm-m)", type = "log"), dragmode = "select",
                   showlegend = length(unique(x$level_no)) <= 12, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = PAL$fg, size = 11)) |>
      plotly::event_register("plotly_selected") |> plotly::event_register("plotly_click") |> plotly::config(displaylogo = FALSE, scrollZoom = TRUE) })
  cut_ids <- reactive(contact_cutoff(ctx$dat(), 1000 * (input$wbe_cut %||% 5)))
  output$wbe_cut_n <- renderUI({ d <- ctx$dat(); if (!any(is.finite(suppressWarnings(as.numeric(d$contact_ohm))))) return(span(class = "wb-p", "This survey has no contact readings per measurement."))
    span(class = "mono", sprintf("%s of %s measurements above %s kohm", num(length(cut_ids())), num(nrow(d)), format(input$wbe_cut %||% 5))) })
  output$wbe_hist <- plotly::renderPlotly({ v <- suppressWarnings(as.numeric(ctx$dat()$contact_ohm)); v <- v[is.finite(v) & v > 0]
    validate(need(length(v) > 0, "No contact readings per measurement in this survey (the contact per electrode is on the Panels tab)."))
    g <- plotly::plot_ly(x = log10(v), type = "histogram", nbinsx = 60, marker = list(color = PAL$fg4), name = "measurements", hovertemplate = "%{y} measurements<extra></extra>")
    sh <- lapply(c(CONTACT_LINES, cutoff = 1000 * (input$wbe_cut %||% 5)), function(z) z)
    lines <- Map(function(z, k) list(type = "line", x0 = log10(z), x1 = log10(z), y0 = 0, y1 = 1, yref = "paper",
                                     line = list(dash = if (k == "cutoff") "solid" else "dot", width = if (k == "cutoff") 2 else 1,
                                                 color = if (k == "cutoff") PAL$accent else PAL$fg4)), sh, names(sh))
    tk <- log_ticks(range(c(log10(v), 3, 4.3)))
    plotly::layout(g, shapes = unname(lines), xaxis = list(title = "contact resistance (ohm)", tickvals = tk$tickvals, ticktext = tk$ticktext), yaxis = list(title = "measurements"),
                   bargap = 0.05, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = PAL$fg, size = 11)) |> plotly::config(displaylogo = FALSE) })
  observeEvent(input$wbe_cut_pick, { ids <- cut_ids(); if (!length(ids)) return(showNotification("No measurement is above the cutoff.", type = "message"))
    pick(list(ids = ids, electrodes = integer(), what = sprintf("contact above %s kohm", format(input$wbe_cut)))) })
  rc <- reactive(rule_counts(ctx$flags_now(), nrow(ctx$dat())))
  output$wbe_rules <- DT::renderDT({ r <- rc()
    DT::datatable(r[, .(Rule = rule_id, Flag = flag, Measurements = n, `%` = round(pct, 1))], rownames = FALSE, selection = "single", class = "compact",
                  options = list(dom = "t", pageLength = 50, ordering = FALSE)) })
  observeEvent(input$wbe_rule_pick, { i <- input$wbe_rules_rows_selected; if (!length(i)) return(showNotification("Pick a rule in the table first.", type = "warning"))
    r <- rc()[i]; f <- ctx$flags_now(); ids <- unique(f[rule == r$rule & (if ("rule_id" %in% names(f)) rule_id == r$rule_id else TRUE)]$meas_id)
    pick(list(ids = ids, electrodes = integer(), what = paste("caught by", r$rule_id))) })
  output$wbe_profiles <- renderUI({ d <- ctx$dat(); s1 <- ctx$part()$s1; cf <- ctx$cfg_now(); prof <- unique(stats::na.omit(cf$qc$profile)); prof <- prof[nzchar(prof)]
    # safe: a QC chip that cannot be worked out is left off
    chips <- lapply(prof, function(p) { f <- tryCatch(qc_survey(d, s1$survey_id, s1$type, cf$qc, p, cf$manual, s1, cf$electrodes), error = function(e) NULL)
      n <- if (is.null(f) || !nrow(f)) 0L else uniqueN(f$meas_id)
      span(class = paste("wb-chip", if (identical(p, ctx$prof_now())) "on"), title = paste(n, "of", nrow(d), "measurements flagged with this profile"),
           sprintf("%s · %s flagged (%s %%)", gsub("_", " ", p), num(n), round(100 * n / max(nrow(d), 1), 1))) })
    div(class = "wb-inline", style = "flex-wrap:wrap;gap:6px", chips, span(class = "wb-p", style = "color:var(--wb-fg2)", paste("The catalogue reports", ctx$prof_now()))) })

  # picks: a click or a box on either tab; cleared when the survey changes
  observeEvent(ctx$key(), pick(NULL))
  take_click <- function(cd) { if (is.null(cd)) return(); cd <- as.character(cd)
    if (startsWith(cd, "e")) { e <- as.integer(sub("^e", "", cd)); d <- ctx$dat(); pick(list(ids = d$meas_id[d$a %in% e | d$b %in% e | d$m %in% e | d$n %in% e], electrodes = e, what = paste("electrode", e))) }
    else pick(list(ids = as.integer(cd), electrodes = integer(), what = paste0("#", cd))) }
  observeEvent(plotly::event_data("plotly_click", source = "wbstack", priority = "event"), take_click(plotly::event_data("plotly_click", source = "wbstack")$customdata[[1]]))
  observeEvent(plotly::event_data("plotly_click", source = "wbedit", priority = "event"), take_click(plotly::event_data("plotly_click", source = "wbedit")$customdata[[1]]))
  take_box <- function(e) { if (is.null(e) || !length(e$customdata)) return(); cd <- as.character(unlist(e$customdata)); ids <- suppressWarnings(as.integer(cd[!startsWith(cd, "e")]))
    ids <- unique(ids[!is.na(ids)]); if (length(ids)) pick(list(ids = ids, electrodes = integer(), what = paste(num(length(ids)), "measurements in the box"))) }
  observeEvent(plotly::event_data("plotly_selected", source = "wbstack"), take_box(plotly::event_data("plotly_selected", source = "wbstack")))
  observeEvent(plotly::event_data("plotly_selected", source = "wbedit"), take_box(plotly::event_data("plotly_selected", source = "wbedit")))
  invisible(ctx)
}

# The right panel for Stack and Edit data: what was picked, its electrodes on the line, its contact and flags, and Decide.
wb_pick_aside <- function(ctx) {
  # safe: nothing picked yet
  p <- ctx$pick(); p0 <- tryCatch(ctx$part(), error = function(e) NULL)
  if (is.null(p0)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("No data yet")), p(class = "wb-p", "Set the folders under Import data and press Scan.")))
  btn <- function(lbl, target, cls = "", title = NULL) tags$button(type = "button", class = paste("wb-btn sm", cls), title = title, onclick = sprintf("$('#%s').click()", target), HTML(lbl))
  decide <- div(class = "wb-hr", span(class = "wb-eb", "Decide"), div(style = "margin-top:8px;display:flex;flex-direction:column;gap:8px",
    div(class = "wb-inline", btn("&#9873; Flag&hellip;", "wb_act_flag", "danger", "Flag with a reason (manual_flags.csv)"), btn("Not a problem&hellip;", "wb_act_unflag", "", "Take it out of its manual flags")),
    p(class = "wb-p", style = "color:var(--wb-fg2)", "Saved to the set-up tables; the next scan applies it. A flag from a QC rule changes with the profile, and electrodes are turned off, on the QC page.")))
  if (is.null(p) || !length(p$ids)) return(div(class = "wb-insp", div(span(class = "wb-eb", "Selection"), tags$h2("Nothing picked"),
    div(class = "sub", p0$label)), p(class = "wb-p", "Click a point, an electrode's contact or flag mark, or draw a box; or use Pick these and Select matching on Edit data.")))
  d <- ctx$dat(); f <- ctx$flags_now(); ed <- electrode_dist(d)
  if (length(p$ids) > 1 || length(p$electrodes)) {
    nf <- if (is.null(f)) 0L else sum(unique(p$ids) %in% f$meas_id)
    return(div(class = "wb-insp", div(span(class = "wb-eb", "Picked"), tags$h2(p$what), div(class = "sub", p0$label)),
      div(class = "wb-facts", wb_fact("measurements", num(length(p$ids))), wb_fact("flagged now", num(nf))),
      if (length(p$electrodes)) { k <- electrode_contact(read_part(ctx$key(), "contact.csv"), ed); k <- if (!is.null(k)) k[electrode %in% p$electrodes]
        div(wb_kv("at", paste(fmtv(ed$dist[match(p$electrodes, ed$electrode)]), "m")), if (!is.null(k) && nrow(k)) wb_kv("median contact", paste(fmtv(k$median_ohm), "ohm"))) },
      decide))
  }
  w <- d[meas_id == p$ids[1]]; if (!nrow(w)) return(NULL); xy <- pseudo_xy(w)
  why <- if (!is.null(f)) f[meas_id == w$meas_id] else data.table()
  near <- ed[which.min(abs(dist - xy$px))]; k <- electrode_contact(read_part(ctx$key(), "contact.csv"), ed); kc <- if (!is.null(k) && nrow(near)) k[electrode == near$electrode]
  lev <- data_levels(d)[meas_id == w$meas_id]
  div(class = "wb-insp",
    div(span(class = "wb-eb", "Measurement"), tags$h2(paste0("#", w$meas_id)), div(class = "sub", p0$label)),
    div(class = "wb-facts", wb_fact("rho_a", fmtv(w$rhoa_ohmm), "ohm-m"), wb_fact("error", fmtv(w$error_pct), "%"),
        wb_fact("at", fmtv(xy$px), "m along"), wb_fact("level", if (nrow(lev) && !is.na(lev$level_no)) lev$level_no else "-", sprintf("%s m deep", fmtv(abs(xy$pz))))),
    div(span(class = "wb-eb", "Electrodes"), HTML(quad_svg(w, ed))),
    if (nrow(near)) wb_kv("nearest electrode", sprintf("%d at %s m%s", near$electrode, fmtv(near$dist), if (!is.null(kc) && nrow(kc)) paste0(", contact ", fmtv(kc$median_ohm), " ohm") else "")),
    div(class = "wb-hr", span(class = "wb-eb", "Flag"), div(style = "margin-top:6px",
      if (nrow(why)) lapply(seq_len(nrow(why)), function(i) wb_step(why$flag[i], paste0(why$rule[i], if (nzchar(why$value[i] %||% "")) paste0(" · ", why$value[i]) else ""), "w"))
      else wb_step("Not flagged", paste("profile", ctx$prof_now(), "and the manual flags")))),
    decide)
}
