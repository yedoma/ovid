# GS/pages/mod_gpr_flow.R  --  GPR processing as a Shiny module, the one screen for both interfaces (REFLEXW-style): the
# processing flow on the left (ordered steps, each with a switch, a one-line summary and its settings folded under it; Up / Down
# to reorder; presets; saved with the line as versions; Undo; Before / after; Apply to all lines), the velocities and the display
# (across, topography, markers, colours, deepest depth), the radargram in the centre (field markers, picks, a cursor; a click
# picks or adds a marker), picks and markers under it, and the A-scan at the cursor with its position and a small map (beside the
# radargram in Standard, in the Workbench's inspector). Raw files are never written: flows, picks and markers go to the project's
# config (pages/fct_gpr_flow.R).

GPR_WB_JS <- "Shiny.addCustomMessageHandler('gpr-wb-order', function(m) { m.order.forEach(function(s, i) { var e = document.getElementById(m.id + s); if (e) e.style.order = i; }); });"
GPR_WB_CSS <- ".gpr-flow{display:flex;flex-direction:column;gap:4px} .gpr-step{border:1px solid var(--wb-ln, #333);padding:4px 6px}
.gpr-step-h{display:flex;align-items:center;gap:6px} .gpr-step-h .form-group,.gpr-step-h .checkbox{margin:0} .gpr-step-n{font-weight:600;white-space:nowrap}
.gpr-step-s{color:var(--wb-fg2, #999);font-size:11.5px;flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap} .gpr-step details summary{font-size:11.5px;color:var(--wb-fg2, #999)}
.gpr-mv{text-decoration:none;padding:0 2px} .gpr-wb .form-group{margin-bottom:6px}"

# wb: TRUE in the Workbench (its button styles; the A-scan and map go to its inspector, mod_gpr_flow_aside_ui()).
mod_gpr_flow_ui <- function(id, wb = FALSE) {
  ns <- NS(id); d <- GPR_PRESETS[[1]]
  b <- if (wb) c(btn = "wb-btn", primary = "wb-btn primary") else c(btn = "btn btn-sm btn-outline-secondary", primary = "btn btn-sm btn-primary")
  num <- function(k, label, value, step, width = "90px", min = 0) numericInput(ns(k), label, value, min = min, step = step, width = width)
  set <- list(
    dc = p(class = "muted", "On: the mean of the last quarter of each trace is taken off (GroundVision DC filter). Off: the mean of each whole trace is taken off."),
    dewow = num("dewow", "Window (periods of the antenna frequency)", if (isTRUE(d$dewow > 0)) d$dewow else 1, 0.5, "200px"),
    t0 = tagList(radioButtons(ns("t0"), NULL, c(`First zero crossing` = "auto", `Set` = "manual"), selected = if (identical(d$t0, "manual")) "manual" else "auto", inline = TRUE),
                 num("t0ns", "Time zero (ns)", 0, 0.5, "130px")),
    bp = tagList(p(class = "muted mb-1", "Ormsby corners, times the antenna frequency."),
                 div(class = "d-flex gap-1", num("bp1", "f1", 0.25, 0.05, "64px"), num("bp2", "f2", 1, 0.05, "64px"), num("bp3", "f3", 2, 0.05, "64px"), num("bp4", "f4", 4, 0.05, "64px"))),
    gain = tagList(selectInput(ns("gain"), NULL, c(`Energy decay (no parameters)` = "decay", `Gain function (1 + a t) exp(b t)` = "fn", AGC = "agc"),
                               selected = if (d$gain %in% c("decay", "fn", "agc")) d$gain else "decay", width = "230px"),
                   div(class = "d-flex gap-1", num("ga", "a'", 0, 0.5, "70px"), num("gb", "b' (dB/m)", 0, 1, "80px"), num("agcns", "AGC (ns)", 25, 5, "80px", 1)),
                   p(class = "muted", "Energy decay and AGC remove true amplitudes.")),
    bg = tagList(selectInput(ns("bg"), NULL, c(`Mean trace of the line` = "mean", `Moving mean of N traces` = "moving"), selected = if (identical(d$bg, "moving")) "moving" else "mean", width = "230px"),
                 num("bgn", "N traces", 50, 5, "90px", 5),
                 p(class = "muted", "Takes off every flat, continuous reflector, which is what a frost table looks like (GroundVision manual, REFLEXW guide II.4); use the moving mean, or none, when the frost table is the target.")))
  on <- c(dc = isTRUE(d$dc), dewow = isTRUE(d$dewow > 0), t0 = !identical(d$t0, "none"), bp = isTRUE(d$bp), gain = !identical(d$gain, "none"), bg = !identical(d$bg, "none"))
  step <- function(k, i) div(id = ns(paste0("row_", k)), class = "gpr-step", style = sprintf("order:%d", i),
    div(class = "gpr-step-h", ovid_switch(ns(paste0("on_", k)), NULL, on[[k]]), span(class = "gpr-step-n", GPR_STEPS[[k]]),
        span(class = "gpr-step-s", textOutput(ns(paste0("sum_", k)), inline = TRUE)),
        actionLink(ns(paste0("up_", k)), HTML("&#9650;"), class = "gpr-mv", title = "Run earlier"), actionLink(ns(paste0("dn_", k)), HTML("&#9660;"), class = "gpr-mv", title = "Run later")),
    tags$details(tags$summary("settings"), set[[k]]))
  div(class = "gpr-wb", tags$style(HTML(GPR_WB_CSS)), tags$script(HTML(GPR_WB_JS)),
    div(class = "d-flex gap-2 align-items-end flex-wrap",
      selectInput(ns("line"), "Line", choices = NULL, width = "260px"),
      selectInput(ns("ver"), "Saved flow", choices = c(`None saved` = ""), width = "220px"),
      actionButton(ns("save"), "Save flow with this line", class = paste(b[["primary"]], "mb-2")),
      actionButton(ns("undo"), "Undo changes", class = paste(b[["btn"]], "mb-2"), title = "Back to the saved version picked"),
      actionButton(ns("all"), HTML("Apply to all lines&hellip;"), class = paste(b[["btn"]], "mb-2")),
      ovid_switch(ns("before"), "Before / after", FALSE),
      radioButtons(ns("v"), "Down", c(`Depth (m)` = "depth", `Time (ns)` = "time"), inline = TRUE),
      radioButtons(ns("tool"), "A click", c(Cursor = "cursor", Pick = "pick", `Add marker` = "marker"), inline = TRUE)),
    uiOutput(ns("head")),
    div(style = sprintf("display:grid;grid-template-columns:minmax(260px,320px) 1fr%s;gap:12px;align-items:start", if (wb) "" else " minmax(220px,280px)"),
      div(
        div(class = "d-flex gap-2 align-items-end", span(class = "wb-eb", "Processing flow"),
            selectInput(ns("preset"), NULL, c(`Preset...` = "", names(GPR_PRESETS)), width = "200px")),
        p(class = "muted", "Steps run top to bottom on a copy of the traces; switch a step off, move it with the arrows, open its settings. The flow belongs to the line: saved in the project (config/gpr_flows), reloaded when the line is opened, kept as versions."),
        div(class = "gpr-flow", div(class = "gpr-step", style = "order:-1", div(class = "gpr-step-h", span(class = "gpr-step-n", "Read"), span(class = "gpr-step-s", "rd3 / rd7 + rad + cor + mrk"))),
            lapply(seq_along(GPR_STEPS), function(i) step(names(GPR_STEPS)[i], i))),
        div(class = "mt-2", span(class = "wb-eb", "Velocity (depth axis)"),
          selectInput(ns("ground"), NULL, c(`Pick a typical value` = "", names(GPR_GROUND)), width = "100%"),
          div(class = "d-flex gap-1", num("vel", "Ground (m/ns)", 0.141, 0.005, "100px", 0.03), num("vtop", "Thawed layer (m/ns)", 0.0775, 0.005, "110px", 0.03),
              num("thaw", "Thaw depth (m)", 0, 0.1, "90px")),
          p(class = "muted", "Depth is two-way time x velocity / 2 from the time zero, with the thawed layer's velocity above the thaw depth (Davis and Annan 1989); the dashed line marks the permafrost table. Velocities are literature values (References below); measure one on site (hyperbola fit, CMP or a probe depth) and replace them.")),
        div(class = "mt-2", span(class = "wb-eb", "Display"),
          radioButtons(ns("x"), "Across", c(`Distance (GPS)` = "distance", `Distance (markers)` = "markers", Trace = "trace"), inline = TRUE),
          num("mspace", "Distance between markers (m), for Distance (markers)", NA, 1, "260px"),
          div(class = "d-flex gap-3", ovid_switch(ns("topo"), "Topography", FALSE), ovid_switch(ns("marks"), "Markers", TRUE)),
          num("zsmooth", "Elevation smoothing (GPS fixes, running median)", 11, 2, "260px", 1),
          sliderInput(ns("clip"), "Colour limit (percentile of amplitude)", min = 0.8, max = 1, value = 0.99, step = 0.005, width = "260px"),
          div(class = "d-flex gap-2", selectInput(ns("scale"), "Colours", names(GPR_SCALES), width = "170px"), num("maxd", "Deepest depth shown (m, 0 = all)", 0, 1, "170px")),
          p(class = "muted", "Topography draws the radargram on elevation (m a.s.l.) from the height in the .cor file, with the vertical axis in depth. A handheld GPS height is good to a few metres, much worse than the position, so smooth it and read the frost-table shape with care. Markers are the field markers in the .mrk file and those added here; Distance (markers) puts them the spacing given apart, the first at 0 (REFLEXW 'mark interpol')."))),
      div(
        textOutput(ns("caption")),
        conditionalPanel("input.before", ns = ns, plotly::plotlyOutput(ns("rg0"), height = "300px")),
        plotly::plotlyOutput(ns("rg"), height = "460px"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px",
          div(div(class = "d-flex gap-2 align-items-end", span(class = "wb-eb", "Picks"), textInput(ns("layer"), NULL, "frost table", width = "160px", placeholder = "layer of the next pick")),
              DTOutput(ns("picks")),
              div(class = "d-flex gap-2 mt-1", actionButton(ns("pick_undo"), "Delete the last pick", class = b[["btn"]]), ovid_export_button(ns("picks_csv"), "csv", "picks", class = b[["btn"]])),
              p(class = "muted mt-1", "The depth of a pick is two-way time x velocity / 2 from the time zero, with the thawed layer's velocity above the thaw depth (Davis and Annan 1989; REFLEXW manual). Picks need Topography off; they are kept in config/gpr_picks.csv.")),
          div(span(class = "wb-eb", "Markers"), DTOutput(ns("marks_tbl")),
              div(class = "d-flex gap-2 align-items-end mt-1", num("mk_tr", "Trace", NA, 1, "90px", 1), textInput(ns("mk_note"), "Note", "", width = "150px"),
                  actionButton(ns("mk_add"), "Add marker", class = paste(b[["btn"]], "mb-2")), actionButton(ns("mk_undo"), "Delete the last marker", class = paste(b[["btn"]], "mb-2")),
                  ovid_export_button(ns("marks_csv"), "csv", "markers", class = paste(b[["btn"]], "mb-2"))),
              p(class = "muted mt-1", "Markers added here are kept in config/gpr_markers.csv; the .mrk and the raw files are not changed."))),
        div(class = "d-flex gap-2 mt-2", ovid_export_button(ns("data_csv"), "csv", "processed data", class = b[["btn"]]),
            span(class = "muted", "The radargram's camera button saves the image with the flow in its caption; the processed data starts with the same caption line.")),
        tags$details(class = "mt-2", tags$summary("References for the processing and the permafrost velocities"), uiOutput(ns("refs")))),
      if (!wb) mod_gpr_flow_aside_ui(id)))
}

# The A-scan at the cursor, where it is, and a small map (the Workbench's inspector, or beside the radargram in Standard).
mod_gpr_flow_aside_ui <- function(id) { ns <- NS(id)
  div(class = "wb-insp", span(class = "wb-eb", "Trace at the cursor (A-scan)"), plotly::plotlyOutput(ns("ascan"), height = "320px"),
      uiOutput(ns("at")), span(class = "wb-eb", "Map"), plotly::plotlyOutput(ns("map"), height = "200px")) }

# lines: the viewable lines (gpr_scan rows); tracks: their GPS (named list of read_cor tables); project: the project folder;
# tick: a reactiveVal bumped when a flow, pick or marker is saved (the GPR Map counts the flows from it). Returns what the GPR
# Map shares: the line shown, its radargram (fig, to_x, to_trace), its field markers placed on the map, and select_line().
mod_gpr_flow_server <- function(id, lines, tracks, project, tick, read_line = gpr_read_line) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns; bump <- function() tick(tick() + 1L)
    ord <- reactiveVal(names(GPR_STEPS))
    observe(session$sendCustomMessage("gpr-wb-order", list(id = ns("row_"), order = as.list(ord()))))
    lapply(names(GPR_STEPS), function(k) { observeEvent(input[[paste0("up_", k)]], ord(gpr_order_move(ord(), k, -1L)))
                                           observeEvent(input[[paste0("dn_", k)]], ord(gpr_order_move(ord(), k, 1L))) })
    observe({ g <- lines(); cur <- isolate(input$line); updateSelectInput(session, "line", choices = g$line, selected = if (length(cur) && cur %in% g$line) cur else g$line[1]) })

    L <- reactive({ g <- lines(); validate(need(nrow(g) > 0, "No GPR line can be viewed yet. Add the folder with the .rad and .rd3 files on the GPR Map page."))
      req(input$line); r <- g[line == input$line]; req(nrow(r) == 1L); x <- read_line(r)
      validate(need(ncol(x$m) >= 2L, sprintf("%s has %s that can be read, too few to draw. The file is left as it is; pick another line, or copy the whole line from the instrument again.", r$line, if (ncol(x$m) == 1L) "1 trace" else paste(ncol(x$m), "traces")))); x })
    # safe: an antenna frequency not stated is not known (NA)
    mhz <- reactive(tryCatch(gpr_antenna_mhz(L()$h$antennas), error = function(e) NA))
    output$head <- renderUI({ x <- L(); r <- x$r; h <- x$h; n <- ncol(x$m)
      p(class = "muted", sprintf("%s: %d traces x %d samples, %s ns window, %s%s%s. %s", r$line, n, r$samples, format(round(h$timewindow_ns, 1)), if (nzchar(h$antennas)) h$antennas else "antenna not stated",
         if (!is.na(h$stacks)) paste0(", ", h$stacks, " stacks") else "",
         { f <- mhz(); if (!is.na(f) && !is.na(h$sampling_mhz)) sprintf(", sampled at %.0f MHz (%.0f x the antenna; MALA advises about 10 x)", h$sampling_mhz, h$sampling_mhz / f) else "" },
         paste(if (is.null(x$cor)) "No GPS file matched." else sprintf("GPS: %d fixes.", nrow(x$cor)), if (is.null(x$mrk)) "" else sprintf("Markers: %d.", nrow(x$mrk)), if (nzchar(r$note)) paste0("(", r$note, ")") else ""))) })
    P <- reactive(list(order = ord(), dc = isTRUE(input$on_dc), dewow = if (isTRUE(input$on_dewow)) input$dewow %||% 1 else 0,
      t0 = if (isTRUE(input$on_t0)) input$t0 %||% "auto" else "none", t0_ns = input$t0ns %||% 0, bp = isTRUE(input$on_bp),
      bp1 = input$bp1 %||% 0.25, bp2 = input$bp2 %||% 1, bp3 = input$bp3 %||% 2, bp4 = input$bp4 %||% 4,
      gain = if (isTRUE(input$on_gain)) input$gain %||% "decay" else "none", ga = input$ga %||% 0, gb = input$gb %||% 0, agc_ns = input$agcns %||% 25,
      bg = if (isTRUE(input$on_bg)) input$bg %||% "mean" else "none", bgn = input$bgn %||% 50))
    lapply(names(GPR_STEPS), function(k) output[[paste0("sum_", k)]] <- renderText(gpr_step_summary(k, P(), mhz())))
    dec <- reactive(gpr_decimate(L()$m))
    R <- reactive({ d <- dec(); r <- gpr_process(d$m, L()$h, P()); r$tr <- d$tr; r })
    R0 <- reactive({ d <- dec(); r <- gpr_process(d$m, L()$h, list(dc = FALSE)); r$tr <- d$tr; r })          # before: as read, the mean of each trace taken off
    X <- reactive({ x <- L(); n <- ncol(x$m); a <- gpr_axis(x, input$x %||% "distance", input$mspace %||% NA); d <- a$full    # pages/fct_gpr_flow.R
      list(full = d, n = n, label = a$label, at = function(tr) if (is.null(d)) tr else approx(seq_len(n), d, tr, rule = 2)$y,
           tr_of = function(x) max(1L, min(n, if (is.null(d)) as.integer(round(x)) else which.min(abs(d - x))))) })

    # ---- the flow: presets, saved versions, save, undo, apply to all ----
    apply_steps <- function(s, order = s$order) {
      ord(gpr_order(order))
      on <- c(dc = isTRUE(s$dc), dewow = isTRUE(s$dewow > 0), t0 = !identical(s$t0 %||% "none", "none"), bp = isTRUE(s$bp), gain = !identical(s$gain %||% "none", "none"), bg = !identical(s$bg %||% "none", "none"))
      for (k in names(on)) updateCheckboxInput(session, paste0("on_", k), value = on[[k]])
      if (on[["t0"]]) updateRadioButtons(session, "t0", selected = s$t0)
      if (on[["gain"]]) updateSelectInput(session, "gain", selected = s$gain)
      if (on[["bg"]]) updateSelectInput(session, "bg", selected = s$bg)
      if (on[["dewow"]]) updateNumericInput(session, "dewow", value = s$dewow)
      ids <- c(t0_ns = "t0ns", bp1 = "bp1", bp2 = "bp2", bp3 = "bp3", bp4 = "bp4", ga = "ga", gb = "gb", agc_ns = "agcns", bgn = "bgn")
      for (k in names(ids)) if (!is.null(s[[k]])) updateNumericInput(session, ids[[k]], value = s[[k]]) }
    observeEvent(input$preset, { s <- GPR_PRESETS[[input$preset]]; req(s); apply_steps(s, names(GPR_STEPS)) }, ignoreInit = TRUE)
    observeEvent(input$ground, if (nzchar(input$ground %||% "")) updateNumericInput(session, "vel", value = GPR_GROUND[[input$ground]]))
    hist <- reactive({ tick(); k <- input$line %||% ""; if (nzchar(k)) gpr_flow_history(project(), k) else list() })
    observe({ h <- rev(hist()); v <- vapply(h, function(x) as.character(x$version), "")
      updateSelectInput(session, "ver", choices = if (length(v)) setNames(v, sprintf("v%s, saved %s", v, vapply(h, function(x) x$saved %||% "", ""))) else c(`None saved` = ""),
                        selected = if (length(v)) v[1] else "") })
    load_ver <- function() { v <- input$ver %||% ""; f <- gpr_flow_load(project(), input$line %||% "", if (nzchar(v)) v) %||% gpr_flow_default(input$preset %||% "")   # none saved: the defaults
      apply_steps(f$steps)
      if (!is.null(f$velocity_m_ns)) updateNumericInput(session, "vel", value = f$velocity_m_ns)
      if (!is.null(f$v_top_m_ns)) updateNumericInput(session, "vtop", value = f$v_top_m_ns)
      if (!is.null(f$thaw_m)) updateNumericInput(session, "thaw", value = f$thaw_m) }
    observeEvent(list(input$line, input$ver), load_ver())
    observeEvent(input$undo, load_ver())
    flow_cur <- reactive({ tick(); v <- input$ver %||% ""; if (!nzchar(v)) return(NULL); f <- gpr_flow_load(project(), input$line %||% "", v); if (gpr_flow_same(f, P())) f })
    caption <- reactive(gpr_flow_caption(input$line %||% "", flow_cur(), R()$log))
    output$caption <- renderText(paste0(caption(), if (is.null(flow_cur())) " (Save flow with this line keeps it.)" else ""))
    vel <- function() input$vel %||% 0.141; vtop <- function() input$vtop %||% 0.0775; thaw <- function() input$thaw %||% 0
    observeEvent(input$save, { k <- input$line %||% ""; req(nzchar(k))
      v <- tryCatch(gpr_flow_save(project(), k, P(), vel(), vtop(), thaw(), input$preset %||% ""), error = function(e) { showNotification(paste("The flow was not saved:", conditionMessage(e)), type = "error"); NULL })
      if (!is.null(v)) { bump(); showNotification(sprintf("Flow v%d saved with %s.", v, k), type = "message") } })
    observeEvent(input$all, { g <- lines(); req(nrow(g) > 0); have <- vapply(g$line, function(k) length(gpr_flow_history(project(), k)) > 0L, NA)
      showModal(modalDialog(title = sprintf("Apply this flow to all %d lines?", nrow(g)),
        p(sprintf("Each line gets it as a new saved version: %d have no flow yet; %d already have one and keep their earlier versions (pick one to go back).", sum(!have), sum(have))),
        p(class = "muted", "The flow: ", paste(vapply(gpr_order(P()$order), function(k) paste0(GPR_STEPS[[k]], " (", gpr_step_summary(k, P(), mhz()), ")"), ""), collapse = " → ")),
        p(class = "muted", "Lines: ", paste(head(g$line, 8), collapse = ", "), if (nrow(g) > 8) sprintf(" and %d more", nrow(g) - 8)),
        footer = tagList(modalButton("Cancel"), actionButton(ns("all_go"), sprintf("Apply to %d lines", nrow(g)), class = "btn-primary")))) })
    observeEvent(input$all_go, { removeModal(); g <- lines()
      n <- tryCatch(gpr_flow_apply_all(project(), g$line, P(), vel(), vtop(), thaw(), input$preset %||% ""), error = function(e) { showNotification(paste("Not applied:", conditionMessage(e)), type = "error"); NULL })
      if (!is.null(n)) { bump(); showNotification(sprintf("Flow saved with %d lines.", n), type = "message") } })

    # ---- radargram (and the one before processing), drawn as the Display settings say ----
    pk <- reactive({ tick(); gpr_picks_read(project(), input$line %||% "") })
    um <- reactive({ tick(); gpr_markers_read(project(), input$line %||% "") })
    topo <- reactive(isTRUE(input$topo))
    draw <- function(r, src) { x <- L(); Xx <- X(); u <- um()[!is.na(trace)]; n <- ncol(x$m)
      mk <- if (isTRUE(input$marks %||% TRUE)) rbind(if (!is.null(x$mrk) && nrow(x$mrk)) data.table(marker = as.character(x$mrk$marker), x = Xx$at(x$mrk$trace)),
                                                     if (nrow(u)) data.table(marker = ifelse(nzchar(u$note), u$note, "+"), x = Xx$at(u$trace)))
      elev <- if (topo()) { z <- gpr_elevation(x$cor, n, input$zsmooth %||% 11L); if (sum(is.finite(z)) < 2L) NULL else approx(seq_len(n), z, r$tr, rule = 2)$y }
      f <- view_radargram(r$m, r$dt_ns, Xx$at(r$tr), Xx$label, input$v %||% "depth", vel(), input$clip %||% 0.99, input$scale %||% "Grey", thaw(), vtop(), input$maxd %||% 0, elev, mk)
      f$x$source <- ns(src); f }
    fig <- reactive(draw(R(), "rg"))
    output$rg <- plotly::renderPlotly({ f <- fig(); p0 <- pk()
      if (nrow(p0) && !topo()) { twt <- as.numeric(p0$twt_ns); y <- if ((input$v %||% "depth") == "depth") gpr_depth(twt, vel(), thaw(), vtop()) else twt
        f <- plotly::add_markers(f, x = X()$at(as.integer(p0$trace)), y = y, inherit = FALSE, text = p0$layer, hoverinfo = "text", showlegend = FALSE,
                                 marker = list(symbol = "circle-open", size = 10, color = PAL$gold, line = list(width = 2))) }
      b <- plotly::plotly_build(f) |> plotly::event_register("plotly_hover") |> plotly::event_register("plotly_click")    # the caption goes into every image saved from the plot
      b$x$layout$annotations <- c(b$x$layout$annotations, list(list(xref = "paper", yref = "paper", x = 0, y = 0, yshift = -46, xanchor = "left", yanchor = "top", showarrow = FALSE,
                                  text = caption(), font = list(size = 10, color = PAL$fg))))
      b$x$layout$margin$b <- 70; b })
    output$rg0 <- plotly::renderPlotly({ req(isTRUE(input$before)); draw(R0(), "rg0") |> plotly::layout(title = list(text = "Before: as read (mean of each trace taken off)", font = list(size = 11), x = 0)) })

    # ---- the cursor: a hover on the radargram picks the trace; a click picks or adds a marker ----
    cur_tr <- reactiveVal(NULL); tr_d <- debounce(reactive(cur_tr()), 60)
    observeEvent(suppressWarnings(plotly::event_data("plotly_hover", source = ns("rg"))), { e <- plotly::event_data("plotly_hover", source = ns("rg")); if (!is.null(e$x)) cur_tr(X()$tr_of(e$x[1])) })
    observeEvent(input$line, cur_tr(NULL))
    observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = ns("rg"))), { e <- plotly::event_data("plotly_click", source = ns("rg")); t <- input$tool %||% "cursor"
      if (is.null(e$x)) return(); k <- input$line %||% ""; tr <- X()$tr_of(e$x[1]); cur_tr(tr); if (t == "cursor") return()
      if (t == "pick" && topo()) return(showNotification("Picks are taken with Topography off, on a depth or time axis.", type = "warning"))
      ok <- tryCatch({ if (t == "pick") gpr_pick_add(project(), gpr_pick_row(k, e$x[1], e$y[1], input$v %||% "depth", X()$full, vel(), thaw(), vtop(), input$layer %||% "", flow_cur()$version %||% NA))
                       else gpr_marker_add(project(), k, tr, input$mk_note %||% ""); TRUE },
                     error = function(err) { showNotification(paste("Not saved:", conditionMessage(err)), type = "error"); FALSE })
      if (ok) bump() })
    output$ascan <- plotly::renderPlotly({ r <- R(); j <- { t <- isolate(tr_d()); if (is.null(t)) ceiling(ncol(r$m) / 2) else which.min(abs(r$tr - t)) }
      a <- r$m[, j]; tt <- (seq_len(nrow(r$m)) - 1) * r$dt_ns
      plotly::plot_ly(x = pmax(a, 0), y = tt, type = "scatter", mode = "lines", fill = "tozerox", fillcolor = col_rgba(PAL$gold, 0.55), line = list(width = 0), hoverinfo = "skip") |>
        plotly::add_trace(x = a, y = tt, mode = "lines", fill = "none", line = list(color = PAL$gold, width = 1), hoverinfo = "skip") |>
        plotly::layout(showlegend = FALSE, xaxis = list(title = "Amplitude", zeroline = TRUE), yaxis = list(title = "Two-way time (ns)", autorange = "reversed"),
                       margin = list(l = 50, r = 6, t = 4, b = 36), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = PAL$fg, size = 10)) |>
        plotly::config(displayModeBar = FALSE) })
    observeEvent(tr_d(), { r <- R(); a <- r$m[, which.min(abs(r$tr - tr_d()))]
      plotly::plotlyProxyInvoke(plotly::plotlyProxy("ascan", session), "restyle", list(x = list(pmax(a, 0), a)), list(0L, 1L)) })
    at <- reactive({ t <- tr_d(); if (is.null(t)) return(NULL); x <- L(); c0 <- x$cor
      g <- if (!is.null(c0) && nrow(c0) >= 2L) { c0 <- c0[order(trace)][!duplicated(trace)]; if (nrow(c0) >= 2L && t >= min(c0$trace) && t <= max(c0$trace))
        list(lat = approx(c0$trace, c0$lat, t)$y, lon = approx(c0$trace, c0$lon, t)$y, z = approx(c0$trace, c0$elev_m, t)$y) }
      list(tr = t, n = ncol(x$m), d = if (is.null(X()$full)) NA else X()$full[t], dmax = if (is.null(X()$full)) NA else max(X()$full, na.rm = TRUE), g = g) })
    output$at <- renderUI({ a <- at(); if (is.null(a)) return(p(class = "wb-p", "Move along the radargram to see the trace there."))
      div(class = "wb-facts", wb_kv("trace", sprintf("%d of %d", a$tr, a$n)), if (!is.na(a$d)) wb_kv("distance", sprintf("%.1f m of %.0f", a$d, a$dmax)),
          if (!is.null(a$g)) tagList(wb_kv("latitude", sprintf("%.5f", a$g$lat)), wb_kv("longitude", sprintf("%.5f", a$g$lon)),
                                     if (is.finite(a$g$z)) wb_kv("GPS elevation", sprintf("%.1f m", a$g$z))) else wb_kv("GPS", "none at this trace")) })
    map_n <- reactiveVal(0L)
    output$map <- plotly::renderPlotly({ k <- input$line %||% ""; t <- tracks()[[k]]; validate(need(!is.null(t) && nrow(t) > 0, "No GPS for this line."))
      g <- gpr_map(setNames(list(t), k), k) |> plotly::add_trace(type = "scattermapbox", mode = "markers", lat = t$lat[1], lon = t$lon[1], hoverinfo = "skip",
                                                                 marker = list(size = 12, color = PAL$fg, opacity = 0), showlegend = FALSE)
      b <- plotly::plotly_build(g |> plotly::config(displayModeBar = FALSE)); map_n(length(b$x$data) - 1L); b })
    observeEvent(at(), { g <- at()$g; if (is.null(g)) return()
      plotly::plotlyProxyInvoke(plotly::plotlyProxy("map", session), "restyle", list(lat = list(list(g$lat)), lon = list(list(g$lon)), `marker.opacity` = 1), list(map_n())) })

    # ---- picks, markers, exports and references ----
    observeEvent(input$pick_undo, { k <- input$line %||% ""; req(nzchar(k)); gpr_pick_drop_last(project(), k); bump() })
    observeEvent(input$mk_undo, { k <- input$line %||% ""; req(nzchar(k)); gpr_marker_drop_last(project(), k); bump() })
    output$picks <- renderDT({ p0 <- pk(); validate(need(nrow(p0) > 0, "No picks yet. Choose Pick above, then click a reflector."))
      ovid_long_table(p0[, .(Trace = trace, `m` = distance_m, `ns` = twt_ns, `Depth (m)` = depth_m, Layer = layer, `v (m/ns)` = velocity_m_ns, Flow = flow_version)], selection = "none", height = "24vh") })
    output$picks_csv <- ovid_download("GS", function() c("gpr picks", input$line %||% ""), "csv", function(file) {
      p0 <- copy(pk()); setcolorder(p0, c(setdiff(GPR_PICK_COLS, c("line", "picked")), "line", "picked")); fwrite(p0, file) })
    marks <- reactive({ x <- L(); Xx <- X(); u <- um()[!is.na(trace)]; c0 <- x$cor
      pos <- function(tr) { if (is.null(c0) || nrow(c0) < 2L) return(list(lat = rep(NA_real_, length(tr)), lon = rep(NA_real_, length(tr))))
        c1 <- c0[order(trace)][!duplicated(trace)]; list(lat = approx(c1$trace, c1$lat, tr)$y, lon = approx(c1$trace, c1$lon, tr)$y) }
      one <- function(tr, label, src) { q <- pos(tr); data.table(Marker = label, Trace = tr, `Distance (m)` = if (is.null(Xx$full)) NA_real_ else round(Xx$full[tr], 1),
                                                                Latitude = round(q$lat, 6), Longitude = round(q$lon, 6), From = src) }
      rbind(if (!is.null(x$mrk) && nrow(x$mrk)) one(x$mrk$trace, as.character(x$mrk$marker), "field (.mrk, read only)"),
            if (nrow(u)) one(u$trace, ifelse(nzchar(u$note), u$note, "+"), paste("added", u$added))) %||% data.table() })
    output$marks_tbl <- renderDT({ m0 <- marks(); validate(need(nrow(m0) > 0, "No markers on this line. Add one by trace below, or choose Add marker and click the radargram."))
      ovid_long_table(m0, selection = "none", height = "24vh") })
    observeEvent(input$mk_add, { k <- input$line %||% ""; t <- input$mk_tr; req(nzchar(k))
      if (!isTRUE(t >= 1 && t <= X()$n)) { showNotification(sprintf("Give a trace from 1 to %d.", X()$n), type = "warning"); return() }
      gpr_marker_add(project(), k, t, input$mk_note %||% ""); bump() })
    output$marks_csv <- ovid_download("GS", function() c("gpr markers", input$line %||% ""), "csv", function(file) fwrite(marks(), file))
    output$data_csv <- ovid_download("GS", function() c("gpr processed", input$line %||% ""), "csv", function(file) {
      x <- L(); r <- gpr_process(x$m, x$h, P())                                 # every trace, at full resolution
      writeLines(paste("#", caption()), file)
      d <- data.table(time_ns = round((seq_len(nrow(r$m)) - 1) * r$dt_ns, 4), r$m); setnames(d, c("time_ns", paste0("trace_", seq_len(ncol(r$m)))))
      fwrite(d, file, append = TRUE, col.names = TRUE) })
    output$refs <- renderUI({ pr <- input$preset %||% ""; tagList(p(class = "muted", GPR_PRESETS[[if (nzchar(pr)) pr else names(GPR_PRESETS)[1]]]$ref), tags$ol(lapply(GPR_REFERENCES, tags$li)),
      p(class = "muted", "Velocities are literature values for particular sites; they are starting points, not constants of permafrost.")) })

    # ---- what the GPR Map shares: the line, its radargram and its field markers on the map ----
    mk_map <- reactive({ x <- L(); if (!isTRUE(input$marks %||% TRUE) || is.null(x$mrk) || !nrow(x$mrk) || is.null(x$cor) || nrow(x$cor) < 2L) return(NULL)
      c <- x$cor[order(trace)][!duplicated(trace)]; k <- x$mrk[trace >= min(c$trace) & trace <= max(c$trace)]; if (!nrow(k)) return(NULL)
      cbind(k[, .(marker, trace)], lat = approx(c$trace, c$lat, k$trace)$y, lon = approx(c$trace, c$lon, k$trace)$y) })
    list(line = reactive(input$line %||% ""), markers = mk_map,
         radargram = reactive({ Xx <- X(); list(fig = fig(), to_x = Xx$at, to_trace = Xx$tr_of) }),
         select_line = function(k) updateSelectInput(session, "line", selected = k))
  })
}
