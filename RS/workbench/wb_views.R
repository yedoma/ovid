# RS/workbench/wb_views.R  --  The Workbench's Needs you and Positioning tabs. Needs you: every flight's open problems,
# worst first (pages/fct_needs.R), with Fix (the page that fixes it, on that flight), Accept as is (a reason, kept on the
# core review queue's file config/review_decisions.csv) and Skip (for now), and the missing metadata. Positioning: the evidence
# per flight and what it is fit for, the status by year, and checkpoint accuracy through the core import
# (pages/mod_checkpoint.R). Sourced by wb_rs.R.

wb_needs_tab_ui <- function() div(class = "wb-std wb-queue", tags$h1(class = "wb-h1", "Needs you"),
  p(class = "wb-lede", "Every flight's open problems from the catalogue and its checks, worst first. Fix opens the page that fixes it on that flight; Accept as is keeps a reason with the decision; Skip hides it until the app is opened again. Nothing here changes the catalogue until a rebuild."),
  uiOutput("wbn_chips"), uiOutput("wbn_list"),
  div(style = "margin-top:16px", span(class = "wb-eb", "Missing metadata"), plotOutput("wbn_missing", height = 300)))

wb_positioning_ui <- function() div(class = "wb-std d-tool", tags$h1(class = "wb-h1", "Positioning"),
  p(class = "wb-lede", "How each flight's positions were got and what they are fit for: corrected (RTK or PPK) positions are fit for mapping and comparing years; standalone GNSS is metres off and fit for the archive. Placed and corrected are counted apart: a log places images without correcting them."),
  DT::DTOutput("wbp_tbl"),
  div(style = "margin-top:16px", span(class = "wb-eb", "Positioning status by year"), plotOutput("wbp_status", height = 300)),
  div(class = "wb-inline", style = "margin:8px 0 16px", tags$a(href = "#", `data-ovid-go` = "GNSS", HTML("The GNSS fixes on file, on a map: the GNSS page &rarr;"))),
  mod_checkpoint_ui("wbckp"))

# ctx: the environment of wb_server (std, flights, set_view).
wb_views_server <- function(input, output, session, ctx) {
  std <- ctx$std
  fl <- reactive(ctx$flights()); ck <- reactive(std$fchk_raw())   # the checks as found: an item accepted here stays listed as done
  rfile <- function() file.path(path.expand(CFG$project_dir), "config", REVIEW_FILE)
  done <- reactive({ p <- rfile(); live_stamp(p); review_read(p) })   # core config.R
  skipped <- reactiveVal(character()); kind_pick <- reactiveVal("")
  items <- reactive({ x <- rs_needs(fl(), ck()); d <- done(); x[, decided := if (!is.null(d) && nrow(d)) d$decision[match(id, d$id)] else NA_character_][] })
  output$wbn_chips <- renderUI({ x <- items()[is.na(decided) & !id %in% skipped()]; if (!nrow(x)) return(p(class = "wb-p", "Nothing waits for you."))
    n <- x[, .N, by = kind][order(-N)]
    div(class = "wb-q-chips", tags$a(href = "#", class = paste("wb-q-chip", if (!nzchar(kind_pick())) "on"), `data-wb-input` = "wbn_kind", `data-wb-key` = "", paste("All", nrow(x))),
        lapply(seq_len(nrow(n)), function(i) tags$a(href = "#", class = paste("wb-q-chip", if (identical(kind_pick(), n$kind[i])) "on"), `data-wb-input` = "wbn_kind", `data-wb-key` = n$kind[i], paste(gsub("_", " ", n$kind[i]), n$N[i]))),
        tags$label(class = "wb-q-done ovid-sw", tags$input(type = "checkbox", role = "switch", id = "wbn_showdone", onchange = "Shiny.setInputValue('wbn_showdone', this.checked)"), paste0(" Show done (", sum(!is.na(items()$decided)), ")"))) })
  observeEvent(input$wbn_kind, kind_pick(input$wbn_kind %||% ""))
  output$wbn_list <- renderUI({ x <- items()[!id %in% skipped()]; if (!nrow(x)) return(NULL)
    if (nzchar(kind_pick())) x <- x[kind == kind_pick()]; if (!isTRUE(input$wbn_showdone)) x <- x[is.na(decided)]
    n <- nrow(x); x <- utils::head(x, 200)
    b <- function(id, act, lab, cls = "") tags$button(type = "button", class = paste("wb-btn sm", cls), onclick = sprintf("Shiny.setInputValue('wbn_act', {id: '%s', act: '%s', n: Date.now()}, {priority: 'event'})", id, act), lab)
    sym <- c(bad = "✕", look = "!", note = "i")
    tagList(div(class = "wb-p", style = "color:var(--wb-fg2)", if (n > 200) paste("The first 200 of", n) else paste(n, if (n == 1) "item" else "items")),
      tags$table(class = "wb-q-tab", tags$thead(tags$tr(tags$th(""), tags$th("flight"), tags$th("date"), tags$th("what"), tags$th("why it matters"), tags$th("suggestion"), tags$th("decide"))),
        tags$tbody(lapply(seq_len(nrow(x)), function(i) { r <- x[i]
          tags$tr(class = if (!is.na(r$decided)) "done", tags$td(class = "mono", title = r$severity, sym[[r$severity]]), tags$td(r$flight), tags$td(class = "mono", r$date),
            tags$td(r$what), tags$td(class = "why", r$why), tags$td(class = "why", r$todo),
            tags$td(class = "acts", if (!is.na(r$decided)) tagList(span(class = "m", r$decided), b(r$id, "undo", "Undo")) else
              tagList(b(r$id, "fix", "Fix…"), b(r$id, "ok", "Accept as is…"), b(r$id, "skip", "Skip")))) })))) })
  pend <- reactiveVal(NULL)
  write_decision <- function(qid, row) review_write(rfile(), qid, row)   # core config.R
  observeEvent(input$wbn_act, { a <- input$wbn_act; r <- items()[id == a$id]; req(nrow(r) == 1)
    if (a$act == "skip") return(skipped(c(skipped(), r$id)))
    if (a$act == "fix") { ctx$key(r$flight_id); std$sel(r$flight_id)                     # the Workbench's flight and the Standard pages' alike
      return(session$sendCustomMessage("wb-open", if (nzchar(r$tab %||% "")) list(go = r$go, tab = r$tab) else list(go = r$go))) }
    if (isTRUE(get0("VIEWER"))) return()
    if (a$act == "undo") return(write_decision(r$id, NULL))
    pend(r); showModal(modalDialog(title = "Accept as is", easyClose = TRUE,
      div(class = "wb-p", style = "color:var(--wb-fg2)", paste0(r$flight, " (", r$date, "): ", r$what, ". ", r$why)),
      textInput("wbn_reason", "Why it is fine as it is (kept with the decision)", width = "100%"), textInput("wbn_author", "Initials", width = "100%"), uiOutput("wbn_reason_err"),
      footer = tagList(modalButton("Cancel"), actionButton("wbn_ok", "Accept as is", class = "btn-primary")))) })
  observeEvent(input$wbn_ok, { r <- pend(); req(r); why <- trim(input$wbn_reason %||% "")
    if (!nzchar(why)) { output$wbn_reason_err <- renderUI(div(class = "text-danger", style = "font-size:12px;margin-top:-6px", "Say why, in a few words: it goes with the decision.")); return() }
    write_decision(r$id, data.table(id = r$id, decision = "accepted as is", note = paste0(why, " (", r$flight, " · ", r$what, ")"), author = trim(input$wbn_author %||% ""),
                                    at = format(Sys.time(), "%Y-%m-%d %H:%M"))); removeModal(); pend(NULL) })

  output$wbp_tbl <- DT::renderDT({ f <- fl(); validate(need(nrow(f) > 0, "No flights yet: build the catalogue first.")); k <- ck()[match(f$flight_id, flight_id)]
    col <- function(v) if (v %in% names(f)) f[[v]] else rep(NA, nrow(f))
    acc <- suppressWarnings(as.numeric(col("acc_capture_m"))); acc2 <- suppressWarnings(as.numeric(col("acc_processed_m"))); acc[!is.finite(acc)] <- acc2[!is.finite(acc)]
    x <- data.table(Flight = f$name, Date = f$date, Method = f$positioning_status, Evidence = col("positioning_evidence"),
                    `Accuracy (m)` = ifelse(is.finite(acc), format(acc), ""), `RTK fixed (%)` = col("rtk_fixed_pct"),
                    Placed = sprintf("%s of %s, from %s", num_or(k$n_placed), num_or(f$n_images), k$placed_from),
                    `Fit for` = ifelse(f$positioning_status %in% CORRECTED_STATES, "mapping", "archive"))
    ovid_long_table(x, selection = "none", height = "40vh") })        # core longtable.R
  output$wbp_status <- renderPlot(bg = PAL$bg1, { f <- fl(); if (!nrow(f)) return(NULL)
    d <- f[, .N, by = .(year, positioning_status)][, positioning_status := factor(positioning_status, rev(POSITIONING))]
    ggplot(d, aes(year, N, fill = positioning_status)) + geom_col(width = 0.7) + scale_fill_manual(values = STATUS_COL, name = NULL, drop = TRUE) +
      labs(x = NULL, y = "flights") + theme_ovid(PAL_PLOT, base_size = 12) + guides(fill = guide_legend(ncol = 2, reverse = TRUE)) })
  output$wbn_missing <- renderPlot(bg = PAL$bg1, { f <- fl(); if (!nrow(f)) return(NULL); m <- rs_missing_counts(f)   # pages/fct_needs.R
    ggplot(m, aes(n, reorder(what, n))) + geom_col(fill = PAL$gold, width = 0.6) + geom_text(aes(label = n), hjust = -0.3, colour = PAL$fg4, size = 3.6) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) + labs(x = "flights", y = NULL) + theme_ovid(PAL_PLOT, base_size = 12) })
  mod_checkpoint_server("wbckp", reactive({ std$rv$tick; tryCatch(load_configs(CFG)$calibration, error = function(e) NULL) }))
  invisible(ctx)
}
