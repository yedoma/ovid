# CS/pages/combined_records.R  --  Combined records.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Combined records ---------------------------------------------------------
  comp_file <- reactive(file.path(pd(), "config", "composites.csv"))
  comp_all <- reactive({ rv$tick; rv$cb_tick; f <- comp_file(); live_stamp(f); if (file.exists(f)) ensure_cols(read_cfg(f), COMPOSITE_COLS)[, COMPOSITE_COLS, with = FALSE] else empty_cfg(COMPOSITE_COLS) })
  observe({ n <- unique(comp_all()$composite); updateSelectizeInput(session, "cb_name", choices = n, selected = isolate(input$cb_name) %||% "", server = FALSE) })
  cb_params <- reactive({ o <- outs(); if (is.null(o) || !nrow(o$pstats)) character() else sort(unique(o$pstats$parameter)) })
  observe(update_selectize(session, "cb_param", choices = cb_params(), selected = isolate(input$cb_param), server = TRUE))
  output$cb_empty <- renderUI(if (!length(cb_params())) empty_msg())
  cb_have <- reactive({ o <- outs(); req(o, nrow(o$pstats), nzchar(input$cb_param %||% "")); o$pstats[parameter == input$cb_param & as.numeric(values) > 0] })
  observe({ h <- cb_have(); updateSelectizeInput(session, "cb_station", choices = sort(unique(h$station)), server = FALSE) })
  observe({ h <- cb_have(); st <- input$cb_station %||% ""; s <- sort(unique(h[station == st]$stream))
    updateSelectInput(session, "cb_stream", choices = c(if (length(s) > 1) c("Every table" = ""), setNames(s, s))) })
  rv$cb <- NULL; rv$cb_tick <- 0; rv$cb_read <- NULL
  observeEvent(input$cb_name, { nm <- input$cb_name %||% ""; d <- comp_all()[composite == nm]
    rv$cb <- rv$cb_read <- if (nrow(d)) d[order(suppressWarnings(as.numeric(priority)))] else empty_cfg(COMPOSITE_COLS)
    if (nrow(d)) { updateSelectizeInput(session, "cb_param", selected = d$parameter[1]); updateSelectInput(session, "cb_step", selected = d$step[1]) } }, ignoreInit = TRUE)
  # the open record saved on another page or renamed: taken up while it has no edits here
  observeEvent(comp_all(), { nm <- input$cb_name %||% ""; if (!nzchar(nm) || is.null(rv$cb_read) || !identical(rv$cb, rv$cb_read)) return()
    d <- comp_all()[composite == nm]; rv$cb <- rv$cb_read <- if (nrow(d)) d[order(suppressWarnings(as.numeric(priority)))] else empty_cfg(COMPOSITE_COLS) }, ignoreInit = TRUE)
  cb_rows <- reactive({ d <- rv$cb; if (is.null(d) || !nrow(d)) return(empty_cfg(COMPOSITE_COLS)); d <- copy(d)
    d[, `:=`(composite = input$cb_name %||% "", parameter = input$cb_param %||% "", step = c(input$cb_step %||% "", rep("", .N - 1)), priority = as.character(seq_len(.N)))]; d })
  observeEvent(input$cb_add, { req(nzchar(input$cb_station %||% ""), nzchar(input$cb_param %||% ""))
    d <- if (is.null(rv$cb)) empty_cfg(COMPOSITE_COLS) else rv$cb
    rv$cb <- rbind(d, data.table(composite = "", parameter = input$cb_param, station = input$cb_station, stream = input$cb_stream %||% "", start = trim(input$cb_start),
                                 end = trim(input$cb_end), priority = "", offset = trim(input$cb_offset), step = "", notes = ""), fill = TRUE)[, COMPOSITE_COLS, with = FALSE] })
  cb_move <- function(by) { d <- rv$cb; s <- input$cb_src_rows_selected; req(d, length(s) == 1); t <- s + by; req(t >= 1, t <= nrow(d))
    o <- seq_len(nrow(d)); o[c(s, t)] <- o[c(t, s)]; rv$cb <- d[o]; DT::selectRows(DT::dataTableProxy("cb_src"), t) }
  observeEvent(input$cb_up, cb_move(-1)); observeEvent(input$cb_down, cb_move(1))
  observeEvent(input$cb_rm, { d <- rv$cb; s <- input$cb_src_rows_selected; req(d, length(s) > 0); rv$cb <- d[-s] })
  output$cb_src <- renderDT({ d <- cb_rows(); validate(need(nrow(d) > 0, "Pick a parameter, then add the stations to take it from, the most preferred first."))
    ovid_dt(d[, .(order = priority, station, table = stream, from = start, to = end, offset, notes)], selection = "single", page = 10,
          editable = list(target = "cell", disable = list(columns = 0:2))) })
  observeEvent(input$cb_src_cell_edit, { e <- input$cb_src_cell_edit; d <- copy(rv$cb); col <- c("priority", "station", "stream", "start", "end", "offset", "notes")[e$col + 1L]
    if (col %in% c("start", "end", "offset", "notes")) { set(d, e$row, col, trim(as.character(e$value))); rv$cb <- d } })
  cb_built <- reactive({ d <- cb_rows(); req(nrow(d), nzchar(input$cb_param %||% ""))
    withProgress(message = "Building the combined record", value = 0.5, safely(build_composite(d, pd()), "Combined record not built")) })
  output$cb_plot <- renderPlot({ b <- cb_built(); req(b, nrow(b$record))
    r <- b$record; lab <- paste0(r$station, ifelse(nzchar(r$stream) & uniqueN(r[, .(station, stream)]) > uniqueN(r$station), paste0(" / ", r$stream), ""))
    r[, source := factor(lab, levels = unique(lab))]
    cols <- distinct_cols(rep(unname(unlist(GB[c("yellow", "aqua", "orange", "purple", "green", "blue", "red")])), length.out = nlevels(r$source)), GB)
    ggplot(r, aes(TIMESTAMP, value, colour = source)) + geom_point(size = 0.35, alpha = 0.8, shape = 16) +
      scale_colour_manual(values = cols, name = NULL) + guides(colour = guide_legend(override.aes = list(size = 3))) +
      labs(x = NULL, y = b$parameter, title = input$cb_name %||% "", subtitle = sprintf("%s values from %d source(s)%s", num(nrow(r)), nlevels(r$source),
           if (nzchar(b$step)) paste0(", ", b$step, "ly means") else "")) + theme_ovid(GB) + theme(legend.position = "top")
  }, bg = GB$bg, res = 96)
  output$cb_used <- renderDT({ b <- cb_built(); req(b, nrow(b$sources)); x <- copy(b$sources)
    for (v in c("first", "last")) if (v %in% names(x)) x[, (v) := fmt_ts(get(v))]
    ovid_dt(x[order(priority)], page = 10) })
  observeEvent(input$cb_save, { if (VIEWER) return(); nm <- trim(input$cb_name %||% "")
    if (!nzchar(nm)) return(note("Give the combined record a name first.", type = "warning"))
    d <- cb_rows(); if (!nrow(d)) return(note("Add at least one source first.", type = "warning"))
    all <- rbind(comp_all()[composite != nm], d)[, COMPOSITE_COLS, with = FALSE]
    try(log_change(comp_file(), all, current_user(session)), silent = TRUE); fwrite_safe(all, comp_file()); rv$cb <- rv$cb_read <- d; rv$cb_tick <- rv$cb_tick + 1; mark()
    note("Saved. It is rebuilt into output/composites on every run; Export CSV gives it now.") })
  observeEvent(input$cb_delete, { if (VIEWER) return(); nm <- input$cb_name %||% ""; req(nzchar(nm), nm %in% comp_all()$composite)
    all <- comp_all()[composite != nm]; try(log_change(comp_file(), all, current_user(session)), silent = TRUE); fwrite_safe(all, comp_file()); mark()
    rv$cb <- NULL; rv$cb_tick <- rv$cb_tick + 1; updateSelectizeInput(session, "cb_name", selected = ""); note("Deleted ", nm, ".") })
  output$cb_dl <- downloadHandler(function() paste0("ovid_cs_combined_", safe_name(input$cb_name %||% "record"), "_", format(Sys.Date()), ".csv"),
    function(file) { b <- cb_built(); r <- copy(b$record); setnames(r, "value", b$parameter); fwrite(r, file, dateTimeAs = "write.csv") })

