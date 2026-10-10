# CS/pages/parameter_guide.R  --  Parameter guide.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Parameter guide ----------------------------------------------------
  pg_data <- reactive({
    i <- copy(info()); o <- outs(); i[, row := .I]
    ps <- if (!is.null(o) && nrow(o$pstats)) copy(o$pstats) else NULL
    if (!is.null(ps)) ps[, row := param_info_row(parameter, i)]
    list(info = i, stats = ps)
  })
  output$pg_cards <- renderUI({
    g <- pg_data(); i <- g$info; ps <- g$stats; show <- input$pg_show
    fmt <- function(x) format(signif(x, 4), big.mark = ",", trim = TRUE)
    cards <- lapply(seq_len(nrow(i)), function(k) {
      mine <- if (is.null(ps)) NULL else ps[row == k]
      lo <- suppressWarnings(as.numeric(i$typical_min[k])); hi <- suppressWarnings(as.numeric(i$typical_max[k]))
      out <- if (is.null(mine) || !nrow(mine) || is.na(lo) || is.na(hi)) NULL else mine[p01 < lo | p99 > hi]
      if (show == "mine" && (is.null(mine) || !nrow(mine))) return(NULL)
      if (show == "out" && (is.null(out) || !nrow(out))) return(NULL)
      yours <- if (!is.null(mine) && nrow(mine)) div(class = "yours",
        tags$b("In your data: "), paste0(num(uniqueN(mine$parameter)), " column", if (uniqueN(mine$parameter) > 1) "s" else "", " at ",
        num(uniqueN(paste(mine$station, mine$stream))), " station table", if (uniqueN(paste(mine$station, mine$stream)) > 1) "s" else "", ", from ", fmt(min(mine$min)), " to ", fmt(max(mine$max)),
        " (middle 98% of values: ", fmt(min(mine$p01)), " to ", fmt(max(mine$p99)), ")."),
        if (!is.null(out) && nrow(out)) span(class = "warnline", tags$br(), icon("triangle-exclamation"), " Outside the usual range at: ",
          paste(head(unique(paste0(out$station, " / ", out$stream, " (", out$parameter, ")")), 6), collapse = ", "), if (nrow(out) > 6) paste0(" and ", nrow(out) - 6, " more"))
        else span(class = "okline", tags$br(), icon("check"), " All within the usual range."),
        tags$br(), span(class = "dim", "Columns: ", paste(head(sort(unique(mine$parameter)), 14), collapse = ", "), if (uniqueN(mine$parameter) > 14) " ..."))
      div(class = "guide", h3(i$topic[k], if (nzchar(i$unit[k])) span(class = "unit", i$unit[k])),
        tags$dl(
          tags$dt("What it is"), tags$dd(i$about[k]),
          tags$dt("Reasonable values"), tags$dd(i$reasonable[k]),
          tags$dt("Usual range"), tags$dd(if (!is.na(lo) && !is.na(hi)) paste0(fmt(lo), " to ", fmt(hi), " ", i$unit[k]) else "not set",
            if (nzchar(i$sensor_min[k]) && nzchar(i$sensor_max[k])) span(class = "dim", paste0("   (sensor limits ", i$sensor_min[k], " to ", i$sensor_max[k], ")"))),
          tags$dt("Watch for"), tags$dd(i$watch_for[k]),
          tags$dt("Sensors"), tags$dd(i$sensors[k]),
          tags$dt("Manuals"), tags$dd(span(class = "dim", i$source[k]))),
        yours,
        if (!is.na(lo) && !is.na(hi)) div(class = "btnrow editor-only", style = "margin-bottom:0",
          actionButton(paste0("pg_rule_", k), "Use the usual range as a quality rule", onclick = sprintf("Shiny.setInputValue('pg_rule', %d, {priority: 'event'})", k))))
    })
    cards <- Filter(Negate(is.null), cards)
    if (!length(cards)) return(empty_msg(switch(show, mine = "No columns in your data match the guide yet. Run the pipeline, or choose Everything.",
                                                out = "Nothing in your data falls outside the usual ranges.", "The guide is empty.")))
    tagList(cards)
  })
  observeEvent(input$pg_rule, {
    i <- info(); k <- input$pg_rule; req(k >= 1, k <= nrow(i))
    q <- m_qc$data()
    if (!is.null(q) && i$pattern[k] %in% q$pattern) return(note("There is already a quality rule with this pattern. Edit it on the Quality rules page.", type = "warning"))
    ok <- m_qc$append(data.frame(pattern = i$pattern[k], min = i$typical_min[k], max = i$typical_max[k], max_spike = "", notes = paste("usual range,", i$topic[k])))
    if (ok) note("Rule added for ", i$topic[k], " (", i$typical_min[k], " to ", i$typical_max[k], "). Run the pipeline to apply it.")
  })

