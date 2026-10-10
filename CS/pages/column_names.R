# CS/pages/column_names.R  --  Column names.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Column names ------------------------------------------------------
  output$dict <- renderDT({ o <- outs(); validate(need(!is.null(o), "Run the pipeline first."))
    d <- copy(o$dict)[, mapped := fifelse(parameter == raw_column, "no", "yes")]
    ovid_dt(d, selection = "none", page = 15) })
  observe({
    o <- outs(); req(o)
    update_selectize(session, "un_cols", choices = sort(unique(o$dict[parameter == raw_column, raw_column])), server = TRUE)
    updateSelectizeInput(session, "un_param", choices = c("", sort(unique(o$dict[parameter != raw_column, parameter]))), selected = "", server = FALSE)
  })
  # built-in aliases, groups and limits this project does not have yet
  sug <- reactive({ if (VIEWER) return(NULL); a <- m_al$data(); g <- m_gr$data(); q <- m_qc$data(); if (is.null(a) || is.null(g) || is.null(q)) return(NULL)
    stale <- which(q$pattern == "^vwc.*" & tolower(q$action) == "flag" & q$min == "-0.05")       # the short-lived flag-only water content limit
    retired <- retired_rows(a)                                                                        # numbered ground-sensor names the depth names replace
    al <- missing_defaults(if (length(retired)) a[-retired, , drop = FALSE] else a, "aliases")
    list(aliases = al, groups = missing_defaults(g, "groups"), qc = missing_defaults(q, "qc"), stale = stale, retired = retired) })
  output$sug_ui <- renderUI({ s <- sug(); if (is.null(s) || !(nrow(s$aliases) + nrow(s$groups) + nrow(s$qc) + length(s$stale))) return(NULL); o <- outs()
    hit <- if (is.null(o) || !nrow(s$aliases)) 0 else { u <- unique(o$dict[parameter == raw_column, raw_column]); sum(vapply(u, function(x) any(vapply(s$aliases$pattern, function(p) grepl(p, x, ignore.case = TRUE, perl = TRUE), logical(1))), logical(1))) }
    div(class = "brushbox", tags$b("Built-in names you have not added yet"),
        p(style = "margin:6px 0", num(nrow(s$aliases)), " aliases, ", num(nrow(s$groups)), " groups and ", num(nrow(s$qc)), " limits come with this version and are not in your tables.",
          if (length(s$stale)) " The water content limit goes back to removing values outside 0 to 1.",
          if (length(s$retired)) tagList(" ", num(length(s$retired)), " older built-in rows that name ground sensors by number are replaced by names that carry the depth (ground_temp_50cm), so columns already named ground_temp_2 and the like are renamed at the next run."),
          if (hit) tagList(" The aliases would name ", tags$b(num(hit)), " of your unmapped columns: ground thermistors by depth (from rows for each logger, otherwise sensor number x 10 cm), wind at two heights, radiation, snow depth, water content and conductivity.")),
        tags$details(tags$summary("Show them"), div(class = "scrollbox", style = "max-height:260px;margin-top:6px", HTML(html_table(rbind(s$aliases[, .(table = "alias", pattern, becomes = parameter, logger = logger_id)],
          s$groups[, .(table = "group", pattern = substr(pattern, 1, 70), becomes = group, logger = "")], s$qc[, .(table = "limit", pattern, becomes = paste(min, "to", max), logger = "")]), 200)))),
        div(class = "btnrow", actionButton("sug_add", "Add them to my tables", class = "btn-primary", icon = icon("plus")),
            span(class = "dim", "Your own rows are kept and tried first. Each addition is recorded under History. Run the pipeline to apply."))) })
  observeEvent(input$sug_add, { s <- sug(); req(s)
    if (length(s$retired)) m_al$mutate(function(d) { i <- retired_rows(d); if (length(i)) d[-i, , drop = FALSE] else d })
    if (nrow(s$aliases)) m_al$mutate(function(d) rbind(d, as.data.frame(ensure_cols(copy(s$aliases), names(d))[, names(d), with = FALSE])))
    if (nrow(s$groups)) m_gr$mutate(function(d) rbind(as.data.frame(s$groups[, names(d), with = FALSE]), d))          # groups: first match wins, so the new ones go on top
    if (length(s$stale)) m_qc$mutate(function(d) { i <- which(d$pattern == "^vwc.*" & tolower(d$action) == "flag" & d$min == "-0.05"); d$min[i] <- "0"; d$action[i] <- ""; d$notes[i] <- "m3/m3; a CS650/CS655 reports 0 to 1 and a loose wire gives values far outside it"; d })
    if (nrow(s$qc)) m_qc$mutate(function(d) { d <- ensure_cols(as.data.table(d), QC_COLS); d[is.na(d)] <- ""; as.data.frame(rbind(d, ensure_cols(copy(s$qc), names(d))[, names(d), with = FALSE])) })
    mark(); note("Added. Run the pipeline to apply the new names.") })
  observeEvent(input$un_add, {
    if (!length(input$un_cols)) return(note("Pick at least one raw column.", type = "warning"))
    p <- trim(input$un_param)
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", p)) return(note("Parameter names use letters, digits and underscores, starting with a letter.", type = "warning"))
    ok <- m_al$append(data.frame(pattern = paste0("^(", paste(rx_escape(input$un_cols), collapse = "|"), ")$"),
                                 parameter = p, logger_id = "", notes = "added in app"))
    if (ok) { note("Alias added for ", p, ". Run the pipeline to apply.")
      updateSelectizeInput(session, "un_cols", selected = character()); updateSelectizeInput(session, "un_param", selected = "") }
  })

