# CS/pages/audit.R  --  Audit.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Audit -------------------------------------------------------------
  au_index <- reactive({ o <- outs(); req(o); a <- copy(o$index)
    a[, who := fifelse(!is.na(station) & station != "", station, fifelse(!is.na(logger_id) & logger_id != "", paste("logger", logger_id), "(files)"))]; a })
  observe({
    a <- au_index(); ch <- function(x) c("All", sort(unique(x[!is.na(x) & x != ""])))
    update_selectize(session, "au_station", choices = ch(a$who), server = TRUE)
    update_selectize(session, "au_column", choices = ch(a$column), server = TRUE)
  })
  output$au_cards <- renderUI({ if (is.null(outs())) return(empty_msg()); a <- au_index(); i <- rule_info(a$rule); o <- outs()
    cnt <- function(sel) sum(ifelse(a$level[sel] == "cell", a$entries[sel], a$n[sel]), na.rm = TRUE)
    st <- function(v, l) div(class = "stat", tags$b(num(v)), span(l))
    div(class = "stats", st(cnt(i$lost == "no" & a$level == "row"), "duplicate records dropped (nothing lost)"), st(cnt(i$lost == "yes" & a$level == "row"), "records dropped for another reason"),
        st(cnt(i$lost == "yes" & a$level == "cell"), "values blanked"), st(cnt(i$lost == "kept"), "values or records changed but kept"),
        st(sum(a$level == "file"), "files not used"), st(if (nrow(o$flags)) sum(as.numeric(o$flags$records)) else 0, "values flagged as suspect (kept)")) })
  au_rule_tbl <- reactive({ a <- au_index(); req(nrow(a) > 0)
    r <- a[, .(count = sum(as.numeric(ifelse(level == "cell", entries, ifelse(level == "file", 1, n))), na.rm = TRUE), where = as.integer(uniqueN(who[who != "(files)"])), level = level[1], stage = stage[1]), by = rule]
    i <- rule_info(r$rule); r[, `:=`(title = i$title, lost = i$lost, meaning = i$meaning, action = i$action, page = i$page)]
    r[order(match(lost, c("yes", "kept", "no", "file")), -count)] })
  output$au_rules <- renderDT({ r <- au_rule_tbl()
    lab <- c(yes = "<span class='d-flag error'>removed</span>", kept = "<span class='d-flag warn'>changed, kept</span>", no = "<span class='d-flag ok'>nothing lost</span>", file = "<span class='d-flag off'>file not used</span>")
    unit <- ifelse(r$level == "cell", "values", ifelse(r$level == "file", "files", "records"))
    d <- data.table(`What happened` = r$title, `Data lost?` = unname(lab[r$lost]), Count = r$count, Unit = unit, `Stations or loggers` = r$where, `Rule name in the files` = r$rule)
    formatRound(ovid_dt(d, selection = "single", page = 40, escape = FALSE, opts = list(dom = "t", scrollY = NULL)), "Count", 0) })
  observeEvent(input$au_rules_rows_selected, { r <- tryCatch(au_rule_tbl(), error = function(e) NULL); i <- input$au_rules_rows_selected; if (is.null(r)) return(); rv$au_rule <- if (length(i) == 1 && i <= nrow(r)) r$rule[i] else NULL; rv$detail <- NULL }, ignoreNULL = FALSE)
  output$au_explain <- renderUI({ k <- rv$au_rule; if (is.null(k)) return(NULL); i <- rule_info(k)
    div(class = "explain", tags$b(i$title), tags$br(), i$meaning, tags$br(), span(class = "dim", tags$b("What to do: "), i$action),
        if (nzchar(i$page) && !(VIEWER && i$page %in% c("Raw files", "Quality rules", "Stations", "Column names", "CR10X files", "Project"))) tagList(" ", actionLink("au_goto", paste0("Open ", if (i$page == "Metadata") "Loggers and files" else i$page)))) })
  observeEvent(input$au_goto, { i <- rule_info(rv$au_rule); if (nzchar(i$page)) go(i$page) })
  output$au_scope <- renderUI(if (is.null(rv$au_rule)) span(class = "dim", "Showing every kind of change.") else span(tags$b(rule_info(rv$au_rule)$title), "  ", actionLink("au_clear", "show all")))
  observeEvent(input$au_clear, { DT::selectRows(DT::dataTableProxy("au_rules"), NULL); rv$au_rule <- NULL })
  au_view <- reactive({
    a <- au_index()
    if (!is.null(rv$au_rule)) a <- a[rule == rv$au_rule]
    if (!input$au_station %in% c("All", "")) a <- a[who == input$au_station]
    if (!input$au_column %in% c("All", "")) a <- a[column == input$au_column]
    a
  })
  observe({ au_view(); rv$detail <- NULL })
  output$au_plot_ui <- renderUI({ a <- tryCatch(au_view(), error = function(e) NULL); if (is.null(a)) return(NULL); n <- min(25, uniqueN(a$who)); plotOutput("au_plot", height = paste0(round((120 + 26 * n) * psz()), "px")) })
  output$au_plot <- renderPlot({
    a <- au_view()[level != "file"]; validate(need(nrow(a) > 0, "Nothing to chart for this choice."))
    a[, cnt := as.numeric(ifelse(level == "cell", entries, n))]; i <- rule_info(a$rule); a[, what := i$title]
    top <- a[, .(t = sum(cnt, na.rm = TRUE)), by = who][order(-t)][seq_len(min(.N, 25))]; a <- a[who %in% top$who]
    b <- a[, .(cnt = sum(cnt, na.rm = TRUE)), by = .(who, what)]; b[, who := factor(who, levels = rev(top$who))]
    ggplot(b, aes(cnt, who, fill = what)) + geom_col(width = 0.75) + scale_fill_gruvbox(name = NULL) + scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(x = "records or values", y = NULL, subtitle = if (uniqueN(au_view()$who) > 25) "The 25 stations and loggers with the most entries" else NULL) + guides(fill = guide_legend(ncol = 2))
  }, bg = GB$bg, res = 96)
  output$au_count <- renderUI({ if (is.null(outs())) return(NULL); a <- au_view()
    p(class = "dim", num(sum(a$entries)), " log entries covering ", num(sum(as.numeric(a$n), na.rm = TRUE)), " records and values. One row per station, logger and column:") })
  output$au_tbl <- renderDT({
    a <- au_view(); validate(need(nrow(a) > 0, "Nothing matches this choice."))
    ovid_dt(a[order(-n), .(`Station or logger` = who, `What happened` = rule_info(rule)$title, Column = column, `Records or values` = ifelse(level == "cell", entries, n), First = first, Last = last, Table = stream, Stage = stage)],
          selection = "none", page = 12) |> formatRound("Records or values", 0)
  }, server = TRUE)
  output$au_dl <- downloadHandler(filename = function() paste0("audit_summary_", Sys.Date(), ".csv"), content = function(f) fwrite(au_view(), f))
  output$au_detail_ui <- renderUI({
    if (is.null(outs())) return(NULL); a <- au_view(); f <- unique(a$detail_file); n <- sum(a$entries)
    if (!length(f)) return(NULL)
    if (n > 300000) return(p(class = "dim", num(n), " entries match. Narrow the filters (one station or one rule) to list them one by one. They are all in ",
                             code("output/audit/detail/"), "."))
    div(class = "btnrow", actionButton("au_load", paste0("List the ", num(n), " matching entries"), icon = icon("list")),
        span(class = "dim", "Each entry shows the old value, the rule, the reason and the source file."))
  })
  observeEvent(input$au_load, {
    a <- au_view(); d <- withProgress(message = "Reading entries", value = 0.5, read_detail(pd(), unique(a$detail_file)))
    if (!nrow(d)) return(note(if (VIEWER) "Individual entries were not included in this viewer. The summary above is complete."
                              else "The detail files were not found. Run the pipeline again.", type = "warning"))
    key <- function(x) paste(x$stage, x$rule, x$station, x$logger_id, x$stream, x$column, sep = "\r")
    for (v in c("station", "logger_id", "stream", "column")) set(d, which(is.na(d[[v]])), v, "")
    a2 <- copy(a); for (v in c("station", "logger_id", "stream", "column")) set(a2, which(is.na(a2[[v]])), v, "")
    rv$detail <- d[key(d) %in% key(a2)]
  })
  output$au_detail <- renderDT({
    d <- rv$detail; req(d)
    ovid_dt(d[, .(`Station or logger` = fifelse(!is.na(station) & station != "", station, logger_id), Timestamp = timestamp, Column = column, `Old value` = old_value, `What happened` = rule_info(rule)$title,
                Reason = reason, `Source file` = basename(source_file), Records = n, Table = stream, `Record no.` = record)], selection = "none", page = 20)
  }, server = TRUE)
