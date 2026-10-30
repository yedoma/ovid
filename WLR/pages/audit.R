# WLR/pages/audit.R  --  Audit.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Audit ------------------------------------------------------------------------------------------------------------------------
  output$au_cards <- renderUI({ o <- outs(); if (is.null(o)) return(empty_msg()); ix <- o$index; r <- o$recon
    st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab))
    div(class = "stats", st(sum(r$read), "readings read"), st(sum(r$written, na.rm = TRUE), "readings written"),
        st(if (nrow(ix)) sum(ix[level == "row", n]) else 0, "readings dropped (copies, outside deployments)"), st(if (nrow(ix)) sum(ix[level == "cell", n]) else 0, "values removed"),
        st(if (nrow(o$flags)) sum(o$flags$records) else 0, "values flagged (kept)"), st(if (all(r$ok)) "all" else sum(!r$ok), if (all(r$ok)) "loggers reconcile" else "loggers do not reconcile")) })
  au_rules <- reactive({ o <- outs(); req(o); ix <- o$index; if (!nrow(ix)) return(data.table())
    a <- ix[, .(n = sum(n, na.rm = TRUE), entries = sum(entries), loggers = uniqueN(logger_id)), by = .(rule, level)]
    a <- cbind(a, rule_info(a$rule)[, .(title, lost, meaning, action, page)]); a[order(-n)] })
  output$au_rules <- renderDT({ a <- au_rules(); validate(need(nrow(a) > 0, "Nothing was removed."))
    x <- a[, .(What = title, Lost = sprintf('<span class="d-flag %s">%s</span>', c(no = "ok", yes = "error", kept = "warn", file = "off")[lost], c(no = "nothing lost", yes = "removed", kept = "kept, changed or flagged", file = "file not used")[lost]), Count = num(n), Level = level, Loggers = loggers)]
    ovid_dt(x, escape = FALSE, selection = "single", page = 12) })
  output$au_explain <- renderUI({ s <- input$au_rules_rows_selected; if (!length(s)) return(NULL); a <- au_rules()[s]
    div(class = "explain", tags$b(a$title), tags$br(), a$meaning, tags$br(), tags$b("What to do: "), a$action) })
  output$au_where <- renderDT({ o <- outs(); req(o); ix <- o$index; validate(need(nrow(ix) > 0, "Nothing to show."))
    s <- input$au_rules_rows_selected; if (length(s)) ix <- ix[rule == au_rules()$rule[s]]
    ovid_dt(ix[order(-n), .(rule, level, site, logger_id, column, n, first, last, detail_file)], page = 12) })
  output$au_recon <- renderDT({ o <- outs(); req(o); ovid_dt(o$recon, page = 12) %>% formatStyle("ok", color = styleEqual(c(TRUE, FALSE), c(NORD$green, NORD$red))) })

  au_fdata <- reactive({ o <- outs(); req(o); f <- o$fileaudit; if (!nrow(f)) return(f)
    switch(input$au_fshow %||% "used", used = f[used %in% TRUE], unused = f[!used %in% TRUE], bad = f[used %in% TRUE & !reconciles %in% TRUE], f) })
  output$au_files <- renderDT({ f <- au_fdata(); validate(need(nrow(f) > 0, "No files in this view."))
    ovid_dt(f[order(folder_year, serial, name), .(name, folder_year, kind, format, serial, status, used, readings_read, in_output, dropped, dropped_why, values_removed, reconciles, used_in, what_happened, md5, first_ts, last_ts)], page = 12) %>%
      formatStyle("reconciles", color = styleEqual(c(TRUE, FALSE), c(NORD$green, NORD$red))) })
  output$au_fdl <- downloadHandler(function() "file_audit.csv", function(file) { fa <- outs()$fileaudit; if (is.null(fa)) { showNotification("No file audit yet: run the app first.", type = "warning"); fa <- data.table(note = "No file audit yet.") }   # a note, not a failed download
    fwrite(fa, file, dateTimeAs = "write.csv") })
