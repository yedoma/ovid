# CS/pages/history.R  --  History.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- History --------------------------------------------------------------
  hi_log <- reactive({ rv$tick; rv$hist; for (m in E) m$data(); hist_log(pd()) })
  output$hi_tbl <- renderDT({ d <- hi_log(); validate(need(nrow(d) > 0, "No changes have been saved yet. Changes to stations, column names, rules and the other set-up tables appear here."))
    ovid_dt(d[, .(When = time, Who = user, Table = table, `Rows before` = rows_before, `Rows after` = rows_after, `Added or changed` = rows_added_or_changed, `Removed or changed` = rows_removed_or_changed)],
          selection = "single", page = 15) })
  hi_sel <- reactive({ d <- hi_log(); s <- input$hi_tbl_rows_selected; if (!length(s) || s > nrow(d)) NULL else d[s] })
  output$hi_detail <- renderUI({ r <- hi_sel(); if (is.null(r)) return(NULL)
    tb <- sub(" \\(.*$", "", r$table); cur <- file.path(pd(), "config", tb); bk <- file.path(pd(), "config", "history", r$backup)
    if (!file.exists(bk)) return(div(class = "explain", "The saved version for this change is no longer in config/history."))
    a <- as.data.frame(read_cfg(bk)); b <- if (file.exists(cur)) as.data.frame(read_cfg(cur)) else a[0, ]
    key <- function(d) if (!nrow(d)) character() else do.call(paste, c(lapply(d, as.character), sep = "\r"))
    only_old <- a[!key(a) %in% key(b), , drop = FALSE]; only_new <- b[!key(b) %in% key(a), , drop = FALSE]
    tbl <- function(d) if (!nrow(d)) p(class = "dim", "none") else div(class = "scrollbox", HTML(html_table(as.data.table(d), 30)))
    div(class = "brushbox", h4(tb, " before the change of ", r$time), p(class = "dim", "Compared with the table as it is now."),
        tags$b("Rows in that version that are not in the current table"), tbl(only_old), tags$b("Rows in the current table that were not in that version"), tbl(only_new),
        div(class = "btnrow", actionButton("hi_restore", "Go back to that version", icon = icon("rotate-left"), class = "btn-danger"),
            span(class = "dim", "The current table is saved to the history first, so this can itself be undone."))) })
  observeEvent(input$hi_restore, { r <- hi_sel(); req(r); if (VIEWER) return()
    req(hist_restore(pd(), r, current_user(session)))
    rv$tick <- rv$tick + 1; mark(); note(sub(" \\(.*$", "", r$table), " restored. Run the pipeline to apply.") })

