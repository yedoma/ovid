# GT/pages/audit.R  --  Audit.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Audit ---------------------------------------------------------------------------------------------------------------
  output$au_cards <- renderUI({ o <- outs(); req(o); r <- o$recon; st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab))
    div(class = "stats", st(sum(r$values_read), "values read"), st(sum(r$values_out), "kept"), st(sum(r$removed), "removed"),
        div(class = "stat", tags$b(if (all(r$reconciles)) icon("check") else icon("xmark"), style = paste0("color:", if (all(r$reconciles)) MI$green else MI$red)),
            span(if (all(r$reconciles)) "every logger reconciles" else "does not reconcile")),
        if (nrow(o$fa)) { ok <- all(o$fa$reconciles %in% c(TRUE, NA)); div(class = "stat", tags$b(if (ok) icon("check") else icon("xmark"), style = paste0("color:", if (ok) MI$green else MI$red)),
            span(if (ok) paste("all", sum(o$fa$used), "files read add up") else paste(sum(o$fa$reconciles %in% FALSE), "files do not add up"))) }) })
  au_rules <- reactive({ o <- outs(); req(o); x <- o$index[, .(values = sum(n)), by = .(stage, rule)][order(-values)]; cbind(x, rule_info(x$rule)[, .(what = title, lost)]) })
  output$au_rules <- renderDT(ovid_dt(au_rules(), selection = "single", page = 20) |> ovid_dt_levels("lost", c(yes = "error", no = "ok", kept = "warn"), MI))
  output$au_explain <- renderUI({ s <- input$au_rules_rows_selected; if (!length(s)) return(NULL); r <- rule_info(au_rules()$rule[s])
    div(class = "explain", tags$b(r$title), tags$br(), r$meaning, tags$br(), span(class = "dim", r$action)) })
  output$au_where <- renderDT({ o <- outs(); req(o); s <- input$au_rules_rows_selected; x <- o$index; if (length(s)) x <- x[rule == au_rules()$rule[s]]
    ovid_dt(x[, .(values = sum(n)), by = .(rule, site, logger_id, channel, first, last)][order(-values)], page = 12) })
  output$au_recon <- renderDT({ o <- outs(); req(o); ovid_dt(o$recon[order(reconciles, logger_id, channel)], page = 12) |> formatStyle("reconciles", color = styleEqual(c(TRUE, FALSE), c(MI$green, MI$red))) })
  output$au_files <- renderDT({ o <- outs(); req(o)
    if (!nrow(o$fa)) return(ovid_dt(o$files[, .(file = rel, status, read = use, values_read, removed, md5, size, mtime)], page = 12) |> status_style("status"))   # output from before the per-file audit
    f <- o$fa[order(!used, type, serial, first_ts)]
    ovid_dt(f[, .(file, bundle, status, values_read = read, in_output, dropped, dropped_why, reconciles, used_in, what_happened, md5, first_ts, last_ts)], page = 12) |> status_style("status") |>
      formatStyle("reconciles", color = styleEqual(c(TRUE, FALSE), c(MI$green, MI$red))) })
  output$au_bundles <- renderDT({ o <- outs(); req(o); b <- o$bundles; validate(need(nrow(b) && "reconciles" %in% names(b), "Run the pipeline to add this table."))
    ovid_dt(b[order(status != "ok", type, serial), .(bundle, status, type, serial, folder, read_from = source_file, values_read, values_out, dropped, reconciles, source_md5, members)], page = 12) |>
      status_style("status") |> formatStyle("reconciles", color = styleEqual(c(TRUE, FALSE), c(MI$green, MI$red))) })
