# CS/pages/explore_neighbours.R  --  Explore: against the neighbours.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore: against the neighbours -----------------------------------
  output$hb_tbl <- renderDT({ o <- outs(); req(o); x <- o$breaks %||% data.table(); validate(need(nrow(x) > 0, "Nothing to test: needs the between-station check, three stations and two years of the same parameter."))
    ovid_dt(x[order(result != "break against the other stations", p)], page = 12) })
  output$he_tbl <- renderDT({ o <- outs(); req(o); x <- o$estimated %||% data.table(); validate(need(nrow(x) > 0, "No daily mean was estimated in the last run."))
    ovid_dt(x, page = 12) })
