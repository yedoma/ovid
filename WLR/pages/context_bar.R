# WLR/pages/context_bar.R  --  Context bar: what is on screen.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Context bar: what is on screen, after the page name the shell shows ------------------------------------------------
  output$d_context <- renderUI({ o <- outs(); pg <- input$nav
    if (identical(pg, "Explore")) { t <- input$ex_tab; req(t)
      dep <- if (identical(input$ex_dep, "all")) "every year" else if (!is.null(o) && isTRUE(nzchar(input$ex_dep))) {
        r <- o$summary[deployment == input$ex_dep]; if (nrow(r)) paste0(r$year[1], ", serial ", r$serial[1]) }
      return(ovid_context(t, switch(t, "One site" = c(input$ex_site, dep), "Compare sites" = if (length(input$cs_sites)) paste(length(input$cs_sites), "sites"),
                                      "Compare years" = input$cy_site, NULL))) }
    switch(pg %||% "", Baro = ovid_context(input$bo_year), NULL) })
