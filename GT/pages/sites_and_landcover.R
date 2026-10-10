# GT/pages/sites_and_landcover.R  --  Sites and landcover.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sites and landcover -------------------------------------------------------------------------------------------------
  observeEvent(input$st_apply, { s <- input$st_sites; if (!length(s)) return(note("Pick sites first.", type = "warning"))
    d <- as.data.table(m_site$data()); req(d); miss <- setdiff(s, d$site)
    if (length(miss)) d <- rbind(d, ensure_cols(data.table(site = miss), SITE_COLS)[, names(d), with = FALSE], fill = TRUE)
    d[site %in% s, landcover := input$st_lc]; if (nzchar(trim(input$st_group))) d[site %in% s, site_group := trim(input$st_group)]
    if (m_site$replace(d)) { mark(); note("Landcover set for ", length(s), " site(s).") } })
