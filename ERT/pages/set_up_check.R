# ERT/pages/set_up_check.R  --  Set-up check and file audit.
# Part of the ERT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, proj, E, mark, cat_s, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- set-up check and file audit (Ovid/preflight.R, Ovid/file_audit.R) ----
  chk <- reactive({ input$chk_run; cat_s(); live_config(proj()); setup_check(CFG) })
  output$chk_sum <- renderUI({ x <- chk(); st <- check_status(x)
    ovid_status(if (st == "ok") "The set-up looks right" else sprintf("%d thing(s) to look at", sum(x$level != "info")), c(ok = "ok", warning = "warn", error = "error")[[st]]) })
  output$chk_tbl <- renderDT(ovid_dt(chk()[, .(Level = level, Area = area, Item = item, Problem = problem, Fix = fix)], selection = "none", page = 8,
                                       opts = list(dom = "tp", language = list(emptyTable = "Nothing to fix"))))
  output$fa_dl <- ovid_download("ERT", "file audit", "csv", function(file) {
    f <- file.path(path.expand(CFG$project_dir), "output", "audit", "file_audit.csv"); if (!file.exists(f)) stop("Scan the archive first"); file.copy(f, file) })
  cat_s <- reactiveVal(load_catalogue())
  ELEV(load_configs(CFG)$elevations)
  qc <- reactiveVal(load_configs(CFG)$qc)
  # The profile the catalogue reports: the one a survey (s), or most of the catalogue, was scanned with; before any scan, the
  # one picked on Settings (R/qc.R run_profiles). Call it inside reactive code (it reads cat_s and qc).
  cat_profile <- function(s = NULL) { p <- stack_blank(if (is.null(s)) cat_s()$qc_profile else s$qc_profile); p <- p[nzchar(p)]
    if (length(p)) names(sort(table(p), decreasing = TRUE))[1] else run_profiles(CFG, qc()$profile)[1] }
  # Settings > QC profile of the catalogue: any profile in qc_profiles.csv, kept in app_settings.json; the next scan uses it
  observe({ ch <- run_profiles(list(qc_profiles = CFG$qc_profiles), qc()$profile)
    updateSelectInput(session, "set_cat_profile", choices = stats::setNames(ch, gsub("_", " ", ch)), selected = run_profiles(CFG, qc()$profile)[1]) })
  settings_autosave("set_cat_profile", keys = c(set_cat_profile = "catalogue_profile"), save = function() {
    CFG$catalogue_profile <<- input$set_cat_profile %||% ""
    SETTINGS <<- utils::modifyList(SETTINGS, list(catalogue_profile = CFG$catalogue_profile)); settings_save(SETTINGS_FILE, SETTINGS["catalogue_profile"]) })
  # the context bar: what is on screen, after the page name the shell shows
  output$d_context <- renderUI({ s <- cat_s(); nm <- function(id) s$name[match(id, s$survey_id)]
    switch(input$nav %||% "", Survey = ovid_context(nm(input$survey), input$view_tab), QC = ovid_context(nm(input$qc_survey), input$profile),
           Map = ovid_context(if (length(input$map_years)) paste(input$map_years, collapse = ", ") else "all years"),
           Stack = ovid_context(input[["stack-project"]]),
           `GPR processing` = ovid_context(input[["gprflow-line"]], { v <- input[["gprflow-ver"]] %||% ""; if (nzchar(v)) paste0("flow v", v) }), NULL) })

  observe({ s <- cat_s()
    o <- s[n_read > 0]; ch <- if (!nrow(o)) character() else setNames(o$survey_id, paste0(o$date, "  ", o$name, ifelse(o$kind == "original", "", paste0("  (", o$kind, ")"))))
    keep <- function(x) if (length(x) && nzchar(x) && x %in% ch) x else ch[1]
    isolate({ updateSelectInput(session, "survey", choices = ch, selected = keep(input$survey)); updateSelectInput(session, "qc_survey", choices = ch, selected = keep(input$qc_survey)) })
  })
  observe(updateSelectInput(session, "profile", choices = unique(qc()$profile), selected = { p <- isolate(input$profile); if (length(p) && nzchar(p)) p else cat_profile() }))
