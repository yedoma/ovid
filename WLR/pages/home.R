# WLR/pages/home.R  --  Home.
# Part of the WLR server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Home -------------------------------------------------------------------------------------------------------------
  output$raw_check <- renderUI({ d <- trim(strsplit(input$raw_dirs %||% "", "\n")[[1]]); d <- d[nzchar(d)]
    if (!length(d)) return(div(class = "check bad", "Add at least one folder."))
    ok <- dir.exists(path.expand(d)); if (!all(ok)) return(div(class = "check bad", "Not found: ", paste(d[!ok], collapse = ", ")))
    f <- raw_files_found(d)$rel                                                        # R/discover.R: the files a run reads
    div(class = "check good", icon("check"), sprintf(" %s .hobo, %s .xle, %s .csv or .txt files (%s in baro folders)", num(sum(grepl("hobo$", f, TRUE))), num(sum(grepl("xle$", f, TRUE))),
                                                   num(sum(grepl("(csv|txt)$", f, TRUE))), num(sum(grepl("(^|/)baro/", tolower(f)))))) })
  output$meta_check <- renderUI({ f <- find_metadata_file(input$metadata_dir %||% "")
    if (is.na(f)) div(class = "check", style = "opacity:.8", icon("circle-info"), " No workbook here. That is fine: loggers are processed under their serials, and the logger list (Loggers and metadata) can name them and give sites.")
    else div(class = "check good", icon("check"), " ", basename(f)) })
  output$stats <- renderUI({ o <- outs()
    if (is.null(o)) return(div(class = "panel-x", h3("Get started"),
      step(1, "Point the app at your data", p(class = "dim", if (embedded) "Under Import data: " else "On 1 Project: ", "the data folder (with year folders and baro/<year>), the metadata folder, the dGPS folder, and a project folder for the results.")),
      step(2, "Press Run", p(class = "dim", "Top right. Files are read, paired, stitched by serial, split into deployments and compensated. Baro groups are suggested on the first run.")),
      step(3, "Check the barologgers and hang lengths", p(class = "dim", "Confirm a primary and secondary barologger for each group on the Barometric page, fill in missing T_ST on the Set-up check, and run again."))))
    st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab)); ix <- o$index
    div(class = "stats", st(sum(o$files$use %in% TRUE), "files used"), st(nrow(o$loggers[kind != "baro"]), "water and EC loggers"), st(nrow(o$loggers[kind == "baro"]), "barologgers"),
        st(nrow(o$summary), "deployments"), st(uniqueN(o$summary$site), "sites"), st(sum(o$summary$records), "readings"),
        st(if (nrow(ix)) sum(ix[level == "cell", n]) else 0, "values removed")) })
  # the tape check's limit (tape_warn_m, a constant), as the run uses it (R/drift.R tape_offset_check)
  tape_lim <- function() CFG$tape_warn_m
  tape_cm <- function() paste(format(round(tape_lim() * 100, 2)), "cm")
  # what the last run found that needs a person, for the Needs you list (setup_flow.R): each with the page that settles it
  wl_notes <- reactive({ o <- outs(); if (is.null(o) || VIEWER) return(NULL); sm <- o$summary
    n <- c(bo = sum(sm$kind == "water" & sm$baro_missing > 0, na.rm = TRUE), tape = sum(abs(sm$tape_diff_median) > tape_lim(), na.rm = TRUE),
           rf = sum(!o$files$status %in% c("ok", "paired_hobo", "companion_csv", "duplicate_file", "temperature_logger")))
    lab <- c(bo = "deployments with readings no barologger covers", tape = sprintf("deployments where logger and tape differ by more than %s", tape_cm()), rf = "files not used")
    go <- c(bo = "Baro", tape = "Visits", rf = "Files"); k <- names(lab)[n[names(lab)] > 0]   # the tape table and Keep are on Manual levels and drift
    pr <- o$problems; pr <- if (!is.null(pr) && nrow(pr)) data.table(kind = "note", where = as.character(pr$where), what = as.character(pr$problem), detail = "the last run", go = "Metadata")
    rbindlist(list(pr, if (length(k)) data.table(kind = "note", where = num(n[k]), what = unname(lab[k]), detail = "", go = unname(go[k]))), fill = TRUE) })   # rbind() of two NULLs gave a 1 x 1 matrix
  home_rows <- reactive({ o <- outs(); req(o); o$summary[order(site, year), .(site, year, serial, kind, start, end, records, `T_ST (m)` = T_ST, reference, `depth median (m)` = depth_median,
                                                                            `tape diff (m)` = tape_diff_median, primary, deployment)] })
  output$home_dep <- renderDT(ovid_dt(home_rows()[, !"deployment"], selection = "single", page = 12))
  observeEvent(input$home_dep_rows_selected, { r <- home_rows()[input$home_dep_rows_selected]
    updateSelectInput(session, "ex_site", selected = r$site); go("Explore"); session$onFlushed(function() updateSelectInput(session, "ex_dep", selected = r$deployment), once = TRUE) })
  output$timeline <- renderPlot({ o <- outs(); validate(need(!is.null(o), "The coverage chart appears after the first run.")); plot_files(o$files) },
                                height = function() { o <- outs(); if (is.null(o)) 200 else 120 + 18 * uniqueN(o$files[!is.na(first_ts), paste(kind, serial)]) }, bg = NORD$bg)
