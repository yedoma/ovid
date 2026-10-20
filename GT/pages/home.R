# GT/pages/home.R  --  Home.
# Part of the GT server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, mark, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Home -------------------------------------------------------------------------------------------------------------
  output$raw_check <- renderUI({ d <- trim(strsplit(input$raw_dirs %||% "", "\n")[[1]]); d <- d[nzchar(d)]
    if (!length(d)) return(div(class = "check bad", "Add at least one folder."))
    ok <- dir.exists(path.expand(d)); if (!all(ok)) return(div(class = "check bad", "Not found: ", paste(d[!ok], collapse = ", ")))
    f <- unlist(lapply(path.expand(d), list.files, recursive = TRUE)); e <- tolower(tools::file_ext(f))
    div(class = "check good", icon("check"), sprintf(" %s files: %s .hex, %s .txt, %s .csv, %s .f2b, %s .hobo, %s .xls, %s .dat", num(length(f)), sum(e == "hex"), sum(e == "txt"), sum(e == "csv"),
                                                     sum(e == "f2b"), sum(e == "hobo"), sum(e %in% c("xls", "xlsx")), sum(e == "dat"))) })
  output$meta_check <- renderUI({ d <- input$metadata_dir %||% ""
    if (!nzchar(d)) return(div(class = "check", style = "opacity:.8", icon("circle-info"), " No metadata folder: that is fine, it is optional."))
    if (!dir.exists(path.expand(d))) return(div(class = "check bad", "Folder not found (it is optional: leave it blank to go without)."))
    # safe: looking for a registry; none found is a valid answer
    r <- tryCatch(find_registry(d), error = function(e) NULL)
    if (is.null(r)) div(class = "check", style = "opacity:.8", icon("circle-info"), " No registry here. That is fine: sites are named after their loggers, and the logger list (Sensors and depths) can name them and give sites and depths.")
    else div(class = "check good", icon("check"), " registry: ", basename(r$file), " / ", r$sheet, " (", nrow(r$data), " rows)") })
  output$stats <- renderUI({ o <- outs()
    if (is.null(o)) return(div(class = "panel-x", h3("Get started"),
      step(1, "Point the app at your data", p(class = "dim", "On 1 Project: the data folder, the metadata folder (registry workbook and field notes) and a working folder for the results.")),
      step(2, "Press Run pipeline", p(class = "dim", "Top right. Files are read and grouped by download, stitched by serial, given sites and depths (from the registry, else the default steps), checked and summarised.")),
      step(3, "Confirm sites, landcover and depths", p(class = "dim", "On the Sites and Sensors and depths pages. Assumed depths are marked everywhere until you confirm them. Then run again."))))
    st <- function(n, lab) div(class = "stat", tags$b(num(n)), span(lab))
    div(class = "stats", st(nrow(o$ss), "sites"), st(nrow(o$loggers), "loggers"), st(sum(o$files$use %in% TRUE), "files read"), st(sum(o$ss$values), "values kept"),
        st(if (nrow(o$index)) sum(o$index[rule %in% c("duplicate_value", "outside_deployment", "site_overlap"), n]) else 0, "copies and values outside a deployment dropped"), st(if (nrow(o$index)) sum(o$index[!rule %in% c("duplicate_value", "outside_deployment", "site_overlap"), n]) else 0, "values removed (audited)"), st(sum(o$bundles$status %in% c("needs_export", "unsupported")), "downloads need an export"),
        st(sum(o$health$health %in% c("failed: nothing usable", "failing: mostly errors")), "sensors failing")) })
  # what the last run found that needs a person, for the Needs you list (setup_flow.R): each with the page that settles it
  gt_notes <- reactive({ o <- outs(); if (is.null(o) || VIEWER) return(NULL)
    n <- c(rf = sum(o$bundles$status %in% c("needs_export", "unsupported")), he = sum(o$health$health %in% c("failed: nothing usable", "failing: mostly errors")), au = sum(!o$recon$reconciles))
    lab <- c(rf = "downloads that need an export (or cannot be read)", he = "sensors failed or failing", au = "logger sensors whose counts do not reconcile")
    go <- c(rf = "Files", he = "Health", au = "Audit"); k <- names(lab)[n[names(lab)] > 0]
    pr <- o$problems; pr <- if (!is.null(pr) && nrow(pr)) data.table(kind = "note", where = as.character(pr$where), what = as.character(pr$problem), detail = "the last run", go = "Preflight")
    rbind(pr, if (length(k)) data.table(kind = "note", where = num(n[k]), what = unname(lab[k]), detail = "", go = unname(go[k])), fill = TRUE) })
  home_rows <- reactive({ o <- outs(); req(o); o$ss[order(site), .(site, instrument, landcover, site_group, loggers, depths, `assumed depths` = assumed_depths, from = substr(first, 1, 10), to = substr(last, 1, 10), values, flagged)] })
  output$home_sites <- renderDT(ovid_dt(home_rows(), selection = "single", page = 12))
  observeEvent(input$home_sites_rows_selected, { r <- home_rows()[input$home_sites_rows_selected]; updateSelectInput(session, "ex_site", selected = r$site); go("Explore") })
  output$coverage <- renderPlot({ o <- outs(); validate(need(!is.null(o) && nrow(o$loggers), "The coverage chart appears after the first run.")); plot_coverage(o$loggers) },
                                height = function() { o <- outs(); if (is.null(o)) 200 else 110 + 24 * max(1, nrow(o$loggers)) }, bg = MI$bg)
