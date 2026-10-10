# RS/pages/gnss_files.R  --  GNSS: stitch uploaded survey files.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- GNSS: stitch uploaded survey files, show them, export one CSV ----
  G <- reactiveVal(list(files = character(), names = character()))
  observeEvent(input$g_files, { f <- input$g_files; g <- G()
    keep <- file.path(tempdir(), "gnss_in"); dir.create(keep, showWarnings = FALSE)
    to <- file.path(keep, paste0(length(g$files) + seq_len(nrow(f)), "_", f$name)); file.copy(f$datapath, to, overwrite = TRUE)
    G(list(files = c(g$files, to), names = c(g$names, f$name))) })
  observeEvent(input$g_clear, G(list(files = character(), names = character())))
  gd <- reactive({ g <- G(); if (!length(g$files)) return(NULL); stitch_gnss(g$files, g$names, zone = input$g_zone) })
  output$g_msgs <- renderUI({ d <- gd(); if (is.null(d)) return(NULL)
    tagList(p(strong(nrow(d), "points"), " from ", length(unique(d$source_file)), " files: ", paste(d[, .N, by = device][, paste(N, device)], collapse = ", ")),
      lapply(attr(d, "problems"), function(x) p(class = "text-warning", x)),
      if (any(nzchar(d$note))) p(class = "text-warning", sum(nzchar(d$note)), " rows reuse a point name with other coordinates (see the note column)")) })
  # the base of each file (config/gnss_bases.csv) and its check; the full message shows on hover and goes into the export
  gb <- reactive({ d <- gd(); live_stamp(cfg_path(CFG$project_dir, "gnss_bases")); live_stamp(cfg_path(CFG$project_dir, "calibration"))
    if (is.null(d) || !nrow(d)) return(data.table()); cf <- tryCatch(load_configs(CFG), error = function(e) NULL); gnss_base_check(d, cf$gnss_bases, cf$calibration) })
  output$g_quality <- renderDT({ d <- gd(); if (is.null(d) || !nrow(d)) return(ovid_dt(data.table()))
    cf <- tryCatch(load_configs(CFG), error = function(e) NULL); q <- gnss_quality(d)
    q <- merge(merge(q, gnss_limit_check(q, cf$calibration, cf$gnss_bases), by = "source_file", all.x = TRUE, sort = FALSE), gb(), by = "source_file", all.x = TRUE, sort = FALSE)
    q[, base_post_processed := fifelse(base_post_processed %in% c("yes", "y", "true"), paste0("yes", fifelse(nzchar(base_service), paste0(" (", base_service, ")"), "")), fifelse(nzchar(base_post_processed), base_post_processed, "\u2013"))]
    q[, base_check := sprintf('<span title="%s">%s</span>', htmltools::htmlEscape(base_note, attribute = TRUE),
                              fifelse(base_check == "flag", "\u26a0 flag", base_check))][, c("base_service", "base_note") := NULL]
    ovid_dt(q, selection = "none", escape = setdiff(names(q), "base_check"), page = 50, opts = list(dom = "t")) })
  output$g_table <- renderDT({ d <- gd(); if (is.null(d)) d <- data.table(); ovid_dt(d, selection = "none", page = 15) })
  observe({ d <- gd(); p <- if (is.null(d)) data.table() else d[is.finite(lat) & is.finite(lon)]
    q <- distinct_cols(c(FIX = PAL$green, FLOAT = PAL$gold, SINGLE = PAL$red), PAL)     # told apart with colour blindness too (core/theme.R)
    cols <- c(q, `50` = q[["FIX"]], `16` = q[["SINGLE"]], `34` = q[["FLOAT"]])
    msg <- list(pts = if (nrow(p)) p[, .(lat, lon, label = paste0(point, " \u00b7 ", device, " \u00b7 ", solution, " \u00b7 ", source_file),
                                                                        col = unname(ifelse(toupper(solution) %in% names(cols), cols[toupper(solution)], PAL$teal)))] else list())
    key <- c(`Fix` = PAL$green, `Float` = PAL$gold, `Single (no correction)` = PAL$red, `Other or not stated` = PAL$teal)
    msg$key <- if (nrow(p)) as.list(key[key %in% msg$pts$col]) else list()
    session$sendCustomMessage("gnssmap", msg); session$sendCustomMessage("rsgnss", msg) })
  output$g_dl <- ovid_download("RS", "gnss stitched", "csv", function(file) {
    d <- gd(); if (is.null(d)) d <- ensure_cols(data.table(), GNSS_COLS)[0] else if (nrow(d)) d <- merge(d, gb(), by = "source_file", all.x = TRUE, sort = FALSE)[, c(GNSS_COLS, setdiff(names(gb()), "source_file")), with = FALSE]
    write_gnss(d, file); write_gnss(d, file.path(path.expand(CFG$project_dir), "output", "gnss", basename(file))) })
