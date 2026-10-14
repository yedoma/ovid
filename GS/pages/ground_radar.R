# GS/pages/ground_radar.R  --  GPR: the folders searched for lines and the lines found (on the GPR Map), and the processing
# screen (pages/mod_gpr_flow.R) with the GPR Map that shares its line.
# Part of the GS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, proj, E, mark, cat_s, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- GPR (R/gpr.R) ----
  gpr_dirs <- reactive({ d <- trim(strsplit(input$gpr_dirs %||% "", "\n")[[1]]); d[nzchar(d)] })
  gpr <- reactive({ input$gpr_rescan; d <- gpr_dirs(); if (length(d)) gpr_scan(d) else data.table() })
  gpr_ok <- reactive({ g <- gpr(); if (nrow(g)) g[status %in% c("ok", "truncated")] else g })
  settings_autosave("gpr_dirs", save = function() { SETTINGS <<- utils::modifyList(SETTINGS, list(gpr_dirs = gpr_dirs())); settings_save(SETTINGS_FILE, SETTINGS["gpr_dirs"]) })
  output$gpr_sum <- renderUI({ g <- gpr(); if (!nrow(g)) return(span(class = "muted", "No .rad files found in these folders."))
    v <- g[status %in% c("ok", "truncated")]
    span(class = "muted", sprintf("%d lines found; %d can be viewed (%d from unrenamed .partial copies, %d truncated), %d with GPS, %d with markers. %s", nrow(g), nrow(v), sum(v$partial), sum(v$status == "truncated"),
         sum(!is.na(v$cor)), sum(!is.na(v$mrk)), { w <- g[!status %in% c("ok", "truncated"), .N, by = status]; if (nrow(w)) paste0("Not viewable: ", paste(w$N, w$status, collapse = "; "), ".") else "" })) })
  gpr_tick <- reactiveVal(0L)                                                # a flow, marker or pick was saved
  gpr_tracks <- reactive({ g <- gpr_ok()[!is.na(cor)]; setNames(lapply(g$cor, read_cor), g$line) })
  # GPR processing, one screen for both interfaces (pages/mod_gpr_flow.R), and the GPR Map that shares its line and radargram
  gflow <- mod_gpr_flow_server("gprflow", gpr_ok, gpr_tracks, proj, gpr_tick)
  mod_gpr_map_server("gprmap", lines = gpr_ok, tracks = gpr_tracks, selected = gflow$line, markers = gflow$markers,
    flows = reactive({ gpr_tick(); g <- gpr_ok(); setNames(vapply(g$line, function(k) length(gpr_flow_history(proj(), k)), 0L), g$line) }),
    radargram = gflow$radargram, select_line = gflow$select_line, open_processing = function() ovid_go(session, "GPR processing"))
  output$gpr_empty <- renderUI({ if (nrow(gpr_ok())) return(NULL)                     # the GPR side with nothing yet (core areas.js: data-area-go)
    div(class = "d-notice info mt-3", style = "max-width:720px", h3("No GPR lines scanned yet"), p("Add folders with .rad and .rd3 files; they are read, never changed."),
        tags$a(class = "btn btn-primary btn-sm", href = "#gpr-folders", "Add folders to scan"),
        tags$a(class = "btn btn-outline-secondary btn-sm ms-2", href = "#", `data-area-go` = "ERT", "Go to ERT")) })
  output$gpr_table <- renderDT({ g <- gpr(); validate(need(nrow(g) > 0, "Nothing found."))
    ovid_long_table(g[, .(Line = line, Status = status, Traces = traces, Samples = samples, `Window (ns)` = round(timewindow_ns, 1), Antennas = antennas, `Matched by` = matched_by,
                    Note = note, Header = rad, Data = data, GPS = cor, Markers = mrk)], selection = "none", height = "40vh") })   # core longtable.R
