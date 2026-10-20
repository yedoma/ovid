# RS/pages/sources.R  --  Sources and the viewer.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sources and the viewer ----
  # each source with its status, drone photos and flights, and Remove with a confirm in its row (the folder is never touched)
  src_counts <- reactive({ C(); rv$tick
    im <- tryCatch(fread(file.path(CAT(), "images.csv"), select = c("source_id", "lat")), error = function(e) data.table(source_id = character(), lat = numeric()))
    # safe: an output of the last scan, only shown
    rd0 <- function(f) { p <- file.path(CAT(), f); if (file.exists(p)) tryCatch(fread(p, colClasses = "character"), error = function(e) data.table()) else data.table() }
    list(photos = im[, .N, by = source_id], positioned = sum(is.finite(im$lat)), n_photos = nrow(im), other = rd0("other_photos.csv"), dup = rd0("duplicates.csv")) })
  rm_ask <- reactiveVal("")
  observeEvent(input$s_rm_ask, rm_ask(input$s_rm_ask %||% ""))
  observeEvent(input$s_rm_do, { sid <- input$s_rm_do; rm_ask("")
    if (run_busy()) return(showNotification("Wait for the scan or cancel it", type = "warning"))
    r <- tryCatch(remove_source(CFG$project_dir, sid), error = function(e) { showNotification(paste("Not removed:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(r)) { reload(); showNotification(sprintf("Source %s removed: %d flight(s) left the catalogue; its files are as they were", sid, r$flights), type = "message") } })
  # safe: the sources table's editor reports a read problem
  output$src_list <- renderUI({ live_stamp(cfg_path(CFG$project_dir, "sources")); s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())
    if (!nrow(s)) return(p(class = "muted", "No source yet: add a folder or drive below and scan it."))
    sc <- if (is.null(C())) data.table() else C()$sources; fl <- if (is.null(C())) data.table() else C()$flights; k <- src_counts()$photos
    js <- function(id, sid) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", id, jsonlite::toJSON(sid, auto_unbox = TRUE))
    np <- normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE)
    tagList(lapply(seq_len(nrow(s)), function(i) { sid <- s$source_id[i]; scanned <- nrow(sc) && sid %in% sc$source_id
      n_ph <- k[source_id == sid, N] %||% 0L; n_fl <- if (nrow(fl) && "source_id" %in% names(fl)) sum(fl$source_id == sid) else 0L
      inside <- s$source_id[-i][startsWith(np[i], paste0(np[-i], "/"))]; holds <- sum(startsWith(np[-i], paste0(np[i], "/")))
      when <- if (scanned) paste("scanned", substr(sc[source_id == sid, last_scanned][1], 1, 10)) else "never finished"
      meta <- paste(c(when, if (scanned) c(paste(num(sum(n_ph)), "drone photos"), paste(num(n_fl), "flights")), if (length(inside)) paste("inside", inside[which.max(nchar(np[-i][startsWith(np[i], paste0(np[-i], "/"))]))]),
                      if (holds) paste("contains", holds, "other source(s)"),
                      if (!dir.exists(path.expand(s$path[i]))) "not plugged in: keeps its last scan"), collapse = " \u00b7 ")
      div(class = "src-row", style = "display:grid;grid-template-columns:minmax(0,1fr) auto auto;gap:12px;align-items:center;padding:8px 0;border-top:1px solid var(--bs-border-color)",
        div(div(style = "font-family:var(--bs-font-monospace);font-size:12.5px;overflow-wrap:anywhere", s$path[i]), div(class = "muted", style = "font-size:12px", paste0(sid, ": ", meta))),
        span(class = "badge-st", style = sprintf("border-color:%s;color:%s", if (scanned) PAL$green else PAL$gold, if (scanned) PAL$green else PAL$gold), if (scanned) "scanned" else "not scanned"),
        tags$button(type = "button", class = "btn btn-sm btn-outline-danger", `aria-expanded` = tolower(identical(rm_ask(), sid)), onclick = js("s_rm_ask", if (identical(rm_ask(), sid)) "" else sid), "Remove"),
        if (identical(rm_ask(), sid)) div(style = sprintf("grid-column:1/-1;border:1px solid %s;padding:10px;display:flex;gap:10px;flex-wrap:wrap;align-items:center", PAL$red),
          span(sprintf("Remove this source? Its %s catalogued flight(s) leave the catalogue; the folder and its files stay as they are. You can add it back and scan again.", num(n_fl))),
          tags$button(type = "button", class = "btn btn-sm btn-danger", onclick = js("s_rm_do", sid), "Remove source"),
          tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", onclick = js("s_rm_ask", ""), "Keep it"))) })) })
  # after a scan: drone photos read, other photos left out (and why), flights, and flights placed from photo geotags
  output$src_scan <- renderUI({ k <- src_counts(); fl <- if (is.null(C())) data.table() else C()$flights
    if (!k$n_photos && !nrow(k$other)) return(p(class = "muted", "Nothing scanned yet."))
    geo <- if (nrow(fl) && "placed_from" %in% names(fl)) sum(fl$placed_from %in% "photo geotags") else 0L
    st <- function(v, l) div(style = "padding:10px;border:1px solid var(--bs-border-color)", tags$b(style = "display:block;font-size:20px;font-weight:500", num(v)), span(class = "muted", style = "font-size:12px", l))
    nd <- if (nrow(k$dup) && "kind" %in% names(k$dup)) sum(k$dup$kind == "photo") else 0L          # photo copies, out of the photos read
    nl <- if (nrow(k$dup) && "kind" %in% names(k$dup)) sum(k$dup$kind == "flight log") else 0L     # eMotion log copies and versions
    tagList(div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:6px",
                st(k$n_photos - nd, "drone photos, every one read"), st(nrow(k$other), "other photos left out"), st(nd + nl, if (nl) sprintf("duplicates counted once (%s flight logs)", num(nl)) else "duplicates counted once"),
                st(nrow(fl), "flights"), st(geo, "flights placed from photo geotags")),
            p(class = "muted mt-2", sprintf("%s of the drone photos have a position. Other photos left out: no drone tag and a camera not in drone_cameras.csv (Set-up tables).", num(k$positioned)))) })
  output$s_dup <- renderDT({ x <- src_counts()$dup; if (!nrow(x)) x <- data.table(kind = character(), kept_path = character(), dup_path = character(), why = character())
    ovid_dt(x[, intersect(c("kind", "kept_path", "dup_path", "why"), names(x)), with = FALSE], selection = "none", page = 10) })
  output$s_other <- renderDT({ x <- src_counts()$other; if (!nrow(x)) x <- data.table(path = character(), make = character(), model = character(), why = character())
    ovid_dt(x[, intersect(c("path", "make", "model", "why"), names(x)), with = FALSE], selection = "none", page = 10) })
  # Scans run in the background (core runner.R): a separate R process, the progress box at the bottom right and Cancel,
  # which stops the scan at its next folder. A stopped scan leaves each source its last good scan.
  # done: the message when it finishes; then(r): anything more to do after a finished run (the viewer's download).
  scan_done <- function(what, done = paste(what, "done"), then = NULL) function(r) {
    for (sdir in Sys.glob(file.path(path.expand(CFG$project_dir), "cache", "sources", "*"))) unlink(file.path(sdir, "scan.rds.part"))
    reload()
    if (r$status == "finished") { showNotification(done, type = "message"); if (is.function(then)) then(r) }
    else if (r$status == "cancelled") showNotification(paste(what, "stopped; each source keeps its last good scan"), type = "warning")
    else showNotification(paste(what, "failed:", r$message), type = "error", duration = NULL) }
  # Every run (scan, rescan, rebuild, viewer) starts here. The set-up check comes first: with errors, a question (review,
  # run anyway, cancel); a run anyway is logged.
  pending <- reactiveVal(NULL)
  start_scan <- function(what, args, force = FALSE, done = paste(what, "done"), then = NULL, verb = "Scan") {
    e <- tryCatch(setup_check(CFG), error = function(e) { showNotification(paste("The set-up check could not run:", conditionMessage(e)), type = "warning"); NULL }); e <- if (is.null(e)) NULL else e[level == "error"]
    if (!force && !is.null(e) && nrow(e)) { pending(list(what = what, args = args, done = done, then = then))
      return(showModal(setup_gate(e, "scan_review", "scan_force", verb))) }
    cfg <- CFG; if (force) cfg$run_anyway <- setup_problem_lines(e)
    run_start(session, cfg, scan_done(what, done, then), label = what, args = args) }
  observeEvent(input$scan_force, { removeModal(); x <- pending(); if (!is.null(x)) start_scan(x$what, x$args, force = TRUE, done = x$done, then = x$then) })
  observeEvent(input$scan_review, { removeModal(); ovid_go(session, "Setup") })
  # a folder inside a source, or holding one, is said so before it is scanned (each file then belongs to the innermost source)
  scan_new <- function(p) { start_scan(paste("Scanning", basename(p)), list(scan = p, source_id = trim(input$s_id))); updateTextInput(session, "s_id", value = "") }
  scan_pending <- reactiveVal(NULL)
  observeEvent(input$s_scan, { p <- trim(input$s_path); if (!nzchar(p) || !dir.exists(path.expand(p))) return(showNotification("That folder was not found", type = "error"))
    # safe: the sources table's editor reports a read problem
    p <- normalizePath(path.expand(p), winslash = "/"); s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())
    np <- if (nrow(s)) normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE) else character()
    if (p %in% np) return(scan_new(p))
    inn <- s$source_id[startsWith(p, paste0(np, "/"))]; has <- s$source_id[startsWith(np, paste0(p, "/"))]
    if (!length(inn) && !length(has)) return(scan_new(p))
    scan_pending(p)
    showModal(modalDialog(title = "This folder overlaps a source",
      if (length(inn)) p(sprintf("It is inside %s. Its files will be counted under the new source only, and left out of %s.", paste(inn, collapse = ", "), paste(inn, collapse = ", "))),
      if (length(has)) p(sprintf("It contains %s. Those folders' files stay with %s.", paste(has, collapse = ", "), paste(has, collapse = ", "))),
      p(class = "muted", "No file is counted twice, and no file is changed."),
      footer = tagList(modalButton("Cancel"), actionButton("s_scan_go", "Scan", class = "btn-primary")))) })
  observeEvent(input$s_scan_go, { removeModal(); p <- scan_pending(); scan_pending(NULL); if (!is.null(p)) scan_new(p) })
  observeEvent(input$s_rescan, start_scan("Rescanning drives", list()))
  observeEvent(input$s_build, start_scan("Rebuilding", list(scan = character()), done = "Catalogue rebuilt from the sources' last scans; no source was read.", verb = "Rebuild"))
