# RS/pages/sources.R  --  Sources and the viewer.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Sources and the viewer ----
  # which of dirs the folder x is inside (none when there are no dirs: paste0(character(), "/") is "/", which every path starts with)
  in_dirs <- function(x, dirs) if (length(dirs)) startsWith(x, paste0(dirs, "/")) else logical()
  # each source with its status, drone photos and flights, and Remove with a confirm in its row (the folder is never touched)
  src_counts <- reactive({ C(); rv$tick
    im <- tryCatch(fread(file = file.path(CAT(), "images.csv"), select = c("source_id", "lat")), error = function(e) data.table(source_id = character(), lat = numeric()))
    # safe: an output of the last scan, only shown
    rd0 <- function(f) { p <- file.path(CAT(), f); if (file.exists(p)) tryCatch(fread(file = p, colClasses = "character"), error = function(e) data.table()) else data.table() }
    list(photos = im[, .N, by = source_id], positioned = sum(is.finite(im$lat)), n_photos = nrow(im), other = rd0("other_photos.csv"), dup = rd0("duplicates.csv")) })
  rm_ask <- reactiveVal("")
  observeEvent(input$s_rm_ask, rm_ask(input$s_rm_ask %||% ""))
  observeEvent(input$s_rm_do, { sid <- input$s_rm_do; rm_ask("")
    if (run_busy()) return(showNotification("Wait for the scan or cancel it", type = "warning"))
    r <- tryCatch(remove_source(CFG$project_dir, sid), error = function(e) { showNotification(paste("Not removed:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(r)) { reload(); showNotification(sprintf("Source %s removed%s; its files are as they were", sid, if (!r$flights) "" else if (r$flights == 1) ": 1 flight left the catalogue" else paste0(": ", r$flights, " flights left the catalogue")), type = "message") } })
  # safe: the sources table's editor reports a read problem
  output$src_list <- renderUI({ live_stamp(cfg_path(CFG$project_dir, "sources")); s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())
    if (!nrow(s)) return(p(class = "muted", "No source yet: add a folder or drive below and scan it."))
    sc <- if (is.null(C())) data.table() else C()$sources; fl <- if (is.null(C())) data.table() else C()$flights; k <- src_counts()$photos
    js <- function(id, sid) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", id, jsonlite::toJSON(sid, auto_unbox = TRUE))
    np <- normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE)
    tagList(lapply(seq_len(nrow(s)), function(i) { sid <- s$source_id[i]; scanned <- nrow(sc) && sid %in% sc$source_id
      n_ph <- k[source_id == sid, N] %||% 0L; n_fl <- if (nrow(fl) && "source_id" %in% names(fl)) sum(fl$source_id == sid) else 0L
      inside <- s$source_id[-i][in_dirs(np[i], np[-i])]; holds <- sum(startsWith(np[-i], paste0(np[i], "/")))
      when <- if (scanned) paste("scanned", substr(sc[source_id == sid, last_scanned][1], 1, 10)) else "never finished"
      meta <- paste(c(when, if (scanned) c(paste(num(sum(n_ph)), if (sum(n_ph) == 1) "drone photo" else "drone photos"), paste(num(n_fl), if (n_fl == 1) "flight" else "flights")), if (length(inside)) paste("inside", inside[which.max(nchar(np[-i][in_dirs(np[i], np[-i])]))]),
                      if (holds) paste("contains", holds, if (holds == 1) "other source" else "other sources"),
                      if (!dir.exists(path.expand(s$path[i]))) "not plugged in: keeps its last scan"), collapse = " \u00b7 ")
      div(class = "src-row", style = "display:grid;grid-template-columns:minmax(0,1fr) auto auto;gap:12px;align-items:center;padding:8px 0;border-top:1px solid var(--bs-border-color)",
        div(div(style = "font-family:var(--bs-font-monospace);font-size:12.5px;overflow-wrap:anywhere", s$path[i]), div(class = "muted", style = "font-size:12px", paste0(sid, ": ", meta))),
        span(class = "badge-st", style = sprintf("border-color:%s;color:%s", if (scanned) PAL$green else PAL$gold, if (scanned) PAL$green else PAL$gold), if (scanned) "scanned" else "not scanned"),
        tags$button(type = "button", class = "btn btn-sm btn-outline-danger", `aria-expanded` = tolower(identical(rm_ask(), sid)), onclick = js("s_rm_ask", if (identical(rm_ask(), sid)) "" else sid), "Remove"),
        if (identical(rm_ask(), sid)) div(style = sprintf("grid-column:1/-1;border:1px solid %s;padding:10px;display:flex;gap:10px;flex-wrap:wrap;align-items:center", PAL$red),
          span(sprintf("Remove this source? %sThe folder and its files stay as they are. You can add it back and scan again.", if (!n_fl) "It has no catalogued flights. " else if (n_fl == 1) "Its 1 catalogued flight leaves the catalogue. " else paste0("Its ", num(n_fl), " catalogued flights leave the catalogue. "))),
          tags$button(type = "button", class = "btn btn-sm btn-danger", onclick = js("s_rm_do", sid), "Remove source"),
          tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", onclick = js("s_rm_ask", ""), "Keep it"))) })) })
  # after a scan: drone photos read, other photos left out (and why), flights, and flights placed from photo geotags
  output$src_scan <- renderUI({ k <- src_counts(); fl <- if (is.null(C())) data.table() else C()$flights
    if (!k$n_photos && !nrow(k$other)) return(p(class = "muted", "Nothing scanned yet."))
    geo <- if (nrow(fl) && "placed_from" %in% names(fl)) sum(fl$placed_from %in% "photo geotags") else 0L
    st <- function(v, l) div(style = "padding:10px;border:1px solid var(--bs-border-color)", tags$b(style = "display:block;font-size:26px;font-weight:500", num(v)), span(class = "muted", style = "font-size:12px", l))
    nd <- if (nrow(k$dup) && "kind" %in% names(k$dup)) sum(k$dup$kind == "photo") else 0L          # photo copies, out of the photos read
    nl <- if (nrow(k$dup) && "kind" %in% names(k$dup)) sum(k$dup$kind == "flight log") else 0L     # eMotion log copies and versions
    tagList(div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:6px",
                st(k$n_photos - nd, "drone photos, every one read"), st(nrow(k$other), "other photos left out"), st(nd + nl, if (nl) sprintf("duplicates counted once (%s flight logs)", num(nl)) else "duplicates counted once"),
                st(nrow(fl), "flights"), st(geo, "flights placed from photo geotags")),
            p(class = "muted mt-2", sprintf("%s of the drone photos have a position. Other photos left out: no drone tag and a camera not in the drone cameras table (Set-up tables).", num(k$positioned)))) })
  output$s_dup <- renderDT({ x <- src_counts()$dup; if (!nrow(x)) x <- data.table(kind = character(), kept_path = character(), dup_path = character(), why = character())
    ovid_dt(x[, intersect(c("kind", "kept_path", "dup_path", "why"), names(x)), with = FALSE], selection = "none", page = 10) })
  output$s_other <- renderDT({ x <- src_counts()$other; if (!nrow(x)) x <- data.table(path = character(), make = character(), model = character(), why = character())
    ovid_dt(x[, intersect(c("path", "make", "model", "why"), names(x)), with = FALSE], selection = "none", page = 10) })
  # Scans run in the background (core runner.R): a separate R process, the progress box at the bottom right and Cancel,
  # which stops the scan at its next folder. A stopped scan leaves each source its last good scan.
  # done: the message when it finishes; then(r): anything more to do after a finished run (the viewer's download).
  scan_done <- function(what, done = paste(what, "done"), then = NULL) function(r) {
    for (sdir in Sys.glob(file.path(path.expand(CFG$project_dir), "cache", "sources", "*"))) unlink(file.path(sdir, "scan.rds.part"))
    if (isTRUE(session$isClosed())) return(invisible())                       # the page was closed or reloaded during the run: the page open now reloads (below)
    reload(); session$userData$run_seen <- RUNNER$tick
    if (r$status == "finished") { showNotification(done, type = "message"); if (is.function(then)) then(r) }
    else if (r$status == "cancelled") showNotification(paste(what, "stopped; each source keeps its last good scan"), type = "warning")
    else showNotification(paste(what, "failed:", r$message), type = "error", duration = NULL) }
  # Every run (scan, rescan, rebuild, viewer) starts here. The set-up check comes first: with errors, a question (review,
  # run anyway, cancel); a run anyway is logged.
  pending <- reactiveVal(NULL)
  # a run another page started (or one this page took over after a reload) has ended: read the catalogue again
  session$userData$run_seen <- RUNNER$tick
  observe({ invalidateLater(1500); t <- RUNNER$tick
    if (!identical(t, session$userData$run_seen) && is.null(RUNNER$job)) { session$userData$run_seen <- t; reload() } })
  start_scan <- function(what, args, force = FALSE, done = paste(what, "done"), then = NULL, verb = "Scan") {
    e <- tryCatch(setup_check(CFG), error = function(e) { showNotification(paste("The set-up check could not run:", conditionMessage(e)), type = "warning"); NULL }); e <- if (is.null(e)) NULL else e[level == "error"]
    if (!force && !is.null(e) && nrow(e)) { pending(list(what = what, args = args, done = done, then = then))
      return(showModal(setup_gate(e, "scan_review", "scan_force", verb))) }
    cfg <- CFG; if (force) cfg$run_anyway <- setup_problem_lines(e)
    run_start(session, cfg, scan_done(what, done, then), label = what, args = args) }
  observeEvent(input$scan_force, { removeModal(); x <- pending(); if (!is.null(x)) start_scan(x$what, x$args, force = TRUE, done = x$done, then = x$then) })
  observeEvent(input$scan_review, { removeModal(); ovid_go(session, "Sources"); updateTabsetPanel(session, "simple_step", selected = "import") })   # the problem list (Standard), Import (Simple)
  # a folder inside a source, or holding one, is said so before it is scanned (each file then belongs to the innermost source)
  scan_new <- function(p) { start_scan(paste("Scanning", basename(p)), list(scan = p, source_id = trim(input$s_id))); updateTextInput(session, "s_id", value = "") }
  scan_pending <- reactiveVal(NULL)
  observeEvent(input$s_scan, { p <- trim(input$s_path); if (!nzchar(p) || !dir.exists(path.expand(p))) return(showNotification("That folder was not found", type = "error"))
    # safe: the sources table's editor reports a read problem
    p <- normalizePath(path.expand(p), winslash = "/"); s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())
    np <- if (nrow(s)) normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE) else character()
    if (p %in% np) return(scan_new(p))
    inn <- s$source_id[in_dirs(p, np)]; has <- s$source_id[startsWith(np, paste0(p, "/"))]
    if (!length(inn) && !length(has)) return(scan_new(p))
    scan_pending(p)
    showModal(modalDialog(title = "This folder overlaps a source",
      if (length(inn)) p(sprintf("It is inside %s. Its files will be counted under the new source only, and left out of %s.", paste(inn, collapse = ", "), paste(inn, collapse = ", "))),
      if (length(has)) p(sprintf("It contains %s. Those folders' files stay with %s.", paste(has, collapse = ", "), paste(has, collapse = ", "))),
      p(class = "muted", "No file is counted twice, and no file is changed."),
      footer = tagList(modalButton("Cancel"), actionButton("s_scan_go", "Scan", class = "btn-primary")))) })
  observeEvent(input$s_scan_go, { removeModal(); p <- scan_pending(); scan_pending(NULL); if (!is.null(p)) scan_new(p) })
  # ---- which other geodata to catalogue (calibration options geodata_gis_layers, geodata_imagery; R/geodata.R), set on Sources
  # and on Simple's Import; it applies on the next scan or rebuild, from the scans already made
  gd_on <- function(v) { cal <- tryCatch(load_configs(CFG)$calibration, error = function(e) NULL); n <- if (is.null(cal)) NA_real_ else cal_num(cal, v); !is.finite(n) || n != 0 }
  observe({ src_cfg(); live_config(CFG$project_dir); for (k in list(c("s_gd_gis", "geodata_gis_layers"), c("s_gd_img", "geodata_imagery"), c("sp_gd_gis", "geodata_gis_layers"), c("sp_gd_img", "geodata_imagery")))
    if (!identical(isolate(input[[k[1]]]), gd_on(k[2]))) updateCheckboxInput(session, k[1], value = gd_on(k[2])) })
  gd_set <- function(v, on) { if (is.null(on) || identical(on, gd_on(v))) return()
    cal <- load_configs(CFG)$calibration; i <- which(cal$value == v & cal$applies_to %in% c("*", ""))
    if (length(i)) { set(cal, i, "choice", "custom"); set(cal, i, "number", if (on) "1" else "0") }
    else cal <- rbind(cal, data.table(app = "RS", value = v, applies_to = "*", choice = "custom", number = if (on) "1" else "0", source = "set on the Sources page", notes = ""), fill = TRUE)
    write_cfg(cal, CFG$project_dir, "calibration", who = "Sources")
    showNotification(paste0(if (on) "On" else "Off", ": applies on the next scan or rebuild."), type = "message") }
  for (k in list(c("s_gd_gis", "geodata_gis_layers"), c("s_gd_img", "geodata_imagery"), c("sp_gd_gis", "geodata_gis_layers"), c("sp_gd_img", "geodata_imagery"))) local({ k <- k
    observeEvent(input[[k[1]]], gd_set(k[2], input[[k[1]]]), ignoreInit = TRUE) })
  # ---- the list of folders to scan: added one by one (rows in sources.csv, so they stay when the page is reloaded), then scanned
  # together in one run. A source with no scan in cache/sources is waiting; it is renamed or removed freely, as nothing was read yet.
  src_bump <- reactiveVal(0)
  src_cfg <- reactive({ src_bump(); rv$tick; live_stamp(cfg_path(CFG$project_dir, "sources"))
    tryCatch(load_configs(CFG)$sources, error = function(e) data.table(source_id = character(), path = character(), label = character(), notes = character())) })
  src_scanned <- function(sid) vapply(sid, function(k) file.exists(file.path(source_dir(CFG, k), "scan.rds")), NA)
  queued <- reactive({ s <- src_cfg(); if (nrow(s)) s[!src_scanned(source_id)] else s })
  add_to_list <- function(p, sid = "") { p <- trim(p %||% "")
    if (!nzchar(p) || !dir.exists(path.expand(p))) return(showNotification("That folder was not found", type = "error"))
    p <- normalizePath(path.expand(p), winslash = "/"); s <- src_cfg()
    if (p %in% normalizePath(path.expand(s$path), winslash = "/", mustWork = FALSE)) return(showNotification("That folder is already in the list.", type = "message"))
    if (startsWith(paste0(normalizePath(path.expand(CFG$project_dir), winslash = "/", mustWork = FALSE), "/"), paste0(p, "/")))
      return(showNotification("The project folder is inside that folder: choose the folder that holds the flights.", type = "error"))
    r <- tryCatch(add_source(load_configs(CFG), CFG, p, trim(sid %||% "")), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(r)) src_bump(src_bump() + 1)
    !is.null(r) }
  scan_queued <- function() { q <- queued(); if (!nrow(q)) return(showNotification("No new folder in the list: add one first.", type = "warning"))
    q <- q[dir.exists(path.expand(path))]; if (!nrow(q)) return(showNotification("None of the new folders is plugged in.", type = "warning"))
    start_scan(if (nrow(q) == 1) paste("Scanning", q$source_id) else paste("Scanning", nrow(q), "folders"), list(scan = q$path)) }
  observeEvent(input$s_add, if (isTRUE(add_to_list(input$s_path, input$s_id))) { updateTextInput(session, "s_path", value = ""); updateTextInput(session, "s_id", value = "") })
  observeEvent(input$sp_add, if (isTRUE(add_to_list(input$sp_path))) updateTextInput(session, "sp_path", value = ""))
  observeEvent(input$s_scan_new, scan_queued()); observeEvent(input$sp_scan_new, scan_queued())
  output$s_new_btn <- renderUI({ n <- nrow(queued()); if (n) actionButton("s_scan_new", sprintf("Scan the %s", if (n == 1) "new folder" else paste(n, "new folders")), class = "btn-primary") })
  # a waiting folder's short name (its source_id), typed in the Simple list
  observeEvent(input$sp_rename, { x <- input$sp_rename; old <- x$sid %||% ""; new <- safe_name(trim(x$name %||% "")); s <- src_cfg()
    if (!nzchar(new) || identical(new, old) || !old %in% s$source_id) return(src_bump(src_bump() + 1))
    if (src_scanned(old)) return(showNotification("A scanned source keeps its name.", type = "warning"))
    if (new %in% s$source_id) { src_bump(src_bump() + 1); return(showNotification(paste0("There is already a source called ", new, "."), type = "warning")) }
    s[source_id == old, source_id := new]; write_cfg(s, CFG$project_dir, "sources", who = "Import"); src_bump(src_bump() + 1) })
  # Remove from the Simple list: a waiting folder at once; a scanned one after a question (its flights leave the catalogue)
  sp_rm_id <- reactiveVal("")
  observeEvent(input$sp_rm, { sid <- input$sp_rm; if (run_busy()) return(showNotification("Wait for the scan or cancel it", type = "warning"))
    if (!src_scanned(sid)) { remove_source(CFG$project_dir, sid); return(src_bump(src_bump() + 1)) }
    sp_rm_id(sid); n_fl <- if (is.null(C())) 0L else sum(C()$flights$source_id == sid, na.rm = TRUE)
    showModal(modalDialog(title = paste0("Remove ", sid, "?"), easyClose = TRUE,
      p(paste(if (!n_fl) "It has no flights in the catalogue." else if (n_fl == 1) "Its 1 flight leaves the catalogue." else paste("Its", n_fl, "flights leave the catalogue."), "The folder and its files stay as they are; you can add it again and scan it.")),
      footer = tagList(modalButton("Keep it"), actionButton("sp_rm_ok", "Remove", class = "btn-danger")))) })
  observeEvent(input$sp_rm_ok, { removeModal(); sid <- sp_rm_id(); sp_rm_id(""); if (!nzchar(sid)) return()
    r <- tryCatch(remove_source(CFG$project_dir, sid), error = function(e) { showNotification(paste("Not removed:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(r)) { reload(); src_bump(src_bump() + 1) } })
  no_source <- function() { s <- tryCatch(load_configs(CFG)$sources, error = function(e) data.table())   # nothing to rescan or rebuild from yet
    if (nrow(s)) return(FALSE); showNotification("No source yet: add a drive or folder and scan it first.", type = "warning"); TRUE }
  observeEvent(input$s_rescan, if (!no_source()) start_scan("Rescanning drives", list()))
  observeEvent(input$s_build, if (!no_source()) start_scan("Rebuilding", list(scan = character()), done = "Catalogue rebuilt from the sources' last scans; no source was read.", verb = "Rebuild"))
