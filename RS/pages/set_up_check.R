# RS/pages/set_up_check.R  --  Set-up check and file audit, the last run's problems, and the viewer download.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- set-up check and file audit (Ovid/preflight.R, Ovid/file_audit.R) ----
  chk <- reactive({ input$chk_run; C(); live_config(CFG$project_dir); setup_check(CFG) })
  output$chk_sum <- renderUI({ x <- chk(); st <- check_status(x)
    ovid_status(if (st == "ok") "The set-up looks right" else sprintf("%d thing(s) to look at", sum(x$level != "info")), c(ok = "ok", warning = "warn", error = "error")[[st]]) })
  output$chk_tbl <- renderDT(ovid_dt(chk()[, .(Level = level, Area = area, Item = item, Problem = problem, Fix = fix)], selection = "none", page = 8,
                                       opts = list(dom = "tp", language = list(emptyTable = "Nothing to fix"))))
  output$fa_dl <- ovid_download("RS", "file audit", "csv", function(file) {
    f <- file.path(path.expand(CFG$project_dir), "output", "audit", "file_audit.csv"); if (!file.exists(f)) stop("Build the catalogue first"); file.copy(f, file) })
  # the problems of the last run (output/audit/problems.csv; none after a clean run), where the status bar's link opens
  run_probs <- reactive({ C(); rv$tick; run_problems_read(CFG$project_dir) })
  output$run_problems_n <- renderText({ n <- nrow(run_probs()); if (n) paste(num(n), if (n == 1) "problem" else "problems", "in the last run") else "No problems in the last run" })
  output$run_problems <- renderDT(ovid_dt(run_probs(), selection = "none", page = 8,
                                            opts = list(dom = "tp", language = list(emptyTable = "Nothing went wrong in the last run"))))
  # the viewer and the STAC catalogue's title: saved with the project (config/project_settings.json, core projects.R) and in
  # the app's settings, so every rebuild's STAC catalogue uses it too. The Sources page and the Workbench's STAC export tab
  # each have the box (x_title, wb_x_title); either sets both.
  viewer_zip <- reactiveVal(NULL)
  show_title <- function(v) for (k in c("x_title", "wb_x_title")) if (!identical(trim(isolate(input[[k]]) %||% ""), v)) updateTextInput(session, k, value = v)
  set_title <- function(v) { v <- trim(v %||% ""); if (!nzchar(v) || identical(v, CFG$viewer_title)) return(invisible())
    CFG$viewer_title <<- v; try(settings_save(SETTINGS_FILE, list(viewer_title = v)), silent = TRUE)
    # a project folder not made yet: the title is kept in the app's settings and goes to the project when it is left (projects_server)
    try(project_settings_save(CFG$project_dir, utils::modifyList(project_settings_read(CFG$project_dir), list(viewer_title = v))), silent = TRUE)
    show_title(v) }
  for (k in c("x_title", "wb_x_title")) local({ k <- k; tt <- debounce(reactive(input[[k]]), 800); observeEvent(tt(), set_title(tt()), ignoreInit = TRUE) })
  # Export viewer (ZIP): saves the title, writes the viewer in the background, then downloads it through the hidden link dl
  make_viewer <- function(title, dl) { set_title(title)
    start_scan("Writing the viewer", list(scan = character(), export = TRUE), done = "Viewer written: its download starts.", verb = "Write the viewer",
               then = function(r) { z <- file.path(path.expand(CFG$project_dir), "output", "viewer", "Ovid_RS_viewer.zip")
                 if (file.exists(z)) { viewer_zip(z); session$sendCustomMessage("rs-download", dl) } }) }   # www/rs_download.js
  if (nzchar(CFG$viewer_title %||% "")) for (k in c("x_title", "wb_x_title")) updateTextInput(session, k, value = CFG$viewer_title)   # the saved title, in every window
  observeEvent(rv$tick, if (nzchar(CFG$viewer_title %||% "")) show_title(CFG$viewer_title), ignoreInit = TRUE)   # another project opened
  observeEvent(input$x_make, make_viewer(input$x_title, "x_dl"))
  observeEvent(input$wb_x_make, make_viewer(input$wb_x_title, "wb_x_dl"))
  viewer_dl <- function(file) { z <- viewer_zip() %||% file.path(path.expand(CFG$project_dir), "output", "viewer", "Ovid_RS_viewer.zip")
    if (!file.exists(z)) stop("Write the viewer first"); file.copy(z, file) }
  output$x_dl <- ovid_download("RS", "viewer", "zip", viewer_dl)
  output$wb_x_dl <- ovid_download("RS", "viewer", "zip", viewer_dl)
  for (k in c("x_dl", "wb_x_dl")) outputOptions(output, k, suspendWhenHidden = FALSE)   # hidden links: their address is still set
  if (is.null(isolate(C()))) showNotification("No catalogue yet: scan a source on the Sources page.", type = "warning", duration = NULL)
