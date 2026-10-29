# core/launcher_app/app.R  --  The ovid launcher: one small page that starts the five apps. The selected app on the left (its
# icon, name, what it does, Open), the apps as a list on the right; arrows and Enter choose and open, Cmd/Ctrl 1-5 open one.
# Each app runs in its own R process on its own port (logic.R); a running app is opened again, never started twice, and has
# a Stop button. The first start installs the packages build/renv.lock lists that this R lacks, with one progress card. An app
# that fails to start says why in one sentence, with Try again, the troubleshooting zip (core manifest.R) and its log.
#   Started by "Open ovid.command" at the top of Ovid_Apps (OVID_ROOT = that folder).

suppressPackageStartupMessages(library(shiny))
ROOT <- normalizePath(Sys.getenv("OVID_ROOT", file.path(getwd(), "..", "..")), mustWork = FALSE)
CORE <- file.path(ROOT, "core")
source(file.path(CORE, "launcher_app", "logic.R"), local = TRUE)
for (.f in c("util", "cache", "manifest")) source(file.path(CORE, paste0(.f, ".R")), local = TRUE)   # troubleshooting_zip()
APP_NAME <- "ovid // launcher"

# The apps' own pixel icons (core/shell.R OVID_ICONS, 64 px), read without sourcing the whole shell.
ICONS <- local({ s <- readLines(file.path(CORE, "shell.R"), warn = FALSE); i <- grep("^OVID_ICONS <- list\\(", s)
  j <- i + which(grepl("\\)\\s*$", s[(i + 1):length(s)]))[1]; eval(parse(text = s[i:j])) })
icon_src <- function(k) paste0("data:image/png;base64,", ICONS[[k]])

ST <- lr_new(ROOT)

CSS <- "
:root{--bg:#22252a;--main:#262a2e;--side:#1b1d21;--card:#2b2e34;--line:#3a3e45;--line2:#4a4e56;--fg:#e6e8eb;--text2:#c9ced5;--mut:#9aa0a8;
  --sel:#2b2e34;--run:#98c379;--err:#f0b3a9;--errbg:#3a2a28;--errline:#6a3f39;--onacc:#201e1e;
  --cs:#bccd80;--wlr:#88c0d0;--gt:#e18072;--gs:#9f8bb8;--rs:#e5c07b;color-scheme:dark}
@media (prefers-color-scheme: light){:root{--bg:#fbfaf7;--main:#ffffff;--side:#f0eee8;--card:#f6f4ef;--line:#e1ded6;--line2:#cdc9bf;--fg:#1b1d1f;
  --text2:#3f4449;--mut:#5c6167;--sel:#ffffff;--run:#3f7a33;--err:#a33f33;--errbg:#fbeae6;--errline:#e3b3a9;--onacc:#ffffff;
  --cs:#5a6b1f;--wlr:#2f6878;--gt:#a33f33;--gs:#5d4f80;--rs:#84620f;color-scheme:light}}
html,body{margin:0;height:100%;background:var(--side)}
body{font:14px/1.45 Inter,-apple-system,'Segoe UI',Helvetica,Arial,sans-serif;color:var(--fg);display:grid;place-items:center}
img{image-rendering:pixelated}
.win{width:min(720px,100vw);height:min(440px,100vh);display:grid;grid-template-columns:minmax(0,1fr) 260px;background:var(--bg);border:1px solid var(--line);border-radius:12px;overflow:hidden}
@media (max-width:640px){.win{grid-template-columns:1fr;height:auto;border-radius:0}}
.main{padding:26px 30px 24px;display:flex;flex-direction:column;gap:14px;background:var(--main);min-width:0}
.brand{font-size:15px;font-weight:700;color:var(--mut)}
.title{font-size:24px;font-weight:600;letter-spacing:-.01em}.key{font-size:13px;font-weight:600}
.what{margin:0;font-size:13.5px;color:var(--text2);max-width:380px}
.grow{flex:1}
.acts{display:flex;align-items:center;gap:10px;flex-wrap:wrap}
.btn{height:40px;padding:0 20px;display:inline-flex;align-items:center;border-radius:9px;border:0;font:inherit;font-weight:600;cursor:pointer}
.btn2{height:40px;padding:0 14px;display:inline-flex;align-items:center;border-radius:9px;border:1px solid var(--line2);background:transparent;color:var(--fg);font:inherit;cursor:pointer;text-decoration:none}
.hint{font-size:12.5px;color:var(--mut)}
.card{margin-top:6px;display:flex;flex-direction:column;gap:10px;padding:18px;border-radius:12px;background:var(--card);border:1px solid var(--line)}
.bar{height:8px;border-radius:999px;background:var(--line);overflow:hidden}.bar i{display:block;height:100%;border-radius:999px;background:var(--fg)}
.err{display:flex;flex-direction:column;gap:10px;padding:14px 16px;border-radius:10px;background:var(--errbg);border:1px solid var(--errline);font-size:13.5px}
.err b{color:var(--err)}.err pre{margin:6px 0 0;font:11.5px/1.6 ui-monospace,Menlo,monospace;white-space:pre-wrap;color:var(--text2);max-height:120px;overflow:auto}
.side{background:var(--side);border-left:1px solid var(--line);padding:14px 10px 12px;display:flex;flex-direction:column;gap:3px}
.row{display:flex;align-items:center;gap:10px;padding:8px 10px;border-radius:9px;color:var(--fg);cursor:pointer;border:0;background:none;font:inherit;text-align:left;width:100%}
.row:hover{background:color-mix(in srgb,var(--sel) 70%,transparent)}
.row.sel{background:var(--sel);font-weight:600}
.row .nm{flex:1;min-width:0;display:flex;flex-direction:column;gap:1px}.row .nm span:first-child{overflow-wrap:anywhere}
.row .rn{font-size:11.5px;font-weight:600;color:var(--run)}
.row .k{font-size:11.5px;color:var(--mut);font-weight:400;opacity:0;transition:opacity .15s}.row:hover .k,.row:focus-visible .k{opacity:1}
.row .stop{height:26px;padding:0 9px;border-radius:6px;border:1px solid var(--line2);background:transparent;color:var(--fg);font:inherit;font-size:12px;cursor:pointer}
.row:disabled{opacity:.4;cursor:default}
.row:focus-visible,.btn:focus-visible,.btn2:focus-visible{outline:2px solid var(--fg);outline-offset:-2px}
.foot{padding:0 10px;font-size:12px;color:var(--mut);display:flex;justify-content:space-between;gap:8px}
.foot button{background:none;border:0;padding:0;color:var(--mut);font:inherit;cursor:pointer;text-decoration:underline}
"

JS <- "
document.addEventListener('keydown', function(e){
  if ((e.metaKey || e.ctrlKey) && e.key >= '1' && e.key <= '5') { e.preventDefault(); Shiny.setInputValue('open_n', {n: +e.key, t: Date.now()}); return; }
  if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;
  if (e.key === 'ArrowDown' || e.key === 'ArrowUp') { e.preventDefault(); Shiny.setInputValue('move', {d: e.key === 'ArrowDown' ? 1 : -1, t: Date.now()}); }
  if (e.key === 'Enter' && !(e.target.closest && e.target.closest('button,a,summary'))) { e.preventDefault(); Shiny.setInputValue('open_sel', Date.now()); }
});
Shiny.addCustomMessageHandler('ovid-open', function(m){ window.open(m.url, 'ovid_' + m.key); });
"

ui <- fluidPage(theme = NULL, title = "ovid",
  tags$head(tags$style(HTML(CSS)), tags$script(HTML(JS)), tags$link(rel = "icon", href = icon_src("CS"))),
  div(class = "win", role = "application", `aria-label` = "ovid launcher", uiOutput("main", class = "main"), uiOutput("side", class = "side")))

server <- function(input, output, session) {
  rv <- reactiveValues(sel = "GT", tick = 0, missing = lr_missing(ROOT), total = NA_integer_, installer = NULL)
  isolate(rv$total <- length(rv$missing))
  first <- reactive(length(rv$missing) > 0)
  # the first start: install what build/renv.lock lists and this R lacks, in the background, counting as it goes
  observe({ if (!length(isolate(rv$missing)) || !is.null(isolate(rv$installer))) return()
    lock <- file.path(ROOT, "build", "renv.lock"); need <- isolate(rv$missing)
    rv$installer <- callr::r_bg(function(lock, need) {
      if (!requireNamespace("renv", quietly = TRUE)) utils::install.packages("renv", repos = "https://cloud.r-project.org")
      # safe: a version that cannot be had falls back to the current CRAN one
      tryCatch(renv::restore(lockfile = lock, packages = need, prompt = FALSE, library = .libPaths()[1]),
               error = function(e) utils::install.packages(need, repos = "https://cloud.r-project.org"))
    }, args = list(lock = lock, need = need), supervise = TRUE) })
  observe({ invalidateLater(1500); rv$tick <- isolate(rv$tick) + 1
    lr_poll(ST)
    if (length(isolate(rv$missing))) { rv$missing <- lr_missing(ROOT); if (!length(rv$missing)) rv$installer <- NULL } })

  open_app <- function(k) {
    if (first()) return()
    res <- lr_start(ST, k, lr_port())
    r <- ST$rec[[k]]
    if (res == "already") { session$sendCustomMessage("ovid-open", list(url = r$url, key = k)); return() }
    # open the page once it answers (polled), at most 60 s
    t0 <- Sys.time()
    wait <- observe({ invalidateLater(500); lr_poll(ST); r <- ST$rec[[k]]
      if (r$state == "running") { session$sendCustomMessage("ovid-open", list(url = r$url, key = k)); wait$destroy() }
      else if (r$state == "failed" || difftime(Sys.time(), t0, units = "secs") > 60) wait$destroy() })
  }
  observeEvent(input$pick, rv$sel <- input$pick$k)
  observeEvent(input$open, { rv$sel <- input$open$k; open_app(input$open$k) })
  observeEvent(input$open_sel, open_app(rv$sel))
  observeEvent(input$open_n, { k <- ST$apps$key[input$open_n$n]; rv$sel <- k; open_app(k) })
  observeEvent(input$move, { i <- match(rv$sel, ST$apps$key) + input$move$d; rv$sel <- ST$apps$key[max(1, min(5, i))] })
  observeEvent(input$stop, { lr_stop(ST, input$stop$k); rv$tick <- rv$tick + 1 })
  observeEvent(input$retry, { lr_stop(ST, rv$sel); open_app(rv$sel) })

  output$main <- renderUI({ rv$tick; k <- rv$sel; a <- ST$apps[ST$apps$key == k, ]; r <- ST$rec[[k]]
    if (first()) {
      n <- rv$total; done <- n - length(rv$missing)
      return(tagList(span(class = "brand", "ovid"),
        div(class = "card", span(style = "font-size:18px;font-weight:600", "Setting up ovid for the first time"),
            div(class = "bar", role = "progressbar", `aria-label` = "Packages installed", `aria-valuemin` = 0, `aria-valuemax` = n, `aria-valuenow` = done,
                tags$i(style = sprintf("width:%d%%", round(100 * done / max(n, 1))))),
            div(style = "display:flex;justify-content:space-between;font-size:13px;color:var(--mut)", span(sprintf("%d of %d packages", done, n)), span("only once")),
            span(style = "font-size:13px;color:var(--mut)", "The apps open as soon as this is done. It never touches your data.")))) }
    fail <- r$state == "failed"
    tagList(span(class = "brand", "ovid"),
      tags$img(src = icon_src(k), alt = "", width = if (fail) 56 else 96, height = if (fail) 56 else 96, style = "margin-top:4px"),   # a failed start needs the room for its message
      div(style = "display:flex;flex-direction:column;gap:4px", span(class = "title", a$name), span(class = "key", style = sprintf("color:var(--%s)", tolower(k)), k)),
      if (!fail) tags$p(class = "what", a$what),
      if (fail) div(class = "err", span(tags$b(paste(k, "didn't start.")), " ", r$error),
                    tags$details(tags$summary(class = "hint", style = "cursor:pointer", "Show log"),
                                 tags$pre(paste(utils::tail(if (file.exists(r$log)) readLines(r$log, warn = FALSE) else "", 40), collapse = "\n")))),
      div(class = "grow"),
      div(class = "acts",
        if (fail) tags$button(class = "btn", style = sprintf("background:var(--%s);color:var(--onacc)", tolower(k)), onclick = "Shiny.setInputValue('retry', Date.now())", "Try again")
        else tags$button(class = "btn", style = sprintf("background:var(--%s);color:var(--onacc)", tolower(k)),
                         onclick = sprintf("Shiny.setInputValue('open', {k: '%s', t: Date.now()})", k),
                         if (r$state == "running") paste("Show", k) else if (r$state == "starting") paste("Starting", k, "…") else paste("Open", k)),
        if (fail) downloadButton("zip", "Troubleshooting zip", class = "btn2", icon = NULL) else span(class = "hint", "opens in your browser"))) })

  output$side <- renderUI({ rv$tick; dis <- first()
    rows <- lapply(seq_len(nrow(ST$apps)), function(i) { k <- ST$apps$key[i]; r <- ST$rec[[k]]; run <- r$state %in% c("starting", "running")
      tags$button(type = "button", class = paste("row", if (k == rv$sel && !dis) "sel"), disabled = if (dis) NA,
        style = if (k == rv$sel && !dis) sprintf("box-shadow:inset 3px 0 0 var(--%s)", tolower(k)),
        onclick = sprintf("Shiny.setInputValue('pick', {k: '%s', t: Date.now()})", k), ondblclick = sprintf("Shiny.setInputValue('open', {k: '%s', t: Date.now()})", k),
        tags$img(src = icon_src(k), alt = "", width = 32, height = 32),
        span(class = "nm", span(ST$apps$name[i]), if (run) span(class = "rn", if (r$state == "running") "Running" else "Starting…")),
        if (run) tags$span(class = "stop", role = "button", tabindex = "0", onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('stop', {k: '%s', t: Date.now()})", k), "Stop")
        else span(class = "k", paste0("⌘", i))) })
    n <- length(lr_running(ST))
    tagList(rows, div(class = "grow"),
      div(class = "foot", span(if (dis) sprintf("R %s · %d of %d packages", getRversion(), rv$total - length(rv$missing), rv$total)
                              else if (n) sprintf("%d running · R %s", n, getRversion()) else sprintf("R %s · ready", getRversion())),
          tags$button(onclick = "Shiny.setInputValue('quit', Date.now())", "Quit"))) })

  # Quit: stop the running apps, or leave them running
  observeEvent(input$quit, {
    if (!length(lr_running(ST))) stopApp()
    else showModal(modalDialog(title = "Quit the launcher", easyClose = TRUE, footer = tagList(
      actionButton("quit_stop", "Stop the apps and quit"), actionButton("quit_leave", "Leave them running"), modalButton("Cancel")),
      sprintf("%s still running.", paste(lr_running(ST), collapse = " and ")))) })
  observeEvent(input$quit_stop, { for (k in lr_running(ST)) lr_stop(ST, k); stopApp() })
  observeEvent(input$quit_leave, stopApp())

  # the troubleshooting zip of a failed start: the app's project folder (from its settings) plus the launcher's log of it
  output$zip <- downloadHandler(function() paste0("ovid_troubleshooting_", tolower(rv$sel), "_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    function(file) { k <- rv$sel; r <- ST$rec[[k]]
      pd <- tempfile("ovid_launch_"); dir.create(file.path(pd, "output", "logs"), recursive = TRUE)
      if (nzchar(r$log) && file.exists(r$log)) file.copy(r$log, file.path(pd, "output", "logs", paste0("launch_", tolower(k), ".log")))
      APP_NAME <<- paste("ovid //", k); troubleshooting_zip(pd, file) },
    contentType = "application/zip")
  session$onSessionEnded(function() NULL)                                     # closing the tab leaves the apps as they are
}

shinyApp(ui, server)
