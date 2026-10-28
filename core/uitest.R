# Ovid/uitest.R  --  Click-through tests of an app in a real browser, shared by the apps' tests. Not sourced by any app.
#
# The app runs in its own R process on a free port; a headless Chrome (chromote) opens it, clicks every page in the
# navigation rail (?ui=standard), then opens the Workbench and each step of the Simple view (an app that has one), and after each click waits for the app to settle. A page fails when it shows
# an output error (a message from validate(need()) is not one: that is the app explaining an empty page), when the browser
# throws a JavaScript error, or when the app writes "Warning: Error in" (an observer or output that stopped). Returns one row
# per page, plus a row for the Workbench and one for errors the server wrote, so a test can say which page broke.
#   app_dir  the app's folder        env  variables for the app (its project folder, so it reads a test project)
#   first_load  seconds the first page may take to load (longer than timeout: a busy computer running several suites at once)
#   ignore_js  JavaScript errors that come from loading map tiles over the network, or from plotly's map fitting itself while
#              its page is hidden (getNorthWest), not from the app
# Ports are drawn by sample() (httpuv::randomPort() for the app, chromote for Chrome), and the tests call set.seed() for
# repeatable data, so every suite would draw the same "random" port and suites run side by side would collide. own_draw()
# runs a draw from a fresh seed (the clock and process id) and then puts the tests' own random state back as it was.
own_draw <- function(expr) {
  g <- globalenv(); had <- exists(".Random.seed", g, inherits = FALSE); if (had) seed <- get(".Random.seed", g)
  on.exit(if (had) assign(".Random.seed", seed, g) else if (exists(".Random.seed", g, inherits = FALSE)) rm(".Random.seed", envir = g))
  set.seed(NULL); expr
}
ui_click_through <- function(app_dir, env = character(), settle = 0.8, timeout = 30, first_load = 90, workbench = TRUE, simple = TRUE, stack = 4L,
                             ignore_js = "Mapbox error|tile|net::ERR|Failed to fetch|getNorthWest") {
  stopifnot(requireNamespace("chromote", quietly = TRUE), requireNamespace("processx", quietly = TRUE))
  port <- own_draw(httpuv::randomPort()); url <- sprintf("http://127.0.0.1:%d/", port)
  p <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", sprintf("shiny::runApp(%s, port = %d, launch.browser = FALSE)", deparse(normalizePath(app_dir)), port)),
                             env = c("current", env), stdout = "|", stderr = "2>&1", wd = app_dir)
  on.exit(p$kill(), add = TRUE)
  # safe: test tool; output of an app already stopped
  log <- character(); grab <- function() if (p$is_alive() || TRUE) log <<- c(log, tryCatch(p$read_output_lines(), error = function(e) character()))
  t0 <- Sys.time()
  # safe: test tool; asking whether the app is up yet
  up <- function() { con <- url(url); r <- tryCatch(suppressWarnings(length(readLines(con, n = 1, warn = FALSE)) > 0), error = function(e) FALSE); try(close(con), silent = TRUE); isTRUE(r) }
  repeat { grab(); if (up()) break
    if (!p$is_alive()) stop("the app stopped before it started:\n", paste(utils::tail(log, 20), collapse = "\n"))
    if (difftime(Sys.time(), t0, units = "secs") > 3 * timeout) stop("the app did not start within ", 3 * timeout, " s"); Sys.sleep(0.5) }
  op <- options(chromote.timeout = max(60, getOption("chromote.timeout", 10)))   # Chrome itself may take a while to start on a busy computer
  # safe: test tool; closing a browser already closed
  b <- own_draw(chromote::ChromoteSession$new(width = 1400, height = 900)); options(op); on.exit(try(b$close(), silent = TRUE), add = TRUE)
  js_err <- character(); b$Runtime$enable()
  b$Runtime$exceptionThrown(callback_ = function(m) { e <- m$exceptionDetails$exception$description %||% m$exceptionDetails$text %||% "JavaScript error"
    if (!grepl(ignore_js, e)) js_err <<- c(js_err, substr(e, 1, 300)) }, wait_ = FALSE)
  js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value
  settle_now <- function() { t1 <- Sys.time(); Sys.sleep(settle)
    while (isTRUE(js("document.documentElement.classList.contains('shiny-busy') || Array.from(document.querySelectorAll('.recalculating')).some(e => e.offsetParent !== null)")) &&
           difftime(Sys.time(), t1, units = "secs") < timeout) Sys.sleep(0.3) }
  # A missed "page loaded" signal is not a failure (Chrome can drop it when several suites start at once): what counts is
  # the page connecting to the app, waited for in the same time.
  connected <- function() isTRUE(tryCatch(js("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())"), error = function(e) FALSE))
  open <- function(u, wait = timeout, must = FALSE) { t1 <- Sys.time(); b$Page$navigate(u, wait_ = FALSE)
    # safe: test tool; a page that does not signal its load is checked by what it shows
    tryCatch(b$Page$loadEventFired(timeout_ = wait), error = function(e) NULL)
    while (!connected() && difftime(Sys.time(), t1, units = "secs") < wait) Sys.sleep(0.3)
    if (must && !connected()) stop("the page did not connect to the app within ", wait, " s")
    settle_now() }
  errors_on_page <- function(scope) js(sprintf("Array.from(document.querySelectorAll('%s .shiny-output-error')).filter(e => !/shiny-output-error-(validation|shiny\\.silent\\.error)/.test(e.className) && e.offsetParent !== null).map(e => (e.id || e.className) + ': ' + e.textContent.trim().slice(0, 160))", scope))
  open(paste0(url, "?ui=standard"), first_load, must = TRUE)
  pages <- unlist(js("Array.from(document.querySelectorAll('.d-rail-link')).map(a => a.getAttribute('data-ovid-go'))"))
  out <- list()
  for (pg in pages) { n0 <- length(js_err); js(sprintf("document.querySelector('.d-rail-link[data-ovid-go=\"%s\"]').click()", pg)); settle_now(); grab()
    shown <- isTRUE(js(sprintf("!!document.querySelector('.tab-pane.active[data-value=\"%s\"]')", pg)))
    e <- unlist(errors_on_page(".tab-pane.active")); e <- c(e, js_err[seq_along(js_err) > n0])
    out[[pg]] <- data.table(page = pg, shown = shown, errors = paste(e, collapse = " | ")) }
  if (workbench) { n0 <- length(js_err); open(paste0(url, "?ui=workbench")); grab()
    e <- c(unlist(errors_on_page("body")), js_err[seq_along(js_err) > n0])
    out[["workbench"]] <- data.table(page = "workbench", shown = isTRUE(js("document.body.innerText.length > 200")), errors = paste(e, collapse = " | ")) }
  if (simple) { open(paste0(url, "?ui=simple")); grab()                                   # the Simple view, in an app that has one:
    if (isTRUE(js("!!document.querySelector('.sp-step')"))) for (st in c("import", "view", "edit", "export")) { n0 <- length(js_err)   # each of its four steps
      js(sprintf("document.querySelector('.sp-step[data-step=\"%s\"]').click()", st)); settle_now(); grab()
      shown <- isTRUE(js(sprintf("!!document.querySelector('.sp-body .tab-pane.active[data-value=\"%s\"]')", st)))
      e <- c(unlist(errors_on_page(".sp-body .tab-pane.active")), js_err[seq_along(js_err) > n0])
      out[[paste0("simple:", st)]] <- data.table(page = paste("simple", st), shown = shown, errors = paste(e, collapse = " | ")) } }
  Sys.sleep(0.5); grab()
  i <- grep("Warning: Error in|^Error", log)                                               # each error with the first lines of its stack
  srv <- vapply(i, function(k) paste(trimws(log[k:min(length(log), k + stack)]), collapse = " / "), "")
  out[["server"]] <- data.table(page = "(server log)", shown = TRUE, errors = paste(unique(srv), collapse = " | "))
  rbindlist(out)
}
