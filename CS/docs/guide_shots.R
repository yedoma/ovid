# docs/guide_shots.R  --  The CS figures that show the app as it is now: the reference guide's (docs/img/cs_01_home.png,
# cs_02_viewer_plot.png, cs_03_alt_plot_export.png) and the how-to's figure for removing a stretch (docs/img/guide/cs_ht_09.png,
# in the Workbench). Modelled on WLR's docs/guide_shots.R.
#
#   Rscript docs/guide_shots.R [names ...]     (from the CS folder; no names = every figure)
#
# The figures come from a guide project built from tests/golden (no site data): guide_project() copies its raw files and
# runs it with its settings. Then start a copy of this CS folder on it, with no saved settings of its own (so only the guide
# project is read), and run this script:
#   CS_PROJECT_DIR=<dir>/project  CS_RAW_DIRS=<dir>/raw  OVID_CORE=<Ovid_Apps>/core   port 8792
# Needs chromote and Google Chrome or Chromium (CHROMOTE_CHROME).

suppressMessages({ library(chromote); library(data.table) })
GUIDE_DIR <- Sys.getenv("CS_GUIDE_DIR", file.path(tempdir(), "cs_guide"))
PORT <- as.integer(Sys.getenv("CS_GUIDE_PORT", "8792"))
STATION <- Sys.getenv("CS_GUIDE_STATION", "52001")                    # a station of the golden project with several years

# ---- The guide project ---------------------------------------------------------------------------------------------------
guide_project <- function(dir = GUIDE_DIR, golden = file.path("tests", "golden")) {
  dir.create(file.path(dir, "raw"), recursive = TRUE, showWarnings = FALSE); dir.create(file.path(dir, "project"), showWarnings = FALSE)
  file.copy(list.files(file.path(golden, "raw"), full.names = TRUE), file.path(dir, "raw"), recursive = TRUE)
  Sys.setenv(CS_NO_RUN = "1"); source("cs_pipeline.R", local = TRUE)
  st <- fread(file = file.path(golden, "settings.csv"), colClasses = "character")
  st <- setNames(lapply(st$value, function(v) { n <- suppressWarnings(as.numeric(v)); if (is.na(n)) v else n }), st$setting)
  invisible(capture.output(run_pipeline(modifyList(CFG, c(list(raw_dirs = file.path(dir, "raw"), project_dir = file.path(dir, "project"),
                                                          make_plots = FALSE, incremental = FALSE), st)))))
  invisible(dir)
}

# ---- Driving the page ----------------------------------------------------------------------------------------------------
# A marker: a 2 px outline round the control and a numbered badge on its top-left corner, in the guides' one accent.
JS_HELPERS <- "
window.G = {
  wait: ms => new Promise(r => setTimeout(r, ms)),
  vis: e => !!(e && e.getClientRects().length && e.getBoundingClientRect().width > 0),
  go: async (v, ms) => { const a = document.querySelector('a[data-ovid-go=\"' + v + '\"]'); if (a) a.click(); await G.wait(ms || 3000); },
  find: (txt, sel) => [...document.querySelectorAll(sel || 'button, a, label, h3, h4, b, span, div')].filter(G.vis)
      .filter(e => (e.innerText || '').trim().replace(/\\s+/g, ' ').startsWith(txt))
      .sort((a, b) => a.getBoundingClientRect().width * a.getBoundingClientRect().height - b.getBoundingClientRect().width * b.getBoundingClientRect().height)[0],
  mark: (e, n) => { if (!e) return 'missing ' + n; const r = e.getBoundingClientRect(), sx = window.scrollX, sy = window.scrollY;
    const o = document.createElement('div'); o.className = 'gmk';
    o.style.cssText = `position:absolute;z-index:99999;pointer-events:none;left:${r.left + sx - 4}px;top:${r.top + sy - 4}px;width:${r.width + 8}px;height:${r.height + 8}px;border:2px solid #D08770;border-radius:6px`;
    const b = document.createElement('div'); b.className = 'gmk'; b.textContent = n;
    b.style.cssText = `position:absolute;z-index:100000;pointer-events:none;left:${r.left + sx - 16}px;top:${r.top + sy - 16}px;width:24px;height:24px;border-radius:50%;background:#D08770;color:#242933;font:700 13px/24px -apple-system,Helvetica,Arial,sans-serif;text-align:center;border:2px solid #242933`;
    document.body.append(o, b); return 'ok'; },
  has: (txt, sel) => [...document.querySelectorAll(sel || 'button, a')].filter(G.vis).find(e => (e.innerText || '').includes(txt)),
  quiet: () => document.querySelectorAll('.shiny-notification').forEach(e => e.remove()),
  paths: (dir, to) => { const w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT); let n;
    while ((n = w.nextNode())) if (n.nodeValue.includes(dir)) n.nodeValue = n.nodeValue.split(dir).join(to);
    document.querySelectorAll('input, textarea').forEach(e => { if (e.value && e.value.includes(dir)) e.value = e.value.split(dir).join(to); }); }
};"

js <- function(b, code) {
  r <- b$Runtime$evaluate(paste0("(async () => {", code, "})()"), awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 180)
  if (!is.null(r$exceptionDetails)) stop("in the page: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
`%||%` <- function(a, b) if (is.null(a)) b else a

open_app <- function(query, width = 1440, height = 900) {
  b <- ChromoteSession$new(width = width, height = height)
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 2, mobile = FALSE)
  b$Emulation$setEmulatedMedia(features = list(list(name = "prefers-color-scheme", value = "dark")))
  b$Page$navigate(sprintf("http://127.0.0.1:%d/%s", PORT, query)); b$Page$loadEventFired(wait_ = TRUE); Sys.sleep(20)
  js(b, JS_HELPERS); b
}

# One figure: prep (JS) in the page, then the markers (JS returning 'ok' each), then the whole window.
shot <- function(b, file, prep = "", marks = "") {
  js(b, prep); js(b, sprintf("G.quiet(); G.paths(%s, '~/Projects/Ovid/Projects/CS');", jsonlite::toJSON(normalizePath(GUIDE_DIR), auto_unbox = TRUE)))
  m <- if (nzchar(marks)) js(b, paste0("return [", marks, "].join(',');")) else ""
  Sys.sleep(1.5); b$screenshot(file, cliprect = c(0, 0, 1440, 900), scale = 1, show = FALSE); js(b, "document.querySelectorAll('.gmk').forEach(e => e.remove());")
  cat(sprintf("  %-28s %s\n", basename(file), m))
}

pick <- function(v) sprintf("{ const e = document.getElementById('%s'); if (e && e.selectize) e.selectize.setValue(%s); }", v[1], jsonlite::toJSON(v[2], auto_unbox = TRUE))   # a select as a person picks it
STANDARD <- list(   # the reference guide's figures, in Standard
  cs_01_home = list(prep = "await G.go('Home', 5000);"),
  cs_02_viewer_plot = list(prep = paste0("await G.go('Explore', 4000);", pick(c("ex_station", STATION)), "await G.wait(8000);")),
  cs_03_alt_plot_export = list(prep = paste0("await G.go('Explore', 4000);", pick(c("ex_station", STATION)), "await G.wait(4000);",
                                             pick(c("ex_kind", "box")), "await G.wait(8000);")))
WORKBENCH <- list(  # removing a stretch is done in the Workbench (how-to task 9)
  cs_ht_09 = list(prep = "await G.wait(4000);",
                  marks = "G.mark(document.getElementById('wb_chart'),1), G.mark(G.has('Remove'),2), G.mark(G.has('Run'),3)"))

if (sys.nframe() == 0) {
  want <- commandArgs(TRUE); sel <- function(l) l[!length(want) | names(l) %in% want]
  if (length(sel(STANDARD))) { b <- open_app("?ui=standard")
    for (k in names(sel(STANDARD))) do.call(shot, c(list(b, file.path("docs", "img", paste0(k, ".png"))), STANDARD[[k]])); b$close() }
  if (length(sel(WORKBENCH))) { b <- open_app(sprintf("?ui=workbench&station=%s", STATION))
    for (k in names(sel(WORKBENCH))) do.call(shot, c(list(b, file.path("docs", "img", "guide", paste0(k, ".png"))), WORKBENCH[[k]])); b$close() }
}
