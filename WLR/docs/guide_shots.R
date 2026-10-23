# docs/guide_shots.R  --  The figures of the WLR quick start and how-to guide (docs/img/guide/wlr_*.png): each is a part
# of a page of the running app, cropped, with numbered markers on the controls the steps name.
#
#   Rscript docs/guide_shots.R [names ...]     (from the WLR folder; no names = every figure)
#
# The figures come from a guide project built from tests/golden (no site data): Well A as in the golden test, plus Well B
# and a second barologger derived from it, and four tape checks. guide_project() writes it; then start two copies of the
# app on it, each from a copy of this WLR folder (the app keeps its settings in its own folder, and a run saves them), and
# run this script:
#   WLR_PROJECT_DIR=<dir>/fresh  WLR_RAW_DIRS=<dir>/raw WLR_METADATA_DIR=<dir>/meta WLR_DGPS_DIR=<dir>/dgps  port 8791
#   WLR_PROJECT_DIR=<dir>/proj   (the same folders)                                                        port 8790
# "fresh" is a project before its first run (quick start), "proj" one that has been set up and run (how-to).
# Needs chromote and Google Chrome or Chromium (CHROMOTE_CHROME).

suppressMessages(library(chromote))
OUT <- file.path("docs", "img", "guide")
GUIDE_DIR <- Sys.getenv("WLR_GUIDE_DIR", file.path(tempdir(), "wlr_guide"))

# ---- The guide project ---------------------------------------------------------------------------------------------------
# A copy of an .xle with another serial and site, and its first channel and temperature changed by f1(value, i) and f2.
xle_variant <- function(src, dst, serial, site, f1, f2) {
  x <- readLines(src, warn = FALSE, encoding = "latin1"); i <- 0L
  x <- sub("<Serial_number>[0-9]+</Serial_number>", sprintf("<Serial_number>%s</Serial_number>", serial), x)
  x <- sub("<Location>[^<]*</Location>", sprintf("<Location>%s</Location>", site), x)
  for (k in grep("<ch1>", x)) { i <- i + 1L; v <- as.numeric(sub(".*<ch1>([-0-9.]+)</ch1>.*", "\\1", x[k]))
    x[k] <- sub("<ch1>[-0-9.]+</ch1>", sprintf("<ch1>%.4f</ch1>", f1(v, i)), x[k]) }
  for (k in grep("<ch2>", x)) { v <- as.numeric(sub(".*<ch2>([-0-9.]+)</ch2>.*", "\\1", x[k]))
    x[k] <- sub("<ch2>[-0-9.]+</ch2>", sprintf("<ch2>%.3f</ch2>", f2(v)), x[k]) }
  writeLines(x, dst, useBytes = TRUE)
}
guide_project <- function(dir = GUIDE_DIR, golden = file.path("tests", "golden")) {
  unlink(dir, recursive = TRUE)
  for (d in c("raw/2014", "raw/baro/2014", "proj/config", "fresh", "meta", "dgps")) dir.create(file.path(dir, d), recursive = TRUE)
  file.copy(file.path(golden, "raw", "2014", "2000001_SiteA.xle"), file.path(dir, "raw", "2014"))
  file.copy(file.path(golden, "raw", "baro", "2014", "1000003_Baro.xle"), file.path(dir, "raw", "baro", "2014"))
  xle_variant(file.path(golden, "raw", "2014", "2000001_SiteA.xle"), file.path(dir, "raw", "2014", "2000002_SiteB.xle"),
              2000002, "SiteB", function(v, i) v + 0.22 + 0.015 * sin(i / 30), function(t) t - 1.2)
  xle_variant(file.path(golden, "raw", "baro", "2014", "1000003_Baro.xle"), file.path(dir, "raw", "baro", "2014", "1000004_Baro.xle"),
              1000004, "SiteB", function(v, i) v + 0.012 + 0.003 * sin(i / 9), function(t) t - 0.4)
  m <- sub(",golden test project[^,]*$", ",", readLines(file.path(golden, "config", "metadata.csv")))   # notes meant for the test
  writeLines(c(m, "2000002,2014,,Well B,WLR,,2014-02-11 18:00,2014-02-17 17:30,120,,,,,,,",
               "1000004,2014,,Well B,Baro,,,,,,,,,,,"), file.path(dir, "proj", "config", "metadata.csv"))
  writeLines(c("site,time,parameter,value,method,author,notes",
               "Well A,2014-02-13 12:00,depth_m,0.28,tape,AB,", "Well A,2014-02-17 12:00,depth_m,0.28,tape,AB,",   # about 1 cm above
               "Well B,2014-02-13 12:00,depth_m,0.50,tape,AB,", "Well B,2014-02-17 12:00,depth_m,0.49,tape,AB,"),  # the logger
             file.path(dir, "proj", "config", "manual_measurements.csv"))
  invisible(dir)
}

# ---- Driving the page ----------------------------------------------------------------------------------------------------
# Helpers put into the page: go to a page, find an element by its text, put a numbered marker on it, clear the markers.
JS_HELPERS <- "
window.G = {
  wait: ms => new Promise(r => setTimeout(r, ms)),
  vis: e => !!(e && e.getClientRects().length && e.getBoundingClientRect().width > 0 && getComputedStyle(e).visibility !== 'hidden'),
  go: async (v, ms) => { const a = document.querySelector('a[data-ovid-go=\"' + v + '\"]'); if (a) a.click(); await G.wait(ms || 2500); },
  find: (txt, sel, nth) => { const all = [...document.querySelectorAll(sel || 'button, a, label, th, td, h1, h2, h3, h4, span, div, input, select, summary, li')]
      .filter(G.vis).filter(e => (e.innerText || e.value || e.placeholder || '').trim().replace(/\\s+/g, ' ').startsWith(txt));
    all.sort((a, b) => a.getBoundingClientRect().width * a.getBoundingClientRect().height - b.getBoundingClientRect().width * b.getBoundingClientRect().height);
    return all[nth || 0]; },
  q: (sel, nth) => [...document.querySelectorAll(sel)].filter(G.vis)[nth || 0],
  // A marker: a soft outline round the control and a numbered badge pinned to its top-left corner (beside the words, for
  // words). One accent (Nord aurora orange), apart from the app's own cyan buttons and yellow warnings.
  ACC: '#D08770',
  // a label or heading is marked by its words, not by the block it sits in
  textlike: e => !(e instanceof Range) && !/^(BUTTON|A|INPUT|SELECT|TEXTAREA|TH|TD|SUMMARY|IMG|svg)$/.test(e.tagName) &&
    !e.matches('.btn, .form-control, .selectize-control, .selectize-input, .shiny-input-container') && (parseFloat(getComputedStyle(e).paddingLeft) || 0) < 4,
  // One rule for every marker: a 2 px outline round the control (6 px clear of words, 4 px of a control, 1 px inside a table
  // heading). A control's badge (24 px) is centred exactly on the outline's top-left corner (bottom-left at the very top of the
  // page); a label's or heading's badge sits 6 px to the left of its outline, centred on it, so it never covers a letter. The
  // number is centred on its cap height.
  mark: (e, n, side) => { if (!e) return 'missing ' + n;
    if (G.textlike(e)) { const t = document.createRange(); t.selectNodeContents(e); e = t; }
    const r = e.getBoundingClientRect(), sx = window.scrollX, sy = window.scrollY;
    const words = e instanceof Range, cell = /^T[HD]$/.test(e.tagName || ''), px = cell ? 1 : words ? 6 : 4, py = cell ? 1 : 4, d = 24;
    const rad = words ? 6 : Math.max(6, (parseFloat(getComputedStyle(e).borderTopLeftRadius) || 0) + px);
    const bx = r.left + sx - px, by = r.top + sy - py, bw = r.width + 2 * px, bh = r.height + 2 * py;
    const box = document.createElement('div'); box.className = 'gmk';
    Object.assign(box.style, { position: 'absolute', left: bx + 'px', top: by + 'px', width: bw + 'px', height: bh + 'px', boxSizing: 'border-box',
      border: '2px solid ' + G.ACC, borderRadius: rad + 'px', boxShadow: '0 0 0 4px rgba(208,135,112,0.22), 0 0 18px rgba(208,135,112,0.18)',
      zIndex: 99998, pointerEvents: 'none' });
    document.body.append(box);
    const top = by - d / 2 < window.scrollY + 2, k = 1;                      // k: the middle of the 2 px outline
    const pos = words ? [side === 'right' ? bx + bw + 6 : bx - d - 6, by + bh / 2 - d / 2]        // words: beside them, centred, never over a letter
                      : [bx + k - d / 2, (top ? by + bh - k : by + k) - d / 2];
    const c = document.createElement('div'); c.className = 'gmk';
    c.innerHTML = '<svg width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" style=\"display:block;overflow:visible\"><circle cx=\"12\" cy=\"12\" r=\"11\" fill=\"' + G.ACC +
      '\" stroke=\"#242933\" stroke-width=\"2\"/><text x=\"12\" y=\"16.6\" text-anchor=\"middle\" fill=\"#242933\" style=\"font:700 13px Inter,-apple-system,Helvetica,Arial,sans-serif;font-variant-numeric:tabular-nums\">' + n + '</text></svg>';
    Object.assign(c.style, { position: 'absolute', left: pos[0] + 'px', top: pos[1] + 'px', width: d + 'px', height: d + 'px',
      filter: 'drop-shadow(0 2px 3px rgba(0,0,0,0.45))', zIndex: 99999, pointerEvents: 'none' });
    document.body.append(c); return 'ok'; },
  // the words themselves (a range), for headings and labels whose element runs the full width
  tx: (txt, nth) => { const w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT); let n, k = 0;
    while ((n = w.nextNode())) { const i = n.nodeValue.indexOf(txt); if (i < 0 || !G.vis(n.parentElement)) continue; if (k++ < (nth || 0)) continue;
      const r = document.createRange(); r.setStart(n, i); r.setEnd(n, i + txt.length); return r; } },
  // a crop widened to its markers and kept left of the side panel
  fit: (r, wide) => { const ms = [...document.querySelectorAll('.gmk')].map(G.rect); let x0 = r[0], y0 = r[1], x1 = r[0] + r[2], y1 = r[1] + r[3];
    ms.forEach(m => { x0 = Math.min(x0, m[0]); y0 = Math.min(y0, m[1]); x1 = Math.max(x1, m[0] + m[2]); y1 = Math.max(y1, m[1] + m[3]); });
    const a = document.querySelector('aside.d-aside'); if (!wide && a && G.vis(a)) x1 = Math.min(x1, a.getBoundingClientRect().left + window.scrollX - 4);
    return [x0, y0, x1 - x0, y1 - y0]; },
  clear: () => document.querySelectorAll('.gmk').forEach(e => e.remove()),
  rect: e => { const r = e.getBoundingClientRect(); return [r.left + window.scrollX, r.top + window.scrollY, r.width, r.height]; },
  union: (...es) => { const rs = es.filter(Boolean).map(G.rect); const x = Math.min(...rs.map(r => r[0])), y = Math.min(...rs.map(r => r[1]));
    return [x, y, Math.max(...rs.map(r => r[0] + r[2])) - x, Math.max(...rs.map(r => r[1] + r[3])) - y]; },
  pane: () => [...document.querySelectorAll('.tab-pane.active')].filter(G.vis).filter(e => !e.parentElement.closest('.tab-pane.active')).pop(),   // the page, not a tab inside it
  card: (txt, nth) => { const h = G.find(txt, 'h1, h2, h3, h4, h5, .card-header, summary, legend, b, strong', nth); return h && (h.closest('.card, .well, .d-card, section, fieldset, details') || h.parentElement); },
  row: async (txt, ms) => { const td = G.find(txt, 'td'); if (td) td.click(); await G.wait(ms || 800); return td; },
  // the guide project lives in a temporary folder: show the folders as they would be on a computer set up as ovid suggests
  paths: (dir, to) => { const fix = v => v.split(dir + '/fresh').join(to + '/project').split(dir + '/proj').join(to + '/project').split(dir).join(to);
    document.querySelectorAll('input, textarea').forEach(e => { if (e.value && e.value.includes(dir)) e.value = fix(e.value); });
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT); let n; while ((n = w.nextNode())) if (n.nodeValue.includes(dir)) n.nodeValue = fix(n.nodeValue); }
};"

js <- function(b, code, await = TRUE) {
  r <- b$Runtime$evaluate(paste0("(async () => {", code, "})()"), awaitPromise = await, returnByValue = TRUE, timeout_ = 180)
  if (!is.null(r$exceptionDetails)) stop("in the page: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# 1180 px wide, so the app's text reads larger on the page; the full top bar and the Workbench are taken 1440 px wide.
open_app <- function(url, width = 1180, height = 900) {
  b <- ChromoteSession$new(width = width, height = height)
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 2, mobile = FALSE)
  b$Emulation$setEmulatedMedia(features = list(list(name = "prefers-color-scheme", value = "dark")))
  b$Page$navigate(url); b$Page$loadEventFired(wait_ = TRUE); Sys.sleep(6)
  js(b, JS_HELPERS); b
}

# One figure: `prep` runs in the page (it may go to a page, click and wait), then `marks` (JS returning 'ok' per marker),
# then the crop: `clip` is JS returning [x, y, w, h] in page pixels.
# wide: the whole width with the rail and the side panel (else the rail is hidden and the crop stops at the side panel).
shot <- function(b, name, prep, marks = "", clip, pad = 16, wide = FALSE) {
  js(b, "G.clear();"); js(b, prep)
  js(b, sprintf("const r = document.querySelector('nav.d-rail'); if (r) r.style.visibility = '%s';", if (wide) "" else "hidden"))
  js(b, sprintf("G.paths(%s, '~/Projects/Ovid/Projects/WLR');", jsonlite::toJSON(normalizePath(GUIDE_DIR), auto_unbox = TRUE)))
  m <- js(b, paste0("return [", marks, "].join(',');"))
  if (grepl("missing", m)) warning(name, ": ", m)
  r <- unlist(js(b, paste0("return G.fit(", clip, ", ", tolower(wide), ");")))
  vw <- js(b, "return document.documentElement.scrollWidth;")
  x <- max(0, r[1] - pad); w <- min(vw - x, r[3] + 2 * pad)
  png <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE,
                                  clip = list(x = x, y = max(0, r[2] - pad), width = w, height = r[4] + 2 * pad, scale = 1))
  writeBin(jsonlite::base64_dec(png$data), file.path(OUT, paste0(name, ".png")))
  js(b, "G.clear();"); cat(sprintf("  %-28s %s\n", name, m))
}

# ---- The figures ---------------------------------------------------------------------------------------------------------
# Run the pipeline from the page, answering the time-zone question with the zone it offers.
JS_RUN <- "document.getElementById('run').click(); for (let i = 0; i < 180 && !G.q('.modal:is(.in,.show) .modal-content'); i++) await G.wait(500);
  const z = [...document.querySelectorAll('.modal:is(.in,.show) button')].find(e => /zone and run/.test(e.innerText)); if (z) z.click();
  for (let i = 0; i < 60 && !/Up to date/.test(document.body.innerText); i++) await G.wait(1000); await G.wait(2000);"

QUICK <- list(   # the fresh project (port 8791), in this order: the run happens between qs_02 and qs_03
  list(name = "wlr_qs_00", prep = "await G.go('Project');",
       marks = "G.mark(G.tx('Bring in'),1,'right'), G.mark(G.tx('More'),2,'right'), G.mark(G.q('#run'),3,'below'), G.mark(G.tx('Not run yet') || G.tx('Set-up changed') || G.tx('Up to date'),4,'below'), G.mark(G.find('Standard','a'),5,'below')",
       clip = "[0, 0, document.documentElement.clientWidth, 420]", wide = TRUE),
  list(name = "wlr_qs_01", prep = "await G.go('Project');",
       marks = "G.mark(G.find('Browse…','button',0),1), G.mark(G.find('Browse…','button',1),2), G.mark(G.find('Browse…','button',3),3)",
       clip = "G.rect(G.card('Folders'))"),
  list(name = "wlr_qs_02", prep = "document.getElementById('run').click(); for (let i = 0; i < 180 && !G.q('.modal:is(.in,.show) .modal-content'); i++) await G.wait(500); await G.wait(800);",
       marks = "G.mark(G.find('UTC-7','label'),1), G.mark(G.find('Use this zone and run','button'),2,'above')",
       clip = "G.rect(G.q('.modal:is(.in,.show) .modal-content'))", pad = 2),
  list(name = "wlr_qs_03", prep = paste("const z = [...document.querySelectorAll('.modal:is(.in,.show) button')].find(e => /zone and run/.test(e.innerText)); if (z) z.click();",
                                         "for (let i = 0; i < 60 && !/Up to date/.test(document.body.innerText); i++) await G.wait(1000); await G.go('Files', 3000);"),
       marks = "G.mark(G.find('Status','th'),1,'above'), G.mark(G.find('Kind','th'),2,'above'), G.mark(G.tx('Sort the selected files'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_qs_04", prep = "await G.go('Metadata', 3000); G.find('Add what was found','button').click(); await G.wait(2500);",
       marks = "G.mark(G.find('Add what was found','button'),1,'left'), G.mark(G.find('Site','th'),2,'above'), G.mark(G.find('Save changes','button'),3,'below')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_qs_05", prep = "await G.go('Needs', 3000);",
       marks = "G.mark(G.find('Accept the suggested matches','button'),1), G.mark(G.tx('What the last run found'),2)",
       clip = "G.rect(G.pane())")
)

HOWTO <- list(   # the set-up project (port 8790), after one run
  list(name = "wlr_qs_06", prep = "await G.go('Site', 4000);",
       marks = "G.mark(G.q('#site_pick + .selectize-control, #site_pick'),1,'below'), G.mark(G.find('half-hourly','a, button, label, span'),2,'below'), G.mark(G.find('PROBLEMS','div'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_qs_07", prep = "await G.go('Export', 3000); const a = G.find('Select all sites', 'a, button'); if (a) a.click(); await G.wait(2000);",
       marks = "G.mark(G.find('Sites','label'),1), G.mark(G.find('Format','label'),2), G.mark(G.find('Export','button, a'),3)",
       clip = "G.union(G.tx('Sites'), G.find('Export','button, a'), G.tx('Select all sites'))"),
  list(name = "wlr_ht_01", prep = "await G.go('Files', 3000); await G.row('2000002_SiteB.xle');",
       marks = "G.mark(G.q('#run'),1,'below'), G.mark(G.find('2000002_SiteB.xle','td'),2), G.mark(G.find('Rescan selected files','button'),3)",
       clip = "G.union(G.q('#run'), G.pane())"),
  list(name = "wlr_ht_02", prep = "await G.go('Files', 3000); await G.row('1000004_Baro.xle');",
       marks = "G.mark(G.find('1000004_Baro.xle','td'),1), G.mark(G.find('Reason','label'),2), G.mark(G.find('Leave out','button'),3,'below'), G.mark(G.find('It is a barologger','button'),4,'below')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_07", prep = "await G.go('Clock', 3000);",
       marks = "G.mark(G.find('read as','th'),1,'above'), G.mark(G.tx('Time-zone table'),2), G.mark(G.find('Add row','button',0),3,'below')",
       clip = "G.union(G.tx('Files and the zone'), G.find('Add row','button',0), G.q('.tab-pane.active table'))"),
  list(name = "wlr_ht_08", prep = "await G.go('Clock', 3000); G.tx('Clock corrections').startContainer.parentElement.scrollIntoView();",
       marks = "G.mark(G.find('Clock corrections','h3, h4, h5, b, strong, div'),1), G.mark(G.find('Add row','button',1),2,'below'), G.mark(G.find('Save changes','button',1),3,'below')",
       clip = "G.rect(G.card('Clock corrections'))"),
  list(name = "wlr_ht_03", prep = "await G.go('Metadata', 3000); await G.row('2000002');",
       marks = "G.mark(G.find('Site','th'),1,'above'), G.mark(G.find('From','th'),2,'above'), G.mark(G.find('To','th'),3,'above'), G.mark(G.find('Save changes','button'),4,'below')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_04", prep = "await G.go('Needs', 3000);",
       marks = "G.mark(G.find('Accept the suggested matches','button'),1), G.mark(G.tx('What the last run found'),2)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_05", prep = "await G.go('Shown', 3000);",
       marks = "G.mark(G.find('Preview','button'),1,'below'), G.mark(G.find('Rename','button'),2,'below'), G.mark(G.find('Shown','th'),3,'above')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_06", prep = "await G.go('Deploy', 3000);",
       marks = "G.mark(G.find('T_ST','th'),1,'above'), G.mark(G.tx('Corrections to the workbook'),2), G.mark(G.find('Add row','button'),3,'below')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_09", prep = "await G.go('Baro', 3000);",
       marks = "G.mark(G.find('Primary barologger','label'),1), G.mark(G.find('Secondary','label'),2), G.mark(G.find('Save choices','button'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_10", prep = "await G.go('Datum', 3000); G.find('Logger offsets','a').click(); await G.wait(1500);",
       marks = "G.mark(G.find('Logger offsets','a'),1,'above'), G.mark(G.tx('Set the offset for'),2), G.mark(G.tx('Measured tip to sensor (cm)'),3), G.mark(G.find('Set offset','button'),4)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_11", prep = "await G.go('Visits', 3000); G.find('Show deployments within the limit too', 'label, span').click(); await G.wait(1500);",
       marks = "G.mark(G.tx('Manual measurements'),1), G.mark(G.find('Add row','button'),2,'below'), G.mark(G.tx('Tape against logger'),3), G.mark(G.find('Keep selected as they are','button'),4)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_13", prep = "await G.go('QC', 3000);",
       marks = "G.mark(G.find('Min','th'),1,'above'), G.mark(G.find('Action','th'),2,'above'), G.mark(G.find('Save changes','button'),3,'below')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_14", prep = "await G.go('Corrections', 3000);",
       marks = "G.mark(G.find('State','th'),1,'above'), G.mark(G.find('Where to change it','th'),2,'above')",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_16", prep = "await G.go('Audit', 3000);",
       marks = "G.mark(G.tx('1. What was done'),1), G.mark(G.tx('2. Where it happened'),2), G.mark(G.tx('3. Reconciliation'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_17", prep = "await G.go('Preflight', 3000);",
       marks = "G.mark(G.find('Check again','button'),1,'below'), G.mark(G.find('Fix','th'),2,'above'), G.mark(G.tx('Hang lengths (T_ST)'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_18", prep = "await G.go('Export', 3000); const a = G.find('Select all sites', 'a, button'); if (a) a.click(); await G.wait(2000);",
       marks = "G.mark(G.find('Values','label'),1), G.mark(G.find('Time step','label'),2), G.mark(G.find('Quality flags','label'),3), G.mark(G.find('Format','label'),4), G.mark(G.find('Export','button, a'),5)",
       clip = "G.union(G.tx('Sites'), G.find('Export','button, a'), G.tx('Select all sites'))"),
  list(name = "wlr_ht_19", prep = "await G.go('Config', 3000);",
       marks = "G.mark(G.find('Export configuration (ZIP)','button, a'),1), G.mark(G.find('Browse...','span, label, button'),2), G.mark(G.find('Apply the import','button'),3)",
       clip = "G.rect(G.pane())"),
  list(name = "wlr_ht_20", width = 1440, prep = "await G.go('Site', 3000);",
       marks = "G.mark(G.q('#ovid_troubleshoot'),1,'below'), G.mark(G.q('a[title^=Help], a[data-original-title^=Help], a[data-bs-original-title^=Help]'),2,'below')",
       clip = "G.union(G.q('#run'), G.q('a[title=Settings]'))", wide = TRUE),
  list(name = "wlr_ht_15", prep = "await G.go('Explore', 4000);",
       marks = "G.mark(G.find('Site','label'),1), G.mark(G.find('Deployment','label'),2), G.mark(G.find('Save data','button'),3,'below'), G.mark(G.tx('Flags on this record'),4)",
       clip = "G.rect(G.pane())")
)

WORKBENCH <- list(   # the set-up project in the Workbench (?ui=workbench)
  list(name = "wlr_ht_12", prep = "await G.wait(4000);",
       marks = "G.mark(G.tx('Loggers'),1,'right'), G.mark(G.tx('Shift + drag selects'),2,'below'), G.mark(G.find('✕ Remove','button'),3,'above'), G.mark(G.tx('No cell selected'),4,'left')",
       clip = "[0, 0, document.documentElement.clientWidth, document.documentElement.clientHeight]", wide = TRUE)
)

if (sys.nframe() == 0) {
  want <- commandArgs(TRUE); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  pick <- function(l) Filter(function(s) !length(want) || s$name %in% want, l)
  b <- open_app("http://127.0.0.1:8791", height = 2400); for (s in pick(QUICK)) do.call(shot, c(list(b), s)); b$close()
  b <- open_app("http://127.0.0.1:8790", height = 2400); if (!grepl("Up to date", js(b, "return document.body.innerText;"))) js(b, JS_RUN)
  narrow <- Filter(function(s) is.null(s$width), pick(HOWTO)); wide <- Filter(function(s) !is.null(s$width), pick(HOWTO))
  for (s in narrow) do.call(shot, c(list(b), s)); b$close()
  if (length(wide)) { b <- open_app("http://127.0.0.1:8790", width = 1440); for (s in wide) do.call(shot, c(list(b), s[names(s) != "width"])); b$close() }
  if (length(pick(WORKBENCH))) { b <- open_app("http://127.0.0.1:8790/?ui=workbench&site=Well+A", width = 1440); for (s in pick(WORKBENCH)) do.call(shot, c(list(b), s)); b$close() }
}
