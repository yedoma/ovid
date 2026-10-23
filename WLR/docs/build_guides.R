# docs/build_guides.R  --  The HTML and PDF of the WLR quick start and how-to guide, from their .md, and the reference
# guide's quick start and defaults sections brought into its HTML (the rest of that HTML is kept as it is), then every PDF printed.
#
#   Rscript docs/build_guides.R          (from the WLR folder; needs commonmark, chromote and Google Chrome or Chromium)
#
# The guides take the reference guide's colours and type, so the three read as one set; figures (docs/img/guide) come
# from docs/guide_shots.R.

suppressMessages({ library(commonmark); library(chromote) })
DOCS <- "docs"
REF_HTML <- file.path(DOCS, "WLR_reference_guide.html")

ref_style <- function() { x <- readLines(REF_HTML, warn = FALSE, encoding = "UTF-8")
  paste(x[seq(grep("<style>", x, fixed = TRUE)[1] + 1, grep("</style>", x, fixed = TRUE)[1] - 1)], collapse = "\n") }
# The guides' own design on the reference guide's colours: a title block, a contents of linked task cards, each task a card
# with its number, callouts for When / Result / Check / Note, the figure markers (1) drawn as the same badges as in the
# figures, figure captions, and a running footer with the page number in the PDF.
GUIDE_CSS <- "
:root{--mk:#D08770;--mk-ink:#242933;--ink:#eceff4;--soft:#d8dee9;--rule2:rgba(255,255,255,.09);--sheet:#323846;--well:#272c36;--frost:#8fbcbb;--num:#ebcb8b}
body{font:16px/1.7 Inter,-apple-system,'Segoe UI',Helvetica,Arial,sans-serif;color:var(--ink);-webkit-font-smoothing:antialiased;text-rendering:optimizeLegibility}
main{max-width:900px;padding:40px 24px 80px}
p,li{max-width:68ch}main p{margin:10px 0}
/* title page block */
.head{padding:0 0 22px;margin:0 0 26px;border-bottom:1px solid var(--rule2)}
.kicker{font:600 11.5px/1 ui-monospace,Menlo,Consolas,monospace;letter-spacing:.16em;text-transform:uppercase;color:var(--accent);margin:0 0 12px}
.head h1{font-size:34px;line-height:1.12;margin:0 0 10px;letter-spacing:-.015em;font-weight:700}
.head .lede{color:var(--soft);font-size:17px;line-height:1.6;margin:0 0 16px;max-width:62ch}
.meta{display:flex;flex-wrap:wrap;gap:8px;margin:0}
.meta span{font-size:12px;color:var(--soft);border:1px solid var(--rule2);border-radius:999px;padding:3px 11px;background:rgba(255,255,255,.03)}
/* contents */
.toc{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin:20px 0 10px;background:none;padding:0;border-radius:0}
.toc .grp{border:1px solid var(--rule2);border-radius:10px;padding:14px 16px;background:var(--sheet)}
.toc .grp h4{margin:0 0 8px;font-size:11px;letter-spacing:.14em;text-transform:uppercase;color:var(--accent)}
.toc ol{margin:0;padding:0;list-style:none;columns:1}
.toc li{margin:4px 0;font-size:14.5px;line-height:1.45;display:flex;gap:10px}
.toc li b{color:var(--soft);font-variant-numeric:tabular-nums;min-width:1.6em;text-align:right;font-weight:600}
.toc a{color:var(--ink);text-decoration:none}
/* sections: a light sheet, the heading set off by a rule */
section.task{background:transparent;border:1px solid var(--rule2);border-radius:12px;padding:6px 26px 18px;margin:30px 0;box-shadow:0 1px 0 rgba(255,255,255,.03) inset}
section.task h2{border-top:none;margin:16px 0 14px;padding:0 0 12px;border-bottom:1px solid var(--rule2);font-size:21px;line-height:1.3;font-weight:650;display:flex;align-items:center;gap:12px;letter-spacing:-.005em}
section.task h2 .no{flex:none;display:inline-flex;align-items:center;justify-content:center;min-width:28px;height:28px;padding:0 7px;border-radius:7px;
  background:rgba(136,192,208,.14);color:var(--accent);border:1px solid rgba(136,192,208,.45);font-size:13.5px;font-weight:700;font-variant-numeric:tabular-nums}
/* text roles: app controls, words to type, numbers with units */
strong,b{color:#fff;font-weight:650}
strong.ui{font-weight:600;color:#fff;background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.14);border-bottom-width:2px;border-radius:5px;padding:0 5px;white-space:nowrap}
strong.lead{color:#fff}
code{font:500 .86em/1.4 'JetBrains Mono',ui-monospace,Menlo,Consolas,monospace;color:var(--frost);background:var(--well);border:1px solid rgba(255,255,255,.07);
  border-radius:5px;padding:1px 6px;box-shadow:inset 0 1px 2px rgba(0,0,0,.35);white-space:nowrap}
.num{font-variant-numeric:tabular-nums lining-nums;color:var(--num);font-weight:550;white-space:nowrap}
/* callouts */
.co{border-radius:8px;padding:10px 14px 10px 16px;margin:14px 0;border:1px solid var(--rule2);border-left:3px solid var(--rule);background:rgba(255,255,255,.025);max-width:none;line-height:1.6}
.co>strong:first-child{display:block;margin:0 0 2px;font-size:11px;letter-spacing:.14em;text-transform:uppercase;font-weight:700;background:none;border:0;padding:0}
.co.when{border-left-color:var(--blue)}.co.when>strong:first-child{color:#d8dee9}
.co.result{border-left-color:var(--green)}.co.result>strong:first-child{color:var(--green)}
.co.check{border-left-color:var(--accent)}.co.check>strong:first-child{color:var(--accent)}
.co.note{border-left-color:var(--amber);background:rgba(235,203,139,.06)}.co.note>strong:first-child{color:var(--amber)}
/* steps */
main ol:not(.plain){padding-left:0;list-style:none;counter-reset:st}
main ol:not(.plain)>li{counter-increment:st;position:relative;padding-left:38px;margin:10px 0}
main ol:not(.plain)>li::before{content:counter(st);position:absolute;left:0;top:3px;width:24px;height:24px;border-radius:6px;background:var(--sheet);border:1px solid var(--rule2);
  color:var(--soft);font-size:12px;font-weight:700;display:flex;align-items:center;justify-content:center;font-variant-numeric:tabular-nums}
main ol.markers>li::before{border-radius:50%;background:var(--mk);color:var(--mk-ink);border:2px solid var(--mk-ink)}
.mk{display:inline-flex;align-items:center;justify-content:center;width:19px;height:19px;border-radius:50%;background:var(--mk);color:var(--mk-ink);
  font:700 11px/1 -apple-system,'Segoe UI',Helvetica,Arial,sans-serif;vertical-align:1px;margin:0 1px;font-variant-numeric:tabular-nums}
main ul{padding-left:22px}main ul li{margin:8px 0}
/* figures, as in a textbook: a matted plate with a numbered caption */
main figure{margin:18px 0 12px;padding:10px;background:var(--well);border:1px solid var(--rule2);border-radius:12px;box-shadow:0 8px 24px rgba(0,0,0,.28);break-inside:avoid;page-break-inside:avoid}
main img{display:block;max-width:100%;height:auto;border-radius:6px;border:1px solid rgba(255,255,255,.06)}
main figcaption{font-size:13px;color:var(--soft);margin:9px 4px 1px;line-height:1.45}
main figcaption b{color:#fff;font-weight:650;margin-right:4px}
.foot{margin-top:40px;padding-top:14px;border-top:1px solid var(--rule2);font-size:12.5px;color:var(--soft)}
@media (max-width:700px){.toc{grid-template-columns:1fr}section.task{padding:4px 14px 12px}}
@page{size:A4;margin:22mm 0 20mm;background:#2e3440;@bottom-left{content:'ovid WLR';color:#c0c8d6;font:9px -apple-system,Helvetica,Arial,sans-serif;padding-left:17mm}@bottom-right{content:counter(page) ' / ' counter(pages);color:#c0c8d6;font:9px -apple-system,Helvetica,Arial,sans-serif;padding-right:17mm}}
@media print{html,body{background:var(--bg)}body{font-size:11.2pt;line-height:1.62;orphans:3;widows:3}main{padding:0 17mm}h1{font-size:28px}
  section.task{border:0;border-radius:0;padding:0;margin:0 0 10mm;box-shadow:none;break-before:auto}
  section.task h2{break-after:avoid;margin-top:0;padding-top:4mm}section.task h2,section.task .co.when{break-after:avoid}
  .toc,.grp,.co,figure,img,li,p,.head{break-inside:avoid;page-break-inside:avoid}figure{box-shadow:none}li{line-height:1.5}.head{margin-bottom:18px}}"

md_body <- function(md) markdown_html(paste(readLines(md, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), extensions = TRUE)
APP_VER <- local({ x <- grep("^APP_VERSION <- ", readLines("wlr_pipeline.R", warn = FALSE), value = TRUE)[1]; sub('.*"([^"]+)".*', "\\1", x) })

# The Markdown's HTML turned into the guide's design.
guide_html <- function(b, markers = FALSE) {
  b <- gsub("<p><strong>(When|Result|Check|Note):</strong>", "<p class=\"co \\L\\1\"><strong>\\1</strong>", b, perl = TRUE)
  b <- gsub("<p class=\"co (when|result|check|note)\"><strong>(\\w+)</strong>", "<p class=\"co \\1\"><strong>\\2</strong>", b, perl = TRUE)
  b <- gsub("<figure><img src=\"([^\"]+)\" alt=\"([^\"]+)\"([^>]*)></figure>", "<figure><img src=\"\\1\" alt=\"\\2\"\\3><figcaption>\\2</figcaption></figure>", b)
  if (markers) b <- gsub("<ol>", "<ol class=\"markers\">", b, fixed = TRUE)   # quick start: each numbered item is a marker in its figure
  b <- gsub("([^\\w(])\\(([1-9])\\)", "\\1<span class=\"mk\">\\2</span>", b, perl = TRUE)   # (3) in a step: the figure's marker
  b <- gsub("<strong>([^<]*[.:])</strong>", "<strong class=\"lead\">\\1</strong>", b, perl = TRUE)        # a lead-in term
  b <- gsub("<strong>([^<]+)</strong>", "<strong class=\"ui\">\\1</strong>", b, perl = TRUE)              # a page, button or field in the app
  b <- gsub("<p class=\"co (when|result|check|note)\"><strong class=\"ui\">", "<p class=\"co \\1\"><strong>", b, perl = TRUE)
  # numbers with a unit, and signed offsets, outside tags and code
  seg <- strsplit(gsub("<", "\001<", b, fixed = TRUE), "\001", fixed = TRUE)[[1]]; incode <- FALSE
  for (k in seq_along(seg)) { x <- seg[k]
    if (startsWith(x, "<code")) incode <- TRUE else if (startsWith(x, "</code")) incode <- FALSE
    if (incode || startsWith(x, "<figure") || startsWith(x, "<img")) next
    t <- sub("^(<[^>]*>)?", "", x); h <- substring(x, 1, nchar(x) - nchar(t))
    t <- gsub("(?<![\\w/.-])(\\d[\\d,]*(?:\\.\\d+)?) to (?=\\d[\\d,]*(?:\\.\\d+)? ?(?:kPa|cm|mm|m|h|\u00b0C)\\b)", "<span class=\"num\">\\1</span> to ", t, perl = TRUE)   # a range
    t <- gsub("(?<![\\w/.-])([\u2212-]?\\d[\\d,]*(?:\\.\\d+)?(?:\u00a0| )?(?:kg/m\u00b3|\u00b5S/cm|kPa|\u00b0C|cm|mm|m|h|px|%|days?|hours?))(?![\\w/])",
              "<span class=\"num\">\\1</span>", t, perl = TRUE)
    t <- gsub("(e\\.g\\. )([\u2212-]\\d+)", "\\1<span class=\"num\">\\2</span>", t, perl = TRUE)
    seg[k] <- paste0(h, t) }
  b <- paste(seg, collapse = "")
  n <- 0L; m <- gregexpr("<figcaption>", b, fixed = TRUE)[[1]]
  if (m[1] > 0) for (pos in rev(m)) { n <- length(m) - match(pos, rev(m)) + 1L
    b <- paste0(substring(b, 1, pos + 11), sprintf("<b>Figure %d.</b> ", n), substring(b, pos + 12)) }
  b }

# The how-to's contents: the grouped list of tasks as linked cards.
toc_html <- function(b) {
  i <- regexpr("<h2>Contents</h2>", b, fixed = TRUE); if (i < 0) return(b)
  j <- regexpr("<h2>", substring(b, i + 1), fixed = TRUE) + i
  ps <- regmatches(substring(b, i, j - 1), gregexpr("<p><strong>[^<]+:</strong>[^<]+</p>", substring(b, i, j - 1)))[[1]]
  grp <- vapply(ps, function(p) { g <- sub("<p><strong>([^<]+):</strong>.*", "\\1", p); items <- strsplit(sub(".*</strong>\\s*([^<]+)</p>", "\\1", p), ";\\s*")[[1]]
    li <- vapply(items, function(t) { n <- sub("^(\\d+)\\..*", "\\1", trimws(t)); sprintf("<li><b>%s</b><a href=\"#task-%s\">%s</a></li>", n, n, sub("^\\d+\\.\\s*", "", trimws(t))) }, "")
    sprintf("<div class=\"grp\"><h4>%s</h4><ol class=\"plain\">%s</ol></div>", g, paste(li, collapse = "")) }, "")
  paste0(substring(b, 1, i - 1), "<nav class=\"toc\">", paste(grp, collapse = ""), "</nav>\n", substring(b, j)) }

# A guide: the .md in the guide's design, written beside it.
build_guide <- function(md) {
  b <- md_body(md); title <- sub("^# ", "", readLines(md, n = 1, warn = FALSE, encoding = "UTF-8"))
  b <- sub("<h1>[^<]*</h1>\n", "", b)
  lede <- sub("^<p>(.*?)</p>.*", "\\1", b); b <- sub("^<p>.*?</p>\n", "", b)
  b <- toc_html(b); b <- guide_html(b, markers = !grepl("how_to", md))
  b <- gsub("<h2>(\\d+)\\. ([^<]+)</h2>", "<h2 id=\"task-\\1\"><span class=\"no\">\\1</span>\\2</h2>", b)   # how-to tasks
  b <- gsub("<h2>(\\d+) ([^<]+)</h2>", "<h2><span class=\"no\">\\1</span>\\2</h2>", b)                       # quick start: the app's step number
  parts <- strsplit(gsub("<h2", "\001<h2", b, fixed = TRUE), "\001", fixed = TRUE)[[1]]   # strsplit() with a look-ahead eats a character
  body <- paste0("<div class=\"intro\">", parts[1], "</div>\n",
    paste0(vapply(parts[-1], function(x) sprintf("<section class=\"task\">\n%s</section>\n", x), ""), collapse = ""))
  kind <- if (grepl("how_to", md)) "How-to guide" else "Quick start"
  head <- sprintf("<header class=\"head\"><p class=\"kicker\">ovid \u00b7 WLR \u00b7 %s</p><h1>%s</h1><p class=\"lede\">%s</p><p class=\"meta\"><span>WLR %s</span><span>%s</span><span>Figures: constructed example, not field data</span></p></header>",
                  kind, sub("^ovid WLR ", "", title) |> (\(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2)))(), lede, APP_VER, format(Sys.Date(), "%d %B %Y"))
  foot <- "<p class=\"foot\">ovid WLR. For what WLR does to the data and why, see the WLR reference guide; every figure can be retaken with docs/guide_shots.R.</p>"
  out <- sub("\\.md$", ".html", md)
  writeLines(c("<!doctype html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
               "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">", paste0("<title>", title, "</title>"),
               "<style>", ref_style(), sub("content:'ovid WLR'", sprintf("content:'%s'", title), GUIDE_CSS, fixed = TRUE), "</style>", "</head>", "<body><main>", head, body, foot, "</main></body>", "</html>"), out, useBytes = TRUE)
  out
}

# The reference guide: the sections below rendered from the .md into its HTML (the rest of that HTML is kept as it is). A
# section the HTML has is replaced, between its <h2> and the next; one it lacks goes in before the section that follows it.
REF_SYNC <- c(quick = "Quick start", defaults = "13. ovid defaults with no published source")
build_reference <- function(md = file.path(DOCS, "WLR_reference_guide.md")) {
  x <- readLines(md, warn = FALSE, encoding = "UTF-8"); h <- grep("^## ", x); titles <- sub("^## ", "", x[h])
  for (id in names(REF_SYNC)) {
    i <- h[titles == REF_SYNC[[id]]]; j <- c(h[h > i], length(x) + 1)[1] - 1
    sec <- c(sprintf("<h2 id=\"%s\">%s</h2>", id, REF_SYNC[[id]]), markdown_html(paste(x[(i + 1):j], collapse = "\n"), extensions = TRUE))
    html <- readLines(REF_HTML, warn = FALSE, encoding = "UTF-8"); hs <- grep("^<h2", html)
    a <- grep(sprintf("<h2 id=\"%s\">", id), html, fixed = TRUE)
    if (length(a)) { b <- c(hs[hs > a], length(html) + 1)[1]; html <- c(html[seq_len(a - 1)], sec, html[b:length(html)]) }
    else { nxt <- titles[match(REF_SYNC[[id]], titles) + 1]; b <- hs[grepl(paste0(">", nxt, "<"), html[hs], fixed = TRUE)][1]
           html <- c(html[seq_len(b - 1)], sec, html[b:length(html)])
           nid <- sub('.*id="([^"]+)".*', "\\1", html[b + length(sec)]); li <- grep(sprintf('href="#%s"', nid), html, fixed = TRUE)[1]   # and in the contents
           if (!is.na(li)) html <- append(html, sprintf('<li><a href="#%s">%s</a></li>', id, REF_SYNC[[id]]), li - 1) }
    writeLines(html, REF_HTML, useBytes = TRUE) }
  REF_HTML
}

print_pdf <- function(html) {
  b <- ChromoteSession$new(); on.exit(b$close())
  loaded <- b$Page$loadEventFired(wait_ = FALSE); b$Page$navigate(paste0("file://", normalizePath(html)))
  b$wait_for(loaded); Sys.sleep(1)
  p <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE, timeout_ = 120)   # the guides' margins, page colour and footer are in @page
  out <- sub("\\.html$", ".pdf", html); writeBin(jsonlite::base64_dec(p$data), out); out
}

if (sys.nframe() == 0) {
  made <- c(build_guide(file.path(DOCS, "WLR_quick_start.md")), build_guide(file.path(DOCS, "WLR_how_to.md")), build_reference())
  for (h in made) cat(" ", h, "->", print_pdf(h), "\n")
}
