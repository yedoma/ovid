# docs/build_guides.R  --  The HTML and PDF of the RS quick start and how-to guide, from their .md, in the reference guide's
# colours and type (its <style>), each section a card; then each PDF printed with headless Chrome.
#
#   Rscript docs/build_guides.R          (from the RS folder; needs commonmark, chromote and Google Chrome or Chromium)
#
# Figures (docs/img/guide) are taken from the app's test project (tests/golden), never from field data; the map imagery is hidden.

suppressMessages({ library(commonmark); library(chromote) })
APP <- "RS"; DOCS <- "docs"
REF_HTML <- file.path(DOCS, paste0(APP, "_reference_guide.html"))

GUIDE_CSS <- "
section.task{border:1px solid var(--rule);border-radius:10px;padding:4px 22px 14px;margin:26px 0;background:var(--panel)}
section.task h2{border-top:0;margin:14px 0 10px;padding:0 0 8px;border-bottom:1px solid var(--rule)}
figure{margin:14px 0;padding:8px;background:var(--bg);border:1px solid var(--rule);border-radius:8px;break-inside:avoid;page-break-inside:avoid}
figure img{display:block;max-width:100%;height:auto;border-radius:4px}
figcaption{font-size:13px;color:var(--muted);margin:6px 2px 0}
p.co{border-left:3px solid var(--accent);padding:4px 12px;margin:10px 0;background:rgba(255,255,255,.03)}
p.co.result{border-left-color:var(--green)} p.co.check{border-left-color:var(--blue)} p.co.note{border-left-color:var(--yellow,var(--amber))}
.foot{margin-top:32px;font-size:12.5px;color:var(--muted)}
@media print{section.task{break-inside:auto;border:0;background:none;padding:0;margin:0 0 8mm}section.task h2{break-after:avoid}figure,p.co,li{break-inside:avoid}}"

ref_head_style <- function() { x <- paste(readLines(REF_HTML, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  sub("(?s).*?<style>(.*?)</style>.*", "\\1", x, perl = TRUE) }

build_guide <- function(md) {
  src <- readLines(md, warn = FALSE, encoding = "UTF-8"); title <- sub("^# ", "", src[1])
  b <- markdown_html(paste(src, collapse = "\n"), extensions = TRUE)
  b <- gsub("<p><strong>(When|Result|Check|Note):</strong>", "<p class=\"co \\L\\1\"><strong>\\1:</strong>", b, perl = TRUE)
  b <- gsub("<figure><img src=\"([^\"]+)\" alt=\"([^\"]+)\"([^>]*)></figure>", "<figure><img src=\"\\1\" alt=\"\\2\"\\3><figcaption>\\2</figcaption></figure>", b)
  parts <- strsplit(gsub("<h2", "\001<h2", b, fixed = TRUE), "\001", fixed = TRUE)[[1]]
  body <- c(parts[1], vapply(parts[-1], function(x) sprintf("<section class=\"task\">\n%s</section>", x), ""),
            sprintf("<p class=\"foot\">ovid %s. For what %s does to the data and why, see the <a href=\"%s_reference_guide.html\">%s reference guide</a>.</p>", APP, APP, APP, APP))
  out <- sub("\\.md$", ".html", md)
  writeLines(enc2utf8(c("<!doctype html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">", paste0("<title>", title, "</title>"),
    "<style>", ref_head_style(), GUIDE_CSS, "</style>", "</head>", "<body><main>", body, "</main></body>", "</html>")), out, useBytes = TRUE)
  out
}

print_pdf <- function(html) {
  b <- ChromoteSession$new(); on.exit(b$close())
  loaded <- b$Page$loadEventFired(wait_ = FALSE); b$Page$navigate(paste0("file://", normalizePath(html))); b$wait_for(loaded); Sys.sleep(1.5)
  p <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE, timeout_ = 120)
  out <- sub("\\.html$", ".pdf", html); writeBin(jsonlite::base64_dec(p$data), out); out
}

if (sys.nframe() == 0)
  for (md in file.path(DOCS, paste0(APP, c("_quick_start.md", "_how_to.md")))) { h <- build_guide(md); cat(" ", h, "->", print_pdf(h), "\n") }
