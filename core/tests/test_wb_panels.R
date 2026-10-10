# core/tests/test_wb_panels.R  --  The Workbench side panels (workbench.R wb_page, wb_panel_head, wb_strip, wb_grip; workbench.js
# wbPanels): fold buttons, strips, resize handles, the float button and their labels.   Rscript core/tests/test_wb_panels.R
suppressMessages({ library(data.table); library(shiny) })
CORE_DIR <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else "core" })
for (.f in c("util", "theme", "shell", "projects", "workbench")) source(file.path(CORE_DIR, paste0(.f, ".R")))
RES <- new.env(); RES$ok <- 0L; RES$fail <- character()
check <- function(what, cond) { if (isTRUE(cond)) { RES$ok <- RES$ok + 1L; cat("  ok   ", what, "\n") } else { RES$fail <- c(RES$fail, what); cat("  FAIL ", what, "\n") } }
pal <- list(bg = "#222222", bg1 = "#2a2a2a", bg2 = "#333333", bg3 = "#444444", fg = "#dddddd", fg4 = "#aaaaaa", gray = "#888888", accent = "#e5c07b",
            red = "#e06c75", green = "#98c379", blue = "#61afef", gold = "#d19a66", teal = "#56b6c2", purple = "#c678dd", bone = "#cccccc")
h <- as.character(htmltools::renderTags(wb_page("X", pal, nav = p("tree rows"), main = p("main"), aside = p("inspector rows")))$html)
has <- function(...) all(vapply(c(...), function(s) grepl(s, h, fixed = TRUE), NA))
pos <- function(s) regexpr(s, h, fixed = TRUE)[1]

cat("side panels\n")
check("each panel has a header with a fold button that says what it does, its key and that it is open",
      has('data-wb-fold="nav"', 'data-wb-fold="aside"', 'aria-label="Hide the tree"', 'aria-label="Hide the inspector"', 'title="Hide (Alt+[)"', 'title="Hide (Alt+])"') &&
      lengths(regmatches(h, gregexpr('data-wb-fold="[a-z]+"[^>]*aria-expanded="true"|aria-expanded="true"[^>]*data-wb-fold', h))) >= 1L && has('aria-controls="wb_nav"', 'aria-controls="wb_aside"'))
check("a folded panel leaves a strip with its name and a button to open it (aria-expanded false)",
      has('class="wb-strip wb-strip-nav"', 'class="wb-strip wb-strip-aside"', 'data-wb-unfold="nav"', 'data-wb-unfold="aside"', 'aria-label="Show the tree"', 'aria-label="Show the inspector"',
          '<span class="lab">Tree</span>', '<span class="lab">Inspector</span>') && grepl('data-wb-unfold="nav"[^>]*aria-expanded="false"|aria-expanded="false"[^>]*data-wb-unfold="nav"', h))
check("a resize handle on each panel's inner edge, as a labelled vertical separator",
      has('data-wb-grip="nav"', 'data-wb-grip="aside"', 'role="separator"', 'aria-orientation="vertical"', 'aria-label="Drag to resize the tree"', 'aria-label="Drag to resize the inspector"') &&
      pos('id="wb_nav"') < pos('data-wb-grip="nav"') && pos('data-wb-grip="nav"') < pos('class="wb-main"') && pos('class="wb-main"') < pos('data-wb-grip="aside"') && pos('data-wb-grip="aside"') < pos('id="wb_aside"'))
check("the inspector can float: a labelled button in its header", has('data-wb-float="1"', 'aria-label="Float the inspector over the page"') && pos('data-wb-float') > pos('id="wb_aside"'))
check("the app's tree and inspector content sit in the panels' scrolling part", grepl('<div class="wb-pc">\\s*<p>tree rows</p>', h) && grepl('<div class="wb-pc">\\s*<p>inspector rows</p>', h))
check("the script: keys, widths 180 to 480, state kept in this browser, plots refit after a change",
      all(vapply(c("BracketLeft", "BracketRight", "Math.max(180, Math.min(480", "localStorage", "ovid.grid.", "dispatchEvent(new Event('resize'))", "asideWide"), function(s) grepl(s, WB_JS, fixed = TRUE), NA)))
check("wide pages show the inspector's strip instead of hiding it", !grepl(".wb-body.wide .wb-aside{display:none}", WB_CSS, fixed = TRUE) && grepl(".wb-body.aside-off .wb-strip-aside{display:flex}", WB_CSS, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", RES$ok, length(RES$fail))); if (length(RES$fail)) quit(status = 1)
