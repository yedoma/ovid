# core/tests/test_areas.R  --  The sides of one app (shell.R ovid_areas_ui / ovid_areas_server, workbench.R wb_page):
# the switch markup, a page without areas left as it was, the side a project opens on and the side kept in its settings.
#   Rscript core/tests/test_areas.R      (exit status 1 if anything fails)
suppressMessages({ library(data.table); library(shiny) })
CORE_DIR <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else "core" })
for (.f in c("util", "theme", "shell", "projects", "workbench")) source(file.path(CORE_DIR, paste0(.f, ".R")))
RES <- new.env(); RES$ok <- 0L; RES$fail <- character()
check <- function(what, cond) { if (isTRUE(cond)) { RES$ok <- RES$ok + 1L; cat("  ok   ", what, "\n") } else { RES$fail <- c(RES$fail, what); cat("  FAIL ", what, "\n") } }
html <- function(x) as.character(htmltools::renderTags(x)$html)
pal <- list(bg = "#222222", bg1 = "#2a2a2a", bg2 = "#333333", bg3 = "#444444", fg = "#dddddd", fg4 = "#aaaaaa", gray = "#888888", accent = "#e5c07b",
            red = "#e06c75", green = "#98c379", blue = "#61afef", gold = "#d19a66", teal = "#56b6c2", purple = "#c678dd", bone = "#cccccc")
pg <- function(...) html(ovid_page("X", pal, "Data", ovid_tool("One", p("1")), ovid_tool("Two", p("2")), "Help", ovid_tool("Reference", p("r")), ...))
AR <- list(ERT = c("One"), GPR = c("Two"))

cat("the switch\n")
check("no areas: no switch, no areas script", is.null(ovid_areas_ui(NULL)) && !grepl("d-areas|data-area", pg()))
check("no areas: the page is the same as before (areas = NULL)", identical(gsub("tab(setid=\"|-)[0-9]+", "", pg()), gsub("tab(setid=\"|-)[0-9]+", "", pg(areas = NULL))))
h <- pg(areas = AR)
check("areas: a switch beside the logo with one button per side, Alt-1 and Alt-2", grepl('class="d-views d-areas"', h, fixed = TRUE) &&
      grepl('data-area="ERT" title="ERT (Alt-1)"', h, fixed = TRUE) && grepl('data-area="GPR" title="GPR (Alt-2)"', h, fixed = TRUE) && regexpr("d-brand", h) < regexpr("d-areas", h))
js <- regmatches(h, regexpr('<script type="application/json" id="d-areas">[^<]*</script>', h))
check("areas: the sides' pages as JSON for the script", identical(jsonlite::fromJSON(sub("<script[^>]*>(.*)</script>", "\\1", js), simplifyVector = FALSE), list(ERT = list("One"), GPR = list("Two"))))
w <- html(wb_page("X", pal, areas = AR))
both <- paste(h, w, as.character(htmltools::renderTags(ovid_page("X", pal, "Data", ovid_tool("One", p("1"))))$head),
              as.character(htmltools::renderTags(wb_page("X", pal))$head))
check("areas: the script comes with the switch; its rules (www/parts.css) with every Standard and Workbench page",
      grepl("ovid-areas", h, fixed = TRUE) && lengths(regmatches(both, gregexpr(".d-area-off{display:none!important}", both, fixed = TRUE))) == 2L)
check("Workbench: the same switch in its top bar, in its own style", grepl('class="wb-switch d-areas"', w, fixed = TRUE) && grepl('data-area="GPR"', w, fixed = TRUE))
check("Workbench without areas: no switch", !grepl("d-areas", html(wb_page("X", pal)), fixed = TRUE))

cat("the side a project opens on\n")
check("the side last used in the project", ovid_area_start(c(ERT = 5, GPR = 2), "GPR") == "GPR")
check("a new project: the side with data, the first if both", ovid_area_start(c(ERT = 0, GPR = 2)) == "GPR" && ovid_area_start(c(ERT = 3, GPR = 2)) == "ERT")
check("nothing anywhere: the first side; an unknown saved side is ignored", ovid_area_start(c(ERT = 0, GPR = 0)) == "ERT" && ovid_area_start(c(ERT = 0, GPR = 1), "RS") == "GPR")
pd <- file.path(tempdir(), "areas_proj"); unlink(pd, TRUE); dir.create(pd)
s <- MockShinySession$new()
withReactiveDomain(s, ovid_areas_server(reactive(c(ERT = 1, GPR = 1)), function() pd)); s$flushReact()
s$setInputs(d_area = "GPR"); s$flushReact()
check("the side the person picks is kept in the project's settings", identical(project_settings_read(pd)$area, "GPR"))

cat(sprintf("\n%d passed, %d failed\n", RES$ok, length(RES$fail))); if (length(RES$fail)) quit(status = 1)
