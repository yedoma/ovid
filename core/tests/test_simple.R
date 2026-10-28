# core/tests/test_simple.R  --  The Simple view (simple.R, workbench.R ovid_view): which view opens, the page and its four
# steps, the Edit step's planned changes, the report.   Rscript core/tests/test_simple.R
suppressMessages({ library(data.table); library(shiny) })
CORE_DIR <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE); if (length(a)) dirname(dirname(normalizePath(sub("^--file=", "", a[1])))) else "core" })
for (.f in c("util", "audit", "theme", "config", "shell", "projects", "workbench", "report", "simple")) source(file.path(CORE_DIR, paste0(.f, ".R")))
APP_NAME <- "Ovid Core // XX"; APP_VERSION <- "0.0.1"; if (!exists("CORE_VERSION")) CORE_VERSION <- "0"
RES <- new.env(); RES$ok <- 0L; RES$fail <- character()
check <- function(what, cond) { cond <- tryCatch(cond, error = function(e) { cat("        error:", conditionMessage(e), "\n"); FALSE })
  if (isTRUE(cond)) { RES$ok <- RES$ok + 1L; cat("  ok   ", what, "\n") } else { RES$fail <- c(RES$fail, what); cat("  FAIL ", what, "\n") } }
pal <- list(bg = "#222222", bg1 = "#2a2a2a", bg2 = "#333333", bg3 = "#444444", fg = "#dddddd", fg4 = "#aaaaaa", gray = "#888888", accent = "#e5c07b",
            red = "#e06c75", green = "#98c379", blue = "#61afef", gold = "#d19a66", teal = "#56b6c2", purple = "#c678dd", bone = "#cccccc")

cat("which view opens\n")
env <- Sys.getenv("OVID_UI"); Sys.unsetenv("OVID_UI")
check("?ui= wins, then OVID_UI, then the setting; Standard by default and for anything else",
      ovid_view("?ui=simple") == "simple" && ovid_view("?ui=standard", "simple") == "standard" && ovid_view("", "simple") == "simple" &&
      ovid_view("") == "standard" && ovid_view(NULL, "nonsense") == "standard" && ovid_view("?ui=ops") == "standard" &&
      { Sys.setenv(OVID_UI = "simple"); x <- ovid_view("") == "simple" && ovid_view("?ui=workbench") == "workbench"; Sys.unsetenv("OVID_UI"); x })
check("wb_wanted and simple_wanted say the same", wb_wanted("?ui=workbench") && !wb_wanted("?ui=simple") && simple_wanted("?ui=simple") && !simple_wanted(""))
if (nzchar(env)) Sys.setenv(OVID_UI = env)
local({ d <- tempfile("first"); dir.create(d); f <- file.path(d, "app_settings.json")
  check("a new install (no settings file) opens in Simple, and finding that writes nothing", ovid_first_view(f) == "simple" && !file.exists(f))
  ovid_first_view_keep(f, "simple")
  check("the first session keeps Simple as the saved view", file.exists(f) && identical(settings_read(f)$ui, "simple"))
  check("an install with settings already stays on Standard", ovid_first_view(f) == "standard")
  g <- file.path(d, "other.json"); ovid_first_view_keep(g, "standard")
  check("keeping writes only for Simple", !file.exists(g)) })

cat("the page\n")
h <- as.character(htmltools::renderTags(simple_page("XX", pal, import = p("imp"), view = p("vw"), edit = simple_edit_ui("spe"), export = p("exp"),
                                                    hidden = list(tabPanel("A page", value = "A", textInput("a_in", "A")))))$html)
has <- function(...) all(vapply(c(...), function(s) grepl(s, h, fixed = TRUE), NA))
check("four numbered steps: Import, View, Edit, Export", has('data-step="import"', 'data-step="view"', 'data-step="edit"', 'data-step="export"') &&
      length(gregexpr('class="sp-step"', h, fixed = TRUE)[[1]]) == 4)
check("the view switch: Simple shown on, links to Standard and the Workbench that carry the context",
      has('class="d-views sp-views"', '<span class="on">Simple</span>', 'href="?ui=standard"', 'data-ui="standard"', 'href="?ui=workbench"', 'data-ui="workbench"'))
check("the Standard pages are there, hidden, so the Standard server's inputs exist", has('class="sp-hidden"', 'aria-hidden="true"', 'id="a_in"') &&
      grepl('id="nav"', h, fixed = TRUE))
check("the name is written all lowercase", grepl("<span>ovid</span>", h, fixed = TRUE) && grepl('class="app">XX<', h, fixed = TRUE) && !grepl(">Ovid<", h, fixed = TRUE))
check("the Edit step names its module for drag and drop", has('class="sp-edit" data-ns="spe-"'))
check("the styles and script are there: steps, tiles, drag and drop", grepl(".sp-tile", SIMPLE_CSS, fixed = TRUE) && grepl("'drop'", SIMPLE_JS, fixed = TRUE) &&
      grepl("sp-done", SIMPLE_JS, fixed = TRUE) && grepl("--sp-acc:", simple_css(pal), fixed = TRUE))
hs <- as.character(htmltools::renderTags(wb_page("X", pal, nav = p("n"), main = p("m")))$html)
check("the Workbench's switch offers Simple too", grepl('href="?ui=simple"', hs, fixed = TRUE))

cat("the Edit step's planned changes\n")
rows <- data.table(item = c("L1", "L2", "L3"), label = c("L1", "L2", "L3"), group = c("North", "North", ""), first = "2020-01-01", last = "2024-01-01",
                   lat = c(60.1, 60.1, NA), lon = c(-120.1, -120.1, NA))
op <- function(...) rbind(SIMPLE_OPS, data.table(...), fill = TRUE)
v <- simple_ops_preview(rows, op(type = "rename", group = "North", to = "Bog tower"))
check("a rename renames the group on every one of its items", all(v[item %in% c("L1", "L2")]$group == "Bog tower") && v[item == "L3"]$group == "")
v <- simple_ops_preview(rows, op(type = "move", group = "", to = "North", item = "L3", at = ""))
check("a move with no date moves the whole record", v[item == "L3"]$group == "North" && nrow(v) == 3)
v <- simple_ops_preview(rows, op(type = "move", group = "North", to = "South", item = "L2", at = "2022-06-01 10:00"))
check("a move from a date splits the record: before it stays, from it goes", nrow(v) == 4 && v[item == "L2" & group == "North"]$last == "2022-06-01 10:00" &&
      v[item == "L2" & group == "South"]$first == "2022-06-01 10:00")
v <- simple_ops_preview(rows, op(type = c("new", "place"), group = c(NA, "Fen"), to = c("Fen", NA), lat = c(NA, 61), lon = c(NA, -121)))
check("a new group appears, and a place is set on it", any(v$group == "Fen") && v[group == "Fen"]$lat == 61)
check("each change reads as a sentence", grepl("whole record", simple_ops_text(op(type = "move", group = "", to = "North", item = "L3", at = "")[1])) &&
      grepl("renamed", simple_ops_text(op(type = "rename", group = "a", to = "b")[1])))
check("coordinates: decimal degrees in range, a comma read as a point, anything else refused",
      simple_coord("60,5", 90) == 60.5 && is.na(simple_coord("91", 90)) && is.na(simple_coord("60 06 N", 90)) && simple_coord("-120.1", 180) == -120.1)

cat("the report\n")
if (requireNamespace("ggplot2", quietly = TRUE)) local({
  av <- data.table(name = c("North", "North", "South"), start = as.Date(c("2020-01-01", "2021-01-01", "2020-06-01")), end = as.Date(c("2020-08-01", "2022-01-01", "2022-01-01")))
  pl <- ggplot2::ggplot(data.table(x = 1:10, y = (1:10)^2), ggplot2::aes(x, y)) + ggplot2::geom_line()
  doc <- simple_report_doc("Field check: test", meta = c("2 stations", "2020 to 2022"), summary = "Both recording.",
    figures = c(Stations = "2", `Days with data` = "91 %"), notes = c(Stations = "with records"), plot = pl, plot_caption = "A line.",
    avail = av, table = data.table(Station = c("North", "South"), Days = c(500L, 580L)), table_caption = "Stations",
    methods = c("WMO (2017) WMO-No. 1203."), record = c(run = "R1", files = "3 found, 3 read"), pal = pal)
  f <- simple_report_write(doc, file.path(tempdir(), "simple_report_test"), "html", pal); x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  check("the HTML report: title, meta, figures with their notes, figure 1 and availability embedded, the table, methods, record",
        all(vapply(c("Field check: test", "2 stations", "Days with data", "with records", "Figure 1.", "Data availability", "<table>", "WMO-No. 1203", "R1", "Simple report"),
                   function(s) grepl(s, x, fixed = TRUE), NA)) && length(gregexpr("data:image/png;base64", x, fixed = TRUE)[[1]]) >= 2)
  check("the report writes the name all lowercase", grepl("ovid // XX", x, fixed = TRUE) && !grepl("Ovid Core", x, fixed = TRUE))
  check("the report's ink reads on paper (contrast 4.5:1 or more)", contrast_ratio(simple_ink(pal), "#fbfaf7") >= 4.5) })

cat(sprintf("\n%d passed, %d failed\n", RES$ok, length(RES$fail))); if (length(RES$fail)) { cat("Failed:\n", paste0("  ", RES$fail, collapse = "\n"), "\n", sep = ""); quit(status = 1) }
