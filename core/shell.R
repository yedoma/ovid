# Ovid/shell.R  --  The shared app shell: one page structure and one interaction language for every Ovid app.
# Part of Ovid Core (shared). Sourced by each app's app.R after its pipeline (which sources theme.R); UI only.
#
# Consistent shell, flexible workspace, distinct module identity:
#   - a compact top bar: "ovid // <app>", the context bar (where you are and what you are looking at), the run
#     state, the main action, Help and Settings, always in the same places;
#   - a narrow navigation rail on the left that collapses to icons (remembered per browser);
#   - the workspace, which takes the rest of the screen. Each app names its own tools and sections.
# Secondary controls go in popovers (ovid_controls(more = ...)), big views get a Focus button (ovid_focus), two
# linked views can sit side by side (ovid_split, off by default), overview cards are for overview pages only, and
# QA/QC states look the same everywhere (ovid_status, ovid_flag, ovid_notice, ovid_review).
# Colours come from the app's own palette (theme.R), so each app keeps its scheme; only structure, sizes, spacing and
# behaviour are shared. Works with Bootstrap 3 (plain Shiny) and Bootstrap 5 (bslib themes) alike.

OVID_SHELL_VERSION <- "1.2.0"

# ---- Tokens: the CSS variables every Ovid page uses, filled from an app palette --------------------------------
# The palette needs bg and fg; anything else it lacks is mixed from those or taken from its hues. Pass `tokens` to
# ovid_page() to override one (WLR keeps Nord's aqua as its second accent, for example).
ovid_tokens <- function(pal) {
  bg <- pal$bg; fg <- pal$fg
  t <- list(bgh = .or(pal$bg_hard, pal$bgh, mix_col(bg, "#000000", 0.22)), bg = bg,
            bg1 = .or(pal$bg1, mix_col(bg, fg, 0.07)), bg2 = .or(pal$bg2, mix_col(bg, fg, 0.14)), bg3 = .or(pal$bg3, mix_col(bg, fg, 0.26)),
            gray = .or(pal$gray, pal$comment, mix_col(fg, bg, 0.45)), fg4 = .or(pal$fg4, mix_col(fg, bg, 0.2)), fg = fg,
            red = .or(pal$red, "#cc6666"), green = .or(pal$green, "#b5bd68"), yellow = .or(pal$yellow, pal$gold, "#f0c674"),
            blue = .or(pal$blue, "#81a2be"), purple = .or(pal$purple, "#b294bb"), aqua = .or(pal$aqua, pal$teal, "#8abeb7"),
            orange = .or(pal$orange, "#de935f"))
  t$acc <- .or(pal$accent, pal$acc, t$blue)
  t$acc2 <- .or(pal$acc2, mix_col(t$acc, fg, 0.25))
  t$onacc <- if (contrast_ratio(t$acc, t$bgh) >= contrast_ratio(t$acc, "#ffffff")) t$bgh else "#ffffff"
  t
}

# The QA/QC levels and their colours. Use these names everywhere a state is shown (tables, chips, banners, plots).
OVID_LEVELS <- c("ok", "warn", "error", "info", "off")
ovid_levels <- function(pal, tokens = ovid_tokens(pal))
  c(ok = tokens$green, warn = tokens$yellow, error = tokens$red, info = tokens$acc, off = tokens$gray)
.level_icon <- c(ok = "circle-check", warn = "triangle-exclamation", error = "circle-xmark", info = "circle-info", off = "circle-minus")
.level <- function(level) { level <- tolower(level[1]); if (level %in% OVID_LEVELS) level else "info" }

# ---- Page ---------------------------------------------------------------------------------------------------------
# One tool (page) of the workspace. icon is a Font Awesome name; rail = FALSE keeps it out of the rail (Settings,
# pages reached from a link). Pass the result, and section labels as plain strings, to ovid_page().
# n: the step's number in the set-up flow (shown before its name, with a status line under it that ovid_steps() fills).
ovid_tool <- function(title, ..., value = title, icon = NULL, rail = TRUE, n = NULL)
  structure(list(title = title, value = value, icon = icon, rail = rail, n = n, ui = tagList(...)), class = "ovid_tool")
# A page used as a step of the set-up flow under another name or number: the tool, and the first heading on its page, take
# them (n NULL keeps the page unnumbered).
ovid_retitle <- function(tool, n = NULL, title = NULL) {
  if (is.null(tool)) return(NULL); if (!is.null(n)) tool$n <- n; if (!is.null(title)) tool$title <- title
  if (!is.null(n) || !is.null(title)) tool$ui <- tryCatch(htmltools::tagQuery(tool$ui)$find("h2.d-title")$filter(function(x, i) i == 1)$
    empty()$append(if (!is.null(tool$n)) span(class = "d-step-n", tool$n), tool$title)$allTags(), error = function(e) tool$ui)
  tool }
# A stage of the set-up flow (1 Bring in, 2 Organize, 3 Correct, 4 Report, 5 Save): a numbered section label.
ovid_stage <- function(n, title) structure(list(n = n, title = title), class = "ovid_stage")

# The whole page. app is the short name ("WLR"); pal the app palette. status and actions sit at the right of the top
# bar (run state, main action); help is a URL (opens in a new tab); settings is the value of the tool the gear opens.
# theme: a bslib theme for Bootstrap 5 apps, NULL for plain Shiny. css: the app's own extra CSS, added after the shell's.
#
# The set-up flow (core 1.2): layout is how the steps are listed, each app its own: "rail" (a list on the left), "icons"
# (WLR: an icon rail, the name under each icon), "notebook" (GT: numbered contents on the left) or "tabs" (CS: the stages
# as a switch on the top bar, the pages of the stage shown listed on the left with their status lines, Go to (Cmd/Ctrl+K)
# for any page, station or setting; strip, e.g. the station strip, shows at the top of the pages named in strip_on, the
# first of which Go to opens on a station). project: the project switcher (projects_switch_ui)
# shown in the top bar with what else is open (context); aside: the information panel on the right (project figures,
# data gaps, the sources of the step). views: the Standard | Workbench switch; explain: the Explanations switch, which
# hides the longer help text (.lede, .d-explain) when off.
ovid_page <- function(app, pal, ..., status = NULL, actions = NULL, help = NULL, settings = NULL, tokens = list(),
                        css = NULL, head = NULL, theme = NULL, layout = c("rail", "icons", "notebook", "tabs"), project = NULL,
                        context = NULL, aside = NULL, strip = NULL, strip_on = NULL, views = TRUE, explain = TRUE, areas = NULL) {
  layout <- match.arg(layout)
  items <- list(...); items <- items[!vapply(items, is.null, NA)]
  tk <- utils::modifyList(ovid_tokens(pal), tokens)
  tools <- Filter(function(x) inherits(x, "ovid_tool"), items)
  titles <- stats::setNames(vapply(tools, `[[`, "", "title"), vapply(tools, `[[`, "", "value"))
  rail <- lapply(items, function(x) {
    if (is.character(x)) return(div(class = "d-rail-sec", x))
    if (inherits(x, "ovid_stage")) return(div(class = "d-rail-sec d-stage", span(class = "d-stage-n", x$n), span(class = "d-rail-label", x$title)))
    if (!inherits(x, "ovid_tool") || !isTRUE(x$rail)) return(NULL)
    tags$a(href = "#", class = paste("d-rail-link", if (!is.null(x$n)) "d-step"), `data-ovid-go` = x$value, title = x$title,
           if (!is.null(x$icon)) icon(x$icon), if (!is.null(x$n)) span(class = "d-step-n", x$n),
           span(class = "d-rail-label", x$title, if (!is.null(x$n)) span(class = "d-step-st", `data-step` = x$value))) })
  panels <- lapply(tools, function(x) tabPanel(x$title, value = x$value, div(class = "d-tool", x$ui)))
  page <- if (is.null(theme)) fluidPage else function(...) fluidPage(..., theme = theme)
  ic <- ovid_icon(app)
  if (!is.null(ovid_font_dir())) shiny::addResourcePath("ovid-fonts", ovid_font_dir())
  views_ui <- if (isTRUE(views)) div(class = "d-views", title = "Three views of the same project, data and edits: Simple, Standard, Workbench",
    tags$a(href = "?ui=simple", class = "d-to-view", `data-ui` = "simple", "Simple"),
    tags$a(class = "on", href = "?ui=standard", "Standard"), tags$a(href = "?ui=workbench", class = "d-to-wb", "Workbench"))
  explain_ui <- if (isTRUE(explain)) tags$label(class = "d-explain-sw ovid-sw", title = "Show or hide the longer explanations on each page",
    tags$input(type = "checkbox", role = "switch", id = "d_explain", checked = NA), span("Explanations"))
  if (layout == "tabs") {        # one bar: the stages as a switch on the top bar; the pages of the stage shown listed on the left with
                                 # their status lines, the pages after "More" folded at the bottom of the list (shell.js)
    grp <- list(list(head = NULL, links = list(), n = 0L))
    for (i in seq_along(items)) { x <- items[[i]]
      if (inherits(x, "ovid_stage") || identical(x, "More")) grp[[length(grp) + 1L]] <- list(head = x, links = list(), n = 0L)
      else if (!is.null(rail[[i]])) { k <- length(grp); grp[[k]]$links <- c(grp[[k]]$links, list(rail[[i]])); grp[[k]]$n <- grp[[k]]$n + inherits(x, "ovid_tool") } }
    more <- Find(function(g) identical(g$head, "More"), grp)
    stg <- Filter(function(g) g$n > 0 && !identical(g$head, "More"), grp)
    nm <- function(g, i) if (inherits(g$head, "ovid_stage")) g$head else list(n = i, title = if (is.character(g$head)) g$head else "Pages")
    stages_ui <- tags$nav(class = "d-stages", `aria-label` = "Stages", lapply(seq_along(stg), function(i) { h <- nm(stg[[i]], i)
      tags$button(type = "button", class = "d-stage-btn", `data-stage` = i, `aria-current` = "false",
        span(class = "d-stg-k", `data-n` = h$n, h$n), span(h$title), span(class = "d-stg-cnt")) }))
    rail <- tagList(lapply(seq_along(stg), function(i) { h <- nm(stg[[i]], i)
        div(class = "d-pg-grp", `data-stage` = i, `data-name` = h$title, hidden = if (i > 1) NA,
          div(class = "d-rail-sec d-pg-head", span(paste(h$n, "\u00b7", h$title)), span(sprintf("%d pages", stg[[i]]$n))), stg[[i]]$links) }),
      if (!is.null(more) && more$n > 0) tags$details(class = "d-pg-more", tags$summary(span("More pages"), span(more$n)), more$links))
    strip <- if (!is.null(strip)) div(class = "d-strip", hidden = NA, `data-pages` = paste(strip_on, collapse = "|"), strip)
    goto <- tags$button(type = "button", class = "d-goto", title = "Go to a page, station or setting", icon("magnifying-glass"),
        span(class = "d-goto-lab", "Go to page or station"), tags$kbd("\u2318K"))
    goto_pal <- div(class = "d-goto-pal", hidden = NA, div(class = "d-goto-box", role = "dialog", `aria-label` = "Go to",
        tags$input(class = "d-goto-in", placeholder = "Go to a page, station or setting\u2026", `aria-label` = "Go to",
                   role = "combobox", `aria-expanded` = "true", `aria-controls` = "d-goto-list"),
        div(class = "d-goto-list", id = "d-goto-list", role = "listbox"),
        div(class = "d-goto-foot", span("\u2191\u2193 choose"), span("\u21b5 open"), span("Esc close")))) }
  nav <- if (layout == "tabs") tags$nav(class = "d-rail d-pages", `aria-label` = "Pages", rail)
         else tags$nav(class = "d-rail", if (layout == "notebook") div(class = "d-nb-head", "Contents"),
                       if (layout == "notebook" && !is.null(views_ui)) div(class = "d-nb-views", views_ui), rail)
  trouble <- function() downloadLink("ovid_troubleshoot", icon("life-ring"), class = "d-icon-btn", title = "Troubleshooting zip: versions, logs, problems and the set-up, no raw data")
  if (layout == "tabs" && !is.null(settings))          # tabs: the Explanations switch and the troubleshooting zip leave the bar for Settings
    panels <- lapply(seq_along(tools), function(i) if (!identical(tools[[i]]$value, settings)) panels[[i]] else
      tabPanel(tools[[i]]$title, value = tools[[i]]$value, div(class = "d-tool", tools[[i]]$ui,
        div(class = "panel-x d-shell-set", h3("Display and support"), explain_ui,
          p(downloadLink("ovid_troubleshoot", tagList(icon("life-ring"), " Troubleshooting zip")), span(class = "dim", ": versions, logs, problems and the set-up, no raw data."))))))
  page(title = paste0("ovid // ", app),
    tags$head(if (!is.null(ic)) tags$link(rel = "icon", type = "image/png", href = ic),
              tags$style(HTML(paste0(ovid_font_css(), ovid_css(tk)))), if (exists("OVID_FILES_CSS")) tags$style(HTML(OVID_FILES_CSS)), if (!is.null(css)) tags$style(HTML(css)),
              tags$style(HTML(OVID_FLOW_CSS)), tags$style(HTML(OVID_PARTS_CSS)), if (exists("TSV_JS")) ts_view_head(), if (exists("DTBL_JS")) ovid_dt_head(),
              tags$script(HTML(OVID_SHELL_JS)), head),
    tags$script(HTML(sprintf("document.documentElement.classList.add('d-layout-%s'%s);", layout, if (!is.null(aside) && layout != "notebook") ",'d-has-aside'" else ""))),
    tags$script(type = "application/json", id = "d-titles", HTML(.d_json(titles))),
    div(class = "d-top",
      tags$button(type = "button", class = "d-icon-btn d-rail-toggle", title = "Show or hide the navigation labels", icon("bars")),
      div(class = "d-brand", if (!is.null(ic)) tags$img(class = "d-brand-icon", src = ic, alt = ""), span(class = "d-brand-d", "ovid"), span(class = "d-brand-sep", "//"), span(class = "d-brand-app", app)),
      ovid_areas_ui(areas),
      if (!is.null(project)) div(class = "d-project", project),
      if (layout == "tabs") stages_ui,
      div(class = "d-context", span(class = "d-crumb d-ctx-page"), uiOutput("d_context", inline = TRUE), context),
      div(class = "d-top-right", if (layout == "tabs") goto, if (layout == "tabs" && !is.null(status)) span(class = "d-run-st", status) else status, actions, if (layout != "tabs") explain_ui, if (layout != "notebook") views_ui,
        if (layout != "tabs" || is.null(settings)) trouble(),
        if (!is.null(help)) tags$a(class = "d-icon-btn", href = help, target = "_blank", title = "Help: the reference guide", icon("circle-question")),
        if (!is.null(settings)) tags$a(class = "d-icon-btn", href = "#", `data-ovid-go` = settings, title = "Settings", icon("gear")))),
    nav,
    div(class = "d-work", if (layout == "notebook" && !is.null(aside)) div(class = "d-nb-facts", aside),     # notebook: the facts are a card at the top, not a side panel
        if (layout == "tabs") strip,                                                                          # tabs: shell.js moves it onto the pages in strip_on
        do.call(tabsetPanel, c(list(id = "nav", type = "hidden"), panels))),
    if (!is.null(aside) && layout != "notebook") tags$aside(class = "d-aside", aside),
    if (layout == "tabs") goto_pal,
    if (exists("run_ui", mode = "function")) run_ui())                  # the progress box of a background run (core runner.R)
}

# Sides of one app (ERT | GPR): areas is a named list of the tool values on each side, e.g. list(ERT = c("Home", "Map"),
# GPR = c("GPR", "GPR lines")); a tool in no side (Reference, Settings) shows on every side. The switch sits beside the logo
# with the count on each side; only the side's own tools show; ?area=gpr, Alt-1 / Alt-2 and [data-area-go] buttons switch
# (www/areas.js). ovid_areas_server() sends the counts and keeps the side last used in the project's settings. NULL: nothing.
ovid_areas_ui <- function(areas, class = "d-views") if (length(areas)) tagList(
  tags$script(type = "application/json", id = "d-areas", HTML(jsonlite::toJSON(lapply(areas, as.character)))),
  div(class = paste(class, "d-areas"), role = "group", `aria-label` = "Side of the app", lapply(seq_along(areas), function(i) { a <- names(areas)[i]
    tags$a(href = "#", class = "d-area", `data-area` = a, title = sprintf("%s (Alt-%d)", a, i), a, span(class = "d-area-n")) })),
  tags$script(HTML(OVID_AREAS_JS)))
OVID_AREAS_JS <- paste(readLines(file.path(CORE_DIR, "www", "areas.js"), encoding = "UTF-8"), collapse = "\n")
ovid_area_start <- function(n, saved = NULL) { w <- names(n)[n > 0]                # the side last used, else the first with data, else the first
  if (length(saved) == 1L && saved %in% names(n)) saved else if (length(w)) w[1] else names(n)[1] }
# counts: a reactive named number per side (what the last scan found); project_dir: a function giving the project folder.
# A new project opens on the first side with data (the first side if none); the side the person picks is kept.
ovid_areas_server <- function(counts, project_dir, session = shiny::getDefaultReactiveDomain()) {
  # safe: no project open yet reads as none
  pd <- function() tryCatch(path.expand(project_dir() %||% ""), error = function(e) "")
  observe({ n <- counts(); p <- pd()
    session$sendCustomMessage("ovid-areas", list(counts = as.list(n), project = p, area = ovid_area_start(n, if (nzchar(p)) project_settings_read(p)$area))) })
  observeEvent(session$input$d_area, { p <- pd(); a <- session$input$d_area; if (!nzchar(p) || !dir.exists(p)) return()
    s <- project_settings_read(p); if (!identical(s$area, a)) { s$area <- a; project_settings_save(p, s) } }, ignoreInit = TRUE)
}

# Each app's icon (docs/icons/pixel/ovid_<app>.png at 64 px), shown beside "ovid // <app>" and as the browser tab
# icon. Kept here as data URIs so every copy of the core (portable folders, the zip) carries them with no extra files.
ovid_icon <- function(app) { b <- OVID_ICONS[[toupper(app)]]; if (is.null(b)) NULL else paste0("data:image/png;base64,", b) }
OVID_ICONS <- list(
  CS = "iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAQKADAAQAAAABAAAAQAAAAABGUUKwAAAKrklEQVR4Ae1bCWwU1xn+d2cP22tjbHxgwAfGGBzAmBCg1AVCEkDBLiSVKDnUqiBVCTRRlSpACWmBtg4VSBVtpIY0KkFRABGESBRMktIElZDSmMMmGIgBIxtz+D6wd73X7Pb/3npWa3vXHuPdNSZ50npmZ3be+7//ff/x/uch+o43TV/4V73w7EbJoC90Oh19/ey+vafT6Um2O468+/b+bYGE7KWA5365Ii/SYBjl5CfaW9o36SRpoeySAz1/X1+XtBI5Zfl4TFxMkY4l7bTbm/a9c7DMV2hc79Z0kq5Io5WW6lxuampopvraBtJyR8OxuXjikkYnLoyLi12o0WqIsR1lHAW+WLwMWP3Cyukura6opurmLGunLUnLDzQ3t5C5w0wajdb3mWFz7na7yBRtovj4OHLxhEZEGutTM8ad1rqcm3a/feA8gHgZ4CJtkl4nFVjMFmrkmZckiWw2KzmdMIbh24BHdrpIlmVKSIwXGB12118VRF4FMMs73W43WRm0xWJm2ntmXaPxkkR5ZlgdMYFOZwczwMXYoggYgVUBIYx71ZpnXu9ot/78ZvXtCS1NreRwOJj2wxu4AtD3qCENOexO0ukNqXPy89LLzpSfENPsdmuf5nuL7tyuZ9rbfZ95oM6BDRiBVWBmdEIBPNtWu93O1O9gW3E+mLPPjAY2YARWYMbsdnPvsI8HvfXE2E0BDzp4f/i+8wrwhkF/2gnHtZ6UhIcKZwAaUgUAqNGoF04X3geBV5ZdHIbDt/YYMgU47DJFmQy08Y/LaERspEhV9UaJvvy8gva8dYJMMcawRKMhUQBS0+yc0ZQzbQyljIujqCg9Z2icl+slypk6hpYsy6WyM9XUftfKGWloE7IhcYJYmPxwwURa/asFbAKSoDxo32m2UdakZFr7m8cpKXmEUEqo/VBYGWDnNDQ+3kSvvPYkJaXEkrndRlHRRvp3cTl9eOCccH5z5k2gn/5sDr20bjGd/fo6vffOV6TThW6ewqYAzHpWdrKg/viJSQKUzeak8vOV9E1pDd2oahR+IGtyMmHtPjY1jmpuxIrFSyhZEDYFOJniS36cS4sKpnKNwSYwWTsd9Ob2Y8LWIyL1ZGeFSJKGtCJt5SUs+woojpQQEQJNhI5bPsIi1sPG8XEyKHj/Lz67RNu3HuHVmcxs8FSc9OwEy87coK0bDlPdnTaalpdKW3b8hNIyE8RzPl0G7TTkCgD4yEgD5T6cSvEJJrJaHXTxm5tUXlZDFRfviLivJD6gfkuzhe/dpLKz1dTcZKbcGWlkMhlDZgohNwFZdnNdbgT97s9PkcGgp9rbrfTG6x8z3R2CCT2nEiaAlOgffztOc/KzaMPWQi9zwJBgt5ArQBFYy3XFTz46T1+frCQXZ3tKxUm53/OI+C9JWpEHPL96Ll0oS6VD+0qCnhyF3AQUYABUeaWeSkuqPI5NuRHgiIqUhfOCyoo6yuSoMTVvXEjygpArQDg/dnRoiOd6gzoag+7fXrxNr/36A7pWUcuscXO1yqlKeQF06veyahPovWrz21+3i1jYFDw1nfJmpnMhUiMcGfoZSF/oA5/U9FH08rpFdIyTpmtXar2Ro9uAPl/U1jRVKwAOTKfTiZDsM06fp5j9mT/IpJmzM6i1FRUoiR1flGoWoHM3gzebZdLrDbSkcBpdvlBLN2+0ch/+RYcLRSXY4VBXzvffC0b2aaipZ2VnUkZmOs+GuqUqEhhrp5NiRoyghjozHT1cTUZDEs1/LMGnZ3Wn5aUWam64RYsLU2nipAkkaRNJ0vlfJGE/o+p6NVVcvir2NvobQZUC4LGbGpuFZkVm1k+vqMGPThlJjy5+iDcjoqij3UIXL1wVTkyJ+f100e02mGS3JXBESOOqbh1dqbjFIdW/6HC2ba13+40yygD+e1Hudh1hT02NLbxj1NTjjv+vENgUnUYPz0nktNdODQ1tYlagvHtVgCnaU63u6Gij+ro7IoP0PzpnERxy1S6jVSkAA3k6VOfBrVbejXF4Evh3ubhx+lSlmJGuzaZAcge8zoQSTg95wao1C2jeY5Np+5YjbI73plDfgYIeBlHYeHRRDs1/YrIYB7EchY3BNCi/saGDDu4todYWC8WO9GxxDaZP5VnVDFAe6O+IELd0eR5XdlLYe9uF3aulY6C+MfP1tXdpz64TlDImllIzEigiQs95gcx5AdNjEC3oDIAssPWKy3dow9r9dLn8FocwdabTFw4o0cigUTZLTY+nbW+upFlzM3mXR11UCtR3CBgAJ0TstWX22K1CYLVJSSAhletQQtnZG/xVQ7PzMymSa4lqopLyvL9j0BWg0B1KAHWDBR7Co79PPjxPl87f4giTIfpWxvMHTs21oJkAQl/OtLH0xs4VNCE7CZMUkgYTMBh1gmVPP/MIrd9cIL4PJL32FSxoCoAAKFxM4KouVn0Xy24FdfYVocEoi8VG//0PJ1bsazKzEgc1TtAUAFuER8Za//1/fkX79pwSxU1F8GAdUTBprG+nHVuLqZT3DlBFwphDxgDEfczKi688Tiu5nI1zeP1QlrKVMVAtxa7Sxj8to/wF2eK/Pwaq6KAwAA4vZ8oYihtlEiVuJD8QMhzNwPUFFE8Tk2PuKSIMOgoI6ndRsJy9844/FPNujy6kDFAUi6iAirLD6ak4wxEjVxhIGxQDUOGdkjuWXv39UkrmnR78O+Fgw5Ja4REN/nXkAu3a+QXZWY4FT+TQy+sXi4iAAorads8KAMWxezMldxzNnT+RWriEjVp+uKgPRVderadTJ6/StW/rxPogf+EkEYLjePtNrVNUrQBsaKAmh90bG2scbf3mQlq2YoZY7Ozc9int3X0qLNRXZhc1AQsvtzevO0Sff3qJd5kNtKloOT25PFd1iqzKBwD83HlZNH1mGhdFZLHAgZfHqgwzDlv0+AI3V2vC4/wUJeCoUN6TeXqiku/9vs5VKQADTHpotNCsjdf6aFjtY78PSUlba6fYxwuT4xfj+/5BxcrcYeXS210aGR/le6vfc9UmIHoCwq4PZj46JoJKT1fTb186QLVs/6GM/X0hQWr82ccXaMurh0SlKFC5zF8fqhgAYOdK+D822q1ippWOQLnr1xpEAgKvPJQNpgkmHnjvf1RT1ax6MlQpAECxh196uqoXRtwbavAQClHBzib50cFz7JP43YCuHedeAve4oEoBeAYsGCqK95A54FdYJ5KwgbSB+YCB9DxMfvu9AsRE8dskOHa0m9nJDa7Gdj9PPLABo2hdmD0M0LgPa9x0LH18GkVERahOI+9nsD1lQ2oMbMAIrPznMH7jTdtWr332R5Kk//JMSWnXm2IPlnWgWMNvkNEjs2dw5uiYt/vv+09CAV6XyW+YRWLD0Wa1c1bVKdJbo9HAm5BDG98h5GAaaI83RZDNAhsSOWBV+vQqgLP5ev5xcXS0aVbKmGR+bY7TS7NZPByuFZ4iVLCOoD0mMU68NucixgaMp4FVGcNrAsqFX7z4XLHeoF+KZL/k1FmxqYkt5+HYsJWPLf3Zc2cKY3fYHUf37NrX7cVJLwMUgHbZsUmSNX/B97Tx4zZlZKYtHOz2k9J3uI9gscvtOu5yOYswNmPrtb3diwG+Qq5a8/xGnZ5fnubX6IZjY9l5xcovT7+1N+DL08MRV1Bl/j/l1T+IZKuHWgAAAABJRU5ErkJggg==",
  WLR = "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",
  GT = "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",
  GS = "iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAACG0lEQVR4nO2bv0sDMRTHv3k8KE72/xA6dHMpVOji5FAH/wFRxNlR/RtEFP8BB29wcilY6OLmUPD/qJN0UlLIEa73I2fvciXvPtBeei+XvJe8l4RLAwhH5QnH48tfBEAU3WXaqfKMns+nCIFeb5jZGAThqGTP272+WHwjBLrd3TVvMJ7AycyhGO1qE0E4yri/cf0QPSAtHHQoRNGdEu8BbFom9J7PspObUOLz5SxX3j9+9KYLQThc9oHr86c4fftwmilLk29KXt0u8jQYDaBd/OP9fu3+/sGFd10IwmHXjIPBUZyefr2t3bMpkmuubia5dWThWnflDTCbva6uw73DOJ1Ey+y8VZNXt4s8DYJwlFkK19Vr24oOoahdCqMNAfK9BC5aBvuGIByCcMhXRdr19RJYf4rCwGeYEIRD2zrI+RowuaqCspQ1911ecth5fYUBQThcx2usMr1eVJ4ZPDfRp5IG6Jcwpkh528XLlGsbXdV7Q4JwGFtC1d7iCkE4XGfhVfVYnfsEjIawB0r7t28IwiEIh5tWoCnXNxCEQxAOl30gawNyk/tVllV2Q5YgHIJwlP5qd4YEQxAOQThcR6H/ma7qmOJcIAiHIBwlfRrkphSo+z+FrhCEQxAOm8Ry+bO6djo7XipuyuWNnQaCcFTy0FTos4H5h6k5NEUQDufFiK/xwHfcO58cnUyeEQKj0Umcbk+OJlAQfnga0vkD8//3/ofewkgAAAAASUVORK5CYII=",
  RS = "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")

# A JSON object of strings without needing jsonlite (the shell has no package dependencies beyond shiny).
.d_json <- function(x) {
  esc <- function(s) gsub("([\"\\\\])", "\\\\\\1", s)
  paste0("{", paste0('"', esc(names(x)), '":"', esc(unname(x)), '"', collapse = ","), "}")
}

# Server side: move to a tool, and fill the context bar after the page name (pass the fields that say what is being
# looked at; empty ones are dropped). output$d_context <- renderUI(ovid_context(site, deployment))
ovid_go <- function(session, value) updateTabsetPanel(session, "nav", selected = value)
# The status line under each step of the flow: a named list, step value = list(level, text), level one of ok, warn,
# error, info, off ("off": not started), and an optional count shown on the step's stage in the tabs layout (what waits).
# ovid_steps(session, list(Project = list("ok", "Folders checked"), Needs = list("warn", "3 to look at", 3)))
ovid_steps <- function(session, x) session$sendCustomMessage("ovid-steps", lapply(x, function(v)
  list(level = .level(v[[1]]), text = as.character(v[[2]]), count = if (length(v) > 2) as.integer(v[[3]]) else 0L)))
ovid_context <- function(...) {
  x <- unlist(list(...)); x <- x[!is.na(x) & nzchar(x)]
  tagList(lapply(x, function(v) span(class = "d-crumb", v)))
}

# ---- Workspace pieces -------------------------------------------------------------------------------------------
# The head of a tool: title, an "About this page" popover for the long explanation, and actions on the right.
ovid_head <- function(title, about = NULL, ...)
  div(class = "d-head", h2(class = "d-title", title),
      if (!is.null(about)) ovid_pop(span(icon("circle-info"), "About"), div(class = "d-about", about), class = "d-about-pop"),
      div(class = "d-head-actions", ...))

# A popover: the label is a button; the content opens below it over the workspace and closes on a click elsewhere.
ovid_pop <- function(label, ..., align = "left", class = NULL, open = FALSE)
  tags$details(class = paste("d-pop", paste0("d-pop-", align), class), open = if (open) NA,
               tags$summary(class = "btn btn-default btn-sm", label), div(class = "d-pop-body", ...))

# The control bar of a tool: the main inputs in one row, secondary ones (plot options, filters, layers, export
# settings) in an Options popover so they do not take workspace.
ovid_controls <- function(..., more = NULL, more_label = "Options")
  div(class = "d-controls", ...,
      if (!is.null(more)) ovid_pop(span(icon("sliders"), more_label), more, align = "right", class = "d-more"))

# A toggle switch: one setting that takes effect at once and is kept (M 14 Oct; Plot_table_standards.md A12). A checkbox
# input underneath (input value TRUE/FALSE, as before) with role="switch" and its state written as On or Off. Choices a
# button acts on later stay checkboxes; a yes/no value in a set-up table stays a Yes/No pick.
ovid_switch <- function(inputId, label = NULL, value = FALSE, width = NULL) {
  x <- htmltools::tagQuery(checkboxInput(inputId, label, value, width))$find("input")$addAttrs(role = "switch")$allTags()
  htmltools::tagAppendAttributes(x, class = "ovid-switch") }

# A view that can fill the screen: a Focus button at its top right; Esc or the button again leaves.
ovid_focus <- function(..., class = NULL)
  div(class = paste("d-focusable", class),
      tags$button(type = "button", class = "d-icon-btn d-focus-btn", title = "Focus: fill the screen (Esc to leave)", icon("expand")), ...)

# Two linked views side by side. By default the second view sits below the first (single view); its toggle
# (ovid_split_toggle, put it in the tool's control bar) puts them side by side.
ovid_split <- function(id, main, side, side_title = NULL, width = "38%", on = FALSE)
  div(id = id, class = paste("d-split", if (on) "on"), style = paste0("--d-split-w:", width),
      div(class = "d-split-main", main), div(class = "d-split-side", if (!is.null(side_title)) h4(side_title), side))
ovid_split_toggle <- function(id, label = "Side by side", icon = "table-columns", on = FALSE)
  tags$button(type = "button", class = paste("btn btn-default btn-sm d-split-btn", if (on) "active"), `data-target` = id, icon(icon), label)

# ---- QA/QC presentation -----------------------------------------------------------------------------------------
# A state with its dot: "ok", "warn", "error", "info" or "off".
ovid_status <- function(text, level = "ok") span(class = paste("d-status", .level(level)), span(class = "d-dot"), text)
# A flag on a record, file or reading: a small tinted chip.
ovid_flag <- function(text, level = "warn", title = NULL) span(class = paste("d-flag", .level(level)), title = title, text)
# A banner for a warning or error that needs attention, with its icon.
ovid_notice <- function(level, title, ...) { l <- .level(level)
  div(class = paste("d-notice", l), icon(.level_icon[[l]]), div(tags$b(title), div(class = "d-notice-body", ...))) }
# The review actions for flagged data: keep it, or remove it. Extra buttons go after them.
ovid_review <- function(accept_id, reject_id, accept = "Keep", reject = "Remove", ...)
  div(class = "d-review", actionButton(accept_id, accept, icon = icon("check"), class = "btn-ok"),
      actionButton(reject_id, reject, icon = icon("xmark"), class = "btn-reject"), ...)
# Colour a DT status column by level: ovid_dt_levels(dt, "status", c(ok = "ok", missing = "error", ...), pal)
ovid_dt_levels <- function(dt, col, map, pal, default = "warn") { lc <- ovid_levels(pal)
  DT::formatStyle(dt, col, fontWeight = 600, color = DT::styleEqual(names(map), unname(lc[unname(map)]), default = lc[[default]])) }

# ---- CSS ----------------------------------------------------------------------------------------------------------
ovid_css <- function(tk) paste0(
  ":root{", paste0("--", names(tk), ":", unlist(tk), ";", collapse = ""),
  "--d-top:46px;--d-rail:204px;--d-rail-min:52px;--d-gap:16px;--d-r:6px;--d-ctl:32px}", OVID_SHELL_CSS)

OVID_SHELL_CSS <- paste(readLines(file.path(CORE_DIR, "www", "shell.css"), encoding = "UTF-8"), collapse = "\n")
OVID_PARTS_CSS <- paste(readLines(file.path(CORE_DIR, "www", "parts.css"), encoding = "UTF-8"), collapse = "\n")   # Standard and Workbench

# ---- The set-up flow: layouts, steps, the information panel -----------------------------------------------------------
# The information panel on the right (ovid_page(aside = ...)): sections of label and value rows.
ovid_aside_sec <- function(title, ..., n = NULL) div(class = "d-aside-sec", div(class = "d-aside-h", title, if (!is.null(n)) span(class = "n", n)), ...)
ovid_kv <- function(label, value, level = NULL, go = NULL)
  div(class = "d-kv", span(label), if (is.null(go)) tags$b(class = .level(level %||% "off"), value) else tags$a(href = "#", `data-ovid-go` = go, class = paste("d-kv-go", .level(level %||% "info")), value))
# A figure on a dashboard: a small label, the value large with its unit, a line under it; level colours the value.
ovid_kpi <- function(label, value, unit = NULL, sub = NULL, level = NULL)
  div(class = paste("d-kpi", if (!is.null(level)) .level(level)), div(class = "l", label), div(class = "v", value, if (!is.null(unit)) tags$small(unit)), if (!is.null(sub)) div(class = "s", sub))
# The page's one primary next step, at its foot: a button that opens the step value.
ovid_next <- function(label, go, note = NULL, id = NULL)
  div(class = "d-next", if (!is.null(note)) div(class = "d-next-note", note),
      if (is.null(id)) tags$a(href = "#", class = "btn btn-primary", `data-ovid-go` = go, label) else actionButton(id, label, class = "btn-primary"))
# The head of a step: its number and name, one sentence of what it is for (hidden with Explanations off), and actions.
ovid_step_head <- function(n, title, lede = NULL, ...)
  tagList(div(class = "d-head d-step-head", h2(class = "d-title", if (!is.null(n)) span(class = "d-step-n", n), title), div(class = "d-head-actions", ...)),
          if (!is.null(lede)) p(class = "lede", lede))

OVID_FLOW_CSS <- paste(readLines(file.path(CORE_DIR, "www", "flow.css"), encoding = "UTF-8"), collapse = "\n")

# ---- Guardrail: an error in a handler never ends the session ---------------------------------------------------------
# Shiny closes the whole session (the page greys out) when an observer stops with an error. Sourced into an app, these
# observe() and observeEvent() run each handler inside ovid_guard(): an error is shown as a notice with a short
# reference, written to the app's error log and the console (as Shiny writes it, with the first calls of its stack), and
# the app carries on. req() and validate() stop a handler quietly, as before. Outputs already show their own errors.
ovid_guard <- function(expr) {
  stack <- NULL
  tryCatch(withCallingHandlers(expr, error = function(e) stack <<- sys.calls()),
    # safe: shiny's silent errors (req()) are meant to show nothing
    shiny.silent.error = function(e) invisible(NULL),
    error = function(e) { if (inherits(e, "validation")) return(invisible(NULL)); ovid_guard_note(e, stack); invisible(NULL) })
}
ovid_guard_note <- function(e, stack = NULL) {
  # safe: the error's reference code is extra; the message itself is shown
  msg <- sub("\\s+$", "", conditionMessage(e)); ref <- tryCatch(error_ref(msg), error = function(x) "")
  cl <- if (length(stack)) rev(utils::tail(vapply(stack, function(x) { f <- x[[1]]; substr(if (is.name(f)) as.character(f) else if (is.call(f)) paste(deparse(f), collapse = "") else "<fn>", 1, 60) }, ""), 14)) else character()
  cl <- cl[cl != "<fn>"]
  cl <- cl[!grepl("^(withCallingHandlers|tryCatch|tryCatchList|tryCatchOne|doTryCatch|ovid_guard|h|\\.handleSimpleError|stop)$", cl)]
  message("Warning: Error in ", paste(deparse(conditionCall(e)), collapse = "")[1], ": ", msg, if (length(cl)) paste0(" / ", paste(utils::head(cl, 6), collapse = " / ")))
  f <- getOption("shiny.error"); if (is.function(f)) { try(stop(paste0("[", ref, "] ", msg), call. = FALSE), silent = TRUE); try(f(), silent = TRUE) }   # the app's error log
  if (!is.null(shiny::getDefaultReactiveDomain()))
    shiny::showNotification(tags$span(tags$b("Something went wrong"), if (nzchar(ref)) paste0(" (", ref, ")"), ": ", msg,
      tags$br(), tags$small("The app carries on. If it happens again, the Troubleshooting button (life ring, top right) collects the details.")),
      type = "error", duration = 15)
  invisible(NULL)
}
observeEvent <- function(eventExpr, handlerExpr, event.env = parent.frame(), event.quoted = FALSE, handler.env = parent.frame(), handler.quoted = FALSE, ...) {
  ev <- if (event.quoted) eventExpr else substitute(eventExpr); hx <- if (handler.quoted) handlerExpr else substitute(handlerExpr)
  shiny::observeEvent(ev, bquote(ovid_guard(.(hx))), event.env = event.env, event.quoted = TRUE, handler.env = handler.env, handler.quoted = TRUE, ...)
}
observe <- function(x, env = parent.frame(), quoted = FALSE, ...) {
  x <- if (quoted) x else substitute(x)
  shiny::observe(bquote(ovid_guard(.(x))), env = env, quoted = TRUE, ...)
}

# ---- The output folder, watched: deleted or replaced outside the app, the pages notice within seconds ----------------------
# outs <- reactive({ rv$tick; out_seen(); load_outputs(pd()) }) with out_seen <- ovid_output_watch(session, pd)
output_stamp <- function(project_dir) { o <- file.path(path.expand(project_dir %||% ""), "output"); if (!dir.exists(o)) return("none")
  d <- c(o, list.dirs(o, recursive = FALSE)); paste(basename(d), as.numeric(file.mtime(d)), collapse = "|") }
ovid_output_watch <- function(session, project_dir, every = 2500) {
  # safe: no project open yet reads as none
  pd <- function() tryCatch(project_dir(), error = function(e) "")
  shiny::reactivePoll(every, session, function() output_stamp(pd()), function() output_stamp(pd())) }

# ---- Behaviour ----------------------------------------------------------------------------------------------------
OVID_SHELL_JS <- paste(readLines(file.path(CORE_DIR, "www", "shell.js"), encoding = "UTF-8"), collapse = "\n")

# The troubleshooting zip behind the top bar's lifebuoy (core manifest.R troubleshooting_zip()). Each app's server calls it
# once with its project folder (a function) and its set-up check (a function returning a table, or NULL).
troubleshoot_server <- function(output, project_dir, check = NULL) {
  suggest_server(project_dir)                                         # what typed fields offer (below); every app calls this once
  output$ovid_troubleshoot <- downloadHandler(function() paste0("ovid_troubleshooting_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    function(file) { ref <- troubleshooting_zip(project_dir(), file, check)
      showNotification(paste("Troubleshooting zip", ref, "saved. Send it with a line on what went wrong."), type = "message", duration = 12) },
    contentType = "application/zip")
}

# updateSelectizeInput(server = TRUE) with no choices makes Shiny's browser code select the first of none and throw an error;
# with nothing to send, the choices go with the page instead. Use it wherever the choices can be empty.
update_selectize <- function(session, inputId, ..., choices = NULL, selected = NULL, server = FALSE) {
  if (isTRUE(server) && !is.null(choices) && !length(choices)) server <- FALSE
  shiny::updateSelectizeInput(session, inputId, ..., choices = choices, selected = selected, server = server)
}

# ---- small page helpers (each app had its own copy) ------------------------------------------------------------------
step <- function(n, title, ...) div(class = "step", div(class = "n", n), div(class = "body", h3(title), ...))
page_head <- function(title, lede) ovid_head(title, about = p(lede))
empty_msg <- function(txt = "Nothing to show yet. Press Run pipeline at the top right.") div(class = "empty", txt)
editUI <- function(id) ovid_edit_ui(id)
# When the set-up last changed, for "Set-up changed since the last rebuild": every config/*.csv except those the app says a
# rebuild does not read (cfg_mtime_ignore() in its app.R; a name with or without .csv).
cfg_mtime_ignore <- function(files) assign("CFG_MTIME_IGNORE", as.character(files), envir = environment(cfg_mtime))
cfg_mtime <- function(pd, ignore = get0("CFG_MTIME_IGNORE", ifnotfound = character())) {
  f <- list.files(file.path(path.expand(pd), "config"), "\\.csv$", full.names = TRUE)
  f <- f[!basename(f) %in% c(ignore, paste0(ignore, ".csv"))]; if (length(f)) max(as.numeric(file.mtime(f))) else 0 }
run_mtime <- function(pd) { f <- file.path(path.expand(pd), "output", "audit", "run_manifest.csv"); if (file.exists(f)) as.numeric(file.mtime(f)) else 0 }

# The set-up check before a run: its errors as lines ("<table>, <row>: <problem>") and the question asked when there are any. A run
# started anyway carries the lines (cfg$run_anyway) and its log (core manifest.R run_logged) records them.
setup_problem_lines <- function(e) { if (is.null(e) || !nrow(e)) return(character()); g <- function(k) if (k %in% names(e)) as.character(e[[k]]) else rep("", nrow(e))
  w <- trimws(paste(ifelse(nzchar(g("table")), g("table"), g("area")), ifelse(nzchar(g("row")), paste("row", g("row")), g("item")))); paste0(w, ": ", g("problem")) }
setup_gate <- function(e, review_id, force_id, verb = "Run") {
  l <- setup_problem_lines(e)
  modalDialog(title = paste0("The set-up needs attention: ", length(l), if (length(l) == 1) " thing" else " things", " would be ignored or misapplied"),
    tags$ul(lapply(utils::head(l, 8), tags$li)), if (length(l) > 8) p(class = "dim", "and ", length(l) - 8, " more."), easyClose = TRUE,
    footer = tagList(actionButton(review_id, "Review the set-up", class = "btn-primary"), actionButton(force_id, paste(verb, "anyway")), modalButton("Cancel")))
}

# What a typed field offers as you type: only what the app holds, never the browser's or the computer's own (Safari once
# offered names from the Mac's Contacts). The last values typed in the same field (initials, reasons, notes, folders; kept in
# the app's settings), and the names in the set-up tables and the deployment sheet in any field whose label names that kind
# ("Site", "Applies to (stations or loggers)", "Serial"). Sent again on each page, so renames and new projects show up.
SUGGEST_KEEP <- 15L
suggest_names <- function(project_dir, spec = get0("RENAMES", ifnotfound = list())) {
  out <- list(); add <- function(w, v) { v <- trim(as.character(v)); v <- v[!is.na(v) & nzchar(v)]; out[[w]] <<- unique(c(out[[w]], v)) }
  # safe: names offered for typing only; a table that cannot be read offers none
  for (k in names(spec)) add(tolower(spec[[k]]$label), tryCatch(rename_names(project_dir, k, spec), error = function(e) character()))
  # safe: as above, names offered for typing only
  d <- if (exists("dep_sheet", mode = "function")) tryCatch(dep_sheet(project_dir, report = FALSE), error = function(e) NULL)
  if (!is.null(d)) for (w in intersect(c("site", "station", "serial"), names(d))) add(w, d[[w]])
  out
}
suggest_server <- function(project_dir, settings_file = get0("SETTINGS_FILE"), session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(invisible())
  # safe: remembered typing is a convenience
  typed <- function() { t <- if (!is.null(settings_file)) tryCatch(settings_read(settings_file)$typed, error = function(e) NULL); if (is.list(t)) t else list() }
  # safe: no project open yet: nothing to send
  send_names <- function() { pd <- tryCatch(project_dir(), error = function(e) NULL); if (is.null(pd) || !nzchar(pd)) return()
    nm <- suggest_names(pd); for (w in names(nm)) session$sendCustomMessage("ovid-suggest", list(word = w, values = as.list(nm[[w]]))) }
  session$onFlushed(function() { t <- typed()
    for (id in names(t)) session$sendCustomMessage("ovid-suggest", list(id = id, values = as.list(t[[id]]))); send_names() }, once = TRUE)
  observeEvent(session$input$nav, send_names(), ignoreInit = TRUE)
  observeEvent(live_config(tryCatch(project_dir(), error = function(e) "")), send_names(), ignoreInit = TRUE)   # names renamed or added on any page
  observeEvent(session$input$ovid_typed, { x <- session$input$ovid_typed
    id <- as.character(x$id %||% ""); v <- trim(as.character(x$value %||% ""))
    if (length(id) != 1L || length(v) != 1L || !nzchar(id) || !nzchar(v) || nchar(v) > 200 || is.null(settings_file)) return()
    t <- typed(); old <- as.character(t[[id]] %||% character()); t[[id]] <- utils::head(c(v, old[tolower(old) != tolower(v)]), SUGGEST_KEEP)
    # safe: remembering typed names is a convenience
    tryCatch(settings_save(settings_file, list(typed = t)), error = function(e) NULL) })
  invisible()
}

# ---- Settings > Colours ------------------------------------------------------------------------------------------------
# The colour schemes an app offers (its APP_SCHEMES, the first its own) with their colours, and a preview of the one picked
# drawn in it. Picking one saves it (colour_scheme in app_settings.json); the app, its Workbench and its runs take it the
# next time the app starts (APP_SCHEME <- saved_scheme(APP_SCHEMES) in the app's palette file, core theme.R).
# Settings > Time zone, for an app whose own Settings have no zone field: the base zone every record is stored in
# (core util.R BASE_OFFSET). Saved with the app; on_change(z) lets the app pass it on to its next run.
time_zone_ui <- function(value = BASE_OFFSET) div(class = "panel-x", h3("Time zone"),
  numericInput("base_offset_h", "Time zone (UTC offset, hours): all records are stored in it. Applies at the next run.", value, step = 1, width = "220px"))
time_zone_server <- function(input, settings_file = get0("SETTINGS_FILE"), on_change = function(z) NULL)
  observeEvent(input$base_offset_h, { z <- input$base_offset_h; if (is.null(z) || is.na(z)) return()
    # safe: the zone applies in this session; only remembering it failed
    if (!is.null(settings_file)) try(settings_save(settings_file, list(base_offset_h = z)), silent = TRUE)
    BASE_OFFSET <<- z; on_change(z) }, ignoreInit = TRUE)

# Settings > Dataset: who made the data and how it may be used (core standards.R dataset_read/dataset_save), per project,
# written into every NetCDF export. pd: the app's project folder (a reactive).
dataset_card_ui <- function() div(class = "panel-x", h3("Dataset"),
  p(class = "dim", "Written into every NetCDF export (ACDD attributes), so the files say who made them and how they may be used. Blank fields are left out."),
  div(class = "d-dataset", style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:8px 14px;max-width:900px",
    textInput("ds_title", "Project title"), textInput("ds_creator", "Creator (person or group)"), textInput("ds_email", "Creator email"),
    textInput("ds_institution", "Institution"), textInput("ds_license", "Licence (e.g. CC-BY-4.0)")),
  textAreaInput("ds_summary", "Summary of the project (added after each file's own summary)", rows = 3, width = "100%"),
  actionButton("ds_save", "Save dataset card", class = "btn-sm"), span(class = "dim", style = "margin-left:8px", textOutput("ds_saved", inline = TRUE)))
dataset_card_server <- function(input, output, pd, session = shiny::getDefaultReactiveDomain()) {
  ids <- c(title = "ds_title", summary = "ds_summary", creator = "ds_creator", email = "ds_email", institution = "ds_institution", license = "ds_license")
  observe({ x <- dataset_read(pd()); for (k in names(ids)) updateTextInput(session, ids[[k]], value = x[[k]] %||% "") })
  observeEvent(input$ds_save, { if (isTRUE(get0("VIEWER"))) return()
    dataset_save(pd(), lapply(ids, function(i) input[[i]] %||% "")); output$ds_saved <- renderText(paste("Saved", format(Sys.time(), "%H:%M"))) })
}

colours_ui <- function(choices = get0("APP_SCHEMES"), pal_of = get0("app_palette")) {
  if (is.null(choices) || is.null(pal_of)) return(NULL)
  sw <- function(p) span(class = "d-sch-sw", lapply(unlist(p[c("bg", "accent", "red", "yellow", "green", "blue", "purple", "aqua")]), function(x) span(style = paste0("background:", x))))
  nm <- lapply(choices, function(n) span(class = "d-sch", span(class = "d-sch-nm", n, if (identical(n, choices[[1]])) span(class = "d-sch-tag", "default")), sw(pal_of(n))))
  tagList(
    div(class = "d-colours", fluidRow(
      column(5, radioButtons("colour_scheme", NULL, choiceNames = nm, choiceValues = choices, selected = saved_scheme(choices), width = "100%")),
      column(7, uiOutput("colour_preview"), p(class = "muted mt-2", textOutput("colour_note", inline = TRUE))))),
    start_view_ui())
}
# Which view opens when the app starts: the "ui" setting that the app reads at start (core workbench.R wb_wanted(); ?ui= in
# the address and OVID_UI still win). Shown with the colours, so every app's Settings has it.
START_VIEWS <- c("Simple view" = "simple", "Standard view" = "standard", "Workbench" = "workbench")
# safe: no saved view means the default one
saved_view <- function(settings_file = get0("SETTINGS_FILE")) { v <- if (!is.null(settings_file)) tryCatch(settings_read(settings_file)$ui, error = function(e) NULL)
  if (length(v) == 1 && v %in% START_VIEWS) v else "standard" }
start_view_ui <- function() div(class = "mt-3", tags$b("Opens in"),
  radioButtons("start_view", NULL, choices = START_VIEWS, selected = saved_view(), inline = TRUE),
  p(class = "muted", "The view the app opens in. Simple: look at the data and fix names and places. Standard: the processes to assess it. Workbench: change the data. Applies the next time the app starts."))
colours_server <- function(input, output, app, settings_file = get0("SETTINGS_FILE"), choices = get0("APP_SCHEMES"), pal_of = get0("app_palette")) {
  if (is.null(choices) || is.null(pal_of)) return(invisible())
  now <- get0("APP_SCHEME", ifnotfound = choices[[1]])                       # what this run of the app uses
  pick <- reactive(if (isTRUE(input$colour_scheme %in% choices)) input$colour_scheme else saved_scheme(choices))
  output$colour_preview <- renderUI(colour_preview(pal_of(pick()), app))
  output$colour_note <- renderText({ made <- attr(pal_of(pick()), "made")
    paste0(if (identical(pick(), now)) paste0(app, " is using ", now, ".") else paste0("Saved. ", app, " opens in ", pick(), " the next time it starts."),
           if (length(made)) paste0(" ", pick(), " has no ", paste(made, collapse = ", "), " of its own, so those are made up from its other colours.") else "") })
  observeEvent(input$colour_scheme, if (isTRUE(input$colour_scheme %in% choices) && !identical(input$colour_scheme, saved_scheme(choices)))
    # safe: the scheme applies in this session; only remembering it failed
    try(settings_save(settings_file, list(colour_scheme = input$colour_scheme)), silent = TRUE), ignoreInit = TRUE)
  if (!is.null(shiny::getDefaultReactiveDomain())) updateRadioButtons(shiny::getDefaultReactiveDomain(), "start_view", selected = saved_view(settings_file))   # the page was built at start
  observeEvent(input$start_view, if (!is.null(settings_file) && isTRUE(input$start_view %in% START_VIEWS) && !identical(input$start_view, saved_view(settings_file)))
    # safe: the view applies in this session; only remembering it failed
    try(settings_save(settings_file, list(ui = input$start_view)), silent = TRUE), ignoreInit = TRUE)
}
# A small screen drawn in palette p: the top bar, the status chips and a plot.
colour_preview <- function(p, app) {
  tk <- ovid_tokens(p); lv <- ovid_levels(p, tk); x <- seq(0, 1, length.out = 80)
  chip <- function(k, t) span(style = sprintf("color:%s;border:1px solid %s;border-radius:6px;padding:1px 8px;font-size:12px", lv[[k]], lv[[k]]), t)
  line <- function(col, f, ph, b) sprintf('<polyline fill="none" stroke="%s" stroke-width="1.6" points="%s"/>', col,
    paste(sprintf("%.1f,%.1f", 10 + x * 400, 8 + 114 * (1 - (b + 0.22 * sin(2 * pi * f * x + ph)))), collapse = " "))
  grid <- paste(sprintf('<line x1="10" x2="410" y1="%d" y2="%d" stroke="%s"/>', c(8, 65, 122), c(8, 65, 122), tk$bg2), collapse = "")
  svg <- HTML(sprintf('<svg viewBox="0 0 420 130" style="width:100%%;height:auto;display:block">%s%s%s%s%s</svg>', grid,
    line(tk$red, 1.2, 0, 0.62), line(tk$green, 1.6, 1.7, 0.49), line(tk$blue, 2, 3.4, 0.36), line(tk$purple, 0.8, 5, 0.3)))
  div(style = sprintf("background:%s;color:%s;border:1px solid %s;border-radius:8px;overflow:hidden", tk$bg, tk$fg, tk$bg2),
    div(style = sprintf("background:%s;padding:6px 10px;font-size:12px;display:flex;gap:8px;align-items:center;border-bottom:1px solid %s", tk$bgh, tk$bg2),
      span("ovid ", tags$b(style = paste0("color:", tk$acc), app)), span(style = paste0("color:", tk$fg4), "/ Check the data"), span(style = "flex:1"),
      span(style = sprintf("background:%s;color:%s;border-radius:4px;padding:1px 8px;font-weight:600", tk$acc, tk$onacc), "Run")),
    div(style = "padding:10px;display:grid;gap:8px",
      div(style = "display:flex;flex-wrap:wrap;gap:6px", chip("ok", "14 ok"), chip("warn", "3 to look at"), chip("error", "1 problem"), chip("info", "2 notes")),
      div(style = sprintf("background:%s;border:1px solid %s;border-radius:6px;padding:8px", tk$bg1, tk$bg2), svg)))
}

# ---- Switching views keeps the context ------------------------------------------------------------------------------------
# Each view tells the page what it shows (ovid_ctx(): the station or site, the parameters, the period and a selected
# stretch); the Standard | Workbench switch then opens the other view on the same: ?ui=...&station=...&params=a,b&from=&to=
# &t=&t1=. Edits saved in either view are the same set-up tables, so pending edits are shared; unapplied changes in a
# Workbench sheet are held as the table's unsaved edits (core editor.R), so they open in the other view still to Apply or Save.
ovid_ctx <- function(session, key, value, params = NULL, from = NULL, to = NULL, t = NULL, t1 = NULL) {
  f <- function(x) if (is.null(x) || !length(x) || all(is.na(x))) NULL else as.character(x[1])
  session$sendCustomMessage("ovid-ctx", list(key = key, value = f(value), params = if (length(params)) I(as.character(params)) else NULL,
                                               from = f(from), to = f(to), t = f(t), t1 = f(t1)))
}
OVID_CTX_JS <- paste(readLines(file.path(CORE_DIR, "www", "shell_menu.js"), encoding = "UTF-8"), collapse = "\n")
# An error shown under the field it is about (not in a message box); it goes when the field is changed.
ovid_field_error <- function(id, msg, session = shiny::getDefaultReactiveDomain()) { if (is.null(session)) return(invisible(FALSE))
  session$sendCustomMessage("ovid-field-err", list(id = session$ns(id), msg = msg)); invisible(FALSE) }
OVID_FIELD_JS <- paste(readLines(file.path(CORE_DIR, "www", "shell_field.js"), encoding = "UTF-8"), collapse = "\n")
OVID_YESNO_JS <- paste(readLines(file.path(CORE_DIR, "www", "yesno.js"), encoding = "UTF-8"), collapse = "\n")
OVID_SHELL_JS <- paste0(OVID_SHELL_JS, "\n", OVID_CTX_JS, "\n", OVID_FIELD_JS, "\n", OVID_YESNO_JS)

# ---- One click, one action ------------------------------------------------------------------------------------------
# Every app's downloads go through this downloadHandler (it stands in for shiny's, which apps call unqualified): when the
# file is made it tells the window (ovid-dl-done), whose download button shows "Preparing..." until then and ignores
# more clicks (shell_menu.js). A double click on Export therefore makes one file, however long it takes.
downloadHandler <- function(filename, content, contentType = NULL, outputArgs = list()) {
  shiny::downloadHandler(filename, function(file) {
    s <- shiny::getDefaultReactiveDomain()
    # safe: a closed page cannot be told the download is done
    on.exit(if (!is.null(s)) try(s$sendCustomMessage("ovid-dl-done", list()), silent = TRUE), add = TRUE)
    content(file) }, contentType, outputArgs)
}
# A long action that runs in the window (a button with class d-once) is turned off by the click; call this when it ends
# to turn the button back on (shell_menu.js).
once_done <- function(id, session = shiny::getDefaultReactiveDomain()) {
  # safe: a closed page cannot be told the action is done
  if (!is.null(session)) try(session$sendCustomMessage("ovid-once-done", list(id = id)), silent = TRUE)
  invisible()
}

# Who made a change, for the set-up history: the signed-in user (also of the session a module runs in), else the computer's.
current_user <- function(session) {
  # safe: no signed-in user: the computer's user is used below
  u <- tryCatch(session$user, error = function(e) NULL)
  # safe: as above
  if (is.null(u) || !nzchar(u)) u <- tryCatch(session$rootScope()$user, error = function(e) NULL)
  if (is.null(u) || !nzchar(u)) u <- Sys.info()[["user"]]
  u
}

# Started from the app's launcher (LOCAL): closing the last browser tab, in either interface, stops the app. app: its short
# name, for the console line. OPEN_SESSIONS is the app's counter (an environment with n).
track_session <- function(session, app) {
  if (!LOCAL) return(invisible())
  OPEN_SESSIONS$n <- OPEN_SESSIONS$n + 1L
  session$onSessionEnded(function() {
    OPEN_SESSIONS$n <- OPEN_SESSIONS$n - 1L
    later::later(function() {
      if (OPEN_SESSIONS$n > 0L) return(invisible())
      cat("Browser closed; stopping Ovid ", app, ".\n", sep = "")
      stopApp()
    }, 8)
  })
}
