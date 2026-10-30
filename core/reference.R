# Ovid/reference.R  --  The Reference page: the app's reference guide as a searchable index of topics, in both views.
# Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): the apps source it, no pipeline does.
#
# The topics come from the app's own guide (docs/<APP>_reference_guide.md), so the page and the guide never disagree:
# each numbered section and sub-section is a topic, each row of a table of formulas a calculated value of its own, and each
# glossary row a glossary entry. Topics are grouped Calculated values, Data handling, QA/QC, Sensors and Glossary. A topic
# shows its text (formulas and worked examples as the guide has them), where it happens in the pipeline, the reasonable
# values of the parameters it names (the guide's table), where it is set in the app, "Show on my data" for a value the run
# writes, and the guide as a PDF.
#
#   reference_tool(app)                          the page (a ovid_tool with value "Reference"), last in every menu
#   reference_server(input, output, session, app, go = list(regex = list(go, tab)))

REF_GROUPS <- c("Calculated values", "Data handling", "QA/QC", "Sensors", "Glossary")
REF_STEPS <- c("Data in", "Set-up", "Process", "Review", "Results", "Out")
REF_SKIP <- "^(Quick start|What it looks like|What happens|The pipeline at a glance|One (reading|image)|A typical session|Using the app|The viewer|Reasonable values|Glossary|References|In the raw file|What the outputs look like|What the plots look like)"

ref_group <- function(t) {
  if (grepl("Sensor|Common parameter|Electrode|Formats and roles|instrument", t, ignore.case = TRUE)) return("Sensors")
  if (grepl("Quality|Flag|check|QC\\b|QA\\b|Between-site|WMO|health|Tape|Drift|Positioning", t, ignore.case = TRUE)) return("QA/QC")
  if (grepl("Derived|Compensation|Depth to water|Daily|Annual|Thaw|Freeze|conductance|Statistics|Geometry|precipitation|Soil moisture|summar|monthly|Units to|pseudo-depth|n-factor", t, ignore.case = TRUE)) return("Calculated values")
  "Data handling" }
ref_step <- function(t) {
  p <- list(`Data in` = "Finding|Reading|Formats|Sources|instrument|scan", `Set-up` = "Clock|Time|Station|Site|Sensor|Calibration|Deployment|Metadata|Flights|Missions|Linking|arrays",
            Process = "Stitch|Compensation|Units|Barometric|Depth|Geometry|Text to numbers|Copies|Surveys|Datasets|GIS", Review = "Quality|Flag|check|Tape|Drift|Between|health|QA|QC|Positioning",
            Results = "Derived|Daily|Annual|Thaw|Freeze|Statistics|Soil|summar|conductance|GNSS", Out = "outputs|Export|Audit|WMO|sharing")
  for (k in names(p)) if (grepl(p[[k]], t, ignore.case = TRUE)) return(k)
  "" }

# The guide split into topics: data.table(id, group, title, md, step).
ref_topics <- function(md_path) {
  if (!file.exists(md_path)) return(data.table(id = character(), group = character(), title = character(), md = character(), step = character()))
  x <- readLines(md_path, warn = FALSE, encoding = "UTF-8"); h <- grep("^#{2,3} ", x)
  if (!length(h)) return(NULL)
  ends <- c(h[-1] - 1L, length(x)); out <- list(); sec2 <- ""
  for (i in seq_along(h)) { lv <- nchar(sub(" .*", "", x[h[i]])); tt <- trimws(sub("^#+ ", "", x[h[i]])); body <- x[seq_len(ends[i] - h[i]) + h[i]]
    if (lv == 2) sec2 <- tt
    tclean <- sub("^[0-9]+[a-z]?\\. ", "", tt)
    if (lv == 2 && grepl("^Glossary", tt)) {                      # each row of the glossary table is an entry
      r <- grep("^\\|\\s*\\*\\*", body, value = TRUE)
      for (l in r) { p <- trimws(strsplit(l, "\\|")[[1]]); p <- p[nzchar(p)]; if (length(p) >= 2)
        out[[length(out) + 1]] <- data.table(group = "Glossary", title = gsub("\\*", "", p[1]), md = paste(p[-1], collapse = " "), step = "") }
      next }
    if (grepl(REF_SKIP, tclean) || (lv == 3 && grepl(REF_SKIP, sec2))) next
    md <- paste(body, collapse = "\n"); if (!nzchar(trimws(md))) next
    g <- ref_group(paste(tclean, if (lv == 3) sec2)); st <- ref_step(paste(tclean, if (lv == 3) sec2))
    out[[length(out) + 1]] <- data.table(group = g, title = if (lv == 3) paste0(tclean, " (", sub("^[0-9]+[a-z]?\\. ", "", sec2), ")") else tclean, md = md, step = st)
    fr <- grep("^\\|.*Formula", body)                              # a table of formulas: each row a calculated value
    if (length(fr)) { rows <- body[(fr[1] + 2):length(body)]; rows <- rows[cumsum(!grepl("^\\|", rows)) == 0]
      hd <- trimws(strsplit(body[fr[1]], "\\|")[[1]]); hd <- hd[nzchar(hd)]
      for (l in rows) { p <- trimws(strsplit(l, "\\|")[[1]]); p <- p[nzchar(p)]; if (length(p) < 2) next
        nm <- gsub("`", "", p[1]); kv <- paste0("**", hd[seq_along(p)][-1], ":** ", p[-1], collapse = "\n\n")
        out[[length(out) + 1]] <- data.table(group = "Calculated values", title = nm, md = paste0(kv, "\n\nFrom the section *", tclean, "* of the guide."), step = "Results") } } }
  d <- rbindlist(out); d[, id := paste0("t", seq_len(.N))]; d[] }

# The guide's table of reasonable values: parameter, range, what to expect.
ref_reasonable <- function(md_path) {
  if (!file.exists(md_path)) return(NULL); x <- readLines(md_path, warn = FALSE, encoding = "UTF-8"); h <- grep("^## Reasonable values", x); if (!length(h)) return(NULL)
  r <- x[(h + 1):length(x)]; r <- r[seq_len(min(length(r), (grep("^## ", r)[1] %||% (length(r) + 1)) - 1))]; r <- grep("^\\|", r, value = TRUE)[-(1:2)]
  rows <- lapply(r, function(l) { p <- trimws(strsplit(l, "\\|")[[1]]); p <- p[nzchar(p)]; if (length(p) >= 2) p[seq_len(min(3, length(p)))] })
  rows <- Filter(Negate(is.null), rows); if (!length(rows)) return(NULL)
  data.table(parameter = vapply(rows, `[`, "", 1), range = vapply(rows, `[`, "", 2), expect = vapply(rows, function(p) if (length(p) > 2) p[3] else "", ""))
}

reference_tool <- function(app) ovid_tool("Reference", value = "Reference", icon = "book",
  div(class = "ref", tags$style(HTML(REF_CSS)), tags$script(HTML(REF_JS)),
    div(class = "ref-side", tags$input(type = "search", class = "form-control ref-q", placeholder = "Search the reference\u2026",
          oninput = "var q=this.value.toLowerCase();document.querySelectorAll('.ref-idx a').forEach(function(a){a.style.display=(a.getAttribute('data-s')||'').indexOf(q)>=0?'':'none'});document.querySelectorAll('.ref-idx .g').forEach(function(g){var n=0,e=g.nextElementSibling;while(e&&!e.classList.contains('g')){if(e.style.display!=='none')n++;e=e.nextElementSibling}g.style.display=n?'':'none'})"),
        uiOutput("ref_index", class = "ref-idx")),
    div(class = "ref-main", uiOutput("ref_topic"))))

REF_JS <- ""                                                   # the Standard view's handler for data-wb-open is in core shell.R
REF_CSS <- paste(readLines(file.path(CORE_DIR, "www", "reference.css"), encoding = "UTF-8"), collapse = "\n")

# go: list(regex = list(go = page, tab = tab)): where a topic is set in the app, by its title.
reference_server <- function(input, output, session, app, go = list(), docs = file.path(get0("APP_DIR", ifnotfound = getwd()), "docs")) {
  md <- file.path(docs, paste0(app, "_reference_guide.md")); pdf <- file.path(docs, paste0(app, "_reference_guide.pdf"))
  topics <- ref_topics(md); rv <- ref_reasonable(md); cur <- reactiveVal(if (!is.null(topics) && nrow(topics)) topics$id[1] else NULL)
  plain <- function(x) tolower(gsub("[`*|#<>]", " ", x))
  output$ref_index <- renderUI({ if (is.null(topics) || !nrow(topics)) return(p(class = "dim", "No reference guide was found for this app."))
    lapply(REF_GROUPS, function(g) { x <- topics[group == g]; if (!nrow(x)) return(NULL)
      tagList(div(class = "g", g), lapply(seq_len(nrow(x)), function(i) tags$a(href = "#", class = if (identical(cur(), x$id[i])) "on", `data-s` = plain(paste(x$title[i], x$md[i])),
        onclick = sprintf("Shiny.setInputValue('ref_pick','%s',{priority:'event'});return false", x$id[i]), x$title[i]))) }) })
  observeEvent(input$ref_pick, cur(input$ref_pick))
  output$ref_topic <- renderUI({ req(topics, cur()); t <- topics[id == cur()]; req(nrow(t) > 0)
    body <- tryCatch(HTML(commonmark::markdown_html(t$md, extensions = TRUE)), error = function(e) pre(t$md))
    words <- unique(tolower(unlist(regmatches(paste(t$title, t$md), gregexpr("[A-Za-z_][A-Za-z0-9_]{2,}", paste(t$title, t$md))))))
    rr <- if (!is.null(rv) && t$group != "Glossary") rv[vapply(strsplit(tolower(gsub("[`*]", "", parameter)), "[ /,]+"), function(p) any(p %in% words), NA)] else NULL
    g <- NULL; for (k in names(go)) if (grepl(k, t$title, ignore.case = TRUE)) { g <- go[[k]]; break }
    var <- if (t$group == "Calculated values" && grepl("^[a-z][a-z0-9_]+$", t$title)) t$title else NULL
    div(h2(t$title), div(class = "dim", t$group),
      if (nzchar(t$step)) div(class = "ref-steps", span(class = "dim", style = "border:0", "Where it happens:"), lapply(REF_STEPS, function(s) span(class = if (identical(s, t$step)) "on", s))),
      div(class = "ref-body", body),
      if (!is.null(rr) && nrow(rr)) tagList(h4("Reasonable values"), tags$table(class = "table d-num", tags$thead(tags$tr(tags$th("Parameter"), tags$th("Typical range"), tags$th("What to expect"))),
        tags$tbody(lapply(seq_len(min(nrow(rr), 8)), function(i) tags$tr(tags$td(gsub("[`*]", "", rr$parameter[i])), tags$td(rr$range[i]), tags$td(rr$expect[i])))))),
      div(class = "ref-acts",
        if (!is.null(g)) tags$a(class = "btn btn-default btn-sm", href = "#", `data-wb-open` = as.character(jsonlite::toJSON(g, auto_unbox = TRUE)), `data-ovid-go` = if (is.null(g$tab)) g$go,
                               title = "Settings and tables that change it", "Where it is set"),
        if (!is.null(var)) tags$a(class = "btn btn-default btn-sm", href = paste0("?ui=workbench&p=", utils::URLencode(var, reserved = TRUE)), title = "Open the Workbench with this value charted", "Show on my data"),
        if (file.exists(pdf)) tags$a(class = "btn btn-default btn-sm", href = paste0("docs/", basename(pdf)), download = NA, "The guide as PDF"),
        if (file.exists(sub("\\.md$", ".html", md))) tags$a(class = "btn btn-default btn-sm", href = paste0("docs/", sub("\\.md$", ".html", basename(md))), target = "_blank", "The whole guide"))) })
  invisible(topics)
}
