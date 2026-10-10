# Ovid/applied.R  --  Corrections applied: what the last run did to the data, as one short list.
# Part of Ovid Core (shared). Sourced by the processors' pipelines (CS, WLR, GT) and so by their apps.
#
# applied_write() runs once at the end of a run, after audit_index.csv and flags.csv are written, and writes
# output/audit/corrections_applied.csv: a row per correction in effect and the values it removed, changed, filled or
# flagged in that run, in three groups, then the totals:
#   On by default              the app's CORRECTIONS that are on (core corrections.R)
#   Corrections over a period  the rows of config/period_corrections.csv that are on (core periods.R)
#   Quality rules and checks   the rules and checks that removed (audit_index.csv, stage "qc") or flagged (flags.csv) values
# Corrections that are off are not listed. Counts are the audit trail's and flags.csv's own numbers; a blank count is a
# correction whose values the audit trail does not attribute to it alone. A CORRECTIONS entry may name the audit rules and
# flag checks it writes (rules = c(...)), which gives its count; one that changes or fills values without an audit line is
# counted by the pipeline (applied_write(counts = list(<id> = n))). A period row counts the audit lines and flags whose reason
# names it ("row N of period_corrections.csv"), whatever their rule; none is a count of 0. An entry counted in something other
# than values (unit = "sensors") says so in its effect ("sensors filled") and is left out of the totals. The list is shown in the top bar (applied_ui() and
# applied_server(): a chip with the totals; a click opens the list), copied into exports (applied_export()) and added to
# reports (applied_report()).

APPLIED_FILE <- "corrections_applied.csv"
APPLIED_GROUPS <- c("On by default", "Corrections over a period", "Quality rules and checks")
APPLIED_DOES <- c(removes = "removed", changes = "changed", fills = "filled", flags = "flagged")
APPLIED_PERIOD <- c(remove = "removed", offset = "changed", scale = "changed", drift = "changed", interpolate = "filled", flag = "flagged")

.applied_row <- function(group, item, does, n, detail = "") data.table(group = group, item = as.character(item), does = as.character(does), n = as.numeric(n), detail = as.character(detail))
.applied_read <- function(f) if (file.exists(f)) tryCatch(fread(f, colClasses = "character", na.strings = ""), error = function(e) data.table()) else data.table()
.applied_label <- function(x) { x <- gsub("_", " ", as.character(x)); paste0(toupper(substring(x, 1, 1)), substring(x, 2)) }
.applied_names <- function(d) { v <- intersect(c("variable", "column", "parameter"), names(d)); if (!length(v)) return("")
  u <- unique(d[[v[1]]]); u <- u[!is.na(u) & nzchar(u)]; if (length(u) > 4) paste0(paste(u[1:4], collapse = ", "), " +", length(u) - 4) else paste(u, collapse = ", ") }
.applied_date <- function(x) sub(" 00:00(:00)?$| 23:59(:59)?$", "", trim(as.character(x)))

# The values each period-correction row changed, by row and rule, from the run's audit lines (output/audit/detail/), whose
# reasons name the row ("row N of period_corrections.csv"); read from the files so a run made in parallel workers counts too.
applied_period_rows <- function(adir) {
  f <- list.files(file.path(adir, "detail"), pattern = "\\.csv$", full.names = TRUE)
  d <- rbindlist(lapply(f, function(x) tryCatch(fread(x, select = c("rule", "reason", "n"), colClasses = "character"), error = function(e) NULL)), fill = TRUE)
  if (!nrow(d)) return(data.table(row = integer(), rule = character(), n = numeric()))
  d <- d[grepl(paste0("row [0-9]+ of ", PERIOD_FILE), reason)]
  d[, .(row = suppressWarnings(as.integer(sub(".*row ([0-9]+) of .*", "\\1", reason))), rule, n = as.numeric(n))][!is.na(row), .(n = sum(n)), by = .(row, rule)] }

applied_write <- function(cfg, corr = get0("CORRECTIONS", ifnotfound = list()), values = NA, counts = list()) {
  pd <- path.expand(cfg$project_dir %||% ""); adir <- file.path(pd, "output", "audit")
  idx <- .applied_read(file.path(adir, "audit_index.csv")); fl <- .applied_read(file.path(adir, "flags.csv"))
  idx <- ensure_cols(idx, c("stage", "rule", "n")); idx[, n := suppressWarnings(as.numeric(n))]
  fl <- ensure_cols(fl, c("check", "detail")); fl[, k := if ("records" %in% names(fl)) suppressWarnings(as.numeric(records)) else rep(1, .N)][is.na(k), k := 1]
  out <- list(); used <- character()
  for (x in corr) { if (nzchar(x$table %||% "")) next                  # a table's rows are listed by the table's own group
    st <- correction_state(x, cfg, pd); if (st %in% c("off", "by table, none")) next
    r <- x$rules %||% character(); used <- c(used, r)
    n <- if (length(r) || !is.null(counts[[x$id]])) sum(idx[rule %in% r, n], fl[check %in% r, k], counts[[x$id]], na.rm = TRUE) else NA
    out[[length(out) + 1L]] <- .applied_row(APPLIED_GROUPS[1], x$label %||% sub("\\.$", "", x$what), paste(c(if (!identical(x$unit %||% "values", "values")) x$unit, APPLIED_DOES[[x$does]]), collapse = " "), n, if (st %in% c("on", "always")) "" else paste("set to", st)) }
  pf <- file.path(pd, "config", PERIOD_FILE); pr <- tryCatch(period_read(pf)$rows, error = function(e) NULL); pn <- applied_period_rows(adir)
  fn <- fl[grepl(paste0("row [0-9]+ of ", PERIOD_FILE), detail)][, row := suppressWarnings(as.integer(sub(".*row ([0-9]+) of .*", "\\1", detail)))]
  if (!is.null(pr) && nrow(pr)) { raw <- ensure_cols(as.data.table(read_cfg(pf)), PERIOD_COLS)
    for (i in seq_len(nrow(pr))) { r <- pr[i]; w <- raw[r$row]
      n <- if (r$action == "flag") fn[row == r$row, sum(k)] else pn[row == r$row, sum(n)]      # a row the run wrote nothing for: 0
      what <- paste0(PERIOD_ACTIONS[[r$action]] %||% r$action, if (!is.na(r$value) && r$action %in% c("offset", "scale", "drift")) paste0(" ", r$value), ", ",
                     if (nzchar(w$from)) .applied_date(w$from) else "start", " to ", if (nzchar(w$to)) .applied_date(w$to) else "end")
      where <- paste(c(if (nzchar(r$target)) r$target else "all", if (nzchar(r$parameter)) r$parameter), collapse = " ")
      out[[length(out) + 1L]] <- .applied_row(APPLIED_GROUPS[2], paste0(where, ": ", what), APPLIED_PERIOD[[r$action]] %||% "changed", n,
                                              paste0("row ", r$row, if (nzchar(r$note)) paste0(": ", r$note))) } }
  q <- idx[stage == "qc" & !rule %in% c(used, pn$rule)]                # rules a period row wrote are listed with the row
  for (r in unique(q$rule)) out[[length(out) + 1L]] <- .applied_row(APPLIED_GROUPS[3], .applied_label(r), "removed", sum(q[rule == r, n], na.rm = TRUE), .applied_names(q[rule == r]))
  f <- fl[!check %in% c(used, "period_flag", fn$check) & !is.na(check)]
  for (r in unique(f$check)) out[[length(out) + 1L]] <- .applied_row(APPLIED_GROUPS[3], .applied_label(r), "flagged", sum(f[check == r, k]), .applied_names(f[check == r]))
  a <- if (length(out)) rbindlist(out) else .applied_row(character(), character(), character(), numeric(), character())
  s <- function(w) sum(a[does %in% w, n], na.rm = TRUE)      # totals: the listed corrections only (not copies of a reading kept once)
  tot <- .applied_row("Totals", c("corrections", "removed", "changed", "flagged", "values"), "", c(nrow(a), s("removed"), s(c("changed", "filled")), s("flagged"), values))
  a <- rbind(a, tot)[, run_id := as.character(AUDIT$run_id %||% "")]
  fwrite(a, file.path(adir, APPLIED_FILE)); invisible(a)
}

applied_read <- function(project_dir) { a <- .applied_read(file.path(path.expand(project_dir %||% ""), "output", "audit", APPLIED_FILE))
  if (nrow(a)) a[, n := suppressWarnings(as.numeric(n))]; a }
applied_total <- function(a, what) { v <- a[group == "Totals" & item == what, n]; if (length(v)) v[1] else NA }
.applied_num <- function(x) ifelse(is.na(x), "", format(x, big.mark = ",", trim = TRUE, scientific = FALSE))

# ---- exports and reports -----------------------------------------------------------------------------------------------------
applied_export <- function(project_dir, dir) { f <- file.path(path.expand(project_dir %||% ""), "output", "audit", APPLIED_FILE)
  if (file.exists(f)) file.copy(f, file.path(dir, APPLIED_FILE), overwrite = TRUE) else FALSE }
APPLIED_README <- paste0(APPLIED_FILE, "  every correction applied in the run, with the values each removed, changed, filled or flagged (off corrections are not listed).")
applied_report <- function(project_dir, scope = "") { a <- applied_read(project_dir); if (!nrow(a)) return(NULL)      # scope: a note on what the counts cover
  list(rpt_heading("Corrections applied"),
       rpt_text(sprintf("%s corrections applied; %s values removed, %s changed, %s flagged.%s", .applied_num(applied_total(a, "corrections")),
                        .applied_num(applied_total(a, "removed")), .applied_num(applied_total(a, "changed")), .applied_num(applied_total(a, "flagged")), if (nzchar(scope)) paste0(" ", scope) else "")),
       rpt_table(a[group != "Totals", .(group, correction = item, values = .applied_num(n), effect = does, detail)],
                 "Corrections that are off are not listed. A blank count: the audit trail does not attribute the values to this correction alone.")) }

# ---- the chip in the top bar -----------------------------------------------------------------------------------------------
APPLIED_CSS <- ".d-applied>summary{border-radius:999px;font-size:12.5px;padding:3px 10px;font-variant-numeric:tabular-nums;background:transparent;border:1px solid var(--bg3);color:var(--fg)}
.d-applied .d-applied-dot{width:7px;height:7px;border-radius:50%;background:var(--acc);display:inline-block}
.d-applied .sep{color:var(--gray)} .d-applied .fl{color:var(--yellow)}
.d-applied>.d-pop-body{width:min(380px,90vw);padding:0;font-size:13px}
.d-applied-head{display:flex;gap:8px;align-items:baseline;padding:9px 12px 7px;border-bottom:1px solid var(--bg2)} .d-applied-head b{font-weight:600}
.d-applied-head span,.d-applied-line{color:var(--fg4);font-size:12px} .d-applied-head span{margin-left:auto}
.d-applied-line{padding:6px 12px;border-bottom:1px solid var(--bg2);font-variant-numeric:tabular-nums} .d-applied-line b{color:var(--fg)}
.d-applied details{border-bottom:1px solid var(--bg2)} .d-applied details>summary{display:flex;gap:8px;padding:6px 12px;cursor:pointer}
.d-applied details>summary .n,.d-applied li .n{margin-left:auto;color:var(--fg4);white-space:nowrap;font-variant-numeric:tabular-nums}
.d-applied ul{list-style:none;margin:0;padding:0 12px 6px 26px;max-height:240px;overflow:auto} .d-applied li{display:flex;gap:8px;padding:2px 0;font-size:12.5px}
.d-applied li small{display:block;color:var(--fg4)} .d-applied li .n.fl{color:var(--yellow)}
.d-applied-foot{padding:7px 12px;font-size:12px} .d-applied-foot a{color:var(--acc)}"

applied_ui <- function(id) tagList(tags$style(HTML(APPLIED_CSS)), uiOutput(NS(id, "chip"), inline = TRUE))

# project_dir: a function giving the project folder; seen: a reactive that changes when the output folder does (core shell.R
# ovid_output_watch()); edit: the tool the "Edit corrections" link opens.
applied_server <- function(id, project_dir, seen = NULL, edit = NULL) moduleServer(id, function(input, output, session) {
  a <- reactive({ if (is.function(seen)) seen(); tryCatch(applied_read(project_dir()), error = function(e) data.table()) })
  output$chip <- renderUI({ a <- a(); if (!nrow(a)) return(NULL); tot <- function(w) applied_total(a, w); num <- function(w) .applied_num(tot(w))
    li <- a[group != "Totals"]; when <- sub("^(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2}).*", "\\1-\\2-\\3 \\4:\\5", a$run_id[1] %|% "")
    grp <- function(g) { d <- li[group == g]; if (!nrow(d)) return(NULL)
      tags$details(open = if (g == APPLIED_GROUPS[2]) NA, tags$summary(g, span(class = "n", nrow(d))),
        tags$ul(lapply(seq_len(nrow(d)), function(i) tags$li(span(d$item[i], if (nzchar(d$detail[i] %|% "")) tags$small(d$detail[i])),
          span(class = paste("n", if (d$does[i] == "flagged") "fl"), if (is.na(d$n[i])) d$does[i] else paste(.applied_num(d$n[i]), d$does[i])))))) }
    tags$details(class = "d-pop d-pop-right d-applied",
      tags$summary(title = "Corrections applied in the last run", span(class = "d-applied-dot"), paste(num("corrections"), "corrections"), span(class = "sep", "·"),
                   paste(num("removed"), "removed"), span(class = "sep", "·"), span(class = "fl", paste(num("flagged"), "flagged"))),
      div(class = "d-pop-body",
        div(class = "d-applied-head", tags$b(paste(num("corrections"), "corrections applied")), span(paste("run", when))),
        div(class = "d-applied-line", "Values: ", if (!is.na(tot("values"))) tagList(tags$b(num("values")), " in output · "), tags$b(num("removed")), " removed · ",
            tags$b(num("changed")), " changed · ", span(class = "fl", paste(num("flagged"), "flagged"))),
        lapply(APPLIED_GROUPS, grp),
        if (!is.null(edit)) div(class = "d-applied-foot", tags$a(href = "#", `data-ovid-go` = edit, "Edit corrections")))) })
})
