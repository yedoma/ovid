# CS/pages/explore_summaries.R  --  Explore: summaries.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Explore: summaries --------------------------------------------------
  su_data <- reactive({ k <- ex_key(); rv$tick; d <- ex_data(); req(d); tb <- attr(d, "tables"); sm <- if (is.null(tb)) station_streams(k, pd()) else tb[order(step_seconds), stream]
    f <- file.path(pd(), "output", "derived", paste0(safe_name(paste(k, sm, sep = "__")), "_", input$su_kind, ".csv")); f <- f[file.exists(f)]
    validate(need(length(f) > 0, "No summaries for this station yet. Run the pipeline once with this version."))
    L <- rbindlist(lapply(f, fread), fill = TRUE, idcol = ".p"); first <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else x[NA_integer_][1] }
    if (input$su_kind == "water_year") return(L[order(.p), .SD[1], by = .(water_year, parameter)][order(parameter, water_year)][, .p := NULL][])
    id <- if (input$su_kind == "daily") "Date" else "Month"; setorderv(L, c(id, ".p"))
    L[, lapply(.SD, first), by = id, .SDcols = setdiff(names(L), c(id, ".p"))] })
  su_cols <- reactive({ d <- su_data(); if (input$su_kind == "water_year" || identical(input$su_cols, "all")) return(names(d))
    id <- intersect(c("Date", "Month", "records"), names(d)); c(id, intersect(c(input$ex_params, "vpd_kpa", "albedo_calc", "net_rad_calc", "snow_depth_calc"), names(d))) })
  output$su_note <- renderUI({ if (su_stats_on()) return(su_stats_note()); o <- outs(); k <- ex_key(); if (is.null(o) || !nrow(o$derived)) return(NULL); r <- o$derived[station == k[1]]
    if (!nrow(r)) return(NULL); j <- function(x) { x <- unique(trim(unlist(strsplit(x, ",")))); x <- x[nzchar(x)]; if (length(x)) paste(x, collapse = ", ") else "none" }
    div(class = "readout", tags$b(k), span("derived: ", j(r$derived)), span("summed as totals: ", j(r$totals))) })
  su_stats_note <- function() { v <- ex_view(); span(class = "dim", tags$b(style = "color:var(--fg)", paste(ex_key(), collapse = " / ")), "  ", num(nrow(v)), " records, ", length(su_params()), " parameters. ",
    if (input$su_by != "all") paste0("Each cell is the ", tolower(names(STAT_WHAT)[STAT_WHAT == input$su_stat]), " of every record in that ", switch(input$su_by, year = "year", month = "calendar month, all years together", wy = "water year (ending 30 September)"), ". "),
    "Select rows to chart them.") }
  # broken down (Break down by): statistics of every record in the station and period of the first tab, in place of the summary table
  su_stats_on <- reactive(nzchar(input$su_by %||% ""))
  su_params <- reactive({ d <- ex_view(); req(d); v <- setdiff(names(d), META_COLS); v <- v[vapply(v, function(x) is.numeric(d[[x]]) && any(!is.na(d[[x]])), logical(1))]
    if (identical(input$su_cols, "all")) v else intersect(input$ex_params, v) })
  # missing share of a parameter, counted only over the logger tables that record it
  miss_pct <- function(d, c) { x <- d[[c]]; inn <- d$stream %in% unique(d$stream[!is.na(x)]); if (!any(inn)) 100 else round(100 * mean(is.na(x[inn])), 1) }
  su_stats <- reactive({ d <- ex_view(); p <- su_params(); k <- ex_key(); o <- outs(); u <- unit_of(p)
    validate(need(nrow(d) > 0, "No records in this period."), need(length(p) > 0, "Pick parameters on the first tab, or show every column."))
    if (input$su_by != "all") return(stat_breakdown(d, p, input$su_by, input$su_stat, u))
    g <- grp_of(p)
    fl <- if (!is.null(o) && nrow(o$flags)) o$flags[station == k[1], .(n = sum(as.numeric(records))), by = parameter] else data.table(parameter = character(), n = numeric())
    rm <- if (!is.null(o) && nrow(o$index)) o$index[station == k[1] & stage == "qc" & level == "cell", .(n = sum(entries)), by = column] else data.table(column = character(), n = numeric())
    rbindlist(lapply(p, function(c) { x <- d[[c]]; q <- stats::quantile(x, c(0.05, 0.95), na.rm = TRUE, names = FALSE); i <- which(!is.na(x))
      data.table(Parameter = c, Unit = if (is.na(u[c])) "" else u[c], Group = g[[c]], Records = sum(!is.na(x)), `Missing %` = miss_pct(d, c),
                 Min = stat_of(x, "min"), `5th pct` = signif(q[1], 5), Median = stat_of(x, "median"), Mean = stat_of(x, "mean"), `95th pct` = signif(q[2], 5), Max = stat_of(x, "max"),
                 `Std dev` = stat_of(x, "sd"), `First value` = if (length(i)) substr(fmt_ts(d$TIMESTAMP[i[1]]), 1, 10) else "", `Last value` = if (length(i)) substr(fmt_ts(d$TIMESTAMP[i[length(i)]]), 1, 10) else "",
                 `Removed by rules` = sum(rm[column == c, n]), `Flagged suspect` = sum(fl[parameter == c, n])) })) })
  su_stats_plot <- function() { t <- su_stats(); i <- head(input$su_tbl_rows_selected, 8); i <- i[i <= nrow(t)]
    validate(need(length(i) > 0, "Select rows in the table to chart them.")); p <- t$Parameter[i]
    if (input$su_by == "all") { long <- melt(ex_view()[, p, with = FALSE], measure.vars = p, variable.name = "parameter", na.rm = TRUE)
      return(ggplot(long, aes(value, fill = parameter)) + geom_histogram(bins = 60, colour = GB$bg, linewidth = 0.15) + scale_fill_manual(values = param_colours(p, GB), guide = "none") + facet_wrap(~parameter, ncol = 2, scales = "free") + labs(x = NULL, y = "Records", subtitle = "Spread of values")) }
    mv <- setdiff(names(t), c("Parameter", "Unit")); validate(need(length(mv) > 0, "Nothing to chart."))
    long <- melt(as.data.table(t)[i, c("Parameter", mv), with = FALSE], id.vars = "Parameter", measure.vars = mv, variable.name = "lev", variable.factor = FALSE)
    long <- long[!is.na(value)]; validate(need(nrow(long) > 0, "No values for these rows.")); set(long, j = "lev", value = factor(long$lev, levels = mv))
    ggplot(long, aes(lev, value, fill = Parameter)) + geom_col(width = 0.75) + scale_fill_manual(values = param_colours(p, GB), guide = "none") + facet_wrap(~Parameter, ncol = 2, scales = "free_y") + labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = if (length(mv) > 14) 60 else 0, hjust = if (length(mv) > 14) 1 else 0.5)) }
  output$su_plot_ui <- renderUI({ n <- if (su_stats_on()) length(input$su_tbl_rows_selected) else 0
    plotOutput("su_plot", height = if (n > 0) paste0(110 + 190 * ceiling(min(n, 8) / 2), "px") else "300px") })
  output$su_plot <- renderPlot({ if (su_stats_on()) return(su_stats_plot()); d <- su_data(); validate(need(input$su_kind != "water_year", "Pick Daily or Monthly for a chart; the water-year table is below."))
    p <- setdiff(su_cols(), c("Date", "Month", "records")); validate(need(length(p) > 0, "Pick parameters on the first tab.")); p <- head(p, 6)
    d <- copy(d); d[, x := if (input$su_kind == "daily") as.Date(Date) else as.Date(paste0(Month, "-01"))]
    m <- melt(d[, c("x", p), with = FALSE], id.vars = "x", na.rm = TRUE); validate(need(nrow(m) > 0, "No complete days or months for these parameters."))
    setorder(m, variable, x); m[, seg := seg_id(as.numeric(x) * 86400, if (input$su_kind == "daily") 86400 else 31 * 86400, tol = if (input$su_kind == "daily") 1.5 else 1.1), by = variable]; m[, n := .N, by = .(variable, seg)]
    ggplot(m, aes(x, value, colour = variable, group = interaction(variable, seg))) + geom_line(data = m[n > 1], linewidth = 0.45) + geom_point(data = m[n == 1], size = 0.6) +
      facet_wrap(~variable, ncol = min(3, length(p)), scales = "free_y") + scale_colour_gruvbox(guide = "none") + labs(x = NULL, y = NULL)
  }, bg = GB$bg, res = 96)
  output$su_tbl <- renderDT({ if (su_stats_on()) return(ovid_dt(su_stats(), selection = "multiple", page = 25, opts = list(scrollX = TRUE)))
    d <- su_data(); ovid_dt(d[, su_cols(), with = FALSE], page = 15, opts = list(scrollX = TRUE)) })
  output$su_dl <- downloadHandler(filename = function() paste0(safe_name(paste(ex_key(), collapse = "__")), "_", if (su_stats_on()) paste0("statistics_", input$su_by) else input$su_kind, ".csv"),
                                  content = function(f) fwrite(if (su_stats_on()) su_stats() else su_data(), f))

