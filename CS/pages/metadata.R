# CS/pages/metadata.R  --  Metadata.
# Part of the CS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, pd, outs, E, rv, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Metadata ----------------------------------------------------------
  md_data <- reactive({
    o <- outs(); req(o)
    switch(input$md_view,
      tables = { m <- o$meta; if (!nrow(m)) return(m)
        m[, .(Station = station, Stream = stream, Logger = logger_id, Serial = serial, Model = model,
              Interval = interval, Records = records, Start = start, End = end, Days = span_days, `Complete %` = complete_pct,
              Gaps = gaps, `Longest gap (h)` = largest_gap_hours, Program = program, Table = table, Columns = columns, Files = files,
              `Duplicate or bad rows` = duplicate_or_bad_rows, `Conflicting values` = conflicting_values, `Logger NAN values` = logger_nan_values,
              OS = os, Format = format, `Id from` = id_source, `Names in headers` = header_names)] },
      gaps = if (nrow(o$gaps)) o$gaps[order(-gap_hours), .(Logger = logger_id, Table = table, `Gap starts` = gap_start, `Gap ends` = gap_end,
                                                         Hours = gap_hours, Days = round(gap_hours / 24, 1), `Missing records` = missing_records)] else data.table(),
      flags = if (nrow(o$flags)) o$flags[, .(Periods = .N, Records = sum(as.numeric(records)), First = min(start), Last = max(end), Why = detail[1]),
                                         by = .(Station = station, Stream = stream, Parameter = parameter, Check = check)][order(-Records)] else data.table(),
      dups = if (nrow(o$dups)) o$dups[, .(`Table A` = paste(station_a, "/", stream_a), `Table B` = paste(station_b, "/", stream_b), `Columns that match` = columns_matching,
                                         `Columns in A` = columns_a, `Columns in B` = columns_b, `Share of the smaller table` = share, From = from, To = to, Examples = examples)] else data.table(),
      runs = if (nrow(o$runs)) { r <- o$runs[rev(seq_len(.N))]; setnames(copy(r), names(r), gsub("_", " ", names(r))) } else data.table(),
      problems = if (nrow(o$problems)) o$problems[, .(Where = where, Problem = problem)] else data.table())
  })
  output$md_dup_ui <- renderUI({ o <- outs(); if (is.null(o) || input$md_view != "dups" || !nrow(o$dups) || VIEWER) return(NULL)
    ch <- unique(c(paste(o$dups$station_a, o$dups$stream_a, sep = "|||"), paste(o$dups$station_b, o$dups$stream_b, sep = "|||")))
    div(class = "btnrow editor-only", selectInput("dup_pick", NULL, setNames(ch, sub("|||", " / ", ch, fixed = TRUE)), width = "340px"),
        actionButton("dup_drop", "Leave this table out of the next run", icon = icon("eye-slash")),
        span(class = "dim", "Sets its station to ", code("ignore"), " on the Stations page. The records are counted in the audit, and you can undo it there.")) })
  observeEvent(input$dup_drop, { o <- outs(); req(o, input$dup_pick); k <- strsplit(input$dup_pick, "|||", fixed = TRUE)[[1]]
    r <- o$registry[vapply(strsplit(station, " | ", fixed = TRUE), function(z) k[1] %in% z, logical(1)) & vapply(strsplit(stream, " | ", fixed = TRUE), function(z) k[2] %in% z, logical(1))]
    if (!nrow(r)) return(note("That table was not found in the last run.", type = "warning"))
    ok <- m_st$mutate(function(d) { for (i in seq_len(nrow(r))) {
        h <- d$logger_id == r$logger_id[i] & d$header_name != "" & d$station == k[1]
        if (any(h)) d$station[h] <- "ignore" else { new <- d[1, , drop = FALSE]; new[1, ] <- ""; new$logger_id <- r$logger_id[i]; new$table <- r$table[i]; new$station <- "ignore"
          new$notes <- paste("left out as a duplicate,", Sys.Date()); d <- rbind(d, new) } }; d })
    if (ok) { mark(); note(k[1], " / ", k[2], " will be left out. Run the pipeline to apply.") } })
  output$md_note <- renderUI({
    if (is.null(outs())) return(empty_msg())
    p(class = "dim", switch(input$md_view,
      tables = "One row per logger table after stitching. Complete % compares the records present with the number expected from the interval over the whole period.",
      gaps = "Periods with no records, longest first.",
      runs = "Every pipeline run on this project, newest first: when, by whom, with which version, and a fingerprint of the set-up tables (config hash). Two runs with the same version, files and config hash give the same output. The full record of the last run, including a fingerprint of each table, is in output/audit/run_manifest.csv.",
      flags = "Values that failed a check but were kept: stuck sensors, sensors that disagree with each other, sunlight at night, days unlike the other stations. They are marked on the Explore plots and get a flag column in exports. To remove them instead, set the rule's action to remove under Quality rules, or drag across the plot.",
      dups = "Two station tables whose values match on the same dates: usually the same logger downloaded under two names, or a converted copy kept beside the original. If they are downloads of one logger, give both the same station name on the Stations page so they are stitched into one record; if one is only a copy, leave it out here.",
      problems = "Things that went wrong in the last run without stopping it. A logger table or station listed here was left out."))
  })
  output$md_tbl <- renderDT({
    d <- md_data(); validate(need(nrow(d) > 0, if (input$md_view == "problems") "No problems in the last run." else "Nothing to show for this view."))
    t <- ovid_dt(d, selection = "none", page = 20)
    if (input$md_view == "tables") t <- formatStyle(t, "Complete %", background = styleColorBar(c(0, 100), paste0(GB$blue, "66")),
                                                    backgroundSize = "100% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center") |>
      formatStyle("Station", fontWeight = 600) |> formatRound(c("Records"), 0)
    t
  }, server = TRUE)
  output$md_dl <- downloadHandler(filename = function() paste0("metadata_", input$md_view, "_", Sys.Date(), ".csv"),
                                  content = function(f) fwrite(md_data(), f, dateTimeAs = "write.csv"))

