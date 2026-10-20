# Ovid/table_rules.R  --  What a set-up table's cells must be, said once and read by both the editor (as you type) and the
# set-up check (before a run). Part of Ovid Core (shared). Sourced by every app's pipeline after config.R.
#
# Every table gets the rules its column names imply (table_rules_auto()): a column named start, end, from, to, date ...
# holds dates; one named latitude, longitude, elevation_m, shift_minutes, ... holds numbers (latitude within -90 to 90,
# longitude within -180 to 180); start/end, from/to and deployed/removed pairs run forwards; a repeated row is reported. An
# app adds to them in its CONFIG_FILES entry (`rules`): required columns, the values a column may take, columns that must
# be unique together, and the values a column must be one of (`known`, a function of the project folder: the stations or
# serials it has). `rules = FALSE` leaves a table out (one with a check of its own, as the deployment sheet).
#
# table_check(d, rules) returns one row per finding: row (in the table, from 1), column, level (error: a cell that would be
# ignored or misapplied; warning: worth a look) and problem. Nothing is changed.

RULE_DATES   <- "^(start|end|from|to|date|day|deployed|removed|installed|install_date|visit|acquired_date|approved_through|datetime_start|datetime_end|start_date|end_date)$"
RULE_NUMBERS <- "^(latitude|longitude|lat|lon|elevation|elevation_m|shift_minutes|offset_m|offset_end_m|distance_m|depth_m|utc_offset_hours|utc_offset_h|offset_hours|interval_min|geoid_n_m|t_st_m|z|x|y)$|_(m|cm|mm|kpa|hpa|minutes|min|hours|h|deg|pct|days)$"
RULE_YESNO   <- "^(enabled|show|identifies|include|active|hidden)$"            # columns that hold only a yes or a no
RULE_PERIODS <- list(c("start", "end"), c("from", "to"), c("deployed", "removed"), c("datetime_start", "datetime_end"), c("start_date", "end_date"))

# A `known` source: the values of one column of a project file (a set-up table or an output of the last run); none when the file
# is not there yet, so nothing is reported before the first run.
known_from <- function(file, col) function(project_dir) { f <- file.path(path.expand(project_dir), file)
  # safe: names to check against; a table that cannot be read gives none, so nothing is called unknown
  if (!file.exists(f)) return(character()); d <- tryCatch(fread(f, colClasses = "character", select = col), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) character() else unique(c(d[[1]][nzchar(d[[1]])], if (exists("norm_serial", mode = "function")) norm_serial(d[[1]][nzchar(d[[1]])]))) }

# Yes/no cells: shown as Yes or No in both views, written TRUE or FALSE; any of these spellings (any case) is read.
YES_WORDS <- c("yes", "y", "true", "t", "1", "on"); NO_WORDS <- c("no", "n", "false", "f", "0", "off")
yesno_value <- function(x) { x <- tolower(trimws(as.character(x))); ifelse(x %in% YES_WORDS, TRUE, ifelse(x %in% NO_WORDS, FALSE, NA)) }
# The yes/no columns of a table, by its file name (the app's CONFIG_FILES rules and the names in RULE_YESNO).
table_yesno_cols <- function(file, cols) { e <- Filter(function(x) identical(x$file, file), get0("CONFIG_FILES", ifnotfound = list()))
  ex <- if (length(e)) e[[1]]$rules else NULL; if (isFALSE(ex)) return(character()); intersect(table_rules(cols, ex)$yesno, cols) }
# d with its yes/no cells as shown (Yes, No) or as written (TRUE, FALSE); blanks and other text are left as they are.
yesno_as <- function(d, file, shown = TRUE) { for (k in table_yesno_cols(file, names(d))) { v <- yesno_value(d[[k]]); x <- as.character(d[[k]])
    x[!is.na(v)] <- if (shown) ifelse(v[!is.na(v)], "Yes", "No") else ifelse(v[!is.na(v)], "TRUE", "FALSE"); d[[k]] <- x }
  d }

table_rules_auto <- function(cols) {
  lc <- tolower(cols)
  list(dates = cols[grepl(RULE_DATES, lc)], numbers = cols[grepl(RULE_NUMBERS, lc) & !grepl(RULE_DATES, lc)], yesno = cols[grepl(RULE_YESNO, lc)],
       periods = Filter(function(p) all(p %in% lc), RULE_PERIODS), repeats = TRUE)
}
# The rules for one table: the automatic ones with the app's added (rules = list(required, choices, unique, known, dates,
# numbers, yesno)), or NULL when the app leaves the table out.
table_rules <- function(cols, extra = NULL) {
  if (isFALSE(extra)) return(NULL)
  r <- table_rules_auto(cols); if (is.null(extra)) return(r)
  for (k in c("dates", "numbers", "yesno")) r[[k]] <- unique(c(r[[k]], extra[[k]]))
  c(r, extra[setdiff(names(extra), c("dates", "numbers", "yesno"))])
}

table_check <- function(d, rules, project_dir = NULL) {
  out <- list(); add <- function(i, col, level, why) if (length(i)) out[[length(out) + 1L]] <<- data.table(row = as.integer(i), column = col, level = level, problem = why)
  if (is.null(rules) || is.null(d) || !nrow(d)) return(data.table(row = integer(), column = character(), level = character(), problem = character()))
  d <- as.data.table(d); v <- function(k) { x <- as.character(d[[k]]); x[is.na(x)] <- ""; trimws(x) }
  live <- Reduce(`|`, lapply(names(d), function(k) nzchar(v(k))))                                     # a blank row is not checked
  for (k in intersect(rules$dates, names(d))) { x <- v(k); bad <- live & nzchar(x) & is.na(parse_date_cell(x))
    add(which(bad), k, "error", paste0("'", x[bad], "' is not a date (YYYY-MM-DD, with a time if wanted)")) }
  for (k in intersect(rules$numbers, names(d))) { x <- v(k); n <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
    bad <- live & nzchar(x) & is.na(n); add(which(bad), k, "error", paste0("'", x[bad], "' is not a number"))
    lim <- if (tolower(k) %in% c("latitude", "lat")) 90 else if (tolower(k) %in% c("longitude", "lon")) 180 else NA
    if (!is.na(lim)) { o <- live & !is.na(n) & abs(n) > lim; add(which(o), k, "error", paste0(x[o], " is outside -", lim, " to ", lim)) } }
  lc <- tolower(names(d))
  for (p in rules$periods) { a <- names(d)[match(p[1], lc)]; b <- names(d)[match(p[2], lc)]
    ta <- parse_date_cell(v(a)); tb <- parse_date_cell(v(b), end = TRUE); bad <- live & !is.na(ta) & !is.na(tb) & tb < ta
    add(which(bad), b, "error", paste0(b, " is before ", a)) }
  for (k in intersect(rules$yesno, names(d))) { x <- v(k); bad <- live & nzchar(x) & is.na(yesno_value(x))
    add(which(bad), k, "error", paste0("'", x[bad], "' is not Yes or No")) }
  for (k in intersect(rules$required, names(d))) add(which(live & !nzchar(v(k))), k, "error", paste0(k, " is needed; the row is left out without it"))
  for (k in intersect(names(rules$choices), names(d))) { x <- tolower(v(k)); ok <- tolower(rules$choices[[k]])
    bad <- live & nzchar(x) & !x %in% ok; add(which(bad), k, "error", paste0("'", v(k)[bad], "' is not one of: ", paste(rules$choices[[k]], collapse = ", "))) }
  if (length(rules$unique) && all(rules$unique %in% names(d))) { key <- do.call(paste, c(lapply(rules$unique, function(k) tolower(v(k))), sep = "\r"))
    dup <- live & duplicated(key) & nzchar(gsub("\r", "", key, fixed = TRUE))
    add(which(dup), rules$unique[1], "error", paste0("the same ", paste(rules$unique, collapse = " and "), " as row ", match(key[dup], key), "; only one is used")) }
  if (!is.null(project_dir)) for (k in intersect(names(rules$known), names(d))) {
    # safe: as above, no known names means no "unknown name" finding
    kn <- tryCatch(as.character(rules$known[[k]](project_dir)), error = function(e) NULL); if (!length(kn)) next
    x <- v(k); bad <- live & nzchar(x) & !tolower(x) %in% tolower(kn) & x != "*"
    add(which(bad), k, "warning", paste0("'", x[bad], "' is not a ", k, " this project has (spelling?)")) }
  if (isTRUE(rules$repeats)) { key <- do.call(paste, c(lapply(names(d), v), sep = "\r")); dup <- live & duplicated(key)
    add(which(dup), names(d)[1], "warning", paste0("the same as row ", match(key[dup], key))) }
  if (!length(out)) return(data.table(row = integer(), column = character(), level = character(), problem = character()))
  rbindlist(out)[order(row, level != "error")]
}

# Every table of the app's CONFIG_FILES in a project, checked: table (file name), label, row, column, level, problem.
declared_checks <- function(project_dir, files = get0("CONFIG_FILES", ifnotfound = list())) {
  pd <- path.expand(project_dir); out <- list()
  for (k in names(files)) { f <- files[[k]]; p <- file.path(pd, "config", f$file); if (!file.exists(p)) next
    d <- try_or(read_cfg(p), where = f$file); if (is.null(d) || !nrow(d)) next
    x <- table_findings(d, f$file, pd)
    if (nrow(x)) out[[k]] <- cbind(data.table(table = f$file, label = f$label %||% f$file), x) }
  if (length(out)) rbindlist(out) else data.table(table = character(), label = character(), row = integer(), column = character(), level = character(), problem = character())
}
# The declared checks added to an app's own set-up check (res: its table; cols: how it names the table and the row). A finding
# for a table and row the app's own check already names is left out, so nothing is said twice.
with_declared <- function(res, project_dir, area = "area", item = "item") {
  x <- try_or(declared_checks(project_dir), where = "declared checks"); if (is.null(x) || !nrow(x)) return(res)
  where <- if (area == "table") x$table else x$label
  row_of <- function(s) suppressWarnings(as.integer(sub("^(row )?([0-9]+).*$", "\\2", s)))
  have <- if (nrow(res)) paste(res[[area]], row_of(as.character(res[[item]]))) else character()
  keep <- !paste(where, x$row) %in% have; x <- x[keep]; where <- where[keep]; if (!nrow(x)) return(res)
  add <- data.table(level = x$level, A = where, I = if (area == "table") as.character(x$row) else paste("row", x$row), problem = paste0(x$column, ": ", x$problem))
  setnames(add, c("A", "I"), c(area, item)); if ("fix" %in% names(res)) add[, fix := "Correct the cell in the table."]
  rbind(res, add, fill = TRUE)[order(match(level, c("error", "warning", "info", "note")))]
}

# What a table's rules find, by its file name: the rules of the app's CONFIG_FILES entry for it (or the automatic ones), the
# deployment sheet by its own check (core deployments.R). Used by the editor as the table is edited.
table_findings <- function(d, file, project_dir = NULL) {
  if (identical(file, get0("DEPLOY_SHEET", ifnotfound = "")) && exists("dep_clean", mode = "function")) {
    p <- try_or(dep_clean(d)$problems, where = file); if (is.null(p) || !nrow(p)) return(data.table(row = integer(), column = character(), level = character(), problem = character()))
    return(p[, .(row, column = field, level = "warning", problem)]) }
  e <- Filter(function(x) identical(x$file, file), get0("CONFIG_FILES", ifnotfound = list()))
  table_check(d, table_rules(names(d), if (length(e)) e[[1]]$rules else NULL), project_dir)
}
