# Ovid/import.R  --  Bringing in a table people already have (a CSV, a text file, an Excel sheet, rows pasted from a
# spreadsheet) without it going wrong: reading it whatever its encoding, separator or layout, finding the heading row,
# matching its columns to the table's fields, putting the values into the form the app reads, and saying plainly what
# is wrong before anything is written.
# Part of Ovid Core (shared). Sourced after metasheet.R (which keeps the older entry points, now built on this file);
# the screen is table_import_server() in metasheet_ui.R.
#
# Principles
#   - Nothing is guessed silently. What was repaired ("read as Windows-1252", "3 lines above the headings skipped") is
#     listed as notes; what cannot be told apart (03/04/2024) must be chosen by the person; what cannot be read is
#     reported with the rows and an example.
#   - A problem is an error (stops the import), a warning (the rows go in, with what will happen to them said) or a note.
#   - The file is only read. Every value stays text until a field's type asks for more; a value that cannot be made
#     into its field's type is kept as typed and reported, never dropped without a word.
#   - Functions here are plain (no Shiny) so they can be tested on awkward files.

IMPORT_MISSING <- c("na", "n/a", "n.a.", "n.a", "#n/a", "nan", "null", "nil", "-", "--", "---", "?", "??", "#value!", "#ref!", "#div/0!", "#name?", "#num!", "#null!")
IMPORT_MAX_BYTES <- 100e6
IMPORT_MAX_ROWS <- 200000L
IMPORT_MAX_COLS <- 500L

imp_s <- function(...) paste0(...)
imp_plural <- function(n, one, many = paste0(one, "s")) paste(format(n, big.mark = ","), if (n == 1) one else many)

# ---- text and cells -----------------------------------------------------------------------------------------------------
# Non-breaking and zero-width characters (they come from web pages and Word), one kind of space, the ends trimmed.
imp_text <- function(x) { x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("[    　]", " ", x); x <- gsub("[​-‍⁠﻿]", "", x)
  x <- gsub("[\r\n\t]+", " ", x); trimws(x) }

# One heading per column: no empty ones ("column 3"), no duplicates ("Depth (2)"), spaces collapsed.
imp_headings <- function(h) { h <- gsub("\\s+", " ", imp_text(h)); e <- !nzchar(h); h[e] <- paste("column", which(e))
  for (n in unique(h[duplicated(h)])) { i <- which(h == n); h[i[-1]] <- paste0(n, " (", seq_along(i)[-1], ")") }
  h }

# ---- reading ------------------------------------------------------------------------------------------------------------
# What a file is, from its first bytes (the name only decides when the bytes do not).
imp_kind <- function(path, name = path) {
  b <- readBin(path, "raw", 16L); ext <- tolower(tools::file_ext(name))
  if (!length(b)) return("empty")
  if (identical(b[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return(if (ext == "ods") "ods" else "xlsx")
  if (identical(b[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) return("xls")
  if (identical(b[1:4], as.raw(c(0x25, 0x50, 0x44, 0x46)))) return("pdf")
  if (identical(b[1:3], as.raw(c(0xff, 0xd8, 0xff))) || identical(b[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))) return("image")
  "text"
}

# Text as UTF-8: a byte-order mark says UTF-8 or UTF-16; otherwise what is valid UTF-8 is, and the rest is Windows-1252 (what
# Excel on Windows writes). Returns list(text, note).
imp_decode <- function(raw) {
  note <- NULL
  if (length(raw) >= 2 && ((raw[1] == 0xff && raw[2] == 0xfe) || (raw[1] == 0xfe && raw[2] == 0xff))) {
    x <- iconv(list(raw), from = if (raw[1] == 0xff) "UTF-16LE" else "UTF-16BE", to = "UTF-8"); x <- sub("^﻿", "", x[[1]]); note <- "Read as UTF-16 text."
    if (is.na(x)) stop("The file looks like UTF-16 text but could not be read. Save it again as CSV (UTF-8) and import that.", call. = FALSE)
    return(list(text = x, note = note)) }
  head <- raw[seq_len(min(length(raw), 4096L))]
  if (length(head) > 4 && mean(head == as.raw(0)) > 0.3) {                        # UTF-16 without a mark: a zero beside every letter
    x <- iconv(list(raw), from = "UTF-16LE", to = "UTF-8"); if (!is.na(x[[1]])) return(list(text = x[[1]], note = "Read as UTF-16 text.")) }
  if (any(head == as.raw(0))) stop("This looks like a binary file, not a table of text. Save it as CSV or Excel (.xlsx) and import that.", call. = FALSE)
  if (length(raw) >= 3 && raw[1] == 0xef && raw[2] == 0xbb && raw[3] == 0xbf) raw <- raw[-(1:3)]
  raw <- raw[raw != as.raw(0)]
  x <- rawToChar(raw); Encoding(x) <- "unknown"
  if (validUTF8(x)) { Encoding(x) <- "UTF-8"; return(list(text = x, note = NULL)) }
  y <- iconv(x, from = "CP1252", to = "UTF-8", sub = "?")
  list(text = y, note = "Read as Windows-1252 text (it was not UTF-8), so accents and degree signs should be right; check them below.")
}

# The separator: the one that gives the same number of fields on most of the first lines (quotes respected).
imp_sep <- function(text) {
  ln <- strsplit(text, "\r\n|\n|\r")[[1]]; ln <- ln[nzchar(trimws(ln))]; ln <- utils::head(ln, 80L)
  if (!length(ln)) return(",")
  best <- NULL; bs <- -1
  for (s in c(",", ";", "\t", "|")) {
    # safe: probing the separator; one that fails to count is ruled out
    n <- tryCatch(suppressWarnings(utils::count.fields(textConnection(ln), sep = s, quote = "\"", blank.lines.skip = FALSE, comment.char = "")), error = function(e) NULL)
    n <- n[!is.na(n)]; if (!length(n)) next
    m <- as.integer(names(sort(table(n), decreasing = TRUE))[1]); if (m < 2) next
    sc <- mean(n == m) + min(m, 20) / 1000
    if (sc > bs) { bs <- sc; best <- s } }
  best %||% ","
}

# Every cell as text, no heading yet: a data.table of V1..Vn.
imp_grid_text <- function(text) {
  s <- imp_sep(text); ln <- strsplit(text, "\r\n|\n|\r")[[1]]
  # safe: probing the quoting; a count that fails is ruled out
  cf <- function(q) tryCatch(suppressWarnings(utils::count.fields(textConnection(utils::head(ln, 5000L)), sep = s, quote = q, blank.lines.skip = FALSE, comment.char = "")), error = function(e) NA_integer_)
  nc <- suppressWarnings(max(c(cf("\""), cf("")), na.rm = TRUE))                                  # a stray quote makes the count NA with quoting on: count without it too
  if (!is.finite(nc) || nc < 1) nc <- 1L
  if (nc > IMPORT_MAX_COLS) stop(imp_s("This table has ", nc, " columns, more than the ", IMPORT_MAX_COLS, " that can be matched. Is it the right file?"), call. = FALSE)
  rd <- function(q) utils::read.table(text = text, sep = s, quote = q, fill = TRUE, header = FALSE, colClasses = "character", comment.char = "", na.strings = character(),
                                      col.names = paste0("V", seq_len(nc)), strip.white = FALSE, blank.lines.skip = FALSE, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  d <- tryCatch(suppressWarnings(rd("\"")), error = function(e) tryCatch(suppressWarnings(rd("")), error = function(e2) NULL))    # a stray quote: read without quoting
  if (is.null(d)) stop("The text could not be split into columns. Save it as CSV (comma-separated) and import that.", call. = FALSE)
  setattr(as.data.table(d), "sep", s)
}

imp_grid_excel <- function(path, sheet) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Reading Excel files needs the readxl package.", call. = FALSE)
  d <- tryCatch(suppressMessages(suppressWarnings(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal", trim_ws = FALSE))),
                error = function(e) { m <- conditionMessage(e)
                  stop(if (grepl("encrypt|password|zip|signature|unable to open|not a valid|corrupt", m, ignore.case = TRUE))
                         "This Excel file could not be opened (it may be password-protected, damaged, or not really an Excel file). Open it in Excel, save a copy as .xlsx or CSV, and import that."
                       else paste("This Excel sheet could not be read:", m), call. = FALSE) })
  d <- as.data.table(d); if (ncol(d)) setnames(d, paste0("V", seq_len(ncol(d)))); d
}

# ---- the heading row ----------------------------------------------------------------------------------------------------
# Titles, notes and blank lines often sit above the headings. Each of the first rows is scored: cells that match a field's name,
# label or alias count most, then rows that are all words (not numbers), have many filled cells and repeat nothing.
imp_header_row <- function(g, spec = NULL) {
  n <- nrow(g); if (!n) return(1L)
  m <- min(n, 40L); cells <- lapply(seq_len(m), function(i) imp_text(unlist(g[i], use.names = FALSE)))
  filled <- vapply(cells, function(v) sum(nzchar(v)), 0L); mx <- max(filled); if (mx == 0) return(1L)
  num <- function(v) !is.na(suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))) | grepl("^\\d{1,4}[-/.]\\d{1,2}[-/.]\\d{1,4}", v)
  sc <- vapply(seq_len(m), function(i) { v <- cells[[i]]; v <- v[nzchar(v)]; k <- length(v); if (k == 0) return(-Inf)
    hit <- if (!is.null(spec) && nrow(spec)) sum(imp_guess(v, spec, stages = "exact")$field != "") else 0
    hit * 6 + (mean(!num(v)) == 1) * 2 + (length(unique(tolower(v))) == k) * 1 + 3 * k / mx - (k < 2 && mx > 1) * 4 - (i < m && filled[i + 1] == 0) * 1 }, 0)
  which.max(sc)
}

# The table from a grid: headings from row `hdr`, the rows below it, empty rows and columns dropped, cells cleaned. Notes say what was done.
imp_table <- function(g, spec = NULL, header_row = NULL) {
  notes <- character()
  if (!nrow(g) || !ncol(g)) stop("The file has no rows.", call. = FALSE)
  for (v in names(g)) set(g, j = v, value = imp_text(g[[v]]))
  g <- g[rowSums(as.matrix(g) != "") > 0 | TRUE]
  auto <- is.null(header_row); hdr <- if (auto) imp_header_row(g, spec) else max(1L, min(as.integer(header_row), nrow(g)))
  if (hdr > 1 && auto) notes <- c(notes, imp_s(imp_plural(hdr - 1L, "line"), " above the headings ", if (hdr == 2) "was" else "were", " skipped (row ", hdr, " holds the headings; change it below if that is wrong)."))
  h <- unlist(g[hdr], use.names = FALSE); body <- g[-seq_len(hdr)]
  if (!nrow(body)) stop("The file has headings but no rows under them.", call. = FALSE)
  body <- body[rowSums(as.matrix(body) != "") > 0]                                   # blank rows (any among the rows) go
  if (!nrow(body)) stop("The file has headings but no rows under them.", call. = FALSE)
  if (nrow(body) > IMPORT_MAX_ROWS) stop(imp_s("The file has ", format(nrow(body), big.mark = ","), " rows. A table of metadata is much smaller than that (the limit is ", format(IMPORT_MAX_ROWS, big.mark = ","), "); is this the right file?"), call. = FALSE)
  keep <- nzchar(h) | vapply(body, function(x) any(nzchar(x)), NA)                  # a column with neither a heading nor a value goes
  h <- h[keep]; body <- body[, which(keep), with = FALSE]
  if (!ncol(body)) stop("The file has no columns with anything in them.", call. = FALSE)
  setnames(body, imp_headings(h))
  # cells meaning "nothing": N/A, #N/A, -, null ...; Excel's error values
  miss <- 0L; errs <- 0L
  for (v in names(body)) { x <- body[[v]]; k <- tolower(x) %in% IMPORT_MISSING & nzchar(x); e <- grepl("^#(n/a|value!|ref!|div/0!|name\\?|num!|null!)$", x, ignore.case = TRUE)
    miss <- miss + sum(k & !e); errs <- errs + sum(e); if (any(k)) set(body, which(k), v, "") }
  if (miss) notes <- c(notes, imp_s(imp_plural(miss, "cell"), " reading N/A, null or - ", if (miss == 1) "was" else "were", " left blank."))
  if (errs) notes <- c(notes, imp_s(imp_plural(errs, "cell"), " held an Excel error (#N/A, #REF! ...) and ", if (errs == 1) "was" else "were", " left blank; check the source."))
  setattr(body, "notes", notes); setattr(body, "header_row", hdr); body
}

# A file or pasted text read into a table of text, whatever its kind. Attributes: notes (what was repaired), header_row (row of the
# grid holding the headings), sheet and sheets (Excel: the one read and all with their sizes), kind.
#   path, name   the file and the name it had when it was uploaded (the extension is not trusted over the content)
#   sheet        Excel: the sheet to read (name or number); NULL picks the one whose rows best match `spec`
#   spec         the table's fields (meta_defaults()), used to find the headings
#   header_row   row of the headings when the person said so
meta_import_read <- function(path, sheet = NULL, name = path, spec = NULL, header_row = NULL) {
  if (!file.exists(path)) stop("The file could not be found.", call. = FALSE)
  sz <- file.size(path); if (is.na(sz) || sz == 0) stop("The file is empty.", call. = FALSE)
  if (sz > IMPORT_MAX_BYTES) stop(imp_s("The file is ", round(sz / 1e6), " MB. A table of metadata is far smaller; is this the right file?"), call. = FALSE)
  kind <- imp_kind(path, name); notes <- character(); sheets <- NULL
  if (kind %in% c("pdf", "image")) stop(imp_s("This is a ", if (kind == "pdf") "PDF" else "picture", ", not a table. Ovid reads CSV, text, and Excel (.xlsx, .xls) files; copy the table into a spreadsheet and save that."), call. = FALSE)
  if (kind == "ods") stop("OpenDocument spreadsheets (.ods) are not read. In your spreadsheet program, save a copy as .xlsx or CSV and import that.", call. = FALSE)
  if (kind == "text") { ext <- tolower(tools::file_ext(name))
    b0 <- readBin(path, "raw", 400L); top <- tolower(substr(iconv(rawToChar(b0[b0 != as.raw(0)]), "latin1", "UTF-8", sub = ""), 1, 200))   # latin1: any bytes will do for sniffing
    if (grepl("<html|<!doctype html|<table", top)) stop("This is a web page (HTML) saved with a spreadsheet name, not a real spreadsheet. Open it in Excel, then Save As .xlsx or CSV, and import that.", call. = FALSE)
    if (grepl("^\\s*[\\[{]", top) && ext %in% c("json", "geojson")) stop("JSON files are not read. Save the table as CSV and import that.", call. = FALSE)
    dc <- imp_decode(readBin(path, "raw", sz)); if (!is.null(dc$note)) notes <- c(notes, dc$note)
    if (!nzchar(trimws(dc$text))) stop("The file is empty.", call. = FALSE)
    g <- imp_grid_text(dc$text); sep <- attr(g, "sep")
    if (!is.null(sep) && sep != ",") notes <- c(notes, paste0("Columns are separated by ", c(";" = "semicolons", "\t" = "tabs", "|" = "bars")[[sep]], "."))
  } else {
    sh <- tryCatch(suppressWarnings(readxl::excel_sheets(path)), error = function(e) stop("This Excel file could not be opened (it may be password-protected, damaged, or not really an Excel file). Open it in Excel, save a copy as .xlsx or CSV, and import that.", call. = FALSE))
    if (!length(sh)) stop("The Excel file has no sheets.", call. = FALSE)
    # safe: a sheet that cannot be read counts as empty and is not picked
    info <- lapply(sh, function(s) { gg <- tryCatch(imp_grid_excel(path, s), error = function(e) NULL)
      if (is.null(gg) || !nrow(gg)) return(list(n = 0L, score = -Inf, g = NULL))
      for (v in names(gg)) set(gg, j = v, value = imp_text(gg[[v]])); gg <- gg[rowSums(as.matrix(gg) != "") > 0]
      if (!nrow(gg)) return(list(n = 0L, score = -Inf, g = NULL))
      h <- imp_header_row(gg, spec); hit <- if (!is.null(spec) && nrow(spec)) sum(imp_guess(imp_text(unlist(gg[h], use.names = FALSE)), spec, stages = "exact")$field != "") else 0
      list(n = nrow(gg), score = hit * 1000 + nrow(gg), g = gg) })
    sheets <- data.table(sheet = sh, rows = vapply(info, `[[`, 0L, "n"))
    pick <- if (!is.null(sheet) && (sheet %in% sh || (is.numeric(sheet) && sheet >= 1 && sheet <= length(sh)))) { if (is.numeric(sheet)) as.integer(sheet) else match(sheet, sh) }
            else which.max(vapply(info, `[[`, 0, "score"))
    if (!length(pick) || !is.finite(info[[pick]]$score)) stop(if (length(sh) > 1) "None of the sheets in this Excel file has any rows." else "The Excel sheet has no rows.", call. = FALSE)
    g <- info[[pick]]$g; if (is.null(sheet) && length(sh) > 1) notes <- c(notes, imp_s("Read the sheet \"", sh[pick], "\" (the file has ", length(sh), " sheets: ", paste(sh, collapse = ", "), ")."))
    sheet <- sh[pick] }
  x <- imp_table(g, spec, header_row); notes <- c(notes, attr(x, "notes"))
  setattr(x, "notes", notes); setattr(x, "kind", kind); setattr(x, "sheet", sheet); setattr(x, "sheets", sheets); x
}

# Rows pasted from a spreadsheet (tabs) or typed (commas); the same repairs as a file. A first line that is data, not headings, is
# the caller's to notice (no heading matches).
meta_import_text <- function(text, spec = NULL, header_row = NULL) {
  if (!nzchar(trimws(text))) return(data.table())
  g <- imp_grid_text(text); x <- imp_table(g, spec, header_row); setattr(x, "kind", "text"); x
}

# ---- matching columns to fields -----------------------------------------------------------------------------------------
imp_norm <- function(x) gsub("[^a-z0-9]", "", tolower(imp_text(x)))
imp_words <- function(x) { x <- gsub("([a-z])([A-Z])", "\\1 \\2", imp_text(x)); w <- strsplit(tolower(gsub("[^A-Za-z0-9]+", " ", x)), " +")[[1]]; w[nzchar(w)] }
imp_nounit <- function(x) trimws(gsub("\\s*[\\(\\[\\{][^\\)\\]\\}]*[\\)\\]\\}]", "", imp_text(x)))      # "Depth (m)" -> "Depth"

# Which field each column heading is, and how sure: how is "memory" (matched the same way last time), "exact" (the field's name,
# label or an alias), "units" (the same once a unit in brackets is left out), "words" (an alias's words all appear in the heading)
# or "fuzzy" (a spelling slip away); "" for no match. A field is given to one column only, the surest match first.
#   memory   data.table(heading, field) of earlier matches; stages   which of the five to use
imp_guess <- function(cols, spec, memory = NULL, stages = c("memory", "exact", "units", "words", "fuzzy")) {
  cols <- as.character(cols); n <- length(cols); out <- data.table(field = rep("", n), how = rep("", n))
  if (!n || !nrow(spec)) return(out)
  keys <- lapply(seq_len(nrow(spec)), function(j) { a <- trimws(strsplit(spec$aliases[j], "|", fixed = TRUE)[[1]]); unique(imp_norm(c(spec$field[j], spec$label[j], a))) })
  aliases <- lapply(seq_len(nrow(spec)), function(j) { a <- trimws(strsplit(spec$aliases[j], "|", fixed = TRUE)[[1]]); unique(c(spec$label[j], spec$field[j], a[nzchar(a)])) })
  take <- function(i, f, how) if (!nzchar(out$field[i]) && !f %in% out$field) { set(out, i, "field", f); set(out, i, "how", how) }
  n0 <- imp_norm(cols); n1 <- imp_norm(vapply(cols, imp_nounit, ""))
  if ("memory" %in% stages && !is.null(memory) && nrow(memory)) for (i in seq_len(n)) { k <- match(n0[i], imp_norm(memory$heading)); if (!is.na(k) && memory$field[k] %in% spec$field) take(i, memory$field[k], "memory") }
  if ("exact" %in% stages) for (i in seq_len(n)) { h <- which(vapply(keys, function(k) n0[i] %in% k, NA)); if (length(h)) take(i, spec$field[h[1]], "exact") }
  if ("units" %in% stages) for (i in seq_len(n)) if (nzchar(n1[i])) { h <- which(vapply(keys, function(k) n1[i] %in% k, NA)); if (length(h)) take(i, spec$field[h[1]], "units") }
  if ("words" %in% stages) for (i in which(out$field == "")) { w <- imp_words(imp_nounit(cols[i])); if (!length(w)) next
    noise <- c("date", "dates", "time", "datetime", "the", "of", "day", "when", "no")                          # "Date in", "Date installed", "Time out" are In, Installed, Out
    wn <- setdiff(w, noise)
    hit <- which(vapply(seq_len(nrow(spec)), function(j) any(vapply(aliases[[j]], function(a) { aw <- imp_words(a); awn <- setdiff(aw, noise)
      (length(aw) >= 1 && (length(aw) >= 2 || nchar(aw) >= 5) && all(aw %in% w)) || (length(wn) >= 1 && length(awn) >= 1 && length(wn) < length(w) && setequal(wn, awn)) }, NA)), NA))
    hit <- hit[!spec$field[hit] %in% out$field]; if (length(hit) == 1) take(i, spec$field[hit], "words") }
  if ("fuzzy" %in% stages) for (i in which(out$field == "")) { v <- n1[i]; if (nchar(v) < 5) next
    d <- vapply(keys, function(k) { k <- k[nchar(k) >= 5 & abs(nchar(k) - nchar(v)) <= 2]; if (!length(k)) Inf else min(drop(utils::adist(v, k))) }, 0)
    d[spec$field %in% out$field] <- Inf; lim <- ceiling(nchar(v) / 4); b <- which(d == min(d))
    if (is.finite(min(d)) && min(d) <= lim && length(b) == 1 && sum(d <= lim) == 1) take(i, spec$field[b], "fuzzy") }
  out
}
# The older entry point: the fields only, from the surest matches (name, label, alias, unit left out). Used where nobody checks.
meta_guess <- function(cols, spec) imp_guess(cols, spec, stages = c("exact", "units"))$field

# ---- values --------------------------------------------------------------------------------------------------------------
IMP_MONTHS <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
imp_month <- function(w) { w <- tolower(w); unname(IMP_MONTHS[substr(w, 1, 3)]) }

# Times like "14:30", "2:30 PM", "14:30:15" as "HH:MM[:SS]" ("" for a blank, NA for what is not a time). Vectorised.
imp_time <- function(s) { s <- trimws(as.character(s)); out <- rep("", length(s)); pat <- "^(\\d{1,2}):(\\d{2})(?::(\\d{2}))?\\s*([AaPp]\\.?[Mm]\\.?)?$"
  has <- nzchar(s); ok <- has & grepl(pat, s, perl = TRUE); out[has & !ok] <- NA_character_
  if (any(ok)) { z <- s[ok]; h <- as.integer(sub(pat, "\\1", z, perl = TRUE)); mi <- as.integer(sub(pat, "\\2", z, perl = TRUE)); se <- suppressWarnings(as.integer(sub(pat, "\\3", z, perl = TRUE)))
    ap <- tolower(substr(sub(pat, "\\4", z, perl = TRUE), 1, 1)); pm <- ap == "p"; has_ap <- nzchar(ap)
    bad <- (has_ap & (h < 1 | h > 12)) | (!has_ap & h > 23) | mi > 59 | (!is.na(se) & se > 59)
    h2 <- ifelse(has_ap, h %% 12 + ifelse(pm, 12L, 0L), h)
    r <- sprintf("%02d:%02d%s", h2, mi, ifelse(is.na(se), "", sprintf(":%02d", se))); r[bad] <- NA_character_; out[ok] <- r }
  out }

# Year, month, day as "YYYY-MM-DD", NA when not a real calendar date (Feb 30, month 13). Two-digit years: 00-69 are 20xx, the rest 19xx. Vectorised.
imp_ymd <- function(y, m, d) { y <- as.integer(y); m <- as.integer(m); d <- as.integer(d); y <- ifelse(!is.na(y) & y < 100, y + ifelse(y <= 69, 2000L, 1900L), y)
  ok <- !is.na(y) & !is.na(m) & !is.na(d) & m >= 1 & m <= 12 & d >= 1 & d <= 31; r <- rep(NA_character_, length(y))
  if (any(ok)) { z <- sprintf("%04d-%02d-%02d", y[ok], m[ok], d[ok]); good <- !is.na(as.Date(z, format = "%Y-%m-%d")) & format(as.Date(z, format = "%Y-%m-%d"), "%Y-%m-%d") == z; r[ok] <- ifelse(good, z, NA_character_) }
  r }

# Date cells of one column made unambiguous: "YYYY-MM-DD" or "YYYY-MM-DD HH:MM[:SS]". Forms the apps already read as they are (a year,
# a year and month, "Sept 2024") are kept as typed. Excel's serial numbers, names of months, 2-digit years, day first or month first.
# order: "auto" works it out from the column (a part over 12 settles it) and reports it when it cannot; "dmy" or "mdy" says so.
# Returns list(value, bad (rows not understood), order (used, or "ambiguous"/"mixed"/"" when there was nothing to decide), serial (n converted from Excel)). Vectorised.
imp_dates <- function(x, order = "auto") {
  x <- imp_text(x); n <- length(x); out <- x; bad <- rep(FALSE, n); has <- nzchar(x); serial <- 0L; used <- ""
  put <- function(idx, d, tm) { ok <- !is.na(d) & !is.na(tm); if (any(ok)) out[idx[ok]] <<- paste0(d[ok], ifelse(nzchar(tm[ok]), paste0(" ", tm[ok]), "")); if (any(!ok)) bad[idx[!ok]] <<- TRUE }
  keep <- has & (grepl("^(19|20)\\d{2}$", x) | grepl("^(19|20)\\d{2}[-/]\\d{1,2}$", x) | grepl("^[A-Za-z]{3,9}\\.?,? +(19|20)\\d{2}$", x))      # forms the apps read as they are
  pi <- "^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})(?:[ T](\\d{1,2}:\\d{2}(?::\\d{2})?)(?:\\.\\d+)?Z?)?$"
  iso <- which(has & !keep & grepl(pi, x, perl = TRUE))
  if (length(iso)) { z <- x[iso]; put(iso, imp_ymd(sub(pi, "\\1", z, perl = TRUE), sub(pi, "\\2", z, perl = TRUE), sub(pi, "\\3", z, perl = TRUE)), imp_time(sub(pi, "\\4", z, perl = TRUE))) }
  num <- suppressWarnings(as.numeric(x)); done <- rep(FALSE, n); done[iso] <- TRUE
  ser <- which(has & !keep & !done & !is.na(num) & num > 20000 & num < 80000)
  if (length(ser)) { t <- as.POSIXct(round(num[ser] * 86400) - 25569 * 86400, origin = "1970-01-01", tz = "UTC"); frac <- num[ser] %% 1 > 1e-6
    out[ser] <- ifelse(frac, format(t, "%Y-%m-%d %H:%M"), format(t, "%Y-%m-%d")); serial <- length(ser); done[ser] <- TRUE }
  ps <- "^(\\d{1,2})[-/.](\\d{1,2})[-/.](\\d{2,4})(?:\\s+(\\S.*))?$"
  sl <- which(has & !keep & !done & grepl(ps, x, perl = TRUE))
  if (length(sl)) { z <- x[sl]; a <- as.integer(sub(ps, "\\1", z, perl = TRUE)); b <- as.integer(sub(ps, "\\2", z, perl = TRUE)); y <- as.integer(sub(ps, "\\3", z, perl = TRUE)); tm <- imp_time(sub(ps, "\\4", z, perl = TRUE))
    dmy_only <- any(a > 12); mdy_only <- any(b > 12)
    ord <- if (order %in% c("dmy", "mdy")) order else if (dmy_only && mdy_only) "mixed" else if (dmy_only) "dmy" else if (mdy_only) "mdy" else "ambiguous"; used <- ord
    if (ord %in% c("dmy", "mdy", "mixed")) {
      d <- if (ord == "mdy") imp_ymd(y, a, b) else if (ord == "dmy") imp_ymd(y, b, a) else ifelse(a > 12, imp_ymd(y, b, a), ifelse(b > 12, imp_ymd(y, a, b), NA_character_))
      put(sl, d, tm) }
    done[sl] <- TRUE }
  w <- which(has & !keep & !done)
  if (length(w)) { z <- x[w]; r <- rep(NA_character_, length(w)); tm <- rep(NA_character_, length(w))
    p1 <- "^(\\d{1,2})(?:st|nd|rd|th)?[ .-]+([A-Za-z]{3,9})\\.?,?[ .-]+(\\d{2,4})(?:[ ,]+(\\S.*))?$"; k1 <- grepl(p1, z, perl = TRUE)
    if (any(k1)) { q <- z[k1]; r[k1] <- imp_ymd(sub(p1, "\\3", q, perl = TRUE), imp_month(sub(p1, "\\2", q, perl = TRUE)), sub(p1, "\\1", q, perl = TRUE)); tm[k1] <- imp_time(sub(p1, "\\4", q, perl = TRUE)) }
    p2 <- "^([A-Za-z]{3,9})\\.?[ .-]+(\\d{1,2})(?:st|nd|rd|th)?,?[ .-]+(\\d{2,4})(?:[ ,]+(\\S.*))?$"; k2 <- !k1 & grepl(p2, z, perl = TRUE)
    if (any(k2)) { q <- z[k2]; r[k2] <- imp_ymd(sub(p2, "\\3", q, perl = TRUE), imp_month(sub(p2, "\\1", q, perl = TRUE)), sub(p2, "\\2", q, perl = TRUE)); tm[k2] <- imp_time(sub(p2, "\\4", q, perl = TRUE)) }
    put(w, r, ifelse(is.na(tm), ifelse(k1 | k2, NA_character_, ""), tm)) }
  bad[!has] <- FALSE
  list(value = out, bad = bad, order = used, serial = serial)
}

# Number cells of one column as plain numbers in text ("1234.5"): comma decimals ("0,25", "1.234,5"), thousands separators ("1,234.5"),
# the typographic minus, a unit after the number (the field's own). Returns list(value, bad, comma (a decimal comma was read), thousands (n read as thousands)). Vectorised.
imp_numbers <- function(x, unit = "") {
  x <- imp_text(x); x <- gsub("[−‒–]", "-", x)
  if (nzchar(unit)) x <- trimws(sub(paste0("\\s*", gsub("([.+*?^$()|\\[\\]\\\\])", "\\\\\\1", unit), "$"), "", x, ignore.case = TRUE))
  x <- gsub("(?<=\\d) (?=\\d{3}(\\D|$))", "", x, perl = TRUE)                          # a space as the thousands separator ("1 234,5")
  has <- nzchar(x); v <- x; th <- 0L
  dec_comma <- any(grepl("^[-+]?\\d+,(\\d{1,2}|\\d{4,})$", x)) || any(grepl("^[-+]?\\d{1,3}(\\.\\d{3})+,\\d+$", x))
  if (dec_comma) { eu <- grepl("^[-+]?\\d{1,3}(\\.\\d{3})+(,\\d+)?$", v); v[eu] <- gsub(",", ".", gsub(".", "", v[eu], fixed = TRUE), fixed = TRUE); r <- has & !eu; v[r] <- gsub(",", ".", v[r], fixed = TRUE) }
  else { t <- grepl("^[-+]?\\d{1,3}(,\\d{3})+(\\.\\d+)?$", v); v[t] <- gsub(",", "", v[t], fixed = TRUE); th <- sum(t) }
  n <- suppressWarnings(as.numeric(v)); bad <- has & is.na(n) & !grepl("^[-+]?(inf|nan)$", tolower(v))
  list(value = v, bad = bad, comma = dec_comma, thousands = th)
}

# The first few distinct values, as text for a message.
imp_eg <- function(x, k = 3L) { u <- unique(x[nzchar(x)]); s <- paste0("\"", utils::head(u, k), "\"", collapse = ", "); if (length(u) > k) paste0(s, ", ...") else s }

# ---- preparing and checking an import -----------------------------------------------------------------------------------
# x: the table read; map: the field for each column ("" leaves it out, "+Name" adds a column). Values of mapped columns are put in the
# form the field's type reads; what cannot be is kept as typed and listed. Returns list(x (prepared copy), issues, dates (order used)).
# issues: severity ("error" stops the import, "warning" says what will happen, "note"), field, column, n, message.
imp_prepare <- function(x, map, spec, date_order = "auto") {
  x <- copy(as.data.table(x)); issues <- list(); used_dates <- ""
  add <- function(sev, msg, field = "", column = "", n = NA_integer_) issues[[length(issues) + 1L]] <<- data.table(severity = sev, field = field, column = column, n = n, message = msg)
  map <- as.character(map); map[is.na(map)] <- ""
  for (i in seq_along(map)) { m <- map[i]; if (!nzchar(m) || startsWith(m, "+")) next
    j <- match(m, spec$field); if (is.na(j)) { add("error", imp_s("The field \"", m, "\" is not one of this table's fields."), m, names(x)[i]); next }
    ty <- spec$type[j]; lab <- spec$label[j]; col <- names(x)[i]; v <- x[[i]]; has <- nzchar(v)
    if (ty %in% c("date", "datetime")) {
      r <- imp_dates(v, date_order); set(x, j = i, value = r$value)
      if (r$serial) add("note", imp_s(imp_plural(r$serial, "date"), " in \"", col, "\" ", if (r$serial == 1) "was" else "were", " Excel serial numbers (like 45531) and ", if (r$serial == 1) "was" else "were", " turned into dates."), m, col, r$serial)
      if (r$order == "ambiguous") add("error", imp_s("Dates in \"", col, "\" such as ", imp_eg(v[grepl("^\\d{1,2}[-/.]\\d{1,2}[-/.]\\d{2,4}", v)], 2), " can be day/month or month/day, and nothing in the file settles it. Choose which below."), m, col)
      else if (r$order == "mixed") add("error", imp_s("\"", col, "\" mixes day-first and month-first dates (some have a first part over 12, others a second part over 12). Fix the source, or choose an order below and the others are listed as not dates."), m, col)
      else if (nzchar(r$order) && date_order == "auto") add("note", imp_s("Dates in \"", col, "\" were read ", c(dmy = "day/month/year", mdy = "month/day/year")[[r$order]], " because the file shows it."), m, col)
      if (nzchar(r$order) && r$order %in% c("dmy", "mdy")) used_dates <- r$order
      if (any(r$bad)) add("warning", imp_s(imp_plural(sum(r$bad), "value"), " in \"", col, "\" (", lab, ") ", if (sum(r$bad) == 1) "is" else "are", " not ", if (sum(r$bad) == 1) "a date" else "dates", ", e.g. ", imp_eg(v[r$bad]), ". ", if (sum(r$bad) == 1) "It is" else "They are", " kept as typed and read as blank by the app."), m, col, sum(r$bad))
    } else if (ty %in% c("number", "integer")) {
      r <- imp_numbers(v, trimws(spec$unit[j])); set(x, j = i, value = r$value)
      if (r$comma) add("note", imp_s("\"", col, "\" uses a decimal comma (0,25); it was read as 0.25."), m, col)
      if (r$thousands) add("warning", imp_s(imp_plural(r$thousands, "value"), " in \"", col, "\" like 1,234 ", if (r$thousands == 1) "was" else "were", " read with the comma as a thousands separator (1234). If it is a decimal comma, change the source."), m, col, r$thousands)
      if (any(r$bad)) add("warning", imp_s(imp_plural(sum(r$bad), "value"), " in \"", col, "\" (", lab, ") ", if (sum(r$bad) == 1) "is" else "are", " not ", if (sum(r$bad) == 1) "a number" else "numbers", ", e.g. ", imp_eg(v[r$bad]), ". ", if (sum(r$bad) == 1) "It is" else "They are", " kept as typed and read as blank by the app."), m, col, sum(r$bad))
      if (ty == "integer") { nn <- suppressWarnings(as.numeric(r$value)); k <- which(has & !is.na(nn) & nn != round(nn)); if (length(k)) add("warning", imp_s(imp_plural(length(k), "value"), " in \"", col, "\" (", lab, ") ", if (length(k) == 1) "is" else "are", " not whole numbers, e.g. ", imp_eg(v[k]), "."), m, col, length(k)) }
    } else if (ty == "choice") {
      ch <- trimws(strsplit(spec$choices[j], "|", fixed = TRUE)[[1]]); ch <- ch[nzchar(ch)]
      if (length(ch)) { k <- match(imp_norm(v), imp_norm(ch)); canon <- ifelse(is.na(k), v, ch[k]); set(x, j = i, value = canon)
        un <- has & is.na(k); if (any(un)) add("warning", imp_s(imp_plural(length(unique(v[un])), "value"), " in \"", col, "\" (", lab, ") ", if (length(unique(v[un])) == 1) "is" else "are", " not one of ", paste(ch, collapse = ", "), ": ", imp_eg(v[un], 4), ". Kept as typed; the app reads them as blank unless you add them to the choices."), m, col, sum(un)) }
    } else if (ty == "text" || ty == "list") {
      v <- sub("^'", "", v); set(x, j = i, value = v)
      if (meta_true(spec$identifies[j])) {                                          # a key: what Excel does to long numbers
        sci <- grepl("^\\d(\\.\\d+)?[Ee][+]?\\d+$", v); if (any(sci)) add("error", imp_s(imp_plural(sum(sci), "value"), " in \"", col, "\" (", lab, ") ", if (sum(sci) == 1) "is" else "are", " in scientific notation, e.g. ", imp_eg(v[sci]), ": Excel has already cut the digits off. Format the column as Text in the source, type the numbers again, and re-save."), m, col, sum(sci))
        z <- has & grepl("^\\d+\\.0+$", v); if (any(z) && all(grepl("^\\d+(\\.0+)?$", v[has]))) { v <- sub("\\.0+$", "", v); set(x, j = i, value = v); add("note", imp_s("\"", col, "\" held whole numbers written as 1234.0; the .0 was removed."), m, col) } }
    }
  }
  map2 <- map[nzchar(map)]; dup <- unique(map2[duplicated(map2) & !startsWith(map2, "+")])
  for (f in dup) add("error", imp_s("Two columns are matched to ", spec$label[match(f, spec$field)], " (", paste0("\"", names(x)[map == f], "\"", collapse = " and "), "). Leave one out."), f, "")
  list(x = x, issues = rbindlist(issues, fill = TRUE) %||% data.table(severity = character(), field = character(), column = character(), n = integer(), message = character()), dates = used_dates)
}

# What the rows will do once they are in: needs the prepared table and its map. existing: the table's current rows (or NULL); mode add, update or replace.
# Adds to the issues of imp_prepare().
imp_check <- function(prep, map, spec, existing = NULL, mode = "add") {
  iss <- list(); x <- prep$x; map <- as.character(map); map[is.na(map)] <- ""
  add <- function(sev, msg, field = "", n = NA_integer_) iss[[length(iss) + 1L]] <<- data.table(severity = sev, field = field, column = "", n = n, message = msg)
  mapped <- map[nzchar(map) & !startsWith(map, "+")]; news <- sum(startsWith(map, "+"))
  if (!length(mapped) && !news) add("error", "No column is matched to a field yet. Match at least one column, or leave the import.")
  keys <- meta_keys(spec); k1 <- if (length(keys)) keys[1] else NA_character_
  if (length(mapped) || news) {
    if (!is.na(k1) && !k1 %in% mapped) add(if (mode == "update") "error" else "warning", imp_s("No column is matched to ", spec$label[match(k1, spec$field)], ", which says what each row is for.", if (mode == "update") " Updating needs it, so choose the column or use Add instead." else " The rows are added, but left aside until it is filled in."), k1)
    if (!is.na(k1) && k1 %in% mapped) { i <- which(map == k1)[1]; kv <- x[[i]]
      blank <- sum(!nzchar(kv)); if (blank) add("warning", imp_s(imp_plural(blank, "row"), " (of ", nrow(x), ") ", if (blank == 1) "has" else "have", " no ", spec$label[match(k1, spec$field)], ". ", if (blank == 1) "It is" else "They are", " added but left aside by the app until filled in."), k1, blank)
      kk <- imp_norm(kv)
      km <- keys[keys %in% mapped]; both <- do.call(paste, c(lapply(km, function(f) imp_norm(x[[which(map == f)[1]]])), sep = "\r"))     # the rows' whole key (serial and year, say)
      dk <- nzchar(kk) & (duplicated(both) | duplicated(both, fromLast = TRUE)); lab <- paste(spec$label[match(km, spec$field)], collapse = " and ")
      if (any(dk)) add("warning", imp_s(imp_plural(length(unique(both[dk])), "value"), " of ", lab, " appear more than once in the file (", imp_eg(kv[dk], 3), "; ", sum(dk), " rows).",
                                        if (mode == "update") " Update uses the last row of each." else " They are added as separate rows."), k1, sum(dk))
      if (!is.null(existing) && nrow(existing) && k1 %in% names(existing) && mode %in% c("add", "update")) { have <- imp_norm(existing[[k1]]); hit <- sum(nzchar(kk) & kk %in% have)
        if (hit) add(if (mode == "add") "warning" else "note", imp_s(imp_plural(hit, "row"), if (hit == 1) " has" else " have", " a ", spec$label[match(k1, spec$field)], " already in the table. ", if (mode == "add") "They are added again as extra rows; use \"Update rows with the same key\" to change the existing ones instead." else "Those rows are updated; blank cells in the file do not erase what is there."), k1, hit) } }
  }
  if (mode == "replace") add("warning", "The table is replaced: its current rows are removed. (Undo brings them back.)")
  um <- sum(!nzchar(map)); if (um && ncol(x)) add("note", imp_s(imp_plural(um, "column"), " of the file ", if (um == 1) "is" else "are", " left out: ", paste0("\"", utils::head(names(x)[!nzchar(map)], 6), "\"", collapse = ", "), if (um > 6) ", ..." else "", "."))
  if (news) add("note", imp_s(imp_plural(news, "column"), " will be added to the table as ", if (news == 1) "a column" else "columns", " of your own (kept and exported, not used by the app)."))
  rbind(prep$issues, rbindlist(iss, fill = TRUE), fill = TRUE)
}

# ---- remembering how a layout was matched ------------------------------------------------------------------------------------
# The same spreadsheet layout comes back (the same headings every season); what was matched the last time is offered first.
# config/import_memory.csv in the project: target, heading, field.
imp_memory_path <- function(project_dir) file.path(path.expand(project_dir), "config", "import_memory.csv")
imp_memory_get <- function(project_dir, tg) { p <- imp_memory_path(project_dir)
  # safe: the remembered column matches are a convenience; without them the columns are matched by name
  d <- tryCatch(if (file.exists(p)) fread(file = p, colClasses = "character", na.strings = NULL) else NULL, error = function(e) NULL)
  if (is.null(d) || !all(c("target", "heading", "field") %in% names(d))) return(data.table(heading = character(), field = character()))
  d[target == tg, .(heading, field)] }
imp_memory_put <- function(project_dir, tg, headings, map) { p <- imp_memory_path(project_dir); if (!length(headings)) return(invisible(FALSE))
  k <- nzchar(map) & !startsWith(map, "+"); if (!any(k)) return(invisible(FALSE))
  new <- data.table(target = tg, heading = headings[k], field = map[k])
  # safe: a damaged memory file is replaced by a new one
  old <- tryCatch(if (file.exists(p)) fread(file = p, colClasses = "character", na.strings = NULL) else NULL, error = function(e) NULL)
  keep <- if (!is.null(old) && all(c("target", "heading", "field") %in% names(old))) old[!(target == tg & imp_norm(heading) %in% imp_norm(new$heading)), .(target, heading, field)] else NULL
  tryCatch({ dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); fwrite(rbind(keep, new), p); invisible(TRUE) }, error = function(e) invisible(FALSE)) }

# ---- fields for tables that declare none ---------------------------------------------------------------------------------
# A set-up table with no IMPORT_SPECS entry (core deployments.R has one for the deployment sheet) still gets its dates, numbers
# and choices put in order on import: the type of each column is judged from its name (start, end, date ... are dates; depth,
# offset, latitude ... are numbers; a column whose rules list choices is a choice) and the table's `rules` (its required column
# is the key). A wrong guess costs little: a value that does not fit is kept as typed and listed, never dropped.
IMPORT_ALIASES <- list(
  serial = "sn|serial no|serial number|s/n|logger serial|logger sn|logger id|instrument serial", site = "site name|area|study area|field site",
  station = "station name|location|location name|well|borehole|hole|plot|logger name", latitude = "lat|latitude dd|decimal latitude|y coordinate",
  longitude = "lon|long|lng|longitude dd|decimal longitude|x coordinate", elevation = "elev|elevation m|altitude|height above sea level",
  start = "start date|begin|first record|installed|install date|from", end = "end date|until|last record|removed|to", from = "start|start date|from date|installed|install date",
  to = "end|end date|to date|until|removed", notes = "note|remarks|comments|comment|description", reason = "why|comment|remarks", depth_m = "depth|depth (m)|sensor depth")
import_spec_guess <- function(cols, entry = NULL) {
  cols <- as.character(cols); rl <- if (is.null(entry)) list() else (entry$rules %||% list()); ch <- rl$choices %||% list()
  ty <- function(f) { fl <- tolower(f)
    if (!is.null(ch[[f]])) "choice"
    else if (grepl("(^|_)(start|end|from|to|date|time|timestamp|datetime|installed|removed|first|last)(_|$)", fl)) "date"
    else if (grepl("(^|_)(lat|latitude|lon|longitude|elevation|elev|depth|height|offset|scale|factor|hours|threshold|slope|distance|rate|gain)(_|$)|_(m|cm|mm|c|pct|h)$", fl)) "number" else "text" }
  req <- as.character(rl$required %||% character()); key <- if (length(req) && req[1] %in% cols) req[1] else NA_character_
  data.table(field = cols, label = ifelse(nzchar(cols), paste0(toupper(substr(gsub("_", " ", cols), 1, 1)), substring(gsub("_", " ", cols), 2)), cols), type = vapply(cols, ty, ""),
             choices = vapply(cols, function(f) if (!is.null(ch[[f]])) paste(ch[[f]], collapse = "|") else "", ""), unit = "", use = "", help = "",
             aliases = vapply(cols, function(f) IMPORT_ALIASES[[f]] %||% "", ""), show = "TRUE", identifies = ifelse(cols %in% key, "TRUE", "FALSE"))
}
