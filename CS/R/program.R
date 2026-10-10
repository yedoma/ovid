# R/program.R  --  Campbell logger program files: what each output table should hold, and which .dat files came from them.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.
#
#   CRBasic (CR1000 .CR1, CR1000X .CR1X, CR3000 .CR3, also .CR6/.CR300): tables, field names, units, processing,
#     output and scan intervals, and the instruction that measured each variable.
#   Edlog (CR10X/CR23X .CSI and .DLD): output arrays, field labels as Edlog writes them in the .FSL, processing
#     and output interval. These also stand in for an .FSL on the CR10X files page.
# Program files are kept in <project>/config/programs. Nothing in a raw file is changed.

PROG_EXT <- "\\.(cr1|cr1x|cr3|cr6|cr300|cr350|cr8|cr2|csi|dld)$"
SYSTEM_TABLES <- c("Status", "DataTableInfo", "Public", "Inlocs", "ConstTable", "CPIStatus", "TableInfo")

# Campbell's 16-bit signature (seed 0xAAAA), the number in the 7th field of a TOA5 header.
cs_signature <- function(bytes) {
  s <- 0xAAAAL
  for (b in as.integer(bytes)) {
    j <- s; s <- bitwAnd(bitwShiftL(s, 1L), 0x1FFL); if (s >= 0x100L) s <- s + 1L
    s <- bitwOr(bitwAnd(s + bitwShiftR(j, 8L) + b, 0xFFL), bitwAnd(bitwShiftL(j, 8L), 0xFFFFL))
  }
  s
}

prog_lines <- function(path) fix_enc(suppressWarnings(readLines(path, warn = FALSE, skipNul = TRUE, encoding = "latin1")))
prog_kind <- function(path) if (grepl("\\.(csi|dld)$", path, ignore.case = TRUE)) "Edlog" else "CRBasic"

# ---- CRBasic ------------------------------------------------------------------
cb_strip <- function(s) {                       # drop a ' comment that is not inside quotes
  if (!grepl("'", s, fixed = TRUE)) return(s)
  ch <- strsplit(s, "", fixed = TRUE)[[1]]; q <- FALSE
  for (i in seq_along(ch)) if (ch[i] == "\"") q <- !q else if (ch[i] == "'" && !q) return(substr(s, 1L, i - 1L))
  s
}
cb_args <- function(s) {                        # split on commas outside brackets and quotes
  ch <- strsplit(s, "", fixed = TRUE)[[1]]; d <- 0L; q <- FALSE; out <- character(); cur <- character()
  for (c in ch) {
    if (c == "\"") q <- !q
    if (!q && c == "(") d <- d + 1L else if (!q && c == ")") d <- d - 1L
    if (!q && d == 0L && c == ",") { out <- c(out, paste(cur, collapse = "")); cur <- character() } else cur <- c(cur, c)
  }
  trim(c(out, paste(cur, collapse = "")))
}
cb_call <- function(s, name) {                  # arguments of Name(...) at the start of a line
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) return(NULL)
  m <- regexpr(sprintf("^\\s*%s\\s*\\(", name), s, ignore.case = TRUE, perl = TRUE)
  if (m < 0) return(NULL)
  body <- sub("\\)\\s*$", "", substring(s, attr(m, "match.length") + m))
  cb_args(body)
}
cb_num <- function(x, consts) {                 # a number, a Const, or simple arithmetic of those
  x <- trim(gsub("\"", "", x)); if (!nzchar(x)) return(NA_real_)
  if (grepl("^-?[0-9.]+$", x)) return(as.numeric(x))
  ids <- regmatches(x, gregexpr("[A-Za-z_][A-Za-z0-9_]*", x))[[1]]
  if (!grepl("^[A-Za-z0-9_.+*/() -]+$", x) || !all(tolower(ids) %in% names(consts))) return(NA_real_)
  x <- gsub("([A-Za-z_][A-Za-z0-9_]*)", "\\L\\1", x, perl = TRUE)
  suppressWarnings(tryCatch(as.numeric(eval(parse(text = x), envir = list2env(consts, parent = baseenv()))), error = function(e) NA_real_))
}
cb_truthy <- function(x) { x <- tolower(trim(x)); nzchar(x) && !x %in% c("false", "0") }
cb_interval <- function(n, u) {
  u <- tolower(trim(u)); n <- suppressWarnings(as.numeric(n))
  mult <- c(usec = 1e-6, msec = 1e-3, sec = 1, min = 60, hr = 3600, day = 86400)[u]
  list(text = if (is.na(n)) "" else paste(format(n), c(usec = "us", msec = "ms", sec = "s", min = "min", hr = "h", day = "d")[u] %||% u),
       seconds = if (is.na(n) || is.na(mult)) NA_real_ else n * unname(mult))
}

# fields written by one output instruction: name, index, processing and units
cb_fields <- function(src, reps, vars, alias, units, suffix, proc) {
  nm <- trim(sub("\\(.*$", "", src)); idx <- if (grepl("\\(", src)) sub("^[^(]*\\((.*)\\)\\s*$", "\\1", src) else ""
  v <- vars[[tolower(nm)]]
  if (is.null(v) && !nzchar(idx) && length(alias)) {                   # source is an alias: use the element it names
    rk <- names(alias)[tolower(unlist(alias)) == tolower(nm)]
    if (length(rk) && grepl("\\(", rk[1])) return(cb_fields(rk[1], reps, vars, alias, units, suffix, proc))
  }
  base <- v$name %||% nm; dims <- v$dims
  reps <- if (is.na(reps) || reps < 1) 1L else as.integer(reps)
  if (!length(dims)) { ix <- rep("", reps); key <- rep(tolower(base), reps) } else {
    st <- vapply(strsplit(idx, ",", fixed = TRUE)[[1]], function(z) cb_num(z, v$consts), 1)
    st <- c(st, rep(1, length(dims)))[seq_along(dims)]; st[is.na(st)] <- 1
    pos0 <- sum((st - 1) * rev(cumprod(rev(c(dims[-1], 1)))))           # row-major position of the first element
    ix <- vapply(pos0 + seq_len(reps) - 1, function(p) { k <- integer(length(dims)); r <- p
      for (d in rev(seq_along(dims))) { k[d] <- r %% dims[d] + 1; r <- r %/% dims[d] }
      paste0("(", paste(k, collapse = ","), ")") }, "")
    key <- paste0(tolower(base), ix)
  }
  out <- list()
  for (r in seq_len(reps)) {
    a <- alias[[key[r]]]; nmr <- if (!is.null(a)) a else base; ixr <- if (!is.null(a)) "" else ix[r]
    un <- units[[tolower(nmr)]] %||% units[[tolower(base)]] %||% ""
    for (k in seq_along(suffix)) out[[length(out) + 1L]] <- data.table(column = paste0(nmr, suffix[k], ixr), source = paste0(base, ix[r]),
      process = proc[k], units = if (proc[k] %in% c("TMx", "TMn")) "" else un)
  }
  rbindlist(out)
}

parse_crbasic <- function(path) {
  raw <- prog_lines(path); l <- trim(vapply(raw, cb_strip, "", USE.NAMES = FALSE))
  consts <- list(); vars <- list(); alias <- list(); units <- list(); measured <- list(); pub <- character(); scan <- list(text = "", seconds = NA_real_)
  skip_ins <- "^(If|ElseIf|Else|EndIf|For|Next|Do|Loop|While|Wend|Select|Case|Call|CallTable|Scan|NextScan|Delay|Exit|Return|Sub|EndSub|Function|EndFunction|Const|Public|Dim|Alias|Units|DataTable|EndTable|BeginProg|EndProg|SequentialMode|PipeLineMode|SlowSequence|EndSequence|Erase|Move|Average|Sample|Maximum|Minimum|Totalize|StdDev|Median|Moment|WindVector|DataInterval|CardOut|TableFile|FieldNames|Include)$"
  for (s in l[grepl("^Const\\s", l, ignore.case = TRUE)]) {
    m <- regmatches(s, regexec("^Const\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*(.+)$", s, ignore.case = TRUE))[[1]]
    if (length(m)) consts[[tolower(m[2])]] <- cb_num(m[3], consts)
  }
  for (s in l) {
    kw <- tolower(regmatches(s, regexpr("^[A-Za-z_][A-Za-z0-9_]*", s))); if (!length(kw)) kw <- ""   # a line that does not start with a name has no keyword
    if (kw %in% c("public", "dim", "static")) {
      for (it in cb_args(sub("^[A-Za-z]+\\s+", "", s))) {
        it <- trim(sub("\\s+As\\s+.*$", "", it, ignore.case = TRUE)); nm <- trim(sub("\\(.*$", "", it))
        if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm)) next
        d <- if (grepl("\\(", it)) vapply(cb_args(sub("^[^(]*\\((.*)\\)\\s*$", "\\1", it)), cb_num, 1, consts = consts) else numeric()
        vars[[tolower(nm)]] <- list(name = nm, dims = d[!is.na(d)], consts = consts)
        if (kw == "public") pub <- c(pub, nm)
      }
    } else if (kw == "alias") {
      m <- regmatches(s, regexec("^Alias\\s+(.+?)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)", s, ignore.case = TRUE))[[1]]
      if (length(m)) { k <- tolower(gsub("\\s", "", m[2])); if (grepl("\\(", k)) {
          n0 <- sub("\\(.*$", "", k); ii <- vapply(cb_args(sub("^[^(]*\\((.*)\\)$", "\\1", k)), cb_num, 1, consts = consts)
          k <- paste0(n0, "(", paste(ii, collapse = ","), ")") }
        alias[[k]] <- m[3] }
    } else if (kw == "units") {
      m <- regmatches(s, regexec("^Units\\s+([A-Za-z_][A-Za-z0-9_()]*)\\s*=\\s*(.*)$", s, ignore.case = TRUE))[[1]]
      if (length(m)) units[[tolower(sub("\\(.*$", "", m[2]))]] <- trim(gsub("\"", "", m[3]))
    } else if (kw == "scan") {
      a <- cb_call(s, "Scan"); if (length(a) >= 2 && is.na(scan$seconds)) scan <- cb_interval(cb_num(a[1], consts), a[2])
    } else if (nzchar(kw) && !grepl(skip_ins, kw, ignore.case = TRUE)) {
      a <- cb_call(s, kw)                       # a measurement: the first argument is where the reading goes
      if (length(a)) { dest <- tolower(sub("\\(.*$", "", a[1])); if (!is.null(vars[[dest]]) && is.null(measured[[dest]])) measured[[dest]] <- sub("^([A-Za-z0-9_]+).*$", "\\1", s) }
    }
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*(\\([^)]*\\))?\\s*=[^=]", s) && !kw %in% c("const", "alias", "units", "public", "dim", "static")) {
      dest <- tolower(sub("^([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", s)); if (!is.null(vars[[dest]]) && is.null(measured[[dest]])) measured[[dest]] <- "calculated"
    }
  }
  tabs <- list(); cur <- NULL; pend <- NULL
  flush_pend <- function() if (!is.null(pend)) { cur$fields[[length(cur$fields) + 1L]] <<- pend; pend <<- NULL }
  ins <- list(Average = list("_Avg", "Avg"), Sample = list("", "Smp"), Totalize = list("_Tot", "Tot"), StdDev = list("_Std", "Std"),
              Median = list("_Med", "Med"), Moment = list("_Mom", "Mom"), Maximum = list("_Max", "Max"), Minimum = list("_Min", "Min"))
  for (s in l) {
    if (is.null(cur)) {
      a <- cb_call(s, "DataTable")
      if (!is.null(a)) cur <- list(name = gsub("\"", "", a[1]), fields = list(), interval = list(text = "on trigger", seconds = NA_real_), skipped = character())
      next
    }
    if (grepl("^EndTable", s, ignore.case = TRUE)) { flush_pend(); tabs[[cur$name]] <- cur; cur <- NULL; next }
    a <- cb_call(s, "DataInterval"); if (!is.null(a)) { cur$interval <- cb_interval(cb_num(a[2], consts), a[3]); next }
    a <- cb_call(s, "FieldNames")
    if (!is.null(a) && !is.null(pend)) {
      nn <- trim(sub(":.*$", "", strsplit(gsub("\"", "", a[1]), ",", fixed = TRUE)[[1]]))
      k <- seq_len(min(length(nn), nrow(pend))); pend[k, column := nn[k]]; next
    }
    kw <- sub("^([A-Za-z]+).*$", "\\1", s); hit <- names(ins)[tolower(names(ins)) == tolower(kw)]
    if (length(hit)) {
      flush_pend(); a <- cb_call(s, hit); if (length(a) < 2) next
      sf <- ins[[hit]][[1]]; pr <- ins[[hit]][[2]]
      if (hit %in% c("Maximum", "Minimum") && length(a) >= 5 && cb_truthy(a[5])) { sf <- c(sf, if (hit == "Maximum") "_TMx" else "_TMn"); pr <- c(pr, if (hit == "Maximum") "TMx" else "TMn") }
      pend <- cb_fields(a[2], cb_num(a[1], consts), vars, alias, units, sf, pr)
    } else if (tolower(kw) == "windvector") {
      flush_pend(); a <- cb_call(s, "WindVector"); if (length(a) < 3) next
      opt <- if (length(a) >= 8) cb_num(a[8], consts) else 0
      spec <- switch(as.character(opt), "1" = list(c(2, 3), c("_S_WVT", "_D1_WVT")), "2" = list(c(2, 2, 3, 3), c("_S_WVT", "_U_WVT", "_DU_WVT", "_SDU_WVT")),
                     "3" = list(c(2, 3), c("_U_WVT", "_DU_WVT")), "4" = list(c(2, 3), c("_S_WVT", "_DU_WVT")),
                     list(c(2, 3, 3), c("_S_WVT", "_D1_WVT", "_SD1_WVT")))
      pend <- rbindlist(lapply(seq_along(spec[[1]]), function(k) cb_fields(a[spec[[1]][k]], 1, vars, alias, units, spec[[2]][k], "WVc")))
    } else if (grepl("^(Histogram|Histogram4D|FFT|LevelCrossing|RainFlow|ETsz|ETo|Covariance|CovSpa|SampleFieldCal|SampleMaxMin|TableHide|ComPortIsActive)$", kw, ignore.case = TRUE)) {
      flush_pend(); if (!grepl("^TableHide$", kw, ignore.case = TRUE)) cur$skipped <- c(cur$skipped, kw)
    }
  }
  cols <- rbindlist(lapply(tabs, function(t) {
    f <- rbindlist(t$fields); if (!nrow(f)) return(NULL)
    f[, `:=`(table = t$name, interval = t$interval$text, interval_seconds = t$interval$seconds,
             note = if (length(t$skipped)) paste("not expanded:", paste(unique(t$skipped), collapse = ", ")) else "")]
  }), fill = TRUE)
  if (length(pub)) cols <- rbind(cols, rbindlist(lapply(pub, function(p) { v <- vars[[tolower(p)]]
    cb_fields(paste0(p, if (length(v$dims)) "(1)" else ""), if (length(v$dims)) prod(v$dims) else 1, vars, alias, units, "", "Smp") }))[
      , `:=`(table = "Public", interval = "current values", interval_seconds = NA_real_, note = "")], fill = TRUE)
  if (!nrow(cols)) return(cols)
  cols[, measured_by := vapply(tolower(sub("\\(.*$", "", source)), function(v) measured[[v]] %||% "", "")]
  cols[, `:=`(position = seq_len(.N)), by = table]
  cols[, `:=`(scan = scan$text, scan_seconds = scan$seconds)][]
}

# ---- Edlog (.CSI source, .DLD download) -------------------------------------
# Labels follow what Edlog writes in the .FSL: label_AVG, label_TOT, label_MAX, label (sample), Year_RTM ...
EDLOG_OUT <- list(`70` = c("", "Smp"), `71` = c("_AVG", "Avg"), `72` = c("_TOT", "Tot"), `73` = c("_MAX", "Max"),
                  `74` = c("_MIN", "Min"), `82` = c("_STD", "Std"))

parse_edlog <- function(path) {
  l <- prog_lines(path); n <- length(l)
  stop_at <- grep("^\\s*-Input Locations-", l, ignore.case = TRUE)[1]
  locs <- character(); inst_desc <- character(); measured <- character()
  if (!is.na(stop_at)) for (s in l[(stop_at + 1L):n]) {
    if (grepl("^\\s*-.*-\\s*$", s)) break
    m <- regmatches(s, regexec("^\\s*([0-9]+)\\s+(\\S+)", s))[[1]]; if (length(m)) locs[m[2]] <- m[3]
  }
  body <- l[seq_len(if (is.na(stop_at)) n else stop_at - 1L)]
  insts <- list(); cur <- NULL; tab <- 0L; scan <- NA_real_
  push <- function() { if (!is.null(cur)) insts[[length(insts) + 1L]] <<- cur; cur <<- NULL }
  for (s in body) {
    if (grepl("^\\s*\\*\\s*Table\\s+[0-9]", s, ignore.case = TRUE)) { tab <- as.integer(sub("^\\s*\\*\\s*Table\\s+([0-9]+).*$", "\\1", s, ignore.case = TRUE)); push(); next }
    if (grepl("^\\s*MODE\\s+[0-9]+", s, ignore.case = TRUE)) { tab <- as.integer(sub("^\\s*MODE\\s+([0-9]+).*$", "\\1", s, ignore.case = TRUE)); push(); next }
    if (grepl("^\\s*SCAN RATE", s, ignore.case = TRUE) && tab == 1L) { scan <- suppressWarnings(as.numeric(sub("^\\s*SCAN RATE\\s+", "", s, ignore.case = TRUE))); next }
    if (tab > 3L) next
    m <- regmatches(s, regexec("^\\s*([0-9]+):\\s*(?:(.*?)\\s*\\()?P([0-9]+)\\)?\\s*$", s, perl = TRUE))[[1]]
    if (length(m)) { push()
      cur <- list(step = as.integer(m[2]), p = m[4], desc = trim(m[3]), par = numeric(), lab = character()); next }
    m <- regmatches(s, regexec("^\\s*([0-9]+):\\s*(-?[0-9.]+)\\s*(.*)$", s))[[1]]
    if (length(m)) {
      if (is.null(cur)) { if (tab == 1L && grepl("Execution Interval", m[4], ignore.case = TRUE)) scan <- as.numeric(m[3]); next }
      cur$par[as.integer(m[2])] <- as.numeric(m[3])
      lb <- regmatches(m[4], regexec("Loc\\s*\\[\\s*([^]]*?)\\s*\\]", m[4]))[[1]]; if (length(lb)) cur$lab[as.integer(m[2])] <- lb[2]
    }
  }
  push()
  lab_of <- function(loc) { x <- locs[as.character(loc)]; ifelse(is.na(x) | !nzchar(x), paste0("Loc", loc), x) }
  arrays <- list(); meta <- list(); id <- NULL; fs <- TRUE; ivl <- ""; ivs <- NA_real_
  add <- function(labs, proc, src = rep("", length(labs)), by = "") {
    if (is.null(id) || !fs) return()
    if (is.null(arrays[[id]])) arrays[[id]] <<- id
    arrays[[id]] <<- c(arrays[[id]], labs)
    meta[[length(meta) + 1L]] <<- data.table(table = paste0("A", id), column = labs, process = proc, source = src,
                                             measured_by = by, interval = ivl, interval_seconds = ivs)
  }
  for (it in insts) {
    p <- it$p; a <- it$par; a[is.na(a)] <- 0
    if (p %in% c("92", "86", "88", "89", "91") && 10 %in% a) {            # output flag set: a new output interval starts
      if (p == "92") { v <- a[2]; ivs <- if (a[1] < 0 || v < 0) abs(v) else v * 60; ivl <- if (ivs %% 60 == 0) paste(ivs / 60, "min") else paste(ivs, "s") }
      id <- as.character(it$step); fs <- TRUE; next }
    if (p == "80") { fs <- a[1] %in% c(1, 2); if (a[2] > 0) id <- as.character(a[2]); next }
    if (p == "77") { d <- strsplit(sprintf("%04d", as.integer(a[1])), "")[[1]]
      add(c("Year_RTM", "Day_RTM", "Hour_Minute_RTM", "Seconds_RTM")[d != "0"], "RTM"); next }
    if (p %in% names(EDLOG_OUT)) {
      reps <- max(1, a[1]); tm <- if (p %in% c("73", "74")) a[2] else 0; loc <- if (p %in% c("73", "74")) a[3] else a[2]
      sf <- EDLOG_OUT[[p]]; ll <- lab_of(loc + seq_len(reps) - 1); labs <- character(); proc <- character()
      for (x in ll) { labs <- c(labs, paste0(x, sf[1])); proc <- c(proc, sf[2])
        if (tm %/% 10 > 0) { labs <- c(labs, paste0(x, "_Hr_Min", sf[1])); proc <- c(proc, paste0("T", sf[2])) }
        if (tm %% 10 > 0)  { labs <- c(labs, paste0(x, "_Sec", sf[1]));    proc <- c(proc, paste0("T", sf[2])) } }
      src <- rep(ll, each = length(labs) / length(ll))
      add(labs, proc, src, unname(measured[src] %|na|% "")); next }
    if (p == "69") {                                                       # wind vector: speed then direction location
      ws <- lab_of(a[4]); wd <- lab_of(a[4] + 1)
      sp <- switch(as.character(a[3]), "1" = c(ws, "_S_WVT", wd, "_D1_WVT"), "2" = c(ws, "_S_WVT", ws, "_U_WVT", wd, "_DU_WVT", wd, "_SDU_WVT"),
                   c(ws, "_S_WVT", wd, "_D1_WVT", wd, "_SD1_WVT"))
      k <- seq(1, length(sp), 2); add(paste0(sp[k], sp[k + 1]), "WVc", sp[k]); next }
    # any other instruction writing to locations: remember what measured them
    for (j in which(nzchar(it$lab %|na|% ""))) { lb <- it$lab[j]; if (is.na(measured[lb])) measured[lb] <- trim(paste0(it$desc, " P", p)) }
  }
  meta <- rbindlist(meta)
  if (nrow(meta)) meta[, position := seq_len(.N) + 1L, by = table][, `:=`(units = "", note = "", scan = if (is.na(scan)) "" else paste(scan, "s"), scan_seconds = scan)]
  list(arrays = arrays, columns = meta)
}
`%|na|%` <- function(a, b) { if (!length(a)) return(a); a[is.na(a)] <- b; a }

# ---- one program file -> what it says ---------------------------------------
read_program <- function(path) {
  kind <- prog_kind(path)
  cols <- tryCatch(if (kind == "Edlog") parse_edlog(path)$columns else parse_crbasic(path), error = function(e) {
    problem(basename(path), paste("program could not be read:", conditionMessage(e))); data.table() })
  list(name = basename(path), path = path, kind = kind, md5 = unname(tools::md5sum(path)), mtime = file.mtime(path),
       signature = cs_signature(readBin(path, "raw", file.size(path))), columns = cols)
}

program_dir <- function(cfg) file.path(path.expand(cfg$project_dir), "config", "programs")

# every program in config/programs (byte-identical copies read once); parsed results kept by md5
load_programs <- function(cfg) {
  f <- list.files(program_dir(cfg), pattern = PROG_EXT, full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  f <- f[!startsWith(basename(f), "._")]
  if (!length(f)) return(list())
  md5 <- unname(tools::md5sum(f)); f <- f[!duplicated(md5)]; md5 <- md5[!duplicated(md5)]
  cf <- file.path(path.expand(cfg$project_dir), "cache", "programs.rds")
  old <- if (file.exists(cf)) tryCatch(readRDS(cf), error = function(e) list()) else list()
  key <- paste(md5, STEP_HASH$programs %||% "")
  out <- lapply(seq_along(f), function(i) { p <- old[[key[i]]]; if (is.null(p)) p <- read_program(f[i]); p$path <- f[i]; p$name <- basename(f[i]); p })
  dir.create(dirname(cf), recursive = TRUE, showWarnings = FALSE)
  try(saveRDS_safe(setNames(out, key), cf), silent = TRUE)
  out
}

# copy program files from a folder (or uploads) into config/programs; Edlog ones also go to config/fsl for the CR10X page
add_programs <- function(cfg, paths, names = basename(paths)) {
  keep <- grepl(PROG_EXT, names, ignore.case = TRUE) & !startsWith(names, "._"); paths <- paths[keep]; names <- names[keep]
  if (!length(paths)) return(character())
  md5 <- unname(tools::md5sum(paths)); paths <- paths[!duplicated(md5)]; names <- names[!duplicated(md5)]
  pd <- program_dir(cfg); dir.create(pd, recursive = TRUE, showWarnings = FALSE)
  have <- list.files(pd, full.names = TRUE); have_md5 <- if (length(have)) unname(tools::md5sum(have)) else character()
  new <- !unname(tools::md5sum(paths)) %in% have_md5
  if (!any(new)) return(character())
  dest <- file.path(pd, names[new])
  clash <- file.exists(dest)                                    # same name, different content: keep both
  dest[clash] <- file.path(pd, paste0(tools::file_path_sans_ext(names[new][clash]), "__", substr(unname(tools::md5sum(paths[new][clash])), 1, 6), ".", tools::file_ext(names[new][clash])))
  file.copy(paths[new], dest)
  ed <- grepl("\\.(csi|dld)$", dest, ignore.case = TRUE)
  if (any(ed)) { fd <- file.path(path.expand(cfg$project_dir), "config", "fsl"); dir.create(fd, recursive = TRUE, showWarnings = FALSE); file.copy(dest[ed], fd, overwrite = FALSE) }
  basename(dest)
}

# ---- which program wrote each file -------------------------------------------
# A program fits a file when it defines the file's table with the same columns. The program name and signature in
# the TOA5 header pick between versions; a renamed program is still found by its columns.
match_programs <- function(man, progs) {
  ok <- man[status == "ok"]
  empty <- data.table(path = character(), table = character(), status = character())
  if (!nrow(ok)) return(empty)
  pn <- tolower(vapply(progs, `[[`, "", "name")); res <- vector("list", nrow(ok))
  for (i in seq_len(nrow(ok))) {
    hp <- trim(sub("^[A-Za-z]{3}:", "", ok$program[i] %||% "")); tb <- ok$table[i]
    sig <- suppressWarnings(as.integer(if ("signature" %in% names(ok)) ok$signature[i] else NA))
    base <- data.table(path = ok$path[i], logger_id = ok$logger_id[i], table = tb, program_in_header = hp, signature_in_header = sig)
    if (identical(ok$format[i], "mixed-array")) { res[[i]] <- base[, `:=`(status = "see CR10X files", program = hp, how = "array id and field count")]; next }
    fc <- setdiff(ok$columns[[i]], c("TIMESTAMP", "RECORD"))
    score <- lapply(seq_along(progs), function(k) {
      pc <- progs[[k]]$columns; if (!nrow(pc)) return(NULL); pc <- pc$column[pc$table == tb]; if (!length(pc)) return(NULL)
      data.table(k = k, name_ok = pn[k] == tolower(basename(hp)), sig_ok = !is.na(sig) && progs[[k]]$signature == sig,
                 same = setequal(fc, pc), found = length(intersect(fc, pc)) / length(union(fc, pc)), n = length(pc), extra = sum(!pc %in% fc))
    })
    s <- rbindlist(score)
    named <- any(pn == tolower(basename(hp)))
    if (!nrow(s) || !any(s$name_ok | s$found >= 0.9 & s$n >= 2)) {
      res[[i]] <- base[, `:=`(status = if (tb %in% SYSTEM_TABLES) "system table" else if (named) "program lacks this table" else "no program",
                              program = if (named) progs[[which(pn == tolower(basename(hp)))[1]]]$name else "", how = "")]; next }
    s <- s[name_ok | sig_ok | n >= 2][order(-same, -sig_ok, -name_ok, -found, extra, -as.numeric(vapply(k, function(z) progs[[z]]$mtime, 0)))]
    b <- s[1]; p <- progs[[b$k]]
    how <- paste(c(if (b$name_ok) "name", if (b$sig_ok) "signature", if (b$same) "columns" else sprintf("%.0f%% of columns", 100 * b$found)), collapse = " + ")
    st <- if (b$same && (b$name_ok || b$sig_ok)) "matched" else if (b$same) "matched by columns (renamed program?)" else if (b$found >= 0.9) "columns differ slightly" else "columns differ"
    pc <- p$columns[table == tb]
    res[[i]] <- base[, `:=`(status = st, program = p$name, how = how, kind = p$kind, program_signature = p$signature,
                            interval = pc$interval[1], scan = pc$scan[1],
                            missing_in_program = paste(head(setdiff(fc, pc$column), 8), collapse = ", "),
                            missing_in_file = paste(head(setdiff(pc$column, fc), 8), collapse = ", "),
                            other_versions = sum(s$same) - 1L)]
  }
  rbindlist(res, fill = TRUE)
}

# Applied in a run: blank units in the file header are filled from the program; the column dictionary gains
# where each column came from. Returns the files (units filled), the matches and the per-column program details.
apply_programs <- function(ok, cfg) {
  progs <- load_programs(cfg)
  if (!length(progs)) return(list(ok = ok, matches = NULL, columns = NULL))
  m <- match_programs(ok, progs)
  use <- m[status %in% c("matched", "matched by columns (renamed program?)", "columns differ slightly")]
  pcols <- list(); filled <- 0L
  byname <- setNames(progs, vapply(progs, `[[`, "", "name"))
  for (j in seq_len(nrow(use))) {
    i <- match(use$path[j], ok$path); pc <- byname[[use$program[j]]]$columns[table == use$table[j]]
    u <- ok$units[[i]]; cl <- ok$columns[[i]]; u <- c(u, rep("", length(cl)))[seq_along(cl)]; u[is.na(u)] <- ""
    w <- which(!nzchar(u) & cl %in% pc$column); if (length(w)) { u[w] <- pc$units[match(cl[w], pc$column)]; filled <- filled + sum(nzchar(u[w])); set(ok, i, "units", list(list(u))) }
    pcols[[length(pcols) + 1L]] <- pc[, .(logger_id = ok$logger_id[i], table, raw_column = column, program = use$program[j], measured_by, source, program_interval = interval)]
  }
  pcols <- unique(rbindlist(pcols, fill = TRUE), by = c("logger_id", "table", "raw_column"))
  if (filled) say("  programs: units filled for ", filled, " column(s) from program files", col = "fg4")
  list(ok = ok, matches = m, columns = pcols)
}

# For the Logger programs page: headers of every .dat file in the raw folders (4 lines each, no data read),
# matched to the programs in config/programs. Written to config/program_matches.csv.
program_report <- function(cfg) {
  cfg$project_dir <- path.expand(cfg$project_dir)
  dirs <- path.expand(trim(cfg$raw_dirs)); dirs <- dirs[dir.exists(dirs)]
  files <- unlist(lapply(dirs, list.files, pattern = "\\.dat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
  files <- unique(files[!startsWith(basename(files), "._")])
  if (!length(files)) stop("No .dat files found in the raw folders.")
  progs <- load_programs(cfg)
  if (!length(progs)) stop("No program files yet. Add some first.")
  man <- rbindlist(lapply(files, function(f) tryCatch(as.data.table(read_header(f)), error = function(e) NULL)), fill = TRUE)
  cxf <- file.path(cfg$project_dir, "config", "cr10x.csv"); cx <- if (file.exists(cxf)) read_cfg(cxf) else data.table(file = character(), fsl = character())
  arr <- man$status == "unsupported_mixed_array"
  man[arr, `:=`(status = "ok", format = "mixed-array", table = "ARRAY", program = cx$fsl[match(path[arr], cx$file)] %|na|% "")]
  ok <- man$status == "ok" & man$format %in% "TOA5"
  man[, logger_id := NA_character_]
  if (any(ok)) man[ok, logger_id := mapply(function(s, n) resolve_logger_id(s, n)[1], serial %|na|% "", station_name %|na|% "")]
  m <- match_programs(man, progs)
  if (nrow(m)) { m[, file := basename(path)]; setcolorder(m, c("file", "table", "status", "program")) }
  fwrite(m, file.path(cfg$project_dir, "config", "program_matches.csv"))
  m
}
