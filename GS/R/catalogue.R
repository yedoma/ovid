# R/catalogue.R  --  Finding files, grouping them into surveys, linking copies and derived sets, and writing the catalogue.
# Part of Ovid Core // GS. Sourced by gs_pipeline.R; not meant to be run on its own.
#
# Every file found is listed in catalogue/files.csv, readable or not. A survey is one instrument data file; its id comes
# from the file's MD5, so renaming or moving the file keeps the id and byte-identical copies are one survey. Companions
# (.crs, .cmd, .trn, memory dumps) join the survey with the same folder and file stem; notes and photos join by folder.

survey_id_of <- function(md5) paste0("ERT-", substr(md5, 1, 10))

file_stem <- function(f) sub("\\.(stg|cmd|crs|trn)(\\.bin|\\.txt)?$|\\.[A-Za-z0-9]+$", "", f, ignore.case = TRUE)

# Zip files are opened, never changed: members are extracted once into <project>/cache/zip/<zip md5>/ and listed as
# "<zip>!/<member>", in a folder of their own, so a SuperSting download kept as a zip is read like a loose one.
expand_zips <- function(out, cfg) {
  z <- out[grepl("\\.zip$", file, ignore.case = TRUE)]
  rbindlist(lapply(seq_len(nrow(z)), function(i) {
    l <- tryCatch(utils::unzip(z$path[i], list = TRUE), error = function(e) { problem(z$rel[i], paste("zip not opened:", conditionMessage(e))); NULL })
    if (is.null(l)) return(NULL); l <- l[!grepl("/$|\\\\$", l$Name) & l$Length < 200e6, ]
    if (!nrow(l)) return(NULL)
    ex <- file.path(path.expand(cfg$project_dir), "cache", "zip", z$md5[i])
    if (!dir.exists(ex)) { dir.create(ex, recursive = TRUE); utils::unzip(z$path[i], exdir = ex) }
    p <- file.path(ex, l$Name); p <- ifelse(file.exists(p), p, file.path(ex, gsub("\\\\", "/", l$Name)))
    keep <- file.exists(p); if (!any(keep)) return(NULL)
    data.table(path = normalizePath(p[keep], winslash = "/"), rel = paste0(z$rel[i], "!/", gsub("\\\\", "/", l$Name[keep])),
               file = basename(gsub("\\\\", "/", l$Name[keep])), folder = paste0(z$path[i], "!"), size = l$Length[keep],
               mtime = as.POSIXct(l$Date[keep]), md5 = unname(tools::md5sum(p[keep])))
  }))
}

discover_files <- function(cfg, rules = NULL) {
  dirs <- path.expand(cfg$raw_dirs); dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) stop("None of the data folders exists: ", paste(cfg$raw_dirs, collapse = ", "))
  out <- rbindlist(lapply(dirs, function(d) {
    f <- list.files(d, recursive = TRUE, full.names = TRUE)
    f <- f[!grepl("^(\\._|\\.DS_Store$|~\\$)", basename(f))]
    if (!length(f)) return(NULL)
    data.table(path = normalizePath(f, winslash = "/"), rel = substring(normalizePath(f, winslash = "/"), nchar(normalizePath(d, winslash = "/")) + 2L))
  }))
  if (!nrow(out)) return(data.table())
  out[, `:=`(file = basename(path), folder = dirname(path), size = file.size(path), mtime = file.mtime(path))]
  out[, md5 := unname(tools::md5sum(path))]
  out <- rbind(out, expand_zips(out, cfg), fill = TRUE)
  out[, format := vapply(path, sniff_format, "")]
  out <- merge(out, FORMAT_INFO, by = "format", all.x = TRUE, sort = FALSE)
  out[is.na(role), `:=`(role = "unknown", type = "other", readable = FALSE)]
  out[, stem := file_stem(file)][, status := ""]
  # file rules: ignore a file (it stays listed) or give it another role
  if (!is.null(rules) && nrow(rules)) for (i in seq_len(nrow(rules))) {
    hit <- grepl(rules$pattern[i], out$rel, perl = TRUE)
    if (rules$action[i] == "ignore") out[hit, status := "ignored"] else if (rules$action[i] %in% c("data", "companion", "derived", "notes", "result")) out[hit, role := rules$action[i]]
  }
  setorder(out, rel)
  out[]
}

# Reads every data and derived file once (by MD5; results cached in <project>/cache) and builds the survey table.
build_surveys <- function(files, cfg) {
  cdir <- file.path(path.expand(cfg$project_dir), "cache")
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  cand <- files[role %in% c("data", "derived") & status != "ignored"]
  cand[, survey_id := survey_id_of(md5)]
  first <- cand[order(grepl("!/", rel, fixed = TRUE), mtime, rel), .SD[1], by = md5]   # a loose file is the primary copy, before one inside a zip
  S <- list(); M <- list(); X <- list()
  for (i in seq_len(nrow(first))) {
    run_tick(i, nrow(first), basename(first$rel[i]))
    f <- first[i]; sid <- f$survey_id
    s <- list(survey_id = sid, kind = if (f$role == "derived") "derived" else "original", format = f$format, type = f$type,
              primary_file = f$rel, other_paths = paste(setdiff(cand[md5 == f$md5]$rel, f$rel), collapse = "; "),
              folder = f$folder, stem = f$stem, status = "ok", message = "")
    if (!f$readable) { s$status <- "needs_reader"; s$message <- paste0("no reader yet for ", f$format); S[[sid]] <- s; next }
    r <- cache_read(cdir, f$md5, STEP_HASH$read)
    if (is.null(r)) { r <- tryCatch(read_format(f$path, f$format), error = function(e) e)
      if (!inherits(r, "error")) cache_write(cdir, f$md5, STEP_HASH$read, r) }
    if (inherits(r, "error")) { s$status <- "read_error"; s$message <- conditionMessage(r); problem(f$rel, s$message); S[[sid]] <- s; next }
    unread_report(r$meta, f$rel)                                              # cells that were not numbers (core audit.R)
    s <- c(s, r$meta[setdiff(names(r$meta), names(s))])
    s$n_read <- nrow(r$meas)
    if (!s$n_read) s$status <- "empty" else if (!is.na(s$n_stated %||% NA) && s$n_read < s$n_stated) {
      s$status <- "incomplete"; s$message <- sprintf("%d of %d stated records read", s$n_read, s$n_stated) } else if (!is.na(s$n_stated %||% NA) && s$n_read > s$n_stated)
      s$message <- sprintf("%d records, more than the %d stated in the header (records added to the file later?)", s$n_read, s$n_stated)
    S[[sid]] <- s; M[[sid]] <- r$meas; X[[sid]] <- r$extra
  }
  surveys <- rbindlist(lapply(S, function(s) as.data.table(lapply(s, function(v) if (is.null(v) || !length(v)) NA else v[1]))), fill = TRUE)
  list(surveys = surveys, meas = M, extra = X)
}

# Copies, merges and derived sets: measurements matched by time stamp and resistance to earlier original surveys.
link_surveys <- function(surveys, meas) {
  key <- lapply(meas, function(d) unique(paste(format(d$datetime, "%Y%m%d%H%M%S"), signif(d$resistance_ohm, 5))))
  surveys[, `:=`(parent_ids = "", overlap = NA_real_)]
  ord <- surveys[order(kind != "original", n_read, datetime_start)]$survey_id   # smaller first, so a merge comes after its parts
  seen <- character()
  for (sid in ord) {
    k <- key[[sid]]; if (is.null(k) || !length(k)) next
    hits <- vapply(seen, function(o) mean(k %in% key[[o]]), 0)
    par <- names(hits)[hits >= 0.1]
    if (length(par)) {
      surveys[survey_id == sid, `:=`(parent_ids = paste(par, collapse = "; "), overlap = round(sum(k %in% unlist(key[par])) / length(k), 3))]
      if (surveys[survey_id == sid]$kind == "original")
        surveys[survey_id == sid, kind := if (length(par) > 1) "merged" else if (setequal(k, key[[par]])) "copy" else "subset"]
    }
    if (surveys[survey_id == sid]$kind == "original") seen <- c(seen, sid)
  }
  surveys
}

# Companion files and folder notes; contact resistance joined to each measurement; geometry summary.
attach_companions <- function(surveys, files, meas) {
  comp <- files[role == "companion" & status != "ignored"]; notes <- files[role %in% c("notes") & status != "ignored"]
  surveys[, `:=`(companions = "", n_photos = 0L, n_documents = 0L, n_commands = NA_integer_)]
  contact <- list(); geometry <- list()
  for (i in seq_len(nrow(surveys))) {
    s <- surveys[i]; sid <- s$survey_id
    cp <- comp[folder == s$folder & tolower(stem) == tolower(s$stem)]
    surveys[i, companions := paste(cp$file, collapse = "; ")]
    surveys[i, `:=`(n_photos = sum(notes$folder == s$folder & notes$format == "photo"), n_documents = sum(notes$folder == s$folder & notes$format == "document"))]
    crs <- cp[format == "agi_crs"]; cmd <- cp[format == "agi_cmd"]
    if (nrow(crs) && !is.null(meas[[sid]])) {
      cr <- tryCatch(read_agi_crs(crs$path[1]), error = function(e) { problem(crs$rel[1], conditionMessage(e)); data.table() })
      if (nrow(cr)) {
        contact[[sid]] <- cr
        d <- meas[[sid]]; cr2 <- cr[, .(ax = round(ax, 2), bx = round(bx, 2), datetime, contact_ohm)]
        d[, `:=`(.ax = round(ax, 2), .bx = round(bx, 2))]
        j <- cr2[d, on = .(ax = .ax, bx = .bx, datetime), roll = TRUE, x.contact_ohm]
        d[, contact_ohm := j][, c(".ax", ".bx") := NULL]
        meas[[sid]] <- d
      }
    }
    if (nrow(cmd)) {
      cm <- tryCatch(read_agi_cmd(cmd$path[1]), error = function(e) { problem(cmd$rel[1], conditionMessage(e)); NULL })
      if (!is.null(cm)) { surveys[i, n_commands := cm$n_commands]; geometry[[sid]] <- cm$geometry
        surveys[i, cmd_program := cm$header$progID %||% NA_character_] }
    }
  }
  list(surveys = surveys, meas = meas, contact = contact, cmd_geometry = geometry)
}

# Elevations people add (config/elevations.csv): rows with an electrode number set that electrode's z; rows with a distance
# along the line are interpolated to every electrode. Only z changes; the instrument's apparent resistivity is kept as read.
apply_elevations <- function(d, elev) {
  if (is.null(elev) || !nrow(elev) || !nrow(d)) return(d)
  e <- electrodes_of(d); e[, s := c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))]
  num <- function(x) suppressWarnings(as.numeric(x))
  byn <- elev[nzchar(electrode)]; byd <- elev[!nzchar(electrode) & nzchar(distance_m)]
  z <- rep(NA_real_, nrow(e))
  if (nrow(byd) >= 2) z <- approx(num(byd$distance_m), num(byd$elevation_m), xout = e$s, rule = 2)$y
  if (nrow(byn)) { i <- match(e$electrode, as.integer(byn$electrode)); z[!is.na(i)] <- num(byn$elevation_m)[i[!is.na(i)]] }
  if (all(is.na(z))) return(d)
  d <- copy(d)
  for (k in c("a", "b", "m", "n")) { j <- match(d[[k]], e$electrode); zz <- z[j]; cz <- paste0(k, "z")
    set(d, j = cz, value = as.numeric(d[[cz]])); set(d, which(!is.na(zz)), cz, zz[!is.na(zz)]) }
  d
}

electrodes_of <- function(d) {
  e <- unique(rbindlist(lapply(c("a", "b", "m", "n"), function(k) d[, .(electrode = get(k), x = get(paste0(k, "x")), y = get(paste0(k, "y")), z = get(paste0(k, "z")))])))
  setorder(e[!is.na(electrode) & electrode > 0], electrode)[]
}

geometry_summary <- function(surveys, meas) {
  surveys[, `:=`(datetime_start = NA_character_, datetime_end = NA_character_, year = NA_integer_)]
  for (sid in names(meas)) {
    d <- meas[[sid]]; if (!nrow(d)) next
    e <- electrodes_of(d); step <- sqrt(diff(e$x)^2 + diff(e$y)^2 + diff(e$z)^2)
    arr <- table(d$array); top <- if (length(arr)) names(arr)[which.max(arr)] else NA_character_
    surveys[survey_id == sid, `:=`(
      datetime_start = if (any(!is.na(d$datetime))) fmt_ts(min(d$datetime, na.rm = TRUE)) else NA_character_,
      datetime_end = if (any(!is.na(d$datetime))) fmt_ts(max(d$datetime, na.rm = TRUE)) else NA_character_,
      year = if (any(!is.na(d$datetime))) as.integer(format(min(d$datetime, na.rm = TRUE), "%Y")) else NA_integer_,
      n_electrodes = nrow(e), spacing_m = if (length(step)) round(median(step), 3) else NA_real_,
      positions_off_grid = length(step) > 0 && any(abs(step / median(step) - round(step / median(step))) > 0.05),
      electrode_gaps = if (length(step)) as.integer(sum(pmax(round(step / median(step)) - 1, 0))) else NA_integer_,
      length_m = round(sum(step), 2), elevation_range_m = if (any(!is.na(e$z))) round(diff(range(e$z, na.rm = TRUE)), 2) else NA_real_,
      has_coordinates = any(e$y != 0 | e$z != 0),
      array = if (!length(arr)) NA_character_ else if (max(arr) >= 0.8 * sum(arr) || top == "mixed") top else paste0("mixed (mostly ", top, ")"),
      rhoa_median = signif(median(d$rhoa_ohmm, na.rm = TRUE), 4), error_pct_median = round(median(d$error_pct, na.rm = TRUE), 2),
      current_ma_median = round(median(d$current_a, na.rm = TRUE) * 1000, 1), n_negative_rhoa = sum(d$rhoa_ohmm < 0, na.rm = TRUE))]
  }
  surveys
}

# ---- time zones and the file audit -----------------------------------------------------------------------------------------
# Instrument clocks keep local time with no zone. config/timezones.csv (Ovid/timezones.R) says which zone a clock was in, by
# instrument serial, model, folder or file (file_pattern, on the primary file's path) and date. Times are kept as the
# instrument wrote them; the survey records the offset (utc_offset_h) and the rule that gave it. With no matching row the
# offset is base (Settings > Time zone), blank when that is not set.
survey_zones <- function(surveys, tz, base = NA_real_) {
  base <- suppressWarnings(as.numeric(base)[1])                  # not set (NULL, NA or blank): unknown
  g <- function(v) if (v %in% names(surveys)) as.character(surveys[[v]]) else rep(NA_character_, nrow(surveys))
  ser <- g("serial"); mod <- g("model"); t0 <- suppressWarnings(as.POSIXct(g("datetime_start"), tz = "UTC"))
  z <- lapply(seq_len(nrow(surveys)), function(i) zone_for(ser[i], surveys$primary_file[i], NA, tz, base, mod[i], t0[i]))
  surveys[, `:=`(utc_offset_h = vapply(z, function(x) as.numeric(x$offset), 0), zone_source = vapply(z, function(x) if (is.na(x$row)) "" else x$why, ""))][]
}

# Every file accounted for: data files by measurements (read = in output; GS keeps every measurement and only flags), the
# rest by what became of them (companion, copy of another file, ignored). One row per file (Ovid/file_audit.R).
ert_file_audit <- function(files, surveys, meas) {
  f <- files[, .(file = rel, format, role, status, size, md5)]
  prim <- surveys[, .(file = primary_file, survey_id, n_read = fifelse(is.na(n_read), 0L, as.integer(n_read)), survey_status = status)]
  f <- merge(f, prim, by = "file", all.x = TRUE, sort = FALSE)
  f[, used := !is.na(survey_id) & !survey_status %in% c("needs_reader", "read_error")]
  f[, read := fifelse(used, n_read, NA_integer_)]
  f[!used %in% TRUE, status := fifelse(!is.na(survey_status), survey_status,
                               fifelse(role %in% c("data", "derived") & status != "ignored", "copy of another file (same content)", fifelse(nzchar(status), status, role)))]
  kept <- f[used %in% TRUE, .(source_file = file, in_output = vapply(survey_id, function(s) if (is.null(meas[[s]])) 0L else nrow(meas[[s]]), 0L), used_in = survey_id)]
  fa <- file_reconcile(f, kept, data.table(source_file = character(), stage = character(), rule = character(), level = character(), n = integer()), unit = "measurements")
  fa[, c("n_read", "survey_status", "values_removed") := NULL][]
}
