# R/export.R  --  Exports: one survey with its QC result and the user's edits, and the whole catalogue as a package
# others can browse without the raw archive. Part of Ovid Core // GS. Sourced by gs_pipeline.R.
#
# Exports are written to new zip files only; raw files and the project's own tables are never changed by an export.


# One survey: every measurement with its QC result (nothing removed), the passed set, the instrument's own fields,
# electrodes, metadata, a provenance record that is enough to reproduce the result, and the pseudosection.
#   s1     the survey's catalogue row (list), with the user's metadata merged in
#   rules  the QC rules used (qc_profiles.csv layout); flags  the flags they gave (meas_id, flag, rule, value)
export_survey <- function(s1, meas, flags, rules, profile, zipfile, project_dir = CFG$project_dir, manual = NULL) {
  nz <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1])
  tmp <- file.path(tempdir(), paste0("ert_export_", s1$survey_id)); unlink(tmp, TRUE); dir.create(tmp)
  fl <- flags[, .(qc_flags = paste(unique(flag), collapse = "; "), qc_rules = paste(unique(rule), collapse = "; ")), by = meas_id]
  m <- merge(copy(meas), fl, by = "meas_id", all.x = TRUE, sort = TRUE)
  m[, qc_pass := is.na(qc_flags)][is.na(qc_flags), `:=`(qc_flags = "", qc_rules = "")]
  m[, `:=`(survey_id = s1$survey_id, transect_id = nz(s1$transect_id))]
  setcolorder(m, c("survey_id", "transect_id", "meas_id", "qc_pass", "qc_flags"))
  fwrite(m, file.path(tmp, "measurements_all.csv")); fwrite(m[qc_pass == TRUE], file.path(tmp, "measurements_passed.csv"))
  sd <- file.path(path.expand(project_dir), "output", "surveys", s1$survey_id)
  fwrite(electrodes_with_status(electrodes_of(meas), tryCatch(load_configs(CFG)$electrodes, error = function(e) NULL), s1$survey_id, s1), file.path(tmp, "electrodes.csv"))
  for (f in c("extra.csv", "contact.csv", "cmd_geometry.csv")) if (file.exists(file.path(sd, f)))
    file.copy(file.path(sd, f), file.path(tmp, if (f == "extra.csv") "instrument_fields.csv" else f))
  meta <- lapply(s1, function(v) if (length(v) && !is.na(v[1])) v[1] else NULL); meta <- meta[!vapply(meta, is.null, TRUE)]
  jsonlite::write_json(meta, file.path(tmp, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  src <- unique(c(s1$primary_file, if (nzchar(s1$other_paths %||% "")) strsplit(s1$other_paths, "; ")[[1]]))
  prov <- list(survey_id = s1$survey_id, transect_id = nz(s1$transect_id), name = survey_label(s1)$name, date = survey_label(s1)$file_date,
               source_files = src, source_format = s1$format, instrument = if (nzchar(nz(s1$model))) nz(s1$model) else nz(s1$manufacturer),
               serial = nz(s1$serial), exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
               ovid = list(app = APP_NAME, version = APP_VERSION, core_version = CORE_VERSION, r = R.version.string),
               qc = list(profile = profile, rules = rules[, .(rule, value, flag, applies_to)],
                         measurements = nrow(m), passed = sum(m$qc_pass), flagged = sum(!m$qc_pass)),
               manual_flags = if (!is.null(manual) && nrow(manual)) manual[survey_id == s1$survey_id] else list(),
               elevations = { ef <- file.path(path.expand(project_dir), "config", "elevations.csv"); e <- if (file.exists(ef)) fread(ef, colClasses = "character")[survey_id == s1$survey_id] else NULL
                              if (is.null(e) || !nrow(e)) "as recorded by the instrument" else list(source = unique(e$source), note = "z of the electrodes set from config/elevations.csv", rows = e) },
               note = "measurements_all.csv holds every measurement as read from the source file; QC only adds qc_pass and qc_flags.")
  jsonlite::write_json(prov, file.path(tmp, "provenance.json"), auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
  g <- plot_pseudosection(meas, flags, s1, profile); if (!is.null(g)) ggsave(file.path(tmp, "pseudosection.png"), g, width = 9, height = 8.5, dpi = 100)
  tryCatch(inversion_files(s1, meas, flags, file.path(tmp, "inversion")), error = function(e) writeLines(paste("inversion files not written:", conditionMessage(e)), file.path(tmp, "inversion_not_written.txt")))
  zip_dir(tmp, zipfile)
}

# The catalogue as a package: catalogue tables, previews, the metadata tables and QC profiles, and index.html, a
# page that lists every survey with its preview and opens in any browser. Measurements are added only on request.
export_archive <- function(zipfile, project_dir = CFG$project_dir, with_measurements = FALSE) {
  pd <- path.expand(project_dir); cat_dir <- file.path(pd, "output", "catalogue")
  tmp <- file.path(tempdir(), "ert_archive"); unlink(tmp, TRUE); dir.create(file.path(tmp, "metadata"), recursive = TRUE)
  file.copy(list.files(cat_dir, full.names = TRUE, include.dirs = TRUE), tmp, recursive = TRUE)
  for (f in c("projects.csv", "sites.csv", "transects.csv", "surveys.csv", "qc_profiles.csv", "manual_flags.csv"))
    if (file.exists(file.path(pd, "config", f))) file.copy(file.path(pd, "config", f), file.path(tmp, "metadata", f))
  if (with_measurements) file.copy(file.path(pd, "output", "surveys"), tmp, recursive = TRUE)
  writeLines(archive_index(fread(file.path(cat_dir, "surveys.csv"), colClasses = list(character = c("datetime_start", "datetime_end")))), file.path(tmp, "index.html"))
  writeLines(c("Ovid GS catalogue", paste("Exported", format(Sys.time(), "%Y-%m-%d %H:%M"), "by", APP_NAME, APP_VERSION), "",
               "Open index.html in a browser to browse the surveys. surveys.csv holds every survey with its metadata, statistics and QC",
               "result; files.csv lists every file in the archive; survey_stats.csv has the distributions; previews/ the pseudosections;",
               "metadata/ the tables people edit. The raw files are not included.",
               if (with_measurements) "surveys/<survey_id>/ holds the measurements of each survey in the common Ovid GS format."),
             file.path(tmp, "README.txt"))
  zip_dir(tmp, zipfile)
}

# A self-contained page: one table of surveys (search box, sortable by clicking a heading) with links to the previews.
archive_index <- function(s) {
  s <- s[kind %in% c("original", "merged")]
  for (v in c("display_name", "acquired_date", "project", "site", "transect_id", "quality", "notes", "preview")) {
    if (!v %in% names(s)) s[, (v) := ""]; s[, (v) := as.character(get(v))]; s[is.na(get(v)), (v) := ""] }
  s[, nm := ifelse(nzchar(display_name), display_name, sub("\\.(stg|csv|txt|dat)$", "", basename(primary_file), ignore.case = TRUE))]
  s[, dt := ifelse(nzchar(acquired_date), acquired_date, ifelse(is.na(datetime_start) | datetime_start == "", "", substr(datetime_start, 1, 10)))]
  cell <- function(x) html_esc(ifelse(is.na(x), "", as.character(x)))
  rows <- s[, sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class=n>%s</td><td class=n>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                      cell(dt), if ("preview" %in% names(s)) ifelse(nzchar(preview), sprintf("<a href='previews/%s' target=_blank>%s</a>", cell(preview), cell(nm)), cell(nm)) else cell(nm),
                      cell(project), cell(site), cell(transect_id), cell(model), cell(n_read), cell(ifelse(is.na(pct_retained), "", paste0(round(pct_retained), "%"))),
                      cell(quality), cell(array), cell(notes))]
  c("<!doctype html><html><head><meta charset='utf-8'><meta name=viewport content='width=device-width,initial-scale=1'><title>Ovid GS catalogue</title><style>",
    sprintf("body{background:%s;color:%s;font:14px/1.45 -apple-system,Segoe UI,Inter,sans-serif;margin:24px} h1{font-weight:600;margin:0 0 4px}
     .muted{color:%s} input{background:%s;color:%s;border:1px solid %s;border-radius:6px;padding:7px 10px;width:320px;margin:14px 0}
     table{border-collapse:collapse;width:100%%} th,td{padding:7px 10px;border-bottom:1px solid %s;text-align:left;white-space:nowrap} th{cursor:pointer;color:%s}
     td.n{text-align:right} a{color:%s} tr:hover td{background:%s}", PAL$bg, PAL$fg, PAL$fg4, PAL$bg1, PAL$fg, PAL$bg2, PAL$bg2, PAL$fg4, PAL$accent, PAL$bg1),
    "</style></head><body><h1>Ovid GS catalogue</h1>",
    sprintf("<div class=muted>%d surveys &middot; exported %s &middot; click a name for its pseudosection, a heading to sort</div>", nrow(s), format(Sys.Date())),
    "<input id=q placeholder='Search' oninput='f()'><table id=t><thead><tr><th>Date</th><th>Name</th><th>Project</th><th>Site</th><th>Transect</th><th>Instrument</th><th>Measurements</th><th>Kept %</th><th>Quality</th><th>Array</th><th>Notes</th></tr></thead><tbody>",
    rows, "</tbody></table><script>",
    "function f(){var q=document.getElementById('q').value.toLowerCase();document.querySelectorAll('#t tbody tr').forEach(r=>r.style.display=r.textContent.toLowerCase().includes(q)?'':'none')}",
    "document.querySelectorAll('#t th').forEach((h,i)=>h.onclick=()=>{var b=document.querySelector('#t tbody'),r=[...b.rows],d=h.dataset.d=h.dataset.d==='1'?'-1':'1';r.sort((x,y)=>{var a=x.cells[i].textContent,c=y.cells[i].textContent,na=parseFloat(a),nc=parseFloat(c);return (isNaN(na)||isNaN(nc)?a.localeCompare(c):na-nc)*d});r.forEach(x=>b.appendChild(x))})",
    "</script></body></html>")
}

# Opens a catalogue made by export_archive() so the app can browse it: the zip is unpacked into dest as a project
# folder (output/catalogue, output/surveys when measurements were included, config from metadata/). The zip is not changed.
import_catalogue <- function(zipfile, dest) {
  tmp <- file.path(tempdir(), paste0("ert_import_", as.integer(Sys.time()))); dir.create(tmp)
  utils::unzip(zipfile, exdir = tmp)
  hits <- list.files(tmp, pattern = "^surveys\\.csv$", recursive = TRUE, full.names = TRUE)
  hits <- hits[file.exists(file.path(dirname(hits), "files.csv"))]                   # the catalogue's, not metadata/surveys.csv
  root <- if (length(hits)) dirname(hits[which.min(nchar(hits))]) else NA
  if (is.na(root)) stop("This zip is not an Ovid GS catalogue (no surveys.csv in it).")
  unlink(dest, recursive = TRUE); cat_dir <- file.path(dest, "output", "catalogue"); dir.create(cat_dir, recursive = TRUE)
  dir.create(file.path(dest, "config"))
  for (f in list.files(root, full.names = TRUE)) {
    b <- basename(f)
    if (b == "surveys" && dir.exists(f)) file.copy(f, file.path(dest, "output"), recursive = TRUE)
    else if (b == "metadata") file.copy(list.files(f, full.names = TRUE), file.path(dest, "config"))
    else file.copy(f, cat_dir, recursive = TRUE)
  }
  writeLines(c(paste("Imported from", basename(zipfile), "on", format(Sys.time(), "%Y-%m-%d %H:%M")),
               "Changes made in the app are saved in this copy only."), file.path(dest, "IMPORTED.txt"))
  unlink(tmp, recursive = TRUE)
  dest
}
