# R/preflight.R  --  Set-up check: what in the set-up would be ignored, misapplied or is missing. Nothing is changed.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R; not meant to be run on its own.

setup_check <- function(cfg) {
  chk <- check_collector(c("level", "area", "item", "problem")); add <- chk$add
  check_folders(add, cfg$raw_dirs, cfg$project_dir)
  unread <- function(what) function(e) { add("warning", "Metadata", what, paste("could not be read:", conditionMessage(e))); NULL }
  has_reg <- length(cfg$metadata_dir) && nzchar(cfg$metadata_dir) && dir.exists(path.expand(cfg$metadata_dir)) &&
    !is.null(tryCatch(find_registry(cfg$metadata_dir), error = unread("site registry")))
  config_migrate(cfg$project_dir, "preflight")                                           # older tables moved in once (core config.R)
  ms <- tryCatch(meta_sheet(cfg$project_dir, META_FIELDS, report = FALSE, deploy = gt_deploy_rows, map = GT_MAP), error = unread("deployment sheet"))
  cf0 <- attr(ms, "conflicts"); for (i in seq_len(NROW(cf0))) { z <- cf0[i]                  # core metasheet.R: the typed column wins, and says so
    add("warning", "Deployment sheet", z$item, paste0("From and To give ", z$field, " '", z$sheet, "', the ", z$field, " column '", z$list, "', which is used. Change the one that is wrong.")) }
  if (!has_reg && is.null(ms)) add("note", "Metadata", "optional", "no registry workbook and no deployment sheet: sites are named after their loggers and depths follow the templates or the default steps",
                                   "To name loggers or give sites and depths, fill in the Deployment sheet (Sensors and depths), or put a registry workbook in the metadata folder.")
  if (!is.null(ms)) { p <- meta_sheet_problems(cfg$project_dir, META_FIELDS, gt_deploy_rows, GT_MAP)
    for (i in seq_len(nrow(p))) add("warning", "Deployment sheet", paste("row", p$row[i] + 1L), paste0(p$field[i], " '", p$value[i], "': ", p$problem[i], "; the value is left blank meanwhile.")) }
  pd <- path.expand(cfg$project_dir)
  if (!dir.exists(pd)) add("note", "Folders", cfg$project_dir, "The project folder will be created at the first run.")
  rd <- function(w) { p <- cfg_path(cfg$project_dir, w); if (file.exists(p)) tryCatch(ensure_cols(read_cfg(p), CONFIG_FILES[[w]]$cols), error = function(e) { add("error", CONFIG_FILES[[w]]$label, basename(p), conditionMessage(e)); NULL }) else NULL }
  se <- rd("sensors")
  if (!is.null(se) && nrow(se)) {
    k <- which(nzchar(se$depth_m) & is.na(suppressWarnings(as.numeric(se$depth_m)))); for (i in k) add("error", "Sensors and depths", paste(se$serial[i], se$sensor[i]), paste0("depth '", se$depth_m[i], "' is not a number (metres)."))
    for (v in c("start", "end")) for (i in which(bad_date(se[[v]]))) add("error", "Sensors and depths", paste(se$serial[i], se$sensor[i]), paste0(v, " '", se[[v]][i], "' is not a date (YYYY-MM-DD or YYYY-MM-DD HH:MM)."))
    k <- which(!se$logger_type %in% c(names(TYPE_LABEL), "")); for (i in k) add("error", "Sensors and depths", se$serial[i], paste0("logger_type '", se$logger_type[i], "' is not one of ", paste(names(TYPE_LABEL), collapse = ", "), "."))
    z <- typed_depth(se$depth_m, depth_sign(cfg)); k <- which(!is.na(z) & (z > 50 | z < -5)); for (i in k) add("warning", "Sensors and depths", paste(se$serial[i], se$sensor[i]), paste0("depth ", se$depth_m[i], " m looks wrong (centimetres instead of metres?)."))
    s2 <- se[nzchar(sensor)][, `:=`(a = as.numeric(parse_date_cell(start)), b = as.numeric(parse_date_cell(end)))]
    ov <- s2[, .(n = .N, overlap = if (.N < 2) FALSE else { o <- order(a, na.last = FALSE); a2 <- a[o]; b2 <- b[o]
      any(is.na(b2[-.N]) | is.na(a2[-1]) | a2[-1] <= b2[-.N]) }), by = .(logger_type, serial, sensor)][overlap == TRUE]
    for (i in seq_len(nrow(ov))) add("warning", "Sensors and depths", paste(ov$serial[i], ov$sensor[i]), "listed more than once with overlapping dates: the first matching row is used. Give each row a start and an end date.")
    dflt <- se[depth_source %in% c("default", "template")]
    if (nrow(dflt)) add("note", "Sensors and depths", paste(uniqueN(dflt$serial), "loggers"), paste0(nrow(dflt), " sensors have assumed depths (default steps or a template). Confirm them on the Sensors and depths page."))
    st <- unique(se[nzchar(site), .(site, logger_type)])[, .N, by = site][N > 1]
    for (s in st$site) add("warning", "Sites", s, "used for more than one instrument type; they will be kept as separate sites, never merged.")
  }
  si <- rd("sites")
  if (!is.null(si) && nrow(si)) {
    k <- which(nzchar(si$landcover) & !si$landcover %in% LANDCOVERS); for (i in k) add("warning", "Sites", si$site[i], paste0("landcover '", si$landcover[i], "' is not one of ", paste(LANDCOVERS, collapse = ", "), "."))
    if (!is.null(se) && nrow(se)) for (s in setdiff(unique(se$site[nzchar(se$site)]), si$site)) add("note", "Sites", s, "has sensors but no row on the Sites page (it will be added at the next run).")
    used <- if (!is.null(se)) unique(se$site) else character()
    k <- which(!nzchar(si$landcover) & si$site %in% used); if (length(k)) add("note", "Sites", paste(length(k), "sites"), paste0("no landcover set: ", paste(head(si$site[k], 8), collapse = ", "), if (length(k) > 8) " ..." else ""))
  }
  qc <- rd("qc")
  if (!is.null(qc)) { for (v in c("min", "max", "max_spike", "max_flat_hours", "flat_exempt_band")) for (i in which(nzchar(qc[[v]]) & is.na(suppressWarnings(as.numeric(qc[[v]])))))
      add("error", "Quality rules", paste(qc$variable[i], qc$applies_to[i]), paste0(v, " '", qc[[v]][i], "' is not a number."))
    for (i in which(!tolower(qc$action) %in% c("remove", "flag", ""))) add("error", "Quality rules", paste(qc$variable[i], qc$applies_to[i]), paste0("action '", qc$action[i], "' must be remove or flag.")) }
  pf <- cfg_path(cfg$project_dir, "period"); pr <- period_read(pf)                      # corrections over a period, the manual removals among them
  for (i in seq_len(nrow(pr$bad))) add("error", "Corrections over a period", paste("row", pr$bad$row[i]), paste0(pr$bad$problem[i], "; the row is not applied."))
  mn <- gt_period_to_manual(period_own_rows(pf, c("remove", "flag")))
  if (nrow(mn)) for (i in which(nzchar(mn$depth_m) & !vapply(strsplit(mn$depth_m, "|", fixed = TRUE), function(z) all(depth_ok(z)), NA)))
    add("error", "Corrections over a period", paste("row", mn$row[i]), paste0("depth_m '", mn$depth_m[i], "' is not a depth in metres (several separated by |)."))
  tz <- rd("timezones")
  if (!is.null(tz) && nrow(tz)) { x <- tz_check(tz); for (i in seq_len(nrow(x))) add("error", "Time zones", paste0("row ", x$row[i], if (nzchar(x$item[i])) paste0(" (", x$item[i], ")") else ""), x$problem[i]) }
  tc <- rd("time")
  if (!is.null(tc) && nrow(tc)) for (i in which(is.na(suppressWarnings(as.numeric(tc$shift_minutes))))) add("error", "Clock corrections", tc$serial[i], "shift_minutes must be a number.")
  tp <- rd("templates")
  if (!is.null(tp) && nrow(tp)) for (i in which(!vapply(tp$depths, function(x) all(!is.na(suppressWarnings(as.numeric(trim(strsplit(x, ",")[[1]]))))), logical(1))))
    add("error", "Depth templates", paste(tp$make[i], tp$model[i]), "depths must be numbers separated by commas.")
  x <- with_declared(chk$result(), cfg$project_dir); x[order(match(level, c("error", "warning", "note")))]   # with every table's declared rules (core table_rules.R)
}
