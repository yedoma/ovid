# R/inversion.R  --  An error model for each survey, files ready for inversion software, and electrode heights from a GNSS file.
# Part of Ovid Core // GS. Sourced by gs_pipeline.R; not meant to be run on its own.
#
# Nothing here changes a measurement. The error model and the inversion files are written into a survey's export; the GNSS
# heights become rows of elevations.csv only when the user adds them.

# ---- Error model -------------------------------------------------------------------------------------------------------------
# With reciprocal pairs (A,B,M,N measured again as M,N,A,B) the error grows with the transfer resistance, as ResIPy fits it:
# the pairs are sorted by |R| into `bins` bins of equal count, and the mean reciprocal error of each bin is fitted against its
# mean |R| as a line (e = a + b|R|) and as a power law (e = a|R|^b); the better fit is kept. Without enough pairs every datum
# gets `default_rel` (4 %, the CPERS assumption), or its own stacking error when that is larger. The relative error of each
# datum (err, a fraction) is what pyGIMLi and ResIPy weight the data with.
error_model <- function(d, bins = 20L, min_pairs = 40L, default_rel = 0.04) {
  R <- d$resistance_ohm; k1 <- paste(d$a, d$b, d$m, d$n); k2 <- paste(d$m, d$n, d$a, d$b); j <- match(k1, k2)
  pr <- data.table(i = seq_len(nrow(d)), j = j)[!is.na(j) & i < j]
  rep_ <- duplicated(k1) | duplicated(k1, fromLast = TRUE)                       # a quadrupole read twice has no single reciprocal
  pr <- pr[is.finite(R[i]) & is.finite(R[j]) & R[i] != 0 & !rep_[i] & !rep_[j]]
  stack <- if ("error_pct" %in% names(d)) suppressWarnings(as.numeric(d$error_pct)) / 100 else rep(NA_real_, nrow(d))
  fallback <- function(why) list(model = "fixed", a = default_rel, b = NA_real_, r2 = NA_real_, pairs = nrow(pr), source = why,
                                 err = round(pmax(default_rel, ifelse(is.finite(stack), stack, 0)), 4))
  if (nrow(pr) < min_pairs) return(fallback(sprintf("%d reciprocal pairs (fewer than %d): %g %% relative, or the stacking error when larger", nrow(pr), min_pairs, 100 * default_rel)))
  pr[, `:=`(r = (abs(R[i]) + abs(R[j])) / 2, e = abs(R[i] - R[j]))]
  pr[, bin := cut(rank(r, ties.method = "first"), min(bins, nrow(pr) %/% 2), labels = FALSE)]
  b <- pr[, .(r = mean(r), e = mean(e)), by = bin][r > 0]
  lin <- stats::lm(e ~ r, data = b); r2l <- summary(lin)$r.squared
  pw <- if (all(b$e > 0)) stats::lm(log(e) ~ log(r), data = b) else NULL
  r2p <- if (!is.null(pw)) { pe <- exp(stats::fitted(pw)); 1 - sum((b$e - pe)^2) / sum((b$e - mean(b$e))^2) } else -Inf
  if (r2p > r2l) { a <- exp(stats::coef(pw)[[1]]); bb <- stats::coef(pw)[[2]]; f <- function(r) a * r^bb; mdl <- "power"; r2 <- r2p }
  else { a <- stats::coef(lin)[[1]]; bb <- stats::coef(lin)[[2]]; f <- function(r) a + bb * r; mdl <- "linear"; r2 <- r2l }
  rel <- f(abs(R)) / abs(R); rel[!is.finite(rel) | rel <= 0] <- default_rel
  list(model = mdl, a = signif(a, 4), b = signif(bb, 4), r2 = round(r2, 3), pairs = nrow(pr), source = sprintf("fitted to %d reciprocal pairs in %d bins", nrow(pr), nrow(b)),
       err = round(pmin(1, pmax(0.005, rel)), 4))
}

# ---- Files for inversion software ---------------------------------------------------------------------------------------------
# The passed measurements of one survey (every flagged measurement left out; the manifest lists which and why) as
#   <id>.ohm           pyGIMLi / BERT unified data format: electrodes (x y z), then a b m n rhoa err (err relative)
#   protocol.dat       R2 / ResIPy: index, A B M N, transfer resistance and its absolute error (by reciprocity the
#                      resistance is the same with the current and potential pairs swapped), with electrodes.csv for the mesh
#   <id>_res2dinv.dat  Res2DInv general array (code 11), electrodes at the surface with their heights as a topography list
#   error_model.json   the error model and where it came from; manifest.json what was left out
# Remote electrodes (0) are written as 0 (pyGIMLi, R2) or at 99999 m (Res2DInv), as those programs expect.
inversion_files <- function(s1, meas, flags, dir, model = NULL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  id <- safe_name(s1$survey_id %||% "survey")
  out <- meas[!meas_id %in% flags$meas_id & is.finite(resistance_ohm) & !is.na(a) & !is.na(m)]
  if (!nrow(out)) return(invisible(NULL))
  em <- model %||% error_model(meas); err <- em$err[match(out$meas_id, meas$meas_id)]
  el <- electrodes_of(meas)[is.finite(x)]
  num <- function(x) formatC(x, format = "fg", digits = 8, flag = " ")
  ix <- function(e) ifelse(e == 0, 0L, match(e, el$electrode))
  L <- c(sprintf("%d# number of electrodes", nrow(el)), "# x y z", sprintf("%.4f %.4f %.4f", el$x, el$y, ifelse(is.finite(el$z), el$z, 0)),
         sprintf("%d# number of data", nrow(out)), "# a b m n rhoa err r",
         sprintf("%d %d %d %d %.6g %.4f %.6g", ix(out$a), ix(out$b), ix(out$m), ix(out$n), out$rhoa_ohmm, err, out$resistance_ohm), "0# number of topography points")
  writeLines(L, file.path(dir, paste0(id, ".ohm")))
  writeLines(c(as.character(nrow(out)), sprintf("%d %d %d %d %d %.6g %.6g", seq_len(nrow(out)), out$a, out$b, out$m, out$n, out$resistance_ohm, abs(out$resistance_ohm) * err)),
             file.path(dir, "protocol.dat"))
  fwrite(el[, .(electrode, x, y, z)], file.path(dir, "electrodes.csv"))
  s <- c(0, cumsum(sqrt(diff(el$x)^2 + diff(el$y)^2))); sx <- function(e) ifelse(e == 0, 99999, s[match(e, el$electrode)])
  sp <- if (nrow(el) > 1) stats::median(diff(s)) else 1
  z <- ifelse(is.finite(el$z), el$z, 0); topo <- any(abs(z - z[1]) > 1e-6)
  writeLines(c(paste(s1$survey_id, "(ovid GS export)"), sprintf("%.3f", sp), "11", "0", "Type of measurement (0=app. resistivity,1=resistance)", "0", as.character(nrow(out)), "1", "0",
               sprintf("4 %.3f 0 %.3f 0 %.3f 0 %.3f 0 %.6g", sx(out$a), sx(out$b), sx(out$m), sx(out$n), out$rhoa_ohmm),
               if (topo) c("Topography in separate list", "2", as.character(nrow(el)), sprintf("%.3f %.3f", s, z), "1"), "0", "0", "0", "0"),
             file.path(dir, paste0(id, "_res2dinv.dat")))
  jsonlite::write_json(em[setdiff(names(em), "err")], file.path(dir, "error_model.json"), auto_unbox = TRUE, pretty = TRUE)
  left <- if (nrow(flags)) flags[meas_id %in% meas$meas_id, .(measurements = uniqueN(meas_id)), by = .(rule, flag)] else data.table()
  jsonlite::write_json(list(survey_id = s1$survey_id, measurements = nrow(meas), written = nrow(out), left_out = nrow(meas) - nrow(out), left_out_by_rule = left,
                            electrodes = nrow(el), topography = if (topo) "from the electrode heights (elevations.csv or the instrument)" else "flat",
                            error = em$source, note = "Every flagged measurement is left out. The error column is relative (a fraction of the value)."),
                       file.path(dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
  invisible(dir)
}

# ---- Electrode heights from a GNSS file ----------------------------------------------------------------------------------------
# A survey file of points (an Emlid, Leica or DJI CSV export, or the GNSS points table of the RS app) with a name column and
# an elevation (or ellipsoidal height). The electrode is the last number in each point's name ("T1-E12", "E12", "12"), unless
# a pattern with one group says where it is. Rows for elevations.csv; heights of different kinds (orthometric, ellipsoidal)
# are never mixed: the elevation column wins.
gnss_elevations <- function(path, survey_id, pattern = "([0-9]+)\\D*$", author = "") {
  x <- fread(file = path, colClasses = "character", na.strings = c("", "NA")); nm <- tolower(gsub("[^a-z0-9]", "", tolower(names(x))))
  pick <- function(c) { k <- match(c, nm); k <- k[!is.na(k)]; if (length(k)) names(x)[k[1]] else NA_character_ }
  nc <- pick(c("name", "pointname", "point", "id", "pointid", "gpsname", "label"))
  zc <- pick(c("elevation", "elevationm", "orthometricheight", "orthometricheightm", "z", "altitude", "alt"))
  hk <- if (is.na(zc)) pick(c("ellipsoidalheight", "ellipsoidalheightm", "ellipsoidheight", "elliph", "ellipsoidal", "height")) else NA_character_
  if (is.na(nc) || (is.na(zc) && is.na(hk))) stop("the file needs a point name column and an elevation (or height) column")
  e <- suppressWarnings(as.integer(sub(paste0("^.*?", pattern), "\\1", x[[nc]], perl = TRUE)))
  e[!grepl(pattern, x[[nc]], perl = TRUE)] <- NA
  v <- suppressWarnings(as.numeric(x[[if (!is.na(zc)) zc else hk]]))
  ok <- !is.na(e) & is.finite(v); if (!any(ok)) stop("no point name holds an electrode number")
  r <- data.table(survey_id = survey_id, electrode = as.character(e[ok]), distance_m = "", elevation_m = as.character(round(v[ok], 3)),
                  source = paste0("GNSS file ", basename(path), if (is.na(zc)) " (ellipsoidal heights)" else ""), author = author)
  r[!duplicated(electrode, fromLast = TRUE)][order(as.integer(electrode))]
}

# Electrode elevations from one file people add on Survey > Geometry: an elevation table (electrode and elevation, or distance
# along the line and elevation) or a GNSS survey file (gnss_elevations()), told apart by its columns: an electrode column makes
# it a table, else a point name column a GNSS file, else a distance column a table. name: the file's name as the person gave it.
# Returns the rows for elevations.csv and which kind it was ("table" or "gnss").
elevation_file_rows <- function(path, survey_id, name = basename(path), author = "") {
  x <- fread(file = path, colClasses = "character", na.strings = c("", "NA")); if (ncol(x) < 2) stop("could not read that file as a table")
  nm <- tolower(gsub("[^a-z0-9]", "", tolower(names(x)))); col <- function(...) { i <- which(nm %in% c(...))[1]; if (is.na(i)) NULL else x[[i]] }
  el <- col("electrode", "elec", "e", "electrodeno"); ds <- col("distance", "dist", "x", "distancem", "s")
  z <- col("elevation", "elev", "z", "height", "elevationm", "zm")
  gnss <- is.null(el) && !is.null(col("name", "pointname", "point", "id", "pointid", "gpsname", "label"))
  if (gnss) { r <- gnss_elevations(path, survey_id, author = author); r[, source := sub("GNSS file [^ ]+", paste("GNSS file", name), source)]; return(list(rows = r, kind = "gnss")) }
  if (is.null(z) || (is.null(el) && is.null(ds))) stop("the file needs an elevation column and an electrode or distance column, or a point name and an elevation (a GNSS file)")
  list(rows = data.table(survey_id = survey_id, electrode = if (!is.null(el)) as.character(el) else "", distance_m = if (is.null(el)) as.character(ds) else "",
                         elevation_m = as.character(z), source = name, author = author), kind = "table")
}
