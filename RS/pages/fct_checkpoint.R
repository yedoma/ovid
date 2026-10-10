# RS/pages/fct_checkpoint.R  --  Reading a checkpoint file through the core import (core/import.R), as a plain function (no Shiny;
# tests call it): columns matched by their headings, values read as numbers, and errors, warnings and notes said before any
# accuracy is worked out. The accuracy itself is checkpoint_accuracy() in R/gnss.R.

CHECKPOINT_XYZ <- c("x_survey", "y_survey", "z_survey", "x_product", "y_product", "z_product")

# x: the table read (meta_import_read()). Returns list(rows (one column per matched field), map (field per column), issues
# (severity, message), ok (no error)). A checkpoint needs a surveyed and a product value for the same axis to count.
checkpoint_read <- function(x, fields = CHECKPOINT_FIELDS) {
  spec <- meta_defaults(fields)[, identifies := "FALSE"]                 # the point name is optional: rows without one are numbered
  map <- imp_guess(names(x), spec, stages = c("exact", "units", "words", "fuzzy"))$field
  prep <- imp_prepare(x, map, spec); iss <- imp_check(prep, map, spec)
  have <- intersect(CHECKPOINT_XYZ, map); pairs <- c(h = all(c("x_survey", "y_survey", "x_product", "y_product") %in% have), v = all(c("z_survey", "z_product") %in% have))
  lab <- function(f) paste(spec$label[match(f, spec$field)], collapse = ", ")
  miss <- setdiff(CHECKPOINT_XYZ, have)
  if (!any(pairs)) iss <- rbind(iss, data.table(severity = "error", message = imp_s("No surveyed and product coordinates were found together (missing: ", lab(miss),
                                                                                  "). Name the columns so their headings say what they are, e.g. x_survey, z_product.")), fill = TRUE)
  else if (length(miss)) iss <- rbind(iss, data.table(severity = "warning", message = imp_s("Columns not found: ", lab(miss), ". Only ",
                                                                                          if (pairs[["h"]]) "horizontal" else "vertical", " accuracy can be worked out.")), fill = TRUE)
  k <- which(nzchar(map)); rows <- prep$x[, k, with = FALSE]; setnames(rows, map[k])
  list(rows = rows, map = setNames(map, names(x)), issues = iss[order(match(severity, c("error", "warning", "note")))], ok = !any(iss$severity == "error"))
}
