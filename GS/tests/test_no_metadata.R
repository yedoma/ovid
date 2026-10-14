# GS/tests/test_no_metadata.R  --  GS runs with only its raw data folder set (no metadata, no project folder): the shared
# test core/tests/test_raw_only.R for this app.   Rscript tests/test_no_metadata.R
a <- grep("^--file=", commandArgs(FALSE), value = TRUE); here <- dirname(dirname(normalizePath(sub("^--file=", "", a[1]))))
q <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(dirname(here), "core", "tests", "test_raw_only.R")), "GS"))
quit(status = q)
