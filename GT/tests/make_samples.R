# tests/make_samples.R  --  Picks one small real file per format from the folders given and copies it to tests/samples/
# (core testkit.R make_reader_samples()); the next test run saves what the readers make of them as the expected results.
#   Rscript tests/make_samples.R <folder> [<folder> ...]
Sys.setenv(GT_NO_RUN = "1")
here <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
suppressMessages(source(file.path(here, "gt_pipeline.R"), chdir = TRUE)); source(file.path(CORE_DIR, "testkit.R")); source(file.path(here, "tests", "samples.R"))
dirs <- commandArgs(TRUE); if (!length(dirs)) stop("give the folders to take samples from")
f <- unlist(lapply(path.expand(dirs), function(d) if (dir.exists(d)) list.files(d, recursive = TRUE, full.names = TRUE) else d))   # folders or single files
s <- make_reader_samples(f, file.path(here, "tests", "samples"), SAMPLE_KIND, SAMPLE_READ, SAMPLE_TRIM, no_rows = SAMPLE_NO_ROWS, max_bytes = if (exists("SAMPLE_MAX_BYTES")) SAMPLE_MAX_BYTES else 1.5e6)
cat(length(s), "sample(s):", basename(s), sep = "\n  ")
