# launcher/run_app.R  --  started by Ovid_RS.bat or "Open RS (Mac).command". Finds the shared Ovid Core files and hands
# over to their launcher (core/launcher.R), which installs packages into library/ and opens the app; "--portable" makes the folder self-contained.

APP <- list(key = "RS", title = "ovid // RS", bat = "Ovid_RS.bat",
            pkgs = c("shiny", "bslib", "DT", "data.table", "ggplot2", "jsonlite", "zip"))
args <- commandArgs(trailingOnly = TRUE)
me <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
app_dir <- normalizePath(file.path(dirname(normalizePath(me)), ".."), winslash = "/")
CORE_DIR <- local({ d <- c(Sys.getenv("OVID_CORE"), file.path(app_dir, "core"), file.path(dirname(app_dir), "core"))
  d <- path.expand(d[nzchar(d)]); hit <- d[file.exists(file.path(d, "launcher.R"))]
  if (!length(hit)) stop("The shared ovid core folder was not found. Looked in: ", paste(d, collapse = ", "),
                         ". Keep this app in the Ovid_Apps folder beside core/,",
                         " or run Make_Portable.bat on a computer that has it.")
  normalizePath(hit[1], winslash = "/") })
source(file.path(CORE_DIR, "launcher.R"))
