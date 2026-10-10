# RS/pages/browse.R  --  Browse.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Browse: the system's folder picker, or the in-app browser (Ovid/files.R); sources are offered as places ----
  ovid_browse(input, output, session, "s_path", prompts = list(s_path = "Folder or drive to scan"), places = function() {
    s <- load_configs(CFG)$sources; if (nrow(s)) setNames(s$path, paste("Source:", s$source_id)) })
