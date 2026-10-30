# RS/pages/browse.R  --  Browse.
# Part of the RS server (app.R): sourced inside server() with local = TRUE, so it shares the server's
# variables (input, output, session, rv, C, sel, E, reload, ...). Keep the order in app.R: later pages may use earlier ones.

  # ---- Browse: the system's folder picker, or the in-app browser (Ovid/files.R); sources are offered as places ----
  ovid_browse(input, output, session, c("s_path", "sp_path", "sp_new_dir", "sp_open_dir"),
              prompts = list(s_path = "Folder or drive to scan", sp_path = "Folder or drive to scan", sp_new_dir = "Where the new catalogue goes", sp_open_dir = "A catalogue folder"), places = function() {   # Standard; Simple: the list, a new catalogue, open one
    s <- load_configs(CFG)$sources; if (nrow(s)) setNames(s$path, paste("Source:", s$source_id)) })
