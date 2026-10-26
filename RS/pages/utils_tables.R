# RS: the theme and plot colours. Sourced by app.R.
# ---- the theme -----------------------------------------------------------------------------------------------------------
THEME <- bs_theme(version = 5, bg = PAL$bg, fg = PAL$fg, primary = PAL$accent, secondary = PAL$bg2, success = PAL$green, warning = PAL$gold,
                  danger = PAL$red, info = PAL$bone, base_font = font_collection("Inter", "Avenir Next", "Segoe UI", "-apple-system", "Helvetica", "Arial", "sans-serif"),
                  "border-radius" = "0.4rem") |>
  bs_add_rules(sprintf("
    :root { --fg:%2$s; --fg4:%3$s; --gray:%4$s; --red:%5$s; --green:%6$s; --acc:%6$s; --bg2:%1$s; --bg3:%7$s; }
    .card { background: %8$s; border-color: %7$s; }  .card-header { font-size: 13.5px; font-weight: 600; }
    label, .control-label { color: %3$s !important; font-weight: 500; font-size: 13.5px !important; }
    table.dataTable { font-size: 13px; } table.dataTable thead th { color: %3$s; font-weight: 600; white-space: nowrap; }
    table.dataTable tbody tr { background: transparent !important; } table.dataTable td { white-space: nowrap; }
    table.dataTable tbody tr:hover { background: %1$s !important; cursor: pointer; } table.dataTable tbody tr.selected td { box-shadow: inset 0 0 0 9999px %7$s !important; color: %2$s !important; }
    .dataTables_wrapper, .dataTables_wrapper .dataTables_filter input, .dataTables_info { color: %3$s !important; }
    .fact { display:flex; justify-content:space-between; gap:12px; padding:3px 0; border-bottom:1px dashed %7$s; font-size:.86rem; }
    .fact .k { color:%3$s; white-space:nowrap; } .fact .v { text-align:right; word-break:break-word; }
    .badge-st { border:1px solid; border-radius:6px; padding:1px 8px; font-size:.8rem; white-space:nowrap; }
    .muted { color:%3$s; font-size:.85rem; } .legend-line { font-size:.8rem; color:%3$s; }
    #rsmap { height: 64vh; min-height: 380px; border-radius: 6px; background: %1$s; }
    .leaflet-container { font: inherit; } .leaflet-tooltip { background:%8$s; color:%2$s; border-color:%7$s; }
    .leaflet-control-layers, .leaflet-bar a { background:%8$s !important; color:%2$s !important; border-color:%7$s !important; }
    .map-style { background:%8$s; color:%2$s; font-size:12.5px; } .map-style .ms-btn { width:auto !important; padding:0 9px; font-weight:600; }
    .map-style .ms-body { padding:8px 10px; max-height:52vh; overflow:auto; min-width:250px; } .map-style label { display:flex; justify-content:space-between; gap:8px; margin:0 0 6px; color:%3$s; }
    .map-style select { background:%1$s; color:%2$s; border:1px solid %7$s; border-radius:4px; }
    .ms-head, .ms-row { display:grid; grid-template-columns:1fr 30px 80px; gap:6px; align-items:center; padding:2px 0; } .ms-head { color:%3$s; border-bottom:1px solid %7$s; }
    .ms-row input[type=color] { width:28px; height:20px; padding:0; border:1px solid %7$s; background:none; } .ms-row input[type=range], .map-style input[type=range] { width:80px; accent-color:%6$s; }
    .ms-reset { display:block; margin-top:6px; color:%6$s; }
    .map-legend { background:%8$s; color:%2$s; padding:6px 9px; border-radius:6px; font-size:12px; line-height:1.5; max-height:40vh; overflow:auto; border:1px solid %7$s; }
    .map-legend i { display:inline-block; width:14px; height:0; border-top:3px solid; margin-right:6px; vertical-align:middle; } .map-legend .lg-t { font-weight:600; margin-top:3px; }
    .map-note { padding:6px 10px; font-size:.85rem; color:%2$s; background:%1$s; border-bottom:1px solid %7$s; }
    .mt-sub { color:%3$s; font-size:.92em; } .lg-n { margin-top:4px; }
    .rs-fp-pick .shiny-options-group { display:grid; grid-template-columns:repeat(3, 1fr); border:1px solid %7$s; border-radius:4px; overflow:hidden; }
    .rs-fp-pick .radio-inline { margin:0 !important; padding:4px 0 !important; text-align:center; border-right:1px solid %7$s; font-size:13px; cursor:pointer; }
    .rs-fp-pick .radio-inline:last-child { border-right:0; } .rs-fp-pick input { position:absolute; opacity:0; }
    .rs-fp-pick .radio-inline:has(input:checked) { background:%7$s; color:%2$s; font-weight:600; } .rs-fp-pick .radio-inline:has(input:focus-visible) { outline:2px solid %6$s; outline-offset:-2px; }
    .rs-fp-sec { margin-top:10px; font-size:.72rem; letter-spacing:.06em; text-transform:uppercase; color:%3$s; border-top:1px solid %7$s; padding-top:6px; }
    .rs-fp-key { display:inline-block; width:18px; height:0; border-top:3px dotted; margin-left:8px; vertical-align:middle; }
    .rs-fp-bars { display:grid; gap:3px; margin-top:6px; font-size:.8rem; } .rs-fp-bar { display:grid; grid-template-columns:76px 1fr 52px; gap:6px; align-items:center; }
    .rs-fp-bar .tr { height:9px; background:%1$s; border-radius:2px; overflow:hidden; } .rs-fp-bar .tr div { height:100%%; } .rs-fp-bar span:last-child { text-align:right; font-variant-numeric:tabular-nums; }
    .bslib-value-box .value-box-value { font-size: 1.5rem; } .bslib-value-box { min-height: 0 !important; }",
    PAL$bg2, PAL$fg, PAL$fg4, PAL$gray, PAL$red, PAL$accent, PAL$bg3, PAL$bg1)) |>
  bs_add_rules(legible_css())
PAL_PLOT <- modifyList(PAL, list(bg = PAL$bg1, bg1 = PAL$bg2, bg2 = PAL$bg3))     # plots sit on the card colour
VB <- function(col = PAL$teal) value_box_theme(bg = PAL$bg1, fg = col)
