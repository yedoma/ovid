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
    .map-legend { background:%8$s; color:%2$s; padding:7px 11px; border-radius:6px; font-size:12.5px; line-height:1.6; max-height:40vh; overflow:auto; border:1px solid %7$s; }
    .map-legend i { display:inline-block; width:22px; height:0; border-top:3px solid; margin-right:8px; vertical-align:middle; } .map-legend .lg-t { font-weight:600; font-size:13px; margin-top:3px; }
    .map-legend .lg-k { display:flex; flex-wrap:wrap; gap:4px 12px; margin-top:6px; padding-top:5px; border-top:1px solid %7$s; color:%3$s; font-size:11.5px; }
    .map-legend .lg-k span { display:inline-flex; align-items:center; } .map-legend .lg-k i { width:16px; border-top-width:2px; margin-right:5px; border-color:%3$s; }
    .map-note { padding:6px 10px; font-size:.85rem; color:%2$s; background:%1$s; border-bottom:1px solid %7$s; }
    .mt-sub { color:%3$s; font-size:.92em; } .lg-n { margin-top:4px; }
    .rs-fp-pick .shiny-input-container { width:100%% !important; } .rs-fp-pick .shiny-options-group { display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:0; width:100%%; height:34px; border:1px solid %7$s; border-radius:6px; overflow:hidden; }
    .rs-fp-pick .radio-inline { display:flex !important; align-items:center; justify-content:center; margin:0 !important; padding:0 !important; border-right:1px solid %7$s; font-size:13.5px; font-weight:500; color:%3$s; cursor:pointer; }
    .rs-fp-pick .radio-inline:last-child { border-right:0; } .rs-fp-pick input { position:absolute; opacity:0; width:0; height:0; margin:0; }
    .rs-fp-pick .radio-inline:has(input:checked) { background:%6$s; color:%9$s !important; font-weight:600; } .rs-fp-pick .radio-inline:has(input:focus-visible) { outline:2px solid %6$s; outline-offset:-2px; }
    .rs-fp-h { display:flex; align-items:center; gap:8px; margin:8px 0 2px; font-size:14px; font-weight:600; }
    .rs-fp-sec { margin:14px 0 2px; font-size:11px; font-weight:600; letter-spacing:.07em; text-transform:uppercase; color:%4$s; }
    .rs-fp-key { display:inline-block; width:22px; height:0; border-top:3px dotted; }
    .rs-fp-f { display:grid; grid-template-columns:auto 1fr; column-gap:12px; padding:5px 0; border-bottom:1px solid %1$s; font-size:13.5px; }
    .rs-fp-f .k { color:%3$s; } .rs-fp-f .v { text-align:right; font-variant-numeric:tabular-nums; overflow-wrap:anywhere; } .rs-fp-f .v.nw { white-space:nowrap; }
    .rs-fp-f .r { grid-column:1 / -1; text-align:right; font-size:12px; color:%4$s; font-variant-numeric:tabular-nums; }
    .rs-fp-bars { display:grid; gap:4px; margin-top:8px; font-size:12.5px; } .rs-fp-bar { display:grid; grid-template-columns:72px 1fr 44px; gap:8px; align-items:center; } .rs-fp-bar > span:first-child { color:%3$s; }
    .rs-fp-bar .tr { height:8px; background:%1$s; border-radius:2px; overflow:hidden; } .rs-fp-bar .tr div { height:100%%; } .rs-fp-bar span:last-child { text-align:right; font-variant-numeric:tabular-nums; }
    .bslib-value-box > .card-body { margin-top:0 !important; } .bslib-value-box .value-box-area { justify-content:flex-start !important; padding:12px 14px !important; }
    .bslib-value-box .value-box-title { color:%3$s; font-size:12.5px; font-weight:500; margin:0 0 2px; } .bslib-value-box .value-box-value { font-size:26px; font-weight:500; line-height:1.25; font-variant-numeric:tabular-nums; }
    .bslib-value-box .muted { font-size:12px; color:%4$s; margin:2px 0 0; } .bslib-value-box { min-height: 0 !important; }",
    PAL$bg2, PAL$fg, PAL$fg4, PAL$gray, PAL$red, PAL$accent, PAL$bg3, PAL$bg1, PAL$bg)) |>   # the map controls: a second call, as sprintf takes at most 8192 characters
  bs_add_rules(sprintf("
    .leaflet-container.leaflet-container { font-family:inherit; font-size:12.5px; }
    .leaflet-container .leaflet-bar, .leaflet-container .leaflet-control-layers { border-width:1px; }
    .leaflet-container .leaflet-bar a, .leaflet-container.leaflet-touch .leaflet-control-layers-toggle, .leaflet-control-layers-toggle { width:30px; height:30px; line-height:30px; }
    .leaflet-control-layers-toggle { display:flex; align-items:center; justify-content:center; background-image:none !important; color:%1$s !important; }
    .leaflet-control-layers-toggle svg { width:16px; height:16px; fill:none; stroke:currentColor; stroke-width:1.8; stroke-linejoin:round; }
    body .d-focusable:has(.leaflet-container) > .d-focus-btn { top:10px; right:50px; width:32px; height:32px; box-sizing:border-box; border:1px solid %2$s; border-radius:4px; opacity:1; color:%1$s; font-size:13px; }
    .rs-tool, .map-style .ms-btn, .dmt > a { display:flex !important; align-items:center; gap:6px; min-width:84px; padding:0 9px !important; font-size:12.5px; font-weight:500; text-align:left !important; }
    .rs-tool svg, .map-style .ms-btn svg, .dmt > a svg { width:15px; height:15px; flex:none; fill:none; stroke:currentColor; stroke-width:1.7; stroke-linecap:round; stroke-linejoin:round; }
    .leaflet-top.leaflet-left .map-style { margin-bottom:0; border-bottom-left-radius:0; border-bottom-right-radius:0; }
    .leaflet-top.leaflet-left .map-style + .dmt { margin-top:0; border-top:0; border-top-left-radius:0; border-top-right-radius:0; }
    .map-style + .dmt > a:first-child { border-top-left-radius:0; border-top-right-radius:0; } .map-style .ms-btn { border-bottom:1px solid %2$s !important; }
    #rep_make { display:inline-flex; align-items:center; gap:6px; } #rep_make .action-icon { margin:0; }",
    PAL$fg, PAL$bg3)) |>
  bs_add_rules(legible_css())
PAL_PLOT <- modifyList(PAL, list(bg = PAL$bg1, bg1 = PAL$bg2, bg2 = PAL$bg3))     # plots sit on the card colour
VB <- function(col = PAL$teal) value_box_theme(bg = PAL$bg1, fg = col)
