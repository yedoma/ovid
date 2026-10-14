# GS: the theme, quality colours and the QC sliders. Sourced by app.R.
# ---- the theme ---------------------------------------------------------------------------------------------------------
THEME <- bs_theme(version = 5, bg = PAL$bg, fg = PAL$fg, primary = PAL$accent, secondary = PAL$bg2, success = PAL$green,
                  warning = PAL$gold, danger = PAL$red, info = PAL$teal, base_font = font_collection("Inter", "Avenir Next", "Segoe UI", "-apple-system", "Helvetica", "Arial", "sans-serif"), "border-radius" = "0.5rem") |>
  bs_add_rules(sprintf("
    /* type, sizes and the page shell come from Ovid/shell.R; these are GS's own */
    .form-label { color: %3$s !important; } .card-header { font-size: 13.5px; font-weight: 600; }
    table.dataTable { font-size: 13.5px; } table.dataTable thead th { color: %3$s; font-weight: 600; white-space: nowrap; }
    table.dataTable tbody td { font-variant-numeric: tabular-nums; }
    .navbar { border-bottom: 1px solid %1$s; }  .card { background: %2$s; border-color: %1$s; }
    .bslib-value-box .value-box-value { font-size: 1.6rem; }  .bslib-value-box .value-box-title { font-size: 13.5px; }
    table.dataTable tbody tr { background: transparent !important; }  table.dataTable td, table.dataTable th { white-space: nowrap; }
    .bslib-value-box { min-height: 0 !important; }  .bslib-value-box .value-box-area { padding: .8rem 1rem !important; }  table.dataTable tbody tr:hover { background: %1$s !important; cursor: pointer; }
    .dataTables_wrapper, .dataTables_wrapper .dataTables_filter input, .dataTables_info, .paginate_button { color: %4$s !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: %5$s; border-color: %5$s; color: %6$s; }
    .facts { display:grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); gap: 14px; }
    .fact-group { background:%6$s; border:1px solid %1$s; border-radius:8px; padding:10px 14px; }
    .fact-title { font-weight:600; color:%5$s; margin-bottom:6px; font-size:.8rem; text-transform:uppercase; letter-spacing:.05em; }
    .fact { display:flex; justify-content:space-between; gap:12px; padding:3px 0; border-bottom:1px dashed %1$s; font-size:.88rem; }
    .fact:last-child { border-bottom:none; } .fact .k { color:%3$s; } .fact .v { text-align:right; word-break:break-word; }
    .viewbar { display:flex; flex-wrap:wrap; gap:14px; align-items:flex-end; margin-bottom:6px; } .viewbar .form-group { margin-bottom:0; }
    .muted { color: %3$s; font-size: 13px; }  .tag { display:inline-block; padding:2px 8px; margin:2px 4px 2px 0; border-radius:6px; background:%1$s; font-size:.8rem; }
    .btnrow { display:flex; align-items:center; flex-wrap:wrap; gap:8px; margin:4px 0 8px; } .btnrow .push { margin-left:auto; } .dim { color: %3$s; font-size: 12.5px; }",
    PAL$bg2, PAL$bg1, PAL$fg4, PAL$fg, PAL$accent, PAL$bg))
VB <- function(fg = PAL$fg) value_box_theme(bg = PAL$bg1, fg = fg)

QUALITY_COL <- c(Good = PAL$green, Usable = PAL$teal, `Usable with caution` = PAL$gold, Poor = PAL$orange, Unusable = PAL$red,
                 Incomplete = PAL$purple, Unknown = PAL$gray)

# ---- QC sliders (units chosen so a linear slider is usable; scale converts back to the rule's units) ----------------------
SLIDERS <- data.table(
  group = c("Signal", "Signal", "Signal", "Signal", "Contact", "Checks", "Checks", "Checks", "Checks"),
  rule  = c("max_error_pct", "min_current_ma", "min_vp_mv", "min_abs_r_ohm", "max_contact_ohm", "outlier_mad", "reciprocal_pct", "moving_median", "bad_electrode"),
  label = c("Max repeat error (%)", "Min current (mA)", "Min voltage (mV)", "Min |V/I| (x 0.0001 ohm)", "Max contact resistance (kohm, A-B pair)",
            "Outlier limit (robust z)", "Max reciprocal error (%)", "Moving median, 5 points (% of log10 rho_a)", "Bad electrode (% of its measurements flagged)"),
  min = c(0, 0, 0, 0, 0, 1, 0, 1, 5), max = c(20, 100, 10, 10, 100, 10, 20, 30, 100), step = c(0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 0.5, 1),
  scale = c(1, 1, 1, 1e-4, 1000, 1, 1, 1, 1),
  flag = c("high error", "low current", "low voltage", "low voltage", "poor contact", "uncertain", "high error", "uncertain", "bad electrode"))
TOGGLES <- data.table(rule = c("negative_rhoa", "duplicates", "geometry", "negative_ip"), label = c("Flag negative apparent resistivity", "Flag repeated quadrupoles", "Flag bad geometry", "Flag negative chargeability (IP)"),
                      flag = c("negative apparent resistivity", "duplicate", "bad geometry", "negative chargeability"))
slider_row <- function(i) div(class = "d-flex align-items-start gap-2",
  div(style = "padding-top:28px", ovid_switch(paste0("on_", SLIDERS$rule[i]), NULL, FALSE)),
  div(class = "flex-grow-1", sliderInput(paste0("v_", SLIDERS$rule[i]),
    tagList(SLIDERS$label[i], " ", tooltip(icon("circle-info"), RULE_HELP[rule == SLIDERS$rule[i]]$why, placement = "right")),
    SLIDERS$min[i], SLIDERS$max[i], SLIDERS$min[i], step = SLIDERS$step[i], width = "100%")))
profile_rows <- function(qc, prof, type) qc[qc$profile == prof & qc$applies_to %in% c("all", "", type) & tolower(qc$enabled) %in% c("true", "1", "yes")]
