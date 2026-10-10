# GT: the theme, the table status styles and what every audit rule means. Sourced by app.R.
# =============================================================================
#  Look: soviet.nvim, dark (warm graphite grounds, book-paper text, enamel and workshop colours; a poster-red accent). Colours: MI in R/util.R
# =============================================================================
mi_css <- paste0(sprintf(":root{--gold:%s;--sand:%s}", MI$gold, MI$sand), "
.step{display:flex;gap:14px;margin-bottom:16px} .step>.n{flex:0 0 28px;height:28px;border-radius:50%;background:var(--bg2);color:var(--acc);font-weight:600;display:flex;align-items:center;justify-content:center} .step>.body{flex:1;min-width:0}
.todo{list-style:none;padding:0;margin:0} .todo li{display:flex;align-items:baseline;gap:10px;padding:9px 0;border-top:1px solid var(--bg2)} .todo li:first-child{border-top:0}
.todo .k{flex:0 0 64px;text-align:right;font-weight:600;font-variant-numeric:tabular-nums;color:var(--gold)} .todo .ok{color:var(--green)} .todo a{margin-left:auto;white-space:nowrap}
.check{font-size:13.5px;margin:-6px 0 12px} .check.good{color:var(--green)} .check.bad{color:var(--red)}
code{background:var(--bg2);color:var(--sand);padding:1px 5px;border-radius:3px;font-size:13px}
.brushbox{background:var(--bg1);border-left:3px solid var(--acc);border-radius:8px;padding:14px 16px;margin:12px 0}
.scrollbox{max-height:560px;overflow-y:auto;border:1px solid var(--bg2);border-radius:8px} .scrollbox .shiny-plot-output img{border:0;border-radius:0}
.tag.ok{background:color-mix(in srgb, var(--green) 22%, transparent);color:var(--green)} .tag.warn{background:color-mix(in srgb, var(--gold) 18%, transparent);color:var(--gold)} .tag.bad{background:color-mix(in srgb, var(--red) 22%, transparent);color:var(--red)} .tag.acc{background:color-mix(in srgb, var(--acc) 20%, transparent);color:var(--acc)}
details.setup{margin:22px 0 6px;border:1px solid var(--bg2);border-radius:10px;padding:12px 16px} details.setup>summary{font-weight:600;color:var(--fg)} details.setup[open]>summary{margin-bottom:14px} details.setup .panel-x{margin-bottom:0}
.modebadge{font-size:13px;color:var(--bgh);background:var(--acc);border-radius:10px;padding:2px 10px;font-weight:600}
.explain{border-left:3px solid var(--acc);padding:2px 0 2px 12px;margin:10px 0;color:var(--fg);max-width:92ch}
.kv{display:grid;grid-template-columns:minmax(150px,240px) 1fr;gap:4px 16px;font-size:13.5px} .kv dt{color:var(--fg4);font-weight:500} .kv dd{margin:0;color:var(--fg)}
.kv dd small{color:var(--gray);margin-left:6px}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:14px}
.note{border-left:3px solid var(--gold);padding:8px 12px;background:var(--bgh);border-radius:4px;margin:6px 0;font-size:13.5px;white-space:pre-wrap}
.note b{color:var(--gold);margin-right:6px}
table.dataTable thead th:first-child{position:sticky;left:0;z-index:3;background:var(--bg1) !important}
table.dataTable tbody td:first-child{position:sticky;left:0;z-index:2;background:var(--bg) !important;box-shadow:inset -1px 0 0 var(--bg2) !important;font-weight:600}
table.dataTable tbody tr.selected>td:first-child{background:var(--bg2) !important}
.wrapcell{display:inline-block;min-width:300px;max-width:560px;white-space:normal}
table.dataTable tbody td input{background:var(--bgh);color:var(--fg);border:1px solid var(--acc);width:100%}
.dataTables_scrollHead{background:var(--bg1)} .dataTables_scrollBody thead th,.dataTables_scrollBody thead td{padding-top:0 !important;padding-bottom:0 !important;height:0 !important;line-height:0 !important;border:0 !important}
")

health_style <- function(dt, col) ovid_dt_levels(dt, col, c(ok = "ok", suspect = "warn", intermittent = "warn", `failing: mostly errors` = "error", `failed: nothing usable` = "error"), MI, default = "off")
status_style <- function(dt, col) ovid_dt_levels(dt, col, c(ok = "ok", needs_export = "error", unsupported = "error", duplicate_file = "off", export_not_used = "off", raw_not_used = "off", companion = "off",
  proprietary_not_readable = "off", excluded_by_user = "off", water_logger = "info", empty_file = "warn", unreadable = "error", unrecognised_format = "off", no_logger_type = "off"), MI, default = "warn")

RULE_INFO <- fread(sep = "|", quote = "", colClasses = "character", text = '
rule|title|lost|meaning|action|page
sensor_error|Error codes written by the logger|yes|The logger wrote an error instead of a reading: (NoReply) from a dead GeoPrecision node, a raw count out of range from an open or shorted RBR thermistor, 4094/4095 from an EM50 port, NAN or -7999 from a Campbell logger. The reason column gives the code.|Many on one sensor means it has failed: see Sensor health.|Health
missing_value|Blank readings|yes|The file has no value at this time (blank or NaN), usually a sensor that was not connected or a channel that is not logged.|Nothing to do.|Health
impossible_time|Readings with an impossible time|yes|Stamped before the first allowed year or in the future: a logger whose clock was not set, or was reset (GeoPrecision firmware resets write 1998).|Nothing to do; see Clock checks.|Clock
stray_record|Readings with corrupt time stamps|yes|A failing memory or clock stamped these at random times among good readings (off the time grid and out of written order), so they were left out.|See Clock checks.|Clock checks
clock_reset|Readings after a clock reset|yes|The clock jumped back by more than a day (a reset or a flat clock battery), so these readings cannot be placed in time.|If you know when the reset happened, a clock correction on the Clock checks page can move them back instead.|Clock
clock_resync|Readings superseded after the clock was set back|yes|The clock was set back by less than a day (for example when a new parameter set was sent); earlier readings carrying the same or later times are replaced by the later ones.|Nothing to do.|Clock
duplicate_value|Identical copies of a reading|no|The same reading is in more than one download (loggers that are not cleared carry every earlier season). One copy is kept.|Nothing to do.|
overlap_conflict|Readings that disagreed between downloads|kept|Two downloads of one logger had different values at the same time. The newest download is kept.|Many on one logger usually means a wrong time zone or two loggers sharing a serial: see Raw files and Clock checks.|Files
outside_deployment|Readings outside a deployment|yes|Before the start or after the end date of the sensor (Sensors and depths page): readings taken before installation or after removal.|Change the dates on the Sensors and depths page if these look wrong.|Sensors
site_overlap|Two loggers at one depth and time|kept|Two loggers of the same site and instrument type cover the same depth and time. The later deployment is kept.|Set end dates on the Sensors and depths page so deployments do not overlap.|Sensors
range_min|Values below a limit|yes|Below the minimum on the Quality rules page.|If real values are removed, lower the limit or set the rule to flag.|QC
range_max|Values above a limit|yes|Above the maximum on the Quality rules page.|If real values are removed, raise the limit or set the rule to flag.|QC
spike|Single-reading spikes|yes|One reading that jumps far more than the sensor usually moves and comes straight back.|Adjust max_spike on the Quality rules page.|QC
flat_line|Stuck readings|kept|The value did not change for longer than the limit (zero-curtain values are exempt).|Adjust max_flat_hours on the Quality rules page.|QC
manual|Values removed by hand|yes|Someone dragged across a plot, or added a removal under Corrections over a period, with the reason shown.|Review under Quality rules.|QC
not_a_sensor|Channels that are not sensors|yes|A channel that is not temperature, moisture or conductivity (an unsupported RBR channel type).|Nothing to do.|
')
rule_info <- function(r) { i <- match(r, RULE_INFO$rule); out <- RULE_INFO[i]; out[is.na(i), `:=`(rule = r[is.na(i)], title = r[is.na(i)], lost = "kept", meaning = "", action = "", page = "")]; out }
kv <- function(...) { x <- list(...); x <- x[!vapply(x, function(v) is.null(v) || all(is.na(v)) || identical(v, ""), logical(1))]
  tags$dl(class = "kv", lapply(names(x), function(k) tagList(tags$dt(k), tags$dd(x[[k]])))) }
