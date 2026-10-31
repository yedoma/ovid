# Ovid/theme.R  --  Colour schemes and the shared ggplot theme. Part of Ovid Core (shared).
# Sourced before each app's own R/ files; not meant to be run on its own.
#
# Each app takes its colours from the dark variant of one vim colour scheme listed on vimcolorschemes.com (M, 2026-10-05):
# CS = olive-crt (its full Neovim palette since 2026-10-06, olive_crt_palette() below), WLR = Nord (@nordtheme/vim, the same
# 16 colours as the iTerm2 Nord below), GT = soviet.nvim, GS = kanagawa.nvim (its default "wave" variant), RS = accent.vim
# (its default yellow accent). Dark only. The earlier iTerm2-Color-Schemes palettes stay below for reference. Each app builds
# its palette list (GB, NORD, MI, PAL) from its scheme, and its plots use theme_ovid(palette), so the five apps draw alike
# and differ only in colour.

# The colours as the schemes publish them: ansi[1..16] are ANSI 0 to 15 (black, red, green, yellow, blue,
# magenta, cyan, white, then the bright versions of each).
OVID_SCHEMES <- list(
  `Tomorrow Night` = list(bg = "#1d1f21", fg = "#c5c8c6", selection = "#373b41", cursor = "#c5c8c6",
    ansi = c("#000000", "#cc6666", "#b5bd68", "#f0c674", "#81a2be", "#b294bb", "#8abeb7", "#ffffff",
             "#000000", "#cc6666", "#b5bd68", "#f0c674", "#81a2be", "#b294bb", "#8abeb7", "#ffffff"),
    extra = c(current_line = "#282a2e", comment = "#969896", orange = "#de935f")),   # from the Tomorrow theme the scheme comes from
  Nord = list(bg = "#2e3440", fg = "#d8dee9", selection = "#eceff4", cursor = "#eceff4",
    ansi = c("#3b4252", "#bf616a", "#a3be8c", "#ebcb8b", "#81a1c1", "#b48ead", "#88c0d0", "#e5e9f0",
             "#4c566a", "#bf616a", "#a3be8c", "#ebcb8b", "#81a1c1", "#b48ead", "#8fbcbb", "#eceff4"),
    # nord0 to nord15 as arcticicestudio names them (used by WLR, nord_palette() below), with WLR's darker status-line ground and its grey
    named = c(nord0 = "#2E3440", nord1 = "#3B4252", nord2 = "#434C5E", nord3 = "#4C566A", nord4 = "#D8DEE9", nord5 = "#E5E9F0",
              nord6 = "#ECEFF4", nord7 = "#8FBCBB", nord8 = "#88C0D0", nord9 = "#81A1C1", nord10 = "#5E81AC", nord11 = "#BF616A",
              nord12 = "#D08770", nord13 = "#EBCB8B", nord14 = "#A3BE8C", nord15 = "#B48EAD", bg_hard = "#242933", gray = "#7B88A1")),
  `Squirrelsong Dark` = list(bg = "#352a21", fg = "#ad9c8b", selection = "#574131", cursor = "#ad9c8b",
    ansi = c("#352a21", "#ac493e", "#558240", "#ceb250", "#5993c2", "#7f61b3", "#4f9593", "#cfbaa5",
             "#6b503c", "#ce574a", "#719955", "#e2c358", "#63a2d6", "#9672d4", "#72aaa8", "#edd5be")),
  `Kanso Mist` = list(bg = "#22262d", fg = "#c5c9c7", selection = "#43464e", cursor = "#c5c9c7",      # used by GS
    ansi = c("#22262d", "#c4746e", "#8a9a7b", "#c4b28a", "#8ba4b0", "#a292a3", "#8ea4a2", "#a4a7a4",
             "#5c6066", "#e46876", "#87a987", "#e6c384", "#7fb4ca", "#938aa9", "#7aa89f", "#c5c9c7")),
  `Petrified` = list(bg = "#3a3031", fg = "#fff0dc", selection = "#625456", cursor = "#f0b65c",      # used by RS
    ansi = c("#5a4c4e", "#ee9676", "#82c794", "#e5c05c", "#80bdd2", "#cc9fc1", "#75ccc7", "#fff0dc",
             "#8c7f7a", "#f4aa8d", "#9bd4a7", "#efd17d", "#9dcddd", "#dab6cf", "#94dad5", "#fff9ed")),
  `Solarized Dark` = list(bg = "#002b36", fg = "#839496", selection = "#073642", cursor = "#839496",     # used by CS ("iTerm2 Solarized Dark")
    ansi = c("#073642", "#dc322f", "#859900", "#b58900", "#268bd2", "#d33682", "#2aa198", "#eee8d5",
             "#335e69", "#cb4b16", "#586e75", "#657b83", "#839496", "#6c71c4", "#93a1a1", "#fdf6e3")),
  Earthsong = list(bg = "#292520", fg = "#e5c7a9", selection = "#121418", cursor = "#f6f7ec",               # used by GT
    ansi = c("#121418", "#c94234", "#85c54c", "#f5ae2e", "#1398b9", "#d0633d", "#509552", "#e5c6aa",
             "#675f54", "#ff645a", "#98e036", "#e0d561", "#5fdaff", "#ff9269", "#84f088", "#f6f7ec")),
  Vague = list(bg = "#141415", fg = "#cdcdcd", selection = "#252530", cursor = "#cdcdcd",                   # used by GS
    ansi = c("#252530", "#d8647e", "#7fa563", "#f3be7c", "#6e94b2", "#bb9dbd", "#aeaed1", "#cdcdcd",
             "#606079", "#e08398", "#99b782", "#f5cb96", "#8ba9c1", "#c9b1ca", "#bebeda", "#d7d7d7")),
  # vimcolorschemes.com, dark variants (2026-10-05). ansi is the scheme's own terminal palette where it ships one; named holds
  # the scheme's own colour names, which the apps pick from.
  `olive-crt` = list(bg = "#1D211A", fg = "#FBFCF6", selection = "#616B5C", cursor = "#FBFCF7",              # used by CS (extras/ghostty/olive-crt-dark)
    ansi = c("#1D211A", "#EA938B", "#BCCD80", "#F7C775", "#90DDE4", "#DAC3ED", "#A5D6CB", "#FBFCF6",
             "#99A293", "#EA938B", "#ECF6AE", "#FBE09F", "#90DDE4", "#DAC3ED", "#A5D6CB", "#FBFCF7"),
    named = c(olive = "#BCCD80", lime = "#ECF6AE", red = "#EA938B", amber = "#F7C775", cyan = "#90DDE4", lilac = "#DAC3ED",
              mint = "#A5D6CB", muted = "#99A293"),
    # The Neovim palette (lua/olive_crt/palette.lua) with its own role names, which CS uses for the screen: bg is the editor,
    # bg_alt the side panels, bg_statusline the title and status bars, bg_visual the selection, border the pane separators
    # (groups/editor.lua). error_bg, warn_bg, info_bg and hint_bg sit behind red, amber, cyan and olive_bright text.
    roles = c(bg = "#171a15", bg_alt = "#232720", bg_float = "#292d26", bg_visual = "#363c34", bg_statusline = "#20241d", border = "#858d80",
              line = "#9aa393", fg = "#fbfcf6", fg_alt = "#edf0e6", fg_idle = "#d8dece", muted = "#c0c7b7", subtle = "#99a293", selection = "#616b5c",
              olive = "#bccd80", olive_bright = "#ecf6ae", amber = "#f7c775", sand = "#fbe09f", teal = "#a5d6cb", cyan = "#90dde4", rust = "#f5a764",
              red = "#ea938b", violet = "#dac3ed", error_bg = "#613330", warn_bg = "#61492f", info_bg = "#284d4b", hint_bg = "#345033")),
  kanagawa = list(bg = "#1F1F28", fg = "#DCD7BA", selection = "#2D4F67", cursor = "#C8C093",                 # used by GS (kanagawa.nvim, wave)
    ansi = c("#16161D", "#C34043", "#76946A", "#C0A36E", "#7E9CD8", "#957FB8", "#6A9589", "#C8C093",
             "#727169", "#E82424", "#98BB6C", "#E6C384", "#7FB4CA", "#938AA9", "#7AA89F", "#DCD7BA"),
    named = c(sumiInk4 = "#2A2A37", sumiInk5 = "#363646", sumiInk6 = "#54546D", fujiGray = "#727169", oldWhite = "#C8C093",
              crystalBlue = "#7E9CD8", springBlue = "#7FB4CA", lightBlue = "#A3D4D5", waveAqua2 = "#7AA89F", springGreen = "#98BB6C",
              carpYellow = "#E6C384", surimiOrange = "#FFA066", waveRed = "#E46876", sakuraPink = "#D27E99", oniViolet = "#957FB8")),
  `soviet-dark` = list(bg = "#292727", fg = "#D8CFC4", selection = "#554B54", cursor = "#E9DFD0",            # used by GT (soviet.nvim lua/soviet/palette.lua)
    ansi = c("#1F1D1D", "#E36A5B", "#65AA88", "#D1B078", "#5B93BC", "#C58F9D", "#45A39E", "#D8CFC4",
             "#746666", "#EF7565", "#CED08B", "#D29A69", "#7899A0", "#B28A75", "#A8AC7D", "#E9DFD0"),
    named = c(bg_dark = "#1F1D1D", bg_light = "#353232", line = "#403A3A", border = "#746666", muted = "#B7AAA3", comment = "#A3A187",
              red = "#E36A5B", burgundy = "#C58F9D", olive = "#A8AC7D", added = "#B8BA73", added_bright = "#CED08B", khaki = "#B1A178",
              brown = "#B28A75", brass = "#D1B078", ochre = "#D29A69", steel = "#7899A0", blue = "#5B93BC", teal = "#45A39E", string = "#65AA88")),
  accent = list(bg = "#282c34", fg = "#bcbfc4", selection = "#484c54", cursor = "#e5c07b",                   # used by RS (accent.vim, default yellow accent)
    ansi = c("#282c34", "#e06c75", "#98c379", "#e5c07b", "#61afe7", "#c688cd", "#56b6c2", "#bcbfc4",
             "#777777", "#e06c75", "#98c379", "#e5c07b", "#61afe7", "#c688cd", "#56b6c2", "#efefff"),
    named = c(bg_b1 = "#383c44", bg_b2 = "#484c54", fg_d1 = "#999999", fg_d2 = "#777777", red = "#e06c75", orange = "#ee9360",
              green = "#98c379", yellow = "#e5c07b", blue = "#61afe7", magenta = "#c688cd", cyan = "#56b6c2")),
  `Black Metal (Khold)` = list(bg = "#000000", fg = "#c1c1c1", selection = "#c1c1c1", cursor = "#c1c1c1",   # used by RS
    ansi = c("#000000", "#5f8787", "#eceee3", "#974b46", "#888888", "#999999", "#aaaaaa", "#c1c1c1",
             "#404040", "#5f8787", "#eceee3", "#974b46", "#888888", "#999999", "#aaaaaa", "#c1c1c1")))

# olive-crt as CS's palette, in the token names ovid_tokens() (shell.R) and the Ovid design system read. Grounds:
# bg_hard = status line (title bar, rail), bg = editor, bg1 = side panel (cards), bg2 = selection (dividers), bg3 = the
# theme's selection grey, line = its window separator. Text: fg, fg4 = muted, gray = subtle. Hues: green is ok (teal, its
# DiagnosticOk), yellow and gold warn (amber), blue info (cyan), red error, orange its rust, purple its violet, aqua and sand
# its sand, accent its olive, acc2 its bright olive. error_bg .. hint_bg sit behind red, amber, cyan and bright-olive text.
olive_crt_palette <- function() { r <- as.list(OVID_SCHEMES$`olive-crt`$roles)
  list(bg_hard = r$bg_statusline, bg = r$bg, bg1 = r$bg_alt, bg2 = r$bg_visual, bg3 = r$selection, bg_float = r$bg_float, line = r$border,
       gray = r$subtle, fg4 = r$muted, fg = r$fg, fg_alt = r$fg_alt,
       red = r$red, green = r$teal, yellow = r$amber, gold = r$amber, blue = r$cyan, cyan = r$cyan, teal = r$teal, purple = r$violet,
       aqua = r$sand, sand = r$sand, orange = r$rust, accent = r$olive, acc2 = r$olive_bright, olive = r$olive, lime = r$olive_bright,
       error_bg = r$error_bg, warn_bg = r$warn_bg, info_bg = r$info_bg, hint_bg = r$hint_bg) }

# Nord as WLR's palette: Polar Night grounds, Snow Storm text, Frost blues as accents, Aurora for data and warnings.
nord_palette <- function() { n <- as.list(OVID_SCHEMES$Nord$named)
  c(n[sprintf("nord%d", 0:15)], list(bg_hard = n$bg_hard, bg = n$nord0, bg1 = n$nord1, bg2 = n$nord2, bg3 = n$nord3, gray = n$gray,
    fg4 = n$nord4, fg = n$nord6, accent = n$nord8, blue = n$nord9, aqua = n$nord7, frost = n$nord8, deep = n$nord10, red = n$nord11,
    orange = n$nord12, yellow = n$nord13, green = n$nord14, purple = n$nord15)) }

# A colour at opacity a (0 to 1) as CSS rgba(), for plotly fills and legend grounds.
col_rgba <- function(col, a) { v <- grDevices::col2rgb(col)[, 1]; sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], format(a)) }

# A colour part way from a to b (t = 0 is a, 1 is b), for the in-between greys a scheme does not define.
mix_col <- function(a, b, t) {
  m <- (1 - t) * grDevices::col2rgb(a) + t * grDevices::col2rgb(b)
  grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
}

# The ggplot theme for every plot. pal needs bg, bg1, bg2, fg, fg4 and gray; the rest only change details.
theme_ovid <- function(pal, base_size = 13, title = pal$fg, subtitle = pal$fg4, border = pal$bg2,
                         legend = "top", strip_size = 0.92) {   # legends at the top (Plot_table_standards A8)
  theme_minimal(base_size = base_size) %+replace% theme(
    plot.background   = element_rect(fill = pal$bg, colour = NA),
    panel.background  = element_rect(fill = pal$bg, colour = NA),
    panel.grid.major  = element_line(colour = pal$bg1, linewidth = 0.35),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(fill = NA, colour = border, linewidth = 0.4),
    text              = element_text(colour = pal$fg),
    axis.text         = element_text(colour = pal$fg4),
    axis.title        = element_text(colour = pal$fg4),
    axis.ticks        = element_line(colour = pal$bg2),
    plot.title        = element_text(colour = title, face = "bold", hjust = 0, margin = margin(b = 6)),
    plot.subtitle     = element_text(colour = subtitle, hjust = 0, margin = margin(b = 8)),
    panel.spacing     = grid::unit(10, "pt"),
    plot.caption      = element_text(colour = pal$gray, hjust = 1),
    strip.background  = element_rect(fill = pal$bg1, colour = NA),
    strip.text        = element_text(colour = pal$fg, hjust = 0, face = "bold", size = rel(strip_size), margin = margin(4, 6, 4, 6)),
    legend.background = element_rect(fill = pal$bg, colour = NA),
    legend.key        = element_rect(fill = pal$bg, colour = NA),
    legend.text       = element_text(colour = pal$fg4),
    legend.title      = element_text(colour = pal$fg4),
    legend.position   = legend,
    plot.margin       = margin(12, 14, 10, 12))
}

# Discrete colour and fill scales that cycle through a list of data colours.
scale_colour_ovid <- function(values, ...) scale_colour_manual(values = rep(values, 20), ...)
scale_fill_ovid   <- function(values, ...) scale_fill_manual(values = rep(values, 20), ...)

# ---- Inter -------------------------------------------------------------------------------------------------------------
# The Ovid type is Inter (SIL Open Font License, core/fonts/LICENSE.txt). The apps load it from core/fonts through a Shiny resource path;
# an HTML report embeds the two weights it uses (regular and semibold) so the file stays self-contained and works offline. Where the
# files are missing, the page falls back to the system sans-serif that follows Inter in every font stack. PDFs use the system fonts.
OVID_FONT_FILES <- c(`400` = "Inter-Regular.woff2", `500` = "Inter-Medium.woff2", `600` = "Inter-SemiBold.woff2", `700` = "Inter-Bold.woff2")
ovid_font_dir <- function() { d <- if (exists("CORE_DIR")) file.path(CORE_DIR, "fonts") else ""; if (nzchar(d) && dir.exists(d)) d else NULL }
ovid_font_css <- function(embed = NULL) {
  d <- ovid_font_dir(); if (is.null(d)) return("")
  w <- if (is.null(embed)) names(OVID_FONT_FILES) else intersect(embed, names(OVID_FONT_FILES)); w <- w[file.exists(file.path(d, OVID_FONT_FILES[w]))]
  paste0(vapply(w, function(k) { f <- file.path(d, OVID_FONT_FILES[[k]])
    src <- if (is.null(embed)) paste0("ovid-fonts/", OVID_FONT_FILES[[k]]) else paste0("data:font/woff2;base64,", base64enc::base64encode(f))
    sprintf("@font-face{font-family:'Inter';font-style:normal;font-weight:%s;font-display:swap;src:url(%s) format('woff2')}", k, src) }, ""), collapse = "\n")
}

# ---- Legibility ---------------------------------------------------------------------------------------------------
# WCAG contrast ratio between two colours (1 to 21). Text needs 4.5, lines and shapes in a chart need 3 (WCAG 1.4.3, 1.4.11).
contrast_ratio <- function(a, b) {
  lum <- function(h) { v <- grDevices::col2rgb(h)[, 1] / 255; v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(c(0.2126, 0.7152, 0.0722) * v) }
  l <- sort(c(lum(a), lum(b)), decreasing = TRUE); (l[1] + 0.05) / (l[2] + 0.05)
}

# The same colour made lighter (or darker, on a light background) until it reaches `min` contrast on every colour in
# `on`. Hue and saturation are kept, so a scheme's red stays its red.
legible_col <- function(col, on, min = 4.5) {
  ok <- function(x) all(vapply(on, function(b) contrast_ratio(x, b), 0) >= min)
  if (ok(col)) return(col)
  lab <- grDevices::convertColor(base::t(grDevices::col2rgb(col) / 255), from = "sRGB", to = "Lab")
  up <- mean(vapply(on, function(b) grDevices::convertColor(base::t(grDevices::col2rgb(b) / 255), from = "sRGB", to = "Lab")[1], 0)) < 50
  for (i in 1:200) { lab[1] <- lab[1] + if (up) 0.5 else -0.5
    r <- pmin(pmax(grDevices::convertColor(lab, from = "Lab", to = "sRGB"), 0), 1); x <- grDevices::rgb(r[1], r[2], r[3])
    if (ok(x) || lab[1] <= 0 || lab[1] >= 100) return(x) }
  col
}

# Lifts the named entries of a palette list to `min` contrast on `on` (default: the page background).
legible <- function(pal, keys, min = 4.5, on = pal$bg) { for (k in intersect(keys, names(pal))) pal[[k]] <- legible_col(pal[[k]], on, min); pal }

# ---- Parameter colours --------------------------------------------------------------------------------------------
# Every app colours a measurement by what it measures, the way weather services draw them (temperature red, humidity
# and dew point green, precipitation and water blue, wind purple, sunlight yellow), taken from the app's own scheme.
# First matching pattern wins, so the narrow ones come first.
PARAM_KINDS <- data.frame(stringsAsFactors = FALSE, kind = c(
  "battery", "panel_temp", "quality", "snow", "precip", "humidity", "wind_dir", "wind", "shortwave", "longwave",
  "heat_flux", "soil_water", "conductivity", "water", "pressure", "ground_temp", "temperature"), pattern = c(
  "batt|volt|^vbat",
  "panel_?t|ptemp|logger_?temp",
  "SR50.*Q|signal_quality|QualityVal|_QC$",
  "snow|SR50|^Dist_|swe",
  "^rain|precip|TBRG|^ppt|geonor|pluvio|RV_Pre",
  "^rh|rh$|^RH_|_RH|humid|dew|vapou?r",
  "wind_?dir|wnd_dir|^WD_",
  "wind|^WS_|wnd_spd|_ws[0-9]*(_|$)|gust",
  "^sw_|SWin|SWout|SWTop|SWBottom|SWUpper|SWLower|Shortwave|Slr|solar|CM3|^Rs_|Rs_net|NetRs|NetShort|PAR|albedo",
  "^lw_|LW|CG3|^Rl_|NetRl|NetLong|Longwave|net_?rad|NetR|^Rn_|NetTot|TotalNet|radiation",
  "HFT|hfp|shf|heat_flux",
  "vwc|WaterContent|_WC_|^VW_|moist|61[56]_?per|period",
  "^EC|_EC|soil_ec|^ec_|spc_|conduct",
  "^pres_|^dp_|depth_m|elev_m|dtw|stage|water_?l|^wl_|level",
  "baro|press|hPa|kpa|^BP_",
  "soil_?t|^T?10[79]|Temp10[79]|^TC_|T_C_|therm|ground|HumT|HolT|CS65.Temp",
  "temp|^t_|airt|tmpr|_T$|^T[0-9]*$"))

.or <- function(...) { for (x in list(...)) if (!is.null(x)) return(x); NULL }

param_kind <- function(x) vapply(as.character(x), function(p) {
  for (i in seq_len(nrow(PARAM_KINDS))) if (grepl(PARAM_KINDS$pattern[i], p, ignore.case = TRUE, perl = TRUE)) return(PARAM_KINDS$kind[i])
  "other" }, "", USE.NAMES = FALSE)

# One colour per kind from an app palette (needs bg, fg, fg4, gray, red, orange, yellow or gold, green, blue; aqua and
# purple are made up when a scheme has none). Every colour reaches 3:1 on the plot background.
kind_colours <- function(pal) {
  aqua <- .or(pal$aqua, pal$teal, mix_col(pal$blue, pal$green, 0.5))
  purple <- .or(pal$purple, mix_col(pal$red, pal$blue, 0.5)); pal$yellow <- .or(pal$yellow, pal$gold)
  k <- c(battery = pal$fg4, panel_temp = mix_col(pal$red, pal$fg, 0.5), quality = pal$gray, snow = mix_col(pal$blue, pal$fg, 0.55),
         precip = pal$blue, humidity = pal$green, wind_dir = mix_col(purple, pal$fg, 0.35), wind = purple, shortwave = pal$yellow,
         longwave = mix_col(pal$yellow, pal$red, 0.45), heat_flux = mix_col(pal$orange, pal$bg, 0.3), soil_water = aqua, conductivity = mix_col(pal$green, aqua, 0.45),
         water = .or(pal$water, pal$blue), pressure = mix_col(purple, pal$blue, 0.5), ground_temp = pal$orange,
         temperature = pal$red, other = pal$fg4)
  vapply(k, legible_col, "", on = pal$bg, min = 3)
}

# Colour-vision deficiency, simulated (Machado, Oliveira and Fernandes 2009, full severity), so colours shown together can
# be checked as about 1 in 12 men see them. Hue alone does not survive red-green colour blindness; lightness does.
CVD_MATRICES <- list(
  protan = matrix(c(0.152286, 1.052583, -0.204868, 0.114503, 0.786281, 0.099216, -0.003882, -0.048116, 1.051998), 3, byrow = TRUE),
  deutan = matrix(c(0.367322, 0.860646, -0.227968, 0.280085, 0.672501, 0.047413, -0.011820, 0.042940, 0.968881), 3, byrow = TRUE),
  tritan = matrix(c(1.255528, -0.076749, -0.178779, -0.078411, 0.930809, 0.147602, 0.004733, 0.691367, 0.303900), 3, byrow = TRUE))
cvd_simulate <- function(col, type = c("protan", "deutan", "tritan")) {
  lin <- function(v) ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  gam <- function(v) { v <- pmin(pmax(v, 0), 1); ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055) }
  g <- gam(CVD_MATRICES[[match.arg(type)]] %*% lin(grDevices::col2rgb(col) / 255)); grDevices::rgb(g[1, ], g[2, ], g[3, ])
}
# A colour as seen with normal vision and the three deficiencies, in Lab
.cvd_lab <- function(x) { lab <- function(h) grDevices::convertColor(t(grDevices::col2rgb(h) / 255), from = "sRGB", to = "Lab")[1, ]
  list(lab(x), lab(cvd_simulate(x, "protan")), lab(cvd_simulate(x, "deutan")), lab(cvd_simulate(x, "tritan"))) }
# How far apart two colours are, as a share of what is needed: 15 in Lab with normal vision, 10 with each deficiency
.cvd_sep <- function(a, b) min(mapply(function(x, y, need) sqrt(sum((x - y)^2)) / need, a, b, c(15, 10, 10, 10)))

# Colours shown together (lines in one plot, categories in one legend): each later colour that would look like an earlier
# one, with normal vision or with colour blindness, takes a lighter or darker shade of itself, so it keeps its meaning
# but can still be told apart. The first colour never changes; every colour stays at 3:1 on the background.
distinct_cols <- function(cols, pal) {
  steps <- c(0, 0.5, -0.45, 0.75, 0.25, -0.25, 0.6, -0.6, 0.35, 0.9)
  out <- cols; seen <- list()
  for (j in seq_along(cols)) { base <- cols[[j]]; best <- base; best_d <- -1
    for (t in steps) { x <- legible_col(if (t >= 0) mix_col(base, pal$fg, t) else mix_col(base, pal$bg, -t), pal$bg, 3); lx <- .cvd_lab(x)
      d <- if (length(seen)) min(vapply(seen, function(o) .cvd_sep(lx, o), 0)) else Inf
      if (d >= 1) { best <- x; best_l <- lx; break }
      if (d > best_d) { best <- x; best_l <- lx; best_d <- d } }
    out[[j]] <- best; seen[[j]] <- best_l }
  out
}

# Named colours for a set of parameters. When two would look alike (air temperature mean, minimum and maximum; air and
# panel temperature in one panel; wind and pressure to a colour-blind eye), the later one takes a lighter or darker shade
# of its own colour (distinct_cols above).
param_colours <- function(params, pal) {
  params <- as.character(params); kc <- kind_colours(pal)
  distinct_cols(setNames(unname(kc[param_kind(params)]), params), pal)
}

# A ramp from the plot background to a colour and on to a pale version of it, for heat maps of one parameter.
param_ramp <- function(col, pal, n = 64) grDevices::colorRampPalette(c(mix_col(col, pal$bg, 0.8), col, mix_col(col, pal$fg, 0.7)))(n)

# CSS added after each app's own page style: DataTables' page buttons drawn in its default dark grey (unreadable on a
# dark page), and coloured words on tinted badges, which need a lighter shade of their colour to reach 4.5:1.
# good and acc name the app's CSS variables for the "nothing lost" badge and its accent tag.
legible_css <- function(good = "--green", acc = "--acc") paste0(
  "body .dataTables_wrapper .dataTables_paginate .paginate_button,body .dataTables_wrapper .dataTables_paginate .paginate_button:active{color:var(--fg4) !important}",
  "body .dataTables_wrapper .dataTables_paginate .paginate_button.current,body .dataTables_wrapper .dataTables_paginate .paginate_button:hover{color:var(--fg) !important}",
  "body .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,body .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover{color:var(--gray) !important;background:transparent !important}",
  ".check.bad,.lost.yes{color:color-mix(in srgb, var(--red) 65%, var(--fg))}",
  ".lost.no{color:color-mix(in srgb, var(", good, ") 70%, var(--fg))}",
  ".tag{color:var(--fg)} .tag.acc{color:color-mix(in srgb, var(", acc, ") 40%, var(--fg))}",
  "body .dataTables_wrapper .page-item.disabled .page-link,body .dataTables_wrapper .page-link.disabled{color:color-mix(in srgb,var(--fg3) 60%,var(--fg)) !important}",   # Bootstrap pager: its 75 % grey
  "body .dataTables_wrapper .dataTables_processing{background:var(--bg2) !important;color:var(--fg) !important;border:1px solid var(--bg3)}")

# ---- Alternate colour schemes (Settings > Colours) -------------------------------------------------------------------
# What an app's Settings > Colours can offer besides the app's own scheme (each app's APP_SCHEMES; picked by M,
# 2026-10-08). Each is the dark variant of a scheme listed on vimcolorschemes.com, at its default contrast, as its own colour
# file sets it (src is the repository). Grounds: bg the editor, bg_hard the status line or darkest ground, bg1 to bg3
# lighter grounds (cursor line, float, selection), gray its comment colour. A hue a scheme leaves out is made up by
# scheme_palette() (see there).
OVID_ALT_SCHEMES <- list(
  Everforest = c(src = "sainnhe/everforest", bg = "#2d353b", bg_hard = "#232a2e", bg1 = "#343f44", bg2 = "#3d484d", bg3 = "#475258", fg = "#d3c6aa",
    gray = "#859289", red = "#e67e80", orange = "#e69875", yellow = "#dbbc7f", green = "#a7c080", aqua = "#83c092", blue = "#7fbbb3", purple = "#d699b6"),
  Werks = c(src = "vim-scripts/werks.vim", bg = "#333333", bg_hard = "#222222", bg2 = "#4D4D4D", fg = "#F5E8D3",
    gray = "#C2BFA5", red = "#CD5C5C", orange = "#FF4500", yellow = "#DAA520", green = "#8FBC8F", aqua = "#00FFFF", blue = "#708090", purple = "#C080D0"),
  `The Unix Zoo` = c(src = "vext01/theunixzoo-vim-colorscheme", bg = "#242429", bg_hard = "#19191e", bg2 = "#46464e", fg = "#d48130",
    gray = "#2E8B57", red = "#f5593f", orange = "#cb4b16", yellow = "#deb032", green = "#2E8B57", aqua = "#87CEEB", blue = "#ADD8E6", purple = "#330088"),
  Iceberg = c(src = "cocopon/iceberg.vim", bg = "#161821", bg_hard = "#0f1117", bg1 = "#1e2132", bg2 = "#272c42", bg3 = "#3d425b", fg = "#c6c8d1",
    gray = "#6b7089", red = "#e27878", orange = "#e2a478", yellow = "#e9b189", green = "#b4be82", aqua = "#89b8c2", blue = "#84a0c6", purple = "#a093c7"),
  Spacedust = c(src = "marcelbeumer/spacedust.vim", bg = "#05242a", bg_hard = "#041f26", bg1 = "#293739", bg2 = "#2c383b", bg3 = "#455354", fg = "#ECF0C1",
    gray = "#898a76", red = "#F92672", orange = "#FD971F", yellow = "#EBC562", green = "#A6E22E", aqua = "#66D9EF", blue = "#2daee0"),
  Aurora = c(src = "rafalbromirski/vim-aurora", bg = "#1b1b24", bg_hard = "#0b0b14", bg1 = "#262631", bg2 = "#3a3a47", fg = "#cfcfdd",
    gray = "#69697c", red = "#ff4040", orange = "#ff9326", yellow = "#ffcb65", green = "#9ceb4f", aqua = "#18ffe0", blue = "#31baff", purple = "#9d8cff"),
  Creature = c(src = "TaurusOlson/creature.vim", bg = "#191814", bg1 = "#262626", bg2 = "#403D3D", bg3 = "#4C4745", fg = "#D6EDE7",
    gray = "#777777", red = "#EB3443", orange = "#FF7A2A", blue = "#44AECF", purple = "#AE81FF"),
  Gruvbox = c(src = "morhetz/gruvbox", bg = "#282828", bg_hard = "#1d2021", bg1 = "#3c3836", bg2 = "#504945", bg3 = "#665c54", fg = "#ebdbb2",
    gray = "#928374", red = "#fb4934", orange = "#fe8019", yellow = "#fabd2f", green = "#b8bb26", aqua = "#8ec07c", blue = "#83a598", purple = "#d3869b"),
  `Birds of Paradise` = c(src = "ronny/birds-of-paradise.vim", bg = "#372725", bg1 = "#493a35", bg2 = "#6a5d53", fg = "#e6e1c4",
    gray = "#8f8475", red = "#ef5d32", orange = "#efac32", yellow = "#d9d762", green = "#49830c", aqua = "#7daf9c", blue = "#6c99bb", purple = "#8856d2"),
  Thornbird = c(src = "xiaody/thornbird.vim", bg = "#372725", bg1 = "#3c3d37", bg2 = "#49483e", bg3 = "#6a5d53", fg = "#e6e1c4",
    gray = "#75715e", red = "#ef5d32", orange = "#efac32", yellow = "#d9d762", aqua = "#7daf9c", blue = "#6c99bb", purple = "#8856d2"),
  Anderson = c(src = "tlhr/anderson.vim", bg = "#363432", bg3 = "#645d59", fg = "#c5beba",
    gray = "#837d7a", red = "#be503e", yellow = "#d7ae38", green = "#7bb292", aqua = "#88a2a4", blue = "#628b97"),
  Nightfox = c(src = "EdenEast/nightfox.nvim", bg = "#192330", bg_hard = "#131a24", bg1 = "#212e3f", bg2 = "#29394f", bg3 = "#39506d", fg = "#cdcecf",
    gray = "#738091", red = "#c94f6d", orange = "#f4a261", yellow = "#dbc074", green = "#81b29a", aqua = "#63cdcf", blue = "#719cd6", purple = "#9d79d6"),
  Dracula = c(src = "dracula/vim", bg = "#282A36", bg_hard = "#21222C", bg1 = "#343746", bg2 = "#424450", bg3 = "#44475A", fg = "#F8F8F2",
    gray = "#6272A4", red = "#FF5555", orange = "#FFB86C", yellow = "#F1FA8C", green = "#50FA7B", aqua = "#8BE9FD", purple = "#BD93F9"),
  Falcon = c(src = "fenetikm/falcon", bg = "#020221", bg_hard = "#000004", bg1 = "#151521", bg2 = "#212127", bg3 = "#36363A", fg = "#B4B4B9",
    gray = "#787882", red = "#FF3600", orange = "#FF761A", yellow = "#FFC552", green = "#859933", aqua = "#34BFA4", blue = "#99A4BC", purple = "#635196"),
  Miasma = c(src = "xero/miasma.nvim", bg = "#222222", bg_hard = "#111111", bg2 = "#444444", fg = "#D7C483",
    gray = "#666666", red = "#B36D43", orange = "#bb7744", yellow = "#C9A554", green = "#5f875f"),
  Afterglow = c(src = "danilo-augusto/vim-afterglow", bg = "#1a1a1a", bg2 = "#393939", bg3 = "#4d5057", fg = "#d6d6d6",
    gray = "#797979", red = "#ac4142", orange = "#e87d3e", yellow = "#e5b567", green = "#b4c973", blue = "#6c99bb", purple = "#9e86c8"),
  `Tomorrow Night` = c(src = "nonsense/tomorrow-night-vim-theme", bg = "#1d1f21", bg1 = "#282a2e", bg2 = "#373b41", bg3 = "#4d5057", fg = "#c5c8c6",
    gray = "#969896", red = "#cc6666", orange = "#de935f", yellow = "#f0c674", green = "#b5bd68", aqua = "#8abeb7", blue = "#81a2be", purple = "#b294bb"),
  Benlight = c(src = "kreeger/benlight", bg = "#141414", bg1 = "#1f1f1f", bg2 = "#2b2b2b", bg3 = "#565656", fg = "#f8f8f8",
    gray = "#868686", red = "#cf6a4c", orange = "#cda869", yellow = "#f9ee98", green = "#8f9d6a", blue = "#7587a6", purple = "#9b859d"))

# Hue (0 to 1), saturation and lightness of a colour, and back.
col_hsl <- function(col) { v <- grDevices::col2rgb(col)[, 1] / 255; mx <- max(v); mn <- min(v); l <- (mx + mn) / 2
  if (mx == mn) return(c(0, 0, l)); d <- mx - mn; s <- if (l > 0.5) d / (2 - mx - mn) else d / (mx + mn)
  h <- if (mx == v[1]) ((v[2] - v[3]) / d) %% 6 else if (mx == v[2]) (v[3] - v[1]) / d + 2 else (v[1] - v[2]) / d + 4
  c(h / 6, s, l) }
hsl_col <- function(h, s, l) { if (s == 0) return(grDevices::rgb(l, l, l)); q <- if (l < 0.5) l * (1 + s) else l + s - l * s; p <- 2 * l - q
  f <- function(t) { t <- t %% 1; if (t < 1 / 6) p + (q - p) * 6 * t else if (t < 1 / 2) q else if (t < 2 / 3) p + (q - p) * (2 / 3 - t) * 6 else p }
  grDevices::rgb(f(h + 1 / 3), f(h), f(h - 1 / 3)) }
.rel_lum <- function(h) { v <- grDevices::col2rgb(h)[, 1] / 255; v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(c(0.2126, 0.7152, 0.0722) * v) }
.SCHEME_HUES <- c(red = 0, orange = 28, yellow = 48, green = 100, aqua = 172, blue = 212, purple = 270)   # degrees, for made-up hues

# An app palette in one of OVID_ALT_SCHEMES. like is the app's own palette: every colour it names that the scheme has no
# role for takes the scheme's colour of the nearest hue, a grey keeps its place between ground and text, and a dark tint
# (contrast under 2 on the ground) stays a dark tint, so the app's code finds every name it uses. accent names the hue
# that is the app's accent in every scheme. A hue the scheme leaves out is made at that hue's angle with the scheme's
# average saturation and lightness; attr(, "made") lists them. Callers lift the colours to a readable contrast after
# (legible()), as they do for their own scheme.
scheme_palette <- function(name, like, accent) {
  s <- as.list(OVID_ALT_SCHEMES[[name]]); hues <- names(.SCHEME_HUES)
  have <- intersect(hues, names(s)); hl <- lapply(s[have], col_hsl)
  sat <- mean(vapply(hl, `[`, 0, 2)); lig <- mean(vapply(hl, `[`, 0, 3)); made <- setdiff(hues, have)
  for (k in made) s[[k]] <- hsl_col(.SCHEME_HUES[[k]] / 360, sat, lig)
  bg <- s$bg; fg <- s$fg
  p <- list(bg_hard = .or(s$bg_hard, mix_col(bg, "#000000", 0.22)), bg = bg, bg1 = .or(s$bg1, mix_col(bg, fg, 0.07)),
            bg2 = .or(s$bg2, mix_col(bg, fg, 0.14)), bg3 = .or(s$bg3, mix_col(bg, fg, 0.26)), gray = .or(s$gray, mix_col(fg, bg, 0.45)),
            fg4 = mix_col(fg, bg, 0.2), fg = fg, red = s$red, orange = s$orange, yellow = s$yellow, gold = s$yellow, green = s$green,
            aqua = s$aqua, teal = s$aqua, cyan = s$aqua, blue = s$blue, purple = s$purple)
  p$accent <- p[[accent]]
  hh <- vapply(p[hues], function(x) col_hsl(x)[1], 0); lb <- .rel_lum(like$bg); lf <- .rel_lum(like$fg)
  for (k in setdiff(names(like), names(p))) { col <- like[[k]]
    if (!is.character(col) || length(col) != 1L || !grepl("^#[0-9A-Fa-f]{6}$", col)) { p[[k]] <- col; next }
    h <- col_hsl(col)
    p[[k]] <- if (h[2] < 0.15) mix_col(p$bg, p$fg, min(1, max(0, (.rel_lum(col) - lb) / (lf - lb)))) else {
      d <- abs(hh - h[1]); near <- p[[hues[which.min(pmin(d, 1 - d))]]]
      if (contrast_ratio(col, like$bg) < 2) mix_col(near, p$bg, 0.7) else near } }
  attr(p, "made") <- made
  p
}

# The colour scheme an app was saved with (Settings > Colours: colour_scheme in app_settings.json beside the app), if it is
# one of choices; else choices[1], the app's own.
saved_scheme <- function(choices, dir = get0("PIPELINE_DIR", ifnotfound = getwd())) {
  # safe: no saved scheme means the app's own
  s <- tryCatch(settings_read(file.path(dir, "app_settings.json"))$colour_scheme, error = function(e) NULL)
  if (is.character(s) && length(s) == 1L && s %in% choices) s else choices[[1]]
}

# pal in the scheme called name: pal itself for the app's own scheme (choices[1]) or a name OVID_ALT_SCHEMES lacks.
scheme_pick <- function(pal, name, accent, choices)
  if (is.null(name) || identical(name, choices[[1]]) || is.null(OVID_ALT_SCHEMES[[name]])) pal else scheme_palette(name, pal, accent)
