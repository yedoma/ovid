# R/util.R  --  Helpers whose rules are GT's own: the palette, Excel times, spans, serial numbers, depth labels.
# Part of Ovid Core // GT. Sourced by gt_pipeline.R after the shared core (Ovid/util.R); not meant to be run on its own.


# soviet.nvim (vimcolorschemes, soviet-dark; see Ovid/theme.R): warm graphite grounds, book-paper text, enamel blue,
# turquoise and workshop olives and brass for data. The accent is its poster red. (The list keeps its old name, MI.)
# Settings > Colours offers these schemes (M, 2026-10-08; OVID_ALT_SCHEMES in core theme.R); the first is GT's own,
# and in the others the accent is the scheme's red. APP_SCHEME is the one picked (app_settings.json), read at the start.
APP_SCHEMES <- c("Soviet", "Creature", "Gruvbox", "Birds of Paradise", "Thornbird", "Anderson")
APP_SCHEME <- saved_scheme(APP_SCHEMES)
app_palette <- function(scheme = APP_SCHEMES[1]) {
  p <- local({ S <- OVID_SCHEMES$`soviet-dark`; n <- S$named
    list(bg_hard = n[["bg_dark"]], bg = S$bg, bg1 = n[["bg_light"]], bg2 = n[["line"]], bg3 = n[["border"]],
         gray = n[["muted"]], fg4 = mix_col(S$fg, n[["border"]], 0.3), fg = S$fg, sand = n[["khaki"]], gold = n[["brass"]], accent = n[["red"]], orange = n[["ochre"]],
         brown = n[["brown"]], green = n[["string"]], olive = n[["olive"]], lime = n[["added_bright"]], red = n[["red"]],
         blue = n[["blue"]], slate = n[["steel"]], teal = n[["teal"]], purple = n[["burgundy"]]) })
  # Muted text and the darkest line colours are lifted to a readable contrast on the brown background (Ovid/theme.R).
  p <- scheme_pick(p, scheme, "red", APP_SCHEMES)
  p <- legible(p, c("gray", "red", "green", "purple", "accent", "orange", "blue"), 4.5, on = c(p$bg, p$bg1)); p <- legible(p, c("brown", "slate", "olive"), 3)
  p }
MI <- app_palette(APP_SCHEME)
PAL <- MI                                            # console colours (say() in the shared core)

excel_time <- function(n) as.POSIXct(round(as.numeric(n) * 86400), origin = "1899-12-30", tz = "UTC")

# Serial numbers: RBR 000123 and the registry's 123 are one logger; GeoPrecision ids (E00001) are kept as letters and digits.

# Depth labels: 0.05 -> "0.05 m"; negative depths are above the ground: -0.5 -> "0.50 m above ground".
depth_lab <- function(z) ifelse(is.na(z), "?", ifelse(z < 0, sprintf("%.2f m above ground", round(-z, 3)), sprintf("%.2f m", round(z, 3))))

# Depths inside GT are metres below the ground surface (negative = above it). Depths typed in the set-up follow the project's
# depth_positive setting: "down" (the default) as GT's own, "up" when sensors above the surface (a chain in a casing or conduit
# that stands above the ground) are written positive and buried ones negative. depth_sign() turns a typed depth into GT's.
depth_sign <- function(cfg) if (identical(tolower(trim(as.character(cfg$depth_positive %||% "down"))), "up")) -1 else 1
# A typed depth in GT's convention. A leading '+' always means that far above the ground ("+0.5": 0.5 m above), whatever
# depth_positive is; other values follow depth_positive (sgn from depth_sign()). Typed depths are kept as text so the '+' stays.
typed_depth <- function(x, sgn = 1) { x <- trim(as.character(x)); v <- suppressWarnings(as.numeric(sub("^\\+", "", x)))
  ifelse(!is.na(x) & startsWith(x, "+"), -abs(v), sgn * v) }
depth_ok <- function(x) !is.na(typed_depth(x))
# The other way: a depth in GT's convention as the project writes it (what typed_depth() reads back), for rows the app writes
# into a set-up table and for the export.
depth_typed <- function(z, sgn = 1) ifelse(is.na(z), NA_real_, round(sgn * z, 4))
# Ground or above-ground: only sensors in the ground enter the ground-only results (thaw depth, ALT, ground state, gap fill, trends).
depth_position <- function(z) ifelse(is.na(z), NA_character_, ifelse(z < 0, "above ground", "ground"))
# The months of a water year that starts in month m: 10 -> "October to September".
wy_months <- function(m) { m <- suppressWarnings(as.integer(m)); if (!length(m) || is.na(m) || m < 1 || m > 12) m <- 10L
  if (m == 1L) "January to December" else paste(month.name[m], "to", month.name[m - 1L]) }
