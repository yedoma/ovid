# docs/charts.R  --  The three illustration charts of the WLR reference guide (docs/img/depth_season.svg, baro_day.svg,
# monthly.svg), drawn from one constructed record so they agree with each other.
#
#   Rscript docs/charts.R          (from the WLR folder; base R only)
#
# The record is the reference guide's worked example ("One reading through the pipeline"), not field data: a logger installed
# on 23 August 2021 (tape check 1.250 m), a snowmelt peak on 1 May 2022, 1.2652 m at 12:00 on 15 July 2022, and a download on
# 17 November 2022. The monthly chart is computed from the same daily means with the app's completeness rule (a month is
# complete with at least 90 % of its days), and the barometric chart is that day's 6-hourly barologger readings (97.824 kPa
# at 07:00, 97.962 kPa at 13:00) interpolated to the water logger's half-hourly times.

OUT <- file.path("docs", "img")
source(file.path("..", "core", "theme.R"))                                       # the colours: WLR's Nord palette
N <- nord_palette()
C <- list(bg = N$bg, fg = N$nord5, mut = N$nord4, rule = N$bg3, grid = N$bg1, line = N$blue, acc = N$accent, dim = N$gray, mk = N$orange, amber = N$yellow)
FONT <- "font:12px -apple-system,'Segoe UI',Helvetica,Arial,sans-serif"

# ---- the constructed record --------------------------------------------------------------------------------------------
daily_record <- function() {
  set.seed(42)
  d <- seq(as.Date("2021-08-24"), as.Date("2022-11-17"), by = "day"); n <- length(d); x <- as.numeric(d - d[1])   # daily means from the first full day
  peak <- as.numeric(as.Date("2022-05-01") - d[1])
  rise <- 1.25 + 0.49 * pmin(1, x / peak)^1.8                                    # slow winter rise to the melt peak
  fall <- 1.17 + (1.74 - 1.17) * exp(-(x - peak) / 41.9)                         # recession: 1.265 m on 15 July
  v <- ifelse(x <= peak, rise, fall) + stats::filter(rnorm(n, 0, 0.005), rep(1 / 3, 3), circular = TRUE)
  data.frame(date = d, depth_m = round(as.numeric(v), 3))
}
monthly_of <- function(r) {
  m <- format(r$date, "%Y-%m"); u <- unique(m)
  do.call(rbind, lapply(u, function(k) { v <- r$depth_m[m == k]; first <- as.Date(paste0(k, "-01"))
    days <- as.numeric(seq(first, by = "month", length.out = 2)[2] - first)
    data.frame(month = first, mean = mean(v), min = min(v), max = max(v), complete = length(v) / days >= 0.9) }))
}

# ---- drawing -----------------------------------------------------------------------------------------------------------
svg_open <- function(w, h, label) sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" role="img" aria-label="%s" style="width:100%%;height:auto;%s"><rect width="%d" height="%d" style="fill:%s"/>', w, h, label, FONT, w, h, C$bg)
txt <- function(x, y, s, col = C$mut, anchor = "start", extra = "") sprintf('<text x="%.1f" y="%.1f" text-anchor="%s" style="fill:%s;%s">%s</text>', x, y, anchor, col, extra, s)
frame <- function(x0, x1, y0, y1, ylim, yticks, ylab, fmt = "%.1f") {
  sy <- function(v) y1 - (v - ylim[1]) / diff(ylim) * (y1 - y0)
  g <- vapply(yticks, function(t) paste0(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" style="stroke:%s"/>', x0, x1, sy(t), sy(t), C$grid),
                                          txt(x0 - 6, sy(t) + 4, sprintf(fmt, t), anchor = "end")), "")
  list(sy = sy, s = c(g, sprintf('<text transform="translate(16 %.1f) rotate(-90)" text-anchor="middle" style="fill:%s">%s</text>', (y0 + y1) / 2, C$mut, ylab)))
}
title_block <- function(t, sub) c(txt(60, 18, t, C$fg, extra = "font-weight:600;font-size:13px"), txt(60, 34, sub, C$mut, extra = "font-size:11px"))

depth_season <- function(r) {
  W <- 860; H <- 300; x0 <- 60; x1 <- 840; y0 <- 50; y1 <- 250
  f <- frame(x0, x1, y0, y1, c(1.0, 1.8), seq(1.0, 1.8, 0.2), "Water above sensor (m)")
  t <- as.numeric(r$date); sx <- function(v) x0 + (v - min(t)) / diff(range(t)) * (x1 - x0)
  pts <- paste(sprintf("%.1f,%.1f", sx(t), f$sy(r$depth_m)), collapse = " ")
  ticks <- seq(as.Date("2021-09-01"), as.Date("2022-11-01"), by = "2 months")
  xt <- vapply(ticks, function(d) txt(sx(as.numeric(d)), y1 + 18, format(d, "%b %Y"), anchor = "middle"), "")
  pk <- which.max(r$depth_m); wy <- sx(as.numeric(as.Date("2022-10-01")))
  c(svg_open(W, H, "Water above the sensor, daily mean (illustration)"),
    title_block("Water above the sensor, daily mean", "The worked example: a constructed record, not field data"),
    f$s, xt, sprintf('<polyline points="%s" style="fill:none;stroke:%s;stroke-width:2"/>', pts, C$line),
    sprintf('<line x1="%.1f" x2="%.1f" y1="%d" y2="%d" style="stroke:%s;stroke-dasharray:4 4"/>', wy, wy, y0, y1, C$mut),
    txt(wy - 6, y1 - 8, "water year 2022 ends (30 Sep)", C$mut, "end"),
    sprintf('<circle cx="%.1f" cy="%.1f" r="4" style="fill:%s"/>', sx(t[pk]), f$sy(r$depth_m[pk]), C$acc),
    txt(sx(t[pk]) + 8, f$sy(r$depth_m[pk]) + 4, sprintf("snowmelt peak %s: %.2f m", trimws(format(r$date[pk], "%e %b")), r$depth_m[pk]), C$fg),
    sprintf('<rect x="%.1f" y="%.1f" width="8" height="8" transform="rotate(45 %.1f %.1f)" style="fill:%s"/>', sx(t[1]) - 4, f$sy(1.25) - 4, sx(t[1]), f$sy(1.25), C$fg),
    txt(sx(t[1]) + 10, f$sy(1.25) + 18, "tape check at installation: 1.250 m", C$fg), "</svg>")
}

monthly <- function(m) {
  W <- 860; H <- 300; x0 <- 60; x1 <- 840; y0 <- 50; y1 <- 250
  f <- frame(x0, x1, y0, y1, c(1.0, 1.8), seq(1.0, 1.8, 0.2), "Water above sensor (m)")
  n <- nrow(m); bw <- (x1 - x0) / n; cx <- x0 + bw * (seq_len(n) - 0.5)
  bars <- vapply(seq_len(n), function(i) paste0(
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" style="%s"/>', cx[i] - bw * 0.35, f$sy(m$mean[i]), bw * 0.7, f$sy(1.0) - f$sy(m$mean[i]),
            if (m$complete[i]) paste0("fill:", C$line) else paste0("fill:none;stroke:", C$dim, ";stroke-width:1.5;stroke-dasharray:4 3")),
    sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" style="stroke:%s;stroke-width:1.5"/>', cx[i], cx[i], f$sy(m$min[i]), f$sy(m$max[i]), C$fg),
    txt(cx[i], y1 + 18, format(m$month[i], "%b"), anchor = "middle")), "")
  yrs <- unique(format(m$month, "%Y")); yx <- vapply(yrs, function(y) mean(cx[format(m$month, "%Y") == y]), 0)
  c(svg_open(W, H, "Monthly mean water above the sensor, minimum to maximum (illustration)"),
    title_block("Monthly mean, with the lowest to highest daily mean", "The worked example, by month"),
    sprintf('<rect x="520" y="10" width="12" height="10" style="fill:%s"/>', C$line), txt(538, 19, "complete month"),
    sprintf('<rect x="650" y="10" width="12" height="10" style="fill:none;stroke:%s;stroke-width:1.5;stroke-dasharray:3 2"/>', C$dim), txt(668, 19, "incomplete (under 90 % of days)"),
    f$s, bars, vapply(seq_along(yrs), function(i) txt(yx[i], y1 + 36, yrs[i], C$fg, "middle"), ""), "</svg>")
}

baro_day <- function() {
  W <- 860; H <- 300; x0 <- 60; x1 <- 840; y0 <- 50; y1 <- 240
  hr <- c(-5, 1, 7, 13, 19, 25); p <- c(97.495, 97.705, 97.824, 97.962, 97.920, 97.962)    # 6-hourly readings, the day before and after too
  f <- frame(x0, x1, y0, y1, c(97.6, 98.0), seq(97.6, 98.0, 0.1), "Barometric pressure (kPa)", "%.1f")
  sx <- function(h) x0 + h / 24 * (x1 - x0)
  wt <- seq(0, 24, 0.5); wp <- approx(hr, p, wt)$y; k <- which(wt == 12)                       # the water logger's times
  line <- paste(sprintf("%.1f,%.1f", sx(wt), f$sy(wp)), collapse = " ")
  xt <- vapply(seq(0, 24, 3), function(h) txt(sx(h), y1 + 18, sprintf("%02d:00", h), anchor = "middle"), "")
  rd <- which(hr >= 0 & hr <= 24)
  c(svg_open(W, H, "Barometric pressure on 15 July 2022, interpolated to the water logger's times (illustration)"),
    title_block("Barometric pressure, 15 July 2022", "The worked example: 6-hourly barologger readings, interpolated to each half-hourly water reading"),
    sprintf('<circle cx="560" cy="14" r="4" style="fill:%s"/>', C$fg), txt(570, 18, "barologger reading"),
    sprintf('<line x1="690" x2="712" y1="14" y2="14" style="stroke:%s;stroke-width:1.5;stroke-dasharray:4 3"/>', C$mut), txt(718, 18, "interpolated"),
    f$s, xt, sprintf('<polyline points="%s" style="fill:none;stroke:%s;stroke-width:1.5;stroke-dasharray:4 3"/>', line, C$mut),
    vapply(rd, function(i) sprintf('<circle cx="%.1f" cy="%.1f" r="4.5" style="fill:%s"/>', sx(hr[i]), f$sy(p[i]), C$fg), ""),
    vapply(rd, function(i) txt(sx(hr[i]), f$sy(p[i]) - 10, sprintf("%.3f", p[i]), C$mut, "middle", "font-size:11px"), ""),
    sprintf('<circle cx="%.1f" cy="%.1f" r="5.5" style="fill:none;stroke:%s;stroke-width:2"/>', sx(12), f$sy(wp[k]), C$mk),
    txt(sx(12) + 12, f$sy(wp[k]) + 26, sprintf("12:00 \u2192 %.3f kPa", wp[k]), C$fg, "start", "font-weight:600"), "</svg>")
}

if (sys.nframe() == 0) {
  r <- daily_record()
  writeLines(depth_season(r), file.path(OUT, "depth_season.svg"))
  writeLines(monthly(monthly_of(r)), file.path(OUT, "monthly.svg"))
  writeLines(baro_day(), file.path(OUT, "baro_day.svg"))
  cat("  docs/img/depth_season.svg, monthly.svg, baro_day.svg\n")
}
