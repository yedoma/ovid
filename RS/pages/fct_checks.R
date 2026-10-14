# RS/pages/fct_checks.R  --  Where each flight's positions came from, how good they are, and its five checks, as plain
# functions (no Shiny; tests call them). Sourced by app.R. Placed and corrected are kept apart: a log (eMotion's geotags)
# says where each image is, not that the position is RTK or PPK.

CHECKS <- c("Images", "Clock", "Positions", "Accuracy", "Log")
CHECK_SYMBOL <- c(ok = "✓", look = "!", bad = "✕")
CORRECTED_STATES <- c("RTK corrected", "PPK corrected")

# One row per flight: images placed (placed_counts(): the image tags, or a flight log when it places more), where they came
# from, images with corrected (RTK or PPK) positions, and the five checks (ok, look or bad) with what each is based on.
# cal: the project's calibration table (config/calibration_options.csv), or NULL for the defaults.
flight_checks <- function(fl, cal = NULL) {
  if (!nrow(fl)) return(data.table(flight_id = character(), n_placed = integer(), placed_from = character(), placed_pct = numeric(), n_corrected = numeric(),
                                   chk_images = character(), chk_clock = character(), chk_positions = character(), chk_accuracy = character(), chk_log = character(), chk_why = character()))
  lim <- function(v) rs_calibration(cal, v)$number
  ok_pct <- lim("check_placed_ok_pct"); look_pct <- lim("check_placed_look_pct"); sa_max <- lim("check_standalone_max_m")
  num0 <- function(v) { x <- if (v %in% names(fl)) suppressWarnings(as.numeric(fl[[v]])) else rep(NA_real_, nrow(fl)); x }
  yes <- function(v) if (v %in% names(fl)) toupper(as.character(fl[[v]])) %in% c("TRUE", "1", "YES") else rep(FALSE, nrow(fl))
  n <- num0("n_images"); n[is.na(n)] <- 0
  pl <- placed_counts(fl); placed <- pl$n_placed; from <- pl$placed_from     # R/catalogue.R: the one definition, as the catalogue
  pct <- ifelse(n > 0, 100 * placed / n, NA_real_)
  st <- if ("positioning_status" %in% names(fl)) as.character(fl$positioning_status) else rep("unknown", nrow(fl)); st[is.na(st) | !nzchar(st)] <- "unknown"
  rtk <- num0("rtk_fixed_pct")
  corrected <- ifelse(st %in% CORRECTED_STATES, placed, ifelse(st == "partly RTK corrected" & !is.na(rtk), round(n * rtk / 100), 0))
  acc <- ifelse(is.finite(num0("acc_capture_m")), num0("acc_capture_m"), num0("acc_processed_m"))
  ts <- if ("time_source" %in% names(fl)) as.character(fl$time_source) else rep("", nrow(fl)); ts[is.na(ts)] <- ""
  c_images <- ifelse(n > 0, "ok", "bad")
  # a decoded flight log is a real clock (the drone's GNSS time), so a time from it is as good as a checked camera clock;
  # a flown track (KML) has times too, but is matched to the flight by area and name, so it is worth a look
  log_time <- grepl("flight log", ts, fixed = TRUE)
  c_clock <- ifelse(yes("clock_ok") | log_time, "ok", ifelse(grepl("KML", ts, fixed = TRUE), "look", "bad"))
  c_pos <- ifelse(is.na(pct), "bad", ifelse(pct >= ok_pct, "ok", ifelse(pct >= look_pct, "look", "bad")))
  c_acc <- ifelse(st %in% CORRECTED_STATES, "ok", ifelse(st %in% c("partly RTK corrected", "correction data available, not confirmed"), "look",
           ifelse(st == "standalone GNSS / uncorrected", ifelse(is.finite(acc) & acc > sa_max, "bad", "look"), "bad")))
  c_log <- ifelse(yes("has_log") | yes("has_mission") | yes("has_mrk") | yes("has_rpt") | yes("has_geotags"), "ok", "look")
  why <- paste0("Images: ", num_or(n), " read. Clock: ", ifelse(yes("clock_ok"), "checked", ifelse(log_time, "time from the flight log (GNSS time)", ifelse(nzchar(ts), paste("unusable; time from", ts), "unusable"))),
                ifelse(is.finite(num0("utc_offset_h")), paste0(" (UTC offset ", num0("utc_offset_h"), " h)"), ""),
                ". Positions: ", num_or(placed), " of ", num_or(n), " placed, from ", from, " (ok at ", ok_pct, " %, look at ", look_pct, " %)",
                ". Accuracy: ", st, ifelse(is.finite(acc), paste0(", ", acc, " m"), ""), " (corrected = RTK or PPK; standalone worse than ", sa_max, " m is bad)",
                ". Log: ", ifelse(c_log == "ok", "flight log, mission, MRK, RPT or geotag file found", "no flight log or mission file"), ".")
  # a photo set (a short run of photos, not a mapping flight) is checked for its images only
  ps <- if ("kind" %in% names(fl)) as.character(fl$kind) %in% "photo set" else rep(FALSE, nrow(fl))
  c_clock[ps] <- "ok"; c_pos[ps] <- "ok"; c_acc[ps] <- "ok"; c_log[ps] <- "ok"
  why[ps] <- paste0("Photo set: checked for its images only (clock, positions, accuracy and log apply to mapping flights). ", why[ps])
  data.table(flight_id = fl$flight_id, n_placed = placed, placed_from = from, placed_pct = round(pct, 1), n_corrected = corrected,
             chk_images = c_images, chk_clock = c_clock, chk_positions = c_pos, chk_accuracy = c_acc, chk_log = c_log, chk_why = why)
}
num_or <- function(x) format(x, big.mark = ",", trim = TRUE)

# The summary of the flights in view: flights, images, percent placed and percent corrected (of all images), and the flights
# with a check to look at.
checks_summary <- function(fl, ck) {
  n <- sum(suppressWarnings(as.numeric(fl$n_images)), na.rm = TRUE)
  bad <- checks_open(ck)
  list(flights = nrow(fl), images = n, placed_pct = if (n > 0) 100 * sum(ck$n_placed) / n else NA_real_,
       corrected_pct = if (n > 0) 100 * sum(ck$n_corrected) / n else NA_real_, n_log = sum(ck$n_placed[ck$placed_from == "flight log"]),
       to_look = sum(bad), not_placed = n - sum(ck$n_placed))
}

# TRUE per flight with a check to look at (any of the five not ok): the summary's count and the Map's filter.
checks_open <- function(ck) ck[, chk_images != "ok" | chk_clock != "ok" | chk_positions != "ok" | chk_accuracy != "ok" | chk_log != "ok"]

# The five checks of a flight as symbols for a table cell (HTML), each with its name; colour and shape both say the level.
checks_html <- function(ck, pal = PAL) {
  col <- c(ok = pal$green, look = pal$gold, bad = pal$red)
  cols <- c("chk_images", "chk_clock", "chk_positions", "chk_accuracy", "chk_log")
  vapply(seq_len(nrow(ck)), function(i) paste0(vapply(seq_along(cols), function(j) { l <- ck[[cols[j]]][i]
    sprintf('<span title="%s: %s" style="color:%s;font-weight:700;margin-right:5px">%s</span>', CHECKS[j], l, col[[l]], CHECK_SYMBOL[[l]]) }, ""), collapse = ""), "")
}
