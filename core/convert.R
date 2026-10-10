# Ovid/convert.R  --  Every unit and coordinate conversion, in one place: units (pressure, length, temperature, speed),
# latitude/longitude in decimal degrees or degrees-minutes-seconds, UTM, and earth-centred XYZ.
# Part of Ovid Core (shared). Sourced by every app; usable on its own (source this file and util.R).
#
# Unit factors: NIST SP 811 (Thompson and Taylor 2008), Appendix B. Transverse Mercator (UTM) series: Snyder (1987),
# Map Projections - A Working Manual, USGS Professional Paper 1395, pp. 61-64, on the WGS84 ellipsoid (NIMA TR8350.2, 2000).
# Earth-centred XYZ to latitude/longitude: Bowring's iteration (Bowring 1976, Survey Review 23(181)).

# ---- units ------------------------------------------------------------------------------------------------------------
# Each unit as a factor to the kind's base unit (kPa, m, m/s); temperature is converted apart (it has an offset).
UNITS <- data.table(
  unit = c("kPa", "Pa", "hPa", "mbar", "bar", "MPa", "psi", "inHg", "mmHg", "atm",
           "m", "cm", "mm", "km", "ft", "in",
           "m/s", "km/h", "kn", "mph",
           "degC", "degF", "K"),
  kind = c(rep("pressure", 10), rep("length", 6), rep("speed", 4), rep("temperature", 3)),
  to_base = c(1, 0.001, 0.1, 0.1, 100, 1000, 6.894757, 3.386389, 0.1333224, 101.325,
              1, 0.01, 0.001, 1000, 0.3048, 0.0254,
              1, 1 / 3.6, 0.5144444, 0.44704,
              NA, NA, NA))

# A unit as written in a logger file ("deg C", "°F", "Kpa", "inches Hg") -> the name in UNITS; NA when not known.
unit_name <- function(u) {
  x <- tolower(gsub("[[:space:]._]", "", enc2utf8(as.character(u))))
  x <- gsub("°|º|deg(rees?)?", "deg", x)
  alias <- c(c = "degC", degc = "degC", celsius = "degC", f = "degF", degf = "degF", fahrenheit = "degF", k = "K", kelvin = "K",
             kpa = "kPa", pa = "Pa", hpa = "hPa", mbar = "mbar", mb = "mbar", millibar = "mbar", bar = "bar", mpa = "MPa", psi = "psi",
             inhg = "inHg", incheshg = "inHg", mmhg = "mmHg", atm = "atm",
             m = "m", metres = "m", meters = "m", cm = "cm", mm = "mm", km = "km", ft = "ft", feet = "ft", `in` = "in", inches = "in",
             `m/s` = "m/s", mps = "m/s", `km/h` = "km/h", kph = "km/h", kn = "kn", knots = "kn", kt = "kn", mph = "mph")
  unname(alias[x])
}

# x in unit `from` -> unit `to`. Units are matched loosely (unit_name()); an unknown unit or two different kinds is an error.
convert_unit <- function(x, from, to) {
  f <- unit_name(from); t <- unit_name(to)
  if (is.na(f) || is.na(t)) stop("unknown unit: ", if (is.na(f)) from else to)
  kf <- UNITS$kind[UNITS$unit == f]; kt <- UNITS$kind[UNITS$unit == t]
  if (kf != kt) stop("cannot convert ", kf, " (", from, ") to ", kt, " (", to, ")")
  if (kf == "temperature") {
    deg_c <- switch(f, degC = x, degF = (x - 32) * 5 / 9, K = x - 273.15)
    return(switch(t, degC = deg_c, degF = deg_c * 9 / 5 + 32, K = deg_c + 273.15))
  }
  x * UNITS$to_base[UNITS$unit == f] / UNITS$to_base[UNITS$unit == t]
}

# ---- latitude and longitude -------------------------------------------------------------------------------------------
# A latitude or longitude written any common way -> decimal degrees: "60.1031", "-120.1082", "60,1031", "60 06 11 N",
# "61 deg 18'11.2\"N", "121° 18.5' W", "S 33 52". S and W are negative. NA when it cannot be read: minutes or
# seconds of 60 or more, or a value beyond 180 degrees.
dms_to_dd <- function(x) {
  x <- trim(as.character(x)); x[is.na(x)] <- ""
  vapply(x, function(v) {
    if (!nzchar(v)) return(NA_real_)
    n <- suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))   # plain decimal degrees
    if (!is.na(n)) return(if (abs(n) <= 180) n else NA_real_)
    neg <- grepl("[SW]\\s*$|^[SW]", v, ignore.case = TRUE) || startsWith(v, "-")
    p <- as.numeric(regmatches(v, gregexpr("[0-9]+(\\.[0-9]+)?", v))[[1]])
    if (!length(p) || length(p) > 3 || any(p[-1] >= 60)) return(NA_real_)
    d <- sum(p / c(1, 60, 3600)[seq_along(p)])
    if (d > 180) return(NA_real_)
    if (neg) -d else d
  }, 0, USE.NAMES = FALSE)
}

# Decimal degrees -> "61° 18' 11.2\" N". axis "lat" (N/S) or "lon" (E/W).
dd_to_dms <- function(x, axis = "lat", digits = 1) {
  h <- if (axis == "lat") ifelse(x < 0, "S", "N") else ifelse(x < 0, "W", "E")
  a <- abs(x); d <- floor(a); m <- floor((a - d) * 60); s <- round(((a - d) * 60 - m) * 60, digits)
  up <- s >= 60; s[up] <- 0; m[up] <- m[up] + 1; up <- m >= 60; m[up] <- 0; d[up] <- d[up] + 1
  out <- sprintf("%d° %d' %s\" %s", as.integer(d), as.integer(m), formatC(s, format = "f", digits = digits), h)
  out[is.na(x)] <- NA_character_
  out
}

# ---- UTM --------------------------------------------------------------------------------------------------------------
WGS84 <- list(a = 6378137, f = 1 / 298.257223563)
UTM_K0 <- 0.9996

# The standard UTM zone of a longitude (1 to 60).
utm_zone_of <- function(lon) as.integer(floor((lon + 180) / 6) %% 60 + 1)

# A zone written as 10, "10", "10N" or "10 S" -> list(zone, south). `south` is used only when the text does not say.
utm_parse_zone <- function(zone, south = FALSE) {
  z <- toupper(gsub("\\s", "", as.character(zone)))
  list(zone = suppressWarnings(as.integer(sub("[NS]$", "", z))), south = ifelse(grepl("S$", z), TRUE, ifelse(grepl("N$", z), FALSE, south)))
}

# NAD83(CSRS) UTM EPSG codes (zones 7 to 22) beside the WGS84 / NAD83 ones, which carry the zone in the code itself.
CSRS_UTM <- c(`3154` = 7, `3155` = 8, `3156` = 9, `3157` = 10, `2955` = 11, `2956` = 12, `2957` = 13, `3158` = 14, `3159` = 15,
              `3160` = 16, `2958` = 17, `2959` = 18, `2960` = 19, `2961` = 20, `2962` = 21, `22207` = 7, `22208` = 8, `22209` = 9,
              `22210` = 10, `22211` = 11, `22212` = 12)

# UTM zone (and south) from an EPSG code or a coordinate system's text; NULL if geographic or unknown.
utm_zone <- function(epsg = NA, text = "") {
  if (!is.na(epsg)) {
    e <- as.integer(epsg)
    if (e %/% 100 %in% c(326L, 269L)) return(list(zone = e %% 100, south = FALSE))
    if (e %/% 100 == 327L) return(list(zone = e %% 100, south = TRUE))
    if (as.character(e) %in% names(CSRS_UTM)) return(list(zone = CSRS_UTM[[as.character(e)]], south = FALSE))
  }
  m <- regmatches(text, regexpr("UTM[ _]?[Zz]one[ _]?([0-9]{1,2}) ?([NS])?", text))
  if (length(m)) return(list(zone = as.integer(sub(".*?([0-9]{1,2}).*", "\\1", m)), south = grepl("S$", m)))
  NULL
}

# Longitude, latitude (decimal degrees) -> UTM easting, northing (m), as a matrix with columns easting and northing.
# zone NA: each point's own standard zone (utm_zone_of). Snyder (1987) eqs. 8-9 to 8-15.
# A point outside UTM's range (80 S to 84 N, longitude within 180) gives NA.
ll_to_utm <- function(lon, lat, zone = NA, south = lat < 0) {
  a <- WGS84$a; f <- WGS84$f; e2 <- f * (2 - f); ep2 <- e2 / (1 - e2); k0 <- UTM_K0
  z <- utm_parse_zone(zone)$zone; if (length(z) == 1 && is.na(z)) z <- utm_zone_of(lon)
  phi <- lat * pi / 180; lam0 <- ((z - 1) * 6 - 180 + 3) * pi / 180
  N <- a / sqrt(1 - e2 * sin(phi)^2); T <- tan(phi)^2; C <- ep2 * cos(phi)^2; A <- cos(phi) * (lon * pi / 180 - lam0)
  M <- a * ((1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256) * phi - (3 * e2 / 8 + 3 * e2^2 / 32 + 45 * e2^3 / 1024) * sin(2 * phi) +
              (15 * e2^2 / 256 + 45 * e2^3 / 1024) * sin(4 * phi) - (35 * e2^3 / 3072) * sin(6 * phi))
  E <- k0 * N * (A + (1 - T + C) * A^3 / 6 + (5 - 18 * T + T^2 + 72 * C - 58 * ep2) * A^5 / 120) + 500000
  Nn <- k0 * (M + N * tan(phi) * (A^2 / 2 + (5 - T + 9 * C + 4 * C^2) * A^4 / 24 + (61 - 58 * T + T^2 + 600 * C - 330 * ep2) * A^6 / 720))
  Nn <- Nn + ifelse(south, 1e7, 0)
  out <- cbind(easting = E, northing = Nn)
  out[is.na(lat) | lat < -80 | lat > 84 | is.na(lon) | abs(lon) > 180, ] <- NA_real_
  out
}

# UTM easting, northing (m) -> latitude, longitude (decimal degrees), as a matrix with columns lat and lon.
# zone: 10, "10N" or "10S" (one for all points, or one per point). Snyder (1987) eqs. 8-17 to 8-25.
utm_to_ll <- function(e, n, zone, south = FALSE) {
  zz <- utm_parse_zone(zone, south); z <- zz$zone; s <- zz$south
  if (anyNA(z)) stop("no UTM zone: set the project's UTM zone in Settings")
  if (any(z < 1 | z > 60)) stop("UTM zone must be 1 to 60, not ", paste(unique(z[z < 1 | z > 60]), collapse = ", "))
  a <- WGS84$a; f <- WGS84$f; e2 <- f * (2 - f); ep2 <- e2 / (1 - e2); k0 <- UTM_K0; e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
  x <- e - 500000; y <- n - ifelse(s, 1e7, 0)        # s is one value or one per point
  mu <- y / k0 / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
  p1 <- mu + (3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) + (21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
    (151 * e1^3 / 96) * sin(6 * mu) + (1097 * e1^4 / 512) * sin(8 * mu)
  c1 <- ep2 * cos(p1)^2; t1 <- tan(p1)^2; n1 <- a / sqrt(1 - e2 * sin(p1)^2); r1 <- a * (1 - e2) / (1 - e2 * sin(p1)^2)^1.5
  d <- x / (n1 * k0)
  lat <- p1 - (n1 * tan(p1) / r1) * (d^2 / 2 - (5 + 3 * t1 + 10 * c1 - 4 * c1^2 - 9 * ep2) * d^4 / 24 +
                                      (61 + 90 * t1 + 298 * c1 + 45 * t1^2 - 252 * ep2 - 3 * c1^2) * d^6 / 720)
  lon <- (d - (1 + 2 * t1 + c1) * d^3 / 6 + (5 - 2 * c1 + 28 * t1 - 3 * c1^2 + 8 * ep2 + 24 * t1^2) * d^5 / 120) / cos(p1)
  cbind(lat = lat * 180 / pi, lon = (z - 1) * 6 - 180 + 3 + lon * 180 / pi)
}

# ---- earth-centred XYZ ------------------------------------------------------------------------------------------------
# WGS84 earth-centred X, Y, Z (m) -> c(lat, lon) in decimal degrees (for a RINEX header's approximate position).
ecef_to_ll <- function(p) {
  a <- WGS84$a; f <- WGS84$f; e2 <- f * (2 - f); r <- sqrt(p[1]^2 + p[2]^2); lat <- atan2(p[3], r * (1 - e2))
  for (i in 1:5) { n <- a / sqrt(1 - e2 * sin(lat)^2); lat <- atan2(p[3] + e2 * n * sin(lat), r) }
  c(lat, atan2(p[2], p[1])) * 180 / pi
}

# ---- numbers written as text ------------------------------------------------------------------------------------------
# A column of a raw file -> numbers. A blank cell or a missing-value word (NA, NAN, INF, NULL) becomes NA quietly. Any other
# cell that is not a number also becomes NA, and is counted, so the reader's file can be named on the problem list
# (unread_mark() puts the count on the reader's result; audit.R unread_report() reports it).
MISSING_WORDS <- c("na", "nan", "+nan", "-nan", "inf", "+inf", "-inf", "null", "none")
UNREAD <- new.env()
UNREAD$n <- 0L; UNREAD$example <- NA_character_
num_cells <- function(x) {
  x <- as.character(x); v <- suppressWarnings(as.numeric(x))   # the warning is replaced by the count below
  s <- trimws(x); bad <- is.na(v) & !is.na(s) & nzchar(s) & !tolower(s) %in% MISSING_WORDS
  if (any(bad)) { UNREAD$n <- UNREAD$n + sum(bad); if (is.na(UNREAD$example)) UNREAD$example <- s[bad][1] }
  v
}
# The count since the last call, then reset to 0.
unread_take <- function() { u <- list(n = UNREAD$n, example = UNREAD$example); UNREAD$n <- 0L; UNREAD$example <- NA_character_; u }
# A reader's result with the count on its meta (unreadable, unreadable_example), so a cached result keeps it.
unread_mark <- function(r) {
  u <- unread_take()
  if (u$n > 0 && is.list(r) && is.list(r$meta)) { r$meta$unreadable <- u$n; r$meta$unreadable_example <- u$example }
  r
}
