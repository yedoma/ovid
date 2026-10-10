# Ovid/convert_page.R  --  The Convert page: units, latitude/longitude in decimal degrees or degrees-minutes-seconds,
# and UTM, for a value typed in. Part of Ovid Core (shared). UI only (CORE_UI_FILES in cache.R): the apps source it,
# no pipeline does. All the arithmetic is in core convert.R; this file only reads the inputs and shows the results.
#
#   convert_tool()                       the page (a ovid_tool with value "Conversions"; WLR already has a page "Convert"), before Reference in every menu
#   convert_server(input, output)        its results; the zone starts at the project's UTM_ZONE when the app has one

CV_KINDS <- c(pressure = "Pressure", length = "Length", speed = "Speed", temperature = "Temperature")
CV_LABEL <- c(degC = "°C", degF = "°F", K = "K")
CV_MSG <- list(ll = "Latitude must be within ±90 and longitude within ±180", range = "UTM covers 80° S to 84° N only",
               utm = "Type a zone (1 to 60), easting and northing", kind = "Pick two units of the same kind")

# The units of UNITS grouped by kind, for a select list.
cv_unit_choices <- function() {
  ch <- lapply(names(CV_KINDS), function(k) { u <- UNITS$unit[UNITS$kind == k]; setNames(u, ifelse(u %in% names(CV_LABEL), CV_LABEL[u], u)) })
  setNames(ch, CV_KINDS)
}

convert_tool <- function() {
  z <- get0("UTM_ZONE", ifnotfound = NULL)
  zone <- if (length(z) == 1 && !is.na(suppressWarnings(as.integer(z)))) paste0(as.integer(z), "N") else ""
  ovid_tool("Convert", value = "Conversions", icon = "right-left",
    ovid_head("Convert", about = p("Units, latitude and longitude, and UTM, for a value typed in. Nothing in the project is changed.")),
    div(class = "panel-x", h3("Units"),
      div(class = "btnrow",
        numericInput("cv_value", "Value", 1, width = "140px"),
        selectInput("cv_from", "From", cv_unit_choices(), selected = "psi", width = "140px"),
        selectInput("cv_to", "To", cv_unit_choices(), selected = "kPa", width = "140px")),
      uiOutput("cv_unit_out"),
      p(class = "dim", "Pressure, length, speed and temperature. Factors from NIST SP 811.")),
    div(class = "panel-x", h3("Coordinates"),
      radioButtons("cv_mode", NULL, c("Lat/long to UTM" = "ll2utm", "UTM to lat/long" = "utm2ll", "Decimal ↔ DMS" = "dms"), inline = TRUE),
      conditionalPanel("input.cv_mode != 'utm2ll'", div(class = "btnrow",
        textInput("cv_lat", "Latitude", "", placeholder = "60.1031 or 60 06 11 N", width = "220px"),
        textInput("cv_lon", "Longitude", "", placeholder = "-120.1082 or 120 06 30 W", width = "220px"))),
      conditionalPanel("input.cv_mode == 'utm2ll'", div(class = "btnrow",
        textInput("cv_zone", "Zone", zone, placeholder = "10N", width = "100px"),
        numericInput("cv_e", "Easting (m)", NA, width = "180px"),
        numericInput("cv_n", "Northing (m)", NA, width = "180px"))),
      uiOutput("cv_coord_out"),
      p(class = "dim", "Type decimal degrees or degrees-minutes-seconds; results show both. UTM on WGS84 (Snyder 1987).")))
}

convert_server <- function(input, output, session = NULL) {
  line <- function(label, value) div(tags$b(label), HTML("&nbsp;&nbsp;"), value)
  note <- function(msg) p(class = "dim", msg)
  num <- function(x) formatC(x, format = "f", digits = 2, big.mark = "")

  output$cv_unit_out <- renderUI({
    x <- input$cv_value; f <- input$cv_from; t <- input$cv_to
    if (is.null(x) || is.na(x) || is.null(f) || is.null(t)) return(NULL)
    kf <- UNITS$kind[UNITS$unit == f]; kt <- UNITS$kind[UNITS$unit == t]
    if (!identical(kf, kt)) return(note(CV_MSG$kind))
    lab <- function(u) if (u %in% names(CV_LABEL)) CV_LABEL[[u]] else u
    line("Result", paste(format(x), lab(f), "=", format(signif(convert_unit(x, f, t), 7)), lab(t)))
  })

  output$cv_coord_out <- renderUI({
    mode <- input$cv_mode %||% "ll2utm"
    both <- function(lat, lon) tagList(line("Decimal", sprintf("%.6f, %.6f", lat, lon)),
                                       line("DMS", paste0(dd_to_dms(lat, "lat"), ", ", dd_to_dms(lon, "lon"))))
    if (mode == "utm2ll") {
      zt <- input$cv_zone %||% ""; e <- input$cv_e; n <- input$cv_n
      if (!nzchar(trimws(zt)) && (is.null(e) || is.na(e)) && (is.null(n) || is.na(n))) return(NULL)
      z <- utm_parse_zone(zt)
      if (is.na(z$zone) || z$zone < 1 || z$zone > 60 || is.null(e) || is.na(e) || is.null(n) || is.na(n)) return(note(CV_MSG$utm))
      ll <- utm_to_ll(e, n, z$zone, z$south)
      if (!is.finite(ll[1, "lat"]) || abs(ll[1, "lat"]) > 90) return(note(CV_MSG$ll))
      return(both(ll[1, "lat"], ll[1, "lon"]))
    }
    lt <- input$cv_lat %||% ""; ln <- input$cv_lon %||% ""
    if (!nzchar(trimws(lt)) && !nzchar(trimws(ln))) return(NULL)
    lat <- dms_to_dd(lt); lon <- dms_to_dd(ln)
    if (is.na(lat) || is.na(lon) || abs(lat) > 90 || abs(lon) > 180) return(note(CV_MSG$ll))
    if (mode == "dms") return(both(lat, lon))
    if (lat < -80 || lat > 84) return(tagList(both(lat, lon), note(CV_MSG$range)))
    u <- ll_to_utm(lon, lat)
    tagList(both(lat, lon),
            line(paste0("Zone ", utm_zone_of(lon), if (lat < 0) "S" else "N"), paste0("E ", num(u[1, "easting"]), "  N ", num(u[1, "northing"]))))
  })
}
