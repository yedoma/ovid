# R/config.R  --  Set-up tables: defaults, loading, column aliases, station assignment.
# Part of Ovid Core // CS. Sourced by cs_pipeline.R; not meant to be run on its own.

parse_time <- function(x) {
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  for (f in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
    todo <- is.na(out) & nzchar(x)
    if (any(todo)) out[todo] <- as.POSIXct(x[todo], tz = "UTC", format = f)
  }
  out
}

alias_base <- function() fread(text = '
pattern,parameter,logger_id,notes
^BattV(_Avg)?$|^batt_volt_Avg$,battery_v,,
^BattV_Min$|^Batt_volt_Min$,battery_v_min,,
^PTemp(_C)?(_Avg)?$|^panel_tmpr_Avg$,panel_temp,,
^AirTC_1_Avg$|^T_tmpr_rh$|^Bog_T$|^PT_T_Avg$|^AirTC_Avg$,air_temp,,primary air temperature
^AirTC_2_Avg$,air_temp_2,,
^AirTC_1_(Min|Max)$,air_temp_\\L\\1,,
^AirTC_2_(Min|Max)$,air_temp_2_\\L\\1,,
^AirTemp_(High|Low)_Avg$,air_temp_\\L\\1,,two-height profile
^AirTemp_(High|Low)_(Min|Max)$,air_temp_\\L\\1_\\2,,
^RH_1_Avg$|^RH_tmpr_rh$|^BogRH$|^PT_RH$|^RH$,rh,,
^RH_2_Avg$,rh_2,,
^RH_1_(Min|Max)$,rh_\\L\\1,,
^RH_2_(Min|Max)$,rh_2_\\L\\1,,
^RH_(High|Low)_Avg$,rh_\\L\\1,,
^RH_(High|Low)_(Min|Max)$,rh_\\L\\1_\\2,,
^WS_ms_S_WVT$|^wnd_spd_Avg$|^WS_ms_Avg$|^Bog_ws$|^WindSpeed_S_WVT$,wind_speed,,
^WS_ms_Max$|^WindSpeed_Max$,wind_speed_max,,
^WindDir_D1_WVT$|^wnd_dir_Unit_Vec$|^WindDir$|^WindDirection_D1_WVT$,wind_dir,,
^WindDir_SD1_WVT$|^wnd_dir_Std$|^WindDirection_SD1_WVT$,wind_dir_sd,,
^SWTop_Avg$|^CM3_up_Avg$|^Rs_incoming_Avg$|^SWin_Avg$|^ShortwaveUp_Avg$,sw_in,,up-facing sensor = incoming
^SWBottom_Avg$|^CM3_down_Avg$|^Rs_outgoing_Avg$|^SWout_Avg$|^ShortwaveDown_Avg$,sw_out,,
^LWTopC_Avg$|^CG3_upCor_Avg$|^Rl_incoming_Avg$|^LongwaveUpCorrected_Avg$,lw_in,,temperature-corrected
^LWBottomC_Avg$|^CG3_dnCor_Avg$|^Rl_outgoing_Avg$|^LongwaveDownCorrected_Avg$|^LW_out_corr_Avg$,lw_out,,temperature-corrected
^LWTop_Avg$|^CG3_up_Avg$|^Rl_incoming_meas_Avg$|^LongwaveUp_Avg$,lw_in_raw,,
^LWBottom_Avg$|^CG3_down_Avg$|^Rl_outgoing_meas_Avg$|^LongwaveDown_Avg$|^LW_out_Avg$,lw_out_raw,,
^CM3Up_Avg$,sw_in,,CNR1 named the CR1000 example-program way
^CM3Dn_Avg$,sw_out,,
^CG3UpCo_Avg$,lw_in,,temperature-corrected
^CG3DnCo_Avg$,lw_out,,temperature-corrected
^CG3Up_Avg$,lw_in_raw,,
^CG3Dn_Avg$,lw_out_raw,,
^Rn_Avg$|^NetTot_Avg$|^TotalNetRadiation_Avg$,net_rad,,
^Rs_net_Avg$|^NetRs_Avg$|^NetShortwave_Avg$,sw_net,,
^Rl_net_Avg$|^NetRl_Avg$|^NetLongwave_Avg$,lw_net,,
^albedo_Avg$,albedo,,
^SnowDepth_Avg$|^snow_depth_Avg$|^SR50Sd$|^SR50A_SnowDepth$,snow_depth,,
^Rain_mm_Tot$|^Rain_Tot$,rain,,
^RV_Pre_Accu_Tot$,precip,,
^VWC_([0-9]+)cm_Avg$,vwc_\\1cm,,
^TC_([0-9]+)cm_Avg$,soil_temp_\\1cm,,
^EC_([0-9]+)cm_Avg$,soil_ec_\\1cm,,
^Batt_V_AVG$|^BatteryV(_Avg)?$|^BattVolts(_Avg)?$,battery_v,,logger battery
^(BatteryV|BattV|BattVolts|Batt_V)_(Min|Max)$,battery_v_\\L\\2,,
^panel_temp_Avg$|^PanelT(_Avg)?$|^LoggerTemp(_Avg)?$,panel_temp,,logger panel temperature
^(PanelT|PTemp_C|PTemp|LoggerTemp|panel_temp)_(Min|Max)$,panel_temp_\\L\\2,,
^SR50A?_SnowDepth_Avg$,snow_depth,,SR50A sonic ranger
^SR50A?_SnowDepth_(Min|Max)$,snow_depth_\\L\\1,,
^ShortwaveUp_(Min|Max)$,sw_in_\\L\\1,,up-facing sensor = incoming
^ShortwaveDown_(Min|Max)$,sw_out_\\L\\1,,
^LongwaveUpCorrected_(Min|Max)$,lw_in_\\L\\1,,
^LongwaveDownCorrected_(Min|Max)$,lw_out_\\L\\1,,
^LongwaveUp_(Min|Max)$,lw_in_raw_\\L\\1,,
^LongwaveDown_(Min|Max)$,lw_out_raw_\\L\\1,,
^RsNet_Avg$,sw_net,,
^RlNet_Avg$,lw_net,,
^NetShortwave_(Min|Max)$,sw_net_\\L\\1,,
^NetLongwave_(Min|Max)$,lw_net_\\L\\1,,
^TotalNetRadiation_(Min|Max)$,net_rad_\\L\\1,,
^Albedo_(Min|Max)$,albedo_\\L\\1,,
^CNR[14]Temp_Avg$|^CNR[14]TC_Avg$|^cnr[14]_T_C_Avg$,radiometer_temp,,radiometer body temperature in deg C
^CNR[14]Temp_(Min|Max)$,radiometer_temp_\\L\\1,,
^wnd_spd$,wind_speed,,
^WS_ms$,wind_speed_sample,,instantaneous reading logged beside the average
^wnd_dir_compass$,wind_dir,,
^std_wnd_dir$,wind_dir_sd,,
^Precip_Tot$,precip,,tipping bucket total
^WindSpeed_(High|Low)_S_WVT$,wind_speed_\\L\\1,,two-height towers
^WindSpeed_(High|Low)_Max$,wind_speed_\\L\\1_max,,
^WindDirection_(High|Low)_D1_WVT$,wind_dir_\\L\\1,,
^WindDirection_(High|Low)_SD1_WVT$,wind_dir_\\L\\1_sd,,
^SR50A?_QualityVal$|^signal_quality_Avg$,snow_depth_quality,,sonic ranger quality number
^raw_distance_Avg$,snow_distance_raw,,sonic ranger distance before temperature correction
^SnowDepth_Max$,snow_depth_max,,
^Perm_([0-9]+)cm_Avg$,soil_permittivity_\\1cm,,
^RV_Tips_Tot$,rain_tips,,
^e_tmpr_rh$,vapour_pressure,,kPa
^e_sat_tmpr_rh$,sat_vapour_pressure,,kPa
^T_nr_Avg$,radiometer_temp_k,,radiometer body temperature in kelvin
^temp$,pluvio_temp,,temperature sensor of the OTT Pluvio
^WS_ms_Min$,wind_speed_min,,
^GN_F_AVG$,geonor_frequency,,
^windspd_AVG$,wind_speed,,
^Panel_T_AVG$,panel_temp,,
^Fen_T_AVG$,air_temp,,
^Fen_RH_AVG$,rh,,
^Fen_RnObs_AVG$,net_rad_raw,,
^Fen_RnCor_AVG$,net_rad,,wind-corrected
^Fen_wind_AVG$,wind_speed,,
^PT_wind_AVG$,wind_speed_2,,
^TBRG_TOT$|^Bog_TBRG_TOT$,rain_tips,,tipping bucket: whole numbers so these are tips and not mm
^Rain_tip_Tot$,rain_tips,,tips
^Rain_tip_co_Tot$,rain,,mm per tip applied by the logger
^615_water_AVG$,vwc,,CS615
^615_perid_AVG$,soil_period,,CS615 period
^appTar_T_AVG$,surface_temp_ir,,Apogee infrared radiometer: target temperature
^Sensor_T_AVG$,ir_sensor_body_temp,,Apogee infrared radiometer: body temperature
^DW0*([0-9]+)_AVG$,ground_temp_down_west_\\1cm,,depth in cm from the column name
^UW0*([0-9]+)_AVG$,ground_temp_up_west_\\1cm,,
^UE0*([0-9]+)_AVG$,ground_temp_up_east_\\1cm,,
^DE0*([0-9]+)_AVG$,ground_temp_down_east_\\1cm,,
^MID0*([0-9]+)_AVG$,ground_temp_mid_\\1cm,,
^SUp_Avg$,sw_in,,up-facing sensor = incoming
^SDn_Avg$,sw_out,,
^LUpCo_Avg$,lw_in,,
^LDnCo_Avg$,lw_out,,
^LUp_Avg$,lw_in_raw,,
^LDn_Avg$,lw_out_raw,,
^HumT_([0-9]+)_Avg$,soil_temp_hummock_\\1cm,,hummock profile; depth in cm on a doubling scale (2 4 8 16 32 64 128)
^HolT_([0-9]+)_Avg$,soil_temp_hollow_\\1cm,,hollow profile; depth in cm on a doubling scale
^HumSurfaceT_Avg$,surface_temp_hummock,,
^HolSurfaceT_Avg$,surface_temp_hollow,,
^HumSnowT_([0-9]+)_Avg$,snow_temp_hummock_\\1cm,,height above the hummock surface in cm
^HolSnowT_([0-9]+)_Avg$,snow_temp_hollow_\\1cm,,
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)(?:_?1)?(?:_Avg)?$,air_temp,,paired sensors (any name not listed above): sensor 1 or no number is the main one
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)(?:_?1)?_(Max|Min)$,air_temp_\\L\\1,,
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)_?([2-9])(?:_Avg)?$,air_temp_\\1,,second (third ...) sensor
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)_?([2-9])_(Max|Min)$,air_temp_\\1_\\L\\2,,
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)_?(High|Low|Hi|Lo|Upper|Lower)(?:_Avg)?$,air_temp_\\L\\1,,two heights
^(?:AirTC|AirT|Air_?Temp|Bog_T|T_?Air)_?(High|Low|Hi|Lo|Upper|Lower)_(Max|Min)$,air_temp_\\L\\1_\\2,,
^(?:RH|Bog_?RH|RelHum)(?:_?1)?(?:_Avg)?$,rh,,
^(?:RH|Bog_?RH|RelHum)(?:_?1)?_(Max|Min)$,rh_\\L\\1,,
^(?:RH|Bog_?RH|RelHum)_?([2-9])(?:_Avg)?$,rh_\\1,,
^(?:RH|Bog_?RH|RelHum)_?([2-9])_(Max|Min)$,rh_\\1_\\L\\2,,
^(?:RH|Bog_?RH|RelHum)_?(High|Low|Hi|Lo|Upper|Lower)(?:_Avg)?$,rh_\\L\\1,,
^(?:RH|Bog_?RH|RelHum)_?(High|Low|Hi|Lo|Upper|Lower)_(Max|Min)$,rh_\\L\\1_\\2,,
^(?:WS_ms|WindSpeed|Wind_Speed|Bog_ws|wnd_spd|AvgWS)(?:_?1)?(?:_Avg|_S_WVT)?$,wind_speed,,
^(?:WS_ms|WindSpeed|Wind_Speed|Bog_ws|wnd_spd)(?:_?1)?_(Max|Min)$,wind_speed_\\L\\1,,
^(?:WS_ms|WindSpeed|Wind_Speed|Bog_ws|wnd_spd)_?([2-9])(?:_Avg|_S_WVT)?$,wind_speed_\\1,,
^(?:WS_ms|WindSpeed|Wind_Speed|Bog_ws|wnd_spd)_?([2-9])_(Max|Min)$,wind_speed_\\1_\\L\\2,,
', sep = ",", colClasses = "character", na.strings = NULL)

# max_spike (a single point that jumps by more than this and straight back; removed unless action is flag) and
# max_step_per_hour (a change faster than this between two readings; only flagged) are starting values, set wide so they
# catch sensor glitches and not weather: a cold front, a cloud or a snowfall should pass. They follow the step and spike tests
# of Zahumensky (2004) and Estevez et al. (2011), with limits widened for 30-minute and hourly records. Tune them per site.
default_qc <- function() { q <- fread(text = '
pattern,min,max,max_spike,max_flat_hours,action,notes,max_step_per_hour
^air_temp.*,-60,50,10,12,,degC,10
^rh(_.*)?$,0,105,40,72,,percent,50
^battery_v.*,0,20,3,,,volts,3
^wind_speed.*,0,75,,48,,m/s,
^wind_dir$,0,360,,,,degrees,
^sw_(in|out)$,-25,1500,,,,W/m2,
^lw_(in|out)$,50,700,150,12,,W/m2,200
^net_rad$,-300,1200,,,,W/m2,
^snow_depth$,-0.25,5,0.2,,,metres,0.3
^soil_temp_.*,-40,50,8,,,degC,10
^(surface_temp|snow_temp).*,-60,60,15,,,degC,20
^temp_above_ground.*,-60,50,10,,,degC; a thermistor in the air or snow above the ground,10
^(ground_temp|soil_probe_temp).*,-50,50,5,,,degC; a 109 measures -50 to 70 and a loose wire reads far below or above,5
^ground_temp_107.*,-35,50,5,,,degC; the measurement range of the 107 probe,5
^vwc.*,0,1,0.15,,,m3/m3; a CS650/CS655 reports 0 to 1 and a loose wire gives values far outside it,0.3
', sep = ",", colClasses = "character", na.strings = NULL)[, applies_to := ""]
  qc_rule_ids(q, "Q")[, ..QC_COLS] }

# Fill blank max_spike and max_step_per_hour cells with the suggested values, for rows whose pattern and applies_to are those
# of a default row. Nothing already typed is changed. Used by "Fill in suggested limits" on the Quality rules page, since a
# project made before these defaults keeps its table as it is. Returns the table and the number of cells filled.
qc_fill_suggested <- function(q) {
  q <- ensure_cols(as.data.table(q), QC_COLS); dq <- default_qc(); n <- 0L
  key <- function(x) paste(trim(x$pattern), trim(x$applies_to %||% ""), sep = "\r")
  m <- match(key(q), key(dq))
  for (v in c("max_spike", "max_step_per_hour")) {
    cur <- trim(as.character(q[[v]])); cur[is.na(cur)] <- ""
    k <- which(!nzchar(cur) & !is.na(m) & nzchar(dq[[v]][m]))
    if (length(k)) { set(q, k, v, dq[[v]][m[k]]); n <- n + length(k) } }
  list(table = q, filled = n)
}

default_groups <- function() fread(text = '
pattern,group
^(Pitch|Roll|IntTemp|IntRH|SupVolt|ResFreq|Alert|ExtTemp|panel_tmpr_CDM.*|AM[0-9]+_T_AVG|Panel_T_AVG_[0-9]+|StationName|SiteName|SerialNumber|OSVersion|OSDate|ProgName|ProgSig(nature)?|RunSignature|StartTime|LithiumBattery|SkippedScan|SkippedSystemScan|VarOutOfBound|WatchdogErrors|Low[0-9]+VCount|PakBusAddress|CompileResults|CPUDriveFree|USRDriveFree|CardBytesFree|ScanRate|Slow_Scan_Rate|ApplyAndRestart|BusLoad|ModuleReportCount|ActiveModules|BuffErr|RxErrMax|TxErrMax|FrameErr|ModuleInfo.*|CPI_DEVICE.*|.*_Add|.*SerialNum(ber)?|.*Const|.*_sensitivity|InitialSnowDepth|SR50A?DistToGrnd)$,Logger housekeeping
^ground_temp|^soil_probe_temp|^surface_temp|^temp_above_ground,Ground temperature
^snow_temp,Snow
^soil_ec,Soil moisture
^radiometer_temp,Radiation
batt|^panel_temp|ptemp,Power
^rh|rh$|^RH_|humid,Relative humidity
^air_temp|AirT,Air temperature
soil_temp|^T10[79]|^10[79]_|Temp109|HumT|HolT|CS655Temp|^TC_|T_C_|therm,Ground temperature
vwc|WaterContent|^HP[0-9]_(WC|VC)|moist,Soil moisture
wind|^WS_|wnd,Wind
^sw_|^lw_|net_rad|albedo|^Rs_|^Rl_|CM3|CG3|CNR|NetR|Tot_Avg$|^Rn_|^SW|^LW,Radiation
snow|SR50,Snow
rain|precip|pluvio|geonor,Precipitation
', sep = ",", colClasses = "character", na.strings = NULL)

# Which group each parameter belongs to (first matching pattern wins).
param_group <- function(x, groups) vapply(fix_enc(x), function(p) {
  for (i in seq_len(nrow(groups)))
    if (nzchar(groups$pattern[i]) && grepl(groups$pattern[i], p, ignore.case = TRUE, perl = TRUE)) return(groups$group[i])
  "Other"
}, character(1), USE.NAMES = FALSE)

# A config table always comes back with the columns the pipeline needs, even
# if the file was made by an older version or edited by hand.
# Rows whose pattern is not a valid regular expression are left out and reported.
valid_patterns <- function(dt, what) {
  if (!nrow(dt)) return(dt)
  ok <- vapply(dt$pattern, function(p) nzchar(p) && !inherits(try(grepl(p, "x", perl = TRUE), silent = TRUE), "try-error"), logical(1))
  for (p in dt$pattern[!ok & nzchar(dt$pattern)]) problem(what, paste0("pattern '", p, "' is not a valid regular expression and was ignored"))
  dt[ok]
}

QC_COLS <- c("pattern", "min", "max", "max_spike", "max_flat_hours", "action", "notes", "applies_to", "max_step_per_hour", "rule_id")

SITE_COLS <- c("station", "latitude", "longitude", "elevation_m", "utc_offset_hours", "snow_sensor_height_m", "wind_height_m", "description")

FILE_COLS <- c("file", "table", "action", "station", "stream", "reason", "author")   # action: assign (to station / stream) or ignore

LOG_COLS <- c("station", "stream", "date", "type", "parameter", "note", "author")

LOG_TYPES <- c("site visit", "gauge serviced", "sensor replaced", "sensor moved", "sensor cleaned", "program changed", "data downloaded", "note")

TIME_COLS <- c("logger_id", "table", "source_file", "start", "end", "action", "shift_minutes", "reason", "author")

INFO_COLS <- c("pattern", "topic", "unit", "typical_min", "typical_max", "sensor_min", "sensor_max", "sensors", "about", "reasonable", "watch_for", "source")

# What each kind of measurement is and what values to expect. Sensor ranges and
# accuracies are from the Campbell manuals named in "source"; "reasonable" values
# are typical field values for a northern site and are meant to be edited.
default_parameter_info <- function() fread(sep = "~", colClasses = "character", na.strings = NULL, quote = "", text = '
pattern~topic~unit~typical_min~typical_max~sensor_min~sensor_max~sensors~about~reasonable~watch_for~source
batt~Battery voltage~V~11.5~14.8~9.6~16~Logger power input~Voltage of the battery that runs the logger. The logger works from 9.6 to 16 V.~12 to 14 V on a healthy solar-charged battery, with a daily cycle of a few tenths of a volt. Lowest just before sunrise in winter.~A slow decline over days means the panel or battery is failing. Below about 11 V measurements become unreliable and below 9.6 V the logger stops, which is when clocks reset and records go missing.~cr1000.pdf
panel_temp|ptemp~Logger panel temperature~deg C~-45~45~-55~85~Thermistor on the logger wiring panel~Temperature of the logger itself, used as the reference for thermocouples.~Follows air temperature but lags it and runs warmer in sun inside the enclosure.~Not an air temperature. Useful as a cross-check: if it disagrees wildly with air temperature one of them is wrong.~cr1000.pdf
^air_temp|AirT|Bog_T$|_airT|T_tmpr|PT_T~Air temperature~deg C~-50~35~-40~60~HMP45C (-40 to 60, about +/-0.2 to 0.5); HC2S3 (-50 to 100, +/-0.1 at 23 C)~Temperature of the air, measured inside a radiation shield.~Northern continental sites run from about -45 in winter to 30 or more in summer. Day to night swings of 10 to 15 degrees are normal; changes of more than a few degrees between 30-minute records are not.~Values pinned at exactly -40 or 60 are the end of the sensor output range, not real readings. A flat line means a failed probe or a wiring fault.~hmp45c.pdf; hc2-s3-l_man.pdf
^rh|rh$|^RH_|humid~Relative humidity~%~10~100~0~100~HMP45C (+/-2% up to 90% RH, +/-3% above); HC2S3 (+/-0.8% at 23 C)~Water vapour in the air as a percentage of saturation.~Usually 20 to 100%. Near 100% at night, in fog and over snow; lowest on warm afternoons.~Readings a little above 100 (to about 103) happen near saturation and are normally capped at 100 rather than removed. Values stuck at 0 or far above 100 mean a failed sensor. Accuracy drifts upward with age.~hmp45c.pdf; hc2-s3-l_man.pdf
wind_?dir|wnd_dir|WindDir~Wind direction~degrees~0~360~0~360~05103 Wind Monitor (+/-3 degrees, 355 degree electrical range)~Direction the wind is coming from, clockwise from north.~Any value from 0 to 360. Meaningless when wind speed is near zero.~The potentiometer has a 5 degree gap near north. Do not average directions as ordinary numbers; the app averages them as directions.~05103.pdf
wind|^WS_|wnd_spd|_ws[0-9]*(_|$)~Wind speed~m/s~0~30~0~100~05103 Wind Monitor (0 to 100 m/s, +/-0.3 m/s or 1%, starts turning at 1.0 m/s); WindSonic~Horizontal wind speed.~Mostly 0 to 10 m/s at a sheltered site, gusts to 20 or 30 in storms.~Long runs of exactly 0 in winter usually mean the propeller is iced or snow-loaded, not calm air. Speeds below the 1 m/s starting threshold read as 0.~05103.pdf; windsonic.pdf
sw_net|NetRs|Rs_net|NetShort~Net shortwave radiation~W/m2~-10~1000~-2000~2000~Calculated: incoming minus reflected shortwave~Sunlight absorbed by the surface.~Zero at night, up to about 800 at midday over dark vegetation, much less over snow.~Negative daytime values mean the reflected reading is larger than the incoming one: a covered upper dome or swapped channels.~cnr1.pdf; cnr4.pdf
lw_net|NetRl|Rl_net|NetLong~Net longwave radiation~W/m2~-200~50~-500~500~Calculated: incoming minus outgoing longwave~Longwave gained from the sky minus longwave lost by the surface.~About -100 under clear sky to near 0 under low cloud or fog. The CNR4 manual gives -100 as typical.~Large positive values are not physical and point to a body temperature fault.~cnr4.pdf
^sw_in|SWin|CM3_up|Rs_incoming|SWTop|SWUpper|ShortwaveUp|SlrW~Incoming shortwave (solar) radiation~W/m2~-10~1200~0~2000~CM3 and the pyranometers in the CNR1 / CNR4 (305 to 2800 nm; daily totals within +/-10%)~Sunlight reaching the up-facing pyranometer.~Zero at night. Up to about 500 under cloud and up to 1000 to 1300 at noon under clear summer sky (CNR1 manual, table A-1).~Night values of a few W/m2 below zero are a normal thermal offset. Sustained daytime zeros in summer mean snow, frost or dirt on the dome. Values above about 1400 are not physical.~cnr1.pdf; cnr4.pdf; cm3.pdf
^sw_out|SWout|CM3_down|Rs_outgoing|SWBottom|SWLower|ShortwaveDown~Reflected shortwave radiation~W/m2~-10~900~0~2000~Down-facing pyranometer of the CNR1 / CNR4~Sunlight reflected by the ground or snow.~0 to about 150 over vegetation and up to 400 on clear days (CNR1 manual); much higher over snow, where most of the incoming light is reflected.~Should never exceed incoming shortwave for long. If it does the upper dome is covered or the two channels are swapped.~cnr1.pdf; cnr4.pdf
albedo~Albedo~fraction~0~1~0~1~Calculated: reflected / incoming shortwave~Share of sunlight that the surface reflects.~About 0.1 to 0.2 over forest and wet peat, 0.2 to 0.3 over dry vegetation, 0.7 to 0.9 over fresh snow.~Only meaningful in daylight. At night and at low sun it divides by nearly zero and gives huge or negative numbers, so use midday values or daily totals.~cnr1.pdf
^lw_(in|out)_raw|CG3_(up|down)_|LW(Top|Bottom|Upper|Lower)_Avg|Rl_.*meas~Longwave radiation, raw thermopile signal~W/m2~-200~80~-250~250~Pyrgeometers (CG3) in the CNR1 / CNR4~The net longwave exchange between the sensor and what it faces, before the sensor body temperature is added back.~Up-facing: about -100 under clear sky to about 0 under low cloud. Down-facing: close to 0.~This is not the true longwave flux. Use the corrected columns (lw_in, lw_out) for energy balance work.~cnr1.pdf; cnr4.pdf
^lw_|LW|CG3.*Cor|Rl_|Longwave~Longwave radiation (temperature corrected)~W/m2~100~550~0~1000~Pyrgeometers in the CNR1 / CNR4 with the Pt-100 or thermistor body temperature~Thermal infrared radiation from the sky (in) and from the surface (out).~Incoming: about 150 in clear arctic winter to 400 or more under warm cloud. Outgoing: about 200 at -30 C to 450 or more at +25 C.~Outgoing longwave follows surface temperature closely; if it does not, the body temperature reading is wrong. A missing temperature reading makes both corrected columns missing.~cnr1.pdf; cnr4.pdf
net_rad|^Rn_|NetTot|TotalNet|NetR~Net radiation~W/m2~-200~900~-2000~2000~CNR1 / CNR4 (sum of four components); NR-Lite~All incoming minus all outgoing radiation.~About -100 on clear nights to 600 to 800 at midday in summer. Slightly negative through most of the winter.~Spikes of thousands of W/m2 come from one bad component, usually the body temperature.~cnr4.pdf; nr-lite2.pdf
HFT|hfp|shf|heat_flux~Ground heat flux~W/m2~-60~120~-100~100~HFT3 (+/-100 W/m2, better than 5%); HFP01 (+/-2000 W/m2)~Heat flowing into (positive) or out of the ground at the plate depth.~A few tens of W/m2 either way; larger in thawing peat in early summer.~Readings at the +/-100 limit of the HFT3 are clipped.~hft-3.pdf; hfp01.pdf
SR50.*Q|signal_quality|QualityVal|SR50_QC~Snow sensor quality number~none~152~300~152~600~SR50A, SnowVUE10~How clean the echo was for each snow depth reading.~152 to 210 is a good reading. 210 to 300 is reduced strength. 0 means no echo.~Above 300 the distance is uncertain; snowfall, blowing snow and a tilted or frosted sensor all raise it. Screen snow depth with this column.~sr50a_man.pdf; snowvue10.pdf
snow|SR50|Dist_~Snow depth / distance to surface~m~-0.05~2.5~0.5~10~SR50A (0.5 to 10 m, +/-1 cm or 0.4% of distance); SnowVUE10 (0.2%)~An ultrasonic sensor measures the distance down to the snow or ground. Depth is the bare-ground distance minus that.~Depth from 0 to about 1 to 1.5 m at boreal sites. Distance columns sit near the mounting height (1 to 3 m).~The speed of sound depends on air temperature, so an uncorrected reading drifts with the daily temperature cycle. Small negative depths in summer are vegetation growth or a slightly wrong bare-ground distance.~sr50a_man.pdf; snowvue10.pdf
^rain|TBRG|Rain_~Rainfall (tipping bucket)~mm per record~0~40~0~500~TE525 (1 tip resolution, +/-1% up to 50 mm/h, works from 0 to 50 C)~Rain collected in each recording interval.~Zero most of the time. A few mm per half hour in ordinary rain, 10 to 30 in a thunderstorm.~An unheated tipping bucket does not measure snow; winter totals are melt, not snowfall. Long dry spells in a wet season often mean a blocked funnel.~te525.pdf
precip|geonor|pluvio|RV_Pre~Precipitation (weighing gauge)~mm~0~1500~0~1500~Geonor T-200B (600 mm capacity, 0.05 mm sensitivity, 0.1% of full scale); OTT Pluvio~A weighing gauge reports the accumulated total in the bucket, rain and snow together.~Rises in steps during precipitation and is flat otherwise. Drops only when the bucket is emptied.~Wind and temperature make the total wobble by a few tenths of a mm. A sudden fall is servicing, not negative precipitation. Take differences to get amounts per interval.~t-200b_man CSC.pdf
615_perid|616_per|period~Soil moisture probe period~microseconds~14~42~14~42~CS615 / CS616~The raw signal from a water content reflectometer, before conversion to water content.~About 14 with the rods in air and about 42 in saturated soil.~Frozen soil reads like dry soil because ice does not respond like liquid water.~cs616.pdf
vwc|WaterContent|_WC_|^VW_|moist~Soil volumetric water content~m3/m3~0~0.95~0~1~CS616 (+/-2.5% with standard calibration); CS650 / CS655 (+/-3% typical); Stevens Hydra Probe~Volume of liquid water per volume of soil.~0.05 to 0.5 in mineral soil; 0.2 to 0.9 in peat. Drops sharply when the soil freezes and recovers at thaw.~Factory calibrations are for mineral soil and read wrong in peat without a soil-specific calibration. 7999 or NAN means the probe did not answer.~cs616.pdf; cs650.pdf; hydraprobe_man.pdf
^EC|_EC|soil_ec~Soil electrical conductivity~dS/m~0~3~0~8~CS650 / CS655 (+/-5% of reading + 0.05 dS/m); Hydra Probe~How well the wet soil conducts electricity, a measure of dissolved salts.~Close to 0 in peat and rain-fed soils, up to 1 to 2 in mineral soils.~Falls to zero when the soil freezes.~cs650.pdf
soil_temp|^T?10[79]|^10[79]_|Temp10[79]|HumT|HolT|CS65.Temp|TC_|T_C_|therm~Ground and soil temperature~deg C~-30~35~-50~70~107 thermistor (-35 to 50 C); 109 thermistor (-50 to 70 C); both about +/-0.1 to 0.2 C; CS650 (+/-0.1 to 0.5 C)~Temperature of the soil, peat or snow at a fixed depth.~Deep probes change slowly and stay near 0 C for weeks during freeze and thaw. Near-surface probes range from about -20 to 25.~A probe that tracks air temperature has been exposed by frost heave or thaw. Sudden jumps of tens of degrees, or values like -80, are broken leads or a failed channel.~107.pdf; 109.pdf; cs650.pdf
baro|press|hPa~Barometric pressure~hPa~930~1060~500~1100~CS100 / CS106 (500 to 1100 hPa, +/-0.3 hPa at 20 C)~Air pressure at the station.~Within about 30 hPa of the site average; roughly 1013 at sea level and about 12 hPa less per 100 m of elevation.~Steps of several hPa between records are not weather.~cs106.pdf
')

# Which row of the parameter guide applies to each parameter (first match), or NA.
param_info_row <- function(x, info) vapply(fix_enc(x), function(p) {
  for (i in seq_len(nrow(info))) if (grepl(info$pattern[i], p, ignore.case = TRUE, perl = TRUE)) return(i)
  NA_integer_
}, integer(1), USE.NAMES = FALSE)

STATION_COLS <- c("logger_id", "header_name", "table", "start", "end", "station", "stream", "seen_as", "notes")

# The shared deployment sheet names the station of a logger that stations.csv leaves without one: one row per period (each its
# own start and end). A logger stations.csv names keeps that name; the sheet is never written into stations.csv. Only a row
# matched by its dates is applied (the core rule, deployments.R dep_match()): a row with no dates, or a From that is only a year
# or a month, is a suggestion on Needs you and is applied once it is given its dates in the sheet.
cs_deploy_dated <- function(dr) (!is.na(dr$.from) | !is.na(dr$.to)) & !dr$.from_prec %in% c("year", "month")
cs_deploy_stations <- function(st, dr) {
  if (is.null(dr) || !nrow(dr) || !nrow(st)) return(st)
  dr <- dr[nzchar(dep_where(dr))]; if (!nrow(dr)) return(st)
  free <- st[!nzchar(station)]$logger_id; sg <- dr[!cs_deploy_dated(dr) & .serial %in% dep_serial(free)]
  if (nrow(sg) && exists("problem", mode = "function")) problem("deployment sheet", paste0(nrow(sg), " row(s) (", paste(unique(sg$serial), collapse = ", "),
    ") have no dates, or only a year or a month, so they were not applied: they are suggestions on Needs you"))
  out <- list()
  for (i in seq_len(nrow(st))) { r <- st[i]
    if (nzchar(r$station)) { o <- dr[.serial == dep_serial(r$logger_id) & tolower(dep_where(dr)) != tolower(r$station)]      # typed in stations.csv: it wins, and says so
      if (nrow(o) && exists("problem", mode = "function")) problem("stations.csv and deployment sheet", paste0(r$logger_id, ": the deployment sheet gives the station '",
        paste(unique(dep_where(o)), collapse = "', '"), "', stations.csv '", r$station, "', which is used")) }
    hit <- if (nzchar(r$station)) dr[0] else dr[.serial == dep_serial(r$logger_id) & cs_deploy_dated(dr)]
    if (!nrow(hit)) { out[[i]] <- r; next }
    out[[i]] <- r[rep(1L, nrow(hit))][, `:=`(station = dep_where(hit), start = fifelse(nzchar(start), start, dep_day(hit$.from)),
                                            end = fifelse(nzchar(end), end, dep_day(hit$.to)), notes = fifelse(nzchar(notes), notes, "station from the deployment sheet"))] }
  rbindlist(out)
}

# CS reads its set-up tables its own way (it has no CONFIG_FILES list): cs_load_configs(), beside the core load_configs().
cs_load_configs <- function(cfg, loggers) {
  cdir <- file.path(cfg$project_dir, "config")
  config_migrate(cfg$project_dir, "pipeline")                                          # calibrations and manual removals into the corrections over a period (core config.R)
  f <- list(stations = file.path(cdir, "stations.csv"), aliases = file.path(cdir, "aliases.csv"),
            qc = file.path(cdir, "qc_rules.csv"),
            groups = file.path(cdir, "groups.csv"), time = file.path(cdir, "time_corrections.csv"),
            info = file.path(cdir, "parameter_info.csv"), gauges = file.path(cdir, "precip_gauges.csv"),
            sites = file.path(cdir, "station_info.csv"), log = file.path(cdir, "field_log.csv"), files = file.path(cdir, "file_rules.csv"),
            timezones = file.path(cdir, "timezones.csv"), composites = file.path(cdir, "composites.csv"), calib = file.path(cdir, "calibration_options.csv"),
            period = file.path(cdir, PERIOD_FILE))
  if (!file.exists(f$files)) fwrite(as.data.table(setNames(rep(list(character()), length(FILE_COLS)), FILE_COLS)), f$files)
  empty <- function(cols) as.data.table(setNames(rep(list(character()), length(cols)), cols))
  if (!file.exists(f$sites)) fwrite(empty(SITE_COLS), f$sites)
  if (!file.exists(f$log)) fwrite(empty(LOG_COLS), f$log)
  if (!file.exists(f$timezones)) fwrite(empty(TZ_COLS), f$timezones)
  if (!file.exists(f$composites)) fwrite(empty(COMPOSITE_COLS), f$composites)
  if (!file.exists(f$period)) fwrite(empty(c(PERIOD_COLS, CS_PERIOD_EXTRA)), f$period)  # corrections over a period (core periods.R)
  if (!file.exists(f$calib)) { fwrite(calibration_default(CS_CALIBRATION), f$calib); say("  created ", f$calib, col = "yellow") }
  # qc_rules.csv from an earlier version: add the new columns, with the default flat-line limits where the pattern is unchanged
  if (file.exists(f$qc)) { q0 <- read_cfg(f$qc)
    if (!"applies_to" %in% names(q0) && "max_flat_hours" %in% names(q0)) { q0[, applies_to := ""]; backup_cfg(f$qc, q0, "pipeline"); fwrite_safe(q0, f$qc); say("  qc_rules.csv: added the applies_to column", col = "yellow") }
    if (!"max_flat_hours" %in% names(q0)) { q0 <- ensure_cols(q0, QC_COLS); dq <- default_qc()
      q0[, max_flat_hours := fifelse(is.na(match(pattern, dq$pattern)), "", dq$max_flat_hours[match(pattern, dq$pattern)])]
      backup_cfg(f$qc, q0[, QC_COLS, with = FALSE], "pipeline"); fwrite_safe(q0[, QC_COLS, with = FALSE], f$qc); say("  qc_rules.csv: added the max_flat_hours and action columns", col = "yellow") } }
  if (!file.exists(f$gauges)) fwrite(as.data.table(setNames(rep(list(character()), length(PRECIP_COLS)), PRECIP_COLS)), f$gauges)
  if (!file.exists(f$time)) fwrite(as.data.table(setNames(rep(list(character()), length(TIME_COLS)), TIME_COLS)), f$time)
  if (!file.exists(f$info)) { fwrite(default_parameter_info(), f$info); say("  created ", f$info, col = "yellow") }
  if (!file.exists(f$groups)) { fwrite(default_groups(), f$groups); say("  created ", f$groups, col = "yellow") }
  if (!file.exists(f$aliases)) { fwrite(default_aliases(), f$aliases); say("  created ", f$aliases, col = "yellow") }
  if (!file.exists(f$qc))      { fwrite(default_qc(), f$qc);           say("  created ", f$qc, col = "yellow") }
  # stations.csv: every logger gets a row; new loggers are appended, your edits are kept
  st <- if (file.exists(f$stations)) read_cfg(f$stations) else data.table(logger_id = character())
  had <- names(st); st <- ensure_cols(st, STATION_COLS); setcolorder(st, STATION_COLS)
  new <- loggers[!logger_id %in% st$logger_id]
  if (nrow(new)) st <- ensure_cols(rbind(st, data.table(logger_id = new$logger_id, seen_as = new$seen_as), fill = TRUE), STATION_COLS)
  if (nrow(new) || !all(STATION_COLS %in% had)) { backup_cfg(f$stations, st, "pipeline"); fwrite_safe(st, f$stations) }
  if (nrow(new)) say("  stations.csv: added ", nrow(new), " new logger(s)", col = "yellow")
  if (!file.exists(dep_sheet_path(cfg$project_dir))) fwrite_safe(empty_cfg(DEPLOY_COLS), dep_sheet_path(cfg$project_dir))
  st <- cs_deploy_stations(st, dep_sheet(cfg$project_dir))           # the shared deployment sheet (core deployments.R)
  for (v in c("start", "end")) {
    bad <- nzchar(st[[v]]) & is.na(parse_time(st[[v]]))
    for (b in which(bad)) problem("stations.csv", paste0("'", st[[v]][b], "' is not a date (use YYYY-MM-DD); that ", v, " date was ignored"))
  }
  list(stations = st, calib = ensure_cols(read_cfg(f$calib), CALIBRATION_COLS),
       aliases = valid_patterns(ensure_cols(read_cfg(f$aliases), c("pattern", "parameter", "logger_id", "notes")), "aliases.csv"),
       qc      = valid_patterns(qc_read_rules(f$qc, QC_COLS, "Q"), "qc_rules.csv"),     # each row gets a lasting rule_id (core/qc.R)
       file_rules = ensure_cols(read_cfg(f$files), FILE_COLS),
       sites   = ensure_cols(read_cfg(f$sites), SITE_COLS), sites_file = f$sites, field_log = ensure_cols(read_cfg(f$log), LOG_COLS),
       manual  = cs_period_to_manual(period_own_rows(f$period, "remove")),               # removals: applied with the quality rules (R/qc.R)
       groups  = valid_patterns(ensure_cols(read_cfg(f$groups), c("pattern", "group")), "groups.csv"),
       time    = ensure_cols(read_cfg(f$time), TIME_COLS),
       timezones = ensure_cols(read_cfg(f$timezones), TZ_COLS), composites = ensure_cols(read_cfg(f$composites), COMPOSITE_COLS),
       gauges  = ensure_cols(read_cfg(f$gauges), PRECIP_COLS), gauges_file = f$gauges,
       info    = valid_patterns(ensure_cols(read_cfg(f$info), INFO_COLS), "parameter_info.csv"),
       period  = period_load(f$period, skip = "remove"))
}

map_parameters <- function(cols, aliases, logger_id, table = "") {
  lid <- logger_id; cols <- fix_enc(cols)                # logger_id may also be "logger:table" to name the columns of one table only
  a <- aliases[aliases$logger_id == "" | aliases$logger_id == lid | aliases$logger_id == paste0(lid, ":", table)]
  a <- a[order(logger_id == "")]                       # logger-specific rows win
  vapply(cols, function(cn) {
    for (i in seq_len(nrow(a)))
      if (grepl(a$pattern[i], cn, ignore.case = TRUE, perl = TRUE))
        return(sub(a$pattern[i], a$parameter[i], cn, ignore.case = TRUE, perl = TRUE))
    cn
  }, character(1), USE.NAMES = FALSE)
}

# Several raw columns can map to one parameter. Two cases:
#  - a sensor was replaced and the program wrote it under a new column (a cup anemometer swapped for an RM Young):
#    the columns take turns, so they are joined into one series. Where both hold a value the newer sensor is kept,
#    the other value is audited, and the hand-over is recorded as a sensor_change line in the audit trail.
#  - two columns were written at the same time (a sample beside its average, or two sensors given one name): they are
#    not merged. The average (or the first) gets the parameter name; the other keeps a name of its own
#    (<parameter>_sample for a plain sample), recorded as alias_kept_apart. Give it its own alias to name it yourself.
apply_aliases <- function(dt, aliases) {
  raw <- setdiff(names(dt), META_COLS)
  par <- map_parameters(raw, aliases, dt$logger_id[1], if ("table" %in% names(dt)) as.character(dt$table[1]) else "")
  tnum <- if ("TIMESTAMP" %in% names(dt)) as.numeric(dt$TIMESTAMP) else seq_len(nrow(dt))
  for (p in unique(par[duplicated(par)])) {
    src <- raw[par == p]
    cls <- vapply(src, function(s) class(dt[[s]])[1], "")
    if (length(unique(cls)) > 1) { par[par == p][-1] <- src[-1]; next }
    has <- lapply(src, function(s) !is.na(dt[[s]]))
    t0 <- vapply(has, function(k) if (any(k)) min(tnum[k]) else Inf, 0)
    avg <- grepl("_(avg|wvt)$", src, ignore.case = TRUE)
    o <- order(!avg, -t0); src <- src[o]; has <- has[o]                       # averages first, then the newest sensor
    together <- vapply(has[-1], function(k) sum(k & has[[1]]) > 0.5 * max(1, min(sum(k), sum(has[[1]]))), logical(1))
    for (s in src[-1][together]) {
      nm <- if (!grepl("_(avg|max|min|tot|std|smp)$", s, ignore.case = TRUE) && !paste0(p, "_sample") %in% c(par, raw)) paste0(p, "_sample") else s
      par[raw == s] <- nm
      audit_add("alias", "alias_kept_apart", "change", 0L, station = .col(dt, "station", 1L), logger_id = dt$logger_id[1], stream = .col(dt, "table", 1L), column = s,
                reason = paste0("'", s, "' and '", src[1], "' both map to '", p, "' but were recorded at the same time; '", src[1], "' is '", p, "' and '", s, "' is kept as '", nm, "'"))
    }
    for (s in src[-1][!together]) {
      a <- dt[[src[1]]]; b <- dt[[s]]; kb <- !is.na(b)
      clash <- which(!is.na(a) & kb & !same_value(a, b))
      remove_cells(dt, s, clash, "alias", "alias_collision",
                   paste0("'", s, "' and '", src[1], "' both map to '", p, "'; kept '", src[1], "' (the newer sensor)"))
      fill <- which(is.na(a) & !is.na(dt[[s]]))
      if (length(fill)) set(dt, fill, src[1], dt[[s]][fill])
      if (any(kb) && any(has[[1]]))
        audit_add("alias", "sensor_change", "change", 0L, station = .col(dt, "station", 1L), logger_id = dt$logger_id[1], stream = .col(dt, "table", 1L), column = p,
                  reason = sprintf("'%s' came from '%s' (%s to %s) and from '%s' (%s to %s): joined into one series", p, s, fmt_ts(min(dt$TIMESTAMP[kb])), fmt_ts(max(dt$TIMESTAMP[kb])),
                                   src[1], fmt_ts(min(dt$TIMESTAMP[has[[1]]])), fmt_ts(max(dt$TIMESTAMP[has[[1]]]))))
      set(dt, j = s, value = NULL)
      keep <- raw != s; raw <- raw[keep]; par <- par[keep]
    }
  }
  setnames(dt, raw, par)
  dt
}

# Station for each source file. A row of stations.csv with a header_name applies
# only to files whose header carries that name (for a logger whose files were
# mislabelled, or two loggers that ended up under one id). Such files are
# stitched on their own so they are never merged with the rest of that logger.
source_station <- function(man, stations) {
  h <- stations[header_name != "" & (station != "" | stream != "")]
  key <- function(l, n, t) paste(l, n, t, sep = "\r")
  pick <- function(col) {
    v <- h[[col]][match(key(man$logger_id, man$station_name, man$table), key(h$logger_id, h$header_name, h$table))]
    w <- h[table == ""]; v2 <- w[[col]][match(key(man$logger_id, man$station_name, ""), key(w$logger_id, w$header_name, ""))]
    v <- fifelse(is.na(v), v2, v); fifelse(is.na(v), "", v)
  }
  man[, `:=`(src_station = pick("station"), src_stream = pick("stream"))]
  man
}

assign_stations <- function(dt, stations, src_station = "", src_stream = "") {
  lid <- dt$logger_id[1]; tb <- dt$table[1]
  station <- rep(lid, nrow(dt)); stream <- rep(tb, nrow(dt))
  if (nzchar(src_station) || nzchar(src_stream)) {
    if (nzchar(src_station)) station[] <- src_station
    if (nzchar(src_stream))  stream[]  <- src_stream
  } else {
    r <- stations[logger_id == lid & header_name == "" & (table == "" | table == tb)]
    r <- r[order(table != "", start != "" | end != "")]   # general rows first, then table- and date-specific
    for (i in seq_len(nrow(r))) {
      s <- parse_time(r$start[i]); e <- parse_time(r$end[i])
      if (!is.na(e) && nchar(r$end[i]) <= 10) e <- e + 86399          # end date is inclusive
      hit <- (is.na(s) | dt$TIMESTAMP >= s) & (is.na(e) | dt$TIMESTAMP <= e)
      if (nzchar(r$station[i])) station[hit] <- r$station[i]
      if (nzchar(r$stream[i]))  stream[hit]  <- r$stream[i]
    }
  }
  dt[, `:=`(station = station, stream = stream)]
  dt
}


# Rows of the built-in tables that a project's own table does not have yet (matched on pattern and logger).
# New built-in aliases only reach an existing project when its owner adds them, so nothing changes silently.
missing_defaults <- function(current, kind = c("aliases", "groups", "qc")) {
  kind <- match.arg(kind); def <- switch(kind, aliases = default_aliases(), groups = default_groups(), qc = default_qc())
  if (is.null(current) || !nrow(current)) return(def)
  key <- function(d) paste(d$pattern, if ("logger_id" %in% names(d)) d$logger_id else "", if ("applies_to" %in% names(d)) d$applies_to else "")
  def[!key(def) %in% key(as.data.table(current))]
}

# ---- thermistor depths ----------------------------------------------------------------------------------------
# Thermistor columns by sensor number. Depths of a project's own loggers go in its config/aliases.csv (one row per logger_id).
therm_pattern <- function(k, type = "10[79]", stat = "Avg") if (k == 1) sprintf("^T%s_C_%s$|^T%s_C_1_%s$|^T%s_1_C_%s$|^Temp%s_1_%s$", type, stat, type, stat, type, stat, type, stat) else sprintf("^T%s_C_%d_%s$|^T%s_%d_C_%s$|^Temp%s_%d_%s$", type, k, stat, type, k, stat, type, k, stat)
depth_alias_rows <- function() {
  out <- list(); add <- function(pattern, parameter, logger_id = "", notes = "") out[[length(out) + 1L]] <<- data.table(pattern = pattern, parameter = parameter, logger_id = logger_id, notes = notes)
  # the depth is not known, so sensor N is taken to be at N x 10 cm (a logger with known depths gets its own rows in config/aliases.csv)
  for (k in 1:12) { cm <- k * 10; note <- "depth ASSUMED: sensor number x 10 cm"
    add(therm_pattern(k, "109"), paste0("ground_temp_", cm, "cm"), "", note); add(gsub("Avg\\$", "(Min|Max)$", therm_pattern(k, "109")), paste0("ground_temp_", cm, "cm_\\L\\1\\2\\3"), "", "")
    add(therm_pattern(k, "107"), paste0("ground_temp_107_", cm, "cm"), "", note); add(gsub("Avg\\$", "(Min|Max)$", therm_pattern(k, "107")), paste0("ground_temp_107_", cm, "cm_\\L\\1\\2\\3"), "", "")
    add(sprintf("^CS65[05]Temp_%d_Avg$", k), paste0("soil_probe_temp_", cm, "cm"), "", note); add(sprintf("^CS65[05]Temp_%d_(Min|Max)$", k), paste0("soil_probe_temp_", cm, "cm_\\L\\1"), "", "")
    add(sprintf("^WaterContent_%d_Avg$|^VW_%d_Avg$%s", k, k, if (k == 1) "|^VW_Avg$" else ""), paste0("vwc_", cm, "cm"), "", note); add(sprintf("^WaterContent_%d_(Min|Max)$", k), paste0("vwc_", cm, "cm_\\L\\1"), "", "")
    add(sprintf("^EC_%d_Avg$%s", k, if (k == 1) "|^EC_Avg$" else ""), paste0("soil_ec_", cm, "cm"), "", note); add(sprintf("^EC_%d_(Min|Max)$%s", k, if (k == 1) "|^EC_(Min|Max)$" else ""), paste0("soil_ec_", cm, "cm_\\L\\1\\2"), "", "") }
  rbindlist(out)
}
# CR10X columns named by position (F05, F06, ...) are named per logger in config/aliases.csv (pattern ^F05$, logger_id <id>:<array>).
default_aliases <- function() rbind(alias_base(), depth_alias_rows())

# ---- calibrations and manual removals: rows of the corrections over a period ---------------------------------------------
# Both are rows of config/period_corrections.csv (core periods.R) now: a calibration (new value = value x multiplier + offset,
# for the stations or loggers in applies_to, between start and end) is a scale row and an offset row; an output name writes the
# result to that parameter and keeps the original (the period table's output column). A manual removal (station, stream, a
# parameter pattern, start and end) is a remove row, applied where it was, with the quality rules (R/qc.R). The tables of an
# older version (calibrations.csv, manual_removals.csv) are moved in once (CONFIG_FOLDS) and left as they were.
# The pages that wrote the old tables still hand rows in their shape: cs_*_to_period() turn them into period rows and
# cs_period_to_*() back (core period_view()).
CAL_COLS <- c("applies_to", "parameter", "output", "multiplier", "offset", "start", "end", "notes")
CS_MANUAL_COLS <- c("station", "stream", "parameter", "start", "end", "reason", "author")
CS_PERIOD_EXTRA <- c("stream", "output")
cs_period_row <- function(n) data.table(target = rep("", n), parameter = "", from = "", to = "", action = "", value = "", on = "", note = "", author = "", added = "", stream = "", output = "")
cs_manual_to_period <- function(m) { m <- ensure_cols(copy(as.data.table(m)), CS_MANUAL_COLS); r <- cs_period_row(nrow(m))
  r[, `:=`(target = m$station, stream = m$stream, parameter = m$parameter, from = m$start, to = m$end, action = "remove", note = m$reason, author = m$author)]; r[] }
cs_period_to_manual <- function(p) { p <- ensure_cols(copy(as.data.table(p)), c(PERIOD_COLS, CS_PERIOD_EXTRA))
  r <- data.table(station = p$target, stream = p$stream, parameter = p$parameter, start = p$from, end = p$to, reason = p$note, author = p$author)
  if ("row" %in% names(p)) r[, row := p$row]; r[] }
# A calibration's end was the first time it no longer applied; a period's To is the last time it applies (a date alone: that
# whole day), so the end goes back one second (a midnight: the day before). A start or end that is not a date applied as no
# limit, so it is left blank and said in the note. A row with no parameter applied to nothing: kept, switched off.
cs_cal_to_period <- function(cal) {
  cal <- ensure_cols(copy(as.data.table(cal)), CAL_COLS); out <- list()
  for (i in seq_len(nrow(cal))) { x <- cal[i]; p <- trim(x$parameter); o <- trim(x$output); why <- character()
    t0 <- parse_time(x$start); t1 <- parse_time(x$end)
    if (nzchar(x$start) && is.na(t0)) why <- c(why, paste0("start '", x$start, "' was not a date"))
    if (nzchar(x$end) && is.na(t1)) why <- c(why, paste0("end '", x$end, "' was not a date"))
    to <- if (is.na(t1)) "" else { e <- t1 - 1; if (format(t1, "%H:%M:%S", tz = "UTC") == "00:00:00") format(e, "%Y-%m-%d", tz = "UTC") else format(e, "%Y-%m-%d %H:%M:%S", tz = "UTC") }
    mu <- suppressWarnings(as.numeric(x$multiplier)); off <- suppressWarnings(as.numeric(x$offset))
    mu_t <- if (is.na(mu)) "1" else trim(x$multiplier); off_t <- if (is.na(off)) "0" else trim(x$offset); if (is.na(mu)) mu <- 1; if (is.na(off)) off <- 0
    note <- paste(c(if (nzchar(x$notes)) x$notes, "a calibration", why), collapse = "; ")
    row <- function(act, val, param, out_col) { r <- cs_period_row(1L)            # set() by name: the arguments are not read as the table's columns
      v <- list(target = paste(trim(strsplit(x$applies_to, "|", fixed = TRUE)[[1]]), collapse = "|"), parameter = param, from = if (is.na(t0)) "" else x$start, to = to,
                action = act, value = val, on = if (nzchar(p)) "" else "off", note = note, output = out_col)
      for (k in names(v)) set(r, j = k, value = v[[k]]); r }
    sc <- mu != 1 || off == 0                                                # the identity still copies into an output
    if (sc) out[[length(out) + 1L]] <- row("scale", mu_t, p, if (nzchar(o) && o != p) o else "")
    if (off != 0) out[[length(out) + 1L]] <- row("offset", off_t, if (sc && nzchar(o) && o != p) o else p, if (!sc && nzchar(o) && o != p) o else "") }
  if (length(out)) rbindlist(out) else cs_period_row(0L)
}
cs_period_to_cal <- function(p) { p <- ensure_cols(copy(as.data.table(p)), c(PERIOD_COLS, CS_PERIOD_EXTRA))
  data.table(applies_to = p$target, parameter = p$parameter, output = p$output, multiplier = ifelse(p$action == "scale", p$value, ""), offset = ifelse(p$action == "offset", p$value, ""),
             start = p$from, end = p$to, notes = p$note) }
# What the scale and offset rows did to a station table (period_apply()'s log), as output/audit/calibrations_applied.csv lists it.
cs_cal_log <- function(l, dt) data.table(station = as.character(dt$station[1]), stream = as.character(dt$stream[1]), parameter = sub(" to .*$", "", l$parameter),
  output = ifelse(grepl(" to ", l$parameter), sub("^.* to ", "", l$parameter), sub(" to .*$", "", l$parameter)),
  multiplier = ifelse(l$action == "scale", l$value, 1), offset = ifelse(l$action == "offset", l$value, 0), start = l$from, end = l$to, values = l$values, notes = l$note)
CONFIG_FOLDS <- list(
  list(from = "calibrations.csv", into = PERIOD_FILE, fold = function(old, cur) {          # calibrations came first: their rows go first
    r <- cs_cal_to_period(old); list(rows = rbind(r, ensure_cols(as.data.table(cur %||% cs_period_row(0L)), names(r)), fill = TRUE), log = NULL) }),
  list(from = "manual_removals.csv", into = PERIOD_FILE, fold = function(old, cur) {
    list(rows = rbind(ensure_cols(as.data.table(cur %||% cs_period_row(0L)), names(cs_period_row(0L))), cs_manual_to_period(old), fill = TRUE), log = NULL) }))
# Built-in aliases from an earlier version that the rows above replace. Removed from a project's table when it takes the new ones.
RETIRED_ALIASES <- c("^AirTemp_(High|Low)_(Avg|Min|Max)$", "^RH_(High|Low)_(Avg|Min|Max)$", "^TBRG_TOT$", "^T109_C_Avg$", "^T109_C_(Min|Max)$", "^T109_C_([0-9]+)_Avg$|^T109_([0-9]+)_C_Avg$|^Temp109_([0-9]+)_Avg$", "^T109_C_([0-9]+)_(Min|Max)$|^Temp109_([0-9]+)_(Min|Max)$",
  "^T107_C_Avg$", "^T107_C_(Min|Max)$", "^T107_C_([0-9]+)_Avg$|^T107_([0-9]+)_C_Avg$|^Temp107_([0-9]+)_Avg$", "^T107_C_([0-9]+)_(Min|Max)$|^Temp107_([0-9]+)_(Min|Max)$",
  "^CS65[05]Temp_([0-9]+)_Avg$", "^CS65[05]Temp_([0-9]+)_(Min|Max)$", "^WaterContent_([0-9]+)_Avg$|^VW_([0-9]+)_Avg$", "^WaterContent_([0-9]+)_(Min|Max)$", "^VW_Avg$", "^EC_Avg$", "^EC_(Min|Max)$",
  "^EC_([0-9]+)_Avg$", "^EC_([0-9]+)_(Min|Max)$")
retired_rows <- function(d) { def <- default_aliases(); lg <- if ("logger_id" %in% names(d)) d$logger_id else rep("", nrow(d))
  which(d$pattern %in% RETIRED_ALIASES & (is.na(lg) | lg == "") & !paste(d$pattern, d$parameter) %in% paste(def$pattern, def$parameter)) }

# Calibration options (core calibration.R): values fixed in code that a project may change in config/calibration_options.csv,
# per station (applies_to) or for all (*). Sources: _hub/Derived_values_references.md, sections 3 and 11.
CS_CALIBRATION <- data.table(app = "CS",
  value = c("pt_alpha", "sr50_temp_correction"),
  label = c("Priestley-Taylor alpha", "SR50 temperature correction"), unit = c("", ""),
  default_number = c("1.26", "auto"),
  default_source = c("Priestley, C.H.B., Taylor, R.J. (1972). Mon. Weather Rev. 100, 81-92 (open water and wet surfaces)",
                     "Campbell Scientific SR50A manual: distance x sqrt((Ta + 273.15) / 273.15); a TCDT column is already corrected"),
  scotty_number = c("0.69", ""),                                                # Scotty Creek only where a calibration is published
  scotty_source = c("Connon, R.F., Quinton, W.L., Craig, J.R., Hanisch, J., Sonnentag, O. (2015). Hydrol. Process. 29 (bog); 0.68-0.91 on plateaus (Quinton and Baltzer 2013)", ""),
  allowed = c("", "auto;none"),
  notes = c("Daily ET by Priestley-Taylor (et_pt_mm).", "auto: TCDT if logged, else DT corrected with air_temp; none: the distance as logged. as in Haynes et al. (2019)."))

# Every set-up table, for exporting and importing the whole configuration as one zip (export_config / read_config_import /
# apply_config_import in Ovid/config.R). CS still reads the tables itself in cs_load_configs() above.
CONFIG_FILES <- list(
  stations   = list(file = "stations.csv",         cols = function() STATION_COLS, label = "Stations"),
  deploy_sheet = list(file = "deployment_sheet.csv", cols = function() DEPLOY_COLS, label = "Deployment sheet"),
  aliases    = list(file = "aliases.csv",          cols = c("pattern", "parameter", "logger_id", "notes"), label = "Column aliases"),
  groups     = list(file = "groups.csv",           cols = c("pattern", "group"), label = "Parameter groups"),
  qc         = list(file = "qc_rules.csv",         cols = function() QC_COLS, label = "Automatic limits"),
  time       = list(file = "time_corrections.csv", cols = function() TIME_COLS, label = "Clock corrections",
                    rules = list(known = list(logger_id = known_from("config/stations.csv", "logger_id")))),
  timezones  = list(file = "timezones.csv",        cols = function() TZ_COLS, label = "Time zones"),
  files      = list(file = "file_rules.csv",       cols = function() FILE_COLS, label = "File rules"),
  cr10x      = list(file = "cr10x.csv",            cols = c("file", "import", "logger_id", "fsl", "start_year", "notes"), label = "CR10X files"),
  gauges     = list(file = "precip_gauges.csv",    cols = function() PRECIP_COLS, label = "Precipitation gauges"),
  sites      = list(file = "station_info.csv",     cols = function() SITE_COLS, label = "Station details"),
  field_log  = list(file = "field_log.csv",        cols = function() LOG_COLS, label = "Field log"),
  composites = list(file = "composites.csv",       cols = function() COMPOSITE_COLS, label = "Combined records"),
  info       = list(file = "parameter_info.csv",   cols = function() INFO_COLS, label = "Parameter guide"),
  calib      = calibration_config(CS_CALIBRATION),
  period     = period_config(CS_PERIOD_EXTRA))

# Every change a run makes to the data on its own (core corrections.R): listed on the Automatic corrections step and in each run's log.
CORRECTIONS <- list(
  list(id = "stray_records", rules = "stray_record", does = "removes", setting = "drop_stray_records", panel = TRUE,
       what = "Records with corrupt time stamps among good ones (off the time grid and out of the order the file was written in) are left out; each is in the audit trail."),
  list(id = "bad_clock", rules = "bad_timestamp", does = "removes", setting = "drop_bad_clock", panel = TRUE,
       what = "Records stamped before the first year (min_year) or more than two days in the future are left out as a reset clock (off: kept at the time they carry; records with no time are always left out)."),
  list(id = "error_codes", rules = "sentinel", does = "removes", setting = "remove_error_codes", panel = TRUE,
       what = "Campbell out-of-range codes (-7999, 7999, -6999, 6999) are removed (off: kept as values). Infinite values are always removed."),
  list(id = "overlaps", rules = c("overlap_conflict", "station_overlap"), does = "changes", setting = "prefer", panel = TRUE, choices = c("The newest file's" = "newest", "The oldest file's" = "oldest"),
       what = "Where two files hold different values for the same time, one file's are kept: the newest download's by default."),
  list(id = "clock", does = "changes", table = "time_corrections.csv", where = "Clock corrections", what = "Logger clocks are shifted by the rows of the clock corrections table."),
  list(id = "zones", does = "changes", table = "timezones.csv", where = "Time zones", what = "Logger clocks are moved to the zone given in the time-zone table."),
  list(id = "qc_rules", does = "removes", table = "qc_rules.csv", where = "QC rules",
       what = "Values outside each rule's limits, spikes and flat lines are removed or flagged, as each row's action says."),
  list(id = "period", does = "changes", table = PERIOD_FILE, where = "Corrections over a period", what = "Values in each period of the corrections list are offset, scaled (calibrations; into another column where output names one), drift-corrected, filled by interpolation or flagged, in the list's order; its removals are made with the quality rules."),
  list(id = "cross_checks", rules = c("sensors_disagree", "mean_outside_min_max", "outgoing_exceeds_incoming", "sunlight_at_night", "sw_above_physical_limit", "lw_outside_physical_limit", "possible_icing", "tips_below_freezing"), does = "flags", setting = "cross_checks", panel = TRUE,
       what = "Values that disagree with another sensor on the same station (two air temperatures, two humidities, a mean outside its minimum and maximum) are flagged."),
  list(id = "neighbours", rules = "differs_from_other_stations", does = "flags", setting = "neighbour_check", panel = TRUE,
       what = "Days on which a station departs from the same parameter at the other stations are flagged."),
  list(id = "hide_accepted", does = "flags", setting = "hide_accepted_flags", panel = TRUE,
       what = "Flags decided not a problem (Needs you) are left out of the exported flag columns; flags.csv keeps them, marked accepted."))

# Where a station's name is held (core rename.R): renaming one changes every table here.
RENAMES <- list(station = list(label = "Station", main = c("stations.csv", "station"),
  cols = list(c("station_info.csv", "station"), c("file_rules.csv", "station"), c("precip_gauges.csv", "station"),
              c("precip_gauges.csv", "wind_station"), c("field_log.csv", "station"), c("composites.csv", "station"), c("deployment_sheet.csv", "station")),
  lists = list(c("qc_rules.csv", "applies_to"))))
