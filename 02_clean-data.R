## This script is used to clean and explore data from the
## 1999-2024 fyke survey and the 2019-2024 water quality data.


## Functions

# A rounding function that always rounds .5 up to the nearest integer. (The normal
# round() function rounds .5 to the nearest even number.)
round2 <- function(x, digits = 0) {  # Function to always round 0.5 up
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}



##### Fyke Data #####

# Load fyke survey data
fyke <- read_excel("data/raw-data/FykeSets.xlsx")

# Change column names
colnames(fyke) <- c("event", "station", "pond", "set.date",
  "haul.date", "haul.month", "haul.year", "haul.winter",
  "set.water.temp_c", "set.air.temp_c", "set.salinity_ppt", "set.do_mg.l",
  "haul.water.temp_c", "haul.air.temp_c", "haul.salinity_ppt", "haul.do_mg.l",
  "soak_days", "set.occurrence_yr", "WFL_Caught", "wfl_freq", "Comments")

# Remove superfluous columns
fyke <- select(fyke, -WFL_Caught, -Comments)

# Convert set air temp from F to C
fyke$set.air.temp_c <- (fyke$set.air.temp_c - 32) * 5/9
fyke$set.air.temp_c <- round2(fyke$set.air.temp_c, 2)

# Convert haul air temp from F to C
fyke$haul.air.temp_c <- (fyke$haul.air.temp_c - 32) * 5/9
fyke$haul.air.temp_c <- round2(fyke$haul.air.temp_c, 2)

# Add haul/set averages
fyke$hs.avg.air.temp_c <- (fyke$haul.air.temp_c + fyke$set.air.temp_c) / 2
fyke$hs.avg.water.temp_c <- (fyke$haul.water.temp_c + fyke$set.water.temp_c) / 2
fyke$hs.avg.salinity_ppt <- (fyke$haul.salinity_ppt + fyke$set.salinity_ppt) / 2
fyke$hs.avg.do_mg.l <- (fyke$haul.do_mg.l + fyke$set.do_mg.l) / 2

# Add a column for change in water temperature
fyke$delta.water.temp_c <- fyke$haul.water.temp_c - fyke$set.water.temp_c

# Add a column for change in water temperature per day
fyke$delta.water.temp_c.day <- fyke$delta.water.temp_c / fyke$soak_days

# Add a column for change in salinity
fyke$delta.salinity_ppt <- fyke$haul.salinity_ppt - fyke$set.salinity_ppt

# Add a column for change in salinity per day
fyke$delta.salinity_ppt.day <- fyke$delta.salinity_ppt / fyke$soak_days

# Add a column for change in dissolved oxygen
fyke$delta.do_mg.l <- fyke$haul.do_mg.l - fyke$set.do_mg.l

# Add a column for change in dissolved oxygen per day
fyke$delta.do_mg.l.day <- fyke$delta.do_mg.l / fyke$soak_days

# Make a binary catch column wherein 1 = winter flounder caught and 0 = no winter flounder caught
fyke$wfl_binary <- ifelse(fyke$wfl_freq == 0,0,1)
fyke$wfl_binary <- as.factor(fyke$wfl_binary)

# Calculate Julian day
fyke$haul.date <- as.POSIXlt(fyke$haul.date, format="%d-%m-%y")
fyke$haul.date_jul <- as.numeric(format(fyke$haul.date, "%j"))
  
# Convert Julian Day to be 1 at Nov 1 as that is earliest conceivable start date
fyke$haul.date_jul <- ifelse(fyke$haul.date_jul < 200,
  fyke$haul.date_jul + 61, fyke$haul.date_jul - 304)

# Calculate mean lunar illumination over soak period
fyke$lunar.illumination <- NA
fyke$haul.date <- as.Date(fyke$haul.date)
for (i in 1:nrow(fyke)){
  
  # If the soak period is missing (28 of 1039 observations or 2.7%)
  if (is.na(fyke$soak_days[i])){
    
    # Calculate lunar illumination over the median soak period (4 days)
    fyke$lunar.illumination[i] <- lunar.illumination.mean(fyke$haul.date[i], towards = -4, shift = -4)
    
  } else {
    
    # Calculate mean lunar illumination over the soak period
    fyke$lunar.illumination[i] <- lunar.illumination.mean(fyke$haul.date[i], towards = (-1 * fyke$soak_days[i]), shift = -4)
    
  }
  
}

# Change 999 to NA in set occurrence per year
fyke$set.occurrence_yr <- ifelse(fyke$set.occurrence_yr == 999, NA, fyke$set.occurrence_yr)

# Fill in haul.year column
fyke$haul.year <- NA
fyke$haul.year <- as.numeric(substr(fyke$event, 5, 8))

# Fill in haul.winter column
fyke$haul.winter <- NA
fyke$haul.winter <- ifelse(fyke$haul.month %in% c(7:12), fyke$haul.year + 1, fyke$haul.year)

# Make haul.year and haul.winter columns 2 and 3
fyke <- fyke[, c(7:8, 1:6, 9:ncol(fyke))]

#gt_plt_summary(fyke[3:ncol(fyke)], "Fyke Summary")




##### Flounder Data #####

# Load data on individual fish
flounder <- read_excel("data/raw-data/WinterFlounder.xlsx")

# Change column names
colnames(flounder)[1] <- "event"
colnames(flounder)[3] <- "length_cm"

# Select desired columns
flounder <- select(flounder, event, length_cm)

# Add new columns
flounder$year <- as.numeric(substr(flounder$event, 5, 8))
flounder$pond <- substr(flounder$event, 10, 11)
flounder$station <- substr(flounder$event, 10, 12)
flounder$month <- as.numeric(substr(flounder$event, 1, 2))

# Correct year to represent survey year
flounder$year <- ifelse(flounder$month > 9, flounder$year + 1, flounder$year)

# Remove event id
flounder <- select(flounder, -event)

# Save cleaned data
write_csv(flounder, "data/clean-data/02_FishLengths.csv")


##### Weather Data #####

## Import and analyze NOAA weather data from:
##  NOAA National Centers for Environmental Information (NCEI) Climate Data Online
##  https://www.ncei.noaa.gov/cdo-web/datasets

## The weather stations are Ninigret (1997-02-27 through 2024-02-06) and
## Westerly State Airport (1999-07-27 through 2024-04-01). Dataset is daily summaries.

# Load weather data
weather <- read.csv("data/raw-data/DailyWeather.csv")

# Change column names
colnames(weather) <- c("code", "station", "latitude", "longitude", "elevation_m",
  "date", "avg.wind_m.s", "precip_mm", "avg.temp_c", "max.temp_c", "min.temp_c", "obs.temp_c")

# Extract weather stations
stations <- weather %>% distinct(code, .keep_all = TRUE)

# Select only station level attributes
stations <- select(stations, code, station, latitude, longitude, elevation_m)

# Specify CRS as WGS84
crs <- st_crs(4326)

# Extract lat/lon of closest station (South Kingston 4.3 WSW, RI US)
lat <- stations$latitude[stations$code == "US1RIWS0080"]
lon <- stations$longitude[stations$code == "US1RIWS0080"]
center <- st_sfc(st_point(c(lon, lat)), crs = crs)

# Add a column for distance from closest station
stations$distance <- st_distance(stations %>% st_as_sf(coords = c("longitude", "latitude"), crs = crs), center)

# Make list of station ID's from closest to farthest
station_priority <- stations$code[order(stations$distance)]

# Delete unwanted columns
weather <- select(weather, -station, -latitude, -longitude, -elevation_m)

# Make date column a date object
weather$date <- as.Date(weather$date, "%m/%d/%y")

# Pivot weather data
weather <- pivot_wider(weather, names_from = code, values_from = c(avg.wind_m.s, precip_mm, avg.temp_c, max.temp_c, min.temp_c, obs.temp_c))

# Create composite columns
weather$avg.temp_c <- NA
weather$avg.temp_station <- NA
weather$avg.temp_type <- NA
weather$precip_mm <- NA
weather$precip_station <- NA
weather$avg.wind_m.s <- NA
weather$avg.wind_station <- NA
weather$max.temp_c <- NA
weather$max.temp_station <- NA
weather$min.temp_c <- NA
weather$min.temp_station <- NA
weather$temp.range_c <- NA

# Fill in composite columns in the order of station_priority
for (i in 1:nrow(weather)){
  
  # Average Temp
  for (j in station_priority){
    if (!is.na(weather[[paste0("avg.temp_c_", j)]][i])){
      weather$avg.temp_c[i] <- weather[[paste0("avg.temp_c_", j)]][i]
      weather$avg.temp_station[i] <- j
      weather$avg.temp_type[i] <- "hrly"
      break
    }
  }
  
  # Precipitation
  for (j in station_priority){
    if (!is.na(weather[[paste0("precip_mm_", j)]][i])){
      weather$precip_mm[i] <- weather[[paste0("precip_mm_", j)]][i]
      weather$precip_station[i] <- j
      break
    }
  }
  
  # Wind Speed
  for (j in station_priority){
    if (!is.na(weather[[paste0("avg.wind_m.s_", j)]][i])){
      weather$avg.wind_m.s[i] <- weather[[paste0("avg.wind_m.s_", j)]][i]
      weather$avg.wind_station[i] <- j
      break
    }
  }
  
  # Max Temp
  for (j in station_priority){
    if (!is.na(weather[[paste0("max.temp_c_", j)]][i])){
      weather$max.temp_c[i] <- weather[[paste0("max.temp_c_", j)]][i]
      weather$max.temp_station[i] <- j
      break
    }
  }
  
  # Min Temp
  for (j in station_priority){
    if (!is.na(weather[[paste0("min.temp_c_", j)]][i])){
      weather$min.temp_c[i] <- weather[[paste0("min.temp_c_", j)]][i]
      weather$min.temp_station[i] <- j
      break
    }
  }
  
  # If average temp is missing, calculate it from max and min
  if (is.na(weather$avg.temp_c[i])){
    if (!is.na(weather$max.temp_c[i]) & !is.na(weather$min.temp_c[i])){
      weather$avg.temp_c[i] <- (weather$max.temp_c[i] + weather$min.temp_c[i]) / 2
      weather$avg.temp_station[i] <- weather$max.temp_station[i]
      weather$avg.temp_type[i] <- "hilo"
    }
  }
  
  # If average temp is still missing, use observed temp in order of station priority
  if (is.na(weather$avg.temp_c[i])){
    for (j in station_priority){
      if (!is.na(weather[[paste0("obs.temp_c_", j)]][i])){
        weather$avg.temp_c[i] <- weather[[paste0("obs.temp_c_", j)]][i]
        weather$avg.temp_station[i] <- j
        weather$avg.temp_type[i] <- "obs"
        break
      }
    }
  }
  
}

# Select desired columns
weather <- select(weather,
  date,
  avg.temp_c, avg.temp_type, avg.temp_station,
  precip_mm, precip_station,
  avg.wind_m.s, avg.wind_station,
  max.temp_c, max.temp_station,
  min.temp_c, min.temp_station,
  temp.range_c)

# Add temperature range
weather$temp.range_c <- weather$max.temp_c - weather$min.temp_c

# Add heating degrees per day relative to 18 C
weather$heating.degrees_day <- ifelse(weather$avg.temp_c < 18, 18 - weather$avg.temp_c, 0)

# Summarize weather data
gt_plt_summary(weather, "Weather Summary")




##### Water Quality Data #####

## This section of code runs through the in-water data loggers placed
## on the fyke nets to generate summary statistics. DO NOT
## USE THE SALINITY VALUES AS SOME OF THE SENSORS ARE
## NOT WORKING.

# List of water quality data files
wq_files <- list.files("data/raw-data/water-quality", full.names = TRUE)

# Read in water quality data
water <- lapply(wq_files, read_excel)

# Add event ID to each water quality data frame
water <- lapply(1:length(water), function(i) {
  water[[i]]$event <- wq_files[i]
  return(water[[i]])
})

# Combine all data into one dataframe
water <- bind_rows(water)

# Remove filepath from event ID
water$event <- gsub("data/raw-data/water-quality/", "", water$event)

# Remove file extension from event ID
water$event <- gsub(".xlsx", "", water$event)

# Change column names
colnames(water) <- c("date.time", "temp_c", "depth_m", "salinity_psu",
  "conductivity_mS.cm", "sound.velocity_m.s", "event")

# Select desired columns
water <- select(water, -salinity_psu, -conductivity_mS.cm, -sound.velocity_m.s)

# Remove rows with missing temperature data
water <- filter(water, !is.na(temp_c))

# Add columns to fyke data for summary statistics
fyke$mean.water.temp_c <- NA #haul and set temps are normally distributed
fyke$min.water.temp_c <- NA
fyke$max.water.temp_c <- NA
fyke$range.water.temp_c <- NA
fyke$skewness.water.temp <- NA
fyke$kurtosis.water.temp <- NA
fyke$bimodality.water.temp <- NA
fyke$mean.depth_m <- NA
fyke$min.depth_m <- NA
fyke$max.depth_m <- NA
fyke$range.depth_m <- NA

# Add summary statistics to fyke data
for(i in 1:nrow(fyke)) {
  
  # Subset water quality data for the current fyke event
  water_sub <- filter(water, event == fyke$event[i])
  
  # Skip if no water quality data are available
  if(nrow(water_sub) > 0) {
    
    # Calculate summary statistics for water temperature  
    fyke$mean.water.temp_c[i] <- mean(water_sub$temp_c, na.rm = TRUE)
    fyke$min.water.temp_c[i] <- min(water_sub$temp_c, na.rm = TRUE)
    fyke$max.water.temp_c[i] <- max(water_sub$temp_c, na.rm = TRUE)
    fyke$range.water.temp_c[i] <- fyke$max.water.temp_c[i] - fyke$min.water.temp_c[i]
    fyke$skewness.water.temp[i] <- skewness(water_sub$temp_c, na.rm = TRUE)
    fyke$kurtosis.water.temp[i] <- kurtosis(water_sub$temp_c, na.rm = TRUE)
    fyke$bimodality.water.temp[i] <- bimodality_coefficient(water_sub$temp_c, na.rm = TRUE)
    
    # Calculate summary statistics for depth
    fyke$mean.depth_m[i] <- mean(water_sub$depth_m, na.rm = TRUE)
    fyke$min.depth_m[i] <- min(water_sub$depth_m, na.rm = TRUE)
    fyke$max.depth_m[i] <- max(water_sub$depth_m, na.rm = TRUE)
    fyke$range.depth_m[i] <- fyke$max.depth_m[i] - fyke$min.depth_m[i]
    
  }
  
}

# Remove Inf and NaN
fyke <- mutate_if(fyke, is.numeric, function(x) ifelse(is.infinite(x), NA, x))
fyke <- mutate_if(fyke, is.numeric, function(x) ifelse(is.nan(x), NA, x))
  
# Summary of fyke data
gt_plt_summary(fyke[3:ncol(fyke)], "Fyke Summary")




##### Combine Data #####

# Add empty columns to fyke data
fyke$noaa.avg.air.temp_c <- NA
fyke$noaa.max.temp_c <- NA
fyke$noaa.min.temp_c <- NA
fyke$noaa.avg.high_c <- NA
fyke$noaa.temp.range_c <- NA
fyke$noaa.precip_mm.day <- NA
fyke$noaa.wind_m.s <- NA
fyke$noaa.heating.degrees_day <- NA

# Convert all date columns to date objects
fyke$set.date <- as.Date(fyke$set.date, format = "%d-%m-%y")
fyke$haul.date <- as.Date(fyke$haul.date, format = "%d-%m-%y")
weather$date <- as.Date(weather$date)

# Combine fyke and weather data
for (i in 1:nrow(fyke)){
  
  # Subset weather data for current fyke event
  if(!is.na(fyke$set.date[i])){
    
    # Using observed haul and set dates
    weather_sub <- filter(weather, date >= fyke$set.date[i] & date <= fyke$haul.date[i])

    } else {
    
    # Using the haul date and median soak period of 4 days
    weather_sub <- filter(weather, date <= fyke$haul.date[i] & date >= fyke$haul.date[i] - 4)
  
  }
  
  # Skip if no weather data are available
  if(nrow(weather_sub) > 0) {
    
    # Calculate summary statistics for weather data
    fyke$noaa.avg.air.temp_c[i] <- mean(weather_sub$avg.temp_c, na.rm = TRUE)
    fyke$noaa.max.temp_c[i] <- max(weather_sub$max.temp_c, na.rm = TRUE)
    fyke$noaa.min.temp_c[i] <- min(weather_sub$min.temp_c, na.rm = TRUE)
    fyke$noaa.avg.high_c[i] <- mean(weather_sub$max.temp_c, na.rm = TRUE)
    fyke$noaa.heating.degrees_day[i] <- mean(weather_sub$heating.degrees_day, na.rm = TRUE)
    
    # Wind data
    fyke$noaa.wind_m.s[i] <- mean(weather_sub$avg.wind_m.s, na.rm = TRUE)
    
    # Skip if any precip data are missing
    if(all(!is.na(weather_sub$precip_mm))) {
      fyke$noaa.precip_mm.day[i] <- mean(weather_sub$precip_mm)
    }
    
  }
  
}

# Calculate NOOA temperature range
fyke$noaa.temp.range_c <- fyke$noaa.max.temp_c - fyke$noaa.min.temp_c

# Replace all NaN with NA
fyke <- mutate_if(fyke, is.numeric, function(x) ifelse(is.nan(x), NA, x))

# Replace all inf with NA
fyke <- mutate_if(fyke, is.numeric, function(x) ifelse(is.infinite(x), NA, x))

# Summary of combined data
fyke.summary <- gt_plt_summary(fyke[3:ncol(fyke)], "Combined Data Summary")

# Save summary
gtsave(fyke.summary, "documents/02_FykeSummary.png")

# Save cleaned data
write_csv(fyke, "data/clean-data/02_FykeSets_Complete.csv")
write_csv(weather, "data/clean-data/02_NOAAWeather.csv")




