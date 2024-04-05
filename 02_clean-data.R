## This script is used to clean and explore data from the
## 1999-2024 fyke survey and the 2019-2024 water quality data.


## TO DO:
##    - Add degree heating days


## Load data

# Load fyke survey data
fyke <- read_excel("data/raw-data/FykeSets.xlsx")

# Load water quality data


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

# Add a column for change in water temperature
fyke$delta.temp_c <- fyke$haul.water.temp_c - fyke$set.water.temp_c

# Add a column for change in water temperature per day
fyke$delta.temp_c.day <- fyke$delta.temp_c / fyke$soak_days

# Add a column for change in air temperature
fyke$delta.air.temp_c <- fyke$haul.air.temp_c - fyke$set.air.temp_c

# Add a column for change in air temperature per day
fyke$delta.air.temp_c.day <- fyke$delta.air.temp_c / fyke$soak_days

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
fyke$haul.date <- as.Date(fyke$haul.date)
for (i in 1:nrow(fyke)){
  
  # If the soak period is missing (28 of 1039 observations or 2.7%)
  if (is.na(fyke$soak_days[i])){
    
    # Calculate lunar illumination for the haul date
    fyke$lunar.illumination[i] <- lunar.illumination(fyke$haul.date[i], shift = -4)
    
  } else {
    
    # Calculate mean lunar illumination over the soak period
    fyke$lunar.illumination[i] <- lunar.illumination.mean(fyke$haul.date[i], towards = (-1 * fyke$soak_days[i]), shift = -4)
    
  }
  
}









# Change 999 to NA in set occurrence per year
fyke$set.occurrence_yr <- ifelse(fyke$set.occurrence_yr == 999, NA, fyke$set.occurrence_yr)




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
  
  # Skip if no water quality data is available
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
  



##### Summarize #####

# Summary of fyke data
gt_plt_summary(fyke[6:ncol(fyke)])







