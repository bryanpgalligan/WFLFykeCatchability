## This script is used to clean and explore data from the
## 1999-2024 fyke survey and the 2019-2024 water quality data.

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

# Make a binary catch column wherein 1 = winter flounder caught and 0 = no winter flounder caught
fyke$wfl_binary <- ifelse(fyke$wfl_freq == 0,0,1)
fyke$wfl_binary <- as.factor(fyke$wfl_binary)

# Calculate Julian day
fyke$haul.date <- as.POSIXlt(fyke$haul.date, format="%d-%m-%y")
fyke$haul.date_jul <- as.numeric(format(fyke$haul.date, "%j"))
  
# Convert Julian Day to be 1 at Nov 1 as that is earliest conceivable start date
fyke$haul.date_jul <- ifelse(fyke$haul.date_jul < 200,
  fyke$haul.date_jul + 61, fyke$haul.date_jul - 304)

# Convert set air temp from F to C
fyke$set.air.temp_c <- (fyke$set.air.temp_c - 32) * 5/9
fyke$set.air.temp_c <- round2(fyke$set.air.temp_c, 2)

# Convert haul air temp from F to C
fyke$haul.air.temp_c <- (fyke$haul.air.temp_c - 32) * 5/9
fyke$haul.air.temp_c <- round2(fyke$haul.air.temp_c, 2)

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

# Add columns to fyke data for summary statistics
fyke$mean.water.temp_c <- NA #haul and set temps are normally distributed
fyke$min.water.temp_c <- NA
fyke$max.water.temp_c <- NA
fyke$


# Add summary statistics to fyke data
for(i in 1:nrow(fyke)) {
  
  # Subset water quality data for the current fyke event
  water_sub <- filter(water, event == fyke$event[i])
  
  
  
  
}
  



##### Summarize #####

# Summary of fyke data
gt_plt_summary(fyke[6:21])






