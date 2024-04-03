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



##### Summarize #####

# Summary of fyke data
gt_plt_summary(fyke[6:21])






