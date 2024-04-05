## This script processes the complete dataset and
## generates the final datasets for the analysis.

# Load data
fyke <- read.csv("data/clean-data/02_FykeSets_Complete.csv")




##### Initial Processing #####

# Remove extra date columns
fyke <- fyke %>%
  select(-set.date, -haul.date, -haul.month, -haul.year)

# Add a water temp column
fyke$water.temp_c <- NA
for (i in 1:nrow(fyke)){
  
  # Order of priority is: hs.avg.water.temp_c, mean.water.temp_c, haul.water.temp_c, set.water.temp_c
  if (!is.na(fyke$hs.avg.water.temp_c[i])){
    fyke$water.temp_c[i] <- fyke$hs.avg.water.temp_c[i]
  } else if (!is.na(fyke$mean.water.temp_c[i])){
    fyke$water.temp_c[i] <- fyke$mean.water.temp_c[i]
  } else if (!is.na(fyke$haul.water.temp_c[i])){
    fyke$water.temp_c[i] <- fyke$haul.water.temp_c[i]
  } else {
    fyke$water.temp_c[i] <- fyke$set.water.temp_c[i]
  }
  
}

# Remove extra water temp columns
fyke <- select(fyke, -hs.avg.water.temp_c, -haul.water.temp_c, -set.water.temp_c)


# Add an air temp column
fyke$air.temp_c <- NA
for (i in 1:nrow(fyke)){
  
  # Order of priority is: hs.avg.air.temp_c, haul.air.temp_c, set.air.temp_c, noaa.avg.high_c
  if (!is.na(fyke$hs.avg.air.temp_c[i])){
    fyke$air.temp_c[i] <- fyke$hs.avg.air.temp_c[i]
  } else if (!is.na(fyke$haul.air.temp_c[i])){
    fyke$air.temp_c[i] <- fyke$haul.air.temp_c[i]
  } else if (!is.na(fyke$set.air.temp_c[i])){
    fyke$air.temp_c[i] <- fyke$set.air.temp_c[i]
  } else {
    fyke$air.temp_c[i] <- fyke$noaa.avg.high_c[i]
  }
  
}

# Recalculate heating degrees per day based on new air temp column
fyke$heating.degrees_day <- 
  ifelse(fyke$air.temp_c < 18, 18 - fyke$air.temp_c, 0)

# Remove extra air temp columns
fyke <- select(fyke, -hs.avg.air.temp_c, -noaa.avg.air.temp_c,
  -haul.air.temp_c, -set.air.temp_c, -noaa.heating.degrees_day,
  -noaa.avg.high_c)

# Add a salinity column
fyke$salinity_ppt <- NA
for (i in 1:nrow(fyke)){
  
  # Order of priority is: hs.avg.salinity_ppt, haul.salinity_ppt, set.salinity_ppt
  if (!is.na(fyke$hs.avg.salinity_ppt[i])){
    fyke$salinity_ppt[i] <- fyke$hs.avg.salinity_ppt[i]
  } else if (!is.na(fyke$haul.salinity_ppt[i])){
    fyke$salinity_ppt[i] <- fyke$haul.salinity_ppt[i]
  } else {
    fyke$salinity_ppt[i] <- fyke$set.salinity_ppt[i]
  }
  
}

# Remove extra salinity columns
fyke <- select(fyke, -hs.avg.salinity_ppt, -haul.salinity_ppt, -set.salinity_ppt)


# Add a DO column
fyke$do_mg.l <- NA
for (i in 1:nrow(fyke)){
  
  # Order of priority is: hs.avg.do_mg.l, haul.do_mg.l, set.do_mg.l
  if (!is.na(fyke$hs.avg.do_mg.l[i])){
    fyke$do_mg.l[i] <- fyke$hs.avg.do_mg.l[i]
  } else if (!is.na(fyke$haul.do_mg.l[i])){
    fyke$do_mg.l[i] <- fyke$haul.do_mg.l[i]
  } else {
    fyke$do_mg.l[i] <- fyke$set.do_mg.l[i]
  }
  
}

# Remove extra DO columns
fyke <- select(fyke, -hs.avg.do_mg.l, -haul.do_mg.l, -set.do_mg.l)

# Remove delta water temp
fyke <- select(fyke, -delta.water.temp_c)

# Remove delta salinity
fyke <- select(fyke, -delta.salinity_ppt)

# Remove delta DO
fyke <- select(fyke, -delta.do_mg.l)




##### Processing for 1999-2024 #####




##### Processing for 2019-2024 #####




# Summarize data
gt_plt_summary(fyke)
