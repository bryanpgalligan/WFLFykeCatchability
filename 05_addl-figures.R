## Additional figures

## Load data
flounder <- read.csv("data/clean-data/02_FishLengths.csv")
fyke <- read_csv("data/clean-data/02_FykeSets_Complete.csv")
fyke_cvars <- read_csv("data/clean-data/03_FykeSets_CandidateVars_1999.csv")

## Effort by pond through time (stacked bars)

# Custom color palette
colors <- c("Potter" = "#FFC20A", "Ninigret" = "#D41159", "Point Judith" = "#0C7BDC")

# Rename ponds
for(i in 1:nrow(fyke)){
  if(fyke$pond[i] == "PP"){
    fyke$pond[i] <- "Potter"
  } else if(fyke$pond[i] == "NP"){
    fyke$pond[i] <- "Ninigret"
  } else if(fyke$pond[i] == "PJ"){
    fyke$pond[i] <- "Point Judith"
  }
}

# Group by haul.winter and pond and count
data <- fyke %>%
  group_by(haul.winter, pond) %>%
  summarise(n = n()) %>%
  ungroup()

# Create a stacked bar plot
ggplot(data, aes(x = haul.winter, y = n, fill = pond)) +
  geom_bar(stat = "identity") +  # Use identity to keep original values
  labs(x = "Year", y = "No. of Fyke Sets") +
  theme_pubr() + 
  scale_fill_manual(values = colors) +
  theme(legend.title = element_blank())

# Save bar plot
ggsave("figures/05_effort_by_pond.png", width = 6, height = 4, units = "in")




## Table of effort by station through time

# Group by haul.winter and station and count
data <- fyke %>%
  group_by(haul.winter, station) %>%
  summarise(n = n()) %>%
  ungroup()

# Add pond column
data$pond <- substr(data$station, 1, 2)

# Create a data frame for PJ pond
pj <- data %>%
  filter(pond == "PJ")

# Make station first column and n by haul.winter columns
pj <- pj %>%
  spread(key = haul.winter, value = n)

# Remove pond column
pj <- select(pj, -pond)

# Create a data frame for NP pond
np <- data %>%
  filter(pond == "NP")

# Make station first column and n by haul.winter columns
np <- np %>%
  spread(key = haul.winter, value = n)

# Remove pond column
np <- select(np, -pond)

# Create a data frame for PP pond
pp <- data %>%
  filter(pond == "PP")

# Make station first column and n by haul.winter columns
pp <- pp %>%
  spread(key = haul.winter, value = n)

# Remove pond column
pp <- select(pp, -pond)


# Save data frames
write.csv(pj, "tables/05_effort_by_station_pj.csv", row.names = FALSE, na = "")
write.csv(np, "tables/05_effort_by_station_np.csv", row.names = FALSE, na = "")
write.csv(pp, "tables/05_effort_by_station_pp.csv", row.names = FALSE, na = "")




## Length distributions for all three ponds

# Temporary data file
data <- flounder

# Rename ponds
for(i in 1:nrow(data)){
  if(data$pond[i] == "PP"){
    data$pond[i] <- "Potter"
  } else if(data$pond[i] == "NP"){
    data$pond[i] <- "Ninigret"
  } else if(data$pond[i] == "PJ"){
    data$pond[i] <- "Point Judith"
  }
}

# Remove outliers
data <- data %>%
  filter(length_cm < 50)

# Plot of density distributions for each pond
ggplot(data, aes(x = length_cm, fill = pond)) +
  geom_density(alpha = 0.5) +
  labs(x = "Length (cm)", y = "Density") +
  theme_pubr() +
  scale_fill_manual(values = colors) +
  theme(legend.title = element_blank())

# Save density plot
ggsave("figures/05_length-distributions.png", width = 4, height = 4, units = "in")




##### Summary statistics #####

# Function to find SE (standard error) of the mean of a vector
se.mean <- function(x){
  
  # If there are NA in x
  if(sum(is.na(x)) > 0){
    
    # Remove NA
    x <- na.omit(x)
  }
  
  sd(x)/sqrt(length(x))
  
}
  

# Soak period
mean(fyke$soak_days, na.rm = TRUE)
se.mean(fyke$soak_days)
min(fyke$soak_days, na.rm = TRUE)
max(fyke$soak_days, na.rm = TRUE)

# Lunar illumination
mean(fyke$lunar.illumination, na.rm = TRUE)
se.mean(fyke$lunar.illumination)
min(fyke$lunar.illumination, na.rm = TRUE)
max(fyke$lunar.illumination, na.rm = TRUE)

# Set occurrence
mean(fyke$set.occurrence_yr, na.rm = TRUE)
se.mean(fyke$set.occurrence_yr)
min(fyke$set.occurrence_yr, na.rm = TRUE)
max(fyke$set.occurrence_yr, na.rm = TRUE)

# Water temperature
mean(fyke_cvars$water.temp_c, na.rm = TRUE)
se.mean(fyke_cvars$water.temp_c)
min(fyke_cvars$water.temp_c, na.rm = TRUE)
max(fyke_cvars$water.temp_c, na.rm = TRUE)

# Air temperature
mean(fyke$noaa.avg.air.temp_c, na.rm = TRUE)
se.mean(fyke$noaa.avg.air.temp_c)
min(fyke$noaa.avg.air.temp_c, na.rm = TRUE)
max(fyke$noaa.avg.air.temp_c, na.rm = TRUE)

# Air temperature range
mean(fyke_cvars$air.temp.range_c, na.rm = TRUE)
se.mean(fyke_cvars$air.temp.range_c)
min(fyke_cvars$air.temp.range_c, na.rm = TRUE)
max(fyke_cvars$air.temp.range_c, na.rm = TRUE)

# Heating degree days
mean(fyke$noaa.heating.degrees_day, na.rm = TRUE)
se.mean(fyke$noaa.heating.degrees_day)
min(fyke$noaa.heating.degrees_day, na.rm = TRUE)
max(fyke$noaa.heating.degrees_day, na.rm = TRUE)

# Salinity
mean(fyke_cvars$salinity_ppt, na.rm = TRUE)
se.mean(fyke_cvars$salinity_ppt)
min(fyke_cvars$salinity_ppt, na.rm = TRUE)
max(fyke_cvars$salinity_ppt, na.rm = TRUE)

# Precipitation
mean(fyke_cvars$precip_mm.day, na.rm = TRUE)
se.mean(fyke_cvars$precip_mm.day)
min(fyke_cvars$precip_mm.day, na.rm = TRUE)
max(fyke_cvars$precip_mm.day, na.rm = TRUE)

# Wind speed
mean(fyke_cvars$wind_m.s, na.rm = TRUE)
se.mean(fyke_cvars$wind_m.s)
min(fyke_cvars$wind_m.s, na.rm = TRUE)
max(fyke_cvars$wind_m.s, na.rm = TRUE)

# Mean length
mean(flounder$length_cm, na.rm = TRUE)
se.mean(flounder$length_cm)

# Plot temp and DOY
ggplot(fyke_cvars, aes(x = haul.date_jul, y = water.temp_c)) +
  geom_point() +
  labs(x = "Day of Year", y = "Water Temperature (C)") +
  theme_pubr()

# Temp
temp <- filter(fyke_cvars, water.temp_c >= 9.5)

# Convert minimum julian day to date
as.Date(min(temp$haul.date_jul), origin = "2011-11-01")

# Water temp range
mean(fyke$range.water.temp_c, na.rm = TRUE)
se.mean(fyke$range.water.temp_c)
min(fyke$range.water.temp_c, na.rm = TRUE)
max(fyke$range.water.temp_c, na.rm = TRUE)

# Water temp skewness
mean(fyke$skewness.water.temp, na.rm = TRUE)
se.mean(fyke$skewness.water.temp)
min(fyke$skewness.water.temp, na.rm = TRUE)
max(fyke$skewness.water.temp, na.rm = TRUE)

# Water temp kurtosis
mean(fyke$kurtosis.water.temp, na.rm = TRUE)
se.mean(fyke$kurtosis.water.temp)
min(fyke$kurtosis.water.temp, na.rm = TRUE)
max(fyke$kurtosis.water.temp, na.rm = TRUE)

# Water temp bimodality
mean(fyke$bimodality.water.temp, na.rm = TRUE)
se.mean(fyke$bimodality.water.temp)
min(fyke$bimodality.water.temp, na.rm = TRUE)
max(fyke$bimodality.water.temp, na.rm = TRUE)

# Delta salinity
mean(fyke$delta.salinity_ppt, na.rm = TRUE)
se.mean(fyke$delta.salinity_ppt)
min(fyke$delta.salinity_ppt, na.rm = TRUE)
max(fyke$delta.salinity_ppt, na.rm = TRUE)

# Dissolved oxygen
fyke19 <- read.csv("data/clean-data/03_FykeSets_CandidateVars_2019.csv")
mean(fyke19$do_mg.l, na.rm = TRUE)
se.mean(fyke19$do_mg.l)
min(fyke19$do_mg.l, na.rm = TRUE)
max(fyke19$do_mg.l, na.rm = TRUE)

# Delta DO
mean(fyke19$delta.do_mg.l.day, na.rm = TRUE)
se.mean(fyke19$delta.do_mg.l.day)
min(fyke19$delta.do_mg.l.day, na.rm = TRUE)
max(fyke19$delta.do_mg.l.day, na.rm = TRUE)

# Depth range
mean(fyke19$range.depth_m, na.rm = TRUE)
se.mean(fyke19$range.depth_m)
min(fyke19$range.depth_m, na.rm = TRUE)
max(fyke19$range.depth_m, na.rm = TRUE)

# Min water temp
mean(fyke$min.water.temp_c, na.rm = TRUE)
se.mean(fyke$min.water.temp_c)
min(fyke$min.water.temp_c, na.rm = TRUE)
max(fyke$min.water.temp_c, na.rm = TRUE)

# Max water temp
mean(fyke$max.water.temp_c, na.rm = TRUE)
se.mean(fyke$max.water.temp_c)
min(fyke$max.water.temp_c, na.rm = TRUE)
max(fyke$max.water.temp_c, na.rm = TRUE)



