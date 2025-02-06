## Additional figures

## Load data
flounder<- read.csv("data/clean-data/02_FishLengths.csv")
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
ggsave("figures/05_effort_by_pond.png", width = 8, height = 6, units = "in")




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




## Length distributions for all three ponds in recent years with good effort

# Filter data for years beginning in 2020
data <- flounder %>%
  filter(year >= 2020)

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
ggsave("figures/05_lengths-since-2020.png", width = 8, height = 6, units = "in")




## Length distribution for PJ pond in first five years

# Subset data
data <- flounder %>%
  filter(year <= 2003, pond == "PJ")

# Plot density distribution
ggplot(data, aes(x = length_cm)) +
  geom_density(fill = "#0C7BDC", alpha = 0.5) +
  labs(x = "Length (cm)", y = "Density") +
  theme_pubr()

# Save plot
ggsave("figures/05_lengths-before-2003.png", width = 8, height = 6, units = "in")




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




