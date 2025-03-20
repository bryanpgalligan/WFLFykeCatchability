## Previous code ##

## Count tagged flounder

# Load raw data
tags <- read_excel("data/raw-data/WinterFlounder.xlsx")

# Count tagged fish
table(tags$Tagged_YN)

# Count fish with a value in Tag_Recapture_Location_1_Lat
table(is.na(tags$Tag_Recapture_Location_1_Lat))

# Count dead fish
table(tags$Dead_YN)

################################################################################
# The actual draft code is below this line
################################################################################

## Load data
fish <- read_excel("data/raw-data/IndividualWFLCaptured.xlsx")
recaps <- read_excel("data/raw-data/RecaptureLocs.xlsx")






########################################################
# Work from Rich is below this line
########################################################

#### Map the relaease and recapture locations of winter flounder ####
library(ggmap)
library(tidyverse)
library(ggplot2)
library(readxl)
library(rio)
library(geosphere)
library(dplyr)
library(leaflet)
library(openxlsx)

# R. Balouskus
# Use query Recapture_Locs from fyke net database

setwd("C:/Users/richard.balouskus/OneDrive - State of Rhode Island/SharedDrive/WinterFlounder/Analysis/Trad_Tags")
gmean <- function(x) exp(mean(log(x)))

#############################################
## This section calculate days at large, growth, and distance between release and capture
#############################################
df <- read_xlsx("Recapture_Locs.xlsx")
df <- df %>%
  rename(
    rel_lat = Latitude,
    rel_lon = Longitude,
    rec_lat = Tag_Recapture_Location_1_Lat,
    rec_lon = Tag_Recapture_Location_1_Lon,
    rel_date = Haul_Date,
    rec_date = Tag_Recapture_Date_1,
    rel_len = Length,
    rec_len = Tag_Recapture_Length_1
  )
### This below needs to be edited - not reading in file correctly, need to set column types
tagged <- read_xlsx("Tagging_Data.xlsx")
tagged <- tagged[which (tagged$Haul_Year_Winter<2025),]

##Make column names more better
# names(df)[5] <- "rel_lat"
# names(df)[6] <- "rel_lon"
# names(df)[7] <- "rec_lat"
# names(df)[8] <- "rec_lon"
# names(df)[9] <- "rel_date"
# names(df)[10] <- "rec_date"
# names(df)[11] <- "rel_len"
# names(df)[12] <- "rec_len"

##############################################################
## General summary stats
# All fish
fish <- 
  tagged %>%
  tally()

# total tagged
tagged_summary <-
  tagged %>%
  group_by(Tagged_YN) %>%
  tally()

# total recaptures
df_summary <-
  df %>%
  summarise(
    count = n()
  )

# total multiple recaptures
recapped2_summary <-
  tagged %>%
  group_by(Recaptured_2_YN) %>%
  tally()

# recaptures by release year
df_summary_rel_year <-
  df %>%
  mutate(year = format(rel_date, "%Y")) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_rel_year, file="recap_byrelyr.csv")
  
# Tagged by pond
tagged$Pond_ID <- substr(tagged$Event_ID,10,11)
tagged_pond_summary <-
  tagged %>%
  group_by(Pond_ID, Tagged_YN) %>%
  tally()

# Recapture by tag Release Pond
df$rel_Pond_ID <- substr(df$Event_ID,10,11)
df_pond_summary <-
  df %>%
  group_by(rel_Pond_ID) %>%
  tally()

#### These below include NAs - may want to remove before analysis, just be aware
# recaptures by recapture year
df_summary_rec_year <-
  df %>%
  mutate(year = format(rec_date, "%Y")) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_rec_year, file="recap_byrecyr.csv")

# Recaptures by gear
df_summary_gear <-
  df %>%
  group_by(Tag_Recapture_Method_1) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_gear, file="df_summary_gear.csv")

# Recapture lengths by gear type
df_summary_gearlen <-
  df %>%
  group_by(Tag_Recapture_Method_1) %>%
  summarise(count = n(),
            mean_rel_len = mean(rel_len),
            median_rel_len = median(rel_len),
            min_rel_len = min(rel_len),
            max_rel_len = max(rel_len)
  )
write.csv(df_summary_gearlen, file="df_summary_gearlen.csv")

# recaptures by recap locations
df_summary_loc <-
  df %>%
  group_by(Tag_Recapture_Location_1) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_loc, file="df_summary_loc.csv")

## Calculate recaptures by month
catch_by_month <-
  df %>%
  mutate(month = format(rec_date, "%m")) %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(catch_by_month, file="catch_by_month.csv")

## Recapture by month by recapture location
catch_by_month_loc <-
  df %>%
  mutate(month = format(rec_date, "%m")) %>%
  group_by(month, Tag_Recapture_Location_1) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(catch_by_month_loc, file="catch_by_month_loc.csv")

## Recapture by month by recapture gear
catch_by_month_gear <-
  df %>%
  mutate(month = format(rec_date, "%m")) %>%
  group_by(month, Tag_Recapture_Method_1) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(catch_by_month_gear, file="catch_by_month_gear.csv")


## Releases by sex
tagged_summary_sex <-
  tagged %>%
  group_by(Tagged_YN, Sex) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(tagged_summary_sex, file="tagged_summary_sex.csv")

## Recapture by sex - this is just recapture - need to compare with tagged releases sex ratio
df_summary_sex <-
  df %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_sex, file="df_summary_sex.csv")

## Releases by spawning stage
tagged_summary_spwnstg <-
  tagged %>%
  group_by(Tagged_YN, Maturity_Stage) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(tagged_summary_spwnstg, file="tagged_summary_spwnstg.csv")

## Recaptures by release spawning stage
df_summary_spwnstg <-
  df %>%
  group_by(Maturity_Stage) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_spwnstg, file="df_summary_spwnstg.csv")

## Releases by length bin
tagged$Length_bin <- cut(tagged$Length, c(10,15,20,25,30,35,40,45,50))
tagged$Length_bin_Lab <- recode_factor(tagged$Length_bin, '(10,15]'="10:15", '(15,20]'="15.1:20", '(20,25]'="20.1:25", '(25,30]'="25.1:30", '(30,35]'="30.1:35", '(35,40]'="35.1:40", '(40,45]'="40.1:45", '(45,50]'="45.1:50")

tagged_summary_lngthbin <-
  tagged %>%
  group_by(Tagged_YN, Length_bin_Lab) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(tagged_summary_lngthbin, file="tagged_summary_lngthbin.csv")

## Recaptures by length bin
df$rel_len_bin <- cut(df$rel_len, c(10,15,20,25,30,35,40,45,50))
df$rel_len_bin_Lab <- recode_factor(df$rel_len_bin, '(10,15]'="10:15", '(15,20]'="15.1:20", '(20,25]'="20.1:25", '(25,30]'="25.1:30", '(30,35]'="30.1:35", '(35,40]'="35.1:40", '(40,45]'="40.1:45", '(45,50]'="45.1:50")

df_summary_spwnstg <-
  df %>%
  group_by(rel_len_bin_Lab) %>%
  summarise(count = n()) %>%
  mutate(rel.freq = paste0(round(100 * count/sum(count), 0), "%"))
write.csv(df_summary_spwnstg, file="df_summary_spwnstg.csv")

##############################################################
## Calculate days at large
## Remove rows with missing release or recapture date data
# df <- read_xlsx("Recapture_Locs.xlsx")
# names(df)[5] <- "rel_lat"
# names(df)[6] <- "rel_lon"
# names(df)[7] <- "rec_lat"
# names(df)[8] <- "rec_lon"
# names(df)[9] <- "rel_date"
# names(df)[10] <- "rec_date"
# names(df)[11] <- "rel_len"
# names(df)[12] <- "rec_len"

df1 <- subset(df, !is.na(rec_date))
df1 <- subset(df1, !is.na(rel_date))
df1$days_at_large <- difftime(df1$rec_date, df1$rel_date, units = c("days"))
df1$days_at_large <- as.numeric(df1$days_at_large)
write.xlsx(df1, "Tag_Recap_DaysatLarge.xlsx", sheetName="DaysatLarge")

## Summary statistics
overall_DaL_stats <- 
  df1 %>%
  summarise(
    count = n(),
    mean_DaL = mean(days_at_large),
    gmean_Dal = gmean(days_at_large),
    median_DaL = median(days_at_large),
    min_DaL = min(days_at_large),
    max_DaL = max(days_at_large)
  )

## Calculate min, max, mean, median days at large by recapture method
DaL_by_Gear <-
  df1 %>%
  group_by(Tag_Recapture_Method_1) %>%
  summarise(
    count = n(),
    mean_DaL = mean(days_at_large),
    gmean_Dal = gmean(days_at_large),
    median_DaL = median(days_at_large),
    min_DaL = min(days_at_large),
    max_DaL = max(days_at_large)
  )


################################################################################
## Calculate growth rate
##remove rows with missing length data from already trimmed date data
df2 <- subset(df1, !is.na(rec_len))
df2 <- subset(df2, !is.na(rel_len))
df2$growth <- df2$rec_len-df2$rel_len
## Convert days at large to number for calculation
df2$days_at_large <- as.numeric(df2$days_at_large)
df2$growth_rate <- (df2$growth)/(df2$days_at_large)

##Length stats
overall_Length_stats <- 
  df2 %>%
  summarise(
    count = n(),
    mean_rel_len = mean(rel_len),
    median_rel_len = median(rel_len),
    min_rel_len = min(rel_len),
    max_rel_len = max(rel_len),
    mean_rec_len = mean(rec_len),
    median_rec_len = median(rec_len),
    min_rec_len = min(rec_len),
    max_rec_len = max(rec_len),
    mean_grwthrate = mean(growth_rate)
  )

Length_by_Gear <-
  df2 %>%
  group_by(Tag_Recapture_Method_1) %>%
  summarise(
    count = n(),
    mean_rec_len = mean(rec_len),
    median_rec_len = median(rec_len),
    min_rec_len = min(rec_len),
    max_rec_len = max(rec_len)
  )

#######################################################################
## Location Analysis
df <- read_xlsx("Recapture_Locs.xlsx")
##Make column names more better
names(df)[5] <- "rel_lat"
names(df)[6] <- "rel_lon"
names(df)[7] <- "rec_lat"
names(df)[8] <- "rec_lon"
names(df)[9] <- "rel_date"
names(df)[10] <- "rec_date"
names(df)[11] <- "rel_len"
names(df)[12] <- "rec_len"
## remove rows with missing location data
df <- subset(df, !is.na(rec_lat))
df <- subset(df, !is.na(rel_lat))

## Calculate distance (km) between release and recapture

df <- mutate(df, dist = distHaversine(cbind(rel_lon, rel_lat), cbind(rec_lon, rec_lat)))
df$dist <- df$dist/1000
df$dist <- round(df$dist, digits=2)

recap_dist <-
  df %>%
  summarise(
    count = n(),
    mean_dist = mean(dist),
    median_dist = median(dist),
    min_dist = min(dist),
    max_dist = max(dist)
  )

## Recap distance by sex
recap_dist_sex <-
  df %>%
  group_by(Sex) %>%
  summarise(
    count = n(),
    mean_dist = mean(dist),
    median_dist = median(dist),
    min_dist = min(dist),
    max_dist = max(dist)
  )
## Recpa distance by size bin

## Plot all relase and recapture points together
##qmplot(rel_lon, rel_lat, data = df, colour = I('white'), size = I(3), darken = .3, maptype = "toner")
##qmplot(rec_lon, rec_lat, data = df, colour = I('green'), size = I(3), darken = .3)
##rel_basemap <- get_stamenmap(location=('rel_lon', 'rel_lat'), zoom=11, source ='stamen', maptype= 'toner')
##ggmap(rel_basemap)

m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = df, lng = ~rel_lon, lat = ~rel_lat) %>%
  addCircleMarkers(data = df, lng = ~rec_lon, lat = ~rec_lat, color="red")

m

## Trying just 1 fish at a time
df1 <- df[1,]

m1 <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = df1, lng = ~rel_lon, lat = ~rel_lat) %>%
  addCircleMarkers(data = df1, lng = ~rec_lon, lat = ~rec_lat, color="red") %>%

m1
