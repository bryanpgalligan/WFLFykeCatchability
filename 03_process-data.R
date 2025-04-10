## This script processes the complete dataset and
## generates the datasets for the analysis.

# Load data
fyke <- read.csv("data/clean-data/02_FykeSets_Complete.csv")

# Create two versions, one for 1999 and one for the years with in situ loggers

# Full dataset
fyke99 <- fyke

# Dataset that includes only the sets with complete data from in situ loggers
fyke19 <- fyke[!is.na(fyke$mean.depth_m),]




##### Select Variables - fyke99 #####



## Water temp
# Create a composite water temp column in the following order of priority:
# hs.avg.water.temp_c, haul.water.temp_c, set.water.temp_c, mean.water.temp_c
fyke99$water.temp_c <- NA
fyke99$water.source <- NA
for (i in 1:nrow(fyke99)){
  
  # Order of priority is: hs.avg.water.temp_c, haul.water.temp_c, set.water.temp_c, mean.water.temp_c
  if (!is.na(fyke99$hs.avg.water.temp_c[i])){
    fyke99$water.temp_c[i] <- fyke99$hs.avg.water.temp_c[i]
    fyke99$water.source[i] <- "hs.avg.water.temp_c"
  } else if (!is.na(fyke99$haul.water.temp_c[i])){
    fyke99$water.temp_c[i] <- fyke99$haul.water.temp_c[i]
    fyke99$water.source[i] <- "haul.water.temp_c"
  } else if (!is.na(fyke99$set.water.temp_c[i])){
    fyke99$water.temp_c[i] <- fyke99$set.water.temp_c[i]
    fyke99$water.source[i] <- "set.water.temp_c"
  } else {
    fyke99$water.temp_c[i] <- fyke99$mean.water.temp_c[i]
    fyke99$water.source[i] <- "mean.water.temp_c"
  }
  
}

# Summary of water temp
gt_plt_summary(fyke99[52:53])

# 2.1% are now missing water temp (1055 total sets)
## 854/1055 = 80.95% are haul/set average
## 145/1055 = 13.74% are haul temp
## 34/1055 = 3.22% are set temp
## 22/1055 = 2.09% are missing


## Salinity
# Create a composite salinity column in the following order of priority:
# hs.avg.salinity_ppt, haul.salinity_ppt, set.salinity_ppt
fyke99$salinity_ppt <- NA
fyke99$salinity.source <- NA
for (i in 1:nrow(fyke99)){
  
  # Order of priority is: hs.avg.salinity_ppt, haul.salinity_ppt, set.salinity_ppt
  if (!is.na(fyke99$hs.avg.salinity_ppt[i])){
    fyke99$salinity_ppt[i] <- fyke99$hs.avg.salinity_ppt[i]
    fyke99$salinity.source[i] <- "hs.avg.salinity_ppt"
  } else if (!is.na(fyke99$haul.salinity_ppt[i])){
    fyke99$salinity_ppt[i] <- fyke99$haul.salinity_ppt[i]
    fyke99$salinity.source[i] <- "haul.salinity_ppt"
  } else {
    fyke99$salinity_ppt[i] <- fyke99$set.salinity_ppt[i]
    fyke99$salinity.source[i] <- "set.salinity_ppt"
  }
  
}

# Summary of salinity
gt_plt_summary(fyke99[54:55])

# 9.1% are missing (1055 total sets)
# 800/1055 = 75.83% are hs.avg.salinity_ppt
# 127/1055 = 12.04% are haul.salinity_ppt
# 32/1055 = 3.03% are set.salinity_ppt


## DO
# Create a composite DO column in the following order of priority:
# hs.avg.do_mg.l, haul.do_mg.l, set.do_mg.l
fyke99$do_mg.l <- NA
fyke99$do.source <- NA
for (i in 1:nrow(fyke99)){
  
  # Order of priority is: hs.avg.do_mg.l, haul.do_mg.l, set.do_mg.l
  if (!is.na(fyke99$hs.avg.do_mg.l[i])){
    fyke99$do_mg.l[i] <- fyke99$hs.avg.do_mg.l[i]
    fyke99$do.source[i] <- "hs.avg.do_mg.l"
  } else if (!is.na(fyke99$haul.do_mg.l[i])){
    fyke99$do_mg.l[i] <- fyke99$haul.do_mg.l[i]
    fyke99$do.source[i] <- "haul.do_mg.l"
  } else {
    fyke99$do_mg.l[i] <- fyke99$set.do_mg.l[i]
    fyke99$do.source[i] <- "set.do_mg.l"
  }
  
}

# Summary of DO
gt_plt_summary(fyke99[56:57])

# 24.5% are still missing (1055 total sets)
# 663/1055 = 62.84% are hs.avg.do_mg.l
# 288/1055 = 27.30% are set.do_mg.l
# 104/1055 = 9.86% are haul.do_mg.l



## Select and reorder columns

# Criteria for inclusion: <=15% missing data

# This deletes the following columns:
# Spatial data:
# Time/Date: set.date, haul.date, haul.month, haul.year
# Sampling: event
# Tide: mean.depth_m, min.depth_m, max.depth_m, range.depth_m
# Water temp: hs.avg.water.temp_c, haul.water.temp_c, set.water.temp_c, mean.water.temp_c,
#     min.water.temp_c, max.water.temp_c, range.water.temp_c, skewness.water.temp, kurtosis.water.temp,
#     bimodality.water.temp, delta.water.temp_c, water.source, delta.water.temp_c.day
# Air temp: set.air.temp_c, haul.air.temp_c, hs.avg.air.temp_c, noaa.max.temp_c, noaa.min.temp_c,
#     noaa.avg.high_c
# Salinity: set.salinity_ppt, haul.salinity_ppt, hs.avg.salinity_ppt, delta.salinity_ppt, salinity.source
#     delta.salinity_ppt.day
# DO: set.do_mg.l, haul.do_mg.l, hs.avg.do_mg.l, delta.do_mg.l, delta.do_mg.l.day, do_mg.l, do.source
# Catch:
# Precip:
# Wind:

# Select columns
fyke99 <- select(fyke99,
  pond, station, #spatial data
  haul.winter, haul.date_jul, #time/date
  soak_days, set.occurrence_yr, #sampling
  lunar.illumination, #tide
  water.temp_c, #water temp
  noaa.avg.air.temp_c, noaa.temp.range_c, noaa.heating.degrees_day, #air temp
  salinity_ppt, #salinity
  noaa.precip_mm.day, #precip
  noaa.wind_m.s, #wind
  wfl_freq, wfl_binary #catch
  )

# Rename columns
fyke99 <- rename(fyke99,
  pond = pond,
  station = station,
  haul.winter = haul.winter,
  haul.date_jul = haul.date_jul,
  soak_days = soak_days,
  set.occurrence_yr = set.occurrence_yr,
  lunar.illumination = lunar.illumination,
  water.temp_c = water.temp_c,
  air.temp_c = noaa.avg.air.temp_c,
  air.temp.range_c = noaa.temp.range_c,
  heating.degrees_day = noaa.heating.degrees_day,
  salinity_ppt = salinity_ppt,
  wfl_freq = wfl_freq,
  wfl_binary = wfl_binary,
  precip_mm.day = noaa.precip_mm.day,
  wind_m.s = noaa.wind_m.s
  )


## Identify autocorrelated variables

# Subset of numeric columns
fyke99_num <- select_if(fyke99, is.numeric)

# Correlation matrix
cor_matrix <- cor(fyke99_num, use = "pairwise.complete.obs", method = "spearman")

# Plot correlations
corrplot(cor_matrix)

# Make correlation matrix a data frame
cor_matrix <- as.data.frame(as.table(cor_matrix))

# Turn all 1's into NA
cor_matrix$Freq[cor_matrix$Freq == 1] <- NA

# Make all values absolute value
cor_matrix$Freq <- abs(cor_matrix$Freq)


## Variables with correlation coefficient >0.7
# heating.degrees_day * air.temp_c [1.00]
# heating.degrees_day * water.temp_c [0.80]
# air.temp_c * water.temp_c [0.80]

# Remove heating.degrees_day
fyke99 <- select(fyke99, -heating.degrees_day)

# Remove air temp
fyke99 <- select(fyke99, -air.temp_c)

# Summary of Fyke 1999+ dataset
fyke.summary <- gt_plt_summary(fyke99[4:14], "Processed Fyke Sets 1999-2024")

# Save summary
gtsave(fyke.summary, "documents/03_FykeSummary_CandidateVars_1999.png")

# Save dataset
write_csv(fyke99, "data/clean-data/03_FykeSets_CandidateVars_1999.csv")




##### Select Variables - fyke19 #####

# Summarize fyke19
gt_plt_summary(fyke19[3:51])

## DO
# Make a composite DO column in the following order of priority:
# hs.avg.do_mg.l, haul.do_mg.l, set.do_mg.l
fyke19$do_mg.l <- NA
fyke19$do.source <- NA
for (i in 1:nrow(fyke19)){
  
  # Order of priority is: hs.avg.do_mg.l, haul.do_mg.l, set.do_mg.l
  if (!is.na(fyke19$hs.avg.do_mg.l[i])){
    fyke19$do_mg.l[i] <- fyke19$hs.avg.do_mg.l[i]
    fyke19$do.source[i] <- "hs.avg.do_mg.l"
  } else if (!is.na(fyke19$haul.do_mg.l[i])){
    fyke19$do_mg.l[i] <- fyke19$haul.do_mg.l[i]
    fyke19$do.source[i] <- "haul.do_mg.l"
  } else {
    fyke19$do_mg.l[i] <- fyke19$set.do_mg.l[i]
    fyke19$do.source[i] <- "set.do_mg.l"
  }
  
}

# Summary of data
gt_plt_summary(fyke19[52:53])

# 10.3% are still missing (439 total sets)
# 391/439 = 89.07% are haul/set average
# 47/439 = 10.70% are set DO
# 1/439 = 0.23% are haul DO

## Select and reorder columns

# Criteria for inclusion: <=15% missing data

# This deletes the following columns:
# Spatial data:
# Time/Date: set.date, haul.date, haul.month, haul.year
# Sampling: event
# Tide: mean.depth_m, min.depth_m, max.depth_m
# Water temp: set.water.temp_c, hs.avg.water.temp_c, haul.water.temp_c, delta.water.temp_c,
#     delta.water.temp_c.day
# Air temp: set.air.temp_c, haul.air.temp_c, hs.avg.air.temp_c, noaa.max.temp_c, noaa.min.temp_c,
#     noaa.avg.high_c
# Salinity: set.salinity_ppt, haul.salinity_ppt, delta.salinity_ppt
# DO: set.do_mg.l, haul.do_mg.l, hs.avg.do_mg.l, delta.do_mg.l, do.source
# Catch:
# Precip:
# Wind:

# Select columns
fyke19 <- select(fyke19,
  pond, station, #spatial data
  haul.winter, haul.date_jul, #time/date
  soak_days, set.occurrence_yr, #sampling
  lunar.illumination, range.depth_m, #tide
  mean.water.temp_c, min.water.temp_c, max.water.temp_c, range.water.temp_c, skewness.water.temp,
    kurtosis.water.temp, bimodality.water.temp, #water temp
  noaa.avg.air.temp_c, noaa.temp.range_c, noaa.heating.degrees_day, #air temp
  hs.avg.salinity_ppt, delta.salinity_ppt.day, #salinity
  do_mg.l, delta.do_mg.l.day, #DO
  noaa.precip_mm.day, #precip
  noaa.wind_m.s, #wind
  wfl_freq, wfl_binary #catch
  )

# Rename columns
fyke19 <- rename(fyke19,
  pond = pond,
  station = station,
  haul.winter = haul.winter,
  haul.date_jul = haul.date_jul,
  soak_days = soak_days,
  set.occurrence_yr = set.occurrence_yr,
  lunar.illumination = lunar.illumination,
  range.depth_m = range.depth_m,
  mean.water.temp_c = mean.water.temp_c,
  min.water.temp_c = min.water.temp_c,
  max.water.temp_c = max.water.temp_c,
  range.water.temp_c = range.water.temp_c,
  skewness.water.temp = skewness.water.temp,
  kurtosis.water.temp = kurtosis.water.temp,
  bimodality.water.temp = bimodality.water.temp,
  air.temp_c = noaa.avg.air.temp_c,
  air.temp.range_c = noaa.temp.range_c,
  heating.degrees_day = noaa.heating.degrees_day,
  salinity_ppt = hs.avg.salinity_ppt,
  delta.salinity_ppt.day = delta.salinity_ppt.day,
  do_mg.l = do_mg.l,
  delta.do_mg.l.day = delta.do_mg.l.day,
  precip_mm.day = noaa.precip_mm.day,
  wind_m.s = noaa.wind_m.s,
  wfl_freq = wfl_freq,
  wfl_binary = wfl_binary
  )


## Identify autocorrelated variables

# Subset of numeric columns
fyke19_num <- select_if(fyke19, is.numeric)

# Correlation matrix
cor_matrix <- cor(fyke19_num, use = "pairwise.complete.obs", method = "spearman")

# Plot correlations
corrplot(cor_matrix)

# Make correlation matrix a data frame
cor_matrix <- as.data.frame(as.table(cor_matrix))

# Turn all 1's into NA
cor_matrix$Freq[cor_matrix$Freq == 1] <- NA

# Make all values absolute value
cor_matrix$Freq <- abs(cor_matrix$Freq)


## Variables with correlation coefficient >0.7
# heating.degrees_day * air.temp_c [1.00]
# min.water.temp_c * mean.water.temp_c [0.92]
# max.water.temp_c * mean.water.temp_c [0.89]
# air.temp_c * mean.water.temp_c [0.78]
# heating.degrees_day * mean.water.temp_c [0.78]
# air.temp_c * min.water.temp_c [0.76]
# heating.degrees_day * min.water.temp_c [0.76]
# air.temp_c * max.water.temp_c [0.70]
# heating.degrees_day * max.water.temp_c [0.70]
# max.water.temp_c * min.water.temp_c [0.70]

# Remove heating.degrees_day
fyke19 <- select(fyke19, -heating.degrees_day)

## Remove min and max water temp because both are highly correlated with the mean and with each other

# Remove min water temp
fyke19 <- select(fyke19, -min.water.temp_c)

# Remove max water temp
fyke19 <- select(fyke19, -max.water.temp_c)

# Remove air temp
fyke19 <- select(fyke19, -air.temp_c)


# Summary of Fyke 2019+ dataset
fyke.summary <- gt_plt_summary(fyke19[4:22], "Processed Fyke Sets 2019-2024")

# Save summary
gtsave(fyke.summary, "documents/03_FykeSummary_CandidateVars_2019.png")

# Save dataset
write_csv(fyke19, "data/clean-data/03_FykeSets_CandidateVars_2019.csv")







