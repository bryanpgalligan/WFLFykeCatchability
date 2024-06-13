## This script uses the RFM algorithm to analyze the data.

## Load data
fyke99 <- read.csv("data/clean-data/03_FykeSets_CandidateVars_1999.csv")
fyke19 <- read.csv("data/clean-data/03_FykeSets_CandidateVars_2019.csv")

# Make all columns numeric or factor
fyke99$pond <- as.factor(fyke99$pond)
fyke19$pond <- as.factor(fyke19$pond)
fyke99$station <- as.factor(fyke99$station)
fyke19$station <- as.factor(fyke19$station)
fyke99$wfl_binary <- as.factor(fyke99$wfl_binary)
fyke19$wfl_binary <- as.factor(fyke19$wfl_binary)


##### WIP - VSURF on 1999 data w/ binary response #####

# Remove freq response column
fyke99_binary <- select(fyke99, -wfl_freq)

# Impute NAs using random forest proximity
set.seed(45612)
fyke99_binary <- rfImpute(wfl_binary ~ ., data = fyke99_binary)

# Select variables using VSURF
set.seed(45612)
vfyke99_bin <- VSURF(wfl_binary ~ ., data = fyke99_binary)

summary(vfyke99_bin)

plot(vfyke99_bin)


# Subset data for random forest
fyke99_binary <- select(fyke99_binary, c("wfl_binary", "station", "haul.date_jul", "haul.winter"))

# Bootstrap sample for model training
set.seed(45612)
fyke99_binary_train <- slice_sample(fyke99_binary, prop = 1, replace = TRUE)

# Out of bag sample for testing
set.seed(45612)
fyke99_binary_test <- setdiff(fyke99_binary, fyke99_binary_train)

# Data for RFM
x <- select(fyke99_binary_train, -wfl_binary)
y <- fyke99_binary_train$wfl_binary
xtest <- select(fyke99_binary_test, -wfl_binary)
ytest <- fyke99_binary_test$wfl_binary

# Tune RFM for optimal mtry
set.seed(45612)
tuneRF(x = x, y = y)

# mtry 2 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f99_binary <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f99_binary)

# Summary
rf_f99_binary




##### WIP - VSURF on 1999 data w/ freq response #####


# Remove binary response column
fyke99_freq <- select(fyke99, -wfl_binary)

# Impute NAs using random forest proximity
set.seed(45612)
fyke99_freq <- rfImpute(wfl_freq ~ ., data = fyke99_freq)

# Select variables using VSURF
set.seed(45612)
vfyke99_freq <- VSURF(wfl_freq ~ ., data = fyke99_freq)

summary(vfyke99_bin)

plot(vfyke99_bin)


# Subset data for random forest
fyke99_freq <- select(fyke99_freq, c("wfl_freq", "haul.winter", "haul.date_jul", "station", "water.temp_c", "air.temp_c", "soak_days", "pond", "salinity_ppt", "set.occurrence_yr"))

# Bootstrap sample for model training
set.seed(45612)
fyke99_freq_train <- slice_sample(fyke99_freq, prop = 1, replace = TRUE)

# Out of bag sample for testing
fyke99_freq_test <- setdiff(fyke99_freq, fyke99_freq_train)

# Data for RFM
x <- select(fyke99_freq_train, -wfl_freq)
y <- fyke99_freq_train$wfl_freq
xtest <- select(fyke99_freq_test, -wfl_freq)
ytest <- fyke99_freq_test$wfl_freq

# Tune RFM for optimal mtry
set.seed(45612)
tuneRF(x = x, y = y)

# mtry 9 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f99_freq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 9,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f99_freq)

# Summary
rf_f99_freq












##### Fyke99 - Water Temp * Bin #####

# Remove unwanted columns
fyke99_waterbin <- select(fyke99, -c(air.temp_c, wfl_freq))

# Impute NAs using random forest proximity
set.seed(123)
fyke99_waterbin <- rfImpute(wfl_binary ~ ., data = fyke99_waterbin)

# Add random numbers
set.seed(123)
fyke99_waterbin$random <- runif(nrow(fyke99_waterbin), min = 1, max = 100)

# Bootstrap sample for model training
set.seed(123)
fyke99_waterbin_train <- slice_sample(fyke99_waterbin, prop = 1, replace = TRUE)

# Out of bag sample for testing
set.seed(123)
fyke99_waterbin_test <- setdiff(fyke99_waterbin, fyke99_waterbin_train)

# Data for RFM
x <- select(fyke99_waterbin_train, -wfl_binary)
y <- fyke99_waterbin_train$wfl_binary
xtest <- select(fyke99_waterbin_test, -wfl_binary)
ytest <- fyke99_waterbin_test$wfl_binary

# Tune RFM for optimal mtry
set.seed(123)
tuneRF(x = x, y = y)

# mtry 2 is optimum based on OOB error

# Random forest model
set.seed(123)
rf_f99_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterbin)

# Summary
rf_f99_waterbin

# Error rate on training data is 8.53% and on testing data is 21.77%

# Variable importance
ImpData <- as.data.frame(importance(rf_f99_waterbin))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$MeanDecreaseAccuracy)
#colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
ggplot(ImpData, aes(x = Var.Names, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Mean Decrease in Accuracy") +
  theme_pubr() +
  theme(legend.position = "bottom")

# Select only variables that outperformed random numbers
x <- select(x, haul.winter, station, haul.date_jul, salinity_ppt, air.temp.range_c)
xtest <- select(xtest, haul.winter, station, haul.date_jul, salinity_ppt, air.temp.range_c)

# Trimmed RFM
set.seed(123)
rf_f99_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterbin)

# Summary
rf_f99_waterbin

# Error rate of 7.68% on training data and 20.16% on testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f99_waterbin))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$MeanDecreaseAccuracy)
#colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
a <- ggplot(ImpData, aes(x = Var.Names, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Mean Decrease in Accuracy") +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Haul winter
pd_winter <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "haul.winter", which.class = 1,
  plot = FALSE, n.pt = 200))
b <- ggplot(pd_winter, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Haul date
pd_date <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "haul.date_jul", which.class = 1,
  plot = FALSE, n.pt = 200))
c <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Station
pd_station <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "station", which.class = 1,
  plot = FALSE))
pd_station$x <- fct_reorder(pd_station$x, pd_station$y, .desc = TRUE)
d <- ggplot(pd_station, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Air temp range
pd_air <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "air.temp.range_c", which.class = 1,
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_air, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Air Temp Range (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Salinity
pd_salinity <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "salinity_ppt", which.class = 1,
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Salinity (ppt)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()


# Combine plots
ggarrange(a, b, c, d, e, f, ncol = 2, nrow = 3)

# Save plots
ggsave("documents/04_Fyke99_WaterBin_PartialDependence.png", width = 10, height = 10, units = "in")




##### Fyke 99 - Water Temp * Freq #####

# Remove unwanted columns
fyke99_waterfreq <- select(fyke99, -c(air.temp_c, wfl_binary))

# Impute NAs using random forest proximity
set.seed(123)
fyke99_waterfreq <- rfImpute(wfl_freq ~ ., data = fyke99_waterfreq)

# Add random numbers
set.seed(123)
fyke99_waterfreq$random <- runif(nrow(fyke99_waterfreq), min = 1, max = 100)

# Bootstrap sample for model training
set.seed(123)
fyke99_waterfreq_train <- slice_sample(fyke99_waterfreq, prop = 1, replace = TRUE)

# Out of bag sample for testing
set.seed(123)
fyke99_waterfreq_test <- setdiff(fyke99_waterfreq, fyke99_waterfreq_train)

# Data for RFM
x <- select(fyke99_waterfreq_train, -wfl_freq)
y <- fyke99_waterfreq_train$wfl_freq
xtest <- select(fyke99_waterfreq_test, -wfl_freq)
ytest <- fyke99_waterfreq_test$wfl_freq

# Tune RFM for optimal mtry
set.seed(123)
tuneRF(x = x, y = y)

# mtry 8 is optimum based on OOB error

# Random forest model
set.seed(123)
rf_f99_waterfreq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 8,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterfreq)

# Summary
rf_f99_waterfreq

# 23.63 MSE of residuals and 72.12% of variance explained on training data
# 36.97 MSE and 53.26% of variance explained on testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f99_waterfreq))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$`%IncMSE`)
colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Increase in Node Purity`), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Increase in Node Purity") +
  theme_pubr() +
  theme(legend.position = "bottom")

# Select only variables that outperformed random numbers
x <- select(x, -c(set.occurrence_yr, air.temp.range_c, pond, random))
xtest <- select(xtest, -c(set.occurrence_yr, air.temp.range_c, pond, random))

# Trimmed RFM
set.seed(123)
rf_f99_waterfreq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 8,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterfreq)

# Summary
rf_f99_waterfreq

# MSE of residuals is 22.29 and 73.71% of variance is explained for training data
# MSE is 40.86 and 48.34% of variance is explained for testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f99_waterfreq))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$`%IncMSE`)
colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
a <- ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Increase in Node Purity`), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("% Increase in MSE") +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Haul winter
pd_winter <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "haul.winter",
  plot = FALSE, n.pt = 200))
b <- ggplot(pd_winter, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Haul date
pd_date <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "haul.date_jul",
  plot = FALSE, n.pt = 200))
c <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Station
pd_station <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "station",
  plot = FALSE))
pd_station$x <- fct_reorder(pd_station$x, pd_station$y, .desc = TRUE)
d <- ggplot(pd_station, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  xlab("Station") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Water temp
pd_water <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "water.temp_c",
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_water, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp (C)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Salinity
pd_salinity <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "salinity_ppt",
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Salinity (ppt)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Soak days
pd_soak <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "soak_days",
  plot = FALSE, n.pt = 200))
g <- ggplot(pd_soak, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Soak Days") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Wind
pd_wind <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "wind_m.s",
  plot = FALSE, n.pt = 200))
h <- ggplot(pd_wind, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Wind Speed (m/s)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Precipitation
pd_precip <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "precip_mm.day",
  plot = FALSE, n.pt = 200))
i <- ggplot(pd_precip, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Precipitation (mm/day)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Lunar illumination
pd_lunar <- bind_rows(partialPlot(rf_f99_waterfreq,
  pred.data = x, x.var = "lunar.illumination",
  plot = FALSE, n.pt = 200))
j <- ggplot(pd_lunar, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Lunar Illumination") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Combine plots
ggarrange(a, b, c, d, e, f, g, h, i, j, ncol = 2, nrow = 5)

# Save plots
ggsave("documents/04_Fyke99_WaterFreq_PartialDependence.png", width = 10, height = 10, units = "in")




##### Fyke 99 - Air Temp * Bin #####

##### Fyke 99 - Air Temp * Freq #####

##### Fyke 19 - Water Temp * Bin #####

# Remove unwanted columns
fyke19_waterbin <- select(fyke19, -c(air.temp_c, wfl_freq))

# Impute NAs using random forest proximity
set.seed(123)
fyke19_waterbin <- rfImpute(wfl_binary ~ ., data = fyke19_waterbin)

# Add random numbers
set.seed(123)
fyke19_waterbin$random <- runif(nrow(fyke19_waterbin), min = 1, max = 100)

# Bootstrap sample for model training
set.seed(123)
fyke19_waterbin_train <- slice_sample(fyke19_waterbin, prop = 1, replace = TRUE)

# Out of bag sample for testing
set.seed(123)
fyke19_waterbin_test <- setdiff(fyke19_waterbin, fyke19_waterbin_train)

# Data for RFM
x <- select(fyke19_waterbin_train, -wfl_binary)
y <- fyke19_waterbin_train$wfl_binary
xtest <- select(fyke19_waterbin_test, -wfl_binary)
ytest <- fyke19_waterbin_test$wfl_binary

# Tune RFM for optimal mtry
set.seed(123)
tuneRF(x = x, y = y)

# mtry 2 is optimum based on OOB error

# Random forest model
set.seed(123)
rf_f19_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f19_waterbin)

# Summary
rf_f19_waterbin

# Error rate on training data is 10.71% and on testing data is 21.16%

# Variable importance
ImpData <- as.data.frame(importance(rf_f19_waterbin))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$MeanDecreaseAccuracy)
#colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
ggplot(ImpData, aes(x = Var.Names, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Mean Decrease in Accuracy") +
  theme_pubr() +
  theme(legend.position = "bottom")

# Select only variables that outperformed random numbers
x <- select(x, station, haul.date_jul, mean.water.temp_c, delta.do_mg.l.day, delta.salinity_ppt.day, salinity_ppt, precip_mm.day, range.depth_m, do_mg.l, skewness.water.temp)
xtest <- select(xtest, station, haul.date_jul, mean.water.temp_c, delta.do_mg.l.day, delta.salinity_ppt.day, salinity_ppt, precip_mm.day, range.depth_m, do_mg.l, skewness.water.temp)

# Trimmed RFM
set.seed(123)
rf_f19_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f19_waterbin)

# Summary
rf_f19_waterbin

# Error rate of 10.48% on training data and 25.31% on testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f19_waterbin))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$MeanDecreaseAccuracy)
#colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
a <- ggplot(ImpData, aes(x = Var.Names, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Mean Decrease in Accuracy") +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Station
pd_station <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "station", which.class = 1,
  plot = FALSE))
pd_station$x <- fct_reorder(pd_station$x, pd_station$y, .desc = TRUE)
b <- ggplot(pd_station, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Haul date
pd_date <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "haul.date_jul", which.class = 1,
  plot = FALSE, n.pt = 200))
c <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Salinity
pd_salinity <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "salinity_ppt", which.class = 1,
  plot = FALSE, n.pt = 200))
d <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Salinity (ppt)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Precip
pd_precip <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "precip_mm.day", which.class = 1,
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_precip, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Precipitation (mm/day)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Mean water temp
pd_water <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "mean.water.temp_c", which.class = 1,
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_water, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Mean Water Temp (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Depth range
pd_depth <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "range.depth_m", which.class = 1,
  plot = FALSE, n.pt = 200))
g <- ggplot(pd_depth, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Depth Range (m)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Delta DO
pd_do <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "delta.do_mg.l.day", which.class = 1,
  plot = FALSE, n.pt = 200))
h <- ggplot(pd_do, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Delta DO (mg/L/day)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Delta Salinity
pd_salinity <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "delta.salinity_ppt.day", which.class = 1,
  plot = FALSE, n.pt = 200))
i <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Delta Salinity (ppt/day)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# DO
pd_do <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "do_mg.l", which.class = 1,
  plot = FALSE, n.pt = 200))
j <- ggplot(pd_do, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("DO (mg/L)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Skewness water temp
pd_skew <- bind_rows(partialPlot(rf_f19_waterbin,
  pred.data = x, x.var = "skewness.water.temp", which.class = 1,
  plot = FALSE, n.pt = 200))
k <- ggplot(pd_skew, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp Skewness") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Combine plots
ggarrange(a, b, c, d, e, f, g, h, i, j, k, ncol = 2, nrow = 6)

# Save plots
ggsave("documents/04_Fyke19_WaterBin_PartialDependence.png", width = 10, height = 12, units = "in")




##### Fyke 19 - Water Temp * Freq #####

# Remove unwanted columns
fyke19_waterfreq <- select(fyke19, -c(air.temp_c, wfl_binary))

# Impute NAs using random forest proximity
set.seed(123)
fyke19_waterfreq <- rfImpute(wfl_freq ~ ., data = fyke19_waterfreq)

# Add random numbers
set.seed(123)
fyke19_waterfreq$random <- runif(nrow(fyke19_waterfreq), min = 1, max = 100)

# Bootstrap sample for model training
set.seed(123)
fyke19_waterfreq_train <- slice_sample(fyke19_waterfreq, prop = 1, replace = TRUE)

# Out of bag sample for testing
set.seed(123)
fyke19_waterfreq_test <- setdiff(fyke19_waterfreq, fyke19_waterfreq_train)

# Data for RFM
x <- select(fyke19_waterfreq_train, -wfl_freq)
y <- fyke19_waterfreq_train$wfl_freq
xtest <- select(fyke19_waterfreq_test, -wfl_freq)
ytest <- fyke19_waterfreq_test$wfl_freq

# Tune RFM for optimal mtry
set.seed(123)
tuneRF(x = x, y = y)

# mtry 2 is optimum based on OOB error

# Random forest model
set.seed(123)
rf_f19_waterfreq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f19_waterfreq)

# Summary
rf_f19_waterfreq

# 6.93 MSE of residuals and 59.91% of variance explained on training data
# 12.84 MSE and 3.03% of variance explained on testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f19_waterfreq))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$`%IncMSE`)
colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Increase in Node Purity`), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Increase in Node Purity") +
  theme_pubr() +
  theme(legend.position = "bottom")

# Select only variables that outperformed random numbers
x <- select(x, haul.date_jul, skewness.water.temp, precip_mm.day, lunar.illumination, bimodality.water.temp, air.temp.range_c)
xtest <- select(xtest, haul.date_jul, skewness.water.temp, precip_mm.day, lunar.illumination, bimodality.water.temp, air.temp.range_c)

# Trimmed RFM
set.seed(123)
rf_f19_waterfreq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 2,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f19_waterfreq)

# Summary
rf_f19_waterfreq

# MSE of residuals is 7.52 and 56.53% of variance is explained for training data
# MSE is 18.37 and -38.74% of variance is explained for testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f19_waterfreq))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$`%IncMSE`)
colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
a <- ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Increase in Node Purity`), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("% Increase in MSE") +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Haul date
pd_date <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "haul.date_jul",
  plot = FALSE, n.pt = 200))
b <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Precip
pd_precip <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "precip_mm.day",
  plot = FALSE, n.pt = 200))
c <- ggplot(pd_precip, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Precipitation (mm/day)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Skewness water temp
pd_skew <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "skewness.water.temp",
  plot = FALSE, n.pt = 200))
d <- ggplot(pd_skew, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp Skewness") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Bimodality water temp
pd_bimodal <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "bimodality.water.temp",
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_bimodal, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp Bimodality") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Lunar illumination
pd_lunar <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "lunar.illumination",
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_lunar, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Lunar Illumination") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Air temp range
pd_air <- bind_rows(partialPlot(rf_f19_waterfreq,
  pred.data = x, x.var = "air.temp.range_c",
  plot = FALSE, n.pt = 200))
g <- ggplot(pd_air, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Air Temp Range (C)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()


# Combine plots
ggarrange(a, b, c, d, e, f, g, ncol = 2, nrow = 4)

# Save plots
ggsave("documents/04_Fyke19_WaterFreq_PartialDependence.png", width = 10, height = 10, units = "in")




##### Fyke 19 - Air Temp * Bin #####

##### Fyke 19 - Air Temp * Freq #####