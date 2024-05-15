## This script uses the RFM algorithm to select
## predictor variables

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

# Random forest model
set.seed(123)
rf_f99_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 5,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterbin)

# Summary
rf_f99_waterbin

## mtry 5 is optimum

# mtry 1 error rate 8.72% on training data and 23.92% on testing data
# mtry 2 error rate 8.82% on training data and 22.31% on testing data
# mtry 3 error rate 8.63% on training data and 22.58% on testing data
# mtry 4 error rate 8.72% on training data and 21.51% on testing data
# mtry 5 error rate 8.44% on training data and 20.43% on testing data
# mtry 6 error rate 8.15% on training data and 21.51% on testing data
# mtry 7 error rate 7.87% on training data and 21.24% on testing data
# mtry 8 error rate 7.77% on training data and 20.43% on testing data
# mtry 9 error rate 7.68% on training data and 20.97% on testing data
# mtry 10 error rate 6.92% on training data and 21.24% on testing data

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
x <- select(x, haul.winter, station, haul.date_jul, water.temp_c, salinity_ppt, air.temp.range_c)
xtest <- select(xtest, haul.winter, station, haul.date_jul, water.temp_c, salinity_ppt, air.temp.range_c)

# Trimmed RFM
set.seed(123)
rf_f99_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 5,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterbin)

# Summary
rf_f99_waterbin

## mtry 5 now has an error rate of 7.58% on training data and 20.7% on testing data

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

# Station
pd_station <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "station", which.class = 1,
  plot = FALSE))
pd_station$x <- fct_reorder(pd_station$x, pd_station$y, .desc = TRUE)
c <- ggplot(pd_station, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Haul date
pd_date <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "haul.date_jul", which.class = 1,
  plot = FALSE, n.pt = 200))
d <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Salinity
pd_salinity <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "salinity_ppt", which.class = 1,
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Salinity (ppt)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Water temp
pd_water <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "water.temp_c", which.class = 1,
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_water, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Air temp range
pd_air <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "air.temp.range_c", which.class = 1,
  plot = FALSE, n.pt = 200))
g <- ggplot(pd_air, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Air Temp Range (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Combine plots
ggarrange(a, b, c, d, e, f, g, ncol = 2, nrow = 4)

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

# Random forest model
set.seed(123)
rf_f99_waterfreq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 5,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterfreq)

# Summary
rf_f99_waterfreq

## mtry 5 is optimum

# mtry 1 error rate 8.72% on training data and 23.92% on testing data
# mtry 2 error rate 8.82% on training data and 22.31% on testing data
# mtry 3 error rate 8.63% on training data and 22.58% on testing data
# mtry 4 error rate 8.72% on training data and 21.51% on testing data
# mtry 5 error rate 8.44% on training data and 20.43% on testing data
# mtry 6 error rate 8.15% on training data and 21.51% on testing data
# mtry 7 error rate 7.87% on training data and 21.24% on testing data
# mtry 8 error rate 7.77% on training data and 20.43% on testing data
# mtry 9 error rate 7.68% on training data and 20.97% on testing data
# mtry 10 error rate 6.92% on training data and 21.24% on testing data

# Variable importance
ImpData <- as.data.frame(importance(rf_f99_waterfreq))
ImpData$Var.Names <- row.names(ImpData)
#ImpData$Var.Names <- c("Trophic Level", "K", "Habitat", "CPUE", "Maturity", "Length")
ImpData$Var.Names <- fct_reorder(ImpData$Var.Names, ImpData$`%IncMSE`)
colnames(ImpData)[colnames(ImpData) == "IncNodePurity"] <- "Increase in Node Purity"
ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = "Increase in Node Purity"), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Increase in Node Purity") +
  theme_pubr() +
  theme(legend.position = "bottom")

# Select only variables that outperformed random numbers
x <- select(x, haul.winter, station, haul.date_jul, water.temp_c, salinity_ppt, air.temp.range_c)
xtest <- select(xtest, haul.winter, station, haul.date_jul, water.temp_c, salinity_ppt, air.temp.range_c)

# Trimmed RFM
set.seed(123)
rf_f99_waterbin <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 5,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model - error reaches asymptote well before 200 trees, so that's the ntree we'll use
plot(rf_f99_waterbin)

# Summary
rf_f99_waterbin

## mtry 5 now has an error rate of 7.58% on training data and 20.7% on testing data

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

# Station
pd_station <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "station", which.class = 1,
  plot = FALSE))
pd_station$x <- fct_reorder(pd_station$x, pd_station$y, .desc = TRUE)
c <- ggplot(pd_station, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Haul date
pd_date <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "haul.date_jul", which.class = 1,
  plot = FALSE, n.pt = 200))
d <- ggplot(pd_date, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Salinity
pd_salinity <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "salinity_ppt", which.class = 1,
  plot = FALSE, n.pt = 200))
e <- ggplot(pd_salinity, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Salinity (ppt)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Water temp
pd_water <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "water.temp_c", which.class = 1,
  plot = FALSE, n.pt = 200))
f <- ggplot(pd_water, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Air temp range
pd_air <- bind_rows(partialPlot(rf_f99_waterbin,
  pred.data = x, x.var = "air.temp.range_c", which.class = 1,
  plot = FALSE, n.pt = 200))
g <- ggplot(pd_air, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Air Temp Range (C)") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

# Combine plots
ggarrange(a, b, c, d, e, f, g, ncol = 2, nrow = 4)

# Save plots
ggsave("documents/04_Fyke99_WaterBin_PartialDependence.png", width = 10, height = 10, units = "in")




##### Fyke 99 - Air Temp * Bin #####

##### Fyke 99 - Air Temp * Freq #####

##### Fyke 19 - Water Temp * Bin #####

##### Fyke 19 - Water Temp * Freq #####

##### Fyke 19 - Air Temp * Bin #####

##### Fyke 19 - Air Temp * Freq #####