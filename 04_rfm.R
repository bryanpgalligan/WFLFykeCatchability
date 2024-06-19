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




##### 1999 data binary response #####

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


# Subset data for random forest - only winter, date, and station are necessary for parsimonious prediction
fyke99_binary <- select(fyke99_binary, c("wfl_binary",
  "haul.winter", "haul.date_jul", "station", "set.occurrence_yr", "pond"))

# Bootstrap sample for model training
set.seed(45612)
fyke99_binary_train <- slice_sample(fyke99_binary, prop = 1, replace = TRUE)

# Out of bag sample for testing
fyke99_binary_test <- setdiff(fyke99_binary, fyke99_binary_train)

# Data for RFM
x <- select(fyke99_binary_train, -wfl_binary)
y <- fyke99_binary_train$wfl_binary
xtest <- select(fyke99_binary_test, -wfl_binary)
ytest <- fyke99_binary_test$wfl_binary

# Tune RFM for optimal mtry
set.seed(45612)
tuneRF(x = x, y = y)

# mtry 5 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f99_binary <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 5,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f99_binary)

# Summary
rf_f99_binary

# Error rate of 7.87% on training data and 18.92% on testing data


## Variable importance

# Variable importance
vimportance <- as.data.frame(importance(rf_f99_binary))
vimportance$var <- row.names(vimportance)

# Clean variable names
vimportance$var <- c("Haul Winter", "Haul Date", "Station", "Set Occurrence", "Pond")

# Reorder in terms of importance
vimportance$var <- fct_reorder(vimportance$var, vimportance$MeanDecreaseAccuracy)

# Rename gini column
colnames(vimportance)[colnames(vimportance) == "MeanDecreaseGini"] <- "Gini"

# Plot variable importance
vars_1999_binary <- ggplot(vimportance, aes(x = var, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = var, xend = var, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_vline(xintercept = 2.5, color = "red", linetype = "dashed") +
  geom_point(aes(size = Gini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Accuracy") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots  


# Haul winter

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "haul.winter", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme_pubr()


# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "station", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Reorder from greatest to least
pd$x <- fct_reorder(pd$x, pd$y, .desc = TRUE)

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity") +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Haul date

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "haul.date_jul", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Convert x to date class as Julian date with 1 = November 1
pd$x <- as.Date(pd$x, origin = "1999-11-01")

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme_pubr()


# Set occurrence

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "set.occurrence_yr", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Set Occurrence") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5, 1), xlim = c(1, 20)) +
  theme_pubr()


# Pond

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "pond", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Rename ponds
pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))

# Reorder from greatest to least
pd$x <- fct_reorder(pd$x, pd$y, .desc = TRUE)

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity") +
  xlab("Pond") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5, 1)) +
  theme_pubr()




##### 1999 data freq response #####

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
fyke99_freq <- select(fyke99_freq, c("wfl_freq", "haul.winter", "haul.date_jul", "station", "water.temp_c", "pond", "soak_days"))

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

# mtry 4 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f99_freq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 4,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f99_freq)

# Summary
rf_f99_freq

# 81.1% of variance explained on training data and 45.5% on testing data

## Variable importance

# Variable importance
vimportance <- as.data.frame(importance(rf_f99_freq))
vimportance$var <- row.names(vimportance)

# Clean variable names
vimportance$var <- c("Haul Winter", "Haul Date", "Station", "Water Temp", "Pond", "Soak Days")

# Reorder in terms of importance
vimportance$var <- fct_reorder(vimportance$var, vimportance$`%IncMSE`)

# Rename Node Purity column
colnames(vimportance)[colnames(vimportance) == "IncNodePurity"] <- "Node Purity"

# Plot variable importance
vars_1999_freq <- ggplot(vimportance, aes(x = var, y = `%IncMSE`)) +
  geom_segment(aes(x = var, xend = var, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Node Purity`), color = "blue", alpha=0.6) +
  geom_vline(xintercept = 3.5, color = "red", linetype = "dashed") +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("% Increase in MSE") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Haul winter

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "haul.winter",
  plot = FALSE))

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()


# Haul date

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "haul.date_jul",
  plot = FALSE))

# Convert x to date class as Julian date with 1 = November 1
pd$x <- as.Date(pd$x, origin = "1999-11-01")

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()


# Water temp

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "water.temp_c",
  plot = FALSE))

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp (C)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()


# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "station",
  plot = FALSE))

# Reorder from greatest to least
pd$x <- fct_reorder(pd$x, pd$y, .desc = TRUE)

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity") +
  xlab("Station") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Soak days

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "soak_days",
  plot = FALSE))

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Soak Days") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()


# Pond

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "pond",
  plot = FALSE))

# Rename ponds
pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))

# Reorder from greatest to least
pd$x <- fct_reorder(pd$x, pd$y, .desc = TRUE)

# Plot partial dependence
ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity") +
  xlab("Pond") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()




##### 2019 data binary response #####





