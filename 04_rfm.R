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
  ylab("Mean Decrease in Accuracy") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom")

## Make a poster version of the plot

# Rename variables
vimportance$var <- c("Year", "Day", "Station", "Set No.", "Pond")
vimportance$var <- fct_reorder(vimportance$var, vimportance$MeanDecreaseAccuracy)

# Load extra fonts
loadfonts()

# Make plot
ggplot(vimportance, aes(x = var, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = var, xend = var, y = 0, yend = MeanDecreaseAccuracy),
    color = "skyblue", linewidth = 1.5) +
  geom_point(aes(size = Gini), color = "blue", alpha=0.6, size = 10) +
  theme_light() +
  coord_flip() +
  labs(x = "", y = "Variable Importance\n(% Decrease in Accuracy)", title = "") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom",
    axis.text = element_text(family = "Arial", size = 40),
    axis.title = element_text(family = "Arial", size = 40),
    legend.title = element_text(family = "Arial", size = 40),
    legend.text = element_text(family = "Arial", size = 40),
    axis.line = element_line(color = "black", linewidth = 1.5),
    axis.ticks.x = element_line(color = "black", linewidth = 1.5),
    axis.ticks.length = unit(0.2, "in"),
    axis.ticks.y = element_blank()
    )

# Save plot
ggsave("figures/04_VariableImportance_AFS.png", width = 10.28, height = 7.46, units = "in")

## Partial dependence plots  


# Haul winter

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "haul.winter", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Plot partial dependence
year_1999_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Catch Probability") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr()


# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "station", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Make x factor
pd$x <- as.factor(pd$x)

# Save factor order
station_order <- levels(pd$x)

# Add column for colors based on x including NP, PP, or PJ
pd$color <- c(
  "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
  "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
  "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
)

# Plot partial dependence
station_1999_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
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
date_1999_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("All Years Classification") +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Make an AFS version of this plot
ggplot(pd, aes(x = x, y = y)) +
  geom_line(linewidth = 1.5) +
  geom_smooth(color = "blue", linewidth = 2) +
  xlab("") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr() +
  theme(
    axis.text = element_text(family = "Arial", size = 40),
    axis.title = element_text(family = "Arial", size = 40),
    axis.line = element_line(color = "black", linewidth = 1.5),
    axis.ticks = element_line(color = "black", linewidth = 1.5),
    axis.ticks.length = unit(0.2, "in"),
    plot.margin = unit(c(0.5, 0, 0, 0.1), "in")
    )

# Save plot
ggsave("figures/04_HaulDate_AFS.png", width = 10.28, height = 7.46, units = "in")

# Set occurrence

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_binary,
  pred.data = x, x.var = "set.occurrence_yr", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Plot partial dependence
set_1999_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Set Occurrence") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 20)) +
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

# Add column for color
pd$color <- c("#D41159", "#0C7BDC", "#FFC20A")

# Plot partial dependence
pond_1999_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Pond") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
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

# Save data for later
pd_year <- pd

# Plot partial dependence
year_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()


##### WIP #####

# Haul winter interacting with pond

# Make haul winter an integer
x$haul.winter <- as.integer(x$haul.winter)

# Subset x for each pond
x_np <- x %>% filter(pond == "NP")
x_pp <- x %>% filter(pond == "PP")
x_pj <- x %>% filter(pond == "PJ")

# Calculate partial dependence for each pond
pd_year_np <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x_np, x.var = "haul.winter", n.pt = 13,
  plot = FALSE))
pd_year_pp <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x_pp, x.var = "haul.winter", n.pt = 26,
  plot = FALSE))
pd_year_pj <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x_pj, x.var = "haul.winter", n.pt = 25,
  plot = FALSE))


# Haul date

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "haul.date_jul",
  plot = FALSE))

# Convert x to date class as Julian date with 1 = November 1
pd$x <- as.Date(pd$x, origin = "1999-11-01")

# Plot partial dependence
date_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
  ggtitle("All Years Regression") +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Water temp

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "water.temp_c",
  plot = FALSE))

# Plot partial dependence
water_temp <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Water Temp (°C)") +
  ylab("Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_pubr()


# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "station",
  plot = FALSE))

# Make factor following order of station_1999_binary
pd$x <- factor(pd$x, levels = station_order)

# Add column for colors based on x including NP, PP, or PJ
pd$color <- c(
  "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
  "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
  "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
)

# Plot partial dependence
station_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Station") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
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
  coord_cartesian(ylim = c(0, 12)) +
  theme_pubr()


# Pond

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = x, x.var = "pond",
  plot = FALSE))

# Rename ponds
pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))

# Add column for color
pd$color <- c("#D41159", "#0C7BDC", "#FFC20A")

# Plot partial dependence
pond_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Pond") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_pubr()




##### 2019 data binary response #####

# Remove freq response column
fyke19_binary <- select(fyke19, -wfl_freq)

# Impute NAs using random forest proximity
set.seed(45612)
fyke19_binary <- rfImpute(wfl_binary ~ ., data = fyke19_binary)

# Select variables using VSURF
set.seed(45612)
vfyke19_binary <- VSURF(wfl_binary ~ ., data = fyke19_binary)

summary(vfyke19_binary)

plot(vfyke19_binary)

# Subset data for random forest
fyke19_binary <- select(fyke19_binary, c("wfl_binary",
  "haul.date_jul", "station", "pond", "set.occurrence_yr"))

# Bootstrap sample for model training
set.seed(45612)
fyke19_binary_train <- slice_sample(fyke19_binary, prop = 1, replace = TRUE)

# Out of bag sample for testing
fyke19_binary_test <- setdiff(fyke19_binary, fyke19_binary_train)

# Data for RFM
x <- select(fyke19_binary_train, -wfl_binary)
y <- fyke19_binary_train$wfl_binary
xtest <- select(fyke19_binary_test, -wfl_binary)
ytest <- fyke19_binary_test$wfl_binary

# Tune RFM for optimal mtry
set.seed(45612)
tuneRF(x = x, y = y)

# mtry 4 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f19_binary <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 4,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f19_binary)

# Summary
rf_f19_binary

# Error rate of 8.66% on training data and 28.1% on testing data


## Variable importance

# Variable importance
vimportance <- as.data.frame(importance(rf_f19_binary))

# Clean variable names
vimportance$var <- c("Haul Date", "Station", "Pond", "Set Occurrence")

# Reorder in terms of importance
vimportance$var <- fct_reorder(vimportance$var, vimportance$MeanDecreaseAccuracy)

# Rename gini column
colnames(vimportance)[colnames(vimportance) == "MeanDecreaseGini"] <- "Gini"

# Plot variable importance
vars_2019_binary <- ggplot(vimportance, aes(x = var, y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = var, xend = var, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
  geom_point(aes(size = Gini), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Mean Decrease in Accuracy") +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Haul date

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_binary,
  pred.data = x, x.var = "haul.date_jul", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Convert x to date class as Julian date with 1 = November 1
pd$x <- as.Date(pd$x, origin = "2019-11-01")

# Plot partial dependence
date_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Five-Year Classification") +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_binary,
  pred.data = x, x.var = "station", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Reorder following order of station_1999_binary
pd$x <- factor(pd$x, levels = station_order)

# Add column for colors based on x including NP, PP, or PJ
pd$color <- c(
  "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
  "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
  "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
)

# Plot partial dependence
station_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Station") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Set occurrence

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_binary,
  pred.data = x, x.var = "set.occurrence_yr", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Plot partial dependence
set_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Set Occurrence") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr()


# Pond

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_binary,
  pred.data = x, x.var = "pond", which.class = "1",
  plot = FALSE))

# Convert logits to probabilities
pd$y <- exp(pd$y) / (1 + exp(pd$y))

# Rename ponds
pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))

# Add column for color
pd$color <- c("#D41159", "#0C7BDC", "#FFC20A")

# Plot partial dependence
pond_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Pond") +
  ylab("Catch Probability") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr()




##### 2019 data freq response #####

# Remove binary response column
fyke19_freq <- select(fyke19, -wfl_binary)

# Impute NAs using random forest proximity
set.seed(45612)
fyke19_freq <- rfImpute(wfl_freq ~ ., data = fyke19_freq)

# Select variables using VSURF
set.seed(45612)
vfyke19_freq <- VSURF(wfl_freq ~ ., data = fyke19_freq)

summary(vfyke19_freq)

plot(vfyke19_freq)

# Subset data for random forest
fyke19_freq <- select(fyke19_freq, c("wfl_freq",
  "haul.date_jul", "station", "set.occurrence_yr", "haul.winter"))

# Bootstrap sample for model training
set.seed(45612)
fyke19_freq_train <- slice_sample(fyke19_freq, prop = 1, replace = TRUE)

# Out of bag sample for testing
fyke19_freq_test <- setdiff(fyke19_freq, fyke19_freq_train)

# Data for RFM
x <- select(fyke19_freq_train, -wfl_freq)
y <- fyke19_freq_train$wfl_freq
xtest <- select(fyke19_freq_test, -wfl_freq)
ytest <- fyke19_freq_test$wfl_freq

# Tune RFM for optimal mtry
set.seed(45612)
tuneRF(x = x, y = y)

# mtry 4 is optimum based on OOB error

# Random forest model
set.seed(45612)
rf_f19_freq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
  ntree = 200, mtry = 4,
  importance = TRUE, proximity = TRUE, keep.forest = TRUE)

# Plot model
plot(rf_f19_freq)

# Summary
rf_f19_freq

# 62.2% of variance explained on training data and 31.0% on testing data


## Variable importance

# Variable importance
vimportance <- as.data.frame(importance(rf_f19_freq))

# Clean variable names
vimportance$var <- c("Haul Date", "Station", "Set Occurrence", "Haul Winter")

# Reorder in terms of importance
vimportance$var <- fct_reorder(vimportance$var, vimportance$`%IncMSE`)

# Rename Node Purity column
colnames(vimportance)[colnames(vimportance) == "IncNodePurity"] <- "Node Purity"

# Plot variable importance
vars_2019_freq <- ggplot(vimportance, aes(x = var, y = `%IncMSE`)) +
  geom_segment(aes(x = var, xend = var, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = `Node Purity`), color = "blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("% Increase in MSE") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 2)) +
  theme_pubr() +
  theme(legend.position = "bottom")


## Partial dependence plots

# Station

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_freq,
  pred.data = x, x.var = "station",
  plot = FALSE))

# Reorder following order of station_1999_binary
pd$x <- factor(pd$x, levels = station_order)

# Add column for colors based on x including NP, PP, or PJ
pd$color <- c(
  "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
  "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
  "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
)

# Plot partial dependence
station_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_bar(stat = "Identity", fill = pd$color) +
  xlab("Station") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Haul date

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_freq,
  pred.data = x, x.var = "haul.date_jul",
  plot = FALSE))

# Convert x to date class as Julian date with 1 = November 1
pd$x <- as.Date(pd$x, origin = "2019-11-01")

# Plot partial dependence
date_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Date") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
  ggtitle("Five-Year Regression") +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Set occurrence

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_freq,
  pred.data = x, x.var = "set.occurrence_yr",
  plot = FALSE))

# Plot partial dependence
set_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Set Occurrence") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_pubr()


# Haul winter

# Calculate partial dependence
pd <- bind_rows(partialPlot(rf_f19_freq,
  pred.data = x, x.var = "haul.winter",
  plot = FALSE))

# Plot partial dependence
year_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(color = "blue") +
  xlab("Haul Winter") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_pubr()




##### Fig - Variable importance #####

# Combine variable importance plots
ggarrange(vars_1999_binary, vars_1999_freq, vars_2019_binary, vars_2019_freq,
  ncol = 4, nrow = 1,
  labels = "AUTO", label.x = c(0.05, 0, 0.05, 0.05))

# Save plot
ggsave("figures/04_variable_importance.png", width = 15, height = 3, units = "in")




##### Fig - Partial plots #####

# Blank ggplot
p <- ggplot() + theme_void()

# Combine partial plots
ggarrange(
  date_1999_binary, date_1999_freq, date_2019_binary, date_2019_freq,
  station_1999_binary, station_1999_freq, station_2019_binary, station_2019_freq,
  set_1999_binary, p, set_2019_binary, set_2019_freq,
  pond_1999_binary, pond_1999_freq, pond_2019_binary, p,
  ncol = 4, nrow = 4
)

# Save plot
ggsave("figures/04_annual_variables.png", width = 15, height = 15, units = "in", bg = "white")




##### Fig - Year #####

# Add marginal distributions

# 1999 binary
year_1999_binary <- year_1999_binary +
  geom_point(data = fyke99_binary, aes(x = haul.winter, y = as.numeric(wfl_binary)), alpha = 0)
year_1999_binary <- ggMarginal(year_1999_binary, type = "density", margins = "x", size = 5, fill = "skyblue")

# 1999 freq
year_1999_freq <- year_1999_freq +
  geom_point(data = fyke99_freq, aes(x = haul.winter, y = wfl_freq), alpha = 0)
year_1999_freq <- ggMarginal(year_1999_freq, type = "density", size = 5, fill = "skyblue")

# 2019 freq
year_2019_freq <- year_2019_freq +
  geom_point(data = fyke19_freq, aes(x = haul.winter, y = wfl_freq), alpha = 0)
year_2019_freq <- ggMarginal(year_2019_freq, type = "density", size = 5, fill = "skyblue")

# Combine plots
ggarrange(year_1999_binary, year_1999_freq, year_2019_freq,
  ncol = 3, nrow = 1,
  widths = c(0.9, 1, 1),
  labels = "AUTO")

# Save plot
ggsave("figures/04_haul_winter.png", width = 10, height = 3, units = "in", bg = "white")




##### Fig - Water temp #####

water_temp <- water_temp +
  geom_point(data = fyke99_freq, aes(x = water.temp_c, y = wfl_freq), alpha = 0)

water_temp <- ggMarginal(water_temp, type = "density", size = 5, fill = "skyblue")

# Save plot
save_plot("figures/04_water_temp.png", plot = water_temp, base_width = 3.5, base_height = 3)




##### Fig - Environmental Variables #####

## 1999 variables

# Water temp
water_temp_1999 <- ggplot(fyke99, aes(x = 1, y = water.temp_c)) +
  geom_violin(fill = "#0C7BDC", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Water Temp (°C)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Salinity
salinity_1999 <- ggplot(fyke99, aes(x = 1, y = salinity_ppt)) +
  geom_violin(fill = "#FFC107", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Salinity (PSU)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Wind
wind_1999 <- ggplot(fyke99, aes(x = 1, y = wind_m.s)) +
  geom_violin(fill = "#D41159", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Wind Speed (m/s)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


## 2019 variables

# Water temp range
water_range_2019 <- ggplot(fyke19, aes(x = 1, y = range.water.temp_c)) +
  geom_violin(fill = "#0C7BDC", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Water Temp Range (°C)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Water temp skewness
water_skewness_2019 <- ggplot(fyke19, aes(x = 1, y = skewness.water.temp)) +
  geom_violin(fill = "#0C7BDC", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Water Temp Skewness") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Water temp kurtosis
water_kurtosis_2019 <- ggplot(fyke19, aes(x = 1, y = kurtosis.water.temp)) +
  geom_violin(fill = "#0C7BDC", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Water Temp Kurtosis") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Salinity rate of change
salinity_change_2019 <- ggplot(fyke19, aes(x = 1, y = delta.salinity_ppt.day)) +
  geom_violin(fill = "#FFC107", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Salinity Rate of Change (PSU/day)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Dissolved oxygen
do_2019 <- ggplot(fyke19, aes(x = 1, y = do_mg.l)) +
  geom_violin(fill = "#004D40", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Dissolved oxygen rate of change
do_change_2019 <- ggplot(fyke19, aes(x = 1, y = delta.do_mg.l.day)) +
  geom_violin(fill = "#004D40", alpha = 0.5,
    draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
  geom_violin(fill = "transparent", draw_quantiles = 0.5) +
  xlab("") +
  ylab("DO Rate of Change (mg/L/day)") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## Combine plots

# Blank plot
p <- ggplot() + theme_void()

# Combine plots
ggarrange(water_temp_1999, salinity_1999, wind_1999, p, p, p,
  water_range_2019, water_skewness_2019, water_kurtosis_2019,
  salinity_change_2019, do_2019, do_change_2019,
  labels = c("A", "", "", "", "", "",
    "B", "", "", "", "", ""),
  ncol = 6, nrow = 2)

# Save plot
ggsave("figures/04_environmental_variables.png", width = 15, height = 6, units = "in", bg = "white")




##### Fig - Abundance Index #####

# Make a non-corrected annual abundance index based on mean freq for Ninegret pond
abundance_np <- fyke99 %>%
  filter(pond == "NP") %>%
  group_by(haul.winter) %>%
  summarise(np_mean_freq = mean(wfl_freq, na.rm = TRUE))

# Rename haul winter to year
colnames(abundance_np)[colnames(abundance_np) == "haul.winter"] <- "year"

# Non-corrected annual abundance for Potter Pond
abundance_pp <- fyke99 %>%
  filter(pond == "PP") %>%
  group_by(haul.winter) %>%
  summarise(pp_mean_freq = mean(wfl_freq, na.rm = TRUE))

# Rename haul winter to year
colnames(abundance_pp)[colnames(abundance_pp) == "haul.winter"] <- "year"

# Non-corrected annual abundance for PJ Pond
abundance_pj <- fyke99 %>%
  filter(pond == "PJ") %>%
  group_by(haul.winter) %>%
  summarise(pj_mean_freq = mean(wfl_freq, na.rm = TRUE))

# Rename haul winter to year
colnames(abundance_pj)[colnames(abundance_pj) == "haul.winter"] <- "year"

# Rename partial dependence columns
colnames(pd_year_np) <- c("year", "np_pred_freq")
colnames(pd_year_pp) <- c("year", "pp_pred_freq")
colnames(pd_year_pj) <- c("year", "pj_pred_freq")

# Merge data
abundance <- left_join(abundance_pp, pd_year_pp, by = "year")
abundance <- left_join(abundance, abundance_pj, by = "year")
abundance <- left_join(abundance, pd_year_pj, by = "year")
abundance <- left_join(abundance, abundance_np, by = "year")
abundance <- left_join(abundance, pd_year_np, by = "year")

##### WIP - next is to plot the merged data #####



# Rename partial dependence predictions as year and pred_freq
colnames(pd_year)[colnames(pd_year) == "x"] <- "year"
colnames(pd_year)[colnames(pd_year) == "y"] <- "pred_freq"

# Merge data
abundance <- left_join(abundance, pd_year, by = "year")




# Plot data
ggplot(abundance, aes(x = year)) +
  geom_line(aes(y = mean_freq, linetype = "Mean"), color = "black") +
  geom_line(aes(y = pred_freq, color = "Predicted"), size = 1) +
  xlab("Year") +
  ylab("Abundance Index (no. per haul)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = c(0.85, 0.85),
        legend.spacing.y = unit(0.1, 'cm'),  # Reduce space between legend entries
        legend.key.height = unit(0.4, 'cm'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  scale_linetype_manual(values = c("Mean" = "dashed"), guide = guide_legend(title = NULL)) +
  scale_color_manual(values = c("Predicted" = "blue"), guide = guide_legend(title = NULL))

# Save plot
ggsave("figures/04_abundance_index.png", width = 6, height = 4, units = "in", bg = "white")

