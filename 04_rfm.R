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

# Function to find SE (standard error) of the mean of a vector
se.mean <- function(x){
  
  # If there are NA in x
  if(sum(is.na(x)) > 0){
    
    # Remove NA
    x <- na.omit(x)
  }
  
  sd(x)/sqrt(length(x))
  
}




##### RFM Classification #####

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
vimportance$var <- c("Year", "Day of Year", "Station", "Set Occurrence", "Pond")

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
  ylab("Mean Decrease in Accuracy (%)") +
  ggtitle("Classification Model") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom",
    plot.title = element_text(hjust = 0.5))

# ## Make a poster version of the plot
# 
# # Rename variables
# vimportance$var <- c("Year", "Day", "Station", "Set No.", "Pond")
# vimportance$var <- fct_reorder(vimportance$var, vimportance$MeanDecreaseAccuracy)
# 
# # Load extra fonts
# loadfonts()
# 
# # Make plot
# ggplot(vimportance, aes(x = var, y = MeanDecreaseAccuracy)) +
#   geom_segment(aes(x = var, xend = var, y = 0, yend = MeanDecreaseAccuracy),
#     color = "skyblue", linewidth = 1.5) +
#   geom_point(aes(size = Gini), color = "blue", alpha=0.6, size = 10) +
#   theme_light() +
#   coord_flip() +
#   labs(x = "", y = "Variable Importance\n(% Decrease in Accuracy)", title = "") +
#   scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
#   theme_pubr() +
#   theme(legend.position = "bottom",
#     axis.text = element_text(family = "Arial", size = 40),
#     axis.title = element_text(family = "Arial", size = 40),
#     legend.title = element_text(family = "Arial", size = 40),
#     legend.text = element_text(family = "Arial", size = 40),
#     axis.line = element_line(color = "black", linewidth = 1.5),
#     axis.ticks.x = element_line(color = "black", linewidth = 1.5),
#     axis.ticks.length = unit(0.2, "in"),
#     axis.ticks.y = element_blank()
#     )
# 
# # Save plot
# ggsave("figures/04_VariableImportance_AFS.png", width = 10.28, height = 7.46, units = "in")




##### ALE Classification #####

# Predictor object to hold model and data
predictor <- Predictor$new(rf_f99_binary, data = fyke99_binary_train[, -1], y = fyke99_binary_train$wfl_binary)

# Calculate all effects using ALE
effs <- FeatureEffects$new(predictor, grid.size = 50)


## Day of year

# Extract rug data
rug <- select(fyke99_binary_train, haul.date_jul)

# Convert julian day to date with 1 = November 1
rug$haul.date_jul <- as.Date(rug[,1], origin = "1999-11-01")

# Extract rf effects for haul date jul
ale <- effs$results$haul.date_jul

# Limit to class = 1
ale <- filter(ale, .class == 1)

# Convert logits to probabilities
ale$.value <- exp(ale$.value) / (1 + exp(ale$.value))

# Convert julian day to date with 1 = November 1
ale$.borders <- as.Date(ale$.borders, origin = "1999-11-01")

# Plot ALE with rug
day_rfc <- ggplot(ale, aes(x = .borders, y = .value)) +
  geom_line(color = "black") +                     # ALE line
  geom_rug(data = rug, aes(x = haul.date_jul), inherit.aes = FALSE,
           sides = "b", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Day of Year",
       y = "Occurrence Effect") +
  # scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr()


## Set occurrence

# Extract rug data
rug <- select(fyke99_binary_train, set.occurrence_yr)

# Extract rf effects for set occurrence
ale <- effs$results$set.occurrence_yr

# Limit to class = 1
ale <- filter(ale, .class == 1)

# Convert logits to probabilities
ale$.value <- exp(ale$.value) / (1 + exp(ale$.value))

# Plot ALE with rug
set_rfc <- ggplot(ale, aes(x = .borders, y = .value)) +
  geom_line(color = "black") +                     # ALE line
  geom_rug(data = rug, aes(x = set.occurrence_yr), inherit.aes = FALSE,
           sides = "b", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Set Occurrence",
       y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pubr()


## Pond

# Extract rug data
rug <- select(fyke99_binary_train, pond)

# Extract rf effects for pond
ale <- effs$results$pond

# Limit to class = 1
ale <- filter(ale, .class == 1)

# Convert logits to probabilities
ale$.value <- exp(ale$.value) / (1 + exp(ale$.value))

# All three ponds are at 0.5, so pond must only be an interacting effect


## Station

# Extract rug data
rug <- select(fyke99_binary_train, station)

# Summarize rug as total number of samples per station
rug <- rug %>% group_by(station) %>% summarise(n = n())

# Extract rf effects for station
ale <- effs$results$station

# Limit to class = 1
ale <- filter(ale, .class == 1)

# Convert logits to probabilities
ale$.value <- exp(ale$.value) / (1 + exp(ale$.value))

# Select columns
ale <- select(ale, .value, .borders)

# Rename columns
colnames(ale) <- c("occurrence", "station")

# Rename ale
station_effects <- ale

# Add sample size to station_effects
station_effects <- left_join(station_effects, rug, by = "station")




##### RFM Regression #####

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
vimportance$var <- c("Year", "Day of Year", "Station", "Water Temp", "Pond", "Soak Period")

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
  ylab("Increase in MSE (%)") +
  ggtitle("Regression Model") +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme_pubr() +
  theme(legend.position = "bottom",
    plot.title = element_text(hjust = 0.5))


##### ALE Regression #####

# Predictor object to hold model and data
predictor <- Predictor$new(rf_f99_freq, data = fyke99_freq_train[, -1], y = fyke99_freq_train$wfl_freq)

# Calculate all effects using ALE
effs <- FeatureEffects$new(predictor, grid.size = 50)


## Day of year

# Extract rug data
rug <- select(fyke99_freq_train, haul.date_jul)

# Convert julian day to date with 1 = November 1
rug$haul.date_jul <- as.Date(rug[,1], origin = "1999-11-01")

# Extract rf effects for haul date jul
ale <- effs$results$haul.date_jul

# Convert julian day to date with 1 = November 1
ale$.borders <- as.Date(ale$.borders, origin = "1999-11-01")

# Plot ALE with rug
day_rfr <- ggplot(ale, aes(x = .borders, y = .value)) +
  geom_line(color = "black") +                     # ALE line
  geom_rug(data = rug, aes(x = haul.date_jul), inherit.aes = FALSE,
           sides = "b", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Day of Year",
       y = "Abundance Effect") +
  # scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-10, 10)) +
  theme_pubr()


## Water temp

# Extract rug data
rug <- select(fyke99_freq_train, water.temp_c)

# Extract rf effects for water temp
ale <- effs$results$water.temp_c

# Remove oulier
ale <- filter(ale, .borders < 15)
rug <- filter(rug, water.temp_c < 15)

# Plot ALE with rug
water.temp_rfr <- ggplot(ale, aes(x = .borders, y = .value)) +
  geom_line(color = "black") +                     # ALE line
  geom_rug(data = rug, aes(x = water.temp_c), inherit.aes = FALSE,
           sides = "b", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Water Temp (°C)",
       y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-10, 10)) +
  theme_pubr()


## Soak period

# Extract rug data
rug <- select(fyke99_freq_train, soak_days)

# Extract rf effects for soak days
ale <- effs$results$soak_days

# Plot ALE with rug
soak_rfr <- ggplot(ale, aes(x = .borders, y = .value)) +
  geom_line(color = "black") +                     # ALE line
  geom_rug(data = rug, aes(x = soak_days), inherit.aes = FALSE,
           sides = "b", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Soak Period (Days)",
       y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-10, 10)) +
  theme_pubr()


## Pond

# Extract rug data
rug <- select(fyke99_freq_train, pond)

# Extract rf effects for pond
ale <- effs$results$pond

# There are some effects here, but all quite small (<1)


## Station

# Extract rf effects for station
ale <- effs$results$station

# Select columns
ale <- select(ale, .value, .borders)

# Rename columns
colnames(ale) <- c("abundance", "station")

# Join to station data
station_effects <- left_join(station_effects, ale, by = "station")

# Reorder columns of station_effects
station_effects <- select(station_effects, station, n, abundance, occurrence)


# # Haul winter
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "haul.winter",
#   plot = FALSE))
# 
# # Save data for later
# pd_year <- pd
# 
# # Plot partial dependence
# year_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Haul Winter") +
#   ylab("Frequency") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 25)) +
#   theme_pubr()
# 
# 
# 
# 
# # Haul date
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "haul.date_jul",
#   plot = FALSE))
# 
# # Convert x to date class as Julian date with 1 = November 1
# pd$x <- as.Date(pd$x, origin = "1999-11-01")
# 
# # Plot partial dependence
# date_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Day of Year") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr()
# 
# 
# # Water temp
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "water.temp_c",
#   plot = FALSE))
# 
# # Plot partial dependence
# water_temp <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Water Temp (°C)") +
#   ylab("") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr()
# 
# 
# # Station
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "station",
#   plot = FALSE))
# 
# # Make x factor in decreasing order of y
# pd$x <- fct_reorder(pd$x, pd$y, .desc = TRUE)
# 
# # Add empty column for sample size annotations
# pd$n <- NA
# 
# # Add sample size annotations based on fyke99
# for (i in 1:nrow(pd)) {
#   pd$n[i] <- length(fyke99$station[fyke99$station == pd$x[i]])
# }
# 
# # Add pond column
# pd$pond <- str_sub(pd$x, 1, 2)
# 
# # Make ponds full names
# for (i in 1:length(pd$pond)){
#   if (pd$pond[i] == "NP") {
#     pd$pond[i] <- "Ninigret"
#   } else if (pd$pond[i] == "PJ") {
#     pd$pond[i] <- "Point Judith"
#   } else {
#     pd$pond[i] <- "Potter"
#   }
# }
# 
# # Add manual alpha column
# pd$alpha <- ifelse(pd$n < 11, 0.4, 1)
# 
# # Custom color palette
# colors <- c("Potter" = "#FFC20A", "Ninigret" = "#D41159", "Point Judith" = "#0C7BDC")
# 
# # Plot partial dependence
# station_1999_freq <- ggplot(pd, aes(x = x, y = y, label = n, fill = pond, alpha = alpha)) +
#   geom_bar(stat = "Identity") +
#   geom_text(size = 3, nudge_y = 0.5) +
#   xlab("Station") +
#   ylab("") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12), clip = "off") +
#   scale_fill_manual(values = colors) +
#   scale_alpha_identity() +
#   guides(alpha = "none") +
#   theme_pubr() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#     legend.title = element_blank(),
#     legend.position = "top"
#     #plot.margin = unit(c(0.5, 0.1, 0, 0.1), "in")
#     )
# 
# 
# # Soak days
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "soak_days",
#   plot = FALSE))
# 
# # Plot partial dependence
# soak_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Soak Period (Days)") +
#   ylab("Frequency") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr()
# 
# 
# # Pond
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f99_freq,
#   pred.data = x, x.var = "pond",
#   plot = FALSE))
# 
# # Rename ponds
# pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))
# 
# # Add column for color
# pd$color <- c("#D41159", "#0C7BDC", "#FFC20A")
# 
# # Plot partial dependence
# pond_1999_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_bar(stat = "Identity", fill = pd$color) +
#   xlab("Pond") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr()




# ##### ALE Abundance Index #####
# 
# # Make haul winter an integer
# fyke99_freq_train$haul.winter <- as.integer(fyke99_freq_train$haul.winter)
# 
# # Subset for each pond
# np <- fyke99_freq_train %>% filter(pond == "NP")
# pp <- fyke99_freq_train %>% filter(pond == "PP")
# pj <- fyke99_freq_train %>% filter(pond == "PJ")
# 
# # Calculate ALE for each pond
# predictor_np <- Predictor$new(rf_f99_freq, data = np[, -1], y = np$wfl_freq)
# predictor_pp <- Predictor$new(rf_f99_freq, data = pp[, -1], y = pp$wfl_freq)
# predictor_pj <- Predictor$new(rf_f99_freq, data = pj[, -1], y = pj$wfl_freq)
# 
# # Feature effect of year for each pond
# ale_np <- FeatureEffect$new(predictor_np, "haul.winter", grid.size = 50)
# ale_pp <- FeatureEffect$new(predictor_pp, "haul.winter", grid.size = 50)
# ale_pj <- FeatureEffect$new(predictor_pj, "haul.winter", grid.size = 50)
# 
# # Extract results
# ale_np <- ale_np$results
# ale_pp <- ale_pp$results
# ale_pj <- ale_pj$results
# 
# # Correct results for mean abundance
# ale_np$.value <- ale_np$.value + mean(fyke99_freq_train$wfl_freq)
# ale_pp$.value <- ale_pp$.value + mean(fyke99_freq_train$wfl_freq)
# ale_pj$.value <- ale_pj$.value + mean(fyke99_freq_train$wfl_freq)
# 
# 
# ## Non-corrected abundance indices
# 
# # Make a non-corrected annual abundance index based on mean freq for Ninigret pond
# abundance_np <- fyke99 %>%
#   filter(pond == "NP") %>%
#   group_by(haul.winter) %>%
#   summarise(np_mean_freq = mean(wfl_freq, na.rm = TRUE))
# 
# # Rename haul winter to year
# colnames(abundance_np)[colnames(abundance_np) == "haul.winter"] <- "year"
# 
# # Non-corrected annual abundance for Potter Pond
# abundance_pp <- fyke99 %>%
#   filter(pond == "PP") %>%
#   group_by(haul.winter) %>%
#   summarise(pp_mean_freq = mean(wfl_freq, na.rm = TRUE))
# 
# # Rename haul winter to year
# colnames(abundance_pp)[colnames(abundance_pp) == "haul.winter"] <- "year"
# 
# # Non-corrected annual abundance for PJ Pond
# abundance_pj <- fyke99 %>%
#   filter(pond == "PJ") %>%
#   group_by(haul.winter) %>%
#   summarise(pj_mean_freq = mean(wfl_freq, na.rm = TRUE))
# 
# # Rename haul winter to year
# colnames(abundance_pj)[colnames(abundance_pj) == "haul.winter"] <- "year"
# 
# # Rename ALE columns
# colnames(ale_np) <- c("type", "np_pred_freq", "year")
# colnames(ale_pp) <- c("type", "pp_pred_freq", "year")
# colnames(ale_pj) <- c("type", "pj_pred_freq", "year")
# 
# 
# ## Merge data and split prediction columns
# 
# # Potter Pond
# abundance_pp <- left_join(abundance_pp, ale_pp, by = "year")
# 
# # Point Judith
# abundance_pj <- left_join(abundance_pj, ale_pj, by = "year")
# 
# # Ninigret Pond
# abundance_np <- left_join(abundance_np, ale_np, by = "year")
# 
# 
# 

##### PDP Abundance Indices #####

## PDP for haul winter for all ponds

# Make haul winter an integer
fyke99_freq$haul.winter <- as.integer(fyke99_freq$haul.winter)

# Calculate partial dependence
pd_year <- bind_rows(partialPlot(rf_f99_freq,
  pred.data = fyke99_freq_train, x.var = "haul.winter", n.pt = 26,
  plot = FALSE))

## Uncorrected abundance for all ponds
mean_abundance <- fyke99 %>%
 group_by(haul.winter) %>%
  summarise(mean_freq = mean(wfl_freq, na.rm = TRUE))


# ## Code for bootstrapped pdp predictions for error ribbon
# 
# # New predictor object
# predictor <- Predictor$new(rf_f99_freq, data = fyke99_freq)
# 
# # Compute PDP with multiple repetitions (for bootstrapping)
# pdp_list <- lapply(1:50,
#   function(i) {
#     sampled_data <- fyke99_freq[sample(nrow(fyke99_freq), replace = TRUE), ]
#     predictor_sampled <- Predictor$new(rf_f99_freq, data = sampled_data)
#     eff <- FeatureEffect$new(predictor_sampled, feature = "haul.winter", method = "pdp")
#     eff$results
#   }
# )
# 
# # Combine rows
# pdp_year <- do.call(rbind, pdp_list)
# 
# # Make haul winter an integer
# pdp_year$haul.winter <- as.integer(round2(pdp_year$haul.winter))
# 
# # Compute mean and error
# pdp_year_error <- pdp_year %>%
#   group_by(haul.winter) %>%
#   summarise(median_abundance = median(.value), 
#             lower = quantile(.value, 0.1),
#             upper = quantile(.value, 0.9),
#             .groups = "drop")





# ##### 2019 data binary response #####
# 
# # Remove freq response column
# fyke19_binary <- select(fyke19, -wfl_freq)
# 
# # Impute NAs using random forest proximity
# set.seed(45612)
# fyke19_binary <- rfImpute(wfl_binary ~ ., data = fyke19_binary)
# 
# # Select variables using VSURF
# set.seed(45612)
# vfyke19_binary <- VSURF(wfl_binary ~ ., data = fyke19_binary)
# 
# summary(vfyke19_binary)
# 
# plot(vfyke19_binary)
# 
# # Subset data for random forest
# fyke19_binary <- select(fyke19_binary, c("wfl_binary",
#   "haul.date_jul", "station", "pond", "set.occurrence_yr"))
# 
# # Bootstrap sample for model training
# set.seed(45612)
# fyke19_binary_train <- slice_sample(fyke19_binary, prop = 1, replace = TRUE)
# 
# # Out of bag sample for testing
# fyke19_binary_test <- setdiff(fyke19_binary, fyke19_binary_train)
# 
# # Data for RFM
# x <- select(fyke19_binary_train, -wfl_binary)
# y <- fyke19_binary_train$wfl_binary
# xtest <- select(fyke19_binary_test, -wfl_binary)
# ytest <- fyke19_binary_test$wfl_binary
# 
# # Tune RFM for optimal mtry
# set.seed(45612)
# tuneRF(x = x, y = y)
# 
# # mtry 4 is optimum based on OOB error
# 
# # Random forest model
# set.seed(45612)
# rf_f19_binary <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
#   ntree = 200, mtry = 4,
#   importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# 
# # Plot model
# plot(rf_f19_binary)
# 
# # Summary
# rf_f19_binary
# 
# # Error rate of 8.66% on training data and 28.1% on testing data
# 
# 
# ## Variable importance
# 
# # Variable importance
# vimportance <- as.data.frame(importance(rf_f19_binary))
# 
# # Clean variable names
# vimportance$var <- c("Day of Year", "Station", "Pond", "Set Occurrence")
# 
# # Reorder in terms of importance
# vimportance$var <- fct_reorder(vimportance$var, vimportance$MeanDecreaseAccuracy)
# 
# # Rename gini column
# colnames(vimportance)[colnames(vimportance) == "MeanDecreaseGini"] <- "Gini"
# 
# # Plot variable importance
# ggplot(vimportance, aes(x = var, y = MeanDecreaseAccuracy)) +
#   geom_segment(aes(x = var, xend = var, y = 0, yend = MeanDecreaseAccuracy), color = "skyblue") +
#   geom_point(aes(size = Gini), color = "blue", alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   xlab("") +
#   ylab("Mean Decrease in Accuracy") +
#   ggtitle("Five-Year Classification") +
#   theme_pubr() +
#   theme(legend.position = "bottom",
#     plot.title = element_text(hjust = 0.5))
# 
# 
# ## Partial dependence plots
# 
# # Haul date
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_binary,
#   pred.data = x, x.var = "haul.date_jul", which.class = "1",
#   plot = FALSE))
# 
# # Convert logits to probabilities
# pd$y <- exp(pd$y) / (1 + exp(pd$y))
# 
# # Convert x to date class as Julian date with 1 = November 1
# pd$x <- as.Date(pd$x, origin = "2019-11-01")
# 
# # Plot partial dependence
# date_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Haul Date") +
#   ylab("Catch Probability") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   ggtitle("Five-Year Classification") +
#   theme_pubr() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# 
# 
# 
# # Station
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_binary,
#   pred.data = x, x.var = "station", which.class = "1",
#   plot = FALSE))
# 
# # Convert logits to probabilities
# pd$y <- exp(pd$y) / (1 + exp(pd$y))
# 
# # Reorder following order of station_1999_binary
# pd$x <- factor(pd$x, levels = station_order)
# 
# # Add column for colors based on x including NP, PP, or PJ
# pd$color <- c(
#   "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
#   "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
#   "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
# )
# 
# # Plot partial dependence
# station_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
#   geom_bar(stat = "Identity", fill = pd$color) +
#   xlab("Station") +
#   ylab("Catch Probability") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   theme_pubr() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# # Set occurrence
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_binary,
#   pred.data = x, x.var = "set.occurrence_yr", which.class = "1",
#   plot = FALSE))
# 
# # Convert logits to probabilities
# pd$y <- exp(pd$y) / (1 + exp(pd$y))
# 
# # Plot partial dependence
# set_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Set Occurrence") +
#   ylab("Catch Probability") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   theme_pubr()
# 
# 
# # Pond
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_binary,
#   pred.data = x, x.var = "pond", which.class = "1",
#   plot = FALSE))
# 
# # Convert logits to probabilities
# pd$y <- exp(pd$y) / (1 + exp(pd$y))
# 
# # Rename ponds
# pd$x <- as.factor(c("Ninigret", "Point Judith", "Potter"))
# 
# # Add column for color
# pd$color <- c("#D41159", "#0C7BDC", "#FFC20A")
# 
# # Plot partial dependence
# pond_2019_binary <- ggplot(pd, aes(x = x, y = y)) +
#   geom_bar(stat = "Identity", fill = pd$color) +
#   xlab("Pond") +
#   ylab("Catch Probability") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   theme_pubr()
# 
# 
# 
# 
# ##### 2019 data freq response #####
# 
# # Remove binary response column
# fyke19_freq <- select(fyke19, -wfl_binary)
# 
# # Impute NAs using random forest proximity
# set.seed(45612)
# fyke19_freq <- rfImpute(wfl_freq ~ ., data = fyke19_freq)
# 
# # Select variables using VSURF
# set.seed(45612)
# vfyke19_freq <- VSURF(wfl_freq ~ ., data = fyke19_freq)
# 
# summary(vfyke19_freq)
# 
# plot(vfyke19_freq)
# 
# # Subset data for random forest
# fyke19_freq <- select(fyke19_freq, c("wfl_freq",
#   "haul.date_jul", "station", "set.occurrence_yr", "haul.winter"))
# 
# # Bootstrap sample for model training
# set.seed(45612)
# fyke19_freq_train <- slice_sample(fyke19_freq, prop = 1, replace = TRUE)
# 
# # Out of bag sample for testing
# fyke19_freq_test <- setdiff(fyke19_freq, fyke19_freq_train)
# 
# # Data for RFM
# x <- select(fyke19_freq_train, -wfl_freq)
# y <- fyke19_freq_train$wfl_freq
# xtest <- select(fyke19_freq_test, -wfl_freq)
# ytest <- fyke19_freq_test$wfl_freq
# 
# # Tune RFM for optimal mtry
# set.seed(45612)
# tuneRF(x = x, y = y)
# 
# # mtry 4 is optimum based on OOB error
# 
# # Random forest model
# set.seed(45612)
# rf_f19_freq <- randomForest(x = x, y = y, xtest = xtest, ytest = ytest,
#   ntree = 200, mtry = 4,
#   importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# 
# # Plot model
# plot(rf_f19_freq)
# 
# # Summary
# rf_f19_freq
# 
# # 62.2% of variance explained on training data and 31.0% on testing data
# 
# 
# ## Variable importance
# 
# # Variable importance
# vimportance <- as.data.frame(importance(rf_f19_freq))
# 
# # Clean variable names
# vimportance$var <- c("Day of Year", "Station", "Set Occurrence", "Year")
# 
# # Reorder in terms of importance
# vimportance$var <- fct_reorder(vimportance$var, vimportance$`%IncMSE`)
# 
# # Rename Node Purity column
# colnames(vimportance)[colnames(vimportance) == "IncNodePurity"] <- "Node Purity"
# 
# # Plot variable importance
# ggplot(vimportance, aes(x = var, y = `%IncMSE`)) +
#   geom_segment(aes(x = var, xend = var, y = 0, yend = `%IncMSE`), color = "skyblue") +
#   geom_point(aes(size = `Node Purity`), color = "blue", alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   xlab("") +
#   ylab("% Increase in MSE") +
#   ggtitle("Five-Year Regression") +
#   scale_size_continuous(breaks = scales::pretty_breaks(n = 2)) +
#   theme_pubr() +
#   theme(legend.position = "bottom",
#     plot.title = element_text(hjust = 0.5))
# 
# 
# ## Partial dependence plots
# 
# # Station
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_freq,
#   pred.data = x, x.var = "station",
#   plot = FALSE))
# 
# # Reorder following order of station_1999_binary
# pd$x <- factor(pd$x, levels = station_order)
# 
# # Add column for colors based on x including NP, PP, or PJ
# pd$color <- c(
#   "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159", "#D41159",
#   "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC", "#0C7BDC",
#   "#FFC20A", "#FFC20A", "#FFC20A", "#FFC20A"
# )
# 
# # Plot partial dependence
# station_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_bar(stat = "Identity", fill = pd$color) +
#   xlab("Station") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# # Haul date
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_freq,
#   pred.data = x, x.var = "haul.date_jul",
#   plot = FALSE))
# 
# # Convert x to date class as Julian date with 1 = November 1
# pd$x <- as.Date(pd$x, origin = "2019-11-01")
# 
# # Plot partial dependence
# date_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Haul Date") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   ggtitle("Five-Year Regression") +
#   theme_pubr() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# 
# 
# 
# # Set occurrence
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_freq,
#   pred.data = x, x.var = "set.occurrence_yr",
#   plot = FALSE))
# 
# # Plot partial dependence
# set_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Set Occurrence") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 12)) +
#   theme_pubr()
# 
# 
# # Haul winter
# 
# # Calculate partial dependence
# pd <- bind_rows(partialPlot(rf_f19_freq,
#   pred.data = x, x.var = "haul.winter",
#   plot = FALSE))
# 
# # Plot partial dependence
# year_2019_freq <- ggplot(pd, aes(x = x, y = y)) +
#   geom_line() +
#   geom_smooth(color = "blue") +
#   xlab("Haul Winter") +
#   ylab("Frequency") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 25)) +
#   theme_pubr()
# 
# 
# 
# 
# ##### Fig - Variable importance #####

# Combine variable importance plots
ggarrange(vars_1999_binary, vars_1999_freq,
  ncol = 2, nrow = 1)
  #labels = "AUTO", label.x = c(0.05, 0, 0.05, 0.05))

# Save plot
ggsave("figures/04_variable-importance.png", width = 10, height = 5, units = "in", dpi = 600)




##### Fig - ALE plots #####

# Prepare a blank ggplot object
blank <- ggplot() + theme_void()

# Combine ALE plots into one figure
ggarrange(
  day_rfc, set_rfc, blank,
  day_rfr, water.temp_rfr, soak_rfr,
  ncol = 3, nrow = 2)

# Save plot
ggsave("figures/04_ale-plots.png", width = 10, height = 8, units = "in", bg = "white", dpi = 600)




# ##### Fig - Environmental Variables #####
# 
# ## 1999 variables
# 
# # Water temp
# water_temp_1999 <- ggplot(fyke99, aes(x = 1, y = water.temp_c)) +
#   geom_violin(fill = "#0C7BDC", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Water Temp (°C)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Salinity
# salinity_1999 <- ggplot(fyke99, aes(x = 1, y = salinity_ppt)) +
#   geom_violin(fill = "#FFC107", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Salinity (PSU)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Wind
# wind_1999 <- ggplot(fyke99, aes(x = 1, y = wind_m.s)) +
#   geom_violin(fill = "#D41159", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Wind Speed (m/s)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# 
# ## 2019 variables
# 
# # Water temp range
# water_range_2019 <- ggplot(fyke19, aes(x = 1, y = range.water.temp_c)) +
#   geom_violin(fill = "#0C7BDC", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Water Temp Range (°C)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Water temp skewness
# water_skewness_2019 <- ggplot(fyke19, aes(x = 1, y = skewness.water.temp)) +
#   geom_violin(fill = "#0C7BDC", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Water Temp Skewness") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Water temp kurtosis
# water_kurtosis_2019 <- ggplot(fyke19, aes(x = 1, y = kurtosis.water.temp)) +
#   geom_violin(fill = "#0C7BDC", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Water Temp Kurtosis") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Salinity rate of change
# salinity_change_2019 <- ggplot(fyke19, aes(x = 1, y = delta.salinity_ppt.day)) +
#   geom_violin(fill = "#FFC107", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Salinity Rate of Change (PSU/day)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Dissolved oxygen
# do_2019 <- ggplot(fyke19, aes(x = 1, y = do_mg.l)) +
#   geom_violin(fill = "#004D40", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("Dissolved Oxygen (mg/L)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# # Dissolved oxygen rate of change
# do_change_2019 <- ggplot(fyke19, aes(x = 1, y = delta.do_mg.l.day)) +
#   geom_violin(fill = "#004D40", alpha = 0.5,
#     draw_quantiles = c(0.25, 0.75), linetype = "dashed") +
#   geom_violin(fill = "transparent", draw_quantiles = 0.5) +
#   xlab("") +
#   ylab("DO Rate of Change (mg/L/day)") +
#   theme_pubr() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# ## Combine plots
# 
# # Blank plot
# p <- ggplot() + theme_void()
# 
# # Combine plots
# ggarrange(water_temp_1999, salinity_1999, wind_1999, p, p, p,
#   water_range_2019, water_skewness_2019, water_kurtosis_2019,
#   salinity_change_2019, do_2019, do_change_2019,
#   labels = c("A", "", "", "", "", "",
#     "B", "", "", "", "", ""),
#   ncol = 6, nrow = 2)
# 
# # Save plot
# ggsave("figures/04_environmental_variables.png", width = 15, height = 6, units = "in", bg = "white")




##### Fig - Station Effects Map #####

# Import lat/lon
stations <- read_excel("data/raw-data/FykeStations.xlsx")

# Select columns
stations <- select(stations, Station_ID, Latitude, Longitude)

# Rename columns
colnames(stations) <- c("station", "lat", "lon")

# Merge with station data
station_effects <- left_join(station_effects, stations, by = "station")

# Group station effects by positive and negative
station_effects$posneg <- ifelse(station_effects$abundance > 0, "Positive", "Negative")

# Add a pond column
station_effects$pond <- substr(station_effects$station, 1, 2)

# Split into two separate data frames, one with n<30 and one with n>=30
station_effects1 <- filter(station_effects, n < 30)
station_effects2 <- filter(station_effects, n >= 30)


## Ninigret abundance

# Define the bounding box for Ninigret Pond
np_bbox <- c(-71.7, 41.34, -71.626912, 41.385)

# Get OSM data for waterbodies
np_map <- opq(bbox = np_bbox) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

# Plot the map
np_stationsplot <- ggplot() +
  geom_sf(data = np_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(np_bbox[1], np_bbox[3]), ylim = c(np_bbox[2], np_bbox[4])) +
  geom_point(data = station_effects1, 
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.7, -71.67, -71.64)) +
  scale_y_continuous(breaks = c(41.34, 41.36, 41.38)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_none(),
    size = guide_legend(title = "Abundance\nEffect Size")
  )


## Potter abundance

# Define the bounding box for Potter Pond
pp_bbox <- c(-71.545, 41.374, -71.5265, 41.40)

# Get OSM data for waterbodies
pp_map <- opq(bbox = pp_bbox) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

# Plot the map
pp_stationsplot <- ggplot() +
  geom_sf(data = pp_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(pp_bbox[1], pp_bbox[3]), ylim = c(pp_bbox[2], pp_bbox[4])) +
  geom_point(data = station_effects1,
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.54, -71.53)) +
  scale_y_continuous(breaks = c(41.37, 41.38, 41.39, 41.4)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_none(),
    size = guide_legend(title = "Abundance\nEffect Size")
  )


## Point Judith abundance

# Define the bounding box for Point Judith Pond
pj_bbox <- c(-71.518, 41.375, -71.49, 41.431)

# Get OSM data for waterbodies
pj_map <- opq(bbox = pj_bbox) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

# Plot the map
pj_stationsplot <- ggplot() +
  geom_sf(data = pj_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(pj_bbox[1], pj_bbox[3]), ylim = c(pj_bbox[2], pj_bbox[4])) +
  geom_point(data = station_effects1,
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(abundance)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.51, -71.49)) +
  scale_y_continuous(breaks = c(41.39, 41.41, 41.43)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_none(),
    size = guide_legend(title = "Abundance\nEffect Size")
  )


# Combine abundance plots
p1 <- ggarrange(np_stationsplot, pp_stationsplot, pj_stationsplot,
  ncol = 3, nrow = 1, common.legend = TRUE, legend = "right") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),  # White background for the whole arrangement
    panel.background = element_rect(fill = "white", color = "white")  # White background for the panels
  )


## Ninigret occurrence

# Plot the map
np_stationsplot <- ggplot() +
  geom_sf(data = np_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(np_bbox[1], np_bbox[3]), ylim = c(np_bbox[2], np_bbox[4])) +
  geom_point(data = station_effects1, 
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.7, -71.67, -71.64)) +
  scale_y_continuous(breaks = c(41.34, 41.36, 41.38)) +
  scale_size_continuous(breaks = c(0.03, 0.06, 0.09, 0.12)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_legend(title = "Effect"),
    size = guide_legend(title = "Occurrence\nEffect Size")
  )


## Potter

# Plot the map
pp_stationsplot <- ggplot() +
  geom_sf(data = pp_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(pp_bbox[1], pp_bbox[3]), ylim = c(pp_bbox[2], pp_bbox[4])) +
  geom_point(data = station_effects1,
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.54, -71.53)) +
  scale_y_continuous(breaks = c(41.37, 41.38, 41.39, 41.4)) +
  scale_size_continuous(breaks = c(0.03, 0.06, 0.09, 0.12)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_legend(title = "Effect"),
    size = guide_legend(title = "Occurrence\nEffect Size")
  )


## Point Judith

# Plot the map
pj_stationsplot <- ggplot() +
  geom_sf(data = pj_map$osm_multipolygons, fill = "gray80", color = "gray50") +
  coord_sf(xlim = c(pj_bbox[1], pj_bbox[3]), ylim = c(pj_bbox[2], pj_bbox[4])) +
  geom_point(data = station_effects1,
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 0.5) +
  geom_point(data = station_effects2,
    aes(x = lon, y = lat, color = posneg, size = abs(0.5 - occurrence)),
    alpha = 1) +
  ggtitle("") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-71.51, -71.49)) +
  scale_y_continuous(breaks = c(41.39, 41.41, 41.43)) +
  scale_size_continuous(breaks = c(0.03, 0.06, 0.09, 0.12)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White panel background
    plot.background = element_rect(fill = "white", color = "white")   # White overall plot background
  ) +
  guides(
    fill = guide_none(),
    color = guide_legend(title = "Effect"),
    size = guide_legend(title = "Occurrence\nEffect Size")
  )


# Combine plots
p2 <- ggarrange(np_stationsplot, pp_stationsplot, pj_stationsplot,
  ncol = 3, nrow = 1, common.legend = TRUE, legend = "right",
  labels = c("Ninigret Pond", "Potter Pond", "Point Judith Pond")) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),  # White background for the whole arrangement
    panel.background = element_rect(fill = "white", color = "white")  # White background for the panels
  )


# Combine abundance and occurrence
ggarrange(p2, p1,
  ncol = 1, nrow = 2)

# Save plot
ggsave("figures/04_station-effects-map.png", width = 10, height = 8, units = "in", dpi = 600)




##### Fig - Abundance Index #####

## Combined PDP index for all ponds

# Create custom rug to show all sampling by date
rug <- fyke99 %>%
  select(haul.winter, haul.date_jul)

# Prepare empty vector for date
rug$haul.date <- NA

# Convert Julian date to date object
for (i in 1:nrow(rug)){
  rug$haul.date[i] <- as.Date(rug$haul.date_jul[i], origin = paste0(rug$haul.winter[i] - 1, "-11-01"))
}

# Make haul.date a date object
rug$haul.date <- as.Date(rug$haul.date, format = "%Y-%m-%d")

# Convert haul.date to decimal years
rug$haul.date <- as.numeric(year(rug$haul.date) + (yday(rug$haul.date) - 1) / ifelse(leap_year(rug$haul.date), 366, 365))

# Plot index
ggplot(mean_abundance, aes(x = haul.winter)) +
  geom_line(aes(y = mean_freq, color = "Mean"), linetype = 2) +
  geom_line(data = pd_year, aes(x = x, y = y, color = "Corrected"), linewidth = 1) +
  geom_rug(data = rug, aes(x = haul.date), inherit.aes = FALSE,
    sides = "b", alpha = 0.1, size = 0.5) +
  xlab("Year") +
  ylab("Abundance (no. per haul)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  coord_cartesian(xlim = c(1999, 2024), ylim = c(0, 30)) +
  theme(legend.position = c(0.85, 0.85),
        legend.spacing.y = unit(0.1, 'cm'),  # Reduce space between legend entries
        legend.key.height = unit(0.4, 'cm'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Mean" = "black", "Corrected" = "blue"), guide = guide_legend(title = NULL))

# Save plot
ggsave("figures/04_abundance-index.png", width = 4, height = 3, units = "in", dpi = 600)



