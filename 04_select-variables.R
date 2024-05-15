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


##### Fyke 99 - Water Temp * Freq #####

##### Fyke 99 - Air Temp * Bin #####

##### Fyke 99 - Air Temp * Freq #####

##### Fyke 19 - Water Temp * Bin #####

##### Fyke 19 - Water Temp * Freq #####

##### Fyke 19 - Air Temp * Bin #####

##### Fyke 19 - Air Temp * Freq #####