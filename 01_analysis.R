##### Explore Winter Flounder for GAMS to look at catchability #####
##### Start just plotting catch with each variable - both real catch and binary #####

# Load packages
library(ggplot2)
library(mgcv)
library(PMCMR)
library(pracma)
library(readxl)
library(tidyr)

# Import data
fyke <- read_excel("data/raw-data/FykeSets.xlsx")

## Making Different Transformations for Analysis of Catch #s ###
fyke$bincatch <- ifelse(fyke$WFL_Frequency == 0,0,1)
fyke$freq_fourth <- (nthroot(fyke$WFL_Frequency, 4))
fyke$freq_log <- log(fyke$WFL_Frequency+1)

### Calculate Julian Day
fyke$Haul_Date <- as.POSIXlt(fyke$Haul_Date, format="%d-%b-%y")
fyke$Haul_Date_Jul <- fyke$Haul_Date
fyke$Haul_Date_Jul <- format(fyke$Haul_Date_Jul, "%j")
fyke$Haul_Date_Jul <- as.integer(fyke$Haul_Date_Jul)
  
### Convert Julian Day to be 1 at Nov 1 as that is earliest conceivable start date ####
fyke$Haul_Date_Jul_Nov <- ifelse(fyke$Haul_Date_Jul<200, fyke$Haul_Date_Jul+61, fyke$Haul_Date_Jul-304)

###Make a dataset trimmed to have No NAs for data of interest###
fyke_all <- fyke %>% drop_na(Soak_Time,Set_Water_Temp)

### Basic Scatter Plots ###
### Soak Time ###
ggplot(fyke_all, aes(x=Soak_Time, y=WFL_Frequency)) + 
  geom_point() +
  geom_smooth(method="auto")

ggplot(fyke_all, aes(x=Soak_Time, y=bincatch)) + 
  geom_point() +
  geom_smooth(method="auto")

### Set # Within Year ###
ggplot(fyke, aes(x=Set_Occ_Year, y=WFL_Frequency)) + 
  geom_point() +
  geom_smooth(method="auto")

ggplot(fyke, aes(x=Set_Occ_Year, y=bincatch)) + 
  geom_point() +
  geom_smooth(method="auto")

### Set Water Temp ###
ggplot(fyke_all, aes(x=Set_Water_Temp, y=WFL_Frequency)) + 
  geom_point() +
  geom_smooth(method="auto")

ggplot(fyke_all, aes(x=Set_Water_Temp, y=bincatch)) + 
  geom_point() +
  geom_smooth(method="auto")

### Year ###
ggplot(fyke, aes(x=Haul_Year_Winter, y=WFL_Frequency)) + 
  geom_point() +
  geom_smooth(method="auto")

ggplot(fyke, aes(x=Haul_Year_Winter, y=bincatch)) + 
  geom_point() +
  geom_smooth(method="auto")

### Julian Day ###
ggplot(fyke, aes(x=Haul_Date_Jul_Nov, y=WFL_Frequency)) + 
  geom_point() +
  geom_smooth(method="auto")

ggplot(fyke, aes(x=Haul_Date_Jul_Nov, y=bincatch)) + 
  geom_point() +
  geom_smooth(method="auto")

## Kruskal Wallis / ANOVAs ##
### Bartlett Test to check homogeneity of variance ###
### With a significant Result - variances are not equal, must run nonparametric ###

## Soak Time ###
bartlett.test(WFL_Frequency ~ Soak_Time, data=fyke_all)
boxplot(fyke_all$WFL_Frequency ~ fyke_all$Soak_Time)
KW<-kruskal.test(fyke_all$WFL_Frequency ~ factor(fyke_all$Soak_Time))
KW
posthoc.kruskal.nemenyi.test(x=fyke_all$WFL_Frequency, g=fyke_all$Soak_Time, method="Tukey")

### Set # Within Year ###
## Need to trim out the record with no pair ###
fyke_set <- fyke[!(fyke$Set_Occ_Year>21),]
bartlett.test(WFL_Frequency ~ Set_Occ_Year, data=fyke_set)
boxplot(fyke_set$WFL_Frequency ~ fyke_set$Set_Occ_Year)
KW<-kruskal.test(fyke_set$WFL_Frequency ~ factor(fyke_set$Set_Occ_Year))
KW
posthoc.kruskal.nemenyi.test(x=fyke_set$WFL_Frequency, g=fyke_set$Set_Occ_Year, method="Tukey")

### Year ###
bartlett.test(WFL_Frequency ~ Haul_Year_Winter, data=fyke)
boxplot(fyke$WFL_Frequency ~ fyke$Haul_Year_Winter)
KW<-kruskal.test(fyke$WFL_Frequency ~ factor(fyke$Haul_Year_Winter))
KW
posthoc.kruskal.nemenyi.test(x=fyke$WFL_Frequency, g=fyke$Haul_Year_Winter, method="Tukey")

### GAMs ###
### Need to make dataset that gets rid of all NAs for all these variable ####
### Need to make this for all data combinations

## Frequency Data###
# Freq - Gaussian
gam_all <- gam(WFL_Frequency~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                  s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20),
               data=fyke_all, method="REML")
gam.check(gam_all)
AIC(gam_all)
summary(gam_all)

# Freq Fourth root - Gaussian
gam_all_4 <- gam(freq_fourth~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                 s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), data=fyke_all, method="REML")
gam.check(gam_all_4)
AIC(gam_all_4)
summary(gam_all_4)

# Freq Log - Gaussian
gam_all_log <- gam(freq_log~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                   s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), data=fyke_all, method="REML")
gam.check(gam_all_log)
AIC(gam_all_log)
summary(gam_all_log)

# Freq - Poisson
gam_all_p <- gam(WFL_Frequency~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                 s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="poisson", data=fyke_all, method="REML")
gam.check(gam_all_p)
AIC(gam_all_p)
summary(gam_all_p)

# Freq Fourth root - Poisson
gam_all_4_p <- gam(freq_fourth~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                   s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="poisson", data=fyke_all)
gam.check(gam_all_4_p)
AIC(gam_all_4_p)
summary(gam_all_4_p)

# Freq Log - Poisson
gam_all_log_p <- gam(freq_log~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                     s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="poisson", data=fyke_all)
gam.check(gam_all_log_p)
AIC(gam_all_log_p)
summary(gam_all_log_p)

# Freq - NegBinom
gam_all_n <- gam(WFL_Frequency~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                   s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="nb", data=fyke_all, method="REML")
gam.check(gam_all_n)
AIC(gam_all_n)
summary(gam_all_n)

# Freq Fourth root - NegBinom
gam_all_4_n <- gam(freq_fourth~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                     s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="nb", data=fyke_all, method="REML")
gam.check(gam_all_4_n)
AIC(gam_all_4_n)
summary(gam_all_4_n)

# Freq Log - NegBinom
gam_all_log_n <- gam(freq_log~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                       s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), family="nb", data=fyke_all, method="REML")
gam.check(gam_all_log_n)
AIC(gam_all_log_n)
summary(gam_all_log_n)


## Binomial Data ###
# Pres/Absence - Binomial
gam_binom <- gam(bincatch~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                   s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15) +s(Haul_Date_Jul_Nov, k=20), binomial, data=fyke_all, method="REML")
gam.check(gam_binom)
AIC(gam_binom)
summary(gam_binom)$dev.expl
summary(gam_binom)

gam_binom <- gam(bincatch~s(Set_Occ_Year, k=15) + s(Soak_Time, k=8) +
                   s(Set_Water_Temp, k=40) + s(Haul_Year_Winter, k=15), binomial, data=fyke_all, method="REML")
gam.check(gam_binom)
AIC(gam_binom)
summary(gam_binom)$dev.expl
summary(gam_binom)

## SEe how model predict works if it all, from old satch code

##############################
##############################
##############################

### These are straight examples of soak time with set_occ year
gam_soak_year <- gam(WFL_Frequency~s(Soak_Time) + s(Set_Occ_Year), data=fyke)
gam.check(gam_soak_year)
gam_soak_year_4 <- gam(freq_fourth~s(Soak_Time) + s(Set_Occ_Year), data=fyke)
gam.check(gam_soak_year_4)
gam_soak_year_log <- gam(freq_log~s(Soak_Time) + s(Set_Occ_Year), data=fyke)
gam.check(gam_soak_year_log)

gam_soak_year_g <- gam(WFL_Frequency~s(Soak_Time) + s(Set_Occ_Year), family="gaussian", data=fyke)
gam.check(gam_soak_year_g)
gam_soak_year_p <- gam(WFL_Frequency~s(Soak_Time) + s(Set_Occ_Year), family="poisson", data=fyke)
gam.check(gam_soak_year_p)
gam_soak_year_nb <- gam(WFL_Frequency~s(Soak_Time) + s(Set_Occ_Year), family="nb", data=fyke)
gam.check(gam_soak_year_nb)
gam_soak_year_tw <- gam(WFL_Frequency~s(Soak_Time) + s(Set_Occ_Year), family="tw", data=fyke)
gam.check(gam_soak_year_tw)

