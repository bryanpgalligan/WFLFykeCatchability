#### Temperature plots ####

## This code runs through the in-water data loggers placed
## on the fyke nets to generate summary statistics. DO NOT
## USE THE SALINITY VALUES HERE AS SOME OF THE SENSORS ARE
## NOT WORKING.

setwd("C:/Users/richard.balouskus.ENTERPRISE/OneDrive - State of Rhode Island/SharedDrive/WinterFlounder/Database/StarOddi/Trimmed/2022-2023")

library(readxl)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(openxlsx)

folder <- "C:/Users/richard.balouskus.ENTERPRISE/OneDrive - State of Rhode Island/SharedDrive/WinterFlounder/Database/StarOddi/Trimmed/2022-2023/"
file_list <- list.files(path = folder, pattern = "*.xlsx")

#add_obs <- function(one_file) {
 # mpa <- read_xlsx(one_file)
  #mpa$obs <- 1:nrow(mpa)
  #write.xlsx(mpa,
   #          file = sub("\\.xlsx", "_new\\.xlsx", one_file),
    #         append = FALSE)
  #return(mpa)
#}

#res <-lapply(file_list, add_obs)
######################################################################
### !!!!  After you run this step - MOVE all original files so code can run
###!!!!!!!!!!!!!!!!!!!!!!

#################
#file_list2 <- list.files(path = folder, pattern = "*.xlsx")

#################### This works, just add in more variables - check results in original excel files
tempmean <- rep(0,length(file_list))
tempmin <- rep(0,length(file_list))
tempmax <- rep(0,length(file_list))
tempmedian <- rep(0,length(file_list))
tempdiff <- rep(0,length(file_list))
#tempslope <- rep(0,length(file_list))
## This is the only part missing. It is not holding the right size for the output of LM - if can figure out, it will work
#tempslopecalc <- rep(0,length(file_list))
depthmean <- rep(0,length(file_list))
salinmean <- rep(0,length(file_list))
salinmin <- rep(0,length(file_list))
salinmax <- rep(0,length(file_list))
salinmedian <- rep(0,length(file_list))
  
for(i in 1:length(file_list)){
  data <- read_xlsx(file_list[i])
  names(data) <-make.names(names(data),unique = TRUE)
  #tempslopecalc <- lm(Date...Time ~ Temp..C., data=data)
  tempmean[i] <- mean(data$Temp..C.)
  tempmin[i] <- min(data$Temp..C.)
  tempmax[i] <- max(data$Temp..C.)
  tempmedian[i] <- median(data$Temp..C.)
  tempdiff[i] <- first(data$Temp..C.) - last(data$Temp..C.)
  #tempslopecalc[i] <- lm(data$Temp..C. ~ data$obs)
  #tempslope[i] <- summary(tempslopecalc$coefficients[2, 1])
  depthmean[i] <- mean(data$Depth.m.)
  salinmean[i] <- mean(data$Salinity.psu.)
  salinmin[i] <- min(data$Salinity.psu.)
  salinmax[i] <- max(data$Salinity.psu.)
  salinmedian[i] <- median(data$Salinity.psu.)
}
result <- cbind(file_list,tempmean, tempmedian, tempmin, tempmax, tempdiff, depthmean, salinmean, salinmedian, salinmin, salinmax)

write.csv(result,"wfl_star_oddi_results.csv")

##reset
# t1 <- read_xlsx("01032020_PJ1.xlsx")
# names(t1) <- make.names(names(t1), unique = TRUE)
# t1$obs <- 1:nrow(t1)
# test <- lm(Temp..C. ~ obs, data=t1)
# test3 <- lm(Temp..C. ~ nrow(data), data=t1)
# test2 <- lm(Temp..C. ~ Date...Time, data=t1)
# summary(test)
# summary(test2)
######################
######################
## Plotting ##########
######################

data <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read.table(paste(folder, x, sep=''), 
                              header = TRUE, 
                              stringsAsFactors = FALSE)))





#### Plotting
lapply(file_list, function(x) {
  TempData <- read_xlsx(x)
  names(TempData) <-make.names(names(TempData),unique = TRUE)

  TempPlot <- ggplot(data = TempData, aes(x = Date...Time)) +
    geom_line(aes(y = Temp..C.)) +
    labs(y = "Water Temperature [C]", x = "Date and Time") +
    scale_x_datetime("Date", date_labels = "%b %d",
    #date_breaks = "1 hour",
    sec.axis = dup_axis(name = "Time of Day",
    labels = scales::time_format("%I %p")))
  })

##Need to add print to file into a new loop. probably need tot change to a for loop instead of lapply

## file_list = gsub("\\.xlsx", "", file_list)


##lapply(names(plots), 
  ##     function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=plots[[x]]))



