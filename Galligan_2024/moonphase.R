#### Winter Flounder Fyke Net Analyses ####
#### R. Balouskus ####

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(splitstackshape)
library(openxlsx)
library(car)
library(plotrix)
library(reshape2)
library(lunar)


## Find Moon Stage by haul date
MoonPhase <- read_xlsx("1_tbl_FykeSets.xlsx")
MoonPhase$Haul_Date <- as.Date(MoonPhase$Haul_Date)
# for moonphase - value is radians - 0-new, pi-full, 3pi/2-last quarter
MoonPhase$MoonPhase <- lunar.phase(MoonPhase$Haul_Date, shift = -5)
#Converting moonphase to be linear from new to full
MoonPhase$MoonPhaselin <- ifelse(MoonPhase$MoonPhase > 3.141592, MoonPhase$MoonPhase-3.141592, MoonPhase$MoonPhase)  
MoonPhase$MoonPhaselin2 <- ifelse(MoonPhase$MoonPhase > 3.141592, 3.141592-MoonPhase$MoonPhaselin, MoonPhase$MoonPhase)
#For moonphase_8 - 1-new, 2-waxing crescent, 3-1st quarter, 4-waxing gibbouse, 5-full, etc.
MoonPhase$MoonPhase_8 <- lunar.phase(MoonPhase$Haul_Date, shift = -5, name = 8)
MoonPhase$MoonPhase_4 <- lunar.phase(MoonPhase$Haul_Date, shift = -5, name = 4)
## Moonstage with total catch
catch_moon8 <- MoonPhase %>%
  group_by(MoonPhase_8) %>%
  summarize(Mean = mean(WFL_Frequency),
            sd = sd(WFL_Frequency),
            se = std.error(WFL_Frequency),
            n=n())

catch_moon8_barplot <- ggplot(catch_moon8, aes(x=factor(MoonPhase_8), y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.2, position=position_dodge(.9)) +
  ggtitle("Mean Catch by Moon stage") +
  xlab("Year") +
  ylab("Mean Catch per Set") +
  theme_minimal()
catch_moon8_barplot + theme(
  plot.title = element_text(size=22, face="bold"),
  axis.title.x = element_text(size=16),
  axis.title.y = element_text(size=16),
  axis.text.x = element_text(size=12, angle=45, hjust=1),
  axis.text.y = element_text(size=16)
)
catch_moon8_barplot

catch_moon8_one_way <- aov(WFL_Frequency~MoonPhase_8, data = MoonPhase)
summary(catch_moon8_one_way)
TukeyHSD(catch_moon8_one_way)



catch_moon4 <- MoonPhase %>%
  group_by(MoonPhase_4) %>%
  summarize(Mean = mean(WFL_Frequency),
            sd = sd(WFL_Frequency),
            se = std.error(WFL_Frequency),
            n=n())
          
catch_moon4_barplot <- ggplot(catch_moon4, aes(x=factor(MoonPhase_4), y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.2, position=position_dodge(.9)) +
  ggtitle("Mean Catch by Moon stage") +
  xlab("Year") +
  ylab("Mean Catch per Set") +
  theme_minimal()
catch_moon4_barplot + theme(
  plot.title = element_text(size=22, face="bold"),
  axis.title.x = element_text(size=16),
  axis.title.y = element_text(size=16),
  axis.text.x = element_text(size=12, angle=45, hjust=1),
  axis.text.y = element_text(size=16)
)
catch_moon4_barplot

catch_moon4_one_way <- aov(WFL_Frequency~MoonPhase_4, data = MoonPhase)
summary(catch_moon4_one_way)
TukeyHSD(catch_moon4_one_way)

cor(MoonPhase$MoonPhaselin2, MoonPhase$WFL_Frequency)
cor(MoonPhase$MoonPhase, MoonPhase$WFL_Frequency)
plot(MoonPhase$MoonPhaselin2, MoonPhase$WFL_Frequency)
plot(MoonPhase$MoonPhase, MoonPhase$WFL_Frequency)
plot(MoonPhase$MoonPhase_8, MoonPhase$WFL_Frequency)

## Explore moonstage with length, sex, stage
MoonPhase_Indiv <- read_xlsx("Monthly_Length_Stage.xlsx")
MoonPhase_Indiv$Haul_Date <- as.Date(MoonPhase_Indiv$Haul_Date)
# for moonphase - value is radians - 0-new, pi-full, 3pi/2-last quarter
MoonPhase_Indiv$MoonPhase <- lunar.phase(MoonPhase_Indiv$Haul_Date, shift = -5)
#Converting moonphase to be linear from new to full
MoonPhase_Indiv$MoonPhaselin <- ifelse(MoonPhase_Indiv$MoonPhase > 3.141592, MoonPhase_Indiv$MoonPhase-3.141592, MoonPhase_Indiv$MoonPhase)  
MoonPhase_Indiv$MoonPhaselin2 <- ifelse(MoonPhase_Indiv$MoonPhase > 3.141592, 3.141592-MoonPhase_Indiv$MoonPhaselin, MoonPhase_Indiv$MoonPhase)
#For moonphase_8 - 1-new, 2-waxing crescent, 3-1st quarter, 4-waxing gibbouse, 5-full, etc.
MoonPhase_Indiv$MoonPhase_8 <- lunar.phase(MoonPhase_Indiv$Haul_Date, shift = -5, name = 8)
MoonPhase_Indiv$MoonPhase_4 <- lunar.phase(MoonPhase_Indiv$Haul_Date, shift = -5, name = 4)

### Stats show identical length fish caught by moon
MoonPhase4_Indiv_len <- MoonPhase_Indiv[!is.na(MoonPhase_Indiv$Length),]
MoonPhase4_Indiv <- MoonPhase4_Indiv_len %>%
  group_by(MoonPhase_4) %>%
  summarize(Mean = mean(Length),
            sd = sd(Length),
            se = std.error(Length),
            n=n())

##Sex with Moon
MoonPhase4_Indiv_sex <- MoonPhase_Indiv[!is.na(MoonPhase_Indiv$Sex),]
MoonPhase4_Indiv_sex_summary <- MoonPhase4_Indiv_sex %>%
  group_by(MoonPhase_4, Sex) %>%
  summarize(n=n())

MoonPhase4_Indiv_sex_summary$Sex <- as.factor(MoonPhase4_Indiv_sex_summary$Sex)

##does ratio of males to females change with moon stage...doesn't seem like it
sex_moon4_barplot <- ggplot(MoonPhase4_Indiv_sex_summary, aes(x=factor(MoonPhase_4), y=n)) +
  geom_bar(aes(fill = Sex), position = "dodge", stat="identity") +
  ggtitle("Sex Catch Composition by Moon stage") +
  xlab("Year") +
  ylab("Total Catch") +
  theme_minimal()
sex_moon4_barplot + theme(
  plot.title = element_text(size=22, face="bold"),
  axis.title.x = element_text(size=16),
  axis.title.y = element_text(size=16),
  axis.text.x = element_text(size=12, angle=45, hjust=1),
  axis.text.y = element_text(size=16)
)
sex_moon4_barplot

##Stage with Moon
MoonPhase4_Indiv_stage <- MoonPhase_Indiv[!is.na(MoonPhase_Indiv$Maturity_Stage),]
MoonPhase4_Indiv_stage_summary <- MoonPhase4_Indiv_stage %>%
  group_by(MoonPhase_4, Maturity_Stage) %>%
  summarize(n=n()) %>%
  mutate(freq = (n / sum(n)*100))

## Isolate jusr Ripe/rr/spent
MoonPhase4_Indiv_stage <- MoonPhase_Indiv[!is.na(MoonPhase_Indiv$Maturity_Stage),]
MoonPhase4_Indiv_stage <- MoonPhase4_Indiv_stage[!(MoonPhase4_Indiv_stage$Maturity_Stage == 0),]
MoonPhase4_Indiv_stage <- MoonPhase4_Indiv_stage[!(MoonPhase4_Indiv_stage$Maturity_Stage == 1),]
MoonPhase4_Indiv_stage <- MoonPhase4_Indiv_stage[!(MoonPhase4_Indiv_stage$Maturity_Stage == 5),]
MoonPhase4_Indiv_stage_summary <- MoonPhase4_Indiv_stage %>%
  group_by(MoonPhase_4, Maturity_Stage) %>%
  summarize(n=n()) %>%
  mutate(freq = (n / sum(n)*100))

##Plot it

MoonPhase4_Indiv_stage_summary$Maturity_Stage <- as.factor(MoonPhase4_Indiv_stage_summary$Maturity_Stage)

##does maturity change with moon stage...doesn't seem like it
stage_moon4_barplot <- ggplot(MoonPhase4_Indiv_stage_summary, aes(x=factor(MoonPhase_4), y=n)) +
  geom_bar(aes(fill = Maturity_Stage), position = "dodge", stat="identity") +
  ggtitle("Maturity Catch Composition by Moon stage") +
  xlab("Year") +
  ylab("Total Catch") +
  theme_minimal()
stage_moon4_barplot + theme(
  plot.title = element_text(size=22, face="bold"),
  axis.title.x = element_text(size=16),
  axis.title.y = element_text(size=16),
  axis.text.x = element_text(size=12, angle=45, hjust=1),
  axis.text.y = element_text(size=16)
)
stage_moon4_barplot
