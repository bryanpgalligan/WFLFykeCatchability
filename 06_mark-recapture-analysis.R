# This script summarizes mark recapture results

## Load data
fish <- read_excel("data/raw-data/IndividualWFLCaptured.xlsx")
recaps <- read_excel("data/raw-data/RecaptureLocs.xlsx")
  
# Standard error function
# Function to find SE (standard error) of the mean of a vector
se.mean <- function(x){
  
  # If there are NA in x
  if(sum(is.na(x)) > 0){
    
    # Remove NA
    x <- na.omit(x)
  }
  
  sd(x)/sqrt(length(x))
  
}


##### Summarize Mark Recapture Frequencies #####

# Fish tagged by pond
fish_summary <- fish %>%
  group_by(Tagged_YN, Pond_ID) %>%
  tally()

# Clean table
fish_summary <- filter(fish_summary, Tagged_YN == TRUE)
fish_summary <- fish_summary[, c(2, 3)]
fish_summary <- rename(fish_summary, "Tagged" = "n")

# Add columns for recapture counts
fish_summary$Recaptured_01 <- NA
fish_summary$Recaptured_02 <- NA

# Fill in recaptured_01
fish_summary$Recaptured_01[fish_summary$Pond_ID == "NP"] <- 
  nrow(filter(
    fish,
    Pond_ID == "NP" & Recaptured_1_0 == 1 & Recaptured_2_1_0 == 0
  ))
fish_summary$Recaptured_01[fish_summary$Pond_ID == "PJ"] <- 
  nrow(filter(
    fish,
    Pond_ID == "PJ" & Recaptured_1_0 == 1 & Recaptured_2_1_0 == 0
  ))
fish_summary$Recaptured_01[fish_summary$Pond_ID == "PP"] <- 
  nrow(filter(
    fish,
    Pond_ID == "PP" & Recaptured_1_0 == 1 & Recaptured_2_1_0 == 0
  ))

# Fill in recaptured_02
fish_summary$Recaptured_02[fish_summary$Pond_ID == "NP"] <- 
  nrow(filter(
    fish,
    Pond_ID == "NP" & Recaptured_2_1_0 == 1
  ))
fish_summary$Recaptured_02[fish_summary$Pond_ID == "PJ"] <- 
  nrow(filter(
    fish,
    Pond_ID == "PJ" & Recaptured_2_1_0 == 1
  ))
fish_summary$Recaptured_02[fish_summary$Pond_ID == "PP"] <- 
  nrow(filter(
    fish,
    Pond_ID == "PP" & Recaptured_2_1_0 == 1
  ))

# Rename pond column
fish_summary <- rename(fish_summary, "Rel_Pond" = "Pond_ID")

# Write to file
write_csv(fish_summary, "tables/06_recapture_summary.csv")




##### Plot Days at Large #####

# Rename columns
recaps <- recaps %>%
  rename(
    rel_lat = Latitude,
    rel_lon = Longitude,
    rec_lat = Tag_Recapture_Location_1_Lat,
    rec_lon = Tag_Recapture_Location_1_Lon,
    rel_date = Haul_Date,
    rec_date = Tag_Recapture_Date_1,
    rel_len = Length,
    rec_len = Tag_Recapture_Length_1
  )

# Add release pond to recaps
recaps$rel_loc <- substr(recaps$Station_ID, 1, 2)

# Summarize recap location
recaps$rec_loc <- NA
for (i in 1:nrow(recaps)){
  
  if (!is.na(recaps$Tag_Recapture_Location_1[i])){
    if (recaps$Tag_Recapture_Location_1[i] == "Ninigret Pond"){
      recaps$rec_loc[i] <- "NP"
    } else if (recaps$Tag_Recapture_Location_1[i] == "Point Judith Pond"){
      recaps$rec_loc[i] <- "PJ"
    } else if (recaps$Tag_Recapture_Location_1[i] == "Potter Pond"){
      recaps$rec_loc[i] <- "PP"
    } else {
      recaps$rec_loc[i] <- "Other"
    }
  }
  
}

# Summarize days at large to first recapture
recaps$days_at_large <- as.numeric(difftime(recaps$rec_date, recaps$rel_date, units = "days"))

# Add full pond names
recaps$rel_loc[recaps$rel_loc == "NP"] <- "Ninigret"
recaps$rel_loc[recaps$rel_loc == "PJ"] <- "Point Judith"
recaps$rel_loc[recaps$rel_loc == "PP"] <- "Potter"

# Create boxplot for days at large by release pond
ggplot(recaps, aes(x = rel_loc, y = days_at_large)) +
  geom_boxplot(outliers = FALSE) +
  labs(
    title = "",
    x = "",
    y = "Days at Large"
  ) +
  theme_pubr()

# Save image
ggsave("figures/06_days-at-large.png", width = 4, height = 4)

# Recapture frequency summary stats
mean(recaps$days_at_large, na.rm = TRUE)
se.mean(recaps$days_at_large)
max(recaps$days_at_large, na.rm = TRUE)
min(recaps$days_at_large, na.rm = TRUE)
nrow(filter(recaps, days_at_large <= 14))




##### Summarize Recapture Locations #####

# Summarize release locations
recap_summary <- recaps %>%
  group_by(rel_loc, rec_loc) %>%
  tally()

# Make rec_loc columns
recap_summary <- spread(recap_summary, key = rec_loc, value = n)

# View NP recaptures in PJ
df <- recaps %>% 
  select(rel_loc, rel_date, rec_loc, rec_date, Tag_Recapture_Method_1) %>%
  filter(rel_loc == "Ninigret") %>% 
  filter(rec_loc == "PJ")

# View PJ recaptures in PP
df <- recaps %>% 
  select(rel_loc, rel_date, rec_loc, rec_date, Tag_Recapture_Method_1) %>%
  filter(rel_loc == "Point Judith") %>% 
  filter(rec_loc == "PP")

# View PP recaptures in PJ
df <- recaps %>% 
  select(rel_loc, rel_date, rec_loc, rec_date, Tag_Recapture_Method_1) %>%
  filter(rel_loc == "Potter") %>% 
  filter(rec_loc == "PJ")

# Clean recapture summary
recap_summary <- select(recap_summary, rel_loc, NP, PJ, PP, Other)

# Write to file
write_csv(recap_summary, "tables/06_recapture_location_summary.csv")








