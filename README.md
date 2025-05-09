# WFLFykeCatchability

This repo contains code and data to accompany Galligan et al. (in review).

This project analyzes 25 years of data from a fyke net (fixed gear) survey of Winter Flounder breeding subpopulations in three Rhode Island coastal ponds to predict abundance and gain insight into fish behavior as it relates to environmental and sampling variables.

### Abstract

Reduced fishing mortality has not led to a recovery of Winter Flounder in Southern New England. This study seeks to better understand the behavior and abundance of Winter Flounder in subpopulations from three coastal ponds by evaluating catch rates in a fixed gear survey, and accounting for catchability considerations to calculate an abundance index. We modeled Winter Flounder catch rates as a function of environmental and sampling factors for a fyke net survey conducted in three Rhode Island salt ponds over 25 years. The survey deployed fyke nets throughout the winter spawning season in Point Judith Pond, Potter Pond, and Ninigret Pond beginning in the winter of 1998–1999. Sampling variables (e.g., location, soak period) and environmental variables (e.g., water temperature, precipitation) were either collected during the course of the survey or compiled from external sources. Random forest models were used to predict relationships between candidate predictors and the occurrence and abundance of Winter Flounder in the catch. Seasonality (day of year) was strongly predictive of Winter Flounder capture, while most environmental variables were not, suggesting consistently timed migration and spawning movements that do not respond to environmental cues. Water temperature predicted abundance but not occurrence, consistent with previous observations of behavioral adaptations but not taxis during cold periods. The decoupling of environmental parameters from Winter Flounder catch rates suggests that behavioral rigidity in breeding adults could be one factor contributing to this species’ failure to recover. Improved abundance predictions suggest a continued decline for Rhode Island Winter Flounder, although this may not be true for all three subpopulations studied here.

## Data

### Complete Fyke Data

This is the comprehensive \*.csv file containing data from RIDMF's Fyke survey for winter flounder (*Pseudopleuronectes americanus*) as well as data compiled from other sources.

**Filepath:** `WFLFykeCatchability/data/clean-data/02_FykeSets_Complete.csv`

It contains **1055 observations** of **51 variables**, where each observation represents a fyke survey sampling event. The variables (columns) are:

| Variable | Units | Description | Source |
|------------------|------------------|------------------|------------------|
| event | N/A | A unique code for each sampling event that contains date and station information | RIDMF Fyke Survey |
| station | N/A | A unique code for each survey station | RIDMF Fyke Survey |
| pond | N/A | Abbreviation for the pond in which sampling took place. NP = Ninigret Pond; PJ = Point Judith Pond; PP = Potter Pond. | RIDMF Fyke Survey |
| set.date | yyyy-mm-dd | Date fyke was set | RIDMF Fyke Survey |
| haul.date | yyyy-mm-dd | Date fyke was hauled | RIDMF Fyke Survey |
| haul.month | mm | Month fyke was hauled | RIDMF Fyke Survey |
| haul.year | yyyy | Year fyke was hauled | RIDMF Fyke Survey |
| haul.winter | yyyy | Sampling year, where samples taken in december are recorded under the following year | RIDMF Fyke Survey |
| set.water.temp_c | Degrees Celsius | Water temperature when fyke was set | RIDMF Fyke Survey |
| set.air.temp_c | Degrees Celsius | Air temperature when fyke was set | RIDMF Fyke Survey |
| set.salinity_ppt | Parts per thousand | Salinity when fyke was set | RIDMF Fyke Survey |
| set.do_mg.l | Milligrams per liter | Dissolved oxygen when fyke was set | RIDMF Fyke Survey |
| haul.water.temp_c | Degrees Celsius | Water temperature when fyke was hauled | RIDMF Fyke Survey |
| haul.air.temp_c | Degrees Celsius | Air temperature when fyke was hauled | RIDMF Fyke Survey |
| haul.salinity_ppt | Parts per thousand | Salinity when fyke was hauled | RIDMF Fyke Survey |
| haul.do_mg.l | Milligrams per liter | Dissolved oxygen when fyke was hauled | RIDMF Fyke Survey |
| soak_days | Days | Number of days between set and haul | RIDMF Fyke Survey |
| set.occurrence_yr | Year^-1^ | Number of occurrences of a specific sampling location in a given sampling year, recorded for possible habituation effects | RIDMF Fyke Survey |
| wfl_freq | Count | Number of winter flounder caught | RIDMF Fyke Survey |
| hs.avg.air.temp_c | Degrees Celsius | Average of set and haul air temperatures | RIDMF Fyke Survey |
| hs.avg.water.temp_c | Degrees Celsius | Average of set and haul water temperatures | RIDMF Fyke Survey |
| hs.avg.salinity_ppt | Parts per thousand | Average of set and haul salinity observations | RIDMF Fyke Survey |
| hs.avg.do_mg.l | Milligrams per liter | Average of set and haul DO observations | RIDMF Fyke Survey |
| delta.water.temp_c | Degrees Celsius | Change in water temperature between set and haul observations | RIDMF Fyke Survey |
| delta.water.temp_c.day | Degrees Celsius per day | Slope of water temperature change between set and haul observations | RIDMF Fyke Survey |
| delta.salinity_ppt | Parts per thousand | Change in salinity between set and haul observations | RIDMF Fyke Survey |
| delta.salinity_ppt.day | Parts per thousand per day | Slope of salinity change between set and haul observations | RIDMF Fyke Survey |
| delta.do_mg.l | Milligrams per liter | Change in DO between set and haul observations | RIDMF Fyke Survey |
| delta.do_mg.l.day | Milligrams per liter per day | Slope of DO change between set and haul observations | RIDMF Fyke Survey |
| wfl_binary | Binary response variable | Presence/absence of winter flounder (0 = absent, 1 = present) | RIDMF Fyke Survey |
| haul.date_jul | Integer | Julian day with 1 = November 1 | RIDMF Fyke Survey |
| lunar.illumination | Percent | Average lunar illumination over the soak period. Where soak period was unavailable (2.7% of observations), illumination was calculated over the median soak time of four days. | Lazaridis, 2022 |
| mean.water.temp_c | Degrees Celsius | Mean water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| min.water.temp_c | Degrees Celsius | Minimum water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| max.water.temp_c | Degrees Celsius | Maximum water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| range.water.temp_c | Degrees Celsius | Range of water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| skewness.water.temp | N/A | Skewness of water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| kurtosis.water.temp | N/A | Kurtosis of water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| bimodality.water.temp | N/A | Bimodality of water temp collected by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| mean.depth_m | Meters | Mean depth recorded by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| min.depth_m | Meters | Minimum depth recorded by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| max.depth_m | Meters | Maximum depth recorded by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| range.depth_m | Meters | Range of depth observations recorded by *in situ* data loggers with observations every 15 minutes over soak period. Data collected beginning in 2019. | RIDMF Fyke Survey |
| noaa.avg.air.temp_c | Degrees Celsius | Average air temp recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.max.temp_c | Degrees Celsius | Maximum air temp recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.min.temp_c | Degrees Celsius | Minimum air temp recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.avg.high_c | Degrees Celsius | Average high temperature recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.temp.range_c | Degrees Celsius | Range between high and low temperature recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.precip_mm.day | Millimeters per day | Average daily precipitation recorded recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.wind_m.s | Meters per second | Average wind speed recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |
| noaa.heating.degrees_day | Degrees Celsius per day | Average heating degree days (relative to 18 C) recorded at nearby weather station (Ninigret Pond or Westerly State Airport) over the soak period. | NOAA, 2024 |

## Code

To replicate the analysis, the R scripts contained here should be run in sequential order, i.e., beginning with `01_load-packages.R` and ending with `06_mark-recapture-analysis.R`.

### Model Deployment

The random forest models developed for Galligan et al. (in review) are also provided in an adapted and streamlined version in the the quarto file `07_model-deployment.qmd`. This version of the project code executes only minimal data cleaning and is designed to produce a series of plots and data summaries in the form of an annual report with key information for Winter Flounder management in Rhode Island.

The models themselves should be referenced by citing Galligan et al. (in review). Specific outputs generated by the model deployment code can be referenced by citing both Galligan et al. (in review) and this software (Galligan & Balouskus 2025).

## **References**

Galligan, B. P., and R. G. Balouskus (2025). *WFLFykeCatchability: Drivers of Winter Flounder subpopulation catch rates* [Software]. <https://doi.org/10.5281/zenodo.14224683>

Galligan, B. P., M. C. McManus, and R. G. Balouskus. Drivers of Winter Flounder subpopulation catch rates in a fisheries-independent fixed gear survey. Transactions of the American Fisheries Society.

Lazaridis, E. (2022). lunar: Lunar phase & distance, seasons, and other environmental factors (version 0.2-01). R package. [https://CRAN.R-project.org/package=lunar](https://cran.r-project.org/package=lunar)

NOAA (2024). Global historical climatology network. <https://www.ncei.noaa.gov/cdo-web/datasets>
