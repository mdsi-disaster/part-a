### Property_type EDA ###

library(readr)
library(funModeling)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)

# import dataset
property_type<- read_csv("data/property_type/property_type_clean_county.csv")

# Check for NAs - about 4.3% of values missing due to missing data in 15 counties
df_status(property_type)
summary(property_type)
class(property_type)
str(property_type)

property_type <- as.data.frame(property_type)
property_type$year <- as.factor(property_type$year) 

# Create group of all apartments in %
property_type$per_apartments <- property_type[,9] + property_type[,11] + property_type[,13]

# Insights for all of California
# Plot per_house
# group and remove NAs
property_house <- property_type %>%
  filter(year != 'NA') %>%
  group_by(year) %>%
  summarise(
    mean_per_house = mean(per_house, na.rm = TRUE),
    .groups = 'drop'
  )

# plot
house <- ggplot(property_house, aes(x=year, y=mean_per_house,group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Plot of average % of house in California",x="Year", y = "% house") + ylim(60,70)

# Plot per_townhouse
# group and remove NAs
property_townhouse <- property_type %>%
  filter(year != 'NA') %>%
  group_by(year) %>%
  summarise(
    mean_per_townhouse = mean(per_townhouse, na.rm = TRUE),
    .groups = 'drop'
  )

# plot
townhouse <- ggplot(property_townhouse, aes(x=year, y=mean_per_townhouse, group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Plot of average % of townhouse in California",x="Year", y = "% townhouse") + ylim(5,10)

# Plot per_apartments
# group and remove NAs
property_apartments <- property_type %>%
  filter(year != 'NA') %>%
  group_by(year) %>%
  summarise(
    mean_per_apartments = mean(per_apartments, na.rm = TRUE),
    .groups = 'drop'
  )

apartments <- ggplot(property_apartments, aes(x=year, y=mean_per_apartments, group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Plot of average % of apartments in California",x="Year", y = "% apartments") + ylim(20,22)


# Compare all 3 
grid.arrange(house, townhouse, apartments, nrow=3) # in 2013 the proportion of house increases whilst apartments decreases before continuing to rise year on year - could be interesting to plot vs prices in the area