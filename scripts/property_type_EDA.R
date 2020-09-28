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
house <- ggplot(property_house, aes(x=year, y=mean_per_house,group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Average % of houses",x="Year", y = "% house") + ylim(65,67)

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
townhouse <- ggplot(property_townhouse, aes(x=year, y=mean_per_townhouse, group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Average % of townhouses ",x="Year", y = "% townhouse") + ylim(7,9)

# Plot per_apartments
# group and remove NAs
property_apartments <- property_type %>%
  filter(year != 'NA') %>%
  group_by(year) %>%
  summarise(
    mean_per_apartments = mean(per_apartments, na.rm = TRUE),
    .groups = 'drop'
  )

apartments <- ggplot(property_apartments, aes(x=year, y=mean_per_apartments, group = 1)) + geom_line() + geom_point(color="red", size=3) + labs(title="Average % of apartments",x="Year", y = "% apartments") + ylim(20,22)


# Compare all 3 
grid.arrange(house, townhouse, apartments, nrow=3,top = 'Property types in California from 2010 to 2018') # in 2013 the proportion of house increases whilst apartments decreases before continuing to rise year on year - could be interesting to plot vs prices in the area

# map % per county 
property_county <- property_type %>%
  filter(year != 'NA') %>%
  group_by(county) %>%
  summarise(
    mean_per_house = mean(per_house, na.rm = TRUE),
    mean_per_apartments = mean(per_apartments, na.rm = TRUE),
    mean_per_townhouse = mean(per_townhouse, na.rm = TRUE),
    .groups = 'drop'
  )

property_county$state <- 'CA'

library(devtools)
library(usmap)

# merging wiht county.fips
county_map <- us_map(regions = "counties")

property_county$county <- paste(property_county$county, 'County', sep = " ")

property_county$county <- str_to_title(property_county$county)  

county_map_joined <- property_county %>% #joining the dataset 'county.fips' from the urbnmapr package so that I can add the fip codes to the fipcrime dataset
  left_join(county_map, by = "county")

# clean county_map_joined
# per_house
map_per_house <- county_map_joined[,c(12,2)]

map_house <- plot_usmap(data = map_per_house, values = "mean_per_house", include = "CA") + 
  scale_fill_gradient2(name = "% of House") + 
  labs(x = "",
       y = "", 
       title = "% of House per County",
       subtitle = "Average from 2010 - 2018",
       caption = "")


# per_townhouse
map_per_townhouse <- county_map_joined[,c(12,4)]

map_townhouse <- plot_usmap(data = map_per_townhouse, values = "mean_per_townhouse", include = "CA") + 
  scale_fill_gradient2(name = "% of Townhouse") + 
  labs(x = "",
       y = "", 
       title = "% of Townhouse per County",
       subtitle = "Average from 2010 - 2018",
       caption = "")

# per apartments
map_per_apartments <- county_map_joined[,c(12,3)]

map_apartments <- plot_usmap(data = map_per_apartments, values = "mean_per_apartments", include = "CA") + 
  scale_fill_gradient2(name = "% of Apartments") + 
  labs(x = "",
       y = "", 
       title = "% of Apartments per County",
       subtitle = "Average from 2010 - 2018",
       caption = "")

map_house
map_townhouse
map_apartments