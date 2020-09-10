#### AT2A - property_type merge ####

# load libraries
library(tidyverse)
library(readr)

# import csv files with selected columns only and remove first row
p_type_2010 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2010.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2011 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2011.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2012 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2012.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')
                                                                                    
p_type_2013 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2013.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2014 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2014.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2015 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2015.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2016 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2016.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2017 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2017.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

p_type_2018 <- read_csv("C:/Users/sandr/projects/part-a/data/property_type/housing_characteristics_2018.csv") %>% select('NAME','DP04_0006E','DP04_0007E','DP04_0007PE','DP04_0008E','DP04_0008PE','DP04_0009E','DP04_0009PE','DP04_0010E','DP04_0010PE','DP04_0011E','DP04_0011PE','DP04_0012E','DP04_0012PE','DP04_0013E','DP04_0013PE')

# Remove first row of each file
p_type_2010 <- p_type_2010[-1,]
p_type_2011 <- p_type_2011[-1,]
p_type_2012 <- p_type_2012[-1,]
p_type_2013 <- p_type_2013[-1,]
p_type_2014 <- p_type_2014[-1,]
p_type_2015 <- p_type_2015[-1,]
p_type_2016 <- p_type_2016[-1,]
p_type_2017 <- p_type_2017[-1,]
p_type_2018 <- p_type_2018[-1,]

# add year 
p_type_2010$year <- '2010'
p_type_2011$year <- '2011'
p_type_2012$year <- '2012'
p_type_2013$year <- '2013'
p_type_2014$year <- '2014'
p_type_2015$year <- '2015'
p_type_2016$year <- '2016'
p_type_2017$year <- '2017'
p_type_2018$year <- '2018'

# merge datasets 
p_type_2010_2018 <- rbind(p_type_2010,p_type_2011, p_type_2012, p_type_2013, p_type_2014, p_type_2015, p_type_2016, p_type_2017, p_type_2018)

# rename columns
p_type_2010_2018 <- p_type_2010_2018 %>%
  rename(county = 'NAME',
         'total_properties' = DP04_0006E,
         'house' = DP04_0007E,
         'per_house'= DP04_0007PE,
         'one_unit_attached' = DP04_0008E,
         'per_one_unit_attached' = DP04_0008PE,
         'two_units' = DP04_0009E,
         'per_two_units' = DP04_0009PE,
         'three_four_units' = DP04_0010E, 
         'per_three_four_units' = DP04_0010PE,
         'five_nine_units' = DP04_0011E,
         'per_five_nine_units' = DP04_0011PE,
         'mid_rise_apartments' = DP04_0012E, 
         'per_mid_rise_apartments' = DP04_0012PE,
         'high_rise_apartments' = DP04_0013E,
         'per_high_rise_apartments' = DP04_0013PE)

# Convert to numeric - identified one row that does not have values: Lake County, California - 2017
p_type_2010_2018[,2:17] <- sapply(p_type_2010_2018[,2:17],as.numeric)

# group by house, townhouse or apartment:
# 1_detached = house, 1_unit_attached and 2_units = townhouse, 3_4, 5_9 = low_rise_apartments, 10_19 = med_rise_apartments, 20_more = high_rise_apartments

p_type_2010_2018$townhouse <- p_type_2010_2018$one_unit_attached +     p_type_2010_2018$two_units
p_type_2010_2018$per_townhouse <- p_type_2010_2018$per_one_unit_attached + p_type_2010_2018$per_two_units

p_type_2010_2018$low_rise_apartments <- p_type_2010_2018$three_four_units + p_type_2010_2018$five_nine_units
p_type_2010_2018$per_low_rise_apartments <- p_type_2010_2018$per_three_four_units + p_type_2010_2018$per_five_nine_units

# keep relevant columns and re-order
p_type_2010_2018 <- p_type_2010_2018[,c(1,17,2:4,18:21,13:16)]

# Split name into county and state
p_type_2010_2018 <- p_type_2010_2018 %>% 
  separate(county, 
           sep = ", ", 
           into = c("county", "state"))

property_type_clean <- p_type_2010_2018 %>% separate(county, sep = " County", into = "county")

property_type_clean <- property_type_clean[,c(-2)]

property_type_clean$county <- tolower(property_type_clean$county)

# export csv
write.csv(property_type_clean,"./property_type_clean.csv", row.names = FALSE)

# import county file
clean_city_county <- read_csv("C:/Users/sandr/projects/part-a/data/crimedata/clean_city_county.csv")

# Keep county level only
clean_county <- clean_city_county[,2]
clean_county_grouped <- distinct(clean_county)

# missing 16 counties
test <- right_join(property_type_clean, clean_county_grouped, by = "county")

write.csv(test,"./property_type_clean_county.csv", row.names = FALSE)
