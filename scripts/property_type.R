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
p_type_2010$year <- '01-01-2010'
p_type_2011$year <- '01-01-2011'
p_type_2012$year <- '01-01-2012'
p_type_2013$year <- '01-01-2013'
p_type_2014$year <- '01-01-2014'
p_type_2015$year <- '01-01-2015'
p_type_2016$year <- '01-01-2016'
p_type_2017$year <- '01-01-2017'
p_type_2018$year <- '01-01-2018'

# merge datasets 
p_type_2010_2018 <- rbind(p_type_2010,p_type_2011, p_type_2012, p_type_2013, p_type_2014, p_type_2015, p_type_2016, p_type_2017, p_type_2018)

p_type_2010_2018 <- p_type_2010_2018 %>%
  rename(county = NAME,
         'total_properties' = DP04_0006E,
         'house' = DP04_0007E,
         '%_house'= DP04_0007PE,
         '1_unit_attached' = DP04_0008E,
         '%1_unit_attached' = DP04_0008PE,
         '2_units' = DP04_0009E,
         '%2_units' = DP04_0009PE,
         '3_4_units' = DP04_0010E, 
         '%3_4_units' = DP04_0010PE,
         '5_9_units' = DP04_0011E,
         '%5_9_units' = DP04_0011PE,
         '10_19_units' = DP04_0012E, 
         '%10_19_units' = DP04_0012PE,
         '20_more_units' = DP04_0013E,
         '%20_more_units' = DP04_0013PE)

# TO DO: group by house, townhouse or apartment:
# 1_detached = house, 1_unit_attached and 2_units = townhouse, 3_4, 5_9 = low_rise_apartments, 10_19 = med_rise_apartments, 20_more = high_rise_apartments

