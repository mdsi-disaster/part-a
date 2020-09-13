# Initial EDA on crime ---------------


library(readxl)
library(here)
library(skimr)
library(janitor)
library(tidyverse)
library(magrittr)
library(Amelia)
library(ggplot2)
library(descriptr)
library(mapproj)
library(ggmap)
library(DeducerSpatial)
library(urbnmapr)
library(viridis)

crime <- read_csv(here("data", "crimedata", "clean_cacrime.csv"))

#crime by year --------------

# are there any patterns in crime rate over the years? 

# create year level data 

yearlv <- crime %>%
  group_by(year) %>%
  summarise(av_crime = (mean(crime_index)), 
            av_violent = (mean(sum_violent_crime)),
            av_property = (mean(sum_property_crime)))

yearlv %>%
  ggplot(mapping = aes(x = year, y=av_violent)) + 
  geom_point()

yearlv %>%
  ggplot(mapping = aes(x=av_violent)) + 
  geom_histogram()

yearlv %>%
  ggplot(mapping = aes(x=year, y=av_violent)) + 
  geom_point() #seems like there is a dramatic decrease in violent crime from 2004 to 2005, however, this is the average of the violent crime for the year and doesn't take population into account 

yearlv %>%
  ggplot(mapping = aes(x=year, y=av_property)) + 
  geom_point() #seems like there is a dramatic decrease in property crime from 2004 to 2005, however, this is the average of the violent crime for the year and doesn't take population into account  

yearlv %>%
  ggplot(mapping = aes(x=year, y=av_crime)) + 
  geom_point() #seems like there is a dramatic increase in crime in 2005. Need to check how this is being calculated might be that the average is throwing this out. 

yearlv2 <- crime %>%
  group_by(year) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

yearlv2 %>%
  ggplot(mapping = aes(x=year, y=av_violent)) + 
  geom_point() #seems like there is a dramatic decrease in violent crime from 2004 to 2005, however, this is the average of the violent crime for the year and doesn't take population into account 

yearlv2 %>%
  ggplot(mapping = aes(x=year, y=av_property)) + 
  geom_point() #seems like there is a dramatic decrease in property crime from 2004 to 2005, however, this is the average of the violent crime for the year and doesn't take population into account  

yearlv2 %>%
  ggplot(mapping = aes(x=year, y=av_crime)) + 
  geom_point()

yearlv3 <- crime %>%
  group_by(year, county) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

yearlv3 %>%
  ggplot(mapping = aes(x=year, y=av_crime)) + 
  geom_point() +
  facet_wrap(~county)

yearlv3 %>%
  ggplot(mapping = aes(x=av_violent)) + 
  geom_histogram()

yearlv3 %>%
  ggplot(mapping = aes(x=av_property)) + 
  geom_histogram()

yearlv3 %>%
  ggplot(mapping = aes(x=av_crime)) + 
  geom_histogram()

yearlv3 %>%
  count(cut_width(av_crime, 1)) 

summary(crime)

# arson ----------

crime %>% #look for unuasual values 
  filter(arson > 10 ) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = arson), binwidth = 5) +
  coord_cartesian(ylim = c(0, 50)) #make it easier to see the values / zoom in. 

#why are there some values that are higher than 2000? These are for the more densely populated cities

arson <- crime %>%
  count(arson, city, county) #confirming that the cities with high counts of arson are all within top ten most populated cities of California 

# murder -----------

crime %>% #look for unuasual values 
  filter(murder_and_nonnegligent_manslaughter > 10 ) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = murder_and_nonnegligent_manslaughter), binwidth = 5) +
  coord_cartesian(ylim = c(0, 50)) #make it easier to see the values / zoom in. 

murder <- crime %>%
  count(murder_and_nonnegligent_manslaughter, city, county) #confirming that the cities with high counts of murder are all within top ten most populated cities of California as expected 

crime %>% #look for unuasual values 
  filter(rape > 10 ) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = rape), binwidth = 5) +
  coord_cartesian(ylim = c(0, 50)) #make it easier to see the values / zoom in. 

rape <- crime %>%
  count(rape, city, county) #confirming that the cities with high counts of murder are all within top ten most populated cities of California as expected 

rape <- crime %>%
  filter(rape > quantile(crime$rape, 0.75)) # only inlcude the cities that are in the3rd quartile 

crime %>%
  filter(city == "los angeles") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = sum_violent_crime))

crime %>%
  filter(year == 1999)

ds_freq_table(crime, year, 40)

ds_extreme_obs(crime, rape)

# highest crime by county --------------

yearlv4 <- crime %>% #this is grouping values by year 
  subset(year != 2019) %>%
  group_by(year, city, county) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100)) 

yearlv4 %>%
  filter(city == "los angeles") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = av_crime))

yearlv4 %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = county))

# looks like los angeles, alameda, monterey and san mateo have the highest crime rates 

yearlv4 %>%
  filter(county %in% c("san mateo", "los angeles", "alameda", "monterey")) %>%
  filter(av_crime < 100) %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = county))

yearlv4 %>% #in this graph can still see the points for each of the cities for each county in 
  filter(year == 2000 & county == "los angeles") %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = city)) 

countycrime <- crime %>%
  subset(year != 2019) %>% #remove 2019 since there is only half a year of data
  group_by(county) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

countycrime %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = county)) #highest crime rate from 2000 - 2019 appears to be in Tuolumne, Tehama, Sanfrancisco 

#highest crime by city -----------

citycrime <- crime %>%
  subset(year != 2019) %>%
  group_by(city, county) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

citycrime %>%
  filter(av_crime < 50) %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = city)) #highest crime rate from 2000 - 2019 appears to be in Tuolumne, Tehama, Sanfrancisco 

yearlv4 %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = city))

summary(yearlv4$av_crime)

yearlv4 %>%
  filter(av_crime < 4.581) %>%
  ggplot() +
  geom_point(mapping = aes(x = av_crime, y = city))


