# Adjustment for crime rate and income --------------

library(readxl)
library(here)
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

income <- read_csv(here("data", "income", "bea_income.csv"))

unemployment <- read_csv(here("data", "unemployment", "unemployment.csv"))

annual_unemployment <- unemployment %>%
  group_by(year, area_name) %>%
  summarise(mean = mean(unemployment_rate))
  
annual_unemployment <- annual_unemployment %>%
  mutate(area_name = str_remove_all(area_name, " city")) %>%
  mutate(area_name = str_remove_all(area_name, " town")) %>%
  mutate(area_name = tolower(area_name)) %>%
  rename(city = "area_name", avg_unemployment = "mean")

income <- income %>%
  mutate(city = tolower(city)) 

#check for unique keys before merging data

annual_unemployment %>%
  count(year, city) %>%
  filter(n > 1)

income %>%
  count(year, city) %>%
  filter(n > 1)

summary(annual_unemployment)

summary(income)

merged <- crime %>% 
  select(-murder_and_nonnegligent_manslaughter, -rape, -robbery, -aggravated_assault, -burglary, -arson, -vehicle_theft, -larceny_theft, -sum_violent_crime, -sum_property_crime) %>%
  left_join(income) %>% #don't specify keys to join by so that all matching keys are joined
  select(-Code, -GeoFips, -GeoName, -CL_UNIT, -UNIT_MULT) %>%
  left_join(annual_unemployment) %>%
  mutate(crime_index = (crime_index*100))
  
missmap(merged)

namerged <- merged %>%
  na.omit(merged)

table <- namerged %>%
  count(county, year)

merged %>%
  ggplot(aes(avg_unemployment, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0,10)

merged %>%
  ggplot(aes(income_per_cap, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0, 5) 

# creating county level data 
countylv <- namerged %>%
  group_by(county, year) %>%
  summarise(av_unemp = (mean(avg_unemployment)), 
            av_income = (mean(income_per_cap)),
            av_crime = (mean(crime_index)))
 
unique(countylv$county) #looking at the unique county names, after removing na values, only 18 counties are left in the dataset

summary(countylv)

summary(countylv)[["3rd Qu."]]
           
countylv %>%
  ggplot(aes(av_unemp, av_crime)) +
  geom_point(alpha = 0.3) +
  ylim(0,10)

countylv %>%
  ggplot(aes(income_per_cap, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0, 5) 


fit0 <- lm(crime_index~avg_unemployment, data = merged)  

summary(fit0)$coef   

fit1 <- lm(crime_index~avg_unemployment + income_per_cap, data = merged)

summary(fit1)$coef   

merged %>%
  ggplot(aes(crime_index, city)) +
  geom_point(alpha = 0.3)


# EDA on crime ------------------

# Visualise the distribution of a categorical variables 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = city))

# can see that we don'd have every city for every year of crime data 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = county))

# for some reason there are a lot of instances of LA - need to check out what's happening there. There are 88 cities in LA and we have 20 years of data so it's possible that there are 1760 observations for LA.  Orange County has the next highest number of observations, OC has 34 cities, over 30 years of data, there could be 680 entries for OC cities, so the number of observations displayed is reasonable. 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = year))

#there is more data from 2005-2018. There is only a comparatively small amount of data from 2019. 

crime %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = year), binwidth = 5)

crime <- crime %>%
  mutate(crime_index = (crime_index*100)) #make the crime index into a percentage, easier number to work with 

crime %>%
  ggplot(mapping = aes(x = year, colour = county)) + 
  geom_freqpoly(binwidth = 0.1)

lacity <- crime %>%
  filter(county == "los angeles")

lacity %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = year))

lacity %>%
  ggplot() +
  geom_bar(mapping = aes(y = city))

lacitycount <- lacity %>%
  count(city)

#There are 87 cities in LA so it makes sense that there are over 1,500 observations for LA county 

crime %>% 
  count(city)

# which cities?  --------------

# There are 482 municipalities in California, how many of them are included in the crime data? Only 461. Which ones are missing from the crime data? 

cities <- read_csv(here("data", "crimedata", "clean_city_county.csv")) #load in all 482 city names 

crimecity <- unique(crime$city)

setdiff(cities$city, crime$city) #this shows the names of the 21 municipalities that we don't have crime data for. These municipalities mostly have a population of less than 10,000 people, except for Wasco which has a poppulation of 25,000 

#which counties? -----------

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



# EDA 2010 - 2018 Crime data --------------

# Which cities do we have data for ----------------

newcrime <- crime %>% #create a new data frame that's just for the years that we have earthquake data for 2010-2020. Remove 2019 since there are only 6 months worth of data for this year
  filter(year >= 2010 & year < 2019)

newcrime %>% #since there are 9 years of data, if there is data for each city for each year, I would expect to have a count of 9 for each of the cities
  count(city, county) %>%
  filter(n != 9) #there are 18 cities that don't have data for each year. 

setdiff(cities$city, newcrime$city) #this shows the names of the 21 municipalities that we don't have crime data for. These municipalities mostly have a population of less than 10,000 people, except for Wasco which has a poppulation of 25,000 

setdiff(cities$county, newcrime$county) #this shows the names of the 2 counties that we don't have crime data for: Sierra (~3,000 people) and Plumas (less than 20,000 people)

# which city has the highest crime index across the 8 year period? -----------

# Industry, Vernon and Sand City have the highest crime rates across the 8 year period

# Calipatria, Imperial and Etna have the lowest crime rates across the 8 year period

citycrime <- newcrime %>% 
  group_by(city, county) %>% 
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), #get the crime index averages when grouping by year 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

citycrime %>% 
  arrange(desc(av_crime)) # top 10 values are >= 7.26

citycrime %>% 
  arrange(desc(av_crime)) %>%
  filter(av_crime >= 7.26) %>%
  ggplot(mapping = aes(x = av_crime, y = city)) + 
  geom_point() 

citycrime %>% 
  arrange(av_crime) # lowest 10 values are <= 0.839

citycrime %>% 
  arrange(av_crime) %>%
  filter(av_crime <= 0.839) %>%
  ggplot(mapping = aes(x = av_crime, y = city)) + 
  geom_point() 



# Which Counties have the highest crime across the 8 year period ------------
# Tuolumne, Tehama and San Francisco have the highest crime rates 
# Calaveras, Colusa, San Benito have the lowest crime rates 

countycrime <- newcrime %>% 
  group_by(county) %>% 
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), #get the crime index averages when grouping by year 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

countycrime %>% 
  arrange(desc(av_crime)) # top 10 values are >= 4.42

countycrime %>% 
  arrange(desc(av_crime)) %>%
  filter(av_crime >= 4.42) %>%
  ggplot(mapping = aes(x = av_crime, y = county)) + 
  geom_point()

countycrime %>% 
  arrange(av_crime) # lowest 10 values are <= 2.26

countycrime %>% 
  arrange(av_crime) %>%
  filter(av_crime <= 2.26) %>%
  ggplot(mapping = aes(x = av_crime, y = county)) + 
  geom_point()

#Which year has the highest crime rate -----------

yearcrime <- newcrime %>%
  group_by(year) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_violent)) + 
  geom_point() #violent crime is at it's highest in 2017 and lowest in 2014

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_property)) + 
  geom_point() #property crime is highest in 2012 and lowest in 2018

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_crime)) + 
  geom_point() #crime index is highest in 2012 and lowest in 2018

ggplot(yearcrime, aes(year)) + 
  geom_line(aes(y=av_crime), colour="red") + 
  geom_line(aes(y=av_violent), colour="green") + 
  geom_line(aes(y=av_property), colour="blue")

# Which year and which city has the highest crime rate? 
# Industry, a city with ~219 people has the highest crime rate, followed by Vernon, a city with ~112 people, then Sand City ~334, Colma ~1792 Are these significant values? 

#taking out the cities with population that is less than 5000, Emeryville and Orange Cove are the top ten cities for crime index 

newcrime %>% 
  arrange(desc(crime_index)) %>%
  select(year, county, city, crime_index) 


newcrime %>% 
  filter(population >= 5000) %>%
  arrange(desc(crime_index)) %>%
  select(year, county, city, crime_index) 


# Crime trend in San Francisco -------------

sanfran <- newcrime %>%
  filter(city == "san francisco")

sanfran %>%
  ggplot(mapping = aes(x=year, y=crime_index)) + 
  geom_point() +
  geom_smooth()


# check out maps ------------
#https://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html

map("usa")
map("county")

map.cities(county = "California")

map("state", "CALIFORNIA")
data(us.cities)
map.cities(us.cities, country = "CA")

map("state", "CALIFORNIA")
map("county")

#https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2

fipcrime <- newcrime #new dataframe that I'll add fip codes to

fipcrime$polyname <- paste("california", fipcrime$county, sep = ",") #new column called polyname that I can use to merge fipcode varaible 

fipcrime <- fipcrime %>%
  left_join(county.fips, by = "polyname")

fipcrime %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "crime_index") +
  theme_urban_map()
                          
summary(fipcrime)

ggplot() +
  geom_polygon(data = fipcrime, 
               mapping = aes(x = long, y = lat, 
                             group = group)) + 
  geom_polygon(data = urbnmapr::)
