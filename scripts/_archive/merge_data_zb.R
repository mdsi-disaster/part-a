library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)
library(magrittr)

## load city,county key
city_key <- read_csv("data/keys/city_key.csv")
county_key <- read_csv("data/keys/county_key.csv")
date_key <- read_csv("data/keys/date_key.csv")

#clean date key
date_key$month <- as.numeric(date_key$month)

count_date_key <- date_key %>%
  count(year)

date_key <- date_key %>%
  filter(!is.na(month)) %>%
  filter(!is.na(year))

write_csv(date_key, "data/keys/date_key.csv")



## build overall key
key <- merge(x=city_key, y=county_key, by="county", all=TRUE)
key <- merge(x=key, y=date_key, all=TRUE)

## load clean data sets -----------------

crime <- read_csv("data/crimedata/clean_cacrime.csv")
earthquake <- read_csv("data/earthquake/earthquake_clean_v2.csv")
gdp <- read_csv("data/gdp/clean_ca_gdp_yoy_change.csv")
homelessness <- read_csv("data/homelessness/homelessness_clean.csv")
house_price <- read_csv("data/house_price/h_price_clean.csv")
income <- read_csv("data/income/earnings_clean.csv")
property <- read_csv("data/property_type/property_type_clean_county.csv")
unemployment <- read_csv("data/unemployment/unemployment_clean_keys.csv")

## rename clean data headers 

crime <- crime %>%
  rename(crime_murder = murder_and_nonnegligent_manslaughter,
         crime_rape = rape,
         crime_robbery = robbery,
         crime_assault = aggravated_assault,
         crime_burglary = burglary,
         crime_larceny = larceny_theft,
         crime_vehicle = vehicle_theft,
         crime_arson = arson,
         crime_violent_total = sum_violent_crime,
         crime_property_total = sum_property_crime,
         crime_total = sum_index_crimes)

income <- income %>%
  rename (income = earnings)

property <- property %>%
  rename(property_total = total_properties,
         property_house = house,
         property_house_per = per_house,
         property_townhouse = townhouse,
         property_townhouse_per = per_townhouse,
         property_low_rise = low_rise_apartments,
         property_low_rise_per = per_low_rise_apartments,
         property_mid_rise = mid_rise_apartments,
         property_mid_rise_per = per_mid_rise_apartments,
         property_high_rise = high_rise_apartments,
         property_high_rise_per = per_high_rise_apartments)


## merging datasets -----------------
#data <- merge(x=key, y=crime, by=c("city","county","year"), all=TRUE)

# data <- merge(x=data,y=earthquake,by=c("county","month","year"), all=TRUE)

data <- earthquake %>%
  left_join(crime, by = c("county", "year"))

data <- data %>%
  left_join(gdp, by = "year")

data <- data %>% 
  left_join(homelessness, by = c("county","year"))

data <- house_price %>%
  left_join(data, by = c("city","county","month","year")) 

data <- data %>%
  left_join(income, by=c("county","month","year"))

data <- data %>%
  left_join(property, by=c("county","year"))

data <- data %>%
  left_join(unemployment, by = c("city","county","month","year")) 

# write merged data file

write_csv(data, "data/merged/merged_data_raw_0914zb.csv")

str(data)

## cleaning of merged data ------------------

data_raw <- read_csv("data/merged/merged_data_raw_0914zb.csv")

sapply(data_raw, function(x) sum(is.na(x)))

## identify values where year is NA
check_data <- data_raw %>%
  filter(is.na(year)) #there are no observations where year is na 

# ## we do not want any records where year is NA.
# data <- data_raw %>%
#   filter(!is.na(year))

sapply(data, function(x) sum(is.na(x)))

## check why city is NA
data %>%
  filter(is.na(city)) %>%
  count(county) #there are no observations where city is na 

## check which years we have data for 
table(data_raw$year)

## check which years we have earthquake data  

quakes <- data_raw %>%
  drop_na(quakes_minor) #remove rows that are 'na' for quakes. 

table(quakes$year) #we only have data for quakes from 2010 to 2019

## simplify the quakes data frame by removing the individual crime counts that we don't need 

quakes <- quakes %>%
  select(-crime_murder, -crime_rape, -crime_arson, -crime_larceny, -crime_assault, -crime_burglary, -crime_robbery, -crime_vehicle) %>%
  mutate(crime_index = (crime_index*100000)) #multiply crime index by 100,000 to complete calculation for crime index 

summary(quakes)  

# merged dataset with years quakes = 'na' are removed 
write_csv(data, "data/merged/quakes.csv")
  