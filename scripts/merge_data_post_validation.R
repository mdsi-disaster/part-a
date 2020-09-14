library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)

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

## load clean data sets

crime <- read_csv("data/crimedata/clean_cacrime.csv")
earthquake <- read_csv("data/earthquake/earthquake_clean_v2.csv")
gdp <- read_csv("data/gdp/clean_ca_gdp_yoy_change.csv")
homelessness <- read_csv("data/homelessness/homelessness_clean.csv")
house_price <- read_csv("data/house_price/h_price_clean.csv")
income <- read_csv("data/income/earnings_clean.csv")
property <- read_csv("data/property_type/property_type_clean_county.csv")
faults <- read_csv("data/fault_lines/aggregate_faults_clean.csv")

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


## merging datasets
data <- merge(x=key, y=crime, by=c("city","county","year"), all=TRUE)

data <- merge(x=data,y=earthquake,by=c("county","month","year"), all=TRUE)

data <- merge(x=data, y=gdp,by="year",all=TRUE)

data <- merge(x=data, y=homelessness, by=c("county","year"), all=TRUE)

data <- merge(x=data, y=house_price, by=c("city","county","month","year"), all=TRUE)

data <- merge(x=data, y=income, by=c("county","month","year"), all=TRUE)

data <- merge(x=data, y=property, by=c("county","year"), all=TRUE)

data <- merge(x=data, y=faults, by="county", all=TRUE)

# write merged data file

write_csv(data, "data/merged_data_raw.csv")

str(data)

## analysis and cleaning of merged data

data_raw <- read_csv("data/merged_data_raw.csv", 
                    col_types = cols(city = col_character(), 
                                     county = col_character(),
                                     .default = "d"))

sapply(data_raw, function(x) sum(is.na(x)))

## identify values where year is NA
check_data <- data_raw %>%
  filter(is.na(year))

## we do not want any records where year is NA.
data <- data_raw %>%
  filter(!is.na(year))

sapply(data, function(x) sum(is.na(x)))

## check why city is NA
data %>%
  filter(is.na(city)) %>%
  count(county)

## ignore values from 1991 to 1997
data <- data %>% filter(year > 1997)
