

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

city_key <- read_csv("./data/crimedata/clean_city_county.csv")

county_key <- city_key %>%
  count(county) %>%
  select(county)

write_csv(city_key, "./data/keys/city_key.csv")
write_csv(county_key, "./data/keys/county_key.csv")


city_key <- read_csv("./data/keys/city_key.csv")
county_key <- read_csv("./data/keys/county_key.csv")

## Crime data cleaning

crime_clean <- read_csv("./data/crimedata/clean_cacrime.csv")

crime_keyed <- merge(x=city_key, y=crime_clean, by=c("city","county"), all.y=TRUE)

count_keys <- crime_clean %>%
  count(city,county,year)

## earthquake data cleaning


earthquake <- read_csv("./data/earthquake/earthquakes_clean.csv")

earthquake_clean <- earthquake %>%
  select(-city) %>%
  group_by(county,month,year) %>%
  summarise(quakes_minor = sum(quakes_minor),
            quakes_low = sum(quakes_low),
            quakes_moderate = sum(quakes_moderate),
            quakes_severe = sum(quakes_severe))

earthquake_counties <- earthquake %>%
  count(county) %>%
  select(county)

write_csv(earthquake_counties, "./data/earthquake/earthquake_counties.csv")
earthquake_counties <-read_csv("./data/earthquake/earthquake_counties.csv")

eq_keyed <- merge(x=earthquake_clean,y=earthquake_counties, by="county",all=TRUE)

eq_keyed <- eq_keyed %>%
  select(-county) %>%
  rename(county = county_key)

count_dupes <- eq_keyed %>%
  count(county,month,year)

count_invalid <- eq_keyed %>%
  filter(is.na(county))

write_csv(eq_keyed, "./data/earthquake/earthquake_clean_v2.csv")

### cleaning homeless data -
# the data included entries which were combined counties or counties and cities,
# as well as city only entries
# i decided to remove the city entries and averaged the shared counts across each county evenly,
# (observations / number of listed counties)
#I then mapped the data back to the county key for merging with larger dataset.


homeless <- read_csv("./data/homelessness/homelessness_inprogress.csv")
homeless_key <- read_csv("./data/homelessness/homelessness_inprogress_county_conversion_key.csv")

homeless_average <- homeless %>%
  mutate(homeless_count_calc = round(total_homeless_count / shared_obs)) %>%
  filter(shared_obs != 0)

homeless_clean <- merge(x=homeless_average, y=homeless_key,by="county_raw" , all=TRUE)

write_csv(homeless_clean, "./data/homelessness/homelessness_inprogress_v2.csv")

homeless_clean_v2 <- homeless_clean %>%
  select(year,county,homeless_count_calc) %>%
  rename(homeless_count = homeless_count_calc)

write_csv(homeless_clean_v2, "./data/homelessness/homelessness_clean.csv")

homeless_clean_v3 <- read_csv("./data/homelessness/homelessness_clean.csv")

homeless_clean_v3 <- homeless_clean_v3 %>%
  filter (county != "a city")

write_csv(homeless_clean_v3, "./data/homelessness/homelessness_clean.csv")

## Finish homeless cleaning

## cleaning earnings data -----

earnings <- read_csv("./data/income/cen_earnings_clean.csv")

earnings_counties <- earnings %>%
  count(county_name) %>%
  select(county_name)

write_csv(earnings_counties, "./data/income/earnings_county_key.csv")
earnings_key <- read_csv("./data/income/earnings_county_key.csv")

earnings <- earnings %>%
  select(-county)

earnings_months <- read_csv("./data/income/qtr_to_month.csv")

earnings_months_v2 <- earnings_months %>%
  select(month)

earnings_v2 = merge(x=earnings,y=earnings_months_v2,y.all=TRUE)

earnings_v3 = merge(x=earnings_v2, y=earnings_months,by=c("quarter","month"),y.all=TRUE)

earnings_clean = merge(x=earnings_v3, y=earnings_key,by="county_name",all=TRUE)

earnings_clean_v2 <- earnings_clean %>%
  select(county,month,year,earnings)

# check for dupes
earnings_clean_dupe <- earnings_clean %>%
  count(county,month,year)

# check for NAs
count_invalid <- earnings_clean %>%
  filter(is.na(county))

write_csv(earnings_clean_v2, "./data/income/earnings_clean.csv")


glimpse(earnings)

glimpse(earnings_months)



## look at unemployment

unemployment  <- read_csv("./data/unemployment/unemployment.csv")

## gdp

gdp  <- read_csv("./data/gdp/clean_ca_gdp_yoy_change.csv")

## property type

property <- read_csv("./data/property_type/property_type_clean_county.csv")

property_keyed <- merge(x=county_key, y=property, by=c("county"), y.all=TRUE)
                        

check_dupe <- property_keyed %>%
  count(county,year)

check_na <- property %>%
  filter(is.na(year))

## house price

house <- read_csv("./data/house_price/CA_house_price.csv")

## create house key

house_key <- house %>%
  select(City,County) %>%
  count(City, County)

write_csv(house_key, "./data/house_price/house_key.csv")

house_key <- read_csv("./data/house_price/house_key.csv")

house_keyed <- merge(x=house_key,y=house, by=c("City","County"), all=TRUE)

house_invalid_city <- house_keyed %>%
  filter(is.na(city_key))

write_csv(house_invalid_city, "./data/house_price/house_invalid_city.csv")

house_clean <- house_keyed %>%
  filter(!is.na(city_key))

count_dupe <- house_clean %>%
  count(city_key,county_key,Year,Month) %>%
  rename(zipcode_records = n)

write_csv(count_dupe, "./data/house_price/house_zipcode_records.csv")

house_clean_v2 <- house_clean %>%
  select(city_key,county_key,Zip_code,Month,Year,ZHVI) %>%
  rename(city = city_key, county = county_key, month = Month, year = Year, zipcode = Zip_code,
         house_price = ZHVI)
  
write_csv(house_clean_v2, "./data/house_price/h_price_clean_by_zipcode.csv")

house_clean_v3 <- house_clean_v2 %>%
  group_by(city,county,month,year) %>%
  summarise(house_price = mean(house_price))

house_clean_v4 <- house_clean_v3 %>%
  filter(!is.na(house_price))

write_csv(house_clean_v4, "./data/house_price/h_price_clean.csv")

### cleaning house month data (from string to int)
h_price_clean <- read_csv("data/house_price/h_price_clean.csv")

