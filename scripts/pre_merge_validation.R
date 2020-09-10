

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

crime_clean <- read_csv("./data/crimedata/clean_cacrime.csv")

crime_keyed <- merge(x=city_key, y=crime_clean, by=c("city","county"), all.y=TRUE)

count_keys <- crime_clean %>%
  count(city,county,year)

## earthquake data
city_key <- read_csv("./data/keys/city_key.csv")
county_key <- read_csv("./data/keys/county_key.csv")

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


