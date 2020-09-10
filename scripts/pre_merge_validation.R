

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


## Finish homeless cleaning