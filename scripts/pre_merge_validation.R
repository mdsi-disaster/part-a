

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