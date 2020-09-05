library(tidyverse)
library(here)
crime <- read.csv(here("data","crimedata","clean_cacrime.csv"))
gdp <- read.csv(here("data","gdp","clean_ca_gdp_yoy_change.csv"),fileEncoding="UTF-8-BOM")
house_price <- read.csv(here("data","house_price","ca_house_price.csv"))
property_type <- read.csv(here("data","property_type","property_type.csv"))

names(property_type) <- tolower(names(property_type))
names(house_price) <- tolower(names(house_price))

house_price %>% count(city,year,month)

house_price_gdp <- gdp %>% right_join(house_price, by = c("year")) 
dim(house_price)
dim(house_price_gdp)

house_price_gdp_type <- property_type %>% right_join(house_price_gdp,by = c('year','county'))

house_price %>% count(city,year,month)