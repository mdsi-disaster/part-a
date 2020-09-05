########## script info ----
# Reverse geocode and frequency data clean:
# This script calculates the city and county dimensions by using the latitude and
# longitude values provided and processing them through Google's Geocoding API.
#
# Earthquake frequences categorised and then summed by month to further clean the
# earthquake data set for use.
#
# Contributors: Ivan
#

# packages setup ----

#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

#API request params
key <- "&key=AIzaSyCBDeto0T7CnoOAcxog8qKtoJE2WcOl1qE"
address <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="
#filter the reverse geo-code to go down only to city level detail (so as not to return stree level detail)
result_type <- "&result_type=political"
result_type_street <- "&result_type=street_address"

# reading input data ----

#reading earthquakes data
earthquakes <- read_csv(here("data/earthquake","earthquakes.csv"))

#mutate region dimension into table
earthquake_with_region <- earthquakes %>%
  mutate(city = "", county = "", state = "")
  
glimpse(earthquake_with_region)

#identified_earthquakes$city[1] = "test"


## --- iterate through earthquakes

for (row in seq_len(nrow(earthquakes))) { #for (row in c(190)) {
  lat <- earthquakes$latitude[row]
  long <- earthquakes$longitude[row]
  latlong <- paste(lat,long,sep=",")
  
  # prepare API request, execute API call and load data
  request <- paste(address,latlong,result_type,key,sep="")
  result <- GET(request)
  data <- fromJSON(rawToChar(result$content))
  
  # check if the reverse lookup returned any results (no result = skip)
  if (length(data$results) != 0) {
    # different formatted_address results return different levels of region detail.
    # parse returned results through regex to return the first value before a coma
    
    fa_results = length(data$results$formatted_address)
    if (fa_results-1 > 0) {
      state <- gsub(",.*$", "", data$results$formatted_address[fa_results-1])  
    } else {
      state <- ""
    }
    if (fa_results-2 > 0) {
      county <- gsub(",.*$", "", data$results$formatted_address[fa_results-2])  
    } else {
      county <- ""
    }
    if (fa_results-3 > 0) {
      city <- gsub(",.*$", "", data$results$formatted_address[fa_results-3])  
    } else {
      city <- ""
    }
    
    # input the results back into the dataframe
    earthquake_with_region$city[row] <- city
    earthquake_with_region$county[row] <- county
    earthquake_with_region$state[row] <- state
    
    print(paste(row,city,county,state,sep=","))
  }
}

glimpse(earthquake_with_region)

write_csv(earthquake_with_region, "data/earthquake/earthquakes_with_region.csv")

# count earthquake freq by magnitude
# grp1 = 1 - 5.5, grp2 = 5.6 - 6.5, grp3 = 6.6 - 7.0, grp4 = 7.1 - 7.9
earthquake_clean <- earthquake_with_region %>%
  select(time,mag,city,county,state) %>%
  filter (state == "California")

glimpse(earthquake_clean)

earthquake_clean <- earthquake_clean %>%
  mutate(quakes_minor = 0, quakes_low = 0, quakes_moderate = 0, quakes_severe = 0)

for (row in seq_len(nrow(earthquake_clean))) {
  if (earthquake_clean$mag[row] <= 5.5) {
    earthquake_clean$quakes_minor <- 1
  } else if (earthquake_clean$mag[row] <= 6.5) {
    earthquake_clean$quakes_low <- 1
  } else if (earthquake_clean$mag[row] <= 7) {
    earthquake_clean$quakes_moderate <- 1
  } else if (earthquake_clean$mag[row] <= 7.9) {
    earthquake_clean$quakes_severe <- 1
  }
}

# final clean of results: ----
# - remove un-needed columns
# - group results by city,county and month,year
earthquake_clean_v2 <- earthquake_clean %>%
  mutate(month = month(earthquake_clean$time), year = year(earthquake_clean$time)) %>%
  select(-mag,-time, -state) %>%
  group_by(city,county,month,year) %>%
  summarise(quakes_minor = sum(quakes_minor),
            quakes_low = sum(quakes_low),
            quakes_moderate = sum(quakes_moderate),
            quakes_severe = sum(quakes_severe))

glimpse(earthquake_clean)

write_csv(earthquake_with_region, "data/earthquake/earthquakes_clean.csv")

# review results ----
earthquake_with_region %>% count(state)
earthquake_with_region %>% 
  filter (state == "California") %>%
  count(county)

earthquake_with_region %>% count(city)




# ------ TEST SCRIPTING ------

# looking at formatted addresses
data$results$formatted_address
length(data$results$formatted_address)

## request lat,long details
lat <- 37.423021
long <- -122.083739
lat <- earthquakes$latitude[18902]
long <- earthquakes$longitude[18902]
latlong <- paste(lat,long,sep=",")

## combine all param and values into a request body
request <- paste(address,latlong,result_type,key,sep="")
request <- paste(address,latlong,result_type_street,key,sep="")
request

## runs the request from Google Reverse Geocoding API
res <- GET(request)

## print summary of result
res

## extract data off the returned json
data <- fromJSON(rawToChar(res$content))

## lists the structure of the JSON
names(data)

length(data$results) == 0

## if length(data$results) == 0 then skip (no address found).

### exported the formatted address from the JSON result
length(data$results$formatted_address)
data$results$formatted_address[1]
data$results$formatted_address[2]

## returns first value in string separated by commas.
gsub(",.*$", "", data$results$formatted_address[1])
