#install.packages(c("httr", "jsonlite"))

# libraries for API requests (RESTful) and JSON parsing.
library(httr)
library(jsonlite)

## API request params
key <- "&key=AIzaSyCBDeto0T7CnoOAcxog8qKtoJE2WcOl1qE"
address <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="
result_type <- "&result_type=locality"

## request lat,long details
lat <- 40.714224
long <- -73.961452
latlong <- paste(lat,long,sep=",")

## combine all param and values into a request body
request <- paste(address,latlong,result_type,key,sep="")

## runs the request from Google Reverse Geocoding API
res <- GET(request)

## print summary of result
res
