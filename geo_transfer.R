library(tidyverse)
library(here)
library(ggmap)
register_google(key='AIzaSyDWDTCdB57wGf4E4xeOQ6rQ8SbIRMgR6oY')

#Using zip code or City name or other address information get address 
#https://developers.google.com/maps/documentation/geocoding/overview
result = geocode(c("Hawthorne","San Francisco", #city name
                   "94109","94501","95608",#zip code
                   "1600 Amphitheatre Parkway, Mountain View, CA"),output = "more") # specific address
result$address
address <- result %>% separate(col = address, into=c("City","County","Country"),sep = ",")
#reverse: using longtitude and latitude to get address
revgeocode(c(-118.3526,
             33.91640))


#Option 2 Can get County Information
data <- read.csv(here('data','house_price','ca_house_raw.csv'))
zipinfo <- data %>% rename(ZipCode = RegionName) %>% select(ZipCode,City,Metro,CountyName)
#write.csv(zipinfo, here('data','house_price','zipinfo.csv'))

searchZipCode <- function(zipcode){
  zipinfo %>%filter(ZipCode==zipcode)
}

result = searchZipCode(zipcode='94109')

