# World Bank - Climate API

# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)
library(tidyr)


# Format: http://climatedataapi.worldbank.org/climateweb/rest/v1/country/type/var/start/end/ISO3[.ext]
base <- "http://climatedataapi.worldbank.org/climateweb/rest/v1/country"

# Monthly average. Can change to yearly or change
type <- "mavg"

# tas = temperature, pr = percipitation
var <- "tas"  

# start/end are set pairs
start <- 1980
end <- 1999

# ISO country code
ISO3 <- "USA"
ext <- "JSON"
  
call <- paste(base, type, var, start, end, ISO3, sep = "/")

# Get request
get_data <- GET(call)

# Extract content
get_data_text <- content(get_data, "text")

# Flatten data
get_data_json <- fromJSON(get_data_text, flatten = TRUE)

# Dataframe format
df <- as.data.frame(get_data_json)
df <- unnest(df, "monthVals")

head(df)
