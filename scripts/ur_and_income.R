library(blsAPI)
library(dplyr)
library(httr)
library(jsonlite)
library(readr)

##### API Keys #####
# Register keys at:
# https://data.bls.gov/registrationEngine/
# https://api.census.gov/data/key_signup.html
# https://apps.bea.gov/API/signup/index.cfm 

bls_key = "c3c3770818ad4c8cbdbeb0503b308d8d"
cen_key <- "3379365abe31dbc9f567a9914ef913a33935ccde"
bea_key <- "47EC1F03-EC80-4F0E-89C3-4A9640317E53"


##### Unemployment Rate ##### 
# From US Bureau of Labor Statistics 

# Download BLS area list and save copy as CSV
areas <- read_delim("https://download.bls.gov/pub/time.series/la/la.area", delim="\t")
write_csv(areas, "./areas.csv")
# areas <- read_csv("./areas.csv")

# Extract Californian cities
city <- areas %>% 
  filter(area_type_code == "G" & grepl(", CA", area_text))  

# Convert area codes into data series codes for blsAPI
codes <- city %>% 
  mutate(search_code = paste0("LAU", area_code, "03")) %>% 
  select(search_code) %>% 
  as.list()

# Split geographic list into sublists as blsAPI has a limit of 50 per query
split_n <- if(lengths(codes) < 50) lengths(codes) else 50
code_lists <- split(codes[[1]], ceiling(seq_along(codes[[1]])/split_n))

# Create Payload to pass parameters into blsAPI. Limit of 20 years per query, 'endyear' not inclusive. 
payload_old <- function(x) list(seriesid = x, startyear = 1990, endyear = 2011, registrationKey = bls_key)
payload_new <- function(x) list(seriesid = x, startyear = 2011, endyear = 2021, registrationKey = bls_key)

# Create payloads for multiple API calls due to limits 
pay_list <- c(lapply(code_lists, payload_old), 
              lapply(code_lists, payload_new))

# BLS call as function to pass payload list through
bls_download <- function(x) {
  
  # More data available through Version 2 of API 
  blsAPI(payload = x, api_version = 2, return_data_frame = TRUE)  
}

# Download Data and convert to one dataframe 
data <- lapply(pay_list, bls_download)
employment <- bind_rows(data)

# Revert Series ID to area code and merge area names 
employment$area_code <- substr(employment$seriesID, 4, 18)
employment <- left_join(employment, select(city, area_code, area_text), by = "area_code")

# Clean data for merging
employment <- employment %>% 
  mutate(area_name = gsub(", CA", "", area_text), month = gsub("(M0?)", "", period)) %>% 
  rename(unemployment_rate = value)

# Save copy 
write_csv(employment, "./data/unemployment.csv")


##### Quarterly Earnings ##### 
# From US Census Bureau 

# Create base url. 
# qwi = Quarterly Workforce Indicators 
# rh = Race, sa = Age, se = Education. This level is optional depending on query parameters below. 
base_cen <- "https://api.census.gov/data/"
base_set <- "timeseries/qwi/rh" 

# Set up parameters
cen_indicator = "EarnS" # Full Quarter Employment (Stable): Average Monthly Earnings
cen_for <- I("county:*")
cen_in <- I("state:06")
cen_time <- "from 1991-Q4 to 2019-Q3"

cen_query <- list('get' = cen_indicator, 
                  'for' = cen_for, 
                  'in' = cen_in, 
                  'time' = cen_time, 
                  'key' = cen_key) 

# API call 
res <- GET(paste0(base_cen, base_set), 
           query = cen_query)

# Convert to dataframe 
earnings <- res$content %>%
  rawToChar() %>% 
  fromJSON() %>% 
  as.data.frame()

# Clean data
names(earnings) <- earnings[1, ]
earnings <- earnings[-1, ]

earnings <- earnings %>% 
  mutate(year = gsub("-Q[1-4]", "", time), quarter = gsub("([0-9]{4})-Q", "", time))

# Add county name
counties <- areas %>% 
  filter(area_type_code == "F" & grepl(", CA", area_text)) %>% 
  mutate(county = substr(area_code, 5, 7), county_name = gsub(", CA", "", area_text)) %>% 
  rename(earnings = EarnS)

earnings <- left_join(earnings, 
                      select(counties, c("county", "county_name")),
                      by = "county")

# Save copy
write_csv(earnings, "./data/cen_earnings.csv")


##### Personal Income per capita ##### 
# From Bureau of Economic Analysis (BEA)
base_bea <- "https://apps.bea.gov/api/data/" 

# Set up parameters 
bea_data <- "Regional"
bea_table <- "CAINC1" # Per Capita Personal Income (County Annual Income)
bea_year <- "ALL" # if not specified, "LAST5" is used 
bea_geo <- "CA"

bea_query <- list('UserID' = bea_key, 
                  'method' = "GetData",
                  'datasetname' = bea_data,
                  'TableName' = bea_table,
                  'LineCode' = 3,
                  'Year' = bea_year, 
                  'GeoFips' = bea_geo, 
                  'ResultFormat' = "json")

# API call
res_bea <- GET(base_bea, query = bea_query) 

# Convert to dataframe
bea_json <- res_bea$content %>%
  rawToChar() %>% 
  fromJSON()

income <- bea_json$BEAAPI$Results$Data %>% 
  as.data.frame()

# Clean data
income <- income %>% 
  filter(GeoFips != "06000") %>% 
  rename(year = TimePeriod, income_per_cap = DataValue) %>% 
  mutate(county_name = gsub(", CA", "", GeoName)) 

# Save Copy 
write_csv(income, "./data/bea_income.csv")


