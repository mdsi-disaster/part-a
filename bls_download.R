# install.packages("blsAPI")

library(readr)
library(dplyr)
library(blsAPI)

api_key = "REGISTER ON BLS.GOV"

# Download area list and save copy as CSV
areas <- read_delim("https://download.bls.gov/pub/time.series/la/la.area",delim="\t")
write_csv(areas, "./areas.csv")
# areas <- read_csv("./areas.csv")

# City Breakdown
city <- areas %>% 
  filter(area_type_code == "G" & grepl(", CA", area_text))  

# Other levels available: (no official documentation) #####
# State:        filter(area_type_code == "A") 
# Metro (stat): filter(area_type_code == "B" & grepl("CA Metropolitan Statistical Area", area_text))
# Metro:        filter(area_type_code == "C" & grepl("CA Metropolitan Division", area_text))
# Micro (stat): filter(area_type_code == "D" & grepl("CA Micropolitan Statistical Area", area_text))
# Combined:     filter(area_type_code == "E" & grepl("CA Combined Statistical Area", area_text))
# County:       filter(area_type_code == "F" & grepl(", CA", area_text))
# City:         filter(area_type_code == "G" & grepl(", CA", area_text))

# Extract codes for blsAPI ("LAU" for unemployment rate)
codes <- city %>% 
  mutate(search_code = as.character(glue("LAU{area_code}03"))) %>% 
  select(search_code) %>% 
  as.list()

# Note: Limits per query are Series = 50, Years = 20

# Split geographic list into sublists of max 50 for queries 
split_n <- if(lengths(codes) < 50) lengths(codes) else 50
code_lists <- split(codes[[1]], ceiling(seq_along(codes[[1]])/split_n))

# Split up payloads due to limits on year
payload_old_func <- function(x) {
  list(
    seriesid = x,
    startyear = 1990,
    endyear = 2011, 
    registrationKey = api_key)
}

payload_recent_func <- function(x) {
  list(
    seriesid = x,
    startyear = 2011,
    endyear = 2021, 
    registrationKey = api_key)
}

# Create list of all payload combinations 
old_list <- lapply(code_lists, payload_old_func)
recent_list <- lapply(code_lists, payload_recent_func)

payload_list <- c(old_list, recent_list) 

# Download data 
bls_download <- function(x) {
  blsAPI(x, 2, TRUE)  
}

data <- lapply(payload_list, bls_download)

# Convert list of df's
df <- bind_rows(data)

# Convert Series ID to area code
df$area_code <- substr(df$seriesID, 4, 18)

# Merge Area Names 
df <- left_join(df, select(city, area_code, area_text), by = "area_code")

head(df)

write_csv(df, "./unemployment.csv")
