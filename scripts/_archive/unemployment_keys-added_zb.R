library(readr)
library(dplyr)


# Unemployment Data ###### 

# Load files
key_file <- read_csv("data/keys/city_key.csv")
ur_data <- read_csv("data/unemployment/unemployment.csv")

# Get unique city values and lower case
ur_data <- ur_data %>% 
  mutate(city = tolower(area_name))

ur_cities <- ur_data %>% 
  select(city) %>% 
  unique()

# Check if all cities in unemployment data are in key file 
key_cities <- select(key_file, "city")
ur_cities %in% key_cities

# View which are missing from the key file
setdiff(ur_cities, key_cities)

# Look for equivalent city names in key file
issues <- c("paso robles", "ventura", "^san francisco")

key_cities %>% 
  filter(grepl(paste(issues, collapse = "|"), city))

# Adjust names in unemployment file 
ur_data <- ur_data %>% 
  mutate(city = gsub("([a-z ]*\\()", "", city)) %>% 
  mutate(city = gsub("(\\)[a-z ]*)", " city", city)) %>% 
  mutate(city = gsub(" county/city", "", city)) %>% 
  mutate(city = gsub(" city", "", city)) #remove the word "city" from the city column 

# Re-check all data has been fixed 
ur_cities <- ur_data %>% 
  select(city) %>% 
  unique()

# Re-check no more missing data
setdiff(ur_cities, key_cities)


# Merge with key
ur_clean <- ur_data %>% 
  select(year, month, city, unemployment_rate)

merged_data <- key_file %>% 
  left_join(ur_clean, by = "city")

# write unemployment file with merged keys

write_csv(merged_data, "data/unemployment/unemployment_clean_keys.csv")
