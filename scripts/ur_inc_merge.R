

library(readr)
library(dplyr)


# Unemployment Data ###### 

# Load files
key_file <- read_csv("./city_key.csv")
ur_data <- read_csv("./unemployment.csv")

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
  mutate(city = gsub(" county/city", "", city))


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



# Income Data ###### 

# Load files
key_file <- read_csv("./county_key.csv")
inc_data <- read_csv("./cen_earnings.csv")

# Clean income file 
inc_data <- inc_data %>% 
  select(year, quarter, county_name, EarnS) %>% 
  rename(county = county_name, income = EarnS) %>% 
  mutate(county = gsub(" county", "", tolower(county)))

# Get unique county names from income data
inc_counties <- inc_data %>% 
  select(county) %>% 
  unique()

# Check if lists are the same
inc_counties %in% key_file

# View which are missing from the key file
setdiff(inc_counties, key_file)

# View which are missing from the income file
setdiff(key_file, inc_counties)

# Fix differences 
inc_data <- inc_data %>% 
  mutate(county = gsub("/city", "", county))


# Create quarter variable
merged_data <- merged_data %>% 
  mutate(quarter = case_when(month %in% c(1, 2, 3) ~ 1, 
                             month %in% c(4, 5, 6) ~ 2, 
                             month %in% c(7, 8, 9) ~ 3, 
                             month %in% c(10, 11, 12) ~ 4))

# Merge income data
merged_data <- merged_data %>% 
  left_join(inc_data, by=c("county", "year", "quarter"))

# Save Copy
write_csv(merged_data, "./merged_data.csv")
