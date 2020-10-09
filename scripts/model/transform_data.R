library(dplyr)
library(readr)
library(tidyr)
library(zoo)

# Set data paths - somes files not included in merged set
data_file <- "./data/merged_data_raw.csv"
inc_file <- "./data/income/income_clean.csv"
ur_file <- "./unemployment/unemployment_clean.csv"
ir_file <- "./interest/interest_clean.csv"

# Load data
df <- read_csv(data_file)
income <- read_csv(inc_file)
unemployment <- read_csv(ur_file)
interest <- read_csv(ir_file)

# Filter data range and set quarter
df <- df %>% 
  filter(year >= 2000, year <= 2019) %>% 
  mutate(quarter = ceiling(month / 3))

# Fix city names
unemployment <- unemployment %>% 
  rename(city = area_name) %>% 
  select(year, month, city, unemployment_rate) %>% 
  mutate(city = tolower(city))

# Check vs key file for merge
#city_key <- read_csv("./keys/city_key.csv")
#setdiff(unemployment$city, city_key$city)

unemployment <- unemployment %>% 
  mutate(city = gsub("([a-z ]*\\()", "", city)) %>% 
  mutate(city = gsub("(\\)[a-z ]*)", " city", city)) %>% 
  mutate(city = gsub(" county/city", "", city))
unemployment <- unemployment %>% 
  mutate(city = gsub(" city", "", city))

# Daily interest rates - take monthly mean
interest <- interest %>% 
  group_by(year, month) %>% 
  summarise(interest = mean(Value)) %>% 
  ungroup()
  
# Merge all files
merged_df <- df %>% 
  select(-income) %>% 
  mutate(quarter = ceiling(month / 3))

merged_df <- merged_df %>% 
  left_join(unemployment, by = c("year", "month", "city")) %>% 
  left_join(income, by = c("year", "quarter", "county")) %>% 
  left_join(interest, by = c("year", "month"))

merged_df %>%  
  arrange(county, city, year, month) %>%
  select(county, city, year, month, unemployment_rate, interest, quakes_minor:quakes_severe)

# Roll up to county level at monthly level
transform_df <- merged_df %>% 
  mutate(across(quakes_minor:quakes_severe, ~replace_na(.x, 0))) %>% 
  filter(year > 2010) %>% 
  group_by(county, year, month) %>% 
  summarise(house_price = median(house_price, na.rm = T), 
            across(population:crime_total, sum), 
            crime_index = median(crime_index), 
            across(property_total:property_high_rise_per, median),
            across(fault_num:fault_length, median),
            homeless_count = median(homeless_count),
            across(quakes_minor:quakes_severe, sum),
            income = median(income), 
            interest = median(interest), 
            unemployment_rate = median(unemployment_rate, na.rm = T)) %>% 
  ungroup()

# Fill missing quake numbers with zero
transform_df <- transform_df %>% 
  mutate(across(quakes_minor:quakes_severe, ~replace_na(.x, 0))) 

# Roll up to quarterly frequency
quarter_df <- transform_df %>% 
  mutate(quarter = ceiling(month / 3)) %>%
  group_by(year, quarter, county) %>%
  summarise(across(house_price:homeless_count, last),
            across(quakes_minor:quakes_severe, sum),
            across(income:unemployment_rate, last)) %>% 
  ungroup()


# Remove missing housing data
quarter_df <- quarter_df %>% 
  filter(!(county %in% c("alpine", "calaveras", "mariposa", "trinity", "tuolumne"))) 

# Drop existing population - too many missing amounts
quarter_df <- quarter_df %>% 
  select(-population)

# new population - Note: assumes 2010 - 2013 is equal. Taken from Census data (see /scripts folder). Data not available for these years. 
pop_df <- read_csv("./data/population/population_redo.csv")

quarter_df <- quarter_df %>% 
  left_join(pop_df, by = c('county', 'year'))

# Assume fault values are zero if missing 
quarter_df <- quarter_df %>% 
  mutate(across(fault_num:fault_length, ~replace_na(.x, 0))) 

# Assume median if missing
quarter_df <- quarter_df %>% 
  group_by(year, quarter) %>% 
  mutate(across(crime_murder:property_high_rise_per, ~ifelse(is.na(.), median(., na.rm=T), .))) %>% 
  ungroup()

quarter_df <- quarter_df %>% 
  group_by(year, quarter) %>% 
  mutate(across(homeless_count, ~ifelse(is.na(.), median(., na.rm=T), .))) %>% 
  ungroup()

quarter_df <- quarter_df %>% 
  group_by(year, quarter) %>% 
  mutate(across(unemployment_rate, ~ifelse(is.na(.), median(., na.rm=T), .))) %>% 
  ungroup()

# Set county as factors 
quarter_df$county <- as.factor(quarter_df$county)

# Lag prices 
lag_df <- quarter_df %>% 
  arrange(county, year, quarter) %>%
  group_by(county) %>% 
  mutate(house_price = lead(house_price)) %>% 
  ungroup() %>% 
  filter(!is.na(house_price))

# Fill forward property data that doesn't exist for 2019
lag_df <- lag_df %>% 
  group_by(county) %>% 
  mutate(across(property_total:property_high_rise_per, na.locf)) %>% 
  ungroup()


# Save dataframe
write_csv(lag_df, "./mn_model_df.csv")

# Note - also removed the following from actual model:
# -year, -quarter, -crime_violent_total, -crime_property_total, -crime_total, -property_total, -fault_num, -fault_length, -homeless_count, -county
# -c(property_house:property_high_rise_per))


