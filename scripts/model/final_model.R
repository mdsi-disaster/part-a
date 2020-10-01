library(dplyr)
library(readr)
library(tidyr)
library(zoo)

# Set variables
path <- "C:/Users/mnelo/Documents/STDS/A2/"
data_file <- "part_b/merged_data_raw.csv"
inc_file <- "income_clean.csv"
ur_file <- "unemployment_clean.csv"
ir_file <- "interest_clean.csv"
fault_file <- "faults_scored.csv"
pop_file <- "population_redo.csv"


# Load data
df <- read_csv(paste0(path, data_file))
income <- read_csv(paste0(path, inc_file))
unemployment <- read_csv(paste0(path, ur_file))
interest <- read_csv(paste0(path, ir_file))
population <- read_csv(paste0(path, pop_file))

# Clean data
df <- df %>% 
  filter(year >= 2010, year <= 2019) %>% 
  select(house_price, county:city, crime_index, quakes_minor:gdp_change) %>% 
  mutate(quarter = ceiling(month / 3))

unemployment <- unemployment %>% 
  rename(city = area_name) %>% 
  select(year, month, city, unemployment_rate) %>% 
  mutate(city = tolower(city))

#city_key <- read_csv(paste0(path, "part_b/city_key.csv"))
#setdiff(unemployment$city, city_key$city)

unemployment <- unemployment %>% 
  mutate(city = gsub("([a-z ]*\\()", "", city)) %>% 
  mutate(city = gsub("(\\)[a-z ]*)", " city", city)) %>% 
  mutate(city = gsub(" county/city", "", city)) %>% 
  mutate(city = gsub(" city", "", city))

interest <- interest %>% 
  group_by(year, month) %>% 
  summarise(interest = mean(Value)) %>% 
  ungroup()

# Merge 
merged_df <- df %>% 
  left_join(unemployment, by = c("year", "month", "city")) %>% 
  left_join(income, by = c("year", "quarter", "county")) %>% 
  left_join(interest, by = c("year", "month"))


# Aggregate city data per month 
transform_df <- merged_df %>% 
  mutate(across(quakes_minor:quakes_severe, ~replace_na(.x, 0))) %>% 
  group_by(county, year, month) %>% 
  summarise(house_price = median(house_price, na.rm = T), 
            crime_index = median(crime_index), 
            across(quakes_minor:quakes_severe, median),
            across(income:interest, median), 
            unemployment_rate = median(unemployment_rate, na.rm = T), 
            quarter = last(quarter)) %>% 
  ungroup()


# Aggregate per quarter 
quarter_df <- transform_df %>% 
  group_by(year, quarter, county) %>%
  summarise(house_price = last(house_price), 
            crime_index = last(crime_index),
            across(quakes_minor:quakes_severe, sum),
            across(income:unemployment_rate, last)) %>% 
  ungroup()

# Remove missing housing data
filtered_df <- quarter_df %>% 
  filter(!(county %in% c("alpine", "calaveras", "mariposa", "trinity", "tuolumne"))) 

# Replace population data (too many missing items on previous set)
filtered_df <- filtered_df %>% 
  left_join(population, by = c('county', 'year'))

# Assume median where missing 
filtered_df <- filtered_df %>% 
  group_by(year, quarter) %>% 
  mutate(crime_index = ifelse(is.na(crime_index), median(crime_index, na.rm = TRUE), crime_index), 
         unemployment_rate = ifelse(is.na(unemployment_rate), median(unemployment_rate, na.rm = TRUE), unemployment_rate)) %>% 
  ungroup()

# Combine fault scores 
fault_df <- read_csv(paste0(path, fault_file))

filtered_df <- filtered_df %>% 
  left_join(select(fault_df, c("county", "fault_score")), by = "county")

# Check for missing data
filtered_df %>% 
  filter(is.na(fault_score)) %>% 
  select(county) %>% 
  unique()

# Lag prices 
lag_df <- filtered_df %>% 
  arrange(county, year, quarter) %>%
  group_by(county) %>% 
  mutate(house_price = lead(house_price)) %>% 
  ungroup() %>% 
  filter(!is.na(house_price))

# Create date variable and drop unused columns
model_df <- lag_df %>% 
  mutate(date = as.yearqtr(paste0(year, quarter), '%Y %q')) %>% 
  select(date, house_price, crime_index, quakes_minor:population, fault_score) %>% 
  na.omit()

# Train/Test Split 
cut_off <- "2017 Q4"

train_set <- model_df %>% 
  filter(date <= cut_off) 

test_set <- model_df %>% 
  filter(date > cut_off) 


# Regression 
# Regular
lm.all <- lm(house_price~., data = train_set)
summary(lm.all)

par(mfrow = c(2, 2)) 
plot(lm.all)

# Log-transform
lm.log <- lm(log(house_price)~.-income+log(income)-population+log(population), data = train_set)
summary(lm.log)

plot(lm.log)


# car::vif(lm.fit)

# Predictions 
test_set$prediction <- predict(lm.log, test_set)

p <- length(attr(summary(lm.log)$terms, 'term.labels'))
n <- nrow(test_set)
y <- test_set$house_price
y_predict <- exp(test_set$prediction)

RSS <- sum((y - y_predict)^2)
MSE <- RSS / (n - p - 1)
RMSE <- sqrt(MSE)
RMSE



# Large earthquake test
quake_flag <- lag_df %>% 
  group_by(county) %>%
  mutate(quake = cumsum(quakes_severe+quakes_moderate) - lag(cumsum(quakes_severe+quakes_moderate), 4)) %>% 
  filter(!is.na(quake)) %>% 
  mutate(quake = ifelse(quake > 0, 1, 0)) %>% 
  ungroup() %>% 
  select(county, year, quarter, quake) 

quake_df <- lag_df %>% 
  left_join(quake_flag, by = c("county", "year", "quarter")) %>% 
  filter(year != 2010)


# Create date variable and drop unused columns
model_quake <- quake_df %>% 
    mutate(date = as.yearqtr(paste0(year, quarter), '%Y %q')) %>% 
    select(date, house_price, crime_index, income:population, fault_score, quake) %>% 
    na.omit()
  
# Train/Test Split 
cut_off <- "2017 Q4"
  
train_quake <- model_quake %>% 
    filter(date <= cut_off) 
  
test_quake <- model_quake %>% 
    filter(date > cut_off) 
  
# Model
lm.quake <- lm(log(house_price)~.-income+log(income)-population+log(population), data = train_quake)
summary(lm.quake)

plot(lm.quake)

# Predictions 
test_quake$prediction <- predict(lm.quake, test_quake)

p <- length(attr(summary(lm.quake)$terms, 'term.labels'))
n <- nrow(test_quake)
y <- test_quake$house_price
y_predict <- exp(test_quake$prediction)

RSS <- sum((y - y_predict)^2)
MSE <- RSS / (n - p - 1)
RMSE <- sqrt(MSE)
RMSE
