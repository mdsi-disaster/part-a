library(dplyr)
library(readr)
library(tidyr)
library(zoo)

# Set variables
data_file <- "./data/merged_data_raw.csv"

# Load data
merged_df <- read_csv(data_file)


# Aggregate city data per month 
transform_df <- merged_df %>% 
  mutate(across(quakes_minor:quakes_severe, ~replace_na(.x, 0))) %>% 
  group_by(county, year, month) %>% 
  summarise(house_price = median(house_price, na.rm = T), 
            crime_index = median(crime_index), 
            across(quakes_minor:quakes_severe, median), 
            across(income:interest, median), 
            unemployment_rate = median(unemployment_rate, na.rm = T), 
            across(quarter:fault_score, last)) %>% 
  ungroup()


# Aggregate per quarter 
quarter_df <- transform_df %>% 
  group_by(year, quarter, county) %>%
  summarise(house_price = last(house_price), 
            crime_index = last(crime_index),
            across(quakes_minor:quakes_severe, sum),
            across(income:fault_score, last)) %>% 
  ungroup()


# Remove missing housing data (small populations)
filtered_df <- quarter_df %>% 
  filter(!(county %in% c("alpine", "calaveras", "mariposa", "trinity", "tuolumne"))) 


# Assume median where missing 
filtered_df <- filtered_df %>% 
  group_by(year, quarter) %>% 
  mutate(crime_index = ifelse(is.na(crime_index), median(crime_index, na.rm = TRUE), crime_index), 
         unemployment_rate = ifelse(is.na(unemployment_rate), median(unemployment_rate, na.rm = TRUE), unemployment_rate)) %>% 
  ungroup() %>% 
  na.omit()


# Lag prices one quarter - in line with prior research, avoid forward looking bias. 
lag_df <- filtered_df %>% 
  arrange(county, year, quarter) %>%
  group_by(county) %>% 
  mutate(house_price = lead(house_price)) %>% 
  ungroup() %>% 
  filter(!is.na(house_price))


# Create date variable and drop unused columns from final model
model_df <- lag_df %>% 
  mutate(date = as.yearqtr(paste0(year, quarter), '%Y %q')) %>% 
  select(date, house_price, crime_index, quakes_minor:population, fault_score) %>% 
  na.omit()


# Train/Test Split - sequential for time series data, not random 
cut_off <- "2017 Q4"

train_set <- model_df %>% 
  filter(date <= cut_off) 

test_set <- model_df %>% 
  filter(date > cut_off) 


### Regression 1: No transformations

lm.all <- lm(house_price~., data = train_set)
summary(lm.all)
par(mfrow = c(2, 2)) 
plot(lm.all)

# Predictions
test_set$prediction <- predict(lm.all, test_set)

# RMSE
p <- length(attr(summary(lm.all)$terms, 'term.labels'))
n <- nrow(test_set)
y <- test_set$house_price
y_predict <- test_set$prediction

RSS <- sum((y - y_predict)^2)
MSE <- RSS / (n - p - 1)
RMSE <- sqrt(MSE)
RMSE

### Regression 2: Log-transformed
lm.log <- lm(log(house_price)~.-income+log(income)-population+log(population), data = train_set)
summary(lm.log)
plot(lm.log)

# Predictions 
test_set$prediction <- predict(lm.log, test_set)

# RMSE
p <- length(attr(summary(lm.log)$terms, 'term.labels'))
n <- nrow(test_set)
y <- test_set$house_price
y_predict <- exp(test_set$prediction)

RSS <- sum((y - y_predict)^2)
MSE <- RSS / (n - p - 1)
RMSE <- sqrt(MSE)
RMSE


### Convert number of quakes into conditional variable 

# Larger earthquake test
quake_flag <- lag_df %>% 
  group_by(county) %>%
  mutate(quake = cumsum(quakes_severe+quakes_moderate) - lag(cumsum(quakes_severe+quakes_moderate), 4)) %>% 
  filter(!is.na(quake)) %>% 
  mutate(quake = ifelse(quake > 0, 1, 0)) %>% 
  ungroup() %>% 
  select(county, year, quarter, quake) 

# Remove NA's due to 12m lookback period
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

### Final Regression Model
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
