library(dplyr)
library(readr)
library(zoo)

df <- read_csv("./data/mn_model_df.csv")

# Create date variable and drop unused columns
model_df <- df %>% 
  mutate(date = as.yearqtr(paste0(year, quarter), '%Y %q')) %>% 
  select(crime_murder:crime_arson, property_house:property_high_rise_per, quakes_minor:population)

# Train test split
train_set <- model_df %>% 
  filter(date <= "2017 Q4") 

test_set <- model_df %>% 
  filter(date > "2017 Q4") 

# Model
lm.fit <- lm(log(house_price)~.-income+log(income)-population+log(population), data = train_set)
summary(lm.fit)
plot(lm.fit)


# car::vif(lm.fit)

# Prediction
test_set$prediction <- predict(lm.fit, test_set)

# RMSE 
p <- length(attr(summary(lm.fit)$terms, 'term.labels'))
n <- nrow(test_set)
y <- test_set$house_price
y_predict <- exp(test_set$prediction)

RSS <- sum((y - y_predict)^2)
MSE <- RSS / (n - p - 1)
RMSE <- sqrt(MSE)
RMSE
