### Earthquake impact on house prices ### 
### Linear Regression model ###
### Basic model without replacing NAs or excluding outliers ###

# Import libraries
library(tidyverse)
library(readr)
library(funModeling)
library(caret)
library(ggcorrplot)
library(DataExplorer)
library(GGally)
library(hydroGOF)

# Set working directory
setwd("C:/Users/sandr/projects/part-a/data")

# load dataset
earthquakes <- read_csv("merged_data_by_city_clean.csv")
summary(earthquakes)

# check for missing values
plot_missing(earthquakes)

# check for multicollinearity using corrplot
library(corrplot)
ggcorr(earthquakes, method = 'pairwise')

# select some variables only - # year,population,crime_total,crime_index,quakes_minor,quakes_moderate,quakes_severe,gdp_change,house_price,fault_length

earthquakes_selected <- earthquakes[, c(1:2,5,16:21,23,37)]

df_status(earthquakes_selected)
earthquakes_selected <- na.omit(earthquakes_selected)

# Set seed for reproducibility 
set.seed(48) 

# Split train/test at random
train_binary = createDataPartition(y = earthquakes_selected$county, p = 0.7, list = F)
training_binary = earthquakes_selected[train_binary, ]
testing_binary = earthquakes_selected[-train_binary, ]

# simple multiple linear regression model # adjusted r square 0.58
slm <- lm(formula =  house_price ~ ., data = training_binary)

# Plot the model information
par(mfrow = c(2, 2)) 
plot(slm)  

# predict on testset
prediction <- predict.lm(slm, testing_binary, type="response", interval="confidence")

# add column with predicted value (fit) and error (difference between actual and predicted)
testing_binary$fit <- as.data.frame(prediction)$fit
testing_binary$error <- testing_binary$house_price-testing_binary$fit

# plot actuals vs predicted
# Compare actuals vs predicted on testset

# calculate RMSE
rmse(testing_binary$house_price, testing_binary$fit)

# calculate RMSE after grouping by county

predicted_actuals <- testing_binary %>% 
  group_by(county) %>% 
  summarise(actual = mean(house_price), 
            predicted = mean(fit))

# RMSE of 33797.5 - on average the predicted price is more or less $33797.5 from the actual price
rmse(predicted_actuals$actual, predicted_actuals$predicted)
