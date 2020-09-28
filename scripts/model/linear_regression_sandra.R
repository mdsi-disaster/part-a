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
library(corrplot)

# Set working directory
setwd("C:/Users/sandr/projects/part-a/data")

# load dataset
earthquakes <- read_csv("merged_data_by_city_clean.csv")
summary(earthquakes)

# check for missing values
plot_missing(earthquakes)

# check for multicollinearity using corrplot
ggcorr(earthquakes, method = 'pairwise')

# select some variables only - # year,population,crime_total,crime_index,quakes_minor,quakes_moderate,quakes_severe,gdp_change,house_price,fault_length

earthquakes_selected <- earthquakes[, c(1:2,5,16:21,23,37)]

# Remove NAs
earthquakes_selected <- na.omit(earthquakes_selected)

# Set seed for reproducibility 
set.seed(48) 

# Split train/test at random
train_split= createDataPartition(y = earthquakes_selected$county, p = 0.7, list = F)
training = earthquakes_selected[train_split, ]
testing = earthquakes_selected[-train_split, ]

# simple multiple linear regression model # adjusted r square 0.58
slm <- lm(formula =  house_price ~ ., data = training)

# Plot the model information
par(mfrow = c(2, 2)) 
plot(slm)  

# predict on testset
prediction <- predict.lm(slm, testing, type="response", interval="confidence")

# add column with predicted value (fit) and error (difference between actual and predicted)
testing$fit <- as.data.frame(prediction)$fit
testing$error <- testing$house_price-testing$fit

# calculate RMSE
rmse(testing$house_price, testing$fit)

# calculate RMSE after grouping by county

predicted_actuals <- testing %>% 
  group_by(county) %>% 
  summarise(actual = mean(house_price), 
            predicted = mean(fit))

# RMSE of 33797.5 - on average the predicted price is more or less $33797.5 from the actual price
rmse(predicted_actuals$actual, predicted_actuals$predicted)
