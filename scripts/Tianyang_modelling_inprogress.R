library(tidyverse)
library(here)
library(janitor)
library(DataExplorer)
library(skimr)
library(Amelia)
data <- read.csv(here('data','merged_data_by_city_clean.csv'),stringsAsFactors = TRUE)
#data1 <- read.csv(here('data/','_archive','merged','merged_data_raw_0914zb.csv'))

data$year <- as.factor(data$year)
data$month <- as.factor(data$month)

colnames(data)
#colnames(data1)
skim(data)
#skim(data1)
sapply(data, function(x) sum(is.na(x)))
#plot_str(data)
introduce(data)
plot_intro(data)

plot_missing(data)


data <- data %>% filter(!is.na(city) & !is.na(house_price))
plot_missing(data)

county_data <- data %>% group_by(county, year,month) %>% 
  summarise(population = mean(population,na.rm = TRUE))
model <- lm(formula = log(house_price) ~ year + population + county + quakes_minor
+quakes_moderate + quakes_severe ,data = data)
summary(model)
par(mfrow=c(2,2))
plot(model)


#using dataset mn_model_df.csv

data <- read.csv(here('data','mn_model_df.csv'),stringsAsFactors = TRUE)
plot_intro(data)
plot_intro(data)

plot_missing(data)

data$id <- 1:nrow(data)
train <- data%>%sample_frac(.75)
test <- anti_join(data,train,by='id')

model <- lm(formula = log(house_price)~.-id,data=train)
#train.back <- drop1(model,test="Chisq")
#train.back

step(model,direction="backward")

model1 <- lm(formula = log(house_price)~crime_murder + crime_rape + crime_robbery + 
               crime_arson + property_house + property_house_per + property_townhouse + 
               property_townhouse_per + property_low_rise_per + property_mid_rise + 
               property_mid_rise_per + property_high_rise_per + income + 
               unemployment_rate + population,data=train)
par(mfrow = c(2, 2)) 
plot(model1)  

pred <- predict(model1,test,type='response',interval = 'confidence')

test$fit <- as.data.frame(pred)$fit
test$error <- log(test$house_price) - test$fit

RMSE(test$house_price,test$fit)
