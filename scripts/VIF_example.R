library(car)
library(tidyverse)
library(here)
library(gvlma)
data <- read.csv(here('data','mn_model_df.csv'),stringsAsFactors = TRUE)

data$id <- 1:nrow(data)
train <- data%>%sample_frac(.75)
test <- anti_join(data,train,by='id')
model <- lm(formula = log(house_price)~crime_murder + crime_rape + crime_robbery + 
               crime_arson + property_house + property_house_per + property_townhouse + 
               property_townhouse_per + property_low_rise_per + property_mid_rise + 
               property_mid_rise_per + property_high_rise_per + income + 
               unemployment_rate + population,data=train)
#multicollinearity

#Variance Inflation Factor
vif(model)

VIF <- function(linear.model, no.intercept=FALSE, all.diagnostics=FALSE, plot=FALSE) {
  require(mctest)
  
  if(no.intercept==FALSE) design.matrix <- model.matrix(linear.model)[,-1]
  if(no.intercept==TRUE) design.matrix <- model.matrix(linear.model)
  
  if(plot==TRUE) mc.plot(design.matrix, linear.model$model[1])
  if(all.diagnostics==FALSE) output <- imcdiag(linear.model, method='VIF')$idiags[,1]
  if(all.diagnostics==TRUE) output <- imcdiag(linear.model)
  output
}
VIF(model)

library(car)
sqrt(vif(model)) > 2
VIF(model, plot=TRUE)
