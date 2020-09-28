# confounders --------------

library(tidyverse)
library(here)
library(dplyr)
library(magrittr)
library(scales)

## load merged data
quakes <- read_csv("data/_archive/merged/quakes.csv")

#filter out yeaars that don't have quakes 
quakes <- quakes %>%
  filter(year >= 2010) %>%
  mutate(crime_index = (crime_index*100000)) #multiply crime index by 100,000 to complete calculation for crime index 

#create variable for observations that have desctructive quakes (moderate + severe) 
quakes <- quakes %>%
  mutate(quakes_m_s = (quakes_moderate + quakes_severe))

#create binary variable where moderate or severe quake = 1 and no moderate or severe quake = 0 - so that we can see when destructive quakes occurred 
quakes$des_quake <- ifelse(quakes$quakes_m_s >= 1, 1, 0)

#replace nas in the des_quake column? Decided not to do this as just because there is an NA doesn't mean that a severe or moderate quake didn't occur 
# quakes <- quakes %>% 
#   dplyr ::mutate(des_quake = replace_na(des_quake,0))

#initial plot reveals some house price outliers and the points all being very congested, decided to remove outliers and take the data up to a year level to reduce the number of observations. 
quakes %>%
ggplot(mapping = aes(x = crime_index, y = house_price, colour = des_quake)) + 
  geom_point()

#divide house price by 1000 so that it's easier to see on the plot  
quakes <- quakes %>%
  mutate(house_price = (house_price/1000)) 

#check the distribution of house prices 
summary(quakes)

#remove outlier house prices -----------------

boxplot(quakes$house_price, main="Outliers", boxwex=0.1) #boxplot shows that there are outliers to deal with

Q <- quantile(quakes$house_price, probs=c(.25, .75), na.rm = FALSE) #use the quantile() function to find the 25th and the 75th percentile of the dataset

iqr <- IQR(quakes$house_price) # use the IQR() function to find the difference of the 75th and 25th percentiles

# Now that you know the IQR and the quantiles, you can find the cut-off ranges beyond which all data points are outliers.

up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range

#Using the subset() function, you can simply extract the part of your dataset between the upper and lower ranges leaving out the outliers. The code for removing outliers is:

sub_quakes <- subset(quakes, quakes$house_price > (Q[1] - 1.5*iqr) & quakes$house_price < (Q[2]+1.5*iqr)) #20 observations that fell outside .25 and .75 have been removed 

boxplot(sub_quakes$house_price, main="Outliers", boxwex=0.1) #boxplot shows that the outliers have been removed 

#remove the variables that I don't need for the year level dataset
yr_quakes <- sub_quakes %>% 
  select(-crime_murder, -crime_rape, -crime_robbery, -crime_assault, -crime_burglary, -crime_larceny, -crime_vehicle, -crime_arson, -crime_violent_total, -crime_property_total, -crime_index, -property_house, -property_total, -property_townhouse, -property_high_rise, -property_low_rise, -property_mid_rise, -des_quake)

#take the data up to a year level 
yr_quakes <- yr_quakes %>%
  group_by(year, city, county) %>% 
  summarise(quakes_minor = (sum(quakes_minor, na.rm = TRUE)), 
            quakes_moderate = (sum(quakes_moderate, na.rm = TRUE)),
            quakes_severe = (sum(quakes_severe, na.rm = TRUE)),
            house_price = (mean(house_price, na.rm = TRUE)),
            population = (mean(population, na.rm = TRUE)),
            crime_rate = (sum(crime_total, na.rm = TRUE))/(sum(population, na.rm = TRUE)), 
            gdp_change = (mean(gdp_change, na.rm = TRUE)), 
            homeless_count = (mean(homeless_count, na.rm = TRUE)),
            income = (mean(income, na.rm = TRUE)),
            property_house_per = (mean(property_house_per, na.rm = TRUE)), 
            property_townhouse_per = (mean(property_townhouse_per, na.rm = TRUE)), 
            property_low_rise_per = (mean(property_low_rise_per, na.rm = TRUE)), 
            property_high_rise_per = (mean(property_high_rise_per, na.rm = TRUE)), 
            property_mid_rise_per = (mean(property_mid_rise_per, na.rm = TRUE)), 
            unemployment_rate = (mean(unemployment_rate, na.rm = TRUE)), 
            quakes_m_s = (sum(quakes_m_s, na.rm = TRUE)))


#create binary variable where moderate or severe quake = 1 and no moderate or severe quake = 0 - so that we can see when destructive quakes occurred 
yr_quakes$des_quake <- ifelse(yr_quakes$quakes_m_s >= 1, 1, 0)

summary(quakes)

by(data = yr_quakes$house_price, INDICES = yr_quakes$des_quake, FUN = mean)

# find out what the average house price when there are severe quakes and when there aren't severe quakes 
mean_houses <- yr_quakes %>%
  group_by(des_quake) %>%
  summarise(house_price_mean = mean(house_price, na.rm = TRUE))

A <- mean_houses %>%
  filter(des_quake == 0) #there weren't sever quakes (black) 

B <- mean_houses %>%
  filter(des_quake == 1) #there were severe quakes (blue) 

# add horizontal lines to the plot to show the average house price when there are severe quakes and when there aren't severe quakes 
yr_quakes %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, colour = des_quake)) + 
  geom_point() +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("blue", "black"))

#find the linear model for the house price based on crime rate 
slm <- lm(crime_rate~house_price,data=yr_quakes)

str(yr_quakes)

#to get a better scale for des_quakes convert des_quakes from numeric to factor 
yr_quakes$des_quake <- as.factor(yr_quakes$des_quake)

# crime ------------------

yr_quakes %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, colour = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  geom_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N')

fit1 <- lm(house_price~des_quake, data = yr_quakes) 
summary(fit1)$coef

crime_fit <- lm(house_price~des_quake+crime_rate, data = yr_quakes) 
summary(crime_fit)$coef 

#make the same graph just for 2010 to see if it's easier to read
quakes_10 <- yr_quakes %>%
  filter(year == 2010)

# find out what the average house price when there are severe quakes and when there aren't severe quakes 
mean_houses_10 <- quakes_10 %>%
  group_by(des_quake) %>%
  summarise(house_price_mean = mean(house_price, na.rm = TRUE))

# 351.4532 average house price when no quakes = CORAL
# 385.3709 average house price when there are severe or moderate quakes = CYAN3

quakes_10 %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, colour = des_quake)) + 
  geom_point() +
  geom_hline(yintercept = c(351.4532, 385.3709), colour = c("coral", "cyan3"), linetype = "dashed") + 
  geom_smooth(method = 'lm', se=FALSE)
#on second thoughts, I don't think that we can use this graph because it is just earthquake data for one year. That means that each observation is unique to a city. We should look at the whole data set so that there are years where a city had a severe earthquake and didn't have a severe earthquake 

#Playing around with different models

fit2 <- lm(house_price~des_quake+crime_rate, data = yr_quakes) #purple 
summary(fit2)$coef 

fit3 <- lm(house_price~crime_rate, data = yr_quakes) #blue 
summary(fit3)$coef 

fit4 <- lm(house_price~crime_rate+des_quake, data = yr_quakes) #orange 
summary(fit4)$coef 


yr_quakes %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, colour = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  geom_abline(slope = fit3$coefficients[2], 
              intercept = fit3$coefficients[1], 
              colour = "blue") + 
  geom_abline(slope = fit2$coefficients[2], 
              intercept = fit2$coefficients[1], 
              colour = "purple") + 
  geom_abline(slope = fit1$coefficients[2], 
              intercept = fit1$coefficients[1], 
              colour = "red")


yr_quakes %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, colour = des_quake)) + 
  geom_point() +
  geom_hline(yintercept = c(438.7198, 452.9788), 
             colour = c("black", "blue"), 
             linetype = "dotted") + 
  geom_abline(slope = fit1$coefficients[2], 
              intercept = fit1$coefficients[1], 
              colour = "red") +
  geom_abline(slope = fit2$coefficients[2], 
              intercept = fit2$coefficients[1], 
              colour = "blue")

yr_quakes %>%
  ggplot(mapping = aes(x = crime_rate, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') +
  geom_abline(slope = fit3$coefficients[2], 
              intercept = fit3$coefficients[1], 
              colour = "blue")

summary(yr_quakes)

# income -------------------
yr_quakes %>%
  ggplot(mapping = aes(x = income, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') 

fit1 <- lm(house_price~des_quake, data = yr_quakes) #red
summary(fit1)$coef

inc_fit <- lm(house_price~des_quake+income, data = yr_quakes) #blue 
summary(inc_fit)$coef 

# unemployment -------------------
yr_quakes %>%
  ggplot(mapping = aes(x = unemployment_rate, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') 

fit1 <- lm(house_price~des_quake, data = yr_quakes) #red
summary(fit1)$coef

unemp_fit <- lm(house_price~des_quake+unemployment_rate, data = yr_quakes) #blue 
summary(unemp_fit)$coef 

# gdp change -------------------
yr_quakes %>%
  ggplot(mapping = aes(x = gdp_change, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') 

fit1 <- lm(house_price~des_quake, data = yr_quakes) #red
summary(fit1)$coef

gdp_fit <- lm(house_price~des_quake+gdp_change, data = yr_quakes) #blue 
summary(gdp_fit)$coef 

# population -------------------
yr_quakes %>%
  ggplot(mapping = aes(x = population, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') 

fit1 <- lm(house_price~des_quake, data = yr_quakes) #red
summary(fit1)$coef

pop_fit <- lm(house_price~des_quake+population, data = yr_quakes) #blue 
summary(pop_fit)$coef 

summary(yr_quakes)

# include an economic measure in the model to see if areas that are higher in crime are associated with lower house prices generally 

fit5 <- lm(house_price~des_quake+crime_rate+unemployment_rate, data = yr_quakes)  
summary(fit5)$coef 

fit6 <- lm(house_price~des_quake+crime_rate+unemployment_rate+income, data = yr_quakes)  
summary(fit6)$coef 

fit7 <- lm(house_price~des_quake+crime_rate+unemployment_rate+gdp_change+population, data = yr_quakes)  
summary(fit7)$coef 

fit8 <- lm(house_price~des_quake+crime_rate+population, data = yr_quakes)  
summary(fit8)$coef 


# # gdp second attempt -------------------
#maybe it would be good to include GDP, but this needs to first be grouped into categoric variables (change from continuous variable since it is a percentage change) #THIS didn't really work - commenting it out 

# gdp_quakes <- yr_quakes
# 
# gdp_quakes$gdp_cat<-cut(gdp_quakes$gdp_change, c(-5,0,5,10,15,20,25,30,35,40,45,50,55))  
# 
# table(gdp_quakes$gdp_cat)
# 
# str(gdp_quakes)
# 
# gdp_quakes %>%
#   ggplot(mapping = aes(x = gdp_cat, y = house_price, col = des_quake)) + 
#   geom_point(alpha = 0.3) +
#   geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
#   stat_smooth(method = 'lm', se=FALSE) +
#   labs(colour='Quakes Y/N') 

#fault score -------------

## load merged data
faults <- read_csv("data/fault_lines/faults_scored.csv")

faults <- faults %>%
  select(county, fault_score)

summary(faults)

fau_quakes <- yr_quakes %>% #adding a column for fault score to the new dataframe 
  left_join(faults, by = "county")

fau_quakes %>%
  ggplot(mapping = aes(x = fault_score, y = house_price, col = des_quake)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(438.7198, 452.9788), colour = c("cyan3", "coral"), linetype = "dashed") + 
  stat_smooth(method = 'lm', se=FALSE) +
  labs(colour='Quakes Y/N') 

fit1 <- lm(house_price~des_quake, data = fau_quakes) #red
summary(fit1)$coef

fault_fit <- lm(house_price~des_quake+fault_score, data = fau_quakes) #blue 
summary(fault_fit)$coef 
