# EDA on crime ------------------

library(readxl)
library(here)
library(skimr)
library(janitor)
library(tidyverse)
library(magrittr)
library(Amelia)
library(ggplot2)
library(descriptr)
library(mapproj)
library(ggmap)
library(urbnmapr)
library(viridis)
library(ruler)
library(maps)
library(usmap)

crime <- read_csv(here("data", "crimedata", "clean_cacrime.csv"))

# check for dupes
crime %>%
  count(county,city,year) %>%
  filter(n != 1) 

# check for NAs
crime %>%
  filter(is.na(county)) 

# Visualise the distribution of a categorical variables 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = city)) # this is a graph of the number of observations for each city, can see that we don'y have every city for every year of crime data 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = county)) # this is a graph of the number of observations for each county. 

# for some reason there are a lot of instances of LA. There are 88 cities in LA and we have 20 years of data so it's possible that there are 1760 observations for LA.  Orange County has the next highest number of observations, OC has 34 cities, over 30 years of data, there could be 680 entries for OC cities, so the number of observations displayed is reasonable. 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = year)) # this is a graph of the number of observations for each year. There is more data from 2006-2018. There is only a comparatively small amount of data from 2019 - 2019 only has data collected for 6 months of the year. Might be best to remove this year. 

crime %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = year), binwidth = 5) # this is a graph of the number of observations for each year. There is more data from 2006-2018.

crime <- crime %>%
  mutate(crime_index = (crime_index*100)) #make the crime index into a percentage, easier number to work with / visualise in graphs 

crime %>%
  ggplot(mapping = aes(x = year, colour = county)) + 
  geom_freqpoly(binwidth = 0.1) #trying to see the number of observations for each year for each county. There are so many counties that it's difficult to see them all in a graph.  

lacity <- crime %>%
  filter(county == "los angeles") #make a data frame for just LA county to have a look at it more closely 

lacity %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = year)) # most observations are between 2006 and 2018

lacity %>%
  ggplot() +
  geom_bar(mapping = aes(y = city)) # there are not the same number of observations for each of the cities in LA 

lacitycount <- lacity %>%
  count(city)

#There are 87 cities in LA so it makes sense that there are over 1,500 observations for LA county 

crime %>% 
  count(city)


# EDA 2010 - 2018 Crime data --------------

# Missing data: which cities and counties do we have data for ----------------

cities <- read_csv(here("data", "crimedata", "clean_city_county.csv")) #load in all 482 city names and all 55 county names 

newcrime <- crime %>% #create a new data frame that's just for the years that we have earthquake data for 2010-2020. Remove 2019 since there are only 6 months worth of data for this year
  filter(year >= 2010 & year < 2019)

newcrime %>% #since there are 9 years of data, if there is data for each city for each year, I would expect to have a count of 9 for each of the cities
  count(city, county) %>%
  filter(n != 9) #there are 18 cities that don't have data for each year. 

setdiff(cities$city, newcrime$city) #this shows the names of the 21 municipalities that we don't have crime data for. These municipalities mostly have a population of less than 10,000 people, except for Wasco which has a poppulation of 25,000 

setdiff(cities$county, newcrime$county) #this shows the names of the 2 counties that we don't have crime data for: Sierra (~3,000 people) and Plumas (less than 20,000 people)

# which city has the highest crime index across the 8 year period? -----------


citycrime <- newcrime %>% 
  group_by(city, county) %>% 
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), #get the crime index averages when grouping by year 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

citycrime %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) #check the distribution of the average crime index in cities. There are some outliers. 

summary(citycrime)

# # identifying outliers - following steps on this page: http://r-statistics.co/Outlier-Treatment-With-R.html
# 
# outlier_values <- boxplot.stats(citycrime$av_crime)$out
# 
# dev.off() # This will delete your current plots in the RStudio Plots Pane. If you have multiple graphics devices open, repeat this command until the output displays null device.
# 
# boxplot(citycrime$av_crime, main="City Crime Index", boxwex=0.1)
# 
# mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
# 
# print(outlier_values) #checking out the av_crime values that are classified as outliers - makes sense as these values are all within the 4th quartile 
# 
# #outlier(citycrime$av_crime) # this is the most extreme observation from the mean 
# 
# #outlier(citycrime$av_crime, opposite = TRUE) # this is the least extreme observation from the mean? 
# 
# # Capping - For missing values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie above the upper limit, with the value of 95th %ile
# 
# x <- citycrime$av_crime #code to replace lower limits with the 5th percentile and values that lie above the upper limit with the 95th percentile 
# qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
# caps <- quantile(x, probs=c(.05, .95), na.rm = T)
# H <- 1.5 * IQR(x, na.rm = T)
# x[x < (qnt[1] - H)] <- caps[1]
# x[x > (qnt[2] + H)] <- caps[2]
# 
# print(x)
# 
# citycrime$av_crime_caps <- x #add a new column to the citycrime data frame for the capped av crime index 

boxplot(citycrime$av_crime_caps, main="City Crime Index", boxwex=0.1) 

#Or remove the values that fall below 25% or above 75% https://www.r-bloggers.com/how-to-remove-outliers-in-r/

Q <- quantile(citycrime$av_crime, probs=c(.25, .75), na.rm = FALSE) #use the quantile() function to find the 25th and the 75th percentile of the dataset

iqr <- IQR(citycrime$av_crime) # use the IQR() function to find the difference of the 75th and 25th percentiles

# Now that you know the IQR and the quantiles, you can find the cut-off ranges beyond which all data points are outliers.

up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range

#Using the subset() function, you can simply extract the part of your dataset between the upper and lower ranges leaving out the outliers. The code for removing outliers is:

eliminated <- subset(citycrime, citycrime$av_crime > (Q[1] - 1.5*iqr) & citycrime$av_crime < (Q[2]+1.5*iqr)) #20 observations that fell outside .25 and .75 have been removed 

boxplot(eliminated$av_crime, main="City Crime Index", boxwex=0.1) #boxplot shows that the outliers have been removed 

citycrime %>% 
  arrange(desc(av_crime)) # top 10 values are >= 7.26

citycrime %>% 
  arrange(desc(av_crime)) %>%
  filter(av_crime >= 7.26) %>%
  ggplot(mapping = aes(x = av_crime, y = city)) + 
  geom_point() 

# Industry, Vernon and Sand City have the highest crime rates across the 8 year period if you don't take the outliers into account

citycrime %>% 
  arrange(av_crime) # lowest 10 values are <= 0.839

citycrime %>% 
  arrange(av_crime) %>%
  filter(av_crime <= 0.839) %>%
  ggplot(mapping = aes(x = av_crime, y = city)) + 
  geom_point() 

# Calipatria, Imperial and etna have the lowest crime rates across the 8 year period 

eliminated %>% 
  arrange(desc(av_crime)) # top 10 values are >= 5.18

eliminated$city <- factor(eliminated$city, levels = eliminated$city[order(eliminated$av_crime)])

eliminated %>% 
  filter(av_crime >= 5.18) %>%
  ggplot( aes(x = av_crime, y = reorder(city, av_crime))) +
  geom_point() + 
  xlab("") +
  theme_bw()

# Industry, Vernon and Sand City have the highest crime rates across the 8 year period if you take the outliers into account

# Big Bear Lake, Palm Springs and Grass valley have the highest crime rates across the 8 year period if you remove the outliers

# Which Counties have the highest crime across the 8 year period ------------

countycrime <- newcrime %>% 
  group_by(county) %>% 
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), #get the crime index averages when grouping by year 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

countycrime %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) #check the distribution of the average crime index in counties. There are some outliers. 

# remove the outliers 

Q2 <- quantile(countycrime$av_crime, probs=c(.25, .75), na.rm = FALSE) #use the quantile() function to find the 25th and the 75th percentile of the dataset

iqr2 <- IQR(countycrime$av_crime) # use the IQR() function to find the difference of the 75th and 25th percentiles

# Now that you know the IQR and the quantiles, you can find the cut-off ranges beyond which all data points are outliers.

up2 <-  Q2[2]+1.5*iqr2 # Upper Range  
low2 <- Q2[1]-1.5*iqr2 # Lower Range

#Using the subset() function, you can simply extract the part of your dataset between the upper and lower ranges leaving out the outliers. The code for removing outliers is:

elim_county <- subset(countycrime, countycrime$av_crime > (Q2[1] - 1.5*iqr2) & countycrime$av_crime < (Q2[2]+1.5*iqr2)) #2 observations that fell outside .25 and .75 have been removed 

boxplot(elim_county$av_crime, main="City Crime Index", boxwex=0.1) #boxplot shows that the outliers have been removed 

elim_county %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) #another way to visualise outliers removed

countycrime %>% 
  arrange(desc(av_crime)) # top 10 values are >= 4.42

countycrime %>% 
  arrange(desc(av_crime)) %>%
  filter(av_crime >= 4.42) %>%
  ggplot(mapping = aes(x = av_crime, y = county)) + 
  geom_point() # Tuolumne, Tehama and San Francisco have the highest crime rates if you don't remove the outliers

elim_county %>% #dataset with outliers eliminated
  arrange(desc(av_crime)) # top 10 values are >= 4.34

eliminated$city <- factor(eliminated$city, levels = eliminated$city[order(eliminated$av_crime)])

elim_county %>% 
  filter(av_crime >= 4.34) %>%
  ggplot( aes(x = av_crime, y = reorder(county, av_crime))) +
  geom_point() + 
  xlab("") +
  theme_bw() # San Francisco, Humboldt and Lake have the highest crime rates if you take the outliers into account 

countycrime %>% 
  arrange(av_crime) # lowest 10 values are <= 2.26

countycrime %>% 
  arrange(av_crime) %>%
  filter(av_crime <= 2.26) %>%
  ggplot(mapping = aes(x = av_crime, y = county)) + 
  geom_point()

# Calaveras, Colusa, San Benito have the lowest crime rates 

# Which year and which city has the highest crime rate? --------------
# Industry, a city with ~219 people has the highest crime rate, followed by Vernon, a city with ~112 people, then Sand City ~334, Colma ~1792 Are these significant values? 

#taking out the cities with population that is less than 5000, Emeryville and Orange Cove are the top ten cities for crime index 

#newcrime %>% #this was how i removed outliers before I found a more systematic way of removing them 
#  filter(population >= 5000) %>%
#  arrange(desc(crime_index)) %>%
#  select(year, county, city, crime_index) 

newcrime %>% 
  arrange(desc(crime_index)) %>%
  select(year, county, city, crime_index) 

newcrime %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = crime_index))

# remove the outliers 

Q3 <- quantile(newcrime$crime_index, probs=c(.25, .75), na.rm = FALSE) #use the quantile() function to find the 25th and the 75th percentile of the dataset

iqr3 <- IQR(newcrime$crime_index) # use the IQR() function to find the difference of the 75th and 25th percentiles

# Now that you know the IQR and the quantiles, you can find the cut-off ranges beyond which all data points are outliers.

up3 <-  Q3[2]+1.5*iqr3 # Upper Range  
low3 <- Q3[1]-1.5*iqr3 # Lower Range

#Using the subset() function, you can simply extract the part of your dataset between the upper and lower ranges leaving out the outliers. The code for removing outliers is:

elim_newc <- subset(newcrime, newcrime$crime_index > (Q3[1] - 1.5*iqr3) & newcrime$crime_index < (Q3[2]+1.5*iqr3)) #179 observations that fell outside .25 and .75 have been removed 

elim_newc %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = crime_index)) #boxplot shows that the outliers have been removed 

elim_newc %>% #find out what the top ten counties and cities are
  arrange(desc(crime_index)) %>% 
  select(year, county, city, crime_index)

# elim_newc$county <- factor(elim_newc$county, levels = elim_newc$county[order(elim_newc$crime_index)]) 

elim_newc %>% #not sure why this plot is not showing the y variables in the correct order 
  filter(crime_index >= 5.81) %>%
  ggplot( aes(x = crime_index, y = reorder(city, crime_index), colour = year)) +
  geom_point() + 
  xlab("") +
  theme_bw()

#Which year has the highest crime rate -----------

yearcrime <- elim_newc %>% #use the dataset that already has the outliers removed
  group_by(year) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_violent)) + 
  geom_point() 
#violent crime is at it's highest in 2017 and lowest in 2013

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_property)) + 
  geom_point() #property crime is highest in 2012 and lowest in 2018

yearcrime %>%
  ggplot(mapping = aes(x=year, y=av_crime)) + 
  geom_line() #crime index is highest in 2012 and lowest in 2018

ggplot(yearcrime, aes(year)) + 
  geom_line(aes(y=av_crime), colour="red") + 
  geom_line(aes(y=av_violent), colour="green") + 
  geom_line(aes(y=av_property), colour="blue")


# Crime trend in San Francisco -------------

sanfran <- newcrime %>%
  filter(city == "san francisco")

sanfran %>%
  ggplot(mapping = aes(x=year, y=crime_index)) + 
  geom_point() +
  geom_smooth()


# check out maps ------------
#https://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html

# map("usa")
# map("county")
# 
# map.cities(county = "California")
# 
# map("state", "CALIFORNIA")
# data(us.cities)
# map.cities(us.cities, country = "CA")
# 
# map("state", "CALIFORNIA")
# map("county")

#https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2

fipcrime <-newcrime

fipcrime <- elim_newc %>% #new dataframe that I'll add fip codes to
  group_by(county) %>%
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))

fipcrime$polyname <- paste("california", fipcrime$county, sep = ",") #new column called polyname that I can use to merge fipcode varaible 

fipcrime <- fipcrime %>% #joining the dataset 'county.fips' from the urbnmapr package so that I can add the fip codes to the fipcrime dataset
  left_join(county.fips, by = "polyname")


#following tutorial here: https://www.cgoodman.com/blog/archives/2018/06/16/maps-in-r-using-urbnmapr/
# # Create quantiles
# no_classes <- 6
# labels <- c()
# quantiles <- quantile(fipcrime$av_crime,
#                       probs = seq(0, 1, length.out = no_classes + 1))
# 
# # Custom labels, rounding
# labels <- c()
# for(idx in 1:length(quantiles)){
#   labels <- c(labels, paste0(round(quantiles[idx], 2),
#                              " – ",
#                              round(quantiles[idx + 1], 2)))
# }
# # Minus one label to remove the odd ending one
# labels <- labels[1:length(labels)-1]
# 
# # Create new variable for fill
# fipcrime$crime_quantiles <- cut(fipcrime$av_crime,
#                                 breaks = quantiles,
#                                 labels = labels,
#                                 include.lowest = T)
# 
# 
# ggplot() +
#   # County map
#   geom_polygon(data = fipcrime,
#                mapping = aes(x = long, y = lat,
#                              group = group,
#                              fill = fipcrime$crime_quantiles)) +
#   # Add state outlines
#   geom_polygon(data = urbnmapr::states,
#                mapping = aes(long, lat, group = group),
#                fill = NA, color = "#ffffff", size = 0.4) +
#   # Projection
#   coord_map(projection = "polyconic")+
#   # Fill color
#   scale_fill_viridis(
#     option = "viridis",
#     name = "Crime Index California",
#     discrete = T,
#     direction = -1,
#     end=0.9,
#     guide = guide_legend(
#       keyheight = unit(5, units = "mm"),
#       title.position = 'top',
#       reverse = F)
#   )+
#   # Theming
#   theme_minimal(base_family = "Open Sans Condensed Light")+
#   theme(
#     legend.position = "bottom",
#     legend.text.align = 0,
#     plot.margin = unit(c(.5,.5,.2,.5), "cm")) +
#   theme(
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )+
#   theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
#   theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
#   theme(plot.margin=unit(rep(0.5, 4), "cm"))+
#   labs(x = "",
#        y = "",
#        title = "Crime Index by County",
#        subtitle = "Metropolitan Power Diffusion Index, Special Districts 2012",
#        caption = "Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau Census of Governments")


# plot_usmap(data = fipcrime, values = "crime_quantiles", include = "CA") # need to set quantiles using above code to do this 

plot_usmap(data = fipcrime, values = "av_crime", include = "CA") + #viridis option colour scheme 
  scale_fill_viridis(
    option = "viridis",
    name = "Crime Index California") + 
  labs(x = "",
       y = "", 
       title = "Crime Index by County",
       subtitle = "Average Index from 2010 - 2018",
       caption = "")


plot_usmap(data = fipcrime, values = "av_crime", include = "CA") + 
  scale_fill_gradient2(midpoint = 3, low="white", mid="yellow", high="red", 
    name = "Crime Index California") + 
  labs(x = "",
       y = "", 
       title = "Crime Index by County",
       subtitle = "Average Index from 2010 - 2018",
       caption = "")
