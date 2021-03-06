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
library(corrplot)
library(qacr)

crime <- read_csv(here("data", "crimedata", "clean_cacrime.csv"))
cities <- read_csv(here("data", "crimedata", "clean_city_county.csv"))

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

crime_year <- crime %>% #checking to 
  group_by(year) %>% 
  summarise(crime = (sum(sum_index_crimes)))

crime06 <- crime %>%
  filter(year == 2006)

crime04 <- crime %>%
  filter(year == 2004)

setdiff(cities$city, crime06$city)

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = county)) # this is a graph of the number of observations for each county. 

# for some reason there are a lot of instances of LA. There are 88 cities in LA and we have 20 years of data so it's possible that there are 1760 observations for LA.  Orange County has the next highest number of observations, OC has 34 cities, over 30 years of data, there could be 680 entries for OC cities, so the number of observations displayed is reasonable. 

crime %>%
  ggplot() + 
  geom_bar(mapping = aes(y = year)) # this is a graph of the number of observations for each year (i.e. number of cities with data recorded each year) There is more data from 2006-2018. There is only a comparatively small amount of data from 1999 - 2005. 2019 only has data collected for 6 months of the year. Might be best to remove this year. 

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

cities %>%
  count(city) #there are 482 cities in total in the list 

county_key <- read_csv(here("data", "keys", "county_key.csv")) 

setdiff(cities$city, newcrime$city) #this shows the names of the 21 municipalities that we don't have crime data for. These municipalities mostly have a population of less than 10,000 people, except for Wasco which has a poppulation of 25,000 

setdiff(county_key$county, newcrime$county) #this shows the names of the 5 counties that we don't have crime data for: Sierra (~3,000 people), Plumas (less than 20,000 people), Trinity, Alpine, Mariposa 

city_pop <- read_csv(here("data", "crimedata", "city_population.csv")) #load in csv with county populations

city_pop <- city_pop %>%
  mutate(City = tolower(City)) #county observations to lowercase

city_missing <- city_pop %>%
  filter(City %in% c("amador city","blue lake","carmel-by-the-sea","colfax",     "corte madera","la cañada flintridge","larkspur","lathrop","live oak","loomis","loyalton","maricopa","plymouth","point arena","portola","portola valley","san carlos","san joaquin","san juan bautista","shasta lake","tehama",               "trinidad","wasco","woodside"))
  
city_missing %>%
  ggplot(aes(x=Population, y=City)) + 
  geom_bar(fill="cadetblue3", stat="identity") +
  labs(title = "Missing Cities by Population") + 
  theme_bw()

# which city has the highest crime index across the 8 year period? -----------


citycrime <- newcrime %>% 
  group_by(city, county) %>% 
  summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), #get the crime index averages when grouping by year 
            av_violent = (sum(sum_violent_crime))/(sum(population)),
            av_property = (sum(sum_property_crime))/(sum(population))) %>%
  mutate(av_crime= (av_crime*100)) %>%
  mutate(av_violent= (av_violent*100)) %>%
  mutate(av_property= (av_property*100))


#label outliers here if I have time

citycrime %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) +
  theme_bw() + 
  labs(title = "Outliers for City Crime Index", 
       y = "Crime Index") 

#check the distribution of the average crime index in cities. There are some outliers. 

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

#boxplot(citycrime$av_crime_caps, main="City Crime Index", boxwex=0.1) 

#Or remove the values that fall below 25% or above 75% https://www.r-bloggers.com/how-to-remove-outliers-in-r/

Q <- quantile(citycrime$av_crime, probs=c(.25, .75), na.rm = FALSE) #use the quantile() function to find the 25th and the 75th percentile of the dataset

iqr <- IQR(citycrime$av_crime) # use the IQR() function to find the difference of the 75th and 25th percentiles

# Now that you know the IQR and the quantiles, you can find the cut-off ranges beyond which all data points are outliers.

up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range

#Using the subset() function, you can simply extract the part of your dataset between the upper and lower ranges leaving out the outliers. The code for removing outliers is:

eliminated <- subset(citycrime, citycrime$av_crime > (Q[1] - 1.5*iqr) & citycrime$av_crime < (Q[2]+1.5*iqr)) #20 observations that fell outside .25 and .75 have been removed 

boxplot(eliminated$av_crime, main="City Crime Index", boxwex=0.1) #boxplot shows that the outliers have been removed 

eliminated %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) +
  theme_bw() + 
  labs(title = "Outliers Removed for City Crime Index", 
       y = "Crime Index")

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
  theme_bw() + 
  labs(title = "Highest Average Crime Index by City 2010-2018", 
       x = "Crime Index", 
       y = "City")

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

#eliminated$county <- factor(eliminated$county, levels = eliminated$county[order(eliminated$av_crime)])

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

# Or should the county crime be calculated once the city outliers are removed? Because otherwise the cities that were considered outliers are included in the average for the counties 


countycrime2 <- eliminated %>% 
  group_by(county) %>% 
  summarise(av_crime = (mean(av_crime)), #get the crime index averages when grouping by year 
            av_violent = (mean(av_violent)),
            av_property = (mean(av_property)))

countycrime2 %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = av_crime)) #check the distribution of the average crime index in counties. There are come outliers but I'd rather not remove them. 

# elim_county %>%
#   ggplot() + 
#   geom_boxplot(mapping = aes(y = av_crime)) #another way to visualise outliers removed

countycrime2 %>% 
  arrange(desc(av_crime)) # top 10 values are >= 3.43

countycrime2$county <- factor(countycrime2$county, levels = countycrime2$county[order(countycrime2$av_crime)])

countycrime2 %>% 
  filter(av_crime >= 3.43) %>%
  ggplot(mapping = aes(x = av_crime, y = county)) + 
  geom_point() + 
  xlab("") +
  theme_bw() +
  labs(title = "Highest Average County Crime Rate", 
       x = "Crime Index", 
       y = "County") # Lake, Shasta and Inyo have the highest average crime rates if you don't remove the outliers

#elim_county2 %>% #dataset with outliers eliminated
#   arrange(desc(av_crime)) # top 10 values are >= 3.24
# 
# eliminated$city <- factor(eliminated$city, levels = eliminated$city[order(eliminated$av_crime)])

# elim_county2 %>% 
#   filter(av_crime >= 3.24) %>%
#   ggplot( aes(x = av_crime, y = reorder(county, av_crime))) +
#   geom_point() + 
#   xlab("") +
#   theme_bw() +
#   labs(title = "Highest Average County Crime Rate", 
#        x = "Crime Index", 
#        y = "County")
# # Tehama, Modoc and Nevada have the highest crime rates if you take the outliers into account 

# elim_county2 %>% 
#   arrange(av_crime) # lowest 10 values are <= 2.05
# 
# elim_county2 %>% 
#   arrange(av_crime) %>%
#   filter(av_crime <= 2.05) %>%
#   ggplot(mapping = aes(x = av_crime, y = county)) + 
#   geom_point() + 
#   theme_bw() + 
#   labs(title = "Lowest County Crime Rate", 
#        x = "Crime Index", 
#        y = "County")



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
  theme_bw() + 
  labs(title = "Highest City Crime Rate", 
       x = "Crime Index", 
       y = "City")

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
  geom_line() + 
  xlab("") +
  theme_bw() + 
  labs(title = "Crime Rate by Year", 
       x = "Year", 
       y = "Crime Rate")

yearcrime %>% #find out what the top ten counties and cities are
  arrange(desc(av_crime))

#crime index is highest in 2012 and lowest in 2018

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


# Check out maps ------------
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

fipcrime <- countycrime2

# fipcrime <- elim_county2 %>% #new dataframe that I'll add fip codes to
#   group_by(county) %>%
#   summarise(av_crime = (sum(sum_index_crimes))/(sum(population)), 
#             av_violent = (sum(sum_violent_crime))/(sum(population)),
#             av_property = (sum(sum_property_crime))/(sum(population))) %>%
#   mutate(av_crime= (av_crime*100)) %>%
#   mutate(av_violent= (av_violent*100)) %>%
#   mutate(av_property= (av_property*100))

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

# Correlation between crime and house prices ------------

# I'm interested in finding out if there is any correlation between crime and house price data 

crime <- read_csv(here("data", "crimedata", "clean_cacrime.csv"))

newcrime <- crime %>% #create a new data frame that's just for the years that we have earthquake data for 2010-2020. Remove 2019 since there are only 6 months worth of data for this year
  filter(year >= 2010 & year < 2019)

house_price <- read_csv("data/house_price/h_price_clean.csv")

house_price <- house_price %>% #create a new data frame that's just for the years that we have crime data for 2010-2018. 
  filter(year >= 2010 & year < 2019)

#Since we only have crime data at an annual level, I'm going to bring house price up to an annual level and take the average house price across the months 

house_price <- house_price %>% 
  group_by(year, county, city) %>%
  summarise(house_price = (mean(house_price)))

house_price %>%
  ggplot() + 
  geom_bar(mapping = aes(y = city)) # this is a graph of the number of observations for each city, can see that we don'y have every city for every year of crime data  

data <- house_price %>%
  left_join(crime, by = c("city","county","year")) 

data <- data %>%
  mutate(crime_index = (crime_index*100)) %>% #multiply index for percentage 
  drop_na() %>% #remove rows that contain na as they seem to mess with the corplot
  rename(murder = murder_and_nonnegligent_manslaughter, property_crime = sum_property_crime, violent_crime = sum_violent_crime) 

data2 <- data %>% #remove variables that I'm not that interested in 
  select(-murder, -rape, -arson, -larceny_theft, -vehicle_theft, -robbery, -aggravated_assault, -burglary) 

corplot(data) #this is a function from the qacr function that only plots the numeric variables 

M <- cor(data2[c(1,4,5,6,7,9)])

MM <- cor(data[c(1,4,5,6,7,8,9,10,11,12,13,14,15,17)])

head(round(M,2))

corrplot(M, method = "circle")
corrplot(MM, method = "circle")

corrplot(M, method = "number")

corrplot(M, method = "color")

corrplot(M, type="upper")

corrplot(M, type="lower")

corrplot(M, type="upper", order="hclust")

library(RColorBrewer)

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, 
         title = "Correlation: Crime and House Prices", 
         mar=c(0,0,2,0), 
         diag=FALSE)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# Correlation earthquakes and crime -------------

earthquakes <- read_csv("data/earthquake/earthquakes_clean.csv")

earthquakes <- earthquakes %>% #create a new data frame that's just for the years that we have crime data for 2010-2018. 
  filter(year >= 2010 & year < 2019)

#Since we only have crime data at an annual level, I'm going to bring earthquake up to an annual level and take the total number of quakes across the months. 

quakes <- earthquakes %>% 
  group_by(year, county) %>%
  summarise(quakes_minor = (sum(quakes_minor)),
            quakes_moderate = (sum(quakes_moderate)),
            quakes_severe = (sum(quakes_severe)))

#Since we only have earthquake data at a county level, I'm going to bring crime up to a county level and take the total number of crimes for the county.  

crime_county <- crime %>% 
  select(-murder_and_nonnegligent_manslaughter, -rape, -arson, -larceny_theft, -vehicle_theft, -robbery, -aggravated_assault, -burglary, -crime_index)

crime_county <- crime_county %>% 
  group_by(year, county) %>%
  summarise(sum_violent_crime = (sum(sum_violent_crime)),
            sum_property_crime = (sum(sum_property_crime)),
            sum_index_crime = (sum(sum_index_crimes)), 
            population = (sum(population))) %>%
  mutate(crime_index = (sum_index_crime)/(population)*100) 

quakes <- quakes %>%
  mutate(county = str_remove_all(county, " County")) %>%
  mutate(county = tolower(county))

quake_crime <- quakes %>%
  left_join(crime_county, by = c("county","year")) %>%
  drop_na()


N <- cor(quake_crime[c(1,3,4,5,6,7,9,10)])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(N, type="upper", order="hclust", tl.col="black", tl.srt=45, 
         title = "Correlation: Earthquakes and Crime", 
         mar=c(0,0,2,0), 
         diag=FALSE)

