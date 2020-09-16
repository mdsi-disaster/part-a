library(here)
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)

house_price <- read_csv(here("data","house_price","h_price_clean.csv"))

skim(house_price)

house_price %>% count(city,county)

#line chart for each county
grouped_house_price <- house_price %>% group_by(county, year) %>% 
  summarise(median_price = median(house_price,na.rm = TRUE))

ggplot(data = grouped_house_price, aes(x=year, y = median_price,color = county)) +
  geom_line(size=1) +
  labs(title = "Median home value for 54 counties")

grouped_house_price_top10 <-  grouped_house_price %>% group_by(county) %>% 
  summarise(std_price = sd(median_price)) %>% 
  filter(rank(desc(std_price))<=10)


counties <- c()
for (i in grouped_house_price_top10['county']){
  print(i)
  counties = cbind(counties,i)
}
top_house_price = grouped_house_price%>% filter(county %in% counties)
ggplot(data = top_house_price, aes(x=year, y = median_price,color = county)) +
  geom_line(size=1) + 
  labs(title = "Median home value for top 10 variant counties")


#Histogram 
skim(grouped_house_price)
ggplot(data = top_house_price, mapping = aes(median_price)) +
  geom_histogram() + 
  facet_wrap(~county) +
  labs(title="Median price histogram for top 10 variant counties")

rest =grouped_house_price %>% filter(!(county %in% counties))

ggplot(data = grouped_house_price, mapping = aes(median_price)) +
  geom_histogram() + 
  facet_wrap(~county) +
  labs(title="Median price histogram for 54 counties")


