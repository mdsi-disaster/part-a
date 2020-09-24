## inflation adjusted house prices -----------------

library(here)
library(tidyverse)
library(readxl)
library(magrittr)

houses <- read_csv(here("data", "house_price", "h_price_clean.csv"))

adj_house <- houses %>%
  mutate(adjusted=NA) %>%
  mutate(adjusted=ifelse(year=="2010",(house_price*0.1920)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2011",(house_price*0.1555)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2012",(house_price*0.1321)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2013",(house_price*0.1157)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2014",(house_price*0.0979)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2015",(house_price*0.0966)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2016",(house_price*0.083)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2017",(house_price*0.0604)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2018",(house_price*0.0346)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2019",(house_price*0.0167)+house_price,adjusted)) %>%
  mutate(adjusted=ifelse(year=="2020",(house_price*0)+house_price,adjusted))

# remove years 2000 to 2009 and nas
adj_house <- adj_house %>%
  drop_na()

# group by year
yr_house <- adj_house %>% 
  group_by(year, county, city) %>%
  summarise(house_price = (mean(house_price)),
            adjusted = (mean(adjusted)))

#try to plot difference between house price and adjusted house price for inflation 
ggplot(yr_house, aes(x=year)) + 
  geom_line(aes(y=house_price), colour="red") + 
  geom_line(aes(y=adjusted), colour="blue")

            