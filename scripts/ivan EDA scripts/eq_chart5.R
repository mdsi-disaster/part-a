library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

library(ggplot2)
library(maps)
library(tidyr)
library(zoo)
library(usmap)
library(scales)

h_price_clean <- read_csv("data/house_price/h_price_clean.csv")
county_fips <- read_csv("data/keys/county_fips.csv")
earthquake_clean_v2 <- read_csv("data/earthquake/earthquake_clean_v2.csv")
earthquakes_clean <- read_csv("data/earthquake/earthquakes_clean.csv")

h_county <- h_price_clean %>%
  group_by(county,month,year) %>%
  summarise(price = mean(house_price))

napa_2019 <- h_county %>%
  filter(year == c(2014), county == "napa")

rect <- data.frame(xmin=8, xmax=8.5, ymin=-Inf, ymax=Inf)



napa_2019$Date <- as.yearmon(paste(napa_2019$year, napa_2019$month), "%Y %m")



ggplot(data=napa_2019, aes(x=month, y=price)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(label = dollar_format()) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.5,
            inherit.aes = FALSE) +
  labs(title = "House price index for Napa county in 2014",
       subtitle = "24th Aug 2014 Napa earthquake highlighted\n(6.0 Magnitude)",
       y = "House Price Index",
       x = "Month")

## version 2 chart - convert to date (filter by 6 months either side of EQ event)
napa_2 <- h_county %>%
  filter(year == 2014 | year == 2015, county == "napa") %>%
  mutate(temp = paste(month.name[month],year,sep=" "))

napa3 <- napa_2 %>%
  mutate(date = as.yearmon(temp)) %>%
  filter(date > 2014.05 & date < 2015.1)

rect2 <- data.frame(xmin=2014.555, xmax=2014.611, ymin=-Inf, ymax=Inf)

ggplot(data=napa3, aes(x=date, y=price)) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.6,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = napa3$date,
                     labels = as.yearmon(napa3$date)) +
  scale_y_continuous(label = dollar_format(), breaks = seq(620500,657000,7000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank()) +
  labs(title = "House price index for Napa county in 2014",
       subtitle = "24th Aug 2014 Napa earthquake highlighted\n(6.0 Magnitude)",
       y = "House Price Index",
       x = "")

## version 3 - house price growth by %
napa4 <- napa3 %>%
  mutate(growth = round(price/620619.5 - 1, digits=4))

ggplot(data=napa4, aes(x=date, y=growth)) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.6,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = napa3$date,
                     labels = as.yearmon(napa3$date)) +
  scale_y_continuous(label = percent_format(), breaks = seq(0,0.07,0.01)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank()) +
  labs(title = "House price index movement before and after",
       subtitle = "the 24th Aug 2014 Napa earthquake\n(6.0 Magnitude)",
       y = "House Price Index Gain\nas of Feb 2014",
       x = "")
