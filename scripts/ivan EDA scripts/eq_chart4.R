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

kern_2019 <- h_county %>%
  filter(year == 2019, county == "kern")

rect <- data.frame(xmin=7, xmax=7.5, ymin=-Inf, ymax=Inf)

ggplot(data=kern_2019, aes(x=month, y=price)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(label = dollar_format()) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.5,
            inherit.aes = FALSE) +
  labs(title = "House price index for Kern county in 2019",
       subtitle = "5th July 2019 Ridgecrest earthquake highlighted\n(7.1 Magnitude)",
       y = "House Price Index",
       x = "Month")

## version 2 chart - convert to date (filter by 6 months either side of EQ event)
kern2 <- h_county %>%
  filter(year == 2018 | year == 2019, county == "kern") %>%
  mutate(temp = paste(month.name[month],year,sep=" "))

kern2_2 <- kern2 %>%
  mutate(date = as.yearmon(temp)) %>%
  filter(date >= 2018 & date < 2019.05)

rect2 <- data.frame(xmin=2018.472, xmax=2018.528, ymin=-Inf, ymax=Inf)

ggplot(data=kern2_2, aes(x=date, y=price)) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.6,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = kern2_2$date,
                     labels = as.yearmon(kern2_2$date)) +
  scale_y_continuous(label = dollar_format(), breaks = seq(175000,190000,2500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank()) +
  labs(title = "House price index for Kern county in 2019",
       subtitle = "5th July 2019 Ridgecrest earthquake highlighted\n(7.1 Magnitude)",
       y = "House Price Index",
       x = "")

## version 3 - house price growth by %
kern3 <- kern2_2 %>%
  mutate(growth = round(price/175903.6 - 1,digits = 4))

ggplot(data=kern3, aes(x=date, y=growth)) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#f7bb97",
            alpha=0.6,
            inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = kern3$date,
                     labels = as.yearmon(kern3$date)) +
  scale_y_continuous(label = percent_format(), breaks = seq(0,0.07,0.01)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank()) +
  labs(title = "House price index for Kern county in 2019",
       subtitle = "5th July 2019 Ridgecrest earthquake highlighted\n(7.1 Magnitude)",
       y = "House Price Index",
       x = "")
