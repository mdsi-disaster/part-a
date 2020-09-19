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



