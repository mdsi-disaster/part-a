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
       subtitle = "24th Aug 2019 Napa earthquake highlighted\n(6.0 Magnitude)",
       y = "House Price Index",
       x = "Month")
