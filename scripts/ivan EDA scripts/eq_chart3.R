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

h_price_clean <- read_csv("data/house_price/h_price_clean.csv")
county_fips <- read_csv("data/keys/county_fips.csv")
earthquake_clean_v2 <- read_csv("data/earthquake/earthquake_clean_v2.csv")
earthquakes_clean <- read_csv("data/earthquake/earthquakes_clean.csv")

h_county <- h_price_clean %>%
  group_by(county,month,year) %>%
  summarise(price = mean(house_price))

h_year <- h_county %>%
  group_by(county,year) %>%
  summarise(annual_price = mean(price))

h_county <- merge(x = h_county, y = h_year, by=c("county","year"), all=TRUE)

h_county_v2 <- h_county %>%
  mutate(dev = (price / annual_price - 1)*100)

merged_df <- merge(x = h_county_v2, y = earthquake_clean_v2, by=c("county","month","year"), all=TRUE)

merged_df_v2 <- merged_df %>%
  mutate(quake_ms = quakes_moderate + quakes_severe) %>%
  mutate(quake_ms = replace_na(quake_ms,0))


merged_df_v2 <- merged_df_v2 %>%
  mutate(quake_bin = 1) ## bin 1 = 0 quakes

merged_df_v2$quake_bin <- as.numeric(merged_df_v2$quake_bin)
merged_df_v2 <- within(merged_df_v2, quake_bin[quake_ms >= 1] <- 2) ## bin2 = 1-10 quakes
merged_df_v2 <- within(merged_df_v2, quake_bin[quake_ms >= 11] <- 3) ## bin3 = 11-20 quakes
merged_df_v2 <- within(merged_df_v2, quake_bin[quake_ms >= 21] <- 4) ## bin3 = 11-100 quakes
merged_df_v2 <- within(merged_df_v2, quake_bin[quake_ms >= 101] <- 5) ## bin4 = 100+ quakes

merged_df_v2$quake_bin <- as.factor(merged_df_v2$quake_bin)

merged_df_v2$dev <- merged_df_v2$dev/100

rect <- data.frame(ymin=-0.0002, ymax=0.0002, xmin=-Inf, xmax=Inf)

ggplot(merged_df_v2, aes(x=quake_bin, y=dev, fill = quake_bin)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
             fill="#F87C7C",
             alpha=1,
             inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.025,0.025)) +
  scale_x_discrete(labels=c("0","1 to 10","11 to 20", "21 to 100", "100+")) +
  scale_fill_manual(values = c("white", "#DCCDFF", "#AEA2C9", "#847B99","#5F586E")) +
  labs(title = "Deviation of monthly house price in periods of high earthquake activity",
       subtitle = "against the county average house price for the year\nlooking at earthquake magnitudes of 3.1 or greater",
       x = "Number of earthquakes detected in the month",
       y = "Deviation in price from the county's\naverage annual house price index")

