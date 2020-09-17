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



earthquake <- read_csv("data/earthquake/earthquakes_with_region.csv")
faults_scored <- read_csv("data/fault_lines/faults_scored.csv")
county_fips <- read_csv("data/keys/county_fips.csv")

faults <- merge(x = county_fips, y = faults_scored, by="county", all=TRUE)


eq_map <- earthquake %>%
  filter(state == "California") %>%
  select(longitude, latitude, mag) %>%
  rename(lon = longitude, lat = latitude)

eq_map <- usmap_transform(eq_map)

eq_map2 <- eq_map %>%
  filter(mag >= 4.5)

plot_usmap(include = "CA", regions = "counties") +
  geom_point(data=eq_map, 
             aes(x=lon.1, y=lat.1, size = mag),
             color = "#97d0bf", 
             alpha = 0.3)

plot_usmap(include = "CA", regions = "counties", data = faults, values = "fault_score") +
  scale_fill_continuous(low = "gray90", 
                        na.value = "white", 
                        high = "gray20",
                        name = "Fault Line Score") +
  geom_point(data=eq_map2, 
             aes(x=lon.1, y=lat.1, size = mag),
             color = "#97d0bf", 
             alpha = 0.6) +
  labs(title = "Earthquakes Recorded in California (2010-2020)",
       subtitle = "projected over fault line score and\nhighlighting events with magnitude 4.5 or greater",
       size = "Magnitude") +
  theme(legend.position = "right")
