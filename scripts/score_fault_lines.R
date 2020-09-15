
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

clean_faults <- read_csv("data/fault_lines/clean_faults.csv")

count_move <- clean_faults %>%
  count(slip_rate)

clean_faults_v2 <- clean_faults %>%
  mutate(slip_score = 1)

clean_faults_v2 <- within(clean_faults_v2, slip_score[slip_rate == "Between 0.2 and 1.0 mm/yr"] <- 2)
clean_faults_v2 <- within(clean_faults_v2, slip_score[slip_rate == "Between 1.0 and 5.0 mm/yr"] <- 3)
clean_faults_v2 <- within(clean_faults_v2, slip_score[slip_rate == "Greater than 5.0 mm/yr"] <- 4)

agg_faults <- clean_faults_v2 %>%
  add_count(county) %>%
  group_by(county,n) %>%
  summarise(fault_length = sum(length_km),fault_slip = mean(slip_score)) %>%
  rename(fault_num = n)

agg_faults <- agg_faults %>%
  mutate(length_score = 1)

agg_faults <- within(agg_faults, length_score[fault_length >= 100] <- 2)
agg_faults <- within(agg_faults, length_score[fault_length >= 250] <- 3)

agg_faults <- agg_faults %>%
  mutate(number_score = 1)

agg_faults <- within(agg_faults, number_score[fault_num >= 10] <- 2)
agg_faults <- within(agg_faults, number_score[fault_num >= 20] <- 3)
agg_faults <- within(agg_faults, number_score[fault_num >= 30] <- 4)

agg_faults <- agg_faults %>%
  mutate(fault_score = number_score + length_score + fault_slip*2)

write_csv(agg_faults, "data/fault_lines/faults_scored.csv")
