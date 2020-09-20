
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)

##### Slide 1: Economic Indicators #####

# Load Data
employment <- read_csv("./data/unemployment/unemployment_clean.csv")
interest_rate <- read_csv(".data/interest/interest_clean.csv")

# Summarise and merge data 
emp_sum <- employment %>% 
  select(year, month, area_name, unemployment_rate) %>% 
  group_by(year, month) %>% 
  summarise(unemployment_rate = mean(unemployment_rate)) %>% 
  ungroup()

int_sum <- interest_rate %>% 
  group_by(year, month) %>% 
  summarise(interest_rate = mean(Value)) %>% 
  ungroup()

rates <- emp_sum %>% 
  left_join(int_sum, by = c("year", "month"))

# Tidy format 
rates <- rates %>% 
  mutate(date = as.yearmon(as.yearmon(paste(year, month), '%Y %m'))) %>% 
  select(date, unemployment_rate, interest_rate) %>% 
  pivot_longer(-date, names_to = "indicator", values_to = "rate")

# Plot graph - larger sizes for slide legibility
g1 <- ggplot(rates, aes(x=date, y=rate, group = indicator, colour = indicator)) + 
  geom_line(size = 1.5) + 
  labs(title = "Time Series of Economic Indicators",
       subtitle = "Inverse Relationship",
       x = "Date", 
       y = "Rate %") +
  scale_colour_discrete(name = "", 
                        breaks = c("interest_rate", "unemployment_rate"),
                        labels = c("Interest Rate", "Unemployment Rate")) + 
  scale_x_yearmon(format = "%Y") + 
  theme_bw(base_size = 20) + 
  theme(legend.position = "bottom")

g1



##### Slide 2: Income, Housing Prices and Fault Scores #####

# Load Data
fault_score <- read_csv("./data/fault_lines/faults_scored.csv")
earnings <- read_csv("./data/income/earnings_clean.csv")
full_data <- read_csv("./data/merged_data_raw.csv")

# Clean data and merge 
merged_inc <- earnings %>% 
  select(year, quarter, county_name, EarnS) %>% 
  rename(county = county_name, income = EarnS) %>% 
  mutate(county = gsub(" county", "", tolower(county))) %>% 
  left_join(fault_score, by = c("county")) 

house_prices <- full_data %>% 
  select(year, month, county, city, house_price) %>% 
  filter(year >= 2000, year < 2020) %>% 
  mutate(quarter = ceiling(month / 3)) %>% 
  group_by(year, quarter, county) %>% 
  summarise(house_price = median(house_price, na.rm = T)) %>% 
  ungroup()

merged_inc <- merged_inc %>% 
  left_join(house_prices, by = c("year", "quarter", "county"))

# Plot Graph 
plot_data <- merged_inc %>% 
  filter(year %in% c(2000, 2005, 2010, 2015)) %>% 
  group_by(year, county) %>% 
  summarise(income = median(income), score = max(number_score), house_price = median(house_price)) %>% 
  na.omit() %>% 
  ungroup()

g2 <- ggplot(plot_data, aes(x = house_price, y = income, colour = score, size = score)) +
  geom_point(alpha = 0.8) + 
  scale_size(range = c(2, 7)) + 
  scale_colour_continuous(name = "Fault Score", labels = c(1, 4, 7, 10)) +
  geom_smooth(method=lm, se = F, colour = "black", size = 0.5, linetype = "dashed") + 
  facet_wrap(~year, scales = "free") + 
  labs(title = "Income vs Housing Prices",
       subtitle = "Per County",
       x = "House Price ($)", 
       y = "Monthly Income ($)") +
  theme_bw(base_size = 16) + 
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  guides(size = F)

g2
