library(ggplot2)
library(maps)
library(tidyr)
library(zoo)

# Use merged data set
data <- read_csv("./merged_data.csv")


# UNEMPLOYMENT ######

# Extract unemployment data and format dates for charts
ur <- data %>% 
  select(year, month, city, unemployment_rate) %>% 
  group_by(year, month, city, unemployment_rate) %>% 
  top_n(1) %>% 
  ungroup() %>% 
  mutate(date = as.yearmon(paste(year, month), '%Y %m'))

# Summarise per date 
ur_sum <- ur %>% 
  group_by(date) %>% 
  summarise(mean = mean(unemployment_rate), 
            median = median(unemployment_rate), 
            high = max(unemployment_rate), 
            low = min(unemployment_rate)) %>% 
  ungroup()

# Tidy format
ur_sum <- ur_sum %>% 
  pivot_longer(c(mean, median, high, low), names_to = "type")

# Plot
ur_sum %>% 
  ggplot(aes(x = date, y = value, group = type, colour = type)) + 
  geom_line() +
  theme(legend.title = element_blank()) + 
  labs(title = "Unemployment Rates",
       subtitle = "Summary of Cities",
       x = "Date (Monthly)",
       y = "Rate (%)")



# INCOME ######

# Extract Income data and format dates for charts
income <- data %>% 
  select(year, quarter, county, income) %>% 
  group_by(year, quarter, county, income) %>% 
  top_n(1) %>% 
  ungroup() %>% 
  mutate(date = as.yearqtr(paste(year, quarter), '%Y %q'))


# Summarise per date
inc_sum <- income %>% 
  group_by(date) %>% 
  summarise(mean = mean(income), 
            median = median(income), 
            high = max(income), 
            low = min(income)) %>% 
  ungroup()

# Tidy format
inc_sum <- inc_sum %>% 
  pivot_longer(c(mean, median, high, low), names_to = "type")

# Plot
inc_sum %>% 
  ggplot(aes(x = date, y = value, group = type, colour = type)) + 
  geom_line() +
  theme(legend.title = element_blank()) + 
  labs(title = "Income",
       subtitle = "Summary of Counties",
       x = "Date (Quarterly)",
       y = "Avg Monthly Earnings")


# PER REGION PLOTS ######

# Load major regions from Census data
regions <- read_csv("./region_list.csv")

# Join with income data
regions <- regions %>% 
  rename(county = County) %>% 
  mutate(county = tolower(county))

inc_region <- income %>% 
  left_join(regions, by = 'county')

# Summarise per region 
inc_reg_sum <- inc_region %>% 
  group_by(date, Region) %>% 
  summarise(mean = mean(income), 
            median = median(income), 
            high = max(income), 
            low = min(income)) %>% 
  ungroup()

# Tidy Format
inc_reg_sum <- inc_reg_sum %>% 
  pivot_longer(c(mean, median, high, low), names_to = "type")

# Plot
inc_reg_sum %>% 
  ggplot(aes(x = date, y = value, group = type, colour = type)) + 
  geom_line() +
  facet_wrap(~Region, ncol = 3) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45)) + 
  labs(title = "Income",
       subtitle = "Summary by Region",
       x = "Date (Quarterly)",
       y = "Avg Monthly Earnings")

# Fix outlier
# inc_region[inc_region$county == "tuolumne" & inc_region$year == 2010 & inc_region$quarter == 2, 'income'] <- 3313


#Extract city to county mapping 
city_county <- data %>% 
  group_by(city, county) %>% 
  summarise(n = n()) %>% 
  select(city, county) %>% 
  ungroup()

# Merge with unemployment
ur <- ur %>% 
  left_join(city_county, by = "city")

# Merge region
ur <- ur %>% 
  left_join(regions, by = "county")

# Summarise per region 
ur_reg_sum <- ur %>% 
  group_by(date, Region) %>% 
  summarise(mean = mean(unemployment_rate), 
            median = median(unemployment_rate), 
            high = max(unemployment_rate), 
            low = min(unemployment_rate)) %>% 
  ungroup()

# Tidy Format
ur_reg_sum <- ur_reg_sum %>% 
  pivot_longer(c(mean, median, high, low), names_to = "type")

# Plot
ur_reg_sum %>% 
  ggplot(aes(x = date, y = value, group = type, colour = type)) + 
  geom_line() +
  facet_wrap(~Region, ncol = 3) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45)) + 
  labs(title = "Unemployment Rate",
       subtitle = "Summary by Region",
       x = "Date (Monthly)",
       y = "Rate %")


# MAPPING ######

# Map counties / regions 
# County data from maps 
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

# Merge with Census region names 
ca_df <- ca_county %>%
  rename(county = subregion) %>% 
  left_join(regions, by = "county")

# Plot
ggplot(ca_df) + 
  geom_polygon(aes(x = long, y = lat, group = county, fill = Region), color = "black") + 
  coord_fixed(1.3) + 
  theme_bw()
