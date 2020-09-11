library(Quandl)

# Effective Federal Funds Rate
data_q <- Quandl("FRED/DFF") 

# Daily Interest Rates 
interest_rate <- data_q %>% 
  mutate(year = as.numeric(substr(Date, 1, 4)), month = as.numeric(substr(Date, 6, 7))) %>% 
  group_by(year, month) %>% 
  summarise(interest_rate = mean(Value)) %>% 
  ungroup()


# Save copy 
write_csv(interest_rate, "./interest_rate.csv")
