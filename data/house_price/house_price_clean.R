library(tidyverse)
library(here)
wide_house <- read.csv(here('data','house_price','ca_house_raw.csv'))
names(wide_house)

long_house <-  wide_house %>% select(-(X:SizeRank),-RegionType,-StateName) %>%
  rename()
  

clean_house <- separate(data = long_house,col = Date,sep = "([.])",into = c("Year",'Month','Day')) %>% 
  separate(col = Year, sep = "(?<=[A-Z])(?=[0-9])",into = c("X","Year")) %>% select(-"X",-"Day") %>%
  rename(Zip_code = RegionName,County = CountyName)

#write.csv(clean_house,here("data","house_price","ca_house_price.csv"))
