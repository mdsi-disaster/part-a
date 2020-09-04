# Clean fault lines ----------------

library(readxl)
library(here)
library(janitor)
library(Amelia)
library(dplyr)
library(magrittr)
library(tidyverse)
library(tidyr)
library(textclean)

# load data that can later be used to verify that the counties in the fault dataset are all Californian
city_county <- read_excel(here("data", "crimedata","city_county.xls"), col_names = TRUE)

city_county <- city_county %>% #rename 'city name column 'Name' variable to city and only select 'city' and 'county' variables
  rename("city" = "Name", "county" = "County") %>% 
  select("city", "county") %>% 
  mutate(county = tolower(county)) %>% #county observations to lowercase
  mutate(city = tolower(city)) #city observations to lowercase


# load fault line data 
faults <- read_excel(here("data", "fault_lines", "fault_lines.xls"), col_names = TRUE)

str(faults)
names(faults)

faults <- faults %>% 
  rename("county" = "Primary  - State", "time" = "Time of most recent deformation","slip_rate" = "Slip-rate category") %>%
  drop_na("Name") %>% #remove na rows from 'name' variable
  drop_na("Fault ID") %>% #remove na rows from 'Fault ID' variable 
  clean_names("snake") %>% 
  mutate(county = tolower(county)) %>% #county observations to lowercase
  mutate(name = tolower(name)) #fault names to lowercase
  
#not required - filter(!county %in% "california") #remove counties called 'California' 

#verify that all of the counties in the fault line dataset are Californian counties 

faults <- faults %>% 						
  filter(county %in% city_county$county) 

counties <- faults %>%
  count(faults$county)

write_csv(faults, here("data", "fault_lines","clean_faults.csv"))

