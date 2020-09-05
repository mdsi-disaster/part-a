### AT2A - Homelessness ###

# load library
library(readxl)
library(tidyverse)
library(lubridate)

# load file
homelessness <- read_excel("C:/Users/sandr/projects/part-a/data/homelessness/2007_2017_point_in_time_count.xlsx")


# keep year only from year column - unable to get it to work with lubridate - need to research
# homelessness$year<- as.POSIXlt(homelessness$year, format = "%y/%m/%d")
# homelessness %>% mutate(year = lubridate::year(date))
homelessness$year <- substr(homelessness$year, 0, 4)


# filter data required
homelessness <- 
  homelessness %>% 
  filter(state == "CA",
         measures == "Total Homeless") %>%
  rename(total_homeless_count = count) %>%
  separate(coc_name, 
           sep = "City ",
           into = c("county", "other")) 

# find regex to split county further


# Keep required columns only
homelessness <- homelessness[,c(1:2,4,7)]

# Could potentially use this as a total of homeless people per year in CA?
write.csv(homelessness,"./homelessness_inprogress.csv", row.names = FALSE)
