# Clean crime -----------

library(readxl)
library(here)
library(janitor)
library(Amelia)
library(dplyr)
library(magrittr)
library(tidyverse)
library(tidyr)
library(textclean)

# load and tidy each dataset ---------------

# county mapping to city data ---------

#create a column for county 
city_county <- read_excel(here("crime data","city_county.xls"), col_names = TRUE)

city_county <- city_county %>% #rename 'city name column 'Name' variable to city and only select 'city' and 'county' variables
  rename("city" = "Name", "county" = "County") %>% 
  select("city", "county") %>% 
  mutate(county = tolower(county)) %>% #county observations to lowercase
  mutate(city = tolower(city)) #city observations to lowercase


#2019---------------

crime19 <- read_excel(here("crime data","california_offenses_2019.xls"), skip = 5, col_names = TRUE) # read in the data, remove the top 5 rows of the spreadsheet 

str(crime19) #check that the variables are in the correct format 
names(crime19)

crime19 <- crime19 %>% #rename and tidy variables 
  rename("population" = "Population1", "rape" = "Rape2", "arson" = "Arson3", "murder_and
         nonnegligent_manslaughter" = "Murder", "vehicle_theft"="Motor \nvehicle \ntheft") %>%
  clean_names("snake")

crime19 <- crime19 %>%
  filter(state == "CALIFORNIA", year == "2019") #remove states other than california and years other than 2019

#remove state column

crime19 <- crime19 %>%
  select(-"state") 

# convert city names from caps to sentence case 
crime19 <- crime19 %>%
  mutate(city = tolower(city))

crime19 <- crime19 %>% #add a column for 'county' 
  left_join(city_county, by = "city") 

crime19 <- crime19 %>% #tidy the county varaible 
  mutate(county = tolower(county))

crime19 <- crime19[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,2)] #reorder variables

crime19[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime19[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2018---------------

crime18 <- read_excel(here("crime data","california_offenses_2018.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime18)#check that the variables are in the correct format 
names(crime18)

crime18 <- crime18 %>% #rename and tidy variables 
  rename("rape" = "Rape1","vehicle_theft"="Motor\nvehicle\ntheft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

crime18$year <- 2018 #add a column for the year 

crime18[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime18[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2017---------------

crime17 <- read_excel(here("crime data","california_offenses_2017.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime17) #check that the variables are in the correct format 

crime17 <- crime17 %>% #rename and tidy variables 
  rename("city" = "State", "rape" = "Rape1", "vehicle_theft"="Motor\nvehicle\ntheft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city")

crime17$year <- 2017 #add a column for the year 

crime17[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime17[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2016---------------

crime16 <- read_excel(here("crime data","california_offenses_2016.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime16) #check that the variables are in the correct format 

names(crime16)

crime16 <- crime16 %>% #rename and tidy variables 
  rename("rape" = "Rape\n(revised\ndefinition1)", "vehicle_theft"="Motor\nvehicle\ntheft") %>%
  select(-"Rape\n(legacy\ndefinition2)") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

crime16$year <- 2016 #add a column for the year 

crime16[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime16[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2015---------------

crime15 <- read_excel(here("crime data","california_offenses_2015.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime15) #check that the variables are in the correct format 
names(crime15)

crime15 <- crime15 %>% #rename and tidy variables 
  rename("rape" = "Rape\n(revised\ndefinition)1", "vehicle_theft"="Motor\nvehicle\ntheft") %>%
  select(-"Rape\n(legacy\ndefinition)2") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

crime15$year <- 2015 #add a column for the year 

crime15[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime15[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2014---------------

crime14 <- read_excel(here("crime data","california_offenses_2014.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime14) #check that the variables are in the correct format 
names(crime14)

#the data from 2014 is slightly different to the data from the other years, there are two columns for 'rape' representing different definitions - the revised definition and the legacy definition. The data from 2015 included these two variables but had no occurrences under the legacy definition 

crime14[c("Rape\n(revised\ndefinition)1","Rape\n(legacy\ndefinition)2")][is.na(crime14[c("Rape\n(revised\ndefinition)1","Rape\n(legacy\ndefinition)2")])] <- 0 #replace NA with 0

crime14 <- crime14 %>% #rename and tidy variables 
  rename("rape_new" = "Rape\n(revised\ndefinition)1", "rape_old" = "Rape\n(legacy\ndefinition)2", "vehicle_theft"= "Motor\nvehicle\ntheft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

names(crime14)

crime14$rape <- crime14$rape_new + crime14$rape_old #sum old and new rape variables to create one aggregate rape variable 

crime14 <- crime14 %>%
  select(-rape_new, -rape_old) #remove individual rape variable 

crime14 <- crime14[,c(1,2,3,4,13,5,6,7,8,9,10,11,12)] #reorder variables

crime14$year <- 2014 #add a column for the year 

crime14[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime14[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2013---------------

crime13 <- read_excel(here("crime data","california_offenses_2013.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime13) #check that the variables are in the correct format 
names(crime13)

crime13 <- crime13 %>% #rename and tidy variables 
  rename("rape" = "Rape\n(legacy\ndefinition)2", "vehicle_theft" = "Motor\nvehicle\ntheft") %>%
  select(-"Rape\n(revised\ndefinition)1") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

crime13$year <- 2013 #add a column for the year 

crime13[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime13[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2012---------------

crime12 <- read_excel(here("crime data","california_offenses_2012.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime12) #check that the variables are in the correct format 
names(crime12)

crime12 <- crime12 %>% #rename and tidy variables 
  rename("rape" = "Forcible\nrape", "vehicle_theft" = "Motor\nvehicle\ntheft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") 

crime12$year <- 2012 #add a column for the year 

crime12[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime12[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2011---------------

crime11 <- read_excel(here("crime data","california_offenses_2011.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime11) #check that the variables are in the correct format 
names(crime11)

crime11 <- crime11 %>% #rename and tidy variables 
  rename("rape" = "Forcible\nrape", "arson" = "Arson1", "vehicle_theft" = "Motor\nvehicle\ntheft") %>%
  select(-"State") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("county")

crime11$year <- 2011 #add a column for the year 

crime11[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime11[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2010---------------

crime10 <- read_excel(here("crime data","california_offenses_2010.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime10) #check that the variables are in the correct format 
names(crime10)

crime10 <- crime10 %>% #rename and tidy variables 
  rename("rape" = "Forcible\nrape", "vehicle_theft" = "Motor\nvehicle\ntheft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city")

crime10$year <- 2010 #add a column for the year 

crime10[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime10[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2009---------------

crime09 <- read_excel(here("crime data","california_offenses_2009.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime09) #check that the variables are in the correct format 
names(crime09)

crime09 <- crime09 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "vehicle_theft" = "Motor vehicle theft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city")

crime09$year <- 2009 #add a column for the year 

crime09[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime09[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2008---------------

crime08 <- read_excel(here("crime data","california_offenses_2008.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime08) #check that the variables are in the correct format 
names(crime08)

crime08 <- crime08 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft") %>%
  select(-"State") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("county")

crime08$year <- 2008 #add a column for the year 

crime08[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime08[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2007---------------

crime07 <- read_excel(here("crime data","california_offenses_2007.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime07) #check that the variables are in the correct format 
names(crime07)

crime07 <- crime07 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft") %>%
  select(-"State") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("county")

crime07$year <- 2007 #add a column for the year 

crime07[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime07[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2006---------------

crime06 <- read_excel(here("crime data","california_offenses_2006.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime06) #check that the variables are in the correct format 
names(crime06)

crime06 <- crime06 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city")

crime06$year <- 2006 #add a column for the year 

crime06[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime06[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2005---------------

crime05 <- read_excel(here("crime data","california_offenses_2005.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime05) #check that the variables are in the correct format 
names(crime05)

crime05 <- crime05 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft") %>%
  select(-"State") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("county")

crime05$year <- 2005 #add a column for the year 

crime05[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime05[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2004---------------

crime04 <- read_excel(here("crime data","california_offenses_2004.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime04) #check that the variables are in the correct format 
names(crime04)

crime04 <- crime04 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape", "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder and non-negligent man-slaughter") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county")

crime04$year <- 2004 #add a column for the year 

crime04[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime04[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2003---------------

crime03 <- read_excel(here("crime data","california_offenses_2003.xls"), skip = 4, col_names = TRUE) # read in the data, remove the top 4 rows of the spreadsheet 

str(crime03) #check that the variables are in the correct format 
names(crime03)

crime03 <- crime03 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape" , "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder and non-negligent man-slaughter") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county")

crime03$year <- 2003 #add a column for the year 

crime03[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime03[c("population","violent_crime","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","property_crime","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0

#2002---------------

crime02 <- read_excel(here("crime data","california_offenses_2002.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime02) #check that the variables are in the correct format 
names(crime02)

crime02 <- crime02 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape" , "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder      and non-negligent         man-     slaughter", "crime_index_total" = "Crime Index") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county") %>%
  select(-"modified_crime_index1")

crime02$year <- 2002 #add a column for the year 

crime02[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime02[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2001---------------

crime01 <- read_excel(here("crime data","california_offenses_2001.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime01) #check that the variables are in the correct format 
names(crime01)

crime01 <- crime01 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape" , "arson" = "Arson1", "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder      and non-negligent         man-     slaughter") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county") %>%
  select(-"modified_crime_index_total1")

crime01$year <- 2001 #add a column for the year 

crime01[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime01[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#2000---------------

crime00 <- read_excel(here("crime data","california_offenses_2000.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime00) #check that the variables are in the correct format 
names(crime00)

crime00 <- crime00 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape" , "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder      and non-negligent         man-     slaughter") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county") %>%
  select(-"modified_crime_index_total1")

crime00$year <- 2000 #add a column for the year 

crime00[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime00[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #change NA values to 0


#1999---------------

crime99 <- read_excel(here("crime data","california_offenses_1999.xls"), skip = 3, col_names = TRUE) # read in the data, remove the top 3 rows of the spreadsheet 

str(crime99) #check that the variables are in the correct format 
names(crime99)

crime99 <- crime99 %>% #rename and tidy variables 
  rename("rape" = "Forcible rape" , "vehicle_theft" = "Motor vehicle theft", "city" = "City by state", "murder_and_nonnegligent_manslaughter" = "Murder      and non-negligent         man-     slaughter") %>%
  clean_names("snake") %>%
  mutate(city = tolower(city)) %>% # convert city names from caps to sentence case 
  left_join(city_county, by = "city") %>%
  drop_na("population") %>%
  drop_na("county") %>%
  select(-"modified_crime_index_total1") %>%
  replace(is.na(.), 0)

crime99$year <- 1999 #add a column for the year 

#binding and cleaning the datasets ---------------------

cacrime <- bind_rows(crime99, crime00, crime01, crime02, crime03, crime04, crime05, crime06, crime07, crime08, crime09, crime10, crime11, crime12, crime13, crime14, crime15, crime16, crime17, crime18, crime19)

cacrime <- cacrime[,c(13,12,1,2,3,4,5,6,7,8,9,10,11,14,15)] #reorder variables

names(cacrime)

cacrime$city <- gsub("[0-9]", "", cacrime$city) #clean the city names, remove random numbers e.g.'lafayette2' becomes 'lafayette'

cacrime <- cacrime %>% #add in county for cities like 'lafayette2' as they missed out on county initially 
  left_join(city_county, by = "city") 

cacrime <- cacrime %>%
  select(-county.x) %>%
  rename("county" = "county.y")

names(cacrime)

cacrime <- cacrime[,c(1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)] #reorder variables

#remove any funky data in the city column e.g. "1 The figures shown in this column for the offense of rape were reported using only the revised Uniform Crime Reporting definition of rape. See the data declaration for further explanation." 

cacrime <- cacrime %>% 						
  filter(city %in% city_county$city) 


#creating indexes ------------------

cacrime <- cacrime %>%
  select(-crime_index_total, -violent_crime, -property_crime) #remove the old crime index 
  
str(cacrime)
names(cacrime) 

crime00[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")][is.na(crime00[c("population","murder_and_nonnegligent_manslaughter","rape","robbery","aggravated_assault","burglary","larceny_theft","vehicle_theft","arson")])] <- 0 #make sure NA values are 0

cacrime <- cacrime %>%
  mutate(sum_violent_crime = rowSums(.[5:8])) %>% #totals for violent crime category 
  mutate(sum_property_crime = rowSums(.[9:12])) %>% #totals for property crime category 
  mutate(sum_index_crimes = rowSums(.[5:11])) #totals for crimes considered by FBI index (this is all crimes except arson https://ucr.fbi.gov/crime-in-the-u.s/2002/02sec2.pdf)

cacrime$crime_index <- cacrime$sum_index_crimes / cacrime$population

table(cacrime$city)

cities <- cacrime %>% 
  count(cacrime$city)

counties <- cacrime %>%
  count(cacrime$county)

write_csv(cacrime, here("crime data","clean_cacrime.csv"))

write_csv(city_county, here("crime data","clean_city_county.csv"))
