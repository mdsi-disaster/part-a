# merging ---------

crime <- read_csv(here("data", "crimedata", "clean_cacrime.csv"))

income <- read_csv(here("data", "income", "bea_income.csv"))

unemployment <- read_csv(here("data", "unemployment", "unemployment.csv"))

annual_unemployment <- unemployment %>%
  group_by(year, area_name) %>%
  summarise(mean = mean(unemployment_rate))

annual_unemployment <- annual_unemployment %>%
  mutate(area_name = str_remove_all(area_name, " city")) %>%
  mutate(area_name = str_remove_all(area_name, " town")) %>%
  mutate(area_name = tolower(area_name)) %>%
  rename(city = "area_name", avg_unemployment = "mean")

income <- income %>%
  mutate(city = tolower(city)) 

#check for unique keys before merging data

annual_unemployment %>%
  count(year, city) %>%
  filter(n > 1)

income %>%
  count(year, city) %>%
  filter(n > 1)

summary(annual_unemployment)

summary(income)

merged <- crime %>% 
  select(-murder_and_nonnegligent_manslaughter, -rape, -robbery, -aggravated_assault, -burglary, -arson, -vehicle_theft, -larceny_theft, -sum_violent_crime, -sum_property_crime) %>%
  left_join(income) %>% #don't specify keys to join by so that all matching keys are joined
  select(-Code, -GeoFips, -GeoName, -CL_UNIT, -UNIT_MULT) %>%
  left_join(annual_unemployment) %>%
  mutate(crime_index = (crime_index*100))

missmap(merged)

namerged <- merged %>%
  na.omit(merged)

table <- namerged %>%
  count(county, year)

merged %>%
  ggplot(aes(avg_unemployment, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0,10)

merged %>%
  ggplot(aes(income_per_cap, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0, 5) 

# creating county level data 
countylv <- namerged %>%
  group_by(county, year) %>%
  summarise(av_unemp = (mean(avg_unemployment)), 
            av_income = (mean(income_per_cap)),
            av_crime = (mean(crime_index)))

unique(countylv$county) #looking at the unique county names, after removing na values, only 18 counties are left in the dataset

summary(countylv)

summary(countylv)[["3rd Qu."]]

countylv %>%
  ggplot(aes(av_unemp, av_crime)) +
  geom_point(alpha = 0.3) +
  ylim(0,10)

countylv %>%
  ggplot(aes(income_per_cap, crime_index)) +
  geom_point(alpha = 0.3) +
  ylim(0, 5) 


fit0 <- lm(crime_index~avg_unemployment, data = merged)  

summary(fit0)$coef   

fit1 <- lm(crime_index~avg_unemployment + income_per_cap, data = merged)

summary(fit1)$coef   

merged %>%
  ggplot(aes(crime_index, city)) +
  geom_point(alpha = 0.3)
