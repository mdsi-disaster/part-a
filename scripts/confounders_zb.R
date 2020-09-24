# confounders --------------

library(tidyverse)
library(here)
library(dplyr)
library(magrittr)

## load merged data
quakes <- read_csv("data/_archive/merged/quakes.csv")

#filter out yeaars that don't have quakes 
quakes <- quakes %>%
  filter(year >= 2010) %>%
  mutate(crime_index = (crime_index*100000)) #multiply crime index by 100,000 to complete calculation for crime index 

#create variable for observations that have desctructive quakes (moderate + severe) 
quakes <- quakes %>%
  mutate(quakes_m_s = (quakes_moderate + quakes_severe))

#create binary variable where moderate or severe quake = 1 and no moderate or severe quake = 0 - so that we can see when destructive quakes occurred 
quakes$des_quake <- ifelse(quakes$quakes_m_s >= 1, 1, 0)

#replace nas in the des_quake column? Decided not to do this as just because there is an NA doesn't mean that a severe or moderate quake didn't occur 
# quakes <- quakes %>% 
#   dplyr ::mutate(des_quake = replace_na(des_quake,0))

quakes %>%
ggplot(mapping = aes(x = crime_index, y = house_price, colour = des_quake)) + 
  geom_point()


plot(quakes, y, type = "n", frame = FALSE)


abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = \
       2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", c\
       ex = 2)
