library(tidyverse)
library(lubridate)
library(Distance)

andamans = read.csv("andamans.csv")
andamans = andamans %>% unite("Sample.Label", c(date,transect.id))

andamans_forest = andamans %>% 
  filter(habitat %in% c("Deciduous","Evergreen/Semi-Evergreen","Forest Edge","Littoral","Mangrove"))

forest.effort = andamans_forest %>% 
  group_by(Sample.Label) %>% slice(1) %>% ungroup() %>%
  select(Sample.Label,Effort)
forest.area = andamans_forest %>% 
  group_by(Region.Label) %>% slice(1) %>% ungroup() %>%
  select(Region.Label,Area)
forest.area$Area = forest.area$Area

an_fl = andamans_forest %>%
  filter(common.name == "Andaman Woodpecker") %>%
  select(Region.Label,Sample.Label,distance)
an_fl = left_join(an_fl,forest.effort)
an_fl = left_join(an_fl,forest.area)
an_fl = an_fl %>% filter(!is.na(Effort),!is.na(Area),!is.na(distance))

hist(an_fl$distance, breaks = 20)

conversion.factor = convert_units("meter", "kilometer", "hectare")
anfl.hn <- ds(data=an_fl, key="hn", adjustment=NULL,
              convert.units=conversion.factor)
summary(anfl.hn)

cutpoints = seq(0,40,2)
plot(anfl.hn, breaks=cutpoints, main="Half normal model, line transects")

