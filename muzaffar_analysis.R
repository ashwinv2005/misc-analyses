library(tidyverse)
library(lubridate)
library(lme4)

datax = read.csv("Data_Muzaffar.csv")

data1 = datax %>% distinct(Sampling.site,Lat,long,Elevation,Distance.to.nearest.DS,Habitat.Type,Count.Type)
specs = unique(datax$Species)
data2 = data1 %>% slice(rep(1:n(), each = 32))
specs1 = rep(specs,78)
data2$Species = specs1
data = left_join(data2,datax)
data$Count[is.na(data$Count)] = 0
data$Elevation1 = scale(data$Elevation, center = F)
data$Distance.to.nearest.DS1 = scale(data$Distance.to.nearest.DS, center = F)

#looking across all species - using the poisson transformation because this is count data, random effects across species

#fit1 = glmer(Count ~ Elevation1 + Distance.to.nearest.DS1 + Habitat.Type + (1|Species), data = data, family = "poisson")
#summary(fit1) # looks like Urban Built Up has the highest abundances, probably driven by Black Kites! 

#removing commensals - removing Black Kite, Black-eared Kite

datan = data %>% filter(!Species %in% c("Black kite","Black-eared kite"))
#fit2 = glmer(Count ~ Elevation1 + Distance.to.nearest.DS1 + Habitat.Type + (1|Species), data = datan, family = "poisson")
#summary(fit2) # now Forest-Farmland interfaces and Urban built up have the highest abundances

#summarizing across species

datas = data %>%
  group_by(Sampling.site,Elevation1,Distance.to.nearest.DS1,Habitat.Type) %>% summarize(Count = sum(Count))
fit3 = glm(Count ~ Elevation1 + Distance.to.nearest.DS1 + Habitat.Type, data = datas, family = "poisson")
summary(fit3)

#summarizing across species except Kites

datat = datan %>%
  group_by(Sampling.site,Elevation1,Distance.to.nearest.DS1,Habitat.Type) %>% summarize(Count = sum(Count))
fit4 = glm(Count ~ Elevation1 + Distance.to.nearest.DS1 + Habitat.Type, data = datat, family = "poisson")
summary(fit4)
