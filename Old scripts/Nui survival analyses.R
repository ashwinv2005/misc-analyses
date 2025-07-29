library(survival)
library(coxme)
library(ggthemes)
library(tidyverse)
library(ggfortify)
library(lme4)
library(lmerTest)

theme_set(theme_tufte())

temp = game

temp$agem = scale(temp$age,center = F)
temp$familym = scale(temp$family,center = F)
temp$yearsm = scale(temp$years,center = F)
temp$landm = scale(temp$land,center = F)
temp$gamelandm = scale(temp$gameland,center = F)
temp$lantotm = scale(temp$lantot,center = F)
temp$endmonm = scale(temp$endmon,center = F)

temp$edufac = temp$education
temp[temp$education == 2,]$edufac = "low"
temp[temp$education == 3,]$edufac = "medium"
temp[temp$education == 4 | temp$education == 5,]$edufac = "high"
temp$edufac = as.factor(temp$edufac)

temp$agefac = "age2"
temp[temp$age <= 32,]$agefac = "age1"
temp[temp$age >= 41,]$agefac = "age3"
temp[temp$age >= 49,]$agefac = "age4"
temp$agefac = as.factor(temp$agefac)

#### token clearing

fit.cletok = coxme(Surv(cletoktim, cletok, type = "right") ~ sex + age + edufac + current +
                     (1|game), data = temp)
summary(fit.cletok)

fit = survfit(Surv(cletoktim, cletok, type = "right") ~ current, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = current, ylab = "tendency to clear land based on current crop")

#### token palm oil

fit.oiltok = coxme(Surv(oiltoktim, oiltok, type = "right") ~ sex + age + edufac + current +
                     (1|game), data = temp)
summary(fit.oiltok)

fit = survfit(Surv(oiltoktim, oiltok, type = "right") ~ edufac, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = edufac, ylab = "tendency to plant oil palm based on education")

#### landuse palm oil

fit.oillan = coxme(Surv(staoiltim, staoil, type = "right") ~ sex + age + edufac + current +
                     (1|game), data = temp)
summary(fit.oillan)

fit = survfit(Surv(oiltoktim, oiltok, type = "right") ~ edufac, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = edufac, ylab = "tendency to convert land to oil palm based on education")





#######################################################


## are paddy growers most likely to persist with paddy?

#### landuse stop paddy

fit.stoppad = coxme(Surv(stoppadtim, stoppad, type = "right") ~ sex + age + edufac + primary +
                      (1|game), data = temp)
summary(fit.stoppad)

fit = survfit(Surv(stoppadtim, stoppad, type = "right") ~ primary, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = primary, ylab = "tendency to stop growing paddy")



#######################################################


#### tokens that make money, drivers of the token use


