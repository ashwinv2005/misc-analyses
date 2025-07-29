game = read.csv("C:/Ashwin/All Research and Teaching/Other Studies/Nui/main.csv")

library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(ggfortify)
library(lme4)
library(lmerTest)

theme_set(theme_tufte())

temp = game
temp$game = as.factor(temp$game)
temp$education = as.factor(temp$education)

temp$agem = scale(temp$age,center = F)
temp$familym = scale(temp$family,center = F)
temp$yearsm = scale(temp$years,center = F)
temp$landm = scale(temp$land,center = F)

temp$gamelandm = scale(temp$gameland,center = F)
temp$lantotm = scale(temp$lantot,center = F)

temp$endmonm = scale(temp$endmon,center = F)

fit.logtok = coxme(Surv(logtoktim, logtok, type = "right") ~ (primary)^2 +
                     (1|game), data = temp)
summary(fit.logtok)

fit.cletok = coxme(Surv(cletoktim, cletok, type = "right") ~ (primary)^2 +
                     (1|game), data = temp)
summary(fit.cletok)

fit.huntok = coxme(Surv(huntoktim, huntok, type = "right") ~ (primary)^2 +
                     (1|game), data = temp)
summary(fit.huntok)

fit.oiltok = coxme(Surv(oiltoktim, oiltok, type = "right") ~ (primary)^2 +
                     (1|game), data = temp)
summary(fit.oiltok)

fit.labtok = coxme(Surv(labtoktim, labtok, type = "right") ~ (primary)^2 +
                     (1|game), data = temp)
summary(fit.labtok)

fit = survfit(Surv(cletoktim, cletok, type = "right") ~ current, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = current, ylab = "tendency to clear land based on future land use")

fit = survfit(Surv(oiltoktim, oiltok, type = "right") ~ primary, data = temp, conf.type = "log-log")
autoplot(fit, fun = 'event', col = primary, ylab = "tendency to plant palm oil based on livelihood")


##########

names(temp)

fit = lm(endmonm ~ (padtoktot + fishtoktot + oiltoktot + labtoktot)^2 + lantot, data = temp[temp$labtoktot > 0,])
summary(fit)

fit = lm(endmonm ~ labtoktot, data = temp[temp$labtoktot > 0,])
summary(fit)

fit = lm(endmonm ~ padtoktot:(labtoktot + oiltoktot + fishtoktot) +
           fishtoktot:(labtoktot + oiltoktot) +  
           oiltoktot:(labtoktot) , data = temp)
summary(fit)

fit = lm(endmonm ~ padtoktot + padtoktot:(logtoktot + cletoktot + oiltoktot) + fishtoktot +
           fishtoktot:(logtoktot + cletoktot + oiltoktot) + oiltoktot + 
           oiltoktot:(logtoktot + cletoktot), data = temp[temp$labtoktot == 0,])
summary(fit)

fit = lm(fishtoktot ~ padtoktot + fishtoktot + oiltoktot + labtoktot, data = temp)
summary(fit)

temp$intoillab = temp$oiltoktot*temp$labtoktot
with(temp,plot(endmonm~intoillab, xlab = "interaction between oil palm cultivation and labour", ylab = "money earned in game"))
with(temp,abline(lm(endmonm~intoillab), col = "blue"))

fit = lm(labtoktot ~ primary, data = temp)
summary(fit)

with(temp,boxplot(labtoktot~primary, xlab = "primary livelihood", ylab = "tokens invested in labour"))
