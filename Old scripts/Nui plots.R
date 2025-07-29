library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

usecalc =  function(dt, c, t)
{
  res = summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx =  length(res$surv)
  res =  cbind(surv=1-res$surv, se=2*res$std.err, 
               lcl48=1-res$surv-2*res$std.err,
               ucl48=1-res$surv+2*res$std.err)[idx,]
  return(res)
}

fit.cletok = coxme(Surv(cletoktim, cletok, type = "right") ~ sex + age + edufac + current +
                     (1|game), data = temp)
summary(fit.cletok)

timepoint = 10

newdat = expand.grid(sex="M",
                      age=40,
                     edufac="medium",
                      current=c('Oilpalm', 'Others'))

finaluse = group_by(temp, current) %>%
  summarise(utility = mean(cletok), n=sum(!is.na(cletok)),
            estuse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["surv"],
            lcluse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=cletoktim, c=cletok, t=timepoint)["ucl48"])


ggp = ggplot(finaluse, aes(x=current, y=estuse)) + 
  geom_point( 
    size = 2) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 0.3, width = 0.05
  ) +
  xlab("crop") +
  ylab("likelihood of clearing land")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 7)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  )

library(gridExtra)
library(grid)

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=7, height=5, res=1000)
ggp1
dev.off()


######################################################################


fit.oillan = coxme(Surv(staoiltim, staoil, type = "right") ~ sex + age + edufac + current +
                     (1|game), data = temp)
summary(fit.oillan)

timepoint = 5

newdat = expand.grid(sex="M",
                     age=40,
                     edufac=c('low','medium','high'),
                     current="Oilpalm")

finaluse = group_by(temp, edufac) %>%
  summarise(utility = mean(staoil), n=sum(!is.na(staoil)),
            estuse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["surv"],
            lcluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=staoiltim, c=staoil, t=timepoint)["ucl48"])

finaluse$edufac = factor(finaluse$edufac, levels = c("low","medium","high"))

ggp = ggplot(finaluse, aes(x=edufac, y=estuse)) + 
  geom_point( 
    size = 2) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 0.3, width = 0.05
  ) +
  xlab("education") +
  ylab("likelihood of growing palm oil halfway through")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 7)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  )

library(gridExtra)
library(grid)

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=7, height=5, res=1000)
ggp1
dev.off()


######################################################################

fit.stoppad = coxme(Surv(stoppadtim, stoppad, type = "right") ~ sex + age + edufac + primary +
                      (1|game), data = temp)
summary(fit.stoppad)

timepoint = 5

newdat = expand.grid(sex="M",
                     age=40,
                     edufac=c('medium'),
                     primary=c('Farming', 'Fishing', 'OIlpalm', 'Misc'))

finaluse = group_by(temp, primary) %>%
  summarise(utility = mean(stoppad), n=sum(!is.na(stoppad)),
            estuse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["surv"],
            lcluse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["lcl48"],
            ucluse = usecalc(dt=stoppadtim, c=stoppad, t=timepoint)["ucl48"])

finaluse$primary = factor(finaluse$primary, levels = c("Farming", "Fishing", "OIlpalm", "Misc"))

ggp = ggplot(finaluse, aes(x=primary, y=estuse)) + 
  geom_point( 
    size = 2) +
  geom_errorbar(aes(ymin=lcluse, ymax=ucluse), size = 0.3, width = 0.05
  ) +
  xlab("primary livelihood") +
  ylab("likelihood of stopping paddy cultivation halfway through")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 7)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  )

library(gridExtra)
library(grid)

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=7, height=5, res=1000)
ggp1
dev.off()
