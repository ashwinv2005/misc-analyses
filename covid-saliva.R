library(Hmisc)
library(tidyverse)
library(ggthemes)
library(boot)
theme_set(theme_tufte())

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)




#a = read.csv("covid-saliva.csv")
a = read.csv("covid-saliva-final.csv")
a = a[!is.na(a$days),]
a = a[a$truth != "Inconclusive",]
a$test = 0
a$test[a$truth == a$saliva] = 1

a$daybins = "3-15"
a$daybins[a$days %in% c(0,1,2)] = "0-2"


a$daybins = factor(a$daybins, levels = c("0-2","3-15"))


a1 = a[a$truth == "Positive",]
a1$severity[a1$severity == "Asymptomatic"] = "Asymptomatic/Mild"
a1$severity[a1$severity == "Mild"] = "Asymptomatic/Mild"


fit1 = glm(test~sex+age+daybins+severity, data = a1, family = "binomial")
summary(fit1)

b1 = a1 %>%
  group_by(daybins) %>% summarize(prop = mean(na.omit(test)),
                                  sample.size = n(),
                                  prop.se = sd(na.omit(test))/sqrt(sample.size))
b1$sex = "Female"
b1$age = median(na.omit(a$age))
b1$severity = "Symptomatic"

c1 = predict(fit1,newdata = b1, type = "link", se = T)

b1$pred = c1$fit
b1$pred.ci = c1$se.fit


e1 = b1 %>% select(daybins,sample.size,pred,pred.ci)
e1$type = "data"

f1 = b1 %>% select(daybins,sample.size,pred,pred.ci)
names(f1) = c("daybins","sample.size","prop","prop.ci")
f1$type = "predicted"

g1 = rbind(f1)

g1$cil = g1$prop - g1$prop.ci
g1$cir = g1$prop + g1$prop.ci

g1$prop = inv.logit(g1$prop)
g1$cil = inv.logit(g1$cil)
g1$cir = inv.logit(g1$cir)

g1$sample.size1 = c("","*")

  
#pd = position_dodge(0.4)   

ggp = ggplot(data = g1, aes(x = daybins, y = prop)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5, width = 0.1) +
  geom_text(aes(label=sample.size1),hjust=-1, vjust=0, col = "black", size = 20)+
  #geom_smooth(aes(x = days, y = pred)) +
  xlab("Time between\ncollection and testing (days)") +
  ylab("Probability of success")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 30, vjust = 1), axis.text.x = element_text(size = 35),
        axis.title.y = element_text(angle = 90, size = 30), axis.text.y = element_text(size = 35)) +
  theme(text=element_text(family="Gill Sans MT")) +
  #coord_fixed(ratio=5) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) 

jpeg('covid_saliva.jpg', units="in", width=6, height=10, res=1000)
ggp1
dev.off()

pdf('covid_saliva.pdf', width=6, height=10)
ggp1
dev.off()



a = read.csv("covid-saliva-final.csv")
a = a[!is.na(a$days),]
a = a[a$truth != "Inconclusive",]
a$test = 0
a$test[a$truth == a$nps] = 1

a$daybins = "3-15"
a$daybins[a$days %in% c(0,1,2)] = "0-2"


a$daybins = factor(a$daybins, levels = c("0-2","3-15"))


a1 = a[a$truth == "Positive",]
a1$severity[a1$severity == "Asymptomatic"] = "Asymptomatic/Mild"
a1$severity[a1$severity == "Mild"] = "Asymptomatic/Mild"


fit2 = glm(test~sex+age+daybins+severity, data = a1, family = "binomial")
summary(fit2)

b1 = a1 %>%
  group_by(daybins) %>% summarize(prop = mean(na.omit(test)),
                                  sample.size = n(),
                                  prop.se = sd(na.omit(test))/sqrt(sample.size))
b1$sex = "Female"
b1$age = median(na.omit(a$age))
b1$severity = "Symptomatic"

c1 = predict(fit2,newdata = b1, type = "response", se = T)

b1$pred = c1$fit
b1$pred.se = c1$se.fit

e1 = b1 %>% select(daybins,sample.size,prop,prop.se)
e1$type = "data"

f1 = b1 %>% select(daybins,sample.size,pred,pred.se)
names(f1) = c("daybins","sample.size","prop","prop.se")
f1$type = "predicted"

g1 = rbind(f1)

g1$sel = g1$prop - g1$prop.se
g1$ser = g1$prop + g1$prop.se

g1$sample.size1 = c("N=49","N=46 (*)")


#pd = position_dodge(0.4)   

ggp = ggplot(data = g1, aes(x = daybins, y = prop)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = sel, ymax = ser), size = 0.5, width = 0.2) +
  geom_text(aes(label=sample.size1),hjust=-1.2, vjust=0, col = "black")+
  #geom_smooth(aes(x = days, y = pred)) +
  xlab("Time between collection and testing (days)") +
  ylab("Probability of success - NPS")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15))

jpeg('covid_nps.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()







