# Niranjana thesis

library(tidyverse)
library(ggthemes)
library(lme4)
library(boot)


theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

clrs = c("#0072B2", "#E69F00")
clrs = c("#de853a","#376a94")

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







############ actual analyses

a = read.csv("niranjana_data.csv")
a = a %>% filter(!is.na(species))
b = read.csv("niranjana_species_family.csv")
a = left_join(a,b)
c = read.csv("nectar_plant.csv")

d = a %>% 
  group_by(species) %>% summarize(count = sum(presence)) %>%
  arrange(desc(count))



## step 1 for all species - testing effects of flowers and host plant presence

temp = a %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence),
            abundance = sum(abundance))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
             data = temp, family=binomial(link = 'logit'))

summary(fit)

fita = glm(abundance~canopy.type + distance.class + season + host + flowers, 
          data = temp, family=poisson(link = 'log'))

summary(fita)


# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()





# within family

# 1) Lycaenidae

temp = a %>% filter(family == "Lycaenidae") %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence),
            abundance = sum(abundance))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
             data = temp, family=binomial(link = 'logit'))
summary(fit)

fita = glm(abundance~canopy.type + distance.class + season + host + flowers, 
          data = temp, family=poisson(link = 'log'))
summary(fita)

# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a Lycaenid butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('Lycaenid_butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()






# 2) Nymphalidae

temp = a %>% filter(family == "Nymphalidae") %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence),
            abundance = sum(abundance))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fita = glm(abundance~canopy.type + distance.class + season + host + flowers, 
           data = temp, family=poisson(link = 'log'))
summary(fita)

# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a Nymphalid butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('Nymphalid_butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()



# 3) Papilionoidae

temp = a %>% filter(family == "Papilionoidae") %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
          data = temp, family=binomial(link = 'logit'))
summary(fit)

# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a Papilionid butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('Papilionid_butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()


# 4) Hesperiidae

temp = a %>% filter(family == "Hesperiidae") %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
          data = temp, family=binomial(link = 'logit'))
summary(fit)

# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a Hesperid butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('Hesperid_butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()


# 5) Pieridae

temp = a %>% filter(family == "Pieridae") %>% group_by(point.name) %>% 
  summarize(presence = sum(presence), total_flowers = sum(total_flowers), 
            flowering.tree = sum(flowering.tree), host.presence = sum(host.presence))

temp1 = a %>% distinct(point.name,canopy.type,distance.class,season)
temp = left_join(temp,temp1)

temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

temp$presence[temp$presence>0] = 1

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
          data = temp, family=binomial(link = 'logit'))
summary(fit)

# simple plotting

newdata1 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Early Season")
newdata2 = data.frame(canopy.type = unique(a$canopy.type), distance.class = "Edge", 
                      host = "Host Absent", flowers = "Flowers Absent", season = "Late Season")

newdata = rbind(newdata1,newdata2)

x = predict(fit,newdata = newdata, type = "link", se = T)

newdata$mean1 = inv.logit(x$fit)
newdata$cil = inv.logit(x$fit-1.96*x$se.fit)
newdata$cir = inv.logit(x$fit+1.96*x$se.fit)

pd = position_dodge(0.2)
ggp = ggplot(data = newdata, aes(x = canopy.type, y = mean1, col = season)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.1, position = pd) +
  xlab("Canopy openness") +
  ylab("Probability of finding a Pierid butterfly")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22, vjust = 1), axis.text.x = element_text(size = 20),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = 'bottom')

jpeg('Pierid_butterfly_presence.jpg', units="in", width=10, height=10, res=1000)
ggp1
dev.off()






# within species

# 1) Common Fourring

temp = a %>% filter(species == "Common Fourring")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
             data = temp, 
             family=binomial(link = 'logit'))
summary(fit)


# 2) Common Sailer

temp = a %>% filter(species == "Common Sailer")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
           data = temp, 
           family=binomial(link = 'logit'))
summary(fit)


# 3) Tiny Grass Blue

temp = a %>% filter(species == "Tiny Grass Blue")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
          data = temp, 
          family=binomial(link = 'logit'))
summary(fit)



# 4) Red Helen

temp = a %>% filter(species == "Red Helen")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + host + flowers, 
          data = temp, 
          family=binomial(link = 'logit'))
summary(fit)


# 5) Common Cerulean

temp = a %>% filter(species == "Common Cerulean")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + flowers, 
          data = temp, 
          family=binomial(link = 'logit'))
summary(fit)


# 6) Chocolate Pansy

temp = a %>% filter(species == "Chocolate Pansy")
temp$flowers = NA
temp$flowers[temp$total_flowers > 0 | temp$flowering.tree!= 0] = "Flowers Present"
temp$flowers[is.na(temp$flowers)] = "Flowers Absent"

temp$host = NA
temp$host[temp$host.presence > 0] = "Host Present"
temp$host[is.na(temp$host)] = "Host Absent"

fit = glm(presence~canopy.type + distance.class + season + flowers, 
          data = temp, 
          family=binomial(link = 'logit'))
summary(fit)
