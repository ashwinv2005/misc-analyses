library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)
library(ggplot2)
library(sp)
library(ggthemes)
theme_set(theme_tufte())

## set date, add month, year and day columns

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

all$OBSERVATION.DATE = as.Date(all$OBSERVATION.DATE)
all$month = month(all$OBSERVATION.DATE)
all$year = year(all$OBSERVATION.DATE)
all$day = day(all$OBSERVATION.DATE) + cdays[all$month]
all$week = week(all$OBSERVATION.DATE)
all$fort = ceiling(all$day/14)

all = all %>%
  filter(SUBNATIONAL1_CODE != "IN-KL" & SUBNATIONAL1_CODE != "IN-KA" & SUBNATIONAL1_CODE != "IN-TN")

allhost = all %>% 
  filter(COMMON.NAME == "Jungle Babbler", APPROVED == 1) %>%
  group_by(group.id) %>% slice(1) %>%
  ungroup

allpar = all %>% 
  filter(APPROVED == 1) %>%
  group_by(group.id) %>% slice(1) %>%
  ungroup

allhost = allhost %>%
  mutate(bre = case_when(BREEDING.BIRD.ATLAS.CODE == "NY" | BREEDING.BIRD.ATLAS.CODE == "NE" | BREEDING.BIRD.ATLAS.CODE == "ON" | BREEDING.BIRD.ATLAS.CODE == "CF" ~ "pre-breeding",
                         BREEDING.BIRD.ATLAS.CODE == "NB" | BREEDING.BIRD.ATLAS.CODE == "CN" | BREEDING.BIRD.ATLAS.CODE == "C" | BREEDING.BIRD.ATLAS.CODE == "N" ~ "breeding",
                         BREEDING.BIRD.ATLAS.CODE == "FL" | BREEDING.BIRD.ATLAS.CODE == "FY" ~ "post-breeding",
                         TRUE ~ "NB"))

hostsum = allhost %>%
  group_by(bre,month) %>% summarize(n = n_distinct(group.id))

parsum = allpar %>%
  group_by(month) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,month) %>% summarize(n = n()/max(lists)) %>%
  filter(COMMON.NAME == "Pied Cuckoo")

names(parsum)[1] = c("bre")
parsum$bre = "cuckoo"

hostpar = expand.grid(c("pre-breeding","breeding","post-breeding","cuckoo"), 1:12)
names(hostpar) = c("bre","month")

hostpar1 = left_join(hostpar,hostsum)
hostpar2 = left_join(hostpar,parsum)
hostpar = rbind(hostpar1[hostpar1$bre != "cuckoo",],hostpar2[hostpar2$bre == "cuckoo",])
hostpar = hostpar %>% mutate(n = case_when(is.na(n) ~ 0,TRUE ~ as.numeric(n)))

hostpar = hostpar %>%
  mutate(bre = case_when(bre == "pre-breeding" ~ "Jungle Babbler pre-nesting codes",
                         bre == "breeding" ~ "Jungle Babbler nesting codes",
                         bre == "post-breeding" ~ "Chicks fledged",
                         TRUE ~ "Pied Cuckoo"))

hostpar$bre = factor(hostpar$bre, levels = c("Jungle Babbler pre-nesting codes","Jungle Babbler nesting codes","Pied Cuckoo","Chicks fledged"))

ggp = ggplot(hostpar, aes(x=month, y=n)) + 
  geom_bar(stat = 'identity', fill = "grey50", col = "grey") + 
  facet_wrap(~bre, nrow = 4, ncol = 1, scales = 'free_y') +
  xlab("month") +
  ylab("freq")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 12), axis.text.y = element_text(size = 10)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 9))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(1:12)) +
  theme(strip.text.x = element_text(size = 12, face = "italic")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  )



