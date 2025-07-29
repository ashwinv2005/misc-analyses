comp = rbind(atlaseBird[,c(1,3)],atlaseBird[,c(1,3)],atlaseBird[,c(1,3)])
comp$type = factor(c(rep("state average",330),rep("district average",330),rep("model",330)), levels = c("state average","district average","model"))
comp$eBird = c(atlaseBird$eBird_state,atlaseBird$eBird_dist,atlaseBird$lla)
comp$resid = comp$eBird - comp$atlas_state
comp$propresid = (comp$eBird - comp$atlas_state)/comp$atlas_state


temp = comp
temp$sqresid = temp$propresid^2

meandev = temp %>% filter(!is.na(sqresid)) %>%
  group_by(type) %>% summarize(ssr = sum(sqresid)/n())

meandev = comp %>% filter(!is.na(propresid)) %>%
  group_by(type) %>% summarize(ssr = sum(propresid))

temp$type = factor(c(rep("state average\nsq.prop.dev = 5.15",330),rep("district average\nsq.prop.dev = 76.1",330),rep("model\nsq.prop.dev = 0.648",330)), levels = c("state average\nsq.prop.dev = 5.15","district average\nsq.prop.dev = 76.1","model\nsq.prop.dev = 0.648"))

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(temp, aes(x=atlas_state, y=eBird)) + 
  geom_abline(intercept = 0, slope = 1, col = "blue") + 
  facet_wrap(~type, nrow = 3, ncol = 1) +
  geom_point(size = 1) +
  xlab("freq. of detection - KL atlas") +
  ylab("freq. of detection - eBird")

ggp +
  theme(axis.title.x = element_text(vjust = 1, size = 12), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 0.5, angle = 90, size = 12), axis.text.y = element_text(size = 10)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 9))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 10)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) 

dev.off()


tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(comp, aes(x=atlas_state, y=resid)) + 
  geom_abline(intercept = 0, slope = 0, col = "blue") + 
  facet_wrap(~type, nrow = 1, ncol = 3) +
  geom_point(size = 1) +
  xlab("freq. of detection - KL atlas") +
  ylab("residuals - eBird")

ggp +
  theme(axis.title.x = element_text(vjust = 1, size = 12), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 0.5, angle = 90, size = 12), axis.text.y = element_text(size = 10)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 9))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 10)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) 

dev.off()


tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(comp, aes(x=atlas_state, y=propresid)) + 
  geom_abline(intercept = 0, slope = 0, col = "blue") + 
  facet_wrap(~type, nrow = 1, ncol = 3) +
  geom_point(size = 1) +
  xlab("freq. of detection - KL atlas") +
  ylab("proportional residuals - eBird")

ggp +
  theme(axis.title.x = element_text(vjust = 1, size = 12), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 0.5, angle = 90, size = 12), axis.text.y = element_text(size = 10)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 9))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-2,25)) +
  theme(strip.text.x = element_text(size = 10)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) 

dev.off()



## investigate relationships between probability of occurence (detection) and different list parameters

## by no.sp


llasum = all %>%
  group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,no.sp,lists) %>% summarize(n = n(),freq = n()/max(lists)) %>%
  ungroup

llasum$COMMON.NAME = factor(llasum$COMMON.NAME, levels = selectsp)

temp = llasum %>% filter(COMMON.NAME %in% selectsp[76:100])

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(temp, aes(x = no.sp, y = freq))  +
  facet_wrap(~COMMON.NAME, scale = "free_y") +
  geom_point(size = 0.1) +
  geom_smooth(method = "loess", alpha = 0.1,size = 0.5) +
  xlab("no of species in list") +
  ylab("frequency of detection") 
ggp + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 10), axis.text.x = element_text(size = 6),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 6)) +
  theme(strip.text.x = element_text(size = 8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  #coord_cartesian(ylim=c(0, 23)) +
  scale_size(guide = 'none') +
  #scale_x_log10() +
  theme(legend.position = "none")

dev.off()



## by duration


llasum = all %>%
  group_by(DURATION.MINUTES) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,DURATION.MINUTES,lists) %>% summarize(n = n(),freq = n()/max(lists)) %>%
  ungroup

llasum$COMMON.NAME = factor(llasum$COMMON.NAME, levels = selectsp)

temp = llasum %>% filter(COMMON.NAME %in% selectsp[76:100])

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(temp, aes(x = DURATION.MINUTES, y = freq))  +
  facet_wrap(~COMMON.NAME, scale = "free_y") +
  #geom_point(size = 0.1) +
  geom_smooth(method = "loess", alpha = 0.1,size = 0.5) +
  xlab("list duration") +
  ylab("frequency of detection") 
ggp + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 10), axis.text.x = element_text(size = 6),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 6)) +
  theme(strip.text.x = element_text(size = 8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  #coord_cartesian(ylim=c(0, 23)) +
  scale_size(guide = 'none') +
  #scale_x_log10() +
  theme(legend.position = "none")

dev.off()




## by distance

all$rounddist = round(all$EFFORT.DISTANCE.KM,1)

llasum = all %>%
  group_by(rounddist) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,rounddist,lists) %>% summarize(n = n(),freq = n()/max(lists)) %>%
  ungroup

llasum$COMMON.NAME = factor(llasum$COMMON.NAME, levels = selectsp)

temp = llasum %>% filter(COMMON.NAME %in% selectsp[51:75])

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)

ggp = ggplot(temp, aes(x = rounddist, y = freq))  +
  facet_wrap(~COMMON.NAME, scale = "free_y") +
  #geom_point(size = 0.1) +
  geom_smooth(method = "loess", alpha = 0.1,size = 0.5) +
  xlab("distance") +
  ylab("frequency of detection") 
ggp + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 10), axis.text.x = element_text(size = 6),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 6)) +
  theme(strip.text.x = element_text(size = 8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  #coord_cartesian(ylim=c(0, 23)) +
  scale_size(guide = 'none') +
  #scale_x_log10() +
  theme(legend.position = "none")

dev.off()