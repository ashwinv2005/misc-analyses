library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)
library(ggplot2)
library(sp)
library(ggthemes)
library(vegan)

dat = read.csv("C:/Users/admin/Desktop/newMDS.csv")
prof = dat[,1]
dat = dat[,-1]
dat1 = dat
dat1 = na.omit(dat1)

theme_set(theme_tufte())

dat1 = dat1[prof != "Local doctor" & prof != "PWD Job",]
prof = prof[prof != "Local doctor" & prof != "PWD Job"]

rank_table = dat1
x = prof
meta_table = data.frame(x)
names(meta_table) = "profession"

grouping_info = data.frame(x)
names(grouping_info) = "profession"
exre = metaMDS(rank_table,distance = "bray", k = 2, trymax = 50)
NMDS=data.frame(x=exre$point[,1],y=exre$point[,2],profession=grouping_info[,1])


NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$profession),mean)

ggp = ggplot(data=NMDS,aes(x,y,col = profession)) +
  geom_point(shape = 16, size = 3, alpha = 0.6) 
#geom_text(data=as.data.frame(df_biofit3*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit3)),
#color="#808080",alpha=1) 

ggp2 = ggp + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14, face = 'bold')) +
  #scale_x_continuous(limits = c(-0.6,0.5)) +
  #scale_y_continuous(limits = c(-0.9,0.4), breaks = seq(-0.8,1,0.3))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )


library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp)
dev.off()
