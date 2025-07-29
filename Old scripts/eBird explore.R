library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)
library(ggplot2)

print.data.frame(select(temp4[300:500,],group.id,COMMON.NAME,LATITUDE,OBSERVATION.COUNT))

####### India Districts Map #######

indmap = readOGR("C:/Users/admin/Desktop/BCI/Data and shapefiles/India Districts",'IndiaDistricts_2011')
#indmap = spTransform(indmap,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

indmap@

indmapdf = fortify(indmap)

indbasemap = ggplot() +
  geom_path(data = indmapdf, aes(x=long, y=lat, group=group), colour = 'black')+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  coord_map()

