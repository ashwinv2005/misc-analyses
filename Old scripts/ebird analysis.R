a = read.csv("C:/Users/ashwinv/Desktop/KA-eBird-records-2016-08-06.csv")
names(a)

#a = ebirdclean(a)
#a = clean1speciesdist(a)

wghatsfilter = read.csv("C:/Users/ashwinv/Desktop/wghats1.csv")

x = head(a)

m = spcum(a,proto = "")
plot(m$cumsp~m$cumdur)

n = totspecies(a)
length(n)

vb = ebirdclean(a)
names(vb)
head(vb)

str(vb)
str(gw$count)
unique(vb$district)

wghats = vb[vb$district == "KD" | vb$district == "HS" | vb$district == "CK" | vb$district == "UK" | vb$district == "BG" | vb$district == "SH", ]
write.csv(wghats,"C:/Users/ashwinv/Desktop/wghats.csv")


head(gw)

unique(wghatsfilter$name)
gw = gw[gw$count < 70,]


gw = wghatsfilter[wghatsfilter$name == "Clamorous Reed-Warbler",]
gw[is.na(gw$count),]$count = 0

ggp = ggplot(gw, aes(x=ovday, y=count))  +
  #facet_grid(name ~ ., scale="free_y")+
  #geom_errorbar(aes(ymin=cil*100, ymax=cir*100), width=.1, position=pd, size = 0.6) +
  #geom_line(position=pd, size = 0.6) +
  geom_point(size = 3) +
  xlab("Day") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 16))+
  ##opts(legend.justification=c(1,1), legend.position=c(0.96,0.97)) +
  scale_x_continuous(limits = c(0,350), breaks = c(0,31,59,90,120,151,181,212,243,273,304,334),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))


gw = wghats[wghats$name == "Blue-tailed Bee-eater",]
gw[is.na(gw$count),]$count = 0

ggp = ggplot(gw, aes(x=ovday, y=count))  +
  #facet_grid(name ~ ., scale="free_y")+
  #geom_errorbar(aes(ymin=cil*100, ymax=cir*100), width=.1, position=pd, size = 0.6) +
  #geom_line(position=pd, size = 0.6) +
  geom_point(size = 3) +
  xlab("Day") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 16))+
  ##opts(legend.justification=c(1,1), legend.position=c(0.96,0.97)) +
  scale_x_continuous(limits = c(0,350), breaks = c(0,31,59,90,120,151,181,212,243,273,304,334),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

gw = gw[with(gw, order(month,day)),]
kw = gw[,c(16,18,19,20,21)]
kw = kw[!is.na(kw$month),]
kw = kw[kw$month == 11,]
kw = kw[kw$day>=26 & kw$day<31,]
kw


c = c[-grep("sp.",c, invert = F)]
c = c[-grep("/",c, invert = F)]