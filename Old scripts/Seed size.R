size = read.csv("C:/Ashwin/Seed and Fruit Size.csv")
arrival = read.csv("C:/Ashwin/Seed arrival.csv")
removal = read.csv("C:/Ashwin/Removal.csv")
sum = read.csv("C:/Ashwin/Summary.csv")
names(size)
names(arrival)
names(removal)
names(sum)
max(size[!is.na(size$fl),]$fl)
seede = seeds[is.na(seeds$fw),]
seede$fw = 0.9518*seede$sw + 7.6723
length(seedf)
length(fruit$fw)

fhist = ggplot(sfhist, aes(w, fill = Type))+
  geom_histogram(binwidth = c(2.5), col = "black")+
  facet_grid(Type ~ ., scale="free_y")+
  xlab("Fruit length (mm)")+
  ylab("Frequency")+
  theme_bw()
fhist1 = fhist+
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 14), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 16))+
  scale_x_continuous(limits = c(20,40))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )+
  theme(strip.text.y = element_text(size = 12, angle = -90))

multiplot(fhist1,ggt1,cols = 2)
sfhist = data.frame(cbind(1:405,0))
sfhist = sfhist[,-c(1,2)]
sfhist$Type = c(rep("Intact",123),rep("Seed dispersed under canopy",282))
sfhist$w = c(fruit$fw,seede$fw)



con = size[size$Type == "C", ]
fig = size[size$Type == "F", ]
drupe = size[size$Type == "D", ]
seed = size[!is.na(size$sw), ]
fruit = size[!is.na(size$fw), ]
summary(size)
seeds = size[!is.na(con$sw),]
length(seeds[seeds$sw > 23 & seeds$sw < 29,]$sw)/length(seeds$sw)
hist(seeds$sw, breaks = seq(14,35,length = 15))
length(fruit$Type)
length(fglmm[fglmm$Type == "Pro",]$Type)

summary(lm(fruit$fl~fruit$fw))
plot(fruseed$fl~fruseed$fw)
fn = "R^2 = 0.5274"
ggs = ggplot(fruit, aes(fw,fl)) + 
geom_point(size = 2.5) + 
stat_smooth(method = "lm", se = F, col = "black") + 
xlab("Fruit Length (mm)") +
ylab("Fruit Diameter (mm)") +
##opts(title = "Fruit Diameter vs Fruit Length") +
theme_bw()+
annotate("text", label = c("a)"), x = 24, y = 24, l = 20)+
annotate("text", label = c("b)"), x = 24, y = 23.5, l = 20)
ggs+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 18))+
  opts(panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
       )

k = lm(fruit$fl~fruit$fw)
abline(k, col = "red")
summary(k)
fruseed = size[!is.na(size$sw & size$fw), ]
length(fruseed$fw)
length(fruit$fw)
length(seed$sw)
plot(fruseed$fw ~ fruseed$sw, main = "Fruit Length vs Seed Length", xlab = "Seed Length", ylab = "Fruit Length")
a = lm(fruseed$fw ~ fruseed$sw)
abline(a, col = "red")
summary(a)
legend(25, 36, bty="n", legend=paste("R sq. = ", 0.8512), cex = 0.8)
legend(25, 37, bty="n", legend=paste("y = 0.9518x + 7.67227"), cex = 0.8)
plot(seed$sw~seed$sl)
plot(fruit$fw~fruit$fl)
cor(fruseed$sw, fruseed$fw)
cor(seed$sw, seed$sl)
cor(fruit$fw, fruit$fl)
plot(fruseed$fl ~ fruseed$sl)
cons = con[!is.na(con$sw), ]
boxplot(cons$sw ~ cons$Name)
par(mfrow = c(4,2))
with(fruit, tapply(X = fw, Name, function(x) hist((x-7.6723)/0.9518))) 
with(cons, tapply(X = sw, Name, function(x) hist(x))) 
hist(cons[is.na(cons$fw), ]$sw)
hist(cons$sw)
hist(fig$sw)
hist(drupe$sw)
hist(fruseed$sw)
disseed = seed[is.na(seed$fw), ]
par(mfrow = c(2,1))
hist(cons[is.na(cons$fw), ]$sw, main = "Seed Length Under Conspecifics", xlab = "Seed Width")
hist(disseed$sw)
fruit$fs = (fruit$fw-7.6723)/0.9518
hist(fruit[is.na(fruit$sw), ]$fs)
hist(fruit$fs, main = "Mean Seed Length Produced by Tree", xlab = "Seed Width")

h = cons[is.na(cons$fw), ]

hist(h$sl)
names(h)
unique(h$Name)
t1 = h[h$Name == "PN1",]$sw
t2 = h[h$Name == "PN2",]$sw
t3 = h[h$Name == "PN4",]$sw
t4 = h[h$Name == "PN5",]$sw
t5 = h[h$Name == "PN6",]$sw
t6 = h[h$Name == "PN8",]$sw
t7 = h[h$Name == "PN9",]$sw
resamplest1 = lapply(1:10000, function(i)sample(t1, replace = T))
resamplest2 = lapply(1:10000, function(i)sample(t2, replace = T))
resamplest3 = lapply(1:10000, function(i)sample(t3, replace = T))
resamplest4 = lapply(1:10000, function(i)sample(t4, replace = T))
resamplest5 = lapply(1:10000, function(i)sample(t5, replace = T))
resamplest6 = lapply(1:10000, function(i)sample(t6, replace = T))
resamplest7 = lapply(1:10000, function(i)sample(t7, replace = T))
r.meant1 = sapply(resamplest1,mean)
r.meant2 = sapply(resamplest2,mean)
r.meant3 = sapply(resamplest3,mean)
r.meant4 = sapply(resamplest4,mean)
r.meant5 = sapply(resamplest5,mean)
r.meant6 = sapply(resamplest6,mean)
r.meant7 = sapply(resamplest7,mean)
for (k in 1:100000)
{
  t = c(sample(r.meant1,1),sample(r.meant2,1),sample(r.meant3,1),sample(r.meant5,1),sample(r.meant6,1))
  resamples[k] = mean(t)
}
r.seedl5 = sapply(resamples,mean)
hcil2 = quantile(r.mean, 0.025)
hcir2 = quantile(r.mean, 0.975)
hm2 = mean(r.mean)
hist(r.mean)

g = fruit
names(g)
g$fs = (fruit$fw-7.6723)/0.9518
hist(g$fs)
unique(g$Name)
v1 = fruit[fruit$Name == "PN1",]$fw
v2 = fruit[fruit$Name == "PN2",]$fw
v3 = fruit[fruit$Name == "PN4",]$fw
v4 = fruit[fruit$Name == "PN6",]$fw
v5 = fruit[fruit$Name == "PN8",]$fw
resamplesv1 = lapply(1:10000, function(i)sample(v1, replace = T))
resamplesv2 = lapply(1:10000, function(i)sample(v2, replace = T))
resamplesv3 = lapply(1:10000, function(i)sample(v3, replace = T))
resamplesv4 = lapply(1:10000, function(i)sample(v4, replace = T))
resamplesv5 = lapply(1:10000, function(i)sample(v5, replace = T))
r.meanv1 = sapply(resamplesv1,mean)
r.meanv2 = sapply(resamplesv2,mean)
r.meanv3 = sapply(resamplesv3,mean)
r.meanv4 = sapply(resamplesv4,mean)
r.meanv5 = sapply(resamplesv5,mean)
for (k in 1:100000)
{
  v = c(sample(r.meanv1,1),sample(r.meanv2,1),sample(r.meanv3,1),sample(r.meanv4,1),sample(r.meanv5,1))
  resamples[k] = mean(v)
}
r.fruitl5 = sapply(resamples,mean)
r.seedfl5 = 0.9518*r.seedl5 + 7.6723
r.seedfl2 = 0.9518*r.seedl2 + 7.6723
hist(r.seedfl5)
gcil5 = quantile(r.fruitl5, 0.025)
gcir5 = quantile(r.fruitl5, 0.975)
hcil5 = quantile(r.seedfl5, 0.025)
hcir5 = quantile(r.seedfl5, 0.975)
gm5 = mean(r.fruitl5)
hm5 = mean(r.seedfl5)
gcil2 = quantile(r.fruitl2, 0.025)
gcir2 = quantile(r.fruitl2, 0.975)
hcil2 = quantile(r.seedfl2, 0.025)
hcir2 = quantile(r.seedfl2, 0.975)
gm2 = mean(r.fruitl2)
hm2 = mean(r.seedfl2)
hist(r.mean)


gp$mean = 0.31196*gp$mean + 11.96858
gp$cil = 0.31196*gp$cil + 11.96858
gp$cir = 0.31196*gp$cir + 11.96858

gp = data.frame(cbind(1:4,0))
gp = gp[,-c(1,2)]
gp$Type = c("Estimated Fruit Dia for Seeds Under Tree","Estimated Fruit Dia for Seeds Under Tree","Estimated Fruit Dia Produced By Tree","Estimated Fruit Dia Produced By Tree")
gp$rem = c("All Trees with Fruit Measurements","During Peak Fruiting","All Trees with Fruit Measurements","During Peak Fruiting")
gp$rem = c("A","B","A","B")
gp$mean = c(hm5,hm2,gm5,gm2)
gp$cil = c(hcil5,hcil2,gcil5,gcil2)
gp$cir = c(hcir5,hcir2,gcir5,gcir2)
vpeakr = ((0.31196*hm2 + 11.96858)^2)*hm2*pi/3
vpeakp = ((0.31196*gm2 + 11.96858)^2)*gm2*pi/3
vpeakp/vpeakr
vpeakp-vpeakr
vallr = ((0.31196*hm5 + 11.96858)^2)*hm5*pi/3
vallp = ((0.31196*gm5 + 11.96858)^2)*gm5*pi/3
vallp/vallr
vallp-vallr
vpeakr/vallr
vpeakr-vallr
vpeakp/vallp
vpeakp-vallp
gm2/gm5
gm2/hm2
gm5/hm5
gp

gt = 0.9518*hm + 7.6723
hm = 0.31196*gt + 11.96858

con

pd = position_dodge(0.1)
gp$Type[c(3,4)] = gp$Type[c(3,4)] = c("Estimated Fruit Dia Overall", "Estimated Fruit Dia during Peak Fruiting") 
gp$rem = rep("B",4)
warnings()
fdg = gp[c(3,4),]
gp$Type = c("Seed dispersed under canopy","Seed dispersed under canopy","Intact","Intact")
fdg$Type = c("Overall","Peak")
names(gp)[1] = "Type"
gp$rem = rep("A",4)
names(gp)[4] = "type"
gpl = gp
gpl$Type[4] = "Fruit peak"
tiff(file = "ss peak.tif", height = 700, width = 500, res = 80)
gpx = gp
gp = gpx
gp$Type = sub("sed ", "sed\n", gp$Type)  
ggt = ggplot(gp[c(3,1),], aes(y=mean, x = rem, linetype = Type)) + 
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=pd, size = 1) +
  geom_point(position=pd, size = 3) +
  ylab("Fruit length (mm)") +
  ##opts(title = "Fruit Size Comparison") +
  theme_bw() 
ggt1 = ggt+
  theme(axis.title.x = element_text(vjust = 0.3, size = 0), axis.text.x = element_text(size = 0), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  ##scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  scale_x_discrete(breaks = NULL)+
  theme(legend.justification=c(1,1), legend.position=c(1,0.95))+
  annotate("text", label = c("b)"), x = 0.6, y = 31.93, l = 20)+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  theme(panel.grid.major = element_blank(),
     panel.grid.minor = element_blank()
     ##panel.border = theme_blank(),
     ##panel.background = theme_blank()
  )

tiff(filename = "c:/Ashwin/All Research and Teaching/Manuscript Images/Seed Size Images/JPE/Figure 3.tiff", 
     res = 1200, width = 14349, height = 9200)
multiplot(ggt1,fhist2,cols = 2, layout = matrix(c(2,2,2,2,2,1,1,1), nrow=1, byrow=TRUE))
dev.off()

png(filename = "Figure 3.png")

ann_text = data.frame(w = 20,lab = "a)",
                       Type = factor("Intact",levels = c("Intact","Seed dispersed under canopy")))
fhist2 = fhist1 + geom_text(data = ann_text,y = 38,label = "a)")
hist(seed$sw)

badia = c(15.15, 17.59, 16.02, 17.47, 17.02, 17.25, 18.18, 14.65, 14.28, 16.81)
mean(badia)
sd(badia)/sqrt(10)


sd(fglmm[fglmm$Type == "Pro",]$fl)

fglmm = read.csv("C:/Ashwin/CSV/GLMM.csv")
names(fglmm)[3] = "sl"
fglmm = fglmm[fglmm$Name != "PN9" & fglmm$Name != "PN5",]
fglmm$fw = 0
fglmm[fglmm$Type == "Reg",]$fw = 11.96858 + 0.31196 * fglmm[fglmm$Type == "Reg",]$fl
fglmm[fglmm$Type == "Pro",]$fw = fruit$fl

fglmm$Typen = 0
fglmm[fglmm$Type == "Reg",]$Typen = 1

frse1 = lmer(fl ~ Type + (1|Name), data = fglmm)
frse2 = lmer(fl ~ (1|Name), data = fglmm)
frse3 = glm(fl ~ Type, data = fglmm)
frse1w = lmer(fw ~ Type + (1|Name), data = fglmm)
frse2w = lmer(fw ~ (1|Name), data = fglmm)
frse3w = glm(fw ~ Type, data = fglmm)

summary(frse1w)
anova(frse1)
anova(frse1w, frse2w)
plot(fl ~ Type, data = fglmm)
names(fglmm)

summary(glm(fw~Type, data = fglmm))
grpf = groupedData(fl ~ Type | Name, data = fglmm)
