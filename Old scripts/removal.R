removal = read.csv("C:/Ashwin/Removal.csv")
prun = removal[removal$Type == "C", ]
fic = removal[removal$Type == "F", ]
lar = removal[removal$Type == "D", ]
eme = removal[removal$Type == "E", ]

names(removal)

n = length(unique(removal[removal$Tree == "PN1", ]$Date))
PNR1 = data.frame(cbind(1:n, 0))
PNR1 = PNR1[, -c(1,2)]
PNR1$Date = unique(removal[removal$Tree == "PN1", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR1[i,(j-1)*2] = sum(removal[removal$Tree == "PN1" & removal$Class == j-2 & removal$Date == PNR1$Date[i], ]$Rem)
PNR1[i, 2*j-1] = sum(removal[removal$Tree == "PN1" & removal$Class == j-2 & removal$Date == PNR1$Date[i], ]$Ori)
}}
names(PNR1)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN4", ]$Date))
PNR4 = data.frame(cbind(1:n, 0))
PNR4 = PNR4[, -c(1,2)]
PNR4$Date = unique(removal[removal$Tree == "PN4", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR4[i,(j-1)*2] = sum(removal[removal$Tree == "PN4" & removal$Class == j-2 & removal$Date == PNR4$Date[i], ]$Rem)
PNR4[i, 2*j-1] = sum(removal[removal$Tree == "PN4" & removal$Class == j-2 & removal$Date == PNR4$Date[i], ]$Ori)
}}
names(PNR4)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN5", ]$Date))
PNR5 = data.frame(cbind(1:n, 0))
PNR5 = PNR5[, -c(1,2)]
PNR5$Date = unique(removal[removal$Tree == "PN5", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR5[i,(j-1)*2] = sum(removal[removal$Tree == "PN5" & removal$Class == j-2 & removal$Date == PNR5$Date[i], ]$Rem)
PNR5[i, 2*j-1] = sum(removal[removal$Tree == "PN5" & removal$Class == j-2 & removal$Date == PNR5$Date[i], ]$Ori)
}}
names(PNR5)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN6", ]$Date))
PNR6 = data.frame(cbind(1:n, 0))
PNR6 = PNR6[, -c(1,2)]
PNR6$Date = unique(removal[removal$Tree == "PN6", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR6[i,(j-1)*2] = sum(removal[removal$Tree == "PN6" & removal$Class == j-2 & removal$Date == PNR6$Date[i], ]$Rem)
PNR6[i, 2*j-1] = sum(removal[removal$Tree == "PN6" & removal$Class == j-2 & removal$Date == PNR6$Date[i], ]$Ori)
}}
names(PNR6)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN7", ]$Date))
PNR7 = data.frame(cbind(1:n, 0))
PNR7 = PNR7[, -c(1,2)]
PNR7$Date = unique(removal[removal$Tree == "PN7", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR7[i,(j-1)*2] = sum(removal[removal$Tree == "PN7" & removal$Class == j-2 & removal$Date == PNR7$Date[i], ]$Rem)
PNR7[i, 2*j-1] = sum(removal[removal$Tree == "PN7" & removal$Class == j-2 & removal$Date == PNR7$Date[i], ]$Ori)
}}
names(PNR7)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN8", ]$Date))
PNR8 = data.frame(cbind(1:n, 0))
PNR8 = PNR8[, -c(1,2)]
PNR8$Date = unique(removal[removal$Tree == "PN8", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR8[i,(j-1)*2] = sum(removal[removal$Tree == "PN8" & removal$Class == j-2 & removal$Date == PNR8$Date[i], ]$Rem)
PNR8[i, 2*j-1] = sum(removal[removal$Tree == "PN8" & removal$Class == j-2 & removal$Date == PNR8$Date[i], ]$Ori)
}}
names(PNR8)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "PN9", ]$Date))
PNR9 = data.frame(cbind(1:n, 0))
PNR9 = PNR9[, -c(1,2)]
PNR9$Date = unique(removal[removal$Tree == "PN9", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
PNR9[i,(j-1)*2] = sum(removal[removal$Tree == "PN9" & removal$Class == j-2 & removal$Date == PNR9$Date[i], ]$Rem)
PNR9[i, 2*j-1] = sum(removal[removal$Tree == "PN9" & removal$Class == j-2 & removal$Date == PNR9$Date[i], ]$Ori)
}}
names(PNR9)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "FS1", ]$Date))
FSR1 = data.frame(cbind(1:n, 0))
FSR1 = FSR1[, -c(1,2)]
FSR1$Date = unique(removal[removal$Tree == "FS1", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FSR1[i,(j-1)*2] = sum(removal[removal$Tree == "FS1" & removal$Class == j-2 & removal$Date == FSR1$Date[i], ]$Rem)
FSR1[i, 2*j-1] = sum(removal[removal$Tree == "FS1" & removal$Class == j-2 & removal$Date == FSR1$Date[i], ]$Ori)
}}
names(FSR1)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "FS2", ]$Date))
FSR2 = data.frame(cbind(1:n, 0))
FSR2 = FSR2[, -c(1,2)]
FSR2$Date = unique(removal[removal$Tree == "FS2", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FSR2[i,(j-1)*2] = sum(removal[removal$Tree == "FS2" & removal$Class == j-2 & removal$Date == FSR2$Date[i], ]$Rem)
FSR2[i, 2*j-1] = sum(removal[removal$Tree == "FS2" & removal$Class == j-2 & removal$Date == FSR2$Date[i], ]$Ori)
}}
names(FSR2)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "FS3", ]$Date))
FSR3 = data.frame(cbind(1:n, 0))
FSR3 = FSR3[, -c(1,2)]
FSR3$Date = unique(removal[removal$Tree == "FS3", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FSR3[i,(j-1)*2] = sum(removal[removal$Tree == "FS3" & removal$Class == j-2 & removal$Date == FSR3$Date[i], ]$Rem)
FSR3[i, 2*j-1] = sum(removal[removal$Tree == "FS3" & removal$Class == j-2 & removal$Date == FSR3$Date[i], ]$Ori)
}}
names(FSR3)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "FS5", ]$Date))
FSR5 = data.frame(cbind(1:n, 0))
FSR5 = FSR5[, -c(1,2)]
FSR5$Date = unique(removal[removal$Tree == "FS5", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FSR5[i,(j-1)*2] = sum(removal[removal$Tree == "FS5" & removal$Class == j-2 & removal$Date == FSR5$Date[i], ]$Rem)
FSR5[i, 2*j-1] = sum(removal[removal$Tree == "FS5" & removal$Class == j-2 & removal$Date == FSR5$Date[i], ]$Ori)
}}
names(FSR5)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "ECC1", ]$Date))
ECCR1 = data.frame(cbind(1:n, 0))
ECCR1 = ECCR1[, -c(1,2)]
ECCR1$Date = unique(removal[removal$Tree == "ECC1", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
ECCR1[i,(j-1)*2] = sum(removal[removal$Tree == "ECC1" & removal$Class == j-2 & removal$Date == ECCR1$Date[i], ]$Rem)
ECCR1[i, 2*j-1] = sum(removal[removal$Tree == "ECC1" & removal$Class == j-2 & removal$Date == ECCR1$Date[i], ]$Ori)
}}
names(ECCR1)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "ECC2", ]$Date))
ECCR2 = data.frame(cbind(1:n, 0))
ECCR2 = ECCR2[, -c(1,2)]
ECCR2$Date = unique(removal[removal$Tree == "ECC2", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
ECCR2[i,(j-1)*2] = sum(removal[removal$Tree == "ECC2" & removal$Class == j-2 & removal$Date == ECCR2$Date[i], ]$Rem)
ECCR2[i, 2*j-1] = sum(removal[removal$Tree == "ECC2" & removal$Class == j-2 & removal$Date == ECCR2$Date[i], ]$Ori)
}}
names(ECCR2)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "ECC3", ]$Date))
ECCR3 = data.frame(cbind(1:n, 0))
ECCR3 = ECCR3[, -c(1,2)]
ECCR3$Date = unique(removal[removal$Tree == "ECC3", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
ECCR3[i,(j-1)*2] = sum(removal[removal$Tree == "ECC3" & removal$Class == j-2 & removal$Date == ECCR3$Date[i], ]$Rem)
ECCR3[i, 2*j-1] = sum(removal[removal$Tree == "ECC3" & removal$Class == j-2 & removal$Date == ECCR3$Date[i], ]$Ori)
}}
names(ECCR3)[2:11] = c("r0","o0","r1","o1","r2","o2","r3","o3","r4","o4")

n = length(unique(removal[removal$Tree == "AIL3", ]$Date))
AILR3 = data.frame(cbind(1:n, 0))
AILR3 = AILR3[, -c(1,2)]
AILR3$Date = unique(removal[removal$Tree == "AIL3", ]$Date)
AILR3$s0 = removal[removal$Tree == "AIL3" & removal$Section == "Below", ]$Rem
AILR3$o0 = removal[removal$Tree == "AIL3" & removal$Section == "Below", ]$Ori
names(AILR3)[2:3] = c("r0","o0")

n = length(unique(removal[removal$Tree == "AIL4", ]$Date))
AILR4 = data.frame(cbind(1:n, 0))
AILR4 = AILR4[, -c(1,2)]
AILR4$Date = unique(removal[removal$Tree == "AIL4", ]$Date)
AILR4$s0 = removal[removal$Tree == "AIL4" & removal$Section == "Below", ]$Rem
AILR4$o0 = removal[removal$Tree == "AIL4" & removal$Section == "Below", ]$Ori
names(AILR4)[2:3] = c("r0","o0")

n = length(unique(removal[removal$Tree == "AIL5", ]$Date))
AILR5 = data.frame(cbind(1:n, 0))
AILR5 = AILR5[, -c(1,2)]
AILR5$Date = unique(removal[removal$Tree == "AIL5", ]$Date)
AILR5$s0 = removal[removal$Tree == "AIL5" & removal$Section == "Below", ]$Rem
AILR5$o0 = removal[removal$Tree == "AIL5" & removal$Section == "Below", ]$Ori
names(AILR5)[2:3] = c("r0","o0")

PNR1
PNR4
PNR5
PNR5$r0[5] = 5
PNR6
PNR6$r0[2:4] = c(0,0,0)
PNR7
PNR7$r0[4:5] = c(7,7)
PNR7$r3[4:5] = c(7,7)
PNR8
PNR8$r0[4] = 3
PNR9
PNR9$r0[3] = 1

FSR1
FSR2
FSR3
FSR5

ECCR1
ECCR2
ECCR3

AILR3$r1 = AILR3$o1 = AILR3$r2 = AILR3$o2 = AILR3$r3 = AILR3$o3 = AILR3$r4 = AILR3$o4 = NA
AILR4$r1 = AILR4$o1 = AILR4$r2 = AILR4$o2 = AILR4$r3 = AILR4$o3 = AILR4$r4 = AILR4$o4 = NA
AILR5$r1 = AILR5$o1 = AILR5$r2 = AILR5$o2 = AILR5$r3 = AILR5$o3 = AILR5$r4 = AILR5$o4 = NA


remsum = rbind(PNR1,PNR4,PNR5,PNR6,PNR7,PNR8,PNR9,FSR1,FSR2,FSR3,FSR5,ECCR1,ECCR2,ECCR3,AILR3,AILR4,AILR5)
remsum1 = data.frame(cbind(1:59,0))
remsum1 = remsum1[,-c(1,2)]
remsum1$Date = remsum$Date
remsum1$p0 = remsum$r0/remsum$o0
remsum1$p1 = remsum$r1/remsum$o1
remsum1$p2 = remsum$r2/remsum$o2
remsum1$p3 = remsum$r3/remsum$o3
remsum1$p4 = remsum$r4/remsum$o4

sursum = data.frame(cbind(1:17,0))
sursum = sursum[,-c(1,2)]
sursum$Tree = c("PN1","PN4","PN5","PN6","PN7","PN8","PN9","FS1","FS2","FS3","FS5","ECC1","ECC2","ECC3","AIL3","AIL4","AIL5")
sursum$Class = c(rep("C",7),rep("F",4),rep("D",3),rep("E",3))
sursum$p0 = remsum1[c(3,8,13,17,22,26,29,31,35,39,41,44,47,50,53,56,59),]$p0
sursum$p1 = remsum1[c(3,8,13,17,22,26,29,31,35,39,41,44,47,50,53,56,59),]$p1
sursum$p2 = remsum1[c(3,8,13,17,22,26,29,31,35,39,41,44,47,50,53,56,59),]$p2
sursum$p3 = remsum1[c(3,8,13,17,22,26,29,31,35,39,41,44,47,50,53,56,59),]$p3
sursum$p4 = remsum1[c(3,8,13,17,22,26,29,31,35,39,41,44,47,50,53,56,59),]$p4
boxplot(sursum$p4~sursum$Class)

surs = data.frame(cbind(1:5,0))
surs = surs[, -c(1,2)]
surs$sl = c(1:5)
sur = t(sursum)
sur = sur[-c(1,2),]
for(i in 2:15)
{
surs[,i] = sur[, i-1]
}
names(surs) = c("sl", sursum$Tree)
surs

boxplot(sursum[sursum$Class == "C",]$p0, at = 1, xlim = c(0,16), col = "red", ylim = c(0,1), xaxt = "n", xlab = "Tree", ylab = "Seed Survival", main = "Comparison Across Treatments") 
boxplot(sursum[sursum$Class == "C",]$p1, at = 2, xaxt = "n", add = TRUE, col = "red")
boxplot(sursum[sursum$Class == "C",]$p2, at = 3, xaxt = "n", add = TRUE, col = "red")
boxplot(sursum[sursum$Class == "C",]$p3, at = 4, xaxt = "n", add = TRUE, col = "red")
boxplot(sursum[sursum$Class == "C",]$p4, at = 5, xaxt = "n", add = TRUE, col = "red")
boxplot(sursum[sursum$Class == "F",]$p0, at = 6, xaxt = "n", add = TRUE, col = "green")
boxplot(sursum[sursum$Class == "F",]$p1, at = 7, xaxt = "n", add = TRUE, col = "green")
boxplot(sursum[sursum$Class == "F",]$p2, at = 8, xaxt = "n", add = TRUE, col = "green")
boxplot(sursum[sursum$Class == "F",]$p3, at = 9, xaxt = "n", add = TRUE, col = "green")
boxplot(sursum[sursum$Class == "F",]$p4, at = 10, xaxt = "n", add = TRUE, col = "green")
boxplot(sursum[sursum$Class == "D",]$p0, at = 11, xaxt = "n", add = TRUE, col = "blue")
boxplot(sursum[sursum$Class == "D",]$p1, at = 12, xaxt = "n", add = TRUE, col = "blue")
boxplot(sursum[sursum$Class == "D",]$p2, at = 13, xaxt = "n", add = TRUE, col = "blue")
boxplot(sursum[sursum$Class == "D",]$p3, at = 14, xaxt = "n", add = TRUE, col = "blue")
boxplot(sursum[sursum$Class == "D",]$p4, at = 15, xaxt = "n", add = TRUE, col = "blue")

legend(7, 0.3, bty = "n", c("Prunus", "Ficus", "Large fruits"), col = c("red", "green", "blue"), pch = 19, cex = 0.8)
axis(1, at=seq(1, 15, by=1), labels = c("Below","4m","8m","12m","16m","Below","4m","8m","12m","16m","Below","4m","8m","12m","16m"), srt = 45, cex = 0.8)

plot(1:5,surs$PN1, type = "b", col = "red", xlim = c(0,36), ylim = c(0,1), xlab = "Tree", ylab = "Survival", main = "Comparison Across Conspecifics")
lines(6:10,surs$PN4, type = "b", col = "red")
lines(11:15,surs$PN5, type = "b", col = "red")
lines(16:20,surs$PN6, type = "b", col = "red")
lines(21:25,surs$PN7, type = "b", col = "red")
lines(26:30,surs$PN8, type = "b", col = "red")
lines(31:35,surs$PN9, type = "b", col = "red")

surlong = data.frame(cbind(1:73),0)
surlong = surlong[,-c(1,2)]
surlong$Tree = c(sursum$Tree, rep(sursum$Tree[1:14],4))
surlong$Class = c(sursum$Class,rep(sursum$Class[1:14],4))
surlong$Section = c(rep(0,17),rep(1,14),rep(2,14),rep(3,14),rep(4,14))
surlong$Survival = c(sursum$p0,sursum$p1[1:14],sursum$p2[1:14],sursum$p3[1:14],sursum$p4[1:14])

fj = summarySE(surlongf, measurevar="Survival", groupvars=c("Class","Section"))

write.csv(fj, "C:/Ashwin/Ashwin/Py.csv")
fj$rem = 1-fj$meanb
fj$reml = 1-fj$cil
fj$remr = 1-fj$cir

library(ggplot2)
pd = position_dodge(.3)

ggpr = ggplot(fj[1:16,], aes(x=Section, y=rem, linetype=Class)) + 
    geom_errorbar(aes(ymin=reml, ymax=remr), width=.1, position=pd, size = 1) +
    geom_point(position=pd, size = 3) +
    xlab("Distance class (m)") +
    ylab("Probability of removal/predation") +
    scale_colour_hue(name="Tree type", 
                     breaks=c("C", "F", "D","E"),
                     labels=c(" Conspecifics", "Figs", "Heterospecific non-figs", "Emergents"),
                     l=40) +
    ##opts(title = "Seed Survival in Different Treatments") +
    theme_bw() 
ggpr+
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.justification=c(1,1), legend.position=c(0.96,1)) +
  scale_x_continuous(expand = c(0.023,0.023),limits = c(-0.2,4.2), breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16")) +
  ##scale_y_continuous(limits = c(0,0.027))+
  ##scale_shape_manual(values=c(1,2,3,4),
    ##                 name="", 
    ##                 breaks=c("C","D","E","F"),
    ##                 labels=c("  Conspecifics", "  Heterospecifics: non-figs", "  Emergents", "  Heterospecifics: figs")) + 
  scale_linetype_manual(values = c(1,3,10,5),
                        name="", 
                        breaks=c("C","D","E","F"),
                        labels=c("  Conspecifics", "  Heterospecifics: non-figs", "  Emergents", "  Heterospecifics: figs"))+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ##panel.border = theme_blank(),
      ##panel.background = theme_blank()
      )                       
cla = c("C","D","E","F")
sec = c(0,1,2,3,4)
co = 0
for (i in 1:4){
for (j in 1:5){
co = co + 1
bb = surlongf[surlongf$Class == cla[i] & surlongf$Section == sec[j],]$Survival
resamples = lapply(1:10000, function(i)sample(bb, replace = T))
r.mean = sapply(resamples,mean)
fj$cil[co] = quantile(r.mean,0.025)
fj$cir[co] = quantile(r.mean,0.975)
fj$meanb[co] = mean(r.mean)
if (co == 11)
  break
}
}

(1-fj$Survival[1])/(1-mean(fj$Survival[-1]))

indtrees = 1 - surlong$Survival
combtrees = 1 - fj$Survival

names(removal)
names(surlong)
removalf = removal[removal$Tree != "PN5" & removal$Tree != "PN4" & removal$Tree != "PN7" & removal$Tree != "FS2",]
surlongf = surlong[surlong$Tree != "PN5" & surlong$Tree != "PN4" & surlong$Tree != "PN7" & surlong$Tree != "FS2",]

removal$Section
removalfcon1 = removalf[removalf$Type == "C" & removalf$Section == "Below",]
radd = removalfcon1[1:4,]
radd$Rem = 8
removalfcon1 = rbind(radd,removalfcon1)
removalfcon1$Date = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,2))
removalfcon1[removalfcon1$Date == 6,]$Rem = c(3,0)

ggp = ggplot(removalfcon1, aes(x=Date, y=Rem, group = Tree, col = Tree)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  xlab("Week") +
  ylab("Surviving seeds") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  #theme(legend.justification=c(1,1), legend.position=c(0.96,1)) +
  theme(legend.position = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = element_blank(),
    ##panel.background = element_blank()
  )
