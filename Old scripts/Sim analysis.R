setup11 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output1/0aMFwz.csv")
#names(setup11)
setup11con = setup11[setup11$treat == "con1",]
#head(setup11con)

setup12 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output1/jsq7pe.csv")
#names(setup12)
setup12con = setup12[setup12$treat == "con1",]
#head(setup12con)

setup13 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output1/lYv9cC.csv")
#names(setup13)
setup13con = setup13[setup13$treat == "con1",]
#head(setup13con)

#########################

setup21 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output2/BV6u3g.csv")
#names(setup21)
setup21con = setup21[setup21$treat == "con1",]
#head(setup21con)

setup22 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output2/CIpuHw.csv")
#names(setup22)
setup22con = setup22[setup22$treat == "con1",]
#head(setup22con)

setup23 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output2/W8jXqa.csv")
#names(setup23)
setup23con = setup23[setup23$treat == "con1",]
#head(setup23con)

##########################

setup31 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output3/74MUGn.csv")
#names(setup31)
setup31con = setup31[setup31$treat == "con1",]
#head(setup31con)

setup32 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output3/D20FeE.csv")
#names(setup32)
setup32con = setup32[setup32$treat == "con1",]
#head(setup32con)

setup33 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output3/vddmbi.csv")
#names(setup33)
setup33con = setup33[setup33$treat == "con1",]
#head(setup33con)

##############################

setup41 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output4/BbGI7i.csv")
#names(setup41)
setup41con = setup41[setup41$treat == "con1",]
#head(setup41con)

setup42 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output4/n8lRIa.csv")
#names(setup42)
setup42con = setup42[setup42$treat == "con1",]
#head(setup42con)

############################

setup51 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output5/n09mFh.csv")
#names(setup51)
setup51con = setup51[setup51$treat == "con1",]
#head(setup51con)

setup52 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output5/vwI67P.csv")
#names(setup52)
setup52con = setup52[setup52$treat == "con1",]
#head(setup52con)

##############################

setup61 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output6/akph4g.csv")
#names(setup61)
setup61con = setup61[setup61$treat == "con1",]
#head(setup61con)

setup62 = read.csv("C:/Users/ashwinv/Desktop/output_folders/output6/RI4aZS.csv")
#names(setup62)
setup62con = setup62[setup62$treat == "con1",]
#head(setup62con)

ggp = ggplot(setupnewcon, aes(x=day, y=actseeds)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  xlab("Days") +
  ylab("Seeds") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.justification=c(1,1), legend.position=c(0.96,1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = element_blank(),
    ##panel.background = element_blank()
  )

names(maxprop1con)
maxprop1 = rbind(setup12, setup13)
maxprop2 = rbind(setup22, setup23)
maxprop3 = rbind(setup32, setup33)
maxprop4 = rbind(setup41, setup42)
maxprop5 = rbind(setup51, setup52)
maxprop6 = rbind(setup61, setup62)
summary(maxprop1)

maxprop1 = maxprop1[maxprop1$day != 41,]
maxprop2 = maxprop2[maxprop2$day != 41,]
maxprop3 = maxprop3[maxprop3$day != 41,]
maxprop4 = maxprop4[maxprop4$day != 41,]
maxprop5 = maxprop5[maxprop5$day != 41,]
maxprop6 = maxprop6[maxprop6$day != 41,]

## setup1

maxprop1$density = maxprop1$actseeds/maxprop1$area
maxprop1$proppred = maxprop1$actpred/maxprop1$actseeds
maxprop1con = maxprop1[maxprop1$treat == "con1",]

mp1 = with(data = maxprop1con, tapply(proppred, run, max))
maxp1 = maxprop1con[maxprop1con$day == 1, 2:11]
maxp1$proppred = mp1

## setup2

maxprop2$density = maxprop2$actseeds/maxprop2$area
maxprop2$proppred = maxprop2$actpred/maxprop2$actseeds
maxprop2con = maxprop2[maxprop2$treat == "con1",]

mp2 = with(data = maxprop2con, tapply(proppred, run, max))
maxp2 = maxprop2con[maxprop2con$day == 1, 2:11]
maxp2$proppred = mp2

## setup3

maxprop3$density = maxprop3$actseeds/maxprop3$area
maxprop3$proppred = maxprop3$actpred/maxprop3$actseeds
maxprop3con = maxprop3[maxprop3$treat == "con1",]

mp3 = with(data = maxprop3con, tapply(proppred, run, max))
maxp3 = maxprop3con[maxprop3con$day == 1, 2:11]
maxp3$proppred = mp3

## setup4

maxprop4$density = maxprop4$actseeds/maxprop4$area
maxprop4$proppred = maxprop4$actpred/maxprop4$actseeds
maxprop4con = maxprop4[maxprop4$treat == "con1",]

mp4 = with(data = maxprop4con, tapply(proppred, run, max))
maxp4 = maxprop4con[maxprop4con$day == 1, 2:11]
maxp4$proppred = mp4

## setup5

maxprop5$density = maxprop5$actseeds/maxprop5$area
maxprop5$proppred = maxprop5$actpred/maxprop5$actseeds
maxprop5con = maxprop5[maxprop5$treat == "con1",]

mp5 = with(data = maxprop5con, tapply(proppred, run, max))
maxp5 = maxprop5con[maxprop5con$day == 1, 2:11]
maxp5$proppred = mp5

## setup6

maxprop6$density = maxprop6$actseeds/maxprop6$area
maxprop6$proppred = maxprop6$actpred/maxprop6$actseeds
maxprop6con = maxprop6[maxprop6$treat == "con1",]

mp6 = with(data = maxprop6con, tapply(proppred, run, max))
maxp6 = maxprop6con[maxprop6con$day == 1, 2:11]
maxp6$proppred = mp6


ana = with(data = maxp1, glm(proppred~number+loops+cap+ars+gud+baseline+lag+minprev))
summary(ana)

arseffect = maxp1[maxp1$number != 50 & maxp1$loops != 40 & maxp1$cap != 10 & maxp1$lag != 0, ]
plot(arseffect$proppred~arseffect$ars)

lageffect = maxp1[maxp1$number != 50 & maxp1$loops != 40 & maxp1$cap != 10 & maxp1$ars != 4, ]
plot(lageffect$proppred~lageffect$lag)

plot(maxprop1$proppred~maxprop1$actseeds)

seclevel1 = rbind(setup12, setup13)
seclevel1 = seclevel1[seclevel1$treat == "con2" & seclevel1$day == 41,]

setupnew = read.csv("C:/Users/ashwinv/Desktop/predfiles/3qgu2H.csv")
#names(setup61)
setupnewcon = setupnew[setupnew$treat == "con1",]
#head(setup61con)

