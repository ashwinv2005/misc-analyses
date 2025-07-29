# Plotting MammalSigns per plot
data<-read.csv("D:/Wildlife/Periyar/Trip/Edge/Mammals.csv")

plot_colors <- c("blue","red","peru","darkslategrey")
y=seq(0,1,0.1)
x=seq(0,80,20)
#jpeg("proportion.jpeg")
#png("proportion.png")
#s<-lm(data$Plot.No.Species,data[,2])
#plot(s,ylim=c(0,1),xlim=c(0,100),axes=FALSE, ann=FALSE,col=plot_colors[1],bg=plot_colors[1],pch=24,type="b")
plot(data$Plot.No.Species,data[,2],ylim=c(0,1),xlim=c(0,100),axes=FALSE, ann=FALSE,col=plot_colors[1],bg=plot_colors[1],pch=24,type="b")
axis(2, at=y, lab=y)
axis(1,at=x, lab=x)
lines(data$Plot.No,data[,3], type="o", pch=22, lty=2,bg=plot_colors[2],col=plot_colors[2])
lines(data$Plot.No,data[,4], type="o", pch=23, lty=2,bg=plot_colors[3],col=plot_colors[3])
lines(data$Plot.No,data[,5], type="o", pch=25, lty=2,bg=plot_colors[4],col=plot_colors[4])
title(main="VARIATION IN PROPORTION OF MAMMAL SIGNS WITH DISTANCE FROM EDGE")
title(xlab= "Distance From The Edge In Meters", col.lab=rgb(0,0.5,0))
title(ylab= "Proportion Of  Plots With Signs (Total Plots =12)", col.lab=rgb(0,0.5,0))
legend(0,1, c("Sambar","Elephant","Gaur","Muntjac"), cex=0.8, col=plot_colors,pch=c(24,22,23,25), lty=1:2)
dev.off()

#Plotting  Mammal Signs Sitewise per plot
png("boxplot.png")
y=seq(0,5,1)

sdata=read.csv("D:/Wildlife/Periyar/Trip/Edge/Final.csv") 

sdata=sdata[,2:6]
colnames(sdata)=c("0","20","40","60","80")
boxplot(sdata,col="gray")
axis(2,at=y,lab=y)

title(main="VARIATION IN MAMMAL SIGNS WITH DISTANCE FROM EDGE")
title(xlab= "Distance From The Edge In Meters", col.lab=rgb(0,0.5,0))
title(ylab= "Number Of Species", col.lab=rgb(0,0.5,0))

dev.off()








# HISTOGRAM


data<-read.csv("Mammals.csv")
sbr=subset(data,Species=="SBR")
sbr=sbr[,3:7]
summary(sbr)
par(col.main="red",col.lab="red")
hist(sbr$PlotNo,breaks=6,xlab="Plot Number",ylab="Frequency",main="Sambar",col="gray")



elp=subset(data,Species=="ELP")
hist(elp$PlotNo)
gar=subset(data,Species=="GAR")
hist(gar$PlotNo)

mjk=subset(data,Species=="MJK")
hist(mjk$PlotNo)

chv=subset(data,Species=="CHV")
hist(chv$PlotNo)

ber=subset(data,Species=="BER")
hist(ber$PlotNo)


