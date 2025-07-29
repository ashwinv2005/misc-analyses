plot(nlastcon2$indexp~nlastcon1$indexp)
summary(lm(nlastcon2$indexp~nlastcon1$indexp))

nsd = summarySE(ncon1, measurevar="eaten", groupvars=c("run"))


## Nn

n = read.csv("C:/Users/ashwinv/Desktop/predfiles/Nn.csv")
nlast = n[n$day == 41,]
nlastcon1 = nlast[nlast$treat == "con1",]
nlastcon2 = nlast[nlast$treat == "con2",]
nlastcon3 = nlast[nlast$treat == "con3",]

ncon1 = n[n$treat == "con1",]
ncon2 = n[n$treat == "con2",]

tempfilter1 = nlastcon1[nlastcon1$indexp <= 30,]

for (i in 1:length(tempfilter1$run))
{
  if (i == 1)
  {
    nfilter1 = n[n$run == tempfilter1$run[i],]
  }
  if (i > 1)
  {
    temp = n[n$run == tempfilter1$run[i],]
    nfilter1 = rbind(nfilter1,temp)
  }
}

tempfilter2 = nlastcon2[nlastcon2$indexp <= 80 & nlastcon2$indexp >=50,]

for (i in 1:length(tempfilter2$run))
{
  if (i == 1)
  {
    nfilter2 = nfilter1[nfilter1$run == tempfilter2$run[i],]
  }
  if (i > 1)
  {
    temp = nfilter1[nfilter1$run == tempfilter2$run[i],]
    nfilter2 = rbind(nfilter2,temp)
  }
}

tempfilter3 = nlastcon3[nlastcon3$index > 90,]

for (i in 1:length(tempfilter3$run))
{
  if (i == 1)
  {
    nfilter3 = nfilter2[nfilter2$run == tempfilter3$run[i],]
  }
  if (i > 1)
  {
    temp = nfilter2[nfilter2$run == tempfilter3$run[i],]
    nfilter3 = rbind(nfilter3,temp)
  }
}

length(nfilter3$run)

## Ny1

y1 = read.csv("C:/Users/ashwinv/Desktop/predfiles/Ny1.csv")
y1last = y1[y1$day == 41,]

y1lastcon1 = y1last[y1last$treat == "con1",]
y1lastcon2 = y1last[y1last$treat == "con2",]
y1lastcon3 = y1last[y1last$treat == "con3",]

y1con1 = y1[y1$treat == "con1",]
y1con2 = y1[y1$treat == "con2",]

tempfilter1 = y1lastcon1[y1lastcon1$indexp <= 30,]

for (i in 1:length(tempfilter1$run))
{
  if (i == 1)
  {
    y1filter1 = y1[y1$run == tempfilter1$run[i],]
  }
  if (i > 1)
  {
    temp = y1[y1$run == tempfilter1$run[i],]
    y1filter1 = rbind(y1filter1,temp)
  }
}

tempfilter2 = y1lastcon2[y1lastcon2$indexp <= 80 & y1lastcon2$indexp >=50,]

for (i in 1:length(tempfilter2$run))
{
  if (i == 1)
  {
    y1filter2 = y1filter1[y1filter1$run == tempfilter2$run[i],]
  }
  if (i > 1)
  {
    temp = y1filter1[y1filter1$run == tempfilter2$run[i],]
    y1filter2 = rbind(y1filter2,temp)
  }
}

tempfilter3 = y1lastcon3[y1lastcon3$index > 70,]

for (i in 1:length(tempfilter3$run))
{
  if (i == 1)
  {
    y1filter3 = y1filter2[y1filter2$run == tempfilter3$run[i],]
  }
  if (i > 1)
  {
    temp = y1filter2[y1filter2$run == tempfilter3$run[i],]
    y1filter3 = rbind(y1filter3,temp)
  }
}

length(y1filter3$run)
## Ny2

y2 = read.csv("C:/Users/ashwinv/Desktop/predfiles/Ny2.csv")

y2last = y2[y2$day == 41,]

y2lastcon1 = y2last[y2last$treat == "con1",]
y2lastcon2 = y2last[y2last$treat == "con2",]
y2lastcon3 = y2last[y2last$treat == "con3",]

y2con1 = y2[y2$treat == "con1",]
y2con2 = y2[y2$treat == "con2",]

tempfilter1 = y2lastcon1[y2lastcon1$indexp <= 30,]

for (i in 1:length(tempfilter1$run))
{
  if (i == 1)
  {
    y2filter1 = y2[y2$run == tempfilter1$run[i],]
  }
  if (i > 1)
  {
    temp = y2[y2$run == tempfilter1$run[i],]
    y2filter1 = rbind(y2filter1,temp)
  }
}

tempfilter2 = y2lastcon2[y2lastcon2$indexp <= 90 & y2lastcon2$indexp >=50,]

for (i in 1:length(tempfilter2$run))
{
  if (i == 1)
  {
    y2filter2 = y2filter1[y2filter1$run == tempfilter2$run[i],]
  }
  if (i > 1)
  {
    temp = y2filter1[y2filter1$run == tempfilter2$run[i],]
    y2filter2 = rbind(y2filter2,temp)
  }
}

tempfilter3 = y2lastcon3[y2lastcon3$index > 85,]

for (i in 1:length(tempfilter3$run))
{
  if (i == 1)
  {
    y2filter3 = y=y2filter2[y2filter2$run == tempfilter3$run[i],]
  }
  if (i > 1)
  {
    temp = y2filter2[y2filter2$run == tempfilter3$run[i],]
    y2filter3 = rbind(y2filter3,temp)
  }
}

length(y2filter3$run)/410

length(y2$run)

#################################################

for (i in 1:131200)
{
  if (y2$day[i] != 1)
  {
    #n$seaten[i] = n$indexp[i-10] - n$indexp[i]
    #y1$seaten[i] = y1$indexp[i-10] - y1$indexp[i]
    y2$seaten[i] = y2$indexp[i-10] - y2$indexp[i]
  }
  
  if (y2$day[i] == 1)
  {
    #n$seaten[i] = 100 - n$indexp[i]
    #y1$seaten[i] = 100 - y1$indexp[i]
    y2$seaten[i] = 100 - y2$indexp[i]
  }
}

write.csv(y2,"C:/Users/ashwinv/Desktop/one.csv")

names(y2filter3)

#########################################

###     112!       128NO       134!       138!!%       167!%       188       204       208       214!%       223NO       228NO       231 
###   9.566201  9.108292    10.227891    9.650161     11.819991  9.036714 10.546471  9.510585    9.095362  9.143304    10.010117  9.709563 
###     235       250!%     252NO       260       262       279%       280       282!NO       283%       286NO       297       320 
###  10.302380  9.600051  9.570152    9.044470 10.756337 11.605560 12.812580   10.846602    9.351536  9.034824 11.228177 10.118083 


library(ggplot2)

ggp = ggplot(y2filter3[y2filter3$treat == "con1" & y2filter3$run == 250,], aes(x=day, y=indexp)) +
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

########################################

nfilter3[nfilter3$day == 41 & (nfilter3$treat == "con1" | nfilter3$treat == "con2"),]
nfilter3[nfilter3$day == 41 & nfilter3$treat == "con1",]

y2filter4[y2filter4$day == 41 & (y2filter4$treat == "con1" | y2filter4$treat == "con2" | y2filter4$treat == "con3"),]
y2filter4[y2filter4$day == 41 & y2filter4$treat == "con1",]

sdn = with(n[n$treat == "con1",], tapply(eaten, run, sd))
sdy1 = with(y1[y1$treat == "con1",], tapply(eaten, run, sd))
sdy2 = with(y2[y2$treat == "con1",], tapply(eaten, run, sd))

sdy2filter = with(y2filter3[y2filter3$treat == "con1",], tapply(eaten, run, sd))
length(sdy2filter)
sdnames = as.numeric(names(sdy2filter))
sdy2filter[sdy2filter > 9.5]

sdfilter = with(y2[y2$treat == "con1",], tapply(eaten, run, sd))

head(y2filter3)
length(y2$run)/410

y2filter3$sd = 0
y2$sd = 0

for (i in 1:320)
{
  y2$sd[(410*(i-1) + 1):(410*(i-1) + 410)] = sdfilter[i]
}




y2filter4 = y2filter3[y2filter3$sd > 9.5,]
head(y2filter4)

mean(sdn)
sd(sdy2)
length(sdn)
max(sdy2)

sdf = data.frame(cbind(1:1944,0))
sdf = sdf[,-c(1,2)]
sdf$type = c(rep("No image",648), rep("Semi-linear",648), rep("Logistic",648))
sdf$sd = c(sdn,sdy1,sdy2)

ggp = ggplot(sdf, aes(x=type, y=sd)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  xlab("Curve Type") +
  ylab("SD") +
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

##########################################

expy2 = read.csv("C:/Users/ashwinv/Desktop/predfiles/Ny2x.csv")
names(expy2)

expy2$eaten = expy2$seeds - expy2$x1.seeds.39
expy2$propeaten = expy2$eaten/expy2$seeds

###     112!       128       134!       138!!       167!       188       204       208       214!       223       228       231 
###   9.566201  9.108292 10.227891    9.650161     11.819991  9.036714 10.546471  9.510585  9.095362  9.143304 10.010117  9.709563 
###     235       250!     252       260       262       279       280       282!       283       286       297       320 
###  10.302380  9.600051  9.570152  9.044470 10.756337 11.605560 12.812580 10.846602  9.351536  9.034824 11.228177 10.118083 

with(expy2f1[expy2f1$run == 138,], plot(propeaten~seeds))
with(expy2f1[expy2f1$run == 138,], abline(lm(propeaten~seeds)))
with(expy2f1[expy2f1$run == 138,], summary(lm(propeaten~seeds)))

expy2f = with(expy2[expy2$run == 250,], tapply(eaten,seeds,mean))
expy2f1 = summarySE(expy2, measurevar="eaten", groupvars=c("run","seeds"))
head(expy2f1)
expy2f1$propeaten = expy2f1$eaten/expy2f1$seeds
expy2f1$sep = expy2f1$se/expy2f1$seeds

ggp = ggplot(expy2f1[expy2f1$run == 112,], aes(x=seeds, y=propeaten)) +
  #geom_line(size = 0.6) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = propeaten-sep, ymax = propeaten+sep), size = 0.2) +
  xlab("Number of seeds") +
  ylab("Mean per capita mortality") +
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

########################

head(y2)
stat = glm(data = y2[y2$treat == "con1" & y2$day == 41,], indexp ~ number + loops + ars + gud + ars:gud + gud:loops + number:gud + ars:gud:loops + ars:gud:number + ars:loops:number + gud:loops:number)
stat = glm(data = y2[y2$treat == "con2" & y2$day == 41,], indexp ~ number*ars*gud*loops)

summary(stat)
