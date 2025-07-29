patch = read.csv("C:/Ashwin/patch.csv")
veg = read.csv("C:/Ashwin/veg.csv")
bare = read.csv("C:/Ashwin/bare.csv")
pell = read.csv("C:/Ashwin/pell.csv")

names(patch)
a = high1*100
plot(sort(patch$patch))
b = exp.fit(a,10)
c = pareto.fit(a,10)
d = powerexp.fit(a,10)
vures = vuong(pareto.exp.llr(a, c, b))
pares = power.powerexp.lrt(c,d)
pares

baremean = with(bare,tapply(bg,transect,mean))
baremedian = with(bare,tapply(bg,transect,median))

plot(sort(high,T))
high = high*100
length(low)
length(high)
min(a)

pell[pell$animal == "cowcam",]$count[1:19] = round(pell[pell$animal == "cowcam",]$count[1:19])

pell[pell$animal == "chinkara",]$count = pell[pell$animal == "chinkara",]$count/a4
pell$count = round(pell$count,3)*10
write.csv(pell,"C:/Ashwin/pell.csv")

graze = with(pell,tapply(count,transect,sum))

pell = pell[-c(1:8),]
pell$transect = rep(1:39, each = 4)

grazegoat = pell[pell$animal == "goat",]$count

plot(graze,baremean)
plot(sort(graze,T))
graze[graze<6.5]

su = summarySE(pell,measurevar = "count", groupvar = "transect")
su$count = su$count*4
su = su[-c(2,4,5,6)]
b = su[su$count<6.5,]$transect
length(bare$transect)

low = bare[bare$transect == 1 | bare$transect == 2 | bare$transect == 4 | bare$transect == 6 | bare$transect == 7 | bare$transect == 8 | bare$transect == 9 | bare$transect == 10 | bare$transect == 12 | bare$transect == 17 | bare$transect == 20 | bare$transect == 25 | bare$transect == 27 | bare$transect == 28 | bare$transect == 29 | bare$transect == 32 | bare$transect == 34 | bare$transect == 35 | bare$transect == 36,]$bg
high = bare[bare$transect == 3 | bare$transect == 5 | bare$transect == 11 | bare$transect == 13 | bare$transect == 14 | bare$transect == 15 | bare$transect == 16 | bare$transect == 18 | bare$transect == 19 | bare$transect == 21 | bare$transect == 22 | bare$transect == 23 | bare$transect == 24 | bare$transect == 26 | bare$transect == 30 | bare$transect == 31 | bare$transect == 33 | bare$transect == 37 | bare$transect == 38 | bare$transect == 39,]$bg

c = 1
for (i in 1:984)
{
  bare$graze[i] = graze[c]
  if (bare$transect[i] != names(graze[c]))
    c = c + 1
}
plot(sort(graze))
hist(bare$graze)

length(bare[bare$graze<3.1,]$bg)
length(bare[bare$graze>=3.1 & bare$graze<10.8,]$bg)
length(bare[bare$graze>=10.8,]$bg)

low1 = bare[bare$graze<3.1,]$bg
med1 = bare[bare$graze>=3.1 & bare$graze<10.8,]$bg
high1 = bare[bare$graze>=10.8,]$bg

length(bare[bare$graze<2.5,]$bg)
length(bare[bare$graze>=2.5 & bare$graze<6.5,]$bg)
length(bare[bare$graze>=6.5 & bare$graze<11.8,]$bg)
length(bare[bare$graze>=11.8,]$bg)

low2 = bare[bare$graze<2.5,]$bg
med12 = bare[bare$graze>=2.5 & bare$graze<6.5,]$bg
med22 = bare[bare$graze>=6.5 & bare$graze<11.8,]$bg
high2 = bare[bare$graze>=11.8,]$bg

plot(sort(low,T), xlab = "Index", ylab = "Bare Ground", main = "Low grazing", ylim = c(0,3200))

a = bare$bg*100
b = exp.fit(a,10)
c = pareto.fit(a,10)
d = powerexp.fit(a,10)
vures = vuong(pareto.exp.llr(a, c, b))
pares = power.powerexp.lrt(c,d)
vures
pares

overbare = summarySE(bare,measurevar = "bg", groupvar = "transect")
overbare$bg = overbare$bg*overbare$N
overbare = overbare[,-c(2,4,5,6)]
boxplot(overbare$gr,overbare$bg)
wilcox.test(overbare[overbare$gr == 1,]$bg, overbare[overbare$gr == 2,]$bg)
hist(graze, 20, xlab = "Grazing index")
for (i in 1:39)
{
  if (graze[i]<6.5)
    overbare$gr[i] = 1
  else
    overbare$gr[i] = 2
}

mean(overbare[overbare$gr == 1,]$bg)
mean(overbare[overbare$gr == 2,]$bg)

hist(bare$bg, 50, xlab = "Bare Ground (m)")
hist(bare$bg*100, 50, xlab = "Bare Ground (cm)", main = "Overall", ylim = c(0,400))

place = summarySE(bare,measurevar = "bg", groupvar = "name")
place = place[,-c(2,4,5,6)]
graplace = summarySE(pell,measurevar = "count", groupvar = "name")
graplace = graplace[,-c(2,4,5,6)]
plot(graplace$count,place$bg, xlab = "grazing", ylab = "Mean bare ground", main = "In every location")

names(veg)
veggra = veg[229:1710,]
unique(veg$species)
crobur = veggra[veggra$species == "crobur",]
capdec = veggra[veggra$species == "capdec",]
plot(capdec$count~graze)

for (i in 1:1710)
{
  if (veg$species[i] == "convolvulaceae")
    veg$species[i] = "conmic"
}

veg$species = as.character(veg$species)

warnings()

veg$species
