pellets = read.csv("C:/Ashwin/pellets.csv")
pellets = pellets[,1:5]
pellets = pellets[!is.na(pellets$count),]
length(pellets$count)
pellets$count
temp = pellets[46:82,]
te = pellets[1:45,]
temp1 = temp[temp$animal == "goat",]
temp2 = temp[temp$animal == "cow",]
temp3 = temp[temp$animal == "sheep",]
temp4 = temp[temp$animal == "camel",]
te1 = te[te$animal == "goat",]
te2 = te[te$animal == "cow",]
te3 = te[te$animal == "sheep",]
te4 = te[te$animal == "camel",]
a = 365/14
b = 21/4
for (i in 1:45)
{
  if (pellets$animal[i] == "goat")
    pellets$count[i] = a*pellets$count[i]
  if (pellets$animal[i] == "sheep")
    pellets$count[i] = b*pellets$count[i]
}
pellets$count = round(pellets$count)
pellets$transect[4:5] = "f"
length(pellets$count)
c = 0
d = pellets$transect[5]
for (i in 6:82)
{
  if (pellets$transect[i] != d | pellets$name[i] != pellets$name[i-1])
    c = c+1
  d = pellets$transect[i]
  pellets$transect[i] = c
}
pellets$transect
write.csv(bare, "c:/Ashwin/bare.csv")

bare = read.csv("C:/Ashwin/bareg.csv")
bare = bare[,1:4]
bare = bare[!is.na(bare$transect),]
bare$sl = 1:984

patch = read.csv("C:/Ashwin/patch.csv")
names(patch)
patch$transect = patch$transect+32
patch = patch[,-4]
patch = write.csv(patch,"C:/Ashwin/patch.csv")

veget = read.csv("C:/Ashwin/veget.csv")
names(veget)
a = with(veget, tapply(count,species,sum))
b = unique(veget$species
length(veget$species)
veget

veg = data.frame(cbind(1:1710,0))
veg = veg[,-c(1,2)]
veg$name = c(rep("bar1",76),rep("bar2",76),rep("bar3",76),rep("jai1",228),rep("jai2",228),rep("jai3",76),rep("jai4",152),rep("bik1",152),rep("bik2",76),rep("bik4",152),rep("bik5",152),rep("nag1",114),rep("chu1",76),rep("chu2",76))
veg$transect = c(rep("a",38),rep("b",38),rep("c",38),rep("d",38),rep("e",38),rep("f",38),rep(1:39,each = 38))
veg$species = rep(b,45)
veg$count = 0
veg$transect
for (i in 1:281)
{
  veg[veg$transect == veget$transect[i] & veg$species == veget$species[i],]$count = veget$count[i]
}
veg[1:500,]
write.csv(veg,"C:/Ashwin/veg.csv")

