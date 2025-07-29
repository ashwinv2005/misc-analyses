bird = read.csv("D:/Wildlife/Periyar/Trip/Edge/Bird data.csv")
bird = bird[!is.na(bird$Species), ]
intersect(bird[bird$TeamNo == 1, ]$Species, bird[bird$TeamNo == 2, ]$Species)
setdiff(bird[bird$TeamNo == 2, ]$Species, bird[bird$TeamNo == 3, ]$Species)
acpoint = data.frame(cbind(1:5, 0))
acpoint = acpoint[, -c(1,2)]
acpoint$Sl = c(1:5)
richvar = data.frame(cbind(1:9, 0))
richvar = richvar[, -c(1,2)]
abundvar = data.frame(cbind(1:9, 0))
abundvar = abundvar[, -c(1,2)]
for(i in 1:5)
{
acpoint$rich[i] = unique(bird[bird$PointStation == i, ]$Species)
acpoint$abund[i] = sum(bird[bird$PointStation == i, ]$Abundance)
a = 0
for(j in 1:3)
{
for(k in 1:3)
{
a=a+1
richvar[a, i] = length(bird[bird$TeamNo == j & bird$Site == k & bird$PointStation == i, ]$Species)
abundvar[a, i] = sum(bird[bird$TeamNo == j & bird$Site == k & bird$PointStation == i, ]$Abundance)
}
}
}
names(richvar) = c("p1", "p2", "p3", "p4", "p5")
names(abundvar) = c("p1", "p2", "p3", "p4", "p5")
sd(richvar)/sqrt(9)
mean(richvar)
boxplot(richvar, main = "Sp. Richness at Distances from Edge", xlab = "Distance from edge(100m)", ylab = "No. of Species")
boxplot(abundvar, main = "Abundance at Distances from Edge", xlab = "Distance from edge(100m)", ylab = "No. of Individuals")
plot(acpoint$rich~acpoint$Sl, type = "b", col = "red", main = "Cumulative Species Richness at at each Distance from Edge", xlab = "Distance from edge(100m)", ylab = "Species Richness")
plot(acpoint$abund~acpoint$Sl, type = "b", col = "blue", main = "Cumulative Abundance at each Distance from Edge", xlab = "Distance from edge(100m)", ylab = "Number of Individuals")
splist = unique(bird$Species)
indbird = data.frame(cbind(1:5, 0))
indbird = indbird[ ,-c(1,2)]
for(i in 1:5)
{
for(j in 1:57)
{
indbird[i, j] = sum(bird[bird$Species == splist[j] & bird$PointStation == i, ]$Abundance)
}
}
names(indbird) = splist
indbird
for (i in 1:3)
{
a[i] = sum(bird[bird$Site == i, ]$Abundance)
b[i] = length(unique(bird[bird$Site == i, ]$Species))
}
?write
write.csv(indbird, "D:/Wildlife/Periyar/Trip/Edge/Final/Species across distance classes.csv") 