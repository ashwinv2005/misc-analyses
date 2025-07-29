meas = read.csv("D:/Wildlife/R/Pongamia-measures.csv")
damage = read.csv("D:/Wildlife/R/Pongamia-damage.csv")
lop = read.csv("D:/Wildlife/R/Pongamia-lopping.csv")
meas.1 = meas[meas$leaflet.id == 1, ]
meas.2 = meas[meas$leaflet.id == 2, ]
meas.3 = meas[meas$leaflet.id == 3, ]
meas.4 = meas[meas$leaflet.id == 4, ]
meas.5 = meas[meas$leaflet.id == 5, ]
meas$unique.id = paste(meas$observer.name, meas$tree.id, meas$leaf.id, sep = "-")
all.equal(faf$wt.dam1, faf$wt.dam2)
damage.1 = damage[damage$leaflet.id == 1, ]
damage.2 = damage[damage$leaflet.id == 2, ]
damage.3 = damage[damage$leaflet.id == 3, ]
damage.4 = damage[damage$leaflet.id == 4, ]
damage.5 = damage[damage$leaflet.id == 5, ]
s = seq(0, 10000, length = 100)
plot((abs(1-s))/(1+s)~s, type = "l")
names(meas)
names(damage)
names(lop)
fa12 = 2*(abs(meas.1$length - meas.2$length))/(meas.1$length+meas.2$length)
fa34 = 2*(abs(meas.3$length - meas.4$length))/(meas.3$length+meas.4$length)
meas.1$fa = (fa12 + fa34)/2
fat = with(meas.1, tapply(fa, tree.id, mean))
fat1 = numeric(40)
a = 0
for (i in c(1:10, 21:30, 41:50, 61:70))
{
a = a+1
fat1[a] = mean(meas.1[meas.1$tree.id == i, ]$fa)
}
fat1
faf = data.frame(cbind(1:40, 0))
faf$sl.no = 1:40
faf$tree.id = unique(meas.1$tree.id)
faf$fa = fat
faf
faf = faf[, -c(1,2)] 
names(damage)
faf$insects = with(damage, tapply(X=insects, tree.id, function(x) table(x)[2]))/25
faf$galls = with(damage, tapply(X = galls, tree.id, function(x) table(x)[2]))/25
faf$herbivory = with(damage, tapply(X = herbivory, tree.id, function(x) table(x)[2]))/25
plot(faf$fa~faf$wt.dam)
lop = lop[order(lop$tree.id), ]
faf$lopped = lop$lopped
faf$height = lop$height
faf$mean.dam = (faf$insects + faf$herbivory)/2
faf$mean.dam = apply(faf[, 4:6], 1, mean)
faf$wt.dam = (faf$insects * mean(faf$insects) + faf$herbivory * mean(faf$herbivory))/(mean(faf$insects) + mean(faf$herbivory))
mean(faf[, 4:6])
mod = lm(fa~mean.dam*lopped, data = faf)
summary(mod)
plot(faf$fa~faf$mean.dam)