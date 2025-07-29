getwd()
setwd("D:/R")
meas = read.csv("Pongamia-measures.csv")
damage = read.csv("Pongamia-damage.csv")
lop = read.csv("Pongamia-lopping.csv")
names(meas)
attach(meas)
summary(meas)
meas[width.l > 45 & width.r < 30 & leaflet.id ==5, ]
with(meas[width.r > 10 & width.l > 15 & tree.id != 66, ], plot(width.l ~ width.r, main = "Apical"))
with(meas[width.r > 10 & width.l > 15 & tree.id != 66, ], abline(lm(width.l ~ width.r), col = "red"))
with(meas[width.r > 10 & width.l > 15 & tree.id != 66, ], abline(c(0,1), col = "blue"))
meas$unique.id = paste(observer.name, tree.id, leaf.id, sep = "-")
meas.1 = meas[leaflet.id == 1, ]
meas.2 = meas[leaflet.id == 2, ]
meas.3 = meas[leaflet.id == 3, ]
meas.4 = meas[leaflet.id == 4, ]
head(meas.1)
head(meas.2)
all.equal(meas.1$unique.id, meas.2$unique.id)
plot(width.l ~ width.r, main = "Apical")
abline(lm(width.l ~ width.r), col = "red")
abline(c(0,1), col = "blue")
plot(meas.2$length ~ meas.1$length)
abline(lm(meas.2$length ~ meas.1$length), col = "red")
abline(c(0,1), col = "blue")
meas.1[meas.1$length>120, ]
plot(meas.4$length ~ meas.3$length)
abline(lm(meas.4$length ~ meas.3$length), col = "red")
abline(c(0,1), col = "blue")
meas.3[meas.3$length<40, ]
boxplot(meas.3$length)
meas.3[meas.3$unique.id == "Amod-30-E", "length"] = 76.76
meas.3[meas.3$unique.id == "Amod-30-E", "length"]
