getwd()
setwd("D:/R")
meas = read.csv("Pongamia-measures.csv")
damage = read.csv("Pongamia-damage.csv")
lop = read.csv("Pongamia-lopping.csv")
names(meas)
attach(meas)
head(meas[observer.no == 2, ])
meas$unique.id = paste(observer.name, tree.id, leaf.id, sep = "-")
summary(meas)
meas.1 = meas[leaflet.id == 1 & observer.no ==1, ]
meas.2 = meas[leaflet.id == 2 & observer.no ==1, ]
meas.3 = meas[leaflet.id == 3 & observer.no ==2, ]
meas.4 = meas[leaflet.id == 4 & observer.no ==2, ]
meas.3[meas.3$unique.id == "Amod-30-E", "length"] = 76.76
head(meas.4)
all.equal(meas.1$unique.id, meas.2$unique.id)
all.equal(meas.3$unique.id, meas.4$unique.id)
par(mfrow = c(3,2))
plot(meas.1$length ~ meas.2$length, main = "Left Leaflet Length(1) vs Right Leaflet Length(2)", col = "blue", xlab = "Right Leaflet Length", ylab = "Right Leaflet Length")
abline(c(0,1))
abline(lm(meas.1$length ~ meas.2$length), col = "red") 
plot(meas.3$length ~ meas.4$length, main = "Left Leaflet Length(3) vs Right Leaflet Length(4)", col = "blue", xlab = "Right Leaflet Length", ylab = "Right Leaflet Length")
abline(c(0,1))
abline(lm(meas.3$length ~ meas.4$length), col = "red")
length(meas.1$length)
j = 0
k = 0
for(i in 1:200)
{
j = j + meas.1$length[i] - meas.2$length[i]
k = k + meas.3$length[i] - meas.4$length[i]
}
j
k
FA12 = abs(meas.1$length - meas.2$length)/((meas.1$length+meas.2$length)/2)
FA34 = abs(meas.3$length - meas.4$length)/((meas.3$length+meas.4$length)/2)
boxplot(FA12, main = "Fluctuating Asymmetry(1 & 2)") 
boxplot(FA34, main = "Fluctuating Asymmetry(3 & 4)") 
hist(FA12, main = "Fluctuating Asymmetry(1 & 2)", xlab = "Fluctuating Asymmetry")
hist(FA34, main = "Fluctuating Asymmetry(3 & 4)", xlab = "Fluctuating Asymmetry")
boxplot(FA12 ~ meas.1$tree.id, main = "FA Grouped by Tree ID", xlab = "Tree ID", ylab = "Fluctuating Asymmetry")
boxplot(FA34 ~ meas.3$tree.id, main = "FA Grouped by Tree ID", xlab = "Tree ID", ylab = "Fluctuating Asymmetry")


