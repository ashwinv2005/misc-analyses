ter = read.csv("D:/R/termite-final.csv")
ter = ter[, 1:12]
ter = ter[!is.na(full.time), ]
summary(ter)
names(ter)
attach(ter)
boxplot(first.resp, main = "First Response Time in min")
par(mfrow = c(4,2))
mar = c(4,4,2,1)
boxplot(circum, main = "Circumference of Mound")
boxplot(height, main = "Height of Mound")
boxplot(area, main = "Area of Mound", varwidth = TRUE)
boxplot(full.time, main = "Time for Full Repair in Minutes", varwidth = TRUE)
hist(circum, main = "Frequency Distribution of Circumference of Mound in cm", xlab = "Circumference")
hist(height, main = "Frequency Distribution of Height of Mound in cm", xlab = "Height")
hist(area, main = "Frequency Distribution of Area of Mound in Sq cm", xlab = "Area")
hist(full.time, main = "Frequency Distribution of Time for Full Repair of Mound in Minutes", xlab = "Time for Full Repair")
ter[circum<150, ]
ter[area>3, ]
ter[first.resp>30, ]
plot(height ~ circum, main = "Height vs Circumference", col = "blue", xlab = "Circumference in cm", ylab = "Height in cm")
abline(lm(height~circum), col = "red")
plot(first.resp ~ hole.pos, main = "First Response vs Hole Position", xlab = "Hole Position", ylab = "First Response Time in min")
plot(full.time ~ hole.pos, main = "Hole Repair Time vs Hole Position", xlab = "Hole Position", ylab = "Hole Repair Time in min")
with(ter[hole.pos == "R", ], plot(full.time ~ height))
with(ter[hole.pos == "R", ], abline(lm(full.time ~ height)))
str(ter)
par(mfrow = c(3,3))
plot(data.frame(diam1, diam2, area))
with(ter[area < 12, ], plot(full.time ~ area, main = "Time of Repair vs area", col = hole.pos))
par(mfrow = c(2,2))
plot(full.time ~ first.resp, main = "Time of Repair vs First Response", col = hole.pos)
plot((full.time - first.resp) ~ area, main = "Added Time of Repair vs First Response", col = hole.pos, log = "x")

