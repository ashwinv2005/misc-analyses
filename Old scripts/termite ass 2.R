ter = read.csv("D:/R/termite-final.csv")
ter = ter[, 1:12]
summary(ter)
names(ter)
attach(ter)
plot(full.time-first.resp ~ area, main = "Repair Time vs Area", xlab = "Hole Area in sq cm(log)", ylab = "Repair Time in min", col = hole.pos, log= "x")
ter$mound.vol = (circum^2)*height/(12*pi)
summary(ter)
attach(ter)
plot(full.time-first.resp ~ mound.vol, main = "Repair Time vs Mound Vol", xlab = "Mound Vol in cubic cm", ylab = "Repair Time in min", col = hole.pos)
plot(full.time-first.resp ~ mound.vol, main = "Repair Time vs Mound Vol", xlab = "Mound Vol in cubic cm(log)", ylab = "Repair Time in min", col = hole.pos, log = "x")
plot(full.time-first.resp ~ hole.pos, main = "Repair Time vs Hole position", xlab = "Hole Position", ylab = "Repair Time in min")


