veg = read.csv("D:/Wildlife/Periyar/Trip/Edge/Veg data.csv")
names(veg)
sapling = veg[, c(1,3,4,5,8)] 
invasive = veg[, c(1,3,4,5,11)]
names(sapling)
sapling = sapling[!is.na(sapling$SapAbund), ]
invasive = invasive[!is.na(invasive$InvAbund), ]
ana = data.frame(cbind(1:5, 0))
ana = ana[, -c(1,2)]
ana$Sl = c(1,2,3,4,5)
richvars = data.frame(cbind(1:12, 0))
richvars = richvar[, -c(1,2)]
abundvars = data.frame(cbind(1:12, 0))
abundvars = abundvar[, -c(1,2)]
richvari = data.frame(cbind(1:12, 0))
richvari = richvar[, -c(1,2)]
abundvari = data.frame(cbind(1:12, 0))
abundvari = abundvar[, -c(1,2)]
b = 0
for(i in 1:5)
{
a = 0
ana[i, 2] = sum(sapling[sapling$PointStation == i, ]$SapAbund)
ana[i, 3] = sum(invasive[invasive$PointStation == i, ]$InvAbund)
for(j in 1:3)
{
for(k in 1:4)
{
a=a+1
richvars[a, b+i] = length(sapling[sapling$TeamNo == j & sapling$Site == k & sapling$PointStation == i, ]$SapAbund)
richvari[a, b+i] = length(invasive[invasive$TeamNo == j & invasive$Site == k & invasive$PointStation == i, ]$InvAbund)
abundvars[a, b+i] = sum(sapling[sapling$TeamNo == j & sapling$Site == k & sapling$PointStation == i, ]$SapAbund)
abundvari[a, b+i] = sum(invasive[invasive$TeamNo == j & invasive$Site == k & invasive$PointStation == i, ]$InvAbund)
richvars[a, b+i+1] = richvari[a, b+i+1] = abundvars[a, b+i+1] = abundvari[a, b+i+1] = mean(veg[veg$TeamNo == j & veg$Site == k & veg$PointStation == i, ]$Canopy)
}
}
b=b+1
}
names(richvars) = names(richvari) = names(abundvars) = names(abundvari) = c("p1", "c1", "p2", "c2", "p3", "c3", "p4", "c4", "p5", "c5")
names(ana) = c("Sl", "Sapling", "Invasive")
ana$canopy = mean(richvars[, c(2,4,6,8,10)])
plot(ana$Sapling~ana$Sl, type = "b", xlab = "Distance from edge(20m)", ylab = "No. of saplings", main = "Sapling Abundance vs Distance from Edge")
plot(ana$Invasive~ana$Sl, type = "b", xlab = "Distance from edge(20m)", ylab = "No. of invasives", main = "Invasive Abundance vs Distance from Edge")
plot(ana$canopy~ana$Sl, type = "b", xlab = "Distance from edge(20m)", ylab = "Canopy density", main = "Canopy Density vs Distance from Edge")
plot(ana$Sapling~ana$canopy, type = "b")
plot(ana$Invasive~ana$canopy, type = "b")
boxplot(richvars[, c(1,3,5,7,9)], xlab = "Distance from edge(20m)", ylab = "No. of sapling species", main = "Sapling Richness at Fixed Distances from Edge")
boxplot(abundvars[, c(1,3,5,7,9)], xlab = "Distance from edge(20m)", ylab = "No. of saplings", main = "Sapling Abundance at Fixed Distances from Edge")
boxplot(richvari[, c(1,3,5,7,9)], xlab = "Distance from edge(20m)", ylab = "No. of sapling species", main = "Invasive Richness at Fixed Distances from Edge")
boxplot(abundvari[, c(1,3,5,7,9)], xlab = "Distance from edge(20m)", ylab = "No. of invasives", main = "Invasive Abundance at Fixed Distances from Edge")
boxplot(richvars[, c(2,4,6,8,10)], xlab = "Distance from edge(20m)", ylab = "Canopy density", main = "Canopy Density at Fixed Distances from Edge")
boxplot(abundvars)
boxplot(richvari)
boxplot(abundvari)
abundvars
abundvari
as = ai = can = numeric(60)
b = 0
for(i in 1:12)
{
for(j in 1:5)
{
b=b+1
as[b] = abundvars[, c(1,3,5,7,9)][i, j]
ai[b] = abundvari[, c(1,3,5,7,9)][i, j]
can[b] = abundvars[, c(2,4,6,8,10)][i,j]
}
}
can = can[1:55]
plot(ai~as, col = c("brown", "red", "blue", "black", "yellow"), pch = 19, xlab = "No. of saplings", ylab = "No. of invasives", main = "Sapling Regeneration vs. Invasive species" )
abline(lm(ai~as), col = "black")
me = nls(ai ~ I(exp(1)^(a + b * as)), start =list(a = 1, b= 0), trace = T)
summary(me)$coefficients
s = seq(0, 70, length = 70)
lines(s, predict(me, list(as = s)), lty = 1, col = "blue")
lines(s, I(exp(1)^(5.95 + -0.044 * s)), lty = 1, col = "red")
l = lm(ai~as)
r1 = 1-sum(residuals(l)^2)/sum((ai-mean(ai))^2)
plot(ai[1:55]~can[1:55], col = c("brown", "red", "blue", "black", "yellow"), pch = 19, xlab = "Canopy density", ylab = "No. of Invasives", main = "Invasive numbers vs Canopy density" )
abline(lm(ai[1:55]~can[1:55]), col = "black")
l = lm(ai[1:55]~can)
r3 = 1-sum(residuals(l)^2)/sum((ai[1:55]-mean(ai[1:55]))^2)
s = seq(0, 100, length = 100)
lines(s+30, I(253 - 3.35 * s), lty = 1, col = "red")
legend(85, 370, bty = "n", c("0m", "20m", "40m", "60m", "80m"), col = c("brown", "red", "blue", "black", "yellow"), pch = 19, cex = 0.8)
legend(40, 150, bty="n", legend=paste("R3 = ", format(r4, digits=4)), cex = 0.8)
legend(47, 200, bty="n", legend=paste("y = 253 - 3.35x"), cex = 0.8)
cor(ai[1:55], can[1:55])
plot(as[1:55]~can, col = c("brown", "red", "blue", "black", "yellow"), pch = 19, xlab = "Canopy density", ylab = "No. of Saplings", main = "Sapling numbers vs Canopy density")
abline(lm(as[1:55]~can), col = "black")
me = nls(as[1:55] ~ I(exp(1)^(a + b * can)), start =list(a = 0, b= 0), trace = T)
summary(me)$coefficients
s = seq(0, 100, length = 100)
lines(s, predict(me, list(can = s)), lty = 1, col = "blue")
lines(s+35, I(exp(1)^(0.073 * s)), lty = 1, col = "red")
l = lm(as[1:55]~can)
r4 = 1-sum(residuals(l)^2)/sum((as[1:55]-mean(as[1:55]))^2)
ai[1:55][as[1:55]>exp(1)^(0.0001 + 0.073 * (can-35))+2]
as[1:55][as[1:55]>exp(1)^(0.0001 + 0.073 * (can-35))+2]
rss = sum(residuals(me)^2)
tss = sum((ai-mean(ai))^2)
r2 = 1-rss/tss
legend(40, 65, bty = "n", c("0m", "20m", "40m", "60m", "80m"), col = c("brown", "red", "blue", "black", "yellow"), pch = 19, cex = 0.8)
legend(55, 340, bty = "n", c("0m", "20m", "40m", "60m", "80m"), col = c("brown", "red", "blue", "black", "yellow"), pch = 19, cex = 0.8)
legend(50, 20, bty="n", legend=paste("R2 = ", format(r4, digits=4)), cex = 0.8)
legend(30, 50, bty="n", legend=paste("R2 = ", format(r1, digits=4)), cex = 0.8)
legend(-5, 130, bty="n", legend=paste("R2 = ", format(r2, digits=4)), cex = 0.8)
legend(15, 230, bty="n", legend=paste("y = e^(5.95 + -0.044x)"), cex = 0.8)
legend(70, 55, bty="n", legend=paste("y = e^(0.073x)"), cex = 0.8)
# p value 
f = g = h = numeric(1000)
for (i in 1:1000)
{
sam1 = sample(as, 60, replace = F)
sam2 = sample(can, 55, replace = F)
#me = nls(ai ~ I(exp(1)^(a + b * sam)), start =list(a = 1, b= 0), trace = T)
l1 = lm(ai[1:55]~sam2)
l2 = lm(as[1:55]~sam2)
#f[i] = 1-sum(residuals(me)^2)/sum((ai-mean(ai))^2)
g[i] = 1-sum(residuals(l1)^2)/sum((ai[55]-mean(ai[55]))^2)
h[i] = 1-sum(residuals(l2)^2)/sum((as[55]-mean(as[55]))^2)
}
f[f>r2]/10000
g[g>r3]/10000
h[f>r4]/10000



