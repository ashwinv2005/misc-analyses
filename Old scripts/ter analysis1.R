ter = read.csv("D:/Wildlife/R/termite-final.csv")
top = ter[ter$hole.pos == "T", ]
side = ter[ter$hole.pos == "S", ]
ridge = ter[ter$hole.pos == "R", ]
valley = ter[ter$hole.pos == "V", ]
names(ter)
ter = ter[, -c(13:18)]
ter$repair.time = ter$full.time - ter$first.resp
ter = ter[!is.na(ter$repair.time), ]
plot(repair.time ~ area, data = ter[ter$area<14, ], col = hole.pos, pch = 16, ex = 0.5)
plot(repair.time ~ area, log = "x", data = ter)
ter$vol = (ter$circum^2)*ter$height/(12*pi)
par(mfrow = c(2,2), mar = c(4,4,2,1))
for(i in c("T", "S", "R", "V"))
{
plot(repair.time ~ I(vol/10^6), data = ter[ter$hole.pos == i, ], main = i,col=hole.pos,pch=16,cex=0.75)
with(ter[ter$hole.pos == i, ], abline(lm(repair.time ~ I(vol/10^6))))
with(ter[ter$hole.pos == i, ], cor(repair.time, I(vol/10^6)))
}
abline(c(15,40), col = "red")
abline(c(3,50), col = "blue")
terv = ter[ter$hole.pos == "V", ]
38/1.4
ssd.fun = function(int, slope, y, x)
{
y.pred = int + slope*x 
sum((y - y.pred)^2)
}
ssd.fun(15, 40, terv$repair.time, I(terv$vol/10^6))
ssd.fun(3, 50, terv$repair.time, I(terv$vol/10^6))
ssd.fun(12.61, 28.86, terv$repair.time, I(terv$vol/10^6))
b = sum((terv$repair.time - mean(terv$repair.time))*(I(terv$vol/10^6)-mean(I(terv$vol/10^6))))/sum((I(terv$vol/10^6) - mean(I(terv$vol/10^6)))^2)
c = mean(terv$repair.time) - b*mean(I(terv$vol/10^6))
y = c + b*I(terv$vol/10^6)
sum((y - mean(terv$repair.time))^2)/sum((terv$repair.time - mean(terv$repair.time))^2)
?heatmap
cor(terv$repair.time, I(terv$vol/10^6))^2
a = lm(terv$repair.time ~ I(terv$vol/10^6))
d = data.frame(cbind(1:50, 1:50))
for(i in 1:50)
{
for(j in 1:50)
{
d[i,j] = ssd.fun(i, j, terv$repair.time, I(terv$vol/10^6))
}
}
x = data.matrix(d)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)
heatmap(x, RowSideColors = rc, ColSideColors = cc)
min(x)
summary(a)
ssd.fun(coef(a)[1], coef(a)[2], terv$repair.time, I(terv$vol/10^6)) 
new1 = runif(26)
new2 = runif(26)
plot(new2 ~ new1)
terv$new1 = new1
terv$new2 = new2
head(terv)
a1 = lm(repair.time ~ I(vol/10^6) + new1, data = terv)
a2 = lm(repair.time ~ I(vol/10^6) + new2, data = terv)
summary(a1)$r.square
summary(a2)$r.square
calc.pred = coef(a)[1] + coef(a)[2]*I(terv$vol/10^6) 
round(calc.pred, 5) == round(predict(a), 5)
resid = terv$repair.time - predict(a)
hist(resid)
plot(resid ~ predict(a))
plot(runif(10) ~ runif(10))