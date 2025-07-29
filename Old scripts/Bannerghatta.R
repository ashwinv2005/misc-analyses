inf = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Dataforana.csv")
crop = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Crops.csv")
miti = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Mitigation.csv")
ancon = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Conflict.csv")
ancon2 = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Conflictm.csv")
inf = inf[, -c(39)]
miti = miti[, -c(11,12)]
ancon1 = ancon[, c(1,2,3,7,11,15,19,23,27,31,35)]
crop$range.code = miti$range.code = ancon2$range.code = inf$range.code
names(inf)
names(crop)
names(miti)
names(ancon2)
names(range)
summary(ancon2)
ancon2[, 12] = numeric(73)
a = 0
for(i in 3:11)
{
a = a + sum(ancon2[, i])
}
for(i in 3:11)
{
ancon2[, 12] = ancon2[, 12] + ancon2[, i]*sum(ancon2[, i])/a
}
for(i in 1:73)
{
ancon2[i, 12] = ancon2[i, 12]/sum(ancon2[i, 3:11])
}
ancon2[c(43,68), 12] = 0
names(ancon2)[12] = "norm"
inf[, 39] = numeric(73)
a = 0
for(i in 18:21)
{
a = a + sum(inf[, i])
}
for(i in 18:21)
{
inf[, 39] = inf[, 39] + inf[, i]*sum(inf[, i])/a
}
for(i in 1:73)
{
if(sum(inf[i, 18:21]) != 0)
{
inf[i, 39] = inf[i, 39]/sum(inf[i, 18:21])
}
else
{
inf[i, 39]
}
}
inf[, 39]
names(inf)[39] = "norm"
names(inf)
vill = data.frame(cbind(1:28,0))
range = data.frame(cbind(1:3,0))
vil = unique(inf$village)
vill[,1] = 1:28
vill[,2] = vil
range[,1] = 1:3
for(i in 1:28)
{
vill[i,3] = mean(inf[inf$village == vil[i], ]$conf.ext)
vill[i,4] = mean(inf[inf$village == vil[i], ]$conf.chg)
vill[i,5] = mean(inf[inf$village == vil[i], ]$amt.sp)
vill[i,6] = table(inf[inf$village == vil[i], ]$compen)[2]/length(inf[inf$village == vil[i], ]$compen)
}
for(j in 1:3)
{
range[j,2] = mean(inf[inf$range.code == j, ]$conf.ext)
range[j,3] = sd(inf[inf$range.code == j, ]$conf.ext)/sqrt(length(inf[inf$range.code == j, ]$conf.ext))
range[j,4] = mean(inf[inf$range.code == j, ]$conf.chg)
range[j,5] = sd(inf[inf$range.code == j, ]$conf.chg)/sqrt(length(inf[inf$range.code == j, ]$conf.chg))
range[j,6] = table(inf[inf$range.code == j, ]$compen)[2]/length(inf[inf$range.code == j, ]$compen)
}
names(vill) = c("sl.no", "village", "conf.ext", "conf.chg", "amt.sp", "pr.comp")
names(range) = c("sl.no", "conf.ext", "SEe", "conf.chg", "SEc", "pr.comp")
range
miti[, 11] = numeric(73)
miti[, 12] = numeric(73)
c = numeric(8)
a = b = 0
for(i in 3:6)
{
a = a + table(miti[, i])[2]
b = b + table(miti[, i+4])[2]
}
for(i in 3:6)
{
miti[, 11] = miti[, 11] + miti[, i]*table(miti[, i])[2]/a
miti[, 12] = miti[, 12] + miti[, i+4]*table(miti[, i+4])[2]/b
c[i-2] = table(miti[, i])[2]/a
c[i + 2] = table(miti[, i+4])[2]/b
}
for(i in 1:73)
{
if(sum(miti[i, 3:6]) != 0)
{
miti[i, 11] = miti[i, 11]/sum(miti[i, 3:6])
}
else
{
miti[i, 11]
}
if(sum(miti[i, 7:10]) != 0)
{
miti[i, 12] = miti[i, 12]/sum(miti[i, 7:10])
}
else
{
miti[i, 12]
}
}
names(miti)[c(11,12)] = c("normo", "normf")
miti$norm = (miti$normo*a+miti$normf*b)/(a+b)
d = numeric(4)
for(i in 1:4)
{
d[i] = (c[i]*a + c[i+4]*b)/(a+b)
}
d
plot(inf$conf.chg ~ miti$norm)
range[1:3, 7] = c("1-2", "2-3", "3-1")
#p value
x = y = z = numeric(10000)
a = inf[inf$range.code == 1, ]$conf.ext
b = inf[inf$range.code == 2, ]$conf.ext
c = inf[inf$range.code == 3, ]$conf.ext
d = a
d[(length(a)+1):(length(a)+length(b))] = b
e = b
e[(length(b)+1):(length(b)+length(c))] = c
f = c
f[(length(c)+1):(length(c)+length(a))] = a
for (i in 1:10000)
{
j = sample(d, length(a), F)
k = sample(e, length(b), F)
l = sample(f, length(c), F)
x[i] = mean(j)-(sum(d)-sum(j))/length(b)
y[i] = mean(k)-(sum(e)-sum(k))/length(c)
z[i] = mean(l)-(sum(f)-sum(l))/length(a)
}
range[1, 8] = length(x[x<(range$conf.ext[1] - range$conf.ext[2]) | x>(-range$conf.ext[1] + range$conf.ext[2])])/10000
range[2, 8] = length(y[y<(-range$conf.ext[2] + range$conf.ext[3]) | y>(range$conf.ext[2] - range$conf.ext[3])])/10000
range[3, 8] = length(z[z<(range$conf.ext[3] - range$conf.ext[1]) | z>(-range$conf.ext[3] + range$conf.ext[1])])/10000
#p value 2
x = y = z = numeric(10000)
a = inf[inf$range.code == 1, ]$conf.chg
b = inf[inf$range.code == 2, ]$conf.chg
c = inf[inf$range.code == 3, ]$conf.chg
d = a
d[(length(a)+1):(length(a)+length(b))] = b
e = b
e[(length(b)+1):(length(b)+length(c))] = c
f = c
f[(length(c)+1):(length(c)+length(a))] = a
for (i in 1:10000)
{
j = sample(d, length(a), F)
k = sample(e, length(b), F)
l = sample(f, length(c), F)
x[i] = mean(j)-(sum(d)-sum(j))/length(b)
y[i] = mean(k)-(sum(e)-sum(k))/length(c)
z[i] = mean(l)-(sum(f)-sum(l))/length(a)
}
range[1, 9] = length(x[x<(range$conf.chg[1] - range$conf.chg[2]) | x>(-range$conf.chg[1] + range$conf.chg[2])])/10000
range[2, 9] = length(y[y<(-range$conf.chg[2] + range$conf.chg[3]) | y>(range$conf.chg[2] - range$conf.chg[3])])/10000
range[3, 9] = length(z[z<(range$conf.chg[3] - range$conf.chg[1]) | z>(-range$conf.chg[3] + range$conf.chg[1])])/10000
names(range)[7:9] = c("sl", "pval.ext", "pval.chg")
#p value 3
x = y = z = numeric(10000)
a = inf[inf$range.code == 1, ]$compen
b = inf[inf$range.code == 2, ]$compen
c = inf[inf$range.code == 3, ]$compen
d = a
d[(length(a)+1):(length(a)+length(b))] = b
e = b
e[(length(b)+1):(length(b)+length(c))] = c
f = c
f[(length(c)+1):(length(c)+length(a))] = a
for (i in 1:10000)
{
j = sample(d, length(a), F)
k = sample(e, length(b), F)
l = sample(f, length(c), F)
x[i] = table(j)[2]/length(j)-(table(d)[2]-table(j)[2])/length(b)
y[i] = table(k)[2]/length(k)-(table(e)[2]-table(k)[2])/length(c)
z[i] = table(l)[2]/length(l)-(table(f)[2]-table(l)[2])/length(a)
}
range[1, 10] = length(x[x<(-range$pr.comp[1] + range$pr.comp[2]) | x>(range$pr.comp[1] - range$pr.comp[2])])/10000
range[2, 10] = length(y[y<(range$pr.comp[2] - range$pr.comp[3]) | y>(-range$pr.comp[2] + range$pr.comp[3])])/10000
range[3, 10] = length(z[z<(range$pr.comp[3] - range$pr.comp[1]) | z>(-range$pr.comp[3] + range$pr.comp[1])])/10000
names(range)[10] = "pval.comp"
ancon2$norm
freq = read.csv("D:/Wildlife/Social Sciences/Bannerghatta/Conflict edited.csv")
miti$range.code = freq$range.code = crop$range.code 
freq$conf.ext = inf$conf.ext
freq$conf.chg = inf$conf.chg
freq$norm = miti$norm
freq = freq[!is.na(freq$Ele), ]
boxplot(freq$Ele~freq$conf.ext)
miti$solar = miti$o.solar+miti$f.solar
miti$nitw = miti$o.nitw + miti$f.nitw
miti$frcrk = miti$o.frcrk + miti$f.frcrk
miti$others = miti$o.others + miti$f.others
for (i in 1:73)
{if(miti$frcrk[i] == 2){miti$frcrk[i] = 1}}
for (i in 1:73)
{if(miti$others[i] == 2){miti$others[i] = 1}}
for (i in 1:73)
{if(miti$nitw[i] == 2){miti$nitw[i] = 1}}
sol = with(miti, tapply(X = (o.solar+f.solar), range.code , function(x) table(x)[2])) 
for (i in 1:3)
{sol[i] = sol[i]/length(inf[inf$range.code == i, ]$sl.no)}
range$sol = sol
range
#p value 4
x = y = z = numeric(10000)
a = miti[miti$range.code == 1, ]$solar
b = miti[miti$range.code == 2, ]$solar
c = miti[miti$range.code == 3, ]$solar
d = a
d[(length(a)+1):(length(a)+length(b))] = b
e = b
e[(length(b)+1):(length(b)+length(c))] = c
f = c
f[(length(c)+1):(length(c)+length(a))] = a
for (i in 1:10000)
{
j = sample(d, length(a), F)
k = sample(e, length(b), F)
l = sample(f, length(c), F)
x[i] = table(j)[2]/length(j)-(table(d)[2]-table(j)[2])/length(b)
y[i] = table(k)[2]/length(k)-(table(e)[2]-table(k)[2])/length(c)
z[i] = table(l)[2]/length(l)-(table(f)[2]-table(l)[2])/length(a)
}
range[1, 12] = length(x[x<(-range$sol[1] + range$sol[2]) | x>(range$sol[1] - range$sol[2])])/10000
range[2, 12] = length(y[y<(range$sol[2] - range$sol[3]) | y>(-range$sol[2] + range$sol[3])])/10000
range[3, 12] = length(z[z<(-range$sol[3] + range$sol[1]) | z>(range$sol[3] - range$sol[1])])/10000
names(range)[12] = "pval.sol"
miti$wt = (miti$nitw*1 + miti$frcrk*2 + miti$others*3 + miti$solar*4)/10
miti$wto = (miti$o.nitw*1 + miti$o.frcrk*2 + miti$o.others*3 + miti$o.solar*4)/10
boxplot(miti$wt~inf$conf.chg, main = "Mitigation Index against Change in Conflict", xlab = "Change in Conflict", ylab = "Index of Mitigation")
plot(miti$wto~inf$norm)
range$miti = with(miti, tapply(wt, range.code, mean)) 
miti$others = miti$f.others+miti$o.others
names(miti)
mat = matrix(,4,3,dimnames = list(c("solar","nitw","frcrk","others"),c("Decrease",2,"Increase")))
mat[1,] = with(miti[, c(15:18, 21)], tapply(solar, conf.chg, sum)) 
mat[2,] = with(miti[, c(15:18, 21)], tapply(nitw, conf.chg, sum)) 
mat[3,] = with(miti[, c(15:18, 21)], tapply(frcrk, conf.chg, sum)) 
mat[4,] = with(miti[, c(15:18, 21)], tapply(others, conf.chg, sum)) 
miti$conf.chg = inf$conf.chg
for(i in c(1,3))
{mat[,i] = mat[,i]/length(miti[miti$conf.chg == i,]$sl.no)}
mat = mat[,-2]
barplot(mat, main = "Mitigation Measures against Change in Conflict", xlab = "Conflict", ylab = "Mitigation Measure", beside = T, col = c("slateblue", "darkslategrey", "lightslategrey", "grey"))
legend("top", bty = "n", c("Solar", "Night Watch", "Firecrackers", "Others"), col = c("slateblue", "darkslategrey", "lightslategrey", "grey"), pch = 19, cex = 0.8)
write.csv(range, "D:/Wildlife/Social Sciences/Bannerghatta/range.csv")
#p value 5
x = y = z = numeric(10000)
a = miti[miti$conf.chg == 1, ]$wt
b = miti[miti$conf.chg == 2, ]$wt
c = miti[miti$conf.chg == 3, ]$wt
d = a
d[(length(a)+1):(length(a)+length(b))] = b
e = b
e[(length(b)+1):(length(b)+length(c))] = c
f = c
f[(length(c)+1):(length(c)+length(a))] = a
for (i in 1:10000)
{
j = sample(d, length(a), F)
k = sample(e, length(b), F)
l = sample(f, length(c), F)
x[i] = mean(j)-(sum(d)-sum(j))/length(b)
y[i] = mean(k)-(sum(e)-sum(k))/length(c)
z[i] = mean(l)-(sum(f)-sum(l))/length(a)
}
mean(a)-mean(b)
length(x[x < -(mean(a)-mean(b)) | x > (mean(a)-mean(b))])/10000
length(y[y < -(mean(b)-mean(c)) | y > (mean(b)-mean(c))])/10000
length(z[z < -(mean(a)-mean(c)) | z > (mean(a)-mean(c))])/10000










