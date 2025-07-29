c = 24:100
b = 1:60
which(c<50)
c[which(b < 25)]
c[b < 50]
a = matrix(1:25, nrow = 5, ncol = 5)
a
dat = read.csv("C:/Documents and Settings/CMDigitalVideo/My Documents/Downloads/example.csv")
dat
dat
getwd()
setwd("D:/R/")
dat = read.csv("example.csv")
dat
names(dat)
head(dat)
getwd()
tail(dat)
str(dat)
a = dat$Age
a
Age[which(a<7)]
which(a<7)
attach(dat)
Age
ls()
attach(dat)
names(dat)
b = dat$Mass
c = dat$Tarsus
cat = dat[1:10,1:3]
cat
b+c
names(cat)
str(cat)
tail(cat)
dat[1:20,c("Age","Mass")]
b*c
b-c
a[10]
c
a
dat[1:10,]
head(dat,10)
b[b<15]
which(b<15)
which(dat$Sex == "M")
length(a)
mean(a)
mean(b)
var(a)
sd(a)
var(b)
sd(b)
mean(c)
var(c)
sd(c)
sqrt(6)
3.12^3
length(dat)
sd(a)/sqrt(length(a))
mat
sd(b)/sqrt(length(b))
length(mat)
c[c<=mean(c)]



