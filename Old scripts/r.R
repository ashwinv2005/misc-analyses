dat = read.csv("D:/R/example.csv")
dat
a = dat$Age
b = dat$Mass
c = dat$Tarsus
getwd()
str(dat)
var(a)
subset(dat, Sex == "M")
male = dat[dat$Sex == "M", ]
female = dat[dat$Sex == "F", ]
subset(dat, Mass == 21.20516)
head(female, 10)
inf = subset(dat, Sex == "M" | Parasites == "Yes")
inf
int = dat[dat$Sex == "M" & dat$Parasites == "Yes", ]
int
d = dat[which(dat$Sex == "M" | dat$Parasites == "Yes") , ]
d
e = dat[dat$Age >=  1 & dat$Age <=5, ] 
e
e = dat[dat$Tarsus > mean(dat$Tarsus), ]
dat[dat$Parasites != "Yes", ]
sqrt(dat$Age)
abs(10/7)

for(i in 2:100)
{
k = 0
for(j in 2:i)
{
if(i != j & round(i/j) == i/j)
{
k = 1
}
}
if(k == 0)
{print(i)}
}

inf
attach(dat)
sample(dat$Mass, 70, replace = T)
dat
plot(Tarsus, Mass)
par(bg = "yellow")
plot(Mass ~ Tarsus, xlab = "Tarsus length", ylab = "Mass value", col = ifelse(Sex == "M", "green", "red"))
col = ifelse(Sex == "M", "green", "pink")
col
? plot
col = 		
plot(Mass, Sex)
volcano
library(lattice)
wireframe(volcano, col = "red")


















