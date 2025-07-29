dat = read.csv("D:/statexam.csv")
names(dat)
# Q1
SE = function(x)
{
sd(x)/sqrt(length(x))
}
# Q2
bimean = with(dat, tapply(Mass, Sex, mean))
bisd = with(dat, tapply(Mass, Sex, sd))
bise = with(dat, tapply(Mass, Sex, SE))
bisamp = with(dat, tapply(Mass, Sex, length))
# Q3
male = dat[dat$Sex == "M", ]
female = dat[dat$Sex == "F", ]
a = numeric(1000)
b = numeric(1000)
for(i in 1:1000)
{
a[i] = mean(sample(male$Mass, 30, T))
b[i] = mean(sample(female$Mass, 30, T))
}
mean(a)
sd(a)
mean(b)
sd(b)
# Q4
par(mfrow = c(1,2))
hist(dat$Mass, main = "Frequency of Mass", xlab = "Mass")
hist(dat$Tarsus, main = "Frequency of Tarsus", xlab = "Tarsus")
# Q5
plot(dat$Mass~dat$Age, main = "Mass vs Age", xlab = "Age", ylab = "Mass", col = dat$Sex)
abline(lm(dat$Mass~dat$Age))
summary(lm(dat$Mass~dat$Age))
# Q6
boxplot(dat$Mass~dat$Parasites, main = "Mass with or without Parasites", xlab = "Parasite", ylab = "Mass")
# Q7
mod = lm(Mass~Age*Sex, data = dat) 
summary(mod)













