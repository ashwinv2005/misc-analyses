summary(lm(log(c$Pys[1:15])~fj$Survival[1:15]))
abline(lm(log(c$Pys[1:15])~fj$Survival[1:15]))
plot(1 - fj$Survival[1:15] ~ c$Pys[1:15])
plot(1 - fj$Survival[1:15] ~ c$Pys[1:15], xlab = "Seed arrival", ylab = "Proportion removed/preyed upon")
abline(lm(1 - fj$Survival[1:15] ~ c$Pys[1:15]))
summary(lm(1 - fj$Survival[1:15] ~ c$Pys[1:15]))
plot(1 - fj$Survival[1:15] ~ log(c$Pys[1:15],10), xlab = "Log seed arrival", ylab = "Proportion removed/preyed upon")

lines(g1,g2)

abline(lm(1 - fj$Survival[1:15] ~ log(c$Pys[1:15],10)))
summary(lm(1 - fj$Survival[1:15] ~ log(c$Pys[1:15],10)))
res = 1 - fj$Survival[1:15]
dep = c$Pys[1:15]

xx = gnls(res ~ 0.45*((1 - exp(-W1*dep))/(1 + exp(-W1*dep))),
         params= list(W1~1), start = c(1))
summary(xx)
plot(xx)

g1 = seq(0,1,0.01)
g2 = 0.47*(1 - exp(-16.13*g1))/(1 + exp(-16.13*g1))

g2 = 0.38704 + 0.06348 * log(g1)
lines(g1,g2)
plot(sur ~ pys, data = g3)

g3 = data.frame(cbind(1:15),0)
g3 = g3[,-c[1,2]]
g3$sur = 1 - fj$Survival[1:15]
g3$pys = c$Pys[1:15]
an1 = nls(sur ~ a * pys ^ b, data = g3, start = list(a = 1, b = 1))
summary(an1)
g3$pys

### ignore
tail(arrdpday)
newan = summarySE(data = arrpday, measurevar = "Pys", groupvars = c("Class", "Type", "Name"))
head(newan)
head(surlong)
newan = newan[,-c(6:8)]
newan = newan[newan$Name != "PN2" & newan$Name != "BEL1" & newan$Name != "BEL2" & newan$Name != "FS4" & newan$Name != "AIL1" & newan$Name != "AIL2" & newan$Name != "ALT1" & newan$Name != "ES1" & newan$Name != "ES2",]
newan$sur = surlong$Survival
newan$pre = 1 - newan$sur
plot(newan$pre ~ newan$Pys)
### ignore

??nls



