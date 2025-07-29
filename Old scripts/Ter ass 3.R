ter = read.csv("D:/Wildlife/R/termite-final.csv")
top = ter[ter$hole.pos == "T", ]
side = ter[ter$hole.pos == "S", ]
comb = data.frame(cbind(1:35, 0))
comb$repairt = top$full.time - top$first.resp
comb$repairs = side$full.time - side$first.resp
comb$diff = comb$repairt - comb$repairs
comb = comb[, -c(1,2)]
comb = comb[-c(11,16), ]
rep = 10000
N = length(comb$diff)
comb$diff1 = ifelse(comb$diff >0, "Y", "N")

n = table(comb$diff1)[1]
binp = function(x,N,p){(factorial(N)/(factorial(x)*factorial(N-x)))*(.5^x)*(.5^(N-x))}
prob = binp(0:N, N, 0.5)
plot(0:N, prob, type = "l", main = "Binomial(Using formula)", xlab = "T/S(Observed = 22/11)", ylab = "Probability")
abline(v = c(11,22), col = "red")
c = cumsum(prob)
plot(c, type = "l", main = "Binomial(Using formula)", xlab = "T/S(Observed = 22/11)", ylab = "Cumulative probability")
abline(v = c(11,22), col = "red")
c[n+1] + 1 - c[N-n+1]


dat = rep(c("Y", "N"), c(50, 50))
n = table(comb$diff1)[1]
simu = numeric(rep)
for(i in 1:rep)
{
sim = sample(dat, N, replace = TRUE)
simu[i] = table(sim)[2]
}
hist(simu, main = "Simulation of NULL hypothesis(Y or N)", xlab = "T/S(Observed = 22/11)")
abline(v = c(11,22), col = "red")
length(simu[simu <= n | simu >= (N-n)])/rep
quantile(simu, c(0.025, 0.5, 0.975))


t = mean(comb$diff)*sqrt(N)/sd(comb$diff)
pt(-t, (N-1)) + 1 - pt(t,(N-1))


diff = numeric(N)
mean1 = numeric(rep)
for(i in 1:rep)
{
sign = rep(c(-1, 1), each = length(comb$diff)/2)
samp = sample(sign, N, replace = TRUE)
diff = samp*comb$diff
mean1[i] = mean(diff)
}
hist(mean1, main = "Simulation of NULL hypothesis(T-S)", xlab = "T-S(Observed = 4.818)")
abline(v = c(-mean(comb$diff), mean(comb$diff)), col = "red")
quantile(mean1, c(0.025, 0.5, 0.975))
length(mean1[mean1 <= -mean(comb$diff) | mean1 >= mean(comb$diff)])/rep


plot(comb$repairt ~ comb$repairs, main = "Repair Time for Top vs Side", xlab = "Side Repair Time", ylab = "Top Repair Time")
abline(lm(comb$repairt ~ comb$repairs), col = "red")
meant = mean(comb$repairt)
means = mean(comb$repairs)
precov = 0
for (i in 1:N){precov = precov + (comb$repairt[i] - meant)*(comb$repairs[i] - means)}
cov = precov/(N-1)
r = cov/(sd(comb$repairt)*sd(comb$repairs))
t = r/sqrt((1-r^2)/(N-2))
pt(-t, (N-2)) + 1 - pt(t, (N-2))


r1 = numeric(rep)
temp = numeric(N)
meant = mean(comb$repairt)
means = mean(comb$repairs)
for (i in 1:rep)
{
temp = sample(comb$repairs, 33, replace = TRUE)
meantemp = mean(temp)
precov = 0
for (j in 1:N){precov = precov + (comb$repairt[j] - meant)*(temp[j] - meantemp)}
cov = precov/(N-1)
r1[i] = cov/(sd(comb$repairt)*sd(temp))
}
precov = 0
for (i in 1:N){precov = precov + (comb$repairt[i] - meant)*(comb$repairs[i] - means)}
cov = precov/(N-1)
r = cov/(sd(comb$repairt)*sd(comb$repairs))
hist(r1, main = "Simulation of NULL hypothesis(r)", xlab = "r(Observed = 0.5885)")
abline(v = c(-r,r), col = "red")
length(r1[r1 <= -r | r1 >= r])/rep
quantile(r1, c(0.025, 0.5, 0.975))





