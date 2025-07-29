alpha = 2; k1 = 0.1
alpha = 2; k2 = 1.5
alpha = 2; xcut = 20; k3 = 1.5; k4 = ((k3-1)/xcut)*log(xcut/alpha)
parsam = sample(2:100000,100,replace = T, prob = (dexp(2:100000,k1))/exp(-alpha*k1))
parsam = sample(2:100000,100,replace = T, prob = dpareto(2:100000,alpha,k2))
parsam = sample(2:100000,1000,replace = T, prob = c(dpareto(2:xcut,alpha,k3),dexp((xcut+1):100000,k4)))

ubin = sort(unique(parsam))
lpar = length(parsam)
lubin = length(ubin)
cont = numeric(lubin)

for (i in 1:lpar)
{
  for (j in 1:lubin)
  {
    if (parsam[i] == ubin[j])
      cont[j] = cont[j] + 1
  }
}

plot(cont~ubin)

potest = lm(log(cont)~log(ubin))
extest = lm(log(cont)~ubin)
y = log(cont)
x = log(ubin)
powexptest = nls(y~a2+b2*x+c2*exp(x),start=list(a2=0,b2=1,c2=1))

summary(potest)
summary(extest)
AIC(potest,extest,powexptest)
plot(log(cont)~log(ubin))
plot(log(cont)~ubin)

exp = exp.fit(parsam,2)
pow = pareto.fit(parsam,2)
powexp = powerexp.fit(parsam,2)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(parsam, pow, exp))

plot(2:100000, prob)
y = log(cont)
x = log(ubin)
powexptest = nls(y~a2+b2*x+c2*exp(x),start=list(a2=0,b2=1,c2=1))
summary(powexptest)

dfg = rpareto(1000,1,1.5)
max(dfg)
hist(dfg,1000)
plot(sort(dfg))