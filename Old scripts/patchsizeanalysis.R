data = read.csv("C:/Ashwin/CSV/Veg/psize2f.csv")
area = read.csv("C:/Ashwin/CSV/Veg/xfin4.csv")
data1 = read.csv("C:/Ashwin/CSV/Veg/psizec1.csv")
data = as.vector(data[,1])
area = as.vector(area[,1])
hist(data)
for (i in 1:length(data)) 
  {
  if (data[i] == 0)
    data[i] = 1
}
data1 = as.vector(data1[,1])
plot(data1,data)
plot(td1,td)
plot(log(td1),log(td))
td = data[data!=0]
td1 = data1[data!=0]

ubin = sort(unique(data))
lpar = length(data)
lubin = length(ubin)
cont = numeric(lubin)

for (i in 1:lpar)
{
  for (j in 1:lubin)
  {
    if (data[i] == ubin[j])
      cont[j] = cont[j] + 1
  }
}

plot(log(cont)~log(ubin))
potest = lm(log(cont)~log(ubin))
abline(potest)
summary(potest)

plot(log(cont)~ubin)
extest = lm(log(cont)~ubin)
abline(extest)
summary(extest)

## MLE tests

minx = min(data)

pow = conpl$new(data)
pow$setXmin(minx)
est1 = estimate_pars(pow)
pow$setPars(est1$pars)
##minx = estimate_xmin(pow)

exp = conexp$new(data)
exp$setXmin(minx)
est2 = estimate_pars(exp)
exp$setPars(est2$pars)

lnorm = conlnorm$new(data)
lnorm$setXmin(minx)
est3 = estimate_pars(lnorm)
lnorm$setPars(est3$pars)

data = rpowerexp(1000,1,1.5,0.1)

exp1 = exp.fit(data,minx)
pow1 = pareto.fit(data,minx)
lnorm1 = lnorm.fit(data,minx)
powexp = powerexp.fit(data,minx)

pares = power.powerexp.lrt(pow1,powexp)
exes = exp.powerexp.lrt(exp1,powexp)
vuong(lnorm.powerexp.llr(data, lnorm1, powexp, minx))

com = compare_distributions(pow,lnorm)
com$test_statistic
com$p_two_sided
com$p_one_sided
plot(com)

plot(pow)
lines(pow)
dist_ll(lnorm)

gra = lines(pow)
head(gra)
length(gra$y)
plot(sort(data, decreasing = T))
hist(data,100)

plot(data1,data)