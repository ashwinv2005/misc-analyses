## exponential

## sample size 10
x = seq(2,100000,10000)
y = 1000*exp(-.0001*x)
y1 = round(y)
y2 = numeric(10)
for (i in 1:10)
{ 
  if(y1[i]!=0)
  y2[i] = rpois(1,y1[i])
  else
  y2[i] = y1[i]
}

## method 1
pow = lm(log(y2+1)~log(x))
summary(pow)
exp = lm(log(y2+1)~x)
summary(exp)
#powexp = nls((y2+1)~a2-b2*x^c2-exp(x)/Sx,start=list(a2=1,b2=5,c2=1,Sx=10))
AIC(pow,exp)

## method 2
y3 = cumsum(y2)
z = numeric(sum(y2))
j = 1
for (i in 1:sum(y2))
{
  if (y3[j]==i)
  {
    z[i] = x[j]
    j=j+1
    next
  }    
  z[i] = x[j]
}
exp = exp.fit(z)
pow = pareto.fit(z,2)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))
plot(x,dpareto(x, 2, 1.294715), type = "l")
plot(x,dexp(x, 0.000164), type = "l")

## sample size 100
x = seq(2,100000,1000)
y = 1000*exp(-.0001*x)
y1 = round(y)
y2 = numeric(100)
for (i in 1:100)
{ 
  if(y1[i]!=0)
    y2[i] = rpois(1,y1[i])
  else
    y2[i] = y1[i]
}

## method 1
pow = lm(log(y2+1)~log(x))
summary(pow)
exp = lm(log(y2+1)~x)
summary(exp)
#powexp = nls((y2+1)~a2-b2*x^c2-exp(x)/Sx,start=list(a2=1,b2=5,c2=1,Sx=10))
AIC(pow,exp)

## method 2
y3 = cumsum(y2)
z = numeric(sum(y2))
j = 1
for (i in 1:sum(y2))
{
  if (y3[j]==i)
  {
    z[i] = x[j]
    j=j+1
    next
  }    
  z[i] = x[j]
}
hist(z)
exp = exp.fit(z)
pow = pareto.fit(z,2)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))

100000/2200
length(x)
warnings()

## sample size 47
x = seq(2,100000,2200)
y = 1000*exp(-.0001*x)
y1 = round(y)
y2 = numeric(46)
for (i in 1:46)
{ 
  if(y1[i]!=0)
    y2[i] = rpois(1,y1[i])
  else
    y2[i] = y1[i]
}

## method 1
pow = lm(log(y2+1)~log(x))
summary(pow)
exp = lm(log(y2+1)~x)
summary(exp)
#powexp = nls((y2+1)~a2-b2*x^c2-exp(x)/Sx,start=list(a2=1,b2=5,c2=1,Sx=10))
AIC(pow,exp)

## method 2
y3 = cumsum(y2)
z = numeric(sum(y2))
j = 1
for (i in 1:sum(y2))
{
  if (y3[j]==i)
  {
    z[i] = x[j]
    j=j+1
    next
  }    
  z[i] = x[j]
}
hist(z)
exp = exp.fit(z)
pow = pareto.fit(z,2)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))

## pareto

## sample size 10
x = seq(2,100000,10000)
y = 100000*x^-1.1
y1 = round(y)
y2 = numeric(10)
for (i in 1:10)
{ 
  if(y1[i]!=0)
    y2[i] = rpois(1,y1[i])
  else
    y2[i] = y1[i]
}

## method 1
pow = lm(log(y2+1)~log(x))
summary(pow)
exp = lm(log(y2+1)~x)
summary(exp)
#powexp = nls((y2+1)~a2-b2*x^c2-exp(x)/Sx,start=list(a2=1,b2=5,c2=1,Sx=10))
AIC(pow,exp)

## method 2
y3 = cumsum(y2)
z = numeric(sum(y2))
j = 1
for (i in 1:sum(y2))
{
  if (y3[j]==i)
  {
    z[i] = x[j]
    j=j+1
    next
  }    
  z[i] = x[j]
}
exp = exp.fit(z)
pow = pareto.fit(z,2)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))

## sample size 100
x = seq(2,100000,1000)
y = 150000*x^-1.1
y1 = round(y+1)
y2 = numeric(100)
for (i in 1:100)
{ 
  if(y1[i]!=0)
    y2[i] = rpois(1,y1[i])
  else
    y2[i] = y1[i]
}

## method 1
pow = lm(log(y2+1)~log(x))
summary(pow)
exp = lm(log(y2+1)~x)
summary(exp)
#powexp = nls((y2+1)~a2-b2*x^c2-exp(x)/Sx,start=list(a2=1,b2=5,c2=1,Sx=10))
AIC(pow,exp)

## method 2
y3 = cumsum(y2)
z = numeric(sum(y2))
j = 1
for (i in 1:sum(y2))
{
  if (y3[j]==i)
  {
    z[i] = x[j]
    j=j+1
    next
  }    
  z[i] = x[j]
}
exp = exp.fit(z)
pow = pareto.fit(z,2)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))

parsam = sample(2:100000,10,replace = T, prob = dpareto(2:100000,alpha,k))
hist(parsam)
plot(2:100000,dpareto(2:100000,alpha,k))
