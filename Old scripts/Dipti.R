a = read.csv("C:/Users/Ashwin/Desktop/Maths.csv")
names(a)
## method 2
y = cumsum(a$people)
z = numeric(sum(a$people))

j = 1
for (i in 1:sum(a$people))
{
  if (y[j]==i)
  {
    z[i] = a$cite[j]
    j=j+1
    next
  }    
  z[i] = a$cite[j]
}
hist(z)
exp = exp.fit(z,1)
pow = pareto.fit(z,1)
powexp = powerexp.fit(z)
power.powerexp.lrt(pow,powexp)
vuong(pareto.exp.llr(z, pow, exp))