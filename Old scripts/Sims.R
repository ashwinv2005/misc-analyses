pardf3$Value = 0
temp1 = 10000
cont = 0
alpha = 2
for (k2 in seq(1.1,3.08,0.02))
{
  for (samp in seq(20,1010,10))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp, replace = T, prob = dpareto(2:temp1,alpha,k2))
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided >= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        pardf3$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        pardf3$Value[cont+10000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        pardf3$Value[cont+20000] = table(cth)[z]/100
    }
    pardf3$K[cont] = k2
    pardf3$K[cont+10000] = k2
    pardf3$K[cont+20000] = k2
    pardf3$Sam[cont] = samp
    pardf3$Sam[cont+10000] = samp
    pardf3$Sam[cont+20000] = samp
    print(samp)
  }
  print(k2)
}

pardf4$Value = 0
temp1 = 100000
cont = 0
alpha = 2
for (k2 in seq(1.1,3.08,0.02))
{
  for (samp in seq(20,1010,10))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp, replace = T, prob = dpareto(2:temp1,alpha,k2))
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided >= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        pardf4$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        pardf4$Value[cont+10000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        pardf4$Value[cont+20000] = table(cth)[z]/100
    }
    pardf4$K[cont] = k2
    pardf4$K[cont+10000] = k2
    pardf4$K[cont+20000] = k2
    pardf4$Sam[cont] = samp
    pardf4$Sam[cont+10000] = samp
    pardf4$Sam[cont+20000] = samp
    print(samp)
  }
  print(k2)
}

expdf1$Value = 0
temp1 = 100
cont = 0
alpha = 2
for (k1 in seq(0.02,0.8,0.02))
{
  for (samp in seq(20,2000,20))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dexp(2:temp1,k1))/exp(-alpha*k1))      
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided > 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        expdf1$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        expdf1$Value[cont+4000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        expdf1$Value[cont+8000] = table(cth)[z]/100
    }
    expdf1$K[cont] = k1
    expdf1$K[cont+4000] = k1
    expdf1$K[cont+8000] = k1
    expdf1$Sam[cont] = samp
    expdf1$Sam[cont+4000] = samp
    expdf1$Sam[cont+8000] = samp
    print(samp)
  }
  print(k1)
}

expdf2$Value = 0
temp1 = 1000
cont = 3400
alpha = 2
for (k1 in seq(0.7,0.8,0.02))
{
  for (samp in seq(20,2000,20))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dexp(2:temp1,k1))/exp(-alpha*k1))      
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided > 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        expdf2$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        expdf2$Value[cont+4000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        expdf2$Value[cont+8000] = table(cth)[z]/100
    }
    expdf2$K[cont] = k1
    expdf2$K[cont+4000] = k1
    expdf2$K[cont+8000] = k1
    expdf2$Sam[cont] = samp
    expdf2$Sam[cont+4000] = samp
    expdf2$Sam[cont+8000] = samp
    print(samp)
  }
  print(k1)
}

expdf3$Value = 0
temp1 = 10000
cont = 0
alpha = 2
for (k1 in seq(0.02,0.8,0.02))
{
  for (samp in seq(20,2000,20))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dexp(2:temp1,k1))/exp(-alpha*k1))      
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided > 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        expdf3$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        expdf3$Value[cont+4000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        expdf3$Value[cont+8000] = table(cth)[z]/100
    }
    expdf3$K[cont] = k1
    expdf3$K[cont+4000] = k1
    expdf3$K[cont+8000] = k1
    expdf3$Sam[cont] = samp
    expdf3$Sam[cont+4000] = samp
    expdf3$Sam[cont+8000] = samp
    print(samp)
  }
  print(k1)
}

expdfn2$Value = 0
temp1 = 1000
cont = 0
alpha = 2
for (k1 in seq(0.02,0.8,0.02))
{
  for (samp in seq(20,2000,20))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dexp(2:temp1,k1))/exp(-alpha*k1))      
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam)
      pares = power.powerexp.lrt(pow,powexp)
      if (vures$p.one.sided > 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        expdfn2$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        expdfn2$Value[cont+4000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        expdfn2$Value[cont+8000] = table(cth)[z]/100
    }
    expdfn2$K[cont] = k1
    expdfn2$K[cont+4000] = k1
    expdfn2$K[cont+8000] = k1
    expdfn2$Sam[cont] = samp
    expdfn2$Sam[cont+4000] = samp
    expdfn2$Sam[cont+8000] = samp
    print(samp)
  }
  print(k1)
}

parexdf1k$Value = 0
cont = 0
alpha = 2
k2 = 0.9
temp1 = 10000
for (wf in seq(0.1,7,0.1))
  
{
  k1 = exp(-wf)
  for (samp in seq(50,2000,50))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dgamma(2:temp1,shape = k2,rate = k1))/(1-pgamma(alpha,k2, lower = F)))
      
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
      
      potest = lm(log(cont)~log(ubin))
      extest = lm(log(cont)~ubin)
      y = log(cont)
      x = log(ubin)
      powexptest = nls(y~a2+b2*x+c2*exp(x),start=list(a2=0,b2=0,c2=0))
      
      if((AIC(potest,extest,powexptest)$AIC[1]-AIC(potest,extest,powexptest)$AIC[2]) < -1 && (AIC(potest,extest,powexptest)$AIC[1]-AIC(potest,extest,powexptest)$AIC[3]) < -1)
        cth[l] = 1
      if((AIC(potest,extest,powexptest)$AIC[2]-AIC(potest,extest,powexptest)$AIC[1]) < -1 && (AIC(potest,extest,powexptest)$AIC[2]-AIC(potest,extest,powexptest)$AIC[3]) < -1)
        cth[l] = 2
      if((AIC(potest,extest,powexptest)$AIC[3]-AIC(potest,extest,powexptest)$AIC[1]) < -1 && (AIC(potest,extest,powexptest)$AIC[3]-AIC(potest,extest,powexptest)$AIC[2]) < -1)
        cth[l] = 3
      
    }
      for (z in 1:length(table(cth)))
      {
        if (names(table(cth))[z] == "1")
          parexdf1k$Value[cont] = table(cth)[z]/100
        if (names(table(cth))[z] == "2")
          parexdf1k$Value[cont+2800] = table(cth)[z]/100
        if (names(table(cth))[z] == "3")
          parexdf1k$Value[cont+5600] = table(cth)[z]/100
      }
    parexdf1k$K[cont] = wf
    parexdf1k$K[cont+2800] = wf
    parexdf1k$K[cont+5600] = wf
    parexdf1k$Sam[cont] = samp
    parexdf1k$Sam[cont+2800] = samp
    parexdf1k$Sam[cont+5600] = samp
      print(samp)
  }
  print(wf)
}

parexdf4l$Value = 0
cont = 0
alpha = 2
k2 = 0.95
temp1 = 10000
for (wf in seq(0.1,7,0.1))
  
{
  k1 = exp(-wf)
  for (samp in seq(50,2000,50))
  {
    
    cont = cont + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:temp1, samp,replace = T, prob = (dgamma(2:temp1,shape = k2,rate = k1))/(1-pgamma(alpha,k2, lower = F)))
      
      exp = exp.fit(parsam,2)
      pow = pareto.fit(parsam,2)
      vures = vuong(pareto.exp.llr(parsam, pow, exp))
      powexp = powerexp.fit(parsam,2)
      pares = power.powerexp.lrt(pow,powexp)
      exres = exp.powerexp.lrt(exp,powexp)
      if (vures$p.one.sided > 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 1
      if (vures$p.one.sided <= 0.05 & vures$p.two.sided <= 0.05)
        cth[l] = 2
      if (cth[l] == 1)
      {
        if (pares$p_value <= 0.05)
          cth[l] = 3
      }
      if (cth[l] == 2)
      {
        if (exres$p_value <= 0.05)
          cth[l] = 3
      }
    }
    for (z in 1:length(table(cth)))
    {
      if (names(table(cth))[z] == "1")
        parexdf4l$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        parexdf4l$Value[cont+2800] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        parexdf4l$Value[cont+5600] = table(cth)[z]/100
    }
    parexdf4l$K[cont] = wf
    parexdf4l$K[cont+2800] = wf
    parexdf4l$K[cont+5600] = wf
    parexdf4l$Sam[cont] = samp
    parexdf4l$Sam[cont+2800] = samp
    parexdf4l$Sam[cont+5600] = samp
    print(samp)
  }
  print(wf)
}

pgamma(2,0.5,lower = F)

alpha = 2
k2 = 0.1
temp1 = 10000
k1 = 0.0001
samp = 1000
parsam = sample(2:temp1, samp,replace = T, prob = (dgamma(2:temp1,shape = k2,rate = k1))/(1-pgamma(alpha,k2, lower = F)))

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
plot(log(cont)~log(ubin))

exp.powerexp.lrt <- function(exp.d,powerexp.d) {
  lr <- (exp.d$loglike - powerexp.d$loglike)
  p <- pchisq(-2*lr,df=1,lower.tail=FALSE)
  Result <- list(log.like.ratio = lr, p_value = p)
  Result
}

