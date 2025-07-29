cthx = numeric(100)
cthy = numeric(100)
cthz = numeric(100)

cth1cut = numeric(100)
for (temp in seq(100,2080,20))
{
for (samp in seq(20,1500,20))
{
  cth = numeric(100)
  for (l in 1:100)
  {
    alpha = 2; xcut = 1000; k3 = 1.5; k4 = ((k3-1)/xcut)*log(xcut/alpha)
    parsam = sample(2:100000,samp,replace = T, prob = c(dpareto(2:xcut,alpha,k3),dexp((xcut+1):100000,k4)))
    
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
    powexptest = nls(y~a2+b2*x+c2*exp(x),start=list(a2=0,b2=1,c2=1))
    
    if((AIC(potest,extest,powexptest)$AIC[1]-AIC(potest,extest,powexptest)$AIC[2]) < -1 && (AIC(potest,extest,powexptest)$AIC[1]-AIC(potest,extest,powexptest)$AIC[3]) < -1)
      cth[l] = 1
    if((AIC(potest,extest,powexptest)$AIC[2]-AIC(potest,extest,powexptest)$AIC[1]) < -1 && (AIC(potest,extest,powexptest)$AIC[2]-AIC(potest,extest,powexptest)$AIC[3]) < -1)
      cth[l] = 2
    if((AIC(potest,extest,powexptest)$AIC[3]-AIC(potest,extest,powexptest)$AIC[1]) < -1 && (AIC(potest,extest,powexptest)$AIC[3]-AIC(potest,extest,powexptest)$AIC[2]) < -1)
      cth[l] = 3
    
  }
  for (z in 1:length(table(cth)))
  {
    if (names(table(cth))[z] == "3")
    {if (table(cth)[z] >= 95)
      break}
  }
  if (table(cth)[z] >= 95)
    break    
}
  ##for (z in 1:length(table(cth)))
  ##{
    ##if (names(table(cth))[z] == "1")
      ##cthx[samp/20] = table(cth)[z]
    ##if (names(table(cth))[z] == "2")
      ##cthy[samp/20] = table(cth)[z]
    ##if (names(table(cth))[z] == "3")
      ##cthz[samp/20] = table(cth)[z]
  ##}
cth1cut[(temp-80)/20] = samp
print(samp)
}

cthu = numeric(100)
cthv = numeric(100)
cthw = numeric(100)
cth2cut = numeric(100)
for (temp in seq(100,2080,20))
{
for (samp in seq(20,1500,20))
{
    cth = numeric(100)
for (l in 1:100)
{
  alpha = 2; xcut = temp; k3 = 1.5; k4 = ((k3-1)/xcut)*log(xcut/alpha)
  parsam = sample(2:100000,samp,replace = T, prob = c(dpareto(2:xcut,alpha,k3),dexp((xcut+1):100000,k4)))
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
if (names(table(cth))[z] == "3")
{if (table(cth)[z] >= 95)
break}
}
if (table(cth)[z] >= 95)
break    
}
##for (z in 1:length(table(cth)))
##{
  ##if (names(table(cth))[z] == "1")
    ##cthu[samp/20] = table(cth)[z]
  ##if (names(table(cth))[z] == "2")
    ##cthv[samp/20] = table(cth)[z]
  ##if (names(table(cth))[z] == "3")
    ##cthw[samp/20] = table(cth)[z]
##}
cth2cut[(temp-80)/20] = samp
print(samp)
}


compar4 = data.frame(cbind(1:100),0)
compar4 = compar4[,-c(1,2)]
compar4$Cut = seq(20,2000,20)
compar4$fp = cthy

pextest = ggplot(compar3, aes(Cut, fp))+
  geom_point(size = 1)+
  geom_smooth(se = 0, size = 1, col = "black")+
  xlab("Cut-off value") +
  ylab("Probability") +
  theme_bw()
pextest+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))+
  ##opts(legend.justification=c(1,1), legend.position=c(0.96,0.6)) +
  ##scale_colour_manual(values = c("black","dark grey"),
                      ##name=""
                      ##breaks = c("Exponential","False positive (Pareto)"),
                      ##labels = c("Exponential","False positive (Pareto)")
  ##)+
    opts(
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank()
      ##panel.border = theme_blank(),
      ##panel.background = theme_blank()
    )