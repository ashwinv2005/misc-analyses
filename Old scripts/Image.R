parparm1 = matrix(nrow = 14,ncol = 50,0) 
parexpm1 = matrix(nrow = 14,ncol = 50,0)
parparexm1 = matrix(nrow = 14,ncol = 50,0)

parparm2 = matrix(nrow = 100,ncol = 100,0) 
parexpm2 = matrix(nrow = 100,ncol = 100,0)
parparexm2 = matrix(nrow = 100,ncol = 100,0)

pardf2 = data.frame(cbind(1:30000,0))
pardf2 = pardf2[,-c(1,2)]
pardf2$Type = c(rep("Pareto",10000),rep("Exponential",10000),rep("Pareto with exponential cut-off",10000))

pardf1 = data.frame(cbind(1:30000,0))
pardf1 = pardf1[,-c(1,2)]
pardf1$Type = c(rep("Pareto",10000),rep("Exponential",10000),rep("Pareto with exponential cut-off",10000))

expdf1 = data.frame(cbind(1:12000,0))
expdf1 = expdf1[,-c(1,2)]
expdf1$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

expdf2 = data.frame(cbind(1:12000,0))
expdf2 = expdf2[,-c(1,2)]
expdf2$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

expdf3 = data.frame(cbind(1:12000,0))
expdf3 = expdf3[,-c(1,2)]
expdf3$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

expdf4 = data.frame(cbind(1:12000,0))
expdf4 = expdf4[,-c(1,2)]
expdf4$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

expdfn2 = data.frame(cbind(1:12000,0))
expdfn2 = expdfn2[,-c(1,2)]
expdfn2$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

parexdf2l = data.frame(cbind(1:8400,0))
parexdf2l = parexdf2l[,-c(1,2)]
parexdf2l$Type = c(rep("Pareto",2800),rep("Exponential",2800),rep("Pareto with exponential cut-off",2800))

parexdf3l = data.frame(cbind(1:8400,0))
parexdf3l = parexdf3l[,-c(1,2)]
parexdf3l$Type = c(rep("Pareto",2800),rep("Exponential",2800),rep("Pareto with exponential cut-off",2800))

parexdf4l = data.frame(cbind(1:8400,0))
parexdf4l = parexdf4l[,-c(1,2)]
parexdf4l$Type = c(rep("Pareto",2800),rep("Exponential",2800),rep("Pareto with exponential cut-off",2800))

parexdf1k = data.frame(cbind(1:8400,0))
parexdf1k = parexdf1k[,-c(1,2)]
parexdf1k$Type = c(rep("Pareto",2800),rep("Exponential",2800),rep("Pareto with exponential cut-off",2800))

expdf = data.frame(cbind(1:9000,0))
expdf = expdf[,-c(1,2)]
expdf$Type = c(rep("Pareto",3000),rep("Exponential",3000),rep("Pareto with exponential cut-off",3000))

parexdf = data.frame(cbind(1:12000,0))
parexdf = parexdf[,-c(1,2)]
parexdf$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

parexdf1 = data.frame(cbind(1:12000,0))
parexdf1 = parexdf1[,-c(1,2)]
parexdf1$Type = c(rep("Pareto",4000),rep("Exponential",4000),rep("Pareto with exponential cut-off",4000))

temp1 = 1000
cont1 = 0
cont = 0
for (k2 in seq(1.1,3.08,0.02))
{
cont1 = cont1 + 1
cont2 = 0
for (samp in seq(20,1010,10))
{
cont2 = cont2 + 1
cont = cont + 1
for (l in 1:100)
{
alpha = 2
parsam = sample(2:temp1, samp, replace = T, prob = dpareto(2:temp1,alpha,k2))

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
  if (names(table(cth))[z] == "1")
    parparm2[cont1,cont2] = table(cth)[z]/100
  if (names(table(cth))[z] == "2")
    parexpm2[cont1,cont2] = table(cth)[z]/100
  if (names(table(cth))[z] == "3")
    parparexm2[cont1,cont2] = table(cth)[z]/100
}
print(samp)
}
print(k2)
}
x = seq(1.1,2.4,0.1)
y = seq(26,124,2)
image.plot(x,y,parparm1, ylab = "Sample size", xlab = "k")

pardf = data.frame(cbind(1:2100,0))
pardf = pardf[,-c(1,2)]
pardf$Type = c(rep("Pareto",700),rep("Exponential",700),rep("Pareto with exponential cut-off",700))
pardf$K = rep(x,150)
pardf$Sam = c(rep(y, each = 14),rep(y, each = 14),rep(y,each = 14))
cont = 1400
cont1 = 1401
for (i in 1:50)
{
  cont = cont + 14
pardf$Value[cont1:cont] = parparexm1[,i]
  cont1 = cont1+14
}

names(expdf)
expdf[parexdf1l$K == 0.26 & expdf$Sam == 260,]
par = ggplot(parexdf4l, aes(Sam, K)) + 
  geom_tile(aes(fill=Value)) +
  scale_fill_gradient(limits=c(0,1),low="yellow", high="red",na.value="white", name = "")+
  facet_grid(Type ~ ., scale="free_y")+
  xlab("Sample size")+
  ylab("k")+
  theme_bw()
parf = par+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = theme_text(size = 12), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = theme_text(size = 12)) +
  opts(legend.title = theme_text(size = 16, face = "bold"), legend.text = theme_text(size = 16))+
  ##scale_x_continuous(limits = c(20,40))+
  ##scale_y_continuous(breaks = c(0,10,20,30,40,50,60))+
  ##scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  ##opts(legend.position = "none")+
  opts(panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )
parf

pardf1$Value = 0
temp1 = 100
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
        pardf2$Value[cont] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        pardf2$Value[cont+10000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        pardf2$Value[cont+20000] = table(cth)[z]/100
    }
    pardf1$K[cont] = k2
    pardf1$K[cont+10000] = k2
    pardf1$K[cont+20000] = k2
    pardf1$Sam[cont] = samp
    pardf1$Sam[cont+10000] = samp
    pardf1$Sam[cont+20000] = samp
    print(samp)
  }
  print(k2)
}

expdf$Value = 0
cont1 = 1110
alpha = 2
k1 = 0.26
samp = 220
for (k1 in seq(0.34,0.62,0.02))
{
  for (samp in seq(1760,2000,20))
  {
    
    cont1 = cont1 + 1
    for (l in 1:100)
    {
      
      parsam = sample(2:1000, samp,replace = T, prob = (dexp(2:1000,k1))/exp(-alpha*k1))
      
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
      if (names(table(cth))[z] == "1")
        expdf$Value[cont1] = table(cth)[z]/100
      if (names(table(cth))[z] == "2")
        expdf$Value[cont1+3000] = table(cth)[z]/100
      if (names(table(cth))[z] == "3")
        expdf$Value[cont1+6000] = table(cth)[z]/100
    }
    expdf$K[cont1] = k1
    expdf$K[cont1+3000] = k1
    expdf$K[cont1+6000] = k1
    expdf$Sam[cont1] = samp
    expdf$Sam[cont1+3000] = samp
    expdf$Sam[cont1+6000] = samp
    print(samp)
    print(cont1)
  }
  print(k1)
}