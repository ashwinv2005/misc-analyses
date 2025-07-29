limctha3 = numeric(50)
limcthb3 = numeric(50)
limcthc3 = numeric(50)
for (samp in seq(16,114,2))
{
cth = numeric(100)
for (l in 1:100)
{
alpha = 2; k2 = 1.5
parsam = sample(2:100, samp, replace = T, prob = dpareto(2:100,alpha,k2))

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
    limctha3[(samp-14)/2] = table(cth)[z]
  if (names(table(cth))[z] == "2")
    limcthb3[(samp-14)/2] = table(cth)[z]
  if (names(table(cth))[z] == "3")
    limcthc3[(samp-14)/2] = table(cth)[z]
}
print(samp)
}

table(cth)

limcthd2kk = numeric(50)
limcthe2kk = numeric(50)
limcthf2kk = numeric(50)
for (samp in seq(20,1000,20))
{
  
cth = numeric(100)
for (l in 1:100)
{
  alpha = 2; k2 = 1.1
  parsam = sample(2:100000,samp,replace = T, prob = dpareto(2:100000,alpha,k2))
  exp = exp.fit(parsam,2)
  pow = pareto.fit(parsam,2)
  vures = vuong(pareto.exp.llr(parsam, pow, exp))
  powexp = powerexp.fit(parsam,2)
  pares = power.powerexp.lrt(pow,powexp)
  if (vures$p.one.sided >= 0.95 & vures$p.two.sided <= 0.05)
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
    limcthd2kk[(samp-40)/20] = table(cth)[z]
  if (names(table(cth))[z] == "2")
    limcthe2kk[(samp-40)] = table(cth)[z]
  if (names(table(cth))[z] == "3")
    limcthf2kk[(samp-40)] = table(cth)[z]
}
print(samp)
}
table(cth)

comparison1 = data.frame(cbind(1:150),0)
comparison1 = comparison1[,-c(1,2)]
comparison1$Sample = rep(seq(16,114,2),3)
comparison1$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison1$fp = c(ctha/100,cthb/100,cthc/100)

comparison2 = data.frame(cbind(1:150),0)
comparison2 = comparison2[,-c(1,2)]
comparison2$Sample = rep(seq(20,1000,20),3)
comparison2$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison2$fp = c(cthd/100,cthe/100,cthf/100)

comparison5 = data.frame(cbind(1:150),0)
comparison5 = comparison5[,-c(1,2)]
comparison5$Sample = rep(seq(16,114,2),3)
comparison5$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison5$fp = c(ctha1/100,cthb1/100,cthc1/100)

comparison6 = data.frame(cbind(1:150),0)
comparison6 = comparison6[,-c(1,2)]
comparison6$Sample = rep(seq(16,114,2),3)
comparison6$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison6$fp = c(ctha2/100,cthb2/100,cthc2/100)

comparison14 = data.frame(cbind(1:150),0)
comparison14 = comparison14[,-c(1,2)]
comparison14$Sample = rep(seq(16,114,2),3)
comparison14$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison14$fp = c(limctha1/100,limcthb1/100,limcthc1/100)

comparison15 = data.frame(cbind(1:150),0)
comparison15 = comparison15[,-c(1,2)]
comparison15$Sample = rep(seq(20,1000,20),3)
comparison15$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison15$fp = c(limcthd1/100,limcthe1/100,limcthf1/100)

comparison16 = data.frame(cbind(1:150),0)
comparison16 = comparison16[,-c(1,2)]
comparison16$Sample = rep(seq(16,114,2),3)
comparison16$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison16$fp = c(limctha2/100,limcthb2/100,limcthc2/100)

comparison17 = data.frame(cbind(1:150),0)
comparison17 = comparison17[,-c(1,2)]
comparison17$Sample = rep(seq(20,1000,20),3)
comparison17$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison17$fp = c(limcthd2/100,limcthe2/100,limcthf2/100)

comparison18 = data.frame(cbind(1:150),0)
comparison18 = comparison18[,-c(1,2)]
comparison18$Sample = rep(seq(16,114,2),3)
comparison18$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison18$fp = c(limctha3/100,limcthb3/100,limcthc3/100)

comparison19 = data.frame(cbind(1:150),0)
comparison19 = comparison19[,-c(1,2)]
comparison19$Sample = rep(seq(20,1000,20),3)
comparison19$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison19$fp = c(limcthd3/100,limcthe3/100,limcthf3/100)

comparison20 = data.frame(cbind(1:150),0)
comparison20 = comparison20[,-c(1,2)]
comparison20$Sample = rep(seq(20,1000,20),3)
comparison20$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison20$fp = c(limcthd1k/100,limcthe1k/100,limcthf1k/100)

comparison21 = data.frame(cbind(1:150),0)
comparison21 = comparison21[,-c(1,2)]
comparison21$Sample = rep(seq(20,1000,20),3)
comparison21$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison21$fp = c(limcthd2k/100,limcthe2k/100,limcthf2k/100)

comparison22 = data.frame(cbind(1:150),0)
comparison22 = comparison22[,-c(1,2)]
comparison22$Sample = rep(seq(20,1000,20),3)
comparison22$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison22$fp = c(limcthd3k/100,limcthe3k/100,limcthf3k/100)

comparison23 = data.frame(cbind(1:150),0)
comparison23 = comparison23[,-c(1,2)]
comparison23$Sample = rep(seq(20,1000,20),3)
comparison23$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison23$fp = c(limcthd1kk/100,limcthe1kk/100,limcthf1kk/100)

comparison24 = data.frame(cbind(1:150),0)
comparison24 = comparison24[,-c(1,2)]
comparison24$Sample = rep(seq(20,1000,20),3)
comparison24$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison24$fp = c(limcthd2kk/100,limcthe2kk/100,limcthf2kk/100)

comparison25 = data.frame(cbind(1:150),0)
comparison25 = comparison25[,-c(1,2)]
comparison25$Sample = rep(seq(20,1000,20),3)
comparison25$Type = c(rep("Pareto",50),rep("False Positive - Exponential",50), rep("False Positive - Pareto Exp cut-off",50))
comparison25$fp = c(limcthd3kk/100,limcthe3kk/100,limcthf3kk/100)


ptest = ggplot(comparison25, aes(Sample, fp, linetype = Type))+
  geom_point(size = 2)+
  geom_smooth(se = 0, size = 1, col = "black")+
  xlab("Sample size") +
  ylab("Probability") +
  theme_bw()
ptest+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))+
  opts(legend.justification=c(1,1), legend.position=c(0.96,0.7)) +
  scale_linetype_manual(values = c(1,2,3),
                      name=""
                      ##breaks = c("Exponential","False positive (Pareto)"),
                      ##labels = c("Exponential","False positive (Pareto)")
  )+
    opts(
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank()
      ##panel.border = theme_blank(),
      ##panel.background = theme_blank()
    )




