cthlkkkk = numeric(100)
cthkkkkk = numeric(100)
cthjkkkk = numeric(100)
for (samp in seq(1780,2000,20))
{
cth = numeric(100)
for (l in 1:100)
{
  alpha = 2; k1 = 0.4
  parsam = sample(2:10000,samp,replace = T, prob = (dexp(2:10000,k1))/exp(-alpha*k1))
  
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
  ##if(summary(potest)$coefficients[8] > 0.05)
    ##cth[l] = 0
  ##if(summary(extest)$coefficients[8] > 0.05)
    ##cth[l] = 0
}
  
for (z in 1:length(table(cth)))
{
if (names(table(cth))[z] == "1")
  cthlkkkk[samp/20] = table(cth)[z]
if (names(table(cth))[z] == "2")
  cthkkkkk[samp/20] = table(cth)[z]
if (names(table(cth))[z] == "3")
  cthjkkkk[samp/20] = table(cth)[z]
}
print(samp)
}

cthp2kkk = numeric(100)
cthg2kkk = numeric(100)
ctht2kkk = numeric(100)
##cth2k = numeric(100)
##tensam = 6600
##for (temp in seq(0.27,0.4,0.01))
##{
for (samp in seq(20,515,5))
{
cth = numeric(100)

for (l in 1:100)
{
  alpha = 2; k1 = 0.2
  parsam = sample(2:100,samp,replace = T, prob = (dexp(2:100,k1))/exp(-alpha*k1))
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
##for (z in 1:length(table(cth)))
##{
  ##if (names(table(cth))[z] == "2")
  ##{if (table(cth)[z] >= 95)
    ##break}
##}
##if (table(cth)[z] >= 95)
  ##break    
##}
for (z in 1:length(table(cth)))
{
  if (names(table(cth))[z] == "1")
    cthp2kkk[(samp-15)/5] = table(cth)[z]
  if (names(table(cth))[z] == "2")
    cthg2kkk[(samp-15)/5] = table(cth)[z]
  if (names(table(cth))[z] == "3")
    ctht2kkk[(samp-15)/5] = table(cth)[z]
}
##cth2k[temp*100] = samp
##tensam = samp
##print(temp)
print(samp)
}
##plot(cth2k[1:28]~seq(0.01,0.28,0.01))

table(cth)
length(cthl)
compar = data.frame(cbind(1:5032),0)
compar = compar[,-c(1,2)]
compar$Sample = rep(50:2565,2)
compar$Type = c(rep("False positive (pareto)",2516),rep("Exponential",2516))
compar$fp[1:2516] = cthk[1:2516]/100
compar$fp[2517:5032] = cthl[1:2516]/100

compar2 = data.frame(cbind(1:300),0)
compar2 = compar2[,-c(1,2)]
compar2$Sample = rep(50:199,2)
compar2$Type = c(rep("False positive (pareto)",150),rep("Exponential",150))
compar2$fp[1:150] = cthg/100
compar2$fp[151:300] = cthp/100

comparison3 = data.frame(cbind(1:300),0)
comparison3 = comparison3[,-c(1,2)]
comparison3$Sample = rep(seq(20,2000,20),3)
comparison3$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison3$fp = c(cthl/100,cthk/100,cthj/100)

comparison4 = data.frame(cbind(1:300),0)
comparison4 = comparison4[,-c(1,2)]
comparison4$Sample = rep(seq(20,218,2),3)
comparison4$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison4$fp = c(cthp/100,cthg/100,ctht/100)

comparison7 = data.frame(cbind(1:300),0)
comparison7 = comparison7[,-c(1,2)]
comparison7$Sample = rep(seq(20,2000,20),3)
comparison7$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison7$fp = c(cthl1/100,cthk1/100,cthj1/100)

comparison8 = data.frame(cbind(1:300),0)
comparison8 = comparison8[,-c(1,2)]
comparison8$Sample = rep(seq(20,218,2),3)
comparison8$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison8$fp = c(cthp1/100,cthg1/100,ctht1/100)

comparison9 = data.frame(cbind(1:300),0)
comparison9 = comparison9[,-c(1,2)]
comparison9$Sample = rep(seq(20,2000,20),3)
comparison9$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison9$fp = c(cthl2/100,cthk2/100,cthj2/100)

comparison10 = data.frame(cbind(1:300),0)
comparison10 = comparison10[,-c(1,2)]
comparison10$Sample = rep(seq(20,515,5),3)
comparison10$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison10$fp = c(cthp2/100,cthg2/100,ctht2/100)

comparison11 = data.frame(cbind(1:300),0)
comparison11 = comparison11[,-c(1,2)]
comparison11$Sample = rep(seq(20,2000,20),3)
comparison11$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison11$fp = c(cthl3/100,cthk3/100,cthj3/100)

comparison12 = data.frame(cbind(1:300),0)
comparison12 = comparison12[,-c(1,2)]
comparison12$Sample = rep(seq(20,515,5),3)
comparison12$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison12$fp = c(cthp3/100,cthg3/100,ctht3/100)

comparison13 = data.frame(cbind(1:28),0)
comparison13 = comparison13[,-c(1,2)]
comparison13$k = seq(0.01,0.28,0.01)
comparison13$Sample = cth2k[1:28]

comparison26 = data.frame(cbind(1:300),0)
comparison26 = comparison26[,-c(1,2)]
comparison26$Sample = rep(seq(20,2000,20),3)
comparison26$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison26$fp = c(cthlk/100,cthkk/100,cthjk/100)

comparison27 = data.frame(cbind(1:300),0)
comparison27 = comparison27[,-c(1,2)]
comparison27$Sample = rep(seq(20,218,2),3)
comparison27$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison27$fp = c(cthpk/100,cthgk/100,cthtk/100)

comparison28 = data.frame(cbind(1:300),0)
comparison28 = comparison28[,-c(1,2)]
comparison28$Sample = rep(seq(20,2000,20),3)
comparison28$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison28$fp = c(cthl1k/100,cthk1k/100,cthj1k/100)

comparison29 = data.frame(cbind(1:300),0)
comparison29 = comparison29[,-c(1,2)]
comparison29$Sample = rep(seq(20,218,2),3)
comparison29$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison29$fp = c(cthp1k/100,cthg1k/100,ctht1k/100)

comparison30 = data.frame(cbind(1:300),0)
comparison30 = comparison30[,-c(1,2)]
comparison30$Sample = rep(seq(20,2000,20),3)
comparison30$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison30$fp = c(cthl2k/100,cthk2k/100,cthj2k/100)

comparison31 = data.frame(cbind(1:300),0)
comparison31 = comparison31[,-c(1,2)]
comparison31$Sample = rep(seq(20,218,2),3)
comparison31$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison31$fp = c(cthp2k/100,cthg2k/100,ctht2k/100)

comparison32 = data.frame(cbind(1:300),0)
comparison32 = comparison32[,-c(1,2)]
comparison32$Sample = rep(seq(20,2000,20),3)
comparison32$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison32$fp = c(cthlkk/100,cthkkk/100,cthjkk/100)

comparison33 = data.frame(cbind(1:300),0)
comparison33 = comparison33[,-c(1,2)]
comparison33$Sample = rep(seq(20,218,2),3)
comparison33$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison33$fp = c(cthpkk/100,cthgkk/100,cthtkk/100)

comparison34 = data.frame(cbind(1:300),0)
comparison34 = comparison34[,-c(1,2)]
comparison34$Sample = rep(seq(20,2000,20),3)
comparison34$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison34$fp = c(cthl1kk/100,cthk1kk/100,cthj1kk/100)

comparison34 = data.frame(cbind(1:300),0)
comparison34 = comparison34[,-c(1,2)]
comparison34$Sample = rep(seq(20,218,2),3)
comparison34$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison34$fp = c(cthp1kk/100,cthg1kk/100,ctht1kk/100)

comparison35 = data.frame(cbind(1:300),0)
comparison35 = comparison35[,-c(1,2)]
comparison35$Sample = rep(seq(20,2000,20),3)
comparison35$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison35$fp = c(cthl2kk/100,cthk2kk/100,cthj2kk/100)

comparison36 = data.frame(cbind(1:300),0)
comparison36 = comparison36[,-c(1,2)]
comparison36$Sample = rep(seq(20,218,2),3)
comparison36$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison36$fp = c(cthp2kk/100,cthg2kk/100,ctht2kk/100)

comparison37 = data.frame(cbind(1:300),0)
comparison37 = comparison37[,-c(1,2)]
comparison37$Sample = rep(seq(20,2000,20),3)
comparison37$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison37$fp = c(cthlkkk/100,cthkkkk/100,cthjkkk/100)

comparison38 = data.frame(cbind(1:300),0)
comparison38 = comparison38[,-c(1,2)]
comparison38$Sample = rep(seq(20,515,5),3)
comparison38$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison38$fp = c(cthpkkk/100,cthgkkk/100,cthtkkk/100)

comparison39 = data.frame(cbind(1:300),0)
comparison39 = comparison39[,-c(1,2)]
comparison39$Sample = rep(seq(20,2000,20),3)
comparison39$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison39$fp = c(cthl1kkk/100,cthk1kkk/100,cthj1kkk/100)

comparison40 = data.frame(cbind(1:300),0)
comparison40 = comparison40[,-c(1,2)]
comparison40$Sample = rep(seq(20,515,5),3)
comparison40$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison40$fp = c(cthp1kkk/100,cthg1kkk/100,ctht1kkk/100)

comparison41 = data.frame(cbind(1:300),0)
comparison41 = comparison41[,-c(1,2)]
comparison41$Sample = rep(seq(20,2000,20),3)
comparison41$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison41$fp = c(cthl2kkk/100,cthk2kkk/100,cthj2kkk/100)

comparison42 = data.frame(cbind(1:300),0)
comparison42 = comparison42[,-c(1,2)]
comparison42$Sample = rep(seq(20,515,5),3)
comparison42$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison42$fp = c(cthp2kkk/100,cthg2kkk/100,ctht2kkk/100)

comparison43 = data.frame(cbind(1:300),0)
comparison43 = comparison43[,-c(1,2)]
comparison43$Sample = rep(seq(20,2000,20),3)
comparison43$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison43$fp = c(cthlkkkk/100,cthkkkkk/100,cthjkkkk/100)

comparison44 = data.frame(cbind(1:300),0)
comparison44 = comparison44[,-c(1,2)]
comparison44$Sample = rep(seq(20,2000,20),3)
comparison44$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison44$fp = c(cthl1kkkk/100,cthk1kkkk/100,cthj1kkkk/100)

comparison45 = data.frame(cbind(1:300),0)
comparison45 = comparison45[,-c(1,2)]
comparison45$Sample = rep(seq(20,2000,20),3)
comparison45$Type = c(rep("False Positive - Pareto",100),rep("Exponential",100), rep("False Positive - Pareto Exp cut-off",100))
comparison45$fp = c(cthl2kkkk/100,cthk2kkkk/100,cthj2kkkk/100)

ptest = ggplot(comparison45, aes(Sample, fp, linetype = Type))+
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

