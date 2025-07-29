nestsur = read.csv("C:/Ashwin/Nest survival.csv")
head(nestsur)
length(nestsur$Mon)
for (i in 1:620)
{
  nestsur$arr[i] = spesur[names(spesur) == nestsur$Species[i]]
}
names(nestsur)
seedspe = seedspe[seedspe$Species == c("AM","AS","CA","DB","KH","LI"),]
seedspe$Species = as.character(seedspe$Species)
seedspe[seedspe$Day >= (nestsur$Day[i] - 5) & seedspe$Day <= (nestsur$Day[i] + 5),]
for (i in 1:620)
{
  temp = seedspe[seedspe$Day >= (nestsur$Day[i] - 4) & seedspe$Day <= (nestsur$Day[i] + 1) & seedspe$Species == nestsur$Species[i],]$Density
  nestsur$arrday[i] = mean(temp)
}
nestsur[nestsur$Status == 0,]$arrday = nestsur[nestsur$Status == 0,]$arr
nestsur[230:240,]
cox1 <- coxph(Surv(Mon,Status) ~ Speciesd, data=nestsur)
summary(cox1)
plot(nestsur$Mon, predict(cox1,type="lp"))

fit0 <- coxph(Surv(Mon) ~ 1, data=nestsur) # null model
plot(nestsur$Ini, resid(fit0))
lines(lowess(nestsur$Ini, resid(fit0), iter=0), lty=2)

proh <- cox.zph(cox1)
proh
par(mfrow=c(2,2))
plot(proh[1:4])
plot(proh[5:7])

dfbeta1 <- resid(cox1, type="dfbeta")
dfbeta1[1:10,]
coef.names <- names(coef(cox1))
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(1:length(dfbeta1[,1]),dfbeta1[,i],main=coef.names[i])
  abline(h = coef(cox1)[i])
}
for (i in 5:7) {
  plot(1:length(dfbeta1[,1]),dfbeta1[,i],main=coef.names[i])
  abline(h = coef(cox1)[i])
}

coxme1 <- coxme(Surv(Mon,Status) ~ arrday + (1|Nest),data = nestsur)
print(coxme1)
coef(coxme1)
coxme1$z

coxme2 <- coxme(Surv(mon) ~ dens + ll + sub + (1|tree),
                data=pyg)
print(coxme2)

est.effect <- coxme1$linear.predictor
par(mfrow=c(1,1))
plot(nestsur$[pyg$sub=="G"], est.effect[pyg$sub=="G"],
     ylim=c(-1,3), xlim=c(0,20), xlab="Density", ylab="Relative logHazard", pch=19)
# plot the fitted hazard ratios - shows the interaction
coef.t1 <- coef(coxme1)
coef.t1
curve( 0.02474139*x, 0, 20, lwd=2, add=T) # cp
points(pyg$dens[pyg$sub=="LOG"], est.effect[pyg$sub=="LOG"],
       col="purple", pch=19)
curve( 0.02474139*x -0.42697888 +0.09607773*x, 0, 100, lwd=2, col="purple",
       add=T)



tt.low <- survfit(Surv(Mon,Status) ~ Speciesd, data=nestsur)
plot(tt.low,col=1:8, lwd=2, xlab="time", ylab="survival")
legend("bottomleft", legend=levels(nestsur$Speciesd), lty=1, lwd=2, cex = 0.6, col=1:8)
tt.high <- survfit(Surv() ~ sub, data=pyg[pyg$dens >=
  12 & pyg$dens <= 15,])
plot(tt.high,col=1:6, lwd=2, xlab="time", ylab="survival")
legend("bottomleft", legend=levels(pyg$sub), lty=1, lwd=2, col=1:6)

length(pyg[pyg$dens>=12,]$dens)/length(pyg$dens)

tt.low <- survfit(Surv(mon) ~ sub, data=pyg[pyg$dens<12,])
plot(tt.low,col=1:6, lwd=2, xlab="time", ylab="survival")
legend("bottomleft", legend=levels(pyg$sub), lty=1, lwd=2, col=1:6)
tt.high <- survfit(Surv(mon) ~ sub, data=pyg[pyg$dens >=
  12,])
plot(tt.high,col=1:6, lwd=2, xlab="time", ylab="survival")
legend("bottomleft", legend=levels(pyg$sub), lty=1, lwd=2, col=1:6)