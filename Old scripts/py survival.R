pyg = read.csv("C:/Ashwin/pys.csv")
head(pyg)
names(pyg) = c("scat","spe","tree","dist","dens","sub","mon","tot","sub1","pred","sub2","can","ll","other")
str(pyg)
unique(pyg$tree)
pyg = pyg[,-c(2)]
pyg[pyg$mon>=4,]
pyg$dens
cox1 <- coxph(Surv(mon) ~ dens*dist*sub, data=pyg)
summary(cox1)
plot(pyg$mon, predict(cox1,type="lp"))

fit0 <- coxph(Surv(mon) ~ 1, data=pyg) # null model
plot(pyg$dens, resid(fit0))
lines(lowess(pyg$dens, resid(fit0), iter=0), lty=2)

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

coxme1 <- coxme(Surv(mon) ~ dens*sub + (1|tree),
                 data=pyg)
print(coxme1)

coxme2 <- coxme(Surv(mon) ~ dens + ll + sub + (1|tree),
                data=pyg)
print(coxme2)

est.effect <- coxme1$linear.predictor
par(mfrow=c(1,1))
plot(pyg$dens[pyg$sub=="G"], est.effect[pyg$sub=="G"],
     ylim=c(-1,3), xlim=c(0,20), xlab="Density", ylab="Relative logHazard", pch=19)
# plot the fitted hazard ratios - shows the interaction
coef.t1 <- coef(coxme1)
coef.t1
curve( 0.02474139*x, 0, 20, lwd=2, add=T) # cp
points(pyg$dens[pyg$sub=="LOG"], est.effect[pyg$sub=="LOG"],
       col="purple", pch=19)
curve( 0.02474139*x -0.42697888 +0.09607773*x, 0, 100, lwd=2, col="purple",
       add=T)

tt.low <- survfit(Surv(mon) ~ sub, data=pyg[pyg$dens<12,])
plot(tt.low,col=1:6, lwd=2, xlab="time", ylab="survival")
legend("bottomleft", legend=levels(pyg$sub), lty=1, lwd=2, col=1:6)
tt.high <- survfit(Surv(mon) ~ sub, data=pyg[pyg$dens >=
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