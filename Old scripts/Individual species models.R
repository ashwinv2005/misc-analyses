abund1 = summarySE(sggroup, groupvar = "species", measurevar = "status1")
abund1$N = abund1$N*abund1$status1
abund1 = abund1[,c(1,2)]

abund2 = summarySE(sggroup, groupvar = "species", measurevar = "status2")
abund2$N = abund2$N*abund2$status2
abund2 = abund2[,c(1,2)]

sggroup$abund1 = 0
sggroup$abund2 = 0

for (i in 1:length(abund1$species))
{
  sggroup[sggroup$species == abund1$species[i],]$abund1 = abund1$N[i]
  sggroup[sggroup$species == abund2$species[i],]$abund2 = abund2$N[i]  
}

sggroup$het = sggroup$total - sggroup$status1

###### Final all ######

y = table(sggroup[sggroup$status1 != 0,]$species)
#y = y[y>=5]

temp = sggroup[sggroup$status1 >= 1 & sggroup$species != "SC" & sggroup$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$abund1 = scale(temp$abund1, center = F)
temp$abund2 = scale(temp$abund2, center = F)
temp$het = scale(temp$het, center = F)


temp = temp[temp$status1 != 49,]
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$status1 != 15,]


temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + (1|species) + (0 + statusm|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat, fplot67 = cMat), family="binomial", nAGQ = 0)


temp = temp[temp$plot12 == 1,]
fit9c = glmer (dd ~ abund2 + slope + (1|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp, family="binomial", nAGQ = 0)


fit9c = glmer (dd ~ plot*statusm + het + abund2 + statusm:abund2 + slope + (1|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial", nAGQ = 0)

summary(fit9c)

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")


###### Final SR ######

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "SR" & sggroup$status1 <= 300,]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$het = scale(temp$het, center = F)
#temp = temp[-c(142,49),]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + 
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat,fplot67 = cMat), family="binomial", nAGQ = 0)

fit9c = glmer (dd ~ plot*statusm + het + slope + 
                 (1|locationfac/group/plot), weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial", nAGQ = 0)



summary(fit9c)

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")
fit8 = fit9c

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
dfbetas(influence, sort=TRUE, to.sort="plot7:statusm", abs=FALSE)

hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[4]][,1], xlab="Intercept", main="", breaks = 60) # random effects

plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$status1, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))




####### Climber 1 ###### strong effects plot2:status1, slope ###### weak effect plot7

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "Climber1",]
#temp[temp$plot == "6",]$plot57 = 2
temp$plot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$plot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$status1 != 49,]
temp$het = scale(temp$het, center = F)
#temp = temp[-c(49,9,6,57,69,47,65,44,60),]
#temp = temp[-9,]


temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ plot57*plot67*statusm + slope + 
                 (1|groupfac/plot), weights = status1, data = temp, contrasts = list(plot57 = cMat,plot67 = cMat), family="binomial", nAGQ = 0)

fit9c = glm (dd ~ plot*statusm + het + slope, weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial")


summary(fit9c)

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

summary(fit9c)
mod = summary(fit9c)
fit8 = fit9c

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
head(dfbetas(influence, sort=TRUE, to.sort="plot1:statusm", abs=FALSE))

hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects

plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$status1, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))




###### Symplocos ###### nothing, only intercept, weak fungal effect

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "Symp",]
#temp[temp$plot == "6",]$plot57 = 2
temp$plot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$plot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$status1 != 15,]
temp$het = scale(temp$het, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ plot57*plot67*statusm + slope +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(plot57 = cMat, plot67 = cMat), family="binomial", nAGQ = 0)

fit9c = glmer (dd ~ plot*statusm + het + slope + 
                 (1|locationfac/group), weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial", nAGQ = 0)


summary(fit9c)

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
head(dfbetas(influence, sort=TRUE, to.sort="plot5:statusm", abs=FALSE))

hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects

plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$status1, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

####### FLC #######

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "FLC",]
temp = temp[temp$status1 < 60,]

#temp[temp$plot == "6",]$plot57 = 2
temp$plot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$plot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$het = scale(temp$het, center = F)




mat = rbind(c(-0.5,0.5,0,0,0))


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ plot57*plot67 + statusm + slope + (1|locationfac/group),
               weights = status1, data = temp, contrasts = list(plot57 = cMat, plot67 = cMat), family="binomial", nAGQ = 0)

fit9c = glmer (dd ~ plot*statusm + het + slope + (1|groupfac),
               weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial", nAGQ = 0)

summary(fit9c)

mod = summary(fit9c)
write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")



###### all the rest ###### Only till density 40 as all five treatments are only comparable within this range
###### 
###### 

z = all[all$species %in% unique(temp$species),]

y = table(sggroup[sggroup$status1 != 0,]$species)
y = y[y>=5]

temp = sggroup[sggroup$status1 >= 1 & sggroup$species != "SR" & sggroup$species != "Symp" & 
                 sggroup$species != "Climber1" & sggroup$species != "SC" & sggroup$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
temp = temp[temp$species != "FLC" & temp$status1 <= 25,]

#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
#temp$AOm = scale(temp$AO, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$abund1 = scale(temp$abund1, center = F)
temp$abund2 = scale(temp$abund2, center = F)
temp$het = scale(temp$het, center = F)

#temp = temp[-c(29,639,348,401,162,558,21,436),] ##### least important
#temp = temp[-c(414,324,548,626,744,558,326),] ##### most important

#temp = temp[temp$plot6 == 0,]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + (1|species) + (0 + statusm|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat,fplot67 = cMat), family="binomial", nAGQ = 0)

temp = temp[temp$plot12 == 1,]
fit9c = glmer (dd ~ abund1*statusm + slope + (1|species) + (0 + statusm|species) +
                 (1|groupfac/plot), weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9c = glmer (dd ~ plot*statusm + het + abund2 + abund2:statusm + slope + (1|species) + (0 + statusm|species) +
                 (1|locationfac), weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial", nAGQ = 0)


summary(fit9c)

fit8 = fit9c

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
tail(dfbetas(influence, sort=TRUE, to.sort="plot7", abs=FALSE))

hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[3]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[4]][,1], xlab="Intercept", main="", breaks = 20) # random effects


plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$status1, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8,"response"))

###### rare species ######

y = table(sggroup[sggroup$status1 != 0,]$species)
y = y[y<5]

temp = sggroup[sggroup$status1 != 0 & sggroup$species %in% names(y),]
temp = temp[temp$species != "Hopea",]
temp$species = as.factor(temp$species)

#temp[temp$plot == "6",]$plot57 = 2
temp$plot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$plot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$abund1 = scale(temp$abund1, center = F)
temp$abund2 = scale(temp$abund2, center = F)
temp$het = scale(temp$het, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glm (dd ~ plot57*plot67 + slope,
             weights = status1, data = temp, contrasts = list(plot57 = cMat, plot67 = cMat), family="binomial")

fit9c = glmer (dd ~ plot + het + abund2 + slope + (1|locationfac), 
             weights = status1, data = temp[temp$plot12 == 1,], contrasts = list(plot = cMat), family="binomial")


summary(fit9c)

mod = summary(fit9c)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")





###### Density dependence - Standardizing values with random effects ######

fit8 = fit
a = predict(fit8, type = "response", re.form = NA)
b = predict(fit8, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Predicted null value ######

x = summary(fit8)$coefficients
binomial()$linkinv(x[1,1])

###### OLD - Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit8, FUN, nsim = 10000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","f","i","dens","size","slope","fi","fdens","idens","fsize","isize","fidens","fisize")
parvals = parvals[!is.na(parvals$dens),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$plot57[i] + mpars[3]*temp$plot67[i] + mpars[4]*temp$statusm[i] + mpars[5]*mean(temp$sizem) +
    mpars[6]*mean(temp$slope) + mpars[7]*temp$plot57[i]*temp$plot67[i] + mpars[8]*temp$plot57[i]*temp$statusm[i] + 
    mpars[9]*temp$plot67[i]*temp$statusm[i] + mpars[10]*temp$plot57[i]*mean(temp$sizem) + 
    mpars[11]*temp$plot67[i]*mean(temp$sizem) + mpars[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
    mpars[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
  temp$pred[i] = binomial()$linkinv(x)
}

parvals$ddval = 0
parvals$ddvalf = 0
parvals$ddvali = 0
parvals$ddvalfi = 0
temp$min = 0
temp$max = 0
temp$minf = 0
temp$maxf = 0
temp$mini = 0
temp$maxi = 0
temp$minfi = 0
temp$maxfi = 0

## control

for (i in 1:length(parvals$int))
{
  x = parvals[,1][i] + parvals[,4][i]*mean(temp$statusm) + parvals[,5][i]*mean(temp$sizem) +
    parvals[,6][i]*mean(temp$slope)
  parvals$ddval[i] = binomial()$linkinv(x)
}

parvalsc = parvals[order(parvals$ddval),]

minvals = parvalsc[250,]
minvals = t(minvals)[,1]
maxvals = parvalsc[(length(parvalsc$ddval)-249),]
maxvals = t(maxvals)[,1]

## f

for (i in 1:length(parvals$int))
{
  x = parvals[,1][i] + parvals[,2][i] + parvals[,4][i]*mean(temp$statusm) + parvals[,5][i]*mean(temp$sizem) +
    parvals[,6][i]*mean(temp$slope) + parvals[,8][i]*mean(temp$statusm) + 
    parvals[,10][i]*mean(temp$sizem)
  parvals$ddvalf[i] = binomial()$linkinv(x)
}

parvalsf = parvals[order(parvals$ddvalf),]

minvalsf = parvalsf[250,]
minvalsf = t(minvalsf)[,1]
maxvalsf = parvalsf[(length(parvalsf$ddvalf)-249),]
maxvalsf = t(maxvalsf)[,1]

## i

for (i in 1:length(parvals$int))
{
  x = parvals[,1][i] + parvals[,3][i] + parvals[,4][i]*mean(temp$statusm) + parvals[,5][i]*mean(temp$sizem) +
    parvals[,6][i]*mean(temp$slope) +  
    parvals[,9][i]*mean(temp$statusm) + 
    parvals[,11][i]*mean(temp$sizem) 
  parvals$ddvali[i] = binomial()$linkinv(x)
}

parvalsi = parvals[order(parvals$ddvali),]

minvalsi = parvalsi[250,]
minvalsi = t(minvalsi)[,1]
maxvalsi = parvalsi[(length(parvalsi$ddvali)-249),]
maxvalsi = t(maxvalsi)[,1]

## fi

for (i in 1:length(parvals$int))
{
  x = parvals[,1][i] + parvals[,2][i] + parvals[,3][i] + parvals[,4][i]*mean(temp$statusm) + 
    parvals[,5][i]*mean(temp$sizem) +
    parvals[,6][i]*mean(temp$slope) + parvals[,7][i] + parvals[,8][i]*mean(temp$statusm) + 
    parvals[,9][i]*mean(temp$statusm) + parvals[,10][i]*mean(temp$sizem) + 
    parvals[,11][i]*mean(temp$sizem) + parvals[,12][i]*mean(temp$statusm) + 
    parvals[,13][i]*mean(temp$sizem)
  parvals$ddvalfi[i] = binomial()$linkinv(x)
}

parvalsfi = parvals[order(parvals$ddvalfi),]

minvalsfi = parvalsfi[250,]
minvalsfi = t(minvalsfi)[,1]
maxvalsfi = parvalsfi[(length(parvalsfi$ddvalfi)-249),]
maxvalsfi = t(maxvalsfi)[,1]

for (i in 1:length(temp$dd))
{
  if (temp$plot[i] == "1" | temp$plot[i] == "2")
  {
    x = minvals[1] + minvals[2]*temp$plot57[i] + minvals[3]*temp$plot67[i] + minvals[4]*temp$statusm[i] + minvals[5]*mean(temp$sizem) +
      minvals[6]*mean(temp$slope) + minvals[7]*temp$plot57[i]*temp$plot67[i] + minvals[8]*temp$plot57[i]*temp$statusm[i] + 
      minvals[9]*temp$plot67[i]*temp$statusm[i] + minvals[10]*temp$plot57[i]*mean(temp$sizem) + 
      minvals[11]*temp$plot67[i]*mean(temp$sizem) + minvals[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      minvals[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
    y = maxvals[1] + maxvals[2]*temp$plot57[i] + maxvals[3]*temp$plot67[i] + maxvals[4]*temp$statusm[i] + maxvals[5]*mean(temp$sizem) +
      maxvals[6]*mean(temp$slope) + maxvals[7]*temp$plot57[i]*temp$plot67[i] + maxvals[8]*temp$plot57[i]*temp$statusm[i] + 
      maxvals[9]*temp$plot67[i]*temp$statusm[i] + maxvals[10]*temp$plot57[i]*mean(temp$sizem) + 
      maxvals[11]*temp$plot67[i]*mean(temp$sizem) + maxvals[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      maxvals[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
  }
  if (temp$plot[i] == "5")
  {
    x = minvalsf[1] + minvalsf[2]*temp$plot57[i] + minvalsf[3]*temp$plot67[i] + minvalsf[4]*temp$statusm[i] + minvalsf[5]*mean(temp$sizem) +
      minvalsf[6]*mean(temp$slope) + minvalsf[7]*temp$plot57[i]*temp$plot67[i] + minvalsf[8]*temp$plot57[i]*temp$statusm[i] + 
      minvalsf[9]*temp$plot67[i]*temp$statusm[i] + minvalsf[10]*temp$plot57[i]*mean(temp$sizem) + 
      minvalsf[11]*temp$plot67[i]*mean(temp$sizem) + minvalsf[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      minvalsf[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
    y = maxvalsf[1] + maxvalsf[2]*temp$plot57[i] + maxvalsf[3]*temp$plot67[i] + maxvalsf[4]*temp$statusm[i] + maxvalsf[5]*mean(temp$sizem) +
      maxvalsf[6]*mean(temp$slope) + maxvalsf[7]*temp$plot57[i]*temp$plot67[i] + maxvalsf[8]*temp$plot57[i]*temp$statusm[i] + 
      maxvalsf[9]*temp$plot67[i]*temp$statusm[i] + maxvalsf[10]*temp$plot57[i]*mean(temp$sizem) + 
      maxvalsf[11]*temp$plot67[i]*mean(temp$sizem) + maxvalsf[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      maxvalsf[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
  }
  if (temp$plot[i] == "6")
  {
    x = minvalsi[1] + minvalsi[2]*temp$plot57[i] + minvalsi[3]*temp$plot67[i] + minvalsi[4]*temp$statusm[i] + minvalsi[5]*mean(temp$sizem) +
      minvalsi[6]*mean(temp$slope) + minvalsi[7]*temp$plot57[i]*temp$plot67[i] + minvalsi[8]*temp$plot57[i]*temp$statusm[i] + 
      minvalsi[9]*temp$plot67[i]*temp$statusm[i] + minvalsi[10]*temp$plot57[i]*mean(temp$sizem) + 
      minvalsi[11]*temp$plot67[i]*mean(temp$sizem) + minvalsi[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      minvalsi[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
    y = maxvalsi[1] + maxvalsi[2]*temp$plot57[i] + maxvalsi[3]*temp$plot67[i] + maxvalsi[4]*temp$statusm[i] + maxvalsi[5]*mean(temp$sizem) +
      maxvalsi[6]*mean(temp$slope) + maxvalsi[7]*temp$plot57[i]*temp$plot67[i] + maxvalsi[8]*temp$plot57[i]*temp$statusm[i] + 
      maxvalsi[9]*temp$plot67[i]*temp$statusm[i] + maxvalsi[10]*temp$plot57[i]*mean(temp$sizem) + 
      maxvalsi[11]*temp$plot67[i]*mean(temp$sizem) + maxvalsi[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      maxvalsi[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
  }
  if (temp$plot[i] == "7")
  {
    x = minvalsfi[1] + minvalsfi[2]*temp$plot57[i] + minvalsfi[3]*temp$plot67[i] + minvalsfi[4]*temp$statusm[i] + minvalsfi[5]*mean(temp$sizem) +
      minvalsfi[6]*mean(temp$slope) + minvalsfi[7]*temp$plot57[i]*temp$plot67[i] + minvalsfi[8]*temp$plot57[i]*temp$statusm[i] + 
      minvalsfi[9]*temp$plot67[i]*temp$statusm[i] + minvalsfi[10]*temp$plot57[i]*mean(temp$sizem) + 
      minvalsfi[11]*temp$plot67[i]*mean(temp$sizem) + minvalsfi[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      minvalsfi[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
    y = maxvalsfi[1] + maxvalsfi[2]*temp$plot57[i] + maxvalsfi[3]*temp$plot67[i] + maxvalsfi[4]*temp$statusm[i] + maxvalsfi[5]*mean(temp$sizem) +
      maxvalsfi[6]*mean(temp$slope) + maxvalsfi[7]*temp$plot57[i]*temp$plot67[i] + maxvalsfi[8]*temp$plot57[i]*temp$statusm[i] + 
      maxvalsfi[9]*temp$plot67[i]*temp$statusm[i] + maxvalsfi[10]*temp$plot57[i]*mean(temp$sizem) + 
      maxvalsfi[11]*temp$plot67[i]*mean(temp$sizem) + maxvalsfi[12]*temp$plot57[i]*temp$plot67[i]*temp$statusm[i] + 
      maxvalsfi[13]*temp$plot57[i]*temp$plot67[i]*mean(temp$sizem)
  }
  
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ###### deepskyblue4 darkolivegreen4 darkslategray


ggp = ggplot(temp[temp$status1 >= 1,], aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plotx))) +
  #geom_smooth(data = plot1, aes(x = 1:200, y = pred1bin), col = "deepskyblue4", size = 1, se = F) +
  #geom_smooth(data = plot2middle, aes(x = 1:200, y = pred2middlebin), col = "darkorange4", size = 1, se = F) +
  #geom_smooth(data = plot5middle, aes(x = 1:200, y = pred5middlebin), col = "darkslategray", size = 1, se = F) +
  #geom_smooth(data = plot6, aes(x = 1:200, y = pred6bin), col = "darkseagreen4", size = 1, se = F) +
  #geom_smooth(data = plot7, aes(x = 1:200, y = pred7bin), col = "darkolivegreen4", size = 1, se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "black", size = 0.5, linetype = "dotted", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #scale_shape_manual(values = c(16,2), name="",
  #                   breaks=c("0","1"),
  #                   labels=c("C CW F", "I"))+
  #scale_colour_manual(values = c("deepskyblue4","darkorange4","darkolivegreen4","darkslategray","darkseagreen4"), name="",
  #                    breaks=c("1","2","5","6","7"),
  #                    labels=c("C","CW","F","I","FI"))+
  #scale_colour_grey(start = 0.1, end = .8) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(1,20,40,60,80,100,120,140,160,180,200), limits = c(1,150)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))


sizespecies = unique(temp$species)

s1sm = sggroup[sggroup$plot == "1" & sggroup$size < 10 & sggroup$species %in% sizespecies,]
s2sm = sggroup[sggroup$plot == "2" & sggroup$size < 10 & sggroup$species %in% sizespecies,]
s5sm = sggroup[sggroup$plot == "5" & sggroup$size < 10 & sggroup$species %in% sizespecies,]
s6sm = sggroup[sggroup$plot == "6" & sggroup$size < 10 & sggroup$species %in% sizespecies,]
s7sm = sggroup[sggroup$plot == "7" & sggroup$size < 10 & sggroup$species %in% sizespecies,]

s1la = sggroup[sggroup$plot == "1" & sggroup$size > 60 & sggroup$species %in% sizespecies,]
s2la = sggroup[sggroup$plot == "2" & sggroup$size > 60 & sggroup$species %in% sizespecies,]
s5la = sggroup[sggroup$plot == "5" & sggroup$size > 60 & sggroup$species %in% sizespecies,]
s6la = sggroup[sggroup$plot == "6" & sggroup$size > 60 & sggroup$species %in% sizespecies,]
s7la = sggroup[sggroup$plot == "7" & sggroup$size > 60 & sggroup$species %in% sizespecies,]

sum(s1sm$status1)/length(s1sm[s1sm$status1 >= 1,]$status1)
sum(s2sm$status1)/length(s2sm[s2sm$status1 >= 1,]$status1)
sum(s5sm$status1)/length(s5sm[s5sm$status1 >= 1,]$status1)
sum(s6sm$status1)/length(s6sm[s6sm$status1 >= 1,]$status1)
sum(s7sm$status1)/length(s7sm[s7sm$status1 >= 1,]$status1)

sum(s1la$status1)/length(s1la[s1la$status1 >= 1,]$status1)
sum(s2la$status1)/length(s2la[s2la$status1 >= 1,]$status1)
sum(s5la$status1)/length(s5la[s5la$status1 >= 1,]$status1)
sum(s6la$status1)/length(s6la[s6la$status1 >= 1,]$status1)
sum(s7la$status1)/length(s7la[s7la$status1 >= 1,]$status1)

s1sm = sggroup[sggroup$plot == "1" & sggroup$size < 10,]
s2sm = sggroup[sggroup$plot == "2" & sggroup$size < 10,]
s5sm = sggroup[sggroup$plot == "5" & sggroup$size < 10,]
s6sm = sggroup[sggroup$plot == "6" & sggroup$size < 10,]
s7sm = sggroup[sggroup$plot == "7" & sggroup$size < 10,]

s1la = sggroup[sggroup$plot == "1" & sggroup$size > 60,]
s2la = sggroup[sggroup$plot == "2" & sggroup$size > 60,]
s5la = sggroup[sggroup$plot == "5" & sggroup$size > 60,]
s6la = sggroup[sggroup$plot == "6" & sggroup$size > 60,]
s7la = sggroup[sggroup$plot == "7" & sggroup$size > 60,]

sum(s1sm$status1)/length(s1sm[s1sm$status1 >= 1,]$status1)
sum(s2sm$status1)/length(s2sm[s2sm$status1 >= 1,]$status1)
sum(s5sm$status1)/length(s5sm[s5sm$status1 >= 1,]$status1)
sum(s6sm$status1)/length(s6sm[s6sm$status1 >= 1,]$status1)
sum(s7sm$status1)/length(s7sm[s7sm$status1 >= 1,]$status1)

sum(s1la$status1)/length(s1la[s1la$status1 >= 1,]$status1)
sum(s2la$status1)/length(s2la[s2la$status1 >= 1,]$status1)
sum(s5la$status1)/length(s5la[s5la$status1 >= 1,]$status1)
sum(s6la$status1)/length(s6la[s6la$status1 >= 1,]$status1)
sum(s7la$status1)/length(s7la[s7la$status1 >= 1,]$status1)

sggroup[sggroup$species == "SC" & sggroup$status1 >= 1,]



(sum(temp[temp$plot == "1",]$status1)-sum(temp[temp$plot == "1",]$status2))/sum(temp[temp$plot == "1",]$status1)
(sum(temp[temp$plot == "2",]$status1)-sum(temp[temp$plot == "2",]$status2))/sum(temp[temp$plot == "2",]$status1)
(sum(temp[temp$plot == "5",]$status1)-sum(temp[temp$plot == "5",]$status2))/sum(temp[temp$plot == "5",]$status1)
(sum(temp[temp$plot == "6",]$status1)-sum(temp[temp$plot == "6",]$status2))/sum(temp[temp$plot == "6",]$status1)
(sum(temp[temp$plot == "7",]$status1)-sum(temp[temp$plot == "7",]$status2))/sum(temp[temp$plot == "7",]$status1)

####### diversity models #######

sgfrag2$shannongrop = sgfrag1$shannongro
sgfrag2$shannonlocp = sgfrag1$shannonloc

mat = rbind(c(-0.5,0.5,0),c(-0.5,0,0.5))
mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))
mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp = sggro2
temp$plotx = temp$plot
#temp[temp$plotx == "2",]$plotx = "1"
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size)
temp$slope = temp$slope/100
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plotx*shannonp*sizem + (1|site/location/group), contrasts = list(plotx = cMat), data = temp)
summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgloc2
temp$plotx = temp$plot
#temp[temp$plotx == "2",]$plotx = "1"
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size)
temp$slope = temp$slope/100
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plotx*shannonp*sizem + (1|site/location), contrasts = list(plotx = cMat), data = temp)
summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgfrag2
temp$plotx = temp$plot
#temp[temp$plotx == "2",]$plotx = "1"
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size)
temp$slope = temp$slope/100
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plotx*shannonp*sizem + (1|site), contrasts = list(plotx = cMat), data = temp)
summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sggro2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size)
temp$slope = temp$slope/100
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plotx*shannonp*sizem + (1|site/location/group), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)
summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgloc2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plot57*plot67*shannonp*sizem + (1|site/location), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(shannonsgsgtrans ~ plot*shannonp*sizem + (1|site/location), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgfrag2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$shannonp != 0,]

fit = lmer(shannonsgsgtrans ~ plot57*plot67*shannonp*sizem + (1|site), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(shannonsgsgtrans ~ plot*shannonp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

hist(temp$shannonsgsgtrans, breaks = 100)

fit8 = fit
plot(temp$shannonsgsgtrans)
influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
tail(dfbetas(influence, sort=TRUE, to.sort="plot7", abs=FALSE))

plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$size, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

tempfrag1m = sgfrag1m[,-2]
tempfrag2m = sgfrag2m[,-2]

sgfrag2$SRshannonp = diversity(tempfrag1m,"shannon")
sgfrag2$SRshannon = diversity(tempfrag2m,"shannon")
sgfrag2$SRshannonsgsgtrans = sgfrag2$SRshannon - sgfrag2$SRshannonp

fit = lmer(SRshannonsgsgtrans ~ plot57*plot67*SRshannonp*sizem + (1|site), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)
summary(fit)

sgfrag2$richp = sgfrag1$rich
sgfrag2$richsgsgtrans = sgfrag2$richp - sgfrag2$rich

fit = glm(richsgsgtrans ~ plot57*plot67*richp*sizem, contrasts = list(plot57 = cMat, plot67 = cMat), data = temp, family = "poisson")

fit = glm(richsgsgtrans ~ plot*richp*sizem, contrasts = list(plot = cMat), data = temp, family = "poisson")

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

fit8 = fit
plot(temp$shannonsgsgtrans)
influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
tail(dfbetas(influence, sort=TRUE, to.sort="plot7", abs=FALSE))

plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$size, resid(fit8,"response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

#### Final diversity without SC ####

mat = rbind(c(-0.5,0.5,0,0,0))
mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp = sgloc2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$shannonp != 0,]

fit = lmer(transSC ~ plot57*plot67*shannonSCp*sizem + (1|site), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(transSC ~ plot*shannonSCp*sizem + (1|site/location), contrasts = list(plot = cMat), data = temp)

fit = lmer(transAO ~ plot57*plot67*shannonAOp*sizem + (1|site/location), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(transAO ~ plot*shannonAOp*sizem + (1|site/location), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgfrag2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$shannonp != 0,]

fit = lmer(transSC ~ plot57*plot67*shannonSCp + (1|site), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(transSC ~ plot*shannonSCp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

fit = lmer(transAO ~ plot57*plot67*shannonAOp*sizem + (1|site), contrasts = list(plot57 = cMat, plot67 = cMat), data = temp)

fit = lmer(transAO ~ plot*shannonAOp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### Diversity differences from 0 ######

temp = sgloc2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$shannonp != 0,]
temp$plot1 = 0
temp[temp$plot == "1",]$plot1 = 1
temp$plot2 = 0
temp[temp$plot == "2",]$plot2 = 1
temp$plot5 = 0
temp[temp$plot == "5",]$plot5 = 1
temp$plot6 = 0
temp[temp$plot == "6",]$plot6 = 1
temp$plot7 = 0
temp[temp$plot == "7",]$plot7 = 1
temp$plot12 = 0
temp[temp$plot1 == 1,]$plot12 = 1
temp[temp$plot2 == 1,]$plot12 = 1
temp$plot57 = 0
temp[temp$plot5 == 1,]$plot57 = 1
temp[temp$plot7 == 1,]$plot57 = 1
temp$plot67 = 0
temp[temp$plot6 == 1,]$plot67 = 1
temp[temp$plot7 == 1,]$plot67 = 1


temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

temp = temp[temp$shannonSCp != 0,]
fit = lmer(transSC ~ 0 + plot12 + plot57*plot67 + plot12:shannonSCp + (plot57*plot67):shannonSCp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transSC ~ plot*shannonSCp*sizem + (1|site/location), contrasts = list(plot = cMat), data = temp)

temp = temp[temp$shannonIp != 0,]
fit = lmer(transI ~ 0 + plot12 + plot57*plot67 + plot12:shannonIp + (plot57*plot67):shannonIp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transI ~ plot*shannonIp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

temp = sgfrag2
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
#temp[temp$plotx == "7",]$plotx = "6"
#temp = temp[temp$plotx != "5",]
temp$sizem = scale(temp$size, center = F)
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp$plot57 = 0
temp[temp$plot5 == 1,]$plot57 = 1
temp[temp$plot7 == 1,]$plot57 = 1
temp$plot67 = 0
temp[temp$plot6 == 1,]$plot67 = 1
temp[temp$plot7 == 1,]$plot67 = 1

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

temp = temp[temp$shannonSCp != 0,]
fit = lmer(transSC ~ 0 + plot12 + plot57*plot67 + plot12:shannonSCp + (plot57*plot67):shannonSCp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transSC ~ plot*shannonSCp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

temp = temp[temp$shannonSPp != 0,]
fit = lmer(transSP ~ 0 + plot12 + plot57*plot67 + plot12:shannonSPp + (plot57*plot67):shannonSPp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transSP ~ plot*shannonSPp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

temp = temp[temp$shannonSRp != 0,]
fit = lmer(transSR ~ 0 + plot12 + plot57*plot67 + plot12:shannonSRp + (plot57*plot67):shannonSRp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transSR ~ plot*shannonSRp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)


temp = temp[temp$shannonIp != 0,]
fit = lmer(transI ~ 0 + plot12 + plot57*plot67 + plot12:shannonIp + (plot57*plot67):shannonIp + 
             plot12:sizem + (plot57*plot67):sizem + (1|site), data = temp)

fit = lmer(transI ~ plot*shannonIp*sizem + (1|site), contrasts = list(plot = cMat), data = temp)

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

hist(temp$shannonI, breaks = 20)
fit8 = fit9c
plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(temp$size, resid(fit8,"response")) # residuals vs. predictor 1
qqnorm(resid(fit8, "response"))
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects

temp = sggroup[sggroup$species == "Climber1" & sggroup$status1 <= 300,]
temp$sizem = scale(temp$size, center = F)
with(temp, plot(status1~size))
hist(temp$size, breaks = 100)
fit = zeroinfl(status1~sizem|1, data = temp)
fit = zeroinfl(status1~sizem|1, data = temp, dist = "negbin")
summary(fit)

fit = glm.nb(status1~sizem, data = temp)
summary(fit)

hist(temp$sizem)

temp = sggroup[sggroup$species == "SC",]

sum(temp[temp$size < 10,]$status1)
sum(temp[temp$size >= 10 & temp$size <= 60,]$status1)
sum(temp[temp$size > 60,]$status1)

mean(temp[temp$size < 10,]$status1)
mean(temp[temp$size >= 10 & temp$size <= 60,]$status1)
mean(temp[temp$size > 60,]$status1)

length(unique(temp[temp$size < 10,]$species))
length(unique(temp[temp$size >= 10 & temp$size <= 60,]$species))
length(unique(temp[temp$size > 60,]$species))



with(temp, plot(status1~size))