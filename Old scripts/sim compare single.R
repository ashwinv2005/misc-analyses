library(RNetLogo)
library(stringi)
nl.path = "C:/Program Files (x86)/Netlogo 5.2-RC3"
NLStart(nl.path, gui = F)
model.path = "C:/Ashwin/R Matlab Netlogo/Netlogo Files/Predation/Setup 1 for comparison.nlogo"
NLLoadModel(model.path)

## Set variables
days = 40
treatments = c("other", "con1", "con2", "con3", "con4", "con5", "het1", "het2", "het3", "fig")
rain = c(0.002, 0.5, 0.05, 0.03, 0.01, 0.005, 0.016, 0.012, 0.009, 0.011)
storepath = "C:/Users/ashwinv/Desktop/predfiles/"
## End

## set up world
NLDoCommandWhile("q < 0.01 or q > 0.015 or r < 0.08 or r > 0.15 or s < 0.03 or s > 0.1", "setup")
s1 = NLReport("ss1")
s2 = NLReport("SS2")
s3 = NLReport("SS3")
s4 = NLReport("SS4")
s5 = NLReport("SS5")
s6 = NLReport("Ss6")
s7 = NLReport("SS7")
s8 = NLReport("SS8")
s9 = NLReport("SS9")
s10 = NLReport("SS10")
s = c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)
daylen = 10
change = 0
countiterations = 0
## set up finished

dailypred = data.frame(day = rep(0,10), treat = treatments, srain = rain, area = s, seeds = rep(0,10), pred = rep(0,10), actpred = rep(0,10), actseeds = rep(0,10), indexp = rep(100,10))
temp = dailypred

## set up new start with original set up
NLDoCommand(1, "initialize-variables")
## end initialization

## get set up details
c1 = NLReport("cr1")
c2 = NLReport("cr2")
c3 = NLReport("cr3")
c4 = NLReport("cr4")
c5 = NLReport("cr5")
c6 = NLReport("cr6")
c7 = NLReport("cr7")
c8 = NLReport("cr8")
c9 = NLReport("cr9")
c10 = NLReport("cr10")
c = c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)

nc1 = NLReport("nocr1")
nc2 = NLReport("nocr2")
nc3 = NLReport("nocr3")
nc4 = NLReport("nocr4")
nc5 = NLReport("nocr5")
nc6 = NLReport("nocr6")
nc7 = NLReport("nocr7")
nc8 = NLReport("nocr8")
nc9 = NLReport("nocr9")
nc10 = NLReport("nocr10")
nc = c(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8,nc9,nc10)

temp$seeds = c
temp$actseeds = c
temp$day = 1

##temp$iter = countiterations + 1
temp$pred = 0
temp$actpred = 0
temp$treat = treatments
temp$srain = rain
temp$area = s

if (countiterations == 0) {dailypred = temp} else {dailypred = rbind(dailypred,temp)}

x = NLGetPatches(c("id","seeds"))
x1 = x[x$id == 20, ]

if (countiterations == 0) {x2 = x1} else {x2 = cbind(x2,x1$seeds)}

## end set up details

## dynamics
system.time(
  for (i in 1:days)
    {
    countiterations = countiterations + 1
    NLDoCommand(1,"go")
    
    c1 = NLReport("cr1")
    c2 = NLReport("cr2")
    c3 = NLReport("cr3")
    c4 = NLReport("cr4")
    c5 = NLReport("cr5")
    c6 = NLReport("cr6")
    c7 = NLReport("cr7")
    c8 = NLReport("cr8")
    c9 = NLReport("cr9")
    c10 = NLReport("cr10")
    c = c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
    
    e1 = NLReport("ea1")
    e2 = NLReport("ea2")
    e3 = NLReport("ea3")
    e4 = NLReport("ea4")
    e5 = NLReport("ea5")
    e6 = NLReport("ea6")
    e7 = NLReport("ea7")
    e8 = NLReport("ea8")
    e9 = NLReport("ea9")
    e10 = NLReport("ea10")
    e = c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)
    
    nc1 = NLReport("nocr1")
    nc2 = NLReport("nocr2")
    nc3 = NLReport("nocr3")
    nc4 = NLReport("nocr4")
    nc5 = NLReport("nocr5")
    nc6 = NLReport("nocr6")
    nc7 = NLReport("nocr7")
    nc8 = NLReport("nocr8")
    nc9 = NLReport("nocr9")
    nc10 = NLReport("nocr10")
    nc = c(nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8,nc9,nc10)
    
    temp$day = i + 1
    ##temp$iter = countiterations + 1
    temp$seeds = c
    temp$pred = e
    temp$actpred = e
    
    temp$treat = treatments
    temp$srain = rain
    temp$area = s
    temp$indexp = nc
    
    dailypred$actpred[(10*(countiterations-1)+1):(10*countiterations)] = e - dailypred$pred[(10*(countiterations-1)+1):(10*countiterations)]
    temp$actseeds = c - dailypred$pred[(10*(countiterations-1)+1):(10*countiterations)]
    
    x = NLGetPatches(c("id","seeds"))
    x1 = x[x$id == 20, ]
    
    dailypred = rbind(dailypred,temp)
    x2 = cbind(x2,x1$seeds)
    print(i)
  }
)

randstr = stri_rand_strings(1, 6)
finpath = stri_join(storepath, randstr, ".csv", sep = "")
write.csv(dailypred, finpath)

randstr = stri_rand_strings(1, 6)
finpath = stri_join(storepath, randstr, ".csv", sep = "")
write.csv(x2, finpath)