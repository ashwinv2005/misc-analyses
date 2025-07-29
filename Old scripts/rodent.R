library(RNetLogo)
nl.path = "C:/Program Files (x86)/Netlogo 5.2-RC3"
NLStart(nl.path, gui = F)
model.path = "C:/Ashwin/R Matlab Netlogo/Netlogo Files/Predation/Rodent with exp plots.nlogo"
NLLoadModel(model.path)

## conspecifics variable
days = 40
## initialized

## seed rain vector
rain = c(0.002, 0.5, 0.05, 0.03, 0.01, 0.005, 0.016, 0.012, 0.009, 0.011)
## initialized

## set up world
NLDoCommandWhile("q < 0.01 or q > 0.011", "setup")
maxx = NLReport("max-pxcor")
maxy = NLReport("max-pycor")
fl = (2*maxx+1)*(2*maxy+1)
daylen = 10
change = 0
countiterations = 0
writeflag = 0
## set up finished

for (it1 in 1:2)
{
  for (it2 in 1:4)
  {
    for (it3 in 1:4)
    {
      for (it4 in 1:3)
      {
        for (it5 in 1:3)
        {
          for (it6 in 1:3)
          {
            for (it7 in 1:3)
            {
              for (it8 in 1:3)
              {
                ## set up new start with original set up
                if ((countiterations != 0 & NLReport("changeflag") == 0) || writeflag > 0)
                  NLDoCommand(1, "initialize-variables")
                else if ((countiterations != 0 & NLReport("changeflag") == 1) || writeflag > 0)
                  NLDoCommand(1, "initialize-variables1")
                ## end initialization
                
                ## get set up details
                x = NLGetPatches(c("plabel","id","seeds","pred"))
                x$day = 1
                x$funcres = NLReport("funcres")
                x$number = NLReport("N")
                x$baseline = NLReport("baseline")
                x$loops = NLReport("loops")
                x$cap = NLReport("cap")
                x$ars = NLReport("ars")
                x$lag = NLReport("lag")
                x$gud = NLReport("gud")
                x$minprev = NLReport("minprev")
                x$iter = countiterations + 1
                if (countiterations == 0)
                  x2 = x
                else
                  x2 = rbind(x2,x)
                ## end set up details
                
                ## dynamics
                for (i in 1:days)
                {
                  countiterations = countiterations + 1
                  NLDoCommand(1,"go")
                  x = NLGetPatches(c("plabel","id","seeds","pred"))
                  x[1:fl,]$day = i + 1
                  x[1:fl,]$funcres = NLReport("funcres")
                  x[1:fl,]$number = NLReport("N")
                  x[1:fl,]$baseline = NLReport("baseline")
                  x[1:fl,]$loops = NLReport("loops")
                  x[1:fl,]$cap = NLReport("cap")
                  x[1:fl,]$ars = NLReport("ars")
                  x[1:fl,]$lag = NLReport("lag")
                  x[1:fl,]$gud = NLReport("gud")
                  x[1:fl,]$minprev = NLReport("minprev")
                  x[1:fl,]$iter = countiterations + 1
                  x2$pred[(fl*(countiterations-1)+1):(fl*countiterations)] = x$pred - x2$pred[(fl*(countiterations-1)+1):(fl*countiterations)]
                  x2 = rbind(x2,x)
                }
                countiterations = countiterations + 1
                ## end dynamics
                if (countiterations > 1000)
                {
                  
                  writeflag = writeflag + 1
                  countiterations = 0
                }
                NLDoCommand(1, "change-minprev")
              }
              if (countiterations > 200)
                break
              NLDoCommand(1, "change-gud")
            }
            if (countiterations > 200)
              break
            NLDoCommand(1, "change-lag")
          }
          if (countiterations > 200)
            break
          NLDoCommand(1, "change-ars")
        }
        if (countiterations > 200)
          break
        NLDoCommand(1, "change-cap")
      }
      if (countiterations > 200)
        break
      NLDoCommand(1, "change-loops")
    }
    if (countiterations > 200)
      break
    NLDoCommand(1, "change-N")
  }
  if (countiterations > 200)
    break
  NLDoCommand(1, "change-funcres")
}
  



NLQuit()

## plot seeds at conspecifics
time = 1:(days+1)
plot(con ~ time, xlab = "Days", ylab = "Total no of seeds")
## end plot

## isolate last rows of data frame
l = length(x2$seeds)
lastrows = x2[(l-fl+1):l, ]
lastrows$total = lastrows$seeds + lastrows$pred
lastrows = lastrows[lastrows$total > 0, ]
lastrows$prop = lastrows$pred/lastrows$total
lastrowsexp = lastrows[lastrows$id == 20, ]
lastrowsnorm = lastrows[lastrows$id != 20, ]
## finished

## last rosw exp and norm average
expsummary = summarySE(lastrowsexp, groupvar = "total", measurevar = "pred")
expsummary = expsummary[, 1:3]
expsummary$prop = expsummary$pred/expsummary$total
normsummary = summarySE(lastrowsnorm, groupvar = "total", measurevar = "pred")
normsummary = normsummary[, 1:3]
normsummary$prop = normsummary$pred/normsummary$total
## end

## plot exp and norm prop vs total
plot(lastrowsexp$prop ~ lastrowsexp$total, xlab = "Initial seeds in a plot", ylab = "Proportion predation")
plot(lastrowsnorm$prop ~ lastrowsnorm$total, xlab = "Total seeds arriving in a plot", ylab = "Proportion predation")
## end plot

## plot averages
plot(expsummary$prop ~ expsummary$total, xlab = "No of initial seeds", ylab = "Average proportion predation")
plot(normsummary$prop ~ normsummary$total, xlab = "No of arriving seeds", ylab = "Average proportion predation")
## end

## plot predation vs treatment
treatpred = with(lastrowsnorm, tapply(pred, id, sum))
treatseeds = with(lastrowsnorm, tapply(total, id, sum))
treatprop = treatpred/treatseeds
plot(treatprop ~ rain, xlab = "Seed rain density", ylab = "Proportion predation")
## end plot

## isolate first rows
firstrows = x2[1:(l-fl), ]
firstrows = firstrows[firstrows$seeds > 0, ]
firstrows$prop = firstrows$pred/firstrows$seeds
firstrowsexp = firstrows[firstrows$id == 20, ]
firstrowsnorm = firstrows[firstrows$id != 20, ]
## end

## first row averages
expsummary1 = summarySE(firstrowsexp, groupvar = "seeds", measurevar = "pred")
expsummary1 = expsummary1[, 1:3]
expsummary1$prop = expsummary1$pred/expsummary1$seeds
normsummary1 = summarySE(firstrowsnorm, groupvar = "seeds", measurevar = "pred")
normsummary1 = normsummary1[, 1:3]
normsummary1$prop = normsummary1$pred/normsummary1$seeds
## end

## plot exp and norm vs seeds
#plot(firstrowsexp$prop ~ firstrowsexp$seeds, xlab = "No of seeds at a point of time in exp plot", ylab = "Proportion eaten in response")
#plot(firstrowsnorm$prop ~ firstrowsnorm$seeds, xlab = "No of seeds at a point of time", ylab = "Proportion eaten in response")
## end

## plot averages
plot(expsummary1$prop ~ expsummary1$seeds, xlab = "No of seeds in exp plots", ylab = "Proportion predation")
plot(normsummary1$prop ~ normsummary1$seeds, xlab = "No of seeds", "Proportion predation")
## end




