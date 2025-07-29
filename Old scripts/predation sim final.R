## Anyway, I am sure you'll understand the whole thing quite easily! Just install R, R-Studio, Netlogo and the required packages. All the
## dynamics are coded on Netlogo only. So you will be running a bunch of simulations which generate the data. That's what the files are. 
## The only data I have is what I have to compare the results to to see what parameter values work. That will be simple stats!

## The whole thing is setup such that if you need to stop the run and continue, you can do so. Just run the "countiterations = 0" command and restart ONLY the loop.

library(RNetLogo)
library(stringi)

## Set Netlogo path and start it (set the path as applicable)

nl.path = "C:/Program Files (x86)/Netlogo 5.2-RC3"
NLStart(nl.path, gui = F)

## Set model path as applicable (Setup 1, Setup 2, etc. are the names)

model.path = "C:/Ashwin/R Matlab Netlogo/Netlogo Files/Predation/Rodent with exp plots.nlogo"
NLLoadModel(model.path)

## Set variables

days = 40
treatments = c("other", "con1", "con2", "con3", "con4", "con5", "het1", "het2", "het3", "fig")
rain = c(0.002, 0.5, 0.05, 0.03, 0.01, 0.005, 0.016, 0.012, 0.009, 0.011)

## Name of storage directory (use new folder for each run!)

storepath = "C:/Users/ashwinv/Desktop/predfiles/"

## set up world (don't worry about the next many lines)

NLDoCommandWhile("q < 0.03 or q > 0.035 or r < 0.1 or r > 0.15 or s < 0.03 or s > 0.06", "setup")
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
countrun = 1

## set up finished

dailypred = data.frame(iter = rep(0,10), run = rep(0,10), day = rep(0,10), number = rep(0,10), loops = rep(0,10), cap = rep(0,10), ars = rep(0,10), gud = rep(0,10), baseline = rep(0,10), lag = rep(0,10), minprev = rep(0,10), treat = treatments, srain = rain, area = s, seeds = rep(0,10), pred = rep(0,10), actpred = rep(0,10), actseeds = rep(0,10))
temp = dailypred

for (it1 in 1:2)
{
  for (it2 in 1:2)
  {
    for (it3 in 1:2)
    {
      for (it4 in 1:2)
      {
        for (it5 in 1:2)
        {
          for (it6 in 1:2)
          {
            for (it7 in 1:2)
            {
              for (it8 in 1:2)
              {
                
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
                
                temp$seeds = c
                temp$actseeds = c
                temp$day = 1
                nb = temp$number = NLReport("N")
                bs = temp$baseline = NLReport("baseline")
                lp = temp$loops = NLReport("loops")
                cp = temp$cap = NLReport("cap")
                ar = temp$ars = NLReport("ars")
                lg = temp$lag = NLReport("lag")
                gd = temp$gud = NLReport("gud")
                mp = temp$minprev = NLReport("minprev")
                temp$iter = countiterations + 1
                temp$pred = 0
                temp$actpred = 0
                temp$treat = treatments
                temp$srain = rain
                temp$area = s
		            temp$run = countrun
                
                if (countiterations == 0)
                  dailypred = temp
                else
                  dailypred = rbind(dailypred,temp)
		            
## end set up details
                
## dynamics
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
                  
                  temp$day = i + 1
                  temp$number = nb
                  temp$baseline = bs
                  temp$loops = lp
                  temp$cap = cp
                  temp$ars = ar
                  temp$lag = lg
                  temp$gud = gd
                  temp$minprev = mp
                  temp$iter = countiterations + 1
                  temp$seeds = c
                  temp$pred = e
                  temp$actpred = e
                  
                  temp$treat = treatments
                  temp$srain = rain
                  temp$area = s
		              temp$run = countrun
                  
                  dailypred$actpred[(10*(countiterations-1)+1):(10*countiterations)] = e - dailypred$pred[(10*(countiterations-1)+1):(10*countiterations)]
                  temp$actseeds = c - dailypred$pred[(10*(countiterations-1)+1):(10*countiterations)]
                  
                  dailypred = rbind(dailypred,temp)
                }
                
                countiterations = countiterations + 1
                
## end dynamics
                
                if (NLReport("minprev") == 6)
                {
                  NLDoCommand(1, "change-minprev")
                  break
                }
                NLDoCommand(1, "change-minprev")
		            countrun = countrun + 1
              }
              
## VERY IMPORTANT - Set the number in the next 'if' command to whatever length you want to store each file at whatever size you want - for example, 125952 means 
## that you will have 2 files for each run which are 52480 lines long. Divide by 2 to shorten if you need to. should be alright with your computing power :)
## 5248 is basically 2 * 2 * 2 * 2 * 2 * 2 * 2 * 41, each with 10 entries, so X 10.
              
	            if (countiterations >= 5248)
              {
                randstr = stri_rand_strings(1, 6)
                finpath = stri_join(storepath, randstr, ".csv", sep = "")
                write.csv(dailypred, finpath)
                countiterations = 0
              }
              
              if (NLReport("lag") == 2)
              {
                NLDoCommand(1, "change-lag")
                break
              }
              NLDoCommand(1, "change-lag")
	            countrun = countrun + 1
            }
            if (NLReport("baseline") == 0.15)
            {
              NLDoCommand(1, "change-baseline")
              break
            }
            NLDoCommand(1, "change-baseline")
	          countrun = countrun + 1
          }
          if (NLReport("gud") == 10)
          {
            NLDoCommand(1, "change-gud")
            break
          }
          NLDoCommand(1, "change-gud")
          countrun = countrun + 1
        }
        if (NLReport("ars") == 4)
        {
          NLDoCommand(1, "change-ars")
          break
        }
        NLDoCommand(1, "change-ars")
        countrun = countrun + 1
      }
      if (NLReport("cap") == 20)
      {
        NLDoCommand(1, "change-cap")
        break
      }
      NLDoCommand(1, "change-cap")
      countrun = countrun + 1
    }
    if (NLReport("loops") == 80)
    {
      NLDoCommand(1, "change-loops")
      break
    }
    NLDoCommand(1, "change-loops")
    countrun = countrun + 1
  }
  if (NLReport("N") == 100)
  {
    NLDoCommand(1, "change-number")
    break
  }
  NLDoCommand(1, "change-number")
  countrun = countrun + 1
}
