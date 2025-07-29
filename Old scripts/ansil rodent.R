

######### NEVER EVER use merged cells in any file that has to be analyzed. In fact stop using merged cells altogether.
######### NEVER EVER use multiple sheets in the same excel file. Keep everything in the same sheet with a column to 
######### differentiate.

######### I am sending you the R files and functions. You need to run both the functions before running the code. 
######### Try to understand the logic behind the codes (they are quite simple :) ) even if it takes 2 or 3 days.

## You may need to install the package 'stringr'

library(stringr)

rodcap = read.csv("C:/Users/ashwinv/Desktop/ansil.csv")
names(rodcap)
rodcap$ind = as.numeric(rodcap$ind)
rodcap$species = as.character(rodcap$species)

date = changedatespecies(rodcap$date) ### Not related, this is just to make the file nicer.
rodcap = cbind(date,rodcap)

rodcap = rodcap[with(rodcap, order(year,month,day)),]

# To save this version of the csv into your default directory or any path of your choice,
# write.csv(rodcap) or write.csv(rodcap,"set path of choice")

### Before doing the analysis, include days when you did not catch anything. 
### For now, I am assuming that at least 1 individual was caught everyday.

curod = spcum(rodcap)
x = 1:length(curod)

plot(x,curod,"b",xlab = "100 trap nights",ylab = "Cumulative captures")

#### Try out other plots and exploration with the data!
