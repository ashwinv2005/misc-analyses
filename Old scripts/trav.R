trav = read.csv("C:/Ashwin/Data Sheets CSV/trav.csv")
trav1 = trav
travname = read.csv("C:/Ashwin/Data Sheets CSV/travname.csv")
names(trav)
names(travname)
trav$name = ""
trav$date = as.character(trav$date)
travname$Travel.Date = as.character(travname$Travel.Date)
trav$slno = as.character(trav$slno)
travname$Invoice.No. = as.character(travname$Invoice.No.)
travname$Passenger = as.character(travname$Passenger)
for (i in 1:length(trav$total))
{
  for (j in 1:length(travname$Passenger))
  {
    if (trav$date[i] == travname$Travel.Date[j] && trav$slno[i] == travname$Invoice.No.[j])
    {
      trav$name[i] = travname$Passenger[j]
    }
  }
}
table(trav$name)

table(trav$name)
head(trav$name)
str(travname$Travel.Date)
str(trav$name)

indivtrav = summarySE(trav, measurevar="total", groupvars=c("route","name"))

indivtrav$perc = indivtrav$sd/indivtrav$total
outlietrav = indivtrav[with(indivtrav, order(-perc)),]
indivtrav[is.na(indivtrav$perc),]$perc = 0;
travmeansd = summarySE(indivtrav, measurevar="perc", groupvars=c("name"))
outlietrav1 = travmeansd[with(travmeansd, order(-perc)),]
fintrav1 = outlietrav1[1:12,1:3]
names(fintrav1)[3] = "mean proportion deviation"
write.csv(fintrav1,"C:/Ashwin/fintrav1.csv")

fintrav = outlietrav[outlietrav$perc >= 0.2, c(1,2,3,4,8)]
fintrav = fintrav[!is.na(fintrav$name),]
names(fintrav)[3:5] = c("no of trips", "mean expense", "proportion deviation")
write.csv(fintrav, "C:/Ashwin/fintrav.csv")


head(an)
head(trav1)
trav$routeon = trav1$routeon
trav$routere = trav1$routere
trav$route = ""

an

for (i in 1:length(trav$slno))
{
  for (j in 1:length(an$route))
  {
    if (trav$routeon[i] == an$routeon[j])
    {
      if (an$route[j] != "n")
      {
        trav$route[i] = an$route[j]
      }
      if (an$route[j] == "n")
      {
        trav$route[i] = trav$routeon[i]
      }
      break
    }
  }
}

head(an$routeon)
trav$name

write.csv(trav, "C:/Ashwin/trav.csv")
?fitdistr
hist(travfin$total,200)
travfin[travfin$total > 100000,]

fitlnorm = fitdistr(travfin$total, "lognormal")
fitnor = fitdistr(travfin$total, "normal")
fitpoi = fitdistr(travfin$total, "poisson")
fitweib = fitdistr(travfin$total, "weibull")
fitnbin = fitdistr(travfin$total, "negative binomial")

x = seq(0,100000,10)
y = dlnorm(x,fitlnorm$estimate[1],fitlnorm$estimate[2])
y1 = dlnorm(x,8.993,0.6186)
plot(density(travfin$total), xlab = "Cost", ylab = "Probability", main = "Log-normal distribution of one-way expenses")
lines(x,y, col = "red")
lines(x,y1,col = "green")

an$routere <- do.call(paste, c(an[c("arr", "dep")], sep = ""))
length(an$routeon)
an$route = "n"

for (i in 1:101)
{
  for (j in 1:101)
  {
    if (an$routeon[i] == an$routere[j])
    {
      an$route[i] = an$route[j] = do.call(paste, c(an[i,c("routeon", "dep")], sep = ""))
      an$routere[i] = an$routeon[i]
    }
  }
}

an$totalcost = an$N*an$total


ann = summarySE(travfin, measurevar="total", groupvar=c("route"))
an2$totalcost = an2$N*an2$totalcost
an2$trips = an1$trips
an2$route = an1$route
an2$avecost = an2$totalcost/an2$trips
an2 = an2[,-c(2,4,5,6)]
an3 = an2

an5 = summarySE(an3, measurevar="trips", groupvar=c("route"))
an5$trips = an5$N*an5$trips
an4$totalcost = an4$N*an4$totalcost
an4$trips = an5$trips
an4 = an4[,-c(2,4,5,6)]
an4$avecost = an4$totalcost/an4$trips
an4$airports = c("Bangalore Ahmedabad", "Bangalore Mumbai", "Bangalore Delhi", "Bangalore Dubai", "Bangalore Newark", "Bangalore Hyderabad", "Bangalore Chandigarh", "Bangalore Kuala Lampur", "Bangalore Lucknow", "Bangalore Chennai", "Bangalore Shanghai", "Bangalore Singapore", "Mumbai Delhi", "Mumbai Hyderabad", "Others")

an8 = an4[with(an4, order(-avecost)),]

length(an1$trips)
an1$route[33] = "Others"

for (i in 1:34)
{
  if (an1$trips[i] < 8)
    an1$route[i] = "Others"
}

slices = an8$avecost
lbls = an8$airports
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, "Rs.", round(slices)) # add percents to labels
lbls = paste("Rs.",sep="", lbls) # ad % to labels 
pie(slices, labels = lbls, main = "Average cost of travel", cex = 0.8)

barplot(an6$trips, width = 1, names.arg = an6$airports)
?barplot

totalcost = sum(an6$totalcost)
trips = sum(an6$trips)
ave = totalcost/trips
ave

an9 = an7[an7$totalcost>300000,]
an9$totalcost = an9$totalcost/totalcost*100
an9$avecost = an9$avecost/sum(an7$avecost)*100
count = rbind(t(an9$totalcost), t(an9$avecost))
barplot(count, beside = T, width = 1, names.arg = an9$airports, cex.names = 0.7, col = c("aquamarine3","coral"), main = "Comparison between total cost and average cost", xlab = "Route", ylab = "Relative weights")


## stay

stay = read.csv("C:/Ashwin/Data Sheets CSV/stay.csv")
head(stay)
str(stay$rate)
outstayrate = summarySE(stay, measurevar="rate", groupvars=c("city","guests"))

outstayrate$perc = outstayrate$sd/outstayrate$rate
outliestay = outstayrate[with(outstayrate, order(-perc)),]
outliestay[is.na(outliestay$perc),]$perc = 0;
finstay = outliestay[outliestay$perc >= 0.2, c(1,2,3,4,8)]
finstay = finstay[!is.na(finstay$guests),]
names(finstay)[3:5] = c("no of visits", "mean expense", "proportion deviation")
write.csv(finstay, "C:/Ashwin/finstay.csv")

staymeansd = summarySE(outliestay, measurevar="perc", groupvars=c("guests"))
outliestay1 = staymeansd[with(staymeansd, order(-perc)),]
finstay1 = outliestay1[outliestay1$perc >= 0.2,1:3]
names(finstay1)[2:3] = c("no of cities","mean proportion deviation")
write.csv(outliestay1,"C:/Ashwin/finstay1.csv")

head(stay)
hist(stay$rate,100)
stay[stay$rate == max(stay$rate),]

fitlnorm = fitdistr(stay$rate, "lognormal")
fitnor = fitdistr(stay$rate, "normal")
fitpoi = fitdistr(stay$rate, "poisson")
fitweib = fitdistr(stay$rate, "weibull")

x = seq(0,100000,10)
y = dlnorm(x,fitlnorm$estimate[1],fitlnorm$estimate[2])
plot(density(stay$rate), xlab = "Cost of 1 night", ylab = "Probability", main = "Log-normal distribution of hotel rates")
lines(x,y, col = "red")

summary(fitlnorm)

sn = summarySE(stay, measurevar="total", groupvars=c("city"))
sn$total1 = sn$N*sn$total
sn1 = sn[with(sn, order(-meanrate)),]
sn2 = summarySE(stay, measurevar = "nights", groupvar = "city")
sn2$visitnights = sn2$N*sn2$nights
sn$visitnights = sn2$visitnights
sn$meanrate = sn$total1/sn$visitnights

sn3 = sn1[, c(1,7,8,9)]
sn3[with(sn3, order(-visitnights)),]

write.csv(sn3, "C:/Ashwin/staysummary.csv")
head(sn3)

## product

forex = read.csv("C:/Ashwin/Data Sheets CSV/forex.csv")
head(indloss1)
str(forex$loss)
forex = forex[,-8]
forex$percloss = forex$loss/forex$inr

boxplot(forex$inr~forex$product, ylab = "Price", main = "Comparison of Prices - CN and VTM")
indloss = summarySE(forex, measurevar = "percloss", groupvar = "name")

indloss = indloss[,-c(4:6)]
indloss1 = indloss[with(indloss, order(-percloss)),]
indloss2 = indloss[with(indloss, order(-N)),]

write.csv(indloss2, "C:/Ashwin/most transactions.csv")


hist(forex[forex$product == "VTM",]$inr, 100)

## new travel

ann3 = ann1[with(ann1, order(-totalcost)),]
ann1 = ann1[,-c(4,5,6)]
names(ann1)[c(2,3)] = c("trips","avecost")
head(ann1)
ann2
ann1$totalcost = ann1$trips*ann1$avecost
length(ann1[ann1$trips > 20,]$trips)

ann1$cat = "Others"
ann1[ann1$trips >= 23,]$cat = as.character(ann1[ann1$trips >= 23,]$route)
ann1$cat = as.character(ann1$cat)
b1 = summarySE(ann1, measurevar = "totalcost", groupvar = "cat")
a1$trips = a1$N*a1$trips
names(a1)[1] = "route"
a1 = a1[,-c(2,4,5,6)]
a1$totalcost = b1$N*b1$totalcost
a1$avecost = a1$totalcost/a1$trips
a1$airports = c("Bangalore Ahmedabad", "Bangalore Mumbai", "Bangalore Kolkata", "Bangalore Delhi", "Bangalore Dubai", "Bangalore Newark", "Bangalore Hyderabad", "Bangalore Chandigarh", "Bangalore Kuala Lampur", "Bangalore Chennai", "Bangalore Singapore", "Mumbai Delhi", "Mumbai Hyderabad", "Bangalore France", "Others", "Bangalore Shanghai")
c1 = a1[with(a1, order(-avecost)),]

d1 = b1[-c(14:16),]
# pie chart

slices = d1$totalcost
lbls = d1$airports
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, "Rs.", round(slices)) # add percents to labels
lbls = paste(lbls, " ", pct, sep="", "%") # ad % to labels 
pie(slices, labels = lbls, main = "Total cost of travel", cex = 0.8)

barplot(an6$trips, width = 1, names.arg = an6$airports)
?barplot

totalcost = sum(an6$totalcost)
trips = sum(an6$trips)
ave = totalcost/trips
ave
b1

totalcost = sum(b1$totalcost)
head(ann3)
a9 = b1[b1$totalcost>1000000,]
a9$totalcost = a9$totalcost/totalcost*100
a9$avecost = a9$avecost/sum(a9$avecost)*100
count = rbind(t(a9$totalcost), t(a9$avecost))
barplot(count, beside = T, width = 1, names.arg = a9$airports, cex.names = 0.7, col = c("aquamarine3","coral"), main = "Comparison between total cost and average cost", xlab = "Route", ylab = "Relative weights")

