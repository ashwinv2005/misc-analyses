trav2 = read.csv("c:/Ashwin/Travel 2.csv")
travfin = read.csv("c:/Ashwin/Data Sheets CSV/travfin.csv")
names(trav2)
travmast[1:20,]
str(trav2)
substr(trav2$sector[1],1,3)
trav2$passenger = as.character(trav2$passenger)
trav2$sector = as.character(trav2$sector)
trav2$via = as.character(trav2$via)
c = 0
trav2$arr = ""
trav2$dep = ""
for (i in 1:length(trav2$passenger))
{
  if (trav2$round[i] == 0)
  {    
    if (c == 0)
    {
      trav2$arr[i] = substr(trav2$sector[i],1,3)
      trav2$dep[i] = substr(trav2$sector[i],5,7)
      c = c + 1
    }
    else
    {
      trav2$dep[i] = trav2$arr[i-1]
      trav2$arr[i] = trav2$dep[i-1]
      c = c - 1
    }    
  }
  if (trav2$round[i] == 1)
  {
    trav2$arr[i] = substr(trav2$sector[i],1,3)
    trav2$dep[i] = substr(trav2$sector[i],9,11)
  }
  if (trav2$round[i] != 0 & trav2$round[i] != 1)
  {
    trav2$arr[i] = substr(trav2$sector[i],1,3)
    trav2$dep[i] = substr(trav2$sector[i],5,7)
  }
}

trav2$routeon = do.call(paste, c(trav2[c("dep", "arr")], sep = ""))

write.csv(travmast, "c:/Ashwin/travmast1.csv")

travmast = read.csv("c:/Ashwin/travmast.csv")
head(travmast)
travmast$route = ""
travmast$routere = as.character(travmast$routere)
travmast$route[1] = do.call(paste, c(travmast[1,][c("routeon", "dep")], sep = ""))


for (i in 2:length(travmast$name))
{
  c = 0
  for (j in 1:(i-1))
  {
    if (travmast$routeon[i] == travmast$routeon[j] || travmast$routeon[i] == travmast$routere[j])
    {
      travmast$route[i] = travmast$route[j]
      c = 1
      break
    }
  }
  if (c == 0)
  {
    travmast$route[i] = do.call(paste, c(travmast[i,][c("routeon", "dep")], sep = ""))
  }
}

indivtravfin = summarySE(travfin, measurevar="total", groupvars=c("route","name"))
indivtravfin$perc = indivtravfin$sd/indivtravfin$total
outlietravfin = indivtravfin[with(indivtravfin, order(-perc)),]
table1 = outlietravfin[outlietravfin$perc > 0.2 & outlietravfin$N > 2,]
table1 = table1[,-c(5,6,7)]
names(table1)[3:5] = c("no of trips", "mean expense", "proportion deviation")
write.csv(table2, "C:/Ashwin/Table 2.csv")

t2 = summarySE(outlietravfin, measurevar="perc", groupvar="name")
t2 = t2[with(t2, order(-perc)),]
head(t2)
t2 = t2[,-c(4,5,6)]
names(table2)[2:3] = c("no of routes","mean proportion deviation")
table2 = t2[t2$perc > 0.1,]
table2 = table2[1:14,]
##travfin[travfin$route == "BLRBOMBLR" & travfin$name == "Chaudhuri Vivek Mr.", ]
##travfin[1890,3] = 11535
