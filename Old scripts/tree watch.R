frtr = read.csv("C:/Ashwin/Tree Watch.csv")
forper = read.csv("C:/Ashwin/forper.csv")
names(frtr)
summary(frtr)
frtr$Type[c(1:22,36:47,83:149)] = rep("C",101)
frtr$Type[c(23:35,48:82)] = rep("H",48)
sum(frtr[frtr$Type == "C" & frtr$Activity == "A" & frtr$Name == "PN6" & frtr$Spe == "MI",]$No)
sum(frtr[frtr$Type == "C" & frtr$Activity == "L" & frtr$Name == "PN6" & frtr$Spe == "MI",]$No)
frtr$Time = frtr$Time/100
truval = trunc(frtr$Time)
dec = frtr$Time - truval
frtr$Time = truval*100 + dec*500/3
frtr$Spe = as.character(frtr$Spe)
frtr$Name = as.character(frtr$Name)

w = unique(frtr[frtr$Type == "C",]$Name)
v = unique(frtr[frtr$Type == "C",]$Spe)
fin = data.frame(cbind(1:8,0))
fin = fin[,-c(1,2)]
fin[,1:12] = 0
fint = data.frame(cbind(1:8,0))
fint = fint[,-c(1,2)]
fint[,1:12] = 0
ghj = data.frame(cbind(1:8,0))
ghj = ghj[,-c(1,2)]
ghj[,1:1000] = 0
jk = numeric(8)
jk[1:8] = 0
names(fin) = v
names(fint) = v
for (i in 1:length(w))
{
  for (j in 1:length(v))
  {
    tr = frtr[frtr$Name == w[i] & frtr$Spe == v[j],]
    ob = length(tr$Time)
    indct = 0
    tel = 0
    tme = 0
    int = 0
    if (ob > 0)
    {
      timetemp = tr[tr$Activity == "A",]$Time
      notemp = cumsum(tr[tr$Activity == "A",]$No)
      for (k in 1:ob)
      {
        ct = 0
        if (tr$Activity[k] == "A")
        {
          indct = indct + tr$No[k]
        }
        if (tr$Activity[k] == "L")
        {
          for (p in 1:tr$No[k])
          {
            if ((indct - p) < 0)
              ct = ct + 1
            for (pd in 1:length(timetemp))
            {
              if (notemp[pd] >= (tel + p - ct))
                break
            }
            if (tr$Time[k] >= timetemp[pd])
              {tme = tme + tr$Time[k] - timetemp[pd]
               jk[j] = jk[j] + 1
               ghj[j,(jk[j])] = tr$Time[k] - timetemp[pd]
               int = int+1}
            else
              {tme = tme + tr$Time[k] - 650
               jk[j] = jk[j] + 1
               ghj[j,(jk[j])] = tr$Time[k] - 650
               int = int+1}
          }      
          indct = indct - tr$No[k]
          tel = tel + tr$No[k]
          if (indct < 0)
          {
            tel = tel + indct
            indct = 0
          }
        }
      }
      if (indct > 0)
      {
        for (po in 1:indct)
        {
          for (pdd in 1:length(timetemp))
          {
            if (notemp[pdd] >= tel + po)
              break
          }
          tme = tme + 1050 - timetemp[pdd]          
          jk[j] = jk[j] + 1
          ghj[j,(jk[j])] = 1050 - timetemp[pdd]
          int = int + 1
        }
      }
      fin[i,j] = tme
      fint[i,j] = int
    }
  }
}
fin = fin*0.6
fint
ghj = ghj*0.6
ghj = t(ghj)
ghj = ghj[1:55,]
v1 = sum(fin$GB)/sum(fint$GB)
v2 = sum(fin$HM)/sum(fint$HM)
v3 = sum(fin$MI)/sum(fint$MI)
v4 = sum(fin$HG)/sum(fint$HG)

de = ghj[1:40,1]
de = as.numeric(de)
dr = ghj[1:25,2]
dt = ghj[1:41,7]
dt = as.numeric(dt)
dr = as.numeric(dr)

bh = lapply(1:10000, function(i)sample(de,replace = T))
bj = lapply(1:10000, function(i)sample(dt,replace = T))
bl = lapply(1:10000, function(i)sample(dr,replace = T))
bhmean = sapply(bh,mean)
bjmean = sapply(bj,mean)
blmean = sapply(bl,mean)
hist(blmean)
gbmean = mean(bhmean)
mimean = mean(bjmean)
hmmean = mean(blmean)
gbcl = quantile(bhmean, 0.025)
gbcr = quantile(bhmean, 0.975)
micl = quantile(bjmean, 0.025)
micr = quantile(bjmean, 0.975)
hmcl = quantile(blmean, 0.025)
hmcr = quantile(blmean, 0.975)

fin/fint
sutr = numeric(8)
for (i in 1:8)
  sutr[i] = sum(fin[,i])
barplot(sutr)
dfsutr = data.frame(cbind(1:8,0))
dfsutr = dfsutr[,-c(1,2)]

dfsutr$Name = c("Great Barbet","Hill Myna","Purple Cochoa","Blue-throated Barbet","UnID","Hoolock Gibbon","Mountain Imperial Pigeon","R-necked Hornbill")
dfsutr$Time = sutr
opts(title = expresssion(paste("Estimated passage of ", italic("Fish fish", " in 2001")) )
dfsutr$Name = c("Megalaima virens","Gracula religiosa","Cochoa purpurea","M. asiatica","UnID","Hoolock hoolock","Ducula badia","Aceros nipalensis")
ggfr = ggplot(dfsutr, aes(Name, Time))+ 
  geom_bar(width = 0.5, fill = "dark grey", col = "black")+
  xlab("Frugivore species")+
  ylab("Effective time spent (min)")+
  ##opts(title = expression(paste("Comparison Between Time Spent by Different Frugivores on ", italic("Prunus zeylanica"))))+
  theme_bw()
ggfr +
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(angle = 10, size = 14, face = "italic"), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  ##scale_fill_manual(values=c("#CCCCCC"))+
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))+
  scale_x_discrete(limits = c("Ducula badia","Megalaima virens","Gracula religiosa","Cochoa purpurea","UnID","M. asiatica","Hoolock hoolock","Aceros nipalensis"))+
  opts(panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )

names(forper)
summary(forper)
gb = forper[forper$Spe == "GB",]
mean(gb[gb$Reg == 0,]$For)
boxplot(forper[forper$P == "S",]$For ~ forper[forper$P == "S",]$Spe)

for (i in 1:35)
{
  if (forper$P[i] == "S" & forper$Per[i] != 0)
    forper$ret[i] = "Y"
  if (forper$P[i] == "S" & forper$Per[i] == 0)
    forper$ret[i] = "N"
  if (forper$P[i] == "L")
    forper$ret[i] = "N"
  if (forper$Reg[i] > forper$Le[i])
    forper$Ind[i] = forper$Reg[i]
  else
    forper$Ind[i] = forper$Le[i]
}

forper$wtf = forper$For*forper$Ind
forper$wtp = forper$Per*forper$Ind

mgb = sum(forper[forper$ret == "Y" & forper$Spe == "GB",]$wtp)/sum(forper[forper$ret == "Y" & forper$Spe == "GB",]$Ind)
mhm = sum(forper[forper$ret == "Y" & forper$Spe == "HM",]$wtp)/sum(forper[forper$ret == "Y" & forper$Spe == "HM",]$Ind)
mmi = sum(forper[forper$Per != 0 & forper$Spe == "MI",]$wtp)/sum(forper[forper$ret == "Y" & forper$Spe == "MI",]$Ind)

fgb = sum(forper[forper$Spe == "GB",]$wtf)/sum(forper[forper$Spe == "GB",]$Ind)
fhm = sum(forper[forper$Spe == "HM",]$wtf)/sum(forper[forper$Spe == "HM",]$Ind)
fmi1 = sum(forper[forper$Spe == "MI" & forper$For >=10,]$wtf)/sum(forper[forper$Spe == "MI" & forper$For>=10,]$Ind)
fmi2 = sum(forper[forper$Spe == "MI" & forper$For <10,]$wtf)/sum(forper[forper$Spe == "MI" & forper$For<10,]$Ind)
fhg = sum(forper[forper$Spe == "HG",]$wtf)/sum(forper[forper$Spe == "HG",]$Ind)

fp = summarySE(fgk, measurevar="For", groupvars=c("Spe"))
dfgh$Spe = c("Great Barbet", "Hill Myna", "Imperial Pigeon", "Hoolock Gibbon")
dfgh$Pro = dfgh$bcl = dfgh$bcr = c(gbpr,hmpr,mipr,hgpr)
dfgh$N = c(sum(forper[forper$Spe == "GB",]$Ind),sum(forper[forper$Spe == "HM",]$Ind),sum(forper[forper$Spe == "MI",]$Ind),sum(forper[forper$Spe == "HG",]$Ind))
dfgh = data.frame(cbind(1:4,0))
dfgh = dfgh[,-c(1,2)]
dfg$Spe = c("Great Barbet", "Hoolock Gibbon", "Hill Myna","Imperial Pigeon 1", "Imperial Pgn 2", "Great Barbet", "Hoolock Gibbon", "Hill Myna","Imperial Pigeon 1")
dfg$Time = c(fp$For, v1, v4, v2, v3)
dfg$Type = c("Foraging time","Foraging time","Foraging time","Foraging time","Foraging time","Visitation time","Visitation time","Visitation time","Visitation time")
dfg$mean = dfg$cil = dfg$cir = dfg$Time
dfg$mean[c(6,8,9)] = c(gbmean,mimean,hmmean)
dfg$cil[c(6,8,9)] = c(gbcl,micl,hmcl)
dfg$cir[c(6,8,9)] = c(gbcr,micr,hmcr)
dfgj = dfg[-5,]
dfgj$Spe[c(4,8)] = rep("Imperial Pigeon",2)
dfgj[1:4,]$cir = dfgj[1:4,]$mean + fp[1:4,]$ci
dfgj[1:4,]$cil = dfgj[1:4,]$mean - fp[1:4,]$ci

dfgh$bcl = dfgh$Pro - 1.96*sqrt(dfgh$Pro*(1-dfgh$Pro)/dfgh$N)
dfgh$bcr = dfgh$Pro + 1.96*sqrt(dfgh$Pro*(1-dfgh$Pro)/dfgh$N)
dfgh$Spe = c("Megalaima virens","Gracula religiosa","Ducula badia","Hoolock Gibbon")
pd = position_dodge(0.5)
ggpp = ggplot(dfgh[-4,], aes(Spe, Pro)) + 
  ##geom_errorbar(aes(ymin=Per-se, ymax=Per+se), width=.1) +
  geom_bar(width = 0.5, position = pd, col = "black", fill = "dark grey") +
  geom_errorbar(aes(ymin = bcl,ymax = bcr), width = 0.1, size = 0.75, position = pd) +
  xlab("Frugivore species") +
  ylab("Proportion of individuals") +
  ##opts(title = "Time Spent Foraging (Foraging Bout)") +
  theme_bw()
ggpp2 = ggpp+
  theme(axis.title.x = element_text(vjust = 0.1, size = 20), axis.text.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.justification=c(1,1), legend.position=c(1,0.97))+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 14))+
  scale_x_discrete(limits = c("Gracula religiosa","Megalaima virens","Ducula badia"))+
  theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )  

pd = position_dodge(0.5)
ggppx = ggplot(dfgj[-c(2,6),], aes(Spe, mean, fill = Type)) + 
  ##geom_errorbar(aes(ymin=Per-se, ymax=Per+se), width=.1) +
  geom_bar(width = 0.5, position = pd, col = "black", stat = "identity") +
  geom_errorbar(aes(ymin = cil,ymax = cir), width = 0.1, size = 0.75, position = pd) +
  xlab("Frugivore species") +
  ylab("Foraging and visitation time (min)") +
  ##opts(title = "Time Spent Foraging (Foraging Bout)") +
  theme_bw()
ggppx2 = ggppx+
  theme(axis.title.x = element_text(vjust = 0.1, size = 20), axis.text.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.justification=c(1,1), legend.position=c(1,0.97))+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 14))+
  scale_x_discrete(limits = c("Gracula religiosa","Megalaima virens","Ducula badia"))+
  theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )  
?geom_bar

ggsave(file = "C:/Ashwin/All Research and Teaching/Manuscript Images/Seed Size Images/Figure 1 new.tiff", dpi = 1200)
     
multiplot(ggpp1,ggpp2,cols = 2)
gbpr = sum(forper[forper$Spe == "GB" & forper$ret == "Y",]$Ind)/sum(forper[forper$Spe == "GB",]$Ind)
hmpr = sum(forper[forper$Spe == "HM" & forper$ret == "Y",]$Ind)/sum(forper[forper$Spe == "HM",]$Ind)
hgpr = sum(forper[forper$Spe == "HG" & forper$ret == "Y",]$Ind)/sum(forper[forper$Spe == "HG",]$Ind)
mipr = sum(forper[forper$Spe == "MI" & forper$ret == "Y",]$Ind)/sum(forper[forper$Spe == "MI",]$Ind)

sum(forper$Ind)
fgk = data.frame(cbind(1:83,0))
fgk = fgk[,-c(1,2)]
kl = 0
for (i in 1:35)
{
  for (j in 1:forper$Ind[i])
  {
    kl = kl + 1
    fgk$Spe[kl] = as.character(forper$Spe[i])
    fgk$For[kl] = forper$For[i]
    fgk$Per[kl] = forper$Per[i]
    fgk$ret[kl] = forper$ret[i]
  }
}
fgk$ret[40:55] = rep("Y",16)

for (i in 1:83)
{
  if (fgk$Spe[i] == "MI" & fgk$For[i] < 10)
    fgk$Spe[i] = "MI1"
}

gbfor = fgk$For[fgk$Spe == "GB"]
hmfor = fgk$For[fgk$Spe == "HM"]
mifor = fgk$For[fgk$Spe == "MI" | fgk$Spe == "MI1"]

gbper = nfgk$Per[nfgk$Spe == "GB"]
hmper = nfgk$Per[nfgk$Spe == "HM"]
miper = nfgk$Per[nfgk$Spe == "MI" | nfgk$Spe == "MI1"]

gbvis = de
hmvis = dr
mivis = dt

temp = dfgj
dfgj = temp

dfgj[7,4:6] = temp[8,4:6]
dfgj[8,4:6] = temp[7,4:6]

gbvis
gbfor

gbtest1 = rep(gbvis, each = length(gbfor))
gbtest2 = rep(gbfor, length(gbvis))

hmtest1 = rep(hmvis, each = length(hmfor))
hmtest2 = rep(hmfor, length(hmvis))

mitest1 = rep(mivis, each = length(mifor))
mitest2 = rep(mifor, length(mivis))

gbtest = gbtest1 - gbtest2
gbtest = gbtest[gbtest >= 0]

hmtest = hmtest1 - hmtest2
hmtest = hmtest[hmtest >= 0]

mitest = mitest1 - mitest2
mitest = mitest[mitest >= 0]

wilcox.test(mitest,gbtest)
wilcox.test(gbper,miper)

mean(miper[miper != 0])
length(miper[miper != 0])

mean(hmfor)
mean(mifor + miper)

nfgk = fgk
nfgk[nfgk$ret == "N",]$Per = 0
