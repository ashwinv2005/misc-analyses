names(arrdpday)
arrpday$Name
boxplot(arrival[arrival$Name == "FS1",]$Pys~arrival[arrival$Name == "FS1",]$Direction)
library(ggplot2)
arrpday = arrival[-c(1273:1306),]
for (i in 1:length(arrpday$Pys))
{
if(arrpday$Name[i] == "PN1")
{arrpday[i,7:21] = arrpday[i,7:21]/PN1[PN1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN2")
{arrpday[i,7:21] = arrpday[i,7:21]/PN2[PN2$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN4")
{arrpday[i,7:21] = arrpday[i,7:21]/PN4[PN4$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN5")
{arrpday[i,7:21] = arrpday[i,7:21]/PN5[PN5$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN6")
{arrpday[i,7:21] = arrpday[i,7:21]/PN6[PN6$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN7")
{arrpday[i,7:21] = arrpday[i,7:21]/PN7[PN7$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN8")
{arrpday[i,7:21] = arrpday[i,7:21]/PN8[PN8$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "PN9")
{arrpday[i,7:21] = arrpday[i,7:21]/PN9[PN9$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "FS1")
{arrpday[i,7:21] = arrpday[i,7:21]/FS1[FS1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "FS2")
{arrpday[i,7:21] = arrpday[i,7:21]/FS2[FS2$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "FS3")
{arrpday[i,7:21] = arrpday[i,7:21]/FS3[FS3$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "FS4")
{arrpday[i,7:21] = arrpday[i,7:21]/FS4[FS4$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "FS5")
{arrpday[i,7:21] = arrpday[i,7:21]/FS5[FS5$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "BEL1")
{arrpday[i,7:21] = arrpday[i,7:21]/BEL1[BEL1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "BEL2")
{arrpday[i,7:21] = arrpday[i,7:21]/BEL2[BEL2$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ECC1")
{arrpday[i,7:21] = arrpday[i,7:21]/ECC1[ECC1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ECC2")
{arrpday[i,7:21] = arrpday[i,7:21]/ECC2[ECC2$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ECC3")
{arrpday[i,7:21] = arrpday[i,7:21]/ECC3[ECC3$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "AIL1")
{arrpday[i,7:21] = arrpday[i,7:21]/AIL1[AIL1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "AIL2")
{arrpday[i,7:21] = arrpday[i,7:21]/AIL2[AIL2$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "AIL3")
{arrpday[i,7:21] = arrpday[i,7:21]/AIL3[AIL3$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "AIL4")
{arrpday[i,7:21] = arrpday[i,7:21]/AIL4[AIL4$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "AIL5")
{arrpday[i,7:21] = arrpday[i,7:21]/AIL5[AIL5$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ALT1")
{arrpday[i,7:21] = arrpday[i,7:21]/ALT1[ALT1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ES1")
{arrpday[i,7:21] = arrpday[i,7:21]/ES1[ES1$Date == arrpday[i,1],]$Days}
if(arrpday$Name[i] == "ES2")
{arrpday[i,7:21] = arrpday[i,7:21]/ES2[ES2$Date == arrpday[i,1],]$Days}
}

arrdpday = arrpday
for (i in 1:length(arrpday$Pys))
{
if(arrdpday$Name[i] == "PN1")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN1",]$s0}
if(arrdpday$Class[i] == 1)
{
if(arrdpday$Direction[i] == "N" | arrdpday$Direction[i] == "S")
{arrdpday[i,7:21] = arrdpday[i,7:21]/3}
else
{arrdpday[i,7:21] = arrdpday[i,7:21]/2}
}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN1",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN1",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN1",]$s4}
}

if(arrdpday$Name[i] == "PN2")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN2",]$s0}
if(arrdpday$Class[i] == 1)
{
if(arrdpday$Direction[i] == "E" | arrdpday$Direction[i] == "W")
{arrdpday[i,7:21] = arrdpday[i,7:21]/3}
else
{arrdpday[i,7:21] = arrdpday[i,7:21]/2}
}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN2",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN2",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN2",]$s4}
}

if(arrdpday$Name[i] == "PN4")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN4",]$s0}
if(arrdpday$Class[i] == 1)
{
if(arrdpday$Direction[i] == "N" | arrdpday$Direction[i] == "S")
{arrdpday[i,7:21] = arrdpday[i,7:21]/3}
else
{arrdpday[i,7:21] = arrdpday[i,7:21]/2}
}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN4",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN4",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN4",]$s4}
}

if(arrdpday$Name[i] == "PN5")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN5",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN5",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN5",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN5",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN5",]$s4}
}

if(arrdpday$Name[i] == "PN6")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN6",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "PN6",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "PN6",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "PN6",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "PN6",]$s4}
}

if(arrdpday$Name[i] == "PN7")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN7",]$s0}
if(arrdpday$Class[i] == 1)
{
if(arrdpday$Direction[i] == "E" | arrdpday$Direction[i] == "W")
{arrdpday[i,7:21] = arrdpday[i,7:21]/3}
else
{arrdpday[i,7:21] = arrdpday[i,7:21]/2}
}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN7",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN7",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN7",]$s4}
}

if(arrdpday$Name[i] == "PN8")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN8",]$s0}
if(arrdpday$Class[i] == 1)
{
if(arrdpday$Direction[i] == "N" | arrdpday$Direction[i] == "S")
{arrdpday[i,7:21] = arrdpday[i,7:21]/3}
else
{arrdpday[i,7:21] = arrdpday[i,7:21]/2}
}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN8",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN8",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN8",]$s4}
}

if(arrdpday$Name[i] == "PN9")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "PN9",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN9",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN9",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN9",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "PN9",]$s4}
}

if(arrdpday$Name[i] == "FS1")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "FS1",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS1",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS1",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS1",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS1",]$s4}
}

if(arrdpday$Name[i] == "FS2")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "FS2",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS2",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS2",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS2",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS2",]$s4}
}

if(arrdpday$Name[i] == "FS3")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "FS3",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 3*arrdpday[i,7:21]/sum[sum$Tree == "FS3",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 3*arrdpday[i,7:21]/sum[sum$Tree == "FS3",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 3*arrdpday[i,7:21]/sum[sum$Tree == "FS3",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 3*arrdpday[i,7:21]/sum[sum$Tree == "FS3",]$s4}
}

if(arrdpday$Name[i] == "FS4")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "FS4",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS4",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS4",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS4",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS4",]$s4}
}

if(arrdpday$Name[i] == "FS5")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "FS5",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS5",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS5",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS5",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "FS5",]$s4}
}

if(arrdpday$Name[i] == "BEL1")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "BEL1",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL1",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL1",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL1",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL1",]$s4}
}

if(arrdpday$Name[i] == "BEL2")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "BEL2",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL2",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL2",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL2",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "BEL2",]$s4}
}

if(arrdpday$Name[i] == "ECC1")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "ECC1",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "ECC1",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "ECC1",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "ECC1",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 2*arrdpday[i,7:21]/sum[sum$Tree == "ECC1",]$s4}
}

if(arrdpday$Name[i] == "ECC2")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "ECC2",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC2",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC2",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC2",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC2",]$s4}
}

if(arrdpday$Name[i] == "ECC3")
{
if (arrdpday$Class[i] == 0)
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == "ECC3",]$s0}
if(arrdpday$Class[i] == 1)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC3",]$s1}
if(arrdpday$Class[i] == 2)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC3",]$s2}
if(arrdpday$Class[i] == 3)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC3",]$s3}
if(arrdpday$Class[i] == 4)
{arrdpday[i,7:21] = 4*arrdpday[i,7:21]/sum[sum$Tree == "ECC3",]$s4}
}

if(arrdpday$Type[i] == "E")
{arrdpday[i,7:21] = arrdpday[i,7:21]/sum[sum$Tree == arrdpday$Name[i],]$s0}
}


for (i in 1:1272)
{arrdpday$otherseeds[i] = sum(arrdpday[i,9:21])
 arrdpday$otherbig[i] = arrdpday$otherseeds[i]-sum(arrdpday[i,c(12,21)])
 arrdpday$othecc[i] = arrdpday$otherseeds[i]-arrdpday$Bel[i]+arrdpday$Pys[i]
 arrdpday$othbel[i] = arrdpday$otherseeds[i]-arrdpday$Ecc[i]+arrdpday$Pys[i]
 arrdpday$allseeds[i] = arrdpday$otherseeds[i]+arrdpday$Pys[i]
   arrdpday$eccbel[i] = arrdpday$Ecc[i] + arrdpday$Bel[i]
 if (arrdpday$Name[i] == "ECC1" | arrdpday$Name[i] == "ECC2" | arrdpday$Name[i] == "ECC3")
   arrdpday$Typez[i] = "EC"
 else
   arrdpday$Typez[i] = as.character(arrdpday$Type[i])
 }
arrdpday$Typez = as.factor(arrdpday$Typez)

arrdpday[arrdpday$Name == "BEL1" | arrdpday$Name == "BEL2",]$otherseeds = arrdpday[arrdpday$Name == "BEL1" | arrdpday$Name == "BEL2",]$otherseeds - arrdpday[arrdpday$Name == "BEL1" | arrdpday$Name == "BEL2",]$Bel
arrdpday[arrdpday$Name == "ECC1" | arrdpday$Name == "ECC2" | arrdpday$Name == "ECC3",]$otherseeds = arrdpday[arrdpday$Name == "ECC1" | arrdpday$Name == "ECC2" | arrdpday$Name == "ECC3",]$otherseeds - arrdpday[arrdpday$Name == "ECC1" | arrdpday$Name == "ECC2" | arrdpday$Name == "ECC3",]$Ecc

names(sum)
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN1"]*pi*sum$Rad[sum$Tree == "PN1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN2"]*pi*sum$Rad[sum$Tree == "PN2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN4"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN4"]*pi*sum$Rad[sum$Tree == "PN4"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN5"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN5"]*pi*sum$Rad[sum$Tree == "PN5"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN6"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN6"]*pi*sum$Rad[sum$Tree == "PN6"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN7"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN7"]*pi*sum$Rad[sum$Tree == "PN7"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN8"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN8"]*pi*sum$Rad[sum$Tree == "PN8"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "PN9"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "PN9"]*pi*sum$Rad[sum$Tree == "PN9"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "FS1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "FS1"]*pi*sum$Rad[sum$Tree == "FS1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "FS2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "FS2"]*pi*sum$Rad[sum$Tree == "FS2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "FS3"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "FS3"]*pi*sum$Rad[sum$Tree == "FS3"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "FS4"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "FS4"]*pi*sum$Rad[sum$Tree == "FS4"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "FS5"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "FS5"]*pi*sum$Rad[sum$Tree == "FS5"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "BEL1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "BEL1"]*pi*sum$Rad[sum$Tree == "BEL1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "BEL2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "BEL2"]*pi*sum$Rad[sum$Tree == "BEL2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ECC1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ECC1"]*pi*sum$Rad[sum$Tree == "ECC1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ECC2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ECC2"]*pi*sum$Rad[sum$Tree == "ECC2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ECC3"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ECC3"]*pi*sum$Rad[sum$Tree == "ECC3"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "AIL1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "AIL1"]*pi*sum$Rad[sum$Tree == "AIL1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "AIL2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "AIL2"]*pi*sum$Rad[sum$Tree == "AIL2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "AIL3"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "AIL3"]*pi*sum$Rad[sum$Tree == "AIL3"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "AIL4"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "AIL4"]*pi*sum$Rad[sum$Tree == "AIL4"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "AIL5"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "AIL5"]*pi*sum$Rad[sum$Tree == "AIL5"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ES1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ES1"]*pi*sum$Rad[sum$Tree == "ES1"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ES2"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ES2"]*pi*sum$Rad[sum$Tree == "ES2"]^2
arrdpday$abs[arrdpday$Class == 0 & arrdpday$Name == "ALT1"] = arrdpday$Pys[arrdpday$Class == 0 & arrdpday$Name == "ALT1"]*pi*sum$Rad[sum$Tree == "ALT1"]^2

l = 25
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN1"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN1"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN2"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN2"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN4"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN4"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN5"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN5"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN6"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN6"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN7"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN7"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN8"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN8"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "PN9"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "PN9"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "FS1"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "FS1"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "FS2"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "FS2"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "FS3"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "FS3"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "FS4"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "FS4"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "FS5"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "FS5"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "BEL1"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "BEL1"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "BEL2"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "BEL2"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "ECC1"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "ECC1"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "ECC2"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "ECC2"]*pi*l
arrdpday$abs[arrdpday$Class != 0 & arrdpday$Name == "ECC3"] = arrdpday$Pys[arrdpday$Class != 0 & arrdpday$Name == "ECC3"]*pi*l

head(arrdpday$abs)
warnings()
a = summarySE(arrdpday, measurevar="abs", groupvars=c("Type","Class"))
b = summarySE(arrdpday, measurevar="othecc", groupvars=c("Name","Direction"))
c = summarySE(arrdpday, measurevar="Pys", groupvars=c("Type","Class"))
d = summarySE(arrdpday, measurevar="othbel", groupvars=c("Type","Class"))

cla = c("AIL1","AIL2","AIL3","AIL4","AIL5","ALT1","BEL1","BEL2","ECC1","ECC2","ECC3","ES1","ES2","FS1","FS2","FS3","FS4","FS5","PN1","PN2","PN4","PN5","PN6","PN7","PN8","PN9")
sec = c(0,1,2,3,4)
co = 0
for (i in 1:28){
for (j in 1:5){
co = co + 1
m = arrdpday[arrdpday$Name == cla[i] & arrdpday$Class == sec[j],]$Pys
resamples = lapply(1:1000, function(i)sample(m, replace = T))
r.mean = sapply(resamples,mean)
c$cil[co] = quantile(r.mean,0.025)
c$cir[co] = quantile(r.mean,0.975)
if (co == 1 | co == 2 | co == 3 | co == 4 | co == 5 | co == 6 | co == 32 | co == 33)
break
}
if (co == 98)
break
}

resamplesm6 = list(10000)

cla = c("C","D","E","F")
sec = c(0,1,2,3,4)
co = 0
for (i in 1:4){
for (j in 1:5){
co = co + 1
if(cla[i] == "C"){
m1 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN1",]$abs
m2 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN2",]$abs
m3 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN4",]$abs
m4 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN5",]$abs
m5 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN6",]$abs
m6 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN7",]$abs
m7 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN8",]$abs
m8 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "PN9",]$abs
resamplesm1 = lapply(1:10000, function(i)sample(m1, replace = T))
resamplesm2 = lapply(1:10000, function(i)sample(m2, replace = T))
resamplesm3 = lapply(1:10000, function(i)sample(m3, replace = T))
resamplesm4 = lapply(1:10000, function(i)sample(m4, replace = T))
resamplesm5 = lapply(1:10000, function(i)sample(m5, replace = T))
resamplesm6 = lapply(1:10000, function(i)sample(m6, replace = T))
resamplesm7 = lapply(1:10000, function(i)sample(m7, replace = T))
resamplesm8 = lapply(1:10000, function(i)sample(m8, replace = T))
r.meanm1 = sapply(resamplesm1,mean)
r.meanm2 = sapply(resamplesm2,mean)
r.meanm3 = sapply(resamplesm3,mean)
r.meanm4 = sapply(resamplesm4,mean)
r.meanm5 = sapply(resamplesm5,mean)
r.meanm6 = sapply(resamplesm6,mean)
r.meanm7 = sapply(resamplesm7,mean)
r.meanm8 = sapply(resamplesm8,mean)
for (k in 1:100000)
{
m = c(sample(r.meanm1,1),sample(r.meanm2,1),sample(r.meanm3,1),sample(r.meanm4,1),sample(r.meanm5,1),sample(r.meanm6,1),sample(r.meanm7,1),sample(r.meanm8,1))
resamples[k] = mean(m)
}
r.mean = sapply(resamples,mean)
a$cil[co] = quantile(r.mean,0.025)
a$cir[co] = quantile(r.mean,0.975)
a$meanp[co] = mean(r.mean)
}
if(cla[i] == "D"){
m9 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "BEL1",]$abs
m10 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "BEL2",]$abs
m11 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ECC1",]$abs
m12 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ECC2",]$abs
m13 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ECC3",]$abs
resamplesm9 = lapply(1:10000, function(i)sample(m9, replace = T))
resamplesm10 = lapply(1:10000, function(i)sample(m10, replace = T))
resamplesm11 = lapply(1:10000, function(i)sample(m11, replace = T))
resamplesm12 = lapply(1:10000, function(i)sample(m12, replace = T))
resamplesm13 = lapply(1:10000, function(i)sample(m13, replace = T))
r.meanm9 = sapply(resamplesm9,mean)
r.meanm10 = sapply(resamplesm10,mean)
r.meanm11 = sapply(resamplesm11,mean)
r.meanm12 = sapply(resamplesm12,mean)
r.meanm13 = sapply(resamplesm13,mean)
for (k in 1:100000)
{
  m = c(sample(r.meanm9,1),sample(r.meanm10,1),sample(r.meanm11,1),sample(r.meanm12,1),sample(r.meanm13,1))
  resamples[k] = mean(m)
}
r.mean = sapply(resamples,mean)
a$cil[co] = quantile(r.mean,0.025)
a$cir[co] = quantile(r.mean,0.975)
a$meanp[co] = mean(r.mean)
}
if(cla[i] == "E"){
m14 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "AIL1",]$abs
m15 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "AIL2",]$abs
m16 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "AIL3",]$abs
m17 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "AIL4",]$abs
m18 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "AIL5",]$abs
m19 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ES1",]$abs
m20 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ES2",]$abs
m21 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "ALT1",]$abs
resamplesm14 = lapply(1:10000, function(i)sample(m14, replace = T))
resamplesm15 = lapply(1:10000, function(i)sample(m15, replace = T))
resamplesm16 = lapply(1:10000, function(i)sample(m16, replace = T))
resamplesm17 = lapply(1:10000, function(i)sample(m17, replace = T))
resamplesm18 = lapply(1:10000, function(i)sample(m18, replace = T))
resamplesm19 = lapply(1:10000, function(i)sample(m19, replace = T))
resamplesm20 = lapply(1:10000, function(i)sample(m20, replace = T))
resamplesm21 = lapply(1:10000, function(i)sample(m21, replace = T))
r.meanm14 = sapply(resamplesm14,mean)
r.meanm15 = sapply(resamplesm15,mean)
r.meanm16 = sapply(resamplesm16,mean)
r.meanm17 = sapply(resamplesm17,mean)
r.meanm18 = sapply(resamplesm18,mean)
r.meanm19 = sapply(resamplesm19,mean)
r.meanm20 = sapply(resamplesm20,mean)
r.meanm21 = sapply(resamplesm21,mean)
for (k in 1:100000)
{
  m = c(sample(r.meanm14,1),sample(r.meanm15,1),sample(r.meanm16,1),sample(r.meanm17,1),sample(r.meanm18,1),sample(r.meanm19,1),sample(r.meanm20,1),sample(r.meanm21,1))
  resamples[k] = mean(m)
}
r.mean = sapply(resamples,mean)
a$cil[co] = quantile(r.mean,0.025)
a$cir[co] = quantile(r.mean,0.975)
a$meanp[co] = mean(r.mean)
}
if(cla[i] == "F"){
m22 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "FS1",]$abs
m23 = arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "FS2",]$abs
m24= arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "FS3",]$abs
m25= arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "FS4",]$abs
m26= arrdpday[arrdpday$Type == cla[i] & arrdpday$Class == sec[j] & arrdpday$Name == "FS5",]$abs
resamplesm22 = lapply(1:10000, function(i)sample(m22, replace = T))
resamplesm23 = lapply(1:10000, function(i)sample(m23, replace = T))
resamplesm24 = lapply(1:10000, function(i)sample(m24, replace = T))
resamplesm25 = lapply(1:10000, function(i)sample(m25, replace = T))
resamplesm26 = lapply(1:10000, function(i)sample(m26, replace = T))
r.meanm22 = sapply(resamplesm22,mean)
r.meanm23 = sapply(resamplesm23,mean)
r.meanm24 = sapply(resamplesm24,mean)
r.meanm25 = sapply(resamplesm25,mean)
r.meanm26 = sapply(resamplesm26,mean)
for (k in 1:100000)
{
  m = c(sample(r.meanm22,1),sample(r.meanm23,1),sample(r.meanm24,1),sample(r.meanm25,1),sample(r.meanm26,1))
  resamples[k] = mean(m)
}
r.mean = sapply(resamples,mean)
a$cil[co] = quantile(r.mean,0.025)
a$cir[co] = quantile(r.mean,0.975)
a$meanp[co] = mean(r.mean)
}
if (co == 11)
break
}
}

pd = position_dodge(.1)

e = a[a$Direction != "Below",]
ggplot(e[c(1:18),], aes(x=Direction, y=Pys, colour=Name)) + 
    geom_errorbar(aes(ymin=Pys-ci, ymax=Pys+ci), width=.1, position = pd) +
    geom_line(position=pd) +
    geom_point(position=pd) 

sidebysideplot = grid.arrange(p2,p3, main = "Seed Arrival of all Seeds (Except those produced by the tree in question) in Different Treatments")

theme_get()
kk
aa = a
cc = c
dd = d
d$Div = c(rep("a)",5), rep("b)",5), rep("c)",6)) 
d$Type1 = rep("x", 16)
d$Type1[11] = "y"
aa$Class = cc$Class = dd$Class = c("Canopy","0-4","4-8","8-12","12-16","Under Canopy","0-4","4-8","8-12","12-16","Under canopy","Under Canopy","0-4","4-8","8-12","12-16")
ddd = c[1:5,]
ddd[6:9,] = c[2:5,]
ddd$Type1 = c(rep("a)",5),rep("b)",4))
d$Div = c(rep("a",5), rep("b",5), rep("c",6))

ggp = ggplot(d[c(1:15),], aes(x=Class, y=meanp*100, linetype = Type1))  +
    facet_grid(Div ~ ., scale="free_y")+
    geom_errorbar(aes(ymin=cil*100, ymax=cir*100), width=.1, position=pd, size = 0.6) +
    geom_line(position=pd, size = 0.6) +
    geom_point(position=pd, size = 3) +
    xlab("Distance class (m)") +
    ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
    scale_colour_hue(name="Type", 
                   breaks=c("C","D","E","F"),
                     labels=c("P. zeylanica", "Fruiting non-figs", "Emergents", "Figs"),
                    l=40) +
    ##opts(title = expression(paste("Arrival of ", italic("Prunus zeylanica"), " In All Treatments"))) +
    ##annotate("text", label = "c)", x = 2.75, y = 0.027, l = 30) +
    theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 16))+
  ##opts(legend.justification=c(1,1), legend.position=c(0.96,0.97)) +
  scale_x_continuous(expand = c(0.023,0.023),limits = c(-0.2,4.2), breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16")) +
  ##scale_y_continuous(limits = c(0,0.027))+
  ##scale_linetype_manual(values = c(1,3,10,5),
  ##                    name="", 
  ##                    breaks=c("C","D","E","F"),
  ##                    labels=c(expression(paste(italic("  P. ceylanica"))), "  Fruiting non-figs", "  Emergents", "  Figs"))+
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()
     ##panel.border = theme_blank(),
     ##panel.background = theme_blank()
  )+
theme(legend.position = "none")

ggsave(file = "C:/Ashwin/Seed Dispersal/Heterogeneity Images/Figure 4.pdf", dpi = 2000)
  
    
ggp = ggplot(c[c(4:15),], aes(x=Class, y=meanp*100, group = Type, shape = Type))  +
  ##facet_grid(Div ~ ., scale="free_y")+
  geom_errorbar(aes(ymin=cil*100, ymax=cir*100), width=.05, position=pd, size = 0.6) +
  geom_line(position=pd, size = 0.6) +
  geom_point(position=pd, size = 5) +
  xlab("Distance class (m)") +
  ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  ##scale_linetype_manual(values = c(1,2,3,4), name="", 
                     ##breaks=c("C","D","E","F"),
                     ##labels=c("  Conspecifics", "  Heterospecifics: non-figs", "  Emergents", "  Heterospecifics: figs"))+
  scale_shape_manual(values = c(15,16,24,17), name="", 
                     breaks=c("C","D","E","F"),
                     labels=c("  Conspecifics", "  Heterospecifics: non-figs", "  Emergents", "  Heterospecifics: figs"))+
  ##opts(title = expression(paste("Arrival of ", italic("Prunus zeylanica"), " In All Treatments"))) +
  ##annotate("text", label = "c)", x = 2.75, y = 0.027, l = 30) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 12))+
  theme(legend.justification=c(1,1), legend.position=c(0.96,1)) +
  scale_x_continuous(expand = c(0.023,0.023),limits = c(-0.2,4.2), breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16")) +
  ##scale_y_continuous(limits = c(0,0.027))+
  theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
  ##panel.border = element_blank(),
  ##panel.background = element_blank()
  )
  
  theme(legend.position = "none") +
  

ddd$Type1 = c(rep("a",5),rep("b",4))
expression(paste(italic("  P. ceylanica")))

ggp = ggplot(ddd, aes(x=Class, y=meanp*100))  +
  facet_grid(Type1 ~ ., scale="free_y")+
  geom_errorbar(aes(ymin=cil*100, ymax=cir*100), width=.1, position=pd, size = 0.6) +
  geom_line(position=pd, size = 0.6) +
  geom_point(position=pd, size = 3) +
  xlab("Distance class (m)") +
  ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 16))+
  ##opts(legend.justification=c(1,1), legend.position=c(0.96,0.97)) +
  scale_x_continuous(expand = c(0.023,0.023),limits = c(-0.2,4.2), breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
    theme(legend.position = "none") 

    geom_line(aes(x = 0:4, y = dd(Class = 0:4, Crop = 50, 0.015984, 90.172371, 1.023932, 16.380358))) 
    geom_line(aes(x = 0:4, y = dd(Class = t, Crop = 30, 0.015984, 90.172371, 1.023932, 16.380358)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 50, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 100, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 150, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 200, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 300, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 500, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 700, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 1000, 0.0007077, -0.7587325)))+
    geom_line(aes(x = 0:4, y = y(Class = 0:4, Crop = 1200, 0.0007077, -0.7587325)))


l1 = y(Class = 0:4, Crop = 1200, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l2 = y(Class = 0:4, Crop = 20, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l3 = y(Class = 0:4, Crop = 30, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l4 = y(Class = 0:4, Crop = 150, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l5 = y(Class = 0:4, Crop = 200, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l6 = y(Class = 0:4, Crop = 800, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l7 = y(Class = 0:4, Crop = 100, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l8 = y(Class = 0:4, Crop = 1200, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l9 = y(Class = 0:4, Crop = 200, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l10 = y(Class = 0:4, Crop = 20, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l11 = y(Class = 0:4, Crop = 30, 0.0007077, -2.91981+0.43342*(0:4), 166.35517)
l = (l1+l2+l3+l4+l5+l6+l7+l8+l9+l10+l11)/11

PN$Corcrop
library(nlme)
library(mlmRev)
library(ggplot2)

d = nlme(Pys~exp(a-b*Class), data = arrdpday, fixed = a+b~1, random = a~1,
	start(a = 0, b = 0))
	
arrdpday[arrdpday$Name == "PN1",]$Crop = c(rep(900,17),rep(100,17),rep(1200,17),rep(200,17),rep(20,17))
arrdpday[arrdpday$Name == "PN2",]$Crop = c(rep(1300,17),rep(200,17),rep(40,17),rep(50,17),rep(20,17))
arrdpday[arrdpday$Name == "PN4",]$Crop = c(rep(800,17),rep(200,17),rep(50,17),rep(100,17),rep(20,17),rep(20,17))
arrdpday[arrdpday$Name == "PN5",]$Crop = c(rep(20,13),rep(20,13),rep(30,13),rep(40,13),rep(200,13),rep(150,13))
arrdpday[arrdpday$Name == "PN6",]$Crop = c(rep(20,9),rep(100,9),rep(50,9),rep(200,9),rep(250,9),rep(250,9))
arrdpday[arrdpday$Name == "PN7",]$Crop = c(rep(200,17),rep(100,17),rep(60,17),rep(20,17))
arrdpday[arrdpday$Name == "PN8",]$Crop = c(rep(30,17),rep(30,17),rep(700,17),rep(100,17),rep(300,17),rep(1100,17))
arrdpday[arrdpday$Name == "PN9",]$Crop = c(rep(500,17),rep(300,17),rep(250,17),rep(300,17),rep(200,17))

plot(log(Pys)~Class, data = arrdpday[arrdpday$Type == "C" & arrdpday$Pys != 0,])
a = lme(fixed = log(Pys) ~ Class + log(Crop),
data = arrdpday[arrdpday$Type == "C" & arrdpday$Pys != 0,],
random = ~ 1 | Name/Direction)
summary(a)

library(nlme)
library(scatterplot3d)
fit = nlme(Pys~a1*exp(a2), 
           fixed=list(a1~Crop,a2~Class), 
           random=a1~Name, groups=~Name,  
           start=c(-15,3,-1.3,1.3),
           data=arrdpday[arrdpday$Type == "C",])

summary(lm(s1~Corcrop,data = PN))
names(arrdpday)
grp = groupedData(Pys ~ Class*Crop | Name/Direction, data = arrdpday[arrdpday$Type == "C",]) 
grp1 = groupedData(Pys ~ Class*Crop | Name, data = PNO)

con = nlmeControl(msMaxIter = 500000)
m = nlme(model = Pys ~ (W1*(Crop+50.6*Class^2)*exp(W2*Class)) ,
control = con,
data = grp ,
fixed = list(W1~1, W2~1) ,
random = pdDiag(W1 ~ 1) ,
weights = varPower(form = ~fitted(.), fixed = 0.5) ,
start = c(1.1,-1.1) ,
method = "ML")
summary(m)

for (i in 1:1272)
{
if (arrdpday$Class[i] <= 2)
arrdpday$Gp[i] = "int"
else
arrdpday$Gp[i] = "sim"
}

s = seq(1.4,1.8,length = 1001)

y = function(Class,Crop,W1,W2){
x = W1*(Crop + 50*atan(Class))*exp(W2*Class)
return(x)}

m = nlme(model = Pys ~ y(Class,Crop,W1,W2) ,
control = con,
data = grp1 ,
fixed = list(W1~1, W2~1) ,
random = pdDiag(W1~1),
weights = varPower(form = ~fitted(.), fixed = 0.5) ,
start = c(0.1,-1) ,
method = "ML")

summary(m)
logLik(m)
summary.corStruct(m)
help(memory.size)
t=0
for (i in seq(1,3,length = 100))
{
  t = t+1
parnames = c("W2","W3","W4")
dd = deriv(expression((Crop + W3)/(W2*Class^1.6 + W4)),
  namevec=parnames, function.arg=c("Class","Crop",parnames))

nstart = c(W2 = 10 ,W3 = 10, W4 = 0.5)
nm = nlmer (Pys ~ dd(Class,Crop, W2, W3, W4) ~
(W4| Name), data = PNO, nAGQ = 5,
start = nstart , verbose = TRUE, method = "ML")


gh[t] = AIC(nm)
}
plot(1:66,gh[1:66])
length(gh)
gh[30]
cor(gg$Pys,fitted(nm))
coef(nm)
summary(nm)
df(nm)
qh = rnorm(10000,8.596684,3.97934)
hist(qh)

s[286]
min(n)
round(s[474],3)
0.00012*278/499 + 17.6767
qplot(s,n[1:111])
warnings()

0.007*(17.57*atan(4)+1400)/(17.57*atan(4)+100)

plot(nm,grid=F,
 main="Residuals vs Fitted, Mixed Effects Model (m)")
ranef(nm, level = 1, standard = TRUE)
fixef(nm)
plot(ACF(nm,maxLag=6),alpha=.05,main="ACF, Model m")
aov(nm)
intervals(nm)
sum(resid(nm))
fitted(nm)
rnorm(10000,1,2)
??rlognorm
nl = rnorm(10000,9.125673,4.2253)
np = rnorm(1,0.0007077,0.0008399569)
np = rnorm(10000,0.0007077,0.0007077)
for (i in 1:10000)
{
  if (np[i] < 0)
    np[i] = rlnorm(1,0,0.000001)
}
hist(nl)
ab = log(0.0007077)
bc = log(0.0008399569)
hist(rlnorm(10000,ab,0.73), xlim = c(0,0.004))


g = gnls(Pys ~ (W1*Crop*exp(W2*Class)) , data=arrdpday[arrdpday$Type == "C",],
 params= list(W1~1,W2~1), start = c(1.1,1.1))
summary(g1)
plot(g1)
g1 = gnls(Pys ~ (W1*Crop*exp(W2*Class)) , data=arrdpday[arrdpday$Type == "C",],
 params= list(W1~Name,W2~1), start = c(1.1,1.1,0,0,0,0,0,0,0))
AIC(m,g,g1)
library(scatterplot3d)

s = c()
for (i in seq(0,1200, length = 100)){
s = c(100,200,300,400,500,600,700,800,900,1000,1100,1200)
s = rep(s,5)
t = c(0,1,2,3,4)
t = rep(t,12)
t = rep(t,100)
newcols = colorRampPalette(c("grey90", "grey10"))
win.graph(width = 6, height = 6, pointsize = 10)
wireframe(dd(Class = t, Crop = s, 5880.68, 64.09, 1024.58) ~ t * s,
          trellis.par.set(list(axis.text=list(cex=1.3))),
          col.regions=newcols(100),
          scales = list(arrows = FALSE, x = list(lab = c("Canopy","0-4","4-8","8-12","12-16"))),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = -40, x = -80, y = 10),
          xlab = list("Distance Class (m)",rot = -10, cex = 1.9),
          ylab = list("Fruit Crop",rot = 33, cex = 1.9),
          zlab = list("Seed Arrival/ sq.m. / Day",rot = 90, cex = 1.9),
          at = c(seq(0,0.1,length = 51), seq(0.11,0.2,length = 10),0.4,0.6,0.8,1,1.2))
s3d = scatterplot3d(t,s, dd(Class = t, Crop = s, 5880.68, 64.09, 1024.58), highlight.3d = TRUE, xlim = c(0,4), col.axis = "blue", xlab = "Distance Class", ylab = "Crop", zlab = "Seed Arrival/Day", pch = 19)
text(s3d$xyz.convert(3, 800, 1.2),
     labels = expression(f(x) == frac(Crop + 5880.68 , 1024.58*phantom(".")*Class^1.6 + 64.09)))       
augPred(m)

g = arrdpday[arrdpday$Type == "C",]
gg = arrdpday[arrdpday$Type == "C",]
g = g[g$Pys < 2,]
g = g[g$Class != 2 | g$Pys < 0.5,]
warnings()
for (i in 1:215)
{
  if(PNO$Name[i] == "PN1")
  PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 635.9368)
  if(PNO$Name[i] == "PN2")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 1054.0220)
  if(PNO$Name[i] == "PN4")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 191.5903)
  if(PNO$Name[i] == "PN5")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 1318.1714)
  if(PNO$Name[i] == "PN6")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 807.4617)
  if(PNO$Name[i] == "PN7")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 827.9441)
  if(PNO$Name[i] == "PN8")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 1595.9907)
  if(PNO$Name[i] == "PN9")
    PNO$Pred[i] = dd(PNO$Class[i], PNO$Crop[i], 5880.60, 64.09, 1363.1826)
}

kk = summarySE(PNO, measurevar="Pys", groupvars=c("Class"))
names(kkf)[3] = "Pys"
kk = rbind(kk,kkf)
kk$Type = c(rep("E",5),rep("P",5))
kk = kk[,-c(4:6)]
kkfinal = kk

for (i in 0:4)
{
n1 = PNO[PNO$Class == i,]$Pys
n2 = PNO[PNO$Class == i,]$Pred
resamplesn1 = lapply(1:10000,function(i)sample(n1,replace = T))
resamplesn2 = lapply(1:10000,function(i)sample(n2,replace = T))
r.n1 = sapply(resamplesn1,mean)
r.n2 = sapply(resamplesn2,mean)
kk$mean[i+1] = mean(r.n1)
kk$mean[i+6] = mean(r.n2)
kk$cil[i+1] = quantile(r.n1,0.025)
kk$cil[i+6] = quantile(r.n2,0.025)
kk$cir[i+1] = quantile(r.n1,0.975)
kk$cir[i+6] = quantile(r.n2,0.975)
}

gg[gg$Name == "PN4",]

ggk = ggplot(g, aes(Class, Pys, col = Crop))+ 
  geom_point()+
  xlab("Distance Class (m)") +
  ylab("Seed Arrival / Sq.m. / Day") +
  ##opts(title = "Predicted Arrival for Different Fruit Crop Sizes")+
  geom_line(aes(y = dd(Class, Crop = 50, 5880.68, 64.09, 1024.58), col = 50))+
  scale_colour_continuous(breaks=c(50,200,500,800,1200))+
  theme_bw()
ggk+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.justification=c(1,1), legend.position=c(0.9,0.9)) +
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))+
  scale_x_continuous(breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16"))
  
mean(PN$Crop)
mean(arrdpday[arrdpday$Type == "F" & arrdpday$Class == 1,]$Pys)
k = 200; l = 7
0.0007977*(k+50.6*l^2)*exp(-1.1637940*l)

hist(PN$Corcrop)
sumcum = numeric(11)
cuml = data.frame(cbind(1:11,0))
cuml = cuml[,-c(1,2)]
cuml$Crop = c(20,20,30,150,200,800,100,1200,200,20,30)
cuml$ff = (0.013091*cuml$Crop + 0.129452)*pi*25
for (i in 0:10)
{
sumcum[i+1] = sum(7*dd(Class = i, Crop = cuml$Crop, 5880.68, 64.09, 1024.58))
}
sumcumt = numeric(11)
r = 5
sumcumt[1] = sumcum[1]*pi*r^2
for (i in 1:10)
{
  sumcumt[i+1] = sumcum[i+1]*8*pi*((r+4*(i-1))+2)
}
sumcumt = round(sumcumt)
sf = data.frame(cbind(1:11,0))
sf = sf[,-c(1,2)]
sf$xax = c(0,seq(4,40,length = 10))
sf$cs = cumsum(sumcumt)
ggh = ggplot(sf, aes(xax,cs))+
  xlab("Distance from Tree")+
  ylab("Cumulative Number of Seeds")+
  ##opts(title = "Cumulative Seed Arrival Away from Parent Tree")+
  geom_point()+
  geom_line()+
  theme_bw()
ggh+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))
  
qplot(c(0,seq(4,40,length = 10)),sumcumt)+theme_bw()
sum(cuml$Crop)
max(cumsum(sumcumt))
sum(cuml$ff)
sum(c(0:10)*sumcumt)/sum(sumcumt)

plot(seq(0,4,length = 50), exp(-4*seq(0,4,length = 50)))

hist(PN$Corcrop, breaks = seq(0,1400,length = 14))
length(PN[PN$Corcrop>=400,]$Corcrop)/length(PN$Corcrop)
ssg = ggplot(PNC,aes(sl, Crop, col = Name))+
  geom_line(size = 0.75)+
  geom_point(size = 1.5)+
  xlab("Week")+
  ylab("Fruit Crop Size")+
  scale_colour_manual(values = rep("black",8),
                   name="Name", 
                   breaks=c("PN1","PN2","PN4","PN5","PN6","PN7","PN8","PN9"),
                   labels=c("Pz1","Pz2","Pz3","Pz4","Pz5","Pz6","Pz7","Pz8")
                   ) +
  theme_bw()
ssg+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))+
  opts(legend.position = "none")
names(PN)
PN
PNC = data.frame(cbind(1:51,0))
PNC = PNC[,-c(1,2)]
PNC$Name = c(PN$Name,unique(PN$Name))
PNC$Crop = c(PN$Corcrop,c(0,0,0,200,150,0,70,10))
PNC$sl = c(PN$sl,c(6,6,7,7,7,5,7,6))

PN$sl = c(1:5,1:5,1:6,1:6,1:6,1:4,1:6,1:5)

out<-capture.output(summary(nm))
cat(out,file="out.xls",sep="\n",append=TRUE)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))   
print(po1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))         
print(po2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(po3, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(hum, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pre, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))

ipx = seq(1,10, length = 1001)
ipy = 1/ipx^2.2
ipz = exp(-(ipx-1))
ip = data.frame(cbind(1:2002,0))
ip = ip[,-c(1,2)]
ip$x = rep(ipx,2)
ip$y = c(ipy,ipz)
ip$z = c(rep("Inverse power",1001),rep("Exponential",1001))
ipe = ggplot(ip, aes(x,y, col = z))+
  geom_line() +
  xlab("Distance") +
  ylab("Seed Arrival") +
  scale_colour_hue(name="Curve type", 
                   breaks=c("Inverse power", "Exponential"),
                   labels=c("Inverse power", "Exponential"),
                   l=40) +
  theme_bw()
ipe+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 24, face = "bold"), axis.text.x = theme_text(size = 20), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 24), axis.text.y = theme_text(size = 20)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.title = theme_text(size = 24, face = "bold"), legend.text = theme_text(size = 20)) +
  scale_x_continuous(breaks = c(2,4,6,8,10), labels = c(8,16,24,32,40))+
  scale_colour_manual(values = c("black","red"),
                      name="Curve type", 
                      breaks=c("Inverse power", "Exponential"),
                      labels=c("Inverse power", "Exponential"))+
  opts(legend.justification=c(1,0), legend.position=c(0.99,0.696)) 
  
hetro = data.frame(cbind(1:12),0)
hetro = hetro[,-c(1,2)]
hetro[,1:3] = 0
names(hetro) = c("Type","Stat","Percent")
hetro$Type = rep(c("Conspecifics","Heterospecific non-figs","Figs","Heterospecifics","Conspecifics + heterospecifics","Others"),2)
hetro$Stat = c(rep("Percent seed arrival",6),rep("Percent area covered by tree type",6))
hetro$Percent = c(11.40021145,30.91079003,9.807900728,40.71869076,52.11890221,47.88109779,1.15395,13.48760833,4.12125,17.60885833,18.76280833,81.23719167)


pd = position_dodge(0.5)
ggpp = ggplot(hetro[c(1,7,4,10,5,11,6,12),], aes(Type, Percent, fill = Stat)) + 
  ##geom_errorbar(aes(ymin=Per-se, ymax=Per+se), width=.1) +
  geom_bar(width = 0.5, position = pd, col = "black") +
  ##geom_errorbar(aes(ymin = bcl,ymax = bcr), width = 0.1, size = 0.75, position = pd) +
  xlab("Tree Type") +
  ylab("Percentage") +
  ##opts(title = "Time Spent Foraging (Foraging Bout)") +
  theme_bw()
ggpp+
  theme(axis.title.x = element_text(vjust = 0.1, size = 20), axis.text.x = element_text(size = 14), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 13)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.justification=c(1,1), legend.position=c(0.4,0.97))+
  theme(legend.title = element_text(size = 0), legend.text = element_text(size = 14))+
  ##scale_x_discrete(limits = c("Figs","Non-figs","Non-figs 0-4","Non-figs 4-8","Cons","FTs","All","Others"))+
  scale_x_discrete(limits = c("Conspecifics","Heterospecifics","Conspecifics + heterospecifics","Others"),breaks = c("Conspecifics","Heterospecifics","Conspecifics + heterospecifics","Others"), labels = c("Conspecifics","Heterospecifics","Conspecifics and Heterospecifics","All other trees"))+
  theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )

