arrival = read.csv("C:/Ashwin/Seed arrival.csv")
sum = read.csv("C:/Ashwin/Summary.csv")
prunus = arrival[arrival$Type == "C", ]
ficus = arrival[arrival$Type == "F", ]
large = arrival[arrival$Type == "D", ]
emer = arrival[arrival$Type == "E", ]

n = length(unique(prunus[prunus$Name == "PN1", ]$Date))
PN1 = data.frame(cbind(1:n, 0))
PN1 = PN1[, -c(1,2)]
PN1$Date = unique(prunus[prunus$Name == "PN1", ]$Date)
PN1$Crop = prunus[prunus$Name == "PN1" & prunus$Direction == "Below", ]$Crop
PN1$Fruits = prunus[prunus$Name == "PN1" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN1[i,j] = sum(prunus[prunus$Name == "PN1" & prunus$Class == j-4 & prunus$Date == PN1$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN1[i,j] = sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN1" & prunus$Class == j-9 & prunus$Date == PN1$Date[i], ]$Gob)
}}
names(PN1)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN1$Cropf[1] = sum[sum$Tree == "PN1", ]$Crop
PN1$Cropf[2:n] = PN1$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN2", ]$Date))
PN2 = data.frame(cbind(1:n, 0))
PN2 = PN2[, -c(1,2)]
PN2$Date = unique(prunus[prunus$Name == "PN2", ]$Date)
PN2$Crop = prunus[prunus$Name == "PN2" & prunus$Direction == "Below", ]$Crop
PN2$Fruits = prunus[prunus$Name == "PN2" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN2[i,j] = sum(prunus[prunus$Name == "PN2" & prunus$Class == j-4 & prunus$Date == PN2$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN2[i,j] = sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN2" & prunus$Class == j-9 & prunus$Date == PN2$Date[i], ]$Gob)
}}
names(PN2)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN2$Cropf[1] = sum[sum$Tree == "PN2", ]$Crop
PN2$Cropf[2:n] = PN2$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN4", ]$Date))
PN4 = data.frame(cbind(1:n, 0))
PN4 = PN4[, -c(1,2)]
PN4$Date = unique(prunus[prunus$Name == "PN4", ]$Date)
PN4$Crop = prunus[prunus$Name == "PN4" & prunus$Direction == "Below", ]$Crop
PN4$Fruits = prunus[prunus$Name == "PN4" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN4[i,j] = sum(prunus[prunus$Name == "PN4" & prunus$Class == j-4 & prunus$Date == PN4$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN4[i,j] = sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN4" & prunus$Class == j-9 & prunus$Date == PN4$Date[i], ]$Gob)
}}
names(PN4)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN4$Cropf[1] = sum[sum$Tree == "PN4", ]$Crop
PN4$Cropf[2:n] = PN4$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN5", ]$Date))
PN5 = data.frame(cbind(1:n, 0))
PN5 = PN5[, -c(1,2)]
PN5$Date = unique(prunus[prunus$Name == "PN5", ]$Date)
PN5$Crop = prunus[prunus$Name == "PN5" & prunus$Direction == "Below", ]$Crop
PN5$Fruits = prunus[prunus$Name == "PN5" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN5[i,j] = sum(prunus[prunus$Name == "PN5" & prunus$Class == j-4 & prunus$Date == PN5$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN5[i,j] = sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN5" & prunus$Class == j-9 & prunus$Date == PN5$Date[i], ]$Gob)
}}
names(PN5)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN5$Cropf[1] = sum[sum$Tree == "PN5", ]$Crop
PN5$Cropf[2:n] = PN5$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN6", ]$Date))
PN6 = data.frame(cbind(1:n, 0))
PN6 = PN6[, -c(1,2)]
PN6$Date = unique(prunus[prunus$Name == "PN6", ]$Date)
PN6$Crop = prunus[prunus$Name == "PN6" & prunus$Direction == "Below", ]$Crop
PN6$Fruits = prunus[prunus$Name == "PN6" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN6[i,j] = sum(prunus[prunus$Name == "PN6" & prunus$Class == j-4 & prunus$Date == PN6$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN6[i,j] = sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN6" & prunus$Class == j-9 & prunus$Date == PN6$Date[i], ]$Gob)
}}
names(PN6)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN6$Cropf[1] = sum[sum$Tree == "PN6", ]$Crop
PN6$Cropf[2:n] = PN6$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN7", ]$Date))
PN7 = data.frame(cbind(1:n, 0))
PN7 = PN7[, -c(1,2)]
PN7$Date = unique(prunus[prunus$Name == "PN7", ]$Date)
PN7$Crop = prunus[prunus$Name == "PN7" & prunus$Direction == "Below", ]$Crop
PN7$Fruits = prunus[prunus$Name == "PN7" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN7[i,j] = sum(prunus[prunus$Name == "PN7" & prunus$Class == j-4 & prunus$Date == PN7$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN7[i,j] = sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN7" & prunus$Class == j-9 & prunus$Date == PN7$Date[i], ]$Gob)
}}
names(PN7)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN7$Cropf[1] = sum[sum$Tree == "PN7", ]$Crop
PN7$Cropf[2:n] = PN7$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN8", ]$Date))
PN8 = data.frame(cbind(1:n, 0))
PN8 = PN8[, -c(1,2)]
PN8$Date = unique(prunus[prunus$Name == "PN8", ]$Date)
PN8$Crop = prunus[prunus$Name == "PN8" & prunus$Direction == "Below", ]$Crop
PN8$Fruits = prunus[prunus$Name == "PN8" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN8[i,j] = sum(prunus[prunus$Name == "PN8" & prunus$Class == j-4 & prunus$Date == PN8$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN8[i,j] = sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN8" & prunus$Class == j-9 & prunus$Date == PN8$Date[i], ]$Gob)
}}
names(PN8)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN8$Cropf[1] = sum[sum$Tree == "PN8", ]$Crop
PN8$Cropf[2:n] = PN8$Crop[1:n-1]

n = length(unique(prunus[prunus$Name == "PN9", ]$Date))
PN9 = data.frame(cbind(1:n, 0))
PN9 = PN9[, -c(1,2)]
PN9$Date = unique(prunus[prunus$Name == "PN9", ]$Date)
PN9$Crop = prunus[prunus$Name == "PN9" & prunus$Direction == "Below", ]$Crop
PN9$Fruits = prunus[prunus$Name == "PN9" & prunus$Direction == "Below", ]$Pyf
for (i in 1:n){
for (j in 4:8)
{
PN9[i,j] = sum(prunus[prunus$Name == "PN9" & prunus$Class == j-4 & prunus$Date == PN9$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 9:13)
{
PN9[i,j] = sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Bel) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Ecc) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Dyso) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$CS) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Pd) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Pha) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Pp) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Unid) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Poly) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Can) + 
sum(prunus[prunus$Name == "PN9" & prunus$Class == j-9 & prunus$Date == PN9$Date[i], ]$Gob)
}}
names(PN9)[4:13] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
PN9$Cropf[1] = sum[sum$Tree == "PN9", ]$Crop
PN9$Cropf[2:n] = PN9$Crop[1:n-1]

####################

n = length(unique(ficus[ficus$Name == "FS1", ]$Date))
FS1 = data.frame(cbind(1:n, 0))
FS1 = FS1[, -c(1,2)]
FS1$Date = unique(ficus[ficus$Name == "FS1", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS1[i,j] = sum(ficus[ficus$Name == "FS1" & ficus$Class == j-2 & ficus$Date == FS1$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS1[i,j] = sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS1" & ficus$Class == j-7 & ficus$Date == FS1$Date[i], ]$Gob)
}}
names(FS1)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

n = length(unique(ficus[ficus$Name == "FS2", ]$Date))
FS2 = data.frame(cbind(1:n, 0))
FS2 = FS2[, -c(1,2)]
FS2$Date = unique(ficus[ficus$Name == "FS2", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS2[i,j] = sum(ficus[ficus$Name == "FS2" & ficus$Class == j-2 & ficus$Date == FS2$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS2[i,j] = sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS2" & ficus$Class == j-7 & ficus$Date == FS2$Date[i], ]$Gob)
}}
names(FS2)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

n = length(unique(ficus[ficus$Name == "FS3", ]$Date))
FS3 = data.frame(cbind(1:n, 0))
FS3 = FS3[, -c(1,2)]
FS3$Date = unique(ficus[ficus$Name == "FS3", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS3[i,j] = sum(ficus[ficus$Name == "FS3" & ficus$Class == j-2 & ficus$Date == FS3$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS3[i,j] = sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS3" & ficus$Class == j-7 & ficus$Date == FS3$Date[i], ]$Gob)
}}
names(FS3)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

n = length(unique(ficus[ficus$Name == "FS4", ]$Date))
FS4 = data.frame(cbind(1:n, 0))
FS4 = FS4[, -c(1,2)]
FS4$Date = unique(ficus[ficus$Name == "FS4", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS4[i,j] = sum(ficus[ficus$Name == "FS4" & ficus$Class == j-2 & ficus$Date == FS4$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS4[i,j] = sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS4" & ficus$Class == j-7 & ficus$Date == FS4$Date[i], ]$Gob)
}}
names(FS4)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

n = length(unique(ficus[ficus$Name == "FS5", ]$Date))
FS5 = data.frame(cbind(1:n, 0))
FS5 = FS5[, -c(1,2)]
FS5$Date = unique(ficus[ficus$Name == "FS5", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS5[i,j] = sum(ficus[ficus$Name == "FS5" & ficus$Class == j-2 & ficus$Date == FS5$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS5[i,j] = sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS5" & ficus$Class == j-7 & ficus$Date == FS5$Date[i], ]$Gob)
}}
names(FS5)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")


n = length(unique(ficus[ficus$Name == "FS6", ]$Date))
FS6 = data.frame(cbind(1:n, 0))
FS6 = FS6[, -c(1,2)]
FS6$Date = unique(ficus[ficus$Name == "FS6", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS6[i,j] = sum(ficus[ficus$Name == "FS6" & ficus$Class == j-2 & ficus$Date == FS6$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS6[i,j] = sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS6" & ficus$Class == j-7 & ficus$Date == FS6$Date[i], ]$Gob)
}}
names(FS6)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

n = length(unique(ficus[ficus$Name == "FS7", ]$Date))
FS7 = data.frame(cbind(1:n, 0))
FS7 = FS7[, -c(1,2)]
FS7$Date = unique(ficus[ficus$Name == "FS7", ]$Date)
for (i in 1:n){
for (j in 2:6)
{
FS7[i,j] = sum(ficus[ficus$Name == "FS7" & ficus$Class == j-2 & ficus$Date == FS7$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 7:11)
{
FS7[i,j] = sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Bel) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Ecc) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Dyso) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$CS) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Pd) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Pha) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Pp) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Unid) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Poly) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Can) + 
sum(ficus[ficus$Name == "FS7" & ficus$Class == j-7 & ficus$Date == FS7$Date[i], ]$Gob)
}}
names(FS7)[2:11] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

##################

n = length(unique(large[large$Name == "BEL1", ]$Date))
BEL1 = data.frame(cbind(1:n, 0))
BEL1 = BEL1[, -c(1,2)]
BEL1$Date = unique(large[large$Name == "BEL1", ]$Date)
BEL1$Crop = large[large$Name == "BEL1" & large$Direction == "Below", ]$Crop
for (i in 1:n){
for (j in 3:7)
{
BEL1[i,j] = sum(large[large$Name == "BEL1" & large$Class == j-3 & large$Date == BEL1$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 8:12)
{
BEL1[i,j] = sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Ecc) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Dyso) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$CS) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Pd) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Pha) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Pp) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Unid) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Poly) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Can) + 
sum(large[large$Name == "BEL1" & large$Class == j-8 & large$Date == BEL1$Date[i], ]$Gob)
}}
names(BEL1)[3:12] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
BEL1$Cropf[1] = sum[sum$Tree == "BEL1", ]$Crop
BEL1$Cropf[2:n] = BEL1$Crop[1:n-1]

n = length(unique(large[large$Name == "BEL2", ]$Date))
BEL2 = data.frame(cbind(1:n, 0))
BEL2 = BEL2[, -c(1,2)]
BEL2$Date = unique(large[large$Name == "BEL2", ]$Date)
BEL2$Crop = large[large$Name == "BEL2" & large$Direction == "Below", ]$Crop
for (i in 1:n){
for (j in 3:7)
{
BEL2[i,j] = sum(large[large$Name == "BEL2" & large$Class == j-3 & large$Date == BEL2$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 8:12)
{
BEL2[i,j] = sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Ecc) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Dyso) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$CS) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Pd) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Pha) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Pp) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Unid) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Poly) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Can) + 
sum(large[large$Name == "BEL2" & large$Class == j-8 & large$Date == BEL2$Date[i], ]$Gob)
}}
names(BEL2)[3:12] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
BEL2$Cropf[1] = sum[sum$Tree == "BEL2", ]$Crop
BEL2$Cropf[2:n] = BEL2$Crop[1:n-1]

n = length(unique(large[large$Name == "ECC1", ]$Date))
ECC1 = data.frame(cbind(1:n, 0))
ECC1 = ECC1[, -c(1,2)]
ECC1$Date = unique(large[large$Name == "ECC1", ]$Date)
ECC1$Crop = large[large$Name == "ECC1" & large$Direction == "Below", ]$Crop
for (i in 1:n){
for (j in 3:7)
{
ECC1[i,j] = sum(large[large$Name == "ECC1" & large$Class == j-3 & large$Date == ECC1$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 8:12)
{
ECC1[i,j] = sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Bel) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Dyso) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$CS) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Pd) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Pha) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Pp) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Unid) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Poly) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Can) + 
sum(large[large$Name == "ECC1" & large$Class == j-8 & large$Date == ECC1$Date[i], ]$Gob)
}}
names(ECC1)[3:12] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
ECC1$Cropf[1] = sum[sum$Tree == "ECC1", ]$Crop
ECC1$Cropf[2:n] = ECC1$Crop[1:n-1]

n = length(unique(large[large$Name == "ECC2", ]$Date))
ECC2 = data.frame(cbind(1:n, 0))
ECC2 = ECC2[, -c(1,2)]
ECC2$Date = unique(large[large$Name == "ECC2", ]$Date)
ECC2$Crop = large[large$Name == "ECC2" & large$Direction == "Below", ]$Crop
for (i in 1:n){
for (j in 3:7)
{
ECC2[i,j] = sum(large[large$Name == "ECC2" & large$Class == j-3 & large$Date == ECC2$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 8:12)
{
ECC2[i,j] = sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Bel) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Dyso) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$CS) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Pd) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Pha) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Pp) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Unid) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Poly) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Can) + 
sum(large[large$Name == "ECC2" & large$Class == j-8 & large$Date == ECC2$Date[i], ]$Gob)
}}
names(ECC2)[3:12] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
ECC2$Cropf[1] = sum[sum$Tree == "ECC2", ]$Crop
ECC2$Cropf[2:n] = ECC2$Crop[1:n-1]

n = length(unique(large[large$Name == "ECC3", ]$Date))
ECC3 = data.frame(cbind(1:n, 0))
ECC3 = ECC3[, -c(1,2)]
ECC3$Date = unique(large[large$Name == "ECC3", ]$Date)
ECC3$Crop = large[large$Name == "ECC3" & large$Direction == "Below", ]$Crop
for (i in 1:n){
for (j in 3:7)
{
ECC3[i,j] = sum(large[large$Name == "ECC3" & large$Class == j-3 & large$Date == ECC3$Date[i], ]$Pys)
}}
for (i in 1:n){
for (j in 8:12)
{
ECC3[i,j] = sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Bel) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Dyso) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$CS) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Pd) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Pha) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Pp) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Unid) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Poly) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Can) + 
sum(large[large$Name == "ECC3" & large$Class == j-8 & large$Date == ECC3$Date[i], ]$Gob)
}}
names(ECC3)[3:12] = c("s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")
ECC3$Cropf[1] = sum[sum$Tree == "ECC3", ]$Crop
ECC3$Cropf[2:n] = ECC3$Crop[1:n-1]

#####################

n = length(unique(emer[emer$Name == "ES1", ]$Date))
ES1 = data.frame(cbind(1:n, 0))
ES1 = ES1[, -c(1,2)]
ES1$Date = unique(emer[emer$Name == "ES1", ]$Date)
ES1$s0 = emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Pys
ES1$o0 = emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "ES1" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "ES2", ]$Date))
ES2 = data.frame(cbind(1:n, 0))
ES2 = ES2[, -c(1,2)]
ES2$Date = unique(emer[emer$Name == "ES2", ]$Date)
ES2$s0 = emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Pys
ES2$o0 = emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "ES2" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "AIL1", ]$Date))
AIL1 = data.frame(cbind(1:n, 0))
AIL1 = AIL1[, -c(1,2)]
AIL1$Date = unique(emer[emer$Name == "AIL1", ]$Date)
AIL1$s0 = emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Pys
AIL1$o0 = emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "AIL1" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "AIL2", ]$Date))
AIL2 = data.frame(cbind(1:n, 0))
AIL2 = AIL2[, -c(1,2)]
AIL2$Date = unique(emer[emer$Name == "AIL2", ]$Date)
AIL2$s0 = emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Pys
AIL2$o0 = emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "AIL2" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "AIL3", ]$Date))
AIL3 = data.frame(cbind(1:n, 0))
AIL3 = AIL3[, -c(1,2)]
AIL3$Date = unique(emer[emer$Name == "AIL3", ]$Date)
AIL3$s0 = emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Pys
AIL3$o0 = emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "AIL3" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "AIL4", ]$Date))
AIL4 = data.frame(cbind(1:n, 0))
AIL4 = AIL4[, -c(1,2)]
AIL4$Date = unique(emer[emer$Name == "AIL4", ]$Date)
AIL4$s0 = emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Pys
AIL4$o0 = emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "AIL4" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "AIL5", ]$Date))
AIL5 = data.frame(cbind(1:n, 0))
AIL5 = AIL5[, -c(1,2)]
AIL5$Date = unique(emer[emer$Name == "AIL5", ]$Date)
AIL5$s0 = emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Pys
AIL5$o0 = emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "AIL5" & emer$Direction == "Below", ]$Can 

n = length(unique(emer[emer$Name == "ALT1", ]$Date))
ALT1 = data.frame(cbind(1:n, 0))
ALT1 = ALT1[, -c(1,2)]
ALT1$Date = unique(emer[emer$Name == "ALT1", ]$Date)
ALT1$s0 = emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Pys
ALT1$o0 = emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Bel +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Ecc +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Dyso +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$CS +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Pd +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Pha +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Pp +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Unid +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Gob +
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Poly + 
emer[emer$Name == "ALT1" & emer$Direction == "Below", ]$Can 

##########################

for (i in 4:8)
{
PN1[,c(i,i+5)] = PN1[,c(i,i+5)]/sum[sum$Tree == "PN1", i+2]
PN2[,c(i,i+5)] = PN2[,c(i,i+5)]/sum[sum$Tree == "PN2", i+2]
PN4[,c(i,i+5)] = PN4[,c(i,i+5)]/sum[sum$Tree == "PN4", i+2]
PN5[,c(i,i+5)] = PN5[,c(i,i+5)]/sum[sum$Tree == "PN5", i+2]
PN6[,c(i,i+5)] = PN6[,c(i,i+5)]/sum[sum$Tree == "PN6", i+2]
PN7[,c(i,i+5)] = PN7[,c(i,i+5)]/sum[sum$Tree == "PN7", i+2]
PN8[,c(i,i+5)] = PN8[,c(i,i+5)]/sum[sum$Tree == "PN8", i+2]
PN9[,c(i,i+5)] = PN9[,c(i,i+5)]/sum[sum$Tree == "PN9", i+2]

FS1[,c(i-2,i+3)] = FS1[,c(i-2,i+3)]/sum[sum$Tree == "FS1", i+2]
FS2[,c(i-2,i+3)] = FS2[,c(i-2,i+3)]/sum[sum$Tree == "FS2", i+2]
FS3[,c(i-2,i+3)] = FS3[,c(i-2,i+3)]/sum[sum$Tree == "FS3", i+2]
FS4[,c(i-2,i+3)] = FS4[,c(i-2,i+3)]/sum[sum$Tree == "FS4", i+2]
FS5[,c(i-2,i+3)] = FS5[,c(i-2,i+3)]/sum[sum$Tree == "FS5", i+2]
FS6[,c(i-2,i+3)] = FS6[,c(i-2,i+3)]/sum[sum$Tree == "FS6", i+2]
FS7[,c(i-2,i+3)] = FS7[,c(i-2,i+3)]/sum[sum$Tree == "FS7", i+2]

BEL1[,c(i-1,i+4)] = BEL1[,c(i-1,i+4)]/sum[sum$Tree == "BEL1", i+2]
BEL2[,c(i-1,i+4)] = BEL2[,c(i-1,i+4)]/sum[sum$Tree == "BEL2", i+2]
ECC1[,c(i-1,i+4)] = ECC1[,c(i-1,i+4)]/sum[sum$Tree == "ECC1", i+2]
ECC2[,c(i-1,i+4)] = ECC2[,c(i-1,i+4)]/sum[sum$Tree == "ECC2", i+2]
ECC3[,c(i-1,i+4)] = ECC3[,c(i-1,i+4)]/sum[sum$Tree == "ECC3", i+2]
}

PN1$Fruits = PN1$Fruits/sum[sum$Tree == "PN1",]$s0
PN2$Fruits = PN2$Fruits/sum[sum$Tree == "PN2",]$s0
PN4$Fruits = PN4$Fruits/sum[sum$Tree == "PN4",]$s0
PN5$Fruits = PN5$Fruits/sum[sum$Tree == "PN5",]$s0
PN6$Fruits = PN6$Fruits/sum[sum$Tree == "PN6",]$s0
PN7$Fruits = PN7$Fruits/sum[sum$Tree == "PN7",]$s0
PN8$Fruits = PN8$Fruits/sum[sum$Tree == "PN8",]$s0
PN9$Fruits = PN9$Fruits/sum[sum$Tree == "PN9",]$s0

ES1[,2:3] = ES1[,2:3]/sum[sum$Tree == "ES1", 6]
ES2[,2:3] = ES2[,2:3]/sum[sum$Tree == "ES2", 6]
AIL1[,2:3] = AIL1[,2:3]/sum[sum$Tree == "AIL1", 6]
AIL2[,2:3] = AIL2[,2:3]/sum[sum$Tree == "AIL2", 6]
AIL3[,2:3] = AIL3[,2:3]/sum[sum$Tree == "AIL3", 6]
AIL4[,2:3] = AIL4[,2:3]/sum[sum$Tree == "AIL4", 6]
AIL5[,2:3] = AIL5[,2:3]/sum[sum$Tree == "AIL5", 6]
ALT1[,2:3] = ALT1[,2:3]/sum[sum$Tree == "ALT1", 6]

########################

perday = data.frame(cbind(1:28, 0))
perday = perday[, -c(1,2)]
dens = data.frame(cbind(1:28, 0))
dens = dens[, -c(1,2)]

dens$Tree = c("PN1","PN2","PN4","PN5","PN6","PN7","PN8","PN9","FS1","FS2","FS3","FS4","FS5","FS6","FS7","BEL1","BEL2","ECC1","ECC2","ECC3","ES1","ES2","AIL1","AIL2","AIL3","AIL4","AIL5","ALT1")
for(i in 4:14)
{
dens[1,i-2] = sum(PN1[,i-1])
dens[2,i-2] = sum(PN2[,i-1])
dens[3,i-2] = sum(PN4[,i-1])
dens[4,i-2] = sum(PN5[,i-1])
dens[5,i-2] = sum(PN6[,i-1])
dens[6,i-2] = sum(PN7[,i-1])
dens[7,i-2] = sum(PN8[,i-1])
dens[8,i-2] = sum(PN9[,i-1])
}
for (i in 4:13)
{
dens[9,i-1] = sum(FS1[,i-2])
dens[10,i-1] = sum(FS2[,i-2])
dens[11,i-1] = sum(FS3[,i-2])
dens[12,i-1] = sum(FS4[,i-2])
dens[13,i-1] = sum(FS5[,i-2])
dens[14,i-1] = sum(FS6[,i-2])
dens[15,i-1] = sum(FS7[,i-2])
dens[16,i-1] = sum(BEL1[,i-1])
dens[17,i-1] = sum(BEL2[,i-1])
dens[18,i-1] = sum(ECC1[,i-1])
dens[19,i-1] = sum(ECC2[,i-1])
dens[20,i-1] = sum(ECC3[,i-1])
}

for (i in 3:4){
dens[21,i+4*(i-3)] = sum(ES1[,i-1])
dens[22,i+4*(i-3)] = sum(ES2[,i-1])
dens[23,i+4*(i-3)] = sum(AIL1[,i-1])
dens[24,i+4*(i-3)] = sum(AIL2[,i-1])
dens[25,i+4*(i-3)] = sum(AIL3[,i-1])
dens[26,i+4*(i-3)] = sum(AIL4[,i-1])
dens[27,i+4*(i-3)] = sum(AIL5[,i-1])
dens[28,i+4*(i-3)] = sum(ALT1[,i-1])
}

names(dens)[2:12] = c("Fruits","s0","s1","s2","s3","s4","o0","o1","o2","o3","o4")

########################



perday = dens
perday[1,2:12] = perday[1,2:12]/sum[sum$Tree == "PN1", 11]
perday[2,2:12] = perday[2,2:12]/sum[sum$Tree == "PN2", 11]
perday[3,2:12] = perday[3,2:12]/sum[sum$Tree == "PN4", 11]
perday[4,2:12] = perday[4,2:12]/sum[sum$Tree == "PN5", 11]
perday[5,2:12] = perday[5,2:12]/sum[sum$Tree == "PN6", 11]
perday[6,2:12] = perday[6,2:12]/sum[sum$Tree == "PN7", 11]
perday[7,2:12] = perday[7,2:12]/sum[sum$Tree == "PN8", 11]
perday[8,2:12] = perday[8,2:12]/sum[sum$Tree == "PN9", 11]
perday[9,3:12] = perday[9,3:12]/sum[sum$Tree == "FS1", 11]
perday[10,3:12] = perday[10,3:12]/sum[sum$Tree == "FS2", 11]
perday[11,3:12] = perday[11,3:12]/sum[sum$Tree == "FS3", 11]
perday[12,3:12] = perday[12,3:12]/sum[sum$Tree == "FS4", 11]
perday[13,3:12] = perday[13,3:12]/sum[sum$Tree == "FS5", 11]
perday[14,3:12] = perday[14,3:12]/sum[sum$Tree == "FS6", 11]
perday[15,3:12] = perday[15,3:12]/sum[sum$Tree == "FS7", 11]
perday[16,3:12] = perday[16,3:12]/sum[sum$Tree == "BEL1", 11]
perday[17,3:12] = perday[17,3:12]/sum[sum$Tree == "BEL2", 11]
perday[18,3:12] = perday[18,3:12]/sum[sum$Tree == "ECC1", 11]
perday[19,3:12] = perday[19,3:12]/sum[sum$Tree == "ECC2", 11]
perday[20,3:12] = perday[20,3:12]/sum[sum$Tree == "ECC3", 11]

perday[21,c(3,8)] = perday[21,c(3,8)]/sum[sum$Tree == "ES1", 11]
perday[22,c(3,8)] = perday[22,c(3,8)]/sum[sum$Tree == "ES2", 11]
perday[23,c(3,8)] = perday[23,c(3,8)]/sum[sum$Tree == "AIL1", 11]
perday[24,c(3,8)] = perday[24,c(3,8)]/sum[sum$Tree == "AIL2", 11]
perday[25,c(3,8)] = perday[25,c(3,8)]/sum[sum$Tree == "AIL3", 11]
perday[26,c(3,8)] = perday[26,c(3,8)]/sum[sum$Tree == "AIL4", 11]
perday[27,c(3,8)] = perday[27,c(3,8)]/sum[sum$Tree == "AIL5", 11]
perday[28,c(3,8)] = perday[28,c(3,8)]/sum[sum$Tree == "ALT1", 11]

pday = data.frame(cbind(1:10,0))
pday = pday[, -c(1,2)]
pday$sl = c(1:10)
day = t(perday)
day = day[-c(1,2),]
for(i in 2:29)
{
pday[,i] = day[, i-1]
}
names(pday) = c("sl", perday$Tree)
pday1 = pday[pday$sl == c(1:5),]
pday2 = pday[pday$sl == c(6:10),]

perday$Class = c(rep("C", 8), rep("F", 7), rep("D",5), rep("E", 8))
perday1 = perday[-c(14,15),]

PN = rbind(PN1,PN2,PN4,PN5,PN6,PN7,PN8,PN9)
FS = rbind(FS1,FS2,FS3,FS4,FS5)
LAR = rbind(BEL1,BEL2,ECC1,ECC2,ECC3)
PN$Crop = PN$Cropf

PNC = PN[PN$Crop > 600 | PN$Fruits < 7,]

plot(PN$s1~PN$Corcrop, main = "Prunus - Seed Arrival (0 to 4m) vs Crop", xlab = "Crop Size", ylab = "Seed Arrival (0 to 4m)")
a = lm(PN$s1~PN$Corcrop)
abline(a, col = "red")
legend(400, 0.2, bty="n", legend=paste("R sq. = 0.4807"), cex = 0.8)
legend(400, 0.17, bty="n", legend=paste("y = 0.0001298x + 0.01629"), cex = 0.8)
summary(a)

plot(PN[PN$s0 < 15,]$s0~PN[PN$s0 < 15,]$Cropf)
a = lm(PN[PN$s0 < 15,]$s0~PN[PN$s0 < 15,]$Cropf)
summary(a)
abline(a)

PN[(PN$Crop < 600 & PN$Fruits > 7) | (PN$Crop > 400 & PN$Fruits < 7),]
PN1$Corcrop = PN1$Cropf
PN2$Corcrop = PN2$Cropf
PN4$Corcrop = PN4$Cropf
PN5$Corcrop = PN5$Cropf
PN6$Corcrop = PN6$Cropf
PN7$Corcrop = PN7$Cropf
PN8$Corcrop = PN8$Cropf
PN9$Corcrop = PN9$Cropf

PN1$Corcrop[3] = round((PN1$Fruits[3] - 0.129)/1.3091)*100
PN8$Corcrop[3] = round((PN8$Fruits[3] - 0.129)/1.3091)*100
PN8$Corcrop[5] = round((PN8$Fruits[5] - 0.129)/1.3091)*100
PN9$Corcrop[1] = round((PN9$Fruits[1] - 0.129)/1.3091)*100

PN1$Days = c(9,9,7,7,7)
PN2$Days = c(8,9,7,7,7)
PN4$Days = c(6,6,7,7,7,7)
PN5$Days = c(7,6,7,7,7,7)
PN6$Days = c(9,4,7,9,5,7)
PN7$Days = c(6,7,7,6)
PN8$Days = c(7,4,7,9,5,7)
PN9$Days = c(7,5,7,7,7)

FS1$Days = c(8,9,7)
FS2$Days = c(3,7,7,6)
FS3$Days = c(7,7,6)
FS4$Days = c(7,7,7,7)
FS5$Days = c(6,7)

BEL1$Days = c(3,7,7,6)
BEL2$Days = c(7,5,7)
ECC1$Days = c(5,6,7,7,7)
ECC2$Days = c(5,6,7,7,7)
ECC3$Days = c(4,7,7,7)

ES1$Days = c(7,8,7,14)
ES2$Days = c(7,5,7,7,7)
AIL1$Days = c(7,5,7,7,7)
AIL2$Days = c(5,9,7,7)
AIL3$Days = c(5,6,7,7,7)
AIL4$Days = c(5,6,7,7,7)
AIL5$Days = c(5,6,7,7,7)
ALT1$Days = c(8,7,11)

PN1[,4:13] = PN1[,4:13]/PN1$Days
PN2[,4:13] = PN2[,4:13]/PN2$Days
PN4[,4:13] = PN4[,4:13]/PN4$Days
PN5[,4:13] = PN5[,4:13]/PN5$Days
PN6[,4:13] = PN6[,4:13]/PN6$Days
PN7[,4:13] = PN7[,4:13]/PN7$Days
PN8[,4:13] = PN8[,4:13]/PN8$Days
PN9[,4:13] = PN9[,4:13]/PN9$Days

FS1[,2:11] = FS1[,2:11]/FS1$Days
FS2[,2:11] = FS2[,2:11]/FS2$Days
FS3[,2:11] = FS3[,2:11]/FS3$Days
FS4[,2:11] = FS4[,2:11]/FS4$Days
FS5[,2:11] = FS5[,2:11]/FS5$Days

BEL1[,3:12] = BEL1[,3:12]/BEL1$Days
BEL2[,3:12] = BEL2[,3:12]/BEL2$Days
ECC1[,3:12] = ECC1[,3:12]/ECC1$Days
ECC2[,3:12] = ECC2[,3:12]/ECC2$Days
ECC3[,3:12] = ECC3[,3:12]/ECC3$Days

ES1[,2:3] = ES1[,2:3]/ES1$Days
ES2[,2:3] = ES2[,2:3]/ES2$Days
AIL1[,2:3] = AIL1[,2:3]/AIL1$Days
AIL2[,2:3] = AIL2[,2:3]/AIL2$Days
AIL3[,2:3] = AIL3[,2:3]/AIL3$Days
AIL4[,2:3] = AIL4[,2:3]/AIL4$Days
AIL5[,2:3] = AIL5[,2:3]/AIL5$Days
ALT1[,2:3] = ALT1[,2:3]/ALT1$Days

par(mfrow = c(2,1))
plot(PNC$s0~PNC$Crop)
plot(PN$s1~PN$s0, main = "Arrival in 4m Section vs Under Tree", xlab = "Arrival Under Tree", ylab = "Arrival in 4m Section")
a = lm(PN$s1~PN$s0)
abline(a, col = "red")
legend(15, 1, bty="n", legend=paste("R sq. = 0.45"), cex = 0.8)
legend(15, 0.9, bty="n", legend=paste("y = 0.06073x + 0.1243"), cex = 0.8)

##########################

plot(perday1[perday1$Class == "C",]$s1 ~ perday1[perday1$Class == "C",]$s0, main = "Arrival in 4m Section vs Under Tree (Entire Period)", xlab = "Arrival Under Tree", ylab = "Arrival in 4m Section")
a = lm(perday1[perday1$Class == "C",]$s1 ~ perday1[perday1$Class == "C",]$s0)
abline(a, col = "red")
legend(0.6, 0.04, bty="n", legend=paste("R sq. = 0.6029"), cex = 0.8)
legend(0.6, 0.03, bty="n", legend=paste("y = 0.06312x + 0.01578"), cex = 0.8)
summary(a)

plot(LAR$s0~LAR$Cropf)
###############################

boxplot(pday1$FS1,pday1$FS2,pday1$FS3,pday1$FS4,pday1$FS5~pday1$sl)

plot(1:2,pday1$PN1[4:5], type = "b", col = "red", xlim = c(0,67), ylim = c(0,0.032), xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Treatments")
lines(3:4,pday1$PN2[4:5], type = "b", col = "red")
lines(5:6,pday1$PN4[4:5], type = "b", col = "red")
lines(7:8,pday1$PN5[4:5], type = "b", col = "red")
lines(9:10,pday1$PN6[4:5], type = "b", col = "red")
lines(11:12,pday1$PN7[4:5], type = "b", col = "red")
lines(13:14,pday1$PN8[4:5], type = "b", col = "red")
lines(15:16,pday1$PN9[4:5], type = "b", col = "red")

lines(17:21,pday1$FS1, type = "b", col = "green")
lines(22:26,pday1$FS2, type = "b", col = "green")
lines(27:31,pday1$FS3, type = "b", col = "green")
lines(32:36,pday1$FS4, type = "b", col = "green")
lines(37:41,pday1$FS5, type = "b", col = "green")

lines(42:46,pday1$BEL1, type = "b", col = "blue")
lines(47:51,pday1$BEL2, type = "b", col = "blue")
lines(52:56,pday1$ECC1, type = "b", col = "blue")
lines(57:61,pday1$ECC2, type = "b", col = "blue")
lines(62:66,pday1$ECC3, type = "b", col = "blue")

legend(2, 0.028, bty = "n", c("Prunus - Last 2 sections", "Ficus", "Large fruits"), col = c("red", "green", "blue"), pch = 19, cex = 0.8)

################################

plot(1:5,pday1$PN1, type = "b", col = "red", xlim = c(0,41), ylim = c(0,1.2), xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Conspecifics")
lines(6:10,pday1$PN2, type = "b", col = "red")
lines(11:15,pday1$PN4, type = "b", col = "red")
lines(16:20,pday1$PN5, type = "b", col = "red")
lines(21:25,pday1$PN6, type = "b", col = "red")
lines(26:30,pday1$PN7, type = "b", col = "red")
lines(31:35,pday1$PN8, type = "b", col = "red")
lines(36:40,pday1$PN9, type = "b", col = "red")

plot(1:4,pday1$PN1[2:5], type = "b", col = "red", xlim = c(0,33), ylim = c(0,0.1), xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Conspecifics - Away from Tree")
lines(5:8,pday1$PN2[2:5], type = "b", col = "red")
lines(9:12,pday1$PN4[2:5], type = "b", col = "red")
lines(13:16,pday1$PN5[2:5], type = "b", col = "red")
lines(17:20,pday1$PN6[2:5], type = "b", col = "red")
lines(21:24,pday1$PN7[2:5], type = "b", col = "red")
lines(25:28,pday1$PN8[2:5], type = "b", col = "red")
lines(29:32,pday1$PN9[2:5], type = "b", col = "red")

###########################

plot(1:4,pday1$FS1[1:4], type = "b", col = "green", xlim = c(0,21), ylim = c(0,0.017), xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Figs")
lines(5:8,pday1$FS2[1:4], type = "b", col = "green")
lines(9:12,pday1$FS3[1:4], type = "b", col = "green")
lines(13:16,pday1$FS4[1:4], type = "b", col = "green")
lines(17:20,pday1$FS5[1:4], type = "b", col = "green")

#########################

plot(1:5,pday1$BEL1, type = "b", col = "blue", xlim = c(0,26), ylim = c(0,0.033), xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Large Fruits")
lines(6:10,pday1$BEL2, type = "b", col = "blue")
lines(11:15,pday1$ECC1, type = "b", col = "blue")
lines(16:20,pday1$ECC2, type = "b", col = "blue")
lines(21:25,pday1$ECC3, type = "b", col = "blue")

legend(40, 150, bty="n", legend=paste("R3 = ", format(r4, digits=4)), cex = 0.8)

####################

boxplot(perday1[perday1$Class == "C",]$s3, at = 1, xlim = c(0,14), ylim = c(0,0.02), xaxt = "n") 
boxplot(perday1[perday1$Class == "C",]$s4, at = 2, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$s0, at = 3, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$s1, at = 4, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$s2, at = 5, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$s3, at = 6, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F" & perday1$s4 < 0.02,]$s4, at = 7, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D" & perday1$s0 < 0.02,]$s0, at = 8, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D" & perday1$s1 < 0.02,]$s1, at = 9, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D" & perday1$s2 < 0.02,]$s2, at = 10, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$s3, at = 11, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$s4, at = 12, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "E",]$s0, at = 13, xaxt = "n", add = TRUE)

boxplot(perday1[perday1$Class == "C",]$s3, at = 1, xlim = c(0,14), col = "red", ylim = c(0,0.02), xaxt = "n", xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison Across Treatments") 
boxplot(perday1[perday1$Class == "C",]$s4, at = 2, xaxt = "n", add = TRUE, col = "red")
boxplot(perday1[perday1$Class == "F",]$s0, at = 3, xaxt = "n", add = TRUE, col = "green")
boxplot(perday1[perday1$Class == "F",]$s1, at = 4, xaxt = "n", add = TRUE, col = "green")
boxplot(perday1[perday1$Class == "F",]$s2, at = 5, xaxt = "n", add = TRUE, col = "green")
boxplot(perday1[perday1$Class == "F",]$s3, at = 6, xaxt = "n", add = TRUE, col = "green")
boxplot(perday1[perday1$Class == "F",]$s4, at = 7, xaxt = "n", add = TRUE, col = "green")
boxplot(perday1[perday1$Class == "D",]$s0, at = 8, xaxt = "n", add = TRUE, col = "blue")
boxplot(perday1[perday1$Class == "D",]$s1, at = 9, xaxt = "n", add = TRUE, col = "blue")
boxplot(perday1[perday1$Class == "D",]$s2, at = 10, xaxt = "n", add = TRUE, col = "blue")
boxplot(perday1[perday1$Class == "D",]$s3, at = 11, xaxt = "n", add = TRUE, col = "blue")
boxplot(perday1[perday1$Class == "D",]$s4, at = 12, xaxt = "n", add = TRUE, col = "blue")
boxplot(perday1[perday1$Class == "E",]$s0, at = 13, xaxt = "n", add = TRUE, col = "yellow")
legend(0, 0.021, bty = "n", c("Prunus - Last 2 sections", "Ficus", "Large fruits", "Emergents"), col = c("red", "green", "blue", "yellow"), pch = 19, cex = 0.8)
axis(1, at=seq(1, 13, by=1), labels = c("12m","16m","Below","4m","8m","12m","16m","Below","4m","8m","12m","16m","Below"), srt = 45, cex = 0.8)
text(x = seq(1, 13, by=1), par("usr")[3] - 0.2, labels = c("12m","16m","Below","4m","8m","12m","16m","Below","4m","8m","12m","16m","Below"), srt = 45, pos = 2, xpd = TRUE, cex = 0.8)

boxplot(perday1[perday1$Class == "C",]$s0, at = 1, xlim = c(0,6), col = "red", ylim = c(0,1.2), xaxt = "n", xlab = "Tree", ylab = "Seed Arrival/Sq.m/day", main = "Comparison for Conspecifics") 
boxplot(perday1[perday1$Class == "C",]$s1, at = 2, xaxt = "n", add = TRUE, col = "red")
boxplot(perday1[perday1$Class == "C",]$s2, at = 3, xaxt = "n", add = TRUE, col = "red")
boxplot(perday1[perday1$Class == "C",]$s3, at = 4, xaxt = "n", add = TRUE, col = "red")
boxplot(perday1[perday1$Class == "C",]$s4, at = 5, xaxt = "n", add = TRUE, col = "red")
axis(1, at=seq(1, 5, by=1), labels = FALSE)
text(seq(1, 5, by=1), par("usr")[3] - 0.05, labels = c("Below","4m","8m","12m","16m"), srt = 45, pos = 1, xpd = TRUE)

boxplot(perday1[perday1$Class == "C",]$o0, at = 1, xlim = c(0,17), ylim = c(0,0.02), xaxt = "n") 
boxplot(perday1[perday1$Class == "C",]$o1, at = 2, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "C",]$o2, at = 3, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "C",]$o3, at = 4, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "C",]$o4, at = 5, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$o0, at = 6, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$o1, at = 7, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$o2, at = 8, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$o3, at = 9, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "F",]$o4, at = 10, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$o0, at = 11, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$o1, at = 12, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$o2, at = 13, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$o3, at = 14, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "D",]$o4, at = 15, xaxt = "n", add = TRUE)
boxplot(perday1[perday1$Class == "E",]$o0, at = 16, xaxt = "n", add = TRUE)

summary(aov(perday[perday$Class == "D",]$s0~perday[perday$Class == "D",]$s3))

#################
par(mfrow = c(1,2))

plot(PN1$Corcrop, type = "b", col = 1, ylim = c(0,1300), xlim = c(1,6), xlab = "Prunus", ylab = "Crop Size", main = "Crop")
lines(PN2$Corcrop, type = "b", col = 2)
lines(PN4$Corcrop, type = "b", col = 3)
lines(PN5$Corcrop, type = "b", col = 4)
lines(PN6$Corcrop, type = "b", col = 5)
lines(PN7$Corcrop, type = "b", col = 6)
lines(PN8$Corcrop, type = "b", col = 7)
lines(PN9$Corcrop, type = "b", col = 8)

legend(4, 1200, bty = "n", c("PN1","PN2","PN4","PN5","PN6","PN7","PN8","PN9"), col = c(1:8), pch = 19, cex = 0.8)

plot(PN1$s0, type = "b", col = 1, ylim = c(0,5), xlim = c(1,45), xlab = "Prunus", ylab = "Seed Arrival/Sq.m/Day", main = "Arrival")
lines(PN2$s0, type = "b", col = 2)
lines(PN4$s0, type = "b", col = 3)
lines(PN5$s0, type = "b", col = 4)
lines(PN6$s0, type = "b", col = 5)
lines(PN7$s0, type = "b", col = 6)
lines(PN8$s0, type = "b", col = 7)
lines(PN9$s0, type = "b", col = 8)

legend(4.5, 4.7, bty = "n", c("PN1","PN2","PN4","PN5","PN6","PN7","PN8","PN9"), col = c(1:8), pch = 19, cex = 0.8)

plot(PN$Corcrop, type = "b", ylab = "Crop/Arrival in fifth section", main = "Crop Size and Seed Arrival (Fifth Section)")
plot(PN$s4, type = "b", col = "red")
lines(PN$Fruits*200, type = "b", col = "green")
legend(15, 1100, bty = "n", c("Crop Size","Seed Arrival Density/Day * 6000"), col = c(1,"red"), pch = 19, cex = 0.8)

boxplot(arrival[arrival$Name == "ECC3" & arrival$Class == 1,]$Pys~arrival[arrival$Name == "ECC3" & arrival$Class == 1,]$Direction)

ns = data.frame(cbind(1:86,0))
ns = ns[,-c(2,1)]
ns$Weeks = c(rep(1:43,2))
ns$Name = rep(PN$Name,2)
ns$y = c(PN$Corcrop, PN$s0)

f = numeric(5)
f[5] = summary(lm(PN$s4~PN$Corcrop))$r.squared
grid = seq(0,4,length = 101)
tanin = function(grid) {
  
    predicted = grid
    h = 0.5+atan(grid)/3.7
  return(h)
}
qplot(0:4,1-f, geom = "smooth", xlab = "Distance Class", ylab = "Variation in Seed Arrival Unexplained by Fruit Crop", main = "Unexplained Variation by Crop vs Distance")+
  
    geom_line(aes(x = grid, y = tanin(grid)), colour = "red")


library(gridExtra)
p1 = qplot(1:43,Corcrop, geom = "line", data = PN, colour = Name, xlab = "Weeks", ylab = "Fruit Crop Size", main = "Fruit Crop Size per Tree per Week")
p2 = qplot(1:43,s4,geom = "line",data = PN, colour = Name, xlab = "Weeks", ylab = "Seed Arrival Density/Day", main = "Seed Arrival per Tree per Week") 
sidebysideplot = grid.arrange(p1,p2, main = "From 12m to 16m Away")
axis(4) 
mtext("Seed Arrival Under Tree",4) 


