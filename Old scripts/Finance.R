dj = read.csv("C:/Ashwin/dowjones.csv")
str(dj$Date)
length(dj$Time)
head(b)
hist(a$N)
head(final)

names(dj)

a = summarySE(djnew, groupvar = "Date", measurevar = "Close")
atest = a[1:1665,]
atest$Date = as.Date(atest$Date, "%m/%d/%Y")
atest1 = a[1666:2749,]

a  = rbind(atest,atest1)

djtest = dj[414136:length(dj$Time),] # from the row where the transition occurs - 1
djtest1 = dj[1:414135,] # till the transition row (r-1)
djtest$Date = as.Date(djtest$Date, "%m/%d/%Y")
djtest1$Date = as.Date(djtest1$Date, "%Y-%m-%d")
djnew = rbind(djtest1,djtest)
djnew = djnew[order(djnew$Date),]

a = summarySE(djnew, groupvar = "Date", measurevar = "Close")

n = 4
b = a[,1:2]
c = length(b$N)
b$N = floor(b$N/n)-1
final = data.frame(cbind(1:(c*n)),0)
final = final[,-c(1,2)]
final[,1:7] = 0
names(final) = names(djnew)
count = 1
for (i in 1:c) 
{
  print(i)
  within = 1
  for (j in 1:n)
  {
    final[count,] = djnew[djnew$Date == b$Date[i],][within,]
    within = within + b$N[i]
    count = count + 1
  }
} 
fin = final
final$Date = as.Date(final$Date)
final$Date = rep(a$Date, each = 4)

write.csv(final, "C:/Ashwin/fin4.csv")


#Extract the data at a given frequency (x times per day).

image.plot(data120)