a = read.csv("C:/Users/ashwinv/Desktop/MyEBirdData.csv")
head(a)

a$Date = as.character(a$Date)
a$month = 0
a$day = 0
a$year = 0

b = strsplit(a$Date, '-')

for(i in 1:length(a$Date))
{
  x = unlist(b[[i]])
  a$month[i] = as.numeric(x[1])
  a$day[i] = as.numeric(x[2])
  a$year[i] = as.numeric(x[3])
}

head(a)

a1 = a[(a$month == "12" & a$year == "2017" & a$day >= 27) | (a$month == "1" & a$year == "2018" & a$day <= 6),]

head(a1)

c = unique(a1$Common.Name)

c = c[-grep("sp.",c, invert = F)]
c = c[-grep("/",c, invert = F)]
