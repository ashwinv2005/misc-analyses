a = read.csv("C:/Users/ashwinv/Desktop/Testfile.csv")
a$id = paste(a$site,a$location,a$group,a$plot,sep = "")
b = read.csv("C:/Users/ashwinv/Desktop/Testfile1.csv")
b$id = paste(b$site,b$location,b$group,b$plot,sep = "")

c = read.csv("C:/Users/ashwinv/Desktop/Seedsoverall.csv")
d = read.csv("C:/Users/ashwinv/Desktop/Fruitsoverall.csv")
e = read.csv("C:/Users/ashwinv/Desktop/Status by species October.csv")
f = read.csv("C:/Users/ashwinv/Desktop/Status by species June July.csv")

c$site = ""
d$site = ""
e$site = ""
f$site = ""

c$location = ""
d$location = ""
e$location = ""
f$location = ""

c$group = ""
d$group = ""
e$group = ""
f$group = ""

c$plot = 0
d$plot = 0
e$plot = 0
f$plot = 0

for (i in 1:length(a$site))
{
  e[e$id == a$id[i],]$site = as.character(a$site[i])
  e[e$id == a$id[i],]$location = as.character(a$location[i])
  e[e$id == a$id[i],]$group = as.character(a$group[i])
  e[e$id == a$id[i],]$plot = a$plot[i]
  
  f[f$id == a$id[i],]$site = as.character(a$site[i])
  f[f$id == a$id[i],]$location = as.character(a$location[i])
  f[f$id == a$id[i],]$group = as.character(a$group[i])
  f[f$id == a$id[i],]$plot = a$plot[i]
  
}

for (i in 1:length(b$site))
{
  c[c$id == b$id[i],]$site = as.character(b$site[i])
  c[c$id == b$id[i],]$location = as.character(b$location[i])
  c[c$id == b$id[i],]$group = as.character(b$group[i])
  c[c$id == b$id[i],]$plot = b$plot[i]
  
  d[d$id == b$id[i],]$site = as.character(b$site[i])
  d[d$id == b$id[i],]$location = as.character(b$location[i])
  d[d$id == b$id[i],]$group = as.character(b$group[i])
  d[d$id == b$id[i],]$plot = b$plot[i]
}


