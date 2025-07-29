a = data.frame(cbind(1:555,0))
head(a)
a = a[,-c(1,2)]
sites = c("S4","S5","S16","S2","S1","S11","S12","S14","S6","S8","S13","S3","S39","S9","S33","S7","S18","S19","S10","S15","S17")
locations = c(2,3,1,1,3,2,2,1,1,1,3,4,3,1,1,3,1,1,1,1,1)
ls = c("L1","L2","L3","L4")
gs = c("G1","G2","G3")
ps = c(1,2,5,6,7)

a$total = a$dead = a$new = a$plot = a$group = a$location = a$site  = a$date = 0

count = 0
for (i in 1:length(sites))
{
  for (j in 1:locations[i])
  {
    for (k in 1:length(gs))
    {
      for (l in 1:length(ps))
      {
        count = count + 1
        a$site[count] = sites[i]
        a$location[count] = ls[j]
        a$group[count] = gs[k]
        a$plot[count] = ps[l]
      }
    }
  }
}

write.csv(b,"C:/Users/ashwinv/Desktop/seeds.csv")

b = read.csv("C:/Users/ashwinv/Desktop/seeds.csv")
head(b)

b$site = as.character(b$site)
b$location = as.character(b$location)
b$group = as.character(b$group)

sss = rep("S",length(b$site))
b$site = paste0(sss,b$site)

lls = rep("L",length(b$location))
b$location = paste0(lls,b$location)

ggs = rep("G",length(b$group))
b$group = paste0(ggs,b$group)

