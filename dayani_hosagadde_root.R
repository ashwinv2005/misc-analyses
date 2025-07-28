library(tidyverse)
data = read.csv("hos.root.csv")

hos = data %>% distinct(cno,cid)

s1 = data %>% filter(tin == 1) %>% select(cno,cid,cum.wt)
names(s1)[3] = "wt1"
s2 = data %>% filter(tin == 2) %>% select(cno,cid,cum.wt)
names(s2)[3] = "wt2"
s3 = data %>% filter(tin == 3) %>% select(cno,cid,cum.wt)
names(s3)[3] = "wt3"
s4 = data %>% filter(tin == 4) %>% select(cno,cid,cum.wt)
names(s4)[3] = "wt4"

hos = left_join(hos,s1)
hos = left_join(hos,s2)
hos = left_join(hos,s3)
hos = left_join(hos,s4)

hos$m1wt3 = 0
hos$m1wt4 = 0
hos$m1wt12 = 0

hos$m2wt3 = 0
hos$m2wt4 = 0
hos$m2wt12 = 0

hos$m3wt3 = 0
hos$m3wt4 = 0
hos$m3wt12 = 0

hos$m4wt3 = 0
hos$m4wt4 = 0
hos$m4wt12 = 0

for (i in 1:length(hos$cno))
{
  y1 = hos$wt1[i]
  y2 = hos$wt2[i]
  
  a1 = (y2-y1)/0.3
  b1 = (1.3*y1-y2)/0.3
  
  a2 = (-y1*y2)/(y2-2*y1)
  b2 = 20*(y1-y2)/(y2-2*y1)
  
  b3 = log((y2/y1))/log(2)
  a3 = y1/(10^b3)
  
  z = (y2 - sqrt((y2^2)-4*y1*(y2-y1)))/(2*y1)
  b4 = -log(z)/10
  a4 = y1/(1-exp(-10*b4))
  
  hos$m1wt3[i] = a1*log10(30) + b1
  hos$m1wt4[i] = a1*log10(40) + b1
  hos$m1wt12[i] = a1*log10(120) + b1
  
  hos$m2wt3[i] = (a2*30)/(b2+30)
  hos$m2wt4[i] = (a2*40)/(b2+40)
  hos$m2wt12[i] = (a2*120)/(b2+120)
  
  hos$m3wt3[i] = a3*(30^b3)
  hos$m3wt4[i] = a3*(40^b3)
  hos$m3wt12[i] = a3*(120^b3)
  
  hos$m4wt3[i] = a4*(1-exp(-b4*30))
  hos$m4wt4[i] = a4*(1-exp(-b4*40))
  hos$m4wt12[i] = a4*(1-exp(-b4*120))
}

hos$m1wt3ssd = (hos$m1wt3-hos$wt3)^2
hos$m2wt3ssd = (hos$m2wt3-hos$wt3)^2
hos$m3wt3ssd = (hos$m3wt3-hos$wt3)^2
hos$m4wt3ssd = (hos$m4wt3-hos$wt3)^2

m1wt3ssd = sum(hos$m1wt3ssd)
m2wt3ssd = sum(hos$m2wt3ssd)
m3wt3ssd = sum(hos$m3wt3ssd)
m4wt3ssd = sum(hos$m4wt3ssd)

hos$m1wt4ssd = (hos$m1wt4-hos$wt4)^2
hos$m2wt4ssd = (hos$m2wt4-hos$wt4)^2
hos$m3wt4ssd = (hos$m3wt4-hos$wt4)^2
hos$m4wt4ssd = (hos$m4wt4-hos$wt4)^2

m1wt4ssd = sum(hos$m1wt4ssd)
m2wt4ssd = sum(hos$m2wt4ssd)
m3wt4ssd = sum(hos$m3wt4ssd)
m4wt4ssd = sum(hos$m4wt4ssd)
