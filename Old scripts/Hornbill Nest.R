rain = read.csv("C:/Ashwin/Seed rain 3 years.csv")
inter = read.csv("C:/Ashwin/Dateday.csv")
recruit = read.csv("C:/Ashwin/Recruitment 2010.csv")
names(recruit)[c(1,3)] = c("Sl","Nest")
head(inter)
head(rain)
length(inter$Int)
seedspe = summarySE(rain, measurevar = "Density", groupvars = c("Species","Day"))
spetot = with(rain, tapply(Density, Species, mean))
names(spetot)
spesur = spetot[c(1,2,4,6,8,9)]
names(rain)[4] = "Sl"
rain$Density = 0
for (i in 1:157)
{
  rain[rain$Sl == inter$Sl[i] && rain$Season == inter$Season[i] && rain$Nest == inter$Nest[i],]$Density = rain[rain$Sl == inter$Sl[i] && rain$Season == inter$Season[i] && rain$Nest == inter$Nest[i],]$Total/inter$Int[i]
}
rain$Day = 0
rain[ && rain$Season == 1 && rain$Nest == "1WH01"]
rain[rain$Season == 1 & rain$Nest == "4WH03",]
inter = inter[-c(57),]
rain$Day
for (i in 1:156)
{
  print(i)
  rain[rain$Sl == inter$Sl[i] & rain$Season == inter$Season[i] & rain$Nest == inter$Nest[i],]$Day = inter$Day[i]
}
warnings()
rain$Density = rain$Density/2
rain[1001:1010,]$Density = rain[1001:1010,]$Density*2
rectot = with(recruit, tapply(Total, Species, sum))
rectot = rectot[-c(7,10,11,12,14)]
recsee = with(recruit, tapply(Seedling, Species, sum))
recsee = recsee[-c(7,10,11,12,14)]
recsap = with(recruit, tapply(Sapling, Species, sum))
recsap = recsap[-c(7,10,11,12,14)]
recpol = with(recruit, tapply(Pole, Species, sum))
recpol = recpol[-c(7,10,11,12,14)]
spetot = spetot[-c(7)]
names(spetot)
plot(rectot~spetot)
plot(recsee~spetot)
plot(recsap~spetot)
plot(recpol~spetot)
abline(lm(recpol~spetot))
summary(lm(recpol~spetot))
antpr = data.frame(cbind(1:9,0))
antpr = antpr[,-c(1,2)]
antpr$spe = names(spetot)
antpr$arr = spetot
antpr$see = recsee
antpr$sap = recsap
antpr$pol = recpol

ggs = ggplot(antpr, aes(arr,pol)) + 
  geom_point(size = 2.5) + 
  stat_smooth(method = "lm", se = F, col = "black") + 
  xlab("Seed arrival density (/Sq. m./Day)") +
  ylab("Number of Poles") +
  ##opts(title = "Fruit Diameter vs Fruit Length") +
  theme_bw() +
  ##annotate("text", label = c("y = 0.31196x + 11.96858"), x = 24, y = 24, l = 20)+
  annotate("text", label = "R sq. = 0.8105", x = 0.55, y = 1, l = 20)
ggs+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 18))+
  opts(panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
       ##panel.border = theme_blank(),
       ##panel.background = theme_blank()
  )





