density = read.csv("C:/Ashwin/Ashwin/Density_effect.csv")
distance = read.csv("C:/Ashwin/Ashwin/Distance_effect.csv")
names(density) = c("Tree","Size","Type","Plot","Treat","Rep","Ori","Ali","Pre","Rem","Cach","Germ","Dead")
names(distance) = c("Sp","Tree","Size","Type","Treat","Rep","Ori","Ali","Pre","Rem","Cach","Germ","Dead")
distance$Pre = distance$Pre/distance$Ori

density[density$Size == "S ",]
distance$Size
dens = summarySE(density, measurevar="Ali", groupvars=c("Type","Size"))
dist$Treat = c("Near","Far")
dist$Treat = c("Near","Far","Near","Far","Near","Far")

pd = position_dodge(0.1)

ggpr = ggplot(dens, aes(x=Size, y=Ali, colour=Type)) + 
  geom_errorbar(aes(ymin=Ali-ci, ymax=Ali+ci), width=.1, position=pd, size = 1) +
  geom_point(position=pd, size = 3) +
  xlab("Size") +
  ylab("Per Ali") +
  ##geom_quantile(aes(Rem,Ori),method = "rq", alpha = 0.9) +
  scale_colour_hue(name="Type", 
                   breaks=c("R", "PR", "N"),
                   labels=c("Removed", "Predated", "Ignored"),
                   l=40) +
                     ##opts(title = "Seed Survival in Different Treatments") +
                     theme_bw() +
                     opts(legend.justification=c(1,0), legend.position=c(0.9,0))  
ggpr+
  opts(axis.title.x = theme_text(vjust = 0.3, size = 20, face = "bold"), axis.text.x = theme_text(size = 16), axis.title.y = theme_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = theme_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  opts(legend.title = theme_text(size = 20, face = "bold"), legend.text = theme_text(size = 16))
  scale_x_discrete(breaks = c(1,2),labels = c("Near","Far")) 
  ##opts(legend.justification=c(1,1), legend.position=c(0.95,0.45)) 

density[density$Rem > 1,]$Rem = 36/38
