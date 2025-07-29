a = read.csv("C:/Ashwin/CSV/Veg/jCorrelations.csv")
names(a)
b = a[a$Plant == "grass",]
c = a[a$Plant == "veg",]
d = b[b$Shrubs == "intact",]
e = c[c$Shrubs == "intact",]

graintg = summarySE(b, measurevar="Cutoff", groupvars=c("Status","Shrubs"))
graintv = summarySE(c, measurevar="Cutoff", groupvars=c("Status","Shrubs"))
grag = summarySE(b, measurevar="Cutoff", groupvars=c("Status"))
grav = summarySE(c, measurevar="Cutoff", groupvars=c("Status"))

ggp = ggplot(d, aes(x=Status, y=Cutoff))  +
  geom_point(size = 2) +
  xlab("Treatment") +
  ylab("Exponential cutoff") +
  theme_bw() 
ggp + 
  #scale_y_continuous(limits = c(0,110000))+
    theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )

ggs = ggplot(a, aes(x = Density, y = Cutoff, col = Plant)) + 
  geom_point(size = 2.5) + 
  #facet_grid(Treat ~ .) +
  stat_smooth(method = "lm", se = F) + 
  xlab("Vegetation density") +
  ylab("Exponential cutoff") +
  theme_bw() 
#annotate("text", label = "R sq. = 0.4147", x = 0.13, y = 1.1, l = 20)
ggs + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  scale_y_log10() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  ) +
  theme(legend.position = "none")

plot(log(b$Cutoff)~b$Density)
r = lm(log(b$Cutoff)~b$Density)
abline(r)
summary(r)

plot(b$Density~c$Density)

pd = position_dodge(0.1)
ggp = ggplot(grag, aes(x = Status, y = Cutoff))  +
  geom_errorbar(aes(ymin=Cutoff - ci, ymax=Cutoff + ci), width=.1, position=pd, size = 0.75) +
  geom_point(position=pd, size = 3) +
  xlab("Treatment") +
  ylab("Cutoff") +
  #scale_colour_hue(name="Shrubs", 
                   #breaks=c("intact","removed"),
                   #labels=c("Shrubs intact", "Shrubs removed"),
                   #l=40) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 12))+
  theme(legend.justification=c(1,1), legend.position=c(0.99,0.31)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )
  theme(legend.position = "none")