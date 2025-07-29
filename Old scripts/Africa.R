c = read.csv("C:/Ashwin/CSV/Veg/aVegetation density.csv")
d = read.csv("C:/Ashwin/CSV/Veg/aCorrelations.csv")
d$Treat = as.character(d$Treat)
head(d)

ggp = ggplot(c, aes(x=Year, y=Density, col = Site))  +
  geom_point(size = 2) +
  geom_line(size = 0.7) +
  facet_grid(Treat ~ .)+
  ##stat_smooth(data = fin, aes(x = x, y = y, col = type), se = F) +
  xlab("Year") +
  ylab("Vegetation density") +
  scale_colour_hue(name="Site", 
                   breaks=c("Dzombo","Numbi","Nwanetsi","Skukuza"),
                   labels=c("Dzombo","Numbi","Nwanetsi","Skukuza"),
                   l=40) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 8))+
  #theme(legend.justification=c(1,1), legend.position=c(0.9,1.05)) +
  scale_x_continuous(breaks = c(1983,2000,2003,2006), limits = c(1982,2007)) + 
  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7)) + # scale y-axis
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )

d = d[d$Density < 0.7,]
e = d[d$Treat == "burnt",]

plot(e$Sh~e$Sc)
fit = lm(e$Sh~e$Sc)
summary(fit)
abline(fit)

ggs = ggplot(d, aes(x = Sc, y = Sh)) + 
  geom_point(size = 2.5) + 
  facet_grid(Treat ~ .) +
  stat_smooth(method = "lm", se = F, col = "black") + 
  xlab("Scale of log-normal distribution") +
  ylab("Shape of log-normal distribution") +
  theme_bw() 
  #annotate("text", label = "R sq. = 0.4147", x = 0.13, y = 1.1, l = 20)
ggs + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )