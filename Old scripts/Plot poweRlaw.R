che1 = data.frame(cbind(1:3012,0))
che1 = che1[,-c(1,2)]
che1$x = dat$x
che1$y = powche

che2 = data.frame(cbind(1:3012,0))
che2 = che2[,-c(1,2)]
che2$x = dat$x
che2$y = expche

minx = min(dat$x)
powche = ppareto(dat$x,minx,2.062212)
powche = 1-powche
expche = pexp(dat$x-minx,0.1830994)
expche = 1-expche
head(expche)
max(data)

dat = plot(pow)
po = lines(pow)
ex = lines(exp)
lno = lines(lnorm)
pox = po
pox$y = 1 - ppowerexp(pox$x,minx,powexp$exponent,powexp$rate,lower.tail=TRUE,log.p=FALSE)
pox$y[pox$y == 0] = 2
pox$y[pox$y == 2] = min(pox$y)

fin = rbind(po,ex,lno)
fin$type = c(rep("Power law",17),rep("Exponential",17),rep("Log-normal",17))

ggp = ggplot(dat, aes(x=x, y=y))  +
  geom_point(size = 1, col = "black") +
  stat_smooth(data = fin, aes(x = x, y = y, col = type), se = F) +
  xlab("Inter Patch Distance") +
  ylab("Log Inverse CDF") +
  scale_colour_hue(name="Distribution", 
                   breaks=c("Exponential","Log-normal","Power law"),
                   labels=c("Exponential", "Log-normal", "Power law"),
                   l=40) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 12))+
  theme(legend.justification=c(1,1), legend.position=c(0.3,0.5)) +
  scale_x_log10()+
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1), limits = c(0.0001,1.01))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )


fat = data.frame(cbind(1:length(td),0))
fat = fat[,-c(1,2)]
fat$x = td1
fat$y = td

ggp = ggplot(fat, aes(x=x, y=y))  +
  geom_point(size = 2, col = "black") +
  ##stat_smooth(data = fin, aes(x = x, y = y, col = type), se = F) +
  xlab("Patch size") +
  ylab("Proportion vegetation") +
  #stat_quantile(geom = "quantile", position = "identity",
                #quantile = c(0.99), formula = y ~ x, method = "rq")+
  #scale_colour_hue(name="Distribution", 
                   #breaks=c("Exponential","Log-normal","Power law"),
                   #labels=c("Exponential", "Log-normal", "Power law"),
                   #l=40) +
  ##opts(title = expression(paste("Arrival of ", italic("Prunus zeylanica"), " In All Treatments"))) +
  ##annotate("text", label = "c)", x = 2.75, y = 0.027, l = 30) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 12))+
  theme(legend.justification=c(1,1), legend.position=c(0.3,0.4)) +
  ##scale_x_continuous(expand = c(0.023,0.023),limits = c(-0.2,4.2), breaks = c(0,1,2,3,4),labels = c("Canopy","0-4","4-8","8-12","12-16")) +
  ##scale_y_continuous(limits = c(0,0.027))+
  #scale_x_log10(breaks = c(2,5,10,30,50))+
  #scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )