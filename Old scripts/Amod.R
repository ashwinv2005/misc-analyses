com = read.csv("C:/community_abundance.csv")
coral = read.csv("C:/coralcover.csv")
summary(com)
names(com)
library(ggplot2)
com$cor = coral$livecoral

e=with(com,tapply(abundance,list(location,depth),sum))
a = summarySE(com, measurevar="corallivore", groupvars=c("depth","location"))
b = summarySE(com[com$location != "wall of wonder",], measurevar="corallivore", groupvars=c("cor","depth"))
b = summarySE(com[com$guild == 1 & com$depth == "shallow",], measurevar="abundance", groupvars=c("location","aspect"))

pd = position_dodge(.1)

ggplot(b, aes(x=aspect, y=corallivore, colour=depth)) + 
    geom_errorbar(aes(ymin=corallivore-ci, ymax=corallivore+ci), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    xlab("Depth") +
    ylab("Total") +
    scale_colour_hue(name="Type", 
                     breaks=c("east","west"),
                     labels=c("east","west"),
                     l=40) +
    opts(title = "Live Coral Cover Across Location") +
    theme_bw() +
    opts(legend.justification=c(1,1), legend.position=c(0.9,1))