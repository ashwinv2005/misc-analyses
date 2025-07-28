require(tidyverse)
require(Hmisc)

a = read.csv("clubs_AT.csv")

a$winsp = binconf(a$wins,a$total)[,1]
a$winsl = binconf(a$wins,a$total)[,2]
a$winsu = binconf(a$wins,a$total)[,3]

a$drawsp = binconf(a$draws,a$total)[,1]
a$drawsl = binconf(a$draws,a$total)[,2]
a$drawsu = binconf(a$draws,a$total)[,3]

a$lossesp = binconf(a$losses,a$total)[,1]
a$lossesl = binconf(a$losses,a$total)[,2]
a$lossesu = binconf(a$losses,a$total)[,3]

library(ggthemes)
theme_set(theme_tufte())

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

pd = position_dodge(0.2)

ggp = ggplot(data = a, aes(x = club, y = winsp, col = type)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = winsl, ymax = winsu), size = 0.5, width = 0.2, position = pd) +
  #geom_smooth(aes(x = days, y = pred)) +
  xlab("Club") +
  ylab("Probability of a win (95% CI)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(legend.position = "bottom")

jpeg('wins_AT.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = a, aes(x = club, y = drawsp, col = type)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = drawsl, ymax = drawsu), size = 0.5, width = 0.2, position = pd) +
  #geom_smooth(aes(x = days, y = pred)) +
  xlab("Club") +
  ylab("Probability of a draw (95% CI)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(legend.position = "bottom")

jpeg('draws_AT.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = a, aes(x = club, y = lossesp, col = type)) +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = lossesl, ymax = lossesu), size = 0.5, width = 0.2, position = pd) +
  #geom_smooth(aes(x = days, y = pred)) +
  xlab("Club") +
  ylab("Probability of a loss (95% CI)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(legend.position = "bottom")

jpeg('losses_AT.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()
