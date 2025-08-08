dat1<-read.csv("kFFTdis1.csv")
dat2<-read.csv("kFFTdis2.csv")
dat3<-read.csv("kFFTdis3.csv")
dat4<-read.csv("kFFTdis4.csv")
data<-as.vector(c(dat1[,1],dat2[,1],dat3[,1],dat4[,1]))


# Load packages poweRlaw, ggplot2

minx = min(data) # if xmin is not known otherwise set known value

# 1) fit pareto, exp and lnorm (uses package poweRlaw; very convenient to plot)
# The following code is for a continuous data points or a very large number of 
# discrete data points. For few discrete data points, use the code at the end

pow = conpl$new(data) # stores data in prep for parameter estimation
# minx = estimate_xmin(pow) use if you need xmin to be estimated
pow$setXmin(minx)
est1 = estimate_pars(pow) # estimates parameters using MLE
pow$setPars(est1$pars) # store estimated pars in the appropriate way

exp = conexp$new(data)
exp$setXmin(minx)
est2 = estimate_pars(exp)
exp$setPars(est2$pars)

lnorm = conlnorm$new(data)
lnorm$setXmin(minx)
est3 = estimate_pars(lnorm)
lnorm$setPars(est3$pars)



 write.matrix(est1,"estpow.kFFTdis.txt")
 write.matrix(est2,"estexp.kFFTdis.txt")
 write.matrix(est3,"estlnorm.kFFTdis.txt")



# 2) Compare fits between these three

com = compare_distributions(pow,exp) # all combinations as required
com$test_statistic # vuong test statistic (pvals calculated from this)
com$p_two_sided 
# p = 1 implies that both distributions are equally far from the data
# p = 0 implies that one of the distributions fits the data significantly better
com$p_one_sided
# p = 1 implies that the first distribution is the better fit
# p = 0 implies that the first distribution can be rejected in favour of the second

# p-2 sided = 0; p-1 sided = 1 => 1st distribution wins
# p-2 sided = 0; p-1 sided = 0 => 2nd distribution wins

# 3) Compare these distributions with power law with exp cutoff (Note: cannot compare exp and powerexp yet)
# all the attached functions have to be run

exp1 = exp.fit(data,minx) # exponential
pow1 = pareto.fit(data,minx) # power law
lnorm1 = lnorm.fit(data,minx) # log-normal
powexp = powerexp.fit(data,minx) # power law with exponential cut-off

write.matrix(powexp,"powexpkFFTdis.txt")
write.matrix(exp1,"expkFFTdis.txt")
write.matrix(lnorm1,"lnormkFFTdis.txt")
write.matrix(pow1,"powkFFTdis.txt")





power.powerexp.lrt(pow1,powexp) # compare pareto and powerexp
# gives only a single p-value. If p = 0, powerexp is better
exp.powerexp.lrt(exp1,powexp)
# gives only a single p-value. There is an issue here. 
# If it is an underlying exponential distribution (extreme case), powerexp will be chosen for
# large sample sizes. In such a case, the rate of decline will be similar
vuong(lnorm.powerexp.llr(data, lnorm1, powexp, minx)) # compare lnorm and powerexp
# 2 p values similar to the previous analysis

# 4) Plotting the fits and visual comparison; requires packages ggplot2, poweRlaw

dat = plot(pow) # from the initial analyses
po = lines(pow)
ex = lines(exp)
lno = lines(lnorm)
# to plot power law with exponential cutoff on the same graph, use powexp from part 3)
pox = po
pox$y = 1 - ppowerexp(pox$x,minx,powexp$exponent,powexp$rate,lower.tail=TRUE,log.p=FALSE)
# To ensure that no log(0)s occur,
pox$y[pox$y == 0] = 2
pox$y[pox$y == 2] = min(pox$y)
ex$y[ex$y == 0] = 2
ex$y[ex$y == 2] = min(ex$y)
lno$y[lno$y == 0] = 2
lno$y[lno$y == 2] = min(lno$y)

fin = rbind(po,ex,lno,pox)
fin$type = c(rep("Power law",length(po$x)),rep("Exponential",length(po$x)),rep("Log-normal",length(po$x)),rep("Pow-Exp",length(po$x)))

ggp = ggplot(dat, aes(x=x, y=y))  +
  geom_point(size = 1, col = "black") +
  stat_smooth(data = fin, aes(x = x, y = y, col = type), se = F) +
  xlab("Log Nearest Neighbor Distance") +
  ylab("Log Inverse CDF") +
  scale_colour_hue(name="Distribution", 
                   breaks=c("Exponential","Log-normal","Power law","Pow-Exp"),
                   labels=c("Exponential", "Log-normal", "Power law","Pow-Exp"),
                   l=40) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16, face = "bold"), axis.text.x = element_text(size = 12), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 12))+
  theme(legend.justification=c(1,1), legend.position=c(0.3,0.5)) +
  ggtitle("Kefi model;m=0.1; b=0.5; FAR FROM THE CRITICAL POINT") +
  scale_x_log10()+ # to scale the x-axis logarithmically. Remove if not required.
  scale_y_log10(breaks = c(0.000001,0.00001,0.0001,0.001,0.01,0.1,1), limits = c(0.0001,1.01))+ # scale y-axis
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank(),
    )

# For discrete analysis

pow = displ$new(data)
pow$setXmin(minx)
est1 = estimate_pars(pow)
pow$setPars(est1$pars)

exp = disexp$new(data)
exp$setXmin(minx)
est2 = estimate_pars(exp)
exp$setPars(est2$pars)

lnorm = dislnorm$new(data)
lnorm$setXmin(minx)
est3 = estimate_pars(lnorm)
lnorm$setPars(est3$pars)

# Plot disctrete fits

dat = plot(pow)
po = lines(pow)
ex = lines(exp)
lno = lines(lnorm)

fin = rbind(po,ex,lno)
fin$type = c(rep("Power law",length(po$x)),rep("Exponential",length(po$x)),rep("Log-normal",length(po$x)))

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