## Checking assumptions

head(abund)

abund = read.csv("C:/Ashwin/newf.csv")
rich = read.csv("C:/Ashwin/rich.csv")
abund$sqsize = abund$size^2
abund$sqcrk = abund$creek^2
abund$sqdep = abund$depth^2
abund$logsize = log(abund$size)
abund$logcrk = log(abund$creek)
abund$logden = log(abund$density)
abund$logdep = log(abund$depth)
abund$logcan = log(abund$canopy)

head(abund)
hist(abund$abd,100)
mod1 = glmer(data = abund, abd ~ canopy + depth + density + creek + size + (1|species), family = poisson)
mod2 = glmer(data = abund, abd ~ depth + density + creek + size + (1|species), family = poisson)
mod3 = glmer(data = abund, abd ~ canopy + density + creek + size + (1|species), family = poisson)
mod4 = glmer(data = abund, abd ~ canopy + depth + creek + size + (1|species), family = poisson)
mod5 = glmer(data = abund, abd ~ canopy + depth + density + size + (1|species), family = poisson)
mod7 = glmer(data = abund, abd ~ canopy + depth + density + creek + size + creek*size + (1|species), family = poisson)
mod8 = glmer(data = abund, abd ~ canopy + depth + creek + size + creek*size + (1|species), family = poisson)
mod6 = glmer(data = abund, abd ~ canopy + depth + creek + logsize + creek*logsize + (1|species), family = poisson)
mod9 = glmer(data = abund, abd ~ depth + creek + size + creek*size + (1|species), family = poisson)

##with plot and species as random effect
mod10 = glmer(data = abund, abd ~ canopy + depth + density + creek + size + (1|species) + (1|plot), family = poisson)
mod11 = glmer(data = abund, abd ~ canopy + depth + density + creek + size + (1|plot), family = poisson)
mod12 = glmer(data = abund, abd ~ canopy + depth + creek + size + (1|plot) + (1|species), family = poisson)
mod13 = glmer(data = abund, abd ~ canopy + depth + size + (1|plot) + (1|species), family = poisson)
mod14 = glmer(data = abund, abd ~ depth + size + (1|plot) + (1|species), family = poisson)
mod15 = glmer(data = abund, abd ~ canopy + depth + creek + size + creek*size + (1|plot) + (1|species), family = poisson)
mod16 = glmer(data = abund, abd ~ depth + creek + size + creek*size + (1|plot) + (1|species), family = poisson)
mod17 = glmer(data = abund, abd ~ depth + sqcrk*size + (1|species/plot), family = poisson)
mod18 = glmer(data = abund, abd ~ logdep + sqcrk*logsize + (1|species/plot), family = poisson)
mod19 = glmer(data = abund, abd ~ depth + sqcrk*logsize + (1|species/plot), family = poisson)
mod20 = glmer(data = abund, abd ~ depth + creek*logsize + (1|species/plot), family = poisson)
mod21 = lmer(data = abund, abd ~ depth + creek*logsize + (1|plot))

##with plot, species, and squared terms
mod22 = glmer(data = abund, abd ~ depth + sqcrk + size + density + canopy + (1|plot) + (1|species), family = poisson)
mod23 = glmer(data = abund, abd ~ depth + sqcrk + size + sqcrk*size + (1|plot) + (1|species), family = poisson)
mod24 = glmer(data = abund, abd ~ sqdep + creek + size + (1|plot) + (1|species), family = poisson)
mod25 = glmer(data = abund, abd ~ sqdep + creek + size + creek*size + (1|plot) + (1|species), family = poisson)
coef(mod21)

##with random intercepts and random slopes
mod1 = glmer(data = abund, abd ~ depth + creek + logsize + (1 + logsize|species/plot), family = poisson)
mod2 = glmer(data = abund, abd ~ depth + density + creek + logsize + (1+ logsize|species/plot) + (1 + depth|species/plot), family = poisson)
coef(mod2)

re<-ranef(mod20,condVar = TRUE)
qqmath(re)
qqnorm(resid(mod20))
lines(seq(-2,2,0.05), seq(-1,1,0.025))

hist(resid(mod20),30)


anova(mod20,mod1)
summary(mod21)
plot(mod24)
qqplot(mod16)

qqnorm(mod16., ~ranef(., level=2))

plot(abund$abd~(1/abund$size)^2)

## with richness
head(rich)
rich$Fish.richness == rich$richness
rich$Plot.No = rich$plot
rich$Density = rich$density
rich$Canopy = rich$canopy
rich$Depth = rich$depth
rich$Creek.Dist = rich$creek
rich$Fish.richness = rich$richness

names(rich)
rich$sqcreek = rich$creek^2

mod26 = glmer(data = rich, richness ~ canopy + depth + density + creek + (1|plot), family = poisson)
mod27 = glmer(data = rich, richness ~ depth + density + creek + (1|plot), family = poisson)
mod28 = glmer(data = rich, richness ~ density + creek + (1|plot), family = poisson)
mod29 = glmer(data = rich, richness ~ creek + (1+creek|plot), family = poisson)
mod30 = glmer(data = rich, richness ~ sqcreek + (1+sqcreek|plot), family = poisson)


summary(mod30)
plot(mod29)
summary(lm(creek~richness, data = rich))

anova(mod30,mod28)

plot(data = rich, richness ~ sqcreek)
abline(lm(data = rich, richness ~ sqcreek))

log(10)
log(exp(1))
exp(3)

summary(mod20)
ran2 = coef(mod20)$'species'
hist(ran1$'(Intercept)', 100)
ran = ranef(mod20)[1]
fix = fixef(mod20)
standardfixm = fix

standardfixm[2] = standardfixm[2]*quantile(abund$depth,0.5)
standardfixm[3] = standardfixm[3]*quantile(abund$creek,0.5)
standardfixm[4] = standardfixm[4]*quantile(abund[!is.na(abund$size),]$logsize,0.5)
standardfixm[5] = standardfixm[5]*quantile(abund[!is.na(abund$size),]$logsize,0.5)*quantile(abund[!is.na(abund$size),]$creek,0.5)

standardfix
standardfix0
standardfix25
standardfix50
standardfix75
standardfix100

standardfixss
standardfixsl
standardfixls
standardfixll
standardfixm


##### WHAT YOU NEED!

plot(mod20,grid=F, xlab="Fitted values", ylab="Residuals",)
hist(ran1$'(Intercept)', 30)
hist(ran2$'(Intercept)', 50)
hist(resid(mod20),30)
qqnorm(resid(mod20))

########
??ggplot
??quantileregression
head(rich)
## quantile regression for richness
rich1 = ggplot(rich, aes(creek, richness)) + 
  ##geom_errorbar(aes(ymin=Per-se, ymax=Per+se), width=.1) +
  ##geom_bar(width = 0.5, colour = "black", fill = "dark grey") +
  ##geom_errorbar(aes(ymin = bcl,ymax = bcr), width = 0.1, size = 1) +
  geom_point(size = 3) +
  geom_quantile(stat = "quantile", position = "identity", quantile = 0.95, col = "black", formula = y~x, method = "rq") +
  xlab("Distance from the creeks") +
  ylab("Species Richness") +
  ##theme(title = "Time Spent Foraging (Foraging Bout)") +
  theme_bw()
rich1+
  theme(axis.title.x = element_text(vjust = 0.1, size = 20, face = "bold"), axis.text.x = element_text(size = 11), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 13)) +
  scale_x_discrete(limits = seq(0,350, length = 15), expand = c(0.05,0.05)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ri = rq(rich$richness~rich$creek, tau = 0.95)
summary(ri, se = "boot")

plot(abund$abd~abund$creek)
##splines
a=qsreg(rich$creek,rich$richness, lam = NA, maxit = 50, maxit.cv = 10, tol =
          1e-07, offset = 0, sc = sqrt(var(abd$Abd)) * 1e-05, alpha =
          .98, wt = rep(1, length(abd$Creek)), cost = 1, nstep.cv = 80,
        hmin = NA, hmax = NA, trmin = 2 * 1.05, trmax = 0.95
        * length(unique(abd$Creek)))
plot(a)
??qsreg
citation()
