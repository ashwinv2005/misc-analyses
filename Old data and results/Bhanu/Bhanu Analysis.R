pred = read.csv("C:/Ashwin/fg.csv")

x = pred$crk
y = pred$pred
dat = data.frame(cbind(1:6),0)
names(dat) = c("x","y")
dat$x = x
dat$y = y
dat$cl = pred$cl
dat$cr = pred$cr
dat$cl[5:6] = 0

## using poly()

fit1 = lm(data = pred, pred~crk)
fit2 = lm(data = pred, pred~poly(crk,2,raw = T))
fit3 = lm(data = pred, pred~poly(crk,3,raw = T))

xx = 1:150
plot(pred$crk, pred$pred)
lines(xx, predict(fit1, data.frame(crk=xx)), col = "red")
lines(xx, predict(fit2, data.frame(crk=xx)), col = "blue")
lines(xx, predict(fit3, data.frame(crk=xx)), col = "green")

teth = glmer(data = pred, pred ~ crk + (1|crk), family = poisson)
summary(teth)
## smoothing using loess

lo = loess(y~x)
plot(pred$pred~pred$crk)
lines(predict(lo), col = "red")

## using nls

models = list(lm(y~x), 
               #lm(y~I(1/x), data = dat),
               #lm(y ~ log(x), data = dat),
               #nls(y ~ I(1/x*a) + b*x, start = list(a = 1, b = 1), data = dat), 
               #nls(y ~ (a + b*log(x)), data=dat, start = setNames(coef(lm(y ~ log(x))), c("a", "b")), data = dat),
               nls(y ~ I(exp(1)^(a + b * x)), start = list(a=0,b=0), data = dat),
               nls(y ~ exp(a*x)/(1 + exp(b*(x+1))), start = list(a=1,b=1), data = dat)
               #nls(y ~ (x^2)/(1 + exp(b*(x+1))), start = list(b=0), data = dat)
)

summary(models[[3]])
hist(resid(models[[3]]))
anova(models[[3]],models[[1]])

library(ggplot2)
ggplot(dat, aes(x, y)) + geom_point(size = 5) +
  stat_smooth(method = "lm", formula = as.formula(models[[1]]), size = 1, se = FALSE, colour = "black") + 
  #stat_smooth(method = "lm", formula = as.formula(models[[2]]), size = 1, se = FALSE, colour = "blue") + 
  #stat_smooth(method = "lm", formula = as.formula(models[[3]]), size = 1, se = FALSE, colour = "yellow") + 
  #stat_smooth(method = "nls", formula = as.formula(models[[4]]), data=dat, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red") + 
  #stat_smooth(method = "nls", formula = as.formula(models[[5]]), data=dat, start = setNames(coef(lm(y ~ log(x), data=dat)), c("a", "b")), size = 1, se = FALSE, colour = "green") +
  stat_smooth(method = "nls", formula = as.formula(models[[2]]), data=dat, start = list(a=0,b=0), size = 1, se = FALSE, colour = "violet") + 
  stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=dat, start = list(a=1,b=1), size = 1, se = FALSE, colour = "orange")

exp(-0.663294)

??nls
predpl = ggplot(dat, aes(x, y)) + geom_point(size = 5) +
  geom_errorbar(aes(ymin=cl, ymax=cr), width=1) +
  stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=dat, start = list(a=1,b=1), size = 1, se = FALSE, colour = "black") +
  xlab("Distance from creek") +
  ylab("Proportion of fish eaten") +
  ##opts(title = "Time Spent Foraging (Foraging Bout)") +
  theme_bw()

predpl +
  theme(axis.title.x = element_text(vjust = 0.1, size = 20, face = "bold"), axis.text.x = element_text(size = 11), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 13)) +
  scale_x_continuous(breaks = c(0,25,50,75,100,150), labels = c(0,25,50,75,100,150)) +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), labels = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )
  
  
## trial

xx = 0:150
yy = exp(-0.01782*xx)/(1 + exp(-0.66329*(xx+1)))
yy = exp(-0.2244*xx)/(1 + exp(-0.66*(xx+1)))
plot(x,y)
lines(xx,yy)
log(0.34/0.66)
exp(0.25)

pr = glm(data = pred, pred~crk, family = binomial(logit))
summary(pr)
plot(pred~crk, data = pred)
lines(pred$crk, pr$fitted, type = "l", col = "red")
