dat1 = factor(rep(c("Yes", "no"), c(9, 1)))
dat2 = factor(rep(c("Yes", "no"), c(7, 3)))
dat3 = factor(rep(c("Yes", "no"), c(38, 12)))
dat4 = factor(rep(c("Yes", "no"), c(26, 24)))
reps = 10000
out.vec1 = numeric(reps)
out.vec2 = numeric(reps)
out.vec3 = numeric(reps)
out.vec4 = numeric(reps)
for(i in 1:reps)
{
temp1 = sample(dat1, length(dat1), replace = T)
temp2 = sample(dat2, length(dat2), replace = T)
temp3 = sample(dat3, length(dat3), replace = T)
temp4 = sample(dat4, length(dat4), replace = T)
out.vec1[i] = table(temp1)[2]/length(temp1)
out.vec2[i] = table(temp2)[2]/length(temp2)
out.vec3[i] = table(temp3)[2]/length(temp3)
out.vec4[i] = table(temp4)[2]/length(temp4)
}
villA.10 = quantile(out.vec1, probs = c(0.025, 0.5, 0.975))
villB.10 = quantile(out.vec2, probs = c(0.025, 0.5, 0.975))
villA.50 = quantile(out.vec3, probs = c(0.025, 0.5, 0.975))
villB.50 = quantile(out.vec4, probs = c(0.025, 0.5, 0.975))
par(mfrow = c(1,4))
boxplot(villA.10, ylim = c(0,1))
boxplot(villB.10, ylim = c(0,1))
boxplot(villA.50, ylim = c(0,1))
boxplot(villB.50, ylim = c(0,1))
mean(out.vec1)
mean(out.vec2)
mean(out.vec3)
mean(out.vec4)






