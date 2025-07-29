dat = factor(rep(c(1,0), c(64,36)))
dat
sam = sample(dat, size = 50, replace = TRUE)
sam
out.vec = numeric(20000)
table(sam)[2]/50
for(i in 1:20000)
	{
	sam = sample(dat, size = 50, replace = TRUE)
	out.vec[i] = table(sam)[2]/50
	}
out.vec
a = 0
diff = numeric(10000)
for(i in 1:10000)
{diff[i] = out.vec[2*i] - out.vec[2*i-1]}
hist(diff)
diff
(table(diff>=0.24)+table(diff<=-0.24))/10000