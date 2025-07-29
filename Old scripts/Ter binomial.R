ter = read.csv("D:/Wildlife/R/termite-final.csv")
head(ter)
top = ter[ter$hole.pos == "T", ]
top
head(top)
side = ter[ter$hole.pos == "S", ]
head(side)
par(mfrow = c(2,1))
hist(top$full.time - top$first.resp)
hist(side$full.time - side$first.resp)
comb$repairt = top$full.time - top$first.resp
comb$repairs = side$full.time - side$first.resp
diff = comb$repairt - comb$repairs
diff = subset(diff, !is.na(diff))
diff = ifelse(diff >=0, "Y", "n")
table(diff)
N = 33
bin = numeric(33)
binp = function(x,N,p)
{
fact = factorial(N)/(factorial(x)*factorial(N-x))
fact*(.5^x)*(.5^(N-x))
}
prob = binp(0:33, N, 0.5)
plot(0:33, prob, type = "l")
c = cumsum(prob)
plot(c, type = "l")
c[10] + 1 - c[25]



