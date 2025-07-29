herp = read.csv("D:/Herps.csv")
plot(herp$cumul ~ herp$Sl, main = "Species Accumulation Curve", xlab = "No. of Individuals", ylab = "Cumulative No. of Species", type = "b")