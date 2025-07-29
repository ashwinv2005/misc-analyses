bricesite = read.csv("C:/Ashwin/All Research and Teaching/Other Studies/Brice analysis/bricesite.csv")
briceloc = read.csv("C:/Ashwin/All Research and Teaching/Other Studies/Brice analysis/briceloc.csv")

library(EcoSimR)    # load EcoSimR library
set.seed(56)

library(cooccur)
data(finches)
head(finches)
str(finches)

temp = bricesite[,-1]
temp = as.matrix(temp)
temp1 = t(temp)

new = data.frame(cbind(1:(length(names(bricesite))-1),0))
new = new[,-c(1,2)]
head(new)
new[,1:96] = 0
colnames(new) = bricesite$Site
rownames(new) = names(bricesite)[-1]
new[1:60,1:96] = temp1

cooccurbrice = cooccur(mat = new,
                       type="spp_site",
                           thresh=TRUE,
                          spp_names=TRUE)

summary(cooccurbrice)
plot(cooccurbrice)

b = prob.table(cooccurbrice)

pair.attributes(cooccurbrice)

write.csv(c,"C:/Users/ashwinv/Desktop/sigcootable.csv")

brice_matrix = matrix(data = rbinom(n = nrow(new)*ncol(new),1,prob = 0.75),
                                           nrow = nrow(new),
                                           ncol = ncol(new),
                                           byrow = T)

a = create.N.matrix(brice_matrix)

obs.v.exp(cooccurbrice)
c = print(cooccurbrice)


head(temp)

x = dist(temp,method = 'binary')
xd = hclust(x)
plot(xd,xlab=NA, sub=NA, main = NA)
xd

head(temp)
rownames(temp) = bricesite$Site
