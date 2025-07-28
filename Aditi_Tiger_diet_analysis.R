library(tidyverse)
library(vegan)
ind = read.csv("4_IndividualId_Diet_Matrix.csv")
ind$uniqueID = paste(ind$Individual,ind$Sample.ID)

# hclust

data = ind[ind$Individual != "Unknown",-c(1:4,21)]
row.names(data) = ind[ind$Individual != "Unknown",]$uniqueID

dist.mat = vegdist(data,method="bray", binary = T)

clust.res1 = hclust(dist.mat)
par(mar=c(5,6,4,10)+.1)
plot(as.dendrogram(clust.res1), edgePar=list(col=3, lwd=3), horiz=T) 

data = ind[ind$Individual == "Unknown",-c(1:4,21)]
row.names(data) = ind[ind$Individual == "Unknown",]$uniqueID

dist.mat = vegdist(data,method="bray")

clust.res2 = hclust(dist.mat)
plot(clust.res2)

data = ind[-c(1:4,21)]
row.names(data) = ind$uniqueID

dist.mat = vegdist(data,method="bray")

clust.res3 = hclust(dist.mat)
plot(clust.res3)


tdist = dist(data, method = "euclidean")
loc = cmdscale(tdist) 
plot(loc[,1], -loc[,2], type="n", xlab="", ylab="", main="cmdscale(tiger diet)")
text(loc[,1], -loc[,2], rownames(loc), cex=0.8) 







########### NMDS

library(tidyverse)
library(vegan)
ind = read.csv("4_IndividualId_Diet_Matrix.csv")

# create a column to exclude individuals that are represented only once

ind$Exclude = NA
ind$Exclude[ind$Individual == "Unknown"] = "Unknown"
ind$Exclude[ind$Individual %in% c("Individual 4","Individual 5","Individual 18","Individual 19",
                                  "Individual 20")] = "One"
ind$Exclude[ind$Individual %in% c("Individual 7","Individual 8",
                                  "Individual 10","Individual 11","Individual 13","Individual 14",
                                  "Individual 17")] = "Two"

ind$uniqueID = paste(ind$Individual,ind$Sample.ID)
ind$others = 0
ind$others[ind$Caracal == 1 | ind$Duck == 1 | ind$Gyps.fulvus == 1 | ind$Junlgefowl == 1 |
             ind$Leopard == 1 | ind$Mus.musculus == 1 | ind$Primate == 1 | ind$Slothbear == 1 |
             ind$Canis.lupus.familiaris == 1 | ind$Wildpig == 1] = 1
ind$Cattle_com = 0
ind$Cattle_com[ind$Cattle == 1 | ind$Buffalo == 1] = 1
ind$Antelope = 0
ind$Antelope[ind$Gazelle == 1 | ind$Nilgai == 1] = 1
ind = ind[,-c(5,6,7,8,10,11,12,13,14,15,16,17,19,20)]

known = ind[!ind$Exclude %in% c("Unknown"),]
knownmat = known[,-c(1:4,7,8)]
indmat = ind[,-c(1:4,7,8)]
colSums(knownmat)



data = known
datamat = knownmat
nmds = metaMDS(datamat, distance = "bray")

data.scores = as.data.frame(scores(nmds))
#add columns to data frame 
data.scores$Individual = data$Individual

ggp = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Individual))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Individual", y = "NMDS2") 



ano = anosim(datamat, data$Individual, distance = "bray", permutations = 999)
ano




################

a = names(table(ind$Individual[ind$others == 1]))
b = names(table(ind$Individual[ind$others == 0]))
setdiff(b,a)




################

# test whether any individual is different from the pool

# 1, 3, 6, 15, 16


library(tidyverse)
library(vegan)
ind = read.csv("4_IndividualId_Diet_Matrix.csv")

# create a column to exclude individuals that are represented only once

ind$Exclude = NA
ind$Exclude[ind$Individual == "Unknown"] = "Unknown"
ind$Exclude[ind$Individual %in% c("Individual 4","Individual 5","Individual 18","Individual 19",
                                  "Individual 20")] = "One"
ind$Exclude[ind$Individual %in% c("Individual 7","Individual 8",
                                  "Individual 10","Individual 11","Individual 13","Individual 14",
                                  "Individual 17")] = "Two"

ind$uniqueID = paste(ind$Individual,ind$Sample.ID)
ind$others = 0
ind$others[ind$Caracal == 1 | ind$Duck == 1 | ind$Gyps.fulvus == 1 | ind$Junlgefowl == 1 |
             ind$Leopard == 1 | ind$Mus.musculus == 1 | ind$Primate == 1 | ind$Slothbear == 1] = 1
ind = ind[,-c(7,10,12,13,14,15,17,19)]


ind$x = "All"
ind$x[ind$Individual == "Individual 1"] = "Ind 1"
ind$x[ind$Individual == "Individual 3"] = "Ind 3"
ind$x[ind$Individual == "Individual 6"] = "Ind 6"
ind$x[ind$Individual == "Individual 15"] = "Ind 15"
ind$x[ind$Individual == "Individual 16"] = "Ind 16"
indmat = ind[,-c(1:4,13,14,16)]



data = ind
datamat = indmat
nmds = metaMDS(datamat, distance = "bray")

data.scores = as.data.frame(scores(nmds))
#add columns to data frame 
data.scores$x = data$x

ggp = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = x))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Individual", y = "NMDS2") 



ano = anosim(datamat, data$x, distance = "bray", permutations = 999)
ano
