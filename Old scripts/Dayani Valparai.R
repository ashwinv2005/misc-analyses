a = read.csv("C:/Ashwin/aktree_rankabund.csv")

## Just to make the names better.
names(a)[4] = "size"
names(a)[6] = "mammals"
names(a)[7] = "others"
names(a)[8] = "sbirds"
names(a)[9] = "lbirds"
names(a)[10] = "ungulates"
names(a)[11] = "bats"
names(a)[12] = "civets"
names(a)[13] = "primates"
names(a)[14] = "elephants"
names(a)[15] = "bears"
names(a)[16] = "rodents"
names(a)[19] = "mechanical"
names(a)[20] = "tree"
names(a)[21] = "saps"
names(a)[22] = "sapstree"

## This will remove all species without adult trees. Alternatively, if you are looking at either seedlings or adults instead of seedlings/adult
## this statement can then be skipped.
a = a[a$tree != 0,]

## You can individually compare effects with and without specific dispersers using this.
s = summarySE(a, measurevar = "sapstree", groupvar = "wind") 

a$disp = 0 ## Making the coluumn with disperser goups and initializing them to 0 (others)

l = length(a[,1]) ## Number of plant species in the site

## this is a loop which converts the data from binary to its own unique decimal form. 
## Eg: if for a tree sbirds = 1 and all others equal 0, then the decimal value will be 2^5 = 32. Read up about the conversion.
## small birds, large birds, bats, civets, primates, rodents is the order. I have only used these. All others are others.

for (i in 1:l) 
{
  a$disp[i] = a$rodents[i]*2^0 + a$primates[i]*2^1 + a$civets[i]*2^2 + a$bats[i]*2^3 + a$lbirds[i]*2^4 + a$sbirds[i]*2^5
}

## This table tells you how many entries are there with each decimal value. As I said earlier, there are 64 possible values.

table(a$disp)
x = names(table(a$disp))
x = as.numeric(x)

## You can now back check to see the binary code. If the number is 15, it will show up as 001111. 
## This means that all trees with 15 are eaten by bats, civets, primates and rodents but not by birds.
n = 15
paste(sapply(strsplit(paste(rev(intToBits(n))),""),`[[`,2)[27:32],collapse="")

## Now the problem is when certain combinations appear only once or twice. 
## You may then prefer it if it is changed to the nearest binary value which occurs most often.
## Eg: if 15 (001111) appears only once or twice, it can be put in either 001110 (14) or 001101 (13) or 001011 (11) or 000111 (7)
## All these are just one disperser away from the original one. I have thought about this and I feel that this choice you make will be subjective.
## Once you choose what to change it to, just change the entry in a$disp in the data frame. Eg: a[a$disp == 15,]$disp = 13

## IMPORTANT
## Before any analysis using this column, modify it(as discussed earlier; remove singleton associations) and 
## convert its contents into factors. a$disp = as.factor(a$disp)

## MAKES SENSE?