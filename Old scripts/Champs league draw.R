as = matrix(data = 0, 8, 8)

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (j != i)
      as[i,j] = 100
  }
}

as1 = as/100
ct = numeric(8)
as1[4,6] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 4 & j != 6)
      ct[j] = ct[j] + as1[i,j]
  }
}

csum = sum(ct)
for (i in 1:8)
{
    ct[i] = ct[i]/csum
}

tem = numeric(8)

for (i in 1:8)
{
  tem[i] = as[4,6]*ct[i]
}
  
as[4,6] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 4 & j != 6)
      as[i,j] = (as[i,j] - tem[j])*as1[i,j]
  }
}

##########

ct = numeric(8)
as1[7,6] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 7 & j != 6)
      ct[j] = ct[j] + as1[i,j]
  }
}

csum = sum(ct)
for (i in 1:8)
{
  ct[i] = ct[i]/csum
}
tem = numeric(8)

for (i in 1:8)
{
  tem[i] = as[7,6]*ct[i]
}

as[7,6] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 7 & j != 6)
      as[i,j] = (as[i,j] - tem[j])*as1[i,j]
  }
}

ct = numeric(8)
as1[8,7] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 8 & j != 7)
      ct[j] = ct[j] + as1[i,j]
  }
}

csum = sum(ct)
for (i in 1:8)
{
  ct[i] = ct[i]/csum
}
tem = numeric(8)

for (i in 1:8)
{
  tem[i] = as[8,7]*ct[i]
}

as[8,7] = 0

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (i != 8 & j != 7)
      as[i,j] = (as[i,j] - tem[j])*as1[i,j]
  }
}

rsum = rowSums(as)

for (i in 1:8)
{
  rowperc[i,] = 100*(as[i,]/rsum[i])
}

rownames(rowperc) = c("Real", "Wolfsburg", "Atletico", "City", "Barcelona", "Bayern", "Chelsea", "Zenit")
colnames(rowperc) = c("PSG", "PSV", "Benfica", "Juventus", "Roma", "Arsenal", "Kyiv", "Gent")
