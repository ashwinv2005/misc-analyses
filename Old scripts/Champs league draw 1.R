as = matrix(0, 8, 8)

for (i in 1:8)
{
  for (j in 1:8)
  {
    if (j != i)
      as[i,j] = 100
  }
}

as1 = as/100

## 4,6 Man City Arsenal

as1[4,6] = 0

ct = matrix(0, 8, 8)

b = 1:8
b1 = b[b != 4]
b2 = b[b != 6]

for (i in b1)
{
  for (j in b2[as1[i,b2] != 0])
  {
    count = 0
    for (k in b1[b1 != i][1])
    {
      for (l in b2[b2 != j & as1[k,b2] != 0])
      {
        for (l1 in b1[b1 != i][2])
        {
          for (l2 in b2[b2 != j & b2 != l & as1[l1,b2] != 0])
          {
            for (l3 in b1[b1 != i][3])
            {
              for (l4 in b2[b2 != j & b2 != l & b2 != l2 & as1[l3,b2] != 0])
              {
                for (l5 in b1[b1 != i][4])
                {
                  for (l6 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & as1[l5,b2] != 0])
                  {
                    for (l7 in b1[b1 != i][5])
                    {
                      for (l8 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & as1[l7,b2] != 0])
                      {
                        for (l9 in b1[b1 != i][6])
                        {
                          for (l10 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & b2 != l8 & as1[l9,b2] != 0])
                          {
                            count = count + 1
                            print(count)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      ct[i,j] = count
    }
  }
}

rsum = rowSums(ct)

for (i in 1:8)
{
  for (j in 1:8)
  {
    ct[i,j] = ct[i,j]/rsum[i]
  }
}

ct = ct*as[4,6]
ct[is.na(ct)] = 0

as = as - ct
as[4,6] = 0

## 7,6 Chelsea Arsenal

as1[7,6] = 0

ct = matrix(0, 8, 8)

b = 1:8
b1 = b[b != 7]
b2 = b[b != 6]

for (i in b1)
{
  for (j in b2[as1[i,b2] != 0])
  {
    count = 0
    for (k in b1[b1 != i][1])
    {
      for (l in b2[b2 != j & as1[k,b2] != 0])
      {
        for (l1 in b1[b1 != i][2])
        {
          for (l2 in b2[b2 != j & b2 != l & as1[l1,b2] != 0])
          {
            for (l3 in b1[b1 != i][3])
            {
              for (l4 in b2[b2 != j & b2 != l & b2 != l2 & as1[l3,b2] != 0])
              {
                for (l5 in b1[b1 != i][4])
                {
                  for (l6 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & as1[l5,b2] != 0])
                  {
                    for (l7 in b1[b1 != i][5])
                    {
                      for (l8 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & as1[l7,b2] != 0])
                      {
                        for (l9 in b1[b1 != i][6])
                        {
                          for (l10 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & b2 != l8 & as1[l9,b2] != 0])
                          {
                            count = count + 1
                            print(count)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      ct[i,j] = count
    }
  }
}

rsum = rowSums(ct)

for (i in 1:8)
{
  for (j in 1:8)
  {
    ct[i,j] = ct[i,j]/rsum[i]
  }
}

ct = ct*as[7,6]
ct[is.na(ct)] = 0

as = as - ct
as[7,6] = 0

## 8,7 Zenit Kyiv

as1[8,7] = 0

ct = matrix(0, 8, 8)

b = 1:8
b1 = b[b != 8]
b2 = b[b != 7]

for (i in b1)
{
  for (j in b2[as1[i,b2] != 0])
  {
    count = 0
    for (k in b1[b1 != i][1])
    {
      for (l in b2[b2 != j & as1[k,b2] != 0])
      {
        for (l1 in b1[b1 != i][2])
        {
          for (l2 in b2[b2 != j & b2 != l & as1[l1,b2] != 0])
          {
            for (l3 in b1[b1 != i][3])
            {
              for (l4 in b2[b2 != j & b2 != l & b2 != l2 & as1[l3,b2] != 0])
              {
                for (l5 in b1[b1 != i][4])
                {
                  for (l6 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & as1[l5,b2] != 0])
                  {
                    for (l7 in b1[b1 != i][5])
                    {
                      for (l8 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & as1[l7,b2] != 0])
                      {
                        for (l9 in b1[b1 != i][6])
                        {
                          for (l10 in b2[b2 != j & b2 != l & b2 != l2 & b2 != l4 & b2 != l6 & b2 != l8 & as1[l9,b2] != 0])
                          {
                            count = count + 1
                            print(count)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      ct[i,j] = count
    }
  }
}

rsum = rowSums(ct)

for (i in 1:8)
{
  for (j in 1:8)
  {
    ct[i,j] = ct[i,j]/rsum[i]
  }
}

ct = ct*as[8,7]
ct[is.na(ct)] = 0

as = as - ct
as[8,7] = 0

rsum = rowSums(as)

for (i in 1:8)
{
  rowperc[i,] = 100*(as[i,]/rsum[i])
}

rownames(rowperc) = c("Real", "Wolfsburg", "Atletico", "City", "Barcelona", "Bayern", "Chelsea", "Zenit")
colnames(rowperc) = c("PSG", "PSV", "Benfica", "Juventus", "Roma", "Arsenal", "Kyiv", "Gent")

colSums(rowperc)
rowSums(rowperc)

write.csv(rowperc, "C:/Users/ashwinv/Desktop/Champs league.csv")
