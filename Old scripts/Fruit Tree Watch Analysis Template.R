frtr$Time = frtr$Time/100
truval = trunc(frtr$Time)
dec = frtr$Time - truval
frtr$Time = truval*100 + dec*500/3
frtr$Spe = as.character(frtr$Spe)
frtr$Name = as.character(frtr$Name)

w = unique(frtr[frtr$Name != "FS4",]$Name)
v = unique(frtr[frtr$Name != "FS4",]$Spe)
fin = data.frame(cbind(1:8,0))
fin = fin[,-c(1,2)]
fin[,1:12] = 0
names(fin) = v
for (i in 1:length(w))
{
  for (j in 1:length(v))
  {
    tr = frtr[frtr$Name == w[i] & frtr$Spe == v[j],]
    ob = length(tr$Time)
    indct = 0
    tel = 0
    tme = 0
    if (ob > 0)
    {
      timetemp = tr[tr$Activity == "A",]$Time
      notemp = cumsum(tr[tr$Activity == "A",]$No)
      for (k in 1:ob)
      {
        ct = 0
        if (tr$Activity[k] == "A")
        {
          indct = indct + tr$No[k]
        }
        if (tr$Activity[k] == "L")
        {
          for (p in 1:tr$No[k])
          {
            if ((indct - p) < 0)
              ct = ct + 1
            for (pd in 1:length(timetemp))
            {
              if (notemp[pd] >= (tel + p - ct))
                break
            }
            if (tr$Time[k] >= timetemp[pd])
              tme = tme + tr$Time[k] - timetemp[pd]
            else
              tme = tme + tr$Time[k] - 650
          }      
          indct = indct - tr$No[k]
          tel = tel + tr$No[k]
          if (indct < 0)
          {
            tel = tel + indct
            indct = 0
          }
        }
      }
      if (indct > 0)
      {
        for (po in 1:indct)
        {
          for (pdd in 1:length(timetemp))
          {
            if (notemp[pdd] >= tel + po)
              break
          }
          tme = tme + 1050 - timetemp[pdd]
        }
      }
      fin[i,j] = tme
    }
  }
}
fin = fin*0.6