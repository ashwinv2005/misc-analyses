# protocol (proto) S,T,ST
# location F if none

spcum = function(data,proto)
{
  b = data
  
  if (proto == "S")
  {
    b = b[b$protocol == "eBird - Stationary Count",]
  }
  
  if (proto == "T")
  {
    b = b[b$protocol == "eBird - Traveling Count",]
  }
  
  if (proto == "ST")
  {
    b = b[b$protocol == "eBird - Stationary Count" | b$protocol == "eBird - Traveling Count",]
  }
  
  len = length(b$id)
  tempid = b$id[1]
  first = b[b$id == tempid,]
  
  specieslist = character(length(unique(b$name)))
  specieslist[1:length(first$name)] = as.character(first$name)
  splen = length(first$name)
  
  cumsp = numeric(length(unique(b$id)))
  cumsp[1] = splen
  
  cumdur = numeric(length(unique(b$id)))
  cumdur[1] = b$dur[1]
  
  count = 1
  
  for(i in 2:len)
  {
    if(b$id[i] != b$id[i-1])
    {
      count = count + 1
      tl = length(b[b$id == b$id[i],]$id)
      for(j in 1:tl)
      {
        ct = 0
        for(k in 1:splen)
        {
          if (b$name[i+j-1] == specieslist[k])
            ct = 1
        }
        if (ct == 0)
        {
          splen = splen + 1
          specieslist[splen] = as.character(b$name[i+j-1])
        }
      }
      cumsp[count] = splen
      cumdur[count] = b$dur[i] + cumdur[count-1]
    }
  }
  
  y = data.frame(cbind(1:length(cumsp),0))
  y = y[-c(1,2)]
  y$cumsp = 0
  y$cumdur = 0
  
  y$cumsp = cumsp
  y$cumdur = cumdur
  return(y)
}