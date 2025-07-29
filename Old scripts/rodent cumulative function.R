spcum = function(data)
{
  b = data
  
  len = length(b$date)
  tempdate = b$date[1]
  first = b[b$date == tempdate,]
  
  specieslist = character(length(unique(b$species)))
  specieslist[1:length(unique(first$species))] = as.character(unique(first$species))
  splen = length(unique(first$species))
  
  cumsp = numeric(length(unique(b$date)))
  cumsp[1] = splen
  
  count = 1
  
  for(i in 2:len)
  {
    if(b$date[i] != b$date[i-1])
    {
      count = count + 1
      tl = length(b[b$date == b$date[i],]$date)
      for(j in 1:tl)
      {
        ct = 0
        for(k in 1:splen)
        {
          if (b$species[i+j-1] == specieslist[k])
            ct = 1
        }
        if (ct == 0)
        {
          splen = splen + 1
          specieslist[splen] = as.character(b$species[i+j-1])
        }
      }
      cumsp[count] = splen
    }
  }
  
  y = data.frame(cbind(1:length(cumsp),0))
  y = y[-c(1,2)]
  y$cumsp = 0

  y$cumsp = cumsp
  
  return(cumsp)
}