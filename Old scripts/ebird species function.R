# protocol (proto) S,T,ST F if none
# location F if none

totspecies = function(data,proto)
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
  
  x = as.character(unique(b$name))
  
  x = x[-grep("sp.",x, invert = F)]
  
  return(x)
}
  
  
  
  
  