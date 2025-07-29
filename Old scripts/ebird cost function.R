######## change country state

cost = function(data)
{
  y = data.frame(cbind(1:length(data),0))
  y = y[-c(1,2)]
  y$country = 0
  y$state = 0
  y$district = 0
  
  data = as.character(data)
  x = str_split(data,"-")
  for (i in 1:length(data))
  {
    for (j in 1:3)
    {
      y[i,j] = x[[i]][j]
    }
  }
  
  return(y)
}