changetime2 = function(data)
{
  y = data.frame(cbind(1:length(data),0))
  y = y[-c(1,2)]
  y$hour = 0
  y$minute = 0
  y$time1440 = 0
  
  
  data = as.character(data)
  x = str_split(data,":")
  for (i in 1:length(data))
  {
    for (j in 1:2)
    {
      if (!is.na(x[[i]][j]))
        y[i,j] = x[[i]][j]
      if (is.na(x[[i]][j]))
      {
        y[i,j] = y[i,j+1] = ""
        break
      }
    }
  }
  y$hour = as.numeric(y$hour)
  y$minute = as.numeric(y$minute)
  
  for (i in 1:length(data))
  {
    if (is.na(y$hour[i]))
      y$time1440[i] = NA
    else
      y$time1440[i] = y$hour[i] * 60 + y$minute[i]
  }
  
  return(y)
}