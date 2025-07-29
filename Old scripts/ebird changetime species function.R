changetimespecies = function(data)
{
  y = data.frame(cbind(1:length(data),0))
  y = y[-c(1,2)]
  y$time = 0
  y$ap = 0
  y$hour = 0
  y$minute = 0
  y$hrs = 0
  y$time1440 = 0
  
  
  data = as.character(data)
  
  for (i in 1:length(data))
  {
    data[i] = str_sub(data[i],1,5)
    data[i] = str_c(data[i], " AM")
  }
  
  x = str_split(data," ")
  for (i in 1:length(data))
  {
    for (j in 1:2)
    {
      if (x[[i]][j] != "")
        y[i,j] = x[[i]][j]
      if (x[[i]][j] == "")
      {
        y[i,j] = y[i,j+1] = ""
        break
      }
    }
  }
  
  x = str_split(y$time,":")
  for (i in 1:length(data))
  {
    for (j in 1:2)
    {
      if (x[[i]][j] != "")
      {
        y[i,j+2] = x[[i]][j]
      }
      if (x[[i]][j] == "")
      {
        y[i,j+2] =y[i,j+3] = NA
        break
      } 
    }
  }
  
  y$hour = as.numeric(y$hour)
  y$minute = as.numeric(y$minute)
  
  for (i in 1:length(data))
  {
    if (y$ap[i] == "AM")
      y$hrs[i] = y$hour[i]
    if (y$ap[i] == "PM")
      y$hrs[i] = y$hour[i] + 12
    if (y$ap[i] == "")
      y$hrs[i] = NA
    if (is.na(y$hrs[i]))
      y$time1440[i] = NA
    else
      y$time1440[i] = y$hrs[i] * 60 + y$minute[i]
  }
  
  return(y[,4:6])
}
