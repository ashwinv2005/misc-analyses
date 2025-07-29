changedatespecies = function(data)
{
  y = data.frame(cbind(1:length(data),0))
  y = y[-c(1,2)]
  y$year = 0
  y$month = 0
  y$day = 0
  y$ovday = 0
  y$monthname = 0
  
  mvec = c(31,59,90,120,151,181,212,243,273,304,334,365)
  mc = c(31,28,31,30,31,30,31,31,30,31,30,31)
  mvecl = c(31,60,91,121,152,182,213,244,274,305,335,366)
  ml = c(31,29,31,30,31,30,31,31,30,31,30,31)
  mname = c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  data = as.character(data)
  x = str_split(data,"-")
  for (i in 1:length(data))
  {
    print(i)
    for (j in c(3,2,1))
    {
      t = as.numeric(x[[i]][j])
      y[i,j] = t
      if (j == 2)
        y$monthname[i] = mname[t]
      if (j == 1)
      {
        if (y$year[i]%%4 == 0)
          y$ovday[i] = mvecl[y$month[i]] - ml[y$month[i]] + y$day[i]
        if (y$year[i]%%4 != 0)
          y$ovday[i] = mvec[y$month[i]] - mc[y$month[i]] + y$day[i]
      }
    }
  }
  return(y)
}
