clean1speciesdist = function(data)
{
  
  b = data
  
  b = b[,c(1,2,3,4,6,10,11,12,13,15)]
  
  names(b) = c("name","sname","count","country","state","latitude","longitude","date","time","comments")
  
  c = changedatespecies(b$date)
  #d = cost(b$countrystate)
  e = changetimespecies(b$time)
  
  x = cbind(b,c,e)
  
  x$count = as.numeric(as.character(x$count))
  x$monthname = as.factor(x$monthname)
  x$country = as.factor(x$country)
  x$state = as.factor(x$state)
  
  x = x[with(x, order(year,month,day,time1440)),]
  
  return(x)
}
