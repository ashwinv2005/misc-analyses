ebirdclean = function(data)
{
   
  b = data
  
  b = b[,c(4,5,7,10,11,14,15,16,17,22,23,25,26,28,29,35)]
  
  names(b) = c("name","sname","count","countrystatedist","location","latitude","longitude","date","time","listid","protocol","dur","dis","obs","complete","observer")
  
  c = changedatespecies(b$date)
  test = 1
  print(test)
  d = cost(b$countrystatedist)
  print(test)
  e = changetime2(b$time)
  
  x = cbind(b,c,d,e)
  
  x$count = as.numeric(as.character(x$count))
  x$monthname = as.factor(x$monthname)
  x$country = as.factor(x$country)
  x$state = as.factor(x$state)
  
  x = x[with(x, order(year,month,day)),]
  
  return(x)
}