coordalt = read.csv("C:/Users/ashwinv/Desktop/coordalt.csv")
coordalt$x = as.character(coordalt$x)
coordalt$y = as.character(coordalt$y)

## x

a = substr(coordalt$x,2,3)
b = substr(coordalt$x,5,6)
c = substr(coordalt$x,8,12)

d = substr(c,5,5)

c[d == "\""] = substr(c[d == "\""],1,4)

a = as.numeric(a)
b = as.numeric(b)
c = as.numeric(c)

coordalt$xs = a + (b + c/60)/60

## y

a = substr(coordalt$y,2,3)
b = substr(coordalt$y,5,6)
c = substr(coordalt$y,8,12)

d = substr(c,5,5)

c[d == "\""] = substr(c[d == "\""],1,4)

a = as.numeric(a)
b = as.numeric(b)
c = as.numeric(c)

coordalt$ys = a + (b + c/60)/60

write.csv(coordalt,"C:/Users/ashwinv/Desktop/coordalt.csv")
