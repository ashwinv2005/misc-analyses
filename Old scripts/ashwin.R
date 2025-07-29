a = 0
n = 50
for (i in 0:n)
{
for (j in 0:n)
{
if (abs(i-j)>=12)
{
b = factorial(n)/(factorial(i)*factorial(n - i))
c = factorial(n)/(factorial(j)*factorial(n - j))
a = a + (b*0.64^i*0.36^(n-i))*(c*0.64^j*0.36^(n-j))
}
}
}
a