vector = c(2,0,-3,3,-4) 
x = -2
d = vector[c(1)]

for (i in 2:5)
{
  temp = d
  dk = vector[c(i)] + temp*x
  d = dk
}

cat("El porlinomio: 2x^4-3x^2+3x-4 evaluado en -2 es  = ", d )
