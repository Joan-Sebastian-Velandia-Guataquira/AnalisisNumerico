x = 3
n = 7
y = 0.5*(x + (n/x) )
e = 0
E = 10e-8

iteracion =  0
errores = c()


cat( "\n", formatC ( c( "Iteracion     ", "    valor de Y      "), width = 2, format = "d", flag = " "  ), "\n")
while(abs(x - y) > E)
{
  iteracion = iteracion + 1
  errores[iteracion] = y
  
  x = y  
  y = 0.5*(x + (n/x) )
  cat( formatC( c(iteracion, y), digits = 15, width = -15, format = "f", flag = "  "  ), "\n" )
  
}

iteraciones = c(1:iteracion)

plot(iteraciones, errores, main = "Y vs Iteraciones", xlab= "Iteraciones", ylab = "Valor de y", type = 'o' )


cat("Solucion real =", sqrt(n)  )
cat("Solución dada por el algoritmo: ", y ," con un error relativo = ", formatC((abs(x - y)/x) *100, digits = 4, width = 0,format = 'f'), "%" )
cat("y^2 = ", y*y)
