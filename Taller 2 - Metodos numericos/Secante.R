secante = function(f, x0, x1, tol, maxiter = 100)
{
  intIni = x0
  intFin  = x1
  
  f0 = f(x0)
  f1 = f(x1)
  k = 0 
  
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  iteraciones = 0
  
  cat( "\n", formatC ( c( "Iteracion     ", "Cero      ", " f(cero)     ", "  Error  "), width = 2, format = "d", flag = " "  ), "\n")
  
  while (abs(x1 - x0) > tol && k <= maxiter )
  {
    k = k+1
    pendiente = (f1 - f0)/(x1 - x0)
    if (pendiente == 0) return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA)
    x2 = x1 - (f1/pendiente)
    f2 = f(x2)
    
    errores[k] = abs(x1-x0)/x1
    
    x0 = x1; f0 = f1
    x1 = x2; f1 = f2
    
    
    # Imprimir iteraciones
    cat( formatC( c(k, x1, x2, abs(x1-x0)/x1 ), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )

  }
  if (k > maxiter) 
  {
    warning("No se alcanzó el número de iteraciones")
  }
  
  iter = c(1:k)
  cont_n = 0;
  cont_e = 0;
  
  repeat
  {
    eje_x[cont_n] = errores[cont_e]
    eje_y[cont_n] = errores[cont_e+1]
    cont_n = cont_n + 1
    cont_e = cont_e + 1;
    
    if (cont_n == k)
    {
      break;
    }
  }
  
  plot(eje_x, eje_y, main = "Convergencia Secante", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
  plot(iter, errores, main = "Medicion del error Secante", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
  
  cat("Raiz de ", deparse(f), "es aproximadamente ", f2,"en el intervalo [", intIni,  ",", intFin, "] y con error menor que ", abs(x2-x1))
  
  #return(list(cero=x2, f.cero=f2, iter=k, ErrorEst =abs(x2-x1)))
}

limInf =-2
limSup = 2
tolerancia = 1e-8
maxIteraciones = 100

limMitad = 1

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x

par(mar = rep(2, 4))

curve(f, limInf, limSup, main = deparse(f)); abline( h = 0, v = 0)
curve(f, limInf, limSup, main = "Funcion en el intervalo 1", xlim = c(limInf,limMitad)); abline( h = 0, v = 0)
curve(f, limInf, limSup, main = "Funcion en el intervalo 2", xlim = c(limMitad, limSup)); abline( h = 0, v = 0)

secante(f, limInf, limMitad, tolerancia, maxIteraciones)
secante(f, limMitad, limSup, tolerancia, maxIteraciones)