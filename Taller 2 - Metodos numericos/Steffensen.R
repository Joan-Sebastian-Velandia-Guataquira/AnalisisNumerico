steffensen = function(f, x0, tol, nmax)
{
  valInicial = x0

  i = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  cat( "\n", formatC ( c( "Iteracion     ", "Cero      ", "  Error     "), width = 2, format = "d", flag = " "  ), "\n")
  
  repeat
  {
    i = i + 1
    a = f(x0)*f(x0)
    b = f(x0+f(x0))-f(x0)
    x1 = x0 - (a/b)
    error = (abs(x1-x0))/x1
    errores[i] = error
    temp = f(x0)
    x0 = x1
    
    cat( formatC( c(i, x0, error), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    
    
    if(abs(temp) <= tol)
    {
      iter = c(1:i)
      cont_n = 0;
      cont_e = 0;
      
      repeat
      {
        eje_x[cont_n] = errores[cont_e]
        eje_y[cont_n] = errores[cont_e+1]
        cont_n = cont_n + 1
        cont_e = cont_e + 1;
        
        if (cont_n == i)
        {
          break;
        }
      }
      
      cat("\nRaiz de ", deparse(f), "es aproximadamente ", x0," con un valor inicial", valInicial," y con error menor que ",formatC(error, digits = 7,  width = -15, format = "f", flag = "  "))
      
      plot(eje_x, eje_y, main = "Convergencia steffensen", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
      plot(iter, errores, main = "Medicion del error steffensen", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      
      break;   
    }
  }
}


valIni = 0
tolerancia = 1e-8
maxIter = 100

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))

curve(f, -2, 2, main = deparse(f)); abline( h = 0, v = 0)
steffensen(f, valIni, tolerancia, maxIter)
valIni = 1.5
steffensen(f, valIni, tolerancia, maxIter)