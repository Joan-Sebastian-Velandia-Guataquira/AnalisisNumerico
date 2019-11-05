steffensen = function(f, x0, tol, nmax)
{
  valInicial = x0
  i = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
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
    
    cat("X=", x0,"\t","E=", error, "iteracion", i,"\n")
    
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
      
      return (x0)
      break;   
    }
  }
}

f = function(x) 2*x^2-10*x+5
x = steffensen(f, 0.5, 10^-8, 10)
y = steffensen(f, 5, 10^-8, 10)
suma = x + y
suma2 = x^2 + y^2
cat("Intercepto 1 y 2 respectivamente: ", x, y)
cat("Comprobación x+y = ", suma, " x^2+y^2 = ", suma2)