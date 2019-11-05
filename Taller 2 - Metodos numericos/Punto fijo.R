
puntofijo = function(g, x0, tol, maxIteraciones)
{
  valIni = x0
  eje_x = c()
  eje_y = c()
  k = 1 
  cont = 0
  errores = c()
  #iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  
  cat( "\n", formatC ( c( "Iteracion    ", "  Resultado  ", "  Error  "), width = 2, format = "s", flag = " "  ), "\n")
  
  repeat
  {
    
    cont = cont + 1
    x1 = g(x0)
    dx = abs(x1 - x0) 
    errores[cont] = dx/x1
    x0 = x1 
    #Imprimir estado
    
    
    cat( formatC( c( k, x1, dx/x1 ), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    
    k = k+1 
    #until 
    if((dx < tol) || (k > maxIteraciones)) break;
  }
  
  # Mensaje de salida 
  if( dx > tol )
  { 
    cat("No hubo convergencia ") 
    
    #return(NULL)
  } 
  else
  { 
    cat("Cero de ", deparse(g), "es aproximadamente ", x1,"con valor incicial x_0 = ", valIni, " y con error menor que ", tol)
    iter = c(1:cont)
    cont_n = 0;
    cont_e = 0;
    
    repeat
    {
      eje_x[cont_n] = errores[cont_e]
      eje_y[cont_n] = errores[cont_e+1]
      cont_n = cont_n + 1
      cont_e = cont_e + 1;
      
      if (cont_n == cont)
      {
        break;
      }
    }
    
    plot(eje_x, eje_y, main = "Convergencia punto fijo", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
    plot(iter, errores, main = "Medicion del error punto fijo", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
  }
}


valInicial = 0
tolerancia = 1e-8
maxIter = 100

g = function(x)  (x^3-1)/-5
curve(g, -6, 6, main = deparse(g)); abline( h = 0, v = 0)
puntofijo(g,  valInicial,  tolerancia, maxIter) 
