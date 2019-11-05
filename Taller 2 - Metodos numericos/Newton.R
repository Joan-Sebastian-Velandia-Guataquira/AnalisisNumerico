
newtonraphson = function(fun, x0, tol, maxiteraciones)
{ 
  valIni = x0
  eje_x = c()
  eje_y = c()
  errores = c()
  cont = 0
  # f = string
  numiter = 0 
  g = parse(text = fun) # parse devuelve tipo "expression"
  g. = D(g,"x") 
  fx = function(x){eval(g)} # convertir f a función
  fp = function(x){eval(g.)} # convertir f' a función
  
  correccion = -fx(x0)/fp(x0) 
  cat( "\n", formatC ( c( "Iteracion     ", "Cero      ", " f(cero)     ", "  Error  "), width = 2, format = "d", flag = " "  ), "\n")
  
  while (abs(correccion) >= tol && numiter <= maxiteraciones)
  {
    cont = cont + 1
    numiter = numiter + 1 
    
    if (fp(x0) == 0) stop("División por cero")
    
    x1 = x0 + correccion 
    errores[cont] = abs((x1-x0))/x1
    correccion = -fx(x1)/fp(x1)
    cat( formatC( c(numiter, x0, fx(x0), errores[cont] ), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    x0 = x1 
  }
  
  if (numiter > maxiteraciones)
  {
    warning("Se alcanzó el máximo número de iteraciones.")
    cat("Estado:\n")
    cat("k = ", k, "x = ", x1, " f(x) = ", f(x1), "Error estimado <= ", correccion)
  }
  else 
  {
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
    

    plot(eje_x, eje_y, main = "Convergencia Newton", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
    plot(iter, errores, main = "Medicion del error Newton", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
    
    cat("Cero de ", fun, "es aproximadamente ", x0,"con valor incicial x_0 = ", valIni, " y con error menor que ", correccion)
    #return(list(cero = x0, f.cero = fx(x0), numeroiter = numiter, error.est = correccion) ) 
    
  }
}

valInicial = 0
tolerancia = 1e-8
maximaIter = 100

f = function(x) x^3 + 5*x -1

newtonraphson("x^3+5*x-1", valInicial, tolerancia, maximaIter)
