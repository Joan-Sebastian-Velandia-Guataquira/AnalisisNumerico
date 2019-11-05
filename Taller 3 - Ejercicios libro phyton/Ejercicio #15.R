riemman = function(f, a, b, n)
{
  i = 1
  delta = (b-a)/n
  x = a + i*delta
  area = 0
  
  repeat
  {
    area = area + f(x)*delta
    i = i + 1
    if (i == n) break;
    x = a + i*delta
  }
  
  #cat ("area" , area, "\n")
  return (area)
}


ejercicio = function(fun, i, f, tol, avance, n)
{
  sumatoria = 0
  x0 = riemman(fun, i, f, n)
  sumatoria =  sumatoria + x0
  
  i = f
  f =  f + avance
  
  repeat
  {
    
    x1 = riemman(fun, i, f, n)
    sumatoria = sumatoria + x1
    #cat ("area" , sumatoria, "\n")
    error = abs(x1-x0)
    i = f
    f = f + avance
    x0 = x1
    
    if (sumatoria >  2 | error < tol)
    {
      cat ("Area = " , sumatoria, "resulta en el intervalo: a = 0","b = ", f, "con un error relativo de",abs(2-sumatoria)/2 * 100,"%\n")
      return(sumatoria)
    }
    
  }
}

f = function(x) 5-(exp(1)^x)
ejercicio(f, 0, 0.1, 10^-8, 0.001, 100)