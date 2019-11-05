biseccion = function(f, xa, xb, tol)
{
  if ( sign( f(xa)  ) == sign ( f(xb) ) ) 
  {
    stop ( "MISMO SIGGNO F(limInf) F(LimSup)" )
  }  
  
  errores = c()
  eje_x = c()
  eje_y = c()
  
  a = xa 
  b = xb
  k = 0
  
  cat( formatC ( c("  Iteracion  ", "  a     "," b     ", "  Raiz  ", "Error"), width = 2, format = "s", flag = " "  ), "\n")
  
  cont = 0
  repeat 
  {
    cont = cont + 1
    
    m = a + 0.5 * (b-a)
    
    if( f( m ) == 0)
    {
      cat("Cero de f en el intervalo [" ,xa, "," ,xb, "] es ", m)
    }
    if( sign ( f(a) ) != sign ( f(m) ))
    {
      b = m
      
    }
    else
    {
      a  = m
    }
    
    dx = (b-a)/2
    errores[cont] = dx
    
    #imprimir estado
    
    cat( formatC( c(cont, a,b,m,dx ), digits = 7, width = -14, format = "f", flag = " "  ), "\n" )
    k = k + 1
    #until
    if( dx < tol )
    {
      cat ("Cero de", deparse(f) ,"en el intervalo [" ,xa, "," ,xb, "] es aprox.", m, "con error <=", dx )
      
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
      
      plot(eje_x, eje_y, main = "Convergencia Biseccion", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
      plot(iter, errores, main = "Medicion del error Biseccion", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      break;
    }
  }
}

limSup = 1
limInf = 0
tole = 1e-8

f = signif(exp(1),8)^x-signif(pi, 8)*x 

curve(f, -2, 2, main = deparse(f)); abline( h = 0, v = 0)
curve(f, -2, 2, main = "Funcion en el intervalo", xlim = c(limInf,limSup)); abline( h = 0, v = 0)

biseccion(f, limInf, limSup, tole)

