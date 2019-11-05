# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)

# Halla la raiz de Fx

regula <- function(a, b, tol) 
{
  intIni = a
  intFin = b
  
  x<-seq(a,b,0.1)
  
  plot(x,Fx(x),type="l", col="blue", main = deparse(Fx))
  abline(h=0,col="blue")
  
  #x<-b
  #d<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  error <- 1
  
  cont = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  cat( "\n", formatC ( c( "Iteracion     ", "Cero      ", "  Error     "), width = 2, format = "d", flag = " "  ), "\n")
  while (error > tol) 
  {
    cont = cont + 1
    x<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) 
    {
      b <- x
    }
    else 
    {
      a <- x
    }
    
    error<-abs(Fx(x)/F1x(x))
    errores[cont] = error
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    
    cat( formatC( c(cont, x, error), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    
    
  }
  
  
  
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
  
  
  cat("\nRaiz de ", deparse(Fx), "es aproximadamente ", x,"en el intervalo [", intIni,  ",", intFin, "] y con error menor que ", formatC(error, digits = 7,  width = -15, format = "f", flag = "  "))
  
  plot(eje_x, eje_y, main = "Convergencia Posicion Falsa", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
  plot(iter, errores, main = "Medicion del error Posicon Falsa", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
}

limInf = 0
limSup = 2
limMitad = 1
tolerancia = 1e-8

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))
curve(f, -2, 2, main = deparse(f)); abline( h = 0, v = 0)

regula(limInf, limMitad, tolerancia)
regula(limMitad,limSup, tolerancia)

