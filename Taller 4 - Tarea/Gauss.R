

gauss = function(A, b){
  
  #Sesuponedet(A)!=0
  n = nrow(A)#=ncol(A)paraqueseacuadrada
  #matrizampliada
  Ab = cbind(A,b)
  #Eliminación
  for(k in 1:(n-1)){#desdecolumnak=1hastak=n-1
    if(Ab[k,k]==0){
      #intercambiodefila
      fila =which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    #Eliminacióncolumnak
    for(i in (k+1):n)
    {
      #debajodeladiagonal
      #Fi=Fi-a_ik/a_kk * Fk,i=k+1,...,n
      
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
    }
  }
  #Sustituciónhaciaatrás------------------------#b(i)=A[i,n+1]
  x=rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n]
  #xn=bn/a_nn
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) )/Ab[i,i]
  }
  return(x)
}
#---Pruebas



  
#A =matrix(rep(0,1000),nrow= 3,ncol= 3); 
vect = c(rep(1,100))
vectSol = c( 1:100)
a = diag(vect)
A = matrix( a, nrow = 100)

##
t <- proc.time()

gauss(A,vectSol)#[1]-1.25806451.4193548-0.61290320.0000000



cat( "tiempo de ejecucion 100*100:", proc.time() - t)


#A =matrix(rep(0,1000),nrow= 3,ncol= 3); 
vect = c(rep(1,500))
vectSol = c( 1:500)
a = diag(vect)
A = matrix( a, nrow = 500)

##
t <- proc.time()

gauss(A,vectSol)

cat( "tiempo de ejecucion 500*500:", proc.time() - t)


vect = c(rep(1,1000))
vectSol = c( 1:1000)
a = diag(vect)
A = matrix( a, nrow = 1000)

##
t <- proc.time()

gauss(A,vectSol)

cat( "tiempo de ejecucion 1000*1000:", proc.time() - t)

