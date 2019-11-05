diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

A = matrix(c(-8.1,   -7, 6.123,  -2,
               -1,    4,    -3,  -1,
                0,   -1,    -5, 0.6,
               -1, 0.33,     6, 1/2), nrow=4, byrow=TRUE)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
x0 = c(1,2,1,1)
D = diag1(A)
U = triu(A,k=1,diag = FALSE)
L = tril(A,k=-1,diag = FALSE)


jacobiPr <- function(A,b, x0, tol)
{
  x_k = matrix(x0)
  
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == tol)
      break
  }
  cat("Solución a 5 iteraciones: ",x_k,"\n")
}

cat("Matriz A")
A

cat("Solucion funcion metodo de jacobi" )
print (jacobiPr(A, b, x0, 5))

cat("Solucion funcion solve")
print(solve(A, b))

I=diag(1,nrow = nrow(A)) #Identidad
T3 = -solve(D) #Diagonal^-1
T4 = T3%*%U #D * U
T5= solve(D) 
T6 = L%*%T5 #LD
T7 = I + T6 #I + LD
T8 = solve(T7)
MatTG = T4%*%T8  #D * U * I + LD

convergencia = norm(MatTG, type = c("I"))

cat("convergencia ", convergencia)

cat("numero de condicion ",1/rcond(A))