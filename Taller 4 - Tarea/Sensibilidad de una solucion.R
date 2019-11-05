A = matrix(c(3.5,4.2,
             1/2,1/3), nrow=2, byrow=TRUE) 
b = c(1.3, 7/4) 

B = matrix(c(3.8,4.5,
             1/2,1/3), nrow=2, byrow=TRUE)  # Matriz modificada

cond = 1/rcond(B) # Numero de condicion

e = cond*((det(A)-det(B))/det(A)) # Error de la solucion

print(A) #Matriz
  print(b) # vector solición

print(B) 

print(cond)

print(abs(e))