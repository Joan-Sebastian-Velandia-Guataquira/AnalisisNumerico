
a <- matrix(c(1,2,4, 2,1,-4, 0,0,3), 3, 3, byrow = TRUE)
r <- eigen(a) 
p <- r$vectors; 
lam <- r$value
d = diag(lam)

cat("Matriz A")
print (a)

cat("Vectores propios P")
p

cat("Diagonal de los vectores propios P")
d

A_3 = p %*% d^3 %*% solve(p)

cat("A^3")
A_3
