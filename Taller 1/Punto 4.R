velocidad = 4
tiempo = 5
eVelocidad = 0.1
eTiempo = 0.1

distancia = velocidad * tiempo
eAbsDistancia = velocidad*eVelocidad + tiempo*eTiempo
eRelDistancia = eVelocidad/velocidad + eTiempo/tiempo

cat("El Error absoluto es = ", eAbsDistancia) 
cat("El error relativo es = ", eRelDistancia*100, "%") 

