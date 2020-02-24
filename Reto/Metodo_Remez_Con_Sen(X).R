#--------------------------------
# RETO #1
# METODO DE REMEZ
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------


ParticionesX <- function(gradosPoli, suma){
  coeficientesX <- c() 
  i <- 1
  while(length(coeficientesX) < gradosPoli){
    if (length(coeficientesX) == 0 )
    {
      coeficientesX[i] <- -pi/64;
      i <- i +1
    }
    coeficientesX[i] <- (coeficientesX[i-1]+ (suma))
    i <- i+1
  }
  
  return (coeficientesX)
}

ParticionesY <- function(gradospoli, funcion,derivada, coeficientesX){
  coeficientesY <- c()
  j <- 1
  while (j < gradospoli+1 )
  {
    coeficientesY[j] <- funcion(coeficientesX[j])
    j <- j+1
  }
  
  coeficientesY[j] <- derivada(pi/128)
  return(coeficientesY)
}

Remez <- function(funcion, derivada, gradosPoli, suma){
  
  coeficientesX <- ParticionesX(gradospoli,suma )
  coeficientesY <- ParticionesY(gradospoli, funcion, derivada, coeficientesX)
  
  for(i in gradospoli)
  
  coeficientesSistema = rbind(
    c(1,coeficientesX[1],(coeficientesX[1])^2,(coeficientesX[1])^3),
    c(1,coeficientesX[2],(coeficientesX[2])^2,(coeficientesX[2])^3),
    c(1,coeficientesX[3],(coeficientesX[3])^2,(coeficientesX[3])^3),
    c(0,1,2*(pi/128),3*(pi/128)^2))
  
  coeficientesPolinomio <- solve(coeficientesSistema, coeficientesY)
  
  print(coeficientesPolinomio)
  
  funcionAproximada <- function(x){
    coeficientesPolinomio[1] + (coeficientesPolinomio[2]*x) + (coeficientesPolinomio[3]*x^2) + (coeficientesPolinomio[4]*x^3)
  }
  
  return (funcionAproximada)
    
}

funcion <- function(x){
  return (sin(x))
}

derivada <- function(x){
  return(cos(x))
}

gradospoli <- 3

suma <- (2*(pi/64))/(gradospoli-1)

funcionAproximada <- Remez(funcion, derivada, gradosPoli, suma)

#Grafica de la funcion seno
plot(funcion,xlim = c(-2,2), ylim=c(-1,1),ylab = "Y", col = "green")
#Grafica del polinomio que aproxima la funcion
par(new = TRUE)
plot(funcionAproximada,xlim = c(-2,2),ylim=c(-1,1),ylab = "Y", main = "Polinomio de Aproximacion", col= "red")

ErrorAbsoluto <- 0
ErrorRelativo <- 0
ValorX <- pi/256

ErroAbsoluto = abs((funcion(ValorX)- funcionAproximada(ValorX))*10^-6)
ErrorRelativo  = ((ErroAbsoluto / funcionAproximada(ValorX))*100)*10^-6

cat("Dado el punto  ",ValorX)
cat(" El Error Relativo es de :" ,ErrorRelativo)
cat(" El Error Absoluto es de :" ,ErroAbsoluto)
#tablaValores <- data.frame (xc,yc)
#print(tablaValores)

#--------------------------------