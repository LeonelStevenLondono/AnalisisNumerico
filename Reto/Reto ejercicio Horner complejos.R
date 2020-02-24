
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------

horner <- function(coef, x)
{
  valorf=as.complex(0+0i)
  valorf <- coef[1]
  itera <-0
  multi = 0
  suma = 0
  for(k in coef[2:length(coef)])
  {
    valorf = (x*valorf) + k
    itera = itera + 7
    multi = multi+4
    suma = suma +3
  }
  itera =itera-1
  return(cat("Resultado: ", valorf, "\n con un numero minimo de operaciones de: ", itera,"\n con: ",multi," multiplicaciones y ",suma, " sumas"))
}
z = as.complex(-28)
xi <- sqrt(z)
coef <- c(2,0,-3,3,-4)
horner(coef,xi)

