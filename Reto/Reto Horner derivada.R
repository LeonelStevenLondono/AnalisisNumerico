
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------

horner <- function(coef, x)
{
  valorf <- coef[1]
  itera <-0
  
  for(k in coef[2:length(coef)])
  {
    valorf = (x*valorf) + k
    itera = itera + 2
  }
  cat(itera,"\n")
  return(valorf)
}
HornerDeri = function(coef )
{
  deri = c()
  valCoef = 0 
  for (i in coef[2:length(coef)])
  {
    valCoef =  i * (length(coef)-i-1)
    deri = c(deri,valCoef)
  }
  return (deri)   
}
xi <- 3
coef <- c(2,0,3,3,4)
der = c()
horner(coef,xi)

der = HornerDeri(coef) 
cat("coefcientes f'(x)",der)
cat("funcion f'(x) evaluada  en  x=",xi,"es igual a ",horner(der,xi))
