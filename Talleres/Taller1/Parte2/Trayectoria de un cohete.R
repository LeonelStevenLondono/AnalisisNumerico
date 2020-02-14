#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
cohete = function()
{
  t = 0
  value = 6+2.13*(t^2)-0.0013*(t^4)
  final = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
  while(value<final)
  {

    t =t+1
    value = 6+2.13*(t^2)-0.0013*(t^4)
    final = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
 
  }
  cat("La mayor altura alcanzada es :",value)
  
  
}
cohete()